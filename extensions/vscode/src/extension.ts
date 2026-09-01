import "source-map-support/register";
import * as vsc from 'vscode';
import * as ls from "vscode-languageclient/node";

import which from "which";
import { exec, execFile } from "node:child_process";
import { promisify } from "node:util";
import fs from "node:fs/promises";
import path from "node:path";
import { pipeline } from "node:stream/promises";
import { Readable } from "node:stream";
import { createWriteStream } from "node:fs";
import { release } from "node:os";

const PBLS_EXECUTABLE_BASE = "powerbuilder-ls";
const EXECUTABLE_EXT = (process.platform === "win32" ? "powerbuilder-ls.exe" : "");
const PBLS_DOWNLOAD = `${PBLS_EXECUTABLE_BASE}-${process.arch}-${process.platform}${EXECUTABLE_EXT}`;
const PBLS_EXECUTABLE = PBLS_EXECUTABLE_BASE + EXECUTABLE_EXT;

export async function getExecutablePath(context: vsc.ExtensionContext): Promise<string | null> {
	try {
		const systemServerPath = await which(PBLS_EXECUTABLE_BASE);
		return systemServerPath;
	} catch { }

	let storagePath = context.globalStorageUri.fsPath;
	let storageServerPath = path.join(storagePath, PBLS_EXECUTABLE);

	try {
		await fs.access(storageServerPath, fs.constants.R_OK | fs.constants.X_OK);
		return storageServerPath;
	} catch (e) {
		console.error(`PowerBuilder-LS executable not found in storage path: ${e}`);
	}

	const msg = await vsc.window.showInformationMessage("No PowerBuilder-LS executable found. Download it from Github Released?", "Yes", "No, don't start the server");
	if (msg !== "Yes") {
		return null;
	}

	const releases = await fetch("https://api.github.com/repos/micha4w/powerbuilder-ls/releases");
	if (!releases.ok) {
		await vsc.window.showErrorMessage(`Failed to fetch latest release: ${releases.status}\n${await releases.text()}`);
		return null;
	}

	const parseVersion = (tag: string) => tag.match(/^v?(\d+)\.(\d+)\.(\d+)/)?.slice(1).map(n => parseInt(n));
	const extVersion = parseVersion(context.extension.packageJSON.version)!;

	const releaseData = await releases.json() as any[];
	const versions = releaseData
		.map(release => [parseVersion(release.tag_name), release] as [number[] | undefined, any])
		.filter(v => !!v[0]) as [number[], any][];

	let max: [number[], any] | null = null;
	for (const [version, release] of versions) {
		if (version[0] === extVersion[0] && version[1] >= extVersion[1]) {
			if (!max || version[1] > max[0][1] || (version[1] === max[0][1] && version[2] > max[0][2])) {
				max = [version, release];
			}
		}
	}

	if (!max) {
		await vsc.window.showErrorMessage("No compatible release found for your extension version.");
		return null;
	}


	for (const asset of max[1].assets) {
		if (asset.name === PBLS_DOWNLOAD) {
			const download = await fetch(asset.browser_download_url);

			if (!download.ok || !download.body) {
				await vsc.window.showErrorMessage(`Failed to download latest release: ${download.status}\n${await download.text()}`);
				return null;
			}

			console.info(`Downloading PowerBuilder-LS executable from ${asset.browser_download_url} to ${storageServerPath}`);

			await fs.mkdir(storagePath, { recursive: true });
			await pipeline(
				Readable.fromWeb(download.body),
				createWriteStream(storageServerPath, { mode: 0o755 })
			);

			return storageServerPath;
		}
	}

	let cargoPath, gitPath;
	try {
		await which("rustup");
		cargoPath = await which("cargo");
		gitPath = await which("git");
	} catch (e) {
		console.error(`Cargo or Git not found in system path: ${e}`);
		await vsc.window.showErrorMessage("The latest release does not contain an Executable for your platform, consider building it from source.");
		return null;
	}

	await vsc.window.showInformationMessage("Found a Rust setup, would you like to build the server from source? (installs a nightly toolchain)", "Yes", "No, don't start the server");
	if (msg !== "Yes") {
		return null;
	}

	try {
		const buildDir = await fs.mkdtemp("powerbuilder-ls-build-");

		const git = await promisify(execFile)(gitPath, ["clone", "https://github.com/micha4w/powerbuilder-ls", buildDir]);
		if (git.stderr) {
			await vsc.window.showErrorMessage(`Failed to clone PowerBuilder-LS: ${git.stderr}`);
			return null;
		}

		const build = await promisify(execFile)(cargoPath, ["build", "--release"], { cwd: buildDir });
		if (build.stderr) {
			await vsc.window.showErrorMessage(`Failed to build PowerBuilder-LS: ${build.stderr}`);
			return null;
		}

		const builtServerPath = path.join(context.extensionPath, "target", "release", PBLS_EXECUTABLE);
		await fs.copyFile(builtServerPath, storageServerPath);
		await fs.chmod(storageServerPath, 0o755);
		return storageServerPath;
	} catch (e) {
		await vsc.window.showErrorMessage(`Failed to build PowerBuilder-LS: ${e}`);
		return null;
	}
}


let client: ls.LanguageClient;

export async function activate(context: vsc.ExtensionContext) {
	const serverPath = vsc.workspace.getConfiguration().get<string>("powerbuilder.languageServer.path") || await getExecutablePath(context);
	const logLevel = vsc.workspace.getConfiguration().get<string>("powerbuilder.languageServer.logLevel");

	if (!serverPath) {
		console.warn("No PowerBuilder-LS executable found, not starting the server.");
		return;
	}

	let disposable = vsc.commands.registerCommand("powerbuilder.restart-ls", async () => await client.restart());
	context.subscriptions.push(disposable);

	const outputChannel = vsc.window.createOutputChannel("PowerBuilder-LS Logs", { log: true });
	const traceOutputChannel = vsc.window.createOutputChannel("PowerBuilder-LS Trace", { log: true });
	context.subscriptions.push(outputChannel, traceOutputChannel);

	const executable = (debug: boolean) => {
		let exec: ls.Executable = {
			command: serverPath,
			options: { env: process.env },
		};
		if (logLevel) {
			if (logLevel !== "off") {
				exec.options!.env.RUST_LOG = logLevel;
			}
		} else if (debug) {
			exec.options!.env.RUST_LOG = "debug";
		}
		return exec;
	};

	const serverOptions: ls.ServerOptions = {
		run: executable(false),
		debug: executable(true),
	};
	const clientOptions: ls.LanguageClientOptions = {
		documentSelector: [{ scheme: "file", language: "powerbuilder" }],
		// TODO: pbsln and pbproj files
		// synchronize: {
		//   fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
		// },
		outputChannel,
		traceOutputChannel,
		markdown: { isTrusted: true, supportHtml: true },
		outputChannelName: "PowerBuilder-LS Logs",
		errorHandler: {
			error: (...args) => ({ action: ls.ErrorAction.Shutdown }),
			closed: async () => {
				const msg = await vsc.window.showErrorMessage("PowerBuilder-LS stopped unexpectedly.", "Restart", "Show Output");
				let ret = { action: ls.CloseAction.DoNotRestart, handled: true };
				switch (msg) {
					case "Restart":
						ret.action = ls.CloseAction.Restart;
						break;
					case "Show Output":
						outputChannel.show();
						break;
				}
				return ret;
			}
		}
	};

	client = new ls.LanguageClient("powerbuilder-ls", "PowerBuilder Language Server", serverOptions, clientOptions);
	client.start();
}

export async function deactivate() {
	await client?.stop();
}