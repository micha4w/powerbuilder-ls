import "source-map-support/register";
import * as vsc from 'vscode';
import * as ls from "vscode-languageclient/node";

import which from "which";
import { exec, execFile } from "node:child_process";
import { promisify } from "node:util";
import fs from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import { pipeline } from "node:stream/promises";
import { Readable } from "node:stream";
import { createWriteStream } from "node:fs";

const PBLS_EXECUTABLE_BASE = "powerbuilder-ls";
const EXECUTABLE_EXT = (process.platform === "win32" ? ".exe" : "");
const PBLS_DOWNLOAD = `${PBLS_EXECUTABLE_BASE}-${process.arch}-${process.platform}${EXECUTABLE_EXT}`;
const PBLS_EXECUTABLE = PBLS_EXECUTABLE_BASE + EXECUTABLE_EXT;

type SemVer = [number, number, number];
function parseVersion(tag: string): SemVer {
    return tag.match(/^v?(\d+)\.(\d+)\.(\d+)/)?.slice(1).map(n => parseInt(n)) as SemVer;
}

export class PBLanguageClient {
    client: ls.LanguageClient | null = null;
    channels: [vsc.LogOutputChannel, vsc.LogOutputChannel] | null = null;

    storagePath: string;
    storedServerExecutable: string;
    extensionVersion: SemVer;

    serverExecutable: string | null = null;

    constructor(context: vsc.ExtensionContext) {
        this.storagePath = context.globalStorageUri.fsPath;
        this.storedServerExecutable = path.join(this.storagePath, PBLS_EXECUTABLE);

        this.extensionVersion = parseVersion(context.extension.packageJSON.version)!;

        let restartCommand = vsc.commands.registerCommand("powerbuilder.restartLs", async () => {
            await this.destroy();
            await this.create();
        });
        context.subscriptions.push(restartCommand, this);
    }

    executbleCompatible(executableVersion: SemVer) {
        return executableVersion[0] === this.extensionVersion[0] && executableVersion[1] >= this.extensionVersion[1];
    }


    async getExecutablePath(): Promise<string | null> {
        let existingExe: string | undefined = undefined;
        try {
            existingExe = await which(PBLS_EXECUTABLE_BASE);
        } catch { }

        try {
            await fs.access(this.storedServerExecutable, fs.constants.R_OK | fs.constants.X_OK);
            existingExe = this.storedServerExecutable;
        } catch (e) {
            console.error(`PowerBuilder-LS executable not found in storage path: ${e}`);
        }

        let msg;
        if (existingExe) {
            const { stdout } = await promisify(execFile)(existingExe, ["--version"]);
            const exeVersion = parseVersion(stdout);
            if (this.executbleCompatible(exeVersion)) {
                return existingExe;
            }

            msg = `Installed PowerBuilder-LS executable is out of date (${existingExe}).\n\nUpdate from GitHub Releases?`;
        } else {
            msg = "No PowerBuilder-LS executable found.\nDownload it from GitHub Releases?";
        }
        const response = await vsc.window.showInformationMessage(msg, "Yes", "No, don't start the server", "No, don't ask again");
        switch (response) {
            case "No, don't ask again":
                vsc.workspace.getConfiguration().update("powerbuilder.languageServer.enable", false, vsc.ConfigurationTarget.Global);
            default:
                return null;
            case "Yes":
        }

        const releases = await fetch("https://api.github.com/repos/micha4w/powerbuilder-ls/releases");
        if (!releases.ok) {
            await vsc.window.showErrorMessage(`Failed to fetch latest release: ${releases.status}\n${await releases.text()}`);
            return null;
        }

        const releaseData = await releases.json() as any[];
        const versions = releaseData
            .map(release => [parseVersion(release.tag_name), release])
            .filter(v => v[0]) as [SemVer, any][];

        let max: [SemVer, any] | null = null;
        for (const [version, release] of versions) {
            if (this.executbleCompatible(version)) {
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

                console.info(`Downloading PowerBuilder-LS executable from ${asset.browser_download_url} to ${this.storedServerExecutable}`);

                await fs.mkdir(this.storagePath, { recursive: true });
                await pipeline(
                    Readable.fromWeb(download.body),
                    createWriteStream(this.storedServerExecutable, { mode: 0o755 })
                );

                return this.storedServerExecutable;
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

        await vsc.window.showInformationMessage("No suitable release found. But a Rust setup was found, would you like to build the server from source? (installs a nightly toolchain)", "Yes", "No, don't start the server");
        if (msg !== "Yes") {
            return null;
        }

        try {
            return await vsc.window.withProgress(
                {
                    location: vsc.ProgressLocation.Notification,
                    title: "PowerBuilder-LS",
                    cancellable: true
                },
                async (progress, token) => {
                    const controller = new AbortController();
                    token.onCancellationRequested(() => controller.abort());

                    const buildDir = await fs.mkdtemp(path.join(os.tmpdir(), "powerbuilder-ls-build-"));
                    console.info(`Building PowerBuilder-LS from source in ${buildDir}`);

                    progress.report({ message: "Cloning..." });
                    await promisify(execFile)(
                        gitPath, ["clone", "https://github.com/micha4w/powerbuilder-ls", buildDir],
                        { signal: controller.signal }
                    );
                    if (controller.signal.aborted) {
                        return null;
                    }

                    progress.report({ message: "Building..." });
                    await promisify(execFile)(
                        cargoPath, ["build", "--release"],
                        { cwd: buildDir, signal: controller.signal }
                    );
                    if (controller.signal.aborted) {
                        return null;
                    }

                    progress.report({ message: "Installing..." });
                    const builtExe = path.join(buildDir, "target", "release", PBLS_EXECUTABLE);

                    console.info(`Copying built PowerBuilder-LS executable from ${builtExe} to ${this.storedServerExecutable}`);
                    await fs.mkdir(this.storagePath, { recursive: true });
                    await fs.copyFile(builtExe, this.storedServerExecutable);
                    await fs.chmod(this.storedServerExecutable, 0o755);

                    return this.storedServerExecutable;
                }
            );

        } catch (e) {
            await vsc.window.showErrorMessage(`Failed to build PowerBuilder-LS: ${e}`);
            return null;
        }
    }

    executable(serverPath: string, logLevel: string | undefined, debug: boolean): ls.Executable {
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

    async create(): Promise<boolean> {
        console.info("Creating PowerBuilder-LS");

        if (!vsc.workspace.getConfiguration().get<boolean>("powerbuilder.languageServer.enable")) {
            this.serverExecutable = null;
        } else {
            this.serverExecutable = vsc.workspace.getConfiguration().get<string>("powerbuilder.languageServer.path") || await this.getExecutablePath();
        }

        vsc.commands.executeCommand("setContext", "powerbuilder.lsFound", this.serverExecutable !== null);
        if (!this.serverExecutable) {
            return false;
        }

        const logLevel = vsc.workspace.getConfiguration().get<string>("powerbuilder.languageServer.logLevel");
        if (!this.channels) {
            const outputChannel = vsc.window.createOutputChannel("PowerBuilder-LS Logs", { log: true });
            const traceOutputChannel = vsc.window.createOutputChannel("PowerBuilder-LS Trace", { log: true });
            this.channels = [outputChannel, traceOutputChannel];
        }

        const serverOptions: ls.ServerOptions = {
            run: this.executable(this.serverExecutable, logLevel, false),
            debug: this.executable(this.serverExecutable, logLevel, true),
        };
        const clientOptions: ls.LanguageClientOptions = {
            documentSelector: [
                { scheme: "file", language: "powerbuilder" },
                { scheme: "file", pattern: "**/*.pb{sln,proj}" },
            ],
            // synchronize: {
            //     fileEvents: [
            //         vsc.workspace.createFileSystemWatcher("**/.pbproj"),
            //         vsc.workspace.createFileSystemWatcher("*.pbsln"),
            //     ],
            // },
            outputChannel: this.channels[0],
            traceOutputChannel: this.channels[1],
            markdown: { isTrusted: true, supportHtml: true },
            outputChannelName: "PowerBuilder-LS Logs",
            errorHandler: {
                error: (...args) => ({ action: ls.ErrorAction.Shutdown }),
                closed: async () => {
                    const msg = await vsc.window.showErrorMessage("PowerBuilder-LS stopped unexpectedly.", "Restart", "Show Output");
                    let ret = { action: ls.CloseAction.DoNotRestart, handled: true };
                    switch (msg) {
                        case "Restart":
                            await this.destroy();
                            await this.create();
                            break;
                        case "Show Output":
                            clientOptions.outputChannel?.show();
                            break;
                    }
                    return ret;
                }
            }
        };

        this.client = new ls.LanguageClient("powerbuilder-ls", "PowerBuilder Language Server", serverOptions, clientOptions);
        await this.client.start();
        return true;
    }

    async destroy() {
        console.info("Destroying PowerBuilder-LS");
        try {
            await this.client?.stop();
        } catch (error) {
            console.warn("Failed to stop old PowerBuilder-LS client", error);
        }
        await this.client?.dispose()
        this.client = null;
    }

    dispose() {
        console.info("Disposing PowerBuilder-LS");
        this.channels?.map(ch => ch.dispose());
    }
}
