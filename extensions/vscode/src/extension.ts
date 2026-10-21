import "source-map-support/register";
import * as vsc from 'vscode';
import { PBLanguageClient } from "./ls";

let client: PBLanguageClient;

export async function activate(context: vsc.ExtensionContext) {
	client = new PBLanguageClient(context);


	vsc.workspace.onDidChangeConfiguration(async (e) => {
		if (!e.affectsConfiguration("powerbuilder.languageServer")) return;

		await client.destroy();
		await client.create();
	});

	client.create();
}

export async function deactivate() {
	await client?.destroy();
}