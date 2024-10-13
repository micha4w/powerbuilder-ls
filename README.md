## NeoVim Testing
```lua
-- Starting an LSP
vim.lsp.start({ name = 'powerbuilder-ls', cmd = {vim.fn.getcwd() .. '/../target/debug/powerbuilder-ls'}, root_dir = vim.fn.getcwd() })

-- Stopping an LSP - or just use :e
vim.lsp.stop_client(vim.lsp.get_active_clients())
```