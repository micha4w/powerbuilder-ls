# [WIP] PowerBuilder-LS

> [!NOTE]  
> This project is mainly for me to learn Rust goodly + try around with LSPs.
> Im still a noob at Rust, so the code quality is not very high.

Currently working features:
 - Parsing most parts of PowerScript
 - Linting Statements and complaining about type Errors

TODOs:
 - Finish parser (SQL, throws)
 - Add smarter linting (loops, try catch)
 - Better Autocomplete (context based, types)
 - Hover Diagnostics
 - Keeping track of file changes without having to wait for a save


## NeoVim Testing
```lua
-- Starting an LSP
vim.lsp.start({ name = 'powerbuilder-ls', cmd = {vim.fn.getcwd() .. '/../target/debug/powerbuilder-ls'}, root_dir = vim.fn.getcwd() })

-- Stopping an LSP - or just use :e
vim.lsp.stop_client(vim.lsp.get_active_clients())
```