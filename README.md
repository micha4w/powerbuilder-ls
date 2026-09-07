# [WIP] PowerBuilder-LS

> [!NOTE]  
> This project is mainly for me to learn Rust goodly + try around with LSPs.
> Im still a noob at Rust, so the code quality is not very high.

Currently working features:
 - Parsing most parts of PowerScript
 - Linting Statements and complaining about type Errors
 - LSP Features (only for the most basic cases):
   - Go to definition
   - Go to type definition
   - Completion (only with objects)
   - Hover

TODOs:
 - Finish parser (SQL, throws, pbni library)
 - Add smarter linting (loops, try catch)
 - Make comments not be ignored (Help String, autocomplete inside comments)
 - Make the LSP Features work in more cases
 - Dont ignore groups (_**group**_`class)

## Building
Best to build this in release mode because speed matters:
```rust
cargo build --release
```


## NeoVim Testing
```lua
-- Starting an LSP
vim.lsp.start({ name = 'powerbuilder-ls', cmd = {vim.fn.getcwd() .. '/../target/debug/powerbuilder-ls'}, root_dir = vim.fn.getcwd() })

-- Stopping an LSP - or just use :e
vim.lsp.stop_client(vim.lsp.get_active_clients())
```