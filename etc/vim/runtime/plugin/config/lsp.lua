-- LSP

-- Mason configuration (needs to be configured before we configure lspconfig
-- This guarantees that the following language servers are installed.
-- You need to configure lsp down below as well...
require("mason").setup()
require("mason-lspconfig").setup({
    ensure_installed = {
        "bashls",
        "clangd",
        "cssls",
        "html",
        "jsonls",
        "lua_ls",
        "pyright",
        "rust_analyzer",
    }
})

-- LSP Config configuration

-- Mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
local opts = { noremap=true, silent=true }
vim.keymap.set('n', '<space>lq', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '<space>lk', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', '<space>lj', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<space>lQ', vim.diagnostic.setloclist, opts)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
    -- Enable completion triggered by <c-x><c-o>
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- Mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    local bufopts = { noremap=true, silent=true, buffer=bufnr }
    vim.keymap.set('n', '<space>lD', vim.lsp.buf.declaration, bufopts)
    vim.keymap.set('n', '<space>ld', vim.lsp.buf.definition, bufopts)
    vim.keymap.set('n', '<space>ll', vim.lsp.buf.hover, bufopts)
    vim.keymap.set('n', '<space>li', vim.lsp.buf.implementation, bufopts)
    vim.keymap.set('n', '<space>l?', vim.lsp.buf.signature_help, bufopts)
    vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, bufopts)
    vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
    vim.keymap.set('n', '<space>wl', function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, bufopts)
    vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, bufopts)
    vim.keymap.set('n', '<space>lR', vim.lsp.buf.rename, bufopts)
    vim.keymap.set('n', '<space>la', vim.lsp.buf.code_action, bufopts)
    --vim.keymap.set('n', '<space>lr', vim.lsp.buf.references, bufopts)
    vim.keymap.set('n', '<space>f', function() vim.lsp.buf.format { async = true } end, bufopts)
end

local lsp_flags = {
    -- This is the default in Nvim 0.7+
    debounce_text_changes = 150,
}

-- Note this will use shellcheck if it's installed externally!
require('lspconfig')['bashls'].setup{}

require('lspconfig')['clangd'].setup{}

require('lspconfig')['cssls'].setup{}

require('lspconfig')['html'].setup{}

require('lspconfig')['jsonls'].setup{}

require('lspconfig')['lua_ls'].setup{
    settings = {
        Lua = {
            diagnostics = {
                globals = { 'vim' }
            }
        }
    }
}

require('lspconfig')['pyright'].setup{
    on_attach = on_attach,
    flags = lsp_flags,
}

require('lspconfig')['rust_analyzer'].setup{
    on_attach = on_attach,
    flags = lsp_flags,
    -- Server-specific settings...
    settings = {
      ["rust-analyzer"] = {}
    }
}
