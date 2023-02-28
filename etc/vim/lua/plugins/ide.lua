--[[--------------------------------------------------------------------------
    ________  ______   ______            _____
   /  _/ __ \/ ____/  / ____/___  ____  / __(_)___ _
   / // / / / __/    / /   / __ \/ __ \/ /_/ / __ `/
 _/ // /_/ / /___   / /___/ /_/ / / / / __/ / /_/ /
/___/_____/_____/   \____/\____/_/ /_/_/ /_/\__, /
                                           /____/
--------------------------------------------------------------------------]]--

-- list of which servers to install and their configurations
-- (this is the settings argument!)
local server_config = {
    bashls = {},
    clangd = {},
    cssls = {},
    html = {},
    jsonls = {},
    lua_ls = {
        Lua = {
            diagnostics = {
                globals = { 'vim' }
            }
        }
    },
    pyright = {},
    rust_analyzer = {},
}

-- get just the keys from the above table here
local server_list = {}
local idx = 0
for k, _ in pairs(server_config) do
    idx = idx + 1
    server_list[idx] = k
end

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

-- Local LSP configuration flags
local lsp_flags = {
    debounce_text_changes = 150, -- This is the default in Nvim 0.7+
}

local mason_config_opts = {
    ui = {
        icons = {
            package_installed = "✓",
            package_pending = "➜",
            package_uninstalled = "✗"
        }
    }
}

-- lazy config
return {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
        'williamboman/mason.nvim',
        'williamboman/mason-lspconfig.nvim',
    },
    config = function()
        -- Mason configuration (needs to be configured before we configure lspconfig
        require("mason").setup(mason_config_opts)
        -- This guarantees that the following language servers are installed before
        require("mason-lspconfig").setup({ ensure_installed = server_list })
        -- Now we can configure each of our lsp servers
        for _, server in ipairs(server_list) do
            require('lspconfig')[server].setup{
                on_attach = on_attach,
                flags = lsp_flags,
                settings = server_config[server]
            }
        end
    end
}
