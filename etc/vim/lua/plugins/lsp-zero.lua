-- Basically use LSP Zero to setup my IDE environment
-- This kinda configures a lot of different plugins

-- colors for nvim-cmp

-- deprecated results
vim.api.nvim_set_hl(0, 'CmpItemAbbrDeprecated', { strikethrough=true })
-- matching text
vim.api.nvim_set_hl(0, 'CmpItemAbbrMatch', { ctermfg='blue' })
vim.api.nvim_set_hl(0, 'CmpItemAbbrMatchFuzzy', { link='CmpItemAbbrMatch' })
-- Class
vim.api.nvim_set_hl(0, 'CmpItemKindClass', { ctermfg='darkyellow' })
-- Variables
vim.api.nvim_set_hl(0, 'CmpItemKindVariable', { ctermfg='darkcyan' })
vim.api.nvim_set_hl(0, 'CmpItemKindInterface', { link='CmpItemKindVariable' })
vim.api.nvim_set_hl(0, 'CmpItemKindText', { link='CmpItemKindVariable' })
-- Functions
vim.api.nvim_set_hl(0, 'CmpItemKindFunction', { ctermfg='darkmagenta' })
vim.api.nvim_set_hl(0, 'CmpItemKindMethod', { link='CmpItemKindFunction' })
-- Keywords
vim.api.nvim_set_hl(0, 'CmpItemKindKeyword', {})
vim.api.nvim_set_hl(0, 'CmpItemKindProperty', { link='CmpItemKindKeyword' })
vim.api.nvim_set_hl(0, 'CmpItemKindUnit', { link='CmpItemKindKeyword' })
-- Constants
vim.api.nvim_set_hl(0, 'CmpItemKindConstant', { ctermfg='darkred' })
-- Snippets
vim.api.nvim_set_hl(0, 'CmpItemKindSnippet', { ctermfg='darkblue' })

return {
    'VonHeikemen/lsp-zero.nvim',
    branch = 'v2.x',
    dependencies = {
        -- LSP Support
        {'neovim/nvim-lspconfig'},             -- Required
        {                                      -- Optional
            'williamboman/mason.nvim',
            build = function()
                pcall(vim.cmd, 'MasonUpdate')
            end,
        },
        {'williamboman/mason-lspconfig.nvim'}, -- Optional

        -- Autocompletion
        {'hrsh7th/nvim-cmp'},         -- Required
        {'hrsh7th/cmp-nvim-lsp'},     -- Required
        {'hrsh7th/cmp-nvim-lua'},     -- Optional
        {'hrsh7th/cmp-buffer'},       -- Optional
        {'hrsh7th/cmp-path'},         -- Optional
        {'saadparwaiz1/cmp_luasnip'}, -- Optional
        {'onsails/lspkind.nvim'},     -- Optional

        -- Snippets
        {'L3MON4D3/LuaSnip'},             -- Required
        {'rafamadriz/friendly-snippets'}, -- Optional

        -- Telescope stuff
        {'nvim-telescope/telescope.nvim'},
    },
    config = function()
        -- GLOBAL LSP ZERO CONFIGURATION
        local lsp = require('lsp-zero').preset({})
        lsp.on_attach(function(client, bufnr)
            lsp.default_keymaps({
                buffer = bufnr,
                omit = {'gl', 'gr', 'K', '[d', ']d', '<F2>', '<F4>'},
                preserve_mappings = false
            })
            -- Custom Telescope LSP functions
            vim.keymap.set('n', 'gl', '<CMD>lua vim.lsp.buf.hover()<CR>', {buffer = true})
            vim.keymap.set('n', 'gr', '<CMD>Telescope lsp_references theme=dropdown<CR>', {buffer = true})
            vim.keymap.set('n', 'gR', '<CMD>lua vim.lsp.buf.rename()<CR>', {buffer = true})
            vim.keymap.set('n', 'ga', '<CMD>lua vim.lsp.buf.code_action()<CR>', {buffer = true})
            vim.keymap.set('n', 'gq', '<CMD>lua vim.diagnostic.open_float()<CR>', {buffer = true})
            vim.keymap.set('n', 'gk', '<CMD>lua vim.diagnostic.goto_prev()<CR>', {buffer = true})
            vim.keymap.set('n', 'gj', '<CMD>lua vim.diagnostic.goto_next()<CR>', {buffer = true})
        end)
        -- set a list of required servers
        lsp.ensure_installed({
            'bashls',
            'clangd',
            'cssls',
            'html',
            'jsonls',
            'lua_ls',
            'pyright',
            'rust_analyzer',
        })
        lsp.set_sign_icons({
            error = '✘',
            warn = '▲',
            hint = '⚑',
            info = '»'
        })
        -- required if we want nvim lua to be respected properly by lsp
        require('lspconfig').lua_ls.setup(lsp.nvim_lua_ls())
        lsp.setup()

        -- CMP CONFIG (You need to setup `cmp` after lsp-zero)
        local cmp = require('cmp')
        local cmp_action = require('lsp-zero').cmp_action()
        require('luasnip.loaders.from_vscode').lazy_load()
        cmp.setup({
            sources = {
                {name = 'path'},
                {name = 'nvim_lsp'},
                {name = 'nvim_lua'},
                {name = 'buffer', keyword_length = 3},
                {name = 'luasnip', keyword_length = 2},
            },
            mapping = {
                -- super tab shenanigans
                ['<Tab>'] = cmp_action.luasnip_supertab(),
                ['<S-Tab>'] = cmp_action.luasnip_shift_supertab(),

                -- `Enter` key to confirm completion
                ['<CR>'] = cmp.mapping.confirm({select = false}),
                ['<C-e>'] = cmp.mapping.abort(),

                -- Ctrl+Space to trigger completion menu
                ['<C-Space>'] = cmp.mapping.complete(),

                -- Navigate between snippet placeholder
                ['<C-f>'] = cmp_action.luasnip_jump_forward(),
                ['<C-b>'] = cmp_action.luasnip_jump_backward(),
            },
            window = {
                -- FIXME: do not use telescope highlight for this and find a suitable alternative
                completion = cmp.config.window.bordered({
                    winhighlight = 'Normal:None,FloatBorder:None,CursorLine:TelescopeSelection,Search:None'
                }),
                -- FIXME: do not use telescope highlight for this and find a suitable alternative
                documentation = cmp.config.window.bordered({
                    winhighlight = 'Normal:None,FloatBorder:None,CursorLine:TelescopeSelection,Search:None'
                }),
            },
            formatting = {
                fields = {'abbr', 'kind', 'menu'},
                format = require('lspkind').cmp_format({
                    mode = 'text', -- show only symbol annotations
                    maxwidth = 50, -- prevent the popup from showing more than provided characters
                    ellipsis_char = '...', -- when popup menu exceed maxwidth replace with ...
                })
            }
        })
    end
}
