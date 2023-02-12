--[[--------------------------------------------------------------------------
_    ___              ____  __            _
| |  / (_)___ ___     / __ \/ /_  ______ _(_)___  _____
| | / / / __ `__ \   / /_/ / / / / / __ `/ / __ \/ ___/
| |/ / / / / / / /  / ____/ / /_/ / /_/ / / / / (__  )
|___/_/_/ /_/ /_/  /_/   /_/\__,_/\__, /_/_/ /_/____/
                                 /____/
--------------------------------------------------------------------------]]--

--[[
PLUGINS TO TRY:
- nvim-cmp
- treesitter-playground
--]]

local ensure_packer = function()
    local fn = vim.fn
    local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
    if fn.empty(fn.glob(install_path)) > 0 then
        fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
        vim.cmd [[packadd packer.nvim]]
        return true
    end
    return false
end

local packer_bootstrap = ensure_packer()

return require('packer').startup(function(use)

    -- Packer can manage itself
    use 'wbthomason/packer.nvim'

    -- Aesthetic Status Bar
    use {
        'nvim-lualine/lualine.nvim',
        requires = {
          'kyazdani42/nvim-web-devicons',
          opt = true
        }
    }

    -- Git plugis
    use 'lewis6991/gitsigns.nvim'
    use 'jreybert/vimagit'

    -- Undo Tree
    use 'mbbill/undotree'

    -- Filesystem Tree
    use 'preservim/nerdtree'

    -- Nice Fuzzy searcher
    use {
        'nvim-telescope/telescope.nvim', tag = '0.1.0',
        requires = { {'nvim-lua/plenary.nvim'} }
    }

    -- Encrypted File Support
    use 'jamessan/vim-gnupg'

    -- Language Server Support
    use 'williamboman/mason.nvim'
    use 'williamboman/mason-lspconfig.nvim'
    use 'neovim/nvim-lspconfig'

    -- Language Based Plugins
    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate'
    }
    use {
        'rust-lang/rust.vim',
        ft = {'rust'}
    }
    use {
        'ledger/vim-ledger',
        ft = {'ledger'}
    }
    use {
        'nathangrigg/vim-beancount',
        ft = {'beancount'}
    }

    -- Markup Language System
    use 'tpope/vim-markdown'
    use {
        'nvim-neorg/neorg',
        run = ":Neorg sync-parsers",
        requires = "nvim-lua/plenary.nvim",
    }

    -- Automatically set up your configuration after cloning packer.nvim
    -- Put this at the end after all plugins
    if packer_bootstrap then
        require('packer').sync()
    end
end)
