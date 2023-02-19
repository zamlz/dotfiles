--[[--------------------------------------------------------------------------
                   _    ___              ______            _____
   ____  ___  ____| |  / (_)___ ___     / ____/___  ____  / __(_)___ _
  / __ \/ _ \/ __ \ | / / / __ `__ \   / /   / __ \/ __ \/ /_/ / __ `/
 / / / /  __/ /_/ / |/ / / / / / / /  / /___/ /_/ / / / / __/ / /_/ /
/_/ /_/\___/\____/|___/_/_/ /_/ /_/   \____/\____/_/ /_/_/ /_/\__, /
                                                             /____/
--------------------------------------------------------------------------]]--

--[[
Fun Fact :
   According to (hillelwayne.com/post/always more history/),
   vi uses hjkl as the arrow bindings not but its own design. But because the
   software was developed on the ADM 3A. This keyboard didn't have dedicated
   arrow keys, but made use of control characters (the first 32 characters of
   the 1967 ASCII Table). As a result, ctrl {h,j,k,l} became the designated
   keys for moving the cursor around. When Bill Joy developed vi, he used
   hjkl for the arrow keys as it was only naturally having used the ADM 32.
--]]

-- NOTE: To debug runtime, make sure to take a look at `:scriptnames`

require('config.options')
require('config.keymaps')

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.runtimepath:prepend(lazypath)

--[[
PLUGINS TO TRY:
- nvim-cmp
- treesitter-playground
--]]
--
require('lazy').setup({{import = 'plugins'}})

-- The runtime files will sourced after init is finished (:h :runtime)
--[[

    -- Language Server Support
    'williamboman/mason.nvim',
    'williamboman/mason-lspconfig.nvim',
    'neovim/nvim-lspconfig',

    -- Language Based Plugins

    -- Markup Language System
--]]

