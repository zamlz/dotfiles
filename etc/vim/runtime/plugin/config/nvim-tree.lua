--[[--------------------------------------------------------------------------
    _   __      _               ______             
   / | / /   __(_)___ ___      /_  __/_______  ___ 
  /  |/ / | / / / __ `__ \______/ / / ___/ _ \/ _ \
 / /|  /| |/ / / / / / / /_____/ / / /  /  __/  __/
/_/ |_/ |___/_/_/ /_/ /_/     /_/ /_/   \___/\___/ 
--------------------------------------------------------------------------]]--

-- disable netrw (maybe do it at the start of init?)
vim.g.loaded = 1
vim.g.loaded_netrwPlugin = 1

-- FIXME: Configure tree view properly
require('nvim-tree').setup()

-- Set leader keybinding
require("common.keybinder")
nmap("<leader>n", ":NvimTreeToggle<CR>")
