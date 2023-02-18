--[[--------------------------------------------------------------------------
   ____        __  _                _____      __          __
  / __ \____  / /_(_)___  ____     / ___/___  / /__  _____/ /_
 / / / / __ \/ __/ / __ \/ __ \    \__ \/ _ \/ / _ \/ ___/ __/
/ /_/ / /_/ / /_/ / /_/ / / / /   ___/ /  __/ /  __/ /__/ /_
\____/ .___/\__/_/\____/_/ /_/   /____/\___/_/\___/\___/\__/
    /_/
--------------------------------------------------------------------------]]--

-- FIXME: convert vim.cmd statements to proper opt statements

-- make sure to set `mapleader` before lazy so your mappings are correct
vim.g.mapleader = " "
vim.g.maplocalleader = " "

local opt = vim.opt

-- fringe line numbers
opt.number         = true
opt.relativenumber = true

-- cursor crosshair and soft-thresholds
opt.ruler        = true
opt.cursorline   = true
opt.cursorcolumn = true
opt.colorcolumn  = {80, 128}

-- text display
opt.wrap = false

-- put and end to this tab vs. spaces war
opt.tabstop     = 4    -- visual spaces per tab character
opt.expandtab   = true -- expand <TAB> key to spaces in insert mode
opt.softtabstop = 4    -- number of spaces to insert for a tab
opt.shiftwidth  = 4    -- number of spaces used for each autoindent step

-- code concealing
opt.conceallevel = 0
--set concealcursor-=n

-- code folding
opt.foldenable     = false
opt.foldmethod     = "expr"
opt.foldexpr       = "nvim_treesitter#foldexpr()"
opt.foldlevelstart = 10
opt.foldnestmax    = 10

-- filetype specific options
vim.cmd("filetype plugin on")
vim.cmd("filetype indent on")

-- dynamic configuration via source files
opt.modeline = true

-- let's use the mouse scrolling cause we can
opt.mouse = "a"

-- Custom file search path
vim.cmd("set path+=**")

-- rice out neovim
opt.syntax     = "on"
opt.lazyredraw = true
opt.showmode   = false -- don't show the current mode under the modeline
vim.cmd("colorscheme peachpuff_custom")
