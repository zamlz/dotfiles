--[[--------------------------------------------------------------------------
    __ __           __    _           ___
   / //_/__  __  __/ /_  (_)___  ____/ (_)___  ____ ______
  / ,< / _ \/ / / / __ \/ / __ \/ __  / / __ \/ __ `/ ___/
 / /| /  __/ /_/ / /_/ / / / / / /_/ / / / / / /_/ (__  )
/_/ |_\___/\__, /_.___/_/_/ /_/\__,_/_/_/ /_/\__, /____/
          /____/                            /____/
--------------------------------------------------------------------------]]--

-- Get the keybinding helper functions
require("common.keybinder")

-- Finally, let's remap the leader key (clear any prexisting uses of <SPACE>)
-- FIXME: figure out a better way to utilize my leader keys
vim.cmd('nnoremap <SPACE> <Nop>')
vim.cmd('let mapleader=" "')

-- Sets a keybind to turn off highlighted searches
-- nnoremap <leader><space> :nohlsearch<CR>
nmap("<leader><space>", ":nohlsearch<CR>")

-- Keybinds to quickly switch buffers
-- noremap <S-j> :bn<CR>
nmap("<S-j>", ":bn<CR>")
-- noremap <S-k> :bp<CR>
nmap("<S-k>", ":bp<CR>")

-- Keybinds to quickly move through window panes
-- refer to the custom window plugin I've written for autocreation commands
nmap("<C-h>", ":wincmd h<CR>")
nmap("<C-j>", ":wincmd j<CR>")
nmap("<C-k>", ":wincmd k<CR>")
nmap("<C-l>", ":wincmd l<CR>")

-- FIXME: translate this into a meaninful command to use for lua
vim.cmd([[
noremap <F6> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") .
    \ '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
    \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">" .
    \ " FG:" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"fg#")<CR>
]])
