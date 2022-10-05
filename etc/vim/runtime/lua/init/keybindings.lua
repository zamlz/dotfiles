--[[--------------------------------------------------------------------------
    __ __           __    _           ___
   / //_/__  __  __/ /_  (_)___  ____/ (_)___  ____ ______
  / ,< / _ \/ / / / __ \/ / __ \/ __  / / __ \/ __ `/ ___/
 / /| /  __/ /_/ / /_/ / / / / / /_/ / / / / / /_/ (__  )
/_/ |_\___/\__, /_.___/_/_/ /_/\__,_/_/_/ /_/\__, /____/
          /____/                            /____/
--------------------------------------------------------------------------]]--

-- Keybinding helper functions

function map(mode, shortcut, command)
  vim.api.nvim_set_keymap(
    mode,
    shortcut,
    command,
    {noremap=true, silent=true}
  )
end

function nmap(shortcut, command)
  map('n', shortcut, command)
end

function imap(shortcut, command)
  map('i', shortcut, command)
end

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

-- FIXME: translate this into a meaninful command to use for lua
--[[
noremap <F6> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") .
    \ '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
    \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">" .
    \ " FG:" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"fg#")<CR>
--]]
