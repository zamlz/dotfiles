-- Lua conversion time
vim.cmd([[
" Finally, let's remap the leader key (clear any prexisting uses of <SPACE>)
" FIXME: figure out a better way to utilize my leader keys
nnoremap <SPACE> <Nop>
let mapleader=" "

" Sets a keybind to turn off highlighted searches
nnoremap <leader><space> :nohlsearch<CR>

" Keybinds to quickly switch buffers
noremap <S-j> :bn<CR>
noremap <S-k> :bp<CR>

noremap <F6> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") .
    \ '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
    \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">" .
    \ " FG:" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"fg#")<CR>

]])
