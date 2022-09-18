" ========================
"  Vim Configuration File
" ========================

" Fun Fact :
" According to (hillelwayne.com/post/always-more-history/),
" vi uses hjkl as the arrow bindings not but its own design. But because the
" software was developed on the ADM-3A. This keyboard didn't have dedicated
" arrow keys, but made use of control characters (the first 32 characters of
" the 1967 ASCII Table). As a result, ctrl-{h,j,k,l} became the designated
" keys for moving the cursor around. When Bill Joy developed vi, he used
" hjkl for the arrow keys as it was only naturally having used the ADM-32.

runtime init_plugins.vim

" ======================
"  PLUGIN CONFIGURATION
" ======================

" vim-airline-themes
" ------------------
" let g:airline_theme = 'base16_gruvbox_dark_hard'

" git-gutter
" ----------
nmap <C-M-j> <Plug>(GitGutterNextHunk)
nmap <C-M-k> <Plug>(GitGutterPrevHunk)
let g:gitgutter_set_sign_backgrounds = 1

" vim ledger
" ----------
let g:ledger_maxwidth = 80
let g:ledger_bin = 'ledger'
let g:ledger_extra_options = '--pedantic --explicit'
let g:ledger_align_at = 77
let g:ledger_date_format = '%Y-%m-%d'

" ===================
"  VIM CONFIGURATION
" ===================

" some saner defaults
" -------------------
set nocompatible
set relativenumber number " Show Line numbers
set ruler                 " Show line and column number of the cursor
set cursorline            " highlight current line
set cursorcolumn          " Create a column for where the cursor is
set colorcolumn=80        " Create a column to show where 80 chars are
set modeline              " Allow source files to configure vim as well
set nowrap

set tabstop=4            " number of visual spaces per TAB
set softtabstop=4        " number of space in TAB while editing
set expandtab            " TABs are now SPACEs
set shiftwidth=4         " Allows helps to make it 4 spaces in neovim

filetype plugin on
filetype indent on       " Load filetype-specific indent files
syntax on

set wildmenu             " visual autocomplete for command menu
set lazyredraw           " redraw only when we need to

set showmatch            " highlight matching brackets [{()}]
set incsearch            " search as characters are entered
set hlsearch             " highlight matches

set noshowmode           " Stop showing the default mode

set mouse=a              " Set mouse wheel to scroll

set conceallevel=2       " Code concealing
set concealcursor-=n

set foldenable           " Enable Code Folding
set foldlevelstart=10
set foldnestmax=10
set foldmethod=indent    " fold based on ident level

set path+=**             " Custom file search

" vim colorschemes
" ----------------

" Use peachpuff built-in colorscheme as the base
colorscheme peachpuff

" Color of the Columns
highlight ColorColumn ctermbg=black
highlight CursorColumn ctermbg=black
highlight VertSplit ctermfg=black

" Change the default coloring of line numbers
highlight LineNr ctermfg=darkgrey

" Change colorscheme of Pmenus
highlight Pmenu ctermfg=darkgrey ctermbg=black

" Set background color of folded blocks
highlight Folded ctermbg=black

" Some syntax highlighting changes (maybe move this to its own file)
highlight Function ctermfg=darkblue
highlight String ctermfg=darkgreen
"highlight Comment ctermfg=darkgrey
highlight Exception ctermfg=darkred

" Fix colors on gitgutter after colorscheme has been set
highlight SignColumn ctermbg=None
highlight GitGutterAdd ctermbg=None ctermfg=green
highlight GitGutterChange ctermbg=None ctermfg=yellow
highlight GitGutterDelete ctermbg=None ctermfg=red

" Change colors to be more intuitive for vimwiki
highlight VimwikiHeader1 ctermbg=None ctermfg=darkred
highlight VimwikiHeader2 ctermbg=None ctermfg=darkblue
highlight VimwikiHeader3 ctermbg=None ctermfg=darkgreen
highlight VimwikiHeader4 ctermbg=None ctermfg=yellow
highlight VimwikiHeader5 ctermbg=None ctermfg=cyan
highlight VimwikiHeader6 ctermbg=None ctermfg=magenta

highlight Conceal ctermbg=None ctermfg=darkblue

" custom keybindings
" ------------------

" Sets a keybind to turn off highlighted searches
nnoremap <leader><space> :nohlsearch<CR>

" Keybinds to quickly switch buffers
noremap <S-j> :bn<CR>
noremap <S-k> :bp<CR>

" Custom git mappings (is fugitive or gitgutter better to use instead?)
noremap <F2> :Git <CR>
noremap <F3> :Git diff<CR>
noremap <F4> :Gclog<CR>

" Key binds to toggle the Undo Tree
noremap <F5> :UndotreeToggle<CR>

noremap <F6> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") .
    \ '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
    \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">" .
    \ " FG:" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"fg#")<CR>

" Ledger remap tab completion
au FileType ledger inoremap <silent> <Tab>
            \ <C-r>=ledger#autocomplete_and_align()<CR>
au FileType ledger vnoremap <silent> <Tab>
            \ :LedgerAlign<CR>
au FileType ledger nnoremap <silent> <C-m>
            \ :silent<space>make<bar>redraw!<bar>cwindow<CR>
au FileType ledger nnoremap <silent> <C-s>
            \ :call ledger#transaction_state_toggle(line('.'), ' !*?')<CR>

" Vimwiki doc pubs open
autocmd FileType vimwiki nnoremap <Leader>p :silent !pubs doc open %:r<CR>

" ======================
"  Custom Vim Functions
" ======================

" Trim Whitespace
" ---------------
" Trim the whitespace present in a file

fun! TrimWhitespace()
    let l:save = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:save)
endfun
command! TrimWhitespace call TrimWhitespace()

" Toggle Calendar
" ---------------
" Toggle calendar view within view if in vim wiki

function! ToggleCalendar()
    execute ":Calendar"
    if exists("g:calendar_open")
        if g:calendar_open == 1
            execute "q"
            unlet g:calendar_open
        else
            g:calendar_open = 1
        end
    else
        let g:calendar_open = 1
    end
endfunction
:autocmd FileType vimwiki map <leader>c :call ToggleCalendar()<CR>

" Workspace Management
" --------------------

function! WinMove(key)
    let t:curwin = winnr()
    exec "wincmd ".a:key
    if (t:curwin == winnr())
        if (match(a:key,'[jk]'))
            wincmd v
        else
            wincmd s
        endif
        exec "wincmd ".a:key
    endif
endfunction

nnoremap <silent> <C-h> :call WinMove('h')<CR>
nnoremap <silent> <C-j> :call WinMove('j')<CR>
nnoremap <silent> <C-k> :call WinMove('k')<CR>
nnoremap <silent> <C-l> :call WinMove('l')<CR>

" vim:ft=vim
