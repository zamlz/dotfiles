" ----------------------------------------------------------------------------
"  _    ___              ______            _____          ____      _ __
" | |  / (_)___ ___     / ____/___  ____  / __(_)___ _   /  _/___  (_) /_
" | | / / / __ `__ \   / /   / __ \/ __ \/ /_/ / __ `/   / // __ \/ / __/
" | |/ / / / / / / /  / /___/ /_/ / / / / __/ / /_/ /  _/ // / / / / /_
" |___/_/_/ /_/ /_/   \____/\____/_/ /_/_/ /_/\__, /  /___/_/ /_/_/\__/
"                                            /____/
" ----------------------------------------------------------------------------

" Fun Fact :
" According to (hillelwayne.com/post/always-more-history/),
" vi uses hjkl as the arrow bindings not but its own design. But because the
" software was developed on the ADM-3A. This keyboard didn't have dedicated
" arrow keys, but made use of control characters (the first 32 characters of
" the 1967 ASCII Table). As a result, ctrl-{h,j,k,l} became the designated
" keys for moving the cursor around. When Bill Joy developed vi, he used
" hjkl for the arrow keys as it was only naturally having used the ADM-32.

" NOTE: To debug runtime, make sure to take a look at `:scriptnames`

" ===================
"  VIM CONFIGURATION
" ===================

" make sure vimplug loads all plugins we want first, then we'll let the
" runtime handle configuring this at the right time
runtime init_plugins.vim

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

" Finally, let's remap the leader key (clear any prexisting uses of <SPACE>)
" FIXME: figure out a better way to utilize my leader keys
nnoremap <SPACE> <Nop>
let mapleader=" "

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

highlight Conceal ctermbg=None ctermfg=darkblue

" custom keybindings
" ------------------

" Sets a keybind to turn off highlighted searches
nnoremap <leader><space> :nohlsearch<CR>

" Keybinds to quickly switch buffers
noremap <S-j> :bn<CR>
noremap <S-k> :bp<CR>

noremap <F6> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") .
    \ '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
    \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">" .
    \ " FG:" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"fg#")<CR>

" vim:ft=vim
