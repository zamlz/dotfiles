
-- Lua conversion time
vim.cmd([[

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

" my choice in colorscheme
colorscheme peachpuff_custom
]])
