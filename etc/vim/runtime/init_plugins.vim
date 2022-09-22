" ----------------------------------------------------------------------------
"  _    ___              ____  __            _
" | |  / (_)___ ___     / __ \/ /_  ______ _(_)___  _____
" | | / / / __ `__ \   / /_/ / / / / / __ `/ / __ \/ ___/
" | |/ / / / / / / /  / ____/ / /_/ / /_/ / / / / (__  )
" |___/_/_/ /_/ /_/  /_/   /_/\__,_/\__, /_/_/ /_/____/
"                                  /____/
" ----------------------------------------------------------------------------

" PLUGINS TO TRY:
" vim-which-key

" ----------------------------------------------------------------------------

" Install VimPlug
" ---------------

let url = 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
let vimplug_file = data_dir.'/autoload/plug.vim'
let plugin_dir = data_dir.'/plugged'

" download the plugin manager if it isn't installed already
if empty(glob(vimplug_file))
  silent execute '!curl -fLo '.vimplug_file.' --create-dirs '.url
endif

" installing any missing plugins
autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \| PlugInstall --sync | source $MYVIMRC
\| endif

" Use VimPlug to specify plguins
" ------------------------------

call plug#begin(plugin_dir)

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

Plug 'mbbill/undotree'

Plug 'jamessan/vim-gnupg'

" Language Server
" (Needs nodejs; but something in my package list pulls this dependency)
" - Install support for other langauges via:
"   > PYTHON :CocInstall coc-pyright
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Language Based Plugins
Plug 'ledger/vim-ledger'
Plug 'nathangrigg/vim-beancount'

call plug#end()
