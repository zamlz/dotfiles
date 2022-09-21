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

call plug#begin('~/.vim/plugged')

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" vim-airline: Lean & mean status/tabline for vim thats light as air
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
