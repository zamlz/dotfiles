" ====================
"  VIM PLUGIN INSTALL
" ====================

call plug#begin('~/.vim/plugged')

" vim-airline: Lean & mean status/tabline for vim thats light as air
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'mbbill/undotree'

Plug 'jamessan/vim-gnupg'

" Language Server
" - Install support for other langauges via:
"   > PYTHON :CocInstall coc-pyright
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Language Based Plugins
Plug 'ledger/vim-ledger'
Plug 'nathangrigg/vim-beancount'

call plug#end()
