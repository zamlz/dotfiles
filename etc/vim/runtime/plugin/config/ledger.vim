" ----------------------------------------------------------------------------
"  _    ___              __             __
" | |  / (_)___ ___     / /   ___  ____/ /___ ____  _____
" | | / / / __ `__ \   / /   / _ \/ __  / __ `/ _ \/ ___/
" | |/ / / / / / / /  / /___/  __/ /_/ / /_/ /  __/ /
" |___/_/_/ /_/ /_/  /_____/\___/\__,_/\__, /\___/_/
"                                     /____/
" ----------------------------------------------------------------------------

let g:ledger_maxwidth = 80
let g:ledger_bin = 'ledger'
let g:ledger_extra_options = '--pedantic --explicit'
let g:ledger_align_at = 77
let g:ledger_date_format = '%Y-%m-%d'

" Ledger remap tab completion
au FileType ledger inoremap <silent> <Tab>
            \ <C-r>=ledger#autocomplete_and_align()<CR>
au FileType ledger vnoremap <silent> <Tab>
            \ :LedgerAlign<CR>
au FileType ledger nnoremap <silent> <C-m>
            \ :silent<space>make<bar>redraw!<bar>cwindow<CR>
au FileType ledger nnoremap <silent> <C-s>
            \ :call ledger#transaction_state_toggle(line('.'), ' !*?')<CR>
