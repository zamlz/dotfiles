--[[--------------------------------------------------------------------------
  _    ___              __             __
 | |  / (_)___ ___     / /   ___  ____/ /___ ____  _____
 | | / / / __ `__ \   / /   / _ \/ __  / __ `/ _ \/ ___/
 | |/ / / / / / / /  / /___/  __/ /_/ / /_/ /  __/ /
 |___/_/_/ /_/ /_/  /_____/\___/\__,_/\__, /\___/_/
                                     /____/
--------------------------------------------------------------------------]]--

vim.g.ledger_maxwidth = 80
vim.g.ledger_bin = 'ledger'
vim.g.ledger_extra_options = '--pedantic --explicit'
vim.g.ledger_align_at = 77
vim.g.ledger_date_format = '%Y-%m-%d'

-- Ledger remap tab completion
vim.cmd("au FileType ledger inoremap <silent> <Tab> <C-r>=ledger#autocomplete_and_align()<CR>")
vim.cmd("au FileType ledger vnoremap <silent> <Tab> :LedgerAlign<CR>")
vim.cmd("au FileType ledger nnoremap <silent> <C-m> :silent<space>make<bar>redraw!<bar>cwindow<CR>")
vim.cmd("au FileType ledger nnoremap <silent> <C-s> :call ledger#transaction_state_toggle(line('.'), ' !*?')<CR>")

return {
    'ledger/vim-ledger',
    ft = 'ledger'
}
