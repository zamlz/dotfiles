--[[--------------------------------------------------------------------------
    __                                                 
   / /   ____ _____  ____ ___  ______ _____ ____  _____
  / /   / __ `/ __ \/ __ `/ / / / __ `/ __ `/ _ \/ ___/
 / /___/ /_/ / / / / /_/ / /_/ / /_/ / /_/ /  __(__  ) 
/_____/\__,_/_/ /_/\__, /\__,_/\__,_/\__, /\___/____/  
                  /____/            /____/             
--------------------------------------------------------------------------]]--

--[[--------------------------------------------------------------------------
-- LEDGER --------------------------------------------------------------------
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

--[[--------------------------------------------------------------------------
-- MARKDOWN ------------------------------------------------------------------
--------------------------------------------------------------------------]]--

-- Enable programming language syntax highlighting in markdown files
vim.g.markdown_syntax_conceal = 0
vim.g.markdown_minlines = 100
vim.g.markdown_fenced_languages = {
    'html',
    'yaml',
    'python',
    'bash=sh',
    'rust'
}
-- Let's also theme the markdown syntax
-- Headers
color_priority = {
    'darkred',
    'darkblue',
    'darkgreen',
    'darkcyan',
    'darkmagenta',
    'darkyellow'
}
for i = 1, 6 do
    vim.cmd("highlight markdownH"..i.." ctermfg="..color_priority[i])
    vim.cmd("highlight markdownH"..i.."Delimiter ctermfg="..color_priority[i])
end
vim.cmd("highlight markdownLinkText ctermfg=darkblue")
vim.cmd("highlight markdownUrl ctermfg=darkblue")
vim.cmd("highlight markdownCodeDelimiter ctermfg=darkred")

--[[--------------------------------------------------------------------------
-- RUST ----------------------------------------------------------------------
--------------------------------------------------------------------------]]--

vim.g.rustfmt_autosave = 1
vim.g.rustfmt_emit_files = 1
vim.g.rustfmt_fail_silently = 0

--[[--------------------------------------------------------------------------
-- LAZY SETUP ----------------------------------------------------------------
--------------------------------------------------------------------------]]--

return {
    {'nathangrigg/vim-beancount', ft = 'beancount'},
    {'ledger/vim-ledger', ft = 'ledger'},
    {'tpope/vim-markdown', ft = 'markdown'},
    {'rust-lang/rust.vim', ft = 'rust'},
}
