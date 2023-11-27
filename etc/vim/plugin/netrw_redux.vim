" Configuration based on blog post:
" - https://vonheikemen.github.io/devlog/tools/using-netrw-vim-builtin-file-explorer/

let g:netrw_keepdir = 0
let g:netrw_liststyle = 0
let g:netrw_banner = 0
let g:netrw_list_hide = '\(^\|\s\s\)\zs\.\S\+'
let g:netrw_localcopydircmd = 'cp -r'
" FIXME: Update highlight color here
hi! link netrwMarkFile Search

" Will open netrw in the directory of the current file
nnoremap <leader>dd :Explore %:p:h<CR>
" Will open netrw in the current working directory
nnoremap <Leader>da :Explore<CR>


" Unfortunately, we can't directly assign a keymap to netrw.
" Instead, since netrw defines it's own filetype, we can create an autocommand
" that is run everytime vim runs netrw. We place our keymaps into the
" function.

" Keymaps:
" ========

" Config:
" -------
" s - change sort algorithm

" Control:
" --------
" h - Go up a directory
" l - Open a directory/file
" H - Go back in history
" . - Toggle dotfiles
" L - Open a file and close netrw
" <leader>-d - Close netrw
" p - Show preview window
" P - Close the preview window 

" Marks:
" ------
" Tab - Toggles mark
" S-Tab - Unmark all the files in current buffer
" <leader>-Tab - Remove all marks on all files

" File Management:
" ----------------
" ff - Create a file this time (similar to %, but touches an empty file)
" fr - Rename a file
" fc - Copy marked files
" fC - Copy marked files to directory under cursor
" fm - Move marked files
" fM - Move marked files to directory under cursor
" fx - Run external command on marked files
" fl - List marked files
" fq - Show target directory
" ft - Mark target directory (and show it)

" Bookmarks:
" ----------
" bb - Create a bookmark
" bd - Remove the most recent bookmark
" bl - Jump to the most recent bookmark

function! NetrwMapping()
    " general
    nmap <buffer> H u
    nmap <buffer> h -^
    nmap <buffer> l <CR>
    nmap <buffer> . gh
    nmap <buffer> P <C-w>z
    " FIXME: does this work with regular explroe
    nmap <buffer> L <CR>:Lexplore<CR>
    nmap <buffer> <Leader>d :Lexplore<CR>
    " marks
    nmap <buffer> <TAB> mf
    nmap <buffer> <S-TAB> mF
    nmap <buffer> <Leader><TAB> mu
    " file management
    nmap <buffer> ff %:w<CR>:buffer #<CR>
    nmap <buffer> fr R
    nmap <buffer> fc mc
    nmap <buffer> fC mtmc
    nmap <buffer> fm mm
    nmap <buffer> fM mtmm
    nmap <buffer> fx mx
    nmap <buffer> fl :echo join(netrw#Expose("netrwmarkfilelist"), "\n")<CR>
    nmap <buffer> fq :echo 'Target:' . netrw#Expose("netrwmftgt")<CR>
    nmap <buffer> ft mtfq
    " bookmarks
    nmap <buffer> bb mb
    nmap <buffer> bd mB
    nmap <buffer> bl gb
endfunction

augroup netrw_mapping
  autocmd!
  autocmd filetype netrw call NetrwMapping()
augroup END
