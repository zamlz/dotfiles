" Workspace Management
" --------------------

function! WindowMoveAutoCreate(key)
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

nnoremap <silent> <C-h> :call WindowMoveAutoCreate('h')<CR>
nnoremap <silent> <C-j> :call WindowMoveAutoCreate('j')<CR>
nnoremap <silent> <C-k> :call WindowMoveAutoCreate('k')<CR>
nnoremap <silent> <C-l> :call WindowMoveAutoCreate('l')<CR>
