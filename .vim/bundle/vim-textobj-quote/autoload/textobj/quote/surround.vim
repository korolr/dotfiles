" ============================================================================
" File:        surround.vim
" Description: autoload functions for surround feature
" Maintainer:  Reed Esau <github.com/reedes>
" Created:     February 6, 2013
" License:     The MIT License (MIT)
" ============================================================================
"
if exists('g:autoloaded_textobj_quote_surround') &&
 \ g:autoloaded_textobj_quote_surround
  fini
en

function! textobj#quote#surround#surround(mode, visual)
  " A simple alternative to Tim Pope's vim-surround
  " wrap word/selection in curly quotes
  " mode=1 is double; mode=0 is single
  if !exists('b:textobj_quote_dl') | return | endif
  if a:mode
    let l:l = b:textobj_quote_dl
    let l:r = b:textobj_quote_dr
  else
    let l:l = b:textobj_quote_sl
    let l:r = b:textobj_quote_sr
  endif
  if a:visual ==# 'v'
    " note: the gv re-establishes the visual selection that <C-u> removed
    execute "normal! gvc" . l:l . "\<C-r>\"" . l:r ."\<Esc>"
  elseif a:visual ==# ''
    execute "normal! ciw" . l:l . "\<C-r>\"" . l:r . "\<Esc>"
  endif
endfunction

let g:autoloaded_textobj_quote_surround = 1
