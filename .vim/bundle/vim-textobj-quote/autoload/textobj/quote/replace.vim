" ============================================================================
" File:        replace.vim
" Description: autoload functions for replace feature
" Maintainer:  Reed Esau <github.com/reedes>
" Created:     February 6, 2013
" License:     The MIT License (MIT)
" ============================================================================
"
if exists('g:autoloaded_textobj_quote_replace') &&
 \ g:autoloaded_textobj_quote_replace
  fini
en

function! textobj#quote#replace#replace(mode, visual)
  " 0=C->S  1=S->C
  if !exists('b:textobj_quote_dl') | return | endif
  " Extract the target text...
  if len(a:visual) > 0
      silent normal! gvy
  else
      silent normal! vipy
  endif
  let l:text = getreg(v:register)

  if a:mode ==# 0     " replace curly with straight
    let l:text = substitute(l:text, '[' . b:textobj_quote_sl . b:textobj_quote_sr . ']', "'", 'g')
    let l:text = substitute(l:text, '[' . b:textobj_quote_dl . b:textobj_quote_dr . ']', '"', 'g')
  else                " replace straight with curly
    let l:text = substitute(l:text, textobj#quote#getPrevCharRE(0) . '\zs''', b:textobj_quote_sl, 'g')
    let l:text = substitute(l:text, textobj#quote#getPrevCharRE(1) . '\zs"' , b:textobj_quote_dl, 'g')
    let l:text = substitute(l:text, "'", b:textobj_quote_sr, 'g')
    let l:text = substitute(l:text, '"', b:textobj_quote_dr, 'g')
  endif

  " Paste back into buffer in place of original...
  call setreg(v:register, l:text, mode())
  silent normal! gvp
endfunction

let g:autoloaded_textobj_quote_replace = 1
