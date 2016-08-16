" ============================================================================
" File:        educate.vim
" Description: autoload functions for educate feature
" Maintainer:  Reed Esau <github.com/reedes>
" Created:     February 6, 2013
" License:     The MIT License (MIT)
" ============================================================================
"
if exists('g:autoloaded_textobj_quote_educate') &&
 \ g:autoloaded_textobj_quote_educate
  fini
en

function! s:unicode_enabled()
  return &encoding == 'utf-8'
endfunction

function! s:educateQuotes(mode)
  " intelligently insert curly quotes
  " mode=1 is double; mode=0 is single
  return search(
       \    textobj#quote#getPrevCharRE(a:mode) . '%#',
       \    'n')
       \ ? (a:mode ? b:textobj_quote_dl : b:textobj_quote_sl)
       \ : (a:mode ? b:textobj_quote_dr : b:textobj_quote_sr)
endfunction

function! textobj#quote#educate#mapKeys(...)
  " Un/Map keys to un/educate quotes for current buffer
  " 1=map, 0=unmap
  let b:textobj_quote_educate_mapped = a:0 ? !!a:1 : 1
  if !exists('b:textobj_quote_dl')
    call textobj#quote#init()
    return
  endif
  if b:textobj_quote_educate_mapped
    " For details on the leading <C-R>, see :help ins-special-special
    " TODO use <expr> instead?
    inoremap <silent> <buffer> " <C-R>=<SID>educateQuotes(1)<CR>
    inoremap <silent> <buffer> ' <C-R>=<SID>educateQuotes(0)<CR>
  else
    silent! iunmap <buffer> "
    silent! iunmap <buffer> '
  endif
endfunction

function! textobj#quote#educate#toggleMappings()
  " Toggle mapped keys for current buffer
  let l:educate =
    \ !exists('b:textobj_quote_educate_mapped')
    \ ? 1
    \ : !b:textobj_quote_educate_mapped
  call textobj#quote#educate#mapKeys(l:educate)
endfunction

let g:autoloaded_textobj_quote_educate = 1
