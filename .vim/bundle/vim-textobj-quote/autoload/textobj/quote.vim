" ============================================================================
" File:        quote.vim
" Description: autoload functions for vim-textobj-quote plugin
" Maintainer:  Reed Esau <github.com/reedes>
" Created:     February 6, 2013
" License:     The MIT License (MIT)
" ============================================================================

scriptencoding utf-8

if &cp || (  exists('g:autoloaded_textobj_quote') &&
          \ !exists('g:force_reload_textobj_quote'))
  finish
endif

function! s:unicode_enabled()
  return &encoding == 'utf-8'
endfunction

" generate a regex for select, allowing for contractions
function! s:get_select_re(l, r, inner)
  return
    \ '\v' .
    \ a:l .
    \ (a:inner ? '\zs' : '') .
    \ '\_.{-}' .
    \ (a:inner ? '\ze' : '') .
    \ a:r .
    \ (a:r == '’' ? '(\w)@!' : '')
endfunction

function! s:select(pattern)
  call search(a:pattern, 'bc')
  let l:start = getpos('.')
  call search(a:pattern, 'ce')
  let l:end = getpos('.')
  return ['v', l:start, l:end]
endfunction

function! textobj#quote#select_d_a()
  if !exists('b:textobj_quote_re_d_a')
    call textobj#quote#init()
  endif
  return s:select(b:textobj_quote_re_d_a)
endfunction
function! textobj#quote#select_d_i()
  if !exists('b:textobj_quote_re_d_i')
    call textobj#quote#init()
  endif
  return s:select(b:textobj_quote_re_d_i)
endfunction

function! textobj#quote#select_s_a()
  if !exists('b:textobj_quote_re_s_a')
    call textobj#quote#init()
  endif
  return s:select(b:textobj_quote_re_s_a)
endfunction
function! textobj#quote#select_s_i()
  if !exists('b:textobj_quote_re_s_i')
    call textobj#quote#init()
  endif
  return s:select(b:textobj_quote_re_s_i)
endfunction

function! textobj#quote#getPrevCharRE(mode)
  " regex to match previous character
  " mode=1 is double; mode=0 is single
  return '\v(^|[[({& ' .
        \ (a:mode
        \   ? (b:textobj_quote_sl . '''])')
        \   : (b:textobj_quote_dl . '"])'))
endfunction

" set up mappings for current buffer only
" initialize buffer-scoped variables
" args: { 'double':'“”', 'single':'‘’',}
function! textobj#quote#init(...)
  if !s:unicode_enabled() | return | endif

  let l:args = a:0 ? a:1 : {}
  let l:double_pair = get(l:args, 'double', g:textobj#quote#doubleDefault)
  let l:single_pair = get(l:args, 'single', g:textobj#quote#singleDefault)

  " obtain the individual quote characters
  let l:d_arg = split(l:double_pair, '\zs')
  let l:s_arg = split(l:single_pair, '\zs')
  let b:textobj_quote_dl = l:d_arg[0]
  let b:textobj_quote_dr = l:d_arg[1]
  let b:textobj_quote_sl = l:s_arg[0]
  let b:textobj_quote_sr = l:s_arg[1]

  " the 'inner' and 'outer' patterns used in select functions
  let b:textobj_quote_re_d_i = s:get_select_re(b:textobj_quote_dl, b:textobj_quote_dr, 1)
  let b:textobj_quote_re_d_a = s:get_select_re(b:textobj_quote_dl, b:textobj_quote_dr, 0)
  let b:textobj_quote_re_s_i = s:get_select_re(b:textobj_quote_sl, b:textobj_quote_sr, 1)
  let b:textobj_quote_re_s_a = s:get_select_re(b:textobj_quote_sl, b:textobj_quote_sr, 0)

  call textobj#user#plugin('quote', {
  \      'select-d': {
  \         'select-a': 'a' . g:textobj#quote#doubleMotion,
  \         'select-i': 'i' . g:textobj#quote#doubleMotion,
  \         '*select-a-function*': 'textobj#quote#select_d_a',
  \         '*select-i-function*': 'textobj#quote#select_d_i',
  \      },
  \      'select-s': {
  \         'select-a': 'a' . g:textobj#quote#singleMotion,
  \         'select-i': 'i' . g:textobj#quote#singleMotion,
  \         '*select-a-function*': 'textobj#quote#select_s_a',
  \         '*select-i-function*': 'textobj#quote#select_s_i',
  \      },
  \})

  " initialize extensions

  if get(l:args, 'matchit', g:textobj#quote#matchit) &&
   \ exists("b:match_words")
    " support '%' navigation of textobj_quote pairs
    if b:textobj_quote_dl != b:textobj_quote_dr
      " specialized closing pattern to ignore use of quote in contractions
      let b:match_words .= ',' . b:textobj_quote_dl .
                          \':' . b:textobj_quote_dr .
                          \      (b:textobj_quote_dr == '’'
                          \       ? '\(\W\|$\)'
                          \       : '')
    endif
    if b:textobj_quote_sl != b:textobj_quote_sr
      " specialized closing pattern to ignore use of quote in contractions
      let b:match_words .= ',' . b:textobj_quote_sl .
                          \':' . b:textobj_quote_sr .
                          \      (b:textobj_quote_sr == '’'
                          \       ? '\(\W\|$\)'
                          \       : '')
    endif
  endif

  if get(l:args, 'educate', g:textobj#quote#educate)
    call textobj#quote#educate#mapKeys(1)
  endif

  " q/Q support for tpope/vim-surround
  let l:char = g:textobj#quote#doubleMotion
  let l:nr = char2nr(l:char)
  exe 'let b:surround_' . l:nr . ' = b:textobj_quote_dl . "\r" . b:textobj_quote_dr'
  let l:char = g:textobj#quote#singleMotion
  let l:nr = char2nr(l:char)
  exe 'let b:surround_' . l:nr . ' = b:textobj_quote_sl . "\r" . b:textobj_quote_sr'

endfunction

let g:autoloaded_textobj_quote = 1
