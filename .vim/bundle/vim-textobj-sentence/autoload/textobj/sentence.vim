" ============================================================================
" File:        textobj_sentence.vim
" Description: load functions for vim-textobj_sentence plugin
" Maintainer:  Reed Esau <github.com/reedes>
" Created:     January 25, 2013
" License:     The MIT License (MIT)
" ============================================================================

scriptencoding utf-8

if &cp || (exists('g:autoloaded_textobj_sentence')
      \ && !exists('g:force_reload_textobj_sentence'))
  finish
endif

function! s:select(pattern)
  call search(a:pattern, 'bc')
  let l:start = getpos('.')
  call search(a:pattern, 'ce')
  let l:end = getpos('.')
  return ['v', l:start, l:end]
endfunction

function! textobj#sentence#select_a()
  if !exists('b:textobj_sentence_re_a')
    call textobj#sentence#init()
  endif
  return s:select(b:textobj_sentence_re_a)
endfunction

function! textobj#sentence#select_i()
  if !exists('b:textobj_sentence_re_i')
    call textobj#sentence#init()
  endif
  return s:select(b:textobj_sentence_re_i)
endfunction

function! textobj#sentence#init(...)
  let l:args = a:0 ? a:1 : {}

  let l:double_pair = get(l:args, 'double', g:textobj#sentence#doubleDefault)
  let l:single_pair = get(l:args, 'single', g:textobj#sentence#singleDefault)

  " obtain the individual quote characters
  let l:d_arg = split(l:double_pair, '\zs')
  let l:s_arg = split(l:single_pair, '\zs')
  let b:textobj_sentence_quote_dl = l:d_arg[0]
  let b:textobj_sentence_quote_dr = l:d_arg[1]
  let b:textobj_sentence_quote_sl = l:s_arg[0]
  let b:textobj_sentence_quote_sr = l:s_arg[1]

  let l:quotes_std = '"''*_'
  let l:leading  =
      \ '(\[' .
      \ l:quotes_std .
      \ b:textobj_sentence_quote_sl .
      \ b:textobj_sentence_quote_dl
  let l:trailing =
      \ ')\]' .
      \ l:quotes_std .
      \ b:textobj_sentence_quote_sr .
      \ b:textobj_sentence_quote_dr

  " Avoid matching where more of the sentence can be found on preceding line(s)
  let l:re_negative_lookback =
      \ '([' .
      \ l:leading .
      \ '[:alnum:]–—()\[\]_*,;:-]\_s*)@2000<!'

  " body (sans terminator) starts with start-of-file, or
  " an uppercase character
  let l:re_sentence_body =
      \ '(%^\zs|\n\s*\n\s*\zs|[' . l:leading . ']*[[:upper:]][^.])\_.{-}'

  let l:abbreviations =
      \ get(l:args, 'abbreviations', g:textobj#sentence#abbreviations)
  let l:bounded_abbrs =
      \ len(l:abbreviations)
      \ ? '|<(' . join(l:abbreviations, '|') . ')>'
      \ : ''

  let l:max_abbrev_len = 10   " allow for lookback on -1.2345678
  let l:re_abbrev_neg_lookback =
      \ '([-0-9]+' .
      \ l:bounded_abbrs .
      \ ')@' . l:max_abbrev_len . '<!'

  " matching against end of sentence, '!', '?', and non-abbrev '.'
  let l:re_term =
      \ '([!?]|(' .
      \ l:re_abbrev_neg_lookback .
      \ '\.))+[' .
      \ l:trailing .
      \ ']*'

  " sentence can also end when followed by at least two line feeds
  let l:re_sentence_term = '(' . l:re_term . '|\ze(\s*\n\s*\n\s*|\_s*%$))'

  " the 'inner' pattern
  let b:textobj_sentence_re_i =
      \ '\v' .
      \ l:re_negative_lookback .
      \ l:re_sentence_body .
      \ l:re_sentence_term

  " include all whitespace to end of line
  let b:textobj_sentence_re_a =
      \ b:textobj_sentence_re_i .
      \ '\s*'

  call textobj#user#plugin('sentence', {
  \      'select': {
  \         'select-a': 'a' . g:textobj#sentence#select,
  \         'select-i': 'i' . g:textobj#sentence#select,
  \         '*select-a-function*': 'textobj#sentence#select_a',
  \         '*select-i-function*': 'textobj#sentence#select_i',
  \      },
  \      'move': {
  \         'pattern': b:textobj_sentence_re_i,
  \         'move-p': g:textobj#sentence#move_p,
  \         'move-n': g:textobj#sentence#move_n,
  \         'move-P': 'g' . g:textobj#sentence#move_p,
  \         'move-N': 'g' . g:textobj#sentence#move_n,
  \      },
  \})
endfunction

let g:autoloaded_textobj_sentence = 1
