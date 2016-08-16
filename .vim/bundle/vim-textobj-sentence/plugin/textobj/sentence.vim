" ============================================================================
" File:        textobj_sentence.vim
" Description: load functions for vim-textobj_sentence plugin
" Maintainer:  Reed Esau <github.com/reedes>
" Created:     January 27, 2013
" License:     The MIT License (MIT)
" ============================================================================

scriptencoding utf-8

if exists('g:loaded_textobj_sentence') | finish | endif

if !exists('g:textobj#sentence#select')
  let g:textobj#sentence#select = 's'
endif

if !exists('g:textobj#sentence#move_p')
  let g:textobj#sentence#move_p = '('
endif

if !exists('g:textobj#sentence#move_n')
  let g:textobj#sentence#move_n = ')'
endif

let g:textobj#sentence#doubleStandard = '“”'
let g:textobj#sentence#singleStandard = '‘’'

if !exists('g:textobj#sentence#doubleDefault')
  "  “double”
  let g:textobj#sentence#doubleDefault = g:textobj#sentence#doubleStandard
endif
if !exists('g:textobj#sentence#singleDefault')
  "  ‘single’
  let g:textobj#sentence#singleDefault = g:textobj#sentence#singleStandard
endif

if !exists('g:textobj#sentence#abbreviations')
  let g:textobj#sentence#abbreviations = [
    \ '[ABCDIMPSUabcdegimpsv]',
    \ 'l[ab]', '[eRr]d', 'Ph', '[Ccp]l', '[Lli]n', '[cn]o',
    \ '[Oe]p', '[DJMSh]r', '[MVv]s', '[CFMPScfpw]t',
    \ 'alt', '[Ee]tc', 'div', 'es[pt]', '[Ll]td', 'min',
    \ '[MD]rs', '[Aa]pt', '[Aa]ve?', '[Ss]tr?',
    \ '[Aa]ssn', '[Bb]lvd', '[Dd]ept', 'incl', 'Inst', 'Prof', 'Univ',
    \ ]
endif

let g:loaded_textobj_sentence = 1
