" ============================================================================
" File:        textobj_quote.vim
" Description: load functions for vim-textobj-quote plugin
" Maintainer:  Reed Esau <github.com/reedes>
" Created:     February 6, 2013
" License:     The MIT License (MIT)
" ============================================================================

scriptencoding utf-8

if exists('g:loaded_textobj_quote') | finish | endif

if !exists('g:textobj#quote#doubleMotion')
  let g:textobj#quote#doubleMotion = 'q'
endif
if !exists('g:textobj#quote#singleMotion')
  let g:textobj#quote#singleMotion = 'Q'
endif

if !exists('g:textobj#quote#move_p')
  let g:textobj#quote#move_p = ''
endif

if !exists('g:textobj#quote#move_n')
  let g:textobj#quote#move_n = ''
endif

if !exists('g:textobj#quote#move_P')
  let g:textobj#quote#move_P = ''
endif

if !exists('g:textobj#quote#move_N')
  let g:textobj#quote#move_N = ''
endif

let g:textobj#quote#doubleStandard = '“”'
let g:textobj#quote#singleStandard = '‘’'

if !exists('g:textobj#quote#doubleDefault')
  "  “double”
  let g:textobj#quote#doubleDefault = g:textobj#quote#doubleStandard
endif
if !exists('g:textobj#quote#singleDefault')
  "  ‘single’
  let g:textobj#quote#singleDefault = g:textobj#quote#singleStandard
endif

" enable/disable features
if !exists('g:textobj#quote#matchit')
  let g:textobj#quote#matchit = 1
endif
if !exists('g:textobj#quote#educate')
  let g:textobj#quote#educate = 1
endif

" needed to match pairs of quotes (via tpope/vim-sensible)
if g:textobj#quote#matchit &&
      \ !exists('g:loaded_matchit') &&
      \ findfile('plugin/matchit.vim', &rtp) ==# ''
  runtime! macros/matchit.vim
endif

" commands to toggle key mappings
command! -nargs=0 Educate call textobj#quote#educate#mapKeys(1)
command! -nargs=0 NoEducate call textobj#quote#educate#mapKeys(0)
command! -nargs=0 ToggleEducate call textobj#quote#educate#toggleMappings()

" replace quotes in bulk
nnoremap <Plug>ReplaceWithCurly    :call textobj#quote#replace#replace(1, '')<cr>
vnoremap <Plug>ReplaceWithCurly    :<C-u>call textobj#quote#replace#replace(1, visualmode())<cr>
nnoremap <Plug>ReplaceWithStraight :call textobj#quote#replace#replace(0, '')<cr>
vnoremap <Plug>ReplaceWithStraight :<C-u>call textobj#quote#replace#replace(0, visualmode())<cr>

" a simple alterative to tpope/vim-surround
nnoremap <Plug>SurroundWithDouble :call textobj#quote#surround#surround(1, '')<cr>
vnoremap <Plug>SurroundWithDouble :<C-u>call textobj#quote#surround#surround(1, visualmode())<cr>
nnoremap <Plug>SurroundWithSingle :call textobj#quote#surround#surround(0, '')<cr>
vnoremap <Plug>SurroundWithSingle :<C-u>call textobj#quote#surround#surround(0, visualmode())<cr>

let g:loaded_textobj_quote = 1
