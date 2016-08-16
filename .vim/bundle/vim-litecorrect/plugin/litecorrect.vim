" =============================================================================
" File:        plugin/litecorrect.vim
" Description: lightweight autocorrection for Vim
" Maintainer:  Reed Esau <github.com/reedes>
" Created:     January 20, 2014
" License:     The MIT License (MIT)
" =============================================================================

if exists('g:loaded_litecorrect') || &cp | finish | endif
let g:loaded_litecorrect = 1

" Save 'cpoptions' and set Vim default to enable line continuations.
let s:save_cpo = &cpo
set cpo&vim

let &cpo = s:save_cpo
unlet s:save_cpo
" vim:ts=2:sw=2:sts=2

