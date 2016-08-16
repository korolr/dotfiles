" ctags.vim: Display function name in the title bar.
" Author: Alexey Marinichev <lyosha@lyosha.2y.net>
" Version: 1.0
"
" DETAILED DESCRIPTION:
" This script uses exuberant ctags to build the list of tags for the current
" file.  CursorHold event is then used to update titlestring.
" 
" Upon sourcing an autocommand is created with event type CursorHold.  It
" updates the title string using the function GetTagName.  Another autocommand
" of type BufEnter is created to generate tags for *.c, *.cpp and *.h files.
" 
" Function GenerateTags builds an array of tag names.
" 
" Function GetTagName takes line number argument and returns the tag name.
"
" INSTALL DETAILS:
" Before sourcing the script do:
"    let g:ctags_path='/path/to/ctags'
"    let g:ctags_args='-I __declspec+'
"        (or whatever other additional arguments you want to pass to ctags)
" :CTAGS command starts the script.



if !exists("ctags_path")
    "let g:ctags_path='ctags'
    "let g:ctags_args=''
    let g:ctags_path=$VIM.'/ctags/ctags'
    let g:ctags_args='-I __declspec+'
endif

command! CTAGS let generate_tags=1|call GenerateTags()

autocmd BufEnter *.c,*.cpp,*.h if exists('generate_tags') && !exists('b:lines') | call GenerateTags() | endif

set updatetime=500
autocmd CursorHold * if exists('generate_tags') | let &titlestring='%t%( %M%)%( (%{expand("%:~:.:h")})%)%( %a%)%='.GetTagName(line(".")) | endif

"set titlestring=%t%(\ %M%)%(\ (%{expand(\"%:~:.:h\")})%)%(\ %a%)%=%(tag:\ %-{GetTagName(line("."))}%)



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" No changes should be reqired below (unless there are bugs).
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

if version < 600
    function! Stridx(haysack, needle)
	return match(a:haysack, a:needle)
    endfunction
else
    function! Stridx(haysack, needle)
	return stridx(a:haysack, a:needle)
    endfunction
endif

let g:ctags_obligatory_args = '-n --sort=no -o -'
let g:ctags_pattern="^\\(.\\{-}\\)\t.\\{-}\t\\(\\d*\\).*"

" This function builds an array of tag names.  b:lines contains line numbers;
" b:l<number> is the tag value for the line <number>.
function! GenerateTags()
    let ctags = system(g:ctags_path.' '.g:ctags_args.' '.g:ctags_obligatory_args.' "'.expand('%').'"')

    " b:length is one greater than the length of maximum line number.
    let b:length = 8
    let b:lines = ''

    " strlen(spaces) must be at least b:length.
    let spaces = '               '
    let i = 1
    let len = strlen(ctags)

    while strlen(ctags) > 0
	let one_tag = strpart(ctags, 0, Stridx(ctags, "\n"))
	let tag_name = substitute(one_tag, g:ctags_pattern, '\1', '')
	let tag_line_number = substitute(one_tag, g:ctags_pattern, '\2', '')
	execute "let b:l".tag_line_number . " = '".tag_name."'"
	let b:lines = strpart(b:lines.tag_line_number.spaces, 0, b:length*i)
	let i = i+1

	" vim 5.x insists that strpart takes 3 arguments.
	let ctags = strpart(ctags, Stridx(ctags, "\n")+1, len)
    endwhile

    let b:lines = b:lines."9999999"
endfunction

" This function returns the tag name for given index.
function! GetLine(i)
    return strpart(b:lines, a:i*b:length, b:length)+0
endfunction

" This function does binary search in the array of tag names and returns
" corresponding tag.
function! GetTagName(curline)
    if !exists("b:lines")
	return ""
    endif

    let left = 0
    let right = strlen(b:lines)/b:length

    if a:curline < GetLine(left)
	return ""
    endif

    while left<right
	let middle = (right+left+1)/2
	let middleline = GetLine(middle)

	if middleline == a:curline
	    let left = middle
	    break
	endif

	if middleline > a:curline
	    let right = middle-1
	else
	    let left = middle
	endif
    endwhile

    exe "let ret=b:l".GetLine(left)
    return ret
endfunction

" vim:set ts=8 sts=4 sw=4:
