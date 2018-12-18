let g:python3_host_prog = '/usr/bin/python3'  " Python 3


call plug#begin('~/.vim/plugged')


Plug 'prettier/vim-prettier'
Plug 'skywind3000/asyncrun.vim'



Plug 'flowtype/vim-flow', {
            \ 'autoload': {
            \     'filetypes': 'javascript'
            \ },
            \ 'build': {
            \     'mac': 'npm install -g flow-bin',
            \     'unix': 'npm install -g flow-bin'
            \ }}


Plug 'vim-scripts/tComment'
Plug 'kopischke/vim-stay'
Plug 'mhinz/vim-startify'
Plug 'myshov/xkbswitch-macosx'
Plug 'vim-scripts/ctags.vim'
Plug 'powerman/vim-plugin-ruscmd'
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'posva/vim-vue'
Plug 'fatih/vim-go'
Plug 'w0rp/ale'
Plug 'jbgutierrez/vim-babel'
Plug 'othree/yajs.vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'arcticicestudio/nord-vim'
Plug 'ayu-theme/ayu-vim'
Plug 'dart-lang/dart-vim-plugin'



Plug 'arcticicestudio/nord-vim'
" Place deoplete first, then autocomplete-flow
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'wokalski/autocomplete-flow'
" You will also nee-ternjs'
Plug 'scrooloose/nerdtree'
Plug 'mattn/emmet-vim'
Plug 'bling/vim-airline'
Plug 'tpope/vim-fugitive'
Plug 'morhetz/gruvbox'
Plug 'othree/yajs.vim', { 'for': 'javascript' }
Plug 'moll/vim-node'
Plug 'lepture/vim-jinja'

" Neoformat for stuff like Prettier
Plug 'sbdchd/neoformat'

" for neovim
if has('nvim')
  " Dark-powered Async completion (needs python3)
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" for vim 8 with python
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'

" Add flow to deoplete
  Plug 'wokalski/autocomplete-flow'

  " Function argument completion for Deoplete
  Plug 'Shougo/neosnippet'
  Plug 'Shougo/neosnippet-snippets'

  " the path to python3 is obtained through executing `:echo exepath('python3')` in vim
  let g:python3_host_prog = "/usr/local/bin/python3"
endif

Plug 'Yggdroot/indentLine'

" Wakatime

" Bottom bar with all settings
Plug 'itchyny/lightline.vim'

" Alignment
Plug 'austintaylor/vim-indentobject'

" Align anything using `ga` command
Plug 'junegunn/vim-easy-align'

" Toggles a sidebar!
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

" Fancy start screen. Lets you open empty buffers, multiple files, etc
Plug 'mhinz/vim-startify'

" Snap windows without ruining your layout using ,ww
Plug 'https://github.com/wesQ3/vim-windowswap'

" Remaps . in a way that plugins can use it too!
Plug 'tpope/vim-repeat'

" Easily delete, change and add surroundings in pairs
Plug 'tpope/vim-surround'

"Bracket maps
Plug 'tpope/vim-unimpaired'

" Indent Guides
Plug 'nathanaelkane/vim-indent-guides', {'on': ['IndentGuidesToggle', 'IndentGuidesEnable']}

" Automatic closing of quotes, parenthesis, brackets, etc
Plug 'Raimondi/delimitMate'

" Change inside surroundings
Plug 'briandoll/change-inside-surroundings.vim'

" focus region, NR will open selected part in new split window
Plug 'chrisbra/NrrwRgn'

" Comments using gcc / gcgc
Plug 'git://github.com/tpope/vim-commentary.git'

" Async linting
Plug 'w0rp/ale'

" Git
" Plug 'junegunn/vim-github-dashboard', { 'on': ['GHDashboard', 'GHActivity'] }
" https://github.com/junegunn/vim-github-dashboard
Plug 'https://github.com/rhysd/committia.vim', { 'for': ['gitcommit']}
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-git'
Plug 'airblade/vim-gitgutter'


" *************************
" Themes
" *************************
Plug 'ayu-theme/ayu-vim'
Plug 'drewtempelmeyer/palenight.vim'
" Plug 'morhetz/gruvbox'
" Plug 'mhartington/oceanic-next'

" *************************
" Text-Related
" *************************

" Expands on commands like 'delete inside' by adding more targets
Plug 'https://github.com/wellle/targets.vim'

" Better job of detecting sentences
Plug 'https://github.com/reedes/vim-textobj-sentence'

" Makes operating on columns super easy
Plug 'coderifous/textobj-word-column.vim'
Plug 'kana/vim-textobj-datetime'
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-function'
Plug 'kana/vim-textobj-user'
Plug 'lucapette/vim-textobj-underscore'
Plug 'vim-scripts/argtextobj.vim'

" Find & replace, etc
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'rking/ag.vim'
Plug 'junegunn/vim-fnr' | Plug 'junegunn/vim-pseudocl' " find & replace
Plug 'vim-scripts/greplace.vim'
Plug 'https://github.com/simnalamburt/vim-mundo'
Plug 'https://github.com/tpope/vim-abolish' " AWESOME case-sensitive replace
Plug 'andymass/vim-matchup'

" *************************
" Language-Related
" *************************

" Handlebars / Mustache
Plug 'juvenn/mustache.vim'
Plug 'nono/vim-handlebars'

" Javascript
Plug 'https://github.com/othree/javascript-libraries-syntax.vim', { 'for': [ 'javascript', 'js', 'jsx' ]}
Plug 'thinca/vim-textobj-function-javascript',    { 'for': [ 'javascript', 'js', 'jsx' ]}
Plug '1995eaton/vim-better-javascript-completion', { 'for': [ 'javascript', 'js', 'jsx' ]}
Plug 'chemzqm/vim-jsx-improve', { 'for': [ 'javascript', 'js', 'jsx' ]}
Plug 'gavocanov/vim-js-indent', { 'for': [ 'javascript', 'js', 'jsx' ]}
Plug 'leafgarland/typescript-vim'

" Graphql
Plug 'jparise/vim-graphql'

" Toolkit - no syntax highlighting https://github.com/moll/vim-node
Plug 'moll/vim-node', { 'for': [ 'javascript', 'js', 'jsx' ]}

" SCSS and CSS syntax highlighting
if v:version < 704
  Plug 'JulesWang/css.vim'
endif
Plug 'cakebaker/scss-syntax.vim'

" HTML
Plug 'tpope/vim-ragtag'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'skwp/vim-html-escape'

" Lua
Plug 'https://github.com/xolox/vim-lua-ftplugin.git', {'for': ['lua']}
Plug 'https://github.com/xolox/vim-misc.git', {'for': ['lua']}

" Perl
Plug 'https://github.com/c9s/perlomni.vim', {'for': ['pl', 'perl', 'p6', 'pm']}

" ReasonML https://github.com/reasonml-editor/vim-reason-plus
Plug 'reasonml-editor/vim-reason-plus'

"Language Client https://github.com/autozimu/LanguageClient-neovim#quick-start
Plug 'autozimu/LanguageClient-neovim', {
    \ 'javascript': ['/usr/bin/javascript-typescript-stdio'],
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }

call plug#end()


set nospell
set cursorline
set nocursorcolumn
filetype plugin on
:let w:persistent_cursorline = 0
set ignorecase
set omnifunc=syntaxcomplete#Complete
:let loaded_matchparen = 0
set gfn=Menlo:h15
map <F3> :NERDTree<CR>

" node
let g:prettier#autoformat = 1
let g:jsx_ext_required = 1

let g:user_emmet_settings = {
  \  'javascript.jsx' : {
    \      'extends' : 'jsx',
    \  },
  \}



autocmd FileType javascript set formatprg=prettier
autocmd BufWritePre *.js :normal gggqG

let g:deoplete#enable_at_startup = 1
let g:neosnippet#enable_completed_snippet = 1

let g:deoplete#enable_at_startup = 1
let g:neosnippet#enable_completed_snippet = 1

" for vim 8 / neovim 0.1.5
if (has("termguicolors"))
 set termguicolors
endif

syntax on
syntax enable
filetype plugin indent on

set termguicolors     " enable true colors support
set background=dark
colorscheme gruvbox

set autoindent
set autoread                                                 " reload files when changed on disk, i.e. via `git checkout`
set backspace=2                                              " Fix broken backspace in some setups
set backupcopy=yes                                           " see :help crontab
set clipboard=unnamed                                        " yank and paste with the system clipboard
set directory-=.                                             " don't store swapfiles in the current directory
set encoding=utf-8
set expandtab                                                " expand tabs to spaces
set ignorecase                                               " case-insensitive search
set incsearch                                                " search as you type
set laststatus=2                                             " always show statusline
set list                                                     " show trailing whitespace
set listchars=space:·,tab:▸\ ,trail:▫,extends:>,precedes:<,nbsp:+,eol:¬
set number                                                   " show line numbers
set ruler                                                    " show where you are
set scrolloff=3                                              " show context above/below cursorline
set shiftwidth=2                                             " normal mode indentation commands use 2 spaces
set showcmd
set smartcase                                                " case-sensitive search if any caps
set softtabstop=2                                            " insert mode tab and backspace use 2 spaces
set tabstop=4                                                " actual tabs occupy 8 characters
set lazyredraw
set synmaxcol=200
set updatetime=250
set nowb
set nobackup
set noswapfile

" highlight all tabs and trailing whitespace characters.
highlight ExtraWhitespace ctermbg=darkgreen guibg=darkgreen
match ExtraWhitespace /\s\+$\|\t/

" Enable persistent undo so that undo history persists across vim sessions
set undofile
set undodir=~/.vim/undo

set nowrap
set linebreak

" Use The Silver Searcher https://github.com/ggreer/the_silver_searcher
if executable('ag')
  " Use Ag over Grep
  set grepprg=ag\ --nogroup\ --nocolor
endif

" force javascript syntax
autocmd BufRead *.js set filetype=javascript
autocmd BufRead *.es6 set filetype=javascript
autocmd BufRead *.jsx set filetype=javascript

" vim-commentary, adjust commentstring to support other libs
autocmd FileType apache setlocal commentstring=#\ %s<Paste>

" fdoc is yaml
autocmd BufRead,BufNewFile *.fdoc set filetype=yaml
" md is markdown
autocmd BufRead,BufNewFile *.md set filetype=markdown
autocmd BufRead,BufNewFile *.md set spell
" automatically rebalance windows on vim resize
autocmd VimResized * :wincmd =

" Enable basic mouse behavior such as resizing buffers.
set mouse=a

" Fix Cursor in TMUX
if exists('$TMUX')
  let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
  let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
else
  let &t_SI = "\<Esc>]50;CursorShape=1\x7"
  let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif

" Don't copy the contents of an overwritten selection.
vnoremap p "_dP


" use homebrew python


" marks 80th column
if (exists('+colorcolumn'))
    set colorcolumn=120
    highlight ColorColumn ctermbg=9
endif
" }}}
"

"https://github.com/reasonml-editor/vim-reason-plus
let g:LanguageClient_serverCommands = {
    \ 'reason': ['~/reason-language-server/reason-language-server.exe'],
    \ 'javascript': ['typescript-language-server', '--stdio'],
    \ }

" Automatically start language servers.
let g:LanguageClient_autoStart = 1

"gd Show type info (and short doc) of identifier under cursor.
nnoremap <silent> gd :call LanguageClient_textDocument_definition()<cr>

"gf Formats code in normal mode
nnoremap <silent> gf :call LanguageClient_textDocument_formatting()<cr>

"Show type info (and short doc) of identifier under cursor.
nnoremap <silent> <cr> :call LanguageClient_textDocument_hover()<cr>

" nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
" nnoremap <silent> gd :call LanguageClient_textDocument_definition


autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS

augroup fmt
  autocmd!
  autocmd BufWritePre *.js undojoin | Neoformat
augroup END

" Use formatprg when available
let g:neoformat_try_formatprg = 1

" enable syntax highlighting for .js files too instead of just .jsx
let g:jsx_ext_required = 0

" flow syntax highlighting
let g:javascript_plugin_flow = 1

" JSDoc syntax highlighting
let g:javascript_plugin_jsdoc = 1

" https://github.com/othree/javascript-libraries-syntax.vim
let g:used_javascript_libs = 'underscore,react,flux,chai'

au BufNewFile,BufRead *.html,*.htm,*.shtml,*.stm set ft=jinja
