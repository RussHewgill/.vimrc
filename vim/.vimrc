"Vundle
"{{{
set nocompatible              " be iMproved, required
filetype off                  " required
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')
" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/syntastic'
Plugin 'tpope/vim-surround'
Plugin 'godlygeek/tabular'
Plugin 'eagletmt/neco-ghc'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'Valloric/YouCompleteMe'
"Plugin 'bruno-/vim-husk'
Plugin 'tpope/vim-rsi'
Plugin 'kana/vim-textobj-user'
Plugin 'nelstrom/vim-textobj-rubyblock'
Plugin 'fs111/pydoc.vim'
Plugin 'tpope/vim-repeat'
Plugin 'sjl/gundo.vim'
Plugin 'xolox/vim-notes'
Plugin 'xolox/vim-misc'
Plugin 'coot/CRDispatcher'
Plugin 'coot/cmdalias_vim'
Plugin 'rhysd/clever-f.vim'
Plugin 'taglist.vim'
Plugin 'wting/rust.vim'
Plugin 'kovisoft/slimv'
Plugin 'https://github.com/wincent/Command-T'
Plugin 'guns/vim-sexp'
Plugin 'jiangmiao/auto-pairs'
"Plugin 'https://github.com/vim-scripts/paredit.vim'
Plugin 'Shougo/vimproc.vim'
Plugin 'eagletmt/ghcmod-vim'
"Plugin 'raichoo/haskell-vim'

"Plugin 'Lokaltog/vim-easymotion'
"Plugin 'tpope/vim-fireplace'
"Plugin 'kien/ctrlp.vim'
"Plugin 'tmhedberg/SimpylFold'
"Plugin 'kchmck/vim-coffee-script'
"Plugin 'davidhalter/jedi-vim'
"Plugin 'Shougo/neocomplcache.vim'
"Plugin 'tpope/vim-commentary'
"Plugin 'ervandew/supertab'
"Plugin 'nathanaelkane/vim-indent-guides'

Plugin 'russhewgill/badwolf'
Plugin 'bling/vim-airline'
Plugin 'altercation/vim-colors-solarized'
Plugin 'tomasr/molokai'
Plugin 'tpope/vim-vividchalk'
Plugin 'nanotech/jellybeans.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList          - list configured plugins
" :PluginInstall(!)    - install (update) plugins
" :PluginSearch(!) foo - search (or refresh cache first) for foo
" :PluginClean(!)      - confirm (or auto-approve) removal of unused plugins
"
" see :h vundle for more details or wiki for FAQ
"}}}

"Settings
"{{{
"

set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set smarttab
set number
set showcmd
set backspace=indent,eol,start
set autoindent
set ruler
set mouse=a
set history=10000

filetype plugin on
filetype indent on

set undofile
set autoread

set wildmenu
set cmdheight=2
set scrolloff=3
set cursorline
set relativenumber
set laststatus=2

set lazyredraw

set magic

set showmatch
set mat=2

set noerrorbells
set novisualbell
set t_vb=
"set tm=500

set wrap
set textwidth=79
set formatoptions=qrn1
"set colorcolumn=83

set modeline

"set list
"set listchars=tab:▸\ ,eol:¬

set splitbelow
set splitright

"""""""""""""""""""""""""""""
"Search settings

nnoremap / /\v
vnoremap / /\v

set ignorecase
set smartcase
set fileignorecase
set gdefault
set incsearch
set showmatch
set hlsearch

nnoremap <silent> ,, :let @/=""<cr>:GhcModTypeClear<cr>
vnoremap <silent> ,, :let @/=""<cr>:GhcModTypeClear<cr>

nnoremap <tab> %
vnoremap <tab> %

""""""""""""""""""""""""""""""""""""""""""""""""""
"Colors and Fonts
""""""""""""""""""""""""""""""""""""""""""""""""""
"{{{
syntax on
let g:hs_highlight_types=1
let g:hs_highlight_boolean=1

"set background=dark
set background=light
"colorscheme jellybeans
"colorscheme base16-default

if has("gui_running")
    set t_Co=256
    let g:badwolf_darkgutter = 1
    colorscheme badwolf
    "let g:solarized_termcolors=256
    "colorscheme solarized

    set guicursor+=a:blinkwait0

    set guifont=Inconsolata-g\ Medium\ 12
    set guioptions-=T
    set guioptions-=e
    set guioptions+=c
    set guioptions-=m
    set guioptions-=r
    set guioptions-=l
    set guioptions-=L
    set guioptions-=r
    set guioptions-=R
    set guioptions-=b
    set guitablabel=%M\ %t
    set guiheadroom=0
else
    set t_Co=256
    let g:badwolf_darkgutter = 1
    "let g:solarized_termcolors=256
    "colorscheme solarized
    colorscheme badwolf
endif

set encoding=utf8
set ffs=unix,dos,mac
set foldmethod=marker
set foldlevelstart=10

"}}}
"}}}

" Key Mappings
"{{{

let mapleader = " "

map j gj
map k gk

map H ^
map L $

cnoremap <expr> <C-f> getcmdpos()>strlen(getcmdline())?&cedit:"\<Lt>Right>"
noremap <C-x><C-c> :qa<CR>

inoremap jk <Esc>
inoremap JK <Esc>
inoremap jj <Esc>i
inoremap jl <Esc>la

noremap Y y$

noremap <leader>tr :w<CR>:so %<cr>
noremap <leader>ti :w<CR>:so %<cr>:PluginInstall<cr>
noremap <leader>tu :w<CR>:so %<cr>:PluginClean<cr>

nnoremap <Leader>ta :tabe ~/.config/.aliases<CR>
nnoremap <Leader>tp :tabe ~/.pentadactylrc<CR>
nnoremap <Leader>tz :tabe ~/.zshrc<CR>
nnoremap <Leader>tx :tabe ~/.xmonad/xmonad.hs<CR>
nnoremap <Leader>tv :tabe ~/.vimrc<CR>
nnoremap <Leader>tm :tabmove
nnoremap <Leader>tq :tabclose<cr>
nnoremap <Leader>to :tabonly<cr>
nnoremap <Leader>tn :tabnew<cr>
noremap <leader>u :w<cr>:silent! execute ":!%:p"<cr><cr>

noremap <F4> :TlistToggle<cr>

noremap <leader>, :call StripWS()<cr>

function! StripWS()
    silent! execute ':%s/\s\+$'
    silent! execute "call histdel('/', -1)"
endfunction

noremap ,. @:
noremap <leader>; A;<esc>

cnoremap <C-p> <up>
cnoremap <C-n> <down>

noremap zk zt
noremap zj zb

nnoremap <Plug>Spacesurround :call Spacesurround()<cr>
nmap ,<space> <Plug>Spacesurround

function! Spacesurround()
    :execute "normal! i\<space>\<right>\<space>\<esc>"
    silent! call repeat#set("\<plug>Spacesurround", v:count)
endfunction

"Hardcore mode
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>


nnoremap <Leader>k :tabn<CR>
nnoremap <Leader>j :tabp<CR>


map <Leader>y "*y
map <Leader>d "*d
map <Leader>p "*p

map <Leader><Leader>y "+y
map <Leader><Leader>d "+d
map <Leader><Leader>p "+p

"Tabularize
noremap ,<Tab> :Tabularize /\s\zs=\s.*/<cr>
noremap <Leader><Leader><Tab> :Tabularize /
noremap <leader><tab> :Tabu<up><cr>

noremap ; :
noremap <C-f> ;
noremap <C-c> ,
noremap \ ;

"nnoremap o o<esc>
"nnoremap O O<esc>

noremap <enter> o<esc>
noremap <S-enter> O<esc>

"Split Commands

noremap <Leader>ww <C-w>v
noremap <Leader>we <C-w>s
noremap <Leader>wr <C-w>r
noremap <Leader>- 5<C-w>-
noremap <Leader>= <C-w>=
noremap <Leader>+ 5<C-w>+
noremap <leader>wt <C-w>T

nnoremap <C-h> <C-w>h
nnoremap <C-k> <C-w>k
nnoremap <C-j> <C-w>j
nnoremap <C-l> <C-w>l

noremap <C-y> 5<C-y>
noremap <C-e> 5<C-e>

inoremap <C-y> <C-o>5<C-y>
inoremap <C-e> <C-o>5<C-e>

noremap Q @@

" Folding



"}}}

"Plugin Settings
"{{{

" Syntastic
"{{{

"noremap <leader>e :call ToggleErrors()<cr>
noremap <leader>ll :call Togglehlint()<cr>
noremap <leader>le :call SyntaxToggleModeAL()<cr>
noremap <leader>lc :SyntasticReset<cr>
noremap <leader>lw :AirlineToggleWhitespace<cr>
noremap [e :lprev<cr>
noremap ]e :lnext<cr>

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_c_checkers               = [ 'gcc', 'clang_tidy' ]
let g:syntastic_python_checkers          = [ 'python' ]
let g:syntastic_javascript_checkers      = [ 'jshint' ]
let g:syntastic_coffee_checkers          = [ 'coffeelint' ]
let g:syntastic_haskell_checkers         = [ 'hdevtools', 'hlint' ]

let g:syntastic_coffee_coffeelint_args = "--csv --file /usr/lib/node_modules/coffeelint/coffeelint.json"
let g:syntastic_cppcheck_config_file   = "/usr/shar/cppcheck/cfg/std.cfg"
let g:syntastic_haskell_hlint_args     = " --hint=Custom"
let g:syntastic_haskell_hdevtools_args = '-g -Wall -g -fno-warn-unused-binds
    \ -g -fno-warn-unused-imports -g -fno-warn-missing-signatures
    \ -g -fno-warn-deprecations -g -fno-warn-unused-binds -g -fno-warn-unused-matches'
"let g:syntastic_asm_dialect = 'nasm'

let g:syntastic_python_pylint_rcfile = '/home/russ/.pylintrc'
let g:syntastic_python_python_exec   = '/usr/bin/python'

let g:syntastic_mode_map = { 'mode': 'active',
                        \ 'active_filetypes': [],
                        \ 'passive_filetypes': [] }

let g:syntastic_auto_loc_list = 0

let g:hask_warning_mode = '#'
let g:hask_error_mode   = '#'

function! SyntaxToggleModeAL()
    silent execute "SyntasticToggleMode"
    if g:syntastic_mode_map['mode'] == 'passive'
        silent execute "let g:hask_error_mode = 'O'"
    else
        silent execute "let g:hask_error_mode = '#'"
    endif
endfunction

function! ToggleErrors()
    let old_last_winnr = winnr('$')
    lclose
    if old_last_winnr == winnr('$')
        Errors
    endif
endfunction

function! Togglehlint()
    if g:syntastic_haskell_checkers == ['hdevtools', 'hlint']
        silent execute "let g:syntastic_haskell_checkers = ['hdevtools']"
        silent execute "let g:syntastic_haskell_hdevtools_args = '-g -w'"
        silent execute "let g:hask_warning_mode = 'O'"
    else
        silent execute "let g:syntastic_haskell_checkers = ['hdevtools', 'hlint']"
        silent execute "let g:syntastic_haskell_hdevtools_args = '-g -Wall -g -fno-warn-unused-binds -g -fno-warn-unused-imports -g -fno-warn-missing-signatures -g -fno-hs-main -g -fno-warn-deprecations'"
        silent execute "let g:hask_warning_mode = '#'"
    endif
endfunction

"}}}

" Shimv
" {{{

set runtimepath+=~/code/vim/shimv

"au filetype haskell noremap ,d :call SendHaskellLet()<cr>
"au filetype haskell noremap ,r :call SendHaskellPlain()<cr>
"au filetype haskell noremap ,l :call SendHaskellLoad()<cr>

" }}}

" GHC mod
" {{{

noremap <leader>i :GhcModType<cr>

" }}}

" NerdCommenter
" {{{

let g:NERDCustomDelimiters = {
    \ 'haskell': { 'left': '-- ', 'leftAlt': '{- ', 'rightAlt': ' -}'},
    \ 'lisp': { 'left': ';; '},
    \ 'obj': { 'left': '#'}
    \ }

map <leader>cc <plug>NERDCommenterAlignBoth
map <leader>cb <plug>NERDCommenterComment

map <leader>cp vip ci

" }}}

" Vim Sexp
" {{{

let g:sexp_filetypes = 'clojure,scheme,lisp,timl,*'
map <M-s> <plug>(sexp_splice_list)
map <M-r> <plug>(sexp_raise_list)

" Slurp
" forwards
map <M-e> mz<M-S-l>`zl
imap <M-e> <esc>mz<M-S-l>`za
map <C-S-0> mz<M-S-l>`al
imap <C-S-0> <esc>mz<M-S-l>`za
" backwards
map <C-S-9> mz<M-S-h>`al
imap <C-S-9> <esc>mz<M-S-h>`za

" Barf
" forwards
map <M-C-e> mz<M-S-k>`zl
imap <M-C-e> <esc>mz<M-S-k>`za
map <C-S-0> mz<M-S-k>`zl
imap <C-S-0> <esc>mz<M-S-k>`za
" Backwards
map <C-S-9> mz<M-S-k>`zl

" }}}

" Gundo
"{{{

map <F5> :GundoToggle<CR>
let g:gundo_preview_bottom = 1
let g:gundo_width = 35
let g:gundo_close_on_revert = 1

"}}}

" Ruby Block
" {{{

runtime macros/matchit.vim

" }}}

" CommandT
" {{{

noremap <leader>f :CommandT<cr>
noremap <leader>m :CommandTTag<cr>
noremap <leader>b :CommandTBuffer<cr>

let g:CommandTFileScanner        = 'find'
let g:CommandTMatchWindowReverse = 1
let g:CommandTBackspaceMap       = '<BS>'
let g:CommandTAcceptSelectionMap = '<tab>'
let g:CommandTCursorLeftMap      = '<C-b>'
let g:CommandTCursorRightMap     = '<C-f>'

" }}}

" CtrlP
" {{{

let g:ctrlp_show_hidden = 1

set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.cache*,*.hi,*~,*.o

let g:ctrlp_max_files = 100000

let g:ctrlp_cache_dir = $HOME . '/.cache/ctrlp'

"if executable('ag')
    "let g:ctrlp_user_command = 'ag %s --ignore ".cache" --ignore "tmp" --ignore "*.swp" --ignore "*.so" --hidden --nocolor -l -g ""'
    "let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'
"endif

" }}}

" Paredit
" {{{

"au FileType vim call PareditInitBuffer ()

" }}}

" AutoPairs
" {{{

let g:AutoPairsCenterLine = 0
"let g:AutoPairsMapSpace = 0

" }}}

" YouCompleteMe
" {{{

let g:ycm_path_to_python_interpreter                    = '/usr/bin/python2'
let g:ycm_register_as_syntastic_checker                 = 0
let g:ycm_collect_identifiers_from_tags_files           = 1
let g:ycm_seed_identifiers_with_syntax                  = 1
let g:ycm_key_detailed_diagnostics                      = ''
let g:ycm_collect_identifiers_from_comments_and_strings = 1
let g:ycm_confirm_extra_conf                            = 0
let g:ycm_complete_in_comments                          = 1
let g:ycm_add_preview_to_completeopt                    = 0
let g:ycm_semantic_triggers                             = { 'haskell': ['.'],
                            \ 'bash': ['#!'], 'python': ['.'], 'rust': ['.']}

nnoremap <leader>h :YcmCompleter GoTo<CR>
let g:necoghc_enable_detailed_browse = 1
au! BufRead,BufNewFile *.hs setlocal omnifunc=necoghc#omnifunc


" }}}

" Slimv
"{{{

let g:slimv_swank_cmd = '! screen -dmS sbcl /usr/bin/sbcl --load /home/russ/.vim/bundle/slimv/slime/start-swank.lisp &'

let g:paredit_mode = 0
let g:lisp_rainbow = 1
let g:slimv_menu = 0

"}}}

" Ultisnips
" {{{

let g:UltiSnipsExpandTrigger="<C-s>"
"let g:UltiSnipsSnippetsDir = "~/.vim/bundle/vim-snippets/Ultisnips"
let g:UltiSnipsSnippetsDir = "~/.vim/UltiSnips/"


" }}}

"Haskell-vim
"{{{

let g:haskell_indent_if    = 4
let g:haskell_indent_case  = 4
let g:haskell_indent_let   = 4
let g:haskell_indent_where = 4
let g:haskell_indent_do    = 4
let g:haskell_indent_in    = 4

"}}}

"Easymotion setup
"{{{

"let g:EasyMotion_do_mapping = 0

"map <leader>g <Plug>(easymotion-bd-w)
"map <leader>s <Plug>(easymotion-s)

"let g:EasyMotion_smartcase = 1
"hi EasyMotionShade guifg=#998f84

"}}}

"Supertab Setup
" {{{

"au Filetype python set omnifunc=pythoncomplete#Complete
"let g:SuperTabDefaultCompletionType = "context"
"let g:SuperTabContextDefaultCompletionType = "<c-n>"
"let g:SuperTabNoCompleteAfter = ['^', '\s', 'do']
"let g:SuperTabLongestEnhanced = 1

set completeopt=longest,menu

" }}}

"Airline Setup
"{{{

let g:airline#extensions#whitespace#enabled = 1
let g:airline#extensions#Syntastic#enabled = 0
let g:airline_powerline_fonts=1

function! Getsyntaxmode()
    if g:airline#extensions#whitespace#enabled == 1
        let l:wsmode = '#'
    else
        let l:wsmode = 'O'
    endif
    return (g:hask_error_mode . g:hask_warning_mode . l:wsmode)
endfunction

call airline#parts#define_function('syntaxmode', 'Getsyntaxmode')

let g:airline_section_z = airline#section#create(['syntaxmode',' ','windowswap', '%3p%%',' ', 'linenr', ':%3c '])

"autocmd BufRead,BufNewFile *.hs call HaskAirline()

"}}}

"}}}

" Other tweaks
"{{{

" Command Aliases
augroup VIMRC_aliases
    au!
    "set bw to bd
    au VimEnter * CmdAlias bw bd
    au VimEnter * CmdAlias ech TabMessage
augroup END

"import System.Randomedit home
cabbrev E <c-r>=(getcmdtype()==':' && getcmdpos()==1 ? 'e ~/ <Backspace>' : 'E')<CR>

"send result of cmd into buffer
function! TabMessage(cmd)
  redir => message
  silent execute a:cmd
  redir END
  let @"=message
endfunction

command! -nargs=+ -complete=command TabMessage call TabMessage(<q-args>)
"cabbrev ech <c-r>=(getcmdtype()==':' && getcmdpos()==1 ? 'TabMessage' : 'ech')<CR>

"fix cabal
let $PATH=$PATH . ':/home/russ/.cabal/bin'

" Save on losing focus
au FocusLost * :wa

" Readonly file saving
command! W w !sudo tee % > /dev/null

"" Return to last edit position when opening files

if has("autocmd")
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

"augroup BgHighlight
    "au!
    "au FocusGained * set colorcolumn=81
    "au FocusLost * set colorcolumn=0
"augroup end

"}}}

" Filetype specific settings
" {{{

set omnifunc=syntaxcomplete#Complete

au BufRead,BufNewFile *.py2 set filetype=python
au BufRead,BufNewFile *.py2 let g:syntastic_python_python_exec = '/usr/bin/python2'

au BufRead,BufNewFile *.x set filetype=alex

"au BufRead,BufNewFile *.rs,*.clj,*.cl,*.lisp,*.cl set omnifunc=syntaxcomplete#Complete
"au FileType rust,clojure,lisp,cs,systemd set omnifunc=syntaxcomplete#Complete

au FileType lisp let b:AutoPairs = {'(':')', '[':']', '{':'}','"':'"', '`':'`'}
au FileType lisp let b:AutoPairsMapCR = 0
au FileType lisp set lisp
"au FileType lisp noremap ,d "ryip:call system("screen -S wat -p wat -X stuff '" . @r . "'")<cr>

" }}}
