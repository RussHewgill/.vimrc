

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
" Plugin 'https://github.com/tpope/vim-commentary'
Plugin 'scrooloose/syntastic'
Plugin 'tpope/vim-surround'
Plugin 'tmhedberg/SimpylFold'
Plugin 'godlygeek/tabular'
Plugin 'Lokaltog/vim-easymotion'
"Plugin 'Shougo/neocomplcache.vim'
Plugin 'eagletmt/neco-ghc'
Plugin 'https://github.com/raichoo/haskell-vim'
Plugin 'https://github.com/kien/ctrlp.vim'
Plugin 'https://github.com/SirVer/ultisnips'
Plugin 'https://github.com/honza/vim-snippets'
"Plugin 'davidhalter/jedi-vim'
Plugin 'https://github.com/Valloric/YouCompleteMe'
"Plugin 'ervandew/supertab'
"Plugin 'https://github.com/nathanaelkane/vim-indent-guides'

"Plugin 'https://github.com/kien/rainbow_parentheses.vim'
"Plugin 'michaeljsmith/vim-indent-object'
"Plugin 'terryma/vim-multiple-cursors'
"Plugin 'dag/vim2hs' 
"Plugin 'travitch/hasksyn'
"Plugin 'Shougo/vimproc.vim'
"Plugin 'eagletmt/ghcmod-vim'

"Plugin 'sjl/badwolf'
Plugin 'russhewgill/badwolf'
Plugin 'bling/vim-airline'
Plugin 'altercation/vim-colors-solarized'
Plugin 'tomasr/molokai'
Plugin 'tpope/vim-vividchalk'
Plugin 'https://github.com/nanotech/jellybeans.vim'


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
"{{{

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
set history=5000

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

syntax on

set wrap
set textwidth=79
set formatoptions=qrn1
set colorcolumn=83

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
set gdefault
set incsearch
set showmatch
set hlsearch

nmap <silent> ,, :let @/=""<cr>
vmap <silent> ,, :let @/=""<cr>

nnoremap <tab> %
vnoremap <tab> %
"}}}

""""""""""""""""""""""""""""""""""""""""""""""""""
"Colors and Fonts
""""""""""""""""""""""""""""""""""""""""""""""""""
"{{{
syntax on
let g:hs_highlight_types=1
let g:hs_highlight_boolean=1

set background=dark
"colorscheme jellybeans
"colorscheme base16-default

if has("gui_running")
    let g:badwolf_darkgutter = 1
    "colorscheme badwolf
    colorscheme badwolf

    set guifont=Inconsolata\ Medium\ 12
    set guioptions-=T
    set guioptions-=e
    set guioptions+=c
    set guioptions-=m
    set guioptions-=r
    set guioptions-=l
    set guioptions-=b
    set guitablabel=%M\ %t
    set guiheadroom=0
else
    set t_Co=256
    let g:badwolf_darkgutter = 1
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

inoremap jk <Esc>
inoremap jj <Esc>i
inoremap jl <Esc>la

nnoremap <leader>tr :w<CR>:so %<cr>

"Hardcore mode
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> =>
inoremap <down> <nop>
inoremap <left> <-
inoremap <right> ->

nnoremap <Leader>tn :tabnew<cr>
nnoremap <Leader>to :tabonly<cr>
nnoremap <Leader>tq :tabclose<cr>
nnoremap <Leader>tm :tabmove

nnoremap <Leader>k :tabn<CR>
nnoremap <Leader>j :tabp<CR>

nnoremap <Leader>tv :tabe ~/.vimrc<CR>
nnoremap <Leader>tx :tabe ~/.xmonad/xmonad.hs<CR>
nnoremap <Leader>tz :tabe ~/.zshrc<CR>
nnoremap <Leader>ta :tabe ~/.config/.aliases<CR>

nnoremap <Leader>f :w<CR>

nmap <Leader>y "*y
vmap <Leader>y "*y
nmap <Leader>d "*d
vmap <Leader>d "*d
nmap <Leader>p "*p
vmap <Leader>p "*p

nmap <Leader><Leader>y "+y
vmap <Leader><Leader>y "+y
nmap <Leader><Leader>d "+d
vmap <Leader><Leader>d "+d
nmap <Leader><Leader>p "+p
vmap <Leader><Leader>p "+p

"Tabularize
nnoremap <Leader><Tab> :Tabularize /

nnoremap ; :
vnoremap ; :
noremap <C-f> ;
noremap <C-c> ,

nnoremap o o<esc>
nnoremap O O<esc>

noremap <M-o> o
noremap <M-O> O

"Split Commands

nnoremap <Leader>w <C-w>v<C-w>l
nnoremap <Leader>e <C-w>n
nnoremap <Leader>- <C-w>-
nnoremap <Leader>= <C-w>=
nnoremap <Leader>+ <C-w>+

nnoremap <C-h> <C-w>h
nnoremap <C-k> <C-w>k
nnoremap <C-j> <C-w>j
nnoremap <C-l> <C-w>l

noremap <C-y> 3<C-y>
noremap <C-e> 3<C-e>

nnoremap <leader>c gc
vnoremap <leader>c gc

nnoremap Q @@
vnoremap Q @@

" Move a line with alt+[jk], indent with alt +[hl]
nnoremap <A-j> :m+<CR>==
nnoremap <A-k> :m-2<CR>==
nnoremap <A-h> <<
nnoremap <A-l> >>
inoremap <A-j> <Esc>:m+<CR>==gi
inoremap <A-k> <Esc>:m-2<CR>==gi
inoremap <A-h> <Esc><<`]a
inoremap <A-l> <Esc>>>`]a
vnoremap <A-j> :m'>+<CR>gv=gv
vnoremap <A-k> :m-2<CR>gv=gv
vnoremap <A-h> <gv
vnoremap <A-l> >gv

" Folding
" {{{

nnoremap zx zo
nnoremap zX zO
nnoremap ZX zO

" }}}

"}}}

"Plugin Settings
"{{{

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

" YouCompleteMe
" {{{

"let g:ycm_path_to_python_interpreter                    = '/usr/bin/python'
let g:ycm_register_as_syntastic_checker                 = 1
let g:ycm_collect_identifiers_from_tags_files           = 1
let g:ycm_seed_identifiers_with_syntax                  = 1
let g:ycm_key_detailed_diagnostics                      = ''
let g:ycm_collect_identifiers_from_comments_and_strings = 1
let g:ycm_semantic_triggers                             = { 'haskell' : ['.'],  
                            \ 'bash' : ['#!'], 'python' : [] }

"autocmd FileType python setlocal omnifunc=pythoncomplete#Complete

nnoremap <leader>h :YcmCompleter GoTo<CR>

" }}}

" Ultisnips
" {{{

let g:UltiSnipsExpandTrigger="<C-s>"
let g:UltiSnipsSnippetsDir = "~/.vim/bundle/vim-snippets/Ultisnips"

" }}}

" Syntastic
"{{{

let g:syntastic_haskell_ghc_mod_args = "-g -fno-warn-missing-signatures"
"let g:syntastic_asm_dialect = 'nasm'

let g:syntastic_python_python_exec = '/usr/bin/python'

let g:syntastic_mode_map = { 'mode': 'active',
                        \ 'active_filetypes': [],
                        \ 'passive_filetypes': [] }

let g:syntastic_python_checkers = ['python', 'pylint']

let g:syntastic_python_pylint_rcfile='/home/russ/.pylintrc'

let g:syntastic_auto_loc_list=0
nnoremap <silent> <leader>b :SyntasticCheck mypy<CR>:Errors<CR>
nnoremap <silent> <leader>v :lclose<CR>
vnoremap <silent> <leader>b :SyntasticCheck mypy<CR>:Errors<CR>
vnoremap <silent> <leader>v :lclose<CR>

"}}}

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

map <leader>g <Plug>(easymotion-bd-w)
map <leader>s <Plug>(easymotion-s)

let g:EasyMotion_smartcase = 1
hi EasyMotionShade guifg=#998f84

"}}}

"Supertab Setup
" {{{

"au Filetype python set omnifunc=pythoncomplete#Complete
"let g:SuperTabDefaultCompletionType = "context"
"let g:SuperTabContextDefaultCompletionType = "<c-n>"
"let g:SuperTabNoCompleteAfter = ['^', '\s', 'do']
"let g:SuperTabLongestEnhanced = 1

"set completeopt=menuone,longest,preview
set completeopt=longest

" }}}

"Airline Setup
"{{{

let g:airline#extensions#whitespace#enabled = 0
let g:airline#extensions#Syntastic#enabled = 0

"}}}

"}}}

" Other tweaks
"{{{

"edit home
cabbrev E <c-r>=(getcmdtype()==':' && getcmdpos()==1 ? 'e ~/ <Backspace>' : 'E')<CR>

"send result of cmd into buffer
function! TabMessage(cmd)
  redir => message
  silent execute a:cmd
  redir END
  let @"=message
endfunction

command! -nargs=+ -complete=command TabMessage call TabMessage(<q-args>)
cabbrev ech <c-r>=(getcmdtype()==':' && getcmdpos()==1 ? 'TabMessage' : 'ech')<CR>

"fix cabal
let $PATH=$PATH . ':/home/russ/.cabal/bin'

" Save on losing focus
au FocusLost * :wa

" Readonly file saving
command! W w !sudo tee % > /dev/null

"" Return to last edit position when opening files (You want this!)
"autocmd BufReadPost *
     "\ if line("'\"") > 0 && line("'\"") <= line("$") |
     "\   exe "normal! g`\"" |
     "\ endif
"" Remember info about open buffers on close
"set viminfo^=%

if has("autocmd")
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

augroup BgHighlight
    au!
    au FocusGained * set colorcolumn=83
    au FocusLost * set colorcolumn=0
augroup end

"}}}

" Filetype specific settings
" {{{

"au Filetype haskell match HaskBrackets /\[\]/

"python


au BufRead,BufNewFile *.py2 set filetype=python
au BufRead,BufNewFile *.py2 let g:syntastic_python_python_exec = '/usr/bin/python2'

" change this if i ever need to use perl
" Welp, perl
"au BufNewFile,BufRead *.pl set filetype=prolog

" }}}

