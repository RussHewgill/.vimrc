

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
Plugin 'ervandew/supertab'
Plugin 'scrooloose/syntastic'
Plugin 'tpope/vim-surround'
Plugin 'tmhedberg/SimpylFold'
Plugin 'godlygeek/tabular'
Plugin 'Lokaltog/vim-easymotion'
"Plugin 'Shougo/neocomplcache.vim'
Plugin 'eagletmt/neco-ghc'
Plugin 'https://github.com/raichoo/haskell-vim'
"Plugin 'https://github.com/kien/rainbow_parentheses.vim'

"Plugin 'davidhalter/jedi-vim'
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

set tabstop=2
set shiftwidth=2
set softtabstop=2
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
set wildignore=*.o,*~,*.pyc
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
filetype on

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

"nmap <M-r> 
nmap <silent> ,, :let @/=""<cr>
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
inoremap jh <Esc>i
inoremap jl <Esc>la

nnoremap <leader>tq :w<CR>:so %<cr>

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
nnoremap <Leader>tc :tabclose<cr>
nnoremap <Leader>tm :tabmove

nnoremap <Leader>ty :tabn<CR>
nnoremap <Leader>tt :tabp<CR>

nnoremap <Leader>tv :tabe ~/.vimrc<CR>

"Tabularize
nnoremap <Leader>= :Tabularize /=<CR>
nnoremap <Leader><Tab> :Tabularize /

nnoremap ; :
vnoremap ; :

nnoremap o o<esc>
nnoremap O O<esc>

noremap <M-o> o
noremap <M-O> O

"Split Commands

nnoremap <Leader>w <C-w>v<C-w>l
nnoremap <C-h> <C-w>h
nnoremap <C-k> <C-w>k
nnoremap <C-j> <C-w>j
nnoremap <C-l> <C-w>l

noremap <C-y> 3<C-y>
noremap <C-e> 3<C-e>

nnoremap <leader><leader>b :r~/.vim/snippets/commentbreak.txt<CR>jla

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

" Syntastic
"{{{

let g:syntastic_haskell_ghc_mod_args = "-g -fno-warn-missing-signatures"

"}}}

" Rainbow Parens 
" {{{

"au VimEnter * RainbowParenthesesToggle
"au Syntax * RainbowParenthesesLoadRound
"au Syntax * RainbowParenthesesLoadSquare
"au Syntax * RainbowParenthesesLoadBraces
"au Syntax * if &ft != 'haskell' | RainbowParenthesesLoadBraces


" }}}

" neco-ghc / neocompl
" {{{

let g:neocomplcache_enable_at_startup = 1
"autocmd FileType python setlocal omnifunc=pythoncomplete#Complete

let g:neocomplcache_enable_ignore_case = 1
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_enable_auto_select = 1
let g:neocomplcache_enable_fuzzy_completion = 1
let g:neocomplcache_enable_camel_case_completion = 1
let g:neocomplcache_enable_underbar_completion = 1

" }}}

"Jedi-vim
"{{{

"autocmd FileType python setlocal completeopt-=preview
"let g:jedi#popup_on_dot = 0

"}}}

"Easymotion s=>etup
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
"set completeopt=menuone,longest,preview

" }}}

"Airline Setup
"{{{

let g:airline#extensions#whitespace#enabled = 0

"}}}

"}}}

" Other tweaks
"{{{

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

au BufNewFile,BufRead *.fish set filetype=sh

" change this if i ever need to use perl
"au BufNewFile,BufRead *.pl set filetype=prolog

" }}}

