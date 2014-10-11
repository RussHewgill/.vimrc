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
"Plugin 'tmhedberg/SimpylFold'
Plugin 'godlygeek/tabular'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'eagletmt/neco-ghc'
"Plugin 'raichoo/haskell-vim'
Plugin 'kien/ctrlp.vim'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'Valloric/YouCompleteMe'
Plugin 'bruno-/vim-husk'
Plugin 'kana/vim-textobj-user'
Plugin 'nelstrom/vim-textobj-rubyblock'
Plugin 'fs111/pydoc.vim'
Plugin 'Raimondi/delimitMate'
Plugin 'https://github.com/tpope/vim-repeat'
Plugin 'sjl/gundo.vim'

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

nnoremap <silent> ,, :let @/=""<cr>
vnoremap <silent> ,, :let @/=""<cr>

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

"set background=dark
set background=light
"colorscheme jellybeans
"colorscheme base16-default

if has("gui_running")
    set t_Co=256
    "let g:badwolf_darkgutter = 1
    colorscheme badwolf
    "let g:solarized_termcolors=256
    "colorscheme solarized

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

inoremap jk <Esc>
inoremap JK <Esc>
inoremap jj <Esc>i
inoremap jl <Esc>la

noremap <leader>tr :w<CR>:so %<cr>
noremap <leader>u :w<cr>:!%:p<cr><cr>

noremap ,. @:

noremap n nzz
noremap N Nzz

"Hardcore mode
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>

nnoremap <Leader>tn :tabnew<cr>
nnoremap <Leader>to :tabonly<cr>
nnoremap <Leader>tq :tabclose<cr>
nnoremap <Leader>tm :tabmove

nnoremap <Leader>k :tabn<CR>
nnoremap <Leader>j :tabp<CR>

noremap <leader><leader>h :Pydoc<space>

nnoremap <Leader>tv :tabe ~/.vimrc<CR>
nnoremap <Leader>tx :tabe ~/.xmonad/xmonad.hs<CR>
nnoremap <Leader>tz :tabe ~/.zshrc<CR>
nnoremap <Leader>ta :tabe ~/.config/.aliases<CR>
nnoremap <Leader>ti :tabe /usr/lib/python3.4/site-packages/prompt_toolkit/key_bindings/vi.py<CR>

map <Leader>y "*y
map <Leader>d "*d
map <Leader>p "*p

map <Leader><Leader>y "+y
map <Leader><Leader>d "+d
map <Leader><Leader>p "+p

"Tabularize
nnoremap <Leader><Tab> :Tabularize /

noremap ; :
noremap <C-f> ;
noremap <C-c> ,
noremap \ ;

nnoremap o o<esc>
nnoremap O O<esc>

noremap <M-o> o
noremap <M-O> O

"Split Commands

nnoremap <Leader>ww <C-w>v<C-w>l
nnoremap <Leader>we <C-w>n
nnoremap <Leader>- <C-w>-
nnoremap <Leader>= <C-w>=
nnoremap <Leader>+ <C-w>+

nnoremap <C-h> <C-w>h
nnoremap <C-k> <C-w>k
nnoremap <C-j> <C-w>j
nnoremap <C-l> <C-w>l

noremap <C-y> 5<C-y>
noremap <C-e> 5<C-e>

noremap <leader>c gc

noremap Q @@

" Folding



"}}}

"Plugin Settings
"{{{

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

let g:ycm_path_to_python_interpreter                    = '/usr/bin/python2'
let g:ycm_register_as_syntastic_checker                 = 0
let g:ycm_collect_identifiers_from_tags_files           = 1
let g:ycm_seed_identifiers_with_syntax                  = 1
let g:ycm_key_detailed_diagnostics                      = ''
let g:ycm_collect_identifiers_from_comments_and_strings = 1
let g:ycm_add_preview_to_completeopt                    = 0
let g:ycm_semantic_triggers                             = { 'haskell' : ['.'],  
                            \ 'bash' : ['#!'], 'python' : [] }


nnoremap <leader>h :YcmCompleter GoTo<CR>

" }}}

" Ultisnips
" {{{

let g:UltiSnipsExpandTrigger="<C-s>"
let g:UltiSnipsSnippetsDir = "~/.vim/bundle/vim-snippets/Ultisnips"

" }}}

" Syntastic
"{{{

let g:syntastic_c_checkers = [ 'gcc' ]
let g:syntastic_python_checkers = ['python', 'pep8']
let g:syntastic_javascript_checkers = [ 'jshint' ]
let g:syntastic_coffee_checkers = [ 'coffeelint' ]
let g:syntastic_haskell_checkers = [ 'hdevtools', 'hlint' ]

let g:syntastic_coffee_coffeelint_args = "--csv --file /usr/lib/node_modules/coffeelint/coffeelint.json"
let g:syntastic_haskell_ghc_mod_args = "-g -fno-warn-missing-signatures"
"let g:syntastic_asm_dialect = 'nasm'

let g:syntastic_python_pylint_rcfile='/home/russ/.pylintrc'
let g:syntastic_python_python_exec = '/usr/bin/python'

let g:syntastic_mode_map = { 'mode': 'active',
                        \ 'active_filetypes': [],
                        \ 'passive_filetypes': [] }



let g:syntastic_auto_loc_list=0
"noremap <silent> <leader>b :SyntasticCheck mypy<CR>:Errors<CR>
"noremap <silent> <leader>v :lclose<CR>

noremap <leader>e :call ToggleErrors()<cr>

function! ToggleErrors()
    let old_last_winnr = winnr('$')
    lclose
    if old_last_winnr == winnr('$')
        " Nothing was closed, open syntastic error location panel
        Errors
    endif
endfunction

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

set completeopt=longest,menu

" }}}

" NERDCommenter
" {{{

map <leader>cc <plug>NERDCommenterAlignBoth

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
au BufRead,BufNewFile *.py2 let g:syntastic_python_checkers = ['python', 'pyflakes']

" change this if i ever need to use perl
" Welp, perl
"au BufNewFile,BufRead *.pl set filetype=prolog

" }}}
