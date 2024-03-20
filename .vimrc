" Line numbers
set number
set relativenumber

" Enable mouse
set mouse=a

" Clipboard settings
set clipboard=unnamedplus

" Indent settings
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
set smartindent
set nowrap

" Undo settings
set nobackup
set undofile

" Searching settings
set ignorecase
set smartcase

" Update settings
set updatetime=100
set timeoutlen=200

" Split settings
set splitright
set splitbelow

" Display settings
set list
set signcolumn=yes
set cursorline

" Scroll settings
set scrolloff=40
set sidescrolloff=1
set conceallevel=0

" Search settings
set incsearch
set nohlsearch

" Appearance settings
set termguicolors
set background=dark
set cmdheight=1

" Behavior settings
set hidden
set noerrorbells
set noswapfile
set backspace=indent,eol,start
set autochdir
set selection=exclusive
set modifiable
set encoding=UTF-8

" Fold settings
" set foldmethod=expr
" set foldexpr=nvim_treesitter#foldexpr()
" set foldlevel=99
let mapleader = " " 

" keyboard settings
" Clear highlights
nnoremap <leader>zz :nohl<CR>

nnoremap <C-s> :w<CR>
nnoremap <leader>q :wq<CR>

" Disable the space key
nnoremap <Space> <Nop>
vnoremap <Space> <Nop>

" Move between buffers
nnoremap <C-i> :bnext<CR>
nnoremap <C-o> :bprevious<CR>

" Auto-pair
inoremap "" ""<left>
inoremap '' ''<left>
inoremap () ()<left>
inoremap {} {}<left>
inoremap [] []<left>

" Indenting
vnoremap < <gv
vnoremap > >gv

" Comments
nnoremap <C-_> gcc
vnoremap <C-_> gc

" Move the highlighted line down
vnoremap J :m '>+1<CR>gv=gv
" Move the highlighted line up
vnoremap K :m '<-2<CR>gv=gv

" Join current line with the line below
nnoremap J mzJ`z

" Keep cursor centered when scrolling
nnoremap <C-d> <C-d>zz
nnoremap <C-u> <C-u>zz

" Keep cursor centered when jumping with 'n' and 'N'
nnoremap n nzzzv
nnoremap N Nzzzv

" Delete selected text and replace with text from system clipboard
xnoremap <leader>p "_dP

" Yank selected text to system clipboard
nnoremap <leader>y "+y
vnoremap <leader>y "+y

" Yank current line to system clipboard
nnoremap <leader>Y "+Y

" Replace the word throughout the file
nnoremap <leader>ss :%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>

" Leave insert mode by pressing leader followed by backspace
inoremap jk <Esc>
inoremap kj <Esc>

