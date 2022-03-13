syntax on
set nocompatible              " be iMproved, required set encoding=utf-8
set tabstop=2 softtabstop=2
set shiftwidth=2
set expandtab 
set ignorecase
set smartcase

set smartindent
set nowrap
set noswapfile
set nobackup
set undodir=~/.vim/undodir
set undofile
set incsearch

set number                     " Show current line number
set relativenumber             " Show relative line numbers

set scrolloff=8

" Disable the default Vim startup message.
set shortmess+=F  " to get rid of the file name displayed in the command line bar

" Disable audible bell because it's annoying.
set noerrorbells visualbell t_vb=

set mouse+=a " Enable mouse support

set colorcolumn=100
highlight ColorColumn ctermbg=0 guibg=lightgrey

call plug#begin('~/.vim/plugged')

Plug 'VundleVim/Vundle.vim'
Plug 'mbbill/undotree'
Plug 'prettier/vim-prettier', { 'do': 'yarn install' }
Plug 'preservim/nerdcommenter'
Plug 'tpope/vim-fugitive'
Plug 'gruvbox-community/gruvbox'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'lervag/vimtex'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'tomlion/vim-solidity'

call plug#end()            " required
filetype plugin indent on    " required


let g:gruvbox_contrast_dark = 'hard'
set background=dark
colorscheme gruvbox

let mapleader = " "

nnoremap <leader>h :wincmd h<CR>
nnoremap <leader>j :wincmd j<CR>
nnoremap <leader>k :wincmd k<CR>
nnoremap <leader>l :wincmd l<CR>
nnoremap <leader>H :windo wincmd H<CR>
nnoremap <leader>J :windo wincmd J<CR>
nnoremap <leader>K :windo wincmd K<CR>
nnoremap <leader>L :windo wincmd L<CR>
nnoremap <leader>o :only<CR>
nnoremap <leader>u :UndotreeShow<CR>
nnoremap <leader>dr :wincmd v<bar> :Ex <bar> :vertical resize 30<CR>
nnoremap <Leader>= :vertical resize +5<CR>
nnoremap <Leader>- :vertical resize -5<CR>
nnoremap <Leader>+ :resize +5<CR>
nnoremap <Leader>_ :resize -5<CR>
nnoremap <Leader>vsp :vsplit<CR>
nnoremap <Leader>hsp :split<CR>

nmap <Leader>gj :diffget //3<CR>
nmap <Leader>gf :diffget //2<CR>
nmap <Leader>gs :G<CR>

set hidden "Not sure what this is
set cmdheight=2

"" Set Prettier on by default
let g:prettier#autoformat = 1
let g:prettier#autoformat_require_pragma = 0
