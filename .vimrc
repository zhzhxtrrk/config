set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

Bundle 'slim-template/vim-slim'
Bundle 'snipMate'
Bundle 'ctrlp.vim'
Bundle 'scrooloose/nerdtree'
Bundle 'mbbill/undotree'
Bundle 'taglist.vim'
Bundle 'sjl/badwolf'
Bundle 'altercation/vim-colors-solarized'
Bundle 'kien/rainbow_parentheses.vim'
Bundle 'guns/vim-clojure-static'
Bundle 'tpope/vim-fireplace'
Bundle 'vim-scripts/paredit.vim'

filetype plugin indent on
syntax on

set backspace=2

set nowb
set nobk

set expandtab
set shiftwidth=4
set tabstop=4
set softtabstop=4
set showmatch
set incsearch
set hlsearch
set wildmenu
set smarttab

set ruler
set number
set scrolloff=3

if has("gui")
    set go=
    nnoremap j gj
    nnoremap k gk
    xnoremap j gj
    xnoremap k gk
    set guifont=Source\ Code\ Pro:h12

    colo badwolf
else
    set nowrap
endif

if has("mouse")
    set mouse=a
endif

" common map
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" locate to last edited position
autocmd BufReadPost *
            \ if line("'\"")>0&&line("'\"")<=line("$") |
            \   exe "normal g'\"" |
            \ endif

" NERDTree & Taglist settings
nnoremap <F10> :NERDTreeToggle<CR>
nnoremap tl :TlistToggle<CR>

let Tlist_GainFocus_On_ToggleOpen = 1
let Tlist_Show_One_File = 1
let Tlist_Use_Right_Window = 1

" show status bar by default
set laststatus=2

" use c syntax for objc .h files
let g:c_syntax_for_h=1

" set ft=slim automantically
autocmd BufNewFile,BufRead *.slim set ft=slim

" Powerline
Bundle 'Lokaltog/vim-powerline'

au BufNewFile,BufRead *.hs set smartindent
