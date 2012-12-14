set nocompatible
set backspace=2

syntax on
filetype plugin indent on

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

set ruler
set showcmd
set number

if has("gui")
    set go=
    set guifont=Menlo:h14
endif

nnoremap j gj
nnoremap k gk
xnoremap j gj
xnoremap k gk

nnoremap <F10> :NERDTreeToggle<CR>
nnoremap tl :TlistToggle<CR>

let Tlist_GainFocus_On_ToggleOpen = 1
let Tlist_Show_One_File = 1
let Tlist_Use_Right_Window = 1

nmap <F2> :FufCoverageFile<CR>
nmap <F3> :FufBufferTag<CR>
nmap <F4> :FufBufferTagAll<CR>
nmap <F6> :FufTag<CR>
nmap <F8> :FufBuffer<CR>

call pathogen#infect()

set laststatus=2

let g:Gtags_Auto_Update = 1
