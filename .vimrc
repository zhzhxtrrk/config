" common settings
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
set scrolloff=3

if has("gui")
    set go=
    set guifont=Menlo:h14
endif

if has("mouse")
    set mouse=a
endif

" common map
nnoremap j gj
nnoremap k gk
xnoremap j gj
xnoremap k gk

" NERDTree & Taglist settings
nnoremap <F10> :NERDTreeToggle<CR>
nnoremap tl :TlistToggle<CR>

let Tlist_GainFocus_On_ToggleOpen = 1
let Tlist_Show_One_File = 1
let Tlist_Use_Right_Window = 1

" fuf keybindings
nmap <F2> :FufCoverageFile<CR>
nmap <F3> :FufBufferTag<CR>
nmap <F4> :FufBufferTagAll<CR>
nmap <F6> :FufTag<CR>
nmap <F8> :FufBuffer<CR>

call pathogen#infect()

" show status bar by default
set laststatus=2

" enable gtags auto update
let g:Gtags_Auto_Update = 1
" gtags map
nnoremap <C-\> :Gtags 
