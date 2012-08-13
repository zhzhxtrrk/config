set nocompatible

syntax on
filetype plugin indent on

set nowb
set nobk

set expandtab
set shiftwidth=4
set tabstop=4
set showmatch
set hlsearch

set ruler
set showcmd

if has("gui")
    set go=T
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


