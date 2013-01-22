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

set nowrap

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

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

autocmd BufReadPost *
            \ if line("'\"")>0&&line("'\"")<=line("$") |
            \   exe "normal g'\"" |
            \ endif

" bundle support
call pathogen#infect()
call pathogen#helptags()

" NERDTree & Taglist settings
nnoremap <F10> :NERDTreeToggle<CR>
nnoremap tl :TlistToggle<CR>

let Tlist_GainFocus_On_ToggleOpen = 1
let Tlist_Show_One_File = 1
let Tlist_Use_Right_Window = 1

" CommandT
map <D-t> :CommandT<CR>
map <D-T> :CommandTGtagBuffer<CR>
map <D-r> :CommandTGtag<CR>
" for console
map ,t :CommandT<CR>
map ,T :CommandTGtagBuffer<CR>
map ,R :CommandTGtag<CR>

" cocoa
map ,s :Alternate<CR>

" auto update gtags database
au BufWritePost * silent CommandTGtagUpdate 

" show status bar by default
set laststatus=2

" use c syntax for objc .h files
let g:c_syntax_for_h=1

" clojure
let g:vimclojure#HighlightBuiltins = 1
let g:vimclujure#ParenRainbow = 1

" gtags map
nnoremap <C-\> :Gtags 

