set nocompatible
set noshowmode
filetype off
filetype plugin indent on
set backspace=indent,eol,start
let g:python_highlight_all = 1
let g:better_whitespace_enabled = 1
let g:strip_whitespace_on_save = 1
let g:strip_whitespace_confirm = 0
let g:strip_whitelines_at_eof = 1
let g:airline_powerline_fonts = 1
syntax on
nmap =j :%!python -m json.tool

set undofile " Maintain undo history between sessions
set undodir=~/.vim/undodir

autocmd Filetype python setlocal expandtab tabstop=4 shiftwidth=4 softtabstop=4
autocmd Filetype go setlocal tabstop=4 shiftwidth=4 softtabstop=4
set tabstop=4
set shiftwidth=4
set softtabstop=4
