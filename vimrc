set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'lifepillar/vim-solarized8'
Plugin 'flazz/vim-colorschemes'
Plugin 'JamshedVesuna/vim-markdown-preview'
call vundle#end()
filetype plugin indent on

syntax on
set tabstop=4 softtabstop=0 expandtab shiftwidth=4 smarttab

set relativenumber
set number

colorscheme wombat

fun! Solarized8Contrast(delta)
  let l:schemes = map(["_low", "_flat", "", "_high"], '"solarized8_".(&background).v:val')
  exe "colors" l:schemes[((a:delta+index(l:schemes, g:colors_name)) % 4 + 4) % 4]
endf

nmap <leader>- :<c-u>call Solarized8Contrast(-v:count1)<cr>
nmap <leader>+ :<c-u>call Solarized8Contrast(+v:count1)<cr>

let vim_markdown_preview_github=1
let vim_markdown_preview_browser='Chromium'
