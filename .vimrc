set incsearch       " When searching, jump to matches for the partial search
                    " before enter is pressed
set hlsearch        " Highlight search matches
set ignorecase      " ignore case in searches
set smartcase       " nevermind, care about case, but only if we have capital
                    " letters in our search
set scrolloff=5     " show 5 lines ahead of the cursor when scrolling

set ruler           " displays the cursor column/row
set number          " enables the numbers along the left

set smartindent     " try to do smart autoindenting for C-like programs
set expandtab       " use spaces instead of tabs

set vb t_vb=""        " turn off beeping

set tabstop=4 shiftwidth=4 softtabstop=4    " a tab == 4 spaces
set spellfile=~/.vim/spellfile.en.add       " dictionary to spellcheck certain file types
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*   " ignore these files
set showcmd                                 " show (partial) command at bottom

set foldlevel=20    " Open folds automatically down to 20 folds deep

set nocompatible

" Invoke pathogen (this invokes all other plugins at ~/.vim/bundle)
call pathogen#infect()
call pathogen#helptags()

color flattr                                " color scheme

filetype plugin indent on
syntax on

let g:haskell_conceal = 0 " vim2hs will read in every line if I don't disable
                          " this. Lags like hell.

" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
autocmd BufReadPost *
            \ if line("'\"") > 0 && line("'\"") <= line("$") |
            \   exe "normal g`\"" |
            \ endif

au BufRead,BufNewFile *.mkd,*.markdown,*.md,*.mkdn setf mkd
au BufRead,BufNewFile *.mkd,*.markdown,*.md,*.mkdn setlocal spell

au BufRead,BufNewFile *.proto setf c

" For all text and markdown files set 'textwidth' to 80 characters.
autocmd FileType text setlocal textwidth=80
autocmd FileType mkd setlocal textwidth=80

if has('statusline')
    set laststatus=2

    set statusline=%<%f\                     " Filename
    set statusline+=%w%h%m%r                 " Options
    set statusline+=\ [%{&ff}/%Y]            " Filetype
    set statusline+=%=%-14.(%l,%c%V%)\ %p%%  " Right aligned file nav info
endif

" pressing jj or kk will get you out of insert mode
inoremap jj <ESC>
inoremap kk <ESC>

" being dumb and holding down shift too long doesn't screw you
command WQ wq
command Wq wq
command W w
command Q q

" Make the following mappings:
" ,p == enter or leave paste mode and hide the numbers
" ,c == clear the current search
" ,s == search for the selected text (intended for use in visual mode)
let mapleader = ","
map <Leader>p :set invpaste invnumber<CR>
map <Leader>c :let @/ = ""<CR>
map <Leader>s y/<C-R>"<CR>

" Key binding for hoogle searches
au BufNewFile,BufRead *.hs map <buffer> <Leader>hs :Hoogle
au BufNewFile,BufRead *.hs map <buffer> <Leader>hi :HoogleInfo
au BufNewFile,BufRead *.hs map <buffer> <Leader>hc :HoogleClose<CR>

" allow backspacing over everything in insert mode
set backspace=indent,eol,start
