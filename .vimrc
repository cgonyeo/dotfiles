set incsearch       " When searching, jump to matches for the partial search before enter is pressed
set ignorecase      " ignore case in searches
set smartcase       " nevermind, care about case, but only if we have capital letters in our search
set scrolloff=5     " show 5 lines ahead of the cursor when scrolling

set ruler           " displays the cursor column/row
set background=dark " makes colors nicer on a dark display
set number          " enables the numbers along the left
set foldcolumn=1    " have a column to display folds

set smartindent     " try to do smart autoindenting for C-like programs
set expandtab       " use spaces instead of tabs

set vb t_vb=""		" turn off beeping

set cuc             " draw a vertical line at the cursor
set cursorline      " underline the characters on the row the cursor's on

set tabstop=4 shiftwidth=4 softtabstop=4    " a tab == 4 spaces
set spellfile=~/.vim/spellfile.en.add       " so we can spellcheck certain file types
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*   " ignore these files
set wildmode=longest,list,full


set runtimepath+=/usr/local/go/misc/vim     " go plugins
set runtimepath+=/home/derek/git/vim2hs     " haskell plugins

set foldlevel=20    " Open folds automatically down to 20 folds deep

filetype plugin indent on
syntax on

let g:haskell_conceal = 0 " vim2hs will read in every line if I don't disable this. Lags like hell.

" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
autocmd BufReadPost *
			\ if line("'\"") > 0 && line("'\"") <= line("$") |
			\   exe "normal g`\"" |
			\ endif

au BufRead,BufNewFile *.mkd,*.markdown,*.md,*.mkdn setf mkd
au BufRead,BufNewFile *.mkd,*.markdown,*.md,*.mkdn setlocal spell

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

" Press , and then ip to go into or out of paste mode, and , and then cs to
" clear a search
let mapleader = ","
map <Leader>ip :set invpaste invnumber<CR>
map <Leader>cs :let @/ = ""<CR>

" allow backspacing over everything in insert mode
set backspace=indent,eol,start
