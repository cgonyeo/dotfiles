" Invoke pathogen (this invokes all other plugins at ~/.vim/bundle)
call pathogen#infect()
call pathogen#helptags()


" Filetype Detection
filetype plugin indent on


" Syntax Highlighting
syntax on


" Be iMprovied
set nocompatible


" General Settings
set backspace=indent,eol,start            " allow backspacing over everything in
                                          " insert mode
set vb t_vb=""                            " turn off beeping
set spellfile=~/.vim/spellfile.en.add     " dictionary for spellcheck
set wildignore+=*/.git/*,*/.hg/*,*/.svn/* " ignore these files
set wildmode=list:longest " When filling in for tab-completions and there's more
                          " than one match, list all matches and complete till
                          " longest common string


" Searching
set incsearch       " When searching, jump to matches for the partial search
                    " before enter is pressed
set hlsearch        " Highlight search matches
set ignorecase      " ignore case in searches
set smartcase       " nevermind, care about case, but only if we have capital
                    " letters in our search


" Display
set ruler           " displays the cursor column/row
set number          " enables the numbers along the left
set foldlevel=20    " Open folds automatically down to 20 folds deep
set showcmd         " show (partial) command at bottom
set t_Co=256        " make 256 colors work more often
set lazyredraw      " don't update screen inside macros, etc
set colorcolumn=81  " Highlight the 81st column
set scrolloff=5     " show 5 lines ahead of the cursor when scrolling
set textwidth=80    " wrap after 80 columns
set colorcolumn=+1  " highlight 81st column
color flattr        " color scheme

if has('statusline')
    set laststatus=2

    set statusline=%<%f\                     " Filename
    set statusline+=%w%h%m%r                 " Options
    set statusline+=\ [%{&ff}/%Y]            " Filetype
    set statusline+=%=%-14.(%l,%c%V%)\ %p%%  " Right aligned file nav info
endif


" Indenting
set smartindent     " try to do smart autoindenting for C-like programs
set expandtab       " use spaces instead of tabs
set tabstop=4 shiftwidth=4 softtabstop=4    " a tab == 4 spaces


" backups and other junky files
set backupdir=~/.vim/backup     " get backups outta here
set directory=~/.vim/swap       " get swapfiles outta here
set writebackup                 " temp backup during write
set undodir=~/.vim/undo         " persistent undo storage
set undofile                    " persistent undo on

" unicode
set encoding=utf-8              " best default encoding
setglobal fileencoding=utf-8    " ...
set nobomb                      " do not write utf-8 BOM!
set fileencodings=ucs-bom,utf-8,iso-8859-1
                                " order to detect Unicodeyness


" Remapping

"" Make the following mappings:
"" ,p == enter or leave paste mode and hide the numbers
"" ,c == clear the current search
"" ,s == search for the selected text (intended for use in visual mode)
let mapleader = ","
map <Leader>p :set invpaste invnumber<CR>
map <Leader>c :let @/ = ""<CR>
map <Leader>s y/<C-R>"<CR>

"" Reselect visual block after indent:
vnoremap < <gv
vnoremap > >gv

"" Move around windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

"" pressing jj or kk will get you out of insert mode
inoremap jj <ESC>
inoremap kk <ESC>

"" being dumb and holding down shift too long doesn't screw you
command WQ wq
command Wq wq
command W w
command Q q
command E e
command QA qa
command Qa qa

"" w!! will sudo the write
cmap w!! %!sudo tee > /dev/null %

"" Make Y behave like other capitals
nnoremap Y y$


" Language specific stuff

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
