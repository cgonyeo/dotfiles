autoload colors; colors

### Tab completion

# Force a reload of completion system if nothing matched; this fixes installing
# a program and then trying to tab-complete its name
_force_rehash() {
    (( CURRENT == 1 )) && rehash
    return 1    # Because we didn't really complete anything
}

# Always use menu completion, and make the colors pretty!
zstyle ':completion:*:default' list-colors ''

# Completers to use: rehash, general completion, then various magic stuff and
# spell-checking.  Only allow two errors when correcting
zstyle ':completion:*' completer _force_rehash _complete _ignored _match _correct _approximate _prefix
zstyle ':completion:*' max-errors 2

# When looking for matches, first try exact matches, then case-insensiive, then
# partial word completion
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'r:|[._-]=** r:|=**'

# Turn on caching, which helps with e.g. apt
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

# Show titles for completion types and group by type
zstyle ':completion:*:descriptions' format "$fg_bold[black]» %d$reset_color"
zstyle ':completion:*' group-name ''

zstyle :compinstall filename '/home/derek/.zshrc'

# Always do mid-word tab completion
setopt complete_in_word

autoload -Uz compinit
compinit


### History
setopt extended_history         # Save each command’s beginning timestamp (in
                                # seconds since the epoch) and the duration (in
                                # seconds) to the history file

setopt hist_no_store            # Remove the history (fc -l) command from the
                                # history list when invoked

setopt hist_ignore_dups         # Do not enter command lines into the history
                                # list if they are duplicates of the previous
                                # event

setopt hist_expire_dups_first   # When trimming history file, remove duplicates
                                # first

setopt hist_find_no_dups        # Don't show duplicates when searching history,
                                # even if non-contiguous

setopt inc_append_history       # Append to history file immediately, not on
                                # shell exit

setopt share_history            # Imports new commands from the history file
                                # and causes your typed commands to be appended
                                # to the history file

setopt hist_reduce_blanks       # Remove superfluous blanks from each command
                                # line being added to the history list.

setopt hist_ignore_space        # Remove command lines from the history list
                                # when the first character on the line is a
                                # space

export HISTFILE=~/.zhistory
export HISTSIZE=1000000
export SAVEHIST=1000000


### Some..  options
setopt autocd           # If the command isn't valid, and is the name of a
                        # directory, cd into it

setopt extendedglob     # Treat the ‘#’, ‘~’ and ‘^’ characters as part of
                        # patterns for filename generation, etc. 

setopt nomatch          # If a pattern for filename generation has no matches,
                        # print an error, instead of leaving it unchanged in
                        # the argument list

setopt rc_quotes        # Allow the character sequence ‘''’ to signify a single
                        # quote within singly quoted strings

unsetopt notify         # Don't report the status of background jobs
                        # immediately, rather than waiting until just before
                        # printing a prompt.

unsetopt beep           # Don't beep on error

# Don't count common path separators as word characters
WORDCHARS=${WORDCHARS//[&.;\/]}

# "Words cannot express how fucking sweet this is" ~eevee
REPORTTIME=5


### Prompt

# user@machine $EXIT_CODE $PATH>
PROMPT="%n@%m %(?..%{$fg_no_bold[red]%}%?%{$reset_color%} )%~> "

### Misc environment and alias stuff

# Don't glob with find, wget, curl, or nix-env
for command in find wget curl nix-env; \
    alias $command="noglob $command"

### ls

LSOPTS='-vF --si'  # natural sort, type squiggles, friendly sizes
LLOPTS='-lA'       # long mode, show all, 
case $(uname -s) in
    FreeBSD)
        LSOPTS="${LSOPTS} -G"
        ;;
    Linux)
        eval "$(dircolors -b)"
        LSOPTS="$LSOPTS --color=auto"
        LLOPTS="$LLOPTS --color=always"  # so | less is colored

        # Just loaded new ls colors via dircolors, so change completion colors
        # to match
        zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
        ;;
esac
alias ls="ls $LSOPTS"
alias ll="ls $LLOPTS | less -FX"

### Recent Directories
#
# maintain a list of recent directories, for quick cd'ing to
DIRSTACKFILE="$HOME/.zdirs"
if [[ -f $DIRSTACKFILE ]] && [[ $#dirstack -eq 0 ]]; then
  dirstack=( ${(f)"$(< $DIRSTACKFILE)"} )
  [[ -d $dirstack[1] ]] && cd $dirstack[1]
fi
chpwd() {
  print -l $PWD ${(u)dirstack} >$DIRSTACKFILE
}
DIRSTACKSIZE=20
setopt autopushd pushdsilent pushdtohome

# Remove duplicate entries in the recent directories list
setopt pushdignoredups
# This reverts the +/- operators.
setopt pushdminus

### screen (and tmux's screen-compatible title support)

function title {
    # param: title to use

    local prefix=''

    # If I'm in a screen, all the windows are probably on the same machine, so
    # I don't really need to title every single one with the machine name.
    # On the other hand, if I'm not logged in as me (but, e.g., root), I'd
    # certainly like to know that!
    if [[ $USER != 'derek' ]]; then
        prefix="[$USER] "
    fi
    # Set screen window title
    if [[ $TERM == "screen"* ]]; then
        print -n "\ek$prefix$1\e\\"
    fi


    # Prefix the xterm title with the current machine name, but only if I'm not
    # on a local machine.  This is tricky, because screen won't reliably know
    # whether I'm using SSH right now!  So just assume I'm local iff I'm not
    # running over SSH *and* not using screen.  Local screens are fairly rare.
    prefix=$HOST
    if [[ $SSH_CONNECTION == '' && $TERM != "screen"* ]]; then
        prefix=''
    fi
    # If we're showing host and I'm not under my usual username, prepend it
    if [[ $prefix != '' && $USER != 'derek' ]]; then
        prefix="$USER@$prefix"
    fi
    # Wrap it in brackets
    if [[ $prefix != '' ]]; then
        prefix="[$prefix] "
    fi

    # Set xterm window title
    if [[ $TERM == "xterm"* || $TERM == "screen"* ]]; then
        print -n "\e]2;$prefix$1\a"
    fi
}

function precmd {
    # Shorten homedir back to '~'
    local shortpwd=${PWD/$HOME/\~}
    title "zsh $shortpwd"
}

function preexec {
    title $*
}


### Keybindings

bindkey -v # Use vi mode

# General movement
# Taken from http://wiki.archlinux.org/index.php/Zsh and Ubuntu's inputrc
bindkey "\e[1~" beginning-of-line
bindkey "\e[4~" end-of-line
bindkey "\e[5~" beginning-of-history
bindkey "\e[6~" end-of-history
bindkey "\e[3~" delete-char
bindkey "\e[2~" quoted-insert
bindkey "\e[1;5C" forward-word
bindkey "\e[1;5D" backward-word
bindkey "\e[5C" forward-word
bindkey "\eOc" emacs-forward-word
bindkey "\e[5D" backward-word
bindkey "\eOd" emacs-backward-word
bindkey "\e\e[C" forward-word
bindkey "\e\e[D" backward-word
# for rxvt
bindkey "\e[8~" end-of-line
bindkey "\e[7~" beginning-of-line
# for non RH/Debian xterm, can't hurt for RH/Debian xterm
bindkey "\eOH" beginning-of-line
bindkey "\eOF" end-of-line
# for freebsd console
bindkey "\e[H" beginning-of-line
bindkey "\e[F" end-of-line

# Tab completion
bindkey '^i' complete-word              # tab to do menu
bindkey "\e[Z" reverse-menu-complete    # shift-tab to reverse menu

# Up/down arrow.
# I want shared history for ^R, but I don't want another shell's activity to
# mess with up/down.  This does that.
down-line-or-local-history() {
    zle set-local-history 1
    zle down-line-or-history
    zle set-local-history 0
}
zle -N down-line-or-local-history
up-line-or-local-history() {
    zle set-local-history 1
    zle up-line-or-history
    zle set-local-history 0
}
zle -N up-line-or-local-history

bindkey "\e[A" up-line-or-local-history
bindkey "\eOA" up-line-or-local-history
bindkey "\e[B" down-line-or-local-history
bindkey "\eOB" down-line-or-local-history

# emacs users will expect these...
bindkey "^a" beginning-of-line
bindkey "^e" end-of-line
bindkey "^r" vi-history-search-backward

# Default behavior for <Esc> / when pressed quickly together is dumb, because
# it defaults to being read as a chord. This stops that, and has them read as
# two separate keys.
vi-search-fix() {
    zle vi-cmd-mode
    zle .vi-history-search-backward
}

autoload vi-search-fix
zle -N vi-search-fix
bindkey -M viins '\e/' vi-search-fix

# Ctrl-Z from the shell will try to fg

fancy-ctrl-z () {
    if [[ $#BUFFER -eq 0 ]]; then
        BUFFER="fg"
        zle accept-line
    else
        zle push-input
        zle zlear-screen
    fi
}
zle -N fancy-ctrl-z
bindkey '^Z' fancy-ctrl-z

# Ctrl-X from the shell will try to fg %-

fancy-ctrl-x () {
    if [[ $#BUFFER -eq 0 ]]; then
        BUFFER="fg %-"
        zle accept-line
    else
        zle push-input
        zle zlear-screen
    fi
}
zle -N fancy-ctrl-x
bindkey '^X' fancy-ctrl-x

### Machine-specific extras

if [[ -r $HOME/.zlocal ]]; then
    source $HOME/.zlocal
fi

source ~/.zaliases

export LESS_TERMCAP_md=$(printf '\e[01;35m')

source ~/.cargo/env
source ~/.profile

. /home/derek/.nix-profile/etc/profile.d/nix.sh
