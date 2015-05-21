#!/bin/zsh

FPATH=~/.zsh:$FPATH

# The following lines were added by compinstall
zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' format 'completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list '' 'r:|[._-/]=** r:|=**' 'l:|=* r:|=*' 'm:{[:lower:]}={[:upper:]} m:{[:lower:][:upper:]}={[:upper:][:lower:]}'
zstyle ':completion:*' menu select=long-list select=0
zstyle ':completion:*' original false
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s

## Enables auto completion
autoload -Uz compinit
compinit

## Enables colors
autoload -U colors 
colors

## HISTORY ##
# set the history file and size
HISTFILE=~/.zhistory
HISTSIZE=10000
SAVEHIST=10000

# ignore duplicates and append to the file incrementally
setopt HIST_IGNORE_DUPS
setopt INC_APPEND_HISTORY

# search through the history with the arrow keys
bindkey "^[[A" history-search-backward
bindkey "^[[B" history-search-forward

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

# don't beep and use emacs bindings
unsetopt beep
bindkey -v

# file with various aliases
source $HOME/.zaliases

## KEYS ##
# Assigns certain keys to the correct actions
# On a new system, type in autoload zkdb and then zkdb to generate a new zkdb file
#autoload zkbd
#source ~/.zkbd/$TERM-${${DISPLAY:t}:-$VENDOR-$OSTYPE}
#
#[[ -n ${key[Backspace]} ]] && bindkey "${key[Backspace]}" backward-delete-char
#[[ -n ${key[Insert]} ]]    && bindkey "${key[Insert]}"    overwrite-mode
#[[ -n ${key[Home]} ]]      && bindkey "${key[Home]}"      beginning-of-line
#[[ -n ${key[PageUp]} ]]    && bindkey "${key[PageUp]}"    up-line-or-history
#[[ -n ${key[Delete]} ]]    && bindkey "${key[Delete]}"    delete-char
#[[ -n ${key[End]} ]]       && bindkey "${key[End]}"       end-of-line
#[[ -n ${key[PageDown]} ]]  && bindkey "${key[PageDown]}"  down-line-or-history
#[[ -n ${key[Up]} ]]        && bindkey "${key[Up]}"        up-line-or-search
#[[ -n ${key[Left]} ]]      && bindkey "${key[Left]}"      backward-char
#[[ -n ${key[Down]} ]]      && bindkey "${key[Down]}"      down-line-or-search
#[[ -n ${key[Right]} ]]     && bindkey "${key[Right]}"     forward-char

# create a zkbd compatible hash;
# to add other keys to this hash, see: man 5 terminfo
typeset -A key

key[Home]=${terminfo[khome]}

key[End]=${terminfo[kend]}
key[Insert]=${terminfo[kich1]}
key[Delete]=${terminfo[kdch1]}
key[Up]=${terminfo[kcuu1]}
key[Down]=${terminfo[kcud1]}
key[Left]=${terminfo[kcub1]}
key[Right]=${terminfo[kcuf1]}
key[PageUp]=${terminfo[kpp]}
key[PageDown]=${terminfo[knp]}

# setup key accordingly
[[ -n "${key[Home]}"     ]]  && bindkey  "${key[Home]}"     beginning-of-line
[[ -n "${key[End]}"      ]]  && bindkey  "${key[End]}"      end-of-line
[[ -n "${key[Insert]}"   ]]  && bindkey  "${key[Insert]}"   overwrite-mode
[[ -n "${key[Delete]}"   ]]  && bindkey  "${key[Delete]}"   delete-char
[[ -n "${key[Up]}"       ]]  && bindkey  "${key[Up]}"       up-line-or-history
[[ -n "${key[Down]}"     ]]  && bindkey  "${key[Down]}"     down-line-or-history
[[ -n "${key[Left]}"     ]]  && bindkey  "${key[Left]}"     backward-char
[[ -n "${key[Right]}"    ]]  && bindkey  "${key[Right]}"    forward-char
[[ -n "${key[PageUp]}"   ]]  && bindkey  "${key[PageUp]}"   beginning-of-buffer-or-history
[[ -n "${key[PageDown]}" ]]  && bindkey  "${key[PageDown]}" end-of-buffer-or-history

# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
    function zle-line-init () {
        printf '%s' "${terminfo[smkx]}"
    }
    function zle-line-finish () {
        printf '%s' "${terminfo[rmkx]}"
    }
    zle -N zle-line-init
    zle -N zle-line-finish
fi

#extended globbing
setopt extendedglob

# Terminal title
case $TERM in
    xterm*|*rxvt*)
        precmd () { print -Pn "\e]0;%n@%m: %~\a" }
        preexec () {
            print -Pn "\e]0;%n@%m: $1\a"
        }
        ;;
esac

# Colored man pages

man() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
            man "$@"
}

PROMPT="%n@%m %~> "

# Cabal stuff
function cabal_sandbox_info() {
    cabal_files=(*.cabal(N))
    if [ $#cabal_files -gt 0 ]; then
        if [ -f cabal.sandbox.config ]; then
            echo "%{$fg[green]%}sandboxed%{$reset_color%}"
        else
            echo "%{$fg[red]%}not sandboxed%{$reset_color%}"
        fi
    fi
}

function _cabal_commands() {
    local ret=1 state
    _arguments ':subcommand:->subcommand' && ret=0

    case $state in
      subcommand)
        subcommands=(
          "bench:Run the benchmark, if any (configure with UserHooks)"
          "build:Compile all targets or specific target."
          "check:Check the package for common mistakes"
          "clean:Clean up after a build"
          "copy:Copy the files into the install locations"
          "configure:Prepare to build the package"
          "exec:Run a command with the cabal environment"
          "fetch:Downloads packages for later installation"
          "freeze:Freeze dependencies."
          "get:Gets a package's source code"
          "haddock:Generate Haddock HTML documentation"
          "help:Help about commands"
          "hscolour:Generate HsColour colourised code, in HTML format"
          "info:Display detailed information about a particular package"
          "init:Interactively create a .cabal file"
          "install:Installs a list of packages"
          "list:List packages matching a search string"
          "register:Register this package with the compiler"
          "repl:Open an interpreter session for the given target"
          "report:Upload build reports to a remote server"
          "run:Runs the compiled executable"
          "sandbox:Create/modify/delete a sandbox"
          "sdist:Generate a source distribution file (.tar.gz)"
          "test:Run the test suite, if any (configure with UserHooks)"
          "unpack:Unpacks packages for user inspection"
          "update:Updates list of known packages"
          "upload:Uploads source packages to Hackage"
        )
        _describe -t subcommands 'cabal subcommands' subcommands && ret=0
    esac

    return ret
}

compdef _cabal_commands cabal

function _cab_commands() {
    local ret=1 state
    _arguments ':subcommand:->subcommand' && ret=0

    case $state in
      subcommand)
        subcommands=(
          "sync:Fetch the latest package index"
          "install:Install packages"
          "uninstall:Uninstall packages"
          "installed:List installed packages"
          "configure:Configure a cabal package"
          "build:Build a cabal package"
          "clean:Clean up a build directory"
          "outdated:Display outdated packages"
          "info:Display information of a package"
          "sdist:Make tar.gz for source distribution"
          "upload:Uploading tar.gz to HackageDB"
          "get:Untar a package in the current directory"
          "deps:Show dependencies of this package"
          "revdeps:Show reverse dependencies of this package"
          "check:Check consistency of packages"
          "genpaths:Generate Paths_<pkg>.hs"
          "search:Search available packages by package name"
          "add:Add a source directory"
          "test:Run tests"
          "bench:Run benchmarks"
          "doc:Generate manuals"
          "ghci:Run GHCi (with a sandbox)"
          "init:Initialize a sandbox"
          "help:Display the help message of the command"
        )
        _describe -t subcommands 'cab subcommands' subcommands && ret=0
    esac

    return ret
}

command -v cab >/dev/null 2>&1 && { compdef _cab_commands cab }
