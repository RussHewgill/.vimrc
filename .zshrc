# vim: set foldlevel=0 :

# oh-my-zsh stuff {{{
# Path to your oh-my-zsh installation.
ZSH=/usr/share/oh-my-zsh/

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# }}}

autoload -U promptinit
promptinit

export EDITOR='vim'
export JAVA_HOME=/opt/java/

# Git Prompt {{{

# Adapted from code found at <https://gist.github.com/1712320>.
 
setopt prompt_subst
autoload -U colors && colors # Enable colors in prompt
 
# Modify the colors and symbols in these variables as desired.
GIT_PROMPT_SYMBOL="%{$fg[blue]%}±"
GIT_PROMPT_PREFIX="%{$fg[green]%}[%{$reset_color%}"
GIT_PROMPT_SUFFIX="%{$fg[green]%}]%{$reset_color%}"
GIT_PROMPT_AHEAD="%{$fg[red]%}ANUM%{$reset_color%}"
GIT_PROMPT_BEHIND="%{$fg[cyan]%}BNUM%{$reset_color%}"
GIT_PROMPT_MERGING="%{$fg_bold[magenta]%}⚡︎%{$reset_color%}"
GIT_PROMPT_UNTRACKED="%{$fg_bold[red]%}●%{$reset_color%}"
GIT_PROMPT_MODIFIED="%{$fg_bold[yellow]%}●%{$reset_color%}"
GIT_PROMPT_STAGED="%{$fg_bold[green]%}●%{$reset_color%}"
 
# Show Git branch/tag, or name-rev if on detached head
parse_git_branch() {
(git symbolic-ref -q HEAD || git name-rev --name-only --no-undefined --always HEAD) 2> /dev/null
}
 
# Show different symbols as appropriate for various Git repository states
parse_git_state() {
 
# Compose this value via multiple conditional appends.
local GIT_STATE=""
 
local NUM_AHEAD="$(git log --oneline @{u}.. 2> /dev/null | wc -l | tr -d ' ')"
if [ "$NUM_AHEAD" -gt 0 ]; then
GIT_STATE=$GIT_STATE${GIT_PROMPT_AHEAD//NUM/$NUM_AHEAD}
fi
 
local NUM_BEHIND="$(git log --oneline ..@{u} 2> /dev/null | wc -l | tr -d ' ')"
if [ "$NUM_BEHIND" -gt 0 ]; then
GIT_STATE=$GIT_STATE${GIT_PROMPT_BEHIND//NUM/$NUM_BEHIND}
fi
 
local GIT_DIR="$(git rev-parse --git-dir 2> /dev/null)"
if [ -n $GIT_DIR ] && test -r $GIT_DIR/MERGE_HEAD; then
GIT_STATE=$GIT_STATE$GIT_PROMPT_MERGING
fi
 
if [[ -n $(git ls-files --other --exclude-standard 2> /dev/null) ]]; then
GIT_STATE=$GIT_STATE$GIT_PROMPT_UNTRACKED
fi
 
if ! git diff --quiet 2> /dev/null; then
GIT_STATE=$GIT_STATE$GIT_PROMPT_MODIFIED
fi
 
if ! git diff --cached --quiet 2> /dev/null; then
GIT_STATE=$GIT_STATE$GIT_PROMPT_STAGED
fi
 
if [[ -n $GIT_STATE ]]; then
echo "$GIT_PROMPT_PREFIX$GIT_STATE$GIT_PROMPT_SUFFIX"
fi
 
}
 
# If inside a Git repository, print its branch and state
git_prompt_string() {
local git_where="$(parse_git_branch)"
[ -n "$git_where" ] && echo "$GIT_PROMPT_SYMBOL$(parse_git_state)$GIT_PROMPT_PREFIX%{$fg[yellow]%}${git_where#(refs/heads/|tags/)}$GIT_PROMPT_SUFFIX"
}
 
# Set the right-hand prompt
RPS1='$(git_prompt_string) '

# }}}

#TODO: notes right of directory
PROMPT='%{$fg[white]%}%n@%M%{$reset_color%} [%{$fg[green]%}%~%{$reset_color%}] 
%{$fg[white]%}$%{$reset_color%} '
#RPROMPT='%M'

# Keybinds {{{
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
# }}}

setopt nobeep
setopt AUTO_CD

export PATH=$HOME/bin:/usr/local/bin:$PATH
export LANG=en_US.UTF-8

set -U BROWSER 'firefox-nightly'

# aliases {{{

alias ls='ls -a --group-directories-first --color=always'
alias lsd='ls -d */'
alias :q='exit'

alias hask='clear; ghci'
alias hhask='cd ~/programs/haskell/; clear; ghci'
alias xmds='cd ~/.xmonad; clear; ghci xmonad.hs'

alias tray='trayer --widthtype pixel --width 200 --heighttype pixel --height 30 --expand false --SetDockType false & nm-applet &> /dev/null &'
alias ktray='pkill nm-applet & pkill trayer'

alias ipy='ipython3'
alias py='python3'
alias server='python3 -m http.server 8080'

alias hman='man --html=firefox-nightly'

alias vi='vim'

alias cdd='cd; cd'
alias ..='cd ..'
alias ...='..; ..'
alias ....='...; ..'

alias grep='grep --color=auto'
alias mkdir='mkdir -p -v'
alias nano='nano -w'
alias ping='ping -c 5'
alias dmesg='dmesg -HL'

alias rm='rm -I'  

alias pacin='sudo pacman -S'
alias pacupg='sudo pacman -Syu'
alias pacins='sudo pacman -U'
alias pacre='sudo pacman -R'
alias pacse='sudo pacman -Ss | grep'

# }}}

