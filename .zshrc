# vim: set foldlevel=0 :

if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
    source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
    source "${ZDOTDIR:-$HOME}/.zprezto/modules/syntax-highlighting/external/zsh-syntax-highlighting.zsh"
fi

#auto load prompt
autoload -U promptinit
promptinit

autoload -U edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

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
 
#ref: http://techanic.net/2012/12/30/my_git_prompt_for_zsh.html



# }}}

# Set the right-hand prompt
RPS1='$(git_prompt_string) '

#TODO: notes right of directory
PROMPT='%{$fg[white]%}%n@%M%{$reset_color%} [%?] [%{$fg[green]%}%~%{$reset_color%}] 
%{$fg[white]%}$%{$reset_color%} '
#RPROMPT='%M'

preexec () { print -Pn '\e]2;./%c%(!.#.$) $1\a' }

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
#[[ -n "${key[Up]}"       ]]  && bindkey  "${key[Up]}"       up-line-or-history
#[[ -n "${key[Down]}"     ]]  && bindkey  "${key[Down]}"     down-line-or-history
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

zstyle ':prezto:module:editor' key-bindings 'emacs'

# }}}

setopt nobeep
setopt AUTO_CD

setopt inc_append_history
setopt share_history

# Exports {{{

export HISTSIZE=10000000
export SAVEHIST=10000000
export EDITOR='vim'
export JAVA_HOME=/opt/java/
export LANG=en_US.UTF-8
export PATH=$HOME/bin:/usr/local/bin:$PATH:/home/russ/.cabal/bin:/home/russ/.gem/ruby/2.1.0/bin
export JAVA_HOME=

# }}}



set -U BROWSER 'firefox-nightly'

# Aliases {{{

source ~/.config/.aliases

function sd () {
    
}

function b64 () {
    echo "$@" | base64 -d
}

function r13 () {
    echo "$@" | tr '[A-Za-z]' '[N-ZA-Mn-za-m]'
}

function shellcode () {
    for i in `objdump -d $1 | sed -ne '/<main>/,/^ *$/ p' | tr '\t' ' ' | tr ' ' '\n' | egrep '^[0-9a-f]{2}$' ` ;  echo -n '\\x'$i
}

function shh () {
    ssh -t $1 'tmux has-session && tmux -2 attach || tmux -2'
}

function c () {
    bc -l <<< "$@"
}

function pypass () {
    python -c "import crypt, getpass, pwd; print(crypt.crypt('$1', '\$6\$saltsalt\$'))"
}

# }}}
