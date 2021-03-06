parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

export PS1="[\[\e[0;35m\]\d, \t\e[0m\]] \u@\h \[\033[32m\]\w\[\033[01;34m\]\$(parse_git_branch)\[\033[00m\] $ "

alias gpr="git pull --rebase"
alias gcm="git checkout master"
alias gs="git status"
alias grm="git rebase master"
