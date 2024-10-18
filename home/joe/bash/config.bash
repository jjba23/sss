# config.bash - Bash configurations

# Copyright (C) 2024 Josep Bigorra

# Version: 0.9.0
# Author: Josep Bigorra <jjbigorra@gmail.com>
# Maintainer: Josep Bigorra <jjbigorra@gmail.com>
# URL: https://github.com/jjba23/sss

# sss is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# sss is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with sss.  If not, see <https://www.gnu.org/licenses/>.

# Commentary:

# TODO

# Code:

# bash colors
blk='\[\033[01;30m\]'   # Black
red='\[\033[01;31m\]'   # Red
grn='\[\033[01;32m\]'   # Green
ylw='\[\033[01;33m\]'   # Yellow
blu='\[\033[01;34m\]'   # Blue
pur='\[\033[01;35m\]'   # Purple
cyn='\[\033[01;36m\]'   # Cyan
wht='\[\033[01;37m\]'   # White
clr='\[\033[00m\]'      # Reset


# set size and add date and time to `history` command
export HISTTIMEFORMAT="%F %T  | "
export HISTSIZE=200000
export HISTCONTROL=ignoreboth:erasedups
export HISTFILESIZE=200000

# set history to append
shopt -s histappend

# Move to the parent folder.
alias ..='cd ..;pwd'

# Move up two parent folders.
alias ...='cd ../..;pwd'

# Move up three parent folders.
alias ....='cd ../../..;pwd'

# Press c to clear the terminal screen.
alias c='clear'

# Press h to view the bash history.
alias h='history'

# Display the directory structure better.
alias tree='tree --dirsfirst -F'

# Make a directory and all parent directories with verbosity.
alias mkdir='mkdir -p -v'

alias ll="ls -lAh --group-directories-first"
alias l="ls -lAh --group-directories-first"
alias e="emacsclient -t"
alias fr="cd ~/Ontwikkeling/Persoonlijk/sss && make fr"
alias sr="cd ~/Ontwikkeling/Persoonlijk/sss && make sr"
alias jr="cd ~/Ontwikkeling/Persoonlijk/sss && make jr"

# View the calender by typing the first three letters of the month.

alias jan='cal -m 01'
alias feb='cal -m 02'
alias mar='cal -m 03'
alias apr='cal -m 04'
alias may='cal -m 05'
alias jun='cal -m 06'
alias jul='cal -m 07'
alias aug='cal -m 08'
alias sep='cal -m 09'
alias oct='cal -m 10'
alias nov='cal -m 11'
alias dec='cal -m 12'

function find_largest_files() {
    du -h -x -s -- * | sort -r -h | head -20;
}

# Grep (search) through your history for previous run commands:

function hg() {
    history | grep "$1";
}

eval "$(fzf --bash)"

# Set the prompt.
export PS1=${grn}"\\u@\\h \\w${GUIX_ENVIRONMENT:+ [env]} λ"${clr}"  "
