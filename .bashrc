#Fink Test Crap
test -r @PREFIX@/bin/init.sh && . @PREFIX@/bin/init.sh


#Custom Aliases
#ls Options
# Color
alias ls='/bin/ls -G'
#ll
alias ll='ls -l'
#l.
alias l.='ls -la'
#la
alias la='ls -a'

#grep Color
alias grep='grep --color'

#gerbv Screen Geometry
alias gerbv='gerbv --geometry 800x600'

## Path Settings
# For mac mini which doesn't have 'make' in the right spot
export PATH=$PATH:/usr/local/bin:/Developer/usr/bin
# For EPD-7.202
PATH="/Library/Framework/Python.framework/Versions/Current/bin:${PATH}"
export PATH
MKL_NUM_THREADS=1
export MKL_NUM_THREADS


#Console Only matlab -- Run File (and log)
alias cmatlab='matlab -nojvm -nosplash -nodisplay -logfile Out.txt -r '
#Console Only matabl -- shell
alias cmat='matlab -nojvm -nosplash -nodisplay'


# Change default system editor
export EDITOR="emacs -nw"

export EMACSLOADPATH=/usr/share/emacs/22.1/site-lisp:/usr/share/emacs/22.1/lisp

# More custom colors
# This is taken from:
## http://dev-spout.blogspot.com/2011/07/mac-terminal-colors-git-prompt.html
##

# export CLICOLOR=1
alias colors='{
  echo -e -n "${black}black ${Black}Black ${on_white}${BLACK}BLACK$off "
  echo -e -n "${red}red ${Red}Red ${on_yellow}${RED}RED$off "
  echo -e -n "${green}green ${Green}Green ${on_blue}${GREEN}GREEN$off "
  echo -e -n "${yellow}yellow ${Yellow}Yellow ${on_red}${YELLOW}YELLOW$off "
  echo -e -n "${blue}blue ${Blue}Blue ${on_green}${BLUE}BLUE$off "
  echo -e -n "${purple}purple ${Purple}Purple ${on_cyan}${PURPLE}PURPLE$off "
  echo -e -n "${cyan}cyan ${Cyan}Cyan ${on_blue}${CYAN}CYAN$off "
  echo -e -n "${white}white ${White}White ${on_purple}${WHITE}WHITE$off \n"
}'

source ~/.colors
function color_my_prompt {
    local user_and_host="\[${Yellow}\]\u@\h"
    local current_location="\[${Cyan}\]\w"
    local git_branch_color="\[${Red}\]"
    local git_branch='`git branch 2> /dev/null | grep -e ^* | sed -E  s/^\\\\\*\ \(.+\)$/\(\\\\\1\)\ /`'
    local prompt_tail="\[${Purple}\]$"
    local last_color="\[${off}\]"
    export PS1="$user_and_host $current_location $git_branch_color$git_branch$prompt_tail$last_color "
}
color_my_prompt