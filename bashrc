# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/jongmin/opt/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/jongmin/opt/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/jongmin/opt/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/jongmin/opt/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup

# <<< conda initialize <<<

set -o vi
# Custom prompt
source ~/.bash/bash_prompt

# Aliases
source ~/.bash/aliases.sh

# Functions
source ~/.bash/functions.sh

#export PATH=/Library/Frameworks/Python.framework/Versions/3.8/bin$PATH

export CLICOLOR=1
