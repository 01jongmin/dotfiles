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

# Rust
. "$HOME/.cargo/env"

export GEM_HOME="$HOME/.gem"

# Custom prompt
source ~/.bash/bash_prompt

# Aliases
source ~/.bash/aliases.sh

# Functions
source ~/.bash/functions.sh

#source ~/emsdk/emsdk_env.sh
export PATH=${PATH}:"/Users/jongmin/Library/Python/3.8/bin"

export CLICOLOR=2
