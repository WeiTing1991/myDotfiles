if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p11k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

if [[ "$OSTYPE" == "linux-gnu"* ]]; then

    # Linux-specific settings (native Linux, not WSL)
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"

    export PATH=$HOME/bin:$HOME/.local/bin:/usr/local/bin:$PATH
    export PATH="$PATH:/usr/local/bin"
    [ -f /home/linuxbrew/.linuxbrew/bin/fzf ] && source <(fzf --zsh)

    export MANPATH="/usr/local/man:$MANPATH"

elif [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS-specific settings

    eval "$(/opt/homebrew/bin/brew shellenv)"

    # enable fzf keybinding
    [ -f /opt/homebrew/bin/fzf ] && source <(fzf --zsh)

elif [[ "$WSL_DISTRO_NAME" != "" ]]; then

    # WSL-specific settings (Windows Subsystem for Linux)
    export PATH=$HOME/bin:$HOME/.local/bin:/usr/local/bin:$PATH
    # export PATH="$PATH:/usr/local/bin"
    [ -f /home/linuxbrew/.linuxbrew/bin/fzf ] && source <(fzf --zsh)

fi

# ----------------------------------- Environment Variables -----------------------------------


# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Path to your Oh My Zsh installation.
export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="powerlevel10k/powerlevel10k"

# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)

plugins=(
  git
  zsh-syntax-highlighting
  zsh-autosuggestions
)

source $ZSH/oh-my-zsh.sh


export FZF_DEFAULT_COMMAND="fd --hidden --strip-cwd-prefix --exclude .git"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND="fd --type=d --hidden --strip-cwd-prefix --exclude .git"

# combine preview with bat and eza
export FZF_CTRL_T_OPTS="--preview 'bat --color=always --line-range :500 {}'"
export FZF_ALT_C_OPTS="--preview 'eza --tree --color=always {} | head -200'"

# keybindings
bindkey -e
bindkey '^p' history-search-backward
bindkey '^n' history-search-forward

# History
HISTSIZE=5000
HISTFILE=$HOME/.zsh_history
SAVEHIST=$HISTSIZE
HISTDUP=erase
setopt appendhistory
setopt sharehistory
setopt hist_ignore_space
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_find_no_dups

# User configuration
# export MANPATH="/usr/local/man:$MANPATH"
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='nvim'
# fi
#


# Compilation flags
# export ARCHFLAGS="-arch $(uname -m)"


# -------------- Alias -----------------------------------

#aliases
# - $ZSH_CUSTOM/aliases.zsh
# - $ZSH_CUSTOM/macos.zsh
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
#
alias n="nvim"
alias e="exit"
alias pj="cd $HOME/project/"

alias ls="eza --sort=type"
alias tree="eza --tree"
alias tree="eza --tree"


# git tools
alias gd="git diff --name-only --relative --diff-filter=d | xargs bat --diff"

# path
alias ob="cd ~/Library/Mobile\ Documents/iCloud~md~obsidian/Documents/weitingchen"

# disable ctrl-D
setopt ignoreeof

# ----------------------------------- TERM -----------------------------------
# bash and zsh
# if [[ $TERM_PROGRAM != "WarpTerminal" ]]; then
#     # Unsupported plugin/prompt code here, i.e.
#     # test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh" || true
#     [[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && "/usr/local/etc/profile.d/bash_completion.sh"
# fi

# ----------------------------------- PATH -----------------------------------

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# magick
# export DYLD_FALLBACK_LIBRARY_PATH="$(brew --prefix)/lib:$DYLD_FALLBACK_LIBRARY_PATH"
# set DYLD_LIBRARY_PATH to "(brew --prefix)/lib"

# Deno
source $HOME/.local/bin/env
source $HOME/.deno/env
# . "/home/weiting/.deno/env"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# bun completions
[ -s "/home/weiting/.bun/_bun" ] && source "/home/weiting/.bun/_bun"

. "/home/weitingub24/.deno/env"