# Remove the default greeting 
set fish_greeting ""

# Set up dircolors for solarized dark
eval (dircolors /home/rkr/.dir_colors/dircolors | head -n 1 | sed 's/^LS_COLORS=/set -x LS_COLORS /;s/;$//')

set -x PATH "/home/rkr/.local/bin" $PATH

# Load pyenv automatically by adding
# the following to ~/.config/fish/config.fish:

# set -x PATH "/home/rkr/.pyenv/bin" $PATH
# # status --is-interactive; and . (pyenv init -|psub)
# status --is-interactive; and . (pyenv virtualenv-init -|psub)

# emacs ansi-term support
if test -n "$EMACS"
  set -x TERM eterm-color
end

# this function may be required
function fish_title
  true
end
