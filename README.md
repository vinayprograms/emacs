# Emacs customizations

Lot of customizations and plugins that lets me customize emacs to my specific workflow. The configuration is designed to lazy load stuff. So some of the packages won't load until you are in the right mode.

## Installation

1. Clone this repo into `~/.emacs.d/` - `git clone https://github.com/vinayprograms/emacs.git ~/.emacs.d/customize`.
2. Make a symlink to `init.el` - `ln -s ~/.emacs.d/customize/init.el ~/.emacs.d/init.el`
3. Create `~/.emacs.d/.env` file and add the following line to it - `ORG_AGENDA_DIR="$HOME/Documents`. Change `Documents` to the directory where you hold all your org-mode files.

The first time you run emacs, it will take some time to install all the necessary extensions and packages.
