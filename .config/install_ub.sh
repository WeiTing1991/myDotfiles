# !bin/bash

# update
sudo apt update && upgrade

# install dependenies
sudo apt install build-essential
sudo apt install curl git

# install brew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
sudo add-apt-repository universe
sudo apt install gnome-tweaks

# install keyboard(chinese)
sudo apt install ibus-chewing

# install zsh
sudo apt install zsh -y
zsh --version
chsh -s $(which zsh)

