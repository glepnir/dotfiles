

# install chunkwm
brew tap crisidev/homebrew-chunkwm
brew install --HEAD --with-tmp-logging chunkwm
brew install --HEAD --with-logging  koekeishiya/formulae/skhd

# tmux plugin urlview
brew install urlview
brew install extract_url

# start service
brew services start chunkwm
brew services start skhd
