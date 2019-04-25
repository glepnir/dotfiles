

# install chunkwm
brew tap crisidev/homebrew-chunkwm
brew install --HEAD --with-tmp-logging chunkwm
brew install --HEAD --with-logging  koekeishiya/formulae/skhd


# start service
brew services start chunkwm
brew services start skhd
