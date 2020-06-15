nvim ?= nvim
XDG_CACHE_HOME ?= $(HOME)/.cache

default: install

install:
	@mkdir -vp "$(XDG_CACHE_HOME)/vim/"{backup,session,swap,tags,undo}; \
	$(nvim)  -V1 -es -i NONE -N -u core/dein.vim -c "try | call dein#update() | finally | echomsg '' | qall! | endtry";\
  rm -rf "$(XDG_CACHE_HOME)/vim/dein/cache_vim";\
  rm -rf "$(XDG_CACHE_HOME)/vim/dein/state_nvim.vim";\
  rm -rf "$(XDG_CACHE_HOME)/vim/dein/.cache/";\
  $(nvim) -u init.vim -c 'call dein#recache_runtimepath()|q'
