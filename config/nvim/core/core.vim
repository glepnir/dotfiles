" Set main configuration directory as parent directory
let $VIM_PATH = fnamemodify(resolve(expand('<sfile>:p')), ':h:h')


" Load Modules:
lua require("init")
lua require("options")
lua require("autocmds")

call initself#source_file($VIM_PATH,'core/dein.vim')
call initself#source_file($VIM_PATH,'core/pmap.vim')
call initself#source_file($VIM_PATH,'core/vmap.vim')
call theme#theme_init()

set secure
" vim: set ts=2 sw=2 tw=80 noet :
