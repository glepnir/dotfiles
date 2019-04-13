augroup MyAutoCmd
	autocmd WinEnter,InsertLeave * set cursorline
	autocmd WinLeave,InsertEnter * set nocursorline
    autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | execute "normal! g'\"" | endif
    autocmd FileType css setlocal equalprg=csstidy\ -\ --silent=true

	autocmd FileType yaml.docker-compose setlocal expandtab

	" https://webpack.github.io/docs/webpack-dev-server.html#working-with-editors-ides-supporting-safe-write
	autocmd FileType css,javascript,jsx,javascript.jsx
		\ setlocal backupcopy=yes
		\| setlocal equalprg=jslint
augroup END
