function! initself#source_file(root_path,path, ...)
	" Source user configuration files with set/global sensitivity
	let use_global = get(a:000, 0, ! has('vim_starting'))
	let abspath = resolve(a:root_path . '/' . a:path)
	if ! use_global
		execute 'source' fnameescape(abspath)
		return
	endif

	let tempfile = tempname()
	let content = map(readfile(abspath),
		\ "substitute(v:val, '^\\W*\\zsset\\ze\\W', 'setglobal', '')")
	try
		call writefile(content, tempfile)
		execute printf('source %s', fnameescape(tempfile))
	finally
		if filereadable(tempfile)
			call delete(tempfile)
		endif
	endtry
endfunction

" Credits: https://github.com/Shougo/shougo-s-github/blob/master/vim/rc/options.rc.vim#L147
" mkdir
function! initself#mkdir_as_necessary(dir, force) abort
  if !isdirectory(a:dir) && &l:buftype == '' &&
        \ (a:force || input(printf('"%s" does not exist. Create? [y/N]',
        \              a:dir)) =~? '^y\%[es]$')
    call mkdir(iconv(a:dir, &encoding, &termencoding), 'p')
  endif
endfunction

function! initself#lsp_init(langs)
  let l:lsp={
    \'go':{'golang': {
          \ "command": "gopls",
          \ "rootPatterns": ["go.mod"],
          \ "disableWorkspaceFolders": "true",
          \ "filetypes": ["go"]
          \ }
          \ },
    \'docker':{'dockerfile': {
          \ "command": "docker-langserver",
          \ "filetypes": ["dockerfile"],
          \ "args": ["--stdio"]
          \ }
          \ },
    \'bash':{'bash': {
          \ "command": "bash-language-server",
          \ "args": ["start"],
          \ "filetypes": ["sh"],
          \ "ignoredRootPaths": ["~"]
          \ }
          \ }
    \}[a:langs]
  call coc#config('languageserver',l:lsp)
endfunction

" COC Jump definition in split window
" when window >=4 jump in other window
function! initself#definition_other_window() abort
  if winnr('$') >= 4 || (winwidth(0) - (max([len(line('$')), &numberwidth-1]) + 1)) < 110
    exec "normal \<Plug>(coc-definition)"
  else
    exec 'vsplit'
    exec "normal \<Plug>(coc-definition)"
  endif
endfunction

" COC select the current word
function! initself#select_current_word()
    if !get(g:, 'coc_cursors_activated', 0)
        return "\<Plug>(coc-cursors-word)"
    endif
    return "*\<Plug>(coc-cursors-word):nohlsearch\<CR>"
endfunc

" exit iron  terminal window
function! initself#exit_iron() abort
  let l:flist = ['gore','ts-node','node','ipython']
  for fname in l:flist
    let l:bnr = bufwinnr(fname)
    if l:bnr > 0
      exec bnr . "wincmd w"
      quit!
    endif
  endfor
endfunction
