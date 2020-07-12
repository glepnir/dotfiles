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

function! initself#silent_organizeImport()
  silent! call CocAction('runCommand', 'editor.action.organizeImport')
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

function! initself#clap_go_source()
  let l:go_root = globpath('/usr/local/Cellar/go', '*').'/libexec/src/'
  let l:go_root_file_list = split(globpath(l:go_root, '*'))
  let l:result=[]
  for item in l:go_root_file_list
    let l:result = extend(l:result,split(globpath(item,'*.go')))
  endfor
  let l:gosource={}
  let l:gosource.sink = 'edit'
  let l:gosource.source = l:result
  return l:gosource
endfunction

" Load Env file and return env content
function! initself#load_env()
  let l:env_file = getenv("HOME")."/.env"
  let l:env_dict={}
  if filereadable(l:env_file)
    let l:env_content = readfile(l:env_file)
    for item in l:env_content
      let l:env_dict[split(item,"=")[0]] = split(item,"=")[1]
    endfor
    return l:env_dict
  else
    echo "env file doesn't exist"
  endif
endfunction

" Load database connection from env file
function! initself#load_db_from_env()
  let l:env = initself#load_env()
  let l:dbs={}
  for key in keys(l:env)
    if stridx(key,"DB_CONNECTION_") >= 0
      let l:dbs[split(key,"_")[2]] = l:env[key]
    endif
  endfor
  if empty(l:dbs)
    echo "Env Database config error"
  endif
  return l:dbs
endfunction
