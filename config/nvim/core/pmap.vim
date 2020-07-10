if dein#tap('vim-buffet')
  nnoremap  ]b :<C-u>bp<CR>
  nnoremap  [b :<C-u>bn<CR>
  nnoremap <silent> <Leader>bc :Bonly<CR>
  nnoremap <silent> <Leader>bx :Bw<CR>
  nmap <leader>1 <Plug>BuffetSwitch(1)
  nmap <leader>2 <Plug>BuffetSwitch(2)
  nmap <leader>3 <Plug>BuffetSwitch(3)
  nmap <leader>4 <Plug>BuffetSwitch(4)
  nmap <leader>5 <Plug>BuffetSwitch(5)
  nmap <leader>6 <Plug>BuffetSwitch(6)
  nmap <leader>7 <Plug>BuffetSwitch(7)
  nmap <leader>8 <Plug>BuffetSwitch(8)
  nmap <leader>9 <Plug>BuffetSwitch(9)
  nmap <leader>0 <Plug>BuffetSwitch(10)
endif

if dein#tap('defx.nvim')
  nnoremap <silent> <Leader>e
    \ :<C-u>Defx -resume -toggle -buffer-name=tab`tabpagenr()`<CR>
  nnoremap <silent> <Leader>F
    \ :<C-u>Defx -resume -buffer-name=tab`tabpagenr()` -search=`expand('%:p')`<CR>
endif

if dein#tap('markdown-preview.nvim')
  nnoremap <silent> <Leader>om  :<C-u>MarkdownPreview<CR>
endif

if dein#tap('vim-floaterm')
  nnoremap <silent> <Leader>t :<C-u>FloatermToggle<CR>
  nnoremap <silent> <Leader>g :<C-u>FloatermNew height=0.7 width=0.8 lazygit<CR>
endif

if dein#tap('vim-dadbod-ui')
  nnoremap <silent> <Leader>od :DBUIToggle<CR>
endif

if dein#tap('dash.vim')
  nnoremap <silent><localleader>d :Dash<CR>
endif

if dein#tap('coc-clap')
  " Show all diagnostics
  nnoremap <silent> <Leader>ce  :Clap coc_diagnostics<CR>
  " Manage extensions
  nnoremap <silent> <Leader>;   :Clap coc_extensions<CR>
  " Show commands
  nnoremap <silent> <Leader>,   :Clap coc_commands<CR>
  " Search workspace symbols
  nnoremap <silent> <Leader>cs  :Clap coc_symbols<CR>
  nnoremap <silent> <Leader>cS  :Clap coc_services<CR>
  nnoremap <silent> <leader>ct  :Clap coc_outline<CR>
endif

if dein#tap('coc.nvim')
  " Remap for do codeAction of selected region
  function! s:cocActionsOpenFromSelected(type) abort
      execute 'CocCommand actions.open ' . a:type
  endfunction
  xmap <silent> <Leader>a :<C-u>execute 'CocCommand actions.open ' . visualmode()<CR>
  nmap <silent> <Leader>a :<C-u>set operatorfunc=<SID>cocActionsOpenFromSelected<CR>g@
  " Do default action for next item.
  nmap <silent> [a  :<C-u>CocNext<CR>
  " Do default action for previous item.
  nmap <silent> ]a  :<C-u>CocPrev<CR>
  " Use `[e` and `]e` for navigate diagnostics
  nmap <silent> ]e <Plug>(coc-diagnostic-prev)
  nmap <silent> [e <Plug>(coc-diagnostic-next)
  " Remap for rename current word
  nmap <Leader>cn <Plug>(coc-rename)
  " Remap for format selected region
  vmap <Leader>cf  <Plug>(coc-format-selected)
  nmap <Leader>cf  <Plug>(coc-format-selected)
  " Fix autofix problem of current line
  nmap <Leader>cF  <Plug>(coc-fix-current)
  " Remap keys for gotos
  nmap <silent> gd :<C-u>call initself#definition_other_window()<CR>
  nmap <silent> gy <Plug>(coc-type-definition)
  nmap <silent> <Leader>ci <Plug>(coc-implementation)
  nmap <silent> gr <Plug>(coc-references)
  " Use K for show documentation in float window
  nnoremap <silent> K :call CocActionAsync('doHover')<CR>
  " use <c-space> for trigger completion.
  inoremap <silent><expr> <c-space> coc#refresh()
  nmap ]g <Plug>(coc-git-prevchunk)
  nmap [g <Plug>(coc-git-nextchunk)
  " show chunk diff at current position
  nmap <Leader>gi <Plug>(coc-git-chunkinfo)
  " show commit contains current position
  nmap <Leader>gm <Plug>(coc-git-commit)
  " float window scroll
  nnoremap <expr><C-f> coc#util#has_float() ? coc#util#float_scroll(1) : "\<C-f>"
  nnoremap <expr><C-b> coc#util#has_float() ? coc#util#float_scroll(0) : "\<C-b>"
  " Use <TAB> for selections ranges.
  " NOTE: Requires 'textDocument/selectionRange' support from the language server.
  " coc-tsserver, coc-python are the examples of servers that support it.
  nmap <silent> <TAB> <Plug>(coc-range-select)
  xmap <silent> <TAB> <Plug>(coc-range-select)
  " Add `:OR` command for organize imports of the current buffer.
  command! -nargs=0 OR  :call CocAction('runCommand', 'editor.action.organizeImport')
  nnoremap <silent> <Leader>co :<C-u>OR<CR>
  " multiple cursors
  nmap <silent><M-s> <Plug>(coc-cursors-position)
  nmap <expr> <silent><M-d> initself#select_current_word()
  xmap <silent><M-d> <Plug>(coc-cursors-range)
  " use normal command like `<Leader>xi(`
  nmap <silent><M-c>  <Plug>(coc-cursors-operator)

  " Use `:Format` for format current buffer
  command! -nargs=0 Format :call CocAction('format')

  nnoremap  <Leader>fz :<C-u>CocSearch -w<Space>
  " " coc-explorer
  " noremap <silent> <Leader>e :execute 'CocCommand explorer' .
  "     \ ' --toggle' .
  "     \ ' --sources=file+'<CR>
  " Introduce function text object
  " NOTE: Requires 'textDocument.documentSymbol' support from the language server.
  xmap if <Plug>(coc-funcobj-i)
  xmap af <Plug>(coc-funcobj-a)
  omap if <Plug>(coc-funcobj-i)
  omap af <Plug>(coc-funcobj-a)
  nmap gcj :execute 'CocCommand docthis.documentThis'<CR>
endif

if dein#tap('vim-clap')
  nnoremap <silent> <Leader>tc :<C-u>Clap colors<CR>
  nnoremap <silent> <Leader>bb :<C-u>Clap buffers<CR>
  nnoremap <silent> <Leader>fa :<C-u>Clap grep2<CR>
  nnoremap <silent> <Leader>fb :<C-u>Clap marks<CR>
  "like emacs counsel-find-file
  nnoremap <silent> <C-x><C-f> :<C-u>Clap filer<CR>
  nnoremap <silent> <Leader>ff :<C-u>Clap files ++finder=rg --ignore --hidden --files<cr>
  nnoremap <silent> <Leader>fg :<C-u>Clap gfiles<CR>
  nnoremap <silent> <Leader>fw :<C-u>Clap grep ++query=<cword><cr>
  nnoremap <silent> <Leader>fh :<C-u>Clap history<CR>
  nnoremap <silent> <Leader>fW :<C-u>Clap windows<CR>
  nnoremap <silent> <Leader>fl :<C-u>Clap loclist<CR>
  nnoremap <silent> <Leader>fu :<C-u>Clap git_diff_files<CR>
  nnoremap <silent> <Leader>fv :<C-u>Clap grep ++query=@visual<CR>
  nnoremap <silent> <Leader>oc :<C-u>Clap personalconf<CR>
  nnoremap <silent> <LocalLeader>g :<C-u>Clap gosource<CR>
endif

if dein#tap('accelerated-jk')
  nmap <silent>j <Plug>(accelerated_jk_gj)
  nmap <silent>k <Plug>(accelerated_jk_gk)
endif

if dein#tap('caw.vim')
	function! InitCaw() abort
		if ! (&l:modifiable && &buftype ==# '')
			silent! nunmap <buffer> gc
			silent! xunmap <buffer> gc
			silent! nunmap <buffer> gcc
			silent! xunmap <buffer> gcc
		else
			nmap <buffer> gc <Plug>(caw:prefix)
			xmap <buffer> gc <Plug>(caw:prefix)
			nmap <buffer> gcc <Plug>(caw:hatpos:toggle)
			xmap <buffer> gcc <Plug>(caw:hatpos:toggle)
		endif
	endfunction
	autocmd FileType * call InitCaw()
	call InitCaw()
endif

if dein#tap('vim-smoothie')
  nnoremap <silent> <C-f> :<C-U>call smoothie#forwards()<CR>
  nnoremap <silent> <C-b> :<C-U>call smoothie#backwards()<CR>
  nnoremap <silent> <C-d> :<C-U>call smoothie#downwards()<CR>
  nnoremap <silent> <C-u> :<C-U>call smoothie#upwards()<CR>
endif

if dein#tap('committia.vim')
  let g:committia_hooks = {}
  function! g:committia_hooks.edit_open(info)
    imap <buffer><C-d> <Plug>(committia-scroll-diff-down-half)
    imap <buffer><C-u> <Plug>(committia-scroll-diff-up-half)
    setlocal winminheight=1 winheight=1
    resize 10
    startinsert
  endfunction
endif

if dein#tap('vim-quickrun')
  nnoremap <silent> <Leader>cr :QuickRun<CR>
endif

if dein#tap('vista.vim')
  nnoremap <silent> <Leader>i :<C-u>Vista!!<CR>
endif

if dein#tap('vim-easymotion')
  nmap gsj <Plug>(easymotion-w)
  nmap gsk <Plug>(easymotion-b)
  nmap gsf <Plug>(easymotion-overwin-f)
  nmap gss <Plug>(easymotion-overwin-f2)
endif

if dein#tap('vim-mundo')
    nnoremap <silent> <Leader>m :MundoToggle<CR>
endif

if dein#tap('vim-smartchr')
  inoremap <expr> , smartchr#one_of(',', ',')
  autocmd FileType go inoremap <buffer><expr> ;
          \ smartchr#loop(':=',';')
  autocmd FileType go inoremap <buffer> <expr> .
        \ smartchr#loop('.', '<-', '->','...')
  autocmd FileType typescript,typescriptreact inoremap <buffer><expr>;
          \ smartchr#loop(':',';')
endif

if dein#tap('iron.nvim')
  nmap <silent> <Leader>rr :<C-u>IronRepl<CR><Esc>
  nmap <silent> <Leader>rf :IronWatchCurrentFile<CR>
  nmap <silent> <Leader>rq :call initself#exit_iron()<CR>
  nmap <silent> <Leader>rl <Plug>(iron-send-line)
  vmap <silent> <Leader>rl <Plug>(iron-visual-send)
  nmap <silent> <Leader>rp <Plug>(iron-repeat-cmd)
  nmap <silent> <Leader>rc <Plug>(iron-clear)
  nmap <silent> <Leader>r<CR>  <Plug>(iron-cr)
  nmap <silent> <Leader>r<Esc> <Plug>(iron-interrupt)
endif

if dein#tap('vim-sandwich')
  nmap <silent> sa <Plug>(operator-sandwich-add)
  xmap <silent> sa <Plug>(operator-sandwich-add)
  omap <silent> sa <Plug>(operator-sandwich-g@)
  nmap <silent> sd <Plug>(operator-sandwich-delete)<Plug>(operator-sandwich-release-count)<Plug>(textobj-sandwich-query-a)
  xmap <silent> sd <Plug>(operator-sandwich-delete)
  nmap <silent> sr <Plug>(operator-sandwich-replace)<Plug>(operator-sandwich-release-count)<Plug>(textobj-sandwich-query-a)  xmap <silent> sr <Plug>(operator-sandwich-replace)
  nmap <silent> sdb <Plug>(operator-sandwich-delete)<Plug>(operator-sandwich-release-count)<Plug>(textobj-sandwich-auto-a)
  nmap <silent> srb <Plug>(operator-sandwich-replace)<Plug>(operator-sandwich-release-count)<Plug>(textobj-sandwich-auto-a)
  omap ib <Plug>(textobj-sandwich-auto-i)
  xmap ib <Plug>(textobj-sandwich-auto-i)
  omap ab <Plug>(textobj-sandwich-auto-a)
  xmap ab <Plug>(textobj-sandwich-auto-a)
  omap is <Plug>(textobj-sandwich-query-i)
  xmap is <Plug>(textobj-sandwich-query-i)
  omap as <Plug>(textobj-sandwich-query-a)
  xmap as <Plug>(textobj-sandwich-query-a)
endif

if dein#tap('vim-niceblock')
  silent! xmap I  <Plug>(niceblock-I)
  silent! xmap gI <Plug>(niceblock-gI)
  silent! xmap A  <Plug>(niceblock-A)
endif

if dein#tap('vim-expand-region')
  xmap v <Plug>(expand_region_expand)
  xmap V <Plug>(expand_region_shrink)
endif

if dein#tap('dsf.vim')
  nmap dsf <Plug>DsfDelete
  nmap csf <Plug>DsfChange
endif

if dein#tap('splitjoin.vim')
  let g:splitjoin_join_mapping = ''
  let g:splitjoin_split_mapping = ''
  nmap sj :SplitjoinJoin<CR>
  nmap sk :SplitjoinSplit<CR>
endif

if dein#tap('vim-operator-replace')
  xmap p <Plug>(operator-replace)
endif

if dein#tap('vim-textobj-multiblock')
  omap <silent> ab <Plug>(textobj-multiblock-a)
  omap <silent> ib <Plug>(textobj-multiblock-i)
  xmap <silent> ab <Plug>(textobj-multiblock-a)
  xmap <silent> ib <Plug>(textobj-multiblock-i)
endif

if dein#tap('vim-textobj-function')
  omap <silent> af <Plug>(textobj-function-a)
  omap <silent> if <Plug>(textobj-function-i)
  xmap <silent> af <Plug>(textobj-function-a)
  xmap <silent> if <Plug>(textobj-function-i)
endif
