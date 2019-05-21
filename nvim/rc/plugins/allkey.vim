"Plugin key settings

if dein#tap('denite.nvim')
        nnoremap <silent><localLeader>dt  :Denite todo<CR>
        nnoremap <silent><localLeader>da  :TodoAdd
        nnoremap <silent><localLeader>dd  :Denite todo:done<CR>
endif

if dein#tap('coc.nvim')
        " Using CocList
        " Show all diagnostics
        nnoremap <silent> <localleader>ca  :<C-u>CocList diagnostics<cr>
        " Manage extensions
        nnoremap <silent> <localleader>ce  :<C-u>CocList extensions<cr>
        " Show commands
        nnoremap <silent> <localleader>cc  :<C-u>CocList commands<cr>
        " Find symbol of current document
        nnoremap <silent> <localleader>co  :<C-u>CocList outline<cr>
        " Search workspace symbols
        nnoremap <silent> <localleader>cs  :<C-u>CocList -I symbols<cr>
        " Do default action for next item.
        nnoremap <silent> <localleader>cj  :<C-u>CocNext<CR>
        " Do default action for previous item.
        nnoremap <silent> <localleader>ck  :<C-u>CocPrev<CR>
        " Resume latest coc list
        nnoremap <silent> <localleader>cr  :<C-u>CocListResume<CR>
        " Use `[c` and `]c` for navigate diagnostics
        nmap <silent> ]c <Plug>(coc-diagnostic-prev)
        nmap <silent> [c <Plug>(coc-diagnostic-next)

        " Remap for format selected region
        vmap <leader>cf  <Plug>(coc-format-selected)
        nmap <leader>cf  <Plug>(coc-format-selected)
        " Remap keys for gotos
        nmap <silent> gd <Plug>(coc-definition)
        nmap <silent> gy <Plug>(coc-type-definition)
        nmap <silent> gi <Plug>(coc-implementation)
        nmap <silent> gr <Plug>(coc-references)
        " Use K for show documentation in preview window
        nnoremap <silent> K :call <sid>show_documentation()<cr>
        " use <c-space> for trigger completion.
        inoremap <silent><expr> <c-space> coc#refresh()

endif

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

if dein#tap('fzf.vim')
        nnoremap <silent> <leader>fc :Colors<CR>
        nnoremap <silent> <localleader>b :Buffers<CR>
        nnoremap <silent> <leader>ff :call Fzf_dev()<CR>
        nnoremap <silent> <leader>fr :Rg<CR>
        nnoremap <silent> <leader>fw :Rg <C-R><C-W><CR>
        nnoremap <silent> <leader>fgc  :Commits<CR>
        nnoremap <silent> <leader>fbc :BCommits<CR>
endif

if dein#tap('vim-easy-align')
    " Start interactive EasyAlign in visual mode (e.g. vipga)
    xmap ga <Plug>(EasyAlign)
    " Start interactive EasyAlign for a motion/text object (e.g. gaip)
    nmap ga <Plug>(EasyAlign)
endif


if dein#tap('vim-go')
     autocmd MyAutoCmd FileType go
                    \  nmap <Leader>gov <Plug>(go-def-vertical)
                    \ | nmap <Leader>goi <Plug>(go-info)
                    \ | nmap <Leader>god <Plug>(go-doc)
                    \ | nmap <leader>gor <Plug>(go-run)
                    \ | nmap <leader>gob <Plug>(go-build)
                    \ | nmap <leader>got <Plug>(go-test)
                    \ | nmap <leader>goc <Plug>(go-coverage)
                    \ | nmap <Leader>gr  <Plug>(go-rename)
endif

if dein#tap('vim-easygit')
	nnoremap <silent> <localleader>ga :Gadd<CR>
	nnoremap <silent> <localleader>gd :Gdiff<CR>
	nnoremap <silent> <localleader>gD :Gdiffoff<CR>
	nnoremap <silent> <localleader>gc :Gcommit<CR>
	nnoremap <silent> <localleader>gb :Gblame<CR>
	nnoremap <silent> <localleader>gB :Gbrowse<CR>
	nnoremap <silent> <localleader>gS :Gstatus<CR>
	nnoremap <silent> <localleader>gp :Gpush<CR>
endif

if dein#tap('git-messenger.vim')
    nmap <localleader>gm  <Plug>(git-messenger)
endif

if dein#tap('vim-mundo')
    nnoremap <silent> <leader>m :MundoToggle<CR>
endif

if dein#tap('comfortable-motion.vim')
    nnoremap <silent> <C-d> :call comfortable_motion#flick(g:comfortable_motion_impulse_multiplier * winheight(0) * 2)<CR>
    nnoremap <silent> <C-u> :call comfortable_motion#flick(g:comfortable_motion_impulse_multiplier * winheight(0) * -2)<CR>
    nnoremap <silent> <C-f> :call comfortable_motion#flick(g:comfortable_motion_impulse_multiplier * winheight(0) * 4)<CR>
    nnoremap <silent> <C-b> :call comfortable_motion#flick(g:comfortable_motion_impulse_multiplier * winheight(0) * -4)<CR>
endif

if dein#tap('goyo.vim')
	nnoremap <Leader>G :Goyo<CR>
endif

if dein#tap('defx.nvim')
        nnoremap <silent> <Leader>e
                \ :<C-u>Defx -resume -toggle -buffer-name=tab`tabpagenr()`<CR>
endif

if dein#tap('vim-startify')
    nnoremap <silent> <leader>s :Startify<CR>
endif

if dein#tap('vim-quickrun')
    nnoremap <silent> <localleader>r :QuickRun<CR>
endif

if dein#tap('dash.vim')
        nnoremap <silent><leader>d :Dash<CR>
endif

if dein#tap('vim-expand-region')
        xmap v <Plug>(expand_region_expand)
        xmap V <Plug>(expand_region_shrink)
endif

if dein#tap('splitjoin.vim')
        let g:splitjoin_join_mapping = ''
        let g:splitjoin_split_mapping = ''
        nmap sj :SplitjoinJoin<CR>
        nmap sk :SplitjoinSplit<CR>
endif

if dein#tap('vista.vim')
        nnoremap <silent><localleader>v :Vista!!<CR>
endif

if dein#tap('tagbar')
        nnoremap <silent><localleader>t :TagbarToggle<CR>
endif

if dein#tap('ale')
        nmap [a <Plug>(ale_next_wrap)
        nmap ]a <Plug>(ale_previous_wrap)
endif

if dein#tap('vim-easymotion')
        nmap <Leader><Leader>w <Plug>(easymotion-w)
	    nmap <Leader><Leader>f <Plug>(easymotion-f)
	    nmap <Leader><Leader>b <Plug>(easymotion-b)
endif

if dein#tap('vim-which-key')
		nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
		nnoremap <silent> <localleader> :<c-u>WhichKey  ';'<CR>
		nnoremap <silent>[              :<c-u>WhichKey  '['<CR>
		nnoremap <silent>]              :<c-u>WhichKey  ']'<CR>
endif

if dein#tap('bps/vim-textobj-python')
        xmap aF <Plug>(textobj-python-function-a)
        omap aF <Plug>(textobj-python-function-a)
        xmap iF <Plug>(textobj-python-function-i)
        omap iF <Plug>(textobj-python-function-i)
endif

"if dein#tap('nerdtree')
        ""nerdtree
        "nnoremap <silent><leader>e :NERDTreeToggle <CR>
        ""nnoremap <leader>f :NERDTreeFind <CR>
"endif
