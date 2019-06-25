call denite#custom#option('_', {
		\ 'cached_filter': v:true,
		\ 'cursor_shape': v:true,
		\ 'cursor_wrap': v:true,
		\ 'highlight_filter_background': 'DeniteFilter',
		\ 'highlight_matched_char': 'Underlined',
		\ 'matchers': 'matcher/fruzzy',
		\ 'prompt': 'λ ',
		\ 'split': 'floating',
		\ 'start_filter': v:true,
		\ 'statusline': v:false,
		\ })
function! s:denite_detect_size() abort
    let s:denite_winheight = 20
    let s:denite_winrow = &lines > s:denite_winheight ? (&lines - s:denite_winheight) / 2 : 0
    let s:denite_winwidth = &columns > 240 ? &columns / 2 : 120
    let s:denite_wincol = &columns > s:denite_winwidth ? (&columns - s:denite_winwidth) / 2 : 0
    call denite#custom#option('_', {
         \ 'wincol': s:denite_wincol,
         \ 'winheight': s:denite_winheight,
         \ 'winrow': s:denite_winrow,
         \ 'winwidth': s:denite_winwidth,
         \ })
  endfunction
   augroup denite-detect-size
    autocmd!
    autocmd VimResized * call <SID>denite_detect_size()
  augroup END
  call s:denite_detect_size()


call denite#custom#option('search', { 'start_filter': 0, 'no_empty': 1 })
call denite#custom#option('list', { 'start_filter': 0 })
call denite#custom#option('jump', { 'start_filter': 0 })
call denite#custom#option('git', { 'start_filter': 0 })
call denite#custom#option('mpc', { 'winheight': 20 })


" MATCHERS
" Default is 'matcher/fuzzy'
call denite#custom#source('tag', 'matchers', ['matcher/substring'])
call denite#custom#source('file/rec', 'matchers', ['matcher/fruzzy'])

if has('nvim') && &runtimepath =~# '\/cpsm'
	call denite#custom#source(
		\ 'buffer,file_mru,file/old,file/rec,grep,mpc,line,neoyank',
		\ 'matchers', ['matcher/cpsm', 'matcher/fuzzy'])
endif


" CONVERTERS
" Default is none
call denite#custom#source(
	\ 'buffer,file_mru,file/old,file/rec,directory/rec,directory_mru',
	\ 'converters', ['devicons_denite_converter','converter_relative_word'])

" FIND and GREP COMMANDS
if executable('ag')
	" The Silver Searcher
	call denite#custom#var('file/rec', 'command',
		\ ['ag', '-U', '--hidden', '--follow', '--nocolor', '--nogroup', '-g', ''])

	" Setup ignore patterns in your .agignore file!
	" https://github.com/ggreer/the_silver_searcher/wiki/Advanced-Usage

	call denite#custom#var('grep', 'command', ['ag'])
	call denite#custom#var('grep', 'recursive_opts', [])
	call denite#custom#var('grep', 'pattern_opt', [])
	call denite#custom#var('grep', 'separator', ['--'])
	call denite#custom#var('grep', 'final_opts', [])
	call denite#custom#var('grep', 'default_opts',
		\ [ '--skip-vcs-ignores', '--vimgrep', '--smart-case', '--hidden' ])

elseif executable('ack')
	" Ack command
	call denite#custom#var('grep', 'command', ['ack'])
	call denite#custom#var('grep', 'recursive_opts', [])
	call denite#custom#var('grep', 'pattern_opt', ['--match'])
	call denite#custom#var('grep', 'separator', ['--'])
	call denite#custom#var('grep', 'final_opts', [])
	call denite#custom#var('grep', 'default_opts',
			\ ['--ackrc', $HOME.'/.config/ackrc', '-H',
			\ '--nopager', '--nocolor', '--nogroup', '--column'])

elseif executable('rg')
	" Ripgrep
  call denite#custom#var('file/rec', 'command',
        \ ['rg', '--files', '--glob', '!.git'])
  call denite#custom#var('grep', 'command', ['rg', '--threads', '1'])
  call denite#custom#var('grep', 'recursive_opts', [])
  call denite#custom#var('grep', 'final_opts', [])
  call denite#custom#var('grep', 'separator', ['--'])
  call denite#custom#var('grep', 'default_opts',
        \ ['-i', '--vimgrep', '--no-heading'])
endif


" KEY MAPPINGS
autocmd FileType denite call s:denite_settings()
function! s:denite_settings() abort
	highlight! link CursorLine Visual
	nnoremap <silent><buffer><expr> <CR> denite#do_map('do_action')
	nnoremap <silent><buffer><expr> i    denite#do_map('open_filter_buffer')
	nnoremap <silent><buffer><expr> d    denite#do_map('do_action', 'delete')
	nnoremap <silent><buffer><expr> p    denite#do_map('do_action', 'preview')
	nnoremap <silent><buffer><expr> st   denite#do_map('do_action', 'tabopen')
	nnoremap <silent><buffer><expr> sv   denite#do_map('do_action', 'vsplit')
	nnoremap <silent><buffer><expr> si   denite#do_map('do_action', 'split')
	nnoremap <silent><buffer><expr> '    denite#do_map('quick_move')
	nnoremap <silent><buffer><expr> q    denite#do_map('quit')
	nnoremap <silent><buffer><expr> r    denite#do_map('redraw')
	nnoremap <silent><buffer><expr> yy   denite#do_map('do_action', 'yank')
	nnoremap <silent><buffer><expr> <Esc>   denite#do_map('quit')
	nnoremap <silent><buffer><expr> <C-u>   denite#do_map('restore_sources')
	nnoremap <silent><buffer><expr> <C-f>   denite#do_map('do_action', 'defx')
	nnoremap <silent><buffer><expr> <C-x>   denite#do_map('choose_action')
	nnoremap <silent><buffer><expr><nowait> <Space> denite#do_map('toggle_select').'j'
endfunction

autocmd FileType denite-filter call s:denite_filter_settings()
function! s:denite_filter_settings() abort
	nnoremap <silent><buffer><expr> <Esc>  denite#do_map('quit')
	" inoremap <silent><buffer><expr> <Esc>  denite#do_map('quit')
	nnoremap <silent><buffer><expr> q      denite#do_map('quit')
	imap <silent><buffer> <C-c> <Plug>(denite_filter_quit)
	"inoremap <silent><buffer><expr> <C-c>  denite#do_map('quit')
	nnoremap <silent><buffer><expr> <C-c>  denite#do_map('quit')
	inoremap <silent><buffer>       kk     <Esc><C-w>p
	nnoremap <silent><buffer>       kk     <C-w>p
	inoremap <silent><buffer>       jj     <Esc><C-w>p
	nnoremap <silent><buffer>       jj     <C-w>p
endfunction

" vim: set ts=3 sw=2 tw=80 noet :
