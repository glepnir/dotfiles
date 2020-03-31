
" Dash keymap
if dein#tap('dash.vim')
    nnoremap <silent><localleader>d :Dash<CR>
endif
" which_key_localmap
let g:which_key_localmap.d="open doc on Dash.app"

" startify according the screen szie
let g:startify_padding_left = 80

" fzf-fzf_preview_use_dev_icons
let g:fzf_preview_use_dev_icons = 1

" Spaceline
let g:spaceline_seperate_style= 'slant'
