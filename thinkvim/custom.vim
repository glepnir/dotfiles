
" If colorscheme doesn't support opacity
hi Normal guibg=NONE ctermbg=NONE

" Dash keymap
if dein#tap('dash.vim')
    nnoremap <silent><localleader>d :Dash<CR>
endif
" which_key_localmap
let g:which_key_localmap.d="open doc on Dash.app"

" startify mid area position according the screen szie
let g:startify_padding_left = 40

" fzf-fzf_preview
let g:fzf_preview_use_dev_icons = 1
let g:fzf_preview_filelist_postprocess_command = 'gxargs -d "\n" exa --color=always'

" Spaceline
" let g:spaceline_seperate_style= 'slant'
" let g:spaceline_seperate_style= 'slant-fade'
" let g:spaceline_seperate_style= 'arrow-fade'
" let g:spaceline_seperate_style= 'curve'
let g:spaceline_seperate_style= 'slant-cons'
