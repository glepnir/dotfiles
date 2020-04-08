
" If colorscheme doesn't support opacity
let g:PaperColor_Theme_Options = {
 \   'theme': {
 \     'default': {
 \       'transparent_background': 1
 \     }
 \   }
 \ }

" Dash keymap
if dein#tap('dash.vim')
    nnoremap <silent><localleader>d :Dash<CR>
endif
" which_key_localmap
let g:which_key_localmap.d="open doc on Dash.app"

" startify mid area position according the screen szie
let g:startify_padding_left = 50

" Spaceline
" let g:spaceline_seperate_style= 'slant'
" let g:spaceline_seperate_style= 'slant-fade'
" let g:spaceline_seperate_style= 'arrow-fade'
" let g:spaceline_seperate_style= 'curve'
let g:spaceline_seperate_style= 'slant-cons'

