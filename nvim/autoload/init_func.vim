" vim:foldmethod=marker:foldlevel=0
" s:attr(group, attr, ...)             | get group attribute {{{
function! s:attr(group, attr, ...) abort
  call assert_inrange(0, 1, a:0)
  if a:0 > 0
    let a = synIDattr(synIDtrans(hlID(a:group)), a:attr, a:1)
  else
    let a = synIDattr(synIDtrans(hlID(a:group)), a:attr)
  endif
  return empty(a) || a ==# '-1' ? 'NONE' :  a
endfunction
" }}}
" s:hi(g, gf, gb, ctf, ctb)            | highlighting helper {{{
function! s:hi(group, guifg, guibg, ctermfg, ctermbg) abort
  exec printf('hi %s guifg=%s guibg=%s ctermfg=%s ctermbg=%s',
       \      a:group, a:guifg, a:guibg, a:ctermfg, a:ctermbg)
endfunction
" }}}
" s:fg(group, mode)                    | get foreground of highlighting group {{{
function! s:fg(group, mode) abort
  return s:attr(a:group, s:attr(a:group, 'reverse', a:mode) ? 'bg' : 'fg', a:mode)
endfunction
" }}}
" s:bg(group, mode)                    | get background of highlighting group {{{
function! s:bg(group, mode) abort
  return s:attr(a:group, s:attr(a:group, 'reverse', a:mode) ? 'fg' : 'bg', a:mode)
endfunction
" }}}
" s:color_distance(hexcode1, hexcode2) | calculate color distance {{{
function! s:color_distance(hexcode1, hexcode2) abort
  call assert_match('^\v#\x{6}$', a:hexcode1)
  call assert_match('^\v#\x{6}$', a:hexcode2)
  let dr = str2nr(a:hexcode1[1:2], 16) - str2nr(a:hexcode2[1:2], 16)
  let dg = str2nr(a:hexcode1[3:4], 16) - str2nr(a:hexcode2[3:4], 16)
  let db = str2nr(a:hexcode1[5:6], 16) - str2nr(a:hexcode2[5:6], 16)
  return dr*dr + dg*dg + db*db
endfunction
" }}}
" s:farthest_color(hexcode, hexcodes)  | return index of farthest color {{{
function! s:farthest_color(hexcode, hexcodes) abort
  let c = -1
  let i = 0
  let d = 0
  for hc in a:hexcodes
    let cd = s:color_distance(a:hexcode, hc)
    if cd > d
      let c = i
      let d = cd
    endif
    let i += 1
  endfor
  return c
endfunction
" }}}
