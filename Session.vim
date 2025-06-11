let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/Documents/abcd-snf
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
let s:shortmess_save = &shortmess
if &shortmess =~ 'A'
  set shortmess=aoOA
else
  set shortmess=aoO
endif
badd +647 abcd/05_longitudinal_preprocessing.qmd
badd +247 abcd/02_baseline_preprocessing.qmd
badd +498 abcd/04_longitudinal_extraction.qmd
badd +3 ~/Documents/prash/time/time.csv
badd +293 abcd/helper_functions.R
badd +39 ~/Documents/abcdutils/R/assign_scanner.R
badd +2 ~/Documents/abcd-snf/abcd/06_descriptive_analyses.qmd
badd +255 ~/Documents/abcd-snf-article/manuscript.typ
argglobal
%argdel
$argadd abcd/05_longitudinal_preprocessing.qmd
edit ~/Documents/abcd-snf-article/manuscript.typ
argglobal
balt ~/Documents/abcd-snf/abcd/06_descriptive_analyses.qmd
setlocal foldmethod=marker
setlocal foldexpr=0
setlocal foldmarker=\ //\ {{{,\ //\ }}}
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
let s:l = 190 - ((13 * winheight(0) + 22) / 45)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 190
normal! 0
tabnext 1
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0 && getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20
let &shortmess = s:shortmess_save
let s:sx = expand("<sfile>:p:r")."x.vim"
if filereadable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &g:so = s:so_save | let &g:siso = s:siso_save
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
