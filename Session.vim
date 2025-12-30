let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/Documents/abcd-snf/abcd
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
let s:shortmess_save = &shortmess
if &shortmess =~ 'A'
  set shortmess=aoOA
else
  set shortmess=aoO
endif
badd +73 01_baseline_extraction.qmd
badd +1 02_baseline_preprocessing.qmd
badd +1 03_baseline_descriptive.qmd
badd +1 04_baseline_mtbi_snf.qmd
badd +1 05_longitudinal_extraction.qmd
badd +1 062_characterization.qmd
badd +1 06_longitudinal_preprocessing.qmd
badd +1 07_longitudinal_descriptive.qmd
badd +1 085_characterization.qmd
badd +1 08_subtype_classifier.qmd
badd +1 09_longitudinal_analyses.qmd
badd +0 ~/Documents/abcd-snf-article/manuscript.typ
argglobal
%argdel
$argadd 01_baseline_extraction.qmd
$argadd 02_baseline_preprocessing.qmd
$argadd 03_baseline_descriptive.qmd
$argadd 04_baseline_mtbi_snf.qmd
$argadd 05_longitudinal_extraction.qmd
$argadd 062_characterization.qmd
$argadd 06_longitudinal_preprocessing.qmd
$argadd 07_longitudinal_descriptive.qmd
$argadd 085_characterization.qmd
$argadd 08_subtype_classifier.qmd
$argadd 09_longitudinal_analyses.qmd
edit ~/Documents/abcd-snf-article/manuscript.typ
argglobal
if bufexists(fnamemodify("~/Documents/abcd-snf-article/manuscript.typ", ":p")) | buffer ~/Documents/abcd-snf-article/manuscript.typ | else | edit ~/Documents/abcd-snf-article/manuscript.typ | endif
if &buftype ==# 'terminal'
  silent file ~/Documents/abcd-snf-article/manuscript.typ
endif
balt 01_baseline_extraction.qmd
setlocal foldmethod=marker
setlocal foldexpr=0
setlocal foldmarker=\ //\ {{{,\ //\ }}}
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
let s:l = 1 - ((0 * winheight(0) + 33) / 67)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 1
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
nohlsearch
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
