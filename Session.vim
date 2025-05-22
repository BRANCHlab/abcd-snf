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
badd +207 abcd/01_baseline_extraction.qmd
badd +57 abcd/02_baseline_preprocessing.qmd
badd +1 abcd/03_longitudinal_extraction.qmd
badd +1 abcd/04_longitudinal_preprocessing.qmd
badd +1 abcd/05_descriptive_analyses.qmd
badd +1 abcd/062_characterization.qmd
badd +1 abcd/06_and_a_half_characterization.qmd
badd +1 abcd/06_baseline_mtbi_snf.qmd
badd +1 abcd/07_subtype_classifier.qmd
badd +13 abcd/08_longitudinal_analyses.qmd
badd +1 abcd/09_baseline_matching.qmd
badd +214 ~/Documents/abcd-prepost-cbcl-rsi/ses_imputation.Rmd
badd +26 ~/Documents/abcd-prepost-cbcl-rsi/README.md
badd +40 ~/Documents/abcd-prepost-cbcl-rsi/combat_harmonization.Rmd
badd +1 ~/Documents/prash/journal/prash.md
badd +2 ~/Documents/prash/time/time.csv
argglobal
%argdel
$argadd abcd/01_baseline_extraction.qmd
$argadd abcd/02_baseline_preprocessing.qmd
$argadd abcd/03_longitudinal_extraction.qmd
$argadd abcd/04_longitudinal_preprocessing.qmd
$argadd abcd/05_descriptive_analyses.qmd
$argadd abcd/062_characterization.qmd
$argadd abcd/06_and_a_half_characterization.qmd
$argadd abcd/06_baseline_mtbi_snf.qmd
$argadd abcd/07_subtype_classifier.qmd
$argadd abcd/08_longitudinal_analyses.qmd
$argadd abcd/09_baseline_matching.qmd
edit abcd/01_baseline_extraction.qmd
let s:save_splitbelow = &splitbelow
let s:save_splitright = &splitright
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
let &splitbelow = s:save_splitbelow
let &splitright = s:save_splitright
wincmd t
let s:save_winminheight = &winminheight
let s:save_winminwidth = &winminwidth
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe 'vert 1resize ' . ((&columns * 119 + 119) / 239)
exe 'vert 2resize ' . ((&columns * 119 + 119) / 239)
argglobal
balt ~/Documents/prash/time/time.csv
setlocal foldmethod=marker
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
let s:l = 207 - ((42 * winheight(0) + 27) / 55)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 207
normal! 0
wincmd w
argglobal
if bufexists(fnamemodify("abcd/02_baseline_preprocessing.qmd", ":p")) | buffer abcd/02_baseline_preprocessing.qmd | else | edit abcd/02_baseline_preprocessing.qmd | endif
if &buftype ==# 'terminal'
  silent file abcd/02_baseline_preprocessing.qmd
endif
balt abcd/01_baseline_extraction.qmd
setlocal foldmethod=marker
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
let s:l = 210 - ((8 * winheight(0) + 27) / 55)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 210
normal! 0
wincmd w
exe 'vert 1resize ' . ((&columns * 119 + 119) / 239)
exe 'vert 2resize ' . ((&columns * 119 + 119) / 239)
tabnext 1
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0 && getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20
let &shortmess = s:shortmess_save
let &winminheight = s:save_winminheight
let &winminwidth = s:save_winminwidth
let s:sx = expand("<sfile>:p:r")."x.vim"
if filereadable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &g:so = s:so_save | let &g:siso = s:siso_save
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
