let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/Documents/research/abcd
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
let s:shortmess_save = &shortmess
if &shortmess =~ 'A'
  set shortmess=aoOA
else
  set shortmess=aoO
endif
badd +1489 04_baseline_mtbi_snf.qmd
badd +354 ~/Documents/abcd-snf-article/abcd_snf_article.tex
badd +6 ~/Documents/prash/time/time.csv
badd +54 05_baseline_matching.qmd
badd +1 01_baseline_extraction.qmd
badd +543 02_preimputation_analysis.qmd
badd +747 03_baseline_preprocessing.qmd
argglobal
%argdel
$argadd 04_baseline_mtbi_snf.qmd
edit 04_baseline_mtbi_snf.qmd
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
exe 'vert 1resize ' . ((&columns * 112 + 113) / 226)
exe 'vert 2resize ' . ((&columns * 113 + 113) / 226)
argglobal
balt ~/Documents/abcd-snf-article/abcd_snf_article.tex
setlocal fdm=marker
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 1192 - ((37 * winheight(0) + 33) / 66)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 1192
normal! 0
lcd ~/Documents/research/abcd
wincmd w
argglobal
if bufexists(fnamemodify("~/Documents/research/abcd/05_baseline_matching.qmd", ":p")) | buffer ~/Documents/research/abcd/05_baseline_matching.qmd | else | edit ~/Documents/research/abcd/05_baseline_matching.qmd | endif
if &buftype ==# 'terminal'
  silent file ~/Documents/research/abcd/05_baseline_matching.qmd
endif
balt ~/Documents/prash/time/time.csv
setlocal fdm=marker
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 44 - ((33 * winheight(0) + 33) / 66)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 44
normal! 0
wincmd w
2wincmd w
exe 'vert 1resize ' . ((&columns * 112 + 113) / 226)
exe 'vert 2resize ' . ((&columns * 113 + 113) / 226)
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
