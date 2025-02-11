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
badd +28 abcd/01_baseline_extraction.qmd
badd +857 abcd/02_baseline_preprocessing.qmd
badd +414 abcd/06_baseline_mtbi_snf.qmd
badd +1 ~/Documents/metasnf/R/t.R
badd +160 ~/Documents/metasnf/R/snf_config.R
badd +293 ~/Documents/metasnf/R/settings_df.R
badd +1 ~/Documents/metasnf/R/signal.R
badd +1 ~/Documents/metasnf/sandbox.R
badd +67 ~/Documents/metasnf/R/extraction.R
badd +88 ~/Documents/metasnf/R/weights_matrix.R
badd +43 ~/Documents/metasnf/R/rbind.R
badd +1 ~/Documents/metasnf/vignettes/a_complete_example.Rmd
badd +167 ~/Documents/metasnf/R/ext_solutions_df.R
badd +285 ~/Documents/metasnf/R/solutions_df.R
argglobal
%argdel
$argadd abcd/01_baseline_extraction.qmd
edit abcd/06_baseline_mtbi_snf.qmd
let s:save_splitbelow = &splitbelow
let s:save_splitright = &splitright
set splitbelow splitright
wincmd _ | wincmd |
split
1wincmd k
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
exe '1resize ' . ((&lines * 26 + 28) / 57)
exe '2resize ' . ((&lines * 27 + 28) / 57)
argglobal
balt ~/Documents/metasnf/R/t.R
setlocal fdm=marker
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 209 - ((17 * winheight(0) + 13) / 26)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 209
normal! 024|
wincmd w
argglobal
if bufexists(fnamemodify("abcd/06_baseline_mtbi_snf.qmd", ":p")) | buffer abcd/06_baseline_mtbi_snf.qmd | else | edit abcd/06_baseline_mtbi_snf.qmd | endif
if &buftype ==# 'terminal'
  silent file abcd/06_baseline_mtbi_snf.qmd
endif
balt ~/Documents/metasnf/R/t.R
setlocal fdm=marker
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 458 - ((18 * winheight(0) + 13) / 27)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 458
normal! 0
wincmd w
exe '1resize ' . ((&lines * 26 + 28) / 57)
exe '2resize ' . ((&lines * 27 + 28) / 57)
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
