let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/Documents/metasnf/R
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
let s:shortmess_save = &shortmess
if &shortmess =~ 'A'
  set shortmess=aoOA
else
  set shortmess=aoO
endif
badd +583 ~/Documents/abcd-snf/abcd/06_baseline_mtbi_snf.qmd
badd +426 ~/Documents/metasnf/vignettes/a_complete_example.Rmd
badd +1 t.R
badd +74 get_representative_solutions.R
badd +9 extraction.R
badd +30 sim_mats_list.R
badd +1 dplyr.R
badd +462 print.R
badd +34 ext_solutions_df.R
badd +36 ~/Documents/prash/journal/prash.md
badd +10 ~/Documents/metasnf/sandbox.R
badd +6 ~/Documents/prash/time/time.csv
badd +73 nmi.R
badd +447 batch_snf.R
badd +209 clust_fns_list.R
badd +41 nclust_estimation.R
argglobal
%argdel
$argadd ~/Documents/abcd-snf/abcd/06_baseline_mtbi_snf.qmd
edit ~/Documents/abcd-snf/abcd/06_baseline_mtbi_snf.qmd
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
exe 'vert 1resize ' . ((&columns * 159 + 159) / 318)
exe 'vert 2resize ' . ((&columns * 158 + 159) / 318)
argglobal
balt ~/Documents/metasnf/vignettes/a_complete_example.Rmd
setlocal fdm=marker
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 589 - ((42 * winheight(0) + 35) / 70)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 589
normal! 022|
lcd ~/Documents/abcd-snf
wincmd w
argglobal
if bufexists(fnamemodify("~/Documents/metasnf/R/nclust_estimation.R", ":p")) | buffer ~/Documents/metasnf/R/nclust_estimation.R | else | edit ~/Documents/metasnf/R/nclust_estimation.R | endif
if &buftype ==# 'terminal'
  silent file ~/Documents/metasnf/R/nclust_estimation.R
endif
balt ~/Documents/metasnf/R/clust_fns_list.R
setlocal fdm=marker
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 63 - ((50 * winheight(0) + 35) / 70)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 63
normal! 014|
wincmd w
exe 'vert 1resize ' . ((&columns * 159 + 159) / 318)
exe 'vert 2resize ' . ((&columns * 158 + 159) / 318)
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
