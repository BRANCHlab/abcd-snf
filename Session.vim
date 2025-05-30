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
badd +1304 abcd/06_baseline_mtbi_snf.qmd
badd +55 abcd/02_baseline_preprocessing.qmd
badd +1 ~/Documents/metasnf/R/plot.R
badd +1 ~/Documents/metasnf/R/ext_solutions_df.R
badd +7 ~/Documents/prash/time/time.csv
badd +431 abcd/helper_functions.R
argglobal
%argdel
$argadd abcd/06_baseline_mtbi_snf.qmd
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
exe '1resize ' . ((&lines * 33 + 34) / 68)
exe '2resize ' . ((&lines * 32 + 34) / 68)
argglobal
balt abcd/helper_functions.R
setlocal foldmethod=marker
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
let s:l = 1304 - ((22 * winheight(0) + 16) / 33)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 1304
normal! 044|
wincmd w
argglobal
if bufexists(fnamemodify("abcd/helper_functions.R", ":p")) | buffer abcd/helper_functions.R | else | edit abcd/helper_functions.R | endif
if &buftype ==# 'terminal'
  silent file abcd/helper_functions.R
endif
balt abcd/06_baseline_mtbi_snf.qmd
setlocal foldmethod=marker
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
let s:l = 385 - ((25 * winheight(0) + 16) / 32)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 385
normal! 029|
wincmd w
exe '1resize ' . ((&lines * 33 + 34) / 68)
exe '2resize ' . ((&lines * 32 + 34) / 68)
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
