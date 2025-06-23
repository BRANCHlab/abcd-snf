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
badd +17 12_correlation_explanation.qmd
badd +5 ~/Documents/prash/time/time.csv
badd +687 ~/Documents/abcd-snf-article/manuscript.typ
badd +154 ~/Documents/abcd-snf/abcd/01_baseline_extraction.qmd
badd +1416 ~/Documents/abcd-snf/abcd/04_baseline_mtbi_snf.qmd
badd +1116 ~/Documents/abcd-snf/abcd/02_baseline_preprocessing.qmd
badd +468 helper_functions.R
argglobal
%argdel
$argadd 12_correlation_explanation.qmd
edit helper_functions.R
let s:save_splitbelow = &splitbelow
let s:save_splitright = &splitright
set splitbelow splitright
wincmd _ | wincmd |
split
wincmd _ | wincmd |
split
2wincmd k
wincmd w
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
exe '1resize ' . ((&lines * 14 + 22) / 44)
exe '2resize ' . ((&lines * 14 + 22) / 44)
exe '3resize ' . ((&lines * 13 + 22) / 44)
argglobal
balt 12_correlation_explanation.qmd
setlocal foldmethod=marker
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
let s:l = 468 - ((6 * winheight(0) + 7) / 14)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 468
normal! 022|
wincmd w
argglobal
if bufexists(fnamemodify("~/Documents/abcd-snf/abcd/04_baseline_mtbi_snf.qmd", ":p")) | buffer ~/Documents/abcd-snf/abcd/04_baseline_mtbi_snf.qmd | else | edit ~/Documents/abcd-snf/abcd/04_baseline_mtbi_snf.qmd | endif
if &buftype ==# 'terminal'
  silent file ~/Documents/abcd-snf/abcd/04_baseline_mtbi_snf.qmd
endif
balt ~/Documents/abcd-snf/abcd/02_baseline_preprocessing.qmd
setlocal foldmethod=marker
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
let s:l = 1416 - ((6 * winheight(0) + 7) / 14)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 1416
normal! 0
wincmd w
argglobal
if bufexists(fnamemodify("~/Documents/abcd-snf-article/manuscript.typ", ":p")) | buffer ~/Documents/abcd-snf-article/manuscript.typ | else | edit ~/Documents/abcd-snf-article/manuscript.typ | endif
if &buftype ==# 'terminal'
  silent file ~/Documents/abcd-snf-article/manuscript.typ
endif
balt ~/Documents/prash/time/time.csv
setlocal foldmethod=marker
setlocal foldexpr=0
setlocal foldmarker=\ //\ {{{,\ //\ }}}
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
let s:l = 693 - ((7 * winheight(0) + 6) / 13)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 693
normal! 0
wincmd w
2wincmd w
exe '1resize ' . ((&lines * 14 + 22) / 44)
exe '2resize ' . ((&lines * 14 + 22) / 44)
exe '3resize ' . ((&lines * 13 + 22) / 44)
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
