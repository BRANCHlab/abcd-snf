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
badd +821 ~/Documents/abcd-snf-article/manuscript.typ
badd +290 ~/Documents/prash/journal/prash.md
badd +1442 10_baseline_matching.qmd
badd +1290 09_longitudinal_analyses.qmd
badd +716 04_baseline_mtbi_snf.qmd
badd +73 ~/Documents/abcd-snf/abcd/08_subtype_classifier.qmd
badd +374 ~/Documents/abcd-snf/abcd/11_sample_size_check.qmd
badd +682 12_correlation_explanation.qmd
argglobal
%argdel
edit ~/Documents/abcd-snf-article/manuscript.typ
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
exe '1resize ' . ((&lines * 32 + 34) / 68)
exe '2resize ' . ((&lines * 33 + 34) / 68)
argglobal
balt 12_correlation_explanation.qmd
setlocal foldmethod=marker
setlocal foldexpr=0
setlocal foldmarker=\ //\ {{{,\ //\ }}}
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
692
sil! normal! zo
809
sil! normal! zo
1277
sil! normal! zo
1288
sil! normal! zo
let s:l = 255 - ((14 * winheight(0) + 16) / 32)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 255
normal! 0
wincmd w
argglobal
if bufexists(fnamemodify("09_longitudinal_analyses.qmd", ":p")) | buffer 09_longitudinal_analyses.qmd | else | edit 09_longitudinal_analyses.qmd | endif
if &buftype ==# 'terminal'
  silent file 09_longitudinal_analyses.qmd
endif
balt 04_baseline_mtbi_snf.qmd
setlocal foldmethod=marker
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
56
sil! normal! zo
471
sil! normal! zo
let s:l = 1290 - ((18 * winheight(0) + 16) / 33)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 1290
normal! 0
wincmd w
2wincmd w
exe '1resize ' . ((&lines * 32 + 34) / 68)
exe '2resize ' . ((&lines * 33 + 34) / 68)
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
