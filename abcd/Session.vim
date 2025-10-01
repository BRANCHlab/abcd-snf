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
badd +171 ~/Documents/abcd-snf-article/manuscript.typ
badd +1317 04_baseline_mtbi_snf.qmd
badd +1912 ~/Documents/abcd-snf/abcd/03_baseline_descriptive.qmd
badd +73 ~/Documents/abcd-snf/abcd/062_characterization.qmd
badd +141 ~/Documents/abcd-snf/abcd/085_characterization.qmd
badd +12 ~/Documents/abcd-snf/abcd/11_sample_size_check.qmd
badd +209 ~/Documents/prash/journal/prash.md
argglobal
%argdel
tabnew +setlocal\ bufhidden=wipe
tabrewind
edit 04_baseline_mtbi_snf.qmd
argglobal
balt ~/Documents/abcd-snf-article/manuscript.typ
setlocal foldmethod=marker
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
let s:l = 1315 - ((20 * winheight(0) + 22) / 45)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 1315
normal! 050|
tabnext
edit ~/Documents/abcd-snf-article/manuscript.typ
let s:save_splitbelow = &splitbelow
let s:save_splitright = &splitright
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
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
exe 'vert 1resize ' . ((&columns * 158 + 159) / 318)
exe '2resize ' . ((&lines * 22 + 23) / 47)
exe 'vert 2resize ' . ((&columns * 159 + 159) / 318)
exe '3resize ' . ((&lines * 22 + 23) / 47)
exe 'vert 3resize ' . ((&columns * 159 + 159) / 318)
argglobal
setlocal foldmethod=marker
setlocal foldexpr=0
setlocal foldmarker=\ //\ {{{,\ //\ }}}
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
585
sil! normal! zo
816
sil! normal! zo
926
sil! normal! zo
let s:l = 934 - ((112 * winheight(0) + 22) / 45)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 934
normal! 065|
wincmd w
argglobal
if bufexists(fnamemodify("~/Documents/abcd-snf-article/manuscript.typ", ":p")) | buffer ~/Documents/abcd-snf-article/manuscript.typ | else | edit ~/Documents/abcd-snf-article/manuscript.typ | endif
if &buftype ==# 'terminal'
  silent file ~/Documents/abcd-snf-article/manuscript.typ
endif
balt 04_baseline_mtbi_snf.qmd
setlocal foldmethod=marker
setlocal foldexpr=0
setlocal foldmarker=\ //\ {{{,\ //\ }}}
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
585
sil! normal! zo
816
sil! normal! zo
926
sil! normal! zo
let s:l = 174 - ((6 * winheight(0) + 11) / 22)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 174
normal! 056|
wincmd w
argglobal
if bufexists(fnamemodify("~/Documents/abcd-snf-article/manuscript.typ", ":p")) | buffer ~/Documents/abcd-snf-article/manuscript.typ | else | edit ~/Documents/abcd-snf-article/manuscript.typ | endif
if &buftype ==# 'terminal'
  silent file ~/Documents/abcd-snf-article/manuscript.typ
endif
balt ~/Documents/abcd-snf/abcd/062_characterization.qmd
setlocal foldmethod=marker
setlocal foldexpr=0
setlocal foldmarker=\ //\ {{{,\ //\ }}}
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
585
sil! normal! zo
612
sil! normal! zo
816
sil! normal! zo
926
sil! normal! zo
let s:l = 622 - ((8 * winheight(0) + 11) / 22)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 622
normal! 08|
wincmd w
2wincmd w
exe 'vert 1resize ' . ((&columns * 158 + 159) / 318)
exe '2resize ' . ((&lines * 22 + 23) / 47)
exe 'vert 2resize ' . ((&columns * 159 + 159) / 318)
exe '3resize ' . ((&lines * 22 + 23) / 47)
exe 'vert 3resize ' . ((&columns * 159 + 159) / 318)
tabnext 2
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
