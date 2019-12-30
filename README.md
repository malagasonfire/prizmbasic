# prizmbasic
VIM syntax highlight for Casio Prizm Basic Calculators

Instructions for VIM instalation:
 
https://vim.fandom.com/wiki/Creating_your_own_syntax_files#Install_the_syntax_file



Make Vim recognize the filetype:

https://vim.fandom.com/wiki/Creating_your_own_syntax_files#Make_Vim_recognize_the_filetype


```bash
au BufRead,BufNewFile *.stc set filetype=prizmbasic
```


Open VIM and set syntax on command editor

```bash
:set syntax=prizmbasic

```
