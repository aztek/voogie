syn keyword Keyword     if else assert forall exists return
syn match   kyNumber    /\(+\|-\)\?\(0\|[1-9][0-9]*\)/
syn match   Type        /\(int\|bool\)\(\[\]\)*/
syn keyword Boolean     true false
syn match   punctuation "(\|)\|{\|}\|,\|;\|\[\|\]"
syn match   kyOperator  "!\|?\|:\|&&\|||\|=>\|<=\|>=\|<\|>\|+\|-\|=="
syn match   Comment     /\/\/.*$/
syn region  Comment     start="\/\*" end="\*\/"

hi def kyOperator  ctermfg=DarkCyan
hi def punctuation ctermfg=59
hi def kyNumber    ctermfg=magenta

setlocal cindent
setlocal expandtab
setlocal shiftwidth=2
setlocal softtabstop=2
