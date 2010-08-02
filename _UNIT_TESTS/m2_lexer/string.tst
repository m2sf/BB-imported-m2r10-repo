(** Good String Literals **)


"It's nine o'clock"
'He said "Modula-2" and smiled'

"This is the end of the line\n"
"If I could escape.. \0\n\r\t\\\'\""
"A lone backslash like \ should get translated to \\"


(** Bad String Literals **)


"
"

'
'

" one
two "

' one
two '

" escape \
escape \""

' escape \
escape \''
