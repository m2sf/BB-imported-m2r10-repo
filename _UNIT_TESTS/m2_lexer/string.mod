(** Good String Literals **)


"It's nine o'clock"
'He said "Modula-2" and smiled'

"This is the end of the line\n"
"If I could escape.. \0\n\r\t\\\'\""


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

"A backslash like \ should ALWAYS be followed by a valid escape character"
