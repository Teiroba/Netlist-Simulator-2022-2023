{
open Lexing
open Rom_parser
exception Eof
}

rule token = parse
    '\n'
      { new_line lexbuf; token lexbuf }
    | [' ' '\t']
      { token lexbuf }
    |((['0'-'1']+) as key) ' '* ':' ' '*  ((['0'-'1']+) as data) 
        {KEY_DATA (key,data)}
    |eof {EOF}
