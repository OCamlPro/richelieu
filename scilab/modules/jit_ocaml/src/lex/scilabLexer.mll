{

  open Lexing
  open ScilabParser

  let last_token = ref EOF

  let return_token tok =
    last_token := tok;
    tok

  let is_transposable () = match !last_token with
    | ID _ | RBRACK | RBRACE | VARINT _ | VARFLOAT _
    | RPAREN | NUM _ | BOOLTRUE | BOOLFALSE -> true
    | _ -> false

  let is_EOL () = match !last_token with
    | EOL -> true
    | _ -> false

 let newline_lex lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

  let end_cmt lexbuf =
    lexbuf.lex_curr_pos <- lexbuf.lex_start_pos;
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
      pos_cnum = lexbuf.lex_start_p.pos_cnum;
    }

  (** xd-y -> xe-y
      xD-y -> xe-y **)
  let convert_scientific_notation str =
    let regexp = Str.regexp "['d''D']" in
    Str.global_replace regexp "e" str
    

  let print_pos pos =
    Printf.printf "%i %i %i" pos.pos_lnum pos.pos_bol pos.pos_cnum

  let print_lexbuf lexbuf =
    Printf.printf "st :"; print_pos lexbuf.lex_start_p;
    Printf.printf "; curr :"; print_pos lexbuf.lex_curr_p;
    Printf.printf "; st_pos :%i" lexbuf.lex_start_pos;
    Printf.printf "; curr_pos :%i \n" lexbuf.lex_curr_pos

  let str_cmt = ref ""

  let str = ref ""

}

let spaces    = [' ' '\t']


let newline   = ('\010' | '\013' | "\013\010")
let blankline = spaces+ newline
let empty     = spaces | [','';']
let emptyline = newline empty+ newline
let next      = ".." | "..."

let integer   = ['0'-'9']+
let number    = ['0'-'9']+(".")['0'-'9']*
let little    = (".")['0'-'9']+

let mantise   = little | integer | number
let exposant  = ['+''-']? integer
let floating  =  mantise ['d''e''D''e'] exposant

let utf2  = ['\xC2'-'\xDF']['\x80'-'\xBF']

let utf31 = ['\xE0']['\xA0'-'\xBF']['\x80'-'\xBF']
let utf32 = ['\xE1'-'\xEC']['\x80'-'\xBF']['\x80'-'\xBF']
let utf33 = ['\xED']['\x80'-'\x9F']['\x80'-'\xBF']
let utf34 = ['\xEE'-'\xEF']['\x80'-'\xBF']['\x80'-'\xBF']
let utf41 = ['\xF0']['\x90'-'\xBF']['\x80'-'\xBF']['\x80'-'\xBF']
let utf42 = ['\xF1'-'\xF3']['\x80'-'\xBF']['\x80'-'\xBF']['\x80'-'\xBF']
let utf43 = ['\xF4']['\x80'-'\x8F']['\x80'-'\xBF']['\x80'-'\xBF']

let utf3 = utf31 | utf32 | utf33 | utf34
let utf4 = utf41 | utf42 | utf43

let utf  =  utf2 | utf3 | utf4

let id1  = ['a'-'z''A'-'Z''_''%''#''?''$'] | utf
let id2  = ['a'-'z''A'-'Z''_''0'-'9''#''?''$'] | utf
let id   = id1 id2*
(* let id   = ['a'-'z''A'-'Z''_''%''#''?''$']['a'-'z''A'-'Z''_''0'-'9''#''?''$'] * *)

let boolnot    = "@" | "~"
let booltrue   = "%t" | "%T"
let boolfalse  = "%f" | "%F"
let booland    = "&"
let boolandand = "&&"
let boolor     = "|"
let booloror   = "||"

let lbrack = "["
let rbrack = "]"

let lparen = "("
let rparen = ")"

let lbrace = "{"
let rbrace = "}"

let dollar = "$"

let semicolon = ";"
let comma     = ","
let colon     = ":"

let startlinecomment  = "//"
let startblockcomment = "/*"
let endblockcomment   = "*/"

let dquote = "\""
let quote  = "'"

let dot        = "."
let dotquote   = ".'"
let dottimes   = ".*"
let dotrdivide = "./"
let dotldivide = ".\\"
let dotpower   = ".^" | ".**"

let plus    = "+"
let minus   = "-"
let rdivide = "/"
let ldivide = "\\"
let times   = "*"
let power   = "^" | "**"

let equal        = "=="
let notequal     = "~=" | "@=" | "<>"
let lowerthan    = "<"
let greaterthan	 = ">"
let lowerequal   = "<="
let greaterequal = ">="

let krontimes   = ".*."
let kronrdivide = "./."
let kronldivide = ".\\."

let controltimes   = "*." [^'0'-'9']
let controlrdivide = "/." [^'0'-'9']
let controlldivide = "\\." [^'0'-'9']

let assign = "="


rule token = parse
  | spaces                       { token lexbuf }
  | blankline                    { newline_lex lexbuf;
                                   if (is_EOL ()) then token lexbuf else return_token EOL }
  | newline                      { newline_lex lexbuf;
                                   if (is_EOL ()) then token lexbuf else return_token EOL }
  | emptyline                    { newline_lex lexbuf;
                                   newline_lex lexbuf;
                                   if (is_EOL ()) then token lexbuf else return_token EOL }
  | startlinecomment             { str_cmt := ""; comment lexbuf }
  | dquote                       { str := ""; doublestr lexbuf }
  | quote                        { if (is_transposable ())
                                   then return_token QUOTE 
                                   else begin str := ""; simplestr lexbuf end}
  | "if"                         { return_token IF }
  | "then"                       { return_token THEN }
  | "else"                       { return_token ELSE }
  | "elseif"                     { return_token ELSEIF }
  | "end"                        { return_token END }
  | "select"                     { return_token SELECT }
  | "switch"                     { return_token SWITCH }
  | "otherwise"                  { return_token OTHERWISE }
  | "case"                       { return_token CASE }
  | "while"                      { return_token WHILE }
  | "do"                         { return_token DO }
  | "try"                        { return_token TRY }
  | "catch"                      { return_token CATCH }
  | "return"                     { return_token RETURN }
  | "break"                      { return_token BREAK }
  | "continue"                   { return_token CONTINUE }
  | "="                          { return_token ASSIGN }
  | "for"                        { return_token FOR }
  | "hidden"                     { return_token HIDDEN }
  | "function"                   { return_token FUNCTION }
  | "#function"                  { return_token HIDDENFUNCTION }
  | "endfunction"                { return_token ENDFUNCTION }
  | dot                          { return_token DOT }
  | dotquote                     { return_token DOTQUOTE }
  | dottimes                     { return_token DOTTIMES }
  | dotpower                     { return_token DOTPOWER }
  | dotldivide                   { return_token DOTLDIVIDE }
  | dotrdivide                   { return_token DOTRDIVIDE }
  | krontimes                    { return_token KRONTIMES }
  | controltimes                 { return_token CONTROLTIMES }
  | next newline                 { newline_lex lexbuf; token lexbuf }
  | next                         { return_token EOL }
  | plus                         { return_token PLUS }
  | minus                        { return_token MINUS }
  | rdivide                      { return_token RDIVIDE }
  | ldivide                      { return_token LDIVIDE }
  | times                        { return_token TIMES }
  | power                        { return_token POWER }
  | equal                        { return_token EQ }
  | notequal                     { return_token NE }
  | lowerthan                    { return_token LT }
  | greaterthan                  { return_token GT }
  | lowerequal                   { return_token LE }
  | greaterequal                 { return_token GE }
  | comma                        { return_token COMMA }
  | semicolon                    { return_token SEMI }
  | colon                        { return_token COLON }
  | integer as inum              { let num = float_of_string inum in
                                   (* Printf.printf "varint[%f]" num; *)
                                   return_token (VARINT num) }
  | number as nnum               { let num = float_of_string nnum in
                                   return_token (NUM num) }
  | little as lnum               { let num = float_of_string lnum in
                                   return_token (NUM num) }
  | floating as float            { let f =(float_of_string (convert_scientific_notation float)) in
                                   NUM f }
  | lparen                       { return_token LPAREN }
  | rparen                       { return_token RPAREN }
  | lbrace                       { return_token LBRACE }
  | rbrace                       { return_token RBRACE }
  | lbrack                       { return_token LBRACK }
  | rbrack                       { return_token RBRACK }
  | dollar                       { return_token DOLLAR }
  | boolnot                      { return_token NOT }
  | booltrue                     { return_token BOOLTRUE }
  | boolfalse                    { return_token BOOLFALSE }
  | booland                      { return_token AND }
  | boolandand                   { return_token ANDAND }
  | boolor                       { return_token OR }
  | booloror                     { return_token OROR }
  | id as str                    { (* Printf.printf "ID[%s]" str; *) return_token (ID str) }
  | eof                          { return_token EOF }
  | _ as c                       { Printf.printf "Lexing error : Unknow character \'%c\'" c;exit 1}

and comment = parse
  | newline                      { newline_lex lexbuf;  end_cmt lexbuf; return_token (COMMENT !str_cmt)}
  | eof                          { return_token (COMMENT !str_cmt) }
  | _ as c                       { str_cmt := !str_cmt^(String.make 1 c); comment lexbuf }

and doublestr = parse
  | dquote                       { return_token (STR !str) }
  | dquote dquote                { str := !str^"\""; doublestr lexbuf }
  | dquote quote                 { str := !str^"\'"; doublestr lexbuf }
  | quote dquote                 { str := !str^"\""; doublestr lexbuf }
  | quote quote                  { str := !str^"\'"; doublestr lexbuf }
  | quote                        { failwith "Error : Heterogeneous string detected, starting with \" and ending with \'." }
  | next newline                 { newline_lex lexbuf; doublestr lexbuf }
  | newline                      { failwith "Error : unexpected newline in a string." }
  | eof                          { failwith "Error : unexpected end of file in a string." }
  | _ as c                       { str := !str^(String.make 1 c); doublestr lexbuf }

and simplestr = parse
  | quote                        { return_token (STR !str)}
  | dquote dquote                { str := !str^"\""; simplestr lexbuf }
  | dquote quote                 { str := !str^"\'"; simplestr lexbuf }
  | quote dquote                 { str := !str^"\""; simplestr lexbuf }
  | quote quote                  { str := !str^"\'"; simplestr lexbuf }
  | dquote                       { failwith "Error : Heterogeneous string detected, starting with \' and ending with \"." }
  | next newline                 { newline_lex lexbuf; simplestr lexbuf }
  | newline                      { failwith "Error : unexpected newline in a string." }
  | eof                          { failwith "Error : unexpected end of file in a string." }
  | _ as c                       { str := !str^(String.make 1 c); simplestr lexbuf }

(* and matrix = parse *)
(*   | spaces+ *)
(*       { Printf.printf " "; *)
(*         matrix lexbuf} *)
(*   | integer as inum *)
(*       { let num = int_of_string inum in *)
(*         Printf.printf "%d" num; *)
(*         matrix lexbuf} *)
(*   | number as nnum *)
(*       { let num = float_of_string nnum in *)
(*         Printf.printf "%f" num; *)
(*         matrix lexbuf} *)
(*   | little as lnum *)
(*       { let num = float_of_string lnum in *)
(*         Printf.printf "%f" num; *)
(*         matrix lexbuf} *)
(*   | rbrack *)
(*       {token lexbuf} *)


















