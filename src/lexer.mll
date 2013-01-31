{

  open Lexing
  open Parser

  (* Updates the line counter, which is used in some error messages. *)

  let update_loc lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }

  (* Signals an error at the current token. *)

  let error lexbuf msg =
    Error.error [ Some (lexeme_start_p lexbuf, lexeme_end_p lexbuf) ] msg

  (* A table of keywords. *)

  let table =
    Hashtbl.create 123

  let () =
    List.iter (fun (id, token) ->
      Hashtbl.add table id token
    ) [
	"end", END;
	"type", TYPE;
        "let", LET;
	"fun", FUN;
        "of", OF;
	"in", IN;
        "rec", REC;
        "match", MATCH;
        "with", WITH;
	"exists", EXISTS;
	"program", PROGRAM;
        "try", TRY;
        "raise", RAISE
    ]

  let identify lexbuf id =
    try
      Hashtbl.find table id
    with Not_found ->
      IDENTIFIER id

}

let newline = ('\010' | '\013' | "\013\010")

let whitespace = [ ' ' '\t' ]

let digit = [ '0'-'9' ]

let integer = ( "0x" | "0o" | "0b" )? digit+

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']

let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']

let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

let identifier = lowercase identchar*

let tag = uppercase identchar*

rule main = parse
| newline
    { update_loc lexbuf; main lexbuf }
| whitespace+
    { main lexbuf }
| ":"
    { COLON }
| "="
    { DEFEQ }
| "("
    { LPAR }
| ")"
    { RPAR }
| ","
    { COMMA }
| "|"
    { BAR }
| "->"
    { ARROW }
| "."
    { DOT }
| "'"
    { QUOTE }
| "*"
    { STAR }
| identifier as id
    { identify lexbuf id }
| tag as t
    { CONSTRUCTOR t }
| "(*"
    { comment (lexeme_start_p lexbuf) lexbuf; main lexbuf }
| eof
    { EOF }
| _
    { error lexbuf "Lexical error: unexpected character(s)." }

and comment openingp = parse
| "(*"
    { comment (lexeme_start_p lexbuf) lexbuf; comment openingp lexbuf }
| "*)"
    { () }
| newline
    { update_loc lexbuf; comment openingp lexbuf }
| eof
    { Error.error [ Some (openingp, lexeme_start_p lexbuf) ] "Unterminated comment." }
| _
    { comment openingp lexbuf }

