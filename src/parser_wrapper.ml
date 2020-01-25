let parse lexbuf =
  Parser_state.reset_line_number ();
  Parser.main Lexer.token lexbuf