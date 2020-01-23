%{
open Exp
%}

// リテラル
%token <string> VAR  // x, y, abc, ...
%token <int> INT     // 0, 1, 2, ...

// 演算子
%token PLUS     // '+'
%token MINUS    // '-'
%token ASTERISK // '*'
%token SLASH    // '/'
%token EQUAL    // '='
%token NOTEQUAL // '<>'
%token LESS     // '<'
%token GREATER  // '>'

// 括弧類
%token LPAREN   // '('
%token RPAREN   // ')'
%token LBRACKET // '['
%token RBRACKET // ']'

// 区切り記号
%token ARROW    // "->"
%token VBAR     // '|'
%token SEMICOL  // ';'
%token COLCOL   // '::'
%token SEMICOLCOL   // ';;'

// キーワード
%token TRUE     // "true"
%token FALSE    // "false"
%token FUN      // "fun"
%token LET      // "let"
%token REC      // "rec"
%token IN       // "in"
%token IF       // "if"
%token THEN     // "then"
%token ELSE     // "else"
%token MATCH    // "match"
%token WITH     // "with"
%token CALLCC   // "call/cc"
%token HEAD     // "ListHead"
%token TAIL     // "ListTail"
%token PRINT    // "Print"

// 制御記号
%token EOF

// 演算子優先順位 (優先度の低いものほど先)
%nonassoc THEN ELSE
%nonassoc IN WITH ARROW
%nonassoc LET_IN
%left VBAR
%left SEMICOL
%left EQUAL NOTEQUAL GREATER LESS
%left APP
%right COLCOL
%left PLUS MINUS
%left ASTERISK SLASH
%nonassoc UNARY
// 最後にarg_exprの一番左のトークンを並べる
%left VAR INT TRUE FALSE LPAREN LBRACKET

%start main
%type <Exp.ast_node option> main

%%

// 開始記号
main:
  | exp EOF { Some ($1) }
  | exp SEMICOLCOL { Some ($1) }
  | EOF { print_newline (); exit 0 } // exit with 0
  | SEMICOLCOL { None }
;

// リストリテラル
list_inner:
  | arg_exp { new_node @@ ListCons($1, new_node @@ ListEmpty) }
  | arg_exp SEMICOL { new_node @@ ListCons($1, new_node @@ ListEmpty) }
  | arg_exp SEMICOL list_inner { new_node @@ ListCons($1, $3) }

// 関数の引数になれる式
arg_exp:
  | VAR   { new_node @@ Var $1 }
  | INT   { new_node @@ IntLit $1 }
  | TRUE  { new_node @@ BoolLit true }
  | FALSE { new_node @@ BoolLit false }

  | LPAREN RPAREN { new_node @@ UnitLit }
  
  // 演算子を関数として使う
  | LPAREN PLUS RPAREN { new_node @@ OpAdd }
  | LPAREN MINUS RPAREN { new_node @@ OpSub }
  | LPAREN ASTERISK RPAREN { new_node @@ OpMul }
  | LPAREN SLASH RPAREN { new_node @@ OpDiv }
  | LPAREN EQUAL RPAREN { new_node @@ OpEq }
  | LPAREN NOTEQUAL RPAREN { new_node @@ OpEq }
  | LPAREN GREATER RPAREN { new_node @@ OpGt }
  | LPAREN LESS RPAREN { new_node @@ OpLt }
  
  // リスト
  | LBRACKET RBRACKET { new_node @@ ListEmpty }
  | LBRACKET list_inner RBRACKET { $2 }
  
  // 括弧で囲まれた式
  | LPAREN exp RPAREN
    { $2 }
;

// 式
exp:
  | arg_exp { $1 }
  // 関数適用 (e1 e2)
  | exp arg_exp %prec APP { new_node @@ App ($1, $2) }
  // 符号の反転 -e
  | MINUS exp %prec UNARY { new_node @@ Sub (new_node @@ IntLit 0, $2) }
  // e1 + e2
  | exp PLUS exp { new_node @@ Add ($1, $3) }
  // e1 - e2
  | exp MINUS exp { new_node @@ Sub ($1, $3) }
  // e1 * e2
  | exp ASTERISK exp { new_node @@ Mul ($1, $3) }
  // e1 / e2
  | exp SLASH exp { new_node @@ Div ($1, $3) }
  // e1 = e2
  | exp EQUAL exp { new_node @@ Eq ($1, $3) }
  // e1 <> e2
  | exp NOTEQUAL exp { new_node @@ Ne ($1, $3) }
  // e1 < e2
  | exp LESS exp { new_node @@ Lt ($1, $3) }
  // e1 > e2
  | exp GREATER exp { new_node @@ Gt ($1, $3) }
    
  // e1 :: e2
  | exp COLCOL exp { new_node @@ ListCons ($1, $3) }
  // List.hd e
  | HEAD arg_exp %prec APP { new_node @@ ListHead $2 }
  // List.tl e
  | TAIL arg_exp %prec APP { new_node @@ ListTail $2 }
  
  // fun x -> e
  | FUN VAR ARROW exp { new_node @@ Fun ($2, $4) }
  
  // call/cc k -> e
  | CALLCC exp %prec APP { new_node @@ CallCC($2) }
  
  // let x = e1 in e2
  | LET VAR EQUAL exp IN exp %prec LET_IN { new_node @@ Let ($2, $4, $6) }
  // let rec f x = e1 in e2
  | LET REC VAR VAR EQUAL exp IN exp %prec LET_IN { new_node @@ LetRec ($3, $4, $6, $8) }
  
  // if e1 then e2 else e3
  | IF exp THEN exp ELSE exp { new_node @@ If ($2, $4, $6) }
  
  // match e with ...
  | MATCH exp WITH cases_rev { new_node @@ Match ($2, List.rev $4) }

  // Skip
  | exp SEMICOL exp { new_node @@ Skip ($1, $3) }

  // Print
  | PRINT arg_exp %prec APP { new_node @@ Print ($2) }

  | error
    { 
      let message =
        Printf.sprintf 
          "parse error near characters %d-%d"
          (Parsing.symbol_start ())
          (Parsing.symbol_end ())
      in
      failwith message
    }
;

// match文のcaseの列
// 注: yaccでは左再帰のほうがスタック消費量が少ない。
cases_rev:
  | pattern ARROW exp
    { [($1, $3)] }
  | VBAR pattern ARROW exp
    { [($2, $4)] }
  | cases_rev VBAR pattern ARROW exp
    { ($3, $5) :: $1 }
;

// パターン
pattern:
  | VAR
    { WildcardPat($1) }
  | INT
    { LiteralPat(new_node @@ IntLit $1) }
  | TRUE
    { LiteralPat(new_node @@ BoolLit true) }
  | FALSE
    { LiteralPat(new_node @@ BoolLit false) }
  | LBRACKET RBRACKET
    { LiteralPat(new_node @@ ListEmpty) }
  | LBRACKET list_inner RBRACKET
    { LiteralPat($2) }
  | pattern COLCOL pattern
    { ListPat ($1, $3) }
  | LPAREN pattern RPAREN
    { $2 }
;
