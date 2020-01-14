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
%type <Exp.exp option> main

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
  | arg_exp { ListCons($1, ListEmpty) }
  | arg_exp SEMICOL { ListCons($1, ListEmpty) }
  | arg_exp SEMICOL list_inner { ListCons($1, $3) }

// 関数の引数になれる式
arg_exp:
  | VAR   { Var $1 }
  | INT   { IntLit $1 }
  | TRUE  { BoolLit true }
  | FALSE { BoolLit false }

  | LPAREN RPAREN { UnitLit }
  
  // 演算子を関数として使う
  | LPAREN PLUS RPAREN { OpAdd } 
  | LPAREN MINUS RPAREN { OpSub } 
  | LPAREN ASTERISK RPAREN { OpMul } 
  | LPAREN SLASH RPAREN { OpDiv } 
  | LPAREN EQUAL RPAREN { OpEq } 
  | LPAREN NOTEQUAL RPAREN { OpEq } 
  | LPAREN GREATER RPAREN { OpGt } 
  | LPAREN LESS RPAREN { OpLt } 
  
  // リスト
  | LBRACKET RBRACKET { ListEmpty }
  | LBRACKET list_inner RBRACKET { $2 }
  
  // 括弧で囲まれた式
  | LPAREN exp RPAREN
    { $2 }
;

// 式
exp:
  | arg_exp { $1 }
  // 関数適用 (e1 e2)
  | exp arg_exp %prec APP { App ($1, $2) }
  // 符号の反転 -e
  | MINUS exp %prec UNARY { Sub (IntLit 0, $2) }
  // e1 + e2
  | exp PLUS exp { Add ($1, $3) }
  // e1 - e2
  | exp MINUS exp { Sub ($1, $3) }
  // e1 * e2
  | exp ASTERISK exp { Mul ($1, $3) }
  // e1 / e2
  | exp SLASH exp { Div ($1, $3) }
  // e1 = e2
  | exp EQUAL exp { Eq ($1, $3) }
  // e1 <> e2
  | exp NOTEQUAL exp { Ne ($1, $3) }
  // e1 < e2
  | exp LESS exp { Lt ($1, $3) }
  // e1 > e2
  | exp GREATER exp { Gt ($1, $3) }
    
  // e1 :: e2
  | exp COLCOL exp { ListCons ($1, $3) }
  // List.hd e
  | HEAD arg_exp %prec APP { ListHead $2 }
  // List.tl e
  | TAIL arg_exp %prec APP { ListTail $2 }
  
  // fun x -> e
  | FUN VAR ARROW exp { Fun ($2, $4) }
  
  // call/cc k -> e
  | CALLCC exp %prec APP { CallCC($2) }
  
  // let x = e1 in e2
  | LET VAR EQUAL exp IN exp %prec LET_IN { Let ($2, $4, $6) }
  // let rec f x = e1 in e2
  | LET REC VAR VAR EQUAL exp IN exp %prec LET_IN { LetRec ($3, $4, $6, $8) }
  
  // if e1 then e2 else e3
  | IF exp THEN exp ELSE exp { If ($2, $4, $6) }
  
  // match e with ...
  | MATCH exp WITH cases_rev { Match ($2, List.rev $4) }

  // Skip
  | exp SEMICOL exp { Skip ($1, $3) }

  // Print
  | PRINT arg_exp %prec APP { Print ($2) }

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
    { LiteralPat(IntLit $1) }
  | TRUE
    { LiteralPat(BoolLit true) }
  | FALSE
    { LiteralPat(BoolLit false) }
  | LBRACKET RBRACKET
    { LiteralPat(ListEmpty) }
  | LBRACKET list_inner RBRACKET
    { LiteralPat($2) }
  | pattern COLCOL pattern
    { ListPat ($1, $3) }
  | LPAREN pattern RPAREN
    { $2 }
;
