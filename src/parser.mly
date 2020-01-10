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
%nonassoc IF THEN IN ELSE ARROW WITH STMT_DELIM
%left VBAR
%left LIST_DELIM
%left EQUAL NOTEQUAL GREATER LESS
%right COLCOL
%left PLUS MINUS
%left ASTERISK SLASH
%nonassoc UNARY
// 最後にarg_exprの一番左のトークンを並べる
%left VAR INT TRUE FALSE LPAREN LBRACKET

%start main
%type <Exp.exp> main

%%

// 開始記号
main:
  | exp EOF { $1 }
;

// リストリテラル
list_inner:
  | exp { ListCons($1, ListEmpty) }
  | exp SEMICOL %prec LIST_DELIM { ListCons($1, ListEmpty) }
  | exp SEMICOL %prec LIST_DELIM list_inner { ListCons($1, $3) }

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
  | exp arg_exp { App ($1, $2) }
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
  | HEAD arg_exp { ListHead $2 }
  // List.tl e
  | TAIL arg_exp { ListTail $2 }
  
  // fun x -> e
  | FUN VAR ARROW exp { Fun ($2, $4) }
  
  // call/cc k -> e
  | CALLCC exp { CallCC($2) }
  
  // let x = e1 in e2
  | LET VAR EQUAL exp IN exp { Let ($2, $4, $6) }
  // let rec f x = e1 in e2
  | LET REC VAR VAR EQUAL exp IN exp { LetRec ($3, $4, $6, $8) }
  
  // if e1 then e2 else e3
  | IF exp THEN exp ELSE exp { If ($2, $4, $6) }
  
  // match e with ...
  | MATCH exp WITH cases_rev { Match ($2, List.rev $4) }
  
  // Skip
  | exp SEMICOL %prec STMT_DELIM exp { Skip ($1, $3) }

  // Print
  | PRINT arg_exp { Print ($2) }

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
