# miniocaml

[主専攻実験「関数プログラミング」](http://logic.cs.tsukuba.ac.jp/jikken/)の成果物

## 依存
- [LLVM9](http://releases.llvm.org/)
- dune
- OCaml libraries
  - llvm
  - ctypes
- [libgc](https://www.hboehm.info/gc/)

## コンパイル・実行
```
$ make
$ ./miniocaml  # REPL
$ ./miniocaml examples/fib1.mml  # compiler
```

## 言語機能

### 型
- int
- bool
- unit
- list
- function

### リテラル
- int: `1`, `-2`
- bool: `true`, `false`
- unit: `()`
- list: `[]`, `[1; 2; 3]`

### 四則演算
```
miniocaml # 1 + 1;;
- : int = 2
miniocaml # 2 * 3 / 6;;
- : int = 1
miniocaml # 1 - 2 * 3;;
- : int = -5
miniocaml # (1 - 2) * 3;;
- : int = -3
```

### if, 条件演算
```
miniocaml # if 1 = 2 then 3 else 4;;
- : int = 4
miniocaml # if 1 <> 2 then 3 else 4;;
- : int = 3
miniocaml # if 1 < 2 then 3 else 4;;
- : int = 3
miniocaml # if 1 > 2 then 3 else 4;;
- : int = 4
```

### fun
引数は1つのみ
```
miniocaml # (fun x -> x * 2) 123;;
- : int = 246
miniocaml # (fun x -> fun y -> x + y) 1 2;;
- : int = 3
```

### let
```
miniocaml # let x = 4 in x * x;;
- : int = 16
miniocaml # let sq = fun x -> x * x in sq 10;;
- : int = 100
```

### let rec
```
miniocaml # let rec sum n =
  if n = 0 then 0
  else n + sum (n - 1)
in sum 10;;
- : int = 55
```

### リスト
```
miniocaml # let l = [3; 2; 1] in 4::l;;
- : (int) list = [4; 3; 2; 1]
miniocaml # let l = [3; 2; 1] in ListHead l;;
- : int = 3
miniocaml # let l = [3; 2; 1] in ListTail l;;
- : (int) list = [2; 1]
```

### パターンマッチ
```
miniocaml # match 1 with 0 -> 0 | x -> x * 2;;
- : int = 2
miniocaml # match 2 with 0 -> 0 | x -> x * 2;;
- : int = 4
```

```
miniocaml # match [1; 2; 3] with [] -> [] | x::xs -> xs;;
- : (int) list = [2; 3]
miniocaml # match [1; 2; 3] with [] -> 0 | x::xs -> x;;
- : int = 1
miniocaml # match [1; 2; 3] with x::xs -> x;;
pattern matching is not exhausted
```

### Print
```
miniocaml # Print 123;;
123
- : unit = ()
miniocaml # Print true;;
true
- : unit = ()
```

### ReadInt
```
miniocaml # let x = ReadInt in x * 2;;
10
- : int = 20
```

### call/cc (インタプリタのみ)
```
miniocaml # 1 + 2 + 3 + (call/cc fun k -> 4 + (k 100));;
- : int = 106
```

## 複雑な例
```
miniocaml #
let l =
  let rec gen_list n =
    if n = 0 then
      []
    else
      n::(gen_list (n - 1))
  in gen_list 10
in
let rec iter f = fun l ->
  match l with
  | [] -> ()
  | x::xs -> (f x); iter f xs
in
let p = iter (fun x -> Print (x)) in
  p l;
  let rec sum l =
    match l with
    | [] -> 0
    | h::t -> h + (sum t)
  in Print (sum l)
;;
10
9
8
7
6
5
4
3
2
1
55
- : unit = ()
```


## 資料の範囲からの拡張としてやったこと・やっていないこと

### やったこと
- LLVMによるネイティブコードへのコンパイル
  - Boehm GCへの依存
- REPLの追加
- エラーメッセージに行番号を表示
- 言語機能の追加
  - リスト
  - パターンマッチ
  - Unit型の追加
  - Print文の追加
  - 文の追加(2項とって、1項目は評価だけして2項目の結果を返す)
  - パターンマッチの網羅性判定
  - 入力文
  - コメントの追加の追加

### やっていないこと
- 「型変数を持つ型」がつく式のコンパイル
- call/ccのコンパイル
- デバッグ情報の追加(LLVM)
- 最適化
  - クロージャ
  - 各種データ構造の効率化
  - GC
- 機能追加
  - letで関数定義(任意個の引数を取れるように)
  - ADTとそのパターンマッチ
  - タプル
  - 相互再帰
  - 例外機構