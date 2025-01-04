# Lang

| things | do |
| --- | --- |
| \* | one or more|
| x -> | x is|
| a \| b | a or b|
| ; | end |
| "..."| literal|
| \[...]| group|
| x?| x is optional|
| x (b)| b maybe apear after x|
| - regex()| capture of regex|

```
package -> decl*;

decl -> fn_decl
    | extern_fn_decl
    | var_decl
    | statement
    | import_decl;

import_decl -> "import" STRING;
extern_fn_decl -> "extern" STRING IDENT :: IDENT "(" parameters? ")";
fn_decl -> IDENT "::" IDENT "(" [IDENT IDENT (",")?]? ")" block;
var_decl -> IDENT ":=" expr;

statement -> expr_stmt
    | block;

block -> "{" decl* "}";

expr -> fn_call;

fn_call -> IDENT "(" [IDENT (",")]? ")";

literal -> BOOL
    | NUMBER
    | STRING
    | IDENT
    | paren_expr;

paren_expr -> "(" expr ")";

NUMBER -> regex([0-9]);
STRING -> """ any_char """;
IDENT -> regex([a-zA-Z]);
BOOL -> "true" | "false";
NIL -> "nil"
```

## fn

```odin
sum :: int(a int, b int) {
    return a + b
}

main :: void() {
}

extern "c" printf :: int(str cstr);
```

## literals

```odin
string -> ""
char -> ''
number -> 19382
bool -> true | false
```

## import

Import tokenizes and parses the given file and outputs everthing to the main translation.
Currently it copies everthing to who imported it.

```odin
import "<filepath>"
```

## var

```odin
a := 0
b := ""
c := fn()
```
