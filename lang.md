# Lang

Number :: 19293823
Str :: "dasdas"
Char :: 'c'
Bool :: true | false
Ident :: [a-zA-Z_]

Literal :: Number | Str | Char | Bool

Stmt :: Literal | FnCall

FnCall :: Ident "(" [Stmt] ")"

BlockExpr :: "{" Stmt "}"
FnDeclExpr :: Ident "::" Ident "(" [Ident,Ident]* ")" BlockExpr
ExternFnDeclExpr :: "extern" Str Ident "::" Ident "(" [Ident Ident]* ")"
VarDeclExpr :: Ident ":=" Stmt
ImportExpr :: "import" Str

## fn

```
sum :: int(a int, b int) {
    return a + b
}

main :: void() {
}

extern "c" printf :: int(str cstr);
```

## literals

```
string -> ""
char -> ''
number -> 19382
bool -> true | false
```

## import

Import tokenizes and parses the given file and outputs everthing to the main translation.
Currently it copies everthing to who imported it.

```
import "<filepath>"
```

## var

```
a := 0
b := ""
c := fn()
```
