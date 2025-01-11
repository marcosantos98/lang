# Lang

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

- Infered:

```odin
a := 0
b := ""
c := fn_call()
d := 1 + 2
```

With type:

```odin
a : int = 10
```

## data

```odin
a := 10
a_ptr := &b
b := *a_ptr

struct A {
   b int,
   c int,
}

main :: void() {
    a := A{10, 11}
}
```

## return

```odin
sum :: int(a int, b int) {
    return a + b
}
```
