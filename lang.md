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
var a = 0
var b = ""
var c = fn_call()
var d = 1 + 2
```

With type:

```odin
var a : int = 10
```

## data

```odin
var a = 10
var a_ptr = &b
var b = *a_ptr

struct A {
   b int,
   c int,
}

main :: void() {
    var a = A{10, 11}
}
```

## return

```odin
sum :: int(a int, b int) {
    return a + b
}
```

## array

```odin
var data = []int{}
data[0] = data[1]
```
