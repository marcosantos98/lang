# Lang

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
