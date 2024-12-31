# lang :computer: :page_facing_up: 

Another attempt at an implementation of a programming language.

## Windows? huhhh

The project contains a single `build.ps1` which is just a wrapper arround `odin`, this means that you can either use the script or just `odin build .`.

```
lang.exe <filename>
```

### Current example usage:

Currently it transpiles the code to a cpp file in the same location as the source file with `.cpp`.
This file can then be compiled with clang or other c++ compiler.

```
lang.exe tests/fn_call.lang
clang++ tests/fn_call.cpp
./a.exe
```
