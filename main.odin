package main

import "core:c/libc"
import "core:fmt"
import "core:os"
import "core:strings"

FileContext :: struct {
    cursor:     int,
    tokens:     []Token,
    ast:        []Expr,
    transpiler: struct {
        defines:   strings.Builder,
        decl:      strings.Builder,
        top_level: strings.Builder,
    },
    file:       struct {
        file_path: string,
        content:   []u8,
    },
}

// :tokenizer

TokenType :: enum {
    IDENTIFIER,
    COLON,
    OPEN_CBRACKET,
    CLOSE_CBRACKET,
    OPEN_PAREN,
    CLOSE_PAREN,
    COMMA,
    LIT_STR,
    EOF,
}

Token :: struct {
    loc:  struct {
        col, row: int,
    },
    lit:  string,
    type: TokenType,
}

tokenize :: proc(file_context: ^FileContext) -> bool {
    cursor := 0
    col, row := 1, 1

    is_eof :: proc(file_ctx: ^FileContext, cursor: int) -> bool {
        return cursor >= len(file_ctx.file.content)
    }

    is_letter :: proc(c: u8) -> bool {
        return (c >= 97 && c <= 122) || (c >= 65 && c <= 90)
    }

    is_digit :: proc(c: u8) -> bool {
        return c >= 48 && c <= 57
    }

    tokenize_word :: proc(file_ctx: ^FileContext, cursor: int) -> int {
        end := cursor
        for !is_eof(file_ctx, end) && is_letter(file_ctx.file.content[end]) {
            end += 1
        }
        return end
    }

    tokenize_str :: proc(file_ctx: ^FileContext, cursor: int) -> int {
        end := cursor
        for !is_eof(file_ctx, end) && file_ctx.file.content[end] != '"' {
            end += 1
        }
        return end
    }

    make_token :: proc(type: TokenType, start, end, col, row: int, file_ctx: ^FileContext) -> Token {
        return Token{type = type, loc = {col, row}, lit = string(file_ctx.file.content[start:end])}
    }

    tokens := make([dynamic]Token, context.temp_allocator)

    for !is_eof(file_context, cursor) {
        c := file_context.file.content[cursor]
        switch c {
        case '\r', '\n':
            col = 1
            row += 1
            cursor += 1
        case ' ', '\t':
            col += 1
            cursor += 1
        case ':':
            append(&tokens, make_token(.COLON, cursor, cursor + 1, col, row, file_context))
            col += 1
            cursor += 1
        case '{':
            append(&tokens, make_token(.OPEN_CBRACKET, cursor, cursor + 1, col, row, file_context))
            col += 1
            cursor += 1
        case '}':
            append(&tokens, make_token(.CLOSE_CBRACKET, cursor, cursor + 1, col, row, file_context))
            col += 1
            cursor += 1
        case '(':
            append(&tokens, make_token(.OPEN_PAREN, cursor, cursor + 1, col, row, file_context))
            col += 1
            cursor += 1
        case ')':
            append(&tokens, make_token(.CLOSE_PAREN, cursor, cursor + 1, col, row, file_context))
            col += 1
            cursor += 1
        case ',':
            append(&tokens, make_token(.COMMA, cursor, cursor + 1, col, row, file_context))
            col += 1
            cursor += 1
        case '"':
            // FIXME: this fucks the col
            start := cursor + 1 // adv "
            cursor = tokenize_str(file_context, start)
            append(&tokens, make_token(.LIT_STR, start, cursor, col, row, file_context))
            cursor += 1
            col += cursor - start
        case:
            if libc.isalpha(auto_cast file_context.file.content[cursor]) != 0 {
                start := cursor
                cursor = tokenize_word(file_context, cursor)
                append(&tokens, make_token(.IDENTIFIER, start, cursor, col, row, file_context))
                col += cursor - start
            } else {
                fmt.printfln(
                    "Tokenizer error: `{:c}` not handled at: {}:{}:{}",
                    c,
                    file_context.file.file_path,
                    row,
                    col,
                )
                return false
            }
        }
    }

    append(&tokens, make_token(.EOF, cursor, cursor, col, row + 1, file_context))
    file_context.tokens = tokens[:]

    return true
}

// ;tokenizer

// :parser
Keywords :: enum {
    EXTERNAL,
}
keywords := map[string]Keywords {
    "extern" = .EXTERNAL,
}

ArgDeclExpr :: struct {
    type: Token,
    name: Token,
}

BlockExpr :: struct {
    expr: []Expr,
}

FnDeclExpr :: struct {
    name:  Token,
    type:  Token,
    args:  []ArgDeclExpr,
    scope: BlockExpr,
}

ExternalFnExpr :: struct {
    lib:  Token,
    name: Token,
    type: Token,
    args: []ArgDeclExpr,
}

LitType :: enum {
    STRING,
}

LiteralExpr :: struct {
    type: LitType,
    lit:  Token,
}

FnCallExpr :: struct {
    name:   Token,
    params: []Expr,
}

Expr :: union {
    FnDeclExpr,
    ExternalFnExpr,
    LiteralExpr,
    FnCallExpr,
}

File :: []Expr

parse :: proc(filectx: ^FileContext) -> bool {

    tok :: proc(filectx: ^FileContext) -> Token {
        return filectx.tokens[filectx.cursor]
    }

    next_is :: proc(filectx: ^FileContext, a: TokenType) -> bool {
        return filectx.tokens[filectx.cursor + 1].type == a
    }

    next_two_are :: proc(filectx: ^FileContext, a, b: TokenType) -> bool {
        return filectx.tokens[filectx.cursor + 1].type == a && filectx.tokens[filectx.cursor + 2].type == b
    }

    adv :: proc(filectx: ^FileContext) {
        filectx.cursor += 1
    }

    parse_block_expr :: proc(filectx: ^FileContext) -> (BlockExpr, bool) {
        exprs := make([dynamic]Expr, context.temp_allocator)
        adv(filectx) // {
        for tok(filectx).type != .CLOSE_CBRACKET {
            fmt.assertf(
                tok(filectx).type != .EOF,
                "Expect either statemt or block close `}`, found end of file",
                tok(filectx).lit,
            )
            if expr, ok := parse_expr(filectx); ok {
                append(&exprs, expr)
            } else {
                return {}, false
            }
        }
        adv(filectx) // }
        return {expr = exprs[:]}, true
    }

    parse_fn_decl_expr :: proc(filectx: ^FileContext) -> (FnDeclExpr, bool) {
        name := tok(filectx)
        adv(filectx) // name
        adv(filectx) // :
        adv(filectx) // :
        fmt.assertf(
            tok(filectx).type == .IDENTIFIER,
            "Expect type after proc declaration `::` found {}",
            tok(filectx).lit,
        )
        type := tok(filectx)
        adv(filectx) // type

        parameters := make([dynamic]ArgDeclExpr, context.temp_allocator)

        fmt.assertf(tok(filectx).type == .OPEN_PAREN, "Expect `(` after proc type found {}", tok(filectx).lit)
        adv(filectx) // (
        for tok(filectx).type != .CLOSE_PAREN {
            fmt.assertf(
                tok(filectx).type != .EOF,
                "Expect either proc parameters or `)`, found end of file",
                tok(filectx).lit,
            )
            fmt.assertf(
                tok(filectx).type == .IDENTIFIER && next_is(filectx, .IDENTIFIER),
                "Expect proc parameters (name + type), found {}",
                tok(filectx).lit,
            )
            arg_name := tok(filectx)
            adv(filectx) // arg_name
            arg_type := tok(filectx)
            adv(filectx) // arg_type

            append(&parameters, ArgDeclExpr{name = arg_name, type = arg_type})

            if tok(filectx).type == .COMMA {
                adv(filectx) // ,
            }
        }
        adv(filectx) // )

        fmt.assertf(
            tok(filectx).type == .OPEN_CBRACKET,
            "Expect `{` after proc parameters declaration, found {}",
            tok(filectx).lit,
        )
        block, ok := parse_block_expr(filectx)
        assert(ok)
        return {name = name, type = type, args = parameters[:], scope = block}, true
    }

    parse_external_fn_expr :: proc(filectx: ^FileContext) -> (ExternalFnExpr, bool) {
        adv(filectx) // external
        fmt.assertf(
            tok(filectx).type == .LIT_STR,
            "Expect external library name after `external` keyword, found {}",
            tok(filectx).lit,
        )
        lib := tok(filectx)
        adv(filectx) // libname

        fmt.assertf(tok(filectx).type == .IDENTIFIER, "Expect external proc name, found {}", tok(filectx).lit)
        name := tok(filectx)
        adv(filectx) // name
        fmt.assertf(
            tok(filectx).type == .COLON && next_is(filectx, .COLON),
            "Expect after name `::`, found {}",
            tok(filectx).lit,
        )
        adv(filectx) // :
        adv(filectx) // :

        fmt.assertf(tok(filectx).type == .IDENTIFIER, "Expect external proc type, found {}", tok(filectx).lit)
        type := tok(filectx)
        adv(filectx) // type

        parameters := make([dynamic]ArgDeclExpr, context.temp_allocator)

        fmt.assertf(tok(filectx).type == .OPEN_PAREN, "Expect `(` after proc type found {}", tok(filectx).lit)
        adv(filectx) // (
        for tok(filectx).type != .CLOSE_PAREN {
            fmt.assertf(
                tok(filectx).type != .EOF,
                "Expect either proc parameters or `)`, found end of file",
                tok(filectx).lit,
            )
            fmt.assertf(
                tok(filectx).type == .IDENTIFIER && next_is(filectx, .IDENTIFIER),
                "Expect proc parameters (name + type), found {} + {} for {} with type {}",
                tok(filectx).type,
                filectx.tokens[filectx.cursor + 1].type,
                name.lit,
                type.lit,
            )
            arg_name := tok(filectx)
            adv(filectx) // arg_name
            arg_type := tok(filectx)
            adv(filectx) // arg_type

            append(&parameters, ArgDeclExpr{name = arg_name, type = arg_type})

            if tok(filectx).type == .COMMA {
                adv(filectx) // ,
            }
        }
        adv(filectx) // )

        return {lib = lib, name = name, type = type, args = parameters[:]}, true
    }

    parse_fn_call_expr :: proc(filectx: ^FileContext) -> (FnCallExpr, bool) {
        name := tok(filectx)
        adv(filectx) // name

        fmt.assertf(tok(filectx).type == .OPEN_PAREN, "Expect '(' after function name, found {}", tok(filectx).lit)
        adv(filectx) // (

        args := make([dynamic]Expr, context.temp_allocator)
        for tok(filectx).type != .CLOSE_PAREN {
            fmt.assertf(
                tok(filectx).type != .EOF,
                "Expect either proc parameters or `)`, found end of file",
                tok(filectx).lit,
            )
            if expr, ok := parse_expr(filectx); ok {
                append(&args, expr)
            } else {
                return {}, false
            }

            if tok(filectx).type == .COMMA {
                adv(filectx) // ,
            }

        }
        adv(filectx) // )
        return {name, args[:]}, true
    }

    try_parse_ident :: proc(filectx: ^FileContext) -> (Expr, bool) {
        if next_two_are(filectx, .COLON, .COLON) {
            return parse_fn_decl_expr(filectx)
        } else if next_is(filectx, .OPEN_PAREN) {
            return parse_fn_call_expr(filectx)
        } else if tok(filectx).lit in keywords {
            switch keywords[tok(filectx).lit] {
            case .EXTERNAL:
                return parse_external_fn_expr(filectx)
            }
        } else {
            fmt.println("Parser: unimplementation on parse_iden", tok(filectx).type, tok(filectx).lit)
            return nil, false
        }
        return nil, false
    }

    parse_lit_str :: proc(filectx: ^FileContext) -> (LiteralExpr, bool) {
        type := LitType.STRING
        lit := tok(filectx)
        adv(filectx)
        return {type, lit}, true
    }

    parse_expr :: proc(filectx: ^FileContext) -> (Expr, bool) {
        #partial switch tok(filectx).type {
        case .IDENTIFIER:
            return try_parse_ident(filectx)
        case .LIT_STR:
            return parse_lit_str(filectx)
        case:
            fmt.println("Parser: Invalid or unimplementation token:", tok(filectx).type, tok(filectx).lit)
        }
        return nil, false
    }

    exprs := make([dynamic]Expr, context.temp_allocator)
    for tok(filectx).type != .EOF {
        if expr, ok := parse_expr(filectx); ok {
            append(&exprs, expr)
        } else {
            return false
        }
    }

    filectx.ast = exprs[:]

    return true
}
// ;parser

// :transpile

transpile_cpp :: proc(filectx: ^FileContext) -> bool {

    decl_write :: proc(filectx: ^FileContext, fmt_: string, args: ..any, newline_ := false) {
        fmt.sbprintf(&filectx.transpiler.decl, fmt_, ..args, newline = newline_)
    }

    def_write :: proc(filectx: ^FileContext, fmt_: string, args: ..any, newline_ := false) {
        fmt.sbprintf(&filectx.transpiler.defines, fmt_, ..args, newline = newline_)
    }

    top_write :: proc(filectx: ^FileContext, fmt_: string, args: ..any, newline_ := false) {
        fmt.sbprintf(&filectx.transpiler.top_level, fmt_, ..args, newline = newline_)
    }

    transpile_fn_decl :: proc(filectx: ^FileContext, expr: FnDeclExpr) {

        fn_name := expr.name.lit == "main" ? "___entry___" : expr.name.lit

        // declaration
        top_write(filectx, "{} {}(", expr.type.lit, fn_name)
        for i in 0 ..< len(expr.args) {
            arg := expr.args[i]
            top_write(filectx, "{}", arg.type.lit)
            if i != len(expr.args) - 1 {
                top_write(filectx, ", ")
            }
        }
        top_write(filectx, ");\n")

        // implementation
        decl_write(filectx, "{} {}(", expr.type.lit, fn_name)
        for i in 0 ..< len(expr.args) {
            arg := expr.args[i]
            decl_write(filectx, "{} {}", arg.type.lit, arg.name.lit)
            if i != len(expr.args) - 1 {
                decl_write(filectx, ", ")
            }
        }
        decl_write(filectx, "){{\n")
        for expr in expr.scope.expr {
            transpile_expr(filectx, expr)
        }
        decl_write(filectx, "}}\n")
    }

    transpile_external_fn_decl :: proc(filectx: ^FileContext, expr: ExternalFnExpr) {
        top_write(filectx, "extern \"C\" {} {}(", expr.type.lit, expr.name.lit)
        for i in 0 ..< len(expr.args) {
            arg := expr.args[i]
            top_write(filectx, "{}", arg.type.lit)
            if i != len(expr.args) - 1 {
                top_write(filectx, ", ")
            }
        }
        top_write(filectx, ");\n")
    }

    transpile_fn_call :: proc(filectx: ^FileContext, expr: FnCallExpr) {
        decl_write(filectx, "{}(", expr.name.lit)
        for i in 0 ..< len(expr.params) {
            it := expr.params[i]
            transpile_expr(filectx, it)
            if i != len(expr.params) - 1 {
                decl_write(filectx, ", ")
            }
        }
        decl_write(filectx, ");\n")
    }

    transpile_lit :: proc(filectx: ^FileContext, expr: LiteralExpr) {
        // FIXME: assuming always used in declaration
        decl_write(filectx, "\"{}\"", expr.lit.lit)
    }

    transpile_expr :: proc(filectx: ^FileContext, expr: Expr) {
        switch it in expr {
        case FnDeclExpr:
            transpile_fn_decl(filectx, it)
        case ExternalFnExpr:
            transpile_external_fn_decl(filectx, it)
        case FnCallExpr:
            transpile_fn_call(filectx, it)
        case LiteralExpr:
            transpile_lit(filectx, it)
        }
    }

    for expr in filectx.ast {
        transpile_expr(filectx, expr)
    }

    // predefine types
    def_write(filectx, "typedef const char* cstr;\n")

    decl_write(filectx, "int main(int argc, char** argv) {{___entry___();}}")

    return true
}

// ;transpile

print_usage :: proc() {
    fmt.printfln("Usage: lang <filename>")
}

main :: proc() {

    if len(os.args) == 1 {
        print_usage()
        return
    }

    // FIXME: dehardcode when more options
    path := os.args[1]
    filectx := FileContext{}
    filectx.file.file_path = path

    if content, ok := os.read_entire_file(path); ok {
        filectx.file.content = content[:]
    } else {
        // do better error printing
        fmt.println("Failed to read the provided file:", path)
        return
    }

    if !tokenize(&filectx) {
        return
    }

    if !parse(&filectx) {
        return
    }

    if !transpile_cpp(&filectx) {
        return
    }

    if path_no_ext, ok := strings.substring_to(path, strings.last_index(path, ".")); ok {
        if fd, err := os.open(fmt.tprintf("{}.cpp", path_no_ext), os.O_CREATE | os.O_RDWR | os.O_TRUNC);
           err == os.ERROR_NONE {
            os.write_string(fd, strings.to_string(filectx.transpiler.defines))
            os.write_string(fd, strings.to_string(filectx.transpiler.top_level))
            os.write_string(fd, strings.to_string(filectx.transpiler.decl))
            fmt.printfln("Transpiled {} to C++ file at: {}", path, fmt.tprintf("{}.cpp", path_no_ext))
        } else {
            fmt.printfln("Failed to create output file: {}", fmt.tprintf("{}.cpp", path_no_ext))
            return
        }
    } else {
        fmt.printfln("Failed while trying to remove extension from path")
        return
    }
}
