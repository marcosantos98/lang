package main

import "core:c/libc"
import "core:fmt"
import "core:os"
import "core:path/filepath"
import "core:slice"
import "core:strings"

_ :: slice

FileContext :: struct {
    cursor:     int,
    tokens:     []Token,
    ast:        []Expr,
    transpiler: struct {
        has_main:  bool,
        defines:   strings.Builder,
        decl:      strings.Builder,
        top_level: strings.Builder,
        functions: map[string]struct {
            type: string,
        },
        vars:      map[string]string,
    },
    file:       struct {
        file_path: string,
        cpp_path:  string,
        content:   []u8,
    },
}

LangCtx :: struct {
    files: [dynamic]FileContext,
}
// FIXME: remove from global
ctx := LangCtx{}

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
    LIT_NUMBER,
    EQ,
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

    tokenize_number :: proc(file_ctx: ^FileContext, cursor: int) -> int {
        end := cursor
        for !is_eof(file_ctx, end) && is_digit(file_ctx.file.content[end]) {
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
        case '=':
            append(&tokens, make_token(.EQ, cursor, cursor + 1, col, row, file_context))
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
            } else if is_digit(file_context.file.content[cursor]) {
                start := cursor
                cursor = tokenize_number(file_context, cursor)
                append(&tokens, make_token(.LIT_NUMBER, start, cursor, col, row, file_context))
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
    IMPORT,
    TRUE,
    FALSE,
}

keywords := map[string]Keywords {
    "extern" = .EXTERNAL,
    "import" = .IMPORT,
    "true"   = .TRUE,
    "false"  = .FALSE,
}

ImportExpr :: struct {
    path: Token,
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
    NUMBER,
    BOOL,
}

LiteralExpr :: struct {
    type: LitType,
    lit:  Token,
}

FnCallExpr :: struct {
    name:   Token,
    params: []Stmt,
}

VarDeclExpr :: struct {
    name: Token,
    expr: Stmt,
}

VarStmt :: struct {
    name: Token,
}

Stmt :: union {
    LiteralExpr,
    FnCallExpr,
    VarStmt,
}

Expr :: union {
    FnDeclExpr,
    ExternalFnExpr,
    LiteralExpr,
    FnCallExpr,
    ImportExpr,
    VarDeclExpr,
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

        args := make([dynamic]Stmt, context.temp_allocator)
        for tok(filectx).type != .CLOSE_PAREN {
            fmt.assertf(
                tok(filectx).type != .EOF,
                "Expect either proc parameters or `)`, found end of file",
                tok(filectx).lit,
            )
            if expr, ok := parse_stmt(filectx); ok {
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

    parse_import_expr :: proc(filectx: ^FileContext) -> (ImportExpr, bool) {
        adv(filectx) // import
        fmt.assertf(
            tok(filectx).type == .LIT_STR,
            "Expect path string after `import` keyword, found {}",
            tok(filectx).lit,
        )
        path := tok(filectx)
        adv(filectx) // path
        return {path}, true
    }

    parse_var_stmt :: proc(filectx: ^FileContext) -> (VarStmt, bool) {
        name := tok(filectx)
        adv(filectx) // name
        return {name}, true
    }

    parse_lit_bool :: proc(filectx: ^FileContext) -> (LiteralExpr, bool) {
        lit := tok(filectx)
        adv(filectx) // bool
        type := LitType.BOOL
        return {type, lit}, true
    }

    parse_stmt :: proc(filectx: ^FileContext) -> (Stmt, bool) {
        #partial switch tok(filectx).type {
        case .LIT_STR:
            return parse_lit_str(filectx)
        case .LIT_NUMBER:
            return parse_lit_number(filectx)
        case .IDENTIFIER:
            if next_is(filectx, .OPEN_PAREN) {
                return parse_fn_call_expr(filectx)
            } else if tok(filectx).lit == "true" || tok(filectx).lit == "false" {
                return parse_lit_bool(filectx)
            } else {
                return parse_var_stmt(filectx)
            }
        }
        return nil, false
    }

    parse_var_decl_expr :: proc(filectx: ^FileContext) -> (VarDeclExpr, bool) {
        name := tok(filectx)
        adv(filectx) // name
        adv(filectx) // :
        adv(filectx) // =
        expr: Stmt
        if expr_, ok := parse_stmt(filectx); ok {
            expr = expr_
        } else {
            return {}, false
        }
        return {name, expr}, true
    }

    try_parse_ident :: proc(filectx: ^FileContext) -> (Expr, bool) {
        if next_two_are(filectx, .COLON, .COLON) {
            return parse_fn_decl_expr(filectx)
        } else if next_two_are(filectx, .COLON, .EQ) {
            return parse_var_decl_expr(filectx)
        } else if next_is(filectx, .OPEN_PAREN) {
            return parse_fn_call_expr(filectx)
        } else if tok(filectx).lit in keywords {
            switch keywords[tok(filectx).lit] {
            case .EXTERNAL:
                return parse_external_fn_expr(filectx)
            case .IMPORT:
                return parse_import_expr(filectx)
            case .TRUE, .FALSE:
                return parse_lit_bool(filectx)
            }
        } else {
            fmt.println("Parser: unimplementation on parse_iden `", tok(filectx).lit, "`")
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

    parse_lit_number :: proc(filectx: ^FileContext) -> (LiteralExpr, bool) {
        type := LitType.NUMBER
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
        case .LIT_NUMBER:
            return parse_lit_number(filectx)
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

Transpiler :: struct {
    decl, defines, top_level: ^strings.Builder,
}

transpile_cpp :: proc(filectx: ^FileContext, transpiler: Transpiler) -> bool {

    decl_write :: proc(transpiler: Transpiler, fmt_: string, args: ..any, newline_ := false) {
        fmt.sbprintf(transpiler.decl, fmt_, ..args, newline = newline_)
    }

    def_write :: proc(transpiler: Transpiler, fmt_: string, args: ..any, newline_ := false) {
        fmt.sbprintf(transpiler.defines, fmt_, ..args, newline = newline_)
    }

    top_write :: proc(transpiler: Transpiler, fmt_: string, args: ..any, newline_ := false) {
        fmt.sbprintf(transpiler.top_level, fmt_, ..args, newline = newline_)
    }

    @(require_results)
    transpile_fn_decl :: proc(filectx: ^FileContext, transpiler: Transpiler, expr: FnDeclExpr) -> bool {

        filectx.transpiler.has_main = expr.name.lit == "main"
        fn_name := expr.name.lit == "main" ? "___entry___" : expr.name.lit

        // declaration
        top_write(transpiler, "{} {}(", expr.type.lit, fn_name)
        for i in 0 ..< len(expr.args) {
            arg := expr.args[i]
            top_write(transpiler, "{}", arg.type.lit)
            if i != len(expr.args) - 1 {
                top_write(transpiler, ", ")
            }
        }
        top_write(transpiler, ");\n")

        // implementation
        decl_write(transpiler, "{} {}(", expr.type.lit, fn_name)
        for i in 0 ..< len(expr.args) {
            arg := expr.args[i]
            decl_write(transpiler, "{} {}", arg.type.lit, arg.name.lit)
            if i != len(expr.args) - 1 {
                decl_write(transpiler, ", ")
            }
        }
        decl_write(transpiler, "){{\n")
        for expr in expr.scope.expr {
            if !transpile_expr(filectx, transpiler, expr) {
                return false
            }
        }
        decl_write(transpiler, "}}\n")

        filectx.transpiler.functions[expr.name.lit] = {expr.type.lit}

        return true
    }

    transpile_external_fn_decl :: proc(filectx: ^FileContext, transpiler: Transpiler, expr: ExternalFnExpr) {
        top_write(transpiler, "extern \"C\" {} {}(", expr.type.lit, expr.name.lit)
        for i in 0 ..< len(expr.args) {
            arg := expr.args[i]
            top_write(transpiler, "{}", arg.type.lit)
            if i != len(expr.args) - 1 {
                top_write(transpiler, ", ")
            }
        }
        top_write(transpiler, ");\n")
    }

    transpile_fn_call :: proc(filectx: ^FileContext, transpiler: Transpiler, expr: FnCallExpr) {
        decl_write(transpiler, "{}(", expr.name.lit)
        for i in 0 ..< len(expr.params) {
            it := expr.params[i]
            transpile_stmt(filectx, transpiler, it)
            if i != len(expr.params) - 1 {
                decl_write(transpiler, ", ")
            }
        }
        decl_write(transpiler, ");\n")
    }

    transpile_lit :: proc(filectx: ^FileContext, transpiler: Transpiler, expr: LiteralExpr) {
        switch expr.type {
        case .STRING:
            // FIXME: assuming always used in declaration
            decl_write(transpiler, "\"{}\"", expr.lit.lit)
        case .NUMBER, .BOOL:
            decl_write(transpiler, "{}", expr.lit.lit)
        }
    }

    transpile_import :: proc(filectx: ^FileContext, transpiler: Transpiler, expr: ImportExpr) {
    }

    try_to_infer :: proc(filectx: ^FileContext, stmt: Stmt) -> (string, bool) {
        switch v in stmt {
        case FnCallExpr:
            // FIXME: Should I worry about non existing functions? YES
            return filectx.transpiler.functions[v.name.lit].type, true
        case LiteralExpr:
            switch v.type {
            case .STRING:
                return "cstr", true
            case .NUMBER:
                // FIXME: Number is hardcoded to int
                return "int", true
            case .BOOL:
                return "bool", true
            }
        case VarStmt:
            if v.name.lit in filectx.transpiler.vars {
                return filectx.transpiler.vars[v.name.lit], true
            }
        }
        return "", false
    }

    transpile_var :: proc(filectx: ^FileContext, transpiler: Transpiler, stmt: VarStmt) {
        decl_write(transpiler, "{}", stmt.name.lit)
    }

    transpile_stmt :: proc(filectx: ^FileContext, transpiler: Transpiler, stmt: Stmt) {
        switch v in stmt {
        case FnCallExpr:
            transpile_fn_call(filectx, transpiler, v)
        case LiteralExpr:
            transpile_lit(filectx, transpiler, v)
        case VarStmt:
            transpile_var(filectx, transpiler, v)
        }
    }

    @(require_results)
    transpile_var_decl :: proc(filectx: ^FileContext, transpiler: Transpiler, expr: VarDeclExpr) -> bool {
        // FIXME: assuming always used in declaration
        type, ok := try_to_infer(filectx, expr.expr)
        if !ok {
            fmt.printfln("Translation: Couldn't infer type for {}", expr.name.lit)
            return false
        }
        decl_write(transpiler, "{} {} = ", type, expr.name.lit)
        transpile_stmt(filectx, transpiler, expr.expr)
        decl_write(transpiler, ";\n")

        filectx.transpiler.vars[expr.name.lit] = type
        return true
    }


    @(require_results)
    transpile_expr :: proc(filectx: ^FileContext, transpiler: Transpiler, expr: Expr) -> bool {
        switch it in expr {
        case FnDeclExpr:
            if !transpile_fn_decl(filectx, transpiler, it) {
                return false
            }
        case ExternalFnExpr:
            transpile_external_fn_decl(filectx, transpiler, it)
        case FnCallExpr:
            transpile_fn_call(filectx, transpiler, it)
        case LiteralExpr:
            transpile_lit(filectx, transpiler, it)
        case ImportExpr:
            transpile_import(filectx, transpiler, it)
        case VarDeclExpr:
            if !transpile_var_decl(filectx, transpiler, it) {
                return false
            }
        }
        return true
    }

    deal_with_import_expr :: proc(filectx: ^FileContext, expr: ImportExpr) -> bool {
        dir :: proc(path: string, allocator := context.allocator) -> string {
            context.allocator = allocator
            vol := filepath.volume_name(path)
            i := len(path) - 1
            for i >= len(vol) && !filepath.is_separator(path[i]) {
                i -= 1
            }
            dir := path[len(vol):i + 1]
            return strings.concatenate({vol, dir})
        }

        current_dir := dir(filectx.file.file_path, context.temp_allocator)
        path := strings.concatenate({current_dir, expr.path.lit}, context.temp_allocator)
        path, _ = filepath.from_slash(path, context.temp_allocator)
        if !os.exists(path) {
            fmt.printf("Transpiler: Failed to import file with path `{}`. Doesn't exist.\n", path)
            return false
        }

        path_file := FileContext{}
        path_file.file.file_path = path
        if content, ok := os.read_entire_file(path_file.file.file_path, context.temp_allocator); ok {
            path_file.file.content = content[:]
        } else {
            // do better error printing
            fmt.println("Failed to read the provided file:", path_file.file.file_path)
            return false
        }

        if !tokenize(&path_file) {
            return false
        }

        if !parse(&path_file) {
            return false
        }

        trans := Transpiler{}
        trans.decl = &filectx.transpiler.decl
        trans.top_level = &filectx.transpiler.top_level
        trans.defines = &filectx.transpiler.defines

        if !transpile_cpp(&path_file, trans) {
            return false
        }

        delete(path_file.transpiler.vars)
        delete(path_file.transpiler.functions)


        return true
    }

    // deal with imports
    for expr in filectx.ast {
        if import_expr, ok := expr.(ImportExpr); ok {
            if !deal_with_import_expr(filectx, import_expr) {
                return false
            }
        }
    }

    for expr in filectx.ast {
        if !transpile_expr(filectx, transpiler, expr) {
            return false
        }
    }


    if filectx.transpiler.has_main {
        decl_write(transpiler, "int main(int argc, char** argv) {{___entry___();}}")
    }

    delete(filectx.transpiler.vars)
    delete(filectx.transpiler.functions)

    return true
}

// ;transpile

print_usage :: proc() {
    fmt.printfln("Usage: lang <filename>")
}

do_all_passes_on_file :: proc(filectx: ^FileContext, out := true) -> bool {

    path := filectx.file.file_path

    if content, ok := os.read_entire_file(path, context.temp_allocator); ok {
        filectx.file.content = content[:]
    } else {
        // do better error printing
        fmt.println("Failed to read the provided file:", path)
        return false
    }

    if !tokenize(filectx) {
        return false
    }

    if !parse(filectx) {
        return false
    }

    trans := Transpiler{}
    trans.decl = &filectx.transpiler.decl
    trans.top_level = &filectx.transpiler.top_level
    trans.defines = &filectx.transpiler.defines

    // predefine types
    fmt.sbprintf(trans.defines, "/* -- Builtin types -- */\n")
    fmt.sbprintf(trans.defines, "typedef const char* cstr;\n")
    fmt.sbprintf(trans.defines, "/* -- ------------- -- */\n")

    if !transpile_cpp(filectx, trans) {
        return false
    }

    if out {
        if path_no_ext, ok := strings.substring_to(path, strings.last_index(path, ".")); ok {
            source := fmt.tprintf(
                "{}\n{}\n{}\n",
                strings.to_string(filectx.transpiler.defines),
                strings.to_string(filectx.transpiler.top_level),
                strings.to_string(filectx.transpiler.decl),
            )
            if suc := os.write_entire_file(fmt.tprintf("{}.cpp", path_no_ext), transmute([]u8)source); suc {
                fmt.printfln("Transpiled {} to C++ file at: {}", path, fmt.tprintf("{}.cpp", path_no_ext))
                filectx.file.cpp_path = fmt.tprintf("{}.cpp", path_no_ext)
            } else {
                fmt.printfln("Failed to create output file: {}", fmt.tprintf("{}.cpp", path_no_ext))
                return false
            }
        } else {
            fmt.printfln("Failed while trying to remove extension from path")
            return false
        }
    }
    return true
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

    if !do_all_passes_on_file(&filectx) {
        return
    }

    strings.builder_destroy(&filectx.transpiler.defines)
    strings.builder_destroy(&filectx.transpiler.decl)
    strings.builder_destroy(&filectx.transpiler.top_level)
}
