package main

import "core:c/libc"
import "core:fmt"
import "core:os"
import "core:path/filepath"
import "core:slice"
import "core:strings"

_ :: slice
_ :: filepath

FileContext :: struct {
    cursor:     int,
    tokens:     []Token,
    ast:        []^AstNode,
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
    PLUS,
    MINUS,
    MULT,
    DIV,
    LT,
    GT,
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
        case '\r':
            col = 1
            cursor += 1
        case '\n':
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
        case '-':
            append(&tokens, make_token(.MINUS, cursor, cursor + 1, col, row, file_context))
            col += 1
            cursor += 1
        case '*':
            append(&tokens, make_token(.MULT, cursor, cursor + 1, col, row, file_context))
            col += 1
            cursor += 1
        case '/':
            append(&tokens, make_token(.DIV, cursor, cursor + 1, col, row, file_context))
            col += 1
            cursor += 1
        case '+':
            append(&tokens, make_token(.PLUS, cursor, cursor + 1, col, row, file_context))
            col += 1
            cursor += 1
        case '<':
            append(&tokens, make_token(.LT, cursor, cursor + 1, col, row, file_context))
            col += 1
            cursor += 1
        case '>':
            append(&tokens, make_token(.GT, cursor, cursor + 1, col, row, file_context))
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
TRACE :: #config(trace, false)

trace :: proc(fmt_: string, args: ..any) {
    when TRACE {
        fmt.printfln(fmt_, ..args)
    }
}

AstNode :: struct {
    as: NodeType,
}

NodeType :: union {
    ^Statement,
    ^FnDeclStmt,
    ^ImportDeclStmt,
    ^VarDeclStmt,
    ^BlockStmt,
    ^ExprStmt,
    ^IfStmt,

    //
    ^VarExpr,
    ^FnCallExpr,
    ^LiteralExpr,
    ^BinaryExpr,
}

Expr :: struct {
    using base: Statement,
    as_expr:    ExprType,
}

BinaryExpr :: struct {
    using expr: Expr,
    operator:   Token,
    lhs:        ^Expr,
    rhs:        ^Expr,
}

VarExpr :: struct {
    using expr: Expr,
    name:       Token,
}

FnCallExpr :: struct {
    using expr: Expr,
    name:       Token,
    args:       []^Expr,
}

ExprType :: union {
    ^FnCallExpr,
    ^VarExpr,
    ^LiteralExpr,
    ^BinaryExpr,
}

Statement :: struct {
    using base: AstNode,
    as_stmt:    StmtType,
}

StmtType :: union {
    ^FnDeclStmt,
    ^ImportDeclStmt,
    ^BlockStmt,
    ^VarDeclStmt,
    ^ExprStmt,
    ^IfStmt,
}

IfStmt :: struct {
    using stmt: Statement,
    cond:       ^Expr,
    block:      ^BlockStmt,
}

ExprStmt :: struct {
    using _: Statement,
    expr:    ^Expr,
}

FnFlags :: enum {
    EXTERNAL,
}

FnArg :: struct {
    name, type: Token,
}

FnDeclStmt :: struct {
    using _:        Statement,
    name, ret_type: Token,
    params:         []FnArg,
    block:          ^BlockStmt,
    flags:          bit_set[FnFlags],
    libname:        Token,
}

ImportDeclStmt :: struct {
    using _: Statement,
    path:    Token,
}

VarDeclStmt :: struct {
    using _: Statement,
    name:    Token,
    expr:    ^Expr,
}

BlockStmt :: struct {
    using _: Statement,
    exprs:   []^Statement,
}

LiteralExpr :: struct {
    using expr: Expr,
    type:       enum {
        STRING,
        NUMBER,
        BOOL,
    },
    lit:        Token,
}

tok :: proc(filectx: ^FileContext) -> Token {
    return filectx.tokens[filectx.cursor]
}

tokloc :: proc(filectx: ^FileContext, i := 0) -> string {
    t := tok(filectx)
    return fmt.tprintf("{}:{}:{}:", filectx.file.file_path, t.loc.row, t.loc.col)
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

tok_lit_is :: proc(filectx: ^FileContext, lit: string) -> bool {
    return filectx.tokens[filectx.cursor].lit == lit
}

tok_is_keyword :: proc(filectx: ^FileContext, keyword: Keywords) -> bool {
    return tok(filectx).lit in keywords && keywords[tok(filectx).lit] == keyword
}

newnode :: proc($type: typeid) -> ^type {
    node := new(type, context.temp_allocator)
    node.as = node
    return node
}

newstmtnode :: proc(type: $T) -> ^ExprStmt {
    stmt := newnode(ExprStmt)
    stmt.as_stmt = stmt
    stmt.expr = type
    return stmt
}

// ;putils

parse_import_stmt :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    adv(filectx) // import
    fmt.assertf(tok(filectx).type == .LIT_STR, "Expect path string after `import` keyword, found {}", tok(filectx).lit)
    path := tok(filectx)
    trace("ImportDeclExpr ({})", path.lit)
    adv(filectx) // path

    node := newnode(ImportDeclStmt)
    node.as_stmt = node
    node.path = path

    return node, true
}

parse_var_decl_stmt :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    name := tok(filectx)
    trace("VarDeclExpr ({})", name.lit)
    adv(filectx) // name
    adv(filectx) // :
    adv(filectx) // =
    expr: ^Expr
    if expr_, ok := int_parse(filectx); ok {
        expr = expr_.as.(^ExprStmt).expr
    } else {
        return {}, false
    }

    node := newnode(VarDeclStmt)
    node.as_stmt = node
    node.expr = expr
    node.name = name

    return node, true
}

parse_block_stmt :: proc(filectx: ^FileContext) -> (^BlockStmt, bool) {
    trace("BlockExpr")
    exprs := make([dynamic]^Statement, context.temp_allocator)
    adv(filectx) // {
    for tok(filectx).type != .CLOSE_CBRACKET {
        fmt.assertf(
            tok(filectx).type != .EOF,
            "Expect either statemt or block close `}`, found end of file",
            tok(filectx).lit,
        )
        if expr, ok := int_parse(filectx); ok {
            append(&exprs, cast(^Statement)expr)
        } else {
            return {}, false
        }
    }
    adv(filectx) // }

    node := newnode(BlockStmt)
    node.as_stmt = node
    node.exprs = exprs[:]

    return node, true
}


parse_fn_call_expr :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    name := tok(filectx)
    trace("FnCallExpr ({})", name.lit)
    adv(filectx) // name

    fmt.assertf(tok(filectx).type == .OPEN_PAREN, "Expect '(' after function name, found {}", tok(filectx).lit)
    adv(filectx) // (

    args := make([dynamic]^Expr, context.temp_allocator)
    for tok(filectx).type != .CLOSE_PAREN {
        fmt.assertf(
            tok(filectx).type != .EOF,
            "Expect either proc parameters or `)`, found end of file",
            tok(filectx).lit,
        )
        // FIXME: maybe only parse expr?
        if expr, ok := int_parse(filectx); ok {
            append(&args, expr.as.(^ExprStmt).expr)
        } else {
            return {}, false
        }

        if tok(filectx).type == .COMMA {
            adv(filectx) // ,
        }

    }
    adv(filectx) // )

    fn_call := newnode(FnCallExpr)
    fn_call.as_expr = fn_call
    fn_call.name = name
    fn_call.args = args[:]

    return newstmtnode(fn_call), true
}

parse_fn_decl_stmt :: proc(filectx: ^FileContext) -> (^AstNode, bool) {

    flags := bit_set[FnFlags]{}
    lib: Token

    if tok_is_keyword(filectx, .EXTERNAL) {
        flags += {.EXTERNAL}
        adv(filectx) // extern
        fmt.assertf(
            tok(filectx).type == .LIT_STR,
            "Parser: {} Expect external library name after `external` keyword, found {}",
            tokloc(filectx),
            tok(filectx).lit,
        )
        lib = tok(filectx)
        adv(filectx) // libname
    }

    name := tok(filectx)
    trace("FnDeclExpr ({})", name.lit)
    adv(filectx) // name
    adv(filectx) // :
    adv(filectx) // :
    fmt.assertf(tok(filectx).type == .IDENTIFIER, "Expect type after proc declaration `::` found {}", tok(filectx).lit)
    type := tok(filectx)
    adv(filectx) // type

    parameters := make([dynamic]FnArg, context.temp_allocator)

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

        append(&parameters, FnArg{name = arg_name, type = arg_type})

        if tok(filectx).type == .COMMA {
            adv(filectx) // ,
        }
    }
    adv(filectx) // )

    node := newnode(FnDeclStmt)
    node.as_stmt = node
    if FnFlags.EXTERNAL not_in flags {
        fmt.assertf(
            tok(filectx).type == .OPEN_CBRACKET,
            "Expect `{` after proc parameters declaration, found {}",
            tok(filectx).lit,
        )

        block, ok := parse_block_stmt(filectx)
        fmt.assertf(ok, "Parser: {} Failed to parse block statement", tokloc(filectx))
        node.block = block
    }
    node.flags = flags
    node.libname = lib
    node.ret_type = type
    node.name = name
    node.params = parameters[:]
    return node, true
}

parse_var_expr :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    name := tok(filectx)
    trace("VarExpr ({})", name.lit)
    adv(filectx) // name

    node := newnode(VarExpr)
    node.as_expr = node
    node.name = name

    return newstmtnode(node), true
}

parse_if_stmt :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    adv(filectx) // if

    cond: ^Expr
    if cond_, ok := int_parse(filectx); ok {
        cond = cond_.as.(^ExprStmt).expr
    } else {
        fmt.println("Failed to parse if condition")
        return nil, false
    }

    fmt.assertf(
        tok(filectx).type == .OPEN_CBRACKET,
        "Parser: {} expected `{{` after `if` condition, but found {}",
        tokloc(filectx),
        tok(filectx).lit,
    )

    block: ^BlockStmt
    if block_, ok := parse_block_stmt(filectx); ok {
        block = block_
    } else {
        fmt.println("Failed to parse if block")
        return nil, false
    }

    node := newnode(IfStmt)
    node.as_stmt = node
    node.cond = cond
    node.block = block

    return node, true
}

try_parse_identifier :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    if next_two_are(filectx, .COLON, .COLON) {
        return parse_fn_decl_stmt(filectx)
    } else if next_is(filectx, .OPEN_PAREN) {
        return parse_fn_call_expr(filectx)
    } else if next_two_are(filectx, .COLON, .EQ) {
        return parse_var_decl_stmt(filectx)
    } else if tok(filectx).lit in keywords {
        switch keywords[tok(filectx).lit] {
        case .EXTERNAL:
            return parse_fn_decl_stmt(filectx)
        case .IMPORT:
            return parse_import_stmt(filectx)
        case .IF:
            return parse_if_stmt(filectx)
        case .TRUE, .FALSE:
            return parse_lit_bool(filectx)
        }
    } else {
        return parse_var_expr(filectx)
    }
    return nil, false
}

parse_lit_str :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    trace("LitStr")
    lit := tok(filectx)
    adv(filectx)
    node := newnode(LiteralExpr)
    node.as_expr = node
    node.type = .STRING
    node.lit = lit
    return newstmtnode(node), true
}

parse_lit_bool :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    trace("LitBool")
    lit := tok(filectx)
    adv(filectx) // bool
    node := newnode(LiteralExpr)
    node.as_expr = node
    node.type = .BOOL
    node.lit = lit
    return newstmtnode(node), true
}

parse_lit_number :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    trace("LitNumber")
    lit := tok(filectx)
    adv(filectx)
    node := newnode(LiteralExpr)
    node.as_expr = node
    node.type = .NUMBER
    node.lit = lit
    return newstmtnode(node), true
}

parse_primary :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    trace("ParsePrimary")
    #partial switch tok(filectx).type {
    case .IDENTIFIER:
        return try_parse_identifier(filectx)
    case .LIT_STR:
        return parse_lit_str(filectx)
    case .LIT_NUMBER:
        return parse_lit_number(filectx)
    case .OPEN_CBRACKET:
        return parse_block_stmt(filectx)
    }
    fmt.panicf("Parser: {} Not valid primary: {}", tokloc(filectx), tok(filectx).lit)
}

parse_lhs :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    trace("ParseLHS")
    return parse_primary(filectx)
}

get_tok_precedence :: proc(filectx: ^FileContext) -> int {
    #partial switch tok(filectx).type {
    case .PLUS, .MINUS:
        return 20
    case .MULT, .DIV:
        return 40
    case .LT, .GT:
        return 10
    }
    return -1
}

parse_rhs :: proc(filectx: ^FileContext, precedence: int, lhs: ^AstNode) -> (^AstNode, bool) {
    for {
        tok_pre := get_tok_precedence(filectx)
        if tok_pre < precedence {
            return lhs, true
        }
        trace("ParseRHS")

        op := tok(filectx)
        adv(filectx)

        rhs, rhs_ok := parse_primary(filectx)
        if !rhs_ok {
            fmt.println("Parse: failed to parse rhs.")
            return nil, false
        }

        next_pre := get_tok_precedence(filectx)
        if tok_pre < next_pre {
            rhs, rhs_ok = parse_rhs(filectx, tok_pre + 1, rhs)
        }

        binexpr := newnode(BinaryExpr)
        binexpr.as_expr = binexpr
        binexpr.operator = op
        binexpr.lhs = lhs.as.(^ExprStmt).expr
        binexpr.rhs = rhs.as.(^ExprStmt).expr

        return newstmtnode(binexpr), true
    }
}

int_parse :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    lhs, lhs_ok := parse_lhs(filectx)
    if lhs_ok {
        ret, rhs_ok := parse_rhs(filectx, 0, lhs)
        if rhs_ok {
            return ret, true
        }
    } else {
        fmt.panicf("Parser: {} Failed while parsing lhs: {}", tokloc(filectx), tok(filectx).type)
    }
    return nil, false
}

parse :: proc(filectx: ^FileContext) -> bool {

    decls := make([dynamic]^AstNode, context.temp_allocator)
    for tok(filectx).type != .EOF {
        if node, ok := int_parse(filectx); ok {
            append(&decls, node)
        } else {
            return false
        }
    }

    filectx.ast = decls[:]
    return true
}

Keywords :: enum {
    EXTERNAL,
    IMPORT,
    IF,
    TRUE,
    FALSE,
}

keywords := map[string]Keywords {
    "extern" = .EXTERNAL,
    "import" = .IMPORT,
    "if"     = .IF,
    "true"   = .TRUE,
    "false"  = .FALSE,
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

    transpile_fn_decl_stmt :: proc(filectx: ^FileContext, transpiler: Transpiler, expr: ^FnDeclStmt) {

        if FnFlags.EXTERNAL in expr.flags {
            top_write(transpiler, "extern \"C\" {} {}(", expr.ret_type.lit, expr.name.lit)
            for i in 0 ..< len(expr.params) {
                arg := expr.params[i]
                top_write(transpiler, "{}", arg.type.lit)
                if i != len(expr.params) - 1 {
                    top_write(transpiler, ", ")
                }
            }
            top_write(transpiler, ");\n")
        } else {

            filectx.transpiler.has_main = expr.name.lit == "main"
            fn_name := expr.name.lit == "main" ? "___entry___" : expr.name.lit

            // declaration
            top_write(transpiler, "{} {}(", expr.ret_type.lit, fn_name)
            for i in 0 ..< len(expr.params) {
                arg := expr.params[i]
                top_write(transpiler, "{}", arg.type.lit)
                if i != len(expr.params) - 1 {
                    top_write(transpiler, ", ")
                }
            }
            top_write(transpiler, ");\n")

            // implementation
            decl_write(transpiler, "{} {}(", expr.ret_type.lit, fn_name)
            for i in 0 ..< len(expr.params) {
                arg := expr.params[i]
                decl_write(transpiler, "{} {}", arg.type.lit, arg.name.lit)
                if i != len(expr.params) - 1 {
                    decl_write(transpiler, ", ")
                }
            }
            decl_write(transpiler, ")")
            transpile_block_stmt(filectx, transpiler, expr.block)

            filectx.transpiler.functions[expr.name.lit] = {expr.ret_type.lit}
        }
    }

    transpile_fn_call_expr :: proc(filectx: ^FileContext, transpiler: Transpiler, expr: ^FnCallExpr) {
        decl_write(transpiler, "{}(", expr.name.lit)
        for i in 0 ..< len(expr.args) {
            it := expr.args[i]
            transpile_expr(filectx, transpiler, it)
            if i != len(expr.args) - 1 {
                decl_write(transpiler, ", ")
            }
        }
        decl_write(transpiler, ");\n")
    }

    transpile_lit_expr :: proc(filectx: ^FileContext, transpiler: Transpiler, expr: ^LiteralExpr) {
        switch expr.type {
        case .STRING:
            // FIXME: assuming always used in declaration
            decl_write(transpiler, "\"{}\"", expr.lit.lit)
        case .NUMBER, .BOOL:
            decl_write(transpiler, "{}", expr.lit.lit)
        }
    }

    transpile_import_stmt :: proc(filectx: ^FileContext, transpiler: Transpiler, expr: ^ImportDeclStmt) {
    }

    try_to_infer :: proc(filectx: ^FileContext, expr: ^Expr) -> (string, bool) {
        switch v in expr.as_expr {
        case ^FnCallExpr:
            // FIXME: Should I worry about non existing functions? YES
            return filectx.transpiler.functions[v.name.lit].type, true
        case ^LiteralExpr:
            switch v.type {
            case .STRING:
                return "cstr", true
            case .NUMBER:
                // FIXME: Number is hardcoded to int
                return "int", true
            case .BOOL:
                return "bool", true
            }
        case ^VarExpr:
            if v.name.lit in filectx.transpiler.vars {
                return filectx.transpiler.vars[v.name.lit], true
            }
        case ^BinaryExpr:
            #partial switch v.operator.type {
            case .LT, .GT:
                return "bool", true
            }
            return try_to_infer(filectx, v.lhs)
        }
        return "", false
    }

    transpile_var_expr :: proc(filectx: ^FileContext, transpiler: Transpiler, stmt: ^VarExpr) {
        decl_write(transpiler, "{}", stmt.name.lit)
    }

    transpile_block_stmt :: proc(filectx: ^FileContext, transpiler: Transpiler, stmt: ^BlockStmt) {
        decl_write(transpiler, "{{\n")
        for s in stmt.exprs {
            transpile_stmt(filectx, transpiler, s)
        }
        decl_write(transpiler, "}}\n")
    }

    transpile_if_stmt :: proc(filectx: ^FileContext, transpiler: Transpiler, stmt: ^IfStmt) {
        decl_write(transpiler, "if (")
        transpile_expr(filectx, transpiler, stmt.cond)
        decl_write(transpiler, ")")
        transpile_block_stmt(filectx, transpiler, stmt.block)
    }

    transpile_stmt :: proc(filectx: ^FileContext, transpiler: Transpiler, stmt: ^Statement) {
        switch it in stmt.as_stmt {
        case ^FnDeclStmt:
            transpile_fn_decl_stmt(filectx, transpiler, it)
        case ^ImportDeclStmt:
            transpile_import_stmt(filectx, transpiler, it)
        case ^BlockStmt:
            transpile_block_stmt(filectx, transpiler, it)
        case ^VarDeclStmt:
            transpile_var_decl_stmt(filectx, transpiler, it)
        case ^ExprStmt:
            transpile_expr(filectx, transpiler, it.expr)
        case ^IfStmt:
            transpile_if_stmt(filectx, transpiler, it)
        }
    }


    tokloc :: proc(filectx: ^FileContext, t: Token) -> string {
        return fmt.tprintf("{}:{}:{}:", filectx.file.file_path, t.loc.row, t.loc.col)
    }

    transpile_var_decl_stmt :: proc(filectx: ^FileContext, transpiler: Transpiler, expr: ^VarDeclStmt) {
        // FIXME: assuming always used in declaration
        type, ok := try_to_infer(filectx, expr.expr)
        if !ok {
            fmt.panicf(
                "CompilerBUG: {} Couldn't infer type for {}. Type inference should be made before.\n",
                tokloc(filectx, expr.name),
                expr.name.lit,
            )
        }
        decl_write(transpiler, "{} {} = ", type, expr.name.lit)
        transpile_expr(filectx, transpiler, expr.expr)
        decl_write(transpiler, ";\n")

        filectx.transpiler.vars[expr.name.lit] = type
    }

    transpile_binary_expr :: proc(filectx: ^FileContext, transpiler: Transpiler, expr: ^BinaryExpr) {
        transpile_expr(filectx, transpiler, expr.lhs)
        decl_write(transpiler, "{}", expr.operator.lit)
        transpile_expr(filectx, transpiler, expr.rhs)
    }


    transpile_expr :: proc(filectx: ^FileContext, transpiler: Transpiler, expr: ^Expr) {
        switch it in expr.as_expr {
        case ^FnCallExpr:
            transpile_fn_call_expr(filectx, transpiler, it)
        case ^LiteralExpr:
            transpile_lit_expr(filectx, transpiler, it)
        case ^VarExpr:
            transpile_var_expr(filectx, transpiler, it)
        case ^BinaryExpr:
            transpile_binary_expr(filectx, transpiler, it)
        }
    }

    deal_with_import_expr :: proc(filectx: ^FileContext, expr: ^ImportDeclStmt) -> bool {
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
        if import_expr, ok := expr.as.(^ImportDeclStmt); ok {
            if !deal_with_import_expr(filectx, import_expr) {
                return false
            }
        }
    }

    for expr in filectx.ast {
        stmt := cast(^Statement)expr
        transpile_stmt(filectx, transpiler, stmt)
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

    //print_node :: proc(node: ^AstNode) {
    //    switch it in node.as {
    //    case ^ExprStmt:
    //        print_node(&it.expr.base)
    //    case ^FnDeclStmt:
    //        fmt.println("FnDeclStmt:", it.name.lit, "Flags", it.flags, "LibName:", it.libname.lit)
    //        fmt.print("Params [")
    //        for p in it.params {
    //            fmt.print(p.name.lit, p.type.lit, ",")
    //        }
    //        fmt.println("]")
    //        if it.block != nil {
    //            print_node(it.block)
    //        }
    //    case ^ImportDeclStmt:
    //        fmt.println("Import:", it.path.lit)
    //    case ^VarDeclStmt:
    //        fmt.println("VarDeclStmt", it.name.lit)
    //        print_node(it.expr)
    //    case ^BlockStmt:
    //        fmt.println("> BlockStmt:")
    //        for e in it.exprs {
    //            print_node(e)
    //        }
    //        fmt.println("< BlockStmt")
    //    case ^VarExpr:
    //        fmt.println("VarExpr", it.name.lit)
    //    case ^FnCallExpr:
    //        fmt.println("(Todo: Args) FnCallExpr", it.name.lit)
    //    case ^LiteralExpr:
    //        fmt.println("LiteralExpr", it.lit.lit, it.type)
    //    case ^Statement:
    //        print_node(it)
    //    }
    //}

    //for expr in filectx.ast {
    //    print_node(expr)
    //}

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
