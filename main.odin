#+feature dynamic-literals
package main

import "core:c/libc"
import "core:fmt"
import "core:os"
import "core:path/filepath"
import "core:slice"
import "core:strings"
import "core:sys/windows"

_ :: slice
_ :: filepath

FileContext :: struct {
    parser: struct {
        in_assign:     bool,
        is_pointer:    bool,
        is_deref:      bool,
        is_type_parse: bool,
        is_arr:        bool,
        as_expr:       bool,
    },
    cursor: int,
    tokens: []Token,
    ast:    []^AstNode,
    file:   struct {
        file_path: string,
        cpp_path:  string,
        content:   []u8,
    },
}

// :tokenizer

TokenType :: enum {
    IDENTIFIER,
    COLON,
    SEMICOLON,
    OPEN_CBRACKET,
    CLOSE_CBRACKET,
    OPEN_PAREN,
    CLOSE_PAREN,
    COMMA,
    LIT_STR,
    LIT_NUMBER,
    LIT_FLOAT,
    PLUS,
    PLUS_EQ,
    MINUS_EQ,
    MINUS,
    ASTERISK,
    DIV,
    MOD,
    LT,
    GT,
    EQ,
    DOT,
    AMPERSAND,
    OPEN_SQRB,
    CLOSE_SQRB,
    CARET,
    AT,
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
        for !is_eof(file_ctx, end) &&
            (is_letter(file_ctx.file.content[end]) ||
                    is_digit(file_ctx.file.content[end]) ||
                    file_ctx.file.content[end] == '_') {
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

    tokenize_number :: proc(file_ctx: ^FileContext, cursor: int) -> (int, bool) {
        end := cursor
        has_dot := false
        for !is_eof(file_ctx, end) && (is_digit(file_ctx.file.content[end]) || file_ctx.file.content[end] == '.') {
            end += 1
            if file_ctx.file.content[end] == '.' {
                fmt.assertf(!has_dot, "Number already contains a `.`")
                has_dot = true
            }
        }
        return end, has_dot
    }

    tokenize_comment :: proc(filectx: ^FileContext, cursor: int) -> int {
        end := cursor
        for filectx.file.content[end] != '\n' {
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
        case '@':
            append(&tokens, make_token(.AT, cursor, cursor + 1, col, row, file_context))
            col += 1
            cursor += 1
        case ':':
            append(&tokens, make_token(.COLON, cursor, cursor + 1, col, row, file_context))
            col += 1
            cursor += 1
        case ';':
            append(&tokens, make_token(.SEMICOLON, cursor, cursor + 1, col, row, file_context))
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
        case '[':
            append(&tokens, make_token(.OPEN_SQRB, cursor, cursor + 1, col, row, file_context))
            col += 1
            cursor += 1
        case ']':
            append(&tokens, make_token(.CLOSE_SQRB, cursor, cursor + 1, col, row, file_context))
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
            if file_context.file.content[cursor + 1] == '=' {
                append(&tokens, make_token(.MINUS_EQ, cursor, cursor + 2, col, row, file_context))
                col += 2
                cursor += 2
            } else {
                append(&tokens, make_token(.MINUS, cursor, cursor + 1, col, row, file_context))
                col += 1
                cursor += 1
            }
        case '*':
            append(&tokens, make_token(.ASTERISK, cursor, cursor + 1, col, row, file_context))
            col += 1
            cursor += 1
        case '%':
            append(&tokens, make_token(.MOD, cursor, cursor + 1, col, row, file_context))
            col += 1
            cursor += 1
        case '/':
            if file_context.file.content[cursor + 1] == '/' {
                start := cursor
                cursor = tokenize_comment(file_context, cursor)
                col += cursor - start
            } else {
                append(&tokens, make_token(.DIV, cursor, cursor + 1, col, row, file_context))
                col += 1
                cursor += 1
            }
        case '+':
            if file_context.file.content[cursor + 1] == '=' {
                append(&tokens, make_token(.PLUS_EQ, cursor, cursor + 2, col, row, file_context))
                col += 2
                cursor += 2
            } else {
                append(&tokens, make_token(.PLUS, cursor, cursor + 1, col, row, file_context))
                col += 1
                cursor += 1
            }
        case '<':
            append(&tokens, make_token(.LT, cursor, cursor + 1, col, row, file_context))
            col += 1
            cursor += 1
        case '.':
            append(&tokens, make_token(.DOT, cursor, cursor + 1, col, row, file_context))
            col += 1
            cursor += 1
        case '&':
            append(&tokens, make_token(.AMPERSAND, cursor, cursor + 1, col, row, file_context))
            col += 1
            cursor += 1
        case '^':
            append(&tokens, make_token(.CARET, cursor, cursor + 1, col, row, file_context))
            col += 1
            cursor += 1
        case '>':
            append(&tokens, make_token(.GT, cursor, cursor + 1, col, row, file_context))
            col += 1
            cursor += 1
        case '"':
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
                float := false
                cursor, float = tokenize_number(file_context, cursor)
                append(&tokens, make_token(float ? .LIT_FLOAT : .LIT_NUMBER, start, cursor, col, row, file_context))
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
    ^WhileStmt,
    ^AssignStmt,
    ^ForStmt,
    ^BreakStmt,
    ^StructDeclStmt,
    ^ReturnStmt,

    //
    ^VarExpr,
    ^FnCallExpr,
    ^LiteralExpr,
    ^BinaryExpr,
    ^StructInitExpr,
    ^StructFieldExpr,
    ^PointerExpr,
    ^DerefExpr,
    ^AutoCastExpr,
    ^ArrayTypeExpr,
    ^ArrayIndexExpr,
    ^VarArgsExpr,
    ^AsExpr,
    ^ParenExpr,
}

Expr :: struct {
    using base: Statement,
    as_expr:    ExprType,
}

AssignFlag :: enum {
    PLUS,
    MINUS,
}

ParenExpr :: struct {
    using _: Expr,
    expr:    ^Expr,
}

AsExpr :: struct {
    using _: Expr,
    lhs:     ^Expr,
    rhs:     ^Expr,
}

ArrayTypeExpr :: struct {
    using _:    Expr,
    len:        ^Expr,
    type:       ^Expr,
    value_list: []^Expr,
}

ArrayIndexExpr :: struct {
    using _: Expr,
    pre:     ^Expr,
    index:   ^Expr,
}

AutoCastExpr :: struct {
    using _: Expr,
    expr:    ^Expr,
}

StructInitExpr :: struct {
    using _:    Expr,
    type:       Token,
    value_list: []^Expr,
}

BinaryExpr :: struct {
    using expr: Expr,
    operator:   Token,
    lhs:        ^Expr,
    rhs:        ^Expr,
}

VarExpr :: struct {
    using expr:           Expr,
    name:                 Token,
    is_pointer, is_deref: bool,
}


FnCallExpr :: struct {
    using expr: Expr,
    name:       Token,
    args:       []^Expr,
    is_stmt:    bool,
}

StructFieldExpr :: struct {
    using _: Expr,
    who:     ^Expr,
    field:   ^Expr,
}

VarArgsExpr :: struct {
    using _: Expr,
}

ExprType :: union {
    ^FnCallExpr,
    ^VarExpr,
    ^LiteralExpr,
    ^BinaryExpr,
    ^StructInitExpr,
    ^StructFieldExpr,
    ^PointerExpr,
    ^DerefExpr,
    ^AutoCastExpr,
    ^ArrayTypeExpr,
    ^ArrayIndexExpr,
    ^VarArgsExpr,
    ^AsExpr,
    ^ParenExpr,
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
    ^WhileStmt,
    ^AssignStmt,
    ^ForStmt,
    ^BreakStmt,
    ^StructDeclStmt,
    ^ReturnStmt,
}

BreakStmt :: struct {
    using expr: Expr,
}

AssignStmt :: struct {
    using stmt: Statement,
    lhs, rhs:   ^Expr,
    flags:      bit_set[AssignFlag],
}

IfStmt :: struct {
    using stmt: Statement,
    cond:       ^Expr,
    block:      ^BlockStmt,
    else_:      ^Statement,
}

WhileStmt :: struct {
    using stmt: Statement,
    cond:       ^Expr,
    block:      ^BlockStmt,
}

ExprStmt :: struct {
    using _: Statement,
    expr:    ^Expr,
}

ForStmt :: struct {
    using _: Statement,
    init:    ^Statement,
    cond:    ^Expr,
    post:    ^Statement,
    block:   ^BlockStmt,
}

ReturnStmt :: struct {
    using _: Statement,
    expr:    ^Expr,
}

FnFlags :: enum {
    EXTERNAL,
    HAS_VARGS,
}

PointerExpr :: struct {
    using _: Expr,
    type:    Token,
}

DerefExpr :: struct {
    using _: Expr,
    expr:    ^Expr,
}

FnArg :: struct {
    name: Token,
    type: ^Expr,
}

StructDeclStmt :: struct {
    using _: Statement,
    name:    Token,
    fields:  []FnArg,
}

FnDeclStmt :: struct {
    using _:  Statement,
    name:     Token,
    ret_type: ^Expr,
    params:   []FnArg,
    block:    ^BlockStmt,
    flags:    bit_set[FnFlags],
    libname:  Token,
}

ImportDeclStmt :: struct {
    using _: Statement,
    path:    Token,
}

VarDeclStmt :: struct {
    using _: Statement,
    name:    Token,
    type:    ^Expr,
    expr:    ^Expr,
    typed:   bool,
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
        NUMBER_FLOAT,
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

next_three_are :: proc(filectx: ^FileContext, a, b, c: TokenType) -> bool {
    return(
        filectx.tokens[filectx.cursor + 1].type == a &&
        filectx.tokens[filectx.cursor + 2].type == b &&
        filectx.tokens[filectx.cursor + 3].type == c \
    )
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

check_if_assign :: proc(filectx: ^FileContext) -> bool {
    cursor := filectx.cursor

    is_tok :: proc(filectx: ^FileContext, cursor: int) -> TokenType {
        return filectx.tokens[cursor].type
    }

    for {
        if is_tok(filectx, cursor) == .IDENTIFIER || is_tok(filectx, cursor) == .DOT {
            cursor += 1
        } else if is_tok(filectx, cursor) == .EQ ||
           is_tok(filectx, cursor) == .PLUS_EQ ||
           is_tok(filectx, cursor) == .MINUS_EQ {
            return true
        } else {
            return false
        }
    }
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

prev_tok :: proc(filectx: ^FileContext) -> Token {
    if filectx.cursor - 1 < 0 {
        panic("Prev tokens out of founds!")
    }
    return filectx.tokens[filectx.cursor - 1]
}

is_unary_context :: proc(filectx: ^FileContext) -> bool {
    prev_tok := prev_tok(filectx)
    return(
        prev_tok.type == .OPEN_PAREN ||
        prev_tok.type == .CLOSE_PAREN ||
        prev_tok.type == .COMMA ||
        prev_tok.type == .EQ ||
        prev_tok.type == .PLUS_EQ ||
        prev_tok.type == .MINUS_EQ \
    )
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

    adv(filectx) // var

    name := tok(filectx)
    trace("VarDeclExpr ({})", name.lit)
    adv(filectx) // name

    typed := false
    type: ^Expr
    if tok(filectx).type == .COLON {
        typed = true
        adv(filectx) // :
        filectx.parser.is_type_parse = true
        type = parse_as_lhs_expr_or(filectx, "Failed to parse var type for {}`{}`", tokloc(filectx), name.lit)
        filectx.parser.is_type_parse = false
    }

    fmt.assertf(
        tok(filectx).type == .EQ,
        "{} Expect `=` after varname got `{}`. (Initialzed variables aren't supported)",
        tokloc(filectx),
        tok(filectx).lit,
    )

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
    node.type = type
    node.typed = typed

    return node, true
}

parse_auto_cast_expr :: proc(filectx: ^FileContext) -> (^AstNode, bool) {

    adv(filectx) // xx

    expr := parse_as_stmt_expr_or(filectx, "Failed to parse the auto cast expression")

    node := newnode(AutoCastExpr)
    node.as_expr = node
    node.expr = expr

    return newstmtnode(node), true
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

parse_array_index_expr :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    trace("ArrayIndexExpr")
    filectx.parser.as_expr = true
    pre := parse_as_stmt_expr_or(filectx, "Failed to parse expression before index")
    filectx.parser.as_expr = false

    adv(filectx) // [

    index := parse_as_stmt_expr_or(filectx, "Failed to parse index expression")

    fmt.assertf(
        tok(filectx).type == .CLOSE_SQRB,
        "{} Expect `[` after index, found `{}`",
        tokloc(filectx),
        tok(filectx).lit,
    )

    adv(filectx) // ]

    node := newnode(ArrayIndexExpr)
    node.as_expr = node
    node.pre = pre
    node.index = index

    return newstmtnode(node), true
}

parse_fn_call_expr :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    name := tok(filectx)
    trace("FnCallExpr ({})", name.lit)
    adv(filectx) // name

    is_stmt := filectx.parser.as_expr

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

        expr := parse_as_stmt_expr_or(filectx, "Failed to parse function call argument")
        append(&args, expr)

        if tok(filectx).type == .COMMA {
            adv(filectx) // ,
        }

    }
    adv(filectx) // )

    fn_call := newnode(FnCallExpr)
    fn_call.as_expr = fn_call
    fn_call.name = name
    fn_call.args = args[:]
    fn_call.is_stmt = is_stmt

    return newstmtnode(fn_call), true
}

parse_fn_decl_stmt :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    trace("FnDeclStmt")
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

    filectx.parser.is_type_parse = true
    type := parse_as_stmt_expr_or(filectx, "Failed to parse type for {}", name)
    filectx.parser.is_type_parse = false

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
            tok(filectx).type == .IDENTIFIER,
            "Expect proc parameters (name + type), found {}",
            tok(filectx).lit,
        )
        arg_name := tok(filectx)
        adv(filectx) // arg_name

        arg_type := parse_as_stmt_expr_or(filectx, "Failed to parse parameters type for {}", arg_name)

        if _, ok := arg_type.as_expr.(^VarArgsExpr); ok {
            flags += {.HAS_VARGS}
        }

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

    is_pointer := filectx.parser.is_pointer
    is_deref := filectx.parser.is_deref

    defer {
        filectx.parser.is_pointer = false
        filectx.parser.is_deref = false
    }

    name := tok(filectx)
    trace("VarExpr ({})", name.lit)
    adv(filectx) // name

    node := newnode(VarExpr)
    node.as_expr = node
    node.name = name
    node.is_pointer = is_pointer
    node.is_deref = is_deref

    return newstmtnode(node), true
}

parse_if_stmt :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    trace("ParseIfStmt")
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

    else_: ^Statement = nil
    if tok_is_keyword(filectx, .ELSE) {
        adv(filectx) // else
        if tok_is_keyword(filectx, .IF) {
            if stmt, ok := parse_if_stmt(filectx); ok {
                else_ = stmt.as.(^IfStmt)
            } else {
                fmt.println("Failed to parse branch at", tokloc(filectx))
                return nil, false
            }
        } else if tok(filectx).type == .OPEN_CBRACKET {
            if stmt, ok := parse_block_stmt(filectx); ok {
                else_ = stmt
            } else {
                fmt.println("Failed to parse else if body")
                return nil, false
            }
        } else {
            fmt.panicf("Failed {}", tok(filectx))
        }
    }

    node := newnode(IfStmt)
    node.as_stmt = node
    node.cond = cond
    node.block = block
    node.else_ = else_

    return node, true
}

parse_while_stmt :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    trace("WhileStmt")
    adv(filectx) // while

    cond: ^Expr
    if cond_, ok := int_parse(filectx); ok {
        cond = cond_.as.(^ExprStmt).expr
    } else {
        fmt.println("Failed to parse while condition")
        return nil, false
    }

    fmt.assertf(
        tok(filectx).type == .OPEN_CBRACKET,
        "Parser: {} expected `{{` after `while` condition, but found {}",
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

    node := newnode(WhileStmt)
    node.as_stmt = node
    node.block = block
    node.cond = cond

    return node, true
}

parse_struct_field_expr :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    trace("StructFieldExpr")
    who: ^Expr
    if who_, ok := parse_var_expr(filectx); ok {
        who = who_.as.(^ExprStmt).expr
    } else {
        fmt.println("Failed to parse struct field who")
        return nil, false
    }

    adv(filectx) // .

    field: ^Expr
    if field_, ok := parse_var_expr(filectx); ok {
        field = field_.as.(^ExprStmt).expr
    } else {
        fmt.println("Failed to parse struct field field")
        return nil, false
    }

    node := newnode(StructFieldExpr)
    node.as_expr = node
    node.who = who
    node.field = field

    return newstmtnode(node), true
}

parse_as_stmt_expr_or :: proc(filectx: ^FileContext, msg: string, args: ..any, loc := #caller_location) -> ^Expr {
    if stmt, ok := int_parse(filectx); ok {
        if expr, okk := stmt.as.(^ExprStmt); okk {
            return expr.as.(^ExprStmt).expr
        } else {
            fmt.print(stmt.as)
        }
    }
    fmt.print(loc, "")
    fmt.panicf(msg, ..args)
}
parse_as_lhs_expr_or :: proc(filectx: ^FileContext, msg: string, args: ..any, loc := #caller_location) -> ^Expr {
    if stmt, ok := parse_lhs(filectx); ok {
        if expr, okk := stmt.as.(^ExprStmt); okk {
            return expr.as.(^ExprStmt).expr
        } else {
            fmt.print(stmt.as)
        }
    }
    fmt.print(loc, "")
    fmt.panicf(msg, ..args)
}
parse_assign_stmt :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    trace("Assign")

    filectx.parser.in_assign = true

    lhs := parse_as_stmt_expr_or(filectx, "Failed to parse left hand side of assign expression")
    flags := bit_set[AssignFlag]{}

    #partial switch tok(filectx).type {
    case .PLUS_EQ:
        flags += {.PLUS}
    case .MINUS_EQ:
        flags += {.MINUS}
    }

    adv(filectx) // = / += / -=

    filectx.parser.in_assign = false

    rhs := parse_as_stmt_expr_or(filectx, "Failed to parse right hand side of assign expression")

    node := newnode(AssignStmt)
    node.as_stmt = node
    node.flags = flags
    node.lhs = lhs
    node.rhs = rhs
    return node, true
}

parse_for_stmt :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    trace("ForStmt")
    adv(filectx) // for

    init: ^Statement
    if init_, ok := int_parse(filectx); ok {
        init = cast(^Statement)init_
    } else {
        fmt.println("Failed to parse for init statement.")
        return nil, false
    }

    fmt.assertf(
        tok(filectx).type == .SEMICOLON,
        "Parser: {} expected `;` after `for` init, but found {}",
        tokloc(filectx),
        tok(filectx).lit,
    )

    adv(filectx) // ;

    cond: ^Expr
    if cond_, ok := int_parse(filectx); ok {
        cond = cond_.as.(^ExprStmt).expr
    } else {
        fmt.println("Failed to parse for condition")
        return nil, false
    }

    fmt.assertf(
        tok(filectx).type == .SEMICOLON,
        "Parser: {} expected `;` after `for` condition, but found {}",
        tokloc(filectx),
        tok(filectx).lit,
    )

    adv(filectx) // ;

    post: ^Statement
    if post_, ok := int_parse(filectx); ok {
        post = cast(^Statement)post_
    } else {
        fmt.println("Failed to parse for post statement.")
        return nil, false
    }

    fmt.assertf(
        tok(filectx).type == .OPEN_CBRACKET,
        "Parser: {} expected `{{` after `for` post condition, but found {}",
        tokloc(filectx),
        tok(filectx).lit,
    )

    block: ^BlockStmt
    if block_, ok := parse_block_stmt(filectx); ok {
        block = block_
    } else {
        fmt.println("Failed to parse for block")
        return nil, false
    }

    node := newnode(ForStmt)
    node.as_stmt = node
    node.init = init
    node.cond = cond
    node.post = post
    node.block = block

    return node, true
}

parse_break_stmt :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    trace("BreakStmt")
    adv(filectx) // break
    node := newnode(BreakStmt)
    node.as_stmt = node
    return node, true
}

parse_struct_decl_stmt :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    trace("StructDeclStmt")
    adv(filectx) // struct

    fmt.assertf(tok(filectx).type == .IDENTIFIER, "Expect name after `struct`, found {}", tok(filectx).lit)

    name := tok(filectx)
    adv(filectx) // name

    fmt.assertf(
        tok(filectx).type == .OPEN_CBRACKET,
        "Expect struct block ({{...}}) after struct name, found {}",
        tok(filectx).lit,
    )

    adv(filectx) // {

    fields := make([dynamic]FnArg, context.temp_allocator)
    for tok(filectx).type != .CLOSE_CBRACKET {
        fmt.assertf(
            tok(filectx).type != .EOF,
            "Expect either struct fields or `}`, found end of file",
            tok(filectx).lit,
        )
        fmt.assertf(tok(filectx).type == .IDENTIFIER, "Expect struct field (name + type), found {}", tok(filectx).lit)
        arg_name := tok(filectx)
        adv(filectx) // arg_name

        arg_type := parse_as_stmt_expr_or(filectx, "Failed to parse field type for {}", arg_name)

        append(&fields, FnArg{name = arg_name, type = arg_type})

        if tok(filectx).type == .COMMA {
            adv(filectx) // ,
        }
    }

    adv(filectx) // }

    node := newnode(StructDeclStmt)
    node.as_stmt = node
    node.name = name
    node.fields = fields[:]

    return node, true
}

parse_struct_init_expr :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    trace("StructInitExpr")
    type := tok(filectx)
    adv(filectx) // type

    adv(filectx) // {

    values := make([dynamic]^Expr, context.temp_allocator)
    for tok(filectx).type != .CLOSE_CBRACKET {
        fmt.assertf(
            tok(filectx).type != .EOF,
            "Expect either expression or block close `}`, found end of file",
            tok(filectx).lit,
        )

        if expr, ok := int_parse(filectx); ok {
            append(&values, expr.as.(^ExprStmt).expr)
        } else {
            fmt.println("Failed to parse expression in struct initializer")
            return nil, false
        }

        if tok(filectx).type == .COMMA {
            adv(filectx) // ,
        }
    }

    adv(filectx) // }

    node := newnode(StructInitExpr)
    node.as_expr = node
    node.type = type
    node.value_list = values[:]

    return newstmtnode(node), true
}

parse_return_stmt :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    trace("ReturnStmt")
    adv(filectx) // return

    expr := parse_as_stmt_expr_or(filectx, "Failed to parse return expr")

    node := newnode(ReturnStmt)
    node.as_stmt = node
    node.expr = expr

    return node, true
}

parse_deref_expr :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    trace("DerefExpr")
    adv(filectx) // $
    expr := parse_as_stmt_expr_or(filectx, "Failed to parse deref expr")

    node := newnode(DerefExpr)
    node.as_expr = node
    node.expr = expr

    return newstmtnode(node), true
}

parse_pointer_expr :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    trace("PointerExpr")
    adv(filectx) // ^
    type := tok(filectx)
    adv(filectx) // type

    node := newnode(PointerExpr)
    node.as_expr = node
    node.type = type

    return newstmtnode(node), true
}

try_parse_identifier :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    trace("ParseIdentifier: ({}{})", tokloc(filectx), tok(filectx).lit)
    if tok(filectx).lit in keywords {
        switch keywords[tok(filectx).lit] {
        case .EXTERNAL:
            return parse_fn_decl_stmt(filectx)
        case .IMPORT:
            return parse_import_stmt(filectx)
        case .IF:
            return parse_if_stmt(filectx)
        case .TRUE, .FALSE:
            return parse_lit_bool(filectx)
        case .WHILE:
            return parse_while_stmt(filectx)
        case .FOR:
            return parse_for_stmt(filectx)
        case .BREAK:
            return parse_break_stmt(filectx)
        case .STRUCT:
            return parse_struct_decl_stmt(filectx)
        case .RETURN:
            return parse_return_stmt(filectx)
        case .VAR:
            return parse_var_decl_stmt(filectx)
        case .AUTO_CAST:
            return parse_auto_cast_expr(filectx)
        case .ELSE, .AS:
            fmt.println("Invalid token:", tok(filectx).lit)
            return nil, false
        }
    } else if next_is(filectx, .OPEN_PAREN) && !filectx.parser.is_type_parse {
        return parse_fn_call_expr(filectx)
    } else if next_two_are(filectx, .COLON, .COLON) {
        return parse_fn_decl_stmt(filectx)
    } else if !filectx.parser.is_arr && !filectx.parser.as_expr && next_is(filectx, .OPEN_CBRACKET) {
        return parse_struct_init_expr(filectx)
    } else if !filectx.parser.as_expr && next_is(filectx, .OPEN_SQRB) {
        return parse_array_index_expr(filectx)
    } else if next_is(filectx, .DOT) {
        return parse_struct_field_expr(filectx)
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

// :lit_float
parse_lit_float :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    trace("LitNumberFloat")
    lit := tok(filectx)
    adv(filectx)
    node := newnode(LiteralExpr)
    node.as_expr = node
    node.type = .NUMBER_FLOAT
    node.lit = lit
    return newstmtnode(node), true
}

parse_arr_type_expr :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    trace("ArrayTypeExpr")
    adv(filectx) // [

    len: ^Expr = nil
    if tok(filectx).type != .CLOSE_SQRB {
        len = parse_as_stmt_expr_or(filectx, "Failed to parse array lenght")
    }

    fmt.assertf(
        tok(filectx).type == .CLOSE_SQRB,
        "{} Expected `]` to close array type, found `{}`",
        tokloc(filectx),
        tok(filectx).lit,
    )

    adv(filectx) // ]

    filectx.parser.is_arr = true
    type := parse_as_stmt_expr_or(filectx, "Failed to parse array type.")
    filectx.parser.is_arr = false

    value_list: [dynamic]^Expr
    if tok(filectx).type == .OPEN_CBRACKET {
        adv(filectx) // {
        value_list = make([dynamic]^Expr, context.temp_allocator)
        for tok(filectx).type != .CLOSE_CBRACKET {
            fmt.assertf(
                tok(filectx).type != .EOF,
                "Expect value or block close `}`, found end of file",
                tok(filectx).lit,
            )

            value := parse_as_stmt_expr_or(filectx, "Failed to parse array value")
            append(&value_list, value)

            if tok(filectx).type == .COMMA {
                adv(filectx) // ,
            }
        }

        adv(filectx) // }
    }

    node := newnode(ArrayTypeExpr)
    node.as_expr = node
    node.type = type
    node.len = len
    node.value_list = value_list[:]

    return newstmtnode(node), true
}

parse_var_args_expr :: proc(filectx: ^FileContext) -> (^AstNode, bool) {
    adv(filectx) // .
    adv(filectx) // .
    adv(filectx) // .
    node := newnode(VarArgsExpr)
    node.as_expr = node
    return newstmtnode(node), true
}

// :parent_expr
parse_paren_expr :: proc(filectx: ^FileContext) -> (^AstNode, bool) {

    adv(filectx) // (
    expr := parse_as_stmt_expr_or(filectx, "Parsed to parse parentheses expression")

    fmt.assertf(
        tok(filectx).type == .CLOSE_PAREN,
        "{} Expected `)`, but found `{}`",
        tokloc(filectx),
        tok(filectx).lit,
    )

    adv(filectx) // )

    node := newnode(ParenExpr)
    node.as_expr = node
    node.expr = expr

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
    case .LIT_FLOAT:
        return parse_lit_float(filectx)
    case .OPEN_CBRACKET:
        return parse_block_stmt(filectx)
    case .AMPERSAND:
        {
            trace("AMPERSAND")
            filectx.parser.is_pointer = true
            adv(filectx) // &
            return parse_primary(filectx)
        }
    case .CARET:
        return parse_pointer_expr(filectx)
    case .AT:
        {
            trace("AT")
            filectx.parser.is_deref = true
            adv(filectx) // @
            return parse_primary(filectx)
        }
    case .OPEN_SQRB:
        return parse_arr_type_expr(filectx)
    case .DOT:
        if next_two_are(filectx, .DOT, .DOT) {
            return parse_var_args_expr(filectx)
        }
    case .OPEN_PAREN:
        return parse_paren_expr(filectx)
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
    case .ASTERISK, .DIV, .MOD:
        return 40
    case .LT, .GT:
        return 10
    case .EQ, .PLUS_EQ, .MINUS_EQ:
        return 5
    case .IDENTIFIER:
        if tok(filectx).lit in keywords {
            if keywords[tok(filectx).lit] == .AS {
                return 6
            }
        }
    }
    return -1
}

parse_rhs :: proc(filectx: ^FileContext, precedence: int, lhs: ^AstNode) -> (^AstNode, bool) {
    for {
        tok_pre := get_tok_precedence(filectx)
        if tok_pre < precedence {
            return lhs, true
        }

        trace("ParseRHS {} {}", tok(filectx).lit, tok_pre)
        op := tok(filectx)
        adv(filectx)

        filectx.parser.as_expr = true
        rhs, rhs_ok := parse_primary(filectx)
        if !rhs_ok {
            fmt.println("Parse: failed to parse rhs.")
            return nil, false
        }
        filectx.parser.as_expr = false

        next_pre := get_tok_precedence(filectx)
        if tok_pre < next_pre {
            rhs, rhs_ok = parse_rhs(filectx, tok_pre + 1, rhs)
        }

        #partial switch op.type {
        case .MINUS_EQ, .PLUS_EQ, .EQ:
            trace("AssignStmt")
            node := newnode(AssignStmt)
            node.as_stmt = node
            node.lhs = lhs.as.(^ExprStmt).expr
            node.rhs = rhs.as.(^ExprStmt).expr
            flags := bit_set[AssignFlag]{}

            #partial switch op.type {
            case .PLUS_EQ:
                flags += {.PLUS}
            case .MINUS_EQ:
                flags += {.MINUS}
            }
            node.flags = flags
            return node, true
        case .IDENTIFIER:
            if op.lit in keywords {
                #partial switch keywords[op.lit] {
                case .AS:
                    node := newnode(AsExpr)
                    node.as_expr = node
                    node.lhs = lhs.as.(^ExprStmt).expr
                    node.rhs = rhs.as.(^ExprStmt).expr
                    return newstmtnode(node), true
                case:
                    panic("Not")
                }
            }
        case:
            binexpr := newnode(BinaryExpr)
            binexpr.as_expr = binexpr
            binexpr.operator = op
            binexpr.lhs = lhs.as.(^ExprStmt).expr
            binexpr.rhs = rhs.as.(^ExprStmt).expr
            return newstmtnode(binexpr), true
        }

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

// :keywords
Keywords :: enum {
    EXTERNAL,
    IMPORT,
    IF,
    TRUE,
    FALSE,
    WHILE,
    FOR,
    BREAK,
    ELSE,
    STRUCT,
    RETURN,
    VAR,
    AUTO_CAST,
    AS,
}

keywords := map[string]Keywords {
    "extern" = .EXTERNAL,
    "import" = .IMPORT,
    "if"     = .IF,
    "true"   = .TRUE,
    "false"  = .FALSE,
    "while"  = .WHILE,
    "for"    = .FOR,
    "break"  = .BREAK,
    "else"   = .ELSE,
    "struct" = .STRUCT,
    "return" = .RETURN,
    "var"    = .VAR,
    "xx"     = .AUTO_CAST,
    "as"     = .AS,
}
// ;parser

print_usage :: proc() {
    fmt.println("Usage: lang command [options]")
    fmt.println("")
    fmt.println("Commands:")
    fmt.println("	run <filename>.lang")
    fmt.println("	  Options:")
    fmt.println("		-o path to output binary;")
    fmt.println("")
    fmt.println("	build <filename>.lang")
    fmt.println("	  Options:")
    fmt.println("		-o path to output binary;")
    fmt.println("")

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

    deal_with_import("./internal/builtin.lang")
    transpile_file(filectx.ast)

    if out {
        if path_no_ext, ok := strings.substring_to(path, strings.last_index(path, ".")); ok {
            source := fmt.tprintf(
                "{}\n{}\n{}\n",
                strings.to_string(ctx.defines),
                strings.to_string(ctx.top_level),
                strings.to_string(ctx.decl),
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

ExecOption :: enum {
    GARBAGE_OUT,
    DONT_HURT_ON_FAIL,
}
ExecOptions :: bit_set[ExecOption]

exec :: proc(cmd: string, opts := ExecOptions{}) -> (u32, bool) {
    when ODIN_OS == .Windows {
        si: windows.STARTUPINFOW

        if .GARBAGE_OUT in opts {
            nul: windows.HANDLE
            nul = windows.CreateFileW(
                windows.utf8_to_wstring("nul"),
                windows.GENERIC_WRITE,
                windows.FILE_SHARE_WRITE,
                nil,
                windows.OPEN_EXISTING,
                windows.FILE_ATTRIBUTE_NORMAL,
                nil,
            )
            if nul == windows.INVALID_HANDLE_VALUE {
                panic("Failed to open null device")
            }
            si.dwFlags |= windows.STARTF_USESTDHANDLES
            si.hStdOutput = nul
            si.hStdError = nul
        }

        pi: windows.PROCESS_INFORMATION
        if !windows.CreateProcessW(nil, windows.utf8_to_wstring(cmd), nil, nil, false, 0, nil, nil, &si, &pi) {
            if .DONT_HURT_ON_FAIL in opts {
                return 0, false
            } else {
                panic("Failed to create process clang++")
            }
        } else {
            if .GARBAGE_OUT not_in opts {
                fmt.println(cmd)
            }
        }

        windows.WaitForSingleObject(pi.hProcess, windows.INFINITE)

        exit_code: windows.DWORD
        if !windows.GetExitCodeProcess(pi.hProcess, &exit_code) {
            panic("Failed to get exit code!")
        }

        windows.CloseHandle(pi.hProcess)
        windows.CloseHandle(pi.hThread)
    } else {
        fmt.panicf("Implement exec on {}", ODIN_OS)
    }

    return exit_code, true
}

Options :: struct {
    src:                string,
    out:                string,
    compile_with_debug: bool,
    run:                bool,
    cflags:             [dynamic]string,
}

options_for_cmd := map[string][]string {
    "run"   = {"-o", "-d", "-cf"},
    "build" = {"-o", "-d", "-cf"},
}

parse_args :: proc() -> (Options, bool) {
    opts := Options{}

    need_n_args_or :: proc(cnt: int, current: int, msg: string) -> bool {
        if (current + cnt) >= len(os.args) {
            fmt.println(msg)
            return false
        }
        return true
    }

    cmd: string

    switch os.args[1] {
    case "run":
        opts.run = true
        if !need_n_args_or(1, 1, "`lang run` requires a filepath after.") {
            print_usage()
            return {}, false
        }
        opts.src = os.args[2]
        cmd = "run"
    case "build":
        if !need_n_args_or(1, 1, "`lang build` requires a filepath.") {
            print_usage()
            return {}, false
        }
        opts.src = os.args[2]
        cmd = "build"
    case:
        panic("Commmand not implemented!")
    }

    for idx := 0; idx < len(os.args); {
        arg := os.args[idx]
        if idx == 0 || !strings.starts_with(arg, "-") {
            idx += 1
            continue
        }
        if slice.contains(options_for_cmd[cmd][:], arg) {
            switch arg {
            case "-o":
                if !need_n_args_or(1, idx, "Found `-o`, but wasn't provided the path") {
                    print_usage()
                    return {}, false
                }
                opts.out = os.args[idx + 1]
                idx += 2
            case "-d":
                opts.compile_with_debug = true
                idx += 1
            case "-cf":
                if !need_n_args_or(1, idx, "Found `-cf`, but wasn't provided a flag") {
                    print_usage()
                    return {}, false
                }
                append(&opts.cflags, os.args[idx + 1])
                idx += 2
            case:
                panic("Option not implemented")
            }
        } else {
            fmt.panicf("Argument `{}` not supported by {}", arg, cmd)
        }
    }

    return opts, true
}

main :: proc() {

    if len(os.args) == 1 {
        print_usage()
        return
    }

    if _, ok := exec("clang++ --version", {.GARBAGE_OUT, .DONT_HURT_ON_FAIL}); !ok {
        fmt.eprintln("Error: Didn't find `clang++`. Be sure to have it on path and installed.")
        return
    }

    opts, ok := parse_args()
    if !ok {
        return
    }

    path := opts.src
    filectx := FileContext{}
    filectx.file.file_path = path

    if !do_all_passes_on_file(&filectx) {
        return
    }

    cmd_builder := strings.builder_make(context.temp_allocator)

    fmt.sbprintf(&cmd_builder, "clang++ ")
    if opts.compile_with_debug {
        fmt.sbprintf(&cmd_builder, "-ggdb ")
    }
    if opts.out != "" {
        fmt.sbprintf(&cmd_builder, "-o {} ", opts.out)
    }
    for cf in opts.cflags {
        fmt.sbprintf(&cmd_builder, "{} ", cf)
    }
    fmt.sbprintf(&cmd_builder, "{} ", filectx.file.cpp_path)

    if code, clang_ok := exec(strings.to_string(cmd_builder)); clang_ok && code != 0 {
        fmt.println("clang return non zero exit code.")
        os.exit(1)
    }

    if opts.run {
        exec(opts.out != "" ? opts.out : "./a.exe")
    }
}
