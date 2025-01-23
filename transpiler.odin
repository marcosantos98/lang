package main

import "core:fmt"
import "core:os"
import "core:path/filepath"
import "core:strings"

Visitor :: struct {
    visit_fn_decl_stmt:      proc(self: Visitor, node: ^FnDeclStmt),
    visit_var_decl_stmt:     proc(self: Visitor, node: ^VarDeclStmt),
    visit_import_decl_stmt:  proc(self: Visitor, node: ^ImportDeclStmt),
    visit_block_stmt:        proc(self: Visitor, node: ^BlockStmt),
    visit_struct_decl_stmt:  proc(self: Visitor, node: ^StructDeclStmt),
    visit_return_stmt:       proc(visitor: Visitor, node: ^ReturnStmt),
    visit_assign_stmt:       proc(visitor: Visitor, node: ^AssignStmt),
    visit_if_stmt:           proc(visitor: Visitor, node: ^IfStmt),
    visit_for_stmt:          proc(visitor: Visitor, node: ^ForStmt),
    visit_while_stmt:        proc(visitor: Visitor, node: ^WhileStmt),
    visit_break_stmt:        proc(visitor: Visitor, node: ^BreakStmt),

    // expressions
    visit_var_expr:          proc(self: Visitor, node: ^VarExpr),
    visit_fn_call_expr:      proc(self: Visitor, node: ^FnCallExpr),
    visit_literal_expr:      proc(self: Visitor, node: ^LiteralExpr),
    visit_struct_field_expr: proc(self: Visitor, node: ^StructFieldExpr),
    visit_var_args_expr:     proc(self: Visitor, node: ^VarArgsExpr),
    visit_pointer_expr:      proc(self: Visitor, node: ^PointerExpr),
    visit_struct_init_expr:  proc(visitor: Visitor, node: ^StructInitExpr),
    visit_auto_cast_expr:    proc(visitor: Visitor, node: ^AutoCastExpr),
    visit_binary_expr:       proc(visitor: Visitor, node: ^BinaryExpr),
    visit_array_type_expr:   proc(visitor: Visitor, node: ^ArrayTypeExpr),
    visit_array_index_expr:  proc(visitor: Visitor, node: ^ArrayIndexExpr),
    visit_as_expr:           proc(visitor: Visitor, node: ^AsExpr),
    visit_paren_expr:        proc(visitor: Visitor, node: ^ParenExpr),
    visit_not_expr:          proc(visitor: Visitor, node: ^NotExpr),
}

visit :: proc(visitor: Visitor, node: ^AstNode) {
    switch stmt in node.as {
    case ^FnDeclStmt:
        visitor.visit_fn_decl_stmt(visitor, stmt)
    case ^VarDeclStmt:
        visitor.visit_var_decl_stmt(visitor, stmt)
    case ^ImportDeclStmt:
        visitor.visit_import_decl_stmt(visitor, stmt)
    case ^StructDeclStmt:
        visitor.visit_struct_decl_stmt(visitor, stmt)
    case ^BlockStmt:
        visitor.visit_block_stmt(visitor, stmt)
    case ^ReturnStmt:
        visitor.visit_return_stmt(visitor, stmt)
    case ^AssignStmt:
        visitor.visit_assign_stmt(visitor, stmt)
    case ^IfStmt:
        visitor.visit_if_stmt(visitor, stmt)
    case ^ForStmt:
        visitor.visit_for_stmt(visitor, stmt)
    case ^WhileStmt:
        visitor.visit_while_stmt(visitor, stmt)
    case ^BreakStmt:
        visitor.visit_break_stmt(visitor, stmt)

    case ^VarExpr:
        visitor.visit_var_expr(visitor, stmt)
    case ^FnCallExpr:
        visitor.visit_fn_call_expr(visitor, stmt)
    case ^LiteralExpr:
        visitor.visit_literal_expr(visitor, stmt)
    case ^StructFieldExpr:
        visitor.visit_struct_field_expr(visitor, stmt)
    case ^VarArgsExpr:
        visitor.visit_var_args_expr(visitor, stmt)
    case ^PointerExpr:
        visitor.visit_pointer_expr(visitor, stmt)
    case ^StructInitExpr:
        visitor.visit_struct_init_expr(visitor, stmt)
    case ^AutoCastExpr:
        visitor.visit_auto_cast_expr(visitor, stmt)
    case ^BinaryExpr:
        visitor.visit_binary_expr(visitor, stmt)
    case ^ArrayTypeExpr:
        visitor.visit_array_type_expr(visitor, stmt)
    case ^ArrayIndexExpr:
        visitor.visit_array_index_expr(visitor, stmt)
    case ^AsExpr:
        visitor.visit_as_expr(visitor, stmt)
    case ^ParenExpr:
        visitor.visit_paren_expr(visitor, stmt)
    case ^NotExpr:
        visitor.visit_not_expr(visitor, stmt)
    case ^ExprStmt:
        visit(visitor, stmt.expr)
    case ^Statement, ^DerefExpr:
        fmt.panicf("Not implemented {}", stmt)
    }
}

ModPath :: struct {
    name:             string,
    already_included: bool,
}

TranspileCtx :: struct {
    std_mod_paths:          map[string]ModPath,
    has_main:               bool,

    // write stuff
    write_state:            enum {
        TOP_LEVEL, // foward declarations of functions, variables
        DEFINE, // definitions like typedef
        DECL, // bodys
    },
    top_level:              strings.Builder,
    defines:                strings.Builder,
    decl:                   strings.Builder,
    in_ctx:                 enum {
        STMT,
        AS_ARG,
        AS_VAR_EXPR,
        IN_STRUCT_INIT,
        FOR_INIT,
        TO_CSTR,
    },

    // functions
    curr_fn:                string,
    curr_fn_call:           string,
    curr_fn_call_arg:       int,
    functions_info:         map[string]FunctionInfo,
    curr_struct_init:       string,
    curr_struct_init_field: int,
    structs_info:           map[string]StructInfo,
    vars:                   map[string]string,
    c_fn_info:              ^FunctionInfo,

    // other
    str_id:                 int,

    // global_vars
    global_var:             map[string]ScopeVariable,
}

ScopeVariable :: struct {
    name, type: string,
    is_array:   bool,
}

FunctionInfo :: struct {
    params:          [dynamic]ScopeVariable,
    scope_variables: map[string]ScopeVariable,
    return_type:     string,
}

StructInfo :: struct {
    fields: [dynamic]ScopeVariable,
}

// :collect_type_data
collect_var_decl :: proc(stmt: ^VarDeclStmt) {
    name := stmt.name.lit

    type := stmt.typed ? type_from_expr(stmt.type) : type_from_expr(stmt.expr)
    if stmt.global {
        // FIXME: check is array
        ctx.global_var[name] = {name, type, false}
    } else {
        // FIXME: check is array
        fn_info := &ctx.functions_info[ctx.curr_fn]
        fn_info.scope_variables[name] = {name, type, false}
    }
}

type_for_var_in_fn :: proc(var, fn: string) -> string {
    return ctx.functions_info[fn].scope_variables[var].type
}

// :inference
type_from_expr :: proc(expr: ^Expr) -> string {
    switch e in expr.as_expr {
    case ^NotExpr:
        return "bool"
    case ^VarExpr:
        if e.name.lit in ctx.global_var {
            if e.is_pointer {
                return fmt.tprintf("{}*", ctx.global_var[e.name.lit].type)
            }
            return ctx.global_var[e.name.lit].type
        } else if e.name.lit in ctx.vars {
            if e.is_pointer {
                return fmt.tprintf("{}*", ctx.vars[e.name.lit])
            }
            return ctx.vars[e.name.lit]
        } else if e.name.lit in ctx.functions_info[ctx.curr_fn].scope_variables {
            return ctx.functions_info[ctx.curr_fn].scope_variables[e.name.lit].type
        } else {
            for param in ctx.functions_info[ctx.curr_fn].params {
                if param.name == e.name.lit {
                    return param.type
                }
            }
        }
        return e.name.lit
    case ^LiteralExpr:
        switch e.type {
        case .STRING:
            return "string"
        case .NUMBER:
            return "int"
        case .NUMBER_FLOAT:
            return "float"
        case .BOOL:
            return "bool"
        }
    case ^PointerExpr:
        return fmt.tprintf("{}*", e.type.lit)
    case ^DerefExpr:
        type := type_from_expr(e.expr)
        return fmt.tprintf("*{}", type)
    case ^AutoCastExpr:
        return type_from_expr(e.expr)
    case ^VarArgsExpr:
        return "UPSIE"
    case ^BinaryExpr:
        return type_from_expr(e.lhs)
    case ^ArrayTypeExpr:
        if is_dynamic_array(e.len) {
            type := type_from_expr(e.type)
            return fmt.tprintf("DynamicArray<{}>", type)
        }
        return type_from_expr(e.type)
    case ^ArrayIndexExpr:
        return type_from_expr(e.pre)
    case ^StructInitExpr:
        return e.type.lit
    case ^FnCallExpr:
        ret_type := ctx.functions_info[e.name.lit].return_type
        return ret_type
    case ^StructFieldExpr:
        var_type := type_from_expr(e.who)
        if strings.ends_with(var_type, "*") {
            ok: bool
            var_type, ok = strings.substring(var_type, 0, len(var_type) - 1)
        }
        if var_type in ctx.structs_info {
            struct_ := ctx.structs_info[var_type]
            field := type_from_expr(e.field)
            for v in struct_.fields {
                if field == v.name {
                    return v.type
                }
            }
        }
        return var_type
    case ^AsExpr:
        return "UPSIE"
    case ^ParenExpr:
        return type_from_expr(e.expr)
    case:
        fmt.panicf("Not implemented for {}", e)
    }
    panic("")
}

collect_scope_info :: proc(stmt: ^FnDeclStmt) {
    name := stmt.name.lit
    ctx.functions_info[name] = {}
    fni := &ctx.functions_info[name]
    ctx.c_fn_info = fni
    fni.return_type = type_from_expr(stmt.ret_type)
    ctx.curr_fn = name

    // Collect provided params
    for param in stmt.params {
        fni.scope_variables[stmt.name.lit] = {param.name.lit, type_from_expr(param.type), false}
        append(&fni.params, ScopeVariable{param.name.lit, type_from_expr(param.type), false})
    }

    do_block :: proc(block: ^BlockStmt, fni: ^FunctionInfo) {
        for body_stmt in block.exprs {
            switch expr in body_stmt.as_stmt {
            case ^VarDeclStmt:
                collect_var_decl(expr)
            case ^BlockStmt:
                do_block(expr, fni)
            case ^WhileStmt:
                do_block(expr.block, fni)
            case ^IfStmt:
                // FIXME: doesnt check `else` block
                do_block(expr.block, fni)
            case ^ForStmt:
                // FIXME: check if init is valid var declaration
                collect_var_decl(expr.init.as.(^VarDeclStmt))
                do_block(expr.block, fni)
            case ^FnDeclStmt, ^ImportDeclStmt, ^ExprStmt, ^AssignStmt, ^StructDeclStmt, ^BreakStmt, ^ReturnStmt:
            }
        }
    }

    if .EXTERNAL not_in stmt.flags {
        do_block(stmt.block, fni)
    }
}

collect_struct_info :: proc(stmt: ^StructDeclStmt) {
    name := stmt.name.lit
    si := StructInfo{}

    for field in stmt.fields {
        append(&si.fields, ScopeVariable{field.name.lit, type_from_expr(field.type), false})
    }

    ctx.structs_info[name] = si
}

ctx := TranspileCtx {
    write_state = .DECL,
}

write :: proc(fmt_: string, args: ..any) {
    switch ctx.write_state {
    case .DECL:
        fmt.sbprintf(&ctx.decl, fmt_, ..args)
    case .DEFINE:
        fmt.sbprintf(&ctx.defines, fmt_, ..args)
    case .TOP_LEVEL:
        fmt.sbprintf(&ctx.top_level, fmt_, ..args)
    }
}

STD_ROOT :: #config(STD_ROOT, "V:/dev/lang/std")

collect_std_mods :: proc() -> map[string]ModPath {
    mods, _ := filepath.glob(fmt.tprintf("{}/*.lang", STD_ROOT))
    std_mods := make(map[string]ModPath, context.temp_allocator)
    for mod in mods {
        file := strings.split(filepath.base(mod), ".", context.temp_allocator)[0]
        std_mods[file] = ModPath{mod, false}
    }
    return std_mods
}

// :import

// import "std.lang" <- relative import
// import "std:libc" <- named import
is_named_import :: proc(import_name: string) -> (string, bool) {
    if strings.contains(import_name, ":") && strings.count(import_name, ":") == 1 {
        return strings.split(import_name, ":")[1], true
    }
    return "", false
}

visit_import_decl_stmt :: proc(visitor: Visitor, stmt: ^ImportDeclStmt) {
    // dont to shit
}

// :block_stmt
visit_block_stmt :: proc(visitor: Visitor, stmt: ^BlockStmt) {
    write("{{\n")
    for stmt in stmt.exprs {
        visit(visitor, stmt)
    }
    write("}}\n")
}

// :fn_decl
visit_fn_decl_stmt :: proc(visitor: Visitor, stmt: ^FnDeclStmt) {

    fn_name := stmt.name.lit

    // if the function is "main" set has main and change name to "___entry___"
    if fn_name == "main" {
        ctx.has_main = true
        fn_name = "___entry___"
    }

    // foward declare
    {
        ctx.write_state = .TOP_LEVEL
        if .EXTERNAL in stmt.flags {
            write("extern \"C\" ")
        }
        visit(visitor, stmt.ret_type)
        write(" {}(", fn_name)
        for param, idx in stmt.params {
            visit(visitor, param.type)
            if idx != len(stmt.params) - 1 {
                write(", ")
            }
        }
        write(");\n")
    }

    // implemetation
    if .EXTERNAL not_in stmt.flags {
        ctx.write_state = .DECL
        ctx.curr_fn = stmt.name.lit // set the current function so future expr can lookup function info. 

        visit(visitor, stmt.ret_type)
        write(" {}(", fn_name)
        for param, idx in stmt.params {
            visit(visitor, param.type)
            write(" {}", param.name.lit)
            if idx != len(stmt.params) - 1 {
                write(", ")
            }
        }
        write(")\n")
        visit(visitor, stmt.block)
    }
}

// :var_expr

visit_var_expr :: proc(visitor: Visitor, expr: ^VarExpr) {
    if expr.is_pointer {
        write("&")
    } else if expr.is_deref {
        write("*")
    }
    write("{}", expr.name.lit)
}

// :var_decl
visit_var_decl_stmt :: proc(visitor: Visitor, stmt: ^VarDeclStmt) {
    if stmt.constant {
        write("constexpr ")
    }
    type := type_for_var_in_fn(stmt.name.lit, ctx.curr_fn)
    if type == "" {
        fmt.println(type, stmt.name.lit)
        if stmt.name.lit in ctx.global_var {
            type = ctx.global_var[stmt.name.lit].type
        }
    }
    is_array := ctx.functions_info[ctx.curr_fn].scope_variables[stmt.name.lit].is_array
    if !is_array || strings.starts_with(type, "DynamicArray") {
        write("{} {} = ", type, stmt.name.lit)
    } else {
        write("{} {}[] =", type, stmt.name.lit)
    }
    prev := ctx.in_ctx
    ctx.in_ctx = .AS_VAR_EXPR
    visit(visitor, stmt.expr)
    ctx.in_ctx = prev
    if ctx.in_ctx != .FOR_INIT {
        write(";\n")
    }
}

// :fn_call
visit_fn_call_expr :: proc(visitor: Visitor, expr: ^FnCallExpr) {
    prev := ctx.in_ctx
    ctx.curr_fn_call = expr.name.lit

    switch ctx.in_ctx {
    case .AS_ARG, .AS_VAR_EXPR, .IN_STRUCT_INIT:
        if expr.name.lit == "len" {
            type := type_from_expr(expr.args[0])
            switch type {
            case "string":
                write("len_str(")
                visit(visitor, expr.args[0])
                write(")")
            case:
                fmt.panicf("Not implemented for {}", type)
            }
        } else if expr.name.lit == "c" {
            prevv := ctx.in_ctx
            ctx.in_ctx = .TO_CSTR
            visit(visitor, expr.args[0])
            ctx.in_ctx = prevv
        } else {
            write("{}(", expr.name.lit)
            for arg, idx in expr.args {
                ctx.in_ctx = .AS_ARG
                ctx.curr_fn_call_arg = idx

                visit(visitor, arg)
                ctx.in_ctx = prev
                if idx != len(expr.args) - 1 {
                    write(", ")
                }
            }
            write(")")
        }
    case .STMT:
        if expr.name.lit == "len" {
            type := type_from_expr(expr.args[0])
            switch type {
            case "string":
                write("len_str(")
                visit(visitor, expr.args[0])
                write(");\n")
            case:
                fmt.panicf("Not implemented for {}", type)
            }
        } else {
            write("{}(", expr.name.lit)
            for arg, idx in expr.args {
                ctx.in_ctx = .AS_ARG
                visit(visitor, arg)
                ctx.in_ctx = prev
                if idx != len(expr.args) - 1 {
                    write(", ")
                }
            }
            write(");\n")
        }
    case .FOR_INIT, .TO_CSTR:
        panic("UPSIE")
    }
}

count_str :: proc(lit: string) -> int {
    a := 0
    for c in lit {
        if c != '\\' {
            a += 1
        }
    }
    return a
}

// :lit_str
visit_literal_expr :: proc(visitor: Visitor, expr: ^LiteralExpr) {
    switch expr.type {
    case .STRING:
        len := count_str(expr.lit.lit)
        switch ctx.in_ctx {
        case .AS_VAR_EXPR:
            write("builtin_make_string((void*)\"{}\", {})", expr.lit.lit, len)
        case .STMT, .IN_STRUCT_INIT, .AS_ARG:
            ctx.write_state = .TOP_LEVEL
            write(
                "static string ___str{}___ = builtin_make_string((void*)\"{}\", {});\n",
                ctx.str_id,
                expr.lit.lit,
                len,
            )
            ctx.write_state = .DECL
            write("___str{}___", ctx.str_id)
            ctx.str_id += 1
        case .FOR_INIT:
            panic("UPSIE")
        case .TO_CSTR:
            write("\"{}\"", expr.lit.lit)
        }
    case .NUMBER, .BOOL, .NUMBER_FLOAT:
        write("{}", expr.lit.lit)
    }
}

// :struct_field
visit_struct_field_expr :: proc(visitor: Visitor, expr: ^StructFieldExpr) {
    visit(visitor, expr.who)
    type := type_from_expr(expr.who)
    if strings.ends_with(type, "*") {
        write("->")
    } else {
        write(".")
    }
    visit(visitor, expr.field)
}

// :vargs
visit_var_args_expr :: proc(visitor: Visitor, expr: ^VarArgsExpr) {
    write("...")
}

// :pointer_expr
visit_pointer_expr :: proc(visitor: Visitor, expr: ^PointerExpr) {
    write("{}*", expr.type.lit)
}

// :struct_decl
visit_struct_decl_stmt :: proc(visitor: Visitor, expr: ^StructDeclStmt) {
    ctx.write_state = .DEFINE
    write("struct {} {{\n", expr.name.lit)
    for field in expr.fields {
        visit(visitor, field.type)
        write(" {};\n", field.name.lit)
    }
    write("\n}};\n")
    ctx.write_state = .DECL
}

// :return_expr
visit_return_stmt :: proc(visitor: Visitor, stmt: ^ReturnStmt) {
    write("return ")
    visit(visitor, stmt.expr)
    write(";\n")
}

// :struct_init
visit_struct_init_expr :: proc(visitor: Visitor, expr: ^StructInitExpr) {
    prev := ctx.in_ctx
    ctx.in_ctx = .IN_STRUCT_INIT
    ctx.curr_struct_init = expr.type.lit

    write("{}{{", expr.type.lit)
    for value, idx in expr.value_list {
        ctx.curr_struct_init_field = idx
        visit(visitor, value)
        if idx != len(expr.value_list) - 1 {
            write(", ")
        }
    }
    write("}}")
    ctx.in_ctx = prev
}

// :auto_cast_expr
visit_auto_cast_expr :: proc(visitor: Visitor, expr: ^AutoCastExpr) {
    switch ctx.in_ctx {
    case .STMT:
        type := type_for_var_in_fn(type_from_expr(expr), ctx.curr_fn)
        write("({})", type)
        visit(visitor, expr.expr)
    case .AS_ARG:
        type := ctx.functions_info[ctx.curr_fn_call].params[ctx.curr_fn_call_arg].type
        write("({})", type)
        visit(visitor, expr.expr)
    case .AS_VAR_EXPR:
        panic("")
    case .IN_STRUCT_INIT:
        type := ctx.structs_info[ctx.curr_struct_init].fields[ctx.curr_struct_init_field].type
        write("({})", type)
        visit(visitor, expr.expr)
    case .FOR_INIT, .TO_CSTR:
        panic("UPSIE")
    }
}

// :binary_expr
visit_binary_expr :: proc(visitor: Visitor, expr: ^BinaryExpr) {
    prev := ctx.in_ctx
    ctx.in_ctx = .AS_ARG
    visit(visitor, expr.lhs)
    write("{}", expr.operator.lit)
    visit(visitor, expr.rhs)
    ctx.in_ctx = prev
}

// :assign_stmt
visit_assign_stmt :: proc(visitor: Visitor, stmt: ^AssignStmt) {
    visit(visitor, stmt.lhs)
    if .PLUS in stmt.flags {
        write("+=")
    } else if .MINUS in stmt.flags {
        write("-=")
    } else {
        write("=")
    }
    visit(visitor, stmt.rhs)
    if ctx.in_ctx != .AS_ARG {
        write(";\n")
    }
}

// :array_type
is_dynamic_array :: proc(expr: ^Expr) -> bool {
    if thing, ok := expr.as_expr.(^VarExpr); ok && thing.name.lit == "dynamic" {
        return true
    }
    return false
}

visit_array_type_expr :: proc(visitor: Visitor, expr: ^ArrayTypeExpr) {
    switch ctx.in_ctx {
    case .STMT, .AS_VAR_EXPR:
        if !is_dynamic_array(expr.len) {
            write("{{")
            for value, idx in expr.value_list {
                visit(visitor, value)
                if idx != len(expr.value_list) - 1 {
                    write(", ")
                }
            }
            write("}}")
        } else {
            type := type_from_expr(expr.type)
            write("make_dynamic_array<{}>()", type)
        }
    case .AS_ARG, .IN_STRUCT_INIT, .FOR_INIT, .TO_CSTR:
        fmt.panicf("Not implemented yet {}", ctx.in_ctx)
    }
}

// :array_index
visit_array_index_expr :: proc(visitor: Visitor, expr: ^ArrayIndexExpr) {
    visit(visitor, expr.pre)
    write("[")
    visit(visitor, expr.index)
    write("]")
}

// :if_stmt
visit_if_stmt :: proc(visitor: Visitor, stmt: ^IfStmt) {
    write("if (")
    prev := ctx.in_ctx
    ctx.in_ctx = .AS_ARG
    visit(visitor, stmt.cond)
    ctx.in_ctx = prev
    write(") ")
    visit(visitor, stmt.block)
    if stmt.else_ != nil {
        write("else ")
        visit(visitor, stmt.else_)
    }
}

// :for_stmt
visit_for_stmt :: proc(visitor: Visitor, stmt: ^ForStmt) {
    prev := ctx.in_ctx
    write("for (")
    ctx.in_ctx = .FOR_INIT
    visit(visitor, stmt.init)
    ctx.in_ctx = .AS_ARG
    write(";")
    visit(visitor, stmt.cond)
    write(";")
    visit(visitor, stmt.post)
    write(")")
    ctx.in_ctx = prev
    visit(visitor, stmt.block)
}

// :while_stmt
visit_while_stmt :: proc(visitor: Visitor, stmt: ^WhileStmt) {
    write("while (")
    visit(visitor, stmt.cond)
    write(")")
    visit(visitor, stmt.block)
}

// :break_stmt
visit_break_stmt :: proc(visitor: Visitor, stmt: ^BreakStmt) {
    write("break;")
}

// :as_expr
visit_as_expr :: proc(visitor: Visitor, expr: ^AsExpr) {
    write("(")
    visit(visitor, expr.rhs)
    write(")")
    visit(visitor, expr.lhs)
}

// :parent_expr
visit_paren_expr :: proc(visitor: Visitor, expr: ^ParenExpr) {
    write("(")
    visit(visitor, expr.expr)
    write(")")
}

// :not_expr
visit_not_expr :: proc(visitor: Visitor, expr: ^NotExpr) {
    write("!")

    prev := ctx.in_ctx
    ctx.in_ctx = .AS_ARG
    visit(visitor, expr.expr)
    ctx.in_ctx = prev
}

deal_with_import :: proc(import_path: string) {
    file := FileContext{}
    file.file.file_path = import_path

    ok: bool
    file.file.content, ok = os.read_entire_file(import_path, context.temp_allocator)


    if !tokenize(&file) {
        fmt.panicf("Tok: Failed {}", import_path)
    }

    if !parse(&file) {
        fmt.panicf("Parse: Failed {}", import_path)
    }

    transpile_cpp(file.ast)

    fmt.printfln("Success transpile of {}", import_path)
}

transpile_file :: proc(ast: []^AstNode) {
    ctx.std_mod_paths = collect_std_mods()

    // note(marco) remove this when typedef
    ctx.write_state = .DEFINE
    write("typedef unsigned char uchar;\n")
    write("extern \"C\" void* realloc(void*, long);\n")
    write("template<typename T>\n")
    write("struct DynamicArray {{\n")
    write("T* items;\n")
    write("int len;\n")
    write("int cap;\n")
    write("}};\n")
    write("template<typename T>\n")
    write("DynamicArray<T> make_dynamic_array() {{\n")
    write("return DynamicArray<T>{{nullptr, 0, 256}};\n")
    write("}}\n")
    write("template<typename T>\n")
    write("T at_dynamic_array(DynamicArray<T> arr, int x) {{\n")
    write("return arr.items[x];\n")
    write("}}\n")
    write("template<typename T>\n")
    write("void append_dynamic_array(DynamicArray<T>* arr, T append) {{\n")
    write("if (arr->items == nullptr || arr->len + 1 >= arr->cap) {{\n")
    write("arr->cap = arr->items == nullptr ? arr->cap : arr->cap * 2;\n")
    write("arr->items = (T*)realloc(arr->items, arr->cap);\n")
    write("}}\n")
    write("arr->items[arr->len] = append;\n")
    write("arr->len += 1;\n")
    write("}}\n")
    ctx.write_state = .TOP_LEVEL
    write("template<typename T>\n")
    write("DynamicArray<T> make_dynamic_array();\n")
    write("template<typename T>\n")
    write("void append_dynamic_array(DynamicArray<T>*, T);\n")
    write("template<typename T>\n")
    write("T at_dynamic_array(DynamicArray<T>, int);\n")
    ctx.write_state = .DECL

    transpile_cpp(ast)
}

transpile_cpp :: proc(ast: []^AstNode) {

    visitor := Visitor {
        visit_fn_decl_stmt,
        visit_var_decl_stmt,
        visit_import_decl_stmt,
        visit_block_stmt,
        visit_struct_decl_stmt,
        visit_return_stmt,
        visit_assign_stmt,
        visit_if_stmt,
        visit_for_stmt,
        visit_while_stmt,
        visit_break_stmt,
        // exprs
        visit_var_expr,
        visit_fn_call_expr,
        visit_literal_expr,
        visit_struct_field_expr,
        visit_var_args_expr,
        visit_pointer_expr,
        visit_struct_init_expr,
        visit_auto_cast_expr,
        visit_binary_expr,
        visit_array_type_expr,
        visit_array_index_expr,
        visit_as_expr,
        visit_paren_expr,
        visit_not_expr,
    }

    for node in ast {
        if import_decl, ok := node.as.(^ImportDeclStmt); ok {
            if name, is := is_named_import(import_decl.path.lit); is {
                if !ctx.std_mod_paths[name].already_included {
                    deal_with_import(ctx.std_mod_paths[name].name)
                    val := &ctx.std_mod_paths[name]
                    val.already_included = true
                }
            } else {
                fmt.panicf("Invalid path {}", import_decl.path.lit)
            }
        }
    }


    //    for k, v in ctx.functions_info {
    //        fmt.println(k)
    //        fmt.println("=> ScopesVariables:")
    //        for _, value in v.scope_variables {
    //            fmt.println("    ", value)
    //        }
    //        fmt.println("=> Args:")
    //        for par in v.params {
    //            fmt.println("    ", par)
    //        }
    //    }

    for node in ast {
        if fn_decl, ok := node.as.(^FnDeclStmt); ok {
            collect_scope_info(fn_decl)
        } else if var_decl, var_ok := node.as.(^VarDeclStmt); var_ok {
            collect_var_decl(var_decl)
        } else if struct_decl, is_ok := node.as.(^StructDeclStmt); is_ok {
            collect_struct_info(struct_decl)
        }
    }

    for node in ast {
        visit(visitor, node)
    }

    if ctx.has_main {
        write("int main(int argc, char** argv) {{ ___entry___(); }}")
    }
}
