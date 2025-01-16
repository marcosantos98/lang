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
}

visit :: proc(visitor: Visitor, node: ^AstNode) {
    #partial switch stmt in node.as {
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
    case ^ExprStmt:
        visit(visitor, stmt.expr)
    case:
        fmt.panicf("Not implemented {}", stmt)
    }
}

TranspileCtx :: struct {
    std_mod_paths:          map[string]string,
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

    // other
    str_id:                 int,
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
    fields: [dynamic]string,
}

type_for_var_in_fn :: proc(var, fn: string) -> string {
    return ctx.functions_info[fn].scope_variables[var].type
}

type_from_expr :: proc(expr: ^Expr) -> string {
    #partial switch e in expr.as_expr {
    case ^VarExpr:
        if e.name.lit in ctx.vars {
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
        case .BOOL:
            return "bool"
        }
    case ^PointerExpr:
        return fmt.tprintf("{}*", e.type.lit)
    case ^AutoCastExpr:
        return type_from_expr(e.expr)
    case ^VarArgsExpr:
        return "UPSIE"
    case ^BinaryExpr:
        return type_from_expr(e.lhs)
    case ^ArrayTypeExpr:
        return type_from_expr(e.type)
    case ^ArrayIndexExpr:
        return type_from_expr(e.pre)
    case ^StructInitExpr:
        return e.type.lit
    case:
        fmt.panicf("Not implemented for {}", e)
    }
    panic("")
}

collect_scope_info :: proc(stmt: ^FnDeclStmt) {
    name := stmt.name.lit
    fni := FunctionInfo{}
    fni.return_type = type_from_expr(stmt.ret_type)
    ctx.curr_fn = name

    // Collect provided params
    for param in stmt.params {
        fni.scope_variables[stmt.name.lit] = {param.name.lit, type_from_expr(param.type), false}
        append(&fni.params, ScopeVariable{param.name.lit, type_from_expr(param.type), false})
    }

    ctx.functions_info[name] = fni

    do_block :: proc(block: ^BlockStmt, fni: ^FunctionInfo) {
        for body_stmt in block.exprs {
            if var_decl, ok := body_stmt.as.(^VarDeclStmt); ok {
                var_name := var_decl.name.lit
                if var_decl.typed {
                    ctx.vars[var_name] = type_from_expr(var_decl.type)
                    fni.scope_variables[var_name] = {var_name, type_from_expr(var_decl.type), false}
                } else {
                    _, is_array := var_decl.expr.as_expr.(^ArrayTypeExpr)
                    ctx.vars[var_name] = type_from_expr(var_decl.expr)
                    fni.scope_variables[var_name] = {var_name, type_from_expr(var_decl.expr), is_array}
                }
            } else if for_stmt, is_ok := body_stmt.as.(^ForStmt); is_ok {
                // FIXME: doesnt check body
                if var, var_ok := for_stmt.init.as.(^VarDeclStmt); var_ok {
                    init_name := var.name.lit
                    if var.typed {
                        ctx.vars[init_name] = type_from_expr(var.type)
                        fni.scope_variables[init_name] = {init_name, type_from_expr(var.type), false}
                    } else {
                        _, is_array := var.expr.as_expr.(^ArrayTypeExpr)
                        ctx.vars[init_name] = type_from_expr(var.expr)
                        fni.scope_variables[init_name] = {init_name, type_from_expr(var.expr), is_array}
                    }
                }
            } else if if_stmt, if_ok := body_stmt.as.(^IfStmt); if_ok {
                do_block(if_stmt.block, fni)
                if if_stmt.else_ != nil {
                    // FIXME: this doesnt check if else is another if
                    if elsee, else_ok := if_stmt.else_.as.(^BlockStmt); else_ok {
                        do_block(elsee, fni)
                    }
                }
            }
        }
    }

    if .EXTERNAL not_in stmt.flags {
        do_block(stmt.block, &fni)
    }

    ctx.functions_info[name] = fni
}

collect_struct_info :: proc(stmt: ^StructDeclStmt) {
    name := stmt.name.lit
    si := StructInfo{}

    for field in stmt.fields {
        append(&si.fields, type_from_expr(field.type))
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

collect_std_mods :: proc() -> map[string]string {
    mods, _ := filepath.glob(fmt.tprintf("{}/*.lang", STD_ROOT))
    std_mods := make(map[string]string, context.temp_allocator)
    for mod in mods {
        file := strings.split(filepath.base(mod), ".", context.temp_allocator)[0]
        std_mods[file] = mod
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
    type := type_for_var_in_fn(stmt.name.lit, ctx.curr_fn)
    is_array := ctx.functions_info[ctx.curr_fn].scope_variables[stmt.name.lit].is_array
    if !is_array {
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
    case .FOR_INIT:
        panic("UPSIE")
    }
}

// :lit_str
visit_literal_expr :: proc(visitor: Visitor, expr: ^LiteralExpr) {
    switch expr.type {
    case .STRING:
        switch ctx.in_ctx {
        case .AS_VAR_EXPR:
            write("builtin_make_string((void*)\"{}\", {})", expr.lit.lit, len(expr.lit.lit))
        case .STMT, .IN_STRUCT_INIT, .AS_ARG:
            ctx.write_state = .TOP_LEVEL
            write(
                "static string ___str{}___ = builtin_make_string((void*)\"{}\", {});\n",
                ctx.str_id,
                expr.lit.lit,
                len(expr.lit.lit),
            )
            ctx.write_state = .DECL
            write("___str{}___", ctx.str_id)
            ctx.str_id += 1
        case .FOR_INIT:
            panic("UPSIE")
        }
    case .NUMBER, .BOOL:
        write("{}", expr.lit.lit)
    }
}

// :struct_field
visit_struct_field_expr :: proc(visitor: Visitor, expr: ^StructFieldExpr) {
    visit(visitor, expr.who)
    write(".")
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
        type := ctx.structs_info[ctx.curr_struct_init].fields[ctx.curr_struct_init_field]
        write("({})", type)
        visit(visitor, expr.expr)
    case .FOR_INIT:
        panic("UPSIE")
    }
}

// :binary_expr
visit_binary_expr :: proc(visitor: Visitor, expr: ^BinaryExpr) {
    visit(visitor, expr.lhs)
    write("{}", expr.operator.lit)
    visit(visitor, expr.rhs)
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
visit_array_type_expr :: proc(visitor: Visitor, expr: ^ArrayTypeExpr) {
    switch ctx.in_ctx {
    case .STMT, .AS_VAR_EXPR:
        write("{{")
        for value, idx in expr.value_list {
            visit(visitor, value)
            if idx != len(expr.value_list) - 1 {
                write(", ")
            }
        }
        write("}}")
    case .AS_ARG, .IN_STRUCT_INIT, .FOR_INIT:
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
    visit(visitor, stmt.cond)
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
}

transpile_file :: proc(ast: []^AstNode) {
    ctx.std_mod_paths = collect_std_mods()

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
    }

    for node in ast {
        if import_decl, ok := node.as.(^ImportDeclStmt); ok {
            if name, is := is_named_import(import_decl.path.lit); is {
                deal_with_import(ctx.std_mod_paths[name])
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
