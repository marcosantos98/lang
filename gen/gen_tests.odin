package main

import "core:fmt"
import format "core:odin/doc-format"
import "core:os"
import "core:path/filepath"
import "core:strings"

main :: proc() {

    files, _ := filepath.glob("./tests/*.lang", context.temp_allocator)

    builder := strings.builder_make()
    strings.write_string(&builder, "package main\n")
    strings.write_string(&builder, "\n")
    strings.write_string(&builder, "import \"core:testing\"\n")
    strings.write_string(&builder, "import \"core:os\"\n")
    strings.write_string(&builder, "import \"core:path/filepath\"\n")
    strings.write_string(&builder, "import \"core:fmt\"\n")
    strings.write_string(&builder, "import \"core:strings\"\n")
    strings.write_string(&builder, "\n")
    strings.write_string(&builder, "\n")

    for f in files {
        s := filepath.stem(f)
        ff, _ := filepath.to_slash(f)
        strings.write_string(&builder, "@(test)\n")
        fmt.sbprintf(&builder, "translation_{} :: proc(t: ^testing.T) {{\n", s)
        fmt.sbprintf(&builder, "    f := \"{}\"\n", ff)
        fmt.sbprintf(&builder, "    cpp := \"./tests/{}.cpp.out\"\n", s)
        fmt.sbprintf(
            &builder,
            "    testing.expectf(t, os.exists(cpp), \"{{}} not recorded. {{}} not found\", f, cpp)\n",
        )
        fmt.sbprintf(&builder, "    file := FileContext{{}}\n")
        fmt.sbprintf(&builder, "    file.file.file_path = f\n")
        //
        fmt.sbprintf(
            &builder,
            "    testing.expectf(t, do_all_passes_on_file(&file, false), \"Failed translation for {{}}\", f)\n",
        )
        //
        fmt.sbprintf(&builder, "   source := fmt.tprintf(\n")
        fmt.sbprintf(&builder, "    \"{{}}\\n{{}}\\n{{}}\\n\",\n")
        fmt.sbprintf(&builder, "    strings.to_string(file.transpiler.defines),\n")
        fmt.sbprintf(&builder, "    strings.to_string(file.transpiler.top_level),\n")
        fmt.sbprintf(&builder, "    strings.to_string(file.transpiler.decl),\n")
        fmt.sbprintf(&builder, "    )\n")
        fmt.sbprintf(&builder, "    read, _ := os.read_entire_file(cpp)\n")
        fmt.sbprintf(&builder, "    read_str := string(read[:])\n")
        fmt.sbprintf(&builder, "    defer delete(read_str)\n")
        fmt.sbprintf(&builder, "    testing.expectf(\n")
        fmt.sbprintf(&builder, "    t,\n")
        fmt.sbprintf(&builder, "    source == read_str,\n")
        fmt.sbprintf(
            &builder,
            "    \"\\n========== TRANSLATE {{}} ========\\n{{}}==========READ {{}} ==========\\n{{}}\",\n",
        )
        fmt.sbprintf(&builder, "    f,\n")
        fmt.sbprintf(&builder, "    source,\n")
        fmt.sbprintf(&builder, "    cpp,\n")
        fmt.sbprintf(&builder, "    read_str,\n")
        fmt.sbprintf(&builder, "    )\n")

        fmt.sbprintf(&builder, "    strings.builder_destroy(&file.transpiler.defines)\n")
        fmt.sbprintf(&builder, "    strings.builder_destroy(&file.transpiler.decl)\n")
        fmt.sbprintf(&builder, "    strings.builder_destroy(&file.transpiler.top_level)\n")

        fmt.sbprintf(&builder, "}\n")
        fmt.sbprintf(&builder, "\n")

    }

    os.write_entire_file("tests.odin", transmute([]u8)strings.to_string(builder))
}
