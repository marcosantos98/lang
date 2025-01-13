package main

string_definitions :: `
struct String {
    void* data;
    int len;
};
static String ___empty_string___ = {(void*)"", 0};

String make_string(const char* cstr, int len) {
    return {(void*)cstr, len};
}

int len_str(String s) {
    return s.len;
}
`
