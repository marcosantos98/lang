typedef const char* cstr;
extern "C" int puts(cstr);
void ___entry___();
void ___entry___(){
puts("Hello, World");
}
int main(int argc, char** argv) {___entry___();}