#include <dlfcn.h>
#include <string.h>

int main()
{
    void *lib;
    char *s;
    int FromPos, ToPos;
    char* (*SubStr)(const char*, int*, int*);
    printf("arh %d\n",RTLD_LAZY);
    lib = dlopen("./libcaseudf.so", RTLD_LAZY);
    printf("Result from dlopen (library handle): 0x%08x\n", lib);
    SubStr = dlsym(lib, "SUBSTR");
    printf("Address of SubStr = 0x%08x, last error code = %i\n",
      SubStr, dlerror());

    s = strdup("Test");
    FromPos = 2;
    ToPos = 3;
    printf("Result from SubStr: '%s'\n", (*SubStr)(s, &FromPos, &ToPos));
    dlclose(lib);
    return 0;
}
