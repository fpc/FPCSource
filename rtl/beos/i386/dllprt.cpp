#include <stdio.h>

class FPC_DLL
{
  public:
    FPC_DLL();
//    ~FPC_DLL();
};

static FPC_DLL fpc_dll();

//FPC_DLL::~FPC_DLL()
//{
//      printf ("main thread ended.");
//}


extern "C" void PASCALMAIN(void);
extern int U_SYSBEOS_ARGC;
extern void * U_SYSBEOS_ARGV;
extern void * U_SYSBEOS_ENVP;

static char * _argv[] = {"dll",0};
static char * _envp[] = {0};

extern "C" void BEGIN()
{
        printf ("init\n");
        U_SYSBEOS_ARGC=0;
        U_SYSBEOS_ARGV = (void *)_argv;
        U_SYSBEOS_ENVP = (void *)_envp;
        PASCALMAIN();
}

FPC_DLL::FPC_DLL()
{
  BEGIN();
}

