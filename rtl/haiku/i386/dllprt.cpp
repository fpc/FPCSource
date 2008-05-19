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
extern int operatingsystem_parameter_argc;
extern void * operatingsystem_parameter_argv;
extern void * operatingsystem_parameter_envp;

static char * _argv[] = {"dll",0};
static char * _envp[] = {0};

extern "C" void BEGIN()
{
        printf ("init\n");
        operatingsystem_parameter_argc=0;
        operatingsystem_parameter_argv = (void *)_argv;
        operatingsystem_parameter_envp = (void *)_envp;
        PASCALMAIN();
}

FPC_DLL::FPC_DLL()
{
  BEGIN();
}

