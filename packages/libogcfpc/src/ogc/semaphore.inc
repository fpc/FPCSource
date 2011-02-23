
{$IFDEF OGC_INTERFACE}
const
  LWP_SEM_NULL = $ffffffff;

type
  sem_t = cuint32;
  Psem_t = ^sem_t;

function LWP_SemInit(sem: Psem_t; start, max: cuint32): cint32; cdecl; external;
function LWP_SemDestroy(sem: sem_t): cint32; cdecl; external;
function LWP_SemWait(sem: sem_t): cint32; cdecl; external;
function LWP_SemPost(sem: sem_t): cint32; cdecl; external;
{$ENDIF OGC_INTERFACE}

