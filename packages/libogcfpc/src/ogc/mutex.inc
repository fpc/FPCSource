
{$IFDEF OGC_INTERFACE}
const
  LWP_MUTEX_NULL = $ffffffff;
type
  mutex_t = cuint32;
  Pmutex_t = ^mutex_t;

function LWP_MutexInit(mutex: Pmutex_t; use_recursive: cbool): cint32; cdecl; external;

function LWP_MutexDestroy(mutex: mutex_t): cint32; cdecl; external;

function LWP_MutexLock(mutex: mutex_t): cint32; cdecl; external;

function LWP_MutexTryLock(mutex: mutex_t): cint32; cdecl; external;

function LWP_MutexUnlock(mutex: mutex_t): cint32; cdecl; external;
{$ENDIF OGC_INTERFACE}

