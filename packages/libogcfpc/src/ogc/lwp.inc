{$IFDEF OGC_INTERFACE}
const
  LWP_CLOSED = - 1;
  LWP_SUCCESSFUL = 0;
  LWP_ALREADY_SUSPENDED = 1;
  LWP_NOT_SUSPENDED = 2;
  LWP_PRIO_IDLE = 0;
  LWP_PRIO_HIGHEST = 127;
  LWP_THREAD_NULL = $ffffffff;
  LWP_TQUEUE_NULL = $ffffffff;

type
  lwp_t = cuint32;
  Plwp_t = ^lwp_t;

  lwpq_t = cuint32;
  Plwpq_t = ^lwpq_t;

  TAuxCallback = function(par0: pointer): pointer; cdecl;

function LWP_CreateThread(thethread: Plwp_t; entry: TAuxCallback;
  arg, stackbase: pointer; stack_size: cuint32; prio: cuint8): cint32; cdecl; external;


function LWP_SuspendThread(thethread: lwp_t): cint32; cdecl; external;

function LWP_ResumeThread(thethread: lwp_t): cint32; cdecl; external;

function LWP_ThreadIsSuspended(thethread: lwp_t): cbool; cdecl; external;

function LWP_GetSelf: lwp_t; cdecl; external;


procedure LWP_SetThreadPriority(thethread: lwp_t; prio: cuint32); cdecl; external;

procedure LWP_YieldThread; cdecl; external;

procedure LWP_Reschedule(prio: cuint32); cdecl; external;

function LWP_JoinThread(thethread: lwp_t; value_ptr: Ppointer): cint32; cdecl; external;

function LWP_InitQueue(thequeue: Plwpq_t): cint32; cdecl; external;

procedure LWP_CloseQueue(thequeue: lwpq_t); cdecl; external;

function LWP_ThreadSleep(thequeue: lwpq_t): cint32; cdecl; external;

procedure LWP_ThreadSignal(thequeue: lwpq_t); cdecl; external;

procedure LWP_ThreadBroadcast(thequeue: lwpq_t); cdecl; external;
{$ENDIF OGC_INTERFACE}

