{$IFDEF OGC_INTERFACE}
const
  LWP_COND_NULL = $ffffffff;

type
  cond_t = cuint32;
  Pcond_t = ^cond_t;

  function LWP_CondInit(cond: Pcond_t): cint32; cdecl; external;


function LWP_CondWait(cond: cond_t; mutex: mutex_t): cint32; cdecl; external;


function LWP_CondSignal(cond: cond_t): cint32; cdecl; external;


function LWP_CondBroadcast(cond: cond_t): cint32; cdecl; external;

function LWP_CondTimedWait(cond: cond_t; mutex: mutex_t; abstime: Ptimespec)
  : cint32; cdecl; external;

function LWP_CondDestroy(cond: cond_t): cint32; cdecl; external;
{$ENDIF}



