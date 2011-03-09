
{$IFDEF OGC_INTERFACE}
const
  LWP_SEMA_MODEFIFO = 0;
  LWP_SEMA_MODEPRIORITY = 1;
  LWP_SEMA_SUCCESSFUL = 0;
  LWP_SEMA_UNSATISFIED_NOWAIT = 1;
  LWP_SEMA_DELETED = 2;
  LWP_SEMA_TIMEOUT = 3;
  LWP_SEMA_MAXCNT_EXCEEDED = 4;

type
  _lwpsemattr = record
    max_cnt : cuint32;
    mode : cuint32;
  end;
  lwp_semattr = _lwpsemattr;
  Plwp_semattr = ^lwp_semattr;


  _lwpsema = record
    wait_queue : lwp_thrqueue;
    attrs : lwp_semattr;
    count : cuint32;
  end;
  lwp_sema = _lwpsema;
  Plwp_sema = ^lwp_sema;


procedure __lwp_sema_initialize(sema: Plwp_sema; attrs: Plwp_semattr;
  init_count: cuint32); cdecl; external;

function __lwp_sema_surrender(sema: Plwp_sema; id: cuint32): cuint32; cdecl; external;

function __lwp_sema_seize(sema: Plwp_sema; id, wait: cuint32; timeout: cuint64)
  : cuint32; cdecl; external;

procedure __lwp_sema_flush(sema: Plwp_sema; status: cuint32); cdecl; external;


//#ifdef LIBOGC_INTERNAL
//#include <libogc/lwp_sema.inl>
//#endif
{$ENDIF}

