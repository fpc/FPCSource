
{$IFDEF OGC_INTERFACE}
const
  LWP_THREADQ_NOTIMEOUT = LWP_WD_NOTIMEOUT;

function __lwp_threadqueue_firstfifo(queue: Plwp_thrqueue): Plwp_cntrl; cdecl; external;

function __lwp_threadqueue_firstpriority(queue: Plwp_thrqueue): Plwp_cntrl; cdecl; external;

procedure __lwp_threadqueue_enqueuefifo(queue: Plwp_thrqueue;
  thethread: Plwp_cntrl; timeout: cuint64); cdecl; external;

function __lwp_threadqueue_dequeuefifo(queue: Plwp_thrqueue): Plwp_cntrl; cdecl; external;

procedure __lwp_threadqueue_enqueuepriority(queue: Plwp_thrqueue;
  thethread: Plwp_cntrl; timeout: cuint64); cdecl; external;

function __lwp_threadqueue_dequeuepriority(queue: Plwp_thrqueue): Plwp_cntrl; cdecl; external;

procedure __lwp_threadqueue_init(queue: Plwp_thrqueue;
  mode, state, timeout_state: cuint32); cdecl; external;

function __lwp_threadqueue_first(queue: Plwp_thrqueue): Plwp_cntrl; cdecl; external;

procedure __lwp_threadqueue_enqueue(queue: Plwp_thrqueue; timeout: cuint64); cdecl; external;

function __lwp_threadqueue_dequeue(queue: Plwp_thrqueue): Plwp_cntrl; cdecl; external;

procedure __lwp_threadqueue_flush(queue: Plwp_thrqueue; status: cuint32); cdecl; external;

procedure __lwp_threadqueue_extract(queue: Plwp_thrqueue; thethread: Plwp_cntrl); cdecl; external;

procedure __lwp_threadqueue_extractfifo(queue: Plwp_thrqueue;
  thethread: Plwp_cntrl); cdecl; external;

procedure __lwp_threadqueue_extractpriority(queue: Plwp_thrqueue;
  thethread: Plwp_cntrl); cdecl; external;

function __lwp_threadqueue_extractproxy(thethread: Plwp_cntrl): cuint32; cdecl; external;


//#ifdef LIBOGC_INTERNAL
//#include <libogc/lwp_threadq.inl>
//#endif

procedure __lwp_threadqueue_csenter(var queue: lwp_thrqueue); inline;
{$ENDIF}

{$IFDEF OGC_IMPLEMENTATION}
procedure __lwp_threadqueue_csenter(var queue: lwp_thrqueue); inline;
begin
  queue.sync_state := LWP_THREADQ_NOTHINGHAPPEND;
end;
{$ENDIF}

