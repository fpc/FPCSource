{$IFDEF OGC_INTERFACE}
//#define _LWPTHREADS_DEBUG
const
  LWP_TIMESLICE_TIMER_ID = $00070040;

const
  LWP_CPU_BUDGET_ALGO_NONE = 0;
  LWP_CPU_BUDGET_ALGO_TIMESLICE = 1;
type
  lwp_cpu_budget_algorithms = Integer;


type
  _lwpwaitinfo = record
    id : cuint32;
    cnt : cuint32;
    ret_arg : pointer;
    ret_arg_1 : pointer;
    option : cuint32;
    ret_code : cuint32;
    block2n : lwp_queue;
    queue : Plwp_thrqueue;
  end;
  lwp_waitinfo = _lwpwaitinfo;


type
 Tentry = function(par0: pointer): pointer; cdecl;

_lwpcntrl = record
    object_ : lwp_obj;
    cur_prio : cuint8;
    real_prio : cuint8;
    suspendcnt : cuint32;
    res_cnt : cuint32;
    isr_level : cuint32;
    cur_state : cuint32;
    cpu_time_budget : cuint32;
    budget_algo : lwp_cpu_budget_algorithms;
    is_preemptible : cbool;
    wait : lwp_waitinfo;
    priomap : prio_cntrl;
    timer : wd_cntrl;
    entry : TEntry;
    arg : pointer;
    stack : pointer;
    stack_size : cuint32;
    stack_allocated : cuint8;
    ready : Plwp_queue;
    join_list : lwp_thrqueue;
    context : frame_context;  //16
    libc_reent : pointer;
  end;
  lwp_cntrl = _lwpcntrl;
  lwp_cntrl_t = ^_lwpcntrl;
  plwp_cntrl = ^_lwpcntrl;


var
  _thr_main : Plwp_cntrl; external;
  _thr_idle : Plwp_cntrl; external;
  _thr_executing : Plwp_cntrl; external;
  _thr_heir : Plwp_cntrl; external;
  _thr_allocated_fp : Plwp_cntrl; external;
  _context_switch_want : cuint32; external;
  _thread_dispatch_disable_level : cuint32; external;
  _lwp_wd_timeslice : wd_cntrl; external;
  __lwp_thr_libc_reent : Ppointer; external;
  _lwp_thr_ready: array [0..0] of lwp_queue; external;

procedure __thread_dispatch; cdecl; external;

procedure __lwp_thread_yield; cdecl; external;

procedure __lwp_thread_closeall; cdecl; external;

procedure __lwp_thread_setstate(par0: Plwp_cntrl; par1: cuint32); cdecl; external;

procedure __lwp_thread_clearstate(par0: Plwp_cntrl; par1: cuint32); cdecl; external;

procedure __lwp_thread_changepriority(par0: Plwp_cntrl; par1, par2: cuint32); cdecl; external;

procedure __lwp_thread_setpriority(par0: Plwp_cntrl; par1: cuint32); cdecl; external;

procedure __lwp_thread_settransient(par0: Plwp_cntrl); cdecl; external;

procedure __lwp_thread_suspend(par0: Plwp_cntrl); cdecl; external;

procedure __lwp_thread_resume(par0: Plwp_cntrl; par1: cuint32); cdecl; external;

procedure __lwp_thread_loadenv(par0: Plwp_cntrl); cdecl; external;

procedure __lwp_thread_ready(par0: Plwp_cntrl); cdecl; external;

function __lwp_thread_init(par0: Plwp_cntrl; par1: pointer;
  par2, par3, par4: cuint32; par5: cbool): cuint32; cdecl; external;

function __lwp_thread_start(par0: Plwp_cntrl; par1: TEntry;
  par2: pointer): cuint32; cdecl; external;

procedure __lwp_thread_exit(par0: pointer); cdecl; external;

procedure __lwp_thread_close(par0: Plwp_cntrl); cdecl; external;

procedure __lwp_thread_startmultitasking; cdecl; external;

type
  TExitFunc = procedure; cdecl;
procedure __lwp_thread_stopmultitasking(exitfunc: TExitFunc); cdecl; external;

function __lwp_thread_getobject(par0: Plwp_cntrl): Plwp_obj; cdecl; external;

function __lwp_evaluatemode: cuint32; cdecl; external;


function __lwp_isr_in_progress: cuint32; cdecl; external;

procedure __lwp_thread_resettimeslice; cdecl; external;

procedure __lwp_rotate_readyqueue(par0: cuint32); cdecl; external;

procedure __lwp_thread_delayended(par0: pointer); cdecl; external;

procedure __lwp_thread_tickle_timeslice(par0: pointer); cdecl; external;


//#ifdef LIBOGC_INTERNAL
//#include <libogc/lwp_threads.inl>
//#endif

procedure __lwp_thread_dispatchdisable(); inline;

{$ENDIF}

{$IFDEF OGC_IMPLEMENTATION}
function __lwp_thread_isexec(thethread: plwp_cntrl): cuint32;
begin
	result := 0;
  if (thethread = _thr_executing) then result := 1;
end;

function __lwp_thread_isheir(thethread: plwp_cntrl): cuint32;
begin
	result := 0;
  if (thethread = _thr_heir) then result := 1;
end;

procedure __lwp_thread_calcheir();
begin
	_thr_heir := plwp_cntrl(_lwp_thr_ready[__lwp_priomap_highest()].first);
//#ifdef _LWPTHREADS_DEBUG
//	printf("__lwp_thread_calcheir(%p)\n",_thr_heir);
//#endif
end;

function __lwp_thread_isallocatedfp(thethread: plwp_cntrl): cuint32;
begin
	result := 0;
  if (thethread = _thr_allocated_fp) then result := 1;
end;

procedure __lwp_thread_deallocatefp();
begin
	_thr_allocated_fp := nil;
end;

procedure __lwp_thread_dispatchinitialize();
begin
	_thread_dispatch_disable_level := 1;
end;

procedure __lwp_thread_dispatchenable();
begin
  dec(_thread_dispatch_disable_level);
	if (_thread_dispatch_disable_level = 0) then
		__thread_dispatch();
end;

procedure __lwp_thread_dispatchdisable();
begin
	inc(_thread_dispatch_disable_level);
end;

procedure __lwp_thread_dispatchunnest();
begin
	dec(_thread_dispatch_disable_level);
end;

procedure __lwp_thread_unblock(thethread: plwp_cntrl);
begin
	__lwp_thread_clearstate(thethread, LWP_STATES_BLOCKED);
end;

function __lwp_thread_getlibcreent(): pointer;
begin
	result := __lwp_thr_libc_reent;
end;

procedure __lwp_thread_setlibcreent(libc_reent: pointer);
begin
	__lwp_thr_libc_reent := libc_reent;
end;

function __lwp_thread_isswitchwant(): cbool;
begin
	result := cbool(_context_switch_want);
end;

function __lwp_thread_isdispatchenabled(): cbool;
begin
	result := (_thread_dispatch_disable_level = 0);
end;

procedure __lwp_thread_inittimeslice();
begin
	__lwp_wd_initialize(@_lwp_wd_timeslice, @__lwp_thread_tickle_timeslice, LWP_TIMESLICE_TIMER_ID, nil);
end;

procedure __lwp_thread_starttimeslice();
begin
	__lwp_wd_insert_ticks(@_lwp_wd_timeslice, millisecs_to_ticks(1));
end;

procedure __lwp_thread_stoptimeslice();
begin
	__lwp_wd_remove_ticks(@_lwp_wd_timeslice);
end;

//procedure __lwp_thread_dispatchdisable(); inline;
//begin
//  inc(_thread_dispatch_disable_level);
//end;
{$ENDIF}

