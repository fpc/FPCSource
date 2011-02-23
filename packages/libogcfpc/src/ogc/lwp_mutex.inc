
{$IFDEF OGC_INTERFACE}
const
  LWP_MUTEX_LOCKED = 0;
  LWP_MUTEX_UNLOCKED = 1;
  LWP_MUTEX_NEST_ACQUIRE = 0;
  LWP_MUTEX_NEST_ERROR = 1;
  LWP_MUTEX_NEST_BLOCK = 2;
  LWP_MUTEX_FIFO = 0;
  LWP_MUTEX_PRIORITY = 1;
  LWP_MUTEX_INHERITPRIO = 2;
  LWP_MUTEX_PRIORITYCEIL = 3;
  LWP_MUTEX_SUCCESSFUL = 0;
  LWP_MUTEX_UNSATISFIED_NOWAIT = 1;
  LWP_MUTEX_NEST_NOTALLOWED = 2;
  LWP_MUTEX_NOTOWNER = 3;
  LWP_MUTEX_DELETED = 4;
  LWP_MUTEX_TIMEOUT = 5;
  LWP_MUTEX_CEILINGVIOL = 6;

type
  _lwpmutexattr = record
    mode : cuint32;
    nest_behavior : cuint32;
    prioceil : cuint8;
    onlyownerrelease : cuint8;
  end;
  lwp_mutex_attr = _lwpmutexattr;
  Plwp_mutex_attr = ^lwp_mutex_attr;

  _lwpmutex = record
    wait_queue : lwp_thrqueue;
    atrrs : lwp_mutex_attr;
    lock : cuint32;
    nest_cnt : cuint32;
    blocked_cnt : cuint32;
    holder : Plwp_cntrl;
  end;
  lwp_mutex = _lwpmutex;
  Plwp_mutex = ^lwp_mutex;


procedure __lwp_mutex_initialize(mutex: Plwp_mutex; attrs: Plwp_mutex_attr;
  init_lock: cuint32); cdecl; external;

function __lwp_mutex_surrender(mutex: Plwp_mutex): cuint32; cdecl; external;

procedure __lwp_mutex_seize_irq_blocking(mutex: Plwp_mutex; timeout: cuint64); cdecl; external;

procedure __lwp_mutex_flush(mutex: Plwp_mutex; status: cuint32); cdecl; external;


function __lwp_mutex_seize_irq_trylock(mutex: plwp_mutex; isr_level: pcuint32): cuint32; cdecl; external;

procedure __lwp_mutex_seize(_mutex_t: plwp_mutex; _id: cuint32; _wait: cuint8; _timeout, _level: cuint32); inline;

{$ENDIF}


{$IFDEF OGC_IMPLEMENTATION}
procedure __lwp_mutex_seize(_mutex_t: plwp_mutex; _id: cuint32; _wait: cuint8; _timeout, _level: cuint32); inline;
begin
  repeat
    if (__lwp_mutex_seize_irq_trylock(_mutex_t, @_level))<> 0 then
    begin
			if (_wait = 0) then
      begin
				_CPU_ISR_Restore(_level);
				_thr_executing^.wait.ret_code := LWP_MUTEX_UNSATISFIED_NOWAIT;
			end else
      begin
				__lwp_threadqueue_csenter(_mutex_t^.wait_queue);
				_thr_executing^.wait.queue^ := _mutex_t^.wait_queue;
				_thr_executing^.wait.id := _id;
				__lwp_thread_dispatchdisable();
				_CPU_ISR_Restore(_level);
				__lwp_mutex_seize_irq_blocking(_mutex_t,cuint64(_timeout));
      end;
		end;
	until false;
end;
//{$ifdef LIBOGC_INTERNAL}
//{$include libogc/lwp_mutex.inl}
//{$endif}
{$ENDIF}
