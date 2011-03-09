
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

function __lwp_mutex_seize_irq_trylock(mutex: plwp_mutex; isr_level: pcuint32): cuint32; inline;

procedure __lwp_mutex_seize(_mutex_t: plwp_mutex; _id: cuint32; _wait: cuint8; _timeout, _level: cuint32); inline;

{$ENDIF}


{$IFDEF OGC_IMPLEMENTATION}

function __lwp_mutex_locked(mutex: plwp_mutex): boolean;
begin
	result := (mutex^.lock = LWP_MUTEX_LOCKED);
end;

function __lwp_mutex_isinheritprio(attrs: plwp_mutex_attr): boolean;
begin
	result := (attrs^.mode = LWP_MUTEX_INHERITPRIO);
end;

function __lwp_mutex_isprioceiling(attrs: plwp_mutex_attr): boolean;
begin
	result := (attrs^.mode = LWP_MUTEX_PRIORITYCEIL);
end;

function __lwp_mutex_ispriority(attrs: plwp_mutex_attr): boolean;
begin
	result := (attrs^.mode = LWP_MUTEX_PRIORITY);
end;

function __lwp_mutex_isfifo(attrs: plwp_mutex_attr): boolean;
begin
	result := (attrs^.mode = LWP_MUTEX_FIFO);
end;

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

function __lwp_mutex_seize_irq_trylock(mutex: plwp_mutex; isr_level: pcuint32): cuint32; inline;
var
	exec: plwp_cntrl;
	level: cuint32;
	prioceiling, priocurr: cuint32;
begin
	level := isr_level^;
	exec := _thr_executing;
	exec^.wait.ret_code := LWP_MUTEX_SUCCESSFUL;
	if not __lwp_mutex_locked(mutex) then 
	begin
		mutex^.lock := LWP_MUTEX_LOCKED;
		mutex^.holder := exec;
		mutex^.nest_cnt := 1;
		if __lwp_mutex_isinheritprio(@(mutex^.atrrs)) or __lwp_mutex_isprioceiling(@(mutex^.atrrs)) then
			inc(exec^.res_cnt);
		if not __lwp_mutex_isprioceiling(@(mutex^.atrrs)) then
		begin
			_CPU_ISR_Restore(level);
			result := 0;
		end;
		begin
			prioceiling := mutex^.atrrs.prioceil;
			priocurr := exec^.cur_prio;
			if (priocurr = prioceiling) then
			begin
				_CPU_ISR_Restore(level);
				result := 0;
			end;
			if (priocurr > prioceiling) then
			begin
				__lwp_thread_dispatchdisable();
				_CPU_ISR_Restore(level);
				__lwp_thread_changepriority(mutex^.holder, mutex^.atrrs.prioceil, 0);
				__lwp_thread_dispatchenable();
				result := 0;
			end;
			exec^.wait.ret_code := LWP_MUTEX_CEILINGVIOL;
			mutex^.nest_cnt := 0;
			dec(exec^.res_cnt);
			_CPU_ISR_Restore(level);
			result := 0;
		end;
		result := 0;
	end;

	if (__lwp_thread_isexec(mutex^.holder)) <> 0 then
	begin
		case mutex^.atrrs.nest_behavior of
			LWP_MUTEX_NEST_ACQUIRE:
			begin
				inc(mutex^.nest_cnt);
				_CPU_ISR_Restore(level);
				result := 0;
			end;
			LWP_MUTEX_NEST_ERROR:
			begin
				exec^.wait.ret_code := LWP_MUTEX_NEST_NOTALLOWED;
				_CPU_ISR_Restore(level);
				result := 0;
			end;
			LWP_MUTEX_NEST_BLOCK:
			begin
			end;
		end;
	end;
	result := 1;
end;

//{$ifdef LIBOGC_INTERNAL}
//{$include libogc/lwp_mutex.inl}
//{$endif}
{$ENDIF}
