unit pthreads;

interface

{$mode objfpc}
{$PACKRECORDS C}

  {
  #define PTHREAD_MUTEX_INITIALIZER {0, 0, 0, PTHREAD_MUTEX_TIMED_NP, __LOCK_INITIALIZER}
  #define PTHREAD_COND_INITIALIZER {__LOCK_INITIALIZER, 0}
   }
  { Values for attributes.   }

Const
  LibThreads = 'pthread';

  PTHREAD_CREATE_JOINABLE = 0;
  PTHREAD_CREATE_DETACHED = 1;

  PTHREAD_INHERIT_SCHED   = 0;
  PTHREAD_EXPLICIT_SCHED  = 1;
 
  PTHREAD_SCOPE_SYSTEM    = 0;
  PTHREAD_SCOPE_PROCESS   = 1;

  NONRECURSIVE  = 0;
  RECURSIVE     = 1;

  PTHREAD_MUTEX_TIMED_NP      = 0;
  PTHREAD_MUTEX_RECURSIVE_NP  = 1;
  PTHREAD_MUTEX_ERRORCHECK_NP = 2;
  PTHREAD_MUTEX_ADAPTIVE_NP   = 3;

  PTHREAD_MUTEX_NORMAL     = PTHREAD_MUTEX_TIMED_NP;
  PTHREAD_MUTEX_RECURSIVE  = PTHREAD_MUTEX_RECURSIVE_NP;
  PTHREAD_MUTEX_ERRORCHECK = PTHREAD_MUTEX_ERRORCHECK_NP;
  PTHREAD_MUTEX_DEFAULT    = PTHREAD_MUTEX_NORMAL;
  PTHREAD_MUTEX_FAST_NP    = PTHREAD_MUTEX_ADAPTIVE_NP;
                                      
  PTHREAD_PROCESS_PRIVATE = 0;
  PTHREAD_PROCESS_SHARED  = 1;
  
  PTHREAD_ONCE_INIT = 0;
  
  PTHREAD_CANCEL_ENABLE  = 0;
  PTHREAD_CANCEL_DISABLE = 1;

  PTHREAD_CANCEL_DEFERRED     = 0;
  PTHREAD_CANCEL_ASYNCHRONOUS = 1;

  PTHREAD_CANCELED = Pointer(-1);
  PTHREAD_BARRIER_SERIAL_THREAD = -1;

  PTHREAD_RWLOCK_PREFER_READER_NP = 0;
  PTHREAD_RWLOCK_PREFER_WRITER_NP = 1;
  PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP = 2; 
  PTHREAD_RWLOCK_DEFAULT_NP = PTHREAD_RWLOCK_PREFER_WRITER_NP;
  
  _SIGSET_NWORDS  = 1024 div (8 * SizeOf(LongWord));
    
type
   __sigset_t = record
     __val: packed array[0.._SIGSET_NWORDS-1] of LongWord;
   end;
   P__sigset_t = ^__sigset_t;
   TSigset = __sigset_t;
   PSigset = ^TSigset;
   
type
  // inserted.
  Size_t = cardinal;
  PSize_t = ^Size_t;
  PCardinal =  ^Cardinal;
  
  // From scheduler.
  
  timespec =  record
    tv_sec: Longint; 
    tv_nsec: Longint; 
  end;
  TTimeSpec = timespec;
  PTimeSpec = ^TTimeSpec;

  // procedural types used in parameters to pthread functions
  
  TStartRoutine = function (_para1:pointer): integer; cdecl;
  TKeyValueDestructor = procedure(ValueInKey: Pointer); cdecl;
  TInitOnceProc = Procedure;cdecl;
  TForkHandler = procedure; cdecl;

  PTHREAD_T  = cardinal;
  TThreadID = pthread_t;
  PPTHREAD_T = ^PTHREAD_T;
  PThreadID = ^TThreadID;
  
  _PTHREAD_DESCR = Pointer;
  TPTHREAD_DESCR = _PTHREAD_DESCR;
  PPTHREAD_DESCR = ^TPTHREAD_DESCR;
  P_PTHREAD_DESCR = ^_PTHREAD_DESCR;
  
  sched_param = record
    sched_priority: Integer;
  end;
  Psched_param = ^sched_param;  
  TSchedParam = sched_param;
  PSchedParam = ^TSchedParam;

  pthread_attr_t = record
    __detachstate,
    __schedpolicy: Integer;
    __schedparam: TSchedParam;
    __inheritsched,
    __scope: Integer;
    __guardsize: Size_T;
    __stackaddr_set: Integer;
    __stackaddr: Pointer;
    __stacksize: Size_T;
  end;
  ppthread_attr_t = ^pthread_attr_t;
  TThreadAttr = pthread_attr_t;
  PThreadAttr = ^TThreadAttr;
  
  _pthread_fastlock = record
    __status: Longint;
    __spinlock: Integer;
  end;
  p_pthread_fastlock = ^_pthread_fastlock; 
  TPthreadFastlock = _pthread_fastlock;
  PPthreadFastlock = ^TPthreadFastlock;
  
  PTHREAD_MUTEX_T =  record
    __m_reserved: Integer;
    __m_count: Integer; 
    __m_owner: _pthread_descr; 
    __m_kind: Integer;   
    __m_lock: _pthread_fastlock;  
  end;
  PPTHREAD_MUTEX_T = ^PTHREAD_MUTEX_T;  
  TPthreadMutex = PTHREAD_MUTEX_T;
  PPthreadMutex = ^TPthreadMutex;
  
  pthread_mutexattr_t = {packed} record
    __mutexkind: Integer;
  end;
  TMutexAttribute = pthread_mutexattr_t;
  PMutexAttribute = ^TMutexAttribute;
  PPTHREAD_MUTEXATTR_T = ^pthread_mutexattr_t;
                   
  TPThreadCleanupRoutine = procedure (_para1:pointer);cdecl;                         
  
  PPthreadCleanupBuffer = ^_pthread_cleanup_buffer;
  p_pthread_cleanup_buffer = ^_pthread_cleanup_buffer;
  _pthread_cleanup_buffer = {packed} record
    __routine: TPThreadCleanupRoutine;    { Function to call.  }
    __arg: Pointer;                       { Its argument.  }
    __canceltype: Integer;                { Saved cancellation type. }
    __prev: p_pthread_Cleanup_Buffer;        { Chaining of cleanup functions.  }
  end;
  TPthreadCleanupBuffer = _pthread_cleanup_buffer;  

  pthread_cond_t = record
    __c_lock: _pthread_fastlock;
    __c_waiting: _pthread_descr;
  end;
  PPthread_cond_t = ^pthread_cond_t;
  TCondVar = pthread_cond_t;
  PCondVar = ^TCondVar;

  pthread_condattr_t = record
    __dummy: Integer;
  end;
  Ppthread_condattr_t = ^pthread_condattr_t;
  TPthreadCondattr = pthread_condattr_t;
  PPthreadCondattr = ^TPthreadCondattr;

  pthread_key_t = Cardinal;
  ppthread_key_t = ^pthread_key_t;
  TPThreadKey = pthread_key_t;
  PPThreadKey = ^TPThreadKey;
       
  pthread_once_t = Integer;
  Ppthread_once_t = ^pthread_once_t;
  TPThreadOnce = pthread_once_t;
  PPThreadOnce = ^pthread_once_t;

  __sem_lock_t = record 
    status: Longint;
    spinlock: Integer;
  end;
  TSemLock = __sem_lock_t;
  PSemLock = ^TSemLock;

  sem_t = record
    __sem_lock: __sem_lock_t;
    __sem_value: Integer;
    __sem_waiting: _pthread_descr;
  end;
  psem_t = ^sem_t;
  TSemaphore = sem_t;
  PSemaphore = ^TSemaphore;

  pthread_rwlock_t = record
    __rw_lock: _pthread_fastlock;
    __rw_readers: Integer;
    __rw_writer: _pthread_descr;
    __rw_read_waiting: _pthread_descr;
    __rw_write_waiting: _pthread_descr;
    __rw_kind: Integer;
    __rw_pshared: Integer;
  end;
  ppthread_rwlock_t = ^pthread_rwlock_t;
  TPthreadRWlock = pthread_rwlock_t;
  PPthreadRWlock = ^TPthreadRWlock;

  pthread_rwlockattr_t = {packed} record
    __lockkind: Integer;
    __pshared: Integer;
  end;
  ppthread_rwlockattr_t = ^pthread_rwlockattr_t;
  TPthreadRWlockAttribute = pthread_rwlockattr_t;
  PPthreadRWlockAttribute = ^TPthreadRWlockAttribute;

  pthread_spinlock_t = Integer;
  ppthread_spinlock_t = ^pthread_spinlock_t;
  TPthreadSpinlock = pthread_spinlock_t;
  PTPthreadSpinlock = ^TPthreadSpinlock;        
        
  pthread_barrier_t = record
    __ba_lock: _pthread_fastlock;
    __ba_required: Integer;
    __ba_present: Integer;
    __ba_waiting: _pthread_descr;
  end;
  ppthread_barrier_t = ^pthread_barrier_t;
  TPthreadBarrier = pthread_barrier_t;
  PPthreadBarrier = ^TPthreadBarrier;

  pthread_barrierattr_t = record
    __pshared: Integer;
  end;
  ppthread_barrierattr_t = ^pthread_barrierattr_t;
  TPthreadBarrierAttribute = pthread_barrierattr_t;
  PPthreadBarrierAttribute = ^TPthreadBarrierAttribute;

{ ---------------------------------------------------------------------
    Raw function prototypes
  ---------------------------------------------------------------------}
  
          
  function pthread_create(__thread:Ppthread_t; __attr:Ppthread_attr_t; __start_routine:TStartRoutine; __arg:pointer):longint;cdecl; external libthreads;
  function pthread_self:pthread_t;cdecl; external libthreads;
  function pthread_equal(__thread1:pthread_t; __thread2:pthread_t):longint;cdecl; external libthreads;
  procedure pthread_exit(__retval:pointer);cdecl; external libthreads;
  function pthread_join(__th:pthread_t; __thread_return:Ppointer):longint;cdecl; external libthreads;
  function pthread_detach(__th:pthread_t):longint;cdecl; external libthreads;
  function pthread_attr_init(__attr:Ppthread_attr_t):longint;cdecl; external libthreads;
  function pthread_attr_destroy(__attr:Ppthread_attr_t):longint;cdecl; external libthreads;
  function pthread_attr_setdetachstate(__attr:Ppthread_attr_t; __detachstate:longint):longint;cdecl; external libthreads;
  function pthread_attr_getdetachstate(__attr:Ppthread_attr_t; __detachstate:Plongint):longint;cdecl; external libthreads;
  function pthread_attr_setschedparam(__attr:Ppthread_attr_t; __param:Psched_param):longint;cdecl; external libthreads;
  function pthread_attr_getschedparam(__attr:Ppthread_attr_t; __param:Psched_param):longint;cdecl; external libthreads;
  function pthread_attr_setschedpolicy(__attr:Ppthread_attr_t; __policy:longint):longint;cdecl; external libthreads;
  function pthread_attr_getschedpolicy(__attr:Ppthread_attr_t; __policy:Plongint):longint;cdecl; external libthreads;
  function pthread_attr_setinheritsched(__attr:Ppthread_attr_t; __inherit:longint):longint;cdecl; external libthreads;
  function pthread_attr_getinheritsched(__attr:Ppthread_attr_t; __inherit:Plongint):longint;cdecl; external libthreads;
  function pthread_attr_setscope(__attr:Ppthread_attr_t; __scope:longint):longint;cdecl; external libthreads;
  function pthread_attr_getscope(__attr:Ppthread_attr_t; __scope:Plongint):longint;cdecl; external libthreads;
  function pthread_attr_setstackaddr(__attr:Ppthread_attr_t; __stackaddr:pointer):longint;cdecl; external libthreads;
  function pthread_attr_getstackaddr(__attr:Ppthread_attr_t; __stackaddr:Ppointer):longint;cdecl; external libthreads;
  function pthread_attr_setstacksize(__attr:Ppthread_attr_t; __stacksize:size_t):longint;cdecl; external libthreads;
  function pthread_attr_getstacksize(__attr:Ppthread_attr_t; __stacksize:Psize_t):longint;cdecl; external libthreads;
  function pthread_attr_getguardsize(__attr:Ppthread_attr_t; Guardsize: PCardinal): Integer; cdecl;external libthreads;
  function pthread_attr_setguardsize(__attr:Ppthread_attr_t; Guardsize: Cardinal): Integer; cdecl;external libthreads;
  function pthread_setschedparam(__target_thread:pthread_t; __policy:longint; __param:Psched_param):longint;cdecl; external libthreads;
  function pthread_getschedparam(__target_thread:pthread_t; __policy:Plongint; __param:Psched_param):longint;cdecl; external libthreads;
  function pthread_getconcurrency: Integer; cdecl;external libthreads;
  function pthread_setconcurrency(Level: Integer): Integer; cdecl;external libthreads;
  function pthread_yield(): Integer; cdecl;external libthreads;
  function pthread_mutex_init(__mutex:Ppthread_mutex_t; __mutex_attr:Ppthread_mutexattr_t):longint;cdecl; external libthreads;
  function pthread_mutex_destroy(__mutex:Ppthread_mutex_t):longint;cdecl; external libthreads;
  function pthread_mutex_trylock(__mutex:Ppthread_mutex_t):longint;cdecl; external libthreads;
  function pthread_mutex_lock(__mutex:Ppthread_mutex_t):longint;cdecl; external libthreads;
  function pthread_mutex_unlock(__mutex:Ppthread_mutex_t):longint;cdecl; external libthreads;
  function pthread_mutexattr_init(__attr:Ppthread_mutexattr_t):longint;cdecl; external libthreads;
  function pthread_mutexattr_destroy(__attr:Ppthread_mutexattr_t):longint;cdecl; external libthreads;
  function pthread_mutexattr_getpshared(__attr:Ppthread_mutexattr_t; __pshared:Plongint):longint;cdecl; external libthreads;
  function pthread_mutexattr_setpshared(__attr:Ppthread_mutexattr_t; __pshared:longint):longint;cdecl; external libthreads;
  function pthread_mutexattr_settype(__attr: Ppthread_mutexattr_t; Kind: Integer): Integer; cdecl;external libthreads;
  function pthread_mutexattr_gettype(__attr: Ppthread_mutexattr_t; var Kind: Integer): Integer; cdecl;external libthreads;
  function pthread_cond_init(__cond:Ppthread_cond_t; __cond_attr:Ppthread_condattr_t):longint;cdecl; external libthreads;
  function pthread_cond_destroy(__cond:Ppthread_cond_t):longint;cdecl; external libthreads;
  function pthread_cond_signal(__cond:Ppthread_cond_t):longint;cdecl; external libthreads;
  function pthread_cond_broadcast(__cond:Ppthread_cond_t):longint;cdecl; external libthreads;
  function pthread_cond_wait(__cond:Ppthread_cond_t; __mutex:Ppthread_mutex_t):longint;cdecl; external libthreads;
  function pthread_cond_timedwait(__cond:Ppthread_cond_t; __mutex:Ppthread_mutex_t; __abstime:Ptimespec):longint;cdecl; external libthreads;
  function pthread_condattr_init(__attr:Ppthread_condattr_t):longint;cdecl; external libthreads;
  function pthread_condattr_destroy(__attr:Ppthread_condattr_t):longint;cdecl; external libthreads;
  function pthread_condattr_getpshared(__attr:Ppthread_condattr_t; __pshared:Plongint):longint;cdecl; external libthreads;
  function pthread_condattr_setpshared(__attr:Ppthread_condattr_t; __pshared:longint):longint;cdecl; external libthreads;
  function pthread_rwlock_init(__rwlock:Ppthread_rwlock_t; __attr:Ppthread_rwlockattr_t):longint;cdecl; external libthreads;
  function pthread_rwlock_destroy(__rwlock:Ppthread_rwlock_t):longint;cdecl;external libthreads;
  function pthread_rwlock_rdlock(__rwlock:Ppthread_rwlock_t):longint;cdecl;external libthreads;
  function pthread_rwlock_tryrdlock(__rwlock:Ppthread_rwlock_t):longint;cdecl;external libthreads;
  function pthread_rwlock_timedrdlock(__rwlock:Ppthread_rwlock_t; __abstime:Ptimespec):longint;cdecl;external libthreads;
  function pthread_rwlock_wrlock(__rwlock:Ppthread_rwlock_t):longint;cdecl;external libthreads;
  function pthread_rwlock_trywrlock(__rwlock:Ppthread_rwlock_t):longint;cdecl; external libthreads;
  function pthread_rwlock_timedwrlock(__rwlock:Ppthread_rwlock_t; __abstime:Ptimespec):longint;cdecl;external libthreads;
  function pthread_rwlock_unlock(__rwlock:Ppthread_rwlock_t):longint;cdecl;external libthreads;
  function pthread_rwlockattr_init(__attr:Ppthread_rwlockattr_t):longint;cdecl;external libthreads;
  function pthread_rwlockattr_destroy(__attr:Ppthread_rwlockattr_t):longint;cdecl;external libthreads;
  function pthread_rwlockattr_getpshared(__attr:Ppthread_rwlockattr_t; __pshared:Plongint):longint;cdecl;external libthreads;
  function pthread_rwlockattr_setpshared(__attr:Ppthread_rwlockattr_t; __pshared:longint):longint;cdecl;external libthreads;
  function pthread_rwlockattr_getkind_np(__attr:Ppthread_rwlockattr_t; __pref:Plongint):longint;cdecl;external libthreads;
  function pthread_rwlockattr_setkind_np(__attr:Ppthread_rwlockattr_t; __pref:longint):longint;cdecl;external libthreads;
  function pthread_spin_init(__lock:Ppthread_spinlock_t; __pshared:longint):longint;cdecl;external libthreads;
  function pthread_spin_destroy(__lock:Ppthread_spinlock_t):longint;cdecl;external libthreads;
  function pthread_spin_lock(__lock:Ppthread_spinlock_t):longint;cdecl;external libthreads;
  function pthread_spin_trylock(__lock:Ppthread_spinlock_t):longint;cdecl;external libthreads;
  function pthread_spin_unlock(__lock:Ppthread_spinlock_t):longint;cdecl;external libthreads;
  function pthread_barrier_init(__barrier:Ppthread_barrier_t; __attr:Ppthread_barrierattr_t; __count:dword):longint;cdecl;external libthreads;
  function pthread_barrier_destroy(__barrier:Ppthread_barrier_t):longint;cdecl;external libthreads;
  function pthread_barrierattr_init(__attr:Ppthread_barrierattr_t):longint;cdecl;external libthreads;
  function pthread_barrierattr_destroy(__attr:Ppthread_barrierattr_t):longint;cdecl;external libthreads;
  function pthread_barrierattr_getpshared(__attr:Ppthread_barrierattr_t; __pshared:Plongint):longint;cdecl;external libthreads;
  function pthread_barrierattr_setpshared(__attr:Ppthread_barrierattr_t; __pshared:longint):longint;cdecl;external libthreads;
  function pthread_barrier_wait(__barrier:Ppthread_barrier_t):longint;cdecl;external libthreads;
  function pthread_key_create(__key:Ppthread_key_t; __destr_function :TKeyValueDestructor):longint;cdecl; external libthreads;
  function pthread_key_delete(__key:pthread_key_t):longint;cdecl; external libthreads;
  function pthread_setspecific(__key:pthread_key_t; __pointer:pointer):longint;cdecl; external libthreads;
  function pthread_getspecific(__key:pthread_key_t):pointer;cdecl; external libthreads;
  function pthread_once(__once_control:Ppthread_once_t; __init_routine:Tprocedure ):longint;cdecl; external libthreads;
  function pthread_setcancelstate(__state:longint; __oldstate:Plongint):longint;cdecl; external libthreads;
  function pthread_setcanceltype(__type:longint; __oldtype:Plongint):longint;cdecl; external libthreads;
  function pthread_cancel(__thread:pthread_t):longint;cdecl; external libthreads;
  procedure pthread_testcancel;cdecl; external libthreads;
  procedure _pthread_cleanup_push(__buffer:P_pthread_cleanup_buffer; __routine:TPthreadCleanupRoutine; __arg:pointer);cdecl; external libthreads;
  procedure _pthread_cleanup_pop(__buffer:P_pthread_cleanup_buffer; __execute:longint);cdecl; external libthreads;
  function pthread_atfork(__prepare:TforkHandler ; __parent:TForkHandler ; __child: TForkHandler ):longint;cdecl; external libthreads;
  procedure pthread_kill_other_threads_np;cdecl; external libthreads;
  procedure __pthread_initialize; cdecl; external libthreads;
  function pthread_sigmask(__how:longint; __newmask:P__sigset_t; __oldmask:P__sigset_t):longint;cdecl; external libthreads;
  function pthread_kill(__thread:pthread_t; __signo:longint):longint;cdecl; external libthreads;

  function sem_init(__sem:Psem_t; __pshared:longint; __value:dword):longint;cdecl;external libthreads;
  function sem_destroy(__sem:Psem_t):longint;cdecl;external libthreads;
  function sem_close(__sem:Psem_t):longint;cdecl;external libthreads;
  function sem_unlink(__name:Pchar):longint;cdecl;external libthreads;
  function sem_wait(__sem:Psem_t):longint;cdecl;external libthreads;
  function sem_trywait(__sem:Psem_t):longint;cdecl;external libthreads;
  function sem_post(__sem:Psem_t):longint;cdecl;external libthreads;
  function sem_getvalue(__sem:Psem_t; __sval:Plongint):longint;cdecl;external libthreads;

{ ---------------------------------------------------------------------
     Overloaded versions with var args instead of pointers
  ---------------------------------------------------------------------}
              
  function pthread_create(var __thread:pthread_t; var __attr: pthread_attr_t; __start_routine:TStartRoutine; __arg:pointer):longint;cdecl; external libthreads;
  function pthread_join(__th:pthread_t; var __thread_return:pointer):longint;cdecl; external libthreads;
  function pthread_attr_init(var __attr: pthread_attr_t):longint;cdecl; external libthreads;
  function pthread_attr_destroy(var __attr: pthread_attr_t):longint;cdecl; external libthreads;
  function pthread_attr_setdetachstate(var __attr: pthread_attr_t; __detachstate:longint):longint;cdecl; external libthreads;
  function pthread_attr_getdetachstate(var __attr: pthread_attr_t; var __detachstate:longint):longint;cdecl; external libthreads;
  function pthread_attr_setschedparam(var __attr: pthread_attr_t; const __param: sched_param):longint;cdecl; external libthreads;
  function pthread_attr_getschedparam(var __attr: pthread_attr_t; var __param: sched_param):longint;cdecl; external libthreads;
  function pthread_attr_setschedpolicy(var __attr: pthread_attr_t; __policy:longint):longint;cdecl; external libthreads;
  function pthread_attr_getschedpolicy(var __attr: pthread_attr_t; var __policy:longint):longint;cdecl; external libthreads;
  function pthread_attr_setinheritsched(var __attr: pthread_attr_t; __inherit:longint):longint;cdecl; external libthreads;
  function pthread_attr_getinheritsched(var __attr: pthread_attr_t; var __inherit: longint):longint;cdecl; external libthreads;
  function pthread_attr_setscope(var __attr: pthread_attr_t; __scope:longint):longint;cdecl; external libthreads;
  function pthread_attr_getscope(var __attr: pthread_attr_t; var __scope: longint):longint;cdecl; external libthreads;
  function pthread_attr_setstackaddr(var __attr: pthread_attr_t; __stackaddr:pointer):longint;cdecl; external libthreads;
  function pthread_attr_getstackaddr(var __attr: pthread_attr_t; var __stackaddr:pointer):longint;cdecl; external libthreads;
  function pthread_attr_setstacksize(var __attr: pthread_attr_t; __stacksize:size_t):longint;cdecl; external libthreads;
  function pthread_attr_getstacksize(var __attr: pthread_attr_t; var __stacksize: size_t):longint;cdecl; external libthreads;
  function pthread_attr_getguardsize(var __attr: pthread_attr_t; var Guardsize: Cardinal): Integer; cdecl;external libthreads;
  function pthread_attr_setguardsize(var __attr: pthread_attr_t; Guardsize: Cardinal): Integer; cdecl;external libthreads;
  function pthread_setschedparam(__target_thread:pthread_t; __policy:longint; const __param: sched_param):longint;cdecl; external libthreads;
  function pthread_getschedparam(__target_thread:pthread_t; var __policy: longint; var __param: sched_param):longint;cdecl; external libthreads;
  function pthread_mutex_init(var __mutex: pthread_mutex_t; var __mutex_attr: pthread_mutexattr_t):longint;cdecl; external libthreads;
  function pthread_mutex_destroy(var __mutex: pthread_mutex_t):longint;cdecl; external libthreads;
  function pthread_mutex_trylock(var __mutex: pthread_mutex_t):longint;cdecl; external libthreads;
  function pthread_mutex_lock(var __mutex: pthread_mutex_t):longint;cdecl; external libthreads;
  function pthread_mutex_unlock(var __mutex: pthread_mutex_t):longint;cdecl; external libthreads;
  function pthread_mutexattr_init(var __attr: pthread_mutexattr_t):longint;cdecl; external libthreads;
  function pthread_mutexattr_destroy(var __attr: pthread_mutexattr_t):longint;cdecl; external libthreads;
  function pthread_mutexattr_getpshared(var __attr: pthread_mutexattr_t; var __pshared: longint):longint;cdecl; external libthreads;
  function pthread_mutexattr_setpshared(var __attr: pthread_mutexattr_t; __pshared:longint):longint;cdecl; external libthreads;
  function pthread_mutexattr_settype(var __attr: pthread_mutexattr_t; Kind: Integer): Integer; cdecl;external libthreads;
  function pthread_mutexattr_gettype(var __attr: pthread_mutexattr_t; var Kind: Integer): Integer; cdecl;external libthreads;
  function pthread_cond_init(var __cond: pthread_cond_t;var __cond_attr: pthread_condattr_t):longint;cdecl; external libthreads;
  function pthread_cond_destroy(var __cond: pthread_cond_t):longint;cdecl; external libthreads;
  function pthread_cond_signal(var __cond: pthread_cond_t):longint;cdecl; external libthreads;
  function pthread_cond_broadcast(var __cond: pthread_cond_t):longint;cdecl; external libthreads;
  function pthread_cond_wait(var __cond: pthread_cond_t; var __mutex: pthread_mutex_t):longint;cdecl; external libthreads;
  function pthread_cond_timedwait(var __cond: pthread_cond_t; var __mutex: pthread_mutex_t; var __abstime: timespec):longint;cdecl; external libthreads;
  function pthread_condattr_init(var __attr: pthread_condattr_t):longint;cdecl; external libthreads;
  function pthread_condattr_destroy(var __attr: pthread_condattr_t):longint;cdecl; external libthreads;
  function pthread_condattr_getpshared(var __attr: pthread_condattr_t; var __pshared:longint):longint;cdecl; external libthreads;
  function pthread_condattr_setpshared(var __attr: pthread_condattr_t; __pshared:longint):longint;cdecl; external libthreads;
  function pthread_rwlock_init(var __rwlock: pthread_rwlock_t; var __attr: pthread_rwlockattr_t):longint;cdecl; external libthreads;
  function pthread_rwlock_destroy(var __rwlock: pthread_rwlock_t):longint;cdecl;external libthreads;
  function pthread_rwlock_rdlock(var __rwlock: pthread_rwlock_t):longint;cdecl;external libthreads;
  function pthread_rwlock_tryrdlock(var __rwlock: pthread_rwlock_t):longint;cdecl;external libthreads;
  function pthread_rwlock_timedrdlock(var __rwlock: pthread_rwlock_t; __abstime:Ptimespec):longint;cdecl;external libthreads;
  function pthread_rwlock_wrlock(var __rwlock: pthread_rwlock_t):longint;cdecl;external libthreads;
  function pthread_rwlock_trywrlock(var __rwlock: pthread_rwlock_t):longint;cdecl; external libthreads;
  function pthread_rwlock_timedwrlock(var __rwlock: pthread_rwlock_t; __abstime:Ptimespec):longint;cdecl;external libthreads;
  function pthread_rwlock_unlock(var __rwlock: pthread_rwlock_t):longint;cdecl;external libthreads;
  function pthread_rwlockattr_init(var __attr: pthread_rwlockattr_t):longint;cdecl;external libthreads;
  function pthread_rwlockattr_destroy(var __attr: pthread_rwlockattr_t):longint;cdecl;external libthreads;
  function pthread_rwlockattr_getpshared(var __attr: pthread_rwlockattr_t; var __pshared: longint):longint;cdecl;external libthreads;
  function pthread_rwlockattr_setpshared(var __attr: pthread_rwlockattr_t; __pshared:longint):longint;cdecl;external libthreads;
  function pthread_rwlockattr_getkind_np(var __attr: pthread_rwlockattr_t; var __pref: longint):longint;cdecl;external libthreads;
  function pthread_rwlockattr_setkind_np(var __attr: pthread_rwlockattr_t; __pref:longint):longint;cdecl;external libthreads;
  function pthread_spin_init(var __lock: pthread_spinlock_t; __pshared:longint):longint;cdecl;external libthreads;
  function pthread_spin_destroy(var __lock: pthread_spinlock_t):longint;cdecl;external libthreads;
  function pthread_spin_lock(var __lock: pthread_spinlock_t):longint;cdecl;external libthreads;
  function pthread_spin_trylock(var __lock: pthread_spinlock_t):longint;cdecl;external libthreads;
  function pthread_spin_unlock(var __lock: pthread_spinlock_t):longint;cdecl;external libthreads;
  function pthread_barrier_init(var __barrier: pthread_barrier_t;var __attr: pthread_barrierattr_t; __count:dword):longint;cdecl;external libthreads;
  function pthread_barrier_destroy(var __barrier: pthread_barrier_t):longint;cdecl;external libthreads;
  function pthread_barrierattr_init(var __attr: pthread_barrierattr_t):longint;cdecl;external libthreads;
  function pthread_barrierattr_destroy(var __attr: pthread_barrierattr_t):longint;cdecl;external libthreads;
  function pthread_barrierattr_getpshared(var __attr: pthread_barrierattr_t; var __pshared:longint):longint;cdecl;external libthreads;
  function pthread_barrierattr_setpshared(var __attr: pthread_barrierattr_t; __pshared:longint):longint;cdecl;external libthreads;
  function pthread_barrier_wait(var __barrier: pthread_barrier_t):longint;cdecl;external libthreads;
  function pthread_key_create(var __key: pthread_key_t; __destr_function :TKeyValueDestructor):longint;cdecl; external libthreads;
  function pthread_once(var __once_control: pthread_once_t; __init_routine:Tprocedure ):longint;cdecl; external libthreads;
  function pthread_setcancelstate(__state:longint; var __oldstate:longint):longint;cdecl; external libthreads;
  function pthread_setcanceltype(__type:longint;var __oldtype:longint):longint;cdecl; external libthreads;

  procedure _pthread_cleanup_push(var __buffer: _pthread_cleanup_buffer; __routine:TPthreadCleanupRoutine; __arg:pointer);cdecl; external libthreads;
  procedure _pthread_cleanup_pop(var __buffer:_pthread_cleanup_buffer; __execute:longint);cdecl; external libthreads;
  function pthread_sigmask(__how:longint; var __newmask:__sigset_t; var __oldmask:__sigset_t):longint;cdecl; external libthreads;

  function sem_init(var __sem: sem_t; __pshared:longint; __value:dword):longint;cdecl;external libthreads;
  function sem_destroy(var __sem: sem_t):longint;cdecl;external libthreads;
  function sem_close(var __sem: sem_t):longint;cdecl;external libthreads;
  function sem_wait(var __sem: sem_t):longint;cdecl;external libthreads;
  function sem_trywait(var __sem: sem_t):longint;cdecl;external libthreads;
  function sem_post(var __sem: sem_t):longint;cdecl;external libthreads;
  function sem_getvalue(var __sem: sem_t; var __sval:longint):longint;cdecl;external libthreads;
                
implementation

end.
