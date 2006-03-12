{
  Automatically converted by H2Pas 1.0.0 from pthread.h
  The following command line parameters were used:
    pthread.h
    -D
    -c
    -s
}

const
  External_library='c'; {Setup as you need}


Type
P_cleanup_t  = ^_cleanup_t;
Pclockid_t  = ^clockid_t;
Plongint  = ^longint;
Ppthread_attr_t  = ^pthread_attr_t;
Ppthread_barrier_t  = ^pthread_barrier_t;
Ppthread_barrierattr_t  = ^pthread_barrierattr_t;
Ppthread_cond_t  = ^pthread_cond_t;
Ppthread_condattr_t  = ^pthread_condattr_t;
Ppthread_key_t  = ^pthread_key_t;

Ppthread_mutex_t  = ^pthread_mutex_t;
TPthreadMutex = pthread_mutex_t;

Ppthread_mutexattr_t  = ^pthread_mutexattr_t;
Ppthread_once_t  = ^pthread_once_t;
Ppthread_rwlock_t  = ^pthread_rwlock_t;
Ppthread_rwlockattr_t  = ^pthread_rwlockattr_t;
Ppthread_spinlock_t  = ^pthread_spinlock_t;
Ppthread_t  = ^pthread_t;
Psched_param  = ^sched_param;
Psize_t  = ^size_t;
Ptimespec  = ^timespec;

   _cleanup = record
        pthread_cleanup_pad : array[0..3] of uintptr_t;
     end;
   _cleanup_t = _cleanup;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


const
   PTHREAD_CREATE_DETACHED = $40;   
   PTHREAD_CREATE_JOINABLE = 0;   


   PTHREAD_SCOPE_SYSTEM = $01;   
   PTHREAD_SCOPE_PROCESS = 0;   


   PTHREAD_INHERIT_SCHED = 1;   
   PTHREAD_EXPLICIT_SCHED = 0;   


   PTHREAD_PROCESS_SHARED = 1;   

   PTHREAD_PROCESS_PRIVATE = 0;   
   _DEFAULT_TYPE = PTHREAD_PROCESS_PRIVATE;   

const
   DEFAULT_TYPE = _DEFAULT_TYPE;   


const
   PTHREAD_MUTEX_NORMAL = $0;   
   PTHREAD_MUTEX_ERRORCHECK = $2;   
   PTHREAD_MUTEX_RECURSIVE = $4;   
   PTHREAD_MUTEX_DEFAULT = PTHREAD_MUTEX_NORMAL;   

   PTHREAD_PRIO_NONE = $0;   
   PTHREAD_PRIO_INHERIT = $10;   
   PTHREAD_PRIO_PROTECT = $20;   

   PTHREAD_MUTEX_STALL_NP = $0;   
   PTHREAD_MUTEX_ROBUST_NP = $40;   


   PTHREAD_CANCEL_ENABLE = $00;   
   PTHREAD_CANCEL_DISABLE = $01;   
   PTHREAD_CANCEL_DEFERRED = $00;   
   PTHREAD_CANCEL_ASYNCHRONOUS = $02;   
   PTHREAD_CANCELED = pointer(-(19));



const
   PTHREAD_ONCE_NOTDONE = 0;   
   PTHREAD_ONCE_DONE = 1;   

   PTHREAD_BARRIER_SERIAL_THREAD = -(2);   

type
   _Voidfp = procedure (_para1:pointer);cdecl;
  _fp = procedure;cdecl;
  P_Voidfp = ^_Voidfp;


procedure __pthread_cleanup_push(_para1:_Voidfp; _para2:pointer; _para3:caddr_t; _para4:P_cleanup_t);cdecl;external External_library name '__pthread_cleanup_push';
procedure __pthread_cleanup_pop(_para1:longint; _para2:P_cleanup_t);cdecl;external External_library name '__pthread_cleanup_pop';
function _getfp:caddr_t;cdecl;external External_library name '_getfp';

function pthread_atfork(_para1:_fp ; _para2:_fp ; _para3:_fp ):longint;cdecl;external External_library name 'pthread_atfork';
function pthread_attr_init(_para1:Ppthread_attr_t):longint;cdecl;external External_library name 'pthread_attr_init';
function pthread_attr_destroy(_para1:Ppthread_attr_t):longint;cdecl;external External_library name 'pthread_attr_destroy';
function pthread_attr_setstack(_para1:Ppthread_attr_t; _para2:pointer; _para3:size_t):longint;cdecl;external External_library name 'pthread_attr_setstack';
(* Const before type ignored *)
function pthread_attr_getstack(_RESTRICT_KYWD1:Ppthread_attr_t; _RESTRICT_KYWD2:Ppointer; _RESTRICT_KYWD3:Psize_t):longint;cdecl;external External_library name 'pthread_attr_getstack';
function pthread_attr_setstacksize(_para1:Ppthread_attr_t; _para2:size_t):longint;cdecl;external External_library name 'pthread_attr_setstacksize';
(* Const before type ignored *)
function pthread_attr_getstacksize(_RESTRICT_KYWD1:Ppthread_attr_t; _RESTRICT_KYWD2:Psize_t):longint;cdecl;external External_library name 'pthread_attr_getstacksize';
function pthread_attr_setstackaddr(_para1:Ppthread_attr_t; _para2:pointer):longint;cdecl;external External_library name 'pthread_attr_setstackaddr';
(* Const before type ignored *)
function pthread_attr_getstackaddr(_RESTRICT_KYWD1:Ppthread_attr_t; _RESTRICT_KYWD2:Ppointer):longint;cdecl;external External_library name 'pthread_attr_getstackaddr';
function pthread_attr_setdetachstate(_para1:Ppthread_attr_t; _para2:longint):longint;cdecl;external External_library name 'pthread_attr_setdetachstate';
(* Const before type ignored *)
function pthread_attr_getdetachstate(_para1:Ppthread_attr_t; _para2:Plongint):longint;cdecl;external External_library name 'pthread_attr_getdetachstate';
function pthread_attr_setscope(_para1:Ppthread_attr_t; _para2:longint):longint;cdecl;external External_library name 'pthread_attr_setscope';
(* Const before type ignored *)
function pthread_attr_getscope(_RESTRICT_KYWD1:Ppthread_attr_t; _RESTRICT_KYWD2:Plongint):longint;cdecl;external External_library name 'pthread_attr_getscope';
function pthread_attr_setinheritsched(_para1:Ppthread_attr_t; _para2:longint):longint;cdecl;external External_library name 'pthread_attr_setinheritsched';
(* Const before type ignored *)
function pthread_attr_getinheritsched(_RESTRICT_KYWD1:Ppthread_attr_t; _RESTRICT_KYWD2:Plongint):longint;cdecl;external External_library name 'pthread_attr_getinheritsched';
function pthread_attr_setschedpolicy(_para1:Ppthread_attr_t; _para2:longint):longint;cdecl;external External_library name 'pthread_attr_setschedpolicy';
(* Const before type ignored *)
function pthread_attr_getschedpolicy(_RESTRICT_KYWD1:Ppthread_attr_t; _RESTRICT_KYWD2:Plongint):longint;cdecl;external External_library name 'pthread_attr_getschedpolicy';
(* Const before type ignored *)
function pthread_attr_setschedparam(_RESTRICT_KYWD1:Ppthread_attr_t; _RESTRICT_KYWD2:Psched_param):longint;cdecl;external External_library name 'pthread_attr_setschedparam';
(* Const before type ignored *)
function pthread_attr_getschedparam(_RESTRICT_KYWD1:Ppthread_attr_t; _RESTRICT_KYWD2:Psched_param):longint;cdecl;external External_library name 'pthread_attr_getschedparam';
(* Const before type ignored *)
function pthread_create(_RESTRICT_KYWD1:Ppthread_t; _RESTRICT_KYWD2:Ppthread_attr_t; _para3:P_Voidfp ; _RESTRICT_KYWD3:pointer):longint;cdecl;external External_library name 'pthread_create';
function pthread_once(_para1:Ppthread_once_t; _para2:_fp ):longint;cdecl;external External_library name 'pthread_once';
function pthread_join(_para1:pthread_t; _para2:Ppointer):longint;cdecl;external External_library name 'pthread_join';
function pthread_detach(_para1:pthread_t):longint;cdecl;external External_library name 'pthread_detach';
procedure pthread_exit(_para1:pointer);cdecl;external External_library name 'pthread_exit';
function pthread_cancel(_para1:pthread_t):longint;cdecl;external External_library name 'pthread_cancel';
(* Const before type ignored *)
function pthread_setschedparam(_para1:pthread_t; _para2:longint; _para3:Psched_param):longint;cdecl;external External_library name 'pthread_setschedparam';
function pthread_getschedparam(_para1:pthread_t; _RESTRICT_KYWD1:Plongint; _RESTRICT_KYWD2:Psched_param):longint;cdecl;external External_library name 'pthread_getschedparam';
function pthread_setschedprio(_para1:pthread_t; _para2:longint):longint;cdecl;external External_library name 'pthread_setschedprio';
function pthread_setcancelstate(_para1:longint; _para2:Plongint):longint;cdecl;external External_library name 'pthread_setcancelstate';
function pthread_setcanceltype(_para1:longint; _para2:Plongint):longint;cdecl;external External_library name 'pthread_setcanceltype';
procedure pthread_testcancel;cdecl;external External_library name 'pthread_testcancel';
function pthread_equal(_para1:pthread_t; _para2:pthread_t):longint;cdecl;external External_library name 'pthread_equal';
function pthread_key_create(_para1:Ppthread_key_t; _para2:_Voidfp):longint;cdecl;external External_library name 'pthread_key_create';
function pthread_key_delete(_para1:pthread_key_t):longint;cdecl;external External_library name 'pthread_key_delete';
(* Const before type ignored *)
function pthread_setspecific(_para1:pthread_key_t; _para2:pointer):longint;cdecl;external External_library name 'pthread_setspecific';
function pthread_getspecific(_para1:pthread_key_t):pointer;cdecl;external External_library name 'pthread_getspecific';
function pthread_self:pthread_t;cdecl;external External_library name 'pthread_self';

function pthread_mutexattr_init(_para1:Ppthread_mutexattr_t):longint;cdecl;external External_library name 'pthread_mutexattr_init';
function pthread_mutexattr_destroy(_para1:Ppthread_mutexattr_t):longint;cdecl;external External_library name 'pthread_mutexattr_destroy';
function pthread_mutexattr_setpshared(_para1:Ppthread_mutexattr_t; _para2:longint):longint;cdecl;external External_library name 'pthread_mutexattr_setpshared';
(* Const before type ignored *)
function pthread_mutexattr_getpshared(_RESTRICT_KYWD1:Ppthread_mutexattr_t; _RESTRICT_KYWD2:Plongint):longint;cdecl;external External_library name 'pthread_mutexattr_getpshared';
function pthread_mutexattr_setprotocol(_para1:Ppthread_mutexattr_t; _para2:longint):longint;cdecl;external External_library name 'pthread_mutexattr_setprotocol';
(* Const before type ignored *)
function pthread_mutexattr_getprotocol(_RESTRICT_KYWD1:Ppthread_mutexattr_t; _RESTRICT_KYWD2:Plongint):longint;cdecl;external External_library name 'pthread_mutexattr_getprotocol';
function pthread_mutexattr_setprioceiling(_para1:Ppthread_mutexattr_t; _para2:longint):longint;cdecl;external External_library name 'pthread_mutexattr_setprioceiling';
(* Const before type ignored *)
function pthread_mutexattr_getprioceiling(_RESTRICT_KYWD1:Ppthread_mutexattr_t; _RESTRICT_KYWD2:Plongint):longint;cdecl;external External_library name 'pthread_mutexattr_getprioceiling';
function pthread_mutexattr_setrobust_np(_para1:Ppthread_mutexattr_t; _para2:longint):longint;cdecl;external External_library name 'pthread_mutexattr_setrobust_np';
(* Const before type ignored *)
function pthread_mutexattr_getrobust_np(_RESTRICT_KYWD1:Ppthread_mutexattr_t; _RESTRICT_KYWD2:Plongint):longint;cdecl;external External_library name 'pthread_mutexattr_getrobust_np';
(* Const before type ignored *)
function pthread_mutex_init(_RESTRICT_KYWD1:Ppthread_mutex_t; _RESTRICT_KYWD2:Ppthread_mutexattr_t):longint;cdecl;external External_library name 'pthread_mutex_init';
function pthread_mutex_consistent_np(_para1:Ppthread_mutex_t):longint;cdecl;external External_library name 'pthread_mutex_consistent_np';
function pthread_mutex_destroy(_para1:Ppthread_mutex_t):longint;cdecl;external External_library name 'pthread_mutex_destroy';
function pthread_mutex_lock(_para1:Ppthread_mutex_t):longint;cdecl;external External_library name 'pthread_mutex_lock';
(* Const before type ignored *)
function pthread_mutex_timedlock(_RESTRICT_KYWD1:Ppthread_mutex_t; _RESTRICT_KYWD2:Ptimespec):longint;cdecl;external External_library name 'pthread_mutex_timedlock';
(* Const before type ignored *)
function pthread_mutex_reltimedlock_np(_RESTRICT_KYWD1:Ppthread_mutex_t; _RESTRICT_KYWD2:Ptimespec):longint;cdecl;external External_library name 'pthread_mutex_reltimedlock_np';
function pthread_mutex_unlock(_para1:Ppthread_mutex_t):longint;cdecl;external External_library name 'pthread_mutex_unlock';
function pthread_mutex_trylock(_para1:Ppthread_mutex_t):longint;cdecl;external External_library name 'pthread_mutex_trylock';
function pthread_mutex_setprioceiling(_RESTRICT_KYWD1:Ppthread_mutex_t; _para2:longint; _RESTRICT_KYWD2:Plongint):longint;cdecl;external External_library name 'pthread_mutex_setprioceiling';
(* Const before type ignored *)
function pthread_mutex_getprioceiling(_RESTRICT_KYWD1:Ppthread_mutex_t; _RESTRICT_KYWD2:Plongint):longint;cdecl;external External_library name 'pthread_mutex_getprioceiling';
function pthread_condattr_init(_para1:Ppthread_condattr_t):longint;cdecl;external External_library name 'pthread_condattr_init';
function pthread_condattr_destroy(_para1:Ppthread_condattr_t):longint;cdecl;external External_library name 'pthread_condattr_destroy';
function pthread_condattr_setclock(_para1:Ppthread_condattr_t; _para2:clockid_t):longint;cdecl;external External_library name 'pthread_condattr_setclock';
(* Const before type ignored *)
function pthread_condattr_getclock(_RESTRICT_KYWD1:Ppthread_condattr_t; _RESTRICT_KYWD2:Pclockid_t):longint;cdecl;external External_library name 'pthread_condattr_getclock';
function pthread_condattr_setpshared(_para1:Ppthread_condattr_t; _para2:longint):longint;cdecl;external External_library name 'pthread_condattr_setpshared';
(* Const before type ignored *)
function pthread_condattr_getpshared(_RESTRICT_KYWD1:Ppthread_condattr_t; _RESTRICT_KYWD2:Plongint):longint;cdecl;external External_library name 'pthread_condattr_getpshared';
(* Const before type ignored *)
function pthread_cond_init(_RESTRICT_KYWD1:Ppthread_cond_t; _RESTRICT_KYWD2:Ppthread_condattr_t):longint;cdecl;external External_library name 'pthread_cond_init';
function pthread_cond_destroy(_para1:Ppthread_cond_t):longint;cdecl;external External_library name 'pthread_cond_destroy';
function pthread_cond_broadcast(_para1:Ppthread_cond_t):longint;cdecl;external External_library name 'pthread_cond_broadcast';
function pthread_cond_signal(_para1:Ppthread_cond_t):longint;cdecl;external External_library name 'pthread_cond_signal';
function pthread_cond_wait(_RESTRICT_KYWD1:Ppthread_cond_t; _RESTRICT_KYWD2:Ppthread_mutex_t):longint;cdecl;external External_library name 'pthread_cond_wait';
(* Const before type ignored *)
function pthread_cond_timedwait(_RESTRICT_KYWD1:Ppthread_cond_t; _RESTRICT_KYWD2:Ppthread_mutex_t; _RESTRICT_KYWD3:Ptimespec):longint;cdecl;external External_library name 'pthread_cond_timedwait';
(* Const before type ignored *)
function pthread_cond_reltimedwait_np(_RESTRICT_KYWD1:Ppthread_cond_t; _RESTRICT_KYWD2:Ppthread_mutex_t; _RESTRICT_KYWD3:Ptimespec):longint;cdecl;external External_library name 'pthread_cond_reltimedwait_np';
(* Const before type ignored *)
function pthread_attr_getguardsize(_RESTRICT_KYWD1:Ppthread_attr_t; _RESTRICT_KYWD2:Psize_t):longint;cdecl;external External_library name 'pthread_attr_getguardsize';
function pthread_attr_setguardsize(_para1:Ppthread_attr_t; _para2:size_t):longint;cdecl;external External_library name 'pthread_attr_setguardsize';
function pthread_getconcurrency:longint;cdecl;external External_library name 'pthread_getconcurrency';
function pthread_setconcurrency(_para1:longint):longint;cdecl;external External_library name 'pthread_setconcurrency';
function pthread_mutexattr_settype(_para1:Ppthread_mutexattr_t; _para2:longint):longint;cdecl;external External_library name 'pthread_mutexattr_settype';
(* Const before type ignored *)
function pthread_mutexattr_gettype(_RESTRICT_KYWD1:Ppthread_mutexattr_t; _RESTRICT_KYWD2:Plongint):longint;cdecl;external External_library name 'pthread_mutexattr_gettype';
(* Const before type ignored *)
function pthread_rwlock_init(_RESTRICT_KYWD1:Ppthread_rwlock_t; _RESTRICT_KYWD2:Ppthread_rwlockattr_t):longint;cdecl;external External_library name 'pthread_rwlock_init';
function pthread_rwlock_destroy(_para1:Ppthread_rwlock_t):longint;cdecl;external External_library name 'pthread_rwlock_destroy';
function pthread_rwlock_rdlock(_para1:Ppthread_rwlock_t):longint;cdecl;external External_library name 'pthread_rwlock_rdlock';
(* Const before type ignored *)
function pthread_rwlock_timedrdlock(_RESTRICT_KYWD1:Ppthread_rwlock_t; _RESTRICT_KYWD2:Ptimespec):longint;cdecl;external External_library name 'pthread_rwlock_timedrdlock';
(* Const before type ignored *)
function pthread_rwlock_reltimedrdlock_np(_RESTRICT_KYWD1:Ppthread_rwlock_t; _RESTRICT_KYWD2:Ptimespec):longint;cdecl;external External_library name 'pthread_rwlock_reltimedrdlock_np';
function pthread_rwlock_tryrdlock(_para1:Ppthread_rwlock_t):longint;cdecl;external External_library name 'pthread_rwlock_tryrdlock';
function pthread_rwlock_wrlock(_para1:Ppthread_rwlock_t):longint;cdecl;external External_library name 'pthread_rwlock_wrlock';
(* Const before type ignored *)
function pthread_rwlock_timedwrlock(_RESTRICT_KYWD1:Ppthread_rwlock_t; _RESTRICT_KYWD2:Ptimespec):longint;cdecl;external External_library name 'pthread_rwlock_timedwrlock';
(* Const before type ignored *)
function pthread_rwlock_reltimedwrlock_np(_RESTRICT_KYWD1:Ppthread_rwlock_t; _RESTRICT_KYWD2:Ptimespec):longint;cdecl;external External_library name 'pthread_rwlock_reltimedwrlock_np';
function pthread_rwlock_trywrlock(_para1:Ppthread_rwlock_t):longint;cdecl;external External_library name 'pthread_rwlock_trywrlock';
function pthread_rwlock_unlock(_para1:Ppthread_rwlock_t):longint;cdecl;external External_library name 'pthread_rwlock_unlock';
function pthread_rwlockattr_init(_para1:Ppthread_rwlockattr_t):longint;cdecl;external External_library name 'pthread_rwlockattr_init';
function pthread_rwlockattr_destroy(_para1:Ppthread_rwlockattr_t):longint;cdecl;external External_library name 'pthread_rwlockattr_destroy';
(* Const before type ignored *)
function pthread_rwlockattr_getpshared(_RESTRICT_KYWD1:Ppthread_rwlockattr_t; _RESTRICT_KYWD2:Plongint):longint;cdecl;external External_library name 'pthread_rwlockattr_getpshared';
function pthread_rwlockattr_setpshared(_para1:Ppthread_rwlockattr_t; _para2:longint):longint;cdecl;external External_library name 'pthread_rwlockattr_setpshared';
function pthread_spin_init(_para1:Ppthread_spinlock_t; _para2:longint):longint;cdecl;external External_library name 'pthread_spin_init';
function pthread_spin_destroy(_para1:Ppthread_spinlock_t):longint;cdecl;external External_library name 'pthread_spin_destroy';
function pthread_spin_lock(_para1:Ppthread_spinlock_t):longint;cdecl;external External_library name 'pthread_spin_lock';
function pthread_spin_trylock(_para1:Ppthread_spinlock_t):longint;cdecl;external External_library name 'pthread_spin_trylock';
function pthread_spin_unlock(_para1:Ppthread_spinlock_t):longint;cdecl;external External_library name 'pthread_spin_unlock';
function pthread_barrierattr_init(_para1:Ppthread_barrierattr_t):longint;cdecl;external External_library name 'pthread_barrierattr_init';
function pthread_barrierattr_destroy(_para1:Ppthread_barrierattr_t):longint;cdecl;external External_library name 'pthread_barrierattr_destroy';
function pthread_barrierattr_setpshared(_para1:Ppthread_barrierattr_t; _para2:longint):longint;cdecl;external External_library name 'pthread_barrierattr_setpshared';
(* Const before type ignored *)
function pthread_barrierattr_getpshared(_RESTRICT_KYWD1:Ppthread_barrierattr_t; _RESTRICT_KYWD2:Plongint):longint;cdecl;external External_library name 'pthread_barrierattr_getpshared';
(* Const before type ignored *)
function pthread_barrier_init(_RESTRICT_KYWD1:Ppthread_barrier_t; _RESTRICT_KYWD2:Ppthread_barrierattr_t; _para3:uint_t):longint;cdecl;external External_library name 'pthread_barrier_init';
function pthread_barrier_destroy(_para1:Ppthread_barrier_t):longint;cdecl;external External_library name 'pthread_barrier_destroy';
function pthread_barrier_wait(_para1:Ppthread_barrier_t):longint;cdecl;external External_library name 'pthread_barrier_wait';
