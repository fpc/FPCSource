{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

Type
  tid_t = {$ifdef cpu64}clong{$else}cint{$endif};
  P__pthrdscreds  = ^__pthrdscreds;
  P__pthrdsinfo  = ^__pthrdsinfo;
  Pclockid_t  = ^clockid_t;
  Ppid_t  = ^pid_t;
  Ppthread_attr_t  = ^pthread_attr_t;
  Ppthread_barrier_t  = ^pthread_barrier_t;
  Ppthread_barrierattr_t  = ^pthread_barrierattr_t;
  Ppthread_cond_t  = ^pthread_cond_t;
  Ppthread_condattr_t  = ^pthread_condattr_t;
  Ppthread_key_t  = ^pthread_key_t;
  Ppthread_mutex_t  = ^pthread_mutex_t;
  Ppthread_mutexattr_t  = ^pthread_mutexattr_t;
  Ppthread_once_t  = ^pthread_once_t;
  Ppthread_rwlock_t  = ^pthread_rwlock_t;
  Ppthread_rwlockattr_t  = ^pthread_rwlockattr_t;
  Ppthread_spinlock_t  = ^pthread_spinlock_t;
  Ppthread_t  = ^pthread_t;
  Prusage  = ^rusage;
  Psched_param  = ^sched_param;
  Psigset_t  = ^sigset_t;
  Psize_t  = ^size_t;
  Ptid_t  = ^tid_t;
  Ptimespec  = ^timespec;
  
  pthread_barrier_t = record
{$ifdef cpu64}
    __br_word: array[0..4] of clong;
{$else}
    __br_word: array[0..7] of cint;
{$endif}
  end;
  
  pthread_barrierattr_t = pointer;

  _once = record
{$ifdef cpu64}
    __on_word: array[0..8] of clong;
{$else}
    __on_word: array[0..27] of cint;
{$endif}
  end;

  pthread_once_t = _once;

  pthread_rwlockattr_t = pointer;

  pthread_spinlock_t = record
{$ifdef cpu64}
    __sp_word: array[0..2] of clong;
{$else}
    __sp_word: array[0..5] of cint;
{$endif}
  end;


  rusage = record
    ru_utime,
    ru_stime: timeval;
    ru_maxrss,
    ru_ixrss,
    ru_idrss,
    ru_isrss,
    ru_minflt,
    ru_majflt,
    ru_nswap,
    ru_inblock,
    ru_oublock,
    ru_msgsnd,
    ru_msgrcv,
    ru_nsignals,
    ru_nvcsw,
    ru_nivcsw: clong;
  end;

  sched_param = record
    sched_priority,
    sched_policy : cint;
    sched_reserved: array[0..5] of cint;
  end;

  plabel_t = ^label_t;
  label_t = record
    prev: plabel_t;
    iar: culong;
    stack: culong;
    toc: culong;
    cr: culong;
    intpri: culong;
    reg: array[0..18] of culong; // non-volatile regs (13..31)
  end;

  padspace_t = ^adspace_t;
  adspace_t = record
    alloc: culong;
    srval: array[0..15] of cuint64;
  end;

  pmstsave = ^mstsave;
  mstsave = record
    prev: pmstsave;
    kjmpbuf: plabel_t;
    stackfix: pchar;
    intpri: byte;
    backt: byte;
    rsvd: array[0..1] of byte;
    curid: pid_t;

    excp_type: cint;
    iar: culong; // instruction address register
    msr: culong;
    cr: culong;
    lr: culong;
    ctr: culong;
    xer: culong;
    mq: culong;
    tid: culong;
    fpscr: culong;
    fpeu: bytebool;
    fpinfo: byte;
    pad: array[0..1] of byte;
    except_: array[0..4] of culong;
    pad1: array[0..3] of byte;
    o_iar: culong;
    o_toc: culong;
    o_arg1: culong;
    excbranch: culong;
    fpscrx: culong;
    o_vaddr: culong;
    cachealign: array[0..6] of culong;
    as_: adspace_t;
    gpr: array[0..31] of culong;
    fpr: array[0..31] of double;
  end;

  pstack_t = ^stack_t;
  stack_t = record
    ss_sp: pointer;
    ss_size: size_t;
    ss_flags: cint;
    __pad: array[0..3] of cint;
  end;

  __vmxreg_t = record
    __v: array[0..3] of cint;
  end;

  __vmx_context = record
    __vr: array[0..31] of __vmxreg_t;
    __pad1: array[0..2] of cuint;
    __vscr: cuint;
    __vrsave: cuint;
    __pad2: array[0..1] of cuint;
  end;

  p__extctx_t = ^__extctx_t;
  __extctx_t = record
    __flags: cuint;
    __rsvd1: array[0..2] of cuint;
    __u1: record
      __vmx: __vmx_context
    end;
    __ukeys: cuint;
    __reserved: array[0..4096-sizeof(__vmx_context)-7*sizeof(cint)-1] of byte;
    __extctx_magic: cint;
  end;


  __pthrdscontext = record
      __pc_gpr : array[0..(32)-1] of culonglong;
      __pc_msr : culonglong;
      __pc_iar : culonglong;
      __pc_lr : culonglong;
      __pc_ctr : culonglong;
      __pc_cr : cuint;
      __pc_xer : cuint;
      __pc_fpscr : cuint;
      __pc_fpscrx : cuint;
      __pc_except : array[0..0] of culonglong;
      __pc_fpr : array[0..(32)-1] of double;
      __pc_fpeu : cchar;
      __pc_fpinfo : cchar;
      __pc_mq : cuint;
      __pc_excp_type : cint;
    end;

  __pthrdsinfo = record
      __pi_handle : culonglong;
      __pi_ptid : pthread_t;
      __pi_tid : tid_t;
      __pi_state : cint;
      __pi_suspendstate : cint;
      __pi_exit : pointer;
      __pi_ustk : culong;
      __pi_func : pointer;
      __pi_arg : pointer;
      __pi_siglevel : cuint;
      __pi_frame_count : cuint;
      __pi_stackaddr : ^cchar;
      __pi_stackend : ^cchar;
      __pi_stacksize : size_t;
{$ifdef cpu64}
      __pi_pad : array[0..13] of culong;
{$else}
      __pi_pad : array[0..12] of culong;
{$endif}
      __pi_context : __pthrdscontext;
    end;

  __pthrdsinfox = record
      __pi : __pthrdsinfo;
      __pi_ec : __extctx_t;
    end;

  __pthrdstlsinfo = record
      pti_vaddr : pointer;
      pti_region : cint;
    end;
  PTHRDS_TLS_INFO = __pthrdstlsinfo;

  p__cap_t = ^__cap_t;
  __cap_t = record
    cap_effective,
    cap_inheritable,
    cap_permitted : cuint64;
  end;
  pcap_t = ^cap_t;
  cap_t = __cap_t;

  ppriv_t = ^priv_t;
  priv_t = record
    pv_priv: array[0..1] of cuint;
  end;
  

  pcred_ext_t = ^cred_ext_t;
  cred_ext_t = record
    crx_ruid,
    crx_uid,
    crx_suid,
    crx_luid,
    crx_acctid : uid_t;
    
    crx_gid,
    crx_rgid,
    crx_sgid : gid_t;
    
    crx_ngrps: cint;
    crx_groups: array[0..128-1] of gid_t;
    
    crx_reserved_1: cint;
    
    crx_caps: __cap_t;
    
    crx_mpriv,
    crx_ipriv,
    crx_epriv,
    crx_bpriv: priv_t;
    
    crx_npags: cint;
    crx_pagids: array[0..15] of byte;
    crx_pags: array[0..15] of cint;
    
    crx_pad: array[0..256-173-1] of cint;
  end;

  __pthrdscreds = record
      __pc_flags : cuint;
      __pc_cred : cred_ext_t;
      __pc_reserved : array[0..8] of cint;
    end;


const
  PTHREAD_PROCESS_SHARED = 0;  
  PTHREAD_PROCESS_PRIVATE = 1;  

{$ifndef _AIX_PTHREADS_D7}

const
  PTHREAD_MUTEX_ERRORCHECK = 3;  
  PTHREAD_MUTEX_NORMAL = 5;  
  PTHREAD_MUTEX_DEFAULT = PTHREAD_MUTEX_NORMAL;  
{$endif}


const
  PTHREAD_MUTEX_RECURSIVE = 4;  

  PTHREAD_PRIO_DEFAULT = 0;  
  PTHREAD_PRIO_NONE = 1;  
  PTHREAD_PRIO_PROTECT = 2;  
  PTHREAD_PRIO_INHERIT = 3;  
  PTHREAD_INHERIT_SCHED = 0;  
  PTHREAD_EXPLICIT_SCHED = 1;  
  PTHREAD_SCOPE_SYSTEM = 0;  
  PTHREAD_SCOPE_PROCESS = 1;  

  PTHREAD_CREATE_DETACHED = 1;  
  PTHREAD_CREATE_JOINABLE = 0;  


const
  PTHREAD_CANCELED = pointer(-1);

  PTHREAD_CANCEL_DISABLE = 0;  

  PTHREAD_CANCEL_ENABLE = 1;  

  PTHREAD_CANCEL_DEFERRED = 0;  

  PTHREAD_CANCEL_ASYNCHRONOUS = 1;  

  PTHREAD_BARRIER_SERIAL_THREAD = 2;  
  _PTH_FLAGS_INIT64 = 2;  


function pthread_attr_init(_para1:Ppthread_attr_t):cint;cdecl;external;
function pthread_attr_destroy(_para1:Ppthread_attr_t):cint;cdecl;external;
{$ifndef _AIX_PTHREADS_D7}

function pthread_attr_setstack(_para1:Ppthread_attr_t; _para2:pointer; _para3:size_t):cint;cdecl;external;
(* Const before type ignored *)
function pthread_attr_getstack(_para1:Ppthread_attr_t; _para2:Ppointer; _para3:Psize_t):cint;cdecl;external;
{$endif}


function pthread_attr_setstacksize(_para1:Ppthread_attr_t; _para2:size_t):cint;cdecl;external;
(* Const before type ignored *)
function pthread_attr_getstacksize(_para1:Ppthread_attr_t; _para2:Psize_t):cint;cdecl;external;
function pthread_attr_setstackaddr(_para1:Ppthread_attr_t; _para2:pointer):cint;cdecl;external;
(* Const before type ignored *)
function pthread_attr_getstackaddr(_para1:Ppthread_attr_t; _para2:Ppointer):cint;cdecl;external;
function pthread_attr_setschedpolicy(_para1:Ppthread_attr_t; _para2:cint):cint;cdecl;external;
(* Const before type ignored *)
function pthread_attr_getschedpolicy(_para1:Ppthread_attr_t; _para2:Pcint):cint;cdecl;external;
(* Const before type ignored *)
function pthread_attr_setschedparam(_para1:Ppthread_attr_t; _para2:Psched_param):cint;cdecl;external;
(* Const before type ignored *)
function pthread_attr_getschedparam(_para1:Ppthread_attr_t; _para2:Psched_param):cint;cdecl;external;
function pthread_attr_setinheritsched(_para1:Ppthread_attr_t; _para2:cint):cint;cdecl;external;
(* Const before type ignored *)
function pthread_attr_getinheritsched(_para1:Ppthread_attr_t; _para2:Pcint):cint;cdecl;external;
function pthread_attr_setdetachstate(_para1:Ppthread_attr_t; _para2:cint):cint;cdecl;external;
(* Const before type ignored *)
function pthread_attr_getdetachstate(_para1:Ppthread_attr_t; _para2:Pcint):cint;cdecl;external;
{$ifndef _AIX_PTHREADS_D7}

function pthread_attr_setguardsize(_para1:Ppthread_attr_t; _para2:size_t):cint;cdecl;external;
(* Const before type ignored *)
function pthread_attr_getguardsize(_para1:Ppthread_attr_t; _para2:Psize_t):cint;cdecl;external;
{$endif}


(* Const before type ignored *)

function pthread_setschedparam(_para1:pthread_t; _para2:cint; _para3:Psched_param):cint;cdecl;external;
function pthread_getschedparam(_para1:pthread_t; _para2:Pcint; _para3:Psched_param):cint;cdecl;external;
function pthread_setschedprio(_para1:pthread_t; _para2:cint):cint;cdecl;external;
function pthread_self:pthread_t;cdecl;external;

(* Const before type ignored *)

type pthreadcreateroutine = function (p:pointer):pointer; cdecl;
function pthread_create(_para1:Ppthread_t; _para2:Ppthread_attr_t; _para3:Pthreadcreateroutine; _para4:pointer):cint;cdecl;external;
function pthread_detach(_para1:pthread_t):cint;cdecl;external;
function pthread_join(_para1:pthread_t; _para2:Ppointer):cint;cdecl;external;
procedure pthread_exit(_para1:pointer);cdecl;external;
{$ifdef _AIX_PTHREADS_D7}
procedure pthread_yield;cdecl;external;
{$endif}

type cleanuproutine= procedure (a:pointer);cdecl;
procedure pthread_cleanup_push(_para1:cleanuproutine; _para2:pointer);cdecl;external;
procedure pthread_cleanup_pop(_para1:cint);cdecl;external;
function pthread_cancel(_para1:pthread_t):cint;cdecl;external;
function pthread_attr_setscope(_para1:Ppthread_attr_t; _para2:cint):cint;cdecl;external;
(* Const before type ignored *)
function pthread_attr_getscope(_para1:Ppthread_attr_t; _para2:Pcint):cint;cdecl;external;
function pthread_equal(_para1:pthread_t; _para2:pthread_t):cint;cdecl;external;
function pthread_kill(_para1:pthread_t; _para2:cint):cint;cdecl;external;
function pthread_checkpnt_pending:cint;cdecl;external;

function pthread_getcpuclockid(_para1:pthread_t; _para2:Pclockid_t):cint;cdecl;external;


function pthread_mutexattr_init(_para1:Ppthread_mutexattr_t):cint;cdecl;external;
function pthread_mutexattr_destroy(_para1:Ppthread_mutexattr_t):cint;cdecl;external;
function pthread_mutexattr_setprotocol(_para1:Ppthread_mutexattr_t; _para2:cint):cint;cdecl;external;
(* Const before type ignored *)
function pthread_mutexattr_getprotocol(_para1:Ppthread_mutexattr_t; _para2:Pcint):cint;cdecl;external;
function pthread_mutexattr_setprioceiling(_para1:Ppthread_mutexattr_t; _para2:cint):cint;cdecl;external;
(* Const before type ignored *)
function pthread_mutexattr_getprioceiling(_para1:Ppthread_mutexattr_t; _para2:Pcint):cint;cdecl;external;
(* Const before type ignored *)
function pthread_mutexattr_getpshared(_para1:Ppthread_mutexattr_t; _para2:Pcint):cint;cdecl;external;
{$ifdef _AIX_PTHREADS_D7}
(* Const before type ignored *)
function pthread_mutexattr_setpshared(_para1:Ppthread_mutexattr_t; _para2:cint):cint;cdecl;external;
{$else}

function pthread_mutexattr_setpshared(_para1:Ppthread_mutexattr_t; _para2:cint):cint;cdecl;external;
{$endif}
{$ifndef _AIX_PTHREADS_D7}
(* Const before type ignored *)

function pthread_mutexattr_gettype(_para1:Ppthread_mutexattr_t; _para2:Pcint):cint;cdecl;external;
function pthread_mutexattr_settype(_para1:Ppthread_mutexattr_t; _para2:cint):cint;cdecl;external;
{$endif}



function pthread_mutex_init(_para1:Ppthread_mutex_t; _para2:Ppthread_mutexattr_t):cint;cdecl;external;
function pthread_mutex_destroy(_para1:Ppthread_mutex_t):cint;cdecl;external;
function pthread_mutex_lock(_para1:Ppthread_mutex_t):cint;cdecl;external;
(* Const before type ignored *)
function pthread_mutex_timedlock(_para1:Ppthread_mutex_t; _para2:Ptimespec):cint;cdecl;external;
function pthread_mutex_trylock(_para1:Ppthread_mutex_t):cint;cdecl;external;
function pthread_mutex_unlock(_para1:Ppthread_mutex_t):cint;cdecl;external;
function pthread_mutex_setprioceiling(_para1:Ppthread_mutex_t; _para2:cint; _para3:Pcint):cint;cdecl;external;
function pthread_mutex_getprioceiling(_para1:Ppthread_mutex_t; _para2:Pcint):cint;cdecl;external;

function pthread_condattr_init(_para1:Ppthread_condattr_t):cint;cdecl;external;
function pthread_condattr_destroy(_para1:Ppthread_condattr_t):cint;cdecl;external;
(* Const before type ignored *)
function pthread_condattr_getpshared(_para1:Ppthread_condattr_t; _para2:Pcint):cint;cdecl;external;
function pthread_condattr_setpshared(_para1:Ppthread_condattr_t; _para2:cint):cint;cdecl;external;
(* Const before type ignored *)
function pthread_condattr_getclock(_para1:Ppthread_condattr_t; _para2:Pclockid_t):cint;cdecl;external;
function pthread_condattr_setclock(_para1:Ppthread_condattr_t; _para2:clockid_t):cint;cdecl;external;

(* Const before type ignored *)
function pthread_cond_init(_para1:Ppthread_cond_t; _para2:Ppthread_condattr_t):cint;cdecl;external;
function pthread_cond_destroy(_para1:Ppthread_cond_t):cint;cdecl;external;
function pthread_cond_wait(_para1:Ppthread_cond_t; _para2:Ppthread_mutex_t):cint;cdecl;external;
(* Const before type ignored *)
function pthread_cond_timedwait(_para1:Ppthread_cond_t; _para2:Ppthread_mutex_t; _para3:Ptimespec):cint;cdecl;external;
function pthread_cond_signal(_para1:Ppthread_cond_t):cint;cdecl;external;
function pthread_cond_broadcast(_para1:Ppthread_cond_t):cint;cdecl;external;

type keycreateroutine = procedure(p: pointer); cdecl;
function pthread_key_create(_para1:Ppthread_key_t; _para2:keycreateroutine):cint;cdecl;external;
function pthread_key_delete(_para1:pthread_key_t):cint;cdecl;external;
function pthread_getspecific(_para1:pthread_key_t):pointer;cdecl;external;
(* Const before type ignored *)
function pthread_setspecific(_para1:pthread_key_t; _para2:pointer):cint;cdecl;external;

procedure pthread_testcancel;cdecl;external;
function pthread_setcancelstate(_para1:cint; _para2:Pcint):cint;cdecl;external;
function pthread_setcanceltype(_para1:cint; _para2:Pcint):cint;cdecl;external;

type pthreadonceroutine = procedure;cdecl;
function pthread_once(_para1:Ppthread_once_t; _para2:pthreadonceroutine ):cint;cdecl;external;

{$ifndef _AIX_PTHREADS_D7}

function pthread_rwlock_destroy(_para1:Ppthread_rwlock_t):cint;cdecl;external;
(* Const before type ignored *)
function pthread_rwlock_init(_para1:Ppthread_rwlock_t; _para2:Ppthread_rwlockattr_t):cint;cdecl;external;
function pthread_rwlock_rdlock(_para1:Ppthread_rwlock_t):cint;cdecl;external;
function pthread_rwlock_tryrdlock(_para1:Ppthread_rwlock_t):cint;cdecl;external;
function pthread_rwlock_trywrlock(_para1:Ppthread_rwlock_t):cint;cdecl;external;
function pthread_rwlock_unlock(_para1:Ppthread_rwlock_t):cint;cdecl;external;
function pthread_rwlock_wrlock(_para1:Ppthread_rwlock_t):cint;cdecl;external;
function pthread_rwlockattr_destroy(_para1:Ppthread_rwlockattr_t):cint;cdecl;external;
(* Const before type ignored *)
function pthread_rwlockattr_getpshared(_para1:Ppthread_rwlockattr_t; _para2:Pcint):cint;cdecl;external;
function pthread_rwlockattr_init(_para1:Ppthread_rwlockattr_t):cint;cdecl;external;
function pthread_rwlockattr_setpshared(_para1:Ppthread_rwlockattr_t; _para2:cint):cint;cdecl;external;
type pthreadatforkroutine = procedure;cdecl;
function pthread_atfork(_para1:pthreadatforkroutine ; _para2:pthreadatforkroutine ; _para3:pthreadatforkroutine ):cint;cdecl;external;

(* Const before type ignored *)

function pthread_rwlock_timedwrlock(_para1:Ppthread_rwlock_t; _para2:Ptimespec):cint;cdecl;external;
(* Const before type ignored *)
function pthread_rwlock_timedrdlock(_para1:Ppthread_rwlock_t; _para2:Ptimespec):cint;cdecl;external;


function pthread_getconcurrency:cint;cdecl;external;
function pthread_setconcurrency(_para1:cint):cint;cdecl;external;
{$endif}

function pthread_spin_init(_para1:Ppthread_spinlock_t; _para2:cint):cint;cdecl;external;
function pthread_spin_destroy(_para1:Ppthread_spinlock_t):cint;cdecl;external;
function pthread_spin_lock(_para1:Ppthread_spinlock_t):cint;cdecl;external;
function pthread_spin_unlock(_para1:Ppthread_spinlock_t):cint;cdecl;external;
function pthread_spin_trylock(_para1:Ppthread_spinlock_t):cint;cdecl;external;

function pthread_barrierattr_init(_para1:Ppthread_barrierattr_t):cint;cdecl;external;
function pthread_barrierattr_destroy(_para1:Ppthread_barrierattr_t):cint;cdecl;external;
(* Const before type ignored *)
function pthread_barrierattr_getpshared(_para1:Ppthread_barrierattr_t; _para2:Pcint):cint;cdecl;external;
function pthread_barrierattr_setpshared(_para1:Ppthread_barrierattr_t; _para2:cint):cint;cdecl;external;

(* Const before type ignored *)
function pthread_barrier_init(_para1:Ppthread_barrier_t; _para2:Ppthread_barrierattr_t; _para3:cunsigned):cint;cdecl;external;
function pthread_barrier_destroy(_para1:Ppthread_barrier_t):cint;cdecl;external;
function pthread_barrier_wait(_para1:Ppthread_barrier_t):cint;cdecl;external;

{$ifdef _AIX_PTHREADS_D7}

function pthread_atfork(_para1:procedure ; _para2:procedure ; _para3:procedure ):cint;cdecl;external;
{$endif}
  var
    __pthread_trace : cint;cvar;external;

    __n_pthreads : cint;cvar;external;

{$if defined(_ALL_SOURCE) or defined(_AIX_PTHREADS_D7)}


const
  LOG_GENERAL = 1;  
  LOG_LOCKS = 2;  
  LOG_DATA = 4;  
  LOG_CREATE = 8;  
  LOG_USCHED = 16;  
  LOG_CANCEL = 32;  
  LOG_SIGNAL = 64;  
  LOG_SUSPEND = 128;  
  LOG_MRQ = 256;  
  LOG_MRQ1 = 512;  

  LOG_HANG = 1024;  
  LOG_LOCKBOOST = 2048;  



  MUTEX_FAST_NP = 2;  
  MUTEX_RECURSIVE_NP = 1;  

  MUTEX_NONRECURSIVE_NP = 0;  

  DEFAULT_SCHED = SCHED_OTHER;  
  DEFAULT_PRIO = PTHREAD_PRIO_MIN;  
  DEFAULT_INHERIT = PTHREAD_INHERIT_SCHED;  
  PTHREAD_SCOPE_GLOBAL = PTHREAD_SCOPE_SYSTEM;  
  PTHREAD_SCOPE_LOCAL = PTHREAD_SCOPE_PROCESS;  
  DEFAULT_SCOPE = PTHREAD_SCOPE_LOCAL;  

  PTHREAD_CREATE_UNDETACHED = 0;  

  PTHREAD_CREATE_SUSPENDED_NP = 1;  
  PTHREAD_CREATE_UNSUSPENDED_NP = 0;  
{$ifdef _AIX_PTHREADS_D7}
  DEFAULT_DETACHSTATE = PTHREAD_CREATE_DETACHED;  
{$else}

const
  DEFAULT_DETACHSTATE = PTHREAD_CREATE_JOINABLE;  
{$endif}
(*
{$ifdef __64BIT__}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

function pthread_mutex_getowner_np(mutex : longint) : longint;

{$else}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

function pthread_mutex_getowner_np(mutex : longint) : longint;

{$endif}
*)

const
  PAGESIZE = 4096;
{$ifdef cpu64}
  PTHREAD_SPECIFIC_DATA	= 2 * PAGESIZE;	{ per-thread data }
  APTHREAD_DATAKEYS_MAX = (PTHREAD_SPECIFIC_DATA / 16);
{$else cpu64}
  PTHREAD_SPECIFIC_DATA	=  PAGESIZE;	{ per-thread data }
  APTHREAD_DATAKEYS_MAX : (PTHREAD_SPECIFIC_DATA / 8);
{$endif cpu64}
  PTHREAD_DATAKEYS_MAX = 450;
  PTHREAD_STACK_MIN_NP = (PAGESIZE * 2);


{$ifdef _AIX_PTHREADS_D7}
  PTHREAD_STACK_MIN = (PAGESIZE * 24);
{$else	_AIX_PTHREADS_D7}
{$ifdef cpu64}
  PTHREAD_STACK_MIN = (2 * PAGESIZE * 2);
{$else cpu64}
  PTHREAD_STACK_MIN = (PAGESIZE * 2);
{$endif cpu64}
{$endif _AIX_PTHREADS_D7}

/* PTHREAD_THREADS_MAX has the same value as PTHREAD_THREADS_MAX in limits.h */
{$ifdef _LARGE_THREADS}
  PTHREAD_THREADS_MAX = 32767;
{$else _LARGE_THREADS}
  PTHREAD_THREADS_MAX = 512;
{$endif _LARGE_THREADS}

const
  __pi_suspended = __pi_suspendstate;  
  __pi_returned = __pi_exit;  

  PTHRDSINFO_STATE_NOTSUP = $00000100;  
  PTHRDSINFO_STATE_IDLE = $00000004;  
  PTHRDSINFO_STATE_RUN = $00000001;  
  PTHRDSINFO_STATE_SLEEP = $00000008;  
  PTHRDSINFO_STATE_READY = $00000002;  
  PTHRDSINFO_STATE_TERM = $00000010;  

  PTHRDSINFO_SUSPENDSTATE_NOTSUP = $00000100;  
  PTHRDSINFO_SUSPENDSTATE_SUSPENDED = $00000001;  
  PTHRDSINFO_SUSPENDSTATE_UNSUSPENDED = $00000000;  


const
  PTHRDSINFO_SIGCTX_BASE = cuint(-1);  

  PTHRDSINFO_SIGCTX_INVALID = cuint(-1);

  PTHRDSINFO_QUERY_GPRS = $00000001;  

  PTHRDSINFO_QUERY_SPRS = $00000002;  

  PTHRDSINFO_QUERY_FPRS = $00000004;  

  PTHRDSINFO_QUERY_REGS = $0000000F;  

  PTHRDSINFO_QUERY_TID = $00000010;  

  PTHRDSINFO_QUERY_ALL = $0000001F;  


  PTHRDSINFO_QUERY_SIGCTX = $00000100;  

  PTHRDSINFO_QUERY_EXTCTX = $00000200;  

  PTHRDSINFO_QUERY_TLS = $00000400;  

  PTHRDSINFO_RUSAGE_START = $00000001;  
  PTHRDSINFO_RUSAGE_STOP = $00000002;  
  PTHRDSINFO_RUSAGE_COLLECT = $00000004;  


  PTHRDSCREDS_INHERIT_UIDS = $00000001;  

  PTHRDSCREDS_INHERIT_GIDS = $00000010;  

  PTHRDSCREDS_INHERIT_GSETS = $00000100;  

  PTHRDSCREDS_INHERIT_CAPS = $00001000;  

  PTHRDSCREDS_INHERIT_PRIVS = $00010000;  

  PTHRDSCREDS_INHERIT_ALL = $00011111;  

{$ifdef _AIX_PTHREADS_D7}
  var
    pthread_attr_default : pthread_attr_t;cvar;external;
    pthread_mutexattr_default : pthread_mutexattr_t;cvar;external;
    pthread_condattr_default : pthread_condattr_t;cvar;external;
    __pthread_cleanup_handlerqueue : pthread_key_t;cvar;external;
{$endif}


procedure __funcblock_np;cdecl;external;

{$ifndef _AIX32_THREADS}

function pthread_getunique_np(_para1:Ppthread_t; _para2:Pcint):cint;cdecl;external;
function pthread_mutexattr_getkind_np(_para1:Ppthread_mutexattr_t; _para2:Pcint):cint;cdecl;external;
{$endif}



function pthread_geteffectiveprio_np(_para1:pthread_t; _para2:Pcint):cint;cdecl;external;
function pthread_mutexattr_setkind_np(_para1:Ppthread_mutexattr_t; _para2:cint):cint;cdecl;external;
function pthread_set_mutexattr_default_np(_para1:cint):cint;cdecl;external;
function pthread_signal_to_cancel_np(_para1:Psigset_t; _para2:Ppthread_t):cint;cdecl;external;
function pthread_delay_np(_para1:Ptimespec):cint;cdecl;external;
function pthread_get_expiration_np(_para1:Ptimespec; _para2:Ptimespec):cint;cdecl;external;
procedure pthread_lock_global_np;cdecl;external;
procedure pthread_unlock_global_np;cdecl;external;
function pthread_atfork_np(_para1:pointer; _para2:procedure (_para1:pointer); _para3:procedure (_para1:pointer); _para4:procedure (_para1:pointer)):cint;cdecl;external;

const
  PTHREAD_ATFORK_ARGUMENT = $00000001;  
  PTHREAD_ATFORK_ALL = $00000002;  

function pthread_atfork_unregister_np(_para1:pointer; _para2:procedure ; _para3:procedure ; _para4:procedure ; _para5:cint):cint;cdecl;external;
function pthread_test_exit_np(_para1:Pcint):cint;cdecl;external;
procedure pthread_clear_exit_np(_para1:pthread_t);cdecl;external;
function pthread_setcancelstate_np(_para1:cint; _para2:Pcint):cint;cdecl;external;
function pthread_join_np(_para1:pthread_t; _para2:Ppointer):cint;cdecl;external;
procedure pthread_cleanup_push_np(_para1:procedure (_para1:pointer); _para2:pointer; _para3:Ppthread_t);cdecl;external;
procedure pthread_cleanup_pop_np(_para1:cint; _para2:pthread_t);cdecl;external;
function pthread_cleanup_information_np(_para1:pthread_t; _para2:pointer; _para3:Ptid_t; _para4:pointer; _para5:Ppthread_t):cint;cdecl;external;

(* Const before type ignored *)

function pthread_create_withcred_np(_para1:Ppthread_t; _para2:Ppthread_attr_t; _para3:Pprocedure (_para1:pointer); _para4:pointer; _para5:P__pthrdscreds):cint;cdecl;external;
{$ifdef _AIX_PTHREADS_D7}
function forkall(_para1:Ppid_t):cint;cdecl;external;
{$endif}
(* Const before type ignored *)

function sigthreadmask(_para1:cint; _para2:Psigset_t; _para3:Psigset_t):cint;cdecl;external;
function pthread_attr_setsuspendstate_np(_para1:Ppthread_attr_t; _para2:cint):cint;cdecl;external;
(* Const before type ignored *)
function pthread_attr_getsuspendstate_np(_para1:Ppthread_attr_t; _para2:Pcint):cint;cdecl;external;
function pthread_suspend_np(_para1:pthread_t):cint;cdecl;external;
function pthread_suspend_others_np:cint;cdecl;external;
function pthread_suspend(_para1:pthread_t):cint;cdecl;external;
function pthread_continue_np(_para1:pthread_t):cint;cdecl;external;
function pthread_unsuspend_np(_para1:pthread_t):cint;cdecl;external;
function pthread_continue_others_np:cint;cdecl;external;
function pthread_unsuspend_others_np:cint;cdecl;external;
function pthread_continue(_para1:pthread_t):cint;cdecl;external;
function pthread_attr_setstacksize_np(_para1:Ppthread_attr_t; _para2:size_t):cint;cdecl;external;
function pthread_attr_setukeyset_np(_para1:Ppthread_attr_t; _para2:Pculonglong):cint;cdecl;external;
(* Const before type ignored *)
function pthread_attr_getukeyset_np(_para1:Ppthread_attr_t; _para2:Pculonglong):cint;cdecl;external;
function pthread_getthrds_np(_para1:Ppthread_t; _para2:cint; _para3:P__pthrdsinfo; _para4:cint; _para5:pointer; 
           _para6:Pcint):cint;cdecl;external;
function pthread_getrusage_np(_para1:pthread_t; _para2:Prusage; _para3:cint):cint;cdecl;external;

{$endif _ALL_SOURCE or _AIX_PTHREADS_D7}
