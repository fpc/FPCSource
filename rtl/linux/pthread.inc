{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Peter Vreman
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This file contains a pthread.h headerconversion for Linux.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*****************************************************************************
                   Local POSIX Threads (pthread) imports
*****************************************************************************}

  { Attributes  }
  const
     THREAD_PRIORITY_IDLE               = 1;
     THREAD_PRIORITY_LOWEST             = 15;
     THREAD_PRIORITY_BELOW_NORMAL       = 30;
     THREAD_PRIORITY_NORMAL             = 50;
     THREAD_PRIORITY_ABOVE_NORMAL       = 70;
     THREAD_PRIORITY_HIGHEST            = 80;
     THREAD_PRIORITY_TIME_CRITICAL      = 99;
     //PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP : array [0..5]of Integer = (0, 0, 0, 1, 0, 0);

Type
   psem_t = ^sem_t;
   TSemaphore = sem_t;
   PSemaphore = ^TSemaphore;

{$ifndef FPC_USE_LIBC}
   tlibc_sigset = array[0..(1024 div 32)-1] of cuLong;
   plibc_sigset = ^tlibc_sigset;
{$else not FPC_USE_LIBC}
   tlibc_sigset = tsigset;
   plibc_sigset = psigset;
{$endif not FPC_USE_LIBC}

     TThreadPriority = (tpIdle, tpLowest, tpLower, tpNormal, tpHigher, tpHighest, tpTimeCritical);
{
  const
     Priorities: array [TThreadPriority] of Integer = (
       THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST, THREAD_PRIORITY_BELOW_NORMAL,
       THREAD_PRIORITY_NORMAL, THREAD_PRIORITY_ABOVE_NORMAL,
       THREAD_PRIORITY_HIGHEST, THREAD_PRIORITY_TIME_CRITICAL
     );
}
  const
     _POSIX_THREAD_THREADS_MAX = 64;
     PTHREAD_THREADS_MAX = 512;
     _POSIX_THREAD_KEYS_MAX = 128;
     PTHREAD_KEYS_MAX = 128;

  type
    ppthread_t = ^pthread_t;
{
     p_pthread_queue = ^_pthread_queue;
}
     ppthread_mutex_t = ^pthread_mutex_t;

     ppthread_cond_t = ^pthread_cond_t;

     { Attributes  }

    const
      PTHREAD_CREATE_JOINABLE = 0;
      PTHREAD_CREATE_DETACHED = 1;
      PTHREAD_INHERIT_SCHED   = 0;
      PTHREAD_EXPLICIT_SCHED  = 1;
      PTHREAD_SCOPE_SYSTEM    = 0;
      PTHREAD_SCOPE_PROCESS   = 1;

    type
       psched_param = ^sched_param;

       ppthread_attr_t = ^pthread_attr_t;

       ppthread_mutexattr_t = ^pthread_mutexattr_t;

       ppthread_condattr_t = ^pthread_condattr_t;

       ppthread_key_t = ^pthread_key_t;

{      pthread_once_t = cint;
       ppthread_once_t = ^pthread_once_t;}

    const
       PTHREAD_ONCE_INIT = 0;

    type
(*
       tpcb_routine = Procedure(P:Pointer); cdecl;

       p_pthread_cleanup_buffer = ^_pthread_cleanup_buffer;
       _pthread_cleanup_buffer = record
          routine : tpcb_routine;             { Function to call. }
          arg : Pointer;                      { Its argument.  }
          canceltype:LongInt;                 { Saved cancellation type. }
          prev : p_pthread_cleanup_buffer; { Chaining of cleanup functions.  }
       end;
*)

     __start_routine_t = function (_para1:pointer):pointer;cdecl;
     __destr_function_t = procedure (_para1:pointer);cdecl;
{     t_pthread_cleanup_push_routine = procedure (_para1:pointer);
     t_pthread_cleanup_push_defer_routine = procedure (_para1:pointer);}

{$ifndef dynpthreads}
    function pthread_create(__thread:ppthread_t; __attr:ppthread_attr_t;__start_routine: __start_routine_t;__arg:pointer):longint;cdecl;external;
    function pthread_self:pthread_t;cdecl;external;
    function pthread_equal(__thread1:pthread_t; __thread2:pthread_t):longint;cdecl;external;
    procedure pthread_exit(__retval:pointer);cdecl;external;
    function pthread_join(__th:pthread_t; __thread_return:ppointer):longint;cdecl;external;
    function pthread_detach(__th:pthread_t):longint;cdecl;external;
    function pthread_attr_init(__attr:ppthread_attr_t):longint;cdecl;external;
    function pthread_attr_destroy(__attr:ppthread_attr_t):longint;cdecl;external;
    function pthread_attr_setdetachstate(__attr:ppthread_attr_t; __detachstate:longint):longint;cdecl;external;
    function pthread_attr_getdetachstate(__attr:ppthread_attr_t; __detachstate:plongint):longint;cdecl;external;
    function pthread_attr_setschedparam(__attr:ppthread_attr_t; __param:psched_param):longint;cdecl;external;
    function pthread_attr_getschedparam(__attr:ppthread_attr_t; __param:psched_param):longint;cdecl;external;
    function pthread_attr_setschedpolicy(__attr:ppthread_attr_t; __policy:longint):longint;cdecl;external;
    function pthread_attr_getschedpolicy(__attr:ppthread_attr_t; __policy:plongint):longint;cdecl;external;
{$ifndef ANDROID}
    function pthread_attr_setinheritsched(__attr:ppthread_attr_t; __inherit:longint):longint;cdecl;external;
    function pthread_attr_getinheritsched(__attr:ppthread_attr_t; __inherit:plongint):longint;cdecl;external;
{$endif}
    function pthread_attr_setstacksize(p: ppthread_attr_t;s:size_t):cint;cdecl;external;
    function pthread_attr_getstacksize(p: ppthread_attr_t;s:psize_t):cint;cdecl;external;
    function pthread_attr_setscope(__attr:ppthread_attr_t; __scope:longint):longint;cdecl;external;
    function pthread_attr_getscope(__attr:ppthread_attr_t; __scope:plongint):longint;cdecl;external;
    function pthread_setschedparam(__target_thread:pthread_t; __policy:longint; __param:psched_param):longint;cdecl;external;
    function pthread_getschedparam(__target_thread:pthread_t; __policy:plongint; __param:psched_param):longint;cdecl;external;
    function pthread_mutex_init(__mutex:ppthread_mutex_t; __mutex_attr:ppthread_mutexattr_t):longint;cdecl;external;
    function pthread_mutex_destroy(__mutex:ppthread_mutex_t):longint;cdecl;external;
    function pthread_mutex_trylock(__mutex:ppthread_mutex_t):longint;cdecl;external;
    function pthread_mutex_timedlock(__mutex:ppthread_mutex_t; __abs_timeout:ptimespec):longint;cdecl;external;
    function pthread_mutex_lock(__mutex:ppthread_mutex_t):longint;cdecl;external;
    function pthread_mutex_unlock(__mutex:ppthread_mutex_t):longint;cdecl;external;
    function pthread_mutexattr_init(__attr:ppthread_mutexattr_t):longint;cdecl;external;
    function pthread_mutexattr_destroy(__attr:ppthread_mutexattr_t):longint;cdecl;external;
{$ifndef ANDROID}
    function pthread_mutexattr_setkind_np(__attr:ppthread_mutexattr_t; __kind:longint):longint;cdecl;external;
    function pthread_mutexattr_getkind_np(__attr:ppthread_mutexattr_t; __kind:plongint):longint;cdecl;external;
{$endif}
    function pthread_cond_init(__cond:ppthread_cond_t; __cond_attr:ppthread_condattr_t):longint;cdecl;external;
    function pthread_cond_destroy(__cond:ppthread_cond_t):longint;cdecl;external;
    function pthread_cond_signal(__cond:ppthread_cond_t):longint;cdecl;external;
    function pthread_cond_broadcast(__cond:ppthread_cond_t):longint;cdecl;external;
    function pthread_cond_wait(__cond:ppthread_cond_t; __mutex:ppthread_mutex_t):longint;cdecl;external;
    function pthread_cond_timedwait(__cond:ppthread_cond_t; __mutex:ppthread_mutex_t; __abstime:ptimespec):longint;cdecl;external;
{$ifndef ANDROID}
    function pthread_condattr_init(__attr:ppthread_condattr_t):longint;cdecl;external;
    function pthread_condattr_destroy(__attr:ppthread_condattr_t):longint;cdecl;external;
    function pthread_condattr_setclock(__attr:ppthread_condattr_t; __clock_id: longint):longint;cdecl;external;
{$endif}
    function pthread_key_create(__key:ppthread_key_t; __destr_function:__destr_function_t):longint;cdecl;external;
    function pthread_key_delete(__key:pthread_key_t):longint;cdecl;external;
    function pthread_setspecific(__key:pthread_key_t; __pointer:pointer):longint;cdecl;external;
    function pthread_getspecific(__key:pthread_key_t):pointer;cdecl;external;
{    function pthread_once(__once_control:ppthread_once_t; __init_routine:tprocedure ):longint;cdecl;external;}
{$ifndef ANDROID}
    function pthread_setcancelstate(__state:longint; __oldstate:plongint):longint;cdecl;external;
    function pthread_setcanceltype(__type:longint; __oldtype:plongint):longint;cdecl;external;
    function pthread_cancel(__thread:pthread_t):longint;cdecl;external;
    procedure pthread_testcancel;cdecl;external;
{$endif}
{    procedure _pthread_cleanup_push(__buffer:p_pthread_cleanup_buffer;__routine:t_pthread_cleanup_push_routine; __arg:pointer);cdecl;external; }
{    procedure _pthread_cleanup_push_defer(__buffer:p_pthread_cleanup_buffer;__routine:t_pthread_cleanup_push_defer_routine; __arg:pointer);cdecl;external;}
{    function pthread_sigmask(__how:longint; __newmask:plibc_sigset; __oldmask:plibc_sigset):longint;cdecl;external;}
    function pthread_kill(__thread:pthread_t; __signo:longint):longint;cdecl;external;
{    function sigwait(__set:plibc_sigset; __sig:plongint):longint;cdecl;external;}
{$ifndef ANDROID}
    function pthread_atfork(__prepare:tprocedure ; __parent:tprocedure ; __child:tprocedure ):longint;cdecl;external;
    procedure pthread_kill_other_threads_np;cdecl;external;
{$endif}
    function pthread_sigmask(how: cint; nset: plibc_sigset; oset: plibc_sigset): cint; cdecl; external;

    function sem_init (__sem:Psem_t; __pshared:longint; __value:dword):longint;cdecl;external;
    function sem_destroy  (__sem:Psem_t):longint;cdecl;external;
    function sem_close (__sem:Psem_t):longint;cdecl;external;
    function sem_unlink (__name:PAnsiChar):longint;cdecl;external;
    function sem_wait (__sem:Psem_t):longint;cdecl;external;
    function sem_timedwait (__sem:Psem_t; __abs_timeout:ptimespec):longint;cdecl;external;
    function sem_trywait  (__sem:Psem_t):longint;cdecl;external;
    function sem_post     (__sem:Psem_t):longint;cdecl;external;
    function sem_getvalue (__sem:Psem_t; __sval:Plongint):longint;cdecl;external;

    function pthread_mutexattr_settype (__attr: Ppthread_mutexattr_t; Kind:Integer): Integer; cdecl;external;
    function pthread_setname_np(thread: pthread_t; name: PAnsiChar):cint;cdecl;external;

{$else}
Var
    pthread_create : Function(__thread:ppthread_t; __attr:ppthread_attr_t;__start_routine: __start_routine_t;__arg:pointer):longint;cdecl;
    pthread_self: Function : pthread_t;cdecl;
    pthread_equal : Function(__thread1:pthread_t; __thread2:pthread_t):longint;cdecl;
    pthread_exit : procedure (__retval:pointer);cdecl;
    pthread_join : Function(__th:pthread_t; __thread_return:ppointer):longint;cdecl;
    pthread_detach : Function(__th:pthread_t):longint;cdecl;
    pthread_attr_init : Function(__attr:ppthread_attr_t):longint;cdecl;
    pthread_attr_destroy : Function(__attr:ppthread_attr_t):longint;cdecl;
    pthread_attr_setdetachstate : Function(__attr:ppthread_attr_t; __detachstate:longint):longint;cdecl;
    pthread_attr_getdetachstate : Function(__attr:ppthread_attr_t; __detachstate:plongint):longint;cdecl;
    pthread_attr_setschedparam : Function(__attr:ppthread_attr_t; __param:psched_param):longint;cdecl;
    pthread_attr_getschedparam : Function(__attr:ppthread_attr_t; __param:psched_param):longint;cdecl;
    pthread_attr_setschedpolicy : Function(__attr:ppthread_attr_t; __policy:longint):longint;cdecl;
    pthread_attr_getschedpolicy : Function(__attr:ppthread_attr_t; __policy:plongint):longint;cdecl;
{$ifndef ANDROID}
    pthread_attr_setinheritsched : Function(__attr:ppthread_attr_t; __inherit:longint):longint;cdecl;
    pthread_attr_getinheritsched : Function(__attr:ppthread_attr_t; __inherit:plongint):longint;cdecl;
{$endif}
    pthread_attr_setscope : Function(__attr:ppthread_attr_t; __scope:longint):longint;cdecl;
    pthread_attr_getscope : Function(__attr:ppthread_attr_t; __scope:plongint):longint;cdecl;
    pthread_setschedparam : Function(__target_thread:pthread_t; __policy:longint; __param:psched_param):longint;cdecl;
    pthread_getschedparam : Function(__target_thread:pthread_t; __policy:plongint; __param:psched_param):longint;cdecl;
    pthread_attr_setstacksize : Function(p: ppthread_attr_t;s:size_t):cint;cdecl;
    pthread_mutex_init : Function(__mutex:ppthread_mutex_t; __mutex_attr:ppthread_mutexattr_t):longint;cdecl;
    pthread_mutex_destroy : Function(__mutex:ppthread_mutex_t):longint;cdecl;
    pthread_mutex_trylock : Function(__mutex:ppthread_mutex_t):longint;cdecl;
    pthread_mutex_timedlock : Function(__mutex:ppthread_mutex_t; __abs_timeout:ptimespec):longint;cdecl;
    pthread_mutex_lock : Function(__mutex:ppthread_mutex_t):longint;cdecl;
    pthread_mutex_unlock : Function(__mutex:ppthread_mutex_t):longint;cdecl;
    pthread_mutexattr_init : Function(__attr:ppthread_mutexattr_t):longint;cdecl;
    pthread_mutexattr_destroy : Function(__attr:ppthread_mutexattr_t):longint;cdecl;
{$ifndef ANDROID}
    pthread_mutexattr_setkind_np : Function(__attr:ppthread_mutexattr_t; __kind:longint):longint;cdecl;
    pthread_mutexattr_getkind_np : Function(__attr:ppthread_mutexattr_t; __kind:plongint):longint;cdecl;
{$endif}
    pthread_cond_init : Function(__cond:ppthread_cond_t; __cond_attr:ppthread_condattr_t):longint;cdecl;
    pthread_cond_destroy : Function(__cond:ppthread_cond_t):longint;cdecl;
    pthread_cond_signal : Function(__cond:ppthread_cond_t):longint;cdecl;
    pthread_cond_broadcast : Function(__cond:ppthread_cond_t):longint;cdecl;
    pthread_cond_wait : Function(__cond:ppthread_cond_t; __mutex:ppthread_mutex_t):longint;cdecl;
    pthread_cond_timedwait : Function(__cond:ppthread_cond_t; __mutex:ppthread_mutex_t; __abstime:ptimespec):longint;cdecl;
{$ifndef ANDROID}
    pthread_condattr_init : Function(__attr:ppthread_condattr_t):longint;cdecl;
    pthread_condattr_destroy : Function(__attr:ppthread_condattr_t):longint;cdecl;
    pthread_condattr_setclock: Function(__attr:ppthread_condattr_t; __clock_id: longint):longint;cdecl;
{$endif}
    pthread_key_create : Function(__key:ppthread_key_t; __destr_function:__destr_function_t):longint;cdecl;
    pthread_key_delete : Function(__key:pthread_key_t):longint;cdecl;
    pthread_setspecific : Function(__key:pthread_key_t; __pointer:pointer):longint;cdecl;
    pthread_getspecific : Function(__key:pthread_key_t):pointer;cdecl;
{    pthread_once : Function(__once_control:ppthread_once_t; __init_routine:tprocedure ):longint;cdecl;}
{$ifndef ANDROID}
    pthread_setcancelstate : Function(__state:longint; __oldstate:plongint):longint;cdecl;
    pthread_setcanceltype : Function(__type:longint; __oldtype:plongint):longint;cdecl;
    pthread_cancel : Function(__thread:pthread_t):longint;cdecl;
    pthread_testcancel : Procedure ;cdecl;
{$endif}
{    _pthread_cleanup_push : procedure (__buffer:p_pthread_cleanup_buffer;__routine:t_pthread_cleanup_push_routine; __arg:pointer);cdecl;}
{    _pthread_cleanup_push_defer : procedure (__buffer:p_pthread_cleanup_buffer;__routine:t_pthread_cleanup_push_defer_routine; __arg:pointer);cdecl;}
{    pthread_sigmask : Function(__how:longint; __newmask:plibc_sigset; __oldmask:plibc_sigset):longint;cdecl;}
    pthread_kill : Function(__thread:pthread_t; __signo:longint):longint;cdecl;
{    sigwait : Function(__set:plibc_sigset; __sig:plongint):longint;cdecl;}
{$ifndef ANDROID}
    pthread_atfork : Function(__prepare:tprocedure ; __parent:tprocedure ; __child:tprocedure ):longint;cdecl;
    pthread_kill_other_threads_np : procedure;cdecl;
{$endif}
    pthread_sigmask: Function(how: cint; nset: plibc_sigset; oset: plibc_sigset): cint;cdecl;

    sem_init     :   function (__sem:Psem_t; __pshared:longint; __value:dword):longint;cdecl;
    sem_destroy  :   function (__sem:Psem_t):longint;cdecl;
    sem_close    :   function (__sem:Psem_t):longint;cdecl;
    sem_unlink   :   function (__name:PAnsiChar):longint;cdecl;
    sem_wait     :   function (__sem:Psem_t):longint;cdecl;
    sem_timedwait:   function (__sem:Psem_t; __abs_timeout:ptimespec):longint;cdecl;
    sem_trywait  :   function (__sem:Psem_t):longint;cdecl;
    sem_post     :   function (__sem:Psem_t):longint;cdecl;
    sem_getvalue :   function (__sem:Psem_t; __sval:Plongint):longint;cdecl;

    pthread_mutexattr_settype : function(__attr: Ppthread_mutexattr_t; Kind:Integer): Integer; cdecl;
    pthread_setname_np : function(thread: pthread_t; name: PAnsiChar):cint;cdecl;
    pthread_timedjoin_np : function(__th:pthread_t; __thread_return:ppointer; __abstime:ptimespec):cint;cdecl;


Var
  PthreadDLL : TLibHandle;
  PthreadDLM : TDynLibsManager;

Function LoadPthreads : Boolean;

begin
  GetDynLibsManager(PthreadDLM);
{$ifndef ANDROID}
  PThreadDLL:=PthreadDLM.LoadLibraryA('libpthread.so.0');
{$else}
  PThreadDLL:=PthreadDLM.LoadLibraryA('libc.so');
{$endif}
  Result:=PThreadDLL<>NilHandle;
  If Not Result then
    exit;
  Pointer(pthread_create) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_create');
  Pointer(pthread_self) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_self');
  Pointer(pthread_equal) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_equal');
  Pointer(pthread_exit) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_exit');
  Pointer(pthread_join) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_join');
  Pointer(pthread_detach) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_detach');
  Pointer(pthread_attr_init) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_attr_init');
  Pointer(pthread_attr_destroy) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_attr_destroy');
  Pointer(pthread_attr_setdetachstate) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_attr_setdetachstate');
  Pointer(pthread_attr_getdetachstate) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_attr_getdetachstate');
  Pointer(pthread_attr_setschedparam) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_attr_setschedparam');
  Pointer(pthread_attr_getschedparam) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_attr_getschedparam');
  Pointer(pthread_attr_setschedpolicy) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_attr_setschedpolicy');
  Pointer(pthread_attr_getschedpolicy) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_attr_getschedpolicy');
{$ifndef ANDROID}
  Pointer(pthread_attr_setinheritsched) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_attr_setinheritsched');
  Pointer(pthread_attr_getinheritsched) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_attr_getinheritsched');
{$endif}
  Pointer(pthread_attr_setscope) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_attr_setscope');
  Pointer(pthread_attr_getscope) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_attr_getscope');
  Pointer(pthread_attr_setstacksize) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_attr_setstacksize');
  Pointer(pthread_setschedparam) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_setschedparam');
  Pointer(pthread_getschedparam) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_getschedparam');
  Pointer(pthread_mutex_init) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_mutex_init');
  Pointer(pthread_mutex_destroy) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_mutex_destroy');
  Pointer(pthread_mutex_trylock) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_mutex_trylock');
  Pointer(pthread_mutex_timedlock) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_mutex_timedlock');
  Pointer(pthread_mutex_lock) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_mutex_lock');
  Pointer(pthread_mutex_unlock) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_mutex_unlock');
  Pointer(pthread_mutexattr_init) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_mutexattr_init');
  Pointer(pthread_mutexattr_destroy) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_mutexattr_destroy');
{$ifndef ANDROID}
  Pointer(pthread_mutexattr_setkind_np) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_mutexattr_setkind_np');
  Pointer(pthread_mutexattr_getkind_np) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_mutexattr_getkind_np');
{$endif}
  Pointer(pthread_cond_init) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_cond_init');
  Pointer(pthread_cond_destroy) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_cond_destroy');
  Pointer(pthread_cond_signal) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_cond_signal');
  Pointer(pthread_cond_broadcast) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_cond_broadcast');
  Pointer(pthread_cond_wait) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_cond_wait');
  Pointer(pthread_cond_timedwait) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_cond_timedwait');
{$ifndef ANDROID}
  Pointer(pthread_condattr_init) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_condattr_init');
  Pointer(pthread_condattr_destroy) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_condattr_destroy');
  Pointer(pthread_condattr_setclock) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_condattr_setclock');
{$endif}
  Pointer(pthread_key_create) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_key_create');
  Pointer(pthread_key_delete) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_key_delete');
  Pointer(pthread_setspecific) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_setspecific');
  Pointer(pthread_getspecific) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_getspecific');
{  Pointer(pthread_once) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_once');}
{$ifndef ANDROID}
  Pointer(pthread_setcancelstate) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_setcancelstate');
  Pointer(pthread_setcanceltype) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_setcanceltype');
  Pointer(pthread_cancel) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_cancel');
  Pointer(pthread_testcancel) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_testcancel');
{$endif}
{  Pointer(_pthread_cleanup_push) := PthreadDLM.GetProcAddress(PthreadDLL,'_pthread_cleanup_push');}
{  Pointer(_pthread_cleanup_push_defer) := PthreadDLM.GetProcAddress(PthreadDLL,'_pthread_cleanup_push_defer');}
{  Pointer(pthread_sigmask) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_sigmask');}
  Pointer(pthread_kill)  := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_kill');
{$ifndef ANDROID}
  Pointer(pthread_atfork):= PthreadDLM.GetProcAddress(PthreadDLL,'pthread_atfork');
  Pointer(pthread_kill_other_threads_np) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_kill_other_threads_np');
{$endif}
  Pointer(pthread_sigmask) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_sigmask');
  Pointer(sem_init     ) := PthreadDLM.GetProcAddress(PthreadDLL,'sem_init');
  Pointer(sem_destroy  ) := PthreadDLM.GetProcAddress(PthreadDLL,'sem_destroy');
  Pointer(sem_close    ) := PthreadDLM.GetProcAddress(PthreadDLL,'sem_close');
  Pointer(sem_unlink   ) := PthreadDLM.GetProcAddress(PthreadDLL,'sem_unlink');
  Pointer(sem_wait     ) := PthreadDLM.GetProcAddress(PthreadDLL,'sem_wait');
  Pointer(sem_timedwait) := PthreadDLM.GetProcAddress(PthreadDLL,'sem_timedwait');
  Pointer(sem_trywait  ) := PthreadDLM.GetProcAddress(PthreadDLL,'sem_trywait');
  Pointer(sem_post     ) := PthreadDLM.GetProcAddress(PthreadDLL,'sem_post');
  Pointer(sem_getvalue ) := PthreadDLM.GetProcAddress(PthreadDLL,'sem_getvalue');
  Pointer(pthread_mutexattr_settype) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_mutexattr_settype');
  Pointer(pthread_setname_np) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_setname_np');
  Pointer(pthread_timedjoin_np) := PthreadDLM.GetProcAddress(PthreadDLL,'pthread_timedjoin_np');
end;

Function UnLoadPthreads : Boolean;

begin
  Result:=PthreadDLM.UnloadLibrary(PThreadDLL);
end;

{$endif}

