{
    $Id$
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
     PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP : array [0..5]of Integer = (0, 0, 0, 1, 0, 0);

  type
     TThreadPriority = (tpIdle, tpLowest, tpLower, tpNormal, tpHigher, tpHighest, tpTimeCritical);

  const
     Priorities: array [TThreadPriority] of Integer = (
       THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST, THREAD_PRIORITY_BELOW_NORMAL,
       THREAD_PRIORITY_NORMAL, THREAD_PRIORITY_ABOVE_NORMAL,
       THREAD_PRIORITY_HIGHEST, THREAD_PRIORITY_TIME_CRITICAL
     );

  type
     psched_param = ^sched_param;
     sched_param = record
        sched_priority : LongInt;
     end;

     ptimespec = ^timespec;
     timespec = record
        tv_sec : LongInt;
        tv_nsec : LongInt;
     end;

     psigset_t = ^sigset_t;
     sigset_t = DWORD; // unsigned long 32 bits

  const
     _POSIX_THREAD_THREADS_MAX = 64;
     PTHREAD_THREADS_MAX = 512;
     _POSIX_THREAD_KEYS_MAX = 128;
     PTHREAD_KEYS_MAX = 128;

  type
    pthread_t = pointer;
    ppthread_t = ^pthread_t;

     p_pthread_queue = ^_pthread_queue;
     _pthread_queue = record
          head : pthread_t;
          tail : pthread_t;
       end;

     ppthread_mutex_t = PRtlCriticalSection;
     pthread_mutex_t = TRtlCriticalSection;

     ppthread_cond_t = ^pthread_cond_t;
     pthread_cond_t = record
          c_spinlock : longint;
          c_waiting : _pthread_queue;
       end;

     { Attributes  }

    const
      PTHREAD_CREATE_JOINABLE = 0;
      PTHREAD_CREATE_DETACHED = 1;
      PTHREAD_INHERIT_SCHED   = 0;
      PTHREAD_EXPLICIT_SCHED  = 1;
      PTHREAD_SCOPE_SYSTEM    = 0;
      PTHREAD_SCOPE_PROCESS   = 1;

    type
       size_t = longint;

       ppthread_attr_t = ^pthread_attr_t;
       pthread_attr_t = record
            detachstate : longint;
            schedpolicy : longint;
            schedparam : sched_param;
            inheritsched : longint;
            scope : longint;
            __guardsize : size_t;
            __stackaddr_set : longint;
            __stackaddr : pointer;
            __stacksize : size_t;
         end;

       ppthread_mutexattr_t = ^pthread_mutexattr_t;
       pthread_mutexattr_t = record
            mutexkind : longint;
         end;


       ppthread_condattr_t = ^pthread_condattr_t;
       pthread_condattr_t = record
            dummy : longint;
         end;

       ppthread_key_t = ^pthread_key_t;
       pthread_key_t = cardinal;

       ppthread_once_t = ^pthread_once_t;
       pthread_once_t = longint;

    const
       PTHREAD_ONCE_INIT = 0;

    type
       tpcb_routine = Procedure(P:Pointer); cdecl;

       p_pthread_cleanup_buffer = ^_pthread_cleanup_buffer;
       _pthread_cleanup_buffer = record
          routine : tpcb_routine;             { Function to call. }
          arg : Pointer;                      { Its argument.  }
          canceltype:LongInt;                 { Saved cancellation type. }
          prev : p_pthread_cleanup_buffer; { Chaining of cleanup functions.  }
       end;

     __start_routine_t = function (_para1:pointer):pointer;cdecl;
     __destr_function_t = procedure (_para1:pointer);
     t_pthread_cleanup_push_routine = procedure (_para1:pointer);
     t_pthread_cleanup_push_defer_routine = procedure (_para1:pointer);

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
    function pthread_attr_setinheritsched(__attr:ppthread_attr_t; __inherit:longint):longint;cdecl;external;
    function pthread_attr_getinheritsched(__attr:ppthread_attr_t; __inherit:plongint):longint;cdecl;external;
    function pthread_attr_setscope(__attr:ppthread_attr_t; __scope:longint):longint;cdecl;external;
    function pthread_attr_getscope(__attr:ppthread_attr_t; __scope:plongint):longint;cdecl;external;
    function pthread_setschedparam(__target_thread:pthread_t; __policy:longint; __param:psched_param):longint;cdecl;external;
    function pthread_getschedparam(__target_thread:pthread_t; __policy:plongint; __param:psched_param):longint;cdecl;external;
    function pthread_mutex_init(__mutex:ppthread_mutex_t; __mutex_attr:ppthread_mutexattr_t):longint;cdecl;external;
    function pthread_mutex_destroy(__mutex:ppthread_mutex_t):longint;cdecl;external;
    function pthread_mutex_trylock(__mutex:ppthread_mutex_t):longint;cdecl;external;
    function pthread_mutex_lock(__mutex:ppthread_mutex_t):longint;cdecl;external;
    function pthread_mutex_unlock(__mutex:ppthread_mutex_t):longint;cdecl;external;
    function pthread_mutexattr_init(__attr:ppthread_mutexattr_t):longint;cdecl;external;
    function pthread_mutexattr_destroy(__attr:ppthread_mutexattr_t):longint;cdecl;external;
    function pthread_mutexattr_setkind_np(__attr:ppthread_mutexattr_t; __kind:longint):longint;cdecl;external;
    function pthread_mutexattr_getkind_np(__attr:ppthread_mutexattr_t; __kind:plongint):longint;cdecl;external;
    function pthread_cond_init(__cond:ppthread_cond_t; __cond_attr:ppthread_condattr_t):longint;cdecl;external;
    function pthread_cond_destroy(__cond:ppthread_cond_t):longint;cdecl;external;
    function pthread_cond_signal(__cond:ppthread_cond_t):longint;cdecl;external;
    function pthread_cond_broadcast(__cond:ppthread_cond_t):longint;cdecl;external;
    function pthread_cond_wait(__cond:ppthread_cond_t; __mutex:ppthread_mutex_t):longint;cdecl;external;
    function pthread_cond_timedwait(__cond:ppthread_cond_t; __mutex:ppthread_mutex_t; __abstime:ptimespec):longint;cdecl;external;
    function pthread_condattr_init(__attr:ppthread_condattr_t):longint;cdecl;external;
    function pthread_condattr_destroy(__attr:ppthread_condattr_t):longint;cdecl;external;
    function pthread_key_create(__key:ppthread_key_t; __destr_function:__destr_function_t):longint;cdecl;external;
    function pthread_key_delete(__key:pthread_key_t):longint;cdecl;external;
    function pthread_setspecific(__key:pthread_key_t; __pointer:pointer):longint;cdecl;external;
    function pthread_getspecific(__key:pthread_key_t):pointer;cdecl;external;
    function pthread_once(__once_control:ppthread_once_t; __init_routine:tprocedure ):longint;cdecl;external;
    function pthread_setcancelstate(__state:longint; __oldstate:plongint):longint;cdecl;external;
    function pthread_setcanceltype(__type:longint; __oldtype:plongint):longint;cdecl;external;
    function pthread_cancel(__thread:pthread_t):longint;cdecl;external;
    procedure pthread_testcancel;cdecl;external;
    procedure _pthread_cleanup_push(__buffer:p_pthread_cleanup_buffer;__routine:t_pthread_cleanup_push_routine; __arg:pointer);cdecl;external;
    procedure _pthread_cleanup_push_defer(__buffer:p_pthread_cleanup_buffer;__routine:t_pthread_cleanup_push_defer_routine; __arg:pointer);cdecl;external;
    function pthread_sigmask(__how:longint; __newmask:psigset_t; __oldmask:psigset_t):longint;cdecl;external;
    function pthread_kill(__thread:pthread_t; __signo:longint):longint;cdecl;external;
    function sigwait(__set:psigset_t; __sig:plongint):longint;cdecl;external;
    function pthread_atfork(__prepare:tprocedure ; __parent:tprocedure ; __child:tprocedure ):longint;cdecl;external;
    procedure pthread_kill_other_threads_np;cdecl;external;

{
  $Log$
  Revision 1.1  2002-10-18 18:03:57  marco
   * From the original systhrds code.
     Turned out to be not entirely the same as *BSD's (constants with
     different names, existing and non existing constants), so I decided to
     make it platform dependant.


}