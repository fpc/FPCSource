{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Peter Vreman
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This file contains a pthread.h headerconversion,
    and should contain an interface to the threading library to be
    used by systhrd, preferably in a somewhat compatible notation
    (compared to the other OSes).

    As a start, I simply used libc_r

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

  const 
    PTHREAD_EXPLICIT_SCHED       = 0;
    PTHREAD_CREATE_DETACHED      = 1;
    PTHREAD_SCOPE_PROCESS        = 0;

  type
    pthread_t = culong;
  
    sched_param = record
      __sched_priority: cint;
    end;
  
    pthread_attr_t = record
      __detachstate: cint;
      __schedpolicy: cint;
      __schedparam: sched_param;
      __inheritsched: cint;
      __scope: cint;
      __guardsize: size_t;
      __stackaddr_set: cint;
      __stackaddr: pointer;
      __stacksize: size_t;
    end;
  
    _pthread_fastlock = record
      __status: clong;
      __spinlock: cint;
    end;
  
    pthread_mutex_t = record
      __m_reserved: cint;
      __m_count: cint;
      __m_owner: pointer;
      __m_kind:  cint;
      __m_lock: _pthread_fastlock;
    end;
  
    pthread_mutexattr_t = record
      __mutexkind: cint;
    end;
  
    pthread_cond_t = record
      __c_lock: _pthread_fastlock;
      __c_waiting: pointer;
      __padding: array[0..48-1-sizeof(_pthread_fastlock)-sizeof(pointer)-sizeof(clonglong)] of byte;
      __align: clonglong;
    end;
  
    pthread_condattr_t = record
      __dummy: cint;
    end;
  
    pthread_key_t = cuint;
  
    pthread_rwlock_t = record
      __rw_readers: cint;
      __rw_writer: pointer;
      __rw_read_waiting: pointer;
      __rw_write_waiting: pointer;
      __rw_kind: cint;
      __rw_pshared: cint;
    end;
  
    pthread_rwlockattr_t = record
      __lockkind: cint;
      __pshared: cint;
    end;
  
    sem_t = record
       __sem_lock: _pthread_fastlock;
       __sem_value: cint;
       __sem_waiting: pointer;
    end;
  
    ppthread_t           = ^pthread_t;
    ppthread_key_t       = ^pthread_key_t;
    ppthread_mutex_t     = ^pthread_mutex_t;
    ppthread_attr_t      = ^pthread_attr_t;
    __destr_func_t       = procedure (p :pointer);cdecl;
    __startroutine_t     = function (p :pointer):pointer;cdecl;
    ppthread_mutexattr_t = ^pthread_mutexattr_t;
    ppthread_cond_t      = ^pthread_cond_t;
    ppthread_condattr_t  = ^pthread_condattr_t;
    
    pthread_t = culong;
  
    sched_param = record
      __sched_priority: cint;
    end;
  
    pthread_attr_t = record
      __detachstate: cint;
      __schedpolicy: cint;
      __schedparam: sched_param;
      __inheritsched: cint;
      __scope: cint;
      __guardsize: size_t;
      __stackaddr_set: cint;
      __stackaddr: pointer;
      __stacksize: size_t;
    end;
  
    _pthread_fastlock = record
      __status: clong;
      __spinlock: cint;
    end;
  
    pthread_mutex_t = record
      __pthread_mutex_flags : record
           __pthread_mutex_flag1 : word;
           __pthread_mutex_flag2 : byte;
           __pthread_mutex_ceiling : byte;
           __pthread_mutex_type : word;
           __pthread_mutex_magic : word;
        end;
      __pthread_mutex_lock : record
          case longint of
             0 : ( __pthread_mutex_lock64 : record
                  __pthread_mutex_pad : array[0..7] of byte;
               end );
             1 : ( __pthread_mutex_lock32 : record
                  __pthread_ownerpid : dword;
                  __pthread_lockword : dword;
               end );
             2 : ( __pthread_mutex_owner64 : qword );
          end;
      __pthread_mutex_data : qword;
    end;
  
    pthread_spinlock_t = pthread_mutex_t;
  
    pthread_mutexattr_t = record
      __mutexkind: cint;
    end;
  
    pthread_cond_t = record
      __c_lock: _pthread_fastlock;
      __c_waiting: pointer;
      __padding: array[0..48-1-sizeof(_pthread_fastlock)-sizeof(pointer)-sizeof(clonglong)] of byte;
      __align: clonglong;
    end;
  
    pthread_condattr_t = record
      __dummy: cint;
    end;
  
    pthread_key_t = cuint;
  
    pthread_rwlock_t = record
      __rw_readers: cint;
      __rw_writer: pointer;
      __rw_read_waiting: pointer;
      __rw_write_waiting: pointer;
      __rw_kind: cint;
      __rw_pshared: cint;
    end;
  
    pthread_rwlockattr_t = record
      __lockkind: cint;
      __pshared: cint;
    end;
  
    _once = record
       __pthread_once_pad : array[0..3] of upad64_t;
    end;
    pthread_once_t = _once;
  
    pthread_barrier_t = record
       __pthread_barrier_count : uint32_t;
       __pthread_barrier_current : uint32_t;
       __pthread_barrier_cycle : upad64_t;
       __pthread_barrier_reserved : upad64_t;
       __pthread_barrier_lock : pthread_mutex_t;
       __pthread_barrier_cond : pthread_cond_t;
    end;
  
    pthread_barrierattr_t = record
       __pthread_barrierattrp : pointer;
    end;
  
    sem_t = record
       __sem_lock: _pthread_fastlock;
       __sem_value: cint;
       __sem_waiting: pointer;
    end;

    sem_t       = cint;
    psem_t          = ^sem_t;
    TSemaphore  = sem_t;
    PSemaphore  = ^TSemaphore;

function  pthread_getspecific      (t : pthread_key_t):pointer; cdecl; external 'c';
function  pthread_setspecific      (t : pthread_key_t;p:pointer):cint; cdecl; external 'c';
function  pthread_key_create       (p : ppthread_key_t;f: __destr_func_t):cint; cdecl;external 'c';
function  pthread_attr_init           (p : ppthread_key_t):cint; cdecl; external 'c';
function  pthread_attr_setinheritsched(p : ppthread_attr_t;i:cint):cint; cdecl; external 'c';
function  pthread_attr_setscope      (p : ppthread_attr_t;i:cint):cint;cdecl;external 'c';
function  pthread_attr_setdetachstate (p : ppthread_attr_t;i:cint):cint;cdecl;external 'c';
function  pthread_create ( p: ppthread_t;attr : ppthread_attr_t;f:__startroutine_t;arg:pointer):cint;cdecl;external 'c';
procedure pthread_exit  ( p: pointer); cdecl;external 'c';
function  pthread_self:cint; cdecl;external 'c';
function  pthread_mutex_init (p:ppthread_mutex_t;o:ppthread_mutexattr_t):cint; cdecl;external 'c';
function  pthread_mutex_destroy (p:ppthread_mutexattr_t):cint; cdecl;external 'c';
function  pthread_mutex_lock    (p:ppthread_mutexattr_t):cint; cdecl;external 'c';
function  pthread_mutex_unlock  (p:ppthread_mutexattr_t):cint; cdecl;external 'c';
function  pthread_cancel(_para1:pthread_t):cint;cdecl;external 'c';
function  pthread_detach(_para1:pthread_t):cint;cdecl;external 'c';
function  pthread_join(_para1:pthread_t; _para2:Ppointer):cint;cdecl;external 'c';
function  pthread_cond_destroy(_para1:Ppthread_cond_t):cint;cdecl;external 'c' name 'pthread_cond_destroy';
function  pthread_cond_init(_para1:Ppthread_cond_t;_para2:Ppthread_condattr_t):cint;cdecl;external  'c' name 'pthread_cond_init';
function  pthread_cond_signal(_para1:Ppthread_cond_t):cint;cdecl;external 'c' name 'pthread_cond_signal';
function  pthread_cond_wait(_para1:Ppthread_cond_t;_para2:Ppthread_mutex_t):cint;cdecl;external 'c' name 'pthread_cond_wait';

function sem_init(__sem:Psem_t; __pshared:cint;__value:cuint):cint;cdecl; external 'c' name 'sem_init';
function sem_destroy(__sem:Psem_t):cint;cdecl;external 'c' name 'sem_destroy';
function sem_close(__sem:Psem_t):cint;cdecl;external 'c'  name 'sem_close';
function sem_unlink(__name:Pchar):cint;cdecl;external 'c' name 'sem_unlink';
function sem_wait(__sem:Psem_t):cint;cdecl;external 'c'  name 'sem_wait';
function sem_trywait(__sem:Psem_t):cint;cdecl;external 'c'  name 'sem_trywait';
function sem_post(__sem:Psem_t):cint;cdecl;external 'c'  name 'sem_post';
function sem_getvalue(__sem:Psem_t; __sval:Pcint):cint;cdecl;external 'c'  name 'sem_getvalue';

function pthread_mutexattr_init(_para1:Ppthread_mutexattr_t):cint;cdecl;external 'c' name 'pthread_mutexattr_init';
function pthread_mutexattr_destroy(_para1:Ppthread_mutexattr_t):cint;cdecl;external 'c' name 'pthread_mutexattr_destroy';
function pthread_mutexattr_gettype(_para1:Ppthread_mutexattr_t; _para2:Pcint):cint;cdecl;external 'c' name 'pthread_mutexattr_gettype';
function pthread_mutexattr_settype(_para1:Ppthread_mutexattr_t; _para2:cint):cint;cdecl;external 'c' name 'pthread_mutexattr_settype';
function pthread_cond_timedwait(__cond:ppthread_cond_t; __mutex:ppthread_mutex_t; __abstime:ptimespec):cint; cdecl;external 'c' name 'pthread_cond_timedwait'; 

