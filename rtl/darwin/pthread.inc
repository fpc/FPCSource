{
    $Id$
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

CONST PTHREAD_EXPLICIT_SCHED       = 0;
      PTHREAD_CREATE_DETACHED      = 1;
      PTHREAD_SCOPE_PROCESS        = 0;
      PTHREAD_MUTEX_RECURSIVE      = 2;

 TYPE
    pthread_t       = pointer;
    ppthread_t      = ^pthread_t;
    pthread_key_t   = cint;
    ppthread_key_t  = ^pthread_key_t;
    pthread_mutex_t = TRTLCriticalSection;
    ppthread_mutex_t= PRTLCriticalSection;
    pthread_attr_t  = pointer; // opague
    ppthread_attr_t = ^pthread_attr_t; // opague
    __destr_func_t  = procedure (p :pointer);cdecl;
    __startroutine_t= function (p :pointer):pointer;cdecl;
    pthread_mutex_attr_t  = pointer;
    ppthread_mutex_attr_t = ^pthread_mutex_t;
    ppthread_mutexattr_t  = ^pthread_mutex_t;

    sem_t	    = cint;
    psem_t	    = ^sem_t;
   TSemaphore         = sem_t;
    PSemaphore         = ^TSemaphore;

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
function  pthread_mutex_init (p:ppthread_mutex_t;o:ppthread_mutex_attr_t):cint; cdecl;external 'c';
function  pthread_mutex_destroy (p:ppthread_mutex_attr_t):cint; cdecl;external 'c';
function  pthread_mutex_lock    (p:ppthread_mutex_attr_t):cint; cdecl;external 'c';
function  pthread_mutex_unlock  (p:ppthread_mutex_attr_t):cint; cdecl;external 'c';
function  pthread_cancel(_para1:pthread_t):cint;cdecl;external 'c';
function  pthread_detach(_para1:pthread_t):cint;cdecl;external 'c';
function  pthread_join(_para1:pthread_t; _para2:Ppointer):cint;cdecl;external 'c';

function sem_init(__sem:Psem_t; __pshared:cint;__value:cuint):cint;cdecl; external;
function sem_destroy(__sem:Psem_t):cint;cdecl;external ;
function sem_close(__sem:Psem_t):cint;cdecl;external ;
function sem_unlink(__name:Pchar):cint;cdecl;external ;
function sem_wait(__sem:Psem_t):cint;cdecl;external ;
function sem_trywait(__sem:Psem_t):cint;cdecl;external ;
function sem_post(__sem:Psem_t):cint;cdecl;external ;
function sem_getvalue(__sem:Psem_t; __sval:Pcint):cint;cdecl;external;


function pthread_mutexattr_init(_para1:Ppthread_mutexattr_t):cint;cdecl;external;
function pthread_mutexattr_destroy(_para1:Ppthread_mutexattr_t):cint;cdecl;external;
function pthread_mutexattr_gettype(_para1:Ppthread_mutexattr_t; _para2:Pcint):cint;cdecl;external;
function pthread_mutexattr_settype(_para1:Ppthread_mutexattr_t; _para2:cint):cint;cdecl;external;


{
  $Log$
  Revision 1.3  2004-05-26 20:05:21  marco
   * 2nd attempt

  Revision 1.2  2004/05/26 19:23:17  marco
   * first iteration

  Revision 1.1  2004/01/04 20:05:38  jonas
    * first working version of the Darwin/Mac OS X (for PowerPC) RTL
      Several non-essential units are still missing, but make cycle works

  Revision 1.2  2003/11/27 20:24:10  michael
  + Redefined ppthread_mutex_t

  Revision 1.1  2003/11/18 22:33:42  marco
   * moved from systhrds.pp


}