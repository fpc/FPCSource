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

 TYPE
    pthread_t       = pointer;
    ppthread_t      = ^pthread_t;
    pthread_key_t   = cint;
    ppthread_key_t  = ^pthread_key_t;
    pthread_mutex_t = pointer;
    ppthread_mutex_t= ^pthread_mutex_t;
    pthread_attr_t  = pointer; // opague
    ppthread_attr_t = ^pthread_attr_t; // opague
    __destr_func_t  = procedure (p :pointer);cdecl;
    __startroutine_t= function (p :pointer):pointer;cdecl;
    pthread_mutex_attr_t  = pointer;
    ppthread_mutex_attr_t = ^pthread_mutex_t;

function  pthread_getspecific      (t : pthread_key_t):pointer; cdecl; external;
function  pthread_setspecific      (t : pthread_key_t;p:pointer):cint; cdecl; external;
function  pthread_key_create       (p : ppthread_key_t;f: __destr_func_t):cint; cdecl;external;
function  pthread_attr_init           (p : ppthread_key_t):cint; cdecl; external;
function  pthread_attr_setinheritsched(p : ppthread_attr_t;i:cint):cint; cdecl; external;
function  pthread_attr_setscope      (p : ppthread_attr_t;i:cint):cint;cdecl;external;
function  pthread_attr_setdetachstate (p : ppthread_attr_t;i:cint):cint;cdecl;external;
function  pthread_create ( p: ppthread_t;attr : ppthread_attr_t;f:__startroutine_t;arg:pointer):cint;cdecl;external;
procedure pthread_exit  ( p: pointer); cdecl;external;
function  pthread_self:cint; cdecl;external;
function  pthread_mutex_init (p:ppthread_mutex_t;o:ppthread_mutex_attr_t):cint; cdecl;external;
function  pthread_mutex_destroy (p:ppthread_mutex_attr_t):cint; cdecl;external;
function  pthread_mutex_lock    (p:ppthread_mutex_attr_t):cint; cdecl;external;
function  pthread_mutex_unlock  (p:ppthread_mutex_attr_t):cint; cdecl;external;
function  pthread_cancel(_para1:pthread_t):cint;cdecl;external;
function  pthread_detach(_para1:pthread_t):cint;cdecl;external;
function  pthread_join(_para1:pthread_t; _para2:Ppointer):cint;cdecl;external;

{
  $Log$
  Revision 1.1  2003-11-18 22:33:42  marco
   * moved from systhrds.pp


}