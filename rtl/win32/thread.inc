{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Multithreading implementation for Win32

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$ifdef MT}
const
   threadvarblocksize : dword = 0;

type
   tthreadinfo = record
      f : tthreadfunc;
      p : pointer;
   end;
   pthreadinfo = ^tthreadinfo;

var
   dataindex : dword;

{ import the necessary stuff from windows }
function TlsAlloc : DWord;external 'kernel32' name 'TlsAlloc';
function TlsGetValue(dwTlsIndex : DWord) : pointer;
  external 'kernel32' name 'TlsGetValue';
function TlsSetValue(dwTlsIndex : DWord;lpTlsValue : pointer) : LongBool;
  external 'kernel32' name 'TlsSetValue';
function TlsFree(dwTlsIndex : DWord) : LongBool;
  external 'kernel32' name 'TlsFree';
function CreateThread(lpThreadAttributes : pointer;
  dwStackSize : DWord; lpStartAddress : pointer;lpParameter : pointer;
  dwCreationFlags : DWord;var lpThreadId : DWord) : THandle;
  external 'kernel32' name 'CreateThread';
procedure ExitThread(dwExitCode : DWord);
  external 'kernel32' name 'ExitThread';

procedure init_threadvar(var offset : dword;size : dword);[public,alias: 'FPC_INIT_THREADVAR'];

  begin
     offset:=threadvarblocksize;
     inc(threadvarblocksize,size);
  end;


function relocate_threadvar(offset : dword) : pointer;[public,alias: 'FPC_RELOCATE_THREADVAR'];

  begin
     relocate_threadvar:=TlsGetValue(dataindex)+offset;
  end;

procedure AllocateThreadVars;

  var
     threadvars : pointer;

  begin
     { we've to allocate the memory from windows }
     { because the FPC heap management uses      }
     { exceptions which use threadvars but       }
     { these aren't allocated yet ...            }
     { allocate room on the heap for the thread vars }
     threadvars:=pointer(GlobalAlloc(GMEM_FIXED or GMEM_ZEROINIT,
       threadvarblocksize));
     TlsSetValue(dataindex,threadvars);
  end;

procedure InitThread;

  begin
     InitFPU;
     { we don't need to set the data to 0 because we did this with }
     { the fillchar above, but it looks nicer                      }

     { ExceptAddrStack and ExceptObjectStack are threadvars       }
     { so every thread has its on exception handling capabilities }
     InitExceptions;
     InOutRes:=0;
     // ErrNo:=0;
  end;

procedure DoneThread;

  var
     threadvars : pointer;

  begin
     { release thread vars }
     threadvars:=TlsGetValue(dataindex);
     GlobalFree(threadvars);
  end;

function ThreadMain(param : pointer) : dword;stdcall;

  var
     ti : tthreadinfo;

  begin
{$ifdef DEBUG_MT}
     writeln('New thread started, initialising ...');
{$endif DEBUG_MT}
     AllocateThreadVars;
     InitThread;
     ti:=pthreadinfo(param)^;
     dispose(pthreadinfo(param));
{$ifdef DEBUG_MT}
     writeln('Jumping to thread function');
{$endif DEBUG_MT}
     ThreadMain:=ti.f(ti.p);
  end;


function BeginThread(sa : Pointer;stacksize : dword;
  ThreadFunction : tthreadfunc;p : pointer;creationFlags : dword;
  var ThreadId : DWord) : DWord;

  var
     ti : pthreadinfo;

  begin
{$ifdef DEBUG_MT}
     writeln('Creating new thread');
{$endif DEBUG_MT}
     IsMultithreaded:=true;
     { the only way to pass data to the newly created thread }
     { in a MT safe way, is to use the heap                  }
     new(ti);
     ti^.f:=ThreadFunction;
     ti^.p:=p;
{$ifdef DEBUG_MT}
     writeln('Starting new thread');
{$endif DEBUG_MT}
     BeginThread:=CreateThread(sa,stacksize,@ThreadMain,ti,
       creationflags,threadid);
  end;

function BeginThread(ThreadFunction : tthreadfunc) : DWord;

  var
     dummy : dword;

  begin
     BeginThread:=BeginThread(nil,0,ThreadFunction,nil,0,dummy);
  end;

function BeginThread(ThreadFunction : tthreadfunc;p : pointer) : DWord;

  var
     dummy : dword;

  begin
     BeginThread:=BeginThread(nil,0,ThreadFunction,p,0,dummy);
  end;

function BeginThread(ThreadFunction : tthreadfunc;p : pointer;
  var ThreadId : DWord) : DWord;

  begin
     BeginThread:=BeginThread(nil,0,ThreadFunction,p,0,ThreadId);
  end;

procedure EndThread(ExitCode : DWord);

  begin
     DoneThread;
     ExitThread(ExitCode);
  end;

procedure EndThread;

  begin
     EndThread(0);
  end;

{ we implement these procedures for win32 by importing them }
{ directly from windows                                     }

procedure InitCriticalsection(var cs : tcriticalsection);

  begin
  end;

procedure DoneCriticalsection(var cs : tcriticalsection);

  begin
  end;

procedure EnterCriticalsection(var cs : tcriticalsection);

  begin
  end;

procedure LeaveCriticalsection(var cs : tcriticalsection);

  begin
  end;

{
procedure InitCriticalSection(var cs : tcriticalsection);
  external 'kernel32' name 'InitializeCriticalSection';

procedure DoneCriticalSection(var cs : tcriticalsection);
  external 'kernel32' name 'DeleteCriticalSection';

procedure EnterCriticalSection(var cs : tcriticalsection);
  external 'kernel32' name 'EnterCriticalSection';

procedure LeaveCriticalSection(var cs : tcriticalsection);
  external 'kernel32' name 'LeaveCriticalSection';
}

{$endif MT}

{
  $Log$
  Revision 1.2  2001-01-24 21:47:38  florian
    + more MT stuff added

  Revision 1.1  2001/01/01 19:06:36  florian
    + initial release
}
