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
function TlsAlloc : DWord;
  external 'kernel32' name 'TlsAlloc';
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
function GlobalAlloc(uFlags:UINT; dwBytes:DWORD):Pointer;
  external 'kernel32' name 'GlobalAlloc';
function GlobalFree(hMem : Pointer):Pointer; external 'kernel32' name 'GlobalFree';

const
  { GlobalAlloc, GlobalFlags  }
  GMEM_FIXED = 0;
  GMEM_ZEROINIT = 64;

procedure init_threadvar(var offset : dword;size : dword);[public,alias: 'FPC_INIT_THREADVAR'];

  begin
     offset:=threadvarblocksize;
     inc(threadvarblocksize,size);
  end;


type
   ltvInitEntry = packed record
      varaddr : pdword;
      size    : longint;
   end;
   pltvInitEntry = ^ltvInitEntry;

procedure init_unit_threadvars (tableEntry : pltvInitEntry);
begin
  while tableEntry^.varaddr <> nil do
  begin
    init_threadvar (tableEntry^.varaddr^, tableEntry^.size);
    inc (pchar (tableEntry), sizeof (tableEntry^));
  end;
end;

type TltvInitTablesTable =
  record
    count : dword;
    tables: array [1..32767] of pltvInitEntry;
  end;

var
  ThreadvarTablesTable : TltvInitTablesTable; external name 'FPC_LOCALTHREADVARTABLES';

procedure init_all_unit_threadvars; [public,alias: 'FPC_INITIALIZELOCALTHREADVARS'];
var i : integer;
begin
  {$ifdef DEBUG_MT}
  WriteLn ('init_all_unit_threadvars (%d) units',ThreadvarTablesTable.count);
  {$endif}
  for i := 1 to ThreadvarTablesTable.count do
    init_unit_threadvars (ThreadvarTablesTable.tables[i]);
end;


function relocate_threadvar(offset : dword) : pointer;[public,alias: 'FPC_RELOCATE_THREADVAR'];

  begin
     asm
        pushal
     end;
     relocate_threadvar:=TlsGetValue(dataindex)+offset;
     asm
        popal
     end;
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

procedure ReleaseThreadVars;

  var
     threadvars : pointer;

  begin
     { release thread vars }
     threadvars:=TlsGetValue(dataindex);
     GlobalFree(threadvars);
  end;

procedure InitThread;

  begin
     InitFPU;
     { we don't need to set the data to 0 because we did this with }
     { the fillchar above, but it looks nicer                      }

     { ExceptAddrStack and ExceptObjectStack are threadvars        }
     { so every thread has its own exception handling capabilities }
     InitExceptions;
     InOutRes:=0;
     // ErrNo:=0;
  end;

procedure DoneThread;

  begin
     { release thread vars }
     ReleaseThreadVars;
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
     DoneThread;
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
     IsMultithread:=true;
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

function BeginThread(ThreadFunction : tthreadfunc;p : pointer;
  var ThreadId : Longint) : DWord;
  begin
     BeginThread:=BeginThread(nil,0,ThreadFunction,p,0,DWord(ThreadId));
  end;


function BeginThread(sa : Pointer;stacksize : dword;
  ThreadFunction : tthreadfunc;p : pointer;creationFlags : dword;
  var ThreadId : Longint) : DWord;
  begin
     BeginThread:=BeginThread(sa,stacksize,ThreadFunction,p,creationflags,DWord(threadid));
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
procedure InitCriticalSection(var cs : TRTLCriticalSection);
  external 'kernel32' name 'InitializeCriticalSection';

procedure DoneCriticalSection(var cs : TRTLCriticalSection);
  external 'kernel32' name 'DeleteCriticalSection';

procedure EnterCriticalSection(var cs : TRTLCriticalSection);
  external 'kernel32' name 'EnterCriticalSection';

procedure LeaveCriticalSection(var cs : TRTLCriticalSection);
  external 'kernel32' name 'LeaveCriticalSection';

{$endif MT}

{
  $Log$
  Revision 1.9  2002-07-28 20:43:50  florian
    * several fixes for linux/powerpc
    * several fixes to MT

  Revision 1.8  2002/03/31 10:03:13  armin
  + call to DoneThread was missing

  Revision 1.7  2002/03/28 16:31:35  armin
  + initialize threadvars defined local in units

  Revision 1.6  2001/10/23 21:51:03  peter
    * criticalsection renamed to rtlcriticalsection for kylix compatibility

  Revision 1.5  2001/10/09 02:38:39  carl
  * bugfix #1639 (IsMultiThread varialbe setting)

  Revision 1.4  2001/01/26 21:02:21  florian
  *** empty log message ***

  Revision 1.3  2001/01/26 16:38:03  florian
  *** empty log message ***

  Revision 1.2  2001/01/24 21:47:38  florian
    + more MT stuff added

  Revision 1.1  2001/01/01 19:06:36  florian
    + initial release
}
