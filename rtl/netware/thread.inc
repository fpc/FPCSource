{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001-2002 by the Free Pascal development team.

    Multithreading implementation for NetWare

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$ifdef MT}

{ Multithreading for netware, armin 16 Mar 2002
  - threads are basicly tested and working
  - threadvars should work but currently there is a bug in the
    compiler preventing using multithreading
  - TRTLCriticalSections are working but NEVER call Enter or
    LeaveCriticalSection with uninitialized CriticalSections.
    Critial Sections are based on local semaphores and the
    Server will abend if the semaphore handles are invalid. There
    are basic tests in the rtl but this will not work in every case.
    Not closed semaphores will be closed by the rtl on program
    termination because some versions of netware will abend if there
    are open semaphores on nlm unload.
}

const
   threadvarblocksize : dword = 0;  // total size of allocated threadvars

type
   tthreadinfo = record
      f : tthreadfunc;
      p : pointer;
   end;
   pthreadinfo = ^tthreadinfo;

{ all needed import stuff is in nwsys.inc and already included by
  system.pp }


procedure init_threadvar(var offset : dword;size : dword);[public,alias: 'FPC_INIT_THREADVAR'];
begin
  offset:=threadvarblocksize;
  inc(threadvarblocksize,size);
  {$ifdef DEBUG_MT}
  ConsolePrintf3(#13'init_threadvar, new offset: (%d), Size:%d'#13#10,offset,size,0);
  {$endif DEBUG_MT}
end;

type ltvInitEntry = 
  record
    varaddr : pdword;
    size    : longint;
  end;
  pltvInitEntry = ^ltvInitEntry;

procedure init_unit_threadvars (tableEntry : pltvInitEntry);
begin
  while tableEntry^.varaddr <> nil do
  begin
    {$ifdef DEBUG_MT}
    ConsolePrintf3(#13'init_unit_threadvars, size: %d, addr: %d'#13#10,tableEntry^.size,dword(tableEntry^.varaddr),0);
    {$endif}
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
  ConsolePrintf(#13'init_all_unit_threadvars (%d) units'#13#10,ThreadvarTablesTable.count);
  {$endif}
  for i := 1 to ThreadvarTablesTable.count do
    init_unit_threadvars (ThreadvarTablesTable.tables[i]);
  
end;

{$ifdef DEBUG_MT}
var dummy_buff : array [0..255] of char;  // to avoid abends (for current compiler error that not all threadvars are initialized)
{$endif}

function relocate_threadvar(offset : dword) : pointer;[public,alias: 'FPC_RELOCATE_THREADVAR'];
begin
 {$ifdef DEBUG_MT}
   ConsolePrintf(#13'relocate_threadvar, offset: (%d)'#13#10,offset);
   if offset > threadvarblocksize then
   begin
     ConsolePrintf(#13'relocate_threadvar, invalid offset'#13#10,0);
     relocate_threadvar := @dummy_buff;
     exit;
   end;
 {$endif DEBUG_MT}
  relocate_threadvar:=_GetThreadDataAreaPtr + offset;
end;

procedure AllocateThreadVars;

  var
     threadvars : pointer;

  begin
     { we've to allocate the memory from netware }
     { because the FPC heap management uses      }
     { exceptions which use threadvars but       }
     { these aren't allocated yet ...            }
     { allocate room on the heap for the thread vars }
     threadvars := _malloc (threadvarblocksize);
     fillchar (threadvars^, threadvarblocksize, 0);
     _SaveThreadDataAreaPtr (threadvars);
     {$ifdef DEBUG_MT}
       ConsolePrintf(#13'threadvars allocated at (%x)'#13#10,longint(threadvars));
       ConsolePrintf(#13'size of threadvars: %d'#13#10,threadvarblocksize);
     {$endif DEBUG_MT}
  end;

procedure ReleaseThreadVars;

  var
     threadvars : pointer;

  begin
     { release thread vars }
     threadvars:=_GetThreadDataAreaPtr;
     _Free (threadvars);
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
  end;


function BeginThread(sa : Pointer;stacksize : dword;
  ThreadFunction : tthreadfunc;p : pointer;creationFlags : dword;
  var ThreadId : DWord) : DWord;

  var ti : pthreadinfo;

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
     BeginThread :=
       _BeginThread (@ThreadMain,NIL,Stacksize,ti);
  end;

function BeginThread(ThreadFunction : tthreadfunc) : DWord;
var dummy : dword;
begin
  BeginThread:=BeginThread(nil,0,ThreadFunction,nil,0,dummy);
end;

function BeginThread(ThreadFunction : tthreadfunc;p : pointer) : DWord;
var dummy : dword;
begin
  BeginThread:=BeginThread(nil,0,ThreadFunction,p,0,dummy);
end;

function BeginThread(ThreadFunction : tthreadfunc;p : pointer;var ThreadId : DWord) : DWord;
begin
  BeginThread:=BeginThread(nil,0,ThreadFunction,p,0,ThreadId);
end;

procedure EndThread(ExitCode : DWord);
begin
  DoneThread;
  ExitThread(ExitCode, TSR_THREAD);
end;

procedure EndThread;
begin
  EndThread(0);
end;


{ netware requires all allocated semaphores }
{ to be closed before terminating the nlm, otherwise }
{ the server will abend (except for netware 6 i think) }

TYPE TSemaList = ARRAY [1..1000] OF LONGINT;
     PSemaList = ^TSemaList;

CONST NumSemaOpen   : LONGINT = 0;
      NumEntriesMax : LONGINT = 0;
      SemaList      : PSemaList = NIL;

PROCEDURE SaveSema (Handle : LONGINT);
BEGIN
  {$ifdef DEBUG_MT}
     ConsolePrintf(#13'new Semaphore allocated (%x)'#13#10,Handle);
  {$endif DEBUG_MT}
  _EnterCritSec;
  IF NumSemaOpen = NumEntriesMax THEN
  BEGIN
    IF SemaList = NIL THEN
    BEGIN
      SemaList := _malloc (32 * SIZEOF (TSemaList[0]));
      NumEntriesMax := 32;
    END ELSE
    BEGIN
      INC (NumEntriesMax, 16);
      SemaList := _realloc (SemaList, NumEntriesMax * SIZEOF (TSemaList[0]));
    END;
  END;
  INC (NumSemaOpen);
  SemaList^[NumSemaOpen] := Handle;
  _ExitCritSec;
END;

PROCEDURE ReleaseSema (Handle : LONGINT);
VAR I : LONGINT;
BEGIN
  {$ifdef DEBUG_MT}
     ConsolePrintf(#13'Semaphore released (%x)'#13#10,Handle);
  {$endif DEBUG_MT}
  _EnterCritSec;
  IF SemaList <> NIL then
    if NumSemaOpen > 0 then
    begin
      for i := 1 to NumSemaOpen do
        if SemaList^[i] = Handle then
        begin
          if i < NumSemaOpen then
            SemaList^[i] := SemaList^[NumSemaOpen];
          dec (NumSemaOpen);
          _ExitCritSec;
          exit;
        end;
    end;
  _ExitCritSec;
  ConsolePrintf (#13'fpc-rtl: ReleaseSema, Handle not found'#13#10,0);
END;


PROCEDURE CloseAllRemainingSemaphores;
var i : LONGINT;
begin
  IF SemaList <> NIL then
  begin
    if NumSemaOpen > 0 then
      for i := 1 to NumSemaOpen do
        _CloseLocalSemaphore (SemaList^[i]);
     _free (SemaList);
     SemaList := NIL;
     NumSemaOpen := 0;
     NumEntriesMax := 0;
  end;
end;

{ this allows to do a lot of things in MT safe way }
{ it is also used to make the heap management      }
{ thread safe                                      }
procedure InitCriticalSection(var cs : TRTLCriticalSection);
begin
  cs.SemaHandle := _OpenLocalSemaphore (1);
  if cs.SemaHandle <> 0 then
  begin
    cs.SemaIsOpen := true;
    SaveSema (cs.SemaHandle);
  end else
  begin
    cs.SemaIsOpen := false;
    ConsolePrintf (#13'fpc-rtl: InitCriticalsection, OpenLocalSemaphore returned error'#13#10,0);
  end;
end;

procedure DoneCriticalsection(var cs : TRTLCriticalSection);
begin
  if cs.SemaIsOpen then
  begin
    _CloseLocalSemaphore (cs.SemaHandle);
    ReleaseSema (cs.SemaHandle);
    cs.SemaIsOpen := FALSE;
  end;
end;

procedure EnterCriticalsection(var cs : TRTLCriticalSection);
begin
  if cs.SemaIsOpen then
    _WaitOnLocalSemaphore (cs.SemaHandle)
  else
    ConsolePrintf (#13'fpc-rtl: EnterCriticalsection, TRTLCriticalSection not open'#13#10,0);
end;

procedure LeaveCriticalsection(var cs : TRTLCriticalSection);
begin
  if cs.SemaIsOpen then
    _SignalLocalSemaphore (cs.SemaHandle)
  else
    ConsolePrintf (#13'fpc-rtl: LeaveCriticalsection, TRTLCriticalSection not open'#13#10,0);
end;


{$endif MT}

{
  $Log$
  Revision 1.2  2002-03-28 16:11:17  armin
  + initialize threadvars defined local in units

  Revision 1.1  2002/03/17 17:57:33  armin
  + threads and winsock2 implemented

}
