{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Multithreading implementation for OS/2

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFDEF MT}
{$DEFINE EMX}

const
 ThreadVarBlockSize: dword = 0;
 pag_Read = 1;
 pag_Write = 2;
 pag_Execute = 4;
 pag_Guard = 8;
 pag_Commit = $10;
 obj_Tile = $40;
 sem_Indefinite_Wait = -1;
 dtSuspended = 1;
 dtStack_Commited = 2;

type
 TThreadInfo = record
  F: TThreadFunc;
  P: pointer;
 end;
 PThreadInfo = ^TThreadInfo;

var
(* Pointer to an allocated dword space within the local thread *)
(* memory area. Pointer to the real memory block allocated for *)
(* thread vars in this block is then stored in this dword.     *)
 DataIndex: PPointer;

{ import the necessary stuff from the OS }
function DosAllocThreadLocalMemory (Count: cardinal; var P: pointer): longint;
                                          cdecl; external 'DOSCALLS' index 454;

function DosFreeThreadLocalMemory (P: pointer): longint; cdecl;
                                                 external 'DOSCALLS' index 455;

function DosCreateThread (var TID: longint; Address: pointer;
(* TThreadFunc *)
        aParam: pointer; Flags: longint; StackSize: longint): longint; cdecl;
                                                 external 'DOSCALLS' index 311;

procedure DosExit (Action, Result: longint); cdecl;
                                                 external 'DOSCALLS' index 234;

function DosCreateMutExSem (Name: PChar; var Handle: longint; Attr: longint;
                State: boolean): longint; cdecl; external 'DOSCALLS' index 331;

function DosCloseMutExSem (Handle: longint): longint; cdecl;
                                                 external 'DOSCALLS' index 333;

function DosQueryMutExSem (Handle: longint; var PID, TID, Count: longint):
                                 longint; cdecl; external 'DOSCALLS' index 336;

function DosRequestMutExSem (Handle, Timeout: longint): longint; cdecl;
                                                 external 'DOSCALLS' index 334;

function DosReleaseMutExSem (Handle: longint): longint; cdecl;
                                                 external 'DOSCALLS' index 335;

function DosAllocMem (var P: pointer; Size, Flag: longint): longint; cdecl;
                                                 external 'DOSCALLS' index 299;

function DosFreeMem (P: pointer): longint; cdecl;
                                                 external 'DOSCALLS' index 304;

function DosEnterCritSec:longint; cdecl; external 'DOSCALLS' index 232;

function DosExitCritSec:longint; cdecl; external 'DOSCALLS' index 233;


procedure Init_ThreadVar (var TVOffset: dword; Size: dword);
                                         [public, alias: 'FPC_INIT_THREADVAR'];
begin
 TVOffset := ThreadVarBlockSize;
 Inc (ThreadVarBlockSize, Size);
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


function Relocate_ThreadVar (TVOffset: dword): pointer;
                                      [public,alias: 'FPC_RELOCATE_THREADVAR'];
begin
 Relocate_ThreadVar := DataIndex^ + TVOffset;
end;

procedure AllocateThreadVars;
begin
 { we've to allocate the memory from the OS }
 { because the FPC heap management uses     }
 { exceptions which use threadvars but      }
 { these aren't allocated yet ...           }
 { allocate room on the heap for the thread vars }
  if DosAllocMem (DataIndex^, ThreadVarBlockSize, pag_Read or pag_Write
                                      or pag_Commit) <> 0 then HandleError (8);
end;

procedure ReleaseThreadVars;
begin
 { release thread vars }
 DosFreeMem (DataIndex^)
end;

procedure InitThread;
begin
 InitFPU;
 { we don't need to set the data to 0 because we did this with }
 { the fillchar above, but it looks nicer                      }

 { ExceptAddrStack and ExceptObjectStack are threadvars       }
 { so every thread has its on exception handling capabilities }
 InitExceptions;
 InOutRes := 0;
{ ErrNo := 0;}
end;

procedure DoneThread;
var
 PTIB: PThreadInfoBlock;
 PPIB: PProcessInfoBlock;
 ThreadID: longint;
begin
 ReleaseThreadVars;
 DosGetInfoBlocks (@PTIB, @PPIB);
 ThreadID := PTIB^.TIB2^.TID;
end;

function ThreadMain (Param: pointer): dword; cdecl;
var
 TI: TThreadInfo;
begin
{$ifdef DEBUG_MT}
 WriteLn ('New thread started, initialising ...');
{$endif DEBUG_MT}
 AllocateThreadVars;
 InitThread;
 TI := PThreadInfo (Param)^;
 Dispose (PThreadInfo (Param));
{$ifdef DEBUG_MT}
 WriteLn ('Jumping to thread function');
{$endif DEBUG_MT}
 ThreadMain := TI.F (TI.P);
end;

function BeginThread (SA: pointer; StackSize: dword;
       ThreadFunction: TThreadFunc; P: pointer; CreationFlags: dword;
                                                   var ThreadID: dword): dword;
var
 TI: PThreadInfo;
begin
{$ifdef DEBUG_MT}
 WriteLn ('Creating new thread');
{$endif DEBUG_MT}
 IsMultiThread := true;
 { the only way to pass data to the newly created thread }
 { in a MT safe way, is to use the heap                  }
 New (TI);
 TI^.F := ThreadFunction;
 TI^.P := P;
{$ifdef DEBUG_MT}
 WriteLn ('Starting new thread');
{$endif DEBUG_MT}
 BeginThread := DosCreateThread (ThreadID, @ThreadMain, TI, CreationFlags,
                                                                    StackSize);
{$IFDEF EMX}
{$ASMMODE INTEL}
 asm
  mov eax, 7F2Ch
  mov edx, ThreadID
  call syscall
{$warning Error checking missing!!}
 end ['eax', 'ecx', 'edx'];
{$ASMMODE DEFAULT}
{$ENDIF EMX}
end;

function BeginThread (ThreadFunction: TThreadFunc): dword;
var
 Dummy: dword;
begin
(* The stack size of 0 causes 4 kB to be allocated for stack. *)
 BeginThread := BeginThread (nil, 0, ThreadFunction, nil, dtStack_Commited,
                                                                        Dummy);
end;

function BeginThread (ThreadFunction: TThreadFunc; P: pointer): dword;
var
 Dummy: dword;
begin
(* The stack size of 0 causes 4 kB to be allocated for stack. *)
 BeginThread := BeginThread (nil, 0, ThreadFunction, P, 0, Dummy);
end;

function BeginThread (ThreadFunction: TThreadFunc; P: pointer;
                                                   var ThreadID: dword): dword;
begin
(* The stack size of 0 causes 4 kB to be allocated for stack. *)
 BeginThread := BeginThread (nil, 0, ThreadFunction, P, 0, ThreadID);
end;

procedure EndThread (ExitCode: dword);
begin
 DoneThread;
 DosExit (0, ExitCode);
end;

procedure EndThread;
begin
 EndThread (0);
end;

procedure InitCriticalSection (var CS: TRTLCriticalSection);
begin
  if DosCreateMutExSem (nil, CS.LockSemaphore2, 0, true) <> 0 then
                                                               HandleError (8);
  DosEnterCritSec;
  CS.LockCount := 0;
  CS.OwningThread := $FFFF;
  DosExitCritSec;
  DosReleaseMutexSem (CS.LockSemaphore2);
end;

procedure DoneCriticalSection (var CS: TRTLCriticalSection);
begin
 DosCloseMutExSem (CS.LockSemaphore2);
end;

procedure EnterCriticalSection (var CS: TRTLCriticalSection);
var
 P, T, Cnt: longint;
 PTIB: PThreadInfoBlock;
 PPIB: PProcessInfoBlock;
begin
  DosGetInfoBlocks (@PTIB, @PPIB);
  DosEnterCritSec;
  with CS do if (LockCount = 0) and
    (DosQueryMutExSem (LockSemaphore2, P, T, Cnt) = 0) and (Cnt = 0) and
                                                     (T = PTIB^.TIB2^.TID) then
  begin
   LockCount := 1;
   OwningThread2 := PTIB^.TIB2^.TID;
   DosExitCritSec;
   DosRequestMutExSem (LockSemaphore2, sem_Indefinite_Wait);
  end else if PTIB^.TIB2^.TID = OwningThread2 then
  begin
   Inc (LockCount);
   if LockCount = 0 then Dec (LockCount);
   DosExitCritSec;
  end else
  begin
   DosExitCritSec;
   DosRequestMutExSem (LockSemaphore2, sem_Indefinite_Wait);
   DosEnterCritSec;
   LockCount := 1;
   OwningThread2 := PTIB^.TIB2^.TID;
   DosExitCritSec;
  end;
end;

procedure LeaveCriticalSection (var CS: TRTLCriticalSection);
var
 PTIB: PThreadInfoBlock;
 PPIB: PProcessInfoBlock;
 Err: boolean;
begin
  Err := false;
  DosGetInfoBlocks (@PTIB, @PPIB);
  DosEnterCritSec;
  with CS do if OwningThread2 <> PTIB^.TIB2^.TID then
  begin
   DosExitCritSec;
   Err := true;
  end else if LockCount = 1 then
  begin
   if DosReleaseMutExSem (LockSemaphore2) <> 0 then Err := true;
   Dec (LockCount);
   DosExitCritSec;
  end else
  begin
   Dec (LockCount);
   DosExitCritSec;
  end;
  if Err then HandleError (5);
end;

{$ENDIF MT}

{
  $Log$
  Revision 1.12  2003-10-08 05:22:47  yuri
  * Some emx code removed

  Revision 1.11  2003/10/07 21:26:35  hajny
    * stdcall fixes and asm routines cleanup

  Revision 1.10  2003/02/20 17:09:49  hajny
    * fixes for OS/2 v2.1 incompatibility

  Revision 1.9  2002/09/07 16:01:25  peter
    * old logs removed and tabs fixed

  Revision 1.8  2002/07/07 18:04:39  hajny
    * correction by Yuri Prokushev

  Revision 1.7  2002/03/28 16:34:29  armin
  + initialize threadvars defined local in units

}
