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
const
 ThreadVarBlockSize: dword = 0;

type
 TThreadInfo = record
  F: TThreadFunc;
  P: pointer;
 end;
 PThreadInfo = ^TThreadInfo;
 PPointer = ^pointer;

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

function DosCreateThread (var TID: longint; Address: TThreadEntry;
        aParam: pointer; Flags: longint; StackSize: longint): longint; cdecl;
                                                 external 'DOSCALLS' index 311;

procedure DosExit (Action, Result: longint); cdecl;
                                                 external 'DOSCALLS' index 233;

procedure Init_ThreadVar (var TVOffset: dword; Size: dword);
                                         [public, alias: 'FPC_INIT_THREADVAR'];
begin
 TVOffset := ThreadVarBlockSize;
 Inc (ThreadVarBlockSize, Size);
end;

function Relocate_ThreadVar (TVOffset: dword): pointer;
                                      [public,alias: 'FPC_RELOCATE_THREADVAR'];
begin
 Relocate_ThreadVar := DataIndex + TVOffset;
end;

procedure AllocateThreadVars;
begin
 { we've to allocate the memory from the OS }
 { because the FPC heap management uses     }
 { exceptions which use threadvars but      }
 { these aren't allocated yet ...           }
 { allocate room on the heap for the thread vars }
 if DosAllocMem (DataIndex^, ThreadVarBlockSize, ) <> 0 then RunError (8);
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
 ErrNo := 0;
end;

procedure DoneThread;
begin
 { release thread vars }
 DosFreeMem (DataIndex^);
end;

function ThreadMain (Param: pointer): dword; cdecl
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
 IsMultiThreaded := true;
 { the only way to pass data to the newly created thread }
 { in a MT safe way, is to use the heap                  }
 New (TI);
 TI^.F := ThreadFunction;
 TI^.P := P;
{$ifdef DEBUG_MT}
 WriteLn ('Starting new thread');
{$endif DEBUG_MT}
 BeginThread := CreateThread (sa,stacksize,@ThreadMain,ti,
       creationflags,threadid);
end;

function BeginThread (ThreadFunction: TThreadFunc): dword;
var
 Dummy: dword;
begin
 BeginThread := BeginThread (nil, 0, ThreadFunction, nil, 0, Dummy);
end;

function BeginThread (ThreadFunction: TThreadFunc; P: pointer): dword;
var
 Dummy: dword;
begin
 BeginThread := BeginThread (nil, 0, ThreadFunction, P, 0, Dummy);
end;

function BeginThread (ThreadFunction: TThreadFunc; P: pointer;
                                                   var ThreadID: dword): dword;
begin
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

procedure InitCriticalSection (var cs : tcriticalsection);
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
  $Log$
  Revision 1.1  2001-01-23 20:38:59  hajny
    + beginning of the OS/2 version

  Revision 1.1  2001/01/01 19:06:36  florian
    + initial release

}