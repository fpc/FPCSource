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
 PPointer = ^pointer;

var
(* Pointer to an allocated dword space within the local thread *)
(* memory area. Pointer to the real memory block allocated for *)
(* thread vars in this block is then stored in this dword.     *)
 DataIndex: PPointer;
 CritSectSem: longint;

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

function DosCreateMutExSem (Name: PChar; var Handle: longint; Attr: longint;
                State: boolean): longint; cdecl; external 'DOSCALLS' index 331;

function DosCloseMutExSem (Handle: longint): longint; cdecl;
                                                 external 'DOSCALLS' index 333;

function DosRequestMutExSem (Handle, Timeout: longint): longint; cdecl;
                                                 external 'DOSCALLS' index 334;

function DosReleaseMutExSem (Handle: longint): longint; cdecl;
                                                 external 'DOSCALLS' index 335;

function DosAllocMem (var P: pointer; Size, Flag: longint): longint; cdecl;
                                                 external 'DOSCALLS' index 299;

function DosFreeMem (P: pointer): longint; cdecl;
                                                 external 'DOSCALLS' index 304;


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
 if os_mode = osOS2 then
 begin
  if DosAllocMem (DataIndex^, ThreadVarBlockSize, pag_Read or pag_Write
                                         or pag_Commit) <> 0 then RunError (8);
 end else
 begin
  (* Allocate the DOS memory here. *)

 end;
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
 if os_mode = osOS2 then
 begin
  DosFreeMem (DataIndex^);
{$IFDEF EMX}
{$ASMMODE INTEL}
  asm
   mov eax, 7F2Dh
   mov edx, ThreadID
   call syscall
  end;
{$ASMMODE DEFAULT}
{$ENDIF EMX}
 end else
 begin
  (* Deallocate the DOS memory here. *)

 end;
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
 BeginThread := DosCreateThread (ThreadID, @ThreadMain, TI, StackSize, TI,
       CreationFlags);
{$IFDEF EMX}
{$ASMMODE INTEL}
 asm
  mov eax, 7F2Ch
  mov edx, ThreadID
  call syscall
 end;
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

procedure InitCriticalSection (var CS);
begin
 if os_mode = osOS2 then
      if DosCreateMutExSem (nil, CritSectSem, 0, false) <> 0 then RunError (8);
end;

procedure DoneCriticalSection (var CS);
begin
 if os_mode = osOS2 then DosCloseMutExSem (CritSectSem);
end;

procedure EnterCriticalsection (var CS);
begin
 if os_mode = osOS2 then DosRequestMutExSem (CritSectSem, sem_Indefinite_Wait);
end;

procedure LeaveCriticalsection(var cs);
begin
 if os_mode = osOS2 then DosReleaseMutExSem (CritSectSem);
end;

{
  $Log$
  Revision 1.2  2001-01-27 18:28:52  hajny
    * OS/2 implementation of threads almost finished

  Revision 1.1  2001/01/23 20:38:59  hajny
    + beginning of the OS/2 version

  Revision 1.1  2001/01/01 19:06:36  florian
    + initial release

}