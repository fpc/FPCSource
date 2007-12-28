{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Peter Vreman,
    member of the Free Pascal development team.

    Linux (pthreads) threading support implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*****************************************************************************
                           Local Api imports
*****************************************************************************}

const
 pag_Read = 1;
 pag_Write = 2;
 pag_Execute = 4;
 pag_Guard = 8;
 pag_Commit = $10;
 obj_Tile = $40;
 sem_Indefinite_Wait = -1;
 dtSuspended = 1;
 dtStack_Commited = 2;

{ import the necessary stuff from the OS }
function DosAllocThreadLocalMemory (Count: cardinal; var P: pointer): cardinal;
                                          cdecl; external 'DOSCALLS' index 454;

function DosFreeThreadLocalMemory (P: pointer): cardinal; cdecl;
                                                 external 'DOSCALLS' index 455;

function DosCreateThread (var TID: cardinal; Address: pointer;
(* TThreadFunc *)
        aParam: pointer; Flags: cardinal; StackSize: cardinal): cardinal; cdecl;
                                                 external 'DOSCALLS' index 311;

procedure DosExit (Action, Result: cardinal); cdecl;
                                                 external 'DOSCALLS' index 234;

function DosCreateMutExSem (Name: PChar; var Handle: longint; Attr: cardinal;
               State: boolean): cardinal; cdecl; external 'DOSCALLS' index 331;

function DosCloseMutExSem (Handle: longint): cardinal; cdecl;
                                                 external 'DOSCALLS' index 333;

function DosQueryMutExSem (Handle: longint; var PID, TID, Count: cardinal):
                                cardinal; cdecl; external 'DOSCALLS' index 336;

function DosRequestMutExSem (Handle:longint; Timeout: cardinal): cardinal; cdecl;
                                                 external 'DOSCALLS' index 334;

function DosReleaseMutExSem (Handle: longint): cardinal; cdecl;
                                                 external 'DOSCALLS' index 335;

function DosAllocMem (var P: pointer; Size, Flag: cardinal): cardinal; cdecl;
                                                 external 'DOSCALLS' index 299;

function DosFreeMem (P: pointer): cardinal; cdecl;
                                                 external 'DOSCALLS' index 304;

function DosEnterCritSec:cardinal; cdecl; external 'DOSCALLS' index 232;

function DosExitCritSec:cardinal; cdecl; external 'DOSCALLS' index 233;


{*****************************************************************************
                             Threadvar support
*****************************************************************************}

const
 ThreadVarBlockSize: dword = 0;

var
(* Pointer to an allocated dword space within the local thread *)
(* memory area. Pointer to the real memory block allocated for *)
(* thread vars in this block is then stored in this dword.     *)
 DataIndex: PPointer;

procedure SysInitThreadvar (var Offset: dword; Size: dword);
begin
 Offset := ThreadVarBlockSize;
 Inc (ThreadVarBlockSize, Size);
end;

function SysRelocateThreadVar (Offset: dword): pointer;
begin
 SysRelocateThreadVar := DataIndex^ + Offset;
end;

procedure SysAllocateThreadVars;
begin
 { we've to allocate the memory from the OS }
 { because the FPC heap management uses     }
 { exceptions which use threadvars but      }
 { these aren't allocated yet ...           }
 { allocate room on the heap for the thread vars }
 if DosAllocMem (DataIndex^, ThreadVarBlockSize, pag_Read or pag_Write
                                      or pag_Commit) <> 0 then HandleError (8);
end;

procedure SysReleaseThreadVars;
begin
 { release thread vars }
 DosFreeMem (DataIndex^);
end;


    procedure InitThreadVars;
      begin
        { allocate one ThreadVar entry from the OS, we use this entry }
        { for a pointer to our threadvars                             }
        if DosAllocThreadLocalMemory (1, DataIndex) <> 0 then HandleError (8);
        { initialize threadvars }
        init_all_unit_threadvars;
        { allocate mem for main thread threadvars }
        SysAllocateThreadVars;
        { copy main thread threadvars }
        copy_all_unit_threadvars;
        { install threadvar handler }
        fpc_threadvar_relocate_proc := @SysRelocateThreadvar;
      end;



{*****************************************************************************
                          Delphi/Win32 compatibility
*****************************************************************************}

{ we implement these procedures for win32 by importing them }
{ directly from windows                                     }
procedure SysInitCriticalSection(var cs : TRTLCriticalSection);
  external 'kernel32' name 'InitializeCriticalSection';

procedure SysDoneCriticalSection(var cs : TRTLCriticalSection);
  external 'kernel32' name 'DeleteCriticalSection';

procedure SysEnterCriticalSection(var cs : TRTLCriticalSection);
  external 'kernel32' name 'EnterCriticalSection';

procedure SysLeaveCriticalSection(var cs : TRTLCriticalSection);
  external 'kernel32' name 'LeaveCriticalSection';
