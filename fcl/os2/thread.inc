{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2002 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{****************************************************************************}
{*                             TThread                                      *}
{****************************************************************************}

(* OS/2 specific declarations - see unit DosCalls for descriptions *)

type
 TByteArray = array [0..$fff0] of byte;
 PByteArray = ^TByteArray;

 TThreadEntry = function (Param: pointer): longint; cdecl;

 TSysThreadIB = record
                 TID, Priority, Version: longint;
                 MCCount, MCForceFlag: word;
                end;
 PSysThreadIB = ^TSysThreadIB;

 TThreadInfoBlock = record
                     Exh_Chain, Stack, StackLimit: pointer;
                     TIB2: PSysThreadIB;
                     Version, Ordinal: longint;
                    end;
 PThreadInfoBlock = ^TThreadInfoBlock;
 PPThreadInfoBlock = ^PThreadInfoBlock;

 TProcessInfoBlock = record
                      PID, ParentPID, HMTE: longint;
                      Cmd, Env: PByteArray;
                      flStatus, tType: longint;
                     end;
 PProcessInfoBlock = ^TProcessInfoBlock;
 PPProcessInfoBlock = ^PProcessInfoBlock;


const
 deThread = 0;
 deProcess = 1;

 dtSuspended = 1;
 dtStack_Commited = 2;

 dtWait = 0;
 dtNoWait = 1;


procedure DosGetInfoBlocks (PATIB: PPThreadInfoBlock;
              PAPIB: PPProcessInfoBlock); cdecl; external 'DOSCALLS' index 312;

function DosSetPriority (Scope, TrClass, Delta, PortID: longint): longint;
                                          cdecl; external 'DOSCALLS' index 236;

procedure DosExit (Action, Result: longint); cdecl;
                                                 external 'DOSCALLS' index 233;

function DosCreateThread (var TID: longint; Address: TThreadEntry;
         aParam: pointer; Flags: longint; StackSize: longint): longint; cdecl;
                                                 external 'DOSCALLS' index 311;

function DosKillThread (TID: longint): longint; cdecl;
                                                 external 'DOSCALLS' index 111;

function DosResumeThread (TID: longint): longint; cdecl;
                                                 external 'DOSCALLS' index 237;

function DosSuspendThread (TID: longint): longint; cdecl;
                                                 external 'DOSCALLS' index 238;

function DosWaitThread (var TID: longint; Option: longint): longint; cdecl;
                                                 external 'DOSCALLS' index 349;


const
 Priorities: array [TThreadPriority] of word = ($100, $200, $207, $20F, $217,
  $21F, $300);
 ThreadCount: longint = 0;

(* Implementation of exported functions *)

procedure AddThread (T: TThread);
begin
 Inc (ThreadCount);
end;


procedure RemoveThread (T: TThread);
begin
 Dec (ThreadCount);
end;


procedure TThread.CallOnTerminate;
begin
 FOnTerminate (Self);
end;


function TThread.GetPriority: TThreadPriority;
var
 PTIB: PThreadInfoBlock;
 I: TThreadPriority;
begin
 DosGetInfoBlocks (@PTIB, nil);
 with PTIB^.TIB2^ do
  if Priority >= $300 then GetPriority := tpTimeCritical else
      if Priority < $200 then GetPriority := tpIdle else
  begin
   I := Succ (Low (TThreadPriority));
   while (I < High (TThreadPriority)) and
    (Priority - Priorities [I] <= Priorities [Succ (I)] - Priority) do Inc (I);
   GetPriority := I;
  end;
end;


procedure TThread.SetPriority(Value: TThreadPriority);
var
 PTIB: PThreadInfoBlock;
begin
 DosGetInfoBlocks (@PTIB, nil);
(*
 PTIB^.TIB2^.Priority := Priorities [Value];
*)
 DosSetPriority (2, High (Priorities [Value]),
                     Low (Priorities [Value]) - PTIB^.TIB2^.Priority, FHandle);
end;


procedure TThread.SetSuspended(Value: Boolean);
begin
 if Value <> FSuspended then
 begin
  if Value then Suspend else Resume;
 end;
end;


procedure TThread.DoTerminate;
begin
 if Assigned (FOnTerminate) then Synchronize (@CallOnTerminate);
end;


procedure TThread.Synchronize(Method: TThreadMethod);
begin
end;


function ThreadProc(Args: pointer): Integer; cdecl;
var
  FreeThread: Boolean;
  Thread: TThread absolute Args;
begin
  Thread.Execute;
  FreeThread := Thread.FFreeOnTerminate;
  Result := Thread.FReturnValue;
  Thread.FFinished := True;
  Thread.DoTerminate;
  if FreeThread then Thread.Free;
  DosExit (deThread, Result);
end;

constructor TThread.Create(CreateSuspended: Boolean);
var
  Flags: Integer;
begin
  inherited Create;
  AddThread (Self);
  FSuspended := CreateSuspended;
  Flags := dtStack_Commited;
  if FSuspended then Flags := Flags or dtSuspended;
  if DosCreateThread (FThreadID, @ThreadProc, pointer (Self), Flags, 16384)
                                                                      <> 0 then
  begin
   FFinished := true;
   Destroy;
  end else FHandle := FThreadID;
  IsMultiThread := TRUE;
end;


destructor TThread.Destroy;
begin
 if not FFinished and not Suspended then
 begin
  Terminate;
  WaitFor;
 end;
 if FHandle <> -1 then DosKillThread (FHandle);
 inherited Destroy;
 RemoveThread (Self);
end;

procedure TThread.Resume;
begin
 FSuspended := not (DosResumeThread (FHandle) = 0);
end;


procedure TThread.Suspend;
begin
 FSuspended := DosSuspendThread (FHandle) = 0;
end;


procedure TThread.Terminate;
begin
 FTerminated := true;
end;


function TThread.WaitFor: Integer;

begin
 WaitFor := DosWaitThread (FHandle, dtWait);
end;


{
  $Log$
  Revision 1.6  2002-09-07 15:15:27  peter
    * old logs removed and tabs fixed

  Revision 1.5  2002/02/10 13:38:14  hajny
    * DosCalls dependency removed to avoid type redefinitions

}
