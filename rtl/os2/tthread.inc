{
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

function DosSetPriority (Scope, TrClass: cardinal; Delta: longint;
             PortID: cardinal): cardinal; cdecl; external 'DOSCALLS' index 236;

procedure DosExit (Action, Result: cardinal); cdecl;
                                                 external 'DOSCALLS' index 233;

function DosCreateThread (var TID: cardinal; Address: TThreadEntry;
      aParam: pointer; Flags: cardinal; StackSize: cardinal): cardinal; cdecl;
                                                 external 'DOSCALLS' index 311;

function DosKillThread (TID: cardinal): cardinal; cdecl;
                                                 external 'DOSCALLS' index 111;

function DosResumeThread (TID: cardinal): cardinal; cdecl;
                                                 external 'DOSCALLS' index 237;

function DosSuspendThread (TID: cardinal): cardinal; cdecl;
                                                 external 'DOSCALLS' index 238;

function DosWaitThread (var TID: cardinal; Option: cardinal): cardinal; cdecl;
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
 PPIB: PProcessInfoBlock;
 I: TThreadPriority;
begin
 DosGetInfoBlocks (@PTIB, @PPIB);
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
 PPIB: PProcessInfoBlock;
begin
 DosGetInfoBlocks (@PTIB, @PPIB);
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


function ThreadProc(Args: pointer): Integer; cdecl;
var
  FreeThread: Boolean;
  Thread: TThread absolute Args;
begin
  try
    Thread.Execute;
  except
    Thread.FFatalException := TObject(AcquireExceptionObject);
  end;
  FreeThread := Thread.FFreeOnTerminate;
  Result := Thread.FReturnValue;
  Thread.FFinished := True;
  Thread.DoTerminate;
  if FreeThread then Thread.Free;
  DosExit (deThread, Result);
end;

constructor TThread.Create(CreateSuspended: Boolean);
var
  Flags: cardinal;
begin
  inherited Create;
  AddThread (Self);
  FSuspended := CreateSuspended;
  Flags := dtStack_Commited;
  if FSuspended then Flags := Flags or dtSuspended;
  if DosCreateThread (cardinal (FThreadID), @ThreadProc, pointer (Self),
                                                        Flags, 16384) <> 0 then
  begin
   FFinished := true;
   Destroy;
  end else FHandle := FThreadID;
  IsMultiThread := true;
  FFatalException := nil;
end;


destructor TThread.Destroy;
begin
 if not FFinished and not Suspended then
 begin
  Terminate;
  WaitFor;
 end;
 if FHandle <> -1 then DosKillThread (cardinal (FHandle));
 FFatalException.Free;
 FFatalException := nil;
 inherited Destroy;
 RemoveThread (Self);
end;

procedure TThread.Resume;
begin
 FSuspended := not (DosResumeThread (cardinal (FHandle)) = 0);
end;


procedure TThread.Suspend;
begin
 FSuspended := DosSuspendThread (cardinal (FHandle)) = 0;
end;


procedure TThread.Terminate;
begin
 FTerminated := true;
end;


function TThread.WaitFor: Integer;
var
 FH: cardinal;
begin
 WaitFor := DosWaitThread (FH, dtWait);
end;


{
  $Log: tthread.inc,v $
  Revision 1.6  2005/02/25 21:41:09  florian
    * generic tthread.synchronize
    * delphi compatible wakemainthread

  Revision 1.5  2005/02/14 17:13:31  peter
    * truncate log

}
