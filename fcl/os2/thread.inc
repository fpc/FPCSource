{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{****************************************************************************}
{*                             TThread                                      *}
{****************************************************************************}

const
 Priorities: array [TThreadPriority] of word = ($100, $200, $207, $20F, $217
  $21F, $300);


procedure AddThread;
begin
 Inc (ThreadCount);
end;


procedure RemoveThread;
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


function ThreadProc(Thread: TThread): Integer; cdecl;
var
  FreeThread: Boolean;
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
  Flags := dtStack_Committed;
  if FSuspended then Flags := Flags or dtSuspended;
  if DosCreateThread (FThreadID, @ThreadProc, pointer (Self), Flags, 16384)
                                                                      <> 0 then
  begin
   FFinished := true;
   Destroy;
  end else FHandle := FThreadID;
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
  Revision 1.2  2000-07-13 11:33:02  michael
  + removed logs
 
}
