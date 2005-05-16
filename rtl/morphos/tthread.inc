{
    $Id: tthread.inc,v 1.3 2005/02/25 21:41:09 florian Exp $
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

{$WARNING This file is only a stub, and will not work!}

const
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
{ PTIB: PThreadInfoBlock;
 PPIB: PProcessInfoBlock;}
 I: TThreadPriority;
begin
{
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
}
end;


procedure TThread.SetPriority(Value: TThreadPriority);
{var
 PTIB: PThreadInfoBlock;
 PPIB: PProcessInfoBlock;}
begin
{ DosGetInfoBlocks (@PTIB, @PPIB);}
(*
 PTIB^.TIB2^.Priority := Priorities [Value];
*)
{
 DosSetPriority (2, High (Priorities [Value]),
                     Low (Priorities [Value]) - PTIB^.TIB2^.Priority, FHandle);}
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
{
  DosExit (deThread, Result);
}
end;

constructor TThread.Create(CreateSuspended: Boolean);
var
  Flags: cardinal;
begin
  inherited Create;
  AddThread (Self);
{
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
}
end;


destructor TThread.Destroy;
begin
 if not FFinished and not Suspended then
 begin
  Terminate;
  WaitFor;
 end;
{
 if FHandle <> -1 then DosKillThread (cardinal (FHandle));
 FFatalException.Free;
 FFatalException := nil;
 inherited Destroy;
 RemoveThread (Self);
}
end;

procedure TThread.Resume;
begin
{ FSuspended := not (DosResumeThread (cardinal (FHandle)) = 0);}
end;


procedure TThread.Suspend;
begin
{ FSuspended := DosSuspendThread (cardinal (FHandle)) = 0;}
end;


procedure TThread.Terminate;
begin
 FTerminated := true;
end;


function TThread.WaitFor: Integer;
var
 FH: cardinal;
begin
{ WaitFor := DosWaitThread (FH, dtWait);}
end;


{
  $Log: tthread.inc,v $
  Revision 1.3  2005/02/25 21:41:09  florian
    * generic tthread.synchronize
    * delphi compatible wakemainthread

  Revision 1.2  2005/02/14 17:13:30  peter
    * truncate log

}
