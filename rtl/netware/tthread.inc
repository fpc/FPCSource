{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2003 by the Free Pascal development team

    Netware TThread implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


type
  PThreadRec=^TThreadRec;
  TThreadRec=record
    thread : TThread;
    next   : PThreadRec;
  end;

var
  ThreadRoot : PThreadRec;
  ThreadsInited : boolean;
//  MainThreadID: longint;

Const
  ThreadCount: longint = 0;

{function ThreadSelf:TThread;
var
  hp : PThreadRec;
  sp : longint;
begin
  sp:=SPtr;
  hp:=ThreadRoot;
  while assigned(hp) do
   begin
     if (sp<=hp^.Thread.FStackPointer) and
        (sp>(hp^.Thread.FStackPointer-hp^.Thread.FStackSize)) then
      begin
        Result:=hp^.Thread;
        exit;
      end;
     hp:=hp^.next;
   end;
  Result:=nil;
end;}


procedure InitThreads;
begin
  ThreadRoot:=nil;
  ThreadsInited:=true;
end;


procedure DoneThreads;
var
  hp : PThreadRec;
begin
  while assigned(ThreadRoot) do
   begin
     ThreadRoot^.Thread.Destroy;
     hp:=ThreadRoot;
     ThreadRoot:=ThreadRoot^.Next;
     dispose(hp);
   end;
  ThreadsInited:=false;
end;


procedure AddThread(t:TThread);
var
  hp : PThreadRec;
begin
  { Need to initialize threads ? }
  if not ThreadsInited then
   InitThreads;

  { Put thread in the linked list }
  new(hp);
  hp^.Thread:=t;
  hp^.next:=ThreadRoot;
  ThreadRoot:=hp;

  inc(ThreadCount, 1);
end;


procedure RemoveThread(t:TThread);
var
  lasthp,hp : PThreadRec;
begin
  hp:=ThreadRoot;
  lasthp:=nil;
  while assigned(hp) do
   begin
     if hp^.Thread=t then
      begin
        if assigned(lasthp) then
         lasthp^.next:=hp^.next
        else
         ThreadRoot:=hp^.next;
        dispose(hp);
        exit;
      end;
     lasthp:=hp;
     hp:=hp^.next;
   end;

  Dec(ThreadCount, 1);
  if ThreadCount = 0 then DoneThreads;
end;


{ TThread }
function ThreadProc(args:pointer): Integer;cdecl;
var
  FreeThread: Boolean;
  Thread : TThread absolute args;
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
  if FreeThread then
    Thread.Free;
  EndThread(Result);
end;


constructor TThread.Create(CreateSuspended: Boolean);
var
  Flags: Integer;
begin
  inherited Create;
  AddThread(self);
  FSuspended := CreateSuspended;
  { Create new thread }
  FHandle := BeginThread (@ThreadProc,self);
  if FSuspended then Suspend;
  FThreadID := FHandle;
  //IsMultiThread := TRUE;  {already set by systhrds}
  FFatalException := nil;
end;


destructor TThread.Destroy;
begin
  if not FFinished {and not Suspended} then
   begin
     if Suspended then ResumeThread (FHandle);  {netware can not kill a thread}
     Terminate;
     WaitFor;
   end;
  if FHandle <> -1 then
    KillThread (FHandle);  {something went wrong, kill the thread (not possible on netware)}
  FFatalException.Free;
  FFatalException := nil;
  inherited Destroy;
  RemoveThread(self);
end;


procedure TThread.CallOnTerminate;
begin
  FOnTerminate(Self);
end;

procedure TThread.DoTerminate;
begin
  if Assigned(FOnTerminate) then
    Synchronize(@CallOnTerminate);
end;


const
  Priorities: array [TThreadPriority] of Integer =
   (THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST, THREAD_PRIORITY_BELOW_NORMAL,
    THREAD_PRIORITY_NORMAL, THREAD_PRIORITY_ABOVE_NORMAL,
    THREAD_PRIORITY_HIGHEST, THREAD_PRIORITY_TIME_CRITICAL);

function TThread.GetPriority: TThreadPriority;
var
  P: Integer;
  I: TThreadPriority;
begin
  P := ThreadGetPriority(FHandle);
  Result := tpNormal;
  for I := Low(TThreadPriority) to High(TThreadPriority) do
    if Priorities[I] = P then Result := I;
end;


procedure TThread.SetPriority(Value: TThreadPriority);
begin
  ThreadSetPriority(FHandle, Priorities[Value]);
end;

{does not make sense for netware}
procedure TThread.Synchronize(Method: TThreadMethod);
begin
  {$ifndef netware}
  FSynchronizeException := nil;
  FMethod := Method;
{  SendMessage(ThreadWindow, CM_EXECPROC, 0, Longint(Self)); }
  {$warning Synchronize needs implementation}
  if Assigned(FSynchronizeException) then
    raise FSynchronizeException;
  {$endif}
end;


procedure TThread.SetSuspended(Value: Boolean);
begin
  if Value <> FSuspended then
    if Value then
      Suspend
    else
      Resume;
end;


procedure TThread.Suspend;
begin
  SuspendThread (FHandle);
  FSuspended := true;
end;


procedure TThread.Resume;
begin
  ResumeThread (FHandle);
  FSuspended := False;
end;


procedure TThread.Terminate;
begin
  FTerminated := True;
  ThreadSwitch;
end;


function TThread.WaitFor: Integer;
begin
  Result := WaitForThreadTerminate (FHandle,0);
  if Result = 0 then
    FHandle := -1;
end;

{
  $Log$
  Revision 1.1  2003-10-06 21:01:06  peter
    * moved classes unit to rtl

  Revision 1.3  2003/10/06 17:06:55  florian
    * applied Johannes Berg's patch for exception handling in threads

  Revision 1.2  2003/03/27 17:14:27  armin
  * more platform independent thread routines, needs to be implemented for unix

  Revision 1.1  2003/03/25 17:56:19  armin
  * first fcl implementation for netware

  Revision 1.7  2002/12/18 20:44:36  peter
    * use fillchar to clear sigset

  Revision 1.6  2002/09/07 15:15:27  peter
    * old logs removed and tabs fixed

}
