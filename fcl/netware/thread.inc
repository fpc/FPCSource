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
 
{ additional functions needed for netware that are not defined in systhrds }

function SuspendThread (threadId : longint) : longint; cdecl; external 'clib' name 'SuspendThread';
function ResumeThread (threadId : longint) : longint; cdecl; external 'clib' name 'ResumeThread';
procedure ThreadSwitchWithDelay; cdecl; external 'clib' name 'ThreadSwitchWithDelay';
function GetThreadName  (threadId : longint; var threadName) : longint; cdecl; external 'clib' name 'GetThreadName';
function RenameThread (threadId : longint; threadName:pchar) : longint; cdecl; external 'clib' name 'RenameThread';
     


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
  Thread.Execute;
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
  nam : string [18];  {17 chars is the maximum}
begin
  inherited Create;
  AddThread(self);
  FSuspended := CreateSuspended;
  { Create new thread }
  FHandle := BeginThread (@ThreadProc,self);
  if FSuspended then Suspend;
  nam := copy (ClassName,1,17)+#0;
  RenameThread (FHandle, @nam[1]);
  FThreadID := FHandle;
  //IsMultiThread := TRUE;  {already set by systhrds}
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
    SuspendThread (FHandle);  {something went wrong, this will crash the server at unload}
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


function TThread.GetPriority: TThreadPriority;
begin
  result := tpNormal;
end;


procedure TThread.SetPriority(Value: TThreadPriority);
begin
end;


procedure TThread.Synchronize(Method: TThreadMethod);
begin
  FSynchronizeException := nil;
  FMethod := Method;
{  SendMessage(ThreadWindow, CM_EXECPROC, 0, Longint(Self)); }
  if Assigned(FSynchronizeException) then
    raise FSynchronizeException;
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
  ThreadSwitchWithDelay;
end;

function TThread.WaitFor: Integer;
var
  status : longint;
  buf : array [0..50] of char;
begin
  repeat
    status := GetThreadName (FHandle,Buf); {should return EBADHNDL if thread is terminated}
    ThreadSwitchWithDelay;
  until status <> 0;    
  Result:=0;
end;

{
  $Log$
  Revision 1.1  2003-03-25 17:56:19  armin
  * first fcl implementation for netware

  Revision 1.7  2002/12/18 20:44:36  peter
    * use fillchar to clear sigset

  Revision 1.6  2002/09/07 15:15:27  peter
    * old logs removed and tabs fixed

}
