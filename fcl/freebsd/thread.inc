{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Peter Vreman

    Linux TThread implementation

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

function ThreadSelf:TThread;
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
end;


//function SIGCHLDHandler(Sig: longint): longint; cdecl;//this is std linux C declaration as function
procedure SIGCHLDHandler(Sig: longint); cdecl;
begin
 {$ifdef ver1_0}
  waitpid(-1, nil, WNOHANG);
 {$else}
  fpwaitpid(-1, nil, WNOHANG);
 {$endif}
end;

const zeroset :sigset = (0,0,0,0);

procedure InitThreads;
var
  Act, OldAct: PSigActionRec;
begin
  ThreadRoot:=nil;
  ThreadsInited:=true;



// This will install SIGCHLD signal handler
// signal() installs "one-shot" handler,
// so it is better to install and set up handler with sigaction()

  GetMem(Act, SizeOf(SigActionRec));
  GetMem(OldAct, SizeOf(SigActionRec));

  {$ifndef ver1_0}
    Act^.sa_handler := @SIGCHLDHandler;
    fillchar(Act^.sa_mask,sizeof(sigset_t),#0);
  {$else}
    Act^.handler.sh := @SIGCHLDHandler;
    Act^.sa_mask := zeroset; 
  {$endif}
  Act^.sa_flags := SA_NOCLDSTOP {or SA_NOMASK or SA_RESTART};
			//Do not block all signals ??. Don't need if SA_NOMASK in flags

  {$ifdef ver1_0}
   SigAction(SIGCHLD, @Act, @OldAct);
  {$else}
   fpsigaction(SIGCHLD, @Act, @OldAct);
  {$endif}

  FreeMem(Act, SizeOf(SigActionRec));
  FreeMem(OldAct, SizeOf(SigActionRec));
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
  {$ifdef ver1_0}ExitProcess{$else}fpExit{$endif}(Result);
end;


constructor TThread.Create(CreateSuspended: Boolean);
var
  Flags: Integer;
begin
  inherited Create;
  AddThread(self);
  FSuspended := CreateSuspended;
  Flags := CLONE_VM + CLONE_FS + CLONE_FILES + CLONE_SIGHAND + SIGCHLD;
  { Setup 16k of stack }
  FStackSize:=16384;
  Getmem(pointer(FStackPointer),FStackSize);
  inc(FStackPointer,FStackSize);
  FCallExitProcess:=false;
  { Clone }
  FHandle:= Clone(@ThreadProc,pointer(FStackPointer),Flags,self);
  if FSuspended then Suspend;
  FThreadID := FHandle;
  IsMultiThread := TRUE;
end;


destructor TThread.Destroy;
begin
  if not FFinished and not Suspended then
   begin
     Terminate;
     WaitFor;
   end;
  if FHandle <> -1 then
    {$ifdef ver1_0}kill({$else}fpkill({$endif}FHandle, SIGKILL);
  dec(FStackPointer,FStackSize);
  Freemem(pointer(FStackPointer),FStackSize);
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
{ I Don't know idle or timecritical, value is also 20, so the largest other
  possibility is 19 (PFV) }
  Priorities: array [TThreadPriority] of Integer =
   (-20,-19,-10,9,10,19,20);

function TThread.GetPriority: TThreadPriority;
var
  P: Integer;
  I: TThreadPriority;
begin
  P := {$ifdef ver1_0}
         Linux.getpriority
       {$else}
         Unix.fpGetPriority
       {$endif}  	(Prio_Process,FHandle);
  Result := tpNormal;
  for I := Low(TThreadPriority) to High(TThreadPriority) do
    if Priorities[I] = P then
      Result := I;
end;


procedure TThread.SetPriority(Value: TThreadPriority);
begin
       {$ifdef ver1_0}
         Linux.Setpriority
       {$else}
        Unix.fpSetPriority
       {$endif} (Prio_Process,FHandle, Priorities[Value]);
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
  {$ifdef ver1_0}kill({$else}fpkill({$endif}FHandle, SIGSTOP);
  FSuspended := true;
end;


procedure TThread.Resume;
begin
  {$ifdef ver1_0}kill({$else}fpkill({$endif}FHandle, SIGCONT);
  FSuspended := False;
end;


procedure TThread.Terminate;
begin
  FTerminated := True;
end;

function TThread.WaitFor: Integer;
var
  status : longint;
begin
{$ifdef ver1_0}
  if FThreadID = MainThreadID then
   WaitPid(0,@status,0)
  else
   WaitPid(FHandle,@status,0);
{$else}
  if FThreadID = MainThreadID then
   fpWaitPid(0,@status,0)
  else
   fpWaitPid(FHandle,@status,0);
{$endif}
  Result:=status;
end;

{
  $Log$
  Revision 1.11  2003-09-20 14:51:42  marco
   * small v1_0 fix

  Revision 1.10  2003/09/20 12:38:29  marco
   * FCL now compiles for FreeBSD with new 1.1. Now Linux.

  Revision 1.9  2003/01/17 19:01:07  marco
   * small fix

  Revision 1.8  2002/11/17 21:09:44  marco
   * 16byte sigset

  Revision 1.7  2002/10/24 12:47:54  marco
   * Fix emptying sa_mask

  Revision 1.6  2002/09/07 15:15:24  peter
    * old logs removed and tabs fixed

}
