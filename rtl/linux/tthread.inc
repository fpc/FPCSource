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
  {$ifdef ver1_0}waitpid{$else}fpwaitpid{$endif}(-1, nil, WNOHANG);
end;

procedure InitThreads;
var
  Act, OldAct: Baseunix.PSigActionRec;
begin
  ThreadRoot:=nil;
  ThreadsInited:=true;


// This will install SIGCHLD signal handler
// signal() installs "one-shot" handler,
// so it is better to install and set up handler with sigaction()

  GetMem(Act, SizeOf(SigActionRec));
  GetMem(OldAct, SizeOf(SigActionRec));

  Act^.sa_handler := @SIGCHLDHandler;
  Act^.sa_flags := SA_NOCLDSTOP {or SA_NOMASK or SA_RESTART};
  Fillchar(Act^.sa_mask,sizeof(Act^.sa_mask),0); //Do not block all signals ??. Don't need if SA_NOMASK in flags
  {$ifdef ver1_0}
  SigAction(SIGCHLD, Act, OldAct);
  {$else}
  FpSigAction(SIGCHLD, @Act, @OldAct);
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
  {$ifdef ver1_0}ExitProcess{$else}fpexit{$endif}(Result);
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
  FFatalException := nil;
end;


destructor TThread.Destroy;
begin
  if not FFinished and not Suspended then
   begin
     Terminate;
     WaitFor;
   end;
  if FHandle <> -1 then
    {$ifdef ver1_0}Kill{$else}fpkill{$endif}(FHandle, SIGKILL);
  dec(FStackPointer,FStackSize);
  Freemem(pointer(FStackPointer),FStackSize);
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
	 Linux.GetPriority(Prio_Process,FHandle);
       {$else}
         Unix.fpGetPriority(Prio_Process,FHandle);
       {$endif}
  Result := tpNormal;
  for I := Low(TThreadPriority) to High(TThreadPriority) do
    if Priorities[I] = P then
      Result := I;
end;


procedure TThread.SetPriority(Value: TThreadPriority);
begin
       {$ifdef ver1_0}
	 Linux.SetPriority(Prio_Process,FHandle,Priorities[Value]);
       {$else}
         Unix.fpSetPriority(Prio_Process,FHandle,Priorities[Value]);
       {$endif}
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
  {$ifdef ver1_0}Kill{$else}fpkill{$endif}(FHandle, SIGSTOP);
  FSuspended := true;
end;


procedure TThread.Resume;
begin
  {$ifdef ver1_0}Kill{$else}fpkill{$endif}(FHandle, SIGCONT);
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
  if FThreadID = MainThreadID then
   {$ifdef ver1_0}waitpid{$else}fpwaitpid{$endif}(0,@status,0)
  else
   {$ifdef ver1_0}waitpid{$else}fpwaitpid{$endif}(FHandle,@status,0);
  Result:=status;
end;

{
  $Log$
  Revision 1.1  2003-10-06 21:01:06  peter
    * moved classes unit to rtl

  Revision 1.9  2003/10/06 17:06:55  florian
    * applied Johannes Berg's patch for exception handling in threads

  Revision 1.8  2003/09/20 15:10:30  marco
   * small fixes. fcl now compiles

  Revision 1.7  2002/12/18 20:44:36  peter
    * use fillchar to clear sigset

  Revision 1.6  2002/09/07 15:15:27  peter
    * old logs removed and tabs fixed

}
