{ BeginThread returns a thread handle and passes the thread ID in ThreadId, issue #40809 }
program tthreadidhandle;

{$mode objfpc}{$h+}

uses
{$ifdef unix}
  cthreads,
{$endif}
  SysUtils, Classes;

type
  TIDThread = class(TThread)
  public
    SeenID: TThreadID;
    procedure Execute; override;
  end;

var
  gSeenID: TThreadID;


// Stores the ID of the running thread in gSeenID
function StoreID(aParam: Pointer): PtrInt;

begin
  gSeenID:=GetCurrentThreadId;
  Result:=0;
end;


procedure TIDThread.Execute;

begin
  SeenID:=GetCurrentThreadId;
end;


var
  lHandle: TThreadHandle;
  lID: TThreadID;
  lThread: TIDThread;

begin
{$ifdef windows}
  if SizeOf(TThreadID)<>SizeOf(DWord) then
    begin
    writeln('SizeOf(TThreadID)=',SizeOf(TThreadID),', expected ',SizeOf(DWord));
    halt(1);
    end;
  if SizeOf(TThreadHandle)<>SizeOf(THandle) then
    begin
    writeln('SizeOf(TThreadHandle)=',SizeOf(TThreadHandle),', expected ',SizeOf(THandle));
    halt(2);
    end;
{$endif}
  lID:=TThreadID(0);
  lHandle:=BeginThread(@StoreID,nil,lID);
  if lHandle=TThreadHandle(0) then
    halt(3);
  WaitForThreadTerminate(lHandle,0);
  CloseThread(lHandle);
  if gSeenID<>lID then
    begin
    writeln('BeginThread passed a thread ID different from GetCurrentThreadId in the thread');
    halt(4);
    end;
  if lID=GetCurrentThreadId then
    begin
    writeln('BeginThread passed the ID of the calling thread');
    halt(5);
    end;
  lThread:=TIDThread.Create(False);
  lThread.WaitFor;
  if lThread.SeenID<>lThread.ThreadID then
    begin
    writeln('TThread.ThreadID differs from GetCurrentThreadId in Execute');
    halt(6);
    end;
  lThread.Free;
  writeln('ok');
end.
