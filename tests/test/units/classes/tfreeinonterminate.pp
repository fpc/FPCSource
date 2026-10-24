{%skiptarget=$nothread }
{ A thread can be destroyed by its own OnTerminate handler, issue #32759 }
program tfreeinonterminate;

{$mode objfpc}{$h+}

uses
{$ifdef unix}
  cthreads,
{$endif}
  SysUtils, Classes;

type
  TWorker = class(TThread)
  protected
    procedure Execute; override;
  public
    destructor Destroy; override;
  end;

  TWatchdog = class(TThread)
  protected
    procedure Execute; override;
  end;

  THandlers = class
  public
    // OnTerminate handler that frees the thread
    procedure FreeSender(Sender: TObject);
    // OnTerminate handler that only counts the call
    procedure CountSender(Sender: TObject);
  end;

var
  lDestroyCount: Integer = 0;
  lTerminateCount: Integer = 0;
  lDone: Boolean = False;


procedure TWorker.Execute;

begin
  Sleep(10);
end;


destructor TWorker.Destroy;

begin
  InterlockedIncrement(lDestroyCount);
  inherited Destroy;
end;


procedure TWatchdog.Execute;

var
  i: Integer;
begin
  for i:=1 to 300 do
    begin
    Sleep(100);
    if lDone then
      exit;
    end;
  Writeln('Failed: deadlock');
  Halt(1);
end;


procedure THandlers.FreeSender(Sender: TObject);

begin
  Inc(lTerminateCount);
  Sender.Free;
end;


procedure THandlers.CountSender(Sender: TObject);

begin
  Inc(lTerminateCount);
end;


procedure Check(aCondition: Boolean; aCode: Integer; const aMsg: string);

begin
  if not aCondition then
    begin
    Writeln('Failed: ',aMsg);
    Halt(aCode);
    end;
end;


{ Runs CheckSynchronize until the OnTerminate handler ran aCount times }
procedure WaitForHandler(aCount: Integer);

begin
  while lTerminateCount<aCount do
    CheckSynchronize(10);
  { lets the thread function finish }
  Sleep(50);
  CheckSynchronize(0);
end;


procedure TestFree(aHandlers: THandlers; aFreeOnTerminate: Boolean; aCode: Integer);

var
  lWorker: TWorker;
  i: Integer;
begin
  lDestroyCount:=0;
  lTerminateCount:=0;
  for i:=1 to 20 do
    begin
    lWorker:=TWorker.Create(True);
    lWorker.FreeOnTerminate:=aFreeOnTerminate;
    lWorker.OnTerminate:=@aHandlers.FreeSender;
    lWorker.Start;
    WaitForHandler(i);
    Check(lDestroyCount=i,aCode,'thread is destroyed once by its OnTerminate handler');
    end;
end;


procedure TestNoFree(aHandlers: THandlers);

var
  lWorker: TWorker;
begin
  lDestroyCount:=0;
  lTerminateCount:=0;
  lWorker:=TWorker.Create(True);
  lWorker.OnTerminate:=@aHandlers.CountSender;
  lWorker.Start;
  WaitForHandler(1);
  lWorker.WaitFor;
  Check(lWorker.Finished,3,'thread is finished after WaitFor');
  lWorker.Free;
  Check(lDestroyCount=1,4,'thread freed after its OnTerminate handler is destroyed');
end;


var
  lHandlers: THandlers;
  lWatchdog: TWatchdog;
begin
  lWatchdog:=TWatchdog.Create(False);
  lHandlers:=THandlers.Create;
  try
    TestFree(lHandlers,False,1);
    TestFree(lHandlers,True,2);
    TestNoFree(lHandlers);
  finally
    lHandlers.Free;
  end;
  lDone:=True;
  lWatchdog.WaitFor;
  lWatchdog.Free;
  Writeln('ok');
end.
