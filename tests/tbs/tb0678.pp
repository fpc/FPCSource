{%skiptarget=$nothread }

program tqueue;

{$mode objfpc}

uses
{$ifdef unix}
  cthreads,
{$endif}
  SysUtils, Classes;

type
  TTest = class
    procedure DoTest;
  end;

  TTestThread = class(TThread)
  protected
    procedure Execute; override;
  end;

var
  count: LongInt = 0;

procedure TTest.DoTest;
begin
  Inc(count);
end;

var
  e: PRTLEvent;
  t1, t2: TTest;

procedure TTestThread.Execute;
var
  method: TMethod;
begin
  Queue(@t1.DoTest);
  Queue(@t2.DoTest);

  { should remove nothing }
  method.Code := @TTest.DoTest;
  method.Data := Nil;

  RemoveQueuedEvents(TThreadMethod(method));

  { should remove only one }
  RemoveQueuedEvents(@t1.DoTest);

  RTLEventSetEvent(e);
end;

var
  t: TTestThread;
begin
  e := Nil;
  t := TTestThread.Create(True);
  try
    e := RTLEventCreate;

    t1 := TTest.Create;
    t2 := TTest.Create;

    t.Start;
    RTLEventWaitFor(e);
    t.WaitFor;

    CheckSynchronize;

    if count <> 1 then
      Halt(1);
  finally
    t1.Free;
    t2.Free;
    t.Free;
    RTLEventDestroy(e);
  end;
end.

