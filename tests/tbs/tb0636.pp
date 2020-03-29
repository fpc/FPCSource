{%skiptarget=$nothread }
program tb0636;

{$mode objfpc}

uses
{$ifdef unix}
  cthreads,
{$endif}
  Classes, syncobjs;

type
  TTestThread1 = class(TThread)
  private
    fOnNotify: TNotifyEvent;
  protected
    procedure Execute; override;
  public
    property OnNotify: TNotifyEvent read fOnNotify write fOnNotify;
  end;

  TTestThread2 = class(TThread)
  private
    procedure DoSync;
    procedure DoSync2;
  private
    procedure HandleNotify(aSender: TObject);
  protected
    procedure Execute; override;
  end;

var
  ev1, ev2: TEvent;
  sync1, sync2: LongInt;

{ TTestThread2 }

procedure TTestThread2.DoSync;
begin
  Inc(sync1);
  Writeln('main in t2: DoSync');
end;

procedure TTestThread2.DoSync2;
begin
  Inc(sync2);
  Writeln('main in t2: DoSync2');
end;

procedure TTestThread2.HandleNotify(aSender: TObject);
begin
  Writeln('t1 in t2: Signalling ev1');
  ev1.SetEvent;
  Writeln('t1 in t2: Synchronizing DoSync');
  Synchronize(@DoSync);
end;

procedure TTestThread2.Execute;
begin
  Writeln('t2: Waiting for ev1');
  ev1.WaitFor(INFINITE);
  Sleep(100);
  Writeln('t2: Signalling ev2');
  ev2.SetEvent;
  Writeln('t2: Synchronizing DoSync2');
  Synchronize(@DoSync2);
  Writeln('t2: Waiting for ev1');
  ev1.WaitFor(INFINITE);
  Writeln('t2: Done');
end;

{ TTestThread1 }

procedure TTestThread1.Execute;
begin
  Writeln('t1: Calling fOnNotify');
  fOnNotify(Self);
  Writeln('t1: Done');
end;

var
  t1: TTestThread1;
  t2: TTestThread2;
begin
  sync1 := 0;
  sync2 := 0;

  ev1 := Nil;
  ev2 := Nil;
  t1 := Nil;
  t2 := Nil;

  try
    ev1 := TEvent.Create(Nil, False, False, '');
    ev2 := TEvent.Create(Nil, False, False, '');
    t1 := TTestThread1.Create(True);
    t2 := TTestThread2.Create(True);

    Writeln('main: Starting t2');
    t2.Start;

    t1.OnNotify := @t2.HandleNotify;
    Writeln('main: Starting t1');
    t1.Start;

    Writeln('main: Waiting for ev2');
    ev2.WaitFor(INFINITE);

    Writeln('main: Calling CheckSynchronize');
    CheckSynchronize();

    Writeln('main: Signalling ev1');
    ev1.SetEvent;

    Writeln('main: Waiting for threads');
    t1.WaitFor;
    t2.WaitFor;

    Writeln('main: Cleaning up');
  finally
    t1.Free;
    t2.Free;
    ev1.Free;
    ev2.Free;
  end;

  if (sync1 <> 1) or (sync2 <> 1) then
    Halt(1);
  Writeln('main: ok');
end.
