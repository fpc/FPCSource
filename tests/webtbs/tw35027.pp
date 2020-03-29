program tw35027;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, sysutils, syncobjs;

type
  MT1= class(TThread)
    procedure Execute; override;
  private
    procedure MySync;
  end;

  { MT2 }

  MT2= class(TThread)
    procedure Execute; override;
  private
    procedure MySync2;
  end;
var
  E1, E2, E3: TEventObject;
  T1: MT1;
  T2: MT2;
  MT1Count, MT2Count: Integer;

{ MT2 }

procedure MT2.Execute;
begin
  E1.WaitFor(INFINITE);
  Sleep(100);
  try
    Synchronize(@MySync2);
  except end;
end;

procedure MT2.MySync2;
begin
  Inc(MT2Count);
  writeln('x2 ');
  raise Exception.Create('Foo'); // prevent event^.Method from being set to nil
end;

procedure MT1.Execute;
begin
  E1.SetEvent;
  try
    Synchronize(@MySync);
  except end;
  E3.SetEvent;
  E2.WaitFor(INFINITE);
  try
    Synchronize(@MySync);
  except end;
end;

procedure MT1.MySync;
begin
  Inc(MT1Count);
  writeln('x');
  raise Exception.Create('Foo'); // prevent event^.Next from being set to nil
end;

begin
  E1 := TEvent.Create(Nil, False, False, '');
  E2 := TEvent.Create(Nil, False, False, '');
  E3 := TEvent.Create(Nil, False, False, '');
  T1 := MT1.Create(False);
  T2 := MT2.Create(False);
  Sleep(2000);
  CheckSynchronize(1000);
  CheckSynchronize(1000);
  E3.WaitFor(INFINITE);
  E2.SetEvent;
  CheckSynchronize(1000);
  CheckSynchronize(1000);
  if (MT1Count <> 2) or (MT2Count <> 1) then
    Halt(1);
  Writeln('ok');
end.
