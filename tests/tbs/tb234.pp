{ Old file: tbs0267.pp }
{ parameters after methodpointer are wrong             OK 0.99.12b (FK) }

{$MODE objfpc}

program procofobject_arg;
type
  TProcOfObject = procedure of object;
  TTestClass = class
    procedure SomeMethod;
  end;

procedure TTestClass.SomeMethod; begin end;


// the following proc won't print i2 correctly

procedure CrashProc(i1: Integer;method: TProcOfObject; i2: Integer);
begin
  WriteLn('i1 is :', i1);
  WriteLn('i2 is :', i2);
  if i2<>456 then
    Halt(1);
end;

var
  instance: TTestClass;
begin
  instance := TTestClass.Create;
  CrashProc(123, @instance.SomeMethod, 456);
end.
