program tanonfunc16;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test using 'self' to access containing class from anonymous function }

type
  TProc = reference to procedure;

  TTest = class
	X: Integer;
	property PX: Integer read X;
	procedure Bar;
	function Foo: TProc;
  end;

var
  i: Integer;

function OverloadTest(const O: TObject): Boolean; overload;
begin
  Result := False
end;

function OverloadTest(const O: TTest): Boolean; overload;
begin
  Result := True;
end;

function TTest.Foo: TProc;
begin
  Result := procedure begin
    if not (self is TTest) then
      halt(1);
	if ClassName <> 'TTest' then
      halt(2);
	if not OverloadTest(self) then
      halt(4);
    X := 42;
    if PX <> 42 then
      halt(5);
	Bar;
    if i <> 43 then
      halt(6);
  end;
end;

procedure TTest.Bar;
begin
  i := X + 1;
end;

var
  Obj: TTest;
  Proc: TProc;
begin
  Obj := TTest.Create;
  Proc := Obj.Foo;
  Proc();
  if Obj.X <> 42 then
    halt(7);
  Obj.Free;
end.

