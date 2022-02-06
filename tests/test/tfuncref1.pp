{ %OPT=-gh }

program tfuncref1;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}

type
  TTest1 = reference to procedure;
  TTest2 = reference to function: LongInt;
  TTest3 = reference to function(aArg: String): LongInt;
  generic TTest4<T> = reference to procedure(aArg: T);

  TImpl1 = class(TInterfacedObject, TTest1)
    procedure Invoke;
  end;

  TImpl2 = class(TInterfacedObject, TTest2)
    function Invoke: LongInt;
  end;

  TImpl3 = class(TInterfacedObject, TTest3)
    function Invoke(aArg: String): LongInt;
  end;

  TImpl4 = class(TInterfacedObject, specialize TTest4<LongInt>)
    procedure Invoke(aArg: LongInt);
  end;

var
  invokeid: LongInt = 0;

procedure TImpl1.Invoke;
begin
  invokeid := 1;
end;

function TImpl2.Invoke: LongInt;
begin
  invokeid := 2;
  Result := 21;
end;

function TImpl3.Invoke(aArg: String): LongInt;
begin
  invokeid := 3;
  Result := 42;
end;

procedure TImpl4.Invoke(aArg: LongInt);
begin
  invokeid := 4;
end;

var
  impl1: TTest1;
  impl2: TTest2;
  impl3: TTest3;
  impl4: specialize TTest4<LongInt>;
begin
  {$if declared(HaltOnNotReleased)}
  HaltOnNotReleased:=True;
  {$endif}
  invokeid := 0;
  impl1 := TImpl1.Create;
  impl1();
  if invokeid <> 1 then
    Halt(1);
  invokeid := 0;
  impl2 := TImpl2.Create;
  impl2();
  if invokeid <> 2 then
    Halt(2);
  invokeid := 0;
  impl3 := TImpl3.Create;
  impl3('Foobar');
  if invokeid <> 3 then
    Halt(3);
  invokeid := 0;
  impl4 := TImpl4.Create;
  impl4(42);
  if invokeid <> 4 then
    Halt(4);
end.
