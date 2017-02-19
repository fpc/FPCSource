program tmoperator8;

{$MODE DELPHI}

type
  TCopyState = (csNone, csSource, csDest);
  PFoo = ^TFoo;
  TFoo = record
  private
    class operator Initialize(var aFoo: TFoo);
    class operator Finalize(var aFoo: TFoo);
    class operator AddRef(var aFoo: TFoo);
    class operator Copy(constref aSrc: TFoo; var aDst: TFoo);
  public
    CopyState: TCopyState;
    Ref: Boolean;
    F, Test: Integer;
  end;

  TFooArray = array of TFoo;

procedure TestFoo(const AValue: TFoo; AF, ATest: Integer; ARef: Boolean; ACopyState: TCopyState);
begin
  WriteLn('    AValue.F = ', AValue.F);
  if AValue.F <> AF then
    Halt(1);
  WriteLn('    AValue.Test = ', AValue.Test);
  if AValue.Test <> ATest then
    Halt(2);
  WriteLn('    AValue.Ref = ', AValue.Ref);
  if AValue.Ref <> ARef then
    Halt(4);
  WriteLn('    AValue.CopyState = ', Ord(AValue.CopyState));
  if AValue.CopyState <> ACopyState then
    Halt(3);
end;

class operator TFoo.Initialize(var aFoo: TFoo);
begin
  WriteLn('TFoo.Initialize');
  aFoo.F := 1;
  aFoo.Ref := False;
  aFoo.Test := 0;
  aFoo.CopyState := csNone;
end;

class operator TFoo.Finalize(var aFoo: TFoo);
begin
  WriteLn('TFoo.Finalize');
  if (aFoo.F <> 2) and not ((aFoo.F = 3) and aFoo.Ref) then
    Halt(5);
  aFoo.F := 4;
end;

class operator TFoo.AddRef(var aFoo: TFoo);
begin
  WriteLn('TFoo.AddRef');
  aFoo.F := 3;
  aFoo.Test := aFoo.Test + 1;
  aFoo.Ref := True;
end;

class operator TFoo.Copy(constref aSrc: TFoo; var aDst: TFoo);
var
  LSrc: PFoo;
begin
  WriteLn('TFoo.Copy');
  LSrc := @aSrc;
  LSrc.CopyState := csSource;
  aDst.CopyState := csDest;
  aDst.Test := aSrc.Test + 1;
  aDst.F := aSrc.F;
end;

procedure TestValue(Value: TFoo);
begin
  writeln('  *Test without modifier:');
  TestFoo(Value, 3, 1, True, csNone);
end;

procedure TestOut(out Value: TFoo);
begin
  WriteLn('  *Test out modifier:');
  TestFoo(Value, 1, 0, False, csNone);
  Value.F := 2;
end;

procedure TestVar(var Value: TFoo);
begin
  writeln('  *Test var modifier:');
  TestFoo(Value, 2, 0, False, csNone);
end;

procedure TestConst(const Value: TFoo);
begin
  writeln('  *Test const modifier:');
  TestFoo(Value, 2, 0, False, csNone);
end;

procedure TestConstref(constref Value: TFoo);
begin
  WriteLn('  *Test constref modifier:');
  TestFoo(Value, 2, 0, False, csNone);
end;

procedure Test;
var
  Foos: TFooArray;
  Foos2: TFooArray;
  A, B, C: TFoo;
  i: Integer;
begin
  WriteLn('*** Test for variable copy');
  TestFoo(B, 1, 0, False, csNone);
  B.F := 2;
  A := B;
  TestFoo(B, 2, 0, False, csSource);
  TestFoo(A, 2, 1, False, csDest);

  WriteLn('*** Test for Copy(dyn array)');
  SetLength(Foos, 5);
  for i := 0 to 4 do
  begin
    Foos[i].F := 2;
    Foos[i].Test := i;
  end;

  Foos2 := Copy(Foos);

  for i := 0 to 4 do
  begin
    TestFoo(Foos[i], 2, i, False, csNone);
    TestFoo(Foos2[i], 3, i + 1, True, csNone);
  end;

  WriteLn('*** Test for parameters modifiers');
  TestValue(C);
  C.F := 2; // reset F to pass finalize before out parameter
  TestOut(C);
  TestVar(C);
  TestConst(C);
  TestConstref(C);
end;

begin
  Test;
  WriteLn('end');
end.
