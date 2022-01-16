{ %TARGET = Win64 }

program tw34496;

{$MODE DELPHI}
{$WARN 5079 OFF}

uses
  TypInfo,
  Rtti;

procedure Test1(const d1, d2: Double);
begin
  WriteLn(d1:0:2,' - ', d2:0:2);
end;

procedure Test2(const d1, d2: Extended);
begin
  WriteLn(d1:0:2,' - ', d2:0:2);
end;

var
  a, b: Double;
begin
  a := 12.34;
  b := 56.78;
  Rtti.Invoke(@Test1, [a, b], ccReg, nil, True, False);
  Rtti.Invoke(@Test2, [a, b], ccReg, nil, True, False);
  //ReadLn;
end.
