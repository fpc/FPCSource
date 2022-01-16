{ %OPT = -gh }

program tarray22;

{$mode objfpc}{$h+}

type
  TIntegerArray = array of Integer;
  TStringArray = array of String;

generic procedure CheckArray<T>(const Actual, Expected: array of T; Code: LongInt);
var
  i: SizeInt;
begin
  if Length(Actual) <> Length(Expected) then
    Halt(Code);
  for i := 0 to High(Actual) do
    if Actual[i] <> Expected[i] then
      Halt(Code);
end;

procedure TestOpen(const A: array of Integer; Exp: array of Integer; Code: LongInt);
var
  B: array of Integer;
begin
  B := Copy(A);
  specialize CheckArray<Integer>(B, Exp, Code);
end;

procedure TestOpen2(const A: array of Integer; Exp: array of Integer; Code: LongInt);
var
  B: array of Integer;
begin
  B := Copy(A, 1, 2);
  specialize CheckArray<Integer>(B, Exp, Code);
end;

procedure TestDyn(const A: TIntegerArray; Exp: array of Integer; Code: LongInt);
var
  B: array of Integer;
begin
  B := Copy(A);
  specialize CheckArray<Integer>(B, Exp, Code);
end;

procedure TestDyn2(const A: TIntegerArray; Exp: array of Integer; Code: LongInt);
var
  B: array of Integer;
begin
  B := Copy(A, 1, 2);
  specialize CheckArray<Integer>(B, Exp, Code);
end;

procedure TestOpen(const A: array of String; Exp: array of String; Code: LongInt);
var
  B: array of String;
begin
  B := Copy(A);
  specialize CheckArray<String>(B, Exp, Code);
end;

procedure TestOpen2(const A: array of String; Exp: array of String; Code: LongInt);
var
  B: array of String;
begin
  B := Copy(A, 1, 2);
  specialize CheckArray<String>(B, Exp, Code);
end;

procedure TestDyn(const A: TStringArray; Exp: array of String; Code: LongInt);
var
  B: array of String;
begin
  B := Copy(A);
  specialize CheckArray<String>(B, Exp, Code);
end;

procedure TestDyn2(const A: TStringArray; Exp: array of String; Code: LongInt);
var
  B: array of String;
begin
  B := Copy(A, 1, 2);
  specialize CheckArray<String>(B, Exp, Code);
end;

begin
  HaltOnNotReleased := True;

  TestOpen([0, 1, 2, 3, 4, 5], [0, 1, 2, 3, 4, 5], 1);
  TestOpen2([0, 1, 2, 3, 4, 5], [1, 2], 2);
  TestDyn([0, 1, 2, 3, 4, 5], [0, 1, 2, 3, 4, 5], 3);
  TestDyn2([0, 1, 2, 3, 4, 5], [1, 2], 4);

  TestOpen(['Alpha', 'Beta', 'Gamma', 'Delta'], ['Alpha', 'Beta', 'Gamma', 'Delta'], 5);
  TestOpen2(['Alpha', 'Beta', 'Gamma', 'Delta'], ['Beta', 'Gamma'], 6);
  TestDyn(['Alpha', 'Beta', 'Gamma', 'Delta'], ['Alpha', 'Beta', 'Gamma', 'Delta'], 7);
  TestDyn2(['Alpha', 'Beta', 'Gamma', 'Delta'], ['Beta', 'Gamma'], 8);
end.

