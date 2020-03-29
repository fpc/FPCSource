program ttbits;

{$MODE objfpc}{$H+}

uses
  Classes;

procedure Fail;
begin
  Writeln('Err!');
  Halt(1);
end;

procedure FillWithRandom(b: TBits);
var
  I: Integer;
begin
  for I := 0 to b.Size - 1 do
    b[I] := Random(2) <> 0;
end;

procedure TestCopyBits;
const
  NumTests = 100;
  MaxBits = 200;
var
  b1: TBits = nil;
  b2: TBits = nil;
  I: Integer;
begin
  try
    b1 := TBits.Create;
    b2 := TBits.Create;
    for I := 1 to NumTests do
    begin
      b1.Size := Random(MaxBits);
      FillWithRandom(b1);
      b2.CopyBits(b1);
      if not b1.Equals(b2) then
        Fail;
      if not b2.Equals(b1) then
        Fail;
    end;
  finally
    b1.Free;
    b2.Free;
  end;
end;

begin
  TestCopyBits;
  Writeln('Ok!');
end.
