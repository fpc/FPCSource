{ %cpu=aarch64 }
{ %norun }

{$mode delphi}
program AsmQBug;

type
  PVector = ^TVector;
  TVector = record
    A, B, C, D: Single;
  end;

function AsmFunc(V: PVector): Single;
{ Delphi code:
begin
  Result := V.A;
end; }
asm
  // This gives error "unknown identifier: NE"
  fccmp  d0, d0, #0x0, ne
  fccmpe  d0, d0, #0x0, ne
end;

var
  V: TVector;
begin
  V.A := 1;
  V.B := 2;
  V.C := 3;
  V.D := 4;
  WriteLn('Expected=1 Actual=', AsmFunc(@V));
end.
