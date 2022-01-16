program tw29372;

{$MODE DELPHI}
type
  TR1 = record
    A, B, C: Int64;
    constructor Create(_A, _B, _C: Int64);
  end;

  TR2 = record
    D, E, F: Int64;
    constructor Create(_D, _E, _F: Int64);
  end;

  constructor TR1.Create(_A, _B, _C: Int64);
  begin
    A := _A;
    B := _B;
    C := _C;
  end;

  constructor TR2.Create(_D, _E, _F: Int64);
  begin
    D := _D;
    E := _E;
    F := _F;
  end;

{ Note: unlike in the file attached at #29372 we use "const" both times to
        trigger the error on x86_64 as well }
procedure Foo(const _1: TR1; const _2: TR2);
begin
  if _1.A <> 1 then
    Halt(1);
  if _1.B <> 2 then
    Halt(2);
  if _1.C <> 3 then
    Halt(3);
  if _2.D <> 4 then
    Halt(2);
  if _2.E <> 5 then
    Halt(5);
  if _2.F <> 6 then
    Halt(6);
end;

begin
  Foo(TR1.Create(1, 2, 3), TR2.Create(4,5,6));
end.
