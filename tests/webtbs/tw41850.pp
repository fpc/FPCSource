{ A structured assignment that the code generator lowers to a call to FPC_MOVE
  must pass the parameters in the order the target pushes them.  Both sides of
  the call are invisible in the source, so a wrong order silently corrupts
  memory at run time.

  getcopymode() only selects that lowering when the copy is larger than
  3*sizeof(aword) and sits in a routine that already makes a call, so both
  properties of this test are deliberate. }
program tw41850;

type
  TBlock = record
    A: array[0..99] of Byte;
  end;

var
  Src, Dst: TBlock;
  I: Integer;

procedure fail(Index: Integer);
begin
  WriteLn('Failure at ', Index);
  Halt(1);
end;

begin
  for I := 0 to 99 do
    Src.A[I] := Byte(I + 1);
  FillChar(Dst, SizeOf(Dst), 0);
  Dst := Src;
  for I := 0 to 99 do
    if Dst.A[I] <> Byte(I + 1) then
      fail(I);
  WriteLn('Success.');
end.
