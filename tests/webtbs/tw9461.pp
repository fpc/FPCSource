{ %CPU=arm }
procedure p; assembler;
  var
    i : longint;
  asm
    mla r0,r1,r2,r3
  end;

begin
end.
