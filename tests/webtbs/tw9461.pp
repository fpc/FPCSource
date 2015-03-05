{ %CPU=arm }
procedure p; assembler;
  var
    i : longint;
  asm
{$ifndef CPUTHUMB}
    mla r0,r1,r2,r3
{$endif CPUTHUMB}
  end;

begin
end.
