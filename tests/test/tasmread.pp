program test;

{$asmmode intel}

var l: longint;

begin
  asm
     mov test.l, 5
  end;
end.
