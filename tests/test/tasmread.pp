{ Checks for qualified variable support
   in assembler reader }
program test;

{$ifdef cpui386}
{$asmmode intel}
{$endif}

var l: longint;

begin
{$ifdef cpui386}
  asm
     mov test.l, 5
  end;
{$endif cpui386}
{$ifdef cpu68k}
  asm
     move.l  test.l,d0
  end;
{$endif cpu68k}

end.
