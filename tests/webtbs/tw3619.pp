{ %cpu=i386 }

{ Source provided for Free Pascal Bug Report 3619 }
{ Submitted by "Thomas Schatzl" on  2005-01-31 }
{ e-mail:  }
program Project2;

{$asmmode intel}

procedure x(bpp : longint); assembler;
asm
  MOV BL,4
  SUB bl, byte ptr [bpp]
end;

begin
end.
