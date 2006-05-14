{ %cpu=arm }
{ Source provided for Free Pascal Bug Report 4722 }
{ Submitted by "Francesco Lombardi" on  2006-01-21 }
{ e-mail: francky74@gmail.com }
procedure SystemCall(n: integer); assembler;
asm
  MOV    R0, R0, LSL #0x10
  SWI    R0
end;

begin
end.
