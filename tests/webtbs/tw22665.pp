{ %cpu=x86_64 }
{ %opt=-Cg }

{$asmmode intel}

var
  val: qword; public;

Function Test: QWord; Assembler; NoStackFrame;
Asm
{$ifdef win64}
 MOV RAX, [RIP+Val]
{$else}
 mov RAX, [val wrt ..gotpcrel]
 mov RAX, [RAX]
{$endif}
End;

BEGIN
  Val := $12345678901;
  if test<>$12345678901 then
    halt(1);
END.

