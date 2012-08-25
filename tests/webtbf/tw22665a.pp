{ %target=win64 }
{ %cpu=x86_64 }
{ %fail }

{$asmmode intel}

var
  val: qword;

Function Test: QWord; Assembler; NoStackFrame;
Asm
 { win64 doesn't support a GOT }
 mov RAX, [val wrt ..gotpcrel]
 mov RAX, [RAX]
End;

BEGIN
  Val := $12345678901;
  if test<>$12345678901 then
    halt(1);
END.

