{ %CPU=i386 }
{ Old file: tbs0285.pp }
{ Asm, TYPE not support in intel mode                  OK 0.99.13 (PFV) }

{$asmmode intel}

TYPE something = RECORD big:LONGINT; small:BYTE; END;

FUNCTION typesize:INTEGER; ASSEMBLER;
ASM
     MOV EAX, TYPE something
END;

BEGIN
  writeln(typesize);
  if typesize<>sizeof(something) then
    begin
      Writeln('Error in type inside intel asm');
      Halt(1);
    end;
END.
