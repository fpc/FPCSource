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

