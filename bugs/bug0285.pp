{$asmmode intel}

TYPE something = RECORD big:LONGINT; small:BYTE; END;

FUNCTION typesize:INTEGER; ASSEMBLER;
ASM
     MOV EAX, TYPE something
END;

BEGIN
  writeln(typesize);
END.

