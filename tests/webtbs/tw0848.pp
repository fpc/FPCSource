{ %CPU=i386 }
{$ASMMODE INTEL}

PROCEDURE a;
VAR v,v2,v3:integer;

  PROCEDURE b;assembler;
    ASM
      MOV AX,v
      mov v2,AX
      mov EDI,0
      MOV AX,[EDI+v]
      MOV AX,[EBP+OFFSET v]
      MOV v3,AX
    END;

BEGIN
  v:=5;
  v2:=4;
  v3:=0;
  b;
  if (v2<>v) or (v3<>v) then
    Halt(1);
END;

begin
 a;
 Writeln('Program works');
end.
