{ %CPU=i8086 }

{$IFDEF FPC}
{$MODE TP}
{$ENDIF FPC}

program tasm24;

procedure t; assembler;
asm
  mov ax, [ds:5+es:7]       { es: }
end;

begin
end.
