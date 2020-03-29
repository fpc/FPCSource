{ %CPU=i8086 }

{$IFDEF FPC}
{$MODE TP}
{$ENDIF}

program tasm22;

var
  p: pointer;

procedure t; assembler;
asm
  call dword ptr es:p
end;

begin
end.
