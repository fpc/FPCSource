{ %NORUN }
{ %CPU=i8086 }

{$IFDEF FPC}
{$MODE TP}
{$ENDIF}
program tasm19;

procedure subpcrash(x: word);

  procedure subtocrash; assembler;
  asm
    { compiler crash here... }
    mov ax,[si+offset x]
  end;

begin
end;

begin
end.
