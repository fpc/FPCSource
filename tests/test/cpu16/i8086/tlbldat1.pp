{ %cpu=i8086 }

{ test for local label data access from within inline asm }

{ this test is Turbo Pascal 7 compatible }

program tlbldat1;
label
  lbl;
var
  a, a2, b, b2: Word;
begin
  asm
    mov ax, word ptr [lbl]
    mov a, ax
    mov ax, word ptr [lbl + 2]
    mov a2, ax
    mov ax, word ptr [@@loc_lbl]
    mov b, ax
    mov ax, word ptr [@@loc_lbl + 2]
    mov b2, ax
    jmp @@GoOn
lbl:
    dw $1234
    dw $4321
@@loc_lbl:
    dw $5678
    dw $8765
@@GoOn:
  end;
  if (a=$1234) and (a2=$4321) and (b=$5678) and (b2=$8765) then
    Writeln('Ok!')
  else
  begin
    Writeln('Error');
    Halt(1);
  end;
end.
