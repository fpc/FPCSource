{ %cpu=i8086 }

{ test for local label data access from within inline asm }

{ this test is Turbo Pascal 7 compatible }

program tlbldat1;
label
  lbl;
var
  a, a_, a__,
  a2, a2_, a2__, a2___, a2____,
  b, b_, b__,
  b2, b2_, b2__, b2___, b2____: Word;
begin
  asm
    mov ax, word [lbl]
    mov a, ax
    mov ax, word ptr [lbl]
    mov a_, ax
    mov ax, word ptr lbl
    mov a__, ax
    mov ax, word [lbl + 2]
    mov a2, ax
    mov ax, word ptr [lbl + 2]
    mov a2_, ax
    mov ax, word ptr lbl + 2
    mov a2__, ax
    mov ax, word [2 + lbl]
    mov a2___, ax
    mov ax, word ptr [2 + lbl]
    mov a2____, ax
    mov ax, word [@@loc_lbl]
    mov b, ax
    mov ax, word ptr [@@loc_lbl]
    mov b_, ax
    mov ax, word ptr @@loc_lbl
    mov b__, ax
    mov ax, word [@@loc_lbl + 2]
    mov b2, ax
    mov ax, word ptr [@@loc_lbl + 2]
    mov b2_, ax
    mov ax, word ptr @@loc_lbl + 2
    mov b2__, ax
    mov ax, word [2 + @@loc_lbl]
    mov b2___, ax
    mov ax, word ptr [2 + @@loc_lbl]
    mov b2____, ax
    jmp @@GoOn
lbl:
    dw $1234
    dw $4321
@@loc_lbl:
    dw $5678
    dw $8765
@@GoOn:
  end;
  if (a=$1234) and (a_=$1234) and (a__=$1234) and
     (a2=$4321) and (a2_=$4321) and (a2__=$4321) and (a2___=$4321) and (a2____=$4321) and
     (b=$5678) and (b_=$5678) and (b__=$5678) and
     (b2=$8765) and (b2_=$8765) and (b2__=$8765) and (b2___=$8765) and (b2____=$8765) then
    Writeln('Ok!')
  else
  begin
    Writeln('Error');
    Halt(1);
  end;
end.
