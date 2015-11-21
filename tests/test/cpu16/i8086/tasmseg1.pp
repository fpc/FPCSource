{ %cpu=i8086 }

{ i8086 test for the SEG @CODE and SEG @DATA inline assembler directives }

{ this test is Turbo Pascal 7 compatible }

program tasmseg1;
var
  Error: Boolean;
begin
  Error := False;
  asm
    jmp @@Skip
@@dw_seg_code:
    dw SEG @CODE
@@dw_seg_data:
    dw SEG @DATA
    dw 55aah
    dd SEG @CODE
    dd SEG @DATA
    dw 0aa55h
@@Skip:
    mov bx, SEG @CODE
    mov cx, cs
    cmp bx, cx
    jne @@Err
    mov bx, word ptr @@dw_seg_code
    cmp bx, cx
    jne @@Err
    mov bx, word ptr [@@dw_seg_code + 6]
    cmp bx, cx
    jne @@Err
    mov dx, SEG @DATA
    mov ax, ds
    cmp ax, dx
    jne @@Err
    mov dx, word ptr @@dw_seg_data
    cmp ax, dx
    jne @@Err
    mov dx, word ptr [@@dw_seg_code + 2]
    cmp ax, dx
    jne @@Err
    mov dx, word ptr [@@dw_seg_code + 10]
    cmp ax, dx
    jne @@Err
    mov ax, word ptr [@@dw_seg_code + 8]
    or ax, word ptr [@@dw_seg_code + 12]
    jnz @@Err
    cmp word ptr [@@dw_seg_data + 2], 55aah
    jne @@Err
    cmp word ptr [@@dw_seg_data + 12], 0aa55h
    jne @@Err
    jmp @@Done
@@Err:
    mov Error, 1
@@Done:
  end;
  if Error then
  begin
    Writeln('Error!');
    Halt(1);
  end
  else
    Writeln('Ok!');
end.
