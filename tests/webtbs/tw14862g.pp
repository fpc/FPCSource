{ %cpu=x86_64 }
{ %opt=-Cg- }


var
  f1, f2, f3: int64;
begin
{$asmmode att}
  asm
    // set alignment check flag so at least one flag in the upper 16 bits
    // is set
    pushfq
    popq %rax
    orq $(1 << 18), %rax
    pushq %rax
    popfq

    // should be same as pushfq in AT&T mode
    xorq %rax, %rax
    pushfw
    popw %ax
    movq %rax, f1(%rip)
    pushf
    popq %rax
    movq %rax, f2(%rip)
    pushfq
    popq %rax
    movq %rax, f3(%rip)
  end;
  if (f1 > high(word)) then
    halt(1);
  if (f2 < high(word)) or
     ((f2 and high(word))<>f1) then
    halt(2);
  if (f3 <> f2) then
    halt(3);
  f1:=0;
  f2:=0;
{$asmmode intel}
  asm
    // should be same as pushfw in Intel mode
    xor rax, rax
    pushf
    pop ax
    mov [f1 + rip], rax
    pushfq
    pop rax
    mov [f2 + rip], rax

    // clear alignment check flag again to be safe
    and rax, not(1 shl 18)
    push rax
    popfq
  end;
  if (f1 > high(word)) then
    halt(4);
  if (f2 < high(word)) or
     ((f2 and high(word))<>f1) then
    halt(5);
end.
