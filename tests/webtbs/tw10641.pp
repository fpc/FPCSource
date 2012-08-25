{ %cpu=x86_64 }
{ %opt=-vw -Sew }

{$ifdef windows}
  {$imagebase $10000}
{$endif}
var
  test : dword;
  test2 : dword;
begin
  test:=$deadbeef;
  test2:=$deadbeef;
  ASM
    MOVL $16,%EAX
    LEA .LLT(%RIP),%RBX
    JMP (%RBX,%RAX)
    .balign 16
.LLT:
    .quad .L3,.L2,.L1
.L2:
    MOVL $12341234,test2(%RIP)
    JMP  .L3
.L1:
    MOVL $0,test(%RIP)
.L3:
  END;
  if (test<>0) or (test2<>$deadbeef) then
    halt(1);
  writeln('ok');
end.
