{ %cpu=x86_64 }
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
{$ifdef FPC_PIC}
    LEA .LLT(%RIP),%RBX
    JMP (%RBX,%RAX)
{$else not FPC_PIC}
    JMP .LLT(%RAX)
{$endif not FPC_PIC}
    .balign 16
.LLT:
    .quad .L3,.L2,.L1
.L2:
{$ifdef FPC_PIC}
    MOVL $12341234,test2(%RIP)
    JMP  .L3(%RIP)
{$else not FPC_PIC}
    MOVL $12341234,test2
    JMP  .L3
{$endif not FPC_PIC}
.L1:
{$ifdef FPC_PIC}
    MOVL $0,test(%RIP)
{$else not FPC_PIC}
    MOVL $0,test
{$endif not FPC_PIC}
.L3:
  END;
  if (test<>0) or (test2<>$deadbeef) then
    halt(1);
  writeln('ok');
end.
