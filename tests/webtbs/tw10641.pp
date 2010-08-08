{ %cpu=x86_64 }
{$ifdef windows}
  {$imagebase $10000}
{$endif}
var
  test : dword;

begin
  test:=$deadbeef;
  ASM
    MOVL $0,%EAX
    JMP .LLT(%RAX)
    .align 16
.LLT:
    .quad .L1,.L2
.L2:
    MOVL $12341234,test
.L1:
    MOVL $0,test
  END;
  if test<>0 then
    halt(1);
  writeln('ok');
end.
