{ %CPU=i386 }

{$asmmode att}
var
  test: array[0..2] of longint;

function proc: longint; assembler;
asm
     call   .L1
.L1:
     pop    %eax
     movl   test-.L1(%eax),%eax
// This should also work (but it doesn't due to bugs in asmreader):
//   movl   test-.L1+8(%eax),%eax     
end;

begin
  test[0]:=5555;
  test[1]:=6666;
  test[2]:=7777;
  if proc<>5555 then
    Halt(1);
  Halt(0);  
end.