{ %CPU=aarch64 }
program project1;
uses crt;
procedure test;
var a:uint64;
begin
a:=1;
    asm
    mov x4,# 0
    mov x12,# 1
    add x4,x4,x12,lsl # 2
    str x4, a
    end;

  writeln(a);
  if a<>4 then
     halt(1);
end;

begin
test;
end.
