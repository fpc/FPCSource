{ %cpu=x86_64 }
{$asmmode intel}
procedure test;
var
  TestVar : Int64;
begin
  TestVar:=1234123412341234;
  asm
    MOV RAX,0
    LEA RBX,TestVar
    MOV QWORD [RBX],RAX
  end;
  writeln(TestVar);
  if TestVar<>0 then
    halt(1);
end;

begin
  test;
  writeln('ok');
end.

