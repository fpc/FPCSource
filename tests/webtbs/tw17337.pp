{ %target=win64, linux, freebsd }
{ %cpu=x86_64 }
{ %opt=-Aas }

{$asmmode att}

procedure test_gas;
var
  test : qword;
begin
  test:=$5ffffffff;
  if (test < qword($2ffffffff)) then
    runerror(1);
  if (test < qword($ffffffff)) then
    runerror(2);
asm
  movq $0xffffffff,%rax
  movq %rax,test
end;
  if test <> $ffffffff then
    runerror(5);
end ;

var
  test : qword;
begin
  test:=$5ffffffff;
  if (test < qword($2ffffffff)) then
    runerror(3);
  if (test < qword($ffffffff)) then
    runerror(4);
  test_gas;
end.
