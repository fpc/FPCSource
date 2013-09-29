{ %cpu=x86_64 }
{ %opt=-Aas }

{$asmmode intel}

procedure test;
var
  i1,i2: int64;
  b: shortint;
begin
  b:=-128;
  asm
    movsx rax, b
    mov  i1,rax
    movzx rax, b
    mov  i2,rax
  end ['rax'];
  if i1<>-128 then
    halt(1);
  if i2<>byte(-128) then
    halt(2);
end;

begin
  test
end.
    
  

