{ %CPU=i386 }
{$asmmode intel}
const climbsize = 4;

procedure test;
var
  value : dword;
begin
  asm
    XOR EAX,EAX
    MOV EDI,1;
    LEA EAX,[EAX + EDI*climbsize]
    MOV value,EAX
  end;
  if value<>4 then
    halt(1);
end;

begin
  test;
  writeln('ok');
end.
