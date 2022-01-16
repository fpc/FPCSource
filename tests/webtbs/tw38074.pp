{ %cpu=x86_64 }
{ %opt=Anasm}
{$ASMMODE INTEL}
const
  value : int64 =$1234567898765432;

function Test:Int64;assembler;
asm
  mov RAX, $1234567898765432
end;

begin
  if Test<>value then
    halt(1);
  writeln('ok');
end.
