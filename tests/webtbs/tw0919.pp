{ %cpu=i386 }

var i:integer;
{$asmmode intel}
{ "mov i,1"
   is like
  "mov word ptr [i],1"
  or
  movw i,$1  in ATT syntax }

begin
 asm
  mov i, 1
 end;
  if i <> 1 then
    halt(1);
end.
