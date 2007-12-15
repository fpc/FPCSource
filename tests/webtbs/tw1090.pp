{ %CPU=i386 }
{ %OPT=-Cg- }
{$asmmode intel}
const
  Number = $7FFFFFF;
  Shift  = 7;
var
  l : longint;
begin
  ASM
        MOV EAX,(Number shr (Shift+3))
        mov l,eax
  End;
  if l<>131071 then
   begin
     writeln('error in constant eval in intel reader');
     halt(1);
   end;
end.
