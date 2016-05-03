{ %CPU=i386 }
{ %OPT=-Cg- }


{$asmmode intel}

var
  i,j  : longint;

begin
  i:=56;
  { this should work as ss and ds have the same selector value }
  asm
    segss
    mov eax,dword ptr [i]
    mov dword ptr [j],eax
  end;
  if i<>j then
    Halt(1);
end.
