{ %CPU=i386 }
{ %fail }

{$asmmode intel}
procedure test; assembler;
asm
     push  eax ebx
     pop   ebx eax
end;

begin
end.
