{ %CPU=i386 }
{$asmmode intel}

var
   a : array[0..5] of byte;

function f : byte;assembler;

  asm
    mov ebx,offset a
    mov ecx,0
    mov al,[ebx+4*ecx]
  end;

begin
   fillchar(a,5,255);
   a[0]:=0;
   if f<>0 then
     begin
        writeln('Scale factor problem in asmmode intel!');
        halt(1);
     end;
end.
