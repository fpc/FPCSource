{ %CPU=i386 }
{ Source provided for Free Pascal Bug Report 2323 }
{ Submitted by "marco" on  2003-01-16 }
{ e-mail: marco@freepascal.org }

{$Mode Delphi}
{$ASMMODE Intel}

var
  buf : array[0..255] of char;
begin
  asm
    lea ebx,buf
    add ebx,'('
    mov al, [ebx - '(']
  end;
end.
