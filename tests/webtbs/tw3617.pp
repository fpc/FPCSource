{ %cpu=i386 }

{ Source provided for Free Pascal Bug Report 3617 }
{ Submitted by "Thomas Schatzl" on  2005-01-31 }
{ e-mail:  }

{$ifdef fpc}{$asmmode intel}{$endif}

function Div64by32( var X : Int64; Y : Longint ) : Longint
; assembler;
asm
  mov ecx, edx
  mov edx, [eax+4].longint
  mov eax, [ eax ].dword
  idiv ecx
end;

begin
end.
