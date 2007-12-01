{ %cpu=i386 }
{ %OPT=-Cg- }

{$ifdef fpc}{$asmmode intel}{$endif}

Type
float=single;
var
  f : float;
begin
  f:=4.0;
asm
  lea eax,f
fld SizeOf(float) ptr [eax]
fsqrt
fstp SizeOf(float) ptr [eax]
end;
  writeln(f);
  if trunc(f)<>2 then
    halt(1);
end.

