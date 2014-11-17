{ %cpu=i386,x86_64 }
{ %opt=-Cg- }
{$mode objfpc}
{$asmmode intel}


function bar: integer;
begin
  result:=$12345678;
end;

function foo: pointer; assembler; nostackframe;
asm
{$ifdef x86_64}
        lea  rax,[bar+rip]
{$else}
        lea  eax,[bar]
{$endif}
end;


begin
  if (foo<>pointer(@bar)) then
    halt(1);
end.

