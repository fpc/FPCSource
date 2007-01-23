{ %cpu=i386 }
{ %target=win32,linux,freebsd }

{$ifdef fpc}
{$mode delphi}
{$endif}

{ should not generate a stack frame }
function testje(l1,l2,l3: longint): longint;
asm
  mov eax, 30000
  ret
end;

procedure test;
var
  l1,l2,l3,l4,l5: cardinal;
begin
  l1 := $f00beef;
  l2 := $cafebabe;
  l3 := $c001d00d;
  l4 := $12345678;
  l5 := $90abcdef;
  if testje(1,2,3) <> 30000 then
    halt(1);
  if (l1 <> $f00beef) or
     (l2 <> $cafebabe) or
     (l3 <> $c001d00d) or
     (l4 <> $12345678) or
     (l5 <> $90abcdef) then
    halt(2);
end;

begin
  test;
end.
