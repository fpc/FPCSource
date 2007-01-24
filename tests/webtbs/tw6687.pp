{ %cpu=i386 }
{ %target=win32,linux,freebsd }

{$ifdef fpc}
{$mode delphi}
{$endif}

{$w+}

{ should not generate a stack frame in spite of w+ above }
function testje(l1,l2,l3: longint): longint;
asm
  mov eax, 30000
  ret
end;

function test: longint;
var
  l1,l2,l3,l4,l5: cardinal;
begin
  test := 12345;
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
  if test <> 12345 then
    halt(3);
end.
