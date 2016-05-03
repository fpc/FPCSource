{ %cpu=i386 }
{ %opt=-Cg- }

{$ifdef fpc}
{$mode delphi}
{$endif}

program test;
type
  tr_rec=object
    x: integer;
    r: record
      a,b: integer;
    end;
    function f:integer;
  end;

var
  x1,a1,a2,b1: byte;
  b2: longint;

function tr_rec.f:integer;
asm
  mov dl,byte ptr [eax].tr_rec.x
  mov x1,dl
  mov dl,byte ptr [eax].tr_rec.r
  mov a1,dl
  mov dl,byte ptr [eax].tr_rec.r.a
  mov a2,dl
  mov dl,byte ptr [eax].tr_rec.r.b
  mov b1,dl
  movzx eax,byte ptr [eax].tr_rec.r.b
  mov b2,eax
  end;

var
  v: tr_rec;
begin
  v.x:=4;
  v.r.a:=10;
  v.r.b:=17;
  writeln(v.f,' (should be 17)');
  writeln(x1,' (should be 4)');
  writeln(a1,' (should be 10)');
  writeln(a2,' (should be 10)');
  writeln(b1,' (should be 17)');
  writeln(b2,' (should be 17)');
  if v.f<>17 then
    halt(1);
  if x1<>4 then
    halt(2);
  if a1<>10 then
    halt(3);
  if a2<>10 then
    halt(4);
  if b1<>17 then
    halt(5);
  if b2<>17 then
    halt(6);
end.
