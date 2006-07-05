{ %fail }
{ %cpu=i386 }

{$ifdef fpc}
  {$mode delphi}
  {$asmmode intel}
{$endif}

type tscreen = class
      x : Cardinal;
      end;
type ttestobj = class
   screen : tscreen;
   constructor create;
   function testasmcall : tscreen;
end;

var
  testobj : ttestobj;

constructor ttestobj.create;
begin
  asm
  mov screen.x,0
  end;
end;

function ttestobj.testasmcall : tscreen;
begin
asm
mov screen.x, 0
ADD screen.x, 1
end;
result := screen;
end;


begin
testobj := ttestobj.create;
testobj.testasmcall;
testobj.destroy;
end.
