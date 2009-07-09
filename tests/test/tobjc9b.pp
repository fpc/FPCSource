{ %target=darwin }
{ %cpu=powerpc,i386 }
{ %norun }

{$mode objfpc}
{$modeswitch objectivec1}

uses
  ctypes;

var
  a: NSObjectProtocol;
  b: NSObject;
begin
  a:=b;
end.
