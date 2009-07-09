{ %target=darwin }
{ %cpu=powerpc,i386 }
{ %fail }

{$mode objfpc}
{$modeswitch objectivec1}

uses
  ctypes;

var
  a: NSObjectProtocol;
  b: NSObject;
begin
  b:=a;
end.
