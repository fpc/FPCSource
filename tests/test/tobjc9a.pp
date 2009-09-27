{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }
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
