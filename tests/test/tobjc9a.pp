{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm,aarch64 }
{ %fail }

{ Written by Jonas Maebe in 2009, released into the public domain }

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
