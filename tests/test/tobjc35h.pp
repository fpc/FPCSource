{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm,aarch64 }
{ %norun }

{ Written by Jonas Maebe in 2010, released into the public domain }

{$mode objfpc}
{$modeswitch objectivec1}

uses
  uobjc35e,uobjc35d,uobjc35f;

var
  a: uobjc35d.MyExternalClass;
  b: uobjc35f.MyExternalClass;
begin
  a:=nil;
  a:=b;
end.
