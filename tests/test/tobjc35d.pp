{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm,aarch64 }

{ Written by Jonas Maebe in 2010, released into the public domain }

{$mode objfpc}
{$modeswitch objectivec1}

uses
  uobjc35d, uobjc35e;

var
  a: MyExternalClass;
begin
  a:=MyExternalClass.alloc.init;
  if a.myTest<>1234 then
    halt(1);
  a.release;
end.
