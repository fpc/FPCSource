{ %target=darwin}
{ %cpu=powerpc,powerpc64,i386,x86_64,arm,aarch64 }
{ %norun }

{ Written by Jonas Maebe in 2009, released into the public domain }

{$mode objfpc}
{$modeswitch objectivec1}

var
  a: NSObject;
  b: id;
begin
  a:=b;
  b:=a;
end.
