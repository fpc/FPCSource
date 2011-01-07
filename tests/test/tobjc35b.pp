{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }

{ Written by Jonas Maebe in 2010, released into the public domain }

{$mode objfpc}
{$modeswitch objectivec1}

type
  NSArray = objcclass external;

var
  a: NSArray;
begin
end.
