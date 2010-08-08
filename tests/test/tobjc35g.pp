{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }
{ %fail }

{ Written by Jonas Maebe in 2010, released into the public domain }

{$mode objfpc}
{$modeswitch objectivec1}

type
  { formal class with external name is not allowed }
  NSArray = objcclass; external name 'NSArray';

begin
end.
