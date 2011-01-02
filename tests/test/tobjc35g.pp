{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }
{ %fail }

{ Written by Jonas Maebe in 2010, released into the public domain }

{$mode objfpc}
{$modeswitch objectivec1}

uses
  uobjc35g;
type
  { external names must match }
  MyNSArray = objcclass external name 'NSArray2';

  ta = objcclass(MyNSArray)
  end;

begin
end.
