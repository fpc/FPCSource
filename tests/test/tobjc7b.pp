{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }
{ %fail }

{ Written by Jonas Maebe in 2009, released into the public domain }

{$mode objfpc}
{$modeswitch objectivec1}

uses
  uobjc7;

type
  tobjcclass = objcclass(tobjcprot)
    procedure isrequired;
{ fake external name to avoid linking errors once we
  add external references in all cases to ensure that
  all necessary libraries are linked, like gcc does }
  end; external name 'NSObject'; 


begin
end.
