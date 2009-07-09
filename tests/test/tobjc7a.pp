{ %target=darwin }
{ %cpu=powerpc,i386 }
{ %fail }


{$mode objfpc}
{$modeswitch objectivec1}

uses
  uobjc7;

type
  tobjcclass = objcclass(tobjcprot)
    procedure alsorequired;
{ fake external name to avoid linking errors once we
  add external references in all cases to ensure that
  all necessary libraries are linked, like gcc does }
  end; external name 'NSObject'; 


begin
end.
