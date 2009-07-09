{ %target=darwin }
{ %cpu=powerpc,i386 }
{ %opt=-vh -Seh }
{ %fail }

{$mode objfpc}
{$modeswitch objectivec1}

uses
  ctypes;

type
  TMyTestClass = objcclass(NSObject)
    { should give a hint because of a missing 'override' }
    function hash: cuint;
  end; external name 'NSZone';

var
  a: id;
begin
  { avoid warnings/hints about unused types/variables }
  a:=TMyTestClass.alloc;
  tmytestclass(a).Retain;
end.
