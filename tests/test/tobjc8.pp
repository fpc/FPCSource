{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }
{ %opt=-vh -Seh }
{ %fail }

{$mode objfpc}
{$modeswitch objectivec1}

uses
  ctypes;

type
  TMyTestClass = objcclass(NSObject)
  end;

  TMyTestClass2 = objcclass(NSObject)
    { should give a hint because of a missing 'override' }
    function hash: cuint;
  end; external name 'TMyTestClass';

var
  a: id;
  b: tmytestclass2;
begin
  b:=nil;
  if assigned(b) then
    ;
  { avoid warnings/hints about unused types/variables }
  a:=TMyTestClass.alloc;
  tmytestclass(a).Retain;
end.
