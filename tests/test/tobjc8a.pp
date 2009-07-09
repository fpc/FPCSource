{ %target=darwin }
{ %cpu=powerpc,i386 }
{ %opt=-vh -Seh }
{ %norun }

{$mode objfpc}
{$modeswitch objectivec1}

uses
  ctypes;

type
  TMyTestClass = objcclass(NSObject)
    { should not give a hint, since we have 'override' }
    function hash: cuint; override;
  end; external name 'NSZone';

var
  a: id;
begin
  { avoid warnings/hints about unused types/variables }
  a:=TMyTestClass.alloc;
  tmytestclass(a).Retain;
end.
