{ Written by Jonas Maebe in 2010, released into the public domain }

{$mode objfpc}
{$modeswitch objectivec1}

unit uobjc35f;

interface

type
  MyExternalClass = objcclass external;

procedure test;

implementation

uses
  uobjc35e;

procedure test;
var
  a: MyExternalClass;
begin
  { the definition in uobjc35e should override the external definition
    from this unit }
  a:=MyExternalClass.alloc.init;
  if a.myTest<>1234 then
    halt(1);
  a.release;
end;

end.
