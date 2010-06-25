{ Written by Jonas Maebe in 2010, released into the public domain }

{$mode objfpc}
{$modeswitch objectivec1}

unit uobjc35e;

interface

type
  MyExternalClass = objcclass(NSObject)
    function myTest: longint; message 'myTest';
  end;

implementation

  function MyExternalClass.myTest: longint;
    begin
      result:=1234;
    end;

end.
