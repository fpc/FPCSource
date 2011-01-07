{ Written by Jonas Maebe in 2010, released into the public domain }

{$mode objfpc}
{$modeswitch objectivec1}

unit uobjc35d;

interface

type
  MyExternalClass = objcclass external;

  MyClass = objcclass(NSObject)
    ff: MyExternalClass
  end;

implementation

end.
