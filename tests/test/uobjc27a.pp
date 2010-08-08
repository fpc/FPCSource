{$mode objfpc}
{$modeswitch objectivec1}

{ Written by Jonas Maebe in 2009, released into the public domain }

unit uobjc27a;

interface

type
  ta = objcclass(NSObject)
  end;

type
  ca = objccategory(ta)
    function ca_categorymethod: longint; message 'ca_categorymethod';
  end;

implementation

uses
  uobjc27b;

function ca.ca_categorymethod: longint;
begin
  result:=da_categorymethod-1;
end;

end.
