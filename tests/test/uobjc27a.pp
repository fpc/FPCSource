{$mode objfpc}
{$modeswitch objectivec1}

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
