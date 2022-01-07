{ %FAIL }

program tprocvar18;

{$mode delphi}

type C = class
    procedure Foo;
end;
procedure C.Foo; begin end;
type T = procedure of object;

var aC: C = nil;
// Still rejected:
var ViaInstance: T = aC.Foo;

begin
end.
