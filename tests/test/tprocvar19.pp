{ %FAIL }

program tprocvar19;

{$mode delphi}

type C = class
    class procedure Foo;
end;
class procedure C.Foo; begin end;
type CC = class of C;
type T = procedure of object;

var aCC: CC = nil;
// Still rejected:
var ViaClassRef: T = aCC.Foo;

begin
end.
