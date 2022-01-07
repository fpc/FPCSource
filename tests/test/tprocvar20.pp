{ %FAIL }

program tprocvar20;

{$mode delphi}

type C = class
end;
type CC = class of C;
type H = class helper for C
  class procedure Foo;
end;
class procedure H.Foo; begin end;
type T = procedure of object;

var ViaHelper: T = H.Foo;

begin
end.
