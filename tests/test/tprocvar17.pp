program tprocvar17;

{$mode delphi}

type C = class
    class procedure Foo;
end;
class procedure C.Foo; begin end;
type CC = class of C;
type H = class helper for C
    class procedure Bar;
end;
class procedure H.Bar; begin end;
type T = procedure of object;
type P = procedure;

const ViaClass: T = C.Foo;
var ViaMetaclass: T = CC.Foo;
var ViaHelperClass: T = C.Bar;
var ViaHelperMetaClass: T = CC.Bar;

procedure Check(aCode: TExitCode; const X: T; aAddr: CodePointer);
begin
    if (TMethod(X).Code <> aAddr) or (TMethod(X).Data <> Pointer(C)) then
      Halt(aCode);
end;

begin
    Check(1, ViaClass, @C.Foo);
    Check(2, ViaMetaclass, @C.Foo);
    Check(3, ViaHelperClass, @C.Bar);
    Check(4, ViaHelperMetaclass, @C.Bar);
end.
