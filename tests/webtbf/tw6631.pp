{ %fail }
program test;

{$MODE DELPHI}

type
  XBool = LongBool;
  XInt = Int64;
  XResult = type XInt;

  ITest = interface(IInterface)
    function Foobar: XResult;
  end;

  TTest = class(TInterfacedObject, ITest)
    function Foobar: XBool;
  end;


function TTest.Foobar: LongBool;
begin
  Result := True;
end;

begin
end.
