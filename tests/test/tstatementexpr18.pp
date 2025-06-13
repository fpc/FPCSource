{$mode objfpc}
{$ModeSwitch StatementExpressions}
uses classes;

type
  ITest1 = interface ['{f97b7b1b-77c0-4c3c-800e-05bbd987493b}'] end;
  ITest2 = interface ['{207fbdba-6605-4168-bb21-fe0429ed55f2}'] end;
  TTest = class(TInterfacedObject, ITest1, ITest2) end;

var
  i: IUnknown;
begin
  i := if 0 < 1 then TTest.Create as ITest2 else TTest.Create as ITest1;
  WriteLn((i as TObject).classname);
  if (not (i is TTest)) then
    Halt(1);
end.
