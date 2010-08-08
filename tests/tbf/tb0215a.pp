{ %FAIL }
// it's not allowed to assign a corbainterface to TGuid

{$mode objfpc}

{$INTERFACES CORBA}
type
  ITest = interface
    ['{0374223C-E460-4CC2-ABFD-7723AAB80CFE}']
  end;

const
  Test_IID: TGuid = ITest;

begin
end.
