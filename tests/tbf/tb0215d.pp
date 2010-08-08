{ %FAIL }
// it's not allowed to assign a corbainterface to TGuid

{$mode objfpc}

{$INTERFACES COM}
type
  ITest = interface
    ['{0374223C-E460-4CC2-ABFD-7723AAB80CFE}']
  end;

procedure foo(iid: shortstring);
begin
end;

begin
  foo(ITest);
end.

