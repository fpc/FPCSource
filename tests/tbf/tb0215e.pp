{ %FAIL }
// it's not allowed to assign a corbainterface to TGuid

{$mode objfpc}

{$INTERFACES COM}

procedure test(iid: tguid);
begin
end;

begin
  test('{0374223C-E460-4CC2-ABFD-7723AAB80CFE}');
end.
