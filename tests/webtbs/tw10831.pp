program test;

type
  TForm = class(TInterfacedObject)
    a : array[0..1000000] of byte;
  end;

  TMyForm = class(TForm, IInterface)
  end;

var
  i : IInterface;
begin
  i:=TMyForm.Create;
end.
