{ %fail }
program Project1;
{$mode objfpc}{$H+}{$Interfaces CORBA}

type
  ITest = interface
  end;

  TBar = class;

  TFoo = class(TObject, ITest)
    Fa: TBar;
    property a: TBar read Fa implements ITest;
  end;

  TBar = class(TObject, ITest)
  end;

begin
end.
