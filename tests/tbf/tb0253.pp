{ %FAIL }

program tb0253;

{$mode objfpc}

type
  TTest = class(TObject, IInterface)
    procedure Blubb.Bar = Foo;
  end;

begin

end.
