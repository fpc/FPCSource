{ %opt=-gw }
{ %norun }
unit tw40462;
{$mode objfpc}
{$scopedenums on}
interface
type
  TPart = (Foo, Bar);
  TFoo = class
  private type
    TPart = (Foo, Bar);
  private
    Part: TPart;
  end;
  
  TBar = class
  private type
    TPart = (Baz, Bad);
  private
    Part: TPart;
  end;

implementation

end.
