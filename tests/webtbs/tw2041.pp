{ %version=1.1}
{$mode objfpc}

type
  ITest = interface
    procedure Put(Key: Integer; Item: IUnknown);
    function Get(Key: Integer): IUnknown;
    property Item[Key: Integer]: IUnknown read Get write Put; default;
  end;

begin
end.
