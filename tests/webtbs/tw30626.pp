{ %NORUN }

program tw30626;

{$mode objfpc}

type
  generic IBase<T> = interface(IUnknown)
  end;

  generic TBase<T> = class(TInterfacedObject, specialize IBase<T>)
  public
    function Test: specialize IBase<T>;
  end;

  generic TDerived<T> = class(specialize TBase<T>)

  end;

function TBase.Test: specialize IBase<T>;
begin
  result := (specialize TDerived<T>).Create;
end;

type
  TIntDerived = specialize TDerived<Integer>;

var
  t: TIntDerived;

begin
  t := TIntDerived.Create;
  t.Test;
end.

