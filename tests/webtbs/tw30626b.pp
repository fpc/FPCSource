{ %NORUN }

program tw30626b;

{$mode objfpc}

type
  generic IBase<T> = interface(IUnknown)
    function Test: specialize IBase<T>;
  end;

  generic TBase<T> = class(TInterfacedObject, specialize IBase<T>)
  public
    function Test: specialize IBase<T>; virtual;
  end;

  generic TDerived<T> = class(specialize TBase<T>)
  public
    function Test: specialize IBase<T>; override;
  end;

function TBase.Test: specialize IBase<T>;
begin
 result := specialize TDerived<T>.Create;
end;

function TDerived.Test: specialize IBase<T>;
begin
 result := specialize TDerived<T>.Create;
end;

type
  IIntegerBase = specialize IBase<Integer>;

var
  Intf, Intf2: IIntegerBase;
begin
  Intf:= specialize TDerived<Integer>.Create;
  Intf2:= Intf.Test;
end.


