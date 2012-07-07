{ %NORUN }

program tw21064b;

{$mode delphi}

type
  IGenericIntf<T> = interface
    function SomeMethod: T;
  end;

  TGenericClass<T> = class(TInterfacedObject, IGenericIntf<T>)
  private
    type
      IntfType = IGenericIntf<T>;
  protected
    function GenericIntf_SomeMethod: T;
    function IntfType.SomeMethod = GenericIntf_SomeMethod;
  end;

function TGenericClass<T>.GenericIntf_SomeMethod: T;
begin
end;

type
  TGenericClassLongInt = TGenericClass<LongInt>;
begin
end.
