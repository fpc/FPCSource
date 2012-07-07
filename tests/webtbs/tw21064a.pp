{ %NORUN }

program tw21064a;

{$mode delphi}

type
  IGenericIntf<T> = interface
    function SomeMethod: T;
  end;

  TGenericClass<T> = class(TInterfacedObject, IGenericIntf<T>)
  private
  protected
    function GenericIntf_SomeMethod: T;
    function IGenericIntf<T>.SomeMethod = GenericIntf_SomeMethod;
  end;

function TGenericClass<T>.GenericIntf_SomeMethod: T;
begin
end;

type
  TGenericClassLongInt = TGenericClass<LongInt>;
begin
end.
