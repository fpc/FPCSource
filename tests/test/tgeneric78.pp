{ %NORUN }

{ additional test based on 21064 }
program tgeneric78;

{$mode delphi}

type
  IGenericIntf<T> = interface
    function SomeMethod: T;
  end;

  TGenericClass<T> = class(TInterfacedObject, IGenericIntf<LongInt>)
  private
  protected
    function GenericIntf_SomeMethod: LongInt;
    function IGenericIntf<LongInt>.SomeMethod = GenericIntf_SomeMethod;
  end;

function TGenericClass<T>.GenericIntf_SomeMethod: LongInt;
begin
end;

type
  TGenericClassLongInt = TGenericClass<String>;
begin
end.
