{ %NORUN }

{ additional test based on 21064 }
program tgeneric79;

{$mode objfpc}

type
  generic IGenericIntf<T> = interface
    function SomeMethod: T;
  end;

  generic TGenericClass<T> = class(TInterfacedObject, specialize IGenericIntf<LongInt>)
  private
  protected
    function GenericIntf_SomeMethod: LongInt;
    function IGenericIntf<LongInt>.SomeMethod = GenericIntf_SomeMethod;
  end;

function TGenericClass.GenericIntf_SomeMethod: LongInt;
begin
end;

type
  TGenericClassLongInt = specialize TGenericClass<String>;
begin
end.
