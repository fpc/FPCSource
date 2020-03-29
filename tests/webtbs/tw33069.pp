{ %OPT=-al }
program test;

{$mode objfpc}

type
  generic IIncrediblyLongInterfaceNaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaame<T> = interface
  end;

  generic TGenericImplementationOfLongInterfaceName<T> =
    class(TInterfacedObject,
          specialize IIncrediblyLongInterfaceNaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaame<T>)
  end;

  TIWillBreakYou = specialize TGenericImplementationOfLongInterfaceName<Integer>;

begin

end.
