{ %FAIL }

program tgenconstraint40;

{$mode objfpc}

type
  ISomeInterface = interface
  end;

  generic TGeneric<T: ISomeInterface> = class
  end;

  ITest = interface;

  TGenericITest = specialize TGeneric<ITest>;

  ITest = interface(ISomeInterface)
  end;

begin

end.
