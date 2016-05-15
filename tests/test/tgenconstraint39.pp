{ %FAIL }

program tgenconstraint39;

{$mode objfpc}

type
  TSomeClass = class
  end;

  generic TGeneric<T: TSomeClass> = class
  end;

  TTest = class;

  TGenericTTest = specialize TGeneric<TTest>;

  TTest = class(TSomeClass)
  end;

begin

end.
