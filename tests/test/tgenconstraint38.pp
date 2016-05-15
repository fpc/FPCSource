{ %FAIL }

program tgenconstraint38;

{$mode objfpc}

type
  generic TGeneric<T: TObject, IInterface> = class
  end;

  TTest = class;

  TGenericTTest = specialize TGeneric<TTest>;

  TTest = class
  end;

begin

end.
