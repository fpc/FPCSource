program tgeneric18;

{$mode objfpc}{$H+}

type

  { TFirstGeneric }

  generic TFirstGeneric<T> = class(TObject)
  end;

  { TSecondGeneric }

  generic TSecondGeneric<T> = class(TObject)
  type public
    TFirstGenericType = specialize TFirstGeneric<T>;
  end;

var
  Second: specialize TSecondGeneric<string>;
begin
end.

