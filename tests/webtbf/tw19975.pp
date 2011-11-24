{ %FAIL }

program uGenHelper;

{$mode objfpc}{$H+}

type
  generic TGeneric<T> = class
  end;

  THelper = class helper for TGeneric
  end;

begin
end.
