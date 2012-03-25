{ %FAIL }

{ unspecialized generics are not allowed for default - case 2 }
program tdefault12;

{$mode objfpc}

type
  generic TTest<T> = class

  end;

var
  t: TObject;
begin
  t := Default(TTest);
end.
