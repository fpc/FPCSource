{ %FAIL }

{ unspecialized generics are not allowed for default - case 1 }
program tdefault11;

{$mode delphi}

type
  TTest<T> = class

  end;

var
  t: TObject;
begin
  t := Default(TTest);
end.
