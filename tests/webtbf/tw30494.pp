{ %FAIL }

program tw30494;

{$MODE DELPHI}

type
  TFoo<T: TObject> = record
  end;

var
  foo: TFoo<Integer>;
begin
end.

