{ %fail }

{$mode delphi}
program test;

type
  TFoo = class(TObject)
    constructor Create(Foo: Pointer);
  end;

constructor TFoo.Create(Foo: Pointer);
begin
end;

procedure shiny;
var
  p: pointer;
  M: TFoo.Create;
begin
end;

end.

