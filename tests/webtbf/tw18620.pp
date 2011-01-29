{ %fail }
program tw18620;
{$mode delphi}

type
  { in mode delphi compiler should not create a forward definistion for ^_TFoo for later resolve
    instead it must search _TFoo amoung already defined symbols }
  TFoo = record
    Foo: ^_TFoo;
  end;
  _TFoo = TFoo;

begin
end.