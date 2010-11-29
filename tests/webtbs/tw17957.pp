{$mode objfpc}
uses fgl;

type
  TFoo1 = class(TObject);

  TFoo2 = class
    type
      TFooList = specialize TFPGList<TFoo1>;
  end;

begin
end.
