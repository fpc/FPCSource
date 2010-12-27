program terecs8;

{$mode delphi}

// allow refence owner type for record and object static fields and class properties
type
  TFoo = record
  class var
    FFoo: TFoo;
  class property Foo: TFoo read FFoo write FFoo;
  end;

  TBar = record
  class var
    FBar: TBar;
  class property Bar: TBar read FBar write FBar;
  end;

begin
end.

