{ %fail}
program terecs9;

{$mode delphi}

// don't allow refence owner type for record and object fields and properties
type
  TFoo = record
  var
    FFoo: TFoo;
  end;

  TBar = record
  var
    FBar: TBar;
  end;

begin
end.

