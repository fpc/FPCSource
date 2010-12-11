{ %fail}
{ %norun}
program terecs4;

{$mode delphi}

type
  TFoo = record
    destructor Destroy; // not allowed
  end;

destructor TFoo.Destroy;
begin
end;

begin
end.
