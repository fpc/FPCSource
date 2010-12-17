{ %fail}
{ %norun}
program terecs1;

{$mode delphi}
{$apptype console}

uses
  terecs_u1;

var
  F: TFoo;
begin
  // we can't access private fields
  F.F1 := 0;
  F.F2 := 1;
end.

