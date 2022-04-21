{ %fail }
program Project1;
{$mode objfpc}{$H+}
type
  TRecursePtrA1 = ^TRecursePtrA2;
  TRecursePtrA2 = ^TRecursePtrA1;
var
 RecursePtrA1:  TRecursePtrA1;
 RecursePtrA2:  TRecursePtrA2;
begin
  RecursePtrA2 := RecursePtrA1;
end.
