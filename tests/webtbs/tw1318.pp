{ %VERSION=1.1 }

{$mode objfpc}
type
  TBadObject = class
    a: array[0..0] of char;
  public
    property a0: char read a[0];
  end;

var
  BadObject: TBadObject;
begin
  BadObject := TBadObject.Create;
  BadObject.a[0] := 'a';
  if BadObject.a0 = BadObject.a[0] then;
  BadObject.Free;
end.
