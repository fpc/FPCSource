{ %VERSION=1.1 }

{$ifdef fpc}{$mode objfpc}{$endif}

type
  rec = record
    ch : char;
  end;

  TBadObject = class
    a: array[0..0,0..0] of array[0..0] of rec;
  public
    property a0: char read a[0,0][0].ch;
  end;

var
  BadObject: TBadObject;
begin
  BadObject := TBadObject.Create;
  BadObject.a[0,0][0].ch := 'a';
  if BadObject.a0 = BadObject.a[0,0][0].ch then;
  BadObject.Free;
end.
