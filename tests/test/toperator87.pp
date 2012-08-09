{ %NORUN }

program toperator87;

{$mode delphi}

var
  p: Pointer;
  o: TObject;
  b: Boolean;
begin
  p := Nil;
  o := Nil;
  b := p <> o;
  b := o <> p;
end.
