// check whether enums and integers can be casted to object references; this 
// should work in Delphi mode (is Delphi compatible)
{$mode delphi}

type
  TEnum = (a, b, c);
  
var
  i : Integer;
  e : TEnum;
  o : TObject;

begin
  o := TObject(e);
  o := TObject(i);
end.
