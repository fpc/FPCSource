// check whether enums and integers can be casted to object references; this 
// should work in Delphi mode (is Delphi compatible)
{$mode delphi}
{$packenum 2}
type
  TEnum = (a, b, c);
  
var
  i : Word;
  e : TEnum;
  o : TObject;

begin
  o := TObject(e);
  o := TObject(i);
  i := Word(o);
  e := TEnum(o);
end.
