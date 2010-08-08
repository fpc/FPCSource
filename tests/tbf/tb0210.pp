{ %fail }

// check whether enums can NOT be casted to object references; this 
// should NOT work in objfpc mode (see also tbs/tb0554.pp)
{$mode objfpc}

{$packenum 2}
type
  TEnum = (a, b, c);
  
var
  e : TEnum;
  o : TObject;

begin
  o := TObject(e);
end.
