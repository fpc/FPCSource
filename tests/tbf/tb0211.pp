{ %fail }

// check whether integers can NOT be casted to object references; this 
// should NOT work in objfpc mode (see also tbs/tb0554.pp)
{$mode objfpc}

var
  i : Word;
  o : TObject;

begin
  o := TObject(i);
end.
