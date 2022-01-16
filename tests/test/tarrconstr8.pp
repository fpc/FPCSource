{ %FAIL }

program tarrconstr8;

type
  TLongIntArray = array of LongInt;

var
  arr: TLongIntArray;
begin
  // Create *must* be used on a type
  arr := arr.Create(1, 2);
end.
