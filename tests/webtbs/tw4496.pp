type
  //BoolDeriv = Boolean; //gives no internal error
  BoolDeriv = false..true;

var
  a: array[BoolDeriv] of char;

begin
  a[true] := 'a'; //ierror 99080501 here
end.
