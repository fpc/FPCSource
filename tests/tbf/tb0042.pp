{ %FAIL }
{ Old file: tbf0208.pp }
{ implicit conversion from boolean to longint should not be allowed }

program tbf0208;

{ implicit boolean to integer conversion should not be
  allowed }
var
  b : boolean;
  i : longint;
begin
  b:=true;
  i:=b;
end.
