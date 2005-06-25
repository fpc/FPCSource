{ Source provided for Free Pascal Bug Report 4115 }
{ Submitted by "Alexey Chernobayev" on  2005-06-25 }
{ e-mail: alexch@caravan.ru }
var
  A: array [WideChar] of Char;
  W: WideChar;
begin
  A['a'] := 'b'; // Ok
  W := 'c';

  A[W] := 'd';
end.
