{ %fail }

{ Source provided for Free Pascal Bug Report 3275 }
{ Submitted by "Vincent Snijders" on  2004-08-27 }
{ e-mail: vslist@zonnet.nl }
program bug3275;

var
  b: boolean;
  i: integer;
  j: integer;

begin
  b := false;
  i := 4;
  j := 5;
  b := b or i <> j;
end.
