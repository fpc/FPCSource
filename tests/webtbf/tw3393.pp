{ %fail }

{ Source provided for Free Pascal Bug Report 3393 }
{ Submitted by "David Emerson" on  2004-11-20 }
{ e-mail: demerson3p@angelbase.com }
var
  array_of_longint : array [0..5] of longint;
  longint_file : file of longint;

begin
  for i := 0 to 5 do
    write (longint_file, array_of_longint[i]);
end.
