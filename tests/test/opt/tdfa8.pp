{ %OPT=-Oodfa -Sew -vw }
{ %fail }
{ %norun }
unit tdfa8;

interface

implementation

{ this test will work only as soon as we have global dfa,
  so this will not be forgotten to be handled properly }
var
  i : longint;

initialization
finalization
  writeln(i);
end.
