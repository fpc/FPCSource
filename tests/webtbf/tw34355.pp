{ %fail }

program tw34355;
{$mode objfpc}
procedure AssignArray(const aArray: array of string);
var
  myArray: array of string;
  S: string;
begin
  myArray := [aArray]; // << no compiler error, yet no assignment is executed
  Writeln('Length(myArray): ', Length(myArray));
  for S in myArray do
    Writeln(S);
end;
begin
  AssignArray(['abc', 'xyz']);
end.
