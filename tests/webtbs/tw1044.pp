{ %NORUN }
{ DONT RUN THIS CODE, its creates an infinite recursion }
{ Code unchanged as this is a test for a compile time GPF. PM }
{ Source provided for Free Pascal Bug Report 1044 }
{ Submitted by "Geoffrey A Swift" on  2000-07-16 }
{ e-mail: blimey@toke.com }
{$mode objfpc}
type
  subrange = 1..6;
  subset = set of subrange;
function solve(numbers : subset) : boolean;
var
  i: subrange;
begin
  if numbers <> [] then
    for i := low(subrange) to high(subrange) do
      result := solve(numbers - [i])
end;
begin
  solve([1,2,3,4,5,6])
end.
