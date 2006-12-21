{$inline on}
{$mode objfpc}
uses tw7975;

var
  c: tc;
begin
  c := tc.create;
  writeln(test(c));
  c.free;
end.
