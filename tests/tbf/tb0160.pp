{%fail}
{%OPT=-Sg}
program tb0160;

procedure crasher;

begin
end;

begin
  {The following caused ie 9999.}
  crasher:
end.
