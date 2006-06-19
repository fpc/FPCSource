program bug;

{$inline on}
{$goto on}

procedure crasher;inline;

label beg;

begin
  goto beg;
beg:
end;

begin
  crasher;
end.
