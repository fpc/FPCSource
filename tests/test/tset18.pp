{ %FAIL }
{ %OPT=-Sew }
{ TP mode: constant set elements >255 are still truncated with a warning }
{$mode tp}
program tset18;

var
  w: word;
begin
  w:=300;
  if w in [300] then
    halt(1);
end.
