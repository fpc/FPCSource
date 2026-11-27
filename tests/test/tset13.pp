{ %FAIL }
{ TP mode keeps the limit of 256 set elements }
{$mode tp}
program tset13;

type
  t = set of 0..256;

begin
end.
