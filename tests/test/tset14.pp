{ %FAIL }
{ Delphi mode keeps the limit of 256 set elements }
{$mode delphi}
program tset14;

type
  t = set of 0..256;

begin
end.
