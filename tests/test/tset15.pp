{ %FAIL }
{ delphiunicode mode keeps the limit of 256 set elements }
{$mode delphiunicode}
program tset15;

type
  t = set of 0..256;

begin
end.
