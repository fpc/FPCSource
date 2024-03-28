{ %opt=-MDelphi }
program SetTest;
{$mode objfpc}
type
  TEnum = (one, two);
  TSet = set of TEnum;
begin
  if SizeOf(TSet)<>4 then
    halt(1);
end.
