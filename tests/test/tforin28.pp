program tforin28;

type
  TEnum = (One, Two, Three);

var
  e: TEnum;
begin
  for e in [One, Three] do Writeln(e);
end.
