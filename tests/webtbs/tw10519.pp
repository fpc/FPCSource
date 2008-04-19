uses sysutils;

var
  cs:String;
begin
  cs:=FormatFloat('0.00000E+0000',52247.9532745);
  if (cs<>'5'+DecimalSeparator+'22480E+0004') then
    halt(1);
end.
