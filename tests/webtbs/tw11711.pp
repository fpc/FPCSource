uses sysutils;

var
  cs:String;
begin
  cs:=FormatFloat('000.000',-1);
  if (cs<>'-001'+DecimalSeparator+'000') then
    halt(1);
end.
