{$coperators+}
{$modeswitch arrayoperators}
 
var
  bools: array of boolean;
  s: string;
 
begin
  bools := [];
  s := 'x';
  bools += [(s <> '') and (s[1] = 'x')]; // project1.lpr(9,40) Error: Internal error 2011010304
  if not(bools[0]) then
    halt(1);
end.
