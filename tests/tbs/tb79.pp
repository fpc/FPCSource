{ Old file: tbs0090.pp }
{  shows PChar comparison problem                        OK 0.99.7 (PFV) }

{$X+}
var
 mystr : array[0..4] of char;

Begin
  if mystr = #0#0#0#0 then
  Begin
  end;
  mystr:=#0#0#0#0;
end.
