{ Old file: tbs0063.pp }
{  shows problem with ranges in sets for variables      OK 0.99.7 (PFV) }

{ may also crash/do weird error messages with the compiler }
var
 min: char;
 max: char;
 i: char;
begin
 min:='c';
 max:='z';
 if i in [min..max] then
 Begin
 end;
end.
