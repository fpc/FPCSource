{ Source provided for Free Pascal Bug Report 3529 }
{ Submitted by "Alexey Barkovoy" on  2005-01-07 }
{ e-mail: clootie@ixbt.com }
var
  v1, v2: Variant;
  S1, S2: AnsiString;
begin
  S1:= 'aa'; 
  S2:= 'bb';
  v1:= S1;
  v2:= S2;
  if v1 = v2 then 
    begin
      WriteLn('Equal');
      halt(1);
    end
  else 
    writeln('not-equal');
end.
