program ifdemo;

var
  a : integer = 0;
 
begin
  if a=1 then
    Writeln('This should not be');
  if a=2 then
    Writeln('This should also not be')
  else
    Writeln('This should be OK');     
end. 