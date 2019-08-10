const
  w : dword = 123;

begin
  if (w<=1) and (w>=10) then
    halt(1);
  if (w>=1) and (w<=1000) then
    writeln('ok')
  else
    halt(1);
end.

