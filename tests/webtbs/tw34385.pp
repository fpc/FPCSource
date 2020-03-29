program rangeTest;
const
  w : dword = 123;
  n : dword = 48;
begin
  if (w<=1) and (w>=10) then
    begin
      writeln('error 1-10');
      halt(1);
    end;
  if (w>=1) and (w<=1000) then
    writeln('ok')
  else
    begin
      writeln('error 1-1000');
      halt(2);
    end;
  if (n>44)and(n<48) then
    begin
      writeln('error 48');
      halt(3);
    end;
end.
