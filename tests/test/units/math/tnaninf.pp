uses
  Math;

begin
  if not(isnan(nan)) then
    begin
      writeln('error 1');
      halt(1);
    end;
  if not(isinfinite(infinity)) then
    begin
      writeln('error 2');
      halt(1);
    end;
  if isnan(12341234) then
    begin
      writeln('error 3');
      halt(1);
    end;
  if isinfinite(0) then
    begin
      writeln('error 4');
      halt(1);
    end;
  if isinfinite(12341234) then
    begin
      writeln('error 5');
      halt(1);
    end;
end.
