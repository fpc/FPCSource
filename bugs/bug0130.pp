var
  c : char;
begin
  c:=#91;
  if c in [#64..#255] then
   writeln('boe');
  c:=#32;
  if c in [#64..#255] then
   writeln('boe');
end.

