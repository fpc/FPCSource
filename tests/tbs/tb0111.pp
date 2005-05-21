{ Old file: tbs0130.pp }
{ in [..#255] problem                                   OK 0.99.6 (PFV) }

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
