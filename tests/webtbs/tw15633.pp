var value : word;
    code : Longint;

begin
  Val('4294901772', Value, code);
  writeln(code);
  if code<>6 then
    halt(1);
end.
