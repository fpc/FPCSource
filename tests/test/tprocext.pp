uses uprocext1;

begin
  err:=true;
  proc1;
  if err then
    halt(1);
end.
