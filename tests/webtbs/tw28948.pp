{ %OPT=-gt }

function f: ansistring;
begin
end;

begin
  writeln(f);
  if pos('uninitialized function result',f)=0 then
    halt(1);
end.
