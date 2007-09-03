{ %fail }

type
  tobj = object b : byte; end;
begin
        writeln(tobj(65).b);
end.

