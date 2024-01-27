{$mode ObjFPC}{$H+}

generic function genericfunc<T>: String;

  function innerfunc: String;
  begin // project1.lpr(6,3) Error: Duplicate identifier "$result"
  end;

begin
end;

begin
end.
