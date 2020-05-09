{$mode objfpc}

procedure outer;

  procedure nest2(l: longint); forward;

  function nest(l: longint): longint;
    begin
      if l>1 then
        result:=nest(l-1)+nest(l-2)
      else
        begin
          result:=1;
          nest2(result);
        end;
    end;

  procedure nest2(l: longint);
    begin
      writeln(l);
    end;

begin
  if nest(3) <> 3 then
    halt(1);
  nest2(4);
end;

begin
  outer;
end.
