{$inline on}

var
  err : boolean;
  i : longint;

procedure p1(b:byte);inline;
begin
  case b of
    1 :
      begin
        writeln('1');
        i:=1;
      end;
    2 :
      begin
        if i<>1 then
          err:=true
        else
          writeln('2');
        i:=2;
      end;
  end;
end;

procedure p2(b:byte);inline;
begin
  case b of
    1 :
      p1(b);
    2 :
      p1(b);
  end;
end;

begin
  p2(1);
  p2(2);
  if err then
    halt(1);
end.
