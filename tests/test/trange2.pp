{$mode objfpc}
uses sysutils;
{$r+}

var
  l: longint;
  c: cardinal;
  n: longint;
begin
  n := 0;
  l := -1;
  try
    c := l;
  except
    writeln('caught 1!');
    inc(n);
  end;
  c := cardinal($ffffffff);
  try
    l := c;
  except
    writeln('caught 2!');
    inc(n);
  end;
  if n <> 2 then
    begin
      writeln('Still problems with range checking between longint/cardinal');
      halt(1);
    end;
end.
