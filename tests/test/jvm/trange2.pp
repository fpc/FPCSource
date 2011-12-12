program trange2;

{$mode objfpc}
{$ifdef cpujvm}
uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

{$macro on}
{$define writeln:=jlsystem.fout.println}
{$define write:=jlsystem.fout.println}
{$else}
uses
  SysUtils;
{$endif}

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
