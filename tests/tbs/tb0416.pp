function f: string;

  procedure t;
    begin
      f := 'test';
    end;

begin
  t;
end;


begin
  if f <> 'test' then
    begin
      writeln('error!');
      halt(1);
    end;
end.
