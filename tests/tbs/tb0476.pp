const
  e = 'as';

procedure p(const p);
  begin
    if pchar(@p)^<>'a' then
      begin
        writeln('error');
        halt(1);
      end;
  end;

begin
  p(e[1]);
end.
