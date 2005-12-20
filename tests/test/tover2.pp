procedure pr(d:single);overload;
begin
  writeln('single');
end;

procedure pr(d:double);overload;
begin
  writeln('double');
end;

procedure pr(d:currency);overload;
begin
  writeln('currency');
end;

begin
  pr(1.0);
end.
