{ Source provided for Free Pascal Bug Report 2897 }
{ Submitted by "C Western" on  2004-01-17 }
{ e-mail: mftq75@dsl.pipex.com }
program stackerr;

{$S+}

procedure Show(v: Integer);
begin
  WriteLn(v);
  if v<>27 then
    begin
      writeln('Error!');
      halt(1);
    end;
end;

begin
  Show(27)
end.
