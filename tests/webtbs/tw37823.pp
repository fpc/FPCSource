{$MODE ISO}
{$implicitExceptions off}
{$Q+}
{$R+}
program gt;
  label 1;
  procedure jump;
  var
    a: integer;
    b: rawbytestring;
  begin
    b := 'nanu';
    writeln('nanu');
    goto 1;
  end;
begin
  jump;
  writeln('not jumped!');
1:
writeln('jumped!');
end.
