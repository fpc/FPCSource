{$mode iso}
{$inline on}
procedure test;
  label
    1;
  procedure p;
    begin
      goto 1;
      halt(1);
    end;
  begin
    p;
    halt(1);
  1:
  end;


label
  1;

procedure p;inline;
  begin
    goto 1;
    halt(1);
  end;

begin
  test;
  p;
  halt(1);
1:
  writeln('ok');
end.

