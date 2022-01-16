{$mode iso}
procedure test;
  label
    1;
  procedure p;
    label
      2;
    procedure pp;
      begin
        goto 2;
        halt(1);
      end;

    begin
      pp;
      halt(1);
    2:
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

procedure p;
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

