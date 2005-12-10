{$inline on}
{$mode objfpc}

type
  tc = class
    lf: longint;
    procedure t(l: longint); inline;
  end;

var
  a: longint;

procedure tc.t(l: longint); inline;
begin
  lf := 10;
  if (l <> 5) then
    begin
      writeln('error class');
      halt(1);
    end;
end;


procedure t(l: longint); inline;
begin
  a := 10;
  if (l <> 5) then
    begin
      writeln('error proc');
      halt(1);
    end;
end;

var
  c: tc;

begin
  c := tc.create;
  c.lf := 5;
  c.t(c.lf);
  a := 5;
  t(a);
end.
