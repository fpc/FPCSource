program spemaxte;

uses
  typ,
  spe;

var
  x, y: ArbFloat;
  h:    string;
  t:    ArbInt;
begin
  Write('program results spemaxte');

  case SizeOf(ArbFloat) of
    4: writeln('(single)');
    8: writeln('(double)');
    6: writeln('(real)');
  end;

  x := pi;
  Str(x, h);
  t := Length(h) - 1;

  writeln;
  writeln('': t div 2, 'x', '': t, 'y', '': t - 2, 'spemax(x, y)');
  writeln;

  while not EOF do
  begin
    readln(x, y);
    writeln(x, ' ': 2, y, ' ': 2, spemax(x, y));
  end;

end.
program spemaxte;

uses
  typ,
  spe;

var
  x, y: ArbFloat;
  h:    string;
  t:    ArbInt;
begin
  Write('program results spemaxte');

  case SizeOf(ArbFloat) of
    4: writeln('(single)');
    8: writeln('(double)');
    6: writeln('(real)');
  end;

  x := pi;
  Str(x, h);
  t := Length(h) - 1;

  writeln;
  writeln('': t div 2, 'x', '': t, 'y', '': t - 2, 'spemax(x, y)');
  writeln;

  while not EOF do
  begin
    readln(x, y);
    writeln(x, ' ': 2, y, ' ': 2, spemax(x, y));
  end;

end.
