program spesgnte;

uses
  spe,
  typ;

var
  x: ArbFloat;
  h: string;
  t: ArbInt;
begin
  Write('program results spesgnte');

  case SizeOf(ArbFloat) of
    4: writeln('(single)');
    8: writeln('(double)');
    6: writeln('(real)');
  end;

  x := pi;
  Str(x, h);
  t := Length(h) - 1;

  writeln;
  writeln('': t div 2, 'x', '': t - 3, 'spesgn(x)');
  writeln;

  while not EOF do
  begin
    readln(x);
    writeln(x, ' ': t div 2, spesgn(x));
  end;

end.
program spesgnte;

uses
  spe,
  typ;

var
  x: ArbFloat;
  h: string;
  t: ArbInt;
begin
  Write('program results spesgnte');

  case SizeOf(ArbFloat) of
    4: writeln('(single)');
    8: writeln('(double)');
    6: writeln('(real)');
  end;

  x := pi;
  Str(x, h);
  t := Length(h) - 1;

  writeln;
  writeln('': t div 2, 'x', '': t - 3, 'spesgn(x)');
  writeln;

  while not EOF do
  begin
    readln(x);
    writeln(x, ' ': t div 2, spesgn(x));
  end;

end.
