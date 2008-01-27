program speentte;

uses
  spe,
  typ;

var
  x: ArbFloat;
  h: string;
  t: ArbInt;
begin
  Write('program results speentte');

  case SizeOf(ArbFloat) of
    4: writeln('(single)');
    8: writeln('(double)');
    6: writeln('(real)');
  end;

  x := pi;
  Str(x, h);
  t := Length(h) - 1;

  writeln;
  writeln('': t div 2, 'x', '': t - 3, 'speent(x)');
  writeln;

  while not EOF do
  begin
    readln(x);
    writeln(x, speent(x): t div 2);
  end;

end.
program speentte;

uses
  spe,
  typ;

var
  x: ArbFloat;
  h: string;
  t: ArbInt;
begin
  Write('program results speentte');

  case SizeOf(ArbFloat) of
    4: writeln('(single)');
    8: writeln('(double)');
    6: writeln('(real)');
  end;

  x := pi;
  Str(x, h);
  t := Length(h) - 1;

  writeln;
  writeln('': t div 2, 'x', '': t - 3, 'speent(x)');
  writeln;

  while not EOF do
  begin
    readln(x);
    writeln(x, speent(x): t div 2);
  end;

end.
