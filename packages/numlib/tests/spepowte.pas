program spepowte;

uses
  typ,
  spe;

var
  x, y: ArbFloat;
  t:    ArbInt;
  s, h: string;

begin
  s := 'spepow';
  Write('program results ' + s + 'te ');
  case SizeOf(ArbFloat) of
    4: writeln('(single)');
    8: writeln('(double)');
    6: writeln('(real)');
  end;

  x := pi;
  Str(x, h);
  t := Length(h) - 1;

  while not EOF do
  begin
    readln(x);
    writeln;
    writeln('   x =  ', x);
    writeln('': t div 2, 'y', '': t - length(s) div 2, s + '(x, y)');
    while not eoln do
    begin
      Read(y);
      writeln(y, '  ', spepow(x, y));
    end;
    writeln;
  end;
end.
program spepowte;

uses
  typ,
  spe;

var
  x, y: ArbFloat;
  t:    ArbInt;
  s, h: string;

begin
  s := 'spepow';
  Write('program results ' + s + 'te ');
  case SizeOf(ArbFloat) of
    4: writeln('(single)');
    8: writeln('(double)');
    6: writeln('(real)');
  end;

  x := pi;
  Str(x, h);
  t := Length(h) - 1;

  while not EOF do
  begin
    readln(x);
    writeln;
    writeln('   x =  ', x);
    writeln('': t div 2, 'y', '': t - length(s) div 2, s + '(x, y)');
    while not eoln do
    begin
      Read(y);
      writeln(y, '  ', spepow(x, y));
    end;
    writeln;
  end;
end.
