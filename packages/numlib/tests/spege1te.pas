program spege1te;

uses
  typ,
  spe;

const
  fspe: array[1..20] of rfunc1r = (@speach, @spearc, @spears, @speash, @speath, @spebi0, @spebi1, @spebj0, @spebj1, @spebk0, @spebk1, @speby0, @speby1, @specoh, @speefc, @speerf, @spegam, @spelga, @spesih, @spetah);

  fnames =
    'speachspearcspearsspeashspeathspebi0spebi1spebj0spebj1spebk0' +
    'spebk1speby0speby1specohspeefcspeerfspegamspelgaspesihspetah';

var
  x:    ArbFloat;
  t, u: ArbInt;
  s, h: string;
  f:    rfunc1r;

begin
  s := ParamStr(1);
  u := (Pos(s, fnames) + 5) div 6;
  if u = 0 then
  begin
    writeln(s, ' (commandlineparameter 1) bestaat niet in SPE');
    halt;
  end;

  f := fspe[u];

  Write('program results ' + s + 'te ');
  case SizeOf(ArbFloat) of
    4: writeln('(single)');
    8: writeln('(double)');
    6: writeln('(real)');
  end;

  x := pi;
  Str(x, h);
  t := Length(h) - 1;

  writeln;
  writeln('': t div 2, 'x', '': t - length(s) div 2, s + '(x)');
  writeln;

  while not EOF do
  begin
    readln(x);
    writeln(x, ' ': 2, f(x));
  end;

end.
program spege1te;

uses
  typ,
  spe;

const
  fspe: array[1..20] of rfunc1r = (@speach, @spearc, @spears, @speash, @speath, @spebi0, @spebi1, @spebj0, @spebj1, @spebk0, @spebk1, @speby0, @speby1, @specoh, @speefc, @speerf, @spegam, @spelga, @spesih, @spetah);

  fnames =
    'speachspearcspearsspeashspeathspebi0spebi1spebj0spebj1spebk0' +
    'spebk1speby0speby1specohspeefcspeerfspegamspelgaspesihspetah';

var
  x:    ArbFloat;
  t, u: ArbInt;
  s, h: string;
  f:    rfunc1r;

begin
  s := ParamStr(1);
  u := (Pos(s, fnames) + 5) div 6;
  if u = 0 then
  begin
    writeln(s, ' (commandlineparameter 1) bestaat niet in SPE');
    halt;
  end;

  f := fspe[u];

  Write('program results ' + s + 'te ');
  case SizeOf(ArbFloat) of
    4: writeln('(single)');
    8: writeln('(double)');
    6: writeln('(real)');
  end;

  x := pi;
  Str(x, h);
  t := Length(h) - 1;

  writeln;
  writeln('': t div 2, 'x', '': t - length(s) div 2, s + '(x)');
  writeln;

  while not EOF do
  begin
    readln(x);
    writeln(x, ' ': 2, f(x));
  end;

end.
