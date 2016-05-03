program TurTe;

uses
  typ;

  function Tweelog(x: real): integer;
  var
    i: integer;
  begin
    i := 0;
    if x > 1 then
      Tweelog := -Tweelog(1 / x)
    else
    begin
      while x < 1 do
      begin
        Dec(i);
        x := 2 * x;
      end;
      if x <> 1 then
        Tweelog := 0
      else
        Tweelog := i;
    end;
  end;

var
  x: real;

begin

  { Test op macheps }
  Writeln('Macheps = 2', Tweelog(macheps));
  Writeln('Hoe wordt er afgerond?');
  x := 1 + macheps;
  Writeln('(1 + macheps     ) - 1 = ', x - 1);
  x := 1 + 0.75 * macheps;
  Writeln('(1 + 0.75*macheps) - 1 = ', x - 1);
  x := 1 + 0.5 * macheps;
  Writeln('(1 + 0.5*macheps ) - 1 = ', x - 1);
  x := 1 + 0.25 * macheps;
  Writeln('(1 + 0.25*macheps) - 1 = ', x - 1);

  {test op giant }
  writeln(' giant = ', giant);
  x := giant / 2;
  writeln(' resultaat van x:= giant / 2 =', x);
  {test op midget}
  writeln(' midget = ', midget);
  x := midget / 2;
  writeln(' resultaat van x:= midget/2 =', x);
  x := midget / 3;
  writeln(' resultaat van x:= midget/3 =', x);
  x := midget * 0.75;
  writeln(' resultaat van x:= midget*0.75 =', x);
end.
program TurTe;

uses
  typ;

  function Tweelog(x: real): integer;
  var
    i: integer;
  begin
    i := 0;
    if x > 1 then
      Tweelog := -Tweelog(1 / x)
    else
    begin
      while x < 1 do
      begin
        Dec(i);
        x := 2 * x;
      end;
      if x <> 1 then
        Tweelog := 0
      else
        Tweelog := i;
    end;
  end;

var
  x: real;

begin

  { Test op macheps }
  Writeln('Macheps = 2', Tweelog(macheps));
  Writeln('Hoe wordt er afgerond?');
  x := 1 + macheps;
  Writeln('(1 + macheps     ) - 1 = ', x - 1);
  x := 1 + 0.75 * macheps;
  Writeln('(1 + 0.75*macheps) - 1 = ', x - 1);
  x := 1 + 0.5 * macheps;
  Writeln('(1 + 0.5*macheps ) - 1 = ', x - 1);
  x := 1 + 0.25 * macheps;
  Writeln('(1 + 0.25*macheps) - 1 = ', x - 1);

  {test op giant }
  writeln(' giant = ', giant);
  x := giant / 2;
  writeln(' resultaat van x:= giant / 2 =', x);
  {test op midget}
  writeln(' midget = ', midget);
  x := midget / 2;
  writeln(' resultaat van x:= midget/2 =', x);
  x := midget / 3;
  writeln(' resultaat van x:= midget/3 =', x);
  x := midget * 0.75;
  writeln(' resultaat van x:= midget*0.75 =', x);
end.
