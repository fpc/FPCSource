program odeiv2te;

uses
  typ,
  ode;

const
  n1 = 3;
  n2 = 4;
  n3 = 6;
  n  = n2 - n1 + 1;
  n4 = n3 + n - 1;
var
  ex, nv, i, j, k, h, term: ArbInt;
  a, b, d, ae: ArbFloat;
  ya: array[n1..n2] of ArbFloat;
  yb: array[n3..n4] of ArbFloat;

  procedure f(x: ArbFloat; var y, y1: ArbFloat);
  var
    yloc:  array[1..n] of ArbFloat absolute y;
    y1loc: array[1..n] of ArbFloat absolute y1;
  begin
    y1loc[1] := 2 * x * yloc[1] + yloc[2];
    y1loc[2] := -yloc[1] + 2 * x * yloc[2];
  end; {f}

  function phi1(x: ArbFloat): ArbFloat;
  begin
    phi1 := exp(x * x) * sin(x);
  end; {phi1}

  function phi2(x: ArbFloat): ArbFloat;
  begin
    phi2 := exp(x * x) * cos(x);
  end; {phi2}

begin
  Write(' program results odeiv2te');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  Read(nv);
  writeln;
  writeln('   number of examples: ', nv: 2);
  k := numdig;
  h := k div 2;
  for ex := 1 to nv do
  begin
    writeln;
    writeln('  example number :', ex: 2);
    Read(a, b);
    for j := n1 to n2 do
      Read(ya[j]);
    Read(ae);
    d := b - a;
    writeln;
    writeln(' ae =', ae: 10);
    writeln;
    writeln('b': 3, 'yb1': h + 4, 'yb2': k, 'phi1(x)': k + 3, 'phi2(x)': k - 2, 'term': h + 2);
    for i := 1 to 5 do
    begin
      odeiv2(@f, a, ya[n1], b, yb[n3], n, ae, term);
      writeln(b: 5: 2, yb[n3]: k, yb[n3 + 1]: k, phi1(b): k, phi2(b): k, term: 3);
      a := b;
      for j := n1 to n2 do
        ya[j] := yb[n3 - n1 + j];
      b := b + d;
    end; {i}
    writeln(' -------------------------------------------------');
  end; {ex}
  Close(input);
  Close(output);
end.
program odeiv2te;

uses
  typ,
  ode;

const
  n1 = 3;
  n2 = 4;
  n3 = 6;
  n  = n2 - n1 + 1;
  n4 = n3 + n - 1;
var
  ex, nv, i, j, k, h, term: ArbInt;
  a, b, d, ae: ArbFloat;
  ya: array[n1..n2] of ArbFloat;
  yb: array[n3..n4] of ArbFloat;

  procedure f(x: ArbFloat; var y, y1: ArbFloat);
  var
    yloc:  array[1..n] of ArbFloat absolute y;
    y1loc: array[1..n] of ArbFloat absolute y1;
  begin
    y1loc[1] := 2 * x * yloc[1] + yloc[2];
    y1loc[2] := -yloc[1] + 2 * x * yloc[2];
  end; {f}

  function phi1(x: ArbFloat): ArbFloat;
  begin
    phi1 := exp(x * x) * sin(x);
  end; {phi1}

  function phi2(x: ArbFloat): ArbFloat;
  begin
    phi2 := exp(x * x) * cos(x);
  end; {phi2}

begin
  Write(' program results odeiv2te');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  Read(nv);
  writeln;
  writeln('   number of examples: ', nv: 2);
  k := numdig;
  h := k div 2;
  for ex := 1 to nv do
  begin
    writeln;
    writeln('  example number :', ex: 2);
    Read(a, b);
    for j := n1 to n2 do
      Read(ya[j]);
    Read(ae);
    d := b - a;
    writeln;
    writeln(' ae =', ae: 10);
    writeln;
    writeln('b': 3, 'yb1': h + 4, 'yb2': k, 'phi1(x)': k + 3, 'phi2(x)': k - 2, 'term': h + 2);
    for i := 1 to 5 do
    begin
      odeiv2(@f, a, ya[n1], b, yb[n3], n, ae, term);
      writeln(b: 5: 2, yb[n3]: k, yb[n3 + 1]: k, phi1(b): k, phi2(b): k, term: 3);
      a := b;
      for j := n1 to n2 do
        ya[j] := yb[n3 - n1 + j];
      b := b + d;
    end; {i}
    writeln(' -------------------------------------------------');
  end; {ex}
  Close(input);
  Close(output);
end.
