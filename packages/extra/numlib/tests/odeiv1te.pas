program odeiv1te;

uses
  typ,
  ode;

var
  ex, nv, i, term:     ArbInt;
  a, b, d, ya, yb, ae: ArbFloat;

  function f(x, y: ArbFloat): ArbFloat;
  begin
    f := -10 * (y - sqr(x));
  end; {f}

  function g(x, y: ArbFloat): ArbFloat;
  begin
    g := -100 * (y - sin(x)) + cos(x);
  end; {g}

  function h(x, y: ArbFloat): ArbFloat;
  begin
    h := 15 * y;
  end; {h}

  function phi(x: ArbFloat): ArbFloat;
  begin
    phi := -exp(-10 * x) * 0.02 + sqr(x) - x * 0.2 + 0.02;
  end; {phi}

begin
  Write(' program results odeiv1te');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  Read(nv);
  writeln('program results odeiv1te');
  writeln;
  writeln('   number of examples: ', nv: 2);
  for ex := 1 to nv do
  begin
    writeln;
    writeln('  example number :', ex: 2);
    Read(a, b, ya, ae);
    d := b - a;
    writeln('a': 3, 'b': 5, 'yb': 12, 'phi(b)': 20, 'ae': 10, 'term': 9);
    for i := 1 to 5 do
    begin
      odeiv1(@f, a, ya, b, yb, ae, term);
      writeln(a: 5: 2, b: 5: 2, ' ', yb: numdig, ' ', phi(b): numdig, ae: 9, term: 3);
      a  := b;
      ya := yb;
      b  := b + d;
    end; {i}
    writeln(' -------------------------------------------------');
  end; {ex}
  a  := 0;
  ya := 1;
  b  := 1;
  ae := 1e-6;
  odeiv1(@g, a, ya, b, yb, ae, term);
  writeln(a: 5: 2, b: 5: 2, ' ', yb - sin(1): numdig, ' ', ae: 9, term: 3);
  a  := 0;
  ya := 1e-3;
  b  := 1;
  ae := 1e-4;
  odeiv1(@h, a, ya, b, yb, ae, term);
  writeln(a: 5: 2, b: 5: 2, ' ', yb: numdig, ' ', 1e-3 * exp(15): numdig, ae: 9, term: 3);
  a  := 0;
  ya := 1e-3;
  b  := 1;
  ae := 1e-6;
  odeiv1(@h, a, ya, b, yb, ae, term);
  writeln(a: 5: 2, b: 5: 2, ' ', yb: numdig, ' ', 1e-3 * exp(15): numdig, ae: 9, term: 3);
  a  := 0;
  ya := 1e-3;
  b  := a;
  ae := 1e-6;
  odeiv1(@h, a, ya, b, yb, ae, term);
  writeln(a: 5: 2, b: 5: 2, ' ', yb: numdig, ' ', 1e-3 * exp(15): numdig, ae: 9, term: 3);
  a  := 0;
  ya := 1e-3;
  b  := a;
  ae := 0;
  odeiv1(@h, a, ya, b, yb, ae, term);
  writeln(a: 5: 2, b: 5: 2, ' ', yb: numdig, ' ', 1e-3 * exp(15): numdig, ae: 9, term: 3);
  Close(input);
  Close(output);
end.
program odeiv1te;

uses
  typ,
  ode;

var
  ex, nv, i, term:     ArbInt;
  a, b, d, ya, yb, ae: ArbFloat;

  function f(x, y: ArbFloat): ArbFloat;
  begin
    f := -10 * (y - sqr(x));
  end; {f}

  function g(x, y: ArbFloat): ArbFloat;
  begin
    g := -100 * (y - sin(x)) + cos(x);
  end; {g}

  function h(x, y: ArbFloat): ArbFloat;
  begin
    h := 15 * y;
  end; {h}

  function phi(x: ArbFloat): ArbFloat;
  begin
    phi := -exp(-10 * x) * 0.02 + sqr(x) - x * 0.2 + 0.02;
  end; {phi}

begin
  Write(' program results odeiv1te');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  Read(nv);
  writeln('program results odeiv1te');
  writeln;
  writeln('   number of examples: ', nv: 2);
  for ex := 1 to nv do
  begin
    writeln;
    writeln('  example number :', ex: 2);
    Read(a, b, ya, ae);
    d := b - a;
    writeln('a': 3, 'b': 5, 'yb': 12, 'phi(b)': 20, 'ae': 10, 'term': 9);
    for i := 1 to 5 do
    begin
      odeiv1(@f, a, ya, b, yb, ae, term);
      writeln(a: 5: 2, b: 5: 2, ' ', yb: numdig, ' ', phi(b): numdig, ae: 9, term: 3);
      a  := b;
      ya := yb;
      b  := b + d;
    end; {i}
    writeln(' -------------------------------------------------');
  end; {ex}
  a  := 0;
  ya := 1;
  b  := 1;
  ae := 1e-6;
  odeiv1(@g, a, ya, b, yb, ae, term);
  writeln(a: 5: 2, b: 5: 2, ' ', yb - sin(1): numdig, ' ', ae: 9, term: 3);
  a  := 0;
  ya := 1e-3;
  b  := 1;
  ae := 1e-4;
  odeiv1(@h, a, ya, b, yb, ae, term);
  writeln(a: 5: 2, b: 5: 2, ' ', yb: numdig, ' ', 1e-3 * exp(15): numdig, ae: 9, term: 3);
  a  := 0;
  ya := 1e-3;
  b  := 1;
  ae := 1e-6;
  odeiv1(@h, a, ya, b, yb, ae, term);
  writeln(a: 5: 2, b: 5: 2, ' ', yb: numdig, ' ', 1e-3 * exp(15): numdig, ae: 9, term: 3);
  a  := 0;
  ya := 1e-3;
  b  := a;
  ae := 1e-6;
  odeiv1(@h, a, ya, b, yb, ae, term);
  writeln(a: 5: 2, b: 5: 2, ' ', yb: numdig, ' ', 1e-3 * exp(15): numdig, ae: 9, term: 3);
  a  := 0;
  ya := 1e-3;
  b  := a;
  ae := 0;
  odeiv1(@h, a, ya, b, yb, ae, term);
  writeln(a: 5: 2, b: 5: 2, ' ', yb: numdig, ' ', 1e-3 * exp(15): numdig, ae: 9, term: 3);
  Close(input);
  Close(output);
end.
