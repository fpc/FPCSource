program Intge3te;

uses
  Typ,
  Int;

const
  ae: ArbFloat = 1e-5;
var
  a, b, integral, err: ArbFloat;
  i, term, nd: ArbInt;

  function cx(x: ArbFloat): ArbFloat;
  begin
    cx := 1 / (sqr(x) + 1);
  end;

begin
  nd := numdig div 2;
  for i := 1 to 11 do
  begin
    case i of
      1:
      begin
        a := -infinity;
        b := 0;
      end;
      2:
      begin
        a := infinity;
        b := 0;
      end;
      3:
      begin
        a := -infinity;
        b := infinity;
      end;
      4:
      begin
        a := infinity;
        b := -infinity;
      end;
      5:
      begin
        a := 0;
        b := 1;
      end;
      6:
      begin
        a := 1;
        b := 1;
      end;
      7:
      begin
        a := 1;
        b := 0;
      end;
      8:
      begin
        a := infinity;
        b := infinity;
      end;
      9:
      begin
        a := 0;
        b := infinity;
      end;
      10:
      begin
        a := 0;
        b := -infinity;
      end;
      11:
      begin
        a := -infinity;
        b := -infinity;
      end;
    end;
    Int1fr(@cx, a, b, ae, integral, err, term);
    if i = 1 then
    begin
      writeln(' ae =', ae: numdig);
      writeln('': nd, 'a', '': numdig, 'b', '': numdig, 'int', '': nd + 3,
        'err', '': nd - 2, 'term');
    end;
    Writeln(a: numdig, ' ', b: numdig, ' ', integral: numdig, '  ',
      err: 12, '   ', term: 1);
  end;
end.
program Intge3te;

uses
  Typ,
  Int;

const
  ae: ArbFloat = 1e-5;
var
  a, b, integral, err: ArbFloat;
  i, term, nd: ArbInt;

  function cx(x: ArbFloat): ArbFloat;
  begin
    cx := 1 / (sqr(x) + 1);
  end;

begin
  nd := numdig div 2;
  for i := 1 to 11 do
  begin
    case i of
      1:
      begin
        a := -infinity;
        b := 0;
      end;
      2:
      begin
        a := infinity;
        b := 0;
      end;
      3:
      begin
        a := -infinity;
        b := infinity;
      end;
      4:
      begin
        a := infinity;
        b := -infinity;
      end;
      5:
      begin
        a := 0;
        b := 1;
      end;
      6:
      begin
        a := 1;
        b := 1;
      end;
      7:
      begin
        a := 1;
        b := 0;
      end;
      8:
      begin
        a := infinity;
        b := infinity;
      end;
      9:
      begin
        a := 0;
        b := infinity;
      end;
      10:
      begin
        a := 0;
        b := -infinity;
      end;
      11:
      begin
        a := -infinity;
        b := -infinity;
      end;
    end;
    Int1fr(@cx, a, b, ae, integral, err, term);
    if i = 1 then
    begin
      writeln(' ae =', ae: numdig);
      writeln('': nd, 'a', '': numdig, 'b', '': numdig, 'int', '': nd + 3,
        'err', '': nd - 2, 'term');
    end;
    Writeln(a: numdig, ' ', b: numdig, ' ', integral: numdig, '  ',
      err: 12, '   ', term: 1);
  end;
end.
