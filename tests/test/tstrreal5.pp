program tstrreal4;
{ test for NegCurFormat values  by Zeljan Rikalo
  valid values are at:
  http://msdn.microsoft.com/en-us/library/dd373791%28VS.85%29.aspx
 }
uses SysUtils;

procedure test;
const
  MaxNegCurFormats = 15;
var
  s: string;
  r: double;
  i: integer;
begin
  DecimalSeparator := '.';
  r := -1.1;
  writeln('NegCurrFormat test pass 1 ...');
  for i := 0 to MaxNegCurFormats do
  begin
    NegCurrFormat := i;
    s := FloatToStrF(r, ffCurrency, 12, 1);
    writeln('NegCurrFormat: ',i,' value: ',s);
    case i of
      0,4,14,15: 
        if (Pos('(', s) = 0) and (Pos(')', s) = 0) then
          halt(1);
      else
      if Pos('-', s) = 0 then
        halt(1);
    end; 
  end;

  r := -0.001;
  writeln('NegCurrFormat test pass 2 ...');
  for i := 0 to MaxNegCurFormats do
  begin
    NegCurrFormat := i;
    s := FloatToStrF(r, ffCurrency, 12, 4);
    writeln('NegCurrFormat: ',i,' value: ',s);
    case i of
      0,4,14,15: 
        if (Pos('(', s) = 0) and (Pos(')', s) = 0) then
          halt(1);
      else
      if Pos('-', s) = 0 then
        halt(1);
    end; 
  end;

  writeln('Tests for NegCurrFormat: SUCCESS');
end;

begin
  test;
end.
