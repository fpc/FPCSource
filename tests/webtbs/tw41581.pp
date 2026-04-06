{$mode objfpc}
program tw41581;
uses
  Math;

function Calc: Double; inline;
begin
  Result := Power(10, Floor(Log10(0.01)));
end;

begin
  writeln(Calc);
  if (Calc>0.0100001) or (Calc<0.0099999) then
    halt(1);
  writeln('ok');
end.
