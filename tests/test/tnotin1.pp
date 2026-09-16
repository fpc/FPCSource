{ Tests the "not in" operator, which is a short form for "not (a in b)" }
program tnotin1;

{$mode objfpc}{$H+}
{$modeswitch reorderedoperators}

type
  TColor = (clRed, clGreen, clBlue);
  TColors = set of TColor;

  TRange = record
    Lo, Hi: longint;
  end;

  generic TChecker<T> = class
    class function NotContains(const Value: T; const Values: TColors): boolean;
  end;

operator in (const Value: longint; const Range: TRange): boolean;
begin
  Result:=(Value>=Range.Lo) and (Value<=Range.Hi);
end;

class function TChecker.NotContains(const Value: T; const Values: TColors): boolean;
begin
  Result:=Value not in Values;
end;

type
  TColorChecker = specialize TChecker<TColor>;

var
  Colors: TColors;
  Color: TColor;
  C: char;
  B: byte;
  Range: TRange;
  Count: longint;
  Flag: boolean;
begin
  { set of enum }
  Colors:=[clRed, clBlue];
  if clRed not in Colors then
    halt(1);
  if not (clGreen not in Colors) then
    halt(2);
  Color:=clGreen;
  if Color not in [clGreen] then
    halt(3);

  { set of char with ranges }
  C:='x';
  if C not in ['a'..'z'] then
    halt(4);
  if not (C not in ['0'..'9', 'A'..'Z']) then
    halt(5);

  { byte in constant set }
  B:=200;
  if not (B not in [0..10, 100, 255]) then
    halt(6);

  { same result as the long form }
  for Color:=Low(TColor) to High(TColor) do
    if (Color not in Colors)<>(not (Color in Colors)) then
      halt(7);

  { plain assignment }
  Flag:=clGreen not in Colors;
  if not Flag then
    halt(8);

  { combined with "and"/"or", which bind tighter and thus need parentheses }
  if not ((clGreen not in Colors) and (clRed in Colors)) then
    halt(9);
  if (clRed not in Colors) or (clBlue not in Colors) then
    halt(10);

  { while condition }
  Count:=0;
  while (clGreen not in Colors) and (Count<2) do
    inc(Count);
  if Count<>2 then
    halt(11);

  { repeat..until condition }
  Count:=0;
  repeat
    inc(Count);
  until (clGreen not in Colors) or (Count>1);
  if Count<>1 then
    halt(12);

  { inside a generic, whose body is parsed from a replayed token stream }
  if not TColorChecker.NotContains(clGreen, Colors) then
    halt(13);
  if TColorChecker.NotContains(clRed, Colors) then
    halt(14);

  { overloaded "in" operator }
  Range.Lo:=1;
  Range.Hi:=10;
  if 5 not in Range then
    halt(15);
  if not (11 not in Range) then
    halt(16);

  writeln('ok');
end.
