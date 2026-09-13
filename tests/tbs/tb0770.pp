{ typed constants whose strings are written after the program has started --
  widestrings on targets where they are not reference counted, at startup;
  ansistrings initialised from resourcestrings, whenever the resourcestrings
  are translated and when the unit is finalized -- inside an array that is
  itself a field of a record, and not at the start of it: the value must be
  stored into the array element, not into the fields in front of the array }
program tb0770;

{$mode objfpc}{$H+}

resourcestring
  rs1 = 'one';
  rs2 = 'two';

type
  TWideRec = record
    a, b: longint;
    w: array[0..1] of widestring;
    c: longint;
  end;

  TResRec = record
    a, b: longint;
    s: array[0..1] of ansistring;
    c: longint;
  end;

  TGridRec = record
    a: longint;
    g: array[0..1,0..1] of ansistring;
  end;

const
  W: TWideRec = (a: 11; b: 22; w: ('xy', 'zw'); c: 55);
  R: TResRec = (a: 33; b: 44; s: (rs1, rs2); c: 66);
  G: TGridRec = (a: 77; g: ((rs1, rs2), (rs2, rs1)));

{ the compiler stores a resourcestring's text directly into an ansistring
  initialised from it, so R and G hold the right values even if the place
  recorded for them is wrong; translating the resourcestrings writes through
  those places }
function Translate(Name: AnsiString; Value: RTLString; Hash: Longint; arg: pointer): RTLString;
begin
  Translate:='T:'+Value;
end;

begin
  if (W.a<>11) or (W.b<>22) or (W.c<>55) then
    halt(1);
  if (W.w[0]<>'xy') or (W.w[1]<>'zw') then
    halt(2);
  if (R.a<>33) or (R.b<>44) or (R.c<>66) then
    halt(3);
  if (R.s[0]<>'one') or (R.s[1]<>'two') then
    halt(4);
  SetResourceStrings(@Translate,nil);
  if (R.a<>33) or (R.b<>44) or (R.c<>66) then
    halt(5);
  if (R.s[0]<>'T:one') or (R.s[1]<>'T:two') then
    halt(6);
  if G.a<>77 then
    halt(7);
  if (G.g[0,0]<>'T:one') or (G.g[0,1]<>'T:two') or (G.g[1,0]<>'T:two') or (G.g[1,1]<>'T:one') then
    halt(8);
  writeln('ok');
end.
