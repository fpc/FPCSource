{$mode macpas}

{$r-}

{$ifdef unix}
uses
  baseunix,unix;
{$endif}

procedure error(l: longint);
begin
  writeln('error near ',l);
  halt(1);
end;


procedure test8bit;
type
  ta = 0..1;
const
  b: packed array[0..9] of ta = (1,0,1,1,1,0,1,1,1,0);
  results: array[0..9] of ta = (1,0,1,1,1,0,1,1,1,0);
var
  i: longint;
begin
  if (sizeof(b)<>2) then
   error(1);
  for i := low(results) to high(results) do
    if b[i] <> results[i] then
      error(7);
end;


procedure test8to16bit;
type
  ta = 0..7;
const
  b: packed array[0..5] of ta = (2,4,1,7,5,1);
  results: array[0..5] of ta = (2,4,1,7,5,1);
var
  i: longint;
begin
  if (sizeof(b)<>3) then
    error(16);
  for i := low(results) to high(results) do
    if b[i] <> results[i] then
      error(17);
end;


procedure test16bit;
type
  ta = 0..511;
const
  b: packed array[0..4] of ta = (356,39,485,100,500);
  results: array[0..4] of ta = (356,39,485,100,500);
var
  i: longint;
begin
  if (sizeof(b)<>6) then
    error(26);
  for i := low(results) to high(results) do
    if b[i] <> results[i] then
      error(27);
end;


procedure test16to24bit;
type
  ta = 0..2047;
const
  b: packed array[0..4] of ta = (1000,67,853,512,759);
  results: array[0..4] of ta = (1000,67,853,512,759);
var
  i: longint;
begin
  if (sizeof(b)<>7) then
    error(36);
  for i := low(results) to high(results) do
    if b[i] <> results[i] then
      error(37);
end;


procedure test32bit;
type
  ta = 0..(1 shl 19) - 1;
const
  b: packed array[0..4] of ta = ($0002F687,$00032222,$000178EE,$000057970,$0007E1D2);
  results: array[0..4] of ta = ($0002F687,$00032222,$000178EE,$000057970,$0007E1D2);
var
  i: longint;
begin
  if (sizeof(b)<>12) then
    error(46);
  for i := low(results) to high(results) do
    if b[i] <> results[i] then
      error(47);
end;


procedure test32bit2;
type
  ta = 0..(1 shl 24) - 1;
  taa = packed array[0..3*32-1] of ta;
  paa = ^taa;
const
  b: packed array[0..3*32-1] of ta = (
$17E546,$6D0CA6,$BC9CCD,$34E268,$F2C58F,$492C7D,$DBDC0F,$375B2C,$8DCC08,$96FE74,
$EF0AAD,$8BBB1A,$DF4554,$B75B0C,$728566,$81059B,$8D51F1,$88EF21,$CFF51E,$29BAAC,
$C52266,$53315E,$A558E9,$093C36,$1357E7,$95CD2E,$173011,$770CB1,$85F746,$7601FE,
$F5CD6A,$4E77B1,$F99073,$7520DB,$3F86DF,$2E5B82,$3282B8,$3A9FCD,$831B0B,$2DC3E6,
$38426E,$22CA1A,$E4FE56,$1B562F,$9A7757,$33BE8B,$013A7A,$7A0A4D,$7BC0B0,$48BFFB,
$62FA6C,$B3D806,$BFD49E,$3B5AB0,$696A18,$CADC48,$458E79,$834F63,$97D7A5,$5C92CB,
$E8E260,$D95895,$3D2DF0,$7257F7,$33D25C,$389DD8,$21107B,$002344,$655E49,$FBA7EF,
$D91F7E,$F694A2,$60F469,$160183,$275CAD,$1B8D0B,$41512E,$4184DE,$4319A9,$C93977,
$D8D40A,$6EBEA5,$C137B8,$82BED4,$67DAC6,$142013,$614C0E,$38867C,$BE1CDD,$6A40E5,
$518787,$219852,$48BD56,$827F40,$3CC0A6,$E79AF6
  );

  results: array[0..3*32-1] of ta = (
$17E546,$6D0CA6,$BC9CCD,$34E268,$F2C58F,$492C7D,$DBDC0F,$375B2C,$8DCC08,$96FE74,
$EF0AAD,$8BBB1A,$DF4554,$B75B0C,$728566,$81059B,$8D51F1,$88EF21,$CFF51E,$29BAAC,
$C52266,$53315E,$A558E9,$093C36,$1357E7,$95CD2E,$173011,$770CB1,$85F746,$7601FE,
$F5CD6A,$4E77B1,$F99073,$7520DB,$3F86DF,$2E5B82,$3282B8,$3A9FCD,$831B0B,$2DC3E6,
$38426E,$22CA1A,$E4FE56,$1B562F,$9A7757,$33BE8B,$013A7A,$7A0A4D,$7BC0B0,$48BFFB,
$62FA6C,$B3D806,$BFD49E,$3B5AB0,$696A18,$CADC48,$458E79,$834F63,$97D7A5,$5C92CB,
$E8E260,$D95895,$3D2DF0,$7257F7,$33D25C,$389DD8,$21107B,$002344,$655E49,$FBA7EF,
$D91F7E,$F694A2,$60F469,$160183,$275CAD,$1B8D0B,$41512E,$4184DE,$4319A9,$C93977,
$D8D40A,$6EBEA5,$C137B8,$82BED4,$67DAC6,$142013,$614C0E,$38867C,$BE1CDD,$6A40E5,
$518787,$219852,$48BD56,$827F40,$3CC0A6,$E79AF6
);
var
  i: longint;
{$ifdef unix}
  p,p2: pbyte;
  bp: paa;
{$endif}
begin
  if (sizeof(b)<>3*length(results)) then
    error(48);
{$ifdef unix}
  { check for reading past end of array }
  repeat
    p := fpmmap(nil,4096,PROT_READ or PROT_WRITE,MAP_PRIVATE or MAP_ANONYMOUS,-1,0);
    p2 := fpmmap(nil,4096,PROT_READ or PROT_WRITE,MAP_PRIVATE or MAP_ANONYMOUS,-1,0);
  until (ptruint(p2) = ptruint(p) + 4096);
  fpmunmap(p2,4096);
  move(b,pbyte(ptruint(p)+4096-sizeof(b))^,sizeof(b));
  bp := paa(ptruint(p)+4096-sizeof(b));
  for i := low(results) to high(results) do
    if bp^[i] <> results[i] then
      begin
        writeln(i);
        error(49);
      end;
  fpmunmap(p,4096);
{$else}
  for i := low(results) to high(results) do
    if b[i] <> results[i] then
      begin
        writeln(i);
        error(49);
      end;
{$endif}
end;


procedure test32to40bit;
type
  ta = 0..$7fffffff;
const
  b: packed array[0..4] of ta = ($71567851,$56789ABD,$50F11178,$39D68DDC,$6C7A5A7);
  results: array[0..4] of ta = ($71567851,$56789ABD,$50F11178,$39D68DDC,$6C7A5A7);
var
  i: longint;
begin
  if (sizeof(b)<>20) then
    error(56);
  for i := low(results) to high(results) do
    if b[i] <> results[i] then
      error(57);
end;

begin
  test8bit;
  test8to16bit;
  test16bit;
  test16to24bit;
  test32bit;
  test32bit2;
  test32to40bit;
end.
