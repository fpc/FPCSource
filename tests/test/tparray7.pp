{$mode macpas}

{$r-}

{$ifdef unix}
uses
  baseunix, unix;
{$endif}

procedure error(l: longint);
begin
  writeln('error near ',l);
  halt(1);
end;


procedure test8bit;
type
  ta = 0..1;
  tb = packed array[0..999] of ta;
  tc = array[0..124] of byte;
const
  results: array[0..9] of ta = (1,0,1,1,1,0,1,1,1,0);
var
  a: ta;
  b: tb;
  i,j: longint;
begin
  fillchar(b,sizeof(b),0);
  for i := low(results) to high(results) do
    begin
      b[i] := results[i];
      for j := succ(i) to high(results) do
        if b[j] <> 0 then
          error(201);
      if b[i] <> results[i] then
        error(202);
    end;
  if (b[0] <> results[0]) then
    error(1);
  if (b[1] <> results[1]) then
    error(2);
  if (b[2] <> results[2]) then
    error(3);
  if (b[3] <> results[3]) then
    error(4);
  if (b[4] <> results[4]) then
    error(5);
  for i := low(results) to high(results) do
    if b[i] <> results[i] then
      error(7);
end;


procedure test8to16bit;
type
  ta = 0..7;
  tb = packed array[0..1000] of ta;
const
  results: array[0..5] of ta = (2,4,1,7,5,1);
var
  a: ta;
  b: tb;
  i,j: longint;
begin
  fillchar(b,sizeof(b),$ff);
  for i := low(results) to high(results) do
    begin
      b[i] := results[i];
      for j := succ(i) to high(results) do
        if b[j] <> high(ta) then
          error(211);
      if b[i] <> results[i] then
        error(212);
    end;
  if (b[0] <> results[0]) then
    error(11);
  if (b[1] <> results[1]) then
    error(12);
  if (b[2] <> results[2]) then
    error(13);
  if (b[3] <> results[3]) then
    error(14);
  if (b[4] <> results[4]) then
    error(15);
  if (b[5] <> results[5]) then
    error(155);
  for i := low(results) to high(results) do
    if b[i] <> results[i] then
      error(17);
end;


procedure test16bit;
type
  ta = 0..511;
  tb = packed array[0..799] of ta;
  tc = array[0..899] of byte;
const
  results: array[0..4] of ta = (356,39,485,100,500);
var
  a: ta;
  b: tb;
  i,j: longint;
begin
  fillchar(b,sizeof(b),$ff);
  for i := low(results) to high(results) do
    begin
      b[i] := results[i];
      for j := succ(i) to high(results) do
        if b[j] <> high(ta) then
          error(221);
      if b[i] <> results[i] then
        error(222);
    end;
  if (b[0] <> results[0]) then
    error(21);
  if (b[1] <> results[1]) then
    error(22);
  if (b[2] <> results[2]) then
    error(23);
  if (b[3] <> results[3]) then
    error(24);
  if (b[4] <> results[4]) then
    error(25);
  for i := low(results) to high(results) do
    if b[i] <> results[i] then
      error(27);
end;


procedure test16to24bit;
type
  ta = 0..2047;
  tb = packed array[0..799] of ta;
  tc = array[0..1099] of byte;
const
  results: array[0..4] of ta = (1000,67,853,512,759);
var
  a: ta;
  b: tb;
  i,j: longint;
begin
  fillchar(b,sizeof(b),$ff);
  for i := low(results) to high(results) do
    begin
      b[i] := results[i];
      for j := succ(i) to high(results) do
        if b[j] <> high(ta) then
          error(231);
      if b[i] <> results[i] then
        error(232);
    end;
  if (b[0] <> results[0]) then
    error(31);
  if (b[1] <> results[1]) then
    error(32);
  if (b[2] <> results[2]) then
    error(33);
  if (b[3] <> results[3]) then
    error(34);
  if (b[4] <> results[4]) then
    error(35);
  for i := low(results) to high(results) do
    if b[i] <> results[i] then
      error(37);
end;


procedure test32bit;
type
  ta = 0..(1 shl 19) - 1;
  tb = packed array[0..799] of ta;
  tc = array[0..1899] of byte;
const
  results: array[0..4] of ta = ($0002F687,$00032222,$000178EE,$000057970,$0007E1D2);
var
  a: ta;
  b: tb;
  i,j: longint;
begin
  fillchar(b,sizeof(b),$ff);
  for i := low(results) to high(results) do
    begin
      b[i] := results[i];
      for j := succ(i) to high(results) do
        if b[j] <> high(ta) then
          error(241);
      if b[i] <> results[i] then
        error(242);
    end;
  if (b[0] <> results[0]) then
    error(41);
  if (b[1] <> results[1]) then
    error(42);
  if (b[2] <> results[2]) then
    error(43);
  if (b[3] <> results[3]) then
    error(44);
  if (b[4] <> results[4]) then
    error(45);
  for i := low(results) to high(results) do
    if b[i] <> results[i] then
      error(47);
end;


procedure test32bit2;
type
  ta = 0..(1 shl 24) - 1;
  tb = packed array[0..3*32-1] of ta;
  paa = ^tb;
const
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
  a: ta;
  i,j: longint;
{$ifdef unix}
  p,p2: pbyte;
  bp: paa;
{$else}
  b: tb;
{$endif}
begin
{$ifdef unix}
  { check for reading/writing past end of array }
  repeat
    p := fpmmap(nil,4096,PROT_READ or PROT_WRITE,MAP_PRIVATE or MAP_ANONYMOUS,-1,0);
    p2 := fpmmap(nil,4096,PROT_READ or PROT_WRITE,MAP_PRIVATE or MAP_ANONYMOUS,-1,0);
  until (ptruint(p2) = ptruint(p) + 4096);
  fpmunmap(p2,4096);
  fillchar(p^,4096,$ff);
  bp := paa(ptruint(p)+4096-sizeof(tb));
  for i := low(results) to high(results) do
    begin
      bp^[i] := results[i];
      for j := succ(i) to high(results) do
        if bp^[j] <> high(ta) then
          error(241);
      if bp^[i] <> results[i] then
        error(242);
    end;
  for i := low(results) to high(results) do
    if bp^[i] <> results[i] then
      begin
        writeln(i);
        error(47);
      end;
  if (bp^[0] <> results[0]) then
    error(41);
  if (bp^[1] <> results[1]) then
    error(42);
  if (bp^[2] <> results[2]) then
    error(43);
  if (bp^[3] <> results[3]) then
    error(44);
  if (bp^[4] <> results[4]) then
    error(45);
  if (bp^[3*32-2] <> results[3*32-2]) then
    error(46);
  if (bp^[3*32-1] <> results[3*32-1]) then
    error(47);
  for i := low(results) to high(results) do
    if bp^[i] <> results[i] then
      error(48);
  fpmunmap(p,4096);
{$else}
  fillchar(b,sizeof(b),$ff);
  for i := low(results) to high(results) do
    begin
      b[i] := results[i];
      for j := succ(i) to high(results) do
        if b[j] <> high(ta) then
          error(241);
      if b[i] <> results[i] then
        error(242);
    end;
  if (b[0] <> results[0]) then
    error(41);
  if (b[1] <> results[1]) then
    error(42);
  if (b[2] <> results[2]) then
    error(43);
  if (b[3] <> results[3]) then
    error(44);
  if (b[4] <> results[4]) then
    error(45);
  for i := low(results) to high(results) do
    if b[i] <> results[i] then
      error(47);
{$endif}
end;


procedure test32to40bit;
type
  ta = 0..$7fffffff;
  tb = packed array[0..799] of ta;
  tc = array[0..3099] of byte;
const
  results: array[0..4] of ta = ($71567851,$56789ABD,$50F11178,$39D68DDC,$6C7A5A7);
var
  a: ta;
  b: tb;
  i,j: longint;
begin
  fillchar(b,sizeof(b),$ff);
  for i := low(results) to high(results) do
    begin
      b[i] := results[i];
      for j := succ(i) to high(results) do
        if b[j] <> high(ta) then
          error(251);
      if b[i] <> results[i] then
        error(252);
    end;
  if (b[0] <> results[0]) then
    error(51);
  if (b[1] <> results[1]) then
    error(52);
  if (b[2] <> results[2]) then
    error(53);
  if (b[3] <> results[3]) then
    error(54);
  if (b[4] <> results[4]) then
    error(55);
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
