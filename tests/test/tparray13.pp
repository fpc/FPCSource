{$mode macpas}

{$r-}

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
  test32to40bit;
end.
