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
  test32to40bit;
end.
