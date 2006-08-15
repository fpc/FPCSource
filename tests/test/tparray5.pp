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
{$ifdef ENDIAN_BIG}
  results: array[0..9] of ta = (1,0,1,0,1,0,1,0,1,0);
{$else}
  results: array[0..9] of ta = (0,1,0,1,0,1,0,1,0,1);
{$endif}
var
  a: ta;
  b: tb;
  i: longint;
begin
  fillchar(b,sizeof(b),$AA);
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
//  if (b[600] <> 1) then
//    error(6);
  for i := low(results) to high(results) do
    if b[i] <> results[i] then
      error(7);
//  i := 500;
//  if (b[i] <> 1) then
//    error(8);
end;


procedure test8to16bit;
type
  ta = 0..7;
  tb = packed array[0..1000] of ta;
const
{$ifdef ENDIAN_BIG}
{ 010 110 100 101 101 001 011 010 010 11010010110100101101001011010 }

  results: array[0..5] of ta = (2,6,4,5,5,1);
{$else}
{ (memory layout is different but equivalent with starting at end }
{ 01011010010110100101101001011 010 010 110 100 101 101 001 011 010 }
  results: array[0..5] of ta = (2,3,1,5,5,4);
{$endif}
var
  a: ta;
  b: tb;
  i: longint;
begin
  fillchar(b,sizeof(b),$5A);
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
//  if (b[600] <> 2) then
//    error(16);
  for i := low(results) to high(results) do
    if b[i] <> results[i] then
      error(17);
//  i := 500;
//  if (b[i] <> 5) then
//    error(18);
end;


procedure test16bit;
type
  ta = 0..511;
  tb = packed array[0..799] of ta;
  tc = array[0..899] of byte;
const
{$ifdef ENDIAN_BIG}
  { 010110100 111011001 011010011 101100101 101001110 110010110 100111011 001011010 0111011001011010011101100101101001110110 }
  results: array[0..4] of ta = ($5A*2,$76 * 4 + 1,$69 * 2 + 1,$B2 * 2 + 1,$A7 * 2);
{$else}
  { algorithm: cut bit string in 16 bit chunks, byteswap, take 9 bits from right to left per chunck, continuing at the right of the next chunck if the previous one is used up }
  { 001011010 100111011 110010110 101001110 101100101 011 0101101001110110010110100111011001011010011101100101101001110110 }

  results: array[0..4] of ta = ($2D*2,$9D*2+1,$CB*2,$A7*2,$B2*2+1);
{$endif}
var
  a: ta;
  b: tb;
  i: longint;
begin
  for i := 0 to high(tc) div 2 do
    begin
      tc(b)[i*2] := $5A;
      tc(b)[i*2+1] := $76;
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
//  if (b[600] <> $76 * 2) then
//    error(26);
  for i := low(results) to high(results) do
    if b[i] <> results[i] then
      error(27);
//  i := 500;
//  if (b[i] <> $A7 * 2) then
//    error(28);
end;

procedure test16to24bit;
type
  ta = 0..2047;
  tb = packed array[0..799] of ta;
  tc = array[0..1099] of byte;
const
{$ifdef ENDIAN_BIG}
  results: array[0..4] of ta = ($5A * 8 + 3,$B2 * 8 + 6,$9D * 8 + 4,$B4 * 8 + 7,$65 * 8 + 5);
{$else}
  {  %11001011010 01101001110 00111011001  01100101101 10110100111 011101100 0111011001011010 0111011001011010 0111011001011010 }
  results: array[0..4] of ta = ($0000065A,$0000034E,$000001D9,$0000032D,$000005A7);
{$endif}
var
  a: ta;
  b: tb;
  i: longint;
begin
  for i := 0 to high(tc) div 2 do
    begin
      tc(b)[i*2] := $5A;
      tc(b)[i*2+1] := $76;
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
//  if (b[600] <> $76 * 8 + 2) then
//    error(36);
  for i := low(results) to high(results) do
    if b[i] <> results[i] then
      error(37);
//  i := 500;
//  if (b[i] <> $65 * 8 + 5) then
//    error(38);
end;


procedure test32bit;
type
  ta = 0..(1 shl 19) - 1;
  tb = packed array[0..799] of ta;
  tc = array[0..1899] of byte;
const
{$ifdef ENDIAN_BIG}
  results: array[0..4] of ta = ($5A76*8+2, $D3B2*8+6,$9D96*8+4,$ECB4*8+7,$65A7*8+3);
{$else}
  {  0100111011001011010 0110100111011001011  0010110100111011001 0110010110100111011 1110110010110100111 0  01110110010110100111011001011010 01110110010110100111011001011010 01110110010110100111011001011010 01110110010110100111011001011010 01110110010110100111011001011010 01110110010110100111011001011010}
  results: array[0..4] of ta = ($0002765A,$00034ECB,$000169D9,$00032D3B,$000765A7);
{$endif}
var
  a: ta;
  b: tb;
  i: longint;
begin
  for i := 0 to high(tc) div 2 do
    begin
      tc(b)[i*2] := $5A;
      tc(b)[i*2+1] := $76;
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
//  if (b[600] <> $765A*8+3) then
//    error(46);
  for i := low(results) to high(results) do
    if b[i] <> results[i] then
      error(47);
//  i := 500;
//  if (b[i] <> $65a7*8+3) then
//    error(48);
end;

{
  write(hexstr(%1110110010110100111011001011010 ,8),',$');
  write(hexstr(%1101100101101001110110010110100 ,8),',$');
  write(hexstr(%1011001011010011101100101101001 ,8),',$');
  write(hexstr(%0110010110100111011001011010011 ,8),',$');
  writeln(hexstr(%1100101101001110110010110100111 ,8));
}


procedure test32to40bit;
type
  ta = 0..$7fffffff;
  tb = packed array[0..799] of ta;
  tc = array[0..3099] of byte;
const
{$ifdef ENDIAN_BIG}
  results: array[0..4] of ta = ($5A765A7*8+3,$2D3B2D3*8+5,$969D969*8+6,$CB4ECB4*8+7,$65A765A*8+3);
{$else}
{
1110110010110100111011001011010 1101100101101001110110010110100 1011001011010011101100101101001 0110010110100111011001011010011 1100101101001110110010110100111 1001011010011101100101101001110 011101
}
  results: array[0..4] of ta = ($765A765A,$6CB4ECB4,$5969D969,$32D3B2D3,$65A765A7);
{$endif}
var
  a: ta;
  b: tb;
  i: longint;
begin
  for i := 0 to high(tc) div 2 do
    begin
      tc(b)[i*2] := $5A;
      tc(b)[i*2+1] := $76;
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
//  i := 500;
//  if (b[i] <> $65A765A*8+3) then
//    error(58);
end;

begin
  test8bit;
  test8to16bit;
  test16bit;
  test16to24bit;
  test32bit;
  test32to40bit;
end.
