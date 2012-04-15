{ Tests passing of byte arrays by value of different sizes to C methods }
program passvaluestest;
{$MODE DELPHI}
{$R-}
{$Q-}
type
  int8_t = shortint;
  pint8_t = ^int8_t;

var
  success : boolean;

{$packrecords c}

type
  arr1 = array[1..1] of int8_t;
  arr2 = array[1..2] of int8_t;
  arr3 = array[1..3] of int8_t;
  arr4 = array[1..4] of int8_t;
  arr5 = array[1..5] of int8_t;
  arr7 = array[1..7] of int8_t;
  arr8 = array[1..8] of int8_t;
  arr9 = array[1..9] of int8_t;
  arr15 = array[1..15] of int8_t;
  arr16 = array[1..16] of int8_t;
  arr17 = array[1..17] of int8_t;
  arr24 = array[1..24] of int8_t;
  arr31 = array[1..31] of int8_t;
  arr32 = array[1..32] of int8_t;
  arr33 = array[1..33] of int8_t;

procedure fill(var mem; size : integer);
var
  i : Integer;
  p : pint8_t;
begin
  p := @mem;
  for i := 0 to size-1 do begin
    p^ := random(255)+1;
    inc(p);
  end;
end;

procedure verify(val1, val2 : int64; nr : Integer);
begin
  success := success and (val1 = val2);
  Write('Testing test ', nr , ', was ', val1, ', should be ', val2, '...');
  if (val1 = val2) then
    WriteLn('Success.')
  else
    WriteLn('Failed');
end;

function check(const s : array of int8_t) : int64;
var
 i : Integer;
begin
  result := 0;
  for i := low(s) to high(s) do
    inc(result, s[i]);
end;

{$L tcext4.o}
function pass1(s : arr1) : int64; cdecl; external;
function pass2(s : arr2) : int64; cdecl; external;
function pass3(s : arr3) : int64; cdecl; external;
function pass4(s : arr4) : int64; cdecl; external;
function pass5(s : arr5) : int64; cdecl; external;
function pass7(s : arr7) : int64; cdecl; external;
function pass8(s : arr8) : int64; cdecl; external;
function pass9(s : arr9) : int64; cdecl; external;
function pass15(s : arr15) : int64; cdecl; external;
function pass16(s : arr16) : int64; cdecl; external;
function pass17(s : arr17) : int64; cdecl; external;
function pass24(s : arr24) : int64; cdecl; external;
function pass31(s : arr31) : int64; cdecl; external;
function pass32(s : arr32) : int64; cdecl; external;
function pass33(s : arr33) : int64; cdecl; external;

procedure dotest;
var
  s1 : arr1;
  s2 : arr2;
  s3 : arr3;
  s4 : arr4;
  s5 : arr5;
  s7 : arr7;
  s8 : arr8;
  s9 : arr9;
  s15 : arr15;
  s16 : arr16;
  s17 : arr17;
  s24 : arr24;
  s31 : arr31;
  s32 : arr32;
  s33 : arr33;

begin
  randseed := 20;
  success := true;

  fill(s1, sizeof(s1));
  fill(s2, sizeof(s2));
  fill(s3, sizeof(s3));
  fill(s4, sizeof(s4));
  fill(s5, sizeof(s5));
  fill(s7, sizeof(s7));
  fill(s8, sizeof(s8));
  fill(s9, sizeof(s9));
  fill(s15, sizeof(s15));
  fill(s16, sizeof(s16));
  fill(s17, sizeof(s17));
  fill(s24, sizeof(s24));
  fill(s31, sizeof(s31));
  fill(s32, sizeof(s32));
  fill(s33, sizeof(s33));

  verify(pass1(s1), check(s1), 1);
  verify(pass2(s2), check(s2), 2);
  verify(pass3(s3), check(s3), 3);
  verify(pass4(s4), check(s4), 4);
  verify(pass5(s5), check(s5), 5);
  verify(pass7(s7), check(s7), 7);
  verify(pass8(s8), check(s8), 8);
  verify(pass9(s9), check(s9), 9);
  verify(pass15(s15), check(s15), 15);
  verify(pass16(s16), check(s16), 16);
  verify(pass17(s17), check(s17), 17);
  verify(pass24(s24), check(s24), 24);
  verify(pass31(s31), check(s31), 31);
  verify(pass32(s32), check(s32), 32);
  verify(pass33(s33), check(s33), 33);

  if (not success) then
    halt(1);
end;

begin
  dotest;
end.
