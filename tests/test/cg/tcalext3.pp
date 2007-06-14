{ Tests passing of different records by value to C methods. 
 One type of these records has one field which is a simple array of bytes,
 the other consists of a few fields of atomic size.
 
 Note that it does not only test a single field of these records, but all
 by comparing the sum of the field values with the sum returned by the
 C function.
}
program calext3;
{$MODE DELPHI}

type
  int8_t = shortint;
  pint8_t = ^int8_t;
  int16_t = smallint;
  int32_t = longint;
  int64_t = int64;

var
  success : boolean;

{$packrecords c}

type
  struct_arr1 = record
    v : array[0..0] of int8_t;
  end;

  struct_arr2 = record
    v : array[0..1] of int8_t;
  end;

  struct_arr3 = record
    v : array[0..2] of int8_t;
  end;

  struct_arr4 = record
    v : array[0..3] of int8_t;
  end;

  struct_arr5 = record
    v : array[0..4] of int8_t;
  end;

  struct_arr6 = record
    v : array[0..5] of int8_t;
  end;

  struct_arr7 = record
    v : array[0..6] of int8_t;
  end;

  struct_arr8 = record
    v : array[0..7] of int8_t;
  end;

  struct_arr9 = record
    v : array[0..8] of int8_t;
  end;

  struct_arr10 = record
    v : array[0..9] of int8_t;
  end;

  struct_arr11 = record
    v : array[0..10] of int8_t;
  end;

  struct_arr15 = record
    v : array[0..14] of int8_t;
  end;

  struct_arr16 = record
    v : array[0..15] of int8_t;
  end;

  struct_arr17 = record
    v : array[0..16] of int8_t;
  end;


  struct_arr27 = record
    v : array[0..26] of int8_t;
  end;

  struct_arr31 = record
    v : array[0..30] of int8_t;
  end;

  struct_arr32 = record
    v : array[0..31] of int8_t;
  end;

  struct_arr33 = record
    v : array[0..32] of int8_t;
  end;


  struct1 = record
    v : int8_t;
  end;

  struct2 = record
    v : int16_t;
  end;

  struct3 = record
    v1 : int16_t;
    v2 : int8_t;
  end;

  struct4 = record
    v : int32_t;
  end;

  struct5 = record
    v1 : int32_t;
    v2 : int8_t;
  end;

  struct6 = record
    v1 : int32_t;
    v2 : int16_t;
  end;
  
  struct7 = record
    v1 : int32_t;
    v2 : int16_t;
    v3 : int8_t;
  end;

  struct8 = record
    v : int64_t
  end;

  struct9 = record
    v1 : int64_t;
    v2 : int8_t;
  end;

  struct10 = record
    v1 : int64_t;
    v2 : int16_t;
  end;

  struct11 = record
    v1 : int64_t;
    v2 : int16_t;
    v3 : int8_t;
  end;

  struct12 = record
    v1 : int64_t;
    v2 : int32_t;
  end;

  struct13 = record
    v1 : int64_t;
    v2 : int32_t;
    v3 : int8_t;
  end;

  struct14 = record
    v1 : int64_t;
    v2 : int32_t;
    v3 : int16_t;
  end;
  
  struct15 = record 
    v1 : int64_t;
    v2 : int32_t;
    v3 : int16_t;
    v4 : int8_t;
  end;

  struct16 = record
    v1 : int64_t;
    v2 : int64_t;
  end;

  struct31 = record
    v1 : int64_t;
    v2 : int64_t;
    v3 : int64_t;
    v4 : int32_t;
    v5 : int16_t;
    v6 : int8_t;
  end;

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

procedure verify(val1, val2 : int64_t; nr : Integer);
begin
  success := success and (val1 = val2);
  Write('Testing test ', nr , ', was ', val1, ', should be ', val2, '...');
  if (val1 = val2) then
    WriteLn('Success.')
  else
    WriteLn('Failed');
end;

function check1(s : struct1) : int64_t;
begin
  result := s.v;
end;

function check2(s : struct2) : int64_t;
begin
  result := s.v;
end;

function check3(s : struct3) : int64_t;
begin
  result := s.v1 + s.v2;
end;

function check4(s : struct4) : int64_t;
begin
  result := s.v;
end;

function check5(s : struct5) : int64_t;
begin
  result := s.v1 + s.v2;
end;

function check6(s : struct6) : int64_t;
begin
  result := s.v1 + s.v2;
end;

function check7(s : struct7) : int64_t;
begin
  result := s.v1 + s.v2 + s.v3;
end;

function check8(s : struct8) : int64_t;
begin
  result := s.v;
end;

function check9(s : struct9) : int64_t;
begin
  result := s.v1 + s.v2;
end;

function check10(s : struct10) : int64_t;
begin
  result := s.v1 + s.v2;
end;

function check11(s : struct11) : int64_t;
begin
  result := s.v1 + s.v2 + s.v3;
end;

function check12(s : struct12) : int64_t;
begin
  result := s.v1 + s.v2;
end;

function check13(s : struct13) : int64_t;
begin
  result := s.v1 + s.v2 + s.v3;
end;

function check14(s : struct14) : int64_t;
begin
  result := s.v1 + s.v2 + s.v3;
end;

function check15(s : struct15) : int64_t;
begin
  result := s.v1 + s.v2 + s.v3 + s.v4;
end;

function check16(s : struct16) : int64_t;
begin
  result := s.v1 + s.v2;
end;

function check31(s : struct31) : int64_t;
begin
  result := s.v1 + s.v2 + s.v3 + s.v4 + s.v5 + s.v6;
end;


function check_arr1(s : struct_arr1) : int64_t;
var
  i : int32_t;
begin
  result := 0;
  for i := low(s.v) to high(s.v) do
    inc(result, s.v[i]);
end;

function check_arr2(s : struct_arr2) : int64_t;
var
  i : int32_t;
begin
  result := 0;
  for i := low(s.v) to high(s.v) do
    inc(result, s.v[i]);
end;

function check_arr3(s : struct_arr3) : int64_t;
var
  i : int32_t;
begin
  result := 0;
  for i := low(s.v) to high(s.v) do
    inc(result, s.v[i]);
end;

function check_arr4(s : struct_arr4) : int64_t;
var
  i : int32_t;
begin
  result := 0;
  for i := low(s.v) to high(s.v) do
    inc(result, s.v[i]);
end;

function check_arr5(s : struct_arr5) : int64_t;
var
  i : int32_t;
begin
  result := 0;
  for i := low(s.v) to high(s.v) do
    inc(result, s.v[i]);
end;

function check_arr6(s : struct_arr6) : int64_t;
var
  i : int32_t;
begin
  result := 0;
  for i := low(s.v) to high(s.v) do
    inc(result, s.v[i]);
end;

function check_arr7(s : struct_arr7) : int64_t;
var
  i : int32_t;
begin
  result := 0;
  for i := low(s.v) to high(s.v) do
    inc(result, s.v[i]);
end;

function check_arr8(s : struct_arr8) : int64_t;
var
  i : int32_t;
begin
  result := 0;
  for i := low(s.v) to high(s.v) do
    inc(result, s.v[i]);
end;

function check_arr9(s : struct_arr9) : int64_t;
var
  i : int32_t;
begin
  result := 0;
  for i := low(s.v) to high(s.v) do
    inc(result, s.v[i]);
end;

function check_arr10(s : struct_arr10) : int64_t;
var
  i : int32_t;
begin
  result := 0;
  for i := low(s.v) to high(s.v) do
    inc(result, s.v[i]);
end;

function check_arr11(s : struct_arr11) : int64_t;
var
  i : int32_t;
begin
  result := 0;
  for i := low(s.v) to high(s.v) do
    inc(result, s.v[i]);
end;

function check_arr15(s : struct_arr15) : int64_t;
var
  i : int32_t;
begin
  result := 0;
  for i := low(s.v) to high(s.v) do
    inc(result, s.v[i]);
end;

function check_arr16(s : struct_arr16) : int64_t;
var
  i : int32_t;
begin
  result := 0;
  for i := low(s.v) to high(s.v) do
    inc(result, s.v[i]);
end;

function check_arr17(s : struct_arr17) : int64_t;
var
  i : int32_t;
begin
  result := 0;
  for i := low(s.v) to high(s.v) do
    inc(result, s.v[i]);
end;

function check_arr27(s : struct_arr27) : int64_t;
var
  i : int32_t;
begin
  result := 0;
  for i := low(s.v) to high(s.v) do
    inc(result, s.v[i]);
end;

function check_arr31(s : struct_arr31) : int64_t;
var
  i : int32_t;
begin
  result := 0;
  for i := low(s.v) to high(s.v) do
    inc(result, s.v[i]);
end;

function check_arr32(s : struct_arr32) : int64_t;
var
  i : int32_t;
begin
  result := 0;
  for i := low(s.v) to high(s.v) do
    inc(result, s.v[i]);
end;

function check_arr33(s : struct_arr33) : int64_t;
var
  i : int32_t;
begin
  result := 0;
  for i := low(s.v) to high(s.v) do
    inc(result, s.v[i]);
end;

{$L tcext3.o}
function pass1(s : struct1) : int64_t; cdecl; external;
function pass2(s : struct2) : int64_t; cdecl; external;
function pass3(s : struct3) : int64_t; cdecl; external;
function pass4(s : struct4) : int64_t; cdecl; external;
function pass5(s : struct5) : int64_t; cdecl; external;
function pass6(s : struct6) : int64_t; cdecl; external;
function pass7(s : struct7) : int64_t; cdecl; external;
function pass8(s : struct8) : int64_t; cdecl; external;
function pass9(s : struct9) : int64_t; cdecl; external;
function pass10(s : struct10) : int64_t; cdecl; external;
function pass11(s : struct11) : int64_t; cdecl; external;
function pass12(s : struct12) : int64_t; cdecl; external;
function pass13(s : struct13) : int64_t; cdecl; external;
function pass14(s : struct14) : int64_t; cdecl; external;
function pass15(s : struct15) : int64_t; cdecl; external;
function pass31(s : struct31) : int64_t; cdecl; external;

function pass_arr1(s : struct_arr1) : int64_t; cdecl; external;
function pass_arr2(s : struct_arr2) : int64_t; cdecl; external;
function pass_arr3(s : struct_arr3) : int64_t; cdecl; external;
function pass_arr4(s : struct_arr4) : int64_t; cdecl; external;
function pass_arr5(s : struct_arr5) : int64_t; cdecl; external;
function pass_arr6(s : struct_arr6) : int64_t; cdecl; external;
function pass_arr7(s : struct_arr7) : int64_t; cdecl; external;
function pass_arr8(s : struct_arr8) : int64_t; cdecl; external;
function pass_arr9(s : struct_arr9) : int64_t; cdecl; external;
function pass_arr10(s : struct_arr10) : int64_t; cdecl; external;
function pass_arr11(s : struct_arr11) : int64_t; cdecl; external;
function pass_arr15(s : struct_arr15) : int64_t; cdecl; external;
function pass_arr16(s : struct_arr16) : int64_t; cdecl; external;
function pass_arr17(s : struct_arr17) : int64_t; cdecl; external;

function pass_arr27(s : struct_arr27) : int64_t; cdecl; external;

function pass_arr31(s : struct_arr31) : int64_t; cdecl; external;
function pass_arr32(s : struct_arr32) : int64_t; cdecl; external;
function pass_arr33(s : struct_arr33) : int64_t; cdecl; external;


procedure dotest;
var
  sa1 : struct_arr1;
  sa2 : struct_arr2;
  sa3 : struct_arr3;
  sa4 : struct_arr4;
  sa5 : struct_arr5;
  sa6 : struct_arr6;
  sa7 : struct_arr7;
  sa8 : struct_arr8;
  sa9 : struct_arr9;
  sa10 : struct_arr10;
  sa11 : struct_arr11;
  sa15 : struct_arr15;
  sa16 : struct_arr16;
  sa17 : struct_arr17;
  sa27 : struct_arr27;
  sa31 : struct_arr31;
  sa32 : struct_arr32;
  sa33 : struct_arr33;
  
  s1 : struct1;
  s2 : struct2;
  s3 : struct3;
  s4 : struct4;
  s5 : struct5;
  s6 : struct6;
  s7 : struct7;
  s8 : struct8;
  s9 : struct9;
  s10 : struct10;
  s11 : struct11;
  s12 : struct12;
  s13 : struct13;
  s14 : struct14;
  s15 : struct15;
  s31 : struct31;

begin
  randseed := 30;
  success := true;

  fill(s1, sizeof(s1));
  fill(s2, sizeof(s2));
  fill(s3, sizeof(s3));
  fill(s4, sizeof(s4));
  fill(s5, sizeof(s5));
  fill(s6, sizeof(s6));
  fill(s7, sizeof(s7));
  fill(s8, sizeof(s8));
  fill(s9, sizeof(s9));
  fill(s10, sizeof(s10));
  fill(s11, sizeof(s11));
  fill(s12, sizeof(s12));
  fill(s13, sizeof(s13));
  fill(s14, sizeof(s14));
  fill(s15, sizeof(s15));
  fill(s31, sizeof(s31));

  fill(sa1, sizeof(sa1));
  fill(sa2, sizeof(sa2));
  fill(sa3, sizeof(sa3));
  fill(sa4, sizeof(sa4));
  fill(sa5, sizeof(sa5));
  fill(sa6, sizeof(sa6));
  fill(sa7, sizeof(sa7));
  fill(sa8, sizeof(sa8));
  fill(sa9, sizeof(sa9));
  fill(sa10, sizeof(sa10));
  fill(sa11, sizeof(sa11));
  fill(sa15, sizeof(sa15));
  fill(sa16, sizeof(sa16));
  fill(sa17, sizeof(sa17));
  fill(sa27, sizeof(sa27));
  fill(sa31, sizeof(sa31));
  fill(sa32, sizeof(sa32));
  fill(sa33, sizeof(sa33));


  verify(pass1(s1), check1(s1), 1);
  verify(pass2(s2), check2(s2), 2);
  verify(pass3(s3), check3(s3), 3);
  verify(pass4(s4), check4(s4), 4);
  verify(pass5(s5), check5(s5), 5);
  verify(pass6(s6), check6(s6), 6);
  verify(pass7(s7), check7(s7), 7);
  verify(pass8(s8), check8(s8), 8);
  verify(pass9(s9), check9(s9), 9);
  verify(pass10(s10), check10(s10), 10);
  verify(pass11(s11), check11(s11), 11);
  verify(pass12(s12), check12(s12), 12);
  verify(pass13(s13), check13(s13), 13);
  verify(pass14(s14), check14(s14), 14);
  verify(pass15(s15), check15(s15), 15);
  verify(pass31(s31), check31(s31), 31);

  verify(pass_arr1(sa1), check_arr1(sa1), 101);
  verify(pass_arr2(sa2), check_arr2(sa2), 102);
  verify(pass_arr3(sa3), check_arr3(sa3), 103);
  verify(pass_arr4(sa4), check_arr4(sa4), 104);
  verify(pass_arr5(sa5), check_arr5(sa5), 105);
  verify(pass_arr6(sa6), check_arr6(sa6), 106);
  verify(pass_arr7(sa7), check_arr7(sa7), 107);
  verify(pass_arr8(sa8), check_arr8(sa8), 108);
  verify(pass_arr9(sa9), check_arr9(sa9), 109);
  verify(pass_arr10(sa10), check_arr10(sa10), 110);
  verify(pass_arr11(sa11), check_arr11(sa11), 111);
  verify(pass_arr15(sa15), check_arr15(sa15), 115);
  verify(pass_arr16(sa16), check_arr16(sa16), 116);
  verify(pass_arr17(sa17), check_arr17(sa17), 117);
  verify(pass_arr27(sa27), check_arr27(sa27), 127);

  verify(pass_arr31(sa31), check_arr31(sa31), 131);
  verify(pass_arr32(sa32), check_arr32(sa32), 132);
  verify(pass_arr33(sa33), check_arr33(sa33), 133);

  if (not success) then
    halt(1);
end;

begin
  dotest;
end.
