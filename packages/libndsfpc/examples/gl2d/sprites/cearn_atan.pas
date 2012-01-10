(*
  coranac.com's awesome atan2 implementation
	Very fast and very small.
*)
unit cearn_atan;

{$mode objfpc}
{$H+}

interface

uses
  ctypes, nds9;
  
const 
  __qran_seed: cint = 42;

function atan2Lookup(x, y: cint): cuint32;
function atan2Lerp(x, y: cint): cuint32;
function sqran(seed: cint): cint;
function qran(): cint; inline;
function qran_range(min, max: cint): cint; inline;	

const
  BRAD_PI_SHIFT = 14;   
  BRAD_PI = 1 shl BRAD_PI_SHIFT;
  BRAD_HPI = BRAD_PI div 2; 
  BRAD_2PI = BRAD_PI * 2;
  ATAN_ONE = $1000; 
  ATAN_FP = 12;

  // Some constants for dealing with atanLUT.
  ATANLUT_STRIDE = ATAN_ONE div $80; 
  ATANLUT_STRIDE_SHIFT = 5;

  // Arctangents LUT. Interval: [0, 1] (one=128); PI=0x20000
  atanLUT: array [0..130-1] of cushort = (
    $0000, $0146, $028C, $03D2, $0517, $065D, $07A2, $08E7,
    $0A2C, $0B71, $0CB5, $0DF9, $0F3C, $107F, $11C1, $1303,
    $1444, $1585, $16C5, $1804, $1943, $1A80, $1BBD, $1CFA,
    $1E35, $1F6F, $20A9, $21E1, $2319, $2450, $2585, $26BA,
    $27ED, $291F, $2A50, $2B80, $2CAF, $2DDC, $2F08, $3033,
    $315D, $3285, $33AC, $34D2, $35F6, $3719, $383A, $395A,
    $3A78, $3B95, $3CB1, $3DCB, $3EE4, $3FFB, $4110, $4224,
    $4336, $4447, $4556, $4664, $4770, $487A, $4983, $4A8B,
    // 64
    $4B90, $4C94, $4D96, $4E97, $4F96, $5093, $518F, $5289,
    $5382, $5478, $556E, $5661, $5753, $5843, $5932, $5A1E,
    $5B0A, $5BF3, $5CDB, $5DC1, $5EA6, $5F89, $606A, $614A,
    $6228, $6305, $63E0, $64B9, $6591, $6667, $673B, $680E,
    $68E0, $69B0, $6A7E, $6B4B, $6C16, $6CDF, $6DA8, $6E6E,
    $6F33, $6FF7, $70B9, $717A, $7239, $72F6, $73B3, $746D,
    $7527, $75DF, $7695, $774A, $77FE, $78B0, $7961, $7A10,
    $7ABF, $7B6B, $7C17, $7CC1, $7D6A, $7E11, $7EB7, $7F5C,
    // 128
    $8000, $80A2);

implementation

// Quick (and very dirty) pseudo-random number generator 
// return random in range [0,8000h>
function qran(): cint; inline;
begin	
  __qran_seed := 1664525 * __qran_seed + 1013904223;
  result := (__qran_seed shr 16) and $7FFF;
end;

function qran_range(min, max: cint): cint; inline;	
begin    
  result := (qran() * (max - min) shr 15) + min;
end;

// Get the octant a coordinate pair is in.
procedure OCTANTIFY(var _x, _y, _o: cint); inline;
var
  _t: cint;
begin
  repeat                             
    _o := 0;                           
    if (_y < 0)  then
    begin
      _x := -_x;   
      _y := -_y; 
      _o := _o + 4; 
    end;     
    if (_x <= 0) then
    begin
      _t := _x;
      _x := _y;   
      _y := -_t; 
      _o := _o + 2; 
    end;     
    if (_x <= _y) then
    begin
      _t := _y - _x; 
      _x := _x + _y; 
      _y := _t; 
      _o := _o + 1; 
    end;     
  until true;
end;

function QDIV(num, den: cint; const bits: cint): cint; inline;
begin
  while (REG_DIVCNT^ and DIV_BUSY) <> 0 do;
  REG_DIVCNT^ := DIV_64_32;

  REG_DIV_NUMER^ := cint64(num) shl bits;
  REG_DIV_DENOM_L^ := den;

  while (REG_DIVCNT^ and DIV_BUSY) <> 0 do;

  result := REG_DIV_RESULT_L^;
end;

function atan2Lerp(x, y: cint): cuint32;
var
  phi: cint;
  t, fa, fb, h: cuint32;
begin
  if (y =0) then 
  begin
    if x >= 0 then
      result := 0
    else
      result := BRAD_PI;
    exit;
  end;
  
  OCTANTIFY(x, y, phi);
  phi := phi * BRAD_PI div 4;
  
  t := QDIV(y, x, ATAN_FP);
  h := t mod ATANLUT_STRIDE;
  
  fa := atanLUT[t div ATANLUT_STRIDE];
  fb := atanLUT[t div ATANLUT_STRIDE + 1];
  
  result := phi + (fa + SarLongint((fb - fa) * h, ATANLUT_STRIDE_SHIFT)) div 8;
end;

function atan2Lookup(x, y: cint): cuint32;
var
  phi: cint;
  t: cuint32;
begin
  if (y = 0) then
  begin
    if x >= 0 then 
      result := 0 
    else 
      result := BRAD_PI;
    exit;
  end;
  
  OCTANTIFY(x, y, phi);
  phi := phi * BRAD_PI div 4;
  t := QDIV(y, x, ATAN_FP);
  
  result := phi + atanLUT[t div ATANLUT_STRIDE] div 8;
end;

function sqran(seed: cint): cint;
var
  old: cint;
begin	
  old := __qran_seed;
  __qran_seed := seed; 
  result := old;	
end;

end.




