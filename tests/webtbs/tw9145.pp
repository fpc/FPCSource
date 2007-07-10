{ %norun }

program main;
{$mode objfpc}
uses
  ctypes;

const
  // CoProcessor registers
  DIV_CR				    : pcuint16 = pointer($04000280);
  DIV_NUMERATOR64		: pcint64  = pointer($04000290);
  DIV_DENOMINATOR32	: pcint32  = pointer($04000298);
  DIV_RESULT32		  : pcint32  = pointer($040002A0);
  DIV_64_32 = 1;
  DIV_BUSY	= (1 shl 15);

function mydivf32(num: cint32; den: cint32): cint32; inline;
begin
	DIV_CR^ := DIV_64_32;
	while (DIV_CR^ and DIV_BUSY) <> 0 do;
	DIV_NUMERATOR64^ := cint64(num) shl 12;
  DIV_DENOMINATOR32^ := den;
	while (DIV_CR^ and DIV_BUSY) <> 0 do;
	mydivf32 := DIV_RESULT32^;
end;

var
  a: cint32;
begin
  a := mydivf32(10, 2);
end.
