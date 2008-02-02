(*
  $Id: math.inc 25 2007-12-10 21:06:46Z p4p3r0 $
  ------------------------------------------------------------------------------
  	Copyright (C) 2005
  		Jason Rogers (dovoto)
  		Dave Murphy (WinterMute)
  	
  	This software is provided 'as-is', without any express or implied
  	warranty.  In no event will the authors be held liable for any
  	damages arising from the use of this software.
  	
  	Permission is granted to anyone to use this software for any
  	purpose, including commercial applications, and to alter it and
  	redistribute it freely, subject to the following restrictions:
  	
  	1.	The origin of this software must not be misrepresented; you
  		must not claim that you wrote the original software. If you use
  		this software in a product, an acknowledgment in the product
  		documentation would be appreciated but is not required.
  	
  	2.	Altered source versions must be plainly marked as such, and
  		must not be misrepresented as being the original software.
  	
  	3.	This notice may not be removed or altered from any source
  		distribution.
  ------------------------------------------------------------------------------
    
    
  Conversion by Legolas (http://itaprogaming.free.fr) for freepascal compiler
  (http://www.freepascal.org)
   
  Copyright (C) 2006  Francesco Lombardi
  Check http://sourceforge.net/projects/libndsfpc for updates
   
  ------------------------------------------------------------------------------

  $Log$

*)

{$ifdef NDS_INTERFACE}
//  Math coprocessor register definitions
const
  DIV_CR				    : pcuint16 = pointer($04000280);
  DIV_NUMERATOR64		: pcint64 = pointer($04000290);
  DIV_NUMERATOR32		: pcint32 = pointer($04000290);
  DIV_DENOMINATOR64	: pcint64 = pointer($04000298);
  DIV_DENOMINATOR32	: pcint32 = pointer($04000298);
  DIV_RESULT64		  : pcint64 = pointer($040002A0);
  DIV_RESULT32		  : pcint32 = pointer($040002A0);
  DIV_REMAINDER64		: pcint64 = pointer($040002A8);
  DIV_REMAINDER32		: pcint32 = pointer($040002A8);

  SQRT_CR           : pcuint16 = pointer($040002B0);
  SQRT_PARAM64      : pcint64 = pointer($040002B8);
  SQRT_RESULT32     : pcint32 = pointer($040002B4);
  SQRT_PARAM32      : pcint32 = pointer($040002B8);

//  Math coprocessor modes

  DIV_64_64		= 2;
  DIV_64_32		= 1;
  DIV_32_32		= 0;
  DIV_BUSY		= (1 shl 15);

  SQRT_64			= 1;
  SQRT_32			= 0;
  SQRT_BUSY		= (1 shl 15);
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
function divf32(num: cint32; den: cint32): cint32; inline;
begin
	DIV_CR^ := DIV_64_32;

	while (DIV_CR^ and DIV_BUSY) <> 0 do;

	DIV_NUMERATOR64^ := cint64(num) shl 12;
	DIV_DENOMINATOR32^ := den;

	while (DIV_CR^ and DIV_BUSY) <> 0 do;

	divf32 := DIV_RESULT32^;
end;

function mulf32(a, b: cint32): cint32; inline;
var
  rslt: clonglong;
begin
	rslt := clonglong(a) * clonglong(b);
	mulf32 := cint32(rslt shr 12);
end;

//  Fixed point square root
//	Takes 1.19.12 fixed point value and
//	returns the fixed point result
function sqrtf32(a: cint32): cint32; inline;
begin
	SQRT_CR^ := SQRT_64;

	while (SQRT_CR^ and SQRT_BUSY) <> 0 do;

	SQRT_PARAM64^ := cint64(a) shl 12;

	while (SQRT_CR^ and SQRT_BUSY) <> 0 do;

	sqrtf32 := SQRT_RESULT32^;
end;

//  Integer versions

//  Integer divide
//  Takes a 32 bit numerator and 32 bit
//	denominator and returns 32 bit result
function div32(num, den: cint32): cint32; inline;
begin
	DIV_CR^ := DIV_32_32;

	while (DIV_CR^ and DIV_BUSY) <> 0 do;

	DIV_NUMERATOR32^ := num;
	DIV_DENOMINATOR32^ := den;

	while (DIV_CR^ and DIV_BUSY) <> 0 do;

	div32 := DIV_RESULT32^;
end;

//  Integer divide
//  Takes a 32 bit numerator and 32 bit
//	denominator and returns 32 bit result
function mod32(num, den: cint32): cint32; inline;
begin
	DIV_CR^ := DIV_32_32;

	while (DIV_CR^ and DIV_BUSY) <> 0 do;

	DIV_NUMERATOR32^ := num;
	DIV_DENOMINATOR32^ := den;

	while (DIV_CR^ and DIV_BUSY) <> 0 do;

	mod32 := DIV_REMAINDER32^;
end;

//  Integer divide
//	Takes a 64 bit numerator and 32 bit
//  denominator are returns 32 bit result
function div64(num: cint64; den: cint32): cint32; inline;
begin
	DIV_CR^ := DIV_64_32;

	while (DIV_CR^ and DIV_BUSY) <> 0 do;

	DIV_NUMERATOR64^ := num;
	DIV_DENOMINATOR32^ := den;

	while (DIV_CR^ and DIV_BUSY) <> 0 do;

	div64 := DIV_RESULT32^;
end;

//  Integer divide
//	Takes a 64 bit numerator and 32 bit
//  denominator are returns 32 bit result
function mod64(num: cint64; den: cint32): cint32; inline;
begin
	DIV_CR^ := DIV_64_32;

	while (DIV_CR^ and DIV_BUSY) <> 0 do;

	DIV_NUMERATOR64^ := num;
	DIV_DENOMINATOR32^ := den;

	while (DIV_CR^ and DIV_BUSY) <> 0 do;

	mod64 := DIV_REMAINDER32^;
end;

//  Integer square root
//  takes a 32 bit integer and returns
//	32 bit result
function sqrt32(a: cint32): cint32; inline;
begin
	SQRT_CR^ := SQRT_32;

	while(SQRT_CR^ and SQRT_BUSY) <> 0 do;

	SQRT_PARAM32^ := a;

	while(SQRT_CR^ and SQRT_BUSY) <> 0 do;

	sqrt32 := SQRT_RESULT32^;
end;

//  Trig Functions  1.19.12 fixed point

// Cross product
// x = Ay * Bz - By * Az
// y = Az * Bx - Bz * Ax
// z = Ax * By - Bx * Ay
procedure crossf32(a: pcint32; b: pcint32; res: pcint32); inline;
begin
	res[0] := mulf32(a[1], b[2]) - mulf32(b[1], a[2]);
	res[1] := mulf32(a[2], b[0]) - mulf32(b[2], a[0]);
	res[2] := mulf32(a[0], b[1]) - mulf32(b[0], a[1]);
end;

// Dot Product
// result = Ax * Bx + Ay * By + Az * Bz
function dotf32(a, b: pcint32): cint32; inline;
begin
	dotf32 := mulf32(a[0], b[0]) + mulf32(a[1], b[1]) + mulf32(a[2], b[2]);
end;

// Normalize
// Ax = Ax / mag
// Ay = Ay / mag
// Az = Az / mag
procedure normalizef32(a: pcint32); inline;
var
  magnitude: cint32;
begin
	// magnitude = sqrt ( Ax^2 + Ay^2 + Az^2 )
	magnitude := sqrtf32( mulf32(a[0], a[0]) + mulf32(a[1], a[1]) + mulf32(a[2], a[2]) );

	a[0] := divf32(a[0], magnitude);
	a[1] := divf32(a[1], magnitude);
	a[2] := divf32(a[2], magnitude);
end;
{$endif NDS_IMPLEMENTATION}

{$ifdef NDS_INTERFACE}
function divf32(num: cint32; den: cint32): cint32; inline;
function mulf32(a, b: cint32): cint32; inline;
function sqrtf32(a: cint32): cint32; inline;
function div32(num, den: cint32): cint32; inline;
function mod32(num, den: cint32): cint32; inline;
function div64(num: cint64; den: cint32): cint32; inline;
function mod64(num: cint64; den: cint32): cint32; inline;
function sqrt32(a: cint32): cint32; inline;
procedure crossf32(a: pcint32; b: pcint32; res: pcint32); inline;
function dotf32(a, b: pcint32): cint32; inline;
procedure normalizef32(a: pcint32); inline;
{$endif NDS_INTERFACE}
