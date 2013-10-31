{
    This file isolates platform-specific routines which perform packing and
    unpacking of ValReal FP values during float <-> ASCII conversions.
    These routines, mostly, were gathered from various places of FPC RTL.

 ****************************************************************************
}
{
    Note about inlining: since unpack_float is used only once in str_real,
    it can be safely inlined; however pack_float is used several times in
    val_real, so its inlining does not seem practical, except of the case
    when this procedure simply calls the SoftFPU implementation.
}
// ---------------------------------------------------------------------
//
// single; format [MSB]: 1 sign bit, 8 bit exponent, 23 bit mantissa
//
// ---------------------------------------------------------------------
{$if defined(VALREAL_32) and not defined(VALREAL_PACK)}
{$if defined(fpc_softfpu_implementation)
     or ( defined(FPC_SYSTEM_HAS_extractFloat32Frac)
      and defined(FPC_SYSTEM_HAS_extractFloat32Exp)
      and defined(FPC_SYSTEM_HAS_extractFloat32Sign)
        )}

function unpack_float( const f: ValReal; out minus: boolean ): TDIY_FP; {$ifdef grisu1_inline}inline;{$endif}
begin
    unpack_float.f := extractFloat32Frac( float32( f ) );
    unpack_float.e := extractFloat32Exp( float32( f ) );
    minus := ( extractFloat32Sign( float32( f ) ) <> 0 );
end;

{$else not fpc_softfpu_implementation}

function unpack_float( const f: ValReal; out minus: boolean ): TDIY_FP; {$ifdef grisu1_inline}inline;{$endif}
type
    TSplitFloat = packed record
      case byte of
        0: ( f: ValReal );
        1: ( b: array [ 0 .. 3 ] of byte );
        2: ( w: array [ 0 .. 1 ] of word );
        3: ( d: dword );
    end;
var
    split: TSplitFloat;
begin
    split.f := f;
 {$ifdef endian_big}
    minus := ( split.b[0] and $80 <> 0 );
    unpack_float.e := ( split.w[0] shr 7 ) and $FF;
 {$else endian_little}
    minus := ( split.b[3] and $80 <> 0 );
    unpack_float.e := ( split.w[1] shr 7 ) and $FF;
 {$endif endian}
    unpack_float.f := split.d and $007FFFFF;
end;

{$endif fpc_softfpu_implementation}
{$endif unpack float32}

{$if defined(VALREAL_32) and defined(VALREAL_PACK)}
{$ifdef fpc_softfpu_implementation}

procedure pack_float( out f: ValReal; minus: boolean; exp: integer; const m: dword ); {$ifdef grisu1_inline}inline;{$endif}
begin
    f := float32rec( packFloat32( ord(minus), exp, m ) );
end;

{$else not fpc_softfpu_implementation}

procedure pack_float( out f: ValReal; minus: boolean; exp: integer; m: dword ); // {$ifdef grisu1_inline}inline;{$endif}
type
    TSplitFloat = packed record
      case byte of
        0: ( f: ValReal );
        1: ( b: array [ 0 .. 3 ] of byte );
        2: ( w: array [ 0 .. 1 ] of word );
        3: ( d: dword );
    end;
var
    split: TSplitFloat;
begin
    split.d := m;
 {$ifdef endian_big}
    split.w[0] := split.w[0] + ( exp and $FF ) shl 7;
    if minus then
        split.b[0] := split.b[0] or $80;
 {$else endian_little}
    split.w[1] := split.w[1] + ( exp and $FF ) shl 7;
    if minus then
        split.b[3] := split.b[3] or $80;
 {$endif endian}
    f := split.f;
end;

{$endif fpc_softfpu_implementation}
{$endif pack float32}

// ---------------------------------------------------------------------
//
// double; format [MSB]: 1 sign bit, 11 bit exponent, 52 bit mantissa
//
// ---------------------------------------------------------------------
{$if defined(VALREAL_64) and not defined(VALREAL_PACK)}
{$ifdef cpujvm}

function unpack_float( const f: ValReal; out minus: boolean ): TDIY_FP; {$ifdef grisu1_inline}inline;{$endif}
var
    doublebits: int64;
begin
    doublebits := JLDouble.doubleToLongBits( f );
    minus := ( doublebits < 0 );
    unpack_float.e := ( doublebits shr 52 ) and $7FF;
    unpack_float.f := ( doublebits and $000FFFFFFFFFFFFF );
end;

{$else not cpujvm}

{$if defined(fpc_softfpu_implementation)
     or ( defined(FPC_SYSTEM_HAS_extractFloat64Frac)
      and defined(FPC_SYSTEM_HAS_extractFloat64Exp)
      and defined(FPC_SYSTEM_HAS_extractFloat64Sign)
        )}

function unpack_float( const f: ValReal; out minus: boolean ): TDIY_FP; {$ifdef grisu1_inline}inline;{$endif}
begin
    unpack_float.f := extractFloat64Frac( f );
    unpack_float.e := extractFloat64Exp( f );
    minus := ( extractFloat64Sign( f ) <> 0 );
end;

{$else not fpc_softfpu_implementation}

function unpack_float( const f: ValReal; out minus: boolean ): TDIY_FP; {$ifdef grisu1_inline}inline;{$endif}
type
    TSplitFloat = packed record
      case byte of
        0: ( f: ValReal );
        1: ( b: array [ 0 .. 7 ] of byte );
        2: ( w: array [ 0 .. 3 ] of word );
        3: ( d: array [ 0 .. 1 ] of dword );
        4: ( l: qword );
    end;
var
    doublebits: TSplitFloat;
begin
  {$ifdef FPC_DOUBLE_HILO_SWAPPED}
    // high and low dword are swapped when using the arm fpa
    doublebits.d[0] := TSplitFloat(f).d[1];
    doublebits.d[1] := TSplitFloat(f).d[0];
  {$else not FPC_DOUBLE_HILO_SWAPPED}
    doublebits.f := f;
  {$endif FPC_DOUBLE_HILO_SWAPPED}
  {$ifdef endian_big}
    minus := ( doublebits.b[0] and $80 <>0 );
    unpack_float.e := ( doublebits.w[0] shr 4 ) and $7FF;
  {$else endian_little}
    minus := ( doublebits.b[7] and $80 <> 0 );
    unpack_float.e := ( doublebits.w[3] shr 4 ) and $7FF;
  {$endif endian}
    unpack_float.f := doublebits.l and $000FFFFFFFFFFFFF;
end;

{$endif fpc_softfpu_implementation}
{$endif cpujvm}
{$endif unpack float64}

{$if defined(VALREAL_64) and defined(VALREAL_PACK)}
{$ifdef cpujvm}

procedure pack_float( out f: ValReal; minus: boolean; exp: integer; const m: qword ); {$ifdef grisu1_inline}inline;{$endif}
var
    doublebits: int64;
begin
    doublebits := ( m and $000FFFFFFFFFFFFF ) + ( qword( exp and $7FF ) shl 52 ) + ( qword( ord(minus) ) shl 63 );
    f := JLDouble.longBitsToDouble( doublebits );
end;

{$else not cpujvm}

{$ifdef fpc_softfpu_implementation}

procedure pack_float( out f: ValReal; minus: boolean; exp: integer; const m: qword ); {$ifdef grisu1_inline}inline;{$endif}
begin
    f := packFloat64( ord(minus), exp, m );
end;

{$else not fpc_softfpu_implementation}

procedure pack_float( out f: ValReal; minus: boolean; exp: integer; const m: qword ); // {$ifdef grisu1_inline}inline;{$endif}
type
    TSplitFloat = packed record
      case byte of
        0: ( f: ValReal );
        1: ( b: array [ 0 .. 7 ] of byte );
        2: ( w: array [ 0 .. 3 ] of word );
        3: ( d: array [ 0 .. 1 ] of dword );
        4: ( l: qword );
    end;
var
    doublebits: TSplitFloat;
begin
    doublebits.l := m;
  {$ifdef endian_big}
    doublebits.w[0] := doublebits.w[0] + ( exp and $7FF ) shl 4;
    if minus then
        doublebits.b[0] := doublebits.b[0] or $80;
  {$else endian_little}
    doublebits.w[3] := doublebits.w[3] + ( exp and $7FF ) shl 4;
    if minus then
        doublebits.b[7] := doublebits.b[7] or $80;
  {$endif endian}
  {$ifdef FPC_DOUBLE_HILO_SWAPPED}
    // high and low dword are swapped when using the arm fpa
    TSplitFloat(f).d[1] := doublebits.d[0];
    TSplitFloat(f).d[0] := doublebits.d[1];
  {$else not FPC_DOUBLE_HILO_SWAPPED}
    f := doublebits.f;
  {$endif FPC_DOUBLE_HILO_SWAPPED}
end;

{$endif fpc_softfpu_implementation}
{$endif cpujvm}
{$endif pack float64}

// ---------------------------------------------------------------------
//
// extended; format [MSB]: 1 Sign bit, 15 bit exponent, 64 bit mantissa (explicit integer bit is included)
//
// ---------------------------------------------------------------------
{$if defined(VALREAL_80) and not defined(VALREAL_PACK)}
{$ifdef fpc_softfpu_implementation}

function unpack_float( const f: ValReal; out minus: boolean ): TDIY_FP; {$ifdef grisu1_inline}inline;{$endif}
begin
    unpack_float.fh := 0;
    unpack_float.f := extractFloatx80Frac( f );
    unpack_float.e := extractFloatx80Exp( f );
    minus := ( extractFloatx80Sign( f ) <> 0 );
end;

{$else not fpc_softfpu_implementation}

function unpack_float( const f: ValReal; out minus: boolean ): TDIY_FP; {$ifdef grisu1_inline}inline;{$endif}
type
    TSplitFloat = packed record
      case byte of
        0: ( f: ValReal );
        1: ( b: array [ 0 .. 9 ] of byte );
        2: ( l: qword; e: word )
    end;
var
    split: TSplitFloat;
begin
    split.f := f;
  {$ifdef endian_big}
    {$error Big endian extended double [80-bit] is not implemented}
  {$else endian_little}
    minus := ( split.b[9] and $80 <> 0 );
    unpack_float.e := split.e and $7FFF;
    unpack_float.f := split.l;
    unpack_float.fh := 0;
  {$endif endian}
end;

{$endif fpc_softfpu_implementation}
{$endif unpack floatx80}

{$if defined(VALREAL_80) and defined(VALREAL_PACK)}
{$ifdef fpc_softfpu_implementation}

procedure pack_float( out f: ValReal; minus: boolean; exp: integer; const m: qword ); {$ifdef grisu1_inline}inline;{$endif}
begin
    f := packFloatx80( ord(minus), exp, m );
end;

{$else not fpc_softfpu_implementation}

procedure pack_float( out f: ValReal; minus: boolean; exp: integer; const m: qword ); // {$ifdef grisu1_inline}inline;{$endif}
type
    TSplitFloat = packed record
      case byte of
        0: ( f: ValReal );
        1: ( b: array [ 0 .. 9 ] of byte );
        2: ( l: qword; e: word )
    end;
var
    split: TSplitFloat;
begin
  {$ifdef endian_big}
    {$error Big endian extended double [80-bit] is not implemented}
  {$else endian_little}
    split.l := m;
    split.e := exp and $7FFF;
    if minus then
        split.b[9] := split.b[9] or $80;
  {$endif endian}
    f := split.f;
end;

{$endif fpc_softfpu_implementation}
{$endif pack floatx80}

// ---------------------------------------------------------------------
//
// float128; format [MSB]: 1 Sign bit, 15 bit exponent, 112 bit mantissa
//
// ---------------------------------------------------------------------
{$if defined(VALREAL_128) and not defined(VALREAL_PACK)}
{$ifdef fpc_softfpu_implementation}

function unpack_float( const f: ValReal; out minus: boolean ): TDIY_FP; {$ifdef grisu1_inline}inline;{$endif}
begin
    unpack_float.fh := extractFloat128Frac0( f );
    unpack_float.f := extractFloat128Frac1( f );
    unpack_float.e := extractFloat128Exp( f );
    minus := ( extractFloat128Sign( f ) <> 0 );
end;

{$else not fpc_softfpu_implementation}

function unpack_float( const f: ValReal; out minus: boolean ): TDIY_FP; {$ifdef grisu1_inline}inline;{$endif}
type
    TSplitFloat = packed record
      case byte of
        0: ( f: ValReal );
        1: ( b: array [ 0 .. 15 ] of byte );
        2: ( w: array [ 0 .. 7 ] of word );
        3: ( l: array [ 0 .. 1 ] of qword );
    end;
var
    split: TSplitFloat;
begin
    split.f := f;
  {$ifdef endian_big}
    {$error Big endian long double [128-bit] is not implemented}
  {$else endian_little}
    minus := ( split.b[15] and $80 <> 0 );
    unpack_float.e := split.w[7] and $7FFF;
    unpack_float.f := split.l[0];
    unpack_float.fh := split.l[1] and $0000FFFFFFFFFFFF;
  {$endif endian}
end;

{$endif fpc_softfpu_implementation}
{$endif unpack float128}

{$if defined(VALREAL_128) and defined(VALREAL_PACK)}
{$ifdef fpc_softfpu_implementation}

procedure pack_float( out f: ValReal; minus: boolean; exp: integer; const h, l: qword ); {$ifdef grisu1_inline}inline;{$endif}
begin
    f := packFloat128( ord(minus), exp, h, l );
end;

{$else not fpc_softfpu_implementation}

procedure pack_float( out f: ValReal; minus: boolean; exp: integer; const h, l: qword ); // {$ifdef grisu1_inline}inline;{$endif}
type
    TSplitFloat = packed record
      case byte of
        0: ( f: ValReal );
        1: ( b: array [ 0 .. 15 ] of byte );
        2: ( w: array [ 0 .. 7 ] of word );
        3: ( l: array [ 0 .. 1 ] of qword );
    end;
var
    split: TSplitFloat;
begin
  {$ifdef endian_big}
    {$error Big endian long double [128-bit] is not implemented}
  {$else endian_little}
    split.l[0] := l;
    split.l[1] := h;
    split.w[7] := exp and $7FFF;
    if minus then
        split.b[15] := split.b[15] or $80;
  {$endif endian}
    f := split.f;
end;

{$endif fpc_softfpu_implementation}
{$endif pack float128}
