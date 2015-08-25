{
  Program to test linking between C and pascal units.

  Pascal counter part
}

unit ptest;

interface

uses
  ctypes;
	
{ Use C alignment of records }
{$PACKRECORDS C}
const
   RESULT_U8BIT = $55;
   RESULT_U16BIT = $500F;
   RESULT_U32BIT = $500F0000;
   RESULT_U64BIT = $1BCDABCD;
   RESULT_S16BIT = -12;
   RESULT_S32BIT = -120;
   RESULT_S64BIT = -12000;
{$ifndef FPUNONE}
   RESULT_FLOAT  = 14.54;
   RESULT_DOUBLE = 15.54;
   RESULT_LONGDOUBLE = 16.54;
{$endif FPU_NONE}
   RESULT_PCHAR  = 'Hello world';

type
 _1byte_ = record
  u8 : byte;
 end;

 _3byte_ = record
  u8 : byte;
  u16 : word;
 end;

 _3byte_s = record
  u16 : word;
  w8 : byte;
 end;

 _5byte_ = record
  u8 : byte;
  u32 : cardinal;
 end;

_7byte_ = record
  u8: byte;
  s64: int64;
  u16: word;
end;
  byte_array = array [0..1] of byte;
  word_array = array [0..1] of word;
  cardinal_array = array [0..1] of cardinal;
  qword_array = array [0..1] of qword;
  smallint_array = array [0..1] of smallint;
  longint_array = array [0..1] of longint;
  int64_array = array [0..1] of int64;
{$ifndef FPUNONE}
  single_array = array [0..1] of single;
  double_array = array [0..1] of double;
  clongdouble_array = array [0..1] of clongdouble;
{$endif FPUNONE}

var
  global_u8bit : byte; cvar;
  global_u16bit : word; cvar;
  global_u32bit : cardinal; cvar;
  global_u64bit : qword; cvar;
  global_s16bit : smallint; cvar;
  global_s32bit : longint; cvar;
  global_s64bit : int64; cvar;
{$ifndef FPUNONE}
  global_float : single; cvar;
  global_double : double; cvar;
  global_long_double : clongdouble; cvar;
{$endif FPUNONE}

{ simple parameter passing }
procedure test_param_u8(x: byte); cdecl; public;
procedure test_param_u16(x : word); cdecl; public;
procedure test_param_u32(x: cardinal); cdecl; public;
procedure test_param_u64(x: qword); cdecl; public;
procedure test_param_s16(x : smallint); cdecl; public;
procedure test_param_s32(x: longint); cdecl; public;
procedure test_param_s64(x: int64); cdecl; public;
{$ifndef FPUNONE}
procedure test_param_float(x : single); cdecl; public;
procedure test_param_double(x: double); cdecl; public;
procedure test_param_longdouble(x: clongdouble); cdecl; public;
{$endif FPUNONE}
procedure test_param_var_u8(var x: byte); cdecl; public;

{ array parameter passing }
procedure test_array_param_u8(x: byte_array); cdecl; public;
procedure test_array_param_u16(x : word_array); cdecl; public;
procedure test_array_param_u32(x: cardinal_array); cdecl; public;
procedure test_array_param_u64(x: qword_array); cdecl; public;
procedure test_array_param_s16(x :smallint_array); cdecl; public;
procedure test_array_param_s32(x: longint_array); cdecl; public;
procedure test_array_param_s64(x: int64_array); cdecl; public;
{$ifndef FPUNONE}
procedure test_array_param_float(x : single_array); cdecl; public;
procedure test_array_param_double(x: double_array); cdecl; public;
procedure test_array_param_longdouble(x: clongdouble_array); cdecl; public;
{$endif FPUNONE}

{ mixed parameter passing }
procedure test_param_mixed_u16(z: byte; x : word; y :byte); cdecl; public;
procedure test_param_mixed_u32(z: byte; x: cardinal; y: byte); cdecl; public;
procedure test_param_mixed_s64(z: byte; x: int64; y: byte); cdecl; public;
{$ifndef FPUNONE}
procedure test_param_mixed_float(x: single; y: byte); cdecl; public;
procedure test_param_mixed_double(x: double; y: byte); cdecl; public;
procedure test_param_mixed_long_double(x: clongdouble; y: byte); cdecl; public;
{$endif FPUNONE}
procedure test_param_mixed_var_u8(var x: byte;y:byte); cdecl; public;
{ structure parameter testing }
procedure test_param_struct_tiny(buffer :   _1BYTE_); cdecl; public;
procedure test_param_struct_small(buffer :  _3BYTE_); cdecl; public;
procedure test_param_struct_small_s(buffer :  _3BYTE_S); cdecl; public;
procedure test_param_struct_medium(buffer : _5BYTE_); cdecl; public;
procedure test_param_struct_large(buffer :  _7BYTE_); cdecl; public;
{ mixed with structure parameter testing }
procedure test_param_mixed_struct_tiny(buffer :   _1BYTE_; y :byte); cdecl; public;
procedure test_param_mixed_struct_small(buffer :  _3BYTE_; y :byte); cdecl; public;
procedure test_param_mixed_struct_small_s(buffer :  _3BYTE_S; y :byte); cdecl; public;
procedure test_param_mixed_struct_medium(buffer : _5BYTE_; y :byte); cdecl; public;
procedure test_param_mixed_struct_large(buffer :  _7BYTE_; y :byte); cdecl; public;
{ function result value testing }
function test_function_u8: byte; cdecl; public;
function test_function_u16: word; cdecl; public;
function test_function_u32: cardinal; cdecl; public;
function test_function_u64: qword; cdecl; public;
function test_function_s16: smallint; cdecl; public;
function test_function_s32: longint; cdecl; public;
function test_function_s64: int64; cdecl; public;
function test_function_pchar: pchar; cdecl; public;
{$ifndef FPUNONE}
function test_function_float : single; cdecl; public;
function test_function_double : double; cdecl; public;
function test_function_longdouble: clongdouble; cdecl; public;
{$endif FPUNONE}
function test_function_tiny_struct : _1byte_; cdecl; public;
function test_function_small_struct : _3byte_; cdecl; public;
function test_function_small_struct_s : _3byte_s; cdecl; public;
function test_function_medium_struct : _5byte_; cdecl; public;
function test_function_struct : _7byte_; cdecl; public;


implementation

{ simple parameter passing }
procedure test_param_u8(x: byte); cdecl; public;
  begin
    global_u8bit:=x;
  end;

procedure test_param_u16(x : word); cdecl; public;
  begin
    global_u16bit:=x;
  end;

procedure test_param_u32(x: cardinal); cdecl; public;
  begin
    global_u32bit:=x;
  end;

procedure test_param_u64(x: qword); cdecl; public;
  begin
    global_u64bit:=x;
  end;

procedure test_param_s16(x : smallint); cdecl; public;
  begin
    global_s16bit:=x;
  end;

procedure test_param_s32(x: longint); cdecl; public;
  begin
    global_s32bit:=x;
  end;

procedure test_param_s64(x: int64); cdecl; public;
  begin
    global_s64bit:=x;
  end;

{$ifndef FPUNONE}
procedure test_param_float(x : single); cdecl; public;
  begin
    global_float:=x;
  end;

procedure test_param_double(x: double); cdecl; public;
  begin
    global_double:=x;
  end;

procedure test_param_longdouble(x: clongdouble); cdecl; public;
  begin
    global_long_double:=x;
  end;
{$endif FPUNONE}

procedure test_param_var_u8(var x: byte); cdecl; public;
  begin
    x:=RESULT_U8BIT;
  end;


{ array parameter passing }
procedure test_array_param_u8(x: byte_array); cdecl; public;
  begin
   global_u8bit:=x[1];
  end;

procedure test_array_param_u16(x : word_array); cdecl; public;
  begin
   global_u16bit:=x[1];
  end;

procedure test_array_param_u32(x: cardinal_array); cdecl; public;
  begin
   global_u32bit:=x[1];
  end;

procedure test_array_param_u64(x: qword_array); cdecl; public;
  begin
   global_u64bit:=x[1];
  end;

procedure test_array_param_s16(x :smallint_array); cdecl; public;
  begin
   global_s16bit:=x[1];
  end;

procedure test_array_param_s32(x: longint_array); cdecl; public;
  begin
   global_s32bit:=x[1];
  end;

procedure test_array_param_s64(x: int64_array); cdecl; public;
  begin
   global_s64bit:=x[1];
  end;

{$ifndef FPUNONE}
procedure test_array_param_float(x : single_array); cdecl; public;
  begin
   global_float:=x[1];
  end;

procedure test_array_param_double(x: double_array); cdecl; public;
  begin
   global_double:=x[1];
  end;

procedure test_array_param_longdouble(x: clongdouble_array); cdecl; public;
  begin
   global_long_double:=x[1];
  end;
{$endif FPUNONE}

{ mixed parameter passing }
procedure test_param_mixed_u16(z: byte; x : word; y :byte); cdecl; public;
  begin
    global_u16bit:=x;
    global_u8bit:=y;
  end;

procedure test_param_mixed_u32(z: byte; x: cardinal; y: byte); cdecl; public;
  begin
    global_u32bit:=x;
    global_u8bit:=y;
  end;

procedure test_param_mixed_s64(z: byte; x: int64; y: byte); cdecl; public;
  begin
    global_s64bit:=x;
    global_u8bit:=y;
  end;

{$ifndef FPUNONE}
procedure test_param_mixed_float(x: single; y: byte); cdecl; public;
  begin
    global_float:=x;
    global_u8bit:=y;
  end;

procedure test_param_mixed_double(x: double; y: byte); cdecl; public;
  begin
    global_double:=x;
    global_u8bit:=y;
  end;

procedure test_param_mixed_long_double(x: clongdouble; y: byte); cdecl; public;
  begin
    global_long_double:=x;
    global_u8bit:=y;
  end;
{$endif FPUNONE}

procedure test_param_mixed_var_u8(var x: byte;y:byte); cdecl; public;
  begin
    x:=RESULT_U8BIT;
    global_u8bit:=y;
  end;

{ structure parameter testing }
procedure test_param_struct_tiny(buffer :   _1BYTE_); cdecl; public;
  begin
    global_u8bit:=buffer.u8;
  end;

procedure test_param_struct_small(buffer :  _3BYTE_); cdecl; public;
  begin
    global_u8bit:=buffer.u8;
    global_u16bit:=buffer.u16;
end;

procedure test_param_struct_small_s(buffer :  _3BYTE_S); cdecl; public;
  begin
    global_u8bit:=buffer.w8;
    global_u16bit:=buffer.u16;
  end;

procedure test_param_struct_medium(buffer : _5BYTE_); cdecl; public;
  begin
    global_u8bit:=buffer.u8;
    global_u32bit:=buffer.u32;
  end;

procedure test_param_struct_large(buffer :  _7BYTE_); cdecl; public;
  begin
    global_u8bit:=buffer.u8;
    global_u16bit:=buffer.u16;
    global_s64bit:=buffer.s64;
  end;

{ mixed with structure parameter testing }
procedure test_param_mixed_struct_tiny(buffer :   _1BYTE_; y :byte); cdecl; public;
  begin
    global_u8bit := y;
  end;

procedure test_param_mixed_struct_small(buffer :  _3BYTE_; y :byte); cdecl; public;
  begin
    global_u8bit := y;
    global_u16bit := buffer.u16;
  end;

procedure test_param_mixed_struct_small_s(buffer :  _3BYTE_S; y :byte); cdecl; public;
  begin
    global_u8bit := y;
    global_u16bit := buffer.u16;
  end;

procedure test_param_mixed_struct_medium(buffer : _5BYTE_; y :byte); cdecl; public;
  begin
    global_u8bit := y;
    global_u32bit := buffer.u32;
  end;

procedure test_param_mixed_struct_large(buffer :  _7BYTE_; y :byte); cdecl; public;
  begin
    global_u8bit:=y;
    global_u16bit:=buffer.u16;
    global_s64bit:=buffer.s64;
  end;

{ function result value testing }
function test_function_u8: byte; cdecl; public;
  begin
    test_function_u8:=RESULT_U8BIT;
  end;

function test_function_u16: word; cdecl; public;
  begin
    test_function_u16:=RESULT_U16BIT;
  end;

function test_function_u32: cardinal; cdecl; public;
  begin
    test_function_u32:=RESULT_U32BIT;
  end;

function test_function_u64: qword; cdecl; public;
  begin
    test_function_u64:=RESULT_U64BIT;
  end;

function test_function_s16: smallint; cdecl; public;
  begin
    test_function_s16:=RESULT_S16BIT;
  end;

function test_function_s32: longint; cdecl; public;
  begin
    test_function_s32:=RESULT_S32BIT;
  end;

function test_function_s64: int64; cdecl; public;
  begin
    test_function_s64:=RESULT_S64BIT;
  end;

function test_function_pchar: pchar; cdecl; public;
  begin
    test_function_pchar:=RESULT_PCHAR;
  end;

{$ifndef FPUNONE}
function test_function_float : single; cdecl; public;
  begin
    test_function_float:=RESULT_FLOAT;
  end;

function test_function_double : double; cdecl; public;
  begin
    test_function_double:=RESULT_DOUBLE;
  end;

function test_function_longdouble: clongdouble; cdecl; public;
  begin
    test_function_longdouble:=RESULT_LONGDOUBLE;
  end;
{$endif FPUNONE}

function test_function_tiny_struct : _1byte_; cdecl; public;
  begin
    test_function_tiny_struct.u8:=RESULT_U8BIT;
  end;

function test_function_small_struct : _3byte_; cdecl; public;
  begin
    test_function_small_struct.u8:=RESULT_U8BIT;
    test_function_small_struct.u16:=RESULT_U16BIT;
  end;

function test_function_small_struct_s : _3byte_s; cdecl; public;
  begin
    test_function_small_struct_s.w8:=RESULT_U8BIT;
    test_function_small_struct_s.u16:=RESULT_U16BIT;
  end;

function test_function_medium_struct : _5byte_; cdecl; public;
  begin
    test_function_medium_struct.u8:=RESULT_U8BIT;
    test_function_medium_struct.u32:=RESULT_U32BIT;
  end;

function test_function_struct : _7byte_; cdecl; public;
  begin
    test_function_struct.u8:=RESULT_U8BIT;
    test_function_struct.u16:=RESULT_U16BIT;
    test_function_struct.s64:=RESULT_S64BIT;
  end;




end.
