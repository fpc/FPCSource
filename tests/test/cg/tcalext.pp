{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{****************************************************************}
{ NODE TESTED : secondcalln()                                    }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{                 secondcalln()                                  }
{                 secondadd()                                    }
{                 secondtypeconv()                               }
{****************************************************************}
{ DEFINES:                                                       }
{****************************************************************}
{ REMARKS: This tests a subset of the secondcalln() , it         }
{          verifies the usage of external cdecl                  }
{          modules compiled with C compilers.                    }
{****************************************************************}
program tcalext;
{$MODE OBJFPC}
{$STATIC ON}
{$R+}
uses strings;

{$L ctest.o}
{ Use C alignment of records }
{$PACKRECORDS C}
const
   RESULT_U8BIT = $55;
   RESULT_U16BIT = $500F;
   RESULT_U32BIT = $500F0000;
   RESULT_S64BIT = -12000;
   RESULT_FLOAT  = 14.54;
   RESULT_DOUBLE = RESULT_FLOAT;
   RESULT_LONGDOUBLE = RESULT_FLOAT;
   RESULT_PCHAR  = 'Hello world';

type
 _3byte_ = record
  u8 : byte;
  u16 : word;
 end;
 
_7byte_ = record
  u8: byte;
  s64: int64;
  u16: word;
end;


{ simple parameter passing }
procedure test_param_u8(x: byte); cdecl; external;
procedure test_param_u16(x : word); cdecl; external;
procedure test_param_u32(x: cardinal); cdecl; external;
procedure test_param_s64(x: int64); cdecl; external;
procedure test_param_float(x : single); cdecl; external;
procedure test_param_double(x: double); cdecl; external;
procedure test_param_longdouble(x: double); cdecl; external;
procedure test_param_var_u8(var x: byte); cdecl; external;

{ mixed parameter passing }
procedure test_param_mixed_u16(z: byte; x : word; y :byte); cdecl; external;
procedure test_param_mixed_u32(z: byte; x: cardinal; y: byte); cdecl; external;
procedure test_param_mixed_s64(z: byte; x: int64; y: byte); cdecl; external;
{ structure parameter testing }
procedure test_param_struct_small(buffer :  _3BYTE_); cdecl; external;
procedure test_param_struct_large(buffer :  _7BYTE_); cdecl; external;
{ function result value testing }
function test_function_u8: byte; cdecl; external;
function test_function_u16: word; cdecl; external;
function test_function_u32: cardinal; cdecl; external;
function test_function_s64: int64; cdecl; external;
function test_function_pchar: pchar; cdecl; external;
function test_function_float : single; cdecl; external;
function test_function_double : double; cdecl; external;
function test_function_longdouble: extended; cdecl; external;
function test_function_struct : _7byte_; cdecl; external;






var
 global_u8bit : byte; cvar; external;
 global_u16bit : word; cvar; external;
 global_u32bit : longint; cvar;external;
 global_s64bit : int64; cvar; external;
 global_float : single; cvar;external;
 global_double : double; cvar;external;
 global_long_double : extended; cvar; external;
 value_u8bit : byte;
 value_u16bit : word;
 value_u32bit : cardinal;
 value_s64bit : int64;
 value_float : single;
 value_double : double;
 value_longdouble : extended;

 procedure clear_globals;
  begin
    global_u8bit := 0;
    global_u16bit := 0;
    global_u32bit := 0;
    global_s64bit := 0;
    global_float := 0.0;
    global_double := 0.0;
    global_long_double := 0.0;
   end;

 procedure clear_values;
  begin
    value_u8bit := 0;
    value_u16bit := 0;
    value_u32bit := 0;
    value_s64bit := 0;
    value_float := 0.0;
    value_double := 0.0;
    value_longdouble := 0.0;
  end;

  procedure fail;
   begin
     WriteLn('Failed!');
     halt(1);
   end;


var failed : boolean;
    smallstruct : _3BYTE_;
    bigstruct : _7BYTE_;
    pc: pchar;
begin
  Write('External simple parameter testing...');
  failed := false;
  
  clear_values;
  clear_globals;
  
  value_u8bit := RESULT_U8BIT;
  test_param_u8(value_u8bit);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  clear_values;
  clear_globals;
  
  value_u16bit := RESULT_U16BIT;
  test_param_u16(value_u16bit);
  if global_u16bit <> RESULT_U16BIT then
    failed := true;

  clear_values;
  clear_globals;

  value_u32bit := RESULT_U32BIT;
  test_param_u32(value_u32bit);
  if global_u32bit <> RESULT_U32BIT then
    failed := true;

  clear_values;
  clear_globals;
  
  value_s64bit := RESULT_S64BIT;
  test_param_s64(value_s64bit);
  if global_s64bit <> RESULT_S64BIT then
    failed := true;

  clear_values;
  clear_globals;

  value_float := RESULT_FLOAT;    
  test_param_float(value_float);
  if trunc(global_float) <> trunc(RESULT_FLOAT) then
    failed := true;
    
  clear_values;
  clear_globals;
  
  value_double := RESULT_DOUBLE;    
  test_param_double(value_double);
  if trunc(global_double) <> trunc(RESULT_DOUBLE) then
    failed := true;

  clear_values;
  clear_globals;

  value_longdouble := RESULT_LONGDOUBLE;    
  test_param_longdouble(value_longdouble);
  if trunc(global_long_double) <> trunc(RESULT_LONGDOUBLE) then
    failed := true;
    
  { var parameter testing }    
  clear_values;
  clear_globals;
  test_param_var_u8(value_u8bit);
  if value_u8bit <> RESULT_U8BIT then
     failed := true;
    
    
  If failed then 
   fail
  else
    WriteLn('Passed!');

  Write('External mixed parameter testing...');
  failed := false;
  
  clear_values;
  clear_globals;
  
  value_u8bit := RESULT_U8BIT;
  value_u16bit := RESULT_U16BIT;
  test_param_mixed_u16(value_u8bit, value_u16bit, value_u8bit);
  if global_u16bit <> RESULT_U16BIT then
    failed := true;
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  clear_values;
  clear_globals;

  value_u8bit := RESULT_U8BIT;
  value_u32bit := RESULT_U32BIT;
  test_param_mixed_u32(value_u8bit, value_u32bit, value_u8bit);
  if global_u32bit <> RESULT_U32BIT then
    failed := true;
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  clear_values;
  clear_globals;
  
  value_u8bit := RESULT_U8BIT;
  value_s64bit := RESULT_S64BIT;
  test_param_mixed_s64(value_u8bit, value_s64bit, value_u8bit);
  if global_s64bit <> RESULT_S64BIT then
    failed := true;
  if global_u8bit <> RESULT_U8BIT then
    failed := true;
  
  If failed then 
   fail
  else
    WriteLn('Passed!');

  Write('External struct parameter testing...');
  
  failed := false;

  clear_values;
  clear_globals;
  
  smallstruct.u8 := RESULT_U8BIT;
  smallstruct.u16 := RESULT_u16BIT;
  test_param_struct_small(smallstruct);
  if global_u16bit <> RESULT_U16BIT then
    failed := true;
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  clear_values;
  clear_globals;


  bigstruct.u8 := RESULT_U8BIT;
  bigstruct.u16 := RESULT_U16BIT;
  bigstruct.s64 := RESULT_S64BIT;
  test_param_struct_large(bigstruct);
  if global_s64bit <> RESULT_S64BIT then
    failed := true;
  if global_u16bit <> RESULT_U16BIT then
    failed := true;
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  If failed then 
   fail
  else
    WriteLn('Passed!');


  Write('Function result testing...');
  failed := false;
  
  clear_values;
  clear_globals;
  
  value_u8bit := test_function_u8;
  if value_u8bit <> RESULT_U8BIT then
    failed := true;

  clear_values;
  clear_globals;
  
  value_u16bit := test_function_u16;
  if value_u16bit <> RESULT_U16BIT then
    failed := true;

  clear_values;
  clear_globals;
  
  value_u32bit := test_function_u32;
  if value_u32bit <> RESULT_U32BIT then
    failed := true;

  clear_values;
  clear_globals;

  value_s64bit := test_function_s64;
  if value_s64bit <> RESULT_S64BIT then
    failed := true;

  clear_values;
  clear_globals;
  
  { verify if the contents both strings are equal }
  pc := test_function_pchar;
  if strcomp(pc, RESULT_PCHAR) <> 0 then
    failed := true;

  clear_values;
  clear_globals;

  value_float := test_function_float;
  if trunc(value_float) <> trunc(RESULT_FLOAT) then
    failed := true;

  clear_values;
  clear_globals;

  value_double := test_function_double;
  if trunc(value_double) <> trunc(RESULT_DOUBLE) then
    failed := true;

  clear_values;
  clear_globals;

  value_longdouble := test_function_longdouble;
  if trunc(value_longdouble) <> trunc(RESULT_LONGDOUBLE) then
    failed := true;
 
  clear_values;
  clear_globals;

  bigstruct := test_function_struct;
  if bigstruct.u8 <> RESULT_U8BIT then
    failed := true;
  if bigstruct.s64 <> RESULT_S64BIT then
    failed := true;
  if bigstruct.u16 <> RESULT_U16BIT then
    failed := true;

  If failed then 
   fail
  else
    WriteLn('Passed!');
  
end.

{
  $Log$
  Revision 1.3  2002-05-04 16:56:54  carl
  + var parameter testing
  + function result testing
  + floating point testing

  Revision 1.2  2002/04/22 19:09:28  carl
  + added structure testing

  Revision 1.1  2002/04/13 21:03:43  carl
  + C module testing (unfinished)

}