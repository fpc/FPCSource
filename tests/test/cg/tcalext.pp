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

{$L ctest.o}
const
   RESULT_U8BIT = $55;
   RESULT_U16BIT = $500F;
   RESULT_U32BIT = $500F0000;
   RESULT_S64BIT = -12000;


{ simple parameter passing }
procedure test_param_u8(x: byte); cdecl; external;
procedure test_param_u16(x : word); cdecl; external;
procedure test_param_u32(x: cardinal); cdecl; external;
procedure test_param_s64(x: int64); cdecl; external;
{ mixed parameter passing }
procedure test_param_mixed_u16(z: byte; x : word; y :byte); cdecl; external;
procedure test_param_mixed_u32(z: byte; x: cardinal; y: byte); cdecl; external;
procedure test_param_mixed_s64(z: byte; x: int64; y: byte); cdecl; external;


var
 global_u8bit : byte; cvar; external;
 global_u16bit : word; cvar; external;
 global_u32bit : longint; cvar;external;
 global_s64bit : int64; cvar; external;
 value_u8bit : byte;
 value_u16bit : word;
 value_u32bit : cardinal;
 value_s64bit : int64;

 procedure clear_globals;
  begin
    global_u8bit := 0;
    global_u16bit := 0;
    global_u32bit := 0;
    global_s64bit := 0;
  end;

 procedure clear_values;
  begin
    value_u8bit := 0;
    value_u16bit := 0;
    value_u32bit := 0;
    value_s64bit := 0;
  end;

  procedure fail;
   begin
     WriteLn('Failed!');
     halt(1);
   end;


var failed : boolean;
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

end.

{
  $Log$
  Revision 1.1  2002-04-13 21:03:43  carl
  + C module testing (unfinished)

}