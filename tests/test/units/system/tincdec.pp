{ Program to test the system unit inc/dec routines }
{ By Carl Eric Codere Copyright (c) 2002           }
program tincdec;

const
   INCDEC_COUNT_SIMPLE = 8;
   INCDEC_COUNT_COMPLEX = -12;

   INIT_U8BIT = $0F;
   INIT_U16BIT = $FF00;
   INIT_U32BIT = $FF00FF00;

   INIT_S8BIT = $0F;
   INIT_S16BIT = -13333;
   INIT_S32BIT = -2335754;
   INIT_S64BIT = Low(longint);
var
 global_s8bit : shortint;
 global_s16bit : smallint;
 global_s32bit :longint;
 global_s64bit : int64;
 global_u8bit : byte;
 global_u16bit : word;
 global_u32bit : longword;
 { the result must be calculated manually since
   FPC 1.0.x does not support adding directly 64-bit
   constants
 }
 result_s64bit_complex : int64;

 procedure init_globals;
   begin
    global_s8bit := INIT_S8BIT;
    global_s16bit := INIT_S16BIT;
    global_s32bit := longint(INIT_S32BIT);
    global_s64bit := INIT_S64BIT;
    global_u8bit := INIT_U8BIT;
    global_u16bit := INIT_U16BIT;
    global_u32bit := INIT_U32BIT;
    result_s64bit_complex := INIT_S64BIT;
    result_s64bit_complex := result_s64bit_complex + INCDEC_COUNT_COMPLEX;
   end;



  procedure fail;
    begin
      WriteLn('Failed!');
      Halt(1);
    end;

  function getcomplex_count_s32 : longint;
   begin
     getcomplex_count_s32 := INCDEC_COUNT_COMPLEX;
   end;

  function getcomplex_count_s8 :shortint;
   begin
     getcomplex_count_s8 := INCDEC_COUNT_COMPLEX;
   end;

  function getcomplex_count_s64 : int64;
   begin
     getcomplex_count_s64 := INCDEC_COUNT_COMPLEX;
   end;

{***********************************************************************}
{                              INC                                      }
{***********************************************************************}

procedure test_inc_s8;
   var
    b: smallint;
    _result : boolean;
 begin
   _result := true;
   Write('Inc() signed 8-bit tests...');

   init_globals;
   Inc(global_s8bit);
   if global_S8bit <> (INIT_S8BIT+1) then
     _result := false;

   init_globals;
   Inc(global_S8bit, INCDEC_COUNT_SIMPLE);
   if global_S8bit <> (INCDEC_COUNT_SIMPLE+INIT_S8BIT) then
     _result := false;

   init_globals;
   Inc(global_S8bit, INCDEC_COUNT_COMPLEX);
   if global_S8bit <> (INCDEC_COUNT_COMPLEX+INIT_S8BIT) then
     _result := false;

   init_globals;
   b:= INCDEC_COUNT_SIMPLE;
   Inc(global_S8bit, b);
   if global_S8bit <> (INCDEC_COUNT_SIMPLE+INIT_S8BIT) then
     _result := false;

   init_globals;
   b:= INCDEC_COUNT_COMPLEX;
   Inc(global_S8bit, b);
   if global_S8bit <> (INCDEC_COUNT_COMPLEX+INIT_S8BIT) then
     _result := false;

   init_globals;
   Inc(global_S8bit, getcomplex_count_s32);
   if global_S8bit <> (INCDEC_COUNT_COMPLEX+INIT_S8BIT) then
     _result := false;

   init_globals;
   Inc(global_S8bit, getcomplex_count_s8);
   if global_S8bit <> (INCDEC_COUNT_COMPLEX+INIT_S8BIT) then
     _result := false;

   if not _result then
      fail
   else
     WriteLn('Success!');
 end;

procedure test_inc_s16;
   var
    b: smallint;
    _result : boolean;
 begin
   _result := true;
   Write('Inc() signed 16-bit tests...');

   init_globals;
   Inc(global_s16bit);
   if global_S16bit <> (INIT_S16BIT+1) then
     _result := false;

   init_globals;
   Inc(global_s16bit, INCDEC_COUNT_SIMPLE);
   if global_s16bit <> (INCDEC_COUNT_SIMPLE+INIT_s16BIT) then
     _result := false;

   init_globals;
   Inc(global_s16bit, INCDEC_COUNT_COMPLEX);
   if global_s16bit <> (INCDEC_COUNT_COMPLEX+INIT_s16BIT) then
     _result := false;

   init_globals;
   b:= INCDEC_COUNT_SIMPLE;
   Inc(global_s16bit, b);
   if global_s16bit <> (INCDEC_COUNT_SIMPLE+INIT_s16BIT) then
     _result := false;

   init_globals;
   b:= INCDEC_COUNT_COMPLEX;
   Inc(global_s16bit, b);
   if global_s16bit <> (INCDEC_COUNT_COMPLEX+INIT_s16BIT) then
     _result := false;

   init_globals;
   Inc(global_s16bit, getcomplex_count_s32);
   if global_s16bit <> (INCDEC_COUNT_COMPLEX+INIT_s16BIT) then
     _result := false;

   init_globals;
   Inc(global_s16bit, getcomplex_count_s8);
   if global_s16bit <> (INCDEC_COUNT_COMPLEX+INIT_s16BIT) then
     _result := false;

   if not _result then
      fail
   else
     WriteLn('Success!');
 end;

procedure test_inc_s32;
   var
    b: smallint;
    _result : boolean;
 begin
    _result := true;
   Write('Inc() signed 32-bit tests...');

   init_globals;
   Inc(global_s32bit);
   if global_S32bit <> (INIT_S32BIT+1) then
     _result := false;

   init_globals;
   Inc(global_s32bit, INCDEC_COUNT_SIMPLE);
   if global_s32bit <> (INCDEC_COUNT_SIMPLE+INIT_s32BIT) then
     _result := false;

   init_globals;
   Inc(global_s32bit, INCDEC_COUNT_COMPLEX);
   if global_s32bit <> (INCDEC_COUNT_COMPLEX+INIT_s32BIT) then
     _result := false;

   init_globals;
   b:= INCDEC_COUNT_SIMPLE;
   Inc(global_s32bit, b);
   if global_s32bit <> (INCDEC_COUNT_SIMPLE+INIT_s32BIT) then
     _result := false;

   init_globals;
   b:= INCDEC_COUNT_COMPLEX;
   Inc(global_s32bit, b);
   if global_s32bit <> (INCDEC_COUNT_COMPLEX+INIT_s32BIT) then
     _result := false;

   init_globals;
   Inc(global_s32bit, getcomplex_count_s32);
   if global_s32bit <> (INCDEC_COUNT_COMPLEX+INIT_s32BIT) then
     _result := false;

   init_globals;
   Inc(global_s32bit, getcomplex_count_s8);
   if global_s32bit <> (INCDEC_COUNT_COMPLEX+INIT_s32BIT) then
     _result := false;

   init_globals;
   Inc(global_s32bit, getcomplex_count_s64);
   if global_s32bit <> (INCDEC_COUNT_COMPLEX+INIT_s32BIT) then
     _result := false;


   if not _result then
      fail
   else
     WriteLn('Success!');
end;

procedure test_inc_s64;
   var
    b: int64;
    _result : boolean;
 begin
   _result := true;
   Write('Inc() signed 64-bit tests...');

   init_globals;
   Inc(global_s64bit);
   if global_S64bit <> (result_s64bit_complex-INCDEC_COUNT_COMPLEX+1) then
     _result := false;

   init_globals;
   Inc(global_s64bit, INCDEC_COUNT_COMPLEX);
   if global_s64bit <> (result_s64bit_complex) then
     _result := false;

   init_globals;
   Inc(global_s64bit, INCDEC_COUNT_COMPLEX);
   if global_s64bit <> (result_s64bit_complex) then
     _result := false;

   init_globals;
   b:= INCDEC_COUNT_COMPLEX;
   Inc(global_s64bit, b);
   if global_s64bit <> (result_s64bit_complex) then
     _result := false;

{$ifndef ver1_0}
   init_globals;
   Inc(global_s64bit, getcomplex_count_s8);
   if global_s64bit <> (INCDEC_COUNT_COMPLEX+INIT_S64BIT) then
     _result := false;

   init_globals;
   Inc(global_s64bit, getcomplex_count_s32);
   if global_s64bit <> (INCDEC_COUNT_COMPLEX+INIT_s64BIT) then
     _result := false;


{$endif}
   if not _result then
      fail
   else
     WriteLn('Success!');
 end;


procedure test_inc_u32;
   var
    b: smallint;
    _result : boolean;
 begin
    _result := true;
   Write('Inc() unsigned 32-bit tests...');

   init_globals;
   Inc(global_u32bit);
   if global_u32bit <> (INIT_U32BIT+1) then
     _result := false;

   init_globals;
   Inc(global_u32bit, INCDEC_COUNT_SIMPLE);
   if global_u32bit <> (INCDEC_COUNT_SIMPLE+INIT_u32BIT) then
     _result := false;

   init_globals;
   Inc(global_u32bit, INCDEC_COUNT_COMPLEX);
   if global_u32bit <> (INCDEC_COUNT_COMPLEX+INIT_u32BIT) then
     _result := false;

   init_globals;
   b:= INCDEC_COUNT_SIMPLE;
   Inc(global_u32bit, b);
   if global_u32bit <> (INCDEC_COUNT_SIMPLE+INIT_u32BIT) then
     _result := false;

   init_globals;
   b:= INCDEC_COUNT_COMPLEX;
   Inc(global_u32bit, b);
   if global_u32bit <> (INCDEC_COUNT_COMPLEX+INIT_u32BIT) then
     _result := false;

   init_globals;
   Inc(global_u32bit, getcomplex_count_s32);
   if global_u32bit <> (INCDEC_COUNT_COMPLEX+INIT_u32BIT) then
     _result := false;

   init_globals;
   Inc(global_u32bit, getcomplex_count_s8);
   if global_u32bit <> (INCDEC_COUNT_COMPLEX+INIT_u32BIT) then
     _result := false;

   init_globals;
   Inc(global_u32bit, getcomplex_count_s64);
   if global_u32bit <> (INCDEC_COUNT_COMPLEX+INIT_u32BIT) then
     _result := false;


   if not _result then
      fail
   else
     WriteLn('Success!');
end;

{***********************************************************************}
{                              DEC                                      }
{***********************************************************************}
procedure test_dec_s8;
   var
    b: smallint;
    _result : boolean;
    l: byte;
 begin
   _result := true;
   Write('dec() signed 8-bit tests...');

   init_globals;
   dec(global_S8bit, INCDEC_COUNT_SIMPLE);
   if global_S8bit <> (INIT_S8BIT-INCDEC_COUNT_SIMPLE) then
     _result := false;

   init_globals;
   dec(global_S8bit, INCDEC_COUNT_COMPLEX);
   if global_S8bit <>  (INIT_S8BIT-INCDEC_COUNT_COMPLEX) then
     _result := false;

   init_globals;
   b:= INCDEC_COUNT_SIMPLE;
   dec(global_S8bit, b);
   if global_S8bit <> (INIT_S8BIT-INCDEC_COUNT_SIMPLE) then
     _result := false;

   init_globals;
   b:= INCDEC_COUNT_COMPLEX;
   dec(global_S8bit, b);
   if global_S8bit <> (INIT_S8BIT-INCDEC_COUNT_COMPLEX) then
     _result := false;

   init_globals;
   dec(global_S8bit, getcomplex_count_s32);
   if global_S8bit <> (INIT_S8BIT-INCDEC_COUNT_COMPLEX) then
     _result := false;

   init_globals;
   dec(global_S8bit, getcomplex_count_s8);
   if global_S8bit <> (INIT_S8BIT-INCDEC_COUNT_COMPLEX) then
     _result := false;

   { extra test for overflow checking }
   l:=byte(high(shortint));
   global_s8bit := high(shortint);
   dec(global_s8bit,l);
   if global_s8bit <> 0 then
     _result := false;


   if not _result then
      fail
   else
     WriteLn('Success!');
 end;

procedure test_dec_s16;
   var
    b: smallint;
    _result : boolean;
 begin
   _result := true;
   Write('dec() signed 16-bit tests...');

   init_globals;
   dec(global_s16bit, INCDEC_COUNT_SIMPLE);
   if global_s16bit <> (INIT_S16BIT-INCDEC_COUNT_SIMPLE) then
     _result := false;

   init_globals;
   dec(global_s16bit, INCDEC_COUNT_COMPLEX);
   if global_s16bit <> (INIT_S16BIT-INCDEC_COUNT_COMPLEX) then
     _result := false;

   init_globals;
   b:= INCDEC_COUNT_SIMPLE;
   dec(global_s16bit, b);
   if global_s16bit <> (INIT_S16BIT-INCDEC_COUNT_SIMPLE) then
     _result := false;

   init_globals;
   b:= INCDEC_COUNT_COMPLEX;
   dec(global_s16bit, b);
   if global_s16bit <> (INIT_S16BIT-INCDEC_COUNT_COMPLEX) then
     _result := false;

   init_globals;
   dec(global_s16bit, getcomplex_count_s32);
   if global_s16bit <> (INIT_S16BIT-INCDEC_COUNT_COMPLEX) then
     _result := false;

   init_globals;
   dec(global_s16bit, getcomplex_count_s8);
   if global_s16bit <> (INIT_S16BIT-INCDEC_COUNT_COMPLEX) then
     _result := false;

   if not _result then
      fail
   else
     WriteLn('Success!');
 end;

procedure test_dec_s32;
   var
    b: smallint;
    _result : boolean;
 begin
    _result := true;
   Write('dec() signed 32-bit tests...');

   init_globals;
   dec(global_s32bit, INCDEC_COUNT_SIMPLE);
   if global_s32bit <> (INIT_S32BIT-INCDEC_COUNT_SIMPLE) then
     _result := false;

   init_globals;
   dec(global_s32bit, INCDEC_COUNT_COMPLEX);
   if global_s32bit <> (INIT_S32BIT-INCDEC_COUNT_COMPLEX) then
     _result := false;

   init_globals;
   b:= INCDEC_COUNT_SIMPLE;
   dec(global_s32bit, b);
   if global_s32bit <>  (INIT_S32BIT-INCDEC_COUNT_SIMPLE) then
     _result := false;

   init_globals;
   b:= INCDEC_COUNT_COMPLEX;
   dec(global_s32bit, b);
   if global_s32bit <> (INIT_S32BIT-INCDEC_COUNT_COMPLEX) then
     _result := false;

   init_globals;
   dec(global_s32bit, getcomplex_count_s32);
   if global_s32bit <> (INIT_S32BIT-INCDEC_COUNT_COMPLEX) then
     _result := false;

   init_globals;
   dec(global_s32bit, getcomplex_count_s8);
   if global_s32bit <> (INIT_S32BIT-INCDEC_COUNT_COMPLEX) then
     _result := false;

   init_globals;
   dec(global_s32bit, getcomplex_count_s64);
   if global_s32bit <> (INIT_S32BIT-INCDEC_COUNT_COMPLEX) then
     _result := false;


   if not _result then
      fail
   else
     WriteLn('Success!');
end;

procedure test_dec_s64;
   var
    b: smallint;
    _result : boolean;
 begin
   _result := true;
   Write('dec() signed 64-bit tests...');


{$ifndef ver1_0}
   init_globals;
   dec(global_s64bit, getcomplex_count_s8);
   if global_s64bit <> (INIT_S64BIT-INCDEC_COUNT_COMPLEX) then
     _result := false;

   init_globals;
   dec(global_s64bit, getcomplex_count_s32);
   if global_s64bit <> (INIT_S64BIT-INCDEC_COUNT_COMPLEX) then
     _result := false;
{$endif}
   if not _result then
      fail
   else
     WriteLn('Success!');
 end;

procedure test_dec_u32;
   var
    b: smallint;
    _result : boolean;
 begin
    _result := true;
   Write('dec() unsigned 32-bit tests...');

   init_globals;
   dec(global_u32bit, INCDEC_COUNT_SIMPLE);
   if global_u32bit <> (INIT_u32BIT-INCDEC_COUNT_SIMPLE) then
     _result := false;

   init_globals;
   dec(global_u32bit, INCDEC_COUNT_COMPLEX);
   if global_u32bit <> (INIT_u32BIT-INCDEC_COUNT_COMPLEX) then
     _result := false;

   init_globals;
   b:= INCDEC_COUNT_SIMPLE;
   dec(global_u32bit, b);
   if global_u32bit <>  (INIT_u32BIT-INCDEC_COUNT_SIMPLE) then
     _result := false;

   init_globals;
   b:= INCDEC_COUNT_COMPLEX;
   dec(global_u32bit, b);
   if global_u32bit <> (INIT_u32BIT-INCDEC_COUNT_COMPLEX) then
     _result := false;

   init_globals;
   dec(global_u32bit, getcomplex_count_s32);
   if global_u32bit <> (INIT_u32BIT-INCDEC_COUNT_COMPLEX) then
     _result := false;

   init_globals;
   dec(global_u32bit, getcomplex_count_s8);
   if global_u32bit <> (INIT_u32BIT-INCDEC_COUNT_COMPLEX) then
     _result := false;

   init_globals;
   dec(global_u32bit, getcomplex_count_s64);
   if global_u32bit <> (INIT_u32BIT-INCDEC_COUNT_COMPLEX) then
     _result := false;



   if not _result then
      fail
   else
     WriteLn('Success!');
end;


procedure test_inc_ptr;
   type
     tstruct = packed record
      b: byte;
      w: word;
     end;
   const
      word_array : array[1..4] of word =
      ($0000,$FFFF,$F0F0,$F00F);
      struct_array : array[1..4] of tstruct = (
       (b:00;w:0001),
       (b:01;w:0102),
       (b:02;w:0203),
       (b:03;w:0304)
      );
   var
    _result : boolean;
    pw : ^word;
    podd : ^tstruct;
    i: integer;
    B : byte;
 begin
    _result := true;
   Write('Inc() pointer to word...');
   pw:=@word_array;
   for i:=1 to 4 do
     begin
        if (word_array[i] <> pw^) then
          _result := false;
        Inc(pw)
     end;

   pw:=@word_array;
   inc(pw,2);
   if pw^<>word_array[3] then
     _result := false;

   pw:=@word_array;
   b:=2;
   inc(pw,b);
   if pw^<>word_array[3] then
     _result := false;

   podd:=@struct_array;
   b:=3;
   inc(podd,b);
   if (podd^.b<>struct_array[4].b) and (podd^.w<>struct_array[4].w) then
     _result := false;

   podd:=@struct_array;
   inc(podd,3);
   if (podd^.b<>struct_array[4].b) and (podd^.w<>struct_array[4].w) then
     _result := false;

   if not _result then
      fail
   else
     WriteLn('Success!');
 end;


Begin
  test_inc_s8;
  test_inc_s16;
  test_inc_s32;
  test_inc_s64;
  test_inc_u32;
  test_inc_ptr;

  test_dec_s8;
  test_dec_s16;
  test_dec_s32;
  test_dec_s64;
  test_dec_u32;
end.
