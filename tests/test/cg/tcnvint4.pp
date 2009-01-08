{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{****************************************************************}
{ NODE TESTED : secondtypeconvert() -> second_int_to_real        }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{                 secondcalln()                                  }
{                 secondinline()                                 }
{                 secondadd()                                    }
{****************************************************************}
{ DEFINES:                                                       }
{****************************************************************}
{ REMARKS: Tests integer to real conversion                      }
{   This routine assumes that there is a type conversion         }
{   from all types to s32bit, u32bit or s64bit before conversion }
{   to a real.                                                   }
{****************************************************************}
program tcnvint4;

{$ifdef VER70}
  {$define tp}
{$endif}

{$R-}

{$ifdef tp}
type
  smallint = integer;
{$endif}

procedure fail;
begin
  WriteLn('Failure.');
  halt(1);
end;


const
 RESULT_U64BIT            =  qword($8fe0000000000000);
 RESULT_S64BIT            =  -101234;
 RESULT_S32BIT            = -1000000;
 RESULT_U32BIT            =  2000000;
 RESULT_S16BIT            =   -12123;
 RESULT_U16BIT            =    12123;
 RESULT_U8BIT             =      247;
 RESULT_S8BIT             =     -123;


{$ifndef tp}
   function gets64bit : int64;
    begin
      gets64bit := RESULT_S64BIT;
    end;

   function getu64bit : qword;
    begin
      getu64bit := RESULT_U64BIT;
    end;
{$endif}

   function gets32bit : longint;
    begin
      gets32bit := RESULT_S32BIT;
    end;

   function gets16bit : smallint;
    begin
      gets16bit := RESULT_S16BIT;
    end;

   function gets8bit : shortint;
    begin
      gets8bit := RESULT_S8BIT;
    end;

   function getu8bit : byte;
    begin
      getu8bit := RESULT_U8BIT;
    end;

   function getu16bit : word;
    begin
      getu16bit := RESULT_U16BIT;
    end;

   function getu32bit : longint;
    begin
      getu32bit := RESULT_U32BIT;
    end;

var
 s32bit : longint;
 failed : boolean;
 s16bit : smallint;
 s8bit : shortint;
 u8bit : byte;
 u16bit : word;
{$ifndef tp}
 s64bit : int64;
 u32bit : cardinal;
{$endif}
 result_val : real;
begin
  { left : LOC_REFERENCE }
  Write('second_int_to_real (left : LOC_REFERENCE)...');
  s64bit := RESULT_S64BIT;
  failed := false;
  result_val := s64bit;
  if trunc(result_val) <> RESULT_S64BIT then
     failed:=true;

  s32bit := RESULT_S32BIT;
  result_val := s32bit;
  if trunc(result_val) <> RESULT_S32BIT then
     failed:=true;


  u32bit := high(u32bit);
  result_val := u32bit;
  if trunc(result_val) <> high(u32bit) then
     failed:=true;

  u32bit := RESULT_U32BIT;
  result_val := u32bit;
  if trunc(result_val) <> RESULT_U32BIT then
     failed:=true;

  s16bit := RESULT_S16BIT;
  result_val := s16bit;
  if trunc(result_val) <> RESULT_S16BIT then
     failed:=true;

  u16bit := RESULT_U16BIT;
  result_val := u16bit;
  if trunc(result_val) <> RESULT_U16BIT then
     failed:=true;


  s8bit := RESULT_S8BIT;
  result_val := s8bit;
  if trunc(result_val) <> RESULT_S8BIT then
     failed:=true;

  u8bit := RESULT_U8BIT;
  result_val := u8bit;
  if trunc(result_val) <> RESULT_U8BIT then
     failed:=true;


  if failed then
    fail
  else
    WriteLn('Passed!');

  Write('second_int_to_real (left : LOC_REGISTER)...');
  failed := false;
  result_val := gets64bit;
  if trunc(result_val) <> RESULT_S64BIT then
     failed:=true;

  result_val := getu64bit;
  if result_val <> 10367286342206881792.0 then
    begin
      writeln('got ',result_val:0);
      writeln('expected ',10367286342206881792.0);
     failed:=true;
    end;

  result_val := gets32bit;
  if trunc(result_val) <> RESULT_S32BIT then
     failed:=true;


  result_val := getu32bit;
  if trunc(result_val) <> RESULT_U32BIT then
     failed:=true;

  result_val := getu8bit;
  if trunc(result_val) <> RESULT_u8BIT then
     failed:=true;


  result_val := gets8bit;
  if trunc(result_val) <> RESULT_s8BIT then
     failed:=true;


  result_val := gets16bit;
  if trunc(result_val) <> RESULT_S16BIT then
     failed:=true;


  result_val := getu16bit;
  if trunc(result_val) <> RESULT_U16BIT then
     failed:=true;


  if failed then
    fail
  else
    WriteLn('Passed!');

end.
