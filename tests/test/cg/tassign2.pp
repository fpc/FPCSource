{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{  By Carl Eric Codere                                           }
{****************************************************************}
{ NODE TESTED : secondassign()                                   }
{****************************************************************}
{ DEFINES:                                                       }
{            FPC     = Target is FreePascal compiler             }
{****************************************************************}
{ REMARKS : Tested with Delphi 3 as reference implementation     }
{****************************************************************}
program tassign2;

{$ifdef fpc}
{$warning Will only work on 32-bit cpu's}
{$mode objfpc}
{$endif}

const
  RESULT_STRING = 'Hello world';
  RESULT_S64BIT = -12;
  RESULT_S32BIT = -124356;
  RESULT_U32BIT = 654321;
  RESULT_U8BIT  = $55;
  RESULT_S16BIT = -12124;
  RESULT_REAL   = 12.12;

  { adjusts the size of the bigrecord }
  MAX_INDEX = 7;

type
  {
    the size of this record should *at least* be the size
    of a natural register for the target processor
  }
  tbigrecord = record
   x : cardinal;
   y : cardinal;
   z : array[0..MAX_INDEX] of byte;
  end;


    procedure fail;
    begin
      WriteLn('Failure.');
      halt(1);
    end;



    function getresults64bit: int64;
     begin
       getresults64bit := RESULT_S64BIT;
     end;

    function getresults32bit : longint;
     begin
       getresults32bit := RESULT_S32BIT;
     end;

    function getresultu8bit : byte;
      begin
        getresultu8bit := RESULT_U8BIT;
      end;

    function getresults16bit : smallint;
      begin
        getresults16bit := RESULT_S16BIT;
      end;

    function getresultreal : real;
      begin
        getresultreal := RESULT_REAL;
      end;

var
 failed : boolean;
 s64bit : int64;
 s32bit : longint;
 s16bit : smallint;
 u8bit : byte;
 boolval : boolean;
 real_val : real;
 bigrecord1, bigrecord2 : tbigrecord;
 i: integer;
Begin
  WriteLn('secondassign node testing.');
  failed := false;
  { possibilities : left : any, right : LOC_REFERENCE, LOC_REGISTER,
    LOC_FPUREGISTER, LOC_CONSTANT, LOC_JUMP and LOC_FLAGS }
  Write('left : LOC_REFERENCE, right : LOC_CONSTANT tests..');
  s64bit := RESULT_S64BIT;
  if s64bit <> RESULT_S64BIT then
    failed := true;

  s32bit := RESULT_S32BIT;
  if s32bit <> RESULT_S32BIT then
    failed := true;

  if failed then
    fail
  else
    WriteLn('Success!');

  Write('left : LOC_REFERENCE, right : LOC_REGISTER tests..');
  failed := false;

  s64bit := getresults64bit;
  if s64bit <> RESULT_S64BIT then
    failed := true;

  s32bit := getresults32bit;
  if s32bit <> RESULT_S32BIT then
    failed := true;

  s16bit := getresults16bit;
  if s16bit <> RESULT_S16BIT then
    failed := true;

  u8bit := getresultu8bit;
  if u8bit <> RESULT_U8BIT then
    failed := true;

  if failed then
    fail
  else
    WriteLn('Success!');

  Write('left : LOC_REFERENCE, right : LOC_FPUREGISTER tests..');
  failed := false;

  real_val := getresultreal;
  if trunc(real_val) <> trunc(RESULT_REAL) then
    failed := true;

  if failed then
    fail
  else
    WriteLn('Success!');

  Write('left : LOC_REFERENCE, right : LOC_REFERENCE tests..');
  failed := false;

  bigrecord1.x := RESULT_U32BIT;
  bigrecord1.y := RESULT_U32BIT;
  for i:=0 to MAX_INDEX do
    bigrecord1.z[i] := RESULT_U8BIT;

  fillchar(bigrecord2, sizeof(bigrecord2),#0);

  bigrecord2 := bigrecord1;

  if bigrecord2.x <> RESULT_U32BIT then
    failed := true;
  if bigrecord2.y <> RESULT_U32BIT then
    failed := true;
  for i:=0 to MAX_INDEX do
    begin
       if bigrecord2.z[i] <> RESULT_U8BIT then
         begin
           failed := true;
           break;
         end;
    end;


  if failed then
    fail
  else
    WriteLn('Success!');

  Write('left : LOC_REFERENCE, right : LOC_JUMP tests (32-bit cpus only!)..');
  {!!!!! This test will only work on 32-bit CPU's probably, on 64-bit CPUs
    the location should be in LOC_FLAGS
  }
  failed := false;

  s64bit := RESULT_S64BIT;
  boolval := s64bit = RESULT_S64BIT;
  if boolval = FALSE then
    failed := true;

  if failed then
    fail
  else
    WriteLn('Success!');


  Write('left : LOC_REFERENCE, right : LOC_FLAGS tests..');
  failed := false;

  s32bit := RESULT_S32BIT;
  boolval := s32bit = RESULT_S32BIT;
  if boolval = FALSE then
    failed := true;


  if failed then
    fail
  else
    WriteLn('Success!');


end.
