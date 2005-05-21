{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{****************************************************************}
{ NODE TESTED : secondcase()                                     }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{                 secondcalln()                                  }
{****************************************************************}
{ DEFINES:                                                       }
{****************************************************************}
{ REMARKS: Tests the case statement (except jump table gen.)     }
{****************************************************************}
program tcase;

{$ifdef FPC}
    {$IFNDEF ver1_0}
        {$define int64_Test}
    {$endif}
{$else}
    {$define int64_Test}
{$endif}

{
   The value is in LOC_REGISTER (operand to test)
}

procedure fail;
begin
  WriteLn('Failed!');
  halt(1);
end;


{************************************************************************}
{                            LINEAR LIST                                 }
{************************************************************************}

{   low = high           }
procedure TestCmpListOneShort;
 var
  s: smallint;
  failed :boolean;
 begin
   Write('Linear Comparison list without ranges (smallint)...');
   s := -12;
   failed := true;
   case s of
   -12 : failed := false;
   -10 : ;
   3 : ;
   else
   end;
   if failed then
     fail
   else
     WriteLn('Passed!');
 end;

{   low = high           }
procedure TestCmpListTwoShort;
 var
  s: smallint;
  failed :boolean;
 begin
   Write('Linear Comparison list without ranges (smallint)...');
   s := 30000;
   failed := true;
   case s of
   -12 : ;
   -10 : ;
   3 : ;
   else
     failed := false;
   end;
   if failed then
     fail
   else
     WriteLn('Passed!');
 end;


{   low = high           }
procedure TestCmpListOneWord;
 var
  s: word;
  failed :boolean;
 begin
   Write('Linear Comparison list without ranges (word)...');
   s := 12;
   failed := true;
   case s of
   12 : failed := false;
   10 : ;
   3 : ;
   end;
   if failed then
     fail
   else
     WriteLn('Passed!');
 end;

{   low = high           }
procedure TestCmpListTwoWord;
 var
  s: word;
  failed :boolean;
 begin
   Write('Linear Comparison list without ranges (word)...');
   s := 30000;
   failed := true;
   case s of
   0 : ;
   512 : ;
   3 : ;
   else
     failed := false;
   end;
   if failed then
     fail
   else
     WriteLn('Passed!');
 end;

{$IFDEF INT64_TEST}
{   low = high           }
procedure TestCmpListOneInt64;
 var
  s: int64;
  failed :boolean;
 begin
   Write('Linear Comparison list without ranges (int64)...');
   s := 3000000;
   failed := true;
   case s of
   3000000 : failed := false;
   10 : ;
   3 : ;
   end;
   if failed then
     fail
   else
     WriteLn('Passed!');
 end;

{   low = high           }
procedure TestCmpListTwoInt64;
 var
  s: int64;
  failed :boolean;
 begin
   Write('Linear Comparison list without ranges (int64)...');
   s := 30000;
   failed := true;
   case s of
   0 : ;
   512 : ;
   3 : ;
   else
     failed := false;
   end;
   if failed then
     fail
   else
     WriteLn('Passed!');
 end;

 {   low = high           }
 procedure TestCmpListThreeInt64;
  var
   s: int64;
   l : longint;
   failed :boolean;
  begin
    Write('Linear Comparison list without ranges (int64)...');
    l:=3000000;
    s := (int64(l) shl 32);
    failed := true;
    case s of
    (int64(3000000) shl 32) : failed := false;
    10 : ;
    3 : ;
    end;
    if failed then
      fail
    else
      WriteLn('Passed!');
  end;
{$ENDIF}


procedure TestCmpListRangesOneShort;
 var
  s: smallint;
  failed :boolean;
 begin
   Write('Linear Comparison list with ranges (smallint)...');
   s := -12;
   failed := true;
   case s of
   -12..-8 : failed := false;
   -7 : ;
   3 : ;
   else
   end;
   if failed then
     fail
   else
     WriteLn('Passed!');
 end;

procedure TestCmpListRangesTwoShort;
 var
  s: smallint;
  failed :boolean;
 begin
   Write('Linear Comparison list with ranges (smallint)...');
   s := 30000;
   failed := true;
   case s of
   -12..-8 : ;
   -7 : ;
   3 : ;
   else
     failed := false;
   end;
   if failed then
     fail
   else
     WriteLn('Passed!');
 end;


{   low = high           }
procedure TestCmpListRangesOneWord;
 var
  s: word;
  failed :boolean;
 begin
   Write('Linear Comparison list with ranges (word)...');
   s := 12;
   failed := true;
   case s of
   12..13 : failed := false;
   10 : ;
   3..7 : ;
   end;
   if failed then
     fail
   else
     WriteLn('Passed!');
 end;

{   low = high           }
procedure TestCmpListRangesTwoWord;
 var
  s: word;
  failed :boolean;
 begin
   Write('Linear Comparison list with ranges (word)...');
   s := 30000;
   failed := true;
   case s of
   0..2 : ;
   3..29999 : ;
   else
     failed := false;
   end;
   if failed then
     fail
   else
     WriteLn('Passed!');
 end;


 procedure TestCmpListRangesThreeWord;
  var
   s: word;
   failed :boolean;
  begin
    Write('Linear Comparison list with ranges (word)...');
    s := 3;
    failed := true;
    case s of
    12..13 : ;
    10 : ;
    3..7 : failed := false;
    end;
    if failed then
      fail
    else
      WriteLn('Passed!');
  end;


{$IFDEF INT64_TEST}
{   low = high           }
procedure TestCmpListRangesOneInt64;
 var
  s: int64;
  failed :boolean;
 begin
   Write('Linear Comparison list with ranges (int64)...');
   s := 3000000;
   failed := true;
   case s of
   11..3000000 : failed := false;
   10 : ;
   0..2 : ;
   end;
   if failed then
     fail
   else
     WriteLn('Passed!');
 end;

{   low = high           }
procedure TestCmpListRangesTwoInt64;
 var
  s: int64;
  failed :boolean;
 begin
   Write('Linear Comparison list with ranges (int64)...');
   s := 30000;
   failed := true;
   case s of
   513..10000 : ;
   512 : ;
   0..3 : ;
   else
     failed := false;
   end;
   if failed then
     fail
   else
     WriteLn('Passed!');
 end;
{$ENDIF}

Begin
  TestCmpListOneShort;
  TestCmpListTwoShort;
  TestCmpListOneWord;
  TestCmpListTwoWord;
  TestCmpListRangesOneShort;
  TestCmpListRangesTwoShort;
  TestCmpListRangesOneWord;
  TestCmpListRangesTwoWord;
  TestCmpListRangesThreeWord;
{$ifdef int64_test}
  TestCmpListOneInt64;
  TestCmpListTwoInt64;
  TestCmpListThreeInt64;
  TestCmpListRangesOneInt64;
  TestCmpListRangesTwoInt64;
{$endif}
end.
