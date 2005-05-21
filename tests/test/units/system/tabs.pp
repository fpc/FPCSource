{ Part of System unit testsuit        }
{ Carl Eric Codere Copyright (c) 2002 }
program tabs;

{$ifdef VER1_0}
  {$define SKIP_CURRENCY_TEST}
{$endif }

{$ifndef MACOS}
{$APPTYPE CONSOLE}
{$else}
{$APPTYPE TOOL}
{$endif}

{$R+}
{$Q+}

const
  RESULT_ONE_INT = 65536;
  VALUE_ONE_INT = -65536;
  RESULT_CONST_ONE_INT = abs(VALUE_ONE_INT);
  RESULT_TWO_INT = 12345;
  VALUE_TWO_INT = 12345;
  RESULT_CONST_TWO_INT = abs(VALUE_TWO_INT);

  RESULT_THREE_INT = 2147483647;
  VALUE_THREE_INT = -2147483647;
  RESULT_CONST_THREE_INT = abs(VALUE_THREE_INT);
  RESULT_FOUR_INT = 2147483647;
  VALUE_FOUR_INT = 2147483647;
  RESULT_CONST_FOUR_INT = abs(VALUE_FOUR_INT);


  RESULT_ONE_REAL = 12345.6789;
  VALUE_ONE_REAL = -12345.6789;
  RESULT_CONST_ONE_REAL = abs(VALUE_ONE_REAL);
  RESULT_TWO_REAL = 54321.6789E+02;
  VALUE_TWO_REAL = 54321.6789E+02;
  RESULT_CONST_TWO_REAL = abs(VALUE_TWO_REAL);

  RESULT_THREE_REAL = 0.0;
  VALUE_THREE_REAL = 0.0;
  RESULT_CONST_THREE_REAL = abs(VALUE_THREE_REAL);
  RESULT_FOUR_REAL = 12.0;
  VALUE_FOUR_REAL = -12.0;
  RESULT_CONST_FOUR_REAL = abs(VALUE_FOUR_REAL);


procedure fail;
 begin
  WriteLn('Failure!');
  halt(1);
 end;


{$ifndef SKIP_CURRENCY_TEST}
 procedure test_abs_currency;
  var
   _result : boolean;
   value : currency;
   value1: currency;
  begin
    Write('Abs() test with currency type...');
    _result := true;

    value := VALUE_ONE_REAL;
    if (trunc(abs(value)) <> trunc(RESULT_CONST_ONE_REAL))  then
       _result := false;

    value := VALUE_TWO_REAL;
    if trunc(abs(value)) <> trunc(RESULT_CONST_TWO_REAL) then
       _result := false;

    value := VALUE_THREE_REAL;
    if trunc(abs(value)) <> trunc(RESULT_CONST_THREE_REAL) then
       _result := false;

    value := VALUE_FOUR_REAL;
    if trunc(abs(value)) <> trunc(RESULT_CONST_FOUR_REAL) then
       _result := false;

    value := VALUE_ONE_REAL;
    value1 := abs(value);
    if trunc(value1) <> trunc(RESULT_ONE_REAL) then
       _result := false;

    value := VALUE_TWO_REAL;
    value1 := abs(value);
    if trunc(value1) <> trunc(RESULT_TWO_REAL) then
       _result := false;

    value := VALUE_THREE_REAL;
    value1 := abs(value);
    if trunc(value1) <> trunc(RESULT_THREE_REAL) then
       _result := false;

    value := VALUE_FOUR_REAL;
    value1 := abs(value);
    if trunc(value1) <> trunc(RESULT_FOUR_REAL) then
       _result := false;


    if not _result then
      fail
    else
      WriteLn('Success!');
  end;
{$endif SKIP_CURRENCY_TEST}



 procedure test_abs_int64;
  var
   _result : boolean;
   value : int64;
   value1: int64;
  begin
    Write('Abs() test with int64 type...');
    _result := true;

   value := VALUE_ONE_INT;
    if (abs(value) <> (RESULT_CONST_ONE_INT))  then
       _result := false;


    value := VALUE_TWO_INT;
    if abs(value) <> (RESULT_CONST_TWO_INT) then
       _result := false;

    value := VALUE_THREE_INT;
    if abs(value) <> (RESULT_CONST_THREE_INT) then
       _result := false;

    value := VALUE_FOUR_INT;
    if abs(value) <> (RESULT_CONST_FOUR_INT) then
       _result := false;

    value := VALUE_ONE_INT;
    value1 := abs(value);
    if value1 <> (RESULT_ONE_INT) then
       _result := false;

    value := VALUE_TWO_INT;
    value1 := abs(value);
    if value1 <> (RESULT_TWO_INT) then
       _result := false;

    value := VALUE_THREE_INT;
    value1 := abs(value);
    if value1 <> (RESULT_THREE_INT) then
       _result := false;

    value := VALUE_FOUR_INT;
    value1 := abs(value);
    if value1 <> (RESULT_FOUR_INT) then
       _result := false;

    if not _result then
      fail
    else
      WriteLn('Success!');
  end;


 procedure test_abs_longint;
  var
   _result : boolean;
   value : longint;
   value1: longint;
  begin
    Write('Abs() test with longint type...');
    _result := true;

   value := VALUE_ONE_INT;
    if (abs(value) <> (RESULT_CONST_ONE_INT))  then
       _result := false;


    value := VALUE_TWO_INT;
    if abs(value) <> (RESULT_CONST_TWO_INT) then
       _result := false;

    value := VALUE_THREE_INT;
    if abs(value) <> (RESULT_CONST_THREE_INT) then
       _result := false;

    value := VALUE_FOUR_INT;
    if abs(value) <> (RESULT_CONST_FOUR_INT) then
       _result := false;

    value := VALUE_ONE_INT;
    value1 := abs(value);
    if value1 <> (RESULT_ONE_INT) then
       _result := false;

    value := VALUE_TWO_INT;
    value1 := abs(value);
    if value1 <> (RESULT_TWO_INT) then
       _result := false;

    value := VALUE_THREE_INT;
    value1 := abs(value);
    if value1 <> (RESULT_THREE_INT) then
       _result := false;

    value := VALUE_FOUR_INT;
    value1 := abs(value);
    if value1 <> (RESULT_FOUR_INT) then
       _result := false;

    if not _result then
      fail
    else
      WriteLn('Success!');
  end;

 procedure test_abs_real;
  var
   _result : boolean;
   value : real;
   value1: real;
  begin
    _result := true;
    Write('Abs() test with real type...');

    value := VALUE_ONE_REAL;
    if (trunc(abs(value)) <> trunc(RESULT_CONST_ONE_REAL))  then
       _result := false;

    value := VALUE_TWO_REAL;
    if trunc(abs(value)) <> trunc(RESULT_CONST_TWO_REAL) then
       _result := false;

    value := VALUE_THREE_REAL;
    if trunc(abs(value)) <> trunc(RESULT_CONST_THREE_REAL) then
       _result := false;

    value := VALUE_FOUR_REAL;
    if trunc(abs(value)) <> trunc(RESULT_CONST_FOUR_REAL) then
       _result := false;

    value := VALUE_ONE_REAL;
    value1 := abs(value);
    if trunc(value1) <> trunc(RESULT_ONE_REAL) then
       _result := false;

    value := VALUE_TWO_REAL;
    value1 := abs(value);
    if trunc(value1) <> trunc(RESULT_TWO_REAL) then
       _result := false;

    value := VALUE_THREE_REAL;
    value1 := abs(value);
    if trunc(value1) <> trunc(RESULT_THREE_REAL) then
       _result := false;

    value := VALUE_FOUR_REAL;
    value1 := abs(value);
    if trunc(value1) <> trunc(RESULT_FOUR_REAL) then
       _result := false;

    if not _result then
      fail
    else
      WriteLn('Success!');
  end;

var
 r: longint;
 _success : boolean;
 l: boolean;
Begin
{$ifdef SKIP_CURRENCY_TEST}
  Writeln('Skipping currency test because its not supported by theis compiler');
{$else SKIP_CURRENCY_TEST}
  test_abs_currency;
{$endif SKIP_CURRENCY_TEST}
  test_abs_real;
  test_abs_longint;
  test_abs_int64;
end.
