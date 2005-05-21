{ this tests the int routine }
{ Contrary to TP, int can be used in the constant section,
  just like in Delphi }
program tint;

{$ifdef VER1_0}
  {$define SKIP_CURRENCY_TEST}
{$endif }

const
  INT_RESULT_ONE = 1234;
  INT_VALUE_ONE = 1234.5678;
  INT_RESULT_CONST_ONE = Int(INT_VALUE_ONE);
  INT_RESULT_TWO = -1234;
  INT_VALUE_TWO = -1234.5678;
  INT_RESULT_CONST_TWO = Int(INT_VALUE_TWO);


 procedure fail;
  begin
    WriteLn('Failed!');
    halt(1);
  end;

procedure test_int_real;
var
 r: real;
 _success : boolean;
Begin
 Write('Int() real testing...');
 _success := true;
 r:=INT_VALUE_ONE;
 if Int(r)<>INT_RESULT_ONE then
   _success:=false;
 if Int(INT_VALUE_ONE)<>INT_RESULT_ONE then
   _success:=false;
 r:=INT_VALUE_ONE;
 if Int(r)<>INT_RESULT_CONST_ONE then
   _success := false;
 r:=INT_VALUE_ONE;
 r:=Int(r);
 if r<>INT_RESULT_ONE then
   _success:=false;
 r:=Int(INT_VALUE_ONE);
 if r<>INT_RESULT_ONE then
   _success:=false;


 r:=INT_VALUE_TWO;
 if Int(r)<>INT_RESULT_TWO then
   _success:=false;
 if Int(INT_VALUE_TWO)<>INT_RESULT_TWO then
   _success:=false;
 r:=INT_VALUE_TWO;
 if Int(r)<>INT_RESULT_CONST_TWO then
   _success := false;
 r:=INT_VALUE_TWO;
 r:=Int(r);
 if r<>INT_RESULT_TWO then
   _success:=false;
 r:=Int(INT_VALUE_TWO);
 if r<>INT_RESULT_TWO then
   _success:=false;


 if not _success then
   fail;
 WriteLn('Success!');
end;

procedure test_int_single;
var
 r: single;
 _success : boolean;
Begin
 Write('Int() single testing...');
 _success := true;
 r:=INT_VALUE_ONE;
 if Int(r)<>INT_RESULT_ONE then
   _success:=false;
 if Int(INT_VALUE_ONE)<>INT_RESULT_ONE then
   _success:=false;
 r:=INT_VALUE_ONE;
 if Int(r)<>INT_RESULT_CONST_ONE then
   _success := false;
 r:=INT_VALUE_ONE;
 r:=Int(r);
 if r<>INT_RESULT_ONE then
   _success:=false;
 r:=Int(INT_VALUE_ONE);
 if r<>INT_RESULT_ONE then
   _success:=false;


 r:=INT_VALUE_TWO;
 if Int(r)<>INT_RESULT_TWO then
   _success:=false;
 if Int(INT_VALUE_TWO)<>INT_RESULT_TWO then
   _success:=false;
 r:=INT_VALUE_TWO;
 if Int(r)<>INT_RESULT_CONST_TWO then
   _success := false;
 r:=INT_VALUE_TWO;
 r:=Int(r);
 if r<>INT_RESULT_TWO then
   _success:=false;
 r:=Int(INT_VALUE_TWO);
 if r<>INT_RESULT_TWO then
   _success:=false;


 if not _success then
   fail;
 WriteLn('Success!');
end;

procedure test_int_double;
var
 r: double;
 _success : boolean;
Begin
 Write('Int() double testing...');
 _success := true;
 r:=INT_VALUE_ONE;
 if Int(r)<>INT_RESULT_ONE then
   _success:=false;
 if Int(INT_VALUE_ONE)<>INT_RESULT_ONE then
   _success:=false;
 r:=INT_VALUE_ONE;
 if Int(r)<>INT_RESULT_CONST_ONE then
   _success := false;
 r:=INT_VALUE_ONE;
 r:=Int(r);
 if r<>INT_RESULT_ONE then
   _success:=false;
 r:=Int(INT_VALUE_ONE);
 if r<>INT_RESULT_ONE then
   _success:=false;


 r:=INT_VALUE_TWO;
 if Int(r)<>INT_RESULT_TWO then
   _success:=false;
 if Int(INT_VALUE_TWO)<>INT_RESULT_TWO then
   _success:=false;
 r:=INT_VALUE_TWO;
 if Int(r)<>INT_RESULT_CONST_TWO then
   _success := false;
 r:=INT_VALUE_TWO;
 r:=Int(r);
 if r<>INT_RESULT_TWO then
   _success:=false;
 r:=Int(INT_VALUE_TWO);
 if r<>INT_RESULT_TWO then
   _success:=false;


 if not _success then
   fail;
 WriteLn('Success!');
end;

{$ifndef SKIP_CURRENCY_TEST}
procedure test_int_currency;
var
 r: currency;
 _success : boolean;
Begin
 Write('Int() currency testing...');
 _success := true;
 r:=INT_VALUE_ONE;
 if Int(r)<>INT_RESULT_ONE then
   _success:=false;

 if not _success then
   fail;

 if Int(INT_VALUE_ONE)<>INT_RESULT_ONE then
   _success:=false;
 r:=INT_VALUE_ONE;
 if Int(r)<>INT_RESULT_CONST_ONE then
   _success := false;
 r:=INT_VALUE_ONE;
 r:=Int(r);
 if r<>INT_RESULT_ONE then
   _success:=false;
 r:=Int(INT_VALUE_ONE);
 if r<>INT_RESULT_ONE then
   _success:=false;

 if not _success then
   fail;

 r:=INT_VALUE_TWO;
 if Int(r)<>INT_RESULT_TWO then
   _success:=false;
 if Int(INT_VALUE_TWO)<>INT_RESULT_TWO then
   _success:=false;
 r:=INT_VALUE_TWO;
 if Int(r)<>INT_RESULT_CONST_TWO then
   _success := false;
 r:=INT_VALUE_TWO;
 r:=Int(r);
 if r<>INT_RESULT_TWO then
   _success:=false;
 r:=Int(INT_VALUE_TWO);
 if r<>INT_RESULT_TWO then
   _success:=false;


 if not _success then
   fail;
 WriteLn('Success!');
end;
{$endif SKIP_CURRENCY_TEST}

Begin
  test_int_real;
  test_int_double;
  test_int_single;
{$ifdef SKIP_CURRENCY_TEST}
  Writeln('Skipping currency test because its not supported by theis compiler');
{$else SKIP_CURRENCY_TEST}
  test_int_currency;
{$endif SKIP_CURRENCY_TEST}
end.
