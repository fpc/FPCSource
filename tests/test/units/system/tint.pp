{ this tests the int routine }
{ Contrary to TP, int can be used in the constant section,
  just like in Delphi }
program tint;

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

Begin
  test_int_real;
  test_int_double;
  test_int_single;
  test_int_currency;
end.

{
  $Log$
  Revision 1.2  2002-09-18 18:30:30  carl
    + currency testing
    * more system unit routine testing

  Revision 1.1  2002/09/16 19:15:54  carl
    * several new routines have a testsuit.

}

