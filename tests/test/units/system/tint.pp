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

var
 r: real;
 _success : boolean;
Begin
 Write('Int() testing...');
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
end.

{
  $Log$
  Revision 1.1  2002-09-16 19:15:54  carl
    * several new routines have a testsuit.

}

