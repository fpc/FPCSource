{ this tests the round routine }
program ttrunc;

const
  RESULT_ONE = 1235;
  VALUE_ONE = 1234.5678;
  RESULT_CONST_ONE = round(VALUE_ONE);
  RESULT_TWO = -1235;
  VALUE_TWO = -1234.5678;
  RESULT_CONST_TWO = round(VALUE_TWO);


 procedure fail;
  begin
    WriteLn('Failed!');
    halt(1);
  end;

var
 r: real;
 _success : boolean;
 l: longint;
Begin
 Write('Round() testing...');
 _success := true;
 r:=VALUE_ONE;
 if round(r)<>RESULT_ONE then
   _success:=false;
 if round(VALUE_ONE)<>RESULT_ONE then
   _success:=false;
 r:=VALUE_ONE;
 if round(r)<>RESULT_CONST_ONE then
   _success := false;
 r:=VALUE_ONE;
 l:=round(r);
 if l<>RESULT_ONE then
   _success:=false;
 l:=round(VALUE_ONE);
 if l<>RESULT_ONE then
   _success:=false;


 r:=VALUE_TWO;
 if round(r)<>RESULT_TWO then
   _success:=false;
 if round(VALUE_TWO)<>RESULT_TWO then
   _success:=false;
 r:=VALUE_TWO;
 if round(r)<>RESULT_CONST_TWO then
   _success := false;
 r:=VALUE_TWO;
 l:=round(r);
 if l<>RESULT_TWO then
   _success:=false;
 l:=round(VALUE_TWO);
 if l<>RESULT_TWO then
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

