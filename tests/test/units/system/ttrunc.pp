{ this tests the trunc routine }
program ttrunc;

const
  RESULT_ONE = 1234;
  VALUE_ONE = 1234.5678;
  RESULT_CONST_ONE = trunc(VALUE_ONE);
  RESULT_TWO = -1234;
  VALUE_TWO = -1234.5678;
  RESULT_CONST_TWO = trunc(VALUE_TWO);


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
 Write('Trunc() testing...');
 _success := true;
 r:=VALUE_ONE;
 if Trunc(r)<>RESULT_ONE then
   _success:=false;
 if Trunc(VALUE_ONE)<>RESULT_ONE then
   _success:=false;
 r:=VALUE_ONE;
 if Trunc(r)<>RESULT_CONST_ONE then
   _success := false;
 r:=VALUE_ONE;
 l:=Trunc(r);
 if l<>RESULT_ONE then
   _success:=false;
 l:=Trunc(VALUE_ONE);
 if l<>RESULT_ONE then
   _success:=false;


 r:=VALUE_TWO;
 if Trunc(r)<>RESULT_TWO then
   _success:=false;
 if Trunc(VALUE_TWO)<>RESULT_TWO then
   _success:=false;
 r:=VALUE_TWO;
 if Trunc(r)<>RESULT_CONST_TWO then
   _success := false;
 r:=VALUE_TWO;
 l:=Trunc(r);
 if l<>RESULT_TWO then
   _success:=false;
 l:=Trunc(VALUE_TWO);
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

