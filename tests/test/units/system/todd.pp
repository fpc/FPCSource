{ Part of System unit testsuit        }
{ Carl Eric Codere Copyright (c) 2002 }
program todd;

const
  RESULT_ONE = FALSE;
  VALUE_ONE = -65536;
  RESULT_CONST_ONE = odd(VALUE_ONE);
  RESULT_TWO = TRUE;
  VALUE_TWO = 12345;
  RESULT_CONST_TWO = odd(VALUE_TWO);


procedure fail;
 begin
  WriteLn('Failure!');
  halt(1);
 end;

var
 r: longint;
 _success : boolean;
 l: boolean;
Begin
 Write('Odd() testing...');
 _success := true;
 r:=VALUE_ONE;
 if odd(r)<>RESULT_ONE then
   _success:=false;
 if odd(VALUE_ONE)<>RESULT_ONE then
   _success:=false;
 r:=VALUE_ONE;
 if odd(r)<>RESULT_CONST_ONE then
   _success := false;
 r:=VALUE_ONE;
 l:=odd(r);
 if l<>RESULT_ONE then
   _success:=false;
 l:=odd(VALUE_ONE);
 if l<>RESULT_ONE then
   _success:=false;


 r:=VALUE_TWO;
 if odd(r)<>RESULT_TWO then
   _success:=false;
 if odd(VALUE_TWO)<>RESULT_TWO then
   _success:=false;
 r:=VALUE_TWO;
 if odd(r)<>RESULT_CONST_TWO then
   _success := false;
 r:=VALUE_TWO;
 l:=odd(r);
 if l<>RESULT_TWO then
   _success:=false;
 l:=odd(VALUE_TWO);
 if l<>RESULT_TWO then
   _success:=false;


 if not _success then
   fail;
 WriteLn('Success!');
end.
