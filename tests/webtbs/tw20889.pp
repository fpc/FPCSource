{$APPTYPE CONSOLE}
program qwordbool_test_02;
//=====================
//
// Using function which returns QWORDBOOL leads to 'Internal error 200410105' during compile in some usage patterns
//
// r19740-win32
//
//=====================
// Sample:
//
function qbool_result(something:integer):qwordbool;
  begin 
    qbool_result:=(something<>0);
  end;

var   test:boolean;
begin 
  test:=qbool_result(123);  //here(17,13) Fatal: Internal error 200410105
  if not(test) then
    halt(1);
  writeln('ok');      
end.
