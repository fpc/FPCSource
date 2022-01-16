program com_clnt;
// Comtest demo from Anton K. mantis #35013

{$ifdef fpc}{$mode delphi}{$endif}
uses variants,sysutils,classes,activex,comobj;

var co,resp:variant;
begin
  co := CreateOleObject('com_serv.TestApp');

  if (VarIsEmpty(co)) then halt(1);

  try 
    co.test('Hello1');
    resp:=widestring('yyyyy');
    co.test_ret(resp);
    writeln(resp);
    if (resp<>'zzzz') then halt(2);
  except
    on E:Exception do
    begin
      writeln(E.Message);
      halt(3);
    end;
  end;
  writeln('Success!');

end.