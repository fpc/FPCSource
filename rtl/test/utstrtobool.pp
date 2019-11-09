unit utstrtobool;

{$mode objfpc}

Interface

uses
  sysutils;
  
implementation
  
uses utrtl, punit;

Function TestStrToBool : AnsiString;
  
var
  b : boolean;
  FS : TFormatSettings;

begin
  Result:='';
  if not TryStrToBool('true',b) then
    exit('Test 1');
  if not b then
    exit('Test 2');
  if not TryStrToBool('false',b) then
    exit('Test 3');
  if b then
    exit('Test 4');

  if not TryStrToBool('True',b) then
    exit('Test 5');
  if not b then
    exit('Test 6');
  if not TryStrToBool('False',b) then
    exit('Test 7');
  if b then
    exit('Test 8');

  if not TryStrToBool('truE',b) then
    exit('Test 9');
  if not b then
    exit('Test 10');
  if not TryStrToBool('falsE',b) then
    exit('Test 11');
  if b then
    exit('Test 12');

  if not TryStrToBool('TRUE',b) then
    exit('Test 13');
  if not b then
    exit('Test 14');
  if not TryStrToBool('FALSE',b) then
    exit('Test 15');
  if b then
    exit('Test 16');

  if not TryStrToBool('3.1415',b) then
    exit('Test 17');
  if not b then
    exit('Test 18');
  if not TryStrToBool('0.0',b) then
    exit('Test 19');
  if b then
    exit('Test 19');

  if TryStrToBool('',b) then
    exit('Test 20');

  if TryStrToBool('asdf',b) then
    exit('Test 21');


  b:=StrToBool('truE');
  if not b then
    exit('Test 22');
  b:=StrToBool('falsE');
  if b then
    exit('Test 23');

  if not(StrToBoolDef('',true)) then
    exit('Test 24');

  if StrToBoolDef('asdf',false) then
    exit('Test 25');

  FS:=DefaultFormatSettings;
  FS.DecimalSeparator:=',';
  If Not TryStrToBool('1,2',B,FS) then
    Exit('test 26');

end;


begin
  SysUtilsTest('TestStrToBool',@TestStrToBool);
end.
