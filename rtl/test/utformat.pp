unit utformat;
{$mode objfpc}{$h+}

interface

uses sysutils;

implementation

uses punit, utrtl;

function testformat : string;

begin
  Result:='';
  if not AssertEquals('Test 1','>         def<', format('>%1:*s<',[0, 12,'def',-15])) then exit;
  if not AssertEquals('Test 2','>         abc< >       def<',format('>%1:*s< >%*s<', [0, 12, 'abc', 10, 'def'])) then exit;
  if not AssertEquals('Test 3','>       abc< >   def<',format('>%1:*.*s< >%*.*s<', [0, 10,10,'abc', 6,6,'def'])) then exit;
end;
    
begin
  SysutilsTest('format',@testformat);
end.
