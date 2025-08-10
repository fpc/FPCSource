unit utcMaskUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, punit, maskutils;

procedure RegisterTests;

implementation

function TMaskUtils_Test1: TTestString;
begin
  Result := '';
  AssertEquals('Test1', 'H1H357-K808K-44616-YK8720', FormatMaskText('!>cccccc\-ccccc\-ccccc\-cccccc;0;*', 'H1H357K808K44616YK8720'));
end;

function TMaskUtils_Test2: TTestString;
begin
  Result := '';
  AssertEquals('Test2', '555.   .   .   ', FormatMaskText('999.999.999.999', '555555'));
end;

function TMaskUtils_Test3: TTestString;
begin
  Result := '';
  AssertEquals('Test3', '555.   .   .   ', FormatMaskText('999.999.999.999;1;_', '555555'));
end;

procedure RegisterTests;
begin
  AddSuite('TMaskUtilsTests');
  AddTest('Test1', @TMaskUtils_Test1, 'TMaskUtilsTests');
  AddTest('Test2', @TMaskUtils_Test2, 'TMaskUtilsTests');
  AddTest('Test3', @TMaskUtils_Test3, 'TMaskUtilsTests');
end;

end.
