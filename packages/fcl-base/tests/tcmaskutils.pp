unit tcmaskutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, maskutils;

Type

  { TTestMaskUtils }

  TTestMaskUtils = Class(TTestCase)
  Published
    Procedure Test1;
    Procedure Test2;
    Procedure Test3;
  end;

implementation

{ TTestMaskUtils }

procedure TTestMaskUtils.Test1;
begin
  AssertEquals('H1H357-K808K-44616-YK8720',FormatMaskText('!>cccccc\-ccccc\-ccccc\-cccccc;0;*', 'H1H357K808K44616YK8720'))
end;

procedure TTestMaskUtils.Test2;
begin
  AssertEquals('555.   .   .   ',FormatMaskText('999.999.999.999','555555'));
end;

procedure TTestMaskUtils.Test3;
begin
  AssertEquals('555.   .   .   ',FormatMaskText('999.999.999.999;1;_','555555'));
end;

initialization
  RegisterTest(TTestMaskUtils);
end.

