unit tcmaskutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit;

Type

  { TTestMaskUtils }

  TTestMaskUtils = Class(TTestCase)
  Published
    Procedure Test1;
  end;

implementation

{ TTestMaskUtils }

procedure TTestMaskUtils.Test1;
begin
  AssertEquals('H1H357-K808K-44616-YK8720',FormatMaskText('!>cccccc\-ccccc\-ccccc\-cccccc;0;*', 'H1H357K808K44616YK8720'))
end;

initialization
  RegisterTest(TTestMaskUtils);
end.

