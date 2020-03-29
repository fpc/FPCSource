program fppkg_tests;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}
  cthreads,
  {$endif unix}
  Classes,
  CustApp,
  consoletestrunner,
  FullFPCInstallationTests;

type

  { TFppkgTestRunner }

  TFppkgTestRunner = class(TTestRunner)
  protected
    procedure AppendLongOpts; override;
    function GetShortOpts: string; override;
  end;

procedure TFppkgTestRunner.AppendLongOpts;
begin
  inherited AppendLongOpts;
  LongOpts.Add('fpcsrcpath::');
  LongOpts.Add('testpath:');
  LongOpts.Add('startcompiler:');
  LongOpts.Add('skipbuildtemplate');
  LongOpts.Add('packagespath:');
end;

function TFppkgTestRunner.GetShortOpts: string;
begin
  Result := inherited GetShortOpts;
  Result := Result + 'f:t:s:Tp:';
end;

begin
  CustomApplication := TFppkgTestRunner.Create(nil);
  CustomApplication.Initialize;
  CustomApplication.Title := 'FPCUnit Console test runner';
  CustomApplication.Run;
  CustomApplication.Free;
end.
