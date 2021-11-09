{ %CONFIGFILE=fpcunit-console-defaults.ini testdefaults.ini }

program tests;

{$mode objfpc}

uses
  consoletestrunner, UTestsHMAC, utestsha256, utestonetimepass, sha512, utestsha512, asn, ecc, pem, utestpem, ecdsa;

var
  Application: TTestRunner;
begin
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=True;
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.

