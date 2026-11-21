{
  This file is part of the Free Component Library.
  Test runner for fcl-quic; QUIC_OPENSSL_DIR selects the directory holding libssl/libcrypto 3.5+.
  See the file COPYING.FPC, included in this distribution, for details about the copyright.
}
program testquic;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads, cwstring,{$endif}
  consoletestrunner, utquicbuffers, utquicframes, utquicpackets, utquictransportparams, utquiccrypto,
  utquicopenssltls, utquicstreams, utquicrecovery, utquicconnection, utquicearlydata, utquicudp, utquicendpoint;

var
  Application: TTestRunner;

begin
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=True;
  Application:=TTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
