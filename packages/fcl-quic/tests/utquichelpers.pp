{
  This file is part of the Free Component Library.
  Shared helpers for the fcl-quic tests: OpenSSL loading and the cipher factory.
  See the file COPYING.FPC, included in this distribution, for details about the copyright.
}
unit utquichelpers;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, FpQuic.Crypto, FpQuic.OpenSSL;

// Loads OpenSSL from $QUIC_OPENSSL_DIR (or the default search path) once; raises when it fails.
procedure NeedOpenSSL;

// Cipher factory shared by all tests.
function TestCipherFactory: TQuicOpenSSLCipherFactory;

// Directory containing the test certificate and key.
function TestDataDir: String;

// Contents of a file.
function FileBytes(const aFileName: String): TBytes;

implementation

const
{$IFDEF WINDOWS}
  SSLLibraryName = 'libssl-3-x64.dll';
  CryptoLibraryName = 'libcrypto-3-x64.dll';
{$ELSE}
  SSLLibraryName = 'libssl.so.3';
  CryptoLibraryName = 'libcrypto.so.3';
{$ENDIF}

var
  GFactory: TQuicOpenSSLCipherFactory;
  GLoaded: Boolean;

procedure NeedOpenSSL;

var
  lDir: String;

begin
  if GLoaded then
    Exit;
  lDir:=GetEnvironmentVariable('QUIC_OPENSSL_DIR');
  if lDir<>'' then
    GLoaded:=QuicInitOpenSSL(IncludeTrailingPathDelimiter(lDir)+SSLLibraryName,
                             IncludeTrailingPathDelimiter(lDir)+CryptoLibraryName)
  else
    GLoaded:=QuicInitOpenSSL;
  if not GLoaded then
    raise Exception.Create('Cannot load OpenSSL 3.5 or later');
end;


function TestCipherFactory: TQuicOpenSSLCipherFactory;

begin
  NeedOpenSSL;
  if GFactory=nil then
    GFactory:=TQuicOpenSSLCipherFactory.Create;
  Result:=GFactory;
end;


function TestDataDir: String;

begin
  Result:=ExtractFilePath(ParamStr(0))+'../../tests/data/';
end;


function FileBytes(const aFileName: String): TBytes;

var
  lFile: TFileStream;

begin
  Result:=nil;
  lFile:=TFileStream.Create(aFileName,fmOpenRead);
  try
    SetLength(Result,lFile.Size);
    lFile.ReadBuffer(Result[0],lFile.Size);
  finally
    lFile.Free;
  end;
end;


finalization
  GFactory.Free;
end.
