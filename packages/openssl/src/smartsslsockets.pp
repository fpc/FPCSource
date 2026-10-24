{
    This file is part of the Free Component Library (FCL).
    Use an OpenSSL library shipped with the application, or the system TLS.
    See COPYING.FPC for details about the copyright.

    On Windows and macOS, using this unit selects the TLS handler at startup:
    OpenSSL if its libraries load from the application directory, otherwise
    the system TLS (Schannel on Windows, Network.framework on macOS).
    On other systems it is the same as using opensslsockets.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit smartsslsockets;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

{$IF DEFINED(WIN32) OR DEFINED(WIN64)}
  {$DEFINE SMART_SCHANNEL}
{$ELSEIF DEFINED(DARWIN) AND NOT DEFINED(IOS) AND NOT DEFINED(IPHONESIM)}
  {$DEFINE SMART_NETWORKFRAMEWORK}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Net.Opensslsockets
  {$IFDEF SMART_SCHANNEL}, System.Net.SchannelSSLSockets{$ENDIF}
  {$IFDEF SMART_NETWORKFRAMEWORK}, System.Net.NetworkFrameworkSSLSockets{$ENDIF};
{$ELSE FPC_DOTTEDUNITS}
uses
  opensslsockets
  {$IFDEF SMART_SCHANNEL}, schannelsslsockets{$ENDIF}
  {$IFDEF SMART_NETWORKFRAMEWORK}, networkframeworksslsockets{$ENDIF};
{$ENDIF FPC_DOTTEDUNITS}

implementation

{$IF DEFINED(SMART_SCHANNEL) OR DEFINED(SMART_NETWORKFRAMEWORK)}

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Net.Sslsockets, Api.Openssl;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, sslsockets, openssl;
{$ENDIF FPC_DOTTEDUNITS}

{$IFDEF SMART_NETWORKFRAMEWORK}
function NSGetExecutablePath(aBuffer: PAnsiChar; var aSize: Cardinal): Integer; cdecl; external name '_NSGetExecutablePath';

// ParamStr(0) is argv[0] on Darwin, which need not contain a directory.
function ApplicationDirectory: String;
var
  Buffer : Array[0..4095] of AnsiChar;
  Size : Cardinal;
begin
  Size:=SizeOf(Buffer);
  if NSGetExecutablePath(@Buffer[0],Size)=0 then
    Result:=ExtractFilePath(ExpandFileName(StrPas(@Buffer[0])))
  else
    Result:='';
end;

// Same names and order as InitSSLInterface, in aDir only.
function LoadBundledOpenSSL(const aDir: String): Boolean;
var
  I : Integer;
begin
  Result:=False;
  for I:=Low(DLLVersions)+StartVersionOffset to High(DLLVersions) do
    if InitSSLInterface(aDir+BaseSSLName+DLLVersions[I]+'.dylib',aDir+BaseCryptoName+DLLVersions[I]+'.dylib') then
      Exit(True);
end;
{$ELSE}
function ApplicationDirectory: String;
begin
  Result:=ExtractFilePath(ParamStr(0));
end;

// Same names and order as InitSSLInterface, in aDir only.
function LoadBundledOpenSSL(const aDir: String): Boolean;
var
  I : Integer;
begin
  Result:=False;
  for I:=Low(SSL_DLL_Names) to High(SSL_DLL_Names) do
    if InitSSLInterface(aDir+SSL_DLL_Names[I]+'.dll',aDir+Crypto_DLL_Names[I]+'.dll') then
      Exit(True);
end;
{$ENDIF}

procedure SelectHandler;
var
  Dir : String;
begin
  Dir:=ApplicationDirectory;
  if (Dir<>'') and LoadBundledOpenSSL(Dir) then
    TSSLSocketHandler.SetDefaultHandlerClass(TOpenSSLSocketHandler)
  else
{$IFDEF SMART_SCHANNEL}
    TSSLSocketHandler.SetDefaultHandlerClass(TSchannelSocketHandler);
{$ELSE}
    TSSLSocketHandler.SetDefaultHandlerClass(TNetworkFrameworkSocketHandler);
{$ENDIF}
end;

initialization
  SelectHandler;
{$ENDIF}
end.
