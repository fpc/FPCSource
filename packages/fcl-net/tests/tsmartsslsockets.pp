program tsmartsslsockets;

{ Tests the TLS handler selected by smartsslsockets and makes one HTTPS
  request with it. Needs network access.
  Usage: tsmartsslsockets <expected handler class name> }

{$mode objfpc}{$H+}

uses
{$IFDEF TEST_DOTTEDUNITS}
  System.SysUtils, System.Net.Sslsockets, FpWeb.Http.Client, System.Net.SmartSSLSockets;
{$ELSE}
  SysUtils, sslsockets, fphttpclient, smartsslsockets;
{$ENDIF}

var
  Selected : String;
  C : TFPHTTPClient;

begin
  Selected:=TSSLSocketHandler.GetDefaultHandlerClass.ClassName;
  Writeln('Selected ',Selected);
  if not SameText(Selected,ParamStr(1)) then
    begin
    Writeln('FAIL: expected ',ParamStr(1));
    Halt(1);
    end;
  C:=TFPHTTPClient.Create(Nil);
  try
    C.ConnectTimeout:=10000;
    C.IOTimeout:=10000;
    C.Get('https://example.com/');
    if C.ResponseStatusCode<>200 then
      begin
      Writeln('FAIL: HTTP status ',C.ResponseStatusCode);
      Halt(1);
      end;
  finally
    C.Free;
  end;
  Writeln('PASS');
end.
