{
  GoogleDiscovery.Http - HTTP client for fetching discovery documents

  Provides HTTP GET functionality for fetching Google API discovery documents.
}
unit GoogleDiscovery.Http;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpJson.Data,
  {$ELSE}
  Classes, SysUtils, fpjson,
  {$ENDIF}
  GoogleDiscovery.Types;

type
  { Exception for HTTP operations }
  EHttpException = class(Exception);
  EHttpRequestException = class(EHttpException);
  EHttpTimeoutException = class(EHttpException);

{ HTTP GET operations }
function HttpGet(const AUrl: string; ATimeout: Integer = 30000): THttpResponse;
function HttpGetString(const AUrl: string; ATimeout: Integer = 30000): string;

{ JSON fetching }
function HttpGetJson(const AUrl: string; ATimeout: Integer = 30000): TJSONData;
function HttpGetJsonObject(const AUrl: string; ATimeout: Integer = 30000): TJSONObject;

{ Discovery API specific }
function FetchDiscoveryIndex(const ADiscoveryUrl: string): TJSONObject;
function FetchServiceDiscovery(const ADiscoveryRestUrl: string): TJSONObject;

{ URL utilities }
function IsValidUrl(const AUrl: string): Boolean;
function NormalizeUrl(const AUrl: string): string;
function UrlEncode(const AValue: string): string;
function BuildQueryString(const AParams: array of string): string;

{ Configuration }
procedure SetHttpUserAgent(const AUserAgent: string);
function GetHttpUserAgent: string;
procedure SetHttpTimeout(ATimeout: Integer);
function GetHttpTimeout: Integer;

implementation

uses
  {$IFDEF FPC_DOTTEDUNITS}
  FpWeb.Http.Client, Fcl.UriParser,
  {$ELSE}
  fphttpclient, URIParser,
  {$ENDIF}
  GoogleDiscovery.Json, GoogleDiscovery.Logging;

var
  GUserAgent: string = 'GoogleDiscovery-Pascal/1.0';
  GDefaultTimeout: Integer = 30000;

{ HTTP GET operations }

function HttpGet(const AUrl: string; ATimeout: Integer): THttpResponse;
var
  Client: TFPHTTPClient;
  ResponseStream: TStringStream;
begin
  Result := THttpResponse.Create;

  if not IsValidUrl(AUrl) then
  begin
    Result.Success := False;
    Result.ErrorMessage := 'Invalid URL: ' + AUrl;
    Exit;
  end;

  Client := TFPHTTPClient.Create(nil);
  ResponseStream := TStringStream.Create('');
  try
    Client.AddHeader('User-Agent', GUserAgent);
    Client.AddHeader('Accept', 'application/json');
    Client.ConnectTimeout := ATimeout;
    Client.IOTimeout := ATimeout;

    try
      Client.Get(AUrl, ResponseStream);
      Result.StatusCode := Client.ResponseStatusCode;
      Result.Body := ResponseStream.DataString;
      Result.Success := (Result.StatusCode >= 200) and (Result.StatusCode < 300);

      if not Result.Success then
        Result.ErrorMessage := Format('HTTP %d: %s', [Result.StatusCode, Client.ResponseStatusText]);

    except
      on E: Exception do
      begin
        Result.Success := False;
        Result.ErrorMessage := 'Request failed: ' + E.Message;
        LogError('HTTP request failed for %s: %s', [AUrl, E.Message]);
      end;
    end;
  finally
    ResponseStream.Free;
    Client.Free;
  end;
end;

function HttpGetString(const AUrl: string; ATimeout: Integer): string;
var
  Response: THttpResponse;
begin
  Response := HttpGet(AUrl, ATimeout);
  if Response.Success then
    Result := Response.Body
  else
    raise EHttpRequestException.Create(Response.ErrorMessage);
end;

{ JSON fetching }

function HttpGetJson(const AUrl: string; ATimeout: Integer): TJSONData;
var
  Response: THttpResponse;
begin
  Response := HttpGet(AUrl, ATimeout);

  if not Response.Success then
    raise EHttpRequestException.Create(Response.ErrorMessage);

  Result := ParseJson(Response.Body);
end;

function HttpGetJsonObject(const AUrl: string; ATimeout: Integer): TJSONObject;
var
  Data: TJSONData;
begin
  Data := HttpGetJson(AUrl, ATimeout);

  if Data.JSONType <> jtObject then
  begin
    Data.Free;
    raise EHttpRequestException.Create('Expected JSON object but got ' + Data.ClassName);
  end;

  Result := TJSONObject(Data);
end;

{ Discovery API specific }

function FetchDiscoveryIndex(const ADiscoveryUrl: string): TJSONObject;
begin
  LogInfo('Fetching discovery index from: %s', [ADiscoveryUrl]);
  Result := HttpGetJsonObject(ADiscoveryUrl);
  LogDebug('Discovery index fetched successfully');
end;

function FetchServiceDiscovery(const ADiscoveryRestUrl: string): TJSONObject;
begin
  LogDebug('Fetching service discovery from: %s', [ADiscoveryRestUrl]);
  Result := HttpGetJsonObject(ADiscoveryRestUrl);
end;

{ URL utilities }

function IsValidUrl(const AUrl: string): Boolean;
var
  URI: TURI;
begin
  Result := False;
  if AUrl = '' then
    Exit;

  try
    URI := ParseURI(AUrl);
    Result := (URI.Protocol <> '') and (URI.Host <> '');
  except
    Result := False;
  end;
end;

function NormalizeUrl(const AUrl: string): string;
begin
  Result := Trim(AUrl);
  // Ensure URL doesn't end with slash for consistency
  while (Length(Result) > 0) and (Result[Length(Result)] = '/') do
    Delete(Result, Length(Result), 1);
end;

function UrlEncode(const AValue: string): string;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(AValue) do
  begin
    C := AValue[I];
    if C in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.', '~'] then
      Result := Result + C
    else
      Result := Result + '%' + IntToHex(Ord(C), 2);
  end;
end;

function BuildQueryString(const AParams: array of string): string;
var
  I: Integer;
begin
  Result := '';
  I := 0;
  while I < Length(AParams) - 1 do
  begin
    if Result <> '' then
      Result := Result + '&';
    Result := Result + UrlEncode(AParams[I]) + '=' + UrlEncode(AParams[I + 1]);
    Inc(I, 2);
  end;
end;

{ Configuration }

procedure SetHttpUserAgent(const AUserAgent: string);
begin
  GUserAgent := AUserAgent;
end;

function GetHttpUserAgent: string;
begin
  Result := GUserAgent;
end;

procedure SetHttpTimeout(ATimeout: Integer);
begin
  if ATimeout > 0 then
    GDefaultTimeout := ATimeout;
end;

function GetHttpTimeout: Integer;
begin
  Result := GDefaultTimeout;
end;

end.
