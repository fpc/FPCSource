{ **********************************************************************
  This file is part of the Free Component Library (FCL)
  Copyright (c) 2015 by the Free Pascal development team
        
  FPWebclient - abstraction for client execution of HTTP requests.
            
  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.
                   
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpwebclient;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

Type

  { TRequestResponse }
  
  // Some IIS servers react badly to svAny. So we set up a system where you can set a min/max SSL version.
  
  TSSLVersion = (svNone,svAny,svSSLv2,svSSLv3,svTLSv1,svTLSv11,svTLSv12,svTLSv13);
  TSSLVersions = Set of TSSLVersion;
  TSSLVersionArray = Array of TSSLVersion;

  TRequestResponse = Class(TObject)
  private
    FHeaders : TStrings;
    FStream : TStream;
    FOwnsStream : Boolean;
    FSSLVersion : TSSLVersion;
  Protected
    function GetHeaders: TStrings;virtual;
    function GetStream: TStream;virtual;
  Public
    Destructor Destroy; override;
    Procedure SetContentFromString(Const S : String) ;
    Function GetContentAsString : String;
    // Request headers or response headers
    Property Headers : TStrings Read GetHeaders;
    // Request content or response content
    Property Content: TStream Read GetStream;
    // SSLVersion : Which version to use
    Property SSLVersion : TSSLVersion Read FSSLVersion Write FSSLVersion;
  end;

  { TWebClientRequest }

  TWebClientRequest = Class(TRequestResponse)
  Private
    FExtraParams : TStrings;
    FResponseStream: TStream;
  Protected
    function GetExtraParams: TStrings; virtual;
  Public
    Destructor Destroy; override;
    Function ParamsAsQuery : String;
    // Query Parameters to include in request
    Property Params : TStrings Read GetExtraParams;
    // If you want the response to go to this stream, set this in the request
    Property ResponseContent : TStream Read FResponseStream Write FResponseStream;
  end;


  { TResponse }

  { TWebClientResponse }

  TWebClientResponse = Class(TRequestResponse)
  Protected
    Function GetStatusCode : Integer; virtual;
    Function GetStatusText : String; virtual;
  Public
    Constructor Create(ARequest : TWebClientRequest); virtual;
    // Status code of request
    Property StatusCode : Integer Read GetStatusCode;
    // Status text of request
    Property StatusText : String Read GetStatusText;
  end;

  { TAbstractRequestSigner }

  TAbstractRequestSigner = Class(TComponent)
  Protected
    Procedure DoSignRequest(ARequest : TWebClientRequest); virtual; abstract;
  Public
    Procedure SignRequest(ARequest : TWebClientRequest);
  end;

  { TAbstractResponseExaminer }

  TAbstractResponseExaminer = Class(TComponent)
  Protected
    Procedure DoExamineResponse(AResponse : TWebClientResponse); virtual; abstract;
  Public
    Procedure ExamineResponse(AResponse : TWebClientResponse);
  end;

  { TAbstractWebClient }


  TAbstractWebClient = Class(TComponent)
  private
    FExaminer: TAbstractResponseExaminer;
    FSigner: TAbstractRequestSigner;
    FLogFile : String;
    FLogStream : TStream;
    FMinSSLVersion: TSSLVersion;
    FMaxSSLVersion: TSSLVersion;
    Procedure LogRequest(AMethod, AURL: String; ARequest: TWebClientRequest);
    Procedure LogResponse(AResponse: TWebClientResponse);
    procedure SetLogFile(const AValue: String);
    procedure SetSSLVersion(AValue : TSSLVersion);
    Function GetSSLVersion : TSSLVersion;
  protected
    // Determine min/max version to try
    procedure GetVersionLimits(out PMin, PMax: TSSLVersion);
    // Write a string to the log file
    procedure StringToStream(const str: string);
    // Must execute the requested method using request/response. Must take ResponseContent stream into account
    Function DoHTTPMethod(Const AMethod,AURL : String; ARequest : TWebClientRequest) : TWebClientResponse; virtual; abstract;
    // Must create a request.
    Function DoCreateRequest : TWebClientRequest; virtual; abstract;
  Public
    Destructor Destroy; override;

    // Executes the HTTP method AMethod on AURL. Raises an exception on error.
    // On success, TWebClientResponse is returned. It must be freed by the caller.
    Function ExecuteRequest(Const AMethod,AURL : String; ARequest : TWebClientRequest) : TWebClientResponse;
    // Same as HTTPMethod, but signs the request first using signer.
    Function ExecuteSignedRequest(Const AMethod,AURL : String; ARequest : TWebClientRequest) : TWebClientResponse;
    // Create a new request. The caller is responsible for freeing the request.
    Function CreateRequest : TWebClientRequest;
    // These can be set to sign/examine the request/response.
    Property RequestSigner : TAbstractRequestSigner Read FSigner Write FSigner;
    Property ResponseExaminer : TAbstractResponseExaminer Read FExaminer Write FExaminer;
    Property LogFile : String Read FLogFile Write SetLogFile;
    // This will set MinSSLversion and MaxSSLversion
    property SSLVersion : TSSLVersion Read GetSSLVersion Write SetSSLVersion;
    // Minimum Version to try. If spNone is set, all should be tried in succession from high to MinSSLVersion.
    Property MinSSLVersion : TSSLVersion Read FMinSSLVersion Write FMinSSLVersion default svAny;
    // Maximum Version to try. If spNone is set, all should be tried in succession from MaxSSLVersion to low.
    Property MaxSSLVersion : TSSLVersion Read FMaxSSLVersion Write FMaxSSLVersion default svAny;
  end;
  TAbstractWebClientClass = Class of TAbstractWebClient;

  EFPWebClient = Class(Exception);

Var
  DefaultWebClientClass : TAbstractWebClientClass = Nil;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses FpWeb.Http.Defs;
{$ELSE FPC_DOTTEDUNITS}
uses httpdefs;
{$ENDIF FPC_DOTTEDUNITS}

{ TAbstractRequestSigner }

Procedure TAbstractRequestSigner.SignRequest(ARequest: TWebClientRequest);
begin
  DoSignRequest(ARequest);
end;

{ TAbstractResponseExaminer }

Procedure TAbstractResponseExaminer.ExamineResponse(
  AResponse: TWebClientResponse);
begin
  DoExamineResponse(AResponse);
end;

{ TWebClientRequest }

function TWebClientRequest.GetExtraParams: TStrings;
begin
  if FExtraParams=Nil then
    FExtraParams:=TStringList.Create;
  Result:=FExtraParams;
end;


destructor TWebClientRequest.Destroy;
begin
  FreeAndNil(FExtraParams);
  inherited Destroy;
end;

function TWebClientRequest.ParamsAsQuery: String;

Var
  N,V : String;
  I : integer;

begin
  Result:='';
  if Assigned(FextraParams) then
    For I:=0 to FextraParams.Count-1 do
      begin
      If Result<>'' then
        Result:=Result+'&';
      FextraParams.GetNameValue(I,N,V);
      Result:=Result+N+'='+HttpEncode(V);
      end;
end;

{ TWebClientResponse }

function TWebClientResponse.GetStatusCode: Integer;
begin
  Result:=0;
end;

function TWebClientResponse.GetStatusText: String;
begin
  Result:='';
end;

constructor TWebClientResponse.Create(ARequest: TWebClientRequest);
begin
  FStream:=ARequest.ResponseContent;
end;

{ TAbstractWebClient }

procedure TAbstractWebClient.SetSSLVersion(AValue : TSSLVersion);

begin
  MinSSLVersion:=AValue;
  MaxSSLVersion:=AValue;
end;

Function TAbstractWebClient.GetSSLVersion : TSSLVersion;

begin
  Result:=MinSSLVersion;
end;

procedure TAbstractWebClient.SetLogFile(const AValue: String);
begin
  if FLogFile=AValue then Exit;
  if Assigned(FlogStream) then
    FreeAndNil(FlogStream);
  FLogFile:=AValue;
  if (FLogFile<>'') then
    if FileExists(FLogFile) then
      begin
      FLogStream:=TFileStream.Create(FLogFile,fmOpenWrite or fmShareDenyWrite);
      FLogStream.Seek(0,soFromEnd);
      end
    else
      FLogStream:=TFileStream.Create(FLogFile,fmCreate or fmShareDenyWrite);
end;


procedure TAbstractWebClient.StringToStream(const str: string);

var
  S : String;

begin
  if Assigned(FLogStream) then
    begin
    S:=Str+sLineBreak;
    FlogStream.Write(S[1],length(S));
    end;
end;

destructor TAbstractWebClient.Destroy;
begin
  LogFile:='';
  inherited Destroy;
end;

procedure TAbstractWebClient.LogRequest(AMethod, AURL: String;
  ARequest: TWebClientRequest);


Var
  I : Integer;

begin
  StringToStream(StringOfChar('-',80));
  StringToStream('Request : '+AMethod+' '+AURL);
  StringToStream('Headers:');
  For I:=0 to ARequest.Headers.Count-1 do
   StringToStream(ARequest.Headers[I]);
  StringToStream('Body:');
  FLogStream.CopyFrom(ARequest.Content,0);
  ARequest.Content.Position:=0;
  StringToStream('');
end;

procedure TAbstractWebClient.LogResponse(AResponse: TWebClientResponse);

Var
  I : Integer;

begin
  StringToStream(StringOfChar('-',80));
  StringToStream('Response : '+IntToStr(AResponse.StatusCode)+' : '+AResponse.StatusText);
  StringToStream('Headers:');
  For I:=0 to AResponse.Headers.Count-1 do
    StringToStream(AResponse.Headers[I]);
  StringToStream('Body:');
  FLogStream.CopyFrom(AResponse.Content,0);
  AResponse.Content.Position:=0;
  StringToStream('');
end;

procedure TAbstractWebClient.GetVersionLimits(out PMin, PMax: TSSLVersion);

begin
  if MinSSLVersion=svNone then
    PMin:=Succ(Low(TSSLVersion))
  else
    PMin:=MinSSLVersion;
  if MaxSSLVersion=svNone then
    PMax:=High(TSSLVersion)
  else
    PMax:=MaxSSLVersion;
  if PMax<PMin then
    PMax:=PMin;
end;

function TAbstractWebClient.ExecuteRequest(const AMethod, AURL: String;
  ARequest: TWebClientRequest): TWebClientResponse;
  
Var
  P,PMax,PMin : TSSLVersion;
  S: String;

begin
  if Assigned(FLogStream) then
    LogRequest(AMethod,AURL,ARequest);
  Result:=DoHTTPMethod(AMethod,AURL,ARequest);
  GetVersionLimits(PMin,PMax);
  if PMin<>PMax then
    StringToStream('Trying multiple protocols.');
  P:=PMax;
  While (Not Assigned(Result)) and (P>=PMin) do
    begin
    Str(P,S);
    StringToStream('Trying protocol: '+S);
    Result:=Nil;
    ARequest.SSLVersion:=P;
    if Assigned(FLogStream) then
      LogRequest(AMethod,AURL,ARequest);
    try
      Result:=DoHTTPMethod(AMethod,AURL,ARequest);
      if Assigned(Result) and Assigned(Result.FStream) then
        Result.FStream.Position:=0;
    except
      if (P=PMin) then
        Raise;
    end;
    P:=Pred(P);
    end;
  if Assigned(Result) then
    begin
    if Assigned(FLogStream) then
      LogResponse(Result);
    If Assigned(FExaminer) then
      FExaminer.ExamineResponse(Result);
    end
  else
    StringToStream('Request generated no response');
end;

function TAbstractWebClient.ExecuteSignedRequest(const AMethod, AURL: String;
  ARequest: TWebClientRequest): TWebClientResponse;
begin
  If Assigned(FSigner) and Assigned(ARequest) then
    FSigner.SignRequest(ARequest);
  Result:=ExecuteRequest(AMethod,AURl,ARequest);
end;

function TAbstractWebClient.CreateRequest: TWebClientRequest;
begin
  Result:=DoCreateRequest;
end;

{ TRequestResponse }

function TRequestResponse.GetHeaders: TStrings;
begin
  if FHeaders=Nil then
    begin
    FHeaders:=TStringList.Create;
    FHeaders.NameValueSeparator:=':';
    end;
  Result:=FHeaders;
end;

function TRequestResponse.GetStream: TStream;
begin
  if (FStream=Nil) then
    begin
    FStream:=TMemoryStream.Create;
    FOwnsStream:=True;
    end;
  Result:=FStream;
end;

Destructor TRequestResponse.Destroy;
begin
  FreeAndNil(FHeaders);
  If FOwnsStream then
    FreeAndNil(FStream);
  inherited Destroy;
end;

Procedure TRequestResponse.SetContentFromString(Const S: String);
begin
  if (S<>'') then
    Content.WriteBuffer(S[1],SizeOf(Char)*Length(S));
end;

Function TRequestResponse.GetContentAsString: String;
begin
  SetLength(Result,Content.Size);
  if (Length(Result)>0) then
    begin
    Content.Position:=0;
    Content.ReadBuffer(Result[1],Length(Result));
    end;
end;

end.

