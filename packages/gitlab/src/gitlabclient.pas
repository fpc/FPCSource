{$IFNDEF FPC_DOTTEDUNITS}
unit gitlabclient;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.IniFiles, FpJson.Data, FpWeb.Client, FpWeb.Client.Http, FpWeb.Http.Protocol;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, inifiles, fpjson, fpwebclient, fphttpwebclient, httpprotocol;
{$ENDIF FPC_DOTTEDUNITS}

Const
  LongThrottleSleep = 60 * 1000; // One minute
  MaxSleepCount = 5; // max times to sleep when consecutive 429s
  SGitlabClient = 'Gitlab'; // Default section
  DefaultGitURL = 'https://gitlab.com/api/v4/';
  DefaultGitKey = ''; // API Key, default none
  DefaultProjectID = 0; // Gitlab Project ID, default none


Type
  EGitLab = Class(Exception);

  { TGitlabConfig }

  TGitlabConfig = Record
    BaseURL : String;
    APIkey : String;
    ProjectID : Int64;
    Procedure Reset;
    Procedure LoadFromFile(const aFileName,aSection : String);
    Procedure LoadFromIni(aIni : TCustomInifile; const aSection : String);
  end;

  { TGitLabClient }
  TLogEvent = Procedure (Sender : TObject; Const aMessage : string) of object;
  TResourceCallback = procedure (Sender : TObject; aPage,aIndex,aCount : Integer; aObject : TJSONObject; aContinue : Boolean)  of object;

  TGitLabClient = class(TObject)
  private
    FConfig: TGitlabConfig;
    FClient : TAbstractWebClient;
    FOnLog: TLogEvent;
    FSudo: String;
    procedure setconfig(AValue: TGitlabConfig);
  Protected
    procedure DoLog(const aMessage : string); overload;
    procedure DoLog(const aFmt : string; aArgs : Array of const);  overload;

    function CreateRequest(aResult: TStream): TWebClientRequest;
    function CreateURL(aName: string; aParams: array of string; useSUDO : Boolean = False): String;
    procedure DoResourceRequest(aVerb, aName: String; aSrc: TStream; ADest: TStream; aContentType: String);
    procedure CreateResource(aName : String; aSrc : TStream; ADest : TStream; aContentType : String = '');
    procedure UpdateResource(aName : String; aSrc : TStream; ADest : TStream; aContentType : String = '');
  public
    Constructor Create; overload;
    Constructor Create(const aConfig : TGitlabConfig); overload;
    Destructor destroy; override;

    function GetProjectResourceURL(aResource: string): String;
    function GetResourceURL(aResource: string): String;
    // Upload file. URL is relative to baseURL, gets upload appended.
    // Return markdown
    Function UploadFile(const aURL,aLocalFileName,aRemoteFileName : String) : TJSONStringType;
    // Return JSON string
    function UploadFileRaw(const aURL,aLocalFileName,aRemoteFileName: string): TJSONStringType;
    // Return JSON Object
    Function UploadFileObject(const aURL,aLocalFileName,aRemoteFileName : String) : TJSONObject;
    // Create
    Function CreateResourceRaw(aName : String; aObj : TJSONObject) : TJSONStringType;
    Function CreateResourceObject(aName : String; aObj : TJSONObject) : TJSONObject;
    Function CreateResource(aName : String; aObj : TJSONObject) : Int64;
    // update
    Function UpdateResourceRaw(aName : String; aObj : TJSONObject) : TJSONStringType;
    Function UpdateResource(aName : String; aObj : TJSONObject) : Int64;
    Function UpdateResourceObject(aName : String; aObj : TJSONObject) : TJSONObject;
    // Get
    Function GetSingleResource(aName : String; aParams : array of string) : TJSONObject;
    Function GetResourceList(aName : String; aParams : array of string) : TJSONArray;
    Procedure GetResource(aName : String; aParams : array of string; aResult : TStream);
    Function ForEachResource(aResource : String; aParams : array of string; CallBack : TResourceCallback) : Integer;
    // Delete
    Procedure DeleteResource(aName : String);
    // Properties
    Property Config : TGitlabConfig Read FConfig write setconfig;
    Property OnLog : TLogEvent Read FOnLog Write FOnLog;
    // use SUDO
    Property Sudo : String Read FSudo Write FSudo;
  end;


implementation


{ TGitlabConfig }

procedure TGitlabConfig.Reset;
begin
  BaseURL:=DefaultGitURL;
  APIKey:=DefaultGitKey;
  ProjectID:=DefaultProjectID;
end;

procedure TGitlabConfig.LoadFromFile(const aFileName, aSection: String);

Var
  aIni : TMemIniFile;

begin
  aIni:=TMemIniFile.Create(aFileName);
  try
    LoadFromIni(aIni,aSection);
  finally
    aIni.Free;
  end;
end;

procedure TGitlabConfig.LoadFromIni(aIni: TCustomInifile; const aSection: String);
Var
  S : String;
begin
  S:=aSection;
  if S='' then
    S:=SGitlabClient;
  BaseURL:=aIni.ReadString(S,'BaseURL',BaseURL);
  APIkey:=aIni.ReadString(S,'APIKey',APIKey);
  ProjectID:=aIni.ReadInt64(S,'ProjectID',ProjectID);
end;


constructor TGitLabClient.Create;
begin
  FClient:=TFPHTTPWebClient.Create(Nil);
end;

constructor TGitLabClient.Create(const aConfig: TGitlabConfig);
begin
  Create;
  Config:=aConfig;
end;

destructor TGitLabClient.destroy;
begin
  FreeAndNil(FClient);
  inherited destroy;
end;

function TGitLabClient.UploadFile(const aURL, aLocalFileName,
  aRemoteFileName: String): TJSONStringType;

Var
  Obj : TJSONObject;

begin
  Obj:=UploadFileObject(aURL,aLocalFileName,aRemoteFilename);
  try
    Result:=Obj.Get('markdown','');
  finally
    Obj.Free;
  end;
end;

function TGitLabClient.UploadFileRaw(const aURL, aLocalFileName,
  aRemoteFileName: string): TJSONStringType;

Const
  CRLF = #13#10;

Var
  S, Sep : string;
  SS,SR : TRawByteStringStream;
  AStream : TFileStream;

begin
  Sep:=Format('%.8x_multipart_boundary',[Random($ffffff)]);
  aStream:=Nil;
  SR:=Nil;
  SS:=TRawByteStringStream.Create('');
  try
    AStream:=TFileStream.Create(aLocalFileName,fmOpenRead);
    S:='--'+Sep+CRLF;
    s:=s+Format('Content-Disposition: form-data; name="%s"; filename="%s"'+CRLF,['file',aRemoteFileName]);
    s:=s+'Content-Type: application/octet-string'+CRLF+CRLF;
    SS.WriteBuffer(S[1],Length(S));
    AStream.Seek(0, soFromBeginning);
    SS.CopyFrom(AStream,AStream.Size);
    S:=CRLF+'--'+Sep+'--'+CRLF;
    SS.WriteBuffer(S[1],Length(S));
    SS.Position:=0;
    SR:=TRawByteStringStream.Create('');
    DoResourceRequest('POST',aURL,SS,SR,'multipart/form-data; boundary='+Sep);
    Result:=SR.DataString;
  finally
    SR.Free;
    SS.Free;
    aStream.Free;
  end;
end;

function TGitLabClient.UploadFileObject(const aURL, aLocalFileName,
  aRemoteFileName: String): TJSONObject;
var
  aJSON : TJSONStringType;
  D : TJSONData;
begin
  aJSON:=UploadFileRaw(aURL,aLocalFileName,aRemoteFileName);
  try
    D:=GetJSON(aJSON);
    Result:=D as TJSONObject;
  except
    on E : Exception do
      begin
      D.Free;
      E.Message:='Invalid JSON returned by upload of '+aLocalFileName+': '+E.Message;
      Raise;
      end;
  end;
end;

function TGitLabClient.CreateResourceRaw(aName: String; aObj: TJSONObject): TJSONStringType;

Var
  Src,Dest : TStringStream;

begin
  Dest:=nil;
  Src:=Nil;
  if Assigned(aObj) then
    Src:=TStringStream.Create(aObj.asJSON);
  try
    Dest:=TStringStream.Create('');
    CreateResource(aName,Src,Dest,'application/json');
    Result:=Dest.DataString;
  finally
    Src.Free;
    Dest.Free;
  end;
end;

function TGitLabClient.CreateResourceObject(aName: String; aObj: TJSONObject): TJSONObject;

Var
  S : TJSONStringType;
  D : TJSONData;

begin
  S:=CreateResourceRaw(aName,aObj);
  try
    D:=GetJSON(S);
    Result:=D as TJSONObject;
  except
    on E : Exception do
      begin
      D.Free;
      E.Message:='Invalid JSON returned by Create of '+aName+': '+E.Message;
      Raise;
      end;
  end;
end;

function TGitLabClient.CreateResource(aName: String; aObj: TJSONObject): Int64;

Var
  Obj : TJSONObject;

begin
  Obj:=CreateResourceObject(aName,aObj);
  try
    Result:=Obj.Get('id',Int64(-1));
  finally
    Obj.Free;
  end;
end;

function TGitLabClient.UpdateResourceRaw(aName: String; aObj: TJSONObject
  ): TJSONStringType;
Var
  Src,Dest : TStringStream;

begin
  Dest:=nil;
  Src:=TStringStream.Create(aObj.asJSON);
  try
    Dest:=TStringStream.Create('');
    UpdateResource(aName,Src,Dest,'application/json');
    Result:=Dest.DataString;
  finally
    Src.Free;
    Dest.Free;
  end;
end;

function TGitLabClient.UpdateResource(aName: String; aObj: TJSONObject): Int64;

Var
  Obj : TJSONObject;

begin
  Obj:=UpdateResourceObject(aName,aObj);
  try
    Result:=Obj.Get('id',Int64(-1));
  finally
    Obj.Free;
  end;
end;

function TGitLabClient.UpdateResourceObject(aName: String; aObj: TJSONObject
  ): TJSONObject;
Var
  S : TJSONStringType;
  D : TJSONData;

begin
  S:=UpdateResourceRaw(aName,aObj);
  try
    D:=GetJSON(S);
    Result:=D as TJSONObject;
  except
    on E : Exception do
      begin
      D.Free;
      E.Message:='Invalid JSON returned by Create of '+aName+': '+E.Message;
      Raise;
      end;
  end;
end;

function TGitLabClient.GetSingleResource(aName: String; aParams: array of string
  ): TJSONObject;

Var
  S : TStream;
  D : TJSONData;

begin
  D:=NIl;
  S:=TMemoryStream.Create;
  try
    GetResource(aName,aParams,S);
    try
      if S.Size>0 then
        D:=GetJSON(S);
      if (D<>Nil) and Not (D is TJSONObject) then
        Raise EGitlab.Create('Not a JSON object '+D.AsJSON);
      Result:=D as TJSONObject;
    except
      On E :Exception do
        E.Message:='Error getting resource'+aName+': '+E.Message;
    end;
  finally
    S.Free;
  end;
end;

function TGitLabClient.GetResourceList(aName: String; aParams: array of string
  ): TJSONArray;

Var
  S : TStream;
  D : TJSONData;

begin
  D:=NIl;
  S:=TMemoryStream.Create;
  try
    GetResource(aName,aParams,S);
    try
      D:=GetJSON(S);
      if Not (D is TJSONArray) then
        Raise EGitlab.Create('Not a JSON array '+D.AsJSON);
      Result:=D as TJSONArray;
    except
      On E :Exception do
        begin 
        E.Message:='Error getting resource'+aName+': '+E.Message;
        Raise;
        end;
    end;
  finally
    S.Free;
  end;
end;

procedure TGitLabClient.setconfig(AValue: TGitlabConfig);
begin
  FConfig:=AValue;
end;

procedure TGitLabClient.DoLog(const aMessage: string);
begin
  If Assigned(FOnLog) then
    FOnLog(Self,aMessage);
end;

procedure TGitLabClient.DoLog(const aFmt: string; aArgs: array of const);
begin
  DoLog(Format(aFmt,aArgs));
end;

function TGitLabClient.CreateRequest(aResult: TStream): TWebClientRequest;

begin
  Result:=FClient.CreateRequest;
  Result.Headers.Values['Authorization']:='Bearer '+FConfig.APIkey;
  Result.ResponseContent:=aResult;
end;

function TGitLabClient.CreateURL(aName: string; aParams: array of string; useSUDO : Boolean = False): String;

Var
  I : Integer;

begin
  Result:=IncludeHTTPPathDelimiter(FConfig.BaseURL);
  Result:=Result+aName;
  if (Length(aParams) mod 2<>0) then
    Raise EGitLab.Create('URL Parameters must come in key=value pairs');
  I:=0;
  While I<Length(aParams)-1 do
   begin
   if I=0 then
     Result:=Result+'?'
   else
     Result:=Result+'&';
   Result:=Result+HTTPEncode(aParams[i])+'='+HTTPEncode(aParams[i+1]);
   inc(I,2);
   end;
 if UseSUDO and (Sudo<>'') then
   begin
   if Length(aParams)=0 then
     Result:=Result+'?'
   else
     Result:=Result+'&';
   Result:=Result+'sudo='+HTTPEncode(SUDO);
   end;
end;

procedure TGitLabClient.CreateResource(aName: String; aSrc: TStream;
  ADest: TStream; aContentType: String);

begin
  DoResourceRequest('POST',aName,aSrc,aDest,aContentType);
end;

procedure TGitLabClient.DoResourceRequest(aVerb,aName: String; aSrc: TStream;
  ADest: TStream; aContentType: String);

 Function StreamToContent(S : TStream) : string;

 begin
   Result:='';
   if (S<>Nil) then
     With TStringStream.Create('') do
       try
         CopyFrom(S,0);
         Result:=DataString;
         S.Position:=0;
       finally
         Free;
       end;
  end;

Var
  aRequest : TWebClientRequest;
  aResponse : TWebClientResponse;
  aContent,aMsg,aURL : String;
  aSleepTime : Integer;
  aSleepCount : integer;
  aTryCount : Integer;
  UseSUDO, ExitLoop : Boolean;


begin
  aSleepCount:=1;
  aTryCount:=0;
  aResponse:=Nil;
  aRequest:=CreateRequest(aDest);
  try
    if (aSrc<>Nil) then
      begin
      if (aContentType='') then
        aContentType:='application/json';
      aRequest.Headers.Values['Content-Type']:=aContentType;
      aRequest.Content.CopyFrom(aSrc,0);
      end;
    repeat
      inc(aTryCount);
      ExitLoop:=True;
      UseSUDO:=False; // (Sudo<>'') and Not SameText(aVerb,'GET')
      aURL:=CreateURL(aName,[],UseSUDO);
      DoLog('URL : %s %s',[aVerb,aURL]);
      // Reset for loop
      FreeAndNil(aResponse);
      if Assigned(aSrc) then
        aRequest.Content.Position:=0;
      // Go !
      aResponse:=FClient.ExecuteRequest(aVerb,aURL,aRequest);
      // Throttle hit ?
      if aResponse.StatusCode=429 then
        begin
        aSleepTime:=LongThrottleSleep*aSleepCount;
        DoLog('API Throttle limit reached. Waiting %d seconds',[aSleepTime div 1000]);
        sleep(aSleepTime);
        Inc(aSleepCount);
        ExitLoop:=(aSleepCount>MaxSleepCount);
        end
      else if aResponse.StatusCode=409 then
        begin
        if aTryCount>1 then
          DoLog('Duplicate ID found at try %d, ignoring.',[aTryCount])
        else
          DoLog('Duplicate ID found at first try, ignoring anyway.');
        ExitLoop:=True;
        end
      else if aResponse.StatusCode=500 then
        begin
        aSleepTime:=LongThrottleSleep*aSleepCount;
        DoLog('Retry 500 error. Waiting %d seconds',[aSleepTime div 1000]);
        sleep(aSleepTime);
        Inc(aSleepCount);
        ExitLoop:=(aSleepCount>MaxSleepCount);
        end
      else if (UseSUDO and ((aResponse.StatusCode=403) or (aResponse.StatusCode=404))) then
        begin
        DoLog('SUDO request for %s failed, switching to non-sudo request',[Sudo]);
        ExitLoop:=False;
        Sudo:='';
        end;
     until ExitLoop;
    if (aResponse.StatusCode div 100)<>2 then
      begin
      aContent:=StreamToContent(aSrc);
      aMsg:=StreamToContent(aDest);
      Raise EGitLab.CreateFmt('Failed to %s URL "%s" : %d (%s):'+sLineBreak+'%s'+sLineBreak+'Request Content:'+sLineBreak+'%s',[aVerb,aURL,aResponse.StatusCode,aResponse.StatusText,aMsg,aContent]);
      end
      else
      begin
        if aSleepCount > 1 then
          DoLog('Success after %d retries', [aSleepCount-1]);
      end;
    if assigned(aDest) then
      aDest.Position:=0;
  finally
    aRequest.Free;
    aResponse.Free;
  end;
end;

procedure TGitLabClient.UpdateResource(aName: String; aSrc: TStream;
  ADest: TStream; aContentType: String);
begin
  DoResourceRequest('PUT',aName,aSrc,aDest,aContentType);
end;


procedure TGitLabClient.GetResource(aName: String; aParams: array of string;
  aResult: TStream);

Var
  aRequest : TWebClientRequest;
  aResponse : TWebClientResponse;
  aURL : String;
begin
  aURL:=CreateURL(aName,aParams);
  aResponse:=Nil;
  aRequest:=CreateRequest(aResult);
  try
    aResponse:=FClient.ExecuteRequest('GET',aURL,aRequest);
    if (aResponse.StatusCode div 100)<>2 then
      Raise EGitLab.CreateFmt('Failed to get URL "%s" : %d (%s)',[aURL,aResponse.StatusCode,aResponse.StatusText]);
    aResult.Position:=0;  
  finally
    aRequest.Free;
    aResponse.Free;
  end;
end;

function TGitLabClient.GetProjectResourceURL(aResource: string): String;

begin
  Result:=GetResourceURL(Format('projects/%d/%s/',[FConfig.ProjectID,aResource]))
end;

function TGitLabClient.GetResourceURL(aResource: string): String;
begin
  Result:= IncludeHTTPPathDelimiter(FConfig.BaseURL)+aResource;
end;

function TGitLabClient.ForEachResource(aResource: String; aParams: array of string;
  CallBack: TResourceCallback): Integer;

Var
  Resources : TJSONArray;
  aLen,aTotalCount,i,aCount,aPage : Integer;
  aID : Int64;
  baseURL : String;
  tParams : Array of string;
  aContinue : Boolean;

begin
  setLength(tParams,Length(aParams)+4);
  aLen:=Length(aParams);
  For I:=0 to Length(aParams)-1 do
    tParams[i]:=aParams[I];
  tParams[aLen]:='per_page';
  tParams[aLen+1]:='100';
  Result:=0;
  aPage:=1;
  Repeat
    tParams[aLen+2]:='page';
    tParams[aLen+3]:=IntToStr(aPage);
    Resources:=GetResourceList(aResource,tParams);
    try
      aCount:=Resources.Count;
      aContinue:=True;
      I:=0;
      While aContinue and (I<aCount) do
        begin
        CallBack(Self,aPage,I,aCount,Resources.Objects[i],aContinue);
        Inc(I);
        Inc(Result);
        end;
    finally
      Resources.Free;
    end;
    inc(aPage);
  until (aCount<100) or Not aContinue;
end;

procedure TGitLabClient.DeleteResource(aName: String);
begin
  DoResourceRequest('DELETE',aName,Nil,Nil,'');
end;



end.

