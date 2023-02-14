{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    Classes to implement a file serving mechanism.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
{$modeswitch advancedrecords}

unit fpwebfile;

interface

uses SysUtils, Classes, httpdefs, fphttp, httproute;

Type
  EFileLocation = class(EHTTP);

  TSimpleFileLog = Procedure (EventType : TEventType; Const Msg : String) of object;

  { TFPCustomFileModule }

  TFPCustomFileModule = Class(TCustomHTTPModule)
  private
    FCacheControlMaxAge: Integer;
    FScriptName:string;
  Protected
    // Determine filename frome request.
    Function GetRequestFileName(Const ARequest : TRequest) : String; virtual;
    // Map request filename to physical filename.
    Function MapFileName(Const AFileName : String) : String; virtual;
    // Override to implement security. Returns true by default.
    Function AllowFile(Const AFileName : String) : Boolean; virtual;
    // Actually Send file to client.
    Procedure SendFile(Const AFileName : String; AResponse : TResponse); virtual;
  Public
    Constructor CreateNew(AOwner: TComponent; CreateMode: Integer); override; overload;
    // Overrides TCustomHTTPModule to implement file serving.
    Procedure HandleRequest(ARequest : TRequest; AResponse : TResponse); override;
    Property CacheControlMaxAge : Integer Read FCacheControlMaxAge Write FCacheControlMaxAge;
  Public Class Var
    // If you want some logging, set this.
    OnLog : TSimpleFileLog;
  Published
    Property CORS;
    property Kind;
    Property BaseURL;
    Property AfterInitModule;
  end;
  TFPCustomFileModuleClass = Class of TFPCustomFileModule;

  TFPWebFileLocationAPIModule = Class;
  TFPWebFileLocationAPIModuleClass = Class of TFPWebFileLocationAPIModule;

  { TFPWebFileLocationAPIModule }

  TFPWebFileLocationAPIModule = class(TCustomHTTPModule)
  Private
    Class Var APIPassword : String;
  Private
    FCors: TCORSSupport;
    procedure SetCors(AValue: TCORSSupport);
  Protected
    procedure CreateLocation(ARequest: TRequest; AResponse: TResponse); virtual;
    procedure DeleteLocation(ARequest: TRequest; AResponse: TResponse); virtual;
    procedure GetLocations(ARequest: TRequest; AResponse: TResponse); virtual;
    procedure UpdateLocation(ARequest: TRequest; AResponse: TResponse); virtual;
    Function IsRequestAuthenticated(aRequest : TRequest): Boolean; virtual;
    class procedure HandleFileLocationAPIModuleRequest(ARequest: TRequest; AResponse: TResponse); static;
  Public
    Class var LocationAPIModuleClass : TFPWebFileLocationAPIModuleClass;
  Public
    Constructor CreateNew(aOwner : TComponent; CreateMode: Integer); override;
    Destructor Destroy; override;
    Procedure HandleRequest(ARequest : TRequest; AResponse : TResponse); override;
    Class procedure RegisterFileLocationAPI(Const aPath,aPassword : String);
    Property CORS : TCORSSupport Read FCors Write SetCors;
  end;

  TSimpleFileModule = Class;
  TSimpleFileModuleClass = class of TSimpleFileModule;

  { TSimpleFileModule }

  TSimpleFileModule = class(TFPCustomFileModule,IRouteInterface)
  Private
    class var
      FPrivateRoute : THTTPRoute;
  private
    FRequestedFileName,
    FMappedFileName : String;
    class procedure HandleSimpleFileRequest(ARequest: TRequest; AResponse: TResponse); static;
  Protected
    Function AllowFile(Const AFileName : String) : Boolean; override;
    Function MapFileName(Const AFileName : String) : String; override;
    Function GetRequestFileName(Const ARequest : TRequest) : String; override;
  Public
  Class var
    // Where to serve files from
    BaseDir : String;
    // For directories, convert to index.html if this is set.
    IndexPageName : String;
    DefaultSimpleFileModuleClass: TSimpleFileModuleClass;
    Class Procedure RegisterDefaultRoute(OverAllDefault : Boolean = True);
    Class function DefaultRouteActive : Boolean;
  end;

Var
  // Set this if you want a descendent class to serve the files.
  // You can use this to customize the behaviour in the descendent, for instance if you have multiple virtual hosts.
  DefaultFileModuleClass : TFPCustomFileModuleClass = TFPCustomFileModule;
  // Setting this will load mime types from that file.
  MimeTypesFile : string;
  DefaultCacheControlMaxAge : Integer = 0;

// use this to map locations (relative to BaseURL of the application) to physical directories.
// More than one location can be registered. Directory must exist, location must not have / or \
Procedure RegisterFileLocation(Const ALocation,ADirectory : String);
Procedure UnRegisterFileLocation(Const ALocation: String);

implementation

uses strutils, fpmimetypes, httpprotocol, fpjson, httpjson;

Resourcestring
  SErrNoLocation = 'Cannot register an empty location.';
  SErrInvalidLocation = 'Location %s contains invalid characters.';
  SErrInvalidDirectory = 'Directory "%s" does not exist';
  SErrNeedAPath = 'Need a path for the API';
  SErrRequiredField = 'Field "%s" is required';



Var
  Locations : TStrings;
  MimeLoaded : Boolean;
  
Procedure CheckMimeLoaded;
begin
  If (Not MimeLoaded) and (MimeTypesFile<>'') then
    begin
    MimeTypes.LoadFromFile(MimeTypesFile);
    MimeLoaded:=true;
    end;
end;

Procedure SetFileLocationPath(Const ALocation,ADirectory : String);

begin
  Locations.Values[IncludeHTTPPathDelimiter(ALocation)]:=aDirectory
end;

Procedure RegisterFileLocation(Const ALocation,ADirectory : String);

Var
  D,BaseDir : String;

begin
  if (ALocation='') then
    Raise HTTPError.Create(SErrNoLocation); 
  if Pos('/',ALocation)<>0 then
    Raise HTTPError.CreateFmt(SErrInvalidLocation,[aLocation]);
  if (Locations=Nil) then
    Locations:=TStringList.Create;
  if DefaultFileModuleClass=Nil then
    DefaultFileModuleClass:=TFPCustomFileModule;
  BaseDir:=ExtractFilePath(ParamStr(0));
  if (ADirectory='') then
    setFileLocationPath(aLocation,BaseDir)
  else
    begin
    D:=ADirectory;
    if (D<>ExpandFileName(D)) then
      D:=BaseDir+D;
    if not DirectoryExists(D) then
      Raise HTTPError.CreateFmt(SErrInvalidDirectory,[D]);
    SetFileLocationPath(ALocation,IncludeTrailingPathDelimiter(D));
    end;
  RegisterHTTPModule(ALocation,DefaultFileModuleClass,true);
  ModuleFactory.MoveModuleBeforeDefault(ALocation);
end;

Function IndexOfFileLocation(const ALocation: String) : Integer;

begin
  if Not Assigned(Locations) then
    Result:=-1
  else
    Result:=Locations.IndexOfName(IncludeHTTPPathDelimiter(ALocation))
end;

procedure UnRegisterFileLocation(const ALocation: String);

Var
  Idx : Integer;

begin
  if Not Assigned(Locations) then
    Exit;
  Idx:=IndexOfFileLocation(aLocation);
  if Idx<>-1 then
    begin
    Locations.Delete(Idx);
    ModuleFactory.RemoveModule(aLocation);
    end;
end;

{ TSimpleFileModule }

Class Procedure TSimpleFileModule.HandleSimpleFileRequest(ARequest : TRequest; AResponse : TResponse); static;

Var
  aClass : TSimpleFileModuleClass;

begin
  aClass:=DefaultSimpleFileModuleClass;
  if aClass=Nil then
    aClass:=TSimpleFileModule;
  With aClass.CreateNew(Nil) do
    try
      HandleRequest(ARequest,AResponse);
    finally
      Free;
    end;
end;


function TSimpleFileModule.AllowFile(const AFileName: String): Boolean;

Var
  FN : String;

begin
  FN:=ExpandFileName(aFileName);
  FN:=ExtractRelativepath(IncludeTrailingPathDelimiter(BaseDir),FN);
  Result:=Pos('..'+PathDelim,FN)=0;
end;

function TSimpleFileModule.MapFileName(const AFileName: String): String;

begin
  Result:=AFileName;
  While (Result<>'') and (Result[1]='/') do
    Delete(Result,1,1);
  Result:=ExpandFileName(IncludeTrailingPathDelimiter(BaseDir)+Result);
  FRequestedFileName:=AFileName;
  FMappedFileName:=Result;
end;

function TSimpleFileModule.GetRequestFileName(const ARequest: TRequest): String;
begin
  Result:=inherited GetRequestFileName(ARequest);
  if (IndexPageName<>'') and ((Result='') or (Result[Length(Result)]='/')) then
    Result:=Result+IndexPageName;
end;

class procedure TSimpleFileModule.RegisterDefaultRoute(OverAllDefault : Boolean = True);
begin
  if BaseDir='' then
    BaseDir:=IncludeTrailingPathDelimiter(GetCurrentDir);
  FPrivateRoute:=httprouter.RegisterRoute('/*',@HandleSimpleFileRequest);
  if OverallDefault then
    FPrivateRoute.Default:=True;
end;

class function TSimpleFileModule.DefaultRouteActive: Boolean;
begin
  Result:=FPrivateRoute<>Nil;
end;

Function TFPCustomFileModule.GetRequestFileName(Const ARequest : TRequest) : String;

  procedure sb;

  begin
    If (Result<>'') and (Result[1]='/') then
      Delete(Result,1,1);
  end;
begin
  Result:=ARequest.PathInfo;
  If (Result='') then
    Result:=ARequest.URI;
  sb;
  If (BaseURL<>'') and (Pos(BaseURL,Result)=1) then
    Delete(Result,1,Length(BaseURL));
  sb;
end;

Function TFPCustomFileModule.MapFileName(Const AFileName : String) : String; 

Var
  D,localBaseURL : String;

begin
  if (BaseURL='') then
    Result:=AFileName
  else
    begin
    if FScriptName<>'' then
      localBaseURL:=Copy(BaseURL,Length(FScriptName)+2,MaxInt)
    else
      localBaseURL:=BaseURL;
    D:=Locations.Values[localBaseURL];
    If (D='') then
      Result:=''
    else
      begin
      if FScriptName<>'' then
        Result:=D+Copy(AFileName,Length(localBaseURL)+1,MaxInt)
      else
        Result:=D+AFileName;
      DoDirSeparators(Result);
      Result:=ExpandFileName(Result);
      end;
    end;
end;

Function TFPCustomFileModule.AllowFile(Const AFileName : String) : Boolean;

Var
  BaseDir,FN : String;

begin
  FN:=ExpandFileName(aFileName);
  if (BaseURL='') then
    BaseDir:=ExtractFilePath(Paramstr(0))
  else
    begin
    BaseDir:=Locations.Values[BaseURL];
    if (BaseDir='') then
      BaseDir:=ExtractFilePath(Paramstr(0))
    end;
  FN:=ExtractRelativepath(BaseDir,FN);
  Result:=Pos('..'+PathDelim,FN)=0;
end;

procedure TFPCustomFileModule.SendFile(Const AFileName : String; AResponse : TResponse);

Var
  F : TFileStream;

begin
  CheckMimeLoaded;
  AResponse.ContentType:=MimeTypes.GetMimeType(ExtractFileExt(AFileName));
  If (AResponse.ContentType='') then
    AResponse.ContentType:='Application/octet-stream';
  if CacheControlMaxAge>0 then
    aResponse.CacheControl:=Format('max-age=%d',[CacheControlMaxAge]);
  F:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
  try
    AResponse.ContentStream:=F; // Sets size
    AResponse.SendContent;
    AResponse.ContentStream:=Nil;
    // We set it again so an after request interceptor can see it
    AResponse.ContentLength:=F.Size;
  finally
    F.Free;
  end;
end;

constructor TFPCustomFileModule.CreateNew(AOwner: TComponent; CreateMode: Integer);
begin
  inherited CreateNew(aOwner,CreateMode);
  CacheControlMaxAge:=DefaultCacheControlMaxAge;
end;


Procedure TFPCustomFileModule.HandleRequest(ARequest : TRequest; AResponse : TResponse);

Var
  RFN,FN : String;

begin
  FScriptName:=ARequest.ScriptName;
  If CompareText(ARequest.Method,'GET')<>0 then
    begin
    AResponse.SetStatus(405,True);
    Exit;
    end;
  RFN:=GetRequestFileName(ARequest);
  if (RFN='') then
    begin
    AResponse.SetStatus(400,True);
    exit;
    end;
  FN:=MapFileName(RFN);
  if (FN='') or not AllowFile(FN) then  
    begin
    AResponse.SetStatus(403,True);
    exit;
    end;
  if  not FileExists(FN) then
    begin
    AResponse.SetStatus(404,True);
    exit;
    end;
  SendFile(FN,AResponse);
  if Assigned (OnLog) then
    OnLog(etInfo,Format('%d serving "%s" -> "%s"',[AResponse.Code,RFN,FN]));
end;

procedure TFPWebFileLocationAPIModule.SetCors(AValue: TCORSSupport);
begin
  if FCors=AValue then Exit;
  FCors.Assign(AValue);
end;

class procedure TFPWebFileLocationAPIModule.HandleFileLocationAPIModuleRequest(ARequest: TRequest; AResponse: TResponse);

Var
  aClass : TFPWebFileLocationAPIModuleClass;

begin
  aClass:=LocationAPIModuleClass;
  if aClass=Nil then
    aClass:=TFPWebFileLocationAPIModule;
  With aClass.CreateNew(Nil) do
    try
      HandleRequest(aRequest,aResponse);
    finally
      Free
    end;
end;

constructor TFPWebFileLocationAPIModule.CreateNew(aOwner: TComponent; CreateMode: Integer);
begin
  inherited CreateNew(aOwner,CreateMode);
  FCors:=TCORSSupport.Create;
  FCors.Enabled:=True;
  FCors.Options:=[coAllowCredentials];
end;

destructor TFPWebFileLocationAPIModule.Destroy;
begin
  FreeAndNil(FCors);
  inherited Destroy;
end;

Type

  { TLocationData }

  TLocationData = Record
    Location : String;
    Path : String;
    Procedure Verify;
    Procedure FromJSON(aJSON : TJSONObject);
    Procedure ToJSON(aJSON : TJSONObject);
  end;

{ TLocationData }

procedure TLocationData.Verify;

  Procedure DoError(aFmt, aField : String);

  Var
    E : EFileLocation;

  begin
    E:=EFileLocation.CreateFmt(aFmt,[aField]);
    E.StatusText:='BAD REQUEST';
    E.StatusCode:=400;
    Raise E;
  end;

begin
  if Location='' then
    DoError(SErrRequiredField,'location');
  if Path='' then
    DoError(SErrRequiredField,'path');
  if Pos('/',Location)<>0 then
    DoError(SErrInvalidLocation,Location);
  if not DirectoryExists(Path) then
    DoError(SErrInvalidDirectory,Location);
end;

procedure TLocationData.FromJSON(aJSON: TJSONObject);

begin
  Location:=aJSON.Get('location','');
  Path:=aJSON.Get('path','');
end;

procedure TLocationData.ToJSON(aJSON: TJSONObject);
begin
  aJSON.Add('location',Location);
  aJSON.Add('path',path);
end;

Procedure TFPWebFileLocationAPIModule.CreateLocation(ARequest: TRequest; AResponse: TResponse);

Var
  aJSON : TJSONObject;
  aLoc : TLocationData;

begin
  aJSON:=aRequest.ContentAsJSONObject;
  try
    ALoc.FromJSON(aJSON);
    ALoc.Verify;
    RegisterFileLocation(aLoc.Location,Aloc.Path);
    aJSON.Clear;
    aLoc.ToJSON(aJSON);
    aResponse.ContentAsJSON:=aJSON;
    aResponse.SetStatus(201,True);
  finally
    aJSON.Free;
  end;
end;

Procedure TFPWebFileLocationAPIModule.UpdateLocation(ARequest: TRequest; AResponse: TResponse);

Var
  aJSON : TJSONObject;
  aLoc : TLocationData;
  aOldLoc : String;
  Idx: Integer;

begin
  aJSON:=aRequest.ContentAsJSONObject;
  try
    ALoc.FromJSON(aJSON);
    aOldLoc:=aRequest.RouteParams['location'];
    if aOldLoc='' then // Location name in payload
      aOldLoc:=aLoc.Location;
    if aLoc.Location='' then // Only path in payload
      aLoc.Location:=aOldLoc;
    ALoc.Verify;
    Idx:=IndexOfFileLocation(aOldLoc);
    if Idx=-1 then
      aResponse.SetStatus(404,True)
    else  if not SameText(aOldLoc,aLoc.Location) then
      begin
      UnRegisterFileLocation(aOldLoc);
      RegisterFileLocation(aLoc.Location,Aloc.Path);
      end
    else
      SetFileLocationPath(aLoc.Location,aLoc.Path);
    aJSON.Clear;
    aLoc.ToJSON(aJSON);
    aResponse.ContentAsJSON:=aJSON;
    aResponse.SendContent;
  finally
    aJSON.Free;
  end;
end;

function TFPWebFileLocationAPIModule.IsRequestAuthenticated(aRequest: TRequest): Boolean;

Var
  aAuth : String;

begin
  Result:=(APIPassword='');
  if Result then exit;
  aAuth:=aRequest.Authorization;
  if (aAuth<>'') and SameText(ExtractWord(1,aAuth,[' ']),'Bearer') then
    aAuth:=ExtractWord(2,aAuth,[' '])
  else
    aAuth:=aRequest.QueryFields.Values['APIKey'];
  Result:=(aAuth=APIPassword);
end;

Procedure TFPWebFileLocationAPIModule.DeleteLocation(ARequest: TRequest; AResponse: TResponse);

Var
  aOldLoc : String;
  Idx: Integer;

begin
  aOldLoc:=aRequest.RouteParams['location'];
  if aOldLoc='' then
    aResponse.SetStatus(400,True)
  else
    begin
    Idx:=IndexOfFileLocation(aOldLoc);
    if Idx=-1 then
      aResponse.SetStatus(404,True)
    else
      begin
      UnRegisterFileLocation(aOldLoc);
      aResponse.SetStatus(204,True);
      end;
    end;
end;

Procedure TFPWebFileLocationAPIModule.GetLocations(ARequest: TRequest; AResponse: TResponse);

Var
  Res,Loc : TJSONObject;
  Arr: TJSONArray;
  I : Integer;
  aLoc : TLocationData;

begin
  Res:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Res.Add('data',Arr);
    if Assigned(Locations) then
      For I:=0 to Locations.Count-1 do
        begin
        Loc:=TJSONObject.Create;
        Arr.Add(Loc);
        with aLoc do
          begin
          Locations.GetNameValue(I,Location,Path);
          aLoc.Location:=ExcludeHTTPPathDelimiter(aLoc.Location);
          aLoc.ToJSON(Loc);
          end;
        end;
    if TSimpleFileModule.DefaultRouteActive then
      begin
      Loc:=TJSONObject.Create;
      Arr.Add(Loc);
      aLoc.Location:='*';
      aLoc.Path:=TSimpleFileModule.BaseDir;
      aLoc.ToJSON(Loc);
      end;
    aResponse.SetContentFromJSON(Res,aRequest.QueryFields.Values['fmt']='1');
    aResponse.SendContent;
  finally
    Res.Free;
  end;
end;

procedure TFPWebFileLocationAPIModule.HandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  if Not IsRequestAuthenticated(aRequest) then
    begin
    aResponse.SetStatus(401,True);
    exit;
    end
  else
    Cors.HandleRequest(aRequest,aResponse,[]);
  try
    Case UpperCase(ARequest.Method) of
      'GET'    : GetLocations(ARequest,aResponse);
      'POST'   : CreateLocation(ARequest,aResponse);
      'PUT'    : UpdateLocation(aRequest,aResponse);
      'DELETE' : DeleteLocation(aRequest,aResponse);
      'OPTIONS' :  Cors.HandleRequest(aRequest,aResponse,[hcSend]);
    else
      aResponse.SetStatus(405);
    end;
  except
    on E : Exception do
      aResponse.SendExceptionJSON(E);
  end;
end;

class procedure TFPWebFileLocationAPIModule.RegisterFileLocationAPI(const aPath, aPassword: String);

Var
  P : String;

begin
  APIPassword:=aPassword;
  if aPath='' then
    Raise EFileLocation.Create(SErrNeedAPath);
  P:=aPath;
  if P[1]<>'/' then
    P:='/'+P;
  httprouter.RegisterRoute(P,@HandleFileLocationAPIModuleRequest); // Get and post
  httprouter.RegisterRoute(IncludeHTTPPathDelimiter(P)+':Location',@HandleFileLocationAPIModuleRequest); // Put & Delete
end;


initialization

finalization
  FreeAndNil(Locations);
end.
