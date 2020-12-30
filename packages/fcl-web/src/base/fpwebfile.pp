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

unit fpwebfile;

interface

uses SysUtils, Classes, httpdefs, fphttp, httproute;

Type

  { TFPCustomFileModule }

  TFPCustomFileModule = Class(TCustomHTTPModule)
  private
    FCacheControlMaxAge: Integer;
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
  end;
  TFPCustomFileModuleClass = Class of TFPCustomFileModule;

  { TSimpleFileModule }

  TSimpleFileLog = Procedure (EventType : TEventType; Const Msg : String) of object;
  TSimpleFileModule = class(TFPCustomFileModule,IRouteInterface)
  private
    FRequestedFileName,
    FMappedFileName : String;
    class procedure HandleSimpleFileRequest(ARequest: TRequest; AResponse: TResponse); static;
  Protected
    Function AllowFile(Const AFileName : String) : Boolean; override;
    Function MapFileName(Const AFileName : String) : String; override;
    Function GetRequestFileName(Const ARequest : TRequest) : String; override;
  Public
    Procedure HandleRequest(ARequest : TRequest; AResponse : TResponse); override;
  Public
  Class var
    // Where to serve files from
    BaseDir : String;
    // For directories, convert to index.html if this is set.
    IndexPageName : String;
    // If you want some logging, set this.
    OnLog : TSimpleFileLog;
    Class Procedure RegisterDefaultRoute;
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

implementation

uses fpmimetypes;

Resourcestring
  SErrNoLocation = 'Cannot register an empty location.';
  SErrInvalidLocation = 'Location contains invalid characters.';
  SErrInvalidDirectory = 'Directory "%s" does not exist';

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
                  
Procedure RegisterFileLocation(Const ALocation,ADirectory : String);

Var
  D,BaseDir : String;

begin
  if (ALocation='') then
    Raise HTTPError.Create(SErrNoLocation); 
  if Pos('/',ALocation)<>0 then
    Raise HTTPError.Create(SErrInvalidLocation);
  if (Locations=Nil) then
    Locations:=TStringList.Create;
  if DefaultFileModuleClass=Nil then
    DefaultFileModuleClass:=TFPCustomFileModule;
  BaseDir:=ExtractFilePath(ParamStr(0));
  if (ADirectory='') then
    Locations.Values[IncludeHTTPPathDelimiter(ALocation)]:=BaseDir
  else
    begin
    D:=ADirectory;
    if (D<>ExpandFileName(D)) then
      D:=BaseDir+D;
    if not DirectoryExists(D) then
      Raise HTTPError.CreateFmt(SErrInvalidDirectory,[D]);
    Locations.Values[IncludeHTTPPathDelimiter(ALocation)]:=IncludeTrailingPathDelimiter(D);
    end;
  RegisterHTTPModule(ALocation,DefaultFileModuleClass,true);
end;

{ TSimpleFileModule }

Class Procedure TSimpleFileModule.HandleSimpleFileRequest(ARequest : TRequest; AResponse : TResponse); static;

begin
  With TSimpleFileModule.CreateNew(Nil) do
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

procedure TSimpleFileModule.HandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  Inherited;
  if Assigned (OnLog) then
    OnLog(etInfo,Format('%d serving "%s" -> "%s"',[AResponse.Code,FRequestedFileName,FMappedFileName]));
end;

class procedure TSimpleFileModule.RegisterDefaultRoute;
begin
  if BaseDir='' then
    BaseDir:=IncludeTrailingPathDelimiter(GetCurrentDir);
  httprouter.RegisterRoute('/*',@HandleSimpleFileRequest);
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
  D : String;

begin
  if (BaseURL='') then
    Result:=AFileName
  else
    begin
    D:=Locations.Values[BaseURL];
    If (D='') then
      Result:=''
    else
      begin
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
    if (BaseURL='') then
      BaseDir:=ExtractFilePath(Paramstr(0))
    end;
  FN:=ExtractRelativepath(BaseDir,aFileName);
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
    AResponse.ContentLength:=F.Size;
    AResponse.ContentStream:=F;
    AResponse.SendContent;
    AResponse.ContentStream:=Nil;
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
  If CompareText(ARequest.Method,'GET')<>0 then
    begin
    AResponse.Code:=405;
    AResponse.CodeText:='Method not allowed';
    AResponse.SendContent;
    Exit;
    end;
  RFN:=GetRequestFileName(ARequest);
  if (RFN='') then
    begin
    AResponse.Code:=400;
    AResponse.CodeText:='Bad request';
    AResponse.SendContent;
    exit;
    end;
  FN:=MapFileName(RFN);
  if (FN='') or not AllowFile(FN) then  
    begin
    AResponse.Code:=403;
    AResponse.CodeText:='Forbidden';
    AResponse.SendContent;
    exit;
    end;
  if  not FileExists(FN) then
    begin
    AResponse.Code:=404;
    AResponse.CodeText:='Not found';
    AResponse.SendContent;
    exit;
    end;
  SendFile(FN,AResponse);
end;

initialization

finalization
  FreeAndNil(Locations);
end.
