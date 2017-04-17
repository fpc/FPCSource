{$mode objfpc}
{$h+}
unit fpwebfile;

interface

uses SysUtils, Classes, httpdefs, fphttp, httproute;

Type
  TFPCustomFileModule = Class(TCustomHTTPModule)
  Protected
    // Determine filename frome request.
    Function GetRequestFileName(Const ARequest : TRequest) : String; virtual;
    // Map request filename to physical filename.
    Function MapFileName(Const AFileName : String) : String; virtual;
    // Override to implement security. Returns true by default.
    Function AllowFile(Const AFileName : String) : Boolean; virtual;
    // Actually Send file to client.
    Procedure SendFile(Const AFileName : String; AResponse : TResponse); virtual;
    // Overrides TCustomHTTPModule to implement file serving.
    Procedure HandleRequest(ARequest : TRequest; AResponse : TResponse); override;
  end;
  TFPCustomFileModuleClass = Class of TFPCustomFileModule;

  { TSimpleFileModule }

  TSimpleFileLog = Procedure (EventType : TEventType; Const Msg : String) of object;
  TSimpleFileModule = class(TFPCustomFileModule,IRouteInterface)
  private
    FRequestedFileName,
    FMappedFileName : String;
    class procedure HandleSimpleFileRequest(ARequest: TRequest; AResponse: TResponse); static;
    Function MapFileName(Const AFileName : String) : String; override;
    Function GetRequestFileName(Const ARequest : TRequest) : String; override;
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

begin
  if (ALocation='') then
    Raise HTTPError.Create(SErrNoLocation); 
  if Pos('/',ALocation)<>0 then
    Raise HTTPError.Create(SErrInvalidLocation);
  if not DirectoryExists(ADirectory) then
    Raise HTTPError.Create(SErrInvalidDirectory);
  if (Locations=Nil) then
    Locations:=TStringList.Create;
  if DefaultFileModuleClass=Nil then
    DefaultFileModuleClass:=TFPCustomFileModule;  
  if (ADirectory='') then
    Locations.Values[IncludeHTTPPathDelimiter(ALocation)]:=ExtractFilePath(ParamStr(0))
  else  
    Locations.Values[IncludeHTTPPathDelimiter(ALocation)]:=IncludeTrailingPathDelimiter(ADirectory);
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

function TSimpleFileModule.MapFileName(const AFileName: String): String;

begin
  Result:=AFileName;
  While (Result<>'') and (Result[1]='/') do
    Delete(Result,1,1);
  Result:=IncludeTrailingPathDelimiter(BaseDir)+Result;
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
      end;
    end;
end;

Function TFPCustomFileModule.AllowFile(Const AFileName : String) : Boolean;

begin
  Result:=True;
end;

procedure TFPCustomFileModule.SendFile(Const AFileName : String; AResponse : TResponse);

Var
  F : TFileStream;

begin
  CheckMimeLoaded;
  AResponse.ContentType:=MimeTypes.GetMimeType(ExtractFileExt(AFileName));
  If (AResponse.ContentType='') then
    AResponse.ContentType:='Application/octet-stream';
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
  if (FN='') or not FileExists(FN) then
    begin
    AResponse.Code:=404;
    AResponse.CodeText:='Not found';
    AResponse.SendContent;
    exit;
    end;
  if not AllowFile(FN) then  
    begin
    AResponse.Code:=403;
    AResponse.CodeText:='Forbidden';
    AResponse.SendContent;
    exit;
    end;
  SendFile(FN,AResponse);
end;

initialization

finalization
  FreeAndNil(Locations);
end.
