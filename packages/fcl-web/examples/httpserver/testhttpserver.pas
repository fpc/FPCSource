unit testhttpserver;

{$mode objfpc}{$H+}
{$define UseCThreads}

interface

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  sysutils, Classes, fphttpserver, fpmimetypes, URIParser;

Type

  TWriteInfoMethod = procedure(S: string) of object;

  { TTestHTTPServer }

  TTestHTTPServer = Class(TFPHTTPServer)
  private
    FBaseDir : String;
    FCount : Integer;
    FMimeLoaded : Boolean;
    FMimeTypesFile: String;
    FWriteInfo: TWriteInfoMethod;
    procedure SetBaseDir(const AValue: String);
  Protected
    procedure CheckMimeLoaded;

    Property MimeLoaded : Boolean Read FMimeLoaded;
  public
    procedure HandleRequest(Var ARequest: TFPHTTPConnectionRequest;
                            Var AResponse : TFPHTTPConnectionResponse); override;
    Property BaseDir : String Read FBaseDir Write SetBaseDir;
    Property MimeTypesFile : String Read FMimeTypesFile Write FMimeTypesFile;
    Property WriteInfo: TWriteInfoMethod Read FWriteInfo Write FWriteInfo;
  end;

implementation

{ TTestHTTPServer }

procedure TTestHTTPServer.SetBaseDir(const AValue: String);
begin
  if FBaseDir=AValue then exit;
  FBaseDir:=AValue;
  If (FBaseDir<>'') then
    FBaseDir:=IncludeTrailingPathDelimiter(FBaseDir);
end;

procedure TTestHTTPServer.CheckMimeLoaded;
begin
  If (Not MimeLoaded) and (MimeTypesFile<>'') then
    begin
    MimeTypes.LoadFromFile(MimeTypesFile);
    FMimeLoaded:=true;
    end;
end;

procedure TTestHTTPServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);

Var
  F : TFileStream;
  FN : String;
  URI: TURI;
  TimeOut: Longint;

begin
  URI:=ParseURI(ARequest.Url, False);
  FN:=URI.Path+URI.Document;
  if TryStrToInt(URI.Params, TimeOut) then
    Sleep(TimeOut);
  If (length(FN)>0) and (FN[1]='/') then
    Delete(FN,1,1);
  DoDirSeparators(FN);
  FN:=BaseDir+FN;
  if FileExists(FN) then
    begin
    F:=TFileStream.Create(FN,fmOpenRead or fmShareDenyNone);
    try
      CheckMimeLoaded;
      AResponse.ContentType:=MimeTypes.GetMimeType(ExtractFileExt(FN));
      WriteInfo('Connection ('+aRequest.Connection.ConnectionID+') - Request ['+aRequest.RequestID+']: Serving file: "'+Fn+'". Reported Mime type: '+AResponse.ContentType);
      AResponse.ContentLength:=F.Size;
      AResponse.ContentStream:=F;
      AResponse.SendContent;
      AResponse.ContentStream:=Nil;
    finally
      F.Free;
    end;
    end
  else
    begin
    AResponse.Code:=404;
    AResponse.ContentLength:=0;
    AResponse.SendContent;
    end;
  Inc(FCount);
end;

end.

