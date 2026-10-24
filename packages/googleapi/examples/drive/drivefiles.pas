{ Google Drive files client: the generated files proxy, extended with
  media download, export and resumable upload. }
unit DriveFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpwebclient, fpopenapiclient,
  drive.Dto, drive.Service.Intf, drive.Service.Impl;

const
  DriveBaseURL = 'https://www.googleapis.com/drive/v3';
  DriveUploadURL = 'https://www.googleapis.com/upload/drive/v3';
  DriveFolderMimeType = 'application/vnd.google-apps.folder';
  GoogleAppsMimePrefix = 'application/vnd.google-apps.';

type
  EDriveFiles = class(Exception);

  { TDriveFilesClient }

  TDriveFilesClient = class(TFilesProxy)
  private
    FUploadURL: String;
    function SendRequest(const aMethod, aURL: String; aRequest: TWebClientRequest): TWebClientResponse;
    function FetchMedia(const aURL: String; aStream: TStream): TServiceResponse;
  public
    constructor Create(aOwner: TComponent); override;
    // Write the content of a stored (non Google Workspace) file to aStream.
    function DownloadContent(const aFileID: String; aStream: TStream): TServiceResponse;
    // Write a Google Workspace document, converted to aMimeType, to aStream.
    function ExportContent(const aFileID, aMimeType: String; aStream: TStream): TServiceResponse;
    // Create file aName in folder aParentID with the content of aStream.
    function UploadContent(const aName, aParentID, aMimeType: String; aStream: TStream): TFileServiceResult;
  published
    // Base URL for media uploads.
    property UploadURL: String read FUploadURL write FUploadURL;
  end;

// Return True if aMimeType denotes a Google Workspace document that must be exported.
function IsGoogleAppsMimeType(const aMimeType: String): Boolean;

implementation

uses
  fpjson, httpprotocol, drive.Serializer;

function IsGoogleAppsMimeType(const aMimeType: String): Boolean;

begin
  Result:=Copy(aMimeType,1,Length(GoogleAppsMimePrefix))=GoogleAppsMimePrefix;
end;


// Copy status, headers and body of a web response to a service response.
function ToServiceResponse(aResponse: TWebClientResponse): TServiceResponse;

begin
  Result:=Default(TServiceResponse);
  Result.StatusCode:=aResponse.StatusCode;
  Result.StatusText:=aResponse.StatusText;
  Result.ContentType:=Trim(aResponse.Headers.Values['Content-Type']);
  Result.Content:=aResponse.GetContentAsString;
end;


// Return the bytes of aStream from aStart to the end as a string.
function StreamTail(aStream: TStream; aStart: Int64): String;

begin
  Result:='';
  SetLength(Result,aStream.Size-aStart);
  if Length(Result)>0 then
    begin
    aStream.Position:=aStart;
    aStream.ReadBuffer(Result[1],Length(Result));
    end;
end;


{ TDriveFilesClient }

constructor TDriveFilesClient.Create(aOwner: TComponent);

begin
  inherited Create(aOwner);
  BaseURL:=DriveBaseURL;
  FUploadURL:=DriveUploadURL;
end;


function TDriveFilesClient.SendRequest(const aMethod, aURL: String; aRequest: TWebClientRequest): TWebClientResponse;

begin
  if not Assigned(WebClient) then
    raise EDriveFiles.Create('No webclient assigned');
  if Assigned(RequestSigner) then
    RequestSigner.SignRequest(aRequest);
  Result:=WebClient.ExecuteRequest(aMethod,aURL,aRequest);
end;


function TDriveFilesClient.FetchMedia(const aURL: String; aStream: TStream): TServiceResponse;

var
  lRequest: TWebClientRequest;
  lResponse: TWebClientResponse;
  lStart: Int64;

begin
  Result:=Default(TServiceResponse);
  lResponse:=nil;
  lStart:=aStream.Position;
  lRequest:=WebClient.CreateRequest;
  try
    lRequest.ResponseContent:=aStream;
    lResponse:=SendRequest('GET',aURL,lRequest);
    Result.StatusCode:=lResponse.StatusCode;
    Result.StatusText:=lResponse.StatusText;
    Result.ContentType:=Trim(lResponse.Headers.Values['Content-Type']);
    // An error body ends up in aStream: return it as Content
    if (Result.StatusCode div 100)<>2 then
      Result.Content:=StreamTail(aStream,lStart);
  finally
    lResponse.Free;
    lRequest.Free;
  end;
end;


function TDriveFilesClient.DownloadContent(const aFileID: String; aStream: TStream): TServiceResponse;

var
  lURL: String;

begin
  lURL:=ReplacePathParam(BuildEndPointURL('/files/{fileId}'),'fileId',aFileID);
  Result:=FetchMedia(lURL+'?alt=media',aStream);
end;


function TDriveFilesClient.ExportContent(const aFileID, aMimeType: String; aStream: TStream): TServiceResponse;

var
  lURL: String;

begin
  lURL:=ReplacePathParam(BuildEndPointURL('/files/{fileId}/export'),'fileId',aFileID);
  Result:=FetchMedia(lURL+'?mimeType='+HTTPEncode(aMimeType),aStream);
end;


function TDriveFilesClient.UploadContent(const aName, aParentID, aMimeType: String; aStream: TStream): TFileServiceResult;

var
  lMeta: TJSONObject;
  lRequest: TWebClientRequest;
  lResponse: TWebClientResponse;
  lServiceResponse: TServiceResponse;
  lSessionURL: String;

begin
  Result:=Default(TFileServiceResult);
  // Start a resumable upload session with the file metadata
  lMeta:=TJSONObject.Create(['name',aName]);
  lResponse:=nil;
  lRequest:=WebClient.CreateRequest;
  try
    if aParentID<>'' then
      lMeta.Add('parents',TJSONArray.Create([aParentID]));
    lRequest.Headers.Values['Content-Type']:='application/json; charset=UTF-8';
    lRequest.Headers.Values['X-Upload-Content-Type']:=aMimeType;
    lRequest.Headers.Values['X-Upload-Content-Length']:=IntToStr(aStream.Size);
    lRequest.SetContentFromString(lMeta.AsJSON);
    lResponse:=SendRequest('POST',FUploadURL+'/files?uploadType=resumable',lRequest);
    lServiceResponse:=ToServiceResponse(lResponse);
    lSessionURL:=Trim(lResponse.Headers.Values['Location']);
  finally
    lResponse.Free;
    lRequest.Free;
    lMeta.Free;
  end;
  if (lServiceResponse.StatusCode div 100)=2 then
    if lSessionURL='' then
      begin
      lServiceResponse.StatusCode:=999;
      lServiceResponse.StatusText:='No upload session URL in response';
      end
    else
      begin
      // Send the file content to the session URL
      lResponse:=nil;
      lRequest:=WebClient.CreateRequest;
      try
        lRequest.Headers.Values['Content-Type']:=aMimeType;
        lRequest.Content:=aStream;
        lResponse:=SendRequest('PUT',lSessionURL,lRequest);
        lServiceResponse:=ToServiceResponse(lResponse);
      finally
        lResponse.Free;
        lRequest.Free;
      end;
      end;
  Result:=TFileServiceResult.Create(lServiceResponse);
  if Result.Success then
    Result.Value:=TFile.Deserialize(lServiceResponse.Content)
  else if lServiceResponse.Content<>'' then
    Result.ErrorText:=Result.ErrorText+': '+lServiceResponse.Content;
end;


end.
