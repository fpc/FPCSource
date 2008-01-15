{$mode objfpc}
{$h+}
unit pkglibcurl;

interface

uses Classes,pkgdownload;

Type
  TLibCurlDownloader = Class(TBaseDownloader)
  Protected
    Procedure LibCurlDownload(Const URL : String; Dest : TStream);
    Procedure FTPDownload(Const URL : String; Dest : TStream); override;
    Procedure HTTPDownload(Const URL : String; Dest : TStream); override;
 end;

implementation

uses sysutils,uriparser,libcurl,pkgmessages,pkgglobals,unixtype;

Function DoStreamWrite(Ptr : Pointer; Size : size_t; nmemb: size_t; Data : Pointer) : size_t;cdecl;

begin
  Result:=TStream(Data).Write(Ptr^,Size*nmemb);
end;

Procedure TLibCurlDownloader.LibCurlDownload(Const URL : String; Dest : TStream);

Var
  HCurl : PCurl;
  ErrorBuffer : Array[0..CURL_ERROR_SIZE] of char;

begin
  hCurl:= curl_easy_init;
  if Assigned(hCurl) then
    Try
      curl_easy_setopt(hCurl,CURLOPT_ERRORBUFFER, [@ErrorBuffer]);
      curl_easy_setopt(hCurl,CURLOPT_URL,[Pchar(URL)]);
      curl_easy_setopt(hCurl,CURLOPT_WRITEFUNCTION,[@DoStreamWrite]);
      curl_easy_setopt(hCurl,CURLOPT_WRITEDATA,[Pointer(Dest)]);
      if Ord(curl_easy_perform(hCurl))<>0 then
        Error(SErrDownloadFailed,[StrPas(@ErrorBuffer)])
    Finally
      curl_easy_cleanup(hCurl);
    end
  else
    Raise Exception.Create('Failed to initialize Curl');
end;


Procedure TLibCurlDownloader.FTPDownload(Const URL : String; Dest : TStream);

begin
  LibCurlDownload(URL,Dest);
end;

Procedure TLibCurlDownloader.HTTPDownload(Const URL : String; Dest : TStream);

begin
  LibCurlDownload(URL,Dest);
end;

initialization
  DownloaderClass:=TLibCurlDownloader;
end.