{$mode objfpc}
{$h+}
unit pkgoCurl;

interface

uses Classes,pkgdownload;

Type
  TOCurlDownloader = Class(TBaseDownloader)
  Private
    FCurl : String;
  Protected
    Procedure OCurlDownload(Const URL : String; Dest : TStream); virtual;
    Procedure FTPDownload(Const URL : String; Dest : TStream); override;
    Procedure HTTPDownload(Const URL : String; Dest : TStream); override;
 Public
    Property Curl : String Read FCurl Write FCurl;
 end;

implementation

uses sysutils,curlobj,pkgmessages;

Procedure TOCurlDownloader.OCurlDownload(Const URL : String; Dest : TStream);

Var
  ACurl : TCurl;
  FN : String;
  F : TFileStream;

begin
  FN:=GetTempFileName();
  Try
    ACurl:=TCurl.Create(Nil);
    Try
      ACurl.URL:=URL;
      ACurl.OutputFile:=FN;
      ACurl.NoProgress:=True;
      ACurl.Verbose:=False;
      ACurl.FollowLocation:=True;
      If Not ACurl.Perform then
        Error(ACurl.ErrorString);
    Finally
      ACurl.Free;
    end;
    F:=TFileStream.Create(FN,fmOpenRead);
    Try
      Dest.CopyFrom(F,0);
    Finally
      F.Free;
    end;
  Finally
    If FileExists(FN) then
      DeleteFile(FN);
  end;
end;

Procedure TOCurlDownloader.FTPDownload(Const URL : String; Dest : TStream);

begin
  OCurlDownload(URL,Dest);
end;

Procedure TOCurlDownloader.HTTPDownload(Const URL : String; Dest : TStream);

begin
  OCurlDownload(URL,Dest);
end;

end.