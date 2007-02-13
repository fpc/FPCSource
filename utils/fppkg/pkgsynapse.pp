{$mode objfpc}
{$h+}
unit pkgsynapse;

interface

uses Classes,pkgdownload;

Type
  TSynapseDownloader = Class(TBaseDownloader)
  Protected
    Procedure FTPDownload(Const URL : String; Dest : TStream); override;
    Procedure HTTPDownload(Const URL : String; Dest : TStream); override;
 end;

implementation

uses sysutils,uriparser,httpsend,ftpsend,pkgmessages;

Procedure TSynapseDownloader.FTPDownload(Const URL : String; Dest : TStream);

Var
  URI : TURI;
  FN : String;
  F : TFileStream;

begin
  // Download in temporary file.
  FN:=GetTempFileName();
  try
    URI:=ParseURI(URL);
    with TFTPSend.Create do
      try
        if URI.UserName <> '' then
         begin
         Username := URI.UserName;
         Password := URI.Password;
         end;
        TargetHost := URI.Host;
        if (URI.Port<>0) then
          TargetPort := IntToStr(URI.Port);
        if not Login then
          Error(SErrLoginFailed);
        DirectFileName := FN;
        DirectFile:=True;
        If (URI.Path<>'') then
          if not ChangeWorkingDir(URI.Path) then
            Error(SErrCWDFailed,[URI.PATH]);
        BinaryMode:=True;
        If Not RetrieveFile(URI.Document, False) then
           Error(SErrGETFailed,[URI.Document]);
        Logout;
      finally
        Free;
      end;
    F:=TFileStream.Create(FN,fmOpenRead);
    Try
      Dest.CopyFrom(F,0);
    Finally
      F.Free;
    end;
  finally
    // Delete temporary file.
    If FileExists(FN) then
      DeleteFile(FN);
  end;
end;

Procedure TSynapseDownloader.HTTPDownload(Const URL : String; Dest : TStream);

begin
  If Not HttpGetBinary(URL,Dest) then
     Error(SErrHTTPGetFailed);
end;

end.