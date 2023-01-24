program httppostfile;


{$mode objfpc}{$H+}

uses
  {$ifdef unix}
  cwstring,
  {$endif}
  SysUtils, Classes, fphttpclient, opensslsockets;

Var
  F : TFileStream;
  Vars : TStrings;

begin
  With TFPHTTPClient.Create(Nil) do
    begin
    F:=TFileStream.Create('response.html',fmCreate);
    try
      Vars:=TstringList.Create;
      try
        FileFormPost(ParamStr(1),'myfile',paramstr(2),f);
      finally
        Vars.Free;
      end;
    finally
      F.Free;
    end;
    end;
end.

