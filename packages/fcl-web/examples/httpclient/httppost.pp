program httppost;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fphttpclient;

Var
  F : TFileStream;
  Vars : TStrings;
  i : integer;
begin
  With TFPHTTPClient.Create(Nil) do
    begin
    F:=TFileStream.Create('response.html',fmCreate);
    try
      Vars:=TstringList.Create;
      try
        For i:=1 to 10 do
          Vars.Add(Format('Var%d=Value %d',[i,i]));
        FormPost(ParamStr(1),vars,f);
      finally
        Vars.Free;
      end;
    finally
      F.Free;
    end;
    end;
end.

