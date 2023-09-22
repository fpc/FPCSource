program httppost;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fphttpclient, opensslsockets;

Var
  F : TFileStream;
  Vars : TStrings;
  i : integer;
  Fmt : UNicodeString;

begin
  With TFPHTTPClient.Create(Nil) do
    begin
    F:=TFileStream.Create('response.html',fmCreate);
    try
      Vars:=TstringList.Create;
      try
        For i:=1 to 10 do
          begin
          Fmt:='Var%d=Value %d';
          Vars.Add(Format(Fmt,[i,i]));
          end;
        FormPost(ParamStr(1),vars,f);
      finally
        Vars.Free;
      end;
    finally
      F.Free;
    end;
    end;
end.

