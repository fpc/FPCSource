program testweb;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, httpdefs, custcgi,cgiapp,fphttp,fpcgi,
  webutil, fpweb;

Type
  TMyWeb=Class(TCustomCGIApplication)
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

procedure TMyWeb.HandleRequest(ARequest: TRequest; AResponse: TResponse);

  Procedure AddNV(Const N,V : String);

  begin
    AResponse.Contents.Add('<TR><TD>'+N+'</TD><TD>'+V+'</TD></TR>');
  end;

Var
  I,P : Integer;
  N,V : String;

begin
  With AResponse.Contents do
    begin
    BeginUpdate;
    Try
      Add('<HTML><TITLE>FPC CGI Test page</TITLE><BODY>');
      DumpRequest(ARequest,AResponse.Contents);
      Add('<H1>CGI environment:</H1>');
      Add('<TABLE BORDER="1">');
      Add('<TR><TD>Name</TD><TD>Value</TD></TR>');
      For I:=1 to GetEnvironmentVariableCount do
        begin
        V:=GetEnvironmentString(i);
        P:=Pos('=',V);
        N:=Copy(V,1,P-1);
        system.Delete(V,1,P);
        AddNV(N,V);
        end;
      Add('</TABLE>');
      Add('</BODY></HTML>');
    Finally
      EndUpdate;
    end;
    end;
end;

Procedure Run;

begin
  With TMyWeb.Create(Nil) do
    try
      Initialize;
      Run;
    Finally
      Free;
    end;
end;

begin
  Run;
end.

