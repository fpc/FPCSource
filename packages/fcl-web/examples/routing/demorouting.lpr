program demorouting;

{$DEFINE STANDALONE}

uses
  sysutils,
  routes,
{$IFDEF STANDALONE}
  fphttpapp,
{$ENDIF}
{$IFDEF CGI}
  fpcgi,
{$ENDIF}
  inifiles;


begin
  With TInifile.Create(ChangeFileExt(ParamStr(0),'.ini')) do
    try
      {$IFDEF CGI}
      BaseURL:=ReadString('CGI','BaseURL','');
      {$ENDIF CGI}
      {$IFDEF STANDALONE}
      Application.Port:=ReadInteger('Standalone','Port',8080);
      BaseURL:=ReadString('Standalone','BaseURL','http://localhost:'+IntToStr(Application.Port));
      {$ENDIF STANDALONE}
    finally
      Free;
    end;
  RegisterRoutes;
  Application.Initialize;
  Application.Run;
end.

