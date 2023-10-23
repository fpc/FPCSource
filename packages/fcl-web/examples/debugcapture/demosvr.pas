{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    Sample HTTP server application

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}

program demosvr;

uses
  custhttpapp,  sysutils, Classes, jsonparser, fpjson, httproute, httpdefs, fpmimetypes, fpwebfile, fpwebproxy,
  fpdebugcapturesvc;

Type
  { THTTPApplication }

  THTTPApplication = Class(TCustomHTTPApplication)
  private
    procedure HandleCaptureOutput(aSender: TObject; aCapture: TJSONData);
  published
    procedure DoLog(EventType: TEventType; const Msg: String); override;
    Procedure Initialize; override;
  end;

procedure THTTPApplication.DoLog(EventType: TEventType; const Msg: String);
begin
  if IsConsole then
    Writeln(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now),' [',EventType,'] ',Msg)
  else
    inherited DoLog(EventType, Msg);
end;

procedure THTTPApplication.Initialize;

var
  aBaseDir : String;
  Svc : TDebugCaptureService;

begin
  Port:=8080;
  Svc:=TDebugCaptureService.Instance;
  Svc.OnLog:=@DoLog;
  Svc.LogFileName:='debug.log';
  Svc.RegisterHandler('log',@HandleCaptureOutput);
  HTTPRouter.RegisterRoute('/debugcapture',rmPost,@Svc.HandleRequest,False);
  aBaseDir:=IncludeTrailingPathDelimiter(GetCurrentDir);
  TSimpleFileModule.RegisterDefaultRoute;
  TSimpleFileModule.BaseDir:=aBaseDir;
  TSimpleFileModule.OnLog:=@Log;
  TSimpleFileModule.IndexPageName:='index.html';
  MimeTypes.LoadKnownTypes;
  inherited;
end;

procedure THTTPApplication.HandleCaptureOutput(aSender: TObject; aCapture: TJSONData);
begin
  DoLog(etDebug,TDebugCaptureService.JSONDataToString(aCapture));
end;


Var
  Application : THTTPApplication;

begin
  Application:=THTTPApplication.Create(Nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.

