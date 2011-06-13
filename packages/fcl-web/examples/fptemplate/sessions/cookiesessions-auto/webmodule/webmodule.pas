unit webmodule; 

{$mode objfpc}{$H+}

interface

uses
  Classes, Sysutils, HTTPDefs, fpHTTP, fpWeb, iniwebsession;

type

  { TFPWebModule1 }

  TFPWebModule1 = class(TFPWebModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleNewSession(Sender: TObject);
    procedure DataModuleSessionExpired(Sender: TObject);
    procedure gotonextpageRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure DataModuleAfterResponse(Sender: TObject; AResponse: TResponse);
  private
    { private declarations }
    NewSessionCreated : Boolean;
    ASessionExpired : Boolean;
    MySessionDir : String;
//    procedure GetSessionEvent(Var ASession : TCustomSession);
    procedure AutoSessionTemplateReplaceTag(Sender: TObject; const TagString:String;
      TagParams: TStringList; Out ReplaceText: String);
  public
    { public declarations }
  end; 

var
  FPWebModule1: TFPWebModule1; 

implementation

{$R *.lfm}

{ TFPWebModule1 }

procedure TFPWebModule1.DataModuleCreate(Sender: TObject);
begin
  NewSessionCreated := false;
  ASessionExpired := false;
  ModuleTemplate.AllowTagParams := true;
  ModuleTemplate.StartDelimiter := '{+';//The default is { and } which is usually not good if we use Javascript in our templates
  ModuleTemplate.EndDelimiter := '+}';

  CreateSession := true;                //Turn on automatic session handling for this web module
  MySessionDir := '';//'/Path/To/A/Directory/';{Use this if you don't want the automatic Temp dir to store the sessionID files under "fpwebsessions" sub-directory}
  with (SessionFactory as TIniSessionFactory) do
  begin
    DefaultTimeoutMinutes := 2;         //Session timeout in minutes
    SessionDir := MySessionDir;
//    SessionCookie:='ACustomCookieName'; {Use this to set the cookie name that will be used for the session management. Default is 'FPWebSession'}
  end;
end;

procedure TFPWebModule1.DataModuleNewSession(Sender: TObject);
begin {Sender as TIniWebSession}
  NewSessionCreated := true;
end;

procedure TFPWebModule1.DataModuleSessionExpired(Sender: TObject);
begin {Sender as TIniWebSession}
  ASessionExpired := true;
end;

procedure TFPWebModule1.gotonextpageRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin     //ModuleTemplate:TFPTemplate is a property of the web module
  ModuleTemplate.FileName := 'autosession-template.html';
  ModuleTemplate.OnReplaceTag := @AutoSessionTemplateReplaceTag;

  AResponse.Content := ModuleTemplate.GetContent;

  Handled := true;
end;

procedure TFPWebModule1.AutoSessionTemplateReplaceTag(Sender: TObject; const TagString:
  String; TagParams: TStringList; Out ReplaceText: String);
begin
  if AnsiCompareText(TagString, 'DATETIME') = 0 then
  begin
    ReplaceText := FormatDateTime(TagParams.Values['FORMAT'], Now);
  end else

  if AnsiCompareText(TagString, 'SESSIONID') = 0 then
  begin
    if Assigned(Session) then
      ReplaceText := Session.SessionID;
  end else

  if AnsiCompareText(TagString, 'TIMEOUTMINUTES') = 0 then
  begin
    if Assigned(Session) then
      ReplaceText := IntToStr(Session.TimeOutMinutes);
  end else

  if AnsiCompareText(TagString, 'SESSIONFILE') = 0 then
  begin
    if Assigned(Session) then
      if MySessionDir = '' then
        ReplaceText := IncludeTrailingPathDelimiter(GetTempDir(True)) + IncludeTrailingPathDelimiter('fpwebsessions') + Session.SessionID
      else
        ReplaceText := IncludeTrailingPathDelimiter(MySessionDir) + Session.SessionID;
{NOTE: GetTempDir
used by the session manager returns the OS temporary directory if possible, or from the
environment variable TEMP . For CGI programs you need to pass global environment
variables, it is not automatic. For example in the Apache httpd.conf with a
"PassEnv TEMP" or "SetEnv TEMP /pathtotmpdir" line so the web server passes this
global environment variable to the CGI programs' local environment variables.
}
  end else

  if AnsiCompareText(TagString, 'EXPIREDMESSAGE') = 0 then
  begin
    if Assigned(Session) and ASessionExpired then
      ReplaceText := TagParams.Values['MESSAGE'];
  end else

    if AnsiCompareText(TagString, 'NEWSESSIONMESSAGE') = 0 then
  begin
    if Assigned(Session) and NewSessionCreated then
      ReplaceText := TagParams.Values['MESSAGE'];
  end else


  begin
    //Not found value for tag -> TagString
    ReplaceText := '[Template tag {+' + TagString + '+} is not implemented yet.]';
  end;
end;

procedure TFPWebModule1.DataModuleAfterResponse(Sender: TObject;
  AResponse: TResponse);
begin
  //reset global variables for apache modules and FCGI applications for the next incoming request
  NewSessionCreated := false;
  ASessionExpired := false;
  ModuleTemplate.FileName := '';
  //
end;

initialization
  RegisterHTTPModule('TFPWebModule1', TFPWebModule1); 
end.
