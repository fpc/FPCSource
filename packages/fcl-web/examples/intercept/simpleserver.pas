{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    Sample HTTP server application with 2 interceptors

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}


program simpleserver;

{$IFDEF USEMICROHTTP}
{$UNDEF USEGNUTLS}
{$ENDIF}

uses
{$ifdef unix}
  cthreads,
{$endif}  
  sysutils, strutils, custapp, custhttpapp, Classes, httproute, httpdefs, fpmimetypes, fpwebfile, fpwebproxy, webutil, base64;

Type

  { THTTPApplication }

  THTTPApplication = Class(TCustomHTTPApplication)
  private
    FBaseDir: string;
    FIndexPageName: String;
    FMimeFile: String;
    FNoIndexPage: Boolean;
    FQuiet: Boolean;
    FPassword : string;
    FAuth : String;
    procedure DoAuthorization(ARequest: TRequest; AResponse: TResponse; var aContinue: Boolean);
    procedure DoRequestEnd(ARequest: TRequest; AResponse: TResponse; var aContinue: Boolean);
    procedure DoRequestStart(ARequest: TRequest; AResponse: TResponse; var aContinue: Boolean);
    procedure ProcessOptions;
    procedure Usage(Msg: String);
    procedure Writeinfo;
  published
    procedure DoLog(EventType: TEventType; const Msg: String); override;
    Procedure DoRun; override;
    property Quiet : Boolean read FQuiet Write FQuiet;
    Property MimeFile : String Read FMimeFile Write FMimeFile;
    Property BaseDir : string Read FBaseDir Write FBaseDir;
    Property NoIndexPage : Boolean Read FNoIndexPage Write FNoIndexPage;
    Property IndexPageName : String Read FIndexPageName Write FIndexPageName;
  end;

Var
  Application : THTTPApplication;

{ THTTPApplication }

procedure THTTPApplication.DoLog(EventType: TEventType; const Msg: String);
begin
  if IsConsole then
    Writeln(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now),' [',EventType,'] ',Msg)
  else
    inherited DoLog(EventType, Msg);
end;

procedure THTTPApplication.Usage(Msg : String);

begin
  if (Msg<>'') then
    Writeln('Error: ',Msg);
  Writeln('Usage ',ExtractFileName(ParamStr(0)),' [options] ');
  Writeln('Where options is one or more of : ');
  Writeln('-d --directory=dir   Base directory from which to serve files.');
  Writeln('                     Default is current working directory: ',GetCurrentDir);
  Writeln('-h --help            This help text');
  Writeln('-i --indexpage=name  Directory index page to use (default: index.html)');
  Writeln('-n --noindexpage     Do not allow index page.');
  Writeln('-p --port=NNNN       TCP/IP port to listen on (default is 3000)');
  Writeln('-q --quiet           Do not register log intercepts');
  Writeln('-a --authenticate=PWD Register authentication intercept - authenticate with PWD');
    Halt(Ord(Msg<>''));
end;


procedure THTTPApplication.ProcessOptions;

Var
  S : String;

begin
  Quiet:=HasOption('q','quiet');
  FAuth:=GetoptionValue('a','authenticate');
  Port:=StrToIntDef(GetOptionValue('p','port'),Port);
  if HasOption('d','directory') then
    BaseDir:=GetOptionValue('d','directory');
  if HasOption('H','hostname') then
    HostName:=GetOptionValue('H','hostname');
  if HasOption('n','noindexpage') then
    NoIndexPage:=True
  else
    IndexPageName:=GetOptionValue('i','indexpage');
end;

procedure THTTPApplication.DoRequestStart(ARequest: TRequest; AResponse: TResponse; var aContinue: Boolean);

begin
  DoLog(etInfo,Format('Request %s: %s',[aRequest.RequestID,aRequest.URL]));
end;

procedure THTTPApplication.DoRequestEnd(ARequest: TRequest; AResponse: TResponse; var aContinue: Boolean);

begin
  DoLog(etInfo,Format('Request %s: %s : %d (%d bytes)',[aRequest.RequestID,aRequest.URL,aResponse.Code, aResponse.ContentLength]));
end;

procedure THTTPApplication.DoAuthorization(ARequest: TRequest; AResponse: TResponse; var aContinue: Boolean);

Var
  S : String;

begin
  S:=Trim(aRequest.Authorization);
  aContinue:=SameText(ExtractWord(1,S,[' ']),'Basic');
  if aContinue then
    begin
    S:=ExtractWord(2,S,[' ']); // Username:Password in base64
    S:=DecodeStringBase64(S); // Decode
    S:=ExtractWord(2,S,[':']); // extract password
    aContinue:=SameText(S,Fauth); // Check
    if not aContinue then
      DoLog(etInfo,'Invalid password provided: '+S);
    end
  else
    if S='' then
      DoLog(etInfo,'Missing authorization header')
    else
      DoLog(etInfo,'Invalid authorization header: '+S);
  if not aContinue then
    begin
    aResponse.Code:=401;
    aResponse.CodeText:='Unauthorized';
    aResponse.WWWAuthenticate:='Basic Realm="This site needs a password"';
    aResponse.SendContent;
    end;
end;

procedure THTTPApplication.Writeinfo;

Var
  I : Integer;

begin
  Log(etInfo,'Listening on port %d, serving files from directory: %s (using SSL: %s)',[Port,BaseDir,BoolToStr(UseSSL,'true','false')]);
  if not NoIndexPage then
    Log(etInfo,'Using index page %s',[IndexPageName]);
end;

procedure THTTPApplication.DoRun;

Var
  S : String;

begin
  S:=Checkoptions('hqnd:p:i:a:',['help','quiet','noindexpage','directory:','port:','indexpage:','authenticate:']);
  if (S<>'') or HasOption('h','help') then
    usage(S);
  ProcessOptions;
  if BaseDir='' then
    BaseDir:=GetCurrentDir;
  if (BaseDir<>'') then
    BaseDir:=IncludeTrailingPathDelimiter(BaseDir);
  MimeTypes.LoadKnownTypes;
  if Fauth<>'' then
    HTTPRouter.RegisterInterceptor('auth',@DoAuthorization);
  if not FQuiet then
    begin
    HTTPRouter.RegisterInterceptor('logstart',@DoRequestStart);
    HTTPRouter.RegisterInterceptor('logend',@DoRequestEnd,iaAfter);
    end;
  TSimpleFileModule.RegisterDefaultRoute;
  TSimpleFileModule.BaseDir:=BaseDir;
  TSimpleFileModule.OnLog:=@Log;
  If not NoIndexPage then
    begin
    if (IndexPageName='') then
      IndexPageName:='index.html';
    TSimpleFileModule.IndexPageName:=IndexPageName;
    end;
  inherited;
end;

begin
  Application:=THTTPApplication.Create(Nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.

