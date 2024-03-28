program sendmsg;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, fpjson, jsonparser, fpfcmclient, opensslsockets, fpfcmtypes, fpwebclient, fphttpwebclient;

type

  { TFCMApplication }

  TFCMApplication = class(TCustomApplication)
  private
    FAccessTokenFile : String;
    procedure ConfigureClient(aClient: TFCMClient);
    procedure ConfigureMessage(Msg: TNotificationMessage);
    procedure DoHandleNewToken(Sender: TObject; const aToken: TBearerToken);
    procedure LoadMessageFromFile(Msg: TNotificationMessage; const aFileName: string);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Usage(Msg: String); virtual;
  end;

{ TFCMApplication }

procedure TFCMApplication.ConfigureClient(aClient : TFCMClient);

var
  CfgFile : string;

begin
  // Service account info
  CfgFile:=GetOptionValue('s','service-account');
  if CfgFile='' then
    CfgFile:=ChangeFileExt(ParamStr(0),'-service-account.json');
  if not FileExists(CfgFile) then
    Raise EInOutError.CreateFmt('No service account configuration file found: %s',[CfgFile]);
  aClient.InitServiceAccount(CfgFile,'');
  // Access token reuse
  if HasOption('a','access-token') then
    begin
    FAccessTokenFile:=GetOptionValue('a','access-token');
    // Load initial token
    if FileExists(FAccessTokenFile) then
      aClient.BearerToken.LoadFromFile(FAccessTokenFile);
    // Set handler so we save the token when it was fetched.
    aClient.OnNewBearerToken:=@DoHandleNewToken;
    end;
  // Log file
  if HasOption('l','log') then
    aClient.LogFile:=GetOptionValue('l','log');
end;

procedure TFCMApplication.LoadMessageFromFile(Msg : TNotificationMessage; const aFileName : string);

Var
  F : TFileStream;
  D : TJSONData;
  Obj : TJSONObject absolute D;

begin
  D:=Nil;
  F:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
  try
    D:=GetJSON(F);
    if not (D is TJSONObject) then
      Raise EFCM.CreateFmt('Invalid JSON data in message file %s',[aFileName]);
    Msg.Title:=Obj.Get('title',Msg.Title);
    Msg.Body:=Obj.Get('body',Msg.Body);
    Msg.Image:=Obj.Get('image',Msg.Image);
  finally
    D.Free;
    F.Free;
  end;
end;


procedure TFCMApplication.ConfigureMessage(Msg : TNotificationMessage);

begin
  if HasOption('m','message') then
    LoadMessageFromFile(Msg,GetOptionValue('m','message'));
  if HasOption('t','title') then
    Msg.Title:=GetoptionValue('t','title');
  if HasOption('b','body') then
    Msg.Body:=GetoptionValue('b','body');
  if HasOption('i','image') then
    Msg.Body:=GetoptionValue('i','image');
end;

procedure TFCMApplication.DoHandleNewToken(Sender: TObject; const aToken: TBearerToken);
begin
  aToken.SaveToFile(FAccessTokenFile);
end;

procedure TFCMApplication.DoRun;

const
  Short = 'hm:b:t:r:s:i:l:a:';
  Long : Array of string = ('help','message:','body:','title:','recipient:','service-account:','image:','log:','access-token:');

var
  ErrorMsg: String;
  Msg : TNotificationMessage;
  Client : TFCMClient;
  Recip : String;

begin
  Terminate;
  // quick check parameters
  ErrorMsg:=CheckOptions(Short,Long);
  if (ErrorMsg<>'') or HasOption('h','help') then
    begin
    Usage(ErrorMsg);
    Exit;
    end;
  Recip:=GetOptionValue('r','recipient');
  if Recip='' then
    begin
    Usage('Need a recipient');
    Exit;
    end;
  Msg:=nil;
  Client:=TFCMClient.Create(Self);
  Try
    ConfigureClient(Client);
    Msg:=TNotificationMessage.Create;
    ConfigureMessage(Msg);
    Client.Send(Msg,Recip);
  Finally
    Msg.Free;
    Client.Free;
  end;
end;

constructor TFCMApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

procedure TFCMApplication.Usage(Msg : String);
begin
  Writeln('Error : ',Msg);
  Writeln('Usage: ', ExeName, ' [options] ');
  Writeln('where options is one or more of');
  Writeln('-h --help                  This message');
  Writeln('-a --access-token=FILE     Save (and optionally load) received access token to FILE');
  Writeln('-b --body=TEXT             Set message body text. Overrides settings in -m');
  Writeln('-i --image=URL             Set message image URL. Overrides settings in -m');
  Writeln('-l --log=FILE              Set log file - all HTTP requests will be logged in this file');
  writeln('-m --message=FILE          Set message body, title and image from FILE, a json file with format:' );
  writeln('                           { "title"  : "string", ');
  Writeln('                             "body" : "string", ');
  Writeln('                             "image" : "url" } ');
  Writeln('-r --recipient=TOKEN       Set message recipient token');
  writeln('-s --service-account=FILE  Read service account data from FILE (google service account data)');
  Writeln('-t --title=TEXT            Set message title text. Overrides settings in -m');
  ExitCode:=Ord(Msg<>'');
end;

var
  Application: TFCMApplication;

begin
  DefaultWebClientClass:=TFPHTTPWebClient;
  Application:=TFCMApplication.Create(nil);
  Application.Title:='FCM Test Application';
  Application.Run;
  Application.Free;
end.

