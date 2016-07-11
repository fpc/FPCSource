program demousersgroups;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, jsonparser, fphttpwebclient, fpoauth2ini,
  restbase, odatabase, msgraph, office365client;


type

  { TGroupsDemoApplication }

  TGroupsDemoApplication = class(TCustomApplication)
  private
    FSession,
    FLogFile,
    FConfig : String;
    FCLient : TOffice365Client;
    FGS : TService;
    FGraph : TGraphService;
    FMaxLevel : Integer;
    procedure EnsureService;
    procedure GetUserGroups(AUser: String);
    procedure GetGroupMembers(AGroup : String);
    procedure ShowUsers;
    procedure ShowGroups;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp(Msg : String); virtual;
  end;

procedure TGroupsDemoApplication.EnsureService;

Var
  FIS : TFPOAuth2IniStore;

begin
  // Auth client
  FCLient:=TOffice365Client.Create(Self);
  FClient.WebClient:=TFPHTTPWebClient.Create(Self);
  FClient.WebClient.LogFile:=FLogFile;
  FClient.AuthHandler:=TAzureADOAuth2Handler.Create(Self);
  FIS:=TFPOAuth2IniStore.Create(Self);

  FIS.ConfigFileName:=FConfig;
  FIS.SessionFileName:=FConfig;
  FClient.AuthHandler.Store:=FIS;
  FClient.AuthHandler.LoadConfig();
  FClient.AuthHandler.LoadSession(FSession);
  FClient.AuthHandler.Config.AuthScope:='https://graph.microsoft.com/';
  FClient.WebClient.RequestSigner:=FClient.AuthHandler;
  FClient.AuthHandler.WebClient:=FClient.WebClient;
  // Service
  FGS:=TService.Create(Self);
  FGS.ServiceURL:='https://graph.microsoft.com/v1.0/';
  FGS.APINeedsAuth:=True;
  FGS.WebClient:=FClient.WebClient;
  // Default container
  FGraph:=FGS.GraphService;
end;


procedure TGroupsDemoApplication.GetUserGroups(AUser: String);

Var
  DOES : TdirectoryObjectsEntitySet;
  L : TdirectoryObjectArray;
  D : TdirectoryObject;
  U : TUser;

begin
  if AUser='' then
    U:=FGraph.me
  else
    begin
    U:=FGraph.users.Get(AUser);
    U.BasePath:='/users/'+AUser;
    end;
  DOES:=U.memberOf(FGS);
  L:=Does.ListAll('');
  For D in L do
    begin
    Writeln('ID ',D.id,': ',D.additionalProperties.AsJSON);
    end;

end;

procedure TGroupsDemoApplication.GetGroupMembers(AGroup: String);

Var
  G : TGroup;
  DA : TDirectoryObjectArray;
  D : TDirectoryObject;

begin
  G:=FGraph.groups.get(agroup);
  G.BasePath:='/groups/'+G.Id;
  DA:=G.members(FGS).ListAll('');
  for D in DA do
    begin
    Writeln('Member ',D.Id,': ',D.additionalProperties.AsJSON);
    end;
end;

{ TGroupsDemoApplication }

procedure TGroupsDemoApplication.DoRun;
var
  FUser,ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hl:r:c:s:u:UGg',['help','recurse:','logfile:','config:','session:','user:','users','groups','group']);
  if (ErrorMsg<>'') or HasOption('h', 'help') then
    WriteHelp(ErrorMsg);
  FLogFile:=GetOptionValue('l','logfile');
  if FLogFile='' then
    FLogFile:=ExtractFilePath(ParamStr(0))+'requests.log';
  FConfig:=GetOptionValue('c','config');
  if FConfig='' then
    FConfig:=ExtractFilePath(ParamStr(0))+'msgraph.ini';
  FMaxLevel:=StrToIntDef(GetOptionValue('r','recurse'),3);
  FSession:=GetOptionValue('s','session');
  FUser:=getOptionValue('u','user');
  if (FSession='') then
    WriteHelp('Need session');
  If FMaxLevel<2 then
    FMaxLevel:=2;
  EnsureService;
  if HasOption('G','groups') then
    ShowGroups
  else if HasOption('U','users') then
    ShowUsers
  else if HasOption('g','group') then
    GetGroupMembers(GetOptionValue('g','group'))
  else
    GetUserGroups(FUser);
  Terminate;
end;

procedure TGroupsDemoApplication.ShowUsers;

Var
  L : TUserArray;
  U : TUser;

begin
  L:=FGraph.users.ListAll('');
  for U in L do
    begin
    Writeln('User ',U.Id,' : ',U.DisplayName,', givenName:',U.givenName,', email: ',U.mail);
    end;
end;

procedure TGroupsDemoApplication.ShowGroups;

Var
  L : TGroupArray;
  G : TGroup;

begin
  L:=FGraph.groups.ListAll('');
  for G in L do
    begin
    Writeln('Group ',G.Id,' : ',G.DisplayName,', Mail:',G.mail);
    end;
end;

constructor TGroupsDemoApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TGroupsDemoApplication.Destroy;
begin

  inherited Destroy;
end;

procedure TGroupsDemoApplication.WriteHelp(Msg: String);
begin
  If Msg<>'' then
    Writeln('Error : ',Msg);
  Writeln('Usage: ', ExeName, ' -s session [options]');
  Writeln('Where options : ');
  Writeln('-h --help            this help');
  Writeln('-c --config          config file with session and client data (default msgraph.ini)');
  Writeln('-l --logfile         config file with session and client data (default requests.log)');
  Writeln('-s --session=name    session to load from config file');
  Writeln('-r --recurse=level   maximum recursion level (defult 2)');
  Halt(Ord(Msg<>''));
end;

var
  Application: TGroupsDemoApplication;
begin
  Application:=TGroupsDemoApplication.Create(nil);
  Application.Title:='OneDrive Demo Command-line Application';
  Application.Run;
  Application.Free;
end.

