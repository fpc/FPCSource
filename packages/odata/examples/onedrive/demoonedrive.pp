program demoonedrive;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, jsonparser, fphttpwebclient, fpoauth2ini,
  restbase, odatabase, msgraph, office365client;


type

  { TOneDriveDemoApplication }

  TOneDriveDemoApplication = class(TCustomApplication)
  private
    FSession,
    FLogFile,
    FConfig : String;
    FGS : TService;
    FGraph : TGraphService;
    FDrive : TDrive;
    FMaxLevel : Integer;
    FClient : TOffice365Client;
    Procedure DriveItemToTree(ALevel: Integer; AItem: TDriveItem);
    procedure EnsureService;
    procedure GetItemChildren(ALevel: Integer; R: TDriveItem);
    procedure GetDriveItems;
    procedure GetShareableLink(D: TDriveItem);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp(Msg : String); virtual;
  end;

procedure TOneDriveDemoApplication.EnsureService;

Var
  FIS : TFPOAuth2IniStore;

begin
  // Auth client
  FClient:=TOffice365Client.Create(Self);
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

Procedure TOneDriveDemoApplication.DriveItemToTree(ALevel : Integer; AItem : TDriveItem) ;

Var
  S : String;
  I : integer;

begin
  S:='';
  For I:=1 to Alevel do
    S:=S+'   ';
  if Assigned(AItem.folder) then
    S:=S+'*- '
  else
    S:=S+'+- ';
  Writeln(S,AItem.Name+' (URL: '+AItem.webUrl+')');
end;

procedure TOneDriveDemoApplication.GetItemChildren(ALevel: Integer;
  R: TDriveItem);

Var
  AR : TDriveItemArray;
  I : Integer;

begin
  if Assigned(R.folder) and (R.folder.childCount>0) then
    begin
    AR:=FDrive.items(FGS).Get(R.id).children(FGS).ListAll('');
    For I:=0 to Length(AR)-1 do
      begin
      DriveItemToTree(ALevel,AR[I]);
      if (ALevel<FMaxLevel) then
        GetItemChildren(ALevel+1,AR[I]);
      end;
    end;
end;

procedure TOneDriveDemoApplication.GetDriveItems;

Var
  Me : TUser;
  R : TDriveItem;
  AR : TDriveItemArray;
  I,L : Integer;

begin
  Me:=FGraph.me;
  FDrive:=Me.drive(FGS);
  R:=FDrive.root(FGS);
  DriveItemToTree(0,R);
//  AR:=TDriveItemArray(FGS.ArrayServiceCall(FGS.ServiceURL+'me/drive/root/children',TdriveItem).Data);
  AR:=R.children(FGS).ListAll('');
  L:=Length(AR);
  For I:=0 to L-1 do
    begin
    DriveItemToTree(1,AR[I]);
    if Assigned(AR[I].folder) and (AR[I].folder.childCount>0) then
      GetItemChildren(2,AR[I]);
    end;
  if L>0 then
    GetShareableLink(AR[L-1]);
end;

procedure TOneDriveDemoApplication.GetShareableLink(D : TDriveItem);

Var
  P : Tpermission;

begin
  Writeln('Getting shareable link for item(',D.id, ') : ',D.name);
  // By default, the Office365 API doesn't return @odata.id,
  // and drive items are contained (i.e. not directly accessible through a entityset)
  // so we set the base path manually.
  D.BasePath:='/me/drive/items/'+D.ID;
  P:=D.createLink(FGS,'view','organization');
  try
    if not Assigned(P.link) then
      Writeln('No permissions link created')
    else
      Writeln('Shareable URL: ',P.link.webUrl);
  finally
    P.Free;
  end;
end;

{ TOneDriveDemoApplication }

procedure TOneDriveDemoApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hl:r:c:s:',['help','recurse:','logfile:','config:','session:']);
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
  if (FSession='') then
    WriteHelp('Need session');
  If FMaxLevel<2 then
    FMaxLevel:=2;
  EnsureService;
  GetDriveItems;
  Terminate;
end;

constructor TOneDriveDemoApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TOneDriveDemoApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TOneDriveDemoApplication.WriteHelp(Msg: String);
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
  Application: TOneDriveDemoApplication;
begin
  Application:=TOneDriveDemoApplication.Create(nil);
  Application.Title:='OneDrive Demo Command-line Application';
  Application.Run;
  Application.Free;
end.

