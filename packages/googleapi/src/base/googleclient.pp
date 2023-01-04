unit googleclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, inifiles, fpjson, fpwebclient, fpoauth2, fpjwt;

Type

  { TGoogleClaims }

  TGoogleClaims = Class(TClaims)
  private
    Fat_hash: string;
    Fazp: string;
    FEmail: String;
    Femail_verified: Boolean;
  Published
    Property azp : string read Fazp Write Fazp;
    Property email : String read FEmail Write FEmail;
    Property email_verified : Boolean read Femail_verified Write Femail_verified;
    Property at_hash : string Read Fat_hash Write Fat_hash;
  end;

  { TGoogleIDToken }

  TGoogleIDToken = Class(TJWTIDtoken)
  private
    function GetGoogleClaims: TGoogleClaims;
  Protected
    Function CreateClaims : TClaims; override;
  Public
    Constructor Create; override;
    Function GetUniqueUserName : String; override;
    Property GoogleClaims : TGoogleClaims Read GetGoogleClaims;
  end;

  { TGoogleOAuth2Handler }

  TGoogleOAuth2Handler = Class(TOAuth2Handler)
  Protected
    function CreateIDToken: TJWTIDToken;override;
  Public
    Constructor Create(AOwner : TComponent);override;
  end;

  TAuthMethod = (amOAuth2, amOpenID, amDeveloperKey, amServiceAccount);

  { TGoogleClientConfig }

  TGoogleClientConfig = Class(TPersistent)
  private
    FApplicationName: String;
    FAuthMethod: TAuthMethod;
    FEnableGZIP: Boolean;
    procedure SetAuthMethod(AValue: TAuthMethod);
  Public
    Procedure Assign(Source : TPersistent); override;
    procedure LoadFromIni(AIni : TCustomIniFile);
    procedure LoadFromJSON(AJSON : TJSONObject);
    Procedure LoadFromFile(Const AFileName : String);
  Published
    Property EnableGZIP : Boolean Read FEnableGZIP Write FEnableGZIP;
    Property ApplicationName : String Read FApplicationName Write FApplicationName;
    Property AuthMethod : TAuthMethod Read FAuthMethod Write SetAuthMethod;
  end;

  TGoogleClient = CLass(TComponent)
  Private
    FConfig: TGoogleClientConfig;
    FWebClient: TAbstractWebClient;
    FAuthHandler : TOAuth2Handler;
    function GetOnUserConsent: TUserConsentHandler;
    procedure SetAuthHandler(AValue: TOAuth2Handler);
    procedure SetClient(AValue: TAbstractWebClient);
    procedure SetConfig(AValue: TGoogleClientConfig);
    procedure SetOnUserConsent(AValue: TUserConsentHandler);
  Protected
    Procedure CheckDefaults; virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Function GetAuthHandler : TOAuth2Handler;
  Published
    Property AuthHandler : TOAuth2Handler Read GetAuthHandler Write SetAuthHandler;
    Property WebClient : TAbstractWebClient Read FWebClient Write SetClient;
    Property Config : TGoogleClientConfig Read FConfig Write SetConfig;
    Property OnUserConsent : TUserConsentHandler Read GetOnUserConsent Write SetOnUserConsent;
  end;

  EGoogleClient = Class(Exception);

Const
  DefAUTHURL='https://accounts.google.com/o/oauth2/auth';
  DefTOKENURL='https://accounts.google.com/o/oauth2/token';

implementation

uses httpdefs;


Const
  SClient            = 'Client';
  SAuth              = 'Authorization';

  KeyenableGZIP      = 'EnableGZIP';
  KeyApplicationName = 'ApplicationName';
  KeyMethod          = 'Method';

{
  KeyDeveloperKey    = 'developerkey';
  KeyOpenIDRealm     = 'OpenIDRealm';
  KeyHostedDomain    = 'HostedDomain';
}

{ TGoogleOAuth2Handler }

constructor TGoogleOAuth2Handler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Config.TokenURL:=DefTOKENURL;
  Config.AuthURL:=DefAuthURL;
end;

function TGoogleOAuth2Handler.CreateIDToken: TJWTIDToken;
begin
  Result:=TGoogleIDToken.Create;
end;

{ TGoogleIDToken }

function TGoogleIDToken.GetGoogleClaims: TGoogleClaims;
begin
  if Claims is TGoogleClaims then
    Result:=TGoogleClaims(Claims)
  else
    Result:=Nil;
end;

function TGoogleIDToken.CreateClaims: TClaims;
begin
  If ClaimsClass=Nil then
    Result:=TGoogleClaims.Create
  else
    Result:=inherited CreateClaims;
end;

constructor TGoogleIDToken.Create;
begin
  Inherited CreateWithClasses(TGoogleClaims,Nil)
end;

function TGoogleIDToken.GetUniqueUserName: String;
begin
  if Assigned(GoogleClaims) then
    Result:=GoogleClaims.email
  else
    Result:=inherited GetUniqueUserName;
end;



{
Function TOAuth2Handler.AuthenticateURL : String;
begin
  Result:=Config.AuthURL
        + '?'+ 'scope='+HTTPEncode(Config.AuthScope)
        +'&redirect_uri='+HTTPEncode(Config.RedirectUri)
        +'&response_type=code'
        +'&client_id='+HTTPEncode(Config.ClientID);
  if (Config.AccessType=atOffline) then
    Result:=Result+'&access_type=offline'; // Request refresh token.
  if (Config.State<>'') then
    Result:=Result +'&state='+HTTPEncode(Config.State);
end;
  try
    Req:=WebClient.CreateRequest;
    Req.Headers.Values['Content-Type']:='application/x-www-form-urlencoded';
    url:=Config.TOKENURL;
    Body:='client_id='+HTTPEncode(Config.ClientID)+
          '&client_secret='+ HTTPEncode(Config.ClientSecret);
    if (Config.RefreshToken<>'') then
      body:=Body+'&refresh_token='+HTTPEncode(Config.RefreshToken)+
                 '&grant_type=refresh_token'
    else
      begin
      body:=Body+'&code='+HTTPEncode(Config.AuthCode)+
            '&redirect_uri='+HTTPEncode(Config.RedirectUri)+
            '&scope='+HTTPEncode(Config.AuthScope)+
            '&grant_type=authorization_code';
      if Config.AccessType=atOffline then
        Body:=Body;
      end;
    Req.SetContentFromString(Body);
    Resp:=WebClient.ExecuteRequest('POST',url,Req);
    if Not (Resp.StatusCode=200) then
      Raise Exception.CreateFmt(SErrFailedToRefreshToken,[Resp.StatusCode,Resp.StatusText]);
    I:=Resp.Content.Size;
    D:=GetJSON(Resp.Content);
    O:=D as TJSONObject;
    S:=O.Get('access_token',Config.AccessToken);
    Config.AccessToken:=S;
    S:=O.Get('refresh_token',Config.RefreshToken);
    Config.RefreshToken:=S;
    S:=O.Get('token_type',Config.AuthTokenType);
    Config.AuthTokenType:=S;
    I:=O.get('expires_in',0);
    if (I>0) then
      Config.AuthExpires:=Now+(I-10.0) / (3600*24); //skim off 10 secs to avoid race conditions
    Result:=True;
  finally
    D.Free;
    Resp.Free;
    Req.Free;
  end;
end;
}


{ TAuthHandler }


{ TGoogleClientConfig }


procedure TGoogleClientConfig.SetAuthMethod(AValue: TAuthMethod);
begin
  if FAuthMethod=AValue then Exit;
  FAuthMethod:=AValue;
end;

Procedure TGoogleClientConfig.Assign(Source: TPersistent);

Var
  C : TGoogleClientConfig;

begin
  if (Source is TGoogleClientConfig) then
    begin
    C:=Source as TGoogleClientConfig;
    EnableGZIP:=C.EnableGZIP;
    ApplicationName:=C.ApplicationName;
    AuthMethod:=C.AuthMethod;
    end;
  inherited Assign(Source);
end;

Function StringToAuthMethod (Const S : String) : TAuthMethod;

begin
  Case Lowercase(S) of
    'oauth2' : Result:=amOAuth2;
    'openid' : Result:=amOpeniD;
    'developerkey' : Result:=amDeveloperkey;
  end;
end;

procedure TGoogleClientConfig.LoadFromIni(AIni: TCustomIniFile);
begin
  With AIni do
    begin
    EnableGZip:=AIni.ReadBool(SClient,KeyenableGZIP,EnableGZip);
    ApplicationName:=AIni.ReadString(SClient,KeyApplicationName,ApplicationName);
    AuthMethod:=StringToAuthMethod(AIni.ReadString(SAuth,KeyMethod,'oauth2'));
    end;
end;

procedure TGoogleClientConfig.LoadFromJSON(AJSON: TJSONObject);

begin
  With AJSON do
    begin
    EnableGZip:=Get(KeyenableGZIP,EnableGZip);
    ApplicationName:=Get(KeyApplicationName,ApplicationName);
    AuthMethod:=StringToAuthMethod(Get(KeyMethod,'oauth2'));
    end;
end;

Procedure TGoogleClientConfig.LoadFromFile(Const AFileName: String);

Var
  J : TJSONData;
  F : TFileStream;
  Ini : TMemIniFile;

begin
  if (lowercase(ExtractFileExt(AFileName))='.json') then
    begin
    J:=Nil;
    F:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
    try
      J:=GetJSON(F);
    finally
      F.Free
    end;
    try
      LoadFromJSON(J as TJSONObject);
    finally
      J.Free;
    end;
    end
  else
    begin
    Ini:=TMemIniFIle.Create(AFileName);
    try
      LoadFromIni(Ini);
    finally
      Ini.Free;
    end;
    end;
end;

{ TGoogleClient }

procedure TGoogleClient.SetClient(AValue: TAbstractWebClient);


Var
  AH : TOAuth2Handler;
begin
  if FWebClient=AValue then Exit;
  if Assigned(FWebClient) then
    FWebClient.RemoveFreeNotification(Self);
  FWebClient:=AValue;
  if Assigned(FWebClient) then
    begin
    FWebClient.FreeNotification(Self);
    AH:=GetAuthHandler;
    FWebClient.RequestSigner:=AH;
    AH.WebClient:=FWebClient;
    end;
end;

function TGoogleClient.GetOnUserConsent: TUserConsentHandler;
begin
  Result:=GetAuthHandler.OnUserConsent;
end;

procedure TGoogleClient.SetAuthHandler(AValue: TOAuth2Handler);
begin
  if FAuthHandler=AValue then Exit;
  if Assigned(FAuthHandler) then
    FAuthHandler.RemoveFreeNotification(Self);
  FAuthHandler:=AValue;
  if Assigned(FAuthHandler) then
    FAuthHandler.FreeNotification(Self);
end;

procedure TGoogleClient.SetConfig(AValue: TGoogleClientConfig);
begin
  if FConfig=AValue then Exit;
  FConfig.Assign(AValue);
  CheckDefaults;
end;

procedure TGoogleClient.SetOnUserConsent(AValue: TUserConsentHandler);
begin
  GetAuthHandler.OnUserConsent:=AValue;
end;

constructor TGoogleClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConfig:=TGoogleClientConfig.Create;
  CheckDefaults;
end;

destructor TGoogleClient.Destroy;
begin
  FConfig.Free;
  inherited Destroy;
end;

procedure TGoogleClient.CheckDefaults;

begin
  With AuthHandler.Config do
    begin
    If (AuthURL='') then
      AuthURL:=DefAuthURL;
    If (TokenURL='') then
      TokenURL:=DefTokenURL;
    end;
end;


function TGoogleClient.GetAuthHandler: TOAuth2Handler;

begin
  if (FAuthHandler=Nil) then
    begin
    FAuthHandler:=TGoogleOAuth2Handler.Create(Self);
    FAuthHandler.SetSubComponent(True);
    if Assigned(FWebClient) then
      begin
      FWebClient.RequestSigner:=FAuthHandler;
      FAuthHandler.WebClient:=FWebClient;
      end;
    end;
  Result:=FAuthHandler;
end;

end.

