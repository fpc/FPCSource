unit office365client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpwebclient, fpoauth2, fpjwt;

Type
  TAuthMethod = (amOAuth2);

  { TAzureADClaims }
  // Claims returned by Azure AD.

  TAzureADClaims = Class(TClaims)
  private
    FFamilyName: String;
    FGivenName: String;
    FOID: String;
    Fpwd_exp: string;
    Fpwd_url: string;
    FTid: String;
    FUniqueName: string;
    Fupn: String;
    Fver: String;
  Published
    Property unique_name : string read FUniqueName Write FUniqueName;
    Property family_name : String read FFamilyName Write FFamilyName;
    Property given_name : String read FGivenName Write FGivenName;
    Property pwd_exp : string Read Fpwd_exp Write Fpwd_exp;
    Property pwd_url : string Read Fpwd_url Write Fpwd_url;
    Property tid : String Read FTid Write FTID; // GUID
    Property upn : String Read Fupn Write Fupn;
    Property ver : String Read Fver Write Fver;
    Property oid : String Read FOID Write FOID; // GUID
  end;

  { TAzureIDToken }

  TAzureIDToken = Class(TJWTIDtoken)
  private
    function GetAzureClaims: TAzureADClaims;
  Protected
    Function CreateClaims : TClaims; override;
  Public
    Constructor Create;
    Function GetUniqueUserID : String; override;
    Function GetUniqueUserName : String; override;
    Function GetUserDisplayName : String; override;
    Property AzureClaims : TAzureADClaims Read GetAzureClaims;
  end;
  { TAzureADOAuth2Handler }

  TAzureADOAuth2Handler = Class(TOAuth2Handler)
  Protected
    function CreateIDToken: TJWTIDToken;override;
  Public
    Constructor Create(AOwner : TComponent); override;
    Class Function AuthScopeVariableName : String; override;
    Class Function DefaultHostedDomain : String; override;
  end;

  // Authentication V2...

  { TAzureAD2OAuth2Handler }

  TAzureAD2OAuth2Handler = Class(TAzureADOAuth2Handler)
  Public
    Constructor Create(AOwner : TComponent); override;
    Class Function AuthScopeVariableName : String; override;
    Class Function DefaultHostedDomain : String; override;
  end;

  TOffice365Client = CLass(TComponent)
  Private
    FWebClient: TAbstractWebClient;
    FAuthHandler : TOAuth2Handler;
    function GetOnUserConsent: TUserConsentHandler;
    procedure SetAuthHandler(AValue: TOAuth2Handler);
    procedure SetClient(AValue: TAbstractWebClient);
    procedure SetOnUserConsent(AValue: TUserConsentHandler);
  Protected
    Procedure CheckDefaults; virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Function GetAuthHandler : TOAuth2Handler;
  Published
    Property AuthHandler : TOAuth2Handler Read GetAuthHandler Write SetAuthHandler;
    Property WebClient : TAbstractWebClient Read FWebClient Write SetClient;
    Property OnUserConsent : TUserConsentHandler Read GetOnUserConsent Write SetOnUserConsent;
  end;

  EOffice365 = Class(Exception);

Const
  DefAUTHURL    = 'https://login.windows.net/%HostedDomain%/oauth2/authorize';
  DefTOKENURL   = 'https://login.windows.net/%HostedDomain%/oauth2/token';

  DefAUTHURLV2  = 'https://login.microsoftonline.com/%HostedDomain%/oauth2/v2.0/authorize';
  DefTOKENURLV2 = 'https://login.microsoftonline.com/%HostedDomain%/oauth2/v2.0/token';


implementation



Function StringToAuthMethod (Const S : String) : TAuthMethod;

begin
  Case Lowercase(S) of
    'oauth2' : Result:=amOAuth2;
  end;
end;

Function StringToAccessType(const S : String) : TAccessType;

begin
  Case lowercase(S) of
    'online' : Result:=atonline;
    'offline' : Result:=atoffline;
  end;
end;

{ TAzureAD2OAuth2Handler }

constructor TAzureAD2OAuth2Handler.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  Config.TokenURL:=DefTOKENURLV2;
  Config.AuthURL:=DefAuthURLV2;
end;

class function TAzureAD2OAuth2Handler.AuthScopeVariableName: String;

begin
  Result:='scope';
end;

class function TAzureAD2OAuth2Handler.DefaultHostedDomain: String;
begin
  Result:='common';
end;

{ TAzureIDToken }

function TAzureIDToken.GetAzureClaims: TAzureADClaims;
begin
  if Claims is TAzureADClaims then
    Result:=TAzureADClaims(Claims)
  else
    Result:=Nil;
end;

function TAzureIDToken.CreateClaims: TClaims;
begin
  If ClaimsClass=Nil then
    Result:=TAzureADClaims.Create
  else
    Result:=inherited CreateClaims;
end;

constructor TAzureIDToken.Create;
begin
  Inherited CreateWithClasses(TAzureADClaims,Nil)
end;

function TAzureIDToken.GetUniqueUserID: String;
begin
  if Assigned(AZureClaims) then
    Result:=AZureClaims.upn
  else
    Result:=inherited GetUniqueUserID;
end;

function TAzureIDToken.GetUniqueUserName: String;
begin
  if Assigned(AZureClaims) then
    Result:=AZureClaims.unique_name
  else
    Result:=inherited GetUniqueUserName;
end;

function TAzureIDToken.GetUserDisplayName: String;
begin
  if Assigned(AZureClaims) then
    Result:=AZureClaims.Given_Name+' '+AZureClaims.Family_Name
  else
    Result:=inherited GetUserDisplayName;
end;

{ TAzureOAuth2Handler }

function TAzureADOAuth2Handler.CreateIDToken: TJWTIDToken;
begin
  Result:=TAzureIDToken.CreateWithClasses(TAzureADClaims,Nil);
end;

Constructor TAzureADOAuth2Handler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Config.TokenURL:=DefTOKENURL;
  Config.AuthURL:=DefAuthURL;
end;

Class Function TAzureADOAuth2Handler.AuthScopeVariableName: String;
begin
  Result:='resource';
end;

Class Function TAzureADOAuth2Handler.DefaultHostedDomain: String;

begin
  Result:='common';
end;


{ TOffice365Client }

procedure TOffice365Client.SetClient(AValue: TAbstractWebClient);

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

function TOffice365Client.GetOnUserConsent: TUserConsentHandler;
begin
  Result:=GetAuthHandler.OnUserConsent;
end;

procedure TOffice365Client.SetAuthHandler(AValue: TOAuth2Handler);
begin
  if FAuthHandler=AValue then Exit;
  FAuthHandler:=AValue;
end;

procedure TOffice365Client.SetOnUserConsent(AValue: TUserConsentHandler);
begin
  GetAuthHandler.OnUserConsent:=AValue;
end;

Constructor TOffice365Client.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CheckDefaults;
end;

Procedure TOffice365Client.CheckDefaults;

begin
  If (AuthHandler.Config.AuthURL='') then
    AuthHandler.Config.AuthURL:=DefAuthURL;
  If (AuthHandler.Config.TokenURL='') then
    AuthHandler.Config.TokenURL:=DefTokenURL;
end;


Function TOffice365Client.GetAuthHandler: TOAuth2Handler;


begin
  if (FAuthHandler=Nil) then
    begin
    FAuthHandler:=TAzureADOAuth2Handler.Create(Self);
    if Assigned(FWebClient) then
      begin
      FWebClient.RequestSigner:=FAuthHandler;
      FAuthHandler.WebClient:=FWebClient;
      end;
    end;
  Result:=FAuthHandler;
end;
end.

