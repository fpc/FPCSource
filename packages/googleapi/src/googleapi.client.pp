{ Google API Client

  This unit provides a convenient wrapper for authenticating Google API requests.
  It encapsulates OAuth2 setup and provides easy configuration similar to the
  old TGoogleClient class.

  Usage:
    var
      Client: TGoogleAPIClient;
      CalendarService: TCalendarListProxy;
    begin
      Client := TGoogleAPIClient.Create(nil);
      Client.WebClient := TFPHTTPWebClient.Create(nil);
      Client.LoadCredentials('client_secrets.json');
      Client.Scopes := 'https://www.googleapis.com/auth/calendar';
      Client.OnUserConsent := @HandleConsent;

      CalendarService := TCalendarListProxy.Create(nil);
      CalendarService.BaseURL := 'https://www.googleapis.com/calendar/v3';
      Client.ConfigureService(CalendarService);
      // CalendarService is now authenticated automatically
    end;

  Copyright (C) 2024 Michael Van Canneyt michael@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
}
unit googleapi.client;

{$mode objfpc}
{$h+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.IniFiles, FpJson.Data,
  FpWeb.Client, Jwt.Oauth2, Jwt.Types,
  FpWeb.OpenAPI.Client;
  {$ELSE}
  Classes, SysUtils, IniFiles, fpjson,
  fpwebclient, fpoauth2, fpjwt,
  fpopenapiclient;
  {$ENDIF}

const
  DefGoogleAuthURL = 'https://accounts.google.com/o/oauth2/auth';
  DefGoogleTokenURL = 'https://oauth2.googleapis.com/token';

type
  EGoogleAPIClient = class(Exception);

  { TGoogleClaims - Extended JWT claims for Google }

  TGoogleClaims = class(TClaims)
  private
    Fat_hash: string;
    Fazp: string;
    FEmail: String;
    Femail_verified: Boolean;
  published
    property azp: string read Fazp write Fazp;
    property email: String read FEmail write FEmail;
    property email_verified: Boolean read Femail_verified write Femail_verified;
    property at_hash: string read Fat_hash write Fat_hash;
  end;

  { TGoogleIDToken }

  TGoogleIDToken = class(TJWTIDToken)
  private
    function GetGoogleClaims: TGoogleClaims;
  protected
    function CreateClaims: TClaims; override;
  public
    constructor Create; override;
    function GetUniqueUserName: String; override;
    property GoogleClaims: TGoogleClaims read GetGoogleClaims;
  end;

  { TGoogleOAuth2Handler - OAuth2 handler configured for Google }

  TGoogleOAuth2Handler = class(TOAuth2Handler)
  protected
    function CreateIDToken: TJWTIDToken; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TGoogleAPIClient - Main client class for Google API authentication }

  TGoogleAPIClient = class(TComponent)
  private
    FAuthHandler: TGoogleOAuth2Handler;
    FOwnsWebClient: Boolean;
    FWebClient: TAbstractWebClient;
    function GetAccessToken: string;
    function GetAuthURL: string;
    function GetClientID: string;
    function GetClientSecret: string;
    function GetOnUserConsent: TUserConsentHandler;
    function GetRedirectUri: string;
    function GetRefreshToken: string;
    function GetScopes: string;
    function GetTokenURL: string;
    procedure SetAccessToken(AValue: string);
    procedure SetAuthURL(AValue: string);
    procedure SetClientID(AValue: string);
    procedure SetClientSecret(AValue: string);
    procedure SetOnUserConsent(AValue: TUserConsentHandler);
    procedure SetRedirectUri(AValue: string);
    procedure SetRefreshToken(AValue: string);
    procedure SetScopes(AValue: string);
    procedure SetTokenURL(AValue: string);
    procedure SetWebClient(AValue: TAbstractWebClient);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetAuthHandler: TGoogleOAuth2Handler;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Load OAuth2 credentials from a Google Cloud Console JSON file
    procedure LoadCredentials(const AFileName: string);
    // Load OAuth2 credentials from JSON data
    procedure LoadCredentialsFromJSON(AJSON: TJSONObject);
    // Load OAuth2 credentials from INI file
    procedure LoadCredentialsFromIni(AIni: TCustomIniFile; const ASection: string = 'OAuth2');

    // Configure a service client to use this client's authentication
    procedure ConfigureService(AService: TFPOpenAPIServiceClient);

    // Check if we have valid credentials configured
    function HasCredentials: Boolean;
    // Check if we have a valid access token
    function HasAccessToken: Boolean;
    // Check if access token needs refresh
    function NeedsTokenRefresh: Boolean;

    // Direct access to the OAuth2 handler for advanced configuration
    property AuthHandler: TGoogleOAuth2Handler read GetAuthHandler;
  published
    // The web client used for HTTP requests
    property WebClient: TAbstractWebClient read FWebClient write SetWebClient;

    // OAuth2 configuration properties
    property ClientID: string read GetClientID write SetClientID;
    property ClientSecret: string read GetClientSecret write SetClientSecret;
    property Scopes: string read GetScopes write SetScopes;
    property RedirectUri: string read GetRedirectUri write SetRedirectUri;
    property AuthURL: string read GetAuthURL write SetAuthURL;
    property TokenURL: string read GetTokenURL write SetTokenURL;

    // Token management
    property AccessToken: string read GetAccessToken write SetAccessToken;
    property RefreshToken: string read GetRefreshToken write SetRefreshToken;

    // Event for handling user consent (OAuth2 authorization flow)
    property OnUserConsent: TUserConsentHandler read GetOnUserConsent write SetOnUserConsent;
  end;

implementation

{ TGoogleIDToken }

function TGoogleIDToken.GetGoogleClaims: TGoogleClaims;
begin
  if Claims is TGoogleClaims then
    Result := TGoogleClaims(Claims)
  else
    Result := nil;
end;

function TGoogleIDToken.CreateClaims: TClaims;
begin
  if ClaimsClass = nil then
    Result := TGoogleClaims.Create
  else
    Result := inherited CreateClaims;
end;

constructor TGoogleIDToken.Create;
begin
  inherited CreateWithClasses(TGoogleClaims, nil);
end;

function TGoogleIDToken.GetUniqueUserName: String;
begin
  if Assigned(GoogleClaims) then
    Result := GoogleClaims.email
  else
    Result := inherited GetUniqueUserName;
end;

{ TGoogleOAuth2Handler }

function TGoogleOAuth2Handler.CreateIDToken: TJWTIDToken;
begin
  Result := TGoogleIDToken.Create;
end;

constructor TGoogleOAuth2Handler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Config.TokenURL := DefGoogleTokenURL;
  Config.AuthURL := DefGoogleAuthURL;
end;

{ TGoogleAPIClient }

constructor TGoogleAPIClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwnsWebClient := False;
end;

destructor TGoogleAPIClient.Destroy;
begin
  if FOwnsWebClient and Assigned(FWebClient) then
    FWebClient.Free;
  FreeAndNil(FAuthHandler);
  inherited Destroy;
end;

procedure TGoogleAPIClient.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FWebClient then
      FWebClient := nil;
    if AComponent = FAuthHandler then
      FAuthHandler := nil;
  end;
end;

function TGoogleAPIClient.GetAuthHandler: TGoogleOAuth2Handler;
begin
  if FAuthHandler = nil then
  begin
    FAuthHandler := TGoogleOAuth2Handler.Create(Self);
    FAuthHandler.SetSubComponent(True);
    if Assigned(FWebClient) then
      FAuthHandler.WebClient := FWebClient;
  end;
  Result := FAuthHandler;
end;

procedure TGoogleAPIClient.SetWebClient(AValue: TAbstractWebClient);
begin
  if FWebClient = AValue then Exit;
  if Assigned(FWebClient) then
    FWebClient.RemoveFreeNotification(Self);
  FWebClient := AValue;
  if Assigned(FWebClient) then
  begin
    FWebClient.FreeNotification(Self);
    // Update the auth handler's web client
    if Assigned(FAuthHandler) then
      FAuthHandler.WebClient := FWebClient;
  end;
end;

function TGoogleAPIClient.GetClientID: string;
begin
  Result := GetAuthHandler.Config.ClientID;
end;

procedure TGoogleAPIClient.SetClientID(AValue: string);
begin
  GetAuthHandler.Config.ClientID := AValue;
end;

function TGoogleAPIClient.GetClientSecret: string;
begin
  Result := GetAuthHandler.Config.ClientSecret;
end;

procedure TGoogleAPIClient.SetClientSecret(AValue: string);
begin
  GetAuthHandler.Config.ClientSecret := AValue;
end;

function TGoogleAPIClient.GetScopes: string;
begin
  Result := GetAuthHandler.Config.AuthScope;
end;

procedure TGoogleAPIClient.SetScopes(AValue: string);
begin
  GetAuthHandler.Config.AuthScope := AValue;
end;

function TGoogleAPIClient.GetRedirectUri: string;
begin
  Result := GetAuthHandler.Config.RedirectUri;
end;

procedure TGoogleAPIClient.SetRedirectUri(AValue: string);
begin
  GetAuthHandler.Config.RedirectUri := AValue;
end;

function TGoogleAPIClient.GetAuthURL: string;
begin
  Result := GetAuthHandler.Config.AuthURL;
end;

procedure TGoogleAPIClient.SetAuthURL(AValue: string);
begin
  GetAuthHandler.Config.AuthURL := AValue;
end;

function TGoogleAPIClient.GetTokenURL: string;
begin
  Result := GetAuthHandler.Config.TokenURL;
end;

procedure TGoogleAPIClient.SetTokenURL(AValue: string);
begin
  GetAuthHandler.Config.TokenURL := AValue;
end;

function TGoogleAPIClient.GetAccessToken: string;
begin
  Result := GetAuthHandler.Session.AccessToken;
end;

procedure TGoogleAPIClient.SetAccessToken(AValue: string);
begin
  GetAuthHandler.Session.AccessToken := AValue;
end;

function TGoogleAPIClient.GetRefreshToken: string;
begin
  Result := GetAuthHandler.Session.RefreshToken;
end;

procedure TGoogleAPIClient.SetRefreshToken(AValue: string);
begin
  GetAuthHandler.Session.RefreshToken := AValue;
end;

function TGoogleAPIClient.GetOnUserConsent: TUserConsentHandler;
begin
  Result := GetAuthHandler.OnUserConsent;
end;

procedure TGoogleAPIClient.SetOnUserConsent(AValue: TUserConsentHandler);
begin
  GetAuthHandler.OnUserConsent := AValue;
end;

procedure TGoogleAPIClient.LoadCredentials(const AFileName: string);
var
  J: TJSONData;
  F: TFileStream;
  Ini: TMemIniFile;
begin
  if LowerCase(ExtractFileExt(AFileName)) = '.json' then
  begin
    J := nil;
    F := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      J := GetJSON(F);
    finally
      F.Free;
    end;
    try
      if not (J is TJSONObject) then
        raise EGoogleAPIClient.Create('Invalid credentials file format');
      LoadCredentialsFromJSON(J as TJSONObject);
    finally
      J.Free;
    end;
  end
  else
  begin
    Ini := TMemIniFile.Create(AFileName);
    try
      LoadCredentialsFromIni(Ini);
    finally
      Ini.Free;
    end;
  end;
end;

procedure TGoogleAPIClient.LoadCredentialsFromJSON(AJSON: TJSONObject);
var
  Installed, Web: TJSONObject;
begin
  // Google Cloud Console exports credentials in either "installed" or "web" format
  Installed := AJSON.Get('installed', TJSONObject(nil));
  if Installed = nil then
    Installed := AJSON.Get('web', TJSONObject(nil));

  if Installed <> nil then
  begin
    ClientID := Installed.Get('client_id', '');
    ClientSecret := Installed.Get('client_secret', '');
    AuthURL := Installed.Get('auth_uri', DefGoogleAuthURL);
    TokenURL := Installed.Get('token_uri', DefGoogleTokenURL);
    // For installed apps, the redirect URI is typically urn:ietf:wg:oauth:2.0:oob
    // or http://localhost
    if RedirectUri = '' then
      RedirectUri := 'urn:ietf:wg:oauth:2.0:oob';
  end
  else
  begin
    // Direct format (not wrapped in installed/web)
    ClientID := AJSON.Get('client_id', '');
    ClientSecret := AJSON.Get('client_secret', '');
    AuthURL := AJSON.Get('auth_uri', DefGoogleAuthURL);
    TokenURL := AJSON.Get('token_uri', DefGoogleTokenURL);
    RedirectUri := AJSON.Get('redirect_uri', RedirectUri);
  end;
end;

procedure TGoogleAPIClient.LoadCredentialsFromIni(AIni: TCustomIniFile; const ASection: string);
begin
  with AIni do
  begin
    ClientID := ReadString(ASection, 'ClientID', ClientID);
    ClientSecret := ReadString(ASection, 'ClientSecret', ClientSecret);
    Scopes := ReadString(ASection, 'Scopes', Scopes);
    RedirectUri := ReadString(ASection, 'RedirectUri', RedirectUri);
    AuthURL := ReadString(ASection, 'AuthURL', DefGoogleAuthURL);
    TokenURL := ReadString(ASection, 'TokenURL', DefGoogleTokenURL);
    // Optionally load stored tokens
    AccessToken := ReadString(ASection, 'AccessToken', AccessToken);
    RefreshToken := ReadString(ASection, 'RefreshToken', RefreshToken);
  end;
end;

procedure TGoogleAPIClient.ConfigureService(AService: TFPOpenAPIServiceClient);
begin
  if not Assigned(FWebClient) then
    raise EGoogleAPIClient.Create('WebClient must be assigned before configuring services');

  AService.WebClient := FWebClient;
  AService.RequestSigner := GetAuthHandler;
end;

function TGoogleAPIClient.HasCredentials: Boolean;
begin
  Result := (ClientID <> '') and (ClientSecret <> '');
end;

function TGoogleAPIClient.HasAccessToken: Boolean;
begin
  Result := AccessToken <> '';
end;

function TGoogleAPIClient.NeedsTokenRefresh: Boolean;
begin
  with GetAuthHandler.Session do
    Result := (AccessToken = '') or
              ((AuthExpires > 0) and (Now >= AuthExpires));
end;

end.
