unit googleidentitytoolkit;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TCreateAuthUriResponse = Class;
  TDeleteAccountResponse = Class;
  TDownloadAccountResponse = Class;
  TEmailTemplate = Class;
  TGetAccountInfoResponse = Class;
  TGetOobConfirmationCodeResponse = Class;
  TGetRecaptchaParamResponse = Class;
  TIdentitytoolkitRelyingpartyCreateAuthUriRequest = Class;
  TIdentitytoolkitRelyingpartyDeleteAccountRequest = Class;
  TIdentitytoolkitRelyingpartyDownloadAccountRequest = Class;
  TIdentitytoolkitRelyingpartyGetAccountInfoRequest = Class;
  TIdentitytoolkitRelyingpartyGetProjectConfigResponse = Class;
  TIdentitytoolkitRelyingpartyGetPublicKeysResponse = Class;
  TIdentitytoolkitRelyingpartyResetPasswordRequest = Class;
  TIdentitytoolkitRelyingpartySetAccountInfoRequest = Class;
  TIdentitytoolkitRelyingpartySetProjectConfigRequest = Class;
  TIdentitytoolkitRelyingpartySetProjectConfigResponse = Class;
  TIdentitytoolkitRelyingpartySignOutUserRequest = Class;
  TIdentitytoolkitRelyingpartySignOutUserResponse = Class;
  TIdentitytoolkitRelyingpartySignupNewUserRequest = Class;
  TIdentitytoolkitRelyingpartyUploadAccountRequest = Class;
  TIdentitytoolkitRelyingpartyVerifyAssertionRequest = Class;
  TIdentitytoolkitRelyingpartyVerifyCustomTokenRequest = Class;
  TIdentitytoolkitRelyingpartyVerifyPasswordRequest = Class;
  TIdpConfig = Class;
  TRelyingparty = Class;
  TResetPasswordResponse = Class;
  TSetAccountInfoResponse = Class;
  TSignupNewUserResponse = Class;
  TUploadAccountResponse = Class;
  TUserInfo = Class;
  TVerifyAssertionResponse = Class;
  TVerifyCustomTokenResponse = Class;
  TVerifyPasswordResponse = Class;
  TCreateAuthUriResponseArray = Array of TCreateAuthUriResponse;
  TDeleteAccountResponseArray = Array of TDeleteAccountResponse;
  TDownloadAccountResponseArray = Array of TDownloadAccountResponse;
  TEmailTemplateArray = Array of TEmailTemplate;
  TGetAccountInfoResponseArray = Array of TGetAccountInfoResponse;
  TGetOobConfirmationCodeResponseArray = Array of TGetOobConfirmationCodeResponse;
  TGetRecaptchaParamResponseArray = Array of TGetRecaptchaParamResponse;
  TIdentitytoolkitRelyingpartyCreateAuthUriRequestArray = Array of TIdentitytoolkitRelyingpartyCreateAuthUriRequest;
  TIdentitytoolkitRelyingpartyDeleteAccountRequestArray = Array of TIdentitytoolkitRelyingpartyDeleteAccountRequest;
  TIdentitytoolkitRelyingpartyDownloadAccountRequestArray = Array of TIdentitytoolkitRelyingpartyDownloadAccountRequest;
  TIdentitytoolkitRelyingpartyGetAccountInfoRequestArray = Array of TIdentitytoolkitRelyingpartyGetAccountInfoRequest;
  TIdentitytoolkitRelyingpartyGetProjectConfigResponseArray = Array of TIdentitytoolkitRelyingpartyGetProjectConfigResponse;
  TIdentitytoolkitRelyingpartyGetPublicKeysResponseArray = Array of TIdentitytoolkitRelyingpartyGetPublicKeysResponse;
  TIdentitytoolkitRelyingpartyResetPasswordRequestArray = Array of TIdentitytoolkitRelyingpartyResetPasswordRequest;
  TIdentitytoolkitRelyingpartySetAccountInfoRequestArray = Array of TIdentitytoolkitRelyingpartySetAccountInfoRequest;
  TIdentitytoolkitRelyingpartySetProjectConfigRequestArray = Array of TIdentitytoolkitRelyingpartySetProjectConfigRequest;
  TIdentitytoolkitRelyingpartySetProjectConfigResponseArray = Array of TIdentitytoolkitRelyingpartySetProjectConfigResponse;
  TIdentitytoolkitRelyingpartySignOutUserRequestArray = Array of TIdentitytoolkitRelyingpartySignOutUserRequest;
  TIdentitytoolkitRelyingpartySignOutUserResponseArray = Array of TIdentitytoolkitRelyingpartySignOutUserResponse;
  TIdentitytoolkitRelyingpartySignupNewUserRequestArray = Array of TIdentitytoolkitRelyingpartySignupNewUserRequest;
  TIdentitytoolkitRelyingpartyUploadAccountRequestArray = Array of TIdentitytoolkitRelyingpartyUploadAccountRequest;
  TIdentitytoolkitRelyingpartyVerifyAssertionRequestArray = Array of TIdentitytoolkitRelyingpartyVerifyAssertionRequest;
  TIdentitytoolkitRelyingpartyVerifyCustomTokenRequestArray = Array of TIdentitytoolkitRelyingpartyVerifyCustomTokenRequest;
  TIdentitytoolkitRelyingpartyVerifyPasswordRequestArray = Array of TIdentitytoolkitRelyingpartyVerifyPasswordRequest;
  TIdpConfigArray = Array of TIdpConfig;
  TRelyingpartyArray = Array of TRelyingparty;
  TResetPasswordResponseArray = Array of TResetPasswordResponse;
  TSetAccountInfoResponseArray = Array of TSetAccountInfoResponse;
  TSignupNewUserResponseArray = Array of TSignupNewUserResponse;
  TUploadAccountResponseArray = Array of TUploadAccountResponse;
  TUserInfoArray = Array of TUserInfo;
  TVerifyAssertionResponseArray = Array of TVerifyAssertionResponse;
  TVerifyCustomTokenResponseArray = Array of TVerifyCustomTokenResponse;
  TVerifyPasswordResponseArray = Array of TVerifyPasswordResponse;
  //Anonymous types, using auto-generated names
  TSetAccountInfoResponseTypeproviderUserInfoItem = Class;
  TUploadAccountResponseTypeerrorItem = Class;
  TUserInfoTypeproviderUserInfoItem = Class;
  TDownloadAccountResponseTypeusersArray = Array of TUserInfo;
  TGetAccountInfoResponseTypeusersArray = Array of TUserInfo;
  TIdentitytoolkitRelyingpartyGetProjectConfigResponseTypeidpConfigArray = Array of TIdpConfig;
  TIdentitytoolkitRelyingpartySetProjectConfigRequestTypeidpConfigArray = Array of TIdpConfig;
  TIdentitytoolkitRelyingpartyUploadAccountRequestTypeusersArray = Array of TUserInfo;
  TSetAccountInfoResponseTypeproviderUserInfoArray = Array of TSetAccountInfoResponseTypeproviderUserInfoItem;
  TUploadAccountResponseTypeerrorArray = Array of TUploadAccountResponseTypeerrorItem;
  TUserInfoTypeproviderUserInfoArray = Array of TUserInfoTypeproviderUserInfoItem;
  
  { --------------------------------------------------------------------
    TCreateAuthUriResponse
    --------------------------------------------------------------------}
  
  TCreateAuthUriResponse = Class(TGoogleBaseObject)
  Private
    FallProviders : TStringArray;
    FauthUri : String;
    FcaptchaRequired : boolean;
    FforExistingProvider : boolean;
    Fkind : String;
    FproviderId : String;
    Fregistered : boolean;
    FsessionId : String;
  Protected
    //Property setters
    Procedure SetallProviders(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetauthUri(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcaptchaRequired(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetforExistingProvider(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetproviderId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setregistered(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetsessionId(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property allProviders : TStringArray Index 0 Read FallProviders Write SetallProviders;
    Property authUri : String Index 8 Read FauthUri Write SetauthUri;
    Property captchaRequired : boolean Index 16 Read FcaptchaRequired Write SetcaptchaRequired;
    Property forExistingProvider : boolean Index 24 Read FforExistingProvider Write SetforExistingProvider;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property providerId : String Index 40 Read FproviderId Write SetproviderId;
    Property registered : boolean Index 48 Read Fregistered Write Setregistered;
    Property sessionId : String Index 56 Read FsessionId Write SetsessionId;
  end;
  TCreateAuthUriResponseClass = Class of TCreateAuthUriResponse;
  
  { --------------------------------------------------------------------
    TDeleteAccountResponse
    --------------------------------------------------------------------}
  
  TDeleteAccountResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
  end;
  TDeleteAccountResponseClass = Class of TDeleteAccountResponse;
  
  { --------------------------------------------------------------------
    TDownloadAccountResponse
    --------------------------------------------------------------------}
  
  TDownloadAccountResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    Fusers : TDownloadAccountResponseTypeusersArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setusers(AIndex : Integer; const AValue : TDownloadAccountResponseTypeusersArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property users : TDownloadAccountResponseTypeusersArray Index 16 Read Fusers Write Setusers;
  end;
  TDownloadAccountResponseClass = Class of TDownloadAccountResponse;
  
  { --------------------------------------------------------------------
    TEmailTemplate
    --------------------------------------------------------------------}
  
  TEmailTemplate = Class(TGoogleBaseObject)
  Private
    Fbody : String;
    Fformat : String;
    Ffrom : String;
    FfromDisplayName : String;
    FreplyTo : String;
    Fsubject : String;
  Protected
    //Property setters
    Procedure Setbody(AIndex : Integer; const AValue : String); virtual;
    Procedure Setformat(AIndex : Integer; const AValue : String); virtual;
    Procedure Setfrom(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfromDisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreplyTo(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsubject(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property body : String Index 0 Read Fbody Write Setbody;
    Property format : String Index 8 Read Fformat Write Setformat;
    Property from : String Index 16 Read Ffrom Write Setfrom;
    Property fromDisplayName : String Index 24 Read FfromDisplayName Write SetfromDisplayName;
    Property replyTo : String Index 32 Read FreplyTo Write SetreplyTo;
    Property subject : String Index 40 Read Fsubject Write Setsubject;
  end;
  TEmailTemplateClass = Class of TEmailTemplate;
  
  { --------------------------------------------------------------------
    TGetAccountInfoResponse
    --------------------------------------------------------------------}
  
  TGetAccountInfoResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fusers : TGetAccountInfoResponseTypeusersArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setusers(AIndex : Integer; const AValue : TGetAccountInfoResponseTypeusersArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property users : TGetAccountInfoResponseTypeusersArray Index 8 Read Fusers Write Setusers;
  end;
  TGetAccountInfoResponseClass = Class of TGetAccountInfoResponse;
  
  { --------------------------------------------------------------------
    TGetOobConfirmationCodeResponse
    --------------------------------------------------------------------}
  
  TGetOobConfirmationCodeResponse = Class(TGoogleBaseObject)
  Private
    Femail : String;
    Fkind : String;
    FoobCode : String;
  Protected
    //Property setters
    Procedure Setemail(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoobCode(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property email : String Index 0 Read Femail Write Setemail;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property oobCode : String Index 16 Read FoobCode Write SetoobCode;
  end;
  TGetOobConfirmationCodeResponseClass = Class of TGetOobConfirmationCodeResponse;
  
  { --------------------------------------------------------------------
    TGetRecaptchaParamResponse
    --------------------------------------------------------------------}
  
  TGetRecaptchaParamResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FrecaptchaSiteKey : String;
    FrecaptchaStoken : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrecaptchaSiteKey(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrecaptchaStoken(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property recaptchaSiteKey : String Index 8 Read FrecaptchaSiteKey Write SetrecaptchaSiteKey;
    Property recaptchaStoken : String Index 16 Read FrecaptchaStoken Write SetrecaptchaStoken;
  end;
  TGetRecaptchaParamResponseClass = Class of TGetRecaptchaParamResponse;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyCreateAuthUriRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyCreateAuthUriRequest = Class(TGoogleBaseObject)
  Private
    FappId : String;
    FclientId : String;
    Fcontext : String;
    FcontinueUri : String;
    Fidentifier : String;
    FoauthConsumerKey : String;
    FoauthScope : String;
    FopenidRealm : String;
    FotaApp : String;
    FproviderId : String;
  Protected
    //Property setters
    Procedure SetappId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetclientId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcontext(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcontinueUri(AIndex : Integer; const AValue : String); virtual;
    Procedure Setidentifier(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoauthConsumerKey(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoauthScope(AIndex : Integer; const AValue : String); virtual;
    Procedure SetopenidRealm(AIndex : Integer; const AValue : String); virtual;
    Procedure SetotaApp(AIndex : Integer; const AValue : String); virtual;
    Procedure SetproviderId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property appId : String Index 0 Read FappId Write SetappId;
    Property clientId : String Index 8 Read FclientId Write SetclientId;
    Property context : String Index 16 Read Fcontext Write Setcontext;
    Property continueUri : String Index 24 Read FcontinueUri Write SetcontinueUri;
    Property identifier : String Index 32 Read Fidentifier Write Setidentifier;
    Property oauthConsumerKey : String Index 40 Read FoauthConsumerKey Write SetoauthConsumerKey;
    Property oauthScope : String Index 48 Read FoauthScope Write SetoauthScope;
    Property openidRealm : String Index 56 Read FopenidRealm Write SetopenidRealm;
    Property otaApp : String Index 64 Read FotaApp Write SetotaApp;
    Property providerId : String Index 72 Read FproviderId Write SetproviderId;
  end;
  TIdentitytoolkitRelyingpartyCreateAuthUriRequestClass = Class of TIdentitytoolkitRelyingpartyCreateAuthUriRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyDeleteAccountRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyDeleteAccountRequest = Class(TGoogleBaseObject)
  Private
    FdelegatedProjectNumber : String;
    FidToken : String;
    FlocalId : String;
  Protected
    //Property setters
    Procedure SetdelegatedProjectNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure SetidToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlocalId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property delegatedProjectNumber : String Index 0 Read FdelegatedProjectNumber Write SetdelegatedProjectNumber;
    Property idToken : String Index 8 Read FidToken Write SetidToken;
    Property localId : String Index 16 Read FlocalId Write SetlocalId;
  end;
  TIdentitytoolkitRelyingpartyDeleteAccountRequestClass = Class of TIdentitytoolkitRelyingpartyDeleteAccountRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyDownloadAccountRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyDownloadAccountRequest = Class(TGoogleBaseObject)
  Private
    FdelegatedProjectNumber : String;
    FmaxResults : integer;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetdelegatedProjectNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmaxResults(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property delegatedProjectNumber : String Index 0 Read FdelegatedProjectNumber Write SetdelegatedProjectNumber;
    Property maxResults : integer Index 8 Read FmaxResults Write SetmaxResults;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TIdentitytoolkitRelyingpartyDownloadAccountRequestClass = Class of TIdentitytoolkitRelyingpartyDownloadAccountRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyGetAccountInfoRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyGetAccountInfoRequest = Class(TGoogleBaseObject)
  Private
    FdelegatedProjectNumber : String;
    Femail : TStringArray;
    FidToken : String;
    FlocalId : TStringArray;
  Protected
    //Property setters
    Procedure SetdelegatedProjectNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetidToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlocalId(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property delegatedProjectNumber : String Index 0 Read FdelegatedProjectNumber Write SetdelegatedProjectNumber;
    Property email : TStringArray Index 8 Read Femail Write Setemail;
    Property idToken : String Index 16 Read FidToken Write SetidToken;
    Property localId : TStringArray Index 24 Read FlocalId Write SetlocalId;
  end;
  TIdentitytoolkitRelyingpartyGetAccountInfoRequestClass = Class of TIdentitytoolkitRelyingpartyGetAccountInfoRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyGetProjectConfigResponse
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyGetProjectConfigResponse = Class(TGoogleBaseObject)
  Private
    FallowPasswordUser : boolean;
    FapiKey : String;
    FauthorizedDomains : TStringArray;
    FchangeEmailTemplate : TEmailTemplate;
    FenableAnonymousUser : boolean;
    FidpConfig : TIdentitytoolkitRelyingpartyGetProjectConfigResponseTypeidpConfigArray;
    FlegacyResetPasswordTemplate : TEmailTemplate;
    FprojectId : String;
    FresetPasswordTemplate : TEmailTemplate;
    FuseEmailSending : boolean;
    FverifyEmailTemplate : TEmailTemplate;
  Protected
    //Property setters
    Procedure SetallowPasswordUser(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetapiKey(AIndex : Integer; const AValue : String); virtual;
    Procedure SetauthorizedDomains(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetchangeEmailTemplate(AIndex : Integer; const AValue : TEmailTemplate); virtual;
    Procedure SetenableAnonymousUser(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetidpConfig(AIndex : Integer; const AValue : TIdentitytoolkitRelyingpartyGetProjectConfigResponseTypeidpConfigArray); virtual;
    Procedure SetlegacyResetPasswordTemplate(AIndex : Integer; const AValue : TEmailTemplate); virtual;
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetresetPasswordTemplate(AIndex : Integer; const AValue : TEmailTemplate); virtual;
    Procedure SetuseEmailSending(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetverifyEmailTemplate(AIndex : Integer; const AValue : TEmailTemplate); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property allowPasswordUser : boolean Index 0 Read FallowPasswordUser Write SetallowPasswordUser;
    Property apiKey : String Index 8 Read FapiKey Write SetapiKey;
    Property authorizedDomains : TStringArray Index 16 Read FauthorizedDomains Write SetauthorizedDomains;
    Property changeEmailTemplate : TEmailTemplate Index 24 Read FchangeEmailTemplate Write SetchangeEmailTemplate;
    Property enableAnonymousUser : boolean Index 32 Read FenableAnonymousUser Write SetenableAnonymousUser;
    Property idpConfig : TIdentitytoolkitRelyingpartyGetProjectConfigResponseTypeidpConfigArray Index 40 Read FidpConfig Write SetidpConfig;
    Property legacyResetPasswordTemplate : TEmailTemplate Index 48 Read FlegacyResetPasswordTemplate Write SetlegacyResetPasswordTemplate;
    Property projectId : String Index 56 Read FprojectId Write SetprojectId;
    Property resetPasswordTemplate : TEmailTemplate Index 64 Read FresetPasswordTemplate Write SetresetPasswordTemplate;
    Property useEmailSending : boolean Index 72 Read FuseEmailSending Write SetuseEmailSending;
    Property verifyEmailTemplate : TEmailTemplate Index 80 Read FverifyEmailTemplate Write SetverifyEmailTemplate;
  end;
  TIdentitytoolkitRelyingpartyGetProjectConfigResponseClass = Class of TIdentitytoolkitRelyingpartyGetProjectConfigResponse;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyGetPublicKeysResponse
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyGetPublicKeysResponse = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TIdentitytoolkitRelyingpartyGetPublicKeysResponseClass = Class of TIdentitytoolkitRelyingpartyGetPublicKeysResponse;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyResetPasswordRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyResetPasswordRequest = Class(TGoogleBaseObject)
  Private
    Femail : String;
    FnewPassword : String;
    FoldPassword : String;
    FoobCode : String;
  Protected
    //Property setters
    Procedure Setemail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnewPassword(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoldPassword(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoobCode(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property email : String Index 0 Read Femail Write Setemail;
    Property newPassword : String Index 8 Read FnewPassword Write SetnewPassword;
    Property oldPassword : String Index 16 Read FoldPassword Write SetoldPassword;
    Property oobCode : String Index 24 Read FoobCode Write SetoobCode;
  end;
  TIdentitytoolkitRelyingpartyResetPasswordRequestClass = Class of TIdentitytoolkitRelyingpartyResetPasswordRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartySetAccountInfoRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartySetAccountInfoRequest = Class(TGoogleBaseObject)
  Private
    FcaptchaChallenge : String;
    FcaptchaResponse : String;
    FdelegatedProjectNumber : String;
    FdeleteAttribute : TStringArray;
    FdeleteProvider : TStringArray;
    FdisableUser : boolean;
    FdisplayName : String;
    Femail : String;
    FemailVerified : boolean;
    FidToken : String;
    FinstanceId : String;
    FlocalId : String;
    FoobCode : String;
    Fpassword : String;
    FphotoUrl : String;
    Fprovider : TStringArray;
    FreturnSecureToken : boolean;
    FupgradeToFederatedLogin : boolean;
    FvalidSince : String;
  Protected
    //Property setters
    Procedure SetcaptchaChallenge(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcaptchaResponse(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdelegatedProjectNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdeleteAttribute(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetdeleteProvider(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetdisableUser(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetemailVerified(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetidToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinstanceId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlocalId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoobCode(AIndex : Integer; const AValue : String); virtual;
    Procedure Setpassword(AIndex : Integer; const AValue : String); virtual;
    Procedure SetphotoUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setprovider(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetreturnSecureToken(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetupgradeToFederatedLogin(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetvalidSince(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property captchaChallenge : String Index 0 Read FcaptchaChallenge Write SetcaptchaChallenge;
    Property captchaResponse : String Index 8 Read FcaptchaResponse Write SetcaptchaResponse;
    Property delegatedProjectNumber : String Index 16 Read FdelegatedProjectNumber Write SetdelegatedProjectNumber;
    Property deleteAttribute : TStringArray Index 24 Read FdeleteAttribute Write SetdeleteAttribute;
    Property deleteProvider : TStringArray Index 32 Read FdeleteProvider Write SetdeleteProvider;
    Property disableUser : boolean Index 40 Read FdisableUser Write SetdisableUser;
    Property displayName : String Index 48 Read FdisplayName Write SetdisplayName;
    Property email : String Index 56 Read Femail Write Setemail;
    Property emailVerified : boolean Index 64 Read FemailVerified Write SetemailVerified;
    Property idToken : String Index 72 Read FidToken Write SetidToken;
    Property instanceId : String Index 80 Read FinstanceId Write SetinstanceId;
    Property localId : String Index 88 Read FlocalId Write SetlocalId;
    Property oobCode : String Index 96 Read FoobCode Write SetoobCode;
    Property password : String Index 104 Read Fpassword Write Setpassword;
    Property photoUrl : String Index 112 Read FphotoUrl Write SetphotoUrl;
    Property provider : TStringArray Index 120 Read Fprovider Write Setprovider;
    Property returnSecureToken : boolean Index 128 Read FreturnSecureToken Write SetreturnSecureToken;
    Property upgradeToFederatedLogin : boolean Index 136 Read FupgradeToFederatedLogin Write SetupgradeToFederatedLogin;
    Property validSince : String Index 144 Read FvalidSince Write SetvalidSince;
  end;
  TIdentitytoolkitRelyingpartySetAccountInfoRequestClass = Class of TIdentitytoolkitRelyingpartySetAccountInfoRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartySetProjectConfigRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartySetProjectConfigRequest = Class(TGoogleBaseObject)
  Private
    FallowPasswordUser : boolean;
    FapiKey : String;
    FauthorizedDomains : TStringArray;
    FchangeEmailTemplate : TEmailTemplate;
    FdelegatedProjectNumber : String;
    FenableAnonymousUser : boolean;
    FidpConfig : TIdentitytoolkitRelyingpartySetProjectConfigRequestTypeidpConfigArray;
    FlegacyResetPasswordTemplate : TEmailTemplate;
    FresetPasswordTemplate : TEmailTemplate;
    FuseEmailSending : boolean;
    FverifyEmailTemplate : TEmailTemplate;
  Protected
    //Property setters
    Procedure SetallowPasswordUser(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetapiKey(AIndex : Integer; const AValue : String); virtual;
    Procedure SetauthorizedDomains(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetchangeEmailTemplate(AIndex : Integer; const AValue : TEmailTemplate); virtual;
    Procedure SetdelegatedProjectNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure SetenableAnonymousUser(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetidpConfig(AIndex : Integer; const AValue : TIdentitytoolkitRelyingpartySetProjectConfigRequestTypeidpConfigArray); virtual;
    Procedure SetlegacyResetPasswordTemplate(AIndex : Integer; const AValue : TEmailTemplate); virtual;
    Procedure SetresetPasswordTemplate(AIndex : Integer; const AValue : TEmailTemplate); virtual;
    Procedure SetuseEmailSending(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetverifyEmailTemplate(AIndex : Integer; const AValue : TEmailTemplate); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property allowPasswordUser : boolean Index 0 Read FallowPasswordUser Write SetallowPasswordUser;
    Property apiKey : String Index 8 Read FapiKey Write SetapiKey;
    Property authorizedDomains : TStringArray Index 16 Read FauthorizedDomains Write SetauthorizedDomains;
    Property changeEmailTemplate : TEmailTemplate Index 24 Read FchangeEmailTemplate Write SetchangeEmailTemplate;
    Property delegatedProjectNumber : String Index 32 Read FdelegatedProjectNumber Write SetdelegatedProjectNumber;
    Property enableAnonymousUser : boolean Index 40 Read FenableAnonymousUser Write SetenableAnonymousUser;
    Property idpConfig : TIdentitytoolkitRelyingpartySetProjectConfigRequestTypeidpConfigArray Index 48 Read FidpConfig Write SetidpConfig;
    Property legacyResetPasswordTemplate : TEmailTemplate Index 56 Read FlegacyResetPasswordTemplate Write SetlegacyResetPasswordTemplate;
    Property resetPasswordTemplate : TEmailTemplate Index 64 Read FresetPasswordTemplate Write SetresetPasswordTemplate;
    Property useEmailSending : boolean Index 72 Read FuseEmailSending Write SetuseEmailSending;
    Property verifyEmailTemplate : TEmailTemplate Index 80 Read FverifyEmailTemplate Write SetverifyEmailTemplate;
  end;
  TIdentitytoolkitRelyingpartySetProjectConfigRequestClass = Class of TIdentitytoolkitRelyingpartySetProjectConfigRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartySetProjectConfigResponse
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartySetProjectConfigResponse = Class(TGoogleBaseObject)
  Private
    FprojectId : String;
  Protected
    //Property setters
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property projectId : String Index 0 Read FprojectId Write SetprojectId;
  end;
  TIdentitytoolkitRelyingpartySetProjectConfigResponseClass = Class of TIdentitytoolkitRelyingpartySetProjectConfigResponse;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartySignOutUserRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartySignOutUserRequest = Class(TGoogleBaseObject)
  Private
    FinstanceId : String;
    FlocalId : String;
  Protected
    //Property setters
    Procedure SetinstanceId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlocalId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property instanceId : String Index 0 Read FinstanceId Write SetinstanceId;
    Property localId : String Index 8 Read FlocalId Write SetlocalId;
  end;
  TIdentitytoolkitRelyingpartySignOutUserRequestClass = Class of TIdentitytoolkitRelyingpartySignOutUserRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartySignOutUserResponse
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartySignOutUserResponse = Class(TGoogleBaseObject)
  Private
    FlocalId : String;
  Protected
    //Property setters
    Procedure SetlocalId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property localId : String Index 0 Read FlocalId Write SetlocalId;
  end;
  TIdentitytoolkitRelyingpartySignOutUserResponseClass = Class of TIdentitytoolkitRelyingpartySignOutUserResponse;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartySignupNewUserRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartySignupNewUserRequest = Class(TGoogleBaseObject)
  Private
    FcaptchaChallenge : String;
    FcaptchaResponse : String;
    FdisplayName : String;
    Femail : String;
    FidToken : String;
    FinstanceId : String;
    Fpassword : String;
  Protected
    //Property setters
    Procedure SetcaptchaChallenge(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcaptchaResponse(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetidToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinstanceId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setpassword(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property captchaChallenge : String Index 0 Read FcaptchaChallenge Write SetcaptchaChallenge;
    Property captchaResponse : String Index 8 Read FcaptchaResponse Write SetcaptchaResponse;
    Property displayName : String Index 16 Read FdisplayName Write SetdisplayName;
    Property email : String Index 24 Read Femail Write Setemail;
    Property idToken : String Index 32 Read FidToken Write SetidToken;
    Property instanceId : String Index 40 Read FinstanceId Write SetinstanceId;
    Property password : String Index 48 Read Fpassword Write Setpassword;
  end;
  TIdentitytoolkitRelyingpartySignupNewUserRequestClass = Class of TIdentitytoolkitRelyingpartySignupNewUserRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyUploadAccountRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyUploadAccountRequest = Class(TGoogleBaseObject)
  Private
    FdelegatedProjectNumber : String;
    FhashAlgorithm : String;
    FmemoryCost : integer;
    Frounds : integer;
    FsaltSeparator : String;
    FsignerKey : String;
    Fusers : TIdentitytoolkitRelyingpartyUploadAccountRequestTypeusersArray;
  Protected
    //Property setters
    Procedure SetdelegatedProjectNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure SethashAlgorithm(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmemoryCost(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setrounds(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetsaltSeparator(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsignerKey(AIndex : Integer; const AValue : String); virtual;
    Procedure Setusers(AIndex : Integer; const AValue : TIdentitytoolkitRelyingpartyUploadAccountRequestTypeusersArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property delegatedProjectNumber : String Index 0 Read FdelegatedProjectNumber Write SetdelegatedProjectNumber;
    Property hashAlgorithm : String Index 8 Read FhashAlgorithm Write SethashAlgorithm;
    Property memoryCost : integer Index 16 Read FmemoryCost Write SetmemoryCost;
    Property rounds : integer Index 24 Read Frounds Write Setrounds;
    Property saltSeparator : String Index 32 Read FsaltSeparator Write SetsaltSeparator;
    Property signerKey : String Index 40 Read FsignerKey Write SetsignerKey;
    Property users : TIdentitytoolkitRelyingpartyUploadAccountRequestTypeusersArray Index 48 Read Fusers Write Setusers;
  end;
  TIdentitytoolkitRelyingpartyUploadAccountRequestClass = Class of TIdentitytoolkitRelyingpartyUploadAccountRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyVerifyAssertionRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyVerifyAssertionRequest = Class(TGoogleBaseObject)
  Private
    FdelegatedProjectNumber : String;
    FidToken : String;
    FinstanceId : String;
    FpendingIdToken : String;
    FpostBody : String;
    FrequestUri : String;
    FreturnRefreshToken : boolean;
    FreturnSecureToken : boolean;
    FsessionId : String;
  Protected
    //Property setters
    Procedure SetdelegatedProjectNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure SetidToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinstanceId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpendingIdToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpostBody(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrequestUri(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreturnRefreshToken(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetreturnSecureToken(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetsessionId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property delegatedProjectNumber : String Index 0 Read FdelegatedProjectNumber Write SetdelegatedProjectNumber;
    Property idToken : String Index 8 Read FidToken Write SetidToken;
    Property instanceId : String Index 16 Read FinstanceId Write SetinstanceId;
    Property pendingIdToken : String Index 24 Read FpendingIdToken Write SetpendingIdToken;
    Property postBody : String Index 32 Read FpostBody Write SetpostBody;
    Property requestUri : String Index 40 Read FrequestUri Write SetrequestUri;
    Property returnRefreshToken : boolean Index 48 Read FreturnRefreshToken Write SetreturnRefreshToken;
    Property returnSecureToken : boolean Index 56 Read FreturnSecureToken Write SetreturnSecureToken;
    Property sessionId : String Index 64 Read FsessionId Write SetsessionId;
  end;
  TIdentitytoolkitRelyingpartyVerifyAssertionRequestClass = Class of TIdentitytoolkitRelyingpartyVerifyAssertionRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyVerifyCustomTokenRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyVerifyCustomTokenRequest = Class(TGoogleBaseObject)
  Private
    FdelegatedProjectNumber : String;
    FinstanceId : String;
    FreturnSecureToken : boolean;
    Ftoken : String;
  Protected
    //Property setters
    Procedure SetdelegatedProjectNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinstanceId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreturnSecureToken(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Settoken(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property delegatedProjectNumber : String Index 0 Read FdelegatedProjectNumber Write SetdelegatedProjectNumber;
    Property instanceId : String Index 8 Read FinstanceId Write SetinstanceId;
    Property returnSecureToken : boolean Index 16 Read FreturnSecureToken Write SetreturnSecureToken;
    Property token : String Index 24 Read Ftoken Write Settoken;
  end;
  TIdentitytoolkitRelyingpartyVerifyCustomTokenRequestClass = Class of TIdentitytoolkitRelyingpartyVerifyCustomTokenRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyVerifyPasswordRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyVerifyPasswordRequest = Class(TGoogleBaseObject)
  Private
    FcaptchaChallenge : String;
    FcaptchaResponse : String;
    FdelegatedProjectNumber : String;
    Femail : String;
    FidToken : String;
    FinstanceId : String;
    Fpassword : String;
    FpendingIdToken : String;
    FreturnSecureToken : boolean;
  Protected
    //Property setters
    Procedure SetcaptchaChallenge(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcaptchaResponse(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdelegatedProjectNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetidToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinstanceId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setpassword(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpendingIdToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreturnSecureToken(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property captchaChallenge : String Index 0 Read FcaptchaChallenge Write SetcaptchaChallenge;
    Property captchaResponse : String Index 8 Read FcaptchaResponse Write SetcaptchaResponse;
    Property delegatedProjectNumber : String Index 16 Read FdelegatedProjectNumber Write SetdelegatedProjectNumber;
    Property email : String Index 24 Read Femail Write Setemail;
    Property idToken : String Index 32 Read FidToken Write SetidToken;
    Property instanceId : String Index 40 Read FinstanceId Write SetinstanceId;
    Property password : String Index 48 Read Fpassword Write Setpassword;
    Property pendingIdToken : String Index 56 Read FpendingIdToken Write SetpendingIdToken;
    Property returnSecureToken : boolean Index 64 Read FreturnSecureToken Write SetreturnSecureToken;
  end;
  TIdentitytoolkitRelyingpartyVerifyPasswordRequestClass = Class of TIdentitytoolkitRelyingpartyVerifyPasswordRequest;
  
  { --------------------------------------------------------------------
    TIdpConfig
    --------------------------------------------------------------------}
  
  TIdpConfig = Class(TGoogleBaseObject)
  Private
    FclientId : String;
    Fenabled : boolean;
    FexperimentPercent : integer;
    Fprovider : String;
    Fsecret : String;
  Protected
    //Property setters
    Procedure SetclientId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setenabled(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetexperimentPercent(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setprovider(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsecret(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property clientId : String Index 0 Read FclientId Write SetclientId;
    Property enabled : boolean Index 8 Read Fenabled Write Setenabled;
    Property experimentPercent : integer Index 16 Read FexperimentPercent Write SetexperimentPercent;
    Property provider : String Index 24 Read Fprovider Write Setprovider;
    Property secret : String Index 32 Read Fsecret Write Setsecret;
  end;
  TIdpConfigClass = Class of TIdpConfig;
  
  { --------------------------------------------------------------------
    TRelyingparty
    --------------------------------------------------------------------}
  
  TRelyingparty = Class(TGoogleBaseObject)
  Private
    FcaptchaResp : String;
    Fchallenge : String;
    Femail : String;
    FidToken : String;
    Fkind : String;
    FnewEmail : String;
    FrequestType : String;
    FuserIp : String;
  Protected
    //Property setters
    Procedure SetcaptchaResp(AIndex : Integer; const AValue : String); virtual;
    Procedure Setchallenge(AIndex : Integer; const AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetidToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnewEmail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrequestType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetuserIp(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property captchaResp : String Index 0 Read FcaptchaResp Write SetcaptchaResp;
    Property challenge : String Index 8 Read Fchallenge Write Setchallenge;
    Property email : String Index 16 Read Femail Write Setemail;
    Property idToken : String Index 24 Read FidToken Write SetidToken;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property newEmail : String Index 40 Read FnewEmail Write SetnewEmail;
    Property requestType : String Index 48 Read FrequestType Write SetrequestType;
    Property userIp : String Index 56 Read FuserIp Write SetuserIp;
  end;
  TRelyingpartyClass = Class of TRelyingparty;
  
  { --------------------------------------------------------------------
    TResetPasswordResponse
    --------------------------------------------------------------------}
  
  TResetPasswordResponse = Class(TGoogleBaseObject)
  Private
    Femail : String;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setemail(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property email : String Index 0 Read Femail Write Setemail;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TResetPasswordResponseClass = Class of TResetPasswordResponse;
  
  { --------------------------------------------------------------------
    TSetAccountInfoResponseTypeproviderUserInfoItem
    --------------------------------------------------------------------}
  
  TSetAccountInfoResponseTypeproviderUserInfoItem = Class(TGoogleBaseObject)
  Private
    FdisplayName : String;
    FfederatedId : String;
    FphotoUrl : String;
    FproviderId : String;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfederatedId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetphotoUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetproviderId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property displayName : String Index 0 Read FdisplayName Write SetdisplayName;
    Property federatedId : String Index 8 Read FfederatedId Write SetfederatedId;
    Property photoUrl : String Index 16 Read FphotoUrl Write SetphotoUrl;
    Property providerId : String Index 24 Read FproviderId Write SetproviderId;
  end;
  TSetAccountInfoResponseTypeproviderUserInfoItemClass = Class of TSetAccountInfoResponseTypeproviderUserInfoItem;
  
  { --------------------------------------------------------------------
    TSetAccountInfoResponse
    --------------------------------------------------------------------}
  
  TSetAccountInfoResponse = Class(TGoogleBaseObject)
  Private
    FdisplayName : String;
    Femail : String;
    FexpiresIn : String;
    FidToken : String;
    Fkind : String;
    FlocalId : String;
    FnewEmail : String;
    FpasswordHash : String;
    FphotoUrl : String;
    FproviderUserInfo : TSetAccountInfoResponseTypeproviderUserInfoArray;
    FrefreshToken : String;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetexpiresIn(AIndex : Integer; const AValue : String); virtual;
    Procedure SetidToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlocalId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnewEmail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpasswordHash(AIndex : Integer; const AValue : String); virtual;
    Procedure SetphotoUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetproviderUserInfo(AIndex : Integer; const AValue : TSetAccountInfoResponseTypeproviderUserInfoArray); virtual;
    Procedure SetrefreshToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property displayName : String Index 0 Read FdisplayName Write SetdisplayName;
    Property email : String Index 8 Read Femail Write Setemail;
    Property expiresIn : String Index 16 Read FexpiresIn Write SetexpiresIn;
    Property idToken : String Index 24 Read FidToken Write SetidToken;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property localId : String Index 40 Read FlocalId Write SetlocalId;
    Property newEmail : String Index 48 Read FnewEmail Write SetnewEmail;
    Property passwordHash : String Index 56 Read FpasswordHash Write SetpasswordHash;
    Property photoUrl : String Index 64 Read FphotoUrl Write SetphotoUrl;
    Property providerUserInfo : TSetAccountInfoResponseTypeproviderUserInfoArray Index 72 Read FproviderUserInfo Write SetproviderUserInfo;
    Property refreshToken : String Index 80 Read FrefreshToken Write SetrefreshToken;
  end;
  TSetAccountInfoResponseClass = Class of TSetAccountInfoResponse;
  
  { --------------------------------------------------------------------
    TSignupNewUserResponse
    --------------------------------------------------------------------}
  
  TSignupNewUserResponse = Class(TGoogleBaseObject)
  Private
    FdisplayName : String;
    Femail : String;
    FexpiresIn : String;
    FidToken : String;
    Fkind : String;
    FlocalId : String;
    FrefreshToken : String;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetexpiresIn(AIndex : Integer; const AValue : String); virtual;
    Procedure SetidToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlocalId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrefreshToken(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property displayName : String Index 0 Read FdisplayName Write SetdisplayName;
    Property email : String Index 8 Read Femail Write Setemail;
    Property expiresIn : String Index 16 Read FexpiresIn Write SetexpiresIn;
    Property idToken : String Index 24 Read FidToken Write SetidToken;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property localId : String Index 40 Read FlocalId Write SetlocalId;
    Property refreshToken : String Index 48 Read FrefreshToken Write SetrefreshToken;
  end;
  TSignupNewUserResponseClass = Class of TSignupNewUserResponse;
  
  { --------------------------------------------------------------------
    TUploadAccountResponseTypeerrorItem
    --------------------------------------------------------------------}
  
  TUploadAccountResponseTypeerrorItem = Class(TGoogleBaseObject)
  Private
    Findex : integer;
    Fmessage : String;
  Protected
    //Property setters
    Procedure Setindex(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setmessage(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property index : integer Index 0 Read Findex Write Setindex;
    Property message : String Index 8 Read Fmessage Write Setmessage;
  end;
  TUploadAccountResponseTypeerrorItemClass = Class of TUploadAccountResponseTypeerrorItem;
  
  { --------------------------------------------------------------------
    TUploadAccountResponse
    --------------------------------------------------------------------}
  
  TUploadAccountResponse = Class(TGoogleBaseObject)
  Private
    Ferror : TUploadAccountResponseTypeerrorArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Seterror(AIndex : Integer; const AValue : TUploadAccountResponseTypeerrorArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property error : TUploadAccountResponseTypeerrorArray Index 0 Read Ferror Write Seterror;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TUploadAccountResponseClass = Class of TUploadAccountResponse;
  
  { --------------------------------------------------------------------
    TUserInfoTypeproviderUserInfoItem
    --------------------------------------------------------------------}
  
  TUserInfoTypeproviderUserInfoItem = Class(TGoogleBaseObject)
  Private
    FdisplayName : String;
    Femail : String;
    FfederatedId : String;
    FphotoUrl : String;
    FproviderId : String;
    FrawId : String;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfederatedId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetphotoUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetproviderId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrawId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property displayName : String Index 0 Read FdisplayName Write SetdisplayName;
    Property email : String Index 8 Read Femail Write Setemail;
    Property federatedId : String Index 16 Read FfederatedId Write SetfederatedId;
    Property photoUrl : String Index 24 Read FphotoUrl Write SetphotoUrl;
    Property providerId : String Index 32 Read FproviderId Write SetproviderId;
    Property rawId : String Index 40 Read FrawId Write SetrawId;
  end;
  TUserInfoTypeproviderUserInfoItemClass = Class of TUserInfoTypeproviderUserInfoItem;
  
  { --------------------------------------------------------------------
    TUserInfo
    --------------------------------------------------------------------}
  
  TUserInfo = Class(TGoogleBaseObject)
  Private
    Fdisabled : boolean;
    FdisplayName : String;
    Femail : String;
    FemailVerified : boolean;
    FlocalId : String;
    FpasswordHash : String;
    FpasswordUpdatedAt : double;
    FphotoUrl : String;
    FproviderUserInfo : TUserInfoTypeproviderUserInfoArray;
    Fsalt : String;
    FvalidSince : String;
    Fversion : integer;
  Protected
    //Property setters
    Procedure Setdisabled(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetemailVerified(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetlocalId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpasswordHash(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpasswordUpdatedAt(AIndex : Integer; const AValue : double); virtual;
    Procedure SetphotoUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetproviderUserInfo(AIndex : Integer; const AValue : TUserInfoTypeproviderUserInfoArray); virtual;
    Procedure Setsalt(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvalidSince(AIndex : Integer; const AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property disabled : boolean Index 0 Read Fdisabled Write Setdisabled;
    Property displayName : String Index 8 Read FdisplayName Write SetdisplayName;
    Property email : String Index 16 Read Femail Write Setemail;
    Property emailVerified : boolean Index 24 Read FemailVerified Write SetemailVerified;
    Property localId : String Index 32 Read FlocalId Write SetlocalId;
    Property passwordHash : String Index 40 Read FpasswordHash Write SetpasswordHash;
    Property passwordUpdatedAt : double Index 48 Read FpasswordUpdatedAt Write SetpasswordUpdatedAt;
    Property photoUrl : String Index 56 Read FphotoUrl Write SetphotoUrl;
    Property providerUserInfo : TUserInfoTypeproviderUserInfoArray Index 64 Read FproviderUserInfo Write SetproviderUserInfo;
    Property salt : String Index 72 Read Fsalt Write Setsalt;
    Property validSince : String Index 80 Read FvalidSince Write SetvalidSince;
    Property version : integer Index 88 Read Fversion Write Setversion;
  end;
  TUserInfoClass = Class of TUserInfo;
  
  { --------------------------------------------------------------------
    TVerifyAssertionResponse
    --------------------------------------------------------------------}
  
  TVerifyAssertionResponse = Class(TGoogleBaseObject)
  Private
    Faction : String;
    FappInstallationUrl : String;
    FappScheme : String;
    Fcontext : String;
    FdateOfBirth : String;
    FdisplayName : String;
    Femail : String;
    FemailRecycled : boolean;
    FemailVerified : boolean;
    FexpiresIn : String;
    FfederatedId : String;
    FfirstName : String;
    FfullName : String;
    FidToken : String;
    FinputEmail : String;
    Fkind : String;
    Flanguage : String;
    FlastName : String;
    FlocalId : String;
    FneedConfirmation : boolean;
    FneedEmail : boolean;
    FnickName : String;
    FoauthAccessToken : String;
    FoauthAuthorizationCode : String;
    FoauthExpireIn : integer;
    FoauthIdToken : String;
    FoauthRequestToken : String;
    FoauthScope : String;
    FoauthTokenSecret : String;
    ForiginalEmail : String;
    FphotoUrl : String;
    FproviderId : String;
    FrefreshToken : String;
    FtimeZone : String;
    FverifiedProvider : TStringArray;
  Protected
    //Property setters
    Procedure Setaction(AIndex : Integer; const AValue : String); virtual;
    Procedure SetappInstallationUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetappScheme(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcontext(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdateOfBirth(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetemailRecycled(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetemailVerified(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetexpiresIn(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfederatedId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfirstName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfullName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetidToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinputEmail(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlanguage(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlastName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlocalId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetneedConfirmation(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetneedEmail(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetnickName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoauthAccessToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoauthAuthorizationCode(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoauthExpireIn(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetoauthIdToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoauthRequestToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoauthScope(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoauthTokenSecret(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoriginalEmail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetphotoUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetproviderId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrefreshToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SettimeZone(AIndex : Integer; const AValue : String); virtual;
    Procedure SetverifiedProvider(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property action : String Index 0 Read Faction Write Setaction;
    Property appInstallationUrl : String Index 8 Read FappInstallationUrl Write SetappInstallationUrl;
    Property appScheme : String Index 16 Read FappScheme Write SetappScheme;
    Property context : String Index 24 Read Fcontext Write Setcontext;
    Property dateOfBirth : String Index 32 Read FdateOfBirth Write SetdateOfBirth;
    Property displayName : String Index 40 Read FdisplayName Write SetdisplayName;
    Property email : String Index 48 Read Femail Write Setemail;
    Property emailRecycled : boolean Index 56 Read FemailRecycled Write SetemailRecycled;
    Property emailVerified : boolean Index 64 Read FemailVerified Write SetemailVerified;
    Property expiresIn : String Index 72 Read FexpiresIn Write SetexpiresIn;
    Property federatedId : String Index 80 Read FfederatedId Write SetfederatedId;
    Property firstName : String Index 88 Read FfirstName Write SetfirstName;
    Property fullName : String Index 96 Read FfullName Write SetfullName;
    Property idToken : String Index 104 Read FidToken Write SetidToken;
    Property inputEmail : String Index 112 Read FinputEmail Write SetinputEmail;
    Property kind : String Index 120 Read Fkind Write Setkind;
    Property language : String Index 128 Read Flanguage Write Setlanguage;
    Property lastName : String Index 136 Read FlastName Write SetlastName;
    Property localId : String Index 144 Read FlocalId Write SetlocalId;
    Property needConfirmation : boolean Index 152 Read FneedConfirmation Write SetneedConfirmation;
    Property needEmail : boolean Index 160 Read FneedEmail Write SetneedEmail;
    Property nickName : String Index 168 Read FnickName Write SetnickName;
    Property oauthAccessToken : String Index 176 Read FoauthAccessToken Write SetoauthAccessToken;
    Property oauthAuthorizationCode : String Index 184 Read FoauthAuthorizationCode Write SetoauthAuthorizationCode;
    Property oauthExpireIn : integer Index 192 Read FoauthExpireIn Write SetoauthExpireIn;
    Property oauthIdToken : String Index 200 Read FoauthIdToken Write SetoauthIdToken;
    Property oauthRequestToken : String Index 208 Read FoauthRequestToken Write SetoauthRequestToken;
    Property oauthScope : String Index 216 Read FoauthScope Write SetoauthScope;
    Property oauthTokenSecret : String Index 224 Read FoauthTokenSecret Write SetoauthTokenSecret;
    Property originalEmail : String Index 232 Read ForiginalEmail Write SetoriginalEmail;
    Property photoUrl : String Index 240 Read FphotoUrl Write SetphotoUrl;
    Property providerId : String Index 248 Read FproviderId Write SetproviderId;
    Property refreshToken : String Index 256 Read FrefreshToken Write SetrefreshToken;
    Property timeZone : String Index 264 Read FtimeZone Write SettimeZone;
    Property verifiedProvider : TStringArray Index 272 Read FverifiedProvider Write SetverifiedProvider;
  end;
  TVerifyAssertionResponseClass = Class of TVerifyAssertionResponse;
  
  { --------------------------------------------------------------------
    TVerifyCustomTokenResponse
    --------------------------------------------------------------------}
  
  TVerifyCustomTokenResponse = Class(TGoogleBaseObject)
  Private
    FexpiresIn : String;
    FidToken : String;
    Fkind : String;
    FrefreshToken : String;
  Protected
    //Property setters
    Procedure SetexpiresIn(AIndex : Integer; const AValue : String); virtual;
    Procedure SetidToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrefreshToken(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property expiresIn : String Index 0 Read FexpiresIn Write SetexpiresIn;
    Property idToken : String Index 8 Read FidToken Write SetidToken;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property refreshToken : String Index 24 Read FrefreshToken Write SetrefreshToken;
  end;
  TVerifyCustomTokenResponseClass = Class of TVerifyCustomTokenResponse;
  
  { --------------------------------------------------------------------
    TVerifyPasswordResponse
    --------------------------------------------------------------------}
  
  TVerifyPasswordResponse = Class(TGoogleBaseObject)
  Private
    FdisplayName : String;
    Femail : String;
    FexpiresIn : String;
    FidToken : String;
    Fkind : String;
    FlocalId : String;
    FoauthAccessToken : String;
    FoauthAuthorizationCode : String;
    FoauthExpireIn : integer;
    FphotoUrl : String;
    FrefreshToken : String;
    Fregistered : boolean;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetexpiresIn(AIndex : Integer; const AValue : String); virtual;
    Procedure SetidToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlocalId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoauthAccessToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoauthAuthorizationCode(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoauthExpireIn(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetphotoUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrefreshToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setregistered(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property displayName : String Index 0 Read FdisplayName Write SetdisplayName;
    Property email : String Index 8 Read Femail Write Setemail;
    Property expiresIn : String Index 16 Read FexpiresIn Write SetexpiresIn;
    Property idToken : String Index 24 Read FidToken Write SetidToken;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property localId : String Index 40 Read FlocalId Write SetlocalId;
    Property oauthAccessToken : String Index 48 Read FoauthAccessToken Write SetoauthAccessToken;
    Property oauthAuthorizationCode : String Index 56 Read FoauthAuthorizationCode Write SetoauthAuthorizationCode;
    Property oauthExpireIn : integer Index 64 Read FoauthExpireIn Write SetoauthExpireIn;
    Property photoUrl : String Index 72 Read FphotoUrl Write SetphotoUrl;
    Property refreshToken : String Index 80 Read FrefreshToken Write SetrefreshToken;
    Property registered : boolean Index 88 Read Fregistered Write Setregistered;
  end;
  TVerifyPasswordResponseClass = Class of TVerifyPasswordResponse;
  
  { --------------------------------------------------------------------
    TRelyingpartyResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TRelyingpartyResource, method GetProjectConfig
  
  TRelyingpartyGetProjectConfigOptions = Record
    delegatedProjectNumber : String;
    projectNumber : String;
  end;
  
  TRelyingpartyResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateAuthUri(aIdentitytoolkitRelyingpartyCreateAuthUriRequest : TIdentitytoolkitRelyingpartyCreateAuthUriRequest) : TCreateAuthUriResponse;
    Function DeleteAccount(aIdentitytoolkitRelyingpartyDeleteAccountRequest : TIdentitytoolkitRelyingpartyDeleteAccountRequest) : TDeleteAccountResponse;
    Function DownloadAccount(aIdentitytoolkitRelyingpartyDownloadAccountRequest : TIdentitytoolkitRelyingpartyDownloadAccountRequest) : TDownloadAccountResponse;
    Function GetAccountInfo(aIdentitytoolkitRelyingpartyGetAccountInfoRequest : TIdentitytoolkitRelyingpartyGetAccountInfoRequest) : TGetAccountInfoResponse;
    Function GetOobConfirmationCode(aRelyingparty : TRelyingparty) : TGetOobConfirmationCodeResponse;
    Function GetProjectConfig(AQuery : string  = '') : TIdentitytoolkitRelyingpartyGetProjectConfigResponse;
    Function GetProjectConfig(AQuery : TRelyingpartygetProjectConfigOptions) : TIdentitytoolkitRelyingpartyGetProjectConfigResponse;
    Function GetPublicKeys : TIdentitytoolkitRelyingpartyGetPublicKeysResponse;
    Function GetRecaptchaParam : TGetRecaptchaParamResponse;
    Function ResetPassword(aIdentitytoolkitRelyingpartyResetPasswordRequest : TIdentitytoolkitRelyingpartyResetPasswordRequest) : TResetPasswordResponse;
    Function SetAccountInfo(aIdentitytoolkitRelyingpartySetAccountInfoRequest : TIdentitytoolkitRelyingpartySetAccountInfoRequest) : TSetAccountInfoResponse;
    Function SetProjectConfig(aIdentitytoolkitRelyingpartySetProjectConfigRequest : TIdentitytoolkitRelyingpartySetProjectConfigRequest) : TIdentitytoolkitRelyingpartySetProjectConfigResponse;
    Function SignOutUser(aIdentitytoolkitRelyingpartySignOutUserRequest : TIdentitytoolkitRelyingpartySignOutUserRequest) : TIdentitytoolkitRelyingpartySignOutUserResponse;
    Function SignupNewUser(aIdentitytoolkitRelyingpartySignupNewUserRequest : TIdentitytoolkitRelyingpartySignupNewUserRequest) : TSignupNewUserResponse;
    Function UploadAccount(aIdentitytoolkitRelyingpartyUploadAccountRequest : TIdentitytoolkitRelyingpartyUploadAccountRequest) : TUploadAccountResponse;
    Function VerifyAssertion(aIdentitytoolkitRelyingpartyVerifyAssertionRequest : TIdentitytoolkitRelyingpartyVerifyAssertionRequest) : TVerifyAssertionResponse;
    Function VerifyCustomToken(aIdentitytoolkitRelyingpartyVerifyCustomTokenRequest : TIdentitytoolkitRelyingpartyVerifyCustomTokenRequest) : TVerifyCustomTokenResponse;
    Function VerifyPassword(aIdentitytoolkitRelyingpartyVerifyPasswordRequest : TIdentitytoolkitRelyingpartyVerifyPasswordRequest) : TVerifyPasswordResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TIdentitytoolkitAPI
    --------------------------------------------------------------------}
  
  TIdentitytoolkitAPI = Class(TGoogleAPI)
  Private
    FRelyingpartyInstance : TRelyingpartyResource;
    Function GetRelyingpartyInstance : TRelyingpartyResource;virtual;
  Public
    //Override class functions with API info
    Class Function APIName : String; override;
    Class Function APIVersion : String; override;
    Class Function APIRevision : String; override;
    Class Function APIID : String; override;
    Class Function APITitle : String; override;
    Class Function APIDescription : String; override;
    Class Function APIOwnerDomain : String; override;
    Class Function APIOwnerName : String; override;
    Class Function APIIcon16 : String; override;
    Class Function APIIcon32 : String; override;
    Class Function APIdocumentationLink : String; override;
    Class Function APIrootUrl : string; override;
    Class Function APIbasePath : string;override;
    Class Function APIbaseURL : String;override;
    Class Function APIProtocol : string;override;
    Class Function APIservicePath : string;override;
    Class Function APIbatchPath : String;override;
    Class Function APIAuthScopes : TScopeInfoArray;override;
    Class Function APINeedsAuth : Boolean;override;
    Class Procedure RegisterAPIResources; override;
    //Add create function for resources
    Function CreateRelyingpartyResource(AOwner : TComponent) : TRelyingpartyResource;virtual;overload;
    Function CreateRelyingpartyResource : TRelyingpartyResource;virtual;overload;
    //Add default on-demand instances for resources
    Property RelyingpartyResource : TRelyingpartyResource Read GetRelyingpartyInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TCreateAuthUriResponse
  --------------------------------------------------------------------}


Procedure TCreateAuthUriResponse.SetallProviders(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FallProviders=AValue) then exit;
  FallProviders:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateAuthUriResponse.SetauthUri(AIndex : Integer; const AValue : String); 

begin
  If (FauthUri=AValue) then exit;
  FauthUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateAuthUriResponse.SetcaptchaRequired(AIndex : Integer; const AValue : boolean); 

begin
  If (FcaptchaRequired=AValue) then exit;
  FcaptchaRequired:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateAuthUriResponse.SetforExistingProvider(AIndex : Integer; const AValue : boolean); 

begin
  If (FforExistingProvider=AValue) then exit;
  FforExistingProvider:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateAuthUriResponse.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateAuthUriResponse.SetproviderId(AIndex : Integer; const AValue : String); 

begin
  If (FproviderId=AValue) then exit;
  FproviderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateAuthUriResponse.Setregistered(AIndex : Integer; const AValue : boolean); 

begin
  If (Fregistered=AValue) then exit;
  Fregistered:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateAuthUriResponse.SetsessionId(AIndex : Integer; const AValue : String); 

begin
  If (FsessionId=AValue) then exit;
  FsessionId:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCreateAuthUriResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'allproviders' : SetLength(FallProviders,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDeleteAccountResponse
  --------------------------------------------------------------------}


Procedure TDeleteAccountResponse.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDownloadAccountResponse
  --------------------------------------------------------------------}


Procedure TDownloadAccountResponse.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccountResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccountResponse.Setusers(AIndex : Integer; const AValue : TDownloadAccountResponseTypeusersArray); 

begin
  If (Fusers=AValue) then exit;
  Fusers:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDownloadAccountResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'users' : SetLength(Fusers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEmailTemplate
  --------------------------------------------------------------------}


Procedure TEmailTemplate.Setbody(AIndex : Integer; const AValue : String); 

begin
  If (Fbody=AValue) then exit;
  Fbody:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEmailTemplate.Setformat(AIndex : Integer; const AValue : String); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEmailTemplate.Setfrom(AIndex : Integer; const AValue : String); 

begin
  If (Ffrom=AValue) then exit;
  Ffrom:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEmailTemplate.SetfromDisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FfromDisplayName=AValue) then exit;
  FfromDisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEmailTemplate.SetreplyTo(AIndex : Integer; const AValue : String); 

begin
  If (FreplyTo=AValue) then exit;
  FreplyTo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEmailTemplate.Setsubject(AIndex : Integer; const AValue : String); 

begin
  If (Fsubject=AValue) then exit;
  Fsubject:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGetAccountInfoResponse
  --------------------------------------------------------------------}


Procedure TGetAccountInfoResponse.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetAccountInfoResponse.Setusers(AIndex : Integer; const AValue : TGetAccountInfoResponseTypeusersArray); 

begin
  If (Fusers=AValue) then exit;
  Fusers:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGetAccountInfoResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'users' : SetLength(Fusers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGetOobConfirmationCodeResponse
  --------------------------------------------------------------------}


Procedure TGetOobConfirmationCodeResponse.Setemail(AIndex : Integer; const AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetOobConfirmationCodeResponse.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetOobConfirmationCodeResponse.SetoobCode(AIndex : Integer; const AValue : String); 

begin
  If (FoobCode=AValue) then exit;
  FoobCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGetRecaptchaParamResponse
  --------------------------------------------------------------------}


Procedure TGetRecaptchaParamResponse.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetRecaptchaParamResponse.SetrecaptchaSiteKey(AIndex : Integer; const AValue : String); 

begin
  If (FrecaptchaSiteKey=AValue) then exit;
  FrecaptchaSiteKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetRecaptchaParamResponse.SetrecaptchaStoken(AIndex : Integer; const AValue : String); 

begin
  If (FrecaptchaStoken=AValue) then exit;
  FrecaptchaStoken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyCreateAuthUriRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetappId(AIndex : Integer; const AValue : String); 

begin
  If (FappId=AValue) then exit;
  FappId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetclientId(AIndex : Integer; const AValue : String); 

begin
  If (FclientId=AValue) then exit;
  FclientId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.Setcontext(AIndex : Integer; const AValue : String); 

begin
  If (Fcontext=AValue) then exit;
  Fcontext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetcontinueUri(AIndex : Integer; const AValue : String); 

begin
  If (FcontinueUri=AValue) then exit;
  FcontinueUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.Setidentifier(AIndex : Integer; const AValue : String); 

begin
  If (Fidentifier=AValue) then exit;
  Fidentifier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetoauthConsumerKey(AIndex : Integer; const AValue : String); 

begin
  If (FoauthConsumerKey=AValue) then exit;
  FoauthConsumerKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetoauthScope(AIndex : Integer; const AValue : String); 

begin
  If (FoauthScope=AValue) then exit;
  FoauthScope:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetopenidRealm(AIndex : Integer; const AValue : String); 

begin
  If (FopenidRealm=AValue) then exit;
  FopenidRealm:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetotaApp(AIndex : Integer; const AValue : String); 

begin
  If (FotaApp=AValue) then exit;
  FotaApp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetproviderId(AIndex : Integer; const AValue : String); 

begin
  If (FproviderId=AValue) then exit;
  FproviderId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyDeleteAccountRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartyDeleteAccountRequest.SetdelegatedProjectNumber(AIndex : Integer; const AValue : String); 

begin
  If (FdelegatedProjectNumber=AValue) then exit;
  FdelegatedProjectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyDeleteAccountRequest.SetidToken(AIndex : Integer; const AValue : String); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyDeleteAccountRequest.SetlocalId(AIndex : Integer; const AValue : String); 

begin
  If (FlocalId=AValue) then exit;
  FlocalId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyDownloadAccountRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartyDownloadAccountRequest.SetdelegatedProjectNumber(AIndex : Integer; const AValue : String); 

begin
  If (FdelegatedProjectNumber=AValue) then exit;
  FdelegatedProjectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyDownloadAccountRequest.SetmaxResults(AIndex : Integer; const AValue : integer); 

begin
  If (FmaxResults=AValue) then exit;
  FmaxResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyDownloadAccountRequest.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyGetAccountInfoRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartyGetAccountInfoRequest.SetdelegatedProjectNumber(AIndex : Integer; const AValue : String); 

begin
  If (FdelegatedProjectNumber=AValue) then exit;
  FdelegatedProjectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyGetAccountInfoRequest.Setemail(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyGetAccountInfoRequest.SetidToken(AIndex : Integer; const AValue : String); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyGetAccountInfoRequest.SetlocalId(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FlocalId=AValue) then exit;
  FlocalId:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TIdentitytoolkitRelyingpartyGetAccountInfoRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'email' : SetLength(Femail,ALength);
  'localid' : SetLength(FlocalId,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyGetProjectConfigResponse
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartyGetProjectConfigResponse.SetallowPasswordUser(AIndex : Integer; const AValue : boolean); 

begin
  If (FallowPasswordUser=AValue) then exit;
  FallowPasswordUser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyGetProjectConfigResponse.SetapiKey(AIndex : Integer; const AValue : String); 

begin
  If (FapiKey=AValue) then exit;
  FapiKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyGetProjectConfigResponse.SetauthorizedDomains(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FauthorizedDomains=AValue) then exit;
  FauthorizedDomains:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyGetProjectConfigResponse.SetchangeEmailTemplate(AIndex : Integer; const AValue : TEmailTemplate); 

begin
  If (FchangeEmailTemplate=AValue) then exit;
  FchangeEmailTemplate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyGetProjectConfigResponse.SetenableAnonymousUser(AIndex : Integer; const AValue : boolean); 

begin
  If (FenableAnonymousUser=AValue) then exit;
  FenableAnonymousUser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyGetProjectConfigResponse.SetidpConfig(AIndex : Integer; const AValue : TIdentitytoolkitRelyingpartyGetProjectConfigResponseTypeidpConfigArray); 

begin
  If (FidpConfig=AValue) then exit;
  FidpConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyGetProjectConfigResponse.SetlegacyResetPasswordTemplate(AIndex : Integer; const AValue : TEmailTemplate); 

begin
  If (FlegacyResetPasswordTemplate=AValue) then exit;
  FlegacyResetPasswordTemplate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyGetProjectConfigResponse.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyGetProjectConfigResponse.SetresetPasswordTemplate(AIndex : Integer; const AValue : TEmailTemplate); 

begin
  If (FresetPasswordTemplate=AValue) then exit;
  FresetPasswordTemplate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyGetProjectConfigResponse.SetuseEmailSending(AIndex : Integer; const AValue : boolean); 

begin
  If (FuseEmailSending=AValue) then exit;
  FuseEmailSending:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyGetProjectConfigResponse.SetverifyEmailTemplate(AIndex : Integer; const AValue : TEmailTemplate); 

begin
  If (FverifyEmailTemplate=AValue) then exit;
  FverifyEmailTemplate:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TIdentitytoolkitRelyingpartyGetProjectConfigResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'authorizeddomains' : SetLength(FauthorizedDomains,ALength);
  'idpconfig' : SetLength(FidpConfig,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyGetPublicKeysResponse
  --------------------------------------------------------------------}


Class Function TIdentitytoolkitRelyingpartyGetPublicKeysResponse.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyResetPasswordRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartyResetPasswordRequest.Setemail(AIndex : Integer; const AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyResetPasswordRequest.SetnewPassword(AIndex : Integer; const AValue : String); 

begin
  If (FnewPassword=AValue) then exit;
  FnewPassword:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyResetPasswordRequest.SetoldPassword(AIndex : Integer; const AValue : String); 

begin
  If (FoldPassword=AValue) then exit;
  FoldPassword:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyResetPasswordRequest.SetoobCode(AIndex : Integer; const AValue : String); 

begin
  If (FoobCode=AValue) then exit;
  FoobCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartySetAccountInfoRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetcaptchaChallenge(AIndex : Integer; const AValue : String); 

begin
  If (FcaptchaChallenge=AValue) then exit;
  FcaptchaChallenge:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetcaptchaResponse(AIndex : Integer; const AValue : String); 

begin
  If (FcaptchaResponse=AValue) then exit;
  FcaptchaResponse:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetdelegatedProjectNumber(AIndex : Integer; const AValue : String); 

begin
  If (FdelegatedProjectNumber=AValue) then exit;
  FdelegatedProjectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetdeleteAttribute(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FdeleteAttribute=AValue) then exit;
  FdeleteAttribute:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetdeleteProvider(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FdeleteProvider=AValue) then exit;
  FdeleteProvider:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetdisableUser(AIndex : Integer; const AValue : boolean); 

begin
  If (FdisableUser=AValue) then exit;
  FdisableUser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.Setemail(AIndex : Integer; const AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetemailVerified(AIndex : Integer; const AValue : boolean); 

begin
  If (FemailVerified=AValue) then exit;
  FemailVerified:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetidToken(AIndex : Integer; const AValue : String); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetinstanceId(AIndex : Integer; const AValue : String); 

begin
  If (FinstanceId=AValue) then exit;
  FinstanceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetlocalId(AIndex : Integer; const AValue : String); 

begin
  If (FlocalId=AValue) then exit;
  FlocalId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetoobCode(AIndex : Integer; const AValue : String); 

begin
  If (FoobCode=AValue) then exit;
  FoobCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.Setpassword(AIndex : Integer; const AValue : String); 

begin
  If (Fpassword=AValue) then exit;
  Fpassword:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetphotoUrl(AIndex : Integer; const AValue : String); 

begin
  If (FphotoUrl=AValue) then exit;
  FphotoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.Setprovider(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fprovider=AValue) then exit;
  Fprovider:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetreturnSecureToken(AIndex : Integer; const AValue : boolean); 

begin
  If (FreturnSecureToken=AValue) then exit;
  FreturnSecureToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetupgradeToFederatedLogin(AIndex : Integer; const AValue : boolean); 

begin
  If (FupgradeToFederatedLogin=AValue) then exit;
  FupgradeToFederatedLogin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetvalidSince(AIndex : Integer; const AValue : String); 

begin
  If (FvalidSince=AValue) then exit;
  FvalidSince:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'deleteattribute' : SetLength(FdeleteAttribute,ALength);
  'deleteprovider' : SetLength(FdeleteProvider,ALength);
  'provider' : SetLength(Fprovider,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartySetProjectConfigRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartySetProjectConfigRequest.SetallowPasswordUser(AIndex : Integer; const AValue : boolean); 

begin
  If (FallowPasswordUser=AValue) then exit;
  FallowPasswordUser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetProjectConfigRequest.SetapiKey(AIndex : Integer; const AValue : String); 

begin
  If (FapiKey=AValue) then exit;
  FapiKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetProjectConfigRequest.SetauthorizedDomains(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FauthorizedDomains=AValue) then exit;
  FauthorizedDomains:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetProjectConfigRequest.SetchangeEmailTemplate(AIndex : Integer; const AValue : TEmailTemplate); 

begin
  If (FchangeEmailTemplate=AValue) then exit;
  FchangeEmailTemplate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetProjectConfigRequest.SetdelegatedProjectNumber(AIndex : Integer; const AValue : String); 

begin
  If (FdelegatedProjectNumber=AValue) then exit;
  FdelegatedProjectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetProjectConfigRequest.SetenableAnonymousUser(AIndex : Integer; const AValue : boolean); 

begin
  If (FenableAnonymousUser=AValue) then exit;
  FenableAnonymousUser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetProjectConfigRequest.SetidpConfig(AIndex : Integer; const AValue : TIdentitytoolkitRelyingpartySetProjectConfigRequestTypeidpConfigArray); 

begin
  If (FidpConfig=AValue) then exit;
  FidpConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetProjectConfigRequest.SetlegacyResetPasswordTemplate(AIndex : Integer; const AValue : TEmailTemplate); 

begin
  If (FlegacyResetPasswordTemplate=AValue) then exit;
  FlegacyResetPasswordTemplate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetProjectConfigRequest.SetresetPasswordTemplate(AIndex : Integer; const AValue : TEmailTemplate); 

begin
  If (FresetPasswordTemplate=AValue) then exit;
  FresetPasswordTemplate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetProjectConfigRequest.SetuseEmailSending(AIndex : Integer; const AValue : boolean); 

begin
  If (FuseEmailSending=AValue) then exit;
  FuseEmailSending:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetProjectConfigRequest.SetverifyEmailTemplate(AIndex : Integer; const AValue : TEmailTemplate); 

begin
  If (FverifyEmailTemplate=AValue) then exit;
  FverifyEmailTemplate:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TIdentitytoolkitRelyingpartySetProjectConfigRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'authorizeddomains' : SetLength(FauthorizedDomains,ALength);
  'idpconfig' : SetLength(FidpConfig,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartySetProjectConfigResponse
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartySetProjectConfigResponse.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartySignOutUserRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartySignOutUserRequest.SetinstanceId(AIndex : Integer; const AValue : String); 

begin
  If (FinstanceId=AValue) then exit;
  FinstanceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySignOutUserRequest.SetlocalId(AIndex : Integer; const AValue : String); 

begin
  If (FlocalId=AValue) then exit;
  FlocalId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartySignOutUserResponse
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartySignOutUserResponse.SetlocalId(AIndex : Integer; const AValue : String); 

begin
  If (FlocalId=AValue) then exit;
  FlocalId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartySignupNewUserRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartySignupNewUserRequest.SetcaptchaChallenge(AIndex : Integer; const AValue : String); 

begin
  If (FcaptchaChallenge=AValue) then exit;
  FcaptchaChallenge:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySignupNewUserRequest.SetcaptchaResponse(AIndex : Integer; const AValue : String); 

begin
  If (FcaptchaResponse=AValue) then exit;
  FcaptchaResponse:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySignupNewUserRequest.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySignupNewUserRequest.Setemail(AIndex : Integer; const AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySignupNewUserRequest.SetidToken(AIndex : Integer; const AValue : String); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySignupNewUserRequest.SetinstanceId(AIndex : Integer; const AValue : String); 

begin
  If (FinstanceId=AValue) then exit;
  FinstanceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySignupNewUserRequest.Setpassword(AIndex : Integer; const AValue : String); 

begin
  If (Fpassword=AValue) then exit;
  Fpassword:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyUploadAccountRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartyUploadAccountRequest.SetdelegatedProjectNumber(AIndex : Integer; const AValue : String); 

begin
  If (FdelegatedProjectNumber=AValue) then exit;
  FdelegatedProjectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyUploadAccountRequest.SethashAlgorithm(AIndex : Integer; const AValue : String); 

begin
  If (FhashAlgorithm=AValue) then exit;
  FhashAlgorithm:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyUploadAccountRequest.SetmemoryCost(AIndex : Integer; const AValue : integer); 

begin
  If (FmemoryCost=AValue) then exit;
  FmemoryCost:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyUploadAccountRequest.Setrounds(AIndex : Integer; const AValue : integer); 

begin
  If (Frounds=AValue) then exit;
  Frounds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyUploadAccountRequest.SetsaltSeparator(AIndex : Integer; const AValue : String); 

begin
  If (FsaltSeparator=AValue) then exit;
  FsaltSeparator:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyUploadAccountRequest.SetsignerKey(AIndex : Integer; const AValue : String); 

begin
  If (FsignerKey=AValue) then exit;
  FsignerKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyUploadAccountRequest.Setusers(AIndex : Integer; const AValue : TIdentitytoolkitRelyingpartyUploadAccountRequestTypeusersArray); 

begin
  If (Fusers=AValue) then exit;
  Fusers:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TIdentitytoolkitRelyingpartyUploadAccountRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'users' : SetLength(Fusers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyVerifyAssertionRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartyVerifyAssertionRequest.SetdelegatedProjectNumber(AIndex : Integer; const AValue : String); 

begin
  If (FdelegatedProjectNumber=AValue) then exit;
  FdelegatedProjectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyAssertionRequest.SetidToken(AIndex : Integer; const AValue : String); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyAssertionRequest.SetinstanceId(AIndex : Integer; const AValue : String); 

begin
  If (FinstanceId=AValue) then exit;
  FinstanceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyAssertionRequest.SetpendingIdToken(AIndex : Integer; const AValue : String); 

begin
  If (FpendingIdToken=AValue) then exit;
  FpendingIdToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyAssertionRequest.SetpostBody(AIndex : Integer; const AValue : String); 

begin
  If (FpostBody=AValue) then exit;
  FpostBody:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyAssertionRequest.SetrequestUri(AIndex : Integer; const AValue : String); 

begin
  If (FrequestUri=AValue) then exit;
  FrequestUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyAssertionRequest.SetreturnRefreshToken(AIndex : Integer; const AValue : boolean); 

begin
  If (FreturnRefreshToken=AValue) then exit;
  FreturnRefreshToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyAssertionRequest.SetreturnSecureToken(AIndex : Integer; const AValue : boolean); 

begin
  If (FreturnSecureToken=AValue) then exit;
  FreturnSecureToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyAssertionRequest.SetsessionId(AIndex : Integer; const AValue : String); 

begin
  If (FsessionId=AValue) then exit;
  FsessionId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyVerifyCustomTokenRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartyVerifyCustomTokenRequest.SetdelegatedProjectNumber(AIndex : Integer; const AValue : String); 

begin
  If (FdelegatedProjectNumber=AValue) then exit;
  FdelegatedProjectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyCustomTokenRequest.SetinstanceId(AIndex : Integer; const AValue : String); 

begin
  If (FinstanceId=AValue) then exit;
  FinstanceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyCustomTokenRequest.SetreturnSecureToken(AIndex : Integer; const AValue : boolean); 

begin
  If (FreturnSecureToken=AValue) then exit;
  FreturnSecureToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyCustomTokenRequest.Settoken(AIndex : Integer; const AValue : String); 

begin
  If (Ftoken=AValue) then exit;
  Ftoken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyVerifyPasswordRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartyVerifyPasswordRequest.SetcaptchaChallenge(AIndex : Integer; const AValue : String); 

begin
  If (FcaptchaChallenge=AValue) then exit;
  FcaptchaChallenge:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyPasswordRequest.SetcaptchaResponse(AIndex : Integer; const AValue : String); 

begin
  If (FcaptchaResponse=AValue) then exit;
  FcaptchaResponse:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyPasswordRequest.SetdelegatedProjectNumber(AIndex : Integer; const AValue : String); 

begin
  If (FdelegatedProjectNumber=AValue) then exit;
  FdelegatedProjectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyPasswordRequest.Setemail(AIndex : Integer; const AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyPasswordRequest.SetidToken(AIndex : Integer; const AValue : String); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyPasswordRequest.SetinstanceId(AIndex : Integer; const AValue : String); 

begin
  If (FinstanceId=AValue) then exit;
  FinstanceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyPasswordRequest.Setpassword(AIndex : Integer; const AValue : String); 

begin
  If (Fpassword=AValue) then exit;
  Fpassword:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyPasswordRequest.SetpendingIdToken(AIndex : Integer; const AValue : String); 

begin
  If (FpendingIdToken=AValue) then exit;
  FpendingIdToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyPasswordRequest.SetreturnSecureToken(AIndex : Integer; const AValue : boolean); 

begin
  If (FreturnSecureToken=AValue) then exit;
  FreturnSecureToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdpConfig
  --------------------------------------------------------------------}


Procedure TIdpConfig.SetclientId(AIndex : Integer; const AValue : String); 

begin
  If (FclientId=AValue) then exit;
  FclientId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdpConfig.Setenabled(AIndex : Integer; const AValue : boolean); 

begin
  If (Fenabled=AValue) then exit;
  Fenabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdpConfig.SetexperimentPercent(AIndex : Integer; const AValue : integer); 

begin
  If (FexperimentPercent=AValue) then exit;
  FexperimentPercent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdpConfig.Setprovider(AIndex : Integer; const AValue : String); 

begin
  If (Fprovider=AValue) then exit;
  Fprovider:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdpConfig.Setsecret(AIndex : Integer; const AValue : String); 

begin
  If (Fsecret=AValue) then exit;
  Fsecret:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRelyingparty
  --------------------------------------------------------------------}


Procedure TRelyingparty.SetcaptchaResp(AIndex : Integer; const AValue : String); 

begin
  If (FcaptchaResp=AValue) then exit;
  FcaptchaResp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelyingparty.Setchallenge(AIndex : Integer; const AValue : String); 

begin
  If (Fchallenge=AValue) then exit;
  Fchallenge:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelyingparty.Setemail(AIndex : Integer; const AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelyingparty.SetidToken(AIndex : Integer; const AValue : String); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelyingparty.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelyingparty.SetnewEmail(AIndex : Integer; const AValue : String); 

begin
  If (FnewEmail=AValue) then exit;
  FnewEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelyingparty.SetrequestType(AIndex : Integer; const AValue : String); 

begin
  If (FrequestType=AValue) then exit;
  FrequestType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelyingparty.SetuserIp(AIndex : Integer; const AValue : String); 

begin
  If (FuserIp=AValue) then exit;
  FuserIp:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResetPasswordResponse
  --------------------------------------------------------------------}


Procedure TResetPasswordResponse.Setemail(AIndex : Integer; const AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResetPasswordResponse.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSetAccountInfoResponseTypeproviderUserInfoItem
  --------------------------------------------------------------------}


Procedure TSetAccountInfoResponseTypeproviderUserInfoItem.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponseTypeproviderUserInfoItem.SetfederatedId(AIndex : Integer; const AValue : String); 

begin
  If (FfederatedId=AValue) then exit;
  FfederatedId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponseTypeproviderUserInfoItem.SetphotoUrl(AIndex : Integer; const AValue : String); 

begin
  If (FphotoUrl=AValue) then exit;
  FphotoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponseTypeproviderUserInfoItem.SetproviderId(AIndex : Integer; const AValue : String); 

begin
  If (FproviderId=AValue) then exit;
  FproviderId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSetAccountInfoResponse
  --------------------------------------------------------------------}


Procedure TSetAccountInfoResponse.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponse.Setemail(AIndex : Integer; const AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponse.SetexpiresIn(AIndex : Integer; const AValue : String); 

begin
  If (FexpiresIn=AValue) then exit;
  FexpiresIn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponse.SetidToken(AIndex : Integer; const AValue : String); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponse.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponse.SetlocalId(AIndex : Integer; const AValue : String); 

begin
  If (FlocalId=AValue) then exit;
  FlocalId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponse.SetnewEmail(AIndex : Integer; const AValue : String); 

begin
  If (FnewEmail=AValue) then exit;
  FnewEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponse.SetpasswordHash(AIndex : Integer; const AValue : String); 

begin
  If (FpasswordHash=AValue) then exit;
  FpasswordHash:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponse.SetphotoUrl(AIndex : Integer; const AValue : String); 

begin
  If (FphotoUrl=AValue) then exit;
  FphotoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponse.SetproviderUserInfo(AIndex : Integer; const AValue : TSetAccountInfoResponseTypeproviderUserInfoArray); 

begin
  If (FproviderUserInfo=AValue) then exit;
  FproviderUserInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponse.SetrefreshToken(AIndex : Integer; const AValue : String); 

begin
  If (FrefreshToken=AValue) then exit;
  FrefreshToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSetAccountInfoResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'provideruserinfo' : SetLength(FproviderUserInfo,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSignupNewUserResponse
  --------------------------------------------------------------------}


Procedure TSignupNewUserResponse.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSignupNewUserResponse.Setemail(AIndex : Integer; const AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSignupNewUserResponse.SetexpiresIn(AIndex : Integer; const AValue : String); 

begin
  If (FexpiresIn=AValue) then exit;
  FexpiresIn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSignupNewUserResponse.SetidToken(AIndex : Integer; const AValue : String); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSignupNewUserResponse.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSignupNewUserResponse.SetlocalId(AIndex : Integer; const AValue : String); 

begin
  If (FlocalId=AValue) then exit;
  FlocalId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSignupNewUserResponse.SetrefreshToken(AIndex : Integer; const AValue : String); 

begin
  If (FrefreshToken=AValue) then exit;
  FrefreshToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUploadAccountResponseTypeerrorItem
  --------------------------------------------------------------------}


Procedure TUploadAccountResponseTypeerrorItem.Setindex(AIndex : Integer; const AValue : integer); 

begin
  If (Findex=AValue) then exit;
  Findex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUploadAccountResponseTypeerrorItem.Setmessage(AIndex : Integer; const AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUploadAccountResponse
  --------------------------------------------------------------------}


Procedure TUploadAccountResponse.Seterror(AIndex : Integer; const AValue : TUploadAccountResponseTypeerrorArray); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUploadAccountResponse.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TUploadAccountResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'error' : SetLength(Ferror,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUserInfoTypeproviderUserInfoItem
  --------------------------------------------------------------------}


Procedure TUserInfoTypeproviderUserInfoItem.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfoTypeproviderUserInfoItem.Setemail(AIndex : Integer; const AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfoTypeproviderUserInfoItem.SetfederatedId(AIndex : Integer; const AValue : String); 

begin
  If (FfederatedId=AValue) then exit;
  FfederatedId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfoTypeproviderUserInfoItem.SetphotoUrl(AIndex : Integer; const AValue : String); 

begin
  If (FphotoUrl=AValue) then exit;
  FphotoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfoTypeproviderUserInfoItem.SetproviderId(AIndex : Integer; const AValue : String); 

begin
  If (FproviderId=AValue) then exit;
  FproviderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfoTypeproviderUserInfoItem.SetrawId(AIndex : Integer; const AValue : String); 

begin
  If (FrawId=AValue) then exit;
  FrawId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserInfo
  --------------------------------------------------------------------}


Procedure TUserInfo.Setdisabled(AIndex : Integer; const AValue : boolean); 

begin
  If (Fdisabled=AValue) then exit;
  Fdisabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.Setemail(AIndex : Integer; const AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.SetemailVerified(AIndex : Integer; const AValue : boolean); 

begin
  If (FemailVerified=AValue) then exit;
  FemailVerified:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.SetlocalId(AIndex : Integer; const AValue : String); 

begin
  If (FlocalId=AValue) then exit;
  FlocalId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.SetpasswordHash(AIndex : Integer; const AValue : String); 

begin
  If (FpasswordHash=AValue) then exit;
  FpasswordHash:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.SetpasswordUpdatedAt(AIndex : Integer; const AValue : double); 

begin
  If (FpasswordUpdatedAt=AValue) then exit;
  FpasswordUpdatedAt:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.SetphotoUrl(AIndex : Integer; const AValue : String); 

begin
  If (FphotoUrl=AValue) then exit;
  FphotoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.SetproviderUserInfo(AIndex : Integer; const AValue : TUserInfoTypeproviderUserInfoArray); 

begin
  If (FproviderUserInfo=AValue) then exit;
  FproviderUserInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.Setsalt(AIndex : Integer; const AValue : String); 

begin
  If (Fsalt=AValue) then exit;
  Fsalt:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.SetvalidSince(AIndex : Integer; const AValue : String); 

begin
  If (FvalidSince=AValue) then exit;
  FvalidSince:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.Setversion(AIndex : Integer; const AValue : integer); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TUserInfo.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'provideruserinfo' : SetLength(FproviderUserInfo,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TVerifyAssertionResponse
  --------------------------------------------------------------------}


Procedure TVerifyAssertionResponse.Setaction(AIndex : Integer; const AValue : String); 

begin
  If (Faction=AValue) then exit;
  Faction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetappInstallationUrl(AIndex : Integer; const AValue : String); 

begin
  If (FappInstallationUrl=AValue) then exit;
  FappInstallationUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetappScheme(AIndex : Integer; const AValue : String); 

begin
  If (FappScheme=AValue) then exit;
  FappScheme:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.Setcontext(AIndex : Integer; const AValue : String); 

begin
  If (Fcontext=AValue) then exit;
  Fcontext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetdateOfBirth(AIndex : Integer; const AValue : String); 

begin
  If (FdateOfBirth=AValue) then exit;
  FdateOfBirth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.Setemail(AIndex : Integer; const AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetemailRecycled(AIndex : Integer; const AValue : boolean); 

begin
  If (FemailRecycled=AValue) then exit;
  FemailRecycled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetemailVerified(AIndex : Integer; const AValue : boolean); 

begin
  If (FemailVerified=AValue) then exit;
  FemailVerified:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetexpiresIn(AIndex : Integer; const AValue : String); 

begin
  If (FexpiresIn=AValue) then exit;
  FexpiresIn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetfederatedId(AIndex : Integer; const AValue : String); 

begin
  If (FfederatedId=AValue) then exit;
  FfederatedId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetfirstName(AIndex : Integer; const AValue : String); 

begin
  If (FfirstName=AValue) then exit;
  FfirstName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetfullName(AIndex : Integer; const AValue : String); 

begin
  If (FfullName=AValue) then exit;
  FfullName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetidToken(AIndex : Integer; const AValue : String); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetinputEmail(AIndex : Integer; const AValue : String); 

begin
  If (FinputEmail=AValue) then exit;
  FinputEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.Setlanguage(AIndex : Integer; const AValue : String); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetlastName(AIndex : Integer; const AValue : String); 

begin
  If (FlastName=AValue) then exit;
  FlastName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetlocalId(AIndex : Integer; const AValue : String); 

begin
  If (FlocalId=AValue) then exit;
  FlocalId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetneedConfirmation(AIndex : Integer; const AValue : boolean); 

begin
  If (FneedConfirmation=AValue) then exit;
  FneedConfirmation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetneedEmail(AIndex : Integer; const AValue : boolean); 

begin
  If (FneedEmail=AValue) then exit;
  FneedEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetnickName(AIndex : Integer; const AValue : String); 

begin
  If (FnickName=AValue) then exit;
  FnickName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetoauthAccessToken(AIndex : Integer; const AValue : String); 

begin
  If (FoauthAccessToken=AValue) then exit;
  FoauthAccessToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetoauthAuthorizationCode(AIndex : Integer; const AValue : String); 

begin
  If (FoauthAuthorizationCode=AValue) then exit;
  FoauthAuthorizationCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetoauthExpireIn(AIndex : Integer; const AValue : integer); 

begin
  If (FoauthExpireIn=AValue) then exit;
  FoauthExpireIn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetoauthIdToken(AIndex : Integer; const AValue : String); 

begin
  If (FoauthIdToken=AValue) then exit;
  FoauthIdToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetoauthRequestToken(AIndex : Integer; const AValue : String); 

begin
  If (FoauthRequestToken=AValue) then exit;
  FoauthRequestToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetoauthScope(AIndex : Integer; const AValue : String); 

begin
  If (FoauthScope=AValue) then exit;
  FoauthScope:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetoauthTokenSecret(AIndex : Integer; const AValue : String); 

begin
  If (FoauthTokenSecret=AValue) then exit;
  FoauthTokenSecret:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetoriginalEmail(AIndex : Integer; const AValue : String); 

begin
  If (ForiginalEmail=AValue) then exit;
  ForiginalEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetphotoUrl(AIndex : Integer; const AValue : String); 

begin
  If (FphotoUrl=AValue) then exit;
  FphotoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetproviderId(AIndex : Integer; const AValue : String); 

begin
  If (FproviderId=AValue) then exit;
  FproviderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetrefreshToken(AIndex : Integer; const AValue : String); 

begin
  If (FrefreshToken=AValue) then exit;
  FrefreshToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SettimeZone(AIndex : Integer; const AValue : String); 

begin
  If (FtimeZone=AValue) then exit;
  FtimeZone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetverifiedProvider(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FverifiedProvider=AValue) then exit;
  FverifiedProvider:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TVerifyAssertionResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'verifiedprovider' : SetLength(FverifiedProvider,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TVerifyCustomTokenResponse
  --------------------------------------------------------------------}


Procedure TVerifyCustomTokenResponse.SetexpiresIn(AIndex : Integer; const AValue : String); 

begin
  If (FexpiresIn=AValue) then exit;
  FexpiresIn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyCustomTokenResponse.SetidToken(AIndex : Integer; const AValue : String); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyCustomTokenResponse.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyCustomTokenResponse.SetrefreshToken(AIndex : Integer; const AValue : String); 

begin
  If (FrefreshToken=AValue) then exit;
  FrefreshToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVerifyPasswordResponse
  --------------------------------------------------------------------}


Procedure TVerifyPasswordResponse.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyPasswordResponse.Setemail(AIndex : Integer; const AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyPasswordResponse.SetexpiresIn(AIndex : Integer; const AValue : String); 

begin
  If (FexpiresIn=AValue) then exit;
  FexpiresIn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyPasswordResponse.SetidToken(AIndex : Integer; const AValue : String); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyPasswordResponse.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyPasswordResponse.SetlocalId(AIndex : Integer; const AValue : String); 

begin
  If (FlocalId=AValue) then exit;
  FlocalId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyPasswordResponse.SetoauthAccessToken(AIndex : Integer; const AValue : String); 

begin
  If (FoauthAccessToken=AValue) then exit;
  FoauthAccessToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyPasswordResponse.SetoauthAuthorizationCode(AIndex : Integer; const AValue : String); 

begin
  If (FoauthAuthorizationCode=AValue) then exit;
  FoauthAuthorizationCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyPasswordResponse.SetoauthExpireIn(AIndex : Integer; const AValue : integer); 

begin
  If (FoauthExpireIn=AValue) then exit;
  FoauthExpireIn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyPasswordResponse.SetphotoUrl(AIndex : Integer; const AValue : String); 

begin
  If (FphotoUrl=AValue) then exit;
  FphotoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyPasswordResponse.SetrefreshToken(AIndex : Integer; const AValue : String); 

begin
  If (FrefreshToken=AValue) then exit;
  FrefreshToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyPasswordResponse.Setregistered(AIndex : Integer; const AValue : boolean); 

begin
  If (Fregistered=AValue) then exit;
  Fregistered:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRelyingpartyResource
  --------------------------------------------------------------------}


Class Function TRelyingpartyResource.ResourceName : String;

begin
  Result:='relyingparty';
end;

Class Function TRelyingpartyResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TidentitytoolkitAPI;
end;

Function TRelyingpartyResource.CreateAuthUri(aIdentitytoolkitRelyingpartyCreateAuthUriRequest : TIdentitytoolkitRelyingpartyCreateAuthUriRequest) : TCreateAuthUriResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'createAuthUri';
  _Methodid   = 'identitytoolkit.relyingparty.createAuthUri';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aIdentitytoolkitRelyingpartyCreateAuthUriRequest,TCreateAuthUriResponse) as TCreateAuthUriResponse;
end;

Function TRelyingpartyResource.DeleteAccount(aIdentitytoolkitRelyingpartyDeleteAccountRequest : TIdentitytoolkitRelyingpartyDeleteAccountRequest) : TDeleteAccountResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'deleteAccount';
  _Methodid   = 'identitytoolkit.relyingparty.deleteAccount';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aIdentitytoolkitRelyingpartyDeleteAccountRequest,TDeleteAccountResponse) as TDeleteAccountResponse;
end;

Function TRelyingpartyResource.DownloadAccount(aIdentitytoolkitRelyingpartyDownloadAccountRequest : TIdentitytoolkitRelyingpartyDownloadAccountRequest) : TDownloadAccountResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'downloadAccount';
  _Methodid   = 'identitytoolkit.relyingparty.downloadAccount';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aIdentitytoolkitRelyingpartyDownloadAccountRequest,TDownloadAccountResponse) as TDownloadAccountResponse;
end;

Function TRelyingpartyResource.GetAccountInfo(aIdentitytoolkitRelyingpartyGetAccountInfoRequest : TIdentitytoolkitRelyingpartyGetAccountInfoRequest) : TGetAccountInfoResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'getAccountInfo';
  _Methodid   = 'identitytoolkit.relyingparty.getAccountInfo';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aIdentitytoolkitRelyingpartyGetAccountInfoRequest,TGetAccountInfoResponse) as TGetAccountInfoResponse;
end;

Function TRelyingpartyResource.GetOobConfirmationCode(aRelyingparty : TRelyingparty) : TGetOobConfirmationCodeResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'getOobConfirmationCode';
  _Methodid   = 'identitytoolkit.relyingparty.getOobConfirmationCode';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aRelyingparty,TGetOobConfirmationCodeResponse) as TGetOobConfirmationCodeResponse;
end;

Function TRelyingpartyResource.GetProjectConfig(AQuery : string = '') : TIdentitytoolkitRelyingpartyGetProjectConfigResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'getProjectConfig';
  _Methodid   = 'identitytoolkit.relyingparty.getProjectConfig';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TIdentitytoolkitRelyingpartyGetProjectConfigResponse) as TIdentitytoolkitRelyingpartyGetProjectConfigResponse;
end;


Function TRelyingpartyResource.GetProjectConfig(AQuery : TRelyingpartygetProjectConfigOptions) : TIdentitytoolkitRelyingpartyGetProjectConfigResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'delegatedProjectNumber',AQuery.delegatedProjectNumber);
  AddToQuery(_Q,'projectNumber',AQuery.projectNumber);
  Result:=GetProjectConfig(_Q);
end;

Function TRelyingpartyResource.GetPublicKeys : TIdentitytoolkitRelyingpartyGetPublicKeysResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'publicKeys';
  _Methodid   = 'identitytoolkit.relyingparty.getPublicKeys';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TIdentitytoolkitRelyingpartyGetPublicKeysResponse) as TIdentitytoolkitRelyingpartyGetPublicKeysResponse;
end;

Function TRelyingpartyResource.GetRecaptchaParam : TGetRecaptchaParamResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'getRecaptchaParam';
  _Methodid   = 'identitytoolkit.relyingparty.getRecaptchaParam';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TGetRecaptchaParamResponse) as TGetRecaptchaParamResponse;
end;

Function TRelyingpartyResource.ResetPassword(aIdentitytoolkitRelyingpartyResetPasswordRequest : TIdentitytoolkitRelyingpartyResetPasswordRequest) : TResetPasswordResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'resetPassword';
  _Methodid   = 'identitytoolkit.relyingparty.resetPassword';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aIdentitytoolkitRelyingpartyResetPasswordRequest,TResetPasswordResponse) as TResetPasswordResponse;
end;

Function TRelyingpartyResource.SetAccountInfo(aIdentitytoolkitRelyingpartySetAccountInfoRequest : TIdentitytoolkitRelyingpartySetAccountInfoRequest) : TSetAccountInfoResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'setAccountInfo';
  _Methodid   = 'identitytoolkit.relyingparty.setAccountInfo';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aIdentitytoolkitRelyingpartySetAccountInfoRequest,TSetAccountInfoResponse) as TSetAccountInfoResponse;
end;

Function TRelyingpartyResource.SetProjectConfig(aIdentitytoolkitRelyingpartySetProjectConfigRequest : TIdentitytoolkitRelyingpartySetProjectConfigRequest) : TIdentitytoolkitRelyingpartySetProjectConfigResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'setProjectConfig';
  _Methodid   = 'identitytoolkit.relyingparty.setProjectConfig';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aIdentitytoolkitRelyingpartySetProjectConfigRequest,TIdentitytoolkitRelyingpartySetProjectConfigResponse) as TIdentitytoolkitRelyingpartySetProjectConfigResponse;
end;

Function TRelyingpartyResource.SignOutUser(aIdentitytoolkitRelyingpartySignOutUserRequest : TIdentitytoolkitRelyingpartySignOutUserRequest) : TIdentitytoolkitRelyingpartySignOutUserResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'signOutUser';
  _Methodid   = 'identitytoolkit.relyingparty.signOutUser';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aIdentitytoolkitRelyingpartySignOutUserRequest,TIdentitytoolkitRelyingpartySignOutUserResponse) as TIdentitytoolkitRelyingpartySignOutUserResponse;
end;

Function TRelyingpartyResource.SignupNewUser(aIdentitytoolkitRelyingpartySignupNewUserRequest : TIdentitytoolkitRelyingpartySignupNewUserRequest) : TSignupNewUserResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'signupNewUser';
  _Methodid   = 'identitytoolkit.relyingparty.signupNewUser';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aIdentitytoolkitRelyingpartySignupNewUserRequest,TSignupNewUserResponse) as TSignupNewUserResponse;
end;

Function TRelyingpartyResource.UploadAccount(aIdentitytoolkitRelyingpartyUploadAccountRequest : TIdentitytoolkitRelyingpartyUploadAccountRequest) : TUploadAccountResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'uploadAccount';
  _Methodid   = 'identitytoolkit.relyingparty.uploadAccount';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aIdentitytoolkitRelyingpartyUploadAccountRequest,TUploadAccountResponse) as TUploadAccountResponse;
end;

Function TRelyingpartyResource.VerifyAssertion(aIdentitytoolkitRelyingpartyVerifyAssertionRequest : TIdentitytoolkitRelyingpartyVerifyAssertionRequest) : TVerifyAssertionResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'verifyAssertion';
  _Methodid   = 'identitytoolkit.relyingparty.verifyAssertion';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aIdentitytoolkitRelyingpartyVerifyAssertionRequest,TVerifyAssertionResponse) as TVerifyAssertionResponse;
end;

Function TRelyingpartyResource.VerifyCustomToken(aIdentitytoolkitRelyingpartyVerifyCustomTokenRequest : TIdentitytoolkitRelyingpartyVerifyCustomTokenRequest) : TVerifyCustomTokenResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'verifyCustomToken';
  _Methodid   = 'identitytoolkit.relyingparty.verifyCustomToken';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aIdentitytoolkitRelyingpartyVerifyCustomTokenRequest,TVerifyCustomTokenResponse) as TVerifyCustomTokenResponse;
end;

Function TRelyingpartyResource.VerifyPassword(aIdentitytoolkitRelyingpartyVerifyPasswordRequest : TIdentitytoolkitRelyingpartyVerifyPasswordRequest) : TVerifyPasswordResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'verifyPassword';
  _Methodid   = 'identitytoolkit.relyingparty.verifyPassword';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aIdentitytoolkitRelyingpartyVerifyPasswordRequest,TVerifyPasswordResponse) as TVerifyPasswordResponse;
end;



{ --------------------------------------------------------------------
  TIdentitytoolkitAPI
  --------------------------------------------------------------------}

Class Function TIdentitytoolkitAPI.APIName : String;

begin
  Result:='identitytoolkit';
end;

Class Function TIdentitytoolkitAPI.APIVersion : String;

begin
  Result:='v3';
end;

Class Function TIdentitytoolkitAPI.APIRevision : String;

begin
  Result:='20160510';
end;

Class Function TIdentitytoolkitAPI.APIID : String;

begin
  Result:='identitytoolkit:v3';
end;

Class Function TIdentitytoolkitAPI.APITitle : String;

begin
  Result:='Google Identity Toolkit API';
end;

Class Function TIdentitytoolkitAPI.APIDescription : String;

begin
  Result:='Help the third party sites to implement federated login.';
end;

Class Function TIdentitytoolkitAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TIdentitytoolkitAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TIdentitytoolkitAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TIdentitytoolkitAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TIdentitytoolkitAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/identity-toolkit/v3/';
end;

Class Function TIdentitytoolkitAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TIdentitytoolkitAPI.APIbasePath : string;

begin
  Result:='/identitytoolkit/v3/relyingparty/';
end;

Class Function TIdentitytoolkitAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/identitytoolkit/v3/relyingparty/';
end;

Class Function TIdentitytoolkitAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TIdentitytoolkitAPI.APIservicePath : string;

begin
  Result:='identitytoolkit/v3/relyingparty/';
end;

Class Function TIdentitytoolkitAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TIdentitytoolkitAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,0);
  
end;

Class Function TIdentitytoolkitAPI.APINeedsAuth : Boolean;

begin
  Result:=False;
end;

Class Procedure TIdentitytoolkitAPI.RegisterAPIResources;

begin
  TCreateAuthUriResponse.RegisterObject;
  TDeleteAccountResponse.RegisterObject;
  TDownloadAccountResponse.RegisterObject;
  TEmailTemplate.RegisterObject;
  TGetAccountInfoResponse.RegisterObject;
  TGetOobConfirmationCodeResponse.RegisterObject;
  TGetRecaptchaParamResponse.RegisterObject;
  TIdentitytoolkitRelyingpartyCreateAuthUriRequest.RegisterObject;
  TIdentitytoolkitRelyingpartyDeleteAccountRequest.RegisterObject;
  TIdentitytoolkitRelyingpartyDownloadAccountRequest.RegisterObject;
  TIdentitytoolkitRelyingpartyGetAccountInfoRequest.RegisterObject;
  TIdentitytoolkitRelyingpartyGetProjectConfigResponse.RegisterObject;
  TIdentitytoolkitRelyingpartyGetPublicKeysResponse.RegisterObject;
  TIdentitytoolkitRelyingpartyResetPasswordRequest.RegisterObject;
  TIdentitytoolkitRelyingpartySetAccountInfoRequest.RegisterObject;
  TIdentitytoolkitRelyingpartySetProjectConfigRequest.RegisterObject;
  TIdentitytoolkitRelyingpartySetProjectConfigResponse.RegisterObject;
  TIdentitytoolkitRelyingpartySignOutUserRequest.RegisterObject;
  TIdentitytoolkitRelyingpartySignOutUserResponse.RegisterObject;
  TIdentitytoolkitRelyingpartySignupNewUserRequest.RegisterObject;
  TIdentitytoolkitRelyingpartyUploadAccountRequest.RegisterObject;
  TIdentitytoolkitRelyingpartyVerifyAssertionRequest.RegisterObject;
  TIdentitytoolkitRelyingpartyVerifyCustomTokenRequest.RegisterObject;
  TIdentitytoolkitRelyingpartyVerifyPasswordRequest.RegisterObject;
  TIdpConfig.RegisterObject;
  TRelyingparty.RegisterObject;
  TResetPasswordResponse.RegisterObject;
  TSetAccountInfoResponseTypeproviderUserInfoItem.RegisterObject;
  TSetAccountInfoResponse.RegisterObject;
  TSignupNewUserResponse.RegisterObject;
  TUploadAccountResponseTypeerrorItem.RegisterObject;
  TUploadAccountResponse.RegisterObject;
  TUserInfoTypeproviderUserInfoItem.RegisterObject;
  TUserInfo.RegisterObject;
  TVerifyAssertionResponse.RegisterObject;
  TVerifyCustomTokenResponse.RegisterObject;
  TVerifyPasswordResponse.RegisterObject;
end;


Function TIdentitytoolkitAPI.GetRelyingpartyInstance : TRelyingpartyResource;

begin
  if (FRelyingpartyInstance=Nil) then
    FRelyingpartyInstance:=CreateRelyingpartyResource;
  Result:=FRelyingpartyInstance;
end;

Function TIdentitytoolkitAPI.CreateRelyingpartyResource : TRelyingpartyResource;

begin
  Result:=CreateRelyingpartyResource(Self);
end;


Function TIdentitytoolkitAPI.CreateRelyingpartyResource(AOwner : TComponent) : TRelyingpartyResource;

begin
  Result:=TRelyingpartyResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TIdentitytoolkitAPI.RegisterAPI;
end.
