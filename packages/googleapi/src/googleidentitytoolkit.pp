unit googleidentitytoolkit;
{
   **********************************************************************
      This file is part of the Free Component Library (FCL)
      Copyright (c) 2015 The free pascal team.
  
      See the file COPYING.FPC, included in this distribution,
      for details about the copyright.
  
      This program is distributed in the hope that it will be useful,
      but WITHOUT ANY WARRANTY; without even the implied warranty of
      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  
   **********************************************************************
}
//Generated on: 9-5-15 13:22:55
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TCreateAuthUriResponse = class;
  TDeleteAccountResponse = class;
  TDownloadAccountResponse = class;
  TGetAccountInfoResponse = class;
  TGetOobConfirmationCodeResponse = class;
  TGetRecaptchaParamResponse = class;
  TIdentitytoolkitRelyingpartyCreateAuthUriRequest = class;
  TIdentitytoolkitRelyingpartyDeleteAccountRequest = class;
  TIdentitytoolkitRelyingpartyDownloadAccountRequest = class;
  TIdentitytoolkitRelyingpartyGetAccountInfoRequest = class;
  TIdentitytoolkitRelyingpartyGetPublicKeysResponse = class;
  TIdentitytoolkitRelyingpartyResetPasswordRequest = class;
  TIdentitytoolkitRelyingpartySetAccountInfoRequest = class;
  TIdentitytoolkitRelyingpartyUploadAccountRequest = class;
  TIdentitytoolkitRelyingpartyVerifyAssertionRequest = class;
  TIdentitytoolkitRelyingpartyVerifyPasswordRequest = class;
  TRelyingparty = class;
  TResetPasswordResponse = class;
  TSetAccountInfoResponse = class;
  TUploadAccountResponse = class;
  TUserInfo = class;
  TVerifyAssertionResponse = class;
  TVerifyPasswordResponse = class;
  TCreateAuthUriResponseArray = Array of TCreateAuthUriResponse;
  TDeleteAccountResponseArray = Array of TDeleteAccountResponse;
  TDownloadAccountResponseArray = Array of TDownloadAccountResponse;
  TGetAccountInfoResponseArray = Array of TGetAccountInfoResponse;
  TGetOobConfirmationCodeResponseArray = Array of TGetOobConfirmationCodeResponse;
  TGetRecaptchaParamResponseArray = Array of TGetRecaptchaParamResponse;
  TIdentitytoolkitRelyingpartyCreateAuthUriRequestArray = Array of TIdentitytoolkitRelyingpartyCreateAuthUriRequest;
  TIdentitytoolkitRelyingpartyDeleteAccountRequestArray = Array of TIdentitytoolkitRelyingpartyDeleteAccountRequest;
  TIdentitytoolkitRelyingpartyDownloadAccountRequestArray = Array of TIdentitytoolkitRelyingpartyDownloadAccountRequest;
  TIdentitytoolkitRelyingpartyGetAccountInfoRequestArray = Array of TIdentitytoolkitRelyingpartyGetAccountInfoRequest;
  TIdentitytoolkitRelyingpartyGetPublicKeysResponseArray = Array of TIdentitytoolkitRelyingpartyGetPublicKeysResponse;
  TIdentitytoolkitRelyingpartyResetPasswordRequestArray = Array of TIdentitytoolkitRelyingpartyResetPasswordRequest;
  TIdentitytoolkitRelyingpartySetAccountInfoRequestArray = Array of TIdentitytoolkitRelyingpartySetAccountInfoRequest;
  TIdentitytoolkitRelyingpartyUploadAccountRequestArray = Array of TIdentitytoolkitRelyingpartyUploadAccountRequest;
  TIdentitytoolkitRelyingpartyVerifyAssertionRequestArray = Array of TIdentitytoolkitRelyingpartyVerifyAssertionRequest;
  TIdentitytoolkitRelyingpartyVerifyPasswordRequestArray = Array of TIdentitytoolkitRelyingpartyVerifyPasswordRequest;
  TRelyingpartyArray = Array of TRelyingparty;
  TResetPasswordResponseArray = Array of TResetPasswordResponse;
  TSetAccountInfoResponseArray = Array of TSetAccountInfoResponse;
  TUploadAccountResponseArray = Array of TUploadAccountResponse;
  TUserInfoArray = Array of TUserInfo;
  TVerifyAssertionResponseArray = Array of TVerifyAssertionResponse;
  TVerifyPasswordResponseArray = Array of TVerifyPasswordResponse;
  //Anonymous types, using auto-generated names
  TSetAccountInfoResponseTypeproviderUserInfoItem = class;
  TUploadAccountResponseTypeerrorItem = class;
  TUserInfoTypeproviderUserInfoItem = class;
  TDownloadAccountResponseTypeusersArray = Array of TUserInfo;
  TGetAccountInfoResponseTypeusersArray = Array of TUserInfo;
  TIdentitytoolkitRelyingpartyUploadAccountRequestTypeusersArray = Array of TUserInfo;
  TSetAccountInfoResponseTypeproviderUserInfoArray = Array of TSetAccountInfoResponseTypeproviderUserInfoItem;
  TUploadAccountResponseTypeerrorArray = Array of TUploadAccountResponseTypeerrorItem;
  TUserInfoTypeproviderUserInfoArray = Array of TUserInfoTypeproviderUserInfoItem;
  
  { --------------------------------------------------------------------
    TCreateAuthUriResponse
    --------------------------------------------------------------------}
  
  TCreateAuthUriResponse = Class(TGoogleBaseObject)
  Private
    FauthUri : String;
    FcaptchaRequired : boolean;
    FforExistingProvider : boolean;
    Fkind : String;
    FproviderId : String;
    Fregistered : boolean;
  Protected
    //Property setters
    Procedure SetauthUri(AIndex : Integer; AValue : String); virtual;
    Procedure SetcaptchaRequired(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetforExistingProvider(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetproviderId(AIndex : Integer; AValue : String); virtual;
    Procedure Setregistered(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property authUri : String Index 0 Read FauthUri Write SetauthUri;
    Property captchaRequired : boolean Index 8 Read FcaptchaRequired Write SetcaptchaRequired;
    Property forExistingProvider : boolean Index 16 Read FforExistingProvider Write SetforExistingProvider;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property providerId : String Index 32 Read FproviderId Write SetproviderId;
    Property registered : boolean Index 40 Read Fregistered Write Setregistered;
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
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
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
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setusers(AIndex : Integer; AValue : TDownloadAccountResponseTypeusersArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property users : TDownloadAccountResponseTypeusersArray Index 16 Read Fusers Write Setusers;
  end;
  TDownloadAccountResponseClass = Class of TDownloadAccountResponse;
  
  { --------------------------------------------------------------------
    TGetAccountInfoResponse
    --------------------------------------------------------------------}
  
  TGetAccountInfoResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fusers : TGetAccountInfoResponseTypeusersArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setusers(AIndex : Integer; AValue : TGetAccountInfoResponseTypeusersArray); virtual;
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
    Fkind : String;
    FoobCode : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetoobCode(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property oobCode : String Index 8 Read FoobCode Write SetoobCode;
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
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetrecaptchaSiteKey(AIndex : Integer; AValue : String); virtual;
    Procedure SetrecaptchaStoken(AIndex : Integer; AValue : String); virtual;
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
    Procedure SetappId(AIndex : Integer; AValue : String); virtual;
    Procedure SetclientId(AIndex : Integer; AValue : String); virtual;
    Procedure Setcontext(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontinueUri(AIndex : Integer; AValue : String); virtual;
    Procedure Setidentifier(AIndex : Integer; AValue : String); virtual;
    Procedure SetoauthConsumerKey(AIndex : Integer; AValue : String); virtual;
    Procedure SetoauthScope(AIndex : Integer; AValue : String); virtual;
    Procedure SetopenidRealm(AIndex : Integer; AValue : String); virtual;
    Procedure SetotaApp(AIndex : Integer; AValue : String); virtual;
    Procedure SetproviderId(AIndex : Integer; AValue : String); virtual;
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
    FlocalId : String;
  Protected
    //Property setters
    Procedure SetlocalId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property localId : String Index 0 Read FlocalId Write SetlocalId;
  end;
  TIdentitytoolkitRelyingpartyDeleteAccountRequestClass = Class of TIdentitytoolkitRelyingpartyDeleteAccountRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyDownloadAccountRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyDownloadAccountRequest = Class(TGoogleBaseObject)
  Private
    FmaxResults : integer;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetmaxResults(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property maxResults : integer Index 0 Read FmaxResults Write SetmaxResults;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TIdentitytoolkitRelyingpartyDownloadAccountRequestClass = Class of TIdentitytoolkitRelyingpartyDownloadAccountRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyGetAccountInfoRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyGetAccountInfoRequest = Class(TGoogleBaseObject)
  Private
    Femail : TStringArray;
    FidToken : String;
    FlocalId : TStringArray;
  Protected
    //Property setters
    Procedure Setemail(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetidToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetlocalId(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property email : TStringArray Index 0 Read Femail Write Setemail;
    Property idToken : String Index 8 Read FidToken Write SetidToken;
    Property localId : TStringArray Index 16 Read FlocalId Write SetlocalId;
  end;
  TIdentitytoolkitRelyingpartyGetAccountInfoRequestClass = Class of TIdentitytoolkitRelyingpartyGetAccountInfoRequest;
  
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
    Procedure Setemail(AIndex : Integer; AValue : String); virtual;
    Procedure SetnewPassword(AIndex : Integer; AValue : String); virtual;
    Procedure SetoldPassword(AIndex : Integer; AValue : String); virtual;
    Procedure SetoobCode(AIndex : Integer; AValue : String); virtual;
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
    FdisplayName : String;
    Femail : String;
    FemailVerified : boolean;
    FidToken : String;
    FlocalId : String;
    FoobCode : String;
    Fpassword : String;
    Fprovider : TStringArray;
    FupgradeToFederatedLogin : boolean;
  Protected
    //Property setters
    Procedure SetcaptchaChallenge(AIndex : Integer; AValue : String); virtual;
    Procedure SetcaptchaResponse(AIndex : Integer; AValue : String); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; AValue : String); virtual;
    Procedure SetemailVerified(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetidToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetlocalId(AIndex : Integer; AValue : String); virtual;
    Procedure SetoobCode(AIndex : Integer; AValue : String); virtual;
    Procedure Setpassword(AIndex : Integer; AValue : String); virtual;
    Procedure Setprovider(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetupgradeToFederatedLogin(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property captchaChallenge : String Index 0 Read FcaptchaChallenge Write SetcaptchaChallenge;
    Property captchaResponse : String Index 8 Read FcaptchaResponse Write SetcaptchaResponse;
    Property displayName : String Index 16 Read FdisplayName Write SetdisplayName;
    Property email : String Index 24 Read Femail Write Setemail;
    Property emailVerified : boolean Index 32 Read FemailVerified Write SetemailVerified;
    Property idToken : String Index 40 Read FidToken Write SetidToken;
    Property localId : String Index 48 Read FlocalId Write SetlocalId;
    Property oobCode : String Index 56 Read FoobCode Write SetoobCode;
    Property password : String Index 64 Read Fpassword Write Setpassword;
    Property provider : TStringArray Index 72 Read Fprovider Write Setprovider;
    Property upgradeToFederatedLogin : boolean Index 80 Read FupgradeToFederatedLogin Write SetupgradeToFederatedLogin;
  end;
  TIdentitytoolkitRelyingpartySetAccountInfoRequestClass = Class of TIdentitytoolkitRelyingpartySetAccountInfoRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyUploadAccountRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyUploadAccountRequest = Class(TGoogleBaseObject)
  Private
    FhashAlgorithm : String;
    FmemoryCost : integer;
    Frounds : integer;
    FsaltSeparator : String;
    FsignerKey : String;
    Fusers : TIdentitytoolkitRelyingpartyUploadAccountRequestTypeusersArray;
  Protected
    //Property setters
    Procedure SethashAlgorithm(AIndex : Integer; AValue : String); virtual;
    Procedure SetmemoryCost(AIndex : Integer; AValue : integer); virtual;
    Procedure Setrounds(AIndex : Integer; AValue : integer); virtual;
    Procedure SetsaltSeparator(AIndex : Integer; AValue : String); virtual;
    Procedure SetsignerKey(AIndex : Integer; AValue : String); virtual;
    Procedure Setusers(AIndex : Integer; AValue : TIdentitytoolkitRelyingpartyUploadAccountRequestTypeusersArray); virtual;
  Public
  Published
    Property hashAlgorithm : String Index 0 Read FhashAlgorithm Write SethashAlgorithm;
    Property memoryCost : integer Index 8 Read FmemoryCost Write SetmemoryCost;
    Property rounds : integer Index 16 Read Frounds Write Setrounds;
    Property saltSeparator : String Index 24 Read FsaltSeparator Write SetsaltSeparator;
    Property signerKey : String Index 32 Read FsignerKey Write SetsignerKey;
    Property users : TIdentitytoolkitRelyingpartyUploadAccountRequestTypeusersArray Index 40 Read Fusers Write Setusers;
  end;
  TIdentitytoolkitRelyingpartyUploadAccountRequestClass = Class of TIdentitytoolkitRelyingpartyUploadAccountRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyVerifyAssertionRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyVerifyAssertionRequest = Class(TGoogleBaseObject)
  Private
    FpendingIdToken : String;
    FpostBody : String;
    FrequestUri : String;
    FreturnRefreshToken : boolean;
  Protected
    //Property setters
    Procedure SetpendingIdToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetpostBody(AIndex : Integer; AValue : String); virtual;
    Procedure SetrequestUri(AIndex : Integer; AValue : String); virtual;
    Procedure SetreturnRefreshToken(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property pendingIdToken : String Index 0 Read FpendingIdToken Write SetpendingIdToken;
    Property postBody : String Index 8 Read FpostBody Write SetpostBody;
    Property requestUri : String Index 16 Read FrequestUri Write SetrequestUri;
    Property returnRefreshToken : boolean Index 24 Read FreturnRefreshToken Write SetreturnRefreshToken;
  end;
  TIdentitytoolkitRelyingpartyVerifyAssertionRequestClass = Class of TIdentitytoolkitRelyingpartyVerifyAssertionRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyVerifyPasswordRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyVerifyPasswordRequest = Class(TGoogleBaseObject)
  Private
    FcaptchaChallenge : String;
    FcaptchaResponse : String;
    Femail : String;
    Fpassword : String;
    FpendingIdToken : String;
  Protected
    //Property setters
    Procedure SetcaptchaChallenge(AIndex : Integer; AValue : String); virtual;
    Procedure SetcaptchaResponse(AIndex : Integer; AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; AValue : String); virtual;
    Procedure Setpassword(AIndex : Integer; AValue : String); virtual;
    Procedure SetpendingIdToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property captchaChallenge : String Index 0 Read FcaptchaChallenge Write SetcaptchaChallenge;
    Property captchaResponse : String Index 8 Read FcaptchaResponse Write SetcaptchaResponse;
    Property email : String Index 16 Read Femail Write Setemail;
    Property password : String Index 24 Read Fpassword Write Setpassword;
    Property pendingIdToken : String Index 32 Read FpendingIdToken Write SetpendingIdToken;
  end;
  TIdentitytoolkitRelyingpartyVerifyPasswordRequestClass = Class of TIdentitytoolkitRelyingpartyVerifyPasswordRequest;
  
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
    Procedure SetcaptchaResp(AIndex : Integer; AValue : String); virtual;
    Procedure Setchallenge(AIndex : Integer; AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; AValue : String); virtual;
    Procedure SetidToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnewEmail(AIndex : Integer; AValue : String); virtual;
    Procedure SetrequestType(AIndex : Integer; AValue : String); virtual;
    Procedure SetuserIp(AIndex : Integer; AValue : String); virtual;
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
    Procedure Setemail(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
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
    FphotoUrl : String;
    FproviderId : String;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure SetphotoUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetproviderId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property displayName : String Index 0 Read FdisplayName Write SetdisplayName;
    Property photoUrl : String Index 8 Read FphotoUrl Write SetphotoUrl;
    Property providerId : String Index 16 Read FproviderId Write SetproviderId;
  end;
  TSetAccountInfoResponseTypeproviderUserInfoItemClass = Class of TSetAccountInfoResponseTypeproviderUserInfoItem;
  
  { --------------------------------------------------------------------
    TSetAccountInfoResponse
    --------------------------------------------------------------------}
  
  TSetAccountInfoResponse = Class(TGoogleBaseObject)
  Private
    FdisplayName : String;
    Femail : String;
    FidToken : String;
    Fkind : String;
    FproviderUserInfo : TSetAccountInfoResponseTypeproviderUserInfoArray;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; AValue : String); virtual;
    Procedure SetidToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetproviderUserInfo(AIndex : Integer; AValue : TSetAccountInfoResponseTypeproviderUserInfoArray); virtual;
  Public
  Published
    Property displayName : String Index 0 Read FdisplayName Write SetdisplayName;
    Property email : String Index 8 Read Femail Write Setemail;
    Property idToken : String Index 16 Read FidToken Write SetidToken;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property providerUserInfo : TSetAccountInfoResponseTypeproviderUserInfoArray Index 32 Read FproviderUserInfo Write SetproviderUserInfo;
  end;
  TSetAccountInfoResponseClass = Class of TSetAccountInfoResponse;
  
  { --------------------------------------------------------------------
    TUploadAccountResponseTypeerrorItem
    --------------------------------------------------------------------}
  
  TUploadAccountResponseTypeerrorItem = Class(TGoogleBaseObject)
  Private
    Findex : integer;
    Fmessage : String;
  Protected
    //Property setters
    Procedure Setindex(AIndex : Integer; AValue : integer); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : String); virtual;
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
    Procedure Seterror(AIndex : Integer; AValue : TUploadAccountResponseTypeerrorArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
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
    FfederatedId : String;
    FphotoUrl : String;
    FproviderId : String;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure SetfederatedId(AIndex : Integer; AValue : String); virtual;
    Procedure SetphotoUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetproviderId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property displayName : String Index 0 Read FdisplayName Write SetdisplayName;
    Property federatedId : String Index 8 Read FfederatedId Write SetfederatedId;
    Property photoUrl : String Index 16 Read FphotoUrl Write SetphotoUrl;
    Property providerId : String Index 24 Read FproviderId Write SetproviderId;
  end;
  TUserInfoTypeproviderUserInfoItemClass = Class of TUserInfoTypeproviderUserInfoItem;
  
  { --------------------------------------------------------------------
    TUserInfo
    --------------------------------------------------------------------}
  
  TUserInfo = Class(TGoogleBaseObject)
  Private
    FdisplayName : String;
    Femail : String;
    FemailVerified : boolean;
    FlocalId : String;
    FpasswordHash : String;
    FpasswordUpdatedAt : double;
    FphotoUrl : String;
    FproviderUserInfo : TUserInfoTypeproviderUserInfoArray;
    Fsalt : String;
    Fversion : integer;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; AValue : String); virtual;
    Procedure SetemailVerified(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetlocalId(AIndex : Integer; AValue : String); virtual;
    Procedure SetpasswordHash(AIndex : Integer; AValue : String); virtual;
    Procedure SetpasswordUpdatedAt(AIndex : Integer; AValue : double); virtual;
    Procedure SetphotoUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetproviderUserInfo(AIndex : Integer; AValue : TUserInfoTypeproviderUserInfoArray); virtual;
    Procedure Setsalt(AIndex : Integer; AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property displayName : String Index 0 Read FdisplayName Write SetdisplayName;
    Property email : String Index 8 Read Femail Write Setemail;
    Property emailVerified : boolean Index 16 Read FemailVerified Write SetemailVerified;
    Property localId : String Index 24 Read FlocalId Write SetlocalId;
    Property passwordHash : String Index 32 Read FpasswordHash Write SetpasswordHash;
    Property passwordUpdatedAt : double Index 40 Read FpasswordUpdatedAt Write SetpasswordUpdatedAt;
    Property photoUrl : String Index 48 Read FphotoUrl Write SetphotoUrl;
    Property providerUserInfo : TUserInfoTypeproviderUserInfoArray Index 56 Read FproviderUserInfo Write SetproviderUserInfo;
    Property salt : String Index 64 Read Fsalt Write Setsalt;
    Property version : integer Index 72 Read Fversion Write Setversion;
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
    FnickName : String;
    FoauthAccessToken : String;
    FoauthAuthorizationCode : String;
    FoauthExpireIn : integer;
    FoauthRequestToken : String;
    FoauthScope : String;
    ForiginalEmail : String;
    FphotoUrl : String;
    FproviderId : String;
    FtimeZone : String;
    FverifiedProvider : TStringArray;
  Protected
    //Property setters
    Procedure Setaction(AIndex : Integer; AValue : String); virtual;
    Procedure SetappInstallationUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetappScheme(AIndex : Integer; AValue : String); virtual;
    Procedure Setcontext(AIndex : Integer; AValue : String); virtual;
    Procedure SetdateOfBirth(AIndex : Integer; AValue : String); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; AValue : String); virtual;
    Procedure SetemailRecycled(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetemailVerified(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetfederatedId(AIndex : Integer; AValue : String); virtual;
    Procedure SetfirstName(AIndex : Integer; AValue : String); virtual;
    Procedure SetfullName(AIndex : Integer; AValue : String); virtual;
    Procedure SetidToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetinputEmail(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlanguage(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastName(AIndex : Integer; AValue : String); virtual;
    Procedure SetlocalId(AIndex : Integer; AValue : String); virtual;
    Procedure SetneedConfirmation(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetnickName(AIndex : Integer; AValue : String); virtual;
    Procedure SetoauthAccessToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetoauthAuthorizationCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetoauthExpireIn(AIndex : Integer; AValue : integer); virtual;
    Procedure SetoauthRequestToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetoauthScope(AIndex : Integer; AValue : String); virtual;
    Procedure SetoriginalEmail(AIndex : Integer; AValue : String); virtual;
    Procedure SetphotoUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetproviderId(AIndex : Integer; AValue : String); virtual;
    Procedure SettimeZone(AIndex : Integer; AValue : String); virtual;
    Procedure SetverifiedProvider(AIndex : Integer; AValue : TStringArray); virtual;
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
    Property federatedId : String Index 72 Read FfederatedId Write SetfederatedId;
    Property firstName : String Index 80 Read FfirstName Write SetfirstName;
    Property fullName : String Index 88 Read FfullName Write SetfullName;
    Property idToken : String Index 96 Read FidToken Write SetidToken;
    Property inputEmail : String Index 104 Read FinputEmail Write SetinputEmail;
    Property kind : String Index 112 Read Fkind Write Setkind;
    Property language : String Index 120 Read Flanguage Write Setlanguage;
    Property lastName : String Index 128 Read FlastName Write SetlastName;
    Property localId : String Index 136 Read FlocalId Write SetlocalId;
    Property needConfirmation : boolean Index 144 Read FneedConfirmation Write SetneedConfirmation;
    Property nickName : String Index 152 Read FnickName Write SetnickName;
    Property oauthAccessToken : String Index 160 Read FoauthAccessToken Write SetoauthAccessToken;
    Property oauthAuthorizationCode : String Index 168 Read FoauthAuthorizationCode Write SetoauthAuthorizationCode;
    Property oauthExpireIn : integer Index 176 Read FoauthExpireIn Write SetoauthExpireIn;
    Property oauthRequestToken : String Index 184 Read FoauthRequestToken Write SetoauthRequestToken;
    Property oauthScope : String Index 192 Read FoauthScope Write SetoauthScope;
    Property originalEmail : String Index 200 Read ForiginalEmail Write SetoriginalEmail;
    Property photoUrl : String Index 208 Read FphotoUrl Write SetphotoUrl;
    Property providerId : String Index 216 Read FproviderId Write SetproviderId;
    Property timeZone : String Index 224 Read FtimeZone Write SettimeZone;
    Property verifiedProvider : TStringArray Index 232 Read FverifiedProvider Write SetverifiedProvider;
  end;
  TVerifyAssertionResponseClass = Class of TVerifyAssertionResponse;
  
  { --------------------------------------------------------------------
    TVerifyPasswordResponse
    --------------------------------------------------------------------}
  
  TVerifyPasswordResponse = Class(TGoogleBaseObject)
  Private
    FdisplayName : String;
    Femail : String;
    FidToken : String;
    Fkind : String;
    FlocalId : String;
    FphotoUrl : String;
    Fregistered : boolean;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; AValue : String); virtual;
    Procedure SetidToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlocalId(AIndex : Integer; AValue : String); virtual;
    Procedure SetphotoUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Setregistered(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property displayName : String Index 0 Read FdisplayName Write SetdisplayName;
    Property email : String Index 8 Read Femail Write Setemail;
    Property idToken : String Index 16 Read FidToken Write SetidToken;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property localId : String Index 32 Read FlocalId Write SetlocalId;
    Property photoUrl : String Index 40 Read FphotoUrl Write SetphotoUrl;
    Property registered : boolean Index 48 Read Fregistered Write Setregistered;
  end;
  TVerifyPasswordResponseClass = Class of TVerifyPasswordResponse;
  
  { --------------------------------------------------------------------
    TRelyingpartyResource
    --------------------------------------------------------------------}
  
  TRelyingpartyResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateAuthUri(aIdentitytoolkitRelyingpartyCreateAuthUriRequest : TIdentitytoolkitRelyingpartyCreateAuthUriRequest) : TCreateAuthUriResponse;
    Function DeleteAccount(aIdentitytoolkitRelyingpartyDeleteAccountRequest : TIdentitytoolkitRelyingpartyDeleteAccountRequest) : TDeleteAccountResponse;
    Function DownloadAccount(aIdentitytoolkitRelyingpartyDownloadAccountRequest : TIdentitytoolkitRelyingpartyDownloadAccountRequest) : TDownloadAccountResponse;
    Function GetAccountInfo(aIdentitytoolkitRelyingpartyGetAccountInfoRequest : TIdentitytoolkitRelyingpartyGetAccountInfoRequest) : TGetAccountInfoResponse;
    Function GetOobConfirmationCode(aRelyingparty : TRelyingparty) : TGetOobConfirmationCodeResponse;
    Function GetPublicKeys : TIdentitytoolkitRelyingpartyGetPublicKeysResponse;
    Function GetRecaptchaParam : TGetRecaptchaParamResponse;
    Function ResetPassword(aIdentitytoolkitRelyingpartyResetPasswordRequest : TIdentitytoolkitRelyingpartyResetPasswordRequest) : TResetPasswordResponse;
    Function SetAccountInfo(aIdentitytoolkitRelyingpartySetAccountInfoRequest : TIdentitytoolkitRelyingpartySetAccountInfoRequest) : TSetAccountInfoResponse;
    Function UploadAccount(aIdentitytoolkitRelyingpartyUploadAccountRequest : TIdentitytoolkitRelyingpartyUploadAccountRequest) : TUploadAccountResponse;
    Function VerifyAssertion(aIdentitytoolkitRelyingpartyVerifyAssertionRequest : TIdentitytoolkitRelyingpartyVerifyAssertionRequest) : TVerifyAssertionResponse;
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


Procedure TCreateAuthUriResponse.SetauthUri(AIndex : Integer; AValue : String); 

begin
  If (FauthUri=AValue) then exit;
  FauthUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateAuthUriResponse.SetcaptchaRequired(AIndex : Integer; AValue : boolean); 

begin
  If (FcaptchaRequired=AValue) then exit;
  FcaptchaRequired:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateAuthUriResponse.SetforExistingProvider(AIndex : Integer; AValue : boolean); 

begin
  If (FforExistingProvider=AValue) then exit;
  FforExistingProvider:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateAuthUriResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateAuthUriResponse.SetproviderId(AIndex : Integer; AValue : String); 

begin
  If (FproviderId=AValue) then exit;
  FproviderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateAuthUriResponse.Setregistered(AIndex : Integer; AValue : boolean); 

begin
  If (Fregistered=AValue) then exit;
  Fregistered:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDeleteAccountResponse
  --------------------------------------------------------------------}


Procedure TDeleteAccountResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDownloadAccountResponse
  --------------------------------------------------------------------}


Procedure TDownloadAccountResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccountResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccountResponse.Setusers(AIndex : Integer; AValue : TDownloadAccountResponseTypeusersArray); 

begin
  If (Fusers=AValue) then exit;
  Fusers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGetAccountInfoResponse
  --------------------------------------------------------------------}


Procedure TGetAccountInfoResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetAccountInfoResponse.Setusers(AIndex : Integer; AValue : TGetAccountInfoResponseTypeusersArray); 

begin
  If (Fusers=AValue) then exit;
  Fusers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGetOobConfirmationCodeResponse
  --------------------------------------------------------------------}


Procedure TGetOobConfirmationCodeResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetOobConfirmationCodeResponse.SetoobCode(AIndex : Integer; AValue : String); 

begin
  If (FoobCode=AValue) then exit;
  FoobCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGetRecaptchaParamResponse
  --------------------------------------------------------------------}


Procedure TGetRecaptchaParamResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetRecaptchaParamResponse.SetrecaptchaSiteKey(AIndex : Integer; AValue : String); 

begin
  If (FrecaptchaSiteKey=AValue) then exit;
  FrecaptchaSiteKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetRecaptchaParamResponse.SetrecaptchaStoken(AIndex : Integer; AValue : String); 

begin
  If (FrecaptchaStoken=AValue) then exit;
  FrecaptchaStoken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyCreateAuthUriRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetappId(AIndex : Integer; AValue : String); 

begin
  If (FappId=AValue) then exit;
  FappId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetclientId(AIndex : Integer; AValue : String); 

begin
  If (FclientId=AValue) then exit;
  FclientId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.Setcontext(AIndex : Integer; AValue : String); 

begin
  If (Fcontext=AValue) then exit;
  Fcontext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetcontinueUri(AIndex : Integer; AValue : String); 

begin
  If (FcontinueUri=AValue) then exit;
  FcontinueUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.Setidentifier(AIndex : Integer; AValue : String); 

begin
  If (Fidentifier=AValue) then exit;
  Fidentifier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetoauthConsumerKey(AIndex : Integer; AValue : String); 

begin
  If (FoauthConsumerKey=AValue) then exit;
  FoauthConsumerKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetoauthScope(AIndex : Integer; AValue : String); 

begin
  If (FoauthScope=AValue) then exit;
  FoauthScope:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetopenidRealm(AIndex : Integer; AValue : String); 

begin
  If (FopenidRealm=AValue) then exit;
  FopenidRealm:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetotaApp(AIndex : Integer; AValue : String); 

begin
  If (FotaApp=AValue) then exit;
  FotaApp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetproviderId(AIndex : Integer; AValue : String); 

begin
  If (FproviderId=AValue) then exit;
  FproviderId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyDeleteAccountRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartyDeleteAccountRequest.SetlocalId(AIndex : Integer; AValue : String); 

begin
  If (FlocalId=AValue) then exit;
  FlocalId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyDownloadAccountRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartyDownloadAccountRequest.SetmaxResults(AIndex : Integer; AValue : integer); 

begin
  If (FmaxResults=AValue) then exit;
  FmaxResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyDownloadAccountRequest.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyGetAccountInfoRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartyGetAccountInfoRequest.Setemail(AIndex : Integer; AValue : TStringArray); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyGetAccountInfoRequest.SetidToken(AIndex : Integer; AValue : String); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyGetAccountInfoRequest.SetlocalId(AIndex : Integer; AValue : TStringArray); 

begin
  If (FlocalId=AValue) then exit;
  FlocalId:=AValue;
  MarkPropertyChanged(AIndex);
end;





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


Procedure TIdentitytoolkitRelyingpartyResetPasswordRequest.Setemail(AIndex : Integer; AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyResetPasswordRequest.SetnewPassword(AIndex : Integer; AValue : String); 

begin
  If (FnewPassword=AValue) then exit;
  FnewPassword:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyResetPasswordRequest.SetoldPassword(AIndex : Integer; AValue : String); 

begin
  If (FoldPassword=AValue) then exit;
  FoldPassword:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyResetPasswordRequest.SetoobCode(AIndex : Integer; AValue : String); 

begin
  If (FoobCode=AValue) then exit;
  FoobCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartySetAccountInfoRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetcaptchaChallenge(AIndex : Integer; AValue : String); 

begin
  If (FcaptchaChallenge=AValue) then exit;
  FcaptchaChallenge:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetcaptchaResponse(AIndex : Integer; AValue : String); 

begin
  If (FcaptchaResponse=AValue) then exit;
  FcaptchaResponse:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.Setemail(AIndex : Integer; AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetemailVerified(AIndex : Integer; AValue : boolean); 

begin
  If (FemailVerified=AValue) then exit;
  FemailVerified:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetidToken(AIndex : Integer; AValue : String); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetlocalId(AIndex : Integer; AValue : String); 

begin
  If (FlocalId=AValue) then exit;
  FlocalId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetoobCode(AIndex : Integer; AValue : String); 

begin
  If (FoobCode=AValue) then exit;
  FoobCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.Setpassword(AIndex : Integer; AValue : String); 

begin
  If (Fpassword=AValue) then exit;
  Fpassword:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.Setprovider(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fprovider=AValue) then exit;
  Fprovider:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetupgradeToFederatedLogin(AIndex : Integer; AValue : boolean); 

begin
  If (FupgradeToFederatedLogin=AValue) then exit;
  FupgradeToFederatedLogin:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyUploadAccountRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartyUploadAccountRequest.SethashAlgorithm(AIndex : Integer; AValue : String); 

begin
  If (FhashAlgorithm=AValue) then exit;
  FhashAlgorithm:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyUploadAccountRequest.SetmemoryCost(AIndex : Integer; AValue : integer); 

begin
  If (FmemoryCost=AValue) then exit;
  FmemoryCost:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyUploadAccountRequest.Setrounds(AIndex : Integer; AValue : integer); 

begin
  If (Frounds=AValue) then exit;
  Frounds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyUploadAccountRequest.SetsaltSeparator(AIndex : Integer; AValue : String); 

begin
  If (FsaltSeparator=AValue) then exit;
  FsaltSeparator:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyUploadAccountRequest.SetsignerKey(AIndex : Integer; AValue : String); 

begin
  If (FsignerKey=AValue) then exit;
  FsignerKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyUploadAccountRequest.Setusers(AIndex : Integer; AValue : TIdentitytoolkitRelyingpartyUploadAccountRequestTypeusersArray); 

begin
  If (Fusers=AValue) then exit;
  Fusers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyVerifyAssertionRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartyVerifyAssertionRequest.SetpendingIdToken(AIndex : Integer; AValue : String); 

begin
  If (FpendingIdToken=AValue) then exit;
  FpendingIdToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyAssertionRequest.SetpostBody(AIndex : Integer; AValue : String); 

begin
  If (FpostBody=AValue) then exit;
  FpostBody:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyAssertionRequest.SetrequestUri(AIndex : Integer; AValue : String); 

begin
  If (FrequestUri=AValue) then exit;
  FrequestUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyAssertionRequest.SetreturnRefreshToken(AIndex : Integer; AValue : boolean); 

begin
  If (FreturnRefreshToken=AValue) then exit;
  FreturnRefreshToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyVerifyPasswordRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartyVerifyPasswordRequest.SetcaptchaChallenge(AIndex : Integer; AValue : String); 

begin
  If (FcaptchaChallenge=AValue) then exit;
  FcaptchaChallenge:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyPasswordRequest.SetcaptchaResponse(AIndex : Integer; AValue : String); 

begin
  If (FcaptchaResponse=AValue) then exit;
  FcaptchaResponse:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyPasswordRequest.Setemail(AIndex : Integer; AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyPasswordRequest.Setpassword(AIndex : Integer; AValue : String); 

begin
  If (Fpassword=AValue) then exit;
  Fpassword:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyPasswordRequest.SetpendingIdToken(AIndex : Integer; AValue : String); 

begin
  If (FpendingIdToken=AValue) then exit;
  FpendingIdToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRelyingparty
  --------------------------------------------------------------------}


Procedure TRelyingparty.SetcaptchaResp(AIndex : Integer; AValue : String); 

begin
  If (FcaptchaResp=AValue) then exit;
  FcaptchaResp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelyingparty.Setchallenge(AIndex : Integer; AValue : String); 

begin
  If (Fchallenge=AValue) then exit;
  Fchallenge:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelyingparty.Setemail(AIndex : Integer; AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelyingparty.SetidToken(AIndex : Integer; AValue : String); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelyingparty.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelyingparty.SetnewEmail(AIndex : Integer; AValue : String); 

begin
  If (FnewEmail=AValue) then exit;
  FnewEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelyingparty.SetrequestType(AIndex : Integer; AValue : String); 

begin
  If (FrequestType=AValue) then exit;
  FrequestType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelyingparty.SetuserIp(AIndex : Integer; AValue : String); 

begin
  If (FuserIp=AValue) then exit;
  FuserIp:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResetPasswordResponse
  --------------------------------------------------------------------}


Procedure TResetPasswordResponse.Setemail(AIndex : Integer; AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResetPasswordResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSetAccountInfoResponseTypeproviderUserInfoItem
  --------------------------------------------------------------------}


Procedure TSetAccountInfoResponseTypeproviderUserInfoItem.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponseTypeproviderUserInfoItem.SetphotoUrl(AIndex : Integer; AValue : String); 

begin
  If (FphotoUrl=AValue) then exit;
  FphotoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponseTypeproviderUserInfoItem.SetproviderId(AIndex : Integer; AValue : String); 

begin
  If (FproviderId=AValue) then exit;
  FproviderId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSetAccountInfoResponse
  --------------------------------------------------------------------}


Procedure TSetAccountInfoResponse.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponse.Setemail(AIndex : Integer; AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponse.SetidToken(AIndex : Integer; AValue : String); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponse.SetproviderUserInfo(AIndex : Integer; AValue : TSetAccountInfoResponseTypeproviderUserInfoArray); 

begin
  If (FproviderUserInfo=AValue) then exit;
  FproviderUserInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUploadAccountResponseTypeerrorItem
  --------------------------------------------------------------------}


Procedure TUploadAccountResponseTypeerrorItem.Setindex(AIndex : Integer; AValue : integer); 

begin
  If (Findex=AValue) then exit;
  Findex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUploadAccountResponseTypeerrorItem.Setmessage(AIndex : Integer; AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUploadAccountResponse
  --------------------------------------------------------------------}


Procedure TUploadAccountResponse.Seterror(AIndex : Integer; AValue : TUploadAccountResponseTypeerrorArray); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUploadAccountResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserInfoTypeproviderUserInfoItem
  --------------------------------------------------------------------}


Procedure TUserInfoTypeproviderUserInfoItem.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfoTypeproviderUserInfoItem.SetfederatedId(AIndex : Integer; AValue : String); 

begin
  If (FfederatedId=AValue) then exit;
  FfederatedId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfoTypeproviderUserInfoItem.SetphotoUrl(AIndex : Integer; AValue : String); 

begin
  If (FphotoUrl=AValue) then exit;
  FphotoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfoTypeproviderUserInfoItem.SetproviderId(AIndex : Integer; AValue : String); 

begin
  If (FproviderId=AValue) then exit;
  FproviderId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserInfo
  --------------------------------------------------------------------}


Procedure TUserInfo.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.Setemail(AIndex : Integer; AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.SetemailVerified(AIndex : Integer; AValue : boolean); 

begin
  If (FemailVerified=AValue) then exit;
  FemailVerified:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.SetlocalId(AIndex : Integer; AValue : String); 

begin
  If (FlocalId=AValue) then exit;
  FlocalId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.SetpasswordHash(AIndex : Integer; AValue : String); 

begin
  If (FpasswordHash=AValue) then exit;
  FpasswordHash:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.SetpasswordUpdatedAt(AIndex : Integer; AValue : double); 

begin
  If (FpasswordUpdatedAt=AValue) then exit;
  FpasswordUpdatedAt:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.SetphotoUrl(AIndex : Integer; AValue : String); 

begin
  If (FphotoUrl=AValue) then exit;
  FphotoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.SetproviderUserInfo(AIndex : Integer; AValue : TUserInfoTypeproviderUserInfoArray); 

begin
  If (FproviderUserInfo=AValue) then exit;
  FproviderUserInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.Setsalt(AIndex : Integer; AValue : String); 

begin
  If (Fsalt=AValue) then exit;
  Fsalt:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.Setversion(AIndex : Integer; AValue : integer); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVerifyAssertionResponse
  --------------------------------------------------------------------}


Procedure TVerifyAssertionResponse.Setaction(AIndex : Integer; AValue : String); 

begin
  If (Faction=AValue) then exit;
  Faction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetappInstallationUrl(AIndex : Integer; AValue : String); 

begin
  If (FappInstallationUrl=AValue) then exit;
  FappInstallationUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetappScheme(AIndex : Integer; AValue : String); 

begin
  If (FappScheme=AValue) then exit;
  FappScheme:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.Setcontext(AIndex : Integer; AValue : String); 

begin
  If (Fcontext=AValue) then exit;
  Fcontext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetdateOfBirth(AIndex : Integer; AValue : String); 

begin
  If (FdateOfBirth=AValue) then exit;
  FdateOfBirth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.Setemail(AIndex : Integer; AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetemailRecycled(AIndex : Integer; AValue : boolean); 

begin
  If (FemailRecycled=AValue) then exit;
  FemailRecycled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetemailVerified(AIndex : Integer; AValue : boolean); 

begin
  If (FemailVerified=AValue) then exit;
  FemailVerified:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetfederatedId(AIndex : Integer; AValue : String); 

begin
  If (FfederatedId=AValue) then exit;
  FfederatedId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetfirstName(AIndex : Integer; AValue : String); 

begin
  If (FfirstName=AValue) then exit;
  FfirstName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetfullName(AIndex : Integer; AValue : String); 

begin
  If (FfullName=AValue) then exit;
  FfullName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetidToken(AIndex : Integer; AValue : String); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetinputEmail(AIndex : Integer; AValue : String); 

begin
  If (FinputEmail=AValue) then exit;
  FinputEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.Setlanguage(AIndex : Integer; AValue : String); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetlastName(AIndex : Integer; AValue : String); 

begin
  If (FlastName=AValue) then exit;
  FlastName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetlocalId(AIndex : Integer; AValue : String); 

begin
  If (FlocalId=AValue) then exit;
  FlocalId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetneedConfirmation(AIndex : Integer; AValue : boolean); 

begin
  If (FneedConfirmation=AValue) then exit;
  FneedConfirmation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetnickName(AIndex : Integer; AValue : String); 

begin
  If (FnickName=AValue) then exit;
  FnickName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetoauthAccessToken(AIndex : Integer; AValue : String); 

begin
  If (FoauthAccessToken=AValue) then exit;
  FoauthAccessToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetoauthAuthorizationCode(AIndex : Integer; AValue : String); 

begin
  If (FoauthAuthorizationCode=AValue) then exit;
  FoauthAuthorizationCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetoauthExpireIn(AIndex : Integer; AValue : integer); 

begin
  If (FoauthExpireIn=AValue) then exit;
  FoauthExpireIn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetoauthRequestToken(AIndex : Integer; AValue : String); 

begin
  If (FoauthRequestToken=AValue) then exit;
  FoauthRequestToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetoauthScope(AIndex : Integer; AValue : String); 

begin
  If (FoauthScope=AValue) then exit;
  FoauthScope:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetoriginalEmail(AIndex : Integer; AValue : String); 

begin
  If (ForiginalEmail=AValue) then exit;
  ForiginalEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetphotoUrl(AIndex : Integer; AValue : String); 

begin
  If (FphotoUrl=AValue) then exit;
  FphotoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetproviderId(AIndex : Integer; AValue : String); 

begin
  If (FproviderId=AValue) then exit;
  FproviderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SettimeZone(AIndex : Integer; AValue : String); 

begin
  If (FtimeZone=AValue) then exit;
  FtimeZone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetverifiedProvider(AIndex : Integer; AValue : TStringArray); 

begin
  If (FverifiedProvider=AValue) then exit;
  FverifiedProvider:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVerifyPasswordResponse
  --------------------------------------------------------------------}


Procedure TVerifyPasswordResponse.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyPasswordResponse.Setemail(AIndex : Integer; AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyPasswordResponse.SetidToken(AIndex : Integer; AValue : String); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyPasswordResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyPasswordResponse.SetlocalId(AIndex : Integer; AValue : String); 

begin
  If (FlocalId=AValue) then exit;
  FlocalId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyPasswordResponse.SetphotoUrl(AIndex : Integer; AValue : String); 

begin
  If (FphotoUrl=AValue) then exit;
  FphotoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyPasswordResponse.Setregistered(AIndex : Integer; AValue : boolean); 

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
  Result:='20150406';
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
  TGetAccountInfoResponse.RegisterObject;
  TGetOobConfirmationCodeResponse.RegisterObject;
  TGetRecaptchaParamResponse.RegisterObject;
  TIdentitytoolkitRelyingpartyCreateAuthUriRequest.RegisterObject;
  TIdentitytoolkitRelyingpartyDeleteAccountRequest.RegisterObject;
  TIdentitytoolkitRelyingpartyDownloadAccountRequest.RegisterObject;
  TIdentitytoolkitRelyingpartyGetAccountInfoRequest.RegisterObject;
  TIdentitytoolkitRelyingpartyGetPublicKeysResponse.RegisterObject;
  TIdentitytoolkitRelyingpartyResetPasswordRequest.RegisterObject;
  TIdentitytoolkitRelyingpartySetAccountInfoRequest.RegisterObject;
  TIdentitytoolkitRelyingpartyUploadAccountRequest.RegisterObject;
  TIdentitytoolkitRelyingpartyVerifyAssertionRequest.RegisterObject;
  TIdentitytoolkitRelyingpartyVerifyPasswordRequest.RegisterObject;
  TRelyingparty.RegisterObject;
  TResetPasswordResponse.RegisterObject;
  TSetAccountInfoResponseTypeproviderUserInfoItem.RegisterObject;
  TSetAccountInfoResponse.RegisterObject;
  TUploadAccountResponseTypeerrorItem.RegisterObject;
  TUploadAccountResponse.RegisterObject;
  TUserInfoTypeproviderUserInfoItem.RegisterObject;
  TUserInfo.RegisterObject;
  TVerifyAssertionResponse.RegisterObject;
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
  Result.API:=Self;
end;



initialization
  TIdentitytoolkitAPI.RegisterAPI;
end.
