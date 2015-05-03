unit googleidentitytoolkit;
{
  This is the file COPYING.FPC, it applies to the Free Pascal Run-Time Library 
  (RTL) and packages (packages) distributed by members of the Free Pascal 
  Development Team.
  
  The source code of the Free Pascal Runtime Libraries and packages are 
  distributed under the Library GNU General Public License 
  (see the file COPYING) with the following modification:
  
  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,
  and to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a module
  which is not derived from or based on this library. If you modify this
  library, you may extend this exception to your version of the library, but you are
  not obligated to do so. If you do not wish to do so, delete this exception
  statement from your version.
  
  If you didn't receive a copy of the file COPYING, contact:
        Free Software Foundation
        675 Mass Ave
        Cambridge, MA  02139
        USA
  
}
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  //
  TCreateAuthUriResponse = class;
  TCreateAuthUriResponseArray = Array of TCreateAuthUriResponse;
  TDeleteAccountResponse = class;
  TDeleteAccountResponseArray = Array of TDeleteAccountResponse;
  TDownloadAccountResponse = class;
  TDownloadAccountResponseArray = Array of TDownloadAccountResponse;
  TDownloadAccountResponseusers = class;
  TDownloadAccountResponseusersArray = Array of TDownloadAccountResponseusers;
  TGetAccountInfoResponse = class;
  TGetAccountInfoResponseArray = Array of TGetAccountInfoResponse;
  TGetAccountInfoResponseusers = class;
  TGetAccountInfoResponseusersArray = Array of TGetAccountInfoResponseusers;
  TGetOobConfirmationCodeResponse = class;
  TGetOobConfirmationCodeResponseArray = Array of TGetOobConfirmationCodeResponse;
  TGetRecaptchaParamResponse = class;
  TGetRecaptchaParamResponseArray = Array of TGetRecaptchaParamResponse;
  TIdentitytoolkitRelyingpartyCreateAuthUriRequest = class;
  TIdentitytoolkitRelyingpartyCreateAuthUriRequestArray = Array of TIdentitytoolkitRelyingpartyCreateAuthUriRequest;
  TIdentitytoolkitRelyingpartyDeleteAccountRequest = class;
  TIdentitytoolkitRelyingpartyDeleteAccountRequestArray = Array of TIdentitytoolkitRelyingpartyDeleteAccountRequest;
  TIdentitytoolkitRelyingpartyDownloadAccountRequest = class;
  TIdentitytoolkitRelyingpartyDownloadAccountRequestArray = Array of TIdentitytoolkitRelyingpartyDownloadAccountRequest;
  TIdentitytoolkitRelyingpartyGetAccountInfoRequest = class;
  TIdentitytoolkitRelyingpartyGetAccountInfoRequestArray = Array of TIdentitytoolkitRelyingpartyGetAccountInfoRequest;
  TIdentitytoolkitRelyingpartyGetAccountInfoRequestemail = class;
  TIdentitytoolkitRelyingpartyGetAccountInfoRequestemailArray = Array of TIdentitytoolkitRelyingpartyGetAccountInfoRequestemail;
  TIdentitytoolkitRelyingpartyGetAccountInfoRequestlocalId = class;
  TIdentitytoolkitRelyingpartyGetAccountInfoRequestlocalIdArray = Array of TIdentitytoolkitRelyingpartyGetAccountInfoRequestlocalId;
  TIdentitytoolkitRelyingpartyGetPublicKeysResponse = class;
  TIdentitytoolkitRelyingpartyGetPublicKeysResponseArray = Array of TIdentitytoolkitRelyingpartyGetPublicKeysResponse;
  TIdentitytoolkitRelyingpartyResetPasswordRequest = class;
  TIdentitytoolkitRelyingpartyResetPasswordRequestArray = Array of TIdentitytoolkitRelyingpartyResetPasswordRequest;
  TIdentitytoolkitRelyingpartySetAccountInfoRequest = class;
  TIdentitytoolkitRelyingpartySetAccountInfoRequestArray = Array of TIdentitytoolkitRelyingpartySetAccountInfoRequest;
  TIdentitytoolkitRelyingpartySetAccountInfoRequestprovider = class;
  TIdentitytoolkitRelyingpartySetAccountInfoRequestproviderArray = Array of TIdentitytoolkitRelyingpartySetAccountInfoRequestprovider;
  TIdentitytoolkitRelyingpartyUploadAccountRequest = class;
  TIdentitytoolkitRelyingpartyUploadAccountRequestArray = Array of TIdentitytoolkitRelyingpartyUploadAccountRequest;
  TIdentitytoolkitRelyingpartyUploadAccountRequestusers = class;
  TIdentitytoolkitRelyingpartyUploadAccountRequestusersArray = Array of TIdentitytoolkitRelyingpartyUploadAccountRequestusers;
  TIdentitytoolkitRelyingpartyVerifyAssertionRequest = class;
  TIdentitytoolkitRelyingpartyVerifyAssertionRequestArray = Array of TIdentitytoolkitRelyingpartyVerifyAssertionRequest;
  TIdentitytoolkitRelyingpartyVerifyPasswordRequest = class;
  TIdentitytoolkitRelyingpartyVerifyPasswordRequestArray = Array of TIdentitytoolkitRelyingpartyVerifyPasswordRequest;
  TRelyingparty = class;
  TRelyingpartyArray = Array of TRelyingparty;
  TResetPasswordResponse = class;
  TResetPasswordResponseArray = Array of TResetPasswordResponse;
  TSetAccountInfoResponse = class;
  TSetAccountInfoResponseArray = Array of TSetAccountInfoResponse;
  TSetAccountInfoResponseproviderUserInfo = class;
  TSetAccountInfoResponseproviderUserInfoArray = Array of TSetAccountInfoResponseproviderUserInfo;
  TUploadAccountResponse = class;
  TUploadAccountResponseArray = Array of TUploadAccountResponse;
  TUploadAccountResponseerror = class;
  TUploadAccountResponseerrorArray = Array of TUploadAccountResponseerror;
  TUserInfo = class;
  TUserInfoArray = Array of TUserInfo;
  TUserInfoproviderUserInfo = class;
  TUserInfoproviderUserInfoArray = Array of TUserInfoproviderUserInfo;
  TVerifyAssertionResponse = class;
  TVerifyAssertionResponseArray = Array of TVerifyAssertionResponse;
  TVerifyAssertionResponseverifiedProvider = class;
  TVerifyAssertionResponseverifiedProviderArray = Array of TVerifyAssertionResponseverifiedProvider;
  TVerifyPasswordResponse = class;
  TVerifyPasswordResponseArray = Array of TVerifyPasswordResponse;
  
  { --------------------------------------------------------------------
    TCreateAuthUriResponse
    --------------------------------------------------------------------}
  
  TCreateAuthUriResponse = Class(TGoogleBaseObject)
  Private
    FauthUri : string;
    FcaptchaRequired : boolean;
    FforExistingProvider : boolean;
    Fkind : string;
    FproviderId : string;
    Fregistered : boolean;
  Protected
    //Property setters
    Procedure SetauthUri(AIndex : Integer; AValue : string); virtual;
    Procedure SetcaptchaRequired(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetforExistingProvider(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetproviderId(AIndex : Integer; AValue : string); virtual;
    Procedure Setregistered(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property authUri : string Index 0 Read FauthUri Write SetauthUri;
    Property captchaRequired : boolean Index 8 Read FcaptchaRequired Write SetcaptchaRequired;
    Property forExistingProvider : boolean Index 16 Read FforExistingProvider Write SetforExistingProvider;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property providerId : string Index 32 Read FproviderId Write SetproviderId;
    Property registered : boolean Index 40 Read Fregistered Write Setregistered;
  end;
  TCreateAuthUriResponseClass = Class of TCreateAuthUriResponse;
  
  { --------------------------------------------------------------------
    TDeleteAccountResponse
    --------------------------------------------------------------------}
  
  TDeleteAccountResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
  end;
  TDeleteAccountResponseClass = Class of TDeleteAccountResponse;
  
  { --------------------------------------------------------------------
    TDownloadAccountResponse
    --------------------------------------------------------------------}
  
  TDownloadAccountResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    FnextPageToken : string;
    Fusers : TDownloadAccountResponseusers;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setusers(AIndex : Integer; AValue : TDownloadAccountResponseusers); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
    Property users : TDownloadAccountResponseusers Index 16 Read Fusers Write Setusers;
  end;
  TDownloadAccountResponseClass = Class of TDownloadAccountResponse;
  
  { --------------------------------------------------------------------
    TDownloadAccountResponseusers
    --------------------------------------------------------------------}
  
  TDownloadAccountResponseusers = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDownloadAccountResponseusersClass = Class of TDownloadAccountResponseusers;
  
  { --------------------------------------------------------------------
    TGetAccountInfoResponse
    --------------------------------------------------------------------}
  
  TGetAccountInfoResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    Fusers : TGetAccountInfoResponseusers;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setusers(AIndex : Integer; AValue : TGetAccountInfoResponseusers); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property users : TGetAccountInfoResponseusers Index 8 Read Fusers Write Setusers;
  end;
  TGetAccountInfoResponseClass = Class of TGetAccountInfoResponse;
  
  { --------------------------------------------------------------------
    TGetAccountInfoResponseusers
    --------------------------------------------------------------------}
  
  TGetAccountInfoResponseusers = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGetAccountInfoResponseusersClass = Class of TGetAccountInfoResponseusers;
  
  { --------------------------------------------------------------------
    TGetOobConfirmationCodeResponse
    --------------------------------------------------------------------}
  
  TGetOobConfirmationCodeResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    FoobCode : string;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetoobCode(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property oobCode : string Index 8 Read FoobCode Write SetoobCode;
  end;
  TGetOobConfirmationCodeResponseClass = Class of TGetOobConfirmationCodeResponse;
  
  { --------------------------------------------------------------------
    TGetRecaptchaParamResponse
    --------------------------------------------------------------------}
  
  TGetRecaptchaParamResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    FrecaptchaSiteKey : string;
    FrecaptchaStoken : string;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetrecaptchaSiteKey(AIndex : Integer; AValue : string); virtual;
    Procedure SetrecaptchaStoken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property recaptchaSiteKey : string Index 8 Read FrecaptchaSiteKey Write SetrecaptchaSiteKey;
    Property recaptchaStoken : string Index 16 Read FrecaptchaStoken Write SetrecaptchaStoken;
  end;
  TGetRecaptchaParamResponseClass = Class of TGetRecaptchaParamResponse;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyCreateAuthUriRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyCreateAuthUriRequest = Class(TGoogleBaseObject)
  Private
    FappId : string;
    FclientId : string;
    Fcontext : string;
    FcontinueUri : string;
    Fidentifier : string;
    FoauthConsumerKey : string;
    FoauthScope : string;
    FopenidRealm : string;
    FotaApp : string;
    FproviderId : string;
  Protected
    //Property setters
    Procedure SetappId(AIndex : Integer; AValue : string); virtual;
    Procedure SetclientId(AIndex : Integer; AValue : string); virtual;
    Procedure Setcontext(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontinueUri(AIndex : Integer; AValue : string); virtual;
    Procedure Setidentifier(AIndex : Integer; AValue : string); virtual;
    Procedure SetoauthConsumerKey(AIndex : Integer; AValue : string); virtual;
    Procedure SetoauthScope(AIndex : Integer; AValue : string); virtual;
    Procedure SetopenidRealm(AIndex : Integer; AValue : string); virtual;
    Procedure SetotaApp(AIndex : Integer; AValue : string); virtual;
    Procedure SetproviderId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property appId : string Index 0 Read FappId Write SetappId;
    Property clientId : string Index 8 Read FclientId Write SetclientId;
    Property context : string Index 16 Read Fcontext Write Setcontext;
    Property continueUri : string Index 24 Read FcontinueUri Write SetcontinueUri;
    Property identifier : string Index 32 Read Fidentifier Write Setidentifier;
    Property oauthConsumerKey : string Index 40 Read FoauthConsumerKey Write SetoauthConsumerKey;
    Property oauthScope : string Index 48 Read FoauthScope Write SetoauthScope;
    Property openidRealm : string Index 56 Read FopenidRealm Write SetopenidRealm;
    Property otaApp : string Index 64 Read FotaApp Write SetotaApp;
    Property providerId : string Index 72 Read FproviderId Write SetproviderId;
  end;
  TIdentitytoolkitRelyingpartyCreateAuthUriRequestClass = Class of TIdentitytoolkitRelyingpartyCreateAuthUriRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyDeleteAccountRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyDeleteAccountRequest = Class(TGoogleBaseObject)
  Private
    FlocalId : string;
  Protected
    //Property setters
    Procedure SetlocalId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property localId : string Index 0 Read FlocalId Write SetlocalId;
  end;
  TIdentitytoolkitRelyingpartyDeleteAccountRequestClass = Class of TIdentitytoolkitRelyingpartyDeleteAccountRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyDownloadAccountRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyDownloadAccountRequest = Class(TGoogleBaseObject)
  Private
    FmaxResults : integer;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure SetmaxResults(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property maxResults : integer Index 0 Read FmaxResults Write SetmaxResults;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TIdentitytoolkitRelyingpartyDownloadAccountRequestClass = Class of TIdentitytoolkitRelyingpartyDownloadAccountRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyGetAccountInfoRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyGetAccountInfoRequest = Class(TGoogleBaseObject)
  Private
    Femail : TIdentitytoolkitRelyingpartyGetAccountInfoRequestemail;
    FidToken : string;
    FlocalId : TIdentitytoolkitRelyingpartyGetAccountInfoRequestlocalId;
  Protected
    //Property setters
    Procedure Setemail(AIndex : Integer; AValue : TIdentitytoolkitRelyingpartyGetAccountInfoRequestemail); virtual;
    Procedure SetidToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetlocalId(AIndex : Integer; AValue : TIdentitytoolkitRelyingpartyGetAccountInfoRequestlocalId); virtual;
  Public
  Published
    Property email : TIdentitytoolkitRelyingpartyGetAccountInfoRequestemail Index 0 Read Femail Write Setemail;
    Property idToken : string Index 8 Read FidToken Write SetidToken;
    Property localId : TIdentitytoolkitRelyingpartyGetAccountInfoRequestlocalId Index 16 Read FlocalId Write SetlocalId;
  end;
  TIdentitytoolkitRelyingpartyGetAccountInfoRequestClass = Class of TIdentitytoolkitRelyingpartyGetAccountInfoRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyGetAccountInfoRequestemail
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyGetAccountInfoRequestemail = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TIdentitytoolkitRelyingpartyGetAccountInfoRequestemailClass = Class of TIdentitytoolkitRelyingpartyGetAccountInfoRequestemail;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyGetAccountInfoRequestlocalId
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyGetAccountInfoRequestlocalId = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TIdentitytoolkitRelyingpartyGetAccountInfoRequestlocalIdClass = Class of TIdentitytoolkitRelyingpartyGetAccountInfoRequestlocalId;
  
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
    Femail : string;
    FnewPassword : string;
    FoldPassword : string;
    FoobCode : string;
  Protected
    //Property setters
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure SetnewPassword(AIndex : Integer; AValue : string); virtual;
    Procedure SetoldPassword(AIndex : Integer; AValue : string); virtual;
    Procedure SetoobCode(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property email : string Index 0 Read Femail Write Setemail;
    Property newPassword : string Index 8 Read FnewPassword Write SetnewPassword;
    Property oldPassword : string Index 16 Read FoldPassword Write SetoldPassword;
    Property oobCode : string Index 24 Read FoobCode Write SetoobCode;
  end;
  TIdentitytoolkitRelyingpartyResetPasswordRequestClass = Class of TIdentitytoolkitRelyingpartyResetPasswordRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartySetAccountInfoRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartySetAccountInfoRequest = Class(TGoogleBaseObject)
  Private
    FcaptchaChallenge : string;
    FcaptchaResponse : string;
    FdisplayName : string;
    Femail : string;
    FemailVerified : boolean;
    FidToken : string;
    FlocalId : string;
    FoobCode : string;
    Fpassword : string;
    Fprovider : TIdentitytoolkitRelyingpartySetAccountInfoRequestprovider;
    FupgradeToFederatedLogin : boolean;
  Protected
    //Property setters
    Procedure SetcaptchaChallenge(AIndex : Integer; AValue : string); virtual;
    Procedure SetcaptchaResponse(AIndex : Integer; AValue : string); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure SetemailVerified(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetidToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetlocalId(AIndex : Integer; AValue : string); virtual;
    Procedure SetoobCode(AIndex : Integer; AValue : string); virtual;
    Procedure Setpassword(AIndex : Integer; AValue : string); virtual;
    Procedure Setprovider(AIndex : Integer; AValue : TIdentitytoolkitRelyingpartySetAccountInfoRequestprovider); virtual;
    Procedure SetupgradeToFederatedLogin(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property captchaChallenge : string Index 0 Read FcaptchaChallenge Write SetcaptchaChallenge;
    Property captchaResponse : string Index 8 Read FcaptchaResponse Write SetcaptchaResponse;
    Property displayName : string Index 16 Read FdisplayName Write SetdisplayName;
    Property email : string Index 24 Read Femail Write Setemail;
    Property emailVerified : boolean Index 32 Read FemailVerified Write SetemailVerified;
    Property idToken : string Index 40 Read FidToken Write SetidToken;
    Property localId : string Index 48 Read FlocalId Write SetlocalId;
    Property oobCode : string Index 56 Read FoobCode Write SetoobCode;
    Property password : string Index 64 Read Fpassword Write Setpassword;
    Property provider : TIdentitytoolkitRelyingpartySetAccountInfoRequestprovider Index 72 Read Fprovider Write Setprovider;
    Property upgradeToFederatedLogin : boolean Index 80 Read FupgradeToFederatedLogin Write SetupgradeToFederatedLogin;
  end;
  TIdentitytoolkitRelyingpartySetAccountInfoRequestClass = Class of TIdentitytoolkitRelyingpartySetAccountInfoRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartySetAccountInfoRequestprovider
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartySetAccountInfoRequestprovider = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TIdentitytoolkitRelyingpartySetAccountInfoRequestproviderClass = Class of TIdentitytoolkitRelyingpartySetAccountInfoRequestprovider;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyUploadAccountRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyUploadAccountRequest = Class(TGoogleBaseObject)
  Private
    FhashAlgorithm : string;
    FmemoryCost : integer;
    Frounds : integer;
    FsaltSeparator : string;
    FsignerKey : string;
    Fusers : TIdentitytoolkitRelyingpartyUploadAccountRequestusers;
  Protected
    //Property setters
    Procedure SethashAlgorithm(AIndex : Integer; AValue : string); virtual;
    Procedure SetmemoryCost(AIndex : Integer; AValue : integer); virtual;
    Procedure Setrounds(AIndex : Integer; AValue : integer); virtual;
    Procedure SetsaltSeparator(AIndex : Integer; AValue : string); virtual;
    Procedure SetsignerKey(AIndex : Integer; AValue : string); virtual;
    Procedure Setusers(AIndex : Integer; AValue : TIdentitytoolkitRelyingpartyUploadAccountRequestusers); virtual;
  Public
  Published
    Property hashAlgorithm : string Index 0 Read FhashAlgorithm Write SethashAlgorithm;
    Property memoryCost : integer Index 8 Read FmemoryCost Write SetmemoryCost;
    Property rounds : integer Index 16 Read Frounds Write Setrounds;
    Property saltSeparator : string Index 24 Read FsaltSeparator Write SetsaltSeparator;
    Property signerKey : string Index 32 Read FsignerKey Write SetsignerKey;
    Property users : TIdentitytoolkitRelyingpartyUploadAccountRequestusers Index 40 Read Fusers Write Setusers;
  end;
  TIdentitytoolkitRelyingpartyUploadAccountRequestClass = Class of TIdentitytoolkitRelyingpartyUploadAccountRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyUploadAccountRequestusers
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyUploadAccountRequestusers = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TIdentitytoolkitRelyingpartyUploadAccountRequestusersClass = Class of TIdentitytoolkitRelyingpartyUploadAccountRequestusers;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyVerifyAssertionRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyVerifyAssertionRequest = Class(TGoogleBaseObject)
  Private
    FpendingIdToken : string;
    FpostBody : string;
    FrequestUri : string;
    FreturnRefreshToken : boolean;
  Protected
    //Property setters
    Procedure SetpendingIdToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetpostBody(AIndex : Integer; AValue : string); virtual;
    Procedure SetrequestUri(AIndex : Integer; AValue : string); virtual;
    Procedure SetreturnRefreshToken(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property pendingIdToken : string Index 0 Read FpendingIdToken Write SetpendingIdToken;
    Property postBody : string Index 8 Read FpostBody Write SetpostBody;
    Property requestUri : string Index 16 Read FrequestUri Write SetrequestUri;
    Property returnRefreshToken : boolean Index 24 Read FreturnRefreshToken Write SetreturnRefreshToken;
  end;
  TIdentitytoolkitRelyingpartyVerifyAssertionRequestClass = Class of TIdentitytoolkitRelyingpartyVerifyAssertionRequest;
  
  { --------------------------------------------------------------------
    TIdentitytoolkitRelyingpartyVerifyPasswordRequest
    --------------------------------------------------------------------}
  
  TIdentitytoolkitRelyingpartyVerifyPasswordRequest = Class(TGoogleBaseObject)
  Private
    FcaptchaChallenge : string;
    FcaptchaResponse : string;
    Femail : string;
    Fpassword : string;
    FpendingIdToken : string;
  Protected
    //Property setters
    Procedure SetcaptchaChallenge(AIndex : Integer; AValue : string); virtual;
    Procedure SetcaptchaResponse(AIndex : Integer; AValue : string); virtual;
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure Setpassword(AIndex : Integer; AValue : string); virtual;
    Procedure SetpendingIdToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property captchaChallenge : string Index 0 Read FcaptchaChallenge Write SetcaptchaChallenge;
    Property captchaResponse : string Index 8 Read FcaptchaResponse Write SetcaptchaResponse;
    Property email : string Index 16 Read Femail Write Setemail;
    Property password : string Index 24 Read Fpassword Write Setpassword;
    Property pendingIdToken : string Index 32 Read FpendingIdToken Write SetpendingIdToken;
  end;
  TIdentitytoolkitRelyingpartyVerifyPasswordRequestClass = Class of TIdentitytoolkitRelyingpartyVerifyPasswordRequest;
  
  { --------------------------------------------------------------------
    TRelyingparty
    --------------------------------------------------------------------}
  
  TRelyingparty = Class(TGoogleBaseObject)
  Private
    FcaptchaResp : string;
    Fchallenge : string;
    Femail : string;
    FidToken : string;
    Fkind : string;
    FnewEmail : string;
    FrequestType : string;
    FuserIp : string;
  Protected
    //Property setters
    Procedure SetcaptchaResp(AIndex : Integer; AValue : string); virtual;
    Procedure Setchallenge(AIndex : Integer; AValue : string); virtual;
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure SetidToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnewEmail(AIndex : Integer; AValue : string); virtual;
    Procedure SetrequestType(AIndex : Integer; AValue : string); virtual;
    Procedure SetuserIp(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property captchaResp : string Index 0 Read FcaptchaResp Write SetcaptchaResp;
    Property challenge : string Index 8 Read Fchallenge Write Setchallenge;
    Property email : string Index 16 Read Femail Write Setemail;
    Property idToken : string Index 24 Read FidToken Write SetidToken;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property newEmail : string Index 40 Read FnewEmail Write SetnewEmail;
    Property requestType : string Index 48 Read FrequestType Write SetrequestType;
    Property userIp : string Index 56 Read FuserIp Write SetuserIp;
  end;
  TRelyingpartyClass = Class of TRelyingparty;
  
  { --------------------------------------------------------------------
    TResetPasswordResponse
    --------------------------------------------------------------------}
  
  TResetPasswordResponse = Class(TGoogleBaseObject)
  Private
    Femail : string;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property email : string Index 0 Read Femail Write Setemail;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TResetPasswordResponseClass = Class of TResetPasswordResponse;
  
  { --------------------------------------------------------------------
    TSetAccountInfoResponse
    --------------------------------------------------------------------}
  
  TSetAccountInfoResponse = Class(TGoogleBaseObject)
  Private
    FdisplayName : string;
    Femail : string;
    FidToken : string;
    Fkind : string;
    FproviderUserInfo : TSetAccountInfoResponseproviderUserInfo;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure SetidToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetproviderUserInfo(AIndex : Integer; AValue : TSetAccountInfoResponseproviderUserInfo); virtual;
  Public
  Published
    Property displayName : string Index 0 Read FdisplayName Write SetdisplayName;
    Property email : string Index 8 Read Femail Write Setemail;
    Property idToken : string Index 16 Read FidToken Write SetidToken;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property providerUserInfo : TSetAccountInfoResponseproviderUserInfo Index 32 Read FproviderUserInfo Write SetproviderUserInfo;
  end;
  TSetAccountInfoResponseClass = Class of TSetAccountInfoResponse;
  
  { --------------------------------------------------------------------
    TSetAccountInfoResponseproviderUserInfo
    --------------------------------------------------------------------}
  
  TSetAccountInfoResponseproviderUserInfo = Class(TGoogleBaseObject)
  Private
    FdisplayName : string;
    FphotoUrl : string;
    FproviderId : string;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure SetphotoUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetproviderId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property displayName : string Index 0 Read FdisplayName Write SetdisplayName;
    Property photoUrl : string Index 8 Read FphotoUrl Write SetphotoUrl;
    Property providerId : string Index 16 Read FproviderId Write SetproviderId;
  end;
  TSetAccountInfoResponseproviderUserInfoClass = Class of TSetAccountInfoResponseproviderUserInfo;
  
  { --------------------------------------------------------------------
    TUploadAccountResponse
    --------------------------------------------------------------------}
  
  TUploadAccountResponse = Class(TGoogleBaseObject)
  Private
    Ferror : TUploadAccountResponseerror;
    Fkind : string;
  Protected
    //Property setters
    Procedure Seterror(AIndex : Integer; AValue : TUploadAccountResponseerror); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property error : TUploadAccountResponseerror Index 0 Read Ferror Write Seterror;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TUploadAccountResponseClass = Class of TUploadAccountResponse;
  
  { --------------------------------------------------------------------
    TUploadAccountResponseerror
    --------------------------------------------------------------------}
  
  TUploadAccountResponseerror = Class(TGoogleBaseObject)
  Private
    Findex : integer;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setindex(AIndex : Integer; AValue : integer); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property index : integer Index 0 Read Findex Write Setindex;
    Property message : string Index 8 Read Fmessage Write Setmessage;
  end;
  TUploadAccountResponseerrorClass = Class of TUploadAccountResponseerror;
  
  { --------------------------------------------------------------------
    TUserInfo
    --------------------------------------------------------------------}
  
  TUserInfo = Class(TGoogleBaseObject)
  Private
    FdisplayName : string;
    Femail : string;
    FemailVerified : boolean;
    FlocalId : string;
    FpasswordHash : string;
    FpasswordUpdatedAt : double;
    FphotoUrl : string;
    FproviderUserInfo : TUserInfoproviderUserInfo;
    Fsalt : string;
    Fversion : integer;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure SetemailVerified(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetlocalId(AIndex : Integer; AValue : string); virtual;
    Procedure SetpasswordHash(AIndex : Integer; AValue : string); virtual;
    Procedure SetpasswordUpdatedAt(AIndex : Integer; AValue : double); virtual;
    Procedure SetphotoUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetproviderUserInfo(AIndex : Integer; AValue : TUserInfoproviderUserInfo); virtual;
    Procedure Setsalt(AIndex : Integer; AValue : string); virtual;
    Procedure Setversion(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property displayName : string Index 0 Read FdisplayName Write SetdisplayName;
    Property email : string Index 8 Read Femail Write Setemail;
    Property emailVerified : boolean Index 16 Read FemailVerified Write SetemailVerified;
    Property localId : string Index 24 Read FlocalId Write SetlocalId;
    Property passwordHash : string Index 32 Read FpasswordHash Write SetpasswordHash;
    Property passwordUpdatedAt : double Index 40 Read FpasswordUpdatedAt Write SetpasswordUpdatedAt;
    Property photoUrl : string Index 48 Read FphotoUrl Write SetphotoUrl;
    Property providerUserInfo : TUserInfoproviderUserInfo Index 56 Read FproviderUserInfo Write SetproviderUserInfo;
    Property salt : string Index 64 Read Fsalt Write Setsalt;
    Property version : integer Index 72 Read Fversion Write Setversion;
  end;
  TUserInfoClass = Class of TUserInfo;
  
  { --------------------------------------------------------------------
    TUserInfoproviderUserInfo
    --------------------------------------------------------------------}
  
  TUserInfoproviderUserInfo = Class(TGoogleBaseObject)
  Private
    FdisplayName : string;
    FfederatedId : string;
    FphotoUrl : string;
    FproviderId : string;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure SetfederatedId(AIndex : Integer; AValue : string); virtual;
    Procedure SetphotoUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetproviderId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property displayName : string Index 0 Read FdisplayName Write SetdisplayName;
    Property federatedId : string Index 8 Read FfederatedId Write SetfederatedId;
    Property photoUrl : string Index 16 Read FphotoUrl Write SetphotoUrl;
    Property providerId : string Index 24 Read FproviderId Write SetproviderId;
  end;
  TUserInfoproviderUserInfoClass = Class of TUserInfoproviderUserInfo;
  
  { --------------------------------------------------------------------
    TVerifyAssertionResponse
    --------------------------------------------------------------------}
  
  TVerifyAssertionResponse = Class(TGoogleBaseObject)
  Private
    Faction : string;
    FappInstallationUrl : string;
    FappScheme : string;
    Fcontext : string;
    FdateOfBirth : string;
    FdisplayName : string;
    Femail : string;
    FemailRecycled : boolean;
    FemailVerified : boolean;
    FfederatedId : string;
    FfirstName : string;
    FfullName : string;
    FidToken : string;
    FinputEmail : string;
    Fkind : string;
    Flanguage : string;
    FlastName : string;
    FlocalId : string;
    FneedConfirmation : boolean;
    FnickName : string;
    FoauthAccessToken : string;
    FoauthAuthorizationCode : string;
    FoauthExpireIn : integer;
    FoauthRequestToken : string;
    FoauthScope : string;
    ForiginalEmail : string;
    FphotoUrl : string;
    FproviderId : string;
    FtimeZone : string;
    FverifiedProvider : TVerifyAssertionResponseverifiedProvider;
  Protected
    //Property setters
    Procedure Setaction(AIndex : Integer; AValue : string); virtual;
    Procedure SetappInstallationUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetappScheme(AIndex : Integer; AValue : string); virtual;
    Procedure Setcontext(AIndex : Integer; AValue : string); virtual;
    Procedure SetdateOfBirth(AIndex : Integer; AValue : string); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure SetemailRecycled(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetemailVerified(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetfederatedId(AIndex : Integer; AValue : string); virtual;
    Procedure SetfirstName(AIndex : Integer; AValue : string); virtual;
    Procedure SetfullName(AIndex : Integer; AValue : string); virtual;
    Procedure SetidToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetinputEmail(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlanguage(AIndex : Integer; AValue : string); virtual;
    Procedure SetlastName(AIndex : Integer; AValue : string); virtual;
    Procedure SetlocalId(AIndex : Integer; AValue : string); virtual;
    Procedure SetneedConfirmation(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetnickName(AIndex : Integer; AValue : string); virtual;
    Procedure SetoauthAccessToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetoauthAuthorizationCode(AIndex : Integer; AValue : string); virtual;
    Procedure SetoauthExpireIn(AIndex : Integer; AValue : integer); virtual;
    Procedure SetoauthRequestToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetoauthScope(AIndex : Integer; AValue : string); virtual;
    Procedure SetoriginalEmail(AIndex : Integer; AValue : string); virtual;
    Procedure SetphotoUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetproviderId(AIndex : Integer; AValue : string); virtual;
    Procedure SettimeZone(AIndex : Integer; AValue : string); virtual;
    Procedure SetverifiedProvider(AIndex : Integer; AValue : TVerifyAssertionResponseverifiedProvider); virtual;
  Public
  Published
    Property action : string Index 0 Read Faction Write Setaction;
    Property appInstallationUrl : string Index 8 Read FappInstallationUrl Write SetappInstallationUrl;
    Property appScheme : string Index 16 Read FappScheme Write SetappScheme;
    Property context : string Index 24 Read Fcontext Write Setcontext;
    Property dateOfBirth : string Index 32 Read FdateOfBirth Write SetdateOfBirth;
    Property displayName : string Index 40 Read FdisplayName Write SetdisplayName;
    Property email : string Index 48 Read Femail Write Setemail;
    Property emailRecycled : boolean Index 56 Read FemailRecycled Write SetemailRecycled;
    Property emailVerified : boolean Index 64 Read FemailVerified Write SetemailVerified;
    Property federatedId : string Index 72 Read FfederatedId Write SetfederatedId;
    Property firstName : string Index 80 Read FfirstName Write SetfirstName;
    Property fullName : string Index 88 Read FfullName Write SetfullName;
    Property idToken : string Index 96 Read FidToken Write SetidToken;
    Property inputEmail : string Index 104 Read FinputEmail Write SetinputEmail;
    Property kind : string Index 112 Read Fkind Write Setkind;
    Property language : string Index 120 Read Flanguage Write Setlanguage;
    Property lastName : string Index 128 Read FlastName Write SetlastName;
    Property localId : string Index 136 Read FlocalId Write SetlocalId;
    Property needConfirmation : boolean Index 144 Read FneedConfirmation Write SetneedConfirmation;
    Property nickName : string Index 152 Read FnickName Write SetnickName;
    Property oauthAccessToken : string Index 160 Read FoauthAccessToken Write SetoauthAccessToken;
    Property oauthAuthorizationCode : string Index 168 Read FoauthAuthorizationCode Write SetoauthAuthorizationCode;
    Property oauthExpireIn : integer Index 176 Read FoauthExpireIn Write SetoauthExpireIn;
    Property oauthRequestToken : string Index 184 Read FoauthRequestToken Write SetoauthRequestToken;
    Property oauthScope : string Index 192 Read FoauthScope Write SetoauthScope;
    Property originalEmail : string Index 200 Read ForiginalEmail Write SetoriginalEmail;
    Property photoUrl : string Index 208 Read FphotoUrl Write SetphotoUrl;
    Property providerId : string Index 216 Read FproviderId Write SetproviderId;
    Property timeZone : string Index 224 Read FtimeZone Write SettimeZone;
    Property verifiedProvider : TVerifyAssertionResponseverifiedProvider Index 232 Read FverifiedProvider Write SetverifiedProvider;
  end;
  TVerifyAssertionResponseClass = Class of TVerifyAssertionResponse;
  
  { --------------------------------------------------------------------
    TVerifyAssertionResponseverifiedProvider
    --------------------------------------------------------------------}
  
  TVerifyAssertionResponseverifiedProvider = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVerifyAssertionResponseverifiedProviderClass = Class of TVerifyAssertionResponseverifiedProvider;
  
  { --------------------------------------------------------------------
    TVerifyPasswordResponse
    --------------------------------------------------------------------}
  
  TVerifyPasswordResponse = Class(TGoogleBaseObject)
  Private
    FdisplayName : string;
    Femail : string;
    FidToken : string;
    Fkind : string;
    FlocalId : string;
    FphotoUrl : string;
    Fregistered : boolean;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure SetidToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetlocalId(AIndex : Integer; AValue : string); virtual;
    Procedure SetphotoUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setregistered(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property displayName : string Index 0 Read FdisplayName Write SetdisplayName;
    Property email : string Index 8 Read Femail Write Setemail;
    Property idToken : string Index 16 Read FidToken Write SetidToken;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property localId : string Index 32 Read FlocalId Write SetlocalId;
    Property photoUrl : string Index 40 Read FphotoUrl Write SetphotoUrl;
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


Procedure TCreateAuthUriResponse.SetauthUri(AIndex : Integer; AValue : string); 

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



Procedure TCreateAuthUriResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateAuthUriResponse.SetproviderId(AIndex : Integer; AValue : string); 

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


Procedure TDeleteAccountResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDownloadAccountResponse
  --------------------------------------------------------------------}


Procedure TDownloadAccountResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccountResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadAccountResponse.Setusers(AIndex : Integer; AValue : TDownloadAccountResponseusers); 

begin
  If (Fusers=AValue) then exit;
  Fusers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDownloadAccountResponseusers
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGetAccountInfoResponse
  --------------------------------------------------------------------}


Procedure TGetAccountInfoResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetAccountInfoResponse.Setusers(AIndex : Integer; AValue : TGetAccountInfoResponseusers); 

begin
  If (Fusers=AValue) then exit;
  Fusers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGetAccountInfoResponseusers
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGetOobConfirmationCodeResponse
  --------------------------------------------------------------------}


Procedure TGetOobConfirmationCodeResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetOobConfirmationCodeResponse.SetoobCode(AIndex : Integer; AValue : string); 

begin
  If (FoobCode=AValue) then exit;
  FoobCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGetRecaptchaParamResponse
  --------------------------------------------------------------------}


Procedure TGetRecaptchaParamResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetRecaptchaParamResponse.SetrecaptchaSiteKey(AIndex : Integer; AValue : string); 

begin
  If (FrecaptchaSiteKey=AValue) then exit;
  FrecaptchaSiteKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetRecaptchaParamResponse.SetrecaptchaStoken(AIndex : Integer; AValue : string); 

begin
  If (FrecaptchaStoken=AValue) then exit;
  FrecaptchaStoken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyCreateAuthUriRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetappId(AIndex : Integer; AValue : string); 

begin
  If (FappId=AValue) then exit;
  FappId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetclientId(AIndex : Integer; AValue : string); 

begin
  If (FclientId=AValue) then exit;
  FclientId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.Setcontext(AIndex : Integer; AValue : string); 

begin
  If (Fcontext=AValue) then exit;
  Fcontext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetcontinueUri(AIndex : Integer; AValue : string); 

begin
  If (FcontinueUri=AValue) then exit;
  FcontinueUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.Setidentifier(AIndex : Integer; AValue : string); 

begin
  If (Fidentifier=AValue) then exit;
  Fidentifier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetoauthConsumerKey(AIndex : Integer; AValue : string); 

begin
  If (FoauthConsumerKey=AValue) then exit;
  FoauthConsumerKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetoauthScope(AIndex : Integer; AValue : string); 

begin
  If (FoauthScope=AValue) then exit;
  FoauthScope:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetopenidRealm(AIndex : Integer; AValue : string); 

begin
  If (FopenidRealm=AValue) then exit;
  FopenidRealm:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetotaApp(AIndex : Integer; AValue : string); 

begin
  If (FotaApp=AValue) then exit;
  FotaApp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyCreateAuthUriRequest.SetproviderId(AIndex : Integer; AValue : string); 

begin
  If (FproviderId=AValue) then exit;
  FproviderId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyDeleteAccountRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartyDeleteAccountRequest.SetlocalId(AIndex : Integer; AValue : string); 

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



Procedure TIdentitytoolkitRelyingpartyDownloadAccountRequest.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyGetAccountInfoRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartyGetAccountInfoRequest.Setemail(AIndex : Integer; AValue : TIdentitytoolkitRelyingpartyGetAccountInfoRequestemail); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyGetAccountInfoRequest.SetidToken(AIndex : Integer; AValue : string); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyGetAccountInfoRequest.SetlocalId(AIndex : Integer; AValue : TIdentitytoolkitRelyingpartyGetAccountInfoRequestlocalId); 

begin
  If (FlocalId=AValue) then exit;
  FlocalId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyGetAccountInfoRequestemail
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyGetAccountInfoRequestlocalId
  --------------------------------------------------------------------}




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


Procedure TIdentitytoolkitRelyingpartyResetPasswordRequest.Setemail(AIndex : Integer; AValue : string); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyResetPasswordRequest.SetnewPassword(AIndex : Integer; AValue : string); 

begin
  If (FnewPassword=AValue) then exit;
  FnewPassword:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyResetPasswordRequest.SetoldPassword(AIndex : Integer; AValue : string); 

begin
  If (FoldPassword=AValue) then exit;
  FoldPassword:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyResetPasswordRequest.SetoobCode(AIndex : Integer; AValue : string); 

begin
  If (FoobCode=AValue) then exit;
  FoobCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartySetAccountInfoRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetcaptchaChallenge(AIndex : Integer; AValue : string); 

begin
  If (FcaptchaChallenge=AValue) then exit;
  FcaptchaChallenge:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetcaptchaResponse(AIndex : Integer; AValue : string); 

begin
  If (FcaptchaResponse=AValue) then exit;
  FcaptchaResponse:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.Setemail(AIndex : Integer; AValue : string); 

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



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetidToken(AIndex : Integer; AValue : string); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetlocalId(AIndex : Integer; AValue : string); 

begin
  If (FlocalId=AValue) then exit;
  FlocalId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.SetoobCode(AIndex : Integer; AValue : string); 

begin
  If (FoobCode=AValue) then exit;
  FoobCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.Setpassword(AIndex : Integer; AValue : string); 

begin
  If (Fpassword=AValue) then exit;
  Fpassword:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartySetAccountInfoRequest.Setprovider(AIndex : Integer; AValue : TIdentitytoolkitRelyingpartySetAccountInfoRequestprovider); 

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
  TIdentitytoolkitRelyingpartySetAccountInfoRequestprovider
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyUploadAccountRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartyUploadAccountRequest.SethashAlgorithm(AIndex : Integer; AValue : string); 

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



Procedure TIdentitytoolkitRelyingpartyUploadAccountRequest.SetsaltSeparator(AIndex : Integer; AValue : string); 

begin
  If (FsaltSeparator=AValue) then exit;
  FsaltSeparator:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyUploadAccountRequest.SetsignerKey(AIndex : Integer; AValue : string); 

begin
  If (FsignerKey=AValue) then exit;
  FsignerKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyUploadAccountRequest.Setusers(AIndex : Integer; AValue : TIdentitytoolkitRelyingpartyUploadAccountRequestusers); 

begin
  If (Fusers=AValue) then exit;
  Fusers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyUploadAccountRequestusers
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TIdentitytoolkitRelyingpartyVerifyAssertionRequest
  --------------------------------------------------------------------}


Procedure TIdentitytoolkitRelyingpartyVerifyAssertionRequest.SetpendingIdToken(AIndex : Integer; AValue : string); 

begin
  If (FpendingIdToken=AValue) then exit;
  FpendingIdToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyAssertionRequest.SetpostBody(AIndex : Integer; AValue : string); 

begin
  If (FpostBody=AValue) then exit;
  FpostBody:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyAssertionRequest.SetrequestUri(AIndex : Integer; AValue : string); 

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


Procedure TIdentitytoolkitRelyingpartyVerifyPasswordRequest.SetcaptchaChallenge(AIndex : Integer; AValue : string); 

begin
  If (FcaptchaChallenge=AValue) then exit;
  FcaptchaChallenge:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyPasswordRequest.SetcaptchaResponse(AIndex : Integer; AValue : string); 

begin
  If (FcaptchaResponse=AValue) then exit;
  FcaptchaResponse:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyPasswordRequest.Setemail(AIndex : Integer; AValue : string); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyPasswordRequest.Setpassword(AIndex : Integer; AValue : string); 

begin
  If (Fpassword=AValue) then exit;
  Fpassword:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIdentitytoolkitRelyingpartyVerifyPasswordRequest.SetpendingIdToken(AIndex : Integer; AValue : string); 

begin
  If (FpendingIdToken=AValue) then exit;
  FpendingIdToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRelyingparty
  --------------------------------------------------------------------}


Procedure TRelyingparty.SetcaptchaResp(AIndex : Integer; AValue : string); 

begin
  If (FcaptchaResp=AValue) then exit;
  FcaptchaResp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelyingparty.Setchallenge(AIndex : Integer; AValue : string); 

begin
  If (Fchallenge=AValue) then exit;
  Fchallenge:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelyingparty.Setemail(AIndex : Integer; AValue : string); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelyingparty.SetidToken(AIndex : Integer; AValue : string); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelyingparty.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelyingparty.SetnewEmail(AIndex : Integer; AValue : string); 

begin
  If (FnewEmail=AValue) then exit;
  FnewEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelyingparty.SetrequestType(AIndex : Integer; AValue : string); 

begin
  If (FrequestType=AValue) then exit;
  FrequestType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelyingparty.SetuserIp(AIndex : Integer; AValue : string); 

begin
  If (FuserIp=AValue) then exit;
  FuserIp:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResetPasswordResponse
  --------------------------------------------------------------------}


Procedure TResetPasswordResponse.Setemail(AIndex : Integer; AValue : string); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResetPasswordResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSetAccountInfoResponse
  --------------------------------------------------------------------}


Procedure TSetAccountInfoResponse.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponse.Setemail(AIndex : Integer; AValue : string); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponse.SetidToken(AIndex : Integer; AValue : string); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponse.SetproviderUserInfo(AIndex : Integer; AValue : TSetAccountInfoResponseproviderUserInfo); 

begin
  If (FproviderUserInfo=AValue) then exit;
  FproviderUserInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSetAccountInfoResponseproviderUserInfo
  --------------------------------------------------------------------}


Procedure TSetAccountInfoResponseproviderUserInfo.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponseproviderUserInfo.SetphotoUrl(AIndex : Integer; AValue : string); 

begin
  If (FphotoUrl=AValue) then exit;
  FphotoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetAccountInfoResponseproviderUserInfo.SetproviderId(AIndex : Integer; AValue : string); 

begin
  If (FproviderId=AValue) then exit;
  FproviderId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUploadAccountResponse
  --------------------------------------------------------------------}


Procedure TUploadAccountResponse.Seterror(AIndex : Integer; AValue : TUploadAccountResponseerror); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUploadAccountResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUploadAccountResponseerror
  --------------------------------------------------------------------}


Procedure TUploadAccountResponseerror.Setindex(AIndex : Integer; AValue : integer); 

begin
  If (Findex=AValue) then exit;
  Findex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUploadAccountResponseerror.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserInfo
  --------------------------------------------------------------------}


Procedure TUserInfo.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.Setemail(AIndex : Integer; AValue : string); 

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



Procedure TUserInfo.SetlocalId(AIndex : Integer; AValue : string); 

begin
  If (FlocalId=AValue) then exit;
  FlocalId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.SetpasswordHash(AIndex : Integer; AValue : string); 

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



Procedure TUserInfo.SetphotoUrl(AIndex : Integer; AValue : string); 

begin
  If (FphotoUrl=AValue) then exit;
  FphotoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.SetproviderUserInfo(AIndex : Integer; AValue : TUserInfoproviderUserInfo); 

begin
  If (FproviderUserInfo=AValue) then exit;
  FproviderUserInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfo.Setsalt(AIndex : Integer; AValue : string); 

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
  TUserInfoproviderUserInfo
  --------------------------------------------------------------------}


Procedure TUserInfoproviderUserInfo.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfoproviderUserInfo.SetfederatedId(AIndex : Integer; AValue : string); 

begin
  If (FfederatedId=AValue) then exit;
  FfederatedId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfoproviderUserInfo.SetphotoUrl(AIndex : Integer; AValue : string); 

begin
  If (FphotoUrl=AValue) then exit;
  FphotoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserInfoproviderUserInfo.SetproviderId(AIndex : Integer; AValue : string); 

begin
  If (FproviderId=AValue) then exit;
  FproviderId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVerifyAssertionResponse
  --------------------------------------------------------------------}


Procedure TVerifyAssertionResponse.Setaction(AIndex : Integer; AValue : string); 

begin
  If (Faction=AValue) then exit;
  Faction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetappInstallationUrl(AIndex : Integer; AValue : string); 

begin
  If (FappInstallationUrl=AValue) then exit;
  FappInstallationUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetappScheme(AIndex : Integer; AValue : string); 

begin
  If (FappScheme=AValue) then exit;
  FappScheme:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.Setcontext(AIndex : Integer; AValue : string); 

begin
  If (Fcontext=AValue) then exit;
  Fcontext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetdateOfBirth(AIndex : Integer; AValue : string); 

begin
  If (FdateOfBirth=AValue) then exit;
  FdateOfBirth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.Setemail(AIndex : Integer; AValue : string); 

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



Procedure TVerifyAssertionResponse.SetfederatedId(AIndex : Integer; AValue : string); 

begin
  If (FfederatedId=AValue) then exit;
  FfederatedId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetfirstName(AIndex : Integer; AValue : string); 

begin
  If (FfirstName=AValue) then exit;
  FfirstName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetfullName(AIndex : Integer; AValue : string); 

begin
  If (FfullName=AValue) then exit;
  FfullName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetidToken(AIndex : Integer; AValue : string); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetinputEmail(AIndex : Integer; AValue : string); 

begin
  If (FinputEmail=AValue) then exit;
  FinputEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.Setlanguage(AIndex : Integer; AValue : string); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetlastName(AIndex : Integer; AValue : string); 

begin
  If (FlastName=AValue) then exit;
  FlastName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetlocalId(AIndex : Integer; AValue : string); 

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



Procedure TVerifyAssertionResponse.SetnickName(AIndex : Integer; AValue : string); 

begin
  If (FnickName=AValue) then exit;
  FnickName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetoauthAccessToken(AIndex : Integer; AValue : string); 

begin
  If (FoauthAccessToken=AValue) then exit;
  FoauthAccessToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetoauthAuthorizationCode(AIndex : Integer; AValue : string); 

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



Procedure TVerifyAssertionResponse.SetoauthRequestToken(AIndex : Integer; AValue : string); 

begin
  If (FoauthRequestToken=AValue) then exit;
  FoauthRequestToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetoauthScope(AIndex : Integer; AValue : string); 

begin
  If (FoauthScope=AValue) then exit;
  FoauthScope:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetoriginalEmail(AIndex : Integer; AValue : string); 

begin
  If (ForiginalEmail=AValue) then exit;
  ForiginalEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetphotoUrl(AIndex : Integer; AValue : string); 

begin
  If (FphotoUrl=AValue) then exit;
  FphotoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetproviderId(AIndex : Integer; AValue : string); 

begin
  If (FproviderId=AValue) then exit;
  FproviderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SettimeZone(AIndex : Integer; AValue : string); 

begin
  If (FtimeZone=AValue) then exit;
  FtimeZone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyAssertionResponse.SetverifiedProvider(AIndex : Integer; AValue : TVerifyAssertionResponseverifiedProvider); 

begin
  If (FverifiedProvider=AValue) then exit;
  FverifiedProvider:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVerifyAssertionResponseverifiedProvider
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVerifyPasswordResponse
  --------------------------------------------------------------------}


Procedure TVerifyPasswordResponse.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyPasswordResponse.Setemail(AIndex : Integer; AValue : string); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyPasswordResponse.SetidToken(AIndex : Integer; AValue : string); 

begin
  If (FidToken=AValue) then exit;
  FidToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyPasswordResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyPasswordResponse.SetlocalId(AIndex : Integer; AValue : string); 

begin
  If (FlocalId=AValue) then exit;
  FlocalId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVerifyPasswordResponse.SetphotoUrl(AIndex : Integer; AValue : string); 

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
  TDownloadAccountResponseusers.RegisterObject;
  TGetAccountInfoResponse.RegisterObject;
  TGetAccountInfoResponseusers.RegisterObject;
  TGetOobConfirmationCodeResponse.RegisterObject;
  TGetRecaptchaParamResponse.RegisterObject;
  TIdentitytoolkitRelyingpartyCreateAuthUriRequest.RegisterObject;
  TIdentitytoolkitRelyingpartyDeleteAccountRequest.RegisterObject;
  TIdentitytoolkitRelyingpartyDownloadAccountRequest.RegisterObject;
  TIdentitytoolkitRelyingpartyGetAccountInfoRequest.RegisterObject;
  TIdentitytoolkitRelyingpartyGetAccountInfoRequestemail.RegisterObject;
  TIdentitytoolkitRelyingpartyGetAccountInfoRequestlocalId.RegisterObject;
  TIdentitytoolkitRelyingpartyGetPublicKeysResponse.RegisterObject;
  TIdentitytoolkitRelyingpartyResetPasswordRequest.RegisterObject;
  TIdentitytoolkitRelyingpartySetAccountInfoRequest.RegisterObject;
  TIdentitytoolkitRelyingpartySetAccountInfoRequestprovider.RegisterObject;
  TIdentitytoolkitRelyingpartyUploadAccountRequest.RegisterObject;
  TIdentitytoolkitRelyingpartyUploadAccountRequestusers.RegisterObject;
  TIdentitytoolkitRelyingpartyVerifyAssertionRequest.RegisterObject;
  TIdentitytoolkitRelyingpartyVerifyPasswordRequest.RegisterObject;
  TRelyingparty.RegisterObject;
  TResetPasswordResponse.RegisterObject;
  TSetAccountInfoResponse.RegisterObject;
  TSetAccountInfoResponseproviderUserInfo.RegisterObject;
  TUploadAccountResponse.RegisterObject;
  TUploadAccountResponseerror.RegisterObject;
  TUserInfo.RegisterObject;
  TUserInfoproviderUserInfo.RegisterObject;
  TVerifyAssertionResponse.RegisterObject;
  TVerifyAssertionResponseverifiedProvider.RegisterObject;
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
