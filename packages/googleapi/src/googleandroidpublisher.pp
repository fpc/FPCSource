unit googleandroidpublisher;
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
  TApk = class;
  TApkArray = Array of TApk;
  TApkBinary = class;
  TApkBinaryArray = Array of TApkBinary;
  TApkListing = class;
  TApkListingArray = Array of TApkListing;
  TApkListingsListResponse = class;
  TApkListingsListResponseArray = Array of TApkListingsListResponse;
  TApkListingsListResponselistings = class;
  TApkListingsListResponselistingsArray = Array of TApkListingsListResponselistings;
  TApksAddExternallyHostedRequest = class;
  TApksAddExternallyHostedRequestArray = Array of TApksAddExternallyHostedRequest;
  TApksAddExternallyHostedResponse = class;
  TApksAddExternallyHostedResponseArray = Array of TApksAddExternallyHostedResponse;
  TApksListResponse = class;
  TApksListResponseArray = Array of TApksListResponse;
  TApksListResponseapks = class;
  TApksListResponseapksArray = Array of TApksListResponseapks;
  TAppDetails = class;
  TAppDetailsArray = Array of TAppDetails;
  TAppEdit = class;
  TAppEditArray = Array of TAppEdit;
  TEntitlement = class;
  TEntitlementArray = Array of TEntitlement;
  TEntitlementsListResponse = class;
  TEntitlementsListResponseArray = Array of TEntitlementsListResponse;
  TEntitlementsListResponseresources = class;
  TEntitlementsListResponseresourcesArray = Array of TEntitlementsListResponseresources;
  TExpansionFile = class;
  TExpansionFileArray = Array of TExpansionFile;
  TExpansionFilesUploadResponse = class;
  TExpansionFilesUploadResponseArray = Array of TExpansionFilesUploadResponse;
  TExternallyHostedApk = class;
  TExternallyHostedApkArray = Array of TExternallyHostedApk;
  TExternallyHostedApkcertificateBase64s = class;
  TExternallyHostedApkcertificateBase64sArray = Array of TExternallyHostedApkcertificateBase64s;
  TExternallyHostedApknativeCodes = class;
  TExternallyHostedApknativeCodesArray = Array of TExternallyHostedApknativeCodes;
  TExternallyHostedApkusesFeatures = class;
  TExternallyHostedApkusesFeaturesArray = Array of TExternallyHostedApkusesFeatures;
  TExternallyHostedApkusesPermissions = class;
  TExternallyHostedApkusesPermissionsArray = Array of TExternallyHostedApkusesPermissions;
  TExternallyHostedApkUsesPermission = class;
  TExternallyHostedApkUsesPermissionArray = Array of TExternallyHostedApkUsesPermission;
  TImage = class;
  TImageArray = Array of TImage;
  TImagesDeleteAllResponse = class;
  TImagesDeleteAllResponseArray = Array of TImagesDeleteAllResponse;
  TImagesDeleteAllResponsedeleted = class;
  TImagesDeleteAllResponsedeletedArray = Array of TImagesDeleteAllResponsedeleted;
  TImagesListResponse = class;
  TImagesListResponseArray = Array of TImagesListResponse;
  TImagesListResponseimages = class;
  TImagesListResponseimagesArray = Array of TImagesListResponseimages;
  TImagesUploadResponse = class;
  TImagesUploadResponseArray = Array of TImagesUploadResponse;
  TInAppProduct = class;
  TInAppProductArray = Array of TInAppProduct;
  TInAppProductlistings = class;
  TInAppProductlistingsArray = Array of TInAppProductlistings;
  TInAppProductprices = class;
  TInAppProductpricesArray = Array of TInAppProductprices;
  TInAppProductListing = class;
  TInAppProductListingArray = Array of TInAppProductListing;
  TInappproductsBatchRequest = class;
  TInappproductsBatchRequestArray = Array of TInappproductsBatchRequest;
  TInappproductsBatchRequestentrys = class;
  TInappproductsBatchRequestentrysArray = Array of TInappproductsBatchRequestentrys;
  TInappproductsBatchRequestEntry = class;
  TInappproductsBatchRequestEntryArray = Array of TInappproductsBatchRequestEntry;
  TInappproductsBatchResponse = class;
  TInappproductsBatchResponseArray = Array of TInappproductsBatchResponse;
  TInappproductsBatchResponseentrys = class;
  TInappproductsBatchResponseentrysArray = Array of TInappproductsBatchResponseentrys;
  TInappproductsBatchResponseEntry = class;
  TInappproductsBatchResponseEntryArray = Array of TInappproductsBatchResponseEntry;
  TInappproductsInsertRequest = class;
  TInappproductsInsertRequestArray = Array of TInappproductsInsertRequest;
  TInappproductsInsertResponse = class;
  TInappproductsInsertResponseArray = Array of TInappproductsInsertResponse;
  TInappproductsListResponse = class;
  TInappproductsListResponseArray = Array of TInappproductsListResponse;
  TInappproductsListResponseinappproduct = class;
  TInappproductsListResponseinappproductArray = Array of TInappproductsListResponseinappproduct;
  TInappproductsUpdateRequest = class;
  TInappproductsUpdateRequestArray = Array of TInappproductsUpdateRequest;
  TInappproductsUpdateResponse = class;
  TInappproductsUpdateResponseArray = Array of TInappproductsUpdateResponse;
  TListing = class;
  TListingArray = Array of TListing;
  TListingsListResponse = class;
  TListingsListResponseArray = Array of TListingsListResponse;
  TListingsListResponselistings = class;
  TListingsListResponselistingsArray = Array of TListingsListResponselistings;
  TMonthDay = class;
  TMonthDayArray = Array of TMonthDay;
  TPageInfo = class;
  TPageInfoArray = Array of TPageInfo;
  TPrice = class;
  TPriceArray = Array of TPrice;
  TProductPurchase = class;
  TProductPurchaseArray = Array of TProductPurchase;
  TSeason = class;
  TSeasonArray = Array of TSeason;
  TSubscriptionDeferralInfo = class;
  TSubscriptionDeferralInfoArray = Array of TSubscriptionDeferralInfo;
  TSubscriptionPurchase = class;
  TSubscriptionPurchaseArray = Array of TSubscriptionPurchase;
  TSubscriptionPurchasesDeferRequest = class;
  TSubscriptionPurchasesDeferRequestArray = Array of TSubscriptionPurchasesDeferRequest;
  TSubscriptionPurchasesDeferResponse = class;
  TSubscriptionPurchasesDeferResponseArray = Array of TSubscriptionPurchasesDeferResponse;
  TTesters = class;
  TTestersArray = Array of TTesters;
  TTestersgoogleGroups = class;
  TTestersgoogleGroupsArray = Array of TTestersgoogleGroups;
  TTestersgooglePlusCommunities = class;
  TTestersgooglePlusCommunitiesArray = Array of TTestersgooglePlusCommunities;
  TTokenPagination = class;
  TTokenPaginationArray = Array of TTokenPagination;
  TTrack = class;
  TTrackArray = Array of TTrack;
  TTrackversionCodes = class;
  TTrackversionCodesArray = Array of TTrackversionCodes;
  TTracksListResponse = class;
  TTracksListResponseArray = Array of TTracksListResponse;
  TTracksListResponsetracks = class;
  TTracksListResponsetracksArray = Array of TTracksListResponsetracks;
  
  { --------------------------------------------------------------------
    TApk
    --------------------------------------------------------------------}
  
  TApk = Class(TGoogleBaseObject)
  Private
    Fbinary : TApkBinary;
    FversionCode : integer;
  Protected
    //Property setters
    Procedure Setbinary(AIndex : Integer; AValue : TApkBinary); virtual;
    Procedure SetversionCode(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property binary : TApkBinary Index 0 Read Fbinary Write Setbinary;
    Property versionCode : integer Index 8 Read FversionCode Write SetversionCode;
  end;
  TApkClass = Class of TApk;
  
  { --------------------------------------------------------------------
    TApkBinary
    --------------------------------------------------------------------}
  
  TApkBinary = Class(TGoogleBaseObject)
  Private
    Fsha1 : string;
  Protected
    //Property setters
    Procedure Setsha1(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property sha1 : string Index 0 Read Fsha1 Write Setsha1;
  end;
  TApkBinaryClass = Class of TApkBinary;
  
  { --------------------------------------------------------------------
    TApkListing
    --------------------------------------------------------------------}
  
  TApkListing = Class(TGoogleBaseObject)
  Private
    Flanguage : string;
    FrecentChanges : string;
  Protected
    //Property setters
    Procedure Setlanguage(AIndex : Integer; AValue : string); virtual;
    Procedure SetrecentChanges(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property language : string Index 0 Read Flanguage Write Setlanguage;
    Property recentChanges : string Index 8 Read FrecentChanges Write SetrecentChanges;
  end;
  TApkListingClass = Class of TApkListing;
  
  { --------------------------------------------------------------------
    TApkListingsListResponse
    --------------------------------------------------------------------}
  
  TApkListingsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    Flistings : TApkListingsListResponselistings;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlistings(AIndex : Integer; AValue : TApkListingsListResponselistings); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property listings : TApkListingsListResponselistings Index 8 Read Flistings Write Setlistings;
  end;
  TApkListingsListResponseClass = Class of TApkListingsListResponse;
  
  { --------------------------------------------------------------------
    TApkListingsListResponselistings
    --------------------------------------------------------------------}
  
  TApkListingsListResponselistings = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TApkListingsListResponselistingsClass = Class of TApkListingsListResponselistings;
  
  { --------------------------------------------------------------------
    TApksAddExternallyHostedRequest
    --------------------------------------------------------------------}
  
  TApksAddExternallyHostedRequest = Class(TGoogleBaseObject)
  Private
    FexternallyHostedApk : TExternallyHostedApk;
  Protected
    //Property setters
    Procedure SetexternallyHostedApk(AIndex : Integer; AValue : TExternallyHostedApk); virtual;
  Public
  Published
    Property externallyHostedApk : TExternallyHostedApk Index 0 Read FexternallyHostedApk Write SetexternallyHostedApk;
  end;
  TApksAddExternallyHostedRequestClass = Class of TApksAddExternallyHostedRequest;
  
  { --------------------------------------------------------------------
    TApksAddExternallyHostedResponse
    --------------------------------------------------------------------}
  
  TApksAddExternallyHostedResponse = Class(TGoogleBaseObject)
  Private
    FexternallyHostedApk : TExternallyHostedApk;
  Protected
    //Property setters
    Procedure SetexternallyHostedApk(AIndex : Integer; AValue : TExternallyHostedApk); virtual;
  Public
  Published
    Property externallyHostedApk : TExternallyHostedApk Index 0 Read FexternallyHostedApk Write SetexternallyHostedApk;
  end;
  TApksAddExternallyHostedResponseClass = Class of TApksAddExternallyHostedResponse;
  
  { --------------------------------------------------------------------
    TApksListResponse
    --------------------------------------------------------------------}
  
  TApksListResponse = Class(TGoogleBaseObject)
  Private
    Fapks : TApksListResponseapks;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setapks(AIndex : Integer; AValue : TApksListResponseapks); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property apks : TApksListResponseapks Index 0 Read Fapks Write Setapks;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TApksListResponseClass = Class of TApksListResponse;
  
  { --------------------------------------------------------------------
    TApksListResponseapks
    --------------------------------------------------------------------}
  
  TApksListResponseapks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TApksListResponseapksClass = Class of TApksListResponseapks;
  
  { --------------------------------------------------------------------
    TAppDetails
    --------------------------------------------------------------------}
  
  TAppDetails = Class(TGoogleBaseObject)
  Private
    FcontactEmail : string;
    FcontactPhone : string;
    FcontactWebsite : string;
    FdefaultLanguage : string;
  Protected
    //Property setters
    Procedure SetcontactEmail(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontactPhone(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontactWebsite(AIndex : Integer; AValue : string); virtual;
    Procedure SetdefaultLanguage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property contactEmail : string Index 0 Read FcontactEmail Write SetcontactEmail;
    Property contactPhone : string Index 8 Read FcontactPhone Write SetcontactPhone;
    Property contactWebsite : string Index 16 Read FcontactWebsite Write SetcontactWebsite;
    Property defaultLanguage : string Index 24 Read FdefaultLanguage Write SetdefaultLanguage;
  end;
  TAppDetailsClass = Class of TAppDetails;
  
  { --------------------------------------------------------------------
    TAppEdit
    --------------------------------------------------------------------}
  
  TAppEdit = Class(TGoogleBaseObject)
  Private
    FexpiryTimeSeconds : string;
    Fid : string;
  Protected
    //Property setters
    Procedure SetexpiryTimeSeconds(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property expiryTimeSeconds : string Index 0 Read FexpiryTimeSeconds Write SetexpiryTimeSeconds;
    Property id : string Index 8 Read Fid Write Setid;
  end;
  TAppEditClass = Class of TAppEdit;
  
  { --------------------------------------------------------------------
    TEntitlement
    --------------------------------------------------------------------}
  
  TEntitlement = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    FproductId : string;
    FproductType : string;
    Ftoken : string;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetproductId(AIndex : Integer; AValue : string); virtual;
    Procedure SetproductType(AIndex : Integer; AValue : string); virtual;
    Procedure Settoken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property productId : string Index 8 Read FproductId Write SetproductId;
    Property productType : string Index 16 Read FproductType Write SetproductType;
    Property token : string Index 24 Read Ftoken Write Settoken;
  end;
  TEntitlementClass = Class of TEntitlement;
  
  { --------------------------------------------------------------------
    TEntitlementsListResponse
    --------------------------------------------------------------------}
  
  TEntitlementsListResponse = Class(TGoogleBaseObject)
  Private
    FpageInfo : TPageInfo;
    Fresources : TEntitlementsListResponseresources;
    FtokenPagination : TTokenPagination;
  Protected
    //Property setters
    Procedure SetpageInfo(AIndex : Integer; AValue : TPageInfo); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TEntitlementsListResponseresources); virtual;
    Procedure SettokenPagination(AIndex : Integer; AValue : TTokenPagination); virtual;
  Public
  Published
    Property pageInfo : TPageInfo Index 0 Read FpageInfo Write SetpageInfo;
    Property resources : TEntitlementsListResponseresources Index 8 Read Fresources Write Setresources;
    Property tokenPagination : TTokenPagination Index 16 Read FtokenPagination Write SettokenPagination;
  end;
  TEntitlementsListResponseClass = Class of TEntitlementsListResponse;
  
  { --------------------------------------------------------------------
    TEntitlementsListResponseresources
    --------------------------------------------------------------------}
  
  TEntitlementsListResponseresources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEntitlementsListResponseresourcesClass = Class of TEntitlementsListResponseresources;
  
  { --------------------------------------------------------------------
    TExpansionFile
    --------------------------------------------------------------------}
  
  TExpansionFile = Class(TGoogleBaseObject)
  Private
    FfileSize : string;
    FreferencesVersion : integer;
  Protected
    //Property setters
    Procedure SetfileSize(AIndex : Integer; AValue : string); virtual;
    Procedure SetreferencesVersion(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property fileSize : string Index 0 Read FfileSize Write SetfileSize;
    Property referencesVersion : integer Index 8 Read FreferencesVersion Write SetreferencesVersion;
  end;
  TExpansionFileClass = Class of TExpansionFile;
  
  { --------------------------------------------------------------------
    TExpansionFilesUploadResponse
    --------------------------------------------------------------------}
  
  TExpansionFilesUploadResponse = Class(TGoogleBaseObject)
  Private
    FexpansionFile : TExpansionFile;
  Protected
    //Property setters
    Procedure SetexpansionFile(AIndex : Integer; AValue : TExpansionFile); virtual;
  Public
  Published
    Property expansionFile : TExpansionFile Index 0 Read FexpansionFile Write SetexpansionFile;
  end;
  TExpansionFilesUploadResponseClass = Class of TExpansionFilesUploadResponse;
  
  { --------------------------------------------------------------------
    TExternallyHostedApk
    --------------------------------------------------------------------}
  
  TExternallyHostedApk = Class(TGoogleBaseObject)
  Private
    FapplicationLabel : string;
    FcertificateBase64s : TExternallyHostedApkcertificateBase64s;
    FexternallyHostedUrl : string;
    FfileSha1Base64 : string;
    FfileSha256Base64 : string;
    FfileSize : string;
    FiconBase64 : string;
    FmaximumSdk : integer;
    FminimumSdk : integer;
    FnativeCodes : TExternallyHostedApknativeCodes;
    FpackageName : string;
    FusesFeatures : TExternallyHostedApkusesFeatures;
    FusesPermissions : TExternallyHostedApkusesPermissions;
    FversionCode : integer;
    FversionName : string;
  Protected
    //Property setters
    Procedure SetapplicationLabel(AIndex : Integer; AValue : string); virtual;
    Procedure SetcertificateBase64s(AIndex : Integer; AValue : TExternallyHostedApkcertificateBase64s); virtual;
    Procedure SetexternallyHostedUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetfileSha1Base64(AIndex : Integer; AValue : string); virtual;
    Procedure SetfileSha256Base64(AIndex : Integer; AValue : string); virtual;
    Procedure SetfileSize(AIndex : Integer; AValue : string); virtual;
    Procedure SeticonBase64(AIndex : Integer; AValue : string); virtual;
    Procedure SetmaximumSdk(AIndex : Integer; AValue : integer); virtual;
    Procedure SetminimumSdk(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnativeCodes(AIndex : Integer; AValue : TExternallyHostedApknativeCodes); virtual;
    Procedure SetpackageName(AIndex : Integer; AValue : string); virtual;
    Procedure SetusesFeatures(AIndex : Integer; AValue : TExternallyHostedApkusesFeatures); virtual;
    Procedure SetusesPermissions(AIndex : Integer; AValue : TExternallyHostedApkusesPermissions); virtual;
    Procedure SetversionCode(AIndex : Integer; AValue : integer); virtual;
    Procedure SetversionName(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property applicationLabel : string Index 0 Read FapplicationLabel Write SetapplicationLabel;
    Property certificateBase64s : TExternallyHostedApkcertificateBase64s Index 8 Read FcertificateBase64s Write SetcertificateBase64s;
    Property externallyHostedUrl : string Index 16 Read FexternallyHostedUrl Write SetexternallyHostedUrl;
    Property fileSha1Base64 : string Index 24 Read FfileSha1Base64 Write SetfileSha1Base64;
    Property fileSha256Base64 : string Index 32 Read FfileSha256Base64 Write SetfileSha256Base64;
    Property fileSize : string Index 40 Read FfileSize Write SetfileSize;
    Property iconBase64 : string Index 48 Read FiconBase64 Write SeticonBase64;
    Property maximumSdk : integer Index 56 Read FmaximumSdk Write SetmaximumSdk;
    Property minimumSdk : integer Index 64 Read FminimumSdk Write SetminimumSdk;
    Property nativeCodes : TExternallyHostedApknativeCodes Index 72 Read FnativeCodes Write SetnativeCodes;
    Property packageName : string Index 80 Read FpackageName Write SetpackageName;
    Property usesFeatures : TExternallyHostedApkusesFeatures Index 88 Read FusesFeatures Write SetusesFeatures;
    Property usesPermissions : TExternallyHostedApkusesPermissions Index 96 Read FusesPermissions Write SetusesPermissions;
    Property versionCode : integer Index 104 Read FversionCode Write SetversionCode;
    Property versionName : string Index 112 Read FversionName Write SetversionName;
  end;
  TExternallyHostedApkClass = Class of TExternallyHostedApk;
  
  { --------------------------------------------------------------------
    TExternallyHostedApkcertificateBase64s
    --------------------------------------------------------------------}
  
  TExternallyHostedApkcertificateBase64s = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TExternallyHostedApkcertificateBase64sClass = Class of TExternallyHostedApkcertificateBase64s;
  
  { --------------------------------------------------------------------
    TExternallyHostedApknativeCodes
    --------------------------------------------------------------------}
  
  TExternallyHostedApknativeCodes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TExternallyHostedApknativeCodesClass = Class of TExternallyHostedApknativeCodes;
  
  { --------------------------------------------------------------------
    TExternallyHostedApkusesFeatures
    --------------------------------------------------------------------}
  
  TExternallyHostedApkusesFeatures = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TExternallyHostedApkusesFeaturesClass = Class of TExternallyHostedApkusesFeatures;
  
  { --------------------------------------------------------------------
    TExternallyHostedApkusesPermissions
    --------------------------------------------------------------------}
  
  TExternallyHostedApkusesPermissions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TExternallyHostedApkusesPermissionsClass = Class of TExternallyHostedApkusesPermissions;
  
  { --------------------------------------------------------------------
    TExternallyHostedApkUsesPermission
    --------------------------------------------------------------------}
  
  TExternallyHostedApkUsesPermission = Class(TGoogleBaseObject)
  Private
    FmaxSdkVersion : integer;
    Fname : string;
  Protected
    //Property setters
    Procedure SetmaxSdkVersion(AIndex : Integer; AValue : integer); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property maxSdkVersion : integer Index 0 Read FmaxSdkVersion Write SetmaxSdkVersion;
    Property name : string Index 8 Read Fname Write Setname;
  end;
  TExternallyHostedApkUsesPermissionClass = Class of TExternallyHostedApkUsesPermission;
  
  { --------------------------------------------------------------------
    TImage
    --------------------------------------------------------------------}
  
  TImage = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fsha1 : string;
    Furl : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setsha1(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property sha1 : string Index 8 Read Fsha1 Write Setsha1;
    Property url : string Index 16 Read Furl Write Seturl;
  end;
  TImageClass = Class of TImage;
  
  { --------------------------------------------------------------------
    TImagesDeleteAllResponse
    --------------------------------------------------------------------}
  
  TImagesDeleteAllResponse = Class(TGoogleBaseObject)
  Private
    Fdeleted : TImagesDeleteAllResponsedeleted;
  Protected
    //Property setters
    Procedure Setdeleted(AIndex : Integer; AValue : TImagesDeleteAllResponsedeleted); virtual;
  Public
  Published
    Property deleted : TImagesDeleteAllResponsedeleted Index 0 Read Fdeleted Write Setdeleted;
  end;
  TImagesDeleteAllResponseClass = Class of TImagesDeleteAllResponse;
  
  { --------------------------------------------------------------------
    TImagesDeleteAllResponsedeleted
    --------------------------------------------------------------------}
  
  TImagesDeleteAllResponsedeleted = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TImagesDeleteAllResponsedeletedClass = Class of TImagesDeleteAllResponsedeleted;
  
  { --------------------------------------------------------------------
    TImagesListResponse
    --------------------------------------------------------------------}
  
  TImagesListResponse = Class(TGoogleBaseObject)
  Private
    Fimages : TImagesListResponseimages;
  Protected
    //Property setters
    Procedure Setimages(AIndex : Integer; AValue : TImagesListResponseimages); virtual;
  Public
  Published
    Property images : TImagesListResponseimages Index 0 Read Fimages Write Setimages;
  end;
  TImagesListResponseClass = Class of TImagesListResponse;
  
  { --------------------------------------------------------------------
    TImagesListResponseimages
    --------------------------------------------------------------------}
  
  TImagesListResponseimages = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TImagesListResponseimagesClass = Class of TImagesListResponseimages;
  
  { --------------------------------------------------------------------
    TImagesUploadResponse
    --------------------------------------------------------------------}
  
  TImagesUploadResponse = Class(TGoogleBaseObject)
  Private
    Fimage : TImage;
  Protected
    //Property setters
    Procedure Setimage(AIndex : Integer; AValue : TImage); virtual;
  Public
  Published
    Property image : TImage Index 0 Read Fimage Write Setimage;
  end;
  TImagesUploadResponseClass = Class of TImagesUploadResponse;
  
  { --------------------------------------------------------------------
    TInAppProduct
    --------------------------------------------------------------------}
  
  TInAppProduct = Class(TGoogleBaseObject)
  Private
    FdefaultLanguage : string;
    FdefaultPrice : TPrice;
    Flistings : TInAppProductlistings;
    FpackageName : string;
    Fprices : TInAppProductprices;
    FpurchaseType : string;
    Fseason : TSeason;
    Fsku : string;
    Fstatus : string;
    FsubscriptionPeriod : string;
    FtrialPeriod : string;
  Protected
    //Property setters
    Procedure SetdefaultLanguage(AIndex : Integer; AValue : string); virtual;
    Procedure SetdefaultPrice(AIndex : Integer; AValue : TPrice); virtual;
    Procedure Setlistings(AIndex : Integer; AValue : TInAppProductlistings); virtual;
    Procedure SetpackageName(AIndex : Integer; AValue : string); virtual;
    Procedure Setprices(AIndex : Integer; AValue : TInAppProductprices); virtual;
    Procedure SetpurchaseType(AIndex : Integer; AValue : string); virtual;
    Procedure Setseason(AIndex : Integer; AValue : TSeason); virtual;
    Procedure Setsku(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure SetsubscriptionPeriod(AIndex : Integer; AValue : string); virtual;
    Procedure SettrialPeriod(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property defaultLanguage : string Index 0 Read FdefaultLanguage Write SetdefaultLanguage;
    Property defaultPrice : TPrice Index 8 Read FdefaultPrice Write SetdefaultPrice;
    Property listings : TInAppProductlistings Index 16 Read Flistings Write Setlistings;
    Property packageName : string Index 24 Read FpackageName Write SetpackageName;
    Property prices : TInAppProductprices Index 32 Read Fprices Write Setprices;
    Property purchaseType : string Index 40 Read FpurchaseType Write SetpurchaseType;
    Property season : TSeason Index 48 Read Fseason Write Setseason;
    Property sku : string Index 56 Read Fsku Write Setsku;
    Property status : string Index 64 Read Fstatus Write Setstatus;
    Property subscriptionPeriod : string Index 72 Read FsubscriptionPeriod Write SetsubscriptionPeriod;
    Property trialPeriod : string Index 80 Read FtrialPeriod Write SettrialPeriod;
  end;
  TInAppProductClass = Class of TInAppProduct;
  
  { --------------------------------------------------------------------
    TInAppProductlistings
    --------------------------------------------------------------------}
  
  TInAppProductlistings = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TInAppProductlistingsClass = Class of TInAppProductlistings;
  
  { --------------------------------------------------------------------
    TInAppProductprices
    --------------------------------------------------------------------}
  
  TInAppProductprices = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TInAppProductpricesClass = Class of TInAppProductprices;
  
  { --------------------------------------------------------------------
    TInAppProductListing
    --------------------------------------------------------------------}
  
  TInAppProductListing = Class(TGoogleBaseObject)
  Private
    Fdescription : string;
    Ftitle : string;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property description : string Index 0 Read Fdescription Write Setdescription;
    Property title : string Index 8 Read Ftitle Write Settitle;
  end;
  TInAppProductListingClass = Class of TInAppProductListing;
  
  { --------------------------------------------------------------------
    TInappproductsBatchRequest
    --------------------------------------------------------------------}
  
  TInappproductsBatchRequest = Class(TGoogleBaseObject)
  Private
    Fentrys : TInappproductsBatchRequestentrys;
  Protected
    //Property setters
    Procedure Setentrys(AIndex : Integer; AValue : TInappproductsBatchRequestentrys); virtual;
  Public
  Published
    Property entrys : TInappproductsBatchRequestentrys Index 0 Read Fentrys Write Setentrys;
  end;
  TInappproductsBatchRequestClass = Class of TInappproductsBatchRequest;
  
  { --------------------------------------------------------------------
    TInappproductsBatchRequestentrys
    --------------------------------------------------------------------}
  
  TInappproductsBatchRequestentrys = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInappproductsBatchRequestentrysClass = Class of TInappproductsBatchRequestentrys;
  
  { --------------------------------------------------------------------
    TInappproductsBatchRequestEntry
    --------------------------------------------------------------------}
  
  TInappproductsBatchRequestEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    Finappproductsinsertrequest : TInappproductsInsertRequest;
    Finappproductsupdaterequest : TInappproductsUpdateRequest;
    FmethodName : string;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Setinappproductsinsertrequest(AIndex : Integer; AValue : TInappproductsInsertRequest); virtual;
    Procedure Setinappproductsupdaterequest(AIndex : Integer; AValue : TInappproductsUpdateRequest); virtual;
    Procedure SetmethodName(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property inappproductsinsertrequest : TInappproductsInsertRequest Index 8 Read Finappproductsinsertrequest Write Setinappproductsinsertrequest;
    Property inappproductsupdaterequest : TInappproductsUpdateRequest Index 16 Read Finappproductsupdaterequest Write Setinappproductsupdaterequest;
    Property methodName : string Index 24 Read FmethodName Write SetmethodName;
  end;
  TInappproductsBatchRequestEntryClass = Class of TInappproductsBatchRequestEntry;
  
  { --------------------------------------------------------------------
    TInappproductsBatchResponse
    --------------------------------------------------------------------}
  
  TInappproductsBatchResponse = Class(TGoogleBaseObject)
  Private
    Fentrys : TInappproductsBatchResponseentrys;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setentrys(AIndex : Integer; AValue : TInappproductsBatchResponseentrys); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property entrys : TInappproductsBatchResponseentrys Index 0 Read Fentrys Write Setentrys;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TInappproductsBatchResponseClass = Class of TInappproductsBatchResponse;
  
  { --------------------------------------------------------------------
    TInappproductsBatchResponseentrys
    --------------------------------------------------------------------}
  
  TInappproductsBatchResponseentrys = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInappproductsBatchResponseentrysClass = Class of TInappproductsBatchResponseentrys;
  
  { --------------------------------------------------------------------
    TInappproductsBatchResponseEntry
    --------------------------------------------------------------------}
  
  TInappproductsBatchResponseEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    Finappproductsinsertresponse : TInappproductsInsertResponse;
    Finappproductsupdateresponse : TInappproductsUpdateResponse;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Setinappproductsinsertresponse(AIndex : Integer; AValue : TInappproductsInsertResponse); virtual;
    Procedure Setinappproductsupdateresponse(AIndex : Integer; AValue : TInappproductsUpdateResponse); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property inappproductsinsertresponse : TInappproductsInsertResponse Index 8 Read Finappproductsinsertresponse Write Setinappproductsinsertresponse;
    Property inappproductsupdateresponse : TInappproductsUpdateResponse Index 16 Read Finappproductsupdateresponse Write Setinappproductsupdateresponse;
  end;
  TInappproductsBatchResponseEntryClass = Class of TInappproductsBatchResponseEntry;
  
  { --------------------------------------------------------------------
    TInappproductsInsertRequest
    --------------------------------------------------------------------}
  
  TInappproductsInsertRequest = Class(TGoogleBaseObject)
  Private
    Finappproduct : TInAppProduct;
  Protected
    //Property setters
    Procedure Setinappproduct(AIndex : Integer; AValue : TInAppProduct); virtual;
  Public
  Published
    Property inappproduct : TInAppProduct Index 0 Read Finappproduct Write Setinappproduct;
  end;
  TInappproductsInsertRequestClass = Class of TInappproductsInsertRequest;
  
  { --------------------------------------------------------------------
    TInappproductsInsertResponse
    --------------------------------------------------------------------}
  
  TInappproductsInsertResponse = Class(TGoogleBaseObject)
  Private
    Finappproduct : TInAppProduct;
  Protected
    //Property setters
    Procedure Setinappproduct(AIndex : Integer; AValue : TInAppProduct); virtual;
  Public
  Published
    Property inappproduct : TInAppProduct Index 0 Read Finappproduct Write Setinappproduct;
  end;
  TInappproductsInsertResponseClass = Class of TInappproductsInsertResponse;
  
  { --------------------------------------------------------------------
    TInappproductsListResponse
    --------------------------------------------------------------------}
  
  TInappproductsListResponse = Class(TGoogleBaseObject)
  Private
    Finappproduct : TInappproductsListResponseinappproduct;
    Fkind : string;
    FpageInfo : TPageInfo;
    FtokenPagination : TTokenPagination;
  Protected
    //Property setters
    Procedure Setinappproduct(AIndex : Integer; AValue : TInappproductsListResponseinappproduct); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetpageInfo(AIndex : Integer; AValue : TPageInfo); virtual;
    Procedure SettokenPagination(AIndex : Integer; AValue : TTokenPagination); virtual;
  Public
  Published
    Property inappproduct : TInappproductsListResponseinappproduct Index 0 Read Finappproduct Write Setinappproduct;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property pageInfo : TPageInfo Index 16 Read FpageInfo Write SetpageInfo;
    Property tokenPagination : TTokenPagination Index 24 Read FtokenPagination Write SettokenPagination;
  end;
  TInappproductsListResponseClass = Class of TInappproductsListResponse;
  
  { --------------------------------------------------------------------
    TInappproductsListResponseinappproduct
    --------------------------------------------------------------------}
  
  TInappproductsListResponseinappproduct = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInappproductsListResponseinappproductClass = Class of TInappproductsListResponseinappproduct;
  
  { --------------------------------------------------------------------
    TInappproductsUpdateRequest
    --------------------------------------------------------------------}
  
  TInappproductsUpdateRequest = Class(TGoogleBaseObject)
  Private
    Finappproduct : TInAppProduct;
  Protected
    //Property setters
    Procedure Setinappproduct(AIndex : Integer; AValue : TInAppProduct); virtual;
  Public
  Published
    Property inappproduct : TInAppProduct Index 0 Read Finappproduct Write Setinappproduct;
  end;
  TInappproductsUpdateRequestClass = Class of TInappproductsUpdateRequest;
  
  { --------------------------------------------------------------------
    TInappproductsUpdateResponse
    --------------------------------------------------------------------}
  
  TInappproductsUpdateResponse = Class(TGoogleBaseObject)
  Private
    Finappproduct : TInAppProduct;
  Protected
    //Property setters
    Procedure Setinappproduct(AIndex : Integer; AValue : TInAppProduct); virtual;
  Public
  Published
    Property inappproduct : TInAppProduct Index 0 Read Finappproduct Write Setinappproduct;
  end;
  TInappproductsUpdateResponseClass = Class of TInappproductsUpdateResponse;
  
  { --------------------------------------------------------------------
    TListing
    --------------------------------------------------------------------}
  
  TListing = Class(TGoogleBaseObject)
  Private
    FfullDescription : string;
    Flanguage : string;
    FshortDescription : string;
    Ftitle : string;
    Fvideo : string;
  Protected
    //Property setters
    Procedure SetfullDescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setlanguage(AIndex : Integer; AValue : string); virtual;
    Procedure SetshortDescription(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure Setvideo(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property fullDescription : string Index 0 Read FfullDescription Write SetfullDescription;
    Property language : string Index 8 Read Flanguage Write Setlanguage;
    Property shortDescription : string Index 16 Read FshortDescription Write SetshortDescription;
    Property title : string Index 24 Read Ftitle Write Settitle;
    Property video : string Index 32 Read Fvideo Write Setvideo;
  end;
  TListingClass = Class of TListing;
  
  { --------------------------------------------------------------------
    TListingsListResponse
    --------------------------------------------------------------------}
  
  TListingsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    Flistings : TListingsListResponselistings;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlistings(AIndex : Integer; AValue : TListingsListResponselistings); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property listings : TListingsListResponselistings Index 8 Read Flistings Write Setlistings;
  end;
  TListingsListResponseClass = Class of TListingsListResponse;
  
  { --------------------------------------------------------------------
    TListingsListResponselistings
    --------------------------------------------------------------------}
  
  TListingsListResponselistings = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListingsListResponselistingsClass = Class of TListingsListResponselistings;
  
  { --------------------------------------------------------------------
    TMonthDay
    --------------------------------------------------------------------}
  
  TMonthDay = Class(TGoogleBaseObject)
  Private
    Fday : integer;
    Fmonth : integer;
  Protected
    //Property setters
    Procedure Setday(AIndex : Integer; AValue : integer); virtual;
    Procedure Setmonth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property day : integer Index 0 Read Fday Write Setday;
    Property month : integer Index 8 Read Fmonth Write Setmonth;
  end;
  TMonthDayClass = Class of TMonthDay;
  
  { --------------------------------------------------------------------
    TPageInfo
    --------------------------------------------------------------------}
  
  TPageInfo = Class(TGoogleBaseObject)
  Private
    FresultPerPage : integer;
    FstartIndex : integer;
    FtotalResults : integer;
  Protected
    //Property setters
    Procedure SetresultPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property resultPerPage : integer Index 0 Read FresultPerPage Write SetresultPerPage;
    Property startIndex : integer Index 8 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 16 Read FtotalResults Write SettotalResults;
  end;
  TPageInfoClass = Class of TPageInfo;
  
  { --------------------------------------------------------------------
    TPrice
    --------------------------------------------------------------------}
  
  TPrice = Class(TGoogleBaseObject)
  Private
    Fcurrency : string;
    FpriceMicros : string;
  Protected
    //Property setters
    Procedure Setcurrency(AIndex : Integer; AValue : string); virtual;
    Procedure SetpriceMicros(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property currency : string Index 0 Read Fcurrency Write Setcurrency;
    Property priceMicros : string Index 8 Read FpriceMicros Write SetpriceMicros;
  end;
  TPriceClass = Class of TPrice;
  
  { --------------------------------------------------------------------
    TProductPurchase
    --------------------------------------------------------------------}
  
  TProductPurchase = Class(TGoogleBaseObject)
  Private
    FconsumptionState : integer;
    FdeveloperPayload : string;
    Fkind : string;
    FpurchaseState : integer;
    FpurchaseTimeMillis : string;
  Protected
    //Property setters
    Procedure SetconsumptionState(AIndex : Integer; AValue : integer); virtual;
    Procedure SetdeveloperPayload(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetpurchaseState(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpurchaseTimeMillis(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property consumptionState : integer Index 0 Read FconsumptionState Write SetconsumptionState;
    Property developerPayload : string Index 8 Read FdeveloperPayload Write SetdeveloperPayload;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property purchaseState : integer Index 24 Read FpurchaseState Write SetpurchaseState;
    Property purchaseTimeMillis : string Index 32 Read FpurchaseTimeMillis Write SetpurchaseTimeMillis;
  end;
  TProductPurchaseClass = Class of TProductPurchase;
  
  { --------------------------------------------------------------------
    TSeason
    --------------------------------------------------------------------}
  
  TSeason = Class(TGoogleBaseObject)
  Private
    F_end : TMonthDay;
    Fstart : TMonthDay;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_end(AIndex : Integer; AValue : TMonthDay); virtual;
    Procedure Setstart(AIndex : Integer; AValue : TMonthDay); virtual;
  Public
  Published
    Property _end : TMonthDay Index 0 Read F_end Write Set_end;
    Property start : TMonthDay Index 8 Read Fstart Write Setstart;
  end;
  TSeasonClass = Class of TSeason;
  
  { --------------------------------------------------------------------
    TSubscriptionDeferralInfo
    --------------------------------------------------------------------}
  
  TSubscriptionDeferralInfo = Class(TGoogleBaseObject)
  Private
    FdesiredExpiryTimeMillis : string;
    FexpectedExpiryTimeMillis : string;
  Protected
    //Property setters
    Procedure SetdesiredExpiryTimeMillis(AIndex : Integer; AValue : string); virtual;
    Procedure SetexpectedExpiryTimeMillis(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property desiredExpiryTimeMillis : string Index 0 Read FdesiredExpiryTimeMillis Write SetdesiredExpiryTimeMillis;
    Property expectedExpiryTimeMillis : string Index 8 Read FexpectedExpiryTimeMillis Write SetexpectedExpiryTimeMillis;
  end;
  TSubscriptionDeferralInfoClass = Class of TSubscriptionDeferralInfo;
  
  { --------------------------------------------------------------------
    TSubscriptionPurchase
    --------------------------------------------------------------------}
  
  TSubscriptionPurchase = Class(TGoogleBaseObject)
  Private
    FautoRenewing : boolean;
    FexpiryTimeMillis : string;
    Fkind : string;
    FstartTimeMillis : string;
  Protected
    //Property setters
    Procedure SetautoRenewing(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetexpiryTimeMillis(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartTimeMillis(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property autoRenewing : boolean Index 0 Read FautoRenewing Write SetautoRenewing;
    Property expiryTimeMillis : string Index 8 Read FexpiryTimeMillis Write SetexpiryTimeMillis;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property startTimeMillis : string Index 24 Read FstartTimeMillis Write SetstartTimeMillis;
  end;
  TSubscriptionPurchaseClass = Class of TSubscriptionPurchase;
  
  { --------------------------------------------------------------------
    TSubscriptionPurchasesDeferRequest
    --------------------------------------------------------------------}
  
  TSubscriptionPurchasesDeferRequest = Class(TGoogleBaseObject)
  Private
    FdeferralInfo : TSubscriptionDeferralInfo;
  Protected
    //Property setters
    Procedure SetdeferralInfo(AIndex : Integer; AValue : TSubscriptionDeferralInfo); virtual;
  Public
  Published
    Property deferralInfo : TSubscriptionDeferralInfo Index 0 Read FdeferralInfo Write SetdeferralInfo;
  end;
  TSubscriptionPurchasesDeferRequestClass = Class of TSubscriptionPurchasesDeferRequest;
  
  { --------------------------------------------------------------------
    TSubscriptionPurchasesDeferResponse
    --------------------------------------------------------------------}
  
  TSubscriptionPurchasesDeferResponse = Class(TGoogleBaseObject)
  Private
    FnewExpiryTimeMillis : string;
  Protected
    //Property setters
    Procedure SetnewExpiryTimeMillis(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property newExpiryTimeMillis : string Index 0 Read FnewExpiryTimeMillis Write SetnewExpiryTimeMillis;
  end;
  TSubscriptionPurchasesDeferResponseClass = Class of TSubscriptionPurchasesDeferResponse;
  
  { --------------------------------------------------------------------
    TTesters
    --------------------------------------------------------------------}
  
  TTesters = Class(TGoogleBaseObject)
  Private
    FgoogleGroups : TTestersgoogleGroups;
    FgooglePlusCommunities : TTestersgooglePlusCommunities;
  Protected
    //Property setters
    Procedure SetgoogleGroups(AIndex : Integer; AValue : TTestersgoogleGroups); virtual;
    Procedure SetgooglePlusCommunities(AIndex : Integer; AValue : TTestersgooglePlusCommunities); virtual;
  Public
  Published
    Property googleGroups : TTestersgoogleGroups Index 0 Read FgoogleGroups Write SetgoogleGroups;
    Property googlePlusCommunities : TTestersgooglePlusCommunities Index 8 Read FgooglePlusCommunities Write SetgooglePlusCommunities;
  end;
  TTestersClass = Class of TTesters;
  
  { --------------------------------------------------------------------
    TTestersgoogleGroups
    --------------------------------------------------------------------}
  
  TTestersgoogleGroups = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTestersgoogleGroupsClass = Class of TTestersgoogleGroups;
  
  { --------------------------------------------------------------------
    TTestersgooglePlusCommunities
    --------------------------------------------------------------------}
  
  TTestersgooglePlusCommunities = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTestersgooglePlusCommunitiesClass = Class of TTestersgooglePlusCommunities;
  
  { --------------------------------------------------------------------
    TTokenPagination
    --------------------------------------------------------------------}
  
  TTokenPagination = Class(TGoogleBaseObject)
  Private
    FnextPageToken : string;
    FpreviousPageToken : string;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetpreviousPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property nextPageToken : string Index 0 Read FnextPageToken Write SetnextPageToken;
    Property previousPageToken : string Index 8 Read FpreviousPageToken Write SetpreviousPageToken;
  end;
  TTokenPaginationClass = Class of TTokenPagination;
  
  { --------------------------------------------------------------------
    TTrack
    --------------------------------------------------------------------}
  
  TTrack = Class(TGoogleBaseObject)
  Private
    Ftrack : string;
    FuserFraction : double;
    FversionCodes : TTrackversionCodes;
  Protected
    //Property setters
    Procedure Settrack(AIndex : Integer; AValue : string); virtual;
    Procedure SetuserFraction(AIndex : Integer; AValue : double); virtual;
    Procedure SetversionCodes(AIndex : Integer; AValue : TTrackversionCodes); virtual;
  Public
  Published
    Property track : string Index 0 Read Ftrack Write Settrack;
    Property userFraction : double Index 8 Read FuserFraction Write SetuserFraction;
    Property versionCodes : TTrackversionCodes Index 16 Read FversionCodes Write SetversionCodes;
  end;
  TTrackClass = Class of TTrack;
  
  { --------------------------------------------------------------------
    TTrackversionCodes
    --------------------------------------------------------------------}
  
  TTrackversionCodes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTrackversionCodesClass = Class of TTrackversionCodes;
  
  { --------------------------------------------------------------------
    TTracksListResponse
    --------------------------------------------------------------------}
  
  TTracksListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    Ftracks : TTracksListResponsetracks;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Settracks(AIndex : Integer; AValue : TTracksListResponsetracks); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property tracks : TTracksListResponsetracks Index 8 Read Ftracks Write Settracks;
  end;
  TTracksListResponseClass = Class of TTracksListResponse;
  
  { --------------------------------------------------------------------
    TTracksListResponsetracks
    --------------------------------------------------------------------}
  
  TTracksListResponsetracks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTracksListResponsetracksClass = Class of TTracksListResponsetracks;
  
  { --------------------------------------------------------------------
    TEditsResource
    --------------------------------------------------------------------}
  
  TEditsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Commit(editId: string; packageName: string) : TAppEdit;
    Procedure Delete(editId: string; packageName: string);
    Function Get(editId: string; packageName: string) : TAppEdit;
    Function Insert(packageName: string; aAppEdit : TAppEdit) : TAppEdit;
    Function Validate(editId: string; packageName: string) : TAppEdit;
  end;
  
  
  { --------------------------------------------------------------------
    TEntitlementsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TEntitlementsResource, method List
  
  TEntitlementsListOptions = Record
    maxResults : integer;
    productId : string;
    startIndex : integer;
    token : string;
  end;
  
  TEntitlementsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(packageName: string; AQuery : string  = '') : TEntitlementsListResponse;
    Function List(packageName: string; AQuery : TEntitlementslistOptions) : TEntitlementsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TInappproductsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TInappproductsResource, method Insert
  
  TInappproductsInsertOptions = Record
    autoConvertMissingPrices : boolean;
  end;
  
  
  //Optional query Options for TInappproductsResource, method List
  
  TInappproductsListOptions = Record
    maxResults : integer;
    startIndex : integer;
    token : string;
  end;
  
  
  //Optional query Options for TInappproductsResource, method Patch
  
  TInappproductsPatchOptions = Record
    autoConvertMissingPrices : boolean;
  end;
  
  
  //Optional query Options for TInappproductsResource, method Update
  
  TInappproductsUpdateOptions = Record
    autoConvertMissingPrices : boolean;
  end;
  
  TInappproductsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Batch(aInappproductsBatchRequest : TInappproductsBatchRequest) : TInappproductsBatchResponse;
    Procedure Delete(packageName: string; sku: string);
    Function Get(packageName: string; sku: string) : TInAppProduct;
    Function Insert(packageName: string; aInAppProduct : TInAppProduct; AQuery : string  = '') : TInAppProduct;
    Function Insert(packageName: string; aInAppProduct : TInAppProduct; AQuery : TInappproductsinsertOptions) : TInAppProduct;
    Function List(packageName: string; AQuery : string  = '') : TInappproductsListResponse;
    Function List(packageName: string; AQuery : TInappproductslistOptions) : TInappproductsListResponse;
    Function Patch(packageName: string; sku: string; aInAppProduct : TInAppProduct; AQuery : string  = '') : TInAppProduct;
    Function Patch(packageName: string; sku: string; aInAppProduct : TInAppProduct; AQuery : TInappproductspatchOptions) : TInAppProduct;
    Function Update(packageName: string; sku: string; aInAppProduct : TInAppProduct; AQuery : string  = '') : TInAppProduct;
    Function Update(packageName: string; sku: string; aInAppProduct : TInAppProduct; AQuery : TInappproductsupdateOptions) : TInAppProduct;
  end;
  
  
  { --------------------------------------------------------------------
    TPurchasesResource
    --------------------------------------------------------------------}
  
  TPurchasesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
  end;
  
  
  { --------------------------------------------------------------------
    TAndroidpublisherAPI
    --------------------------------------------------------------------}
  
  TAndroidpublisherAPI = Class(TGoogleAPI)
  Private
    FEditsInstance : TEditsResource;
    FEntitlementsInstance : TEntitlementsResource;
    FInappproductsInstance : TInappproductsResource;
    FPurchasesInstance : TPurchasesResource;
    Function GetEditsInstance : TEditsResource;virtual;
    Function GetEntitlementsInstance : TEntitlementsResource;virtual;
    Function GetInappproductsInstance : TInappproductsResource;virtual;
    Function GetPurchasesInstance : TPurchasesResource;virtual;
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
    Function CreateEditsResource(AOwner : TComponent) : TEditsResource;virtual;overload;
    Function CreateEditsResource : TEditsResource;virtual;overload;
    Function CreateEntitlementsResource(AOwner : TComponent) : TEntitlementsResource;virtual;overload;
    Function CreateEntitlementsResource : TEntitlementsResource;virtual;overload;
    Function CreateInappproductsResource(AOwner : TComponent) : TInappproductsResource;virtual;overload;
    Function CreateInappproductsResource : TInappproductsResource;virtual;overload;
    Function CreatePurchasesResource(AOwner : TComponent) : TPurchasesResource;virtual;overload;
    Function CreatePurchasesResource : TPurchasesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property EditsResource : TEditsResource Read GetEditsInstance;
    Property EntitlementsResource : TEntitlementsResource Read GetEntitlementsInstance;
    Property InappproductsResource : TInappproductsResource Read GetInappproductsInstance;
    Property PurchasesResource : TPurchasesResource Read GetPurchasesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TApk
  --------------------------------------------------------------------}


Procedure TApk.Setbinary(AIndex : Integer; AValue : TApkBinary); 

begin
  If (Fbinary=AValue) then exit;
  Fbinary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApk.SetversionCode(AIndex : Integer; AValue : integer); 

begin
  If (FversionCode=AValue) then exit;
  FversionCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TApkBinary
  --------------------------------------------------------------------}


Procedure TApkBinary.Setsha1(AIndex : Integer; AValue : string); 

begin
  If (Fsha1=AValue) then exit;
  Fsha1:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TApkListing
  --------------------------------------------------------------------}


Procedure TApkListing.Setlanguage(AIndex : Integer; AValue : string); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApkListing.SetrecentChanges(AIndex : Integer; AValue : string); 

begin
  If (FrecentChanges=AValue) then exit;
  FrecentChanges:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TApkListingsListResponse
  --------------------------------------------------------------------}


Procedure TApkListingsListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApkListingsListResponse.Setlistings(AIndex : Integer; AValue : TApkListingsListResponselistings); 

begin
  If (Flistings=AValue) then exit;
  Flistings:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TApkListingsListResponselistings
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TApksAddExternallyHostedRequest
  --------------------------------------------------------------------}


Procedure TApksAddExternallyHostedRequest.SetexternallyHostedApk(AIndex : Integer; AValue : TExternallyHostedApk); 

begin
  If (FexternallyHostedApk=AValue) then exit;
  FexternallyHostedApk:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TApksAddExternallyHostedResponse
  --------------------------------------------------------------------}


Procedure TApksAddExternallyHostedResponse.SetexternallyHostedApk(AIndex : Integer; AValue : TExternallyHostedApk); 

begin
  If (FexternallyHostedApk=AValue) then exit;
  FexternallyHostedApk:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TApksListResponse
  --------------------------------------------------------------------}


Procedure TApksListResponse.Setapks(AIndex : Integer; AValue : TApksListResponseapks); 

begin
  If (Fapks=AValue) then exit;
  Fapks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApksListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TApksListResponseapks
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAppDetails
  --------------------------------------------------------------------}


Procedure TAppDetails.SetcontactEmail(AIndex : Integer; AValue : string); 

begin
  If (FcontactEmail=AValue) then exit;
  FcontactEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppDetails.SetcontactPhone(AIndex : Integer; AValue : string); 

begin
  If (FcontactPhone=AValue) then exit;
  FcontactPhone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppDetails.SetcontactWebsite(AIndex : Integer; AValue : string); 

begin
  If (FcontactWebsite=AValue) then exit;
  FcontactWebsite:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppDetails.SetdefaultLanguage(AIndex : Integer; AValue : string); 

begin
  If (FdefaultLanguage=AValue) then exit;
  FdefaultLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAppEdit
  --------------------------------------------------------------------}


Procedure TAppEdit.SetexpiryTimeSeconds(AIndex : Integer; AValue : string); 

begin
  If (FexpiryTimeSeconds=AValue) then exit;
  FexpiryTimeSeconds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppEdit.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEntitlement
  --------------------------------------------------------------------}


Procedure TEntitlement.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntitlement.SetproductId(AIndex : Integer; AValue : string); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntitlement.SetproductType(AIndex : Integer; AValue : string); 

begin
  If (FproductType=AValue) then exit;
  FproductType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntitlement.Settoken(AIndex : Integer; AValue : string); 

begin
  If (Ftoken=AValue) then exit;
  Ftoken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEntitlementsListResponse
  --------------------------------------------------------------------}


Procedure TEntitlementsListResponse.SetpageInfo(AIndex : Integer; AValue : TPageInfo); 

begin
  If (FpageInfo=AValue) then exit;
  FpageInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntitlementsListResponse.Setresources(AIndex : Integer; AValue : TEntitlementsListResponseresources); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntitlementsListResponse.SettokenPagination(AIndex : Integer; AValue : TTokenPagination); 

begin
  If (FtokenPagination=AValue) then exit;
  FtokenPagination:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEntitlementsListResponseresources
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TExpansionFile
  --------------------------------------------------------------------}


Procedure TExpansionFile.SetfileSize(AIndex : Integer; AValue : string); 

begin
  If (FfileSize=AValue) then exit;
  FfileSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExpansionFile.SetreferencesVersion(AIndex : Integer; AValue : integer); 

begin
  If (FreferencesVersion=AValue) then exit;
  FreferencesVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExpansionFilesUploadResponse
  --------------------------------------------------------------------}


Procedure TExpansionFilesUploadResponse.SetexpansionFile(AIndex : Integer; AValue : TExpansionFile); 

begin
  If (FexpansionFile=AValue) then exit;
  FexpansionFile:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExternallyHostedApk
  --------------------------------------------------------------------}


Procedure TExternallyHostedApk.SetapplicationLabel(AIndex : Integer; AValue : string); 

begin
  If (FapplicationLabel=AValue) then exit;
  FapplicationLabel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SetcertificateBase64s(AIndex : Integer; AValue : TExternallyHostedApkcertificateBase64s); 

begin
  If (FcertificateBase64s=AValue) then exit;
  FcertificateBase64s:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SetexternallyHostedUrl(AIndex : Integer; AValue : string); 

begin
  If (FexternallyHostedUrl=AValue) then exit;
  FexternallyHostedUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SetfileSha1Base64(AIndex : Integer; AValue : string); 

begin
  If (FfileSha1Base64=AValue) then exit;
  FfileSha1Base64:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SetfileSha256Base64(AIndex : Integer; AValue : string); 

begin
  If (FfileSha256Base64=AValue) then exit;
  FfileSha256Base64:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SetfileSize(AIndex : Integer; AValue : string); 

begin
  If (FfileSize=AValue) then exit;
  FfileSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SeticonBase64(AIndex : Integer; AValue : string); 

begin
  If (FiconBase64=AValue) then exit;
  FiconBase64:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SetmaximumSdk(AIndex : Integer; AValue : integer); 

begin
  If (FmaximumSdk=AValue) then exit;
  FmaximumSdk:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SetminimumSdk(AIndex : Integer; AValue : integer); 

begin
  If (FminimumSdk=AValue) then exit;
  FminimumSdk:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SetnativeCodes(AIndex : Integer; AValue : TExternallyHostedApknativeCodes); 

begin
  If (FnativeCodes=AValue) then exit;
  FnativeCodes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SetpackageName(AIndex : Integer; AValue : string); 

begin
  If (FpackageName=AValue) then exit;
  FpackageName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SetusesFeatures(AIndex : Integer; AValue : TExternallyHostedApkusesFeatures); 

begin
  If (FusesFeatures=AValue) then exit;
  FusesFeatures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SetusesPermissions(AIndex : Integer; AValue : TExternallyHostedApkusesPermissions); 

begin
  If (FusesPermissions=AValue) then exit;
  FusesPermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SetversionCode(AIndex : Integer; AValue : integer); 

begin
  If (FversionCode=AValue) then exit;
  FversionCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SetversionName(AIndex : Integer; AValue : string); 

begin
  If (FversionName=AValue) then exit;
  FversionName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExternallyHostedApkcertificateBase64s
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TExternallyHostedApknativeCodes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TExternallyHostedApkusesFeatures
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TExternallyHostedApkusesPermissions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TExternallyHostedApkUsesPermission
  --------------------------------------------------------------------}


Procedure TExternallyHostedApkUsesPermission.SetmaxSdkVersion(AIndex : Integer; AValue : integer); 

begin
  If (FmaxSdkVersion=AValue) then exit;
  FmaxSdkVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApkUsesPermission.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImage
  --------------------------------------------------------------------}


Procedure TImage.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImage.Setsha1(AIndex : Integer; AValue : string); 

begin
  If (Fsha1=AValue) then exit;
  Fsha1:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImage.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImagesDeleteAllResponse
  --------------------------------------------------------------------}


Procedure TImagesDeleteAllResponse.Setdeleted(AIndex : Integer; AValue : TImagesDeleteAllResponsedeleted); 

begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImagesDeleteAllResponsedeleted
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TImagesListResponse
  --------------------------------------------------------------------}


Procedure TImagesListResponse.Setimages(AIndex : Integer; AValue : TImagesListResponseimages); 

begin
  If (Fimages=AValue) then exit;
  Fimages:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImagesListResponseimages
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TImagesUploadResponse
  --------------------------------------------------------------------}


Procedure TImagesUploadResponse.Setimage(AIndex : Integer; AValue : TImage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInAppProduct
  --------------------------------------------------------------------}


Procedure TInAppProduct.SetdefaultLanguage(AIndex : Integer; AValue : string); 

begin
  If (FdefaultLanguage=AValue) then exit;
  FdefaultLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInAppProduct.SetdefaultPrice(AIndex : Integer; AValue : TPrice); 

begin
  If (FdefaultPrice=AValue) then exit;
  FdefaultPrice:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInAppProduct.Setlistings(AIndex : Integer; AValue : TInAppProductlistings); 

begin
  If (Flistings=AValue) then exit;
  Flistings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInAppProduct.SetpackageName(AIndex : Integer; AValue : string); 

begin
  If (FpackageName=AValue) then exit;
  FpackageName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInAppProduct.Setprices(AIndex : Integer; AValue : TInAppProductprices); 

begin
  If (Fprices=AValue) then exit;
  Fprices:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInAppProduct.SetpurchaseType(AIndex : Integer; AValue : string); 

begin
  If (FpurchaseType=AValue) then exit;
  FpurchaseType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInAppProduct.Setseason(AIndex : Integer; AValue : TSeason); 

begin
  If (Fseason=AValue) then exit;
  Fseason:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInAppProduct.Setsku(AIndex : Integer; AValue : string); 

begin
  If (Fsku=AValue) then exit;
  Fsku:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInAppProduct.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInAppProduct.SetsubscriptionPeriod(AIndex : Integer; AValue : string); 

begin
  If (FsubscriptionPeriod=AValue) then exit;
  FsubscriptionPeriod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInAppProduct.SettrialPeriod(AIndex : Integer; AValue : string); 

begin
  If (FtrialPeriod=AValue) then exit;
  FtrialPeriod:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInAppProductlistings
  --------------------------------------------------------------------}


Class Function TInAppProductlistings.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TInAppProductprices
  --------------------------------------------------------------------}


Class Function TInAppProductprices.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TInAppProductListing
  --------------------------------------------------------------------}


Procedure TInAppProductListing.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInAppProductListing.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInappproductsBatchRequest
  --------------------------------------------------------------------}


Procedure TInappproductsBatchRequest.Setentrys(AIndex : Integer; AValue : TInappproductsBatchRequestentrys); 

begin
  If (Fentrys=AValue) then exit;
  Fentrys:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInappproductsBatchRequestentrys
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInappproductsBatchRequestEntry
  --------------------------------------------------------------------}


Procedure TInappproductsBatchRequestEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInappproductsBatchRequestEntry.Setinappproductsinsertrequest(AIndex : Integer; AValue : TInappproductsInsertRequest); 

begin
  If (Finappproductsinsertrequest=AValue) then exit;
  Finappproductsinsertrequest:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInappproductsBatchRequestEntry.Setinappproductsupdaterequest(AIndex : Integer; AValue : TInappproductsUpdateRequest); 

begin
  If (Finappproductsupdaterequest=AValue) then exit;
  Finappproductsupdaterequest:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInappproductsBatchRequestEntry.SetmethodName(AIndex : Integer; AValue : string); 

begin
  If (FmethodName=AValue) then exit;
  FmethodName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInappproductsBatchResponse
  --------------------------------------------------------------------}


Procedure TInappproductsBatchResponse.Setentrys(AIndex : Integer; AValue : TInappproductsBatchResponseentrys); 

begin
  If (Fentrys=AValue) then exit;
  Fentrys:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInappproductsBatchResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInappproductsBatchResponseentrys
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInappproductsBatchResponseEntry
  --------------------------------------------------------------------}


Procedure TInappproductsBatchResponseEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInappproductsBatchResponseEntry.Setinappproductsinsertresponse(AIndex : Integer; AValue : TInappproductsInsertResponse); 

begin
  If (Finappproductsinsertresponse=AValue) then exit;
  Finappproductsinsertresponse:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInappproductsBatchResponseEntry.Setinappproductsupdateresponse(AIndex : Integer; AValue : TInappproductsUpdateResponse); 

begin
  If (Finappproductsupdateresponse=AValue) then exit;
  Finappproductsupdateresponse:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInappproductsInsertRequest
  --------------------------------------------------------------------}


Procedure TInappproductsInsertRequest.Setinappproduct(AIndex : Integer; AValue : TInAppProduct); 

begin
  If (Finappproduct=AValue) then exit;
  Finappproduct:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInappproductsInsertResponse
  --------------------------------------------------------------------}


Procedure TInappproductsInsertResponse.Setinappproduct(AIndex : Integer; AValue : TInAppProduct); 

begin
  If (Finappproduct=AValue) then exit;
  Finappproduct:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInappproductsListResponse
  --------------------------------------------------------------------}


Procedure TInappproductsListResponse.Setinappproduct(AIndex : Integer; AValue : TInappproductsListResponseinappproduct); 

begin
  If (Finappproduct=AValue) then exit;
  Finappproduct:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInappproductsListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInappproductsListResponse.SetpageInfo(AIndex : Integer; AValue : TPageInfo); 

begin
  If (FpageInfo=AValue) then exit;
  FpageInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInappproductsListResponse.SettokenPagination(AIndex : Integer; AValue : TTokenPagination); 

begin
  If (FtokenPagination=AValue) then exit;
  FtokenPagination:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInappproductsListResponseinappproduct
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInappproductsUpdateRequest
  --------------------------------------------------------------------}


Procedure TInappproductsUpdateRequest.Setinappproduct(AIndex : Integer; AValue : TInAppProduct); 

begin
  If (Finappproduct=AValue) then exit;
  Finappproduct:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInappproductsUpdateResponse
  --------------------------------------------------------------------}


Procedure TInappproductsUpdateResponse.Setinappproduct(AIndex : Integer; AValue : TInAppProduct); 

begin
  If (Finappproduct=AValue) then exit;
  Finappproduct:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListing
  --------------------------------------------------------------------}


Procedure TListing.SetfullDescription(AIndex : Integer; AValue : string); 

begin
  If (FfullDescription=AValue) then exit;
  FfullDescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListing.Setlanguage(AIndex : Integer; AValue : string); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListing.SetshortDescription(AIndex : Integer; AValue : string); 

begin
  If (FshortDescription=AValue) then exit;
  FshortDescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListing.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListing.Setvideo(AIndex : Integer; AValue : string); 

begin
  If (Fvideo=AValue) then exit;
  Fvideo:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListingsListResponse
  --------------------------------------------------------------------}


Procedure TListingsListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListingsListResponse.Setlistings(AIndex : Integer; AValue : TListingsListResponselistings); 

begin
  If (Flistings=AValue) then exit;
  Flistings:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListingsListResponselistings
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMonthDay
  --------------------------------------------------------------------}


Procedure TMonthDay.Setday(AIndex : Integer; AValue : integer); 

begin
  If (Fday=AValue) then exit;
  Fday:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMonthDay.Setmonth(AIndex : Integer; AValue : integer); 

begin
  If (Fmonth=AValue) then exit;
  Fmonth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPageInfo
  --------------------------------------------------------------------}


Procedure TPageInfo.SetresultPerPage(AIndex : Integer; AValue : integer); 

begin
  If (FresultPerPage=AValue) then exit;
  FresultPerPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageInfo.SetstartIndex(AIndex : Integer; AValue : integer); 

begin
  If (FstartIndex=AValue) then exit;
  FstartIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageInfo.SettotalResults(AIndex : Integer; AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPrice
  --------------------------------------------------------------------}


Procedure TPrice.Setcurrency(AIndex : Integer; AValue : string); 

begin
  If (Fcurrency=AValue) then exit;
  Fcurrency:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPrice.SetpriceMicros(AIndex : Integer; AValue : string); 

begin
  If (FpriceMicros=AValue) then exit;
  FpriceMicros:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductPurchase
  --------------------------------------------------------------------}


Procedure TProductPurchase.SetconsumptionState(AIndex : Integer; AValue : integer); 

begin
  If (FconsumptionState=AValue) then exit;
  FconsumptionState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductPurchase.SetdeveloperPayload(AIndex : Integer; AValue : string); 

begin
  If (FdeveloperPayload=AValue) then exit;
  FdeveloperPayload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductPurchase.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductPurchase.SetpurchaseState(AIndex : Integer; AValue : integer); 

begin
  If (FpurchaseState=AValue) then exit;
  FpurchaseState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductPurchase.SetpurchaseTimeMillis(AIndex : Integer; AValue : string); 

begin
  If (FpurchaseTimeMillis=AValue) then exit;
  FpurchaseTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSeason
  --------------------------------------------------------------------}


Procedure TSeason.Set_end(AIndex : Integer; AValue : TMonthDay); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeason.Setstart(AIndex : Integer; AValue : TMonthDay); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TSeason.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TSubscriptionDeferralInfo
  --------------------------------------------------------------------}


Procedure TSubscriptionDeferralInfo.SetdesiredExpiryTimeMillis(AIndex : Integer; AValue : string); 

begin
  If (FdesiredExpiryTimeMillis=AValue) then exit;
  FdesiredExpiryTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionDeferralInfo.SetexpectedExpiryTimeMillis(AIndex : Integer; AValue : string); 

begin
  If (FexpectedExpiryTimeMillis=AValue) then exit;
  FexpectedExpiryTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSubscriptionPurchase
  --------------------------------------------------------------------}


Procedure TSubscriptionPurchase.SetautoRenewing(AIndex : Integer; AValue : boolean); 

begin
  If (FautoRenewing=AValue) then exit;
  FautoRenewing:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionPurchase.SetexpiryTimeMillis(AIndex : Integer; AValue : string); 

begin
  If (FexpiryTimeMillis=AValue) then exit;
  FexpiryTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionPurchase.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionPurchase.SetstartTimeMillis(AIndex : Integer; AValue : string); 

begin
  If (FstartTimeMillis=AValue) then exit;
  FstartTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSubscriptionPurchasesDeferRequest
  --------------------------------------------------------------------}


Procedure TSubscriptionPurchasesDeferRequest.SetdeferralInfo(AIndex : Integer; AValue : TSubscriptionDeferralInfo); 

begin
  If (FdeferralInfo=AValue) then exit;
  FdeferralInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSubscriptionPurchasesDeferResponse
  --------------------------------------------------------------------}


Procedure TSubscriptionPurchasesDeferResponse.SetnewExpiryTimeMillis(AIndex : Integer; AValue : string); 

begin
  If (FnewExpiryTimeMillis=AValue) then exit;
  FnewExpiryTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTesters
  --------------------------------------------------------------------}


Procedure TTesters.SetgoogleGroups(AIndex : Integer; AValue : TTestersgoogleGroups); 

begin
  If (FgoogleGroups=AValue) then exit;
  FgoogleGroups:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTesters.SetgooglePlusCommunities(AIndex : Integer; AValue : TTestersgooglePlusCommunities); 

begin
  If (FgooglePlusCommunities=AValue) then exit;
  FgooglePlusCommunities:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTestersgoogleGroups
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTestersgooglePlusCommunities
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTokenPagination
  --------------------------------------------------------------------}


Procedure TTokenPagination.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTokenPagination.SetpreviousPageToken(AIndex : Integer; AValue : string); 

begin
  If (FpreviousPageToken=AValue) then exit;
  FpreviousPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTrack
  --------------------------------------------------------------------}


Procedure TTrack.Settrack(AIndex : Integer; AValue : string); 

begin
  If (Ftrack=AValue) then exit;
  Ftrack:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrack.SetuserFraction(AIndex : Integer; AValue : double); 

begin
  If (FuserFraction=AValue) then exit;
  FuserFraction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrack.SetversionCodes(AIndex : Integer; AValue : TTrackversionCodes); 

begin
  If (FversionCodes=AValue) then exit;
  FversionCodes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTrackversionCodes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTracksListResponse
  --------------------------------------------------------------------}


Procedure TTracksListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTracksListResponse.Settracks(AIndex : Integer; AValue : TTracksListResponsetracks); 

begin
  If (Ftracks=AValue) then exit;
  Ftracks:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTracksListResponsetracks
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TEditsResource
  --------------------------------------------------------------------}


Class Function TEditsResource.ResourceName : String;

begin
  Result:='edits';
end;

Class Function TEditsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidpublisherAPI;
end;

Function TEditsResource.Commit(editId: string; packageName: string) : TAppEdit;

Const
  _HTTPMethod = 'POST';
  _Path       = '{packageName}/edits/{editId}:commit';
  _Methodid   = 'androidpublisher.edits.commit';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAppEdit) as TAppEdit;
end;

Procedure TEditsResource.Delete(editId: string; packageName: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{packageName}/edits/{editId}';
  _Methodid   = 'androidpublisher.edits.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'packageName',packageName]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TEditsResource.Get(editId: string; packageName: string) : TAppEdit;

Const
  _HTTPMethod = 'GET';
  _Path       = '{packageName}/edits/{editId}';
  _Methodid   = 'androidpublisher.edits.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAppEdit) as TAppEdit;
end;

Function TEditsResource.Insert(packageName: string; aAppEdit : TAppEdit) : TAppEdit;

Const
  _HTTPMethod = 'POST';
  _Path       = '{packageName}/edits';
  _Methodid   = 'androidpublisher.edits.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAppEdit,TAppEdit) as TAppEdit;
end;

Function TEditsResource.Validate(editId: string; packageName: string) : TAppEdit;

Const
  _HTTPMethod = 'POST';
  _Path       = '{packageName}/edits/{editId}:validate';
  _Methodid   = 'androidpublisher.edits.validate';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAppEdit) as TAppEdit;
end;



{ --------------------------------------------------------------------
  TEntitlementsResource
  --------------------------------------------------------------------}


Class Function TEntitlementsResource.ResourceName : String;

begin
  Result:='entitlements';
end;

Class Function TEntitlementsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidpublisherAPI;
end;

Function TEntitlementsResource.List(packageName: string; AQuery : string = '') : TEntitlementsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{packageName}/entitlements';
  _Methodid   = 'androidpublisher.entitlements.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TEntitlementsListResponse) as TEntitlementsListResponse;
end;


Function TEntitlementsResource.List(packageName: string; AQuery : TEntitlementslistOptions) : TEntitlementsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'productId',AQuery.productId);
  AddToQuery(_Q,'startIndex',AQuery.startIndex);
  AddToQuery(_Q,'token',AQuery.token);
  Result:=List(packageName,_Q);
end;



{ --------------------------------------------------------------------
  TInappproductsResource
  --------------------------------------------------------------------}


Class Function TInappproductsResource.ResourceName : String;

begin
  Result:='inappproducts';
end;

Class Function TInappproductsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidpublisherAPI;
end;

Function TInappproductsResource.Batch(aInappproductsBatchRequest : TInappproductsBatchRequest) : TInappproductsBatchResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'inappproducts/batch';
  _Methodid   = 'androidpublisher.inappproducts.batch';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aInappproductsBatchRequest,TInappproductsBatchResponse) as TInappproductsBatchResponse;
end;

Procedure TInappproductsResource.Delete(packageName: string; sku: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{packageName}/inappproducts/{sku}';
  _Methodid   = 'androidpublisher.inappproducts.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['packageName',packageName,'sku',sku]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TInappproductsResource.Get(packageName: string; sku: string) : TInAppProduct;

Const
  _HTTPMethod = 'GET';
  _Path       = '{packageName}/inappproducts/{sku}';
  _Methodid   = 'androidpublisher.inappproducts.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['packageName',packageName,'sku',sku]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TInAppProduct) as TInAppProduct;
end;

Function TInappproductsResource.Insert(packageName: string; aInAppProduct : TInAppProduct; AQuery : string = '') : TInAppProduct;

Const
  _HTTPMethod = 'POST';
  _Path       = '{packageName}/inappproducts';
  _Methodid   = 'androidpublisher.inappproducts.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aInAppProduct,TInAppProduct) as TInAppProduct;
end;


Function TInappproductsResource.Insert(packageName: string; aInAppProduct : TInAppProduct; AQuery : TInappproductsinsertOptions) : TInAppProduct;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'autoConvertMissingPrices',AQuery.autoConvertMissingPrices);
  Result:=Insert(packageName,aInAppProduct,_Q);
end;

Function TInappproductsResource.List(packageName: string; AQuery : string = '') : TInappproductsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{packageName}/inappproducts';
  _Methodid   = 'androidpublisher.inappproducts.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TInappproductsListResponse) as TInappproductsListResponse;
end;


Function TInappproductsResource.List(packageName: string; AQuery : TInappproductslistOptions) : TInappproductsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'startIndex',AQuery.startIndex);
  AddToQuery(_Q,'token',AQuery.token);
  Result:=List(packageName,_Q);
end;

Function TInappproductsResource.Patch(packageName: string; sku: string; aInAppProduct : TInAppProduct; AQuery : string = '') : TInAppProduct;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{packageName}/inappproducts/{sku}';
  _Methodid   = 'androidpublisher.inappproducts.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['packageName',packageName,'sku',sku]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aInAppProduct,TInAppProduct) as TInAppProduct;
end;


Function TInappproductsResource.Patch(packageName: string; sku: string; aInAppProduct : TInAppProduct; AQuery : TInappproductspatchOptions) : TInAppProduct;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'autoConvertMissingPrices',AQuery.autoConvertMissingPrices);
  Result:=Patch(packageName,sku,aInAppProduct,_Q);
end;

Function TInappproductsResource.Update(packageName: string; sku: string; aInAppProduct : TInAppProduct; AQuery : string = '') : TInAppProduct;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{packageName}/inappproducts/{sku}';
  _Methodid   = 'androidpublisher.inappproducts.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['packageName',packageName,'sku',sku]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aInAppProduct,TInAppProduct) as TInAppProduct;
end;


Function TInappproductsResource.Update(packageName: string; sku: string; aInAppProduct : TInAppProduct; AQuery : TInappproductsupdateOptions) : TInAppProduct;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'autoConvertMissingPrices',AQuery.autoConvertMissingPrices);
  Result:=Update(packageName,sku,aInAppProduct,_Q);
end;



{ --------------------------------------------------------------------
  TPurchasesResource
  --------------------------------------------------------------------}


Class Function TPurchasesResource.ResourceName : String;

begin
  Result:='purchases';
end;

Class Function TPurchasesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidpublisherAPI;
end;



{ --------------------------------------------------------------------
  TAndroidpublisherAPI
  --------------------------------------------------------------------}

Class Function TAndroidpublisherAPI.APIName : String;

begin
  Result:='androidpublisher';
end;

Class Function TAndroidpublisherAPI.APIVersion : String;

begin
  Result:='v2';
end;

Class Function TAndroidpublisherAPI.APIRevision : String;

begin
  Result:='20150316';
end;

Class Function TAndroidpublisherAPI.APIID : String;

begin
  Result:='androidpublisher:v2';
end;

Class Function TAndroidpublisherAPI.APITitle : String;

begin
  Result:='Google Play Developer API';
end;

Class Function TAndroidpublisherAPI.APIDescription : String;

begin
  Result:='Lets Android application developers access their Google Play accounts.';
end;

Class Function TAndroidpublisherAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TAndroidpublisherAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TAndroidpublisherAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/android-16.png';
end;

Class Function TAndroidpublisherAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/android-32.png';
end;

Class Function TAndroidpublisherAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/android-publisher';
end;

Class Function TAndroidpublisherAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TAndroidpublisherAPI.APIbasePath : string;

begin
  Result:='/androidpublisher/v2/applications/';
end;

Class Function TAndroidpublisherAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/androidpublisher/v2/applications/';
end;

Class Function TAndroidpublisherAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TAndroidpublisherAPI.APIservicePath : string;

begin
  Result:='androidpublisher/v2/applications/';
end;

Class Function TAndroidpublisherAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TAndroidpublisherAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/androidpublisher';
  Result[0].Description:='View and manage your Google Play Developer account';
  
end;

Class Function TAndroidpublisherAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TAndroidpublisherAPI.RegisterAPIResources;

begin
  TApk.RegisterObject;
  TApkBinary.RegisterObject;
  TApkListing.RegisterObject;
  TApkListingsListResponse.RegisterObject;
  TApkListingsListResponselistings.RegisterObject;
  TApksAddExternallyHostedRequest.RegisterObject;
  TApksAddExternallyHostedResponse.RegisterObject;
  TApksListResponse.RegisterObject;
  TApksListResponseapks.RegisterObject;
  TAppDetails.RegisterObject;
  TAppEdit.RegisterObject;
  TEntitlement.RegisterObject;
  TEntitlementsListResponse.RegisterObject;
  TEntitlementsListResponseresources.RegisterObject;
  TExpansionFile.RegisterObject;
  TExpansionFilesUploadResponse.RegisterObject;
  TExternallyHostedApk.RegisterObject;
  TExternallyHostedApkcertificateBase64s.RegisterObject;
  TExternallyHostedApknativeCodes.RegisterObject;
  TExternallyHostedApkusesFeatures.RegisterObject;
  TExternallyHostedApkusesPermissions.RegisterObject;
  TExternallyHostedApkUsesPermission.RegisterObject;
  TImage.RegisterObject;
  TImagesDeleteAllResponse.RegisterObject;
  TImagesDeleteAllResponsedeleted.RegisterObject;
  TImagesListResponse.RegisterObject;
  TImagesListResponseimages.RegisterObject;
  TImagesUploadResponse.RegisterObject;
  TInAppProduct.RegisterObject;
  TInAppProductlistings.RegisterObject;
  TInAppProductprices.RegisterObject;
  TInAppProductListing.RegisterObject;
  TInappproductsBatchRequest.RegisterObject;
  TInappproductsBatchRequestentrys.RegisterObject;
  TInappproductsBatchRequestEntry.RegisterObject;
  TInappproductsBatchResponse.RegisterObject;
  TInappproductsBatchResponseentrys.RegisterObject;
  TInappproductsBatchResponseEntry.RegisterObject;
  TInappproductsInsertRequest.RegisterObject;
  TInappproductsInsertResponse.RegisterObject;
  TInappproductsListResponse.RegisterObject;
  TInappproductsListResponseinappproduct.RegisterObject;
  TInappproductsUpdateRequest.RegisterObject;
  TInappproductsUpdateResponse.RegisterObject;
  TListing.RegisterObject;
  TListingsListResponse.RegisterObject;
  TListingsListResponselistings.RegisterObject;
  TMonthDay.RegisterObject;
  TPageInfo.RegisterObject;
  TPrice.RegisterObject;
  TProductPurchase.RegisterObject;
  TSeason.RegisterObject;
  TSubscriptionDeferralInfo.RegisterObject;
  TSubscriptionPurchase.RegisterObject;
  TSubscriptionPurchasesDeferRequest.RegisterObject;
  TSubscriptionPurchasesDeferResponse.RegisterObject;
  TTesters.RegisterObject;
  TTestersgoogleGroups.RegisterObject;
  TTestersgooglePlusCommunities.RegisterObject;
  TTokenPagination.RegisterObject;
  TTrack.RegisterObject;
  TTrackversionCodes.RegisterObject;
  TTracksListResponse.RegisterObject;
  TTracksListResponsetracks.RegisterObject;
end;


Function TAndroidpublisherAPI.GetEditsInstance : TEditsResource;

begin
  if (FEditsInstance=Nil) then
    FEditsInstance:=CreateEditsResource;
  Result:=FEditsInstance;
end;

Function TAndroidpublisherAPI.CreateEditsResource : TEditsResource;

begin
  Result:=CreateEditsResource(Self);
end;


Function TAndroidpublisherAPI.CreateEditsResource(AOwner : TComponent) : TEditsResource;

begin
  Result:=TEditsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAndroidpublisherAPI.GetEntitlementsInstance : TEntitlementsResource;

begin
  if (FEntitlementsInstance=Nil) then
    FEntitlementsInstance:=CreateEntitlementsResource;
  Result:=FEntitlementsInstance;
end;

Function TAndroidpublisherAPI.CreateEntitlementsResource : TEntitlementsResource;

begin
  Result:=CreateEntitlementsResource(Self);
end;


Function TAndroidpublisherAPI.CreateEntitlementsResource(AOwner : TComponent) : TEntitlementsResource;

begin
  Result:=TEntitlementsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAndroidpublisherAPI.GetInappproductsInstance : TInappproductsResource;

begin
  if (FInappproductsInstance=Nil) then
    FInappproductsInstance:=CreateInappproductsResource;
  Result:=FInappproductsInstance;
end;

Function TAndroidpublisherAPI.CreateInappproductsResource : TInappproductsResource;

begin
  Result:=CreateInappproductsResource(Self);
end;


Function TAndroidpublisherAPI.CreateInappproductsResource(AOwner : TComponent) : TInappproductsResource;

begin
  Result:=TInappproductsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAndroidpublisherAPI.GetPurchasesInstance : TPurchasesResource;

begin
  if (FPurchasesInstance=Nil) then
    FPurchasesInstance:=CreatePurchasesResource;
  Result:=FPurchasesInstance;
end;

Function TAndroidpublisherAPI.CreatePurchasesResource : TPurchasesResource;

begin
  Result:=CreatePurchasesResource(Self);
end;


Function TAndroidpublisherAPI.CreatePurchasesResource(AOwner : TComponent) : TPurchasesResource;

begin
  Result:=TPurchasesResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TAndroidpublisherAPI.RegisterAPI;
end.
