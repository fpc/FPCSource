unit googleandroidpublisher;
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
//Generated on: 16-5-15 08:52:58
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TApk = Class;
  TApkBinary = Class;
  TApkListing = Class;
  TApkListingsListResponse = Class;
  TApksAddExternallyHostedRequest = Class;
  TApksAddExternallyHostedResponse = Class;
  TApksListResponse = Class;
  TAppDetails = Class;
  TAppEdit = Class;
  TEntitlement = Class;
  TEntitlementsListResponse = Class;
  TExpansionFile = Class;
  TExpansionFilesUploadResponse = Class;
  TExternallyHostedApk = Class;
  TExternallyHostedApkUsesPermission = Class;
  TImage = Class;
  TImagesDeleteAllResponse = Class;
  TImagesListResponse = Class;
  TImagesUploadResponse = Class;
  TInAppProduct = Class;
  TInAppProductListing = Class;
  TInappproductsBatchRequest = Class;
  TInappproductsBatchRequestEntry = Class;
  TInappproductsBatchResponse = Class;
  TInappproductsBatchResponseEntry = Class;
  TInappproductsInsertRequest = Class;
  TInappproductsInsertResponse = Class;
  TInappproductsListResponse = Class;
  TInappproductsUpdateRequest = Class;
  TInappproductsUpdateResponse = Class;
  TListing = Class;
  TListingsListResponse = Class;
  TMonthDay = Class;
  TPageInfo = Class;
  TPrice = Class;
  TProductPurchase = Class;
  TSeason = Class;
  TSubscriptionDeferralInfo = Class;
  TSubscriptionPurchase = Class;
  TSubscriptionPurchasesDeferRequest = Class;
  TSubscriptionPurchasesDeferResponse = Class;
  TTesters = Class;
  TTokenPagination = Class;
  TTrack = Class;
  TTracksListResponse = Class;
  TApkArray = Array of TApk;
  TApkBinaryArray = Array of TApkBinary;
  TApkListingArray = Array of TApkListing;
  TApkListingsListResponseArray = Array of TApkListingsListResponse;
  TApksAddExternallyHostedRequestArray = Array of TApksAddExternallyHostedRequest;
  TApksAddExternallyHostedResponseArray = Array of TApksAddExternallyHostedResponse;
  TApksListResponseArray = Array of TApksListResponse;
  TAppDetailsArray = Array of TAppDetails;
  TAppEditArray = Array of TAppEdit;
  TEntitlementArray = Array of TEntitlement;
  TEntitlementsListResponseArray = Array of TEntitlementsListResponse;
  TExpansionFileArray = Array of TExpansionFile;
  TExpansionFilesUploadResponseArray = Array of TExpansionFilesUploadResponse;
  TExternallyHostedApkArray = Array of TExternallyHostedApk;
  TExternallyHostedApkUsesPermissionArray = Array of TExternallyHostedApkUsesPermission;
  TImageArray = Array of TImage;
  TImagesDeleteAllResponseArray = Array of TImagesDeleteAllResponse;
  TImagesListResponseArray = Array of TImagesListResponse;
  TImagesUploadResponseArray = Array of TImagesUploadResponse;
  TInAppProductArray = Array of TInAppProduct;
  TInAppProductListingArray = Array of TInAppProductListing;
  TInappproductsBatchRequestArray = Array of TInappproductsBatchRequest;
  TInappproductsBatchRequestEntryArray = Array of TInappproductsBatchRequestEntry;
  TInappproductsBatchResponseArray = Array of TInappproductsBatchResponse;
  TInappproductsBatchResponseEntryArray = Array of TInappproductsBatchResponseEntry;
  TInappproductsInsertRequestArray = Array of TInappproductsInsertRequest;
  TInappproductsInsertResponseArray = Array of TInappproductsInsertResponse;
  TInappproductsListResponseArray = Array of TInappproductsListResponse;
  TInappproductsUpdateRequestArray = Array of TInappproductsUpdateRequest;
  TInappproductsUpdateResponseArray = Array of TInappproductsUpdateResponse;
  TListingArray = Array of TListing;
  TListingsListResponseArray = Array of TListingsListResponse;
  TMonthDayArray = Array of TMonthDay;
  TPageInfoArray = Array of TPageInfo;
  TPriceArray = Array of TPrice;
  TProductPurchaseArray = Array of TProductPurchase;
  TSeasonArray = Array of TSeason;
  TSubscriptionDeferralInfoArray = Array of TSubscriptionDeferralInfo;
  TSubscriptionPurchaseArray = Array of TSubscriptionPurchase;
  TSubscriptionPurchasesDeferRequestArray = Array of TSubscriptionPurchasesDeferRequest;
  TSubscriptionPurchasesDeferResponseArray = Array of TSubscriptionPurchasesDeferResponse;
  TTestersArray = Array of TTesters;
  TTokenPaginationArray = Array of TTokenPagination;
  TTrackArray = Array of TTrack;
  TTracksListResponseArray = Array of TTracksListResponse;
  //Anonymous types, using auto-generated names
  TInAppProductTypelistings = Class;
  TInAppProductTypeprices = Class;
  TApkListingsListResponseTypelistingsArray = Array of TApkListing;
  TApksListResponseTypeapksArray = Array of TApk;
  TEntitlementsListResponseTyperesourcesArray = Array of TEntitlement;
  TExternallyHostedApkTypeusesPermissionsArray = Array of TExternallyHostedApkUsesPermission;
  TImagesDeleteAllResponseTypedeletedArray = Array of TImage;
  TImagesListResponseTypeimagesArray = Array of TImage;
  TInappproductsBatchRequestTypeentrysArray = Array of TInappproductsBatchRequestEntry;
  TInappproductsBatchResponseTypeentrysArray = Array of TInappproductsBatchResponseEntry;
  TInappproductsListResponseTypeinappproductArray = Array of TInAppProduct;
  TListingsListResponseTypelistingsArray = Array of TListing;
  TTracksListResponseTypetracksArray = Array of TTrack;
  
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
    Fsha1 : String;
  Protected
    //Property setters
    Procedure Setsha1(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property sha1 : String Index 0 Read Fsha1 Write Setsha1;
  end;
  TApkBinaryClass = Class of TApkBinary;
  
  { --------------------------------------------------------------------
    TApkListing
    --------------------------------------------------------------------}
  
  TApkListing = Class(TGoogleBaseObject)
  Private
    Flanguage : String;
    FrecentChanges : String;
  Protected
    //Property setters
    Procedure Setlanguage(AIndex : Integer; AValue : String); virtual;
    Procedure SetrecentChanges(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property language : String Index 0 Read Flanguage Write Setlanguage;
    Property recentChanges : String Index 8 Read FrecentChanges Write SetrecentChanges;
  end;
  TApkListingClass = Class of TApkListing;
  
  { --------------------------------------------------------------------
    TApkListingsListResponse
    --------------------------------------------------------------------}
  
  TApkListingsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Flistings : TApkListingsListResponseTypelistingsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlistings(AIndex : Integer; AValue : TApkListingsListResponseTypelistingsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property listings : TApkListingsListResponseTypelistingsArray Index 8 Read Flistings Write Setlistings;
  end;
  TApkListingsListResponseClass = Class of TApkListingsListResponse;
  
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
    Fapks : TApksListResponseTypeapksArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setapks(AIndex : Integer; AValue : TApksListResponseTypeapksArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property apks : TApksListResponseTypeapksArray Index 0 Read Fapks Write Setapks;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TApksListResponseClass = Class of TApksListResponse;
  
  { --------------------------------------------------------------------
    TAppDetails
    --------------------------------------------------------------------}
  
  TAppDetails = Class(TGoogleBaseObject)
  Private
    FcontactEmail : String;
    FcontactPhone : String;
    FcontactWebsite : String;
    FdefaultLanguage : String;
  Protected
    //Property setters
    Procedure SetcontactEmail(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontactPhone(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontactWebsite(AIndex : Integer; AValue : String); virtual;
    Procedure SetdefaultLanguage(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property contactEmail : String Index 0 Read FcontactEmail Write SetcontactEmail;
    Property contactPhone : String Index 8 Read FcontactPhone Write SetcontactPhone;
    Property contactWebsite : String Index 16 Read FcontactWebsite Write SetcontactWebsite;
    Property defaultLanguage : String Index 24 Read FdefaultLanguage Write SetdefaultLanguage;
  end;
  TAppDetailsClass = Class of TAppDetails;
  
  { --------------------------------------------------------------------
    TAppEdit
    --------------------------------------------------------------------}
  
  TAppEdit = Class(TGoogleBaseObject)
  Private
    FexpiryTimeSeconds : String;
    Fid : String;
  Protected
    //Property setters
    Procedure SetexpiryTimeSeconds(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property expiryTimeSeconds : String Index 0 Read FexpiryTimeSeconds Write SetexpiryTimeSeconds;
    Property id : String Index 8 Read Fid Write Setid;
  end;
  TAppEditClass = Class of TAppEdit;
  
  { --------------------------------------------------------------------
    TEntitlement
    --------------------------------------------------------------------}
  
  TEntitlement = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FproductId : String;
    FproductType : String;
    Ftoken : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetproductId(AIndex : Integer; AValue : String); virtual;
    Procedure SetproductType(AIndex : Integer; AValue : String); virtual;
    Procedure Settoken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property productId : String Index 8 Read FproductId Write SetproductId;
    Property productType : String Index 16 Read FproductType Write SetproductType;
    Property token : String Index 24 Read Ftoken Write Settoken;
  end;
  TEntitlementClass = Class of TEntitlement;
  
  { --------------------------------------------------------------------
    TEntitlementsListResponse
    --------------------------------------------------------------------}
  
  TEntitlementsListResponse = Class(TGoogleBaseObject)
  Private
    FpageInfo : TPageInfo;
    Fresources : TEntitlementsListResponseTyperesourcesArray;
    FtokenPagination : TTokenPagination;
  Protected
    //Property setters
    Procedure SetpageInfo(AIndex : Integer; AValue : TPageInfo); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TEntitlementsListResponseTyperesourcesArray); virtual;
    Procedure SettokenPagination(AIndex : Integer; AValue : TTokenPagination); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property pageInfo : TPageInfo Index 0 Read FpageInfo Write SetpageInfo;
    Property resources : TEntitlementsListResponseTyperesourcesArray Index 8 Read Fresources Write Setresources;
    Property tokenPagination : TTokenPagination Index 16 Read FtokenPagination Write SettokenPagination;
  end;
  TEntitlementsListResponseClass = Class of TEntitlementsListResponse;
  
  { --------------------------------------------------------------------
    TExpansionFile
    --------------------------------------------------------------------}
  
  TExpansionFile = Class(TGoogleBaseObject)
  Private
    FfileSize : String;
    FreferencesVersion : integer;
  Protected
    //Property setters
    Procedure SetfileSize(AIndex : Integer; AValue : String); virtual;
    Procedure SetreferencesVersion(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property fileSize : String Index 0 Read FfileSize Write SetfileSize;
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
    FapplicationLabel : String;
    FcertificateBase64s : TStringArray;
    FexternallyHostedUrl : String;
    FfileSha1Base64 : String;
    FfileSha256Base64 : String;
    FfileSize : String;
    FiconBase64 : String;
    FmaximumSdk : integer;
    FminimumSdk : integer;
    FnativeCodes : TStringArray;
    FpackageName : String;
    FusesFeatures : TStringArray;
    FusesPermissions : TExternallyHostedApkTypeusesPermissionsArray;
    FversionCode : integer;
    FversionName : String;
  Protected
    //Property setters
    Procedure SetapplicationLabel(AIndex : Integer; AValue : String); virtual;
    Procedure SetcertificateBase64s(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetexternallyHostedUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetfileSha1Base64(AIndex : Integer; AValue : String); virtual;
    Procedure SetfileSha256Base64(AIndex : Integer; AValue : String); virtual;
    Procedure SetfileSize(AIndex : Integer; AValue : String); virtual;
    Procedure SeticonBase64(AIndex : Integer; AValue : String); virtual;
    Procedure SetmaximumSdk(AIndex : Integer; AValue : integer); virtual;
    Procedure SetminimumSdk(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnativeCodes(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetpackageName(AIndex : Integer; AValue : String); virtual;
    Procedure SetusesFeatures(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetusesPermissions(AIndex : Integer; AValue : TExternallyHostedApkTypeusesPermissionsArray); virtual;
    Procedure SetversionCode(AIndex : Integer; AValue : integer); virtual;
    Procedure SetversionName(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property applicationLabel : String Index 0 Read FapplicationLabel Write SetapplicationLabel;
    Property certificateBase64s : TStringArray Index 8 Read FcertificateBase64s Write SetcertificateBase64s;
    Property externallyHostedUrl : String Index 16 Read FexternallyHostedUrl Write SetexternallyHostedUrl;
    Property fileSha1Base64 : String Index 24 Read FfileSha1Base64 Write SetfileSha1Base64;
    Property fileSha256Base64 : String Index 32 Read FfileSha256Base64 Write SetfileSha256Base64;
    Property fileSize : String Index 40 Read FfileSize Write SetfileSize;
    Property iconBase64 : String Index 48 Read FiconBase64 Write SeticonBase64;
    Property maximumSdk : integer Index 56 Read FmaximumSdk Write SetmaximumSdk;
    Property minimumSdk : integer Index 64 Read FminimumSdk Write SetminimumSdk;
    Property nativeCodes : TStringArray Index 72 Read FnativeCodes Write SetnativeCodes;
    Property packageName : String Index 80 Read FpackageName Write SetpackageName;
    Property usesFeatures : TStringArray Index 88 Read FusesFeatures Write SetusesFeatures;
    Property usesPermissions : TExternallyHostedApkTypeusesPermissionsArray Index 96 Read FusesPermissions Write SetusesPermissions;
    Property versionCode : integer Index 104 Read FversionCode Write SetversionCode;
    Property versionName : String Index 112 Read FversionName Write SetversionName;
  end;
  TExternallyHostedApkClass = Class of TExternallyHostedApk;
  
  { --------------------------------------------------------------------
    TExternallyHostedApkUsesPermission
    --------------------------------------------------------------------}
  
  TExternallyHostedApkUsesPermission = Class(TGoogleBaseObject)
  Private
    FmaxSdkVersion : integer;
    Fname : String;
  Protected
    //Property setters
    Procedure SetmaxSdkVersion(AIndex : Integer; AValue : integer); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property maxSdkVersion : integer Index 0 Read FmaxSdkVersion Write SetmaxSdkVersion;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TExternallyHostedApkUsesPermissionClass = Class of TExternallyHostedApkUsesPermission;
  
  { --------------------------------------------------------------------
    TImage
    --------------------------------------------------------------------}
  
  TImage = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fsha1 : String;
    Furl : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setsha1(AIndex : Integer; AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property sha1 : String Index 8 Read Fsha1 Write Setsha1;
    Property url : String Index 16 Read Furl Write Seturl;
  end;
  TImageClass = Class of TImage;
  
  { --------------------------------------------------------------------
    TImagesDeleteAllResponse
    --------------------------------------------------------------------}
  
  TImagesDeleteAllResponse = Class(TGoogleBaseObject)
  Private
    Fdeleted : TImagesDeleteAllResponseTypedeletedArray;
  Protected
    //Property setters
    Procedure Setdeleted(AIndex : Integer; AValue : TImagesDeleteAllResponseTypedeletedArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property deleted : TImagesDeleteAllResponseTypedeletedArray Index 0 Read Fdeleted Write Setdeleted;
  end;
  TImagesDeleteAllResponseClass = Class of TImagesDeleteAllResponse;
  
  { --------------------------------------------------------------------
    TImagesListResponse
    --------------------------------------------------------------------}
  
  TImagesListResponse = Class(TGoogleBaseObject)
  Private
    Fimages : TImagesListResponseTypeimagesArray;
  Protected
    //Property setters
    Procedure Setimages(AIndex : Integer; AValue : TImagesListResponseTypeimagesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property images : TImagesListResponseTypeimagesArray Index 0 Read Fimages Write Setimages;
  end;
  TImagesListResponseClass = Class of TImagesListResponse;
  
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
    TInAppProductTypelistings
    --------------------------------------------------------------------}
  
  TInAppProductTypelistings = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TInAppProductTypelistingsClass = Class of TInAppProductTypelistings;
  
  { --------------------------------------------------------------------
    TInAppProductTypeprices
    --------------------------------------------------------------------}
  
  TInAppProductTypeprices = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TInAppProductTypepricesClass = Class of TInAppProductTypeprices;
  
  { --------------------------------------------------------------------
    TInAppProduct
    --------------------------------------------------------------------}
  
  TInAppProduct = Class(TGoogleBaseObject)
  Private
    FdefaultLanguage : String;
    FdefaultPrice : TPrice;
    Flistings : TInAppProductTypelistings;
    FpackageName : String;
    Fprices : TInAppProductTypeprices;
    FpurchaseType : String;
    Fseason : TSeason;
    Fsku : String;
    Fstatus : String;
    FsubscriptionPeriod : String;
    FtrialPeriod : String;
  Protected
    //Property setters
    Procedure SetdefaultLanguage(AIndex : Integer; AValue : String); virtual;
    Procedure SetdefaultPrice(AIndex : Integer; AValue : TPrice); virtual;
    Procedure Setlistings(AIndex : Integer; AValue : TInAppProductTypelistings); virtual;
    Procedure SetpackageName(AIndex : Integer; AValue : String); virtual;
    Procedure Setprices(AIndex : Integer; AValue : TInAppProductTypeprices); virtual;
    Procedure SetpurchaseType(AIndex : Integer; AValue : String); virtual;
    Procedure Setseason(AIndex : Integer; AValue : TSeason); virtual;
    Procedure Setsku(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetsubscriptionPeriod(AIndex : Integer; AValue : String); virtual;
    Procedure SettrialPeriod(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property defaultLanguage : String Index 0 Read FdefaultLanguage Write SetdefaultLanguage;
    Property defaultPrice : TPrice Index 8 Read FdefaultPrice Write SetdefaultPrice;
    Property listings : TInAppProductTypelistings Index 16 Read Flistings Write Setlistings;
    Property packageName : String Index 24 Read FpackageName Write SetpackageName;
    Property prices : TInAppProductTypeprices Index 32 Read Fprices Write Setprices;
    Property purchaseType : String Index 40 Read FpurchaseType Write SetpurchaseType;
    Property season : TSeason Index 48 Read Fseason Write Setseason;
    Property sku : String Index 56 Read Fsku Write Setsku;
    Property status : String Index 64 Read Fstatus Write Setstatus;
    Property subscriptionPeriod : String Index 72 Read FsubscriptionPeriod Write SetsubscriptionPeriod;
    Property trialPeriod : String Index 80 Read FtrialPeriod Write SettrialPeriod;
  end;
  TInAppProductClass = Class of TInAppProduct;
  
  { --------------------------------------------------------------------
    TInAppProductListing
    --------------------------------------------------------------------}
  
  TInAppProductListing = Class(TGoogleBaseObject)
  Private
    Fdescription : String;
    Ftitle : String;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property description : String Index 0 Read Fdescription Write Setdescription;
    Property title : String Index 8 Read Ftitle Write Settitle;
  end;
  TInAppProductListingClass = Class of TInAppProductListing;
  
  { --------------------------------------------------------------------
    TInappproductsBatchRequest
    --------------------------------------------------------------------}
  
  TInappproductsBatchRequest = Class(TGoogleBaseObject)
  Private
    Fentrys : TInappproductsBatchRequestTypeentrysArray;
  Protected
    //Property setters
    Procedure Setentrys(AIndex : Integer; AValue : TInappproductsBatchRequestTypeentrysArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entrys : TInappproductsBatchRequestTypeentrysArray Index 0 Read Fentrys Write Setentrys;
  end;
  TInappproductsBatchRequestClass = Class of TInappproductsBatchRequest;
  
  { --------------------------------------------------------------------
    TInappproductsBatchRequestEntry
    --------------------------------------------------------------------}
  
  TInappproductsBatchRequestEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    Finappproductsinsertrequest : TInappproductsInsertRequest;
    Finappproductsupdaterequest : TInappproductsUpdateRequest;
    FmethodName : String;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Setinappproductsinsertrequest(AIndex : Integer; AValue : TInappproductsInsertRequest); virtual;
    Procedure Setinappproductsupdaterequest(AIndex : Integer; AValue : TInappproductsUpdateRequest); virtual;
    Procedure SetmethodName(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property inappproductsinsertrequest : TInappproductsInsertRequest Index 8 Read Finappproductsinsertrequest Write Setinappproductsinsertrequest;
    Property inappproductsupdaterequest : TInappproductsUpdateRequest Index 16 Read Finappproductsupdaterequest Write Setinappproductsupdaterequest;
    Property methodName : String Index 24 Read FmethodName Write SetmethodName;
  end;
  TInappproductsBatchRequestEntryClass = Class of TInappproductsBatchRequestEntry;
  
  { --------------------------------------------------------------------
    TInappproductsBatchResponse
    --------------------------------------------------------------------}
  
  TInappproductsBatchResponse = Class(TGoogleBaseObject)
  Private
    Fentrys : TInappproductsBatchResponseTypeentrysArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setentrys(AIndex : Integer; AValue : TInappproductsBatchResponseTypeentrysArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entrys : TInappproductsBatchResponseTypeentrysArray Index 0 Read Fentrys Write Setentrys;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TInappproductsBatchResponseClass = Class of TInappproductsBatchResponse;
  
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
    Finappproduct : TInappproductsListResponseTypeinappproductArray;
    Fkind : String;
    FpageInfo : TPageInfo;
    FtokenPagination : TTokenPagination;
  Protected
    //Property setters
    Procedure Setinappproduct(AIndex : Integer; AValue : TInappproductsListResponseTypeinappproductArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetpageInfo(AIndex : Integer; AValue : TPageInfo); virtual;
    Procedure SettokenPagination(AIndex : Integer; AValue : TTokenPagination); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property inappproduct : TInappproductsListResponseTypeinappproductArray Index 0 Read Finappproduct Write Setinappproduct;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property pageInfo : TPageInfo Index 16 Read FpageInfo Write SetpageInfo;
    Property tokenPagination : TTokenPagination Index 24 Read FtokenPagination Write SettokenPagination;
  end;
  TInappproductsListResponseClass = Class of TInappproductsListResponse;
  
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
    FfullDescription : String;
    Flanguage : String;
    FshortDescription : String;
    Ftitle : String;
    Fvideo : String;
  Protected
    //Property setters
    Procedure SetfullDescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setlanguage(AIndex : Integer; AValue : String); virtual;
    Procedure SetshortDescription(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure Setvideo(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property fullDescription : String Index 0 Read FfullDescription Write SetfullDescription;
    Property language : String Index 8 Read Flanguage Write Setlanguage;
    Property shortDescription : String Index 16 Read FshortDescription Write SetshortDescription;
    Property title : String Index 24 Read Ftitle Write Settitle;
    Property video : String Index 32 Read Fvideo Write Setvideo;
  end;
  TListingClass = Class of TListing;
  
  { --------------------------------------------------------------------
    TListingsListResponse
    --------------------------------------------------------------------}
  
  TListingsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Flistings : TListingsListResponseTypelistingsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlistings(AIndex : Integer; AValue : TListingsListResponseTypelistingsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property listings : TListingsListResponseTypelistingsArray Index 8 Read Flistings Write Setlistings;
  end;
  TListingsListResponseClass = Class of TListingsListResponse;
  
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
    Fcurrency : String;
    FpriceMicros : String;
  Protected
    //Property setters
    Procedure Setcurrency(AIndex : Integer; AValue : String); virtual;
    Procedure SetpriceMicros(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property currency : String Index 0 Read Fcurrency Write Setcurrency;
    Property priceMicros : String Index 8 Read FpriceMicros Write SetpriceMicros;
  end;
  TPriceClass = Class of TPrice;
  
  { --------------------------------------------------------------------
    TProductPurchase
    --------------------------------------------------------------------}
  
  TProductPurchase = Class(TGoogleBaseObject)
  Private
    FconsumptionState : integer;
    FdeveloperPayload : String;
    Fkind : String;
    FpurchaseState : integer;
    FpurchaseTimeMillis : String;
  Protected
    //Property setters
    Procedure SetconsumptionState(AIndex : Integer; AValue : integer); virtual;
    Procedure SetdeveloperPayload(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetpurchaseState(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpurchaseTimeMillis(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property consumptionState : integer Index 0 Read FconsumptionState Write SetconsumptionState;
    Property developerPayload : String Index 8 Read FdeveloperPayload Write SetdeveloperPayload;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property purchaseState : integer Index 24 Read FpurchaseState Write SetpurchaseState;
    Property purchaseTimeMillis : String Index 32 Read FpurchaseTimeMillis Write SetpurchaseTimeMillis;
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
    FdesiredExpiryTimeMillis : String;
    FexpectedExpiryTimeMillis : String;
  Protected
    //Property setters
    Procedure SetdesiredExpiryTimeMillis(AIndex : Integer; AValue : String); virtual;
    Procedure SetexpectedExpiryTimeMillis(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property desiredExpiryTimeMillis : String Index 0 Read FdesiredExpiryTimeMillis Write SetdesiredExpiryTimeMillis;
    Property expectedExpiryTimeMillis : String Index 8 Read FexpectedExpiryTimeMillis Write SetexpectedExpiryTimeMillis;
  end;
  TSubscriptionDeferralInfoClass = Class of TSubscriptionDeferralInfo;
  
  { --------------------------------------------------------------------
    TSubscriptionPurchase
    --------------------------------------------------------------------}
  
  TSubscriptionPurchase = Class(TGoogleBaseObject)
  Private
    FautoRenewing : boolean;
    FexpiryTimeMillis : String;
    Fkind : String;
    FstartTimeMillis : String;
  Protected
    //Property setters
    Procedure SetautoRenewing(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetexpiryTimeMillis(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartTimeMillis(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property autoRenewing : boolean Index 0 Read FautoRenewing Write SetautoRenewing;
    Property expiryTimeMillis : String Index 8 Read FexpiryTimeMillis Write SetexpiryTimeMillis;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property startTimeMillis : String Index 24 Read FstartTimeMillis Write SetstartTimeMillis;
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
    FnewExpiryTimeMillis : String;
  Protected
    //Property setters
    Procedure SetnewExpiryTimeMillis(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property newExpiryTimeMillis : String Index 0 Read FnewExpiryTimeMillis Write SetnewExpiryTimeMillis;
  end;
  TSubscriptionPurchasesDeferResponseClass = Class of TSubscriptionPurchasesDeferResponse;
  
  { --------------------------------------------------------------------
    TTesters
    --------------------------------------------------------------------}
  
  TTesters = Class(TGoogleBaseObject)
  Private
    FgoogleGroups : TStringArray;
    FgooglePlusCommunities : TStringArray;
  Protected
    //Property setters
    Procedure SetgoogleGroups(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetgooglePlusCommunities(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property googleGroups : TStringArray Index 0 Read FgoogleGroups Write SetgoogleGroups;
    Property googlePlusCommunities : TStringArray Index 8 Read FgooglePlusCommunities Write SetgooglePlusCommunities;
  end;
  TTestersClass = Class of TTesters;
  
  { --------------------------------------------------------------------
    TTokenPagination
    --------------------------------------------------------------------}
  
  TTokenPagination = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    FpreviousPageToken : String;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreviousPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property previousPageToken : String Index 8 Read FpreviousPageToken Write SetpreviousPageToken;
  end;
  TTokenPaginationClass = Class of TTokenPagination;
  
  { --------------------------------------------------------------------
    TTrack
    --------------------------------------------------------------------}
  
  TTrack = Class(TGoogleBaseObject)
  Private
    Ftrack : String;
    FuserFraction : double;
    FversionCodes : TintegerArray;
  Protected
    //Property setters
    Procedure Settrack(AIndex : Integer; AValue : String); virtual;
    Procedure SetuserFraction(AIndex : Integer; AValue : double); virtual;
    Procedure SetversionCodes(AIndex : Integer; AValue : TintegerArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property track : String Index 0 Read Ftrack Write Settrack;
    Property userFraction : double Index 8 Read FuserFraction Write SetuserFraction;
    Property versionCodes : TintegerArray Index 16 Read FversionCodes Write SetversionCodes;
  end;
  TTrackClass = Class of TTrack;
  
  { --------------------------------------------------------------------
    TTracksListResponse
    --------------------------------------------------------------------}
  
  TTracksListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Ftracks : TTracksListResponseTypetracksArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Settracks(AIndex : Integer; AValue : TTracksListResponseTypetracksArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property tracks : TTracksListResponseTypetracksArray Index 8 Read Ftracks Write Settracks;
  end;
  TTracksListResponseClass = Class of TTracksListResponse;
  
  { --------------------------------------------------------------------
    TEditsApklistingsResource
    --------------------------------------------------------------------}
  
  TEditsApklistingsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(apkVersionCode: integer; editId: string; language: string; packageName: string);
    Procedure Deleteall(apkVersionCode: integer; editId: string; packageName: string);
    Function Get(apkVersionCode: integer; editId: string; language: string; packageName: string) : TApkListing;
    Function List(apkVersionCode: integer; editId: string; packageName: string) : TApkListingsListResponse;
    Function Patch(apkVersionCode: integer; editId: string; language: string; packageName: string; aApkListing : TApkListing) : TApkListing;
    Function Update(apkVersionCode: integer; editId: string; language: string; packageName: string; aApkListing : TApkListing) : TApkListing;
  end;
  
  
  { --------------------------------------------------------------------
    TEditsApksResource
    --------------------------------------------------------------------}
  
  TEditsApksResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Addexternallyhosted(editId: string; packageName: string; aApksAddExternallyHostedRequest : TApksAddExternallyHostedRequest) : TApksAddExternallyHostedResponse;
    Function List(editId: string; packageName: string) : TApksListResponse;
    Function Upload(editId: string; packageName: string) : TApk;
  end;
  
  
  { --------------------------------------------------------------------
    TEditsDetailsResource
    --------------------------------------------------------------------}
  
  TEditsDetailsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(editId: string; packageName: string) : TAppDetails;
    Function Patch(editId: string; packageName: string; aAppDetails : TAppDetails) : TAppDetails;
    Function Update(editId: string; packageName: string; aAppDetails : TAppDetails) : TAppDetails;
  end;
  
  
  { --------------------------------------------------------------------
    TEditsExpansionfilesResource
    --------------------------------------------------------------------}
  
  TEditsExpansionfilesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(apkVersionCode: integer; editId: string; expansionFileType: string; packageName: string) : TExpansionFile;
    Function Patch(apkVersionCode: integer; editId: string; expansionFileType: string; packageName: string; aExpansionFile : TExpansionFile) : TExpansionFile;
    Function Update(apkVersionCode: integer; editId: string; expansionFileType: string; packageName: string; aExpansionFile : TExpansionFile) : TExpansionFile;
    Function Upload(apkVersionCode: integer; editId: string; expansionFileType: string; packageName: string) : TExpansionFilesUploadResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TEditsImagesResource
    --------------------------------------------------------------------}
  
  TEditsImagesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(editId: string; imageId: string; imageType: string; language: string; packageName: string);
    Function Deleteall(editId: string; imageType: string; language: string; packageName: string) : TImagesDeleteAllResponse;
    Function List(editId: string; imageType: string; language: string; packageName: string) : TImagesListResponse;
    Function Upload(editId: string; imageType: string; language: string; packageName: string) : TImagesUploadResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TEditsListingsResource
    --------------------------------------------------------------------}
  
  TEditsListingsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(editId: string; language: string; packageName: string);
    Procedure Deleteall(editId: string; packageName: string);
    Function Get(editId: string; language: string; packageName: string) : TListing;
    Function List(editId: string; packageName: string) : TListingsListResponse;
    Function Patch(editId: string; language: string; packageName: string; aListing : TListing) : TListing;
    Function Update(editId: string; language: string; packageName: string; aListing : TListing) : TListing;
  end;
  
  
  { --------------------------------------------------------------------
    TEditsTestersResource
    --------------------------------------------------------------------}
  
  TEditsTestersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(editId: string; packageName: string; track: string) : TTesters;
    Function Patch(editId: string; packageName: string; track: string; aTesters : TTesters) : TTesters;
    Function Update(editId: string; packageName: string; track: string; aTesters : TTesters) : TTesters;
  end;
  
  
  { --------------------------------------------------------------------
    TEditsTracksResource
    --------------------------------------------------------------------}
  
  TEditsTracksResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(editId: string; packageName: string; track: string) : TTrack;
    Function List(editId: string; packageName: string) : TTracksListResponse;
    Function Patch(editId: string; packageName: string; track: string; aTrack : TTrack) : TTrack;
    Function Update(editId: string; packageName: string; track: string; aTrack : TTrack) : TTrack;
  end;
  
  
  { --------------------------------------------------------------------
    TEditsResource
    --------------------------------------------------------------------}
  
  TEditsResource = Class(TGoogleResource)
  Private
    FApklistingsInstance : TEditsApklistingsResource;
    FApksInstance : TEditsApksResource;
    FDetailsInstance : TEditsDetailsResource;
    FExpansionfilesInstance : TEditsExpansionfilesResource;
    FImagesInstance : TEditsImagesResource;
    FListingsInstance : TEditsListingsResource;
    FTestersInstance : TEditsTestersResource;
    FTracksInstance : TEditsTracksResource;
    Function GetApklistingsInstance : TEditsApklistingsResource;virtual;
    Function GetApksInstance : TEditsApksResource;virtual;
    Function GetDetailsInstance : TEditsDetailsResource;virtual;
    Function GetExpansionfilesInstance : TEditsExpansionfilesResource;virtual;
    Function GetImagesInstance : TEditsImagesResource;virtual;
    Function GetListingsInstance : TEditsListingsResource;virtual;
    Function GetTestersInstance : TEditsTestersResource;virtual;
    Function GetTracksInstance : TEditsTracksResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Commit(editId: string; packageName: string) : TAppEdit;
    Procedure Delete(editId: string; packageName: string);
    Function Get(editId: string; packageName: string) : TAppEdit;
    Function Insert(packageName: string; aAppEdit : TAppEdit) : TAppEdit;
    Function Validate(editId: string; packageName: string) : TAppEdit;
    Function CreateApklistingsResource(AOwner : TComponent) : TEditsApklistingsResource;virtual;overload;
    Function CreateApklistingsResource : TEditsApklistingsResource;virtual;overload;
    Function CreateApksResource(AOwner : TComponent) : TEditsApksResource;virtual;overload;
    Function CreateApksResource : TEditsApksResource;virtual;overload;
    Function CreateDetailsResource(AOwner : TComponent) : TEditsDetailsResource;virtual;overload;
    Function CreateDetailsResource : TEditsDetailsResource;virtual;overload;
    Function CreateExpansionfilesResource(AOwner : TComponent) : TEditsExpansionfilesResource;virtual;overload;
    Function CreateExpansionfilesResource : TEditsExpansionfilesResource;virtual;overload;
    Function CreateImagesResource(AOwner : TComponent) : TEditsImagesResource;virtual;overload;
    Function CreateImagesResource : TEditsImagesResource;virtual;overload;
    Function CreateListingsResource(AOwner : TComponent) : TEditsListingsResource;virtual;overload;
    Function CreateListingsResource : TEditsListingsResource;virtual;overload;
    Function CreateTestersResource(AOwner : TComponent) : TEditsTestersResource;virtual;overload;
    Function CreateTestersResource : TEditsTestersResource;virtual;overload;
    Function CreateTracksResource(AOwner : TComponent) : TEditsTracksResource;virtual;overload;
    Function CreateTracksResource : TEditsTracksResource;virtual;overload;
    Property ApklistingsResource : TEditsApklistingsResource Read GetApklistingsInstance;
    Property ApksResource : TEditsApksResource Read GetApksInstance;
    Property DetailsResource : TEditsDetailsResource Read GetDetailsInstance;
    Property ExpansionfilesResource : TEditsExpansionfilesResource Read GetExpansionfilesInstance;
    Property ImagesResource : TEditsImagesResource Read GetImagesInstance;
    Property ListingsResource : TEditsListingsResource Read GetListingsInstance;
    Property TestersResource : TEditsTestersResource Read GetTestersInstance;
    Property TracksResource : TEditsTracksResource Read GetTracksInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TEntitlementsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TEntitlementsResource, method List
  
  TEntitlementsListOptions = Record
    maxResults : integer;
    productId : String;
    startIndex : integer;
    token : String;
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
    token : String;
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
    TPurchasesProductsResource
    --------------------------------------------------------------------}
  
  TPurchasesProductsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(packageName: string; productId: string; token: string) : TProductPurchase;
  end;
  
  
  { --------------------------------------------------------------------
    TPurchasesSubscriptionsResource
    --------------------------------------------------------------------}
  
  TPurchasesSubscriptionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Cancel(packageName: string; subscriptionId: string; token: string);
    Function Defer(packageName: string; subscriptionId: string; token: string; aSubscriptionPurchasesDeferRequest : TSubscriptionPurchasesDeferRequest) : TSubscriptionPurchasesDeferResponse;
    Function Get(packageName: string; subscriptionId: string; token: string) : TSubscriptionPurchase;
    Procedure Refund(packageName: string; subscriptionId: string; token: string);
    Procedure Revoke(packageName: string; subscriptionId: string; token: string);
  end;
  
  
  { --------------------------------------------------------------------
    TPurchasesResource
    --------------------------------------------------------------------}
  
  TPurchasesResource = Class(TGoogleResource)
  Private
    FProductsInstance : TPurchasesProductsResource;
    FSubscriptionsInstance : TPurchasesSubscriptionsResource;
    Function GetProductsInstance : TPurchasesProductsResource;virtual;
    Function GetSubscriptionsInstance : TPurchasesSubscriptionsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateProductsResource(AOwner : TComponent) : TPurchasesProductsResource;virtual;overload;
    Function CreateProductsResource : TPurchasesProductsResource;virtual;overload;
    Function CreateSubscriptionsResource(AOwner : TComponent) : TPurchasesSubscriptionsResource;virtual;overload;
    Function CreateSubscriptionsResource : TPurchasesSubscriptionsResource;virtual;overload;
    Property ProductsResource : TPurchasesProductsResource Read GetProductsInstance;
    Property SubscriptionsResource : TPurchasesSubscriptionsResource Read GetSubscriptionsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TAndroidpublisherAPI
    --------------------------------------------------------------------}
  
  TAndroidpublisherAPI = Class(TGoogleAPI)
  Private
    FEditsApklistingsInstance : TEditsApklistingsResource;
    FEditsApksInstance : TEditsApksResource;
    FEditsDetailsInstance : TEditsDetailsResource;
    FEditsExpansionfilesInstance : TEditsExpansionfilesResource;
    FEditsImagesInstance : TEditsImagesResource;
    FEditsListingsInstance : TEditsListingsResource;
    FEditsTestersInstance : TEditsTestersResource;
    FEditsTracksInstance : TEditsTracksResource;
    FEditsInstance : TEditsResource;
    FEntitlementsInstance : TEntitlementsResource;
    FInappproductsInstance : TInappproductsResource;
    FPurchasesProductsInstance : TPurchasesProductsResource;
    FPurchasesSubscriptionsInstance : TPurchasesSubscriptionsResource;
    FPurchasesInstance : TPurchasesResource;
    Function GetEditsApklistingsInstance : TEditsApklistingsResource;virtual;
    Function GetEditsApksInstance : TEditsApksResource;virtual;
    Function GetEditsDetailsInstance : TEditsDetailsResource;virtual;
    Function GetEditsExpansionfilesInstance : TEditsExpansionfilesResource;virtual;
    Function GetEditsImagesInstance : TEditsImagesResource;virtual;
    Function GetEditsListingsInstance : TEditsListingsResource;virtual;
    Function GetEditsTestersInstance : TEditsTestersResource;virtual;
    Function GetEditsTracksInstance : TEditsTracksResource;virtual;
    Function GetEditsInstance : TEditsResource;virtual;
    Function GetEntitlementsInstance : TEntitlementsResource;virtual;
    Function GetInappproductsInstance : TInappproductsResource;virtual;
    Function GetPurchasesProductsInstance : TPurchasesProductsResource;virtual;
    Function GetPurchasesSubscriptionsInstance : TPurchasesSubscriptionsResource;virtual;
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
    Function CreateEditsApklistingsResource(AOwner : TComponent) : TEditsApklistingsResource;virtual;overload;
    Function CreateEditsApklistingsResource : TEditsApklistingsResource;virtual;overload;
    Function CreateEditsApksResource(AOwner : TComponent) : TEditsApksResource;virtual;overload;
    Function CreateEditsApksResource : TEditsApksResource;virtual;overload;
    Function CreateEditsDetailsResource(AOwner : TComponent) : TEditsDetailsResource;virtual;overload;
    Function CreateEditsDetailsResource : TEditsDetailsResource;virtual;overload;
    Function CreateEditsExpansionfilesResource(AOwner : TComponent) : TEditsExpansionfilesResource;virtual;overload;
    Function CreateEditsExpansionfilesResource : TEditsExpansionfilesResource;virtual;overload;
    Function CreateEditsImagesResource(AOwner : TComponent) : TEditsImagesResource;virtual;overload;
    Function CreateEditsImagesResource : TEditsImagesResource;virtual;overload;
    Function CreateEditsListingsResource(AOwner : TComponent) : TEditsListingsResource;virtual;overload;
    Function CreateEditsListingsResource : TEditsListingsResource;virtual;overload;
    Function CreateEditsTestersResource(AOwner : TComponent) : TEditsTestersResource;virtual;overload;
    Function CreateEditsTestersResource : TEditsTestersResource;virtual;overload;
    Function CreateEditsTracksResource(AOwner : TComponent) : TEditsTracksResource;virtual;overload;
    Function CreateEditsTracksResource : TEditsTracksResource;virtual;overload;
    Function CreateEditsResource(AOwner : TComponent) : TEditsResource;virtual;overload;
    Function CreateEditsResource : TEditsResource;virtual;overload;
    Function CreateEntitlementsResource(AOwner : TComponent) : TEntitlementsResource;virtual;overload;
    Function CreateEntitlementsResource : TEntitlementsResource;virtual;overload;
    Function CreateInappproductsResource(AOwner : TComponent) : TInappproductsResource;virtual;overload;
    Function CreateInappproductsResource : TInappproductsResource;virtual;overload;
    Function CreatePurchasesProductsResource(AOwner : TComponent) : TPurchasesProductsResource;virtual;overload;
    Function CreatePurchasesProductsResource : TPurchasesProductsResource;virtual;overload;
    Function CreatePurchasesSubscriptionsResource(AOwner : TComponent) : TPurchasesSubscriptionsResource;virtual;overload;
    Function CreatePurchasesSubscriptionsResource : TPurchasesSubscriptionsResource;virtual;overload;
    Function CreatePurchasesResource(AOwner : TComponent) : TPurchasesResource;virtual;overload;
    Function CreatePurchasesResource : TPurchasesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property EditsApklistingsResource : TEditsApklistingsResource Read GetEditsApklistingsInstance;
    Property EditsApksResource : TEditsApksResource Read GetEditsApksInstance;
    Property EditsDetailsResource : TEditsDetailsResource Read GetEditsDetailsInstance;
    Property EditsExpansionfilesResource : TEditsExpansionfilesResource Read GetEditsExpansionfilesInstance;
    Property EditsImagesResource : TEditsImagesResource Read GetEditsImagesInstance;
    Property EditsListingsResource : TEditsListingsResource Read GetEditsListingsInstance;
    Property EditsTestersResource : TEditsTestersResource Read GetEditsTestersInstance;
    Property EditsTracksResource : TEditsTracksResource Read GetEditsTracksInstance;
    Property EditsResource : TEditsResource Read GetEditsInstance;
    Property EntitlementsResource : TEntitlementsResource Read GetEntitlementsInstance;
    Property InappproductsResource : TInappproductsResource Read GetInappproductsInstance;
    Property PurchasesProductsResource : TPurchasesProductsResource Read GetPurchasesProductsInstance;
    Property PurchasesSubscriptionsResource : TPurchasesSubscriptionsResource Read GetPurchasesSubscriptionsInstance;
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


Procedure TApkBinary.Setsha1(AIndex : Integer; AValue : String); 

begin
  If (Fsha1=AValue) then exit;
  Fsha1:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TApkListing
  --------------------------------------------------------------------}


Procedure TApkListing.Setlanguage(AIndex : Integer; AValue : String); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApkListing.SetrecentChanges(AIndex : Integer; AValue : String); 

begin
  If (FrecentChanges=AValue) then exit;
  FrecentChanges:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TApkListingsListResponse
  --------------------------------------------------------------------}


Procedure TApkListingsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApkListingsListResponse.Setlistings(AIndex : Integer; AValue : TApkListingsListResponseTypelistingsArray); 

begin
  If (Flistings=AValue) then exit;
  Flistings:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TApkListingsListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'listings' : SetLength(Flistings,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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


Procedure TApksListResponse.Setapks(AIndex : Integer; AValue : TApksListResponseTypeapksArray); 

begin
  If (Fapks=AValue) then exit;
  Fapks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApksListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TApksListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'apks' : SetLength(Fapks,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAppDetails
  --------------------------------------------------------------------}


Procedure TAppDetails.SetcontactEmail(AIndex : Integer; AValue : String); 

begin
  If (FcontactEmail=AValue) then exit;
  FcontactEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppDetails.SetcontactPhone(AIndex : Integer; AValue : String); 

begin
  If (FcontactPhone=AValue) then exit;
  FcontactPhone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppDetails.SetcontactWebsite(AIndex : Integer; AValue : String); 

begin
  If (FcontactWebsite=AValue) then exit;
  FcontactWebsite:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppDetails.SetdefaultLanguage(AIndex : Integer; AValue : String); 

begin
  If (FdefaultLanguage=AValue) then exit;
  FdefaultLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAppEdit
  --------------------------------------------------------------------}


Procedure TAppEdit.SetexpiryTimeSeconds(AIndex : Integer; AValue : String); 

begin
  If (FexpiryTimeSeconds=AValue) then exit;
  FexpiryTimeSeconds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppEdit.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEntitlement
  --------------------------------------------------------------------}


Procedure TEntitlement.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntitlement.SetproductId(AIndex : Integer; AValue : String); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntitlement.SetproductType(AIndex : Integer; AValue : String); 

begin
  If (FproductType=AValue) then exit;
  FproductType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntitlement.Settoken(AIndex : Integer; AValue : String); 

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



Procedure TEntitlementsListResponse.Setresources(AIndex : Integer; AValue : TEntitlementsListResponseTyperesourcesArray); 

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


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TEntitlementsListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'resources' : SetLength(Fresources,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TExpansionFile
  --------------------------------------------------------------------}


Procedure TExpansionFile.SetfileSize(AIndex : Integer; AValue : String); 

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


Procedure TExternallyHostedApk.SetapplicationLabel(AIndex : Integer; AValue : String); 

begin
  If (FapplicationLabel=AValue) then exit;
  FapplicationLabel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SetcertificateBase64s(AIndex : Integer; AValue : TStringArray); 

begin
  If (FcertificateBase64s=AValue) then exit;
  FcertificateBase64s:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SetexternallyHostedUrl(AIndex : Integer; AValue : String); 

begin
  If (FexternallyHostedUrl=AValue) then exit;
  FexternallyHostedUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SetfileSha1Base64(AIndex : Integer; AValue : String); 

begin
  If (FfileSha1Base64=AValue) then exit;
  FfileSha1Base64:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SetfileSha256Base64(AIndex : Integer; AValue : String); 

begin
  If (FfileSha256Base64=AValue) then exit;
  FfileSha256Base64:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SetfileSize(AIndex : Integer; AValue : String); 

begin
  If (FfileSize=AValue) then exit;
  FfileSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SeticonBase64(AIndex : Integer; AValue : String); 

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



Procedure TExternallyHostedApk.SetnativeCodes(AIndex : Integer; AValue : TStringArray); 

begin
  If (FnativeCodes=AValue) then exit;
  FnativeCodes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SetpackageName(AIndex : Integer; AValue : String); 

begin
  If (FpackageName=AValue) then exit;
  FpackageName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SetusesFeatures(AIndex : Integer; AValue : TStringArray); 

begin
  If (FusesFeatures=AValue) then exit;
  FusesFeatures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApk.SetusesPermissions(AIndex : Integer; AValue : TExternallyHostedApkTypeusesPermissionsArray); 

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



Procedure TExternallyHostedApk.SetversionName(AIndex : Integer; AValue : String); 

begin
  If (FversionName=AValue) then exit;
  FversionName:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TExternallyHostedApk.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'certificatebase64s' : SetLength(FcertificateBase64s,ALength);
  'nativecodes' : SetLength(FnativeCodes,ALength);
  'usesfeatures' : SetLength(FusesFeatures,ALength);
  'usespermissions' : SetLength(FusesPermissions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TExternallyHostedApkUsesPermission
  --------------------------------------------------------------------}


Procedure TExternallyHostedApkUsesPermission.SetmaxSdkVersion(AIndex : Integer; AValue : integer); 

begin
  If (FmaxSdkVersion=AValue) then exit;
  FmaxSdkVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternallyHostedApkUsesPermission.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImage
  --------------------------------------------------------------------}


Procedure TImage.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImage.Setsha1(AIndex : Integer; AValue : String); 

begin
  If (Fsha1=AValue) then exit;
  Fsha1:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImage.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImagesDeleteAllResponse
  --------------------------------------------------------------------}


Procedure TImagesDeleteAllResponse.Setdeleted(AIndex : Integer; AValue : TImagesDeleteAllResponseTypedeletedArray); 

begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TImagesDeleteAllResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'deleted' : SetLength(Fdeleted,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TImagesListResponse
  --------------------------------------------------------------------}


Procedure TImagesListResponse.Setimages(AIndex : Integer; AValue : TImagesListResponseTypeimagesArray); 

begin
  If (Fimages=AValue) then exit;
  Fimages:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TImagesListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'images' : SetLength(Fimages,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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
  TInAppProductTypelistings
  --------------------------------------------------------------------}


Class Function TInAppProductTypelistings.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TInAppProductTypeprices
  --------------------------------------------------------------------}


Class Function TInAppProductTypeprices.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TInAppProduct
  --------------------------------------------------------------------}


Procedure TInAppProduct.SetdefaultLanguage(AIndex : Integer; AValue : String); 

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



Procedure TInAppProduct.Setlistings(AIndex : Integer; AValue : TInAppProductTypelistings); 

begin
  If (Flistings=AValue) then exit;
  Flistings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInAppProduct.SetpackageName(AIndex : Integer; AValue : String); 

begin
  If (FpackageName=AValue) then exit;
  FpackageName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInAppProduct.Setprices(AIndex : Integer; AValue : TInAppProductTypeprices); 

begin
  If (Fprices=AValue) then exit;
  Fprices:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInAppProduct.SetpurchaseType(AIndex : Integer; AValue : String); 

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



Procedure TInAppProduct.Setsku(AIndex : Integer; AValue : String); 

begin
  If (Fsku=AValue) then exit;
  Fsku:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInAppProduct.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInAppProduct.SetsubscriptionPeriod(AIndex : Integer; AValue : String); 

begin
  If (FsubscriptionPeriod=AValue) then exit;
  FsubscriptionPeriod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInAppProduct.SettrialPeriod(AIndex : Integer; AValue : String); 

begin
  If (FtrialPeriod=AValue) then exit;
  FtrialPeriod:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInAppProductListing
  --------------------------------------------------------------------}


Procedure TInAppProductListing.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInAppProductListing.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInappproductsBatchRequest
  --------------------------------------------------------------------}


Procedure TInappproductsBatchRequest.Setentrys(AIndex : Integer; AValue : TInappproductsBatchRequestTypeentrysArray); 

begin
  If (Fentrys=AValue) then exit;
  Fentrys:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TInappproductsBatchRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entrys' : SetLength(Fentrys,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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



Procedure TInappproductsBatchRequestEntry.SetmethodName(AIndex : Integer; AValue : String); 

begin
  If (FmethodName=AValue) then exit;
  FmethodName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInappproductsBatchResponse
  --------------------------------------------------------------------}


Procedure TInappproductsBatchResponse.Setentrys(AIndex : Integer; AValue : TInappproductsBatchResponseTypeentrysArray); 

begin
  If (Fentrys=AValue) then exit;
  Fentrys:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInappproductsBatchResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TInappproductsBatchResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entrys' : SetLength(Fentrys,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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


Procedure TInappproductsListResponse.Setinappproduct(AIndex : Integer; AValue : TInappproductsListResponseTypeinappproductArray); 

begin
  If (Finappproduct=AValue) then exit;
  Finappproduct:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInappproductsListResponse.Setkind(AIndex : Integer; AValue : String); 

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


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TInappproductsListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'inappproduct' : SetLength(Finappproduct,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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


Procedure TListing.SetfullDescription(AIndex : Integer; AValue : String); 

begin
  If (FfullDescription=AValue) then exit;
  FfullDescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListing.Setlanguage(AIndex : Integer; AValue : String); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListing.SetshortDescription(AIndex : Integer; AValue : String); 

begin
  If (FshortDescription=AValue) then exit;
  FshortDescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListing.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListing.Setvideo(AIndex : Integer; AValue : String); 

begin
  If (Fvideo=AValue) then exit;
  Fvideo:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListingsListResponse
  --------------------------------------------------------------------}


Procedure TListingsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListingsListResponse.Setlistings(AIndex : Integer; AValue : TListingsListResponseTypelistingsArray); 

begin
  If (Flistings=AValue) then exit;
  Flistings:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListingsListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'listings' : SetLength(Flistings,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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


Procedure TPrice.Setcurrency(AIndex : Integer; AValue : String); 

begin
  If (Fcurrency=AValue) then exit;
  Fcurrency:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPrice.SetpriceMicros(AIndex : Integer; AValue : String); 

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



Procedure TProductPurchase.SetdeveloperPayload(AIndex : Integer; AValue : String); 

begin
  If (FdeveloperPayload=AValue) then exit;
  FdeveloperPayload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductPurchase.Setkind(AIndex : Integer; AValue : String); 

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



Procedure TProductPurchase.SetpurchaseTimeMillis(AIndex : Integer; AValue : String); 

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


Procedure TSubscriptionDeferralInfo.SetdesiredExpiryTimeMillis(AIndex : Integer; AValue : String); 

begin
  If (FdesiredExpiryTimeMillis=AValue) then exit;
  FdesiredExpiryTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionDeferralInfo.SetexpectedExpiryTimeMillis(AIndex : Integer; AValue : String); 

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



Procedure TSubscriptionPurchase.SetexpiryTimeMillis(AIndex : Integer; AValue : String); 

begin
  If (FexpiryTimeMillis=AValue) then exit;
  FexpiryTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionPurchase.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionPurchase.SetstartTimeMillis(AIndex : Integer; AValue : String); 

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


Procedure TSubscriptionPurchasesDeferResponse.SetnewExpiryTimeMillis(AIndex : Integer; AValue : String); 

begin
  If (FnewExpiryTimeMillis=AValue) then exit;
  FnewExpiryTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTesters
  --------------------------------------------------------------------}


Procedure TTesters.SetgoogleGroups(AIndex : Integer; AValue : TStringArray); 

begin
  If (FgoogleGroups=AValue) then exit;
  FgoogleGroups:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTesters.SetgooglePlusCommunities(AIndex : Integer; AValue : TStringArray); 

begin
  If (FgooglePlusCommunities=AValue) then exit;
  FgooglePlusCommunities:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTesters.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'googlegroups' : SetLength(FgoogleGroups,ALength);
  'googlepluscommunities' : SetLength(FgooglePlusCommunities,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTokenPagination
  --------------------------------------------------------------------}


Procedure TTokenPagination.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTokenPagination.SetpreviousPageToken(AIndex : Integer; AValue : String); 

begin
  If (FpreviousPageToken=AValue) then exit;
  FpreviousPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTrack
  --------------------------------------------------------------------}


Procedure TTrack.Settrack(AIndex : Integer; AValue : String); 

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



Procedure TTrack.SetversionCodes(AIndex : Integer; AValue : TintegerArray); 

begin
  If (FversionCodes=AValue) then exit;
  FversionCodes:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTrack.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'versioncodes' : SetLength(FversionCodes,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTracksListResponse
  --------------------------------------------------------------------}


Procedure TTracksListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTracksListResponse.Settracks(AIndex : Integer; AValue : TTracksListResponseTypetracksArray); 

begin
  If (Ftracks=AValue) then exit;
  Ftracks:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTracksListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'tracks' : SetLength(Ftracks,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEditsApklistingsResource
  --------------------------------------------------------------------}


Class Function TEditsApklistingsResource.ResourceName : String;

begin
  Result:='apklistings';
end;

Class Function TEditsApklistingsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidpublisherAPI;
end;

Procedure TEditsApklistingsResource.Delete(apkVersionCode: integer; editId: string; language: string; packageName: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{packageName}/edits/{editId}/apks/{apkVersionCode}/listings/{language}';
  _Methodid   = 'androidpublisher.edits.apklistings.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['apkVersionCode',apkVersionCode,'editId',editId,'language',language,'packageName',packageName]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Procedure TEditsApklistingsResource.Deleteall(apkVersionCode: integer; editId: string; packageName: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{packageName}/edits/{editId}/apks/{apkVersionCode}/listings';
  _Methodid   = 'androidpublisher.edits.apklistings.deleteall';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['apkVersionCode',apkVersionCode,'editId',editId,'packageName',packageName]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TEditsApklistingsResource.Get(apkVersionCode: integer; editId: string; language: string; packageName: string) : TApkListing;

Const
  _HTTPMethod = 'GET';
  _Path       = '{packageName}/edits/{editId}/apks/{apkVersionCode}/listings/{language}';
  _Methodid   = 'androidpublisher.edits.apklistings.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['apkVersionCode',apkVersionCode,'editId',editId,'language',language,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TApkListing) as TApkListing;
end;

Function TEditsApklistingsResource.List(apkVersionCode: integer; editId: string; packageName: string) : TApkListingsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{packageName}/edits/{editId}/apks/{apkVersionCode}/listings';
  _Methodid   = 'androidpublisher.edits.apklistings.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['apkVersionCode',apkVersionCode,'editId',editId,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TApkListingsListResponse) as TApkListingsListResponse;
end;

Function TEditsApklistingsResource.Patch(apkVersionCode: integer; editId: string; language: string; packageName: string; aApkListing : TApkListing) : TApkListing;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{packageName}/edits/{editId}/apks/{apkVersionCode}/listings/{language}';
  _Methodid   = 'androidpublisher.edits.apklistings.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['apkVersionCode',apkVersionCode,'editId',editId,'language',language,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aApkListing,TApkListing) as TApkListing;
end;

Function TEditsApklistingsResource.Update(apkVersionCode: integer; editId: string; language: string; packageName: string; aApkListing : TApkListing) : TApkListing;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{packageName}/edits/{editId}/apks/{apkVersionCode}/listings/{language}';
  _Methodid   = 'androidpublisher.edits.apklistings.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['apkVersionCode',apkVersionCode,'editId',editId,'language',language,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aApkListing,TApkListing) as TApkListing;
end;



{ --------------------------------------------------------------------
  TEditsApksResource
  --------------------------------------------------------------------}


Class Function TEditsApksResource.ResourceName : String;

begin
  Result:='apks';
end;

Class Function TEditsApksResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidpublisherAPI;
end;

Function TEditsApksResource.Addexternallyhosted(editId: string; packageName: string; aApksAddExternallyHostedRequest : TApksAddExternallyHostedRequest) : TApksAddExternallyHostedResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{packageName}/edits/{editId}/apks/externallyHosted';
  _Methodid   = 'androidpublisher.edits.apks.addexternallyhosted';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aApksAddExternallyHostedRequest,TApksAddExternallyHostedResponse) as TApksAddExternallyHostedResponse;
end;

Function TEditsApksResource.List(editId: string; packageName: string) : TApksListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{packageName}/edits/{editId}/apks';
  _Methodid   = 'androidpublisher.edits.apks.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TApksListResponse) as TApksListResponse;
end;

Function TEditsApksResource.Upload(editId: string; packageName: string) : TApk;

Const
  _HTTPMethod = 'POST';
  _Path       = '{packageName}/edits/{editId}/apks';
  _Methodid   = 'androidpublisher.edits.apks.upload';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TApk) as TApk;
end;



{ --------------------------------------------------------------------
  TEditsDetailsResource
  --------------------------------------------------------------------}


Class Function TEditsDetailsResource.ResourceName : String;

begin
  Result:='details';
end;

Class Function TEditsDetailsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidpublisherAPI;
end;

Function TEditsDetailsResource.Get(editId: string; packageName: string) : TAppDetails;

Const
  _HTTPMethod = 'GET';
  _Path       = '{packageName}/edits/{editId}/details';
  _Methodid   = 'androidpublisher.edits.details.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAppDetails) as TAppDetails;
end;

Function TEditsDetailsResource.Patch(editId: string; packageName: string; aAppDetails : TAppDetails) : TAppDetails;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{packageName}/edits/{editId}/details';
  _Methodid   = 'androidpublisher.edits.details.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAppDetails,TAppDetails) as TAppDetails;
end;

Function TEditsDetailsResource.Update(editId: string; packageName: string; aAppDetails : TAppDetails) : TAppDetails;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{packageName}/edits/{editId}/details';
  _Methodid   = 'androidpublisher.edits.details.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAppDetails,TAppDetails) as TAppDetails;
end;



{ --------------------------------------------------------------------
  TEditsExpansionfilesResource
  --------------------------------------------------------------------}


Class Function TEditsExpansionfilesResource.ResourceName : String;

begin
  Result:='expansionfiles';
end;

Class Function TEditsExpansionfilesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidpublisherAPI;
end;

Function TEditsExpansionfilesResource.Get(apkVersionCode: integer; editId: string; expansionFileType: string; packageName: string) : TExpansionFile;

Const
  _HTTPMethod = 'GET';
  _Path       = '{packageName}/edits/{editId}/apks/{apkVersionCode}/expansionFiles/{expansionFileType}';
  _Methodid   = 'androidpublisher.edits.expansionfiles.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['apkVersionCode',apkVersionCode,'editId',editId,'expansionFileType',expansionFileType,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TExpansionFile) as TExpansionFile;
end;

Function TEditsExpansionfilesResource.Patch(apkVersionCode: integer; editId: string; expansionFileType: string; packageName: string; aExpansionFile : TExpansionFile) : TExpansionFile;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{packageName}/edits/{editId}/apks/{apkVersionCode}/expansionFiles/{expansionFileType}';
  _Methodid   = 'androidpublisher.edits.expansionfiles.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['apkVersionCode',apkVersionCode,'editId',editId,'expansionFileType',expansionFileType,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aExpansionFile,TExpansionFile) as TExpansionFile;
end;

Function TEditsExpansionfilesResource.Update(apkVersionCode: integer; editId: string; expansionFileType: string; packageName: string; aExpansionFile : TExpansionFile) : TExpansionFile;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{packageName}/edits/{editId}/apks/{apkVersionCode}/expansionFiles/{expansionFileType}';
  _Methodid   = 'androidpublisher.edits.expansionfiles.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['apkVersionCode',apkVersionCode,'editId',editId,'expansionFileType',expansionFileType,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aExpansionFile,TExpansionFile) as TExpansionFile;
end;

Function TEditsExpansionfilesResource.Upload(apkVersionCode: integer; editId: string; expansionFileType: string; packageName: string) : TExpansionFilesUploadResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{packageName}/edits/{editId}/apks/{apkVersionCode}/expansionFiles/{expansionFileType}';
  _Methodid   = 'androidpublisher.edits.expansionfiles.upload';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['apkVersionCode',apkVersionCode,'editId',editId,'expansionFileType',expansionFileType,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TExpansionFilesUploadResponse) as TExpansionFilesUploadResponse;
end;



{ --------------------------------------------------------------------
  TEditsImagesResource
  --------------------------------------------------------------------}


Class Function TEditsImagesResource.ResourceName : String;

begin
  Result:='images';
end;

Class Function TEditsImagesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidpublisherAPI;
end;

Procedure TEditsImagesResource.Delete(editId: string; imageId: string; imageType: string; language: string; packageName: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{packageName}/edits/{editId}/listings/{language}/{imageType}/{imageId}';
  _Methodid   = 'androidpublisher.edits.images.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'imageId',imageId,'imageType',imageType,'language',language,'packageName',packageName]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TEditsImagesResource.Deleteall(editId: string; imageType: string; language: string; packageName: string) : TImagesDeleteAllResponse;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{packageName}/edits/{editId}/listings/{language}/{imageType}';
  _Methodid   = 'androidpublisher.edits.images.deleteall';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'imageType',imageType,'language',language,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TImagesDeleteAllResponse) as TImagesDeleteAllResponse;
end;

Function TEditsImagesResource.List(editId: string; imageType: string; language: string; packageName: string) : TImagesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{packageName}/edits/{editId}/listings/{language}/{imageType}';
  _Methodid   = 'androidpublisher.edits.images.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'imageType',imageType,'language',language,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TImagesListResponse) as TImagesListResponse;
end;

Function TEditsImagesResource.Upload(editId: string; imageType: string; language: string; packageName: string) : TImagesUploadResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{packageName}/edits/{editId}/listings/{language}/{imageType}';
  _Methodid   = 'androidpublisher.edits.images.upload';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'imageType',imageType,'language',language,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TImagesUploadResponse) as TImagesUploadResponse;
end;



{ --------------------------------------------------------------------
  TEditsListingsResource
  --------------------------------------------------------------------}


Class Function TEditsListingsResource.ResourceName : String;

begin
  Result:='listings';
end;

Class Function TEditsListingsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidpublisherAPI;
end;

Procedure TEditsListingsResource.Delete(editId: string; language: string; packageName: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{packageName}/edits/{editId}/listings/{language}';
  _Methodid   = 'androidpublisher.edits.listings.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'language',language,'packageName',packageName]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Procedure TEditsListingsResource.Deleteall(editId: string; packageName: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{packageName}/edits/{editId}/listings';
  _Methodid   = 'androidpublisher.edits.listings.deleteall';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'packageName',packageName]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TEditsListingsResource.Get(editId: string; language: string; packageName: string) : TListing;

Const
  _HTTPMethod = 'GET';
  _Path       = '{packageName}/edits/{editId}/listings/{language}';
  _Methodid   = 'androidpublisher.edits.listings.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'language',language,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TListing) as TListing;
end;

Function TEditsListingsResource.List(editId: string; packageName: string) : TListingsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{packageName}/edits/{editId}/listings';
  _Methodid   = 'androidpublisher.edits.listings.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TListingsListResponse) as TListingsListResponse;
end;

Function TEditsListingsResource.Patch(editId: string; language: string; packageName: string; aListing : TListing) : TListing;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{packageName}/edits/{editId}/listings/{language}';
  _Methodid   = 'androidpublisher.edits.listings.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'language',language,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aListing,TListing) as TListing;
end;

Function TEditsListingsResource.Update(editId: string; language: string; packageName: string; aListing : TListing) : TListing;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{packageName}/edits/{editId}/listings/{language}';
  _Methodid   = 'androidpublisher.edits.listings.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'language',language,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aListing,TListing) as TListing;
end;



{ --------------------------------------------------------------------
  TEditsTestersResource
  --------------------------------------------------------------------}


Class Function TEditsTestersResource.ResourceName : String;

begin
  Result:='testers';
end;

Class Function TEditsTestersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidpublisherAPI;
end;

Function TEditsTestersResource.Get(editId: string; packageName: string; track: string) : TTesters;

Const
  _HTTPMethod = 'GET';
  _Path       = '{packageName}/edits/{editId}/testers/{track}';
  _Methodid   = 'androidpublisher.edits.testers.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'packageName',packageName,'track',track]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTesters) as TTesters;
end;

Function TEditsTestersResource.Patch(editId: string; packageName: string; track: string; aTesters : TTesters) : TTesters;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{packageName}/edits/{editId}/testers/{track}';
  _Methodid   = 'androidpublisher.edits.testers.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'packageName',packageName,'track',track]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTesters,TTesters) as TTesters;
end;

Function TEditsTestersResource.Update(editId: string; packageName: string; track: string; aTesters : TTesters) : TTesters;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{packageName}/edits/{editId}/testers/{track}';
  _Methodid   = 'androidpublisher.edits.testers.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'packageName',packageName,'track',track]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTesters,TTesters) as TTesters;
end;



{ --------------------------------------------------------------------
  TEditsTracksResource
  --------------------------------------------------------------------}


Class Function TEditsTracksResource.ResourceName : String;

begin
  Result:='tracks';
end;

Class Function TEditsTracksResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidpublisherAPI;
end;

Function TEditsTracksResource.Get(editId: string; packageName: string; track: string) : TTrack;

Const
  _HTTPMethod = 'GET';
  _Path       = '{packageName}/edits/{editId}/tracks/{track}';
  _Methodid   = 'androidpublisher.edits.tracks.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'packageName',packageName,'track',track]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTrack) as TTrack;
end;

Function TEditsTracksResource.List(editId: string; packageName: string) : TTracksListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{packageName}/edits/{editId}/tracks';
  _Methodid   = 'androidpublisher.edits.tracks.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'packageName',packageName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTracksListResponse) as TTracksListResponse;
end;

Function TEditsTracksResource.Patch(editId: string; packageName: string; track: string; aTrack : TTrack) : TTrack;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{packageName}/edits/{editId}/tracks/{track}';
  _Methodid   = 'androidpublisher.edits.tracks.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'packageName',packageName,'track',track]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTrack,TTrack) as TTrack;
end;

Function TEditsTracksResource.Update(editId: string; packageName: string; track: string; aTrack : TTrack) : TTrack;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{packageName}/edits/{editId}/tracks/{track}';
  _Methodid   = 'androidpublisher.edits.tracks.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['editId',editId,'packageName',packageName,'track',track]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTrack,TTrack) as TTrack;
end;



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



Function TEditsResource.GetApklistingsInstance : TEditsApklistingsResource;

begin
  if (FApklistingsInstance=Nil) then
    FApklistingsInstance:=CreateApklistingsResource;
  Result:=FApklistingsInstance;
end;

Function TEditsResource.CreateApklistingsResource : TEditsApklistingsResource;

begin
  Result:=CreateApklistingsResource(Self);
end;


Function TEditsResource.CreateApklistingsResource(AOwner : TComponent) : TEditsApklistingsResource;

begin
  Result:=TEditsApklistingsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TEditsResource.GetApksInstance : TEditsApksResource;

begin
  if (FApksInstance=Nil) then
    FApksInstance:=CreateApksResource;
  Result:=FApksInstance;
end;

Function TEditsResource.CreateApksResource : TEditsApksResource;

begin
  Result:=CreateApksResource(Self);
end;


Function TEditsResource.CreateApksResource(AOwner : TComponent) : TEditsApksResource;

begin
  Result:=TEditsApksResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TEditsResource.GetDetailsInstance : TEditsDetailsResource;

begin
  if (FDetailsInstance=Nil) then
    FDetailsInstance:=CreateDetailsResource;
  Result:=FDetailsInstance;
end;

Function TEditsResource.CreateDetailsResource : TEditsDetailsResource;

begin
  Result:=CreateDetailsResource(Self);
end;


Function TEditsResource.CreateDetailsResource(AOwner : TComponent) : TEditsDetailsResource;

begin
  Result:=TEditsDetailsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TEditsResource.GetExpansionfilesInstance : TEditsExpansionfilesResource;

begin
  if (FExpansionfilesInstance=Nil) then
    FExpansionfilesInstance:=CreateExpansionfilesResource;
  Result:=FExpansionfilesInstance;
end;

Function TEditsResource.CreateExpansionfilesResource : TEditsExpansionfilesResource;

begin
  Result:=CreateExpansionfilesResource(Self);
end;


Function TEditsResource.CreateExpansionfilesResource(AOwner : TComponent) : TEditsExpansionfilesResource;

begin
  Result:=TEditsExpansionfilesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TEditsResource.GetImagesInstance : TEditsImagesResource;

begin
  if (FImagesInstance=Nil) then
    FImagesInstance:=CreateImagesResource;
  Result:=FImagesInstance;
end;

Function TEditsResource.CreateImagesResource : TEditsImagesResource;

begin
  Result:=CreateImagesResource(Self);
end;


Function TEditsResource.CreateImagesResource(AOwner : TComponent) : TEditsImagesResource;

begin
  Result:=TEditsImagesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TEditsResource.GetListingsInstance : TEditsListingsResource;

begin
  if (FListingsInstance=Nil) then
    FListingsInstance:=CreateListingsResource;
  Result:=FListingsInstance;
end;

Function TEditsResource.CreateListingsResource : TEditsListingsResource;

begin
  Result:=CreateListingsResource(Self);
end;


Function TEditsResource.CreateListingsResource(AOwner : TComponent) : TEditsListingsResource;

begin
  Result:=TEditsListingsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TEditsResource.GetTestersInstance : TEditsTestersResource;

begin
  if (FTestersInstance=Nil) then
    FTestersInstance:=CreateTestersResource;
  Result:=FTestersInstance;
end;

Function TEditsResource.CreateTestersResource : TEditsTestersResource;

begin
  Result:=CreateTestersResource(Self);
end;


Function TEditsResource.CreateTestersResource(AOwner : TComponent) : TEditsTestersResource;

begin
  Result:=TEditsTestersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TEditsResource.GetTracksInstance : TEditsTracksResource;

begin
  if (FTracksInstance=Nil) then
    FTracksInstance:=CreateTracksResource;
  Result:=FTracksInstance;
end;

Function TEditsResource.CreateTracksResource : TEditsTracksResource;

begin
  Result:=CreateTracksResource(Self);
end;


Function TEditsResource.CreateTracksResource(AOwner : TComponent) : TEditsTracksResource;

begin
  Result:=TEditsTracksResource.Create(AOwner);
  Result.API:=Self.API;
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
  TPurchasesProductsResource
  --------------------------------------------------------------------}


Class Function TPurchasesProductsResource.ResourceName : String;

begin
  Result:='products';
end;

Class Function TPurchasesProductsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidpublisherAPI;
end;

Function TPurchasesProductsResource.Get(packageName: string; productId: string; token: string) : TProductPurchase;

Const
  _HTTPMethod = 'GET';
  _Path       = '{packageName}/purchases/products/{productId}/tokens/{token}';
  _Methodid   = 'androidpublisher.purchases.products.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['packageName',packageName,'productId',productId,'token',token]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TProductPurchase) as TProductPurchase;
end;



{ --------------------------------------------------------------------
  TPurchasesSubscriptionsResource
  --------------------------------------------------------------------}


Class Function TPurchasesSubscriptionsResource.ResourceName : String;

begin
  Result:='subscriptions';
end;

Class Function TPurchasesSubscriptionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidpublisherAPI;
end;

Procedure TPurchasesSubscriptionsResource.Cancel(packageName: string; subscriptionId: string; token: string);

Const
  _HTTPMethod = 'POST';
  _Path       = '{packageName}/purchases/subscriptions/{subscriptionId}/tokens/{token}:cancel';
  _Methodid   = 'androidpublisher.purchases.subscriptions.cancel';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['packageName',packageName,'subscriptionId',subscriptionId,'token',token]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TPurchasesSubscriptionsResource.Defer(packageName: string; subscriptionId: string; token: string; aSubscriptionPurchasesDeferRequest : TSubscriptionPurchasesDeferRequest) : TSubscriptionPurchasesDeferResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{packageName}/purchases/subscriptions/{subscriptionId}/tokens/{token}:defer';
  _Methodid   = 'androidpublisher.purchases.subscriptions.defer';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['packageName',packageName,'subscriptionId',subscriptionId,'token',token]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSubscriptionPurchasesDeferRequest,TSubscriptionPurchasesDeferResponse) as TSubscriptionPurchasesDeferResponse;
end;

Function TPurchasesSubscriptionsResource.Get(packageName: string; subscriptionId: string; token: string) : TSubscriptionPurchase;

Const
  _HTTPMethod = 'GET';
  _Path       = '{packageName}/purchases/subscriptions/{subscriptionId}/tokens/{token}';
  _Methodid   = 'androidpublisher.purchases.subscriptions.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['packageName',packageName,'subscriptionId',subscriptionId,'token',token]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TSubscriptionPurchase) as TSubscriptionPurchase;
end;

Procedure TPurchasesSubscriptionsResource.Refund(packageName: string; subscriptionId: string; token: string);

Const
  _HTTPMethod = 'POST';
  _Path       = '{packageName}/purchases/subscriptions/{subscriptionId}/tokens/{token}:refund';
  _Methodid   = 'androidpublisher.purchases.subscriptions.refund';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['packageName',packageName,'subscriptionId',subscriptionId,'token',token]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Procedure TPurchasesSubscriptionsResource.Revoke(packageName: string; subscriptionId: string; token: string);

Const
  _HTTPMethod = 'POST';
  _Path       = '{packageName}/purchases/subscriptions/{subscriptionId}/tokens/{token}:revoke';
  _Methodid   = 'androidpublisher.purchases.subscriptions.revoke';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['packageName',packageName,'subscriptionId',subscriptionId,'token',token]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
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



Function TPurchasesResource.GetProductsInstance : TPurchasesProductsResource;

begin
  if (FProductsInstance=Nil) then
    FProductsInstance:=CreateProductsResource;
  Result:=FProductsInstance;
end;

Function TPurchasesResource.CreateProductsResource : TPurchasesProductsResource;

begin
  Result:=CreateProductsResource(Self);
end;


Function TPurchasesResource.CreateProductsResource(AOwner : TComponent) : TPurchasesProductsResource;

begin
  Result:=TPurchasesProductsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TPurchasesResource.GetSubscriptionsInstance : TPurchasesSubscriptionsResource;

begin
  if (FSubscriptionsInstance=Nil) then
    FSubscriptionsInstance:=CreateSubscriptionsResource;
  Result:=FSubscriptionsInstance;
end;

Function TPurchasesResource.CreateSubscriptionsResource : TPurchasesSubscriptionsResource;

begin
  Result:=CreateSubscriptionsResource(Self);
end;


Function TPurchasesResource.CreateSubscriptionsResource(AOwner : TComponent) : TPurchasesSubscriptionsResource;

begin
  Result:=TPurchasesSubscriptionsResource.Create(AOwner);
  Result.API:=Self.API;
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
  Result:='https://www.googleapis.com:443/';
end;

Class Function TAndroidpublisherAPI.APIbasePath : string;

begin
  Result:='/androidpublisher/v2/applications/';
end;

Class Function TAndroidpublisherAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/androidpublisher/v2/applications/';
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
  TApksAddExternallyHostedRequest.RegisterObject;
  TApksAddExternallyHostedResponse.RegisterObject;
  TApksListResponse.RegisterObject;
  TAppDetails.RegisterObject;
  TAppEdit.RegisterObject;
  TEntitlement.RegisterObject;
  TEntitlementsListResponse.RegisterObject;
  TExpansionFile.RegisterObject;
  TExpansionFilesUploadResponse.RegisterObject;
  TExternallyHostedApk.RegisterObject;
  TExternallyHostedApkUsesPermission.RegisterObject;
  TImage.RegisterObject;
  TImagesDeleteAllResponse.RegisterObject;
  TImagesListResponse.RegisterObject;
  TImagesUploadResponse.RegisterObject;
  TInAppProductTypelistings.RegisterObject;
  TInAppProductTypeprices.RegisterObject;
  TInAppProduct.RegisterObject;
  TInAppProductListing.RegisterObject;
  TInappproductsBatchRequest.RegisterObject;
  TInappproductsBatchRequestEntry.RegisterObject;
  TInappproductsBatchResponse.RegisterObject;
  TInappproductsBatchResponseEntry.RegisterObject;
  TInappproductsInsertRequest.RegisterObject;
  TInappproductsInsertResponse.RegisterObject;
  TInappproductsListResponse.RegisterObject;
  TInappproductsUpdateRequest.RegisterObject;
  TInappproductsUpdateResponse.RegisterObject;
  TListing.RegisterObject;
  TListingsListResponse.RegisterObject;
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
  TTokenPagination.RegisterObject;
  TTrack.RegisterObject;
  TTracksListResponse.RegisterObject;
end;


Function TAndroidpublisherAPI.GetEditsApklistingsInstance : TEditsApklistingsResource;

begin
  if (FEditsApklistingsInstance=Nil) then
    FEditsApklistingsInstance:=CreateEditsApklistingsResource;
  Result:=FEditsApklistingsInstance;
end;

Function TAndroidpublisherAPI.CreateEditsApklistingsResource : TEditsApklistingsResource;

begin
  Result:=CreateEditsApklistingsResource(Self);
end;


Function TAndroidpublisherAPI.CreateEditsApklistingsResource(AOwner : TComponent) : TEditsApklistingsResource;

begin
  Result:=TEditsApklistingsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAndroidpublisherAPI.GetEditsApksInstance : TEditsApksResource;

begin
  if (FEditsApksInstance=Nil) then
    FEditsApksInstance:=CreateEditsApksResource;
  Result:=FEditsApksInstance;
end;

Function TAndroidpublisherAPI.CreateEditsApksResource : TEditsApksResource;

begin
  Result:=CreateEditsApksResource(Self);
end;


Function TAndroidpublisherAPI.CreateEditsApksResource(AOwner : TComponent) : TEditsApksResource;

begin
  Result:=TEditsApksResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAndroidpublisherAPI.GetEditsDetailsInstance : TEditsDetailsResource;

begin
  if (FEditsDetailsInstance=Nil) then
    FEditsDetailsInstance:=CreateEditsDetailsResource;
  Result:=FEditsDetailsInstance;
end;

Function TAndroidpublisherAPI.CreateEditsDetailsResource : TEditsDetailsResource;

begin
  Result:=CreateEditsDetailsResource(Self);
end;


Function TAndroidpublisherAPI.CreateEditsDetailsResource(AOwner : TComponent) : TEditsDetailsResource;

begin
  Result:=TEditsDetailsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAndroidpublisherAPI.GetEditsExpansionfilesInstance : TEditsExpansionfilesResource;

begin
  if (FEditsExpansionfilesInstance=Nil) then
    FEditsExpansionfilesInstance:=CreateEditsExpansionfilesResource;
  Result:=FEditsExpansionfilesInstance;
end;

Function TAndroidpublisherAPI.CreateEditsExpansionfilesResource : TEditsExpansionfilesResource;

begin
  Result:=CreateEditsExpansionfilesResource(Self);
end;


Function TAndroidpublisherAPI.CreateEditsExpansionfilesResource(AOwner : TComponent) : TEditsExpansionfilesResource;

begin
  Result:=TEditsExpansionfilesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAndroidpublisherAPI.GetEditsImagesInstance : TEditsImagesResource;

begin
  if (FEditsImagesInstance=Nil) then
    FEditsImagesInstance:=CreateEditsImagesResource;
  Result:=FEditsImagesInstance;
end;

Function TAndroidpublisherAPI.CreateEditsImagesResource : TEditsImagesResource;

begin
  Result:=CreateEditsImagesResource(Self);
end;


Function TAndroidpublisherAPI.CreateEditsImagesResource(AOwner : TComponent) : TEditsImagesResource;

begin
  Result:=TEditsImagesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAndroidpublisherAPI.GetEditsListingsInstance : TEditsListingsResource;

begin
  if (FEditsListingsInstance=Nil) then
    FEditsListingsInstance:=CreateEditsListingsResource;
  Result:=FEditsListingsInstance;
end;

Function TAndroidpublisherAPI.CreateEditsListingsResource : TEditsListingsResource;

begin
  Result:=CreateEditsListingsResource(Self);
end;


Function TAndroidpublisherAPI.CreateEditsListingsResource(AOwner : TComponent) : TEditsListingsResource;

begin
  Result:=TEditsListingsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAndroidpublisherAPI.GetEditsTestersInstance : TEditsTestersResource;

begin
  if (FEditsTestersInstance=Nil) then
    FEditsTestersInstance:=CreateEditsTestersResource;
  Result:=FEditsTestersInstance;
end;

Function TAndroidpublisherAPI.CreateEditsTestersResource : TEditsTestersResource;

begin
  Result:=CreateEditsTestersResource(Self);
end;


Function TAndroidpublisherAPI.CreateEditsTestersResource(AOwner : TComponent) : TEditsTestersResource;

begin
  Result:=TEditsTestersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAndroidpublisherAPI.GetEditsTracksInstance : TEditsTracksResource;

begin
  if (FEditsTracksInstance=Nil) then
    FEditsTracksInstance:=CreateEditsTracksResource;
  Result:=FEditsTracksInstance;
end;

Function TAndroidpublisherAPI.CreateEditsTracksResource : TEditsTracksResource;

begin
  Result:=CreateEditsTracksResource(Self);
end;


Function TAndroidpublisherAPI.CreateEditsTracksResource(AOwner : TComponent) : TEditsTracksResource;

begin
  Result:=TEditsTracksResource.Create(AOwner);
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
end;



Function TAndroidpublisherAPI.GetPurchasesProductsInstance : TPurchasesProductsResource;

begin
  if (FPurchasesProductsInstance=Nil) then
    FPurchasesProductsInstance:=CreatePurchasesProductsResource;
  Result:=FPurchasesProductsInstance;
end;

Function TAndroidpublisherAPI.CreatePurchasesProductsResource : TPurchasesProductsResource;

begin
  Result:=CreatePurchasesProductsResource(Self);
end;


Function TAndroidpublisherAPI.CreatePurchasesProductsResource(AOwner : TComponent) : TPurchasesProductsResource;

begin
  Result:=TPurchasesProductsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAndroidpublisherAPI.GetPurchasesSubscriptionsInstance : TPurchasesSubscriptionsResource;

begin
  if (FPurchasesSubscriptionsInstance=Nil) then
    FPurchasesSubscriptionsInstance:=CreatePurchasesSubscriptionsResource;
  Result:=FPurchasesSubscriptionsInstance;
end;

Function TAndroidpublisherAPI.CreatePurchasesSubscriptionsResource : TPurchasesSubscriptionsResource;

begin
  Result:=CreatePurchasesSubscriptionsResource(Self);
end;


Function TAndroidpublisherAPI.CreatePurchasesSubscriptionsResource(AOwner : TComponent) : TPurchasesSubscriptionsResource;

begin
  Result:=TPurchasesSubscriptionsResource.Create(AOwner);
  Result.API:=Self.API;
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
  Result.API:=Self.API;
end;



initialization
  TAndroidpublisherAPI.RegisterAPI;
end.
