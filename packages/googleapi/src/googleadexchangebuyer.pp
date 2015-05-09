unit googleadexchangebuyer;
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
//Generated on: 9-5-15 13:22:47
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAccount = class;
  TAccountsList = class;
  TBillingInfo = class;
  TBillingInfoList = class;
  TBudget = class;
  TCreative = class;
  TCreativesList = class;
  TDirectDeal = class;
  TDirectDealsList = class;
  TPerformanceReport = class;
  TPerformanceReportList = class;
  TPretargetingConfig = class;
  TPretargetingConfigList = class;
  TAccountArray = Array of TAccount;
  TAccountsListArray = Array of TAccountsList;
  TBillingInfoArray = Array of TBillingInfo;
  TBillingInfoListArray = Array of TBillingInfoList;
  TBudgetArray = Array of TBudget;
  TCreativeArray = Array of TCreative;
  TCreativesListArray = Array of TCreativesList;
  TDirectDealArray = Array of TDirectDeal;
  TDirectDealsListArray = Array of TDirectDealsList;
  TPerformanceReportArray = Array of TPerformanceReport;
  TPerformanceReportListArray = Array of TPerformanceReportList;
  TPretargetingConfigArray = Array of TPretargetingConfig;
  TPretargetingConfigListArray = Array of TPretargetingConfigList;
  //Anonymous types, using auto-generated names
  TAccountTypebidderLocationItem = class;
  TCreativeTypecorrectionsItem = class;
  TCreativeTypedisapprovalReasonsItem = class;
  TCreativeTypefilteringReasonsTypereasonsItem = class;
  TCreativeTypefilteringReasons = class;
  TPretargetingConfigTypedimensionsItem = class;
  TPretargetingConfigTypeexcludedPlacementsItem = class;
  TPretargetingConfigTypeplacementsItem = class;
  TAccountTypebidderLocationArray = Array of TAccountTypebidderLocationItem;
  TAccountsListTypeitemsArray = Array of TAccount;
  TBillingInfoListTypeitemsArray = Array of TBillingInfo;
  TCreativeTypecorrectionsArray = Array of TCreativeTypecorrectionsItem;
  TCreativeTypedisapprovalReasonsArray = Array of TCreativeTypedisapprovalReasonsItem;
  TCreativeTypefilteringReasonsTypereasonsArray = Array of TCreativeTypefilteringReasonsTypereasonsItem;
  TCreativesListTypeitemsArray = Array of TCreative;
  TDirectDealsListTypedirectDealsArray = Array of TDirectDeal;
  TPerformanceReportListTypeperformanceReportArray = Array of TPerformanceReport;
  TPretargetingConfigTypedimensionsArray = Array of TPretargetingConfigTypedimensionsItem;
  TPretargetingConfigTypeexcludedPlacementsArray = Array of TPretargetingConfigTypeexcludedPlacementsItem;
  TPretargetingConfigTypeplacementsArray = Array of TPretargetingConfigTypeplacementsItem;
  TPretargetingConfigListTypeitemsArray = Array of TPretargetingConfig;
  
  { --------------------------------------------------------------------
    TAccountTypebidderLocationItem
    --------------------------------------------------------------------}
  
  TAccountTypebidderLocationItem = Class(TGoogleBaseObject)
  Private
    FmaximumQps : integer;
    Fregion : String;
    Furl : String;
  Protected
    //Property setters
    Procedure SetmaximumQps(AIndex : Integer; AValue : integer); virtual;
    Procedure Setregion(AIndex : Integer; AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property maximumQps : integer Index 0 Read FmaximumQps Write SetmaximumQps;
    Property region : String Index 8 Read Fregion Write Setregion;
    Property url : String Index 16 Read Furl Write Seturl;
  end;
  TAccountTypebidderLocationItemClass = Class of TAccountTypebidderLocationItem;
  
  { --------------------------------------------------------------------
    TAccount
    --------------------------------------------------------------------}
  
  TAccount = Class(TGoogleBaseObject)
  Private
    FbidderLocation : TAccountTypebidderLocationArray;
    FcookieMatchingNid : String;
    FcookieMatchingUrl : String;
    Fid : integer;
    Fkind : String;
    FmaximumActiveCreatives : integer;
    FmaximumTotalQps : integer;
    FnumberActiveCreatives : integer;
  Protected
    //Property setters
    Procedure SetbidderLocation(AIndex : Integer; AValue : TAccountTypebidderLocationArray); virtual;
    Procedure SetcookieMatchingNid(AIndex : Integer; AValue : String); virtual;
    Procedure SetcookieMatchingUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmaximumActiveCreatives(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmaximumTotalQps(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnumberActiveCreatives(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property bidderLocation : TAccountTypebidderLocationArray Index 0 Read FbidderLocation Write SetbidderLocation;
    Property cookieMatchingNid : String Index 8 Read FcookieMatchingNid Write SetcookieMatchingNid;
    Property cookieMatchingUrl : String Index 16 Read FcookieMatchingUrl Write SetcookieMatchingUrl;
    Property id : integer Index 24 Read Fid Write Setid;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property maximumActiveCreatives : integer Index 40 Read FmaximumActiveCreatives Write SetmaximumActiveCreatives;
    Property maximumTotalQps : integer Index 48 Read FmaximumTotalQps Write SetmaximumTotalQps;
    Property numberActiveCreatives : integer Index 56 Read FnumberActiveCreatives Write SetnumberActiveCreatives;
  end;
  TAccountClass = Class of TAccount;
  
  { --------------------------------------------------------------------
    TAccountsList
    --------------------------------------------------------------------}
  
  TAccountsList = Class(TGoogleBaseObject)
  Private
    Fitems : TAccountsListTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TAccountsListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TAccountsListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TAccountsListClass = Class of TAccountsList;
  
  { --------------------------------------------------------------------
    TBillingInfo
    --------------------------------------------------------------------}
  
  TBillingInfo = Class(TGoogleBaseObject)
  Private
    FaccountId : integer;
    FaccountName : String;
    FbillingId : TStringArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetaccountName(AIndex : Integer; AValue : String); virtual;
    Procedure SetbillingId(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : integer Index 0 Read FaccountId Write SetaccountId;
    Property accountName : String Index 8 Read FaccountName Write SetaccountName;
    Property billingId : TStringArray Index 16 Read FbillingId Write SetbillingId;
    Property kind : String Index 24 Read Fkind Write Setkind;
  end;
  TBillingInfoClass = Class of TBillingInfo;
  
  { --------------------------------------------------------------------
    TBillingInfoList
    --------------------------------------------------------------------}
  
  TBillingInfoList = Class(TGoogleBaseObject)
  Private
    Fitems : TBillingInfoListTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TBillingInfoListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TBillingInfoListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TBillingInfoListClass = Class of TBillingInfoList;
  
  { --------------------------------------------------------------------
    TBudget
    --------------------------------------------------------------------}
  
  TBudget = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FbillingId : String;
    FbudgetAmount : String;
    FcurrencyCode : String;
    Fid : String;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetbillingId(AIndex : Integer; AValue : String); virtual;
    Procedure SetbudgetAmount(AIndex : Integer; AValue : String); virtual;
    Procedure SetcurrencyCode(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property billingId : String Index 8 Read FbillingId Write SetbillingId;
    Property budgetAmount : String Index 16 Read FbudgetAmount Write SetbudgetAmount;
    Property currencyCode : String Index 24 Read FcurrencyCode Write SetcurrencyCode;
    Property id : String Index 32 Read Fid Write Setid;
    Property kind : String Index 40 Read Fkind Write Setkind;
  end;
  TBudgetClass = Class of TBudget;
  
  { --------------------------------------------------------------------
    TCreativeTypecorrectionsItem
    --------------------------------------------------------------------}
  
  TCreativeTypecorrectionsItem = Class(TGoogleBaseObject)
  Private
    Fdetails : TStringArray;
    Freason : String;
  Protected
    //Property setters
    Procedure Setdetails(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setreason(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property details : TStringArray Index 0 Read Fdetails Write Setdetails;
    Property reason : String Index 8 Read Freason Write Setreason;
  end;
  TCreativeTypecorrectionsItemClass = Class of TCreativeTypecorrectionsItem;
  
  { --------------------------------------------------------------------
    TCreativeTypedisapprovalReasonsItem
    --------------------------------------------------------------------}
  
  TCreativeTypedisapprovalReasonsItem = Class(TGoogleBaseObject)
  Private
    Fdetails : TStringArray;
    Freason : String;
  Protected
    //Property setters
    Procedure Setdetails(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setreason(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property details : TStringArray Index 0 Read Fdetails Write Setdetails;
    Property reason : String Index 8 Read Freason Write Setreason;
  end;
  TCreativeTypedisapprovalReasonsItemClass = Class of TCreativeTypedisapprovalReasonsItem;
  
  { --------------------------------------------------------------------
    TCreativeTypefilteringReasonsTypereasonsItem
    --------------------------------------------------------------------}
  
  TCreativeTypefilteringReasonsTypereasonsItem = Class(TGoogleBaseObject)
  Private
    FfilteringCount : String;
    FfilteringStatus : integer;
  Protected
    //Property setters
    Procedure SetfilteringCount(AIndex : Integer; AValue : String); virtual;
    Procedure SetfilteringStatus(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property filteringCount : String Index 0 Read FfilteringCount Write SetfilteringCount;
    Property filteringStatus : integer Index 8 Read FfilteringStatus Write SetfilteringStatus;
  end;
  TCreativeTypefilteringReasonsTypereasonsItemClass = Class of TCreativeTypefilteringReasonsTypereasonsItem;
  
  { --------------------------------------------------------------------
    TCreativeTypefilteringReasons
    --------------------------------------------------------------------}
  
  TCreativeTypefilteringReasons = Class(TGoogleBaseObject)
  Private
    Fdate : String;
    Freasons : TCreativeTypefilteringReasonsTypereasonsArray;
  Protected
    //Property setters
    Procedure Setdate(AIndex : Integer; AValue : String); virtual;
    Procedure Setreasons(AIndex : Integer; AValue : TCreativeTypefilteringReasonsTypereasonsArray); virtual;
  Public
  Published
    Property date : String Index 0 Read Fdate Write Setdate;
    Property reasons : TCreativeTypefilteringReasonsTypereasonsArray Index 8 Read Freasons Write Setreasons;
  end;
  TCreativeTypefilteringReasonsClass = Class of TCreativeTypefilteringReasons;
  
  { --------------------------------------------------------------------
    TCreative
    --------------------------------------------------------------------}
  
  TCreative = Class(TGoogleBaseObject)
  Private
    FHTMLSnippet : String;
    FaccountId : integer;
    FadvertiserId : TStringArray;
    FadvertiserName : String;
    FagencyId : String;
    Fattribute : TintegerArray;
    FbuyerCreativeId : String;
    FclickThroughUrl : TStringArray;
    Fcorrections : TCreativeTypecorrectionsArray;
    FdisapprovalReasons : TCreativeTypedisapprovalReasonsArray;
    FfilteringReasons : TCreativeTypefilteringReasons;
    Fheight : integer;
    Fkind : String;
    FproductCategories : TintegerArray;
    FrestrictedCategories : TintegerArray;
    FsensitiveCategories : TintegerArray;
    Fstatus : String;
    FvendorType : TintegerArray;
    FvideoURL : String;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure SetHTMLSnippet(AIndex : Integer; AValue : String); virtual;
    Procedure SetaccountId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetadvertiserName(AIndex : Integer; AValue : String); virtual;
    Procedure SetagencyId(AIndex : Integer; AValue : String); virtual;
    Procedure Setattribute(AIndex : Integer; AValue : TintegerArray); virtual;
    Procedure SetbuyerCreativeId(AIndex : Integer; AValue : String); virtual;
    Procedure SetclickThroughUrl(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setcorrections(AIndex : Integer; AValue : TCreativeTypecorrectionsArray); virtual;
    Procedure SetdisapprovalReasons(AIndex : Integer; AValue : TCreativeTypedisapprovalReasonsArray); virtual;
    Procedure SetfilteringReasons(AIndex : Integer; AValue : TCreativeTypefilteringReasons); virtual;
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetproductCategories(AIndex : Integer; AValue : TintegerArray); virtual;
    Procedure SetrestrictedCategories(AIndex : Integer; AValue : TintegerArray); virtual;
    Procedure SetsensitiveCategories(AIndex : Integer; AValue : TintegerArray); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetvendorType(AIndex : Integer; AValue : TintegerArray); virtual;
    Procedure SetvideoURL(AIndex : Integer; AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property HTMLSnippet : String Index 0 Read FHTMLSnippet Write SetHTMLSnippet;
    Property accountId : integer Index 8 Read FaccountId Write SetaccountId;
    Property advertiserId : TStringArray Index 16 Read FadvertiserId Write SetadvertiserId;
    Property advertiserName : String Index 24 Read FadvertiserName Write SetadvertiserName;
    Property agencyId : String Index 32 Read FagencyId Write SetagencyId;
    Property attribute : TintegerArray Index 40 Read Fattribute Write Setattribute;
    Property buyerCreativeId : String Index 48 Read FbuyerCreativeId Write SetbuyerCreativeId;
    Property clickThroughUrl : TStringArray Index 56 Read FclickThroughUrl Write SetclickThroughUrl;
    Property corrections : TCreativeTypecorrectionsArray Index 64 Read Fcorrections Write Setcorrections;
    Property disapprovalReasons : TCreativeTypedisapprovalReasonsArray Index 72 Read FdisapprovalReasons Write SetdisapprovalReasons;
    Property filteringReasons : TCreativeTypefilteringReasons Index 80 Read FfilteringReasons Write SetfilteringReasons;
    Property height : integer Index 88 Read Fheight Write Setheight;
    Property kind : String Index 96 Read Fkind Write Setkind;
    Property productCategories : TintegerArray Index 104 Read FproductCategories Write SetproductCategories;
    Property restrictedCategories : TintegerArray Index 112 Read FrestrictedCategories Write SetrestrictedCategories;
    Property sensitiveCategories : TintegerArray Index 120 Read FsensitiveCategories Write SetsensitiveCategories;
    Property status : String Index 128 Read Fstatus Write Setstatus;
    Property vendorType : TintegerArray Index 136 Read FvendorType Write SetvendorType;
    Property videoURL : String Index 144 Read FvideoURL Write SetvideoURL;
    Property width : integer Index 152 Read Fwidth Write Setwidth;
  end;
  TCreativeClass = Class of TCreative;
  
  { --------------------------------------------------------------------
    TCreativesList
    --------------------------------------------------------------------}
  
  TCreativesList = Class(TGoogleBaseObject)
  Private
    Fitems : TCreativesListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TCreativesListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TCreativesListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TCreativesListClass = Class of TCreativesList;
  
  { --------------------------------------------------------------------
    TDirectDeal
    --------------------------------------------------------------------}
  
  TDirectDeal = Class(TGoogleBaseObject)
  Private
    FaccountId : integer;
    Fadvertiser : String;
    FcurrencyCode : String;
    FendTime : String;
    FfixedCpm : String;
    Fid : String;
    Fkind : String;
    Fname : String;
    FprivateExchangeMinCpm : String;
    FpublisherBlocksOverriden : boolean;
    FsellerNetwork : String;
    FstartTime : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : integer); virtual;
    Procedure Setadvertiser(AIndex : Integer; AValue : String); virtual;
    Procedure SetcurrencyCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetendTime(AIndex : Integer; AValue : String); virtual;
    Procedure SetfixedCpm(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetprivateExchangeMinCpm(AIndex : Integer; AValue : String); virtual;
    Procedure SetpublisherBlocksOverriden(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsellerNetwork(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : integer Index 0 Read FaccountId Write SetaccountId;
    Property advertiser : String Index 8 Read Fadvertiser Write Setadvertiser;
    Property currencyCode : String Index 16 Read FcurrencyCode Write SetcurrencyCode;
    Property endTime : String Index 24 Read FendTime Write SetendTime;
    Property fixedCpm : String Index 32 Read FfixedCpm Write SetfixedCpm;
    Property id : String Index 40 Read Fid Write Setid;
    Property kind : String Index 48 Read Fkind Write Setkind;
    Property name : String Index 56 Read Fname Write Setname;
    Property privateExchangeMinCpm : String Index 64 Read FprivateExchangeMinCpm Write SetprivateExchangeMinCpm;
    Property publisherBlocksOverriden : boolean Index 72 Read FpublisherBlocksOverriden Write SetpublisherBlocksOverriden;
    Property sellerNetwork : String Index 80 Read FsellerNetwork Write SetsellerNetwork;
    Property startTime : String Index 88 Read FstartTime Write SetstartTime;
  end;
  TDirectDealClass = Class of TDirectDeal;
  
  { --------------------------------------------------------------------
    TDirectDealsList
    --------------------------------------------------------------------}
  
  TDirectDealsList = Class(TGoogleBaseObject)
  Private
    FdirectDeals : TDirectDealsListTypedirectDealsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetdirectDeals(AIndex : Integer; AValue : TDirectDealsListTypedirectDealsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property directDeals : TDirectDealsListTypedirectDealsArray Index 0 Read FdirectDeals Write SetdirectDeals;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TDirectDealsListClass = Class of TDirectDealsList;
  
  { --------------------------------------------------------------------
    TPerformanceReport
    --------------------------------------------------------------------}
  
  TPerformanceReport = Class(TGoogleBaseObject)
  Private
    FcalloutStatusRate : TTJSONSchemaArray;
    FcookieMatcherStatusRate : TTJSONSchemaArray;
    FcreativeStatusRate : TTJSONSchemaArray;
    FhostedMatchStatusRate : TTJSONSchemaArray;
    Fkind : String;
    Flatency50thPercentile : double;
    Flatency85thPercentile : double;
    Flatency95thPercentile : double;
    FnoQuotaInRegion : double;
    FoutOfQuota : double;
    FpixelMatchRequests : double;
    FpixelMatchResponses : double;
    FquotaConfiguredLimit : double;
    FquotaThrottledLimit : double;
    Fregion : String;
    Ftimestamp : String;
  Protected
    //Property setters
    Procedure SetcalloutStatusRate(AIndex : Integer; AValue : TTJSONSchemaArray); virtual;
    Procedure SetcookieMatcherStatusRate(AIndex : Integer; AValue : TTJSONSchemaArray); virtual;
    Procedure SetcreativeStatusRate(AIndex : Integer; AValue : TTJSONSchemaArray); virtual;
    Procedure SethostedMatchStatusRate(AIndex : Integer; AValue : TTJSONSchemaArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlatency50thPercentile(AIndex : Integer; AValue : double); virtual;
    Procedure Setlatency85thPercentile(AIndex : Integer; AValue : double); virtual;
    Procedure Setlatency95thPercentile(AIndex : Integer; AValue : double); virtual;
    Procedure SetnoQuotaInRegion(AIndex : Integer; AValue : double); virtual;
    Procedure SetoutOfQuota(AIndex : Integer; AValue : double); virtual;
    Procedure SetpixelMatchRequests(AIndex : Integer; AValue : double); virtual;
    Procedure SetpixelMatchResponses(AIndex : Integer; AValue : double); virtual;
    Procedure SetquotaConfiguredLimit(AIndex : Integer; AValue : double); virtual;
    Procedure SetquotaThrottledLimit(AIndex : Integer; AValue : double); virtual;
    Procedure Setregion(AIndex : Integer; AValue : String); virtual;
    Procedure Settimestamp(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property calloutStatusRate : TTJSONSchemaArray Index 0 Read FcalloutStatusRate Write SetcalloutStatusRate;
    Property cookieMatcherStatusRate : TTJSONSchemaArray Index 8 Read FcookieMatcherStatusRate Write SetcookieMatcherStatusRate;
    Property creativeStatusRate : TTJSONSchemaArray Index 16 Read FcreativeStatusRate Write SetcreativeStatusRate;
    Property hostedMatchStatusRate : TTJSONSchemaArray Index 24 Read FhostedMatchStatusRate Write SethostedMatchStatusRate;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property latency50thPercentile : double Index 40 Read Flatency50thPercentile Write Setlatency50thPercentile;
    Property latency85thPercentile : double Index 48 Read Flatency85thPercentile Write Setlatency85thPercentile;
    Property latency95thPercentile : double Index 56 Read Flatency95thPercentile Write Setlatency95thPercentile;
    Property noQuotaInRegion : double Index 64 Read FnoQuotaInRegion Write SetnoQuotaInRegion;
    Property outOfQuota : double Index 72 Read FoutOfQuota Write SetoutOfQuota;
    Property pixelMatchRequests : double Index 80 Read FpixelMatchRequests Write SetpixelMatchRequests;
    Property pixelMatchResponses : double Index 88 Read FpixelMatchResponses Write SetpixelMatchResponses;
    Property quotaConfiguredLimit : double Index 96 Read FquotaConfiguredLimit Write SetquotaConfiguredLimit;
    Property quotaThrottledLimit : double Index 104 Read FquotaThrottledLimit Write SetquotaThrottledLimit;
    Property region : String Index 112 Read Fregion Write Setregion;
    Property timestamp : String Index 120 Read Ftimestamp Write Settimestamp;
  end;
  TPerformanceReportClass = Class of TPerformanceReport;
  
  { --------------------------------------------------------------------
    TPerformanceReportList
    --------------------------------------------------------------------}
  
  TPerformanceReportList = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FperformanceReport : TPerformanceReportListTypeperformanceReportArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetperformanceReport(AIndex : Integer; AValue : TPerformanceReportListTypeperformanceReportArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property performanceReport : TPerformanceReportListTypeperformanceReportArray Index 8 Read FperformanceReport Write SetperformanceReport;
  end;
  TPerformanceReportListClass = Class of TPerformanceReportList;
  
  { --------------------------------------------------------------------
    TPretargetingConfigTypedimensionsItem
    --------------------------------------------------------------------}
  
  TPretargetingConfigTypedimensionsItem = Class(TGoogleBaseObject)
  Private
    Fheight : String;
    Fwidth : String;
  Protected
    //Property setters
    Procedure Setheight(AIndex : Integer; AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property height : String Index 0 Read Fheight Write Setheight;
    Property width : String Index 8 Read Fwidth Write Setwidth;
  end;
  TPretargetingConfigTypedimensionsItemClass = Class of TPretargetingConfigTypedimensionsItem;
  
  { --------------------------------------------------------------------
    TPretargetingConfigTypeexcludedPlacementsItem
    --------------------------------------------------------------------}
  
  TPretargetingConfigTypeexcludedPlacementsItem = Class(TGoogleBaseObject)
  Private
    Ftoken : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Settoken(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property token : String Index 0 Read Ftoken Write Settoken;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TPretargetingConfigTypeexcludedPlacementsItemClass = Class of TPretargetingConfigTypeexcludedPlacementsItem;
  
  { --------------------------------------------------------------------
    TPretargetingConfigTypeplacementsItem
    --------------------------------------------------------------------}
  
  TPretargetingConfigTypeplacementsItem = Class(TGoogleBaseObject)
  Private
    Ftoken : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Settoken(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property token : String Index 0 Read Ftoken Write Settoken;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TPretargetingConfigTypeplacementsItemClass = Class of TPretargetingConfigTypeplacementsItem;
  
  { --------------------------------------------------------------------
    TPretargetingConfig
    --------------------------------------------------------------------}
  
  TPretargetingConfig = Class(TGoogleBaseObject)
  Private
    FbillingId : String;
    FconfigId : String;
    FconfigName : String;
    FcreativeType : TStringArray;
    Fdimensions : TPretargetingConfigTypedimensionsArray;
    FexcludedContentLabels : TStringArray;
    FexcludedGeoCriteriaIds : TStringArray;
    FexcludedPlacements : TPretargetingConfigTypeexcludedPlacementsArray;
    FexcludedUserLists : TStringArray;
    FexcludedVerticals : TStringArray;
    FgeoCriteriaIds : TStringArray;
    FisActive : boolean;
    Fkind : String;
    Flanguages : TStringArray;
    FmobileCarriers : TStringArray;
    FmobileDevices : TStringArray;
    FmobileOperatingSystemVersions : TStringArray;
    Fplacements : TPretargetingConfigTypeplacementsArray;
    Fplatforms : TStringArray;
    FsupportedCreativeAttributes : TStringArray;
    FuserLists : TStringArray;
    FvendorTypes : TStringArray;
    Fverticals : TStringArray;
  Protected
    //Property setters
    Procedure SetbillingId(AIndex : Integer; AValue : String); virtual;
    Procedure SetconfigId(AIndex : Integer; AValue : String); virtual;
    Procedure SetconfigName(AIndex : Integer; AValue : String); virtual;
    Procedure SetcreativeType(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setdimensions(AIndex : Integer; AValue : TPretargetingConfigTypedimensionsArray); virtual;
    Procedure SetexcludedContentLabels(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetexcludedGeoCriteriaIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetexcludedPlacements(AIndex : Integer; AValue : TPretargetingConfigTypeexcludedPlacementsArray); virtual;
    Procedure SetexcludedUserLists(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetexcludedVerticals(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetgeoCriteriaIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetisActive(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlanguages(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetmobileCarriers(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetmobileDevices(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetmobileOperatingSystemVersions(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setplacements(AIndex : Integer; AValue : TPretargetingConfigTypeplacementsArray); virtual;
    Procedure Setplatforms(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetsupportedCreativeAttributes(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetuserLists(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetvendorTypes(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setverticals(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property billingId : String Index 0 Read FbillingId Write SetbillingId;
    Property configId : String Index 8 Read FconfigId Write SetconfigId;
    Property configName : String Index 16 Read FconfigName Write SetconfigName;
    Property creativeType : TStringArray Index 24 Read FcreativeType Write SetcreativeType;
    Property dimensions : TPretargetingConfigTypedimensionsArray Index 32 Read Fdimensions Write Setdimensions;
    Property excludedContentLabels : TStringArray Index 40 Read FexcludedContentLabels Write SetexcludedContentLabels;
    Property excludedGeoCriteriaIds : TStringArray Index 48 Read FexcludedGeoCriteriaIds Write SetexcludedGeoCriteriaIds;
    Property excludedPlacements : TPretargetingConfigTypeexcludedPlacementsArray Index 56 Read FexcludedPlacements Write SetexcludedPlacements;
    Property excludedUserLists : TStringArray Index 64 Read FexcludedUserLists Write SetexcludedUserLists;
    Property excludedVerticals : TStringArray Index 72 Read FexcludedVerticals Write SetexcludedVerticals;
    Property geoCriteriaIds : TStringArray Index 80 Read FgeoCriteriaIds Write SetgeoCriteriaIds;
    Property isActive : boolean Index 88 Read FisActive Write SetisActive;
    Property kind : String Index 96 Read Fkind Write Setkind;
    Property languages : TStringArray Index 104 Read Flanguages Write Setlanguages;
    Property mobileCarriers : TStringArray Index 112 Read FmobileCarriers Write SetmobileCarriers;
    Property mobileDevices : TStringArray Index 120 Read FmobileDevices Write SetmobileDevices;
    Property mobileOperatingSystemVersions : TStringArray Index 128 Read FmobileOperatingSystemVersions Write SetmobileOperatingSystemVersions;
    Property placements : TPretargetingConfigTypeplacementsArray Index 136 Read Fplacements Write Setplacements;
    Property platforms : TStringArray Index 144 Read Fplatforms Write Setplatforms;
    Property supportedCreativeAttributes : TStringArray Index 152 Read FsupportedCreativeAttributes Write SetsupportedCreativeAttributes;
    Property userLists : TStringArray Index 160 Read FuserLists Write SetuserLists;
    Property vendorTypes : TStringArray Index 168 Read FvendorTypes Write SetvendorTypes;
    Property verticals : TStringArray Index 176 Read Fverticals Write Setverticals;
  end;
  TPretargetingConfigClass = Class of TPretargetingConfig;
  
  { --------------------------------------------------------------------
    TPretargetingConfigList
    --------------------------------------------------------------------}
  
  TPretargetingConfigList = Class(TGoogleBaseObject)
  Private
    Fitems : TPretargetingConfigListTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TPretargetingConfigListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TPretargetingConfigListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TPretargetingConfigListClass = Class of TPretargetingConfigList;
  
  { --------------------------------------------------------------------
    TAccountsResource
    --------------------------------------------------------------------}
  
  TAccountsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: integer) : TAccount;
    Function List : TAccountsList;
    Function Patch(id: integer; aAccount : TAccount) : TAccount;
    Function Update(id: integer; aAccount : TAccount) : TAccount;
  end;
  
  
  { --------------------------------------------------------------------
    TBillingInfoResource
    --------------------------------------------------------------------}
  
  TBillingInfoResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: integer) : TBillingInfo;
    Function List : TBillingInfoList;
  end;
  
  
  { --------------------------------------------------------------------
    TBudgetResource
    --------------------------------------------------------------------}
  
  TBudgetResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: string; billingId: string) : TBudget;
    Function Patch(accountId: string; billingId: string; aBudget : TBudget) : TBudget;
    Function Update(accountId: string; billingId: string; aBudget : TBudget) : TBudget;
  end;
  
  
  { --------------------------------------------------------------------
    TCreativesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCreativesResource, method List
  
  TCreativesListOptions = Record
    accountId : integer;
    buyerCreativeId : String;
    maxResults : integer;
    pageToken : String;
    statusFilter : String;
  end;
  
  TCreativesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: integer; buyerCreativeId: string) : TCreative;
    Function Insert(aCreative : TCreative) : TCreative;
    Function List(AQuery : string  = '') : TCreativesList;
    Function List(AQuery : TCreativeslistOptions) : TCreativesList;
  end;
  
  
  { --------------------------------------------------------------------
    TDirectDealsResource
    --------------------------------------------------------------------}
  
  TDirectDealsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string) : TDirectDeal;
    Function List : TDirectDealsList;
  end;
  
  
  { --------------------------------------------------------------------
    TPerformanceReportResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPerformanceReportResource, method List
  
  TPerformanceReportListOptions = Record
    accountId : int64;
    endDateTime : String;
    maxResults : integer;
    pageToken : String;
    startDateTime : String;
  end;
  
  TPerformanceReportResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TPerformanceReportList;
    Function List(AQuery : TPerformanceReportlistOptions) : TPerformanceReportList;
  end;
  
  
  { --------------------------------------------------------------------
    TPretargetingConfigResource
    --------------------------------------------------------------------}
  
  TPretargetingConfigResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(accountId: string; configId: string);
    Function Get(accountId: string; configId: string) : TPretargetingConfig;
    Function Insert(accountId: string; aPretargetingConfig : TPretargetingConfig) : TPretargetingConfig;
    Function List(accountId: string) : TPretargetingConfigList;
    Function Patch(accountId: string; configId: string; aPretargetingConfig : TPretargetingConfig) : TPretargetingConfig;
    Function Update(accountId: string; configId: string; aPretargetingConfig : TPretargetingConfig) : TPretargetingConfig;
  end;
  
  
  { --------------------------------------------------------------------
    TAdexchangebuyerAPI
    --------------------------------------------------------------------}
  
  TAdexchangebuyerAPI = Class(TGoogleAPI)
  Private
    FAccountsInstance : TAccountsResource;
    FBillingInfoInstance : TBillingInfoResource;
    FBudgetInstance : TBudgetResource;
    FCreativesInstance : TCreativesResource;
    FDirectDealsInstance : TDirectDealsResource;
    FPerformanceReportInstance : TPerformanceReportResource;
    FPretargetingConfigInstance : TPretargetingConfigResource;
    Function GetAccountsInstance : TAccountsResource;virtual;
    Function GetBillingInfoInstance : TBillingInfoResource;virtual;
    Function GetBudgetInstance : TBudgetResource;virtual;
    Function GetCreativesInstance : TCreativesResource;virtual;
    Function GetDirectDealsInstance : TDirectDealsResource;virtual;
    Function GetPerformanceReportInstance : TPerformanceReportResource;virtual;
    Function GetPretargetingConfigInstance : TPretargetingConfigResource;virtual;
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
    Function CreateAccountsResource(AOwner : TComponent) : TAccountsResource;virtual;overload;
    Function CreateAccountsResource : TAccountsResource;virtual;overload;
    Function CreateBillingInfoResource(AOwner : TComponent) : TBillingInfoResource;virtual;overload;
    Function CreateBillingInfoResource : TBillingInfoResource;virtual;overload;
    Function CreateBudgetResource(AOwner : TComponent) : TBudgetResource;virtual;overload;
    Function CreateBudgetResource : TBudgetResource;virtual;overload;
    Function CreateCreativesResource(AOwner : TComponent) : TCreativesResource;virtual;overload;
    Function CreateCreativesResource : TCreativesResource;virtual;overload;
    Function CreateDirectDealsResource(AOwner : TComponent) : TDirectDealsResource;virtual;overload;
    Function CreateDirectDealsResource : TDirectDealsResource;virtual;overload;
    Function CreatePerformanceReportResource(AOwner : TComponent) : TPerformanceReportResource;virtual;overload;
    Function CreatePerformanceReportResource : TPerformanceReportResource;virtual;overload;
    Function CreatePretargetingConfigResource(AOwner : TComponent) : TPretargetingConfigResource;virtual;overload;
    Function CreatePretargetingConfigResource : TPretargetingConfigResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AccountsResource : TAccountsResource Read GetAccountsInstance;
    Property BillingInfoResource : TBillingInfoResource Read GetBillingInfoInstance;
    Property BudgetResource : TBudgetResource Read GetBudgetInstance;
    Property CreativesResource : TCreativesResource Read GetCreativesInstance;
    Property DirectDealsResource : TDirectDealsResource Read GetDirectDealsInstance;
    Property PerformanceReportResource : TPerformanceReportResource Read GetPerformanceReportInstance;
    Property PretargetingConfigResource : TPretargetingConfigResource Read GetPretargetingConfigInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAccountTypebidderLocationItem
  --------------------------------------------------------------------}


Procedure TAccountTypebidderLocationItem.SetmaximumQps(AIndex : Integer; AValue : integer); 

begin
  If (FmaximumQps=AValue) then exit;
  FmaximumQps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTypebidderLocationItem.Setregion(AIndex : Integer; AValue : String); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTypebidderLocationItem.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccount
  --------------------------------------------------------------------}


Procedure TAccount.SetbidderLocation(AIndex : Integer; AValue : TAccountTypebidderLocationArray); 

begin
  If (FbidderLocation=AValue) then exit;
  FbidderLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetcookieMatchingNid(AIndex : Integer; AValue : String); 

begin
  If (FcookieMatchingNid=AValue) then exit;
  FcookieMatchingNid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetcookieMatchingUrl(AIndex : Integer; AValue : String); 

begin
  If (FcookieMatchingUrl=AValue) then exit;
  FcookieMatchingUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setid(AIndex : Integer; AValue : integer); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetmaximumActiveCreatives(AIndex : Integer; AValue : integer); 

begin
  If (FmaximumActiveCreatives=AValue) then exit;
  FmaximumActiveCreatives:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetmaximumTotalQps(AIndex : Integer; AValue : integer); 

begin
  If (FmaximumTotalQps=AValue) then exit;
  FmaximumTotalQps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetnumberActiveCreatives(AIndex : Integer; AValue : integer); 

begin
  If (FnumberActiveCreatives=AValue) then exit;
  FnumberActiveCreatives:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountsList
  --------------------------------------------------------------------}


Procedure TAccountsList.Setitems(AIndex : Integer; AValue : TAccountsListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBillingInfo
  --------------------------------------------------------------------}


Procedure TBillingInfo.SetaccountId(AIndex : Integer; AValue : integer); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBillingInfo.SetaccountName(AIndex : Integer; AValue : String); 

begin
  If (FaccountName=AValue) then exit;
  FaccountName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBillingInfo.SetbillingId(AIndex : Integer; AValue : TStringArray); 

begin
  If (FbillingId=AValue) then exit;
  FbillingId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBillingInfo.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBillingInfoList
  --------------------------------------------------------------------}


Procedure TBillingInfoList.Setitems(AIndex : Integer; AValue : TBillingInfoListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBillingInfoList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBudget
  --------------------------------------------------------------------}


Procedure TBudget.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBudget.SetbillingId(AIndex : Integer; AValue : String); 

begin
  If (FbillingId=AValue) then exit;
  FbillingId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBudget.SetbudgetAmount(AIndex : Integer; AValue : String); 

begin
  If (FbudgetAmount=AValue) then exit;
  FbudgetAmount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBudget.SetcurrencyCode(AIndex : Integer; AValue : String); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBudget.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBudget.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativeTypecorrectionsItem
  --------------------------------------------------------------------}


Procedure TCreativeTypecorrectionsItem.Setdetails(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypecorrectionsItem.Setreason(AIndex : Integer; AValue : String); 

begin
  If (Freason=AValue) then exit;
  Freason:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativeTypedisapprovalReasonsItem
  --------------------------------------------------------------------}


Procedure TCreativeTypedisapprovalReasonsItem.Setdetails(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypedisapprovalReasonsItem.Setreason(AIndex : Integer; AValue : String); 

begin
  If (Freason=AValue) then exit;
  Freason:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativeTypefilteringReasonsTypereasonsItem
  --------------------------------------------------------------------}


Procedure TCreativeTypefilteringReasonsTypereasonsItem.SetfilteringCount(AIndex : Integer; AValue : String); 

begin
  If (FfilteringCount=AValue) then exit;
  FfilteringCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypefilteringReasonsTypereasonsItem.SetfilteringStatus(AIndex : Integer; AValue : integer); 

begin
  If (FfilteringStatus=AValue) then exit;
  FfilteringStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativeTypefilteringReasons
  --------------------------------------------------------------------}


Procedure TCreativeTypefilteringReasons.Setdate(AIndex : Integer; AValue : String); 

begin
  If (Fdate=AValue) then exit;
  Fdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypefilteringReasons.Setreasons(AIndex : Integer; AValue : TCreativeTypefilteringReasonsTypereasonsArray); 

begin
  If (Freasons=AValue) then exit;
  Freasons:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreative
  --------------------------------------------------------------------}


Procedure TCreative.SetHTMLSnippet(AIndex : Integer; AValue : String); 

begin
  If (FHTMLSnippet=AValue) then exit;
  FHTMLSnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetaccountId(AIndex : Integer; AValue : integer); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetadvertiserId(AIndex : Integer; AValue : TStringArray); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetadvertiserName(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserName=AValue) then exit;
  FadvertiserName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetagencyId(AIndex : Integer; AValue : String); 

begin
  If (FagencyId=AValue) then exit;
  FagencyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setattribute(AIndex : Integer; AValue : TintegerArray); 

begin
  If (Fattribute=AValue) then exit;
  Fattribute:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetbuyerCreativeId(AIndex : Integer; AValue : String); 

begin
  If (FbuyerCreativeId=AValue) then exit;
  FbuyerCreativeId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetclickThroughUrl(AIndex : Integer; AValue : TStringArray); 

begin
  If (FclickThroughUrl=AValue) then exit;
  FclickThroughUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setcorrections(AIndex : Integer; AValue : TCreativeTypecorrectionsArray); 

begin
  If (Fcorrections=AValue) then exit;
  Fcorrections:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetdisapprovalReasons(AIndex : Integer; AValue : TCreativeTypedisapprovalReasonsArray); 

begin
  If (FdisapprovalReasons=AValue) then exit;
  FdisapprovalReasons:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetfilteringReasons(AIndex : Integer; AValue : TCreativeTypefilteringReasons); 

begin
  If (FfilteringReasons=AValue) then exit;
  FfilteringReasons:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetproductCategories(AIndex : Integer; AValue : TintegerArray); 

begin
  If (FproductCategories=AValue) then exit;
  FproductCategories:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetrestrictedCategories(AIndex : Integer; AValue : TintegerArray); 

begin
  If (FrestrictedCategories=AValue) then exit;
  FrestrictedCategories:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetsensitiveCategories(AIndex : Integer; AValue : TintegerArray); 

begin
  If (FsensitiveCategories=AValue) then exit;
  FsensitiveCategories:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetvendorType(AIndex : Integer; AValue : TintegerArray); 

begin
  If (FvendorType=AValue) then exit;
  FvendorType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetvideoURL(AIndex : Integer; AValue : String); 

begin
  If (FvideoURL=AValue) then exit;
  FvideoURL:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativesList
  --------------------------------------------------------------------}


Procedure TCreativesList.Setitems(AIndex : Integer; AValue : TCreativesListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativesList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativesList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDirectDeal
  --------------------------------------------------------------------}


Procedure TDirectDeal.SetaccountId(AIndex : Integer; AValue : integer); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectDeal.Setadvertiser(AIndex : Integer; AValue : String); 

begin
  If (Fadvertiser=AValue) then exit;
  Fadvertiser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectDeal.SetcurrencyCode(AIndex : Integer; AValue : String); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectDeal.SetendTime(AIndex : Integer; AValue : String); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectDeal.SetfixedCpm(AIndex : Integer; AValue : String); 

begin
  If (FfixedCpm=AValue) then exit;
  FfixedCpm:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectDeal.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectDeal.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectDeal.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectDeal.SetprivateExchangeMinCpm(AIndex : Integer; AValue : String); 

begin
  If (FprivateExchangeMinCpm=AValue) then exit;
  FprivateExchangeMinCpm:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectDeal.SetpublisherBlocksOverriden(AIndex : Integer; AValue : boolean); 

begin
  If (FpublisherBlocksOverriden=AValue) then exit;
  FpublisherBlocksOverriden:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectDeal.SetsellerNetwork(AIndex : Integer; AValue : String); 

begin
  If (FsellerNetwork=AValue) then exit;
  FsellerNetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectDeal.SetstartTime(AIndex : Integer; AValue : String); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDirectDealsList
  --------------------------------------------------------------------}


Procedure TDirectDealsList.SetdirectDeals(AIndex : Integer; AValue : TDirectDealsListTypedirectDealsArray); 

begin
  If (FdirectDeals=AValue) then exit;
  FdirectDeals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectDealsList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPerformanceReport
  --------------------------------------------------------------------}


Procedure TPerformanceReport.SetcalloutStatusRate(AIndex : Integer; AValue : TTJSONSchemaArray); 

begin
  If (FcalloutStatusRate=AValue) then exit;
  FcalloutStatusRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetcookieMatcherStatusRate(AIndex : Integer; AValue : TTJSONSchemaArray); 

begin
  If (FcookieMatcherStatusRate=AValue) then exit;
  FcookieMatcherStatusRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetcreativeStatusRate(AIndex : Integer; AValue : TTJSONSchemaArray); 

begin
  If (FcreativeStatusRate=AValue) then exit;
  FcreativeStatusRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SethostedMatchStatusRate(AIndex : Integer; AValue : TTJSONSchemaArray); 

begin
  If (FhostedMatchStatusRate=AValue) then exit;
  FhostedMatchStatusRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.Setlatency50thPercentile(AIndex : Integer; AValue : double); 

begin
  If (Flatency50thPercentile=AValue) then exit;
  Flatency50thPercentile:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.Setlatency85thPercentile(AIndex : Integer; AValue : double); 

begin
  If (Flatency85thPercentile=AValue) then exit;
  Flatency85thPercentile:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.Setlatency95thPercentile(AIndex : Integer; AValue : double); 

begin
  If (Flatency95thPercentile=AValue) then exit;
  Flatency95thPercentile:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetnoQuotaInRegion(AIndex : Integer; AValue : double); 

begin
  If (FnoQuotaInRegion=AValue) then exit;
  FnoQuotaInRegion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetoutOfQuota(AIndex : Integer; AValue : double); 

begin
  If (FoutOfQuota=AValue) then exit;
  FoutOfQuota:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetpixelMatchRequests(AIndex : Integer; AValue : double); 

begin
  If (FpixelMatchRequests=AValue) then exit;
  FpixelMatchRequests:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetpixelMatchResponses(AIndex : Integer; AValue : double); 

begin
  If (FpixelMatchResponses=AValue) then exit;
  FpixelMatchResponses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetquotaConfiguredLimit(AIndex : Integer; AValue : double); 

begin
  If (FquotaConfiguredLimit=AValue) then exit;
  FquotaConfiguredLimit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetquotaThrottledLimit(AIndex : Integer; AValue : double); 

begin
  If (FquotaThrottledLimit=AValue) then exit;
  FquotaThrottledLimit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.Setregion(AIndex : Integer; AValue : String); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.Settimestamp(AIndex : Integer; AValue : String); 

begin
  If (Ftimestamp=AValue) then exit;
  Ftimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPerformanceReportList
  --------------------------------------------------------------------}


Procedure TPerformanceReportList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReportList.SetperformanceReport(AIndex : Integer; AValue : TPerformanceReportListTypeperformanceReportArray); 

begin
  If (FperformanceReport=AValue) then exit;
  FperformanceReport:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPretargetingConfigTypedimensionsItem
  --------------------------------------------------------------------}


Procedure TPretargetingConfigTypedimensionsItem.Setheight(AIndex : Integer; AValue : String); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfigTypedimensionsItem.Setwidth(AIndex : Integer; AValue : String); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPretargetingConfigTypeexcludedPlacementsItem
  --------------------------------------------------------------------}


Procedure TPretargetingConfigTypeexcludedPlacementsItem.Settoken(AIndex : Integer; AValue : String); 

begin
  If (Ftoken=AValue) then exit;
  Ftoken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfigTypeexcludedPlacementsItem.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPretargetingConfigTypeexcludedPlacementsItem.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPretargetingConfigTypeplacementsItem
  --------------------------------------------------------------------}


Procedure TPretargetingConfigTypeplacementsItem.Settoken(AIndex : Integer; AValue : String); 

begin
  If (Ftoken=AValue) then exit;
  Ftoken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfigTypeplacementsItem.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPretargetingConfigTypeplacementsItem.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPretargetingConfig
  --------------------------------------------------------------------}


Procedure TPretargetingConfig.SetbillingId(AIndex : Integer; AValue : String); 

begin
  If (FbillingId=AValue) then exit;
  FbillingId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetconfigId(AIndex : Integer; AValue : String); 

begin
  If (FconfigId=AValue) then exit;
  FconfigId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetconfigName(AIndex : Integer; AValue : String); 

begin
  If (FconfigName=AValue) then exit;
  FconfigName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetcreativeType(AIndex : Integer; AValue : TStringArray); 

begin
  If (FcreativeType=AValue) then exit;
  FcreativeType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.Setdimensions(AIndex : Integer; AValue : TPretargetingConfigTypedimensionsArray); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetexcludedContentLabels(AIndex : Integer; AValue : TStringArray); 

begin
  If (FexcludedContentLabels=AValue) then exit;
  FexcludedContentLabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetexcludedGeoCriteriaIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FexcludedGeoCriteriaIds=AValue) then exit;
  FexcludedGeoCriteriaIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetexcludedPlacements(AIndex : Integer; AValue : TPretargetingConfigTypeexcludedPlacementsArray); 

begin
  If (FexcludedPlacements=AValue) then exit;
  FexcludedPlacements:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetexcludedUserLists(AIndex : Integer; AValue : TStringArray); 

begin
  If (FexcludedUserLists=AValue) then exit;
  FexcludedUserLists:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetexcludedVerticals(AIndex : Integer; AValue : TStringArray); 

begin
  If (FexcludedVerticals=AValue) then exit;
  FexcludedVerticals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetgeoCriteriaIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FgeoCriteriaIds=AValue) then exit;
  FgeoCriteriaIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetisActive(AIndex : Integer; AValue : boolean); 

begin
  If (FisActive=AValue) then exit;
  FisActive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.Setlanguages(AIndex : Integer; AValue : TStringArray); 

begin
  If (Flanguages=AValue) then exit;
  Flanguages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetmobileCarriers(AIndex : Integer; AValue : TStringArray); 

begin
  If (FmobileCarriers=AValue) then exit;
  FmobileCarriers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetmobileDevices(AIndex : Integer; AValue : TStringArray); 

begin
  If (FmobileDevices=AValue) then exit;
  FmobileDevices:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetmobileOperatingSystemVersions(AIndex : Integer; AValue : TStringArray); 

begin
  If (FmobileOperatingSystemVersions=AValue) then exit;
  FmobileOperatingSystemVersions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.Setplacements(AIndex : Integer; AValue : TPretargetingConfigTypeplacementsArray); 

begin
  If (Fplacements=AValue) then exit;
  Fplacements:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.Setplatforms(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fplatforms=AValue) then exit;
  Fplatforms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetsupportedCreativeAttributes(AIndex : Integer; AValue : TStringArray); 

begin
  If (FsupportedCreativeAttributes=AValue) then exit;
  FsupportedCreativeAttributes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetuserLists(AIndex : Integer; AValue : TStringArray); 

begin
  If (FuserLists=AValue) then exit;
  FuserLists:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetvendorTypes(AIndex : Integer; AValue : TStringArray); 

begin
  If (FvendorTypes=AValue) then exit;
  FvendorTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.Setverticals(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fverticals=AValue) then exit;
  Fverticals:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPretargetingConfigList
  --------------------------------------------------------------------}


Procedure TPretargetingConfigList.Setitems(AIndex : Integer; AValue : TPretargetingConfigListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfigList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountsResource
  --------------------------------------------------------------------}


Class Function TAccountsResource.ResourceName : String;

begin
  Result:='accounts';
end;

Class Function TAccountsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangebuyerAPI;
end;

Function TAccountsResource.Get(id: integer) : TAccount;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{id}';
  _Methodid   = 'adexchangebuyer.accounts.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAccount) as TAccount;
end;

Function TAccountsResource.List : TAccountsList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts';
  _Methodid   = 'adexchangebuyer.accounts.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TAccountsList) as TAccountsList;
end;

Function TAccountsResource.Patch(id: integer; aAccount : TAccount) : TAccount;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'accounts/{id}';
  _Methodid   = 'adexchangebuyer.accounts.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAccount,TAccount) as TAccount;
end;

Function TAccountsResource.Update(id: integer; aAccount : TAccount) : TAccount;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'accounts/{id}';
  _Methodid   = 'adexchangebuyer.accounts.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAccount,TAccount) as TAccount;
end;



{ --------------------------------------------------------------------
  TBillingInfoResource
  --------------------------------------------------------------------}


Class Function TBillingInfoResource.ResourceName : String;

begin
  Result:='billingInfo';
end;

Class Function TBillingInfoResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangebuyerAPI;
end;

Function TBillingInfoResource.Get(accountId: integer) : TBillingInfo;

Const
  _HTTPMethod = 'GET';
  _Path       = 'billinginfo/{accountId}';
  _Methodid   = 'adexchangebuyer.billingInfo.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TBillingInfo) as TBillingInfo;
end;

Function TBillingInfoResource.List : TBillingInfoList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'billinginfo';
  _Methodid   = 'adexchangebuyer.billingInfo.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TBillingInfoList) as TBillingInfoList;
end;



{ --------------------------------------------------------------------
  TBudgetResource
  --------------------------------------------------------------------}


Class Function TBudgetResource.ResourceName : String;

begin
  Result:='budget';
end;

Class Function TBudgetResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangebuyerAPI;
end;

Function TBudgetResource.Get(accountId: string; billingId: string) : TBudget;

Const
  _HTTPMethod = 'GET';
  _Path       = 'billinginfo/{accountId}/{billingId}';
  _Methodid   = 'adexchangebuyer.budget.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'billingId',billingId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TBudget) as TBudget;
end;

Function TBudgetResource.Patch(accountId: string; billingId: string; aBudget : TBudget) : TBudget;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'billinginfo/{accountId}/{billingId}';
  _Methodid   = 'adexchangebuyer.budget.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'billingId',billingId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aBudget,TBudget) as TBudget;
end;

Function TBudgetResource.Update(accountId: string; billingId: string; aBudget : TBudget) : TBudget;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'billinginfo/{accountId}/{billingId}';
  _Methodid   = 'adexchangebuyer.budget.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'billingId',billingId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aBudget,TBudget) as TBudget;
end;



{ --------------------------------------------------------------------
  TCreativesResource
  --------------------------------------------------------------------}


Class Function TCreativesResource.ResourceName : String;

begin
  Result:='creatives';
end;

Class Function TCreativesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangebuyerAPI;
end;

Function TCreativesResource.Get(accountId: integer; buyerCreativeId: string) : TCreative;

Const
  _HTTPMethod = 'GET';
  _Path       = 'creatives/{accountId}/{buyerCreativeId}';
  _Methodid   = 'adexchangebuyer.creatives.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'buyerCreativeId',buyerCreativeId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCreative) as TCreative;
end;

Function TCreativesResource.Insert(aCreative : TCreative) : TCreative;

Const
  _HTTPMethod = 'POST';
  _Path       = 'creatives';
  _Methodid   = 'adexchangebuyer.creatives.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aCreative,TCreative) as TCreative;
end;

Function TCreativesResource.List(AQuery : string = '') : TCreativesList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'creatives';
  _Methodid   = 'adexchangebuyer.creatives.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TCreativesList) as TCreativesList;
end;


Function TCreativesResource.List(AQuery : TCreativeslistOptions) : TCreativesList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'accountId',AQuery.accountId);
  AddToQuery(_Q,'buyerCreativeId',AQuery.buyerCreativeId);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'statusFilter',AQuery.statusFilter);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TDirectDealsResource
  --------------------------------------------------------------------}


Class Function TDirectDealsResource.ResourceName : String;

begin
  Result:='directDeals';
end;

Class Function TDirectDealsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangebuyerAPI;
end;

Function TDirectDealsResource.Get(id: string) : TDirectDeal;

Const
  _HTTPMethod = 'GET';
  _Path       = 'directdeals/{id}';
  _Methodid   = 'adexchangebuyer.directDeals.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDirectDeal) as TDirectDeal;
end;

Function TDirectDealsResource.List : TDirectDealsList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'directdeals';
  _Methodid   = 'adexchangebuyer.directDeals.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TDirectDealsList) as TDirectDealsList;
end;



{ --------------------------------------------------------------------
  TPerformanceReportResource
  --------------------------------------------------------------------}


Class Function TPerformanceReportResource.ResourceName : String;

begin
  Result:='performanceReport';
end;

Class Function TPerformanceReportResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangebuyerAPI;
end;

Function TPerformanceReportResource.List(AQuery : string = '') : TPerformanceReportList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'performancereport';
  _Methodid   = 'adexchangebuyer.performanceReport.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TPerformanceReportList) as TPerformanceReportList;
end;


Function TPerformanceReportResource.List(AQuery : TPerformanceReportlistOptions) : TPerformanceReportList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'accountId',AQuery.accountId);
  AddToQuery(_Q,'endDateTime',AQuery.endDateTime);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'startDateTime',AQuery.startDateTime);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TPretargetingConfigResource
  --------------------------------------------------------------------}


Class Function TPretargetingConfigResource.ResourceName : String;

begin
  Result:='pretargetingConfig';
end;

Class Function TPretargetingConfigResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangebuyerAPI;
end;

Procedure TPretargetingConfigResource.Delete(accountId: string; configId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'pretargetingconfigs/{accountId}/{configId}';
  _Methodid   = 'adexchangebuyer.pretargetingConfig.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'configId',configId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TPretargetingConfigResource.Get(accountId: string; configId: string) : TPretargetingConfig;

Const
  _HTTPMethod = 'GET';
  _Path       = 'pretargetingconfigs/{accountId}/{configId}';
  _Methodid   = 'adexchangebuyer.pretargetingConfig.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'configId',configId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPretargetingConfig) as TPretargetingConfig;
end;

Function TPretargetingConfigResource.Insert(accountId: string; aPretargetingConfig : TPretargetingConfig) : TPretargetingConfig;

Const
  _HTTPMethod = 'POST';
  _Path       = 'pretargetingconfigs/{accountId}';
  _Methodid   = 'adexchangebuyer.pretargetingConfig.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aPretargetingConfig,TPretargetingConfig) as TPretargetingConfig;
end;

Function TPretargetingConfigResource.List(accountId: string) : TPretargetingConfigList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'pretargetingconfigs/{accountId}';
  _Methodid   = 'adexchangebuyer.pretargetingConfig.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPretargetingConfigList) as TPretargetingConfigList;
end;

Function TPretargetingConfigResource.Patch(accountId: string; configId: string; aPretargetingConfig : TPretargetingConfig) : TPretargetingConfig;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'pretargetingconfigs/{accountId}/{configId}';
  _Methodid   = 'adexchangebuyer.pretargetingConfig.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'configId',configId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aPretargetingConfig,TPretargetingConfig) as TPretargetingConfig;
end;

Function TPretargetingConfigResource.Update(accountId: string; configId: string; aPretargetingConfig : TPretargetingConfig) : TPretargetingConfig;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'pretargetingconfigs/{accountId}/{configId}';
  _Methodid   = 'adexchangebuyer.pretargetingConfig.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'configId',configId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aPretargetingConfig,TPretargetingConfig) as TPretargetingConfig;
end;



{ --------------------------------------------------------------------
  TAdexchangebuyerAPI
  --------------------------------------------------------------------}

Class Function TAdexchangebuyerAPI.APIName : String;

begin
  Result:='adexchangebuyer';
end;

Class Function TAdexchangebuyerAPI.APIVersion : String;

begin
  Result:='v1.3';
end;

Class Function TAdexchangebuyerAPI.APIRevision : String;

begin
  Result:='20150326';
end;

Class Function TAdexchangebuyerAPI.APIID : String;

begin
  Result:='adexchangebuyer:v1.3';
end;

Class Function TAdexchangebuyerAPI.APITitle : String;

begin
  Result:='Ad Exchange Buyer API';
end;

Class Function TAdexchangebuyerAPI.APIDescription : String;

begin
  Result:='Accesses your bidding-account information, submits creatives for validation, finds available direct deals, and retrieves performance reports.';
end;

Class Function TAdexchangebuyerAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TAdexchangebuyerAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TAdexchangebuyerAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/doubleclick-16.gif';
end;

Class Function TAdexchangebuyerAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/doubleclick-32.gif';
end;

Class Function TAdexchangebuyerAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/ad-exchange/buyer-rest';
end;

Class Function TAdexchangebuyerAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TAdexchangebuyerAPI.APIbasePath : string;

begin
  Result:='/adexchangebuyer/v1.3/';
end;

Class Function TAdexchangebuyerAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/adexchangebuyer/v1.3/';
end;

Class Function TAdexchangebuyerAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TAdexchangebuyerAPI.APIservicePath : string;

begin
  Result:='adexchangebuyer/v1.3/';
end;

Class Function TAdexchangebuyerAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TAdexchangebuyerAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/adexchange.buyer';
  Result[0].Description:='Manage your Ad Exchange buyer account configuration';
  
end;

Class Function TAdexchangebuyerAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TAdexchangebuyerAPI.RegisterAPIResources;

begin
  TAccountTypebidderLocationItem.RegisterObject;
  TAccount.RegisterObject;
  TAccountsList.RegisterObject;
  TBillingInfo.RegisterObject;
  TBillingInfoList.RegisterObject;
  TBudget.RegisterObject;
  TCreativeTypecorrectionsItem.RegisterObject;
  TCreativeTypedisapprovalReasonsItem.RegisterObject;
  TCreativeTypefilteringReasonsTypereasonsItem.RegisterObject;
  TCreativeTypefilteringReasons.RegisterObject;
  TCreative.RegisterObject;
  TCreativesList.RegisterObject;
  TDirectDeal.RegisterObject;
  TDirectDealsList.RegisterObject;
  TPerformanceReport.RegisterObject;
  TPerformanceReportList.RegisterObject;
  TPretargetingConfigTypedimensionsItem.RegisterObject;
  TPretargetingConfigTypeexcludedPlacementsItem.RegisterObject;
  TPretargetingConfigTypeplacementsItem.RegisterObject;
  TPretargetingConfig.RegisterObject;
  TPretargetingConfigList.RegisterObject;
end;


Function TAdexchangebuyerAPI.GetAccountsInstance : TAccountsResource;

begin
  if (FAccountsInstance=Nil) then
    FAccountsInstance:=CreateAccountsResource;
  Result:=FAccountsInstance;
end;

Function TAdexchangebuyerAPI.CreateAccountsResource : TAccountsResource;

begin
  Result:=CreateAccountsResource(Self);
end;


Function TAdexchangebuyerAPI.CreateAccountsResource(AOwner : TComponent) : TAccountsResource;

begin
  Result:=TAccountsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAdexchangebuyerAPI.GetBillingInfoInstance : TBillingInfoResource;

begin
  if (FBillingInfoInstance=Nil) then
    FBillingInfoInstance:=CreateBillingInfoResource;
  Result:=FBillingInfoInstance;
end;

Function TAdexchangebuyerAPI.CreateBillingInfoResource : TBillingInfoResource;

begin
  Result:=CreateBillingInfoResource(Self);
end;


Function TAdexchangebuyerAPI.CreateBillingInfoResource(AOwner : TComponent) : TBillingInfoResource;

begin
  Result:=TBillingInfoResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAdexchangebuyerAPI.GetBudgetInstance : TBudgetResource;

begin
  if (FBudgetInstance=Nil) then
    FBudgetInstance:=CreateBudgetResource;
  Result:=FBudgetInstance;
end;

Function TAdexchangebuyerAPI.CreateBudgetResource : TBudgetResource;

begin
  Result:=CreateBudgetResource(Self);
end;


Function TAdexchangebuyerAPI.CreateBudgetResource(AOwner : TComponent) : TBudgetResource;

begin
  Result:=TBudgetResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAdexchangebuyerAPI.GetCreativesInstance : TCreativesResource;

begin
  if (FCreativesInstance=Nil) then
    FCreativesInstance:=CreateCreativesResource;
  Result:=FCreativesInstance;
end;

Function TAdexchangebuyerAPI.CreateCreativesResource : TCreativesResource;

begin
  Result:=CreateCreativesResource(Self);
end;


Function TAdexchangebuyerAPI.CreateCreativesResource(AOwner : TComponent) : TCreativesResource;

begin
  Result:=TCreativesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAdexchangebuyerAPI.GetDirectDealsInstance : TDirectDealsResource;

begin
  if (FDirectDealsInstance=Nil) then
    FDirectDealsInstance:=CreateDirectDealsResource;
  Result:=FDirectDealsInstance;
end;

Function TAdexchangebuyerAPI.CreateDirectDealsResource : TDirectDealsResource;

begin
  Result:=CreateDirectDealsResource(Self);
end;


Function TAdexchangebuyerAPI.CreateDirectDealsResource(AOwner : TComponent) : TDirectDealsResource;

begin
  Result:=TDirectDealsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAdexchangebuyerAPI.GetPerformanceReportInstance : TPerformanceReportResource;

begin
  if (FPerformanceReportInstance=Nil) then
    FPerformanceReportInstance:=CreatePerformanceReportResource;
  Result:=FPerformanceReportInstance;
end;

Function TAdexchangebuyerAPI.CreatePerformanceReportResource : TPerformanceReportResource;

begin
  Result:=CreatePerformanceReportResource(Self);
end;


Function TAdexchangebuyerAPI.CreatePerformanceReportResource(AOwner : TComponent) : TPerformanceReportResource;

begin
  Result:=TPerformanceReportResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAdexchangebuyerAPI.GetPretargetingConfigInstance : TPretargetingConfigResource;

begin
  if (FPretargetingConfigInstance=Nil) then
    FPretargetingConfigInstance:=CreatePretargetingConfigResource;
  Result:=FPretargetingConfigInstance;
end;

Function TAdexchangebuyerAPI.CreatePretargetingConfigResource : TPretargetingConfigResource;

begin
  Result:=CreatePretargetingConfigResource(Self);
end;


Function TAdexchangebuyerAPI.CreatePretargetingConfigResource(AOwner : TComponent) : TPretargetingConfigResource;

begin
  Result:=TPretargetingConfigResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TAdexchangebuyerAPI.RegisterAPI;
end.
