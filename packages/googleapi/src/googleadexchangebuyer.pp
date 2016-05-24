unit googleadexchangebuyer;
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
  TAccount = class;
  TAccountArray = Array of TAccount;
  TAccountbidderLocation = class;
  TAccountbidderLocationArray = Array of TAccountbidderLocation;
  TAccountsList = class;
  TAccountsListArray = Array of TAccountsList;
  TAccountsListitems = class;
  TAccountsListitemsArray = Array of TAccountsListitems;
  TBillingInfo = class;
  TBillingInfoArray = Array of TBillingInfo;
  TBillingInfobillingId = class;
  TBillingInfobillingIdArray = Array of TBillingInfobillingId;
  TBillingInfoList = class;
  TBillingInfoListArray = Array of TBillingInfoList;
  TBillingInfoListitems = class;
  TBillingInfoListitemsArray = Array of TBillingInfoListitems;
  TBudget = class;
  TBudgetArray = Array of TBudget;
  TCreative = class;
  TCreativeArray = Array of TCreative;
  TCreativeadvertiserId = class;
  TCreativeadvertiserIdArray = Array of TCreativeadvertiserId;
  TCreativeattribute = class;
  TCreativeattributeArray = Array of TCreativeattribute;
  TCreativeclickThroughUrl = class;
  TCreativeclickThroughUrlArray = Array of TCreativeclickThroughUrl;
  TCreativecorrections = class;
  TCreativecorrectionsArray = Array of TCreativecorrections;
  TCreativecorrectionsdetails = class;
  TCreativecorrectionsdetailsArray = Array of TCreativecorrectionsdetails;
  TCreativedisapprovalReasons = class;
  TCreativedisapprovalReasonsArray = Array of TCreativedisapprovalReasons;
  TCreativedisapprovalReasonsdetails = class;
  TCreativedisapprovalReasonsdetailsArray = Array of TCreativedisapprovalReasonsdetails;
  TCreativefilteringReasons = class;
  TCreativefilteringReasonsArray = Array of TCreativefilteringReasons;
  TCreativefilteringReasonsreasons = class;
  TCreativefilteringReasonsreasonsArray = Array of TCreativefilteringReasonsreasons;
  TCreativeproductCategories = class;
  TCreativeproductCategoriesArray = Array of TCreativeproductCategories;
  TCreativerestrictedCategories = class;
  TCreativerestrictedCategoriesArray = Array of TCreativerestrictedCategories;
  TCreativesensitiveCategories = class;
  TCreativesensitiveCategoriesArray = Array of TCreativesensitiveCategories;
  TCreativevendorType = class;
  TCreativevendorTypeArray = Array of TCreativevendorType;
  TCreativesList = class;
  TCreativesListArray = Array of TCreativesList;
  TCreativesListitems = class;
  TCreativesListitemsArray = Array of TCreativesListitems;
  TDirectDeal = class;
  TDirectDealArray = Array of TDirectDeal;
  TDirectDealsList = class;
  TDirectDealsListArray = Array of TDirectDealsList;
  TDirectDealsListdirectDeals = class;
  TDirectDealsListdirectDealsArray = Array of TDirectDealsListdirectDeals;
  TPerformanceReport = class;
  TPerformanceReportArray = Array of TPerformanceReport;
  TPerformanceReportcalloutStatusRate = class;
  TPerformanceReportcalloutStatusRateArray = Array of TPerformanceReportcalloutStatusRate;
  TPerformanceReportcookieMatcherStatusRate = class;
  TPerformanceReportcookieMatcherStatusRateArray = Array of TPerformanceReportcookieMatcherStatusRate;
  TPerformanceReportcreativeStatusRate = class;
  TPerformanceReportcreativeStatusRateArray = Array of TPerformanceReportcreativeStatusRate;
  TPerformanceReporthostedMatchStatusRate = class;
  TPerformanceReporthostedMatchStatusRateArray = Array of TPerformanceReporthostedMatchStatusRate;
  TPerformanceReportList = class;
  TPerformanceReportListArray = Array of TPerformanceReportList;
  TPerformanceReportListperformanceReport = class;
  TPerformanceReportListperformanceReportArray = Array of TPerformanceReportListperformanceReport;
  TPretargetingConfig = class;
  TPretargetingConfigArray = Array of TPretargetingConfig;
  TPretargetingConfigcreativeType = class;
  TPretargetingConfigcreativeTypeArray = Array of TPretargetingConfigcreativeType;
  TPretargetingConfigdimensions = class;
  TPretargetingConfigdimensionsArray = Array of TPretargetingConfigdimensions;
  TPretargetingConfigexcludedContentLabels = class;
  TPretargetingConfigexcludedContentLabelsArray = Array of TPretargetingConfigexcludedContentLabels;
  TPretargetingConfigexcludedGeoCriteriaIds = class;
  TPretargetingConfigexcludedGeoCriteriaIdsArray = Array of TPretargetingConfigexcludedGeoCriteriaIds;
  TPretargetingConfigexcludedPlacements = class;
  TPretargetingConfigexcludedPlacementsArray = Array of TPretargetingConfigexcludedPlacements;
  TPretargetingConfigexcludedUserLists = class;
  TPretargetingConfigexcludedUserListsArray = Array of TPretargetingConfigexcludedUserLists;
  TPretargetingConfigexcludedVerticals = class;
  TPretargetingConfigexcludedVerticalsArray = Array of TPretargetingConfigexcludedVerticals;
  TPretargetingConfiggeoCriteriaIds = class;
  TPretargetingConfiggeoCriteriaIdsArray = Array of TPretargetingConfiggeoCriteriaIds;
  TPretargetingConfiglanguages = class;
  TPretargetingConfiglanguagesArray = Array of TPretargetingConfiglanguages;
  TPretargetingConfigmobileCarriers = class;
  TPretargetingConfigmobileCarriersArray = Array of TPretargetingConfigmobileCarriers;
  TPretargetingConfigmobileDevices = class;
  TPretargetingConfigmobileDevicesArray = Array of TPretargetingConfigmobileDevices;
  TPretargetingConfigmobileOperatingSystemVersions = class;
  TPretargetingConfigmobileOperatingSystemVersionsArray = Array of TPretargetingConfigmobileOperatingSystemVersions;
  TPretargetingConfigplacements = class;
  TPretargetingConfigplacementsArray = Array of TPretargetingConfigplacements;
  TPretargetingConfigplatforms = class;
  TPretargetingConfigplatformsArray = Array of TPretargetingConfigplatforms;
  TPretargetingConfigsupportedCreativeAttributes = class;
  TPretargetingConfigsupportedCreativeAttributesArray = Array of TPretargetingConfigsupportedCreativeAttributes;
  TPretargetingConfiguserLists = class;
  TPretargetingConfiguserListsArray = Array of TPretargetingConfiguserLists;
  TPretargetingConfigvendorTypes = class;
  TPretargetingConfigvendorTypesArray = Array of TPretargetingConfigvendorTypes;
  TPretargetingConfigverticals = class;
  TPretargetingConfigverticalsArray = Array of TPretargetingConfigverticals;
  TPretargetingConfigList = class;
  TPretargetingConfigListArray = Array of TPretargetingConfigList;
  TPretargetingConfigListitems = class;
  TPretargetingConfigListitemsArray = Array of TPretargetingConfigListitems;
  
  { --------------------------------------------------------------------
    TAccount
    --------------------------------------------------------------------}
  
  TAccount = Class(TGoogleBaseObject)
  Private
    FbidderLocation : TAccountbidderLocation;
    FcookieMatchingNid : string;
    FcookieMatchingUrl : string;
    Fid : integer;
    Fkind : string;
    FmaximumActiveCreatives : integer;
    FmaximumTotalQps : integer;
    FnumberActiveCreatives : integer;
  Protected
    //Property setters
    Procedure SetbidderLocation(AIndex : Integer; AValue : TAccountbidderLocation); virtual;
    Procedure SetcookieMatchingNid(AIndex : Integer; AValue : string); virtual;
    Procedure SetcookieMatchingUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmaximumActiveCreatives(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmaximumTotalQps(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnumberActiveCreatives(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property bidderLocation : TAccountbidderLocation Index 0 Read FbidderLocation Write SetbidderLocation;
    Property cookieMatchingNid : string Index 8 Read FcookieMatchingNid Write SetcookieMatchingNid;
    Property cookieMatchingUrl : string Index 16 Read FcookieMatchingUrl Write SetcookieMatchingUrl;
    Property id : integer Index 24 Read Fid Write Setid;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property maximumActiveCreatives : integer Index 40 Read FmaximumActiveCreatives Write SetmaximumActiveCreatives;
    Property maximumTotalQps : integer Index 48 Read FmaximumTotalQps Write SetmaximumTotalQps;
    Property numberActiveCreatives : integer Index 56 Read FnumberActiveCreatives Write SetnumberActiveCreatives;
  end;
  TAccountClass = Class of TAccount;
  
  { --------------------------------------------------------------------
    TAccountbidderLocation
    --------------------------------------------------------------------}
  
  TAccountbidderLocation = Class(TGoogleBaseObject)
  Private
    FmaximumQps : integer;
    Fregion : string;
    Furl : string;
  Protected
    //Property setters
    Procedure SetmaximumQps(AIndex : Integer; AValue : integer); virtual;
    Procedure Setregion(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property maximumQps : integer Index 0 Read FmaximumQps Write SetmaximumQps;
    Property region : string Index 8 Read Fregion Write Setregion;
    Property url : string Index 16 Read Furl Write Seturl;
  end;
  TAccountbidderLocationClass = Class of TAccountbidderLocation;
  
  { --------------------------------------------------------------------
    TAccountsList
    --------------------------------------------------------------------}
  
  TAccountsList = Class(TGoogleBaseObject)
  Private
    Fitems : TAccountsListitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TAccountsListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TAccountsListitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TAccountsListClass = Class of TAccountsList;
  
  { --------------------------------------------------------------------
    TAccountsListitems
    --------------------------------------------------------------------}
  
  TAccountsListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountsListitemsClass = Class of TAccountsListitems;
  
  { --------------------------------------------------------------------
    TBillingInfo
    --------------------------------------------------------------------}
  
  TBillingInfo = Class(TGoogleBaseObject)
  Private
    FaccountId : integer;
    FaccountName : string;
    FbillingId : TBillingInfobillingId;
    Fkind : string;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetaccountName(AIndex : Integer; AValue : string); virtual;
    Procedure SetbillingId(AIndex : Integer; AValue : TBillingInfobillingId); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : integer Index 0 Read FaccountId Write SetaccountId;
    Property accountName : string Index 8 Read FaccountName Write SetaccountName;
    Property billingId : TBillingInfobillingId Index 16 Read FbillingId Write SetbillingId;
    Property kind : string Index 24 Read Fkind Write Setkind;
  end;
  TBillingInfoClass = Class of TBillingInfo;
  
  { --------------------------------------------------------------------
    TBillingInfobillingId
    --------------------------------------------------------------------}
  
  TBillingInfobillingId = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBillingInfobillingIdClass = Class of TBillingInfobillingId;
  
  { --------------------------------------------------------------------
    TBillingInfoList
    --------------------------------------------------------------------}
  
  TBillingInfoList = Class(TGoogleBaseObject)
  Private
    Fitems : TBillingInfoListitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TBillingInfoListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TBillingInfoListitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TBillingInfoListClass = Class of TBillingInfoList;
  
  { --------------------------------------------------------------------
    TBillingInfoListitems
    --------------------------------------------------------------------}
  
  TBillingInfoListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBillingInfoListitemsClass = Class of TBillingInfoListitems;
  
  { --------------------------------------------------------------------
    TBudget
    --------------------------------------------------------------------}
  
  TBudget = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    FbillingId : string;
    FbudgetAmount : string;
    FcurrencyCode : string;
    Fid : string;
    Fkind : string;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetbillingId(AIndex : Integer; AValue : string); virtual;
    Procedure SetbudgetAmount(AIndex : Integer; AValue : string); virtual;
    Procedure SetcurrencyCode(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property billingId : string Index 8 Read FbillingId Write SetbillingId;
    Property budgetAmount : string Index 16 Read FbudgetAmount Write SetbudgetAmount;
    Property currencyCode : string Index 24 Read FcurrencyCode Write SetcurrencyCode;
    Property id : string Index 32 Read Fid Write Setid;
    Property kind : string Index 40 Read Fkind Write Setkind;
  end;
  TBudgetClass = Class of TBudget;
  
  { --------------------------------------------------------------------
    TCreative
    --------------------------------------------------------------------}
  
  TCreative = Class(TGoogleBaseObject)
  Private
    FHTMLSnippet : string;
    FaccountId : integer;
    FadvertiserId : TCreativeadvertiserId;
    FadvertiserName : string;
    FagencyId : string;
    Fattribute : TCreativeattribute;
    FbuyerCreativeId : string;
    FclickThroughUrl : TCreativeclickThroughUrl;
    Fcorrections : TCreativecorrections;
    FdisapprovalReasons : TCreativedisapprovalReasons;
    FfilteringReasons : TCreativefilteringReasons;
    Fheight : integer;
    Fkind : string;
    FproductCategories : TCreativeproductCategories;
    FrestrictedCategories : TCreativerestrictedCategories;
    FsensitiveCategories : TCreativesensitiveCategories;
    Fstatus : string;
    FvendorType : TCreativevendorType;
    FvideoURL : string;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure SetHTMLSnippet(AIndex : Integer; AValue : string); virtual;
    Procedure SetaccountId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : TCreativeadvertiserId); virtual;
    Procedure SetadvertiserName(AIndex : Integer; AValue : string); virtual;
    Procedure SetagencyId(AIndex : Integer; AValue : string); virtual;
    Procedure Setattribute(AIndex : Integer; AValue : TCreativeattribute); virtual;
    Procedure SetbuyerCreativeId(AIndex : Integer; AValue : string); virtual;
    Procedure SetclickThroughUrl(AIndex : Integer; AValue : TCreativeclickThroughUrl); virtual;
    Procedure Setcorrections(AIndex : Integer; AValue : TCreativecorrections); virtual;
    Procedure SetdisapprovalReasons(AIndex : Integer; AValue : TCreativedisapprovalReasons); virtual;
    Procedure SetfilteringReasons(AIndex : Integer; AValue : TCreativefilteringReasons); virtual;
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetproductCategories(AIndex : Integer; AValue : TCreativeproductCategories); virtual;
    Procedure SetrestrictedCategories(AIndex : Integer; AValue : TCreativerestrictedCategories); virtual;
    Procedure SetsensitiveCategories(AIndex : Integer; AValue : TCreativesensitiveCategories); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure SetvendorType(AIndex : Integer; AValue : TCreativevendorType); virtual;
    Procedure SetvideoURL(AIndex : Integer; AValue : string); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property HTMLSnippet : string Index 0 Read FHTMLSnippet Write SetHTMLSnippet;
    Property accountId : integer Index 8 Read FaccountId Write SetaccountId;
    Property advertiserId : TCreativeadvertiserId Index 16 Read FadvertiserId Write SetadvertiserId;
    Property advertiserName : string Index 24 Read FadvertiserName Write SetadvertiserName;
    Property agencyId : string Index 32 Read FagencyId Write SetagencyId;
    Property attribute : TCreativeattribute Index 40 Read Fattribute Write Setattribute;
    Property buyerCreativeId : string Index 48 Read FbuyerCreativeId Write SetbuyerCreativeId;
    Property clickThroughUrl : TCreativeclickThroughUrl Index 56 Read FclickThroughUrl Write SetclickThroughUrl;
    Property corrections : TCreativecorrections Index 64 Read Fcorrections Write Setcorrections;
    Property disapprovalReasons : TCreativedisapprovalReasons Index 72 Read FdisapprovalReasons Write SetdisapprovalReasons;
    Property filteringReasons : TCreativefilteringReasons Index 80 Read FfilteringReasons Write SetfilteringReasons;
    Property height : integer Index 88 Read Fheight Write Setheight;
    Property kind : string Index 96 Read Fkind Write Setkind;
    Property productCategories : TCreativeproductCategories Index 104 Read FproductCategories Write SetproductCategories;
    Property restrictedCategories : TCreativerestrictedCategories Index 112 Read FrestrictedCategories Write SetrestrictedCategories;
    Property sensitiveCategories : TCreativesensitiveCategories Index 120 Read FsensitiveCategories Write SetsensitiveCategories;
    Property status : string Index 128 Read Fstatus Write Setstatus;
    Property vendorType : TCreativevendorType Index 136 Read FvendorType Write SetvendorType;
    Property videoURL : string Index 144 Read FvideoURL Write SetvideoURL;
    Property width : integer Index 152 Read Fwidth Write Setwidth;
  end;
  TCreativeClass = Class of TCreative;
  
  { --------------------------------------------------------------------
    TCreativeadvertiserId
    --------------------------------------------------------------------}
  
  TCreativeadvertiserId = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCreativeadvertiserIdClass = Class of TCreativeadvertiserId;
  
  { --------------------------------------------------------------------
    TCreativeattribute
    --------------------------------------------------------------------}
  
  TCreativeattribute = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCreativeattributeClass = Class of TCreativeattribute;
  
  { --------------------------------------------------------------------
    TCreativeclickThroughUrl
    --------------------------------------------------------------------}
  
  TCreativeclickThroughUrl = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCreativeclickThroughUrlClass = Class of TCreativeclickThroughUrl;
  
  { --------------------------------------------------------------------
    TCreativecorrections
    --------------------------------------------------------------------}
  
  TCreativecorrections = Class(TGoogleBaseObject)
  Private
    Fdetails : TCreativecorrectionsdetails;
    Freason : string;
  Protected
    //Property setters
    Procedure Setdetails(AIndex : Integer; AValue : TCreativecorrectionsdetails); virtual;
    Procedure Setreason(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property details : TCreativecorrectionsdetails Index 0 Read Fdetails Write Setdetails;
    Property reason : string Index 8 Read Freason Write Setreason;
  end;
  TCreativecorrectionsClass = Class of TCreativecorrections;
  
  { --------------------------------------------------------------------
    TCreativecorrectionsdetails
    --------------------------------------------------------------------}
  
  TCreativecorrectionsdetails = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCreativecorrectionsdetailsClass = Class of TCreativecorrectionsdetails;
  
  { --------------------------------------------------------------------
    TCreativedisapprovalReasons
    --------------------------------------------------------------------}
  
  TCreativedisapprovalReasons = Class(TGoogleBaseObject)
  Private
    Fdetails : TCreativedisapprovalReasonsdetails;
    Freason : string;
  Protected
    //Property setters
    Procedure Setdetails(AIndex : Integer; AValue : TCreativedisapprovalReasonsdetails); virtual;
    Procedure Setreason(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property details : TCreativedisapprovalReasonsdetails Index 0 Read Fdetails Write Setdetails;
    Property reason : string Index 8 Read Freason Write Setreason;
  end;
  TCreativedisapprovalReasonsClass = Class of TCreativedisapprovalReasons;
  
  { --------------------------------------------------------------------
    TCreativedisapprovalReasonsdetails
    --------------------------------------------------------------------}
  
  TCreativedisapprovalReasonsdetails = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCreativedisapprovalReasonsdetailsClass = Class of TCreativedisapprovalReasonsdetails;
  
  { --------------------------------------------------------------------
    TCreativefilteringReasons
    --------------------------------------------------------------------}
  
  TCreativefilteringReasons = Class(TGoogleBaseObject)
  Private
    Fdate : string;
    Freasons : TCreativefilteringReasonsreasons;
  Protected
    //Property setters
    Procedure Setdate(AIndex : Integer; AValue : string); virtual;
    Procedure Setreasons(AIndex : Integer; AValue : TCreativefilteringReasonsreasons); virtual;
  Public
  Published
    Property date : string Index 0 Read Fdate Write Setdate;
    Property reasons : TCreativefilteringReasonsreasons Index 8 Read Freasons Write Setreasons;
  end;
  TCreativefilteringReasonsClass = Class of TCreativefilteringReasons;
  
  { --------------------------------------------------------------------
    TCreativefilteringReasonsreasons
    --------------------------------------------------------------------}
  
  TCreativefilteringReasonsreasons = Class(TGoogleBaseObject)
  Private
    FfilteringCount : string;
    FfilteringStatus : integer;
  Protected
    //Property setters
    Procedure SetfilteringCount(AIndex : Integer; AValue : string); virtual;
    Procedure SetfilteringStatus(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property filteringCount : string Index 0 Read FfilteringCount Write SetfilteringCount;
    Property filteringStatus : integer Index 8 Read FfilteringStatus Write SetfilteringStatus;
  end;
  TCreativefilteringReasonsreasonsClass = Class of TCreativefilteringReasonsreasons;
  
  { --------------------------------------------------------------------
    TCreativeproductCategories
    --------------------------------------------------------------------}
  
  TCreativeproductCategories = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCreativeproductCategoriesClass = Class of TCreativeproductCategories;
  
  { --------------------------------------------------------------------
    TCreativerestrictedCategories
    --------------------------------------------------------------------}
  
  TCreativerestrictedCategories = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCreativerestrictedCategoriesClass = Class of TCreativerestrictedCategories;
  
  { --------------------------------------------------------------------
    TCreativesensitiveCategories
    --------------------------------------------------------------------}
  
  TCreativesensitiveCategories = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCreativesensitiveCategoriesClass = Class of TCreativesensitiveCategories;
  
  { --------------------------------------------------------------------
    TCreativevendorType
    --------------------------------------------------------------------}
  
  TCreativevendorType = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCreativevendorTypeClass = Class of TCreativevendorType;
  
  { --------------------------------------------------------------------
    TCreativesList
    --------------------------------------------------------------------}
  
  TCreativesList = Class(TGoogleBaseObject)
  Private
    Fitems : TCreativesListitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TCreativesListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TCreativesListitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TCreativesListClass = Class of TCreativesList;
  
  { --------------------------------------------------------------------
    TCreativesListitems
    --------------------------------------------------------------------}
  
  TCreativesListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCreativesListitemsClass = Class of TCreativesListitems;
  
  { --------------------------------------------------------------------
    TDirectDeal
    --------------------------------------------------------------------}
  
  TDirectDeal = Class(TGoogleBaseObject)
  Private
    FaccountId : integer;
    Fadvertiser : string;
    FcurrencyCode : string;
    FendTime : string;
    FfixedCpm : string;
    Fid : string;
    Fkind : string;
    Fname : string;
    FprivateExchangeMinCpm : string;
    FpublisherBlocksOverriden : boolean;
    FsellerNetwork : string;
    FstartTime : string;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : integer); virtual;
    Procedure Setadvertiser(AIndex : Integer; AValue : string); virtual;
    Procedure SetcurrencyCode(AIndex : Integer; AValue : string); virtual;
    Procedure SetendTime(AIndex : Integer; AValue : string); virtual;
    Procedure SetfixedCpm(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetprivateExchangeMinCpm(AIndex : Integer; AValue : string); virtual;
    Procedure SetpublisherBlocksOverriden(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsellerNetwork(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : integer Index 0 Read FaccountId Write SetaccountId;
    Property advertiser : string Index 8 Read Fadvertiser Write Setadvertiser;
    Property currencyCode : string Index 16 Read FcurrencyCode Write SetcurrencyCode;
    Property endTime : string Index 24 Read FendTime Write SetendTime;
    Property fixedCpm : string Index 32 Read FfixedCpm Write SetfixedCpm;
    Property id : string Index 40 Read Fid Write Setid;
    Property kind : string Index 48 Read Fkind Write Setkind;
    Property name : string Index 56 Read Fname Write Setname;
    Property privateExchangeMinCpm : string Index 64 Read FprivateExchangeMinCpm Write SetprivateExchangeMinCpm;
    Property publisherBlocksOverriden : boolean Index 72 Read FpublisherBlocksOverriden Write SetpublisherBlocksOverriden;
    Property sellerNetwork : string Index 80 Read FsellerNetwork Write SetsellerNetwork;
    Property startTime : string Index 88 Read FstartTime Write SetstartTime;
  end;
  TDirectDealClass = Class of TDirectDeal;
  
  { --------------------------------------------------------------------
    TDirectDealsList
    --------------------------------------------------------------------}
  
  TDirectDealsList = Class(TGoogleBaseObject)
  Private
    FdirectDeals : TDirectDealsListdirectDeals;
    Fkind : string;
  Protected
    //Property setters
    Procedure SetdirectDeals(AIndex : Integer; AValue : TDirectDealsListdirectDeals); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property directDeals : TDirectDealsListdirectDeals Index 0 Read FdirectDeals Write SetdirectDeals;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TDirectDealsListClass = Class of TDirectDealsList;
  
  { --------------------------------------------------------------------
    TDirectDealsListdirectDeals
    --------------------------------------------------------------------}
  
  TDirectDealsListdirectDeals = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDirectDealsListdirectDealsClass = Class of TDirectDealsListdirectDeals;
  
  { --------------------------------------------------------------------
    TPerformanceReport
    --------------------------------------------------------------------}
  
  TPerformanceReport = Class(TGoogleBaseObject)
  Private
    FcalloutStatusRate : TPerformanceReportcalloutStatusRate;
    FcookieMatcherStatusRate : TPerformanceReportcookieMatcherStatusRate;
    FcreativeStatusRate : TPerformanceReportcreativeStatusRate;
    FhostedMatchStatusRate : TPerformanceReporthostedMatchStatusRate;
    Fkind : string;
    Flatency50thPercentile : double;
    Flatency85thPercentile : double;
    Flatency95thPercentile : double;
    FnoQuotaInRegion : double;
    FoutOfQuota : double;
    FpixelMatchRequests : double;
    FpixelMatchResponses : double;
    FquotaConfiguredLimit : double;
    FquotaThrottledLimit : double;
    Fregion : string;
    Ftimestamp : string;
  Protected
    //Property setters
    Procedure SetcalloutStatusRate(AIndex : Integer; AValue : TPerformanceReportcalloutStatusRate); virtual;
    Procedure SetcookieMatcherStatusRate(AIndex : Integer; AValue : TPerformanceReportcookieMatcherStatusRate); virtual;
    Procedure SetcreativeStatusRate(AIndex : Integer; AValue : TPerformanceReportcreativeStatusRate); virtual;
    Procedure SethostedMatchStatusRate(AIndex : Integer; AValue : TPerformanceReporthostedMatchStatusRate); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlatency50thPercentile(AIndex : Integer; AValue : double); virtual;
    Procedure Setlatency85thPercentile(AIndex : Integer; AValue : double); virtual;
    Procedure Setlatency95thPercentile(AIndex : Integer; AValue : double); virtual;
    Procedure SetnoQuotaInRegion(AIndex : Integer; AValue : double); virtual;
    Procedure SetoutOfQuota(AIndex : Integer; AValue : double); virtual;
    Procedure SetpixelMatchRequests(AIndex : Integer; AValue : double); virtual;
    Procedure SetpixelMatchResponses(AIndex : Integer; AValue : double); virtual;
    Procedure SetquotaConfiguredLimit(AIndex : Integer; AValue : double); virtual;
    Procedure SetquotaThrottledLimit(AIndex : Integer; AValue : double); virtual;
    Procedure Setregion(AIndex : Integer; AValue : string); virtual;
    Procedure Settimestamp(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property calloutStatusRate : TPerformanceReportcalloutStatusRate Index 0 Read FcalloutStatusRate Write SetcalloutStatusRate;
    Property cookieMatcherStatusRate : TPerformanceReportcookieMatcherStatusRate Index 8 Read FcookieMatcherStatusRate Write SetcookieMatcherStatusRate;
    Property creativeStatusRate : TPerformanceReportcreativeStatusRate Index 16 Read FcreativeStatusRate Write SetcreativeStatusRate;
    Property hostedMatchStatusRate : TPerformanceReporthostedMatchStatusRate Index 24 Read FhostedMatchStatusRate Write SethostedMatchStatusRate;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property latency50thPercentile : double Index 40 Read Flatency50thPercentile Write Setlatency50thPercentile;
    Property latency85thPercentile : double Index 48 Read Flatency85thPercentile Write Setlatency85thPercentile;
    Property latency95thPercentile : double Index 56 Read Flatency95thPercentile Write Setlatency95thPercentile;
    Property noQuotaInRegion : double Index 64 Read FnoQuotaInRegion Write SetnoQuotaInRegion;
    Property outOfQuota : double Index 72 Read FoutOfQuota Write SetoutOfQuota;
    Property pixelMatchRequests : double Index 80 Read FpixelMatchRequests Write SetpixelMatchRequests;
    Property pixelMatchResponses : double Index 88 Read FpixelMatchResponses Write SetpixelMatchResponses;
    Property quotaConfiguredLimit : double Index 96 Read FquotaConfiguredLimit Write SetquotaConfiguredLimit;
    Property quotaThrottledLimit : double Index 104 Read FquotaThrottledLimit Write SetquotaThrottledLimit;
    Property region : string Index 112 Read Fregion Write Setregion;
    Property timestamp : string Index 120 Read Ftimestamp Write Settimestamp;
  end;
  TPerformanceReportClass = Class of TPerformanceReport;
  
  { --------------------------------------------------------------------
    TPerformanceReportcalloutStatusRate
    --------------------------------------------------------------------}
  
  TPerformanceReportcalloutStatusRate = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPerformanceReportcalloutStatusRateClass = Class of TPerformanceReportcalloutStatusRate;
  
  { --------------------------------------------------------------------
    TPerformanceReportcookieMatcherStatusRate
    --------------------------------------------------------------------}
  
  TPerformanceReportcookieMatcherStatusRate = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPerformanceReportcookieMatcherStatusRateClass = Class of TPerformanceReportcookieMatcherStatusRate;
  
  { --------------------------------------------------------------------
    TPerformanceReportcreativeStatusRate
    --------------------------------------------------------------------}
  
  TPerformanceReportcreativeStatusRate = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPerformanceReportcreativeStatusRateClass = Class of TPerformanceReportcreativeStatusRate;
  
  { --------------------------------------------------------------------
    TPerformanceReporthostedMatchStatusRate
    --------------------------------------------------------------------}
  
  TPerformanceReporthostedMatchStatusRate = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPerformanceReporthostedMatchStatusRateClass = Class of TPerformanceReporthostedMatchStatusRate;
  
  { --------------------------------------------------------------------
    TPerformanceReportList
    --------------------------------------------------------------------}
  
  TPerformanceReportList = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    FperformanceReport : TPerformanceReportListperformanceReport;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetperformanceReport(AIndex : Integer; AValue : TPerformanceReportListperformanceReport); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property performanceReport : TPerformanceReportListperformanceReport Index 8 Read FperformanceReport Write SetperformanceReport;
  end;
  TPerformanceReportListClass = Class of TPerformanceReportList;
  
  { --------------------------------------------------------------------
    TPerformanceReportListperformanceReport
    --------------------------------------------------------------------}
  
  TPerformanceReportListperformanceReport = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPerformanceReportListperformanceReportClass = Class of TPerformanceReportListperformanceReport;
  
  { --------------------------------------------------------------------
    TPretargetingConfig
    --------------------------------------------------------------------}
  
  TPretargetingConfig = Class(TGoogleBaseObject)
  Private
    FbillingId : string;
    FconfigId : string;
    FconfigName : string;
    FcreativeType : TPretargetingConfigcreativeType;
    Fdimensions : TPretargetingConfigdimensions;
    FexcludedContentLabels : TPretargetingConfigexcludedContentLabels;
    FexcludedGeoCriteriaIds : TPretargetingConfigexcludedGeoCriteriaIds;
    FexcludedPlacements : TPretargetingConfigexcludedPlacements;
    FexcludedUserLists : TPretargetingConfigexcludedUserLists;
    FexcludedVerticals : TPretargetingConfigexcludedVerticals;
    FgeoCriteriaIds : TPretargetingConfiggeoCriteriaIds;
    FisActive : boolean;
    Fkind : string;
    Flanguages : TPretargetingConfiglanguages;
    FmobileCarriers : TPretargetingConfigmobileCarriers;
    FmobileDevices : TPretargetingConfigmobileDevices;
    FmobileOperatingSystemVersions : TPretargetingConfigmobileOperatingSystemVersions;
    Fplacements : TPretargetingConfigplacements;
    Fplatforms : TPretargetingConfigplatforms;
    FsupportedCreativeAttributes : TPretargetingConfigsupportedCreativeAttributes;
    FuserLists : TPretargetingConfiguserLists;
    FvendorTypes : TPretargetingConfigvendorTypes;
    Fverticals : TPretargetingConfigverticals;
  Protected
    //Property setters
    Procedure SetbillingId(AIndex : Integer; AValue : string); virtual;
    Procedure SetconfigId(AIndex : Integer; AValue : string); virtual;
    Procedure SetconfigName(AIndex : Integer; AValue : string); virtual;
    Procedure SetcreativeType(AIndex : Integer; AValue : TPretargetingConfigcreativeType); virtual;
    Procedure Setdimensions(AIndex : Integer; AValue : TPretargetingConfigdimensions); virtual;
    Procedure SetexcludedContentLabels(AIndex : Integer; AValue : TPretargetingConfigexcludedContentLabels); virtual;
    Procedure SetexcludedGeoCriteriaIds(AIndex : Integer; AValue : TPretargetingConfigexcludedGeoCriteriaIds); virtual;
    Procedure SetexcludedPlacements(AIndex : Integer; AValue : TPretargetingConfigexcludedPlacements); virtual;
    Procedure SetexcludedUserLists(AIndex : Integer; AValue : TPretargetingConfigexcludedUserLists); virtual;
    Procedure SetexcludedVerticals(AIndex : Integer; AValue : TPretargetingConfigexcludedVerticals); virtual;
    Procedure SetgeoCriteriaIds(AIndex : Integer; AValue : TPretargetingConfiggeoCriteriaIds); virtual;
    Procedure SetisActive(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlanguages(AIndex : Integer; AValue : TPretargetingConfiglanguages); virtual;
    Procedure SetmobileCarriers(AIndex : Integer; AValue : TPretargetingConfigmobileCarriers); virtual;
    Procedure SetmobileDevices(AIndex : Integer; AValue : TPretargetingConfigmobileDevices); virtual;
    Procedure SetmobileOperatingSystemVersions(AIndex : Integer; AValue : TPretargetingConfigmobileOperatingSystemVersions); virtual;
    Procedure Setplacements(AIndex : Integer; AValue : TPretargetingConfigplacements); virtual;
    Procedure Setplatforms(AIndex : Integer; AValue : TPretargetingConfigplatforms); virtual;
    Procedure SetsupportedCreativeAttributes(AIndex : Integer; AValue : TPretargetingConfigsupportedCreativeAttributes); virtual;
    Procedure SetuserLists(AIndex : Integer; AValue : TPretargetingConfiguserLists); virtual;
    Procedure SetvendorTypes(AIndex : Integer; AValue : TPretargetingConfigvendorTypes); virtual;
    Procedure Setverticals(AIndex : Integer; AValue : TPretargetingConfigverticals); virtual;
  Public
  Published
    Property billingId : string Index 0 Read FbillingId Write SetbillingId;
    Property configId : string Index 8 Read FconfigId Write SetconfigId;
    Property configName : string Index 16 Read FconfigName Write SetconfigName;
    Property creativeType : TPretargetingConfigcreativeType Index 24 Read FcreativeType Write SetcreativeType;
    Property dimensions : TPretargetingConfigdimensions Index 32 Read Fdimensions Write Setdimensions;
    Property excludedContentLabels : TPretargetingConfigexcludedContentLabels Index 40 Read FexcludedContentLabels Write SetexcludedContentLabels;
    Property excludedGeoCriteriaIds : TPretargetingConfigexcludedGeoCriteriaIds Index 48 Read FexcludedGeoCriteriaIds Write SetexcludedGeoCriteriaIds;
    Property excludedPlacements : TPretargetingConfigexcludedPlacements Index 56 Read FexcludedPlacements Write SetexcludedPlacements;
    Property excludedUserLists : TPretargetingConfigexcludedUserLists Index 64 Read FexcludedUserLists Write SetexcludedUserLists;
    Property excludedVerticals : TPretargetingConfigexcludedVerticals Index 72 Read FexcludedVerticals Write SetexcludedVerticals;
    Property geoCriteriaIds : TPretargetingConfiggeoCriteriaIds Index 80 Read FgeoCriteriaIds Write SetgeoCriteriaIds;
    Property isActive : boolean Index 88 Read FisActive Write SetisActive;
    Property kind : string Index 96 Read Fkind Write Setkind;
    Property languages : TPretargetingConfiglanguages Index 104 Read Flanguages Write Setlanguages;
    Property mobileCarriers : TPretargetingConfigmobileCarriers Index 112 Read FmobileCarriers Write SetmobileCarriers;
    Property mobileDevices : TPretargetingConfigmobileDevices Index 120 Read FmobileDevices Write SetmobileDevices;
    Property mobileOperatingSystemVersions : TPretargetingConfigmobileOperatingSystemVersions Index 128 Read FmobileOperatingSystemVersions Write SetmobileOperatingSystemVersions;
    Property placements : TPretargetingConfigplacements Index 136 Read Fplacements Write Setplacements;
    Property platforms : TPretargetingConfigplatforms Index 144 Read Fplatforms Write Setplatforms;
    Property supportedCreativeAttributes : TPretargetingConfigsupportedCreativeAttributes Index 152 Read FsupportedCreativeAttributes Write SetsupportedCreativeAttributes;
    Property userLists : TPretargetingConfiguserLists Index 160 Read FuserLists Write SetuserLists;
    Property vendorTypes : TPretargetingConfigvendorTypes Index 168 Read FvendorTypes Write SetvendorTypes;
    Property verticals : TPretargetingConfigverticals Index 176 Read Fverticals Write Setverticals;
  end;
  TPretargetingConfigClass = Class of TPretargetingConfig;
  
  { --------------------------------------------------------------------
    TPretargetingConfigcreativeType
    --------------------------------------------------------------------}
  
  TPretargetingConfigcreativeType = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPretargetingConfigcreativeTypeClass = Class of TPretargetingConfigcreativeType;
  
  { --------------------------------------------------------------------
    TPretargetingConfigdimensions
    --------------------------------------------------------------------}
  
  TPretargetingConfigdimensions = Class(TGoogleBaseObject)
  Private
    Fheight : string;
    Fwidth : string;
  Protected
    //Property setters
    Procedure Setheight(AIndex : Integer; AValue : string); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property height : string Index 0 Read Fheight Write Setheight;
    Property width : string Index 8 Read Fwidth Write Setwidth;
  end;
  TPretargetingConfigdimensionsClass = Class of TPretargetingConfigdimensions;
  
  { --------------------------------------------------------------------
    TPretargetingConfigexcludedContentLabels
    --------------------------------------------------------------------}
  
  TPretargetingConfigexcludedContentLabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPretargetingConfigexcludedContentLabelsClass = Class of TPretargetingConfigexcludedContentLabels;
  
  { --------------------------------------------------------------------
    TPretargetingConfigexcludedGeoCriteriaIds
    --------------------------------------------------------------------}
  
  TPretargetingConfigexcludedGeoCriteriaIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPretargetingConfigexcludedGeoCriteriaIdsClass = Class of TPretargetingConfigexcludedGeoCriteriaIds;
  
  { --------------------------------------------------------------------
    TPretargetingConfigexcludedPlacements
    --------------------------------------------------------------------}
  
  TPretargetingConfigexcludedPlacements = Class(TGoogleBaseObject)
  Private
    Ftoken : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Settoken(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property token : string Index 0 Read Ftoken Write Settoken;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TPretargetingConfigexcludedPlacementsClass = Class of TPretargetingConfigexcludedPlacements;
  
  { --------------------------------------------------------------------
    TPretargetingConfigexcludedUserLists
    --------------------------------------------------------------------}
  
  TPretargetingConfigexcludedUserLists = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPretargetingConfigexcludedUserListsClass = Class of TPretargetingConfigexcludedUserLists;
  
  { --------------------------------------------------------------------
    TPretargetingConfigexcludedVerticals
    --------------------------------------------------------------------}
  
  TPretargetingConfigexcludedVerticals = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPretargetingConfigexcludedVerticalsClass = Class of TPretargetingConfigexcludedVerticals;
  
  { --------------------------------------------------------------------
    TPretargetingConfiggeoCriteriaIds
    --------------------------------------------------------------------}
  
  TPretargetingConfiggeoCriteriaIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPretargetingConfiggeoCriteriaIdsClass = Class of TPretargetingConfiggeoCriteriaIds;
  
  { --------------------------------------------------------------------
    TPretargetingConfiglanguages
    --------------------------------------------------------------------}
  
  TPretargetingConfiglanguages = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPretargetingConfiglanguagesClass = Class of TPretargetingConfiglanguages;
  
  { --------------------------------------------------------------------
    TPretargetingConfigmobileCarriers
    --------------------------------------------------------------------}
  
  TPretargetingConfigmobileCarriers = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPretargetingConfigmobileCarriersClass = Class of TPretargetingConfigmobileCarriers;
  
  { --------------------------------------------------------------------
    TPretargetingConfigmobileDevices
    --------------------------------------------------------------------}
  
  TPretargetingConfigmobileDevices = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPretargetingConfigmobileDevicesClass = Class of TPretargetingConfigmobileDevices;
  
  { --------------------------------------------------------------------
    TPretargetingConfigmobileOperatingSystemVersions
    --------------------------------------------------------------------}
  
  TPretargetingConfigmobileOperatingSystemVersions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPretargetingConfigmobileOperatingSystemVersionsClass = Class of TPretargetingConfigmobileOperatingSystemVersions;
  
  { --------------------------------------------------------------------
    TPretargetingConfigplacements
    --------------------------------------------------------------------}
  
  TPretargetingConfigplacements = Class(TGoogleBaseObject)
  Private
    Ftoken : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Settoken(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property token : string Index 0 Read Ftoken Write Settoken;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TPretargetingConfigplacementsClass = Class of TPretargetingConfigplacements;
  
  { --------------------------------------------------------------------
    TPretargetingConfigplatforms
    --------------------------------------------------------------------}
  
  TPretargetingConfigplatforms = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPretargetingConfigplatformsClass = Class of TPretargetingConfigplatforms;
  
  { --------------------------------------------------------------------
    TPretargetingConfigsupportedCreativeAttributes
    --------------------------------------------------------------------}
  
  TPretargetingConfigsupportedCreativeAttributes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPretargetingConfigsupportedCreativeAttributesClass = Class of TPretargetingConfigsupportedCreativeAttributes;
  
  { --------------------------------------------------------------------
    TPretargetingConfiguserLists
    --------------------------------------------------------------------}
  
  TPretargetingConfiguserLists = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPretargetingConfiguserListsClass = Class of TPretargetingConfiguserLists;
  
  { --------------------------------------------------------------------
    TPretargetingConfigvendorTypes
    --------------------------------------------------------------------}
  
  TPretargetingConfigvendorTypes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPretargetingConfigvendorTypesClass = Class of TPretargetingConfigvendorTypes;
  
  { --------------------------------------------------------------------
    TPretargetingConfigverticals
    --------------------------------------------------------------------}
  
  TPretargetingConfigverticals = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPretargetingConfigverticalsClass = Class of TPretargetingConfigverticals;
  
  { --------------------------------------------------------------------
    TPretargetingConfigList
    --------------------------------------------------------------------}
  
  TPretargetingConfigList = Class(TGoogleBaseObject)
  Private
    Fitems : TPretargetingConfigListitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TPretargetingConfigListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TPretargetingConfigListitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TPretargetingConfigListClass = Class of TPretargetingConfigList;
  
  { --------------------------------------------------------------------
    TPretargetingConfigListitems
    --------------------------------------------------------------------}
  
  TPretargetingConfigListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPretargetingConfigListitemsClass = Class of TPretargetingConfigListitems;
  
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
    buyerCreativeId : string;
    maxResults : integer;
    pageToken : string;
    statusFilter : string;
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
    endDateTime : string;
    maxResults : integer;
    pageToken : string;
    startDateTime : string;
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
  TAccount
  --------------------------------------------------------------------}


Procedure TAccount.SetbidderLocation(AIndex : Integer; AValue : TAccountbidderLocation); 

begin
  If (FbidderLocation=AValue) then exit;
  FbidderLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetcookieMatchingNid(AIndex : Integer; AValue : string); 

begin
  If (FcookieMatchingNid=AValue) then exit;
  FcookieMatchingNid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetcookieMatchingUrl(AIndex : Integer; AValue : string); 

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



Procedure TAccount.Setkind(AIndex : Integer; AValue : string); 

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
  TAccountbidderLocation
  --------------------------------------------------------------------}


Procedure TAccountbidderLocation.SetmaximumQps(AIndex : Integer; AValue : integer); 

begin
  If (FmaximumQps=AValue) then exit;
  FmaximumQps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountbidderLocation.Setregion(AIndex : Integer; AValue : string); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountbidderLocation.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountsList
  --------------------------------------------------------------------}


Procedure TAccountsList.Setitems(AIndex : Integer; AValue : TAccountsListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountsListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TBillingInfo
  --------------------------------------------------------------------}


Procedure TBillingInfo.SetaccountId(AIndex : Integer; AValue : integer); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBillingInfo.SetaccountName(AIndex : Integer; AValue : string); 

begin
  If (FaccountName=AValue) then exit;
  FaccountName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBillingInfo.SetbillingId(AIndex : Integer; AValue : TBillingInfobillingId); 

begin
  If (FbillingId=AValue) then exit;
  FbillingId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBillingInfo.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBillingInfobillingId
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TBillingInfoList
  --------------------------------------------------------------------}


Procedure TBillingInfoList.Setitems(AIndex : Integer; AValue : TBillingInfoListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBillingInfoList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBillingInfoListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TBudget
  --------------------------------------------------------------------}


Procedure TBudget.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBudget.SetbillingId(AIndex : Integer; AValue : string); 

begin
  If (FbillingId=AValue) then exit;
  FbillingId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBudget.SetbudgetAmount(AIndex : Integer; AValue : string); 

begin
  If (FbudgetAmount=AValue) then exit;
  FbudgetAmount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBudget.SetcurrencyCode(AIndex : Integer; AValue : string); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBudget.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBudget.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreative
  --------------------------------------------------------------------}


Procedure TCreative.SetHTMLSnippet(AIndex : Integer; AValue : string); 

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



Procedure TCreative.SetadvertiserId(AIndex : Integer; AValue : TCreativeadvertiserId); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetadvertiserName(AIndex : Integer; AValue : string); 

begin
  If (FadvertiserName=AValue) then exit;
  FadvertiserName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetagencyId(AIndex : Integer; AValue : string); 

begin
  If (FagencyId=AValue) then exit;
  FagencyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setattribute(AIndex : Integer; AValue : TCreativeattribute); 

begin
  If (Fattribute=AValue) then exit;
  Fattribute:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetbuyerCreativeId(AIndex : Integer; AValue : string); 

begin
  If (FbuyerCreativeId=AValue) then exit;
  FbuyerCreativeId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetclickThroughUrl(AIndex : Integer; AValue : TCreativeclickThroughUrl); 

begin
  If (FclickThroughUrl=AValue) then exit;
  FclickThroughUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setcorrections(AIndex : Integer; AValue : TCreativecorrections); 

begin
  If (Fcorrections=AValue) then exit;
  Fcorrections:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetdisapprovalReasons(AIndex : Integer; AValue : TCreativedisapprovalReasons); 

begin
  If (FdisapprovalReasons=AValue) then exit;
  FdisapprovalReasons:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetfilteringReasons(AIndex : Integer; AValue : TCreativefilteringReasons); 

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



Procedure TCreative.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetproductCategories(AIndex : Integer; AValue : TCreativeproductCategories); 

begin
  If (FproductCategories=AValue) then exit;
  FproductCategories:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetrestrictedCategories(AIndex : Integer; AValue : TCreativerestrictedCategories); 

begin
  If (FrestrictedCategories=AValue) then exit;
  FrestrictedCategories:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetsensitiveCategories(AIndex : Integer; AValue : TCreativesensitiveCategories); 

begin
  If (FsensitiveCategories=AValue) then exit;
  FsensitiveCategories:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetvendorType(AIndex : Integer; AValue : TCreativevendorType); 

begin
  If (FvendorType=AValue) then exit;
  FvendorType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetvideoURL(AIndex : Integer; AValue : string); 

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
  TCreativeadvertiserId
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCreativeattribute
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCreativeclickThroughUrl
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCreativecorrections
  --------------------------------------------------------------------}


Procedure TCreativecorrections.Setdetails(AIndex : Integer; AValue : TCreativecorrectionsdetails); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativecorrections.Setreason(AIndex : Integer; AValue : string); 

begin
  If (Freason=AValue) then exit;
  Freason:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativecorrectionsdetails
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCreativedisapprovalReasons
  --------------------------------------------------------------------}


Procedure TCreativedisapprovalReasons.Setdetails(AIndex : Integer; AValue : TCreativedisapprovalReasonsdetails); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativedisapprovalReasons.Setreason(AIndex : Integer; AValue : string); 

begin
  If (Freason=AValue) then exit;
  Freason:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativedisapprovalReasonsdetails
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCreativefilteringReasons
  --------------------------------------------------------------------}


Procedure TCreativefilteringReasons.Setdate(AIndex : Integer; AValue : string); 

begin
  If (Fdate=AValue) then exit;
  Fdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativefilteringReasons.Setreasons(AIndex : Integer; AValue : TCreativefilteringReasonsreasons); 

begin
  If (Freasons=AValue) then exit;
  Freasons:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativefilteringReasonsreasons
  --------------------------------------------------------------------}


Procedure TCreativefilteringReasonsreasons.SetfilteringCount(AIndex : Integer; AValue : string); 

begin
  If (FfilteringCount=AValue) then exit;
  FfilteringCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativefilteringReasonsreasons.SetfilteringStatus(AIndex : Integer; AValue : integer); 

begin
  If (FfilteringStatus=AValue) then exit;
  FfilteringStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativeproductCategories
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCreativerestrictedCategories
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCreativesensitiveCategories
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCreativevendorType
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCreativesList
  --------------------------------------------------------------------}


Procedure TCreativesList.Setitems(AIndex : Integer; AValue : TCreativesListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativesList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativesList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativesListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDirectDeal
  --------------------------------------------------------------------}


Procedure TDirectDeal.SetaccountId(AIndex : Integer; AValue : integer); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectDeal.Setadvertiser(AIndex : Integer; AValue : string); 

begin
  If (Fadvertiser=AValue) then exit;
  Fadvertiser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectDeal.SetcurrencyCode(AIndex : Integer; AValue : string); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectDeal.SetendTime(AIndex : Integer; AValue : string); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectDeal.SetfixedCpm(AIndex : Integer; AValue : string); 

begin
  If (FfixedCpm=AValue) then exit;
  FfixedCpm:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectDeal.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectDeal.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectDeal.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectDeal.SetprivateExchangeMinCpm(AIndex : Integer; AValue : string); 

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



Procedure TDirectDeal.SetsellerNetwork(AIndex : Integer; AValue : string); 

begin
  If (FsellerNetwork=AValue) then exit;
  FsellerNetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectDeal.SetstartTime(AIndex : Integer; AValue : string); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDirectDealsList
  --------------------------------------------------------------------}


Procedure TDirectDealsList.SetdirectDeals(AIndex : Integer; AValue : TDirectDealsListdirectDeals); 

begin
  If (FdirectDeals=AValue) then exit;
  FdirectDeals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDirectDealsList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDirectDealsListdirectDeals
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPerformanceReport
  --------------------------------------------------------------------}


Procedure TPerformanceReport.SetcalloutStatusRate(AIndex : Integer; AValue : TPerformanceReportcalloutStatusRate); 

begin
  If (FcalloutStatusRate=AValue) then exit;
  FcalloutStatusRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetcookieMatcherStatusRate(AIndex : Integer; AValue : TPerformanceReportcookieMatcherStatusRate); 

begin
  If (FcookieMatcherStatusRate=AValue) then exit;
  FcookieMatcherStatusRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetcreativeStatusRate(AIndex : Integer; AValue : TPerformanceReportcreativeStatusRate); 

begin
  If (FcreativeStatusRate=AValue) then exit;
  FcreativeStatusRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SethostedMatchStatusRate(AIndex : Integer; AValue : TPerformanceReporthostedMatchStatusRate); 

begin
  If (FhostedMatchStatusRate=AValue) then exit;
  FhostedMatchStatusRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.Setkind(AIndex : Integer; AValue : string); 

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



Procedure TPerformanceReport.Setregion(AIndex : Integer; AValue : string); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.Settimestamp(AIndex : Integer; AValue : string); 

begin
  If (Ftimestamp=AValue) then exit;
  Ftimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPerformanceReportcalloutStatusRate
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPerformanceReportcookieMatcherStatusRate
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPerformanceReportcreativeStatusRate
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPerformanceReporthostedMatchStatusRate
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPerformanceReportList
  --------------------------------------------------------------------}


Procedure TPerformanceReportList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReportList.SetperformanceReport(AIndex : Integer; AValue : TPerformanceReportListperformanceReport); 

begin
  If (FperformanceReport=AValue) then exit;
  FperformanceReport:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPerformanceReportListperformanceReport
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPretargetingConfig
  --------------------------------------------------------------------}


Procedure TPretargetingConfig.SetbillingId(AIndex : Integer; AValue : string); 

begin
  If (FbillingId=AValue) then exit;
  FbillingId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetconfigId(AIndex : Integer; AValue : string); 

begin
  If (FconfigId=AValue) then exit;
  FconfigId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetconfigName(AIndex : Integer; AValue : string); 

begin
  If (FconfigName=AValue) then exit;
  FconfigName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetcreativeType(AIndex : Integer; AValue : TPretargetingConfigcreativeType); 

begin
  If (FcreativeType=AValue) then exit;
  FcreativeType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.Setdimensions(AIndex : Integer; AValue : TPretargetingConfigdimensions); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetexcludedContentLabels(AIndex : Integer; AValue : TPretargetingConfigexcludedContentLabels); 

begin
  If (FexcludedContentLabels=AValue) then exit;
  FexcludedContentLabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetexcludedGeoCriteriaIds(AIndex : Integer; AValue : TPretargetingConfigexcludedGeoCriteriaIds); 

begin
  If (FexcludedGeoCriteriaIds=AValue) then exit;
  FexcludedGeoCriteriaIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetexcludedPlacements(AIndex : Integer; AValue : TPretargetingConfigexcludedPlacements); 

begin
  If (FexcludedPlacements=AValue) then exit;
  FexcludedPlacements:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetexcludedUserLists(AIndex : Integer; AValue : TPretargetingConfigexcludedUserLists); 

begin
  If (FexcludedUserLists=AValue) then exit;
  FexcludedUserLists:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetexcludedVerticals(AIndex : Integer; AValue : TPretargetingConfigexcludedVerticals); 

begin
  If (FexcludedVerticals=AValue) then exit;
  FexcludedVerticals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetgeoCriteriaIds(AIndex : Integer; AValue : TPretargetingConfiggeoCriteriaIds); 

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



Procedure TPretargetingConfig.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.Setlanguages(AIndex : Integer; AValue : TPretargetingConfiglanguages); 

begin
  If (Flanguages=AValue) then exit;
  Flanguages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetmobileCarriers(AIndex : Integer; AValue : TPretargetingConfigmobileCarriers); 

begin
  If (FmobileCarriers=AValue) then exit;
  FmobileCarriers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetmobileDevices(AIndex : Integer; AValue : TPretargetingConfigmobileDevices); 

begin
  If (FmobileDevices=AValue) then exit;
  FmobileDevices:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetmobileOperatingSystemVersions(AIndex : Integer; AValue : TPretargetingConfigmobileOperatingSystemVersions); 

begin
  If (FmobileOperatingSystemVersions=AValue) then exit;
  FmobileOperatingSystemVersions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.Setplacements(AIndex : Integer; AValue : TPretargetingConfigplacements); 

begin
  If (Fplacements=AValue) then exit;
  Fplacements:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.Setplatforms(AIndex : Integer; AValue : TPretargetingConfigplatforms); 

begin
  If (Fplatforms=AValue) then exit;
  Fplatforms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetsupportedCreativeAttributes(AIndex : Integer; AValue : TPretargetingConfigsupportedCreativeAttributes); 

begin
  If (FsupportedCreativeAttributes=AValue) then exit;
  FsupportedCreativeAttributes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetuserLists(AIndex : Integer; AValue : TPretargetingConfiguserLists); 

begin
  If (FuserLists=AValue) then exit;
  FuserLists:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetvendorTypes(AIndex : Integer; AValue : TPretargetingConfigvendorTypes); 

begin
  If (FvendorTypes=AValue) then exit;
  FvendorTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.Setverticals(AIndex : Integer; AValue : TPretargetingConfigverticals); 

begin
  If (Fverticals=AValue) then exit;
  Fverticals:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPretargetingConfigcreativeType
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPretargetingConfigdimensions
  --------------------------------------------------------------------}


Procedure TPretargetingConfigdimensions.Setheight(AIndex : Integer; AValue : string); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfigdimensions.Setwidth(AIndex : Integer; AValue : string); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPretargetingConfigexcludedContentLabels
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPretargetingConfigexcludedGeoCriteriaIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPretargetingConfigexcludedPlacements
  --------------------------------------------------------------------}


Procedure TPretargetingConfigexcludedPlacements.Settoken(AIndex : Integer; AValue : string); 

begin
  If (Ftoken=AValue) then exit;
  Ftoken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfigexcludedPlacements.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPretargetingConfigexcludedPlacements.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPretargetingConfigexcludedUserLists
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPretargetingConfigexcludedVerticals
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPretargetingConfiggeoCriteriaIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPretargetingConfiglanguages
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPretargetingConfigmobileCarriers
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPretargetingConfigmobileDevices
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPretargetingConfigmobileOperatingSystemVersions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPretargetingConfigplacements
  --------------------------------------------------------------------}


Procedure TPretargetingConfigplacements.Settoken(AIndex : Integer; AValue : string); 

begin
  If (Ftoken=AValue) then exit;
  Ftoken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfigplacements.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPretargetingConfigplacements.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPretargetingConfigplatforms
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPretargetingConfigsupportedCreativeAttributes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPretargetingConfiguserLists
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPretargetingConfigvendorTypes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPretargetingConfigverticals
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPretargetingConfigList
  --------------------------------------------------------------------}


Procedure TPretargetingConfigList.Setitems(AIndex : Integer; AValue : TPretargetingConfigListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfigList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPretargetingConfigListitems
  --------------------------------------------------------------------}




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
  TAccount.RegisterObject;
  TAccountbidderLocation.RegisterObject;
  TAccountsList.RegisterObject;
  TAccountsListitems.RegisterObject;
  TBillingInfo.RegisterObject;
  TBillingInfobillingId.RegisterObject;
  TBillingInfoList.RegisterObject;
  TBillingInfoListitems.RegisterObject;
  TBudget.RegisterObject;
  TCreative.RegisterObject;
  TCreativeadvertiserId.RegisterObject;
  TCreativeattribute.RegisterObject;
  TCreativeclickThroughUrl.RegisterObject;
  TCreativecorrections.RegisterObject;
  TCreativecorrectionsdetails.RegisterObject;
  TCreativedisapprovalReasons.RegisterObject;
  TCreativedisapprovalReasonsdetails.RegisterObject;
  TCreativefilteringReasons.RegisterObject;
  TCreativefilteringReasonsreasons.RegisterObject;
  TCreativeproductCategories.RegisterObject;
  TCreativerestrictedCategories.RegisterObject;
  TCreativesensitiveCategories.RegisterObject;
  TCreativevendorType.RegisterObject;
  TCreativesList.RegisterObject;
  TCreativesListitems.RegisterObject;
  TDirectDeal.RegisterObject;
  TDirectDealsList.RegisterObject;
  TDirectDealsListdirectDeals.RegisterObject;
  TPerformanceReport.RegisterObject;
  TPerformanceReportcalloutStatusRate.RegisterObject;
  TPerformanceReportcookieMatcherStatusRate.RegisterObject;
  TPerformanceReportcreativeStatusRate.RegisterObject;
  TPerformanceReporthostedMatchStatusRate.RegisterObject;
  TPerformanceReportList.RegisterObject;
  TPerformanceReportListperformanceReport.RegisterObject;
  TPretargetingConfig.RegisterObject;
  TPretargetingConfigcreativeType.RegisterObject;
  TPretargetingConfigdimensions.RegisterObject;
  TPretargetingConfigexcludedContentLabels.RegisterObject;
  TPretargetingConfigexcludedGeoCriteriaIds.RegisterObject;
  TPretargetingConfigexcludedPlacements.RegisterObject;
  TPretargetingConfigexcludedUserLists.RegisterObject;
  TPretargetingConfigexcludedVerticals.RegisterObject;
  TPretargetingConfiggeoCriteriaIds.RegisterObject;
  TPretargetingConfiglanguages.RegisterObject;
  TPretargetingConfigmobileCarriers.RegisterObject;
  TPretargetingConfigmobileDevices.RegisterObject;
  TPretargetingConfigmobileOperatingSystemVersions.RegisterObject;
  TPretargetingConfigplacements.RegisterObject;
  TPretargetingConfigplatforms.RegisterObject;
  TPretargetingConfigsupportedCreativeAttributes.RegisterObject;
  TPretargetingConfiguserLists.RegisterObject;
  TPretargetingConfigvendorTypes.RegisterObject;
  TPretargetingConfigverticals.RegisterObject;
  TPretargetingConfigList.RegisterObject;
  TPretargetingConfigListitems.RegisterObject;
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
