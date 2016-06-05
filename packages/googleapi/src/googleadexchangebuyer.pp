unit googleadexchangebuyer;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAccount = Class;
  TAccountsList = Class;
  TAddOrderDealsRequest = Class;
  TAddOrderDealsResponse = Class;
  TAddOrderNotesRequest = Class;
  TAddOrderNotesResponse = Class;
  TBillingInfo = Class;
  TBillingInfoList = Class;
  TBudget = Class;
  TBuyer = Class;
  TContactInformation = Class;
  TCreateOrdersRequest = Class;
  TCreateOrdersResponse = Class;
  TCreative = Class;
  TCreativesList = Class;
  TDealServingMetadata = Class;
  TDealServingMetadataDealPauseStatus = Class;
  TDealTerms = Class;
  TDealTermsGuaranteedFixedPriceTerms = Class;
  TDealTermsGuaranteedFixedPriceTermsBillingInfo = Class;
  TDealTermsNonGuaranteedAuctionTerms = Class;
  TDealTermsNonGuaranteedFixedPriceTerms = Class;
  TDeleteOrderDealsRequest = Class;
  TDeleteOrderDealsResponse = Class;
  TDeliveryControl = Class;
  TDeliveryControlFrequencyCap = Class;
  TDimension = Class;
  TDimensionDimensionValue = Class;
  TEditAllOrderDealsRequest = Class;
  TEditAllOrderDealsResponse = Class;
  TGetOffersResponse = Class;
  TGetOrderDealsResponse = Class;
  TGetOrderNotesResponse = Class;
  TGetOrdersResponse = Class;
  TGetPublisherProfilesByAccountIdResponse = Class;
  TMarketplaceDeal = Class;
  TMarketplaceDealParty = Class;
  TMarketplaceLabel = Class;
  TMarketplaceNote = Class;
  TPerformanceReport = Class;
  TPerformanceReportList = Class;
  TPretargetingConfig = Class;
  TPretargetingConfigList = Class;
  TPrice = Class;
  TPricePerBuyer = Class;
  TPrivateData = Class;
  TProduct = Class;
  TProposal = Class;
  TPublisherProfileApiProto = Class;
  TPublisherProvidedForecast = Class;
  TSeller = Class;
  TSharedTargeting = Class;
  TTargetingValue = Class;
  TTargetingValueCreativeSize = Class;
  TTargetingValueDayPartTargeting = Class;
  TTargetingValueDayPartTargetingDayPart = Class;
  TTargetingValueSize = Class;
  TUpdatePrivateAuctionProposalRequest = Class;
  TAccountArray = Array of TAccount;
  TAccountsListArray = Array of TAccountsList;
  TAddOrderDealsRequestArray = Array of TAddOrderDealsRequest;
  TAddOrderDealsResponseArray = Array of TAddOrderDealsResponse;
  TAddOrderNotesRequestArray = Array of TAddOrderNotesRequest;
  TAddOrderNotesResponseArray = Array of TAddOrderNotesResponse;
  TBillingInfoArray = Array of TBillingInfo;
  TBillingInfoListArray = Array of TBillingInfoList;
  TBudgetArray = Array of TBudget;
  TBuyerArray = Array of TBuyer;
  TContactInformationArray = Array of TContactInformation;
  TCreateOrdersRequestArray = Array of TCreateOrdersRequest;
  TCreateOrdersResponseArray = Array of TCreateOrdersResponse;
  TCreativeArray = Array of TCreative;
  TCreativesListArray = Array of TCreativesList;
  TDealServingMetadataArray = Array of TDealServingMetadata;
  TDealServingMetadataDealPauseStatusArray = Array of TDealServingMetadataDealPauseStatus;
  TDealTermsArray = Array of TDealTerms;
  TDealTermsGuaranteedFixedPriceTermsArray = Array of TDealTermsGuaranteedFixedPriceTerms;
  TDealTermsGuaranteedFixedPriceTermsBillingInfoArray = Array of TDealTermsGuaranteedFixedPriceTermsBillingInfo;
  TDealTermsNonGuaranteedAuctionTermsArray = Array of TDealTermsNonGuaranteedAuctionTerms;
  TDealTermsNonGuaranteedFixedPriceTermsArray = Array of TDealTermsNonGuaranteedFixedPriceTerms;
  TDeleteOrderDealsRequestArray = Array of TDeleteOrderDealsRequest;
  TDeleteOrderDealsResponseArray = Array of TDeleteOrderDealsResponse;
  TDeliveryControlArray = Array of TDeliveryControl;
  TDeliveryControlFrequencyCapArray = Array of TDeliveryControlFrequencyCap;
  TDimensionArray = Array of TDimension;
  TDimensionDimensionValueArray = Array of TDimensionDimensionValue;
  TEditAllOrderDealsRequestArray = Array of TEditAllOrderDealsRequest;
  TEditAllOrderDealsResponseArray = Array of TEditAllOrderDealsResponse;
  TGetOffersResponseArray = Array of TGetOffersResponse;
  TGetOrderDealsResponseArray = Array of TGetOrderDealsResponse;
  TGetOrderNotesResponseArray = Array of TGetOrderNotesResponse;
  TGetOrdersResponseArray = Array of TGetOrdersResponse;
  TGetPublisherProfilesByAccountIdResponseArray = Array of TGetPublisherProfilesByAccountIdResponse;
  TMarketplaceDealArray = Array of TMarketplaceDeal;
  TMarketplaceDealPartyArray = Array of TMarketplaceDealParty;
  TMarketplaceLabelArray = Array of TMarketplaceLabel;
  TMarketplaceNoteArray = Array of TMarketplaceNote;
  TPerformanceReportArray = Array of TPerformanceReport;
  TPerformanceReportListArray = Array of TPerformanceReportList;
  TPretargetingConfigArray = Array of TPretargetingConfig;
  TPretargetingConfigListArray = Array of TPretargetingConfigList;
  TPriceArray = Array of TPrice;
  TPricePerBuyerArray = Array of TPricePerBuyer;
  TPrivateDataArray = Array of TPrivateData;
  TProductArray = Array of TProduct;
  TProposalArray = Array of TProposal;
  TPublisherProfileApiProtoArray = Array of TPublisherProfileApiProto;
  TPublisherProvidedForecastArray = Array of TPublisherProvidedForecast;
  TSellerArray = Array of TSeller;
  TSharedTargetingArray = Array of TSharedTargeting;
  TTargetingValueArray = Array of TTargetingValue;
  TTargetingValueCreativeSizeArray = Array of TTargetingValueCreativeSize;
  TTargetingValueDayPartTargetingArray = Array of TTargetingValueDayPartTargeting;
  TTargetingValueDayPartTargetingDayPartArray = Array of TTargetingValueDayPartTargetingDayPart;
  TTargetingValueSizeArray = Array of TTargetingValueSize;
  TUpdatePrivateAuctionProposalRequestArray = Array of TUpdatePrivateAuctionProposalRequest;
  //Anonymous types, using auto-generated names
  TAccountTypebidderLocationItem = Class;
  TCreativeTypecorrectionsItem = Class;
  TCreativeTypefilteringReasonsTypereasonsItem = Class;
  TCreativeTypefilteringReasons = Class;
  TCreativeTypenativeAdTypeappIcon = Class;
  TCreativeTypenativeAdTypeimage = Class;
  TCreativeTypenativeAdTypelogo = Class;
  TCreativeTypenativeAd = Class;
  TCreativeTypeservingRestrictionsItemTypecontextsItem = Class;
  TCreativeTypeservingRestrictionsItemTypedisapprovalReasonsItem = Class;
  TCreativeTypeservingRestrictionsItem = Class;
  TPretargetingConfigTypedimensionsItem = Class;
  TPretargetingConfigTypeexcludedPlacementsItem = Class;
  TPretargetingConfigTypeplacementsItem = Class;
  TPretargetingConfigTypevideoPlayerSizesItem = Class;
  TAccountTypebidderLocationArray = Array of TAccountTypebidderLocationItem;
  TAccountsListTypeitemsArray = Array of TAccount;
  TAddOrderDealsRequestTypedealsArray = Array of TMarketplaceDeal;
  TAddOrderDealsResponseTypedealsArray = Array of TMarketplaceDeal;
  TAddOrderNotesRequestTypenotesArray = Array of TMarketplaceNote;
  TAddOrderNotesResponseTypenotesArray = Array of TMarketplaceNote;
  TBillingInfoListTypeitemsArray = Array of TBillingInfo;
  TCreateOrdersRequestTypeproposalsArray = Array of TProposal;
  TCreateOrdersResponseTypeproposalsArray = Array of TProposal;
  TCreativeTypecorrectionsArray = Array of TCreativeTypecorrectionsItem;
  TCreativeTypefilteringReasonsTypereasonsArray = Array of TCreativeTypefilteringReasonsTypereasonsItem;
  TCreativeTypeservingRestrictionsItemTypecontextsArray = Array of TCreativeTypeservingRestrictionsItemTypecontextsItem;
  TCreativeTypeservingRestrictionsItemTypedisapprovalReasonsArray = Array of TCreativeTypeservingRestrictionsItemTypedisapprovalReasonsItem;
  TCreativeTypeservingRestrictionsArray = Array of TCreativeTypeservingRestrictionsItem;
  TCreativesListTypeitemsArray = Array of TCreative;
  TDealTermsGuaranteedFixedPriceTermsTypefixedPricesArray = Array of TPricePerBuyer;
  TDealTermsNonGuaranteedAuctionTermsTypereservePricePerBuyersArray = Array of TPricePerBuyer;
  TDealTermsNonGuaranteedFixedPriceTermsTypefixedPricesArray = Array of TPricePerBuyer;
  TDeleteOrderDealsResponseTypedealsArray = Array of TMarketplaceDeal;
  TDeliveryControlTypefrequencyCapsArray = Array of TDeliveryControlFrequencyCap;
  TDimensionTypedimensionValuesArray = Array of TDimensionDimensionValue;
  TEditAllOrderDealsRequestTypedealsArray = Array of TMarketplaceDeal;
  TEditAllOrderDealsResponseTypedealsArray = Array of TMarketplaceDeal;
  TGetOffersResponseTypeproductsArray = Array of TProduct;
  TGetOrderDealsResponseTypedealsArray = Array of TMarketplaceDeal;
  TGetOrderNotesResponseTypenotesArray = Array of TMarketplaceNote;
  TGetOrdersResponseTypeproposalsArray = Array of TProposal;
  TGetPublisherProfilesByAccountIdResponseTypeprofilesArray = Array of TPublisherProfileApiProto;
  TMarketplaceDealTypesellerContactsArray = Array of TContactInformation;
  TMarketplaceDealTypesharedTargetingsArray = Array of TSharedTargeting;
  TPerformanceReportListTypeperformanceReportArray = Array of TPerformanceReport;
  TPretargetingConfigTypedimensionsArray = Array of TPretargetingConfigTypedimensionsItem;
  TPretargetingConfigTypeexcludedPlacementsArray = Array of TPretargetingConfigTypeexcludedPlacementsItem;
  TPretargetingConfigTypeplacementsArray = Array of TPretargetingConfigTypeplacementsItem;
  TPretargetingConfigTypevideoPlayerSizesArray = Array of TPretargetingConfigTypevideoPlayerSizesItem;
  TPretargetingConfigListTypeitemsArray = Array of TPretargetingConfig;
  TProductTypecreatorContactsArray = Array of TContactInformation;
  TProductTypelabelsArray = Array of TMarketplaceLabel;
  TProductTypesharedTargetingsArray = Array of TSharedTargeting;
  TProposalTypebuyerContactsArray = Array of TContactInformation;
  TProposalTypelabelsArray = Array of TMarketplaceLabel;
  TProposalTypesellerContactsArray = Array of TContactInformation;
  TPublisherProvidedForecastTypedimensionsArray = Array of TDimension;
  TSharedTargetingTypeexclusionsArray = Array of TTargetingValue;
  TSharedTargetingTypeinclusionsArray = Array of TTargetingValue;
  TTargetingValueCreativeSizeTypecompanionSizesArray = Array of TTargetingValueSize;
  TTargetingValueDayPartTargetingTypedayPartsArray = Array of TTargetingValueDayPartTargetingDayPart;
  
  { --------------------------------------------------------------------
    TAccountTypebidderLocationItem
    --------------------------------------------------------------------}
  
  TAccountTypebidderLocationItem = Class(TGoogleBaseObject)
  Private
    FbidProtocol : String;
    FmaximumQps : integer;
    Fregion : String;
    Furl : String;
  Protected
    //Property setters
    Procedure SetbidProtocol(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmaximumQps(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setregion(AIndex : Integer; const AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property bidProtocol : String Index 0 Read FbidProtocol Write SetbidProtocol;
    Property maximumQps : integer Index 8 Read FmaximumQps Write SetmaximumQps;
    Property region : String Index 16 Read Fregion Write Setregion;
    Property url : String Index 24 Read Furl Write Seturl;
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
    Procedure SetbidderLocation(AIndex : Integer; const AValue : TAccountTypebidderLocationArray); virtual;
    Procedure SetcookieMatchingNid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcookieMatchingUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmaximumActiveCreatives(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetmaximumTotalQps(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetnumberActiveCreatives(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
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
    Procedure Setitems(AIndex : Integer; const AValue : TAccountsListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TAccountsListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TAccountsListClass = Class of TAccountsList;
  
  { --------------------------------------------------------------------
    TAddOrderDealsRequest
    --------------------------------------------------------------------}
  
  TAddOrderDealsRequest = Class(TGoogleBaseObject)
  Private
    Fdeals : TAddOrderDealsRequestTypedealsArray;
    FproposalRevisionNumber : String;
    FupdateAction : String;
  Protected
    //Property setters
    Procedure Setdeals(AIndex : Integer; const AValue : TAddOrderDealsRequestTypedealsArray); virtual;
    Procedure SetproposalRevisionNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure SetupdateAction(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property deals : TAddOrderDealsRequestTypedealsArray Index 0 Read Fdeals Write Setdeals;
    Property proposalRevisionNumber : String Index 8 Read FproposalRevisionNumber Write SetproposalRevisionNumber;
    Property updateAction : String Index 16 Read FupdateAction Write SetupdateAction;
  end;
  TAddOrderDealsRequestClass = Class of TAddOrderDealsRequest;
  
  { --------------------------------------------------------------------
    TAddOrderDealsResponse
    --------------------------------------------------------------------}
  
  TAddOrderDealsResponse = Class(TGoogleBaseObject)
  Private
    Fdeals : TAddOrderDealsResponseTypedealsArray;
    FproposalRevisionNumber : String;
  Protected
    //Property setters
    Procedure Setdeals(AIndex : Integer; const AValue : TAddOrderDealsResponseTypedealsArray); virtual;
    Procedure SetproposalRevisionNumber(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property deals : TAddOrderDealsResponseTypedealsArray Index 0 Read Fdeals Write Setdeals;
    Property proposalRevisionNumber : String Index 8 Read FproposalRevisionNumber Write SetproposalRevisionNumber;
  end;
  TAddOrderDealsResponseClass = Class of TAddOrderDealsResponse;
  
  { --------------------------------------------------------------------
    TAddOrderNotesRequest
    --------------------------------------------------------------------}
  
  TAddOrderNotesRequest = Class(TGoogleBaseObject)
  Private
    Fnotes : TAddOrderNotesRequestTypenotesArray;
  Protected
    //Property setters
    Procedure Setnotes(AIndex : Integer; const AValue : TAddOrderNotesRequestTypenotesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property notes : TAddOrderNotesRequestTypenotesArray Index 0 Read Fnotes Write Setnotes;
  end;
  TAddOrderNotesRequestClass = Class of TAddOrderNotesRequest;
  
  { --------------------------------------------------------------------
    TAddOrderNotesResponse
    --------------------------------------------------------------------}
  
  TAddOrderNotesResponse = Class(TGoogleBaseObject)
  Private
    Fnotes : TAddOrderNotesResponseTypenotesArray;
  Protected
    //Property setters
    Procedure Setnotes(AIndex : Integer; const AValue : TAddOrderNotesResponseTypenotesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property notes : TAddOrderNotesResponseTypenotesArray Index 0 Read Fnotes Write Setnotes;
  end;
  TAddOrderNotesResponseClass = Class of TAddOrderNotesResponse;
  
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
    Procedure SetaccountId(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetaccountName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetbillingId(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
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
    Procedure Setitems(AIndex : Integer; const AValue : TBillingInfoListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
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
    Procedure SetaccountId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetbillingId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetbudgetAmount(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcurrencyCode(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
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
    TBuyer
    --------------------------------------------------------------------}
  
  TBuyer = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
  end;
  TBuyerClass = Class of TBuyer;
  
  { --------------------------------------------------------------------
    TContactInformation
    --------------------------------------------------------------------}
  
  TContactInformation = Class(TGoogleBaseObject)
  Private
    Femail : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setemail(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property email : String Index 0 Read Femail Write Setemail;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TContactInformationClass = Class of TContactInformation;
  
  { --------------------------------------------------------------------
    TCreateOrdersRequest
    --------------------------------------------------------------------}
  
  TCreateOrdersRequest = Class(TGoogleBaseObject)
  Private
    Fproposals : TCreateOrdersRequestTypeproposalsArray;
    FwebPropertyCode : String;
  Protected
    //Property setters
    Procedure Setproposals(AIndex : Integer; const AValue : TCreateOrdersRequestTypeproposalsArray); virtual;
    Procedure SetwebPropertyCode(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property proposals : TCreateOrdersRequestTypeproposalsArray Index 0 Read Fproposals Write Setproposals;
    Property webPropertyCode : String Index 8 Read FwebPropertyCode Write SetwebPropertyCode;
  end;
  TCreateOrdersRequestClass = Class of TCreateOrdersRequest;
  
  { --------------------------------------------------------------------
    TCreateOrdersResponse
    --------------------------------------------------------------------}
  
  TCreateOrdersResponse = Class(TGoogleBaseObject)
  Private
    Fproposals : TCreateOrdersResponseTypeproposalsArray;
  Protected
    //Property setters
    Procedure Setproposals(AIndex : Integer; const AValue : TCreateOrdersResponseTypeproposalsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property proposals : TCreateOrdersResponseTypeproposalsArray Index 0 Read Fproposals Write Setproposals;
  end;
  TCreateOrdersResponseClass = Class of TCreateOrdersResponse;
  
  { --------------------------------------------------------------------
    TCreativeTypecorrectionsItem
    --------------------------------------------------------------------}
  
  TCreativeTypecorrectionsItem = Class(TGoogleBaseObject)
  Private
    Fdetails : TStringArray;
    Freason : String;
  Protected
    //Property setters
    Procedure Setdetails(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setreason(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property details : TStringArray Index 0 Read Fdetails Write Setdetails;
    Property reason : String Index 8 Read Freason Write Setreason;
  end;
  TCreativeTypecorrectionsItemClass = Class of TCreativeTypecorrectionsItem;
  
  { --------------------------------------------------------------------
    TCreativeTypefilteringReasonsTypereasonsItem
    --------------------------------------------------------------------}
  
  TCreativeTypefilteringReasonsTypereasonsItem = Class(TGoogleBaseObject)
  Private
    FfilteringCount : String;
    FfilteringStatus : integer;
  Protected
    //Property setters
    Procedure SetfilteringCount(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfilteringStatus(AIndex : Integer; const AValue : integer); virtual;
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
    Procedure Setdate(AIndex : Integer; const AValue : String); virtual;
    Procedure Setreasons(AIndex : Integer; const AValue : TCreativeTypefilteringReasonsTypereasonsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property date : String Index 0 Read Fdate Write Setdate;
    Property reasons : TCreativeTypefilteringReasonsTypereasonsArray Index 8 Read Freasons Write Setreasons;
  end;
  TCreativeTypefilteringReasonsClass = Class of TCreativeTypefilteringReasons;
  
  { --------------------------------------------------------------------
    TCreativeTypenativeAdTypeappIcon
    --------------------------------------------------------------------}
  
  TCreativeTypenativeAdTypeappIcon = Class(TGoogleBaseObject)
  Private
    Fheight : integer;
    Furl : String;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure Setheight(AIndex : Integer; const AValue : integer); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property height : integer Index 0 Read Fheight Write Setheight;
    Property url : String Index 8 Read Furl Write Seturl;
    Property width : integer Index 16 Read Fwidth Write Setwidth;
  end;
  TCreativeTypenativeAdTypeappIconClass = Class of TCreativeTypenativeAdTypeappIcon;
  
  { --------------------------------------------------------------------
    TCreativeTypenativeAdTypeimage
    --------------------------------------------------------------------}
  
  TCreativeTypenativeAdTypeimage = Class(TGoogleBaseObject)
  Private
    Fheight : integer;
    Furl : String;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure Setheight(AIndex : Integer; const AValue : integer); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property height : integer Index 0 Read Fheight Write Setheight;
    Property url : String Index 8 Read Furl Write Seturl;
    Property width : integer Index 16 Read Fwidth Write Setwidth;
  end;
  TCreativeTypenativeAdTypeimageClass = Class of TCreativeTypenativeAdTypeimage;
  
  { --------------------------------------------------------------------
    TCreativeTypenativeAdTypelogo
    --------------------------------------------------------------------}
  
  TCreativeTypenativeAdTypelogo = Class(TGoogleBaseObject)
  Private
    Fheight : integer;
    Furl : String;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure Setheight(AIndex : Integer; const AValue : integer); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property height : integer Index 0 Read Fheight Write Setheight;
    Property url : String Index 8 Read Furl Write Seturl;
    Property width : integer Index 16 Read Fwidth Write Setwidth;
  end;
  TCreativeTypenativeAdTypelogoClass = Class of TCreativeTypenativeAdTypelogo;
  
  { --------------------------------------------------------------------
    TCreativeTypenativeAd
    --------------------------------------------------------------------}
  
  TCreativeTypenativeAd = Class(TGoogleBaseObject)
  Private
    Fadvertiser : String;
    FappIcon : TCreativeTypenativeAdTypeappIcon;
    Fbody : String;
    FcallToAction : String;
    FclickTrackingUrl : String;
    Fheadline : String;
    Fimage : TCreativeTypenativeAdTypeimage;
    FimpressionTrackingUrl : TStringArray;
    Flogo : TCreativeTypenativeAdTypelogo;
    Fprice : String;
    FstarRating : double;
    Fstore : String;
  Protected
    //Property setters
    Procedure Setadvertiser(AIndex : Integer; const AValue : String); virtual;
    Procedure SetappIcon(AIndex : Integer; const AValue : TCreativeTypenativeAdTypeappIcon); virtual;
    Procedure Setbody(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcallToAction(AIndex : Integer; const AValue : String); virtual;
    Procedure SetclickTrackingUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setheadline(AIndex : Integer; const AValue : String); virtual;
    Procedure Setimage(AIndex : Integer; const AValue : TCreativeTypenativeAdTypeimage); virtual;
    Procedure SetimpressionTrackingUrl(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setlogo(AIndex : Integer; const AValue : TCreativeTypenativeAdTypelogo); virtual;
    Procedure Setprice(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstarRating(AIndex : Integer; const AValue : double); virtual;
    Procedure Setstore(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property advertiser : String Index 0 Read Fadvertiser Write Setadvertiser;
    Property appIcon : TCreativeTypenativeAdTypeappIcon Index 8 Read FappIcon Write SetappIcon;
    Property body : String Index 16 Read Fbody Write Setbody;
    Property callToAction : String Index 24 Read FcallToAction Write SetcallToAction;
    Property clickTrackingUrl : String Index 32 Read FclickTrackingUrl Write SetclickTrackingUrl;
    Property headline : String Index 40 Read Fheadline Write Setheadline;
    Property image : TCreativeTypenativeAdTypeimage Index 48 Read Fimage Write Setimage;
    Property impressionTrackingUrl : TStringArray Index 56 Read FimpressionTrackingUrl Write SetimpressionTrackingUrl;
    Property logo : TCreativeTypenativeAdTypelogo Index 64 Read Flogo Write Setlogo;
    Property price : String Index 72 Read Fprice Write Setprice;
    Property starRating : double Index 80 Read FstarRating Write SetstarRating;
    Property store : String Index 88 Read Fstore Write Setstore;
  end;
  TCreativeTypenativeAdClass = Class of TCreativeTypenativeAd;
  
  { --------------------------------------------------------------------
    TCreativeTypeservingRestrictionsItemTypecontextsItem
    --------------------------------------------------------------------}
  
  TCreativeTypeservingRestrictionsItemTypecontextsItem = Class(TGoogleBaseObject)
  Private
    FauctionType : TStringArray;
    FcontextType : String;
    FgeoCriteriaId : TintegerArray;
    Fplatform : TStringArray;
  Protected
    //Property setters
    Procedure SetauctionType(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetcontextType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetgeoCriteriaId(AIndex : Integer; const AValue : TintegerArray); virtual;
    Procedure Setplatform(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property auctionType : TStringArray Index 0 Read FauctionType Write SetauctionType;
    Property contextType : String Index 8 Read FcontextType Write SetcontextType;
    Property geoCriteriaId : TintegerArray Index 16 Read FgeoCriteriaId Write SetgeoCriteriaId;
    Property platform : TStringArray Index 24 Read Fplatform Write Setplatform;
  end;
  TCreativeTypeservingRestrictionsItemTypecontextsItemClass = Class of TCreativeTypeservingRestrictionsItemTypecontextsItem;
  
  { --------------------------------------------------------------------
    TCreativeTypeservingRestrictionsItemTypedisapprovalReasonsItem
    --------------------------------------------------------------------}
  
  TCreativeTypeservingRestrictionsItemTypedisapprovalReasonsItem = Class(TGoogleBaseObject)
  Private
    Fdetails : TStringArray;
    Freason : String;
  Protected
    //Property setters
    Procedure Setdetails(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setreason(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property details : TStringArray Index 0 Read Fdetails Write Setdetails;
    Property reason : String Index 8 Read Freason Write Setreason;
  end;
  TCreativeTypeservingRestrictionsItemTypedisapprovalReasonsItemClass = Class of TCreativeTypeservingRestrictionsItemTypedisapprovalReasonsItem;
  
  { --------------------------------------------------------------------
    TCreativeTypeservingRestrictionsItem
    --------------------------------------------------------------------}
  
  TCreativeTypeservingRestrictionsItem = Class(TGoogleBaseObject)
  Private
    Fcontexts : TCreativeTypeservingRestrictionsItemTypecontextsArray;
    FdisapprovalReasons : TCreativeTypeservingRestrictionsItemTypedisapprovalReasonsArray;
    Freason : String;
  Protected
    //Property setters
    Procedure Setcontexts(AIndex : Integer; const AValue : TCreativeTypeservingRestrictionsItemTypecontextsArray); virtual;
    Procedure SetdisapprovalReasons(AIndex : Integer; const AValue : TCreativeTypeservingRestrictionsItemTypedisapprovalReasonsArray); virtual;
    Procedure Setreason(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property contexts : TCreativeTypeservingRestrictionsItemTypecontextsArray Index 0 Read Fcontexts Write Setcontexts;
    Property disapprovalReasons : TCreativeTypeservingRestrictionsItemTypedisapprovalReasonsArray Index 8 Read FdisapprovalReasons Write SetdisapprovalReasons;
    Property reason : String Index 16 Read Freason Write Setreason;
  end;
  TCreativeTypeservingRestrictionsItemClass = Class of TCreativeTypeservingRestrictionsItem;
  
  { --------------------------------------------------------------------
    TCreative
    --------------------------------------------------------------------}
  
  TCreative = Class(TGoogleBaseObject)
  Private
    FHTMLSnippet : String;
    FaccountId : integer;
    FadChoicesDestinationUrl : String;
    FadvertiserId : TStringArray;
    FadvertiserName : String;
    FagencyId : String;
    FapiUploadTimestamp : TDatetime;
    Fattribute : TintegerArray;
    FbuyerCreativeId : String;
    FclickThroughUrl : TStringArray;
    Fcorrections : TCreativeTypecorrectionsArray;
    FdealsStatus : String;
    FfilteringReasons : TCreativeTypefilteringReasons;
    Fheight : integer;
    FimpressionTrackingUrl : TStringArray;
    Fkind : String;
    FnativeAd : TCreativeTypenativeAd;
    FopenAuctionStatus : String;
    FproductCategories : TintegerArray;
    FrestrictedCategories : TintegerArray;
    FsensitiveCategories : TintegerArray;
    FservingRestrictions : TCreativeTypeservingRestrictionsArray;
    FvendorType : TintegerArray;
    Fversion : integer;
    FvideoURL : String;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure SetHTMLSnippet(AIndex : Integer; const AValue : String); virtual;
    Procedure SetaccountId(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetadChoicesDestinationUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetadvertiserId(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetadvertiserName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetagencyId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetapiUploadTimestamp(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure Setattribute(AIndex : Integer; const AValue : TintegerArray); virtual;
    Procedure SetbuyerCreativeId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetclickThroughUrl(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setcorrections(AIndex : Integer; const AValue : TCreativeTypecorrectionsArray); virtual;
    Procedure SetdealsStatus(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfilteringReasons(AIndex : Integer; const AValue : TCreativeTypefilteringReasons); virtual;
    Procedure Setheight(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetimpressionTrackingUrl(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnativeAd(AIndex : Integer; const AValue : TCreativeTypenativeAd); virtual;
    Procedure SetopenAuctionStatus(AIndex : Integer; const AValue : String); virtual;
    Procedure SetproductCategories(AIndex : Integer; const AValue : TintegerArray); virtual;
    Procedure SetrestrictedCategories(AIndex : Integer; const AValue : TintegerArray); virtual;
    Procedure SetsensitiveCategories(AIndex : Integer; const AValue : TintegerArray); virtual;
    Procedure SetservingRestrictions(AIndex : Integer; const AValue : TCreativeTypeservingRestrictionsArray); virtual;
    Procedure SetvendorType(AIndex : Integer; const AValue : TintegerArray); virtual;
    Procedure Setversion(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetvideoURL(AIndex : Integer; const AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property HTMLSnippet : String Index 0 Read FHTMLSnippet Write SetHTMLSnippet;
    Property accountId : integer Index 8 Read FaccountId Write SetaccountId;
    Property adChoicesDestinationUrl : String Index 16 Read FadChoicesDestinationUrl Write SetadChoicesDestinationUrl;
    Property advertiserId : TStringArray Index 24 Read FadvertiserId Write SetadvertiserId;
    Property advertiserName : String Index 32 Read FadvertiserName Write SetadvertiserName;
    Property agencyId : String Index 40 Read FagencyId Write SetagencyId;
    Property apiUploadTimestamp : TDatetime Index 48 Read FapiUploadTimestamp Write SetapiUploadTimestamp;
    Property attribute : TintegerArray Index 56 Read Fattribute Write Setattribute;
    Property buyerCreativeId : String Index 64 Read FbuyerCreativeId Write SetbuyerCreativeId;
    Property clickThroughUrl : TStringArray Index 72 Read FclickThroughUrl Write SetclickThroughUrl;
    Property corrections : TCreativeTypecorrectionsArray Index 80 Read Fcorrections Write Setcorrections;
    Property dealsStatus : String Index 88 Read FdealsStatus Write SetdealsStatus;
    Property filteringReasons : TCreativeTypefilteringReasons Index 96 Read FfilteringReasons Write SetfilteringReasons;
    Property height : integer Index 104 Read Fheight Write Setheight;
    Property impressionTrackingUrl : TStringArray Index 112 Read FimpressionTrackingUrl Write SetimpressionTrackingUrl;
    Property kind : String Index 120 Read Fkind Write Setkind;
    Property nativeAd : TCreativeTypenativeAd Index 128 Read FnativeAd Write SetnativeAd;
    Property openAuctionStatus : String Index 136 Read FopenAuctionStatus Write SetopenAuctionStatus;
    Property productCategories : TintegerArray Index 144 Read FproductCategories Write SetproductCategories;
    Property restrictedCategories : TintegerArray Index 152 Read FrestrictedCategories Write SetrestrictedCategories;
    Property sensitiveCategories : TintegerArray Index 160 Read FsensitiveCategories Write SetsensitiveCategories;
    Property servingRestrictions : TCreativeTypeservingRestrictionsArray Index 168 Read FservingRestrictions Write SetservingRestrictions;
    Property vendorType : TintegerArray Index 176 Read FvendorType Write SetvendorType;
    Property version : integer Index 184 Read Fversion Write Setversion;
    Property videoURL : String Index 192 Read FvideoURL Write SetvideoURL;
    Property width : integer Index 200 Read Fwidth Write Setwidth;
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
    Procedure Setitems(AIndex : Integer; const AValue : TCreativesListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TCreativesListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TCreativesListClass = Class of TCreativesList;
  
  { --------------------------------------------------------------------
    TDealServingMetadata
    --------------------------------------------------------------------}
  
  TDealServingMetadata = Class(TGoogleBaseObject)
  Private
    FdealPauseStatus : TDealServingMetadataDealPauseStatus;
  Protected
    //Property setters
    Procedure SetdealPauseStatus(AIndex : Integer; const AValue : TDealServingMetadataDealPauseStatus); virtual;
  Public
  Published
    Property dealPauseStatus : TDealServingMetadataDealPauseStatus Index 0 Read FdealPauseStatus Write SetdealPauseStatus;
  end;
  TDealServingMetadataClass = Class of TDealServingMetadata;
  
  { --------------------------------------------------------------------
    TDealServingMetadataDealPauseStatus
    --------------------------------------------------------------------}
  
  TDealServingMetadataDealPauseStatus = Class(TGoogleBaseObject)
  Private
    FfirstPausedBy : String;
    FhasBuyerPaused : boolean;
    FhasSellerPaused : boolean;
  Protected
    //Property setters
    Procedure SetfirstPausedBy(AIndex : Integer; const AValue : String); virtual;
    Procedure SethasBuyerPaused(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SethasSellerPaused(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property firstPausedBy : String Index 0 Read FfirstPausedBy Write SetfirstPausedBy;
    Property hasBuyerPaused : boolean Index 8 Read FhasBuyerPaused Write SethasBuyerPaused;
    Property hasSellerPaused : boolean Index 16 Read FhasSellerPaused Write SethasSellerPaused;
  end;
  TDealServingMetadataDealPauseStatusClass = Class of TDealServingMetadataDealPauseStatus;
  
  { --------------------------------------------------------------------
    TDealTerms
    --------------------------------------------------------------------}
  
  TDealTerms = Class(TGoogleBaseObject)
  Private
    FbrandingType : String;
    Fdescription : String;
    FestimatedGrossSpend : TPrice;
    FestimatedImpressionsPerDay : String;
    FguaranteedFixedPriceTerms : TDealTermsGuaranteedFixedPriceTerms;
    FnonGuaranteedAuctionTerms : TDealTermsNonGuaranteedAuctionTerms;
    FnonGuaranteedFixedPriceTerms : TDealTermsNonGuaranteedFixedPriceTerms;
    FsellerTimeZone : String;
  Protected
    //Property setters
    Procedure SetbrandingType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure SetestimatedGrossSpend(AIndex : Integer; const AValue : TPrice); virtual;
    Procedure SetestimatedImpressionsPerDay(AIndex : Integer; const AValue : String); virtual;
    Procedure SetguaranteedFixedPriceTerms(AIndex : Integer; const AValue : TDealTermsGuaranteedFixedPriceTerms); virtual;
    Procedure SetnonGuaranteedAuctionTerms(AIndex : Integer; const AValue : TDealTermsNonGuaranteedAuctionTerms); virtual;
    Procedure SetnonGuaranteedFixedPriceTerms(AIndex : Integer; const AValue : TDealTermsNonGuaranteedFixedPriceTerms); virtual;
    Procedure SetsellerTimeZone(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property brandingType : String Index 0 Read FbrandingType Write SetbrandingType;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property estimatedGrossSpend : TPrice Index 16 Read FestimatedGrossSpend Write SetestimatedGrossSpend;
    Property estimatedImpressionsPerDay : String Index 24 Read FestimatedImpressionsPerDay Write SetestimatedImpressionsPerDay;
    Property guaranteedFixedPriceTerms : TDealTermsGuaranteedFixedPriceTerms Index 32 Read FguaranteedFixedPriceTerms Write SetguaranteedFixedPriceTerms;
    Property nonGuaranteedAuctionTerms : TDealTermsNonGuaranteedAuctionTerms Index 40 Read FnonGuaranteedAuctionTerms Write SetnonGuaranteedAuctionTerms;
    Property nonGuaranteedFixedPriceTerms : TDealTermsNonGuaranteedFixedPriceTerms Index 48 Read FnonGuaranteedFixedPriceTerms Write SetnonGuaranteedFixedPriceTerms;
    Property sellerTimeZone : String Index 56 Read FsellerTimeZone Write SetsellerTimeZone;
  end;
  TDealTermsClass = Class of TDealTerms;
  
  { --------------------------------------------------------------------
    TDealTermsGuaranteedFixedPriceTerms
    --------------------------------------------------------------------}
  
  TDealTermsGuaranteedFixedPriceTerms = Class(TGoogleBaseObject)
  Private
    FbillingInfo : TDealTermsGuaranteedFixedPriceTermsBillingInfo;
    FfixedPrices : TDealTermsGuaranteedFixedPriceTermsTypefixedPricesArray;
    FguaranteedImpressions : String;
    FguaranteedLooks : String;
  Protected
    //Property setters
    Procedure SetbillingInfo(AIndex : Integer; const AValue : TDealTermsGuaranteedFixedPriceTermsBillingInfo); virtual;
    Procedure SetfixedPrices(AIndex : Integer; const AValue : TDealTermsGuaranteedFixedPriceTermsTypefixedPricesArray); virtual;
    Procedure SetguaranteedImpressions(AIndex : Integer; const AValue : String); virtual;
    Procedure SetguaranteedLooks(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property billingInfo : TDealTermsGuaranteedFixedPriceTermsBillingInfo Index 0 Read FbillingInfo Write SetbillingInfo;
    Property fixedPrices : TDealTermsGuaranteedFixedPriceTermsTypefixedPricesArray Index 8 Read FfixedPrices Write SetfixedPrices;
    Property guaranteedImpressions : String Index 16 Read FguaranteedImpressions Write SetguaranteedImpressions;
    Property guaranteedLooks : String Index 24 Read FguaranteedLooks Write SetguaranteedLooks;
  end;
  TDealTermsGuaranteedFixedPriceTermsClass = Class of TDealTermsGuaranteedFixedPriceTerms;
  
  { --------------------------------------------------------------------
    TDealTermsGuaranteedFixedPriceTermsBillingInfo
    --------------------------------------------------------------------}
  
  TDealTermsGuaranteedFixedPriceTermsBillingInfo = Class(TGoogleBaseObject)
  Private
    FcurrencyConversionTimeMs : String;
    ForiginalContractedQuantity : String;
    Fprice : TPrice;
  Protected
    //Property setters
    Procedure SetcurrencyConversionTimeMs(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoriginalContractedQuantity(AIndex : Integer; const AValue : String); virtual;
    Procedure Setprice(AIndex : Integer; const AValue : TPrice); virtual;
  Public
  Published
    Property currencyConversionTimeMs : String Index 0 Read FcurrencyConversionTimeMs Write SetcurrencyConversionTimeMs;
    Property originalContractedQuantity : String Index 8 Read ForiginalContractedQuantity Write SetoriginalContractedQuantity;
    Property price : TPrice Index 16 Read Fprice Write Setprice;
  end;
  TDealTermsGuaranteedFixedPriceTermsBillingInfoClass = Class of TDealTermsGuaranteedFixedPriceTermsBillingInfo;
  
  { --------------------------------------------------------------------
    TDealTermsNonGuaranteedAuctionTerms
    --------------------------------------------------------------------}
  
  TDealTermsNonGuaranteedAuctionTerms = Class(TGoogleBaseObject)
  Private
    FautoOptimizePrivateAuction : boolean;
    FreservePricePerBuyers : TDealTermsNonGuaranteedAuctionTermsTypereservePricePerBuyersArray;
  Protected
    //Property setters
    Procedure SetautoOptimizePrivateAuction(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetreservePricePerBuyers(AIndex : Integer; const AValue : TDealTermsNonGuaranteedAuctionTermsTypereservePricePerBuyersArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property autoOptimizePrivateAuction : boolean Index 0 Read FautoOptimizePrivateAuction Write SetautoOptimizePrivateAuction;
    Property reservePricePerBuyers : TDealTermsNonGuaranteedAuctionTermsTypereservePricePerBuyersArray Index 8 Read FreservePricePerBuyers Write SetreservePricePerBuyers;
  end;
  TDealTermsNonGuaranteedAuctionTermsClass = Class of TDealTermsNonGuaranteedAuctionTerms;
  
  { --------------------------------------------------------------------
    TDealTermsNonGuaranteedFixedPriceTerms
    --------------------------------------------------------------------}
  
  TDealTermsNonGuaranteedFixedPriceTerms = Class(TGoogleBaseObject)
  Private
    FfixedPrices : TDealTermsNonGuaranteedFixedPriceTermsTypefixedPricesArray;
  Protected
    //Property setters
    Procedure SetfixedPrices(AIndex : Integer; const AValue : TDealTermsNonGuaranteedFixedPriceTermsTypefixedPricesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property fixedPrices : TDealTermsNonGuaranteedFixedPriceTermsTypefixedPricesArray Index 0 Read FfixedPrices Write SetfixedPrices;
  end;
  TDealTermsNonGuaranteedFixedPriceTermsClass = Class of TDealTermsNonGuaranteedFixedPriceTerms;
  
  { --------------------------------------------------------------------
    TDeleteOrderDealsRequest
    --------------------------------------------------------------------}
  
  TDeleteOrderDealsRequest = Class(TGoogleBaseObject)
  Private
    FdealIds : TStringArray;
    FproposalRevisionNumber : String;
    FupdateAction : String;
  Protected
    //Property setters
    Procedure SetdealIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetproposalRevisionNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure SetupdateAction(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property dealIds : TStringArray Index 0 Read FdealIds Write SetdealIds;
    Property proposalRevisionNumber : String Index 8 Read FproposalRevisionNumber Write SetproposalRevisionNumber;
    Property updateAction : String Index 16 Read FupdateAction Write SetupdateAction;
  end;
  TDeleteOrderDealsRequestClass = Class of TDeleteOrderDealsRequest;
  
  { --------------------------------------------------------------------
    TDeleteOrderDealsResponse
    --------------------------------------------------------------------}
  
  TDeleteOrderDealsResponse = Class(TGoogleBaseObject)
  Private
    Fdeals : TDeleteOrderDealsResponseTypedealsArray;
    FproposalRevisionNumber : String;
  Protected
    //Property setters
    Procedure Setdeals(AIndex : Integer; const AValue : TDeleteOrderDealsResponseTypedealsArray); virtual;
    Procedure SetproposalRevisionNumber(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property deals : TDeleteOrderDealsResponseTypedealsArray Index 0 Read Fdeals Write Setdeals;
    Property proposalRevisionNumber : String Index 8 Read FproposalRevisionNumber Write SetproposalRevisionNumber;
  end;
  TDeleteOrderDealsResponseClass = Class of TDeleteOrderDealsResponse;
  
  { --------------------------------------------------------------------
    TDeliveryControl
    --------------------------------------------------------------------}
  
  TDeliveryControl = Class(TGoogleBaseObject)
  Private
    FcreativeBlockingLevel : String;
    FdeliveryRateType : String;
    FfrequencyCaps : TDeliveryControlTypefrequencyCapsArray;
  Protected
    //Property setters
    Procedure SetcreativeBlockingLevel(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdeliveryRateType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfrequencyCaps(AIndex : Integer; const AValue : TDeliveryControlTypefrequencyCapsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property creativeBlockingLevel : String Index 0 Read FcreativeBlockingLevel Write SetcreativeBlockingLevel;
    Property deliveryRateType : String Index 8 Read FdeliveryRateType Write SetdeliveryRateType;
    Property frequencyCaps : TDeliveryControlTypefrequencyCapsArray Index 16 Read FfrequencyCaps Write SetfrequencyCaps;
  end;
  TDeliveryControlClass = Class of TDeliveryControl;
  
  { --------------------------------------------------------------------
    TDeliveryControlFrequencyCap
    --------------------------------------------------------------------}
  
  TDeliveryControlFrequencyCap = Class(TGoogleBaseObject)
  Private
    FmaxImpressions : integer;
    FnumTimeUnits : integer;
    FtimeUnitType : String;
  Protected
    //Property setters
    Procedure SetmaxImpressions(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetnumTimeUnits(AIndex : Integer; const AValue : integer); virtual;
    Procedure SettimeUnitType(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property maxImpressions : integer Index 0 Read FmaxImpressions Write SetmaxImpressions;
    Property numTimeUnits : integer Index 8 Read FnumTimeUnits Write SetnumTimeUnits;
    Property timeUnitType : String Index 16 Read FtimeUnitType Write SettimeUnitType;
  end;
  TDeliveryControlFrequencyCapClass = Class of TDeliveryControlFrequencyCap;
  
  { --------------------------------------------------------------------
    TDimension
    --------------------------------------------------------------------}
  
  TDimension = Class(TGoogleBaseObject)
  Private
    FdimensionType : String;
    FdimensionValues : TDimensionTypedimensionValuesArray;
  Protected
    //Property setters
    Procedure SetdimensionType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdimensionValues(AIndex : Integer; const AValue : TDimensionTypedimensionValuesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property dimensionType : String Index 0 Read FdimensionType Write SetdimensionType;
    Property dimensionValues : TDimensionTypedimensionValuesArray Index 8 Read FdimensionValues Write SetdimensionValues;
  end;
  TDimensionClass = Class of TDimension;
  
  { --------------------------------------------------------------------
    TDimensionDimensionValue
    --------------------------------------------------------------------}
  
  TDimensionDimensionValue = Class(TGoogleBaseObject)
  Private
    Fid : integer;
    Fname : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property id : integer Index 0 Read Fid Write Setid;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TDimensionDimensionValueClass = Class of TDimensionDimensionValue;
  
  { --------------------------------------------------------------------
    TEditAllOrderDealsRequest
    --------------------------------------------------------------------}
  
  TEditAllOrderDealsRequest = Class(TGoogleBaseObject)
  Private
    Fdeals : TEditAllOrderDealsRequestTypedealsArray;
    Fproposal : TProposal;
    FproposalRevisionNumber : String;
    FupdateAction : String;
  Protected
    //Property setters
    Procedure Setdeals(AIndex : Integer; const AValue : TEditAllOrderDealsRequestTypedealsArray); virtual;
    Procedure Setproposal(AIndex : Integer; const AValue : TProposal); virtual;
    Procedure SetproposalRevisionNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure SetupdateAction(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property deals : TEditAllOrderDealsRequestTypedealsArray Index 0 Read Fdeals Write Setdeals;
    Property proposal : TProposal Index 8 Read Fproposal Write Setproposal;
    Property proposalRevisionNumber : String Index 16 Read FproposalRevisionNumber Write SetproposalRevisionNumber;
    Property updateAction : String Index 24 Read FupdateAction Write SetupdateAction;
  end;
  TEditAllOrderDealsRequestClass = Class of TEditAllOrderDealsRequest;
  
  { --------------------------------------------------------------------
    TEditAllOrderDealsResponse
    --------------------------------------------------------------------}
  
  TEditAllOrderDealsResponse = Class(TGoogleBaseObject)
  Private
    Fdeals : TEditAllOrderDealsResponseTypedealsArray;
    ForderRevisionNumber : String;
  Protected
    //Property setters
    Procedure Setdeals(AIndex : Integer; const AValue : TEditAllOrderDealsResponseTypedealsArray); virtual;
    Procedure SetorderRevisionNumber(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property deals : TEditAllOrderDealsResponseTypedealsArray Index 0 Read Fdeals Write Setdeals;
    Property orderRevisionNumber : String Index 8 Read ForderRevisionNumber Write SetorderRevisionNumber;
  end;
  TEditAllOrderDealsResponseClass = Class of TEditAllOrderDealsResponse;
  
  { --------------------------------------------------------------------
    TGetOffersResponse
    --------------------------------------------------------------------}
  
  TGetOffersResponse = Class(TGoogleBaseObject)
  Private
    Fproducts : TGetOffersResponseTypeproductsArray;
  Protected
    //Property setters
    Procedure Setproducts(AIndex : Integer; const AValue : TGetOffersResponseTypeproductsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property products : TGetOffersResponseTypeproductsArray Index 0 Read Fproducts Write Setproducts;
  end;
  TGetOffersResponseClass = Class of TGetOffersResponse;
  
  { --------------------------------------------------------------------
    TGetOrderDealsResponse
    --------------------------------------------------------------------}
  
  TGetOrderDealsResponse = Class(TGoogleBaseObject)
  Private
    Fdeals : TGetOrderDealsResponseTypedealsArray;
  Protected
    //Property setters
    Procedure Setdeals(AIndex : Integer; const AValue : TGetOrderDealsResponseTypedealsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property deals : TGetOrderDealsResponseTypedealsArray Index 0 Read Fdeals Write Setdeals;
  end;
  TGetOrderDealsResponseClass = Class of TGetOrderDealsResponse;
  
  { --------------------------------------------------------------------
    TGetOrderNotesResponse
    --------------------------------------------------------------------}
  
  TGetOrderNotesResponse = Class(TGoogleBaseObject)
  Private
    Fnotes : TGetOrderNotesResponseTypenotesArray;
  Protected
    //Property setters
    Procedure Setnotes(AIndex : Integer; const AValue : TGetOrderNotesResponseTypenotesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property notes : TGetOrderNotesResponseTypenotesArray Index 0 Read Fnotes Write Setnotes;
  end;
  TGetOrderNotesResponseClass = Class of TGetOrderNotesResponse;
  
  { --------------------------------------------------------------------
    TGetOrdersResponse
    --------------------------------------------------------------------}
  
  TGetOrdersResponse = Class(TGoogleBaseObject)
  Private
    Fproposals : TGetOrdersResponseTypeproposalsArray;
  Protected
    //Property setters
    Procedure Setproposals(AIndex : Integer; const AValue : TGetOrdersResponseTypeproposalsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property proposals : TGetOrdersResponseTypeproposalsArray Index 0 Read Fproposals Write Setproposals;
  end;
  TGetOrdersResponseClass = Class of TGetOrdersResponse;
  
  { --------------------------------------------------------------------
    TGetPublisherProfilesByAccountIdResponse
    --------------------------------------------------------------------}
  
  TGetPublisherProfilesByAccountIdResponse = Class(TGoogleBaseObject)
  Private
    Fprofiles : TGetPublisherProfilesByAccountIdResponseTypeprofilesArray;
  Protected
    //Property setters
    Procedure Setprofiles(AIndex : Integer; const AValue : TGetPublisherProfilesByAccountIdResponseTypeprofilesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property profiles : TGetPublisherProfilesByAccountIdResponseTypeprofilesArray Index 0 Read Fprofiles Write Setprofiles;
  end;
  TGetPublisherProfilesByAccountIdResponseClass = Class of TGetPublisherProfilesByAccountIdResponse;
  
  { --------------------------------------------------------------------
    TMarketplaceDeal
    --------------------------------------------------------------------}
  
  TMarketplaceDeal = Class(TGoogleBaseObject)
  Private
    FbuyerPrivateData : TPrivateData;
    FcreationTimeMs : String;
    FcreativePreApprovalPolicy : String;
    FcreativeSafeFrameCompatibility : String;
    FdealId : String;
    FdealServingMetadata : TDealServingMetadata;
    FdeliveryControl : TDeliveryControl;
    FexternalDealId : String;
    FflightEndTimeMs : String;
    FflightStartTimeMs : String;
    FinventoryDescription : String;
    Fkind : String;
    FlastUpdateTimeMs : String;
    Fname : String;
    FproductId : String;
    FproductRevisionNumber : String;
    FprogrammaticCreativeSource : String;
    FproposalId : String;
    FsellerContacts : TMarketplaceDealTypesellerContactsArray;
    FsharedTargetings : TMarketplaceDealTypesharedTargetingsArray;
    FsyndicationProduct : String;
    Fterms : TDealTerms;
    FwebPropertyCode : String;
  Protected
    //Property setters
    Procedure SetbuyerPrivateData(AIndex : Integer; const AValue : TPrivateData); virtual;
    Procedure SetcreationTimeMs(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcreativePreApprovalPolicy(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcreativeSafeFrameCompatibility(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdealId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdealServingMetadata(AIndex : Integer; const AValue : TDealServingMetadata); virtual;
    Procedure SetdeliveryControl(AIndex : Integer; const AValue : TDeliveryControl); virtual;
    Procedure SetexternalDealId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetflightEndTimeMs(AIndex : Integer; const AValue : String); virtual;
    Procedure SetflightStartTimeMs(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinventoryDescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlastUpdateTimeMs(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetproductId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetproductRevisionNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprogrammaticCreativeSource(AIndex : Integer; const AValue : String); virtual;
    Procedure SetproposalId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsellerContacts(AIndex : Integer; const AValue : TMarketplaceDealTypesellerContactsArray); virtual;
    Procedure SetsharedTargetings(AIndex : Integer; const AValue : TMarketplaceDealTypesharedTargetingsArray); virtual;
    Procedure SetsyndicationProduct(AIndex : Integer; const AValue : String); virtual;
    Procedure Setterms(AIndex : Integer; const AValue : TDealTerms); virtual;
    Procedure SetwebPropertyCode(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property buyerPrivateData : TPrivateData Index 0 Read FbuyerPrivateData Write SetbuyerPrivateData;
    Property creationTimeMs : String Index 8 Read FcreationTimeMs Write SetcreationTimeMs;
    Property creativePreApprovalPolicy : String Index 16 Read FcreativePreApprovalPolicy Write SetcreativePreApprovalPolicy;
    Property creativeSafeFrameCompatibility : String Index 24 Read FcreativeSafeFrameCompatibility Write SetcreativeSafeFrameCompatibility;
    Property dealId : String Index 32 Read FdealId Write SetdealId;
    Property dealServingMetadata : TDealServingMetadata Index 40 Read FdealServingMetadata Write SetdealServingMetadata;
    Property deliveryControl : TDeliveryControl Index 48 Read FdeliveryControl Write SetdeliveryControl;
    Property externalDealId : String Index 56 Read FexternalDealId Write SetexternalDealId;
    Property flightEndTimeMs : String Index 64 Read FflightEndTimeMs Write SetflightEndTimeMs;
    Property flightStartTimeMs : String Index 72 Read FflightStartTimeMs Write SetflightStartTimeMs;
    Property inventoryDescription : String Index 80 Read FinventoryDescription Write SetinventoryDescription;
    Property kind : String Index 88 Read Fkind Write Setkind;
    Property lastUpdateTimeMs : String Index 96 Read FlastUpdateTimeMs Write SetlastUpdateTimeMs;
    Property name : String Index 104 Read Fname Write Setname;
    Property productId : String Index 112 Read FproductId Write SetproductId;
    Property productRevisionNumber : String Index 120 Read FproductRevisionNumber Write SetproductRevisionNumber;
    Property programmaticCreativeSource : String Index 128 Read FprogrammaticCreativeSource Write SetprogrammaticCreativeSource;
    Property proposalId : String Index 136 Read FproposalId Write SetproposalId;
    Property sellerContacts : TMarketplaceDealTypesellerContactsArray Index 144 Read FsellerContacts Write SetsellerContacts;
    Property sharedTargetings : TMarketplaceDealTypesharedTargetingsArray Index 152 Read FsharedTargetings Write SetsharedTargetings;
    Property syndicationProduct : String Index 160 Read FsyndicationProduct Write SetsyndicationProduct;
    Property terms : TDealTerms Index 168 Read Fterms Write Setterms;
    Property webPropertyCode : String Index 176 Read FwebPropertyCode Write SetwebPropertyCode;
  end;
  TMarketplaceDealClass = Class of TMarketplaceDeal;
  
  { --------------------------------------------------------------------
    TMarketplaceDealParty
    --------------------------------------------------------------------}
  
  TMarketplaceDealParty = Class(TGoogleBaseObject)
  Private
    Fbuyer : TBuyer;
    Fseller : TSeller;
  Protected
    //Property setters
    Procedure Setbuyer(AIndex : Integer; const AValue : TBuyer); virtual;
    Procedure Setseller(AIndex : Integer; const AValue : TSeller); virtual;
  Public
  Published
    Property buyer : TBuyer Index 0 Read Fbuyer Write Setbuyer;
    Property seller : TSeller Index 8 Read Fseller Write Setseller;
  end;
  TMarketplaceDealPartyClass = Class of TMarketplaceDealParty;
  
  { --------------------------------------------------------------------
    TMarketplaceLabel
    --------------------------------------------------------------------}
  
  TMarketplaceLabel = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FcreateTimeMs : String;
    FdeprecatedMarketplaceDealParty : TMarketplaceDealParty;
    F_label : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcreateTimeMs(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdeprecatedMarketplaceDealParty(AIndex : Integer; const AValue : TMarketplaceDealParty); virtual;
    Procedure Set_label(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property createTimeMs : String Index 8 Read FcreateTimeMs Write SetcreateTimeMs;
    Property deprecatedMarketplaceDealParty : TMarketplaceDealParty Index 16 Read FdeprecatedMarketplaceDealParty Write SetdeprecatedMarketplaceDealParty;
    Property _label : String Index 24 Read F_label Write Set_label;
  end;
  TMarketplaceLabelClass = Class of TMarketplaceLabel;
  
  { --------------------------------------------------------------------
    TMarketplaceNote
    --------------------------------------------------------------------}
  
  TMarketplaceNote = Class(TGoogleBaseObject)
  Private
    FcreatorRole : String;
    FdealId : String;
    Fkind : String;
    Fnote : String;
    FnoteId : String;
    FproposalId : String;
    FproposalRevisionNumber : String;
    FtimestampMs : String;
  Protected
    //Property setters
    Procedure SetcreatorRole(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdealId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setnote(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnoteId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetproposalId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetproposalRevisionNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure SettimestampMs(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property creatorRole : String Index 0 Read FcreatorRole Write SetcreatorRole;
    Property dealId : String Index 8 Read FdealId Write SetdealId;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property note : String Index 24 Read Fnote Write Setnote;
    Property noteId : String Index 32 Read FnoteId Write SetnoteId;
    Property proposalId : String Index 40 Read FproposalId Write SetproposalId;
    Property proposalRevisionNumber : String Index 48 Read FproposalRevisionNumber Write SetproposalRevisionNumber;
    Property timestampMs : String Index 56 Read FtimestampMs Write SettimestampMs;
  end;
  TMarketplaceNoteClass = Class of TMarketplaceNote;
  
  { --------------------------------------------------------------------
    TPerformanceReport
    --------------------------------------------------------------------}
  
  TPerformanceReport = Class(TGoogleBaseObject)
  Private
    FbidRate : double;
    FbidRequestRate : double;
    FcalloutStatusRate : TTJSONSchemaArray;
    FcookieMatcherStatusRate : TTJSONSchemaArray;
    FcreativeStatusRate : TTJSONSchemaArray;
    FfilteredBidRate : double;
    FhostedMatchStatusRate : TTJSONSchemaArray;
    FinventoryMatchRate : double;
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
    FsuccessfulRequestRate : double;
    Ftimestamp : String;
    FunsuccessfulRequestRate : double;
  Protected
    //Property setters
    Procedure SetbidRate(AIndex : Integer; const AValue : double); virtual;
    Procedure SetbidRequestRate(AIndex : Integer; const AValue : double); virtual;
    Procedure SetcalloutStatusRate(AIndex : Integer; const AValue : TTJSONSchemaArray); virtual;
    Procedure SetcookieMatcherStatusRate(AIndex : Integer; const AValue : TTJSONSchemaArray); virtual;
    Procedure SetcreativeStatusRate(AIndex : Integer; const AValue : TTJSONSchemaArray); virtual;
    Procedure SetfilteredBidRate(AIndex : Integer; const AValue : double); virtual;
    Procedure SethostedMatchStatusRate(AIndex : Integer; const AValue : TTJSONSchemaArray); virtual;
    Procedure SetinventoryMatchRate(AIndex : Integer; const AValue : double); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlatency50thPercentile(AIndex : Integer; const AValue : double); virtual;
    Procedure Setlatency85thPercentile(AIndex : Integer; const AValue : double); virtual;
    Procedure Setlatency95thPercentile(AIndex : Integer; const AValue : double); virtual;
    Procedure SetnoQuotaInRegion(AIndex : Integer; const AValue : double); virtual;
    Procedure SetoutOfQuota(AIndex : Integer; const AValue : double); virtual;
    Procedure SetpixelMatchRequests(AIndex : Integer; const AValue : double); virtual;
    Procedure SetpixelMatchResponses(AIndex : Integer; const AValue : double); virtual;
    Procedure SetquotaConfiguredLimit(AIndex : Integer; const AValue : double); virtual;
    Procedure SetquotaThrottledLimit(AIndex : Integer; const AValue : double); virtual;
    Procedure Setregion(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsuccessfulRequestRate(AIndex : Integer; const AValue : double); virtual;
    Procedure Settimestamp(AIndex : Integer; const AValue : String); virtual;
    Procedure SetunsuccessfulRequestRate(AIndex : Integer; const AValue : double); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property bidRate : double Index 0 Read FbidRate Write SetbidRate;
    Property bidRequestRate : double Index 8 Read FbidRequestRate Write SetbidRequestRate;
    Property calloutStatusRate : TTJSONSchemaArray Index 16 Read FcalloutStatusRate Write SetcalloutStatusRate;
    Property cookieMatcherStatusRate : TTJSONSchemaArray Index 24 Read FcookieMatcherStatusRate Write SetcookieMatcherStatusRate;
    Property creativeStatusRate : TTJSONSchemaArray Index 32 Read FcreativeStatusRate Write SetcreativeStatusRate;
    Property filteredBidRate : double Index 40 Read FfilteredBidRate Write SetfilteredBidRate;
    Property hostedMatchStatusRate : TTJSONSchemaArray Index 48 Read FhostedMatchStatusRate Write SethostedMatchStatusRate;
    Property inventoryMatchRate : double Index 56 Read FinventoryMatchRate Write SetinventoryMatchRate;
    Property kind : String Index 64 Read Fkind Write Setkind;
    Property latency50thPercentile : double Index 72 Read Flatency50thPercentile Write Setlatency50thPercentile;
    Property latency85thPercentile : double Index 80 Read Flatency85thPercentile Write Setlatency85thPercentile;
    Property latency95thPercentile : double Index 88 Read Flatency95thPercentile Write Setlatency95thPercentile;
    Property noQuotaInRegion : double Index 96 Read FnoQuotaInRegion Write SetnoQuotaInRegion;
    Property outOfQuota : double Index 104 Read FoutOfQuota Write SetoutOfQuota;
    Property pixelMatchRequests : double Index 112 Read FpixelMatchRequests Write SetpixelMatchRequests;
    Property pixelMatchResponses : double Index 120 Read FpixelMatchResponses Write SetpixelMatchResponses;
    Property quotaConfiguredLimit : double Index 128 Read FquotaConfiguredLimit Write SetquotaConfiguredLimit;
    Property quotaThrottledLimit : double Index 136 Read FquotaThrottledLimit Write SetquotaThrottledLimit;
    Property region : String Index 144 Read Fregion Write Setregion;
    Property successfulRequestRate : double Index 152 Read FsuccessfulRequestRate Write SetsuccessfulRequestRate;
    Property timestamp : String Index 160 Read Ftimestamp Write Settimestamp;
    Property unsuccessfulRequestRate : double Index 168 Read FunsuccessfulRequestRate Write SetunsuccessfulRequestRate;
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
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetperformanceReport(AIndex : Integer; const AValue : TPerformanceReportListTypeperformanceReportArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
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
    Procedure Setheight(AIndex : Integer; const AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; const AValue : String); virtual;
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
    Procedure Settoken(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
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
    Procedure Settoken(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property token : String Index 0 Read Ftoken Write Settoken;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TPretargetingConfigTypeplacementsItemClass = Class of TPretargetingConfigTypeplacementsItem;
  
  { --------------------------------------------------------------------
    TPretargetingConfigTypevideoPlayerSizesItem
    --------------------------------------------------------------------}
  
  TPretargetingConfigTypevideoPlayerSizesItem = Class(TGoogleBaseObject)
  Private
    FaspectRatio : String;
    FminHeight : String;
    FminWidth : String;
  Protected
    //Property setters
    Procedure SetaspectRatio(AIndex : Integer; const AValue : String); virtual;
    Procedure SetminHeight(AIndex : Integer; const AValue : String); virtual;
    Procedure SetminWidth(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property aspectRatio : String Index 0 Read FaspectRatio Write SetaspectRatio;
    Property minHeight : String Index 8 Read FminHeight Write SetminHeight;
    Property minWidth : String Index 16 Read FminWidth Write SetminWidth;
  end;
  TPretargetingConfigTypevideoPlayerSizesItemClass = Class of TPretargetingConfigTypevideoPlayerSizesItem;
  
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
    FvideoPlayerSizes : TPretargetingConfigTypevideoPlayerSizesArray;
  Protected
    //Property setters
    Procedure SetbillingId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetconfigId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetconfigName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcreativeType(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setdimensions(AIndex : Integer; const AValue : TPretargetingConfigTypedimensionsArray); virtual;
    Procedure SetexcludedContentLabels(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetexcludedGeoCriteriaIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetexcludedPlacements(AIndex : Integer; const AValue : TPretargetingConfigTypeexcludedPlacementsArray); virtual;
    Procedure SetexcludedUserLists(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetexcludedVerticals(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetgeoCriteriaIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetisActive(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlanguages(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetmobileCarriers(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetmobileDevices(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetmobileOperatingSystemVersions(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setplacements(AIndex : Integer; const AValue : TPretargetingConfigTypeplacementsArray); virtual;
    Procedure Setplatforms(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetsupportedCreativeAttributes(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetuserLists(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetvendorTypes(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setverticals(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetvideoPlayerSizes(AIndex : Integer; const AValue : TPretargetingConfigTypevideoPlayerSizesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
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
    Property videoPlayerSizes : TPretargetingConfigTypevideoPlayerSizesArray Index 184 Read FvideoPlayerSizes Write SetvideoPlayerSizes;
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
    Procedure Setitems(AIndex : Integer; const AValue : TPretargetingConfigListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TPretargetingConfigListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TPretargetingConfigListClass = Class of TPretargetingConfigList;
  
  { --------------------------------------------------------------------
    TPrice
    --------------------------------------------------------------------}
  
  TPrice = Class(TGoogleBaseObject)
  Private
    FamountMicros : double;
    FcurrencyCode : String;
    FpricingType : String;
  Protected
    //Property setters
    Procedure SetamountMicros(AIndex : Integer; const AValue : double); virtual;
    Procedure SetcurrencyCode(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpricingType(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property amountMicros : double Index 0 Read FamountMicros Write SetamountMicros;
    Property currencyCode : String Index 8 Read FcurrencyCode Write SetcurrencyCode;
    Property pricingType : String Index 16 Read FpricingType Write SetpricingType;
  end;
  TPriceClass = Class of TPrice;
  
  { --------------------------------------------------------------------
    TPricePerBuyer
    --------------------------------------------------------------------}
  
  TPricePerBuyer = Class(TGoogleBaseObject)
  Private
    FauctionTier : String;
    Fbuyer : TBuyer;
    Fprice : TPrice;
  Protected
    //Property setters
    Procedure SetauctionTier(AIndex : Integer; const AValue : String); virtual;
    Procedure Setbuyer(AIndex : Integer; const AValue : TBuyer); virtual;
    Procedure Setprice(AIndex : Integer; const AValue : TPrice); virtual;
  Public
  Published
    Property auctionTier : String Index 0 Read FauctionTier Write SetauctionTier;
    Property buyer : TBuyer Index 8 Read Fbuyer Write Setbuyer;
    Property price : TPrice Index 16 Read Fprice Write Setprice;
  end;
  TPricePerBuyerClass = Class of TPricePerBuyer;
  
  { --------------------------------------------------------------------
    TPrivateData
    --------------------------------------------------------------------}
  
  TPrivateData = Class(TGoogleBaseObject)
  Private
    FreferenceId : String;
    FreferencePayload : String;
  Protected
    //Property setters
    Procedure SetreferenceId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreferencePayload(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property referenceId : String Index 0 Read FreferenceId Write SetreferenceId;
    Property referencePayload : String Index 8 Read FreferencePayload Write SetreferencePayload;
  end;
  TPrivateDataClass = Class of TPrivateData;
  
  { --------------------------------------------------------------------
    TProduct
    --------------------------------------------------------------------}
  
  TProduct = Class(TGoogleBaseObject)
  Private
    FcreationTimeMs : String;
    FcreatorContacts : TProductTypecreatorContactsArray;
    FdeliveryControl : TDeliveryControl;
    FflightEndTimeMs : String;
    FflightStartTimeMs : String;
    FhasCreatorSignedOff : boolean;
    FinventorySource : String;
    Fkind : String;
    Flabels : TProductTypelabelsArray;
    FlastUpdateTimeMs : String;
    FlegacyOfferId : String;
    Fname : String;
    FprivateAuctionId : String;
    FproductId : String;
    FpublisherProfileId : String;
    FpublisherProvidedForecast : TPublisherProvidedForecast;
    FrevisionNumber : String;
    Fseller : TSeller;
    FsharedTargetings : TProductTypesharedTargetingsArray;
    Fstate : String;
    FsyndicationProduct : String;
    Fterms : TDealTerms;
    FwebPropertyCode : String;
  Protected
    //Property setters
    Procedure SetcreationTimeMs(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcreatorContacts(AIndex : Integer; const AValue : TProductTypecreatorContactsArray); virtual;
    Procedure SetdeliveryControl(AIndex : Integer; const AValue : TDeliveryControl); virtual;
    Procedure SetflightEndTimeMs(AIndex : Integer; const AValue : String); virtual;
    Procedure SetflightStartTimeMs(AIndex : Integer; const AValue : String); virtual;
    Procedure SethasCreatorSignedOff(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetinventorySource(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlabels(AIndex : Integer; const AValue : TProductTypelabelsArray); virtual;
    Procedure SetlastUpdateTimeMs(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlegacyOfferId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprivateAuctionId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetproductId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpublisherProfileId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpublisherProvidedForecast(AIndex : Integer; const AValue : TPublisherProvidedForecast); virtual;
    Procedure SetrevisionNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure Setseller(AIndex : Integer; const AValue : TSeller); virtual;
    Procedure SetsharedTargetings(AIndex : Integer; const AValue : TProductTypesharedTargetingsArray); virtual;
    Procedure Setstate(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsyndicationProduct(AIndex : Integer; const AValue : String); virtual;
    Procedure Setterms(AIndex : Integer; const AValue : TDealTerms); virtual;
    Procedure SetwebPropertyCode(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property creationTimeMs : String Index 0 Read FcreationTimeMs Write SetcreationTimeMs;
    Property creatorContacts : TProductTypecreatorContactsArray Index 8 Read FcreatorContacts Write SetcreatorContacts;
    Property deliveryControl : TDeliveryControl Index 16 Read FdeliveryControl Write SetdeliveryControl;
    Property flightEndTimeMs : String Index 24 Read FflightEndTimeMs Write SetflightEndTimeMs;
    Property flightStartTimeMs : String Index 32 Read FflightStartTimeMs Write SetflightStartTimeMs;
    Property hasCreatorSignedOff : boolean Index 40 Read FhasCreatorSignedOff Write SethasCreatorSignedOff;
    Property inventorySource : String Index 48 Read FinventorySource Write SetinventorySource;
    Property kind : String Index 56 Read Fkind Write Setkind;
    Property labels : TProductTypelabelsArray Index 64 Read Flabels Write Setlabels;
    Property lastUpdateTimeMs : String Index 72 Read FlastUpdateTimeMs Write SetlastUpdateTimeMs;
    Property legacyOfferId : String Index 80 Read FlegacyOfferId Write SetlegacyOfferId;
    Property name : String Index 88 Read Fname Write Setname;
    Property privateAuctionId : String Index 96 Read FprivateAuctionId Write SetprivateAuctionId;
    Property productId : String Index 104 Read FproductId Write SetproductId;
    Property publisherProfileId : String Index 112 Read FpublisherProfileId Write SetpublisherProfileId;
    Property publisherProvidedForecast : TPublisherProvidedForecast Index 120 Read FpublisherProvidedForecast Write SetpublisherProvidedForecast;
    Property revisionNumber : String Index 128 Read FrevisionNumber Write SetrevisionNumber;
    Property seller : TSeller Index 136 Read Fseller Write Setseller;
    Property sharedTargetings : TProductTypesharedTargetingsArray Index 144 Read FsharedTargetings Write SetsharedTargetings;
    Property state : String Index 152 Read Fstate Write Setstate;
    Property syndicationProduct : String Index 160 Read FsyndicationProduct Write SetsyndicationProduct;
    Property terms : TDealTerms Index 168 Read Fterms Write Setterms;
    Property webPropertyCode : String Index 176 Read FwebPropertyCode Write SetwebPropertyCode;
  end;
  TProductClass = Class of TProduct;
  
  { --------------------------------------------------------------------
    TProposal
    --------------------------------------------------------------------}
  
  TProposal = Class(TGoogleBaseObject)
  Private
    FbilledBuyer : TBuyer;
    Fbuyer : TBuyer;
    FbuyerContacts : TProposalTypebuyerContactsArray;
    FbuyerPrivateData : TPrivateData;
    FhasBuyerSignedOff : boolean;
    FhasSellerSignedOff : boolean;
    FinventorySource : String;
    FisRenegotiating : boolean;
    FisSetupComplete : boolean;
    Fkind : String;
    Flabels : TProposalTypelabelsArray;
    FlastUpdaterOrCommentorRole : String;
    Fname : String;
    FnegotiationId : String;
    ForiginatorRole : String;
    FprivateAuctionId : String;
    FproposalId : String;
    FproposalState : String;
    FrevisionNumber : String;
    FrevisionTimeMs : String;
    Fseller : TSeller;
    FsellerContacts : TProposalTypesellerContactsArray;
  Protected
    //Property setters
    Procedure SetbilledBuyer(AIndex : Integer; const AValue : TBuyer); virtual;
    Procedure Setbuyer(AIndex : Integer; const AValue : TBuyer); virtual;
    Procedure SetbuyerContacts(AIndex : Integer; const AValue : TProposalTypebuyerContactsArray); virtual;
    Procedure SetbuyerPrivateData(AIndex : Integer; const AValue : TPrivateData); virtual;
    Procedure SethasBuyerSignedOff(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SethasSellerSignedOff(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetinventorySource(AIndex : Integer; const AValue : String); virtual;
    Procedure SetisRenegotiating(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetisSetupComplete(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlabels(AIndex : Integer; const AValue : TProposalTypelabelsArray); virtual;
    Procedure SetlastUpdaterOrCommentorRole(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnegotiationId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoriginatorRole(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprivateAuctionId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetproposalId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetproposalState(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrevisionNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrevisionTimeMs(AIndex : Integer; const AValue : String); virtual;
    Procedure Setseller(AIndex : Integer; const AValue : TSeller); virtual;
    Procedure SetsellerContacts(AIndex : Integer; const AValue : TProposalTypesellerContactsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property billedBuyer : TBuyer Index 0 Read FbilledBuyer Write SetbilledBuyer;
    Property buyer : TBuyer Index 8 Read Fbuyer Write Setbuyer;
    Property buyerContacts : TProposalTypebuyerContactsArray Index 16 Read FbuyerContacts Write SetbuyerContacts;
    Property buyerPrivateData : TPrivateData Index 24 Read FbuyerPrivateData Write SetbuyerPrivateData;
    Property hasBuyerSignedOff : boolean Index 32 Read FhasBuyerSignedOff Write SethasBuyerSignedOff;
    Property hasSellerSignedOff : boolean Index 40 Read FhasSellerSignedOff Write SethasSellerSignedOff;
    Property inventorySource : String Index 48 Read FinventorySource Write SetinventorySource;
    Property isRenegotiating : boolean Index 56 Read FisRenegotiating Write SetisRenegotiating;
    Property isSetupComplete : boolean Index 64 Read FisSetupComplete Write SetisSetupComplete;
    Property kind : String Index 72 Read Fkind Write Setkind;
    Property labels : TProposalTypelabelsArray Index 80 Read Flabels Write Setlabels;
    Property lastUpdaterOrCommentorRole : String Index 88 Read FlastUpdaterOrCommentorRole Write SetlastUpdaterOrCommentorRole;
    Property name : String Index 96 Read Fname Write Setname;
    Property negotiationId : String Index 104 Read FnegotiationId Write SetnegotiationId;
    Property originatorRole : String Index 112 Read ForiginatorRole Write SetoriginatorRole;
    Property privateAuctionId : String Index 120 Read FprivateAuctionId Write SetprivateAuctionId;
    Property proposalId : String Index 128 Read FproposalId Write SetproposalId;
    Property proposalState : String Index 136 Read FproposalState Write SetproposalState;
    Property revisionNumber : String Index 144 Read FrevisionNumber Write SetrevisionNumber;
    Property revisionTimeMs : String Index 152 Read FrevisionTimeMs Write SetrevisionTimeMs;
    Property seller : TSeller Index 160 Read Fseller Write Setseller;
    Property sellerContacts : TProposalTypesellerContactsArray Index 168 Read FsellerContacts Write SetsellerContacts;
  end;
  TProposalClass = Class of TProposal;
  
  { --------------------------------------------------------------------
    TPublisherProfileApiProto
    --------------------------------------------------------------------}
  
  TPublisherProfileApiProto = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Faudience : String;
    FbuyerPitchStatement : String;
    FdirectContact : String;
    Fexchange : String;
    FgooglePlusLink : String;
    FisParent : boolean;
    FisPublished : boolean;
    Fkind : String;
    FlogoUrl : String;
    FmediaKitLink : String;
    Fname : String;
    Foverview : String;
    FprofileId : integer;
    FprogrammaticContact : String;
    FpublisherDomains : TStringArray;
    FpublisherProfileId : String;
    FpublisherProvidedForecast : TPublisherProvidedForecast;
    FrateCardInfoLink : String;
    FsamplePageLink : String;
    Fseller : TSeller;
    Fstate : String;
    FtopHeadlines : TStringArray;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setaudience(AIndex : Integer; const AValue : String); virtual;
    Procedure SetbuyerPitchStatement(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdirectContact(AIndex : Integer; const AValue : String); virtual;
    Procedure Setexchange(AIndex : Integer; const AValue : String); virtual;
    Procedure SetgooglePlusLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetisParent(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetisPublished(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlogoUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmediaKitLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setoverview(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprofileId(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetprogrammaticContact(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpublisherDomains(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetpublisherProfileId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpublisherProvidedForecast(AIndex : Integer; const AValue : TPublisherProvidedForecast); virtual;
    Procedure SetrateCardInfoLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsamplePageLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Setseller(AIndex : Integer; const AValue : TSeller); virtual;
    Procedure Setstate(AIndex : Integer; const AValue : String); virtual;
    Procedure SettopHeadlines(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property audience : String Index 8 Read Faudience Write Setaudience;
    Property buyerPitchStatement : String Index 16 Read FbuyerPitchStatement Write SetbuyerPitchStatement;
    Property directContact : String Index 24 Read FdirectContact Write SetdirectContact;
    Property exchange : String Index 32 Read Fexchange Write Setexchange;
    Property googlePlusLink : String Index 40 Read FgooglePlusLink Write SetgooglePlusLink;
    Property isParent : boolean Index 48 Read FisParent Write SetisParent;
    Property isPublished : boolean Index 56 Read FisPublished Write SetisPublished;
    Property kind : String Index 64 Read Fkind Write Setkind;
    Property logoUrl : String Index 72 Read FlogoUrl Write SetlogoUrl;
    Property mediaKitLink : String Index 80 Read FmediaKitLink Write SetmediaKitLink;
    Property name : String Index 88 Read Fname Write Setname;
    Property overview : String Index 96 Read Foverview Write Setoverview;
    Property profileId : integer Index 104 Read FprofileId Write SetprofileId;
    Property programmaticContact : String Index 112 Read FprogrammaticContact Write SetprogrammaticContact;
    Property publisherDomains : TStringArray Index 120 Read FpublisherDomains Write SetpublisherDomains;
    Property publisherProfileId : String Index 128 Read FpublisherProfileId Write SetpublisherProfileId;
    Property publisherProvidedForecast : TPublisherProvidedForecast Index 136 Read FpublisherProvidedForecast Write SetpublisherProvidedForecast;
    Property rateCardInfoLink : String Index 144 Read FrateCardInfoLink Write SetrateCardInfoLink;
    Property samplePageLink : String Index 152 Read FsamplePageLink Write SetsamplePageLink;
    Property seller : TSeller Index 160 Read Fseller Write Setseller;
    Property state : String Index 168 Read Fstate Write Setstate;
    Property topHeadlines : TStringArray Index 176 Read FtopHeadlines Write SettopHeadlines;
  end;
  TPublisherProfileApiProtoClass = Class of TPublisherProfileApiProto;
  
  { --------------------------------------------------------------------
    TPublisherProvidedForecast
    --------------------------------------------------------------------}
  
  TPublisherProvidedForecast = Class(TGoogleBaseObject)
  Private
    Fdimensions : TPublisherProvidedForecastTypedimensionsArray;
    FweeklyImpressions : String;
    FweeklyUniques : String;
  Protected
    //Property setters
    Procedure Setdimensions(AIndex : Integer; const AValue : TPublisherProvidedForecastTypedimensionsArray); virtual;
    Procedure SetweeklyImpressions(AIndex : Integer; const AValue : String); virtual;
    Procedure SetweeklyUniques(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property dimensions : TPublisherProvidedForecastTypedimensionsArray Index 0 Read Fdimensions Write Setdimensions;
    Property weeklyImpressions : String Index 8 Read FweeklyImpressions Write SetweeklyImpressions;
    Property weeklyUniques : String Index 16 Read FweeklyUniques Write SetweeklyUniques;
  end;
  TPublisherProvidedForecastClass = Class of TPublisherProvidedForecast;
  
  { --------------------------------------------------------------------
    TSeller
    --------------------------------------------------------------------}
  
  TSeller = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FsubAccountId : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsubAccountId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property subAccountId : String Index 8 Read FsubAccountId Write SetsubAccountId;
  end;
  TSellerClass = Class of TSeller;
  
  { --------------------------------------------------------------------
    TSharedTargeting
    --------------------------------------------------------------------}
  
  TSharedTargeting = Class(TGoogleBaseObject)
  Private
    Fexclusions : TSharedTargetingTypeexclusionsArray;
    Finclusions : TSharedTargetingTypeinclusionsArray;
    Fkey : String;
  Protected
    //Property setters
    Procedure Setexclusions(AIndex : Integer; const AValue : TSharedTargetingTypeexclusionsArray); virtual;
    Procedure Setinclusions(AIndex : Integer; const AValue : TSharedTargetingTypeinclusionsArray); virtual;
    Procedure Setkey(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property exclusions : TSharedTargetingTypeexclusionsArray Index 0 Read Fexclusions Write Setexclusions;
    Property inclusions : TSharedTargetingTypeinclusionsArray Index 8 Read Finclusions Write Setinclusions;
    Property key : String Index 16 Read Fkey Write Setkey;
  end;
  TSharedTargetingClass = Class of TSharedTargeting;
  
  { --------------------------------------------------------------------
    TTargetingValue
    --------------------------------------------------------------------}
  
  TTargetingValue = Class(TGoogleBaseObject)
  Private
    FcreativeSizeValue : TTargetingValueCreativeSize;
    FdayPartTargetingValue : TTargetingValueDayPartTargeting;
    FlongValue : String;
    FstringValue : String;
  Protected
    //Property setters
    Procedure SetcreativeSizeValue(AIndex : Integer; const AValue : TTargetingValueCreativeSize); virtual;
    Procedure SetdayPartTargetingValue(AIndex : Integer; const AValue : TTargetingValueDayPartTargeting); virtual;
    Procedure SetlongValue(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstringValue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property creativeSizeValue : TTargetingValueCreativeSize Index 0 Read FcreativeSizeValue Write SetcreativeSizeValue;
    Property dayPartTargetingValue : TTargetingValueDayPartTargeting Index 8 Read FdayPartTargetingValue Write SetdayPartTargetingValue;
    Property longValue : String Index 16 Read FlongValue Write SetlongValue;
    Property stringValue : String Index 24 Read FstringValue Write SetstringValue;
  end;
  TTargetingValueClass = Class of TTargetingValue;
  
  { --------------------------------------------------------------------
    TTargetingValueCreativeSize
    --------------------------------------------------------------------}
  
  TTargetingValueCreativeSize = Class(TGoogleBaseObject)
  Private
    FcompanionSizes : TTargetingValueCreativeSizeTypecompanionSizesArray;
    FcreativeSizeType : String;
    Fsize : TTargetingValueSize;
  Protected
    //Property setters
    Procedure SetcompanionSizes(AIndex : Integer; const AValue : TTargetingValueCreativeSizeTypecompanionSizesArray); virtual;
    Procedure SetcreativeSizeType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsize(AIndex : Integer; const AValue : TTargetingValueSize); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property companionSizes : TTargetingValueCreativeSizeTypecompanionSizesArray Index 0 Read FcompanionSizes Write SetcompanionSizes;
    Property creativeSizeType : String Index 8 Read FcreativeSizeType Write SetcreativeSizeType;
    Property size : TTargetingValueSize Index 16 Read Fsize Write Setsize;
  end;
  TTargetingValueCreativeSizeClass = Class of TTargetingValueCreativeSize;
  
  { --------------------------------------------------------------------
    TTargetingValueDayPartTargeting
    --------------------------------------------------------------------}
  
  TTargetingValueDayPartTargeting = Class(TGoogleBaseObject)
  Private
    FdayParts : TTargetingValueDayPartTargetingTypedayPartsArray;
    FtimeZoneType : String;
  Protected
    //Property setters
    Procedure SetdayParts(AIndex : Integer; const AValue : TTargetingValueDayPartTargetingTypedayPartsArray); virtual;
    Procedure SettimeZoneType(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property dayParts : TTargetingValueDayPartTargetingTypedayPartsArray Index 0 Read FdayParts Write SetdayParts;
    Property timeZoneType : String Index 8 Read FtimeZoneType Write SettimeZoneType;
  end;
  TTargetingValueDayPartTargetingClass = Class of TTargetingValueDayPartTargeting;
  
  { --------------------------------------------------------------------
    TTargetingValueDayPartTargetingDayPart
    --------------------------------------------------------------------}
  
  TTargetingValueDayPartTargetingDayPart = Class(TGoogleBaseObject)
  Private
    FdayOfWeek : String;
    FendHour : integer;
    FendMinute : integer;
    FstartHour : integer;
    FstartMinute : integer;
  Protected
    //Property setters
    Procedure SetdayOfWeek(AIndex : Integer; const AValue : String); virtual;
    Procedure SetendHour(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetendMinute(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetstartHour(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetstartMinute(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property dayOfWeek : String Index 0 Read FdayOfWeek Write SetdayOfWeek;
    Property endHour : integer Index 8 Read FendHour Write SetendHour;
    Property endMinute : integer Index 16 Read FendMinute Write SetendMinute;
    Property startHour : integer Index 24 Read FstartHour Write SetstartHour;
    Property startMinute : integer Index 32 Read FstartMinute Write SetstartMinute;
  end;
  TTargetingValueDayPartTargetingDayPartClass = Class of TTargetingValueDayPartTargetingDayPart;
  
  { --------------------------------------------------------------------
    TTargetingValueSize
    --------------------------------------------------------------------}
  
  TTargetingValueSize = Class(TGoogleBaseObject)
  Private
    Fheight : integer;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure Setheight(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setwidth(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property height : integer Index 0 Read Fheight Write Setheight;
    Property width : integer Index 8 Read Fwidth Write Setwidth;
  end;
  TTargetingValueSizeClass = Class of TTargetingValueSize;
  
  { --------------------------------------------------------------------
    TUpdatePrivateAuctionProposalRequest
    --------------------------------------------------------------------}
  
  TUpdatePrivateAuctionProposalRequest = Class(TGoogleBaseObject)
  Private
    FexternalDealId : String;
    Fnote : TMarketplaceNote;
    FproposalRevisionNumber : String;
    FupdateAction : String;
  Protected
    //Property setters
    Procedure SetexternalDealId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setnote(AIndex : Integer; const AValue : TMarketplaceNote); virtual;
    Procedure SetproposalRevisionNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure SetupdateAction(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property externalDealId : String Index 0 Read FexternalDealId Write SetexternalDealId;
    Property note : TMarketplaceNote Index 8 Read Fnote Write Setnote;
    Property proposalRevisionNumber : String Index 16 Read FproposalRevisionNumber Write SetproposalRevisionNumber;
    Property updateAction : String Index 24 Read FupdateAction Write SetupdateAction;
  end;
  TUpdatePrivateAuctionProposalRequestClass = Class of TUpdatePrivateAuctionProposalRequest;
  
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
    dealsStatusFilter : String;
    maxResults : integer;
    openAuctionStatusFilter : String;
    pageToken : String;
  end;
  
  TCreativesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure AddDeal(accountId: integer; buyerCreativeId: string; dealId: string);
    Function Get(accountId: integer; buyerCreativeId: string) : TCreative;
    Function Insert(aCreative : TCreative) : TCreative;
    Function List(AQuery : string  = '') : TCreativesList;
    Function List(AQuery : TCreativeslistOptions) : TCreativesList;
    Procedure RemoveDeal(accountId: integer; buyerCreativeId: string; dealId: string);
  end;
  
  
  { --------------------------------------------------------------------
    TMarketplacedealsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TMarketplacedealsResource, method List
  
  TMarketplacedealsListOptions = Record
    pqlQuery : String;
  end;
  
  TMarketplacedealsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(proposalId: string; aDeleteOrderDealsRequest : TDeleteOrderDealsRequest) : TDeleteOrderDealsResponse;
    Function Insert(proposalId: string; aAddOrderDealsRequest : TAddOrderDealsRequest) : TAddOrderDealsResponse;
    Function List(proposalId: string; AQuery : string  = '') : TGetOrderDealsResponse;
    Function List(proposalId: string; AQuery : TMarketplacedealslistOptions) : TGetOrderDealsResponse;
    Function Update(proposalId: string; aEditAllOrderDealsRequest : TEditAllOrderDealsRequest) : TEditAllOrderDealsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TMarketplacenotesResource
    --------------------------------------------------------------------}
  
  TMarketplacenotesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Insert(proposalId: string; aAddOrderNotesRequest : TAddOrderNotesRequest) : TAddOrderNotesResponse;
    Function List(proposalId: string) : TGetOrderNotesResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TMarketplaceprivateauctionResource
    --------------------------------------------------------------------}
  
  TMarketplaceprivateauctionResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Updateproposal(privateAuctionId: string; aUpdatePrivateAuctionProposalRequest : TUpdatePrivateAuctionProposalRequest);
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
    TProductsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProductsResource, method Search
  
  TProductsSearchOptions = Record
    pqlQuery : String;
  end;
  
  TProductsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(productId: string) : TProduct;
    Function Search(AQuery : string  = '') : TGetOffersResponse;
    Function Search(AQuery : TProductssearchOptions) : TGetOffersResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TProposalsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProposalsResource, method Search
  
  TProposalsSearchOptions = Record
    pqlQuery : String;
  end;
  
  TProposalsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(proposalId: string) : TProposal;
    Function Insert(aCreateOrdersRequest : TCreateOrdersRequest) : TCreateOrdersResponse;
    Function Patch(proposalId: string; revisionNumber: string; _updateAction: string; aProposal : TProposal) : TProposal;
    Function Search(AQuery : string  = '') : TGetOrdersResponse;
    Function Search(AQuery : TProposalssearchOptions) : TGetOrdersResponse;
    Procedure Setupcomplete(proposalId: string);
    Function Update(proposalId: string; revisionNumber: string; _updateAction: string; aProposal : TProposal) : TProposal;
  end;
  
  
  { --------------------------------------------------------------------
    TPubprofilesResource
    --------------------------------------------------------------------}
  
  TPubprofilesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(accountId: integer) : TGetPublisherProfilesByAccountIdResponse;
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
    FMarketplacedealsInstance : TMarketplacedealsResource;
    FMarketplacenotesInstance : TMarketplacenotesResource;
    FMarketplaceprivateauctionInstance : TMarketplaceprivateauctionResource;
    FPerformanceReportInstance : TPerformanceReportResource;
    FPretargetingConfigInstance : TPretargetingConfigResource;
    FProductsInstance : TProductsResource;
    FProposalsInstance : TProposalsResource;
    FPubprofilesInstance : TPubprofilesResource;
    Function GetAccountsInstance : TAccountsResource;virtual;
    Function GetBillingInfoInstance : TBillingInfoResource;virtual;
    Function GetBudgetInstance : TBudgetResource;virtual;
    Function GetCreativesInstance : TCreativesResource;virtual;
    Function GetMarketplacedealsInstance : TMarketplacedealsResource;virtual;
    Function GetMarketplacenotesInstance : TMarketplacenotesResource;virtual;
    Function GetMarketplaceprivateauctionInstance : TMarketplaceprivateauctionResource;virtual;
    Function GetPerformanceReportInstance : TPerformanceReportResource;virtual;
    Function GetPretargetingConfigInstance : TPretargetingConfigResource;virtual;
    Function GetProductsInstance : TProductsResource;virtual;
    Function GetProposalsInstance : TProposalsResource;virtual;
    Function GetPubprofilesInstance : TPubprofilesResource;virtual;
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
    Function CreateMarketplacedealsResource(AOwner : TComponent) : TMarketplacedealsResource;virtual;overload;
    Function CreateMarketplacedealsResource : TMarketplacedealsResource;virtual;overload;
    Function CreateMarketplacenotesResource(AOwner : TComponent) : TMarketplacenotesResource;virtual;overload;
    Function CreateMarketplacenotesResource : TMarketplacenotesResource;virtual;overload;
    Function CreateMarketplaceprivateauctionResource(AOwner : TComponent) : TMarketplaceprivateauctionResource;virtual;overload;
    Function CreateMarketplaceprivateauctionResource : TMarketplaceprivateauctionResource;virtual;overload;
    Function CreatePerformanceReportResource(AOwner : TComponent) : TPerformanceReportResource;virtual;overload;
    Function CreatePerformanceReportResource : TPerformanceReportResource;virtual;overload;
    Function CreatePretargetingConfigResource(AOwner : TComponent) : TPretargetingConfigResource;virtual;overload;
    Function CreatePretargetingConfigResource : TPretargetingConfigResource;virtual;overload;
    Function CreateProductsResource(AOwner : TComponent) : TProductsResource;virtual;overload;
    Function CreateProductsResource : TProductsResource;virtual;overload;
    Function CreateProposalsResource(AOwner : TComponent) : TProposalsResource;virtual;overload;
    Function CreateProposalsResource : TProposalsResource;virtual;overload;
    Function CreatePubprofilesResource(AOwner : TComponent) : TPubprofilesResource;virtual;overload;
    Function CreatePubprofilesResource : TPubprofilesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AccountsResource : TAccountsResource Read GetAccountsInstance;
    Property BillingInfoResource : TBillingInfoResource Read GetBillingInfoInstance;
    Property BudgetResource : TBudgetResource Read GetBudgetInstance;
    Property CreativesResource : TCreativesResource Read GetCreativesInstance;
    Property MarketplacedealsResource : TMarketplacedealsResource Read GetMarketplacedealsInstance;
    Property MarketplacenotesResource : TMarketplacenotesResource Read GetMarketplacenotesInstance;
    Property MarketplaceprivateauctionResource : TMarketplaceprivateauctionResource Read GetMarketplaceprivateauctionInstance;
    Property PerformanceReportResource : TPerformanceReportResource Read GetPerformanceReportInstance;
    Property PretargetingConfigResource : TPretargetingConfigResource Read GetPretargetingConfigInstance;
    Property ProductsResource : TProductsResource Read GetProductsInstance;
    Property ProposalsResource : TProposalsResource Read GetProposalsInstance;
    Property PubprofilesResource : TPubprofilesResource Read GetPubprofilesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAccountTypebidderLocationItem
  --------------------------------------------------------------------}


Procedure TAccountTypebidderLocationItem.SetbidProtocol(AIndex : Integer; const AValue : String); 

begin
  If (FbidProtocol=AValue) then exit;
  FbidProtocol:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTypebidderLocationItem.SetmaximumQps(AIndex : Integer; const AValue : integer); 

begin
  If (FmaximumQps=AValue) then exit;
  FmaximumQps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTypebidderLocationItem.Setregion(AIndex : Integer; const AValue : String); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTypebidderLocationItem.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccount
  --------------------------------------------------------------------}


Procedure TAccount.SetbidderLocation(AIndex : Integer; const AValue : TAccountTypebidderLocationArray); 

begin
  If (FbidderLocation=AValue) then exit;
  FbidderLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetcookieMatchingNid(AIndex : Integer; const AValue : String); 

begin
  If (FcookieMatchingNid=AValue) then exit;
  FcookieMatchingNid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetcookieMatchingUrl(AIndex : Integer; const AValue : String); 

begin
  If (FcookieMatchingUrl=AValue) then exit;
  FcookieMatchingUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setid(AIndex : Integer; const AValue : integer); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetmaximumActiveCreatives(AIndex : Integer; const AValue : integer); 

begin
  If (FmaximumActiveCreatives=AValue) then exit;
  FmaximumActiveCreatives:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetmaximumTotalQps(AIndex : Integer; const AValue : integer); 

begin
  If (FmaximumTotalQps=AValue) then exit;
  FmaximumTotalQps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetnumberActiveCreatives(AIndex : Integer; const AValue : integer); 

begin
  If (FnumberActiveCreatives=AValue) then exit;
  FnumberActiveCreatives:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccount.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'bidderlocation' : SetLength(FbidderLocation,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccountsList
  --------------------------------------------------------------------}


Procedure TAccountsList.Setitems(AIndex : Integer; const AValue : TAccountsListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsList.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccountsList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAddOrderDealsRequest
  --------------------------------------------------------------------}


Procedure TAddOrderDealsRequest.Setdeals(AIndex : Integer; const AValue : TAddOrderDealsRequestTypedealsArray); 

begin
  If (Fdeals=AValue) then exit;
  Fdeals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddOrderDealsRequest.SetproposalRevisionNumber(AIndex : Integer; const AValue : String); 

begin
  If (FproposalRevisionNumber=AValue) then exit;
  FproposalRevisionNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddOrderDealsRequest.SetupdateAction(AIndex : Integer; const AValue : String); 

begin
  If (FupdateAction=AValue) then exit;
  FupdateAction:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAddOrderDealsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'deals' : SetLength(Fdeals,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAddOrderDealsResponse
  --------------------------------------------------------------------}


Procedure TAddOrderDealsResponse.Setdeals(AIndex : Integer; const AValue : TAddOrderDealsResponseTypedealsArray); 

begin
  If (Fdeals=AValue) then exit;
  Fdeals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddOrderDealsResponse.SetproposalRevisionNumber(AIndex : Integer; const AValue : String); 

begin
  If (FproposalRevisionNumber=AValue) then exit;
  FproposalRevisionNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAddOrderDealsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'deals' : SetLength(Fdeals,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAddOrderNotesRequest
  --------------------------------------------------------------------}


Procedure TAddOrderNotesRequest.Setnotes(AIndex : Integer; const AValue : TAddOrderNotesRequestTypenotesArray); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAddOrderNotesRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'notes' : SetLength(Fnotes,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAddOrderNotesResponse
  --------------------------------------------------------------------}


Procedure TAddOrderNotesResponse.Setnotes(AIndex : Integer; const AValue : TAddOrderNotesResponseTypenotesArray); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAddOrderNotesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'notes' : SetLength(Fnotes,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBillingInfo
  --------------------------------------------------------------------}


Procedure TBillingInfo.SetaccountId(AIndex : Integer; const AValue : integer); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBillingInfo.SetaccountName(AIndex : Integer; const AValue : String); 

begin
  If (FaccountName=AValue) then exit;
  FaccountName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBillingInfo.SetbillingId(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FbillingId=AValue) then exit;
  FbillingId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBillingInfo.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBillingInfo.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'billingid' : SetLength(FbillingId,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBillingInfoList
  --------------------------------------------------------------------}


Procedure TBillingInfoList.Setitems(AIndex : Integer; const AValue : TBillingInfoListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBillingInfoList.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBillingInfoList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBudget
  --------------------------------------------------------------------}


Procedure TBudget.SetaccountId(AIndex : Integer; const AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBudget.SetbillingId(AIndex : Integer; const AValue : String); 

begin
  If (FbillingId=AValue) then exit;
  FbillingId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBudget.SetbudgetAmount(AIndex : Integer; const AValue : String); 

begin
  If (FbudgetAmount=AValue) then exit;
  FbudgetAmount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBudget.SetcurrencyCode(AIndex : Integer; const AValue : String); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBudget.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBudget.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBuyer
  --------------------------------------------------------------------}


Procedure TBuyer.SetaccountId(AIndex : Integer; const AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TContactInformation
  --------------------------------------------------------------------}


Procedure TContactInformation.Setemail(AIndex : Integer; const AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContactInformation.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreateOrdersRequest
  --------------------------------------------------------------------}


Procedure TCreateOrdersRequest.Setproposals(AIndex : Integer; const AValue : TCreateOrdersRequestTypeproposalsArray); 

begin
  If (Fproposals=AValue) then exit;
  Fproposals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateOrdersRequest.SetwebPropertyCode(AIndex : Integer; const AValue : String); 

begin
  If (FwebPropertyCode=AValue) then exit;
  FwebPropertyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCreateOrdersRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'proposals' : SetLength(Fproposals,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCreateOrdersResponse
  --------------------------------------------------------------------}


Procedure TCreateOrdersResponse.Setproposals(AIndex : Integer; const AValue : TCreateOrdersResponseTypeproposalsArray); 

begin
  If (Fproposals=AValue) then exit;
  Fproposals:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCreateOrdersResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'proposals' : SetLength(Fproposals,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCreativeTypecorrectionsItem
  --------------------------------------------------------------------}


Procedure TCreativeTypecorrectionsItem.Setdetails(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypecorrectionsItem.Setreason(AIndex : Integer; const AValue : String); 

begin
  If (Freason=AValue) then exit;
  Freason:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCreativeTypecorrectionsItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'details' : SetLength(Fdetails,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCreativeTypefilteringReasonsTypereasonsItem
  --------------------------------------------------------------------}


Procedure TCreativeTypefilteringReasonsTypereasonsItem.SetfilteringCount(AIndex : Integer; const AValue : String); 

begin
  If (FfilteringCount=AValue) then exit;
  FfilteringCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypefilteringReasonsTypereasonsItem.SetfilteringStatus(AIndex : Integer; const AValue : integer); 

begin
  If (FfilteringStatus=AValue) then exit;
  FfilteringStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativeTypefilteringReasons
  --------------------------------------------------------------------}


Procedure TCreativeTypefilteringReasons.Setdate(AIndex : Integer; const AValue : String); 

begin
  If (Fdate=AValue) then exit;
  Fdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypefilteringReasons.Setreasons(AIndex : Integer; const AValue : TCreativeTypefilteringReasonsTypereasonsArray); 

begin
  If (Freasons=AValue) then exit;
  Freasons:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCreativeTypefilteringReasons.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'reasons' : SetLength(Freasons,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCreativeTypenativeAdTypeappIcon
  --------------------------------------------------------------------}


Procedure TCreativeTypenativeAdTypeappIcon.Setheight(AIndex : Integer; const AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypenativeAdTypeappIcon.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypenativeAdTypeappIcon.Setwidth(AIndex : Integer; const AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativeTypenativeAdTypeimage
  --------------------------------------------------------------------}


Procedure TCreativeTypenativeAdTypeimage.Setheight(AIndex : Integer; const AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypenativeAdTypeimage.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypenativeAdTypeimage.Setwidth(AIndex : Integer; const AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativeTypenativeAdTypelogo
  --------------------------------------------------------------------}


Procedure TCreativeTypenativeAdTypelogo.Setheight(AIndex : Integer; const AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypenativeAdTypelogo.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypenativeAdTypelogo.Setwidth(AIndex : Integer; const AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreativeTypenativeAd
  --------------------------------------------------------------------}


Procedure TCreativeTypenativeAd.Setadvertiser(AIndex : Integer; const AValue : String); 

begin
  If (Fadvertiser=AValue) then exit;
  Fadvertiser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypenativeAd.SetappIcon(AIndex : Integer; const AValue : TCreativeTypenativeAdTypeappIcon); 

begin
  If (FappIcon=AValue) then exit;
  FappIcon:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypenativeAd.Setbody(AIndex : Integer; const AValue : String); 

begin
  If (Fbody=AValue) then exit;
  Fbody:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypenativeAd.SetcallToAction(AIndex : Integer; const AValue : String); 

begin
  If (FcallToAction=AValue) then exit;
  FcallToAction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypenativeAd.SetclickTrackingUrl(AIndex : Integer; const AValue : String); 

begin
  If (FclickTrackingUrl=AValue) then exit;
  FclickTrackingUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypenativeAd.Setheadline(AIndex : Integer; const AValue : String); 

begin
  If (Fheadline=AValue) then exit;
  Fheadline:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypenativeAd.Setimage(AIndex : Integer; const AValue : TCreativeTypenativeAdTypeimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypenativeAd.SetimpressionTrackingUrl(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FimpressionTrackingUrl=AValue) then exit;
  FimpressionTrackingUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypenativeAd.Setlogo(AIndex : Integer; const AValue : TCreativeTypenativeAdTypelogo); 

begin
  If (Flogo=AValue) then exit;
  Flogo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypenativeAd.Setprice(AIndex : Integer; const AValue : String); 

begin
  If (Fprice=AValue) then exit;
  Fprice:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypenativeAd.SetstarRating(AIndex : Integer; const AValue : double); 

begin
  If (FstarRating=AValue) then exit;
  FstarRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypenativeAd.Setstore(AIndex : Integer; const AValue : String); 

begin
  If (Fstore=AValue) then exit;
  Fstore:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCreativeTypenativeAd.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'impressiontrackingurl' : SetLength(FimpressionTrackingUrl,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCreativeTypeservingRestrictionsItemTypecontextsItem
  --------------------------------------------------------------------}


Procedure TCreativeTypeservingRestrictionsItemTypecontextsItem.SetauctionType(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FauctionType=AValue) then exit;
  FauctionType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypeservingRestrictionsItemTypecontextsItem.SetcontextType(AIndex : Integer; const AValue : String); 

begin
  If (FcontextType=AValue) then exit;
  FcontextType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypeservingRestrictionsItemTypecontextsItem.SetgeoCriteriaId(AIndex : Integer; const AValue : TintegerArray); 

begin
  If (FgeoCriteriaId=AValue) then exit;
  FgeoCriteriaId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypeservingRestrictionsItemTypecontextsItem.Setplatform(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fplatform=AValue) then exit;
  Fplatform:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCreativeTypeservingRestrictionsItemTypecontextsItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'auctiontype' : SetLength(FauctionType,ALength);
  'geocriteriaid' : SetLength(FgeoCriteriaId,ALength);
  'platform' : SetLength(Fplatform,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCreativeTypeservingRestrictionsItemTypedisapprovalReasonsItem
  --------------------------------------------------------------------}


Procedure TCreativeTypeservingRestrictionsItemTypedisapprovalReasonsItem.Setdetails(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypeservingRestrictionsItemTypedisapprovalReasonsItem.Setreason(AIndex : Integer; const AValue : String); 

begin
  If (Freason=AValue) then exit;
  Freason:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCreativeTypeservingRestrictionsItemTypedisapprovalReasonsItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'details' : SetLength(Fdetails,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCreativeTypeservingRestrictionsItem
  --------------------------------------------------------------------}


Procedure TCreativeTypeservingRestrictionsItem.Setcontexts(AIndex : Integer; const AValue : TCreativeTypeservingRestrictionsItemTypecontextsArray); 

begin
  If (Fcontexts=AValue) then exit;
  Fcontexts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypeservingRestrictionsItem.SetdisapprovalReasons(AIndex : Integer; const AValue : TCreativeTypeservingRestrictionsItemTypedisapprovalReasonsArray); 

begin
  If (FdisapprovalReasons=AValue) then exit;
  FdisapprovalReasons:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativeTypeservingRestrictionsItem.Setreason(AIndex : Integer; const AValue : String); 

begin
  If (Freason=AValue) then exit;
  Freason:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCreativeTypeservingRestrictionsItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'contexts' : SetLength(Fcontexts,ALength);
  'disapprovalreasons' : SetLength(FdisapprovalReasons,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCreative
  --------------------------------------------------------------------}


Procedure TCreative.SetHTMLSnippet(AIndex : Integer; const AValue : String); 

begin
  If (FHTMLSnippet=AValue) then exit;
  FHTMLSnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetaccountId(AIndex : Integer; const AValue : integer); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetadChoicesDestinationUrl(AIndex : Integer; const AValue : String); 

begin
  If (FadChoicesDestinationUrl=AValue) then exit;
  FadChoicesDestinationUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetadvertiserId(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetadvertiserName(AIndex : Integer; const AValue : String); 

begin
  If (FadvertiserName=AValue) then exit;
  FadvertiserName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetagencyId(AIndex : Integer; const AValue : String); 

begin
  If (FagencyId=AValue) then exit;
  FagencyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetapiUploadTimestamp(AIndex : Integer; const AValue : TDatetime); 

begin
  If (FapiUploadTimestamp=AValue) then exit;
  FapiUploadTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setattribute(AIndex : Integer; const AValue : TintegerArray); 

begin
  If (Fattribute=AValue) then exit;
  Fattribute:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetbuyerCreativeId(AIndex : Integer; const AValue : String); 

begin
  If (FbuyerCreativeId=AValue) then exit;
  FbuyerCreativeId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetclickThroughUrl(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FclickThroughUrl=AValue) then exit;
  FclickThroughUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setcorrections(AIndex : Integer; const AValue : TCreativeTypecorrectionsArray); 

begin
  If (Fcorrections=AValue) then exit;
  Fcorrections:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetdealsStatus(AIndex : Integer; const AValue : String); 

begin
  If (FdealsStatus=AValue) then exit;
  FdealsStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetfilteringReasons(AIndex : Integer; const AValue : TCreativeTypefilteringReasons); 

begin
  If (FfilteringReasons=AValue) then exit;
  FfilteringReasons:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setheight(AIndex : Integer; const AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetimpressionTrackingUrl(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FimpressionTrackingUrl=AValue) then exit;
  FimpressionTrackingUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetnativeAd(AIndex : Integer; const AValue : TCreativeTypenativeAd); 

begin
  If (FnativeAd=AValue) then exit;
  FnativeAd:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetopenAuctionStatus(AIndex : Integer; const AValue : String); 

begin
  If (FopenAuctionStatus=AValue) then exit;
  FopenAuctionStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetproductCategories(AIndex : Integer; const AValue : TintegerArray); 

begin
  If (FproductCategories=AValue) then exit;
  FproductCategories:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetrestrictedCategories(AIndex : Integer; const AValue : TintegerArray); 

begin
  If (FrestrictedCategories=AValue) then exit;
  FrestrictedCategories:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetsensitiveCategories(AIndex : Integer; const AValue : TintegerArray); 

begin
  If (FsensitiveCategories=AValue) then exit;
  FsensitiveCategories:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetservingRestrictions(AIndex : Integer; const AValue : TCreativeTypeservingRestrictionsArray); 

begin
  If (FservingRestrictions=AValue) then exit;
  FservingRestrictions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetvendorType(AIndex : Integer; const AValue : TintegerArray); 

begin
  If (FvendorType=AValue) then exit;
  FvendorType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setversion(AIndex : Integer; const AValue : integer); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.SetvideoURL(AIndex : Integer; const AValue : String); 

begin
  If (FvideoURL=AValue) then exit;
  FvideoURL:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreative.Setwidth(AIndex : Integer; const AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCreative.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'advertiserid' : SetLength(FadvertiserId,ALength);
  'attribute' : SetLength(Fattribute,ALength);
  'clickthroughurl' : SetLength(FclickThroughUrl,ALength);
  'corrections' : SetLength(Fcorrections,ALength);
  'impressiontrackingurl' : SetLength(FimpressionTrackingUrl,ALength);
  'productcategories' : SetLength(FproductCategories,ALength);
  'restrictedcategories' : SetLength(FrestrictedCategories,ALength);
  'sensitivecategories' : SetLength(FsensitiveCategories,ALength);
  'servingrestrictions' : SetLength(FservingRestrictions,ALength);
  'vendortype' : SetLength(FvendorType,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCreativesList
  --------------------------------------------------------------------}


Procedure TCreativesList.Setitems(AIndex : Integer; const AValue : TCreativesListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativesList.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreativesList.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCreativesList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDealServingMetadata
  --------------------------------------------------------------------}


Procedure TDealServingMetadata.SetdealPauseStatus(AIndex : Integer; const AValue : TDealServingMetadataDealPauseStatus); 

begin
  If (FdealPauseStatus=AValue) then exit;
  FdealPauseStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDealServingMetadataDealPauseStatus
  --------------------------------------------------------------------}


Procedure TDealServingMetadataDealPauseStatus.SetfirstPausedBy(AIndex : Integer; const AValue : String); 

begin
  If (FfirstPausedBy=AValue) then exit;
  FfirstPausedBy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDealServingMetadataDealPauseStatus.SethasBuyerPaused(AIndex : Integer; const AValue : boolean); 

begin
  If (FhasBuyerPaused=AValue) then exit;
  FhasBuyerPaused:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDealServingMetadataDealPauseStatus.SethasSellerPaused(AIndex : Integer; const AValue : boolean); 

begin
  If (FhasSellerPaused=AValue) then exit;
  FhasSellerPaused:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDealTerms
  --------------------------------------------------------------------}


Procedure TDealTerms.SetbrandingType(AIndex : Integer; const AValue : String); 

begin
  If (FbrandingType=AValue) then exit;
  FbrandingType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDealTerms.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDealTerms.SetestimatedGrossSpend(AIndex : Integer; const AValue : TPrice); 

begin
  If (FestimatedGrossSpend=AValue) then exit;
  FestimatedGrossSpend:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDealTerms.SetestimatedImpressionsPerDay(AIndex : Integer; const AValue : String); 

begin
  If (FestimatedImpressionsPerDay=AValue) then exit;
  FestimatedImpressionsPerDay:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDealTerms.SetguaranteedFixedPriceTerms(AIndex : Integer; const AValue : TDealTermsGuaranteedFixedPriceTerms); 

begin
  If (FguaranteedFixedPriceTerms=AValue) then exit;
  FguaranteedFixedPriceTerms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDealTerms.SetnonGuaranteedAuctionTerms(AIndex : Integer; const AValue : TDealTermsNonGuaranteedAuctionTerms); 

begin
  If (FnonGuaranteedAuctionTerms=AValue) then exit;
  FnonGuaranteedAuctionTerms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDealTerms.SetnonGuaranteedFixedPriceTerms(AIndex : Integer; const AValue : TDealTermsNonGuaranteedFixedPriceTerms); 

begin
  If (FnonGuaranteedFixedPriceTerms=AValue) then exit;
  FnonGuaranteedFixedPriceTerms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDealTerms.SetsellerTimeZone(AIndex : Integer; const AValue : String); 

begin
  If (FsellerTimeZone=AValue) then exit;
  FsellerTimeZone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDealTermsGuaranteedFixedPriceTerms
  --------------------------------------------------------------------}


Procedure TDealTermsGuaranteedFixedPriceTerms.SetbillingInfo(AIndex : Integer; const AValue : TDealTermsGuaranteedFixedPriceTermsBillingInfo); 

begin
  If (FbillingInfo=AValue) then exit;
  FbillingInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDealTermsGuaranteedFixedPriceTerms.SetfixedPrices(AIndex : Integer; const AValue : TDealTermsGuaranteedFixedPriceTermsTypefixedPricesArray); 

begin
  If (FfixedPrices=AValue) then exit;
  FfixedPrices:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDealTermsGuaranteedFixedPriceTerms.SetguaranteedImpressions(AIndex : Integer; const AValue : String); 

begin
  If (FguaranteedImpressions=AValue) then exit;
  FguaranteedImpressions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDealTermsGuaranteedFixedPriceTerms.SetguaranteedLooks(AIndex : Integer; const AValue : String); 

begin
  If (FguaranteedLooks=AValue) then exit;
  FguaranteedLooks:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDealTermsGuaranteedFixedPriceTerms.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'fixedprices' : SetLength(FfixedPrices,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDealTermsGuaranteedFixedPriceTermsBillingInfo
  --------------------------------------------------------------------}


Procedure TDealTermsGuaranteedFixedPriceTermsBillingInfo.SetcurrencyConversionTimeMs(AIndex : Integer; const AValue : String); 

begin
  If (FcurrencyConversionTimeMs=AValue) then exit;
  FcurrencyConversionTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDealTermsGuaranteedFixedPriceTermsBillingInfo.SetoriginalContractedQuantity(AIndex : Integer; const AValue : String); 

begin
  If (ForiginalContractedQuantity=AValue) then exit;
  ForiginalContractedQuantity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDealTermsGuaranteedFixedPriceTermsBillingInfo.Setprice(AIndex : Integer; const AValue : TPrice); 

begin
  If (Fprice=AValue) then exit;
  Fprice:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDealTermsNonGuaranteedAuctionTerms
  --------------------------------------------------------------------}


Procedure TDealTermsNonGuaranteedAuctionTerms.SetautoOptimizePrivateAuction(AIndex : Integer; const AValue : boolean); 

begin
  If (FautoOptimizePrivateAuction=AValue) then exit;
  FautoOptimizePrivateAuction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDealTermsNonGuaranteedAuctionTerms.SetreservePricePerBuyers(AIndex : Integer; const AValue : TDealTermsNonGuaranteedAuctionTermsTypereservePricePerBuyersArray); 

begin
  If (FreservePricePerBuyers=AValue) then exit;
  FreservePricePerBuyers:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDealTermsNonGuaranteedAuctionTerms.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'reservepriceperbuyers' : SetLength(FreservePricePerBuyers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDealTermsNonGuaranteedFixedPriceTerms
  --------------------------------------------------------------------}


Procedure TDealTermsNonGuaranteedFixedPriceTerms.SetfixedPrices(AIndex : Integer; const AValue : TDealTermsNonGuaranteedFixedPriceTermsTypefixedPricesArray); 

begin
  If (FfixedPrices=AValue) then exit;
  FfixedPrices:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDealTermsNonGuaranteedFixedPriceTerms.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'fixedprices' : SetLength(FfixedPrices,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDeleteOrderDealsRequest
  --------------------------------------------------------------------}


Procedure TDeleteOrderDealsRequest.SetdealIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FdealIds=AValue) then exit;
  FdealIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeleteOrderDealsRequest.SetproposalRevisionNumber(AIndex : Integer; const AValue : String); 

begin
  If (FproposalRevisionNumber=AValue) then exit;
  FproposalRevisionNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeleteOrderDealsRequest.SetupdateAction(AIndex : Integer; const AValue : String); 

begin
  If (FupdateAction=AValue) then exit;
  FupdateAction:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDeleteOrderDealsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'dealids' : SetLength(FdealIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDeleteOrderDealsResponse
  --------------------------------------------------------------------}


Procedure TDeleteOrderDealsResponse.Setdeals(AIndex : Integer; const AValue : TDeleteOrderDealsResponseTypedealsArray); 

begin
  If (Fdeals=AValue) then exit;
  Fdeals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeleteOrderDealsResponse.SetproposalRevisionNumber(AIndex : Integer; const AValue : String); 

begin
  If (FproposalRevisionNumber=AValue) then exit;
  FproposalRevisionNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDeleteOrderDealsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'deals' : SetLength(Fdeals,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDeliveryControl
  --------------------------------------------------------------------}


Procedure TDeliveryControl.SetcreativeBlockingLevel(AIndex : Integer; const AValue : String); 

begin
  If (FcreativeBlockingLevel=AValue) then exit;
  FcreativeBlockingLevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeliveryControl.SetdeliveryRateType(AIndex : Integer; const AValue : String); 

begin
  If (FdeliveryRateType=AValue) then exit;
  FdeliveryRateType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeliveryControl.SetfrequencyCaps(AIndex : Integer; const AValue : TDeliveryControlTypefrequencyCapsArray); 

begin
  If (FfrequencyCaps=AValue) then exit;
  FfrequencyCaps:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDeliveryControl.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'frequencycaps' : SetLength(FfrequencyCaps,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDeliveryControlFrequencyCap
  --------------------------------------------------------------------}


Procedure TDeliveryControlFrequencyCap.SetmaxImpressions(AIndex : Integer; const AValue : integer); 

begin
  If (FmaxImpressions=AValue) then exit;
  FmaxImpressions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeliveryControlFrequencyCap.SetnumTimeUnits(AIndex : Integer; const AValue : integer); 

begin
  If (FnumTimeUnits=AValue) then exit;
  FnumTimeUnits:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeliveryControlFrequencyCap.SettimeUnitType(AIndex : Integer; const AValue : String); 

begin
  If (FtimeUnitType=AValue) then exit;
  FtimeUnitType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDimension
  --------------------------------------------------------------------}


Procedure TDimension.SetdimensionType(AIndex : Integer; const AValue : String); 

begin
  If (FdimensionType=AValue) then exit;
  FdimensionType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimension.SetdimensionValues(AIndex : Integer; const AValue : TDimensionTypedimensionValuesArray); 

begin
  If (FdimensionValues=AValue) then exit;
  FdimensionValues:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDimension.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'dimensionvalues' : SetLength(FdimensionValues,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDimensionDimensionValue
  --------------------------------------------------------------------}


Procedure TDimensionDimensionValue.Setid(AIndex : Integer; const AValue : integer); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionDimensionValue.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEditAllOrderDealsRequest
  --------------------------------------------------------------------}


Procedure TEditAllOrderDealsRequest.Setdeals(AIndex : Integer; const AValue : TEditAllOrderDealsRequestTypedealsArray); 

begin
  If (Fdeals=AValue) then exit;
  Fdeals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEditAllOrderDealsRequest.Setproposal(AIndex : Integer; const AValue : TProposal); 

begin
  If (Fproposal=AValue) then exit;
  Fproposal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEditAllOrderDealsRequest.SetproposalRevisionNumber(AIndex : Integer; const AValue : String); 

begin
  If (FproposalRevisionNumber=AValue) then exit;
  FproposalRevisionNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEditAllOrderDealsRequest.SetupdateAction(AIndex : Integer; const AValue : String); 

begin
  If (FupdateAction=AValue) then exit;
  FupdateAction:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TEditAllOrderDealsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'deals' : SetLength(Fdeals,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEditAllOrderDealsResponse
  --------------------------------------------------------------------}


Procedure TEditAllOrderDealsResponse.Setdeals(AIndex : Integer; const AValue : TEditAllOrderDealsResponseTypedealsArray); 

begin
  If (Fdeals=AValue) then exit;
  Fdeals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEditAllOrderDealsResponse.SetorderRevisionNumber(AIndex : Integer; const AValue : String); 

begin
  If (ForderRevisionNumber=AValue) then exit;
  ForderRevisionNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TEditAllOrderDealsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'deals' : SetLength(Fdeals,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGetOffersResponse
  --------------------------------------------------------------------}


Procedure TGetOffersResponse.Setproducts(AIndex : Integer; const AValue : TGetOffersResponseTypeproductsArray); 

begin
  If (Fproducts=AValue) then exit;
  Fproducts:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGetOffersResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'products' : SetLength(Fproducts,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGetOrderDealsResponse
  --------------------------------------------------------------------}


Procedure TGetOrderDealsResponse.Setdeals(AIndex : Integer; const AValue : TGetOrderDealsResponseTypedealsArray); 

begin
  If (Fdeals=AValue) then exit;
  Fdeals:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGetOrderDealsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'deals' : SetLength(Fdeals,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGetOrderNotesResponse
  --------------------------------------------------------------------}


Procedure TGetOrderNotesResponse.Setnotes(AIndex : Integer; const AValue : TGetOrderNotesResponseTypenotesArray); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGetOrderNotesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'notes' : SetLength(Fnotes,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGetOrdersResponse
  --------------------------------------------------------------------}


Procedure TGetOrdersResponse.Setproposals(AIndex : Integer; const AValue : TGetOrdersResponseTypeproposalsArray); 

begin
  If (Fproposals=AValue) then exit;
  Fproposals:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGetOrdersResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'proposals' : SetLength(Fproposals,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGetPublisherProfilesByAccountIdResponse
  --------------------------------------------------------------------}


Procedure TGetPublisherProfilesByAccountIdResponse.Setprofiles(AIndex : Integer; const AValue : TGetPublisherProfilesByAccountIdResponseTypeprofilesArray); 

begin
  If (Fprofiles=AValue) then exit;
  Fprofiles:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGetPublisherProfilesByAccountIdResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'profiles' : SetLength(Fprofiles,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMarketplaceDeal
  --------------------------------------------------------------------}


Procedure TMarketplaceDeal.SetbuyerPrivateData(AIndex : Integer; const AValue : TPrivateData); 

begin
  If (FbuyerPrivateData=AValue) then exit;
  FbuyerPrivateData:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDeal.SetcreationTimeMs(AIndex : Integer; const AValue : String); 

begin
  If (FcreationTimeMs=AValue) then exit;
  FcreationTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDeal.SetcreativePreApprovalPolicy(AIndex : Integer; const AValue : String); 

begin
  If (FcreativePreApprovalPolicy=AValue) then exit;
  FcreativePreApprovalPolicy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDeal.SetcreativeSafeFrameCompatibility(AIndex : Integer; const AValue : String); 

begin
  If (FcreativeSafeFrameCompatibility=AValue) then exit;
  FcreativeSafeFrameCompatibility:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDeal.SetdealId(AIndex : Integer; const AValue : String); 

begin
  If (FdealId=AValue) then exit;
  FdealId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDeal.SetdealServingMetadata(AIndex : Integer; const AValue : TDealServingMetadata); 

begin
  If (FdealServingMetadata=AValue) then exit;
  FdealServingMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDeal.SetdeliveryControl(AIndex : Integer; const AValue : TDeliveryControl); 

begin
  If (FdeliveryControl=AValue) then exit;
  FdeliveryControl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDeal.SetexternalDealId(AIndex : Integer; const AValue : String); 

begin
  If (FexternalDealId=AValue) then exit;
  FexternalDealId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDeal.SetflightEndTimeMs(AIndex : Integer; const AValue : String); 

begin
  If (FflightEndTimeMs=AValue) then exit;
  FflightEndTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDeal.SetflightStartTimeMs(AIndex : Integer; const AValue : String); 

begin
  If (FflightStartTimeMs=AValue) then exit;
  FflightStartTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDeal.SetinventoryDescription(AIndex : Integer; const AValue : String); 

begin
  If (FinventoryDescription=AValue) then exit;
  FinventoryDescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDeal.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDeal.SetlastUpdateTimeMs(AIndex : Integer; const AValue : String); 

begin
  If (FlastUpdateTimeMs=AValue) then exit;
  FlastUpdateTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDeal.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDeal.SetproductId(AIndex : Integer; const AValue : String); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDeal.SetproductRevisionNumber(AIndex : Integer; const AValue : String); 

begin
  If (FproductRevisionNumber=AValue) then exit;
  FproductRevisionNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDeal.SetprogrammaticCreativeSource(AIndex : Integer; const AValue : String); 

begin
  If (FprogrammaticCreativeSource=AValue) then exit;
  FprogrammaticCreativeSource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDeal.SetproposalId(AIndex : Integer; const AValue : String); 

begin
  If (FproposalId=AValue) then exit;
  FproposalId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDeal.SetsellerContacts(AIndex : Integer; const AValue : TMarketplaceDealTypesellerContactsArray); 

begin
  If (FsellerContacts=AValue) then exit;
  FsellerContacts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDeal.SetsharedTargetings(AIndex : Integer; const AValue : TMarketplaceDealTypesharedTargetingsArray); 

begin
  If (FsharedTargetings=AValue) then exit;
  FsharedTargetings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDeal.SetsyndicationProduct(AIndex : Integer; const AValue : String); 

begin
  If (FsyndicationProduct=AValue) then exit;
  FsyndicationProduct:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDeal.Setterms(AIndex : Integer; const AValue : TDealTerms); 

begin
  If (Fterms=AValue) then exit;
  Fterms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDeal.SetwebPropertyCode(AIndex : Integer; const AValue : String); 

begin
  If (FwebPropertyCode=AValue) then exit;
  FwebPropertyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TMarketplaceDeal.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sellercontacts' : SetLength(FsellerContacts,ALength);
  'sharedtargetings' : SetLength(FsharedTargetings,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMarketplaceDealParty
  --------------------------------------------------------------------}


Procedure TMarketplaceDealParty.Setbuyer(AIndex : Integer; const AValue : TBuyer); 

begin
  If (Fbuyer=AValue) then exit;
  Fbuyer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceDealParty.Setseller(AIndex : Integer; const AValue : TSeller); 

begin
  If (Fseller=AValue) then exit;
  Fseller:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMarketplaceLabel
  --------------------------------------------------------------------}


Procedure TMarketplaceLabel.SetaccountId(AIndex : Integer; const AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceLabel.SetcreateTimeMs(AIndex : Integer; const AValue : String); 

begin
  If (FcreateTimeMs=AValue) then exit;
  FcreateTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceLabel.SetdeprecatedMarketplaceDealParty(AIndex : Integer; const AValue : TMarketplaceDealParty); 

begin
  If (FdeprecatedMarketplaceDealParty=AValue) then exit;
  FdeprecatedMarketplaceDealParty:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceLabel.Set_label(AIndex : Integer; const AValue : String); 

begin
  If (F_label=AValue) then exit;
  F_label:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TMarketplaceLabel.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_label' : Result:='label';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TMarketplaceNote
  --------------------------------------------------------------------}


Procedure TMarketplaceNote.SetcreatorRole(AIndex : Integer; const AValue : String); 

begin
  If (FcreatorRole=AValue) then exit;
  FcreatorRole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceNote.SetdealId(AIndex : Integer; const AValue : String); 

begin
  If (FdealId=AValue) then exit;
  FdealId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceNote.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceNote.Setnote(AIndex : Integer; const AValue : String); 

begin
  If (Fnote=AValue) then exit;
  Fnote:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceNote.SetnoteId(AIndex : Integer; const AValue : String); 

begin
  If (FnoteId=AValue) then exit;
  FnoteId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceNote.SetproposalId(AIndex : Integer; const AValue : String); 

begin
  If (FproposalId=AValue) then exit;
  FproposalId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceNote.SetproposalRevisionNumber(AIndex : Integer; const AValue : String); 

begin
  If (FproposalRevisionNumber=AValue) then exit;
  FproposalRevisionNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMarketplaceNote.SettimestampMs(AIndex : Integer; const AValue : String); 

begin
  If (FtimestampMs=AValue) then exit;
  FtimestampMs:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPerformanceReport
  --------------------------------------------------------------------}


Procedure TPerformanceReport.SetbidRate(AIndex : Integer; const AValue : double); 

begin
  If (FbidRate=AValue) then exit;
  FbidRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetbidRequestRate(AIndex : Integer; const AValue : double); 

begin
  If (FbidRequestRate=AValue) then exit;
  FbidRequestRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetcalloutStatusRate(AIndex : Integer; const AValue : TTJSONSchemaArray); 

begin
  If (FcalloutStatusRate=AValue) then exit;
  FcalloutStatusRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetcookieMatcherStatusRate(AIndex : Integer; const AValue : TTJSONSchemaArray); 

begin
  If (FcookieMatcherStatusRate=AValue) then exit;
  FcookieMatcherStatusRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetcreativeStatusRate(AIndex : Integer; const AValue : TTJSONSchemaArray); 

begin
  If (FcreativeStatusRate=AValue) then exit;
  FcreativeStatusRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetfilteredBidRate(AIndex : Integer; const AValue : double); 

begin
  If (FfilteredBidRate=AValue) then exit;
  FfilteredBidRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SethostedMatchStatusRate(AIndex : Integer; const AValue : TTJSONSchemaArray); 

begin
  If (FhostedMatchStatusRate=AValue) then exit;
  FhostedMatchStatusRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetinventoryMatchRate(AIndex : Integer; const AValue : double); 

begin
  If (FinventoryMatchRate=AValue) then exit;
  FinventoryMatchRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.Setlatency50thPercentile(AIndex : Integer; const AValue : double); 

begin
  If (Flatency50thPercentile=AValue) then exit;
  Flatency50thPercentile:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.Setlatency85thPercentile(AIndex : Integer; const AValue : double); 

begin
  If (Flatency85thPercentile=AValue) then exit;
  Flatency85thPercentile:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.Setlatency95thPercentile(AIndex : Integer; const AValue : double); 

begin
  If (Flatency95thPercentile=AValue) then exit;
  Flatency95thPercentile:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetnoQuotaInRegion(AIndex : Integer; const AValue : double); 

begin
  If (FnoQuotaInRegion=AValue) then exit;
  FnoQuotaInRegion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetoutOfQuota(AIndex : Integer; const AValue : double); 

begin
  If (FoutOfQuota=AValue) then exit;
  FoutOfQuota:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetpixelMatchRequests(AIndex : Integer; const AValue : double); 

begin
  If (FpixelMatchRequests=AValue) then exit;
  FpixelMatchRequests:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetpixelMatchResponses(AIndex : Integer; const AValue : double); 

begin
  If (FpixelMatchResponses=AValue) then exit;
  FpixelMatchResponses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetquotaConfiguredLimit(AIndex : Integer; const AValue : double); 

begin
  If (FquotaConfiguredLimit=AValue) then exit;
  FquotaConfiguredLimit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetquotaThrottledLimit(AIndex : Integer; const AValue : double); 

begin
  If (FquotaThrottledLimit=AValue) then exit;
  FquotaThrottledLimit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.Setregion(AIndex : Integer; const AValue : String); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetsuccessfulRequestRate(AIndex : Integer; const AValue : double); 

begin
  If (FsuccessfulRequestRate=AValue) then exit;
  FsuccessfulRequestRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.Settimestamp(AIndex : Integer; const AValue : String); 

begin
  If (Ftimestamp=AValue) then exit;
  Ftimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReport.SetunsuccessfulRequestRate(AIndex : Integer; const AValue : double); 

begin
  If (FunsuccessfulRequestRate=AValue) then exit;
  FunsuccessfulRequestRate:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPerformanceReport.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'calloutstatusrate' : SetLength(FcalloutStatusRate,ALength);
  'cookiematcherstatusrate' : SetLength(FcookieMatcherStatusRate,ALength);
  'creativestatusrate' : SetLength(FcreativeStatusRate,ALength);
  'hostedmatchstatusrate' : SetLength(FhostedMatchStatusRate,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPerformanceReportList
  --------------------------------------------------------------------}


Procedure TPerformanceReportList.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerformanceReportList.SetperformanceReport(AIndex : Integer; const AValue : TPerformanceReportListTypeperformanceReportArray); 

begin
  If (FperformanceReport=AValue) then exit;
  FperformanceReport:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPerformanceReportList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'performancereport' : SetLength(FperformanceReport,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPretargetingConfigTypedimensionsItem
  --------------------------------------------------------------------}


Procedure TPretargetingConfigTypedimensionsItem.Setheight(AIndex : Integer; const AValue : String); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfigTypedimensionsItem.Setwidth(AIndex : Integer; const AValue : String); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPretargetingConfigTypeexcludedPlacementsItem
  --------------------------------------------------------------------}


Procedure TPretargetingConfigTypeexcludedPlacementsItem.Settoken(AIndex : Integer; const AValue : String); 

begin
  If (Ftoken=AValue) then exit;
  Ftoken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfigTypeexcludedPlacementsItem.Set_type(AIndex : Integer; const AValue : String); 

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


Procedure TPretargetingConfigTypeplacementsItem.Settoken(AIndex : Integer; const AValue : String); 

begin
  If (Ftoken=AValue) then exit;
  Ftoken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfigTypeplacementsItem.Set_type(AIndex : Integer; const AValue : String); 

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
  TPretargetingConfigTypevideoPlayerSizesItem
  --------------------------------------------------------------------}


Procedure TPretargetingConfigTypevideoPlayerSizesItem.SetaspectRatio(AIndex : Integer; const AValue : String); 

begin
  If (FaspectRatio=AValue) then exit;
  FaspectRatio:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfigTypevideoPlayerSizesItem.SetminHeight(AIndex : Integer; const AValue : String); 

begin
  If (FminHeight=AValue) then exit;
  FminHeight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfigTypevideoPlayerSizesItem.SetminWidth(AIndex : Integer; const AValue : String); 

begin
  If (FminWidth=AValue) then exit;
  FminWidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPretargetingConfig
  --------------------------------------------------------------------}


Procedure TPretargetingConfig.SetbillingId(AIndex : Integer; const AValue : String); 

begin
  If (FbillingId=AValue) then exit;
  FbillingId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetconfigId(AIndex : Integer; const AValue : String); 

begin
  If (FconfigId=AValue) then exit;
  FconfigId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetconfigName(AIndex : Integer; const AValue : String); 

begin
  If (FconfigName=AValue) then exit;
  FconfigName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetcreativeType(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FcreativeType=AValue) then exit;
  FcreativeType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.Setdimensions(AIndex : Integer; const AValue : TPretargetingConfigTypedimensionsArray); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetexcludedContentLabels(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FexcludedContentLabels=AValue) then exit;
  FexcludedContentLabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetexcludedGeoCriteriaIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FexcludedGeoCriteriaIds=AValue) then exit;
  FexcludedGeoCriteriaIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetexcludedPlacements(AIndex : Integer; const AValue : TPretargetingConfigTypeexcludedPlacementsArray); 

begin
  If (FexcludedPlacements=AValue) then exit;
  FexcludedPlacements:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetexcludedUserLists(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FexcludedUserLists=AValue) then exit;
  FexcludedUserLists:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetexcludedVerticals(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FexcludedVerticals=AValue) then exit;
  FexcludedVerticals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetgeoCriteriaIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FgeoCriteriaIds=AValue) then exit;
  FgeoCriteriaIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetisActive(AIndex : Integer; const AValue : boolean); 

begin
  If (FisActive=AValue) then exit;
  FisActive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.Setlanguages(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Flanguages=AValue) then exit;
  Flanguages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetmobileCarriers(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FmobileCarriers=AValue) then exit;
  FmobileCarriers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetmobileDevices(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FmobileDevices=AValue) then exit;
  FmobileDevices:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetmobileOperatingSystemVersions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FmobileOperatingSystemVersions=AValue) then exit;
  FmobileOperatingSystemVersions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.Setplacements(AIndex : Integer; const AValue : TPretargetingConfigTypeplacementsArray); 

begin
  If (Fplacements=AValue) then exit;
  Fplacements:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.Setplatforms(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fplatforms=AValue) then exit;
  Fplatforms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetsupportedCreativeAttributes(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FsupportedCreativeAttributes=AValue) then exit;
  FsupportedCreativeAttributes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetuserLists(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FuserLists=AValue) then exit;
  FuserLists:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetvendorTypes(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FvendorTypes=AValue) then exit;
  FvendorTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.Setverticals(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fverticals=AValue) then exit;
  Fverticals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfig.SetvideoPlayerSizes(AIndex : Integer; const AValue : TPretargetingConfigTypevideoPlayerSizesArray); 

begin
  If (FvideoPlayerSizes=AValue) then exit;
  FvideoPlayerSizes:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPretargetingConfig.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'creativetype' : SetLength(FcreativeType,ALength);
  'dimensions' : SetLength(Fdimensions,ALength);
  'excludedcontentlabels' : SetLength(FexcludedContentLabels,ALength);
  'excludedgeocriteriaids' : SetLength(FexcludedGeoCriteriaIds,ALength);
  'excludedplacements' : SetLength(FexcludedPlacements,ALength);
  'excludeduserlists' : SetLength(FexcludedUserLists,ALength);
  'excludedverticals' : SetLength(FexcludedVerticals,ALength);
  'geocriteriaids' : SetLength(FgeoCriteriaIds,ALength);
  'languages' : SetLength(Flanguages,ALength);
  'mobilecarriers' : SetLength(FmobileCarriers,ALength);
  'mobiledevices' : SetLength(FmobileDevices,ALength);
  'mobileoperatingsystemversions' : SetLength(FmobileOperatingSystemVersions,ALength);
  'placements' : SetLength(Fplacements,ALength);
  'platforms' : SetLength(Fplatforms,ALength);
  'supportedcreativeattributes' : SetLength(FsupportedCreativeAttributes,ALength);
  'userlists' : SetLength(FuserLists,ALength);
  'vendortypes' : SetLength(FvendorTypes,ALength);
  'verticals' : SetLength(Fverticals,ALength);
  'videoplayersizes' : SetLength(FvideoPlayerSizes,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPretargetingConfigList
  --------------------------------------------------------------------}


Procedure TPretargetingConfigList.Setitems(AIndex : Integer; const AValue : TPretargetingConfigListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPretargetingConfigList.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPretargetingConfigList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPrice
  --------------------------------------------------------------------}


Procedure TPrice.SetamountMicros(AIndex : Integer; const AValue : double); 

begin
  If (FamountMicros=AValue) then exit;
  FamountMicros:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPrice.SetcurrencyCode(AIndex : Integer; const AValue : String); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPrice.SetpricingType(AIndex : Integer; const AValue : String); 

begin
  If (FpricingType=AValue) then exit;
  FpricingType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPricePerBuyer
  --------------------------------------------------------------------}


Procedure TPricePerBuyer.SetauctionTier(AIndex : Integer; const AValue : String); 

begin
  If (FauctionTier=AValue) then exit;
  FauctionTier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricePerBuyer.Setbuyer(AIndex : Integer; const AValue : TBuyer); 

begin
  If (Fbuyer=AValue) then exit;
  Fbuyer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricePerBuyer.Setprice(AIndex : Integer; const AValue : TPrice); 

begin
  If (Fprice=AValue) then exit;
  Fprice:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPrivateData
  --------------------------------------------------------------------}


Procedure TPrivateData.SetreferenceId(AIndex : Integer; const AValue : String); 

begin
  If (FreferenceId=AValue) then exit;
  FreferenceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPrivateData.SetreferencePayload(AIndex : Integer; const AValue : String); 

begin
  If (FreferencePayload=AValue) then exit;
  FreferencePayload:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProduct
  --------------------------------------------------------------------}


Procedure TProduct.SetcreationTimeMs(AIndex : Integer; const AValue : String); 

begin
  If (FcreationTimeMs=AValue) then exit;
  FcreationTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetcreatorContacts(AIndex : Integer; const AValue : TProductTypecreatorContactsArray); 

begin
  If (FcreatorContacts=AValue) then exit;
  FcreatorContacts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetdeliveryControl(AIndex : Integer; const AValue : TDeliveryControl); 

begin
  If (FdeliveryControl=AValue) then exit;
  FdeliveryControl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetflightEndTimeMs(AIndex : Integer; const AValue : String); 

begin
  If (FflightEndTimeMs=AValue) then exit;
  FflightEndTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetflightStartTimeMs(AIndex : Integer; const AValue : String); 

begin
  If (FflightStartTimeMs=AValue) then exit;
  FflightStartTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SethasCreatorSignedOff(AIndex : Integer; const AValue : boolean); 

begin
  If (FhasCreatorSignedOff=AValue) then exit;
  FhasCreatorSignedOff:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetinventorySource(AIndex : Integer; const AValue : String); 

begin
  If (FinventorySource=AValue) then exit;
  FinventorySource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setlabels(AIndex : Integer; const AValue : TProductTypelabelsArray); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetlastUpdateTimeMs(AIndex : Integer; const AValue : String); 

begin
  If (FlastUpdateTimeMs=AValue) then exit;
  FlastUpdateTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetlegacyOfferId(AIndex : Integer; const AValue : String); 

begin
  If (FlegacyOfferId=AValue) then exit;
  FlegacyOfferId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetprivateAuctionId(AIndex : Integer; const AValue : String); 

begin
  If (FprivateAuctionId=AValue) then exit;
  FprivateAuctionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetproductId(AIndex : Integer; const AValue : String); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetpublisherProfileId(AIndex : Integer; const AValue : String); 

begin
  If (FpublisherProfileId=AValue) then exit;
  FpublisherProfileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetpublisherProvidedForecast(AIndex : Integer; const AValue : TPublisherProvidedForecast); 

begin
  If (FpublisherProvidedForecast=AValue) then exit;
  FpublisherProvidedForecast:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetrevisionNumber(AIndex : Integer; const AValue : String); 

begin
  If (FrevisionNumber=AValue) then exit;
  FrevisionNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setseller(AIndex : Integer; const AValue : TSeller); 

begin
  If (Fseller=AValue) then exit;
  Fseller:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetsharedTargetings(AIndex : Integer; const AValue : TProductTypesharedTargetingsArray); 

begin
  If (FsharedTargetings=AValue) then exit;
  FsharedTargetings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setstate(AIndex : Integer; const AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetsyndicationProduct(AIndex : Integer; const AValue : String); 

begin
  If (FsyndicationProduct=AValue) then exit;
  FsyndicationProduct:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setterms(AIndex : Integer; const AValue : TDealTerms); 

begin
  If (Fterms=AValue) then exit;
  Fterms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetwebPropertyCode(AIndex : Integer; const AValue : String); 

begin
  If (FwebPropertyCode=AValue) then exit;
  FwebPropertyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TProduct.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'creatorcontacts' : SetLength(FcreatorContacts,ALength);
  'labels' : SetLength(Flabels,ALength);
  'sharedtargetings' : SetLength(FsharedTargetings,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TProposal
  --------------------------------------------------------------------}


Procedure TProposal.SetbilledBuyer(AIndex : Integer; const AValue : TBuyer); 

begin
  If (FbilledBuyer=AValue) then exit;
  FbilledBuyer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProposal.Setbuyer(AIndex : Integer; const AValue : TBuyer); 

begin
  If (Fbuyer=AValue) then exit;
  Fbuyer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProposal.SetbuyerContacts(AIndex : Integer; const AValue : TProposalTypebuyerContactsArray); 

begin
  If (FbuyerContacts=AValue) then exit;
  FbuyerContacts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProposal.SetbuyerPrivateData(AIndex : Integer; const AValue : TPrivateData); 

begin
  If (FbuyerPrivateData=AValue) then exit;
  FbuyerPrivateData:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProposal.SethasBuyerSignedOff(AIndex : Integer; const AValue : boolean); 

begin
  If (FhasBuyerSignedOff=AValue) then exit;
  FhasBuyerSignedOff:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProposal.SethasSellerSignedOff(AIndex : Integer; const AValue : boolean); 

begin
  If (FhasSellerSignedOff=AValue) then exit;
  FhasSellerSignedOff:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProposal.SetinventorySource(AIndex : Integer; const AValue : String); 

begin
  If (FinventorySource=AValue) then exit;
  FinventorySource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProposal.SetisRenegotiating(AIndex : Integer; const AValue : boolean); 

begin
  If (FisRenegotiating=AValue) then exit;
  FisRenegotiating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProposal.SetisSetupComplete(AIndex : Integer; const AValue : boolean); 

begin
  If (FisSetupComplete=AValue) then exit;
  FisSetupComplete:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProposal.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProposal.Setlabels(AIndex : Integer; const AValue : TProposalTypelabelsArray); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProposal.SetlastUpdaterOrCommentorRole(AIndex : Integer; const AValue : String); 

begin
  If (FlastUpdaterOrCommentorRole=AValue) then exit;
  FlastUpdaterOrCommentorRole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProposal.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProposal.SetnegotiationId(AIndex : Integer; const AValue : String); 

begin
  If (FnegotiationId=AValue) then exit;
  FnegotiationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProposal.SetoriginatorRole(AIndex : Integer; const AValue : String); 

begin
  If (ForiginatorRole=AValue) then exit;
  ForiginatorRole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProposal.SetprivateAuctionId(AIndex : Integer; const AValue : String); 

begin
  If (FprivateAuctionId=AValue) then exit;
  FprivateAuctionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProposal.SetproposalId(AIndex : Integer; const AValue : String); 

begin
  If (FproposalId=AValue) then exit;
  FproposalId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProposal.SetproposalState(AIndex : Integer; const AValue : String); 

begin
  If (FproposalState=AValue) then exit;
  FproposalState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProposal.SetrevisionNumber(AIndex : Integer; const AValue : String); 

begin
  If (FrevisionNumber=AValue) then exit;
  FrevisionNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProposal.SetrevisionTimeMs(AIndex : Integer; const AValue : String); 

begin
  If (FrevisionTimeMs=AValue) then exit;
  FrevisionTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProposal.Setseller(AIndex : Integer; const AValue : TSeller); 

begin
  If (Fseller=AValue) then exit;
  Fseller:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProposal.SetsellerContacts(AIndex : Integer; const AValue : TProposalTypesellerContactsArray); 

begin
  If (FsellerContacts=AValue) then exit;
  FsellerContacts:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TProposal.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'buyercontacts' : SetLength(FbuyerContacts,ALength);
  'labels' : SetLength(Flabels,ALength);
  'sellercontacts' : SetLength(FsellerContacts,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPublisherProfileApiProto
  --------------------------------------------------------------------}


Procedure TPublisherProfileApiProto.SetaccountId(AIndex : Integer; const AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProfileApiProto.Setaudience(AIndex : Integer; const AValue : String); 

begin
  If (Faudience=AValue) then exit;
  Faudience:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProfileApiProto.SetbuyerPitchStatement(AIndex : Integer; const AValue : String); 

begin
  If (FbuyerPitchStatement=AValue) then exit;
  FbuyerPitchStatement:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProfileApiProto.SetdirectContact(AIndex : Integer; const AValue : String); 

begin
  If (FdirectContact=AValue) then exit;
  FdirectContact:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProfileApiProto.Setexchange(AIndex : Integer; const AValue : String); 

begin
  If (Fexchange=AValue) then exit;
  Fexchange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProfileApiProto.SetgooglePlusLink(AIndex : Integer; const AValue : String); 

begin
  If (FgooglePlusLink=AValue) then exit;
  FgooglePlusLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProfileApiProto.SetisParent(AIndex : Integer; const AValue : boolean); 

begin
  If (FisParent=AValue) then exit;
  FisParent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProfileApiProto.SetisPublished(AIndex : Integer; const AValue : boolean); 

begin
  If (FisPublished=AValue) then exit;
  FisPublished:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProfileApiProto.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProfileApiProto.SetlogoUrl(AIndex : Integer; const AValue : String); 

begin
  If (FlogoUrl=AValue) then exit;
  FlogoUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProfileApiProto.SetmediaKitLink(AIndex : Integer; const AValue : String); 

begin
  If (FmediaKitLink=AValue) then exit;
  FmediaKitLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProfileApiProto.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProfileApiProto.Setoverview(AIndex : Integer; const AValue : String); 

begin
  If (Foverview=AValue) then exit;
  Foverview:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProfileApiProto.SetprofileId(AIndex : Integer; const AValue : integer); 

begin
  If (FprofileId=AValue) then exit;
  FprofileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProfileApiProto.SetprogrammaticContact(AIndex : Integer; const AValue : String); 

begin
  If (FprogrammaticContact=AValue) then exit;
  FprogrammaticContact:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProfileApiProto.SetpublisherDomains(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FpublisherDomains=AValue) then exit;
  FpublisherDomains:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProfileApiProto.SetpublisherProfileId(AIndex : Integer; const AValue : String); 

begin
  If (FpublisherProfileId=AValue) then exit;
  FpublisherProfileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProfileApiProto.SetpublisherProvidedForecast(AIndex : Integer; const AValue : TPublisherProvidedForecast); 

begin
  If (FpublisherProvidedForecast=AValue) then exit;
  FpublisherProvidedForecast:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProfileApiProto.SetrateCardInfoLink(AIndex : Integer; const AValue : String); 

begin
  If (FrateCardInfoLink=AValue) then exit;
  FrateCardInfoLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProfileApiProto.SetsamplePageLink(AIndex : Integer; const AValue : String); 

begin
  If (FsamplePageLink=AValue) then exit;
  FsamplePageLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProfileApiProto.Setseller(AIndex : Integer; const AValue : TSeller); 

begin
  If (Fseller=AValue) then exit;
  Fseller:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProfileApiProto.Setstate(AIndex : Integer; const AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProfileApiProto.SettopHeadlines(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FtopHeadlines=AValue) then exit;
  FtopHeadlines:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPublisherProfileApiProto.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'publisherdomains' : SetLength(FpublisherDomains,ALength);
  'topheadlines' : SetLength(FtopHeadlines,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPublisherProvidedForecast
  --------------------------------------------------------------------}


Procedure TPublisherProvidedForecast.Setdimensions(AIndex : Integer; const AValue : TPublisherProvidedForecastTypedimensionsArray); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProvidedForecast.SetweeklyImpressions(AIndex : Integer; const AValue : String); 

begin
  If (FweeklyImpressions=AValue) then exit;
  FweeklyImpressions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublisherProvidedForecast.SetweeklyUniques(AIndex : Integer; const AValue : String); 

begin
  If (FweeklyUniques=AValue) then exit;
  FweeklyUniques:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPublisherProvidedForecast.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'dimensions' : SetLength(Fdimensions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSeller
  --------------------------------------------------------------------}


Procedure TSeller.SetaccountId(AIndex : Integer; const AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeller.SetsubAccountId(AIndex : Integer; const AValue : String); 

begin
  If (FsubAccountId=AValue) then exit;
  FsubAccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSharedTargeting
  --------------------------------------------------------------------}


Procedure TSharedTargeting.Setexclusions(AIndex : Integer; const AValue : TSharedTargetingTypeexclusionsArray); 

begin
  If (Fexclusions=AValue) then exit;
  Fexclusions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSharedTargeting.Setinclusions(AIndex : Integer; const AValue : TSharedTargetingTypeinclusionsArray); 

begin
  If (Finclusions=AValue) then exit;
  Finclusions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSharedTargeting.Setkey(AIndex : Integer; const AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSharedTargeting.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'exclusions' : SetLength(Fexclusions,ALength);
  'inclusions' : SetLength(Finclusions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTargetingValue
  --------------------------------------------------------------------}


Procedure TTargetingValue.SetcreativeSizeValue(AIndex : Integer; const AValue : TTargetingValueCreativeSize); 

begin
  If (FcreativeSizeValue=AValue) then exit;
  FcreativeSizeValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetingValue.SetdayPartTargetingValue(AIndex : Integer; const AValue : TTargetingValueDayPartTargeting); 

begin
  If (FdayPartTargetingValue=AValue) then exit;
  FdayPartTargetingValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetingValue.SetlongValue(AIndex : Integer; const AValue : String); 

begin
  If (FlongValue=AValue) then exit;
  FlongValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetingValue.SetstringValue(AIndex : Integer; const AValue : String); 

begin
  If (FstringValue=AValue) then exit;
  FstringValue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetingValueCreativeSize
  --------------------------------------------------------------------}


Procedure TTargetingValueCreativeSize.SetcompanionSizes(AIndex : Integer; const AValue : TTargetingValueCreativeSizeTypecompanionSizesArray); 

begin
  If (FcompanionSizes=AValue) then exit;
  FcompanionSizes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetingValueCreativeSize.SetcreativeSizeType(AIndex : Integer; const AValue : String); 

begin
  If (FcreativeSizeType=AValue) then exit;
  FcreativeSizeType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetingValueCreativeSize.Setsize(AIndex : Integer; const AValue : TTargetingValueSize); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTargetingValueCreativeSize.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'companionsizes' : SetLength(FcompanionSizes,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTargetingValueDayPartTargeting
  --------------------------------------------------------------------}


Procedure TTargetingValueDayPartTargeting.SetdayParts(AIndex : Integer; const AValue : TTargetingValueDayPartTargetingTypedayPartsArray); 

begin
  If (FdayParts=AValue) then exit;
  FdayParts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetingValueDayPartTargeting.SettimeZoneType(AIndex : Integer; const AValue : String); 

begin
  If (FtimeZoneType=AValue) then exit;
  FtimeZoneType:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTargetingValueDayPartTargeting.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'dayparts' : SetLength(FdayParts,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTargetingValueDayPartTargetingDayPart
  --------------------------------------------------------------------}


Procedure TTargetingValueDayPartTargetingDayPart.SetdayOfWeek(AIndex : Integer; const AValue : String); 

begin
  If (FdayOfWeek=AValue) then exit;
  FdayOfWeek:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetingValueDayPartTargetingDayPart.SetendHour(AIndex : Integer; const AValue : integer); 

begin
  If (FendHour=AValue) then exit;
  FendHour:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetingValueDayPartTargetingDayPart.SetendMinute(AIndex : Integer; const AValue : integer); 

begin
  If (FendMinute=AValue) then exit;
  FendMinute:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetingValueDayPartTargetingDayPart.SetstartHour(AIndex : Integer; const AValue : integer); 

begin
  If (FstartHour=AValue) then exit;
  FstartHour:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetingValueDayPartTargetingDayPart.SetstartMinute(AIndex : Integer; const AValue : integer); 

begin
  If (FstartMinute=AValue) then exit;
  FstartMinute:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetingValueSize
  --------------------------------------------------------------------}


Procedure TTargetingValueSize.Setheight(AIndex : Integer; const AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetingValueSize.Setwidth(AIndex : Integer; const AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUpdatePrivateAuctionProposalRequest
  --------------------------------------------------------------------}


Procedure TUpdatePrivateAuctionProposalRequest.SetexternalDealId(AIndex : Integer; const AValue : String); 

begin
  If (FexternalDealId=AValue) then exit;
  FexternalDealId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdatePrivateAuctionProposalRequest.Setnote(AIndex : Integer; const AValue : TMarketplaceNote); 

begin
  If (Fnote=AValue) then exit;
  Fnote:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdatePrivateAuctionProposalRequest.SetproposalRevisionNumber(AIndex : Integer; const AValue : String); 

begin
  If (FproposalRevisionNumber=AValue) then exit;
  FproposalRevisionNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdatePrivateAuctionProposalRequest.SetupdateAction(AIndex : Integer; const AValue : String); 

begin
  If (FupdateAction=AValue) then exit;
  FupdateAction:=AValue;
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

Procedure TCreativesResource.AddDeal(accountId: integer; buyerCreativeId: string; dealId: string);

Const
  _HTTPMethod = 'POST';
  _Path       = 'creatives/{accountId}/{buyerCreativeId}/addDeal/{dealId}';
  _Methodid   = 'adexchangebuyer.creatives.addDeal';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'buyerCreativeId',buyerCreativeId,'dealId',dealId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
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
  AddToQuery(_Q,'dealsStatusFilter',AQuery.dealsStatusFilter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'openAuctionStatusFilter',AQuery.openAuctionStatusFilter);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_Q);
end;

Procedure TCreativesResource.RemoveDeal(accountId: integer; buyerCreativeId: string; dealId: string);

Const
  _HTTPMethod = 'POST';
  _Path       = 'creatives/{accountId}/{buyerCreativeId}/removeDeal/{dealId}';
  _Methodid   = 'adexchangebuyer.creatives.removeDeal';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'buyerCreativeId',buyerCreativeId,'dealId',dealId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;



{ --------------------------------------------------------------------
  TMarketplacedealsResource
  --------------------------------------------------------------------}


Class Function TMarketplacedealsResource.ResourceName : String;

begin
  Result:='marketplacedeals';
end;

Class Function TMarketplacedealsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangebuyerAPI;
end;

Function TMarketplacedealsResource.Delete(proposalId: string; aDeleteOrderDealsRequest : TDeleteOrderDealsRequest) : TDeleteOrderDealsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'proposals/{proposalId}/deals/delete';
  _Methodid   = 'adexchangebuyer.marketplacedeals.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['proposalId',proposalId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDeleteOrderDealsRequest,TDeleteOrderDealsResponse) as TDeleteOrderDealsResponse;
end;

Function TMarketplacedealsResource.Insert(proposalId: string; aAddOrderDealsRequest : TAddOrderDealsRequest) : TAddOrderDealsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'proposals/{proposalId}/deals/insert';
  _Methodid   = 'adexchangebuyer.marketplacedeals.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['proposalId',proposalId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAddOrderDealsRequest,TAddOrderDealsResponse) as TAddOrderDealsResponse;
end;

Function TMarketplacedealsResource.List(proposalId: string; AQuery : string = '') : TGetOrderDealsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'proposals/{proposalId}/deals';
  _Methodid   = 'adexchangebuyer.marketplacedeals.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['proposalId',proposalId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TGetOrderDealsResponse) as TGetOrderDealsResponse;
end;


Function TMarketplacedealsResource.List(proposalId: string; AQuery : TMarketplacedealslistOptions) : TGetOrderDealsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pqlQuery',AQuery.pqlQuery);
  Result:=List(proposalId,_Q);
end;

Function TMarketplacedealsResource.Update(proposalId: string; aEditAllOrderDealsRequest : TEditAllOrderDealsRequest) : TEditAllOrderDealsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'proposals/{proposalId}/deals/update';
  _Methodid   = 'adexchangebuyer.marketplacedeals.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['proposalId',proposalId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aEditAllOrderDealsRequest,TEditAllOrderDealsResponse) as TEditAllOrderDealsResponse;
end;



{ --------------------------------------------------------------------
  TMarketplacenotesResource
  --------------------------------------------------------------------}


Class Function TMarketplacenotesResource.ResourceName : String;

begin
  Result:='marketplacenotes';
end;

Class Function TMarketplacenotesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangebuyerAPI;
end;

Function TMarketplacenotesResource.Insert(proposalId: string; aAddOrderNotesRequest : TAddOrderNotesRequest) : TAddOrderNotesResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'proposals/{proposalId}/notes/insert';
  _Methodid   = 'adexchangebuyer.marketplacenotes.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['proposalId',proposalId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAddOrderNotesRequest,TAddOrderNotesResponse) as TAddOrderNotesResponse;
end;

Function TMarketplacenotesResource.List(proposalId: string) : TGetOrderNotesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'proposals/{proposalId}/notes';
  _Methodid   = 'adexchangebuyer.marketplacenotes.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['proposalId',proposalId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TGetOrderNotesResponse) as TGetOrderNotesResponse;
end;



{ --------------------------------------------------------------------
  TMarketplaceprivateauctionResource
  --------------------------------------------------------------------}


Class Function TMarketplaceprivateauctionResource.ResourceName : String;

begin
  Result:='marketplaceprivateauction';
end;

Class Function TMarketplaceprivateauctionResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangebuyerAPI;
end;

Procedure TMarketplaceprivateauctionResource.Updateproposal(privateAuctionId: string; aUpdatePrivateAuctionProposalRequest : TUpdatePrivateAuctionProposalRequest);

Const
  _HTTPMethod = 'POST';
  _Path       = 'privateauction/{privateAuctionId}/updateproposal';
  _Methodid   = 'adexchangebuyer.marketplaceprivateauction.updateproposal';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['privateAuctionId',privateAuctionId]);
  ServiceCall(_HTTPMethod,_P,'',aUpdatePrivateAuctionProposalRequest,Nil);
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
  TProductsResource
  --------------------------------------------------------------------}


Class Function TProductsResource.ResourceName : String;

begin
  Result:='products';
end;

Class Function TProductsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangebuyerAPI;
end;

Function TProductsResource.Get(productId: string) : TProduct;

Const
  _HTTPMethod = 'GET';
  _Path       = 'products/{productId}';
  _Methodid   = 'adexchangebuyer.products.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['productId',productId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TProduct) as TProduct;
end;

Function TProductsResource.Search(AQuery : string = '') : TGetOffersResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'products/search';
  _Methodid   = 'adexchangebuyer.products.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TGetOffersResponse) as TGetOffersResponse;
end;


Function TProductsResource.Search(AQuery : TProductssearchOptions) : TGetOffersResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pqlQuery',AQuery.pqlQuery);
  Result:=Search(_Q);
end;



{ --------------------------------------------------------------------
  TProposalsResource
  --------------------------------------------------------------------}


Class Function TProposalsResource.ResourceName : String;

begin
  Result:='proposals';
end;

Class Function TProposalsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangebuyerAPI;
end;

Function TProposalsResource.Get(proposalId: string) : TProposal;

Const
  _HTTPMethod = 'GET';
  _Path       = 'proposals/{proposalId}';
  _Methodid   = 'adexchangebuyer.proposals.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['proposalId',proposalId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TProposal) as TProposal;
end;

Function TProposalsResource.Insert(aCreateOrdersRequest : TCreateOrdersRequest) : TCreateOrdersResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'proposals/insert';
  _Methodid   = 'adexchangebuyer.proposals.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aCreateOrdersRequest,TCreateOrdersResponse) as TCreateOrdersResponse;
end;

Function TProposalsResource.Patch(proposalId: string; revisionNumber: string; _updateAction: string; aProposal : TProposal) : TProposal;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'proposals/{proposalId}/{revisionNumber}/{updateAction}';
  _Methodid   = 'adexchangebuyer.proposals.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['proposalId',proposalId,'revisionNumber',revisionNumber,'updateAction',_updateAction]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aProposal,TProposal) as TProposal;
end;

Function TProposalsResource.Search(AQuery : string = '') : TGetOrdersResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'proposals/search';
  _Methodid   = 'adexchangebuyer.proposals.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TGetOrdersResponse) as TGetOrdersResponse;
end;


Function TProposalsResource.Search(AQuery : TProposalssearchOptions) : TGetOrdersResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pqlQuery',AQuery.pqlQuery);
  Result:=Search(_Q);
end;

Procedure TProposalsResource.Setupcomplete(proposalId: string);

Const
  _HTTPMethod = 'POST';
  _Path       = 'proposals/{proposalId}/setupcomplete';
  _Methodid   = 'adexchangebuyer.proposals.setupcomplete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['proposalId',proposalId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TProposalsResource.Update(proposalId: string; revisionNumber: string; _updateAction: string; aProposal : TProposal) : TProposal;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'proposals/{proposalId}/{revisionNumber}/{updateAction}';
  _Methodid   = 'adexchangebuyer.proposals.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['proposalId',proposalId,'revisionNumber',revisionNumber,'updateAction',_updateAction]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aProposal,TProposal) as TProposal;
end;



{ --------------------------------------------------------------------
  TPubprofilesResource
  --------------------------------------------------------------------}


Class Function TPubprofilesResource.ResourceName : String;

begin
  Result:='pubprofiles';
end;

Class Function TPubprofilesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangebuyerAPI;
end;

Function TPubprofilesResource.List(accountId: integer) : TGetPublisherProfilesByAccountIdResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'publisher/{accountId}/profiles';
  _Methodid   = 'adexchangebuyer.pubprofiles.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TGetPublisherProfilesByAccountIdResponse) as TGetPublisherProfilesByAccountIdResponse;
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
  Result:='v1.4';
end;

Class Function TAdexchangebuyerAPI.APIRevision : String;

begin
  Result:='20160509';
end;

Class Function TAdexchangebuyerAPI.APIID : String;

begin
  Result:='adexchangebuyer:v1.4';
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
  Result:='/adexchangebuyer/v1.4/';
end;

Class Function TAdexchangebuyerAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/adexchangebuyer/v1.4/';
end;

Class Function TAdexchangebuyerAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TAdexchangebuyerAPI.APIservicePath : string;

begin
  Result:='adexchangebuyer/v1.4/';
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
  TAddOrderDealsRequest.RegisterObject;
  TAddOrderDealsResponse.RegisterObject;
  TAddOrderNotesRequest.RegisterObject;
  TAddOrderNotesResponse.RegisterObject;
  TBillingInfo.RegisterObject;
  TBillingInfoList.RegisterObject;
  TBudget.RegisterObject;
  TBuyer.RegisterObject;
  TContactInformation.RegisterObject;
  TCreateOrdersRequest.RegisterObject;
  TCreateOrdersResponse.RegisterObject;
  TCreativeTypecorrectionsItem.RegisterObject;
  TCreativeTypefilteringReasonsTypereasonsItem.RegisterObject;
  TCreativeTypefilteringReasons.RegisterObject;
  TCreativeTypenativeAdTypeappIcon.RegisterObject;
  TCreativeTypenativeAdTypeimage.RegisterObject;
  TCreativeTypenativeAdTypelogo.RegisterObject;
  TCreativeTypenativeAd.RegisterObject;
  TCreativeTypeservingRestrictionsItemTypecontextsItem.RegisterObject;
  TCreativeTypeservingRestrictionsItemTypedisapprovalReasonsItem.RegisterObject;
  TCreativeTypeservingRestrictionsItem.RegisterObject;
  TCreative.RegisterObject;
  TCreativesList.RegisterObject;
  TDealServingMetadata.RegisterObject;
  TDealServingMetadataDealPauseStatus.RegisterObject;
  TDealTerms.RegisterObject;
  TDealTermsGuaranteedFixedPriceTerms.RegisterObject;
  TDealTermsGuaranteedFixedPriceTermsBillingInfo.RegisterObject;
  TDealTermsNonGuaranteedAuctionTerms.RegisterObject;
  TDealTermsNonGuaranteedFixedPriceTerms.RegisterObject;
  TDeleteOrderDealsRequest.RegisterObject;
  TDeleteOrderDealsResponse.RegisterObject;
  TDeliveryControl.RegisterObject;
  TDeliveryControlFrequencyCap.RegisterObject;
  TDimension.RegisterObject;
  TDimensionDimensionValue.RegisterObject;
  TEditAllOrderDealsRequest.RegisterObject;
  TEditAllOrderDealsResponse.RegisterObject;
  TGetOffersResponse.RegisterObject;
  TGetOrderDealsResponse.RegisterObject;
  TGetOrderNotesResponse.RegisterObject;
  TGetOrdersResponse.RegisterObject;
  TGetPublisherProfilesByAccountIdResponse.RegisterObject;
  TMarketplaceDeal.RegisterObject;
  TMarketplaceDealParty.RegisterObject;
  TMarketplaceLabel.RegisterObject;
  TMarketplaceNote.RegisterObject;
  TPerformanceReport.RegisterObject;
  TPerformanceReportList.RegisterObject;
  TPretargetingConfigTypedimensionsItem.RegisterObject;
  TPretargetingConfigTypeexcludedPlacementsItem.RegisterObject;
  TPretargetingConfigTypeplacementsItem.RegisterObject;
  TPretargetingConfigTypevideoPlayerSizesItem.RegisterObject;
  TPretargetingConfig.RegisterObject;
  TPretargetingConfigList.RegisterObject;
  TPrice.RegisterObject;
  TPricePerBuyer.RegisterObject;
  TPrivateData.RegisterObject;
  TProduct.RegisterObject;
  TProposal.RegisterObject;
  TPublisherProfileApiProto.RegisterObject;
  TPublisherProvidedForecast.RegisterObject;
  TSeller.RegisterObject;
  TSharedTargeting.RegisterObject;
  TTargetingValue.RegisterObject;
  TTargetingValueCreativeSize.RegisterObject;
  TTargetingValueDayPartTargeting.RegisterObject;
  TTargetingValueDayPartTargetingDayPart.RegisterObject;
  TTargetingValueSize.RegisterObject;
  TUpdatePrivateAuctionProposalRequest.RegisterObject;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
end;



Function TAdexchangebuyerAPI.GetMarketplacedealsInstance : TMarketplacedealsResource;

begin
  if (FMarketplacedealsInstance=Nil) then
    FMarketplacedealsInstance:=CreateMarketplacedealsResource;
  Result:=FMarketplacedealsInstance;
end;

Function TAdexchangebuyerAPI.CreateMarketplacedealsResource : TMarketplacedealsResource;

begin
  Result:=CreateMarketplacedealsResource(Self);
end;


Function TAdexchangebuyerAPI.CreateMarketplacedealsResource(AOwner : TComponent) : TMarketplacedealsResource;

begin
  Result:=TMarketplacedealsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdexchangebuyerAPI.GetMarketplacenotesInstance : TMarketplacenotesResource;

begin
  if (FMarketplacenotesInstance=Nil) then
    FMarketplacenotesInstance:=CreateMarketplacenotesResource;
  Result:=FMarketplacenotesInstance;
end;

Function TAdexchangebuyerAPI.CreateMarketplacenotesResource : TMarketplacenotesResource;

begin
  Result:=CreateMarketplacenotesResource(Self);
end;


Function TAdexchangebuyerAPI.CreateMarketplacenotesResource(AOwner : TComponent) : TMarketplacenotesResource;

begin
  Result:=TMarketplacenotesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdexchangebuyerAPI.GetMarketplaceprivateauctionInstance : TMarketplaceprivateauctionResource;

begin
  if (FMarketplaceprivateauctionInstance=Nil) then
    FMarketplaceprivateauctionInstance:=CreateMarketplaceprivateauctionResource;
  Result:=FMarketplaceprivateauctionInstance;
end;

Function TAdexchangebuyerAPI.CreateMarketplaceprivateauctionResource : TMarketplaceprivateauctionResource;

begin
  Result:=CreateMarketplaceprivateauctionResource(Self);
end;


Function TAdexchangebuyerAPI.CreateMarketplaceprivateauctionResource(AOwner : TComponent) : TMarketplaceprivateauctionResource;

begin
  Result:=TMarketplaceprivateauctionResource.Create(AOwner);
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
end;



Function TAdexchangebuyerAPI.GetProductsInstance : TProductsResource;

begin
  if (FProductsInstance=Nil) then
    FProductsInstance:=CreateProductsResource;
  Result:=FProductsInstance;
end;

Function TAdexchangebuyerAPI.CreateProductsResource : TProductsResource;

begin
  Result:=CreateProductsResource(Self);
end;


Function TAdexchangebuyerAPI.CreateProductsResource(AOwner : TComponent) : TProductsResource;

begin
  Result:=TProductsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdexchangebuyerAPI.GetProposalsInstance : TProposalsResource;

begin
  if (FProposalsInstance=Nil) then
    FProposalsInstance:=CreateProposalsResource;
  Result:=FProposalsInstance;
end;

Function TAdexchangebuyerAPI.CreateProposalsResource : TProposalsResource;

begin
  Result:=CreateProposalsResource(Self);
end;


Function TAdexchangebuyerAPI.CreateProposalsResource(AOwner : TComponent) : TProposalsResource;

begin
  Result:=TProposalsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdexchangebuyerAPI.GetPubprofilesInstance : TPubprofilesResource;

begin
  if (FPubprofilesInstance=Nil) then
    FPubprofilesInstance:=CreatePubprofilesResource;
  Result:=FPubprofilesInstance;
end;

Function TAdexchangebuyerAPI.CreatePubprofilesResource : TPubprofilesResource;

begin
  Result:=CreatePubprofilesResource(Self);
end;


Function TAdexchangebuyerAPI.CreatePubprofilesResource(AOwner : TComponent) : TPubprofilesResource;

begin
  Result:=TPubprofilesResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TAdexchangebuyerAPI.RegisterAPI;
end.
