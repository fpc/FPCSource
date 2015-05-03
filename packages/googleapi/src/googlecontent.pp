unit googlecontent;
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
  TAccountadwordsLinks = class;
  TAccountadwordsLinksArray = Array of TAccountadwordsLinks;
  TAccountusers = class;
  TAccountusersArray = Array of TAccountusers;
  TAccountAdwordsLink = class;
  TAccountAdwordsLinkArray = Array of TAccountAdwordsLink;
  TAccountIdentifier = class;
  TAccountIdentifierArray = Array of TAccountIdentifier;
  TAccountShipping = class;
  TAccountShippingArray = Array of TAccountShipping;
  TAccountShippingcarrierRates = class;
  TAccountShippingcarrierRatesArray = Array of TAccountShippingcarrierRates;
  TAccountShippinglocationGroups = class;
  TAccountShippinglocationGroupsArray = Array of TAccountShippinglocationGroups;
  TAccountShippingrateTables = class;
  TAccountShippingrateTablesArray = Array of TAccountShippingrateTables;
  TAccountShippingservices = class;
  TAccountShippingservicesArray = Array of TAccountShippingservices;
  TAccountShippingCarrierRate = class;
  TAccountShippingCarrierRateArray = Array of TAccountShippingCarrierRate;
  TAccountShippingCondition = class;
  TAccountShippingConditionArray = Array of TAccountShippingCondition;
  TAccountShippingLocationGroup = class;
  TAccountShippingLocationGroupArray = Array of TAccountShippingLocationGroup;
  TAccountShippingLocationGrouplocationIds = class;
  TAccountShippingLocationGrouplocationIdsArray = Array of TAccountShippingLocationGrouplocationIds;
  TAccountShippingLocationGrouppostalCodeRanges = class;
  TAccountShippingLocationGrouppostalCodeRangesArray = Array of TAccountShippingLocationGrouppostalCodeRanges;
  TAccountShippingLocationGrouppostalCodes = class;
  TAccountShippingLocationGrouppostalCodesArray = Array of TAccountShippingLocationGrouppostalCodes;
  TAccountShippingPostalCodeRange = class;
  TAccountShippingPostalCodeRangeArray = Array of TAccountShippingPostalCodeRange;
  TAccountShippingRateTable = class;
  TAccountShippingRateTableArray = Array of TAccountShippingRateTable;
  TAccountShippingRateTablecontent = class;
  TAccountShippingRateTablecontentArray = Array of TAccountShippingRateTablecontent;
  TAccountShippingRateTableCell = class;
  TAccountShippingRateTableCellArray = Array of TAccountShippingRateTableCell;
  TAccountShippingShippingService = class;
  TAccountShippingShippingServiceArray = Array of TAccountShippingShippingService;
  TAccountShippingShippingServiceCalculationMethod = class;
  TAccountShippingShippingServiceCalculationMethodArray = Array of TAccountShippingShippingServiceCalculationMethod;
  TAccountShippingShippingServiceCostRule = class;
  TAccountShippingShippingServiceCostRuleArray = Array of TAccountShippingShippingServiceCostRule;
  TAccountShippingShippingServiceCostRulechildren = class;
  TAccountShippingShippingServiceCostRulechildrenArray = Array of TAccountShippingShippingServiceCostRulechildren;
  TAccountStatus = class;
  TAccountStatusArray = Array of TAccountStatus;
  TAccountStatusdataQualityIssues = class;
  TAccountStatusdataQualityIssuesArray = Array of TAccountStatusdataQualityIssues;
  TAccountStatusDataQualityIssue = class;
  TAccountStatusDataQualityIssueArray = Array of TAccountStatusDataQualityIssue;
  TAccountStatusDataQualityIssueexampleItems = class;
  TAccountStatusDataQualityIssueexampleItemsArray = Array of TAccountStatusDataQualityIssueexampleItems;
  TAccountStatusExampleItem = class;
  TAccountStatusExampleItemArray = Array of TAccountStatusExampleItem;
  TAccountTax = class;
  TAccountTaxArray = Array of TAccountTax;
  TAccountTaxrules = class;
  TAccountTaxrulesArray = Array of TAccountTaxrules;
  TAccountTaxTaxRule = class;
  TAccountTaxTaxRuleArray = Array of TAccountTaxTaxRule;
  TAccountUser = class;
  TAccountUserArray = Array of TAccountUser;
  TAccountsAuthInfoResponse = class;
  TAccountsAuthInfoResponseArray = Array of TAccountsAuthInfoResponse;
  TAccountsAuthInfoResponseaccountIdentifiers = class;
  TAccountsAuthInfoResponseaccountIdentifiersArray = Array of TAccountsAuthInfoResponseaccountIdentifiers;
  TAccountsCustomBatchRequest = class;
  TAccountsCustomBatchRequestArray = Array of TAccountsCustomBatchRequest;
  TAccountsCustomBatchRequestentries = class;
  TAccountsCustomBatchRequestentriesArray = Array of TAccountsCustomBatchRequestentries;
  TAccountsCustomBatchRequestEntry = class;
  TAccountsCustomBatchRequestEntryArray = Array of TAccountsCustomBatchRequestEntry;
  TAccountsCustomBatchResponse = class;
  TAccountsCustomBatchResponseArray = Array of TAccountsCustomBatchResponse;
  TAccountsCustomBatchResponseentries = class;
  TAccountsCustomBatchResponseentriesArray = Array of TAccountsCustomBatchResponseentries;
  TAccountsCustomBatchResponseEntry = class;
  TAccountsCustomBatchResponseEntryArray = Array of TAccountsCustomBatchResponseEntry;
  TAccountsListResponse = class;
  TAccountsListResponseArray = Array of TAccountsListResponse;
  TAccountsListResponseresources = class;
  TAccountsListResponseresourcesArray = Array of TAccountsListResponseresources;
  TAccountshippingCustomBatchRequest = class;
  TAccountshippingCustomBatchRequestArray = Array of TAccountshippingCustomBatchRequest;
  TAccountshippingCustomBatchRequestentries = class;
  TAccountshippingCustomBatchRequestentriesArray = Array of TAccountshippingCustomBatchRequestentries;
  TAccountshippingCustomBatchRequestEntry = class;
  TAccountshippingCustomBatchRequestEntryArray = Array of TAccountshippingCustomBatchRequestEntry;
  TAccountshippingCustomBatchResponse = class;
  TAccountshippingCustomBatchResponseArray = Array of TAccountshippingCustomBatchResponse;
  TAccountshippingCustomBatchResponseentries = class;
  TAccountshippingCustomBatchResponseentriesArray = Array of TAccountshippingCustomBatchResponseentries;
  TAccountshippingCustomBatchResponseEntry = class;
  TAccountshippingCustomBatchResponseEntryArray = Array of TAccountshippingCustomBatchResponseEntry;
  TAccountshippingListResponse = class;
  TAccountshippingListResponseArray = Array of TAccountshippingListResponse;
  TAccountshippingListResponseresources = class;
  TAccountshippingListResponseresourcesArray = Array of TAccountshippingListResponseresources;
  TAccountstatusesCustomBatchRequest = class;
  TAccountstatusesCustomBatchRequestArray = Array of TAccountstatusesCustomBatchRequest;
  TAccountstatusesCustomBatchRequestentries = class;
  TAccountstatusesCustomBatchRequestentriesArray = Array of TAccountstatusesCustomBatchRequestentries;
  TAccountstatusesCustomBatchRequestEntry = class;
  TAccountstatusesCustomBatchRequestEntryArray = Array of TAccountstatusesCustomBatchRequestEntry;
  TAccountstatusesCustomBatchResponse = class;
  TAccountstatusesCustomBatchResponseArray = Array of TAccountstatusesCustomBatchResponse;
  TAccountstatusesCustomBatchResponseentries = class;
  TAccountstatusesCustomBatchResponseentriesArray = Array of TAccountstatusesCustomBatchResponseentries;
  TAccountstatusesCustomBatchResponseEntry = class;
  TAccountstatusesCustomBatchResponseEntryArray = Array of TAccountstatusesCustomBatchResponseEntry;
  TAccountstatusesListResponse = class;
  TAccountstatusesListResponseArray = Array of TAccountstatusesListResponse;
  TAccountstatusesListResponseresources = class;
  TAccountstatusesListResponseresourcesArray = Array of TAccountstatusesListResponseresources;
  TAccounttaxCustomBatchRequest = class;
  TAccounttaxCustomBatchRequestArray = Array of TAccounttaxCustomBatchRequest;
  TAccounttaxCustomBatchRequestentries = class;
  TAccounttaxCustomBatchRequestentriesArray = Array of TAccounttaxCustomBatchRequestentries;
  TAccounttaxCustomBatchRequestEntry = class;
  TAccounttaxCustomBatchRequestEntryArray = Array of TAccounttaxCustomBatchRequestEntry;
  TAccounttaxCustomBatchResponse = class;
  TAccounttaxCustomBatchResponseArray = Array of TAccounttaxCustomBatchResponse;
  TAccounttaxCustomBatchResponseentries = class;
  TAccounttaxCustomBatchResponseentriesArray = Array of TAccounttaxCustomBatchResponseentries;
  TAccounttaxCustomBatchResponseEntry = class;
  TAccounttaxCustomBatchResponseEntryArray = Array of TAccounttaxCustomBatchResponseEntry;
  TAccounttaxListResponse = class;
  TAccounttaxListResponseArray = Array of TAccounttaxListResponse;
  TAccounttaxListResponseresources = class;
  TAccounttaxListResponseresourcesArray = Array of TAccounttaxListResponseresources;
  TDatafeed = class;
  TDatafeedArray = Array of TDatafeed;
  TDatafeedintendedDestinations = class;
  TDatafeedintendedDestinationsArray = Array of TDatafeedintendedDestinations;
  TDatafeedFetchSchedule = class;
  TDatafeedFetchScheduleArray = Array of TDatafeedFetchSchedule;
  TDatafeedFormat = class;
  TDatafeedFormatArray = Array of TDatafeedFormat;
  TDatafeedStatus = class;
  TDatafeedStatusArray = Array of TDatafeedStatus;
  TDatafeedStatuserrors = class;
  TDatafeedStatuserrorsArray = Array of TDatafeedStatuserrors;
  TDatafeedStatuswarnings = class;
  TDatafeedStatuswarningsArray = Array of TDatafeedStatuswarnings;
  TDatafeedStatusError = class;
  TDatafeedStatusErrorArray = Array of TDatafeedStatusError;
  TDatafeedStatusErrorexamples = class;
  TDatafeedStatusErrorexamplesArray = Array of TDatafeedStatusErrorexamples;
  TDatafeedStatusExample = class;
  TDatafeedStatusExampleArray = Array of TDatafeedStatusExample;
  TDatafeedsCustomBatchRequest = class;
  TDatafeedsCustomBatchRequestArray = Array of TDatafeedsCustomBatchRequest;
  TDatafeedsCustomBatchRequestentries = class;
  TDatafeedsCustomBatchRequestentriesArray = Array of TDatafeedsCustomBatchRequestentries;
  TDatafeedsCustomBatchRequestEntry = class;
  TDatafeedsCustomBatchRequestEntryArray = Array of TDatafeedsCustomBatchRequestEntry;
  TDatafeedsCustomBatchResponse = class;
  TDatafeedsCustomBatchResponseArray = Array of TDatafeedsCustomBatchResponse;
  TDatafeedsCustomBatchResponseentries = class;
  TDatafeedsCustomBatchResponseentriesArray = Array of TDatafeedsCustomBatchResponseentries;
  TDatafeedsCustomBatchResponseEntry = class;
  TDatafeedsCustomBatchResponseEntryArray = Array of TDatafeedsCustomBatchResponseEntry;
  TDatafeedsListResponse = class;
  TDatafeedsListResponseArray = Array of TDatafeedsListResponse;
  TDatafeedsListResponseresources = class;
  TDatafeedsListResponseresourcesArray = Array of TDatafeedsListResponseresources;
  TDatafeedstatusesCustomBatchRequest = class;
  TDatafeedstatusesCustomBatchRequestArray = Array of TDatafeedstatusesCustomBatchRequest;
  TDatafeedstatusesCustomBatchRequestentries = class;
  TDatafeedstatusesCustomBatchRequestentriesArray = Array of TDatafeedstatusesCustomBatchRequestentries;
  TDatafeedstatusesCustomBatchRequestEntry = class;
  TDatafeedstatusesCustomBatchRequestEntryArray = Array of TDatafeedstatusesCustomBatchRequestEntry;
  TDatafeedstatusesCustomBatchResponse = class;
  TDatafeedstatusesCustomBatchResponseArray = Array of TDatafeedstatusesCustomBatchResponse;
  TDatafeedstatusesCustomBatchResponseentries = class;
  TDatafeedstatusesCustomBatchResponseentriesArray = Array of TDatafeedstatusesCustomBatchResponseentries;
  TDatafeedstatusesCustomBatchResponseEntry = class;
  TDatafeedstatusesCustomBatchResponseEntryArray = Array of TDatafeedstatusesCustomBatchResponseEntry;
  TDatafeedstatusesListResponse = class;
  TDatafeedstatusesListResponseArray = Array of TDatafeedstatusesListResponse;
  TDatafeedstatusesListResponseresources = class;
  TDatafeedstatusesListResponseresourcesArray = Array of TDatafeedstatusesListResponseresources;
  TError = class;
  TErrorArray = Array of TError;
  TErrors = class;
  TErrorsArray = Array of TErrors;
  TErrorserrors = class;
  TErrorserrorsArray = Array of TErrorserrors;
  TInventory = class;
  TInventoryArray = Array of TInventory;
  TInventoryCustomBatchRequest = class;
  TInventoryCustomBatchRequestArray = Array of TInventoryCustomBatchRequest;
  TInventoryCustomBatchRequestentries = class;
  TInventoryCustomBatchRequestentriesArray = Array of TInventoryCustomBatchRequestentries;
  TInventoryCustomBatchRequestEntry = class;
  TInventoryCustomBatchRequestEntryArray = Array of TInventoryCustomBatchRequestEntry;
  TInventoryCustomBatchResponse = class;
  TInventoryCustomBatchResponseArray = Array of TInventoryCustomBatchResponse;
  TInventoryCustomBatchResponseentries = class;
  TInventoryCustomBatchResponseentriesArray = Array of TInventoryCustomBatchResponseentries;
  TInventoryCustomBatchResponseEntry = class;
  TInventoryCustomBatchResponseEntryArray = Array of TInventoryCustomBatchResponseEntry;
  TInventorySetRequest = class;
  TInventorySetRequestArray = Array of TInventorySetRequest;
  TInventorySetResponse = class;
  TInventorySetResponseArray = Array of TInventorySetResponse;
  TLoyaltyPoints = class;
  TLoyaltyPointsArray = Array of TLoyaltyPoints;
  TPrice = class;
  TPriceArray = Array of TPrice;
  TProduct = class;
  TProductArray = Array of TProduct;
  TProductadditionalImageLinks = class;
  TProductadditionalImageLinksArray = Array of TProductadditionalImageLinks;
  TProductadwordsLabels = class;
  TProductadwordsLabelsArray = Array of TProductadwordsLabels;
  TProductaspects = class;
  TProductaspectsArray = Array of TProductaspects;
  TProductcustomAttributes = class;
  TProductcustomAttributesArray = Array of TProductcustomAttributes;
  TProductcustomGroups = class;
  TProductcustomGroupsArray = Array of TProductcustomGroups;
  TProductdestinations = class;
  TProductdestinationsArray = Array of TProductdestinations;
  TProductdisplayAdsSimilarIds = class;
  TProductdisplayAdsSimilarIdsArray = Array of TProductdisplayAdsSimilarIds;
  TProductshipping = class;
  TProductshippingArray = Array of TProductshipping;
  TProductsizes = class;
  TProductsizesArray = Array of TProductsizes;
  TProducttaxes = class;
  TProducttaxesArray = Array of TProducttaxes;
  TProductvalidatedDestinations = class;
  TProductvalidatedDestinationsArray = Array of TProductvalidatedDestinations;
  TProductwarnings = class;
  TProductwarningsArray = Array of TProductwarnings;
  TProductAspect = class;
  TProductAspectArray = Array of TProductAspect;
  TProductCustomAttribute = class;
  TProductCustomAttributeArray = Array of TProductCustomAttribute;
  TProductCustomGroup = class;
  TProductCustomGroupArray = Array of TProductCustomGroup;
  TProductCustomGroupattributes = class;
  TProductCustomGroupattributesArray = Array of TProductCustomGroupattributes;
  TProductDestination = class;
  TProductDestinationArray = Array of TProductDestination;
  TProductInstallment = class;
  TProductInstallmentArray = Array of TProductInstallment;
  TProductShippingDimension = class;
  TProductShippingDimensionArray = Array of TProductShippingDimension;
  TProductShippingWeight = class;
  TProductShippingWeightArray = Array of TProductShippingWeight;
  TProductStatus = class;
  TProductStatusArray = Array of TProductStatus;
  TProductStatusdataQualityIssues = class;
  TProductStatusdataQualityIssuesArray = Array of TProductStatusdataQualityIssues;
  TProductStatusdestinationStatuses = class;
  TProductStatusdestinationStatusesArray = Array of TProductStatusdestinationStatuses;
  TProductStatusDataQualityIssue = class;
  TProductStatusDataQualityIssueArray = Array of TProductStatusDataQualityIssue;
  TProductStatusDestinationStatus = class;
  TProductStatusDestinationStatusArray = Array of TProductStatusDestinationStatus;
  TProductTax = class;
  TProductTaxArray = Array of TProductTax;
  TProductUnitPricingBaseMeasure = class;
  TProductUnitPricingBaseMeasureArray = Array of TProductUnitPricingBaseMeasure;
  TProductUnitPricingMeasure = class;
  TProductUnitPricingMeasureArray = Array of TProductUnitPricingMeasure;
  TProductsCustomBatchRequest = class;
  TProductsCustomBatchRequestArray = Array of TProductsCustomBatchRequest;
  TProductsCustomBatchRequestentries = class;
  TProductsCustomBatchRequestentriesArray = Array of TProductsCustomBatchRequestentries;
  TProductsCustomBatchRequestEntry = class;
  TProductsCustomBatchRequestEntryArray = Array of TProductsCustomBatchRequestEntry;
  TProductsCustomBatchResponse = class;
  TProductsCustomBatchResponseArray = Array of TProductsCustomBatchResponse;
  TProductsCustomBatchResponseentries = class;
  TProductsCustomBatchResponseentriesArray = Array of TProductsCustomBatchResponseentries;
  TProductsCustomBatchResponseEntry = class;
  TProductsCustomBatchResponseEntryArray = Array of TProductsCustomBatchResponseEntry;
  TProductsListResponse = class;
  TProductsListResponseArray = Array of TProductsListResponse;
  TProductsListResponseresources = class;
  TProductsListResponseresourcesArray = Array of TProductsListResponseresources;
  TProductstatusesCustomBatchRequest = class;
  TProductstatusesCustomBatchRequestArray = Array of TProductstatusesCustomBatchRequest;
  TProductstatusesCustomBatchRequestentries = class;
  TProductstatusesCustomBatchRequestentriesArray = Array of TProductstatusesCustomBatchRequestentries;
  TProductstatusesCustomBatchRequestEntry = class;
  TProductstatusesCustomBatchRequestEntryArray = Array of TProductstatusesCustomBatchRequestEntry;
  TProductstatusesCustomBatchResponse = class;
  TProductstatusesCustomBatchResponseArray = Array of TProductstatusesCustomBatchResponse;
  TProductstatusesCustomBatchResponseentries = class;
  TProductstatusesCustomBatchResponseentriesArray = Array of TProductstatusesCustomBatchResponseentries;
  TProductstatusesCustomBatchResponseEntry = class;
  TProductstatusesCustomBatchResponseEntryArray = Array of TProductstatusesCustomBatchResponseEntry;
  TProductstatusesListResponse = class;
  TProductstatusesListResponseArray = Array of TProductstatusesListResponse;
  TProductstatusesListResponseresources = class;
  TProductstatusesListResponseresourcesArray = Array of TProductstatusesListResponseresources;
  TWeight = class;
  TWeightArray = Array of TWeight;
  
  { --------------------------------------------------------------------
    TAccount
    --------------------------------------------------------------------}
  
  TAccount = Class(TGoogleBaseObject)
  Private
    FadultContent : boolean;
    FadwordsLinks : TAccountadwordsLinks;
    Fid : string;
    Fkind : string;
    Fname : string;
    FreviewsUrl : string;
    FsellerId : string;
    Fusers : TAccountusers;
    FwebsiteUrl : string;
  Protected
    //Property setters
    Procedure SetadultContent(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetadwordsLinks(AIndex : Integer; AValue : TAccountadwordsLinks); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetreviewsUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetsellerId(AIndex : Integer; AValue : string); virtual;
    Procedure Setusers(AIndex : Integer; AValue : TAccountusers); virtual;
    Procedure SetwebsiteUrl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property adultContent : boolean Index 0 Read FadultContent Write SetadultContent;
    Property adwordsLinks : TAccountadwordsLinks Index 8 Read FadwordsLinks Write SetadwordsLinks;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property name : string Index 32 Read Fname Write Setname;
    Property reviewsUrl : string Index 40 Read FreviewsUrl Write SetreviewsUrl;
    Property sellerId : string Index 48 Read FsellerId Write SetsellerId;
    Property users : TAccountusers Index 56 Read Fusers Write Setusers;
    Property websiteUrl : string Index 64 Read FwebsiteUrl Write SetwebsiteUrl;
  end;
  TAccountClass = Class of TAccount;
  
  { --------------------------------------------------------------------
    TAccountadwordsLinks
    --------------------------------------------------------------------}
  
  TAccountadwordsLinks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountadwordsLinksClass = Class of TAccountadwordsLinks;
  
  { --------------------------------------------------------------------
    TAccountusers
    --------------------------------------------------------------------}
  
  TAccountusers = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountusersClass = Class of TAccountusers;
  
  { --------------------------------------------------------------------
    TAccountAdwordsLink
    --------------------------------------------------------------------}
  
  TAccountAdwordsLink = Class(TGoogleBaseObject)
  Private
    FadwordsId : string;
    Fstatus : string;
  Protected
    //Property setters
    Procedure SetadwordsId(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property adwordsId : string Index 0 Read FadwordsId Write SetadwordsId;
    Property status : string Index 8 Read Fstatus Write Setstatus;
  end;
  TAccountAdwordsLinkClass = Class of TAccountAdwordsLink;
  
  { --------------------------------------------------------------------
    TAccountIdentifier
    --------------------------------------------------------------------}
  
  TAccountIdentifier = Class(TGoogleBaseObject)
  Private
    FaggregatorId : string;
    FmerchantId : string;
  Protected
    //Property setters
    Procedure SetaggregatorId(AIndex : Integer; AValue : string); virtual;
    Procedure SetmerchantId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property aggregatorId : string Index 0 Read FaggregatorId Write SetaggregatorId;
    Property merchantId : string Index 8 Read FmerchantId Write SetmerchantId;
  end;
  TAccountIdentifierClass = Class of TAccountIdentifier;
  
  { --------------------------------------------------------------------
    TAccountShipping
    --------------------------------------------------------------------}
  
  TAccountShipping = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    FcarrierRates : TAccountShippingcarrierRates;
    Fkind : string;
    FlocationGroups : TAccountShippinglocationGroups;
    FrateTables : TAccountShippingrateTables;
    Fservices : TAccountShippingservices;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetcarrierRates(AIndex : Integer; AValue : TAccountShippingcarrierRates); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetlocationGroups(AIndex : Integer; AValue : TAccountShippinglocationGroups); virtual;
    Procedure SetrateTables(AIndex : Integer; AValue : TAccountShippingrateTables); virtual;
    Procedure Setservices(AIndex : Integer; AValue : TAccountShippingservices); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property carrierRates : TAccountShippingcarrierRates Index 8 Read FcarrierRates Write SetcarrierRates;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property locationGroups : TAccountShippinglocationGroups Index 24 Read FlocationGroups Write SetlocationGroups;
    Property rateTables : TAccountShippingrateTables Index 32 Read FrateTables Write SetrateTables;
    Property services : TAccountShippingservices Index 40 Read Fservices Write Setservices;
  end;
  TAccountShippingClass = Class of TAccountShipping;
  
  { --------------------------------------------------------------------
    TAccountShippingcarrierRates
    --------------------------------------------------------------------}
  
  TAccountShippingcarrierRates = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountShippingcarrierRatesClass = Class of TAccountShippingcarrierRates;
  
  { --------------------------------------------------------------------
    TAccountShippinglocationGroups
    --------------------------------------------------------------------}
  
  TAccountShippinglocationGroups = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountShippinglocationGroupsClass = Class of TAccountShippinglocationGroups;
  
  { --------------------------------------------------------------------
    TAccountShippingrateTables
    --------------------------------------------------------------------}
  
  TAccountShippingrateTables = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountShippingrateTablesClass = Class of TAccountShippingrateTables;
  
  { --------------------------------------------------------------------
    TAccountShippingservices
    --------------------------------------------------------------------}
  
  TAccountShippingservices = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountShippingservicesClass = Class of TAccountShippingservices;
  
  { --------------------------------------------------------------------
    TAccountShippingCarrierRate
    --------------------------------------------------------------------}
  
  TAccountShippingCarrierRate = Class(TGoogleBaseObject)
  Private
    Fcarrier : string;
    FcarrierService : string;
    FmodifierFlatRate : TPrice;
    FmodifierPercent : string;
    Fname : string;
    FsaleCountry : string;
    FshippingOrigin : string;
  Protected
    //Property setters
    Procedure Setcarrier(AIndex : Integer; AValue : string); virtual;
    Procedure SetcarrierService(AIndex : Integer; AValue : string); virtual;
    Procedure SetmodifierFlatRate(AIndex : Integer; AValue : TPrice); virtual;
    Procedure SetmodifierPercent(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetsaleCountry(AIndex : Integer; AValue : string); virtual;
    Procedure SetshippingOrigin(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property carrier : string Index 0 Read Fcarrier Write Setcarrier;
    Property carrierService : string Index 8 Read FcarrierService Write SetcarrierService;
    Property modifierFlatRate : TPrice Index 16 Read FmodifierFlatRate Write SetmodifierFlatRate;
    Property modifierPercent : string Index 24 Read FmodifierPercent Write SetmodifierPercent;
    Property name : string Index 32 Read Fname Write Setname;
    Property saleCountry : string Index 40 Read FsaleCountry Write SetsaleCountry;
    Property shippingOrigin : string Index 48 Read FshippingOrigin Write SetshippingOrigin;
  end;
  TAccountShippingCarrierRateClass = Class of TAccountShippingCarrierRate;
  
  { --------------------------------------------------------------------
    TAccountShippingCondition
    --------------------------------------------------------------------}
  
  TAccountShippingCondition = Class(TGoogleBaseObject)
  Private
    FdeliveryLocationGroup : string;
    FdeliveryLocationId : string;
    FdeliveryPostalCode : string;
    FdeliveryPostalCodeRange : TAccountShippingPostalCodeRange;
    FpriceMax : TPrice;
    FshippingLabel : string;
    FweightMax : TWeight;
  Protected
    //Property setters
    Procedure SetdeliveryLocationGroup(AIndex : Integer; AValue : string); virtual;
    Procedure SetdeliveryLocationId(AIndex : Integer; AValue : string); virtual;
    Procedure SetdeliveryPostalCode(AIndex : Integer; AValue : string); virtual;
    Procedure SetdeliveryPostalCodeRange(AIndex : Integer; AValue : TAccountShippingPostalCodeRange); virtual;
    Procedure SetpriceMax(AIndex : Integer; AValue : TPrice); virtual;
    Procedure SetshippingLabel(AIndex : Integer; AValue : string); virtual;
    Procedure SetweightMax(AIndex : Integer; AValue : TWeight); virtual;
  Public
  Published
    Property deliveryLocationGroup : string Index 0 Read FdeliveryLocationGroup Write SetdeliveryLocationGroup;
    Property deliveryLocationId : string Index 8 Read FdeliveryLocationId Write SetdeliveryLocationId;
    Property deliveryPostalCode : string Index 16 Read FdeliveryPostalCode Write SetdeliveryPostalCode;
    Property deliveryPostalCodeRange : TAccountShippingPostalCodeRange Index 24 Read FdeliveryPostalCodeRange Write SetdeliveryPostalCodeRange;
    Property priceMax : TPrice Index 32 Read FpriceMax Write SetpriceMax;
    Property shippingLabel : string Index 40 Read FshippingLabel Write SetshippingLabel;
    Property weightMax : TWeight Index 48 Read FweightMax Write SetweightMax;
  end;
  TAccountShippingConditionClass = Class of TAccountShippingCondition;
  
  { --------------------------------------------------------------------
    TAccountShippingLocationGroup
    --------------------------------------------------------------------}
  
  TAccountShippingLocationGroup = Class(TGoogleBaseObject)
  Private
    Fcountry : string;
    FlocationIds : TAccountShippingLocationGrouplocationIds;
    Fname : string;
    FpostalCodeRanges : TAccountShippingLocationGrouppostalCodeRanges;
    FpostalCodes : TAccountShippingLocationGrouppostalCodes;
  Protected
    //Property setters
    Procedure Setcountry(AIndex : Integer; AValue : string); virtual;
    Procedure SetlocationIds(AIndex : Integer; AValue : TAccountShippingLocationGrouplocationIds); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetpostalCodeRanges(AIndex : Integer; AValue : TAccountShippingLocationGrouppostalCodeRanges); virtual;
    Procedure SetpostalCodes(AIndex : Integer; AValue : TAccountShippingLocationGrouppostalCodes); virtual;
  Public
  Published
    Property country : string Index 0 Read Fcountry Write Setcountry;
    Property locationIds : TAccountShippingLocationGrouplocationIds Index 8 Read FlocationIds Write SetlocationIds;
    Property name : string Index 16 Read Fname Write Setname;
    Property postalCodeRanges : TAccountShippingLocationGrouppostalCodeRanges Index 24 Read FpostalCodeRanges Write SetpostalCodeRanges;
    Property postalCodes : TAccountShippingLocationGrouppostalCodes Index 32 Read FpostalCodes Write SetpostalCodes;
  end;
  TAccountShippingLocationGroupClass = Class of TAccountShippingLocationGroup;
  
  { --------------------------------------------------------------------
    TAccountShippingLocationGrouplocationIds
    --------------------------------------------------------------------}
  
  TAccountShippingLocationGrouplocationIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountShippingLocationGrouplocationIdsClass = Class of TAccountShippingLocationGrouplocationIds;
  
  { --------------------------------------------------------------------
    TAccountShippingLocationGrouppostalCodeRanges
    --------------------------------------------------------------------}
  
  TAccountShippingLocationGrouppostalCodeRanges = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountShippingLocationGrouppostalCodeRangesClass = Class of TAccountShippingLocationGrouppostalCodeRanges;
  
  { --------------------------------------------------------------------
    TAccountShippingLocationGrouppostalCodes
    --------------------------------------------------------------------}
  
  TAccountShippingLocationGrouppostalCodes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountShippingLocationGrouppostalCodesClass = Class of TAccountShippingLocationGrouppostalCodes;
  
  { --------------------------------------------------------------------
    TAccountShippingPostalCodeRange
    --------------------------------------------------------------------}
  
  TAccountShippingPostalCodeRange = Class(TGoogleBaseObject)
  Private
    F_end : string;
    Fstart : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_end(AIndex : Integer; AValue : string); virtual;
    Procedure Setstart(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property _end : string Index 0 Read F_end Write Set_end;
    Property start : string Index 8 Read Fstart Write Setstart;
  end;
  TAccountShippingPostalCodeRangeClass = Class of TAccountShippingPostalCodeRange;
  
  { --------------------------------------------------------------------
    TAccountShippingRateTable
    --------------------------------------------------------------------}
  
  TAccountShippingRateTable = Class(TGoogleBaseObject)
  Private
    Fcontent : TAccountShippingRateTablecontent;
    Fname : string;
    FsaleCountry : string;
  Protected
    //Property setters
    Procedure Setcontent(AIndex : Integer; AValue : TAccountShippingRateTablecontent); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetsaleCountry(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property content : TAccountShippingRateTablecontent Index 0 Read Fcontent Write Setcontent;
    Property name : string Index 8 Read Fname Write Setname;
    Property saleCountry : string Index 16 Read FsaleCountry Write SetsaleCountry;
  end;
  TAccountShippingRateTableClass = Class of TAccountShippingRateTable;
  
  { --------------------------------------------------------------------
    TAccountShippingRateTablecontent
    --------------------------------------------------------------------}
  
  TAccountShippingRateTablecontent = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountShippingRateTablecontentClass = Class of TAccountShippingRateTablecontent;
  
  { --------------------------------------------------------------------
    TAccountShippingRateTableCell
    --------------------------------------------------------------------}
  
  TAccountShippingRateTableCell = Class(TGoogleBaseObject)
  Private
    Fcondition : TAccountShippingCondition;
    Frate : TPrice;
  Protected
    //Property setters
    Procedure Setcondition(AIndex : Integer; AValue : TAccountShippingCondition); virtual;
    Procedure Setrate(AIndex : Integer; AValue : TPrice); virtual;
  Public
  Published
    Property condition : TAccountShippingCondition Index 0 Read Fcondition Write Setcondition;
    Property rate : TPrice Index 8 Read Frate Write Setrate;
  end;
  TAccountShippingRateTableCellClass = Class of TAccountShippingRateTableCell;
  
  { --------------------------------------------------------------------
    TAccountShippingShippingService
    --------------------------------------------------------------------}
  
  TAccountShippingShippingService = Class(TGoogleBaseObject)
  Private
    Factive : boolean;
    FcalculationMethod : TAccountShippingShippingServiceCalculationMethod;
    FcostRuleTree : TAccountShippingShippingServiceCostRule;
    Fname : string;
    FsaleCountry : string;
  Protected
    //Property setters
    Procedure Setactive(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcalculationMethod(AIndex : Integer; AValue : TAccountShippingShippingServiceCalculationMethod); virtual;
    Procedure SetcostRuleTree(AIndex : Integer; AValue : TAccountShippingShippingServiceCostRule); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetsaleCountry(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property active : boolean Index 0 Read Factive Write Setactive;
    Property calculationMethod : TAccountShippingShippingServiceCalculationMethod Index 8 Read FcalculationMethod Write SetcalculationMethod;
    Property costRuleTree : TAccountShippingShippingServiceCostRule Index 16 Read FcostRuleTree Write SetcostRuleTree;
    Property name : string Index 24 Read Fname Write Setname;
    Property saleCountry : string Index 32 Read FsaleCountry Write SetsaleCountry;
  end;
  TAccountShippingShippingServiceClass = Class of TAccountShippingShippingService;
  
  { --------------------------------------------------------------------
    TAccountShippingShippingServiceCalculationMethod
    --------------------------------------------------------------------}
  
  TAccountShippingShippingServiceCalculationMethod = Class(TGoogleBaseObject)
  Private
    FcarrierRate : string;
    Fexcluded : boolean;
    FflatRate : TPrice;
    FpercentageRate : string;
    FrateTable : string;
  Protected
    //Property setters
    Procedure SetcarrierRate(AIndex : Integer; AValue : string); virtual;
    Procedure Setexcluded(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetflatRate(AIndex : Integer; AValue : TPrice); virtual;
    Procedure SetpercentageRate(AIndex : Integer; AValue : string); virtual;
    Procedure SetrateTable(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property carrierRate : string Index 0 Read FcarrierRate Write SetcarrierRate;
    Property excluded : boolean Index 8 Read Fexcluded Write Setexcluded;
    Property flatRate : TPrice Index 16 Read FflatRate Write SetflatRate;
    Property percentageRate : string Index 24 Read FpercentageRate Write SetpercentageRate;
    Property rateTable : string Index 32 Read FrateTable Write SetrateTable;
  end;
  TAccountShippingShippingServiceCalculationMethodClass = Class of TAccountShippingShippingServiceCalculationMethod;
  
  { --------------------------------------------------------------------
    TAccountShippingShippingServiceCostRule
    --------------------------------------------------------------------}
  
  TAccountShippingShippingServiceCostRule = Class(TGoogleBaseObject)
  Private
    FcalculationMethod : TAccountShippingShippingServiceCalculationMethod;
    Fchildren : TAccountShippingShippingServiceCostRulechildren;
    Fcondition : TAccountShippingCondition;
  Protected
    //Property setters
    Procedure SetcalculationMethod(AIndex : Integer; AValue : TAccountShippingShippingServiceCalculationMethod); virtual;
    Procedure Setchildren(AIndex : Integer; AValue : TAccountShippingShippingServiceCostRulechildren); virtual;
    Procedure Setcondition(AIndex : Integer; AValue : TAccountShippingCondition); virtual;
  Public
  Published
    Property calculationMethod : TAccountShippingShippingServiceCalculationMethod Index 0 Read FcalculationMethod Write SetcalculationMethod;
    Property children : TAccountShippingShippingServiceCostRulechildren Index 8 Read Fchildren Write Setchildren;
    Property condition : TAccountShippingCondition Index 16 Read Fcondition Write Setcondition;
  end;
  TAccountShippingShippingServiceCostRuleClass = Class of TAccountShippingShippingServiceCostRule;
  
  { --------------------------------------------------------------------
    TAccountShippingShippingServiceCostRulechildren
    --------------------------------------------------------------------}
  
  TAccountShippingShippingServiceCostRulechildren = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountShippingShippingServiceCostRulechildrenClass = Class of TAccountShippingShippingServiceCostRulechildren;
  
  { --------------------------------------------------------------------
    TAccountStatus
    --------------------------------------------------------------------}
  
  TAccountStatus = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    FdataQualityIssues : TAccountStatusdataQualityIssues;
    Fkind : string;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetdataQualityIssues(AIndex : Integer; AValue : TAccountStatusdataQualityIssues); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property dataQualityIssues : TAccountStatusdataQualityIssues Index 8 Read FdataQualityIssues Write SetdataQualityIssues;
    Property kind : string Index 16 Read Fkind Write Setkind;
  end;
  TAccountStatusClass = Class of TAccountStatus;
  
  { --------------------------------------------------------------------
    TAccountStatusdataQualityIssues
    --------------------------------------------------------------------}
  
  TAccountStatusdataQualityIssues = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountStatusdataQualityIssuesClass = Class of TAccountStatusdataQualityIssues;
  
  { --------------------------------------------------------------------
    TAccountStatusDataQualityIssue
    --------------------------------------------------------------------}
  
  TAccountStatusDataQualityIssue = Class(TGoogleBaseObject)
  Private
    Fcountry : string;
    FdisplayedValue : string;
    FexampleItems : TAccountStatusDataQualityIssueexampleItems;
    Fid : string;
    FlastChecked : string;
    FnumItems : integer;
    Fseverity : string;
    FsubmittedValue : string;
  Protected
    //Property setters
    Procedure Setcountry(AIndex : Integer; AValue : string); virtual;
    Procedure SetdisplayedValue(AIndex : Integer; AValue : string); virtual;
    Procedure SetexampleItems(AIndex : Integer; AValue : TAccountStatusDataQualityIssueexampleItems); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetlastChecked(AIndex : Integer; AValue : string); virtual;
    Procedure SetnumItems(AIndex : Integer; AValue : integer); virtual;
    Procedure Setseverity(AIndex : Integer; AValue : string); virtual;
    Procedure SetsubmittedValue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property country : string Index 0 Read Fcountry Write Setcountry;
    Property displayedValue : string Index 8 Read FdisplayedValue Write SetdisplayedValue;
    Property exampleItems : TAccountStatusDataQualityIssueexampleItems Index 16 Read FexampleItems Write SetexampleItems;
    Property id : string Index 24 Read Fid Write Setid;
    Property lastChecked : string Index 32 Read FlastChecked Write SetlastChecked;
    Property numItems : integer Index 40 Read FnumItems Write SetnumItems;
    Property severity : string Index 48 Read Fseverity Write Setseverity;
    Property submittedValue : string Index 56 Read FsubmittedValue Write SetsubmittedValue;
  end;
  TAccountStatusDataQualityIssueClass = Class of TAccountStatusDataQualityIssue;
  
  { --------------------------------------------------------------------
    TAccountStatusDataQualityIssueexampleItems
    --------------------------------------------------------------------}
  
  TAccountStatusDataQualityIssueexampleItems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountStatusDataQualityIssueexampleItemsClass = Class of TAccountStatusDataQualityIssueexampleItems;
  
  { --------------------------------------------------------------------
    TAccountStatusExampleItem
    --------------------------------------------------------------------}
  
  TAccountStatusExampleItem = Class(TGoogleBaseObject)
  Private
    FitemId : string;
    Flink : string;
    FsubmittedValue : string;
    Ftitle : string;
    FvalueOnLandingPage : string;
  Protected
    //Property setters
    Procedure SetitemId(AIndex : Integer; AValue : string); virtual;
    Procedure Setlink(AIndex : Integer; AValue : string); virtual;
    Procedure SetsubmittedValue(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure SetvalueOnLandingPage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property itemId : string Index 0 Read FitemId Write SetitemId;
    Property link : string Index 8 Read Flink Write Setlink;
    Property submittedValue : string Index 16 Read FsubmittedValue Write SetsubmittedValue;
    Property title : string Index 24 Read Ftitle Write Settitle;
    Property valueOnLandingPage : string Index 32 Read FvalueOnLandingPage Write SetvalueOnLandingPage;
  end;
  TAccountStatusExampleItemClass = Class of TAccountStatusExampleItem;
  
  { --------------------------------------------------------------------
    TAccountTax
    --------------------------------------------------------------------}
  
  TAccountTax = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    Fkind : string;
    Frules : TAccountTaxrules;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setrules(AIndex : Integer; AValue : TAccountTaxrules); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property rules : TAccountTaxrules Index 16 Read Frules Write Setrules;
  end;
  TAccountTaxClass = Class of TAccountTax;
  
  { --------------------------------------------------------------------
    TAccountTaxrules
    --------------------------------------------------------------------}
  
  TAccountTaxrules = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountTaxrulesClass = Class of TAccountTaxrules;
  
  { --------------------------------------------------------------------
    TAccountTaxTaxRule
    --------------------------------------------------------------------}
  
  TAccountTaxTaxRule = Class(TGoogleBaseObject)
  Private
    Fcountry : string;
    FlocationId : string;
    FratePercent : string;
    FshippingTaxed : boolean;
    FuseGlobalRate : boolean;
  Protected
    //Property setters
    Procedure Setcountry(AIndex : Integer; AValue : string); virtual;
    Procedure SetlocationId(AIndex : Integer; AValue : string); virtual;
    Procedure SetratePercent(AIndex : Integer; AValue : string); virtual;
    Procedure SetshippingTaxed(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetuseGlobalRate(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property country : string Index 0 Read Fcountry Write Setcountry;
    Property locationId : string Index 8 Read FlocationId Write SetlocationId;
    Property ratePercent : string Index 16 Read FratePercent Write SetratePercent;
    Property shippingTaxed : boolean Index 24 Read FshippingTaxed Write SetshippingTaxed;
    Property useGlobalRate : boolean Index 32 Read FuseGlobalRate Write SetuseGlobalRate;
  end;
  TAccountTaxTaxRuleClass = Class of TAccountTaxTaxRule;
  
  { --------------------------------------------------------------------
    TAccountUser
    --------------------------------------------------------------------}
  
  TAccountUser = Class(TGoogleBaseObject)
  Private
    Fadmin : boolean;
    FemailAddress : string;
  Protected
    //Property setters
    Procedure Setadmin(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetemailAddress(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property admin : boolean Index 0 Read Fadmin Write Setadmin;
    Property emailAddress : string Index 8 Read FemailAddress Write SetemailAddress;
  end;
  TAccountUserClass = Class of TAccountUser;
  
  { --------------------------------------------------------------------
    TAccountsAuthInfoResponse
    --------------------------------------------------------------------}
  
  TAccountsAuthInfoResponse = Class(TGoogleBaseObject)
  Private
    FaccountIdentifiers : TAccountsAuthInfoResponseaccountIdentifiers;
    Fkind : string;
  Protected
    //Property setters
    Procedure SetaccountIdentifiers(AIndex : Integer; AValue : TAccountsAuthInfoResponseaccountIdentifiers); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountIdentifiers : TAccountsAuthInfoResponseaccountIdentifiers Index 0 Read FaccountIdentifiers Write SetaccountIdentifiers;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TAccountsAuthInfoResponseClass = Class of TAccountsAuthInfoResponse;
  
  { --------------------------------------------------------------------
    TAccountsAuthInfoResponseaccountIdentifiers
    --------------------------------------------------------------------}
  
  TAccountsAuthInfoResponseaccountIdentifiers = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountsAuthInfoResponseaccountIdentifiersClass = Class of TAccountsAuthInfoResponseaccountIdentifiers;
  
  { --------------------------------------------------------------------
    TAccountsCustomBatchRequest
    --------------------------------------------------------------------}
  
  TAccountsCustomBatchRequest = Class(TGoogleBaseObject)
  Private
    Fentries : TAccountsCustomBatchRequestentries;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TAccountsCustomBatchRequestentries); virtual;
  Public
  Published
    Property entries : TAccountsCustomBatchRequestentries Index 0 Read Fentries Write Setentries;
  end;
  TAccountsCustomBatchRequestClass = Class of TAccountsCustomBatchRequest;
  
  { --------------------------------------------------------------------
    TAccountsCustomBatchRequestentries
    --------------------------------------------------------------------}
  
  TAccountsCustomBatchRequestentries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountsCustomBatchRequestentriesClass = Class of TAccountsCustomBatchRequestentries;
  
  { --------------------------------------------------------------------
    TAccountsCustomBatchRequestEntry
    --------------------------------------------------------------------}
  
  TAccountsCustomBatchRequestEntry = Class(TGoogleBaseObject)
  Private
    Faccount : TAccount;
    FaccountId : string;
    FbatchId : integer;
    FmerchantId : string;
    Fmethod : string;
  Protected
    //Property setters
    Procedure Setaccount(AIndex : Integer; AValue : TAccount); virtual;
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmerchantId(AIndex : Integer; AValue : string); virtual;
    Procedure Setmethod(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property account : TAccount Index 0 Read Faccount Write Setaccount;
    Property accountId : string Index 8 Read FaccountId Write SetaccountId;
    Property batchId : integer Index 16 Read FbatchId Write SetbatchId;
    Property merchantId : string Index 24 Read FmerchantId Write SetmerchantId;
    Property method : string Index 32 Read Fmethod Write Setmethod;
  end;
  TAccountsCustomBatchRequestEntryClass = Class of TAccountsCustomBatchRequestEntry;
  
  { --------------------------------------------------------------------
    TAccountsCustomBatchResponse
    --------------------------------------------------------------------}
  
  TAccountsCustomBatchResponse = Class(TGoogleBaseObject)
  Private
    Fentries : TAccountsCustomBatchResponseentries;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TAccountsCustomBatchResponseentries); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property entries : TAccountsCustomBatchResponseentries Index 0 Read Fentries Write Setentries;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TAccountsCustomBatchResponseClass = Class of TAccountsCustomBatchResponse;
  
  { --------------------------------------------------------------------
    TAccountsCustomBatchResponseentries
    --------------------------------------------------------------------}
  
  TAccountsCustomBatchResponseentries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountsCustomBatchResponseentriesClass = Class of TAccountsCustomBatchResponseentries;
  
  { --------------------------------------------------------------------
    TAccountsCustomBatchResponseEntry
    --------------------------------------------------------------------}
  
  TAccountsCustomBatchResponseEntry = Class(TGoogleBaseObject)
  Private
    Faccount : TAccount;
    FbatchId : integer;
    Ferrors : TDatafeedStatuserrors;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setaccount(AIndex : Integer; AValue : TAccount); virtual;
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TDatafeedStatuserrors); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property account : TAccount Index 0 Read Faccount Write Setaccount;
    Property batchId : integer Index 8 Read FbatchId Write SetbatchId;
    Property errors : TDatafeedStatuserrors Index 16 Read Ferrors Write Seterrors;
    Property kind : string Index 24 Read Fkind Write Setkind;
  end;
  TAccountsCustomBatchResponseEntryClass = Class of TAccountsCustomBatchResponseEntry;
  
  { --------------------------------------------------------------------
    TAccountsListResponse
    --------------------------------------------------------------------}
  
  TAccountsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    FnextPageToken : string;
    Fresources : TAccountsListResponseresources;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TAccountsListResponseresources); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
    Property resources : TAccountsListResponseresources Index 16 Read Fresources Write Setresources;
  end;
  TAccountsListResponseClass = Class of TAccountsListResponse;
  
  { --------------------------------------------------------------------
    TAccountsListResponseresources
    --------------------------------------------------------------------}
  
  TAccountsListResponseresources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountsListResponseresourcesClass = Class of TAccountsListResponseresources;
  
  { --------------------------------------------------------------------
    TAccountshippingCustomBatchRequest
    --------------------------------------------------------------------}
  
  TAccountshippingCustomBatchRequest = Class(TGoogleBaseObject)
  Private
    Fentries : TAccountshippingCustomBatchRequestentries;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TAccountshippingCustomBatchRequestentries); virtual;
  Public
  Published
    Property entries : TAccountshippingCustomBatchRequestentries Index 0 Read Fentries Write Setentries;
  end;
  TAccountshippingCustomBatchRequestClass = Class of TAccountshippingCustomBatchRequest;
  
  { --------------------------------------------------------------------
    TAccountshippingCustomBatchRequestentries
    --------------------------------------------------------------------}
  
  TAccountshippingCustomBatchRequestentries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountshippingCustomBatchRequestentriesClass = Class of TAccountshippingCustomBatchRequestentries;
  
  { --------------------------------------------------------------------
    TAccountshippingCustomBatchRequestEntry
    --------------------------------------------------------------------}
  
  TAccountshippingCustomBatchRequestEntry = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    FaccountShipping : TAccountShipping;
    FbatchId : integer;
    FmerchantId : string;
    Fmethod : string;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetaccountShipping(AIndex : Integer; AValue : TAccountShipping); virtual;
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmerchantId(AIndex : Integer; AValue : string); virtual;
    Procedure Setmethod(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property accountShipping : TAccountShipping Index 8 Read FaccountShipping Write SetaccountShipping;
    Property batchId : integer Index 16 Read FbatchId Write SetbatchId;
    Property merchantId : string Index 24 Read FmerchantId Write SetmerchantId;
    Property method : string Index 32 Read Fmethod Write Setmethod;
  end;
  TAccountshippingCustomBatchRequestEntryClass = Class of TAccountshippingCustomBatchRequestEntry;
  
  { --------------------------------------------------------------------
    TAccountshippingCustomBatchResponse
    --------------------------------------------------------------------}
  
  TAccountshippingCustomBatchResponse = Class(TGoogleBaseObject)
  Private
    Fentries : TAccountshippingCustomBatchResponseentries;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TAccountshippingCustomBatchResponseentries); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property entries : TAccountshippingCustomBatchResponseentries Index 0 Read Fentries Write Setentries;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TAccountshippingCustomBatchResponseClass = Class of TAccountshippingCustomBatchResponse;
  
  { --------------------------------------------------------------------
    TAccountshippingCustomBatchResponseentries
    --------------------------------------------------------------------}
  
  TAccountshippingCustomBatchResponseentries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountshippingCustomBatchResponseentriesClass = Class of TAccountshippingCustomBatchResponseentries;
  
  { --------------------------------------------------------------------
    TAccountshippingCustomBatchResponseEntry
    --------------------------------------------------------------------}
  
  TAccountshippingCustomBatchResponseEntry = Class(TGoogleBaseObject)
  Private
    FaccountShipping : TAccountShipping;
    FbatchId : integer;
    Ferrors : TDatafeedStatuserrors;
    Fkind : string;
  Protected
    //Property setters
    Procedure SetaccountShipping(AIndex : Integer; AValue : TAccountShipping); virtual;
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TDatafeedStatuserrors); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountShipping : TAccountShipping Index 0 Read FaccountShipping Write SetaccountShipping;
    Property batchId : integer Index 8 Read FbatchId Write SetbatchId;
    Property errors : TDatafeedStatuserrors Index 16 Read Ferrors Write Seterrors;
    Property kind : string Index 24 Read Fkind Write Setkind;
  end;
  TAccountshippingCustomBatchResponseEntryClass = Class of TAccountshippingCustomBatchResponseEntry;
  
  { --------------------------------------------------------------------
    TAccountshippingListResponse
    --------------------------------------------------------------------}
  
  TAccountshippingListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    FnextPageToken : string;
    Fresources : TAccountshippingListResponseresources;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TAccountshippingListResponseresources); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
    Property resources : TAccountshippingListResponseresources Index 16 Read Fresources Write Setresources;
  end;
  TAccountshippingListResponseClass = Class of TAccountshippingListResponse;
  
  { --------------------------------------------------------------------
    TAccountshippingListResponseresources
    --------------------------------------------------------------------}
  
  TAccountshippingListResponseresources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountshippingListResponseresourcesClass = Class of TAccountshippingListResponseresources;
  
  { --------------------------------------------------------------------
    TAccountstatusesCustomBatchRequest
    --------------------------------------------------------------------}
  
  TAccountstatusesCustomBatchRequest = Class(TGoogleBaseObject)
  Private
    Fentries : TAccountstatusesCustomBatchRequestentries;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TAccountstatusesCustomBatchRequestentries); virtual;
  Public
  Published
    Property entries : TAccountstatusesCustomBatchRequestentries Index 0 Read Fentries Write Setentries;
  end;
  TAccountstatusesCustomBatchRequestClass = Class of TAccountstatusesCustomBatchRequest;
  
  { --------------------------------------------------------------------
    TAccountstatusesCustomBatchRequestentries
    --------------------------------------------------------------------}
  
  TAccountstatusesCustomBatchRequestentries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountstatusesCustomBatchRequestentriesClass = Class of TAccountstatusesCustomBatchRequestentries;
  
  { --------------------------------------------------------------------
    TAccountstatusesCustomBatchRequestEntry
    --------------------------------------------------------------------}
  
  TAccountstatusesCustomBatchRequestEntry = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    FbatchId : integer;
    FmerchantId : string;
    Fmethod : string;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmerchantId(AIndex : Integer; AValue : string); virtual;
    Procedure Setmethod(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property batchId : integer Index 8 Read FbatchId Write SetbatchId;
    Property merchantId : string Index 16 Read FmerchantId Write SetmerchantId;
    Property method : string Index 24 Read Fmethod Write Setmethod;
  end;
  TAccountstatusesCustomBatchRequestEntryClass = Class of TAccountstatusesCustomBatchRequestEntry;
  
  { --------------------------------------------------------------------
    TAccountstatusesCustomBatchResponse
    --------------------------------------------------------------------}
  
  TAccountstatusesCustomBatchResponse = Class(TGoogleBaseObject)
  Private
    Fentries : TAccountstatusesCustomBatchResponseentries;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TAccountstatusesCustomBatchResponseentries); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property entries : TAccountstatusesCustomBatchResponseentries Index 0 Read Fentries Write Setentries;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TAccountstatusesCustomBatchResponseClass = Class of TAccountstatusesCustomBatchResponse;
  
  { --------------------------------------------------------------------
    TAccountstatusesCustomBatchResponseentries
    --------------------------------------------------------------------}
  
  TAccountstatusesCustomBatchResponseentries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountstatusesCustomBatchResponseentriesClass = Class of TAccountstatusesCustomBatchResponseentries;
  
  { --------------------------------------------------------------------
    TAccountstatusesCustomBatchResponseEntry
    --------------------------------------------------------------------}
  
  TAccountstatusesCustomBatchResponseEntry = Class(TGoogleBaseObject)
  Private
    FaccountStatus : TAccountStatus;
    FbatchId : integer;
    Ferrors : TDatafeedStatuserrors;
  Protected
    //Property setters
    Procedure SetaccountStatus(AIndex : Integer; AValue : TAccountStatus); virtual;
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TDatafeedStatuserrors); virtual;
  Public
  Published
    Property accountStatus : TAccountStatus Index 0 Read FaccountStatus Write SetaccountStatus;
    Property batchId : integer Index 8 Read FbatchId Write SetbatchId;
    Property errors : TDatafeedStatuserrors Index 16 Read Ferrors Write Seterrors;
  end;
  TAccountstatusesCustomBatchResponseEntryClass = Class of TAccountstatusesCustomBatchResponseEntry;
  
  { --------------------------------------------------------------------
    TAccountstatusesListResponse
    --------------------------------------------------------------------}
  
  TAccountstatusesListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    FnextPageToken : string;
    Fresources : TAccountstatusesListResponseresources;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TAccountstatusesListResponseresources); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
    Property resources : TAccountstatusesListResponseresources Index 16 Read Fresources Write Setresources;
  end;
  TAccountstatusesListResponseClass = Class of TAccountstatusesListResponse;
  
  { --------------------------------------------------------------------
    TAccountstatusesListResponseresources
    --------------------------------------------------------------------}
  
  TAccountstatusesListResponseresources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountstatusesListResponseresourcesClass = Class of TAccountstatusesListResponseresources;
  
  { --------------------------------------------------------------------
    TAccounttaxCustomBatchRequest
    --------------------------------------------------------------------}
  
  TAccounttaxCustomBatchRequest = Class(TGoogleBaseObject)
  Private
    Fentries : TAccounttaxCustomBatchRequestentries;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TAccounttaxCustomBatchRequestentries); virtual;
  Public
  Published
    Property entries : TAccounttaxCustomBatchRequestentries Index 0 Read Fentries Write Setentries;
  end;
  TAccounttaxCustomBatchRequestClass = Class of TAccounttaxCustomBatchRequest;
  
  { --------------------------------------------------------------------
    TAccounttaxCustomBatchRequestentries
    --------------------------------------------------------------------}
  
  TAccounttaxCustomBatchRequestentries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccounttaxCustomBatchRequestentriesClass = Class of TAccounttaxCustomBatchRequestentries;
  
  { --------------------------------------------------------------------
    TAccounttaxCustomBatchRequestEntry
    --------------------------------------------------------------------}
  
  TAccounttaxCustomBatchRequestEntry = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    FaccountTax : TAccountTax;
    FbatchId : integer;
    FmerchantId : string;
    Fmethod : string;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetaccountTax(AIndex : Integer; AValue : TAccountTax); virtual;
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmerchantId(AIndex : Integer; AValue : string); virtual;
    Procedure Setmethod(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property accountTax : TAccountTax Index 8 Read FaccountTax Write SetaccountTax;
    Property batchId : integer Index 16 Read FbatchId Write SetbatchId;
    Property merchantId : string Index 24 Read FmerchantId Write SetmerchantId;
    Property method : string Index 32 Read Fmethod Write Setmethod;
  end;
  TAccounttaxCustomBatchRequestEntryClass = Class of TAccounttaxCustomBatchRequestEntry;
  
  { --------------------------------------------------------------------
    TAccounttaxCustomBatchResponse
    --------------------------------------------------------------------}
  
  TAccounttaxCustomBatchResponse = Class(TGoogleBaseObject)
  Private
    Fentries : TAccounttaxCustomBatchResponseentries;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TAccounttaxCustomBatchResponseentries); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property entries : TAccounttaxCustomBatchResponseentries Index 0 Read Fentries Write Setentries;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TAccounttaxCustomBatchResponseClass = Class of TAccounttaxCustomBatchResponse;
  
  { --------------------------------------------------------------------
    TAccounttaxCustomBatchResponseentries
    --------------------------------------------------------------------}
  
  TAccounttaxCustomBatchResponseentries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccounttaxCustomBatchResponseentriesClass = Class of TAccounttaxCustomBatchResponseentries;
  
  { --------------------------------------------------------------------
    TAccounttaxCustomBatchResponseEntry
    --------------------------------------------------------------------}
  
  TAccounttaxCustomBatchResponseEntry = Class(TGoogleBaseObject)
  Private
    FaccountTax : TAccountTax;
    FbatchId : integer;
    Ferrors : TDatafeedStatuserrors;
    Fkind : string;
  Protected
    //Property setters
    Procedure SetaccountTax(AIndex : Integer; AValue : TAccountTax); virtual;
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TDatafeedStatuserrors); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountTax : TAccountTax Index 0 Read FaccountTax Write SetaccountTax;
    Property batchId : integer Index 8 Read FbatchId Write SetbatchId;
    Property errors : TDatafeedStatuserrors Index 16 Read Ferrors Write Seterrors;
    Property kind : string Index 24 Read Fkind Write Setkind;
  end;
  TAccounttaxCustomBatchResponseEntryClass = Class of TAccounttaxCustomBatchResponseEntry;
  
  { --------------------------------------------------------------------
    TAccounttaxListResponse
    --------------------------------------------------------------------}
  
  TAccounttaxListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    FnextPageToken : string;
    Fresources : TAccounttaxListResponseresources;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TAccounttaxListResponseresources); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
    Property resources : TAccounttaxListResponseresources Index 16 Read Fresources Write Setresources;
  end;
  TAccounttaxListResponseClass = Class of TAccounttaxListResponse;
  
  { --------------------------------------------------------------------
    TAccounttaxListResponseresources
    --------------------------------------------------------------------}
  
  TAccounttaxListResponseresources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccounttaxListResponseresourcesClass = Class of TAccounttaxListResponseresources;
  
  { --------------------------------------------------------------------
    TDatafeed
    --------------------------------------------------------------------}
  
  TDatafeed = Class(TGoogleBaseObject)
  Private
    FattributeLanguage : string;
    FcontentLanguage : string;
    FcontentType : string;
    FfetchSchedule : TDatafeedFetchSchedule;
    FfileName : string;
    Fformat : TDatafeedFormat;
    Fid : string;
    FintendedDestinations : TDatafeedintendedDestinations;
    Fkind : string;
    Fname : string;
    FtargetCountry : string;
  Protected
    //Property setters
    Procedure SetattributeLanguage(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontentLanguage(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontentType(AIndex : Integer; AValue : string); virtual;
    Procedure SetfetchSchedule(AIndex : Integer; AValue : TDatafeedFetchSchedule); virtual;
    Procedure SetfileName(AIndex : Integer; AValue : string); virtual;
    Procedure Setformat(AIndex : Integer; AValue : TDatafeedFormat); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetintendedDestinations(AIndex : Integer; AValue : TDatafeedintendedDestinations); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetCountry(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property attributeLanguage : string Index 0 Read FattributeLanguage Write SetattributeLanguage;
    Property contentLanguage : string Index 8 Read FcontentLanguage Write SetcontentLanguage;
    Property contentType : string Index 16 Read FcontentType Write SetcontentType;
    Property fetchSchedule : TDatafeedFetchSchedule Index 24 Read FfetchSchedule Write SetfetchSchedule;
    Property fileName : string Index 32 Read FfileName Write SetfileName;
    Property format : TDatafeedFormat Index 40 Read Fformat Write Setformat;
    Property id : string Index 48 Read Fid Write Setid;
    Property intendedDestinations : TDatafeedintendedDestinations Index 56 Read FintendedDestinations Write SetintendedDestinations;
    Property kind : string Index 64 Read Fkind Write Setkind;
    Property name : string Index 72 Read Fname Write Setname;
    Property targetCountry : string Index 80 Read FtargetCountry Write SettargetCountry;
  end;
  TDatafeedClass = Class of TDatafeed;
  
  { --------------------------------------------------------------------
    TDatafeedintendedDestinations
    --------------------------------------------------------------------}
  
  TDatafeedintendedDestinations = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDatafeedintendedDestinationsClass = Class of TDatafeedintendedDestinations;
  
  { --------------------------------------------------------------------
    TDatafeedFetchSchedule
    --------------------------------------------------------------------}
  
  TDatafeedFetchSchedule = Class(TGoogleBaseObject)
  Private
    FdayOfMonth : integer;
    FfetchUrl : string;
    Fhour : integer;
    Fpassword : string;
    FtimeZone : string;
    Fusername : string;
    Fweekday : string;
  Protected
    //Property setters
    Procedure SetdayOfMonth(AIndex : Integer; AValue : integer); virtual;
    Procedure SetfetchUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Sethour(AIndex : Integer; AValue : integer); virtual;
    Procedure Setpassword(AIndex : Integer; AValue : string); virtual;
    Procedure SettimeZone(AIndex : Integer; AValue : string); virtual;
    Procedure Setusername(AIndex : Integer; AValue : string); virtual;
    Procedure Setweekday(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property dayOfMonth : integer Index 0 Read FdayOfMonth Write SetdayOfMonth;
    Property fetchUrl : string Index 8 Read FfetchUrl Write SetfetchUrl;
    Property hour : integer Index 16 Read Fhour Write Sethour;
    Property password : string Index 24 Read Fpassword Write Setpassword;
    Property timeZone : string Index 32 Read FtimeZone Write SettimeZone;
    Property username : string Index 40 Read Fusername Write Setusername;
    Property weekday : string Index 48 Read Fweekday Write Setweekday;
  end;
  TDatafeedFetchScheduleClass = Class of TDatafeedFetchSchedule;
  
  { --------------------------------------------------------------------
    TDatafeedFormat
    --------------------------------------------------------------------}
  
  TDatafeedFormat = Class(TGoogleBaseObject)
  Private
    FcolumnDelimiter : string;
    FfileEncoding : string;
    FquotingMode : string;
  Protected
    //Property setters
    Procedure SetcolumnDelimiter(AIndex : Integer; AValue : string); virtual;
    Procedure SetfileEncoding(AIndex : Integer; AValue : string); virtual;
    Procedure SetquotingMode(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property columnDelimiter : string Index 0 Read FcolumnDelimiter Write SetcolumnDelimiter;
    Property fileEncoding : string Index 8 Read FfileEncoding Write SetfileEncoding;
    Property quotingMode : string Index 16 Read FquotingMode Write SetquotingMode;
  end;
  TDatafeedFormatClass = Class of TDatafeedFormat;
  
  { --------------------------------------------------------------------
    TDatafeedStatus
    --------------------------------------------------------------------}
  
  TDatafeedStatus = Class(TGoogleBaseObject)
  Private
    FdatafeedId : string;
    Ferrors : TDatafeedStatuserrors;
    FitemsTotal : string;
    FitemsValid : string;
    Fkind : string;
    FlastUploadDate : string;
    FprocessingStatus : string;
    Fwarnings : TDatafeedStatuswarnings;
  Protected
    //Property setters
    Procedure SetdatafeedId(AIndex : Integer; AValue : string); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TDatafeedStatuserrors); virtual;
    Procedure SetitemsTotal(AIndex : Integer; AValue : string); virtual;
    Procedure SetitemsValid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetlastUploadDate(AIndex : Integer; AValue : string); virtual;
    Procedure SetprocessingStatus(AIndex : Integer; AValue : string); virtual;
    Procedure Setwarnings(AIndex : Integer; AValue : TDatafeedStatuswarnings); virtual;
  Public
  Published
    Property datafeedId : string Index 0 Read FdatafeedId Write SetdatafeedId;
    Property errors : TDatafeedStatuserrors Index 8 Read Ferrors Write Seterrors;
    Property itemsTotal : string Index 16 Read FitemsTotal Write SetitemsTotal;
    Property itemsValid : string Index 24 Read FitemsValid Write SetitemsValid;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property lastUploadDate : string Index 40 Read FlastUploadDate Write SetlastUploadDate;
    Property processingStatus : string Index 48 Read FprocessingStatus Write SetprocessingStatus;
    Property warnings : TDatafeedStatuswarnings Index 56 Read Fwarnings Write Setwarnings;
  end;
  TDatafeedStatusClass = Class of TDatafeedStatus;
  
  { --------------------------------------------------------------------
    TDatafeedStatuserrors
    --------------------------------------------------------------------}
  
  TDatafeedStatuserrors = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDatafeedStatuserrorsClass = Class of TDatafeedStatuserrors;
  
  { --------------------------------------------------------------------
    TDatafeedStatuswarnings
    --------------------------------------------------------------------}
  
  TDatafeedStatuswarnings = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDatafeedStatuswarningsClass = Class of TDatafeedStatuswarnings;
  
  { --------------------------------------------------------------------
    TDatafeedStatusError
    --------------------------------------------------------------------}
  
  TDatafeedStatusError = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fcount : string;
    Fexamples : TDatafeedStatusErrorexamples;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setcount(AIndex : Integer; AValue : string); virtual;
    Procedure Setexamples(AIndex : Integer; AValue : TDatafeedStatusErrorexamples); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property count : string Index 8 Read Fcount Write Setcount;
    Property examples : TDatafeedStatusErrorexamples Index 16 Read Fexamples Write Setexamples;
    Property message : string Index 24 Read Fmessage Write Setmessage;
  end;
  TDatafeedStatusErrorClass = Class of TDatafeedStatusError;
  
  { --------------------------------------------------------------------
    TDatafeedStatusErrorexamples
    --------------------------------------------------------------------}
  
  TDatafeedStatusErrorexamples = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDatafeedStatusErrorexamplesClass = Class of TDatafeedStatusErrorexamples;
  
  { --------------------------------------------------------------------
    TDatafeedStatusExample
    --------------------------------------------------------------------}
  
  TDatafeedStatusExample = Class(TGoogleBaseObject)
  Private
    FitemId : string;
    FlineNumber : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure SetitemId(AIndex : Integer; AValue : string); virtual;
    Procedure SetlineNumber(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property itemId : string Index 0 Read FitemId Write SetitemId;
    Property lineNumber : string Index 8 Read FlineNumber Write SetlineNumber;
    Property value : string Index 16 Read Fvalue Write Setvalue;
  end;
  TDatafeedStatusExampleClass = Class of TDatafeedStatusExample;
  
  { --------------------------------------------------------------------
    TDatafeedsCustomBatchRequest
    --------------------------------------------------------------------}
  
  TDatafeedsCustomBatchRequest = Class(TGoogleBaseObject)
  Private
    Fentries : TDatafeedsCustomBatchRequestentries;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TDatafeedsCustomBatchRequestentries); virtual;
  Public
  Published
    Property entries : TDatafeedsCustomBatchRequestentries Index 0 Read Fentries Write Setentries;
  end;
  TDatafeedsCustomBatchRequestClass = Class of TDatafeedsCustomBatchRequest;
  
  { --------------------------------------------------------------------
    TDatafeedsCustomBatchRequestentries
    --------------------------------------------------------------------}
  
  TDatafeedsCustomBatchRequestentries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDatafeedsCustomBatchRequestentriesClass = Class of TDatafeedsCustomBatchRequestentries;
  
  { --------------------------------------------------------------------
    TDatafeedsCustomBatchRequestEntry
    --------------------------------------------------------------------}
  
  TDatafeedsCustomBatchRequestEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    Fdatafeed : TDatafeed;
    FdatafeedId : string;
    FmerchantId : string;
    Fmethod : string;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Setdatafeed(AIndex : Integer; AValue : TDatafeed); virtual;
    Procedure SetdatafeedId(AIndex : Integer; AValue : string); virtual;
    Procedure SetmerchantId(AIndex : Integer; AValue : string); virtual;
    Procedure Setmethod(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property datafeed : TDatafeed Index 8 Read Fdatafeed Write Setdatafeed;
    Property datafeedId : string Index 16 Read FdatafeedId Write SetdatafeedId;
    Property merchantId : string Index 24 Read FmerchantId Write SetmerchantId;
    Property method : string Index 32 Read Fmethod Write Setmethod;
  end;
  TDatafeedsCustomBatchRequestEntryClass = Class of TDatafeedsCustomBatchRequestEntry;
  
  { --------------------------------------------------------------------
    TDatafeedsCustomBatchResponse
    --------------------------------------------------------------------}
  
  TDatafeedsCustomBatchResponse = Class(TGoogleBaseObject)
  Private
    Fentries : TDatafeedsCustomBatchResponseentries;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TDatafeedsCustomBatchResponseentries); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property entries : TDatafeedsCustomBatchResponseentries Index 0 Read Fentries Write Setentries;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TDatafeedsCustomBatchResponseClass = Class of TDatafeedsCustomBatchResponse;
  
  { --------------------------------------------------------------------
    TDatafeedsCustomBatchResponseentries
    --------------------------------------------------------------------}
  
  TDatafeedsCustomBatchResponseentries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDatafeedsCustomBatchResponseentriesClass = Class of TDatafeedsCustomBatchResponseentries;
  
  { --------------------------------------------------------------------
    TDatafeedsCustomBatchResponseEntry
    --------------------------------------------------------------------}
  
  TDatafeedsCustomBatchResponseEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    Fdatafeed : TDatafeed;
    Ferrors : TDatafeedStatuserrors;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Setdatafeed(AIndex : Integer; AValue : TDatafeed); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TDatafeedStatuserrors); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property datafeed : TDatafeed Index 8 Read Fdatafeed Write Setdatafeed;
    Property errors : TDatafeedStatuserrors Index 16 Read Ferrors Write Seterrors;
  end;
  TDatafeedsCustomBatchResponseEntryClass = Class of TDatafeedsCustomBatchResponseEntry;
  
  { --------------------------------------------------------------------
    TDatafeedsListResponse
    --------------------------------------------------------------------}
  
  TDatafeedsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    FnextPageToken : string;
    Fresources : TDatafeedsListResponseresources;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TDatafeedsListResponseresources); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
    Property resources : TDatafeedsListResponseresources Index 16 Read Fresources Write Setresources;
  end;
  TDatafeedsListResponseClass = Class of TDatafeedsListResponse;
  
  { --------------------------------------------------------------------
    TDatafeedsListResponseresources
    --------------------------------------------------------------------}
  
  TDatafeedsListResponseresources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDatafeedsListResponseresourcesClass = Class of TDatafeedsListResponseresources;
  
  { --------------------------------------------------------------------
    TDatafeedstatusesCustomBatchRequest
    --------------------------------------------------------------------}
  
  TDatafeedstatusesCustomBatchRequest = Class(TGoogleBaseObject)
  Private
    Fentries : TDatafeedstatusesCustomBatchRequestentries;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TDatafeedstatusesCustomBatchRequestentries); virtual;
  Public
  Published
    Property entries : TDatafeedstatusesCustomBatchRequestentries Index 0 Read Fentries Write Setentries;
  end;
  TDatafeedstatusesCustomBatchRequestClass = Class of TDatafeedstatusesCustomBatchRequest;
  
  { --------------------------------------------------------------------
    TDatafeedstatusesCustomBatchRequestentries
    --------------------------------------------------------------------}
  
  TDatafeedstatusesCustomBatchRequestentries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDatafeedstatusesCustomBatchRequestentriesClass = Class of TDatafeedstatusesCustomBatchRequestentries;
  
  { --------------------------------------------------------------------
    TDatafeedstatusesCustomBatchRequestEntry
    --------------------------------------------------------------------}
  
  TDatafeedstatusesCustomBatchRequestEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    FdatafeedId : string;
    FmerchantId : string;
    Fmethod : string;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetdatafeedId(AIndex : Integer; AValue : string); virtual;
    Procedure SetmerchantId(AIndex : Integer; AValue : string); virtual;
    Procedure Setmethod(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property datafeedId : string Index 8 Read FdatafeedId Write SetdatafeedId;
    Property merchantId : string Index 16 Read FmerchantId Write SetmerchantId;
    Property method : string Index 24 Read Fmethod Write Setmethod;
  end;
  TDatafeedstatusesCustomBatchRequestEntryClass = Class of TDatafeedstatusesCustomBatchRequestEntry;
  
  { --------------------------------------------------------------------
    TDatafeedstatusesCustomBatchResponse
    --------------------------------------------------------------------}
  
  TDatafeedstatusesCustomBatchResponse = Class(TGoogleBaseObject)
  Private
    Fentries : TDatafeedstatusesCustomBatchResponseentries;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TDatafeedstatusesCustomBatchResponseentries); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property entries : TDatafeedstatusesCustomBatchResponseentries Index 0 Read Fentries Write Setentries;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TDatafeedstatusesCustomBatchResponseClass = Class of TDatafeedstatusesCustomBatchResponse;
  
  { --------------------------------------------------------------------
    TDatafeedstatusesCustomBatchResponseentries
    --------------------------------------------------------------------}
  
  TDatafeedstatusesCustomBatchResponseentries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDatafeedstatusesCustomBatchResponseentriesClass = Class of TDatafeedstatusesCustomBatchResponseentries;
  
  { --------------------------------------------------------------------
    TDatafeedstatusesCustomBatchResponseEntry
    --------------------------------------------------------------------}
  
  TDatafeedstatusesCustomBatchResponseEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    FdatafeedStatus : TDatafeedStatus;
    Ferrors : TDatafeedStatuserrors;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetdatafeedStatus(AIndex : Integer; AValue : TDatafeedStatus); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TDatafeedStatuserrors); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property datafeedStatus : TDatafeedStatus Index 8 Read FdatafeedStatus Write SetdatafeedStatus;
    Property errors : TDatafeedStatuserrors Index 16 Read Ferrors Write Seterrors;
  end;
  TDatafeedstatusesCustomBatchResponseEntryClass = Class of TDatafeedstatusesCustomBatchResponseEntry;
  
  { --------------------------------------------------------------------
    TDatafeedstatusesListResponse
    --------------------------------------------------------------------}
  
  TDatafeedstatusesListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    FnextPageToken : string;
    Fresources : TDatafeedstatusesListResponseresources;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TDatafeedstatusesListResponseresources); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
    Property resources : TDatafeedstatusesListResponseresources Index 16 Read Fresources Write Setresources;
  end;
  TDatafeedstatusesListResponseClass = Class of TDatafeedstatusesListResponse;
  
  { --------------------------------------------------------------------
    TDatafeedstatusesListResponseresources
    --------------------------------------------------------------------}
  
  TDatafeedstatusesListResponseresources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDatafeedstatusesListResponseresourcesClass = Class of TDatafeedstatusesListResponseresources;
  
  { --------------------------------------------------------------------
    TError
    --------------------------------------------------------------------}
  
  TError = Class(TGoogleBaseObject)
  Private
    Fdomain : string;
    Fmessage : string;
    Freason : string;
  Protected
    //Property setters
    Procedure Setdomain(AIndex : Integer; AValue : string); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
    Procedure Setreason(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property domain : string Index 0 Read Fdomain Write Setdomain;
    Property message : string Index 8 Read Fmessage Write Setmessage;
    Property reason : string Index 16 Read Freason Write Setreason;
  end;
  TErrorClass = Class of TError;
  
  { --------------------------------------------------------------------
    TErrors
    --------------------------------------------------------------------}
  
  TErrors = Class(TGoogleBaseObject)
  Private
    Fcode : integer;
    Ferrors : TErrorserrors;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : integer); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TErrorserrors); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : integer Index 0 Read Fcode Write Setcode;
    Property errors : TErrorserrors Index 8 Read Ferrors Write Seterrors;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TErrorsClass = Class of TErrors;
  
  { --------------------------------------------------------------------
    TErrorserrors
    --------------------------------------------------------------------}
  
  TErrorserrors = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TErrorserrorsClass = Class of TErrorserrors;
  
  { --------------------------------------------------------------------
    TInventory
    --------------------------------------------------------------------}
  
  TInventory = Class(TGoogleBaseObject)
  Private
    Favailability : string;
    Fkind : string;
    Fprice : TPrice;
    Fquantity : integer;
    FsalePrice : TPrice;
    FsalePriceEffectiveDate : string;
  Protected
    //Property setters
    Procedure Setavailability(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setprice(AIndex : Integer; AValue : TPrice); virtual;
    Procedure Setquantity(AIndex : Integer; AValue : integer); virtual;
    Procedure SetsalePrice(AIndex : Integer; AValue : TPrice); virtual;
    Procedure SetsalePriceEffectiveDate(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property availability : string Index 0 Read Favailability Write Setavailability;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property price : TPrice Index 16 Read Fprice Write Setprice;
    Property quantity : integer Index 24 Read Fquantity Write Setquantity;
    Property salePrice : TPrice Index 32 Read FsalePrice Write SetsalePrice;
    Property salePriceEffectiveDate : string Index 40 Read FsalePriceEffectiveDate Write SetsalePriceEffectiveDate;
  end;
  TInventoryClass = Class of TInventory;
  
  { --------------------------------------------------------------------
    TInventoryCustomBatchRequest
    --------------------------------------------------------------------}
  
  TInventoryCustomBatchRequest = Class(TGoogleBaseObject)
  Private
    Fentries : TInventoryCustomBatchRequestentries;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TInventoryCustomBatchRequestentries); virtual;
  Public
  Published
    Property entries : TInventoryCustomBatchRequestentries Index 0 Read Fentries Write Setentries;
  end;
  TInventoryCustomBatchRequestClass = Class of TInventoryCustomBatchRequest;
  
  { --------------------------------------------------------------------
    TInventoryCustomBatchRequestentries
    --------------------------------------------------------------------}
  
  TInventoryCustomBatchRequestentries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInventoryCustomBatchRequestentriesClass = Class of TInventoryCustomBatchRequestentries;
  
  { --------------------------------------------------------------------
    TInventoryCustomBatchRequestEntry
    --------------------------------------------------------------------}
  
  TInventoryCustomBatchRequestEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    Finventory : TInventory;
    FmerchantId : string;
    FproductId : string;
    FstoreCode : string;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Setinventory(AIndex : Integer; AValue : TInventory); virtual;
    Procedure SetmerchantId(AIndex : Integer; AValue : string); virtual;
    Procedure SetproductId(AIndex : Integer; AValue : string); virtual;
    Procedure SetstoreCode(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property inventory : TInventory Index 8 Read Finventory Write Setinventory;
    Property merchantId : string Index 16 Read FmerchantId Write SetmerchantId;
    Property productId : string Index 24 Read FproductId Write SetproductId;
    Property storeCode : string Index 32 Read FstoreCode Write SetstoreCode;
  end;
  TInventoryCustomBatchRequestEntryClass = Class of TInventoryCustomBatchRequestEntry;
  
  { --------------------------------------------------------------------
    TInventoryCustomBatchResponse
    --------------------------------------------------------------------}
  
  TInventoryCustomBatchResponse = Class(TGoogleBaseObject)
  Private
    Fentries : TInventoryCustomBatchResponseentries;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TInventoryCustomBatchResponseentries); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property entries : TInventoryCustomBatchResponseentries Index 0 Read Fentries Write Setentries;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TInventoryCustomBatchResponseClass = Class of TInventoryCustomBatchResponse;
  
  { --------------------------------------------------------------------
    TInventoryCustomBatchResponseentries
    --------------------------------------------------------------------}
  
  TInventoryCustomBatchResponseentries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInventoryCustomBatchResponseentriesClass = Class of TInventoryCustomBatchResponseentries;
  
  { --------------------------------------------------------------------
    TInventoryCustomBatchResponseEntry
    --------------------------------------------------------------------}
  
  TInventoryCustomBatchResponseEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    Ferrors : TDatafeedStatuserrors;
    Fkind : string;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TDatafeedStatuserrors); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property errors : TDatafeedStatuserrors Index 8 Read Ferrors Write Seterrors;
    Property kind : string Index 16 Read Fkind Write Setkind;
  end;
  TInventoryCustomBatchResponseEntryClass = Class of TInventoryCustomBatchResponseEntry;
  
  { --------------------------------------------------------------------
    TInventorySetRequest
    --------------------------------------------------------------------}
  
  TInventorySetRequest = Class(TGoogleBaseObject)
  Private
    Favailability : string;
    Fprice : TPrice;
    Fquantity : integer;
    FsalePrice : TPrice;
    FsalePriceEffectiveDate : string;
  Protected
    //Property setters
    Procedure Setavailability(AIndex : Integer; AValue : string); virtual;
    Procedure Setprice(AIndex : Integer; AValue : TPrice); virtual;
    Procedure Setquantity(AIndex : Integer; AValue : integer); virtual;
    Procedure SetsalePrice(AIndex : Integer; AValue : TPrice); virtual;
    Procedure SetsalePriceEffectiveDate(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property availability : string Index 0 Read Favailability Write Setavailability;
    Property price : TPrice Index 8 Read Fprice Write Setprice;
    Property quantity : integer Index 16 Read Fquantity Write Setquantity;
    Property salePrice : TPrice Index 24 Read FsalePrice Write SetsalePrice;
    Property salePriceEffectiveDate : string Index 32 Read FsalePriceEffectiveDate Write SetsalePriceEffectiveDate;
  end;
  TInventorySetRequestClass = Class of TInventorySetRequest;
  
  { --------------------------------------------------------------------
    TInventorySetResponse
    --------------------------------------------------------------------}
  
  TInventorySetResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
  end;
  TInventorySetResponseClass = Class of TInventorySetResponse;
  
  { --------------------------------------------------------------------
    TLoyaltyPoints
    --------------------------------------------------------------------}
  
  TLoyaltyPoints = Class(TGoogleBaseObject)
  Private
    Fname : string;
    FpointsValue : string;
    Fratio : double;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetpointsValue(AIndex : Integer; AValue : string); virtual;
    Procedure Setratio(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
    Property pointsValue : string Index 8 Read FpointsValue Write SetpointsValue;
    Property ratio : double Index 16 Read Fratio Write Setratio;
  end;
  TLoyaltyPointsClass = Class of TLoyaltyPoints;
  
  { --------------------------------------------------------------------
    TPrice
    --------------------------------------------------------------------}
  
  TPrice = Class(TGoogleBaseObject)
  Private
    Fcurrency : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setcurrency(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property currency : string Index 0 Read Fcurrency Write Setcurrency;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TPriceClass = Class of TPrice;
  
  { --------------------------------------------------------------------
    TProduct
    --------------------------------------------------------------------}
  
  TProduct = Class(TGoogleBaseObject)
  Private
    FadditionalImageLinks : TProductadditionalImageLinks;
    Fadult : boolean;
    FadwordsGrouping : string;
    FadwordsLabels : TProductadwordsLabels;
    FadwordsRedirect : string;
    FageGroup : string;
    Faspects : TProductaspects;
    Favailability : string;
    FavailabilityDate : string;
    Fbrand : string;
    Fchannel : string;
    Fcolor : string;
    Fcondition : string;
    FcontentLanguage : string;
    FcustomAttributes : TProductcustomAttributes;
    FcustomGroups : TProductcustomGroups;
    FcustomLabel0 : string;
    FcustomLabel1 : string;
    FcustomLabel2 : string;
    FcustomLabel3 : string;
    FcustomLabel4 : string;
    Fdescription : string;
    Fdestinations : TProductdestinations;
    FdisplayAdsId : string;
    FdisplayAdsLink : string;
    FdisplayAdsSimilarIds : TProductdisplayAdsSimilarIds;
    FdisplayAdsTitle : string;
    FdisplayAdsValue : double;
    FenergyEfficiencyClass : string;
    FexpirationDate : string;
    Fgender : string;
    FgoogleProductCategory : string;
    Fgtin : string;
    Fid : string;
    FidentifierExists : boolean;
    FimageLink : string;
    Finstallment : TProductInstallment;
    FisBundle : boolean;
    FitemGroupId : string;
    Fkind : string;
    Flink : string;
    FloyaltyPoints : TLoyaltyPoints;
    Fmaterial : string;
    FmobileLink : string;
    Fmpn : string;
    Fmultipack : string;
    FofferId : string;
    FonlineOnly : boolean;
    Fpattern : string;
    Fprice : TPrice;
    FproductType : string;
    FsalePrice : TPrice;
    FsalePriceEffectiveDate : string;
    Fshipping : TProductshipping;
    FshippingHeight : TProductShippingDimension;
    FshippingLabel : string;
    FshippingLength : TProductShippingDimension;
    FshippingWeight : TProductShippingWeight;
    FshippingWidth : TProductShippingDimension;
    FsizeSystem : string;
    FsizeType : string;
    Fsizes : TProductsizes;
    FtargetCountry : string;
    Ftaxes : TProducttaxes;
    Ftitle : string;
    FunitPricingBaseMeasure : TProductUnitPricingBaseMeasure;
    FunitPricingMeasure : TProductUnitPricingMeasure;
    FvalidatedDestinations : TProductvalidatedDestinations;
    Fwarnings : TProductwarnings;
  Protected
    //Property setters
    Procedure SetadditionalImageLinks(AIndex : Integer; AValue : TProductadditionalImageLinks); virtual;
    Procedure Setadult(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetadwordsGrouping(AIndex : Integer; AValue : string); virtual;
    Procedure SetadwordsLabels(AIndex : Integer; AValue : TProductadwordsLabels); virtual;
    Procedure SetadwordsRedirect(AIndex : Integer; AValue : string); virtual;
    Procedure SetageGroup(AIndex : Integer; AValue : string); virtual;
    Procedure Setaspects(AIndex : Integer; AValue : TProductaspects); virtual;
    Procedure Setavailability(AIndex : Integer; AValue : string); virtual;
    Procedure SetavailabilityDate(AIndex : Integer; AValue : string); virtual;
    Procedure Setbrand(AIndex : Integer; AValue : string); virtual;
    Procedure Setchannel(AIndex : Integer; AValue : string); virtual;
    Procedure Setcolor(AIndex : Integer; AValue : string); virtual;
    Procedure Setcondition(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontentLanguage(AIndex : Integer; AValue : string); virtual;
    Procedure SetcustomAttributes(AIndex : Integer; AValue : TProductcustomAttributes); virtual;
    Procedure SetcustomGroups(AIndex : Integer; AValue : TProductcustomGroups); virtual;
    Procedure SetcustomLabel0(AIndex : Integer; AValue : string); virtual;
    Procedure SetcustomLabel1(AIndex : Integer; AValue : string); virtual;
    Procedure SetcustomLabel2(AIndex : Integer; AValue : string); virtual;
    Procedure SetcustomLabel3(AIndex : Integer; AValue : string); virtual;
    Procedure SetcustomLabel4(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setdestinations(AIndex : Integer; AValue : TProductdestinations); virtual;
    Procedure SetdisplayAdsId(AIndex : Integer; AValue : string); virtual;
    Procedure SetdisplayAdsLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetdisplayAdsSimilarIds(AIndex : Integer; AValue : TProductdisplayAdsSimilarIds); virtual;
    Procedure SetdisplayAdsTitle(AIndex : Integer; AValue : string); virtual;
    Procedure SetdisplayAdsValue(AIndex : Integer; AValue : double); virtual;
    Procedure SetenergyEfficiencyClass(AIndex : Integer; AValue : string); virtual;
    Procedure SetexpirationDate(AIndex : Integer; AValue : string); virtual;
    Procedure Setgender(AIndex : Integer; AValue : string); virtual;
    Procedure SetgoogleProductCategory(AIndex : Integer; AValue : string); virtual;
    Procedure Setgtin(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetidentifierExists(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetimageLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setinstallment(AIndex : Integer; AValue : TProductInstallment); virtual;
    Procedure SetisBundle(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetitemGroupId(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlink(AIndex : Integer; AValue : string); virtual;
    Procedure SetloyaltyPoints(AIndex : Integer; AValue : TLoyaltyPoints); virtual;
    Procedure Setmaterial(AIndex : Integer; AValue : string); virtual;
    Procedure SetmobileLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setmpn(AIndex : Integer; AValue : string); virtual;
    Procedure Setmultipack(AIndex : Integer; AValue : string); virtual;
    Procedure SetofferId(AIndex : Integer; AValue : string); virtual;
    Procedure SetonlineOnly(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setpattern(AIndex : Integer; AValue : string); virtual;
    Procedure Setprice(AIndex : Integer; AValue : TPrice); virtual;
    Procedure SetproductType(AIndex : Integer; AValue : string); virtual;
    Procedure SetsalePrice(AIndex : Integer; AValue : TPrice); virtual;
    Procedure SetsalePriceEffectiveDate(AIndex : Integer; AValue : string); virtual;
    Procedure Setshipping(AIndex : Integer; AValue : TProductshipping); virtual;
    Procedure SetshippingHeight(AIndex : Integer; AValue : TProductShippingDimension); virtual;
    Procedure SetshippingLabel(AIndex : Integer; AValue : string); virtual;
    Procedure SetshippingLength(AIndex : Integer; AValue : TProductShippingDimension); virtual;
    Procedure SetshippingWeight(AIndex : Integer; AValue : TProductShippingWeight); virtual;
    Procedure SetshippingWidth(AIndex : Integer; AValue : TProductShippingDimension); virtual;
    Procedure SetsizeSystem(AIndex : Integer; AValue : string); virtual;
    Procedure SetsizeType(AIndex : Integer; AValue : string); virtual;
    Procedure Setsizes(AIndex : Integer; AValue : TProductsizes); virtual;
    Procedure SettargetCountry(AIndex : Integer; AValue : string); virtual;
    Procedure Settaxes(AIndex : Integer; AValue : TProducttaxes); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure SetunitPricingBaseMeasure(AIndex : Integer; AValue : TProductUnitPricingBaseMeasure); virtual;
    Procedure SetunitPricingMeasure(AIndex : Integer; AValue : TProductUnitPricingMeasure); virtual;
    Procedure SetvalidatedDestinations(AIndex : Integer; AValue : TProductvalidatedDestinations); virtual;
    Procedure Setwarnings(AIndex : Integer; AValue : TProductwarnings); virtual;
  Public
  Published
    Property additionalImageLinks : TProductadditionalImageLinks Index 0 Read FadditionalImageLinks Write SetadditionalImageLinks;
    Property adult : boolean Index 8 Read Fadult Write Setadult;
    Property adwordsGrouping : string Index 16 Read FadwordsGrouping Write SetadwordsGrouping;
    Property adwordsLabels : TProductadwordsLabels Index 24 Read FadwordsLabels Write SetadwordsLabels;
    Property adwordsRedirect : string Index 32 Read FadwordsRedirect Write SetadwordsRedirect;
    Property ageGroup : string Index 40 Read FageGroup Write SetageGroup;
    Property aspects : TProductaspects Index 48 Read Faspects Write Setaspects;
    Property availability : string Index 56 Read Favailability Write Setavailability;
    Property availabilityDate : string Index 64 Read FavailabilityDate Write SetavailabilityDate;
    Property brand : string Index 72 Read Fbrand Write Setbrand;
    Property channel : string Index 80 Read Fchannel Write Setchannel;
    Property color : string Index 88 Read Fcolor Write Setcolor;
    Property condition : string Index 96 Read Fcondition Write Setcondition;
    Property contentLanguage : string Index 104 Read FcontentLanguage Write SetcontentLanguage;
    Property customAttributes : TProductcustomAttributes Index 112 Read FcustomAttributes Write SetcustomAttributes;
    Property customGroups : TProductcustomGroups Index 120 Read FcustomGroups Write SetcustomGroups;
    Property customLabel0 : string Index 128 Read FcustomLabel0 Write SetcustomLabel0;
    Property customLabel1 : string Index 136 Read FcustomLabel1 Write SetcustomLabel1;
    Property customLabel2 : string Index 144 Read FcustomLabel2 Write SetcustomLabel2;
    Property customLabel3 : string Index 152 Read FcustomLabel3 Write SetcustomLabel3;
    Property customLabel4 : string Index 160 Read FcustomLabel4 Write SetcustomLabel4;
    Property description : string Index 168 Read Fdescription Write Setdescription;
    Property destinations : TProductdestinations Index 176 Read Fdestinations Write Setdestinations;
    Property displayAdsId : string Index 184 Read FdisplayAdsId Write SetdisplayAdsId;
    Property displayAdsLink : string Index 192 Read FdisplayAdsLink Write SetdisplayAdsLink;
    Property displayAdsSimilarIds : TProductdisplayAdsSimilarIds Index 200 Read FdisplayAdsSimilarIds Write SetdisplayAdsSimilarIds;
    Property displayAdsTitle : string Index 208 Read FdisplayAdsTitle Write SetdisplayAdsTitle;
    Property displayAdsValue : double Index 216 Read FdisplayAdsValue Write SetdisplayAdsValue;
    Property energyEfficiencyClass : string Index 224 Read FenergyEfficiencyClass Write SetenergyEfficiencyClass;
    Property expirationDate : string Index 232 Read FexpirationDate Write SetexpirationDate;
    Property gender : string Index 240 Read Fgender Write Setgender;
    Property googleProductCategory : string Index 248 Read FgoogleProductCategory Write SetgoogleProductCategory;
    Property gtin : string Index 256 Read Fgtin Write Setgtin;
    Property id : string Index 264 Read Fid Write Setid;
    Property identifierExists : boolean Index 272 Read FidentifierExists Write SetidentifierExists;
    Property imageLink : string Index 280 Read FimageLink Write SetimageLink;
    Property installment : TProductInstallment Index 288 Read Finstallment Write Setinstallment;
    Property isBundle : boolean Index 296 Read FisBundle Write SetisBundle;
    Property itemGroupId : string Index 304 Read FitemGroupId Write SetitemGroupId;
    Property kind : string Index 312 Read Fkind Write Setkind;
    Property link : string Index 320 Read Flink Write Setlink;
    Property loyaltyPoints : TLoyaltyPoints Index 328 Read FloyaltyPoints Write SetloyaltyPoints;
    Property material : string Index 336 Read Fmaterial Write Setmaterial;
    Property mobileLink : string Index 344 Read FmobileLink Write SetmobileLink;
    Property mpn : string Index 352 Read Fmpn Write Setmpn;
    Property multipack : string Index 360 Read Fmultipack Write Setmultipack;
    Property offerId : string Index 368 Read FofferId Write SetofferId;
    Property onlineOnly : boolean Index 376 Read FonlineOnly Write SetonlineOnly;
    Property pattern : string Index 384 Read Fpattern Write Setpattern;
    Property price : TPrice Index 392 Read Fprice Write Setprice;
    Property productType : string Index 400 Read FproductType Write SetproductType;
    Property salePrice : TPrice Index 408 Read FsalePrice Write SetsalePrice;
    Property salePriceEffectiveDate : string Index 416 Read FsalePriceEffectiveDate Write SetsalePriceEffectiveDate;
    Property shipping : TProductshipping Index 424 Read Fshipping Write Setshipping;
    Property shippingHeight : TProductShippingDimension Index 432 Read FshippingHeight Write SetshippingHeight;
    Property shippingLabel : string Index 440 Read FshippingLabel Write SetshippingLabel;
    Property shippingLength : TProductShippingDimension Index 448 Read FshippingLength Write SetshippingLength;
    Property shippingWeight : TProductShippingWeight Index 456 Read FshippingWeight Write SetshippingWeight;
    Property shippingWidth : TProductShippingDimension Index 464 Read FshippingWidth Write SetshippingWidth;
    Property sizeSystem : string Index 472 Read FsizeSystem Write SetsizeSystem;
    Property sizeType : string Index 480 Read FsizeType Write SetsizeType;
    Property sizes : TProductsizes Index 488 Read Fsizes Write Setsizes;
    Property targetCountry : string Index 496 Read FtargetCountry Write SettargetCountry;
    Property taxes : TProducttaxes Index 504 Read Ftaxes Write Settaxes;
    Property title : string Index 512 Read Ftitle Write Settitle;
    Property unitPricingBaseMeasure : TProductUnitPricingBaseMeasure Index 520 Read FunitPricingBaseMeasure Write SetunitPricingBaseMeasure;
    Property unitPricingMeasure : TProductUnitPricingMeasure Index 528 Read FunitPricingMeasure Write SetunitPricingMeasure;
    Property validatedDestinations : TProductvalidatedDestinations Index 536 Read FvalidatedDestinations Write SetvalidatedDestinations;
    Property warnings : TProductwarnings Index 544 Read Fwarnings Write Setwarnings;
  end;
  TProductClass = Class of TProduct;
  
  { --------------------------------------------------------------------
    TProductadditionalImageLinks
    --------------------------------------------------------------------}
  
  TProductadditionalImageLinks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProductadditionalImageLinksClass = Class of TProductadditionalImageLinks;
  
  { --------------------------------------------------------------------
    TProductadwordsLabels
    --------------------------------------------------------------------}
  
  TProductadwordsLabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProductadwordsLabelsClass = Class of TProductadwordsLabels;
  
  { --------------------------------------------------------------------
    TProductaspects
    --------------------------------------------------------------------}
  
  TProductaspects = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProductaspectsClass = Class of TProductaspects;
  
  { --------------------------------------------------------------------
    TProductcustomAttributes
    --------------------------------------------------------------------}
  
  TProductcustomAttributes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProductcustomAttributesClass = Class of TProductcustomAttributes;
  
  { --------------------------------------------------------------------
    TProductcustomGroups
    --------------------------------------------------------------------}
  
  TProductcustomGroups = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProductcustomGroupsClass = Class of TProductcustomGroups;
  
  { --------------------------------------------------------------------
    TProductdestinations
    --------------------------------------------------------------------}
  
  TProductdestinations = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProductdestinationsClass = Class of TProductdestinations;
  
  { --------------------------------------------------------------------
    TProductdisplayAdsSimilarIds
    --------------------------------------------------------------------}
  
  TProductdisplayAdsSimilarIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProductdisplayAdsSimilarIdsClass = Class of TProductdisplayAdsSimilarIds;
  
  { --------------------------------------------------------------------
    TProductshipping
    --------------------------------------------------------------------}
  
  TProductshipping = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProductshippingClass = Class of TProductshipping;
  
  { --------------------------------------------------------------------
    TProductsizes
    --------------------------------------------------------------------}
  
  TProductsizes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProductsizesClass = Class of TProductsizes;
  
  { --------------------------------------------------------------------
    TProducttaxes
    --------------------------------------------------------------------}
  
  TProducttaxes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProducttaxesClass = Class of TProducttaxes;
  
  { --------------------------------------------------------------------
    TProductvalidatedDestinations
    --------------------------------------------------------------------}
  
  TProductvalidatedDestinations = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProductvalidatedDestinationsClass = Class of TProductvalidatedDestinations;
  
  { --------------------------------------------------------------------
    TProductwarnings
    --------------------------------------------------------------------}
  
  TProductwarnings = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProductwarningsClass = Class of TProductwarnings;
  
  { --------------------------------------------------------------------
    TProductAspect
    --------------------------------------------------------------------}
  
  TProductAspect = Class(TGoogleBaseObject)
  Private
    FaspectName : string;
    FdestinationName : string;
    Fintention : string;
  Protected
    //Property setters
    Procedure SetaspectName(AIndex : Integer; AValue : string); virtual;
    Procedure SetdestinationName(AIndex : Integer; AValue : string); virtual;
    Procedure Setintention(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property aspectName : string Index 0 Read FaspectName Write SetaspectName;
    Property destinationName : string Index 8 Read FdestinationName Write SetdestinationName;
    Property intention : string Index 16 Read Fintention Write Setintention;
  end;
  TProductAspectClass = Class of TProductAspect;
  
  { --------------------------------------------------------------------
    TProductCustomAttribute
    --------------------------------------------------------------------}
  
  TProductCustomAttribute = Class(TGoogleBaseObject)
  Private
    Fname : string;
    F_type : string;
    F_unit : string;
    Fvalue : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Set_unit(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
    Property _type : string Index 8 Read F_type Write Set_type;
    Property _unit : string Index 16 Read F_unit Write Set_unit;
    Property value : string Index 24 Read Fvalue Write Setvalue;
  end;
  TProductCustomAttributeClass = Class of TProductCustomAttribute;
  
  { --------------------------------------------------------------------
    TProductCustomGroup
    --------------------------------------------------------------------}
  
  TProductCustomGroup = Class(TGoogleBaseObject)
  Private
    Fattributes : TProductCustomGroupattributes;
    Fname : string;
  Protected
    //Property setters
    Procedure Setattributes(AIndex : Integer; AValue : TProductCustomGroupattributes); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property attributes : TProductCustomGroupattributes Index 0 Read Fattributes Write Setattributes;
    Property name : string Index 8 Read Fname Write Setname;
  end;
  TProductCustomGroupClass = Class of TProductCustomGroup;
  
  { --------------------------------------------------------------------
    TProductCustomGroupattributes
    --------------------------------------------------------------------}
  
  TProductCustomGroupattributes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProductCustomGroupattributesClass = Class of TProductCustomGroupattributes;
  
  { --------------------------------------------------------------------
    TProductDestination
    --------------------------------------------------------------------}
  
  TProductDestination = Class(TGoogleBaseObject)
  Private
    FdestinationName : string;
    Fintention : string;
  Protected
    //Property setters
    Procedure SetdestinationName(AIndex : Integer; AValue : string); virtual;
    Procedure Setintention(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property destinationName : string Index 0 Read FdestinationName Write SetdestinationName;
    Property intention : string Index 8 Read Fintention Write Setintention;
  end;
  TProductDestinationClass = Class of TProductDestination;
  
  { --------------------------------------------------------------------
    TProductInstallment
    --------------------------------------------------------------------}
  
  TProductInstallment = Class(TGoogleBaseObject)
  Private
    Famount : TPrice;
    Fmonths : string;
  Protected
    //Property setters
    Procedure Setamount(AIndex : Integer; AValue : TPrice); virtual;
    Procedure Setmonths(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property amount : TPrice Index 0 Read Famount Write Setamount;
    Property months : string Index 8 Read Fmonths Write Setmonths;
  end;
  TProductInstallmentClass = Class of TProductInstallment;
  
  { --------------------------------------------------------------------
    TProductShippingDimension
    --------------------------------------------------------------------}
  
  TProductShippingDimension = Class(TGoogleBaseObject)
  Private
    F_unit : string;
    Fvalue : double;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_unit(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property _unit : string Index 0 Read F_unit Write Set_unit;
    Property value : double Index 8 Read Fvalue Write Setvalue;
  end;
  TProductShippingDimensionClass = Class of TProductShippingDimension;
  
  { --------------------------------------------------------------------
    TProductShippingWeight
    --------------------------------------------------------------------}
  
  TProductShippingWeight = Class(TGoogleBaseObject)
  Private
    F_unit : string;
    Fvalue : double;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_unit(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property _unit : string Index 0 Read F_unit Write Set_unit;
    Property value : double Index 8 Read Fvalue Write Setvalue;
  end;
  TProductShippingWeightClass = Class of TProductShippingWeight;
  
  { --------------------------------------------------------------------
    TProductStatus
    --------------------------------------------------------------------}
  
  TProductStatus = Class(TGoogleBaseObject)
  Private
    FcreationDate : string;
    FdataQualityIssues : TProductStatusdataQualityIssues;
    FdestinationStatuses : TProductStatusdestinationStatuses;
    FgoogleExpirationDate : string;
    Fkind : string;
    FlastUpdateDate : string;
    Flink : string;
    FproductId : string;
    Ftitle : string;
  Protected
    //Property setters
    Procedure SetcreationDate(AIndex : Integer; AValue : string); virtual;
    Procedure SetdataQualityIssues(AIndex : Integer; AValue : TProductStatusdataQualityIssues); virtual;
    Procedure SetdestinationStatuses(AIndex : Integer; AValue : TProductStatusdestinationStatuses); virtual;
    Procedure SetgoogleExpirationDate(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetlastUpdateDate(AIndex : Integer; AValue : string); virtual;
    Procedure Setlink(AIndex : Integer; AValue : string); virtual;
    Procedure SetproductId(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property creationDate : string Index 0 Read FcreationDate Write SetcreationDate;
    Property dataQualityIssues : TProductStatusdataQualityIssues Index 8 Read FdataQualityIssues Write SetdataQualityIssues;
    Property destinationStatuses : TProductStatusdestinationStatuses Index 16 Read FdestinationStatuses Write SetdestinationStatuses;
    Property googleExpirationDate : string Index 24 Read FgoogleExpirationDate Write SetgoogleExpirationDate;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property lastUpdateDate : string Index 40 Read FlastUpdateDate Write SetlastUpdateDate;
    Property link : string Index 48 Read Flink Write Setlink;
    Property productId : string Index 56 Read FproductId Write SetproductId;
    Property title : string Index 64 Read Ftitle Write Settitle;
  end;
  TProductStatusClass = Class of TProductStatus;
  
  { --------------------------------------------------------------------
    TProductStatusdataQualityIssues
    --------------------------------------------------------------------}
  
  TProductStatusdataQualityIssues = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProductStatusdataQualityIssuesClass = Class of TProductStatusdataQualityIssues;
  
  { --------------------------------------------------------------------
    TProductStatusdestinationStatuses
    --------------------------------------------------------------------}
  
  TProductStatusdestinationStatuses = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProductStatusdestinationStatusesClass = Class of TProductStatusdestinationStatuses;
  
  { --------------------------------------------------------------------
    TProductStatusDataQualityIssue
    --------------------------------------------------------------------}
  
  TProductStatusDataQualityIssue = Class(TGoogleBaseObject)
  Private
    Fdetail : string;
    FfetchStatus : string;
    Fid : string;
    Flocation : string;
    Fseverity : string;
    Ftimestamp : string;
    FvalueOnLandingPage : string;
    FvalueProvided : string;
  Protected
    //Property setters
    Procedure Setdetail(AIndex : Integer; AValue : string); virtual;
    Procedure SetfetchStatus(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : string); virtual;
    Procedure Setseverity(AIndex : Integer; AValue : string); virtual;
    Procedure Settimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure SetvalueOnLandingPage(AIndex : Integer; AValue : string); virtual;
    Procedure SetvalueProvided(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property detail : string Index 0 Read Fdetail Write Setdetail;
    Property fetchStatus : string Index 8 Read FfetchStatus Write SetfetchStatus;
    Property id : string Index 16 Read Fid Write Setid;
    Property location : string Index 24 Read Flocation Write Setlocation;
    Property severity : string Index 32 Read Fseverity Write Setseverity;
    Property timestamp : string Index 40 Read Ftimestamp Write Settimestamp;
    Property valueOnLandingPage : string Index 48 Read FvalueOnLandingPage Write SetvalueOnLandingPage;
    Property valueProvided : string Index 56 Read FvalueProvided Write SetvalueProvided;
  end;
  TProductStatusDataQualityIssueClass = Class of TProductStatusDataQualityIssue;
  
  { --------------------------------------------------------------------
    TProductStatusDestinationStatus
    --------------------------------------------------------------------}
  
  TProductStatusDestinationStatus = Class(TGoogleBaseObject)
  Private
    FapprovalStatus : string;
    Fdestination : string;
    Fintention : string;
  Protected
    //Property setters
    Procedure SetapprovalStatus(AIndex : Integer; AValue : string); virtual;
    Procedure Setdestination(AIndex : Integer; AValue : string); virtual;
    Procedure Setintention(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property approvalStatus : string Index 0 Read FapprovalStatus Write SetapprovalStatus;
    Property destination : string Index 8 Read Fdestination Write Setdestination;
    Property intention : string Index 16 Read Fintention Write Setintention;
  end;
  TProductStatusDestinationStatusClass = Class of TProductStatusDestinationStatus;
  
  { --------------------------------------------------------------------
    TProductTax
    --------------------------------------------------------------------}
  
  TProductTax = Class(TGoogleBaseObject)
  Private
    Fcountry : string;
    FlocationId : string;
    FpostalCode : string;
    Frate : double;
    Fregion : string;
    FtaxShip : boolean;
  Protected
    //Property setters
    Procedure Setcountry(AIndex : Integer; AValue : string); virtual;
    Procedure SetlocationId(AIndex : Integer; AValue : string); virtual;
    Procedure SetpostalCode(AIndex : Integer; AValue : string); virtual;
    Procedure Setrate(AIndex : Integer; AValue : double); virtual;
    Procedure Setregion(AIndex : Integer; AValue : string); virtual;
    Procedure SettaxShip(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property country : string Index 0 Read Fcountry Write Setcountry;
    Property locationId : string Index 8 Read FlocationId Write SetlocationId;
    Property postalCode : string Index 16 Read FpostalCode Write SetpostalCode;
    Property rate : double Index 24 Read Frate Write Setrate;
    Property region : string Index 32 Read Fregion Write Setregion;
    Property taxShip : boolean Index 40 Read FtaxShip Write SettaxShip;
  end;
  TProductTaxClass = Class of TProductTax;
  
  { --------------------------------------------------------------------
    TProductUnitPricingBaseMeasure
    --------------------------------------------------------------------}
  
  TProductUnitPricingBaseMeasure = Class(TGoogleBaseObject)
  Private
    F_unit : string;
    Fvalue : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_unit(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property _unit : string Index 0 Read F_unit Write Set_unit;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TProductUnitPricingBaseMeasureClass = Class of TProductUnitPricingBaseMeasure;
  
  { --------------------------------------------------------------------
    TProductUnitPricingMeasure
    --------------------------------------------------------------------}
  
  TProductUnitPricingMeasure = Class(TGoogleBaseObject)
  Private
    F_unit : string;
    Fvalue : double;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_unit(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property _unit : string Index 0 Read F_unit Write Set_unit;
    Property value : double Index 8 Read Fvalue Write Setvalue;
  end;
  TProductUnitPricingMeasureClass = Class of TProductUnitPricingMeasure;
  
  { --------------------------------------------------------------------
    TProductsCustomBatchRequest
    --------------------------------------------------------------------}
  
  TProductsCustomBatchRequest = Class(TGoogleBaseObject)
  Private
    Fentries : TProductsCustomBatchRequestentries;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TProductsCustomBatchRequestentries); virtual;
  Public
  Published
    Property entries : TProductsCustomBatchRequestentries Index 0 Read Fentries Write Setentries;
  end;
  TProductsCustomBatchRequestClass = Class of TProductsCustomBatchRequest;
  
  { --------------------------------------------------------------------
    TProductsCustomBatchRequestentries
    --------------------------------------------------------------------}
  
  TProductsCustomBatchRequestentries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProductsCustomBatchRequestentriesClass = Class of TProductsCustomBatchRequestentries;
  
  { --------------------------------------------------------------------
    TProductsCustomBatchRequestEntry
    --------------------------------------------------------------------}
  
  TProductsCustomBatchRequestEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    FmerchantId : string;
    Fmethod : string;
    Fproduct : TProduct;
    FproductId : string;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmerchantId(AIndex : Integer; AValue : string); virtual;
    Procedure Setmethod(AIndex : Integer; AValue : string); virtual;
    Procedure Setproduct(AIndex : Integer; AValue : TProduct); virtual;
    Procedure SetproductId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property merchantId : string Index 8 Read FmerchantId Write SetmerchantId;
    Property method : string Index 16 Read Fmethod Write Setmethod;
    Property product : TProduct Index 24 Read Fproduct Write Setproduct;
    Property productId : string Index 32 Read FproductId Write SetproductId;
  end;
  TProductsCustomBatchRequestEntryClass = Class of TProductsCustomBatchRequestEntry;
  
  { --------------------------------------------------------------------
    TProductsCustomBatchResponse
    --------------------------------------------------------------------}
  
  TProductsCustomBatchResponse = Class(TGoogleBaseObject)
  Private
    Fentries : TProductsCustomBatchResponseentries;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TProductsCustomBatchResponseentries); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property entries : TProductsCustomBatchResponseentries Index 0 Read Fentries Write Setentries;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TProductsCustomBatchResponseClass = Class of TProductsCustomBatchResponse;
  
  { --------------------------------------------------------------------
    TProductsCustomBatchResponseentries
    --------------------------------------------------------------------}
  
  TProductsCustomBatchResponseentries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProductsCustomBatchResponseentriesClass = Class of TProductsCustomBatchResponseentries;
  
  { --------------------------------------------------------------------
    TProductsCustomBatchResponseEntry
    --------------------------------------------------------------------}
  
  TProductsCustomBatchResponseEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    Ferrors : TDatafeedStatuserrors;
    Fkind : string;
    Fproduct : TProduct;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TDatafeedStatuserrors); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setproduct(AIndex : Integer; AValue : TProduct); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property errors : TDatafeedStatuserrors Index 8 Read Ferrors Write Seterrors;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property product : TProduct Index 24 Read Fproduct Write Setproduct;
  end;
  TProductsCustomBatchResponseEntryClass = Class of TProductsCustomBatchResponseEntry;
  
  { --------------------------------------------------------------------
    TProductsListResponse
    --------------------------------------------------------------------}
  
  TProductsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    FnextPageToken : string;
    Fresources : TProductsListResponseresources;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TProductsListResponseresources); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
    Property resources : TProductsListResponseresources Index 16 Read Fresources Write Setresources;
  end;
  TProductsListResponseClass = Class of TProductsListResponse;
  
  { --------------------------------------------------------------------
    TProductsListResponseresources
    --------------------------------------------------------------------}
  
  TProductsListResponseresources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProductsListResponseresourcesClass = Class of TProductsListResponseresources;
  
  { --------------------------------------------------------------------
    TProductstatusesCustomBatchRequest
    --------------------------------------------------------------------}
  
  TProductstatusesCustomBatchRequest = Class(TGoogleBaseObject)
  Private
    Fentries : TProductstatusesCustomBatchRequestentries;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TProductstatusesCustomBatchRequestentries); virtual;
  Public
  Published
    Property entries : TProductstatusesCustomBatchRequestentries Index 0 Read Fentries Write Setentries;
  end;
  TProductstatusesCustomBatchRequestClass = Class of TProductstatusesCustomBatchRequest;
  
  { --------------------------------------------------------------------
    TProductstatusesCustomBatchRequestentries
    --------------------------------------------------------------------}
  
  TProductstatusesCustomBatchRequestentries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProductstatusesCustomBatchRequestentriesClass = Class of TProductstatusesCustomBatchRequestentries;
  
  { --------------------------------------------------------------------
    TProductstatusesCustomBatchRequestEntry
    --------------------------------------------------------------------}
  
  TProductstatusesCustomBatchRequestEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    FmerchantId : string;
    Fmethod : string;
    FproductId : string;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmerchantId(AIndex : Integer; AValue : string); virtual;
    Procedure Setmethod(AIndex : Integer; AValue : string); virtual;
    Procedure SetproductId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property merchantId : string Index 8 Read FmerchantId Write SetmerchantId;
    Property method : string Index 16 Read Fmethod Write Setmethod;
    Property productId : string Index 24 Read FproductId Write SetproductId;
  end;
  TProductstatusesCustomBatchRequestEntryClass = Class of TProductstatusesCustomBatchRequestEntry;
  
  { --------------------------------------------------------------------
    TProductstatusesCustomBatchResponse
    --------------------------------------------------------------------}
  
  TProductstatusesCustomBatchResponse = Class(TGoogleBaseObject)
  Private
    Fentries : TProductstatusesCustomBatchResponseentries;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TProductstatusesCustomBatchResponseentries); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property entries : TProductstatusesCustomBatchResponseentries Index 0 Read Fentries Write Setentries;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TProductstatusesCustomBatchResponseClass = Class of TProductstatusesCustomBatchResponse;
  
  { --------------------------------------------------------------------
    TProductstatusesCustomBatchResponseentries
    --------------------------------------------------------------------}
  
  TProductstatusesCustomBatchResponseentries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProductstatusesCustomBatchResponseentriesClass = Class of TProductstatusesCustomBatchResponseentries;
  
  { --------------------------------------------------------------------
    TProductstatusesCustomBatchResponseEntry
    --------------------------------------------------------------------}
  
  TProductstatusesCustomBatchResponseEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    Ferrors : TDatafeedStatuserrors;
    Fkind : string;
    FproductStatus : TProductStatus;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TDatafeedStatuserrors); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetproductStatus(AIndex : Integer; AValue : TProductStatus); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property errors : TDatafeedStatuserrors Index 8 Read Ferrors Write Seterrors;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property productStatus : TProductStatus Index 24 Read FproductStatus Write SetproductStatus;
  end;
  TProductstatusesCustomBatchResponseEntryClass = Class of TProductstatusesCustomBatchResponseEntry;
  
  { --------------------------------------------------------------------
    TProductstatusesListResponse
    --------------------------------------------------------------------}
  
  TProductstatusesListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    FnextPageToken : string;
    Fresources : TProductstatusesListResponseresources;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TProductstatusesListResponseresources); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
    Property resources : TProductstatusesListResponseresources Index 16 Read Fresources Write Setresources;
  end;
  TProductstatusesListResponseClass = Class of TProductstatusesListResponse;
  
  { --------------------------------------------------------------------
    TProductstatusesListResponseresources
    --------------------------------------------------------------------}
  
  TProductstatusesListResponseresources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProductstatusesListResponseresourcesClass = Class of TProductstatusesListResponseresources;
  
  { --------------------------------------------------------------------
    TWeight
    --------------------------------------------------------------------}
  
  TWeight = Class(TGoogleBaseObject)
  Private
    F_unit : string;
    Fvalue : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_unit(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property _unit : string Index 0 Read F_unit Write Set_unit;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TWeightClass = Class of TWeight;
  
  { --------------------------------------------------------------------
    TAccountsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsResource, method List
  
  TAccountsListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  TAccountsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Authinfo : TAccountsAuthInfoResponse;
    Function Custombatch(aAccountsCustomBatchRequest : TAccountsCustomBatchRequest) : TAccountsCustomBatchResponse;
    Procedure Delete(accountId: string; merchantId: string);
    Function Get(accountId: string; merchantId: string) : TAccount;
    Function Insert(merchantId: string; aAccount : TAccount) : TAccount;
    Function List(merchantId: string; AQuery : string  = '') : TAccountsListResponse;
    Function List(merchantId: string; AQuery : TAccountslistOptions) : TAccountsListResponse;
    Function Patch(accountId: string; merchantId: string; aAccount : TAccount) : TAccount;
    Function Update(accountId: string; merchantId: string; aAccount : TAccount) : TAccount;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountshippingResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountshippingResource, method Custombatch
  
  TAccountshippingCustombatchOptions = Record
    dryRun : boolean;
  end;
  
  
  //Optional query Options for TAccountshippingResource, method List
  
  TAccountshippingListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TAccountshippingResource, method Patch
  
  TAccountshippingPatchOptions = Record
    dryRun : boolean;
  end;
  
  
  //Optional query Options for TAccountshippingResource, method Update
  
  TAccountshippingUpdateOptions = Record
    dryRun : boolean;
  end;
  
  TAccountshippingResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Custombatch(aAccountshippingCustomBatchRequest : TAccountshippingCustomBatchRequest; AQuery : string  = '') : TAccountshippingCustomBatchResponse;
    Function Custombatch(aAccountshippingCustomBatchRequest : TAccountshippingCustomBatchRequest; AQuery : TAccountshippingcustombatchOptions) : TAccountshippingCustomBatchResponse;
    Function Get(accountId: string; merchantId: string) : TAccountShipping;
    Function List(merchantId: string; AQuery : string  = '') : TAccountshippingListResponse;
    Function List(merchantId: string; AQuery : TAccountshippinglistOptions) : TAccountshippingListResponse;
    Function Patch(accountId: string; merchantId: string; aAccountShipping : TAccountShipping; AQuery : string  = '') : TAccountShipping;
    Function Patch(accountId: string; merchantId: string; aAccountShipping : TAccountShipping; AQuery : TAccountshippingpatchOptions) : TAccountShipping;
    Function Update(accountId: string; merchantId: string; aAccountShipping : TAccountShipping; AQuery : string  = '') : TAccountShipping;
    Function Update(accountId: string; merchantId: string; aAccountShipping : TAccountShipping; AQuery : TAccountshippingupdateOptions) : TAccountShipping;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountstatusesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountstatusesResource, method List
  
  TAccountstatusesListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  TAccountstatusesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Custombatch(aAccountstatusesCustomBatchRequest : TAccountstatusesCustomBatchRequest) : TAccountstatusesCustomBatchResponse;
    Function Get(accountId: string; merchantId: string) : TAccountStatus;
    Function List(merchantId: string; AQuery : string  = '') : TAccountstatusesListResponse;
    Function List(merchantId: string; AQuery : TAccountstatuseslistOptions) : TAccountstatusesListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TAccounttaxResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccounttaxResource, method Custombatch
  
  TAccounttaxCustombatchOptions = Record
    dryRun : boolean;
  end;
  
  
  //Optional query Options for TAccounttaxResource, method List
  
  TAccounttaxListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TAccounttaxResource, method Patch
  
  TAccounttaxPatchOptions = Record
    dryRun : boolean;
  end;
  
  
  //Optional query Options for TAccounttaxResource, method Update
  
  TAccounttaxUpdateOptions = Record
    dryRun : boolean;
  end;
  
  TAccounttaxResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Custombatch(aAccounttaxCustomBatchRequest : TAccounttaxCustomBatchRequest; AQuery : string  = '') : TAccounttaxCustomBatchResponse;
    Function Custombatch(aAccounttaxCustomBatchRequest : TAccounttaxCustomBatchRequest; AQuery : TAccounttaxcustombatchOptions) : TAccounttaxCustomBatchResponse;
    Function Get(accountId: string; merchantId: string) : TAccountTax;
    Function List(merchantId: string; AQuery : string  = '') : TAccounttaxListResponse;
    Function List(merchantId: string; AQuery : TAccounttaxlistOptions) : TAccounttaxListResponse;
    Function Patch(accountId: string; merchantId: string; aAccountTax : TAccountTax; AQuery : string  = '') : TAccountTax;
    Function Patch(accountId: string; merchantId: string; aAccountTax : TAccountTax; AQuery : TAccounttaxpatchOptions) : TAccountTax;
    Function Update(accountId: string; merchantId: string; aAccountTax : TAccountTax; AQuery : string  = '') : TAccountTax;
    Function Update(accountId: string; merchantId: string; aAccountTax : TAccountTax; AQuery : TAccounttaxupdateOptions) : TAccountTax;
  end;
  
  
  { --------------------------------------------------------------------
    TDatafeedsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDatafeedsResource, method List
  
  TDatafeedsListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  TDatafeedsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Custombatch(aDatafeedsCustomBatchRequest : TDatafeedsCustomBatchRequest) : TDatafeedsCustomBatchResponse;
    Procedure Delete(datafeedId: string; merchantId: string);
    Function Get(datafeedId: string; merchantId: string) : TDatafeed;
    Function Insert(merchantId: string; aDatafeed : TDatafeed) : TDatafeed;
    Function List(merchantId: string; AQuery : string  = '') : TDatafeedsListResponse;
    Function List(merchantId: string; AQuery : TDatafeedslistOptions) : TDatafeedsListResponse;
    Function Patch(datafeedId: string; merchantId: string; aDatafeed : TDatafeed) : TDatafeed;
    Function Update(datafeedId: string; merchantId: string; aDatafeed : TDatafeed) : TDatafeed;
  end;
  
  
  { --------------------------------------------------------------------
    TDatafeedstatusesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDatafeedstatusesResource, method List
  
  TDatafeedstatusesListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  TDatafeedstatusesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Custombatch(aDatafeedstatusesCustomBatchRequest : TDatafeedstatusesCustomBatchRequest) : TDatafeedstatusesCustomBatchResponse;
    Function Get(datafeedId: string; merchantId: string) : TDatafeedStatus;
    Function List(merchantId: string; AQuery : string  = '') : TDatafeedstatusesListResponse;
    Function List(merchantId: string; AQuery : TDatafeedstatuseslistOptions) : TDatafeedstatusesListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TInventoryResource
    --------------------------------------------------------------------}
  
  TInventoryResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Custombatch(aInventoryCustomBatchRequest : TInventoryCustomBatchRequest) : TInventoryCustomBatchResponse;
    Function _set(merchantId: string; productId: string; storeCode: string; aInventorySetRequest : TInventorySetRequest) : TInventorySetResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TProductsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProductsResource, method Custombatch
  
  TProductsCustombatchOptions = Record
    dryRun : boolean;
  end;
  
  
  //Optional query Options for TProductsResource, method Delete
  
  TProductsDeleteOptions = Record
    dryRun : boolean;
  end;
  
  
  //Optional query Options for TProductsResource, method Insert
  
  TProductsInsertOptions = Record
    dryRun : boolean;
  end;
  
  
  //Optional query Options for TProductsResource, method List
  
  TProductsListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  TProductsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Custombatch(aProductsCustomBatchRequest : TProductsCustomBatchRequest; AQuery : string  = '') : TProductsCustomBatchResponse;
    Function Custombatch(aProductsCustomBatchRequest : TProductsCustomBatchRequest; AQuery : TProductscustombatchOptions) : TProductsCustomBatchResponse;
    Procedure Delete(merchantId: string; productId: string; AQuery : string  = '');
    Procedure Delete(merchantId: string; productId: string; AQuery : TProductsdeleteOptions);
    Function Get(merchantId: string; productId: string) : TProduct;
    Function Insert(merchantId: string; aProduct : TProduct; AQuery : string  = '') : TProduct;
    Function Insert(merchantId: string; aProduct : TProduct; AQuery : TProductsinsertOptions) : TProduct;
    Function List(merchantId: string; AQuery : string  = '') : TProductsListResponse;
    Function List(merchantId: string; AQuery : TProductslistOptions) : TProductsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TProductstatusesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProductstatusesResource, method List
  
  TProductstatusesListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  TProductstatusesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Custombatch(aProductstatusesCustomBatchRequest : TProductstatusesCustomBatchRequest) : TProductstatusesCustomBatchResponse;
    Function Get(merchantId: string; productId: string) : TProductStatus;
    Function List(merchantId: string; AQuery : string  = '') : TProductstatusesListResponse;
    Function List(merchantId: string; AQuery : TProductstatuseslistOptions) : TProductstatusesListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TContentAPI
    --------------------------------------------------------------------}
  
  TContentAPI = Class(TGoogleAPI)
  Private
    FAccountsInstance : TAccountsResource;
    FAccountshippingInstance : TAccountshippingResource;
    FAccountstatusesInstance : TAccountstatusesResource;
    FAccounttaxInstance : TAccounttaxResource;
    FDatafeedsInstance : TDatafeedsResource;
    FDatafeedstatusesInstance : TDatafeedstatusesResource;
    FInventoryInstance : TInventoryResource;
    FProductsInstance : TProductsResource;
    FProductstatusesInstance : TProductstatusesResource;
    Function GetAccountsInstance : TAccountsResource;virtual;
    Function GetAccountshippingInstance : TAccountshippingResource;virtual;
    Function GetAccountstatusesInstance : TAccountstatusesResource;virtual;
    Function GetAccounttaxInstance : TAccounttaxResource;virtual;
    Function GetDatafeedsInstance : TDatafeedsResource;virtual;
    Function GetDatafeedstatusesInstance : TDatafeedstatusesResource;virtual;
    Function GetInventoryInstance : TInventoryResource;virtual;
    Function GetProductsInstance : TProductsResource;virtual;
    Function GetProductstatusesInstance : TProductstatusesResource;virtual;
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
    Function CreateAccountshippingResource(AOwner : TComponent) : TAccountshippingResource;virtual;overload;
    Function CreateAccountshippingResource : TAccountshippingResource;virtual;overload;
    Function CreateAccountstatusesResource(AOwner : TComponent) : TAccountstatusesResource;virtual;overload;
    Function CreateAccountstatusesResource : TAccountstatusesResource;virtual;overload;
    Function CreateAccounttaxResource(AOwner : TComponent) : TAccounttaxResource;virtual;overload;
    Function CreateAccounttaxResource : TAccounttaxResource;virtual;overload;
    Function CreateDatafeedsResource(AOwner : TComponent) : TDatafeedsResource;virtual;overload;
    Function CreateDatafeedsResource : TDatafeedsResource;virtual;overload;
    Function CreateDatafeedstatusesResource(AOwner : TComponent) : TDatafeedstatusesResource;virtual;overload;
    Function CreateDatafeedstatusesResource : TDatafeedstatusesResource;virtual;overload;
    Function CreateInventoryResource(AOwner : TComponent) : TInventoryResource;virtual;overload;
    Function CreateInventoryResource : TInventoryResource;virtual;overload;
    Function CreateProductsResource(AOwner : TComponent) : TProductsResource;virtual;overload;
    Function CreateProductsResource : TProductsResource;virtual;overload;
    Function CreateProductstatusesResource(AOwner : TComponent) : TProductstatusesResource;virtual;overload;
    Function CreateProductstatusesResource : TProductstatusesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AccountsResource : TAccountsResource Read GetAccountsInstance;
    Property AccountshippingResource : TAccountshippingResource Read GetAccountshippingInstance;
    Property AccountstatusesResource : TAccountstatusesResource Read GetAccountstatusesInstance;
    Property AccounttaxResource : TAccounttaxResource Read GetAccounttaxInstance;
    Property DatafeedsResource : TDatafeedsResource Read GetDatafeedsInstance;
    Property DatafeedstatusesResource : TDatafeedstatusesResource Read GetDatafeedstatusesInstance;
    Property InventoryResource : TInventoryResource Read GetInventoryInstance;
    Property ProductsResource : TProductsResource Read GetProductsInstance;
    Property ProductstatusesResource : TProductstatusesResource Read GetProductstatusesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAccount
  --------------------------------------------------------------------}


Procedure TAccount.SetadultContent(AIndex : Integer; AValue : boolean); 

begin
  If (FadultContent=AValue) then exit;
  FadultContent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetadwordsLinks(AIndex : Integer; AValue : TAccountadwordsLinks); 

begin
  If (FadwordsLinks=AValue) then exit;
  FadwordsLinks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setid(AIndex : Integer; AValue : string); 

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



Procedure TAccount.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetreviewsUrl(AIndex : Integer; AValue : string); 

begin
  If (FreviewsUrl=AValue) then exit;
  FreviewsUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetsellerId(AIndex : Integer; AValue : string); 

begin
  If (FsellerId=AValue) then exit;
  FsellerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setusers(AIndex : Integer; AValue : TAccountusers); 

begin
  If (Fusers=AValue) then exit;
  Fusers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetwebsiteUrl(AIndex : Integer; AValue : string); 

begin
  If (FwebsiteUrl=AValue) then exit;
  FwebsiteUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountadwordsLinks
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountusers
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountAdwordsLink
  --------------------------------------------------------------------}


Procedure TAccountAdwordsLink.SetadwordsId(AIndex : Integer; AValue : string); 

begin
  If (FadwordsId=AValue) then exit;
  FadwordsId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountAdwordsLink.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountIdentifier
  --------------------------------------------------------------------}


Procedure TAccountIdentifier.SetaggregatorId(AIndex : Integer; AValue : string); 

begin
  If (FaggregatorId=AValue) then exit;
  FaggregatorId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountIdentifier.SetmerchantId(AIndex : Integer; AValue : string); 

begin
  If (FmerchantId=AValue) then exit;
  FmerchantId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountShipping
  --------------------------------------------------------------------}


Procedure TAccountShipping.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShipping.SetcarrierRates(AIndex : Integer; AValue : TAccountShippingcarrierRates); 

begin
  If (FcarrierRates=AValue) then exit;
  FcarrierRates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShipping.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShipping.SetlocationGroups(AIndex : Integer; AValue : TAccountShippinglocationGroups); 

begin
  If (FlocationGroups=AValue) then exit;
  FlocationGroups:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShipping.SetrateTables(AIndex : Integer; AValue : TAccountShippingrateTables); 

begin
  If (FrateTables=AValue) then exit;
  FrateTables:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShipping.Setservices(AIndex : Integer; AValue : TAccountShippingservices); 

begin
  If (Fservices=AValue) then exit;
  Fservices:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountShippingcarrierRates
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountShippinglocationGroups
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountShippingrateTables
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountShippingservices
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountShippingCarrierRate
  --------------------------------------------------------------------}


Procedure TAccountShippingCarrierRate.Setcarrier(AIndex : Integer; AValue : string); 

begin
  If (Fcarrier=AValue) then exit;
  Fcarrier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingCarrierRate.SetcarrierService(AIndex : Integer; AValue : string); 

begin
  If (FcarrierService=AValue) then exit;
  FcarrierService:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingCarrierRate.SetmodifierFlatRate(AIndex : Integer; AValue : TPrice); 

begin
  If (FmodifierFlatRate=AValue) then exit;
  FmodifierFlatRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingCarrierRate.SetmodifierPercent(AIndex : Integer; AValue : string); 

begin
  If (FmodifierPercent=AValue) then exit;
  FmodifierPercent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingCarrierRate.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingCarrierRate.SetsaleCountry(AIndex : Integer; AValue : string); 

begin
  If (FsaleCountry=AValue) then exit;
  FsaleCountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingCarrierRate.SetshippingOrigin(AIndex : Integer; AValue : string); 

begin
  If (FshippingOrigin=AValue) then exit;
  FshippingOrigin:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountShippingCondition
  --------------------------------------------------------------------}


Procedure TAccountShippingCondition.SetdeliveryLocationGroup(AIndex : Integer; AValue : string); 

begin
  If (FdeliveryLocationGroup=AValue) then exit;
  FdeliveryLocationGroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingCondition.SetdeliveryLocationId(AIndex : Integer; AValue : string); 

begin
  If (FdeliveryLocationId=AValue) then exit;
  FdeliveryLocationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingCondition.SetdeliveryPostalCode(AIndex : Integer; AValue : string); 

begin
  If (FdeliveryPostalCode=AValue) then exit;
  FdeliveryPostalCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingCondition.SetdeliveryPostalCodeRange(AIndex : Integer; AValue : TAccountShippingPostalCodeRange); 

begin
  If (FdeliveryPostalCodeRange=AValue) then exit;
  FdeliveryPostalCodeRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingCondition.SetpriceMax(AIndex : Integer; AValue : TPrice); 

begin
  If (FpriceMax=AValue) then exit;
  FpriceMax:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingCondition.SetshippingLabel(AIndex : Integer; AValue : string); 

begin
  If (FshippingLabel=AValue) then exit;
  FshippingLabel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingCondition.SetweightMax(AIndex : Integer; AValue : TWeight); 

begin
  If (FweightMax=AValue) then exit;
  FweightMax:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountShippingLocationGroup
  --------------------------------------------------------------------}


Procedure TAccountShippingLocationGroup.Setcountry(AIndex : Integer; AValue : string); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingLocationGroup.SetlocationIds(AIndex : Integer; AValue : TAccountShippingLocationGrouplocationIds); 

begin
  If (FlocationIds=AValue) then exit;
  FlocationIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingLocationGroup.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingLocationGroup.SetpostalCodeRanges(AIndex : Integer; AValue : TAccountShippingLocationGrouppostalCodeRanges); 

begin
  If (FpostalCodeRanges=AValue) then exit;
  FpostalCodeRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingLocationGroup.SetpostalCodes(AIndex : Integer; AValue : TAccountShippingLocationGrouppostalCodes); 

begin
  If (FpostalCodes=AValue) then exit;
  FpostalCodes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountShippingLocationGrouplocationIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountShippingLocationGrouppostalCodeRanges
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountShippingLocationGrouppostalCodes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountShippingPostalCodeRange
  --------------------------------------------------------------------}


Procedure TAccountShippingPostalCodeRange.Set_end(AIndex : Integer; AValue : string); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingPostalCodeRange.Setstart(AIndex : Integer; AValue : string); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAccountShippingPostalCodeRange.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAccountShippingRateTable
  --------------------------------------------------------------------}


Procedure TAccountShippingRateTable.Setcontent(AIndex : Integer; AValue : TAccountShippingRateTablecontent); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingRateTable.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingRateTable.SetsaleCountry(AIndex : Integer; AValue : string); 

begin
  If (FsaleCountry=AValue) then exit;
  FsaleCountry:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountShippingRateTablecontent
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountShippingRateTableCell
  --------------------------------------------------------------------}


Procedure TAccountShippingRateTableCell.Setcondition(AIndex : Integer; AValue : TAccountShippingCondition); 

begin
  If (Fcondition=AValue) then exit;
  Fcondition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingRateTableCell.Setrate(AIndex : Integer; AValue : TPrice); 

begin
  If (Frate=AValue) then exit;
  Frate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountShippingShippingService
  --------------------------------------------------------------------}


Procedure TAccountShippingShippingService.Setactive(AIndex : Integer; AValue : boolean); 

begin
  If (Factive=AValue) then exit;
  Factive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingShippingService.SetcalculationMethod(AIndex : Integer; AValue : TAccountShippingShippingServiceCalculationMethod); 

begin
  If (FcalculationMethod=AValue) then exit;
  FcalculationMethod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingShippingService.SetcostRuleTree(AIndex : Integer; AValue : TAccountShippingShippingServiceCostRule); 

begin
  If (FcostRuleTree=AValue) then exit;
  FcostRuleTree:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingShippingService.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingShippingService.SetsaleCountry(AIndex : Integer; AValue : string); 

begin
  If (FsaleCountry=AValue) then exit;
  FsaleCountry:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountShippingShippingServiceCalculationMethod
  --------------------------------------------------------------------}


Procedure TAccountShippingShippingServiceCalculationMethod.SetcarrierRate(AIndex : Integer; AValue : string); 

begin
  If (FcarrierRate=AValue) then exit;
  FcarrierRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingShippingServiceCalculationMethod.Setexcluded(AIndex : Integer; AValue : boolean); 

begin
  If (Fexcluded=AValue) then exit;
  Fexcluded:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingShippingServiceCalculationMethod.SetflatRate(AIndex : Integer; AValue : TPrice); 

begin
  If (FflatRate=AValue) then exit;
  FflatRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingShippingServiceCalculationMethod.SetpercentageRate(AIndex : Integer; AValue : string); 

begin
  If (FpercentageRate=AValue) then exit;
  FpercentageRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingShippingServiceCalculationMethod.SetrateTable(AIndex : Integer; AValue : string); 

begin
  If (FrateTable=AValue) then exit;
  FrateTable:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountShippingShippingServiceCostRule
  --------------------------------------------------------------------}


Procedure TAccountShippingShippingServiceCostRule.SetcalculationMethod(AIndex : Integer; AValue : TAccountShippingShippingServiceCalculationMethod); 

begin
  If (FcalculationMethod=AValue) then exit;
  FcalculationMethod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingShippingServiceCostRule.Setchildren(AIndex : Integer; AValue : TAccountShippingShippingServiceCostRulechildren); 

begin
  If (Fchildren=AValue) then exit;
  Fchildren:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingShippingServiceCostRule.Setcondition(AIndex : Integer; AValue : TAccountShippingCondition); 

begin
  If (Fcondition=AValue) then exit;
  Fcondition:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountShippingShippingServiceCostRulechildren
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountStatus
  --------------------------------------------------------------------}


Procedure TAccountStatus.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatus.SetdataQualityIssues(AIndex : Integer; AValue : TAccountStatusdataQualityIssues); 

begin
  If (FdataQualityIssues=AValue) then exit;
  FdataQualityIssues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatus.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountStatusdataQualityIssues
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountStatusDataQualityIssue
  --------------------------------------------------------------------}


Procedure TAccountStatusDataQualityIssue.Setcountry(AIndex : Integer; AValue : string); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatusDataQualityIssue.SetdisplayedValue(AIndex : Integer; AValue : string); 

begin
  If (FdisplayedValue=AValue) then exit;
  FdisplayedValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatusDataQualityIssue.SetexampleItems(AIndex : Integer; AValue : TAccountStatusDataQualityIssueexampleItems); 

begin
  If (FexampleItems=AValue) then exit;
  FexampleItems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatusDataQualityIssue.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatusDataQualityIssue.SetlastChecked(AIndex : Integer; AValue : string); 

begin
  If (FlastChecked=AValue) then exit;
  FlastChecked:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatusDataQualityIssue.SetnumItems(AIndex : Integer; AValue : integer); 

begin
  If (FnumItems=AValue) then exit;
  FnumItems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatusDataQualityIssue.Setseverity(AIndex : Integer; AValue : string); 

begin
  If (Fseverity=AValue) then exit;
  Fseverity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatusDataQualityIssue.SetsubmittedValue(AIndex : Integer; AValue : string); 

begin
  If (FsubmittedValue=AValue) then exit;
  FsubmittedValue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountStatusDataQualityIssueexampleItems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountStatusExampleItem
  --------------------------------------------------------------------}


Procedure TAccountStatusExampleItem.SetitemId(AIndex : Integer; AValue : string); 

begin
  If (FitemId=AValue) then exit;
  FitemId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatusExampleItem.Setlink(AIndex : Integer; AValue : string); 

begin
  If (Flink=AValue) then exit;
  Flink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatusExampleItem.SetsubmittedValue(AIndex : Integer; AValue : string); 

begin
  If (FsubmittedValue=AValue) then exit;
  FsubmittedValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatusExampleItem.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatusExampleItem.SetvalueOnLandingPage(AIndex : Integer; AValue : string); 

begin
  If (FvalueOnLandingPage=AValue) then exit;
  FvalueOnLandingPage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountTax
  --------------------------------------------------------------------}


Procedure TAccountTax.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTax.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTax.Setrules(AIndex : Integer; AValue : TAccountTaxrules); 

begin
  If (Frules=AValue) then exit;
  Frules:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountTaxrules
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountTaxTaxRule
  --------------------------------------------------------------------}


Procedure TAccountTaxTaxRule.Setcountry(AIndex : Integer; AValue : string); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTaxTaxRule.SetlocationId(AIndex : Integer; AValue : string); 

begin
  If (FlocationId=AValue) then exit;
  FlocationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTaxTaxRule.SetratePercent(AIndex : Integer; AValue : string); 

begin
  If (FratePercent=AValue) then exit;
  FratePercent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTaxTaxRule.SetshippingTaxed(AIndex : Integer; AValue : boolean); 

begin
  If (FshippingTaxed=AValue) then exit;
  FshippingTaxed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTaxTaxRule.SetuseGlobalRate(AIndex : Integer; AValue : boolean); 

begin
  If (FuseGlobalRate=AValue) then exit;
  FuseGlobalRate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountUser
  --------------------------------------------------------------------}


Procedure TAccountUser.Setadmin(AIndex : Integer; AValue : boolean); 

begin
  If (Fadmin=AValue) then exit;
  Fadmin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountUser.SetemailAddress(AIndex : Integer; AValue : string); 

begin
  If (FemailAddress=AValue) then exit;
  FemailAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountsAuthInfoResponse
  --------------------------------------------------------------------}


Procedure TAccountsAuthInfoResponse.SetaccountIdentifiers(AIndex : Integer; AValue : TAccountsAuthInfoResponseaccountIdentifiers); 

begin
  If (FaccountIdentifiers=AValue) then exit;
  FaccountIdentifiers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsAuthInfoResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountsAuthInfoResponseaccountIdentifiers
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountsCustomBatchRequest
  --------------------------------------------------------------------}


Procedure TAccountsCustomBatchRequest.Setentries(AIndex : Integer; AValue : TAccountsCustomBatchRequestentries); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountsCustomBatchRequestentries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountsCustomBatchRequestEntry
  --------------------------------------------------------------------}


Procedure TAccountsCustomBatchRequestEntry.Setaccount(AIndex : Integer; AValue : TAccount); 

begin
  If (Faccount=AValue) then exit;
  Faccount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsCustomBatchRequestEntry.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsCustomBatchRequestEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsCustomBatchRequestEntry.SetmerchantId(AIndex : Integer; AValue : string); 

begin
  If (FmerchantId=AValue) then exit;
  FmerchantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsCustomBatchRequestEntry.Setmethod(AIndex : Integer; AValue : string); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountsCustomBatchResponse
  --------------------------------------------------------------------}


Procedure TAccountsCustomBatchResponse.Setentries(AIndex : Integer; AValue : TAccountsCustomBatchResponseentries); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsCustomBatchResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountsCustomBatchResponseentries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountsCustomBatchResponseEntry
  --------------------------------------------------------------------}


Procedure TAccountsCustomBatchResponseEntry.Setaccount(AIndex : Integer; AValue : TAccount); 

begin
  If (Faccount=AValue) then exit;
  Faccount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsCustomBatchResponseEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsCustomBatchResponseEntry.Seterrors(AIndex : Integer; AValue : TDatafeedStatuserrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsCustomBatchResponseEntry.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountsListResponse
  --------------------------------------------------------------------}


Procedure TAccountsListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsListResponse.Setresources(AIndex : Integer; AValue : TAccountsListResponseresources); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountsListResponseresources
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountshippingCustomBatchRequest
  --------------------------------------------------------------------}


Procedure TAccountshippingCustomBatchRequest.Setentries(AIndex : Integer; AValue : TAccountshippingCustomBatchRequestentries); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountshippingCustomBatchRequestentries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountshippingCustomBatchRequestEntry
  --------------------------------------------------------------------}


Procedure TAccountshippingCustomBatchRequestEntry.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountshippingCustomBatchRequestEntry.SetaccountShipping(AIndex : Integer; AValue : TAccountShipping); 

begin
  If (FaccountShipping=AValue) then exit;
  FaccountShipping:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountshippingCustomBatchRequestEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountshippingCustomBatchRequestEntry.SetmerchantId(AIndex : Integer; AValue : string); 

begin
  If (FmerchantId=AValue) then exit;
  FmerchantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountshippingCustomBatchRequestEntry.Setmethod(AIndex : Integer; AValue : string); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountshippingCustomBatchResponse
  --------------------------------------------------------------------}


Procedure TAccountshippingCustomBatchResponse.Setentries(AIndex : Integer; AValue : TAccountshippingCustomBatchResponseentries); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountshippingCustomBatchResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountshippingCustomBatchResponseentries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountshippingCustomBatchResponseEntry
  --------------------------------------------------------------------}


Procedure TAccountshippingCustomBatchResponseEntry.SetaccountShipping(AIndex : Integer; AValue : TAccountShipping); 

begin
  If (FaccountShipping=AValue) then exit;
  FaccountShipping:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountshippingCustomBatchResponseEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountshippingCustomBatchResponseEntry.Seterrors(AIndex : Integer; AValue : TDatafeedStatuserrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountshippingCustomBatchResponseEntry.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountshippingListResponse
  --------------------------------------------------------------------}


Procedure TAccountshippingListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountshippingListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountshippingListResponse.Setresources(AIndex : Integer; AValue : TAccountshippingListResponseresources); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountshippingListResponseresources
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountstatusesCustomBatchRequest
  --------------------------------------------------------------------}


Procedure TAccountstatusesCustomBatchRequest.Setentries(AIndex : Integer; AValue : TAccountstatusesCustomBatchRequestentries); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountstatusesCustomBatchRequestentries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountstatusesCustomBatchRequestEntry
  --------------------------------------------------------------------}


Procedure TAccountstatusesCustomBatchRequestEntry.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountstatusesCustomBatchRequestEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountstatusesCustomBatchRequestEntry.SetmerchantId(AIndex : Integer; AValue : string); 

begin
  If (FmerchantId=AValue) then exit;
  FmerchantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountstatusesCustomBatchRequestEntry.Setmethod(AIndex : Integer; AValue : string); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountstatusesCustomBatchResponse
  --------------------------------------------------------------------}


Procedure TAccountstatusesCustomBatchResponse.Setentries(AIndex : Integer; AValue : TAccountstatusesCustomBatchResponseentries); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountstatusesCustomBatchResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountstatusesCustomBatchResponseentries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountstatusesCustomBatchResponseEntry
  --------------------------------------------------------------------}


Procedure TAccountstatusesCustomBatchResponseEntry.SetaccountStatus(AIndex : Integer; AValue : TAccountStatus); 

begin
  If (FaccountStatus=AValue) then exit;
  FaccountStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountstatusesCustomBatchResponseEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountstatusesCustomBatchResponseEntry.Seterrors(AIndex : Integer; AValue : TDatafeedStatuserrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountstatusesListResponse
  --------------------------------------------------------------------}


Procedure TAccountstatusesListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountstatusesListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountstatusesListResponse.Setresources(AIndex : Integer; AValue : TAccountstatusesListResponseresources); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountstatusesListResponseresources
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccounttaxCustomBatchRequest
  --------------------------------------------------------------------}


Procedure TAccounttaxCustomBatchRequest.Setentries(AIndex : Integer; AValue : TAccounttaxCustomBatchRequestentries); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccounttaxCustomBatchRequestentries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccounttaxCustomBatchRequestEntry
  --------------------------------------------------------------------}


Procedure TAccounttaxCustomBatchRequestEntry.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounttaxCustomBatchRequestEntry.SetaccountTax(AIndex : Integer; AValue : TAccountTax); 

begin
  If (FaccountTax=AValue) then exit;
  FaccountTax:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounttaxCustomBatchRequestEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounttaxCustomBatchRequestEntry.SetmerchantId(AIndex : Integer; AValue : string); 

begin
  If (FmerchantId=AValue) then exit;
  FmerchantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounttaxCustomBatchRequestEntry.Setmethod(AIndex : Integer; AValue : string); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccounttaxCustomBatchResponse
  --------------------------------------------------------------------}


Procedure TAccounttaxCustomBatchResponse.Setentries(AIndex : Integer; AValue : TAccounttaxCustomBatchResponseentries); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounttaxCustomBatchResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccounttaxCustomBatchResponseentries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccounttaxCustomBatchResponseEntry
  --------------------------------------------------------------------}


Procedure TAccounttaxCustomBatchResponseEntry.SetaccountTax(AIndex : Integer; AValue : TAccountTax); 

begin
  If (FaccountTax=AValue) then exit;
  FaccountTax:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounttaxCustomBatchResponseEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounttaxCustomBatchResponseEntry.Seterrors(AIndex : Integer; AValue : TDatafeedStatuserrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounttaxCustomBatchResponseEntry.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccounttaxListResponse
  --------------------------------------------------------------------}


Procedure TAccounttaxListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounttaxListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounttaxListResponse.Setresources(AIndex : Integer; AValue : TAccounttaxListResponseresources); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccounttaxListResponseresources
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDatafeed
  --------------------------------------------------------------------}


Procedure TDatafeed.SetattributeLanguage(AIndex : Integer; AValue : string); 

begin
  If (FattributeLanguage=AValue) then exit;
  FattributeLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeed.SetcontentLanguage(AIndex : Integer; AValue : string); 

begin
  If (FcontentLanguage=AValue) then exit;
  FcontentLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeed.SetcontentType(AIndex : Integer; AValue : string); 

begin
  If (FcontentType=AValue) then exit;
  FcontentType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeed.SetfetchSchedule(AIndex : Integer; AValue : TDatafeedFetchSchedule); 

begin
  If (FfetchSchedule=AValue) then exit;
  FfetchSchedule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeed.SetfileName(AIndex : Integer; AValue : string); 

begin
  If (FfileName=AValue) then exit;
  FfileName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeed.Setformat(AIndex : Integer; AValue : TDatafeedFormat); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeed.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeed.SetintendedDestinations(AIndex : Integer; AValue : TDatafeedintendedDestinations); 

begin
  If (FintendedDestinations=AValue) then exit;
  FintendedDestinations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeed.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeed.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeed.SettargetCountry(AIndex : Integer; AValue : string); 

begin
  If (FtargetCountry=AValue) then exit;
  FtargetCountry:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedintendedDestinations
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDatafeedFetchSchedule
  --------------------------------------------------------------------}


Procedure TDatafeedFetchSchedule.SetdayOfMonth(AIndex : Integer; AValue : integer); 

begin
  If (FdayOfMonth=AValue) then exit;
  FdayOfMonth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedFetchSchedule.SetfetchUrl(AIndex : Integer; AValue : string); 

begin
  If (FfetchUrl=AValue) then exit;
  FfetchUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedFetchSchedule.Sethour(AIndex : Integer; AValue : integer); 

begin
  If (Fhour=AValue) then exit;
  Fhour:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedFetchSchedule.Setpassword(AIndex : Integer; AValue : string); 

begin
  If (Fpassword=AValue) then exit;
  Fpassword:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedFetchSchedule.SettimeZone(AIndex : Integer; AValue : string); 

begin
  If (FtimeZone=AValue) then exit;
  FtimeZone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedFetchSchedule.Setusername(AIndex : Integer; AValue : string); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedFetchSchedule.Setweekday(AIndex : Integer; AValue : string); 

begin
  If (Fweekday=AValue) then exit;
  Fweekday:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedFormat
  --------------------------------------------------------------------}


Procedure TDatafeedFormat.SetcolumnDelimiter(AIndex : Integer; AValue : string); 

begin
  If (FcolumnDelimiter=AValue) then exit;
  FcolumnDelimiter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedFormat.SetfileEncoding(AIndex : Integer; AValue : string); 

begin
  If (FfileEncoding=AValue) then exit;
  FfileEncoding:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedFormat.SetquotingMode(AIndex : Integer; AValue : string); 

begin
  If (FquotingMode=AValue) then exit;
  FquotingMode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedStatus
  --------------------------------------------------------------------}


Procedure TDatafeedStatus.SetdatafeedId(AIndex : Integer; AValue : string); 

begin
  If (FdatafeedId=AValue) then exit;
  FdatafeedId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatus.Seterrors(AIndex : Integer; AValue : TDatafeedStatuserrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatus.SetitemsTotal(AIndex : Integer; AValue : string); 

begin
  If (FitemsTotal=AValue) then exit;
  FitemsTotal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatus.SetitemsValid(AIndex : Integer; AValue : string); 

begin
  If (FitemsValid=AValue) then exit;
  FitemsValid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatus.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatus.SetlastUploadDate(AIndex : Integer; AValue : string); 

begin
  If (FlastUploadDate=AValue) then exit;
  FlastUploadDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatus.SetprocessingStatus(AIndex : Integer; AValue : string); 

begin
  If (FprocessingStatus=AValue) then exit;
  FprocessingStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatus.Setwarnings(AIndex : Integer; AValue : TDatafeedStatuswarnings); 

begin
  If (Fwarnings=AValue) then exit;
  Fwarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedStatuserrors
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDatafeedStatuswarnings
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDatafeedStatusError
  --------------------------------------------------------------------}


Procedure TDatafeedStatusError.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatusError.Setcount(AIndex : Integer; AValue : string); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatusError.Setexamples(AIndex : Integer; AValue : TDatafeedStatusErrorexamples); 

begin
  If (Fexamples=AValue) then exit;
  Fexamples:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatusError.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedStatusErrorexamples
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDatafeedStatusExample
  --------------------------------------------------------------------}


Procedure TDatafeedStatusExample.SetitemId(AIndex : Integer; AValue : string); 

begin
  If (FitemId=AValue) then exit;
  FitemId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatusExample.SetlineNumber(AIndex : Integer; AValue : string); 

begin
  If (FlineNumber=AValue) then exit;
  FlineNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatusExample.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedsCustomBatchRequest
  --------------------------------------------------------------------}


Procedure TDatafeedsCustomBatchRequest.Setentries(AIndex : Integer; AValue : TDatafeedsCustomBatchRequestentries); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedsCustomBatchRequestentries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDatafeedsCustomBatchRequestEntry
  --------------------------------------------------------------------}


Procedure TDatafeedsCustomBatchRequestEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedsCustomBatchRequestEntry.Setdatafeed(AIndex : Integer; AValue : TDatafeed); 

begin
  If (Fdatafeed=AValue) then exit;
  Fdatafeed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedsCustomBatchRequestEntry.SetdatafeedId(AIndex : Integer; AValue : string); 

begin
  If (FdatafeedId=AValue) then exit;
  FdatafeedId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedsCustomBatchRequestEntry.SetmerchantId(AIndex : Integer; AValue : string); 

begin
  If (FmerchantId=AValue) then exit;
  FmerchantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedsCustomBatchRequestEntry.Setmethod(AIndex : Integer; AValue : string); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedsCustomBatchResponse
  --------------------------------------------------------------------}


Procedure TDatafeedsCustomBatchResponse.Setentries(AIndex : Integer; AValue : TDatafeedsCustomBatchResponseentries); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedsCustomBatchResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedsCustomBatchResponseentries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDatafeedsCustomBatchResponseEntry
  --------------------------------------------------------------------}


Procedure TDatafeedsCustomBatchResponseEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedsCustomBatchResponseEntry.Setdatafeed(AIndex : Integer; AValue : TDatafeed); 

begin
  If (Fdatafeed=AValue) then exit;
  Fdatafeed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedsCustomBatchResponseEntry.Seterrors(AIndex : Integer; AValue : TDatafeedStatuserrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedsListResponse
  --------------------------------------------------------------------}


Procedure TDatafeedsListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedsListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedsListResponse.Setresources(AIndex : Integer; AValue : TDatafeedsListResponseresources); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedsListResponseresources
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDatafeedstatusesCustomBatchRequest
  --------------------------------------------------------------------}


Procedure TDatafeedstatusesCustomBatchRequest.Setentries(AIndex : Integer; AValue : TDatafeedstatusesCustomBatchRequestentries); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedstatusesCustomBatchRequestentries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDatafeedstatusesCustomBatchRequestEntry
  --------------------------------------------------------------------}


Procedure TDatafeedstatusesCustomBatchRequestEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedstatusesCustomBatchRequestEntry.SetdatafeedId(AIndex : Integer; AValue : string); 

begin
  If (FdatafeedId=AValue) then exit;
  FdatafeedId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedstatusesCustomBatchRequestEntry.SetmerchantId(AIndex : Integer; AValue : string); 

begin
  If (FmerchantId=AValue) then exit;
  FmerchantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedstatusesCustomBatchRequestEntry.Setmethod(AIndex : Integer; AValue : string); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedstatusesCustomBatchResponse
  --------------------------------------------------------------------}


Procedure TDatafeedstatusesCustomBatchResponse.Setentries(AIndex : Integer; AValue : TDatafeedstatusesCustomBatchResponseentries); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedstatusesCustomBatchResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedstatusesCustomBatchResponseentries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDatafeedstatusesCustomBatchResponseEntry
  --------------------------------------------------------------------}


Procedure TDatafeedstatusesCustomBatchResponseEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedstatusesCustomBatchResponseEntry.SetdatafeedStatus(AIndex : Integer; AValue : TDatafeedStatus); 

begin
  If (FdatafeedStatus=AValue) then exit;
  FdatafeedStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedstatusesCustomBatchResponseEntry.Seterrors(AIndex : Integer; AValue : TDatafeedStatuserrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedstatusesListResponse
  --------------------------------------------------------------------}


Procedure TDatafeedstatusesListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedstatusesListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedstatusesListResponse.Setresources(AIndex : Integer; AValue : TDatafeedstatusesListResponseresources); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedstatusesListResponseresources
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TError
  --------------------------------------------------------------------}


Procedure TError.Setdomain(AIndex : Integer; AValue : string); 

begin
  If (Fdomain=AValue) then exit;
  Fdomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TError.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TError.Setreason(AIndex : Integer; AValue : string); 

begin
  If (Freason=AValue) then exit;
  Freason:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TErrors
  --------------------------------------------------------------------}


Procedure TErrors.Setcode(AIndex : Integer; AValue : integer); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrors.Seterrors(AIndex : Integer; AValue : TErrorserrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrors.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TErrorserrors
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInventory
  --------------------------------------------------------------------}


Procedure TInventory.Setavailability(AIndex : Integer; AValue : string); 

begin
  If (Favailability=AValue) then exit;
  Favailability:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventory.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventory.Setprice(AIndex : Integer; AValue : TPrice); 

begin
  If (Fprice=AValue) then exit;
  Fprice:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventory.Setquantity(AIndex : Integer; AValue : integer); 

begin
  If (Fquantity=AValue) then exit;
  Fquantity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventory.SetsalePrice(AIndex : Integer; AValue : TPrice); 

begin
  If (FsalePrice=AValue) then exit;
  FsalePrice:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventory.SetsalePriceEffectiveDate(AIndex : Integer; AValue : string); 

begin
  If (FsalePriceEffectiveDate=AValue) then exit;
  FsalePriceEffectiveDate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInventoryCustomBatchRequest
  --------------------------------------------------------------------}


Procedure TInventoryCustomBatchRequest.Setentries(AIndex : Integer; AValue : TInventoryCustomBatchRequestentries); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInventoryCustomBatchRequestentries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInventoryCustomBatchRequestEntry
  --------------------------------------------------------------------}


Procedure TInventoryCustomBatchRequestEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryCustomBatchRequestEntry.Setinventory(AIndex : Integer; AValue : TInventory); 

begin
  If (Finventory=AValue) then exit;
  Finventory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryCustomBatchRequestEntry.SetmerchantId(AIndex : Integer; AValue : string); 

begin
  If (FmerchantId=AValue) then exit;
  FmerchantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryCustomBatchRequestEntry.SetproductId(AIndex : Integer; AValue : string); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryCustomBatchRequestEntry.SetstoreCode(AIndex : Integer; AValue : string); 

begin
  If (FstoreCode=AValue) then exit;
  FstoreCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInventoryCustomBatchResponse
  --------------------------------------------------------------------}


Procedure TInventoryCustomBatchResponse.Setentries(AIndex : Integer; AValue : TInventoryCustomBatchResponseentries); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryCustomBatchResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInventoryCustomBatchResponseentries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInventoryCustomBatchResponseEntry
  --------------------------------------------------------------------}


Procedure TInventoryCustomBatchResponseEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryCustomBatchResponseEntry.Seterrors(AIndex : Integer; AValue : TDatafeedStatuserrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryCustomBatchResponseEntry.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInventorySetRequest
  --------------------------------------------------------------------}


Procedure TInventorySetRequest.Setavailability(AIndex : Integer; AValue : string); 

begin
  If (Favailability=AValue) then exit;
  Favailability:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventorySetRequest.Setprice(AIndex : Integer; AValue : TPrice); 

begin
  If (Fprice=AValue) then exit;
  Fprice:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventorySetRequest.Setquantity(AIndex : Integer; AValue : integer); 

begin
  If (Fquantity=AValue) then exit;
  Fquantity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventorySetRequest.SetsalePrice(AIndex : Integer; AValue : TPrice); 

begin
  If (FsalePrice=AValue) then exit;
  FsalePrice:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventorySetRequest.SetsalePriceEffectiveDate(AIndex : Integer; AValue : string); 

begin
  If (FsalePriceEffectiveDate=AValue) then exit;
  FsalePriceEffectiveDate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInventorySetResponse
  --------------------------------------------------------------------}


Procedure TInventorySetResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLoyaltyPoints
  --------------------------------------------------------------------}


Procedure TLoyaltyPoints.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLoyaltyPoints.SetpointsValue(AIndex : Integer; AValue : string); 

begin
  If (FpointsValue=AValue) then exit;
  FpointsValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLoyaltyPoints.Setratio(AIndex : Integer; AValue : double); 

begin
  If (Fratio=AValue) then exit;
  Fratio:=AValue;
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



Procedure TPrice.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProduct
  --------------------------------------------------------------------}


Procedure TProduct.SetadditionalImageLinks(AIndex : Integer; AValue : TProductadditionalImageLinks); 

begin
  If (FadditionalImageLinks=AValue) then exit;
  FadditionalImageLinks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setadult(AIndex : Integer; AValue : boolean); 

begin
  If (Fadult=AValue) then exit;
  Fadult:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetadwordsGrouping(AIndex : Integer; AValue : string); 

begin
  If (FadwordsGrouping=AValue) then exit;
  FadwordsGrouping:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetadwordsLabels(AIndex : Integer; AValue : TProductadwordsLabels); 

begin
  If (FadwordsLabels=AValue) then exit;
  FadwordsLabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetadwordsRedirect(AIndex : Integer; AValue : string); 

begin
  If (FadwordsRedirect=AValue) then exit;
  FadwordsRedirect:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetageGroup(AIndex : Integer; AValue : string); 

begin
  If (FageGroup=AValue) then exit;
  FageGroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setaspects(AIndex : Integer; AValue : TProductaspects); 

begin
  If (Faspects=AValue) then exit;
  Faspects:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setavailability(AIndex : Integer; AValue : string); 

begin
  If (Favailability=AValue) then exit;
  Favailability:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetavailabilityDate(AIndex : Integer; AValue : string); 

begin
  If (FavailabilityDate=AValue) then exit;
  FavailabilityDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setbrand(AIndex : Integer; AValue : string); 

begin
  If (Fbrand=AValue) then exit;
  Fbrand:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setchannel(AIndex : Integer; AValue : string); 

begin
  If (Fchannel=AValue) then exit;
  Fchannel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setcolor(AIndex : Integer; AValue : string); 

begin
  If (Fcolor=AValue) then exit;
  Fcolor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setcondition(AIndex : Integer; AValue : string); 

begin
  If (Fcondition=AValue) then exit;
  Fcondition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetcontentLanguage(AIndex : Integer; AValue : string); 

begin
  If (FcontentLanguage=AValue) then exit;
  FcontentLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetcustomAttributes(AIndex : Integer; AValue : TProductcustomAttributes); 

begin
  If (FcustomAttributes=AValue) then exit;
  FcustomAttributes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetcustomGroups(AIndex : Integer; AValue : TProductcustomGroups); 

begin
  If (FcustomGroups=AValue) then exit;
  FcustomGroups:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetcustomLabel0(AIndex : Integer; AValue : string); 

begin
  If (FcustomLabel0=AValue) then exit;
  FcustomLabel0:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetcustomLabel1(AIndex : Integer; AValue : string); 

begin
  If (FcustomLabel1=AValue) then exit;
  FcustomLabel1:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetcustomLabel2(AIndex : Integer; AValue : string); 

begin
  If (FcustomLabel2=AValue) then exit;
  FcustomLabel2:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetcustomLabel3(AIndex : Integer; AValue : string); 

begin
  If (FcustomLabel3=AValue) then exit;
  FcustomLabel3:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetcustomLabel4(AIndex : Integer; AValue : string); 

begin
  If (FcustomLabel4=AValue) then exit;
  FcustomLabel4:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setdestinations(AIndex : Integer; AValue : TProductdestinations); 

begin
  If (Fdestinations=AValue) then exit;
  Fdestinations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetdisplayAdsId(AIndex : Integer; AValue : string); 

begin
  If (FdisplayAdsId=AValue) then exit;
  FdisplayAdsId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetdisplayAdsLink(AIndex : Integer; AValue : string); 

begin
  If (FdisplayAdsLink=AValue) then exit;
  FdisplayAdsLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetdisplayAdsSimilarIds(AIndex : Integer; AValue : TProductdisplayAdsSimilarIds); 

begin
  If (FdisplayAdsSimilarIds=AValue) then exit;
  FdisplayAdsSimilarIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetdisplayAdsTitle(AIndex : Integer; AValue : string); 

begin
  If (FdisplayAdsTitle=AValue) then exit;
  FdisplayAdsTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetdisplayAdsValue(AIndex : Integer; AValue : double); 

begin
  If (FdisplayAdsValue=AValue) then exit;
  FdisplayAdsValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetenergyEfficiencyClass(AIndex : Integer; AValue : string); 

begin
  If (FenergyEfficiencyClass=AValue) then exit;
  FenergyEfficiencyClass:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetexpirationDate(AIndex : Integer; AValue : string); 

begin
  If (FexpirationDate=AValue) then exit;
  FexpirationDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setgender(AIndex : Integer; AValue : string); 

begin
  If (Fgender=AValue) then exit;
  Fgender:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetgoogleProductCategory(AIndex : Integer; AValue : string); 

begin
  If (FgoogleProductCategory=AValue) then exit;
  FgoogleProductCategory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setgtin(AIndex : Integer; AValue : string); 

begin
  If (Fgtin=AValue) then exit;
  Fgtin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetidentifierExists(AIndex : Integer; AValue : boolean); 

begin
  If (FidentifierExists=AValue) then exit;
  FidentifierExists:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetimageLink(AIndex : Integer; AValue : string); 

begin
  If (FimageLink=AValue) then exit;
  FimageLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setinstallment(AIndex : Integer; AValue : TProductInstallment); 

begin
  If (Finstallment=AValue) then exit;
  Finstallment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetisBundle(AIndex : Integer; AValue : boolean); 

begin
  If (FisBundle=AValue) then exit;
  FisBundle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetitemGroupId(AIndex : Integer; AValue : string); 

begin
  If (FitemGroupId=AValue) then exit;
  FitemGroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setlink(AIndex : Integer; AValue : string); 

begin
  If (Flink=AValue) then exit;
  Flink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetloyaltyPoints(AIndex : Integer; AValue : TLoyaltyPoints); 

begin
  If (FloyaltyPoints=AValue) then exit;
  FloyaltyPoints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setmaterial(AIndex : Integer; AValue : string); 

begin
  If (Fmaterial=AValue) then exit;
  Fmaterial:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetmobileLink(AIndex : Integer; AValue : string); 

begin
  If (FmobileLink=AValue) then exit;
  FmobileLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setmpn(AIndex : Integer; AValue : string); 

begin
  If (Fmpn=AValue) then exit;
  Fmpn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setmultipack(AIndex : Integer; AValue : string); 

begin
  If (Fmultipack=AValue) then exit;
  Fmultipack:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetofferId(AIndex : Integer; AValue : string); 

begin
  If (FofferId=AValue) then exit;
  FofferId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetonlineOnly(AIndex : Integer; AValue : boolean); 

begin
  If (FonlineOnly=AValue) then exit;
  FonlineOnly:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setpattern(AIndex : Integer; AValue : string); 

begin
  If (Fpattern=AValue) then exit;
  Fpattern:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setprice(AIndex : Integer; AValue : TPrice); 

begin
  If (Fprice=AValue) then exit;
  Fprice:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetproductType(AIndex : Integer; AValue : string); 

begin
  If (FproductType=AValue) then exit;
  FproductType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetsalePrice(AIndex : Integer; AValue : TPrice); 

begin
  If (FsalePrice=AValue) then exit;
  FsalePrice:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetsalePriceEffectiveDate(AIndex : Integer; AValue : string); 

begin
  If (FsalePriceEffectiveDate=AValue) then exit;
  FsalePriceEffectiveDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setshipping(AIndex : Integer; AValue : TProductshipping); 

begin
  If (Fshipping=AValue) then exit;
  Fshipping:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetshippingHeight(AIndex : Integer; AValue : TProductShippingDimension); 

begin
  If (FshippingHeight=AValue) then exit;
  FshippingHeight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetshippingLabel(AIndex : Integer; AValue : string); 

begin
  If (FshippingLabel=AValue) then exit;
  FshippingLabel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetshippingLength(AIndex : Integer; AValue : TProductShippingDimension); 

begin
  If (FshippingLength=AValue) then exit;
  FshippingLength:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetshippingWeight(AIndex : Integer; AValue : TProductShippingWeight); 

begin
  If (FshippingWeight=AValue) then exit;
  FshippingWeight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetshippingWidth(AIndex : Integer; AValue : TProductShippingDimension); 

begin
  If (FshippingWidth=AValue) then exit;
  FshippingWidth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetsizeSystem(AIndex : Integer; AValue : string); 

begin
  If (FsizeSystem=AValue) then exit;
  FsizeSystem:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetsizeType(AIndex : Integer; AValue : string); 

begin
  If (FsizeType=AValue) then exit;
  FsizeType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setsizes(AIndex : Integer; AValue : TProductsizes); 

begin
  If (Fsizes=AValue) then exit;
  Fsizes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SettargetCountry(AIndex : Integer; AValue : string); 

begin
  If (FtargetCountry=AValue) then exit;
  FtargetCountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Settaxes(AIndex : Integer; AValue : TProducttaxes); 

begin
  If (Ftaxes=AValue) then exit;
  Ftaxes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetunitPricingBaseMeasure(AIndex : Integer; AValue : TProductUnitPricingBaseMeasure); 

begin
  If (FunitPricingBaseMeasure=AValue) then exit;
  FunitPricingBaseMeasure:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetunitPricingMeasure(AIndex : Integer; AValue : TProductUnitPricingMeasure); 

begin
  If (FunitPricingMeasure=AValue) then exit;
  FunitPricingMeasure:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetvalidatedDestinations(AIndex : Integer; AValue : TProductvalidatedDestinations); 

begin
  If (FvalidatedDestinations=AValue) then exit;
  FvalidatedDestinations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setwarnings(AIndex : Integer; AValue : TProductwarnings); 

begin
  If (Fwarnings=AValue) then exit;
  Fwarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductadditionalImageLinks
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProductadwordsLabels
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProductaspects
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProductcustomAttributes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProductcustomGroups
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProductdestinations
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProductdisplayAdsSimilarIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProductshipping
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProductsizes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProducttaxes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProductvalidatedDestinations
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProductwarnings
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProductAspect
  --------------------------------------------------------------------}


Procedure TProductAspect.SetaspectName(AIndex : Integer; AValue : string); 

begin
  If (FaspectName=AValue) then exit;
  FaspectName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductAspect.SetdestinationName(AIndex : Integer; AValue : string); 

begin
  If (FdestinationName=AValue) then exit;
  FdestinationName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductAspect.Setintention(AIndex : Integer; AValue : string); 

begin
  If (Fintention=AValue) then exit;
  Fintention:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductCustomAttribute
  --------------------------------------------------------------------}


Procedure TProductCustomAttribute.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductCustomAttribute.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductCustomAttribute.Set_unit(AIndex : Integer; AValue : string); 

begin
  If (F_unit=AValue) then exit;
  F_unit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductCustomAttribute.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TProductCustomAttribute.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  '_unit' : Result:='unit';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TProductCustomGroup
  --------------------------------------------------------------------}


Procedure TProductCustomGroup.Setattributes(AIndex : Integer; AValue : TProductCustomGroupattributes); 

begin
  If (Fattributes=AValue) then exit;
  Fattributes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductCustomGroup.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductCustomGroupattributes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProductDestination
  --------------------------------------------------------------------}


Procedure TProductDestination.SetdestinationName(AIndex : Integer; AValue : string); 

begin
  If (FdestinationName=AValue) then exit;
  FdestinationName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductDestination.Setintention(AIndex : Integer; AValue : string); 

begin
  If (Fintention=AValue) then exit;
  Fintention:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductInstallment
  --------------------------------------------------------------------}


Procedure TProductInstallment.Setamount(AIndex : Integer; AValue : TPrice); 

begin
  If (Famount=AValue) then exit;
  Famount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductInstallment.Setmonths(AIndex : Integer; AValue : string); 

begin
  If (Fmonths=AValue) then exit;
  Fmonths:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductShippingDimension
  --------------------------------------------------------------------}


Procedure TProductShippingDimension.Set_unit(AIndex : Integer; AValue : string); 

begin
  If (F_unit=AValue) then exit;
  F_unit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductShippingDimension.Setvalue(AIndex : Integer; AValue : double); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TProductShippingDimension.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_unit' : Result:='unit';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TProductShippingWeight
  --------------------------------------------------------------------}


Procedure TProductShippingWeight.Set_unit(AIndex : Integer; AValue : string); 

begin
  If (F_unit=AValue) then exit;
  F_unit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductShippingWeight.Setvalue(AIndex : Integer; AValue : double); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TProductShippingWeight.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_unit' : Result:='unit';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TProductStatus
  --------------------------------------------------------------------}


Procedure TProductStatus.SetcreationDate(AIndex : Integer; AValue : string); 

begin
  If (FcreationDate=AValue) then exit;
  FcreationDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatus.SetdataQualityIssues(AIndex : Integer; AValue : TProductStatusdataQualityIssues); 

begin
  If (FdataQualityIssues=AValue) then exit;
  FdataQualityIssues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatus.SetdestinationStatuses(AIndex : Integer; AValue : TProductStatusdestinationStatuses); 

begin
  If (FdestinationStatuses=AValue) then exit;
  FdestinationStatuses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatus.SetgoogleExpirationDate(AIndex : Integer; AValue : string); 

begin
  If (FgoogleExpirationDate=AValue) then exit;
  FgoogleExpirationDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatus.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatus.SetlastUpdateDate(AIndex : Integer; AValue : string); 

begin
  If (FlastUpdateDate=AValue) then exit;
  FlastUpdateDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatus.Setlink(AIndex : Integer; AValue : string); 

begin
  If (Flink=AValue) then exit;
  Flink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatus.SetproductId(AIndex : Integer; AValue : string); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatus.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductStatusdataQualityIssues
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProductStatusdestinationStatuses
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProductStatusDataQualityIssue
  --------------------------------------------------------------------}


Procedure TProductStatusDataQualityIssue.Setdetail(AIndex : Integer; AValue : string); 

begin
  If (Fdetail=AValue) then exit;
  Fdetail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatusDataQualityIssue.SetfetchStatus(AIndex : Integer; AValue : string); 

begin
  If (FfetchStatus=AValue) then exit;
  FfetchStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatusDataQualityIssue.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatusDataQualityIssue.Setlocation(AIndex : Integer; AValue : string); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatusDataQualityIssue.Setseverity(AIndex : Integer; AValue : string); 

begin
  If (Fseverity=AValue) then exit;
  Fseverity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatusDataQualityIssue.Settimestamp(AIndex : Integer; AValue : string); 

begin
  If (Ftimestamp=AValue) then exit;
  Ftimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatusDataQualityIssue.SetvalueOnLandingPage(AIndex : Integer; AValue : string); 

begin
  If (FvalueOnLandingPage=AValue) then exit;
  FvalueOnLandingPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatusDataQualityIssue.SetvalueProvided(AIndex : Integer; AValue : string); 

begin
  If (FvalueProvided=AValue) then exit;
  FvalueProvided:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductStatusDestinationStatus
  --------------------------------------------------------------------}


Procedure TProductStatusDestinationStatus.SetapprovalStatus(AIndex : Integer; AValue : string); 

begin
  If (FapprovalStatus=AValue) then exit;
  FapprovalStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatusDestinationStatus.Setdestination(AIndex : Integer; AValue : string); 

begin
  If (Fdestination=AValue) then exit;
  Fdestination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatusDestinationStatus.Setintention(AIndex : Integer; AValue : string); 

begin
  If (Fintention=AValue) then exit;
  Fintention:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductTax
  --------------------------------------------------------------------}


Procedure TProductTax.Setcountry(AIndex : Integer; AValue : string); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductTax.SetlocationId(AIndex : Integer; AValue : string); 

begin
  If (FlocationId=AValue) then exit;
  FlocationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductTax.SetpostalCode(AIndex : Integer; AValue : string); 

begin
  If (FpostalCode=AValue) then exit;
  FpostalCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductTax.Setrate(AIndex : Integer; AValue : double); 

begin
  If (Frate=AValue) then exit;
  Frate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductTax.Setregion(AIndex : Integer; AValue : string); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductTax.SettaxShip(AIndex : Integer; AValue : boolean); 

begin
  If (FtaxShip=AValue) then exit;
  FtaxShip:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductUnitPricingBaseMeasure
  --------------------------------------------------------------------}


Procedure TProductUnitPricingBaseMeasure.Set_unit(AIndex : Integer; AValue : string); 

begin
  If (F_unit=AValue) then exit;
  F_unit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductUnitPricingBaseMeasure.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TProductUnitPricingBaseMeasure.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_unit' : Result:='unit';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TProductUnitPricingMeasure
  --------------------------------------------------------------------}


Procedure TProductUnitPricingMeasure.Set_unit(AIndex : Integer; AValue : string); 

begin
  If (F_unit=AValue) then exit;
  F_unit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductUnitPricingMeasure.Setvalue(AIndex : Integer; AValue : double); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TProductUnitPricingMeasure.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_unit' : Result:='unit';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TProductsCustomBatchRequest
  --------------------------------------------------------------------}


Procedure TProductsCustomBatchRequest.Setentries(AIndex : Integer; AValue : TProductsCustomBatchRequestentries); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductsCustomBatchRequestentries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProductsCustomBatchRequestEntry
  --------------------------------------------------------------------}


Procedure TProductsCustomBatchRequestEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductsCustomBatchRequestEntry.SetmerchantId(AIndex : Integer; AValue : string); 

begin
  If (FmerchantId=AValue) then exit;
  FmerchantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductsCustomBatchRequestEntry.Setmethod(AIndex : Integer; AValue : string); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductsCustomBatchRequestEntry.Setproduct(AIndex : Integer; AValue : TProduct); 

begin
  If (Fproduct=AValue) then exit;
  Fproduct:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductsCustomBatchRequestEntry.SetproductId(AIndex : Integer; AValue : string); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductsCustomBatchResponse
  --------------------------------------------------------------------}


Procedure TProductsCustomBatchResponse.Setentries(AIndex : Integer; AValue : TProductsCustomBatchResponseentries); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductsCustomBatchResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductsCustomBatchResponseentries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProductsCustomBatchResponseEntry
  --------------------------------------------------------------------}


Procedure TProductsCustomBatchResponseEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductsCustomBatchResponseEntry.Seterrors(AIndex : Integer; AValue : TDatafeedStatuserrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductsCustomBatchResponseEntry.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductsCustomBatchResponseEntry.Setproduct(AIndex : Integer; AValue : TProduct); 

begin
  If (Fproduct=AValue) then exit;
  Fproduct:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductsListResponse
  --------------------------------------------------------------------}


Procedure TProductsListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductsListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductsListResponse.Setresources(AIndex : Integer; AValue : TProductsListResponseresources); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductsListResponseresources
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProductstatusesCustomBatchRequest
  --------------------------------------------------------------------}


Procedure TProductstatusesCustomBatchRequest.Setentries(AIndex : Integer; AValue : TProductstatusesCustomBatchRequestentries); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductstatusesCustomBatchRequestentries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProductstatusesCustomBatchRequestEntry
  --------------------------------------------------------------------}


Procedure TProductstatusesCustomBatchRequestEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductstatusesCustomBatchRequestEntry.SetmerchantId(AIndex : Integer; AValue : string); 

begin
  If (FmerchantId=AValue) then exit;
  FmerchantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductstatusesCustomBatchRequestEntry.Setmethod(AIndex : Integer; AValue : string); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductstatusesCustomBatchRequestEntry.SetproductId(AIndex : Integer; AValue : string); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductstatusesCustomBatchResponse
  --------------------------------------------------------------------}


Procedure TProductstatusesCustomBatchResponse.Setentries(AIndex : Integer; AValue : TProductstatusesCustomBatchResponseentries); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductstatusesCustomBatchResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductstatusesCustomBatchResponseentries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProductstatusesCustomBatchResponseEntry
  --------------------------------------------------------------------}


Procedure TProductstatusesCustomBatchResponseEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductstatusesCustomBatchResponseEntry.Seterrors(AIndex : Integer; AValue : TDatafeedStatuserrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductstatusesCustomBatchResponseEntry.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductstatusesCustomBatchResponseEntry.SetproductStatus(AIndex : Integer; AValue : TProductStatus); 

begin
  If (FproductStatus=AValue) then exit;
  FproductStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductstatusesListResponse
  --------------------------------------------------------------------}


Procedure TProductstatusesListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductstatusesListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductstatusesListResponse.Setresources(AIndex : Integer; AValue : TProductstatusesListResponseresources); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductstatusesListResponseresources
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TWeight
  --------------------------------------------------------------------}


Procedure TWeight.Set_unit(AIndex : Integer; AValue : string); 

begin
  If (F_unit=AValue) then exit;
  F_unit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWeight.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TWeight.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_unit' : Result:='unit';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
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
  Result:=TcontentAPI;
end;

Function TAccountsResource.Authinfo : TAccountsAuthInfoResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/authinfo';
  _Methodid   = 'content.accounts.authinfo';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TAccountsAuthInfoResponse) as TAccountsAuthInfoResponse;
end;

Function TAccountsResource.Custombatch(aAccountsCustomBatchRequest : TAccountsCustomBatchRequest) : TAccountsCustomBatchResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'accounts/batch';
  _Methodid   = 'content.accounts.custombatch';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aAccountsCustomBatchRequest,TAccountsCustomBatchResponse) as TAccountsCustomBatchResponse;
end;

Procedure TAccountsResource.Delete(accountId: string; merchantId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{merchantId}/accounts/{accountId}';
  _Methodid   = 'content.accounts.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'merchantId',merchantId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TAccountsResource.Get(accountId: string; merchantId: string) : TAccount;

Const
  _HTTPMethod = 'GET';
  _Path       = '{merchantId}/accounts/{accountId}';
  _Methodid   = 'content.accounts.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAccount) as TAccount;
end;

Function TAccountsResource.Insert(merchantId: string; aAccount : TAccount) : TAccount;

Const
  _HTTPMethod = 'POST';
  _Path       = '{merchantId}/accounts';
  _Methodid   = 'content.accounts.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAccount,TAccount) as TAccount;
end;

Function TAccountsResource.List(merchantId: string; AQuery : string = '') : TAccountsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{merchantId}/accounts';
  _Methodid   = 'content.accounts.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAccountsListResponse) as TAccountsListResponse;
end;


Function TAccountsResource.List(merchantId: string; AQuery : TAccountslistOptions) : TAccountsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(merchantId,_Q);
end;

Function TAccountsResource.Patch(accountId: string; merchantId: string; aAccount : TAccount) : TAccount;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{merchantId}/accounts/{accountId}';
  _Methodid   = 'content.accounts.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAccount,TAccount) as TAccount;
end;

Function TAccountsResource.Update(accountId: string; merchantId: string; aAccount : TAccount) : TAccount;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{merchantId}/accounts/{accountId}';
  _Methodid   = 'content.accounts.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAccount,TAccount) as TAccount;
end;



{ --------------------------------------------------------------------
  TAccountshippingResource
  --------------------------------------------------------------------}


Class Function TAccountshippingResource.ResourceName : String;

begin
  Result:='accountshipping';
end;

Class Function TAccountshippingResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcontentAPI;
end;

Function TAccountshippingResource.Custombatch(aAccountshippingCustomBatchRequest : TAccountshippingCustomBatchRequest; AQuery : string = '') : TAccountshippingCustomBatchResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'accountshipping/batch';
  _Methodid   = 'content.accountshipping.custombatch';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aAccountshippingCustomBatchRequest,TAccountshippingCustomBatchResponse) as TAccountshippingCustomBatchResponse;
end;


Function TAccountshippingResource.Custombatch(aAccountshippingCustomBatchRequest : TAccountshippingCustomBatchRequest; AQuery : TAccountshippingcustombatchOptions) : TAccountshippingCustomBatchResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'dryRun',AQuery.dryRun);
  Result:=Custombatch(aAccountshippingCustomBatchRequest,_Q);
end;

Function TAccountshippingResource.Get(accountId: string; merchantId: string) : TAccountShipping;

Const
  _HTTPMethod = 'GET';
  _Path       = '{merchantId}/accountshipping/{accountId}';
  _Methodid   = 'content.accountshipping.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAccountShipping) as TAccountShipping;
end;

Function TAccountshippingResource.List(merchantId: string; AQuery : string = '') : TAccountshippingListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{merchantId}/accountshipping';
  _Methodid   = 'content.accountshipping.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAccountshippingListResponse) as TAccountshippingListResponse;
end;


Function TAccountshippingResource.List(merchantId: string; AQuery : TAccountshippinglistOptions) : TAccountshippingListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(merchantId,_Q);
end;

Function TAccountshippingResource.Patch(accountId: string; merchantId: string; aAccountShipping : TAccountShipping; AQuery : string = '') : TAccountShipping;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{merchantId}/accountshipping/{accountId}';
  _Methodid   = 'content.accountshipping.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aAccountShipping,TAccountShipping) as TAccountShipping;
end;


Function TAccountshippingResource.Patch(accountId: string; merchantId: string; aAccountShipping : TAccountShipping; AQuery : TAccountshippingpatchOptions) : TAccountShipping;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'dryRun',AQuery.dryRun);
  Result:=Patch(accountId,merchantId,aAccountShipping,_Q);
end;

Function TAccountshippingResource.Update(accountId: string; merchantId: string; aAccountShipping : TAccountShipping; AQuery : string = '') : TAccountShipping;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{merchantId}/accountshipping/{accountId}';
  _Methodid   = 'content.accountshipping.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aAccountShipping,TAccountShipping) as TAccountShipping;
end;


Function TAccountshippingResource.Update(accountId: string; merchantId: string; aAccountShipping : TAccountShipping; AQuery : TAccountshippingupdateOptions) : TAccountShipping;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'dryRun',AQuery.dryRun);
  Result:=Update(accountId,merchantId,aAccountShipping,_Q);
end;



{ --------------------------------------------------------------------
  TAccountstatusesResource
  --------------------------------------------------------------------}


Class Function TAccountstatusesResource.ResourceName : String;

begin
  Result:='accountstatuses';
end;

Class Function TAccountstatusesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcontentAPI;
end;

Function TAccountstatusesResource.Custombatch(aAccountstatusesCustomBatchRequest : TAccountstatusesCustomBatchRequest) : TAccountstatusesCustomBatchResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'accountstatuses/batch';
  _Methodid   = 'content.accountstatuses.custombatch';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aAccountstatusesCustomBatchRequest,TAccountstatusesCustomBatchResponse) as TAccountstatusesCustomBatchResponse;
end;

Function TAccountstatusesResource.Get(accountId: string; merchantId: string) : TAccountStatus;

Const
  _HTTPMethod = 'GET';
  _Path       = '{merchantId}/accountstatuses/{accountId}';
  _Methodid   = 'content.accountstatuses.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAccountStatus) as TAccountStatus;
end;

Function TAccountstatusesResource.List(merchantId: string; AQuery : string = '') : TAccountstatusesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{merchantId}/accountstatuses';
  _Methodid   = 'content.accountstatuses.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAccountstatusesListResponse) as TAccountstatusesListResponse;
end;


Function TAccountstatusesResource.List(merchantId: string; AQuery : TAccountstatuseslistOptions) : TAccountstatusesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(merchantId,_Q);
end;



{ --------------------------------------------------------------------
  TAccounttaxResource
  --------------------------------------------------------------------}


Class Function TAccounttaxResource.ResourceName : String;

begin
  Result:='accounttax';
end;

Class Function TAccounttaxResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcontentAPI;
end;

Function TAccounttaxResource.Custombatch(aAccounttaxCustomBatchRequest : TAccounttaxCustomBatchRequest; AQuery : string = '') : TAccounttaxCustomBatchResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'accounttax/batch';
  _Methodid   = 'content.accounttax.custombatch';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aAccounttaxCustomBatchRequest,TAccounttaxCustomBatchResponse) as TAccounttaxCustomBatchResponse;
end;


Function TAccounttaxResource.Custombatch(aAccounttaxCustomBatchRequest : TAccounttaxCustomBatchRequest; AQuery : TAccounttaxcustombatchOptions) : TAccounttaxCustomBatchResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'dryRun',AQuery.dryRun);
  Result:=Custombatch(aAccounttaxCustomBatchRequest,_Q);
end;

Function TAccounttaxResource.Get(accountId: string; merchantId: string) : TAccountTax;

Const
  _HTTPMethod = 'GET';
  _Path       = '{merchantId}/accounttax/{accountId}';
  _Methodid   = 'content.accounttax.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAccountTax) as TAccountTax;
end;

Function TAccounttaxResource.List(merchantId: string; AQuery : string = '') : TAccounttaxListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{merchantId}/accounttax';
  _Methodid   = 'content.accounttax.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAccounttaxListResponse) as TAccounttaxListResponse;
end;


Function TAccounttaxResource.List(merchantId: string; AQuery : TAccounttaxlistOptions) : TAccounttaxListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(merchantId,_Q);
end;

Function TAccounttaxResource.Patch(accountId: string; merchantId: string; aAccountTax : TAccountTax; AQuery : string = '') : TAccountTax;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{merchantId}/accounttax/{accountId}';
  _Methodid   = 'content.accounttax.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aAccountTax,TAccountTax) as TAccountTax;
end;


Function TAccounttaxResource.Patch(accountId: string; merchantId: string; aAccountTax : TAccountTax; AQuery : TAccounttaxpatchOptions) : TAccountTax;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'dryRun',AQuery.dryRun);
  Result:=Patch(accountId,merchantId,aAccountTax,_Q);
end;

Function TAccounttaxResource.Update(accountId: string; merchantId: string; aAccountTax : TAccountTax; AQuery : string = '') : TAccountTax;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{merchantId}/accounttax/{accountId}';
  _Methodid   = 'content.accounttax.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aAccountTax,TAccountTax) as TAccountTax;
end;


Function TAccounttaxResource.Update(accountId: string; merchantId: string; aAccountTax : TAccountTax; AQuery : TAccounttaxupdateOptions) : TAccountTax;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'dryRun',AQuery.dryRun);
  Result:=Update(accountId,merchantId,aAccountTax,_Q);
end;



{ --------------------------------------------------------------------
  TDatafeedsResource
  --------------------------------------------------------------------}


Class Function TDatafeedsResource.ResourceName : String;

begin
  Result:='datafeeds';
end;

Class Function TDatafeedsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcontentAPI;
end;

Function TDatafeedsResource.Custombatch(aDatafeedsCustomBatchRequest : TDatafeedsCustomBatchRequest) : TDatafeedsCustomBatchResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'datafeeds/batch';
  _Methodid   = 'content.datafeeds.custombatch';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aDatafeedsCustomBatchRequest,TDatafeedsCustomBatchResponse) as TDatafeedsCustomBatchResponse;
end;

Procedure TDatafeedsResource.Delete(datafeedId: string; merchantId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{merchantId}/datafeeds/{datafeedId}';
  _Methodid   = 'content.datafeeds.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datafeedId',datafeedId,'merchantId',merchantId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TDatafeedsResource.Get(datafeedId: string; merchantId: string) : TDatafeed;

Const
  _HTTPMethod = 'GET';
  _Path       = '{merchantId}/datafeeds/{datafeedId}';
  _Methodid   = 'content.datafeeds.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datafeedId',datafeedId,'merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDatafeed) as TDatafeed;
end;

Function TDatafeedsResource.Insert(merchantId: string; aDatafeed : TDatafeed) : TDatafeed;

Const
  _HTTPMethod = 'POST';
  _Path       = '{merchantId}/datafeeds';
  _Methodid   = 'content.datafeeds.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDatafeed,TDatafeed) as TDatafeed;
end;

Function TDatafeedsResource.List(merchantId: string; AQuery : string = '') : TDatafeedsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{merchantId}/datafeeds';
  _Methodid   = 'content.datafeeds.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TDatafeedsListResponse) as TDatafeedsListResponse;
end;


Function TDatafeedsResource.List(merchantId: string; AQuery : TDatafeedslistOptions) : TDatafeedsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(merchantId,_Q);
end;

Function TDatafeedsResource.Patch(datafeedId: string; merchantId: string; aDatafeed : TDatafeed) : TDatafeed;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{merchantId}/datafeeds/{datafeedId}';
  _Methodid   = 'content.datafeeds.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datafeedId',datafeedId,'merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDatafeed,TDatafeed) as TDatafeed;
end;

Function TDatafeedsResource.Update(datafeedId: string; merchantId: string; aDatafeed : TDatafeed) : TDatafeed;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{merchantId}/datafeeds/{datafeedId}';
  _Methodid   = 'content.datafeeds.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datafeedId',datafeedId,'merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDatafeed,TDatafeed) as TDatafeed;
end;



{ --------------------------------------------------------------------
  TDatafeedstatusesResource
  --------------------------------------------------------------------}


Class Function TDatafeedstatusesResource.ResourceName : String;

begin
  Result:='datafeedstatuses';
end;

Class Function TDatafeedstatusesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcontentAPI;
end;

Function TDatafeedstatusesResource.Custombatch(aDatafeedstatusesCustomBatchRequest : TDatafeedstatusesCustomBatchRequest) : TDatafeedstatusesCustomBatchResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'datafeedstatuses/batch';
  _Methodid   = 'content.datafeedstatuses.custombatch';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aDatafeedstatusesCustomBatchRequest,TDatafeedstatusesCustomBatchResponse) as TDatafeedstatusesCustomBatchResponse;
end;

Function TDatafeedstatusesResource.Get(datafeedId: string; merchantId: string) : TDatafeedStatus;

Const
  _HTTPMethod = 'GET';
  _Path       = '{merchantId}/datafeedstatuses/{datafeedId}';
  _Methodid   = 'content.datafeedstatuses.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datafeedId',datafeedId,'merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDatafeedStatus) as TDatafeedStatus;
end;

Function TDatafeedstatusesResource.List(merchantId: string; AQuery : string = '') : TDatafeedstatusesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{merchantId}/datafeedstatuses';
  _Methodid   = 'content.datafeedstatuses.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TDatafeedstatusesListResponse) as TDatafeedstatusesListResponse;
end;


Function TDatafeedstatusesResource.List(merchantId: string; AQuery : TDatafeedstatuseslistOptions) : TDatafeedstatusesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(merchantId,_Q);
end;



{ --------------------------------------------------------------------
  TInventoryResource
  --------------------------------------------------------------------}


Class Function TInventoryResource.ResourceName : String;

begin
  Result:='inventory';
end;

Class Function TInventoryResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcontentAPI;
end;

Function TInventoryResource.Custombatch(aInventoryCustomBatchRequest : TInventoryCustomBatchRequest) : TInventoryCustomBatchResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'inventory/batch';
  _Methodid   = 'content.inventory.custombatch';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aInventoryCustomBatchRequest,TInventoryCustomBatchResponse) as TInventoryCustomBatchResponse;
end;

Function TInventoryResource._set(merchantId: string; productId: string; storeCode: string; aInventorySetRequest : TInventorySetRequest) : TInventorySetResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{merchantId}/inventory/{storeCode}/products/{productId}';
  _Methodid   = 'content.inventory.set';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['merchantId',merchantId,'productId',productId,'storeCode',storeCode]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInventorySetRequest,TInventorySetResponse) as TInventorySetResponse;
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
  Result:=TcontentAPI;
end;

Function TProductsResource.Custombatch(aProductsCustomBatchRequest : TProductsCustomBatchRequest; AQuery : string = '') : TProductsCustomBatchResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'products/batch';
  _Methodid   = 'content.products.custombatch';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aProductsCustomBatchRequest,TProductsCustomBatchResponse) as TProductsCustomBatchResponse;
end;


Function TProductsResource.Custombatch(aProductsCustomBatchRequest : TProductsCustomBatchRequest; AQuery : TProductscustombatchOptions) : TProductsCustomBatchResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'dryRun',AQuery.dryRun);
  Result:=Custombatch(aProductsCustomBatchRequest,_Q);
end;

Procedure TProductsResource.Delete(merchantId: string; productId: string; AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{merchantId}/products/{productId}';
  _Methodid   = 'content.products.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['merchantId',merchantId,'productId',productId]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TProductsResource.Delete(merchantId: string; productId: string; AQuery : TProductsdeleteOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'dryRun',AQuery.dryRun);
  Delete(merchantId,productId,_Q);
end;

Function TProductsResource.Get(merchantId: string; productId: string) : TProduct;

Const
  _HTTPMethod = 'GET';
  _Path       = '{merchantId}/products/{productId}';
  _Methodid   = 'content.products.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['merchantId',merchantId,'productId',productId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TProduct) as TProduct;
end;

Function TProductsResource.Insert(merchantId: string; aProduct : TProduct; AQuery : string = '') : TProduct;

Const
  _HTTPMethod = 'POST';
  _Path       = '{merchantId}/products';
  _Methodid   = 'content.products.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aProduct,TProduct) as TProduct;
end;


Function TProductsResource.Insert(merchantId: string; aProduct : TProduct; AQuery : TProductsinsertOptions) : TProduct;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'dryRun',AQuery.dryRun);
  Result:=Insert(merchantId,aProduct,_Q);
end;

Function TProductsResource.List(merchantId: string; AQuery : string = '') : TProductsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{merchantId}/products';
  _Methodid   = 'content.products.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TProductsListResponse) as TProductsListResponse;
end;


Function TProductsResource.List(merchantId: string; AQuery : TProductslistOptions) : TProductsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(merchantId,_Q);
end;



{ --------------------------------------------------------------------
  TProductstatusesResource
  --------------------------------------------------------------------}


Class Function TProductstatusesResource.ResourceName : String;

begin
  Result:='productstatuses';
end;

Class Function TProductstatusesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcontentAPI;
end;

Function TProductstatusesResource.Custombatch(aProductstatusesCustomBatchRequest : TProductstatusesCustomBatchRequest) : TProductstatusesCustomBatchResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'productstatuses/batch';
  _Methodid   = 'content.productstatuses.custombatch';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aProductstatusesCustomBatchRequest,TProductstatusesCustomBatchResponse) as TProductstatusesCustomBatchResponse;
end;

Function TProductstatusesResource.Get(merchantId: string; productId: string) : TProductStatus;

Const
  _HTTPMethod = 'GET';
  _Path       = '{merchantId}/productstatuses/{productId}';
  _Methodid   = 'content.productstatuses.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['merchantId',merchantId,'productId',productId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TProductStatus) as TProductStatus;
end;

Function TProductstatusesResource.List(merchantId: string; AQuery : string = '') : TProductstatusesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{merchantId}/productstatuses';
  _Methodid   = 'content.productstatuses.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['merchantId',merchantId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TProductstatusesListResponse) as TProductstatusesListResponse;
end;


Function TProductstatusesResource.List(merchantId: string; AQuery : TProductstatuseslistOptions) : TProductstatusesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(merchantId,_Q);
end;



{ --------------------------------------------------------------------
  TContentAPI
  --------------------------------------------------------------------}

Class Function TContentAPI.APIName : String;

begin
  Result:='content';
end;

Class Function TContentAPI.APIVersion : String;

begin
  Result:='v2';
end;

Class Function TContentAPI.APIRevision : String;

begin
  Result:='20150424';
end;

Class Function TContentAPI.APIID : String;

begin
  Result:='content:v2';
end;

Class Function TContentAPI.APITitle : String;

begin
  Result:='Content API for Shopping';
end;

Class Function TContentAPI.APIDescription : String;

begin
  Result:='Manage product items, inventory, and Merchant Center accounts for Google Shopping.';
end;

Class Function TContentAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TContentAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TContentAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TContentAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TContentAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/shopping-content/v2/';
end;

Class Function TContentAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TContentAPI.APIbasePath : string;

begin
  Result:='/content/v2/';
end;

Class Function TContentAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/content/v2/';
end;

Class Function TContentAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TContentAPI.APIservicePath : string;

begin
  Result:='content/v2/';
end;

Class Function TContentAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TContentAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/content';
  Result[0].Description:='Manage your product listings and accounts for Google Shopping';
  
end;

Class Function TContentAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TContentAPI.RegisterAPIResources;

begin
  TAccount.RegisterObject;
  TAccountadwordsLinks.RegisterObject;
  TAccountusers.RegisterObject;
  TAccountAdwordsLink.RegisterObject;
  TAccountIdentifier.RegisterObject;
  TAccountShipping.RegisterObject;
  TAccountShippingcarrierRates.RegisterObject;
  TAccountShippinglocationGroups.RegisterObject;
  TAccountShippingrateTables.RegisterObject;
  TAccountShippingservices.RegisterObject;
  TAccountShippingCarrierRate.RegisterObject;
  TAccountShippingCondition.RegisterObject;
  TAccountShippingLocationGroup.RegisterObject;
  TAccountShippingLocationGrouplocationIds.RegisterObject;
  TAccountShippingLocationGrouppostalCodeRanges.RegisterObject;
  TAccountShippingLocationGrouppostalCodes.RegisterObject;
  TAccountShippingPostalCodeRange.RegisterObject;
  TAccountShippingRateTable.RegisterObject;
  TAccountShippingRateTablecontent.RegisterObject;
  TAccountShippingRateTableCell.RegisterObject;
  TAccountShippingShippingService.RegisterObject;
  TAccountShippingShippingServiceCalculationMethod.RegisterObject;
  TAccountShippingShippingServiceCostRule.RegisterObject;
  TAccountShippingShippingServiceCostRulechildren.RegisterObject;
  TAccountStatus.RegisterObject;
  TAccountStatusdataQualityIssues.RegisterObject;
  TAccountStatusDataQualityIssue.RegisterObject;
  TAccountStatusDataQualityIssueexampleItems.RegisterObject;
  TAccountStatusExampleItem.RegisterObject;
  TAccountTax.RegisterObject;
  TAccountTaxrules.RegisterObject;
  TAccountTaxTaxRule.RegisterObject;
  TAccountUser.RegisterObject;
  TAccountsAuthInfoResponse.RegisterObject;
  TAccountsAuthInfoResponseaccountIdentifiers.RegisterObject;
  TAccountsCustomBatchRequest.RegisterObject;
  TAccountsCustomBatchRequestentries.RegisterObject;
  TAccountsCustomBatchRequestEntry.RegisterObject;
  TAccountsCustomBatchResponse.RegisterObject;
  TAccountsCustomBatchResponseentries.RegisterObject;
  TAccountsCustomBatchResponseEntry.RegisterObject;
  TAccountsListResponse.RegisterObject;
  TAccountsListResponseresources.RegisterObject;
  TAccountshippingCustomBatchRequest.RegisterObject;
  TAccountshippingCustomBatchRequestentries.RegisterObject;
  TAccountshippingCustomBatchRequestEntry.RegisterObject;
  TAccountshippingCustomBatchResponse.RegisterObject;
  TAccountshippingCustomBatchResponseentries.RegisterObject;
  TAccountshippingCustomBatchResponseEntry.RegisterObject;
  TAccountshippingListResponse.RegisterObject;
  TAccountshippingListResponseresources.RegisterObject;
  TAccountstatusesCustomBatchRequest.RegisterObject;
  TAccountstatusesCustomBatchRequestentries.RegisterObject;
  TAccountstatusesCustomBatchRequestEntry.RegisterObject;
  TAccountstatusesCustomBatchResponse.RegisterObject;
  TAccountstatusesCustomBatchResponseentries.RegisterObject;
  TAccountstatusesCustomBatchResponseEntry.RegisterObject;
  TAccountstatusesListResponse.RegisterObject;
  TAccountstatusesListResponseresources.RegisterObject;
  TAccounttaxCustomBatchRequest.RegisterObject;
  TAccounttaxCustomBatchRequestentries.RegisterObject;
  TAccounttaxCustomBatchRequestEntry.RegisterObject;
  TAccounttaxCustomBatchResponse.RegisterObject;
  TAccounttaxCustomBatchResponseentries.RegisterObject;
  TAccounttaxCustomBatchResponseEntry.RegisterObject;
  TAccounttaxListResponse.RegisterObject;
  TAccounttaxListResponseresources.RegisterObject;
  TDatafeed.RegisterObject;
  TDatafeedintendedDestinations.RegisterObject;
  TDatafeedFetchSchedule.RegisterObject;
  TDatafeedFormat.RegisterObject;
  TDatafeedStatus.RegisterObject;
  TDatafeedStatuserrors.RegisterObject;
  TDatafeedStatuswarnings.RegisterObject;
  TDatafeedStatusError.RegisterObject;
  TDatafeedStatusErrorexamples.RegisterObject;
  TDatafeedStatusExample.RegisterObject;
  TDatafeedsCustomBatchRequest.RegisterObject;
  TDatafeedsCustomBatchRequestentries.RegisterObject;
  TDatafeedsCustomBatchRequestEntry.RegisterObject;
  TDatafeedsCustomBatchResponse.RegisterObject;
  TDatafeedsCustomBatchResponseentries.RegisterObject;
  TDatafeedsCustomBatchResponseEntry.RegisterObject;
  TDatafeedsListResponse.RegisterObject;
  TDatafeedsListResponseresources.RegisterObject;
  TDatafeedstatusesCustomBatchRequest.RegisterObject;
  TDatafeedstatusesCustomBatchRequestentries.RegisterObject;
  TDatafeedstatusesCustomBatchRequestEntry.RegisterObject;
  TDatafeedstatusesCustomBatchResponse.RegisterObject;
  TDatafeedstatusesCustomBatchResponseentries.RegisterObject;
  TDatafeedstatusesCustomBatchResponseEntry.RegisterObject;
  TDatafeedstatusesListResponse.RegisterObject;
  TDatafeedstatusesListResponseresources.RegisterObject;
  TError.RegisterObject;
  TErrors.RegisterObject;
  TErrorserrors.RegisterObject;
  TInventory.RegisterObject;
  TInventoryCustomBatchRequest.RegisterObject;
  TInventoryCustomBatchRequestentries.RegisterObject;
  TInventoryCustomBatchRequestEntry.RegisterObject;
  TInventoryCustomBatchResponse.RegisterObject;
  TInventoryCustomBatchResponseentries.RegisterObject;
  TInventoryCustomBatchResponseEntry.RegisterObject;
  TInventorySetRequest.RegisterObject;
  TInventorySetResponse.RegisterObject;
  TLoyaltyPoints.RegisterObject;
  TPrice.RegisterObject;
  TProduct.RegisterObject;
  TProductadditionalImageLinks.RegisterObject;
  TProductadwordsLabels.RegisterObject;
  TProductaspects.RegisterObject;
  TProductcustomAttributes.RegisterObject;
  TProductcustomGroups.RegisterObject;
  TProductdestinations.RegisterObject;
  TProductdisplayAdsSimilarIds.RegisterObject;
  TProductshipping.RegisterObject;
  TProductsizes.RegisterObject;
  TProducttaxes.RegisterObject;
  TProductvalidatedDestinations.RegisterObject;
  TProductwarnings.RegisterObject;
  TProductAspect.RegisterObject;
  TProductCustomAttribute.RegisterObject;
  TProductCustomGroup.RegisterObject;
  TProductCustomGroupattributes.RegisterObject;
  TProductDestination.RegisterObject;
  TProductInstallment.RegisterObject;
  TProductShippingDimension.RegisterObject;
  TProductShippingWeight.RegisterObject;
  TProductStatus.RegisterObject;
  TProductStatusdataQualityIssues.RegisterObject;
  TProductStatusdestinationStatuses.RegisterObject;
  TProductStatusDataQualityIssue.RegisterObject;
  TProductStatusDestinationStatus.RegisterObject;
  TProductTax.RegisterObject;
  TProductUnitPricingBaseMeasure.RegisterObject;
  TProductUnitPricingMeasure.RegisterObject;
  TProductsCustomBatchRequest.RegisterObject;
  TProductsCustomBatchRequestentries.RegisterObject;
  TProductsCustomBatchRequestEntry.RegisterObject;
  TProductsCustomBatchResponse.RegisterObject;
  TProductsCustomBatchResponseentries.RegisterObject;
  TProductsCustomBatchResponseEntry.RegisterObject;
  TProductsListResponse.RegisterObject;
  TProductsListResponseresources.RegisterObject;
  TProductstatusesCustomBatchRequest.RegisterObject;
  TProductstatusesCustomBatchRequestentries.RegisterObject;
  TProductstatusesCustomBatchRequestEntry.RegisterObject;
  TProductstatusesCustomBatchResponse.RegisterObject;
  TProductstatusesCustomBatchResponseentries.RegisterObject;
  TProductstatusesCustomBatchResponseEntry.RegisterObject;
  TProductstatusesListResponse.RegisterObject;
  TProductstatusesListResponseresources.RegisterObject;
  TWeight.RegisterObject;
end;


Function TContentAPI.GetAccountsInstance : TAccountsResource;

begin
  if (FAccountsInstance=Nil) then
    FAccountsInstance:=CreateAccountsResource;
  Result:=FAccountsInstance;
end;

Function TContentAPI.CreateAccountsResource : TAccountsResource;

begin
  Result:=CreateAccountsResource(Self);
end;


Function TContentAPI.CreateAccountsResource(AOwner : TComponent) : TAccountsResource;

begin
  Result:=TAccountsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TContentAPI.GetAccountshippingInstance : TAccountshippingResource;

begin
  if (FAccountshippingInstance=Nil) then
    FAccountshippingInstance:=CreateAccountshippingResource;
  Result:=FAccountshippingInstance;
end;

Function TContentAPI.CreateAccountshippingResource : TAccountshippingResource;

begin
  Result:=CreateAccountshippingResource(Self);
end;


Function TContentAPI.CreateAccountshippingResource(AOwner : TComponent) : TAccountshippingResource;

begin
  Result:=TAccountshippingResource.Create(AOwner);
  Result.API:=Self;
end;



Function TContentAPI.GetAccountstatusesInstance : TAccountstatusesResource;

begin
  if (FAccountstatusesInstance=Nil) then
    FAccountstatusesInstance:=CreateAccountstatusesResource;
  Result:=FAccountstatusesInstance;
end;

Function TContentAPI.CreateAccountstatusesResource : TAccountstatusesResource;

begin
  Result:=CreateAccountstatusesResource(Self);
end;


Function TContentAPI.CreateAccountstatusesResource(AOwner : TComponent) : TAccountstatusesResource;

begin
  Result:=TAccountstatusesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TContentAPI.GetAccounttaxInstance : TAccounttaxResource;

begin
  if (FAccounttaxInstance=Nil) then
    FAccounttaxInstance:=CreateAccounttaxResource;
  Result:=FAccounttaxInstance;
end;

Function TContentAPI.CreateAccounttaxResource : TAccounttaxResource;

begin
  Result:=CreateAccounttaxResource(Self);
end;


Function TContentAPI.CreateAccounttaxResource(AOwner : TComponent) : TAccounttaxResource;

begin
  Result:=TAccounttaxResource.Create(AOwner);
  Result.API:=Self;
end;



Function TContentAPI.GetDatafeedsInstance : TDatafeedsResource;

begin
  if (FDatafeedsInstance=Nil) then
    FDatafeedsInstance:=CreateDatafeedsResource;
  Result:=FDatafeedsInstance;
end;

Function TContentAPI.CreateDatafeedsResource : TDatafeedsResource;

begin
  Result:=CreateDatafeedsResource(Self);
end;


Function TContentAPI.CreateDatafeedsResource(AOwner : TComponent) : TDatafeedsResource;

begin
  Result:=TDatafeedsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TContentAPI.GetDatafeedstatusesInstance : TDatafeedstatusesResource;

begin
  if (FDatafeedstatusesInstance=Nil) then
    FDatafeedstatusesInstance:=CreateDatafeedstatusesResource;
  Result:=FDatafeedstatusesInstance;
end;

Function TContentAPI.CreateDatafeedstatusesResource : TDatafeedstatusesResource;

begin
  Result:=CreateDatafeedstatusesResource(Self);
end;


Function TContentAPI.CreateDatafeedstatusesResource(AOwner : TComponent) : TDatafeedstatusesResource;

begin
  Result:=TDatafeedstatusesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TContentAPI.GetInventoryInstance : TInventoryResource;

begin
  if (FInventoryInstance=Nil) then
    FInventoryInstance:=CreateInventoryResource;
  Result:=FInventoryInstance;
end;

Function TContentAPI.CreateInventoryResource : TInventoryResource;

begin
  Result:=CreateInventoryResource(Self);
end;


Function TContentAPI.CreateInventoryResource(AOwner : TComponent) : TInventoryResource;

begin
  Result:=TInventoryResource.Create(AOwner);
  Result.API:=Self;
end;



Function TContentAPI.GetProductsInstance : TProductsResource;

begin
  if (FProductsInstance=Nil) then
    FProductsInstance:=CreateProductsResource;
  Result:=FProductsInstance;
end;

Function TContentAPI.CreateProductsResource : TProductsResource;

begin
  Result:=CreateProductsResource(Self);
end;


Function TContentAPI.CreateProductsResource(AOwner : TComponent) : TProductsResource;

begin
  Result:=TProductsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TContentAPI.GetProductstatusesInstance : TProductstatusesResource;

begin
  if (FProductstatusesInstance=Nil) then
    FProductstatusesInstance:=CreateProductstatusesResource;
  Result:=FProductstatusesInstance;
end;

Function TContentAPI.CreateProductstatusesResource : TProductstatusesResource;

begin
  Result:=CreateProductstatusesResource(Self);
end;


Function TContentAPI.CreateProductstatusesResource(AOwner : TComponent) : TProductstatusesResource;

begin
  Result:=TProductstatusesResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TContentAPI.RegisterAPI;
end.
