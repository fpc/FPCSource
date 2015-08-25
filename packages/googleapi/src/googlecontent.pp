unit googlecontent;
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
//Generated on: 16-5-15 08:53:01
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAccount = Class;
  TAccountAdwordsLink = Class;
  TAccountIdentifier = Class;
  TAccountShipping = Class;
  TAccountShippingCarrierRate = Class;
  TAccountShippingCondition = Class;
  TAccountShippingLocationGroup = Class;
  TAccountShippingPostalCodeRange = Class;
  TAccountShippingRateTable = Class;
  TAccountShippingRateTableCell = Class;
  TAccountShippingShippingService = Class;
  TAccountShippingShippingServiceCalculationMethod = Class;
  TAccountShippingShippingServiceCostRule = Class;
  TAccountStatus = Class;
  TAccountStatusDataQualityIssue = Class;
  TAccountStatusExampleItem = Class;
  TAccountTax = Class;
  TAccountTaxTaxRule = Class;
  TAccountUser = Class;
  TAccountsAuthInfoResponse = Class;
  TAccountsCustomBatchRequest = Class;
  TAccountsCustomBatchRequestEntry = Class;
  TAccountsCustomBatchResponse = Class;
  TAccountsCustomBatchResponseEntry = Class;
  TAccountsListResponse = Class;
  TAccountshippingCustomBatchRequest = Class;
  TAccountshippingCustomBatchRequestEntry = Class;
  TAccountshippingCustomBatchResponse = Class;
  TAccountshippingCustomBatchResponseEntry = Class;
  TAccountshippingListResponse = Class;
  TAccountstatusesCustomBatchRequest = Class;
  TAccountstatusesCustomBatchRequestEntry = Class;
  TAccountstatusesCustomBatchResponse = Class;
  TAccountstatusesCustomBatchResponseEntry = Class;
  TAccountstatusesListResponse = Class;
  TAccounttaxCustomBatchRequest = Class;
  TAccounttaxCustomBatchRequestEntry = Class;
  TAccounttaxCustomBatchResponse = Class;
  TAccounttaxCustomBatchResponseEntry = Class;
  TAccounttaxListResponse = Class;
  TDatafeed = Class;
  TDatafeedFetchSchedule = Class;
  TDatafeedFormat = Class;
  TDatafeedStatus = Class;
  TDatafeedStatusError = Class;
  TDatafeedStatusExample = Class;
  TDatafeedsCustomBatchRequest = Class;
  TDatafeedsCustomBatchRequestEntry = Class;
  TDatafeedsCustomBatchResponse = Class;
  TDatafeedsCustomBatchResponseEntry = Class;
  TDatafeedsListResponse = Class;
  TDatafeedstatusesCustomBatchRequest = Class;
  TDatafeedstatusesCustomBatchRequestEntry = Class;
  TDatafeedstatusesCustomBatchResponse = Class;
  TDatafeedstatusesCustomBatchResponseEntry = Class;
  TDatafeedstatusesListResponse = Class;
  TError = Class;
  TErrors = Class;
  TInventory = Class;
  TInventoryCustomBatchRequest = Class;
  TInventoryCustomBatchRequestEntry = Class;
  TInventoryCustomBatchResponse = Class;
  TInventoryCustomBatchResponseEntry = Class;
  TInventorySetRequest = Class;
  TInventorySetResponse = Class;
  TLoyaltyPoints = Class;
  TPrice = Class;
  TProduct = Class;
  TProductAspect = Class;
  TProductCustomAttribute = Class;
  TProductCustomGroup = Class;
  TProductDestination = Class;
  TProductInstallment = Class;
  TProductShipping = Class;
  TProductShippingDimension = Class;
  TProductShippingWeight = Class;
  TProductStatus = Class;
  TProductStatusDataQualityIssue = Class;
  TProductStatusDestinationStatus = Class;
  TProductTax = Class;
  TProductUnitPricingBaseMeasure = Class;
  TProductUnitPricingMeasure = Class;
  TProductsCustomBatchRequest = Class;
  TProductsCustomBatchRequestEntry = Class;
  TProductsCustomBatchResponse = Class;
  TProductsCustomBatchResponseEntry = Class;
  TProductsListResponse = Class;
  TProductstatusesCustomBatchRequest = Class;
  TProductstatusesCustomBatchRequestEntry = Class;
  TProductstatusesCustomBatchResponse = Class;
  TProductstatusesCustomBatchResponseEntry = Class;
  TProductstatusesListResponse = Class;
  TWeight = Class;
  TAccountArray = Array of TAccount;
  TAccountAdwordsLinkArray = Array of TAccountAdwordsLink;
  TAccountIdentifierArray = Array of TAccountIdentifier;
  TAccountShippingArray = Array of TAccountShipping;
  TAccountShippingCarrierRateArray = Array of TAccountShippingCarrierRate;
  TAccountShippingConditionArray = Array of TAccountShippingCondition;
  TAccountShippingLocationGroupArray = Array of TAccountShippingLocationGroup;
  TAccountShippingPostalCodeRangeArray = Array of TAccountShippingPostalCodeRange;
  TAccountShippingRateTableArray = Array of TAccountShippingRateTable;
  TAccountShippingRateTableCellArray = Array of TAccountShippingRateTableCell;
  TAccountShippingShippingServiceArray = Array of TAccountShippingShippingService;
  TAccountShippingShippingServiceCalculationMethodArray = Array of TAccountShippingShippingServiceCalculationMethod;
  TAccountShippingShippingServiceCostRuleArray = Array of TAccountShippingShippingServiceCostRule;
  TAccountStatusArray = Array of TAccountStatus;
  TAccountStatusDataQualityIssueArray = Array of TAccountStatusDataQualityIssue;
  TAccountStatusExampleItemArray = Array of TAccountStatusExampleItem;
  TAccountTaxArray = Array of TAccountTax;
  TAccountTaxTaxRuleArray = Array of TAccountTaxTaxRule;
  TAccountUserArray = Array of TAccountUser;
  TAccountsAuthInfoResponseArray = Array of TAccountsAuthInfoResponse;
  TAccountsCustomBatchRequestArray = Array of TAccountsCustomBatchRequest;
  TAccountsCustomBatchRequestEntryArray = Array of TAccountsCustomBatchRequestEntry;
  TAccountsCustomBatchResponseArray = Array of TAccountsCustomBatchResponse;
  TAccountsCustomBatchResponseEntryArray = Array of TAccountsCustomBatchResponseEntry;
  TAccountsListResponseArray = Array of TAccountsListResponse;
  TAccountshippingCustomBatchRequestArray = Array of TAccountshippingCustomBatchRequest;
  TAccountshippingCustomBatchRequestEntryArray = Array of TAccountshippingCustomBatchRequestEntry;
  TAccountshippingCustomBatchResponseArray = Array of TAccountshippingCustomBatchResponse;
  TAccountshippingCustomBatchResponseEntryArray = Array of TAccountshippingCustomBatchResponseEntry;
  TAccountshippingListResponseArray = Array of TAccountshippingListResponse;
  TAccountstatusesCustomBatchRequestArray = Array of TAccountstatusesCustomBatchRequest;
  TAccountstatusesCustomBatchRequestEntryArray = Array of TAccountstatusesCustomBatchRequestEntry;
  TAccountstatusesCustomBatchResponseArray = Array of TAccountstatusesCustomBatchResponse;
  TAccountstatusesCustomBatchResponseEntryArray = Array of TAccountstatusesCustomBatchResponseEntry;
  TAccountstatusesListResponseArray = Array of TAccountstatusesListResponse;
  TAccounttaxCustomBatchRequestArray = Array of TAccounttaxCustomBatchRequest;
  TAccounttaxCustomBatchRequestEntryArray = Array of TAccounttaxCustomBatchRequestEntry;
  TAccounttaxCustomBatchResponseArray = Array of TAccounttaxCustomBatchResponse;
  TAccounttaxCustomBatchResponseEntryArray = Array of TAccounttaxCustomBatchResponseEntry;
  TAccounttaxListResponseArray = Array of TAccounttaxListResponse;
  TDatafeedArray = Array of TDatafeed;
  TDatafeedFetchScheduleArray = Array of TDatafeedFetchSchedule;
  TDatafeedFormatArray = Array of TDatafeedFormat;
  TDatafeedStatusArray = Array of TDatafeedStatus;
  TDatafeedStatusErrorArray = Array of TDatafeedStatusError;
  TDatafeedStatusExampleArray = Array of TDatafeedStatusExample;
  TDatafeedsCustomBatchRequestArray = Array of TDatafeedsCustomBatchRequest;
  TDatafeedsCustomBatchRequestEntryArray = Array of TDatafeedsCustomBatchRequestEntry;
  TDatafeedsCustomBatchResponseArray = Array of TDatafeedsCustomBatchResponse;
  TDatafeedsCustomBatchResponseEntryArray = Array of TDatafeedsCustomBatchResponseEntry;
  TDatafeedsListResponseArray = Array of TDatafeedsListResponse;
  TDatafeedstatusesCustomBatchRequestArray = Array of TDatafeedstatusesCustomBatchRequest;
  TDatafeedstatusesCustomBatchRequestEntryArray = Array of TDatafeedstatusesCustomBatchRequestEntry;
  TDatafeedstatusesCustomBatchResponseArray = Array of TDatafeedstatusesCustomBatchResponse;
  TDatafeedstatusesCustomBatchResponseEntryArray = Array of TDatafeedstatusesCustomBatchResponseEntry;
  TDatafeedstatusesListResponseArray = Array of TDatafeedstatusesListResponse;
  TErrorArray = Array of TError;
  TErrorsArray = Array of TErrors;
  TInventoryArray = Array of TInventory;
  TInventoryCustomBatchRequestArray = Array of TInventoryCustomBatchRequest;
  TInventoryCustomBatchRequestEntryArray = Array of TInventoryCustomBatchRequestEntry;
  TInventoryCustomBatchResponseArray = Array of TInventoryCustomBatchResponse;
  TInventoryCustomBatchResponseEntryArray = Array of TInventoryCustomBatchResponseEntry;
  TInventorySetRequestArray = Array of TInventorySetRequest;
  TInventorySetResponseArray = Array of TInventorySetResponse;
  TLoyaltyPointsArray = Array of TLoyaltyPoints;
  TPriceArray = Array of TPrice;
  TProductArray = Array of TProduct;
  TProductAspectArray = Array of TProductAspect;
  TProductCustomAttributeArray = Array of TProductCustomAttribute;
  TProductCustomGroupArray = Array of TProductCustomGroup;
  TProductDestinationArray = Array of TProductDestination;
  TProductInstallmentArray = Array of TProductInstallment;
  TProductShippingArray = Array of TProductShipping;
  TProductShippingDimensionArray = Array of TProductShippingDimension;
  TProductShippingWeightArray = Array of TProductShippingWeight;
  TProductStatusArray = Array of TProductStatus;
  TProductStatusDataQualityIssueArray = Array of TProductStatusDataQualityIssue;
  TProductStatusDestinationStatusArray = Array of TProductStatusDestinationStatus;
  TProductTaxArray = Array of TProductTax;
  TProductUnitPricingBaseMeasureArray = Array of TProductUnitPricingBaseMeasure;
  TProductUnitPricingMeasureArray = Array of TProductUnitPricingMeasure;
  TProductsCustomBatchRequestArray = Array of TProductsCustomBatchRequest;
  TProductsCustomBatchRequestEntryArray = Array of TProductsCustomBatchRequestEntry;
  TProductsCustomBatchResponseArray = Array of TProductsCustomBatchResponse;
  TProductsCustomBatchResponseEntryArray = Array of TProductsCustomBatchResponseEntry;
  TProductsListResponseArray = Array of TProductsListResponse;
  TProductstatusesCustomBatchRequestArray = Array of TProductstatusesCustomBatchRequest;
  TProductstatusesCustomBatchRequestEntryArray = Array of TProductstatusesCustomBatchRequestEntry;
  TProductstatusesCustomBatchResponseArray = Array of TProductstatusesCustomBatchResponse;
  TProductstatusesCustomBatchResponseEntryArray = Array of TProductstatusesCustomBatchResponseEntry;
  TProductstatusesListResponseArray = Array of TProductstatusesListResponse;
  TWeightArray = Array of TWeight;
  //Anonymous types, using auto-generated names
  TAccountTypeadwordsLinksArray = Array of TAccountAdwordsLink;
  TAccountTypeusersArray = Array of TAccountUser;
  TAccountShippingTypecarrierRatesArray = Array of TAccountShippingCarrierRate;
  TAccountShippingTypelocationGroupsArray = Array of TAccountShippingLocationGroup;
  TAccountShippingTyperateTablesArray = Array of TAccountShippingRateTable;
  TAccountShippingTypeservicesArray = Array of TAccountShippingShippingService;
  TAccountShippingLocationGroupTypepostalCodeRangesArray = Array of TAccountShippingPostalCodeRange;
  TAccountShippingRateTableTypecontentArray = Array of TAccountShippingRateTableCell;
  TAccountShippingShippingServiceCostRuleTypechildrenArray = Array of TAccountShippingShippingServiceCostRule;
  TAccountStatusTypedataQualityIssuesArray = Array of TAccountStatusDataQualityIssue;
  TAccountStatusDataQualityIssueTypeexampleItemsArray = Array of TAccountStatusExampleItem;
  TAccountTaxTyperulesArray = Array of TAccountTaxTaxRule;
  TAccountsAuthInfoResponseTypeaccountIdentifiersArray = Array of TAccountIdentifier;
  TAccountsCustomBatchRequestTypeentriesArray = Array of TAccountsCustomBatchRequestEntry;
  TAccountsCustomBatchResponseTypeentriesArray = Array of TAccountsCustomBatchResponseEntry;
  TAccountsListResponseTyperesourcesArray = Array of TAccount;
  TAccountshippingCustomBatchRequestTypeentriesArray = Array of TAccountshippingCustomBatchRequestEntry;
  TAccountshippingCustomBatchResponseTypeentriesArray = Array of TAccountshippingCustomBatchResponseEntry;
  TAccountshippingListResponseTyperesourcesArray = Array of TAccountShipping;
  TAccountstatusesCustomBatchRequestTypeentriesArray = Array of TAccountstatusesCustomBatchRequestEntry;
  TAccountstatusesCustomBatchResponseTypeentriesArray = Array of TAccountstatusesCustomBatchResponseEntry;
  TAccountstatusesListResponseTyperesourcesArray = Array of TAccountStatus;
  TAccounttaxCustomBatchRequestTypeentriesArray = Array of TAccounttaxCustomBatchRequestEntry;
  TAccounttaxCustomBatchResponseTypeentriesArray = Array of TAccounttaxCustomBatchResponseEntry;
  TAccounttaxListResponseTyperesourcesArray = Array of TAccountTax;
  TDatafeedStatusTypeerrorsArray = Array of TDatafeedStatusError;
  TDatafeedStatusTypewarningsArray = Array of TDatafeedStatusError;
  TDatafeedStatusErrorTypeexamplesArray = Array of TDatafeedStatusExample;
  TDatafeedsCustomBatchRequestTypeentriesArray = Array of TDatafeedsCustomBatchRequestEntry;
  TDatafeedsCustomBatchResponseTypeentriesArray = Array of TDatafeedsCustomBatchResponseEntry;
  TDatafeedsListResponseTyperesourcesArray = Array of TDatafeed;
  TDatafeedstatusesCustomBatchRequestTypeentriesArray = Array of TDatafeedstatusesCustomBatchRequestEntry;
  TDatafeedstatusesCustomBatchResponseTypeentriesArray = Array of TDatafeedstatusesCustomBatchResponseEntry;
  TDatafeedstatusesListResponseTyperesourcesArray = Array of TDatafeedStatus;
  TErrorsTypeerrorsArray = Array of TError;
  TInventoryCustomBatchRequestTypeentriesArray = Array of TInventoryCustomBatchRequestEntry;
  TInventoryCustomBatchResponseTypeentriesArray = Array of TInventoryCustomBatchResponseEntry;
  TProductTypeaspectsArray = Array of TProductAspect;
  TProductTypecustomAttributesArray = Array of TProductCustomAttribute;
  TProductTypecustomGroupsArray = Array of TProductCustomGroup;
  TProductTypedestinationsArray = Array of TProductDestination;
  TProductTypeshippingArray = Array of TProductShipping;
  TProductTypetaxesArray = Array of TProductTax;
  TProductTypewarningsArray = Array of TError;
  TProductCustomGroupTypeattributesArray = Array of TProductCustomAttribute;
  TProductStatusTypedataQualityIssuesArray = Array of TProductStatusDataQualityIssue;
  TProductStatusTypedestinationStatusesArray = Array of TProductStatusDestinationStatus;
  TProductsCustomBatchRequestTypeentriesArray = Array of TProductsCustomBatchRequestEntry;
  TProductsCustomBatchResponseTypeentriesArray = Array of TProductsCustomBatchResponseEntry;
  TProductsListResponseTyperesourcesArray = Array of TProduct;
  TProductstatusesCustomBatchRequestTypeentriesArray = Array of TProductstatusesCustomBatchRequestEntry;
  TProductstatusesCustomBatchResponseTypeentriesArray = Array of TProductstatusesCustomBatchResponseEntry;
  TProductstatusesListResponseTyperesourcesArray = Array of TProductStatus;
  
  { --------------------------------------------------------------------
    TAccount
    --------------------------------------------------------------------}
  
  TAccount = Class(TGoogleBaseObject)
  Private
    FadultContent : boolean;
    FadwordsLinks : TAccountTypeadwordsLinksArray;
    Fid : String;
    Fkind : String;
    Fname : String;
    FreviewsUrl : String;
    FsellerId : String;
    Fusers : TAccountTypeusersArray;
    FwebsiteUrl : String;
  Protected
    //Property setters
    Procedure SetadultContent(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetadwordsLinks(AIndex : Integer; AValue : TAccountTypeadwordsLinksArray); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetreviewsUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetsellerId(AIndex : Integer; AValue : String); virtual;
    Procedure Setusers(AIndex : Integer; AValue : TAccountTypeusersArray); virtual;
    Procedure SetwebsiteUrl(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property adultContent : boolean Index 0 Read FadultContent Write SetadultContent;
    Property adwordsLinks : TAccountTypeadwordsLinksArray Index 8 Read FadwordsLinks Write SetadwordsLinks;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property name : String Index 32 Read Fname Write Setname;
    Property reviewsUrl : String Index 40 Read FreviewsUrl Write SetreviewsUrl;
    Property sellerId : String Index 48 Read FsellerId Write SetsellerId;
    Property users : TAccountTypeusersArray Index 56 Read Fusers Write Setusers;
    Property websiteUrl : String Index 64 Read FwebsiteUrl Write SetwebsiteUrl;
  end;
  TAccountClass = Class of TAccount;
  
  { --------------------------------------------------------------------
    TAccountAdwordsLink
    --------------------------------------------------------------------}
  
  TAccountAdwordsLink = Class(TGoogleBaseObject)
  Private
    FadwordsId : String;
    Fstatus : String;
  Protected
    //Property setters
    Procedure SetadwordsId(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property adwordsId : String Index 0 Read FadwordsId Write SetadwordsId;
    Property status : String Index 8 Read Fstatus Write Setstatus;
  end;
  TAccountAdwordsLinkClass = Class of TAccountAdwordsLink;
  
  { --------------------------------------------------------------------
    TAccountIdentifier
    --------------------------------------------------------------------}
  
  TAccountIdentifier = Class(TGoogleBaseObject)
  Private
    FaggregatorId : String;
    FmerchantId : String;
  Protected
    //Property setters
    Procedure SetaggregatorId(AIndex : Integer; AValue : String); virtual;
    Procedure SetmerchantId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property aggregatorId : String Index 0 Read FaggregatorId Write SetaggregatorId;
    Property merchantId : String Index 8 Read FmerchantId Write SetmerchantId;
  end;
  TAccountIdentifierClass = Class of TAccountIdentifier;
  
  { --------------------------------------------------------------------
    TAccountShipping
    --------------------------------------------------------------------}
  
  TAccountShipping = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FcarrierRates : TAccountShippingTypecarrierRatesArray;
    Fkind : String;
    FlocationGroups : TAccountShippingTypelocationGroupsArray;
    FrateTables : TAccountShippingTyperateTablesArray;
    Fservices : TAccountShippingTypeservicesArray;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcarrierRates(AIndex : Integer; AValue : TAccountShippingTypecarrierRatesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlocationGroups(AIndex : Integer; AValue : TAccountShippingTypelocationGroupsArray); virtual;
    Procedure SetrateTables(AIndex : Integer; AValue : TAccountShippingTyperateTablesArray); virtual;
    Procedure Setservices(AIndex : Integer; AValue : TAccountShippingTypeservicesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property carrierRates : TAccountShippingTypecarrierRatesArray Index 8 Read FcarrierRates Write SetcarrierRates;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property locationGroups : TAccountShippingTypelocationGroupsArray Index 24 Read FlocationGroups Write SetlocationGroups;
    Property rateTables : TAccountShippingTyperateTablesArray Index 32 Read FrateTables Write SetrateTables;
    Property services : TAccountShippingTypeservicesArray Index 40 Read Fservices Write Setservices;
  end;
  TAccountShippingClass = Class of TAccountShipping;
  
  { --------------------------------------------------------------------
    TAccountShippingCarrierRate
    --------------------------------------------------------------------}
  
  TAccountShippingCarrierRate = Class(TGoogleBaseObject)
  Private
    Fcarrier : String;
    FcarrierService : String;
    FmodifierFlatRate : TPrice;
    FmodifierPercent : String;
    Fname : String;
    FsaleCountry : String;
    FshippingOrigin : String;
  Protected
    //Property setters
    Procedure Setcarrier(AIndex : Integer; AValue : String); virtual;
    Procedure SetcarrierService(AIndex : Integer; AValue : String); virtual;
    Procedure SetmodifierFlatRate(AIndex : Integer; AValue : TPrice); virtual;
    Procedure SetmodifierPercent(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetsaleCountry(AIndex : Integer; AValue : String); virtual;
    Procedure SetshippingOrigin(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property carrier : String Index 0 Read Fcarrier Write Setcarrier;
    Property carrierService : String Index 8 Read FcarrierService Write SetcarrierService;
    Property modifierFlatRate : TPrice Index 16 Read FmodifierFlatRate Write SetmodifierFlatRate;
    Property modifierPercent : String Index 24 Read FmodifierPercent Write SetmodifierPercent;
    Property name : String Index 32 Read Fname Write Setname;
    Property saleCountry : String Index 40 Read FsaleCountry Write SetsaleCountry;
    Property shippingOrigin : String Index 48 Read FshippingOrigin Write SetshippingOrigin;
  end;
  TAccountShippingCarrierRateClass = Class of TAccountShippingCarrierRate;
  
  { --------------------------------------------------------------------
    TAccountShippingCondition
    --------------------------------------------------------------------}
  
  TAccountShippingCondition = Class(TGoogleBaseObject)
  Private
    FdeliveryLocationGroup : String;
    FdeliveryLocationId : String;
    FdeliveryPostalCode : String;
    FdeliveryPostalCodeRange : TAccountShippingPostalCodeRange;
    FpriceMax : TPrice;
    FshippingLabel : String;
    FweightMax : TWeight;
  Protected
    //Property setters
    Procedure SetdeliveryLocationGroup(AIndex : Integer; AValue : String); virtual;
    Procedure SetdeliveryLocationId(AIndex : Integer; AValue : String); virtual;
    Procedure SetdeliveryPostalCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetdeliveryPostalCodeRange(AIndex : Integer; AValue : TAccountShippingPostalCodeRange); virtual;
    Procedure SetpriceMax(AIndex : Integer; AValue : TPrice); virtual;
    Procedure SetshippingLabel(AIndex : Integer; AValue : String); virtual;
    Procedure SetweightMax(AIndex : Integer; AValue : TWeight); virtual;
  Public
  Published
    Property deliveryLocationGroup : String Index 0 Read FdeliveryLocationGroup Write SetdeliveryLocationGroup;
    Property deliveryLocationId : String Index 8 Read FdeliveryLocationId Write SetdeliveryLocationId;
    Property deliveryPostalCode : String Index 16 Read FdeliveryPostalCode Write SetdeliveryPostalCode;
    Property deliveryPostalCodeRange : TAccountShippingPostalCodeRange Index 24 Read FdeliveryPostalCodeRange Write SetdeliveryPostalCodeRange;
    Property priceMax : TPrice Index 32 Read FpriceMax Write SetpriceMax;
    Property shippingLabel : String Index 40 Read FshippingLabel Write SetshippingLabel;
    Property weightMax : TWeight Index 48 Read FweightMax Write SetweightMax;
  end;
  TAccountShippingConditionClass = Class of TAccountShippingCondition;
  
  { --------------------------------------------------------------------
    TAccountShippingLocationGroup
    --------------------------------------------------------------------}
  
  TAccountShippingLocationGroup = Class(TGoogleBaseObject)
  Private
    Fcountry : String;
    FlocationIds : TStringArray;
    Fname : String;
    FpostalCodeRanges : TAccountShippingLocationGroupTypepostalCodeRangesArray;
    FpostalCodes : TStringArray;
  Protected
    //Property setters
    Procedure Setcountry(AIndex : Integer; AValue : String); virtual;
    Procedure SetlocationIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetpostalCodeRanges(AIndex : Integer; AValue : TAccountShippingLocationGroupTypepostalCodeRangesArray); virtual;
    Procedure SetpostalCodes(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property country : String Index 0 Read Fcountry Write Setcountry;
    Property locationIds : TStringArray Index 8 Read FlocationIds Write SetlocationIds;
    Property name : String Index 16 Read Fname Write Setname;
    Property postalCodeRanges : TAccountShippingLocationGroupTypepostalCodeRangesArray Index 24 Read FpostalCodeRanges Write SetpostalCodeRanges;
    Property postalCodes : TStringArray Index 32 Read FpostalCodes Write SetpostalCodes;
  end;
  TAccountShippingLocationGroupClass = Class of TAccountShippingLocationGroup;
  
  { --------------------------------------------------------------------
    TAccountShippingPostalCodeRange
    --------------------------------------------------------------------}
  
  TAccountShippingPostalCodeRange = Class(TGoogleBaseObject)
  Private
    F_end : String;
    Fstart : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_end(AIndex : Integer; AValue : String); virtual;
    Procedure Setstart(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property _end : String Index 0 Read F_end Write Set_end;
    Property start : String Index 8 Read Fstart Write Setstart;
  end;
  TAccountShippingPostalCodeRangeClass = Class of TAccountShippingPostalCodeRange;
  
  { --------------------------------------------------------------------
    TAccountShippingRateTable
    --------------------------------------------------------------------}
  
  TAccountShippingRateTable = Class(TGoogleBaseObject)
  Private
    Fcontent : TAccountShippingRateTableTypecontentArray;
    Fname : String;
    FsaleCountry : String;
  Protected
    //Property setters
    Procedure Setcontent(AIndex : Integer; AValue : TAccountShippingRateTableTypecontentArray); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetsaleCountry(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property content : TAccountShippingRateTableTypecontentArray Index 0 Read Fcontent Write Setcontent;
    Property name : String Index 8 Read Fname Write Setname;
    Property saleCountry : String Index 16 Read FsaleCountry Write SetsaleCountry;
  end;
  TAccountShippingRateTableClass = Class of TAccountShippingRateTable;
  
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
    Fname : String;
    FsaleCountry : String;
  Protected
    //Property setters
    Procedure Setactive(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcalculationMethod(AIndex : Integer; AValue : TAccountShippingShippingServiceCalculationMethod); virtual;
    Procedure SetcostRuleTree(AIndex : Integer; AValue : TAccountShippingShippingServiceCostRule); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetsaleCountry(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property active : boolean Index 0 Read Factive Write Setactive;
    Property calculationMethod : TAccountShippingShippingServiceCalculationMethod Index 8 Read FcalculationMethod Write SetcalculationMethod;
    Property costRuleTree : TAccountShippingShippingServiceCostRule Index 16 Read FcostRuleTree Write SetcostRuleTree;
    Property name : String Index 24 Read Fname Write Setname;
    Property saleCountry : String Index 32 Read FsaleCountry Write SetsaleCountry;
  end;
  TAccountShippingShippingServiceClass = Class of TAccountShippingShippingService;
  
  { --------------------------------------------------------------------
    TAccountShippingShippingServiceCalculationMethod
    --------------------------------------------------------------------}
  
  TAccountShippingShippingServiceCalculationMethod = Class(TGoogleBaseObject)
  Private
    FcarrierRate : String;
    Fexcluded : boolean;
    FflatRate : TPrice;
    FpercentageRate : String;
    FrateTable : String;
  Protected
    //Property setters
    Procedure SetcarrierRate(AIndex : Integer; AValue : String); virtual;
    Procedure Setexcluded(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetflatRate(AIndex : Integer; AValue : TPrice); virtual;
    Procedure SetpercentageRate(AIndex : Integer; AValue : String); virtual;
    Procedure SetrateTable(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property carrierRate : String Index 0 Read FcarrierRate Write SetcarrierRate;
    Property excluded : boolean Index 8 Read Fexcluded Write Setexcluded;
    Property flatRate : TPrice Index 16 Read FflatRate Write SetflatRate;
    Property percentageRate : String Index 24 Read FpercentageRate Write SetpercentageRate;
    Property rateTable : String Index 32 Read FrateTable Write SetrateTable;
  end;
  TAccountShippingShippingServiceCalculationMethodClass = Class of TAccountShippingShippingServiceCalculationMethod;
  
  { --------------------------------------------------------------------
    TAccountShippingShippingServiceCostRule
    --------------------------------------------------------------------}
  
  TAccountShippingShippingServiceCostRule = Class(TGoogleBaseObject)
  Private
    FcalculationMethod : TAccountShippingShippingServiceCalculationMethod;
    Fchildren : TAccountShippingShippingServiceCostRuleTypechildrenArray;
    Fcondition : TAccountShippingCondition;
  Protected
    //Property setters
    Procedure SetcalculationMethod(AIndex : Integer; AValue : TAccountShippingShippingServiceCalculationMethod); virtual;
    Procedure Setchildren(AIndex : Integer; AValue : TAccountShippingShippingServiceCostRuleTypechildrenArray); virtual;
    Procedure Setcondition(AIndex : Integer; AValue : TAccountShippingCondition); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property calculationMethod : TAccountShippingShippingServiceCalculationMethod Index 0 Read FcalculationMethod Write SetcalculationMethod;
    Property children : TAccountShippingShippingServiceCostRuleTypechildrenArray Index 8 Read Fchildren Write Setchildren;
    Property condition : TAccountShippingCondition Index 16 Read Fcondition Write Setcondition;
  end;
  TAccountShippingShippingServiceCostRuleClass = Class of TAccountShippingShippingServiceCostRule;
  
  { --------------------------------------------------------------------
    TAccountStatus
    --------------------------------------------------------------------}
  
  TAccountStatus = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FdataQualityIssues : TAccountStatusTypedataQualityIssuesArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetdataQualityIssues(AIndex : Integer; AValue : TAccountStatusTypedataQualityIssuesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property dataQualityIssues : TAccountStatusTypedataQualityIssuesArray Index 8 Read FdataQualityIssues Write SetdataQualityIssues;
    Property kind : String Index 16 Read Fkind Write Setkind;
  end;
  TAccountStatusClass = Class of TAccountStatus;
  
  { --------------------------------------------------------------------
    TAccountStatusDataQualityIssue
    --------------------------------------------------------------------}
  
  TAccountStatusDataQualityIssue = Class(TGoogleBaseObject)
  Private
    Fcountry : String;
    FdisplayedValue : String;
    FexampleItems : TAccountStatusDataQualityIssueTypeexampleItemsArray;
    Fid : String;
    FlastChecked : String;
    FnumItems : integer;
    Fseverity : String;
    FsubmittedValue : String;
  Protected
    //Property setters
    Procedure Setcountry(AIndex : Integer; AValue : String); virtual;
    Procedure SetdisplayedValue(AIndex : Integer; AValue : String); virtual;
    Procedure SetexampleItems(AIndex : Integer; AValue : TAccountStatusDataQualityIssueTypeexampleItemsArray); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastChecked(AIndex : Integer; AValue : String); virtual;
    Procedure SetnumItems(AIndex : Integer; AValue : integer); virtual;
    Procedure Setseverity(AIndex : Integer; AValue : String); virtual;
    Procedure SetsubmittedValue(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property country : String Index 0 Read Fcountry Write Setcountry;
    Property displayedValue : String Index 8 Read FdisplayedValue Write SetdisplayedValue;
    Property exampleItems : TAccountStatusDataQualityIssueTypeexampleItemsArray Index 16 Read FexampleItems Write SetexampleItems;
    Property id : String Index 24 Read Fid Write Setid;
    Property lastChecked : String Index 32 Read FlastChecked Write SetlastChecked;
    Property numItems : integer Index 40 Read FnumItems Write SetnumItems;
    Property severity : String Index 48 Read Fseverity Write Setseverity;
    Property submittedValue : String Index 56 Read FsubmittedValue Write SetsubmittedValue;
  end;
  TAccountStatusDataQualityIssueClass = Class of TAccountStatusDataQualityIssue;
  
  { --------------------------------------------------------------------
    TAccountStatusExampleItem
    --------------------------------------------------------------------}
  
  TAccountStatusExampleItem = Class(TGoogleBaseObject)
  Private
    FitemId : String;
    Flink : String;
    FsubmittedValue : String;
    Ftitle : String;
    FvalueOnLandingPage : String;
  Protected
    //Property setters
    Procedure SetitemId(AIndex : Integer; AValue : String); virtual;
    Procedure Setlink(AIndex : Integer; AValue : String); virtual;
    Procedure SetsubmittedValue(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure SetvalueOnLandingPage(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property itemId : String Index 0 Read FitemId Write SetitemId;
    Property link : String Index 8 Read Flink Write Setlink;
    Property submittedValue : String Index 16 Read FsubmittedValue Write SetsubmittedValue;
    Property title : String Index 24 Read Ftitle Write Settitle;
    Property valueOnLandingPage : String Index 32 Read FvalueOnLandingPage Write SetvalueOnLandingPage;
  end;
  TAccountStatusExampleItemClass = Class of TAccountStatusExampleItem;
  
  { --------------------------------------------------------------------
    TAccountTax
    --------------------------------------------------------------------}
  
  TAccountTax = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Fkind : String;
    Frules : TAccountTaxTyperulesArray;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setrules(AIndex : Integer; AValue : TAccountTaxTyperulesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property rules : TAccountTaxTyperulesArray Index 16 Read Frules Write Setrules;
  end;
  TAccountTaxClass = Class of TAccountTax;
  
  { --------------------------------------------------------------------
    TAccountTaxTaxRule
    --------------------------------------------------------------------}
  
  TAccountTaxTaxRule = Class(TGoogleBaseObject)
  Private
    Fcountry : String;
    FlocationId : String;
    FratePercent : String;
    FshippingTaxed : boolean;
    FuseGlobalRate : boolean;
  Protected
    //Property setters
    Procedure Setcountry(AIndex : Integer; AValue : String); virtual;
    Procedure SetlocationId(AIndex : Integer; AValue : String); virtual;
    Procedure SetratePercent(AIndex : Integer; AValue : String); virtual;
    Procedure SetshippingTaxed(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetuseGlobalRate(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property country : String Index 0 Read Fcountry Write Setcountry;
    Property locationId : String Index 8 Read FlocationId Write SetlocationId;
    Property ratePercent : String Index 16 Read FratePercent Write SetratePercent;
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
    FemailAddress : String;
  Protected
    //Property setters
    Procedure Setadmin(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetemailAddress(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property admin : boolean Index 0 Read Fadmin Write Setadmin;
    Property emailAddress : String Index 8 Read FemailAddress Write SetemailAddress;
  end;
  TAccountUserClass = Class of TAccountUser;
  
  { --------------------------------------------------------------------
    TAccountsAuthInfoResponse
    --------------------------------------------------------------------}
  
  TAccountsAuthInfoResponse = Class(TGoogleBaseObject)
  Private
    FaccountIdentifiers : TAccountsAuthInfoResponseTypeaccountIdentifiersArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetaccountIdentifiers(AIndex : Integer; AValue : TAccountsAuthInfoResponseTypeaccountIdentifiersArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property accountIdentifiers : TAccountsAuthInfoResponseTypeaccountIdentifiersArray Index 0 Read FaccountIdentifiers Write SetaccountIdentifiers;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TAccountsAuthInfoResponseClass = Class of TAccountsAuthInfoResponse;
  
  { --------------------------------------------------------------------
    TAccountsCustomBatchRequest
    --------------------------------------------------------------------}
  
  TAccountsCustomBatchRequest = Class(TGoogleBaseObject)
  Private
    Fentries : TAccountsCustomBatchRequestTypeentriesArray;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TAccountsCustomBatchRequestTypeentriesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entries : TAccountsCustomBatchRequestTypeentriesArray Index 0 Read Fentries Write Setentries;
  end;
  TAccountsCustomBatchRequestClass = Class of TAccountsCustomBatchRequest;
  
  { --------------------------------------------------------------------
    TAccountsCustomBatchRequestEntry
    --------------------------------------------------------------------}
  
  TAccountsCustomBatchRequestEntry = Class(TGoogleBaseObject)
  Private
    Faccount : TAccount;
    FaccountId : String;
    FbatchId : integer;
    FmerchantId : String;
    Fmethod : String;
  Protected
    //Property setters
    Procedure Setaccount(AIndex : Integer; AValue : TAccount); virtual;
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmerchantId(AIndex : Integer; AValue : String); virtual;
    Procedure Setmethod(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property account : TAccount Index 0 Read Faccount Write Setaccount;
    Property accountId : String Index 8 Read FaccountId Write SetaccountId;
    Property batchId : integer Index 16 Read FbatchId Write SetbatchId;
    Property merchantId : String Index 24 Read FmerchantId Write SetmerchantId;
    Property method : String Index 32 Read Fmethod Write Setmethod;
  end;
  TAccountsCustomBatchRequestEntryClass = Class of TAccountsCustomBatchRequestEntry;
  
  { --------------------------------------------------------------------
    TAccountsCustomBatchResponse
    --------------------------------------------------------------------}
  
  TAccountsCustomBatchResponse = Class(TGoogleBaseObject)
  Private
    Fentries : TAccountsCustomBatchResponseTypeentriesArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TAccountsCustomBatchResponseTypeentriesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entries : TAccountsCustomBatchResponseTypeentriesArray Index 0 Read Fentries Write Setentries;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TAccountsCustomBatchResponseClass = Class of TAccountsCustomBatchResponse;
  
  { --------------------------------------------------------------------
    TAccountsCustomBatchResponseEntry
    --------------------------------------------------------------------}
  
  TAccountsCustomBatchResponseEntry = Class(TGoogleBaseObject)
  Private
    Faccount : TAccount;
    FbatchId : integer;
    Ferrors : TErrors;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setaccount(AIndex : Integer; AValue : TAccount); virtual;
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TErrors); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property account : TAccount Index 0 Read Faccount Write Setaccount;
    Property batchId : integer Index 8 Read FbatchId Write SetbatchId;
    Property errors : TErrors Index 16 Read Ferrors Write Seterrors;
    Property kind : String Index 24 Read Fkind Write Setkind;
  end;
  TAccountsCustomBatchResponseEntryClass = Class of TAccountsCustomBatchResponseEntry;
  
  { --------------------------------------------------------------------
    TAccountsListResponse
    --------------------------------------------------------------------}
  
  TAccountsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    Fresources : TAccountsListResponseTyperesourcesArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TAccountsListResponseTyperesourcesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property resources : TAccountsListResponseTyperesourcesArray Index 16 Read Fresources Write Setresources;
  end;
  TAccountsListResponseClass = Class of TAccountsListResponse;
  
  { --------------------------------------------------------------------
    TAccountshippingCustomBatchRequest
    --------------------------------------------------------------------}
  
  TAccountshippingCustomBatchRequest = Class(TGoogleBaseObject)
  Private
    Fentries : TAccountshippingCustomBatchRequestTypeentriesArray;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TAccountshippingCustomBatchRequestTypeentriesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entries : TAccountshippingCustomBatchRequestTypeentriesArray Index 0 Read Fentries Write Setentries;
  end;
  TAccountshippingCustomBatchRequestClass = Class of TAccountshippingCustomBatchRequest;
  
  { --------------------------------------------------------------------
    TAccountshippingCustomBatchRequestEntry
    --------------------------------------------------------------------}
  
  TAccountshippingCustomBatchRequestEntry = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FaccountShipping : TAccountShipping;
    FbatchId : integer;
    FmerchantId : String;
    Fmethod : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetaccountShipping(AIndex : Integer; AValue : TAccountShipping); virtual;
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmerchantId(AIndex : Integer; AValue : String); virtual;
    Procedure Setmethod(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property accountShipping : TAccountShipping Index 8 Read FaccountShipping Write SetaccountShipping;
    Property batchId : integer Index 16 Read FbatchId Write SetbatchId;
    Property merchantId : String Index 24 Read FmerchantId Write SetmerchantId;
    Property method : String Index 32 Read Fmethod Write Setmethod;
  end;
  TAccountshippingCustomBatchRequestEntryClass = Class of TAccountshippingCustomBatchRequestEntry;
  
  { --------------------------------------------------------------------
    TAccountshippingCustomBatchResponse
    --------------------------------------------------------------------}
  
  TAccountshippingCustomBatchResponse = Class(TGoogleBaseObject)
  Private
    Fentries : TAccountshippingCustomBatchResponseTypeentriesArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TAccountshippingCustomBatchResponseTypeentriesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entries : TAccountshippingCustomBatchResponseTypeentriesArray Index 0 Read Fentries Write Setentries;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TAccountshippingCustomBatchResponseClass = Class of TAccountshippingCustomBatchResponse;
  
  { --------------------------------------------------------------------
    TAccountshippingCustomBatchResponseEntry
    --------------------------------------------------------------------}
  
  TAccountshippingCustomBatchResponseEntry = Class(TGoogleBaseObject)
  Private
    FaccountShipping : TAccountShipping;
    FbatchId : integer;
    Ferrors : TErrors;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetaccountShipping(AIndex : Integer; AValue : TAccountShipping); virtual;
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TErrors); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountShipping : TAccountShipping Index 0 Read FaccountShipping Write SetaccountShipping;
    Property batchId : integer Index 8 Read FbatchId Write SetbatchId;
    Property errors : TErrors Index 16 Read Ferrors Write Seterrors;
    Property kind : String Index 24 Read Fkind Write Setkind;
  end;
  TAccountshippingCustomBatchResponseEntryClass = Class of TAccountshippingCustomBatchResponseEntry;
  
  { --------------------------------------------------------------------
    TAccountshippingListResponse
    --------------------------------------------------------------------}
  
  TAccountshippingListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    Fresources : TAccountshippingListResponseTyperesourcesArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TAccountshippingListResponseTyperesourcesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property resources : TAccountshippingListResponseTyperesourcesArray Index 16 Read Fresources Write Setresources;
  end;
  TAccountshippingListResponseClass = Class of TAccountshippingListResponse;
  
  { --------------------------------------------------------------------
    TAccountstatusesCustomBatchRequest
    --------------------------------------------------------------------}
  
  TAccountstatusesCustomBatchRequest = Class(TGoogleBaseObject)
  Private
    Fentries : TAccountstatusesCustomBatchRequestTypeentriesArray;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TAccountstatusesCustomBatchRequestTypeentriesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entries : TAccountstatusesCustomBatchRequestTypeentriesArray Index 0 Read Fentries Write Setentries;
  end;
  TAccountstatusesCustomBatchRequestClass = Class of TAccountstatusesCustomBatchRequest;
  
  { --------------------------------------------------------------------
    TAccountstatusesCustomBatchRequestEntry
    --------------------------------------------------------------------}
  
  TAccountstatusesCustomBatchRequestEntry = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FbatchId : integer;
    FmerchantId : String;
    Fmethod : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmerchantId(AIndex : Integer; AValue : String); virtual;
    Procedure Setmethod(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property batchId : integer Index 8 Read FbatchId Write SetbatchId;
    Property merchantId : String Index 16 Read FmerchantId Write SetmerchantId;
    Property method : String Index 24 Read Fmethod Write Setmethod;
  end;
  TAccountstatusesCustomBatchRequestEntryClass = Class of TAccountstatusesCustomBatchRequestEntry;
  
  { --------------------------------------------------------------------
    TAccountstatusesCustomBatchResponse
    --------------------------------------------------------------------}
  
  TAccountstatusesCustomBatchResponse = Class(TGoogleBaseObject)
  Private
    Fentries : TAccountstatusesCustomBatchResponseTypeentriesArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TAccountstatusesCustomBatchResponseTypeentriesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entries : TAccountstatusesCustomBatchResponseTypeentriesArray Index 0 Read Fentries Write Setentries;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TAccountstatusesCustomBatchResponseClass = Class of TAccountstatusesCustomBatchResponse;
  
  { --------------------------------------------------------------------
    TAccountstatusesCustomBatchResponseEntry
    --------------------------------------------------------------------}
  
  TAccountstatusesCustomBatchResponseEntry = Class(TGoogleBaseObject)
  Private
    FaccountStatus : TAccountStatus;
    FbatchId : integer;
    Ferrors : TErrors;
  Protected
    //Property setters
    Procedure SetaccountStatus(AIndex : Integer; AValue : TAccountStatus); virtual;
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TErrors); virtual;
  Public
  Published
    Property accountStatus : TAccountStatus Index 0 Read FaccountStatus Write SetaccountStatus;
    Property batchId : integer Index 8 Read FbatchId Write SetbatchId;
    Property errors : TErrors Index 16 Read Ferrors Write Seterrors;
  end;
  TAccountstatusesCustomBatchResponseEntryClass = Class of TAccountstatusesCustomBatchResponseEntry;
  
  { --------------------------------------------------------------------
    TAccountstatusesListResponse
    --------------------------------------------------------------------}
  
  TAccountstatusesListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    Fresources : TAccountstatusesListResponseTyperesourcesArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TAccountstatusesListResponseTyperesourcesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property resources : TAccountstatusesListResponseTyperesourcesArray Index 16 Read Fresources Write Setresources;
  end;
  TAccountstatusesListResponseClass = Class of TAccountstatusesListResponse;
  
  { --------------------------------------------------------------------
    TAccounttaxCustomBatchRequest
    --------------------------------------------------------------------}
  
  TAccounttaxCustomBatchRequest = Class(TGoogleBaseObject)
  Private
    Fentries : TAccounttaxCustomBatchRequestTypeentriesArray;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TAccounttaxCustomBatchRequestTypeentriesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entries : TAccounttaxCustomBatchRequestTypeentriesArray Index 0 Read Fentries Write Setentries;
  end;
  TAccounttaxCustomBatchRequestClass = Class of TAccounttaxCustomBatchRequest;
  
  { --------------------------------------------------------------------
    TAccounttaxCustomBatchRequestEntry
    --------------------------------------------------------------------}
  
  TAccounttaxCustomBatchRequestEntry = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FaccountTax : TAccountTax;
    FbatchId : integer;
    FmerchantId : String;
    Fmethod : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetaccountTax(AIndex : Integer; AValue : TAccountTax); virtual;
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmerchantId(AIndex : Integer; AValue : String); virtual;
    Procedure Setmethod(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property accountTax : TAccountTax Index 8 Read FaccountTax Write SetaccountTax;
    Property batchId : integer Index 16 Read FbatchId Write SetbatchId;
    Property merchantId : String Index 24 Read FmerchantId Write SetmerchantId;
    Property method : String Index 32 Read Fmethod Write Setmethod;
  end;
  TAccounttaxCustomBatchRequestEntryClass = Class of TAccounttaxCustomBatchRequestEntry;
  
  { --------------------------------------------------------------------
    TAccounttaxCustomBatchResponse
    --------------------------------------------------------------------}
  
  TAccounttaxCustomBatchResponse = Class(TGoogleBaseObject)
  Private
    Fentries : TAccounttaxCustomBatchResponseTypeentriesArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TAccounttaxCustomBatchResponseTypeentriesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entries : TAccounttaxCustomBatchResponseTypeentriesArray Index 0 Read Fentries Write Setentries;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TAccounttaxCustomBatchResponseClass = Class of TAccounttaxCustomBatchResponse;
  
  { --------------------------------------------------------------------
    TAccounttaxCustomBatchResponseEntry
    --------------------------------------------------------------------}
  
  TAccounttaxCustomBatchResponseEntry = Class(TGoogleBaseObject)
  Private
    FaccountTax : TAccountTax;
    FbatchId : integer;
    Ferrors : TErrors;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetaccountTax(AIndex : Integer; AValue : TAccountTax); virtual;
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TErrors); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountTax : TAccountTax Index 0 Read FaccountTax Write SetaccountTax;
    Property batchId : integer Index 8 Read FbatchId Write SetbatchId;
    Property errors : TErrors Index 16 Read Ferrors Write Seterrors;
    Property kind : String Index 24 Read Fkind Write Setkind;
  end;
  TAccounttaxCustomBatchResponseEntryClass = Class of TAccounttaxCustomBatchResponseEntry;
  
  { --------------------------------------------------------------------
    TAccounttaxListResponse
    --------------------------------------------------------------------}
  
  TAccounttaxListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    Fresources : TAccounttaxListResponseTyperesourcesArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TAccounttaxListResponseTyperesourcesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property resources : TAccounttaxListResponseTyperesourcesArray Index 16 Read Fresources Write Setresources;
  end;
  TAccounttaxListResponseClass = Class of TAccounttaxListResponse;
  
  { --------------------------------------------------------------------
    TDatafeed
    --------------------------------------------------------------------}
  
  TDatafeed = Class(TGoogleBaseObject)
  Private
    FattributeLanguage : String;
    FcontentLanguage : String;
    FcontentType : String;
    FfetchSchedule : TDatafeedFetchSchedule;
    FfileName : String;
    Fformat : TDatafeedFormat;
    Fid : String;
    FintendedDestinations : TStringArray;
    Fkind : String;
    Fname : String;
    FtargetCountry : String;
  Protected
    //Property setters
    Procedure SetattributeLanguage(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontentLanguage(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontentType(AIndex : Integer; AValue : String); virtual;
    Procedure SetfetchSchedule(AIndex : Integer; AValue : TDatafeedFetchSchedule); virtual;
    Procedure SetfileName(AIndex : Integer; AValue : String); virtual;
    Procedure Setformat(AIndex : Integer; AValue : TDatafeedFormat); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetintendedDestinations(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetCountry(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property attributeLanguage : String Index 0 Read FattributeLanguage Write SetattributeLanguage;
    Property contentLanguage : String Index 8 Read FcontentLanguage Write SetcontentLanguage;
    Property contentType : String Index 16 Read FcontentType Write SetcontentType;
    Property fetchSchedule : TDatafeedFetchSchedule Index 24 Read FfetchSchedule Write SetfetchSchedule;
    Property fileName : String Index 32 Read FfileName Write SetfileName;
    Property format : TDatafeedFormat Index 40 Read Fformat Write Setformat;
    Property id : String Index 48 Read Fid Write Setid;
    Property intendedDestinations : TStringArray Index 56 Read FintendedDestinations Write SetintendedDestinations;
    Property kind : String Index 64 Read Fkind Write Setkind;
    Property name : String Index 72 Read Fname Write Setname;
    Property targetCountry : String Index 80 Read FtargetCountry Write SettargetCountry;
  end;
  TDatafeedClass = Class of TDatafeed;
  
  { --------------------------------------------------------------------
    TDatafeedFetchSchedule
    --------------------------------------------------------------------}
  
  TDatafeedFetchSchedule = Class(TGoogleBaseObject)
  Private
    FdayOfMonth : integer;
    FfetchUrl : String;
    Fhour : integer;
    Fpassword : String;
    FtimeZone : String;
    Fusername : String;
    Fweekday : String;
  Protected
    //Property setters
    Procedure SetdayOfMonth(AIndex : Integer; AValue : integer); virtual;
    Procedure SetfetchUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Sethour(AIndex : Integer; AValue : integer); virtual;
    Procedure Setpassword(AIndex : Integer; AValue : String); virtual;
    Procedure SettimeZone(AIndex : Integer; AValue : String); virtual;
    Procedure Setusername(AIndex : Integer; AValue : String); virtual;
    Procedure Setweekday(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property dayOfMonth : integer Index 0 Read FdayOfMonth Write SetdayOfMonth;
    Property fetchUrl : String Index 8 Read FfetchUrl Write SetfetchUrl;
    Property hour : integer Index 16 Read Fhour Write Sethour;
    Property password : String Index 24 Read Fpassword Write Setpassword;
    Property timeZone : String Index 32 Read FtimeZone Write SettimeZone;
    Property username : String Index 40 Read Fusername Write Setusername;
    Property weekday : String Index 48 Read Fweekday Write Setweekday;
  end;
  TDatafeedFetchScheduleClass = Class of TDatafeedFetchSchedule;
  
  { --------------------------------------------------------------------
    TDatafeedFormat
    --------------------------------------------------------------------}
  
  TDatafeedFormat = Class(TGoogleBaseObject)
  Private
    FcolumnDelimiter : String;
    FfileEncoding : String;
    FquotingMode : String;
  Protected
    //Property setters
    Procedure SetcolumnDelimiter(AIndex : Integer; AValue : String); virtual;
    Procedure SetfileEncoding(AIndex : Integer; AValue : String); virtual;
    Procedure SetquotingMode(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property columnDelimiter : String Index 0 Read FcolumnDelimiter Write SetcolumnDelimiter;
    Property fileEncoding : String Index 8 Read FfileEncoding Write SetfileEncoding;
    Property quotingMode : String Index 16 Read FquotingMode Write SetquotingMode;
  end;
  TDatafeedFormatClass = Class of TDatafeedFormat;
  
  { --------------------------------------------------------------------
    TDatafeedStatus
    --------------------------------------------------------------------}
  
  TDatafeedStatus = Class(TGoogleBaseObject)
  Private
    FdatafeedId : String;
    Ferrors : TDatafeedStatusTypeerrorsArray;
    FitemsTotal : String;
    FitemsValid : String;
    Fkind : String;
    FlastUploadDate : String;
    FprocessingStatus : String;
    Fwarnings : TDatafeedStatusTypewarningsArray;
  Protected
    //Property setters
    Procedure SetdatafeedId(AIndex : Integer; AValue : String); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TDatafeedStatusTypeerrorsArray); virtual;
    Procedure SetitemsTotal(AIndex : Integer; AValue : String); virtual;
    Procedure SetitemsValid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastUploadDate(AIndex : Integer; AValue : String); virtual;
    Procedure SetprocessingStatus(AIndex : Integer; AValue : String); virtual;
    Procedure Setwarnings(AIndex : Integer; AValue : TDatafeedStatusTypewarningsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property datafeedId : String Index 0 Read FdatafeedId Write SetdatafeedId;
    Property errors : TDatafeedStatusTypeerrorsArray Index 8 Read Ferrors Write Seterrors;
    Property itemsTotal : String Index 16 Read FitemsTotal Write SetitemsTotal;
    Property itemsValid : String Index 24 Read FitemsValid Write SetitemsValid;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property lastUploadDate : String Index 40 Read FlastUploadDate Write SetlastUploadDate;
    Property processingStatus : String Index 48 Read FprocessingStatus Write SetprocessingStatus;
    Property warnings : TDatafeedStatusTypewarningsArray Index 56 Read Fwarnings Write Setwarnings;
  end;
  TDatafeedStatusClass = Class of TDatafeedStatus;
  
  { --------------------------------------------------------------------
    TDatafeedStatusError
    --------------------------------------------------------------------}
  
  TDatafeedStatusError = Class(TGoogleBaseObject)
  Private
    Fcode : String;
    Fcount : String;
    Fexamples : TDatafeedStatusErrorTypeexamplesArray;
    Fmessage : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : String); virtual;
    Procedure Setcount(AIndex : Integer; AValue : String); virtual;
    Procedure Setexamples(AIndex : Integer; AValue : TDatafeedStatusErrorTypeexamplesArray); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property code : String Index 0 Read Fcode Write Setcode;
    Property count : String Index 8 Read Fcount Write Setcount;
    Property examples : TDatafeedStatusErrorTypeexamplesArray Index 16 Read Fexamples Write Setexamples;
    Property message : String Index 24 Read Fmessage Write Setmessage;
  end;
  TDatafeedStatusErrorClass = Class of TDatafeedStatusError;
  
  { --------------------------------------------------------------------
    TDatafeedStatusExample
    --------------------------------------------------------------------}
  
  TDatafeedStatusExample = Class(TGoogleBaseObject)
  Private
    FitemId : String;
    FlineNumber : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure SetitemId(AIndex : Integer; AValue : String); virtual;
    Procedure SetlineNumber(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property itemId : String Index 0 Read FitemId Write SetitemId;
    Property lineNumber : String Index 8 Read FlineNumber Write SetlineNumber;
    Property value : String Index 16 Read Fvalue Write Setvalue;
  end;
  TDatafeedStatusExampleClass = Class of TDatafeedStatusExample;
  
  { --------------------------------------------------------------------
    TDatafeedsCustomBatchRequest
    --------------------------------------------------------------------}
  
  TDatafeedsCustomBatchRequest = Class(TGoogleBaseObject)
  Private
    Fentries : TDatafeedsCustomBatchRequestTypeentriesArray;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TDatafeedsCustomBatchRequestTypeentriesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entries : TDatafeedsCustomBatchRequestTypeentriesArray Index 0 Read Fentries Write Setentries;
  end;
  TDatafeedsCustomBatchRequestClass = Class of TDatafeedsCustomBatchRequest;
  
  { --------------------------------------------------------------------
    TDatafeedsCustomBatchRequestEntry
    --------------------------------------------------------------------}
  
  TDatafeedsCustomBatchRequestEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    Fdatafeed : TDatafeed;
    FdatafeedId : String;
    FmerchantId : String;
    Fmethod : String;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Setdatafeed(AIndex : Integer; AValue : TDatafeed); virtual;
    Procedure SetdatafeedId(AIndex : Integer; AValue : String); virtual;
    Procedure SetmerchantId(AIndex : Integer; AValue : String); virtual;
    Procedure Setmethod(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property datafeed : TDatafeed Index 8 Read Fdatafeed Write Setdatafeed;
    Property datafeedId : String Index 16 Read FdatafeedId Write SetdatafeedId;
    Property merchantId : String Index 24 Read FmerchantId Write SetmerchantId;
    Property method : String Index 32 Read Fmethod Write Setmethod;
  end;
  TDatafeedsCustomBatchRequestEntryClass = Class of TDatafeedsCustomBatchRequestEntry;
  
  { --------------------------------------------------------------------
    TDatafeedsCustomBatchResponse
    --------------------------------------------------------------------}
  
  TDatafeedsCustomBatchResponse = Class(TGoogleBaseObject)
  Private
    Fentries : TDatafeedsCustomBatchResponseTypeentriesArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TDatafeedsCustomBatchResponseTypeentriesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entries : TDatafeedsCustomBatchResponseTypeentriesArray Index 0 Read Fentries Write Setentries;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TDatafeedsCustomBatchResponseClass = Class of TDatafeedsCustomBatchResponse;
  
  { --------------------------------------------------------------------
    TDatafeedsCustomBatchResponseEntry
    --------------------------------------------------------------------}
  
  TDatafeedsCustomBatchResponseEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    Fdatafeed : TDatafeed;
    Ferrors : TErrors;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Setdatafeed(AIndex : Integer; AValue : TDatafeed); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TErrors); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property datafeed : TDatafeed Index 8 Read Fdatafeed Write Setdatafeed;
    Property errors : TErrors Index 16 Read Ferrors Write Seterrors;
  end;
  TDatafeedsCustomBatchResponseEntryClass = Class of TDatafeedsCustomBatchResponseEntry;
  
  { --------------------------------------------------------------------
    TDatafeedsListResponse
    --------------------------------------------------------------------}
  
  TDatafeedsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    Fresources : TDatafeedsListResponseTyperesourcesArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TDatafeedsListResponseTyperesourcesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property resources : TDatafeedsListResponseTyperesourcesArray Index 16 Read Fresources Write Setresources;
  end;
  TDatafeedsListResponseClass = Class of TDatafeedsListResponse;
  
  { --------------------------------------------------------------------
    TDatafeedstatusesCustomBatchRequest
    --------------------------------------------------------------------}
  
  TDatafeedstatusesCustomBatchRequest = Class(TGoogleBaseObject)
  Private
    Fentries : TDatafeedstatusesCustomBatchRequestTypeentriesArray;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TDatafeedstatusesCustomBatchRequestTypeentriesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entries : TDatafeedstatusesCustomBatchRequestTypeentriesArray Index 0 Read Fentries Write Setentries;
  end;
  TDatafeedstatusesCustomBatchRequestClass = Class of TDatafeedstatusesCustomBatchRequest;
  
  { --------------------------------------------------------------------
    TDatafeedstatusesCustomBatchRequestEntry
    --------------------------------------------------------------------}
  
  TDatafeedstatusesCustomBatchRequestEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    FdatafeedId : String;
    FmerchantId : String;
    Fmethod : String;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetdatafeedId(AIndex : Integer; AValue : String); virtual;
    Procedure SetmerchantId(AIndex : Integer; AValue : String); virtual;
    Procedure Setmethod(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property datafeedId : String Index 8 Read FdatafeedId Write SetdatafeedId;
    Property merchantId : String Index 16 Read FmerchantId Write SetmerchantId;
    Property method : String Index 24 Read Fmethod Write Setmethod;
  end;
  TDatafeedstatusesCustomBatchRequestEntryClass = Class of TDatafeedstatusesCustomBatchRequestEntry;
  
  { --------------------------------------------------------------------
    TDatafeedstatusesCustomBatchResponse
    --------------------------------------------------------------------}
  
  TDatafeedstatusesCustomBatchResponse = Class(TGoogleBaseObject)
  Private
    Fentries : TDatafeedstatusesCustomBatchResponseTypeentriesArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TDatafeedstatusesCustomBatchResponseTypeentriesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entries : TDatafeedstatusesCustomBatchResponseTypeentriesArray Index 0 Read Fentries Write Setentries;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TDatafeedstatusesCustomBatchResponseClass = Class of TDatafeedstatusesCustomBatchResponse;
  
  { --------------------------------------------------------------------
    TDatafeedstatusesCustomBatchResponseEntry
    --------------------------------------------------------------------}
  
  TDatafeedstatusesCustomBatchResponseEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    FdatafeedStatus : TDatafeedStatus;
    Ferrors : TErrors;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetdatafeedStatus(AIndex : Integer; AValue : TDatafeedStatus); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TErrors); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property datafeedStatus : TDatafeedStatus Index 8 Read FdatafeedStatus Write SetdatafeedStatus;
    Property errors : TErrors Index 16 Read Ferrors Write Seterrors;
  end;
  TDatafeedstatusesCustomBatchResponseEntryClass = Class of TDatafeedstatusesCustomBatchResponseEntry;
  
  { --------------------------------------------------------------------
    TDatafeedstatusesListResponse
    --------------------------------------------------------------------}
  
  TDatafeedstatusesListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    Fresources : TDatafeedstatusesListResponseTyperesourcesArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TDatafeedstatusesListResponseTyperesourcesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property resources : TDatafeedstatusesListResponseTyperesourcesArray Index 16 Read Fresources Write Setresources;
  end;
  TDatafeedstatusesListResponseClass = Class of TDatafeedstatusesListResponse;
  
  { --------------------------------------------------------------------
    TError
    --------------------------------------------------------------------}
  
  TError = Class(TGoogleBaseObject)
  Private
    Fdomain : String;
    Fmessage : String;
    Freason : String;
  Protected
    //Property setters
    Procedure Setdomain(AIndex : Integer; AValue : String); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : String); virtual;
    Procedure Setreason(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property domain : String Index 0 Read Fdomain Write Setdomain;
    Property message : String Index 8 Read Fmessage Write Setmessage;
    Property reason : String Index 16 Read Freason Write Setreason;
  end;
  TErrorClass = Class of TError;
  
  { --------------------------------------------------------------------
    TErrors
    --------------------------------------------------------------------}
  
  TErrors = Class(TGoogleBaseObject)
  Private
    Fcode : integer;
    Ferrors : TErrorsTypeerrorsArray;
    Fmessage : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : integer); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TErrorsTypeerrorsArray); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property code : integer Index 0 Read Fcode Write Setcode;
    Property errors : TErrorsTypeerrorsArray Index 8 Read Ferrors Write Seterrors;
    Property message : String Index 16 Read Fmessage Write Setmessage;
  end;
  TErrorsClass = Class of TErrors;
  
  { --------------------------------------------------------------------
    TInventory
    --------------------------------------------------------------------}
  
  TInventory = Class(TGoogleBaseObject)
  Private
    Favailability : String;
    Fkind : String;
    Fprice : TPrice;
    Fquantity : integer;
    FsalePrice : TPrice;
    FsalePriceEffectiveDate : String;
  Protected
    //Property setters
    Procedure Setavailability(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setprice(AIndex : Integer; AValue : TPrice); virtual;
    Procedure Setquantity(AIndex : Integer; AValue : integer); virtual;
    Procedure SetsalePrice(AIndex : Integer; AValue : TPrice); virtual;
    Procedure SetsalePriceEffectiveDate(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property availability : String Index 0 Read Favailability Write Setavailability;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property price : TPrice Index 16 Read Fprice Write Setprice;
    Property quantity : integer Index 24 Read Fquantity Write Setquantity;
    Property salePrice : TPrice Index 32 Read FsalePrice Write SetsalePrice;
    Property salePriceEffectiveDate : String Index 40 Read FsalePriceEffectiveDate Write SetsalePriceEffectiveDate;
  end;
  TInventoryClass = Class of TInventory;
  
  { --------------------------------------------------------------------
    TInventoryCustomBatchRequest
    --------------------------------------------------------------------}
  
  TInventoryCustomBatchRequest = Class(TGoogleBaseObject)
  Private
    Fentries : TInventoryCustomBatchRequestTypeentriesArray;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TInventoryCustomBatchRequestTypeentriesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entries : TInventoryCustomBatchRequestTypeentriesArray Index 0 Read Fentries Write Setentries;
  end;
  TInventoryCustomBatchRequestClass = Class of TInventoryCustomBatchRequest;
  
  { --------------------------------------------------------------------
    TInventoryCustomBatchRequestEntry
    --------------------------------------------------------------------}
  
  TInventoryCustomBatchRequestEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    Finventory : TInventory;
    FmerchantId : String;
    FproductId : String;
    FstoreCode : String;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Setinventory(AIndex : Integer; AValue : TInventory); virtual;
    Procedure SetmerchantId(AIndex : Integer; AValue : String); virtual;
    Procedure SetproductId(AIndex : Integer; AValue : String); virtual;
    Procedure SetstoreCode(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property inventory : TInventory Index 8 Read Finventory Write Setinventory;
    Property merchantId : String Index 16 Read FmerchantId Write SetmerchantId;
    Property productId : String Index 24 Read FproductId Write SetproductId;
    Property storeCode : String Index 32 Read FstoreCode Write SetstoreCode;
  end;
  TInventoryCustomBatchRequestEntryClass = Class of TInventoryCustomBatchRequestEntry;
  
  { --------------------------------------------------------------------
    TInventoryCustomBatchResponse
    --------------------------------------------------------------------}
  
  TInventoryCustomBatchResponse = Class(TGoogleBaseObject)
  Private
    Fentries : TInventoryCustomBatchResponseTypeentriesArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TInventoryCustomBatchResponseTypeentriesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entries : TInventoryCustomBatchResponseTypeentriesArray Index 0 Read Fentries Write Setentries;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TInventoryCustomBatchResponseClass = Class of TInventoryCustomBatchResponse;
  
  { --------------------------------------------------------------------
    TInventoryCustomBatchResponseEntry
    --------------------------------------------------------------------}
  
  TInventoryCustomBatchResponseEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    Ferrors : TErrors;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TErrors); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property errors : TErrors Index 8 Read Ferrors Write Seterrors;
    Property kind : String Index 16 Read Fkind Write Setkind;
  end;
  TInventoryCustomBatchResponseEntryClass = Class of TInventoryCustomBatchResponseEntry;
  
  { --------------------------------------------------------------------
    TInventorySetRequest
    --------------------------------------------------------------------}
  
  TInventorySetRequest = Class(TGoogleBaseObject)
  Private
    Favailability : String;
    Fprice : TPrice;
    Fquantity : integer;
    FsalePrice : TPrice;
    FsalePriceEffectiveDate : String;
  Protected
    //Property setters
    Procedure Setavailability(AIndex : Integer; AValue : String); virtual;
    Procedure Setprice(AIndex : Integer; AValue : TPrice); virtual;
    Procedure Setquantity(AIndex : Integer; AValue : integer); virtual;
    Procedure SetsalePrice(AIndex : Integer; AValue : TPrice); virtual;
    Procedure SetsalePriceEffectiveDate(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property availability : String Index 0 Read Favailability Write Setavailability;
    Property price : TPrice Index 8 Read Fprice Write Setprice;
    Property quantity : integer Index 16 Read Fquantity Write Setquantity;
    Property salePrice : TPrice Index 24 Read FsalePrice Write SetsalePrice;
    Property salePriceEffectiveDate : String Index 32 Read FsalePriceEffectiveDate Write SetsalePriceEffectiveDate;
  end;
  TInventorySetRequestClass = Class of TInventorySetRequest;
  
  { --------------------------------------------------------------------
    TInventorySetResponse
    --------------------------------------------------------------------}
  
  TInventorySetResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
  end;
  TInventorySetResponseClass = Class of TInventorySetResponse;
  
  { --------------------------------------------------------------------
    TLoyaltyPoints
    --------------------------------------------------------------------}
  
  TLoyaltyPoints = Class(TGoogleBaseObject)
  Private
    Fname : String;
    FpointsValue : String;
    Fratio : double;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetpointsValue(AIndex : Integer; AValue : String); virtual;
    Procedure Setratio(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property pointsValue : String Index 8 Read FpointsValue Write SetpointsValue;
    Property ratio : double Index 16 Read Fratio Write Setratio;
  end;
  TLoyaltyPointsClass = Class of TLoyaltyPoints;
  
  { --------------------------------------------------------------------
    TPrice
    --------------------------------------------------------------------}
  
  TPrice = Class(TGoogleBaseObject)
  Private
    Fcurrency : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setcurrency(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property currency : String Index 0 Read Fcurrency Write Setcurrency;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TPriceClass = Class of TPrice;
  
  { --------------------------------------------------------------------
    TProduct
    --------------------------------------------------------------------}
  
  TProduct = Class(TGoogleBaseObject)
  Private
    FadditionalImageLinks : TStringArray;
    Fadult : boolean;
    FadwordsGrouping : String;
    FadwordsLabels : TStringArray;
    FadwordsRedirect : String;
    FageGroup : String;
    Faspects : TProductTypeaspectsArray;
    Favailability : String;
    FavailabilityDate : String;
    Fbrand : String;
    Fchannel : String;
    Fcolor : String;
    Fcondition : String;
    FcontentLanguage : String;
    FcustomAttributes : TProductTypecustomAttributesArray;
    FcustomGroups : TProductTypecustomGroupsArray;
    FcustomLabel0 : String;
    FcustomLabel1 : String;
    FcustomLabel2 : String;
    FcustomLabel3 : String;
    FcustomLabel4 : String;
    Fdescription : String;
    Fdestinations : TProductTypedestinationsArray;
    FdisplayAdsId : String;
    FdisplayAdsLink : String;
    FdisplayAdsSimilarIds : TStringArray;
    FdisplayAdsTitle : String;
    FdisplayAdsValue : double;
    FenergyEfficiencyClass : String;
    FexpirationDate : String;
    Fgender : String;
    FgoogleProductCategory : String;
    Fgtin : String;
    Fid : String;
    FidentifierExists : boolean;
    FimageLink : String;
    Finstallment : TProductInstallment;
    FisBundle : boolean;
    FitemGroupId : String;
    Fkind : String;
    Flink : String;
    FloyaltyPoints : TLoyaltyPoints;
    Fmaterial : String;
    FmobileLink : String;
    Fmpn : String;
    Fmultipack : String;
    FofferId : String;
    FonlineOnly : boolean;
    Fpattern : String;
    Fprice : TPrice;
    FproductType : String;
    FsalePrice : TPrice;
    FsalePriceEffectiveDate : String;
    Fshipping : TProductTypeshippingArray;
    FshippingHeight : TProductShippingDimension;
    FshippingLabel : String;
    FshippingLength : TProductShippingDimension;
    FshippingWeight : TProductShippingWeight;
    FshippingWidth : TProductShippingDimension;
    FsizeSystem : String;
    FsizeType : String;
    Fsizes : TStringArray;
    FtargetCountry : String;
    Ftaxes : TProductTypetaxesArray;
    Ftitle : String;
    FunitPricingBaseMeasure : TProductUnitPricingBaseMeasure;
    FunitPricingMeasure : TProductUnitPricingMeasure;
    FvalidatedDestinations : TStringArray;
    Fwarnings : TProductTypewarningsArray;
  Protected
    //Property setters
    Procedure SetadditionalImageLinks(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setadult(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetadwordsGrouping(AIndex : Integer; AValue : String); virtual;
    Procedure SetadwordsLabels(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetadwordsRedirect(AIndex : Integer; AValue : String); virtual;
    Procedure SetageGroup(AIndex : Integer; AValue : String); virtual;
    Procedure Setaspects(AIndex : Integer; AValue : TProductTypeaspectsArray); virtual;
    Procedure Setavailability(AIndex : Integer; AValue : String); virtual;
    Procedure SetavailabilityDate(AIndex : Integer; AValue : String); virtual;
    Procedure Setbrand(AIndex : Integer; AValue : String); virtual;
    Procedure Setchannel(AIndex : Integer; AValue : String); virtual;
    Procedure Setcolor(AIndex : Integer; AValue : String); virtual;
    Procedure Setcondition(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontentLanguage(AIndex : Integer; AValue : String); virtual;
    Procedure SetcustomAttributes(AIndex : Integer; AValue : TProductTypecustomAttributesArray); virtual;
    Procedure SetcustomGroups(AIndex : Integer; AValue : TProductTypecustomGroupsArray); virtual;
    Procedure SetcustomLabel0(AIndex : Integer; AValue : String); virtual;
    Procedure SetcustomLabel1(AIndex : Integer; AValue : String); virtual;
    Procedure SetcustomLabel2(AIndex : Integer; AValue : String); virtual;
    Procedure SetcustomLabel3(AIndex : Integer; AValue : String); virtual;
    Procedure SetcustomLabel4(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setdestinations(AIndex : Integer; AValue : TProductTypedestinationsArray); virtual;
    Procedure SetdisplayAdsId(AIndex : Integer; AValue : String); virtual;
    Procedure SetdisplayAdsLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetdisplayAdsSimilarIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetdisplayAdsTitle(AIndex : Integer; AValue : String); virtual;
    Procedure SetdisplayAdsValue(AIndex : Integer; AValue : double); virtual;
    Procedure SetenergyEfficiencyClass(AIndex : Integer; AValue : String); virtual;
    Procedure SetexpirationDate(AIndex : Integer; AValue : String); virtual;
    Procedure Setgender(AIndex : Integer; AValue : String); virtual;
    Procedure SetgoogleProductCategory(AIndex : Integer; AValue : String); virtual;
    Procedure Setgtin(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetidentifierExists(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetimageLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setinstallment(AIndex : Integer; AValue : TProductInstallment); virtual;
    Procedure SetisBundle(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetitemGroupId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlink(AIndex : Integer; AValue : String); virtual;
    Procedure SetloyaltyPoints(AIndex : Integer; AValue : TLoyaltyPoints); virtual;
    Procedure Setmaterial(AIndex : Integer; AValue : String); virtual;
    Procedure SetmobileLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setmpn(AIndex : Integer; AValue : String); virtual;
    Procedure Setmultipack(AIndex : Integer; AValue : String); virtual;
    Procedure SetofferId(AIndex : Integer; AValue : String); virtual;
    Procedure SetonlineOnly(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setpattern(AIndex : Integer; AValue : String); virtual;
    Procedure Setprice(AIndex : Integer; AValue : TPrice); virtual;
    Procedure SetproductType(AIndex : Integer; AValue : String); virtual;
    Procedure SetsalePrice(AIndex : Integer; AValue : TPrice); virtual;
    Procedure SetsalePriceEffectiveDate(AIndex : Integer; AValue : String); virtual;
    Procedure Setshipping(AIndex : Integer; AValue : TProductTypeshippingArray); virtual;
    Procedure SetshippingHeight(AIndex : Integer; AValue : TProductShippingDimension); virtual;
    Procedure SetshippingLabel(AIndex : Integer; AValue : String); virtual;
    Procedure SetshippingLength(AIndex : Integer; AValue : TProductShippingDimension); virtual;
    Procedure SetshippingWeight(AIndex : Integer; AValue : TProductShippingWeight); virtual;
    Procedure SetshippingWidth(AIndex : Integer; AValue : TProductShippingDimension); virtual;
    Procedure SetsizeSystem(AIndex : Integer; AValue : String); virtual;
    Procedure SetsizeType(AIndex : Integer; AValue : String); virtual;
    Procedure Setsizes(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SettargetCountry(AIndex : Integer; AValue : String); virtual;
    Procedure Settaxes(AIndex : Integer; AValue : TProductTypetaxesArray); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure SetunitPricingBaseMeasure(AIndex : Integer; AValue : TProductUnitPricingBaseMeasure); virtual;
    Procedure SetunitPricingMeasure(AIndex : Integer; AValue : TProductUnitPricingMeasure); virtual;
    Procedure SetvalidatedDestinations(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setwarnings(AIndex : Integer; AValue : TProductTypewarningsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property additionalImageLinks : TStringArray Index 0 Read FadditionalImageLinks Write SetadditionalImageLinks;
    Property adult : boolean Index 8 Read Fadult Write Setadult;
    Property adwordsGrouping : String Index 16 Read FadwordsGrouping Write SetadwordsGrouping;
    Property adwordsLabels : TStringArray Index 24 Read FadwordsLabels Write SetadwordsLabels;
    Property adwordsRedirect : String Index 32 Read FadwordsRedirect Write SetadwordsRedirect;
    Property ageGroup : String Index 40 Read FageGroup Write SetageGroup;
    Property aspects : TProductTypeaspectsArray Index 48 Read Faspects Write Setaspects;
    Property availability : String Index 56 Read Favailability Write Setavailability;
    Property availabilityDate : String Index 64 Read FavailabilityDate Write SetavailabilityDate;
    Property brand : String Index 72 Read Fbrand Write Setbrand;
    Property channel : String Index 80 Read Fchannel Write Setchannel;
    Property color : String Index 88 Read Fcolor Write Setcolor;
    Property condition : String Index 96 Read Fcondition Write Setcondition;
    Property contentLanguage : String Index 104 Read FcontentLanguage Write SetcontentLanguage;
    Property customAttributes : TProductTypecustomAttributesArray Index 112 Read FcustomAttributes Write SetcustomAttributes;
    Property customGroups : TProductTypecustomGroupsArray Index 120 Read FcustomGroups Write SetcustomGroups;
    Property customLabel0 : String Index 128 Read FcustomLabel0 Write SetcustomLabel0;
    Property customLabel1 : String Index 136 Read FcustomLabel1 Write SetcustomLabel1;
    Property customLabel2 : String Index 144 Read FcustomLabel2 Write SetcustomLabel2;
    Property customLabel3 : String Index 152 Read FcustomLabel3 Write SetcustomLabel3;
    Property customLabel4 : String Index 160 Read FcustomLabel4 Write SetcustomLabel4;
    Property description : String Index 168 Read Fdescription Write Setdescription;
    Property destinations : TProductTypedestinationsArray Index 176 Read Fdestinations Write Setdestinations;
    Property displayAdsId : String Index 184 Read FdisplayAdsId Write SetdisplayAdsId;
    Property displayAdsLink : String Index 192 Read FdisplayAdsLink Write SetdisplayAdsLink;
    Property displayAdsSimilarIds : TStringArray Index 200 Read FdisplayAdsSimilarIds Write SetdisplayAdsSimilarIds;
    Property displayAdsTitle : String Index 208 Read FdisplayAdsTitle Write SetdisplayAdsTitle;
    Property displayAdsValue : double Index 216 Read FdisplayAdsValue Write SetdisplayAdsValue;
    Property energyEfficiencyClass : String Index 224 Read FenergyEfficiencyClass Write SetenergyEfficiencyClass;
    Property expirationDate : String Index 232 Read FexpirationDate Write SetexpirationDate;
    Property gender : String Index 240 Read Fgender Write Setgender;
    Property googleProductCategory : String Index 248 Read FgoogleProductCategory Write SetgoogleProductCategory;
    Property gtin : String Index 256 Read Fgtin Write Setgtin;
    Property id : String Index 264 Read Fid Write Setid;
    Property identifierExists : boolean Index 272 Read FidentifierExists Write SetidentifierExists;
    Property imageLink : String Index 280 Read FimageLink Write SetimageLink;
    Property installment : TProductInstallment Index 288 Read Finstallment Write Setinstallment;
    Property isBundle : boolean Index 296 Read FisBundle Write SetisBundle;
    Property itemGroupId : String Index 304 Read FitemGroupId Write SetitemGroupId;
    Property kind : String Index 312 Read Fkind Write Setkind;
    Property link : String Index 320 Read Flink Write Setlink;
    Property loyaltyPoints : TLoyaltyPoints Index 328 Read FloyaltyPoints Write SetloyaltyPoints;
    Property material : String Index 336 Read Fmaterial Write Setmaterial;
    Property mobileLink : String Index 344 Read FmobileLink Write SetmobileLink;
    Property mpn : String Index 352 Read Fmpn Write Setmpn;
    Property multipack : String Index 360 Read Fmultipack Write Setmultipack;
    Property offerId : String Index 368 Read FofferId Write SetofferId;
    Property onlineOnly : boolean Index 376 Read FonlineOnly Write SetonlineOnly;
    Property pattern : String Index 384 Read Fpattern Write Setpattern;
    Property price : TPrice Index 392 Read Fprice Write Setprice;
    Property productType : String Index 400 Read FproductType Write SetproductType;
    Property salePrice : TPrice Index 408 Read FsalePrice Write SetsalePrice;
    Property salePriceEffectiveDate : String Index 416 Read FsalePriceEffectiveDate Write SetsalePriceEffectiveDate;
    Property shipping : TProductTypeshippingArray Index 424 Read Fshipping Write Setshipping;
    Property shippingHeight : TProductShippingDimension Index 432 Read FshippingHeight Write SetshippingHeight;
    Property shippingLabel : String Index 440 Read FshippingLabel Write SetshippingLabel;
    Property shippingLength : TProductShippingDimension Index 448 Read FshippingLength Write SetshippingLength;
    Property shippingWeight : TProductShippingWeight Index 456 Read FshippingWeight Write SetshippingWeight;
    Property shippingWidth : TProductShippingDimension Index 464 Read FshippingWidth Write SetshippingWidth;
    Property sizeSystem : String Index 472 Read FsizeSystem Write SetsizeSystem;
    Property sizeType : String Index 480 Read FsizeType Write SetsizeType;
    Property sizes : TStringArray Index 488 Read Fsizes Write Setsizes;
    Property targetCountry : String Index 496 Read FtargetCountry Write SettargetCountry;
    Property taxes : TProductTypetaxesArray Index 504 Read Ftaxes Write Settaxes;
    Property title : String Index 512 Read Ftitle Write Settitle;
    Property unitPricingBaseMeasure : TProductUnitPricingBaseMeasure Index 520 Read FunitPricingBaseMeasure Write SetunitPricingBaseMeasure;
    Property unitPricingMeasure : TProductUnitPricingMeasure Index 528 Read FunitPricingMeasure Write SetunitPricingMeasure;
    Property validatedDestinations : TStringArray Index 536 Read FvalidatedDestinations Write SetvalidatedDestinations;
    Property warnings : TProductTypewarningsArray Index 544 Read Fwarnings Write Setwarnings;
  end;
  TProductClass = Class of TProduct;
  
  { --------------------------------------------------------------------
    TProductAspect
    --------------------------------------------------------------------}
  
  TProductAspect = Class(TGoogleBaseObject)
  Private
    FaspectName : String;
    FdestinationName : String;
    Fintention : String;
  Protected
    //Property setters
    Procedure SetaspectName(AIndex : Integer; AValue : String); virtual;
    Procedure SetdestinationName(AIndex : Integer; AValue : String); virtual;
    Procedure Setintention(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property aspectName : String Index 0 Read FaspectName Write SetaspectName;
    Property destinationName : String Index 8 Read FdestinationName Write SetdestinationName;
    Property intention : String Index 16 Read Fintention Write Setintention;
  end;
  TProductAspectClass = Class of TProductAspect;
  
  { --------------------------------------------------------------------
    TProductCustomAttribute
    --------------------------------------------------------------------}
  
  TProductCustomAttribute = Class(TGoogleBaseObject)
  Private
    Fname : String;
    F_type : String;
    F_unit : String;
    Fvalue : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Set_unit(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property _type : String Index 8 Read F_type Write Set_type;
    Property _unit : String Index 16 Read F_unit Write Set_unit;
    Property value : String Index 24 Read Fvalue Write Setvalue;
  end;
  TProductCustomAttributeClass = Class of TProductCustomAttribute;
  
  { --------------------------------------------------------------------
    TProductCustomGroup
    --------------------------------------------------------------------}
  
  TProductCustomGroup = Class(TGoogleBaseObject)
  Private
    Fattributes : TProductCustomGroupTypeattributesArray;
    Fname : String;
  Protected
    //Property setters
    Procedure Setattributes(AIndex : Integer; AValue : TProductCustomGroupTypeattributesArray); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property attributes : TProductCustomGroupTypeattributesArray Index 0 Read Fattributes Write Setattributes;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TProductCustomGroupClass = Class of TProductCustomGroup;
  
  { --------------------------------------------------------------------
    TProductDestination
    --------------------------------------------------------------------}
  
  TProductDestination = Class(TGoogleBaseObject)
  Private
    FdestinationName : String;
    Fintention : String;
  Protected
    //Property setters
    Procedure SetdestinationName(AIndex : Integer; AValue : String); virtual;
    Procedure Setintention(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property destinationName : String Index 0 Read FdestinationName Write SetdestinationName;
    Property intention : String Index 8 Read Fintention Write Setintention;
  end;
  TProductDestinationClass = Class of TProductDestination;
  
  { --------------------------------------------------------------------
    TProductInstallment
    --------------------------------------------------------------------}
  
  TProductInstallment = Class(TGoogleBaseObject)
  Private
    Famount : TPrice;
    Fmonths : String;
  Protected
    //Property setters
    Procedure Setamount(AIndex : Integer; AValue : TPrice); virtual;
    Procedure Setmonths(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property amount : TPrice Index 0 Read Famount Write Setamount;
    Property months : String Index 8 Read Fmonths Write Setmonths;
  end;
  TProductInstallmentClass = Class of TProductInstallment;
  
  { --------------------------------------------------------------------
    TProductShipping
    --------------------------------------------------------------------}
  
  TProductShipping = Class(TGoogleBaseObject)
  Private
    Fcountry : String;
    FlocationGroupName : String;
    FlocationId : String;
    FpostalCode : String;
    Fprice : TPrice;
    Fregion : String;
    Fservice : String;
  Protected
    //Property setters
    Procedure Setcountry(AIndex : Integer; AValue : String); virtual;
    Procedure SetlocationGroupName(AIndex : Integer; AValue : String); virtual;
    Procedure SetlocationId(AIndex : Integer; AValue : String); virtual;
    Procedure SetpostalCode(AIndex : Integer; AValue : String); virtual;
    Procedure Setprice(AIndex : Integer; AValue : TPrice); virtual;
    Procedure Setregion(AIndex : Integer; AValue : String); virtual;
    Procedure Setservice(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property country : String Index 0 Read Fcountry Write Setcountry;
    Property locationGroupName : String Index 8 Read FlocationGroupName Write SetlocationGroupName;
    Property locationId : String Index 16 Read FlocationId Write SetlocationId;
    Property postalCode : String Index 24 Read FpostalCode Write SetpostalCode;
    Property price : TPrice Index 32 Read Fprice Write Setprice;
    Property region : String Index 40 Read Fregion Write Setregion;
    Property service : String Index 48 Read Fservice Write Setservice;
  end;
  TProductShippingClass = Class of TProductShipping;
  
  { --------------------------------------------------------------------
    TProductShippingDimension
    --------------------------------------------------------------------}
  
  TProductShippingDimension = Class(TGoogleBaseObject)
  Private
    F_unit : String;
    Fvalue : double;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_unit(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property _unit : String Index 0 Read F_unit Write Set_unit;
    Property value : double Index 8 Read Fvalue Write Setvalue;
  end;
  TProductShippingDimensionClass = Class of TProductShippingDimension;
  
  { --------------------------------------------------------------------
    TProductShippingWeight
    --------------------------------------------------------------------}
  
  TProductShippingWeight = Class(TGoogleBaseObject)
  Private
    F_unit : String;
    Fvalue : double;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_unit(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property _unit : String Index 0 Read F_unit Write Set_unit;
    Property value : double Index 8 Read Fvalue Write Setvalue;
  end;
  TProductShippingWeightClass = Class of TProductShippingWeight;
  
  { --------------------------------------------------------------------
    TProductStatus
    --------------------------------------------------------------------}
  
  TProductStatus = Class(TGoogleBaseObject)
  Private
    FcreationDate : String;
    FdataQualityIssues : TProductStatusTypedataQualityIssuesArray;
    FdestinationStatuses : TProductStatusTypedestinationStatusesArray;
    FgoogleExpirationDate : String;
    Fkind : String;
    FlastUpdateDate : String;
    Flink : String;
    FproductId : String;
    Ftitle : String;
  Protected
    //Property setters
    Procedure SetcreationDate(AIndex : Integer; AValue : String); virtual;
    Procedure SetdataQualityIssues(AIndex : Integer; AValue : TProductStatusTypedataQualityIssuesArray); virtual;
    Procedure SetdestinationStatuses(AIndex : Integer; AValue : TProductStatusTypedestinationStatusesArray); virtual;
    Procedure SetgoogleExpirationDate(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastUpdateDate(AIndex : Integer; AValue : String); virtual;
    Procedure Setlink(AIndex : Integer; AValue : String); virtual;
    Procedure SetproductId(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property creationDate : String Index 0 Read FcreationDate Write SetcreationDate;
    Property dataQualityIssues : TProductStatusTypedataQualityIssuesArray Index 8 Read FdataQualityIssues Write SetdataQualityIssues;
    Property destinationStatuses : TProductStatusTypedestinationStatusesArray Index 16 Read FdestinationStatuses Write SetdestinationStatuses;
    Property googleExpirationDate : String Index 24 Read FgoogleExpirationDate Write SetgoogleExpirationDate;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property lastUpdateDate : String Index 40 Read FlastUpdateDate Write SetlastUpdateDate;
    Property link : String Index 48 Read Flink Write Setlink;
    Property productId : String Index 56 Read FproductId Write SetproductId;
    Property title : String Index 64 Read Ftitle Write Settitle;
  end;
  TProductStatusClass = Class of TProductStatus;
  
  { --------------------------------------------------------------------
    TProductStatusDataQualityIssue
    --------------------------------------------------------------------}
  
  TProductStatusDataQualityIssue = Class(TGoogleBaseObject)
  Private
    Fdetail : String;
    FfetchStatus : String;
    Fid : String;
    Flocation : String;
    Fseverity : String;
    Ftimestamp : String;
    FvalueOnLandingPage : String;
    FvalueProvided : String;
  Protected
    //Property setters
    Procedure Setdetail(AIndex : Integer; AValue : String); virtual;
    Procedure SetfetchStatus(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : String); virtual;
    Procedure Setseverity(AIndex : Integer; AValue : String); virtual;
    Procedure Settimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure SetvalueOnLandingPage(AIndex : Integer; AValue : String); virtual;
    Procedure SetvalueProvided(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property detail : String Index 0 Read Fdetail Write Setdetail;
    Property fetchStatus : String Index 8 Read FfetchStatus Write SetfetchStatus;
    Property id : String Index 16 Read Fid Write Setid;
    Property location : String Index 24 Read Flocation Write Setlocation;
    Property severity : String Index 32 Read Fseverity Write Setseverity;
    Property timestamp : String Index 40 Read Ftimestamp Write Settimestamp;
    Property valueOnLandingPage : String Index 48 Read FvalueOnLandingPage Write SetvalueOnLandingPage;
    Property valueProvided : String Index 56 Read FvalueProvided Write SetvalueProvided;
  end;
  TProductStatusDataQualityIssueClass = Class of TProductStatusDataQualityIssue;
  
  { --------------------------------------------------------------------
    TProductStatusDestinationStatus
    --------------------------------------------------------------------}
  
  TProductStatusDestinationStatus = Class(TGoogleBaseObject)
  Private
    FapprovalStatus : String;
    Fdestination : String;
    Fintention : String;
  Protected
    //Property setters
    Procedure SetapprovalStatus(AIndex : Integer; AValue : String); virtual;
    Procedure Setdestination(AIndex : Integer; AValue : String); virtual;
    Procedure Setintention(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property approvalStatus : String Index 0 Read FapprovalStatus Write SetapprovalStatus;
    Property destination : String Index 8 Read Fdestination Write Setdestination;
    Property intention : String Index 16 Read Fintention Write Setintention;
  end;
  TProductStatusDestinationStatusClass = Class of TProductStatusDestinationStatus;
  
  { --------------------------------------------------------------------
    TProductTax
    --------------------------------------------------------------------}
  
  TProductTax = Class(TGoogleBaseObject)
  Private
    Fcountry : String;
    FlocationId : String;
    FpostalCode : String;
    Frate : double;
    Fregion : String;
    FtaxShip : boolean;
  Protected
    //Property setters
    Procedure Setcountry(AIndex : Integer; AValue : String); virtual;
    Procedure SetlocationId(AIndex : Integer; AValue : String); virtual;
    Procedure SetpostalCode(AIndex : Integer; AValue : String); virtual;
    Procedure Setrate(AIndex : Integer; AValue : double); virtual;
    Procedure Setregion(AIndex : Integer; AValue : String); virtual;
    Procedure SettaxShip(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property country : String Index 0 Read Fcountry Write Setcountry;
    Property locationId : String Index 8 Read FlocationId Write SetlocationId;
    Property postalCode : String Index 16 Read FpostalCode Write SetpostalCode;
    Property rate : double Index 24 Read Frate Write Setrate;
    Property region : String Index 32 Read Fregion Write Setregion;
    Property taxShip : boolean Index 40 Read FtaxShip Write SettaxShip;
  end;
  TProductTaxClass = Class of TProductTax;
  
  { --------------------------------------------------------------------
    TProductUnitPricingBaseMeasure
    --------------------------------------------------------------------}
  
  TProductUnitPricingBaseMeasure = Class(TGoogleBaseObject)
  Private
    F_unit : String;
    Fvalue : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_unit(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property _unit : String Index 0 Read F_unit Write Set_unit;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TProductUnitPricingBaseMeasureClass = Class of TProductUnitPricingBaseMeasure;
  
  { --------------------------------------------------------------------
    TProductUnitPricingMeasure
    --------------------------------------------------------------------}
  
  TProductUnitPricingMeasure = Class(TGoogleBaseObject)
  Private
    F_unit : String;
    Fvalue : double;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_unit(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property _unit : String Index 0 Read F_unit Write Set_unit;
    Property value : double Index 8 Read Fvalue Write Setvalue;
  end;
  TProductUnitPricingMeasureClass = Class of TProductUnitPricingMeasure;
  
  { --------------------------------------------------------------------
    TProductsCustomBatchRequest
    --------------------------------------------------------------------}
  
  TProductsCustomBatchRequest = Class(TGoogleBaseObject)
  Private
    Fentries : TProductsCustomBatchRequestTypeentriesArray;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TProductsCustomBatchRequestTypeentriesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entries : TProductsCustomBatchRequestTypeentriesArray Index 0 Read Fentries Write Setentries;
  end;
  TProductsCustomBatchRequestClass = Class of TProductsCustomBatchRequest;
  
  { --------------------------------------------------------------------
    TProductsCustomBatchRequestEntry
    --------------------------------------------------------------------}
  
  TProductsCustomBatchRequestEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    FmerchantId : String;
    Fmethod : String;
    Fproduct : TProduct;
    FproductId : String;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmerchantId(AIndex : Integer; AValue : String); virtual;
    Procedure Setmethod(AIndex : Integer; AValue : String); virtual;
    Procedure Setproduct(AIndex : Integer; AValue : TProduct); virtual;
    Procedure SetproductId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property merchantId : String Index 8 Read FmerchantId Write SetmerchantId;
    Property method : String Index 16 Read Fmethod Write Setmethod;
    Property product : TProduct Index 24 Read Fproduct Write Setproduct;
    Property productId : String Index 32 Read FproductId Write SetproductId;
  end;
  TProductsCustomBatchRequestEntryClass = Class of TProductsCustomBatchRequestEntry;
  
  { --------------------------------------------------------------------
    TProductsCustomBatchResponse
    --------------------------------------------------------------------}
  
  TProductsCustomBatchResponse = Class(TGoogleBaseObject)
  Private
    Fentries : TProductsCustomBatchResponseTypeentriesArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TProductsCustomBatchResponseTypeentriesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entries : TProductsCustomBatchResponseTypeentriesArray Index 0 Read Fentries Write Setentries;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TProductsCustomBatchResponseClass = Class of TProductsCustomBatchResponse;
  
  { --------------------------------------------------------------------
    TProductsCustomBatchResponseEntry
    --------------------------------------------------------------------}
  
  TProductsCustomBatchResponseEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    Ferrors : TErrors;
    Fkind : String;
    Fproduct : TProduct;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TErrors); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setproduct(AIndex : Integer; AValue : TProduct); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property errors : TErrors Index 8 Read Ferrors Write Seterrors;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property product : TProduct Index 24 Read Fproduct Write Setproduct;
  end;
  TProductsCustomBatchResponseEntryClass = Class of TProductsCustomBatchResponseEntry;
  
  { --------------------------------------------------------------------
    TProductsListResponse
    --------------------------------------------------------------------}
  
  TProductsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    Fresources : TProductsListResponseTyperesourcesArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TProductsListResponseTyperesourcesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property resources : TProductsListResponseTyperesourcesArray Index 16 Read Fresources Write Setresources;
  end;
  TProductsListResponseClass = Class of TProductsListResponse;
  
  { --------------------------------------------------------------------
    TProductstatusesCustomBatchRequest
    --------------------------------------------------------------------}
  
  TProductstatusesCustomBatchRequest = Class(TGoogleBaseObject)
  Private
    Fentries : TProductstatusesCustomBatchRequestTypeentriesArray;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TProductstatusesCustomBatchRequestTypeentriesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entries : TProductstatusesCustomBatchRequestTypeentriesArray Index 0 Read Fentries Write Setentries;
  end;
  TProductstatusesCustomBatchRequestClass = Class of TProductstatusesCustomBatchRequest;
  
  { --------------------------------------------------------------------
    TProductstatusesCustomBatchRequestEntry
    --------------------------------------------------------------------}
  
  TProductstatusesCustomBatchRequestEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    FmerchantId : String;
    Fmethod : String;
    FproductId : String;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmerchantId(AIndex : Integer; AValue : String); virtual;
    Procedure Setmethod(AIndex : Integer; AValue : String); virtual;
    Procedure SetproductId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property merchantId : String Index 8 Read FmerchantId Write SetmerchantId;
    Property method : String Index 16 Read Fmethod Write Setmethod;
    Property productId : String Index 24 Read FproductId Write SetproductId;
  end;
  TProductstatusesCustomBatchRequestEntryClass = Class of TProductstatusesCustomBatchRequestEntry;
  
  { --------------------------------------------------------------------
    TProductstatusesCustomBatchResponse
    --------------------------------------------------------------------}
  
  TProductstatusesCustomBatchResponse = Class(TGoogleBaseObject)
  Private
    Fentries : TProductstatusesCustomBatchResponseTypeentriesArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TProductstatusesCustomBatchResponseTypeentriesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entries : TProductstatusesCustomBatchResponseTypeentriesArray Index 0 Read Fentries Write Setentries;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TProductstatusesCustomBatchResponseClass = Class of TProductstatusesCustomBatchResponse;
  
  { --------------------------------------------------------------------
    TProductstatusesCustomBatchResponseEntry
    --------------------------------------------------------------------}
  
  TProductstatusesCustomBatchResponseEntry = Class(TGoogleBaseObject)
  Private
    FbatchId : integer;
    Ferrors : TErrors;
    Fkind : String;
    FproductStatus : TProductStatus;
  Protected
    //Property setters
    Procedure SetbatchId(AIndex : Integer; AValue : integer); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TErrors); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetproductStatus(AIndex : Integer; AValue : TProductStatus); virtual;
  Public
  Published
    Property batchId : integer Index 0 Read FbatchId Write SetbatchId;
    Property errors : TErrors Index 8 Read Ferrors Write Seterrors;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property productStatus : TProductStatus Index 24 Read FproductStatus Write SetproductStatus;
  end;
  TProductstatusesCustomBatchResponseEntryClass = Class of TProductstatusesCustomBatchResponseEntry;
  
  { --------------------------------------------------------------------
    TProductstatusesListResponse
    --------------------------------------------------------------------}
  
  TProductstatusesListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    Fresources : TProductstatusesListResponseTyperesourcesArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TProductstatusesListResponseTyperesourcesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property resources : TProductstatusesListResponseTyperesourcesArray Index 16 Read Fresources Write Setresources;
  end;
  TProductstatusesListResponseClass = Class of TProductstatusesListResponse;
  
  { --------------------------------------------------------------------
    TWeight
    --------------------------------------------------------------------}
  
  TWeight = Class(TGoogleBaseObject)
  Private
    F_unit : String;
    Fvalue : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_unit(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property _unit : String Index 0 Read F_unit Write Set_unit;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TWeightClass = Class of TWeight;
  
  { --------------------------------------------------------------------
    TAccountsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsResource, method List
  
  TAccountsListOptions = Record
    maxResults : integer;
    pageToken : String;
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
    pageToken : String;
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
    pageToken : String;
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
    pageToken : String;
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
    pageToken : String;
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
    pageToken : String;
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
    pageToken : String;
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
    pageToken : String;
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



Procedure TAccount.SetadwordsLinks(AIndex : Integer; AValue : TAccountTypeadwordsLinksArray); 

begin
  If (FadwordsLinks=AValue) then exit;
  FadwordsLinks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setid(AIndex : Integer; AValue : String); 

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



Procedure TAccount.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetreviewsUrl(AIndex : Integer; AValue : String); 

begin
  If (FreviewsUrl=AValue) then exit;
  FreviewsUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetsellerId(AIndex : Integer; AValue : String); 

begin
  If (FsellerId=AValue) then exit;
  FsellerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setusers(AIndex : Integer; AValue : TAccountTypeusersArray); 

begin
  If (Fusers=AValue) then exit;
  Fusers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetwebsiteUrl(AIndex : Integer; AValue : String); 

begin
  If (FwebsiteUrl=AValue) then exit;
  FwebsiteUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccount.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'adwordslinks' : SetLength(FadwordsLinks,ALength);
  'users' : SetLength(Fusers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccountAdwordsLink
  --------------------------------------------------------------------}


Procedure TAccountAdwordsLink.SetadwordsId(AIndex : Integer; AValue : String); 

begin
  If (FadwordsId=AValue) then exit;
  FadwordsId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountAdwordsLink.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountIdentifier
  --------------------------------------------------------------------}


Procedure TAccountIdentifier.SetaggregatorId(AIndex : Integer; AValue : String); 

begin
  If (FaggregatorId=AValue) then exit;
  FaggregatorId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountIdentifier.SetmerchantId(AIndex : Integer; AValue : String); 

begin
  If (FmerchantId=AValue) then exit;
  FmerchantId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountShipping
  --------------------------------------------------------------------}


Procedure TAccountShipping.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShipping.SetcarrierRates(AIndex : Integer; AValue : TAccountShippingTypecarrierRatesArray); 

begin
  If (FcarrierRates=AValue) then exit;
  FcarrierRates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShipping.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShipping.SetlocationGroups(AIndex : Integer; AValue : TAccountShippingTypelocationGroupsArray); 

begin
  If (FlocationGroups=AValue) then exit;
  FlocationGroups:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShipping.SetrateTables(AIndex : Integer; AValue : TAccountShippingTyperateTablesArray); 

begin
  If (FrateTables=AValue) then exit;
  FrateTables:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShipping.Setservices(AIndex : Integer; AValue : TAccountShippingTypeservicesArray); 

begin
  If (Fservices=AValue) then exit;
  Fservices:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccountShipping.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'carrierrates' : SetLength(FcarrierRates,ALength);
  'locationgroups' : SetLength(FlocationGroups,ALength);
  'ratetables' : SetLength(FrateTables,ALength);
  'services' : SetLength(Fservices,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccountShippingCarrierRate
  --------------------------------------------------------------------}


Procedure TAccountShippingCarrierRate.Setcarrier(AIndex : Integer; AValue : String); 

begin
  If (Fcarrier=AValue) then exit;
  Fcarrier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingCarrierRate.SetcarrierService(AIndex : Integer; AValue : String); 

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



Procedure TAccountShippingCarrierRate.SetmodifierPercent(AIndex : Integer; AValue : String); 

begin
  If (FmodifierPercent=AValue) then exit;
  FmodifierPercent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingCarrierRate.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingCarrierRate.SetsaleCountry(AIndex : Integer; AValue : String); 

begin
  If (FsaleCountry=AValue) then exit;
  FsaleCountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingCarrierRate.SetshippingOrigin(AIndex : Integer; AValue : String); 

begin
  If (FshippingOrigin=AValue) then exit;
  FshippingOrigin:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountShippingCondition
  --------------------------------------------------------------------}


Procedure TAccountShippingCondition.SetdeliveryLocationGroup(AIndex : Integer; AValue : String); 

begin
  If (FdeliveryLocationGroup=AValue) then exit;
  FdeliveryLocationGroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingCondition.SetdeliveryLocationId(AIndex : Integer; AValue : String); 

begin
  If (FdeliveryLocationId=AValue) then exit;
  FdeliveryLocationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingCondition.SetdeliveryPostalCode(AIndex : Integer; AValue : String); 

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



Procedure TAccountShippingCondition.SetshippingLabel(AIndex : Integer; AValue : String); 

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


Procedure TAccountShippingLocationGroup.Setcountry(AIndex : Integer; AValue : String); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingLocationGroup.SetlocationIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FlocationIds=AValue) then exit;
  FlocationIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingLocationGroup.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingLocationGroup.SetpostalCodeRanges(AIndex : Integer; AValue : TAccountShippingLocationGroupTypepostalCodeRangesArray); 

begin
  If (FpostalCodeRanges=AValue) then exit;
  FpostalCodeRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingLocationGroup.SetpostalCodes(AIndex : Integer; AValue : TStringArray); 

begin
  If (FpostalCodes=AValue) then exit;
  FpostalCodes:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccountShippingLocationGroup.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'locationids' : SetLength(FlocationIds,ALength);
  'postalcoderanges' : SetLength(FpostalCodeRanges,ALength);
  'postalcodes' : SetLength(FpostalCodes,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccountShippingPostalCodeRange
  --------------------------------------------------------------------}


Procedure TAccountShippingPostalCodeRange.Set_end(AIndex : Integer; AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingPostalCodeRange.Setstart(AIndex : Integer; AValue : String); 

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


Procedure TAccountShippingRateTable.Setcontent(AIndex : Integer; AValue : TAccountShippingRateTableTypecontentArray); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingRateTable.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingRateTable.SetsaleCountry(AIndex : Integer; AValue : String); 

begin
  If (FsaleCountry=AValue) then exit;
  FsaleCountry:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccountShippingRateTable.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'content' : SetLength(Fcontent,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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



Procedure TAccountShippingShippingService.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingShippingService.SetsaleCountry(AIndex : Integer; AValue : String); 

begin
  If (FsaleCountry=AValue) then exit;
  FsaleCountry:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountShippingShippingServiceCalculationMethod
  --------------------------------------------------------------------}


Procedure TAccountShippingShippingServiceCalculationMethod.SetcarrierRate(AIndex : Integer; AValue : String); 

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



Procedure TAccountShippingShippingServiceCalculationMethod.SetpercentageRate(AIndex : Integer; AValue : String); 

begin
  If (FpercentageRate=AValue) then exit;
  FpercentageRate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountShippingShippingServiceCalculationMethod.SetrateTable(AIndex : Integer; AValue : String); 

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



Procedure TAccountShippingShippingServiceCostRule.Setchildren(AIndex : Integer; AValue : TAccountShippingShippingServiceCostRuleTypechildrenArray); 

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


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccountShippingShippingServiceCostRule.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'children' : SetLength(Fchildren,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccountStatus
  --------------------------------------------------------------------}


Procedure TAccountStatus.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatus.SetdataQualityIssues(AIndex : Integer; AValue : TAccountStatusTypedataQualityIssuesArray); 

begin
  If (FdataQualityIssues=AValue) then exit;
  FdataQualityIssues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatus.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccountStatus.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'dataqualityissues' : SetLength(FdataQualityIssues,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccountStatusDataQualityIssue
  --------------------------------------------------------------------}


Procedure TAccountStatusDataQualityIssue.Setcountry(AIndex : Integer; AValue : String); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatusDataQualityIssue.SetdisplayedValue(AIndex : Integer; AValue : String); 

begin
  If (FdisplayedValue=AValue) then exit;
  FdisplayedValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatusDataQualityIssue.SetexampleItems(AIndex : Integer; AValue : TAccountStatusDataQualityIssueTypeexampleItemsArray); 

begin
  If (FexampleItems=AValue) then exit;
  FexampleItems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatusDataQualityIssue.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatusDataQualityIssue.SetlastChecked(AIndex : Integer; AValue : String); 

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



Procedure TAccountStatusDataQualityIssue.Setseverity(AIndex : Integer; AValue : String); 

begin
  If (Fseverity=AValue) then exit;
  Fseverity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatusDataQualityIssue.SetsubmittedValue(AIndex : Integer; AValue : String); 

begin
  If (FsubmittedValue=AValue) then exit;
  FsubmittedValue:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccountStatusDataQualityIssue.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'exampleitems' : SetLength(FexampleItems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccountStatusExampleItem
  --------------------------------------------------------------------}


Procedure TAccountStatusExampleItem.SetitemId(AIndex : Integer; AValue : String); 

begin
  If (FitemId=AValue) then exit;
  FitemId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatusExampleItem.Setlink(AIndex : Integer; AValue : String); 

begin
  If (Flink=AValue) then exit;
  Flink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatusExampleItem.SetsubmittedValue(AIndex : Integer; AValue : String); 

begin
  If (FsubmittedValue=AValue) then exit;
  FsubmittedValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatusExampleItem.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountStatusExampleItem.SetvalueOnLandingPage(AIndex : Integer; AValue : String); 

begin
  If (FvalueOnLandingPage=AValue) then exit;
  FvalueOnLandingPage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountTax
  --------------------------------------------------------------------}


Procedure TAccountTax.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTax.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTax.Setrules(AIndex : Integer; AValue : TAccountTaxTyperulesArray); 

begin
  If (Frules=AValue) then exit;
  Frules:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccountTax.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'rules' : SetLength(Frules,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccountTaxTaxRule
  --------------------------------------------------------------------}


Procedure TAccountTaxTaxRule.Setcountry(AIndex : Integer; AValue : String); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTaxTaxRule.SetlocationId(AIndex : Integer; AValue : String); 

begin
  If (FlocationId=AValue) then exit;
  FlocationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTaxTaxRule.SetratePercent(AIndex : Integer; AValue : String); 

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



Procedure TAccountUser.SetemailAddress(AIndex : Integer; AValue : String); 

begin
  If (FemailAddress=AValue) then exit;
  FemailAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountsAuthInfoResponse
  --------------------------------------------------------------------}


Procedure TAccountsAuthInfoResponse.SetaccountIdentifiers(AIndex : Integer; AValue : TAccountsAuthInfoResponseTypeaccountIdentifiersArray); 

begin
  If (FaccountIdentifiers=AValue) then exit;
  FaccountIdentifiers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsAuthInfoResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccountsAuthInfoResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'accountidentifiers' : SetLength(FaccountIdentifiers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccountsCustomBatchRequest
  --------------------------------------------------------------------}


Procedure TAccountsCustomBatchRequest.Setentries(AIndex : Integer; AValue : TAccountsCustomBatchRequestTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccountsCustomBatchRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccountsCustomBatchRequestEntry
  --------------------------------------------------------------------}


Procedure TAccountsCustomBatchRequestEntry.Setaccount(AIndex : Integer; AValue : TAccount); 

begin
  If (Faccount=AValue) then exit;
  Faccount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsCustomBatchRequestEntry.SetaccountId(AIndex : Integer; AValue : String); 

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



Procedure TAccountsCustomBatchRequestEntry.SetmerchantId(AIndex : Integer; AValue : String); 

begin
  If (FmerchantId=AValue) then exit;
  FmerchantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsCustomBatchRequestEntry.Setmethod(AIndex : Integer; AValue : String); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountsCustomBatchResponse
  --------------------------------------------------------------------}


Procedure TAccountsCustomBatchResponse.Setentries(AIndex : Integer; AValue : TAccountsCustomBatchResponseTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsCustomBatchResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccountsCustomBatchResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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



Procedure TAccountsCustomBatchResponseEntry.Seterrors(AIndex : Integer; AValue : TErrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsCustomBatchResponseEntry.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountsListResponse
  --------------------------------------------------------------------}


Procedure TAccountsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountsListResponse.Setresources(AIndex : Integer; AValue : TAccountsListResponseTyperesourcesArray); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccountsListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'resources' : SetLength(Fresources,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccountshippingCustomBatchRequest
  --------------------------------------------------------------------}


Procedure TAccountshippingCustomBatchRequest.Setentries(AIndex : Integer; AValue : TAccountshippingCustomBatchRequestTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccountshippingCustomBatchRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccountshippingCustomBatchRequestEntry
  --------------------------------------------------------------------}


Procedure TAccountshippingCustomBatchRequestEntry.SetaccountId(AIndex : Integer; AValue : String); 

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



Procedure TAccountshippingCustomBatchRequestEntry.SetmerchantId(AIndex : Integer; AValue : String); 

begin
  If (FmerchantId=AValue) then exit;
  FmerchantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountshippingCustomBatchRequestEntry.Setmethod(AIndex : Integer; AValue : String); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountshippingCustomBatchResponse
  --------------------------------------------------------------------}


Procedure TAccountshippingCustomBatchResponse.Setentries(AIndex : Integer; AValue : TAccountshippingCustomBatchResponseTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountshippingCustomBatchResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccountshippingCustomBatchResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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



Procedure TAccountshippingCustomBatchResponseEntry.Seterrors(AIndex : Integer; AValue : TErrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountshippingCustomBatchResponseEntry.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountshippingListResponse
  --------------------------------------------------------------------}


Procedure TAccountshippingListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountshippingListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountshippingListResponse.Setresources(AIndex : Integer; AValue : TAccountshippingListResponseTyperesourcesArray); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccountshippingListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'resources' : SetLength(Fresources,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccountstatusesCustomBatchRequest
  --------------------------------------------------------------------}


Procedure TAccountstatusesCustomBatchRequest.Setentries(AIndex : Integer; AValue : TAccountstatusesCustomBatchRequestTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccountstatusesCustomBatchRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccountstatusesCustomBatchRequestEntry
  --------------------------------------------------------------------}


Procedure TAccountstatusesCustomBatchRequestEntry.SetaccountId(AIndex : Integer; AValue : String); 

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



Procedure TAccountstatusesCustomBatchRequestEntry.SetmerchantId(AIndex : Integer; AValue : String); 

begin
  If (FmerchantId=AValue) then exit;
  FmerchantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountstatusesCustomBatchRequestEntry.Setmethod(AIndex : Integer; AValue : String); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountstatusesCustomBatchResponse
  --------------------------------------------------------------------}


Procedure TAccountstatusesCustomBatchResponse.Setentries(AIndex : Integer; AValue : TAccountstatusesCustomBatchResponseTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountstatusesCustomBatchResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccountstatusesCustomBatchResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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



Procedure TAccountstatusesCustomBatchResponseEntry.Seterrors(AIndex : Integer; AValue : TErrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountstatusesListResponse
  --------------------------------------------------------------------}


Procedure TAccountstatusesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountstatusesListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountstatusesListResponse.Setresources(AIndex : Integer; AValue : TAccountstatusesListResponseTyperesourcesArray); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccountstatusesListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'resources' : SetLength(Fresources,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccounttaxCustomBatchRequest
  --------------------------------------------------------------------}


Procedure TAccounttaxCustomBatchRequest.Setentries(AIndex : Integer; AValue : TAccounttaxCustomBatchRequestTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccounttaxCustomBatchRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccounttaxCustomBatchRequestEntry
  --------------------------------------------------------------------}


Procedure TAccounttaxCustomBatchRequestEntry.SetaccountId(AIndex : Integer; AValue : String); 

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



Procedure TAccounttaxCustomBatchRequestEntry.SetmerchantId(AIndex : Integer; AValue : String); 

begin
  If (FmerchantId=AValue) then exit;
  FmerchantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounttaxCustomBatchRequestEntry.Setmethod(AIndex : Integer; AValue : String); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccounttaxCustomBatchResponse
  --------------------------------------------------------------------}


Procedure TAccounttaxCustomBatchResponse.Setentries(AIndex : Integer; AValue : TAccounttaxCustomBatchResponseTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounttaxCustomBatchResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccounttaxCustomBatchResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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



Procedure TAccounttaxCustomBatchResponseEntry.Seterrors(AIndex : Integer; AValue : TErrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounttaxCustomBatchResponseEntry.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccounttaxListResponse
  --------------------------------------------------------------------}


Procedure TAccounttaxListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounttaxListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounttaxListResponse.Setresources(AIndex : Integer; AValue : TAccounttaxListResponseTyperesourcesArray); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccounttaxListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'resources' : SetLength(Fresources,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDatafeed
  --------------------------------------------------------------------}


Procedure TDatafeed.SetattributeLanguage(AIndex : Integer; AValue : String); 

begin
  If (FattributeLanguage=AValue) then exit;
  FattributeLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeed.SetcontentLanguage(AIndex : Integer; AValue : String); 

begin
  If (FcontentLanguage=AValue) then exit;
  FcontentLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeed.SetcontentType(AIndex : Integer; AValue : String); 

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



Procedure TDatafeed.SetfileName(AIndex : Integer; AValue : String); 

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



Procedure TDatafeed.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeed.SetintendedDestinations(AIndex : Integer; AValue : TStringArray); 

begin
  If (FintendedDestinations=AValue) then exit;
  FintendedDestinations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeed.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeed.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeed.SettargetCountry(AIndex : Integer; AValue : String); 

begin
  If (FtargetCountry=AValue) then exit;
  FtargetCountry:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDatafeed.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'intendeddestinations' : SetLength(FintendedDestinations,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDatafeedFetchSchedule
  --------------------------------------------------------------------}


Procedure TDatafeedFetchSchedule.SetdayOfMonth(AIndex : Integer; AValue : integer); 

begin
  If (FdayOfMonth=AValue) then exit;
  FdayOfMonth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedFetchSchedule.SetfetchUrl(AIndex : Integer; AValue : String); 

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



Procedure TDatafeedFetchSchedule.Setpassword(AIndex : Integer; AValue : String); 

begin
  If (Fpassword=AValue) then exit;
  Fpassword:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedFetchSchedule.SettimeZone(AIndex : Integer; AValue : String); 

begin
  If (FtimeZone=AValue) then exit;
  FtimeZone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedFetchSchedule.Setusername(AIndex : Integer; AValue : String); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedFetchSchedule.Setweekday(AIndex : Integer; AValue : String); 

begin
  If (Fweekday=AValue) then exit;
  Fweekday:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedFormat
  --------------------------------------------------------------------}


Procedure TDatafeedFormat.SetcolumnDelimiter(AIndex : Integer; AValue : String); 

begin
  If (FcolumnDelimiter=AValue) then exit;
  FcolumnDelimiter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedFormat.SetfileEncoding(AIndex : Integer; AValue : String); 

begin
  If (FfileEncoding=AValue) then exit;
  FfileEncoding:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedFormat.SetquotingMode(AIndex : Integer; AValue : String); 

begin
  If (FquotingMode=AValue) then exit;
  FquotingMode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedStatus
  --------------------------------------------------------------------}


Procedure TDatafeedStatus.SetdatafeedId(AIndex : Integer; AValue : String); 

begin
  If (FdatafeedId=AValue) then exit;
  FdatafeedId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatus.Seterrors(AIndex : Integer; AValue : TDatafeedStatusTypeerrorsArray); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatus.SetitemsTotal(AIndex : Integer; AValue : String); 

begin
  If (FitemsTotal=AValue) then exit;
  FitemsTotal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatus.SetitemsValid(AIndex : Integer; AValue : String); 

begin
  If (FitemsValid=AValue) then exit;
  FitemsValid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatus.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatus.SetlastUploadDate(AIndex : Integer; AValue : String); 

begin
  If (FlastUploadDate=AValue) then exit;
  FlastUploadDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatus.SetprocessingStatus(AIndex : Integer; AValue : String); 

begin
  If (FprocessingStatus=AValue) then exit;
  FprocessingStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatus.Setwarnings(AIndex : Integer; AValue : TDatafeedStatusTypewarningsArray); 

begin
  If (Fwarnings=AValue) then exit;
  Fwarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDatafeedStatus.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errors' : SetLength(Ferrors,ALength);
  'warnings' : SetLength(Fwarnings,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDatafeedStatusError
  --------------------------------------------------------------------}


Procedure TDatafeedStatusError.Setcode(AIndex : Integer; AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatusError.Setcount(AIndex : Integer; AValue : String); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatusError.Setexamples(AIndex : Integer; AValue : TDatafeedStatusErrorTypeexamplesArray); 

begin
  If (Fexamples=AValue) then exit;
  Fexamples:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatusError.Setmessage(AIndex : Integer; AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDatafeedStatusError.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'examples' : SetLength(Fexamples,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDatafeedStatusExample
  --------------------------------------------------------------------}


Procedure TDatafeedStatusExample.SetitemId(AIndex : Integer; AValue : String); 

begin
  If (FitemId=AValue) then exit;
  FitemId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatusExample.SetlineNumber(AIndex : Integer; AValue : String); 

begin
  If (FlineNumber=AValue) then exit;
  FlineNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedStatusExample.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedsCustomBatchRequest
  --------------------------------------------------------------------}


Procedure TDatafeedsCustomBatchRequest.Setentries(AIndex : Integer; AValue : TDatafeedsCustomBatchRequestTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDatafeedsCustomBatchRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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



Procedure TDatafeedsCustomBatchRequestEntry.SetdatafeedId(AIndex : Integer; AValue : String); 

begin
  If (FdatafeedId=AValue) then exit;
  FdatafeedId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedsCustomBatchRequestEntry.SetmerchantId(AIndex : Integer; AValue : String); 

begin
  If (FmerchantId=AValue) then exit;
  FmerchantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedsCustomBatchRequestEntry.Setmethod(AIndex : Integer; AValue : String); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedsCustomBatchResponse
  --------------------------------------------------------------------}


Procedure TDatafeedsCustomBatchResponse.Setentries(AIndex : Integer; AValue : TDatafeedsCustomBatchResponseTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedsCustomBatchResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDatafeedsCustomBatchResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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



Procedure TDatafeedsCustomBatchResponseEntry.Seterrors(AIndex : Integer; AValue : TErrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedsListResponse
  --------------------------------------------------------------------}


Procedure TDatafeedsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedsListResponse.Setresources(AIndex : Integer; AValue : TDatafeedsListResponseTyperesourcesArray); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDatafeedsListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'resources' : SetLength(Fresources,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDatafeedstatusesCustomBatchRequest
  --------------------------------------------------------------------}


Procedure TDatafeedstatusesCustomBatchRequest.Setentries(AIndex : Integer; AValue : TDatafeedstatusesCustomBatchRequestTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDatafeedstatusesCustomBatchRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDatafeedstatusesCustomBatchRequestEntry
  --------------------------------------------------------------------}


Procedure TDatafeedstatusesCustomBatchRequestEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedstatusesCustomBatchRequestEntry.SetdatafeedId(AIndex : Integer; AValue : String); 

begin
  If (FdatafeedId=AValue) then exit;
  FdatafeedId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedstatusesCustomBatchRequestEntry.SetmerchantId(AIndex : Integer; AValue : String); 

begin
  If (FmerchantId=AValue) then exit;
  FmerchantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedstatusesCustomBatchRequestEntry.Setmethod(AIndex : Integer; AValue : String); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedstatusesCustomBatchResponse
  --------------------------------------------------------------------}


Procedure TDatafeedstatusesCustomBatchResponse.Setentries(AIndex : Integer; AValue : TDatafeedstatusesCustomBatchResponseTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedstatusesCustomBatchResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDatafeedstatusesCustomBatchResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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



Procedure TDatafeedstatusesCustomBatchResponseEntry.Seterrors(AIndex : Integer; AValue : TErrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatafeedstatusesListResponse
  --------------------------------------------------------------------}


Procedure TDatafeedstatusesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedstatusesListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatafeedstatusesListResponse.Setresources(AIndex : Integer; AValue : TDatafeedstatusesListResponseTyperesourcesArray); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDatafeedstatusesListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'resources' : SetLength(Fresources,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TError
  --------------------------------------------------------------------}


Procedure TError.Setdomain(AIndex : Integer; AValue : String); 

begin
  If (Fdomain=AValue) then exit;
  Fdomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TError.Setmessage(AIndex : Integer; AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TError.Setreason(AIndex : Integer; AValue : String); 

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



Procedure TErrors.Seterrors(AIndex : Integer; AValue : TErrorsTypeerrorsArray); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrors.Setmessage(AIndex : Integer; AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TErrors.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errors' : SetLength(Ferrors,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TInventory
  --------------------------------------------------------------------}


Procedure TInventory.Setavailability(AIndex : Integer; AValue : String); 

begin
  If (Favailability=AValue) then exit;
  Favailability:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventory.Setkind(AIndex : Integer; AValue : String); 

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



Procedure TInventory.SetsalePriceEffectiveDate(AIndex : Integer; AValue : String); 

begin
  If (FsalePriceEffectiveDate=AValue) then exit;
  FsalePriceEffectiveDate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInventoryCustomBatchRequest
  --------------------------------------------------------------------}


Procedure TInventoryCustomBatchRequest.Setentries(AIndex : Integer; AValue : TInventoryCustomBatchRequestTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TInventoryCustomBatchRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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



Procedure TInventoryCustomBatchRequestEntry.SetmerchantId(AIndex : Integer; AValue : String); 

begin
  If (FmerchantId=AValue) then exit;
  FmerchantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryCustomBatchRequestEntry.SetproductId(AIndex : Integer; AValue : String); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryCustomBatchRequestEntry.SetstoreCode(AIndex : Integer; AValue : String); 

begin
  If (FstoreCode=AValue) then exit;
  FstoreCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInventoryCustomBatchResponse
  --------------------------------------------------------------------}


Procedure TInventoryCustomBatchResponse.Setentries(AIndex : Integer; AValue : TInventoryCustomBatchResponseTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryCustomBatchResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TInventoryCustomBatchResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TInventoryCustomBatchResponseEntry
  --------------------------------------------------------------------}


Procedure TInventoryCustomBatchResponseEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryCustomBatchResponseEntry.Seterrors(AIndex : Integer; AValue : TErrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInventoryCustomBatchResponseEntry.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInventorySetRequest
  --------------------------------------------------------------------}


Procedure TInventorySetRequest.Setavailability(AIndex : Integer; AValue : String); 

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



Procedure TInventorySetRequest.SetsalePriceEffectiveDate(AIndex : Integer; AValue : String); 

begin
  If (FsalePriceEffectiveDate=AValue) then exit;
  FsalePriceEffectiveDate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInventorySetResponse
  --------------------------------------------------------------------}


Procedure TInventorySetResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLoyaltyPoints
  --------------------------------------------------------------------}


Procedure TLoyaltyPoints.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLoyaltyPoints.SetpointsValue(AIndex : Integer; AValue : String); 

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


Procedure TPrice.Setcurrency(AIndex : Integer; AValue : String); 

begin
  If (Fcurrency=AValue) then exit;
  Fcurrency:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPrice.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProduct
  --------------------------------------------------------------------}


Procedure TProduct.SetadditionalImageLinks(AIndex : Integer; AValue : TStringArray); 

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



Procedure TProduct.SetadwordsGrouping(AIndex : Integer; AValue : String); 

begin
  If (FadwordsGrouping=AValue) then exit;
  FadwordsGrouping:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetadwordsLabels(AIndex : Integer; AValue : TStringArray); 

begin
  If (FadwordsLabels=AValue) then exit;
  FadwordsLabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetadwordsRedirect(AIndex : Integer; AValue : String); 

begin
  If (FadwordsRedirect=AValue) then exit;
  FadwordsRedirect:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetageGroup(AIndex : Integer; AValue : String); 

begin
  If (FageGroup=AValue) then exit;
  FageGroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setaspects(AIndex : Integer; AValue : TProductTypeaspectsArray); 

begin
  If (Faspects=AValue) then exit;
  Faspects:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setavailability(AIndex : Integer; AValue : String); 

begin
  If (Favailability=AValue) then exit;
  Favailability:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetavailabilityDate(AIndex : Integer; AValue : String); 

begin
  If (FavailabilityDate=AValue) then exit;
  FavailabilityDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setbrand(AIndex : Integer; AValue : String); 

begin
  If (Fbrand=AValue) then exit;
  Fbrand:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setchannel(AIndex : Integer; AValue : String); 

begin
  If (Fchannel=AValue) then exit;
  Fchannel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setcolor(AIndex : Integer; AValue : String); 

begin
  If (Fcolor=AValue) then exit;
  Fcolor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setcondition(AIndex : Integer; AValue : String); 

begin
  If (Fcondition=AValue) then exit;
  Fcondition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetcontentLanguage(AIndex : Integer; AValue : String); 

begin
  If (FcontentLanguage=AValue) then exit;
  FcontentLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetcustomAttributes(AIndex : Integer; AValue : TProductTypecustomAttributesArray); 

begin
  If (FcustomAttributes=AValue) then exit;
  FcustomAttributes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetcustomGroups(AIndex : Integer; AValue : TProductTypecustomGroupsArray); 

begin
  If (FcustomGroups=AValue) then exit;
  FcustomGroups:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetcustomLabel0(AIndex : Integer; AValue : String); 

begin
  If (FcustomLabel0=AValue) then exit;
  FcustomLabel0:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetcustomLabel1(AIndex : Integer; AValue : String); 

begin
  If (FcustomLabel1=AValue) then exit;
  FcustomLabel1:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetcustomLabel2(AIndex : Integer; AValue : String); 

begin
  If (FcustomLabel2=AValue) then exit;
  FcustomLabel2:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetcustomLabel3(AIndex : Integer; AValue : String); 

begin
  If (FcustomLabel3=AValue) then exit;
  FcustomLabel3:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetcustomLabel4(AIndex : Integer; AValue : String); 

begin
  If (FcustomLabel4=AValue) then exit;
  FcustomLabel4:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setdestinations(AIndex : Integer; AValue : TProductTypedestinationsArray); 

begin
  If (Fdestinations=AValue) then exit;
  Fdestinations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetdisplayAdsId(AIndex : Integer; AValue : String); 

begin
  If (FdisplayAdsId=AValue) then exit;
  FdisplayAdsId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetdisplayAdsLink(AIndex : Integer; AValue : String); 

begin
  If (FdisplayAdsLink=AValue) then exit;
  FdisplayAdsLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetdisplayAdsSimilarIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FdisplayAdsSimilarIds=AValue) then exit;
  FdisplayAdsSimilarIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetdisplayAdsTitle(AIndex : Integer; AValue : String); 

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



Procedure TProduct.SetenergyEfficiencyClass(AIndex : Integer; AValue : String); 

begin
  If (FenergyEfficiencyClass=AValue) then exit;
  FenergyEfficiencyClass:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetexpirationDate(AIndex : Integer; AValue : String); 

begin
  If (FexpirationDate=AValue) then exit;
  FexpirationDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setgender(AIndex : Integer; AValue : String); 

begin
  If (Fgender=AValue) then exit;
  Fgender:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetgoogleProductCategory(AIndex : Integer; AValue : String); 

begin
  If (FgoogleProductCategory=AValue) then exit;
  FgoogleProductCategory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setgtin(AIndex : Integer; AValue : String); 

begin
  If (Fgtin=AValue) then exit;
  Fgtin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setid(AIndex : Integer; AValue : String); 

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



Procedure TProduct.SetimageLink(AIndex : Integer; AValue : String); 

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



Procedure TProduct.SetitemGroupId(AIndex : Integer; AValue : String); 

begin
  If (FitemGroupId=AValue) then exit;
  FitemGroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setlink(AIndex : Integer; AValue : String); 

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



Procedure TProduct.Setmaterial(AIndex : Integer; AValue : String); 

begin
  If (Fmaterial=AValue) then exit;
  Fmaterial:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetmobileLink(AIndex : Integer; AValue : String); 

begin
  If (FmobileLink=AValue) then exit;
  FmobileLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setmpn(AIndex : Integer; AValue : String); 

begin
  If (Fmpn=AValue) then exit;
  Fmpn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setmultipack(AIndex : Integer; AValue : String); 

begin
  If (Fmultipack=AValue) then exit;
  Fmultipack:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetofferId(AIndex : Integer; AValue : String); 

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



Procedure TProduct.Setpattern(AIndex : Integer; AValue : String); 

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



Procedure TProduct.SetproductType(AIndex : Integer; AValue : String); 

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



Procedure TProduct.SetsalePriceEffectiveDate(AIndex : Integer; AValue : String); 

begin
  If (FsalePriceEffectiveDate=AValue) then exit;
  FsalePriceEffectiveDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setshipping(AIndex : Integer; AValue : TProductTypeshippingArray); 

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



Procedure TProduct.SetshippingLabel(AIndex : Integer; AValue : String); 

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



Procedure TProduct.SetsizeSystem(AIndex : Integer; AValue : String); 

begin
  If (FsizeSystem=AValue) then exit;
  FsizeSystem:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetsizeType(AIndex : Integer; AValue : String); 

begin
  If (FsizeType=AValue) then exit;
  FsizeType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setsizes(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fsizes=AValue) then exit;
  Fsizes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SettargetCountry(AIndex : Integer; AValue : String); 

begin
  If (FtargetCountry=AValue) then exit;
  FtargetCountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Settaxes(AIndex : Integer; AValue : TProductTypetaxesArray); 

begin
  If (Ftaxes=AValue) then exit;
  Ftaxes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Settitle(AIndex : Integer; AValue : String); 

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



Procedure TProduct.SetvalidatedDestinations(AIndex : Integer; AValue : TStringArray); 

begin
  If (FvalidatedDestinations=AValue) then exit;
  FvalidatedDestinations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setwarnings(AIndex : Integer; AValue : TProductTypewarningsArray); 

begin
  If (Fwarnings=AValue) then exit;
  Fwarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TProduct.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'additionalimagelinks' : SetLength(FadditionalImageLinks,ALength);
  'adwordslabels' : SetLength(FadwordsLabels,ALength);
  'aspects' : SetLength(Faspects,ALength);
  'customattributes' : SetLength(FcustomAttributes,ALength);
  'customgroups' : SetLength(FcustomGroups,ALength);
  'destinations' : SetLength(Fdestinations,ALength);
  'displayadssimilarids' : SetLength(FdisplayAdsSimilarIds,ALength);
  'shipping' : SetLength(Fshipping,ALength);
  'sizes' : SetLength(Fsizes,ALength);
  'taxes' : SetLength(Ftaxes,ALength);
  'validateddestinations' : SetLength(FvalidatedDestinations,ALength);
  'warnings' : SetLength(Fwarnings,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TProductAspect
  --------------------------------------------------------------------}


Procedure TProductAspect.SetaspectName(AIndex : Integer; AValue : String); 

begin
  If (FaspectName=AValue) then exit;
  FaspectName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductAspect.SetdestinationName(AIndex : Integer; AValue : String); 

begin
  If (FdestinationName=AValue) then exit;
  FdestinationName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductAspect.Setintention(AIndex : Integer; AValue : String); 

begin
  If (Fintention=AValue) then exit;
  Fintention:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductCustomAttribute
  --------------------------------------------------------------------}


Procedure TProductCustomAttribute.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductCustomAttribute.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductCustomAttribute.Set_unit(AIndex : Integer; AValue : String); 

begin
  If (F_unit=AValue) then exit;
  F_unit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductCustomAttribute.Setvalue(AIndex : Integer; AValue : String); 

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


Procedure TProductCustomGroup.Setattributes(AIndex : Integer; AValue : TProductCustomGroupTypeattributesArray); 

begin
  If (Fattributes=AValue) then exit;
  Fattributes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductCustomGroup.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TProductCustomGroup.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'attributes' : SetLength(Fattributes,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TProductDestination
  --------------------------------------------------------------------}


Procedure TProductDestination.SetdestinationName(AIndex : Integer; AValue : String); 

begin
  If (FdestinationName=AValue) then exit;
  FdestinationName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductDestination.Setintention(AIndex : Integer; AValue : String); 

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



Procedure TProductInstallment.Setmonths(AIndex : Integer; AValue : String); 

begin
  If (Fmonths=AValue) then exit;
  Fmonths:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductShipping
  --------------------------------------------------------------------}


Procedure TProductShipping.Setcountry(AIndex : Integer; AValue : String); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductShipping.SetlocationGroupName(AIndex : Integer; AValue : String); 

begin
  If (FlocationGroupName=AValue) then exit;
  FlocationGroupName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductShipping.SetlocationId(AIndex : Integer; AValue : String); 

begin
  If (FlocationId=AValue) then exit;
  FlocationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductShipping.SetpostalCode(AIndex : Integer; AValue : String); 

begin
  If (FpostalCode=AValue) then exit;
  FpostalCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductShipping.Setprice(AIndex : Integer; AValue : TPrice); 

begin
  If (Fprice=AValue) then exit;
  Fprice:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductShipping.Setregion(AIndex : Integer; AValue : String); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductShipping.Setservice(AIndex : Integer; AValue : String); 

begin
  If (Fservice=AValue) then exit;
  Fservice:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductShippingDimension
  --------------------------------------------------------------------}


Procedure TProductShippingDimension.Set_unit(AIndex : Integer; AValue : String); 

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


Procedure TProductShippingWeight.Set_unit(AIndex : Integer; AValue : String); 

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


Procedure TProductStatus.SetcreationDate(AIndex : Integer; AValue : String); 

begin
  If (FcreationDate=AValue) then exit;
  FcreationDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatus.SetdataQualityIssues(AIndex : Integer; AValue : TProductStatusTypedataQualityIssuesArray); 

begin
  If (FdataQualityIssues=AValue) then exit;
  FdataQualityIssues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatus.SetdestinationStatuses(AIndex : Integer; AValue : TProductStatusTypedestinationStatusesArray); 

begin
  If (FdestinationStatuses=AValue) then exit;
  FdestinationStatuses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatus.SetgoogleExpirationDate(AIndex : Integer; AValue : String); 

begin
  If (FgoogleExpirationDate=AValue) then exit;
  FgoogleExpirationDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatus.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatus.SetlastUpdateDate(AIndex : Integer; AValue : String); 

begin
  If (FlastUpdateDate=AValue) then exit;
  FlastUpdateDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatus.Setlink(AIndex : Integer; AValue : String); 

begin
  If (Flink=AValue) then exit;
  Flink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatus.SetproductId(AIndex : Integer; AValue : String); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatus.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TProductStatus.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'dataqualityissues' : SetLength(FdataQualityIssues,ALength);
  'destinationstatuses' : SetLength(FdestinationStatuses,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TProductStatusDataQualityIssue
  --------------------------------------------------------------------}


Procedure TProductStatusDataQualityIssue.Setdetail(AIndex : Integer; AValue : String); 

begin
  If (Fdetail=AValue) then exit;
  Fdetail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatusDataQualityIssue.SetfetchStatus(AIndex : Integer; AValue : String); 

begin
  If (FfetchStatus=AValue) then exit;
  FfetchStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatusDataQualityIssue.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatusDataQualityIssue.Setlocation(AIndex : Integer; AValue : String); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatusDataQualityIssue.Setseverity(AIndex : Integer; AValue : String); 

begin
  If (Fseverity=AValue) then exit;
  Fseverity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatusDataQualityIssue.Settimestamp(AIndex : Integer; AValue : String); 

begin
  If (Ftimestamp=AValue) then exit;
  Ftimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatusDataQualityIssue.SetvalueOnLandingPage(AIndex : Integer; AValue : String); 

begin
  If (FvalueOnLandingPage=AValue) then exit;
  FvalueOnLandingPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatusDataQualityIssue.SetvalueProvided(AIndex : Integer; AValue : String); 

begin
  If (FvalueProvided=AValue) then exit;
  FvalueProvided:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductStatusDestinationStatus
  --------------------------------------------------------------------}


Procedure TProductStatusDestinationStatus.SetapprovalStatus(AIndex : Integer; AValue : String); 

begin
  If (FapprovalStatus=AValue) then exit;
  FapprovalStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatusDestinationStatus.Setdestination(AIndex : Integer; AValue : String); 

begin
  If (Fdestination=AValue) then exit;
  Fdestination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductStatusDestinationStatus.Setintention(AIndex : Integer; AValue : String); 

begin
  If (Fintention=AValue) then exit;
  Fintention:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductTax
  --------------------------------------------------------------------}


Procedure TProductTax.Setcountry(AIndex : Integer; AValue : String); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductTax.SetlocationId(AIndex : Integer; AValue : String); 

begin
  If (FlocationId=AValue) then exit;
  FlocationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductTax.SetpostalCode(AIndex : Integer; AValue : String); 

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



Procedure TProductTax.Setregion(AIndex : Integer; AValue : String); 

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


Procedure TProductUnitPricingBaseMeasure.Set_unit(AIndex : Integer; AValue : String); 

begin
  If (F_unit=AValue) then exit;
  F_unit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductUnitPricingBaseMeasure.Setvalue(AIndex : Integer; AValue : String); 

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


Procedure TProductUnitPricingMeasure.Set_unit(AIndex : Integer; AValue : String); 

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


Procedure TProductsCustomBatchRequest.Setentries(AIndex : Integer; AValue : TProductsCustomBatchRequestTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TProductsCustomBatchRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TProductsCustomBatchRequestEntry
  --------------------------------------------------------------------}


Procedure TProductsCustomBatchRequestEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductsCustomBatchRequestEntry.SetmerchantId(AIndex : Integer; AValue : String); 

begin
  If (FmerchantId=AValue) then exit;
  FmerchantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductsCustomBatchRequestEntry.Setmethod(AIndex : Integer; AValue : String); 

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



Procedure TProductsCustomBatchRequestEntry.SetproductId(AIndex : Integer; AValue : String); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductsCustomBatchResponse
  --------------------------------------------------------------------}


Procedure TProductsCustomBatchResponse.Setentries(AIndex : Integer; AValue : TProductsCustomBatchResponseTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductsCustomBatchResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TProductsCustomBatchResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TProductsCustomBatchResponseEntry
  --------------------------------------------------------------------}


Procedure TProductsCustomBatchResponseEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductsCustomBatchResponseEntry.Seterrors(AIndex : Integer; AValue : TErrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductsCustomBatchResponseEntry.Setkind(AIndex : Integer; AValue : String); 

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


Procedure TProductsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductsListResponse.Setresources(AIndex : Integer; AValue : TProductsListResponseTyperesourcesArray); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TProductsListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'resources' : SetLength(Fresources,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TProductstatusesCustomBatchRequest
  --------------------------------------------------------------------}


Procedure TProductstatusesCustomBatchRequest.Setentries(AIndex : Integer; AValue : TProductstatusesCustomBatchRequestTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TProductstatusesCustomBatchRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TProductstatusesCustomBatchRequestEntry
  --------------------------------------------------------------------}


Procedure TProductstatusesCustomBatchRequestEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductstatusesCustomBatchRequestEntry.SetmerchantId(AIndex : Integer; AValue : String); 

begin
  If (FmerchantId=AValue) then exit;
  FmerchantId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductstatusesCustomBatchRequestEntry.Setmethod(AIndex : Integer; AValue : String); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductstatusesCustomBatchRequestEntry.SetproductId(AIndex : Integer; AValue : String); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductstatusesCustomBatchResponse
  --------------------------------------------------------------------}


Procedure TProductstatusesCustomBatchResponse.Setentries(AIndex : Integer; AValue : TProductstatusesCustomBatchResponseTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductstatusesCustomBatchResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TProductstatusesCustomBatchResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TProductstatusesCustomBatchResponseEntry
  --------------------------------------------------------------------}


Procedure TProductstatusesCustomBatchResponseEntry.SetbatchId(AIndex : Integer; AValue : integer); 

begin
  If (FbatchId=AValue) then exit;
  FbatchId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductstatusesCustomBatchResponseEntry.Seterrors(AIndex : Integer; AValue : TErrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductstatusesCustomBatchResponseEntry.Setkind(AIndex : Integer; AValue : String); 

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


Procedure TProductstatusesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductstatusesListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductstatusesListResponse.Setresources(AIndex : Integer; AValue : TProductstatusesListResponseTyperesourcesArray); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TProductstatusesListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'resources' : SetLength(Fresources,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TWeight
  --------------------------------------------------------------------}


Procedure TWeight.Set_unit(AIndex : Integer; AValue : String); 

begin
  If (F_unit=AValue) then exit;
  F_unit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWeight.Setvalue(AIndex : Integer; AValue : String); 

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
  Result:='https://www.googleapis.com:443/';
end;

Class Function TContentAPI.APIbasePath : string;

begin
  Result:='/content/v2/';
end;

Class Function TContentAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/content/v2/';
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
  TAccountAdwordsLink.RegisterObject;
  TAccountIdentifier.RegisterObject;
  TAccountShipping.RegisterObject;
  TAccountShippingCarrierRate.RegisterObject;
  TAccountShippingCondition.RegisterObject;
  TAccountShippingLocationGroup.RegisterObject;
  TAccountShippingPostalCodeRange.RegisterObject;
  TAccountShippingRateTable.RegisterObject;
  TAccountShippingRateTableCell.RegisterObject;
  TAccountShippingShippingService.RegisterObject;
  TAccountShippingShippingServiceCalculationMethod.RegisterObject;
  TAccountShippingShippingServiceCostRule.RegisterObject;
  TAccountStatus.RegisterObject;
  TAccountStatusDataQualityIssue.RegisterObject;
  TAccountStatusExampleItem.RegisterObject;
  TAccountTax.RegisterObject;
  TAccountTaxTaxRule.RegisterObject;
  TAccountUser.RegisterObject;
  TAccountsAuthInfoResponse.RegisterObject;
  TAccountsCustomBatchRequest.RegisterObject;
  TAccountsCustomBatchRequestEntry.RegisterObject;
  TAccountsCustomBatchResponse.RegisterObject;
  TAccountsCustomBatchResponseEntry.RegisterObject;
  TAccountsListResponse.RegisterObject;
  TAccountshippingCustomBatchRequest.RegisterObject;
  TAccountshippingCustomBatchRequestEntry.RegisterObject;
  TAccountshippingCustomBatchResponse.RegisterObject;
  TAccountshippingCustomBatchResponseEntry.RegisterObject;
  TAccountshippingListResponse.RegisterObject;
  TAccountstatusesCustomBatchRequest.RegisterObject;
  TAccountstatusesCustomBatchRequestEntry.RegisterObject;
  TAccountstatusesCustomBatchResponse.RegisterObject;
  TAccountstatusesCustomBatchResponseEntry.RegisterObject;
  TAccountstatusesListResponse.RegisterObject;
  TAccounttaxCustomBatchRequest.RegisterObject;
  TAccounttaxCustomBatchRequestEntry.RegisterObject;
  TAccounttaxCustomBatchResponse.RegisterObject;
  TAccounttaxCustomBatchResponseEntry.RegisterObject;
  TAccounttaxListResponse.RegisterObject;
  TDatafeed.RegisterObject;
  TDatafeedFetchSchedule.RegisterObject;
  TDatafeedFormat.RegisterObject;
  TDatafeedStatus.RegisterObject;
  TDatafeedStatusError.RegisterObject;
  TDatafeedStatusExample.RegisterObject;
  TDatafeedsCustomBatchRequest.RegisterObject;
  TDatafeedsCustomBatchRequestEntry.RegisterObject;
  TDatafeedsCustomBatchResponse.RegisterObject;
  TDatafeedsCustomBatchResponseEntry.RegisterObject;
  TDatafeedsListResponse.RegisterObject;
  TDatafeedstatusesCustomBatchRequest.RegisterObject;
  TDatafeedstatusesCustomBatchRequestEntry.RegisterObject;
  TDatafeedstatusesCustomBatchResponse.RegisterObject;
  TDatafeedstatusesCustomBatchResponseEntry.RegisterObject;
  TDatafeedstatusesListResponse.RegisterObject;
  TError.RegisterObject;
  TErrors.RegisterObject;
  TInventory.RegisterObject;
  TInventoryCustomBatchRequest.RegisterObject;
  TInventoryCustomBatchRequestEntry.RegisterObject;
  TInventoryCustomBatchResponse.RegisterObject;
  TInventoryCustomBatchResponseEntry.RegisterObject;
  TInventorySetRequest.RegisterObject;
  TInventorySetResponse.RegisterObject;
  TLoyaltyPoints.RegisterObject;
  TPrice.RegisterObject;
  TProduct.RegisterObject;
  TProductAspect.RegisterObject;
  TProductCustomAttribute.RegisterObject;
  TProductCustomGroup.RegisterObject;
  TProductDestination.RegisterObject;
  TProductInstallment.RegisterObject;
  TProductShipping.RegisterObject;
  TProductShippingDimension.RegisterObject;
  TProductShippingWeight.RegisterObject;
  TProductStatus.RegisterObject;
  TProductStatusDataQualityIssue.RegisterObject;
  TProductStatusDestinationStatus.RegisterObject;
  TProductTax.RegisterObject;
  TProductUnitPricingBaseMeasure.RegisterObject;
  TProductUnitPricingMeasure.RegisterObject;
  TProductsCustomBatchRequest.RegisterObject;
  TProductsCustomBatchRequestEntry.RegisterObject;
  TProductsCustomBatchResponse.RegisterObject;
  TProductsCustomBatchResponseEntry.RegisterObject;
  TProductsListResponse.RegisterObject;
  TProductstatusesCustomBatchRequest.RegisterObject;
  TProductstatusesCustomBatchRequestEntry.RegisterObject;
  TProductstatusesCustomBatchResponse.RegisterObject;
  TProductstatusesCustomBatchResponseEntry.RegisterObject;
  TProductstatusesListResponse.RegisterObject;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
end;



initialization
  TContentAPI.RegisterAPI;
end.
