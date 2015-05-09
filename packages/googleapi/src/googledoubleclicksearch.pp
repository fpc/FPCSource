unit googledoubleclicksearch;
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
//Generated on: 9-5-15 13:22:53
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAvailability = class;
  TConversion = class;
  TConversionList = class;
  TCustomDimension = class;
  TCustomMetric = class;
  TReport = class;
  TReportApiColumnSpec = class;
  TReportRequest = class;
  TReportRow = class;
  TSavedColumn = class;
  TSavedColumnList = class;
  TUpdateAvailabilityRequest = class;
  TUpdateAvailabilityResponse = class;
  TAvailabilityArray = Array of TAvailability;
  TConversionArray = Array of TConversion;
  TConversionListArray = Array of TConversionList;
  TCustomDimensionArray = Array of TCustomDimension;
  TCustomMetricArray = Array of TCustomMetric;
  TReportArray = Array of TReport;
  TReportApiColumnSpecArray = Array of TReportApiColumnSpec;
  TReportRequestArray = Array of TReportRequest;
  TReportRowArray = Array of TReportRow;
  TSavedColumnArray = Array of TSavedColumn;
  TSavedColumnListArray = Array of TSavedColumnList;
  TUpdateAvailabilityRequestArray = Array of TUpdateAvailabilityRequest;
  TUpdateAvailabilityResponseArray = Array of TUpdateAvailabilityResponse;
  //Anonymous types, using auto-generated names
  TReportTypefilesItem = class;
  TReportRequestTypefiltersItem = class;
  TReportRequestTypeorderByItem = class;
  TReportRequestTypereportScope = class;
  TReportRequestTypetimeRange = class;
  TConversionTypecustomDimensionArray = Array of TCustomDimension;
  TConversionTypecustomMetricArray = Array of TCustomMetric;
  TConversionListTypeconversionArray = Array of TConversion;
  TReportTypefilesArray = Array of TReportTypefilesItem;
  TReportTyperowsArray = Array of TReportRow;
  TReportRequestTypecolumnsArray = Array of TReportApiColumnSpec;
  TReportRequestTypefiltersArray = Array of TReportRequestTypefiltersItem;
  TReportRequestTypeorderByArray = Array of TReportRequestTypeorderByItem;
  TSavedColumnListTypeitemsArray = Array of TSavedColumn;
  TUpdateAvailabilityRequestTypeavailabilitiesArray = Array of TAvailability;
  TUpdateAvailabilityResponseTypeavailabilitiesArray = Array of TAvailability;
  
  { --------------------------------------------------------------------
    TAvailability
    --------------------------------------------------------------------}
  
  TAvailability = Class(TGoogleBaseObject)
  Private
    FadvertiserId : String;
    FagencyId : String;
    FavailabilityTimestamp : String;
    FsegmentationId : String;
    FsegmentationName : String;
    FsegmentationType : String;
  Protected
    //Property setters
    Procedure SetadvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetagencyId(AIndex : Integer; AValue : String); virtual;
    Procedure SetavailabilityTimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure SetsegmentationId(AIndex : Integer; AValue : String); virtual;
    Procedure SetsegmentationName(AIndex : Integer; AValue : String); virtual;
    Procedure SetsegmentationType(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property advertiserId : String Index 0 Read FadvertiserId Write SetadvertiserId;
    Property agencyId : String Index 8 Read FagencyId Write SetagencyId;
    Property availabilityTimestamp : String Index 16 Read FavailabilityTimestamp Write SetavailabilityTimestamp;
    Property segmentationId : String Index 24 Read FsegmentationId Write SetsegmentationId;
    Property segmentationName : String Index 32 Read FsegmentationName Write SetsegmentationName;
    Property segmentationType : String Index 40 Read FsegmentationType Write SetsegmentationType;
  end;
  TAvailabilityClass = Class of TAvailability;
  
  { --------------------------------------------------------------------
    TConversion
    --------------------------------------------------------------------}
  
  TConversion = Class(TGoogleBaseObject)
  Private
    FadGroupId : String;
    FadId : String;
    FadvertiserId : String;
    FagencyId : String;
    FattributionModel : String;
    FcampaignId : String;
    Fchannel : String;
    FclickId : String;
    FconversionId : String;
    FconversionModifiedTimestamp : String;
    FconversionTimestamp : String;
    FcountMillis : String;
    FcriterionId : String;
    FcurrencyCode : String;
    FcustomDimension : TConversionTypecustomDimensionArray;
    FcustomMetric : TConversionTypecustomMetricArray;
    FdeviceType : String;
    FdsConversionId : String;
    FengineAccountId : String;
    FfeedId : String;
    FfloodlightOrderId : String;
    FproductCountry : String;
    FproductGroupId : String;
    FproductId : String;
    FproductLanguage : String;
    FquantityMillis : String;
    FrevenueMicros : String;
    FsegmentationId : String;
    FsegmentationName : String;
    FsegmentationType : String;
    Fstate : String;
    FstoreId : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetadGroupId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetagencyId(AIndex : Integer; AValue : String); virtual;
    Procedure SetattributionModel(AIndex : Integer; AValue : String); virtual;
    Procedure SetcampaignId(AIndex : Integer; AValue : String); virtual;
    Procedure Setchannel(AIndex : Integer; AValue : String); virtual;
    Procedure SetclickId(AIndex : Integer; AValue : String); virtual;
    Procedure SetconversionId(AIndex : Integer; AValue : String); virtual;
    Procedure SetconversionModifiedTimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure SetconversionTimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure SetcountMillis(AIndex : Integer; AValue : String); virtual;
    Procedure SetcriterionId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcurrencyCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetcustomDimension(AIndex : Integer; AValue : TConversionTypecustomDimensionArray); virtual;
    Procedure SetcustomMetric(AIndex : Integer; AValue : TConversionTypecustomMetricArray); virtual;
    Procedure SetdeviceType(AIndex : Integer; AValue : String); virtual;
    Procedure SetdsConversionId(AIndex : Integer; AValue : String); virtual;
    Procedure SetengineAccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetfeedId(AIndex : Integer; AValue : String); virtual;
    Procedure SetfloodlightOrderId(AIndex : Integer; AValue : String); virtual;
    Procedure SetproductCountry(AIndex : Integer; AValue : String); virtual;
    Procedure SetproductGroupId(AIndex : Integer; AValue : String); virtual;
    Procedure SetproductId(AIndex : Integer; AValue : String); virtual;
    Procedure SetproductLanguage(AIndex : Integer; AValue : String); virtual;
    Procedure SetquantityMillis(AIndex : Integer; AValue : String); virtual;
    Procedure SetrevenueMicros(AIndex : Integer; AValue : String); virtual;
    Procedure SetsegmentationId(AIndex : Integer; AValue : String); virtual;
    Procedure SetsegmentationName(AIndex : Integer; AValue : String); virtual;
    Procedure SetsegmentationType(AIndex : Integer; AValue : String); virtual;
    Procedure Setstate(AIndex : Integer; AValue : String); virtual;
    Procedure SetstoreId(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property adGroupId : String Index 0 Read FadGroupId Write SetadGroupId;
    Property adId : String Index 8 Read FadId Write SetadId;
    Property advertiserId : String Index 16 Read FadvertiserId Write SetadvertiserId;
    Property agencyId : String Index 24 Read FagencyId Write SetagencyId;
    Property attributionModel : String Index 32 Read FattributionModel Write SetattributionModel;
    Property campaignId : String Index 40 Read FcampaignId Write SetcampaignId;
    Property channel : String Index 48 Read Fchannel Write Setchannel;
    Property clickId : String Index 56 Read FclickId Write SetclickId;
    Property conversionId : String Index 64 Read FconversionId Write SetconversionId;
    Property conversionModifiedTimestamp : String Index 72 Read FconversionModifiedTimestamp Write SetconversionModifiedTimestamp;
    Property conversionTimestamp : String Index 80 Read FconversionTimestamp Write SetconversionTimestamp;
    Property countMillis : String Index 88 Read FcountMillis Write SetcountMillis;
    Property criterionId : String Index 96 Read FcriterionId Write SetcriterionId;
    Property currencyCode : String Index 104 Read FcurrencyCode Write SetcurrencyCode;
    Property customDimension : TConversionTypecustomDimensionArray Index 112 Read FcustomDimension Write SetcustomDimension;
    Property customMetric : TConversionTypecustomMetricArray Index 120 Read FcustomMetric Write SetcustomMetric;
    Property deviceType : String Index 128 Read FdeviceType Write SetdeviceType;
    Property dsConversionId : String Index 136 Read FdsConversionId Write SetdsConversionId;
    Property engineAccountId : String Index 144 Read FengineAccountId Write SetengineAccountId;
    Property feedId : String Index 152 Read FfeedId Write SetfeedId;
    Property floodlightOrderId : String Index 160 Read FfloodlightOrderId Write SetfloodlightOrderId;
    Property productCountry : String Index 168 Read FproductCountry Write SetproductCountry;
    Property productGroupId : String Index 176 Read FproductGroupId Write SetproductGroupId;
    Property productId : String Index 184 Read FproductId Write SetproductId;
    Property productLanguage : String Index 192 Read FproductLanguage Write SetproductLanguage;
    Property quantityMillis : String Index 200 Read FquantityMillis Write SetquantityMillis;
    Property revenueMicros : String Index 208 Read FrevenueMicros Write SetrevenueMicros;
    Property segmentationId : String Index 216 Read FsegmentationId Write SetsegmentationId;
    Property segmentationName : String Index 224 Read FsegmentationName Write SetsegmentationName;
    Property segmentationType : String Index 232 Read FsegmentationType Write SetsegmentationType;
    Property state : String Index 240 Read Fstate Write Setstate;
    Property storeId : String Index 248 Read FstoreId Write SetstoreId;
    Property _type : String Index 256 Read F_type Write Set_type;
  end;
  TConversionClass = Class of TConversion;
  
  { --------------------------------------------------------------------
    TConversionList
    --------------------------------------------------------------------}
  
  TConversionList = Class(TGoogleBaseObject)
  Private
    Fconversion : TConversionListTypeconversionArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setconversion(AIndex : Integer; AValue : TConversionListTypeconversionArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property conversion : TConversionListTypeconversionArray Index 0 Read Fconversion Write Setconversion;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TConversionListClass = Class of TConversionList;
  
  { --------------------------------------------------------------------
    TCustomDimension
    --------------------------------------------------------------------}
  
  TCustomDimension = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TCustomDimensionClass = Class of TCustomDimension;
  
  { --------------------------------------------------------------------
    TCustomMetric
    --------------------------------------------------------------------}
  
  TCustomMetric = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fvalue : double;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property value : double Index 8 Read Fvalue Write Setvalue;
  end;
  TCustomMetricClass = Class of TCustomMetric;
  
  { --------------------------------------------------------------------
    TReportTypefilesItem
    --------------------------------------------------------------------}
  
  TReportTypefilesItem = Class(TGoogleBaseObject)
  Private
    FbyteCount : String;
    Furl : String;
  Protected
    //Property setters
    Procedure SetbyteCount(AIndex : Integer; AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property byteCount : String Index 0 Read FbyteCount Write SetbyteCount;
    Property url : String Index 8 Read Furl Write Seturl;
  end;
  TReportTypefilesItemClass = Class of TReportTypefilesItem;
  
  { --------------------------------------------------------------------
    TReport
    --------------------------------------------------------------------}
  
  TReport = Class(TGoogleBaseObject)
  Private
    Ffiles : TReportTypefilesArray;
    Fid : String;
    FisReportReady : boolean;
    Fkind : String;
    Frequest : TReportRequest;
    FrowCount : integer;
    Frows : TReportTyperowsArray;
    FstatisticsCurrencyCode : String;
    FstatisticsTimeZone : String;
  Protected
    //Property setters
    Procedure Setfiles(AIndex : Integer; AValue : TReportTypefilesArray); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetisReportReady(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setrequest(AIndex : Integer; AValue : TReportRequest); virtual;
    Procedure SetrowCount(AIndex : Integer; AValue : integer); virtual;
    Procedure Setrows(AIndex : Integer; AValue : TReportTyperowsArray); virtual;
    Procedure SetstatisticsCurrencyCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetstatisticsTimeZone(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property files : TReportTypefilesArray Index 0 Read Ffiles Write Setfiles;
    Property id : String Index 8 Read Fid Write Setid;
    Property isReportReady : boolean Index 16 Read FisReportReady Write SetisReportReady;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property request : TReportRequest Index 32 Read Frequest Write Setrequest;
    Property rowCount : integer Index 40 Read FrowCount Write SetrowCount;
    Property rows : TReportTyperowsArray Index 48 Read Frows Write Setrows;
    Property statisticsCurrencyCode : String Index 56 Read FstatisticsCurrencyCode Write SetstatisticsCurrencyCode;
    Property statisticsTimeZone : String Index 64 Read FstatisticsTimeZone Write SetstatisticsTimeZone;
  end;
  TReportClass = Class of TReport;
  
  { --------------------------------------------------------------------
    TReportApiColumnSpec
    --------------------------------------------------------------------}
  
  TReportApiColumnSpec = Class(TGoogleBaseObject)
  Private
    FcolumnName : String;
    FcustomDimensionName : String;
    FcustomMetricName : String;
    FendDate : String;
    FgroupByColumn : boolean;
    FheaderText : String;
    FplatformSource : String;
    FsavedColumnName : String;
    FstartDate : String;
  Protected
    //Property setters
    Procedure SetcolumnName(AIndex : Integer; AValue : String); virtual;
    Procedure SetcustomDimensionName(AIndex : Integer; AValue : String); virtual;
    Procedure SetcustomMetricName(AIndex : Integer; AValue : String); virtual;
    Procedure SetendDate(AIndex : Integer; AValue : String); virtual;
    Procedure SetgroupByColumn(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetheaderText(AIndex : Integer; AValue : String); virtual;
    Procedure SetplatformSource(AIndex : Integer; AValue : String); virtual;
    Procedure SetsavedColumnName(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property columnName : String Index 0 Read FcolumnName Write SetcolumnName;
    Property customDimensionName : String Index 8 Read FcustomDimensionName Write SetcustomDimensionName;
    Property customMetricName : String Index 16 Read FcustomMetricName Write SetcustomMetricName;
    Property endDate : String Index 24 Read FendDate Write SetendDate;
    Property groupByColumn : boolean Index 32 Read FgroupByColumn Write SetgroupByColumn;
    Property headerText : String Index 40 Read FheaderText Write SetheaderText;
    Property platformSource : String Index 48 Read FplatformSource Write SetplatformSource;
    Property savedColumnName : String Index 56 Read FsavedColumnName Write SetsavedColumnName;
    Property startDate : String Index 64 Read FstartDate Write SetstartDate;
  end;
  TReportApiColumnSpecClass = Class of TReportApiColumnSpec;
  
  { --------------------------------------------------------------------
    TReportRequestTypefiltersItem
    --------------------------------------------------------------------}
  
  TReportRequestTypefiltersItem = Class(TGoogleBaseObject)
  Private
    Fcolumn : TReportApiColumnSpec;
    F_operator : String;
    Fvalues : TTJSONSchemaArray;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcolumn(AIndex : Integer; AValue : TReportApiColumnSpec); virtual;
    Procedure Set_operator(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalues(AIndex : Integer; AValue : TTJSONSchemaArray); virtual;
  Public
  Published
    Property column : TReportApiColumnSpec Index 0 Read Fcolumn Write Setcolumn;
    Property _operator : String Index 8 Read F_operator Write Set_operator;
    Property values : TTJSONSchemaArray Index 16 Read Fvalues Write Setvalues;
  end;
  TReportRequestTypefiltersItemClass = Class of TReportRequestTypefiltersItem;
  
  { --------------------------------------------------------------------
    TReportRequestTypeorderByItem
    --------------------------------------------------------------------}
  
  TReportRequestTypeorderByItem = Class(TGoogleBaseObject)
  Private
    Fcolumn : TReportApiColumnSpec;
    FsortOrder : String;
  Protected
    //Property setters
    Procedure Setcolumn(AIndex : Integer; AValue : TReportApiColumnSpec); virtual;
    Procedure SetsortOrder(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property column : TReportApiColumnSpec Index 0 Read Fcolumn Write Setcolumn;
    Property sortOrder : String Index 8 Read FsortOrder Write SetsortOrder;
  end;
  TReportRequestTypeorderByItemClass = Class of TReportRequestTypeorderByItem;
  
  { --------------------------------------------------------------------
    TReportRequestTypereportScope
    --------------------------------------------------------------------}
  
  TReportRequestTypereportScope = Class(TGoogleBaseObject)
  Private
    FadGroupId : String;
    FadId : String;
    FadvertiserId : String;
    FagencyId : String;
    FcampaignId : String;
    FengineAccountId : String;
    FkeywordId : String;
  Protected
    //Property setters
    Procedure SetadGroupId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : String); virtual;
    Procedure SetagencyId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcampaignId(AIndex : Integer; AValue : String); virtual;
    Procedure SetengineAccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetkeywordId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property adGroupId : String Index 0 Read FadGroupId Write SetadGroupId;
    Property adId : String Index 8 Read FadId Write SetadId;
    Property advertiserId : String Index 16 Read FadvertiserId Write SetadvertiserId;
    Property agencyId : String Index 24 Read FagencyId Write SetagencyId;
    Property campaignId : String Index 32 Read FcampaignId Write SetcampaignId;
    Property engineAccountId : String Index 40 Read FengineAccountId Write SetengineAccountId;
    Property keywordId : String Index 48 Read FkeywordId Write SetkeywordId;
  end;
  TReportRequestTypereportScopeClass = Class of TReportRequestTypereportScope;
  
  { --------------------------------------------------------------------
    TReportRequestTypetimeRange
    --------------------------------------------------------------------}
  
  TReportRequestTypetimeRange = Class(TGoogleBaseObject)
  Private
    FchangedAttributesSinceTimestamp : TDatetime;
    FchangedMetricsSinceTimestamp : TDatetime;
    FendDate : String;
    FstartDate : String;
  Protected
    //Property setters
    Procedure SetchangedAttributesSinceTimestamp(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetchangedMetricsSinceTimestamp(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetendDate(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property changedAttributesSinceTimestamp : TDatetime Index 0 Read FchangedAttributesSinceTimestamp Write SetchangedAttributesSinceTimestamp;
    Property changedMetricsSinceTimestamp : TDatetime Index 8 Read FchangedMetricsSinceTimestamp Write SetchangedMetricsSinceTimestamp;
    Property endDate : String Index 16 Read FendDate Write SetendDate;
    Property startDate : String Index 24 Read FstartDate Write SetstartDate;
  end;
  TReportRequestTypetimeRangeClass = Class of TReportRequestTypetimeRange;
  
  { --------------------------------------------------------------------
    TReportRequest
    --------------------------------------------------------------------}
  
  TReportRequest = Class(TGoogleBaseObject)
  Private
    Fcolumns : TReportRequestTypecolumnsArray;
    FdownloadFormat : String;
    Ffilters : TReportRequestTypefiltersArray;
    FincludeDeletedEntities : boolean;
    FincludeRemovedEntities : boolean;
    FmaxRowsPerFile : integer;
    ForderBy : TReportRequestTypeorderByArray;
    FreportScope : TReportRequestTypereportScope;
    FreportType : String;
    FrowCount : integer;
    FstartRow : integer;
    FstatisticsCurrency : String;
    FtimeRange : TReportRequestTypetimeRange;
    FverifySingleTimeZone : boolean;
  Protected
    //Property setters
    Procedure Setcolumns(AIndex : Integer; AValue : TReportRequestTypecolumnsArray); virtual;
    Procedure SetdownloadFormat(AIndex : Integer; AValue : String); virtual;
    Procedure Setfilters(AIndex : Integer; AValue : TReportRequestTypefiltersArray); virtual;
    Procedure SetincludeDeletedEntities(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetincludeRemovedEntities(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetmaxRowsPerFile(AIndex : Integer; AValue : integer); virtual;
    Procedure SetorderBy(AIndex : Integer; AValue : TReportRequestTypeorderByArray); virtual;
    Procedure SetreportScope(AIndex : Integer; AValue : TReportRequestTypereportScope); virtual;
    Procedure SetreportType(AIndex : Integer; AValue : String); virtual;
    Procedure SetrowCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetstartRow(AIndex : Integer; AValue : integer); virtual;
    Procedure SetstatisticsCurrency(AIndex : Integer; AValue : String); virtual;
    Procedure SettimeRange(AIndex : Integer; AValue : TReportRequestTypetimeRange); virtual;
    Procedure SetverifySingleTimeZone(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property columns : TReportRequestTypecolumnsArray Index 0 Read Fcolumns Write Setcolumns;
    Property downloadFormat : String Index 8 Read FdownloadFormat Write SetdownloadFormat;
    Property filters : TReportRequestTypefiltersArray Index 16 Read Ffilters Write Setfilters;
    Property includeDeletedEntities : boolean Index 24 Read FincludeDeletedEntities Write SetincludeDeletedEntities;
    Property includeRemovedEntities : boolean Index 32 Read FincludeRemovedEntities Write SetincludeRemovedEntities;
    Property maxRowsPerFile : integer Index 40 Read FmaxRowsPerFile Write SetmaxRowsPerFile;
    Property orderBy : TReportRequestTypeorderByArray Index 48 Read ForderBy Write SetorderBy;
    Property reportScope : TReportRequestTypereportScope Index 56 Read FreportScope Write SetreportScope;
    Property reportType : String Index 64 Read FreportType Write SetreportType;
    Property rowCount : integer Index 72 Read FrowCount Write SetrowCount;
    Property startRow : integer Index 80 Read FstartRow Write SetstartRow;
    Property statisticsCurrency : String Index 88 Read FstatisticsCurrency Write SetstatisticsCurrency;
    Property timeRange : TReportRequestTypetimeRange Index 96 Read FtimeRange Write SettimeRange;
    Property verifySingleTimeZone : boolean Index 104 Read FverifySingleTimeZone Write SetverifySingleTimeZone;
  end;
  TReportRequestClass = Class of TReportRequest;
  
  { --------------------------------------------------------------------
    TReportRow
    --------------------------------------------------------------------}
  
  TReportRow = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TReportRowClass = Class of TReportRow;
  
  { --------------------------------------------------------------------
    TSavedColumn
    --------------------------------------------------------------------}
  
  TSavedColumn = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FsavedColumnName : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetsavedColumnName(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property savedColumnName : String Index 8 Read FsavedColumnName Write SetsavedColumnName;
    Property _type : String Index 16 Read F_type Write Set_type;
  end;
  TSavedColumnClass = Class of TSavedColumn;
  
  { --------------------------------------------------------------------
    TSavedColumnList
    --------------------------------------------------------------------}
  
  TSavedColumnList = Class(TGoogleBaseObject)
  Private
    Fitems : TSavedColumnListTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TSavedColumnListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TSavedColumnListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TSavedColumnListClass = Class of TSavedColumnList;
  
  { --------------------------------------------------------------------
    TUpdateAvailabilityRequest
    --------------------------------------------------------------------}
  
  TUpdateAvailabilityRequest = Class(TGoogleBaseObject)
  Private
    Favailabilities : TUpdateAvailabilityRequestTypeavailabilitiesArray;
  Protected
    //Property setters
    Procedure Setavailabilities(AIndex : Integer; AValue : TUpdateAvailabilityRequestTypeavailabilitiesArray); virtual;
  Public
  Published
    Property availabilities : TUpdateAvailabilityRequestTypeavailabilitiesArray Index 0 Read Favailabilities Write Setavailabilities;
  end;
  TUpdateAvailabilityRequestClass = Class of TUpdateAvailabilityRequest;
  
  { --------------------------------------------------------------------
    TUpdateAvailabilityResponse
    --------------------------------------------------------------------}
  
  TUpdateAvailabilityResponse = Class(TGoogleBaseObject)
  Private
    Favailabilities : TUpdateAvailabilityResponseTypeavailabilitiesArray;
  Protected
    //Property setters
    Procedure Setavailabilities(AIndex : Integer; AValue : TUpdateAvailabilityResponseTypeavailabilitiesArray); virtual;
  Public
  Published
    Property availabilities : TUpdateAvailabilityResponseTypeavailabilitiesArray Index 0 Read Favailabilities Write Setavailabilities;
  end;
  TUpdateAvailabilityResponseClass = Class of TUpdateAvailabilityResponse;
  
  { --------------------------------------------------------------------
    TConversionResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TConversionResource, method Get
  
  TConversionGetOptions = Record
    adGroupId : int64;
    adId : int64;
    campaignId : int64;
    criterionId : int64;
    endDate : integer;
    rowCount : integer;
    startDate : integer;
    startRow : integer;
  end;
  
  
  //Optional query Options for TConversionResource, method Patch
  
  TConversionPatchOptions = Record
    advertiserId : int64;
    agencyId : int64;
    endDate : integer;
    engineAccountId : int64;
    rowCount : integer;
    startDate : integer;
    startRow : integer;
  end;
  
  TConversionResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(advertiserId: string; agencyId: string; engineAccountId: string; AQuery : string  = '') : TConversionList;
    Function Get(advertiserId: string; agencyId: string; engineAccountId: string; AQuery : TConversiongetOptions) : TConversionList;
    Function Insert(aConversionList : TConversionList) : TConversionList;
    Function Patch(aConversionList : TConversionList; AQuery : string  = '') : TConversionList;
    Function Patch(aConversionList : TConversionList; AQuery : TConversionpatchOptions) : TConversionList;
    Function Update(aConversionList : TConversionList) : TConversionList;
    Function UpdateAvailability(aUpdateAvailabilityRequest : TUpdateAvailabilityRequest) : TUpdateAvailabilityResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TReportsResource
    --------------------------------------------------------------------}
  
  TReportsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Generate(aReportRequest : TReportRequest) : TReport;
    Function Get(reportId: string) : TReport;
    Procedure GetFile(reportFragment: integer; reportId: string);
    Function Request(aReportRequest : TReportRequest) : TReport;
  end;
  
  
  { --------------------------------------------------------------------
    TSavedColumnsResource
    --------------------------------------------------------------------}
  
  TSavedColumnsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(advertiserId: string; agencyId: string) : TSavedColumnList;
  end;
  
  
  { --------------------------------------------------------------------
    TDoubleclicksearchAPI
    --------------------------------------------------------------------}
  
  TDoubleclicksearchAPI = Class(TGoogleAPI)
  Private
    FConversionInstance : TConversionResource;
    FReportsInstance : TReportsResource;
    FSavedColumnsInstance : TSavedColumnsResource;
    Function GetConversionInstance : TConversionResource;virtual;
    Function GetReportsInstance : TReportsResource;virtual;
    Function GetSavedColumnsInstance : TSavedColumnsResource;virtual;
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
    Function CreateConversionResource(AOwner : TComponent) : TConversionResource;virtual;overload;
    Function CreateConversionResource : TConversionResource;virtual;overload;
    Function CreateReportsResource(AOwner : TComponent) : TReportsResource;virtual;overload;
    Function CreateReportsResource : TReportsResource;virtual;overload;
    Function CreateSavedColumnsResource(AOwner : TComponent) : TSavedColumnsResource;virtual;overload;
    Function CreateSavedColumnsResource : TSavedColumnsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ConversionResource : TConversionResource Read GetConversionInstance;
    Property ReportsResource : TReportsResource Read GetReportsInstance;
    Property SavedColumnsResource : TSavedColumnsResource Read GetSavedColumnsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAvailability
  --------------------------------------------------------------------}


Procedure TAvailability.SetadvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvailability.SetagencyId(AIndex : Integer; AValue : String); 

begin
  If (FagencyId=AValue) then exit;
  FagencyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvailability.SetavailabilityTimestamp(AIndex : Integer; AValue : String); 

begin
  If (FavailabilityTimestamp=AValue) then exit;
  FavailabilityTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvailability.SetsegmentationId(AIndex : Integer; AValue : String); 

begin
  If (FsegmentationId=AValue) then exit;
  FsegmentationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvailability.SetsegmentationName(AIndex : Integer; AValue : String); 

begin
  If (FsegmentationName=AValue) then exit;
  FsegmentationName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvailability.SetsegmentationType(AIndex : Integer; AValue : String); 

begin
  If (FsegmentationType=AValue) then exit;
  FsegmentationType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TConversion
  --------------------------------------------------------------------}


Procedure TConversion.SetadGroupId(AIndex : Integer; AValue : String); 

begin
  If (FadGroupId=AValue) then exit;
  FadGroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetadId(AIndex : Integer; AValue : String); 

begin
  If (FadId=AValue) then exit;
  FadId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetadvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetagencyId(AIndex : Integer; AValue : String); 

begin
  If (FagencyId=AValue) then exit;
  FagencyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetattributionModel(AIndex : Integer; AValue : String); 

begin
  If (FattributionModel=AValue) then exit;
  FattributionModel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetcampaignId(AIndex : Integer; AValue : String); 

begin
  If (FcampaignId=AValue) then exit;
  FcampaignId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.Setchannel(AIndex : Integer; AValue : String); 

begin
  If (Fchannel=AValue) then exit;
  Fchannel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetclickId(AIndex : Integer; AValue : String); 

begin
  If (FclickId=AValue) then exit;
  FclickId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetconversionId(AIndex : Integer; AValue : String); 

begin
  If (FconversionId=AValue) then exit;
  FconversionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetconversionModifiedTimestamp(AIndex : Integer; AValue : String); 

begin
  If (FconversionModifiedTimestamp=AValue) then exit;
  FconversionModifiedTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetconversionTimestamp(AIndex : Integer; AValue : String); 

begin
  If (FconversionTimestamp=AValue) then exit;
  FconversionTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetcountMillis(AIndex : Integer; AValue : String); 

begin
  If (FcountMillis=AValue) then exit;
  FcountMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetcriterionId(AIndex : Integer; AValue : String); 

begin
  If (FcriterionId=AValue) then exit;
  FcriterionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetcurrencyCode(AIndex : Integer; AValue : String); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetcustomDimension(AIndex : Integer; AValue : TConversionTypecustomDimensionArray); 

begin
  If (FcustomDimension=AValue) then exit;
  FcustomDimension:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetcustomMetric(AIndex : Integer; AValue : TConversionTypecustomMetricArray); 

begin
  If (FcustomMetric=AValue) then exit;
  FcustomMetric:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetdeviceType(AIndex : Integer; AValue : String); 

begin
  If (FdeviceType=AValue) then exit;
  FdeviceType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetdsConversionId(AIndex : Integer; AValue : String); 

begin
  If (FdsConversionId=AValue) then exit;
  FdsConversionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetengineAccountId(AIndex : Integer; AValue : String); 

begin
  If (FengineAccountId=AValue) then exit;
  FengineAccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetfeedId(AIndex : Integer; AValue : String); 

begin
  If (FfeedId=AValue) then exit;
  FfeedId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetfloodlightOrderId(AIndex : Integer; AValue : String); 

begin
  If (FfloodlightOrderId=AValue) then exit;
  FfloodlightOrderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetproductCountry(AIndex : Integer; AValue : String); 

begin
  If (FproductCountry=AValue) then exit;
  FproductCountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetproductGroupId(AIndex : Integer; AValue : String); 

begin
  If (FproductGroupId=AValue) then exit;
  FproductGroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetproductId(AIndex : Integer; AValue : String); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetproductLanguage(AIndex : Integer; AValue : String); 

begin
  If (FproductLanguage=AValue) then exit;
  FproductLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetquantityMillis(AIndex : Integer; AValue : String); 

begin
  If (FquantityMillis=AValue) then exit;
  FquantityMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetrevenueMicros(AIndex : Integer; AValue : String); 

begin
  If (FrevenueMicros=AValue) then exit;
  FrevenueMicros:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetsegmentationId(AIndex : Integer; AValue : String); 

begin
  If (FsegmentationId=AValue) then exit;
  FsegmentationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetsegmentationName(AIndex : Integer; AValue : String); 

begin
  If (FsegmentationName=AValue) then exit;
  FsegmentationName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetsegmentationType(AIndex : Integer; AValue : String); 

begin
  If (FsegmentationType=AValue) then exit;
  FsegmentationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.Setstate(AIndex : Integer; AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetstoreId(AIndex : Integer; AValue : String); 

begin
  If (FstoreId=AValue) then exit;
  FstoreId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TConversion.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TConversionList
  --------------------------------------------------------------------}


Procedure TConversionList.Setconversion(AIndex : Integer; AValue : TConversionListTypeconversionArray); 

begin
  If (Fconversion=AValue) then exit;
  Fconversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversionList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCustomDimension
  --------------------------------------------------------------------}


Procedure TCustomDimension.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimension.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCustomMetric
  --------------------------------------------------------------------}


Procedure TCustomMetric.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetric.Setvalue(AIndex : Integer; AValue : double); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportTypefilesItem
  --------------------------------------------------------------------}


Procedure TReportTypefilesItem.SetbyteCount(AIndex : Integer; AValue : String); 

begin
  If (FbyteCount=AValue) then exit;
  FbyteCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypefilesItem.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReport
  --------------------------------------------------------------------}


Procedure TReport.Setfiles(AIndex : Integer; AValue : TReportTypefilesArray); 

begin
  If (Ffiles=AValue) then exit;
  Ffiles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SetisReportReady(AIndex : Integer; AValue : boolean); 

begin
  If (FisReportReady=AValue) then exit;
  FisReportReady:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setrequest(AIndex : Integer; AValue : TReportRequest); 

begin
  If (Frequest=AValue) then exit;
  Frequest:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SetrowCount(AIndex : Integer; AValue : integer); 

begin
  If (FrowCount=AValue) then exit;
  FrowCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setrows(AIndex : Integer; AValue : TReportTyperowsArray); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SetstatisticsCurrencyCode(AIndex : Integer; AValue : String); 

begin
  If (FstatisticsCurrencyCode=AValue) then exit;
  FstatisticsCurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SetstatisticsTimeZone(AIndex : Integer; AValue : String); 

begin
  If (FstatisticsTimeZone=AValue) then exit;
  FstatisticsTimeZone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportApiColumnSpec
  --------------------------------------------------------------------}


Procedure TReportApiColumnSpec.SetcolumnName(AIndex : Integer; AValue : String); 

begin
  If (FcolumnName=AValue) then exit;
  FcolumnName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportApiColumnSpec.SetcustomDimensionName(AIndex : Integer; AValue : String); 

begin
  If (FcustomDimensionName=AValue) then exit;
  FcustomDimensionName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportApiColumnSpec.SetcustomMetricName(AIndex : Integer; AValue : String); 

begin
  If (FcustomMetricName=AValue) then exit;
  FcustomMetricName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportApiColumnSpec.SetendDate(AIndex : Integer; AValue : String); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportApiColumnSpec.SetgroupByColumn(AIndex : Integer; AValue : boolean); 

begin
  If (FgroupByColumn=AValue) then exit;
  FgroupByColumn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportApiColumnSpec.SetheaderText(AIndex : Integer; AValue : String); 

begin
  If (FheaderText=AValue) then exit;
  FheaderText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportApiColumnSpec.SetplatformSource(AIndex : Integer; AValue : String); 

begin
  If (FplatformSource=AValue) then exit;
  FplatformSource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportApiColumnSpec.SetsavedColumnName(AIndex : Integer; AValue : String); 

begin
  If (FsavedColumnName=AValue) then exit;
  FsavedColumnName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportApiColumnSpec.SetstartDate(AIndex : Integer; AValue : String); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportRequestTypefiltersItem
  --------------------------------------------------------------------}


Procedure TReportRequestTypefiltersItem.Setcolumn(AIndex : Integer; AValue : TReportApiColumnSpec); 

begin
  If (Fcolumn=AValue) then exit;
  Fcolumn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequestTypefiltersItem.Set_operator(AIndex : Integer; AValue : String); 

begin
  If (F_operator=AValue) then exit;
  F_operator:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequestTypefiltersItem.Setvalues(AIndex : Integer; AValue : TTJSONSchemaArray); 

begin
  If (Fvalues=AValue) then exit;
  Fvalues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TReportRequestTypefiltersItem.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_operator' : Result:='operator';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TReportRequestTypeorderByItem
  --------------------------------------------------------------------}


Procedure TReportRequestTypeorderByItem.Setcolumn(AIndex : Integer; AValue : TReportApiColumnSpec); 

begin
  If (Fcolumn=AValue) then exit;
  Fcolumn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequestTypeorderByItem.SetsortOrder(AIndex : Integer; AValue : String); 

begin
  If (FsortOrder=AValue) then exit;
  FsortOrder:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportRequestTypereportScope
  --------------------------------------------------------------------}


Procedure TReportRequestTypereportScope.SetadGroupId(AIndex : Integer; AValue : String); 

begin
  If (FadGroupId=AValue) then exit;
  FadGroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequestTypereportScope.SetadId(AIndex : Integer; AValue : String); 

begin
  If (FadId=AValue) then exit;
  FadId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequestTypereportScope.SetadvertiserId(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequestTypereportScope.SetagencyId(AIndex : Integer; AValue : String); 

begin
  If (FagencyId=AValue) then exit;
  FagencyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequestTypereportScope.SetcampaignId(AIndex : Integer; AValue : String); 

begin
  If (FcampaignId=AValue) then exit;
  FcampaignId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequestTypereportScope.SetengineAccountId(AIndex : Integer; AValue : String); 

begin
  If (FengineAccountId=AValue) then exit;
  FengineAccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequestTypereportScope.SetkeywordId(AIndex : Integer; AValue : String); 

begin
  If (FkeywordId=AValue) then exit;
  FkeywordId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportRequestTypetimeRange
  --------------------------------------------------------------------}


Procedure TReportRequestTypetimeRange.SetchangedAttributesSinceTimestamp(AIndex : Integer; AValue : TDatetime); 

begin
  If (FchangedAttributesSinceTimestamp=AValue) then exit;
  FchangedAttributesSinceTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequestTypetimeRange.SetchangedMetricsSinceTimestamp(AIndex : Integer; AValue : TDatetime); 

begin
  If (FchangedMetricsSinceTimestamp=AValue) then exit;
  FchangedMetricsSinceTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequestTypetimeRange.SetendDate(AIndex : Integer; AValue : String); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequestTypetimeRange.SetstartDate(AIndex : Integer; AValue : String); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportRequest
  --------------------------------------------------------------------}


Procedure TReportRequest.Setcolumns(AIndex : Integer; AValue : TReportRequestTypecolumnsArray); 

begin
  If (Fcolumns=AValue) then exit;
  Fcolumns:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetdownloadFormat(AIndex : Integer; AValue : String); 

begin
  If (FdownloadFormat=AValue) then exit;
  FdownloadFormat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.Setfilters(AIndex : Integer; AValue : TReportRequestTypefiltersArray); 

begin
  If (Ffilters=AValue) then exit;
  Ffilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetincludeDeletedEntities(AIndex : Integer; AValue : boolean); 

begin
  If (FincludeDeletedEntities=AValue) then exit;
  FincludeDeletedEntities:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetincludeRemovedEntities(AIndex : Integer; AValue : boolean); 

begin
  If (FincludeRemovedEntities=AValue) then exit;
  FincludeRemovedEntities:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetmaxRowsPerFile(AIndex : Integer; AValue : integer); 

begin
  If (FmaxRowsPerFile=AValue) then exit;
  FmaxRowsPerFile:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetorderBy(AIndex : Integer; AValue : TReportRequestTypeorderByArray); 

begin
  If (ForderBy=AValue) then exit;
  ForderBy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetreportScope(AIndex : Integer; AValue : TReportRequestTypereportScope); 

begin
  If (FreportScope=AValue) then exit;
  FreportScope:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetreportType(AIndex : Integer; AValue : String); 

begin
  If (FreportType=AValue) then exit;
  FreportType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetrowCount(AIndex : Integer; AValue : integer); 

begin
  If (FrowCount=AValue) then exit;
  FrowCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetstartRow(AIndex : Integer; AValue : integer); 

begin
  If (FstartRow=AValue) then exit;
  FstartRow:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetstatisticsCurrency(AIndex : Integer; AValue : String); 

begin
  If (FstatisticsCurrency=AValue) then exit;
  FstatisticsCurrency:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SettimeRange(AIndex : Integer; AValue : TReportRequestTypetimeRange); 

begin
  If (FtimeRange=AValue) then exit;
  FtimeRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetverifySingleTimeZone(AIndex : Integer; AValue : boolean); 

begin
  If (FverifySingleTimeZone=AValue) then exit;
  FverifySingleTimeZone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportRow
  --------------------------------------------------------------------}


Class Function TReportRow.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSavedColumn
  --------------------------------------------------------------------}


Procedure TSavedColumn.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedColumn.SetsavedColumnName(AIndex : Integer; AValue : String); 

begin
  If (FsavedColumnName=AValue) then exit;
  FsavedColumnName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedColumn.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TSavedColumn.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TSavedColumnList
  --------------------------------------------------------------------}


Procedure TSavedColumnList.Setitems(AIndex : Integer; AValue : TSavedColumnListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedColumnList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUpdateAvailabilityRequest
  --------------------------------------------------------------------}


Procedure TUpdateAvailabilityRequest.Setavailabilities(AIndex : Integer; AValue : TUpdateAvailabilityRequestTypeavailabilitiesArray); 

begin
  If (Favailabilities=AValue) then exit;
  Favailabilities:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUpdateAvailabilityResponse
  --------------------------------------------------------------------}


Procedure TUpdateAvailabilityResponse.Setavailabilities(AIndex : Integer; AValue : TUpdateAvailabilityResponseTypeavailabilitiesArray); 

begin
  If (Favailabilities=AValue) then exit;
  Favailabilities:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TConversionResource
  --------------------------------------------------------------------}


Class Function TConversionResource.ResourceName : String;

begin
  Result:='conversion';
end;

Class Function TConversionResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdoubleclicksearchAPI;
end;

Function TConversionResource.Get(advertiserId: string; agencyId: string; engineAccountId: string; AQuery : string = '') : TConversionList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'agency/{agencyId}/advertiser/{advertiserId}/engine/{engineAccountId}/conversion';
  _Methodid   = 'doubleclicksearch.conversion.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['advertiserId',advertiserId,'agencyId',agencyId,'engineAccountId',engineAccountId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TConversionList) as TConversionList;
end;


Function TConversionResource.Get(advertiserId: string; agencyId: string; engineAccountId: string; AQuery : TConversiongetOptions) : TConversionList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'adGroupId',AQuery.adGroupId);
  AddToQuery(_Q,'adId',AQuery.adId);
  AddToQuery(_Q,'campaignId',AQuery.campaignId);
  AddToQuery(_Q,'criterionId',AQuery.criterionId);
  AddToQuery(_Q,'endDate',AQuery.endDate);
  AddToQuery(_Q,'rowCount',AQuery.rowCount);
  AddToQuery(_Q,'startDate',AQuery.startDate);
  AddToQuery(_Q,'startRow',AQuery.startRow);
  Result:=Get(advertiserId,agencyId,engineAccountId,_Q);
end;

Function TConversionResource.Insert(aConversionList : TConversionList) : TConversionList;

Const
  _HTTPMethod = 'POST';
  _Path       = 'conversion';
  _Methodid   = 'doubleclicksearch.conversion.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aConversionList,TConversionList) as TConversionList;
end;

Function TConversionResource.Patch(aConversionList : TConversionList; AQuery : string = '') : TConversionList;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'conversion';
  _Methodid   = 'doubleclicksearch.conversion.patch';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aConversionList,TConversionList) as TConversionList;
end;


Function TConversionResource.Patch(aConversionList : TConversionList; AQuery : TConversionpatchOptions) : TConversionList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'advertiserId',AQuery.advertiserId);
  AddToQuery(_Q,'agencyId',AQuery.agencyId);
  AddToQuery(_Q,'endDate',AQuery.endDate);
  AddToQuery(_Q,'engineAccountId',AQuery.engineAccountId);
  AddToQuery(_Q,'rowCount',AQuery.rowCount);
  AddToQuery(_Q,'startDate',AQuery.startDate);
  AddToQuery(_Q,'startRow',AQuery.startRow);
  Result:=Patch(aConversionList,_Q);
end;

Function TConversionResource.Update(aConversionList : TConversionList) : TConversionList;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'conversion';
  _Methodid   = 'doubleclicksearch.conversion.update';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aConversionList,TConversionList) as TConversionList;
end;

Function TConversionResource.UpdateAvailability(aUpdateAvailabilityRequest : TUpdateAvailabilityRequest) : TUpdateAvailabilityResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'conversion/updateAvailability';
  _Methodid   = 'doubleclicksearch.conversion.updateAvailability';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aUpdateAvailabilityRequest,TUpdateAvailabilityResponse) as TUpdateAvailabilityResponse;
end;



{ --------------------------------------------------------------------
  TReportsResource
  --------------------------------------------------------------------}


Class Function TReportsResource.ResourceName : String;

begin
  Result:='reports';
end;

Class Function TReportsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdoubleclicksearchAPI;
end;

Function TReportsResource.Generate(aReportRequest : TReportRequest) : TReport;

Const
  _HTTPMethod = 'POST';
  _Path       = 'reports/generate';
  _Methodid   = 'doubleclicksearch.reports.generate';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aReportRequest,TReport) as TReport;
end;

Function TReportsResource.Get(reportId: string) : TReport;

Const
  _HTTPMethod = 'GET';
  _Path       = 'reports/{reportId}';
  _Methodid   = 'doubleclicksearch.reports.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['reportId',reportId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TReport) as TReport;
end;

Procedure TReportsResource.GetFile(reportFragment: integer; reportId: string);

Const
  _HTTPMethod = 'GET';
  _Path       = 'reports/{reportId}/files/{reportFragment}';
  _Methodid   = 'doubleclicksearch.reports.getFile';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['reportFragment',reportFragment,'reportId',reportId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TReportsResource.Request(aReportRequest : TReportRequest) : TReport;

Const
  _HTTPMethod = 'POST';
  _Path       = 'reports';
  _Methodid   = 'doubleclicksearch.reports.request';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aReportRequest,TReport) as TReport;
end;



{ --------------------------------------------------------------------
  TSavedColumnsResource
  --------------------------------------------------------------------}


Class Function TSavedColumnsResource.ResourceName : String;

begin
  Result:='savedColumns';
end;

Class Function TSavedColumnsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdoubleclicksearchAPI;
end;

Function TSavedColumnsResource.List(advertiserId: string; agencyId: string) : TSavedColumnList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'agency/{agencyId}/advertiser/{advertiserId}/savedcolumns';
  _Methodid   = 'doubleclicksearch.savedColumns.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['advertiserId',advertiserId,'agencyId',agencyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TSavedColumnList) as TSavedColumnList;
end;



{ --------------------------------------------------------------------
  TDoubleclicksearchAPI
  --------------------------------------------------------------------}

Class Function TDoubleclicksearchAPI.APIName : String;

begin
  Result:='doubleclicksearch';
end;

Class Function TDoubleclicksearchAPI.APIVersion : String;

begin
  Result:='v2';
end;

Class Function TDoubleclicksearchAPI.APIRevision : String;

begin
  Result:='20150305';
end;

Class Function TDoubleclicksearchAPI.APIID : String;

begin
  Result:='doubleclicksearch:v2';
end;

Class Function TDoubleclicksearchAPI.APITitle : String;

begin
  Result:='DoubleClick Search API';
end;

Class Function TDoubleclicksearchAPI.APIDescription : String;

begin
  Result:='Report and modify your advertising data in DoubleClick Search (for example, campaigns, ad groups, keywords, and conversions).';
end;

Class Function TDoubleclicksearchAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TDoubleclicksearchAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TDoubleclicksearchAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TDoubleclicksearchAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TDoubleclicksearchAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/doubleclick-search/';
end;

Class Function TDoubleclicksearchAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TDoubleclicksearchAPI.APIbasePath : string;

begin
  Result:='/doubleclicksearch/v2/';
end;

Class Function TDoubleclicksearchAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/doubleclicksearch/v2/';
end;

Class Function TDoubleclicksearchAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TDoubleclicksearchAPI.APIservicePath : string;

begin
  Result:='doubleclicksearch/v2/';
end;

Class Function TDoubleclicksearchAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TDoubleclicksearchAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/doubleclicksearch';
  Result[0].Description:='View and manage your advertising data in DoubleClick Search';
  
end;

Class Function TDoubleclicksearchAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TDoubleclicksearchAPI.RegisterAPIResources;

begin
  TAvailability.RegisterObject;
  TConversion.RegisterObject;
  TConversionList.RegisterObject;
  TCustomDimension.RegisterObject;
  TCustomMetric.RegisterObject;
  TReportTypefilesItem.RegisterObject;
  TReport.RegisterObject;
  TReportApiColumnSpec.RegisterObject;
  TReportRequestTypefiltersItem.RegisterObject;
  TReportRequestTypeorderByItem.RegisterObject;
  TReportRequestTypereportScope.RegisterObject;
  TReportRequestTypetimeRange.RegisterObject;
  TReportRequest.RegisterObject;
  TReportRow.RegisterObject;
  TSavedColumn.RegisterObject;
  TSavedColumnList.RegisterObject;
  TUpdateAvailabilityRequest.RegisterObject;
  TUpdateAvailabilityResponse.RegisterObject;
end;


Function TDoubleclicksearchAPI.GetConversionInstance : TConversionResource;

begin
  if (FConversionInstance=Nil) then
    FConversionInstance:=CreateConversionResource;
  Result:=FConversionInstance;
end;

Function TDoubleclicksearchAPI.CreateConversionResource : TConversionResource;

begin
  Result:=CreateConversionResource(Self);
end;


Function TDoubleclicksearchAPI.CreateConversionResource(AOwner : TComponent) : TConversionResource;

begin
  Result:=TConversionResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDoubleclicksearchAPI.GetReportsInstance : TReportsResource;

begin
  if (FReportsInstance=Nil) then
    FReportsInstance:=CreateReportsResource;
  Result:=FReportsInstance;
end;

Function TDoubleclicksearchAPI.CreateReportsResource : TReportsResource;

begin
  Result:=CreateReportsResource(Self);
end;


Function TDoubleclicksearchAPI.CreateReportsResource(AOwner : TComponent) : TReportsResource;

begin
  Result:=TReportsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDoubleclicksearchAPI.GetSavedColumnsInstance : TSavedColumnsResource;

begin
  if (FSavedColumnsInstance=Nil) then
    FSavedColumnsInstance:=CreateSavedColumnsResource;
  Result:=FSavedColumnsInstance;
end;

Function TDoubleclicksearchAPI.CreateSavedColumnsResource : TSavedColumnsResource;

begin
  Result:=CreateSavedColumnsResource(Self);
end;


Function TDoubleclicksearchAPI.CreateSavedColumnsResource(AOwner : TComponent) : TSavedColumnsResource;

begin
  Result:=TSavedColumnsResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TDoubleclicksearchAPI.RegisterAPI;
end.
