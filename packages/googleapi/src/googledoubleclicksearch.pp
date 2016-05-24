unit googledoubleclicksearch;
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
  TAvailability = class;
  TAvailabilityArray = Array of TAvailability;
  TConversion = class;
  TConversionArray = Array of TConversion;
  TConversioncustomDimension = class;
  TConversioncustomDimensionArray = Array of TConversioncustomDimension;
  TConversioncustomMetric = class;
  TConversioncustomMetricArray = Array of TConversioncustomMetric;
  TConversionList = class;
  TConversionListArray = Array of TConversionList;
  TConversionListconversion = class;
  TConversionListconversionArray = Array of TConversionListconversion;
  TCustomDimension = class;
  TCustomDimensionArray = Array of TCustomDimension;
  TCustomMetric = class;
  TCustomMetricArray = Array of TCustomMetric;
  TReport = class;
  TReportArray = Array of TReport;
  TReportfiles = class;
  TReportfilesArray = Array of TReportfiles;
  TReportrows = class;
  TReportrowsArray = Array of TReportrows;
  TReportApiColumnSpec = class;
  TReportApiColumnSpecArray = Array of TReportApiColumnSpec;
  TReportRequest = class;
  TReportRequestArray = Array of TReportRequest;
  TReportRequestcolumns = class;
  TReportRequestcolumnsArray = Array of TReportRequestcolumns;
  TReportRequestfilters = class;
  TReportRequestfiltersArray = Array of TReportRequestfilters;
  TReportRequestfiltersvalues = class;
  TReportRequestfiltersvaluesArray = Array of TReportRequestfiltersvalues;
  TReportRequestorderBy = class;
  TReportRequestorderByArray = Array of TReportRequestorderBy;
  TReportRequestreportScope = class;
  TReportRequestreportScopeArray = Array of TReportRequestreportScope;
  TReportRequesttimeRange = class;
  TReportRequesttimeRangeArray = Array of TReportRequesttimeRange;
  TReportRow = class;
  TReportRowArray = Array of TReportRow;
  TSavedColumn = class;
  TSavedColumnArray = Array of TSavedColumn;
  TSavedColumnList = class;
  TSavedColumnListArray = Array of TSavedColumnList;
  TSavedColumnListitems = class;
  TSavedColumnListitemsArray = Array of TSavedColumnListitems;
  TUpdateAvailabilityRequest = class;
  TUpdateAvailabilityRequestArray = Array of TUpdateAvailabilityRequest;
  TUpdateAvailabilityRequestavailabilities = class;
  TUpdateAvailabilityRequestavailabilitiesArray = Array of TUpdateAvailabilityRequestavailabilities;
  TUpdateAvailabilityResponse = class;
  TUpdateAvailabilityResponseArray = Array of TUpdateAvailabilityResponse;
  TUpdateAvailabilityResponseavailabilities = class;
  TUpdateAvailabilityResponseavailabilitiesArray = Array of TUpdateAvailabilityResponseavailabilities;
  
  { --------------------------------------------------------------------
    TAvailability
    --------------------------------------------------------------------}
  
  TAvailability = Class(TGoogleBaseObject)
  Private
    FadvertiserId : string;
    FagencyId : string;
    FavailabilityTimestamp : string;
    FsegmentationId : string;
    FsegmentationName : string;
    FsegmentationType : string;
  Protected
    //Property setters
    Procedure SetadvertiserId(AIndex : Integer; AValue : string); virtual;
    Procedure SetagencyId(AIndex : Integer; AValue : string); virtual;
    Procedure SetavailabilityTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure SetsegmentationId(AIndex : Integer; AValue : string); virtual;
    Procedure SetsegmentationName(AIndex : Integer; AValue : string); virtual;
    Procedure SetsegmentationType(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property advertiserId : string Index 0 Read FadvertiserId Write SetadvertiserId;
    Property agencyId : string Index 8 Read FagencyId Write SetagencyId;
    Property availabilityTimestamp : string Index 16 Read FavailabilityTimestamp Write SetavailabilityTimestamp;
    Property segmentationId : string Index 24 Read FsegmentationId Write SetsegmentationId;
    Property segmentationName : string Index 32 Read FsegmentationName Write SetsegmentationName;
    Property segmentationType : string Index 40 Read FsegmentationType Write SetsegmentationType;
  end;
  TAvailabilityClass = Class of TAvailability;
  
  { --------------------------------------------------------------------
    TConversion
    --------------------------------------------------------------------}
  
  TConversion = Class(TGoogleBaseObject)
  Private
    FadGroupId : string;
    FadId : string;
    FadvertiserId : string;
    FagencyId : string;
    FattributionModel : string;
    FcampaignId : string;
    Fchannel : string;
    FclickId : string;
    FconversionId : string;
    FconversionModifiedTimestamp : string;
    FconversionTimestamp : string;
    FcountMillis : string;
    FcriterionId : string;
    FcurrencyCode : string;
    FcustomDimension : TConversioncustomDimension;
    FcustomMetric : TConversioncustomMetric;
    FdsConversionId : string;
    FengineAccountId : string;
    FfeedId : string;
    FfloodlightOrderId : string;
    FproductCountry : string;
    FproductGroupId : string;
    FproductId : string;
    FproductLanguage : string;
    FquantityMillis : string;
    FrevenueMicros : string;
    FsegmentationId : string;
    FsegmentationName : string;
    FsegmentationType : string;
    Fstate : string;
    FstoreId : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetadGroupId(AIndex : Integer; AValue : string); virtual;
    Procedure SetadId(AIndex : Integer; AValue : string); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : string); virtual;
    Procedure SetagencyId(AIndex : Integer; AValue : string); virtual;
    Procedure SetattributionModel(AIndex : Integer; AValue : string); virtual;
    Procedure SetcampaignId(AIndex : Integer; AValue : string); virtual;
    Procedure Setchannel(AIndex : Integer; AValue : string); virtual;
    Procedure SetclickId(AIndex : Integer; AValue : string); virtual;
    Procedure SetconversionId(AIndex : Integer; AValue : string); virtual;
    Procedure SetconversionModifiedTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure SetconversionTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure SetcountMillis(AIndex : Integer; AValue : string); virtual;
    Procedure SetcriterionId(AIndex : Integer; AValue : string); virtual;
    Procedure SetcurrencyCode(AIndex : Integer; AValue : string); virtual;
    Procedure SetcustomDimension(AIndex : Integer; AValue : TConversioncustomDimension); virtual;
    Procedure SetcustomMetric(AIndex : Integer; AValue : TConversioncustomMetric); virtual;
    Procedure SetdsConversionId(AIndex : Integer; AValue : string); virtual;
    Procedure SetengineAccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetfeedId(AIndex : Integer; AValue : string); virtual;
    Procedure SetfloodlightOrderId(AIndex : Integer; AValue : string); virtual;
    Procedure SetproductCountry(AIndex : Integer; AValue : string); virtual;
    Procedure SetproductGroupId(AIndex : Integer; AValue : string); virtual;
    Procedure SetproductId(AIndex : Integer; AValue : string); virtual;
    Procedure SetproductLanguage(AIndex : Integer; AValue : string); virtual;
    Procedure SetquantityMillis(AIndex : Integer; AValue : string); virtual;
    Procedure SetrevenueMicros(AIndex : Integer; AValue : string); virtual;
    Procedure SetsegmentationId(AIndex : Integer; AValue : string); virtual;
    Procedure SetsegmentationName(AIndex : Integer; AValue : string); virtual;
    Procedure SetsegmentationType(AIndex : Integer; AValue : string); virtual;
    Procedure Setstate(AIndex : Integer; AValue : string); virtual;
    Procedure SetstoreId(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property adGroupId : string Index 0 Read FadGroupId Write SetadGroupId;
    Property adId : string Index 8 Read FadId Write SetadId;
    Property advertiserId : string Index 16 Read FadvertiserId Write SetadvertiserId;
    Property agencyId : string Index 24 Read FagencyId Write SetagencyId;
    Property attributionModel : string Index 32 Read FattributionModel Write SetattributionModel;
    Property campaignId : string Index 40 Read FcampaignId Write SetcampaignId;
    Property channel : string Index 48 Read Fchannel Write Setchannel;
    Property clickId : string Index 56 Read FclickId Write SetclickId;
    Property conversionId : string Index 64 Read FconversionId Write SetconversionId;
    Property conversionModifiedTimestamp : string Index 72 Read FconversionModifiedTimestamp Write SetconversionModifiedTimestamp;
    Property conversionTimestamp : string Index 80 Read FconversionTimestamp Write SetconversionTimestamp;
    Property countMillis : string Index 88 Read FcountMillis Write SetcountMillis;
    Property criterionId : string Index 96 Read FcriterionId Write SetcriterionId;
    Property currencyCode : string Index 104 Read FcurrencyCode Write SetcurrencyCode;
    Property customDimension : TConversioncustomDimension Index 112 Read FcustomDimension Write SetcustomDimension;
    Property customMetric : TConversioncustomMetric Index 120 Read FcustomMetric Write SetcustomMetric;
    Property dsConversionId : string Index 128 Read FdsConversionId Write SetdsConversionId;
    Property engineAccountId : string Index 136 Read FengineAccountId Write SetengineAccountId;
    Property feedId : string Index 144 Read FfeedId Write SetfeedId;
    Property floodlightOrderId : string Index 152 Read FfloodlightOrderId Write SetfloodlightOrderId;
    Property productCountry : string Index 160 Read FproductCountry Write SetproductCountry;
    Property productGroupId : string Index 168 Read FproductGroupId Write SetproductGroupId;
    Property productId : string Index 176 Read FproductId Write SetproductId;
    Property productLanguage : string Index 184 Read FproductLanguage Write SetproductLanguage;
    Property quantityMillis : string Index 192 Read FquantityMillis Write SetquantityMillis;
    Property revenueMicros : string Index 200 Read FrevenueMicros Write SetrevenueMicros;
    Property segmentationId : string Index 208 Read FsegmentationId Write SetsegmentationId;
    Property segmentationName : string Index 216 Read FsegmentationName Write SetsegmentationName;
    Property segmentationType : string Index 224 Read FsegmentationType Write SetsegmentationType;
    Property state : string Index 232 Read Fstate Write Setstate;
    Property storeId : string Index 240 Read FstoreId Write SetstoreId;
    Property _type : string Index 248 Read F_type Write Set_type;
  end;
  TConversionClass = Class of TConversion;
  
  { --------------------------------------------------------------------
    TConversioncustomDimension
    --------------------------------------------------------------------}
  
  TConversioncustomDimension = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TConversioncustomDimensionClass = Class of TConversioncustomDimension;
  
  { --------------------------------------------------------------------
    TConversioncustomMetric
    --------------------------------------------------------------------}
  
  TConversioncustomMetric = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TConversioncustomMetricClass = Class of TConversioncustomMetric;
  
  { --------------------------------------------------------------------
    TConversionList
    --------------------------------------------------------------------}
  
  TConversionList = Class(TGoogleBaseObject)
  Private
    Fconversion : TConversionListconversion;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setconversion(AIndex : Integer; AValue : TConversionListconversion); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property conversion : TConversionListconversion Index 0 Read Fconversion Write Setconversion;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TConversionListClass = Class of TConversionList;
  
  { --------------------------------------------------------------------
    TConversionListconversion
    --------------------------------------------------------------------}
  
  TConversionListconversion = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TConversionListconversionClass = Class of TConversionListconversion;
  
  { --------------------------------------------------------------------
    TCustomDimension
    --------------------------------------------------------------------}
  
  TCustomDimension = Class(TGoogleBaseObject)
  Private
    Fname : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TCustomDimensionClass = Class of TCustomDimension;
  
  { --------------------------------------------------------------------
    TCustomMetric
    --------------------------------------------------------------------}
  
  TCustomMetric = Class(TGoogleBaseObject)
  Private
    Fname : string;
    Fvalue : double;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
    Property value : double Index 8 Read Fvalue Write Setvalue;
  end;
  TCustomMetricClass = Class of TCustomMetric;
  
  { --------------------------------------------------------------------
    TReport
    --------------------------------------------------------------------}
  
  TReport = Class(TGoogleBaseObject)
  Private
    Ffiles : TReportfiles;
    Fid : string;
    FisReportReady : boolean;
    Fkind : string;
    Frequest : TReportRequest;
    FrowCount : integer;
    Frows : TReportrows;
    FstatisticsCurrencyCode : string;
    FstatisticsTimeZone : string;
  Protected
    //Property setters
    Procedure Setfiles(AIndex : Integer; AValue : TReportfiles); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetisReportReady(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setrequest(AIndex : Integer; AValue : TReportRequest); virtual;
    Procedure SetrowCount(AIndex : Integer; AValue : integer); virtual;
    Procedure Setrows(AIndex : Integer; AValue : TReportrows); virtual;
    Procedure SetstatisticsCurrencyCode(AIndex : Integer; AValue : string); virtual;
    Procedure SetstatisticsTimeZone(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property files : TReportfiles Index 0 Read Ffiles Write Setfiles;
    Property id : string Index 8 Read Fid Write Setid;
    Property isReportReady : boolean Index 16 Read FisReportReady Write SetisReportReady;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property request : TReportRequest Index 32 Read Frequest Write Setrequest;
    Property rowCount : integer Index 40 Read FrowCount Write SetrowCount;
    Property rows : TReportrows Index 48 Read Frows Write Setrows;
    Property statisticsCurrencyCode : string Index 56 Read FstatisticsCurrencyCode Write SetstatisticsCurrencyCode;
    Property statisticsTimeZone : string Index 64 Read FstatisticsTimeZone Write SetstatisticsTimeZone;
  end;
  TReportClass = Class of TReport;
  
  { --------------------------------------------------------------------
    TReportfiles
    --------------------------------------------------------------------}
  
  TReportfiles = Class(TGoogleBaseObject)
  Private
    FbyteCount : string;
    Furl : string;
  Protected
    //Property setters
    Procedure SetbyteCount(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property byteCount : string Index 0 Read FbyteCount Write SetbyteCount;
    Property url : string Index 8 Read Furl Write Seturl;
  end;
  TReportfilesClass = Class of TReportfiles;
  
  { --------------------------------------------------------------------
    TReportrows
    --------------------------------------------------------------------}
  
  TReportrows = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReportrowsClass = Class of TReportrows;
  
  { --------------------------------------------------------------------
    TReportApiColumnSpec
    --------------------------------------------------------------------}
  
  TReportApiColumnSpec = Class(TGoogleBaseObject)
  Private
    FcolumnName : string;
    FcustomDimensionName : string;
    FcustomMetricName : string;
    FendDate : string;
    FgroupByColumn : boolean;
    FheaderText : string;
    FplatformSource : string;
    FsavedColumnName : string;
    FstartDate : string;
  Protected
    //Property setters
    Procedure SetcolumnName(AIndex : Integer; AValue : string); virtual;
    Procedure SetcustomDimensionName(AIndex : Integer; AValue : string); virtual;
    Procedure SetcustomMetricName(AIndex : Integer; AValue : string); virtual;
    Procedure SetendDate(AIndex : Integer; AValue : string); virtual;
    Procedure SetgroupByColumn(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetheaderText(AIndex : Integer; AValue : string); virtual;
    Procedure SetplatformSource(AIndex : Integer; AValue : string); virtual;
    Procedure SetsavedColumnName(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property columnName : string Index 0 Read FcolumnName Write SetcolumnName;
    Property customDimensionName : string Index 8 Read FcustomDimensionName Write SetcustomDimensionName;
    Property customMetricName : string Index 16 Read FcustomMetricName Write SetcustomMetricName;
    Property endDate : string Index 24 Read FendDate Write SetendDate;
    Property groupByColumn : boolean Index 32 Read FgroupByColumn Write SetgroupByColumn;
    Property headerText : string Index 40 Read FheaderText Write SetheaderText;
    Property platformSource : string Index 48 Read FplatformSource Write SetplatformSource;
    Property savedColumnName : string Index 56 Read FsavedColumnName Write SetsavedColumnName;
    Property startDate : string Index 64 Read FstartDate Write SetstartDate;
  end;
  TReportApiColumnSpecClass = Class of TReportApiColumnSpec;
  
  { --------------------------------------------------------------------
    TReportRequest
    --------------------------------------------------------------------}
  
  TReportRequest = Class(TGoogleBaseObject)
  Private
    Fcolumns : TReportRequestcolumns;
    FdownloadFormat : string;
    Ffilters : TReportRequestfilters;
    FincludeDeletedEntities : boolean;
    FincludeRemovedEntities : boolean;
    FmaxRowsPerFile : integer;
    ForderBy : TReportRequestorderBy;
    FreportScope : TReportRequestreportScope;
    FreportType : string;
    FrowCount : integer;
    FstartRow : integer;
    FstatisticsCurrency : string;
    FtimeRange : TReportRequesttimeRange;
    FverifySingleTimeZone : boolean;
  Protected
    //Property setters
    Procedure Setcolumns(AIndex : Integer; AValue : TReportRequestcolumns); virtual;
    Procedure SetdownloadFormat(AIndex : Integer; AValue : string); virtual;
    Procedure Setfilters(AIndex : Integer; AValue : TReportRequestfilters); virtual;
    Procedure SetincludeDeletedEntities(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetincludeRemovedEntities(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetmaxRowsPerFile(AIndex : Integer; AValue : integer); virtual;
    Procedure SetorderBy(AIndex : Integer; AValue : TReportRequestorderBy); virtual;
    Procedure SetreportScope(AIndex : Integer; AValue : TReportRequestreportScope); virtual;
    Procedure SetreportType(AIndex : Integer; AValue : string); virtual;
    Procedure SetrowCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetstartRow(AIndex : Integer; AValue : integer); virtual;
    Procedure SetstatisticsCurrency(AIndex : Integer; AValue : string); virtual;
    Procedure SettimeRange(AIndex : Integer; AValue : TReportRequesttimeRange); virtual;
    Procedure SetverifySingleTimeZone(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property columns : TReportRequestcolumns Index 0 Read Fcolumns Write Setcolumns;
    Property downloadFormat : string Index 8 Read FdownloadFormat Write SetdownloadFormat;
    Property filters : TReportRequestfilters Index 16 Read Ffilters Write Setfilters;
    Property includeDeletedEntities : boolean Index 24 Read FincludeDeletedEntities Write SetincludeDeletedEntities;
    Property includeRemovedEntities : boolean Index 32 Read FincludeRemovedEntities Write SetincludeRemovedEntities;
    Property maxRowsPerFile : integer Index 40 Read FmaxRowsPerFile Write SetmaxRowsPerFile;
    Property orderBy : TReportRequestorderBy Index 48 Read ForderBy Write SetorderBy;
    Property reportScope : TReportRequestreportScope Index 56 Read FreportScope Write SetreportScope;
    Property reportType : string Index 64 Read FreportType Write SetreportType;
    Property rowCount : integer Index 72 Read FrowCount Write SetrowCount;
    Property startRow : integer Index 80 Read FstartRow Write SetstartRow;
    Property statisticsCurrency : string Index 88 Read FstatisticsCurrency Write SetstatisticsCurrency;
    Property timeRange : TReportRequesttimeRange Index 96 Read FtimeRange Write SettimeRange;
    Property verifySingleTimeZone : boolean Index 104 Read FverifySingleTimeZone Write SetverifySingleTimeZone;
  end;
  TReportRequestClass = Class of TReportRequest;
  
  { --------------------------------------------------------------------
    TReportRequestcolumns
    --------------------------------------------------------------------}
  
  TReportRequestcolumns = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReportRequestcolumnsClass = Class of TReportRequestcolumns;
  
  { --------------------------------------------------------------------
    TReportRequestfilters
    --------------------------------------------------------------------}
  
  TReportRequestfilters = Class(TGoogleBaseObject)
  Private
    Fcolumn : TReportApiColumnSpec;
    F_operator : string;
    Fvalues : TReportRequestfiltersvalues;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcolumn(AIndex : Integer; AValue : TReportApiColumnSpec); virtual;
    Procedure Set_operator(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalues(AIndex : Integer; AValue : TReportRequestfiltersvalues); virtual;
  Public
  Published
    Property column : TReportApiColumnSpec Index 0 Read Fcolumn Write Setcolumn;
    Property _operator : string Index 8 Read F_operator Write Set_operator;
    Property values : TReportRequestfiltersvalues Index 16 Read Fvalues Write Setvalues;
  end;
  TReportRequestfiltersClass = Class of TReportRequestfilters;
  
  { --------------------------------------------------------------------
    TReportRequestfiltersvalues
    --------------------------------------------------------------------}
  
  TReportRequestfiltersvalues = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReportRequestfiltersvaluesClass = Class of TReportRequestfiltersvalues;
  
  { --------------------------------------------------------------------
    TReportRequestorderBy
    --------------------------------------------------------------------}
  
  TReportRequestorderBy = Class(TGoogleBaseObject)
  Private
    Fcolumn : TReportApiColumnSpec;
    FsortOrder : string;
  Protected
    //Property setters
    Procedure Setcolumn(AIndex : Integer; AValue : TReportApiColumnSpec); virtual;
    Procedure SetsortOrder(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property column : TReportApiColumnSpec Index 0 Read Fcolumn Write Setcolumn;
    Property sortOrder : string Index 8 Read FsortOrder Write SetsortOrder;
  end;
  TReportRequestorderByClass = Class of TReportRequestorderBy;
  
  { --------------------------------------------------------------------
    TReportRequestreportScope
    --------------------------------------------------------------------}
  
  TReportRequestreportScope = Class(TGoogleBaseObject)
  Private
    FadGroupId : string;
    FadId : string;
    FadvertiserId : string;
    FagencyId : string;
    FcampaignId : string;
    FengineAccountId : string;
    FkeywordId : string;
  Protected
    //Property setters
    Procedure SetadGroupId(AIndex : Integer; AValue : string); virtual;
    Procedure SetadId(AIndex : Integer; AValue : string); virtual;
    Procedure SetadvertiserId(AIndex : Integer; AValue : string); virtual;
    Procedure SetagencyId(AIndex : Integer; AValue : string); virtual;
    Procedure SetcampaignId(AIndex : Integer; AValue : string); virtual;
    Procedure SetengineAccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetkeywordId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property adGroupId : string Index 0 Read FadGroupId Write SetadGroupId;
    Property adId : string Index 8 Read FadId Write SetadId;
    Property advertiserId : string Index 16 Read FadvertiserId Write SetadvertiserId;
    Property agencyId : string Index 24 Read FagencyId Write SetagencyId;
    Property campaignId : string Index 32 Read FcampaignId Write SetcampaignId;
    Property engineAccountId : string Index 40 Read FengineAccountId Write SetengineAccountId;
    Property keywordId : string Index 48 Read FkeywordId Write SetkeywordId;
  end;
  TReportRequestreportScopeClass = Class of TReportRequestreportScope;
  
  { --------------------------------------------------------------------
    TReportRequesttimeRange
    --------------------------------------------------------------------}
  
  TReportRequesttimeRange = Class(TGoogleBaseObject)
  Private
    FchangedAttributesSinceTimestamp : TDatetime;
    FchangedMetricsSinceTimestamp : TDatetime;
    FendDate : string;
    FstartDate : string;
  Protected
    //Property setters
    Procedure SetchangedAttributesSinceTimestamp(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetchangedMetricsSinceTimestamp(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetendDate(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property changedAttributesSinceTimestamp : TDatetime Index 0 Read FchangedAttributesSinceTimestamp Write SetchangedAttributesSinceTimestamp;
    Property changedMetricsSinceTimestamp : TDatetime Index 8 Read FchangedMetricsSinceTimestamp Write SetchangedMetricsSinceTimestamp;
    Property endDate : string Index 16 Read FendDate Write SetendDate;
    Property startDate : string Index 24 Read FstartDate Write SetstartDate;
  end;
  TReportRequesttimeRangeClass = Class of TReportRequesttimeRange;
  
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
    Fkind : string;
    FsavedColumnName : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetsavedColumnName(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property savedColumnName : string Index 8 Read FsavedColumnName Write SetsavedColumnName;
    Property _type : string Index 16 Read F_type Write Set_type;
  end;
  TSavedColumnClass = Class of TSavedColumn;
  
  { --------------------------------------------------------------------
    TSavedColumnList
    --------------------------------------------------------------------}
  
  TSavedColumnList = Class(TGoogleBaseObject)
  Private
    Fitems : TSavedColumnListitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TSavedColumnListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TSavedColumnListitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TSavedColumnListClass = Class of TSavedColumnList;
  
  { --------------------------------------------------------------------
    TSavedColumnListitems
    --------------------------------------------------------------------}
  
  TSavedColumnListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSavedColumnListitemsClass = Class of TSavedColumnListitems;
  
  { --------------------------------------------------------------------
    TUpdateAvailabilityRequest
    --------------------------------------------------------------------}
  
  TUpdateAvailabilityRequest = Class(TGoogleBaseObject)
  Private
    Favailabilities : TUpdateAvailabilityRequestavailabilities;
  Protected
    //Property setters
    Procedure Setavailabilities(AIndex : Integer; AValue : TUpdateAvailabilityRequestavailabilities); virtual;
  Public
  Published
    Property availabilities : TUpdateAvailabilityRequestavailabilities Index 0 Read Favailabilities Write Setavailabilities;
  end;
  TUpdateAvailabilityRequestClass = Class of TUpdateAvailabilityRequest;
  
  { --------------------------------------------------------------------
    TUpdateAvailabilityRequestavailabilities
    --------------------------------------------------------------------}
  
  TUpdateAvailabilityRequestavailabilities = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUpdateAvailabilityRequestavailabilitiesClass = Class of TUpdateAvailabilityRequestavailabilities;
  
  { --------------------------------------------------------------------
    TUpdateAvailabilityResponse
    --------------------------------------------------------------------}
  
  TUpdateAvailabilityResponse = Class(TGoogleBaseObject)
  Private
    Favailabilities : TUpdateAvailabilityResponseavailabilities;
  Protected
    //Property setters
    Procedure Setavailabilities(AIndex : Integer; AValue : TUpdateAvailabilityResponseavailabilities); virtual;
  Public
  Published
    Property availabilities : TUpdateAvailabilityResponseavailabilities Index 0 Read Favailabilities Write Setavailabilities;
  end;
  TUpdateAvailabilityResponseClass = Class of TUpdateAvailabilityResponse;
  
  { --------------------------------------------------------------------
    TUpdateAvailabilityResponseavailabilities
    --------------------------------------------------------------------}
  
  TUpdateAvailabilityResponseavailabilities = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUpdateAvailabilityResponseavailabilitiesClass = Class of TUpdateAvailabilityResponseavailabilities;
  
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


Procedure TAvailability.SetadvertiserId(AIndex : Integer; AValue : string); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvailability.SetagencyId(AIndex : Integer; AValue : string); 

begin
  If (FagencyId=AValue) then exit;
  FagencyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvailability.SetavailabilityTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FavailabilityTimestamp=AValue) then exit;
  FavailabilityTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvailability.SetsegmentationId(AIndex : Integer; AValue : string); 

begin
  If (FsegmentationId=AValue) then exit;
  FsegmentationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvailability.SetsegmentationName(AIndex : Integer; AValue : string); 

begin
  If (FsegmentationName=AValue) then exit;
  FsegmentationName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvailability.SetsegmentationType(AIndex : Integer; AValue : string); 

begin
  If (FsegmentationType=AValue) then exit;
  FsegmentationType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TConversion
  --------------------------------------------------------------------}


Procedure TConversion.SetadGroupId(AIndex : Integer; AValue : string); 

begin
  If (FadGroupId=AValue) then exit;
  FadGroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetadId(AIndex : Integer; AValue : string); 

begin
  If (FadId=AValue) then exit;
  FadId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetadvertiserId(AIndex : Integer; AValue : string); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetagencyId(AIndex : Integer; AValue : string); 

begin
  If (FagencyId=AValue) then exit;
  FagencyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetattributionModel(AIndex : Integer; AValue : string); 

begin
  If (FattributionModel=AValue) then exit;
  FattributionModel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetcampaignId(AIndex : Integer; AValue : string); 

begin
  If (FcampaignId=AValue) then exit;
  FcampaignId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.Setchannel(AIndex : Integer; AValue : string); 

begin
  If (Fchannel=AValue) then exit;
  Fchannel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetclickId(AIndex : Integer; AValue : string); 

begin
  If (FclickId=AValue) then exit;
  FclickId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetconversionId(AIndex : Integer; AValue : string); 

begin
  If (FconversionId=AValue) then exit;
  FconversionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetconversionModifiedTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FconversionModifiedTimestamp=AValue) then exit;
  FconversionModifiedTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetconversionTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FconversionTimestamp=AValue) then exit;
  FconversionTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetcountMillis(AIndex : Integer; AValue : string); 

begin
  If (FcountMillis=AValue) then exit;
  FcountMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetcriterionId(AIndex : Integer; AValue : string); 

begin
  If (FcriterionId=AValue) then exit;
  FcriterionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetcurrencyCode(AIndex : Integer; AValue : string); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetcustomDimension(AIndex : Integer; AValue : TConversioncustomDimension); 

begin
  If (FcustomDimension=AValue) then exit;
  FcustomDimension:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetcustomMetric(AIndex : Integer; AValue : TConversioncustomMetric); 

begin
  If (FcustomMetric=AValue) then exit;
  FcustomMetric:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetdsConversionId(AIndex : Integer; AValue : string); 

begin
  If (FdsConversionId=AValue) then exit;
  FdsConversionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetengineAccountId(AIndex : Integer; AValue : string); 

begin
  If (FengineAccountId=AValue) then exit;
  FengineAccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetfeedId(AIndex : Integer; AValue : string); 

begin
  If (FfeedId=AValue) then exit;
  FfeedId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetfloodlightOrderId(AIndex : Integer; AValue : string); 

begin
  If (FfloodlightOrderId=AValue) then exit;
  FfloodlightOrderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetproductCountry(AIndex : Integer; AValue : string); 

begin
  If (FproductCountry=AValue) then exit;
  FproductCountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetproductGroupId(AIndex : Integer; AValue : string); 

begin
  If (FproductGroupId=AValue) then exit;
  FproductGroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetproductId(AIndex : Integer; AValue : string); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetproductLanguage(AIndex : Integer; AValue : string); 

begin
  If (FproductLanguage=AValue) then exit;
  FproductLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetquantityMillis(AIndex : Integer; AValue : string); 

begin
  If (FquantityMillis=AValue) then exit;
  FquantityMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetrevenueMicros(AIndex : Integer; AValue : string); 

begin
  If (FrevenueMicros=AValue) then exit;
  FrevenueMicros:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetsegmentationId(AIndex : Integer; AValue : string); 

begin
  If (FsegmentationId=AValue) then exit;
  FsegmentationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetsegmentationName(AIndex : Integer; AValue : string); 

begin
  If (FsegmentationName=AValue) then exit;
  FsegmentationName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetsegmentationType(AIndex : Integer; AValue : string); 

begin
  If (FsegmentationType=AValue) then exit;
  FsegmentationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.Setstate(AIndex : Integer; AValue : string); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.SetstoreId(AIndex : Integer; AValue : string); 

begin
  If (FstoreId=AValue) then exit;
  FstoreId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversion.Set_type(AIndex : Integer; AValue : string); 

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
  TConversioncustomDimension
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TConversioncustomMetric
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TConversionList
  --------------------------------------------------------------------}


Procedure TConversionList.Setconversion(AIndex : Integer; AValue : TConversionListconversion); 

begin
  If (Fconversion=AValue) then exit;
  Fconversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConversionList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TConversionListconversion
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCustomDimension
  --------------------------------------------------------------------}


Procedure TCustomDimension.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimension.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCustomMetric
  --------------------------------------------------------------------}


Procedure TCustomMetric.Setname(AIndex : Integer; AValue : string); 

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
  TReport
  --------------------------------------------------------------------}


Procedure TReport.Setfiles(AIndex : Integer; AValue : TReportfiles); 

begin
  If (Ffiles=AValue) then exit;
  Ffiles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setid(AIndex : Integer; AValue : string); 

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



Procedure TReport.Setkind(AIndex : Integer; AValue : string); 

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



Procedure TReport.Setrows(AIndex : Integer; AValue : TReportrows); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SetstatisticsCurrencyCode(AIndex : Integer; AValue : string); 

begin
  If (FstatisticsCurrencyCode=AValue) then exit;
  FstatisticsCurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SetstatisticsTimeZone(AIndex : Integer; AValue : string); 

begin
  If (FstatisticsTimeZone=AValue) then exit;
  FstatisticsTimeZone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportfiles
  --------------------------------------------------------------------}


Procedure TReportfiles.SetbyteCount(AIndex : Integer; AValue : string); 

begin
  If (FbyteCount=AValue) then exit;
  FbyteCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportfiles.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportrows
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReportApiColumnSpec
  --------------------------------------------------------------------}


Procedure TReportApiColumnSpec.SetcolumnName(AIndex : Integer; AValue : string); 

begin
  If (FcolumnName=AValue) then exit;
  FcolumnName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportApiColumnSpec.SetcustomDimensionName(AIndex : Integer; AValue : string); 

begin
  If (FcustomDimensionName=AValue) then exit;
  FcustomDimensionName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportApiColumnSpec.SetcustomMetricName(AIndex : Integer; AValue : string); 

begin
  If (FcustomMetricName=AValue) then exit;
  FcustomMetricName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportApiColumnSpec.SetendDate(AIndex : Integer; AValue : string); 

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



Procedure TReportApiColumnSpec.SetheaderText(AIndex : Integer; AValue : string); 

begin
  If (FheaderText=AValue) then exit;
  FheaderText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportApiColumnSpec.SetplatformSource(AIndex : Integer; AValue : string); 

begin
  If (FplatformSource=AValue) then exit;
  FplatformSource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportApiColumnSpec.SetsavedColumnName(AIndex : Integer; AValue : string); 

begin
  If (FsavedColumnName=AValue) then exit;
  FsavedColumnName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportApiColumnSpec.SetstartDate(AIndex : Integer; AValue : string); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportRequest
  --------------------------------------------------------------------}


Procedure TReportRequest.Setcolumns(AIndex : Integer; AValue : TReportRequestcolumns); 

begin
  If (Fcolumns=AValue) then exit;
  Fcolumns:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetdownloadFormat(AIndex : Integer; AValue : string); 

begin
  If (FdownloadFormat=AValue) then exit;
  FdownloadFormat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.Setfilters(AIndex : Integer; AValue : TReportRequestfilters); 

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



Procedure TReportRequest.SetorderBy(AIndex : Integer; AValue : TReportRequestorderBy); 

begin
  If (ForderBy=AValue) then exit;
  ForderBy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetreportScope(AIndex : Integer; AValue : TReportRequestreportScope); 

begin
  If (FreportScope=AValue) then exit;
  FreportScope:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetreportType(AIndex : Integer; AValue : string); 

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



Procedure TReportRequest.SetstatisticsCurrency(AIndex : Integer; AValue : string); 

begin
  If (FstatisticsCurrency=AValue) then exit;
  FstatisticsCurrency:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SettimeRange(AIndex : Integer; AValue : TReportRequesttimeRange); 

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
  TReportRequestcolumns
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReportRequestfilters
  --------------------------------------------------------------------}


Procedure TReportRequestfilters.Setcolumn(AIndex : Integer; AValue : TReportApiColumnSpec); 

begin
  If (Fcolumn=AValue) then exit;
  Fcolumn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequestfilters.Set_operator(AIndex : Integer; AValue : string); 

begin
  If (F_operator=AValue) then exit;
  F_operator:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequestfilters.Setvalues(AIndex : Integer; AValue : TReportRequestfiltersvalues); 

begin
  If (Fvalues=AValue) then exit;
  Fvalues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TReportRequestfilters.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_operator' : Result:='operator';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TReportRequestfiltersvalues
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReportRequestorderBy
  --------------------------------------------------------------------}


Procedure TReportRequestorderBy.Setcolumn(AIndex : Integer; AValue : TReportApiColumnSpec); 

begin
  If (Fcolumn=AValue) then exit;
  Fcolumn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequestorderBy.SetsortOrder(AIndex : Integer; AValue : string); 

begin
  If (FsortOrder=AValue) then exit;
  FsortOrder:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportRequestreportScope
  --------------------------------------------------------------------}


Procedure TReportRequestreportScope.SetadGroupId(AIndex : Integer; AValue : string); 

begin
  If (FadGroupId=AValue) then exit;
  FadGroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequestreportScope.SetadId(AIndex : Integer; AValue : string); 

begin
  If (FadId=AValue) then exit;
  FadId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequestreportScope.SetadvertiserId(AIndex : Integer; AValue : string); 

begin
  If (FadvertiserId=AValue) then exit;
  FadvertiserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequestreportScope.SetagencyId(AIndex : Integer; AValue : string); 

begin
  If (FagencyId=AValue) then exit;
  FagencyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequestreportScope.SetcampaignId(AIndex : Integer; AValue : string); 

begin
  If (FcampaignId=AValue) then exit;
  FcampaignId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequestreportScope.SetengineAccountId(AIndex : Integer; AValue : string); 

begin
  If (FengineAccountId=AValue) then exit;
  FengineAccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequestreportScope.SetkeywordId(AIndex : Integer; AValue : string); 

begin
  If (FkeywordId=AValue) then exit;
  FkeywordId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportRequesttimeRange
  --------------------------------------------------------------------}


Procedure TReportRequesttimeRange.SetchangedAttributesSinceTimestamp(AIndex : Integer; AValue : TDatetime); 

begin
  If (FchangedAttributesSinceTimestamp=AValue) then exit;
  FchangedAttributesSinceTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequesttimeRange.SetchangedMetricsSinceTimestamp(AIndex : Integer; AValue : TDatetime); 

begin
  If (FchangedMetricsSinceTimestamp=AValue) then exit;
  FchangedMetricsSinceTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequesttimeRange.SetendDate(AIndex : Integer; AValue : string); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequesttimeRange.SetstartDate(AIndex : Integer; AValue : string); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
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


Procedure TSavedColumn.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedColumn.SetsavedColumnName(AIndex : Integer; AValue : string); 

begin
  If (FsavedColumnName=AValue) then exit;
  FsavedColumnName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedColumn.Set_type(AIndex : Integer; AValue : string); 

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


Procedure TSavedColumnList.Setitems(AIndex : Integer; AValue : TSavedColumnListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedColumnList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSavedColumnListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUpdateAvailabilityRequest
  --------------------------------------------------------------------}


Procedure TUpdateAvailabilityRequest.Setavailabilities(AIndex : Integer; AValue : TUpdateAvailabilityRequestavailabilities); 

begin
  If (Favailabilities=AValue) then exit;
  Favailabilities:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUpdateAvailabilityRequestavailabilities
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUpdateAvailabilityResponse
  --------------------------------------------------------------------}


Procedure TUpdateAvailabilityResponse.Setavailabilities(AIndex : Integer; AValue : TUpdateAvailabilityResponseavailabilities); 

begin
  If (Favailabilities=AValue) then exit;
  Favailabilities:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUpdateAvailabilityResponseavailabilities
  --------------------------------------------------------------------}




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
  Result:='20150312';
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
  TConversioncustomDimension.RegisterObject;
  TConversioncustomMetric.RegisterObject;
  TConversionList.RegisterObject;
  TConversionListconversion.RegisterObject;
  TCustomDimension.RegisterObject;
  TCustomMetric.RegisterObject;
  TReport.RegisterObject;
  TReportfiles.RegisterObject;
  TReportrows.RegisterObject;
  TReportApiColumnSpec.RegisterObject;
  TReportRequest.RegisterObject;
  TReportRequestcolumns.RegisterObject;
  TReportRequestfilters.RegisterObject;
  TReportRequestfiltersvalues.RegisterObject;
  TReportRequestorderBy.RegisterObject;
  TReportRequestreportScope.RegisterObject;
  TReportRequesttimeRange.RegisterObject;
  TReportRow.RegisterObject;
  TSavedColumn.RegisterObject;
  TSavedColumnList.RegisterObject;
  TSavedColumnListitems.RegisterObject;
  TUpdateAvailabilityRequest.RegisterObject;
  TUpdateAvailabilityRequestavailabilities.RegisterObject;
  TUpdateAvailabilityResponse.RegisterObject;
  TUpdateAvailabilityResponseavailabilities.RegisterObject;
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
