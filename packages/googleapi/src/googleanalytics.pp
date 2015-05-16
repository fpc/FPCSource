unit googleanalytics;
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
  TAccount = Class;
  TAccountRef = Class;
  TAccountSummaries = Class;
  TAccountSummary = Class;
  TAccountTicket = Class;
  TAccounts = Class;
  TAdWordsAccount = Class;
  TAnalyticsDataimportDeleteUploadDataRequest = Class;
  TColumn = Class;
  TColumns = Class;
  TCustomDataSource = Class;
  TCustomDataSources = Class;
  TCustomDimension = Class;
  TCustomDimensions = Class;
  TCustomMetric = Class;
  TCustomMetrics = Class;
  TEntityAdWordsLink = Class;
  TEntityAdWordsLinks = Class;
  TEntityUserLink = Class;
  TEntityUserLinks = Class;
  TExperiment = Class;
  TExperiments = Class;
  TFilter = Class;
  TFilterExpression = Class;
  TFilterRef = Class;
  TFilters = Class;
  TGaData = Class;
  TGoal = Class;
  TGoals = Class;
  TMcfData = Class;
  TProfile = Class;
  TProfileFilterLink = Class;
  TProfileFilterLinks = Class;
  TProfileRef = Class;
  TProfileSummary = Class;
  TProfiles = Class;
  TRealtimeData = Class;
  TSegment = Class;
  TSegments = Class;
  TUnsampledReport = Class;
  TUnsampledReports = Class;
  TUpload = Class;
  TUploads = Class;
  TUserRef = Class;
  TWebPropertyRef = Class;
  TWebPropertySummary = Class;
  TWebproperties = Class;
  TWebproperty = Class;
  TAccountArray = Array of TAccount;
  TAccountRefArray = Array of TAccountRef;
  TAccountSummariesArray = Array of TAccountSummaries;
  TAccountSummaryArray = Array of TAccountSummary;
  TAccountTicketArray = Array of TAccountTicket;
  TAccountsArray = Array of TAccounts;
  TAdWordsAccountArray = Array of TAdWordsAccount;
  TAnalyticsDataimportDeleteUploadDataRequestArray = Array of TAnalyticsDataimportDeleteUploadDataRequest;
  TColumnArray = Array of TColumn;
  TColumnsArray = Array of TColumns;
  TCustomDataSourceArray = Array of TCustomDataSource;
  TCustomDataSourcesArray = Array of TCustomDataSources;
  TCustomDimensionArray = Array of TCustomDimension;
  TCustomDimensionsArray = Array of TCustomDimensions;
  TCustomMetricArray = Array of TCustomMetric;
  TCustomMetricsArray = Array of TCustomMetrics;
  TEntityAdWordsLinkArray = Array of TEntityAdWordsLink;
  TEntityAdWordsLinksArray = Array of TEntityAdWordsLinks;
  TEntityUserLinkArray = Array of TEntityUserLink;
  TEntityUserLinksArray = Array of TEntityUserLinks;
  TExperimentArray = Array of TExperiment;
  TExperimentsArray = Array of TExperiments;
  TFilterArray = Array of TFilter;
  TFilterExpressionArray = Array of TFilterExpression;
  TFilterRefArray = Array of TFilterRef;
  TFiltersArray = Array of TFilters;
  TGaDataArray = Array of TGaData;
  TGoalArray = Array of TGoal;
  TGoalsArray = Array of TGoals;
  TMcfDataArray = Array of TMcfData;
  TProfileArray = Array of TProfile;
  TProfileFilterLinkArray = Array of TProfileFilterLink;
  TProfileFilterLinksArray = Array of TProfileFilterLinks;
  TProfileRefArray = Array of TProfileRef;
  TProfileSummaryArray = Array of TProfileSummary;
  TProfilesArray = Array of TProfiles;
  TRealtimeDataArray = Array of TRealtimeData;
  TSegmentArray = Array of TSegment;
  TSegmentsArray = Array of TSegments;
  TUnsampledReportArray = Array of TUnsampledReport;
  TUnsampledReportsArray = Array of TUnsampledReports;
  TUploadArray = Array of TUpload;
  TUploadsArray = Array of TUploads;
  TUserRefArray = Array of TUserRef;
  TWebPropertyRefArray = Array of TWebPropertyRef;
  TWebPropertySummaryArray = Array of TWebPropertySummary;
  TWebpropertiesArray = Array of TWebproperties;
  TWebpropertyArray = Array of TWebproperty;
  //Anonymous types, using auto-generated names
  TAccountTypechildLink = Class;
  TAccountTypepermissions = Class;
  TColumnTypeattributes = Class;
  TCustomDataSourceTypechildLink = Class;
  TCustomDataSourceTypeparentLink = Class;
  TCustomDimensionTypeparentLink = Class;
  TCustomMetricTypeparentLink = Class;
  TEntityAdWordsLinkTypeentity = Class;
  TEntityUserLinkTypeentity = Class;
  TEntityUserLinkTypepermissions = Class;
  TExperimentTypeparentLink = Class;
  TExperimentTypevariationsItem = Class;
  TFilterTypeadvancedDetails = Class;
  TFilterTypelowercaseDetails = Class;
  TFilterTypeparentLink = Class;
  TFilterTypesearchAndReplaceDetails = Class;
  TFilterTypeuppercaseDetails = Class;
  TGaDataTypecolumnHeadersItem = Class;
  TGaDataTypedataTableTypecolsItem = Class;
  TGaDataTypedataTableTyperowsItemTypecItem = Class;
  TGaDataTypedataTableTyperowsItem = Class;
  TGaDataTypedataTable = Class;
  TGaDataTypeprofileInfo = Class;
  TGaDataTypequery = Class;
  TGaDataTypetotalsForAllResults = Class;
  TGoalTypeeventDetailsTypeeventConditionsItem = Class;
  TGoalTypeeventDetails = Class;
  TGoalTypeparentLink = Class;
  TGoalTypeurlDestinationDetailsTypestepsItem = Class;
  TGoalTypeurlDestinationDetails = Class;
  TGoalTypevisitNumPagesDetails = Class;
  TGoalTypevisitTimeOnSiteDetails = Class;
  TMcfDataTypecolumnHeadersItem = Class;
  TMcfDataTypeprofileInfo = Class;
  TMcfDataTypequery = Class;
  TMcfDataTyperowsItemItemTypeconversionPathValueItem = Class;
  TMcfDataTyperowsItemItem = Class;
  TMcfDataTypetotalsForAllResults = Class;
  TProfileTypechildLink = Class;
  TProfileTypeparentLink = Class;
  TProfileTypepermissions = Class;
  TRealtimeDataTypecolumnHeadersItem = Class;
  TRealtimeDataTypeprofileInfo = Class;
  TRealtimeDataTypequery = Class;
  TRealtimeDataTypetotalsForAllResults = Class;
  TUnsampledReportTypecloudStorageDownloadDetails = Class;
  TUnsampledReportTypedriveDownloadDetails = Class;
  TWebpropertyTypechildLink = Class;
  TWebpropertyTypeparentLink = Class;
  TWebpropertyTypepermissions = Class;
  TAccountSummariesTypeitemsArray = Array of TAccountSummary;
  TAccountSummaryTypewebPropertiesArray = Array of TWebPropertySummary;
  TAccountsTypeitemsArray = Array of TAccount;
  TColumnsTypeitemsArray = Array of TColumn;
  TCustomDataSourcesTypeitemsArray = Array of TCustomDataSource;
  TCustomDimensionsTypeitemsArray = Array of TCustomDimension;
  TCustomMetricsTypeitemsArray = Array of TCustomMetric;
  TEntityAdWordsLinkTypeadWordsAccountsArray = Array of TAdWordsAccount;
  TEntityAdWordsLinksTypeitemsArray = Array of TEntityAdWordsLink;
  TEntityUserLinksTypeitemsArray = Array of TEntityUserLink;
  TExperimentTypevariationsArray = Array of TExperimentTypevariationsItem;
  TExperimentsTypeitemsArray = Array of TExperiment;
  TFiltersTypeitemsArray = Array of TFilter;
  TGaDataTypecolumnHeadersArray = Array of TGaDataTypecolumnHeadersItem;
  TGaDataTypedataTableTypecolsArray = Array of TGaDataTypedataTableTypecolsItem;
  TGaDataTypedataTableTyperowsItemTypecArray = Array of TGaDataTypedataTableTyperowsItemTypecItem;
  TGaDataTypedataTableTyperowsArray = Array of TGaDataTypedataTableTyperowsItem;
  TGaDataTyperowsArray = Array of TStringArray;
  TGoalTypeeventDetailsTypeeventConditionsArray = Array of TGoalTypeeventDetailsTypeeventConditionsItem;
  TGoalTypeurlDestinationDetailsTypestepsArray = Array of TGoalTypeurlDestinationDetailsTypestepsItem;
  TGoalsTypeitemsArray = Array of TGoal;
  TMcfDataTypecolumnHeadersArray = Array of TMcfDataTypecolumnHeadersItem;
  TMcfDataTyperowsItemItemTypeconversionPathValueArray = Array of TMcfDataTyperowsItemItemTypeconversionPathValueItem;
  TMcfDataTyperowsItemArray = Array of TMcfDataTyperowsItemItem;
  TMcfDataTyperowsArray = Array of TMcfDataTyperowsItemArray;
  TProfileFilterLinksTypeitemsArray = Array of TProfileFilterLink;
  TProfilesTypeitemsArray = Array of TProfile;
  TRealtimeDataTypecolumnHeadersArray = Array of TRealtimeDataTypecolumnHeadersItem;
  TRealtimeDataTyperowsArray = Array of TStringArray;
  TSegmentsTypeitemsArray = Array of TSegment;
  TUnsampledReportsTypeitemsArray = Array of TUnsampledReport;
  TUploadsTypeitemsArray = Array of TUpload;
  TWebPropertySummaryTypeprofilesArray = Array of TProfileSummary;
  TWebpropertiesTypeitemsArray = Array of TWebproperty;
  
  { --------------------------------------------------------------------
    TAccountTypechildLink
    --------------------------------------------------------------------}
  
  TAccountTypechildLink = Class(TGoogleBaseObject)
  Private
    Fhref : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property href : String Index 0 Read Fhref Write Sethref;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TAccountTypechildLinkClass = Class of TAccountTypechildLink;
  
  { --------------------------------------------------------------------
    TAccountTypepermissions
    --------------------------------------------------------------------}
  
  TAccountTypepermissions = Class(TGoogleBaseObject)
  Private
    Feffective : TStringArray;
  Protected
    //Property setters
    Procedure Seteffective(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property effective : TStringArray Index 0 Read Feffective Write Seteffective;
  end;
  TAccountTypepermissionsClass = Class of TAccountTypepermissions;
  
  { --------------------------------------------------------------------
    TAccount
    --------------------------------------------------------------------}
  
  TAccount = Class(TGoogleBaseObject)
  Private
    FchildLink : TAccountTypechildLink;
    Fcreated : TDatetime;
    Fid : String;
    Fkind : String;
    Fname : String;
    Fpermissions : TAccountTypepermissions;
    FselfLink : String;
    Fupdated : TDatetime;
  Protected
    //Property setters
    Procedure SetchildLink(AIndex : Integer; AValue : TAccountTypechildLink); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setpermissions(AIndex : Integer; AValue : TAccountTypepermissions); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property childLink : TAccountTypechildLink Index 0 Read FchildLink Write SetchildLink;
    Property created : TDatetime Index 8 Read Fcreated Write Setcreated;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property name : String Index 32 Read Fname Write Setname;
    Property permissions : TAccountTypepermissions Index 40 Read Fpermissions Write Setpermissions;
    Property selfLink : String Index 48 Read FselfLink Write SetselfLink;
    Property updated : TDatetime Index 56 Read Fupdated Write Setupdated;
  end;
  TAccountClass = Class of TAccount;
  
  { --------------------------------------------------------------------
    TAccountRef
    --------------------------------------------------------------------}
  
  TAccountRef = Class(TGoogleBaseObject)
  Private
    Fhref : String;
    Fid : String;
    Fkind : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property href : String Index 0 Read Fhref Write Sethref;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property name : String Index 24 Read Fname Write Setname;
  end;
  TAccountRefClass = Class of TAccountRef;
  
  { --------------------------------------------------------------------
    TAccountSummaries
    --------------------------------------------------------------------}
  
  TAccountSummaries = Class(TGoogleBaseObject)
  Private
    Fitems : TAccountSummariesTypeitemsArray;
    FitemsPerPage : integer;
    Fkind : String;
    FnextLink : String;
    FpreviousLink : String;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TAccountSummariesTypeitemsArray); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TAccountSummariesTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextLink : String Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : String Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : String Index 56 Read Fusername Write Setusername;
  end;
  TAccountSummariesClass = Class of TAccountSummaries;
  
  { --------------------------------------------------------------------
    TAccountSummary
    --------------------------------------------------------------------}
  
  TAccountSummary = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
    Fname : String;
    FwebProperties : TAccountSummaryTypewebPropertiesArray;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetwebProperties(AIndex : Integer; AValue : TAccountSummaryTypewebPropertiesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property name : String Index 16 Read Fname Write Setname;
    Property webProperties : TAccountSummaryTypewebPropertiesArray Index 24 Read FwebProperties Write SetwebProperties;
  end;
  TAccountSummaryClass = Class of TAccountSummary;
  
  { --------------------------------------------------------------------
    TAccountTicket
    --------------------------------------------------------------------}
  
  TAccountTicket = Class(TGoogleBaseObject)
  Private
    Faccount : TAccount;
    Fid : String;
    Fkind : String;
    Fprofile : TProfile;
    FredirectUri : String;
    Fwebproperty : TWebproperty;
  Protected
    //Property setters
    Procedure Setaccount(AIndex : Integer; AValue : TAccount); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setprofile(AIndex : Integer; AValue : TProfile); virtual;
    Procedure SetredirectUri(AIndex : Integer; AValue : String); virtual;
    Procedure Setwebproperty(AIndex : Integer; AValue : TWebproperty); virtual;
  Public
  Published
    Property account : TAccount Index 0 Read Faccount Write Setaccount;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property profile : TProfile Index 24 Read Fprofile Write Setprofile;
    Property redirectUri : String Index 32 Read FredirectUri Write SetredirectUri;
    Property webproperty : TWebproperty Index 40 Read Fwebproperty Write Setwebproperty;
  end;
  TAccountTicketClass = Class of TAccountTicket;
  
  { --------------------------------------------------------------------
    TAccounts
    --------------------------------------------------------------------}
  
  TAccounts = Class(TGoogleBaseObject)
  Private
    Fitems : TAccountsTypeitemsArray;
    FitemsPerPage : integer;
    Fkind : String;
    FnextLink : String;
    FpreviousLink : String;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TAccountsTypeitemsArray); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TAccountsTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextLink : String Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : String Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : String Index 56 Read Fusername Write Setusername;
  end;
  TAccountsClass = Class of TAccounts;
  
  { --------------------------------------------------------------------
    TAdWordsAccount
    --------------------------------------------------------------------}
  
  TAdWordsAccount = Class(TGoogleBaseObject)
  Private
    FautoTaggingEnabled : boolean;
    FcustomerId : String;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetautoTaggingEnabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcustomerId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property autoTaggingEnabled : boolean Index 0 Read FautoTaggingEnabled Write SetautoTaggingEnabled;
    Property customerId : String Index 8 Read FcustomerId Write SetcustomerId;
    Property kind : String Index 16 Read Fkind Write Setkind;
  end;
  TAdWordsAccountClass = Class of TAdWordsAccount;
  
  { --------------------------------------------------------------------
    TAnalyticsDataimportDeleteUploadDataRequest
    --------------------------------------------------------------------}
  
  TAnalyticsDataimportDeleteUploadDataRequest = Class(TGoogleBaseObject)
  Private
    FcustomDataImportUids : TStringArray;
  Protected
    //Property setters
    Procedure SetcustomDataImportUids(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property customDataImportUids : TStringArray Index 0 Read FcustomDataImportUids Write SetcustomDataImportUids;
  end;
  TAnalyticsDataimportDeleteUploadDataRequestClass = Class of TAnalyticsDataimportDeleteUploadDataRequest;
  
  { --------------------------------------------------------------------
    TColumnTypeattributes
    --------------------------------------------------------------------}
  
  TColumnTypeattributes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TColumnTypeattributesClass = Class of TColumnTypeattributes;
  
  { --------------------------------------------------------------------
    TColumn
    --------------------------------------------------------------------}
  
  TColumn = Class(TGoogleBaseObject)
  Private
    Fattributes : TColumnTypeattributes;
    Fid : String;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setattributes(AIndex : Integer; AValue : TColumnTypeattributes); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property attributes : TColumnTypeattributes Index 0 Read Fattributes Write Setattributes;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
  end;
  TColumnClass = Class of TColumn;
  
  { --------------------------------------------------------------------
    TColumns
    --------------------------------------------------------------------}
  
  TColumns = Class(TGoogleBaseObject)
  Private
    FattributeNames : TStringArray;
    Fetag : String;
    Fitems : TColumnsTypeitemsArray;
    Fkind : String;
    FtotalResults : integer;
  Protected
    //Property setters
    Procedure SetattributeNames(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TColumnsTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property attributeNames : TStringArray Index 0 Read FattributeNames Write SetattributeNames;
    Property etag : String Index 8 Read Fetag Write Setetag;
    Property items : TColumnsTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property totalResults : integer Index 32 Read FtotalResults Write SettotalResults;
  end;
  TColumnsClass = Class of TColumns;
  
  { --------------------------------------------------------------------
    TCustomDataSourceTypechildLink
    --------------------------------------------------------------------}
  
  TCustomDataSourceTypechildLink = Class(TGoogleBaseObject)
  Private
    Fhref : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property href : String Index 0 Read Fhref Write Sethref;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TCustomDataSourceTypechildLinkClass = Class of TCustomDataSourceTypechildLink;
  
  { --------------------------------------------------------------------
    TCustomDataSourceTypeparentLink
    --------------------------------------------------------------------}
  
  TCustomDataSourceTypeparentLink = Class(TGoogleBaseObject)
  Private
    Fhref : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property href : String Index 0 Read Fhref Write Sethref;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TCustomDataSourceTypeparentLinkClass = Class of TCustomDataSourceTypeparentLink;
  
  { --------------------------------------------------------------------
    TCustomDataSource
    --------------------------------------------------------------------}
  
  TCustomDataSource = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FchildLink : TCustomDataSourceTypechildLink;
    Fcreated : TDatetime;
    Fdescription : String;
    Fid : String;
    FimportBehavior : String;
    Fkind : String;
    Fname : String;
    FparentLink : TCustomDataSourceTypeparentLink;
    FprofilesLinked : TStringArray;
    FselfLink : String;
    F_type : String;
    Fupdated : TDatetime;
    FuploadType : String;
    FwebPropertyId : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetchildLink(AIndex : Integer; AValue : TCustomDataSourceTypechildLink); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetimportBehavior(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetparentLink(AIndex : Integer; AValue : TCustomDataSourceTypeparentLink); virtual;
    Procedure SetprofilesLinked(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetuploadType(AIndex : Integer; AValue : String); virtual;
    Procedure SetwebPropertyId(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property childLink : TCustomDataSourceTypechildLink Index 8 Read FchildLink Write SetchildLink;
    Property created : TDatetime Index 16 Read Fcreated Write Setcreated;
    Property description : String Index 24 Read Fdescription Write Setdescription;
    Property id : String Index 32 Read Fid Write Setid;
    Property importBehavior : String Index 40 Read FimportBehavior Write SetimportBehavior;
    Property kind : String Index 48 Read Fkind Write Setkind;
    Property name : String Index 56 Read Fname Write Setname;
    Property parentLink : TCustomDataSourceTypeparentLink Index 64 Read FparentLink Write SetparentLink;
    Property profilesLinked : TStringArray Index 72 Read FprofilesLinked Write SetprofilesLinked;
    Property selfLink : String Index 80 Read FselfLink Write SetselfLink;
    Property _type : String Index 88 Read F_type Write Set_type;
    Property updated : TDatetime Index 96 Read Fupdated Write Setupdated;
    Property uploadType : String Index 104 Read FuploadType Write SetuploadType;
    Property webPropertyId : String Index 112 Read FwebPropertyId Write SetwebPropertyId;
  end;
  TCustomDataSourceClass = Class of TCustomDataSource;
  
  { --------------------------------------------------------------------
    TCustomDataSources
    --------------------------------------------------------------------}
  
  TCustomDataSources = Class(TGoogleBaseObject)
  Private
    Fitems : TCustomDataSourcesTypeitemsArray;
    FitemsPerPage : integer;
    Fkind : String;
    FnextLink : String;
    FpreviousLink : String;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TCustomDataSourcesTypeitemsArray); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TCustomDataSourcesTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextLink : String Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : String Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : String Index 56 Read Fusername Write Setusername;
  end;
  TCustomDataSourcesClass = Class of TCustomDataSources;
  
  { --------------------------------------------------------------------
    TCustomDimensionTypeparentLink
    --------------------------------------------------------------------}
  
  TCustomDimensionTypeparentLink = Class(TGoogleBaseObject)
  Private
    Fhref : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property href : String Index 0 Read Fhref Write Sethref;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TCustomDimensionTypeparentLinkClass = Class of TCustomDimensionTypeparentLink;
  
  { --------------------------------------------------------------------
    TCustomDimension
    --------------------------------------------------------------------}
  
  TCustomDimension = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Factive : boolean;
    Fcreated : TDatetime;
    Fid : String;
    Findex : integer;
    Fkind : String;
    Fname : String;
    FparentLink : TCustomDimensionTypeparentLink;
    Fscope : String;
    FselfLink : String;
    Fupdated : TDatetime;
    FwebPropertyId : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Setactive(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setindex(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetparentLink(AIndex : Integer; AValue : TCustomDimensionTypeparentLink); virtual;
    Procedure Setscope(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetwebPropertyId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property active : boolean Index 8 Read Factive Write Setactive;
    Property created : TDatetime Index 16 Read Fcreated Write Setcreated;
    Property id : String Index 24 Read Fid Write Setid;
    Property index : integer Index 32 Read Findex Write Setindex;
    Property kind : String Index 40 Read Fkind Write Setkind;
    Property name : String Index 48 Read Fname Write Setname;
    Property parentLink : TCustomDimensionTypeparentLink Index 56 Read FparentLink Write SetparentLink;
    Property scope : String Index 64 Read Fscope Write Setscope;
    Property selfLink : String Index 72 Read FselfLink Write SetselfLink;
    Property updated : TDatetime Index 80 Read Fupdated Write Setupdated;
    Property webPropertyId : String Index 88 Read FwebPropertyId Write SetwebPropertyId;
  end;
  TCustomDimensionClass = Class of TCustomDimension;
  
  { --------------------------------------------------------------------
    TCustomDimensions
    --------------------------------------------------------------------}
  
  TCustomDimensions = Class(TGoogleBaseObject)
  Private
    Fitems : TCustomDimensionsTypeitemsArray;
    FitemsPerPage : integer;
    Fkind : String;
    FnextLink : String;
    FpreviousLink : String;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TCustomDimensionsTypeitemsArray); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TCustomDimensionsTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextLink : String Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : String Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : String Index 56 Read Fusername Write Setusername;
  end;
  TCustomDimensionsClass = Class of TCustomDimensions;
  
  { --------------------------------------------------------------------
    TCustomMetricTypeparentLink
    --------------------------------------------------------------------}
  
  TCustomMetricTypeparentLink = Class(TGoogleBaseObject)
  Private
    Fhref : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property href : String Index 0 Read Fhref Write Sethref;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TCustomMetricTypeparentLinkClass = Class of TCustomMetricTypeparentLink;
  
  { --------------------------------------------------------------------
    TCustomMetric
    --------------------------------------------------------------------}
  
  TCustomMetric = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Factive : boolean;
    Fcreated : TDatetime;
    Fid : String;
    Findex : integer;
    Fkind : String;
    Fmax_value : String;
    Fmin_value : String;
    Fname : String;
    FparentLink : TCustomMetricTypeparentLink;
    Fscope : String;
    FselfLink : String;
    F_type : String;
    Fupdated : TDatetime;
    FwebPropertyId : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Setactive(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setindex(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setmax_value(AIndex : Integer; AValue : String); virtual;
    Procedure Setmin_value(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetparentLink(AIndex : Integer; AValue : TCustomMetricTypeparentLink); virtual;
    Procedure Setscope(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetwebPropertyId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property active : boolean Index 8 Read Factive Write Setactive;
    Property created : TDatetime Index 16 Read Fcreated Write Setcreated;
    Property id : String Index 24 Read Fid Write Setid;
    Property index : integer Index 32 Read Findex Write Setindex;
    Property kind : String Index 40 Read Fkind Write Setkind;
    Property max_value : String Index 48 Read Fmax_value Write Setmax_value;
    Property min_value : String Index 56 Read Fmin_value Write Setmin_value;
    Property name : String Index 64 Read Fname Write Setname;
    Property parentLink : TCustomMetricTypeparentLink Index 72 Read FparentLink Write SetparentLink;
    Property scope : String Index 80 Read Fscope Write Setscope;
    Property selfLink : String Index 88 Read FselfLink Write SetselfLink;
    Property _type : String Index 96 Read F_type Write Set_type;
    Property updated : TDatetime Index 104 Read Fupdated Write Setupdated;
    Property webPropertyId : String Index 112 Read FwebPropertyId Write SetwebPropertyId;
  end;
  TCustomMetricClass = Class of TCustomMetric;
  
  { --------------------------------------------------------------------
    TCustomMetrics
    --------------------------------------------------------------------}
  
  TCustomMetrics = Class(TGoogleBaseObject)
  Private
    Fitems : TCustomMetricsTypeitemsArray;
    FitemsPerPage : integer;
    Fkind : String;
    FnextLink : String;
    FpreviousLink : String;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TCustomMetricsTypeitemsArray); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TCustomMetricsTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextLink : String Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : String Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : String Index 56 Read Fusername Write Setusername;
  end;
  TCustomMetricsClass = Class of TCustomMetrics;
  
  { --------------------------------------------------------------------
    TEntityAdWordsLinkTypeentity
    --------------------------------------------------------------------}
  
  TEntityAdWordsLinkTypeentity = Class(TGoogleBaseObject)
  Private
    FwebPropertyRef : TWebPropertyRef;
  Protected
    //Property setters
    Procedure SetwebPropertyRef(AIndex : Integer; AValue : TWebPropertyRef); virtual;
  Public
  Published
    Property webPropertyRef : TWebPropertyRef Index 0 Read FwebPropertyRef Write SetwebPropertyRef;
  end;
  TEntityAdWordsLinkTypeentityClass = Class of TEntityAdWordsLinkTypeentity;
  
  { --------------------------------------------------------------------
    TEntityAdWordsLink
    --------------------------------------------------------------------}
  
  TEntityAdWordsLink = Class(TGoogleBaseObject)
  Private
    FadWordsAccounts : TEntityAdWordsLinkTypeadWordsAccountsArray;
    Fentity : TEntityAdWordsLinkTypeentity;
    Fid : String;
    Fkind : String;
    Fname : String;
    FprofileIds : TStringArray;
    FselfLink : String;
  Protected
    //Property setters
    Procedure SetadWordsAccounts(AIndex : Integer; AValue : TEntityAdWordsLinkTypeadWordsAccountsArray); virtual;
    Procedure Setentity(AIndex : Integer; AValue : TEntityAdWordsLinkTypeentity); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetprofileIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property adWordsAccounts : TEntityAdWordsLinkTypeadWordsAccountsArray Index 0 Read FadWordsAccounts Write SetadWordsAccounts;
    Property entity : TEntityAdWordsLinkTypeentity Index 8 Read Fentity Write Setentity;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property name : String Index 32 Read Fname Write Setname;
    Property profileIds : TStringArray Index 40 Read FprofileIds Write SetprofileIds;
    Property selfLink : String Index 48 Read FselfLink Write SetselfLink;
  end;
  TEntityAdWordsLinkClass = Class of TEntityAdWordsLink;
  
  { --------------------------------------------------------------------
    TEntityAdWordsLinks
    --------------------------------------------------------------------}
  
  TEntityAdWordsLinks = Class(TGoogleBaseObject)
  Private
    Fitems : TEntityAdWordsLinksTypeitemsArray;
    FitemsPerPage : integer;
    Fkind : String;
    FnextLink : String;
    FpreviousLink : String;
    FstartIndex : integer;
    FtotalResults : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TEntityAdWordsLinksTypeitemsArray); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TEntityAdWordsLinksTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextLink : String Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : String Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
  end;
  TEntityAdWordsLinksClass = Class of TEntityAdWordsLinks;
  
  { --------------------------------------------------------------------
    TEntityUserLinkTypeentity
    --------------------------------------------------------------------}
  
  TEntityUserLinkTypeentity = Class(TGoogleBaseObject)
  Private
    FaccountRef : TAccountRef;
    FprofileRef : TProfileRef;
    FwebPropertyRef : TWebPropertyRef;
  Protected
    //Property setters
    Procedure SetaccountRef(AIndex : Integer; AValue : TAccountRef); virtual;
    Procedure SetprofileRef(AIndex : Integer; AValue : TProfileRef); virtual;
    Procedure SetwebPropertyRef(AIndex : Integer; AValue : TWebPropertyRef); virtual;
  Public
  Published
    Property accountRef : TAccountRef Index 0 Read FaccountRef Write SetaccountRef;
    Property profileRef : TProfileRef Index 8 Read FprofileRef Write SetprofileRef;
    Property webPropertyRef : TWebPropertyRef Index 16 Read FwebPropertyRef Write SetwebPropertyRef;
  end;
  TEntityUserLinkTypeentityClass = Class of TEntityUserLinkTypeentity;
  
  { --------------------------------------------------------------------
    TEntityUserLinkTypepermissions
    --------------------------------------------------------------------}
  
  TEntityUserLinkTypepermissions = Class(TGoogleBaseObject)
  Private
    Feffective : TStringArray;
    Flocal : TStringArray;
  Protected
    //Property setters
    Procedure Seteffective(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setlocal(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property effective : TStringArray Index 0 Read Feffective Write Seteffective;
    Property local : TStringArray Index 8 Read Flocal Write Setlocal;
  end;
  TEntityUserLinkTypepermissionsClass = Class of TEntityUserLinkTypepermissions;
  
  { --------------------------------------------------------------------
    TEntityUserLink
    --------------------------------------------------------------------}
  
  TEntityUserLink = Class(TGoogleBaseObject)
  Private
    Fentity : TEntityUserLinkTypeentity;
    Fid : String;
    Fkind : String;
    Fpermissions : TEntityUserLinkTypepermissions;
    FselfLink : String;
    FuserRef : TUserRef;
  Protected
    //Property setters
    Procedure Setentity(AIndex : Integer; AValue : TEntityUserLinkTypeentity); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setpermissions(AIndex : Integer; AValue : TEntityUserLinkTypepermissions); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetuserRef(AIndex : Integer; AValue : TUserRef); virtual;
  Public
  Published
    Property entity : TEntityUserLinkTypeentity Index 0 Read Fentity Write Setentity;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property permissions : TEntityUserLinkTypepermissions Index 24 Read Fpermissions Write Setpermissions;
    Property selfLink : String Index 32 Read FselfLink Write SetselfLink;
    Property userRef : TUserRef Index 40 Read FuserRef Write SetuserRef;
  end;
  TEntityUserLinkClass = Class of TEntityUserLink;
  
  { --------------------------------------------------------------------
    TEntityUserLinks
    --------------------------------------------------------------------}
  
  TEntityUserLinks = Class(TGoogleBaseObject)
  Private
    Fitems : TEntityUserLinksTypeitemsArray;
    FitemsPerPage : integer;
    Fkind : String;
    FnextLink : String;
    FpreviousLink : String;
    FstartIndex : integer;
    FtotalResults : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TEntityUserLinksTypeitemsArray); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TEntityUserLinksTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextLink : String Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : String Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
  end;
  TEntityUserLinksClass = Class of TEntityUserLinks;
  
  { --------------------------------------------------------------------
    TExperimentTypeparentLink
    --------------------------------------------------------------------}
  
  TExperimentTypeparentLink = Class(TGoogleBaseObject)
  Private
    Fhref : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property href : String Index 0 Read Fhref Write Sethref;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TExperimentTypeparentLinkClass = Class of TExperimentTypeparentLink;
  
  { --------------------------------------------------------------------
    TExperimentTypevariationsItem
    --------------------------------------------------------------------}
  
  TExperimentTypevariationsItem = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fstatus : String;
    Furl : String;
    Fweight : double;
    Fwon : boolean;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
    Procedure Setweight(AIndex : Integer; AValue : double); virtual;
    Procedure Setwon(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property status : String Index 8 Read Fstatus Write Setstatus;
    Property url : String Index 16 Read Furl Write Seturl;
    Property weight : double Index 24 Read Fweight Write Setweight;
    Property won : boolean Index 32 Read Fwon Write Setwon;
  end;
  TExperimentTypevariationsItemClass = Class of TExperimentTypevariationsItem;
  
  { --------------------------------------------------------------------
    TExperiment
    --------------------------------------------------------------------}
  
  TExperiment = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Fcreated : TDatetime;
    Fdescription : String;
    FeditableInGaUi : boolean;
    FendTime : TDatetime;
    FequalWeighting : boolean;
    Fid : String;
    FinternalWebPropertyId : String;
    Fkind : String;
    FminimumExperimentLengthInDays : integer;
    Fname : String;
    FobjectiveMetric : String;
    FoptimizationType : String;
    FparentLink : TExperimentTypeparentLink;
    FprofileId : String;
    FreasonExperimentEnded : String;
    FrewriteVariationUrlsAsOriginal : boolean;
    FselfLink : String;
    FservingFramework : String;
    Fsnippet : String;
    FstartTime : TDatetime;
    Fstatus : String;
    FtrafficCoverage : double;
    Fupdated : TDatetime;
    Fvariations : TExperimentTypevariationsArray;
    FwebPropertyId : String;
    FwinnerConfidenceLevel : double;
    FwinnerFound : boolean;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SeteditableInGaUi(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetendTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetequalWeighting(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetinternalWebPropertyId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetminimumExperimentLengthInDays(AIndex : Integer; AValue : integer); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetobjectiveMetric(AIndex : Integer; AValue : String); virtual;
    Procedure SetoptimizationType(AIndex : Integer; AValue : String); virtual;
    Procedure SetparentLink(AIndex : Integer; AValue : TExperimentTypeparentLink); virtual;
    Procedure SetprofileId(AIndex : Integer; AValue : String); virtual;
    Procedure SetreasonExperimentEnded(AIndex : Integer; AValue : String); virtual;
    Procedure SetrewriteVariationUrlsAsOriginal(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetservingFramework(AIndex : Integer; AValue : String); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure SettrafficCoverage(AIndex : Integer; AValue : double); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setvariations(AIndex : Integer; AValue : TExperimentTypevariationsArray); virtual;
    Procedure SetwebPropertyId(AIndex : Integer; AValue : String); virtual;
    Procedure SetwinnerConfidenceLevel(AIndex : Integer; AValue : double); virtual;
    Procedure SetwinnerFound(AIndex : Integer; AValue : boolean); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property created : TDatetime Index 8 Read Fcreated Write Setcreated;
    Property description : String Index 16 Read Fdescription Write Setdescription;
    Property editableInGaUi : boolean Index 24 Read FeditableInGaUi Write SeteditableInGaUi;
    Property endTime : TDatetime Index 32 Read FendTime Write SetendTime;
    Property equalWeighting : boolean Index 40 Read FequalWeighting Write SetequalWeighting;
    Property id : String Index 48 Read Fid Write Setid;
    Property internalWebPropertyId : String Index 56 Read FinternalWebPropertyId Write SetinternalWebPropertyId;
    Property kind : String Index 64 Read Fkind Write Setkind;
    Property minimumExperimentLengthInDays : integer Index 72 Read FminimumExperimentLengthInDays Write SetminimumExperimentLengthInDays;
    Property name : String Index 80 Read Fname Write Setname;
    Property objectiveMetric : String Index 88 Read FobjectiveMetric Write SetobjectiveMetric;
    Property optimizationType : String Index 96 Read FoptimizationType Write SetoptimizationType;
    Property parentLink : TExperimentTypeparentLink Index 104 Read FparentLink Write SetparentLink;
    Property profileId : String Index 112 Read FprofileId Write SetprofileId;
    Property reasonExperimentEnded : String Index 120 Read FreasonExperimentEnded Write SetreasonExperimentEnded;
    Property rewriteVariationUrlsAsOriginal : boolean Index 128 Read FrewriteVariationUrlsAsOriginal Write SetrewriteVariationUrlsAsOriginal;
    Property selfLink : String Index 136 Read FselfLink Write SetselfLink;
    Property servingFramework : String Index 144 Read FservingFramework Write SetservingFramework;
    Property snippet : String Index 152 Read Fsnippet Write Setsnippet;
    Property startTime : TDatetime Index 160 Read FstartTime Write SetstartTime;
    Property status : String Index 168 Read Fstatus Write Setstatus;
    Property trafficCoverage : double Index 176 Read FtrafficCoverage Write SettrafficCoverage;
    Property updated : TDatetime Index 184 Read Fupdated Write Setupdated;
    Property variations : TExperimentTypevariationsArray Index 192 Read Fvariations Write Setvariations;
    Property webPropertyId : String Index 200 Read FwebPropertyId Write SetwebPropertyId;
    Property winnerConfidenceLevel : double Index 208 Read FwinnerConfidenceLevel Write SetwinnerConfidenceLevel;
    Property winnerFound : boolean Index 216 Read FwinnerFound Write SetwinnerFound;
  end;
  TExperimentClass = Class of TExperiment;
  
  { --------------------------------------------------------------------
    TExperiments
    --------------------------------------------------------------------}
  
  TExperiments = Class(TGoogleBaseObject)
  Private
    Fitems : TExperimentsTypeitemsArray;
    FitemsPerPage : integer;
    Fkind : String;
    FnextLink : String;
    FpreviousLink : String;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TExperimentsTypeitemsArray); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TExperimentsTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextLink : String Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : String Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : String Index 56 Read Fusername Write Setusername;
  end;
  TExperimentsClass = Class of TExperiments;
  
  { --------------------------------------------------------------------
    TFilterTypeadvancedDetails
    --------------------------------------------------------------------}
  
  TFilterTypeadvancedDetails = Class(TGoogleBaseObject)
  Private
    FcaseSensitive : boolean;
    FextractA : String;
    FextractB : String;
    FfieldA : String;
    FfieldAIndex : integer;
    FfieldARequired : boolean;
    FfieldB : String;
    FfieldBIndex : integer;
    FfieldBRequired : boolean;
    FoutputConstructor : String;
    FoutputToField : String;
    FoutputToFieldIndex : integer;
    FoverrideOutputField : boolean;
  Protected
    //Property setters
    Procedure SetcaseSensitive(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetextractA(AIndex : Integer; AValue : String); virtual;
    Procedure SetextractB(AIndex : Integer; AValue : String); virtual;
    Procedure SetfieldA(AIndex : Integer; AValue : String); virtual;
    Procedure SetfieldAIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SetfieldARequired(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetfieldB(AIndex : Integer; AValue : String); virtual;
    Procedure SetfieldBIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SetfieldBRequired(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetoutputConstructor(AIndex : Integer; AValue : String); virtual;
    Procedure SetoutputToField(AIndex : Integer; AValue : String); virtual;
    Procedure SetoutputToFieldIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SetoverrideOutputField(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property caseSensitive : boolean Index 0 Read FcaseSensitive Write SetcaseSensitive;
    Property extractA : String Index 8 Read FextractA Write SetextractA;
    Property extractB : String Index 16 Read FextractB Write SetextractB;
    Property fieldA : String Index 24 Read FfieldA Write SetfieldA;
    Property fieldAIndex : integer Index 32 Read FfieldAIndex Write SetfieldAIndex;
    Property fieldARequired : boolean Index 40 Read FfieldARequired Write SetfieldARequired;
    Property fieldB : String Index 48 Read FfieldB Write SetfieldB;
    Property fieldBIndex : integer Index 56 Read FfieldBIndex Write SetfieldBIndex;
    Property fieldBRequired : boolean Index 64 Read FfieldBRequired Write SetfieldBRequired;
    Property outputConstructor : String Index 72 Read FoutputConstructor Write SetoutputConstructor;
    Property outputToField : String Index 80 Read FoutputToField Write SetoutputToField;
    Property outputToFieldIndex : integer Index 88 Read FoutputToFieldIndex Write SetoutputToFieldIndex;
    Property overrideOutputField : boolean Index 96 Read FoverrideOutputField Write SetoverrideOutputField;
  end;
  TFilterTypeadvancedDetailsClass = Class of TFilterTypeadvancedDetails;
  
  { --------------------------------------------------------------------
    TFilterTypelowercaseDetails
    --------------------------------------------------------------------}
  
  TFilterTypelowercaseDetails = Class(TGoogleBaseObject)
  Private
    Ffield : String;
    FfieldIndex : integer;
  Protected
    //Property setters
    Procedure Setfield(AIndex : Integer; AValue : String); virtual;
    Procedure SetfieldIndex(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property field : String Index 0 Read Ffield Write Setfield;
    Property fieldIndex : integer Index 8 Read FfieldIndex Write SetfieldIndex;
  end;
  TFilterTypelowercaseDetailsClass = Class of TFilterTypelowercaseDetails;
  
  { --------------------------------------------------------------------
    TFilterTypeparentLink
    --------------------------------------------------------------------}
  
  TFilterTypeparentLink = Class(TGoogleBaseObject)
  Private
    Fhref : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property href : String Index 0 Read Fhref Write Sethref;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TFilterTypeparentLinkClass = Class of TFilterTypeparentLink;
  
  { --------------------------------------------------------------------
    TFilterTypesearchAndReplaceDetails
    --------------------------------------------------------------------}
  
  TFilterTypesearchAndReplaceDetails = Class(TGoogleBaseObject)
  Private
    FcaseSensitive : boolean;
    Ffield : String;
    FfieldIndex : integer;
    FreplaceString : String;
    FsearchString : String;
  Protected
    //Property setters
    Procedure SetcaseSensitive(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setfield(AIndex : Integer; AValue : String); virtual;
    Procedure SetfieldIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SetreplaceString(AIndex : Integer; AValue : String); virtual;
    Procedure SetsearchString(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property caseSensitive : boolean Index 0 Read FcaseSensitive Write SetcaseSensitive;
    Property field : String Index 8 Read Ffield Write Setfield;
    Property fieldIndex : integer Index 16 Read FfieldIndex Write SetfieldIndex;
    Property replaceString : String Index 24 Read FreplaceString Write SetreplaceString;
    Property searchString : String Index 32 Read FsearchString Write SetsearchString;
  end;
  TFilterTypesearchAndReplaceDetailsClass = Class of TFilterTypesearchAndReplaceDetails;
  
  { --------------------------------------------------------------------
    TFilterTypeuppercaseDetails
    --------------------------------------------------------------------}
  
  TFilterTypeuppercaseDetails = Class(TGoogleBaseObject)
  Private
    Ffield : String;
    FfieldIndex : integer;
  Protected
    //Property setters
    Procedure Setfield(AIndex : Integer; AValue : String); virtual;
    Procedure SetfieldIndex(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property field : String Index 0 Read Ffield Write Setfield;
    Property fieldIndex : integer Index 8 Read FfieldIndex Write SetfieldIndex;
  end;
  TFilterTypeuppercaseDetailsClass = Class of TFilterTypeuppercaseDetails;
  
  { --------------------------------------------------------------------
    TFilter
    --------------------------------------------------------------------}
  
  TFilter = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FadvancedDetails : TFilterTypeadvancedDetails;
    Fcreated : TDatetime;
    FexcludeDetails : TFilterExpression;
    Fid : String;
    FincludeDetails : TFilterExpression;
    Fkind : String;
    FlowercaseDetails : TFilterTypelowercaseDetails;
    Fname : String;
    FparentLink : TFilterTypeparentLink;
    FsearchAndReplaceDetails : TFilterTypesearchAndReplaceDetails;
    FselfLink : String;
    F_type : String;
    Fupdated : TDatetime;
    FuppercaseDetails : TFilterTypeuppercaseDetails;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetadvancedDetails(AIndex : Integer; AValue : TFilterTypeadvancedDetails); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetexcludeDetails(AIndex : Integer; AValue : TFilterExpression); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetincludeDetails(AIndex : Integer; AValue : TFilterExpression); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlowercaseDetails(AIndex : Integer; AValue : TFilterTypelowercaseDetails); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetparentLink(AIndex : Integer; AValue : TFilterTypeparentLink); virtual;
    Procedure SetsearchAndReplaceDetails(AIndex : Integer; AValue : TFilterTypesearchAndReplaceDetails); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetuppercaseDetails(AIndex : Integer; AValue : TFilterTypeuppercaseDetails); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property advancedDetails : TFilterTypeadvancedDetails Index 8 Read FadvancedDetails Write SetadvancedDetails;
    Property created : TDatetime Index 16 Read Fcreated Write Setcreated;
    Property excludeDetails : TFilterExpression Index 24 Read FexcludeDetails Write SetexcludeDetails;
    Property id : String Index 32 Read Fid Write Setid;
    Property includeDetails : TFilterExpression Index 40 Read FincludeDetails Write SetincludeDetails;
    Property kind : String Index 48 Read Fkind Write Setkind;
    Property lowercaseDetails : TFilterTypelowercaseDetails Index 56 Read FlowercaseDetails Write SetlowercaseDetails;
    Property name : String Index 64 Read Fname Write Setname;
    Property parentLink : TFilterTypeparentLink Index 72 Read FparentLink Write SetparentLink;
    Property searchAndReplaceDetails : TFilterTypesearchAndReplaceDetails Index 80 Read FsearchAndReplaceDetails Write SetsearchAndReplaceDetails;
    Property selfLink : String Index 88 Read FselfLink Write SetselfLink;
    Property _type : String Index 96 Read F_type Write Set_type;
    Property updated : TDatetime Index 104 Read Fupdated Write Setupdated;
    Property uppercaseDetails : TFilterTypeuppercaseDetails Index 112 Read FuppercaseDetails Write SetuppercaseDetails;
  end;
  TFilterClass = Class of TFilter;
  
  { --------------------------------------------------------------------
    TFilterExpression
    --------------------------------------------------------------------}
  
  TFilterExpression = Class(TGoogleBaseObject)
  Private
    FcaseSensitive : boolean;
    FexpressionValue : String;
    Ffield : String;
    FfieldIndex : integer;
    Fkind : String;
    FmatchType : String;
  Protected
    //Property setters
    Procedure SetcaseSensitive(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetexpressionValue(AIndex : Integer; AValue : String); virtual;
    Procedure Setfield(AIndex : Integer; AValue : String); virtual;
    Procedure SetfieldIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmatchType(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property caseSensitive : boolean Index 0 Read FcaseSensitive Write SetcaseSensitive;
    Property expressionValue : String Index 8 Read FexpressionValue Write SetexpressionValue;
    Property field : String Index 16 Read Ffield Write Setfield;
    Property fieldIndex : integer Index 24 Read FfieldIndex Write SetfieldIndex;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property matchType : String Index 40 Read FmatchType Write SetmatchType;
  end;
  TFilterExpressionClass = Class of TFilterExpression;
  
  { --------------------------------------------------------------------
    TFilterRef
    --------------------------------------------------------------------}
  
  TFilterRef = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Fhref : String;
    Fid : String;
    Fkind : String;
    Fname : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Sethref(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property href : String Index 8 Read Fhref Write Sethref;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property name : String Index 32 Read Fname Write Setname;
  end;
  TFilterRefClass = Class of TFilterRef;
  
  { --------------------------------------------------------------------
    TFilters
    --------------------------------------------------------------------}
  
  TFilters = Class(TGoogleBaseObject)
  Private
    Fitems : TFiltersTypeitemsArray;
    FitemsPerPage : integer;
    Fkind : String;
    FnextLink : String;
    FpreviousLink : String;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TFiltersTypeitemsArray); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TFiltersTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextLink : String Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : String Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : String Index 56 Read Fusername Write Setusername;
  end;
  TFiltersClass = Class of TFilters;
  
  { --------------------------------------------------------------------
    TGaDataTypecolumnHeadersItem
    --------------------------------------------------------------------}
  
  TGaDataTypecolumnHeadersItem = Class(TGoogleBaseObject)
  Private
    FcolumnType : String;
    FdataType : String;
    Fname : String;
  Protected
    //Property setters
    Procedure SetcolumnType(AIndex : Integer; AValue : String); virtual;
    Procedure SetdataType(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property columnType : String Index 0 Read FcolumnType Write SetcolumnType;
    Property dataType : String Index 8 Read FdataType Write SetdataType;
    Property name : String Index 16 Read Fname Write Setname;
  end;
  TGaDataTypecolumnHeadersItemClass = Class of TGaDataTypecolumnHeadersItem;
  
  { --------------------------------------------------------------------
    TGaDataTypedataTableTypecolsItem
    --------------------------------------------------------------------}
  
  TGaDataTypedataTableTypecolsItem = Class(TGoogleBaseObject)
  Private
    Fid : String;
    F_label : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Set_label(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property _label : String Index 8 Read F_label Write Set_label;
    Property _type : String Index 16 Read F_type Write Set_type;
  end;
  TGaDataTypedataTableTypecolsItemClass = Class of TGaDataTypedataTableTypecolsItem;
  
  { --------------------------------------------------------------------
    TGaDataTypedataTableTyperowsItemTypecItem
    --------------------------------------------------------------------}
  
  TGaDataTypedataTableTyperowsItemTypecItem = Class(TGoogleBaseObject)
  Private
    Fv : String;
  Protected
    //Property setters
    Procedure Setv(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property v : String Index 0 Read Fv Write Setv;
  end;
  TGaDataTypedataTableTyperowsItemTypecItemClass = Class of TGaDataTypedataTableTyperowsItemTypecItem;
  
  { --------------------------------------------------------------------
    TGaDataTypedataTableTyperowsItem
    --------------------------------------------------------------------}
  
  TGaDataTypedataTableTyperowsItem = Class(TGoogleBaseObject)
  Private
    Fc : TGaDataTypedataTableTyperowsItemTypecArray;
  Protected
    //Property setters
    Procedure Setc(AIndex : Integer; AValue : TGaDataTypedataTableTyperowsItemTypecArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property c : TGaDataTypedataTableTyperowsItemTypecArray Index 0 Read Fc Write Setc;
  end;
  TGaDataTypedataTableTyperowsItemClass = Class of TGaDataTypedataTableTyperowsItem;
  
  { --------------------------------------------------------------------
    TGaDataTypedataTable
    --------------------------------------------------------------------}
  
  TGaDataTypedataTable = Class(TGoogleBaseObject)
  Private
    Fcols : TGaDataTypedataTableTypecolsArray;
    Frows : TGaDataTypedataTableTyperowsArray;
  Protected
    //Property setters
    Procedure Setcols(AIndex : Integer; AValue : TGaDataTypedataTableTypecolsArray); virtual;
    Procedure Setrows(AIndex : Integer; AValue : TGaDataTypedataTableTyperowsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property cols : TGaDataTypedataTableTypecolsArray Index 0 Read Fcols Write Setcols;
    Property rows : TGaDataTypedataTableTyperowsArray Index 8 Read Frows Write Setrows;
  end;
  TGaDataTypedataTableClass = Class of TGaDataTypedataTable;
  
  { --------------------------------------------------------------------
    TGaDataTypeprofileInfo
    --------------------------------------------------------------------}
  
  TGaDataTypeprofileInfo = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FinternalWebPropertyId : String;
    FprofileId : String;
    FprofileName : String;
    FtableId : String;
    FwebPropertyId : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetinternalWebPropertyId(AIndex : Integer; AValue : String); virtual;
    Procedure SetprofileId(AIndex : Integer; AValue : String); virtual;
    Procedure SetprofileName(AIndex : Integer; AValue : String); virtual;
    Procedure SettableId(AIndex : Integer; AValue : String); virtual;
    Procedure SetwebPropertyId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property internalWebPropertyId : String Index 8 Read FinternalWebPropertyId Write SetinternalWebPropertyId;
    Property profileId : String Index 16 Read FprofileId Write SetprofileId;
    Property profileName : String Index 24 Read FprofileName Write SetprofileName;
    Property tableId : String Index 32 Read FtableId Write SettableId;
    Property webPropertyId : String Index 40 Read FwebPropertyId Write SetwebPropertyId;
  end;
  TGaDataTypeprofileInfoClass = Class of TGaDataTypeprofileInfo;
  
  { --------------------------------------------------------------------
    TGaDataTypequery
    --------------------------------------------------------------------}
  
  TGaDataTypequery = Class(TGoogleBaseObject)
  Private
    Fdimensions : String;
    Fenddate : String;
    Ffilters : String;
    Fids : String;
    Fmaxresults : integer;
    Fmetrics : TStringArray;
    FsamplingLevel : String;
    Fsegment : String;
    Fsort : TStringArray;
    Fstartdate : String;
    Fstartindex : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdimensions(AIndex : Integer; AValue : String); virtual;
    Procedure Setenddate(AIndex : Integer; AValue : String); virtual;
    Procedure Setfilters(AIndex : Integer; AValue : String); virtual;
    Procedure Setids(AIndex : Integer; AValue : String); virtual;
    Procedure Setmaxresults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setmetrics(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetsamplingLevel(AIndex : Integer; AValue : String); virtual;
    Procedure Setsegment(AIndex : Integer; AValue : String); virtual;
    Procedure Setsort(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setstartdate(AIndex : Integer; AValue : String); virtual;
    Procedure Setstartindex(AIndex : Integer; AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property dimensions : String Index 0 Read Fdimensions Write Setdimensions;
    Property enddate : String Index 8 Read Fenddate Write Setenddate;
    Property filters : String Index 16 Read Ffilters Write Setfilters;
    Property ids : String Index 24 Read Fids Write Setids;
    Property maxresults : integer Index 32 Read Fmaxresults Write Setmaxresults;
    Property metrics : TStringArray Index 40 Read Fmetrics Write Setmetrics;
    Property samplingLevel : String Index 48 Read FsamplingLevel Write SetsamplingLevel;
    Property segment : String Index 56 Read Fsegment Write Setsegment;
    Property sort : TStringArray Index 64 Read Fsort Write Setsort;
    Property startdate : String Index 72 Read Fstartdate Write Setstartdate;
    Property startindex : integer Index 80 Read Fstartindex Write Setstartindex;
  end;
  TGaDataTypequeryClass = Class of TGaDataTypequery;
  
  { --------------------------------------------------------------------
    TGaDataTypetotalsForAllResults
    --------------------------------------------------------------------}
  
  TGaDataTypetotalsForAllResults = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TGaDataTypetotalsForAllResultsClass = Class of TGaDataTypetotalsForAllResults;
  
  { --------------------------------------------------------------------
    TGaData
    --------------------------------------------------------------------}
  
  TGaData = Class(TGoogleBaseObject)
  Private
    FcolumnHeaders : TGaDataTypecolumnHeadersArray;
    FcontainsSampledData : boolean;
    FdataTable : TGaDataTypedataTable;
    Fid : String;
    FitemsPerPage : integer;
    Fkind : String;
    FnextLink : String;
    FpreviousLink : String;
    FprofileInfo : TGaDataTypeprofileInfo;
    Fquery : TGaDataTypequery;
    Frows : TGaDataTyperowsArray;
    FsampleSize : String;
    FsampleSpace : String;
    FselfLink : String;
    FtotalResults : integer;
    FtotalsForAllResults : TGaDataTypetotalsForAllResults;
  Protected
    //Property setters
    Procedure SetcolumnHeaders(AIndex : Integer; AValue : TGaDataTypecolumnHeadersArray); virtual;
    Procedure SetcontainsSampledData(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetdataTable(AIndex : Integer; AValue : TGaDataTypedataTable); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetprofileInfo(AIndex : Integer; AValue : TGaDataTypeprofileInfo); virtual;
    Procedure Setquery(AIndex : Integer; AValue : TGaDataTypequery); virtual;
    Procedure Setrows(AIndex : Integer; AValue : TGaDataTyperowsArray); virtual;
    Procedure SetsampleSize(AIndex : Integer; AValue : String); virtual;
    Procedure SetsampleSpace(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalsForAllResults(AIndex : Integer; AValue : TGaDataTypetotalsForAllResults); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property columnHeaders : TGaDataTypecolumnHeadersArray Index 0 Read FcolumnHeaders Write SetcolumnHeaders;
    Property containsSampledData : boolean Index 8 Read FcontainsSampledData Write SetcontainsSampledData;
    Property dataTable : TGaDataTypedataTable Index 16 Read FdataTable Write SetdataTable;
    Property id : String Index 24 Read Fid Write Setid;
    Property itemsPerPage : integer Index 32 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : String Index 40 Read Fkind Write Setkind;
    Property nextLink : String Index 48 Read FnextLink Write SetnextLink;
    Property previousLink : String Index 56 Read FpreviousLink Write SetpreviousLink;
    Property profileInfo : TGaDataTypeprofileInfo Index 64 Read FprofileInfo Write SetprofileInfo;
    Property query : TGaDataTypequery Index 72 Read Fquery Write Setquery;
    Property rows : TGaDataTyperowsArray Index 80 Read Frows Write Setrows;
    Property sampleSize : String Index 88 Read FsampleSize Write SetsampleSize;
    Property sampleSpace : String Index 96 Read FsampleSpace Write SetsampleSpace;
    Property selfLink : String Index 104 Read FselfLink Write SetselfLink;
    Property totalResults : integer Index 112 Read FtotalResults Write SettotalResults;
    Property totalsForAllResults : TGaDataTypetotalsForAllResults Index 120 Read FtotalsForAllResults Write SettotalsForAllResults;
  end;
  TGaDataClass = Class of TGaData;
  
  { --------------------------------------------------------------------
    TGoalTypeeventDetailsTypeeventConditionsItem
    --------------------------------------------------------------------}
  
  TGoalTypeeventDetailsTypeeventConditionsItem = Class(TGoogleBaseObject)
  Private
    FcomparisonType : String;
    FcomparisonValue : String;
    Fexpression : String;
    FmatchType : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetcomparisonType(AIndex : Integer; AValue : String); virtual;
    Procedure SetcomparisonValue(AIndex : Integer; AValue : String); virtual;
    Procedure Setexpression(AIndex : Integer; AValue : String); virtual;
    Procedure SetmatchType(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property comparisonType : String Index 0 Read FcomparisonType Write SetcomparisonType;
    Property comparisonValue : String Index 8 Read FcomparisonValue Write SetcomparisonValue;
    Property expression : String Index 16 Read Fexpression Write Setexpression;
    Property matchType : String Index 24 Read FmatchType Write SetmatchType;
    Property _type : String Index 32 Read F_type Write Set_type;
  end;
  TGoalTypeeventDetailsTypeeventConditionsItemClass = Class of TGoalTypeeventDetailsTypeeventConditionsItem;
  
  { --------------------------------------------------------------------
    TGoalTypeeventDetails
    --------------------------------------------------------------------}
  
  TGoalTypeeventDetails = Class(TGoogleBaseObject)
  Private
    FeventConditions : TGoalTypeeventDetailsTypeeventConditionsArray;
    FuseEventValue : boolean;
  Protected
    //Property setters
    Procedure SeteventConditions(AIndex : Integer; AValue : TGoalTypeeventDetailsTypeeventConditionsArray); virtual;
    Procedure SetuseEventValue(AIndex : Integer; AValue : boolean); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property eventConditions : TGoalTypeeventDetailsTypeeventConditionsArray Index 0 Read FeventConditions Write SeteventConditions;
    Property useEventValue : boolean Index 8 Read FuseEventValue Write SetuseEventValue;
  end;
  TGoalTypeeventDetailsClass = Class of TGoalTypeeventDetails;
  
  { --------------------------------------------------------------------
    TGoalTypeparentLink
    --------------------------------------------------------------------}
  
  TGoalTypeparentLink = Class(TGoogleBaseObject)
  Private
    Fhref : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property href : String Index 0 Read Fhref Write Sethref;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TGoalTypeparentLinkClass = Class of TGoalTypeparentLink;
  
  { --------------------------------------------------------------------
    TGoalTypeurlDestinationDetailsTypestepsItem
    --------------------------------------------------------------------}
  
  TGoalTypeurlDestinationDetailsTypestepsItem = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fnumber : integer;
    Furl : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setnumber(AIndex : Integer; AValue : integer); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property number : integer Index 8 Read Fnumber Write Setnumber;
    Property url : String Index 16 Read Furl Write Seturl;
  end;
  TGoalTypeurlDestinationDetailsTypestepsItemClass = Class of TGoalTypeurlDestinationDetailsTypestepsItem;
  
  { --------------------------------------------------------------------
    TGoalTypeurlDestinationDetails
    --------------------------------------------------------------------}
  
  TGoalTypeurlDestinationDetails = Class(TGoogleBaseObject)
  Private
    FcaseSensitive : boolean;
    FfirstStepRequired : boolean;
    FmatchType : String;
    Fsteps : TGoalTypeurlDestinationDetailsTypestepsArray;
    Furl : String;
  Protected
    //Property setters
    Procedure SetcaseSensitive(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetfirstStepRequired(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetmatchType(AIndex : Integer; AValue : String); virtual;
    Procedure Setsteps(AIndex : Integer; AValue : TGoalTypeurlDestinationDetailsTypestepsArray); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property caseSensitive : boolean Index 0 Read FcaseSensitive Write SetcaseSensitive;
    Property firstStepRequired : boolean Index 8 Read FfirstStepRequired Write SetfirstStepRequired;
    Property matchType : String Index 16 Read FmatchType Write SetmatchType;
    Property steps : TGoalTypeurlDestinationDetailsTypestepsArray Index 24 Read Fsteps Write Setsteps;
    Property url : String Index 32 Read Furl Write Seturl;
  end;
  TGoalTypeurlDestinationDetailsClass = Class of TGoalTypeurlDestinationDetails;
  
  { --------------------------------------------------------------------
    TGoalTypevisitNumPagesDetails
    --------------------------------------------------------------------}
  
  TGoalTypevisitNumPagesDetails = Class(TGoogleBaseObject)
  Private
    FcomparisonType : String;
    FcomparisonValue : String;
  Protected
    //Property setters
    Procedure SetcomparisonType(AIndex : Integer; AValue : String); virtual;
    Procedure SetcomparisonValue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property comparisonType : String Index 0 Read FcomparisonType Write SetcomparisonType;
    Property comparisonValue : String Index 8 Read FcomparisonValue Write SetcomparisonValue;
  end;
  TGoalTypevisitNumPagesDetailsClass = Class of TGoalTypevisitNumPagesDetails;
  
  { --------------------------------------------------------------------
    TGoalTypevisitTimeOnSiteDetails
    --------------------------------------------------------------------}
  
  TGoalTypevisitTimeOnSiteDetails = Class(TGoogleBaseObject)
  Private
    FcomparisonType : String;
    FcomparisonValue : String;
  Protected
    //Property setters
    Procedure SetcomparisonType(AIndex : Integer; AValue : String); virtual;
    Procedure SetcomparisonValue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property comparisonType : String Index 0 Read FcomparisonType Write SetcomparisonType;
    Property comparisonValue : String Index 8 Read FcomparisonValue Write SetcomparisonValue;
  end;
  TGoalTypevisitTimeOnSiteDetailsClass = Class of TGoalTypevisitTimeOnSiteDetails;
  
  { --------------------------------------------------------------------
    TGoal
    --------------------------------------------------------------------}
  
  TGoal = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Factive : boolean;
    Fcreated : TDatetime;
    FeventDetails : TGoalTypeeventDetails;
    Fid : String;
    FinternalWebPropertyId : String;
    Fkind : String;
    Fname : String;
    FparentLink : TGoalTypeparentLink;
    FprofileId : String;
    FselfLink : String;
    F_type : String;
    Fupdated : TDatetime;
    FurlDestinationDetails : TGoalTypeurlDestinationDetails;
    Fvalue : integer;
    FvisitNumPagesDetails : TGoalTypevisitNumPagesDetails;
    FvisitTimeOnSiteDetails : TGoalTypevisitTimeOnSiteDetails;
    FwebPropertyId : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Setactive(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SeteventDetails(AIndex : Integer; AValue : TGoalTypeeventDetails); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetinternalWebPropertyId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetparentLink(AIndex : Integer; AValue : TGoalTypeparentLink); virtual;
    Procedure SetprofileId(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SeturlDestinationDetails(AIndex : Integer; AValue : TGoalTypeurlDestinationDetails); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : integer); virtual;
    Procedure SetvisitNumPagesDetails(AIndex : Integer; AValue : TGoalTypevisitNumPagesDetails); virtual;
    Procedure SetvisitTimeOnSiteDetails(AIndex : Integer; AValue : TGoalTypevisitTimeOnSiteDetails); virtual;
    Procedure SetwebPropertyId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property active : boolean Index 8 Read Factive Write Setactive;
    Property created : TDatetime Index 16 Read Fcreated Write Setcreated;
    Property eventDetails : TGoalTypeeventDetails Index 24 Read FeventDetails Write SeteventDetails;
    Property id : String Index 32 Read Fid Write Setid;
    Property internalWebPropertyId : String Index 40 Read FinternalWebPropertyId Write SetinternalWebPropertyId;
    Property kind : String Index 48 Read Fkind Write Setkind;
    Property name : String Index 56 Read Fname Write Setname;
    Property parentLink : TGoalTypeparentLink Index 64 Read FparentLink Write SetparentLink;
    Property profileId : String Index 72 Read FprofileId Write SetprofileId;
    Property selfLink : String Index 80 Read FselfLink Write SetselfLink;
    Property _type : String Index 88 Read F_type Write Set_type;
    Property updated : TDatetime Index 96 Read Fupdated Write Setupdated;
    Property urlDestinationDetails : TGoalTypeurlDestinationDetails Index 104 Read FurlDestinationDetails Write SeturlDestinationDetails;
    Property value : integer Index 112 Read Fvalue Write Setvalue;
    Property visitNumPagesDetails : TGoalTypevisitNumPagesDetails Index 120 Read FvisitNumPagesDetails Write SetvisitNumPagesDetails;
    Property visitTimeOnSiteDetails : TGoalTypevisitTimeOnSiteDetails Index 128 Read FvisitTimeOnSiteDetails Write SetvisitTimeOnSiteDetails;
    Property webPropertyId : String Index 136 Read FwebPropertyId Write SetwebPropertyId;
  end;
  TGoalClass = Class of TGoal;
  
  { --------------------------------------------------------------------
    TGoals
    --------------------------------------------------------------------}
  
  TGoals = Class(TGoogleBaseObject)
  Private
    Fitems : TGoalsTypeitemsArray;
    FitemsPerPage : integer;
    Fkind : String;
    FnextLink : String;
    FpreviousLink : String;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TGoalsTypeitemsArray); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TGoalsTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextLink : String Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : String Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : String Index 56 Read Fusername Write Setusername;
  end;
  TGoalsClass = Class of TGoals;
  
  { --------------------------------------------------------------------
    TMcfDataTypecolumnHeadersItem
    --------------------------------------------------------------------}
  
  TMcfDataTypecolumnHeadersItem = Class(TGoogleBaseObject)
  Private
    FcolumnType : String;
    FdataType : String;
    Fname : String;
  Protected
    //Property setters
    Procedure SetcolumnType(AIndex : Integer; AValue : String); virtual;
    Procedure SetdataType(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property columnType : String Index 0 Read FcolumnType Write SetcolumnType;
    Property dataType : String Index 8 Read FdataType Write SetdataType;
    Property name : String Index 16 Read Fname Write Setname;
  end;
  TMcfDataTypecolumnHeadersItemClass = Class of TMcfDataTypecolumnHeadersItem;
  
  { --------------------------------------------------------------------
    TMcfDataTypeprofileInfo
    --------------------------------------------------------------------}
  
  TMcfDataTypeprofileInfo = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FinternalWebPropertyId : String;
    FprofileId : String;
    FprofileName : String;
    FtableId : String;
    FwebPropertyId : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetinternalWebPropertyId(AIndex : Integer; AValue : String); virtual;
    Procedure SetprofileId(AIndex : Integer; AValue : String); virtual;
    Procedure SetprofileName(AIndex : Integer; AValue : String); virtual;
    Procedure SettableId(AIndex : Integer; AValue : String); virtual;
    Procedure SetwebPropertyId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property internalWebPropertyId : String Index 8 Read FinternalWebPropertyId Write SetinternalWebPropertyId;
    Property profileId : String Index 16 Read FprofileId Write SetprofileId;
    Property profileName : String Index 24 Read FprofileName Write SetprofileName;
    Property tableId : String Index 32 Read FtableId Write SettableId;
    Property webPropertyId : String Index 40 Read FwebPropertyId Write SetwebPropertyId;
  end;
  TMcfDataTypeprofileInfoClass = Class of TMcfDataTypeprofileInfo;
  
  { --------------------------------------------------------------------
    TMcfDataTypequery
    --------------------------------------------------------------------}
  
  TMcfDataTypequery = Class(TGoogleBaseObject)
  Private
    Fdimensions : String;
    Fenddate : String;
    Ffilters : String;
    Fids : String;
    Fmaxresults : integer;
    Fmetrics : TStringArray;
    FsamplingLevel : String;
    Fsegment : String;
    Fsort : TStringArray;
    Fstartdate : String;
    Fstartindex : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdimensions(AIndex : Integer; AValue : String); virtual;
    Procedure Setenddate(AIndex : Integer; AValue : String); virtual;
    Procedure Setfilters(AIndex : Integer; AValue : String); virtual;
    Procedure Setids(AIndex : Integer; AValue : String); virtual;
    Procedure Setmaxresults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setmetrics(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetsamplingLevel(AIndex : Integer; AValue : String); virtual;
    Procedure Setsegment(AIndex : Integer; AValue : String); virtual;
    Procedure Setsort(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setstartdate(AIndex : Integer; AValue : String); virtual;
    Procedure Setstartindex(AIndex : Integer; AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property dimensions : String Index 0 Read Fdimensions Write Setdimensions;
    Property enddate : String Index 8 Read Fenddate Write Setenddate;
    Property filters : String Index 16 Read Ffilters Write Setfilters;
    Property ids : String Index 24 Read Fids Write Setids;
    Property maxresults : integer Index 32 Read Fmaxresults Write Setmaxresults;
    Property metrics : TStringArray Index 40 Read Fmetrics Write Setmetrics;
    Property samplingLevel : String Index 48 Read FsamplingLevel Write SetsamplingLevel;
    Property segment : String Index 56 Read Fsegment Write Setsegment;
    Property sort : TStringArray Index 64 Read Fsort Write Setsort;
    Property startdate : String Index 72 Read Fstartdate Write Setstartdate;
    Property startindex : integer Index 80 Read Fstartindex Write Setstartindex;
  end;
  TMcfDataTypequeryClass = Class of TMcfDataTypequery;
  
  { --------------------------------------------------------------------
    TMcfDataTyperowsItemItemTypeconversionPathValueItem
    --------------------------------------------------------------------}
  
  TMcfDataTyperowsItemItemTypeconversionPathValueItem = Class(TGoogleBaseObject)
  Private
    FinteractionType : String;
    FnodeValue : String;
  Protected
    //Property setters
    Procedure SetinteractionType(AIndex : Integer; AValue : String); virtual;
    Procedure SetnodeValue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property interactionType : String Index 0 Read FinteractionType Write SetinteractionType;
    Property nodeValue : String Index 8 Read FnodeValue Write SetnodeValue;
  end;
  TMcfDataTyperowsItemItemTypeconversionPathValueItemClass = Class of TMcfDataTyperowsItemItemTypeconversionPathValueItem;
  
  { --------------------------------------------------------------------
    TMcfDataTyperowsItemItem
    --------------------------------------------------------------------}
  
  TMcfDataTyperowsItemItem = Class(TGoogleBaseObject)
  Private
    FconversionPathValue : TMcfDataTyperowsItemItemTypeconversionPathValueArray;
    FprimitiveValue : String;
  Protected
    //Property setters
    Procedure SetconversionPathValue(AIndex : Integer; AValue : TMcfDataTyperowsItemItemTypeconversionPathValueArray); virtual;
    Procedure SetprimitiveValue(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property conversionPathValue : TMcfDataTyperowsItemItemTypeconversionPathValueArray Index 0 Read FconversionPathValue Write SetconversionPathValue;
    Property primitiveValue : String Index 8 Read FprimitiveValue Write SetprimitiveValue;
  end;
  TMcfDataTyperowsItemItemClass = Class of TMcfDataTyperowsItemItem;
  
  { --------------------------------------------------------------------
    TMcfDataTypetotalsForAllResults
    --------------------------------------------------------------------}
  
  TMcfDataTypetotalsForAllResults = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TMcfDataTypetotalsForAllResultsClass = Class of TMcfDataTypetotalsForAllResults;
  
  { --------------------------------------------------------------------
    TMcfData
    --------------------------------------------------------------------}
  
  TMcfData = Class(TGoogleBaseObject)
  Private
    FcolumnHeaders : TMcfDataTypecolumnHeadersArray;
    FcontainsSampledData : boolean;
    Fid : String;
    FitemsPerPage : integer;
    Fkind : String;
    FnextLink : String;
    FpreviousLink : String;
    FprofileInfo : TMcfDataTypeprofileInfo;
    Fquery : TMcfDataTypequery;
    Frows : TMcfDataTyperowsArray;
    FsampleSize : String;
    FsampleSpace : String;
    FselfLink : String;
    FtotalResults : integer;
    FtotalsForAllResults : TMcfDataTypetotalsForAllResults;
  Protected
    //Property setters
    Procedure SetcolumnHeaders(AIndex : Integer; AValue : TMcfDataTypecolumnHeadersArray); virtual;
    Procedure SetcontainsSampledData(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetprofileInfo(AIndex : Integer; AValue : TMcfDataTypeprofileInfo); virtual;
    Procedure Setquery(AIndex : Integer; AValue : TMcfDataTypequery); virtual;
    Procedure Setrows(AIndex : Integer; AValue : TMcfDataTyperowsArray); virtual;
    Procedure SetsampleSize(AIndex : Integer; AValue : String); virtual;
    Procedure SetsampleSpace(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalsForAllResults(AIndex : Integer; AValue : TMcfDataTypetotalsForAllResults); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property columnHeaders : TMcfDataTypecolumnHeadersArray Index 0 Read FcolumnHeaders Write SetcolumnHeaders;
    Property containsSampledData : boolean Index 8 Read FcontainsSampledData Write SetcontainsSampledData;
    Property id : String Index 16 Read Fid Write Setid;
    Property itemsPerPage : integer Index 24 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property nextLink : String Index 40 Read FnextLink Write SetnextLink;
    Property previousLink : String Index 48 Read FpreviousLink Write SetpreviousLink;
    Property profileInfo : TMcfDataTypeprofileInfo Index 56 Read FprofileInfo Write SetprofileInfo;
    Property query : TMcfDataTypequery Index 64 Read Fquery Write Setquery;
    Property rows : TMcfDataTyperowsArray Index 72 Read Frows Write Setrows;
    Property sampleSize : String Index 80 Read FsampleSize Write SetsampleSize;
    Property sampleSpace : String Index 88 Read FsampleSpace Write SetsampleSpace;
    Property selfLink : String Index 96 Read FselfLink Write SetselfLink;
    Property totalResults : integer Index 104 Read FtotalResults Write SettotalResults;
    Property totalsForAllResults : TMcfDataTypetotalsForAllResults Index 112 Read FtotalsForAllResults Write SettotalsForAllResults;
  end;
  TMcfDataClass = Class of TMcfData;
  
  { --------------------------------------------------------------------
    TProfileTypechildLink
    --------------------------------------------------------------------}
  
  TProfileTypechildLink = Class(TGoogleBaseObject)
  Private
    Fhref : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property href : String Index 0 Read Fhref Write Sethref;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TProfileTypechildLinkClass = Class of TProfileTypechildLink;
  
  { --------------------------------------------------------------------
    TProfileTypeparentLink
    --------------------------------------------------------------------}
  
  TProfileTypeparentLink = Class(TGoogleBaseObject)
  Private
    Fhref : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property href : String Index 0 Read Fhref Write Sethref;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TProfileTypeparentLinkClass = Class of TProfileTypeparentLink;
  
  { --------------------------------------------------------------------
    TProfileTypepermissions
    --------------------------------------------------------------------}
  
  TProfileTypepermissions = Class(TGoogleBaseObject)
  Private
    Feffective : TStringArray;
  Protected
    //Property setters
    Procedure Seteffective(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property effective : TStringArray Index 0 Read Feffective Write Seteffective;
  end;
  TProfileTypepermissionsClass = Class of TProfileTypepermissions;
  
  { --------------------------------------------------------------------
    TProfile
    --------------------------------------------------------------------}
  
  TProfile = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FchildLink : TProfileTypechildLink;
    Fcreated : TDatetime;
    Fcurrency : String;
    FdefaultPage : String;
    FeCommerceTracking : boolean;
    FenhancedECommerceTracking : boolean;
    FexcludeQueryParameters : String;
    Fid : String;
    FinternalWebPropertyId : String;
    Fkind : String;
    Fname : String;
    FparentLink : TProfileTypeparentLink;
    Fpermissions : TProfileTypepermissions;
    FselfLink : String;
    FsiteSearchCategoryParameters : String;
    FsiteSearchQueryParameters : String;
    FstripSiteSearchCategoryParameters : boolean;
    FstripSiteSearchQueryParameters : boolean;
    Ftimezone : String;
    F_type : String;
    Fupdated : TDatetime;
    FwebPropertyId : String;
    FwebsiteUrl : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetchildLink(AIndex : Integer; AValue : TProfileTypechildLink); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setcurrency(AIndex : Integer; AValue : String); virtual;
    Procedure SetdefaultPage(AIndex : Integer; AValue : String); virtual;
    Procedure SeteCommerceTracking(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetenhancedECommerceTracking(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetexcludeQueryParameters(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetinternalWebPropertyId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetparentLink(AIndex : Integer; AValue : TProfileTypeparentLink); virtual;
    Procedure Setpermissions(AIndex : Integer; AValue : TProfileTypepermissions); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetsiteSearchCategoryParameters(AIndex : Integer; AValue : String); virtual;
    Procedure SetsiteSearchQueryParameters(AIndex : Integer; AValue : String); virtual;
    Procedure SetstripSiteSearchCategoryParameters(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetstripSiteSearchQueryParameters(AIndex : Integer; AValue : boolean); virtual;
    Procedure Settimezone(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetwebPropertyId(AIndex : Integer; AValue : String); virtual;
    Procedure SetwebsiteUrl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property childLink : TProfileTypechildLink Index 8 Read FchildLink Write SetchildLink;
    Property created : TDatetime Index 16 Read Fcreated Write Setcreated;
    Property currency : String Index 24 Read Fcurrency Write Setcurrency;
    Property defaultPage : String Index 32 Read FdefaultPage Write SetdefaultPage;
    Property eCommerceTracking : boolean Index 40 Read FeCommerceTracking Write SeteCommerceTracking;
    Property enhancedECommerceTracking : boolean Index 48 Read FenhancedECommerceTracking Write SetenhancedECommerceTracking;
    Property excludeQueryParameters : String Index 56 Read FexcludeQueryParameters Write SetexcludeQueryParameters;
    Property id : String Index 64 Read Fid Write Setid;
    Property internalWebPropertyId : String Index 72 Read FinternalWebPropertyId Write SetinternalWebPropertyId;
    Property kind : String Index 80 Read Fkind Write Setkind;
    Property name : String Index 88 Read Fname Write Setname;
    Property parentLink : TProfileTypeparentLink Index 96 Read FparentLink Write SetparentLink;
    Property permissions : TProfileTypepermissions Index 104 Read Fpermissions Write Setpermissions;
    Property selfLink : String Index 112 Read FselfLink Write SetselfLink;
    Property siteSearchCategoryParameters : String Index 120 Read FsiteSearchCategoryParameters Write SetsiteSearchCategoryParameters;
    Property siteSearchQueryParameters : String Index 128 Read FsiteSearchQueryParameters Write SetsiteSearchQueryParameters;
    Property stripSiteSearchCategoryParameters : boolean Index 136 Read FstripSiteSearchCategoryParameters Write SetstripSiteSearchCategoryParameters;
    Property stripSiteSearchQueryParameters : boolean Index 144 Read FstripSiteSearchQueryParameters Write SetstripSiteSearchQueryParameters;
    Property timezone : String Index 152 Read Ftimezone Write Settimezone;
    Property _type : String Index 160 Read F_type Write Set_type;
    Property updated : TDatetime Index 168 Read Fupdated Write Setupdated;
    Property webPropertyId : String Index 176 Read FwebPropertyId Write SetwebPropertyId;
    Property websiteUrl : String Index 184 Read FwebsiteUrl Write SetwebsiteUrl;
  end;
  TProfileClass = Class of TProfile;
  
  { --------------------------------------------------------------------
    TProfileFilterLink
    --------------------------------------------------------------------}
  
  TProfileFilterLink = Class(TGoogleBaseObject)
  Private
    FfilterRef : TFilterRef;
    Fid : String;
    Fkind : String;
    FprofileRef : TProfileRef;
    Frank : integer;
    FselfLink : String;
  Protected
    //Property setters
    Procedure SetfilterRef(AIndex : Integer; AValue : TFilterRef); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetprofileRef(AIndex : Integer; AValue : TProfileRef); virtual;
    Procedure Setrank(AIndex : Integer; AValue : integer); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property filterRef : TFilterRef Index 0 Read FfilterRef Write SetfilterRef;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property profileRef : TProfileRef Index 24 Read FprofileRef Write SetprofileRef;
    Property rank : integer Index 32 Read Frank Write Setrank;
    Property selfLink : String Index 40 Read FselfLink Write SetselfLink;
  end;
  TProfileFilterLinkClass = Class of TProfileFilterLink;
  
  { --------------------------------------------------------------------
    TProfileFilterLinks
    --------------------------------------------------------------------}
  
  TProfileFilterLinks = Class(TGoogleBaseObject)
  Private
    Fitems : TProfileFilterLinksTypeitemsArray;
    FitemsPerPage : integer;
    Fkind : String;
    FnextLink : String;
    FpreviousLink : String;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TProfileFilterLinksTypeitemsArray); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TProfileFilterLinksTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextLink : String Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : String Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : String Index 56 Read Fusername Write Setusername;
  end;
  TProfileFilterLinksClass = Class of TProfileFilterLinks;
  
  { --------------------------------------------------------------------
    TProfileRef
    --------------------------------------------------------------------}
  
  TProfileRef = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Fhref : String;
    Fid : String;
    FinternalWebPropertyId : String;
    Fkind : String;
    Fname : String;
    FwebPropertyId : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Sethref(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetinternalWebPropertyId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetwebPropertyId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property href : String Index 8 Read Fhref Write Sethref;
    Property id : String Index 16 Read Fid Write Setid;
    Property internalWebPropertyId : String Index 24 Read FinternalWebPropertyId Write SetinternalWebPropertyId;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property name : String Index 40 Read Fname Write Setname;
    Property webPropertyId : String Index 48 Read FwebPropertyId Write SetwebPropertyId;
  end;
  TProfileRefClass = Class of TProfileRef;
  
  { --------------------------------------------------------------------
    TProfileSummary
    --------------------------------------------------------------------}
  
  TProfileSummary = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
    Fname : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property name : String Index 16 Read Fname Write Setname;
    Property _type : String Index 24 Read F_type Write Set_type;
  end;
  TProfileSummaryClass = Class of TProfileSummary;
  
  { --------------------------------------------------------------------
    TProfiles
    --------------------------------------------------------------------}
  
  TProfiles = Class(TGoogleBaseObject)
  Private
    Fitems : TProfilesTypeitemsArray;
    FitemsPerPage : integer;
    Fkind : String;
    FnextLink : String;
    FpreviousLink : String;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TProfilesTypeitemsArray); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TProfilesTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextLink : String Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : String Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : String Index 56 Read Fusername Write Setusername;
  end;
  TProfilesClass = Class of TProfiles;
  
  { --------------------------------------------------------------------
    TRealtimeDataTypecolumnHeadersItem
    --------------------------------------------------------------------}
  
  TRealtimeDataTypecolumnHeadersItem = Class(TGoogleBaseObject)
  Private
    FcolumnType : String;
    FdataType : String;
    Fname : String;
  Protected
    //Property setters
    Procedure SetcolumnType(AIndex : Integer; AValue : String); virtual;
    Procedure SetdataType(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property columnType : String Index 0 Read FcolumnType Write SetcolumnType;
    Property dataType : String Index 8 Read FdataType Write SetdataType;
    Property name : String Index 16 Read Fname Write Setname;
  end;
  TRealtimeDataTypecolumnHeadersItemClass = Class of TRealtimeDataTypecolumnHeadersItem;
  
  { --------------------------------------------------------------------
    TRealtimeDataTypeprofileInfo
    --------------------------------------------------------------------}
  
  TRealtimeDataTypeprofileInfo = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FinternalWebPropertyId : String;
    FprofileId : String;
    FprofileName : String;
    FtableId : String;
    FwebPropertyId : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetinternalWebPropertyId(AIndex : Integer; AValue : String); virtual;
    Procedure SetprofileId(AIndex : Integer; AValue : String); virtual;
    Procedure SetprofileName(AIndex : Integer; AValue : String); virtual;
    Procedure SettableId(AIndex : Integer; AValue : String); virtual;
    Procedure SetwebPropertyId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property internalWebPropertyId : String Index 8 Read FinternalWebPropertyId Write SetinternalWebPropertyId;
    Property profileId : String Index 16 Read FprofileId Write SetprofileId;
    Property profileName : String Index 24 Read FprofileName Write SetprofileName;
    Property tableId : String Index 32 Read FtableId Write SettableId;
    Property webPropertyId : String Index 40 Read FwebPropertyId Write SetwebPropertyId;
  end;
  TRealtimeDataTypeprofileInfoClass = Class of TRealtimeDataTypeprofileInfo;
  
  { --------------------------------------------------------------------
    TRealtimeDataTypequery
    --------------------------------------------------------------------}
  
  TRealtimeDataTypequery = Class(TGoogleBaseObject)
  Private
    Fdimensions : String;
    Ffilters : String;
    Fids : String;
    Fmaxresults : integer;
    Fmetrics : TStringArray;
    Fsort : TStringArray;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdimensions(AIndex : Integer; AValue : String); virtual;
    Procedure Setfilters(AIndex : Integer; AValue : String); virtual;
    Procedure Setids(AIndex : Integer; AValue : String); virtual;
    Procedure Setmaxresults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setmetrics(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setsort(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property dimensions : String Index 0 Read Fdimensions Write Setdimensions;
    Property filters : String Index 8 Read Ffilters Write Setfilters;
    Property ids : String Index 16 Read Fids Write Setids;
    Property maxresults : integer Index 24 Read Fmaxresults Write Setmaxresults;
    Property metrics : TStringArray Index 32 Read Fmetrics Write Setmetrics;
    Property sort : TStringArray Index 40 Read Fsort Write Setsort;
  end;
  TRealtimeDataTypequeryClass = Class of TRealtimeDataTypequery;
  
  { --------------------------------------------------------------------
    TRealtimeDataTypetotalsForAllResults
    --------------------------------------------------------------------}
  
  TRealtimeDataTypetotalsForAllResults = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TRealtimeDataTypetotalsForAllResultsClass = Class of TRealtimeDataTypetotalsForAllResults;
  
  { --------------------------------------------------------------------
    TRealtimeData
    --------------------------------------------------------------------}
  
  TRealtimeData = Class(TGoogleBaseObject)
  Private
    FcolumnHeaders : TRealtimeDataTypecolumnHeadersArray;
    Fid : String;
    Fkind : String;
    FprofileInfo : TRealtimeDataTypeprofileInfo;
    Fquery : TRealtimeDataTypequery;
    Frows : TRealtimeDataTyperowsArray;
    FselfLink : String;
    FtotalResults : integer;
    FtotalsForAllResults : TRealtimeDataTypetotalsForAllResults;
  Protected
    //Property setters
    Procedure SetcolumnHeaders(AIndex : Integer; AValue : TRealtimeDataTypecolumnHeadersArray); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetprofileInfo(AIndex : Integer; AValue : TRealtimeDataTypeprofileInfo); virtual;
    Procedure Setquery(AIndex : Integer; AValue : TRealtimeDataTypequery); virtual;
    Procedure Setrows(AIndex : Integer; AValue : TRealtimeDataTyperowsArray); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalsForAllResults(AIndex : Integer; AValue : TRealtimeDataTypetotalsForAllResults); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property columnHeaders : TRealtimeDataTypecolumnHeadersArray Index 0 Read FcolumnHeaders Write SetcolumnHeaders;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property profileInfo : TRealtimeDataTypeprofileInfo Index 24 Read FprofileInfo Write SetprofileInfo;
    Property query : TRealtimeDataTypequery Index 32 Read Fquery Write Setquery;
    Property rows : TRealtimeDataTyperowsArray Index 40 Read Frows Write Setrows;
    Property selfLink : String Index 48 Read FselfLink Write SetselfLink;
    Property totalResults : integer Index 56 Read FtotalResults Write SettotalResults;
    Property totalsForAllResults : TRealtimeDataTypetotalsForAllResults Index 64 Read FtotalsForAllResults Write SettotalsForAllResults;
  end;
  TRealtimeDataClass = Class of TRealtimeData;
  
  { --------------------------------------------------------------------
    TSegment
    --------------------------------------------------------------------}
  
  TSegment = Class(TGoogleBaseObject)
  Private
    Fcreated : TDatetime;
    Fdefinition : String;
    Fid : String;
    Fkind : String;
    Fname : String;
    FsegmentId : String;
    FselfLink : String;
    F_type : String;
    Fupdated : TDatetime;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setdefinition(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetsegmentId(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property created : TDatetime Index 0 Read Fcreated Write Setcreated;
    Property definition : String Index 8 Read Fdefinition Write Setdefinition;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property name : String Index 32 Read Fname Write Setname;
    Property segmentId : String Index 40 Read FsegmentId Write SetsegmentId;
    Property selfLink : String Index 48 Read FselfLink Write SetselfLink;
    Property _type : String Index 56 Read F_type Write Set_type;
    Property updated : TDatetime Index 64 Read Fupdated Write Setupdated;
  end;
  TSegmentClass = Class of TSegment;
  
  { --------------------------------------------------------------------
    TSegments
    --------------------------------------------------------------------}
  
  TSegments = Class(TGoogleBaseObject)
  Private
    Fitems : TSegmentsTypeitemsArray;
    FitemsPerPage : integer;
    Fkind : String;
    FnextLink : String;
    FpreviousLink : String;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TSegmentsTypeitemsArray); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TSegmentsTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextLink : String Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : String Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : String Index 56 Read Fusername Write Setusername;
  end;
  TSegmentsClass = Class of TSegments;
  
  { --------------------------------------------------------------------
    TUnsampledReportTypecloudStorageDownloadDetails
    --------------------------------------------------------------------}
  
  TUnsampledReportTypecloudStorageDownloadDetails = Class(TGoogleBaseObject)
  Private
    FbucketId : String;
    FobjectId : String;
  Protected
    //Property setters
    Procedure SetbucketId(AIndex : Integer; AValue : String); virtual;
    Procedure SetobjectId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property bucketId : String Index 0 Read FbucketId Write SetbucketId;
    Property objectId : String Index 8 Read FobjectId Write SetobjectId;
  end;
  TUnsampledReportTypecloudStorageDownloadDetailsClass = Class of TUnsampledReportTypecloudStorageDownloadDetails;
  
  { --------------------------------------------------------------------
    TUnsampledReportTypedriveDownloadDetails
    --------------------------------------------------------------------}
  
  TUnsampledReportTypedriveDownloadDetails = Class(TGoogleBaseObject)
  Private
    FdocumentId : String;
  Protected
    //Property setters
    Procedure SetdocumentId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property documentId : String Index 0 Read FdocumentId Write SetdocumentId;
  end;
  TUnsampledReportTypedriveDownloadDetailsClass = Class of TUnsampledReportTypedriveDownloadDetails;
  
  { --------------------------------------------------------------------
    TUnsampledReport
    --------------------------------------------------------------------}
  
  TUnsampledReport = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FcloudStorageDownloadDetails : TUnsampledReportTypecloudStorageDownloadDetails;
    Fcreated : TDatetime;
    Fdimensions : String;
    FdownloadType : String;
    FdriveDownloadDetails : TUnsampledReportTypedriveDownloadDetails;
    Fenddate : String;
    Ffilters : String;
    Fid : String;
    Fkind : String;
    Fmetrics : String;
    FprofileId : String;
    Fsegment : String;
    FselfLink : String;
    Fstartdate : String;
    Fstatus : String;
    Ftitle : String;
    Fupdated : TDatetime;
    FwebPropertyId : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcloudStorageDownloadDetails(AIndex : Integer; AValue : TUnsampledReportTypecloudStorageDownloadDetails); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setdimensions(AIndex : Integer; AValue : String); virtual;
    Procedure SetdownloadType(AIndex : Integer; AValue : String); virtual;
    Procedure SetdriveDownloadDetails(AIndex : Integer; AValue : TUnsampledReportTypedriveDownloadDetails); virtual;
    Procedure Setenddate(AIndex : Integer; AValue : String); virtual;
    Procedure Setfilters(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setmetrics(AIndex : Integer; AValue : String); virtual;
    Procedure SetprofileId(AIndex : Integer; AValue : String); virtual;
    Procedure Setsegment(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setstartdate(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetwebPropertyId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property cloudStorageDownloadDetails : TUnsampledReportTypecloudStorageDownloadDetails Index 8 Read FcloudStorageDownloadDetails Write SetcloudStorageDownloadDetails;
    Property created : TDatetime Index 16 Read Fcreated Write Setcreated;
    Property dimensions : String Index 24 Read Fdimensions Write Setdimensions;
    Property downloadType : String Index 32 Read FdownloadType Write SetdownloadType;
    Property driveDownloadDetails : TUnsampledReportTypedriveDownloadDetails Index 40 Read FdriveDownloadDetails Write SetdriveDownloadDetails;
    Property enddate : String Index 48 Read Fenddate Write Setenddate;
    Property filters : String Index 56 Read Ffilters Write Setfilters;
    Property id : String Index 64 Read Fid Write Setid;
    Property kind : String Index 72 Read Fkind Write Setkind;
    Property metrics : String Index 80 Read Fmetrics Write Setmetrics;
    Property profileId : String Index 88 Read FprofileId Write SetprofileId;
    Property segment : String Index 96 Read Fsegment Write Setsegment;
    Property selfLink : String Index 104 Read FselfLink Write SetselfLink;
    Property startdate : String Index 112 Read Fstartdate Write Setstartdate;
    Property status : String Index 120 Read Fstatus Write Setstatus;
    Property title : String Index 128 Read Ftitle Write Settitle;
    Property updated : TDatetime Index 136 Read Fupdated Write Setupdated;
    Property webPropertyId : String Index 144 Read FwebPropertyId Write SetwebPropertyId;
  end;
  TUnsampledReportClass = Class of TUnsampledReport;
  
  { --------------------------------------------------------------------
    TUnsampledReports
    --------------------------------------------------------------------}
  
  TUnsampledReports = Class(TGoogleBaseObject)
  Private
    Fitems : TUnsampledReportsTypeitemsArray;
    FitemsPerPage : integer;
    Fkind : String;
    FnextLink : String;
    FpreviousLink : String;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TUnsampledReportsTypeitemsArray); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TUnsampledReportsTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextLink : String Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : String Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : String Index 56 Read Fusername Write Setusername;
  end;
  TUnsampledReportsClass = Class of TUnsampledReports;
  
  { --------------------------------------------------------------------
    TUpload
    --------------------------------------------------------------------}
  
  TUpload = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FcustomDataSourceId : String;
    Ferrors : TStringArray;
    Fid : String;
    Fkind : String;
    Fstatus : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcustomDataSourceId(AIndex : Integer; AValue : String); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property customDataSourceId : String Index 8 Read FcustomDataSourceId Write SetcustomDataSourceId;
    Property errors : TStringArray Index 16 Read Ferrors Write Seterrors;
    Property id : String Index 24 Read Fid Write Setid;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property status : String Index 40 Read Fstatus Write Setstatus;
  end;
  TUploadClass = Class of TUpload;
  
  { --------------------------------------------------------------------
    TUploads
    --------------------------------------------------------------------}
  
  TUploads = Class(TGoogleBaseObject)
  Private
    Fitems : TUploadsTypeitemsArray;
    FitemsPerPage : integer;
    Fkind : String;
    FnextLink : String;
    FpreviousLink : String;
    FstartIndex : integer;
    FtotalResults : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TUploadsTypeitemsArray); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TUploadsTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextLink : String Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : String Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
  end;
  TUploadsClass = Class of TUploads;
  
  { --------------------------------------------------------------------
    TUserRef
    --------------------------------------------------------------------}
  
  TUserRef = Class(TGoogleBaseObject)
  Private
    Femail : String;
    Fid : String;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setemail(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property email : String Index 0 Read Femail Write Setemail;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
  end;
  TUserRefClass = Class of TUserRef;
  
  { --------------------------------------------------------------------
    TWebPropertyRef
    --------------------------------------------------------------------}
  
  TWebPropertyRef = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Fhref : String;
    Fid : String;
    FinternalWebPropertyId : String;
    Fkind : String;
    Fname : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Sethref(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetinternalWebPropertyId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property href : String Index 8 Read Fhref Write Sethref;
    Property id : String Index 16 Read Fid Write Setid;
    Property internalWebPropertyId : String Index 24 Read FinternalWebPropertyId Write SetinternalWebPropertyId;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property name : String Index 40 Read Fname Write Setname;
  end;
  TWebPropertyRefClass = Class of TWebPropertyRef;
  
  { --------------------------------------------------------------------
    TWebPropertySummary
    --------------------------------------------------------------------}
  
  TWebPropertySummary = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FinternalWebPropertyId : String;
    Fkind : String;
    Flevel : String;
    Fname : String;
    Fprofiles : TWebPropertySummaryTypeprofilesArray;
    FwebsiteUrl : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetinternalWebPropertyId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlevel(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setprofiles(AIndex : Integer; AValue : TWebPropertySummaryTypeprofilesArray); virtual;
    Procedure SetwebsiteUrl(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property internalWebPropertyId : String Index 8 Read FinternalWebPropertyId Write SetinternalWebPropertyId;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property level : String Index 24 Read Flevel Write Setlevel;
    Property name : String Index 32 Read Fname Write Setname;
    Property profiles : TWebPropertySummaryTypeprofilesArray Index 40 Read Fprofiles Write Setprofiles;
    Property websiteUrl : String Index 48 Read FwebsiteUrl Write SetwebsiteUrl;
  end;
  TWebPropertySummaryClass = Class of TWebPropertySummary;
  
  { --------------------------------------------------------------------
    TWebproperties
    --------------------------------------------------------------------}
  
  TWebproperties = Class(TGoogleBaseObject)
  Private
    Fitems : TWebpropertiesTypeitemsArray;
    FitemsPerPage : integer;
    Fkind : String;
    FnextLink : String;
    FpreviousLink : String;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TWebpropertiesTypeitemsArray); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TWebpropertiesTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextLink : String Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : String Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : String Index 56 Read Fusername Write Setusername;
  end;
  TWebpropertiesClass = Class of TWebproperties;
  
  { --------------------------------------------------------------------
    TWebpropertyTypechildLink
    --------------------------------------------------------------------}
  
  TWebpropertyTypechildLink = Class(TGoogleBaseObject)
  Private
    Fhref : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property href : String Index 0 Read Fhref Write Sethref;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TWebpropertyTypechildLinkClass = Class of TWebpropertyTypechildLink;
  
  { --------------------------------------------------------------------
    TWebpropertyTypeparentLink
    --------------------------------------------------------------------}
  
  TWebpropertyTypeparentLink = Class(TGoogleBaseObject)
  Private
    Fhref : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property href : String Index 0 Read Fhref Write Sethref;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TWebpropertyTypeparentLinkClass = Class of TWebpropertyTypeparentLink;
  
  { --------------------------------------------------------------------
    TWebpropertyTypepermissions
    --------------------------------------------------------------------}
  
  TWebpropertyTypepermissions = Class(TGoogleBaseObject)
  Private
    Feffective : TStringArray;
  Protected
    //Property setters
    Procedure Seteffective(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property effective : TStringArray Index 0 Read Feffective Write Seteffective;
  end;
  TWebpropertyTypepermissionsClass = Class of TWebpropertyTypepermissions;
  
  { --------------------------------------------------------------------
    TWebproperty
    --------------------------------------------------------------------}
  
  TWebproperty = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FchildLink : TWebpropertyTypechildLink;
    Fcreated : TDatetime;
    FdefaultProfileId : String;
    Fid : String;
    FindustryVertical : String;
    FinternalWebPropertyId : String;
    Fkind : String;
    Flevel : String;
    Fname : String;
    FparentLink : TWebpropertyTypeparentLink;
    Fpermissions : TWebpropertyTypepermissions;
    FprofileCount : integer;
    FselfLink : String;
    Fupdated : TDatetime;
    FwebsiteUrl : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetchildLink(AIndex : Integer; AValue : TWebpropertyTypechildLink); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetdefaultProfileId(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetindustryVertical(AIndex : Integer; AValue : String); virtual;
    Procedure SetinternalWebPropertyId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlevel(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetparentLink(AIndex : Integer; AValue : TWebpropertyTypeparentLink); virtual;
    Procedure Setpermissions(AIndex : Integer; AValue : TWebpropertyTypepermissions); virtual;
    Procedure SetprofileCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetwebsiteUrl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property childLink : TWebpropertyTypechildLink Index 8 Read FchildLink Write SetchildLink;
    Property created : TDatetime Index 16 Read Fcreated Write Setcreated;
    Property defaultProfileId : String Index 24 Read FdefaultProfileId Write SetdefaultProfileId;
    Property id : String Index 32 Read Fid Write Setid;
    Property industryVertical : String Index 40 Read FindustryVertical Write SetindustryVertical;
    Property internalWebPropertyId : String Index 48 Read FinternalWebPropertyId Write SetinternalWebPropertyId;
    Property kind : String Index 56 Read Fkind Write Setkind;
    Property level : String Index 64 Read Flevel Write Setlevel;
    Property name : String Index 72 Read Fname Write Setname;
    Property parentLink : TWebpropertyTypeparentLink Index 80 Read FparentLink Write SetparentLink;
    Property permissions : TWebpropertyTypepermissions Index 88 Read Fpermissions Write Setpermissions;
    Property profileCount : integer Index 96 Read FprofileCount Write SetprofileCount;
    Property selfLink : String Index 104 Read FselfLink Write SetselfLink;
    Property updated : TDatetime Index 112 Read Fupdated Write Setupdated;
    Property websiteUrl : String Index 120 Read FwebsiteUrl Write SetwebsiteUrl;
  end;
  TWebpropertyClass = Class of TWebproperty;
  
  { --------------------------------------------------------------------
    TDataGaResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDataGaResource, method Get
  
  TDataGaGetOptions = Record
    dimensions : String;
    enddate : String;
    filters : String;
    ids : String;
    maxresults : integer;
    metrics : String;
    output : String;
    samplingLevel : String;
    segment : String;
    sort : String;
    startdate : String;
    startindex : integer;
  end;
  
  TDataGaResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(AQuery : string  = '') : TGaData;
    Function Get(AQuery : TDataGagetOptions) : TGaData;
  end;
  
  
  { --------------------------------------------------------------------
    TDataMcfResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDataMcfResource, method Get
  
  TDataMcfGetOptions = Record
    dimensions : String;
    enddate : String;
    filters : String;
    ids : String;
    maxresults : integer;
    metrics : String;
    samplingLevel : String;
    sort : String;
    startdate : String;
    startindex : integer;
  end;
  
  TDataMcfResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(AQuery : string  = '') : TMcfData;
    Function Get(AQuery : TDataMcfgetOptions) : TMcfData;
  end;
  
  
  { --------------------------------------------------------------------
    TDataRealtimeResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDataRealtimeResource, method Get
  
  TDataRealtimeGetOptions = Record
    dimensions : String;
    filters : String;
    ids : String;
    maxresults : integer;
    metrics : String;
    sort : String;
  end;
  
  TDataRealtimeResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(AQuery : string  = '') : TRealtimeData;
    Function Get(AQuery : TDataRealtimegetOptions) : TRealtimeData;
  end;
  
  
  { --------------------------------------------------------------------
    TDataResource
    --------------------------------------------------------------------}
  
  TDataResource = Class(TGoogleResource)
  Private
    FGaInstance : TDataGaResource;
    FMcfInstance : TDataMcfResource;
    FRealtimeInstance : TDataRealtimeResource;
    Function GetGaInstance : TDataGaResource;virtual;
    Function GetMcfInstance : TDataMcfResource;virtual;
    Function GetRealtimeInstance : TDataRealtimeResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateGaResource(AOwner : TComponent) : TDataGaResource;virtual;overload;
    Function CreateGaResource : TDataGaResource;virtual;overload;
    Function CreateMcfResource(AOwner : TComponent) : TDataMcfResource;virtual;overload;
    Function CreateMcfResource : TDataMcfResource;virtual;overload;
    Function CreateRealtimeResource(AOwner : TComponent) : TDataRealtimeResource;virtual;overload;
    Function CreateRealtimeResource : TDataRealtimeResource;virtual;overload;
    Property GaResource : TDataGaResource Read GetGaInstance;
    Property McfResource : TDataMcfResource Read GetMcfInstance;
    Property RealtimeResource : TDataRealtimeResource Read GetRealtimeInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TManagementAccountSummariesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TManagementAccountSummariesResource, method List
  
  TManagementAccountSummariesListOptions = Record
    maxresults : integer;
    startindex : integer;
  end;
  
  TManagementAccountSummariesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TAccountSummaries;
    Function List(AQuery : TManagementAccountSummarieslistOptions) : TAccountSummaries;
  end;
  
  
  { --------------------------------------------------------------------
    TManagementAccountUserLinksResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TManagementAccountUserLinksResource, method List
  
  TManagementAccountUserLinksListOptions = Record
    maxresults : integer;
    startindex : integer;
  end;
  
  TManagementAccountUserLinksResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(accountId: string; linkId: string);
    Function Insert(accountId: string; aEntityUserLink : TEntityUserLink) : TEntityUserLink;
    Function List(accountId: string; AQuery : string  = '') : TEntityUserLinks;
    Function List(accountId: string; AQuery : TManagementAccountUserLinkslistOptions) : TEntityUserLinks;
    Function Update(accountId: string; linkId: string; aEntityUserLink : TEntityUserLink) : TEntityUserLink;
  end;
  
  
  { --------------------------------------------------------------------
    TManagementAccountsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TManagementAccountsResource, method List
  
  TManagementAccountsListOptions = Record
    maxresults : integer;
    startindex : integer;
  end;
  
  TManagementAccountsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TAccounts;
    Function List(AQuery : TManagementAccountslistOptions) : TAccounts;
  end;
  
  
  { --------------------------------------------------------------------
    TManagementCustomDataSourcesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TManagementCustomDataSourcesResource, method List
  
  TManagementCustomDataSourcesListOptions = Record
    maxresults : integer;
    startindex : integer;
  end;
  
  TManagementCustomDataSourcesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(accountId: string; webPropertyId: string; AQuery : string  = '') : TCustomDataSources;
    Function List(accountId: string; webPropertyId: string; AQuery : TManagementCustomDataSourceslistOptions) : TCustomDataSources;
  end;
  
  
  { --------------------------------------------------------------------
    TManagementCustomDimensionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TManagementCustomDimensionsResource, method List
  
  TManagementCustomDimensionsListOptions = Record
    maxresults : integer;
    startindex : integer;
  end;
  
  
  //Optional query Options for TManagementCustomDimensionsResource, method Patch
  
  TManagementCustomDimensionsPatchOptions = Record
    ignoreCustomDataSourceLinks : boolean;
  end;
  
  
  //Optional query Options for TManagementCustomDimensionsResource, method Update
  
  TManagementCustomDimensionsUpdateOptions = Record
    ignoreCustomDataSourceLinks : boolean;
  end;
  
  TManagementCustomDimensionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: string; customDimensionId: string; webPropertyId: string) : TCustomDimension;
    Function Insert(accountId: string; webPropertyId: string; aCustomDimension : TCustomDimension) : TCustomDimension;
    Function List(accountId: string; webPropertyId: string; AQuery : string  = '') : TCustomDimensions;
    Function List(accountId: string; webPropertyId: string; AQuery : TManagementCustomDimensionslistOptions) : TCustomDimensions;
    Function Patch(accountId: string; customDimensionId: string; webPropertyId: string; aCustomDimension : TCustomDimension; AQuery : string  = '') : TCustomDimension;
    Function Patch(accountId: string; customDimensionId: string; webPropertyId: string; aCustomDimension : TCustomDimension; AQuery : TManagementCustomDimensionspatchOptions) : TCustomDimension;
    Function Update(accountId: string; customDimensionId: string; webPropertyId: string; aCustomDimension : TCustomDimension; AQuery : string  = '') : TCustomDimension;
    Function Update(accountId: string; customDimensionId: string; webPropertyId: string; aCustomDimension : TCustomDimension; AQuery : TManagementCustomDimensionsupdateOptions) : TCustomDimension;
  end;
  
  
  { --------------------------------------------------------------------
    TManagementCustomMetricsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TManagementCustomMetricsResource, method List
  
  TManagementCustomMetricsListOptions = Record
    maxresults : integer;
    startindex : integer;
  end;
  
  
  //Optional query Options for TManagementCustomMetricsResource, method Patch
  
  TManagementCustomMetricsPatchOptions = Record
    ignoreCustomDataSourceLinks : boolean;
  end;
  
  
  //Optional query Options for TManagementCustomMetricsResource, method Update
  
  TManagementCustomMetricsUpdateOptions = Record
    ignoreCustomDataSourceLinks : boolean;
  end;
  
  TManagementCustomMetricsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: string; customMetricId: string; webPropertyId: string) : TCustomMetric;
    Function Insert(accountId: string; webPropertyId: string; aCustomMetric : TCustomMetric) : TCustomMetric;
    Function List(accountId: string; webPropertyId: string; AQuery : string  = '') : TCustomMetrics;
    Function List(accountId: string; webPropertyId: string; AQuery : TManagementCustomMetricslistOptions) : TCustomMetrics;
    Function Patch(accountId: string; customMetricId: string; webPropertyId: string; aCustomMetric : TCustomMetric; AQuery : string  = '') : TCustomMetric;
    Function Patch(accountId: string; customMetricId: string; webPropertyId: string; aCustomMetric : TCustomMetric; AQuery : TManagementCustomMetricspatchOptions) : TCustomMetric;
    Function Update(accountId: string; customMetricId: string; webPropertyId: string; aCustomMetric : TCustomMetric; AQuery : string  = '') : TCustomMetric;
    Function Update(accountId: string; customMetricId: string; webPropertyId: string; aCustomMetric : TCustomMetric; AQuery : TManagementCustomMetricsupdateOptions) : TCustomMetric;
  end;
  
  
  { --------------------------------------------------------------------
    TManagementExperimentsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TManagementExperimentsResource, method List
  
  TManagementExperimentsListOptions = Record
    maxresults : integer;
    startindex : integer;
  end;
  
  TManagementExperimentsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(accountId: string; experimentId: string; profileId: string; webPropertyId: string);
    Function Get(accountId: string; experimentId: string; profileId: string; webPropertyId: string) : TExperiment;
    Function Insert(accountId: string; profileId: string; webPropertyId: string; aExperiment : TExperiment) : TExperiment;
    Function List(accountId: string; profileId: string; webPropertyId: string; AQuery : string  = '') : TExperiments;
    Function List(accountId: string; profileId: string; webPropertyId: string; AQuery : TManagementExperimentslistOptions) : TExperiments;
    Function Patch(accountId: string; experimentId: string; profileId: string; webPropertyId: string; aExperiment : TExperiment) : TExperiment;
    Function Update(accountId: string; experimentId: string; profileId: string; webPropertyId: string; aExperiment : TExperiment) : TExperiment;
  end;
  
  
  { --------------------------------------------------------------------
    TManagementFiltersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TManagementFiltersResource, method List
  
  TManagementFiltersListOptions = Record
    maxresults : integer;
    startindex : integer;
  end;
  
  TManagementFiltersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(accountId: string; filterId: string) : TFilter;
    Function Get(accountId: string; filterId: string) : TFilter;
    Function Insert(accountId: string; aFilter : TFilter) : TFilter;
    Function List(accountId: string; AQuery : string  = '') : TFilters;
    Function List(accountId: string; AQuery : TManagementFilterslistOptions) : TFilters;
    Function Patch(accountId: string; filterId: string; aFilter : TFilter) : TFilter;
    Function Update(accountId: string; filterId: string; aFilter : TFilter) : TFilter;
  end;
  
  
  { --------------------------------------------------------------------
    TManagementGoalsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TManagementGoalsResource, method List
  
  TManagementGoalsListOptions = Record
    maxresults : integer;
    startindex : integer;
  end;
  
  TManagementGoalsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: string; goalId: string; profileId: string; webPropertyId: string) : TGoal;
    Function Insert(accountId: string; profileId: string; webPropertyId: string; aGoal : TGoal) : TGoal;
    Function List(accountId: string; profileId: string; webPropertyId: string; AQuery : string  = '') : TGoals;
    Function List(accountId: string; profileId: string; webPropertyId: string; AQuery : TManagementGoalslistOptions) : TGoals;
    Function Patch(accountId: string; goalId: string; profileId: string; webPropertyId: string; aGoal : TGoal) : TGoal;
    Function Update(accountId: string; goalId: string; profileId: string; webPropertyId: string; aGoal : TGoal) : TGoal;
  end;
  
  
  { --------------------------------------------------------------------
    TManagementProfileFilterLinksResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TManagementProfileFilterLinksResource, method List
  
  TManagementProfileFilterLinksListOptions = Record
    maxresults : integer;
    startindex : integer;
  end;
  
  TManagementProfileFilterLinksResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(accountId: string; linkId: string; profileId: string; webPropertyId: string);
    Function Get(accountId: string; linkId: string; profileId: string; webPropertyId: string) : TProfileFilterLink;
    Function Insert(accountId: string; profileId: string; webPropertyId: string; aProfileFilterLink : TProfileFilterLink) : TProfileFilterLink;
    Function List(accountId: string; profileId: string; webPropertyId: string; AQuery : string  = '') : TProfileFilterLinks;
    Function List(accountId: string; profileId: string; webPropertyId: string; AQuery : TManagementProfileFilterLinkslistOptions) : TProfileFilterLinks;
    Function Patch(accountId: string; linkId: string; profileId: string; webPropertyId: string; aProfileFilterLink : TProfileFilterLink) : TProfileFilterLink;
    Function Update(accountId: string; linkId: string; profileId: string; webPropertyId: string; aProfileFilterLink : TProfileFilterLink) : TProfileFilterLink;
  end;
  
  
  { --------------------------------------------------------------------
    TManagementProfileUserLinksResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TManagementProfileUserLinksResource, method List
  
  TManagementProfileUserLinksListOptions = Record
    maxresults : integer;
    startindex : integer;
  end;
  
  TManagementProfileUserLinksResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(accountId: string; linkId: string; profileId: string; webPropertyId: string);
    Function Insert(accountId: string; profileId: string; webPropertyId: string; aEntityUserLink : TEntityUserLink) : TEntityUserLink;
    Function List(accountId: string; profileId: string; webPropertyId: string; AQuery : string  = '') : TEntityUserLinks;
    Function List(accountId: string; profileId: string; webPropertyId: string; AQuery : TManagementProfileUserLinkslistOptions) : TEntityUserLinks;
    Function Update(accountId: string; linkId: string; profileId: string; webPropertyId: string; aEntityUserLink : TEntityUserLink) : TEntityUserLink;
  end;
  
  
  { --------------------------------------------------------------------
    TManagementProfilesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TManagementProfilesResource, method List
  
  TManagementProfilesListOptions = Record
    maxresults : integer;
    startindex : integer;
  end;
  
  TManagementProfilesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(accountId: string; profileId: string; webPropertyId: string);
    Function Get(accountId: string; profileId: string; webPropertyId: string) : TProfile;
    Function Insert(accountId: string; webPropertyId: string; aProfile : TProfile) : TProfile;
    Function List(accountId: string; webPropertyId: string; AQuery : string  = '') : TProfiles;
    Function List(accountId: string; webPropertyId: string; AQuery : TManagementProfileslistOptions) : TProfiles;
    Function Patch(accountId: string; profileId: string; webPropertyId: string; aProfile : TProfile) : TProfile;
    Function Update(accountId: string; profileId: string; webPropertyId: string; aProfile : TProfile) : TProfile;
  end;
  
  
  { --------------------------------------------------------------------
    TManagementSegmentsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TManagementSegmentsResource, method List
  
  TManagementSegmentsListOptions = Record
    maxresults : integer;
    startindex : integer;
  end;
  
  TManagementSegmentsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TSegments;
    Function List(AQuery : TManagementSegmentslistOptions) : TSegments;
  end;
  
  
  { --------------------------------------------------------------------
    TManagementUnsampledReportsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TManagementUnsampledReportsResource, method List
  
  TManagementUnsampledReportsListOptions = Record
    maxresults : integer;
    startindex : integer;
  end;
  
  TManagementUnsampledReportsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: string; profileId: string; unsampledReportId: string; webPropertyId: string) : TUnsampledReport;
    Function Insert(accountId: string; profileId: string; webPropertyId: string; aUnsampledReport : TUnsampledReport) : TUnsampledReport;
    Function List(accountId: string; profileId: string; webPropertyId: string; AQuery : string  = '') : TUnsampledReports;
    Function List(accountId: string; profileId: string; webPropertyId: string; AQuery : TManagementUnsampledReportslistOptions) : TUnsampledReports;
  end;
  
  
  { --------------------------------------------------------------------
    TManagementUploadsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TManagementUploadsResource, method List
  
  TManagementUploadsListOptions = Record
    maxresults : integer;
    startindex : integer;
  end;
  
  TManagementUploadsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure DeleteUploadData(accountId: string; customDataSourceId: string; webPropertyId: string; aAnalyticsDataimportDeleteUploadDataRequest : TAnalyticsDataimportDeleteUploadDataRequest);
    Function Get(accountId: string; customDataSourceId: string; uploadId: string; webPropertyId: string) : TUpload;
    Function List(accountId: string; customDataSourceId: string; webPropertyId: string; AQuery : string  = '') : TUploads;
    Function List(accountId: string; customDataSourceId: string; webPropertyId: string; AQuery : TManagementUploadslistOptions) : TUploads;
    Function UploadData(accountId: string; customDataSourceId: string; webPropertyId: string) : TUpload;
  end;
  
  
  { --------------------------------------------------------------------
    TManagementWebPropertyAdWordsLinksResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TManagementWebPropertyAdWordsLinksResource, method List
  
  TManagementWebPropertyAdWordsLinksListOptions = Record
    maxresults : integer;
    startindex : integer;
  end;
  
  TManagementWebPropertyAdWordsLinksResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(accountId: string; webPropertyAdWordsLinkId: string; webPropertyId: string);
    Function Get(accountId: string; webPropertyAdWordsLinkId: string; webPropertyId: string) : TEntityAdWordsLink;
    Function Insert(accountId: string; webPropertyId: string; aEntityAdWordsLink : TEntityAdWordsLink) : TEntityAdWordsLink;
    Function List(accountId: string; webPropertyId: string; AQuery : string  = '') : TEntityAdWordsLinks;
    Function List(accountId: string; webPropertyId: string; AQuery : TManagementWebPropertyAdWordsLinkslistOptions) : TEntityAdWordsLinks;
    Function Patch(accountId: string; webPropertyAdWordsLinkId: string; webPropertyId: string; aEntityAdWordsLink : TEntityAdWordsLink) : TEntityAdWordsLink;
    Function Update(accountId: string; webPropertyAdWordsLinkId: string; webPropertyId: string; aEntityAdWordsLink : TEntityAdWordsLink) : TEntityAdWordsLink;
  end;
  
  
  { --------------------------------------------------------------------
    TManagementWebpropertiesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TManagementWebpropertiesResource, method List
  
  TManagementWebpropertiesListOptions = Record
    maxresults : integer;
    startindex : integer;
  end;
  
  TManagementWebpropertiesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: string; webPropertyId: string) : TWebproperty;
    Function Insert(accountId: string; aWebproperty : TWebproperty) : TWebproperty;
    Function List(accountId: string; AQuery : string  = '') : TWebproperties;
    Function List(accountId: string; AQuery : TManagementWebpropertieslistOptions) : TWebproperties;
    Function Patch(accountId: string; webPropertyId: string; aWebproperty : TWebproperty) : TWebproperty;
    Function Update(accountId: string; webPropertyId: string; aWebproperty : TWebproperty) : TWebproperty;
  end;
  
  
  { --------------------------------------------------------------------
    TManagementWebpropertyUserLinksResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TManagementWebpropertyUserLinksResource, method List
  
  TManagementWebpropertyUserLinksListOptions = Record
    maxresults : integer;
    startindex : integer;
  end;
  
  TManagementWebpropertyUserLinksResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(accountId: string; linkId: string; webPropertyId: string);
    Function Insert(accountId: string; webPropertyId: string; aEntityUserLink : TEntityUserLink) : TEntityUserLink;
    Function List(accountId: string; webPropertyId: string; AQuery : string  = '') : TEntityUserLinks;
    Function List(accountId: string; webPropertyId: string; AQuery : TManagementWebpropertyUserLinkslistOptions) : TEntityUserLinks;
    Function Update(accountId: string; linkId: string; webPropertyId: string; aEntityUserLink : TEntityUserLink) : TEntityUserLink;
  end;
  
  
  { --------------------------------------------------------------------
    TManagementResource
    --------------------------------------------------------------------}
  
  TManagementResource = Class(TGoogleResource)
  Private
    FAccountSummariesInstance : TManagementAccountSummariesResource;
    FAccountUserLinksInstance : TManagementAccountUserLinksResource;
    FAccountsInstance : TManagementAccountsResource;
    FCustomDataSourcesInstance : TManagementCustomDataSourcesResource;
    FCustomDimensionsInstance : TManagementCustomDimensionsResource;
    FCustomMetricsInstance : TManagementCustomMetricsResource;
    FExperimentsInstance : TManagementExperimentsResource;
    FFiltersInstance : TManagementFiltersResource;
    FGoalsInstance : TManagementGoalsResource;
    FProfileFilterLinksInstance : TManagementProfileFilterLinksResource;
    FProfileUserLinksInstance : TManagementProfileUserLinksResource;
    FProfilesInstance : TManagementProfilesResource;
    FSegmentsInstance : TManagementSegmentsResource;
    FUnsampledReportsInstance : TManagementUnsampledReportsResource;
    FUploadsInstance : TManagementUploadsResource;
    FWebPropertyAdWordsLinksInstance : TManagementWebPropertyAdWordsLinksResource;
    FWebpropertiesInstance : TManagementWebpropertiesResource;
    FWebpropertyUserLinksInstance : TManagementWebpropertyUserLinksResource;
    Function GetAccountSummariesInstance : TManagementAccountSummariesResource;virtual;
    Function GetAccountUserLinksInstance : TManagementAccountUserLinksResource;virtual;
    Function GetAccountsInstance : TManagementAccountsResource;virtual;
    Function GetCustomDataSourcesInstance : TManagementCustomDataSourcesResource;virtual;
    Function GetCustomDimensionsInstance : TManagementCustomDimensionsResource;virtual;
    Function GetCustomMetricsInstance : TManagementCustomMetricsResource;virtual;
    Function GetExperimentsInstance : TManagementExperimentsResource;virtual;
    Function GetFiltersInstance : TManagementFiltersResource;virtual;
    Function GetGoalsInstance : TManagementGoalsResource;virtual;
    Function GetProfileFilterLinksInstance : TManagementProfileFilterLinksResource;virtual;
    Function GetProfileUserLinksInstance : TManagementProfileUserLinksResource;virtual;
    Function GetProfilesInstance : TManagementProfilesResource;virtual;
    Function GetSegmentsInstance : TManagementSegmentsResource;virtual;
    Function GetUnsampledReportsInstance : TManagementUnsampledReportsResource;virtual;
    Function GetUploadsInstance : TManagementUploadsResource;virtual;
    Function GetWebPropertyAdWordsLinksInstance : TManagementWebPropertyAdWordsLinksResource;virtual;
    Function GetWebpropertiesInstance : TManagementWebpropertiesResource;virtual;
    Function GetWebpropertyUserLinksInstance : TManagementWebpropertyUserLinksResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateAccountSummariesResource(AOwner : TComponent) : TManagementAccountSummariesResource;virtual;overload;
    Function CreateAccountSummariesResource : TManagementAccountSummariesResource;virtual;overload;
    Function CreateAccountUserLinksResource(AOwner : TComponent) : TManagementAccountUserLinksResource;virtual;overload;
    Function CreateAccountUserLinksResource : TManagementAccountUserLinksResource;virtual;overload;
    Function CreateAccountsResource(AOwner : TComponent) : TManagementAccountsResource;virtual;overload;
    Function CreateAccountsResource : TManagementAccountsResource;virtual;overload;
    Function CreateCustomDataSourcesResource(AOwner : TComponent) : TManagementCustomDataSourcesResource;virtual;overload;
    Function CreateCustomDataSourcesResource : TManagementCustomDataSourcesResource;virtual;overload;
    Function CreateCustomDimensionsResource(AOwner : TComponent) : TManagementCustomDimensionsResource;virtual;overload;
    Function CreateCustomDimensionsResource : TManagementCustomDimensionsResource;virtual;overload;
    Function CreateCustomMetricsResource(AOwner : TComponent) : TManagementCustomMetricsResource;virtual;overload;
    Function CreateCustomMetricsResource : TManagementCustomMetricsResource;virtual;overload;
    Function CreateExperimentsResource(AOwner : TComponent) : TManagementExperimentsResource;virtual;overload;
    Function CreateExperimentsResource : TManagementExperimentsResource;virtual;overload;
    Function CreateFiltersResource(AOwner : TComponent) : TManagementFiltersResource;virtual;overload;
    Function CreateFiltersResource : TManagementFiltersResource;virtual;overload;
    Function CreateGoalsResource(AOwner : TComponent) : TManagementGoalsResource;virtual;overload;
    Function CreateGoalsResource : TManagementGoalsResource;virtual;overload;
    Function CreateProfileFilterLinksResource(AOwner : TComponent) : TManagementProfileFilterLinksResource;virtual;overload;
    Function CreateProfileFilterLinksResource : TManagementProfileFilterLinksResource;virtual;overload;
    Function CreateProfileUserLinksResource(AOwner : TComponent) : TManagementProfileUserLinksResource;virtual;overload;
    Function CreateProfileUserLinksResource : TManagementProfileUserLinksResource;virtual;overload;
    Function CreateProfilesResource(AOwner : TComponent) : TManagementProfilesResource;virtual;overload;
    Function CreateProfilesResource : TManagementProfilesResource;virtual;overload;
    Function CreateSegmentsResource(AOwner : TComponent) : TManagementSegmentsResource;virtual;overload;
    Function CreateSegmentsResource : TManagementSegmentsResource;virtual;overload;
    Function CreateUnsampledReportsResource(AOwner : TComponent) : TManagementUnsampledReportsResource;virtual;overload;
    Function CreateUnsampledReportsResource : TManagementUnsampledReportsResource;virtual;overload;
    Function CreateUploadsResource(AOwner : TComponent) : TManagementUploadsResource;virtual;overload;
    Function CreateUploadsResource : TManagementUploadsResource;virtual;overload;
    Function CreateWebPropertyAdWordsLinksResource(AOwner : TComponent) : TManagementWebPropertyAdWordsLinksResource;virtual;overload;
    Function CreateWebPropertyAdWordsLinksResource : TManagementWebPropertyAdWordsLinksResource;virtual;overload;
    Function CreateWebpropertiesResource(AOwner : TComponent) : TManagementWebpropertiesResource;virtual;overload;
    Function CreateWebpropertiesResource : TManagementWebpropertiesResource;virtual;overload;
    Function CreateWebpropertyUserLinksResource(AOwner : TComponent) : TManagementWebpropertyUserLinksResource;virtual;overload;
    Function CreateWebpropertyUserLinksResource : TManagementWebpropertyUserLinksResource;virtual;overload;
    Property AccountSummariesResource : TManagementAccountSummariesResource Read GetAccountSummariesInstance;
    Property AccountUserLinksResource : TManagementAccountUserLinksResource Read GetAccountUserLinksInstance;
    Property AccountsResource : TManagementAccountsResource Read GetAccountsInstance;
    Property CustomDataSourcesResource : TManagementCustomDataSourcesResource Read GetCustomDataSourcesInstance;
    Property CustomDimensionsResource : TManagementCustomDimensionsResource Read GetCustomDimensionsInstance;
    Property CustomMetricsResource : TManagementCustomMetricsResource Read GetCustomMetricsInstance;
    Property ExperimentsResource : TManagementExperimentsResource Read GetExperimentsInstance;
    Property FiltersResource : TManagementFiltersResource Read GetFiltersInstance;
    Property GoalsResource : TManagementGoalsResource Read GetGoalsInstance;
    Property ProfileFilterLinksResource : TManagementProfileFilterLinksResource Read GetProfileFilterLinksInstance;
    Property ProfileUserLinksResource : TManagementProfileUserLinksResource Read GetProfileUserLinksInstance;
    Property ProfilesResource : TManagementProfilesResource Read GetProfilesInstance;
    Property SegmentsResource : TManagementSegmentsResource Read GetSegmentsInstance;
    Property UnsampledReportsResource : TManagementUnsampledReportsResource Read GetUnsampledReportsInstance;
    Property UploadsResource : TManagementUploadsResource Read GetUploadsInstance;
    Property WebPropertyAdWordsLinksResource : TManagementWebPropertyAdWordsLinksResource Read GetWebPropertyAdWordsLinksInstance;
    Property WebpropertiesResource : TManagementWebpropertiesResource Read GetWebpropertiesInstance;
    Property WebpropertyUserLinksResource : TManagementWebpropertyUserLinksResource Read GetWebpropertyUserLinksInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TMetadataColumnsResource
    --------------------------------------------------------------------}
  
  TMetadataColumnsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(reportType: string) : TColumns;
  end;
  
  
  { --------------------------------------------------------------------
    TMetadataResource
    --------------------------------------------------------------------}
  
  TMetadataResource = Class(TGoogleResource)
  Private
    FColumnsInstance : TMetadataColumnsResource;
    Function GetColumnsInstance : TMetadataColumnsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateColumnsResource(AOwner : TComponent) : TMetadataColumnsResource;virtual;overload;
    Function CreateColumnsResource : TMetadataColumnsResource;virtual;overload;
    Property ColumnsResource : TMetadataColumnsResource Read GetColumnsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TProvisioningResource
    --------------------------------------------------------------------}
  
  TProvisioningResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateAccountTicket(aAccountTicket : TAccountTicket) : TAccountTicket;
  end;
  
  
  { --------------------------------------------------------------------
    TAnalyticsAPI
    --------------------------------------------------------------------}
  
  TAnalyticsAPI = Class(TGoogleAPI)
  Private
    FDataGaInstance : TDataGaResource;
    FDataMcfInstance : TDataMcfResource;
    FDataRealtimeInstance : TDataRealtimeResource;
    FDataInstance : TDataResource;
    FManagementAccountSummariesInstance : TManagementAccountSummariesResource;
    FManagementAccountUserLinksInstance : TManagementAccountUserLinksResource;
    FManagementAccountsInstance : TManagementAccountsResource;
    FManagementCustomDataSourcesInstance : TManagementCustomDataSourcesResource;
    FManagementCustomDimensionsInstance : TManagementCustomDimensionsResource;
    FManagementCustomMetricsInstance : TManagementCustomMetricsResource;
    FManagementExperimentsInstance : TManagementExperimentsResource;
    FManagementFiltersInstance : TManagementFiltersResource;
    FManagementGoalsInstance : TManagementGoalsResource;
    FManagementProfileFilterLinksInstance : TManagementProfileFilterLinksResource;
    FManagementProfileUserLinksInstance : TManagementProfileUserLinksResource;
    FManagementProfilesInstance : TManagementProfilesResource;
    FManagementSegmentsInstance : TManagementSegmentsResource;
    FManagementUnsampledReportsInstance : TManagementUnsampledReportsResource;
    FManagementUploadsInstance : TManagementUploadsResource;
    FManagementWebPropertyAdWordsLinksInstance : TManagementWebPropertyAdWordsLinksResource;
    FManagementWebpropertiesInstance : TManagementWebpropertiesResource;
    FManagementWebpropertyUserLinksInstance : TManagementWebpropertyUserLinksResource;
    FManagementInstance : TManagementResource;
    FMetadataColumnsInstance : TMetadataColumnsResource;
    FMetadataInstance : TMetadataResource;
    FProvisioningInstance : TProvisioningResource;
    Function GetDataGaInstance : TDataGaResource;virtual;
    Function GetDataMcfInstance : TDataMcfResource;virtual;
    Function GetDataRealtimeInstance : TDataRealtimeResource;virtual;
    Function GetDataInstance : TDataResource;virtual;
    Function GetManagementAccountSummariesInstance : TManagementAccountSummariesResource;virtual;
    Function GetManagementAccountUserLinksInstance : TManagementAccountUserLinksResource;virtual;
    Function GetManagementAccountsInstance : TManagementAccountsResource;virtual;
    Function GetManagementCustomDataSourcesInstance : TManagementCustomDataSourcesResource;virtual;
    Function GetManagementCustomDimensionsInstance : TManagementCustomDimensionsResource;virtual;
    Function GetManagementCustomMetricsInstance : TManagementCustomMetricsResource;virtual;
    Function GetManagementExperimentsInstance : TManagementExperimentsResource;virtual;
    Function GetManagementFiltersInstance : TManagementFiltersResource;virtual;
    Function GetManagementGoalsInstance : TManagementGoalsResource;virtual;
    Function GetManagementProfileFilterLinksInstance : TManagementProfileFilterLinksResource;virtual;
    Function GetManagementProfileUserLinksInstance : TManagementProfileUserLinksResource;virtual;
    Function GetManagementProfilesInstance : TManagementProfilesResource;virtual;
    Function GetManagementSegmentsInstance : TManagementSegmentsResource;virtual;
    Function GetManagementUnsampledReportsInstance : TManagementUnsampledReportsResource;virtual;
    Function GetManagementUploadsInstance : TManagementUploadsResource;virtual;
    Function GetManagementWebPropertyAdWordsLinksInstance : TManagementWebPropertyAdWordsLinksResource;virtual;
    Function GetManagementWebpropertiesInstance : TManagementWebpropertiesResource;virtual;
    Function GetManagementWebpropertyUserLinksInstance : TManagementWebpropertyUserLinksResource;virtual;
    Function GetManagementInstance : TManagementResource;virtual;
    Function GetMetadataColumnsInstance : TMetadataColumnsResource;virtual;
    Function GetMetadataInstance : TMetadataResource;virtual;
    Function GetProvisioningInstance : TProvisioningResource;virtual;
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
    Function CreateDataGaResource(AOwner : TComponent) : TDataGaResource;virtual;overload;
    Function CreateDataGaResource : TDataGaResource;virtual;overload;
    Function CreateDataMcfResource(AOwner : TComponent) : TDataMcfResource;virtual;overload;
    Function CreateDataMcfResource : TDataMcfResource;virtual;overload;
    Function CreateDataRealtimeResource(AOwner : TComponent) : TDataRealtimeResource;virtual;overload;
    Function CreateDataRealtimeResource : TDataRealtimeResource;virtual;overload;
    Function CreateDataResource(AOwner : TComponent) : TDataResource;virtual;overload;
    Function CreateDataResource : TDataResource;virtual;overload;
    Function CreateManagementAccountSummariesResource(AOwner : TComponent) : TManagementAccountSummariesResource;virtual;overload;
    Function CreateManagementAccountSummariesResource : TManagementAccountSummariesResource;virtual;overload;
    Function CreateManagementAccountUserLinksResource(AOwner : TComponent) : TManagementAccountUserLinksResource;virtual;overload;
    Function CreateManagementAccountUserLinksResource : TManagementAccountUserLinksResource;virtual;overload;
    Function CreateManagementAccountsResource(AOwner : TComponent) : TManagementAccountsResource;virtual;overload;
    Function CreateManagementAccountsResource : TManagementAccountsResource;virtual;overload;
    Function CreateManagementCustomDataSourcesResource(AOwner : TComponent) : TManagementCustomDataSourcesResource;virtual;overload;
    Function CreateManagementCustomDataSourcesResource : TManagementCustomDataSourcesResource;virtual;overload;
    Function CreateManagementCustomDimensionsResource(AOwner : TComponent) : TManagementCustomDimensionsResource;virtual;overload;
    Function CreateManagementCustomDimensionsResource : TManagementCustomDimensionsResource;virtual;overload;
    Function CreateManagementCustomMetricsResource(AOwner : TComponent) : TManagementCustomMetricsResource;virtual;overload;
    Function CreateManagementCustomMetricsResource : TManagementCustomMetricsResource;virtual;overload;
    Function CreateManagementExperimentsResource(AOwner : TComponent) : TManagementExperimentsResource;virtual;overload;
    Function CreateManagementExperimentsResource : TManagementExperimentsResource;virtual;overload;
    Function CreateManagementFiltersResource(AOwner : TComponent) : TManagementFiltersResource;virtual;overload;
    Function CreateManagementFiltersResource : TManagementFiltersResource;virtual;overload;
    Function CreateManagementGoalsResource(AOwner : TComponent) : TManagementGoalsResource;virtual;overload;
    Function CreateManagementGoalsResource : TManagementGoalsResource;virtual;overload;
    Function CreateManagementProfileFilterLinksResource(AOwner : TComponent) : TManagementProfileFilterLinksResource;virtual;overload;
    Function CreateManagementProfileFilterLinksResource : TManagementProfileFilterLinksResource;virtual;overload;
    Function CreateManagementProfileUserLinksResource(AOwner : TComponent) : TManagementProfileUserLinksResource;virtual;overload;
    Function CreateManagementProfileUserLinksResource : TManagementProfileUserLinksResource;virtual;overload;
    Function CreateManagementProfilesResource(AOwner : TComponent) : TManagementProfilesResource;virtual;overload;
    Function CreateManagementProfilesResource : TManagementProfilesResource;virtual;overload;
    Function CreateManagementSegmentsResource(AOwner : TComponent) : TManagementSegmentsResource;virtual;overload;
    Function CreateManagementSegmentsResource : TManagementSegmentsResource;virtual;overload;
    Function CreateManagementUnsampledReportsResource(AOwner : TComponent) : TManagementUnsampledReportsResource;virtual;overload;
    Function CreateManagementUnsampledReportsResource : TManagementUnsampledReportsResource;virtual;overload;
    Function CreateManagementUploadsResource(AOwner : TComponent) : TManagementUploadsResource;virtual;overload;
    Function CreateManagementUploadsResource : TManagementUploadsResource;virtual;overload;
    Function CreateManagementWebPropertyAdWordsLinksResource(AOwner : TComponent) : TManagementWebPropertyAdWordsLinksResource;virtual;overload;
    Function CreateManagementWebPropertyAdWordsLinksResource : TManagementWebPropertyAdWordsLinksResource;virtual;overload;
    Function CreateManagementWebpropertiesResource(AOwner : TComponent) : TManagementWebpropertiesResource;virtual;overload;
    Function CreateManagementWebpropertiesResource : TManagementWebpropertiesResource;virtual;overload;
    Function CreateManagementWebpropertyUserLinksResource(AOwner : TComponent) : TManagementWebpropertyUserLinksResource;virtual;overload;
    Function CreateManagementWebpropertyUserLinksResource : TManagementWebpropertyUserLinksResource;virtual;overload;
    Function CreateManagementResource(AOwner : TComponent) : TManagementResource;virtual;overload;
    Function CreateManagementResource : TManagementResource;virtual;overload;
    Function CreateMetadataColumnsResource(AOwner : TComponent) : TMetadataColumnsResource;virtual;overload;
    Function CreateMetadataColumnsResource : TMetadataColumnsResource;virtual;overload;
    Function CreateMetadataResource(AOwner : TComponent) : TMetadataResource;virtual;overload;
    Function CreateMetadataResource : TMetadataResource;virtual;overload;
    Function CreateProvisioningResource(AOwner : TComponent) : TProvisioningResource;virtual;overload;
    Function CreateProvisioningResource : TProvisioningResource;virtual;overload;
    //Add default on-demand instances for resources
    Property DataGaResource : TDataGaResource Read GetDataGaInstance;
    Property DataMcfResource : TDataMcfResource Read GetDataMcfInstance;
    Property DataRealtimeResource : TDataRealtimeResource Read GetDataRealtimeInstance;
    Property DataResource : TDataResource Read GetDataInstance;
    Property ManagementAccountSummariesResource : TManagementAccountSummariesResource Read GetManagementAccountSummariesInstance;
    Property ManagementAccountUserLinksResource : TManagementAccountUserLinksResource Read GetManagementAccountUserLinksInstance;
    Property ManagementAccountsResource : TManagementAccountsResource Read GetManagementAccountsInstance;
    Property ManagementCustomDataSourcesResource : TManagementCustomDataSourcesResource Read GetManagementCustomDataSourcesInstance;
    Property ManagementCustomDimensionsResource : TManagementCustomDimensionsResource Read GetManagementCustomDimensionsInstance;
    Property ManagementCustomMetricsResource : TManagementCustomMetricsResource Read GetManagementCustomMetricsInstance;
    Property ManagementExperimentsResource : TManagementExperimentsResource Read GetManagementExperimentsInstance;
    Property ManagementFiltersResource : TManagementFiltersResource Read GetManagementFiltersInstance;
    Property ManagementGoalsResource : TManagementGoalsResource Read GetManagementGoalsInstance;
    Property ManagementProfileFilterLinksResource : TManagementProfileFilterLinksResource Read GetManagementProfileFilterLinksInstance;
    Property ManagementProfileUserLinksResource : TManagementProfileUserLinksResource Read GetManagementProfileUserLinksInstance;
    Property ManagementProfilesResource : TManagementProfilesResource Read GetManagementProfilesInstance;
    Property ManagementSegmentsResource : TManagementSegmentsResource Read GetManagementSegmentsInstance;
    Property ManagementUnsampledReportsResource : TManagementUnsampledReportsResource Read GetManagementUnsampledReportsInstance;
    Property ManagementUploadsResource : TManagementUploadsResource Read GetManagementUploadsInstance;
    Property ManagementWebPropertyAdWordsLinksResource : TManagementWebPropertyAdWordsLinksResource Read GetManagementWebPropertyAdWordsLinksInstance;
    Property ManagementWebpropertiesResource : TManagementWebpropertiesResource Read GetManagementWebpropertiesInstance;
    Property ManagementWebpropertyUserLinksResource : TManagementWebpropertyUserLinksResource Read GetManagementWebpropertyUserLinksInstance;
    Property ManagementResource : TManagementResource Read GetManagementInstance;
    Property MetadataColumnsResource : TMetadataColumnsResource Read GetMetadataColumnsInstance;
    Property MetadataResource : TMetadataResource Read GetMetadataInstance;
    Property ProvisioningResource : TProvisioningResource Read GetProvisioningInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAccountTypechildLink
  --------------------------------------------------------------------}


Procedure TAccountTypechildLink.Sethref(AIndex : Integer; AValue : String); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTypechildLink.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAccountTypechildLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAccountTypepermissions
  --------------------------------------------------------------------}


Procedure TAccountTypepermissions.Seteffective(AIndex : Integer; AValue : TStringArray); 

begin
  If (Feffective=AValue) then exit;
  Feffective:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccountTypepermissions.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'effective' : SetLength(Feffective,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccount
  --------------------------------------------------------------------}


Procedure TAccount.SetchildLink(AIndex : Integer; AValue : TAccountTypechildLink); 

begin
  If (FchildLink=AValue) then exit;
  FchildLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setcreated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
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



Procedure TAccount.Setpermissions(AIndex : Integer; AValue : TAccountTypepermissions); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountRef
  --------------------------------------------------------------------}


Procedure TAccountRef.Sethref(AIndex : Integer; AValue : String); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountRef.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountRef.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountRef.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountSummaries
  --------------------------------------------------------------------}


Procedure TAccountSummaries.Setitems(AIndex : Integer; AValue : TAccountSummariesTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountSummaries.SetitemsPerPage(AIndex : Integer; AValue : integer); 

begin
  If (FitemsPerPage=AValue) then exit;
  FitemsPerPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountSummaries.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountSummaries.SetnextLink(AIndex : Integer; AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountSummaries.SetpreviousLink(AIndex : Integer; AValue : String); 

begin
  If (FpreviousLink=AValue) then exit;
  FpreviousLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountSummaries.SetstartIndex(AIndex : Integer; AValue : integer); 

begin
  If (FstartIndex=AValue) then exit;
  FstartIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountSummaries.SettotalResults(AIndex : Integer; AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountSummaries.Setusername(AIndex : Integer; AValue : String); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccountSummaries.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccountSummary
  --------------------------------------------------------------------}


Procedure TAccountSummary.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountSummary.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountSummary.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountSummary.SetwebProperties(AIndex : Integer; AValue : TAccountSummaryTypewebPropertiesArray); 

begin
  If (FwebProperties=AValue) then exit;
  FwebProperties:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccountSummary.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'webproperties' : SetLength(FwebProperties,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccountTicket
  --------------------------------------------------------------------}


Procedure TAccountTicket.Setaccount(AIndex : Integer; AValue : TAccount); 

begin
  If (Faccount=AValue) then exit;
  Faccount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTicket.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTicket.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTicket.Setprofile(AIndex : Integer; AValue : TProfile); 

begin
  If (Fprofile=AValue) then exit;
  Fprofile:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTicket.SetredirectUri(AIndex : Integer; AValue : String); 

begin
  If (FredirectUri=AValue) then exit;
  FredirectUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTicket.Setwebproperty(AIndex : Integer; AValue : TWebproperty); 

begin
  If (Fwebproperty=AValue) then exit;
  Fwebproperty:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccounts
  --------------------------------------------------------------------}


Procedure TAccounts.Setitems(AIndex : Integer; AValue : TAccountsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounts.SetitemsPerPage(AIndex : Integer; AValue : integer); 

begin
  If (FitemsPerPage=AValue) then exit;
  FitemsPerPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounts.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounts.SetnextLink(AIndex : Integer; AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounts.SetpreviousLink(AIndex : Integer; AValue : String); 

begin
  If (FpreviousLink=AValue) then exit;
  FpreviousLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounts.SetstartIndex(AIndex : Integer; AValue : integer); 

begin
  If (FstartIndex=AValue) then exit;
  FstartIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounts.SettotalResults(AIndex : Integer; AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounts.Setusername(AIndex : Integer; AValue : String); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccounts.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAdWordsAccount
  --------------------------------------------------------------------}


Procedure TAdWordsAccount.SetautoTaggingEnabled(AIndex : Integer; AValue : boolean); 

begin
  If (FautoTaggingEnabled=AValue) then exit;
  FautoTaggingEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdWordsAccount.SetcustomerId(AIndex : Integer; AValue : String); 

begin
  If (FcustomerId=AValue) then exit;
  FcustomerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdWordsAccount.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnalyticsDataimportDeleteUploadDataRequest
  --------------------------------------------------------------------}


Procedure TAnalyticsDataimportDeleteUploadDataRequest.SetcustomDataImportUids(AIndex : Integer; AValue : TStringArray); 

begin
  If (FcustomDataImportUids=AValue) then exit;
  FcustomDataImportUids:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAnalyticsDataimportDeleteUploadDataRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'customdataimportuids' : SetLength(FcustomDataImportUids,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TColumnTypeattributes
  --------------------------------------------------------------------}


Class Function TColumnTypeattributes.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TColumn
  --------------------------------------------------------------------}


Procedure TColumn.Setattributes(AIndex : Integer; AValue : TColumnTypeattributes); 

begin
  If (Fattributes=AValue) then exit;
  Fattributes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TColumns
  --------------------------------------------------------------------}


Procedure TColumns.SetattributeNames(AIndex : Integer; AValue : TStringArray); 

begin
  If (FattributeNames=AValue) then exit;
  FattributeNames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumns.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumns.Setitems(AIndex : Integer; AValue : TColumnsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumns.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumns.SettotalResults(AIndex : Integer; AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TColumns.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'attributenames' : SetLength(FattributeNames,ALength);
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCustomDataSourceTypechildLink
  --------------------------------------------------------------------}


Procedure TCustomDataSourceTypechildLink.Sethref(AIndex : Integer; AValue : String); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSourceTypechildLink.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCustomDataSourceTypechildLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TCustomDataSourceTypeparentLink
  --------------------------------------------------------------------}


Procedure TCustomDataSourceTypeparentLink.Sethref(AIndex : Integer; AValue : String); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSourceTypeparentLink.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCustomDataSourceTypeparentLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TCustomDataSource
  --------------------------------------------------------------------}


Procedure TCustomDataSource.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.SetchildLink(AIndex : Integer; AValue : TCustomDataSourceTypechildLink); 

begin
  If (FchildLink=AValue) then exit;
  FchildLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.Setcreated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.SetimportBehavior(AIndex : Integer; AValue : String); 

begin
  If (FimportBehavior=AValue) then exit;
  FimportBehavior:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.SetparentLink(AIndex : Integer; AValue : TCustomDataSourceTypeparentLink); 

begin
  If (FparentLink=AValue) then exit;
  FparentLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.SetprofilesLinked(AIndex : Integer; AValue : TStringArray); 

begin
  If (FprofilesLinked=AValue) then exit;
  FprofilesLinked:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.SetuploadType(AIndex : Integer; AValue : String); 

begin
  If (FuploadType=AValue) then exit;
  FuploadType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.SetwebPropertyId(AIndex : Integer; AValue : String); 

begin
  If (FwebPropertyId=AValue) then exit;
  FwebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCustomDataSource.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCustomDataSource.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'profileslinked' : SetLength(FprofilesLinked,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCustomDataSources
  --------------------------------------------------------------------}


Procedure TCustomDataSources.Setitems(AIndex : Integer; AValue : TCustomDataSourcesTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSources.SetitemsPerPage(AIndex : Integer; AValue : integer); 

begin
  If (FitemsPerPage=AValue) then exit;
  FitemsPerPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSources.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSources.SetnextLink(AIndex : Integer; AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSources.SetpreviousLink(AIndex : Integer; AValue : String); 

begin
  If (FpreviousLink=AValue) then exit;
  FpreviousLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSources.SetstartIndex(AIndex : Integer; AValue : integer); 

begin
  If (FstartIndex=AValue) then exit;
  FstartIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSources.SettotalResults(AIndex : Integer; AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSources.Setusername(AIndex : Integer; AValue : String); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCustomDataSources.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCustomDimensionTypeparentLink
  --------------------------------------------------------------------}


Procedure TCustomDimensionTypeparentLink.Sethref(AIndex : Integer; AValue : String); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimensionTypeparentLink.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCustomDimensionTypeparentLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TCustomDimension
  --------------------------------------------------------------------}


Procedure TCustomDimension.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimension.Setactive(AIndex : Integer; AValue : boolean); 

begin
  If (Factive=AValue) then exit;
  Factive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimension.Setcreated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimension.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimension.Setindex(AIndex : Integer; AValue : integer); 

begin
  If (Findex=AValue) then exit;
  Findex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimension.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimension.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimension.SetparentLink(AIndex : Integer; AValue : TCustomDimensionTypeparentLink); 

begin
  If (FparentLink=AValue) then exit;
  FparentLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimension.Setscope(AIndex : Integer; AValue : String); 

begin
  If (Fscope=AValue) then exit;
  Fscope:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimension.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimension.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimension.SetwebPropertyId(AIndex : Integer; AValue : String); 

begin
  If (FwebPropertyId=AValue) then exit;
  FwebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCustomDimensions
  --------------------------------------------------------------------}


Procedure TCustomDimensions.Setitems(AIndex : Integer; AValue : TCustomDimensionsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimensions.SetitemsPerPage(AIndex : Integer; AValue : integer); 

begin
  If (FitemsPerPage=AValue) then exit;
  FitemsPerPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimensions.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimensions.SetnextLink(AIndex : Integer; AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimensions.SetpreviousLink(AIndex : Integer; AValue : String); 

begin
  If (FpreviousLink=AValue) then exit;
  FpreviousLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimensions.SetstartIndex(AIndex : Integer; AValue : integer); 

begin
  If (FstartIndex=AValue) then exit;
  FstartIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimensions.SettotalResults(AIndex : Integer; AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimensions.Setusername(AIndex : Integer; AValue : String); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCustomDimensions.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCustomMetricTypeparentLink
  --------------------------------------------------------------------}


Procedure TCustomMetricTypeparentLink.Sethref(AIndex : Integer; AValue : String); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetricTypeparentLink.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCustomMetricTypeparentLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TCustomMetric
  --------------------------------------------------------------------}


Procedure TCustomMetric.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetric.Setactive(AIndex : Integer; AValue : boolean); 

begin
  If (Factive=AValue) then exit;
  Factive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetric.Setcreated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetric.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetric.Setindex(AIndex : Integer; AValue : integer); 

begin
  If (Findex=AValue) then exit;
  Findex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetric.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetric.Setmax_value(AIndex : Integer; AValue : String); 

begin
  If (Fmax_value=AValue) then exit;
  Fmax_value:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetric.Setmin_value(AIndex : Integer; AValue : String); 

begin
  If (Fmin_value=AValue) then exit;
  Fmin_value:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetric.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetric.SetparentLink(AIndex : Integer; AValue : TCustomMetricTypeparentLink); 

begin
  If (FparentLink=AValue) then exit;
  FparentLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetric.Setscope(AIndex : Integer; AValue : String); 

begin
  If (Fscope=AValue) then exit;
  Fscope:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetric.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetric.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetric.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetric.SetwebPropertyId(AIndex : Integer; AValue : String); 

begin
  If (FwebPropertyId=AValue) then exit;
  FwebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCustomMetric.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TCustomMetrics
  --------------------------------------------------------------------}


Procedure TCustomMetrics.Setitems(AIndex : Integer; AValue : TCustomMetricsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetrics.SetitemsPerPage(AIndex : Integer; AValue : integer); 

begin
  If (FitemsPerPage=AValue) then exit;
  FitemsPerPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetrics.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetrics.SetnextLink(AIndex : Integer; AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetrics.SetpreviousLink(AIndex : Integer; AValue : String); 

begin
  If (FpreviousLink=AValue) then exit;
  FpreviousLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetrics.SetstartIndex(AIndex : Integer; AValue : integer); 

begin
  If (FstartIndex=AValue) then exit;
  FstartIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetrics.SettotalResults(AIndex : Integer; AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetrics.Setusername(AIndex : Integer; AValue : String); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCustomMetrics.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEntityAdWordsLinkTypeentity
  --------------------------------------------------------------------}


Procedure TEntityAdWordsLinkTypeentity.SetwebPropertyRef(AIndex : Integer; AValue : TWebPropertyRef); 

begin
  If (FwebPropertyRef=AValue) then exit;
  FwebPropertyRef:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEntityAdWordsLink
  --------------------------------------------------------------------}


Procedure TEntityAdWordsLink.SetadWordsAccounts(AIndex : Integer; AValue : TEntityAdWordsLinkTypeadWordsAccountsArray); 

begin
  If (FadWordsAccounts=AValue) then exit;
  FadWordsAccounts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAdWordsLink.Setentity(AIndex : Integer; AValue : TEntityAdWordsLinkTypeentity); 

begin
  If (Fentity=AValue) then exit;
  Fentity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAdWordsLink.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAdWordsLink.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAdWordsLink.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAdWordsLink.SetprofileIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FprofileIds=AValue) then exit;
  FprofileIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAdWordsLink.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TEntityAdWordsLink.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'adwordsaccounts' : SetLength(FadWordsAccounts,ALength);
  'profileids' : SetLength(FprofileIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEntityAdWordsLinks
  --------------------------------------------------------------------}


Procedure TEntityAdWordsLinks.Setitems(AIndex : Integer; AValue : TEntityAdWordsLinksTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAdWordsLinks.SetitemsPerPage(AIndex : Integer; AValue : integer); 

begin
  If (FitemsPerPage=AValue) then exit;
  FitemsPerPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAdWordsLinks.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAdWordsLinks.SetnextLink(AIndex : Integer; AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAdWordsLinks.SetpreviousLink(AIndex : Integer; AValue : String); 

begin
  If (FpreviousLink=AValue) then exit;
  FpreviousLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAdWordsLinks.SetstartIndex(AIndex : Integer; AValue : integer); 

begin
  If (FstartIndex=AValue) then exit;
  FstartIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAdWordsLinks.SettotalResults(AIndex : Integer; AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TEntityAdWordsLinks.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEntityUserLinkTypeentity
  --------------------------------------------------------------------}


Procedure TEntityUserLinkTypeentity.SetaccountRef(AIndex : Integer; AValue : TAccountRef); 

begin
  If (FaccountRef=AValue) then exit;
  FaccountRef:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLinkTypeentity.SetprofileRef(AIndex : Integer; AValue : TProfileRef); 

begin
  If (FprofileRef=AValue) then exit;
  FprofileRef:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLinkTypeentity.SetwebPropertyRef(AIndex : Integer; AValue : TWebPropertyRef); 

begin
  If (FwebPropertyRef=AValue) then exit;
  FwebPropertyRef:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEntityUserLinkTypepermissions
  --------------------------------------------------------------------}


Procedure TEntityUserLinkTypepermissions.Seteffective(AIndex : Integer; AValue : TStringArray); 

begin
  If (Feffective=AValue) then exit;
  Feffective:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLinkTypepermissions.Setlocal(AIndex : Integer; AValue : TStringArray); 

begin
  If (Flocal=AValue) then exit;
  Flocal:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TEntityUserLinkTypepermissions.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'effective' : SetLength(Feffective,ALength);
  'local' : SetLength(Flocal,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEntityUserLink
  --------------------------------------------------------------------}


Procedure TEntityUserLink.Setentity(AIndex : Integer; AValue : TEntityUserLinkTypeentity); 

begin
  If (Fentity=AValue) then exit;
  Fentity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLink.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLink.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLink.Setpermissions(AIndex : Integer; AValue : TEntityUserLinkTypepermissions); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLink.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLink.SetuserRef(AIndex : Integer; AValue : TUserRef); 

begin
  If (FuserRef=AValue) then exit;
  FuserRef:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEntityUserLinks
  --------------------------------------------------------------------}


Procedure TEntityUserLinks.Setitems(AIndex : Integer; AValue : TEntityUserLinksTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLinks.SetitemsPerPage(AIndex : Integer; AValue : integer); 

begin
  If (FitemsPerPage=AValue) then exit;
  FitemsPerPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLinks.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLinks.SetnextLink(AIndex : Integer; AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLinks.SetpreviousLink(AIndex : Integer; AValue : String); 

begin
  If (FpreviousLink=AValue) then exit;
  FpreviousLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLinks.SetstartIndex(AIndex : Integer; AValue : integer); 

begin
  If (FstartIndex=AValue) then exit;
  FstartIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLinks.SettotalResults(AIndex : Integer; AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TEntityUserLinks.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TExperimentTypeparentLink
  --------------------------------------------------------------------}


Procedure TExperimentTypeparentLink.Sethref(AIndex : Integer; AValue : String); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperimentTypeparentLink.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TExperimentTypeparentLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TExperimentTypevariationsItem
  --------------------------------------------------------------------}


Procedure TExperimentTypevariationsItem.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperimentTypevariationsItem.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperimentTypevariationsItem.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperimentTypevariationsItem.Setweight(AIndex : Integer; AValue : double); 

begin
  If (Fweight=AValue) then exit;
  Fweight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperimentTypevariationsItem.Setwon(AIndex : Integer; AValue : boolean); 

begin
  If (Fwon=AValue) then exit;
  Fwon:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExperiment
  --------------------------------------------------------------------}


Procedure TExperiment.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.Setcreated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SeteditableInGaUi(AIndex : Integer; AValue : boolean); 

begin
  If (FeditableInGaUi=AValue) then exit;
  FeditableInGaUi:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetendTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetequalWeighting(AIndex : Integer; AValue : boolean); 

begin
  If (FequalWeighting=AValue) then exit;
  FequalWeighting:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetinternalWebPropertyId(AIndex : Integer; AValue : String); 

begin
  If (FinternalWebPropertyId=AValue) then exit;
  FinternalWebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetminimumExperimentLengthInDays(AIndex : Integer; AValue : integer); 

begin
  If (FminimumExperimentLengthInDays=AValue) then exit;
  FminimumExperimentLengthInDays:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetobjectiveMetric(AIndex : Integer; AValue : String); 

begin
  If (FobjectiveMetric=AValue) then exit;
  FobjectiveMetric:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetoptimizationType(AIndex : Integer; AValue : String); 

begin
  If (FoptimizationType=AValue) then exit;
  FoptimizationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetparentLink(AIndex : Integer; AValue : TExperimentTypeparentLink); 

begin
  If (FparentLink=AValue) then exit;
  FparentLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetprofileId(AIndex : Integer; AValue : String); 

begin
  If (FprofileId=AValue) then exit;
  FprofileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetreasonExperimentEnded(AIndex : Integer; AValue : String); 

begin
  If (FreasonExperimentEnded=AValue) then exit;
  FreasonExperimentEnded:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetrewriteVariationUrlsAsOriginal(AIndex : Integer; AValue : boolean); 

begin
  If (FrewriteVariationUrlsAsOriginal=AValue) then exit;
  FrewriteVariationUrlsAsOriginal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetservingFramework(AIndex : Integer; AValue : String); 

begin
  If (FservingFramework=AValue) then exit;
  FservingFramework:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.Setsnippet(AIndex : Integer; AValue : String); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetstartTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SettrafficCoverage(AIndex : Integer; AValue : double); 

begin
  If (FtrafficCoverage=AValue) then exit;
  FtrafficCoverage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.Setvariations(AIndex : Integer; AValue : TExperimentTypevariationsArray); 

begin
  If (Fvariations=AValue) then exit;
  Fvariations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetwebPropertyId(AIndex : Integer; AValue : String); 

begin
  If (FwebPropertyId=AValue) then exit;
  FwebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetwinnerConfidenceLevel(AIndex : Integer; AValue : double); 

begin
  If (FwinnerConfidenceLevel=AValue) then exit;
  FwinnerConfidenceLevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetwinnerFound(AIndex : Integer; AValue : boolean); 

begin
  If (FwinnerFound=AValue) then exit;
  FwinnerFound:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TExperiment.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'variations' : SetLength(Fvariations,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TExperiments
  --------------------------------------------------------------------}


Procedure TExperiments.Setitems(AIndex : Integer; AValue : TExperimentsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiments.SetitemsPerPage(AIndex : Integer; AValue : integer); 

begin
  If (FitemsPerPage=AValue) then exit;
  FitemsPerPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiments.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiments.SetnextLink(AIndex : Integer; AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiments.SetpreviousLink(AIndex : Integer; AValue : String); 

begin
  If (FpreviousLink=AValue) then exit;
  FpreviousLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiments.SetstartIndex(AIndex : Integer; AValue : integer); 

begin
  If (FstartIndex=AValue) then exit;
  FstartIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiments.SettotalResults(AIndex : Integer; AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiments.Setusername(AIndex : Integer; AValue : String); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TExperiments.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TFilterTypeadvancedDetails
  --------------------------------------------------------------------}


Procedure TFilterTypeadvancedDetails.SetcaseSensitive(AIndex : Integer; AValue : boolean); 

begin
  If (FcaseSensitive=AValue) then exit;
  FcaseSensitive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterTypeadvancedDetails.SetextractA(AIndex : Integer; AValue : String); 

begin
  If (FextractA=AValue) then exit;
  FextractA:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterTypeadvancedDetails.SetextractB(AIndex : Integer; AValue : String); 

begin
  If (FextractB=AValue) then exit;
  FextractB:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterTypeadvancedDetails.SetfieldA(AIndex : Integer; AValue : String); 

begin
  If (FfieldA=AValue) then exit;
  FfieldA:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterTypeadvancedDetails.SetfieldAIndex(AIndex : Integer; AValue : integer); 

begin
  If (FfieldAIndex=AValue) then exit;
  FfieldAIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterTypeadvancedDetails.SetfieldARequired(AIndex : Integer; AValue : boolean); 

begin
  If (FfieldARequired=AValue) then exit;
  FfieldARequired:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterTypeadvancedDetails.SetfieldB(AIndex : Integer; AValue : String); 

begin
  If (FfieldB=AValue) then exit;
  FfieldB:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterTypeadvancedDetails.SetfieldBIndex(AIndex : Integer; AValue : integer); 

begin
  If (FfieldBIndex=AValue) then exit;
  FfieldBIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterTypeadvancedDetails.SetfieldBRequired(AIndex : Integer; AValue : boolean); 

begin
  If (FfieldBRequired=AValue) then exit;
  FfieldBRequired:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterTypeadvancedDetails.SetoutputConstructor(AIndex : Integer; AValue : String); 

begin
  If (FoutputConstructor=AValue) then exit;
  FoutputConstructor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterTypeadvancedDetails.SetoutputToField(AIndex : Integer; AValue : String); 

begin
  If (FoutputToField=AValue) then exit;
  FoutputToField:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterTypeadvancedDetails.SetoutputToFieldIndex(AIndex : Integer; AValue : integer); 

begin
  If (FoutputToFieldIndex=AValue) then exit;
  FoutputToFieldIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterTypeadvancedDetails.SetoverrideOutputField(AIndex : Integer; AValue : boolean); 

begin
  If (FoverrideOutputField=AValue) then exit;
  FoverrideOutputField:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFilterTypelowercaseDetails
  --------------------------------------------------------------------}


Procedure TFilterTypelowercaseDetails.Setfield(AIndex : Integer; AValue : String); 

begin
  If (Ffield=AValue) then exit;
  Ffield:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterTypelowercaseDetails.SetfieldIndex(AIndex : Integer; AValue : integer); 

begin
  If (FfieldIndex=AValue) then exit;
  FfieldIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFilterTypeparentLink
  --------------------------------------------------------------------}


Procedure TFilterTypeparentLink.Sethref(AIndex : Integer; AValue : String); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterTypeparentLink.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TFilterTypeparentLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TFilterTypesearchAndReplaceDetails
  --------------------------------------------------------------------}


Procedure TFilterTypesearchAndReplaceDetails.SetcaseSensitive(AIndex : Integer; AValue : boolean); 

begin
  If (FcaseSensitive=AValue) then exit;
  FcaseSensitive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterTypesearchAndReplaceDetails.Setfield(AIndex : Integer; AValue : String); 

begin
  If (Ffield=AValue) then exit;
  Ffield:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterTypesearchAndReplaceDetails.SetfieldIndex(AIndex : Integer; AValue : integer); 

begin
  If (FfieldIndex=AValue) then exit;
  FfieldIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterTypesearchAndReplaceDetails.SetreplaceString(AIndex : Integer; AValue : String); 

begin
  If (FreplaceString=AValue) then exit;
  FreplaceString:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterTypesearchAndReplaceDetails.SetsearchString(AIndex : Integer; AValue : String); 

begin
  If (FsearchString=AValue) then exit;
  FsearchString:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFilterTypeuppercaseDetails
  --------------------------------------------------------------------}


Procedure TFilterTypeuppercaseDetails.Setfield(AIndex : Integer; AValue : String); 

begin
  If (Ffield=AValue) then exit;
  Ffield:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterTypeuppercaseDetails.SetfieldIndex(AIndex : Integer; AValue : integer); 

begin
  If (FfieldIndex=AValue) then exit;
  FfieldIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFilter
  --------------------------------------------------------------------}


Procedure TFilter.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.SetadvancedDetails(AIndex : Integer; AValue : TFilterTypeadvancedDetails); 

begin
  If (FadvancedDetails=AValue) then exit;
  FadvancedDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.Setcreated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.SetexcludeDetails(AIndex : Integer; AValue : TFilterExpression); 

begin
  If (FexcludeDetails=AValue) then exit;
  FexcludeDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.SetincludeDetails(AIndex : Integer; AValue : TFilterExpression); 

begin
  If (FincludeDetails=AValue) then exit;
  FincludeDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.SetlowercaseDetails(AIndex : Integer; AValue : TFilterTypelowercaseDetails); 

begin
  If (FlowercaseDetails=AValue) then exit;
  FlowercaseDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.SetparentLink(AIndex : Integer; AValue : TFilterTypeparentLink); 

begin
  If (FparentLink=AValue) then exit;
  FparentLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.SetsearchAndReplaceDetails(AIndex : Integer; AValue : TFilterTypesearchAndReplaceDetails); 

begin
  If (FsearchAndReplaceDetails=AValue) then exit;
  FsearchAndReplaceDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.SetuppercaseDetails(AIndex : Integer; AValue : TFilterTypeuppercaseDetails); 

begin
  If (FuppercaseDetails=AValue) then exit;
  FuppercaseDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TFilter.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TFilterExpression
  --------------------------------------------------------------------}


Procedure TFilterExpression.SetcaseSensitive(AIndex : Integer; AValue : boolean); 

begin
  If (FcaseSensitive=AValue) then exit;
  FcaseSensitive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterExpression.SetexpressionValue(AIndex : Integer; AValue : String); 

begin
  If (FexpressionValue=AValue) then exit;
  FexpressionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterExpression.Setfield(AIndex : Integer; AValue : String); 

begin
  If (Ffield=AValue) then exit;
  Ffield:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterExpression.SetfieldIndex(AIndex : Integer; AValue : integer); 

begin
  If (FfieldIndex=AValue) then exit;
  FfieldIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterExpression.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterExpression.SetmatchType(AIndex : Integer; AValue : String); 

begin
  If (FmatchType=AValue) then exit;
  FmatchType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFilterRef
  --------------------------------------------------------------------}


Procedure TFilterRef.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterRef.Sethref(AIndex : Integer; AValue : String); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterRef.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterRef.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterRef.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFilters
  --------------------------------------------------------------------}


Procedure TFilters.Setitems(AIndex : Integer; AValue : TFiltersTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilters.SetitemsPerPage(AIndex : Integer; AValue : integer); 

begin
  If (FitemsPerPage=AValue) then exit;
  FitemsPerPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilters.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilters.SetnextLink(AIndex : Integer; AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilters.SetpreviousLink(AIndex : Integer; AValue : String); 

begin
  If (FpreviousLink=AValue) then exit;
  FpreviousLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilters.SetstartIndex(AIndex : Integer; AValue : integer); 

begin
  If (FstartIndex=AValue) then exit;
  FstartIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilters.SettotalResults(AIndex : Integer; AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilters.Setusername(AIndex : Integer; AValue : String); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TFilters.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGaDataTypecolumnHeadersItem
  --------------------------------------------------------------------}


Procedure TGaDataTypecolumnHeadersItem.SetcolumnType(AIndex : Integer; AValue : String); 

begin
  If (FcolumnType=AValue) then exit;
  FcolumnType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataTypecolumnHeadersItem.SetdataType(AIndex : Integer; AValue : String); 

begin
  If (FdataType=AValue) then exit;
  FdataType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataTypecolumnHeadersItem.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGaDataTypedataTableTypecolsItem
  --------------------------------------------------------------------}


Procedure TGaDataTypedataTableTypecolsItem.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataTypedataTableTypecolsItem.Set_label(AIndex : Integer; AValue : String); 

begin
  If (F_label=AValue) then exit;
  F_label:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataTypedataTableTypecolsItem.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TGaDataTypedataTableTypecolsItem.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_label' : Result:='label';
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TGaDataTypedataTableTyperowsItemTypecItem
  --------------------------------------------------------------------}


Procedure TGaDataTypedataTableTyperowsItemTypecItem.Setv(AIndex : Integer; AValue : String); 

begin
  If (Fv=AValue) then exit;
  Fv:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGaDataTypedataTableTyperowsItem
  --------------------------------------------------------------------}


Procedure TGaDataTypedataTableTyperowsItem.Setc(AIndex : Integer; AValue : TGaDataTypedataTableTyperowsItemTypecArray); 

begin
  If (Fc=AValue) then exit;
  Fc:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGaDataTypedataTableTyperowsItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'c' : SetLength(Fc,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGaDataTypedataTable
  --------------------------------------------------------------------}


Procedure TGaDataTypedataTable.Setcols(AIndex : Integer; AValue : TGaDataTypedataTableTypecolsArray); 

begin
  If (Fcols=AValue) then exit;
  Fcols:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataTypedataTable.Setrows(AIndex : Integer; AValue : TGaDataTypedataTableTyperowsArray); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGaDataTypedataTable.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'cols' : SetLength(Fcols,ALength);
  'rows' : SetLength(Frows,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGaDataTypeprofileInfo
  --------------------------------------------------------------------}


Procedure TGaDataTypeprofileInfo.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataTypeprofileInfo.SetinternalWebPropertyId(AIndex : Integer; AValue : String); 

begin
  If (FinternalWebPropertyId=AValue) then exit;
  FinternalWebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataTypeprofileInfo.SetprofileId(AIndex : Integer; AValue : String); 

begin
  If (FprofileId=AValue) then exit;
  FprofileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataTypeprofileInfo.SetprofileName(AIndex : Integer; AValue : String); 

begin
  If (FprofileName=AValue) then exit;
  FprofileName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataTypeprofileInfo.SettableId(AIndex : Integer; AValue : String); 

begin
  If (FtableId=AValue) then exit;
  FtableId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataTypeprofileInfo.SetwebPropertyId(AIndex : Integer; AValue : String); 

begin
  If (FwebPropertyId=AValue) then exit;
  FwebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGaDataTypequery
  --------------------------------------------------------------------}


Procedure TGaDataTypequery.Setdimensions(AIndex : Integer; AValue : String); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataTypequery.Setenddate(AIndex : Integer; AValue : String); 

begin
  If (Fenddate=AValue) then exit;
  Fenddate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataTypequery.Setfilters(AIndex : Integer; AValue : String); 

begin
  If (Ffilters=AValue) then exit;
  Ffilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataTypequery.Setids(AIndex : Integer; AValue : String); 

begin
  If (Fids=AValue) then exit;
  Fids:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataTypequery.Setmaxresults(AIndex : Integer; AValue : integer); 

begin
  If (Fmaxresults=AValue) then exit;
  Fmaxresults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataTypequery.Setmetrics(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataTypequery.SetsamplingLevel(AIndex : Integer; AValue : String); 

begin
  If (FsamplingLevel=AValue) then exit;
  FsamplingLevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataTypequery.Setsegment(AIndex : Integer; AValue : String); 

begin
  If (Fsegment=AValue) then exit;
  Fsegment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataTypequery.Setsort(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fsort=AValue) then exit;
  Fsort:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataTypequery.Setstartdate(AIndex : Integer; AValue : String); 

begin
  If (Fstartdate=AValue) then exit;
  Fstartdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataTypequery.Setstartindex(AIndex : Integer; AValue : integer); 

begin
  If (Fstartindex=AValue) then exit;
  Fstartindex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TGaDataTypequery.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  'enddate' : Result:='end-date';
  'maxresults' : Result:='max-results';
  'startdate' : Result:='start-date';
  'startindex' : Result:='start-index';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGaDataTypequery.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'metrics' : SetLength(Fmetrics,ALength);
  'sort' : SetLength(Fsort,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGaDataTypetotalsForAllResults
  --------------------------------------------------------------------}


Class Function TGaDataTypetotalsForAllResults.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TGaData
  --------------------------------------------------------------------}


Procedure TGaData.SetcolumnHeaders(AIndex : Integer; AValue : TGaDataTypecolumnHeadersArray); 

begin
  If (FcolumnHeaders=AValue) then exit;
  FcolumnHeaders:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.SetcontainsSampledData(AIndex : Integer; AValue : boolean); 

begin
  If (FcontainsSampledData=AValue) then exit;
  FcontainsSampledData:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.SetdataTable(AIndex : Integer; AValue : TGaDataTypedataTable); 

begin
  If (FdataTable=AValue) then exit;
  FdataTable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.SetitemsPerPage(AIndex : Integer; AValue : integer); 

begin
  If (FitemsPerPage=AValue) then exit;
  FitemsPerPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.SetnextLink(AIndex : Integer; AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.SetpreviousLink(AIndex : Integer; AValue : String); 

begin
  If (FpreviousLink=AValue) then exit;
  FpreviousLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.SetprofileInfo(AIndex : Integer; AValue : TGaDataTypeprofileInfo); 

begin
  If (FprofileInfo=AValue) then exit;
  FprofileInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.Setquery(AIndex : Integer; AValue : TGaDataTypequery); 

begin
  If (Fquery=AValue) then exit;
  Fquery:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.Setrows(AIndex : Integer; AValue : TGaDataTyperowsArray); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.SetsampleSize(AIndex : Integer; AValue : String); 

begin
  If (FsampleSize=AValue) then exit;
  FsampleSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.SetsampleSpace(AIndex : Integer; AValue : String); 

begin
  If (FsampleSpace=AValue) then exit;
  FsampleSpace:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.SettotalResults(AIndex : Integer; AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.SettotalsForAllResults(AIndex : Integer; AValue : TGaDataTypetotalsForAllResults); 

begin
  If (FtotalsForAllResults=AValue) then exit;
  FtotalsForAllResults:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGaData.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'columnheaders' : SetLength(FcolumnHeaders,ALength);
  'rows' : SetLength(Frows,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGoalTypeeventDetailsTypeeventConditionsItem
  --------------------------------------------------------------------}


Procedure TGoalTypeeventDetailsTypeeventConditionsItem.SetcomparisonType(AIndex : Integer; AValue : String); 

begin
  If (FcomparisonType=AValue) then exit;
  FcomparisonType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalTypeeventDetailsTypeeventConditionsItem.SetcomparisonValue(AIndex : Integer; AValue : String); 

begin
  If (FcomparisonValue=AValue) then exit;
  FcomparisonValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalTypeeventDetailsTypeeventConditionsItem.Setexpression(AIndex : Integer; AValue : String); 

begin
  If (Fexpression=AValue) then exit;
  Fexpression:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalTypeeventDetailsTypeeventConditionsItem.SetmatchType(AIndex : Integer; AValue : String); 

begin
  If (FmatchType=AValue) then exit;
  FmatchType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalTypeeventDetailsTypeeventConditionsItem.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TGoalTypeeventDetailsTypeeventConditionsItem.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TGoalTypeeventDetails
  --------------------------------------------------------------------}


Procedure TGoalTypeeventDetails.SeteventConditions(AIndex : Integer; AValue : TGoalTypeeventDetailsTypeeventConditionsArray); 

begin
  If (FeventConditions=AValue) then exit;
  FeventConditions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalTypeeventDetails.SetuseEventValue(AIndex : Integer; AValue : boolean); 

begin
  If (FuseEventValue=AValue) then exit;
  FuseEventValue:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGoalTypeeventDetails.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'eventconditions' : SetLength(FeventConditions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGoalTypeparentLink
  --------------------------------------------------------------------}


Procedure TGoalTypeparentLink.Sethref(AIndex : Integer; AValue : String); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalTypeparentLink.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TGoalTypeparentLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TGoalTypeurlDestinationDetailsTypestepsItem
  --------------------------------------------------------------------}


Procedure TGoalTypeurlDestinationDetailsTypestepsItem.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalTypeurlDestinationDetailsTypestepsItem.Setnumber(AIndex : Integer; AValue : integer); 

begin
  If (Fnumber=AValue) then exit;
  Fnumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalTypeurlDestinationDetailsTypestepsItem.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGoalTypeurlDestinationDetails
  --------------------------------------------------------------------}


Procedure TGoalTypeurlDestinationDetails.SetcaseSensitive(AIndex : Integer; AValue : boolean); 

begin
  If (FcaseSensitive=AValue) then exit;
  FcaseSensitive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalTypeurlDestinationDetails.SetfirstStepRequired(AIndex : Integer; AValue : boolean); 

begin
  If (FfirstStepRequired=AValue) then exit;
  FfirstStepRequired:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalTypeurlDestinationDetails.SetmatchType(AIndex : Integer; AValue : String); 

begin
  If (FmatchType=AValue) then exit;
  FmatchType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalTypeurlDestinationDetails.Setsteps(AIndex : Integer; AValue : TGoalTypeurlDestinationDetailsTypestepsArray); 

begin
  If (Fsteps=AValue) then exit;
  Fsteps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalTypeurlDestinationDetails.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGoalTypeurlDestinationDetails.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'steps' : SetLength(Fsteps,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGoalTypevisitNumPagesDetails
  --------------------------------------------------------------------}


Procedure TGoalTypevisitNumPagesDetails.SetcomparisonType(AIndex : Integer; AValue : String); 

begin
  If (FcomparisonType=AValue) then exit;
  FcomparisonType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalTypevisitNumPagesDetails.SetcomparisonValue(AIndex : Integer; AValue : String); 

begin
  If (FcomparisonValue=AValue) then exit;
  FcomparisonValue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGoalTypevisitTimeOnSiteDetails
  --------------------------------------------------------------------}


Procedure TGoalTypevisitTimeOnSiteDetails.SetcomparisonType(AIndex : Integer; AValue : String); 

begin
  If (FcomparisonType=AValue) then exit;
  FcomparisonType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalTypevisitTimeOnSiteDetails.SetcomparisonValue(AIndex : Integer; AValue : String); 

begin
  If (FcomparisonValue=AValue) then exit;
  FcomparisonValue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGoal
  --------------------------------------------------------------------}


Procedure TGoal.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.Setactive(AIndex : Integer; AValue : boolean); 

begin
  If (Factive=AValue) then exit;
  Factive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.Setcreated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.SeteventDetails(AIndex : Integer; AValue : TGoalTypeeventDetails); 

begin
  If (FeventDetails=AValue) then exit;
  FeventDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.SetinternalWebPropertyId(AIndex : Integer; AValue : String); 

begin
  If (FinternalWebPropertyId=AValue) then exit;
  FinternalWebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.SetparentLink(AIndex : Integer; AValue : TGoalTypeparentLink); 

begin
  If (FparentLink=AValue) then exit;
  FparentLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.SetprofileId(AIndex : Integer; AValue : String); 

begin
  If (FprofileId=AValue) then exit;
  FprofileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.SeturlDestinationDetails(AIndex : Integer; AValue : TGoalTypeurlDestinationDetails); 

begin
  If (FurlDestinationDetails=AValue) then exit;
  FurlDestinationDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.Setvalue(AIndex : Integer; AValue : integer); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.SetvisitNumPagesDetails(AIndex : Integer; AValue : TGoalTypevisitNumPagesDetails); 

begin
  If (FvisitNumPagesDetails=AValue) then exit;
  FvisitNumPagesDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.SetvisitTimeOnSiteDetails(AIndex : Integer; AValue : TGoalTypevisitTimeOnSiteDetails); 

begin
  If (FvisitTimeOnSiteDetails=AValue) then exit;
  FvisitTimeOnSiteDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.SetwebPropertyId(AIndex : Integer; AValue : String); 

begin
  If (FwebPropertyId=AValue) then exit;
  FwebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TGoal.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TGoals
  --------------------------------------------------------------------}


Procedure TGoals.Setitems(AIndex : Integer; AValue : TGoalsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoals.SetitemsPerPage(AIndex : Integer; AValue : integer); 

begin
  If (FitemsPerPage=AValue) then exit;
  FitemsPerPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoals.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoals.SetnextLink(AIndex : Integer; AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoals.SetpreviousLink(AIndex : Integer; AValue : String); 

begin
  If (FpreviousLink=AValue) then exit;
  FpreviousLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoals.SetstartIndex(AIndex : Integer; AValue : integer); 

begin
  If (FstartIndex=AValue) then exit;
  FstartIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoals.SettotalResults(AIndex : Integer; AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoals.Setusername(AIndex : Integer; AValue : String); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGoals.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMcfDataTypecolumnHeadersItem
  --------------------------------------------------------------------}


Procedure TMcfDataTypecolumnHeadersItem.SetcolumnType(AIndex : Integer; AValue : String); 

begin
  If (FcolumnType=AValue) then exit;
  FcolumnType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataTypecolumnHeadersItem.SetdataType(AIndex : Integer; AValue : String); 

begin
  If (FdataType=AValue) then exit;
  FdataType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataTypecolumnHeadersItem.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMcfDataTypeprofileInfo
  --------------------------------------------------------------------}


Procedure TMcfDataTypeprofileInfo.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataTypeprofileInfo.SetinternalWebPropertyId(AIndex : Integer; AValue : String); 

begin
  If (FinternalWebPropertyId=AValue) then exit;
  FinternalWebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataTypeprofileInfo.SetprofileId(AIndex : Integer; AValue : String); 

begin
  If (FprofileId=AValue) then exit;
  FprofileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataTypeprofileInfo.SetprofileName(AIndex : Integer; AValue : String); 

begin
  If (FprofileName=AValue) then exit;
  FprofileName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataTypeprofileInfo.SettableId(AIndex : Integer; AValue : String); 

begin
  If (FtableId=AValue) then exit;
  FtableId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataTypeprofileInfo.SetwebPropertyId(AIndex : Integer; AValue : String); 

begin
  If (FwebPropertyId=AValue) then exit;
  FwebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMcfDataTypequery
  --------------------------------------------------------------------}


Procedure TMcfDataTypequery.Setdimensions(AIndex : Integer; AValue : String); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataTypequery.Setenddate(AIndex : Integer; AValue : String); 

begin
  If (Fenddate=AValue) then exit;
  Fenddate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataTypequery.Setfilters(AIndex : Integer; AValue : String); 

begin
  If (Ffilters=AValue) then exit;
  Ffilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataTypequery.Setids(AIndex : Integer; AValue : String); 

begin
  If (Fids=AValue) then exit;
  Fids:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataTypequery.Setmaxresults(AIndex : Integer; AValue : integer); 

begin
  If (Fmaxresults=AValue) then exit;
  Fmaxresults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataTypequery.Setmetrics(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataTypequery.SetsamplingLevel(AIndex : Integer; AValue : String); 

begin
  If (FsamplingLevel=AValue) then exit;
  FsamplingLevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataTypequery.Setsegment(AIndex : Integer; AValue : String); 

begin
  If (Fsegment=AValue) then exit;
  Fsegment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataTypequery.Setsort(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fsort=AValue) then exit;
  Fsort:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataTypequery.Setstartdate(AIndex : Integer; AValue : String); 

begin
  If (Fstartdate=AValue) then exit;
  Fstartdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataTypequery.Setstartindex(AIndex : Integer; AValue : integer); 

begin
  If (Fstartindex=AValue) then exit;
  Fstartindex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TMcfDataTypequery.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  'enddate' : Result:='end-date';
  'maxresults' : Result:='max-results';
  'startdate' : Result:='start-date';
  'startindex' : Result:='start-index';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TMcfDataTypequery.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'metrics' : SetLength(Fmetrics,ALength);
  'sort' : SetLength(Fsort,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMcfDataTyperowsItemItemTypeconversionPathValueItem
  --------------------------------------------------------------------}


Procedure TMcfDataTyperowsItemItemTypeconversionPathValueItem.SetinteractionType(AIndex : Integer; AValue : String); 

begin
  If (FinteractionType=AValue) then exit;
  FinteractionType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataTyperowsItemItemTypeconversionPathValueItem.SetnodeValue(AIndex : Integer; AValue : String); 

begin
  If (FnodeValue=AValue) then exit;
  FnodeValue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMcfDataTyperowsItemItem
  --------------------------------------------------------------------}


Procedure TMcfDataTyperowsItemItem.SetconversionPathValue(AIndex : Integer; AValue : TMcfDataTyperowsItemItemTypeconversionPathValueArray); 

begin
  If (FconversionPathValue=AValue) then exit;
  FconversionPathValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataTyperowsItemItem.SetprimitiveValue(AIndex : Integer; AValue : String); 

begin
  If (FprimitiveValue=AValue) then exit;
  FprimitiveValue:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TMcfDataTyperowsItemItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'conversionpathvalue' : SetLength(FconversionPathValue,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMcfDataTypetotalsForAllResults
  --------------------------------------------------------------------}


Class Function TMcfDataTypetotalsForAllResults.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TMcfData
  --------------------------------------------------------------------}


Procedure TMcfData.SetcolumnHeaders(AIndex : Integer; AValue : TMcfDataTypecolumnHeadersArray); 

begin
  If (FcolumnHeaders=AValue) then exit;
  FcolumnHeaders:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfData.SetcontainsSampledData(AIndex : Integer; AValue : boolean); 

begin
  If (FcontainsSampledData=AValue) then exit;
  FcontainsSampledData:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfData.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfData.SetitemsPerPage(AIndex : Integer; AValue : integer); 

begin
  If (FitemsPerPage=AValue) then exit;
  FitemsPerPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfData.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfData.SetnextLink(AIndex : Integer; AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfData.SetpreviousLink(AIndex : Integer; AValue : String); 

begin
  If (FpreviousLink=AValue) then exit;
  FpreviousLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfData.SetprofileInfo(AIndex : Integer; AValue : TMcfDataTypeprofileInfo); 

begin
  If (FprofileInfo=AValue) then exit;
  FprofileInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfData.Setquery(AIndex : Integer; AValue : TMcfDataTypequery); 

begin
  If (Fquery=AValue) then exit;
  Fquery:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfData.Setrows(AIndex : Integer; AValue : TMcfDataTyperowsArray); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfData.SetsampleSize(AIndex : Integer; AValue : String); 

begin
  If (FsampleSize=AValue) then exit;
  FsampleSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfData.SetsampleSpace(AIndex : Integer; AValue : String); 

begin
  If (FsampleSpace=AValue) then exit;
  FsampleSpace:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfData.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfData.SettotalResults(AIndex : Integer; AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfData.SettotalsForAllResults(AIndex : Integer; AValue : TMcfDataTypetotalsForAllResults); 

begin
  If (FtotalsForAllResults=AValue) then exit;
  FtotalsForAllResults:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TMcfData.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'columnheaders' : SetLength(FcolumnHeaders,ALength);
  'rows' : SetLength(Frows,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TProfileTypechildLink
  --------------------------------------------------------------------}


Procedure TProfileTypechildLink.Sethref(AIndex : Integer; AValue : String); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileTypechildLink.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TProfileTypechildLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TProfileTypeparentLink
  --------------------------------------------------------------------}


Procedure TProfileTypeparentLink.Sethref(AIndex : Integer; AValue : String); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileTypeparentLink.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TProfileTypeparentLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TProfileTypepermissions
  --------------------------------------------------------------------}


Procedure TProfileTypepermissions.Seteffective(AIndex : Integer; AValue : TStringArray); 

begin
  If (Feffective=AValue) then exit;
  Feffective:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TProfileTypepermissions.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'effective' : SetLength(Feffective,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TProfile
  --------------------------------------------------------------------}


Procedure TProfile.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetchildLink(AIndex : Integer; AValue : TProfileTypechildLink); 

begin
  If (FchildLink=AValue) then exit;
  FchildLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.Setcreated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.Setcurrency(AIndex : Integer; AValue : String); 

begin
  If (Fcurrency=AValue) then exit;
  Fcurrency:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetdefaultPage(AIndex : Integer; AValue : String); 

begin
  If (FdefaultPage=AValue) then exit;
  FdefaultPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SeteCommerceTracking(AIndex : Integer; AValue : boolean); 

begin
  If (FeCommerceTracking=AValue) then exit;
  FeCommerceTracking:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetenhancedECommerceTracking(AIndex : Integer; AValue : boolean); 

begin
  If (FenhancedECommerceTracking=AValue) then exit;
  FenhancedECommerceTracking:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetexcludeQueryParameters(AIndex : Integer; AValue : String); 

begin
  If (FexcludeQueryParameters=AValue) then exit;
  FexcludeQueryParameters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetinternalWebPropertyId(AIndex : Integer; AValue : String); 

begin
  If (FinternalWebPropertyId=AValue) then exit;
  FinternalWebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetparentLink(AIndex : Integer; AValue : TProfileTypeparentLink); 

begin
  If (FparentLink=AValue) then exit;
  FparentLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.Setpermissions(AIndex : Integer; AValue : TProfileTypepermissions); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetsiteSearchCategoryParameters(AIndex : Integer; AValue : String); 

begin
  If (FsiteSearchCategoryParameters=AValue) then exit;
  FsiteSearchCategoryParameters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetsiteSearchQueryParameters(AIndex : Integer; AValue : String); 

begin
  If (FsiteSearchQueryParameters=AValue) then exit;
  FsiteSearchQueryParameters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetstripSiteSearchCategoryParameters(AIndex : Integer; AValue : boolean); 

begin
  If (FstripSiteSearchCategoryParameters=AValue) then exit;
  FstripSiteSearchCategoryParameters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetstripSiteSearchQueryParameters(AIndex : Integer; AValue : boolean); 

begin
  If (FstripSiteSearchQueryParameters=AValue) then exit;
  FstripSiteSearchQueryParameters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.Settimezone(AIndex : Integer; AValue : String); 

begin
  If (Ftimezone=AValue) then exit;
  Ftimezone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetwebPropertyId(AIndex : Integer; AValue : String); 

begin
  If (FwebPropertyId=AValue) then exit;
  FwebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetwebsiteUrl(AIndex : Integer; AValue : String); 

begin
  If (FwebsiteUrl=AValue) then exit;
  FwebsiteUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TProfile.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TProfileFilterLink
  --------------------------------------------------------------------}


Procedure TProfileFilterLink.SetfilterRef(AIndex : Integer; AValue : TFilterRef); 

begin
  If (FfilterRef=AValue) then exit;
  FfilterRef:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileFilterLink.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileFilterLink.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileFilterLink.SetprofileRef(AIndex : Integer; AValue : TProfileRef); 

begin
  If (FprofileRef=AValue) then exit;
  FprofileRef:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileFilterLink.Setrank(AIndex : Integer; AValue : integer); 

begin
  If (Frank=AValue) then exit;
  Frank:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileFilterLink.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProfileFilterLinks
  --------------------------------------------------------------------}


Procedure TProfileFilterLinks.Setitems(AIndex : Integer; AValue : TProfileFilterLinksTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileFilterLinks.SetitemsPerPage(AIndex : Integer; AValue : integer); 

begin
  If (FitemsPerPage=AValue) then exit;
  FitemsPerPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileFilterLinks.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileFilterLinks.SetnextLink(AIndex : Integer; AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileFilterLinks.SetpreviousLink(AIndex : Integer; AValue : String); 

begin
  If (FpreviousLink=AValue) then exit;
  FpreviousLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileFilterLinks.SetstartIndex(AIndex : Integer; AValue : integer); 

begin
  If (FstartIndex=AValue) then exit;
  FstartIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileFilterLinks.SettotalResults(AIndex : Integer; AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileFilterLinks.Setusername(AIndex : Integer; AValue : String); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TProfileFilterLinks.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TProfileRef
  --------------------------------------------------------------------}


Procedure TProfileRef.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileRef.Sethref(AIndex : Integer; AValue : String); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileRef.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileRef.SetinternalWebPropertyId(AIndex : Integer; AValue : String); 

begin
  If (FinternalWebPropertyId=AValue) then exit;
  FinternalWebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileRef.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileRef.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileRef.SetwebPropertyId(AIndex : Integer; AValue : String); 

begin
  If (FwebPropertyId=AValue) then exit;
  FwebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProfileSummary
  --------------------------------------------------------------------}


Procedure TProfileSummary.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileSummary.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileSummary.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileSummary.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TProfileSummary.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TProfiles
  --------------------------------------------------------------------}


Procedure TProfiles.Setitems(AIndex : Integer; AValue : TProfilesTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfiles.SetitemsPerPage(AIndex : Integer; AValue : integer); 

begin
  If (FitemsPerPage=AValue) then exit;
  FitemsPerPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfiles.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfiles.SetnextLink(AIndex : Integer; AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfiles.SetpreviousLink(AIndex : Integer; AValue : String); 

begin
  If (FpreviousLink=AValue) then exit;
  FpreviousLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfiles.SetstartIndex(AIndex : Integer; AValue : integer); 

begin
  If (FstartIndex=AValue) then exit;
  FstartIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfiles.SettotalResults(AIndex : Integer; AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfiles.Setusername(AIndex : Integer; AValue : String); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TProfiles.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TRealtimeDataTypecolumnHeadersItem
  --------------------------------------------------------------------}


Procedure TRealtimeDataTypecolumnHeadersItem.SetcolumnType(AIndex : Integer; AValue : String); 

begin
  If (FcolumnType=AValue) then exit;
  FcolumnType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDataTypecolumnHeadersItem.SetdataType(AIndex : Integer; AValue : String); 

begin
  If (FdataType=AValue) then exit;
  FdataType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDataTypecolumnHeadersItem.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRealtimeDataTypeprofileInfo
  --------------------------------------------------------------------}


Procedure TRealtimeDataTypeprofileInfo.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDataTypeprofileInfo.SetinternalWebPropertyId(AIndex : Integer; AValue : String); 

begin
  If (FinternalWebPropertyId=AValue) then exit;
  FinternalWebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDataTypeprofileInfo.SetprofileId(AIndex : Integer; AValue : String); 

begin
  If (FprofileId=AValue) then exit;
  FprofileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDataTypeprofileInfo.SetprofileName(AIndex : Integer; AValue : String); 

begin
  If (FprofileName=AValue) then exit;
  FprofileName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDataTypeprofileInfo.SettableId(AIndex : Integer; AValue : String); 

begin
  If (FtableId=AValue) then exit;
  FtableId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDataTypeprofileInfo.SetwebPropertyId(AIndex : Integer; AValue : String); 

begin
  If (FwebPropertyId=AValue) then exit;
  FwebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRealtimeDataTypequery
  --------------------------------------------------------------------}


Procedure TRealtimeDataTypequery.Setdimensions(AIndex : Integer; AValue : String); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDataTypequery.Setfilters(AIndex : Integer; AValue : String); 

begin
  If (Ffilters=AValue) then exit;
  Ffilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDataTypequery.Setids(AIndex : Integer; AValue : String); 

begin
  If (Fids=AValue) then exit;
  Fids:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDataTypequery.Setmaxresults(AIndex : Integer; AValue : integer); 

begin
  If (Fmaxresults=AValue) then exit;
  Fmaxresults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDataTypequery.Setmetrics(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDataTypequery.Setsort(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fsort=AValue) then exit;
  Fsort:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TRealtimeDataTypequery.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  'maxresults' : Result:='max-results';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TRealtimeDataTypequery.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'metrics' : SetLength(Fmetrics,ALength);
  'sort' : SetLength(Fsort,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TRealtimeDataTypetotalsForAllResults
  --------------------------------------------------------------------}


Class Function TRealtimeDataTypetotalsForAllResults.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TRealtimeData
  --------------------------------------------------------------------}


Procedure TRealtimeData.SetcolumnHeaders(AIndex : Integer; AValue : TRealtimeDataTypecolumnHeadersArray); 

begin
  If (FcolumnHeaders=AValue) then exit;
  FcolumnHeaders:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeData.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeData.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeData.SetprofileInfo(AIndex : Integer; AValue : TRealtimeDataTypeprofileInfo); 

begin
  If (FprofileInfo=AValue) then exit;
  FprofileInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeData.Setquery(AIndex : Integer; AValue : TRealtimeDataTypequery); 

begin
  If (Fquery=AValue) then exit;
  Fquery:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeData.Setrows(AIndex : Integer; AValue : TRealtimeDataTyperowsArray); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeData.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeData.SettotalResults(AIndex : Integer; AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeData.SettotalsForAllResults(AIndex : Integer; AValue : TRealtimeDataTypetotalsForAllResults); 

begin
  If (FtotalsForAllResults=AValue) then exit;
  FtotalsForAllResults:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TRealtimeData.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'columnheaders' : SetLength(FcolumnHeaders,ALength);
  'rows' : SetLength(Frows,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSegment
  --------------------------------------------------------------------}


Procedure TSegment.Setcreated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegment.Setdefinition(AIndex : Integer; AValue : String); 

begin
  If (Fdefinition=AValue) then exit;
  Fdefinition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegment.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegment.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegment.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegment.SetsegmentId(AIndex : Integer; AValue : String); 

begin
  If (FsegmentId=AValue) then exit;
  FsegmentId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegment.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegment.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegment.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TSegment.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TSegments
  --------------------------------------------------------------------}


Procedure TSegments.Setitems(AIndex : Integer; AValue : TSegmentsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegments.SetitemsPerPage(AIndex : Integer; AValue : integer); 

begin
  If (FitemsPerPage=AValue) then exit;
  FitemsPerPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegments.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegments.SetnextLink(AIndex : Integer; AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegments.SetpreviousLink(AIndex : Integer; AValue : String); 

begin
  If (FpreviousLink=AValue) then exit;
  FpreviousLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegments.SetstartIndex(AIndex : Integer; AValue : integer); 

begin
  If (FstartIndex=AValue) then exit;
  FstartIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegments.SettotalResults(AIndex : Integer; AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegments.Setusername(AIndex : Integer; AValue : String); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSegments.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUnsampledReportTypecloudStorageDownloadDetails
  --------------------------------------------------------------------}


Procedure TUnsampledReportTypecloudStorageDownloadDetails.SetbucketId(AIndex : Integer; AValue : String); 

begin
  If (FbucketId=AValue) then exit;
  FbucketId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReportTypecloudStorageDownloadDetails.SetobjectId(AIndex : Integer; AValue : String); 

begin
  If (FobjectId=AValue) then exit;
  FobjectId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUnsampledReportTypedriveDownloadDetails
  --------------------------------------------------------------------}


Procedure TUnsampledReportTypedriveDownloadDetails.SetdocumentId(AIndex : Integer; AValue : String); 

begin
  If (FdocumentId=AValue) then exit;
  FdocumentId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUnsampledReport
  --------------------------------------------------------------------}


Procedure TUnsampledReport.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.SetcloudStorageDownloadDetails(AIndex : Integer; AValue : TUnsampledReportTypecloudStorageDownloadDetails); 

begin
  If (FcloudStorageDownloadDetails=AValue) then exit;
  FcloudStorageDownloadDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.Setcreated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.Setdimensions(AIndex : Integer; AValue : String); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.SetdownloadType(AIndex : Integer; AValue : String); 

begin
  If (FdownloadType=AValue) then exit;
  FdownloadType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.SetdriveDownloadDetails(AIndex : Integer; AValue : TUnsampledReportTypedriveDownloadDetails); 

begin
  If (FdriveDownloadDetails=AValue) then exit;
  FdriveDownloadDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.Setenddate(AIndex : Integer; AValue : String); 

begin
  If (Fenddate=AValue) then exit;
  Fenddate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.Setfilters(AIndex : Integer; AValue : String); 

begin
  If (Ffilters=AValue) then exit;
  Ffilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.Setmetrics(AIndex : Integer; AValue : String); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.SetprofileId(AIndex : Integer; AValue : String); 

begin
  If (FprofileId=AValue) then exit;
  FprofileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.Setsegment(AIndex : Integer; AValue : String); 

begin
  If (Fsegment=AValue) then exit;
  Fsegment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.Setstartdate(AIndex : Integer; AValue : String); 

begin
  If (Fstartdate=AValue) then exit;
  Fstartdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.SetwebPropertyId(AIndex : Integer; AValue : String); 

begin
  If (FwebPropertyId=AValue) then exit;
  FwebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TUnsampledReport.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  'enddate' : Result:='end-date';
  'startdate' : Result:='start-date';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TUnsampledReports
  --------------------------------------------------------------------}


Procedure TUnsampledReports.Setitems(AIndex : Integer; AValue : TUnsampledReportsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReports.SetitemsPerPage(AIndex : Integer; AValue : integer); 

begin
  If (FitemsPerPage=AValue) then exit;
  FitemsPerPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReports.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReports.SetnextLink(AIndex : Integer; AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReports.SetpreviousLink(AIndex : Integer; AValue : String); 

begin
  If (FpreviousLink=AValue) then exit;
  FpreviousLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReports.SetstartIndex(AIndex : Integer; AValue : integer); 

begin
  If (FstartIndex=AValue) then exit;
  FstartIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReports.SettotalResults(AIndex : Integer; AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReports.Setusername(AIndex : Integer; AValue : String); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TUnsampledReports.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUpload
  --------------------------------------------------------------------}


Procedure TUpload.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpload.SetcustomDataSourceId(AIndex : Integer; AValue : String); 

begin
  If (FcustomDataSourceId=AValue) then exit;
  FcustomDataSourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpload.Seterrors(AIndex : Integer; AValue : TStringArray); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpload.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpload.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpload.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TUpload.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errors' : SetLength(Ferrors,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUploads
  --------------------------------------------------------------------}


Procedure TUploads.Setitems(AIndex : Integer; AValue : TUploadsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUploads.SetitemsPerPage(AIndex : Integer; AValue : integer); 

begin
  If (FitemsPerPage=AValue) then exit;
  FitemsPerPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUploads.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUploads.SetnextLink(AIndex : Integer; AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUploads.SetpreviousLink(AIndex : Integer; AValue : String); 

begin
  If (FpreviousLink=AValue) then exit;
  FpreviousLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUploads.SetstartIndex(AIndex : Integer; AValue : integer); 

begin
  If (FstartIndex=AValue) then exit;
  FstartIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUploads.SettotalResults(AIndex : Integer; AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TUploads.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUserRef
  --------------------------------------------------------------------}


Procedure TUserRef.Setemail(AIndex : Integer; AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserRef.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserRef.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWebPropertyRef
  --------------------------------------------------------------------}


Procedure TWebPropertyRef.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebPropertyRef.Sethref(AIndex : Integer; AValue : String); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebPropertyRef.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebPropertyRef.SetinternalWebPropertyId(AIndex : Integer; AValue : String); 

begin
  If (FinternalWebPropertyId=AValue) then exit;
  FinternalWebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebPropertyRef.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebPropertyRef.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWebPropertySummary
  --------------------------------------------------------------------}


Procedure TWebPropertySummary.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebPropertySummary.SetinternalWebPropertyId(AIndex : Integer; AValue : String); 

begin
  If (FinternalWebPropertyId=AValue) then exit;
  FinternalWebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebPropertySummary.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebPropertySummary.Setlevel(AIndex : Integer; AValue : String); 

begin
  If (Flevel=AValue) then exit;
  Flevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebPropertySummary.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebPropertySummary.Setprofiles(AIndex : Integer; AValue : TWebPropertySummaryTypeprofilesArray); 

begin
  If (Fprofiles=AValue) then exit;
  Fprofiles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebPropertySummary.SetwebsiteUrl(AIndex : Integer; AValue : String); 

begin
  If (FwebsiteUrl=AValue) then exit;
  FwebsiteUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TWebPropertySummary.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'profiles' : SetLength(Fprofiles,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TWebproperties
  --------------------------------------------------------------------}


Procedure TWebproperties.Setitems(AIndex : Integer; AValue : TWebpropertiesTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperties.SetitemsPerPage(AIndex : Integer; AValue : integer); 

begin
  If (FitemsPerPage=AValue) then exit;
  FitemsPerPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperties.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperties.SetnextLink(AIndex : Integer; AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperties.SetpreviousLink(AIndex : Integer; AValue : String); 

begin
  If (FpreviousLink=AValue) then exit;
  FpreviousLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperties.SetstartIndex(AIndex : Integer; AValue : integer); 

begin
  If (FstartIndex=AValue) then exit;
  FstartIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperties.SettotalResults(AIndex : Integer; AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperties.Setusername(AIndex : Integer; AValue : String); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TWebproperties.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TWebpropertyTypechildLink
  --------------------------------------------------------------------}


Procedure TWebpropertyTypechildLink.Sethref(AIndex : Integer; AValue : String); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebpropertyTypechildLink.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TWebpropertyTypechildLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TWebpropertyTypeparentLink
  --------------------------------------------------------------------}


Procedure TWebpropertyTypeparentLink.Sethref(AIndex : Integer; AValue : String); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebpropertyTypeparentLink.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TWebpropertyTypeparentLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TWebpropertyTypepermissions
  --------------------------------------------------------------------}


Procedure TWebpropertyTypepermissions.Seteffective(AIndex : Integer; AValue : TStringArray); 

begin
  If (Feffective=AValue) then exit;
  Feffective:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TWebpropertyTypepermissions.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'effective' : SetLength(Feffective,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TWebproperty
  --------------------------------------------------------------------}


Procedure TWebproperty.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.SetchildLink(AIndex : Integer; AValue : TWebpropertyTypechildLink); 

begin
  If (FchildLink=AValue) then exit;
  FchildLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.Setcreated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.SetdefaultProfileId(AIndex : Integer; AValue : String); 

begin
  If (FdefaultProfileId=AValue) then exit;
  FdefaultProfileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.SetindustryVertical(AIndex : Integer; AValue : String); 

begin
  If (FindustryVertical=AValue) then exit;
  FindustryVertical:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.SetinternalWebPropertyId(AIndex : Integer; AValue : String); 

begin
  If (FinternalWebPropertyId=AValue) then exit;
  FinternalWebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.Setlevel(AIndex : Integer; AValue : String); 

begin
  If (Flevel=AValue) then exit;
  Flevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.SetparentLink(AIndex : Integer; AValue : TWebpropertyTypeparentLink); 

begin
  If (FparentLink=AValue) then exit;
  FparentLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.Setpermissions(AIndex : Integer; AValue : TWebpropertyTypepermissions); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.SetprofileCount(AIndex : Integer; AValue : integer); 

begin
  If (FprofileCount=AValue) then exit;
  FprofileCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.SetwebsiteUrl(AIndex : Integer; AValue : String); 

begin
  If (FwebsiteUrl=AValue) then exit;
  FwebsiteUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDataGaResource
  --------------------------------------------------------------------}


Class Function TDataGaResource.ResourceName : String;

begin
  Result:='ga';
end;

Class Function TDataGaResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Function TDataGaResource.Get(AQuery : string = '') : TGaData;

Const
  _HTTPMethod = 'GET';
  _Path       = 'data/ga';
  _Methodid   = 'analytics.data.ga.get';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TGaData) as TGaData;
end;


Function TDataGaResource.Get(AQuery : TDataGagetOptions) : TGaData;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'dimensions',AQuery.dimensions);
  AddToQuery(_Q,'end-date',AQuery.enddate);
  AddToQuery(_Q,'filters',AQuery.filters);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'max-results',AQuery.maxresults);
  AddToQuery(_Q,'metrics',AQuery.metrics);
  AddToQuery(_Q,'output',AQuery.output);
  AddToQuery(_Q,'samplingLevel',AQuery.samplingLevel);
  AddToQuery(_Q,'segment',AQuery.segment);
  AddToQuery(_Q,'sort',AQuery.sort);
  AddToQuery(_Q,'start-date',AQuery.startdate);
  AddToQuery(_Q,'start-index',AQuery.startindex);
  Result:=Get(_Q);
end;



{ --------------------------------------------------------------------
  TDataMcfResource
  --------------------------------------------------------------------}


Class Function TDataMcfResource.ResourceName : String;

begin
  Result:='mcf';
end;

Class Function TDataMcfResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Function TDataMcfResource.Get(AQuery : string = '') : TMcfData;

Const
  _HTTPMethod = 'GET';
  _Path       = 'data/mcf';
  _Methodid   = 'analytics.data.mcf.get';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TMcfData) as TMcfData;
end;


Function TDataMcfResource.Get(AQuery : TDataMcfgetOptions) : TMcfData;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'dimensions',AQuery.dimensions);
  AddToQuery(_Q,'end-date',AQuery.enddate);
  AddToQuery(_Q,'filters',AQuery.filters);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'max-results',AQuery.maxresults);
  AddToQuery(_Q,'metrics',AQuery.metrics);
  AddToQuery(_Q,'samplingLevel',AQuery.samplingLevel);
  AddToQuery(_Q,'sort',AQuery.sort);
  AddToQuery(_Q,'start-date',AQuery.startdate);
  AddToQuery(_Q,'start-index',AQuery.startindex);
  Result:=Get(_Q);
end;



{ --------------------------------------------------------------------
  TDataRealtimeResource
  --------------------------------------------------------------------}


Class Function TDataRealtimeResource.ResourceName : String;

begin
  Result:='realtime';
end;

Class Function TDataRealtimeResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Function TDataRealtimeResource.Get(AQuery : string = '') : TRealtimeData;

Const
  _HTTPMethod = 'GET';
  _Path       = 'data/realtime';
  _Methodid   = 'analytics.data.realtime.get';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TRealtimeData) as TRealtimeData;
end;


Function TDataRealtimeResource.Get(AQuery : TDataRealtimegetOptions) : TRealtimeData;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'dimensions',AQuery.dimensions);
  AddToQuery(_Q,'filters',AQuery.filters);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'max-results',AQuery.maxresults);
  AddToQuery(_Q,'metrics',AQuery.metrics);
  AddToQuery(_Q,'sort',AQuery.sort);
  Result:=Get(_Q);
end;



{ --------------------------------------------------------------------
  TDataResource
  --------------------------------------------------------------------}


Class Function TDataResource.ResourceName : String;

begin
  Result:='data';
end;

Class Function TDataResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;



Function TDataResource.GetGaInstance : TDataGaResource;

begin
  if (FGaInstance=Nil) then
    FGaInstance:=CreateGaResource;
  Result:=FGaInstance;
end;

Function TDataResource.CreateGaResource : TDataGaResource;

begin
  Result:=CreateGaResource(Self);
end;


Function TDataResource.CreateGaResource(AOwner : TComponent) : TDataGaResource;

begin
  Result:=TDataGaResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TDataResource.GetMcfInstance : TDataMcfResource;

begin
  if (FMcfInstance=Nil) then
    FMcfInstance:=CreateMcfResource;
  Result:=FMcfInstance;
end;

Function TDataResource.CreateMcfResource : TDataMcfResource;

begin
  Result:=CreateMcfResource(Self);
end;


Function TDataResource.CreateMcfResource(AOwner : TComponent) : TDataMcfResource;

begin
  Result:=TDataMcfResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TDataResource.GetRealtimeInstance : TDataRealtimeResource;

begin
  if (FRealtimeInstance=Nil) then
    FRealtimeInstance:=CreateRealtimeResource;
  Result:=FRealtimeInstance;
end;

Function TDataResource.CreateRealtimeResource : TDataRealtimeResource;

begin
  Result:=CreateRealtimeResource(Self);
end;


Function TDataResource.CreateRealtimeResource(AOwner : TComponent) : TDataRealtimeResource;

begin
  Result:=TDataRealtimeResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TManagementAccountSummariesResource
  --------------------------------------------------------------------}


Class Function TManagementAccountSummariesResource.ResourceName : String;

begin
  Result:='accountSummaries';
end;

Class Function TManagementAccountSummariesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Function TManagementAccountSummariesResource.List(AQuery : string = '') : TAccountSummaries;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accountSummaries';
  _Methodid   = 'analytics.management.accountSummaries.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TAccountSummaries) as TAccountSummaries;
end;


Function TManagementAccountSummariesResource.List(AQuery : TManagementAccountSummarieslistOptions) : TAccountSummaries;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'max-results',AQuery.maxresults);
  AddToQuery(_Q,'start-index',AQuery.startindex);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TManagementAccountUserLinksResource
  --------------------------------------------------------------------}


Class Function TManagementAccountUserLinksResource.ResourceName : String;

begin
  Result:='accountUserLinks';
end;

Class Function TManagementAccountUserLinksResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Procedure TManagementAccountUserLinksResource.Delete(accountId: string; linkId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'management/accounts/{accountId}/entityUserLinks/{linkId}';
  _Methodid   = 'analytics.management.accountUserLinks.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'linkId',linkId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TManagementAccountUserLinksResource.Insert(accountId: string; aEntityUserLink : TEntityUserLink) : TEntityUserLink;

Const
  _HTTPMethod = 'POST';
  _Path       = 'management/accounts/{accountId}/entityUserLinks';
  _Methodid   = 'analytics.management.accountUserLinks.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aEntityUserLink,TEntityUserLink) as TEntityUserLink;
end;

Function TManagementAccountUserLinksResource.List(accountId: string; AQuery : string = '') : TEntityUserLinks;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/entityUserLinks';
  _Methodid   = 'analytics.management.accountUserLinks.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TEntityUserLinks) as TEntityUserLinks;
end;


Function TManagementAccountUserLinksResource.List(accountId: string; AQuery : TManagementAccountUserLinkslistOptions) : TEntityUserLinks;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'max-results',AQuery.maxresults);
  AddToQuery(_Q,'start-index',AQuery.startindex);
  Result:=List(accountId,_Q);
end;

Function TManagementAccountUserLinksResource.Update(accountId: string; linkId: string; aEntityUserLink : TEntityUserLink) : TEntityUserLink;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'management/accounts/{accountId}/entityUserLinks/{linkId}';
  _Methodid   = 'analytics.management.accountUserLinks.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'linkId',linkId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aEntityUserLink,TEntityUserLink) as TEntityUserLink;
end;



{ --------------------------------------------------------------------
  TManagementAccountsResource
  --------------------------------------------------------------------}


Class Function TManagementAccountsResource.ResourceName : String;

begin
  Result:='accounts';
end;

Class Function TManagementAccountsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Function TManagementAccountsResource.List(AQuery : string = '') : TAccounts;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts';
  _Methodid   = 'analytics.management.accounts.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TAccounts) as TAccounts;
end;


Function TManagementAccountsResource.List(AQuery : TManagementAccountslistOptions) : TAccounts;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'max-results',AQuery.maxresults);
  AddToQuery(_Q,'start-index',AQuery.startindex);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TManagementCustomDataSourcesResource
  --------------------------------------------------------------------}


Class Function TManagementCustomDataSourcesResource.ResourceName : String;

begin
  Result:='customDataSources';
end;

Class Function TManagementCustomDataSourcesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Function TManagementCustomDataSourcesResource.List(accountId: string; webPropertyId: string; AQuery : string = '') : TCustomDataSources;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/customDataSources';
  _Methodid   = 'analytics.management.customDataSources.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCustomDataSources) as TCustomDataSources;
end;


Function TManagementCustomDataSourcesResource.List(accountId: string; webPropertyId: string; AQuery : TManagementCustomDataSourceslistOptions) : TCustomDataSources;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'max-results',AQuery.maxresults);
  AddToQuery(_Q,'start-index',AQuery.startindex);
  Result:=List(accountId,webPropertyId,_Q);
end;



{ --------------------------------------------------------------------
  TManagementCustomDimensionsResource
  --------------------------------------------------------------------}


Class Function TManagementCustomDimensionsResource.ResourceName : String;

begin
  Result:='customDimensions';
end;

Class Function TManagementCustomDimensionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Function TManagementCustomDimensionsResource.Get(accountId: string; customDimensionId: string; webPropertyId: string) : TCustomDimension;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/customDimensions/{customDimensionId}';
  _Methodid   = 'analytics.management.customDimensions.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'customDimensionId',customDimensionId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCustomDimension) as TCustomDimension;
end;

Function TManagementCustomDimensionsResource.Insert(accountId: string; webPropertyId: string; aCustomDimension : TCustomDimension) : TCustomDimension;

Const
  _HTTPMethod = 'POST';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/customDimensions';
  _Methodid   = 'analytics.management.customDimensions.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCustomDimension,TCustomDimension) as TCustomDimension;
end;

Function TManagementCustomDimensionsResource.List(accountId: string; webPropertyId: string; AQuery : string = '') : TCustomDimensions;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/customDimensions';
  _Methodid   = 'analytics.management.customDimensions.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCustomDimensions) as TCustomDimensions;
end;


Function TManagementCustomDimensionsResource.List(accountId: string; webPropertyId: string; AQuery : TManagementCustomDimensionslistOptions) : TCustomDimensions;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'max-results',AQuery.maxresults);
  AddToQuery(_Q,'start-index',AQuery.startindex);
  Result:=List(accountId,webPropertyId,_Q);
end;

Function TManagementCustomDimensionsResource.Patch(accountId: string; customDimensionId: string; webPropertyId: string; aCustomDimension : TCustomDimension; AQuery : string = '') : TCustomDimension;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/customDimensions/{customDimensionId}';
  _Methodid   = 'analytics.management.customDimensions.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'customDimensionId',customDimensionId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aCustomDimension,TCustomDimension) as TCustomDimension;
end;


Function TManagementCustomDimensionsResource.Patch(accountId: string; customDimensionId: string; webPropertyId: string; aCustomDimension : TCustomDimension; AQuery : TManagementCustomDimensionspatchOptions) : TCustomDimension;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ignoreCustomDataSourceLinks',AQuery.ignoreCustomDataSourceLinks);
  Result:=Patch(accountId,customDimensionId,webPropertyId,aCustomDimension,_Q);
end;

Function TManagementCustomDimensionsResource.Update(accountId: string; customDimensionId: string; webPropertyId: string; aCustomDimension : TCustomDimension; AQuery : string = '') : TCustomDimension;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/customDimensions/{customDimensionId}';
  _Methodid   = 'analytics.management.customDimensions.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'customDimensionId',customDimensionId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aCustomDimension,TCustomDimension) as TCustomDimension;
end;


Function TManagementCustomDimensionsResource.Update(accountId: string; customDimensionId: string; webPropertyId: string; aCustomDimension : TCustomDimension; AQuery : TManagementCustomDimensionsupdateOptions) : TCustomDimension;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ignoreCustomDataSourceLinks',AQuery.ignoreCustomDataSourceLinks);
  Result:=Update(accountId,customDimensionId,webPropertyId,aCustomDimension,_Q);
end;



{ --------------------------------------------------------------------
  TManagementCustomMetricsResource
  --------------------------------------------------------------------}


Class Function TManagementCustomMetricsResource.ResourceName : String;

begin
  Result:='customMetrics';
end;

Class Function TManagementCustomMetricsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Function TManagementCustomMetricsResource.Get(accountId: string; customMetricId: string; webPropertyId: string) : TCustomMetric;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/customMetrics/{customMetricId}';
  _Methodid   = 'analytics.management.customMetrics.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'customMetricId',customMetricId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCustomMetric) as TCustomMetric;
end;

Function TManagementCustomMetricsResource.Insert(accountId: string; webPropertyId: string; aCustomMetric : TCustomMetric) : TCustomMetric;

Const
  _HTTPMethod = 'POST';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/customMetrics';
  _Methodid   = 'analytics.management.customMetrics.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCustomMetric,TCustomMetric) as TCustomMetric;
end;

Function TManagementCustomMetricsResource.List(accountId: string; webPropertyId: string; AQuery : string = '') : TCustomMetrics;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/customMetrics';
  _Methodid   = 'analytics.management.customMetrics.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCustomMetrics) as TCustomMetrics;
end;


Function TManagementCustomMetricsResource.List(accountId: string; webPropertyId: string; AQuery : TManagementCustomMetricslistOptions) : TCustomMetrics;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'max-results',AQuery.maxresults);
  AddToQuery(_Q,'start-index',AQuery.startindex);
  Result:=List(accountId,webPropertyId,_Q);
end;

Function TManagementCustomMetricsResource.Patch(accountId: string; customMetricId: string; webPropertyId: string; aCustomMetric : TCustomMetric; AQuery : string = '') : TCustomMetric;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/customMetrics/{customMetricId}';
  _Methodid   = 'analytics.management.customMetrics.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'customMetricId',customMetricId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aCustomMetric,TCustomMetric) as TCustomMetric;
end;


Function TManagementCustomMetricsResource.Patch(accountId: string; customMetricId: string; webPropertyId: string; aCustomMetric : TCustomMetric; AQuery : TManagementCustomMetricspatchOptions) : TCustomMetric;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ignoreCustomDataSourceLinks',AQuery.ignoreCustomDataSourceLinks);
  Result:=Patch(accountId,customMetricId,webPropertyId,aCustomMetric,_Q);
end;

Function TManagementCustomMetricsResource.Update(accountId: string; customMetricId: string; webPropertyId: string; aCustomMetric : TCustomMetric; AQuery : string = '') : TCustomMetric;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/customMetrics/{customMetricId}';
  _Methodid   = 'analytics.management.customMetrics.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'customMetricId',customMetricId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aCustomMetric,TCustomMetric) as TCustomMetric;
end;


Function TManagementCustomMetricsResource.Update(accountId: string; customMetricId: string; webPropertyId: string; aCustomMetric : TCustomMetric; AQuery : TManagementCustomMetricsupdateOptions) : TCustomMetric;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ignoreCustomDataSourceLinks',AQuery.ignoreCustomDataSourceLinks);
  Result:=Update(accountId,customMetricId,webPropertyId,aCustomMetric,_Q);
end;



{ --------------------------------------------------------------------
  TManagementExperimentsResource
  --------------------------------------------------------------------}


Class Function TManagementExperimentsResource.ResourceName : String;

begin
  Result:='experiments';
end;

Class Function TManagementExperimentsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Procedure TManagementExperimentsResource.Delete(accountId: string; experimentId: string; profileId: string; webPropertyId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/experiments/{experimentId}';
  _Methodid   = 'analytics.management.experiments.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'experimentId',experimentId,'profileId',profileId,'webPropertyId',webPropertyId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TManagementExperimentsResource.Get(accountId: string; experimentId: string; profileId: string; webPropertyId: string) : TExperiment;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/experiments/{experimentId}';
  _Methodid   = 'analytics.management.experiments.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'experimentId',experimentId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TExperiment) as TExperiment;
end;

Function TManagementExperimentsResource.Insert(accountId: string; profileId: string; webPropertyId: string; aExperiment : TExperiment) : TExperiment;

Const
  _HTTPMethod = 'POST';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/experiments';
  _Methodid   = 'analytics.management.experiments.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aExperiment,TExperiment) as TExperiment;
end;

Function TManagementExperimentsResource.List(accountId: string; profileId: string; webPropertyId: string; AQuery : string = '') : TExperiments;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/experiments';
  _Methodid   = 'analytics.management.experiments.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TExperiments) as TExperiments;
end;


Function TManagementExperimentsResource.List(accountId: string; profileId: string; webPropertyId: string; AQuery : TManagementExperimentslistOptions) : TExperiments;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'max-results',AQuery.maxresults);
  AddToQuery(_Q,'start-index',AQuery.startindex);
  Result:=List(accountId,profileId,webPropertyId,_Q);
end;

Function TManagementExperimentsResource.Patch(accountId: string; experimentId: string; profileId: string; webPropertyId: string; aExperiment : TExperiment) : TExperiment;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/experiments/{experimentId}';
  _Methodid   = 'analytics.management.experiments.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'experimentId',experimentId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aExperiment,TExperiment) as TExperiment;
end;

Function TManagementExperimentsResource.Update(accountId: string; experimentId: string; profileId: string; webPropertyId: string; aExperiment : TExperiment) : TExperiment;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/experiments/{experimentId}';
  _Methodid   = 'analytics.management.experiments.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'experimentId',experimentId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aExperiment,TExperiment) as TExperiment;
end;



{ --------------------------------------------------------------------
  TManagementFiltersResource
  --------------------------------------------------------------------}


Class Function TManagementFiltersResource.ResourceName : String;

begin
  Result:='filters';
end;

Class Function TManagementFiltersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Function TManagementFiltersResource.Delete(accountId: string; filterId: string) : TFilter;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'management/accounts/{accountId}/filters/{filterId}';
  _Methodid   = 'analytics.management.filters.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'filterId',filterId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TFilter) as TFilter;
end;

Function TManagementFiltersResource.Get(accountId: string; filterId: string) : TFilter;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/filters/{filterId}';
  _Methodid   = 'analytics.management.filters.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'filterId',filterId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TFilter) as TFilter;
end;

Function TManagementFiltersResource.Insert(accountId: string; aFilter : TFilter) : TFilter;

Const
  _HTTPMethod = 'POST';
  _Path       = 'management/accounts/{accountId}/filters';
  _Methodid   = 'analytics.management.filters.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aFilter,TFilter) as TFilter;
end;

Function TManagementFiltersResource.List(accountId: string; AQuery : string = '') : TFilters;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/filters';
  _Methodid   = 'analytics.management.filters.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TFilters) as TFilters;
end;


Function TManagementFiltersResource.List(accountId: string; AQuery : TManagementFilterslistOptions) : TFilters;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'max-results',AQuery.maxresults);
  AddToQuery(_Q,'start-index',AQuery.startindex);
  Result:=List(accountId,_Q);
end;

Function TManagementFiltersResource.Patch(accountId: string; filterId: string; aFilter : TFilter) : TFilter;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'management/accounts/{accountId}/filters/{filterId}';
  _Methodid   = 'analytics.management.filters.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'filterId',filterId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aFilter,TFilter) as TFilter;
end;

Function TManagementFiltersResource.Update(accountId: string; filterId: string; aFilter : TFilter) : TFilter;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'management/accounts/{accountId}/filters/{filterId}';
  _Methodid   = 'analytics.management.filters.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'filterId',filterId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aFilter,TFilter) as TFilter;
end;



{ --------------------------------------------------------------------
  TManagementGoalsResource
  --------------------------------------------------------------------}


Class Function TManagementGoalsResource.ResourceName : String;

begin
  Result:='goals';
end;

Class Function TManagementGoalsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Function TManagementGoalsResource.Get(accountId: string; goalId: string; profileId: string; webPropertyId: string) : TGoal;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/goals/{goalId}';
  _Methodid   = 'analytics.management.goals.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'goalId',goalId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TGoal) as TGoal;
end;

Function TManagementGoalsResource.Insert(accountId: string; profileId: string; webPropertyId: string; aGoal : TGoal) : TGoal;

Const
  _HTTPMethod = 'POST';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/goals';
  _Methodid   = 'analytics.management.goals.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aGoal,TGoal) as TGoal;
end;

Function TManagementGoalsResource.List(accountId: string; profileId: string; webPropertyId: string; AQuery : string = '') : TGoals;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/goals';
  _Methodid   = 'analytics.management.goals.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TGoals) as TGoals;
end;


Function TManagementGoalsResource.List(accountId: string; profileId: string; webPropertyId: string; AQuery : TManagementGoalslistOptions) : TGoals;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'max-results',AQuery.maxresults);
  AddToQuery(_Q,'start-index',AQuery.startindex);
  Result:=List(accountId,profileId,webPropertyId,_Q);
end;

Function TManagementGoalsResource.Patch(accountId: string; goalId: string; profileId: string; webPropertyId: string; aGoal : TGoal) : TGoal;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/goals/{goalId}';
  _Methodid   = 'analytics.management.goals.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'goalId',goalId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aGoal,TGoal) as TGoal;
end;

Function TManagementGoalsResource.Update(accountId: string; goalId: string; profileId: string; webPropertyId: string; aGoal : TGoal) : TGoal;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/goals/{goalId}';
  _Methodid   = 'analytics.management.goals.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'goalId',goalId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aGoal,TGoal) as TGoal;
end;



{ --------------------------------------------------------------------
  TManagementProfileFilterLinksResource
  --------------------------------------------------------------------}


Class Function TManagementProfileFilterLinksResource.ResourceName : String;

begin
  Result:='profileFilterLinks';
end;

Class Function TManagementProfileFilterLinksResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Procedure TManagementProfileFilterLinksResource.Delete(accountId: string; linkId: string; profileId: string; webPropertyId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/profileFilterLinks/{linkId}';
  _Methodid   = 'analytics.management.profileFilterLinks.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'linkId',linkId,'profileId',profileId,'webPropertyId',webPropertyId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TManagementProfileFilterLinksResource.Get(accountId: string; linkId: string; profileId: string; webPropertyId: string) : TProfileFilterLink;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/profileFilterLinks/{linkId}';
  _Methodid   = 'analytics.management.profileFilterLinks.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'linkId',linkId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TProfileFilterLink) as TProfileFilterLink;
end;

Function TManagementProfileFilterLinksResource.Insert(accountId: string; profileId: string; webPropertyId: string; aProfileFilterLink : TProfileFilterLink) : TProfileFilterLink;

Const
  _HTTPMethod = 'POST';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/profileFilterLinks';
  _Methodid   = 'analytics.management.profileFilterLinks.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aProfileFilterLink,TProfileFilterLink) as TProfileFilterLink;
end;

Function TManagementProfileFilterLinksResource.List(accountId: string; profileId: string; webPropertyId: string; AQuery : string = '') : TProfileFilterLinks;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/profileFilterLinks';
  _Methodid   = 'analytics.management.profileFilterLinks.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TProfileFilterLinks) as TProfileFilterLinks;
end;


Function TManagementProfileFilterLinksResource.List(accountId: string; profileId: string; webPropertyId: string; AQuery : TManagementProfileFilterLinkslistOptions) : TProfileFilterLinks;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'max-results',AQuery.maxresults);
  AddToQuery(_Q,'start-index',AQuery.startindex);
  Result:=List(accountId,profileId,webPropertyId,_Q);
end;

Function TManagementProfileFilterLinksResource.Patch(accountId: string; linkId: string; profileId: string; webPropertyId: string; aProfileFilterLink : TProfileFilterLink) : TProfileFilterLink;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/profileFilterLinks/{linkId}';
  _Methodid   = 'analytics.management.profileFilterLinks.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'linkId',linkId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aProfileFilterLink,TProfileFilterLink) as TProfileFilterLink;
end;

Function TManagementProfileFilterLinksResource.Update(accountId: string; linkId: string; profileId: string; webPropertyId: string; aProfileFilterLink : TProfileFilterLink) : TProfileFilterLink;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/profileFilterLinks/{linkId}';
  _Methodid   = 'analytics.management.profileFilterLinks.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'linkId',linkId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aProfileFilterLink,TProfileFilterLink) as TProfileFilterLink;
end;



{ --------------------------------------------------------------------
  TManagementProfileUserLinksResource
  --------------------------------------------------------------------}


Class Function TManagementProfileUserLinksResource.ResourceName : String;

begin
  Result:='profileUserLinks';
end;

Class Function TManagementProfileUserLinksResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Procedure TManagementProfileUserLinksResource.Delete(accountId: string; linkId: string; profileId: string; webPropertyId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/entityUserLinks/{linkId}';
  _Methodid   = 'analytics.management.profileUserLinks.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'linkId',linkId,'profileId',profileId,'webPropertyId',webPropertyId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TManagementProfileUserLinksResource.Insert(accountId: string; profileId: string; webPropertyId: string; aEntityUserLink : TEntityUserLink) : TEntityUserLink;

Const
  _HTTPMethod = 'POST';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/entityUserLinks';
  _Methodid   = 'analytics.management.profileUserLinks.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aEntityUserLink,TEntityUserLink) as TEntityUserLink;
end;

Function TManagementProfileUserLinksResource.List(accountId: string; profileId: string; webPropertyId: string; AQuery : string = '') : TEntityUserLinks;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/entityUserLinks';
  _Methodid   = 'analytics.management.profileUserLinks.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TEntityUserLinks) as TEntityUserLinks;
end;


Function TManagementProfileUserLinksResource.List(accountId: string; profileId: string; webPropertyId: string; AQuery : TManagementProfileUserLinkslistOptions) : TEntityUserLinks;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'max-results',AQuery.maxresults);
  AddToQuery(_Q,'start-index',AQuery.startindex);
  Result:=List(accountId,profileId,webPropertyId,_Q);
end;

Function TManagementProfileUserLinksResource.Update(accountId: string; linkId: string; profileId: string; webPropertyId: string; aEntityUserLink : TEntityUserLink) : TEntityUserLink;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/entityUserLinks/{linkId}';
  _Methodid   = 'analytics.management.profileUserLinks.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'linkId',linkId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aEntityUserLink,TEntityUserLink) as TEntityUserLink;
end;



{ --------------------------------------------------------------------
  TManagementProfilesResource
  --------------------------------------------------------------------}


Class Function TManagementProfilesResource.ResourceName : String;

begin
  Result:='profiles';
end;

Class Function TManagementProfilesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Procedure TManagementProfilesResource.Delete(accountId: string; profileId: string; webPropertyId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}';
  _Methodid   = 'analytics.management.profiles.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'profileId',profileId,'webPropertyId',webPropertyId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TManagementProfilesResource.Get(accountId: string; profileId: string; webPropertyId: string) : TProfile;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}';
  _Methodid   = 'analytics.management.profiles.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TProfile) as TProfile;
end;

Function TManagementProfilesResource.Insert(accountId: string; webPropertyId: string; aProfile : TProfile) : TProfile;

Const
  _HTTPMethod = 'POST';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles';
  _Methodid   = 'analytics.management.profiles.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aProfile,TProfile) as TProfile;
end;

Function TManagementProfilesResource.List(accountId: string; webPropertyId: string; AQuery : string = '') : TProfiles;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles';
  _Methodid   = 'analytics.management.profiles.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TProfiles) as TProfiles;
end;


Function TManagementProfilesResource.List(accountId: string; webPropertyId: string; AQuery : TManagementProfileslistOptions) : TProfiles;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'max-results',AQuery.maxresults);
  AddToQuery(_Q,'start-index',AQuery.startindex);
  Result:=List(accountId,webPropertyId,_Q);
end;

Function TManagementProfilesResource.Patch(accountId: string; profileId: string; webPropertyId: string; aProfile : TProfile) : TProfile;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}';
  _Methodid   = 'analytics.management.profiles.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aProfile,TProfile) as TProfile;
end;

Function TManagementProfilesResource.Update(accountId: string; profileId: string; webPropertyId: string; aProfile : TProfile) : TProfile;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}';
  _Methodid   = 'analytics.management.profiles.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aProfile,TProfile) as TProfile;
end;



{ --------------------------------------------------------------------
  TManagementSegmentsResource
  --------------------------------------------------------------------}


Class Function TManagementSegmentsResource.ResourceName : String;

begin
  Result:='segments';
end;

Class Function TManagementSegmentsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Function TManagementSegmentsResource.List(AQuery : string = '') : TSegments;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/segments';
  _Methodid   = 'analytics.management.segments.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TSegments) as TSegments;
end;


Function TManagementSegmentsResource.List(AQuery : TManagementSegmentslistOptions) : TSegments;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'max-results',AQuery.maxresults);
  AddToQuery(_Q,'start-index',AQuery.startindex);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TManagementUnsampledReportsResource
  --------------------------------------------------------------------}


Class Function TManagementUnsampledReportsResource.ResourceName : String;

begin
  Result:='unsampledReports';
end;

Class Function TManagementUnsampledReportsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Function TManagementUnsampledReportsResource.Get(accountId: string; profileId: string; unsampledReportId: string; webPropertyId: string) : TUnsampledReport;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/unsampledReports/{unsampledReportId}';
  _Methodid   = 'analytics.management.unsampledReports.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'profileId',profileId,'unsampledReportId',unsampledReportId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TUnsampledReport) as TUnsampledReport;
end;

Function TManagementUnsampledReportsResource.Insert(accountId: string; profileId: string; webPropertyId: string; aUnsampledReport : TUnsampledReport) : TUnsampledReport;

Const
  _HTTPMethod = 'POST';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/unsampledReports';
  _Methodid   = 'analytics.management.unsampledReports.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aUnsampledReport,TUnsampledReport) as TUnsampledReport;
end;

Function TManagementUnsampledReportsResource.List(accountId: string; profileId: string; webPropertyId: string; AQuery : string = '') : TUnsampledReports;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/unsampledReports';
  _Methodid   = 'analytics.management.unsampledReports.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'profileId',profileId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TUnsampledReports) as TUnsampledReports;
end;


Function TManagementUnsampledReportsResource.List(accountId: string; profileId: string; webPropertyId: string; AQuery : TManagementUnsampledReportslistOptions) : TUnsampledReports;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'max-results',AQuery.maxresults);
  AddToQuery(_Q,'start-index',AQuery.startindex);
  Result:=List(accountId,profileId,webPropertyId,_Q);
end;



{ --------------------------------------------------------------------
  TManagementUploadsResource
  --------------------------------------------------------------------}


Class Function TManagementUploadsResource.ResourceName : String;

begin
  Result:='uploads';
end;

Class Function TManagementUploadsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Procedure TManagementUploadsResource.DeleteUploadData(accountId: string; customDataSourceId: string; webPropertyId: string; aAnalyticsDataimportDeleteUploadDataRequest : TAnalyticsDataimportDeleteUploadDataRequest);

Const
  _HTTPMethod = 'POST';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/customDataSources/{customDataSourceId}/deleteUploadData';
  _Methodid   = 'analytics.management.uploads.deleteUploadData';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'customDataSourceId',customDataSourceId,'webPropertyId',webPropertyId]);
  ServiceCall(_HTTPMethod,_P,'',aAnalyticsDataimportDeleteUploadDataRequest,Nil);
end;

Function TManagementUploadsResource.Get(accountId: string; customDataSourceId: string; uploadId: string; webPropertyId: string) : TUpload;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/customDataSources/{customDataSourceId}/uploads/{uploadId}';
  _Methodid   = 'analytics.management.uploads.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'customDataSourceId',customDataSourceId,'uploadId',uploadId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TUpload) as TUpload;
end;

Function TManagementUploadsResource.List(accountId: string; customDataSourceId: string; webPropertyId: string; AQuery : string = '') : TUploads;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/customDataSources/{customDataSourceId}/uploads';
  _Methodid   = 'analytics.management.uploads.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'customDataSourceId',customDataSourceId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TUploads) as TUploads;
end;


Function TManagementUploadsResource.List(accountId: string; customDataSourceId: string; webPropertyId: string; AQuery : TManagementUploadslistOptions) : TUploads;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'max-results',AQuery.maxresults);
  AddToQuery(_Q,'start-index',AQuery.startindex);
  Result:=List(accountId,customDataSourceId,webPropertyId,_Q);
end;

Function TManagementUploadsResource.UploadData(accountId: string; customDataSourceId: string; webPropertyId: string) : TUpload;

Const
  _HTTPMethod = 'POST';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/customDataSources/{customDataSourceId}/uploads';
  _Methodid   = 'analytics.management.uploads.uploadData';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'customDataSourceId',customDataSourceId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TUpload) as TUpload;
end;



{ --------------------------------------------------------------------
  TManagementWebPropertyAdWordsLinksResource
  --------------------------------------------------------------------}


Class Function TManagementWebPropertyAdWordsLinksResource.ResourceName : String;

begin
  Result:='webPropertyAdWordsLinks';
end;

Class Function TManagementWebPropertyAdWordsLinksResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Procedure TManagementWebPropertyAdWordsLinksResource.Delete(accountId: string; webPropertyAdWordsLinkId: string; webPropertyId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/entityAdWordsLinks/{webPropertyAdWordsLinkId}';
  _Methodid   = 'analytics.management.webPropertyAdWordsLinks.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'webPropertyAdWordsLinkId',webPropertyAdWordsLinkId,'webPropertyId',webPropertyId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TManagementWebPropertyAdWordsLinksResource.Get(accountId: string; webPropertyAdWordsLinkId: string; webPropertyId: string) : TEntityAdWordsLink;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/entityAdWordsLinks/{webPropertyAdWordsLinkId}';
  _Methodid   = 'analytics.management.webPropertyAdWordsLinks.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'webPropertyAdWordsLinkId',webPropertyAdWordsLinkId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEntityAdWordsLink) as TEntityAdWordsLink;
end;

Function TManagementWebPropertyAdWordsLinksResource.Insert(accountId: string; webPropertyId: string; aEntityAdWordsLink : TEntityAdWordsLink) : TEntityAdWordsLink;

Const
  _HTTPMethod = 'POST';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/entityAdWordsLinks';
  _Methodid   = 'analytics.management.webPropertyAdWordsLinks.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aEntityAdWordsLink,TEntityAdWordsLink) as TEntityAdWordsLink;
end;

Function TManagementWebPropertyAdWordsLinksResource.List(accountId: string; webPropertyId: string; AQuery : string = '') : TEntityAdWordsLinks;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/entityAdWordsLinks';
  _Methodid   = 'analytics.management.webPropertyAdWordsLinks.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TEntityAdWordsLinks) as TEntityAdWordsLinks;
end;


Function TManagementWebPropertyAdWordsLinksResource.List(accountId: string; webPropertyId: string; AQuery : TManagementWebPropertyAdWordsLinkslistOptions) : TEntityAdWordsLinks;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'max-results',AQuery.maxresults);
  AddToQuery(_Q,'start-index',AQuery.startindex);
  Result:=List(accountId,webPropertyId,_Q);
end;

Function TManagementWebPropertyAdWordsLinksResource.Patch(accountId: string; webPropertyAdWordsLinkId: string; webPropertyId: string; aEntityAdWordsLink : TEntityAdWordsLink) : TEntityAdWordsLink;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/entityAdWordsLinks/{webPropertyAdWordsLinkId}';
  _Methodid   = 'analytics.management.webPropertyAdWordsLinks.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'webPropertyAdWordsLinkId',webPropertyAdWordsLinkId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aEntityAdWordsLink,TEntityAdWordsLink) as TEntityAdWordsLink;
end;

Function TManagementWebPropertyAdWordsLinksResource.Update(accountId: string; webPropertyAdWordsLinkId: string; webPropertyId: string; aEntityAdWordsLink : TEntityAdWordsLink) : TEntityAdWordsLink;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/entityAdWordsLinks/{webPropertyAdWordsLinkId}';
  _Methodid   = 'analytics.management.webPropertyAdWordsLinks.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'webPropertyAdWordsLinkId',webPropertyAdWordsLinkId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aEntityAdWordsLink,TEntityAdWordsLink) as TEntityAdWordsLink;
end;



{ --------------------------------------------------------------------
  TManagementWebpropertiesResource
  --------------------------------------------------------------------}


Class Function TManagementWebpropertiesResource.ResourceName : String;

begin
  Result:='webproperties';
end;

Class Function TManagementWebpropertiesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Function TManagementWebpropertiesResource.Get(accountId: string; webPropertyId: string) : TWebproperty;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}';
  _Methodid   = 'analytics.management.webproperties.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TWebproperty) as TWebproperty;
end;

Function TManagementWebpropertiesResource.Insert(accountId: string; aWebproperty : TWebproperty) : TWebproperty;

Const
  _HTTPMethod = 'POST';
  _Path       = 'management/accounts/{accountId}/webproperties';
  _Methodid   = 'analytics.management.webproperties.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aWebproperty,TWebproperty) as TWebproperty;
end;

Function TManagementWebpropertiesResource.List(accountId: string; AQuery : string = '') : TWebproperties;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties';
  _Methodid   = 'analytics.management.webproperties.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TWebproperties) as TWebproperties;
end;


Function TManagementWebpropertiesResource.List(accountId: string; AQuery : TManagementWebpropertieslistOptions) : TWebproperties;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'max-results',AQuery.maxresults);
  AddToQuery(_Q,'start-index',AQuery.startindex);
  Result:=List(accountId,_Q);
end;

Function TManagementWebpropertiesResource.Patch(accountId: string; webPropertyId: string; aWebproperty : TWebproperty) : TWebproperty;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}';
  _Methodid   = 'analytics.management.webproperties.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aWebproperty,TWebproperty) as TWebproperty;
end;

Function TManagementWebpropertiesResource.Update(accountId: string; webPropertyId: string; aWebproperty : TWebproperty) : TWebproperty;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}';
  _Methodid   = 'analytics.management.webproperties.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aWebproperty,TWebproperty) as TWebproperty;
end;



{ --------------------------------------------------------------------
  TManagementWebpropertyUserLinksResource
  --------------------------------------------------------------------}


Class Function TManagementWebpropertyUserLinksResource.ResourceName : String;

begin
  Result:='webpropertyUserLinks';
end;

Class Function TManagementWebpropertyUserLinksResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Procedure TManagementWebpropertyUserLinksResource.Delete(accountId: string; linkId: string; webPropertyId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/entityUserLinks/{linkId}';
  _Methodid   = 'analytics.management.webpropertyUserLinks.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'linkId',linkId,'webPropertyId',webPropertyId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TManagementWebpropertyUserLinksResource.Insert(accountId: string; webPropertyId: string; aEntityUserLink : TEntityUserLink) : TEntityUserLink;

Const
  _HTTPMethod = 'POST';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/entityUserLinks';
  _Methodid   = 'analytics.management.webpropertyUserLinks.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aEntityUserLink,TEntityUserLink) as TEntityUserLink;
end;

Function TManagementWebpropertyUserLinksResource.List(accountId: string; webPropertyId: string; AQuery : string = '') : TEntityUserLinks;

Const
  _HTTPMethod = 'GET';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/entityUserLinks';
  _Methodid   = 'analytics.management.webpropertyUserLinks.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TEntityUserLinks) as TEntityUserLinks;
end;


Function TManagementWebpropertyUserLinksResource.List(accountId: string; webPropertyId: string; AQuery : TManagementWebpropertyUserLinkslistOptions) : TEntityUserLinks;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'max-results',AQuery.maxresults);
  AddToQuery(_Q,'start-index',AQuery.startindex);
  Result:=List(accountId,webPropertyId,_Q);
end;

Function TManagementWebpropertyUserLinksResource.Update(accountId: string; linkId: string; webPropertyId: string; aEntityUserLink : TEntityUserLink) : TEntityUserLink;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'management/accounts/{accountId}/webproperties/{webPropertyId}/entityUserLinks/{linkId}';
  _Methodid   = 'analytics.management.webpropertyUserLinks.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'linkId',linkId,'webPropertyId',webPropertyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aEntityUserLink,TEntityUserLink) as TEntityUserLink;
end;



{ --------------------------------------------------------------------
  TManagementResource
  --------------------------------------------------------------------}


Class Function TManagementResource.ResourceName : String;

begin
  Result:='management';
end;

Class Function TManagementResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;



Function TManagementResource.GetAccountSummariesInstance : TManagementAccountSummariesResource;

begin
  if (FAccountSummariesInstance=Nil) then
    FAccountSummariesInstance:=CreateAccountSummariesResource;
  Result:=FAccountSummariesInstance;
end;

Function TManagementResource.CreateAccountSummariesResource : TManagementAccountSummariesResource;

begin
  Result:=CreateAccountSummariesResource(Self);
end;


Function TManagementResource.CreateAccountSummariesResource(AOwner : TComponent) : TManagementAccountSummariesResource;

begin
  Result:=TManagementAccountSummariesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TManagementResource.GetAccountUserLinksInstance : TManagementAccountUserLinksResource;

begin
  if (FAccountUserLinksInstance=Nil) then
    FAccountUserLinksInstance:=CreateAccountUserLinksResource;
  Result:=FAccountUserLinksInstance;
end;

Function TManagementResource.CreateAccountUserLinksResource : TManagementAccountUserLinksResource;

begin
  Result:=CreateAccountUserLinksResource(Self);
end;


Function TManagementResource.CreateAccountUserLinksResource(AOwner : TComponent) : TManagementAccountUserLinksResource;

begin
  Result:=TManagementAccountUserLinksResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TManagementResource.GetAccountsInstance : TManagementAccountsResource;

begin
  if (FAccountsInstance=Nil) then
    FAccountsInstance:=CreateAccountsResource;
  Result:=FAccountsInstance;
end;

Function TManagementResource.CreateAccountsResource : TManagementAccountsResource;

begin
  Result:=CreateAccountsResource(Self);
end;


Function TManagementResource.CreateAccountsResource(AOwner : TComponent) : TManagementAccountsResource;

begin
  Result:=TManagementAccountsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TManagementResource.GetCustomDataSourcesInstance : TManagementCustomDataSourcesResource;

begin
  if (FCustomDataSourcesInstance=Nil) then
    FCustomDataSourcesInstance:=CreateCustomDataSourcesResource;
  Result:=FCustomDataSourcesInstance;
end;

Function TManagementResource.CreateCustomDataSourcesResource : TManagementCustomDataSourcesResource;

begin
  Result:=CreateCustomDataSourcesResource(Self);
end;


Function TManagementResource.CreateCustomDataSourcesResource(AOwner : TComponent) : TManagementCustomDataSourcesResource;

begin
  Result:=TManagementCustomDataSourcesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TManagementResource.GetCustomDimensionsInstance : TManagementCustomDimensionsResource;

begin
  if (FCustomDimensionsInstance=Nil) then
    FCustomDimensionsInstance:=CreateCustomDimensionsResource;
  Result:=FCustomDimensionsInstance;
end;

Function TManagementResource.CreateCustomDimensionsResource : TManagementCustomDimensionsResource;

begin
  Result:=CreateCustomDimensionsResource(Self);
end;


Function TManagementResource.CreateCustomDimensionsResource(AOwner : TComponent) : TManagementCustomDimensionsResource;

begin
  Result:=TManagementCustomDimensionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TManagementResource.GetCustomMetricsInstance : TManagementCustomMetricsResource;

begin
  if (FCustomMetricsInstance=Nil) then
    FCustomMetricsInstance:=CreateCustomMetricsResource;
  Result:=FCustomMetricsInstance;
end;

Function TManagementResource.CreateCustomMetricsResource : TManagementCustomMetricsResource;

begin
  Result:=CreateCustomMetricsResource(Self);
end;


Function TManagementResource.CreateCustomMetricsResource(AOwner : TComponent) : TManagementCustomMetricsResource;

begin
  Result:=TManagementCustomMetricsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TManagementResource.GetExperimentsInstance : TManagementExperimentsResource;

begin
  if (FExperimentsInstance=Nil) then
    FExperimentsInstance:=CreateExperimentsResource;
  Result:=FExperimentsInstance;
end;

Function TManagementResource.CreateExperimentsResource : TManagementExperimentsResource;

begin
  Result:=CreateExperimentsResource(Self);
end;


Function TManagementResource.CreateExperimentsResource(AOwner : TComponent) : TManagementExperimentsResource;

begin
  Result:=TManagementExperimentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TManagementResource.GetFiltersInstance : TManagementFiltersResource;

begin
  if (FFiltersInstance=Nil) then
    FFiltersInstance:=CreateFiltersResource;
  Result:=FFiltersInstance;
end;

Function TManagementResource.CreateFiltersResource : TManagementFiltersResource;

begin
  Result:=CreateFiltersResource(Self);
end;


Function TManagementResource.CreateFiltersResource(AOwner : TComponent) : TManagementFiltersResource;

begin
  Result:=TManagementFiltersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TManagementResource.GetGoalsInstance : TManagementGoalsResource;

begin
  if (FGoalsInstance=Nil) then
    FGoalsInstance:=CreateGoalsResource;
  Result:=FGoalsInstance;
end;

Function TManagementResource.CreateGoalsResource : TManagementGoalsResource;

begin
  Result:=CreateGoalsResource(Self);
end;


Function TManagementResource.CreateGoalsResource(AOwner : TComponent) : TManagementGoalsResource;

begin
  Result:=TManagementGoalsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TManagementResource.GetProfileFilterLinksInstance : TManagementProfileFilterLinksResource;

begin
  if (FProfileFilterLinksInstance=Nil) then
    FProfileFilterLinksInstance:=CreateProfileFilterLinksResource;
  Result:=FProfileFilterLinksInstance;
end;

Function TManagementResource.CreateProfileFilterLinksResource : TManagementProfileFilterLinksResource;

begin
  Result:=CreateProfileFilterLinksResource(Self);
end;


Function TManagementResource.CreateProfileFilterLinksResource(AOwner : TComponent) : TManagementProfileFilterLinksResource;

begin
  Result:=TManagementProfileFilterLinksResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TManagementResource.GetProfileUserLinksInstance : TManagementProfileUserLinksResource;

begin
  if (FProfileUserLinksInstance=Nil) then
    FProfileUserLinksInstance:=CreateProfileUserLinksResource;
  Result:=FProfileUserLinksInstance;
end;

Function TManagementResource.CreateProfileUserLinksResource : TManagementProfileUserLinksResource;

begin
  Result:=CreateProfileUserLinksResource(Self);
end;


Function TManagementResource.CreateProfileUserLinksResource(AOwner : TComponent) : TManagementProfileUserLinksResource;

begin
  Result:=TManagementProfileUserLinksResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TManagementResource.GetProfilesInstance : TManagementProfilesResource;

begin
  if (FProfilesInstance=Nil) then
    FProfilesInstance:=CreateProfilesResource;
  Result:=FProfilesInstance;
end;

Function TManagementResource.CreateProfilesResource : TManagementProfilesResource;

begin
  Result:=CreateProfilesResource(Self);
end;


Function TManagementResource.CreateProfilesResource(AOwner : TComponent) : TManagementProfilesResource;

begin
  Result:=TManagementProfilesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TManagementResource.GetSegmentsInstance : TManagementSegmentsResource;

begin
  if (FSegmentsInstance=Nil) then
    FSegmentsInstance:=CreateSegmentsResource;
  Result:=FSegmentsInstance;
end;

Function TManagementResource.CreateSegmentsResource : TManagementSegmentsResource;

begin
  Result:=CreateSegmentsResource(Self);
end;


Function TManagementResource.CreateSegmentsResource(AOwner : TComponent) : TManagementSegmentsResource;

begin
  Result:=TManagementSegmentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TManagementResource.GetUnsampledReportsInstance : TManagementUnsampledReportsResource;

begin
  if (FUnsampledReportsInstance=Nil) then
    FUnsampledReportsInstance:=CreateUnsampledReportsResource;
  Result:=FUnsampledReportsInstance;
end;

Function TManagementResource.CreateUnsampledReportsResource : TManagementUnsampledReportsResource;

begin
  Result:=CreateUnsampledReportsResource(Self);
end;


Function TManagementResource.CreateUnsampledReportsResource(AOwner : TComponent) : TManagementUnsampledReportsResource;

begin
  Result:=TManagementUnsampledReportsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TManagementResource.GetUploadsInstance : TManagementUploadsResource;

begin
  if (FUploadsInstance=Nil) then
    FUploadsInstance:=CreateUploadsResource;
  Result:=FUploadsInstance;
end;

Function TManagementResource.CreateUploadsResource : TManagementUploadsResource;

begin
  Result:=CreateUploadsResource(Self);
end;


Function TManagementResource.CreateUploadsResource(AOwner : TComponent) : TManagementUploadsResource;

begin
  Result:=TManagementUploadsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TManagementResource.GetWebPropertyAdWordsLinksInstance : TManagementWebPropertyAdWordsLinksResource;

begin
  if (FWebPropertyAdWordsLinksInstance=Nil) then
    FWebPropertyAdWordsLinksInstance:=CreateWebPropertyAdWordsLinksResource;
  Result:=FWebPropertyAdWordsLinksInstance;
end;

Function TManagementResource.CreateWebPropertyAdWordsLinksResource : TManagementWebPropertyAdWordsLinksResource;

begin
  Result:=CreateWebPropertyAdWordsLinksResource(Self);
end;


Function TManagementResource.CreateWebPropertyAdWordsLinksResource(AOwner : TComponent) : TManagementWebPropertyAdWordsLinksResource;

begin
  Result:=TManagementWebPropertyAdWordsLinksResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TManagementResource.GetWebpropertiesInstance : TManagementWebpropertiesResource;

begin
  if (FWebpropertiesInstance=Nil) then
    FWebpropertiesInstance:=CreateWebpropertiesResource;
  Result:=FWebpropertiesInstance;
end;

Function TManagementResource.CreateWebpropertiesResource : TManagementWebpropertiesResource;

begin
  Result:=CreateWebpropertiesResource(Self);
end;


Function TManagementResource.CreateWebpropertiesResource(AOwner : TComponent) : TManagementWebpropertiesResource;

begin
  Result:=TManagementWebpropertiesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TManagementResource.GetWebpropertyUserLinksInstance : TManagementWebpropertyUserLinksResource;

begin
  if (FWebpropertyUserLinksInstance=Nil) then
    FWebpropertyUserLinksInstance:=CreateWebpropertyUserLinksResource;
  Result:=FWebpropertyUserLinksInstance;
end;

Function TManagementResource.CreateWebpropertyUserLinksResource : TManagementWebpropertyUserLinksResource;

begin
  Result:=CreateWebpropertyUserLinksResource(Self);
end;


Function TManagementResource.CreateWebpropertyUserLinksResource(AOwner : TComponent) : TManagementWebpropertyUserLinksResource;

begin
  Result:=TManagementWebpropertyUserLinksResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TMetadataColumnsResource
  --------------------------------------------------------------------}


Class Function TMetadataColumnsResource.ResourceName : String;

begin
  Result:='columns';
end;

Class Function TMetadataColumnsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Function TMetadataColumnsResource.List(reportType: string) : TColumns;

Const
  _HTTPMethod = 'GET';
  _Path       = 'metadata/{reportType}/columns';
  _Methodid   = 'analytics.metadata.columns.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['reportType',reportType]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TColumns) as TColumns;
end;



{ --------------------------------------------------------------------
  TMetadataResource
  --------------------------------------------------------------------}


Class Function TMetadataResource.ResourceName : String;

begin
  Result:='metadata';
end;

Class Function TMetadataResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;



Function TMetadataResource.GetColumnsInstance : TMetadataColumnsResource;

begin
  if (FColumnsInstance=Nil) then
    FColumnsInstance:=CreateColumnsResource;
  Result:=FColumnsInstance;
end;

Function TMetadataResource.CreateColumnsResource : TMetadataColumnsResource;

begin
  Result:=CreateColumnsResource(Self);
end;


Function TMetadataResource.CreateColumnsResource(AOwner : TComponent) : TMetadataColumnsResource;

begin
  Result:=TMetadataColumnsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TProvisioningResource
  --------------------------------------------------------------------}


Class Function TProvisioningResource.ResourceName : String;

begin
  Result:='provisioning';
end;

Class Function TProvisioningResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsAPI;
end;

Function TProvisioningResource.CreateAccountTicket(aAccountTicket : TAccountTicket) : TAccountTicket;

Const
  _HTTPMethod = 'POST';
  _Path       = 'provisioning/createAccountTicket';
  _Methodid   = 'analytics.provisioning.createAccountTicket';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aAccountTicket,TAccountTicket) as TAccountTicket;
end;



{ --------------------------------------------------------------------
  TAnalyticsAPI
  --------------------------------------------------------------------}

Class Function TAnalyticsAPI.APIName : String;

begin
  Result:='analytics';
end;

Class Function TAnalyticsAPI.APIVersion : String;

begin
  Result:='v3';
end;

Class Function TAnalyticsAPI.APIRevision : String;

begin
  Result:='20150417';
end;

Class Function TAnalyticsAPI.APIID : String;

begin
  Result:='analytics:v3';
end;

Class Function TAnalyticsAPI.APITitle : String;

begin
  Result:='Google Analytics API';
end;

Class Function TAnalyticsAPI.APIDescription : String;

begin
  Result:='View and manage your Google Analytics data';
end;

Class Function TAnalyticsAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TAnalyticsAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TAnalyticsAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/analytics-16.png';
end;

Class Function TAnalyticsAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/analytics-32.png';
end;

Class Function TAnalyticsAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/analytics/';
end;

Class Function TAnalyticsAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com:443/';
end;

Class Function TAnalyticsAPI.APIbasePath : string;

begin
  Result:='/analytics/v3/';
end;

Class Function TAnalyticsAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/analytics/v3/';
end;

Class Function TAnalyticsAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TAnalyticsAPI.APIservicePath : string;

begin
  Result:='analytics/v3/';
end;

Class Function TAnalyticsAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TAnalyticsAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,6);
  Result[0].Name:='https://www.googleapis.com/auth/analytics';
  Result[0].Description:='View and manage your Google Analytics data';
  Result[1].Name:='https://www.googleapis.com/auth/analytics.edit';
  Result[1].Description:='Edit Google Analytics management entities';
  Result[2].Name:='https://www.googleapis.com/auth/analytics.manage.users';
  Result[2].Description:='Manage Google Analytics Account users by email address';
  Result[3].Name:='https://www.googleapis.com/auth/analytics.manage.users.readonly';
  Result[3].Description:='View Google Analytics user permissions';
  Result[4].Name:='https://www.googleapis.com/auth/analytics.provision';
  Result[4].Description:='Create a new Google Analytics account along with its default property and view';
  Result[5].Name:='https://www.googleapis.com/auth/analytics.readonly';
  Result[5].Description:='View your Google Analytics data';
  
end;

Class Function TAnalyticsAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TAnalyticsAPI.RegisterAPIResources;

begin
  TAccountTypechildLink.RegisterObject;
  TAccountTypepermissions.RegisterObject;
  TAccount.RegisterObject;
  TAccountRef.RegisterObject;
  TAccountSummaries.RegisterObject;
  TAccountSummary.RegisterObject;
  TAccountTicket.RegisterObject;
  TAccounts.RegisterObject;
  TAdWordsAccount.RegisterObject;
  TAnalyticsDataimportDeleteUploadDataRequest.RegisterObject;
  TColumnTypeattributes.RegisterObject;
  TColumn.RegisterObject;
  TColumns.RegisterObject;
  TCustomDataSourceTypechildLink.RegisterObject;
  TCustomDataSourceTypeparentLink.RegisterObject;
  TCustomDataSource.RegisterObject;
  TCustomDataSources.RegisterObject;
  TCustomDimensionTypeparentLink.RegisterObject;
  TCustomDimension.RegisterObject;
  TCustomDimensions.RegisterObject;
  TCustomMetricTypeparentLink.RegisterObject;
  TCustomMetric.RegisterObject;
  TCustomMetrics.RegisterObject;
  TEntityAdWordsLinkTypeentity.RegisterObject;
  TEntityAdWordsLink.RegisterObject;
  TEntityAdWordsLinks.RegisterObject;
  TEntityUserLinkTypeentity.RegisterObject;
  TEntityUserLinkTypepermissions.RegisterObject;
  TEntityUserLink.RegisterObject;
  TEntityUserLinks.RegisterObject;
  TExperimentTypeparentLink.RegisterObject;
  TExperimentTypevariationsItem.RegisterObject;
  TExperiment.RegisterObject;
  TExperiments.RegisterObject;
  TFilterTypeadvancedDetails.RegisterObject;
  TFilterTypelowercaseDetails.RegisterObject;
  TFilterTypeparentLink.RegisterObject;
  TFilterTypesearchAndReplaceDetails.RegisterObject;
  TFilterTypeuppercaseDetails.RegisterObject;
  TFilter.RegisterObject;
  TFilterExpression.RegisterObject;
  TFilterRef.RegisterObject;
  TFilters.RegisterObject;
  TGaDataTypecolumnHeadersItem.RegisterObject;
  TGaDataTypedataTableTypecolsItem.RegisterObject;
  TGaDataTypedataTableTyperowsItemTypecItem.RegisterObject;
  TGaDataTypedataTableTyperowsItem.RegisterObject;
  TGaDataTypedataTable.RegisterObject;
  TGaDataTypeprofileInfo.RegisterObject;
  TGaDataTypequery.RegisterObject;
  TGaDataTypetotalsForAllResults.RegisterObject;
  TGaData.RegisterObject;
  TGoalTypeeventDetailsTypeeventConditionsItem.RegisterObject;
  TGoalTypeeventDetails.RegisterObject;
  TGoalTypeparentLink.RegisterObject;
  TGoalTypeurlDestinationDetailsTypestepsItem.RegisterObject;
  TGoalTypeurlDestinationDetails.RegisterObject;
  TGoalTypevisitNumPagesDetails.RegisterObject;
  TGoalTypevisitTimeOnSiteDetails.RegisterObject;
  TGoal.RegisterObject;
  TGoals.RegisterObject;
  TMcfDataTypecolumnHeadersItem.RegisterObject;
  TMcfDataTypeprofileInfo.RegisterObject;
  TMcfDataTypequery.RegisterObject;
  TMcfDataTyperowsItemItemTypeconversionPathValueItem.RegisterObject;
  TMcfDataTyperowsItemItem.RegisterObject;
  TMcfDataTypetotalsForAllResults.RegisterObject;
  TMcfData.RegisterObject;
  TProfileTypechildLink.RegisterObject;
  TProfileTypeparentLink.RegisterObject;
  TProfileTypepermissions.RegisterObject;
  TProfile.RegisterObject;
  TProfileFilterLink.RegisterObject;
  TProfileFilterLinks.RegisterObject;
  TProfileRef.RegisterObject;
  TProfileSummary.RegisterObject;
  TProfiles.RegisterObject;
  TRealtimeDataTypecolumnHeadersItem.RegisterObject;
  TRealtimeDataTypeprofileInfo.RegisterObject;
  TRealtimeDataTypequery.RegisterObject;
  TRealtimeDataTypetotalsForAllResults.RegisterObject;
  TRealtimeData.RegisterObject;
  TSegment.RegisterObject;
  TSegments.RegisterObject;
  TUnsampledReportTypecloudStorageDownloadDetails.RegisterObject;
  TUnsampledReportTypedriveDownloadDetails.RegisterObject;
  TUnsampledReport.RegisterObject;
  TUnsampledReports.RegisterObject;
  TUpload.RegisterObject;
  TUploads.RegisterObject;
  TUserRef.RegisterObject;
  TWebPropertyRef.RegisterObject;
  TWebPropertySummary.RegisterObject;
  TWebproperties.RegisterObject;
  TWebpropertyTypechildLink.RegisterObject;
  TWebpropertyTypeparentLink.RegisterObject;
  TWebpropertyTypepermissions.RegisterObject;
  TWebproperty.RegisterObject;
end;


Function TAnalyticsAPI.GetDataGaInstance : TDataGaResource;

begin
  if (FDataGaInstance=Nil) then
    FDataGaInstance:=CreateDataGaResource;
  Result:=FDataGaInstance;
end;

Function TAnalyticsAPI.CreateDataGaResource : TDataGaResource;

begin
  Result:=CreateDataGaResource(Self);
end;


Function TAnalyticsAPI.CreateDataGaResource(AOwner : TComponent) : TDataGaResource;

begin
  Result:=TDataGaResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetDataMcfInstance : TDataMcfResource;

begin
  if (FDataMcfInstance=Nil) then
    FDataMcfInstance:=CreateDataMcfResource;
  Result:=FDataMcfInstance;
end;

Function TAnalyticsAPI.CreateDataMcfResource : TDataMcfResource;

begin
  Result:=CreateDataMcfResource(Self);
end;


Function TAnalyticsAPI.CreateDataMcfResource(AOwner : TComponent) : TDataMcfResource;

begin
  Result:=TDataMcfResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetDataRealtimeInstance : TDataRealtimeResource;

begin
  if (FDataRealtimeInstance=Nil) then
    FDataRealtimeInstance:=CreateDataRealtimeResource;
  Result:=FDataRealtimeInstance;
end;

Function TAnalyticsAPI.CreateDataRealtimeResource : TDataRealtimeResource;

begin
  Result:=CreateDataRealtimeResource(Self);
end;


Function TAnalyticsAPI.CreateDataRealtimeResource(AOwner : TComponent) : TDataRealtimeResource;

begin
  Result:=TDataRealtimeResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetDataInstance : TDataResource;

begin
  if (FDataInstance=Nil) then
    FDataInstance:=CreateDataResource;
  Result:=FDataInstance;
end;

Function TAnalyticsAPI.CreateDataResource : TDataResource;

begin
  Result:=CreateDataResource(Self);
end;


Function TAnalyticsAPI.CreateDataResource(AOwner : TComponent) : TDataResource;

begin
  Result:=TDataResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetManagementAccountSummariesInstance : TManagementAccountSummariesResource;

begin
  if (FManagementAccountSummariesInstance=Nil) then
    FManagementAccountSummariesInstance:=CreateManagementAccountSummariesResource;
  Result:=FManagementAccountSummariesInstance;
end;

Function TAnalyticsAPI.CreateManagementAccountSummariesResource : TManagementAccountSummariesResource;

begin
  Result:=CreateManagementAccountSummariesResource(Self);
end;


Function TAnalyticsAPI.CreateManagementAccountSummariesResource(AOwner : TComponent) : TManagementAccountSummariesResource;

begin
  Result:=TManagementAccountSummariesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetManagementAccountUserLinksInstance : TManagementAccountUserLinksResource;

begin
  if (FManagementAccountUserLinksInstance=Nil) then
    FManagementAccountUserLinksInstance:=CreateManagementAccountUserLinksResource;
  Result:=FManagementAccountUserLinksInstance;
end;

Function TAnalyticsAPI.CreateManagementAccountUserLinksResource : TManagementAccountUserLinksResource;

begin
  Result:=CreateManagementAccountUserLinksResource(Self);
end;


Function TAnalyticsAPI.CreateManagementAccountUserLinksResource(AOwner : TComponent) : TManagementAccountUserLinksResource;

begin
  Result:=TManagementAccountUserLinksResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetManagementAccountsInstance : TManagementAccountsResource;

begin
  if (FManagementAccountsInstance=Nil) then
    FManagementAccountsInstance:=CreateManagementAccountsResource;
  Result:=FManagementAccountsInstance;
end;

Function TAnalyticsAPI.CreateManagementAccountsResource : TManagementAccountsResource;

begin
  Result:=CreateManagementAccountsResource(Self);
end;


Function TAnalyticsAPI.CreateManagementAccountsResource(AOwner : TComponent) : TManagementAccountsResource;

begin
  Result:=TManagementAccountsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetManagementCustomDataSourcesInstance : TManagementCustomDataSourcesResource;

begin
  if (FManagementCustomDataSourcesInstance=Nil) then
    FManagementCustomDataSourcesInstance:=CreateManagementCustomDataSourcesResource;
  Result:=FManagementCustomDataSourcesInstance;
end;

Function TAnalyticsAPI.CreateManagementCustomDataSourcesResource : TManagementCustomDataSourcesResource;

begin
  Result:=CreateManagementCustomDataSourcesResource(Self);
end;


Function TAnalyticsAPI.CreateManagementCustomDataSourcesResource(AOwner : TComponent) : TManagementCustomDataSourcesResource;

begin
  Result:=TManagementCustomDataSourcesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetManagementCustomDimensionsInstance : TManagementCustomDimensionsResource;

begin
  if (FManagementCustomDimensionsInstance=Nil) then
    FManagementCustomDimensionsInstance:=CreateManagementCustomDimensionsResource;
  Result:=FManagementCustomDimensionsInstance;
end;

Function TAnalyticsAPI.CreateManagementCustomDimensionsResource : TManagementCustomDimensionsResource;

begin
  Result:=CreateManagementCustomDimensionsResource(Self);
end;


Function TAnalyticsAPI.CreateManagementCustomDimensionsResource(AOwner : TComponent) : TManagementCustomDimensionsResource;

begin
  Result:=TManagementCustomDimensionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetManagementCustomMetricsInstance : TManagementCustomMetricsResource;

begin
  if (FManagementCustomMetricsInstance=Nil) then
    FManagementCustomMetricsInstance:=CreateManagementCustomMetricsResource;
  Result:=FManagementCustomMetricsInstance;
end;

Function TAnalyticsAPI.CreateManagementCustomMetricsResource : TManagementCustomMetricsResource;

begin
  Result:=CreateManagementCustomMetricsResource(Self);
end;


Function TAnalyticsAPI.CreateManagementCustomMetricsResource(AOwner : TComponent) : TManagementCustomMetricsResource;

begin
  Result:=TManagementCustomMetricsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetManagementExperimentsInstance : TManagementExperimentsResource;

begin
  if (FManagementExperimentsInstance=Nil) then
    FManagementExperimentsInstance:=CreateManagementExperimentsResource;
  Result:=FManagementExperimentsInstance;
end;

Function TAnalyticsAPI.CreateManagementExperimentsResource : TManagementExperimentsResource;

begin
  Result:=CreateManagementExperimentsResource(Self);
end;


Function TAnalyticsAPI.CreateManagementExperimentsResource(AOwner : TComponent) : TManagementExperimentsResource;

begin
  Result:=TManagementExperimentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetManagementFiltersInstance : TManagementFiltersResource;

begin
  if (FManagementFiltersInstance=Nil) then
    FManagementFiltersInstance:=CreateManagementFiltersResource;
  Result:=FManagementFiltersInstance;
end;

Function TAnalyticsAPI.CreateManagementFiltersResource : TManagementFiltersResource;

begin
  Result:=CreateManagementFiltersResource(Self);
end;


Function TAnalyticsAPI.CreateManagementFiltersResource(AOwner : TComponent) : TManagementFiltersResource;

begin
  Result:=TManagementFiltersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetManagementGoalsInstance : TManagementGoalsResource;

begin
  if (FManagementGoalsInstance=Nil) then
    FManagementGoalsInstance:=CreateManagementGoalsResource;
  Result:=FManagementGoalsInstance;
end;

Function TAnalyticsAPI.CreateManagementGoalsResource : TManagementGoalsResource;

begin
  Result:=CreateManagementGoalsResource(Self);
end;


Function TAnalyticsAPI.CreateManagementGoalsResource(AOwner : TComponent) : TManagementGoalsResource;

begin
  Result:=TManagementGoalsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetManagementProfileFilterLinksInstance : TManagementProfileFilterLinksResource;

begin
  if (FManagementProfileFilterLinksInstance=Nil) then
    FManagementProfileFilterLinksInstance:=CreateManagementProfileFilterLinksResource;
  Result:=FManagementProfileFilterLinksInstance;
end;

Function TAnalyticsAPI.CreateManagementProfileFilterLinksResource : TManagementProfileFilterLinksResource;

begin
  Result:=CreateManagementProfileFilterLinksResource(Self);
end;


Function TAnalyticsAPI.CreateManagementProfileFilterLinksResource(AOwner : TComponent) : TManagementProfileFilterLinksResource;

begin
  Result:=TManagementProfileFilterLinksResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetManagementProfileUserLinksInstance : TManagementProfileUserLinksResource;

begin
  if (FManagementProfileUserLinksInstance=Nil) then
    FManagementProfileUserLinksInstance:=CreateManagementProfileUserLinksResource;
  Result:=FManagementProfileUserLinksInstance;
end;

Function TAnalyticsAPI.CreateManagementProfileUserLinksResource : TManagementProfileUserLinksResource;

begin
  Result:=CreateManagementProfileUserLinksResource(Self);
end;


Function TAnalyticsAPI.CreateManagementProfileUserLinksResource(AOwner : TComponent) : TManagementProfileUserLinksResource;

begin
  Result:=TManagementProfileUserLinksResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetManagementProfilesInstance : TManagementProfilesResource;

begin
  if (FManagementProfilesInstance=Nil) then
    FManagementProfilesInstance:=CreateManagementProfilesResource;
  Result:=FManagementProfilesInstance;
end;

Function TAnalyticsAPI.CreateManagementProfilesResource : TManagementProfilesResource;

begin
  Result:=CreateManagementProfilesResource(Self);
end;


Function TAnalyticsAPI.CreateManagementProfilesResource(AOwner : TComponent) : TManagementProfilesResource;

begin
  Result:=TManagementProfilesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetManagementSegmentsInstance : TManagementSegmentsResource;

begin
  if (FManagementSegmentsInstance=Nil) then
    FManagementSegmentsInstance:=CreateManagementSegmentsResource;
  Result:=FManagementSegmentsInstance;
end;

Function TAnalyticsAPI.CreateManagementSegmentsResource : TManagementSegmentsResource;

begin
  Result:=CreateManagementSegmentsResource(Self);
end;


Function TAnalyticsAPI.CreateManagementSegmentsResource(AOwner : TComponent) : TManagementSegmentsResource;

begin
  Result:=TManagementSegmentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetManagementUnsampledReportsInstance : TManagementUnsampledReportsResource;

begin
  if (FManagementUnsampledReportsInstance=Nil) then
    FManagementUnsampledReportsInstance:=CreateManagementUnsampledReportsResource;
  Result:=FManagementUnsampledReportsInstance;
end;

Function TAnalyticsAPI.CreateManagementUnsampledReportsResource : TManagementUnsampledReportsResource;

begin
  Result:=CreateManagementUnsampledReportsResource(Self);
end;


Function TAnalyticsAPI.CreateManagementUnsampledReportsResource(AOwner : TComponent) : TManagementUnsampledReportsResource;

begin
  Result:=TManagementUnsampledReportsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetManagementUploadsInstance : TManagementUploadsResource;

begin
  if (FManagementUploadsInstance=Nil) then
    FManagementUploadsInstance:=CreateManagementUploadsResource;
  Result:=FManagementUploadsInstance;
end;

Function TAnalyticsAPI.CreateManagementUploadsResource : TManagementUploadsResource;

begin
  Result:=CreateManagementUploadsResource(Self);
end;


Function TAnalyticsAPI.CreateManagementUploadsResource(AOwner : TComponent) : TManagementUploadsResource;

begin
  Result:=TManagementUploadsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetManagementWebPropertyAdWordsLinksInstance : TManagementWebPropertyAdWordsLinksResource;

begin
  if (FManagementWebPropertyAdWordsLinksInstance=Nil) then
    FManagementWebPropertyAdWordsLinksInstance:=CreateManagementWebPropertyAdWordsLinksResource;
  Result:=FManagementWebPropertyAdWordsLinksInstance;
end;

Function TAnalyticsAPI.CreateManagementWebPropertyAdWordsLinksResource : TManagementWebPropertyAdWordsLinksResource;

begin
  Result:=CreateManagementWebPropertyAdWordsLinksResource(Self);
end;


Function TAnalyticsAPI.CreateManagementWebPropertyAdWordsLinksResource(AOwner : TComponent) : TManagementWebPropertyAdWordsLinksResource;

begin
  Result:=TManagementWebPropertyAdWordsLinksResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetManagementWebpropertiesInstance : TManagementWebpropertiesResource;

begin
  if (FManagementWebpropertiesInstance=Nil) then
    FManagementWebpropertiesInstance:=CreateManagementWebpropertiesResource;
  Result:=FManagementWebpropertiesInstance;
end;

Function TAnalyticsAPI.CreateManagementWebpropertiesResource : TManagementWebpropertiesResource;

begin
  Result:=CreateManagementWebpropertiesResource(Self);
end;


Function TAnalyticsAPI.CreateManagementWebpropertiesResource(AOwner : TComponent) : TManagementWebpropertiesResource;

begin
  Result:=TManagementWebpropertiesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetManagementWebpropertyUserLinksInstance : TManagementWebpropertyUserLinksResource;

begin
  if (FManagementWebpropertyUserLinksInstance=Nil) then
    FManagementWebpropertyUserLinksInstance:=CreateManagementWebpropertyUserLinksResource;
  Result:=FManagementWebpropertyUserLinksInstance;
end;

Function TAnalyticsAPI.CreateManagementWebpropertyUserLinksResource : TManagementWebpropertyUserLinksResource;

begin
  Result:=CreateManagementWebpropertyUserLinksResource(Self);
end;


Function TAnalyticsAPI.CreateManagementWebpropertyUserLinksResource(AOwner : TComponent) : TManagementWebpropertyUserLinksResource;

begin
  Result:=TManagementWebpropertyUserLinksResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetManagementInstance : TManagementResource;

begin
  if (FManagementInstance=Nil) then
    FManagementInstance:=CreateManagementResource;
  Result:=FManagementInstance;
end;

Function TAnalyticsAPI.CreateManagementResource : TManagementResource;

begin
  Result:=CreateManagementResource(Self);
end;


Function TAnalyticsAPI.CreateManagementResource(AOwner : TComponent) : TManagementResource;

begin
  Result:=TManagementResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetMetadataColumnsInstance : TMetadataColumnsResource;

begin
  if (FMetadataColumnsInstance=Nil) then
    FMetadataColumnsInstance:=CreateMetadataColumnsResource;
  Result:=FMetadataColumnsInstance;
end;

Function TAnalyticsAPI.CreateMetadataColumnsResource : TMetadataColumnsResource;

begin
  Result:=CreateMetadataColumnsResource(Self);
end;


Function TAnalyticsAPI.CreateMetadataColumnsResource(AOwner : TComponent) : TMetadataColumnsResource;

begin
  Result:=TMetadataColumnsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetMetadataInstance : TMetadataResource;

begin
  if (FMetadataInstance=Nil) then
    FMetadataInstance:=CreateMetadataResource;
  Result:=FMetadataInstance;
end;

Function TAnalyticsAPI.CreateMetadataResource : TMetadataResource;

begin
  Result:=CreateMetadataResource(Self);
end;


Function TAnalyticsAPI.CreateMetadataResource(AOwner : TComponent) : TMetadataResource;

begin
  Result:=TMetadataResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAnalyticsAPI.GetProvisioningInstance : TProvisioningResource;

begin
  if (FProvisioningInstance=Nil) then
    FProvisioningInstance:=CreateProvisioningResource;
  Result:=FProvisioningInstance;
end;

Function TAnalyticsAPI.CreateProvisioningResource : TProvisioningResource;

begin
  Result:=CreateProvisioningResource(Self);
end;


Function TAnalyticsAPI.CreateProvisioningResource(AOwner : TComponent) : TProvisioningResource;

begin
  Result:=TProvisioningResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TAnalyticsAPI.RegisterAPI;
end.
