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
//Generated on: 9-5-15 13:22:48
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAccount = class;
  TAccountRef = class;
  TAccountSummaries = class;
  TAccountSummary = class;
  TAccountTicket = class;
  TAccounts = class;
  TAdWordsAccount = class;
  TAnalyticsDataimportDeleteUploadDataRequest = class;
  TColumn = class;
  TColumns = class;
  TCustomDataSource = class;
  TCustomDataSources = class;
  TCustomDimension = class;
  TCustomDimensions = class;
  TCustomMetric = class;
  TCustomMetrics = class;
  TEntityAdWordsLink = class;
  TEntityAdWordsLinks = class;
  TEntityUserLink = class;
  TEntityUserLinks = class;
  TExperiment = class;
  TExperiments = class;
  TFilter = class;
  TFilterExpression = class;
  TFilterRef = class;
  TFilters = class;
  TGaData = class;
  TGoal = class;
  TGoals = class;
  TMcfData = class;
  TProfile = class;
  TProfileFilterLink = class;
  TProfileFilterLinks = class;
  TProfileRef = class;
  TProfileSummary = class;
  TProfiles = class;
  TRealtimeData = class;
  TSegment = class;
  TSegments = class;
  TUnsampledReport = class;
  TUnsampledReports = class;
  TUpload = class;
  TUploads = class;
  TUserRef = class;
  TWebPropertyRef = class;
  TWebPropertySummary = class;
  TWebproperties = class;
  TWebproperty = class;
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
  TAccountTypechildLink = class;
  TAccountTypepermissions = class;
  TColumnTypeattributes = class;
  TCustomDataSourceTypechildLink = class;
  TCustomDataSourceTypeparentLink = class;
  TCustomDimensionTypeparentLink = class;
  TCustomMetricTypeparentLink = class;
  TEntityAdWordsLinkTypeentity = class;
  TEntityUserLinkTypeentity = class;
  TEntityUserLinkTypepermissions = class;
  TExperimentTypeparentLink = class;
  TExperimentTypevariationsItem = class;
  TFilterTypeadvancedDetails = class;
  TFilterTypelowercaseDetails = class;
  TFilterTypeparentLink = class;
  TFilterTypesearchAndReplaceDetails = class;
  TFilterTypeuppercaseDetails = class;
  TGaDataTypecolumnHeadersItem = class;
  TGaDataTypedataTableTypecolsItem = class;
  TGaDataTypedataTableTyperowsItemTypecItem = class;
  TGaDataTypedataTableTyperowsItem = class;
  TGaDataTypedataTable = class;
  TGaDataTypeprofileInfo = class;
  TGaDataTypequery = class;
  TGaDataTypetotalsForAllResults = class;
  TGoalTypeeventDetailsTypeeventConditionsItem = class;
  TGoalTypeeventDetails = class;
  TGoalTypeparentLink = class;
  TGoalTypeurlDestinationDetailsTypestepsItem = class;
  TGoalTypeurlDestinationDetails = class;
  TGoalTypevisitNumPagesDetails = class;
  TGoalTypevisitTimeOnSiteDetails = class;
  TMcfDataTypecolumnHeadersItem = class;
  TMcfDataTypeprofileInfo = class;
  TMcfDataTypequery = class;
  TMcfDataTyperowsItemItemTypeconversionPathValueItem = class;
  TMcfDataTyperowsItemItem = class;
  TMcfDataTypetotalsForAllResults = class;
  TProfileTypechildLink = class;
  TProfileTypeparentLink = class;
  TProfileTypepermissions = class;
  TRealtimeDataTypecolumnHeadersItem = class;
  TRealtimeDataTypeprofileInfo = class;
  TRealtimeDataTypequery = class;
  TRealtimeDataTypetotalsForAllResults = class;
  TUnsampledReportTypecloudStorageDownloadDetails = class;
  TUnsampledReportTypedriveDownloadDetails = class;
  TWebpropertyTypechildLink = class;
  TWebpropertyTypeparentLink = class;
  TWebpropertyTypepermissions = class;
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
    TDataResource
    --------------------------------------------------------------------}
  
  TDataResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
  end;
  
  
  { --------------------------------------------------------------------
    TManagementResource
    --------------------------------------------------------------------}
  
  TManagementResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
  end;
  
  
  { --------------------------------------------------------------------
    TMetadataResource
    --------------------------------------------------------------------}
  
  TMetadataResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
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
    FDataInstance : TDataResource;
    FManagementInstance : TManagementResource;
    FMetadataInstance : TMetadataResource;
    FProvisioningInstance : TProvisioningResource;
    Function GetDataInstance : TDataResource;virtual;
    Function GetManagementInstance : TManagementResource;virtual;
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
    Function CreateDataResource(AOwner : TComponent) : TDataResource;virtual;overload;
    Function CreateDataResource : TDataResource;virtual;overload;
    Function CreateManagementResource(AOwner : TComponent) : TManagementResource;virtual;overload;
    Function CreateManagementResource : TManagementResource;virtual;overload;
    Function CreateMetadataResource(AOwner : TComponent) : TMetadataResource;virtual;overload;
    Function CreateMetadataResource : TMetadataResource;virtual;overload;
    Function CreateProvisioningResource(AOwner : TComponent) : TProvisioningResource;virtual;overload;
    Function CreateProvisioningResource : TProvisioningResource;virtual;overload;
    //Add default on-demand instances for resources
    Property DataResource : TDataResource Read GetDataInstance;
    Property ManagementResource : TManagementResource Read GetManagementInstance;
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
  Result:='https://www.googleapis.com/';
end;

Class Function TAnalyticsAPI.APIbasePath : string;

begin
  Result:='/analytics/v3/';
end;

Class Function TAnalyticsAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/analytics/v3/';
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
end;



initialization
  TAnalyticsAPI.RegisterAPI;
end.
