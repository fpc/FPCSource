unit googleanalytics;
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
  TAccountchildLink = class;
  TAccountchildLinkArray = Array of TAccountchildLink;
  TAccountpermissions = class;
  TAccountpermissionsArray = Array of TAccountpermissions;
  TAccountpermissionseffective = class;
  TAccountpermissionseffectiveArray = Array of TAccountpermissionseffective;
  TAccountRef = class;
  TAccountRefArray = Array of TAccountRef;
  TAccountSummaries = class;
  TAccountSummariesArray = Array of TAccountSummaries;
  TAccountSummariesitems = class;
  TAccountSummariesitemsArray = Array of TAccountSummariesitems;
  TAccountSummary = class;
  TAccountSummaryArray = Array of TAccountSummary;
  TAccountSummarywebProperties = class;
  TAccountSummarywebPropertiesArray = Array of TAccountSummarywebProperties;
  TAccountTicket = class;
  TAccountTicketArray = Array of TAccountTicket;
  TAccounts = class;
  TAccountsArray = Array of TAccounts;
  TAccountsitems = class;
  TAccountsitemsArray = Array of TAccountsitems;
  TAdWordsAccount = class;
  TAdWordsAccountArray = Array of TAdWordsAccount;
  TAnalyticsDataimportDeleteUploadDataRequest = class;
  TAnalyticsDataimportDeleteUploadDataRequestArray = Array of TAnalyticsDataimportDeleteUploadDataRequest;
  TAnalyticsDataimportDeleteUploadDataRequestcustomDataImportUids = class;
  TAnalyticsDataimportDeleteUploadDataRequestcustomDataImportUidsArray = Array of TAnalyticsDataimportDeleteUploadDataRequestcustomDataImportUids;
  TColumn = class;
  TColumnArray = Array of TColumn;
  TColumnattributes = class;
  TColumnattributesArray = Array of TColumnattributes;
  TColumns = class;
  TColumnsArray = Array of TColumns;
  TColumnsattributeNames = class;
  TColumnsattributeNamesArray = Array of TColumnsattributeNames;
  TColumnsitems = class;
  TColumnsitemsArray = Array of TColumnsitems;
  TCustomDataSource = class;
  TCustomDataSourceArray = Array of TCustomDataSource;
  TCustomDataSourcechildLink = class;
  TCustomDataSourcechildLinkArray = Array of TCustomDataSourcechildLink;
  TCustomDataSourceparentLink = class;
  TCustomDataSourceparentLinkArray = Array of TCustomDataSourceparentLink;
  TCustomDataSourceprofilesLinked = class;
  TCustomDataSourceprofilesLinkedArray = Array of TCustomDataSourceprofilesLinked;
  TCustomDataSources = class;
  TCustomDataSourcesArray = Array of TCustomDataSources;
  TCustomDataSourcesitems = class;
  TCustomDataSourcesitemsArray = Array of TCustomDataSourcesitems;
  TCustomDimension = class;
  TCustomDimensionArray = Array of TCustomDimension;
  TCustomDimensionparentLink = class;
  TCustomDimensionparentLinkArray = Array of TCustomDimensionparentLink;
  TCustomDimensions = class;
  TCustomDimensionsArray = Array of TCustomDimensions;
  TCustomDimensionsitems = class;
  TCustomDimensionsitemsArray = Array of TCustomDimensionsitems;
  TCustomMetric = class;
  TCustomMetricArray = Array of TCustomMetric;
  TCustomMetricparentLink = class;
  TCustomMetricparentLinkArray = Array of TCustomMetricparentLink;
  TCustomMetrics = class;
  TCustomMetricsArray = Array of TCustomMetrics;
  TCustomMetricsitems = class;
  TCustomMetricsitemsArray = Array of TCustomMetricsitems;
  TEntityAdWordsLink = class;
  TEntityAdWordsLinkArray = Array of TEntityAdWordsLink;
  TEntityAdWordsLinkadWordsAccounts = class;
  TEntityAdWordsLinkadWordsAccountsArray = Array of TEntityAdWordsLinkadWordsAccounts;
  TEntityAdWordsLinkentity = class;
  TEntityAdWordsLinkentityArray = Array of TEntityAdWordsLinkentity;
  TEntityAdWordsLinkprofileIds = class;
  TEntityAdWordsLinkprofileIdsArray = Array of TEntityAdWordsLinkprofileIds;
  TEntityAdWordsLinks = class;
  TEntityAdWordsLinksArray = Array of TEntityAdWordsLinks;
  TEntityAdWordsLinksitems = class;
  TEntityAdWordsLinksitemsArray = Array of TEntityAdWordsLinksitems;
  TEntityUserLink = class;
  TEntityUserLinkArray = Array of TEntityUserLink;
  TEntityUserLinkentity = class;
  TEntityUserLinkentityArray = Array of TEntityUserLinkentity;
  TEntityUserLinkpermissions = class;
  TEntityUserLinkpermissionsArray = Array of TEntityUserLinkpermissions;
  TEntityUserLinkpermissionseffective = class;
  TEntityUserLinkpermissionseffectiveArray = Array of TEntityUserLinkpermissionseffective;
  TEntityUserLinkpermissionslocal = class;
  TEntityUserLinkpermissionslocalArray = Array of TEntityUserLinkpermissionslocal;
  TEntityUserLinks = class;
  TEntityUserLinksArray = Array of TEntityUserLinks;
  TEntityUserLinksitems = class;
  TEntityUserLinksitemsArray = Array of TEntityUserLinksitems;
  TExperiment = class;
  TExperimentArray = Array of TExperiment;
  TExperimentparentLink = class;
  TExperimentparentLinkArray = Array of TExperimentparentLink;
  TExperimentvariations = class;
  TExperimentvariationsArray = Array of TExperimentvariations;
  TExperiments = class;
  TExperimentsArray = Array of TExperiments;
  TExperimentsitems = class;
  TExperimentsitemsArray = Array of TExperimentsitems;
  TFilter = class;
  TFilterArray = Array of TFilter;
  TFilteradvancedDetails = class;
  TFilteradvancedDetailsArray = Array of TFilteradvancedDetails;
  TFilterlowercaseDetails = class;
  TFilterlowercaseDetailsArray = Array of TFilterlowercaseDetails;
  TFilterparentLink = class;
  TFilterparentLinkArray = Array of TFilterparentLink;
  TFiltersearchAndReplaceDetails = class;
  TFiltersearchAndReplaceDetailsArray = Array of TFiltersearchAndReplaceDetails;
  TFilteruppercaseDetails = class;
  TFilteruppercaseDetailsArray = Array of TFilteruppercaseDetails;
  TFilterExpression = class;
  TFilterExpressionArray = Array of TFilterExpression;
  TFilterRef = class;
  TFilterRefArray = Array of TFilterRef;
  TFilters = class;
  TFiltersArray = Array of TFilters;
  TFiltersitems = class;
  TFiltersitemsArray = Array of TFiltersitems;
  TGaData = class;
  TGaDataArray = Array of TGaData;
  TGaDatacolumnHeaders = class;
  TGaDatacolumnHeadersArray = Array of TGaDatacolumnHeaders;
  TGaDatadataTable = class;
  TGaDatadataTableArray = Array of TGaDatadataTable;
  TGaDatadataTablecols = class;
  TGaDatadataTablecolsArray = Array of TGaDatadataTablecols;
  TGaDatadataTablerows = class;
  TGaDatadataTablerowsArray = Array of TGaDatadataTablerows;
  TGaDatadataTablerowsc = class;
  TGaDatadataTablerowscArray = Array of TGaDatadataTablerowsc;
  TGaDataprofileInfo = class;
  TGaDataprofileInfoArray = Array of TGaDataprofileInfo;
  TGaDataquery = class;
  TGaDataqueryArray = Array of TGaDataquery;
  TGaDataquerymetrics = class;
  TGaDataquerymetricsArray = Array of TGaDataquerymetrics;
  TGaDataquerysort = class;
  TGaDataquerysortArray = Array of TGaDataquerysort;
  TGaDatarows = class;
  TGaDatarowsArray = Array of TGaDatarows;
  TGaDatatotalsForAllResults = class;
  TGaDatatotalsForAllResultsArray = Array of TGaDatatotalsForAllResults;
  TGoal = class;
  TGoalArray = Array of TGoal;
  TGoaleventDetails = class;
  TGoaleventDetailsArray = Array of TGoaleventDetails;
  TGoaleventDetailseventConditions = class;
  TGoaleventDetailseventConditionsArray = Array of TGoaleventDetailseventConditions;
  TGoalparentLink = class;
  TGoalparentLinkArray = Array of TGoalparentLink;
  TGoalurlDestinationDetails = class;
  TGoalurlDestinationDetailsArray = Array of TGoalurlDestinationDetails;
  TGoalurlDestinationDetailssteps = class;
  TGoalurlDestinationDetailsstepsArray = Array of TGoalurlDestinationDetailssteps;
  TGoalvisitNumPagesDetails = class;
  TGoalvisitNumPagesDetailsArray = Array of TGoalvisitNumPagesDetails;
  TGoalvisitTimeOnSiteDetails = class;
  TGoalvisitTimeOnSiteDetailsArray = Array of TGoalvisitTimeOnSiteDetails;
  TGoals = class;
  TGoalsArray = Array of TGoals;
  TGoalsitems = class;
  TGoalsitemsArray = Array of TGoalsitems;
  TMcfData = class;
  TMcfDataArray = Array of TMcfData;
  TMcfDatacolumnHeaders = class;
  TMcfDatacolumnHeadersArray = Array of TMcfDatacolumnHeaders;
  TMcfDataprofileInfo = class;
  TMcfDataprofileInfoArray = Array of TMcfDataprofileInfo;
  TMcfDataquery = class;
  TMcfDataqueryArray = Array of TMcfDataquery;
  TMcfDataquerymetrics = class;
  TMcfDataquerymetricsArray = Array of TMcfDataquerymetrics;
  TMcfDataquerysort = class;
  TMcfDataquerysortArray = Array of TMcfDataquerysort;
  TMcfDatarows = class;
  TMcfDatarowsArray = Array of TMcfDatarows;
  TMcfDatatotalsForAllResults = class;
  TMcfDatatotalsForAllResultsArray = Array of TMcfDatatotalsForAllResults;
  TProfile = class;
  TProfileArray = Array of TProfile;
  TProfilechildLink = class;
  TProfilechildLinkArray = Array of TProfilechildLink;
  TProfileparentLink = class;
  TProfileparentLinkArray = Array of TProfileparentLink;
  TProfilepermissions = class;
  TProfilepermissionsArray = Array of TProfilepermissions;
  TProfilepermissionseffective = class;
  TProfilepermissionseffectiveArray = Array of TProfilepermissionseffective;
  TProfileFilterLink = class;
  TProfileFilterLinkArray = Array of TProfileFilterLink;
  TProfileFilterLinks = class;
  TProfileFilterLinksArray = Array of TProfileFilterLinks;
  TProfileFilterLinksitems = class;
  TProfileFilterLinksitemsArray = Array of TProfileFilterLinksitems;
  TProfileRef = class;
  TProfileRefArray = Array of TProfileRef;
  TProfileSummary = class;
  TProfileSummaryArray = Array of TProfileSummary;
  TProfiles = class;
  TProfilesArray = Array of TProfiles;
  TProfilesitems = class;
  TProfilesitemsArray = Array of TProfilesitems;
  TRealtimeData = class;
  TRealtimeDataArray = Array of TRealtimeData;
  TRealtimeDatacolumnHeaders = class;
  TRealtimeDatacolumnHeadersArray = Array of TRealtimeDatacolumnHeaders;
  TRealtimeDataprofileInfo = class;
  TRealtimeDataprofileInfoArray = Array of TRealtimeDataprofileInfo;
  TRealtimeDataquery = class;
  TRealtimeDataqueryArray = Array of TRealtimeDataquery;
  TRealtimeDataquerymetrics = class;
  TRealtimeDataquerymetricsArray = Array of TRealtimeDataquerymetrics;
  TRealtimeDataquerysort = class;
  TRealtimeDataquerysortArray = Array of TRealtimeDataquerysort;
  TRealtimeDatarows = class;
  TRealtimeDatarowsArray = Array of TRealtimeDatarows;
  TRealtimeDatatotalsForAllResults = class;
  TRealtimeDatatotalsForAllResultsArray = Array of TRealtimeDatatotalsForAllResults;
  TSegment = class;
  TSegmentArray = Array of TSegment;
  TSegments = class;
  TSegmentsArray = Array of TSegments;
  TSegmentsitems = class;
  TSegmentsitemsArray = Array of TSegmentsitems;
  TUnsampledReport = class;
  TUnsampledReportArray = Array of TUnsampledReport;
  TUnsampledReportcloudStorageDownloadDetails = class;
  TUnsampledReportcloudStorageDownloadDetailsArray = Array of TUnsampledReportcloudStorageDownloadDetails;
  TUnsampledReportdriveDownloadDetails = class;
  TUnsampledReportdriveDownloadDetailsArray = Array of TUnsampledReportdriveDownloadDetails;
  TUnsampledReports = class;
  TUnsampledReportsArray = Array of TUnsampledReports;
  TUnsampledReportsitems = class;
  TUnsampledReportsitemsArray = Array of TUnsampledReportsitems;
  TUpload = class;
  TUploadArray = Array of TUpload;
  TUploaderrors = class;
  TUploaderrorsArray = Array of TUploaderrors;
  TUploads = class;
  TUploadsArray = Array of TUploads;
  TUploadsitems = class;
  TUploadsitemsArray = Array of TUploadsitems;
  TUserRef = class;
  TUserRefArray = Array of TUserRef;
  TWebPropertyRef = class;
  TWebPropertyRefArray = Array of TWebPropertyRef;
  TWebPropertySummary = class;
  TWebPropertySummaryArray = Array of TWebPropertySummary;
  TWebPropertySummaryprofiles = class;
  TWebPropertySummaryprofilesArray = Array of TWebPropertySummaryprofiles;
  TWebproperties = class;
  TWebpropertiesArray = Array of TWebproperties;
  TWebpropertiesitems = class;
  TWebpropertiesitemsArray = Array of TWebpropertiesitems;
  TWebproperty = class;
  TWebpropertyArray = Array of TWebproperty;
  TWebpropertychildLink = class;
  TWebpropertychildLinkArray = Array of TWebpropertychildLink;
  TWebpropertyparentLink = class;
  TWebpropertyparentLinkArray = Array of TWebpropertyparentLink;
  TWebpropertypermissions = class;
  TWebpropertypermissionsArray = Array of TWebpropertypermissions;
  TWebpropertypermissionseffective = class;
  TWebpropertypermissionseffectiveArray = Array of TWebpropertypermissionseffective;
  
  { --------------------------------------------------------------------
    TAccount
    --------------------------------------------------------------------}
  
  TAccount = Class(TGoogleBaseObject)
  Private
    FchildLink : TAccountchildLink;
    Fcreated : TDatetime;
    Fid : string;
    Fkind : string;
    Fname : string;
    Fpermissions : TAccountpermissions;
    FselfLink : string;
    Fupdated : TDatetime;
  Protected
    //Property setters
    Procedure SetchildLink(AIndex : Integer; AValue : TAccountchildLink); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setpermissions(AIndex : Integer; AValue : TAccountpermissions); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property childLink : TAccountchildLink Index 0 Read FchildLink Write SetchildLink;
    Property created : TDatetime Index 8 Read Fcreated Write Setcreated;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property name : string Index 32 Read Fname Write Setname;
    Property permissions : TAccountpermissions Index 40 Read Fpermissions Write Setpermissions;
    Property selfLink : string Index 48 Read FselfLink Write SetselfLink;
    Property updated : TDatetime Index 56 Read Fupdated Write Setupdated;
  end;
  TAccountClass = Class of TAccount;
  
  { --------------------------------------------------------------------
    TAccountchildLink
    --------------------------------------------------------------------}
  
  TAccountchildLink = Class(TGoogleBaseObject)
  Private
    Fhref : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property href : string Index 0 Read Fhref Write Sethref;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TAccountchildLinkClass = Class of TAccountchildLink;
  
  { --------------------------------------------------------------------
    TAccountpermissions
    --------------------------------------------------------------------}
  
  TAccountpermissions = Class(TGoogleBaseObject)
  Private
    Feffective : TAccountpermissionseffective;
  Protected
    //Property setters
    Procedure Seteffective(AIndex : Integer; AValue : TAccountpermissionseffective); virtual;
  Public
  Published
    Property effective : TAccountpermissionseffective Index 0 Read Feffective Write Seteffective;
  end;
  TAccountpermissionsClass = Class of TAccountpermissions;
  
  { --------------------------------------------------------------------
    TAccountpermissionseffective
    --------------------------------------------------------------------}
  
  TAccountpermissionseffective = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountpermissionseffectiveClass = Class of TAccountpermissionseffective;
  
  { --------------------------------------------------------------------
    TAccountRef
    --------------------------------------------------------------------}
  
  TAccountRef = Class(TGoogleBaseObject)
  Private
    Fhref : string;
    Fid : string;
    Fkind : string;
    Fname : string;
  Protected
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property href : string Index 0 Read Fhref Write Sethref;
    Property id : string Index 8 Read Fid Write Setid;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property name : string Index 24 Read Fname Write Setname;
  end;
  TAccountRefClass = Class of TAccountRef;
  
  { --------------------------------------------------------------------
    TAccountSummaries
    --------------------------------------------------------------------}
  
  TAccountSummaries = Class(TGoogleBaseObject)
  Private
    Fitems : TAccountSummariesitems;
    FitemsPerPage : integer;
    Fkind : string;
    FnextLink : string;
    FpreviousLink : string;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TAccountSummariesitems); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TAccountSummariesitems Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextLink : string Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : string Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : string Index 56 Read Fusername Write Setusername;
  end;
  TAccountSummariesClass = Class of TAccountSummaries;
  
  { --------------------------------------------------------------------
    TAccountSummariesitems
    --------------------------------------------------------------------}
  
  TAccountSummariesitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountSummariesitemsClass = Class of TAccountSummariesitems;
  
  { --------------------------------------------------------------------
    TAccountSummary
    --------------------------------------------------------------------}
  
  TAccountSummary = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fkind : string;
    Fname : string;
    FwebProperties : TAccountSummarywebProperties;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetwebProperties(AIndex : Integer; AValue : TAccountSummarywebProperties); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property name : string Index 16 Read Fname Write Setname;
    Property webProperties : TAccountSummarywebProperties Index 24 Read FwebProperties Write SetwebProperties;
  end;
  TAccountSummaryClass = Class of TAccountSummary;
  
  { --------------------------------------------------------------------
    TAccountSummarywebProperties
    --------------------------------------------------------------------}
  
  TAccountSummarywebProperties = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountSummarywebPropertiesClass = Class of TAccountSummarywebProperties;
  
  { --------------------------------------------------------------------
    TAccountTicket
    --------------------------------------------------------------------}
  
  TAccountTicket = Class(TGoogleBaseObject)
  Private
    Faccount : TAccount;
    Fid : string;
    Fkind : string;
    Fprofile : TProfile;
    FredirectUri : string;
    Fwebproperty : TWebproperty;
  Protected
    //Property setters
    Procedure Setaccount(AIndex : Integer; AValue : TAccount); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setprofile(AIndex : Integer; AValue : TProfile); virtual;
    Procedure SetredirectUri(AIndex : Integer; AValue : string); virtual;
    Procedure Setwebproperty(AIndex : Integer; AValue : TWebproperty); virtual;
  Public
  Published
    Property account : TAccount Index 0 Read Faccount Write Setaccount;
    Property id : string Index 8 Read Fid Write Setid;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property profile : TProfile Index 24 Read Fprofile Write Setprofile;
    Property redirectUri : string Index 32 Read FredirectUri Write SetredirectUri;
    Property webproperty : TWebproperty Index 40 Read Fwebproperty Write Setwebproperty;
  end;
  TAccountTicketClass = Class of TAccountTicket;
  
  { --------------------------------------------------------------------
    TAccounts
    --------------------------------------------------------------------}
  
  TAccounts = Class(TGoogleBaseObject)
  Private
    Fitems : TAccountsitems;
    FitemsPerPage : integer;
    Fkind : string;
    FnextLink : string;
    FpreviousLink : string;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TAccountsitems); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TAccountsitems Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextLink : string Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : string Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : string Index 56 Read Fusername Write Setusername;
  end;
  TAccountsClass = Class of TAccounts;
  
  { --------------------------------------------------------------------
    TAccountsitems
    --------------------------------------------------------------------}
  
  TAccountsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountsitemsClass = Class of TAccountsitems;
  
  { --------------------------------------------------------------------
    TAdWordsAccount
    --------------------------------------------------------------------}
  
  TAdWordsAccount = Class(TGoogleBaseObject)
  Private
    FautoTaggingEnabled : boolean;
    FcustomerId : string;
    Fkind : string;
  Protected
    //Property setters
    Procedure SetautoTaggingEnabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcustomerId(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property autoTaggingEnabled : boolean Index 0 Read FautoTaggingEnabled Write SetautoTaggingEnabled;
    Property customerId : string Index 8 Read FcustomerId Write SetcustomerId;
    Property kind : string Index 16 Read Fkind Write Setkind;
  end;
  TAdWordsAccountClass = Class of TAdWordsAccount;
  
  { --------------------------------------------------------------------
    TAnalyticsDataimportDeleteUploadDataRequest
    --------------------------------------------------------------------}
  
  TAnalyticsDataimportDeleteUploadDataRequest = Class(TGoogleBaseObject)
  Private
    FcustomDataImportUids : TAnalyticsDataimportDeleteUploadDataRequestcustomDataImportUids;
  Protected
    //Property setters
    Procedure SetcustomDataImportUids(AIndex : Integer; AValue : TAnalyticsDataimportDeleteUploadDataRequestcustomDataImportUids); virtual;
  Public
  Published
    Property customDataImportUids : TAnalyticsDataimportDeleteUploadDataRequestcustomDataImportUids Index 0 Read FcustomDataImportUids Write SetcustomDataImportUids;
  end;
  TAnalyticsDataimportDeleteUploadDataRequestClass = Class of TAnalyticsDataimportDeleteUploadDataRequest;
  
  { --------------------------------------------------------------------
    TAnalyticsDataimportDeleteUploadDataRequestcustomDataImportUids
    --------------------------------------------------------------------}
  
  TAnalyticsDataimportDeleteUploadDataRequestcustomDataImportUids = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAnalyticsDataimportDeleteUploadDataRequestcustomDataImportUidsClass = Class of TAnalyticsDataimportDeleteUploadDataRequestcustomDataImportUids;
  
  { --------------------------------------------------------------------
    TColumn
    --------------------------------------------------------------------}
  
  TColumn = Class(TGoogleBaseObject)
  Private
    Fattributes : TColumnattributes;
    Fid : string;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setattributes(AIndex : Integer; AValue : TColumnattributes); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property attributes : TColumnattributes Index 0 Read Fattributes Write Setattributes;
    Property id : string Index 8 Read Fid Write Setid;
    Property kind : string Index 16 Read Fkind Write Setkind;
  end;
  TColumnClass = Class of TColumn;
  
  { --------------------------------------------------------------------
    TColumnattributes
    --------------------------------------------------------------------}
  
  TColumnattributes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TColumnattributesClass = Class of TColumnattributes;
  
  { --------------------------------------------------------------------
    TColumns
    --------------------------------------------------------------------}
  
  TColumns = Class(TGoogleBaseObject)
  Private
    FattributeNames : TColumnsattributeNames;
    Fetag : string;
    Fitems : TColumnsitems;
    Fkind : string;
    FtotalResults : integer;
  Protected
    //Property setters
    Procedure SetattributeNames(AIndex : Integer; AValue : TColumnsattributeNames); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TColumnsitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property attributeNames : TColumnsattributeNames Index 0 Read FattributeNames Write SetattributeNames;
    Property etag : string Index 8 Read Fetag Write Setetag;
    Property items : TColumnsitems Index 16 Read Fitems Write Setitems;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property totalResults : integer Index 32 Read FtotalResults Write SettotalResults;
  end;
  TColumnsClass = Class of TColumns;
  
  { --------------------------------------------------------------------
    TColumnsattributeNames
    --------------------------------------------------------------------}
  
  TColumnsattributeNames = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TColumnsattributeNamesClass = Class of TColumnsattributeNames;
  
  { --------------------------------------------------------------------
    TColumnsitems
    --------------------------------------------------------------------}
  
  TColumnsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TColumnsitemsClass = Class of TColumnsitems;
  
  { --------------------------------------------------------------------
    TCustomDataSource
    --------------------------------------------------------------------}
  
  TCustomDataSource = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    FchildLink : TCustomDataSourcechildLink;
    Fcreated : TDatetime;
    Fdescription : string;
    Fid : string;
    FimportBehavior : string;
    Fkind : string;
    Fname : string;
    FparentLink : TCustomDataSourceparentLink;
    FprofilesLinked : TCustomDataSourceprofilesLinked;
    FselfLink : string;
    F_type : string;
    Fupdated : TDatetime;
    FuploadType : string;
    FwebPropertyId : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetchildLink(AIndex : Integer; AValue : TCustomDataSourcechildLink); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetimportBehavior(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetparentLink(AIndex : Integer; AValue : TCustomDataSourceparentLink); virtual;
    Procedure SetprofilesLinked(AIndex : Integer; AValue : TCustomDataSourceprofilesLinked); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetuploadType(AIndex : Integer; AValue : string); virtual;
    Procedure SetwebPropertyId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property childLink : TCustomDataSourcechildLink Index 8 Read FchildLink Write SetchildLink;
    Property created : TDatetime Index 16 Read Fcreated Write Setcreated;
    Property description : string Index 24 Read Fdescription Write Setdescription;
    Property id : string Index 32 Read Fid Write Setid;
    Property importBehavior : string Index 40 Read FimportBehavior Write SetimportBehavior;
    Property kind : string Index 48 Read Fkind Write Setkind;
    Property name : string Index 56 Read Fname Write Setname;
    Property parentLink : TCustomDataSourceparentLink Index 64 Read FparentLink Write SetparentLink;
    Property profilesLinked : TCustomDataSourceprofilesLinked Index 72 Read FprofilesLinked Write SetprofilesLinked;
    Property selfLink : string Index 80 Read FselfLink Write SetselfLink;
    Property _type : string Index 88 Read F_type Write Set_type;
    Property updated : TDatetime Index 96 Read Fupdated Write Setupdated;
    Property uploadType : string Index 104 Read FuploadType Write SetuploadType;
    Property webPropertyId : string Index 112 Read FwebPropertyId Write SetwebPropertyId;
  end;
  TCustomDataSourceClass = Class of TCustomDataSource;
  
  { --------------------------------------------------------------------
    TCustomDataSourcechildLink
    --------------------------------------------------------------------}
  
  TCustomDataSourcechildLink = Class(TGoogleBaseObject)
  Private
    Fhref : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property href : string Index 0 Read Fhref Write Sethref;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TCustomDataSourcechildLinkClass = Class of TCustomDataSourcechildLink;
  
  { --------------------------------------------------------------------
    TCustomDataSourceparentLink
    --------------------------------------------------------------------}
  
  TCustomDataSourceparentLink = Class(TGoogleBaseObject)
  Private
    Fhref : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property href : string Index 0 Read Fhref Write Sethref;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TCustomDataSourceparentLinkClass = Class of TCustomDataSourceparentLink;
  
  { --------------------------------------------------------------------
    TCustomDataSourceprofilesLinked
    --------------------------------------------------------------------}
  
  TCustomDataSourceprofilesLinked = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCustomDataSourceprofilesLinkedClass = Class of TCustomDataSourceprofilesLinked;
  
  { --------------------------------------------------------------------
    TCustomDataSources
    --------------------------------------------------------------------}
  
  TCustomDataSources = Class(TGoogleBaseObject)
  Private
    Fitems : TCustomDataSourcesitems;
    FitemsPerPage : integer;
    Fkind : string;
    FnextLink : string;
    FpreviousLink : string;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TCustomDataSourcesitems); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TCustomDataSourcesitems Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextLink : string Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : string Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : string Index 56 Read Fusername Write Setusername;
  end;
  TCustomDataSourcesClass = Class of TCustomDataSources;
  
  { --------------------------------------------------------------------
    TCustomDataSourcesitems
    --------------------------------------------------------------------}
  
  TCustomDataSourcesitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCustomDataSourcesitemsClass = Class of TCustomDataSourcesitems;
  
  { --------------------------------------------------------------------
    TCustomDimension
    --------------------------------------------------------------------}
  
  TCustomDimension = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    Factive : boolean;
    Fcreated : TDatetime;
    Fid : string;
    Findex : integer;
    Fkind : string;
    Fname : string;
    FparentLink : TCustomDimensionparentLink;
    Fscope : string;
    FselfLink : string;
    Fupdated : TDatetime;
    FwebPropertyId : string;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure Setactive(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setindex(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetparentLink(AIndex : Integer; AValue : TCustomDimensionparentLink); virtual;
    Procedure Setscope(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetwebPropertyId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property active : boolean Index 8 Read Factive Write Setactive;
    Property created : TDatetime Index 16 Read Fcreated Write Setcreated;
    Property id : string Index 24 Read Fid Write Setid;
    Property index : integer Index 32 Read Findex Write Setindex;
    Property kind : string Index 40 Read Fkind Write Setkind;
    Property name : string Index 48 Read Fname Write Setname;
    Property parentLink : TCustomDimensionparentLink Index 56 Read FparentLink Write SetparentLink;
    Property scope : string Index 64 Read Fscope Write Setscope;
    Property selfLink : string Index 72 Read FselfLink Write SetselfLink;
    Property updated : TDatetime Index 80 Read Fupdated Write Setupdated;
    Property webPropertyId : string Index 88 Read FwebPropertyId Write SetwebPropertyId;
  end;
  TCustomDimensionClass = Class of TCustomDimension;
  
  { --------------------------------------------------------------------
    TCustomDimensionparentLink
    --------------------------------------------------------------------}
  
  TCustomDimensionparentLink = Class(TGoogleBaseObject)
  Private
    Fhref : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property href : string Index 0 Read Fhref Write Sethref;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TCustomDimensionparentLinkClass = Class of TCustomDimensionparentLink;
  
  { --------------------------------------------------------------------
    TCustomDimensions
    --------------------------------------------------------------------}
  
  TCustomDimensions = Class(TGoogleBaseObject)
  Private
    Fitems : TCustomDimensionsitems;
    FitemsPerPage : integer;
    Fkind : string;
    FnextLink : string;
    FpreviousLink : string;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TCustomDimensionsitems); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TCustomDimensionsitems Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextLink : string Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : string Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : string Index 56 Read Fusername Write Setusername;
  end;
  TCustomDimensionsClass = Class of TCustomDimensions;
  
  { --------------------------------------------------------------------
    TCustomDimensionsitems
    --------------------------------------------------------------------}
  
  TCustomDimensionsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCustomDimensionsitemsClass = Class of TCustomDimensionsitems;
  
  { --------------------------------------------------------------------
    TCustomMetric
    --------------------------------------------------------------------}
  
  TCustomMetric = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    Factive : boolean;
    Fcreated : TDatetime;
    Fid : string;
    Findex : integer;
    Fkind : string;
    Fmax_value : string;
    Fmin_value : string;
    Fname : string;
    FparentLink : TCustomMetricparentLink;
    Fscope : string;
    FselfLink : string;
    F_type : string;
    Fupdated : TDatetime;
    FwebPropertyId : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure Setactive(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setindex(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setmax_value(AIndex : Integer; AValue : string); virtual;
    Procedure Setmin_value(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetparentLink(AIndex : Integer; AValue : TCustomMetricparentLink); virtual;
    Procedure Setscope(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetwebPropertyId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property active : boolean Index 8 Read Factive Write Setactive;
    Property created : TDatetime Index 16 Read Fcreated Write Setcreated;
    Property id : string Index 24 Read Fid Write Setid;
    Property index : integer Index 32 Read Findex Write Setindex;
    Property kind : string Index 40 Read Fkind Write Setkind;
    Property max_value : string Index 48 Read Fmax_value Write Setmax_value;
    Property min_value : string Index 56 Read Fmin_value Write Setmin_value;
    Property name : string Index 64 Read Fname Write Setname;
    Property parentLink : TCustomMetricparentLink Index 72 Read FparentLink Write SetparentLink;
    Property scope : string Index 80 Read Fscope Write Setscope;
    Property selfLink : string Index 88 Read FselfLink Write SetselfLink;
    Property _type : string Index 96 Read F_type Write Set_type;
    Property updated : TDatetime Index 104 Read Fupdated Write Setupdated;
    Property webPropertyId : string Index 112 Read FwebPropertyId Write SetwebPropertyId;
  end;
  TCustomMetricClass = Class of TCustomMetric;
  
  { --------------------------------------------------------------------
    TCustomMetricparentLink
    --------------------------------------------------------------------}
  
  TCustomMetricparentLink = Class(TGoogleBaseObject)
  Private
    Fhref : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property href : string Index 0 Read Fhref Write Sethref;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TCustomMetricparentLinkClass = Class of TCustomMetricparentLink;
  
  { --------------------------------------------------------------------
    TCustomMetrics
    --------------------------------------------------------------------}
  
  TCustomMetrics = Class(TGoogleBaseObject)
  Private
    Fitems : TCustomMetricsitems;
    FitemsPerPage : integer;
    Fkind : string;
    FnextLink : string;
    FpreviousLink : string;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TCustomMetricsitems); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TCustomMetricsitems Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextLink : string Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : string Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : string Index 56 Read Fusername Write Setusername;
  end;
  TCustomMetricsClass = Class of TCustomMetrics;
  
  { --------------------------------------------------------------------
    TCustomMetricsitems
    --------------------------------------------------------------------}
  
  TCustomMetricsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCustomMetricsitemsClass = Class of TCustomMetricsitems;
  
  { --------------------------------------------------------------------
    TEntityAdWordsLink
    --------------------------------------------------------------------}
  
  TEntityAdWordsLink = Class(TGoogleBaseObject)
  Private
    FadWordsAccounts : TEntityAdWordsLinkadWordsAccounts;
    Fentity : TEntityAdWordsLinkentity;
    Fid : string;
    Fkind : string;
    Fname : string;
    FprofileIds : TEntityAdWordsLinkprofileIds;
    FselfLink : string;
  Protected
    //Property setters
    Procedure SetadWordsAccounts(AIndex : Integer; AValue : TEntityAdWordsLinkadWordsAccounts); virtual;
    Procedure Setentity(AIndex : Integer; AValue : TEntityAdWordsLinkentity); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetprofileIds(AIndex : Integer; AValue : TEntityAdWordsLinkprofileIds); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property adWordsAccounts : TEntityAdWordsLinkadWordsAccounts Index 0 Read FadWordsAccounts Write SetadWordsAccounts;
    Property entity : TEntityAdWordsLinkentity Index 8 Read Fentity Write Setentity;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property name : string Index 32 Read Fname Write Setname;
    Property profileIds : TEntityAdWordsLinkprofileIds Index 40 Read FprofileIds Write SetprofileIds;
    Property selfLink : string Index 48 Read FselfLink Write SetselfLink;
  end;
  TEntityAdWordsLinkClass = Class of TEntityAdWordsLink;
  
  { --------------------------------------------------------------------
    TEntityAdWordsLinkadWordsAccounts
    --------------------------------------------------------------------}
  
  TEntityAdWordsLinkadWordsAccounts = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEntityAdWordsLinkadWordsAccountsClass = Class of TEntityAdWordsLinkadWordsAccounts;
  
  { --------------------------------------------------------------------
    TEntityAdWordsLinkentity
    --------------------------------------------------------------------}
  
  TEntityAdWordsLinkentity = Class(TGoogleBaseObject)
  Private
    FwebPropertyRef : TWebPropertyRef;
  Protected
    //Property setters
    Procedure SetwebPropertyRef(AIndex : Integer; AValue : TWebPropertyRef); virtual;
  Public
  Published
    Property webPropertyRef : TWebPropertyRef Index 0 Read FwebPropertyRef Write SetwebPropertyRef;
  end;
  TEntityAdWordsLinkentityClass = Class of TEntityAdWordsLinkentity;
  
  { --------------------------------------------------------------------
    TEntityAdWordsLinkprofileIds
    --------------------------------------------------------------------}
  
  TEntityAdWordsLinkprofileIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEntityAdWordsLinkprofileIdsClass = Class of TEntityAdWordsLinkprofileIds;
  
  { --------------------------------------------------------------------
    TEntityAdWordsLinks
    --------------------------------------------------------------------}
  
  TEntityAdWordsLinks = Class(TGoogleBaseObject)
  Private
    Fitems : TEntityAdWordsLinksitems;
    FitemsPerPage : integer;
    Fkind : string;
    FnextLink : string;
    FpreviousLink : string;
    FstartIndex : integer;
    FtotalResults : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TEntityAdWordsLinksitems); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property items : TEntityAdWordsLinksitems Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextLink : string Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : string Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
  end;
  TEntityAdWordsLinksClass = Class of TEntityAdWordsLinks;
  
  { --------------------------------------------------------------------
    TEntityAdWordsLinksitems
    --------------------------------------------------------------------}
  
  TEntityAdWordsLinksitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEntityAdWordsLinksitemsClass = Class of TEntityAdWordsLinksitems;
  
  { --------------------------------------------------------------------
    TEntityUserLink
    --------------------------------------------------------------------}
  
  TEntityUserLink = Class(TGoogleBaseObject)
  Private
    Fentity : TEntityUserLinkentity;
    Fid : string;
    Fkind : string;
    Fpermissions : TEntityUserLinkpermissions;
    FselfLink : string;
    FuserRef : TUserRef;
  Protected
    //Property setters
    Procedure Setentity(AIndex : Integer; AValue : TEntityUserLinkentity); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setpermissions(AIndex : Integer; AValue : TEntityUserLinkpermissions); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetuserRef(AIndex : Integer; AValue : TUserRef); virtual;
  Public
  Published
    Property entity : TEntityUserLinkentity Index 0 Read Fentity Write Setentity;
    Property id : string Index 8 Read Fid Write Setid;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property permissions : TEntityUserLinkpermissions Index 24 Read Fpermissions Write Setpermissions;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
    Property userRef : TUserRef Index 40 Read FuserRef Write SetuserRef;
  end;
  TEntityUserLinkClass = Class of TEntityUserLink;
  
  { --------------------------------------------------------------------
    TEntityUserLinkentity
    --------------------------------------------------------------------}
  
  TEntityUserLinkentity = Class(TGoogleBaseObject)
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
  TEntityUserLinkentityClass = Class of TEntityUserLinkentity;
  
  { --------------------------------------------------------------------
    TEntityUserLinkpermissions
    --------------------------------------------------------------------}
  
  TEntityUserLinkpermissions = Class(TGoogleBaseObject)
  Private
    Feffective : TEntityUserLinkpermissionseffective;
    Flocal : TEntityUserLinkpermissionslocal;
  Protected
    //Property setters
    Procedure Seteffective(AIndex : Integer; AValue : TEntityUserLinkpermissionseffective); virtual;
    Procedure Setlocal(AIndex : Integer; AValue : TEntityUserLinkpermissionslocal); virtual;
  Public
  Published
    Property effective : TEntityUserLinkpermissionseffective Index 0 Read Feffective Write Seteffective;
    Property local : TEntityUserLinkpermissionslocal Index 8 Read Flocal Write Setlocal;
  end;
  TEntityUserLinkpermissionsClass = Class of TEntityUserLinkpermissions;
  
  { --------------------------------------------------------------------
    TEntityUserLinkpermissionseffective
    --------------------------------------------------------------------}
  
  TEntityUserLinkpermissionseffective = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEntityUserLinkpermissionseffectiveClass = Class of TEntityUserLinkpermissionseffective;
  
  { --------------------------------------------------------------------
    TEntityUserLinkpermissionslocal
    --------------------------------------------------------------------}
  
  TEntityUserLinkpermissionslocal = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEntityUserLinkpermissionslocalClass = Class of TEntityUserLinkpermissionslocal;
  
  { --------------------------------------------------------------------
    TEntityUserLinks
    --------------------------------------------------------------------}
  
  TEntityUserLinks = Class(TGoogleBaseObject)
  Private
    Fitems : TEntityUserLinksitems;
    FitemsPerPage : integer;
    Fkind : string;
    FnextLink : string;
    FpreviousLink : string;
    FstartIndex : integer;
    FtotalResults : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TEntityUserLinksitems); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property items : TEntityUserLinksitems Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextLink : string Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : string Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
  end;
  TEntityUserLinksClass = Class of TEntityUserLinks;
  
  { --------------------------------------------------------------------
    TEntityUserLinksitems
    --------------------------------------------------------------------}
  
  TEntityUserLinksitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEntityUserLinksitemsClass = Class of TEntityUserLinksitems;
  
  { --------------------------------------------------------------------
    TExperiment
    --------------------------------------------------------------------}
  
  TExperiment = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    Fcreated : TDatetime;
    Fdescription : string;
    FeditableInGaUi : boolean;
    FendTime : TDatetime;
    FequalWeighting : boolean;
    Fid : string;
    FinternalWebPropertyId : string;
    Fkind : string;
    FminimumExperimentLengthInDays : integer;
    Fname : string;
    FobjectiveMetric : string;
    FoptimizationType : string;
    FparentLink : TExperimentparentLink;
    FprofileId : string;
    FreasonExperimentEnded : string;
    FrewriteVariationUrlsAsOriginal : boolean;
    FselfLink : string;
    FservingFramework : string;
    Fsnippet : string;
    FstartTime : TDatetime;
    Fstatus : string;
    FtrafficCoverage : double;
    Fupdated : TDatetime;
    Fvariations : TExperimentvariations;
    FwebPropertyId : string;
    FwinnerConfidenceLevel : double;
    FwinnerFound : boolean;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SeteditableInGaUi(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetendTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetequalWeighting(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetinternalWebPropertyId(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetminimumExperimentLengthInDays(AIndex : Integer; AValue : integer); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetobjectiveMetric(AIndex : Integer; AValue : string); virtual;
    Procedure SetoptimizationType(AIndex : Integer; AValue : string); virtual;
    Procedure SetparentLink(AIndex : Integer; AValue : TExperimentparentLink); virtual;
    Procedure SetprofileId(AIndex : Integer; AValue : string); virtual;
    Procedure SetreasonExperimentEnded(AIndex : Integer; AValue : string); virtual;
    Procedure SetrewriteVariationUrlsAsOriginal(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetservingFramework(AIndex : Integer; AValue : string); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure SettrafficCoverage(AIndex : Integer; AValue : double); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setvariations(AIndex : Integer; AValue : TExperimentvariations); virtual;
    Procedure SetwebPropertyId(AIndex : Integer; AValue : string); virtual;
    Procedure SetwinnerConfidenceLevel(AIndex : Integer; AValue : double); virtual;
    Procedure SetwinnerFound(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property created : TDatetime Index 8 Read Fcreated Write Setcreated;
    Property description : string Index 16 Read Fdescription Write Setdescription;
    Property editableInGaUi : boolean Index 24 Read FeditableInGaUi Write SeteditableInGaUi;
    Property endTime : TDatetime Index 32 Read FendTime Write SetendTime;
    Property equalWeighting : boolean Index 40 Read FequalWeighting Write SetequalWeighting;
    Property id : string Index 48 Read Fid Write Setid;
    Property internalWebPropertyId : string Index 56 Read FinternalWebPropertyId Write SetinternalWebPropertyId;
    Property kind : string Index 64 Read Fkind Write Setkind;
    Property minimumExperimentLengthInDays : integer Index 72 Read FminimumExperimentLengthInDays Write SetminimumExperimentLengthInDays;
    Property name : string Index 80 Read Fname Write Setname;
    Property objectiveMetric : string Index 88 Read FobjectiveMetric Write SetobjectiveMetric;
    Property optimizationType : string Index 96 Read FoptimizationType Write SetoptimizationType;
    Property parentLink : TExperimentparentLink Index 104 Read FparentLink Write SetparentLink;
    Property profileId : string Index 112 Read FprofileId Write SetprofileId;
    Property reasonExperimentEnded : string Index 120 Read FreasonExperimentEnded Write SetreasonExperimentEnded;
    Property rewriteVariationUrlsAsOriginal : boolean Index 128 Read FrewriteVariationUrlsAsOriginal Write SetrewriteVariationUrlsAsOriginal;
    Property selfLink : string Index 136 Read FselfLink Write SetselfLink;
    Property servingFramework : string Index 144 Read FservingFramework Write SetservingFramework;
    Property snippet : string Index 152 Read Fsnippet Write Setsnippet;
    Property startTime : TDatetime Index 160 Read FstartTime Write SetstartTime;
    Property status : string Index 168 Read Fstatus Write Setstatus;
    Property trafficCoverage : double Index 176 Read FtrafficCoverage Write SettrafficCoverage;
    Property updated : TDatetime Index 184 Read Fupdated Write Setupdated;
    Property variations : TExperimentvariations Index 192 Read Fvariations Write Setvariations;
    Property webPropertyId : string Index 200 Read FwebPropertyId Write SetwebPropertyId;
    Property winnerConfidenceLevel : double Index 208 Read FwinnerConfidenceLevel Write SetwinnerConfidenceLevel;
    Property winnerFound : boolean Index 216 Read FwinnerFound Write SetwinnerFound;
  end;
  TExperimentClass = Class of TExperiment;
  
  { --------------------------------------------------------------------
    TExperimentparentLink
    --------------------------------------------------------------------}
  
  TExperimentparentLink = Class(TGoogleBaseObject)
  Private
    Fhref : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property href : string Index 0 Read Fhref Write Sethref;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TExperimentparentLinkClass = Class of TExperimentparentLink;
  
  { --------------------------------------------------------------------
    TExperimentvariations
    --------------------------------------------------------------------}
  
  TExperimentvariations = Class(TGoogleBaseObject)
  Private
    Fname : string;
    Fstatus : string;
    Furl : string;
    Fweight : double;
    Fwon : boolean;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
    Procedure Setweight(AIndex : Integer; AValue : double); virtual;
    Procedure Setwon(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
    Property status : string Index 8 Read Fstatus Write Setstatus;
    Property url : string Index 16 Read Furl Write Seturl;
    Property weight : double Index 24 Read Fweight Write Setweight;
    Property won : boolean Index 32 Read Fwon Write Setwon;
  end;
  TExperimentvariationsClass = Class of TExperimentvariations;
  
  { --------------------------------------------------------------------
    TExperiments
    --------------------------------------------------------------------}
  
  TExperiments = Class(TGoogleBaseObject)
  Private
    Fitems : TExperimentsitems;
    FitemsPerPage : integer;
    Fkind : string;
    FnextLink : string;
    FpreviousLink : string;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TExperimentsitems); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TExperimentsitems Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextLink : string Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : string Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : string Index 56 Read Fusername Write Setusername;
  end;
  TExperimentsClass = Class of TExperiments;
  
  { --------------------------------------------------------------------
    TExperimentsitems
    --------------------------------------------------------------------}
  
  TExperimentsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TExperimentsitemsClass = Class of TExperimentsitems;
  
  { --------------------------------------------------------------------
    TFilter
    --------------------------------------------------------------------}
  
  TFilter = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    FadvancedDetails : TFilteradvancedDetails;
    Fcreated : TDatetime;
    FexcludeDetails : TFilterExpression;
    Fid : string;
    FincludeDetails : TFilterExpression;
    Fkind : string;
    FlowercaseDetails : TFilterlowercaseDetails;
    Fname : string;
    FparentLink : TFilterparentLink;
    FsearchAndReplaceDetails : TFiltersearchAndReplaceDetails;
    FselfLink : string;
    F_type : string;
    Fupdated : TDatetime;
    FuppercaseDetails : TFilteruppercaseDetails;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetadvancedDetails(AIndex : Integer; AValue : TFilteradvancedDetails); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetexcludeDetails(AIndex : Integer; AValue : TFilterExpression); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetincludeDetails(AIndex : Integer; AValue : TFilterExpression); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetlowercaseDetails(AIndex : Integer; AValue : TFilterlowercaseDetails); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetparentLink(AIndex : Integer; AValue : TFilterparentLink); virtual;
    Procedure SetsearchAndReplaceDetails(AIndex : Integer; AValue : TFiltersearchAndReplaceDetails); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetuppercaseDetails(AIndex : Integer; AValue : TFilteruppercaseDetails); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property advancedDetails : TFilteradvancedDetails Index 8 Read FadvancedDetails Write SetadvancedDetails;
    Property created : TDatetime Index 16 Read Fcreated Write Setcreated;
    Property excludeDetails : TFilterExpression Index 24 Read FexcludeDetails Write SetexcludeDetails;
    Property id : string Index 32 Read Fid Write Setid;
    Property includeDetails : TFilterExpression Index 40 Read FincludeDetails Write SetincludeDetails;
    Property kind : string Index 48 Read Fkind Write Setkind;
    Property lowercaseDetails : TFilterlowercaseDetails Index 56 Read FlowercaseDetails Write SetlowercaseDetails;
    Property name : string Index 64 Read Fname Write Setname;
    Property parentLink : TFilterparentLink Index 72 Read FparentLink Write SetparentLink;
    Property searchAndReplaceDetails : TFiltersearchAndReplaceDetails Index 80 Read FsearchAndReplaceDetails Write SetsearchAndReplaceDetails;
    Property selfLink : string Index 88 Read FselfLink Write SetselfLink;
    Property _type : string Index 96 Read F_type Write Set_type;
    Property updated : TDatetime Index 104 Read Fupdated Write Setupdated;
    Property uppercaseDetails : TFilteruppercaseDetails Index 112 Read FuppercaseDetails Write SetuppercaseDetails;
  end;
  TFilterClass = Class of TFilter;
  
  { --------------------------------------------------------------------
    TFilteradvancedDetails
    --------------------------------------------------------------------}
  
  TFilteradvancedDetails = Class(TGoogleBaseObject)
  Private
    FcaseSensitive : boolean;
    FextractA : string;
    FextractB : string;
    FfieldA : string;
    FfieldARequired : boolean;
    FfieldB : string;
    FfieldBRequired : boolean;
    FoutputConstructor : string;
    FoutputToField : string;
    FoverrideOutputField : boolean;
  Protected
    //Property setters
    Procedure SetcaseSensitive(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetextractA(AIndex : Integer; AValue : string); virtual;
    Procedure SetextractB(AIndex : Integer; AValue : string); virtual;
    Procedure SetfieldA(AIndex : Integer; AValue : string); virtual;
    Procedure SetfieldARequired(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetfieldB(AIndex : Integer; AValue : string); virtual;
    Procedure SetfieldBRequired(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetoutputConstructor(AIndex : Integer; AValue : string); virtual;
    Procedure SetoutputToField(AIndex : Integer; AValue : string); virtual;
    Procedure SetoverrideOutputField(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property caseSensitive : boolean Index 0 Read FcaseSensitive Write SetcaseSensitive;
    Property extractA : string Index 8 Read FextractA Write SetextractA;
    Property extractB : string Index 16 Read FextractB Write SetextractB;
    Property fieldA : string Index 24 Read FfieldA Write SetfieldA;
    Property fieldARequired : boolean Index 32 Read FfieldARequired Write SetfieldARequired;
    Property fieldB : string Index 40 Read FfieldB Write SetfieldB;
    Property fieldBRequired : boolean Index 48 Read FfieldBRequired Write SetfieldBRequired;
    Property outputConstructor : string Index 56 Read FoutputConstructor Write SetoutputConstructor;
    Property outputToField : string Index 64 Read FoutputToField Write SetoutputToField;
    Property overrideOutputField : boolean Index 72 Read FoverrideOutputField Write SetoverrideOutputField;
  end;
  TFilteradvancedDetailsClass = Class of TFilteradvancedDetails;
  
  { --------------------------------------------------------------------
    TFilterlowercaseDetails
    --------------------------------------------------------------------}
  
  TFilterlowercaseDetails = Class(TGoogleBaseObject)
  Private
    Ffield : string;
  Protected
    //Property setters
    Procedure Setfield(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property field : string Index 0 Read Ffield Write Setfield;
  end;
  TFilterlowercaseDetailsClass = Class of TFilterlowercaseDetails;
  
  { --------------------------------------------------------------------
    TFilterparentLink
    --------------------------------------------------------------------}
  
  TFilterparentLink = Class(TGoogleBaseObject)
  Private
    Fhref : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property href : string Index 0 Read Fhref Write Sethref;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TFilterparentLinkClass = Class of TFilterparentLink;
  
  { --------------------------------------------------------------------
    TFiltersearchAndReplaceDetails
    --------------------------------------------------------------------}
  
  TFiltersearchAndReplaceDetails = Class(TGoogleBaseObject)
  Private
    FcaseSensitive : boolean;
    Ffield : string;
    FreplaceString : string;
    FsearchString : string;
  Protected
    //Property setters
    Procedure SetcaseSensitive(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setfield(AIndex : Integer; AValue : string); virtual;
    Procedure SetreplaceString(AIndex : Integer; AValue : string); virtual;
    Procedure SetsearchString(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property caseSensitive : boolean Index 0 Read FcaseSensitive Write SetcaseSensitive;
    Property field : string Index 8 Read Ffield Write Setfield;
    Property replaceString : string Index 16 Read FreplaceString Write SetreplaceString;
    Property searchString : string Index 24 Read FsearchString Write SetsearchString;
  end;
  TFiltersearchAndReplaceDetailsClass = Class of TFiltersearchAndReplaceDetails;
  
  { --------------------------------------------------------------------
    TFilteruppercaseDetails
    --------------------------------------------------------------------}
  
  TFilteruppercaseDetails = Class(TGoogleBaseObject)
  Private
    Ffield : string;
  Protected
    //Property setters
    Procedure Setfield(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property field : string Index 0 Read Ffield Write Setfield;
  end;
  TFilteruppercaseDetailsClass = Class of TFilteruppercaseDetails;
  
  { --------------------------------------------------------------------
    TFilterExpression
    --------------------------------------------------------------------}
  
  TFilterExpression = Class(TGoogleBaseObject)
  Private
    FcaseSensitive : boolean;
    FexpressionValue : string;
    Ffield : string;
    Fkind : string;
    FmatchType : string;
  Protected
    //Property setters
    Procedure SetcaseSensitive(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetexpressionValue(AIndex : Integer; AValue : string); virtual;
    Procedure Setfield(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmatchType(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property caseSensitive : boolean Index 0 Read FcaseSensitive Write SetcaseSensitive;
    Property expressionValue : string Index 8 Read FexpressionValue Write SetexpressionValue;
    Property field : string Index 16 Read Ffield Write Setfield;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property matchType : string Index 32 Read FmatchType Write SetmatchType;
  end;
  TFilterExpressionClass = Class of TFilterExpression;
  
  { --------------------------------------------------------------------
    TFilterRef
    --------------------------------------------------------------------}
  
  TFilterRef = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    Fhref : string;
    Fid : string;
    Fkind : string;
    Fname : string;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure Sethref(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property href : string Index 8 Read Fhref Write Sethref;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property name : string Index 32 Read Fname Write Setname;
  end;
  TFilterRefClass = Class of TFilterRef;
  
  { --------------------------------------------------------------------
    TFilters
    --------------------------------------------------------------------}
  
  TFilters = Class(TGoogleBaseObject)
  Private
    Fitems : TFiltersitems;
    FitemsPerPage : integer;
    Fkind : string;
    FnextLink : string;
    FpreviousLink : string;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TFiltersitems); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TFiltersitems Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextLink : string Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : string Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : string Index 56 Read Fusername Write Setusername;
  end;
  TFiltersClass = Class of TFilters;
  
  { --------------------------------------------------------------------
    TFiltersitems
    --------------------------------------------------------------------}
  
  TFiltersitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFiltersitemsClass = Class of TFiltersitems;
  
  { --------------------------------------------------------------------
    TGaData
    --------------------------------------------------------------------}
  
  TGaData = Class(TGoogleBaseObject)
  Private
    FcolumnHeaders : TGaDatacolumnHeaders;
    FcontainsSampledData : boolean;
    FdataTable : TGaDatadataTable;
    Fid : string;
    FitemsPerPage : integer;
    Fkind : string;
    FnextLink : string;
    FpreviousLink : string;
    FprofileInfo : TGaDataprofileInfo;
    Fquery : TGaDataquery;
    Frows : TGaDatarows;
    FsampleSize : string;
    FsampleSpace : string;
    FselfLink : string;
    FtotalResults : integer;
    FtotalsForAllResults : TGaDatatotalsForAllResults;
  Protected
    //Property setters
    Procedure SetcolumnHeaders(AIndex : Integer; AValue : TGaDatacolumnHeaders); virtual;
    Procedure SetcontainsSampledData(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetdataTable(AIndex : Integer; AValue : TGaDatadataTable); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetprofileInfo(AIndex : Integer; AValue : TGaDataprofileInfo); virtual;
    Procedure Setquery(AIndex : Integer; AValue : TGaDataquery); virtual;
    Procedure Setrows(AIndex : Integer; AValue : TGaDatarows); virtual;
    Procedure SetsampleSize(AIndex : Integer; AValue : string); virtual;
    Procedure SetsampleSpace(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalsForAllResults(AIndex : Integer; AValue : TGaDatatotalsForAllResults); virtual;
  Public
  Published
    Property columnHeaders : TGaDatacolumnHeaders Index 0 Read FcolumnHeaders Write SetcolumnHeaders;
    Property containsSampledData : boolean Index 8 Read FcontainsSampledData Write SetcontainsSampledData;
    Property dataTable : TGaDatadataTable Index 16 Read FdataTable Write SetdataTable;
    Property id : string Index 24 Read Fid Write Setid;
    Property itemsPerPage : integer Index 32 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : string Index 40 Read Fkind Write Setkind;
    Property nextLink : string Index 48 Read FnextLink Write SetnextLink;
    Property previousLink : string Index 56 Read FpreviousLink Write SetpreviousLink;
    Property profileInfo : TGaDataprofileInfo Index 64 Read FprofileInfo Write SetprofileInfo;
    Property query : TGaDataquery Index 72 Read Fquery Write Setquery;
    Property rows : TGaDatarows Index 80 Read Frows Write Setrows;
    Property sampleSize : string Index 88 Read FsampleSize Write SetsampleSize;
    Property sampleSpace : string Index 96 Read FsampleSpace Write SetsampleSpace;
    Property selfLink : string Index 104 Read FselfLink Write SetselfLink;
    Property totalResults : integer Index 112 Read FtotalResults Write SettotalResults;
    Property totalsForAllResults : TGaDatatotalsForAllResults Index 120 Read FtotalsForAllResults Write SettotalsForAllResults;
  end;
  TGaDataClass = Class of TGaData;
  
  { --------------------------------------------------------------------
    TGaDatacolumnHeaders
    --------------------------------------------------------------------}
  
  TGaDatacolumnHeaders = Class(TGoogleBaseObject)
  Private
    FcolumnType : string;
    FdataType : string;
    Fname : string;
  Protected
    //Property setters
    Procedure SetcolumnType(AIndex : Integer; AValue : string); virtual;
    Procedure SetdataType(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property columnType : string Index 0 Read FcolumnType Write SetcolumnType;
    Property dataType : string Index 8 Read FdataType Write SetdataType;
    Property name : string Index 16 Read Fname Write Setname;
  end;
  TGaDatacolumnHeadersClass = Class of TGaDatacolumnHeaders;
  
  { --------------------------------------------------------------------
    TGaDatadataTable
    --------------------------------------------------------------------}
  
  TGaDatadataTable = Class(TGoogleBaseObject)
  Private
    Fcols : TGaDatadataTablecols;
    Frows : TGaDatadataTablerows;
  Protected
    //Property setters
    Procedure Setcols(AIndex : Integer; AValue : TGaDatadataTablecols); virtual;
    Procedure Setrows(AIndex : Integer; AValue : TGaDatadataTablerows); virtual;
  Public
  Published
    Property cols : TGaDatadataTablecols Index 0 Read Fcols Write Setcols;
    Property rows : TGaDatadataTablerows Index 8 Read Frows Write Setrows;
  end;
  TGaDatadataTableClass = Class of TGaDatadataTable;
  
  { --------------------------------------------------------------------
    TGaDatadataTablecols
    --------------------------------------------------------------------}
  
  TGaDatadataTablecols = Class(TGoogleBaseObject)
  Private
    Fid : string;
    F_label : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Set_label(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property _label : string Index 8 Read F_label Write Set_label;
    Property _type : string Index 16 Read F_type Write Set_type;
  end;
  TGaDatadataTablecolsClass = Class of TGaDatadataTablecols;
  
  { --------------------------------------------------------------------
    TGaDatadataTablerows
    --------------------------------------------------------------------}
  
  TGaDatadataTablerows = Class(TGoogleBaseObject)
  Private
    Fc : TGaDatadataTablerowsc;
  Protected
    //Property setters
    Procedure Setc(AIndex : Integer; AValue : TGaDatadataTablerowsc); virtual;
  Public
  Published
    Property c : TGaDatadataTablerowsc Index 0 Read Fc Write Setc;
  end;
  TGaDatadataTablerowsClass = Class of TGaDatadataTablerows;
  
  { --------------------------------------------------------------------
    TGaDatadataTablerowsc
    --------------------------------------------------------------------}
  
  TGaDatadataTablerowsc = Class(TGoogleBaseObject)
  Private
    Fv : string;
  Protected
    //Property setters
    Procedure Setv(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property v : string Index 0 Read Fv Write Setv;
  end;
  TGaDatadataTablerowscClass = Class of TGaDatadataTablerowsc;
  
  { --------------------------------------------------------------------
    TGaDataprofileInfo
    --------------------------------------------------------------------}
  
  TGaDataprofileInfo = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    FinternalWebPropertyId : string;
    FprofileId : string;
    FprofileName : string;
    FtableId : string;
    FwebPropertyId : string;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetinternalWebPropertyId(AIndex : Integer; AValue : string); virtual;
    Procedure SetprofileId(AIndex : Integer; AValue : string); virtual;
    Procedure SetprofileName(AIndex : Integer; AValue : string); virtual;
    Procedure SettableId(AIndex : Integer; AValue : string); virtual;
    Procedure SetwebPropertyId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property internalWebPropertyId : string Index 8 Read FinternalWebPropertyId Write SetinternalWebPropertyId;
    Property profileId : string Index 16 Read FprofileId Write SetprofileId;
    Property profileName : string Index 24 Read FprofileName Write SetprofileName;
    Property tableId : string Index 32 Read FtableId Write SettableId;
    Property webPropertyId : string Index 40 Read FwebPropertyId Write SetwebPropertyId;
  end;
  TGaDataprofileInfoClass = Class of TGaDataprofileInfo;
  
  { --------------------------------------------------------------------
    TGaDataquery
    --------------------------------------------------------------------}
  
  TGaDataquery = Class(TGoogleBaseObject)
  Private
    Fdimensions : string;
    Fenddate : string;
    Ffilters : string;
    Fids : string;
    Fmaxresults : integer;
    Fmetrics : TGaDataquerymetrics;
    FsamplingLevel : string;
    Fsegment : string;
    Fsort : TGaDataquerysort;
    Fstartdate : string;
    Fstartindex : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdimensions(AIndex : Integer; AValue : string); virtual;
    Procedure Setenddate(AIndex : Integer; AValue : string); virtual;
    Procedure Setfilters(AIndex : Integer; AValue : string); virtual;
    Procedure Setids(AIndex : Integer; AValue : string); virtual;
    Procedure Setmaxresults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setmetrics(AIndex : Integer; AValue : TGaDataquerymetrics); virtual;
    Procedure SetsamplingLevel(AIndex : Integer; AValue : string); virtual;
    Procedure Setsegment(AIndex : Integer; AValue : string); virtual;
    Procedure Setsort(AIndex : Integer; AValue : TGaDataquerysort); virtual;
    Procedure Setstartdate(AIndex : Integer; AValue : string); virtual;
    Procedure Setstartindex(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property dimensions : string Index 0 Read Fdimensions Write Setdimensions;
    Property enddate : string Index 8 Read Fenddate Write Setenddate;
    Property filters : string Index 16 Read Ffilters Write Setfilters;
    Property ids : string Index 24 Read Fids Write Setids;
    Property maxresults : integer Index 32 Read Fmaxresults Write Setmaxresults;
    Property metrics : TGaDataquerymetrics Index 40 Read Fmetrics Write Setmetrics;
    Property samplingLevel : string Index 48 Read FsamplingLevel Write SetsamplingLevel;
    Property segment : string Index 56 Read Fsegment Write Setsegment;
    Property sort : TGaDataquerysort Index 64 Read Fsort Write Setsort;
    Property startdate : string Index 72 Read Fstartdate Write Setstartdate;
    Property startindex : integer Index 80 Read Fstartindex Write Setstartindex;
  end;
  TGaDataqueryClass = Class of TGaDataquery;
  
  { --------------------------------------------------------------------
    TGaDataquerymetrics
    --------------------------------------------------------------------}
  
  TGaDataquerymetrics = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGaDataquerymetricsClass = Class of TGaDataquerymetrics;
  
  { --------------------------------------------------------------------
    TGaDataquerysort
    --------------------------------------------------------------------}
  
  TGaDataquerysort = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGaDataquerysortClass = Class of TGaDataquerysort;
  
  { --------------------------------------------------------------------
    TGaDatarows
    --------------------------------------------------------------------}
  
  TGaDatarows = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGaDatarowsClass = Class of TGaDatarows;
  
  { --------------------------------------------------------------------
    TGaDatatotalsForAllResults
    --------------------------------------------------------------------}
  
  TGaDatatotalsForAllResults = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TGaDatatotalsForAllResultsClass = Class of TGaDatatotalsForAllResults;
  
  { --------------------------------------------------------------------
    TGoal
    --------------------------------------------------------------------}
  
  TGoal = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    Factive : boolean;
    Fcreated : TDatetime;
    FeventDetails : TGoaleventDetails;
    Fid : string;
    FinternalWebPropertyId : string;
    Fkind : string;
    Fname : string;
    FparentLink : TGoalparentLink;
    FprofileId : string;
    FselfLink : string;
    F_type : string;
    Fupdated : TDatetime;
    FurlDestinationDetails : TGoalurlDestinationDetails;
    Fvalue : integer;
    FvisitNumPagesDetails : TGoalvisitNumPagesDetails;
    FvisitTimeOnSiteDetails : TGoalvisitTimeOnSiteDetails;
    FwebPropertyId : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure Setactive(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SeteventDetails(AIndex : Integer; AValue : TGoaleventDetails); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetinternalWebPropertyId(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetparentLink(AIndex : Integer; AValue : TGoalparentLink); virtual;
    Procedure SetprofileId(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SeturlDestinationDetails(AIndex : Integer; AValue : TGoalurlDestinationDetails); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : integer); virtual;
    Procedure SetvisitNumPagesDetails(AIndex : Integer; AValue : TGoalvisitNumPagesDetails); virtual;
    Procedure SetvisitTimeOnSiteDetails(AIndex : Integer; AValue : TGoalvisitTimeOnSiteDetails); virtual;
    Procedure SetwebPropertyId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property active : boolean Index 8 Read Factive Write Setactive;
    Property created : TDatetime Index 16 Read Fcreated Write Setcreated;
    Property eventDetails : TGoaleventDetails Index 24 Read FeventDetails Write SeteventDetails;
    Property id : string Index 32 Read Fid Write Setid;
    Property internalWebPropertyId : string Index 40 Read FinternalWebPropertyId Write SetinternalWebPropertyId;
    Property kind : string Index 48 Read Fkind Write Setkind;
    Property name : string Index 56 Read Fname Write Setname;
    Property parentLink : TGoalparentLink Index 64 Read FparentLink Write SetparentLink;
    Property profileId : string Index 72 Read FprofileId Write SetprofileId;
    Property selfLink : string Index 80 Read FselfLink Write SetselfLink;
    Property _type : string Index 88 Read F_type Write Set_type;
    Property updated : TDatetime Index 96 Read Fupdated Write Setupdated;
    Property urlDestinationDetails : TGoalurlDestinationDetails Index 104 Read FurlDestinationDetails Write SeturlDestinationDetails;
    Property value : integer Index 112 Read Fvalue Write Setvalue;
    Property visitNumPagesDetails : TGoalvisitNumPagesDetails Index 120 Read FvisitNumPagesDetails Write SetvisitNumPagesDetails;
    Property visitTimeOnSiteDetails : TGoalvisitTimeOnSiteDetails Index 128 Read FvisitTimeOnSiteDetails Write SetvisitTimeOnSiteDetails;
    Property webPropertyId : string Index 136 Read FwebPropertyId Write SetwebPropertyId;
  end;
  TGoalClass = Class of TGoal;
  
  { --------------------------------------------------------------------
    TGoaleventDetails
    --------------------------------------------------------------------}
  
  TGoaleventDetails = Class(TGoogleBaseObject)
  Private
    FeventConditions : TGoaleventDetailseventConditions;
    FuseEventValue : boolean;
  Protected
    //Property setters
    Procedure SeteventConditions(AIndex : Integer; AValue : TGoaleventDetailseventConditions); virtual;
    Procedure SetuseEventValue(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property eventConditions : TGoaleventDetailseventConditions Index 0 Read FeventConditions Write SeteventConditions;
    Property useEventValue : boolean Index 8 Read FuseEventValue Write SetuseEventValue;
  end;
  TGoaleventDetailsClass = Class of TGoaleventDetails;
  
  { --------------------------------------------------------------------
    TGoaleventDetailseventConditions
    --------------------------------------------------------------------}
  
  TGoaleventDetailseventConditions = Class(TGoogleBaseObject)
  Private
    FcomparisonType : string;
    FcomparisonValue : string;
    Fexpression : string;
    FmatchType : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetcomparisonType(AIndex : Integer; AValue : string); virtual;
    Procedure SetcomparisonValue(AIndex : Integer; AValue : string); virtual;
    Procedure Setexpression(AIndex : Integer; AValue : string); virtual;
    Procedure SetmatchType(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property comparisonType : string Index 0 Read FcomparisonType Write SetcomparisonType;
    Property comparisonValue : string Index 8 Read FcomparisonValue Write SetcomparisonValue;
    Property expression : string Index 16 Read Fexpression Write Setexpression;
    Property matchType : string Index 24 Read FmatchType Write SetmatchType;
    Property _type : string Index 32 Read F_type Write Set_type;
  end;
  TGoaleventDetailseventConditionsClass = Class of TGoaleventDetailseventConditions;
  
  { --------------------------------------------------------------------
    TGoalparentLink
    --------------------------------------------------------------------}
  
  TGoalparentLink = Class(TGoogleBaseObject)
  Private
    Fhref : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property href : string Index 0 Read Fhref Write Sethref;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TGoalparentLinkClass = Class of TGoalparentLink;
  
  { --------------------------------------------------------------------
    TGoalurlDestinationDetails
    --------------------------------------------------------------------}
  
  TGoalurlDestinationDetails = Class(TGoogleBaseObject)
  Private
    FcaseSensitive : boolean;
    FfirstStepRequired : boolean;
    FmatchType : string;
    Fsteps : TGoalurlDestinationDetailssteps;
    Furl : string;
  Protected
    //Property setters
    Procedure SetcaseSensitive(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetfirstStepRequired(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetmatchType(AIndex : Integer; AValue : string); virtual;
    Procedure Setsteps(AIndex : Integer; AValue : TGoalurlDestinationDetailssteps); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property caseSensitive : boolean Index 0 Read FcaseSensitive Write SetcaseSensitive;
    Property firstStepRequired : boolean Index 8 Read FfirstStepRequired Write SetfirstStepRequired;
    Property matchType : string Index 16 Read FmatchType Write SetmatchType;
    Property steps : TGoalurlDestinationDetailssteps Index 24 Read Fsteps Write Setsteps;
    Property url : string Index 32 Read Furl Write Seturl;
  end;
  TGoalurlDestinationDetailsClass = Class of TGoalurlDestinationDetails;
  
  { --------------------------------------------------------------------
    TGoalurlDestinationDetailssteps
    --------------------------------------------------------------------}
  
  TGoalurlDestinationDetailssteps = Class(TGoogleBaseObject)
  Private
    Fname : string;
    Fnumber : integer;
    Furl : string;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setnumber(AIndex : Integer; AValue : integer); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
    Property number : integer Index 8 Read Fnumber Write Setnumber;
    Property url : string Index 16 Read Furl Write Seturl;
  end;
  TGoalurlDestinationDetailsstepsClass = Class of TGoalurlDestinationDetailssteps;
  
  { --------------------------------------------------------------------
    TGoalvisitNumPagesDetails
    --------------------------------------------------------------------}
  
  TGoalvisitNumPagesDetails = Class(TGoogleBaseObject)
  Private
    FcomparisonType : string;
    FcomparisonValue : string;
  Protected
    //Property setters
    Procedure SetcomparisonType(AIndex : Integer; AValue : string); virtual;
    Procedure SetcomparisonValue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property comparisonType : string Index 0 Read FcomparisonType Write SetcomparisonType;
    Property comparisonValue : string Index 8 Read FcomparisonValue Write SetcomparisonValue;
  end;
  TGoalvisitNumPagesDetailsClass = Class of TGoalvisitNumPagesDetails;
  
  { --------------------------------------------------------------------
    TGoalvisitTimeOnSiteDetails
    --------------------------------------------------------------------}
  
  TGoalvisitTimeOnSiteDetails = Class(TGoogleBaseObject)
  Private
    FcomparisonType : string;
    FcomparisonValue : string;
  Protected
    //Property setters
    Procedure SetcomparisonType(AIndex : Integer; AValue : string); virtual;
    Procedure SetcomparisonValue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property comparisonType : string Index 0 Read FcomparisonType Write SetcomparisonType;
    Property comparisonValue : string Index 8 Read FcomparisonValue Write SetcomparisonValue;
  end;
  TGoalvisitTimeOnSiteDetailsClass = Class of TGoalvisitTimeOnSiteDetails;
  
  { --------------------------------------------------------------------
    TGoals
    --------------------------------------------------------------------}
  
  TGoals = Class(TGoogleBaseObject)
  Private
    Fitems : TGoalsitems;
    FitemsPerPage : integer;
    Fkind : string;
    FnextLink : string;
    FpreviousLink : string;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TGoalsitems); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TGoalsitems Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextLink : string Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : string Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : string Index 56 Read Fusername Write Setusername;
  end;
  TGoalsClass = Class of TGoals;
  
  { --------------------------------------------------------------------
    TGoalsitems
    --------------------------------------------------------------------}
  
  TGoalsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGoalsitemsClass = Class of TGoalsitems;
  
  { --------------------------------------------------------------------
    TMcfData
    --------------------------------------------------------------------}
  
  TMcfData = Class(TGoogleBaseObject)
  Private
    FcolumnHeaders : TMcfDatacolumnHeaders;
    FcontainsSampledData : boolean;
    Fid : string;
    FitemsPerPage : integer;
    Fkind : string;
    FnextLink : string;
    FpreviousLink : string;
    FprofileInfo : TMcfDataprofileInfo;
    Fquery : TMcfDataquery;
    Frows : TMcfDatarows;
    FsampleSize : string;
    FsampleSpace : string;
    FselfLink : string;
    FtotalResults : integer;
    FtotalsForAllResults : TMcfDatatotalsForAllResults;
  Protected
    //Property setters
    Procedure SetcolumnHeaders(AIndex : Integer; AValue : TMcfDatacolumnHeaders); virtual;
    Procedure SetcontainsSampledData(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetprofileInfo(AIndex : Integer; AValue : TMcfDataprofileInfo); virtual;
    Procedure Setquery(AIndex : Integer; AValue : TMcfDataquery); virtual;
    Procedure Setrows(AIndex : Integer; AValue : TMcfDatarows); virtual;
    Procedure SetsampleSize(AIndex : Integer; AValue : string); virtual;
    Procedure SetsampleSpace(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalsForAllResults(AIndex : Integer; AValue : TMcfDatatotalsForAllResults); virtual;
  Public
  Published
    Property columnHeaders : TMcfDatacolumnHeaders Index 0 Read FcolumnHeaders Write SetcolumnHeaders;
    Property containsSampledData : boolean Index 8 Read FcontainsSampledData Write SetcontainsSampledData;
    Property id : string Index 16 Read Fid Write Setid;
    Property itemsPerPage : integer Index 24 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property nextLink : string Index 40 Read FnextLink Write SetnextLink;
    Property previousLink : string Index 48 Read FpreviousLink Write SetpreviousLink;
    Property profileInfo : TMcfDataprofileInfo Index 56 Read FprofileInfo Write SetprofileInfo;
    Property query : TMcfDataquery Index 64 Read Fquery Write Setquery;
    Property rows : TMcfDatarows Index 72 Read Frows Write Setrows;
    Property sampleSize : string Index 80 Read FsampleSize Write SetsampleSize;
    Property sampleSpace : string Index 88 Read FsampleSpace Write SetsampleSpace;
    Property selfLink : string Index 96 Read FselfLink Write SetselfLink;
    Property totalResults : integer Index 104 Read FtotalResults Write SettotalResults;
    Property totalsForAllResults : TMcfDatatotalsForAllResults Index 112 Read FtotalsForAllResults Write SettotalsForAllResults;
  end;
  TMcfDataClass = Class of TMcfData;
  
  { --------------------------------------------------------------------
    TMcfDatacolumnHeaders
    --------------------------------------------------------------------}
  
  TMcfDatacolumnHeaders = Class(TGoogleBaseObject)
  Private
    FcolumnType : string;
    FdataType : string;
    Fname : string;
  Protected
    //Property setters
    Procedure SetcolumnType(AIndex : Integer; AValue : string); virtual;
    Procedure SetdataType(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property columnType : string Index 0 Read FcolumnType Write SetcolumnType;
    Property dataType : string Index 8 Read FdataType Write SetdataType;
    Property name : string Index 16 Read Fname Write Setname;
  end;
  TMcfDatacolumnHeadersClass = Class of TMcfDatacolumnHeaders;
  
  { --------------------------------------------------------------------
    TMcfDataprofileInfo
    --------------------------------------------------------------------}
  
  TMcfDataprofileInfo = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    FinternalWebPropertyId : string;
    FprofileId : string;
    FprofileName : string;
    FtableId : string;
    FwebPropertyId : string;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetinternalWebPropertyId(AIndex : Integer; AValue : string); virtual;
    Procedure SetprofileId(AIndex : Integer; AValue : string); virtual;
    Procedure SetprofileName(AIndex : Integer; AValue : string); virtual;
    Procedure SettableId(AIndex : Integer; AValue : string); virtual;
    Procedure SetwebPropertyId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property internalWebPropertyId : string Index 8 Read FinternalWebPropertyId Write SetinternalWebPropertyId;
    Property profileId : string Index 16 Read FprofileId Write SetprofileId;
    Property profileName : string Index 24 Read FprofileName Write SetprofileName;
    Property tableId : string Index 32 Read FtableId Write SettableId;
    Property webPropertyId : string Index 40 Read FwebPropertyId Write SetwebPropertyId;
  end;
  TMcfDataprofileInfoClass = Class of TMcfDataprofileInfo;
  
  { --------------------------------------------------------------------
    TMcfDataquery
    --------------------------------------------------------------------}
  
  TMcfDataquery = Class(TGoogleBaseObject)
  Private
    Fdimensions : string;
    Fenddate : string;
    Ffilters : string;
    Fids : string;
    Fmaxresults : integer;
    Fmetrics : TMcfDataquerymetrics;
    FsamplingLevel : string;
    Fsegment : string;
    Fsort : TMcfDataquerysort;
    Fstartdate : string;
    Fstartindex : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdimensions(AIndex : Integer; AValue : string); virtual;
    Procedure Setenddate(AIndex : Integer; AValue : string); virtual;
    Procedure Setfilters(AIndex : Integer; AValue : string); virtual;
    Procedure Setids(AIndex : Integer; AValue : string); virtual;
    Procedure Setmaxresults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setmetrics(AIndex : Integer; AValue : TMcfDataquerymetrics); virtual;
    Procedure SetsamplingLevel(AIndex : Integer; AValue : string); virtual;
    Procedure Setsegment(AIndex : Integer; AValue : string); virtual;
    Procedure Setsort(AIndex : Integer; AValue : TMcfDataquerysort); virtual;
    Procedure Setstartdate(AIndex : Integer; AValue : string); virtual;
    Procedure Setstartindex(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property dimensions : string Index 0 Read Fdimensions Write Setdimensions;
    Property enddate : string Index 8 Read Fenddate Write Setenddate;
    Property filters : string Index 16 Read Ffilters Write Setfilters;
    Property ids : string Index 24 Read Fids Write Setids;
    Property maxresults : integer Index 32 Read Fmaxresults Write Setmaxresults;
    Property metrics : TMcfDataquerymetrics Index 40 Read Fmetrics Write Setmetrics;
    Property samplingLevel : string Index 48 Read FsamplingLevel Write SetsamplingLevel;
    Property segment : string Index 56 Read Fsegment Write Setsegment;
    Property sort : TMcfDataquerysort Index 64 Read Fsort Write Setsort;
    Property startdate : string Index 72 Read Fstartdate Write Setstartdate;
    Property startindex : integer Index 80 Read Fstartindex Write Setstartindex;
  end;
  TMcfDataqueryClass = Class of TMcfDataquery;
  
  { --------------------------------------------------------------------
    TMcfDataquerymetrics
    --------------------------------------------------------------------}
  
  TMcfDataquerymetrics = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMcfDataquerymetricsClass = Class of TMcfDataquerymetrics;
  
  { --------------------------------------------------------------------
    TMcfDataquerysort
    --------------------------------------------------------------------}
  
  TMcfDataquerysort = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMcfDataquerysortClass = Class of TMcfDataquerysort;
  
  { --------------------------------------------------------------------
    TMcfDatarows
    --------------------------------------------------------------------}
  
  TMcfDatarows = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMcfDatarowsClass = Class of TMcfDatarows;
  
  { --------------------------------------------------------------------
    TMcfDatatotalsForAllResults
    --------------------------------------------------------------------}
  
  TMcfDatatotalsForAllResults = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TMcfDatatotalsForAllResultsClass = Class of TMcfDatatotalsForAllResults;
  
  { --------------------------------------------------------------------
    TProfile
    --------------------------------------------------------------------}
  
  TProfile = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    FchildLink : TProfilechildLink;
    Fcreated : TDatetime;
    Fcurrency : string;
    FdefaultPage : string;
    FeCommerceTracking : boolean;
    FenhancedECommerceTracking : boolean;
    FexcludeQueryParameters : string;
    Fid : string;
    FinternalWebPropertyId : string;
    Fkind : string;
    Fname : string;
    FparentLink : TProfileparentLink;
    Fpermissions : TProfilepermissions;
    FselfLink : string;
    FsiteSearchCategoryParameters : string;
    FsiteSearchQueryParameters : string;
    FstripSiteSearchCategoryParameters : boolean;
    FstripSiteSearchQueryParameters : boolean;
    Ftimezone : string;
    F_type : string;
    Fupdated : TDatetime;
    FwebPropertyId : string;
    FwebsiteUrl : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetchildLink(AIndex : Integer; AValue : TProfilechildLink); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setcurrency(AIndex : Integer; AValue : string); virtual;
    Procedure SetdefaultPage(AIndex : Integer; AValue : string); virtual;
    Procedure SeteCommerceTracking(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetenhancedECommerceTracking(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetexcludeQueryParameters(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetinternalWebPropertyId(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetparentLink(AIndex : Integer; AValue : TProfileparentLink); virtual;
    Procedure Setpermissions(AIndex : Integer; AValue : TProfilepermissions); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetsiteSearchCategoryParameters(AIndex : Integer; AValue : string); virtual;
    Procedure SetsiteSearchQueryParameters(AIndex : Integer; AValue : string); virtual;
    Procedure SetstripSiteSearchCategoryParameters(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetstripSiteSearchQueryParameters(AIndex : Integer; AValue : boolean); virtual;
    Procedure Settimezone(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetwebPropertyId(AIndex : Integer; AValue : string); virtual;
    Procedure SetwebsiteUrl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property childLink : TProfilechildLink Index 8 Read FchildLink Write SetchildLink;
    Property created : TDatetime Index 16 Read Fcreated Write Setcreated;
    Property currency : string Index 24 Read Fcurrency Write Setcurrency;
    Property defaultPage : string Index 32 Read FdefaultPage Write SetdefaultPage;
    Property eCommerceTracking : boolean Index 40 Read FeCommerceTracking Write SeteCommerceTracking;
    Property enhancedECommerceTracking : boolean Index 48 Read FenhancedECommerceTracking Write SetenhancedECommerceTracking;
    Property excludeQueryParameters : string Index 56 Read FexcludeQueryParameters Write SetexcludeQueryParameters;
    Property id : string Index 64 Read Fid Write Setid;
    Property internalWebPropertyId : string Index 72 Read FinternalWebPropertyId Write SetinternalWebPropertyId;
    Property kind : string Index 80 Read Fkind Write Setkind;
    Property name : string Index 88 Read Fname Write Setname;
    Property parentLink : TProfileparentLink Index 96 Read FparentLink Write SetparentLink;
    Property permissions : TProfilepermissions Index 104 Read Fpermissions Write Setpermissions;
    Property selfLink : string Index 112 Read FselfLink Write SetselfLink;
    Property siteSearchCategoryParameters : string Index 120 Read FsiteSearchCategoryParameters Write SetsiteSearchCategoryParameters;
    Property siteSearchQueryParameters : string Index 128 Read FsiteSearchQueryParameters Write SetsiteSearchQueryParameters;
    Property stripSiteSearchCategoryParameters : boolean Index 136 Read FstripSiteSearchCategoryParameters Write SetstripSiteSearchCategoryParameters;
    Property stripSiteSearchQueryParameters : boolean Index 144 Read FstripSiteSearchQueryParameters Write SetstripSiteSearchQueryParameters;
    Property timezone : string Index 152 Read Ftimezone Write Settimezone;
    Property _type : string Index 160 Read F_type Write Set_type;
    Property updated : TDatetime Index 168 Read Fupdated Write Setupdated;
    Property webPropertyId : string Index 176 Read FwebPropertyId Write SetwebPropertyId;
    Property websiteUrl : string Index 184 Read FwebsiteUrl Write SetwebsiteUrl;
  end;
  TProfileClass = Class of TProfile;
  
  { --------------------------------------------------------------------
    TProfilechildLink
    --------------------------------------------------------------------}
  
  TProfilechildLink = Class(TGoogleBaseObject)
  Private
    Fhref : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property href : string Index 0 Read Fhref Write Sethref;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TProfilechildLinkClass = Class of TProfilechildLink;
  
  { --------------------------------------------------------------------
    TProfileparentLink
    --------------------------------------------------------------------}
  
  TProfileparentLink = Class(TGoogleBaseObject)
  Private
    Fhref : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property href : string Index 0 Read Fhref Write Sethref;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TProfileparentLinkClass = Class of TProfileparentLink;
  
  { --------------------------------------------------------------------
    TProfilepermissions
    --------------------------------------------------------------------}
  
  TProfilepermissions = Class(TGoogleBaseObject)
  Private
    Feffective : TProfilepermissionseffective;
  Protected
    //Property setters
    Procedure Seteffective(AIndex : Integer; AValue : TProfilepermissionseffective); virtual;
  Public
  Published
    Property effective : TProfilepermissionseffective Index 0 Read Feffective Write Seteffective;
  end;
  TProfilepermissionsClass = Class of TProfilepermissions;
  
  { --------------------------------------------------------------------
    TProfilepermissionseffective
    --------------------------------------------------------------------}
  
  TProfilepermissionseffective = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProfilepermissionseffectiveClass = Class of TProfilepermissionseffective;
  
  { --------------------------------------------------------------------
    TProfileFilterLink
    --------------------------------------------------------------------}
  
  TProfileFilterLink = Class(TGoogleBaseObject)
  Private
    FfilterRef : TFilterRef;
    Fid : string;
    Fkind : string;
    FprofileRef : TProfileRef;
    Frank : integer;
    FselfLink : string;
  Protected
    //Property setters
    Procedure SetfilterRef(AIndex : Integer; AValue : TFilterRef); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetprofileRef(AIndex : Integer; AValue : TProfileRef); virtual;
    Procedure Setrank(AIndex : Integer; AValue : integer); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property filterRef : TFilterRef Index 0 Read FfilterRef Write SetfilterRef;
    Property id : string Index 8 Read Fid Write Setid;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property profileRef : TProfileRef Index 24 Read FprofileRef Write SetprofileRef;
    Property rank : integer Index 32 Read Frank Write Setrank;
    Property selfLink : string Index 40 Read FselfLink Write SetselfLink;
  end;
  TProfileFilterLinkClass = Class of TProfileFilterLink;
  
  { --------------------------------------------------------------------
    TProfileFilterLinks
    --------------------------------------------------------------------}
  
  TProfileFilterLinks = Class(TGoogleBaseObject)
  Private
    Fitems : TProfileFilterLinksitems;
    FitemsPerPage : integer;
    Fkind : string;
    FnextLink : string;
    FpreviousLink : string;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TProfileFilterLinksitems); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TProfileFilterLinksitems Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextLink : string Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : string Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : string Index 56 Read Fusername Write Setusername;
  end;
  TProfileFilterLinksClass = Class of TProfileFilterLinks;
  
  { --------------------------------------------------------------------
    TProfileFilterLinksitems
    --------------------------------------------------------------------}
  
  TProfileFilterLinksitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProfileFilterLinksitemsClass = Class of TProfileFilterLinksitems;
  
  { --------------------------------------------------------------------
    TProfileRef
    --------------------------------------------------------------------}
  
  TProfileRef = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    Fhref : string;
    Fid : string;
    FinternalWebPropertyId : string;
    Fkind : string;
    Fname : string;
    FwebPropertyId : string;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure Sethref(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetinternalWebPropertyId(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetwebPropertyId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property href : string Index 8 Read Fhref Write Sethref;
    Property id : string Index 16 Read Fid Write Setid;
    Property internalWebPropertyId : string Index 24 Read FinternalWebPropertyId Write SetinternalWebPropertyId;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property name : string Index 40 Read Fname Write Setname;
    Property webPropertyId : string Index 48 Read FwebPropertyId Write SetwebPropertyId;
  end;
  TProfileRefClass = Class of TProfileRef;
  
  { --------------------------------------------------------------------
    TProfileSummary
    --------------------------------------------------------------------}
  
  TProfileSummary = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fkind : string;
    Fname : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property name : string Index 16 Read Fname Write Setname;
    Property _type : string Index 24 Read F_type Write Set_type;
  end;
  TProfileSummaryClass = Class of TProfileSummary;
  
  { --------------------------------------------------------------------
    TProfiles
    --------------------------------------------------------------------}
  
  TProfiles = Class(TGoogleBaseObject)
  Private
    Fitems : TProfilesitems;
    FitemsPerPage : integer;
    Fkind : string;
    FnextLink : string;
    FpreviousLink : string;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TProfilesitems); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TProfilesitems Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextLink : string Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : string Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : string Index 56 Read Fusername Write Setusername;
  end;
  TProfilesClass = Class of TProfiles;
  
  { --------------------------------------------------------------------
    TProfilesitems
    --------------------------------------------------------------------}
  
  TProfilesitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TProfilesitemsClass = Class of TProfilesitems;
  
  { --------------------------------------------------------------------
    TRealtimeData
    --------------------------------------------------------------------}
  
  TRealtimeData = Class(TGoogleBaseObject)
  Private
    FcolumnHeaders : TRealtimeDatacolumnHeaders;
    Fid : string;
    Fkind : string;
    FprofileInfo : TRealtimeDataprofileInfo;
    Fquery : TRealtimeDataquery;
    Frows : TRealtimeDatarows;
    FselfLink : string;
    FtotalResults : integer;
    FtotalsForAllResults : TRealtimeDatatotalsForAllResults;
  Protected
    //Property setters
    Procedure SetcolumnHeaders(AIndex : Integer; AValue : TRealtimeDatacolumnHeaders); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetprofileInfo(AIndex : Integer; AValue : TRealtimeDataprofileInfo); virtual;
    Procedure Setquery(AIndex : Integer; AValue : TRealtimeDataquery); virtual;
    Procedure Setrows(AIndex : Integer; AValue : TRealtimeDatarows); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalsForAllResults(AIndex : Integer; AValue : TRealtimeDatatotalsForAllResults); virtual;
  Public
  Published
    Property columnHeaders : TRealtimeDatacolumnHeaders Index 0 Read FcolumnHeaders Write SetcolumnHeaders;
    Property id : string Index 8 Read Fid Write Setid;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property profileInfo : TRealtimeDataprofileInfo Index 24 Read FprofileInfo Write SetprofileInfo;
    Property query : TRealtimeDataquery Index 32 Read Fquery Write Setquery;
    Property rows : TRealtimeDatarows Index 40 Read Frows Write Setrows;
    Property selfLink : string Index 48 Read FselfLink Write SetselfLink;
    Property totalResults : integer Index 56 Read FtotalResults Write SettotalResults;
    Property totalsForAllResults : TRealtimeDatatotalsForAllResults Index 64 Read FtotalsForAllResults Write SettotalsForAllResults;
  end;
  TRealtimeDataClass = Class of TRealtimeData;
  
  { --------------------------------------------------------------------
    TRealtimeDatacolumnHeaders
    --------------------------------------------------------------------}
  
  TRealtimeDatacolumnHeaders = Class(TGoogleBaseObject)
  Private
    FcolumnType : string;
    FdataType : string;
    Fname : string;
  Protected
    //Property setters
    Procedure SetcolumnType(AIndex : Integer; AValue : string); virtual;
    Procedure SetdataType(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property columnType : string Index 0 Read FcolumnType Write SetcolumnType;
    Property dataType : string Index 8 Read FdataType Write SetdataType;
    Property name : string Index 16 Read Fname Write Setname;
  end;
  TRealtimeDatacolumnHeadersClass = Class of TRealtimeDatacolumnHeaders;
  
  { --------------------------------------------------------------------
    TRealtimeDataprofileInfo
    --------------------------------------------------------------------}
  
  TRealtimeDataprofileInfo = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    FinternalWebPropertyId : string;
    FprofileId : string;
    FprofileName : string;
    FtableId : string;
    FwebPropertyId : string;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetinternalWebPropertyId(AIndex : Integer; AValue : string); virtual;
    Procedure SetprofileId(AIndex : Integer; AValue : string); virtual;
    Procedure SetprofileName(AIndex : Integer; AValue : string); virtual;
    Procedure SettableId(AIndex : Integer; AValue : string); virtual;
    Procedure SetwebPropertyId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property internalWebPropertyId : string Index 8 Read FinternalWebPropertyId Write SetinternalWebPropertyId;
    Property profileId : string Index 16 Read FprofileId Write SetprofileId;
    Property profileName : string Index 24 Read FprofileName Write SetprofileName;
    Property tableId : string Index 32 Read FtableId Write SettableId;
    Property webPropertyId : string Index 40 Read FwebPropertyId Write SetwebPropertyId;
  end;
  TRealtimeDataprofileInfoClass = Class of TRealtimeDataprofileInfo;
  
  { --------------------------------------------------------------------
    TRealtimeDataquery
    --------------------------------------------------------------------}
  
  TRealtimeDataquery = Class(TGoogleBaseObject)
  Private
    Fdimensions : string;
    Ffilters : string;
    Fids : string;
    Fmaxresults : integer;
    Fmetrics : TRealtimeDataquerymetrics;
    Fsort : TRealtimeDataquerysort;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdimensions(AIndex : Integer; AValue : string); virtual;
    Procedure Setfilters(AIndex : Integer; AValue : string); virtual;
    Procedure Setids(AIndex : Integer; AValue : string); virtual;
    Procedure Setmaxresults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setmetrics(AIndex : Integer; AValue : TRealtimeDataquerymetrics); virtual;
    Procedure Setsort(AIndex : Integer; AValue : TRealtimeDataquerysort); virtual;
  Public
  Published
    Property dimensions : string Index 0 Read Fdimensions Write Setdimensions;
    Property filters : string Index 8 Read Ffilters Write Setfilters;
    Property ids : string Index 16 Read Fids Write Setids;
    Property maxresults : integer Index 24 Read Fmaxresults Write Setmaxresults;
    Property metrics : TRealtimeDataquerymetrics Index 32 Read Fmetrics Write Setmetrics;
    Property sort : TRealtimeDataquerysort Index 40 Read Fsort Write Setsort;
  end;
  TRealtimeDataqueryClass = Class of TRealtimeDataquery;
  
  { --------------------------------------------------------------------
    TRealtimeDataquerymetrics
    --------------------------------------------------------------------}
  
  TRealtimeDataquerymetrics = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRealtimeDataquerymetricsClass = Class of TRealtimeDataquerymetrics;
  
  { --------------------------------------------------------------------
    TRealtimeDataquerysort
    --------------------------------------------------------------------}
  
  TRealtimeDataquerysort = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRealtimeDataquerysortClass = Class of TRealtimeDataquerysort;
  
  { --------------------------------------------------------------------
    TRealtimeDatarows
    --------------------------------------------------------------------}
  
  TRealtimeDatarows = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRealtimeDatarowsClass = Class of TRealtimeDatarows;
  
  { --------------------------------------------------------------------
    TRealtimeDatatotalsForAllResults
    --------------------------------------------------------------------}
  
  TRealtimeDatatotalsForAllResults = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TRealtimeDatatotalsForAllResultsClass = Class of TRealtimeDatatotalsForAllResults;
  
  { --------------------------------------------------------------------
    TSegment
    --------------------------------------------------------------------}
  
  TSegment = Class(TGoogleBaseObject)
  Private
    Fcreated : TDatetime;
    Fdefinition : string;
    Fid : string;
    Fkind : string;
    Fname : string;
    FsegmentId : string;
    FselfLink : string;
    F_type : string;
    Fupdated : TDatetime;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setdefinition(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetsegmentId(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property created : TDatetime Index 0 Read Fcreated Write Setcreated;
    Property definition : string Index 8 Read Fdefinition Write Setdefinition;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property name : string Index 32 Read Fname Write Setname;
    Property segmentId : string Index 40 Read FsegmentId Write SetsegmentId;
    Property selfLink : string Index 48 Read FselfLink Write SetselfLink;
    Property _type : string Index 56 Read F_type Write Set_type;
    Property updated : TDatetime Index 64 Read Fupdated Write Setupdated;
  end;
  TSegmentClass = Class of TSegment;
  
  { --------------------------------------------------------------------
    TSegments
    --------------------------------------------------------------------}
  
  TSegments = Class(TGoogleBaseObject)
  Private
    Fitems : TSegmentsitems;
    FitemsPerPage : integer;
    Fkind : string;
    FnextLink : string;
    FpreviousLink : string;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TSegmentsitems); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TSegmentsitems Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextLink : string Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : string Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : string Index 56 Read Fusername Write Setusername;
  end;
  TSegmentsClass = Class of TSegments;
  
  { --------------------------------------------------------------------
    TSegmentsitems
    --------------------------------------------------------------------}
  
  TSegmentsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSegmentsitemsClass = Class of TSegmentsitems;
  
  { --------------------------------------------------------------------
    TUnsampledReport
    --------------------------------------------------------------------}
  
  TUnsampledReport = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    FcloudStorageDownloadDetails : TUnsampledReportcloudStorageDownloadDetails;
    Fcreated : TDatetime;
    Fdimensions : string;
    FdownloadType : string;
    FdriveDownloadDetails : TUnsampledReportdriveDownloadDetails;
    Fenddate : string;
    Ffilters : string;
    Fid : string;
    Fkind : string;
    Fmetrics : string;
    FprofileId : string;
    Fsegment : string;
    FselfLink : string;
    Fstartdate : string;
    Fstatus : string;
    Ftitle : string;
    Fupdated : TDatetime;
    FwebPropertyId : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetcloudStorageDownloadDetails(AIndex : Integer; AValue : TUnsampledReportcloudStorageDownloadDetails); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setdimensions(AIndex : Integer; AValue : string); virtual;
    Procedure SetdownloadType(AIndex : Integer; AValue : string); virtual;
    Procedure SetdriveDownloadDetails(AIndex : Integer; AValue : TUnsampledReportdriveDownloadDetails); virtual;
    Procedure Setenddate(AIndex : Integer; AValue : string); virtual;
    Procedure Setfilters(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setmetrics(AIndex : Integer; AValue : string); virtual;
    Procedure SetprofileId(AIndex : Integer; AValue : string); virtual;
    Procedure Setsegment(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setstartdate(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetwebPropertyId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property cloudStorageDownloadDetails : TUnsampledReportcloudStorageDownloadDetails Index 8 Read FcloudStorageDownloadDetails Write SetcloudStorageDownloadDetails;
    Property created : TDatetime Index 16 Read Fcreated Write Setcreated;
    Property dimensions : string Index 24 Read Fdimensions Write Setdimensions;
    Property downloadType : string Index 32 Read FdownloadType Write SetdownloadType;
    Property driveDownloadDetails : TUnsampledReportdriveDownloadDetails Index 40 Read FdriveDownloadDetails Write SetdriveDownloadDetails;
    Property enddate : string Index 48 Read Fenddate Write Setenddate;
    Property filters : string Index 56 Read Ffilters Write Setfilters;
    Property id : string Index 64 Read Fid Write Setid;
    Property kind : string Index 72 Read Fkind Write Setkind;
    Property metrics : string Index 80 Read Fmetrics Write Setmetrics;
    Property profileId : string Index 88 Read FprofileId Write SetprofileId;
    Property segment : string Index 96 Read Fsegment Write Setsegment;
    Property selfLink : string Index 104 Read FselfLink Write SetselfLink;
    Property startdate : string Index 112 Read Fstartdate Write Setstartdate;
    Property status : string Index 120 Read Fstatus Write Setstatus;
    Property title : string Index 128 Read Ftitle Write Settitle;
    Property updated : TDatetime Index 136 Read Fupdated Write Setupdated;
    Property webPropertyId : string Index 144 Read FwebPropertyId Write SetwebPropertyId;
  end;
  TUnsampledReportClass = Class of TUnsampledReport;
  
  { --------------------------------------------------------------------
    TUnsampledReportcloudStorageDownloadDetails
    --------------------------------------------------------------------}
  
  TUnsampledReportcloudStorageDownloadDetails = Class(TGoogleBaseObject)
  Private
    FbucketId : string;
    FobjectId : string;
  Protected
    //Property setters
    Procedure SetbucketId(AIndex : Integer; AValue : string); virtual;
    Procedure SetobjectId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property bucketId : string Index 0 Read FbucketId Write SetbucketId;
    Property objectId : string Index 8 Read FobjectId Write SetobjectId;
  end;
  TUnsampledReportcloudStorageDownloadDetailsClass = Class of TUnsampledReportcloudStorageDownloadDetails;
  
  { --------------------------------------------------------------------
    TUnsampledReportdriveDownloadDetails
    --------------------------------------------------------------------}
  
  TUnsampledReportdriveDownloadDetails = Class(TGoogleBaseObject)
  Private
    FdocumentId : string;
  Protected
    //Property setters
    Procedure SetdocumentId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property documentId : string Index 0 Read FdocumentId Write SetdocumentId;
  end;
  TUnsampledReportdriveDownloadDetailsClass = Class of TUnsampledReportdriveDownloadDetails;
  
  { --------------------------------------------------------------------
    TUnsampledReports
    --------------------------------------------------------------------}
  
  TUnsampledReports = Class(TGoogleBaseObject)
  Private
    Fitems : TUnsampledReportsitems;
    FitemsPerPage : integer;
    Fkind : string;
    FnextLink : string;
    FpreviousLink : string;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TUnsampledReportsitems); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TUnsampledReportsitems Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextLink : string Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : string Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : string Index 56 Read Fusername Write Setusername;
  end;
  TUnsampledReportsClass = Class of TUnsampledReports;
  
  { --------------------------------------------------------------------
    TUnsampledReportsitems
    --------------------------------------------------------------------}
  
  TUnsampledReportsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUnsampledReportsitemsClass = Class of TUnsampledReportsitems;
  
  { --------------------------------------------------------------------
    TUpload
    --------------------------------------------------------------------}
  
  TUpload = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    FcustomDataSourceId : string;
    Ferrors : TUploaderrors;
    Fid : string;
    Fkind : string;
    Fstatus : string;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetcustomDataSourceId(AIndex : Integer; AValue : string); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TUploaderrors); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property customDataSourceId : string Index 8 Read FcustomDataSourceId Write SetcustomDataSourceId;
    Property errors : TUploaderrors Index 16 Read Ferrors Write Seterrors;
    Property id : string Index 24 Read Fid Write Setid;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property status : string Index 40 Read Fstatus Write Setstatus;
  end;
  TUploadClass = Class of TUpload;
  
  { --------------------------------------------------------------------
    TUploaderrors
    --------------------------------------------------------------------}
  
  TUploaderrors = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUploaderrorsClass = Class of TUploaderrors;
  
  { --------------------------------------------------------------------
    TUploads
    --------------------------------------------------------------------}
  
  TUploads = Class(TGoogleBaseObject)
  Private
    Fitems : TUploadsitems;
    FitemsPerPage : integer;
    Fkind : string;
    FnextLink : string;
    FpreviousLink : string;
    FstartIndex : integer;
    FtotalResults : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TUploadsitems); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property items : TUploadsitems Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextLink : string Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : string Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
  end;
  TUploadsClass = Class of TUploads;
  
  { --------------------------------------------------------------------
    TUploadsitems
    --------------------------------------------------------------------}
  
  TUploadsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUploadsitemsClass = Class of TUploadsitems;
  
  { --------------------------------------------------------------------
    TUserRef
    --------------------------------------------------------------------}
  
  TUserRef = Class(TGoogleBaseObject)
  Private
    Femail : string;
    Fid : string;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property email : string Index 0 Read Femail Write Setemail;
    Property id : string Index 8 Read Fid Write Setid;
    Property kind : string Index 16 Read Fkind Write Setkind;
  end;
  TUserRefClass = Class of TUserRef;
  
  { --------------------------------------------------------------------
    TWebPropertyRef
    --------------------------------------------------------------------}
  
  TWebPropertyRef = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    Fhref : string;
    Fid : string;
    FinternalWebPropertyId : string;
    Fkind : string;
    Fname : string;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure Sethref(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetinternalWebPropertyId(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property href : string Index 8 Read Fhref Write Sethref;
    Property id : string Index 16 Read Fid Write Setid;
    Property internalWebPropertyId : string Index 24 Read FinternalWebPropertyId Write SetinternalWebPropertyId;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property name : string Index 40 Read Fname Write Setname;
  end;
  TWebPropertyRefClass = Class of TWebPropertyRef;
  
  { --------------------------------------------------------------------
    TWebPropertySummary
    --------------------------------------------------------------------}
  
  TWebPropertySummary = Class(TGoogleBaseObject)
  Private
    Fid : string;
    FinternalWebPropertyId : string;
    Fkind : string;
    Flevel : string;
    Fname : string;
    Fprofiles : TWebPropertySummaryprofiles;
    FwebsiteUrl : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetinternalWebPropertyId(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlevel(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setprofiles(AIndex : Integer; AValue : TWebPropertySummaryprofiles); virtual;
    Procedure SetwebsiteUrl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property internalWebPropertyId : string Index 8 Read FinternalWebPropertyId Write SetinternalWebPropertyId;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property level : string Index 24 Read Flevel Write Setlevel;
    Property name : string Index 32 Read Fname Write Setname;
    Property profiles : TWebPropertySummaryprofiles Index 40 Read Fprofiles Write Setprofiles;
    Property websiteUrl : string Index 48 Read FwebsiteUrl Write SetwebsiteUrl;
  end;
  TWebPropertySummaryClass = Class of TWebPropertySummary;
  
  { --------------------------------------------------------------------
    TWebPropertySummaryprofiles
    --------------------------------------------------------------------}
  
  TWebPropertySummaryprofiles = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TWebPropertySummaryprofilesClass = Class of TWebPropertySummaryprofiles;
  
  { --------------------------------------------------------------------
    TWebproperties
    --------------------------------------------------------------------}
  
  TWebproperties = Class(TGoogleBaseObject)
  Private
    Fitems : TWebpropertiesitems;
    FitemsPerPage : integer;
    Fkind : string;
    FnextLink : string;
    FpreviousLink : string;
    FstartIndex : integer;
    FtotalResults : integer;
    Fusername : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TWebpropertiesitems); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetpreviousLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TWebpropertiesitems Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextLink : string Index 24 Read FnextLink Write SetnextLink;
    Property previousLink : string Index 32 Read FpreviousLink Write SetpreviousLink;
    Property startIndex : integer Index 40 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 48 Read FtotalResults Write SettotalResults;
    Property username : string Index 56 Read Fusername Write Setusername;
  end;
  TWebpropertiesClass = Class of TWebproperties;
  
  { --------------------------------------------------------------------
    TWebpropertiesitems
    --------------------------------------------------------------------}
  
  TWebpropertiesitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TWebpropertiesitemsClass = Class of TWebpropertiesitems;
  
  { --------------------------------------------------------------------
    TWebproperty
    --------------------------------------------------------------------}
  
  TWebproperty = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    FchildLink : TWebpropertychildLink;
    Fcreated : TDatetime;
    FdefaultProfileId : string;
    Fid : string;
    FindustryVertical : string;
    FinternalWebPropertyId : string;
    Fkind : string;
    Flevel : string;
    Fname : string;
    FparentLink : TWebpropertyparentLink;
    Fpermissions : TWebpropertypermissions;
    FprofileCount : integer;
    FselfLink : string;
    Fupdated : TDatetime;
    FwebsiteUrl : string;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetchildLink(AIndex : Integer; AValue : TWebpropertychildLink); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetdefaultProfileId(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetindustryVertical(AIndex : Integer; AValue : string); virtual;
    Procedure SetinternalWebPropertyId(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlevel(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetparentLink(AIndex : Integer; AValue : TWebpropertyparentLink); virtual;
    Procedure Setpermissions(AIndex : Integer; AValue : TWebpropertypermissions); virtual;
    Procedure SetprofileCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetwebsiteUrl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property childLink : TWebpropertychildLink Index 8 Read FchildLink Write SetchildLink;
    Property created : TDatetime Index 16 Read Fcreated Write Setcreated;
    Property defaultProfileId : string Index 24 Read FdefaultProfileId Write SetdefaultProfileId;
    Property id : string Index 32 Read Fid Write Setid;
    Property industryVertical : string Index 40 Read FindustryVertical Write SetindustryVertical;
    Property internalWebPropertyId : string Index 48 Read FinternalWebPropertyId Write SetinternalWebPropertyId;
    Property kind : string Index 56 Read Fkind Write Setkind;
    Property level : string Index 64 Read Flevel Write Setlevel;
    Property name : string Index 72 Read Fname Write Setname;
    Property parentLink : TWebpropertyparentLink Index 80 Read FparentLink Write SetparentLink;
    Property permissions : TWebpropertypermissions Index 88 Read Fpermissions Write Setpermissions;
    Property profileCount : integer Index 96 Read FprofileCount Write SetprofileCount;
    Property selfLink : string Index 104 Read FselfLink Write SetselfLink;
    Property updated : TDatetime Index 112 Read Fupdated Write Setupdated;
    Property websiteUrl : string Index 120 Read FwebsiteUrl Write SetwebsiteUrl;
  end;
  TWebpropertyClass = Class of TWebproperty;
  
  { --------------------------------------------------------------------
    TWebpropertychildLink
    --------------------------------------------------------------------}
  
  TWebpropertychildLink = Class(TGoogleBaseObject)
  Private
    Fhref : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property href : string Index 0 Read Fhref Write Sethref;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TWebpropertychildLinkClass = Class of TWebpropertychildLink;
  
  { --------------------------------------------------------------------
    TWebpropertyparentLink
    --------------------------------------------------------------------}
  
  TWebpropertyparentLink = Class(TGoogleBaseObject)
  Private
    Fhref : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Sethref(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property href : string Index 0 Read Fhref Write Sethref;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TWebpropertyparentLinkClass = Class of TWebpropertyparentLink;
  
  { --------------------------------------------------------------------
    TWebpropertypermissions
    --------------------------------------------------------------------}
  
  TWebpropertypermissions = Class(TGoogleBaseObject)
  Private
    Feffective : TWebpropertypermissionseffective;
  Protected
    //Property setters
    Procedure Seteffective(AIndex : Integer; AValue : TWebpropertypermissionseffective); virtual;
  Public
  Published
    Property effective : TWebpropertypermissionseffective Index 0 Read Feffective Write Seteffective;
  end;
  TWebpropertypermissionsClass = Class of TWebpropertypermissions;
  
  { --------------------------------------------------------------------
    TWebpropertypermissionseffective
    --------------------------------------------------------------------}
  
  TWebpropertypermissionseffective = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TWebpropertypermissionseffectiveClass = Class of TWebpropertypermissionseffective;
  
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
  TAccount
  --------------------------------------------------------------------}


Procedure TAccount.SetchildLink(AIndex : Integer; AValue : TAccountchildLink); 

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



Procedure TAccount.Setpermissions(AIndex : Integer; AValue : TAccountpermissions); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetselfLink(AIndex : Integer; AValue : string); 

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
  TAccountchildLink
  --------------------------------------------------------------------}


Procedure TAccountchildLink.Sethref(AIndex : Integer; AValue : string); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountchildLink.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAccountchildLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAccountpermissions
  --------------------------------------------------------------------}


Procedure TAccountpermissions.Seteffective(AIndex : Integer; AValue : TAccountpermissionseffective); 

begin
  If (Feffective=AValue) then exit;
  Feffective:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountpermissionseffective
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountRef
  --------------------------------------------------------------------}


Procedure TAccountRef.Sethref(AIndex : Integer; AValue : string); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountRef.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountRef.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountRef.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountSummaries
  --------------------------------------------------------------------}


Procedure TAccountSummaries.Setitems(AIndex : Integer; AValue : TAccountSummariesitems); 

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



Procedure TAccountSummaries.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountSummaries.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountSummaries.SetpreviousLink(AIndex : Integer; AValue : string); 

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



Procedure TAccountSummaries.Setusername(AIndex : Integer; AValue : string); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountSummariesitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountSummary
  --------------------------------------------------------------------}


Procedure TAccountSummary.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountSummary.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountSummary.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountSummary.SetwebProperties(AIndex : Integer; AValue : TAccountSummarywebProperties); 

begin
  If (FwebProperties=AValue) then exit;
  FwebProperties:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountSummarywebProperties
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountTicket
  --------------------------------------------------------------------}


Procedure TAccountTicket.Setaccount(AIndex : Integer; AValue : TAccount); 

begin
  If (Faccount=AValue) then exit;
  Faccount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTicket.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccountTicket.Setkind(AIndex : Integer; AValue : string); 

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



Procedure TAccountTicket.SetredirectUri(AIndex : Integer; AValue : string); 

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


Procedure TAccounts.Setitems(AIndex : Integer; AValue : TAccountsitems); 

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



Procedure TAccounts.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounts.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounts.SetpreviousLink(AIndex : Integer; AValue : string); 

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



Procedure TAccounts.Setusername(AIndex : Integer; AValue : string); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAdWordsAccount
  --------------------------------------------------------------------}


Procedure TAdWordsAccount.SetautoTaggingEnabled(AIndex : Integer; AValue : boolean); 

begin
  If (FautoTaggingEnabled=AValue) then exit;
  FautoTaggingEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdWordsAccount.SetcustomerId(AIndex : Integer; AValue : string); 

begin
  If (FcustomerId=AValue) then exit;
  FcustomerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdWordsAccount.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnalyticsDataimportDeleteUploadDataRequest
  --------------------------------------------------------------------}


Procedure TAnalyticsDataimportDeleteUploadDataRequest.SetcustomDataImportUids(AIndex : Integer; AValue : TAnalyticsDataimportDeleteUploadDataRequestcustomDataImportUids); 

begin
  If (FcustomDataImportUids=AValue) then exit;
  FcustomDataImportUids:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnalyticsDataimportDeleteUploadDataRequestcustomDataImportUids
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TColumn
  --------------------------------------------------------------------}


Procedure TColumn.Setattributes(AIndex : Integer; AValue : TColumnattributes); 

begin
  If (Fattributes=AValue) then exit;
  Fattributes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumn.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TColumnattributes
  --------------------------------------------------------------------}


Class Function TColumnattributes.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TColumns
  --------------------------------------------------------------------}


Procedure TColumns.SetattributeNames(AIndex : Integer; AValue : TColumnsattributeNames); 

begin
  If (FattributeNames=AValue) then exit;
  FattributeNames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumns.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumns.Setitems(AIndex : Integer; AValue : TColumnsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumns.Setkind(AIndex : Integer; AValue : string); 

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
  TColumnsattributeNames
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TColumnsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCustomDataSource
  --------------------------------------------------------------------}


Procedure TCustomDataSource.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.SetchildLink(AIndex : Integer; AValue : TCustomDataSourcechildLink); 

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



Procedure TCustomDataSource.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.SetimportBehavior(AIndex : Integer; AValue : string); 

begin
  If (FimportBehavior=AValue) then exit;
  FimportBehavior:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.SetparentLink(AIndex : Integer; AValue : TCustomDataSourceparentLink); 

begin
  If (FparentLink=AValue) then exit;
  FparentLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.SetprofilesLinked(AIndex : Integer; AValue : TCustomDataSourceprofilesLinked); 

begin
  If (FprofilesLinked=AValue) then exit;
  FprofilesLinked:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.Set_type(AIndex : Integer; AValue : string); 

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



Procedure TCustomDataSource.SetuploadType(AIndex : Integer; AValue : string); 

begin
  If (FuploadType=AValue) then exit;
  FuploadType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSource.SetwebPropertyId(AIndex : Integer; AValue : string); 

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
  TCustomDataSourcechildLink
  --------------------------------------------------------------------}


Procedure TCustomDataSourcechildLink.Sethref(AIndex : Integer; AValue : string); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSourcechildLink.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCustomDataSourcechildLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TCustomDataSourceparentLink
  --------------------------------------------------------------------}


Procedure TCustomDataSourceparentLink.Sethref(AIndex : Integer; AValue : string); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSourceparentLink.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCustomDataSourceparentLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TCustomDataSourceprofilesLinked
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCustomDataSources
  --------------------------------------------------------------------}


Procedure TCustomDataSources.Setitems(AIndex : Integer; AValue : TCustomDataSourcesitems); 

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



Procedure TCustomDataSources.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSources.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDataSources.SetpreviousLink(AIndex : Integer; AValue : string); 

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



Procedure TCustomDataSources.Setusername(AIndex : Integer; AValue : string); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCustomDataSourcesitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCustomDimension
  --------------------------------------------------------------------}


Procedure TCustomDimension.SetaccountId(AIndex : Integer; AValue : string); 

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



Procedure TCustomDimension.Setid(AIndex : Integer; AValue : string); 

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



Procedure TCustomDimension.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimension.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimension.SetparentLink(AIndex : Integer; AValue : TCustomDimensionparentLink); 

begin
  If (FparentLink=AValue) then exit;
  FparentLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimension.Setscope(AIndex : Integer; AValue : string); 

begin
  If (Fscope=AValue) then exit;
  Fscope:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimension.SetselfLink(AIndex : Integer; AValue : string); 

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



Procedure TCustomDimension.SetwebPropertyId(AIndex : Integer; AValue : string); 

begin
  If (FwebPropertyId=AValue) then exit;
  FwebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCustomDimensionparentLink
  --------------------------------------------------------------------}


Procedure TCustomDimensionparentLink.Sethref(AIndex : Integer; AValue : string); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimensionparentLink.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCustomDimensionparentLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TCustomDimensions
  --------------------------------------------------------------------}


Procedure TCustomDimensions.Setitems(AIndex : Integer; AValue : TCustomDimensionsitems); 

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



Procedure TCustomDimensions.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimensions.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomDimensions.SetpreviousLink(AIndex : Integer; AValue : string); 

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



Procedure TCustomDimensions.Setusername(AIndex : Integer; AValue : string); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCustomDimensionsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCustomMetric
  --------------------------------------------------------------------}


Procedure TCustomMetric.SetaccountId(AIndex : Integer; AValue : string); 

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



Procedure TCustomMetric.Setid(AIndex : Integer; AValue : string); 

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



Procedure TCustomMetric.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetric.Setmax_value(AIndex : Integer; AValue : string); 

begin
  If (Fmax_value=AValue) then exit;
  Fmax_value:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetric.Setmin_value(AIndex : Integer; AValue : string); 

begin
  If (Fmin_value=AValue) then exit;
  Fmin_value:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetric.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetric.SetparentLink(AIndex : Integer; AValue : TCustomMetricparentLink); 

begin
  If (FparentLink=AValue) then exit;
  FparentLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetric.Setscope(AIndex : Integer; AValue : string); 

begin
  If (Fscope=AValue) then exit;
  Fscope:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetric.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetric.Set_type(AIndex : Integer; AValue : string); 

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



Procedure TCustomMetric.SetwebPropertyId(AIndex : Integer; AValue : string); 

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
  TCustomMetricparentLink
  --------------------------------------------------------------------}


Procedure TCustomMetricparentLink.Sethref(AIndex : Integer; AValue : string); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetricparentLink.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCustomMetricparentLink.ExportPropertyName(Const AName : String) :String;

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


Procedure TCustomMetrics.Setitems(AIndex : Integer; AValue : TCustomMetricsitems); 

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



Procedure TCustomMetrics.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetrics.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomMetrics.SetpreviousLink(AIndex : Integer; AValue : string); 

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



Procedure TCustomMetrics.Setusername(AIndex : Integer; AValue : string); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCustomMetricsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TEntityAdWordsLink
  --------------------------------------------------------------------}


Procedure TEntityAdWordsLink.SetadWordsAccounts(AIndex : Integer; AValue : TEntityAdWordsLinkadWordsAccounts); 

begin
  If (FadWordsAccounts=AValue) then exit;
  FadWordsAccounts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAdWordsLink.Setentity(AIndex : Integer; AValue : TEntityAdWordsLinkentity); 

begin
  If (Fentity=AValue) then exit;
  Fentity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAdWordsLink.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAdWordsLink.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAdWordsLink.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAdWordsLink.SetprofileIds(AIndex : Integer; AValue : TEntityAdWordsLinkprofileIds); 

begin
  If (FprofileIds=AValue) then exit;
  FprofileIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAdWordsLink.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEntityAdWordsLinkadWordsAccounts
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TEntityAdWordsLinkentity
  --------------------------------------------------------------------}


Procedure TEntityAdWordsLinkentity.SetwebPropertyRef(AIndex : Integer; AValue : TWebPropertyRef); 

begin
  If (FwebPropertyRef=AValue) then exit;
  FwebPropertyRef:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEntityAdWordsLinkprofileIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TEntityAdWordsLinks
  --------------------------------------------------------------------}


Procedure TEntityAdWordsLinks.Setitems(AIndex : Integer; AValue : TEntityAdWordsLinksitems); 

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



Procedure TEntityAdWordsLinks.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAdWordsLinks.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityAdWordsLinks.SetpreviousLink(AIndex : Integer; AValue : string); 

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
  TEntityAdWordsLinksitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TEntityUserLink
  --------------------------------------------------------------------}


Procedure TEntityUserLink.Setentity(AIndex : Integer; AValue : TEntityUserLinkentity); 

begin
  If (Fentity=AValue) then exit;
  Fentity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLink.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLink.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLink.Setpermissions(AIndex : Integer; AValue : TEntityUserLinkpermissions); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLink.SetselfLink(AIndex : Integer; AValue : string); 

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
  TEntityUserLinkentity
  --------------------------------------------------------------------}


Procedure TEntityUserLinkentity.SetaccountRef(AIndex : Integer; AValue : TAccountRef); 

begin
  If (FaccountRef=AValue) then exit;
  FaccountRef:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLinkentity.SetprofileRef(AIndex : Integer; AValue : TProfileRef); 

begin
  If (FprofileRef=AValue) then exit;
  FprofileRef:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLinkentity.SetwebPropertyRef(AIndex : Integer; AValue : TWebPropertyRef); 

begin
  If (FwebPropertyRef=AValue) then exit;
  FwebPropertyRef:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEntityUserLinkpermissions
  --------------------------------------------------------------------}


Procedure TEntityUserLinkpermissions.Seteffective(AIndex : Integer; AValue : TEntityUserLinkpermissionseffective); 

begin
  If (Feffective=AValue) then exit;
  Feffective:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLinkpermissions.Setlocal(AIndex : Integer; AValue : TEntityUserLinkpermissionslocal); 

begin
  If (Flocal=AValue) then exit;
  Flocal:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEntityUserLinkpermissionseffective
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TEntityUserLinkpermissionslocal
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TEntityUserLinks
  --------------------------------------------------------------------}


Procedure TEntityUserLinks.Setitems(AIndex : Integer; AValue : TEntityUserLinksitems); 

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



Procedure TEntityUserLinks.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLinks.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityUserLinks.SetpreviousLink(AIndex : Integer; AValue : string); 

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
  TEntityUserLinksitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TExperiment
  --------------------------------------------------------------------}


Procedure TExperiment.SetaccountId(AIndex : Integer; AValue : string); 

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



Procedure TExperiment.Setdescription(AIndex : Integer; AValue : string); 

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



Procedure TExperiment.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetinternalWebPropertyId(AIndex : Integer; AValue : string); 

begin
  If (FinternalWebPropertyId=AValue) then exit;
  FinternalWebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.Setkind(AIndex : Integer; AValue : string); 

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



Procedure TExperiment.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetobjectiveMetric(AIndex : Integer; AValue : string); 

begin
  If (FobjectiveMetric=AValue) then exit;
  FobjectiveMetric:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetoptimizationType(AIndex : Integer; AValue : string); 

begin
  If (FoptimizationType=AValue) then exit;
  FoptimizationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetparentLink(AIndex : Integer; AValue : TExperimentparentLink); 

begin
  If (FparentLink=AValue) then exit;
  FparentLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetprofileId(AIndex : Integer; AValue : string); 

begin
  If (FprofileId=AValue) then exit;
  FprofileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetreasonExperimentEnded(AIndex : Integer; AValue : string); 

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



Procedure TExperiment.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetservingFramework(AIndex : Integer; AValue : string); 

begin
  If (FservingFramework=AValue) then exit;
  FservingFramework:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.Setsnippet(AIndex : Integer; AValue : string); 

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



Procedure TExperiment.Setstatus(AIndex : Integer; AValue : string); 

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



Procedure TExperiment.Setvariations(AIndex : Integer; AValue : TExperimentvariations); 

begin
  If (Fvariations=AValue) then exit;
  Fvariations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetwebPropertyId(AIndex : Integer; AValue : string); 

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
  TExperimentparentLink
  --------------------------------------------------------------------}


Procedure TExperimentparentLink.Sethref(AIndex : Integer; AValue : string); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperimentparentLink.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TExperimentparentLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TExperimentvariations
  --------------------------------------------------------------------}


Procedure TExperimentvariations.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperimentvariations.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperimentvariations.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperimentvariations.Setweight(AIndex : Integer; AValue : double); 

begin
  If (Fweight=AValue) then exit;
  Fweight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperimentvariations.Setwon(AIndex : Integer; AValue : boolean); 

begin
  If (Fwon=AValue) then exit;
  Fwon:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExperiments
  --------------------------------------------------------------------}


Procedure TExperiments.Setitems(AIndex : Integer; AValue : TExperimentsitems); 

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



Procedure TExperiments.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiments.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiments.SetpreviousLink(AIndex : Integer; AValue : string); 

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



Procedure TExperiments.Setusername(AIndex : Integer; AValue : string); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExperimentsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFilter
  --------------------------------------------------------------------}


Procedure TFilter.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.SetadvancedDetails(AIndex : Integer; AValue : TFilteradvancedDetails); 

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



Procedure TFilter.Setid(AIndex : Integer; AValue : string); 

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



Procedure TFilter.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.SetlowercaseDetails(AIndex : Integer; AValue : TFilterlowercaseDetails); 

begin
  If (FlowercaseDetails=AValue) then exit;
  FlowercaseDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.SetparentLink(AIndex : Integer; AValue : TFilterparentLink); 

begin
  If (FparentLink=AValue) then exit;
  FparentLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.SetsearchAndReplaceDetails(AIndex : Integer; AValue : TFiltersearchAndReplaceDetails); 

begin
  If (FsearchAndReplaceDetails=AValue) then exit;
  FsearchAndReplaceDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.Set_type(AIndex : Integer; AValue : string); 

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



Procedure TFilter.SetuppercaseDetails(AIndex : Integer; AValue : TFilteruppercaseDetails); 

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
  TFilteradvancedDetails
  --------------------------------------------------------------------}


Procedure TFilteradvancedDetails.SetcaseSensitive(AIndex : Integer; AValue : boolean); 

begin
  If (FcaseSensitive=AValue) then exit;
  FcaseSensitive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilteradvancedDetails.SetextractA(AIndex : Integer; AValue : string); 

begin
  If (FextractA=AValue) then exit;
  FextractA:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilteradvancedDetails.SetextractB(AIndex : Integer; AValue : string); 

begin
  If (FextractB=AValue) then exit;
  FextractB:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilteradvancedDetails.SetfieldA(AIndex : Integer; AValue : string); 

begin
  If (FfieldA=AValue) then exit;
  FfieldA:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilteradvancedDetails.SetfieldARequired(AIndex : Integer; AValue : boolean); 

begin
  If (FfieldARequired=AValue) then exit;
  FfieldARequired:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilteradvancedDetails.SetfieldB(AIndex : Integer; AValue : string); 

begin
  If (FfieldB=AValue) then exit;
  FfieldB:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilteradvancedDetails.SetfieldBRequired(AIndex : Integer; AValue : boolean); 

begin
  If (FfieldBRequired=AValue) then exit;
  FfieldBRequired:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilteradvancedDetails.SetoutputConstructor(AIndex : Integer; AValue : string); 

begin
  If (FoutputConstructor=AValue) then exit;
  FoutputConstructor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilteradvancedDetails.SetoutputToField(AIndex : Integer; AValue : string); 

begin
  If (FoutputToField=AValue) then exit;
  FoutputToField:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilteradvancedDetails.SetoverrideOutputField(AIndex : Integer; AValue : boolean); 

begin
  If (FoverrideOutputField=AValue) then exit;
  FoverrideOutputField:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFilterlowercaseDetails
  --------------------------------------------------------------------}


Procedure TFilterlowercaseDetails.Setfield(AIndex : Integer; AValue : string); 

begin
  If (Ffield=AValue) then exit;
  Ffield:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFilterparentLink
  --------------------------------------------------------------------}


Procedure TFilterparentLink.Sethref(AIndex : Integer; AValue : string); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterparentLink.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TFilterparentLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TFiltersearchAndReplaceDetails
  --------------------------------------------------------------------}


Procedure TFiltersearchAndReplaceDetails.SetcaseSensitive(AIndex : Integer; AValue : boolean); 

begin
  If (FcaseSensitive=AValue) then exit;
  FcaseSensitive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFiltersearchAndReplaceDetails.Setfield(AIndex : Integer; AValue : string); 

begin
  If (Ffield=AValue) then exit;
  Ffield:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFiltersearchAndReplaceDetails.SetreplaceString(AIndex : Integer; AValue : string); 

begin
  If (FreplaceString=AValue) then exit;
  FreplaceString:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFiltersearchAndReplaceDetails.SetsearchString(AIndex : Integer; AValue : string); 

begin
  If (FsearchString=AValue) then exit;
  FsearchString:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFilteruppercaseDetails
  --------------------------------------------------------------------}


Procedure TFilteruppercaseDetails.Setfield(AIndex : Integer; AValue : string); 

begin
  If (Ffield=AValue) then exit;
  Ffield:=AValue;
  MarkPropertyChanged(AIndex);
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



Procedure TFilterExpression.SetexpressionValue(AIndex : Integer; AValue : string); 

begin
  If (FexpressionValue=AValue) then exit;
  FexpressionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterExpression.Setfield(AIndex : Integer; AValue : string); 

begin
  If (Ffield=AValue) then exit;
  Ffield:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterExpression.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterExpression.SetmatchType(AIndex : Integer; AValue : string); 

begin
  If (FmatchType=AValue) then exit;
  FmatchType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFilterRef
  --------------------------------------------------------------------}


Procedure TFilterRef.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterRef.Sethref(AIndex : Integer; AValue : string); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterRef.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterRef.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterRef.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFilters
  --------------------------------------------------------------------}


Procedure TFilters.Setitems(AIndex : Integer; AValue : TFiltersitems); 

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



Procedure TFilters.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilters.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilters.SetpreviousLink(AIndex : Integer; AValue : string); 

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



Procedure TFilters.Setusername(AIndex : Integer; AValue : string); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFiltersitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGaData
  --------------------------------------------------------------------}


Procedure TGaData.SetcolumnHeaders(AIndex : Integer; AValue : TGaDatacolumnHeaders); 

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



Procedure TGaData.SetdataTable(AIndex : Integer; AValue : TGaDatadataTable); 

begin
  If (FdataTable=AValue) then exit;
  FdataTable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.Setid(AIndex : Integer; AValue : string); 

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



Procedure TGaData.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.SetpreviousLink(AIndex : Integer; AValue : string); 

begin
  If (FpreviousLink=AValue) then exit;
  FpreviousLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.SetprofileInfo(AIndex : Integer; AValue : TGaDataprofileInfo); 

begin
  If (FprofileInfo=AValue) then exit;
  FprofileInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.Setquery(AIndex : Integer; AValue : TGaDataquery); 

begin
  If (Fquery=AValue) then exit;
  Fquery:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.Setrows(AIndex : Integer; AValue : TGaDatarows); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.SetsampleSize(AIndex : Integer; AValue : string); 

begin
  If (FsampleSize=AValue) then exit;
  FsampleSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.SetsampleSpace(AIndex : Integer; AValue : string); 

begin
  If (FsampleSpace=AValue) then exit;
  FsampleSpace:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaData.SetselfLink(AIndex : Integer; AValue : string); 

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



Procedure TGaData.SettotalsForAllResults(AIndex : Integer; AValue : TGaDatatotalsForAllResults); 

begin
  If (FtotalsForAllResults=AValue) then exit;
  FtotalsForAllResults:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGaDatacolumnHeaders
  --------------------------------------------------------------------}


Procedure TGaDatacolumnHeaders.SetcolumnType(AIndex : Integer; AValue : string); 

begin
  If (FcolumnType=AValue) then exit;
  FcolumnType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDatacolumnHeaders.SetdataType(AIndex : Integer; AValue : string); 

begin
  If (FdataType=AValue) then exit;
  FdataType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDatacolumnHeaders.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGaDatadataTable
  --------------------------------------------------------------------}


Procedure TGaDatadataTable.Setcols(AIndex : Integer; AValue : TGaDatadataTablecols); 

begin
  If (Fcols=AValue) then exit;
  Fcols:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDatadataTable.Setrows(AIndex : Integer; AValue : TGaDatadataTablerows); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGaDatadataTablecols
  --------------------------------------------------------------------}


Procedure TGaDatadataTablecols.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDatadataTablecols.Set_label(AIndex : Integer; AValue : string); 

begin
  If (F_label=AValue) then exit;
  F_label:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDatadataTablecols.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TGaDatadataTablecols.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_label' : Result:='label';
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TGaDatadataTablerows
  --------------------------------------------------------------------}


Procedure TGaDatadataTablerows.Setc(AIndex : Integer; AValue : TGaDatadataTablerowsc); 

begin
  If (Fc=AValue) then exit;
  Fc:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGaDatadataTablerowsc
  --------------------------------------------------------------------}


Procedure TGaDatadataTablerowsc.Setv(AIndex : Integer; AValue : string); 

begin
  If (Fv=AValue) then exit;
  Fv:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGaDataprofileInfo
  --------------------------------------------------------------------}


Procedure TGaDataprofileInfo.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataprofileInfo.SetinternalWebPropertyId(AIndex : Integer; AValue : string); 

begin
  If (FinternalWebPropertyId=AValue) then exit;
  FinternalWebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataprofileInfo.SetprofileId(AIndex : Integer; AValue : string); 

begin
  If (FprofileId=AValue) then exit;
  FprofileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataprofileInfo.SetprofileName(AIndex : Integer; AValue : string); 

begin
  If (FprofileName=AValue) then exit;
  FprofileName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataprofileInfo.SettableId(AIndex : Integer; AValue : string); 

begin
  If (FtableId=AValue) then exit;
  FtableId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataprofileInfo.SetwebPropertyId(AIndex : Integer; AValue : string); 

begin
  If (FwebPropertyId=AValue) then exit;
  FwebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGaDataquery
  --------------------------------------------------------------------}


Procedure TGaDataquery.Setdimensions(AIndex : Integer; AValue : string); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataquery.Setenddate(AIndex : Integer; AValue : string); 

begin
  If (Fenddate=AValue) then exit;
  Fenddate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataquery.Setfilters(AIndex : Integer; AValue : string); 

begin
  If (Ffilters=AValue) then exit;
  Ffilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataquery.Setids(AIndex : Integer; AValue : string); 

begin
  If (Fids=AValue) then exit;
  Fids:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataquery.Setmaxresults(AIndex : Integer; AValue : integer); 

begin
  If (Fmaxresults=AValue) then exit;
  Fmaxresults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataquery.Setmetrics(AIndex : Integer; AValue : TGaDataquerymetrics); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataquery.SetsamplingLevel(AIndex : Integer; AValue : string); 

begin
  If (FsamplingLevel=AValue) then exit;
  FsamplingLevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataquery.Setsegment(AIndex : Integer; AValue : string); 

begin
  If (Fsegment=AValue) then exit;
  Fsegment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataquery.Setsort(AIndex : Integer; AValue : TGaDataquerysort); 

begin
  If (Fsort=AValue) then exit;
  Fsort:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataquery.Setstartdate(AIndex : Integer; AValue : string); 

begin
  If (Fstartdate=AValue) then exit;
  Fstartdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGaDataquery.Setstartindex(AIndex : Integer; AValue : integer); 

begin
  If (Fstartindex=AValue) then exit;
  Fstartindex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TGaDataquery.ExportPropertyName(Const AName : String) :String;

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
  TGaDataquerymetrics
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGaDataquerysort
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGaDatarows
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGaDatatotalsForAllResults
  --------------------------------------------------------------------}


Class Function TGaDatatotalsForAllResults.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TGoal
  --------------------------------------------------------------------}


Procedure TGoal.SetaccountId(AIndex : Integer; AValue : string); 

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



Procedure TGoal.SeteventDetails(AIndex : Integer; AValue : TGoaleventDetails); 

begin
  If (FeventDetails=AValue) then exit;
  FeventDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.SetinternalWebPropertyId(AIndex : Integer; AValue : string); 

begin
  If (FinternalWebPropertyId=AValue) then exit;
  FinternalWebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.SetparentLink(AIndex : Integer; AValue : TGoalparentLink); 

begin
  If (FparentLink=AValue) then exit;
  FparentLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.SetprofileId(AIndex : Integer; AValue : string); 

begin
  If (FprofileId=AValue) then exit;
  FprofileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.Set_type(AIndex : Integer; AValue : string); 

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



Procedure TGoal.SeturlDestinationDetails(AIndex : Integer; AValue : TGoalurlDestinationDetails); 

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



Procedure TGoal.SetvisitNumPagesDetails(AIndex : Integer; AValue : TGoalvisitNumPagesDetails); 

begin
  If (FvisitNumPagesDetails=AValue) then exit;
  FvisitNumPagesDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.SetvisitTimeOnSiteDetails(AIndex : Integer; AValue : TGoalvisitTimeOnSiteDetails); 

begin
  If (FvisitTimeOnSiteDetails=AValue) then exit;
  FvisitTimeOnSiteDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoal.SetwebPropertyId(AIndex : Integer; AValue : string); 

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
  TGoaleventDetails
  --------------------------------------------------------------------}


Procedure TGoaleventDetails.SeteventConditions(AIndex : Integer; AValue : TGoaleventDetailseventConditions); 

begin
  If (FeventConditions=AValue) then exit;
  FeventConditions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoaleventDetails.SetuseEventValue(AIndex : Integer; AValue : boolean); 

begin
  If (FuseEventValue=AValue) then exit;
  FuseEventValue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGoaleventDetailseventConditions
  --------------------------------------------------------------------}


Procedure TGoaleventDetailseventConditions.SetcomparisonType(AIndex : Integer; AValue : string); 

begin
  If (FcomparisonType=AValue) then exit;
  FcomparisonType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoaleventDetailseventConditions.SetcomparisonValue(AIndex : Integer; AValue : string); 

begin
  If (FcomparisonValue=AValue) then exit;
  FcomparisonValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoaleventDetailseventConditions.Setexpression(AIndex : Integer; AValue : string); 

begin
  If (Fexpression=AValue) then exit;
  Fexpression:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoaleventDetailseventConditions.SetmatchType(AIndex : Integer; AValue : string); 

begin
  If (FmatchType=AValue) then exit;
  FmatchType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoaleventDetailseventConditions.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TGoaleventDetailseventConditions.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TGoalparentLink
  --------------------------------------------------------------------}


Procedure TGoalparentLink.Sethref(AIndex : Integer; AValue : string); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalparentLink.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TGoalparentLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TGoalurlDestinationDetails
  --------------------------------------------------------------------}


Procedure TGoalurlDestinationDetails.SetcaseSensitive(AIndex : Integer; AValue : boolean); 

begin
  If (FcaseSensitive=AValue) then exit;
  FcaseSensitive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalurlDestinationDetails.SetfirstStepRequired(AIndex : Integer; AValue : boolean); 

begin
  If (FfirstStepRequired=AValue) then exit;
  FfirstStepRequired:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalurlDestinationDetails.SetmatchType(AIndex : Integer; AValue : string); 

begin
  If (FmatchType=AValue) then exit;
  FmatchType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalurlDestinationDetails.Setsteps(AIndex : Integer; AValue : TGoalurlDestinationDetailssteps); 

begin
  If (Fsteps=AValue) then exit;
  Fsteps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalurlDestinationDetails.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGoalurlDestinationDetailssteps
  --------------------------------------------------------------------}


Procedure TGoalurlDestinationDetailssteps.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalurlDestinationDetailssteps.Setnumber(AIndex : Integer; AValue : integer); 

begin
  If (Fnumber=AValue) then exit;
  Fnumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalurlDestinationDetailssteps.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGoalvisitNumPagesDetails
  --------------------------------------------------------------------}


Procedure TGoalvisitNumPagesDetails.SetcomparisonType(AIndex : Integer; AValue : string); 

begin
  If (FcomparisonType=AValue) then exit;
  FcomparisonType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalvisitNumPagesDetails.SetcomparisonValue(AIndex : Integer; AValue : string); 

begin
  If (FcomparisonValue=AValue) then exit;
  FcomparisonValue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGoalvisitTimeOnSiteDetails
  --------------------------------------------------------------------}


Procedure TGoalvisitTimeOnSiteDetails.SetcomparisonType(AIndex : Integer; AValue : string); 

begin
  If (FcomparisonType=AValue) then exit;
  FcomparisonType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoalvisitTimeOnSiteDetails.SetcomparisonValue(AIndex : Integer; AValue : string); 

begin
  If (FcomparisonValue=AValue) then exit;
  FcomparisonValue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGoals
  --------------------------------------------------------------------}


Procedure TGoals.Setitems(AIndex : Integer; AValue : TGoalsitems); 

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



Procedure TGoals.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoals.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGoals.SetpreviousLink(AIndex : Integer; AValue : string); 

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



Procedure TGoals.Setusername(AIndex : Integer; AValue : string); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGoalsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMcfData
  --------------------------------------------------------------------}


Procedure TMcfData.SetcolumnHeaders(AIndex : Integer; AValue : TMcfDatacolumnHeaders); 

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



Procedure TMcfData.Setid(AIndex : Integer; AValue : string); 

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



Procedure TMcfData.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfData.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfData.SetpreviousLink(AIndex : Integer; AValue : string); 

begin
  If (FpreviousLink=AValue) then exit;
  FpreviousLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfData.SetprofileInfo(AIndex : Integer; AValue : TMcfDataprofileInfo); 

begin
  If (FprofileInfo=AValue) then exit;
  FprofileInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfData.Setquery(AIndex : Integer; AValue : TMcfDataquery); 

begin
  If (Fquery=AValue) then exit;
  Fquery:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfData.Setrows(AIndex : Integer; AValue : TMcfDatarows); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfData.SetsampleSize(AIndex : Integer; AValue : string); 

begin
  If (FsampleSize=AValue) then exit;
  FsampleSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfData.SetsampleSpace(AIndex : Integer; AValue : string); 

begin
  If (FsampleSpace=AValue) then exit;
  FsampleSpace:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfData.SetselfLink(AIndex : Integer; AValue : string); 

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



Procedure TMcfData.SettotalsForAllResults(AIndex : Integer; AValue : TMcfDatatotalsForAllResults); 

begin
  If (FtotalsForAllResults=AValue) then exit;
  FtotalsForAllResults:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMcfDatacolumnHeaders
  --------------------------------------------------------------------}


Procedure TMcfDatacolumnHeaders.SetcolumnType(AIndex : Integer; AValue : string); 

begin
  If (FcolumnType=AValue) then exit;
  FcolumnType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDatacolumnHeaders.SetdataType(AIndex : Integer; AValue : string); 

begin
  If (FdataType=AValue) then exit;
  FdataType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDatacolumnHeaders.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMcfDataprofileInfo
  --------------------------------------------------------------------}


Procedure TMcfDataprofileInfo.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataprofileInfo.SetinternalWebPropertyId(AIndex : Integer; AValue : string); 

begin
  If (FinternalWebPropertyId=AValue) then exit;
  FinternalWebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataprofileInfo.SetprofileId(AIndex : Integer; AValue : string); 

begin
  If (FprofileId=AValue) then exit;
  FprofileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataprofileInfo.SetprofileName(AIndex : Integer; AValue : string); 

begin
  If (FprofileName=AValue) then exit;
  FprofileName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataprofileInfo.SettableId(AIndex : Integer; AValue : string); 

begin
  If (FtableId=AValue) then exit;
  FtableId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataprofileInfo.SetwebPropertyId(AIndex : Integer; AValue : string); 

begin
  If (FwebPropertyId=AValue) then exit;
  FwebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMcfDataquery
  --------------------------------------------------------------------}


Procedure TMcfDataquery.Setdimensions(AIndex : Integer; AValue : string); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataquery.Setenddate(AIndex : Integer; AValue : string); 

begin
  If (Fenddate=AValue) then exit;
  Fenddate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataquery.Setfilters(AIndex : Integer; AValue : string); 

begin
  If (Ffilters=AValue) then exit;
  Ffilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataquery.Setids(AIndex : Integer; AValue : string); 

begin
  If (Fids=AValue) then exit;
  Fids:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataquery.Setmaxresults(AIndex : Integer; AValue : integer); 

begin
  If (Fmaxresults=AValue) then exit;
  Fmaxresults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataquery.Setmetrics(AIndex : Integer; AValue : TMcfDataquerymetrics); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataquery.SetsamplingLevel(AIndex : Integer; AValue : string); 

begin
  If (FsamplingLevel=AValue) then exit;
  FsamplingLevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataquery.Setsegment(AIndex : Integer; AValue : string); 

begin
  If (Fsegment=AValue) then exit;
  Fsegment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataquery.Setsort(AIndex : Integer; AValue : TMcfDataquerysort); 

begin
  If (Fsort=AValue) then exit;
  Fsort:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataquery.Setstartdate(AIndex : Integer; AValue : string); 

begin
  If (Fstartdate=AValue) then exit;
  Fstartdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMcfDataquery.Setstartindex(AIndex : Integer; AValue : integer); 

begin
  If (Fstartindex=AValue) then exit;
  Fstartindex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TMcfDataquery.ExportPropertyName(Const AName : String) :String;

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
  TMcfDataquerymetrics
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMcfDataquerysort
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMcfDatarows
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMcfDatatotalsForAllResults
  --------------------------------------------------------------------}


Class Function TMcfDatatotalsForAllResults.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TProfile
  --------------------------------------------------------------------}


Procedure TProfile.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetchildLink(AIndex : Integer; AValue : TProfilechildLink); 

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



Procedure TProfile.Setcurrency(AIndex : Integer; AValue : string); 

begin
  If (Fcurrency=AValue) then exit;
  Fcurrency:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetdefaultPage(AIndex : Integer; AValue : string); 

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



Procedure TProfile.SetexcludeQueryParameters(AIndex : Integer; AValue : string); 

begin
  If (FexcludeQueryParameters=AValue) then exit;
  FexcludeQueryParameters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetinternalWebPropertyId(AIndex : Integer; AValue : string); 

begin
  If (FinternalWebPropertyId=AValue) then exit;
  FinternalWebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetparentLink(AIndex : Integer; AValue : TProfileparentLink); 

begin
  If (FparentLink=AValue) then exit;
  FparentLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.Setpermissions(AIndex : Integer; AValue : TProfilepermissions); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetsiteSearchCategoryParameters(AIndex : Integer; AValue : string); 

begin
  If (FsiteSearchCategoryParameters=AValue) then exit;
  FsiteSearchCategoryParameters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetsiteSearchQueryParameters(AIndex : Integer; AValue : string); 

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



Procedure TProfile.Settimezone(AIndex : Integer; AValue : string); 

begin
  If (Ftimezone=AValue) then exit;
  Ftimezone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.Set_type(AIndex : Integer; AValue : string); 

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



Procedure TProfile.SetwebPropertyId(AIndex : Integer; AValue : string); 

begin
  If (FwebPropertyId=AValue) then exit;
  FwebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetwebsiteUrl(AIndex : Integer; AValue : string); 

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
  TProfilechildLink
  --------------------------------------------------------------------}


Procedure TProfilechildLink.Sethref(AIndex : Integer; AValue : string); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfilechildLink.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TProfilechildLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TProfileparentLink
  --------------------------------------------------------------------}


Procedure TProfileparentLink.Sethref(AIndex : Integer; AValue : string); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileparentLink.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TProfileparentLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TProfilepermissions
  --------------------------------------------------------------------}


Procedure TProfilepermissions.Seteffective(AIndex : Integer; AValue : TProfilepermissionseffective); 

begin
  If (Feffective=AValue) then exit;
  Feffective:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProfilepermissionseffective
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProfileFilterLink
  --------------------------------------------------------------------}


Procedure TProfileFilterLink.SetfilterRef(AIndex : Integer; AValue : TFilterRef); 

begin
  If (FfilterRef=AValue) then exit;
  FfilterRef:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileFilterLink.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileFilterLink.Setkind(AIndex : Integer; AValue : string); 

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



Procedure TProfileFilterLink.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProfileFilterLinks
  --------------------------------------------------------------------}


Procedure TProfileFilterLinks.Setitems(AIndex : Integer; AValue : TProfileFilterLinksitems); 

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



Procedure TProfileFilterLinks.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileFilterLinks.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileFilterLinks.SetpreviousLink(AIndex : Integer; AValue : string); 

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



Procedure TProfileFilterLinks.Setusername(AIndex : Integer; AValue : string); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProfileFilterLinksitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProfileRef
  --------------------------------------------------------------------}


Procedure TProfileRef.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileRef.Sethref(AIndex : Integer; AValue : string); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileRef.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileRef.SetinternalWebPropertyId(AIndex : Integer; AValue : string); 

begin
  If (FinternalWebPropertyId=AValue) then exit;
  FinternalWebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileRef.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileRef.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileRef.SetwebPropertyId(AIndex : Integer; AValue : string); 

begin
  If (FwebPropertyId=AValue) then exit;
  FwebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProfileSummary
  --------------------------------------------------------------------}


Procedure TProfileSummary.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileSummary.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileSummary.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfileSummary.Set_type(AIndex : Integer; AValue : string); 

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


Procedure TProfiles.Setitems(AIndex : Integer; AValue : TProfilesitems); 

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



Procedure TProfiles.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfiles.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfiles.SetpreviousLink(AIndex : Integer; AValue : string); 

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



Procedure TProfiles.Setusername(AIndex : Integer; AValue : string); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProfilesitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRealtimeData
  --------------------------------------------------------------------}


Procedure TRealtimeData.SetcolumnHeaders(AIndex : Integer; AValue : TRealtimeDatacolumnHeaders); 

begin
  If (FcolumnHeaders=AValue) then exit;
  FcolumnHeaders:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeData.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeData.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeData.SetprofileInfo(AIndex : Integer; AValue : TRealtimeDataprofileInfo); 

begin
  If (FprofileInfo=AValue) then exit;
  FprofileInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeData.Setquery(AIndex : Integer; AValue : TRealtimeDataquery); 

begin
  If (Fquery=AValue) then exit;
  Fquery:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeData.Setrows(AIndex : Integer; AValue : TRealtimeDatarows); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeData.SetselfLink(AIndex : Integer; AValue : string); 

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



Procedure TRealtimeData.SettotalsForAllResults(AIndex : Integer; AValue : TRealtimeDatatotalsForAllResults); 

begin
  If (FtotalsForAllResults=AValue) then exit;
  FtotalsForAllResults:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRealtimeDatacolumnHeaders
  --------------------------------------------------------------------}


Procedure TRealtimeDatacolumnHeaders.SetcolumnType(AIndex : Integer; AValue : string); 

begin
  If (FcolumnType=AValue) then exit;
  FcolumnType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDatacolumnHeaders.SetdataType(AIndex : Integer; AValue : string); 

begin
  If (FdataType=AValue) then exit;
  FdataType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDatacolumnHeaders.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRealtimeDataprofileInfo
  --------------------------------------------------------------------}


Procedure TRealtimeDataprofileInfo.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDataprofileInfo.SetinternalWebPropertyId(AIndex : Integer; AValue : string); 

begin
  If (FinternalWebPropertyId=AValue) then exit;
  FinternalWebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDataprofileInfo.SetprofileId(AIndex : Integer; AValue : string); 

begin
  If (FprofileId=AValue) then exit;
  FprofileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDataprofileInfo.SetprofileName(AIndex : Integer; AValue : string); 

begin
  If (FprofileName=AValue) then exit;
  FprofileName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDataprofileInfo.SettableId(AIndex : Integer; AValue : string); 

begin
  If (FtableId=AValue) then exit;
  FtableId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDataprofileInfo.SetwebPropertyId(AIndex : Integer; AValue : string); 

begin
  If (FwebPropertyId=AValue) then exit;
  FwebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRealtimeDataquery
  --------------------------------------------------------------------}


Procedure TRealtimeDataquery.Setdimensions(AIndex : Integer; AValue : string); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDataquery.Setfilters(AIndex : Integer; AValue : string); 

begin
  If (Ffilters=AValue) then exit;
  Ffilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDataquery.Setids(AIndex : Integer; AValue : string); 

begin
  If (Fids=AValue) then exit;
  Fids:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDataquery.Setmaxresults(AIndex : Integer; AValue : integer); 

begin
  If (Fmaxresults=AValue) then exit;
  Fmaxresults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDataquery.Setmetrics(AIndex : Integer; AValue : TRealtimeDataquerymetrics); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRealtimeDataquery.Setsort(AIndex : Integer; AValue : TRealtimeDataquerysort); 

begin
  If (Fsort=AValue) then exit;
  Fsort:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TRealtimeDataquery.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  'maxresults' : Result:='max-results';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TRealtimeDataquerymetrics
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRealtimeDataquerysort
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRealtimeDatarows
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRealtimeDatatotalsForAllResults
  --------------------------------------------------------------------}


Class Function TRealtimeDatatotalsForAllResults.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
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



Procedure TSegment.Setdefinition(AIndex : Integer; AValue : string); 

begin
  If (Fdefinition=AValue) then exit;
  Fdefinition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegment.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegment.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegment.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegment.SetsegmentId(AIndex : Integer; AValue : string); 

begin
  If (FsegmentId=AValue) then exit;
  FsegmentId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegment.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegment.Set_type(AIndex : Integer; AValue : string); 

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


Procedure TSegments.Setitems(AIndex : Integer; AValue : TSegmentsitems); 

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



Procedure TSegments.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegments.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegments.SetpreviousLink(AIndex : Integer; AValue : string); 

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



Procedure TSegments.Setusername(AIndex : Integer; AValue : string); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSegmentsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUnsampledReport
  --------------------------------------------------------------------}


Procedure TUnsampledReport.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.SetcloudStorageDownloadDetails(AIndex : Integer; AValue : TUnsampledReportcloudStorageDownloadDetails); 

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



Procedure TUnsampledReport.Setdimensions(AIndex : Integer; AValue : string); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.SetdownloadType(AIndex : Integer; AValue : string); 

begin
  If (FdownloadType=AValue) then exit;
  FdownloadType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.SetdriveDownloadDetails(AIndex : Integer; AValue : TUnsampledReportdriveDownloadDetails); 

begin
  If (FdriveDownloadDetails=AValue) then exit;
  FdriveDownloadDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.Setenddate(AIndex : Integer; AValue : string); 

begin
  If (Fenddate=AValue) then exit;
  Fenddate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.Setfilters(AIndex : Integer; AValue : string); 

begin
  If (Ffilters=AValue) then exit;
  Ffilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.Setmetrics(AIndex : Integer; AValue : string); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.SetprofileId(AIndex : Integer; AValue : string); 

begin
  If (FprofileId=AValue) then exit;
  FprofileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.Setsegment(AIndex : Integer; AValue : string); 

begin
  If (Fsegment=AValue) then exit;
  Fsegment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.Setstartdate(AIndex : Integer; AValue : string); 

begin
  If (Fstartdate=AValue) then exit;
  Fstartdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReport.Settitle(AIndex : Integer; AValue : string); 

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



Procedure TUnsampledReport.SetwebPropertyId(AIndex : Integer; AValue : string); 

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
  TUnsampledReportcloudStorageDownloadDetails
  --------------------------------------------------------------------}


Procedure TUnsampledReportcloudStorageDownloadDetails.SetbucketId(AIndex : Integer; AValue : string); 

begin
  If (FbucketId=AValue) then exit;
  FbucketId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReportcloudStorageDownloadDetails.SetobjectId(AIndex : Integer; AValue : string); 

begin
  If (FobjectId=AValue) then exit;
  FobjectId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUnsampledReportdriveDownloadDetails
  --------------------------------------------------------------------}


Procedure TUnsampledReportdriveDownloadDetails.SetdocumentId(AIndex : Integer; AValue : string); 

begin
  If (FdocumentId=AValue) then exit;
  FdocumentId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUnsampledReports
  --------------------------------------------------------------------}


Procedure TUnsampledReports.Setitems(AIndex : Integer; AValue : TUnsampledReportsitems); 

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



Procedure TUnsampledReports.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReports.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUnsampledReports.SetpreviousLink(AIndex : Integer; AValue : string); 

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



Procedure TUnsampledReports.Setusername(AIndex : Integer; AValue : string); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUnsampledReportsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUpload
  --------------------------------------------------------------------}


Procedure TUpload.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpload.SetcustomDataSourceId(AIndex : Integer; AValue : string); 

begin
  If (FcustomDataSourceId=AValue) then exit;
  FcustomDataSourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpload.Seterrors(AIndex : Integer; AValue : TUploaderrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpload.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpload.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpload.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUploaderrors
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUploads
  --------------------------------------------------------------------}


Procedure TUploads.Setitems(AIndex : Integer; AValue : TUploadsitems); 

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



Procedure TUploads.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUploads.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUploads.SetpreviousLink(AIndex : Integer; AValue : string); 

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
  TUploadsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUserRef
  --------------------------------------------------------------------}


Procedure TUserRef.Setemail(AIndex : Integer; AValue : string); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserRef.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserRef.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWebPropertyRef
  --------------------------------------------------------------------}


Procedure TWebPropertyRef.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebPropertyRef.Sethref(AIndex : Integer; AValue : string); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebPropertyRef.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebPropertyRef.SetinternalWebPropertyId(AIndex : Integer; AValue : string); 

begin
  If (FinternalWebPropertyId=AValue) then exit;
  FinternalWebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebPropertyRef.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebPropertyRef.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWebPropertySummary
  --------------------------------------------------------------------}


Procedure TWebPropertySummary.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebPropertySummary.SetinternalWebPropertyId(AIndex : Integer; AValue : string); 

begin
  If (FinternalWebPropertyId=AValue) then exit;
  FinternalWebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebPropertySummary.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebPropertySummary.Setlevel(AIndex : Integer; AValue : string); 

begin
  If (Flevel=AValue) then exit;
  Flevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebPropertySummary.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebPropertySummary.Setprofiles(AIndex : Integer; AValue : TWebPropertySummaryprofiles); 

begin
  If (Fprofiles=AValue) then exit;
  Fprofiles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebPropertySummary.SetwebsiteUrl(AIndex : Integer; AValue : string); 

begin
  If (FwebsiteUrl=AValue) then exit;
  FwebsiteUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWebPropertySummaryprofiles
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TWebproperties
  --------------------------------------------------------------------}


Procedure TWebproperties.Setitems(AIndex : Integer; AValue : TWebpropertiesitems); 

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



Procedure TWebproperties.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperties.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperties.SetpreviousLink(AIndex : Integer; AValue : string); 

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



Procedure TWebproperties.Setusername(AIndex : Integer; AValue : string); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWebpropertiesitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TWebproperty
  --------------------------------------------------------------------}


Procedure TWebproperty.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.SetchildLink(AIndex : Integer; AValue : TWebpropertychildLink); 

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



Procedure TWebproperty.SetdefaultProfileId(AIndex : Integer; AValue : string); 

begin
  If (FdefaultProfileId=AValue) then exit;
  FdefaultProfileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.SetindustryVertical(AIndex : Integer; AValue : string); 

begin
  If (FindustryVertical=AValue) then exit;
  FindustryVertical:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.SetinternalWebPropertyId(AIndex : Integer; AValue : string); 

begin
  If (FinternalWebPropertyId=AValue) then exit;
  FinternalWebPropertyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.Setlevel(AIndex : Integer; AValue : string); 

begin
  If (Flevel=AValue) then exit;
  Flevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.SetparentLink(AIndex : Integer; AValue : TWebpropertyparentLink); 

begin
  If (FparentLink=AValue) then exit;
  FparentLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebproperty.Setpermissions(AIndex : Integer; AValue : TWebpropertypermissions); 

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



Procedure TWebproperty.SetselfLink(AIndex : Integer; AValue : string); 

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



Procedure TWebproperty.SetwebsiteUrl(AIndex : Integer; AValue : string); 

begin
  If (FwebsiteUrl=AValue) then exit;
  FwebsiteUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWebpropertychildLink
  --------------------------------------------------------------------}


Procedure TWebpropertychildLink.Sethref(AIndex : Integer; AValue : string); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebpropertychildLink.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TWebpropertychildLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TWebpropertyparentLink
  --------------------------------------------------------------------}


Procedure TWebpropertyparentLink.Sethref(AIndex : Integer; AValue : string); 

begin
  If (Fhref=AValue) then exit;
  Fhref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebpropertyparentLink.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TWebpropertyparentLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TWebpropertypermissions
  --------------------------------------------------------------------}


Procedure TWebpropertypermissions.Seteffective(AIndex : Integer; AValue : TWebpropertypermissionseffective); 

begin
  If (Feffective=AValue) then exit;
  Feffective:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWebpropertypermissionseffective
  --------------------------------------------------------------------}




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
  Result:='20150305';
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
  TAccount.RegisterObject;
  TAccountchildLink.RegisterObject;
  TAccountpermissions.RegisterObject;
  TAccountpermissionseffective.RegisterObject;
  TAccountRef.RegisterObject;
  TAccountSummaries.RegisterObject;
  TAccountSummariesitems.RegisterObject;
  TAccountSummary.RegisterObject;
  TAccountSummarywebProperties.RegisterObject;
  TAccountTicket.RegisterObject;
  TAccounts.RegisterObject;
  TAccountsitems.RegisterObject;
  TAdWordsAccount.RegisterObject;
  TAnalyticsDataimportDeleteUploadDataRequest.RegisterObject;
  TAnalyticsDataimportDeleteUploadDataRequestcustomDataImportUids.RegisterObject;
  TColumn.RegisterObject;
  TColumnattributes.RegisterObject;
  TColumns.RegisterObject;
  TColumnsattributeNames.RegisterObject;
  TColumnsitems.RegisterObject;
  TCustomDataSource.RegisterObject;
  TCustomDataSourcechildLink.RegisterObject;
  TCustomDataSourceparentLink.RegisterObject;
  TCustomDataSourceprofilesLinked.RegisterObject;
  TCustomDataSources.RegisterObject;
  TCustomDataSourcesitems.RegisterObject;
  TCustomDimension.RegisterObject;
  TCustomDimensionparentLink.RegisterObject;
  TCustomDimensions.RegisterObject;
  TCustomDimensionsitems.RegisterObject;
  TCustomMetric.RegisterObject;
  TCustomMetricparentLink.RegisterObject;
  TCustomMetrics.RegisterObject;
  TCustomMetricsitems.RegisterObject;
  TEntityAdWordsLink.RegisterObject;
  TEntityAdWordsLinkadWordsAccounts.RegisterObject;
  TEntityAdWordsLinkentity.RegisterObject;
  TEntityAdWordsLinkprofileIds.RegisterObject;
  TEntityAdWordsLinks.RegisterObject;
  TEntityAdWordsLinksitems.RegisterObject;
  TEntityUserLink.RegisterObject;
  TEntityUserLinkentity.RegisterObject;
  TEntityUserLinkpermissions.RegisterObject;
  TEntityUserLinkpermissionseffective.RegisterObject;
  TEntityUserLinkpermissionslocal.RegisterObject;
  TEntityUserLinks.RegisterObject;
  TEntityUserLinksitems.RegisterObject;
  TExperiment.RegisterObject;
  TExperimentparentLink.RegisterObject;
  TExperimentvariations.RegisterObject;
  TExperiments.RegisterObject;
  TExperimentsitems.RegisterObject;
  TFilter.RegisterObject;
  TFilteradvancedDetails.RegisterObject;
  TFilterlowercaseDetails.RegisterObject;
  TFilterparentLink.RegisterObject;
  TFiltersearchAndReplaceDetails.RegisterObject;
  TFilteruppercaseDetails.RegisterObject;
  TFilterExpression.RegisterObject;
  TFilterRef.RegisterObject;
  TFilters.RegisterObject;
  TFiltersitems.RegisterObject;
  TGaData.RegisterObject;
  TGaDatacolumnHeaders.RegisterObject;
  TGaDatadataTable.RegisterObject;
  TGaDatadataTablecols.RegisterObject;
  TGaDatadataTablerows.RegisterObject;
  TGaDatadataTablerowsc.RegisterObject;
  TGaDataprofileInfo.RegisterObject;
  TGaDataquery.RegisterObject;
  TGaDataquerymetrics.RegisterObject;
  TGaDataquerysort.RegisterObject;
  TGaDatarows.RegisterObject;
  TGaDatatotalsForAllResults.RegisterObject;
  TGoal.RegisterObject;
  TGoaleventDetails.RegisterObject;
  TGoaleventDetailseventConditions.RegisterObject;
  TGoalparentLink.RegisterObject;
  TGoalurlDestinationDetails.RegisterObject;
  TGoalurlDestinationDetailssteps.RegisterObject;
  TGoalvisitNumPagesDetails.RegisterObject;
  TGoalvisitTimeOnSiteDetails.RegisterObject;
  TGoals.RegisterObject;
  TGoalsitems.RegisterObject;
  TMcfData.RegisterObject;
  TMcfDatacolumnHeaders.RegisterObject;
  TMcfDataprofileInfo.RegisterObject;
  TMcfDataquery.RegisterObject;
  TMcfDataquerymetrics.RegisterObject;
  TMcfDataquerysort.RegisterObject;
  TMcfDatarows.RegisterObject;
  TMcfDatatotalsForAllResults.RegisterObject;
  TProfile.RegisterObject;
  TProfilechildLink.RegisterObject;
  TProfileparentLink.RegisterObject;
  TProfilepermissions.RegisterObject;
  TProfilepermissionseffective.RegisterObject;
  TProfileFilterLink.RegisterObject;
  TProfileFilterLinks.RegisterObject;
  TProfileFilterLinksitems.RegisterObject;
  TProfileRef.RegisterObject;
  TProfileSummary.RegisterObject;
  TProfiles.RegisterObject;
  TProfilesitems.RegisterObject;
  TRealtimeData.RegisterObject;
  TRealtimeDatacolumnHeaders.RegisterObject;
  TRealtimeDataprofileInfo.RegisterObject;
  TRealtimeDataquery.RegisterObject;
  TRealtimeDataquerymetrics.RegisterObject;
  TRealtimeDataquerysort.RegisterObject;
  TRealtimeDatarows.RegisterObject;
  TRealtimeDatatotalsForAllResults.RegisterObject;
  TSegment.RegisterObject;
  TSegments.RegisterObject;
  TSegmentsitems.RegisterObject;
  TUnsampledReport.RegisterObject;
  TUnsampledReportcloudStorageDownloadDetails.RegisterObject;
  TUnsampledReportdriveDownloadDetails.RegisterObject;
  TUnsampledReports.RegisterObject;
  TUnsampledReportsitems.RegisterObject;
  TUpload.RegisterObject;
  TUploaderrors.RegisterObject;
  TUploads.RegisterObject;
  TUploadsitems.RegisterObject;
  TUserRef.RegisterObject;
  TWebPropertyRef.RegisterObject;
  TWebPropertySummary.RegisterObject;
  TWebPropertySummaryprofiles.RegisterObject;
  TWebproperties.RegisterObject;
  TWebpropertiesitems.RegisterObject;
  TWebproperty.RegisterObject;
  TWebpropertychildLink.RegisterObject;
  TWebpropertyparentLink.RegisterObject;
  TWebpropertypermissions.RegisterObject;
  TWebpropertypermissionseffective.RegisterObject;
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
