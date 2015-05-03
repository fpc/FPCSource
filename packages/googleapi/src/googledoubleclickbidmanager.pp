unit googledoubleclickbidmanager;
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
  TDownloadLineItemsRequest = class;
  TDownloadLineItemsRequestArray = Array of TDownloadLineItemsRequest;
  TDownloadLineItemsRequestfilterIds = class;
  TDownloadLineItemsRequestfilterIdsArray = Array of TDownloadLineItemsRequestfilterIds;
  TDownloadLineItemsResponse = class;
  TDownloadLineItemsResponseArray = Array of TDownloadLineItemsResponse;
  TFilterPair = class;
  TFilterPairArray = Array of TFilterPair;
  TListQueriesResponse = class;
  TListQueriesResponseArray = Array of TListQueriesResponse;
  TListQueriesResponsequeries = class;
  TListQueriesResponsequeriesArray = Array of TListQueriesResponsequeries;
  TListReportsResponse = class;
  TListReportsResponseArray = Array of TListReportsResponse;
  TListReportsResponsereports = class;
  TListReportsResponsereportsArray = Array of TListReportsResponsereports;
  TParameters = class;
  TParametersArray = Array of TParameters;
  TParametersfilters = class;
  TParametersfiltersArray = Array of TParametersfilters;
  TParametersgroupBys = class;
  TParametersgroupBysArray = Array of TParametersgroupBys;
  TParametersmetrics = class;
  TParametersmetricsArray = Array of TParametersmetrics;
  TQuery = class;
  TQueryArray = Array of TQuery;
  TQueryMetadata = class;
  TQueryMetadataArray = Array of TQueryMetadata;
  TQueryMetadatashareEmailAddress = class;
  TQueryMetadatashareEmailAddressArray = Array of TQueryMetadatashareEmailAddress;
  TQuerySchedule = class;
  TQueryScheduleArray = Array of TQuerySchedule;
  TReport = class;
  TReportArray = Array of TReport;
  TReportFailure = class;
  TReportFailureArray = Array of TReportFailure;
  TReportKey = class;
  TReportKeyArray = Array of TReportKey;
  TReportMetadata = class;
  TReportMetadataArray = Array of TReportMetadata;
  TReportStatus = class;
  TReportStatusArray = Array of TReportStatus;
  TRowStatus = class;
  TRowStatusArray = Array of TRowStatus;
  TRowStatuserrors = class;
  TRowStatuserrorsArray = Array of TRowStatuserrors;
  TRunQueryRequest = class;
  TRunQueryRequestArray = Array of TRunQueryRequest;
  TUploadLineItemsRequest = class;
  TUploadLineItemsRequestArray = Array of TUploadLineItemsRequest;
  TUploadLineItemsResponse = class;
  TUploadLineItemsResponseArray = Array of TUploadLineItemsResponse;
  TUploadStatus = class;
  TUploadStatusArray = Array of TUploadStatus;
  TUploadStatuserrors = class;
  TUploadStatuserrorsArray = Array of TUploadStatuserrors;
  TUploadStatusrowStatus = class;
  TUploadStatusrowStatusArray = Array of TUploadStatusrowStatus;
  
  { --------------------------------------------------------------------
    TDownloadLineItemsRequest
    --------------------------------------------------------------------}
  
  TDownloadLineItemsRequest = Class(TGoogleBaseObject)
  Private
    FfilterIds : TDownloadLineItemsRequestfilterIds;
    FfilterType : string;
    Fformat : string;
  Protected
    //Property setters
    Procedure SetfilterIds(AIndex : Integer; AValue : TDownloadLineItemsRequestfilterIds); virtual;
    Procedure SetfilterType(AIndex : Integer; AValue : string); virtual;
    Procedure Setformat(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property filterIds : TDownloadLineItemsRequestfilterIds Index 0 Read FfilterIds Write SetfilterIds;
    Property filterType : string Index 8 Read FfilterType Write SetfilterType;
    Property format : string Index 16 Read Fformat Write Setformat;
  end;
  TDownloadLineItemsRequestClass = Class of TDownloadLineItemsRequest;
  
  { --------------------------------------------------------------------
    TDownloadLineItemsRequestfilterIds
    --------------------------------------------------------------------}
  
  TDownloadLineItemsRequestfilterIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDownloadLineItemsRequestfilterIdsClass = Class of TDownloadLineItemsRequestfilterIds;
  
  { --------------------------------------------------------------------
    TDownloadLineItemsResponse
    --------------------------------------------------------------------}
  
  TDownloadLineItemsResponse = Class(TGoogleBaseObject)
  Private
    FlineItems : string;
  Protected
    //Property setters
    Procedure SetlineItems(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property lineItems : string Index 0 Read FlineItems Write SetlineItems;
  end;
  TDownloadLineItemsResponseClass = Class of TDownloadLineItemsResponse;
  
  { --------------------------------------------------------------------
    TFilterPair
    --------------------------------------------------------------------}
  
  TFilterPair = Class(TGoogleBaseObject)
  Private
    F_type : string;
    Fvalue : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property _type : string Index 0 Read F_type Write Set_type;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TFilterPairClass = Class of TFilterPair;
  
  { --------------------------------------------------------------------
    TListQueriesResponse
    --------------------------------------------------------------------}
  
  TListQueriesResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    Fqueries : TListQueriesResponsequeries;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setqueries(AIndex : Integer; AValue : TListQueriesResponsequeries); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property queries : TListQueriesResponsequeries Index 8 Read Fqueries Write Setqueries;
  end;
  TListQueriesResponseClass = Class of TListQueriesResponse;
  
  { --------------------------------------------------------------------
    TListQueriesResponsequeries
    --------------------------------------------------------------------}
  
  TListQueriesResponsequeries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListQueriesResponsequeriesClass = Class of TListQueriesResponsequeries;
  
  { --------------------------------------------------------------------
    TListReportsResponse
    --------------------------------------------------------------------}
  
  TListReportsResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    Freports : TListReportsResponsereports;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setreports(AIndex : Integer; AValue : TListReportsResponsereports); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property reports : TListReportsResponsereports Index 8 Read Freports Write Setreports;
  end;
  TListReportsResponseClass = Class of TListReportsResponse;
  
  { --------------------------------------------------------------------
    TListReportsResponsereports
    --------------------------------------------------------------------}
  
  TListReportsResponsereports = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListReportsResponsereportsClass = Class of TListReportsResponsereports;
  
  { --------------------------------------------------------------------
    TParameters
    --------------------------------------------------------------------}
  
  TParameters = Class(TGoogleBaseObject)
  Private
    Ffilters : TParametersfilters;
    FgroupBys : TParametersgroupBys;
    FincludeInviteData : boolean;
    Fmetrics : TParametersmetrics;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setfilters(AIndex : Integer; AValue : TParametersfilters); virtual;
    Procedure SetgroupBys(AIndex : Integer; AValue : TParametersgroupBys); virtual;
    Procedure SetincludeInviteData(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setmetrics(AIndex : Integer; AValue : TParametersmetrics); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property filters : TParametersfilters Index 0 Read Ffilters Write Setfilters;
    Property groupBys : TParametersgroupBys Index 8 Read FgroupBys Write SetgroupBys;
    Property includeInviteData : boolean Index 16 Read FincludeInviteData Write SetincludeInviteData;
    Property metrics : TParametersmetrics Index 24 Read Fmetrics Write Setmetrics;
    Property _type : string Index 32 Read F_type Write Set_type;
  end;
  TParametersClass = Class of TParameters;
  
  { --------------------------------------------------------------------
    TParametersfilters
    --------------------------------------------------------------------}
  
  TParametersfilters = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TParametersfiltersClass = Class of TParametersfilters;
  
  { --------------------------------------------------------------------
    TParametersgroupBys
    --------------------------------------------------------------------}
  
  TParametersgroupBys = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TParametersgroupBysClass = Class of TParametersgroupBys;
  
  { --------------------------------------------------------------------
    TParametersmetrics
    --------------------------------------------------------------------}
  
  TParametersmetrics = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TParametersmetricsClass = Class of TParametersmetrics;
  
  { --------------------------------------------------------------------
    TQuery
    --------------------------------------------------------------------}
  
  TQuery = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    Fmetadata : TQueryMetadata;
    Fparams : TParameters;
    FqueryId : string;
    FreportDataEndTimeMs : string;
    FreportDataStartTimeMs : string;
    Fschedule : TQuerySchedule;
    FtimezoneCode : string;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setmetadata(AIndex : Integer; AValue : TQueryMetadata); virtual;
    Procedure Setparams(AIndex : Integer; AValue : TParameters); virtual;
    Procedure SetqueryId(AIndex : Integer; AValue : string); virtual;
    Procedure SetreportDataEndTimeMs(AIndex : Integer; AValue : string); virtual;
    Procedure SetreportDataStartTimeMs(AIndex : Integer; AValue : string); virtual;
    Procedure Setschedule(AIndex : Integer; AValue : TQuerySchedule); virtual;
    Procedure SettimezoneCode(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property metadata : TQueryMetadata Index 8 Read Fmetadata Write Setmetadata;
    Property params : TParameters Index 16 Read Fparams Write Setparams;
    Property queryId : string Index 24 Read FqueryId Write SetqueryId;
    Property reportDataEndTimeMs : string Index 32 Read FreportDataEndTimeMs Write SetreportDataEndTimeMs;
    Property reportDataStartTimeMs : string Index 40 Read FreportDataStartTimeMs Write SetreportDataStartTimeMs;
    Property schedule : TQuerySchedule Index 48 Read Fschedule Write Setschedule;
    Property timezoneCode : string Index 56 Read FtimezoneCode Write SettimezoneCode;
  end;
  TQueryClass = Class of TQuery;
  
  { --------------------------------------------------------------------
    TQueryMetadata
    --------------------------------------------------------------------}
  
  TQueryMetadata = Class(TGoogleBaseObject)
  Private
    FdataRange : string;
    Fformat : string;
    FgoogleCloudStoragePathForLatestReport : string;
    FgoogleDrivePathForLatestReport : string;
    FlatestReportRunTimeMs : string;
    Flocale : string;
    FreportCount : integer;
    Frunning : boolean;
    FsendNotification : boolean;
    FshareEmailAddress : TQueryMetadatashareEmailAddress;
    Ftitle : string;
  Protected
    //Property setters
    Procedure SetdataRange(AIndex : Integer; AValue : string); virtual;
    Procedure Setformat(AIndex : Integer; AValue : string); virtual;
    Procedure SetgoogleCloudStoragePathForLatestReport(AIndex : Integer; AValue : string); virtual;
    Procedure SetgoogleDrivePathForLatestReport(AIndex : Integer; AValue : string); virtual;
    Procedure SetlatestReportRunTimeMs(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocale(AIndex : Integer; AValue : string); virtual;
    Procedure SetreportCount(AIndex : Integer; AValue : integer); virtual;
    Procedure Setrunning(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsendNotification(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetshareEmailAddress(AIndex : Integer; AValue : TQueryMetadatashareEmailAddress); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property dataRange : string Index 0 Read FdataRange Write SetdataRange;
    Property format : string Index 8 Read Fformat Write Setformat;
    Property googleCloudStoragePathForLatestReport : string Index 16 Read FgoogleCloudStoragePathForLatestReport Write SetgoogleCloudStoragePathForLatestReport;
    Property googleDrivePathForLatestReport : string Index 24 Read FgoogleDrivePathForLatestReport Write SetgoogleDrivePathForLatestReport;
    Property latestReportRunTimeMs : string Index 32 Read FlatestReportRunTimeMs Write SetlatestReportRunTimeMs;
    Property locale : string Index 40 Read Flocale Write Setlocale;
    Property reportCount : integer Index 48 Read FreportCount Write SetreportCount;
    Property running : boolean Index 56 Read Frunning Write Setrunning;
    Property sendNotification : boolean Index 64 Read FsendNotification Write SetsendNotification;
    Property shareEmailAddress : TQueryMetadatashareEmailAddress Index 72 Read FshareEmailAddress Write SetshareEmailAddress;
    Property title : string Index 80 Read Ftitle Write Settitle;
  end;
  TQueryMetadataClass = Class of TQueryMetadata;
  
  { --------------------------------------------------------------------
    TQueryMetadatashareEmailAddress
    --------------------------------------------------------------------}
  
  TQueryMetadatashareEmailAddress = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TQueryMetadatashareEmailAddressClass = Class of TQueryMetadatashareEmailAddress;
  
  { --------------------------------------------------------------------
    TQuerySchedule
    --------------------------------------------------------------------}
  
  TQuerySchedule = Class(TGoogleBaseObject)
  Private
    FendTimeMs : string;
    Ffrequency : string;
    FnextRunMinuteOfDay : integer;
    FnextRunTimezoneCode : string;
  Protected
    //Property setters
    Procedure SetendTimeMs(AIndex : Integer; AValue : string); virtual;
    Procedure Setfrequency(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextRunMinuteOfDay(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnextRunTimezoneCode(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property endTimeMs : string Index 0 Read FendTimeMs Write SetendTimeMs;
    Property frequency : string Index 8 Read Ffrequency Write Setfrequency;
    Property nextRunMinuteOfDay : integer Index 16 Read FnextRunMinuteOfDay Write SetnextRunMinuteOfDay;
    Property nextRunTimezoneCode : string Index 24 Read FnextRunTimezoneCode Write SetnextRunTimezoneCode;
  end;
  TQueryScheduleClass = Class of TQuerySchedule;
  
  { --------------------------------------------------------------------
    TReport
    --------------------------------------------------------------------}
  
  TReport = Class(TGoogleBaseObject)
  Private
    Fkey : TReportKey;
    Fmetadata : TReportMetadata;
    Fparams : TParameters;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : TReportKey); virtual;
    Procedure Setmetadata(AIndex : Integer; AValue : TReportMetadata); virtual;
    Procedure Setparams(AIndex : Integer; AValue : TParameters); virtual;
  Public
  Published
    Property key : TReportKey Index 0 Read Fkey Write Setkey;
    Property metadata : TReportMetadata Index 8 Read Fmetadata Write Setmetadata;
    Property params : TParameters Index 16 Read Fparams Write Setparams;
  end;
  TReportClass = Class of TReport;
  
  { --------------------------------------------------------------------
    TReportFailure
    --------------------------------------------------------------------}
  
  TReportFailure = Class(TGoogleBaseObject)
  Private
    FerrorCode : string;
  Protected
    //Property setters
    Procedure SeterrorCode(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property errorCode : string Index 0 Read FerrorCode Write SeterrorCode;
  end;
  TReportFailureClass = Class of TReportFailure;
  
  { --------------------------------------------------------------------
    TReportKey
    --------------------------------------------------------------------}
  
  TReportKey = Class(TGoogleBaseObject)
  Private
    FqueryId : string;
    FreportId : string;
  Protected
    //Property setters
    Procedure SetqueryId(AIndex : Integer; AValue : string); virtual;
    Procedure SetreportId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property queryId : string Index 0 Read FqueryId Write SetqueryId;
    Property reportId : string Index 8 Read FreportId Write SetreportId;
  end;
  TReportKeyClass = Class of TReportKey;
  
  { --------------------------------------------------------------------
    TReportMetadata
    --------------------------------------------------------------------}
  
  TReportMetadata = Class(TGoogleBaseObject)
  Private
    FgoogleCloudStoragePath : string;
    FreportDataEndTimeMs : string;
    FreportDataStartTimeMs : string;
    Fstatus : TReportStatus;
  Protected
    //Property setters
    Procedure SetgoogleCloudStoragePath(AIndex : Integer; AValue : string); virtual;
    Procedure SetreportDataEndTimeMs(AIndex : Integer; AValue : string); virtual;
    Procedure SetreportDataStartTimeMs(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : TReportStatus); virtual;
  Public
  Published
    Property googleCloudStoragePath : string Index 0 Read FgoogleCloudStoragePath Write SetgoogleCloudStoragePath;
    Property reportDataEndTimeMs : string Index 8 Read FreportDataEndTimeMs Write SetreportDataEndTimeMs;
    Property reportDataStartTimeMs : string Index 16 Read FreportDataStartTimeMs Write SetreportDataStartTimeMs;
    Property status : TReportStatus Index 24 Read Fstatus Write Setstatus;
  end;
  TReportMetadataClass = Class of TReportMetadata;
  
  { --------------------------------------------------------------------
    TReportStatus
    --------------------------------------------------------------------}
  
  TReportStatus = Class(TGoogleBaseObject)
  Private
    Ffailure : TReportFailure;
    FfinishTimeMs : string;
    Fformat : string;
    Fstate : string;
  Protected
    //Property setters
    Procedure Setfailure(AIndex : Integer; AValue : TReportFailure); virtual;
    Procedure SetfinishTimeMs(AIndex : Integer; AValue : string); virtual;
    Procedure Setformat(AIndex : Integer; AValue : string); virtual;
    Procedure Setstate(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property failure : TReportFailure Index 0 Read Ffailure Write Setfailure;
    Property finishTimeMs : string Index 8 Read FfinishTimeMs Write SetfinishTimeMs;
    Property format : string Index 16 Read Fformat Write Setformat;
    Property state : string Index 24 Read Fstate Write Setstate;
  end;
  TReportStatusClass = Class of TReportStatus;
  
  { --------------------------------------------------------------------
    TRowStatus
    --------------------------------------------------------------------}
  
  TRowStatus = Class(TGoogleBaseObject)
  Private
    Fchanged : boolean;
    FentityId : string;
    FentityName : string;
    Ferrors : TRowStatuserrors;
    Fpersisted : boolean;
    FrowNumber : integer;
  Protected
    //Property setters
    Procedure Setchanged(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetentityId(AIndex : Integer; AValue : string); virtual;
    Procedure SetentityName(AIndex : Integer; AValue : string); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TRowStatuserrors); virtual;
    Procedure Setpersisted(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetrowNumber(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property changed : boolean Index 0 Read Fchanged Write Setchanged;
    Property entityId : string Index 8 Read FentityId Write SetentityId;
    Property entityName : string Index 16 Read FentityName Write SetentityName;
    Property errors : TRowStatuserrors Index 24 Read Ferrors Write Seterrors;
    Property persisted : boolean Index 32 Read Fpersisted Write Setpersisted;
    Property rowNumber : integer Index 40 Read FrowNumber Write SetrowNumber;
  end;
  TRowStatusClass = Class of TRowStatus;
  
  { --------------------------------------------------------------------
    TRowStatuserrors
    --------------------------------------------------------------------}
  
  TRowStatuserrors = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRowStatuserrorsClass = Class of TRowStatuserrors;
  
  { --------------------------------------------------------------------
    TRunQueryRequest
    --------------------------------------------------------------------}
  
  TRunQueryRequest = Class(TGoogleBaseObject)
  Private
    FdataRange : string;
    FreportDataEndTimeMs : string;
    FreportDataStartTimeMs : string;
    FtimezoneCode : string;
  Protected
    //Property setters
    Procedure SetdataRange(AIndex : Integer; AValue : string); virtual;
    Procedure SetreportDataEndTimeMs(AIndex : Integer; AValue : string); virtual;
    Procedure SetreportDataStartTimeMs(AIndex : Integer; AValue : string); virtual;
    Procedure SettimezoneCode(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property dataRange : string Index 0 Read FdataRange Write SetdataRange;
    Property reportDataEndTimeMs : string Index 8 Read FreportDataEndTimeMs Write SetreportDataEndTimeMs;
    Property reportDataStartTimeMs : string Index 16 Read FreportDataStartTimeMs Write SetreportDataStartTimeMs;
    Property timezoneCode : string Index 24 Read FtimezoneCode Write SettimezoneCode;
  end;
  TRunQueryRequestClass = Class of TRunQueryRequest;
  
  { --------------------------------------------------------------------
    TUploadLineItemsRequest
    --------------------------------------------------------------------}
  
  TUploadLineItemsRequest = Class(TGoogleBaseObject)
  Private
    FdryRun : boolean;
    Fformat : string;
    FlineItems : string;
  Protected
    //Property setters
    Procedure SetdryRun(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setformat(AIndex : Integer; AValue : string); virtual;
    Procedure SetlineItems(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property dryRun : boolean Index 0 Read FdryRun Write SetdryRun;
    Property format : string Index 8 Read Fformat Write Setformat;
    Property lineItems : string Index 16 Read FlineItems Write SetlineItems;
  end;
  TUploadLineItemsRequestClass = Class of TUploadLineItemsRequest;
  
  { --------------------------------------------------------------------
    TUploadLineItemsResponse
    --------------------------------------------------------------------}
  
  TUploadLineItemsResponse = Class(TGoogleBaseObject)
  Private
    FuploadStatus : TUploadStatus;
  Protected
    //Property setters
    Procedure SetuploadStatus(AIndex : Integer; AValue : TUploadStatus); virtual;
  Public
  Published
    Property uploadStatus : TUploadStatus Index 0 Read FuploadStatus Write SetuploadStatus;
  end;
  TUploadLineItemsResponseClass = Class of TUploadLineItemsResponse;
  
  { --------------------------------------------------------------------
    TUploadStatus
    --------------------------------------------------------------------}
  
  TUploadStatus = Class(TGoogleBaseObject)
  Private
    Ferrors : TUploadStatuserrors;
    FrowStatus : TUploadStatusrowStatus;
  Protected
    //Property setters
    Procedure Seterrors(AIndex : Integer; AValue : TUploadStatuserrors); virtual;
    Procedure SetrowStatus(AIndex : Integer; AValue : TUploadStatusrowStatus); virtual;
  Public
  Published
    Property errors : TUploadStatuserrors Index 0 Read Ferrors Write Seterrors;
    Property rowStatus : TUploadStatusrowStatus Index 8 Read FrowStatus Write SetrowStatus;
  end;
  TUploadStatusClass = Class of TUploadStatus;
  
  { --------------------------------------------------------------------
    TUploadStatuserrors
    --------------------------------------------------------------------}
  
  TUploadStatuserrors = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUploadStatuserrorsClass = Class of TUploadStatuserrors;
  
  { --------------------------------------------------------------------
    TUploadStatusrowStatus
    --------------------------------------------------------------------}
  
  TUploadStatusrowStatus = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUploadStatusrowStatusClass = Class of TUploadStatusrowStatus;
  
  { --------------------------------------------------------------------
    TLineitemsResource
    --------------------------------------------------------------------}
  
  TLineitemsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Downloadlineitems(aDownloadLineItemsRequest : TDownloadLineItemsRequest) : TDownloadLineItemsResponse;
    Function Uploadlineitems(aUploadLineItemsRequest : TUploadLineItemsRequest) : TUploadLineItemsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TQueriesResource
    --------------------------------------------------------------------}
  
  TQueriesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Createquery(aQuery : TQuery) : TQuery;
    Procedure Deletequery(queryId: string);
    Function Getquery(queryId: string) : TQuery;
    Function Listqueries : TListQueriesResponse;
    Procedure Runquery(queryId: string; aRunQueryRequest : TRunQueryRequest);
  end;
  
  
  { --------------------------------------------------------------------
    TReportsResource
    --------------------------------------------------------------------}
  
  TReportsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Listreports(queryId: string) : TListReportsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TDoubleclickbidmanagerAPI
    --------------------------------------------------------------------}
  
  TDoubleclickbidmanagerAPI = Class(TGoogleAPI)
  Private
    FLineitemsInstance : TLineitemsResource;
    FQueriesInstance : TQueriesResource;
    FReportsInstance : TReportsResource;
    Function GetLineitemsInstance : TLineitemsResource;virtual;
    Function GetQueriesInstance : TQueriesResource;virtual;
    Function GetReportsInstance : TReportsResource;virtual;
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
    Function CreateLineitemsResource(AOwner : TComponent) : TLineitemsResource;virtual;overload;
    Function CreateLineitemsResource : TLineitemsResource;virtual;overload;
    Function CreateQueriesResource(AOwner : TComponent) : TQueriesResource;virtual;overload;
    Function CreateQueriesResource : TQueriesResource;virtual;overload;
    Function CreateReportsResource(AOwner : TComponent) : TReportsResource;virtual;overload;
    Function CreateReportsResource : TReportsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property LineitemsResource : TLineitemsResource Read GetLineitemsInstance;
    Property QueriesResource : TQueriesResource Read GetQueriesInstance;
    Property ReportsResource : TReportsResource Read GetReportsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TDownloadLineItemsRequest
  --------------------------------------------------------------------}


Procedure TDownloadLineItemsRequest.SetfilterIds(AIndex : Integer; AValue : TDownloadLineItemsRequestfilterIds); 

begin
  If (FfilterIds=AValue) then exit;
  FfilterIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadLineItemsRequest.SetfilterType(AIndex : Integer; AValue : string); 

begin
  If (FfilterType=AValue) then exit;
  FfilterType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadLineItemsRequest.Setformat(AIndex : Integer; AValue : string); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDownloadLineItemsRequestfilterIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDownloadLineItemsResponse
  --------------------------------------------------------------------}


Procedure TDownloadLineItemsResponse.SetlineItems(AIndex : Integer; AValue : string); 

begin
  If (FlineItems=AValue) then exit;
  FlineItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFilterPair
  --------------------------------------------------------------------}


Procedure TFilterPair.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterPair.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TFilterPair.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TListQueriesResponse
  --------------------------------------------------------------------}


Procedure TListQueriesResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListQueriesResponse.Setqueries(AIndex : Integer; AValue : TListQueriesResponsequeries); 

begin
  If (Fqueries=AValue) then exit;
  Fqueries:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListQueriesResponsequeries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListReportsResponse
  --------------------------------------------------------------------}


Procedure TListReportsResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListReportsResponse.Setreports(AIndex : Integer; AValue : TListReportsResponsereports); 

begin
  If (Freports=AValue) then exit;
  Freports:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListReportsResponsereports
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TParameters
  --------------------------------------------------------------------}


Procedure TParameters.Setfilters(AIndex : Integer; AValue : TParametersfilters); 

begin
  If (Ffilters=AValue) then exit;
  Ffilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParameters.SetgroupBys(AIndex : Integer; AValue : TParametersgroupBys); 

begin
  If (FgroupBys=AValue) then exit;
  FgroupBys:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParameters.SetincludeInviteData(AIndex : Integer; AValue : boolean); 

begin
  If (FincludeInviteData=AValue) then exit;
  FincludeInviteData:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParameters.Setmetrics(AIndex : Integer; AValue : TParametersmetrics); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParameters.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TParameters.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TParametersfilters
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TParametersgroupBys
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TParametersmetrics
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TQuery
  --------------------------------------------------------------------}


Procedure TQuery.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setmetadata(AIndex : Integer; AValue : TQueryMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setparams(AIndex : Integer; AValue : TParameters); 

begin
  If (Fparams=AValue) then exit;
  Fparams:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetqueryId(AIndex : Integer; AValue : string); 

begin
  If (FqueryId=AValue) then exit;
  FqueryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetreportDataEndTimeMs(AIndex : Integer; AValue : string); 

begin
  If (FreportDataEndTimeMs=AValue) then exit;
  FreportDataEndTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetreportDataStartTimeMs(AIndex : Integer; AValue : string); 

begin
  If (FreportDataStartTimeMs=AValue) then exit;
  FreportDataStartTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setschedule(AIndex : Integer; AValue : TQuerySchedule); 

begin
  If (Fschedule=AValue) then exit;
  Fschedule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SettimezoneCode(AIndex : Integer; AValue : string); 

begin
  If (FtimezoneCode=AValue) then exit;
  FtimezoneCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TQueryMetadata
  --------------------------------------------------------------------}


Procedure TQueryMetadata.SetdataRange(AIndex : Integer; AValue : string); 

begin
  If (FdataRange=AValue) then exit;
  FdataRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryMetadata.Setformat(AIndex : Integer; AValue : string); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryMetadata.SetgoogleCloudStoragePathForLatestReport(AIndex : Integer; AValue : string); 

begin
  If (FgoogleCloudStoragePathForLatestReport=AValue) then exit;
  FgoogleCloudStoragePathForLatestReport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryMetadata.SetgoogleDrivePathForLatestReport(AIndex : Integer; AValue : string); 

begin
  If (FgoogleDrivePathForLatestReport=AValue) then exit;
  FgoogleDrivePathForLatestReport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryMetadata.SetlatestReportRunTimeMs(AIndex : Integer; AValue : string); 

begin
  If (FlatestReportRunTimeMs=AValue) then exit;
  FlatestReportRunTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryMetadata.Setlocale(AIndex : Integer; AValue : string); 

begin
  If (Flocale=AValue) then exit;
  Flocale:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryMetadata.SetreportCount(AIndex : Integer; AValue : integer); 

begin
  If (FreportCount=AValue) then exit;
  FreportCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryMetadata.Setrunning(AIndex : Integer; AValue : boolean); 

begin
  If (Frunning=AValue) then exit;
  Frunning:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryMetadata.SetsendNotification(AIndex : Integer; AValue : boolean); 

begin
  If (FsendNotification=AValue) then exit;
  FsendNotification:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryMetadata.SetshareEmailAddress(AIndex : Integer; AValue : TQueryMetadatashareEmailAddress); 

begin
  If (FshareEmailAddress=AValue) then exit;
  FshareEmailAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryMetadata.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TQueryMetadatashareEmailAddress
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TQuerySchedule
  --------------------------------------------------------------------}


Procedure TQuerySchedule.SetendTimeMs(AIndex : Integer; AValue : string); 

begin
  If (FendTimeMs=AValue) then exit;
  FendTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuerySchedule.Setfrequency(AIndex : Integer; AValue : string); 

begin
  If (Ffrequency=AValue) then exit;
  Ffrequency:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuerySchedule.SetnextRunMinuteOfDay(AIndex : Integer; AValue : integer); 

begin
  If (FnextRunMinuteOfDay=AValue) then exit;
  FnextRunMinuteOfDay:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuerySchedule.SetnextRunTimezoneCode(AIndex : Integer; AValue : string); 

begin
  If (FnextRunTimezoneCode=AValue) then exit;
  FnextRunTimezoneCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReport
  --------------------------------------------------------------------}


Procedure TReport.Setkey(AIndex : Integer; AValue : TReportKey); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setmetadata(AIndex : Integer; AValue : TReportMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setparams(AIndex : Integer; AValue : TParameters); 

begin
  If (Fparams=AValue) then exit;
  Fparams:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportFailure
  --------------------------------------------------------------------}


Procedure TReportFailure.SeterrorCode(AIndex : Integer; AValue : string); 

begin
  If (FerrorCode=AValue) then exit;
  FerrorCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportKey
  --------------------------------------------------------------------}


Procedure TReportKey.SetqueryId(AIndex : Integer; AValue : string); 

begin
  If (FqueryId=AValue) then exit;
  FqueryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportKey.SetreportId(AIndex : Integer; AValue : string); 

begin
  If (FreportId=AValue) then exit;
  FreportId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportMetadata
  --------------------------------------------------------------------}


Procedure TReportMetadata.SetgoogleCloudStoragePath(AIndex : Integer; AValue : string); 

begin
  If (FgoogleCloudStoragePath=AValue) then exit;
  FgoogleCloudStoragePath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportMetadata.SetreportDataEndTimeMs(AIndex : Integer; AValue : string); 

begin
  If (FreportDataEndTimeMs=AValue) then exit;
  FreportDataEndTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportMetadata.SetreportDataStartTimeMs(AIndex : Integer; AValue : string); 

begin
  If (FreportDataStartTimeMs=AValue) then exit;
  FreportDataStartTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportMetadata.Setstatus(AIndex : Integer; AValue : TReportStatus); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportStatus
  --------------------------------------------------------------------}


Procedure TReportStatus.Setfailure(AIndex : Integer; AValue : TReportFailure); 

begin
  If (Ffailure=AValue) then exit;
  Ffailure:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportStatus.SetfinishTimeMs(AIndex : Integer; AValue : string); 

begin
  If (FfinishTimeMs=AValue) then exit;
  FfinishTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportStatus.Setformat(AIndex : Integer; AValue : string); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportStatus.Setstate(AIndex : Integer; AValue : string); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRowStatus
  --------------------------------------------------------------------}


Procedure TRowStatus.Setchanged(AIndex : Integer; AValue : boolean); 

begin
  If (Fchanged=AValue) then exit;
  Fchanged:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRowStatus.SetentityId(AIndex : Integer; AValue : string); 

begin
  If (FentityId=AValue) then exit;
  FentityId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRowStatus.SetentityName(AIndex : Integer; AValue : string); 

begin
  If (FentityName=AValue) then exit;
  FentityName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRowStatus.Seterrors(AIndex : Integer; AValue : TRowStatuserrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRowStatus.Setpersisted(AIndex : Integer; AValue : boolean); 

begin
  If (Fpersisted=AValue) then exit;
  Fpersisted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRowStatus.SetrowNumber(AIndex : Integer; AValue : integer); 

begin
  If (FrowNumber=AValue) then exit;
  FrowNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRowStatuserrors
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRunQueryRequest
  --------------------------------------------------------------------}


Procedure TRunQueryRequest.SetdataRange(AIndex : Integer; AValue : string); 

begin
  If (FdataRange=AValue) then exit;
  FdataRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRunQueryRequest.SetreportDataEndTimeMs(AIndex : Integer; AValue : string); 

begin
  If (FreportDataEndTimeMs=AValue) then exit;
  FreportDataEndTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRunQueryRequest.SetreportDataStartTimeMs(AIndex : Integer; AValue : string); 

begin
  If (FreportDataStartTimeMs=AValue) then exit;
  FreportDataStartTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRunQueryRequest.SettimezoneCode(AIndex : Integer; AValue : string); 

begin
  If (FtimezoneCode=AValue) then exit;
  FtimezoneCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUploadLineItemsRequest
  --------------------------------------------------------------------}


Procedure TUploadLineItemsRequest.SetdryRun(AIndex : Integer; AValue : boolean); 

begin
  If (FdryRun=AValue) then exit;
  FdryRun:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUploadLineItemsRequest.Setformat(AIndex : Integer; AValue : string); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUploadLineItemsRequest.SetlineItems(AIndex : Integer; AValue : string); 

begin
  If (FlineItems=AValue) then exit;
  FlineItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUploadLineItemsResponse
  --------------------------------------------------------------------}


Procedure TUploadLineItemsResponse.SetuploadStatus(AIndex : Integer; AValue : TUploadStatus); 

begin
  If (FuploadStatus=AValue) then exit;
  FuploadStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUploadStatus
  --------------------------------------------------------------------}


Procedure TUploadStatus.Seterrors(AIndex : Integer; AValue : TUploadStatuserrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUploadStatus.SetrowStatus(AIndex : Integer; AValue : TUploadStatusrowStatus); 

begin
  If (FrowStatus=AValue) then exit;
  FrowStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUploadStatuserrors
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUploadStatusrowStatus
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLineitemsResource
  --------------------------------------------------------------------}


Class Function TLineitemsResource.ResourceName : String;

begin
  Result:='lineitems';
end;

Class Function TLineitemsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdoubleclickbidmanagerAPI;
end;

Function TLineitemsResource.Downloadlineitems(aDownloadLineItemsRequest : TDownloadLineItemsRequest) : TDownloadLineItemsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'lineitems/downloadlineitems';
  _Methodid   = 'doubleclickbidmanager.lineitems.downloadlineitems';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aDownloadLineItemsRequest,TDownloadLineItemsResponse) as TDownloadLineItemsResponse;
end;

Function TLineitemsResource.Uploadlineitems(aUploadLineItemsRequest : TUploadLineItemsRequest) : TUploadLineItemsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'lineitems/uploadlineitems';
  _Methodid   = 'doubleclickbidmanager.lineitems.uploadlineitems';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aUploadLineItemsRequest,TUploadLineItemsResponse) as TUploadLineItemsResponse;
end;



{ --------------------------------------------------------------------
  TQueriesResource
  --------------------------------------------------------------------}


Class Function TQueriesResource.ResourceName : String;

begin
  Result:='queries';
end;

Class Function TQueriesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdoubleclickbidmanagerAPI;
end;

Function TQueriesResource.Createquery(aQuery : TQuery) : TQuery;

Const
  _HTTPMethod = 'POST';
  _Path       = 'query';
  _Methodid   = 'doubleclickbidmanager.queries.createquery';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aQuery,TQuery) as TQuery;
end;

Procedure TQueriesResource.Deletequery(queryId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'query/{queryId}';
  _Methodid   = 'doubleclickbidmanager.queries.deletequery';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['queryId',queryId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TQueriesResource.Getquery(queryId: string) : TQuery;

Const
  _HTTPMethod = 'GET';
  _Path       = 'query/{queryId}';
  _Methodid   = 'doubleclickbidmanager.queries.getquery';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['queryId',queryId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TQuery) as TQuery;
end;

Function TQueriesResource.Listqueries : TListQueriesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'queries';
  _Methodid   = 'doubleclickbidmanager.queries.listqueries';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TListQueriesResponse) as TListQueriesResponse;
end;

Procedure TQueriesResource.Runquery(queryId: string; aRunQueryRequest : TRunQueryRequest);

Const
  _HTTPMethod = 'POST';
  _Path       = 'query/{queryId}';
  _Methodid   = 'doubleclickbidmanager.queries.runquery';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['queryId',queryId]);
  ServiceCall(_HTTPMethod,_P,'',aRunQueryRequest,Nil);
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
  Result:=TdoubleclickbidmanagerAPI;
end;

Function TReportsResource.Listreports(queryId: string) : TListReportsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'queries/{queryId}/reports';
  _Methodid   = 'doubleclickbidmanager.reports.listreports';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['queryId',queryId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TListReportsResponse) as TListReportsResponse;
end;



{ --------------------------------------------------------------------
  TDoubleclickbidmanagerAPI
  --------------------------------------------------------------------}

Class Function TDoubleclickbidmanagerAPI.APIName : String;

begin
  Result:='doubleclickbidmanager';
end;

Class Function TDoubleclickbidmanagerAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TDoubleclickbidmanagerAPI.APIRevision : String;

begin
  Result:='20150326';
end;

Class Function TDoubleclickbidmanagerAPI.APIID : String;

begin
  Result:='doubleclickbidmanager:v1';
end;

Class Function TDoubleclickbidmanagerAPI.APITitle : String;

begin
  Result:='DoubleClick Bid Manager API';
end;

Class Function TDoubleclickbidmanagerAPI.APIDescription : String;

begin
  Result:='API for viewing and managing your reports in DoubleClick Bid Manager.';
end;

Class Function TDoubleclickbidmanagerAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TDoubleclickbidmanagerAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TDoubleclickbidmanagerAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TDoubleclickbidmanagerAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TDoubleclickbidmanagerAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/bid-manager/';
end;

Class Function TDoubleclickbidmanagerAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TDoubleclickbidmanagerAPI.APIbasePath : string;

begin
  Result:='/doubleclickbidmanager/v1/';
end;

Class Function TDoubleclickbidmanagerAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/doubleclickbidmanager/v1/';
end;

Class Function TDoubleclickbidmanagerAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TDoubleclickbidmanagerAPI.APIservicePath : string;

begin
  Result:='doubleclickbidmanager/v1/';
end;

Class Function TDoubleclickbidmanagerAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TDoubleclickbidmanagerAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,0);
  
end;

Class Function TDoubleclickbidmanagerAPI.APINeedsAuth : Boolean;

begin
  Result:=False;
end;

Class Procedure TDoubleclickbidmanagerAPI.RegisterAPIResources;

begin
  TDownloadLineItemsRequest.RegisterObject;
  TDownloadLineItemsRequestfilterIds.RegisterObject;
  TDownloadLineItemsResponse.RegisterObject;
  TFilterPair.RegisterObject;
  TListQueriesResponse.RegisterObject;
  TListQueriesResponsequeries.RegisterObject;
  TListReportsResponse.RegisterObject;
  TListReportsResponsereports.RegisterObject;
  TParameters.RegisterObject;
  TParametersfilters.RegisterObject;
  TParametersgroupBys.RegisterObject;
  TParametersmetrics.RegisterObject;
  TQuery.RegisterObject;
  TQueryMetadata.RegisterObject;
  TQueryMetadatashareEmailAddress.RegisterObject;
  TQuerySchedule.RegisterObject;
  TReport.RegisterObject;
  TReportFailure.RegisterObject;
  TReportKey.RegisterObject;
  TReportMetadata.RegisterObject;
  TReportStatus.RegisterObject;
  TRowStatus.RegisterObject;
  TRowStatuserrors.RegisterObject;
  TRunQueryRequest.RegisterObject;
  TUploadLineItemsRequest.RegisterObject;
  TUploadLineItemsResponse.RegisterObject;
  TUploadStatus.RegisterObject;
  TUploadStatuserrors.RegisterObject;
  TUploadStatusrowStatus.RegisterObject;
end;


Function TDoubleclickbidmanagerAPI.GetLineitemsInstance : TLineitemsResource;

begin
  if (FLineitemsInstance=Nil) then
    FLineitemsInstance:=CreateLineitemsResource;
  Result:=FLineitemsInstance;
end;

Function TDoubleclickbidmanagerAPI.CreateLineitemsResource : TLineitemsResource;

begin
  Result:=CreateLineitemsResource(Self);
end;


Function TDoubleclickbidmanagerAPI.CreateLineitemsResource(AOwner : TComponent) : TLineitemsResource;

begin
  Result:=TLineitemsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDoubleclickbidmanagerAPI.GetQueriesInstance : TQueriesResource;

begin
  if (FQueriesInstance=Nil) then
    FQueriesInstance:=CreateQueriesResource;
  Result:=FQueriesInstance;
end;

Function TDoubleclickbidmanagerAPI.CreateQueriesResource : TQueriesResource;

begin
  Result:=CreateQueriesResource(Self);
end;


Function TDoubleclickbidmanagerAPI.CreateQueriesResource(AOwner : TComponent) : TQueriesResource;

begin
  Result:=TQueriesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDoubleclickbidmanagerAPI.GetReportsInstance : TReportsResource;

begin
  if (FReportsInstance=Nil) then
    FReportsInstance:=CreateReportsResource;
  Result:=FReportsInstance;
end;

Function TDoubleclickbidmanagerAPI.CreateReportsResource : TReportsResource;

begin
  Result:=CreateReportsResource(Self);
end;


Function TDoubleclickbidmanagerAPI.CreateReportsResource(AOwner : TComponent) : TReportsResource;

begin
  Result:=TReportsResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TDoubleclickbidmanagerAPI.RegisterAPI;
end.
