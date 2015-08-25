unit googledoubleclickbidmanager;
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
//Generated on: 16-5-15 08:53:02
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TDownloadLineItemsRequest = Class;
  TDownloadLineItemsResponse = Class;
  TFilterPair = Class;
  TListQueriesResponse = Class;
  TListReportsResponse = Class;
  TParameters = Class;
  TQuery = Class;
  TQueryMetadata = Class;
  TQuerySchedule = Class;
  TReport = Class;
  TReportFailure = Class;
  TReportKey = Class;
  TReportMetadata = Class;
  TReportStatus = Class;
  TRowStatus = Class;
  TRunQueryRequest = Class;
  TUploadLineItemsRequest = Class;
  TUploadLineItemsResponse = Class;
  TUploadStatus = Class;
  TDownloadLineItemsRequestArray = Array of TDownloadLineItemsRequest;
  TDownloadLineItemsResponseArray = Array of TDownloadLineItemsResponse;
  TFilterPairArray = Array of TFilterPair;
  TListQueriesResponseArray = Array of TListQueriesResponse;
  TListReportsResponseArray = Array of TListReportsResponse;
  TParametersArray = Array of TParameters;
  TQueryArray = Array of TQuery;
  TQueryMetadataArray = Array of TQueryMetadata;
  TQueryScheduleArray = Array of TQuerySchedule;
  TReportArray = Array of TReport;
  TReportFailureArray = Array of TReportFailure;
  TReportKeyArray = Array of TReportKey;
  TReportMetadataArray = Array of TReportMetadata;
  TReportStatusArray = Array of TReportStatus;
  TRowStatusArray = Array of TRowStatus;
  TRunQueryRequestArray = Array of TRunQueryRequest;
  TUploadLineItemsRequestArray = Array of TUploadLineItemsRequest;
  TUploadLineItemsResponseArray = Array of TUploadLineItemsResponse;
  TUploadStatusArray = Array of TUploadStatus;
  //Anonymous types, using auto-generated names
  TListQueriesResponseTypequeriesArray = Array of TQuery;
  TListReportsResponseTypereportsArray = Array of TReport;
  TParametersTypefiltersArray = Array of TFilterPair;
  TUploadStatusTyperowStatusArray = Array of TRowStatus;
  
  { --------------------------------------------------------------------
    TDownloadLineItemsRequest
    --------------------------------------------------------------------}
  
  TDownloadLineItemsRequest = Class(TGoogleBaseObject)
  Private
    FfilterIds : TStringArray;
    FfilterType : String;
    Fformat : String;
  Protected
    //Property setters
    Procedure SetfilterIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetfilterType(AIndex : Integer; AValue : String); virtual;
    Procedure Setformat(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property filterIds : TStringArray Index 0 Read FfilterIds Write SetfilterIds;
    Property filterType : String Index 8 Read FfilterType Write SetfilterType;
    Property format : String Index 16 Read Fformat Write Setformat;
  end;
  TDownloadLineItemsRequestClass = Class of TDownloadLineItemsRequest;
  
  { --------------------------------------------------------------------
    TDownloadLineItemsResponse
    --------------------------------------------------------------------}
  
  TDownloadLineItemsResponse = Class(TGoogleBaseObject)
  Private
    FlineItems : String;
  Protected
    //Property setters
    Procedure SetlineItems(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property lineItems : String Index 0 Read FlineItems Write SetlineItems;
  end;
  TDownloadLineItemsResponseClass = Class of TDownloadLineItemsResponse;
  
  { --------------------------------------------------------------------
    TFilterPair
    --------------------------------------------------------------------}
  
  TFilterPair = Class(TGoogleBaseObject)
  Private
    F_type : String;
    Fvalue : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property _type : String Index 0 Read F_type Write Set_type;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TFilterPairClass = Class of TFilterPair;
  
  { --------------------------------------------------------------------
    TListQueriesResponse
    --------------------------------------------------------------------}
  
  TListQueriesResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fqueries : TListQueriesResponseTypequeriesArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setqueries(AIndex : Integer; AValue : TListQueriesResponseTypequeriesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property queries : TListQueriesResponseTypequeriesArray Index 8 Read Fqueries Write Setqueries;
  end;
  TListQueriesResponseClass = Class of TListQueriesResponse;
  
  { --------------------------------------------------------------------
    TListReportsResponse
    --------------------------------------------------------------------}
  
  TListReportsResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Freports : TListReportsResponseTypereportsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setreports(AIndex : Integer; AValue : TListReportsResponseTypereportsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property reports : TListReportsResponseTypereportsArray Index 8 Read Freports Write Setreports;
  end;
  TListReportsResponseClass = Class of TListReportsResponse;
  
  { --------------------------------------------------------------------
    TParameters
    --------------------------------------------------------------------}
  
  TParameters = Class(TGoogleBaseObject)
  Private
    Ffilters : TParametersTypefiltersArray;
    FgroupBys : TStringArray;
    FincludeInviteData : boolean;
    Fmetrics : TStringArray;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setfilters(AIndex : Integer; AValue : TParametersTypefiltersArray); virtual;
    Procedure SetgroupBys(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetincludeInviteData(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setmetrics(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property filters : TParametersTypefiltersArray Index 0 Read Ffilters Write Setfilters;
    Property groupBys : TStringArray Index 8 Read FgroupBys Write SetgroupBys;
    Property includeInviteData : boolean Index 16 Read FincludeInviteData Write SetincludeInviteData;
    Property metrics : TStringArray Index 24 Read Fmetrics Write Setmetrics;
    Property _type : String Index 32 Read F_type Write Set_type;
  end;
  TParametersClass = Class of TParameters;
  
  { --------------------------------------------------------------------
    TQuery
    --------------------------------------------------------------------}
  
  TQuery = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fmetadata : TQueryMetadata;
    Fparams : TParameters;
    FqueryId : String;
    FreportDataEndTimeMs : String;
    FreportDataStartTimeMs : String;
    Fschedule : TQuerySchedule;
    FtimezoneCode : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setmetadata(AIndex : Integer; AValue : TQueryMetadata); virtual;
    Procedure Setparams(AIndex : Integer; AValue : TParameters); virtual;
    Procedure SetqueryId(AIndex : Integer; AValue : String); virtual;
    Procedure SetreportDataEndTimeMs(AIndex : Integer; AValue : String); virtual;
    Procedure SetreportDataStartTimeMs(AIndex : Integer; AValue : String); virtual;
    Procedure Setschedule(AIndex : Integer; AValue : TQuerySchedule); virtual;
    Procedure SettimezoneCode(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property metadata : TQueryMetadata Index 8 Read Fmetadata Write Setmetadata;
    Property params : TParameters Index 16 Read Fparams Write Setparams;
    Property queryId : String Index 24 Read FqueryId Write SetqueryId;
    Property reportDataEndTimeMs : String Index 32 Read FreportDataEndTimeMs Write SetreportDataEndTimeMs;
    Property reportDataStartTimeMs : String Index 40 Read FreportDataStartTimeMs Write SetreportDataStartTimeMs;
    Property schedule : TQuerySchedule Index 48 Read Fschedule Write Setschedule;
    Property timezoneCode : String Index 56 Read FtimezoneCode Write SettimezoneCode;
  end;
  TQueryClass = Class of TQuery;
  
  { --------------------------------------------------------------------
    TQueryMetadata
    --------------------------------------------------------------------}
  
  TQueryMetadata = Class(TGoogleBaseObject)
  Private
    FdataRange : String;
    Fformat : String;
    FgoogleCloudStoragePathForLatestReport : String;
    FgoogleDrivePathForLatestReport : String;
    FlatestReportRunTimeMs : String;
    Flocale : String;
    FreportCount : integer;
    Frunning : boolean;
    FsendNotification : boolean;
    FshareEmailAddress : TStringArray;
    Ftitle : String;
  Protected
    //Property setters
    Procedure SetdataRange(AIndex : Integer; AValue : String); virtual;
    Procedure Setformat(AIndex : Integer; AValue : String); virtual;
    Procedure SetgoogleCloudStoragePathForLatestReport(AIndex : Integer; AValue : String); virtual;
    Procedure SetgoogleDrivePathForLatestReport(AIndex : Integer; AValue : String); virtual;
    Procedure SetlatestReportRunTimeMs(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocale(AIndex : Integer; AValue : String); virtual;
    Procedure SetreportCount(AIndex : Integer; AValue : integer); virtual;
    Procedure Setrunning(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsendNotification(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetshareEmailAddress(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property dataRange : String Index 0 Read FdataRange Write SetdataRange;
    Property format : String Index 8 Read Fformat Write Setformat;
    Property googleCloudStoragePathForLatestReport : String Index 16 Read FgoogleCloudStoragePathForLatestReport Write SetgoogleCloudStoragePathForLatestReport;
    Property googleDrivePathForLatestReport : String Index 24 Read FgoogleDrivePathForLatestReport Write SetgoogleDrivePathForLatestReport;
    Property latestReportRunTimeMs : String Index 32 Read FlatestReportRunTimeMs Write SetlatestReportRunTimeMs;
    Property locale : String Index 40 Read Flocale Write Setlocale;
    Property reportCount : integer Index 48 Read FreportCount Write SetreportCount;
    Property running : boolean Index 56 Read Frunning Write Setrunning;
    Property sendNotification : boolean Index 64 Read FsendNotification Write SetsendNotification;
    Property shareEmailAddress : TStringArray Index 72 Read FshareEmailAddress Write SetshareEmailAddress;
    Property title : String Index 80 Read Ftitle Write Settitle;
  end;
  TQueryMetadataClass = Class of TQueryMetadata;
  
  { --------------------------------------------------------------------
    TQuerySchedule
    --------------------------------------------------------------------}
  
  TQuerySchedule = Class(TGoogleBaseObject)
  Private
    FendTimeMs : String;
    Ffrequency : String;
    FnextRunMinuteOfDay : integer;
    FnextRunTimezoneCode : String;
  Protected
    //Property setters
    Procedure SetendTimeMs(AIndex : Integer; AValue : String); virtual;
    Procedure Setfrequency(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextRunMinuteOfDay(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnextRunTimezoneCode(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property endTimeMs : String Index 0 Read FendTimeMs Write SetendTimeMs;
    Property frequency : String Index 8 Read Ffrequency Write Setfrequency;
    Property nextRunMinuteOfDay : integer Index 16 Read FnextRunMinuteOfDay Write SetnextRunMinuteOfDay;
    Property nextRunTimezoneCode : String Index 24 Read FnextRunTimezoneCode Write SetnextRunTimezoneCode;
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
    FerrorCode : String;
  Protected
    //Property setters
    Procedure SeterrorCode(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property errorCode : String Index 0 Read FerrorCode Write SeterrorCode;
  end;
  TReportFailureClass = Class of TReportFailure;
  
  { --------------------------------------------------------------------
    TReportKey
    --------------------------------------------------------------------}
  
  TReportKey = Class(TGoogleBaseObject)
  Private
    FqueryId : String;
    FreportId : String;
  Protected
    //Property setters
    Procedure SetqueryId(AIndex : Integer; AValue : String); virtual;
    Procedure SetreportId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property queryId : String Index 0 Read FqueryId Write SetqueryId;
    Property reportId : String Index 8 Read FreportId Write SetreportId;
  end;
  TReportKeyClass = Class of TReportKey;
  
  { --------------------------------------------------------------------
    TReportMetadata
    --------------------------------------------------------------------}
  
  TReportMetadata = Class(TGoogleBaseObject)
  Private
    FgoogleCloudStoragePath : String;
    FreportDataEndTimeMs : String;
    FreportDataStartTimeMs : String;
    Fstatus : TReportStatus;
  Protected
    //Property setters
    Procedure SetgoogleCloudStoragePath(AIndex : Integer; AValue : String); virtual;
    Procedure SetreportDataEndTimeMs(AIndex : Integer; AValue : String); virtual;
    Procedure SetreportDataStartTimeMs(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : TReportStatus); virtual;
  Public
  Published
    Property googleCloudStoragePath : String Index 0 Read FgoogleCloudStoragePath Write SetgoogleCloudStoragePath;
    Property reportDataEndTimeMs : String Index 8 Read FreportDataEndTimeMs Write SetreportDataEndTimeMs;
    Property reportDataStartTimeMs : String Index 16 Read FreportDataStartTimeMs Write SetreportDataStartTimeMs;
    Property status : TReportStatus Index 24 Read Fstatus Write Setstatus;
  end;
  TReportMetadataClass = Class of TReportMetadata;
  
  { --------------------------------------------------------------------
    TReportStatus
    --------------------------------------------------------------------}
  
  TReportStatus = Class(TGoogleBaseObject)
  Private
    Ffailure : TReportFailure;
    FfinishTimeMs : String;
    Fformat : String;
    Fstate : String;
  Protected
    //Property setters
    Procedure Setfailure(AIndex : Integer; AValue : TReportFailure); virtual;
    Procedure SetfinishTimeMs(AIndex : Integer; AValue : String); virtual;
    Procedure Setformat(AIndex : Integer; AValue : String); virtual;
    Procedure Setstate(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property failure : TReportFailure Index 0 Read Ffailure Write Setfailure;
    Property finishTimeMs : String Index 8 Read FfinishTimeMs Write SetfinishTimeMs;
    Property format : String Index 16 Read Fformat Write Setformat;
    Property state : String Index 24 Read Fstate Write Setstate;
  end;
  TReportStatusClass = Class of TReportStatus;
  
  { --------------------------------------------------------------------
    TRowStatus
    --------------------------------------------------------------------}
  
  TRowStatus = Class(TGoogleBaseObject)
  Private
    Fchanged : boolean;
    FentityId : String;
    FentityName : String;
    Ferrors : TStringArray;
    Fpersisted : boolean;
    FrowNumber : integer;
  Protected
    //Property setters
    Procedure Setchanged(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetentityId(AIndex : Integer; AValue : String); virtual;
    Procedure SetentityName(AIndex : Integer; AValue : String); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setpersisted(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetrowNumber(AIndex : Integer; AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property changed : boolean Index 0 Read Fchanged Write Setchanged;
    Property entityId : String Index 8 Read FentityId Write SetentityId;
    Property entityName : String Index 16 Read FentityName Write SetentityName;
    Property errors : TStringArray Index 24 Read Ferrors Write Seterrors;
    Property persisted : boolean Index 32 Read Fpersisted Write Setpersisted;
    Property rowNumber : integer Index 40 Read FrowNumber Write SetrowNumber;
  end;
  TRowStatusClass = Class of TRowStatus;
  
  { --------------------------------------------------------------------
    TRunQueryRequest
    --------------------------------------------------------------------}
  
  TRunQueryRequest = Class(TGoogleBaseObject)
  Private
    FdataRange : String;
    FreportDataEndTimeMs : String;
    FreportDataStartTimeMs : String;
    FtimezoneCode : String;
  Protected
    //Property setters
    Procedure SetdataRange(AIndex : Integer; AValue : String); virtual;
    Procedure SetreportDataEndTimeMs(AIndex : Integer; AValue : String); virtual;
    Procedure SetreportDataStartTimeMs(AIndex : Integer; AValue : String); virtual;
    Procedure SettimezoneCode(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property dataRange : String Index 0 Read FdataRange Write SetdataRange;
    Property reportDataEndTimeMs : String Index 8 Read FreportDataEndTimeMs Write SetreportDataEndTimeMs;
    Property reportDataStartTimeMs : String Index 16 Read FreportDataStartTimeMs Write SetreportDataStartTimeMs;
    Property timezoneCode : String Index 24 Read FtimezoneCode Write SettimezoneCode;
  end;
  TRunQueryRequestClass = Class of TRunQueryRequest;
  
  { --------------------------------------------------------------------
    TUploadLineItemsRequest
    --------------------------------------------------------------------}
  
  TUploadLineItemsRequest = Class(TGoogleBaseObject)
  Private
    FdryRun : boolean;
    Fformat : String;
    FlineItems : String;
  Protected
    //Property setters
    Procedure SetdryRun(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setformat(AIndex : Integer; AValue : String); virtual;
    Procedure SetlineItems(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property dryRun : boolean Index 0 Read FdryRun Write SetdryRun;
    Property format : String Index 8 Read Fformat Write Setformat;
    Property lineItems : String Index 16 Read FlineItems Write SetlineItems;
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
    Ferrors : TStringArray;
    FrowStatus : TUploadStatusTyperowStatusArray;
  Protected
    //Property setters
    Procedure Seterrors(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetrowStatus(AIndex : Integer; AValue : TUploadStatusTyperowStatusArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property errors : TStringArray Index 0 Read Ferrors Write Seterrors;
    Property rowStatus : TUploadStatusTyperowStatusArray Index 8 Read FrowStatus Write SetrowStatus;
  end;
  TUploadStatusClass = Class of TUploadStatus;
  
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


Procedure TDownloadLineItemsRequest.SetfilterIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FfilterIds=AValue) then exit;
  FfilterIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadLineItemsRequest.SetfilterType(AIndex : Integer; AValue : String); 

begin
  If (FfilterType=AValue) then exit;
  FfilterType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDownloadLineItemsRequest.Setformat(AIndex : Integer; AValue : String); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDownloadLineItemsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'filterids' : SetLength(FfilterIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDownloadLineItemsResponse
  --------------------------------------------------------------------}


Procedure TDownloadLineItemsResponse.SetlineItems(AIndex : Integer; AValue : String); 

begin
  If (FlineItems=AValue) then exit;
  FlineItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFilterPair
  --------------------------------------------------------------------}


Procedure TFilterPair.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterPair.Setvalue(AIndex : Integer; AValue : String); 

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


Procedure TListQueriesResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListQueriesResponse.Setqueries(AIndex : Integer; AValue : TListQueriesResponseTypequeriesArray); 

begin
  If (Fqueries=AValue) then exit;
  Fqueries:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListQueriesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'queries' : SetLength(Fqueries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListReportsResponse
  --------------------------------------------------------------------}


Procedure TListReportsResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListReportsResponse.Setreports(AIndex : Integer; AValue : TListReportsResponseTypereportsArray); 

begin
  If (Freports=AValue) then exit;
  Freports:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListReportsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'reports' : SetLength(Freports,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TParameters
  --------------------------------------------------------------------}


Procedure TParameters.Setfilters(AIndex : Integer; AValue : TParametersTypefiltersArray); 

begin
  If (Ffilters=AValue) then exit;
  Ffilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParameters.SetgroupBys(AIndex : Integer; AValue : TStringArray); 

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



Procedure TParameters.Setmetrics(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParameters.Set_type(AIndex : Integer; AValue : String); 

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

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TParameters.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'filters' : SetLength(Ffilters,ALength);
  'groupbys' : SetLength(FgroupBys,ALength);
  'metrics' : SetLength(Fmetrics,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TQuery
  --------------------------------------------------------------------}


Procedure TQuery.Setkind(AIndex : Integer; AValue : String); 

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



Procedure TQuery.SetqueryId(AIndex : Integer; AValue : String); 

begin
  If (FqueryId=AValue) then exit;
  FqueryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetreportDataEndTimeMs(AIndex : Integer; AValue : String); 

begin
  If (FreportDataEndTimeMs=AValue) then exit;
  FreportDataEndTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetreportDataStartTimeMs(AIndex : Integer; AValue : String); 

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



Procedure TQuery.SettimezoneCode(AIndex : Integer; AValue : String); 

begin
  If (FtimezoneCode=AValue) then exit;
  FtimezoneCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TQueryMetadata
  --------------------------------------------------------------------}


Procedure TQueryMetadata.SetdataRange(AIndex : Integer; AValue : String); 

begin
  If (FdataRange=AValue) then exit;
  FdataRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryMetadata.Setformat(AIndex : Integer; AValue : String); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryMetadata.SetgoogleCloudStoragePathForLatestReport(AIndex : Integer; AValue : String); 

begin
  If (FgoogleCloudStoragePathForLatestReport=AValue) then exit;
  FgoogleCloudStoragePathForLatestReport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryMetadata.SetgoogleDrivePathForLatestReport(AIndex : Integer; AValue : String); 

begin
  If (FgoogleDrivePathForLatestReport=AValue) then exit;
  FgoogleDrivePathForLatestReport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryMetadata.SetlatestReportRunTimeMs(AIndex : Integer; AValue : String); 

begin
  If (FlatestReportRunTimeMs=AValue) then exit;
  FlatestReportRunTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryMetadata.Setlocale(AIndex : Integer; AValue : String); 

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



Procedure TQueryMetadata.SetshareEmailAddress(AIndex : Integer; AValue : TStringArray); 

begin
  If (FshareEmailAddress=AValue) then exit;
  FshareEmailAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryMetadata.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TQueryMetadata.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'shareemailaddress' : SetLength(FshareEmailAddress,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TQuerySchedule
  --------------------------------------------------------------------}


Procedure TQuerySchedule.SetendTimeMs(AIndex : Integer; AValue : String); 

begin
  If (FendTimeMs=AValue) then exit;
  FendTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuerySchedule.Setfrequency(AIndex : Integer; AValue : String); 

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



Procedure TQuerySchedule.SetnextRunTimezoneCode(AIndex : Integer; AValue : String); 

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


Procedure TReportFailure.SeterrorCode(AIndex : Integer; AValue : String); 

begin
  If (FerrorCode=AValue) then exit;
  FerrorCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportKey
  --------------------------------------------------------------------}


Procedure TReportKey.SetqueryId(AIndex : Integer; AValue : String); 

begin
  If (FqueryId=AValue) then exit;
  FqueryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportKey.SetreportId(AIndex : Integer; AValue : String); 

begin
  If (FreportId=AValue) then exit;
  FreportId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportMetadata
  --------------------------------------------------------------------}


Procedure TReportMetadata.SetgoogleCloudStoragePath(AIndex : Integer; AValue : String); 

begin
  If (FgoogleCloudStoragePath=AValue) then exit;
  FgoogleCloudStoragePath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportMetadata.SetreportDataEndTimeMs(AIndex : Integer; AValue : String); 

begin
  If (FreportDataEndTimeMs=AValue) then exit;
  FreportDataEndTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportMetadata.SetreportDataStartTimeMs(AIndex : Integer; AValue : String); 

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



Procedure TReportStatus.SetfinishTimeMs(AIndex : Integer; AValue : String); 

begin
  If (FfinishTimeMs=AValue) then exit;
  FfinishTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportStatus.Setformat(AIndex : Integer; AValue : String); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportStatus.Setstate(AIndex : Integer; AValue : String); 

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



Procedure TRowStatus.SetentityId(AIndex : Integer; AValue : String); 

begin
  If (FentityId=AValue) then exit;
  FentityId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRowStatus.SetentityName(AIndex : Integer; AValue : String); 

begin
  If (FentityName=AValue) then exit;
  FentityName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRowStatus.Seterrors(AIndex : Integer; AValue : TStringArray); 

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


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TRowStatus.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errors' : SetLength(Ferrors,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TRunQueryRequest
  --------------------------------------------------------------------}


Procedure TRunQueryRequest.SetdataRange(AIndex : Integer; AValue : String); 

begin
  If (FdataRange=AValue) then exit;
  FdataRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRunQueryRequest.SetreportDataEndTimeMs(AIndex : Integer; AValue : String); 

begin
  If (FreportDataEndTimeMs=AValue) then exit;
  FreportDataEndTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRunQueryRequest.SetreportDataStartTimeMs(AIndex : Integer; AValue : String); 

begin
  If (FreportDataStartTimeMs=AValue) then exit;
  FreportDataStartTimeMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRunQueryRequest.SettimezoneCode(AIndex : Integer; AValue : String); 

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



Procedure TUploadLineItemsRequest.Setformat(AIndex : Integer; AValue : String); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUploadLineItemsRequest.SetlineItems(AIndex : Integer; AValue : String); 

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


Procedure TUploadStatus.Seterrors(AIndex : Integer; AValue : TStringArray); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUploadStatus.SetrowStatus(AIndex : Integer; AValue : TUploadStatusTyperowStatusArray); 

begin
  If (FrowStatus=AValue) then exit;
  FrowStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TUploadStatus.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errors' : SetLength(Ferrors,ALength);
  'rowstatus' : SetLength(FrowStatus,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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
  Result:='https://www.googleapis.com:443/';
end;

Class Function TDoubleclickbidmanagerAPI.APIbasePath : string;

begin
  Result:='/doubleclickbidmanager/v1/';
end;

Class Function TDoubleclickbidmanagerAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/doubleclickbidmanager/v1/';
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
  TDownloadLineItemsResponse.RegisterObject;
  TFilterPair.RegisterObject;
  TListQueriesResponse.RegisterObject;
  TListReportsResponse.RegisterObject;
  TParameters.RegisterObject;
  TQuery.RegisterObject;
  TQueryMetadata.RegisterObject;
  TQuerySchedule.RegisterObject;
  TReport.RegisterObject;
  TReportFailure.RegisterObject;
  TReportKey.RegisterObject;
  TReportMetadata.RegisterObject;
  TReportStatus.RegisterObject;
  TRowStatus.RegisterObject;
  TRunQueryRequest.RegisterObject;
  TUploadLineItemsRequest.RegisterObject;
  TUploadLineItemsResponse.RegisterObject;
  TUploadStatus.RegisterObject;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
end;



initialization
  TDoubleclickbidmanagerAPI.RegisterAPI;
end.
