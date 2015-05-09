unit googlelogging;
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
//Generated on: 9-5-15 13:22:55
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TListLogsResponse = class;
  TLog = class;
  TEmpty = class;
  TWriteLogEntriesRequest = class;
  TLogEntry = class;
  TLogEntryMetadata = class;
  TWriteLogEntriesResponse = class;
  TListLogServicesResponse = class;
  TLogService = class;
  TListLogServiceIndexesResponse = class;
  TListLogSinksResponse = class;
  TLogSink = class;
  TLogError = class;
  TStatus = class;
  TListLogServiceSinksResponse = class;
  TListLogsResponseArray = Array of TListLogsResponse;
  TLogArray = Array of TLog;
  TEmptyArray = Array of TEmpty;
  TWriteLogEntriesRequestArray = Array of TWriteLogEntriesRequest;
  TLogEntryArray = Array of TLogEntry;
  TLogEntryMetadataArray = Array of TLogEntryMetadata;
  TWriteLogEntriesResponseArray = Array of TWriteLogEntriesResponse;
  TListLogServicesResponseArray = Array of TListLogServicesResponse;
  TLogServiceArray = Array of TLogService;
  TListLogServiceIndexesResponseArray = Array of TListLogServiceIndexesResponse;
  TListLogSinksResponseArray = Array of TListLogSinksResponse;
  TLogSinkArray = Array of TLogSink;
  TLogErrorArray = Array of TLogError;
  TStatusArray = Array of TStatus;
  TListLogServiceSinksResponseArray = Array of TListLogServiceSinksResponse;
  //Anonymous types, using auto-generated names
  TWriteLogEntriesRequestTypecommonLabels = class;
  TLogEntryTypeprotoPayload = class;
  TLogEntryTypestructPayload = class;
  TLogEntryMetadataTypelabels = class;
  TStatusTypedetailsItem = class;
  TListLogsResponseTypelogsArray = Array of TLog;
  TWriteLogEntriesRequestTypeentriesArray = Array of TLogEntry;
  TListLogServicesResponseTypelogServicesArray = Array of TLogService;
  TListLogSinksResponseTypesinksArray = Array of TLogSink;
  TLogSinkTypeerrorsArray = Array of TLogError;
  TStatusTypedetailsArray = Array of TStatusTypedetailsItem;
  TListLogServiceSinksResponseTypesinksArray = Array of TLogSink;
  
  { --------------------------------------------------------------------
    TListLogsResponse
    --------------------------------------------------------------------}
  
  TListLogsResponse = Class(TGoogleBaseObject)
  Private
    Flogs : TListLogsResponseTypelogsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setlogs(AIndex : Integer; AValue : TListLogsResponseTypelogsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property logs : TListLogsResponseTypelogsArray Index 0 Read Flogs Write Setlogs;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListLogsResponseClass = Class of TListLogsResponse;
  
  { --------------------------------------------------------------------
    TLog
    --------------------------------------------------------------------}
  
  TLog = Class(TGoogleBaseObject)
  Private
    Fname : String;
    FdisplayName : String;
    FpayloadType : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure SetpayloadType(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property displayName : String Index 8 Read FdisplayName Write SetdisplayName;
    Property payloadType : String Index 16 Read FpayloadType Write SetpayloadType;
  end;
  TLogClass = Class of TLog;
  
  { --------------------------------------------------------------------
    TEmpty
    --------------------------------------------------------------------}
  
  TEmpty = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEmptyClass = Class of TEmpty;
  
  { --------------------------------------------------------------------
    TWriteLogEntriesRequestTypecommonLabels
    --------------------------------------------------------------------}
  
  TWriteLogEntriesRequestTypecommonLabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TWriteLogEntriesRequestTypecommonLabelsClass = Class of TWriteLogEntriesRequestTypecommonLabels;
  
  { --------------------------------------------------------------------
    TWriteLogEntriesRequest
    --------------------------------------------------------------------}
  
  TWriteLogEntriesRequest = Class(TGoogleBaseObject)
  Private
    FcommonLabels : TWriteLogEntriesRequestTypecommonLabels;
    Fentries : TWriteLogEntriesRequestTypeentriesArray;
  Protected
    //Property setters
    Procedure SetcommonLabels(AIndex : Integer; AValue : TWriteLogEntriesRequestTypecommonLabels); virtual;
    Procedure Setentries(AIndex : Integer; AValue : TWriteLogEntriesRequestTypeentriesArray); virtual;
  Public
  Published
    Property commonLabels : TWriteLogEntriesRequestTypecommonLabels Index 0 Read FcommonLabels Write SetcommonLabels;
    Property entries : TWriteLogEntriesRequestTypeentriesArray Index 8 Read Fentries Write Setentries;
  end;
  TWriteLogEntriesRequestClass = Class of TWriteLogEntriesRequest;
  
  { --------------------------------------------------------------------
    TLogEntryTypeprotoPayload
    --------------------------------------------------------------------}
  
  TLogEntryTypeprotoPayload = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TLogEntryTypeprotoPayloadClass = Class of TLogEntryTypeprotoPayload;
  
  { --------------------------------------------------------------------
    TLogEntryTypestructPayload
    --------------------------------------------------------------------}
  
  TLogEntryTypestructPayload = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TLogEntryTypestructPayloadClass = Class of TLogEntryTypestructPayload;
  
  { --------------------------------------------------------------------
    TLogEntry
    --------------------------------------------------------------------}
  
  TLogEntry = Class(TGoogleBaseObject)
  Private
    Fmetadata : TLogEntryMetadata;
    FprotoPayload : TLogEntryTypeprotoPayload;
    FtextPayload : String;
    FstructPayload : TLogEntryTypestructPayload;
    FinsertId : String;
    Flog : String;
  Protected
    //Property setters
    Procedure Setmetadata(AIndex : Integer; AValue : TLogEntryMetadata); virtual;
    Procedure SetprotoPayload(AIndex : Integer; AValue : TLogEntryTypeprotoPayload); virtual;
    Procedure SettextPayload(AIndex : Integer; AValue : String); virtual;
    Procedure SetstructPayload(AIndex : Integer; AValue : TLogEntryTypestructPayload); virtual;
    Procedure SetinsertId(AIndex : Integer; AValue : String); virtual;
    Procedure Setlog(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property metadata : TLogEntryMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property protoPayload : TLogEntryTypeprotoPayload Index 8 Read FprotoPayload Write SetprotoPayload;
    Property textPayload : String Index 16 Read FtextPayload Write SettextPayload;
    Property structPayload : TLogEntryTypestructPayload Index 24 Read FstructPayload Write SetstructPayload;
    Property insertId : String Index 32 Read FinsertId Write SetinsertId;
    Property log : String Index 40 Read Flog Write Setlog;
  end;
  TLogEntryClass = Class of TLogEntry;
  
  { --------------------------------------------------------------------
    TLogEntryMetadataTypelabels
    --------------------------------------------------------------------}
  
  TLogEntryMetadataTypelabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TLogEntryMetadataTypelabelsClass = Class of TLogEntryMetadataTypelabels;
  
  { --------------------------------------------------------------------
    TLogEntryMetadata
    --------------------------------------------------------------------}
  
  TLogEntryMetadata = Class(TGoogleBaseObject)
  Private
    Ftimestamp : String;
    Fseverity : String;
    FprojectId : String;
    FserviceName : String;
    Fregion : String;
    Fzone : String;
    FuserId : String;
    Flabels : TLogEntryMetadataTypelabels;
  Protected
    //Property setters
    Procedure Settimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure Setseverity(AIndex : Integer; AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : String); virtual;
    Procedure SetserviceName(AIndex : Integer; AValue : String); virtual;
    Procedure Setregion(AIndex : Integer; AValue : String); virtual;
    Procedure Setzone(AIndex : Integer; AValue : String); virtual;
    Procedure SetuserId(AIndex : Integer; AValue : String); virtual;
    Procedure Setlabels(AIndex : Integer; AValue : TLogEntryMetadataTypelabels); virtual;
  Public
  Published
    Property timestamp : String Index 0 Read Ftimestamp Write Settimestamp;
    Property severity : String Index 8 Read Fseverity Write Setseverity;
    Property projectId : String Index 16 Read FprojectId Write SetprojectId;
    Property serviceName : String Index 24 Read FserviceName Write SetserviceName;
    Property region : String Index 32 Read Fregion Write Setregion;
    Property zone : String Index 40 Read Fzone Write Setzone;
    Property userId : String Index 48 Read FuserId Write SetuserId;
    Property labels : TLogEntryMetadataTypelabels Index 56 Read Flabels Write Setlabels;
  end;
  TLogEntryMetadataClass = Class of TLogEntryMetadata;
  
  { --------------------------------------------------------------------
    TWriteLogEntriesResponse
    --------------------------------------------------------------------}
  
  TWriteLogEntriesResponse = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TWriteLogEntriesResponseClass = Class of TWriteLogEntriesResponse;
  
  { --------------------------------------------------------------------
    TListLogServicesResponse
    --------------------------------------------------------------------}
  
  TListLogServicesResponse = Class(TGoogleBaseObject)
  Private
    FlogServices : TListLogServicesResponseTypelogServicesArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetlogServices(AIndex : Integer; AValue : TListLogServicesResponseTypelogServicesArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property logServices : TListLogServicesResponseTypelogServicesArray Index 0 Read FlogServices Write SetlogServices;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListLogServicesResponseClass = Class of TListLogServicesResponse;
  
  { --------------------------------------------------------------------
    TLogService
    --------------------------------------------------------------------}
  
  TLogService = Class(TGoogleBaseObject)
  Private
    Fname : String;
    FindexKeys : TStringArray;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetindexKeys(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property indexKeys : TStringArray Index 8 Read FindexKeys Write SetindexKeys;
  end;
  TLogServiceClass = Class of TLogService;
  
  { --------------------------------------------------------------------
    TListLogServiceIndexesResponse
    --------------------------------------------------------------------}
  
  TListLogServiceIndexesResponse = Class(TGoogleBaseObject)
  Private
    FserviceIndexPrefixes : TStringArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetserviceIndexPrefixes(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property serviceIndexPrefixes : TStringArray Index 0 Read FserviceIndexPrefixes Write SetserviceIndexPrefixes;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListLogServiceIndexesResponseClass = Class of TListLogServiceIndexesResponse;
  
  { --------------------------------------------------------------------
    TListLogSinksResponse
    --------------------------------------------------------------------}
  
  TListLogSinksResponse = Class(TGoogleBaseObject)
  Private
    Fsinks : TListLogSinksResponseTypesinksArray;
  Protected
    //Property setters
    Procedure Setsinks(AIndex : Integer; AValue : TListLogSinksResponseTypesinksArray); virtual;
  Public
  Published
    Property sinks : TListLogSinksResponseTypesinksArray Index 0 Read Fsinks Write Setsinks;
  end;
  TListLogSinksResponseClass = Class of TListLogSinksResponse;
  
  { --------------------------------------------------------------------
    TLogSink
    --------------------------------------------------------------------}
  
  TLogSink = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fdestination : String;
    Ferrors : TLogSinkTypeerrorsArray;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setdestination(AIndex : Integer; AValue : String); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TLogSinkTypeerrorsArray); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property destination : String Index 8 Read Fdestination Write Setdestination;
    Property errors : TLogSinkTypeerrorsArray Index 16 Read Ferrors Write Seterrors;
  end;
  TLogSinkClass = Class of TLogSink;
  
  { --------------------------------------------------------------------
    TLogError
    --------------------------------------------------------------------}
  
  TLogError = Class(TGoogleBaseObject)
  Private
    Fresource : String;
    Fstatus : TStatus;
    FtimeNanos : String;
  Protected
    //Property setters
    Procedure Setresource(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : TStatus); virtual;
    Procedure SettimeNanos(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property resource : String Index 0 Read Fresource Write Setresource;
    Property status : TStatus Index 8 Read Fstatus Write Setstatus;
    Property timeNanos : String Index 16 Read FtimeNanos Write SettimeNanos;
  end;
  TLogErrorClass = Class of TLogError;
  
  { --------------------------------------------------------------------
    TStatusTypedetailsItem
    --------------------------------------------------------------------}
  
  TStatusTypedetailsItem = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TStatusTypedetailsItemClass = Class of TStatusTypedetailsItem;
  
  { --------------------------------------------------------------------
    TStatus
    --------------------------------------------------------------------}
  
  TStatus = Class(TGoogleBaseObject)
  Private
    Fcode : integer;
    Fmessage : String;
    Fdetails : TStatusTypedetailsArray;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : integer); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : String); virtual;
    Procedure Setdetails(AIndex : Integer; AValue : TStatusTypedetailsArray); virtual;
  Public
  Published
    Property code : integer Index 0 Read Fcode Write Setcode;
    Property message : String Index 8 Read Fmessage Write Setmessage;
    Property details : TStatusTypedetailsArray Index 16 Read Fdetails Write Setdetails;
  end;
  TStatusClass = Class of TStatus;
  
  { --------------------------------------------------------------------
    TListLogServiceSinksResponse
    --------------------------------------------------------------------}
  
  TListLogServiceSinksResponse = Class(TGoogleBaseObject)
  Private
    Fsinks : TListLogServiceSinksResponseTypesinksArray;
  Protected
    //Property setters
    Procedure Setsinks(AIndex : Integer; AValue : TListLogServiceSinksResponseTypesinksArray); virtual;
  Public
  Published
    Property sinks : TListLogServiceSinksResponseTypesinksArray Index 0 Read Fsinks Write Setsinks;
  end;
  TListLogServiceSinksResponseClass = Class of TListLogServiceSinksResponse;
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
  end;
  
  
  { --------------------------------------------------------------------
    TLoggingAPI
    --------------------------------------------------------------------}
  
  TLoggingAPI = Class(TGoogleAPI)
  Private
    FProjectsInstance : TProjectsResource;
    Function GetProjectsInstance : TProjectsResource;virtual;
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
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TListLogsResponse
  --------------------------------------------------------------------}


Procedure TListLogsResponse.Setlogs(AIndex : Integer; AValue : TListLogsResponseTypelogsArray); 

begin
  If (Flogs=AValue) then exit;
  Flogs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListLogsResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLog
  --------------------------------------------------------------------}


Procedure TLog.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLog.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLog.SetpayloadType(AIndex : Integer; AValue : String); 

begin
  If (FpayloadType=AValue) then exit;
  FpayloadType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEmpty
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TWriteLogEntriesRequestTypecommonLabels
  --------------------------------------------------------------------}


Class Function TWriteLogEntriesRequestTypecommonLabels.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TWriteLogEntriesRequest
  --------------------------------------------------------------------}


Procedure TWriteLogEntriesRequest.SetcommonLabels(AIndex : Integer; AValue : TWriteLogEntriesRequestTypecommonLabels); 

begin
  If (FcommonLabels=AValue) then exit;
  FcommonLabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWriteLogEntriesRequest.Setentries(AIndex : Integer; AValue : TWriteLogEntriesRequestTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLogEntryTypeprotoPayload
  --------------------------------------------------------------------}


Class Function TLogEntryTypeprotoPayload.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TLogEntryTypestructPayload
  --------------------------------------------------------------------}


Class Function TLogEntryTypestructPayload.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TLogEntry
  --------------------------------------------------------------------}


Procedure TLogEntry.Setmetadata(AIndex : Integer; AValue : TLogEntryMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntry.SetprotoPayload(AIndex : Integer; AValue : TLogEntryTypeprotoPayload); 

begin
  If (FprotoPayload=AValue) then exit;
  FprotoPayload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntry.SettextPayload(AIndex : Integer; AValue : String); 

begin
  If (FtextPayload=AValue) then exit;
  FtextPayload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntry.SetstructPayload(AIndex : Integer; AValue : TLogEntryTypestructPayload); 

begin
  If (FstructPayload=AValue) then exit;
  FstructPayload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntry.SetinsertId(AIndex : Integer; AValue : String); 

begin
  If (FinsertId=AValue) then exit;
  FinsertId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntry.Setlog(AIndex : Integer; AValue : String); 

begin
  If (Flog=AValue) then exit;
  Flog:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLogEntryMetadataTypelabels
  --------------------------------------------------------------------}


Class Function TLogEntryMetadataTypelabels.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TLogEntryMetadata
  --------------------------------------------------------------------}


Procedure TLogEntryMetadata.Settimestamp(AIndex : Integer; AValue : String); 

begin
  If (Ftimestamp=AValue) then exit;
  Ftimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntryMetadata.Setseverity(AIndex : Integer; AValue : String); 

begin
  If (Fseverity=AValue) then exit;
  Fseverity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntryMetadata.SetprojectId(AIndex : Integer; AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntryMetadata.SetserviceName(AIndex : Integer; AValue : String); 

begin
  If (FserviceName=AValue) then exit;
  FserviceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntryMetadata.Setregion(AIndex : Integer; AValue : String); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntryMetadata.Setzone(AIndex : Integer; AValue : String); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntryMetadata.SetuserId(AIndex : Integer; AValue : String); 

begin
  If (FuserId=AValue) then exit;
  FuserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntryMetadata.Setlabels(AIndex : Integer; AValue : TLogEntryMetadataTypelabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWriteLogEntriesResponse
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListLogServicesResponse
  --------------------------------------------------------------------}


Procedure TListLogServicesResponse.SetlogServices(AIndex : Integer; AValue : TListLogServicesResponseTypelogServicesArray); 

begin
  If (FlogServices=AValue) then exit;
  FlogServices:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListLogServicesResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLogService
  --------------------------------------------------------------------}


Procedure TLogService.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogService.SetindexKeys(AIndex : Integer; AValue : TStringArray); 

begin
  If (FindexKeys=AValue) then exit;
  FindexKeys:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListLogServiceIndexesResponse
  --------------------------------------------------------------------}


Procedure TListLogServiceIndexesResponse.SetserviceIndexPrefixes(AIndex : Integer; AValue : TStringArray); 

begin
  If (FserviceIndexPrefixes=AValue) then exit;
  FserviceIndexPrefixes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListLogServiceIndexesResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListLogSinksResponse
  --------------------------------------------------------------------}


Procedure TListLogSinksResponse.Setsinks(AIndex : Integer; AValue : TListLogSinksResponseTypesinksArray); 

begin
  If (Fsinks=AValue) then exit;
  Fsinks:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLogSink
  --------------------------------------------------------------------}


Procedure TLogSink.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogSink.Setdestination(AIndex : Integer; AValue : String); 

begin
  If (Fdestination=AValue) then exit;
  Fdestination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogSink.Seterrors(AIndex : Integer; AValue : TLogSinkTypeerrorsArray); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLogError
  --------------------------------------------------------------------}


Procedure TLogError.Setresource(AIndex : Integer; AValue : String); 

begin
  If (Fresource=AValue) then exit;
  Fresource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogError.Setstatus(AIndex : Integer; AValue : TStatus); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogError.SettimeNanos(AIndex : Integer; AValue : String); 

begin
  If (FtimeNanos=AValue) then exit;
  FtimeNanos:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStatusTypedetailsItem
  --------------------------------------------------------------------}


Class Function TStatusTypedetailsItem.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TStatus
  --------------------------------------------------------------------}


Procedure TStatus.Setcode(AIndex : Integer; AValue : integer); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStatus.Setmessage(AIndex : Integer; AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStatus.Setdetails(AIndex : Integer; AValue : TStatusTypedetailsArray); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListLogServiceSinksResponse
  --------------------------------------------------------------------}


Procedure TListLogServiceSinksResponse.Setsinks(AIndex : Integer; AValue : TListLogServiceSinksResponseTypesinksArray); 

begin
  If (Fsinks=AValue) then exit;
  Fsinks:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProjectsResource
  --------------------------------------------------------------------}


Class Function TProjectsResource.ResourceName : String;

begin
  Result:='projects';
end;

Class Function TProjectsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TloggingAPI;
end;



{ --------------------------------------------------------------------
  TLoggingAPI
  --------------------------------------------------------------------}

Class Function TLoggingAPI.APIName : String;

begin
  Result:='logging';
end;

Class Function TLoggingAPI.APIVersion : String;

begin
  Result:='v1beta3';
end;

Class Function TLoggingAPI.APIRevision : String;

begin
  Result:='20150326';
end;

Class Function TLoggingAPI.APIID : String;

begin
  Result:='logging:v1beta3';
end;

Class Function TLoggingAPI.APITitle : String;

begin
  Result:='Google Cloud Logging API';
end;

Class Function TLoggingAPI.APIDescription : String;

begin
  Result:='Google Cloud Logging API lets you create logs, ingest log entries, and manage log sinks.';
end;

Class Function TLoggingAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TLoggingAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TLoggingAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TLoggingAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TLoggingAPI.APIdocumentationLink : String;

begin
  Result:='';
end;

Class Function TLoggingAPI.APIrootUrl : string;

begin
  Result:='https://logging.googleapis.com/';
end;

Class Function TLoggingAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TLoggingAPI.APIbaseURL : String;

begin
  Result:='https://logging.googleapis.com/';
end;

Class Function TLoggingAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TLoggingAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TLoggingAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TLoggingAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  
end;

Class Function TLoggingAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TLoggingAPI.RegisterAPIResources;

begin
  TListLogsResponse.RegisterObject;
  TLog.RegisterObject;
  TEmpty.RegisterObject;
  TWriteLogEntriesRequestTypecommonLabels.RegisterObject;
  TWriteLogEntriesRequest.RegisterObject;
  TLogEntryTypeprotoPayload.RegisterObject;
  TLogEntryTypestructPayload.RegisterObject;
  TLogEntry.RegisterObject;
  TLogEntryMetadataTypelabels.RegisterObject;
  TLogEntryMetadata.RegisterObject;
  TWriteLogEntriesResponse.RegisterObject;
  TListLogServicesResponse.RegisterObject;
  TLogService.RegisterObject;
  TListLogServiceIndexesResponse.RegisterObject;
  TListLogSinksResponse.RegisterObject;
  TLogSink.RegisterObject;
  TLogError.RegisterObject;
  TStatusTypedetailsItem.RegisterObject;
  TStatus.RegisterObject;
  TListLogServiceSinksResponse.RegisterObject;
end;


Function TLoggingAPI.GetProjectsInstance : TProjectsResource;

begin
  if (FProjectsInstance=Nil) then
    FProjectsInstance:=CreateProjectsResource;
  Result:=FProjectsInstance;
end;

Function TLoggingAPI.CreateProjectsResource : TProjectsResource;

begin
  Result:=CreateProjectsResource(Self);
end;


Function TLoggingAPI.CreateProjectsResource(AOwner : TComponent) : TProjectsResource;

begin
  Result:=TProjectsResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TLoggingAPI.RegisterAPI;
end.
