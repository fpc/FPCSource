unit googlelogging;
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
  TListLogsResponse = class;
  TListLogsResponseArray = Array of TListLogsResponse;
  TListLogsResponselogs = class;
  TListLogsResponselogsArray = Array of TListLogsResponselogs;
  TLog = class;
  TLogArray = Array of TLog;
  TEmpty = class;
  TEmptyArray = Array of TEmpty;
  TWriteLogEntriesRequest = class;
  TWriteLogEntriesRequestArray = Array of TWriteLogEntriesRequest;
  TWriteLogEntriesRequestcommonLabels = class;
  TWriteLogEntriesRequestcommonLabelsArray = Array of TWriteLogEntriesRequestcommonLabels;
  TWriteLogEntriesRequestentries = class;
  TWriteLogEntriesRequestentriesArray = Array of TWriteLogEntriesRequestentries;
  TLogEntry = class;
  TLogEntryArray = Array of TLogEntry;
  TLogEntryprotoPayload = class;
  TLogEntryprotoPayloadArray = Array of TLogEntryprotoPayload;
  TLogEntrystructPayload = class;
  TLogEntrystructPayloadArray = Array of TLogEntrystructPayload;
  TLogEntryMetadata = class;
  TLogEntryMetadataArray = Array of TLogEntryMetadata;
  TLogEntryMetadatalabels = class;
  TLogEntryMetadatalabelsArray = Array of TLogEntryMetadatalabels;
  TWriteLogEntriesResponse = class;
  TWriteLogEntriesResponseArray = Array of TWriteLogEntriesResponse;
  TListLogServicesResponse = class;
  TListLogServicesResponseArray = Array of TListLogServicesResponse;
  TListLogServicesResponselogServices = class;
  TListLogServicesResponselogServicesArray = Array of TListLogServicesResponselogServices;
  TLogService = class;
  TLogServiceArray = Array of TLogService;
  TLogServiceindexKeys = class;
  TLogServiceindexKeysArray = Array of TLogServiceindexKeys;
  TListLogServiceIndexesResponse = class;
  TListLogServiceIndexesResponseArray = Array of TListLogServiceIndexesResponse;
  TListLogServiceIndexesResponseserviceIndexPrefixes = class;
  TListLogServiceIndexesResponseserviceIndexPrefixesArray = Array of TListLogServiceIndexesResponseserviceIndexPrefixes;
  TListLogSinksResponse = class;
  TListLogSinksResponseArray = Array of TListLogSinksResponse;
  TListLogSinksResponsesinks = class;
  TListLogSinksResponsesinksArray = Array of TListLogSinksResponsesinks;
  TLogSink = class;
  TLogSinkArray = Array of TLogSink;
  TLogSinkerrors = class;
  TLogSinkerrorsArray = Array of TLogSinkerrors;
  TLogError = class;
  TLogErrorArray = Array of TLogError;
  TStatus = class;
  TStatusArray = Array of TStatus;
  TStatusdetails = class;
  TStatusdetailsArray = Array of TStatusdetails;
  TListLogServiceSinksResponse = class;
  TListLogServiceSinksResponseArray = Array of TListLogServiceSinksResponse;
  TListLogServiceSinksResponsesinks = class;
  TListLogServiceSinksResponsesinksArray = Array of TListLogServiceSinksResponsesinks;
  
  { --------------------------------------------------------------------
    TListLogsResponse
    --------------------------------------------------------------------}
  
  TListLogsResponse = Class(TGoogleBaseObject)
  Private
    Flogs : TListLogsResponselogs;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setlogs(AIndex : Integer; AValue : TListLogsResponselogs); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property logs : TListLogsResponselogs Index 0 Read Flogs Write Setlogs;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListLogsResponseClass = Class of TListLogsResponse;
  
  { --------------------------------------------------------------------
    TListLogsResponselogs
    --------------------------------------------------------------------}
  
  TListLogsResponselogs = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListLogsResponselogsClass = Class of TListLogsResponselogs;
  
  { --------------------------------------------------------------------
    TLog
    --------------------------------------------------------------------}
  
  TLog = Class(TGoogleBaseObject)
  Private
    Fname : string;
    FdisplayName : string;
    FpayloadType : string;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure SetpayloadType(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
    Property displayName : string Index 8 Read FdisplayName Write SetdisplayName;
    Property payloadType : string Index 16 Read FpayloadType Write SetpayloadType;
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
    TWriteLogEntriesRequest
    --------------------------------------------------------------------}
  
  TWriteLogEntriesRequest = Class(TGoogleBaseObject)
  Private
    FcommonLabels : TWriteLogEntriesRequestcommonLabels;
    Fentries : TWriteLogEntriesRequestentries;
  Protected
    //Property setters
    Procedure SetcommonLabels(AIndex : Integer; AValue : TWriteLogEntriesRequestcommonLabels); virtual;
    Procedure Setentries(AIndex : Integer; AValue : TWriteLogEntriesRequestentries); virtual;
  Public
  Published
    Property commonLabels : TWriteLogEntriesRequestcommonLabels Index 0 Read FcommonLabels Write SetcommonLabels;
    Property entries : TWriteLogEntriesRequestentries Index 8 Read Fentries Write Setentries;
  end;
  TWriteLogEntriesRequestClass = Class of TWriteLogEntriesRequest;
  
  { --------------------------------------------------------------------
    TWriteLogEntriesRequestcommonLabels
    --------------------------------------------------------------------}
  
  TWriteLogEntriesRequestcommonLabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TWriteLogEntriesRequestcommonLabelsClass = Class of TWriteLogEntriesRequestcommonLabels;
  
  { --------------------------------------------------------------------
    TWriteLogEntriesRequestentries
    --------------------------------------------------------------------}
  
  TWriteLogEntriesRequestentries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TWriteLogEntriesRequestentriesClass = Class of TWriteLogEntriesRequestentries;
  
  { --------------------------------------------------------------------
    TLogEntry
    --------------------------------------------------------------------}
  
  TLogEntry = Class(TGoogleBaseObject)
  Private
    Fmetadata : TLogEntryMetadata;
    FprotoPayload : TLogEntryprotoPayload;
    FtextPayload : string;
    FstructPayload : TLogEntrystructPayload;
    FinsertId : string;
    Flog : string;
  Protected
    //Property setters
    Procedure Setmetadata(AIndex : Integer; AValue : TLogEntryMetadata); virtual;
    Procedure SetprotoPayload(AIndex : Integer; AValue : TLogEntryprotoPayload); virtual;
    Procedure SettextPayload(AIndex : Integer; AValue : string); virtual;
    Procedure SetstructPayload(AIndex : Integer; AValue : TLogEntrystructPayload); virtual;
    Procedure SetinsertId(AIndex : Integer; AValue : string); virtual;
    Procedure Setlog(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property metadata : TLogEntryMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property protoPayload : TLogEntryprotoPayload Index 8 Read FprotoPayload Write SetprotoPayload;
    Property textPayload : string Index 16 Read FtextPayload Write SettextPayload;
    Property structPayload : TLogEntrystructPayload Index 24 Read FstructPayload Write SetstructPayload;
    Property insertId : string Index 32 Read FinsertId Write SetinsertId;
    Property log : string Index 40 Read Flog Write Setlog;
  end;
  TLogEntryClass = Class of TLogEntry;
  
  { --------------------------------------------------------------------
    TLogEntryprotoPayload
    --------------------------------------------------------------------}
  
  TLogEntryprotoPayload = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TLogEntryprotoPayloadClass = Class of TLogEntryprotoPayload;
  
  { --------------------------------------------------------------------
    TLogEntrystructPayload
    --------------------------------------------------------------------}
  
  TLogEntrystructPayload = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TLogEntrystructPayloadClass = Class of TLogEntrystructPayload;
  
  { --------------------------------------------------------------------
    TLogEntryMetadata
    --------------------------------------------------------------------}
  
  TLogEntryMetadata = Class(TGoogleBaseObject)
  Private
    Ftimestamp : string;
    Fseverity : string;
    FprojectId : string;
    FserviceName : string;
    Fregion : string;
    Fzone : string;
    FuserId : string;
    Flabels : TLogEntryMetadatalabels;
  Protected
    //Property setters
    Procedure Settimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setseverity(AIndex : Integer; AValue : string); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : string); virtual;
    Procedure SetserviceName(AIndex : Integer; AValue : string); virtual;
    Procedure Setregion(AIndex : Integer; AValue : string); virtual;
    Procedure Setzone(AIndex : Integer; AValue : string); virtual;
    Procedure SetuserId(AIndex : Integer; AValue : string); virtual;
    Procedure Setlabels(AIndex : Integer; AValue : TLogEntryMetadatalabels); virtual;
  Public
  Published
    Property timestamp : string Index 0 Read Ftimestamp Write Settimestamp;
    Property severity : string Index 8 Read Fseverity Write Setseverity;
    Property projectId : string Index 16 Read FprojectId Write SetprojectId;
    Property serviceName : string Index 24 Read FserviceName Write SetserviceName;
    Property region : string Index 32 Read Fregion Write Setregion;
    Property zone : string Index 40 Read Fzone Write Setzone;
    Property userId : string Index 48 Read FuserId Write SetuserId;
    Property labels : TLogEntryMetadatalabels Index 56 Read Flabels Write Setlabels;
  end;
  TLogEntryMetadataClass = Class of TLogEntryMetadata;
  
  { --------------------------------------------------------------------
    TLogEntryMetadatalabels
    --------------------------------------------------------------------}
  
  TLogEntryMetadatalabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TLogEntryMetadatalabelsClass = Class of TLogEntryMetadatalabels;
  
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
    FlogServices : TListLogServicesResponselogServices;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure SetlogServices(AIndex : Integer; AValue : TListLogServicesResponselogServices); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property logServices : TListLogServicesResponselogServices Index 0 Read FlogServices Write SetlogServices;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListLogServicesResponseClass = Class of TListLogServicesResponse;
  
  { --------------------------------------------------------------------
    TListLogServicesResponselogServices
    --------------------------------------------------------------------}
  
  TListLogServicesResponselogServices = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListLogServicesResponselogServicesClass = Class of TListLogServicesResponselogServices;
  
  { --------------------------------------------------------------------
    TLogService
    --------------------------------------------------------------------}
  
  TLogService = Class(TGoogleBaseObject)
  Private
    Fname : string;
    FindexKeys : TLogServiceindexKeys;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetindexKeys(AIndex : Integer; AValue : TLogServiceindexKeys); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
    Property indexKeys : TLogServiceindexKeys Index 8 Read FindexKeys Write SetindexKeys;
  end;
  TLogServiceClass = Class of TLogService;
  
  { --------------------------------------------------------------------
    TLogServiceindexKeys
    --------------------------------------------------------------------}
  
  TLogServiceindexKeys = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLogServiceindexKeysClass = Class of TLogServiceindexKeys;
  
  { --------------------------------------------------------------------
    TListLogServiceIndexesResponse
    --------------------------------------------------------------------}
  
  TListLogServiceIndexesResponse = Class(TGoogleBaseObject)
  Private
    FserviceIndexPrefixes : TListLogServiceIndexesResponseserviceIndexPrefixes;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure SetserviceIndexPrefixes(AIndex : Integer; AValue : TListLogServiceIndexesResponseserviceIndexPrefixes); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property serviceIndexPrefixes : TListLogServiceIndexesResponseserviceIndexPrefixes Index 0 Read FserviceIndexPrefixes Write SetserviceIndexPrefixes;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListLogServiceIndexesResponseClass = Class of TListLogServiceIndexesResponse;
  
  { --------------------------------------------------------------------
    TListLogServiceIndexesResponseserviceIndexPrefixes
    --------------------------------------------------------------------}
  
  TListLogServiceIndexesResponseserviceIndexPrefixes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListLogServiceIndexesResponseserviceIndexPrefixesClass = Class of TListLogServiceIndexesResponseserviceIndexPrefixes;
  
  { --------------------------------------------------------------------
    TListLogSinksResponse
    --------------------------------------------------------------------}
  
  TListLogSinksResponse = Class(TGoogleBaseObject)
  Private
    Fsinks : TListLogSinksResponsesinks;
  Protected
    //Property setters
    Procedure Setsinks(AIndex : Integer; AValue : TListLogSinksResponsesinks); virtual;
  Public
  Published
    Property sinks : TListLogSinksResponsesinks Index 0 Read Fsinks Write Setsinks;
  end;
  TListLogSinksResponseClass = Class of TListLogSinksResponse;
  
  { --------------------------------------------------------------------
    TListLogSinksResponsesinks
    --------------------------------------------------------------------}
  
  TListLogSinksResponsesinks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListLogSinksResponsesinksClass = Class of TListLogSinksResponsesinks;
  
  { --------------------------------------------------------------------
    TLogSink
    --------------------------------------------------------------------}
  
  TLogSink = Class(TGoogleBaseObject)
  Private
    Fname : string;
    Fdestination : string;
    Ferrors : TLogSinkerrors;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setdestination(AIndex : Integer; AValue : string); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TLogSinkerrors); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
    Property destination : string Index 8 Read Fdestination Write Setdestination;
    Property errors : TLogSinkerrors Index 16 Read Ferrors Write Seterrors;
  end;
  TLogSinkClass = Class of TLogSink;
  
  { --------------------------------------------------------------------
    TLogSinkerrors
    --------------------------------------------------------------------}
  
  TLogSinkerrors = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLogSinkerrorsClass = Class of TLogSinkerrors;
  
  { --------------------------------------------------------------------
    TLogError
    --------------------------------------------------------------------}
  
  TLogError = Class(TGoogleBaseObject)
  Private
    Fresource : string;
    Fstatus : TStatus;
    FtimeNanos : string;
  Protected
    //Property setters
    Procedure Setresource(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : TStatus); virtual;
    Procedure SettimeNanos(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property resource : string Index 0 Read Fresource Write Setresource;
    Property status : TStatus Index 8 Read Fstatus Write Setstatus;
    Property timeNanos : string Index 16 Read FtimeNanos Write SettimeNanos;
  end;
  TLogErrorClass = Class of TLogError;
  
  { --------------------------------------------------------------------
    TStatus
    --------------------------------------------------------------------}
  
  TStatus = Class(TGoogleBaseObject)
  Private
    Fcode : integer;
    Fmessage : string;
    Fdetails : TStatusdetails;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : integer); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
    Procedure Setdetails(AIndex : Integer; AValue : TStatusdetails); virtual;
  Public
  Published
    Property code : integer Index 0 Read Fcode Write Setcode;
    Property message : string Index 8 Read Fmessage Write Setmessage;
    Property details : TStatusdetails Index 16 Read Fdetails Write Setdetails;
  end;
  TStatusClass = Class of TStatus;
  
  { --------------------------------------------------------------------
    TStatusdetails
    --------------------------------------------------------------------}
  
  TStatusdetails = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TStatusdetailsClass = Class of TStatusdetails;
  
  { --------------------------------------------------------------------
    TListLogServiceSinksResponse
    --------------------------------------------------------------------}
  
  TListLogServiceSinksResponse = Class(TGoogleBaseObject)
  Private
    Fsinks : TListLogServiceSinksResponsesinks;
  Protected
    //Property setters
    Procedure Setsinks(AIndex : Integer; AValue : TListLogServiceSinksResponsesinks); virtual;
  Public
  Published
    Property sinks : TListLogServiceSinksResponsesinks Index 0 Read Fsinks Write Setsinks;
  end;
  TListLogServiceSinksResponseClass = Class of TListLogServiceSinksResponse;
  
  { --------------------------------------------------------------------
    TListLogServiceSinksResponsesinks
    --------------------------------------------------------------------}
  
  TListLogServiceSinksResponsesinks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListLogServiceSinksResponsesinksClass = Class of TListLogServiceSinksResponsesinks;
  
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


Procedure TListLogsResponse.Setlogs(AIndex : Integer; AValue : TListLogsResponselogs); 

begin
  If (Flogs=AValue) then exit;
  Flogs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListLogsResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListLogsResponselogs
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLog
  --------------------------------------------------------------------}


Procedure TLog.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLog.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLog.SetpayloadType(AIndex : Integer; AValue : string); 

begin
  If (FpayloadType=AValue) then exit;
  FpayloadType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEmpty
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TWriteLogEntriesRequest
  --------------------------------------------------------------------}


Procedure TWriteLogEntriesRequest.SetcommonLabels(AIndex : Integer; AValue : TWriteLogEntriesRequestcommonLabels); 

begin
  If (FcommonLabels=AValue) then exit;
  FcommonLabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWriteLogEntriesRequest.Setentries(AIndex : Integer; AValue : TWriteLogEntriesRequestentries); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWriteLogEntriesRequestcommonLabels
  --------------------------------------------------------------------}


Class Function TWriteLogEntriesRequestcommonLabels.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TWriteLogEntriesRequestentries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLogEntry
  --------------------------------------------------------------------}


Procedure TLogEntry.Setmetadata(AIndex : Integer; AValue : TLogEntryMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntry.SetprotoPayload(AIndex : Integer; AValue : TLogEntryprotoPayload); 

begin
  If (FprotoPayload=AValue) then exit;
  FprotoPayload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntry.SettextPayload(AIndex : Integer; AValue : string); 

begin
  If (FtextPayload=AValue) then exit;
  FtextPayload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntry.SetstructPayload(AIndex : Integer; AValue : TLogEntrystructPayload); 

begin
  If (FstructPayload=AValue) then exit;
  FstructPayload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntry.SetinsertId(AIndex : Integer; AValue : string); 

begin
  If (FinsertId=AValue) then exit;
  FinsertId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntry.Setlog(AIndex : Integer; AValue : string); 

begin
  If (Flog=AValue) then exit;
  Flog:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLogEntryprotoPayload
  --------------------------------------------------------------------}


Class Function TLogEntryprotoPayload.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TLogEntrystructPayload
  --------------------------------------------------------------------}


Class Function TLogEntrystructPayload.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TLogEntryMetadata
  --------------------------------------------------------------------}


Procedure TLogEntryMetadata.Settimestamp(AIndex : Integer; AValue : string); 

begin
  If (Ftimestamp=AValue) then exit;
  Ftimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntryMetadata.Setseverity(AIndex : Integer; AValue : string); 

begin
  If (Fseverity=AValue) then exit;
  Fseverity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntryMetadata.SetprojectId(AIndex : Integer; AValue : string); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntryMetadata.SetserviceName(AIndex : Integer; AValue : string); 

begin
  If (FserviceName=AValue) then exit;
  FserviceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntryMetadata.Setregion(AIndex : Integer; AValue : string); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntryMetadata.Setzone(AIndex : Integer; AValue : string); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntryMetadata.SetuserId(AIndex : Integer; AValue : string); 

begin
  If (FuserId=AValue) then exit;
  FuserId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntryMetadata.Setlabels(AIndex : Integer; AValue : TLogEntryMetadatalabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLogEntryMetadatalabels
  --------------------------------------------------------------------}


Class Function TLogEntryMetadatalabels.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TWriteLogEntriesResponse
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListLogServicesResponse
  --------------------------------------------------------------------}


Procedure TListLogServicesResponse.SetlogServices(AIndex : Integer; AValue : TListLogServicesResponselogServices); 

begin
  If (FlogServices=AValue) then exit;
  FlogServices:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListLogServicesResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListLogServicesResponselogServices
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLogService
  --------------------------------------------------------------------}


Procedure TLogService.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogService.SetindexKeys(AIndex : Integer; AValue : TLogServiceindexKeys); 

begin
  If (FindexKeys=AValue) then exit;
  FindexKeys:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLogServiceindexKeys
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListLogServiceIndexesResponse
  --------------------------------------------------------------------}


Procedure TListLogServiceIndexesResponse.SetserviceIndexPrefixes(AIndex : Integer; AValue : TListLogServiceIndexesResponseserviceIndexPrefixes); 

begin
  If (FserviceIndexPrefixes=AValue) then exit;
  FserviceIndexPrefixes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListLogServiceIndexesResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListLogServiceIndexesResponseserviceIndexPrefixes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListLogSinksResponse
  --------------------------------------------------------------------}


Procedure TListLogSinksResponse.Setsinks(AIndex : Integer; AValue : TListLogSinksResponsesinks); 

begin
  If (Fsinks=AValue) then exit;
  Fsinks:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListLogSinksResponsesinks
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLogSink
  --------------------------------------------------------------------}


Procedure TLogSink.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogSink.Setdestination(AIndex : Integer; AValue : string); 

begin
  If (Fdestination=AValue) then exit;
  Fdestination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogSink.Seterrors(AIndex : Integer; AValue : TLogSinkerrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLogSinkerrors
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLogError
  --------------------------------------------------------------------}


Procedure TLogError.Setresource(AIndex : Integer; AValue : string); 

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



Procedure TLogError.SettimeNanos(AIndex : Integer; AValue : string); 

begin
  If (FtimeNanos=AValue) then exit;
  FtimeNanos:=AValue;
  MarkPropertyChanged(AIndex);
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



Procedure TStatus.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStatus.Setdetails(AIndex : Integer; AValue : TStatusdetails); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStatusdetails
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListLogServiceSinksResponse
  --------------------------------------------------------------------}


Procedure TListLogServiceSinksResponse.Setsinks(AIndex : Integer; AValue : TListLogServiceSinksResponsesinks); 

begin
  If (Fsinks=AValue) then exit;
  Fsinks:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListLogServiceSinksResponsesinks
  --------------------------------------------------------------------}




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
  TListLogsResponselogs.RegisterObject;
  TLog.RegisterObject;
  TEmpty.RegisterObject;
  TWriteLogEntriesRequest.RegisterObject;
  TWriteLogEntriesRequestcommonLabels.RegisterObject;
  TWriteLogEntriesRequestentries.RegisterObject;
  TLogEntry.RegisterObject;
  TLogEntryprotoPayload.RegisterObject;
  TLogEntrystructPayload.RegisterObject;
  TLogEntryMetadata.RegisterObject;
  TLogEntryMetadatalabels.RegisterObject;
  TWriteLogEntriesResponse.RegisterObject;
  TListLogServicesResponse.RegisterObject;
  TListLogServicesResponselogServices.RegisterObject;
  TLogService.RegisterObject;
  TLogServiceindexKeys.RegisterObject;
  TListLogServiceIndexesResponse.RegisterObject;
  TListLogServiceIndexesResponseserviceIndexPrefixes.RegisterObject;
  TListLogSinksResponse.RegisterObject;
  TListLogSinksResponsesinks.RegisterObject;
  TLogSink.RegisterObject;
  TLogSinkerrors.RegisterObject;
  TLogError.RegisterObject;
  TStatus.RegisterObject;
  TStatusdetails.RegisterObject;
  TListLogServiceSinksResponse.RegisterObject;
  TListLogServiceSinksResponsesinks.RegisterObject;
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
