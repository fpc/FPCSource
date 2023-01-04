unit googlelogging;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TEmpty = Class;
  TWriteLogEntriesRequest = Class;
  TMonitoredResource = Class;
  TLogEntry = Class;
  THttpRequest = Class;
  TLogEntryOperation = Class;
  TWriteLogEntriesResponse = Class;
  TListLogEntriesRequest = Class;
  TListLogEntriesResponse = Class;
  TStatus = Class;
  TListMonitoredResourceDescriptorsResponse = Class;
  TMonitoredResourceDescriptor = Class;
  TLabelDescriptor = Class;
  TListSinksResponse = Class;
  TLogSink = Class;
  TListLogMetricsResponse = Class;
  TLogMetric = Class;
  TRequestLog = Class;
  TLogLine = Class;
  TSourceLocation = Class;
  TSourceReference = Class;
  TEmptyArray = Array of TEmpty;
  TWriteLogEntriesRequestArray = Array of TWriteLogEntriesRequest;
  TMonitoredResourceArray = Array of TMonitoredResource;
  TLogEntryArray = Array of TLogEntry;
  THttpRequestArray = Array of THttpRequest;
  TLogEntryOperationArray = Array of TLogEntryOperation;
  TWriteLogEntriesResponseArray = Array of TWriteLogEntriesResponse;
  TListLogEntriesRequestArray = Array of TListLogEntriesRequest;
  TListLogEntriesResponseArray = Array of TListLogEntriesResponse;
  TStatusArray = Array of TStatus;
  TListMonitoredResourceDescriptorsResponseArray = Array of TListMonitoredResourceDescriptorsResponse;
  TMonitoredResourceDescriptorArray = Array of TMonitoredResourceDescriptor;
  TLabelDescriptorArray = Array of TLabelDescriptor;
  TListSinksResponseArray = Array of TListSinksResponse;
  TLogSinkArray = Array of TLogSink;
  TListLogMetricsResponseArray = Array of TListLogMetricsResponse;
  TLogMetricArray = Array of TLogMetric;
  TRequestLogArray = Array of TRequestLog;
  TLogLineArray = Array of TLogLine;
  TSourceLocationArray = Array of TSourceLocation;
  TSourceReferenceArray = Array of TSourceReference;
  //Anonymous types, using auto-generated names
  TWriteLogEntriesRequestTypelabels = Class;
  TMonitoredResourceTypelabels = Class;
  TLogEntryTypeprotoPayload = Class;
  TLogEntryTypejsonPayload = Class;
  TLogEntryTypelabels = Class;
  TListLogEntriesResponseTypeprojectIdErrors = Class;
  TStatusTypedetailsItem = Class;
  TWriteLogEntriesRequestTypeentriesArray = Array of TLogEntry;
  TListLogEntriesResponseTypeentriesArray = Array of TLogEntry;
  TStatusTypedetailsArray = Array of TStatusTypedetailsItem;
  TListMonitoredResourceDescriptorsResponseTyperesourceDescriptorsArray = Array of TMonitoredResourceDescriptor;
  TMonitoredResourceDescriptorTypelabelsArray = Array of TLabelDescriptor;
  TListSinksResponseTypesinksArray = Array of TLogSink;
  TListLogMetricsResponseTypemetricsArray = Array of TLogMetric;
  TRequestLogTypelineArray = Array of TLogLine;
  TRequestLogTypesourceReferenceArray = Array of TSourceReference;
  
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
    TWriteLogEntriesRequestTypelabels
    --------------------------------------------------------------------}
  
  TWriteLogEntriesRequestTypelabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TWriteLogEntriesRequestTypelabelsClass = Class of TWriteLogEntriesRequestTypelabels;
  
  { --------------------------------------------------------------------
    TWriteLogEntriesRequest
    --------------------------------------------------------------------}
  
  TWriteLogEntriesRequest = Class(TGoogleBaseObject)
  Private
    FlogName : String;
    Fresource : TMonitoredResource;
    Flabels : TWriteLogEntriesRequestTypelabels;
    Fentries : TWriteLogEntriesRequestTypeentriesArray;
    FpartialSuccess : boolean;
  Protected
    //Property setters
    Procedure SetlogName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setresource(AIndex : Integer; const AValue : TMonitoredResource); virtual;
    Procedure Setlabels(AIndex : Integer; const AValue : TWriteLogEntriesRequestTypelabels); virtual;
    Procedure Setentries(AIndex : Integer; const AValue : TWriteLogEntriesRequestTypeentriesArray); virtual;
    Procedure SetpartialSuccess(AIndex : Integer; const AValue : boolean); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property logName : String Index 0 Read FlogName Write SetlogName;
    Property resource : TMonitoredResource Index 8 Read Fresource Write Setresource;
    Property labels : TWriteLogEntriesRequestTypelabels Index 16 Read Flabels Write Setlabels;
    Property entries : TWriteLogEntriesRequestTypeentriesArray Index 24 Read Fentries Write Setentries;
    Property partialSuccess : boolean Index 32 Read FpartialSuccess Write SetpartialSuccess;
  end;
  TWriteLogEntriesRequestClass = Class of TWriteLogEntriesRequest;
  
  { --------------------------------------------------------------------
    TMonitoredResourceTypelabels
    --------------------------------------------------------------------}
  
  TMonitoredResourceTypelabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TMonitoredResourceTypelabelsClass = Class of TMonitoredResourceTypelabels;
  
  { --------------------------------------------------------------------
    TMonitoredResource
    --------------------------------------------------------------------}
  
  TMonitoredResource = Class(TGoogleBaseObject)
  Private
    F_type : String;
    Flabels : TMonitoredResourceTypelabels;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlabels(AIndex : Integer; const AValue : TMonitoredResourceTypelabels); virtual;
  Public
  Published
    Property _type : String Index 0 Read F_type Write Set_type;
    Property labels : TMonitoredResourceTypelabels Index 8 Read Flabels Write Setlabels;
  end;
  TMonitoredResourceClass = Class of TMonitoredResource;
  
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
    TLogEntryTypejsonPayload
    --------------------------------------------------------------------}
  
  TLogEntryTypejsonPayload = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TLogEntryTypejsonPayloadClass = Class of TLogEntryTypejsonPayload;
  
  { --------------------------------------------------------------------
    TLogEntryTypelabels
    --------------------------------------------------------------------}
  
  TLogEntryTypelabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TLogEntryTypelabelsClass = Class of TLogEntryTypelabels;
  
  { --------------------------------------------------------------------
    TLogEntry
    --------------------------------------------------------------------}
  
  TLogEntry = Class(TGoogleBaseObject)
  Private
    FlogName : String;
    Fresource : TMonitoredResource;
    FprotoPayload : TLogEntryTypeprotoPayload;
    FtextPayload : String;
    FjsonPayload : TLogEntryTypejsonPayload;
    Ftimestamp : String;
    Fseverity : String;
    FinsertId : String;
    FhttpRequest : THttpRequest;
    Flabels : TLogEntryTypelabels;
    Foperation : TLogEntryOperation;
  Protected
    //Property setters
    Procedure SetlogName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setresource(AIndex : Integer; const AValue : TMonitoredResource); virtual;
    Procedure SetprotoPayload(AIndex : Integer; const AValue : TLogEntryTypeprotoPayload); virtual;
    Procedure SettextPayload(AIndex : Integer; const AValue : String); virtual;
    Procedure SetjsonPayload(AIndex : Integer; const AValue : TLogEntryTypejsonPayload); virtual;
    Procedure Settimestamp(AIndex : Integer; const AValue : String); virtual;
    Procedure Setseverity(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinsertId(AIndex : Integer; const AValue : String); virtual;
    Procedure SethttpRequest(AIndex : Integer; const AValue : THttpRequest); virtual;
    Procedure Setlabels(AIndex : Integer; const AValue : TLogEntryTypelabels); virtual;
    Procedure Setoperation(AIndex : Integer; const AValue : TLogEntryOperation); virtual;
  Public
  Published
    Property logName : String Index 0 Read FlogName Write SetlogName;
    Property resource : TMonitoredResource Index 8 Read Fresource Write Setresource;
    Property protoPayload : TLogEntryTypeprotoPayload Index 16 Read FprotoPayload Write SetprotoPayload;
    Property textPayload : String Index 24 Read FtextPayload Write SettextPayload;
    Property jsonPayload : TLogEntryTypejsonPayload Index 32 Read FjsonPayload Write SetjsonPayload;
    Property timestamp : String Index 40 Read Ftimestamp Write Settimestamp;
    Property severity : String Index 48 Read Fseverity Write Setseverity;
    Property insertId : String Index 56 Read FinsertId Write SetinsertId;
    Property httpRequest : THttpRequest Index 64 Read FhttpRequest Write SethttpRequest;
    Property labels : TLogEntryTypelabels Index 72 Read Flabels Write Setlabels;
    Property operation : TLogEntryOperation Index 80 Read Foperation Write Setoperation;
  end;
  TLogEntryClass = Class of TLogEntry;
  
  { --------------------------------------------------------------------
    THttpRequest
    --------------------------------------------------------------------}
  
  THttpRequest = Class(TGoogleBaseObject)
  Private
    FrequestMethod : String;
    FrequestUrl : String;
    FrequestSize : String;
    Fstatus : integer;
    FresponseSize : String;
    FuserAgent : String;
    FremoteIp : String;
    Freferer : String;
    FcacheLookup : boolean;
    FcacheHit : boolean;
    FcacheValidatedWithOriginServer : boolean;
    FcacheFillBytes : String;
  Protected
    //Property setters
    Procedure SetrequestMethod(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrequestUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrequestSize(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetresponseSize(AIndex : Integer; const AValue : String); virtual;
    Procedure SetuserAgent(AIndex : Integer; const AValue : String); virtual;
    Procedure SetremoteIp(AIndex : Integer; const AValue : String); virtual;
    Procedure Setreferer(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcacheLookup(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetcacheHit(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetcacheValidatedWithOriginServer(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetcacheFillBytes(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property requestMethod : String Index 0 Read FrequestMethod Write SetrequestMethod;
    Property requestUrl : String Index 8 Read FrequestUrl Write SetrequestUrl;
    Property requestSize : String Index 16 Read FrequestSize Write SetrequestSize;
    Property status : integer Index 24 Read Fstatus Write Setstatus;
    Property responseSize : String Index 32 Read FresponseSize Write SetresponseSize;
    Property userAgent : String Index 40 Read FuserAgent Write SetuserAgent;
    Property remoteIp : String Index 48 Read FremoteIp Write SetremoteIp;
    Property referer : String Index 56 Read Freferer Write Setreferer;
    Property cacheLookup : boolean Index 64 Read FcacheLookup Write SetcacheLookup;
    Property cacheHit : boolean Index 72 Read FcacheHit Write SetcacheHit;
    Property cacheValidatedWithOriginServer : boolean Index 80 Read FcacheValidatedWithOriginServer Write SetcacheValidatedWithOriginServer;
    Property cacheFillBytes : String Index 88 Read FcacheFillBytes Write SetcacheFillBytes;
  end;
  THttpRequestClass = Class of THttpRequest;
  
  { --------------------------------------------------------------------
    TLogEntryOperation
    --------------------------------------------------------------------}
  
  TLogEntryOperation = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fproducer : String;
    Ffirst : boolean;
    Flast : boolean;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setproducer(AIndex : Integer; const AValue : String); virtual;
    Procedure Setfirst(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setlast(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property producer : String Index 8 Read Fproducer Write Setproducer;
    Property first : boolean Index 16 Read Ffirst Write Setfirst;
    Property last : boolean Index 24 Read Flast Write Setlast;
  end;
  TLogEntryOperationClass = Class of TLogEntryOperation;
  
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
    TListLogEntriesRequest
    --------------------------------------------------------------------}
  
  TListLogEntriesRequest = Class(TGoogleBaseObject)
  Private
    FprojectIds : TStringArray;
    Ffilter : String;
    ForderBy : String;
    FpageSize : integer;
    FpageToken : String;
    FpartialSuccess : boolean;
  Protected
    //Property setters
    Procedure SetprojectIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setfilter(AIndex : Integer; const AValue : String); virtual;
    Procedure SetorderBy(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageSize(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetpageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpartialSuccess(AIndex : Integer; const AValue : boolean); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property projectIds : TStringArray Index 0 Read FprojectIds Write SetprojectIds;
    Property filter : String Index 8 Read Ffilter Write Setfilter;
    Property orderBy : String Index 16 Read ForderBy Write SetorderBy;
    Property pageSize : integer Index 24 Read FpageSize Write SetpageSize;
    Property pageToken : String Index 32 Read FpageToken Write SetpageToken;
    Property partialSuccess : boolean Index 40 Read FpartialSuccess Write SetpartialSuccess;
  end;
  TListLogEntriesRequestClass = Class of TListLogEntriesRequest;
  
  { --------------------------------------------------------------------
    TListLogEntriesResponseTypeprojectIdErrors
    --------------------------------------------------------------------}
  
  TListLogEntriesResponseTypeprojectIdErrors = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TListLogEntriesResponseTypeprojectIdErrorsClass = Class of TListLogEntriesResponseTypeprojectIdErrors;
  
  { --------------------------------------------------------------------
    TListLogEntriesResponse
    --------------------------------------------------------------------}
  
  TListLogEntriesResponse = Class(TGoogleBaseObject)
  Private
    Fentries : TListLogEntriesResponseTypeentriesArray;
    FnextPageToken : String;
    FprojectIdErrors : TListLogEntriesResponseTypeprojectIdErrors;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; const AValue : TListLogEntriesResponseTypeentriesArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprojectIdErrors(AIndex : Integer; const AValue : TListLogEntriesResponseTypeprojectIdErrors); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entries : TListLogEntriesResponseTypeentriesArray Index 0 Read Fentries Write Setentries;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property projectIdErrors : TListLogEntriesResponseTypeprojectIdErrors Index 16 Read FprojectIdErrors Write SetprojectIdErrors;
  end;
  TListLogEntriesResponseClass = Class of TListLogEntriesResponse;
  
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
    Procedure Setcode(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setmessage(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdetails(AIndex : Integer; const AValue : TStatusTypedetailsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property code : integer Index 0 Read Fcode Write Setcode;
    Property message : String Index 8 Read Fmessage Write Setmessage;
    Property details : TStatusTypedetailsArray Index 16 Read Fdetails Write Setdetails;
  end;
  TStatusClass = Class of TStatus;
  
  { --------------------------------------------------------------------
    TListMonitoredResourceDescriptorsResponse
    --------------------------------------------------------------------}
  
  TListMonitoredResourceDescriptorsResponse = Class(TGoogleBaseObject)
  Private
    FresourceDescriptors : TListMonitoredResourceDescriptorsResponseTyperesourceDescriptorsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetresourceDescriptors(AIndex : Integer; const AValue : TListMonitoredResourceDescriptorsResponseTyperesourceDescriptorsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property resourceDescriptors : TListMonitoredResourceDescriptorsResponseTyperesourceDescriptorsArray Index 0 Read FresourceDescriptors Write SetresourceDescriptors;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListMonitoredResourceDescriptorsResponseClass = Class of TListMonitoredResourceDescriptorsResponse;
  
  { --------------------------------------------------------------------
    TMonitoredResourceDescriptor
    --------------------------------------------------------------------}
  
  TMonitoredResourceDescriptor = Class(TGoogleBaseObject)
  Private
    Fname : String;
    F_type : String;
    FdisplayName : String;
    Fdescription : String;
    Flabels : TMonitoredResourceDescriptorTypelabelsArray;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlabels(AIndex : Integer; const AValue : TMonitoredResourceDescriptorTypelabelsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property _type : String Index 8 Read F_type Write Set_type;
    Property displayName : String Index 16 Read FdisplayName Write SetdisplayName;
    Property description : String Index 24 Read Fdescription Write Setdescription;
    Property labels : TMonitoredResourceDescriptorTypelabelsArray Index 32 Read Flabels Write Setlabels;
  end;
  TMonitoredResourceDescriptorClass = Class of TMonitoredResourceDescriptor;
  
  { --------------------------------------------------------------------
    TLabelDescriptor
    --------------------------------------------------------------------}
  
  TLabelDescriptor = Class(TGoogleBaseObject)
  Private
    Fkey : String;
    FvalueType : String;
    Fdescription : String;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvalueType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property key : String Index 0 Read Fkey Write Setkey;
    Property valueType : String Index 8 Read FvalueType Write SetvalueType;
    Property description : String Index 16 Read Fdescription Write Setdescription;
  end;
  TLabelDescriptorClass = Class of TLabelDescriptor;
  
  { --------------------------------------------------------------------
    TListSinksResponse
    --------------------------------------------------------------------}
  
  TListSinksResponse = Class(TGoogleBaseObject)
  Private
    Fsinks : TListSinksResponseTypesinksArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setsinks(AIndex : Integer; const AValue : TListSinksResponseTypesinksArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property sinks : TListSinksResponseTypesinksArray Index 0 Read Fsinks Write Setsinks;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListSinksResponseClass = Class of TListSinksResponse;
  
  { --------------------------------------------------------------------
    TLogSink
    --------------------------------------------------------------------}
  
  TLogSink = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fdestination : String;
    Ffilter : String;
    FoutputVersionFormat : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdestination(AIndex : Integer; const AValue : String); virtual;
    Procedure Setfilter(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoutputVersionFormat(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property destination : String Index 8 Read Fdestination Write Setdestination;
    Property filter : String Index 16 Read Ffilter Write Setfilter;
    Property outputVersionFormat : String Index 24 Read FoutputVersionFormat Write SetoutputVersionFormat;
  end;
  TLogSinkClass = Class of TLogSink;
  
  { --------------------------------------------------------------------
    TListLogMetricsResponse
    --------------------------------------------------------------------}
  
  TListLogMetricsResponse = Class(TGoogleBaseObject)
  Private
    Fmetrics : TListLogMetricsResponseTypemetricsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setmetrics(AIndex : Integer; const AValue : TListLogMetricsResponseTypemetricsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property metrics : TListLogMetricsResponseTypemetricsArray Index 0 Read Fmetrics Write Setmetrics;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListLogMetricsResponseClass = Class of TListLogMetricsResponse;
  
  { --------------------------------------------------------------------
    TLogMetric
    --------------------------------------------------------------------}
  
  TLogMetric = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fdescription : String;
    Ffilter : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setfilter(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property filter : String Index 16 Read Ffilter Write Setfilter;
  end;
  TLogMetricClass = Class of TLogMetric;
  
  { --------------------------------------------------------------------
    TRequestLog
    --------------------------------------------------------------------}
  
  TRequestLog = Class(TGoogleBaseObject)
  Private
    FappId : String;
    FmoduleId : String;
    FversionId : String;
    FrequestId : String;
    Fip : String;
    FstartTime : String;
    FendTime : String;
    Flatency : String;
    FmegaCycles : String;
    Fmethod : String;
    Fresource : String;
    FhttpVersion : String;
    Fstatus : integer;
    FresponseSize : String;
    Freferrer : String;
    FuserAgent : String;
    Fnickname : String;
    FurlMapEntry : String;
    Fhost : String;
    Fcost : double;
    FtaskQueueName : String;
    FtaskName : String;
    FwasLoadingRequest : boolean;
    FpendingTime : String;
    FinstanceIndex : integer;
    Ffinished : boolean;
    Ffirst : boolean;
    FinstanceId : String;
    Fline : TRequestLogTypelineArray;
    FappEngineRelease : String;
    FtraceId : String;
    FsourceReference : TRequestLogTypesourceReferenceArray;
  Protected
    //Property setters
    Procedure SetappId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmoduleId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetversionId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrequestId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setip(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstartTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetendTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlatency(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmegaCycles(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmethod(AIndex : Integer; const AValue : String); virtual;
    Procedure Setresource(AIndex : Integer; const AValue : String); virtual;
    Procedure SethttpVersion(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetresponseSize(AIndex : Integer; const AValue : String); virtual;
    Procedure Setreferrer(AIndex : Integer; const AValue : String); virtual;
    Procedure SetuserAgent(AIndex : Integer; const AValue : String); virtual;
    Procedure Setnickname(AIndex : Integer; const AValue : String); virtual;
    Procedure SeturlMapEntry(AIndex : Integer; const AValue : String); virtual;
    Procedure Sethost(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcost(AIndex : Integer; const AValue : double); virtual;
    Procedure SettaskQueueName(AIndex : Integer; const AValue : String); virtual;
    Procedure SettaskName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetwasLoadingRequest(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetpendingTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinstanceIndex(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setfinished(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setfirst(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetinstanceId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setline(AIndex : Integer; const AValue : TRequestLogTypelineArray); virtual;
    Procedure SetappEngineRelease(AIndex : Integer; const AValue : String); virtual;
    Procedure SettraceId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsourceReference(AIndex : Integer; const AValue : TRequestLogTypesourceReferenceArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property appId : String Index 0 Read FappId Write SetappId;
    Property moduleId : String Index 8 Read FmoduleId Write SetmoduleId;
    Property versionId : String Index 16 Read FversionId Write SetversionId;
    Property requestId : String Index 24 Read FrequestId Write SetrequestId;
    Property ip : String Index 32 Read Fip Write Setip;
    Property startTime : String Index 40 Read FstartTime Write SetstartTime;
    Property endTime : String Index 48 Read FendTime Write SetendTime;
    Property latency : String Index 56 Read Flatency Write Setlatency;
    Property megaCycles : String Index 64 Read FmegaCycles Write SetmegaCycles;
    Property method : String Index 72 Read Fmethod Write Setmethod;
    Property resource : String Index 80 Read Fresource Write Setresource;
    Property httpVersion : String Index 88 Read FhttpVersion Write SethttpVersion;
    Property status : integer Index 96 Read Fstatus Write Setstatus;
    Property responseSize : String Index 104 Read FresponseSize Write SetresponseSize;
    Property referrer : String Index 112 Read Freferrer Write Setreferrer;
    Property userAgent : String Index 120 Read FuserAgent Write SetuserAgent;
    Property nickname : String Index 128 Read Fnickname Write Setnickname;
    Property urlMapEntry : String Index 136 Read FurlMapEntry Write SeturlMapEntry;
    Property host : String Index 144 Read Fhost Write Sethost;
    Property cost : double Index 152 Read Fcost Write Setcost;
    Property taskQueueName : String Index 160 Read FtaskQueueName Write SettaskQueueName;
    Property taskName : String Index 168 Read FtaskName Write SettaskName;
    Property wasLoadingRequest : boolean Index 176 Read FwasLoadingRequest Write SetwasLoadingRequest;
    Property pendingTime : String Index 184 Read FpendingTime Write SetpendingTime;
    Property instanceIndex : integer Index 192 Read FinstanceIndex Write SetinstanceIndex;
    Property finished : boolean Index 200 Read Ffinished Write Setfinished;
    Property first : boolean Index 208 Read Ffirst Write Setfirst;
    Property instanceId : String Index 216 Read FinstanceId Write SetinstanceId;
    Property line : TRequestLogTypelineArray Index 224 Read Fline Write Setline;
    Property appEngineRelease : String Index 232 Read FappEngineRelease Write SetappEngineRelease;
    Property traceId : String Index 240 Read FtraceId Write SettraceId;
    Property sourceReference : TRequestLogTypesourceReferenceArray Index 248 Read FsourceReference Write SetsourceReference;
  end;
  TRequestLogClass = Class of TRequestLog;
  
  { --------------------------------------------------------------------
    TLogLine
    --------------------------------------------------------------------}
  
  TLogLine = Class(TGoogleBaseObject)
  Private
    Ftime : String;
    Fseverity : String;
    FlogMessage : String;
    FsourceLocation : TSourceLocation;
  Protected
    //Property setters
    Procedure Settime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setseverity(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlogMessage(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsourceLocation(AIndex : Integer; const AValue : TSourceLocation); virtual;
  Public
  Published
    Property time : String Index 0 Read Ftime Write Settime;
    Property severity : String Index 8 Read Fseverity Write Setseverity;
    Property logMessage : String Index 16 Read FlogMessage Write SetlogMessage;
    Property sourceLocation : TSourceLocation Index 24 Read FsourceLocation Write SetsourceLocation;
  end;
  TLogLineClass = Class of TLogLine;
  
  { --------------------------------------------------------------------
    TSourceLocation
    --------------------------------------------------------------------}
  
  TSourceLocation = Class(TGoogleBaseObject)
  Private
    F_file : String;
    Fline : String;
    FfunctionName : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_file(AIndex : Integer; const AValue : String); virtual;
    Procedure Setline(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfunctionName(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property _file : String Index 0 Read F_file Write Set_file;
    Property line : String Index 8 Read Fline Write Setline;
    Property functionName : String Index 16 Read FfunctionName Write SetfunctionName;
  end;
  TSourceLocationClass = Class of TSourceLocation;
  
  { --------------------------------------------------------------------
    TSourceReference
    --------------------------------------------------------------------}
  
  TSourceReference = Class(TGoogleBaseObject)
  Private
    Frepository : String;
    FrevisionId : String;
  Protected
    //Property setters
    Procedure Setrepository(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrevisionId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property repository : String Index 0 Read Frepository Write Setrepository;
    Property revisionId : String Index 8 Read FrevisionId Write SetrevisionId;
  end;
  TSourceReferenceClass = Class of TSourceReference;
  
  { --------------------------------------------------------------------
    TProjectsLogsResource
    --------------------------------------------------------------------}
  
  TProjectsLogsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(logName: string) : TEmpty;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsSinksResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsSinksResource, method List
  
  TProjectsSinksListOptions = Record
    pageToken : String;
    pageSize : integer;
  end;
  
  TProjectsSinksResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(projectName: string; AQuery : string  = '') : TListSinksResponse;
    Function List(projectName: string; AQuery : TProjectsSinkslistOptions) : TListSinksResponse;
    Function Get(sinkName: string) : TLogSink;
    Function Create(projectName: string; aLogSink : TLogSink) : TLogSink;overload;
    Function Update(sinkName: string; aLogSink : TLogSink) : TLogSink;
    Function Delete(sinkName: string) : TEmpty;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsMetricsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsMetricsResource, method List
  
  TProjectsMetricsListOptions = Record
    pageToken : String;
    pageSize : integer;
  end;
  
  TProjectsMetricsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(projectName: string; AQuery : string  = '') : TListLogMetricsResponse;
    Function List(projectName: string; AQuery : TProjectsMetricslistOptions) : TListLogMetricsResponse;
    Function Get(metricName: string) : TLogMetric;
    Function Create(projectName: string; aLogMetric : TLogMetric) : TLogMetric;overload;
    Function Update(metricName: string; aLogMetric : TLogMetric) : TLogMetric;
    Function Delete(metricName: string) : TEmpty;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Private
    FLogsInstance : TProjectsLogsResource;
    FSinksInstance : TProjectsSinksResource;
    FMetricsInstance : TProjectsMetricsResource;
    Function GetLogsInstance : TProjectsLogsResource;virtual;
    Function GetSinksInstance : TProjectsSinksResource;virtual;
    Function GetMetricsInstance : TProjectsMetricsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateLogsResource(AOwner : TComponent) : TProjectsLogsResource;virtual;overload;
    Function CreateLogsResource : TProjectsLogsResource;virtual;overload;
    Function CreateSinksResource(AOwner : TComponent) : TProjectsSinksResource;virtual;overload;
    Function CreateSinksResource : TProjectsSinksResource;virtual;overload;
    Function CreateMetricsResource(AOwner : TComponent) : TProjectsMetricsResource;virtual;overload;
    Function CreateMetricsResource : TProjectsMetricsResource;virtual;overload;
    Property LogsResource : TProjectsLogsResource Read GetLogsInstance;
    Property SinksResource : TProjectsSinksResource Read GetSinksInstance;
    Property MetricsResource : TProjectsMetricsResource Read GetMetricsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TEntriesResource
    --------------------------------------------------------------------}
  
  TEntriesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Write(aWriteLogEntriesRequest : TWriteLogEntriesRequest) : TWriteLogEntriesResponse;
    Function List(aListLogEntriesRequest : TListLogEntriesRequest) : TListLogEntriesResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TMonitoredResourceDescriptorsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TMonitoredResourceDescriptorsResource, method List
  
  TMonitoredResourceDescriptorsListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  TMonitoredResourceDescriptorsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TListMonitoredResourceDescriptorsResponse;
    Function List(AQuery : TMonitoredResourceDescriptorslistOptions) : TListMonitoredResourceDescriptorsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TLoggingAPI
    --------------------------------------------------------------------}
  
  TLoggingAPI = Class(TGoogleAPI)
  Private
    FProjectsLogsInstance : TProjectsLogsResource;
    FProjectsSinksInstance : TProjectsSinksResource;
    FProjectsMetricsInstance : TProjectsMetricsResource;
    FProjectsInstance : TProjectsResource;
    FEntriesInstance : TEntriesResource;
    FMonitoredResourceDescriptorsInstance : TMonitoredResourceDescriptorsResource;
    Function GetProjectsLogsInstance : TProjectsLogsResource;virtual;
    Function GetProjectsSinksInstance : TProjectsSinksResource;virtual;
    Function GetProjectsMetricsInstance : TProjectsMetricsResource;virtual;
    Function GetProjectsInstance : TProjectsResource;virtual;
    Function GetEntriesInstance : TEntriesResource;virtual;
    Function GetMonitoredResourceDescriptorsInstance : TMonitoredResourceDescriptorsResource;virtual;
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
    Function CreateProjectsLogsResource(AOwner : TComponent) : TProjectsLogsResource;virtual;overload;
    Function CreateProjectsLogsResource : TProjectsLogsResource;virtual;overload;
    Function CreateProjectsSinksResource(AOwner : TComponent) : TProjectsSinksResource;virtual;overload;
    Function CreateProjectsSinksResource : TProjectsSinksResource;virtual;overload;
    Function CreateProjectsMetricsResource(AOwner : TComponent) : TProjectsMetricsResource;virtual;overload;
    Function CreateProjectsMetricsResource : TProjectsMetricsResource;virtual;overload;
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    Function CreateEntriesResource(AOwner : TComponent) : TEntriesResource;virtual;overload;
    Function CreateEntriesResource : TEntriesResource;virtual;overload;
    Function CreateMonitoredResourceDescriptorsResource(AOwner : TComponent) : TMonitoredResourceDescriptorsResource;virtual;overload;
    Function CreateMonitoredResourceDescriptorsResource : TMonitoredResourceDescriptorsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ProjectsLogsResource : TProjectsLogsResource Read GetProjectsLogsInstance;
    Property ProjectsSinksResource : TProjectsSinksResource Read GetProjectsSinksInstance;
    Property ProjectsMetricsResource : TProjectsMetricsResource Read GetProjectsMetricsInstance;
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
    Property EntriesResource : TEntriesResource Read GetEntriesInstance;
    Property MonitoredResourceDescriptorsResource : TMonitoredResourceDescriptorsResource Read GetMonitoredResourceDescriptorsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TEmpty
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TWriteLogEntriesRequestTypelabels
  --------------------------------------------------------------------}


Class Function TWriteLogEntriesRequestTypelabels.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TWriteLogEntriesRequest
  --------------------------------------------------------------------}


Procedure TWriteLogEntriesRequest.SetlogName(AIndex : Integer; const AValue : String); 

begin
  If (FlogName=AValue) then exit;
  FlogName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWriteLogEntriesRequest.Setresource(AIndex : Integer; const AValue : TMonitoredResource); 

begin
  If (Fresource=AValue) then exit;
  Fresource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWriteLogEntriesRequest.Setlabels(AIndex : Integer; const AValue : TWriteLogEntriesRequestTypelabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWriteLogEntriesRequest.Setentries(AIndex : Integer; const AValue : TWriteLogEntriesRequestTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWriteLogEntriesRequest.SetpartialSuccess(AIndex : Integer; const AValue : boolean); 

begin
  If (FpartialSuccess=AValue) then exit;
  FpartialSuccess:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TWriteLogEntriesRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMonitoredResourceTypelabels
  --------------------------------------------------------------------}


Class Function TMonitoredResourceTypelabels.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TMonitoredResource
  --------------------------------------------------------------------}


Procedure TMonitoredResource.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMonitoredResource.Setlabels(AIndex : Integer; const AValue : TMonitoredResourceTypelabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TMonitoredResource.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TLogEntryTypeprotoPayload
  --------------------------------------------------------------------}


Class Function TLogEntryTypeprotoPayload.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TLogEntryTypejsonPayload
  --------------------------------------------------------------------}


Class Function TLogEntryTypejsonPayload.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TLogEntryTypelabels
  --------------------------------------------------------------------}


Class Function TLogEntryTypelabels.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TLogEntry
  --------------------------------------------------------------------}


Procedure TLogEntry.SetlogName(AIndex : Integer; const AValue : String); 

begin
  If (FlogName=AValue) then exit;
  FlogName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntry.Setresource(AIndex : Integer; const AValue : TMonitoredResource); 

begin
  If (Fresource=AValue) then exit;
  Fresource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntry.SetprotoPayload(AIndex : Integer; const AValue : TLogEntryTypeprotoPayload); 

begin
  If (FprotoPayload=AValue) then exit;
  FprotoPayload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntry.SettextPayload(AIndex : Integer; const AValue : String); 

begin
  If (FtextPayload=AValue) then exit;
  FtextPayload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntry.SetjsonPayload(AIndex : Integer; const AValue : TLogEntryTypejsonPayload); 

begin
  If (FjsonPayload=AValue) then exit;
  FjsonPayload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntry.Settimestamp(AIndex : Integer; const AValue : String); 

begin
  If (Ftimestamp=AValue) then exit;
  Ftimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntry.Setseverity(AIndex : Integer; const AValue : String); 

begin
  If (Fseverity=AValue) then exit;
  Fseverity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntry.SetinsertId(AIndex : Integer; const AValue : String); 

begin
  If (FinsertId=AValue) then exit;
  FinsertId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntry.SethttpRequest(AIndex : Integer; const AValue : THttpRequest); 

begin
  If (FhttpRequest=AValue) then exit;
  FhttpRequest:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntry.Setlabels(AIndex : Integer; const AValue : TLogEntryTypelabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntry.Setoperation(AIndex : Integer; const AValue : TLogEntryOperation); 

begin
  If (Foperation=AValue) then exit;
  Foperation:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  THttpRequest
  --------------------------------------------------------------------}


Procedure THttpRequest.SetrequestMethod(AIndex : Integer; const AValue : String); 

begin
  If (FrequestMethod=AValue) then exit;
  FrequestMethod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpRequest.SetrequestUrl(AIndex : Integer; const AValue : String); 

begin
  If (FrequestUrl=AValue) then exit;
  FrequestUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpRequest.SetrequestSize(AIndex : Integer; const AValue : String); 

begin
  If (FrequestSize=AValue) then exit;
  FrequestSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpRequest.Setstatus(AIndex : Integer; const AValue : integer); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpRequest.SetresponseSize(AIndex : Integer; const AValue : String); 

begin
  If (FresponseSize=AValue) then exit;
  FresponseSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpRequest.SetuserAgent(AIndex : Integer; const AValue : String); 

begin
  If (FuserAgent=AValue) then exit;
  FuserAgent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpRequest.SetremoteIp(AIndex : Integer; const AValue : String); 

begin
  If (FremoteIp=AValue) then exit;
  FremoteIp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpRequest.Setreferer(AIndex : Integer; const AValue : String); 

begin
  If (Freferer=AValue) then exit;
  Freferer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpRequest.SetcacheLookup(AIndex : Integer; const AValue : boolean); 

begin
  If (FcacheLookup=AValue) then exit;
  FcacheLookup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpRequest.SetcacheHit(AIndex : Integer; const AValue : boolean); 

begin
  If (FcacheHit=AValue) then exit;
  FcacheHit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpRequest.SetcacheValidatedWithOriginServer(AIndex : Integer; const AValue : boolean); 

begin
  If (FcacheValidatedWithOriginServer=AValue) then exit;
  FcacheValidatedWithOriginServer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpRequest.SetcacheFillBytes(AIndex : Integer; const AValue : String); 

begin
  If (FcacheFillBytes=AValue) then exit;
  FcacheFillBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLogEntryOperation
  --------------------------------------------------------------------}


Procedure TLogEntryOperation.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntryOperation.Setproducer(AIndex : Integer; const AValue : String); 

begin
  If (Fproducer=AValue) then exit;
  Fproducer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntryOperation.Setfirst(AIndex : Integer; const AValue : boolean); 

begin
  If (Ffirst=AValue) then exit;
  Ffirst:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogEntryOperation.Setlast(AIndex : Integer; const AValue : boolean); 

begin
  If (Flast=AValue) then exit;
  Flast:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWriteLogEntriesResponse
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListLogEntriesRequest
  --------------------------------------------------------------------}


Procedure TListLogEntriesRequest.SetprojectIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FprojectIds=AValue) then exit;
  FprojectIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListLogEntriesRequest.Setfilter(AIndex : Integer; const AValue : String); 

begin
  If (Ffilter=AValue) then exit;
  Ffilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListLogEntriesRequest.SetorderBy(AIndex : Integer; const AValue : String); 

begin
  If (ForderBy=AValue) then exit;
  ForderBy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListLogEntriesRequest.SetpageSize(AIndex : Integer; const AValue : integer); 

begin
  If (FpageSize=AValue) then exit;
  FpageSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListLogEntriesRequest.SetpageToken(AIndex : Integer; const AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListLogEntriesRequest.SetpartialSuccess(AIndex : Integer; const AValue : boolean); 

begin
  If (FpartialSuccess=AValue) then exit;
  FpartialSuccess:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListLogEntriesRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'projectids' : SetLength(FprojectIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListLogEntriesResponseTypeprojectIdErrors
  --------------------------------------------------------------------}


Class Function TListLogEntriesResponseTypeprojectIdErrors.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TListLogEntriesResponse
  --------------------------------------------------------------------}


Procedure TListLogEntriesResponse.Setentries(AIndex : Integer; const AValue : TListLogEntriesResponseTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListLogEntriesResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListLogEntriesResponse.SetprojectIdErrors(AIndex : Integer; const AValue : TListLogEntriesResponseTypeprojectIdErrors); 

begin
  If (FprojectIdErrors=AValue) then exit;
  FprojectIdErrors:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListLogEntriesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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


Procedure TStatus.Setcode(AIndex : Integer; const AValue : integer); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStatus.Setmessage(AIndex : Integer; const AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStatus.Setdetails(AIndex : Integer; const AValue : TStatusTypedetailsArray); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TStatus.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'details' : SetLength(Fdetails,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListMonitoredResourceDescriptorsResponse
  --------------------------------------------------------------------}


Procedure TListMonitoredResourceDescriptorsResponse.SetresourceDescriptors(AIndex : Integer; const AValue : TListMonitoredResourceDescriptorsResponseTyperesourceDescriptorsArray); 

begin
  If (FresourceDescriptors=AValue) then exit;
  FresourceDescriptors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListMonitoredResourceDescriptorsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListMonitoredResourceDescriptorsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'resourcedescriptors' : SetLength(FresourceDescriptors,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMonitoredResourceDescriptor
  --------------------------------------------------------------------}


Procedure TMonitoredResourceDescriptor.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMonitoredResourceDescriptor.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMonitoredResourceDescriptor.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMonitoredResourceDescriptor.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMonitoredResourceDescriptor.Setlabels(AIndex : Integer; const AValue : TMonitoredResourceDescriptorTypelabelsArray); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TMonitoredResourceDescriptor.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TMonitoredResourceDescriptor.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'labels' : SetLength(Flabels,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLabelDescriptor
  --------------------------------------------------------------------}


Procedure TLabelDescriptor.Setkey(AIndex : Integer; const AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabelDescriptor.SetvalueType(AIndex : Integer; const AValue : String); 

begin
  If (FvalueType=AValue) then exit;
  FvalueType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabelDescriptor.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListSinksResponse
  --------------------------------------------------------------------}


Procedure TListSinksResponse.Setsinks(AIndex : Integer; const AValue : TListSinksResponseTypesinksArray); 

begin
  If (Fsinks=AValue) then exit;
  Fsinks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListSinksResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListSinksResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sinks' : SetLength(Fsinks,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLogSink
  --------------------------------------------------------------------}


Procedure TLogSink.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogSink.Setdestination(AIndex : Integer; const AValue : String); 

begin
  If (Fdestination=AValue) then exit;
  Fdestination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogSink.Setfilter(AIndex : Integer; const AValue : String); 

begin
  If (Ffilter=AValue) then exit;
  Ffilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogSink.SetoutputVersionFormat(AIndex : Integer; const AValue : String); 

begin
  If (FoutputVersionFormat=AValue) then exit;
  FoutputVersionFormat:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListLogMetricsResponse
  --------------------------------------------------------------------}


Procedure TListLogMetricsResponse.Setmetrics(AIndex : Integer; const AValue : TListLogMetricsResponseTypemetricsArray); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListLogMetricsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListLogMetricsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'metrics' : SetLength(Fmetrics,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLogMetric
  --------------------------------------------------------------------}


Procedure TLogMetric.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogMetric.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogMetric.Setfilter(AIndex : Integer; const AValue : String); 

begin
  If (Ffilter=AValue) then exit;
  Ffilter:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRequestLog
  --------------------------------------------------------------------}


Procedure TRequestLog.SetappId(AIndex : Integer; const AValue : String); 

begin
  If (FappId=AValue) then exit;
  FappId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.SetmoduleId(AIndex : Integer; const AValue : String); 

begin
  If (FmoduleId=AValue) then exit;
  FmoduleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.SetversionId(AIndex : Integer; const AValue : String); 

begin
  If (FversionId=AValue) then exit;
  FversionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.SetrequestId(AIndex : Integer; const AValue : String); 

begin
  If (FrequestId=AValue) then exit;
  FrequestId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.Setip(AIndex : Integer; const AValue : String); 

begin
  If (Fip=AValue) then exit;
  Fip:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.SetstartTime(AIndex : Integer; const AValue : String); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.SetendTime(AIndex : Integer; const AValue : String); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.Setlatency(AIndex : Integer; const AValue : String); 

begin
  If (Flatency=AValue) then exit;
  Flatency:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.SetmegaCycles(AIndex : Integer; const AValue : String); 

begin
  If (FmegaCycles=AValue) then exit;
  FmegaCycles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.Setmethod(AIndex : Integer; const AValue : String); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.Setresource(AIndex : Integer; const AValue : String); 

begin
  If (Fresource=AValue) then exit;
  Fresource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.SethttpVersion(AIndex : Integer; const AValue : String); 

begin
  If (FhttpVersion=AValue) then exit;
  FhttpVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.Setstatus(AIndex : Integer; const AValue : integer); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.SetresponseSize(AIndex : Integer; const AValue : String); 

begin
  If (FresponseSize=AValue) then exit;
  FresponseSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.Setreferrer(AIndex : Integer; const AValue : String); 

begin
  If (Freferrer=AValue) then exit;
  Freferrer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.SetuserAgent(AIndex : Integer; const AValue : String); 

begin
  If (FuserAgent=AValue) then exit;
  FuserAgent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.Setnickname(AIndex : Integer; const AValue : String); 

begin
  If (Fnickname=AValue) then exit;
  Fnickname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.SeturlMapEntry(AIndex : Integer; const AValue : String); 

begin
  If (FurlMapEntry=AValue) then exit;
  FurlMapEntry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.Sethost(AIndex : Integer; const AValue : String); 

begin
  If (Fhost=AValue) then exit;
  Fhost:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.Setcost(AIndex : Integer; const AValue : double); 

begin
  If (Fcost=AValue) then exit;
  Fcost:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.SettaskQueueName(AIndex : Integer; const AValue : String); 

begin
  If (FtaskQueueName=AValue) then exit;
  FtaskQueueName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.SettaskName(AIndex : Integer; const AValue : String); 

begin
  If (FtaskName=AValue) then exit;
  FtaskName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.SetwasLoadingRequest(AIndex : Integer; const AValue : boolean); 

begin
  If (FwasLoadingRequest=AValue) then exit;
  FwasLoadingRequest:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.SetpendingTime(AIndex : Integer; const AValue : String); 

begin
  If (FpendingTime=AValue) then exit;
  FpendingTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.SetinstanceIndex(AIndex : Integer; const AValue : integer); 

begin
  If (FinstanceIndex=AValue) then exit;
  FinstanceIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.Setfinished(AIndex : Integer; const AValue : boolean); 

begin
  If (Ffinished=AValue) then exit;
  Ffinished:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.Setfirst(AIndex : Integer; const AValue : boolean); 

begin
  If (Ffirst=AValue) then exit;
  Ffirst:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.SetinstanceId(AIndex : Integer; const AValue : String); 

begin
  If (FinstanceId=AValue) then exit;
  FinstanceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.Setline(AIndex : Integer; const AValue : TRequestLogTypelineArray); 

begin
  If (Fline=AValue) then exit;
  Fline:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.SetappEngineRelease(AIndex : Integer; const AValue : String); 

begin
  If (FappEngineRelease=AValue) then exit;
  FappEngineRelease:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.SettraceId(AIndex : Integer; const AValue : String); 

begin
  If (FtraceId=AValue) then exit;
  FtraceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestLog.SetsourceReference(AIndex : Integer; const AValue : TRequestLogTypesourceReferenceArray); 

begin
  If (FsourceReference=AValue) then exit;
  FsourceReference:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TRequestLog.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'line' : SetLength(Fline,ALength);
  'sourcereference' : SetLength(FsourceReference,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLogLine
  --------------------------------------------------------------------}


Procedure TLogLine.Settime(AIndex : Integer; const AValue : String); 

begin
  If (Ftime=AValue) then exit;
  Ftime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogLine.Setseverity(AIndex : Integer; const AValue : String); 

begin
  If (Fseverity=AValue) then exit;
  Fseverity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogLine.SetlogMessage(AIndex : Integer; const AValue : String); 

begin
  If (FlogMessage=AValue) then exit;
  FlogMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogLine.SetsourceLocation(AIndex : Integer; const AValue : TSourceLocation); 

begin
  If (FsourceLocation=AValue) then exit;
  FsourceLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourceLocation
  --------------------------------------------------------------------}


Procedure TSourceLocation.Set_file(AIndex : Integer; const AValue : String); 

begin
  If (F_file=AValue) then exit;
  F_file:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceLocation.Setline(AIndex : Integer; const AValue : String); 

begin
  If (Fline=AValue) then exit;
  Fline:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceLocation.SetfunctionName(AIndex : Integer; const AValue : String); 

begin
  If (FfunctionName=AValue) then exit;
  FfunctionName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TSourceLocation.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_file' : Result:='file';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TSourceReference
  --------------------------------------------------------------------}


Procedure TSourceReference.Setrepository(AIndex : Integer; const AValue : String); 

begin
  If (Frepository=AValue) then exit;
  Frepository:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceReference.SetrevisionId(AIndex : Integer; const AValue : String); 

begin
  If (FrevisionId=AValue) then exit;
  FrevisionId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProjectsLogsResource
  --------------------------------------------------------------------}


Class Function TProjectsLogsResource.ResourceName : String;

begin
  Result:='logs';
end;

Class Function TProjectsLogsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TloggingAPI;
end;

Function TProjectsLogsResource.Delete(logName: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v2beta1/{+logName}';
  _Methodid   = 'logging.projects.logs.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['logName',logName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;



{ --------------------------------------------------------------------
  TProjectsSinksResource
  --------------------------------------------------------------------}


Class Function TProjectsSinksResource.ResourceName : String;

begin
  Result:='sinks';
end;

Class Function TProjectsSinksResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TloggingAPI;
end;

Function TProjectsSinksResource.List(projectName: string; AQuery : string = '') : TListSinksResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v2beta1/{+projectName}/sinks';
  _Methodid   = 'logging.projects.sinks.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectName',projectName]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListSinksResponse) as TListSinksResponse;
end;


Function TProjectsSinksResource.List(projectName: string; AQuery : TProjectsSinkslistOptions) : TListSinksResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  Result:=List(projectName,_Q);
end;

Function TProjectsSinksResource.Get(sinkName: string) : TLogSink;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v2beta1/{+sinkName}';
  _Methodid   = 'logging.projects.sinks.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['sinkName',sinkName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TLogSink) as TLogSink;
end;

Function TProjectsSinksResource.Create(projectName: string; aLogSink : TLogSink) : TLogSink;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v2beta1/{+projectName}/sinks';
  _Methodid   = 'logging.projects.sinks.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectName',projectName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aLogSink,TLogSink) as TLogSink;
end;

Function TProjectsSinksResource.Update(sinkName: string; aLogSink : TLogSink) : TLogSink;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v2beta1/{+sinkName}';
  _Methodid   = 'logging.projects.sinks.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['sinkName',sinkName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aLogSink,TLogSink) as TLogSink;
end;

Function TProjectsSinksResource.Delete(sinkName: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v2beta1/{+sinkName}';
  _Methodid   = 'logging.projects.sinks.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['sinkName',sinkName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;



{ --------------------------------------------------------------------
  TProjectsMetricsResource
  --------------------------------------------------------------------}


Class Function TProjectsMetricsResource.ResourceName : String;

begin
  Result:='metrics';
end;

Class Function TProjectsMetricsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TloggingAPI;
end;

Function TProjectsMetricsResource.List(projectName: string; AQuery : string = '') : TListLogMetricsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v2beta1/{+projectName}/metrics';
  _Methodid   = 'logging.projects.metrics.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectName',projectName]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListLogMetricsResponse) as TListLogMetricsResponse;
end;


Function TProjectsMetricsResource.List(projectName: string; AQuery : TProjectsMetricslistOptions) : TListLogMetricsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  Result:=List(projectName,_Q);
end;

Function TProjectsMetricsResource.Get(metricName: string) : TLogMetric;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v2beta1/{+metricName}';
  _Methodid   = 'logging.projects.metrics.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['metricName',metricName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TLogMetric) as TLogMetric;
end;

Function TProjectsMetricsResource.Create(projectName: string; aLogMetric : TLogMetric) : TLogMetric;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v2beta1/{+projectName}/metrics';
  _Methodid   = 'logging.projects.metrics.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectName',projectName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aLogMetric,TLogMetric) as TLogMetric;
end;

Function TProjectsMetricsResource.Update(metricName: string; aLogMetric : TLogMetric) : TLogMetric;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v2beta1/{+metricName}';
  _Methodid   = 'logging.projects.metrics.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['metricName',metricName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aLogMetric,TLogMetric) as TLogMetric;
end;

Function TProjectsMetricsResource.Delete(metricName: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v2beta1/{+metricName}';
  _Methodid   = 'logging.projects.metrics.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['metricName',metricName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
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



Function TProjectsResource.GetLogsInstance : TProjectsLogsResource;

begin
  if (FLogsInstance=Nil) then
    FLogsInstance:=CreateLogsResource;
  Result:=FLogsInstance;
end;

Function TProjectsResource.CreateLogsResource : TProjectsLogsResource;

begin
  Result:=CreateLogsResource(Self);
end;


Function TProjectsResource.CreateLogsResource(AOwner : TComponent) : TProjectsLogsResource;

begin
  Result:=TProjectsLogsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetSinksInstance : TProjectsSinksResource;

begin
  if (FSinksInstance=Nil) then
    FSinksInstance:=CreateSinksResource;
  Result:=FSinksInstance;
end;

Function TProjectsResource.CreateSinksResource : TProjectsSinksResource;

begin
  Result:=CreateSinksResource(Self);
end;


Function TProjectsResource.CreateSinksResource(AOwner : TComponent) : TProjectsSinksResource;

begin
  Result:=TProjectsSinksResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetMetricsInstance : TProjectsMetricsResource;

begin
  if (FMetricsInstance=Nil) then
    FMetricsInstance:=CreateMetricsResource;
  Result:=FMetricsInstance;
end;

Function TProjectsResource.CreateMetricsResource : TProjectsMetricsResource;

begin
  Result:=CreateMetricsResource(Self);
end;


Function TProjectsResource.CreateMetricsResource(AOwner : TComponent) : TProjectsMetricsResource;

begin
  Result:=TProjectsMetricsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TEntriesResource
  --------------------------------------------------------------------}


Class Function TEntriesResource.ResourceName : String;

begin
  Result:='entries';
end;

Class Function TEntriesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TloggingAPI;
end;

Function TEntriesResource.Write(aWriteLogEntriesRequest : TWriteLogEntriesRequest) : TWriteLogEntriesResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v2beta1/entries:write';
  _Methodid   = 'logging.entries.write';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aWriteLogEntriesRequest,TWriteLogEntriesResponse) as TWriteLogEntriesResponse;
end;

Function TEntriesResource.List(aListLogEntriesRequest : TListLogEntriesRequest) : TListLogEntriesResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v2beta1/entries:list';
  _Methodid   = 'logging.entries.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aListLogEntriesRequest,TListLogEntriesResponse) as TListLogEntriesResponse;
end;



{ --------------------------------------------------------------------
  TMonitoredResourceDescriptorsResource
  --------------------------------------------------------------------}


Class Function TMonitoredResourceDescriptorsResource.ResourceName : String;

begin
  Result:='monitoredResourceDescriptors';
end;

Class Function TMonitoredResourceDescriptorsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TloggingAPI;
end;

Function TMonitoredResourceDescriptorsResource.List(AQuery : string = '') : TListMonitoredResourceDescriptorsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v2beta1/monitoredResourceDescriptors';
  _Methodid   = 'logging.monitoredResourceDescriptors.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TListMonitoredResourceDescriptorsResponse) as TListMonitoredResourceDescriptorsResponse;
end;


Function TMonitoredResourceDescriptorsResource.List(AQuery : TMonitoredResourceDescriptorslistOptions) : TListMonitoredResourceDescriptorsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_Q);
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
  Result:='v2beta1';
end;

Class Function TLoggingAPI.APIRevision : String;

begin
  Result:='20160322';
end;

Class Function TLoggingAPI.APIID : String;

begin
  Result:='logging:v2beta1';
end;

Class Function TLoggingAPI.APITitle : String;

begin
  Result:='Google Cloud Logging API';
end;

Class Function TLoggingAPI.APIDescription : String;

begin
  Result:='Writes log entries and manages your logs, log sinks, and logs-based metrics.';
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
  Result:='https://cloud.google.com/logging/docs/';
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
  SetLength(Result,5);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/cloud-platform.read-only';
  Result[1].Description:='View your data across Google Cloud Platform services';
  Result[2].Name:='https://www.googleapis.com/auth/logging.admin';
  Result[2].Description:='Administrate log data for your projects';
  Result[3].Name:='https://www.googleapis.com/auth/logging.read';
  Result[3].Description:='View log data for your projects';
  Result[4].Name:='https://www.googleapis.com/auth/logging.write';
  Result[4].Description:='Submit log data for your projects';
  
end;

Class Function TLoggingAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TLoggingAPI.RegisterAPIResources;

begin
  TEmpty.RegisterObject;
  TWriteLogEntriesRequestTypelabels.RegisterObject;
  TWriteLogEntriesRequest.RegisterObject;
  TMonitoredResourceTypelabels.RegisterObject;
  TMonitoredResource.RegisterObject;
  TLogEntryTypeprotoPayload.RegisterObject;
  TLogEntryTypejsonPayload.RegisterObject;
  TLogEntryTypelabels.RegisterObject;
  TLogEntry.RegisterObject;
  THttpRequest.RegisterObject;
  TLogEntryOperation.RegisterObject;
  TWriteLogEntriesResponse.RegisterObject;
  TListLogEntriesRequest.RegisterObject;
  TListLogEntriesResponseTypeprojectIdErrors.RegisterObject;
  TListLogEntriesResponse.RegisterObject;
  TStatusTypedetailsItem.RegisterObject;
  TStatus.RegisterObject;
  TListMonitoredResourceDescriptorsResponse.RegisterObject;
  TMonitoredResourceDescriptor.RegisterObject;
  TLabelDescriptor.RegisterObject;
  TListSinksResponse.RegisterObject;
  TLogSink.RegisterObject;
  TListLogMetricsResponse.RegisterObject;
  TLogMetric.RegisterObject;
  TRequestLog.RegisterObject;
  TLogLine.RegisterObject;
  TSourceLocation.RegisterObject;
  TSourceReference.RegisterObject;
end;


Function TLoggingAPI.GetProjectsLogsInstance : TProjectsLogsResource;

begin
  if (FProjectsLogsInstance=Nil) then
    FProjectsLogsInstance:=CreateProjectsLogsResource;
  Result:=FProjectsLogsInstance;
end;

Function TLoggingAPI.CreateProjectsLogsResource : TProjectsLogsResource;

begin
  Result:=CreateProjectsLogsResource(Self);
end;


Function TLoggingAPI.CreateProjectsLogsResource(AOwner : TComponent) : TProjectsLogsResource;

begin
  Result:=TProjectsLogsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TLoggingAPI.GetProjectsSinksInstance : TProjectsSinksResource;

begin
  if (FProjectsSinksInstance=Nil) then
    FProjectsSinksInstance:=CreateProjectsSinksResource;
  Result:=FProjectsSinksInstance;
end;

Function TLoggingAPI.CreateProjectsSinksResource : TProjectsSinksResource;

begin
  Result:=CreateProjectsSinksResource(Self);
end;


Function TLoggingAPI.CreateProjectsSinksResource(AOwner : TComponent) : TProjectsSinksResource;

begin
  Result:=TProjectsSinksResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TLoggingAPI.GetProjectsMetricsInstance : TProjectsMetricsResource;

begin
  if (FProjectsMetricsInstance=Nil) then
    FProjectsMetricsInstance:=CreateProjectsMetricsResource;
  Result:=FProjectsMetricsInstance;
end;

Function TLoggingAPI.CreateProjectsMetricsResource : TProjectsMetricsResource;

begin
  Result:=CreateProjectsMetricsResource(Self);
end;


Function TLoggingAPI.CreateProjectsMetricsResource(AOwner : TComponent) : TProjectsMetricsResource;

begin
  Result:=TProjectsMetricsResource.Create(AOwner);
  Result.API:=Self.API;
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
  Result.API:=Self.API;
end;



Function TLoggingAPI.GetEntriesInstance : TEntriesResource;

begin
  if (FEntriesInstance=Nil) then
    FEntriesInstance:=CreateEntriesResource;
  Result:=FEntriesInstance;
end;

Function TLoggingAPI.CreateEntriesResource : TEntriesResource;

begin
  Result:=CreateEntriesResource(Self);
end;


Function TLoggingAPI.CreateEntriesResource(AOwner : TComponent) : TEntriesResource;

begin
  Result:=TEntriesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TLoggingAPI.GetMonitoredResourceDescriptorsInstance : TMonitoredResourceDescriptorsResource;

begin
  if (FMonitoredResourceDescriptorsInstance=Nil) then
    FMonitoredResourceDescriptorsInstance:=CreateMonitoredResourceDescriptorsResource;
  Result:=FMonitoredResourceDescriptorsInstance;
end;

Function TLoggingAPI.CreateMonitoredResourceDescriptorsResource : TMonitoredResourceDescriptorsResource;

begin
  Result:=CreateMonitoredResourceDescriptorsResource(Self);
end;


Function TLoggingAPI.CreateMonitoredResourceDescriptorsResource(AOwner : TComponent) : TMonitoredResourceDescriptorsResource;

begin
  Result:=TMonitoredResourceDescriptorsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TLoggingAPI.RegisterAPI;
end.
