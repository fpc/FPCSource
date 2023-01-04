unit googlefitness;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAggregateBucket = Class;
  TAggregateBy = Class;
  TAggregateRequest = Class;
  TAggregateResponse = Class;
  TApplication = Class;
  TBucketByActivity = Class;
  TBucketBySession = Class;
  TBucketByTime = Class;
  TDataPoint = Class;
  TDataSource = Class;
  TDataType = Class;
  TDataTypeField = Class;
  TDataset = Class;
  TDevice = Class;
  TListDataSourcesResponse = Class;
  TListSessionsResponse = Class;
  TMapValue = Class;
  TSession = Class;
  TValue = Class;
  TValueMapValEntry = Class;
  TAggregateBucketArray = Array of TAggregateBucket;
  TAggregateByArray = Array of TAggregateBy;
  TAggregateRequestArray = Array of TAggregateRequest;
  TAggregateResponseArray = Array of TAggregateResponse;
  TApplicationArray = Array of TApplication;
  TBucketByActivityArray = Array of TBucketByActivity;
  TBucketBySessionArray = Array of TBucketBySession;
  TBucketByTimeArray = Array of TBucketByTime;
  TDataPointArray = Array of TDataPoint;
  TDataSourceArray = Array of TDataSource;
  TDataTypeArray = Array of TDataType;
  TDataTypeFieldArray = Array of TDataTypeField;
  TDatasetArray = Array of TDataset;
  TDeviceArray = Array of TDevice;
  TListDataSourcesResponseArray = Array of TListDataSourcesResponse;
  TListSessionsResponseArray = Array of TListSessionsResponse;
  TMapValueArray = Array of TMapValue;
  TSessionArray = Array of TSession;
  TValueArray = Array of TValue;
  TValueMapValEntryArray = Array of TValueMapValEntry;
  //Anonymous types, using auto-generated names
  TAggregateBucketTypedatasetArray = Array of TDataset;
  TAggregateRequestTypeaggregateByArray = Array of TAggregateBy;
  TAggregateResponseTypebucketArray = Array of TAggregateBucket;
  TDataPointTypevalueArray = Array of TValue;
  TDataTypeTypefieldArray = Array of TDataTypeField;
  TDatasetTypepointArray = Array of TDataPoint;
  TListDataSourcesResponseTypedataSourceArray = Array of TDataSource;
  TListSessionsResponseTypedeletedSessionArray = Array of TSession;
  TListSessionsResponseTypesessionArray = Array of TSession;
  TValueTypemapValArray = Array of TValueMapValEntry;
  
  { --------------------------------------------------------------------
    TAggregateBucket
    --------------------------------------------------------------------}
  
  TAggregateBucket = Class(TGoogleBaseObject)
  Private
    Factivity : integer;
    Fdataset : TAggregateBucketTypedatasetArray;
    FendTimeMillis : String;
    Fsession : TSession;
    FstartTimeMillis : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setactivity(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setdataset(AIndex : Integer; const AValue : TAggregateBucketTypedatasetArray); virtual;
    Procedure SetendTimeMillis(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsession(AIndex : Integer; const AValue : TSession); virtual;
    Procedure SetstartTimeMillis(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property activity : integer Index 0 Read Factivity Write Setactivity;
    Property dataset : TAggregateBucketTypedatasetArray Index 8 Read Fdataset Write Setdataset;
    Property endTimeMillis : String Index 16 Read FendTimeMillis Write SetendTimeMillis;
    Property session : TSession Index 24 Read Fsession Write Setsession;
    Property startTimeMillis : String Index 32 Read FstartTimeMillis Write SetstartTimeMillis;
    Property _type : String Index 40 Read F_type Write Set_type;
  end;
  TAggregateBucketClass = Class of TAggregateBucket;
  
  { --------------------------------------------------------------------
    TAggregateBy
    --------------------------------------------------------------------}
  
  TAggregateBy = Class(TGoogleBaseObject)
  Private
    FdataSourceId : String;
    FdataTypeName : String;
  Protected
    //Property setters
    Procedure SetdataSourceId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdataTypeName(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property dataSourceId : String Index 0 Read FdataSourceId Write SetdataSourceId;
    Property dataTypeName : String Index 8 Read FdataTypeName Write SetdataTypeName;
  end;
  TAggregateByClass = Class of TAggregateBy;
  
  { --------------------------------------------------------------------
    TAggregateRequest
    --------------------------------------------------------------------}
  
  TAggregateRequest = Class(TGoogleBaseObject)
  Private
    FaggregateBy : TAggregateRequestTypeaggregateByArray;
    FbucketByActivitySegment : TBucketByActivity;
    FbucketByActivityType : TBucketByActivity;
    FbucketBySession : TBucketBySession;
    FbucketByTime : TBucketByTime;
    FendTimeMillis : String;
    FstartTimeMillis : String;
  Protected
    //Property setters
    Procedure SetaggregateBy(AIndex : Integer; const AValue : TAggregateRequestTypeaggregateByArray); virtual;
    Procedure SetbucketByActivitySegment(AIndex : Integer; const AValue : TBucketByActivity); virtual;
    Procedure SetbucketByActivityType(AIndex : Integer; const AValue : TBucketByActivity); virtual;
    Procedure SetbucketBySession(AIndex : Integer; const AValue : TBucketBySession); virtual;
    Procedure SetbucketByTime(AIndex : Integer; const AValue : TBucketByTime); virtual;
    Procedure SetendTimeMillis(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstartTimeMillis(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property aggregateBy : TAggregateRequestTypeaggregateByArray Index 0 Read FaggregateBy Write SetaggregateBy;
    Property bucketByActivitySegment : TBucketByActivity Index 8 Read FbucketByActivitySegment Write SetbucketByActivitySegment;
    Property bucketByActivityType : TBucketByActivity Index 16 Read FbucketByActivityType Write SetbucketByActivityType;
    Property bucketBySession : TBucketBySession Index 24 Read FbucketBySession Write SetbucketBySession;
    Property bucketByTime : TBucketByTime Index 32 Read FbucketByTime Write SetbucketByTime;
    Property endTimeMillis : String Index 40 Read FendTimeMillis Write SetendTimeMillis;
    Property startTimeMillis : String Index 48 Read FstartTimeMillis Write SetstartTimeMillis;
  end;
  TAggregateRequestClass = Class of TAggregateRequest;
  
  { --------------------------------------------------------------------
    TAggregateResponse
    --------------------------------------------------------------------}
  
  TAggregateResponse = Class(TGoogleBaseObject)
  Private
    Fbucket : TAggregateResponseTypebucketArray;
  Protected
    //Property setters
    Procedure Setbucket(AIndex : Integer; const AValue : TAggregateResponseTypebucketArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property bucket : TAggregateResponseTypebucketArray Index 0 Read Fbucket Write Setbucket;
  end;
  TAggregateResponseClass = Class of TAggregateResponse;
  
  { --------------------------------------------------------------------
    TApplication
    --------------------------------------------------------------------}
  
  TApplication = Class(TGoogleBaseObject)
  Private
    FdetailsUrl : String;
    Fname : String;
    FpackageName : String;
    Fversion : String;
  Protected
    //Property setters
    Procedure SetdetailsUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpackageName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property detailsUrl : String Index 0 Read FdetailsUrl Write SetdetailsUrl;
    Property name : String Index 8 Read Fname Write Setname;
    Property packageName : String Index 16 Read FpackageName Write SetpackageName;
    Property version : String Index 24 Read Fversion Write Setversion;
  end;
  TApplicationClass = Class of TApplication;
  
  { --------------------------------------------------------------------
    TBucketByActivity
    --------------------------------------------------------------------}
  
  TBucketByActivity = Class(TGoogleBaseObject)
  Private
    FactivityDataSourceId : String;
    FminDurationMillis : String;
  Protected
    //Property setters
    Procedure SetactivityDataSourceId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetminDurationMillis(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property activityDataSourceId : String Index 0 Read FactivityDataSourceId Write SetactivityDataSourceId;
    Property minDurationMillis : String Index 8 Read FminDurationMillis Write SetminDurationMillis;
  end;
  TBucketByActivityClass = Class of TBucketByActivity;
  
  { --------------------------------------------------------------------
    TBucketBySession
    --------------------------------------------------------------------}
  
  TBucketBySession = Class(TGoogleBaseObject)
  Private
    FminDurationMillis : String;
  Protected
    //Property setters
    Procedure SetminDurationMillis(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property minDurationMillis : String Index 0 Read FminDurationMillis Write SetminDurationMillis;
  end;
  TBucketBySessionClass = Class of TBucketBySession;
  
  { --------------------------------------------------------------------
    TBucketByTime
    --------------------------------------------------------------------}
  
  TBucketByTime = Class(TGoogleBaseObject)
  Private
    FdurationMillis : String;
  Protected
    //Property setters
    Procedure SetdurationMillis(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property durationMillis : String Index 0 Read FdurationMillis Write SetdurationMillis;
  end;
  TBucketByTimeClass = Class of TBucketByTime;
  
  { --------------------------------------------------------------------
    TDataPoint
    --------------------------------------------------------------------}
  
  TDataPoint = Class(TGoogleBaseObject)
  Private
    FcomputationTimeMillis : String;
    FdataTypeName : String;
    FendTimeNanos : String;
    FmodifiedTimeMillis : String;
    ForiginDataSourceId : String;
    FrawTimestampNanos : String;
    FstartTimeNanos : String;
    Fvalue : TDataPointTypevalueArray;
  Protected
    //Property setters
    Procedure SetcomputationTimeMillis(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdataTypeName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetendTimeNanos(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmodifiedTimeMillis(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoriginDataSourceId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrawTimestampNanos(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstartTimeNanos(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : TDataPointTypevalueArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property computationTimeMillis : String Index 0 Read FcomputationTimeMillis Write SetcomputationTimeMillis;
    Property dataTypeName : String Index 8 Read FdataTypeName Write SetdataTypeName;
    Property endTimeNanos : String Index 16 Read FendTimeNanos Write SetendTimeNanos;
    Property modifiedTimeMillis : String Index 24 Read FmodifiedTimeMillis Write SetmodifiedTimeMillis;
    Property originDataSourceId : String Index 32 Read ForiginDataSourceId Write SetoriginDataSourceId;
    Property rawTimestampNanos : String Index 40 Read FrawTimestampNanos Write SetrawTimestampNanos;
    Property startTimeNanos : String Index 48 Read FstartTimeNanos Write SetstartTimeNanos;
    Property value : TDataPointTypevalueArray Index 56 Read Fvalue Write Setvalue;
  end;
  TDataPointClass = Class of TDataPoint;
  
  { --------------------------------------------------------------------
    TDataSource
    --------------------------------------------------------------------}
  
  TDataSource = Class(TGoogleBaseObject)
  Private
    Fapplication : TApplication;
    FdataStreamId : String;
    FdataStreamName : String;
    FdataType : TDataType;
    Fdevice : TDevice;
    Fname : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setapplication(AIndex : Integer; const AValue : TApplication); virtual;
    Procedure SetdataStreamId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdataStreamName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdataType(AIndex : Integer; const AValue : TDataType); virtual;
    Procedure Setdevice(AIndex : Integer; const AValue : TDevice); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property application : TApplication Index 0 Read Fapplication Write Setapplication;
    Property dataStreamId : String Index 8 Read FdataStreamId Write SetdataStreamId;
    Property dataStreamName : String Index 16 Read FdataStreamName Write SetdataStreamName;
    Property dataType : TDataType Index 24 Read FdataType Write SetdataType;
    Property device : TDevice Index 32 Read Fdevice Write Setdevice;
    Property name : String Index 40 Read Fname Write Setname;
    Property _type : String Index 48 Read F_type Write Set_type;
  end;
  TDataSourceClass = Class of TDataSource;
  
  { --------------------------------------------------------------------
    TDataType
    --------------------------------------------------------------------}
  
  TDataType = Class(TGoogleBaseObject)
  Private
    Ffield : TDataTypeTypefieldArray;
    Fname : String;
  Protected
    //Property setters
    Procedure Setfield(AIndex : Integer; const AValue : TDataTypeTypefieldArray); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property field : TDataTypeTypefieldArray Index 0 Read Ffield Write Setfield;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TDataTypeClass = Class of TDataType;
  
  { --------------------------------------------------------------------
    TDataTypeField
    --------------------------------------------------------------------}
  
  TDataTypeField = Class(TGoogleBaseObject)
  Private
    Fformat : String;
    Fname : String;
    Foptional : boolean;
  Protected
    //Property setters
    Procedure Setformat(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setoptional(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property format : String Index 0 Read Fformat Write Setformat;
    Property name : String Index 8 Read Fname Write Setname;
    Property optional : boolean Index 16 Read Foptional Write Setoptional;
  end;
  TDataTypeFieldClass = Class of TDataTypeField;
  
  { --------------------------------------------------------------------
    TDataset
    --------------------------------------------------------------------}
  
  TDataset = Class(TGoogleBaseObject)
  Private
    FdataSourceId : String;
    FmaxEndTimeNs : String;
    FminStartTimeNs : String;
    FnextPageToken : String;
    Fpoint : TDatasetTypepointArray;
  Protected
    //Property setters
    Procedure SetdataSourceId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmaxEndTimeNs(AIndex : Integer; const AValue : String); virtual;
    Procedure SetminStartTimeNs(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setpoint(AIndex : Integer; const AValue : TDatasetTypepointArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property dataSourceId : String Index 0 Read FdataSourceId Write SetdataSourceId;
    Property maxEndTimeNs : String Index 8 Read FmaxEndTimeNs Write SetmaxEndTimeNs;
    Property minStartTimeNs : String Index 16 Read FminStartTimeNs Write SetminStartTimeNs;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
    Property point : TDatasetTypepointArray Index 32 Read Fpoint Write Setpoint;
  end;
  TDatasetClass = Class of TDataset;
  
  { --------------------------------------------------------------------
    TDevice
    --------------------------------------------------------------------}
  
  TDevice = Class(TGoogleBaseObject)
  Private
    Fmanufacturer : String;
    Fmodel : String;
    F_type : String;
    Fuid : String;
    Fversion : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setmanufacturer(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmodel(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setuid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property manufacturer : String Index 0 Read Fmanufacturer Write Setmanufacturer;
    Property model : String Index 8 Read Fmodel Write Setmodel;
    Property _type : String Index 16 Read F_type Write Set_type;
    Property uid : String Index 24 Read Fuid Write Setuid;
    Property version : String Index 32 Read Fversion Write Setversion;
  end;
  TDeviceClass = Class of TDevice;
  
  { --------------------------------------------------------------------
    TListDataSourcesResponse
    --------------------------------------------------------------------}
  
  TListDataSourcesResponse = Class(TGoogleBaseObject)
  Private
    FdataSource : TListDataSourcesResponseTypedataSourceArray;
  Protected
    //Property setters
    Procedure SetdataSource(AIndex : Integer; const AValue : TListDataSourcesResponseTypedataSourceArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property dataSource : TListDataSourcesResponseTypedataSourceArray Index 0 Read FdataSource Write SetdataSource;
  end;
  TListDataSourcesResponseClass = Class of TListDataSourcesResponse;
  
  { --------------------------------------------------------------------
    TListSessionsResponse
    --------------------------------------------------------------------}
  
  TListSessionsResponse = Class(TGoogleBaseObject)
  Private
    FdeletedSession : TListSessionsResponseTypedeletedSessionArray;
    FnextPageToken : String;
    Fsession : TListSessionsResponseTypesessionArray;
  Protected
    //Property setters
    Procedure SetdeletedSession(AIndex : Integer; const AValue : TListSessionsResponseTypedeletedSessionArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsession(AIndex : Integer; const AValue : TListSessionsResponseTypesessionArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property deletedSession : TListSessionsResponseTypedeletedSessionArray Index 0 Read FdeletedSession Write SetdeletedSession;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property session : TListSessionsResponseTypesessionArray Index 16 Read Fsession Write Setsession;
  end;
  TListSessionsResponseClass = Class of TListSessionsResponse;
  
  { --------------------------------------------------------------------
    TMapValue
    --------------------------------------------------------------------}
  
  TMapValue = Class(TGoogleBaseObject)
  Private
    FfpVal : double;
  Protected
    //Property setters
    Procedure SetfpVal(AIndex : Integer; const AValue : double); virtual;
  Public
  Published
    Property fpVal : double Index 0 Read FfpVal Write SetfpVal;
  end;
  TMapValueClass = Class of TMapValue;
  
  { --------------------------------------------------------------------
    TSession
    --------------------------------------------------------------------}
  
  TSession = Class(TGoogleBaseObject)
  Private
    FactiveTimeMillis : String;
    FactivityType : integer;
    Fapplication : TApplication;
    Fdescription : String;
    FendTimeMillis : String;
    Fid : String;
    FmodifiedTimeMillis : String;
    Fname : String;
    FstartTimeMillis : String;
  Protected
    //Property setters
    Procedure SetactiveTimeMillis(AIndex : Integer; const AValue : String); virtual;
    Procedure SetactivityType(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setapplication(AIndex : Integer; const AValue : TApplication); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure SetendTimeMillis(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmodifiedTimeMillis(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstartTimeMillis(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property activeTimeMillis : String Index 0 Read FactiveTimeMillis Write SetactiveTimeMillis;
    Property activityType : integer Index 8 Read FactivityType Write SetactivityType;
    Property application : TApplication Index 16 Read Fapplication Write Setapplication;
    Property description : String Index 24 Read Fdescription Write Setdescription;
    Property endTimeMillis : String Index 32 Read FendTimeMillis Write SetendTimeMillis;
    Property id : String Index 40 Read Fid Write Setid;
    Property modifiedTimeMillis : String Index 48 Read FmodifiedTimeMillis Write SetmodifiedTimeMillis;
    Property name : String Index 56 Read Fname Write Setname;
    Property startTimeMillis : String Index 64 Read FstartTimeMillis Write SetstartTimeMillis;
  end;
  TSessionClass = Class of TSession;
  
  { --------------------------------------------------------------------
    TValue
    --------------------------------------------------------------------}
  
  TValue = Class(TGoogleBaseObject)
  Private
    FfpVal : double;
    FintVal : integer;
    FmapVal : TValueTypemapValArray;
    FstringVal : String;
  Protected
    //Property setters
    Procedure SetfpVal(AIndex : Integer; const AValue : double); virtual;
    Procedure SetintVal(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetmapVal(AIndex : Integer; const AValue : TValueTypemapValArray); virtual;
    Procedure SetstringVal(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property fpVal : double Index 0 Read FfpVal Write SetfpVal;
    Property intVal : integer Index 8 Read FintVal Write SetintVal;
    Property mapVal : TValueTypemapValArray Index 16 Read FmapVal Write SetmapVal;
    Property stringVal : String Index 24 Read FstringVal Write SetstringVal;
  end;
  TValueClass = Class of TValue;
  
  { --------------------------------------------------------------------
    TValueMapValEntry
    --------------------------------------------------------------------}
  
  TValueMapValEntry = Class(TGoogleBaseObject)
  Private
    Fkey : String;
    Fvalue : TMapValue;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : TMapValue); virtual;
  Public
  Published
    Property key : String Index 0 Read Fkey Write Setkey;
    Property value : TMapValue Index 8 Read Fvalue Write Setvalue;
  end;
  TValueMapValEntryClass = Class of TValueMapValEntry;
  
  { --------------------------------------------------------------------
    TUsersDataSourcesDatasetsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TUsersDataSourcesDatasetsResource, method Delete
  
  TUsersDataSourcesDatasetsDeleteOptions = Record
    currentTimeMillis : int64;
    modifiedTimeMillis : int64;
  end;
  
  
  //Optional query Options for TUsersDataSourcesDatasetsResource, method Get
  
  TUsersDataSourcesDatasetsGetOptions = Record
    limit : integer;
    pageToken : String;
  end;
  
  
  //Optional query Options for TUsersDataSourcesDatasetsResource, method Patch
  
  TUsersDataSourcesDatasetsPatchOptions = Record
    currentTimeMillis : int64;
  end;
  
  TUsersDataSourcesDatasetsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(dataSourceId: string; datasetId: string; userId: string; AQuery : string  = '');
    Procedure Delete(dataSourceId: string; datasetId: string; userId: string; AQuery : TUsersDataSourcesDatasetsdeleteOptions);
    Function Get(dataSourceId: string; datasetId: string; userId: string; AQuery : string  = '') : TDataset;
    Function Get(dataSourceId: string; datasetId: string; userId: string; AQuery : TUsersDataSourcesDatasetsgetOptions) : TDataset;
    Function Patch(dataSourceId: string; datasetId: string; userId: string; aDataset : TDataset; AQuery : string  = '') : TDataset;
    Function Patch(dataSourceId: string; datasetId: string; userId: string; aDataset : TDataset; AQuery : TUsersDataSourcesDatasetspatchOptions) : TDataset;
  end;
  
  
  { --------------------------------------------------------------------
    TUsersDataSourcesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TUsersDataSourcesResource, method List
  
  TUsersDataSourcesListOptions = Record
    dataTypeName : String;
  end;
  
  TUsersDataSourcesResource = Class(TGoogleResource)
  Private
    FDatasetsInstance : TUsersDataSourcesDatasetsResource;
    Function GetDatasetsInstance : TUsersDataSourcesDatasetsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(userId: string; aDataSource : TDataSource) : TDataSource;overload;
    Function Delete(dataSourceId: string; userId: string) : TDataSource;
    Function Get(dataSourceId: string; userId: string) : TDataSource;
    Function List(userId: string; AQuery : string  = '') : TListDataSourcesResponse;
    Function List(userId: string; AQuery : TUsersDataSourceslistOptions) : TListDataSourcesResponse;
    Function Patch(dataSourceId: string; userId: string; aDataSource : TDataSource) : TDataSource;
    Function Update(dataSourceId: string; userId: string; aDataSource : TDataSource) : TDataSource;
    Function CreateDatasetsResource(AOwner : TComponent) : TUsersDataSourcesDatasetsResource;virtual;overload;
    Function CreateDatasetsResource : TUsersDataSourcesDatasetsResource;virtual;overload;
    Property DatasetsResource : TUsersDataSourcesDatasetsResource Read GetDatasetsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TUsersDatasetResource
    --------------------------------------------------------------------}
  
  TUsersDatasetResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Aggregate(userId: string; aAggregateRequest : TAggregateRequest) : TAggregateResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TUsersSessionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TUsersSessionsResource, method Delete
  
  TUsersSessionsDeleteOptions = Record
    currentTimeMillis : int64;
  end;
  
  
  //Optional query Options for TUsersSessionsResource, method List
  
  TUsersSessionsListOptions = Record
    endTime : String;
    includeDeleted : boolean;
    pageToken : String;
    startTime : String;
  end;
  
  
  //Optional query Options for TUsersSessionsResource, method Update
  
  TUsersSessionsUpdateOptions = Record
    currentTimeMillis : int64;
  end;
  
  TUsersSessionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(sessionId: string; userId: string; AQuery : string  = '');
    Procedure Delete(sessionId: string; userId: string; AQuery : TUsersSessionsdeleteOptions);
    Function List(userId: string; AQuery : string  = '') : TListSessionsResponse;
    Function List(userId: string; AQuery : TUsersSessionslistOptions) : TListSessionsResponse;
    Function Update(sessionId: string; userId: string; aSession : TSession; AQuery : string  = '') : TSession;
    Function Update(sessionId: string; userId: string; aSession : TSession; AQuery : TUsersSessionsupdateOptions) : TSession;
  end;
  
  
  { --------------------------------------------------------------------
    TUsersResource
    --------------------------------------------------------------------}
  
  TUsersResource = Class(TGoogleResource)
  Private
    FDataSourcesDatasetsInstance : TUsersDataSourcesDatasetsResource;
    FDataSourcesInstance : TUsersDataSourcesResource;
    FDatasetInstance : TUsersDatasetResource;
    FSessionsInstance : TUsersSessionsResource;
    Function GetDataSourcesDatasetsInstance : TUsersDataSourcesDatasetsResource;virtual;
    Function GetDataSourcesInstance : TUsersDataSourcesResource;virtual;
    Function GetDatasetInstance : TUsersDatasetResource;virtual;
    Function GetSessionsInstance : TUsersSessionsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateDataSourcesDatasetsResource(AOwner : TComponent) : TUsersDataSourcesDatasetsResource;virtual;overload;
    Function CreateDataSourcesDatasetsResource : TUsersDataSourcesDatasetsResource;virtual;overload;
    Function CreateDataSourcesResource(AOwner : TComponent) : TUsersDataSourcesResource;virtual;overload;
    Function CreateDataSourcesResource : TUsersDataSourcesResource;virtual;overload;
    Function CreateDatasetResource(AOwner : TComponent) : TUsersDatasetResource;virtual;overload;
    Function CreateDatasetResource : TUsersDatasetResource;virtual;overload;
    Function CreateSessionsResource(AOwner : TComponent) : TUsersSessionsResource;virtual;overload;
    Function CreateSessionsResource : TUsersSessionsResource;virtual;overload;
    Property DataSourcesDatasetsResource : TUsersDataSourcesDatasetsResource Read GetDataSourcesDatasetsInstance;
    Property DataSourcesResource : TUsersDataSourcesResource Read GetDataSourcesInstance;
    Property DatasetResource : TUsersDatasetResource Read GetDatasetInstance;
    Property SessionsResource : TUsersSessionsResource Read GetSessionsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TFitnessAPI
    --------------------------------------------------------------------}
  
  TFitnessAPI = Class(TGoogleAPI)
  Private
    FUsersDataSourcesDatasetsInstance : TUsersDataSourcesDatasetsResource;
    FUsersDataSourcesInstance : TUsersDataSourcesResource;
    FUsersDatasetInstance : TUsersDatasetResource;
    FUsersSessionsInstance : TUsersSessionsResource;
    FUsersInstance : TUsersResource;
    Function GetUsersDataSourcesDatasetsInstance : TUsersDataSourcesDatasetsResource;virtual;
    Function GetUsersDataSourcesInstance : TUsersDataSourcesResource;virtual;
    Function GetUsersDatasetInstance : TUsersDatasetResource;virtual;
    Function GetUsersSessionsInstance : TUsersSessionsResource;virtual;
    Function GetUsersInstance : TUsersResource;virtual;
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
    Function CreateUsersDataSourcesDatasetsResource(AOwner : TComponent) : TUsersDataSourcesDatasetsResource;virtual;overload;
    Function CreateUsersDataSourcesDatasetsResource : TUsersDataSourcesDatasetsResource;virtual;overload;
    Function CreateUsersDataSourcesResource(AOwner : TComponent) : TUsersDataSourcesResource;virtual;overload;
    Function CreateUsersDataSourcesResource : TUsersDataSourcesResource;virtual;overload;
    Function CreateUsersDatasetResource(AOwner : TComponent) : TUsersDatasetResource;virtual;overload;
    Function CreateUsersDatasetResource : TUsersDatasetResource;virtual;overload;
    Function CreateUsersSessionsResource(AOwner : TComponent) : TUsersSessionsResource;virtual;overload;
    Function CreateUsersSessionsResource : TUsersSessionsResource;virtual;overload;
    Function CreateUsersResource(AOwner : TComponent) : TUsersResource;virtual;overload;
    Function CreateUsersResource : TUsersResource;virtual;overload;
    //Add default on-demand instances for resources
    Property UsersDataSourcesDatasetsResource : TUsersDataSourcesDatasetsResource Read GetUsersDataSourcesDatasetsInstance;
    Property UsersDataSourcesResource : TUsersDataSourcesResource Read GetUsersDataSourcesInstance;
    Property UsersDatasetResource : TUsersDatasetResource Read GetUsersDatasetInstance;
    Property UsersSessionsResource : TUsersSessionsResource Read GetUsersSessionsInstance;
    Property UsersResource : TUsersResource Read GetUsersInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAggregateBucket
  --------------------------------------------------------------------}


Procedure TAggregateBucket.Setactivity(AIndex : Integer; const AValue : integer); 

begin
  If (Factivity=AValue) then exit;
  Factivity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAggregateBucket.Setdataset(AIndex : Integer; const AValue : TAggregateBucketTypedatasetArray); 

begin
  If (Fdataset=AValue) then exit;
  Fdataset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAggregateBucket.SetendTimeMillis(AIndex : Integer; const AValue : String); 

begin
  If (FendTimeMillis=AValue) then exit;
  FendTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAggregateBucket.Setsession(AIndex : Integer; const AValue : TSession); 

begin
  If (Fsession=AValue) then exit;
  Fsession:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAggregateBucket.SetstartTimeMillis(AIndex : Integer; const AValue : String); 

begin
  If (FstartTimeMillis=AValue) then exit;
  FstartTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAggregateBucket.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAggregateBucket.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAggregateBucket.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'dataset' : SetLength(Fdataset,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAggregateBy
  --------------------------------------------------------------------}


Procedure TAggregateBy.SetdataSourceId(AIndex : Integer; const AValue : String); 

begin
  If (FdataSourceId=AValue) then exit;
  FdataSourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAggregateBy.SetdataTypeName(AIndex : Integer; const AValue : String); 

begin
  If (FdataTypeName=AValue) then exit;
  FdataTypeName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAggregateRequest
  --------------------------------------------------------------------}


Procedure TAggregateRequest.SetaggregateBy(AIndex : Integer; const AValue : TAggregateRequestTypeaggregateByArray); 

begin
  If (FaggregateBy=AValue) then exit;
  FaggregateBy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAggregateRequest.SetbucketByActivitySegment(AIndex : Integer; const AValue : TBucketByActivity); 

begin
  If (FbucketByActivitySegment=AValue) then exit;
  FbucketByActivitySegment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAggregateRequest.SetbucketByActivityType(AIndex : Integer; const AValue : TBucketByActivity); 

begin
  If (FbucketByActivityType=AValue) then exit;
  FbucketByActivityType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAggregateRequest.SetbucketBySession(AIndex : Integer; const AValue : TBucketBySession); 

begin
  If (FbucketBySession=AValue) then exit;
  FbucketBySession:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAggregateRequest.SetbucketByTime(AIndex : Integer; const AValue : TBucketByTime); 

begin
  If (FbucketByTime=AValue) then exit;
  FbucketByTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAggregateRequest.SetendTimeMillis(AIndex : Integer; const AValue : String); 

begin
  If (FendTimeMillis=AValue) then exit;
  FendTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAggregateRequest.SetstartTimeMillis(AIndex : Integer; const AValue : String); 

begin
  If (FstartTimeMillis=AValue) then exit;
  FstartTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAggregateRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'aggregateby' : SetLength(FaggregateBy,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAggregateResponse
  --------------------------------------------------------------------}


Procedure TAggregateResponse.Setbucket(AIndex : Integer; const AValue : TAggregateResponseTypebucketArray); 

begin
  If (Fbucket=AValue) then exit;
  Fbucket:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAggregateResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'bucket' : SetLength(Fbucket,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TApplication
  --------------------------------------------------------------------}


Procedure TApplication.SetdetailsUrl(AIndex : Integer; const AValue : String); 

begin
  If (FdetailsUrl=AValue) then exit;
  FdetailsUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.SetpackageName(AIndex : Integer; const AValue : String); 

begin
  If (FpackageName=AValue) then exit;
  FpackageName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.Setversion(AIndex : Integer; const AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketByActivity
  --------------------------------------------------------------------}


Procedure TBucketByActivity.SetactivityDataSourceId(AIndex : Integer; const AValue : String); 

begin
  If (FactivityDataSourceId=AValue) then exit;
  FactivityDataSourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketByActivity.SetminDurationMillis(AIndex : Integer; const AValue : String); 

begin
  If (FminDurationMillis=AValue) then exit;
  FminDurationMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketBySession
  --------------------------------------------------------------------}


Procedure TBucketBySession.SetminDurationMillis(AIndex : Integer; const AValue : String); 

begin
  If (FminDurationMillis=AValue) then exit;
  FminDurationMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketByTime
  --------------------------------------------------------------------}


Procedure TBucketByTime.SetdurationMillis(AIndex : Integer; const AValue : String); 

begin
  If (FdurationMillis=AValue) then exit;
  FdurationMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDataPoint
  --------------------------------------------------------------------}


Procedure TDataPoint.SetcomputationTimeMillis(AIndex : Integer; const AValue : String); 

begin
  If (FcomputationTimeMillis=AValue) then exit;
  FcomputationTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataPoint.SetdataTypeName(AIndex : Integer; const AValue : String); 

begin
  If (FdataTypeName=AValue) then exit;
  FdataTypeName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataPoint.SetendTimeNanos(AIndex : Integer; const AValue : String); 

begin
  If (FendTimeNanos=AValue) then exit;
  FendTimeNanos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataPoint.SetmodifiedTimeMillis(AIndex : Integer; const AValue : String); 

begin
  If (FmodifiedTimeMillis=AValue) then exit;
  FmodifiedTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataPoint.SetoriginDataSourceId(AIndex : Integer; const AValue : String); 

begin
  If (ForiginDataSourceId=AValue) then exit;
  ForiginDataSourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataPoint.SetrawTimestampNanos(AIndex : Integer; const AValue : String); 

begin
  If (FrawTimestampNanos=AValue) then exit;
  FrawTimestampNanos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataPoint.SetstartTimeNanos(AIndex : Integer; const AValue : String); 

begin
  If (FstartTimeNanos=AValue) then exit;
  FstartTimeNanos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataPoint.Setvalue(AIndex : Integer; const AValue : TDataPointTypevalueArray); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDataPoint.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'value' : SetLength(Fvalue,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDataSource
  --------------------------------------------------------------------}


Procedure TDataSource.Setapplication(AIndex : Integer; const AValue : TApplication); 

begin
  If (Fapplication=AValue) then exit;
  Fapplication:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataSource.SetdataStreamId(AIndex : Integer; const AValue : String); 

begin
  If (FdataStreamId=AValue) then exit;
  FdataStreamId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataSource.SetdataStreamName(AIndex : Integer; const AValue : String); 

begin
  If (FdataStreamName=AValue) then exit;
  FdataStreamName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataSource.SetdataType(AIndex : Integer; const AValue : TDataType); 

begin
  If (FdataType=AValue) then exit;
  FdataType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataSource.Setdevice(AIndex : Integer; const AValue : TDevice); 

begin
  If (Fdevice=AValue) then exit;
  Fdevice:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataSource.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataSource.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TDataSource.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TDataType
  --------------------------------------------------------------------}


Procedure TDataType.Setfield(AIndex : Integer; const AValue : TDataTypeTypefieldArray); 

begin
  If (Ffield=AValue) then exit;
  Ffield:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataType.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDataType.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'field' : SetLength(Ffield,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDataTypeField
  --------------------------------------------------------------------}


Procedure TDataTypeField.Setformat(AIndex : Integer; const AValue : String); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataTypeField.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataTypeField.Setoptional(AIndex : Integer; const AValue : boolean); 

begin
  If (Foptional=AValue) then exit;
  Foptional:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDataset
  --------------------------------------------------------------------}


Procedure TDataset.SetdataSourceId(AIndex : Integer; const AValue : String); 

begin
  If (FdataSourceId=AValue) then exit;
  FdataSourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.SetmaxEndTimeNs(AIndex : Integer; const AValue : String); 

begin
  If (FmaxEndTimeNs=AValue) then exit;
  FmaxEndTimeNs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.SetminStartTimeNs(AIndex : Integer; const AValue : String); 

begin
  If (FminStartTimeNs=AValue) then exit;
  FminStartTimeNs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.Setpoint(AIndex : Integer; const AValue : TDatasetTypepointArray); 

begin
  If (Fpoint=AValue) then exit;
  Fpoint:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDataset.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'point' : SetLength(Fpoint,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDevice
  --------------------------------------------------------------------}


Procedure TDevice.Setmanufacturer(AIndex : Integer; const AValue : String); 

begin
  If (Fmanufacturer=AValue) then exit;
  Fmanufacturer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDevice.Setmodel(AIndex : Integer; const AValue : String); 

begin
  If (Fmodel=AValue) then exit;
  Fmodel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDevice.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDevice.Setuid(AIndex : Integer; const AValue : String); 

begin
  If (Fuid=AValue) then exit;
  Fuid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDevice.Setversion(AIndex : Integer; const AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TDevice.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TListDataSourcesResponse
  --------------------------------------------------------------------}


Procedure TListDataSourcesResponse.SetdataSource(AIndex : Integer; const AValue : TListDataSourcesResponseTypedataSourceArray); 

begin
  If (FdataSource=AValue) then exit;
  FdataSource:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListDataSourcesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'datasource' : SetLength(FdataSource,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListSessionsResponse
  --------------------------------------------------------------------}


Procedure TListSessionsResponse.SetdeletedSession(AIndex : Integer; const AValue : TListSessionsResponseTypedeletedSessionArray); 

begin
  If (FdeletedSession=AValue) then exit;
  FdeletedSession:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListSessionsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListSessionsResponse.Setsession(AIndex : Integer; const AValue : TListSessionsResponseTypesessionArray); 

begin
  If (Fsession=AValue) then exit;
  Fsession:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListSessionsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'deletedsession' : SetLength(FdeletedSession,ALength);
  'session' : SetLength(Fsession,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMapValue
  --------------------------------------------------------------------}


Procedure TMapValue.SetfpVal(AIndex : Integer; const AValue : double); 

begin
  If (FfpVal=AValue) then exit;
  FfpVal:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSession
  --------------------------------------------------------------------}


Procedure TSession.SetactiveTimeMillis(AIndex : Integer; const AValue : String); 

begin
  If (FactiveTimeMillis=AValue) then exit;
  FactiveTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSession.SetactivityType(AIndex : Integer; const AValue : integer); 

begin
  If (FactivityType=AValue) then exit;
  FactivityType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSession.Setapplication(AIndex : Integer; const AValue : TApplication); 

begin
  If (Fapplication=AValue) then exit;
  Fapplication:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSession.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSession.SetendTimeMillis(AIndex : Integer; const AValue : String); 

begin
  If (FendTimeMillis=AValue) then exit;
  FendTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSession.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSession.SetmodifiedTimeMillis(AIndex : Integer; const AValue : String); 

begin
  If (FmodifiedTimeMillis=AValue) then exit;
  FmodifiedTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSession.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSession.SetstartTimeMillis(AIndex : Integer; const AValue : String); 

begin
  If (FstartTimeMillis=AValue) then exit;
  FstartTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TValue
  --------------------------------------------------------------------}


Procedure TValue.SetfpVal(AIndex : Integer; const AValue : double); 

begin
  If (FfpVal=AValue) then exit;
  FfpVal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetintVal(AIndex : Integer; const AValue : integer); 

begin
  If (FintVal=AValue) then exit;
  FintVal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetmapVal(AIndex : Integer; const AValue : TValueTypemapValArray); 

begin
  If (FmapVal=AValue) then exit;
  FmapVal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetstringVal(AIndex : Integer; const AValue : String); 

begin
  If (FstringVal=AValue) then exit;
  FstringVal:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TValue.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'mapval' : SetLength(FmapVal,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TValueMapValEntry
  --------------------------------------------------------------------}


Procedure TValueMapValEntry.Setkey(AIndex : Integer; const AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValueMapValEntry.Setvalue(AIndex : Integer; const AValue : TMapValue); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUsersDataSourcesDatasetsResource
  --------------------------------------------------------------------}


Class Function TUsersDataSourcesDatasetsResource.ResourceName : String;

begin
  Result:='datasets';
end;

Class Function TUsersDataSourcesDatasetsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TfitnessAPI;
end;

Procedure TUsersDataSourcesDatasetsResource.Delete(dataSourceId: string; datasetId: string; userId: string; AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{userId}/dataSources/{dataSourceId}/datasets/{datasetId}';
  _Methodid   = 'fitness.users.dataSources.datasets.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['dataSourceId',dataSourceId,'datasetId',datasetId,'userId',userId]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TUsersDataSourcesDatasetsResource.Delete(dataSourceId: string; datasetId: string; userId: string; AQuery : TUsersDataSourcesDatasetsdeleteOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'currentTimeMillis',AQuery.currentTimeMillis);
  AddToQuery(_Q,'modifiedTimeMillis',AQuery.modifiedTimeMillis);
  Delete(dataSourceId,datasetId,userId,_Q);
end;

Function TUsersDataSourcesDatasetsResource.Get(dataSourceId: string; datasetId: string; userId: string; AQuery : string = '') : TDataset;

Const
  _HTTPMethod = 'GET';
  _Path       = '{userId}/dataSources/{dataSourceId}/datasets/{datasetId}';
  _Methodid   = 'fitness.users.dataSources.datasets.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['dataSourceId',dataSourceId,'datasetId',datasetId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TDataset) as TDataset;
end;


Function TUsersDataSourcesDatasetsResource.Get(dataSourceId: string; datasetId: string; userId: string; AQuery : TUsersDataSourcesDatasetsgetOptions) : TDataset;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'limit',AQuery.limit);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=Get(dataSourceId,datasetId,userId,_Q);
end;

Function TUsersDataSourcesDatasetsResource.Patch(dataSourceId: string; datasetId: string; userId: string; aDataset : TDataset; AQuery : string = '') : TDataset;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{userId}/dataSources/{dataSourceId}/datasets/{datasetId}';
  _Methodid   = 'fitness.users.dataSources.datasets.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['dataSourceId',dataSourceId,'datasetId',datasetId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aDataset,TDataset) as TDataset;
end;


Function TUsersDataSourcesDatasetsResource.Patch(dataSourceId: string; datasetId: string; userId: string; aDataset : TDataset; AQuery : TUsersDataSourcesDatasetspatchOptions) : TDataset;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'currentTimeMillis',AQuery.currentTimeMillis);
  Result:=Patch(dataSourceId,datasetId,userId,aDataset,_Q);
end;



{ --------------------------------------------------------------------
  TUsersDataSourcesResource
  --------------------------------------------------------------------}


Class Function TUsersDataSourcesResource.ResourceName : String;

begin
  Result:='dataSources';
end;

Class Function TUsersDataSourcesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TfitnessAPI;
end;

Function TUsersDataSourcesResource.Create(userId: string; aDataSource : TDataSource) : TDataSource;

Const
  _HTTPMethod = 'POST';
  _Path       = '{userId}/dataSources';
  _Methodid   = 'fitness.users.dataSources.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDataSource,TDataSource) as TDataSource;
end;

Function TUsersDataSourcesResource.Delete(dataSourceId: string; userId: string) : TDataSource;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{userId}/dataSources/{dataSourceId}';
  _Methodid   = 'fitness.users.dataSources.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['dataSourceId',dataSourceId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDataSource) as TDataSource;
end;

Function TUsersDataSourcesResource.Get(dataSourceId: string; userId: string) : TDataSource;

Const
  _HTTPMethod = 'GET';
  _Path       = '{userId}/dataSources/{dataSourceId}';
  _Methodid   = 'fitness.users.dataSources.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['dataSourceId',dataSourceId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDataSource) as TDataSource;
end;

Function TUsersDataSourcesResource.List(userId: string; AQuery : string = '') : TListDataSourcesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{userId}/dataSources';
  _Methodid   = 'fitness.users.dataSources.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListDataSourcesResponse) as TListDataSourcesResponse;
end;


Function TUsersDataSourcesResource.List(userId: string; AQuery : TUsersDataSourceslistOptions) : TListDataSourcesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'dataTypeName',AQuery.dataTypeName);
  Result:=List(userId,_Q);
end;

Function TUsersDataSourcesResource.Patch(dataSourceId: string; userId: string; aDataSource : TDataSource) : TDataSource;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{userId}/dataSources/{dataSourceId}';
  _Methodid   = 'fitness.users.dataSources.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['dataSourceId',dataSourceId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDataSource,TDataSource) as TDataSource;
end;

Function TUsersDataSourcesResource.Update(dataSourceId: string; userId: string; aDataSource : TDataSource) : TDataSource;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{userId}/dataSources/{dataSourceId}';
  _Methodid   = 'fitness.users.dataSources.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['dataSourceId',dataSourceId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDataSource,TDataSource) as TDataSource;
end;



Function TUsersDataSourcesResource.GetDatasetsInstance : TUsersDataSourcesDatasetsResource;

begin
  if (FDatasetsInstance=Nil) then
    FDatasetsInstance:=CreateDatasetsResource;
  Result:=FDatasetsInstance;
end;

Function TUsersDataSourcesResource.CreateDatasetsResource : TUsersDataSourcesDatasetsResource;

begin
  Result:=CreateDatasetsResource(Self);
end;


Function TUsersDataSourcesResource.CreateDatasetsResource(AOwner : TComponent) : TUsersDataSourcesDatasetsResource;

begin
  Result:=TUsersDataSourcesDatasetsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TUsersDatasetResource
  --------------------------------------------------------------------}


Class Function TUsersDatasetResource.ResourceName : String;

begin
  Result:='dataset';
end;

Class Function TUsersDatasetResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TfitnessAPI;
end;

Function TUsersDatasetResource.Aggregate(userId: string; aAggregateRequest : TAggregateRequest) : TAggregateResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{userId}/dataset:aggregate';
  _Methodid   = 'fitness.users.dataset.aggregate';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAggregateRequest,TAggregateResponse) as TAggregateResponse;
end;



{ --------------------------------------------------------------------
  TUsersSessionsResource
  --------------------------------------------------------------------}


Class Function TUsersSessionsResource.ResourceName : String;

begin
  Result:='sessions';
end;

Class Function TUsersSessionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TfitnessAPI;
end;

Procedure TUsersSessionsResource.Delete(sessionId: string; userId: string; AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{userId}/sessions/{sessionId}';
  _Methodid   = 'fitness.users.sessions.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['sessionId',sessionId,'userId',userId]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TUsersSessionsResource.Delete(sessionId: string; userId: string; AQuery : TUsersSessionsdeleteOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'currentTimeMillis',AQuery.currentTimeMillis);
  Delete(sessionId,userId,_Q);
end;

Function TUsersSessionsResource.List(userId: string; AQuery : string = '') : TListSessionsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{userId}/sessions';
  _Methodid   = 'fitness.users.sessions.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListSessionsResponse) as TListSessionsResponse;
end;


Function TUsersSessionsResource.List(userId: string; AQuery : TUsersSessionslistOptions) : TListSessionsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'endTime',AQuery.endTime);
  AddToQuery(_Q,'includeDeleted',AQuery.includeDeleted);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'startTime',AQuery.startTime);
  Result:=List(userId,_Q);
end;

Function TUsersSessionsResource.Update(sessionId: string; userId: string; aSession : TSession; AQuery : string = '') : TSession;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{userId}/sessions/{sessionId}';
  _Methodid   = 'fitness.users.sessions.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['sessionId',sessionId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aSession,TSession) as TSession;
end;


Function TUsersSessionsResource.Update(sessionId: string; userId: string; aSession : TSession; AQuery : TUsersSessionsupdateOptions) : TSession;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'currentTimeMillis',AQuery.currentTimeMillis);
  Result:=Update(sessionId,userId,aSession,_Q);
end;



{ --------------------------------------------------------------------
  TUsersResource
  --------------------------------------------------------------------}


Class Function TUsersResource.ResourceName : String;

begin
  Result:='users';
end;

Class Function TUsersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TfitnessAPI;
end;



Function TUsersResource.GetDataSourcesDatasetsInstance : TUsersDataSourcesDatasetsResource;

begin
  if (FDataSourcesDatasetsInstance=Nil) then
    FDataSourcesDatasetsInstance:=CreateDataSourcesDatasetsResource;
  Result:=FDataSourcesDatasetsInstance;
end;

Function TUsersResource.CreateDataSourcesDatasetsResource : TUsersDataSourcesDatasetsResource;

begin
  Result:=CreateDataSourcesDatasetsResource(Self);
end;


Function TUsersResource.CreateDataSourcesDatasetsResource(AOwner : TComponent) : TUsersDataSourcesDatasetsResource;

begin
  Result:=TUsersDataSourcesDatasetsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TUsersResource.GetDataSourcesInstance : TUsersDataSourcesResource;

begin
  if (FDataSourcesInstance=Nil) then
    FDataSourcesInstance:=CreateDataSourcesResource;
  Result:=FDataSourcesInstance;
end;

Function TUsersResource.CreateDataSourcesResource : TUsersDataSourcesResource;

begin
  Result:=CreateDataSourcesResource(Self);
end;


Function TUsersResource.CreateDataSourcesResource(AOwner : TComponent) : TUsersDataSourcesResource;

begin
  Result:=TUsersDataSourcesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TUsersResource.GetDatasetInstance : TUsersDatasetResource;

begin
  if (FDatasetInstance=Nil) then
    FDatasetInstance:=CreateDatasetResource;
  Result:=FDatasetInstance;
end;

Function TUsersResource.CreateDatasetResource : TUsersDatasetResource;

begin
  Result:=CreateDatasetResource(Self);
end;


Function TUsersResource.CreateDatasetResource(AOwner : TComponent) : TUsersDatasetResource;

begin
  Result:=TUsersDatasetResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TUsersResource.GetSessionsInstance : TUsersSessionsResource;

begin
  if (FSessionsInstance=Nil) then
    FSessionsInstance:=CreateSessionsResource;
  Result:=FSessionsInstance;
end;

Function TUsersResource.CreateSessionsResource : TUsersSessionsResource;

begin
  Result:=CreateSessionsResource(Self);
end;


Function TUsersResource.CreateSessionsResource(AOwner : TComponent) : TUsersSessionsResource;

begin
  Result:=TUsersSessionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TFitnessAPI
  --------------------------------------------------------------------}

Class Function TFitnessAPI.APIName : String;

begin
  Result:='fitness';
end;

Class Function TFitnessAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TFitnessAPI.APIRevision : String;

begin
  Result:='20151021';
end;

Class Function TFitnessAPI.APIID : String;

begin
  Result:='fitness:v1';
end;

Class Function TFitnessAPI.APITitle : String;

begin
  Result:='Fitness';
end;

Class Function TFitnessAPI.APIDescription : String;

begin
  Result:='Google Fit API';
end;

Class Function TFitnessAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TFitnessAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TFitnessAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TFitnessAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TFitnessAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/fit/rest/';
end;

Class Function TFitnessAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TFitnessAPI.APIbasePath : string;

begin
  Result:='/fitness/v1/users/';
end;

Class Function TFitnessAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/fitness/v1/users/';
end;

Class Function TFitnessAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TFitnessAPI.APIservicePath : string;

begin
  Result:='fitness/v1/users/';
end;

Class Function TFitnessAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TFitnessAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,6);
  Result[0].Name:='https://www.googleapis.com/auth/fitness.activity.read';
  Result[0].Description:='View your activity information in Google Fit';
  Result[1].Name:='https://www.googleapis.com/auth/fitness.activity.write';
  Result[1].Description:='View and store your activity information in Google Fit';
  Result[2].Name:='https://www.googleapis.com/auth/fitness.body.read';
  Result[2].Description:='View body sensor information in Google Fit';
  Result[3].Name:='https://www.googleapis.com/auth/fitness.body.write';
  Result[3].Description:='View and store body sensor data in Google Fit';
  Result[4].Name:='https://www.googleapis.com/auth/fitness.location.read';
  Result[4].Description:='View your stored location data in Google Fit';
  Result[5].Name:='https://www.googleapis.com/auth/fitness.location.write';
  Result[5].Description:='View and store your location data in Google Fit';
  
end;

Class Function TFitnessAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TFitnessAPI.RegisterAPIResources;

begin
  TAggregateBucket.RegisterObject;
  TAggregateBy.RegisterObject;
  TAggregateRequest.RegisterObject;
  TAggregateResponse.RegisterObject;
  TApplication.RegisterObject;
  TBucketByActivity.RegisterObject;
  TBucketBySession.RegisterObject;
  TBucketByTime.RegisterObject;
  TDataPoint.RegisterObject;
  TDataSource.RegisterObject;
  TDataType.RegisterObject;
  TDataTypeField.RegisterObject;
  TDataset.RegisterObject;
  TDevice.RegisterObject;
  TListDataSourcesResponse.RegisterObject;
  TListSessionsResponse.RegisterObject;
  TMapValue.RegisterObject;
  TSession.RegisterObject;
  TValue.RegisterObject;
  TValueMapValEntry.RegisterObject;
end;


Function TFitnessAPI.GetUsersDataSourcesDatasetsInstance : TUsersDataSourcesDatasetsResource;

begin
  if (FUsersDataSourcesDatasetsInstance=Nil) then
    FUsersDataSourcesDatasetsInstance:=CreateUsersDataSourcesDatasetsResource;
  Result:=FUsersDataSourcesDatasetsInstance;
end;

Function TFitnessAPI.CreateUsersDataSourcesDatasetsResource : TUsersDataSourcesDatasetsResource;

begin
  Result:=CreateUsersDataSourcesDatasetsResource(Self);
end;


Function TFitnessAPI.CreateUsersDataSourcesDatasetsResource(AOwner : TComponent) : TUsersDataSourcesDatasetsResource;

begin
  Result:=TUsersDataSourcesDatasetsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TFitnessAPI.GetUsersDataSourcesInstance : TUsersDataSourcesResource;

begin
  if (FUsersDataSourcesInstance=Nil) then
    FUsersDataSourcesInstance:=CreateUsersDataSourcesResource;
  Result:=FUsersDataSourcesInstance;
end;

Function TFitnessAPI.CreateUsersDataSourcesResource : TUsersDataSourcesResource;

begin
  Result:=CreateUsersDataSourcesResource(Self);
end;


Function TFitnessAPI.CreateUsersDataSourcesResource(AOwner : TComponent) : TUsersDataSourcesResource;

begin
  Result:=TUsersDataSourcesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TFitnessAPI.GetUsersDatasetInstance : TUsersDatasetResource;

begin
  if (FUsersDatasetInstance=Nil) then
    FUsersDatasetInstance:=CreateUsersDatasetResource;
  Result:=FUsersDatasetInstance;
end;

Function TFitnessAPI.CreateUsersDatasetResource : TUsersDatasetResource;

begin
  Result:=CreateUsersDatasetResource(Self);
end;


Function TFitnessAPI.CreateUsersDatasetResource(AOwner : TComponent) : TUsersDatasetResource;

begin
  Result:=TUsersDatasetResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TFitnessAPI.GetUsersSessionsInstance : TUsersSessionsResource;

begin
  if (FUsersSessionsInstance=Nil) then
    FUsersSessionsInstance:=CreateUsersSessionsResource;
  Result:=FUsersSessionsInstance;
end;

Function TFitnessAPI.CreateUsersSessionsResource : TUsersSessionsResource;

begin
  Result:=CreateUsersSessionsResource(Self);
end;


Function TFitnessAPI.CreateUsersSessionsResource(AOwner : TComponent) : TUsersSessionsResource;

begin
  Result:=TUsersSessionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TFitnessAPI.GetUsersInstance : TUsersResource;

begin
  if (FUsersInstance=Nil) then
    FUsersInstance:=CreateUsersResource;
  Result:=FUsersInstance;
end;

Function TFitnessAPI.CreateUsersResource : TUsersResource;

begin
  Result:=CreateUsersResource(Self);
end;


Function TFitnessAPI.CreateUsersResource(AOwner : TComponent) : TUsersResource;

begin
  Result:=TUsersResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TFitnessAPI.RegisterAPI;
end.
