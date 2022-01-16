unit googlestoragetransfer;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TGoogleServiceAccount = Class;
  TTransferJob = Class;
  TTransferSpec = Class;
  TGcsData = Class;
  TAwsS3Data = Class;
  TAwsAccessKey = Class;
  THttpData = Class;
  TObjectConditions = Class;
  TTransferOptions = Class;
  TSchedule = Class;
  TDate = Class;
  TTimeOfDay = Class;
  TUpdateTransferJobRequest = Class;
  TListTransferJobsResponse = Class;
  TPauseTransferOperationRequest = Class;
  TEmpty = Class;
  TResumeTransferOperationRequest = Class;
  TOperation = Class;
  TStatus = Class;
  TListOperationsResponse = Class;
  TTransferOperation = Class;
  TTransferCounters = Class;
  TErrorSummary = Class;
  TErrorLogEntry = Class;
  TGoogleServiceAccountArray = Array of TGoogleServiceAccount;
  TTransferJobArray = Array of TTransferJob;
  TTransferSpecArray = Array of TTransferSpec;
  TGcsDataArray = Array of TGcsData;
  TAwsS3DataArray = Array of TAwsS3Data;
  TAwsAccessKeyArray = Array of TAwsAccessKey;
  THttpDataArray = Array of THttpData;
  TObjectConditionsArray = Array of TObjectConditions;
  TTransferOptionsArray = Array of TTransferOptions;
  TScheduleArray = Array of TSchedule;
  TDateArray = Array of TDate;
  TTimeOfDayArray = Array of TTimeOfDay;
  TUpdateTransferJobRequestArray = Array of TUpdateTransferJobRequest;
  TListTransferJobsResponseArray = Array of TListTransferJobsResponse;
  TPauseTransferOperationRequestArray = Array of TPauseTransferOperationRequest;
  TEmptyArray = Array of TEmpty;
  TResumeTransferOperationRequestArray = Array of TResumeTransferOperationRequest;
  TOperationArray = Array of TOperation;
  TStatusArray = Array of TStatus;
  TListOperationsResponseArray = Array of TListOperationsResponse;
  TTransferOperationArray = Array of TTransferOperation;
  TTransferCountersArray = Array of TTransferCounters;
  TErrorSummaryArray = Array of TErrorSummary;
  TErrorLogEntryArray = Array of TErrorLogEntry;
  //Anonymous types, using auto-generated names
  TOperationTypemetadata = Class;
  TOperationTyperesponse = Class;
  TStatusTypedetailsItem = Class;
  TListTransferJobsResponseTypetransferJobsArray = Array of TTransferJob;
  TStatusTypedetailsArray = Array of TStatusTypedetailsItem;
  TListOperationsResponseTypeoperationsArray = Array of TOperation;
  TTransferOperationTypeerrorBreakdownsArray = Array of TErrorSummary;
  TErrorSummaryTypeerrorLogEntriesArray = Array of TErrorLogEntry;
  
  { --------------------------------------------------------------------
    TGoogleServiceAccount
    --------------------------------------------------------------------}
  
  TGoogleServiceAccount = Class(TGoogleBaseObject)
  Private
    FaccountEmail : String;
  Protected
    //Property setters
    Procedure SetaccountEmail(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property accountEmail : String Index 0 Read FaccountEmail Write SetaccountEmail;
  end;
  TGoogleServiceAccountClass = Class of TGoogleServiceAccount;
  
  { --------------------------------------------------------------------
    TTransferJob
    --------------------------------------------------------------------}
  
  TTransferJob = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fdescription : String;
    FprojectId : String;
    FtransferSpec : TTransferSpec;
    Fschedule : TSchedule;
    Fstatus : String;
    FcreationTime : String;
    FlastModificationTime : String;
    FdeletionTime : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
    Procedure SettransferSpec(AIndex : Integer; const AValue : TTransferSpec); virtual;
    Procedure Setschedule(AIndex : Integer; const AValue : TSchedule); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcreationTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlastModificationTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdeletionTime(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property projectId : String Index 16 Read FprojectId Write SetprojectId;
    Property transferSpec : TTransferSpec Index 24 Read FtransferSpec Write SettransferSpec;
    Property schedule : TSchedule Index 32 Read Fschedule Write Setschedule;
    Property status : String Index 40 Read Fstatus Write Setstatus;
    Property creationTime : String Index 48 Read FcreationTime Write SetcreationTime;
    Property lastModificationTime : String Index 56 Read FlastModificationTime Write SetlastModificationTime;
    Property deletionTime : String Index 64 Read FdeletionTime Write SetdeletionTime;
  end;
  TTransferJobClass = Class of TTransferJob;
  
  { --------------------------------------------------------------------
    TTransferSpec
    --------------------------------------------------------------------}
  
  TTransferSpec = Class(TGoogleBaseObject)
  Private
    FgcsDataSource : TGcsData;
    FawsS3DataSource : TAwsS3Data;
    FhttpDataSource : THttpData;
    FgcsDataSink : TGcsData;
    FobjectConditions : TObjectConditions;
    FtransferOptions : TTransferOptions;
  Protected
    //Property setters
    Procedure SetgcsDataSource(AIndex : Integer; const AValue : TGcsData); virtual;
    Procedure SetawsS3DataSource(AIndex : Integer; const AValue : TAwsS3Data); virtual;
    Procedure SethttpDataSource(AIndex : Integer; const AValue : THttpData); virtual;
    Procedure SetgcsDataSink(AIndex : Integer; const AValue : TGcsData); virtual;
    Procedure SetobjectConditions(AIndex : Integer; const AValue : TObjectConditions); virtual;
    Procedure SettransferOptions(AIndex : Integer; const AValue : TTransferOptions); virtual;
  Public
  Published
    Property gcsDataSource : TGcsData Index 0 Read FgcsDataSource Write SetgcsDataSource;
    Property awsS3DataSource : TAwsS3Data Index 8 Read FawsS3DataSource Write SetawsS3DataSource;
    Property httpDataSource : THttpData Index 16 Read FhttpDataSource Write SethttpDataSource;
    Property gcsDataSink : TGcsData Index 24 Read FgcsDataSink Write SetgcsDataSink;
    Property objectConditions : TObjectConditions Index 32 Read FobjectConditions Write SetobjectConditions;
    Property transferOptions : TTransferOptions Index 40 Read FtransferOptions Write SettransferOptions;
  end;
  TTransferSpecClass = Class of TTransferSpec;
  
  { --------------------------------------------------------------------
    TGcsData
    --------------------------------------------------------------------}
  
  TGcsData = Class(TGoogleBaseObject)
  Private
    FbucketName : String;
  Protected
    //Property setters
    Procedure SetbucketName(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property bucketName : String Index 0 Read FbucketName Write SetbucketName;
  end;
  TGcsDataClass = Class of TGcsData;
  
  { --------------------------------------------------------------------
    TAwsS3Data
    --------------------------------------------------------------------}
  
  TAwsS3Data = Class(TGoogleBaseObject)
  Private
    FbucketName : String;
    FawsAccessKey : TAwsAccessKey;
  Protected
    //Property setters
    Procedure SetbucketName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetawsAccessKey(AIndex : Integer; const AValue : TAwsAccessKey); virtual;
  Public
  Published
    Property bucketName : String Index 0 Read FbucketName Write SetbucketName;
    Property awsAccessKey : TAwsAccessKey Index 8 Read FawsAccessKey Write SetawsAccessKey;
  end;
  TAwsS3DataClass = Class of TAwsS3Data;
  
  { --------------------------------------------------------------------
    TAwsAccessKey
    --------------------------------------------------------------------}
  
  TAwsAccessKey = Class(TGoogleBaseObject)
  Private
    FaccessKeyId : String;
    FsecretAccessKey : String;
  Protected
    //Property setters
    Procedure SetaccessKeyId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsecretAccessKey(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property accessKeyId : String Index 0 Read FaccessKeyId Write SetaccessKeyId;
    Property secretAccessKey : String Index 8 Read FsecretAccessKey Write SetsecretAccessKey;
  end;
  TAwsAccessKeyClass = Class of TAwsAccessKey;
  
  { --------------------------------------------------------------------
    THttpData
    --------------------------------------------------------------------}
  
  THttpData = Class(TGoogleBaseObject)
  Private
    FlistUrl : String;
  Protected
    //Property setters
    Procedure SetlistUrl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property listUrl : String Index 0 Read FlistUrl Write SetlistUrl;
  end;
  THttpDataClass = Class of THttpData;
  
  { --------------------------------------------------------------------
    TObjectConditions
    --------------------------------------------------------------------}
  
  TObjectConditions = Class(TGoogleBaseObject)
  Private
    FminTimeElapsedSinceLastModification : String;
    FmaxTimeElapsedSinceLastModification : String;
    FincludePrefixes : TStringArray;
    FexcludePrefixes : TStringArray;
  Protected
    //Property setters
    Procedure SetminTimeElapsedSinceLastModification(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmaxTimeElapsedSinceLastModification(AIndex : Integer; const AValue : String); virtual;
    Procedure SetincludePrefixes(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetexcludePrefixes(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property minTimeElapsedSinceLastModification : String Index 0 Read FminTimeElapsedSinceLastModification Write SetminTimeElapsedSinceLastModification;
    Property maxTimeElapsedSinceLastModification : String Index 8 Read FmaxTimeElapsedSinceLastModification Write SetmaxTimeElapsedSinceLastModification;
    Property includePrefixes : TStringArray Index 16 Read FincludePrefixes Write SetincludePrefixes;
    Property excludePrefixes : TStringArray Index 24 Read FexcludePrefixes Write SetexcludePrefixes;
  end;
  TObjectConditionsClass = Class of TObjectConditions;
  
  { --------------------------------------------------------------------
    TTransferOptions
    --------------------------------------------------------------------}
  
  TTransferOptions = Class(TGoogleBaseObject)
  Private
    FoverwriteObjectsAlreadyExistingInSink : boolean;
    FdeleteObjectsUniqueInSink : boolean;
    FdeleteObjectsFromSourceAfterTransfer : boolean;
  Protected
    //Property setters
    Procedure SetoverwriteObjectsAlreadyExistingInSink(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetdeleteObjectsUniqueInSink(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetdeleteObjectsFromSourceAfterTransfer(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property overwriteObjectsAlreadyExistingInSink : boolean Index 0 Read FoverwriteObjectsAlreadyExistingInSink Write SetoverwriteObjectsAlreadyExistingInSink;
    Property deleteObjectsUniqueInSink : boolean Index 8 Read FdeleteObjectsUniqueInSink Write SetdeleteObjectsUniqueInSink;
    Property deleteObjectsFromSourceAfterTransfer : boolean Index 16 Read FdeleteObjectsFromSourceAfterTransfer Write SetdeleteObjectsFromSourceAfterTransfer;
  end;
  TTransferOptionsClass = Class of TTransferOptions;
  
  { --------------------------------------------------------------------
    TSchedule
    --------------------------------------------------------------------}
  
  TSchedule = Class(TGoogleBaseObject)
  Private
    FscheduleStartDate : TDate;
    FscheduleEndDate : TDate;
    FstartTimeOfDay : TTimeOfDay;
  Protected
    //Property setters
    Procedure SetscheduleStartDate(AIndex : Integer; const AValue : TDate); virtual;
    Procedure SetscheduleEndDate(AIndex : Integer; const AValue : TDate); virtual;
    Procedure SetstartTimeOfDay(AIndex : Integer; const AValue : TTimeOfDay); virtual;
  Public
  Published
    Property scheduleStartDate : TDate Index 0 Read FscheduleStartDate Write SetscheduleStartDate;
    Property scheduleEndDate : TDate Index 8 Read FscheduleEndDate Write SetscheduleEndDate;
    Property startTimeOfDay : TTimeOfDay Index 16 Read FstartTimeOfDay Write SetstartTimeOfDay;
  end;
  TScheduleClass = Class of TSchedule;
  
  { --------------------------------------------------------------------
    TDate
    --------------------------------------------------------------------}
  
  TDate = Class(TGoogleBaseObject)
  Private
    Fyear : integer;
    Fmonth : integer;
    Fday : integer;
  Protected
    //Property setters
    Procedure Setyear(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setmonth(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setday(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property year : integer Index 0 Read Fyear Write Setyear;
    Property month : integer Index 8 Read Fmonth Write Setmonth;
    Property day : integer Index 16 Read Fday Write Setday;
  end;
  TDateClass = Class of TDate;
  
  { --------------------------------------------------------------------
    TTimeOfDay
    --------------------------------------------------------------------}
  
  TTimeOfDay = Class(TGoogleBaseObject)
  Private
    Fhours : integer;
    Fminutes : integer;
    Fseconds : integer;
    Fnanos : integer;
  Protected
    //Property setters
    Procedure Sethours(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setminutes(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setseconds(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setnanos(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property hours : integer Index 0 Read Fhours Write Sethours;
    Property minutes : integer Index 8 Read Fminutes Write Setminutes;
    Property seconds : integer Index 16 Read Fseconds Write Setseconds;
    Property nanos : integer Index 24 Read Fnanos Write Setnanos;
  end;
  TTimeOfDayClass = Class of TTimeOfDay;
  
  { --------------------------------------------------------------------
    TUpdateTransferJobRequest
    --------------------------------------------------------------------}
  
  TUpdateTransferJobRequest = Class(TGoogleBaseObject)
  Private
    FprojectId : String;
    FtransferJob : TTransferJob;
    FupdateTransferJobFieldMask : String;
  Protected
    //Property setters
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
    Procedure SettransferJob(AIndex : Integer; const AValue : TTransferJob); virtual;
    Procedure SetupdateTransferJobFieldMask(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property projectId : String Index 0 Read FprojectId Write SetprojectId;
    Property transferJob : TTransferJob Index 8 Read FtransferJob Write SettransferJob;
    Property updateTransferJobFieldMask : String Index 16 Read FupdateTransferJobFieldMask Write SetupdateTransferJobFieldMask;
  end;
  TUpdateTransferJobRequestClass = Class of TUpdateTransferJobRequest;
  
  { --------------------------------------------------------------------
    TListTransferJobsResponse
    --------------------------------------------------------------------}
  
  TListTransferJobsResponse = Class(TGoogleBaseObject)
  Private
    FtransferJobs : TListTransferJobsResponseTypetransferJobsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SettransferJobs(AIndex : Integer; const AValue : TListTransferJobsResponseTypetransferJobsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property transferJobs : TListTransferJobsResponseTypetransferJobsArray Index 0 Read FtransferJobs Write SettransferJobs;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListTransferJobsResponseClass = Class of TListTransferJobsResponse;
  
  { --------------------------------------------------------------------
    TPauseTransferOperationRequest
    --------------------------------------------------------------------}
  
  TPauseTransferOperationRequest = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPauseTransferOperationRequestClass = Class of TPauseTransferOperationRequest;
  
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
    TResumeTransferOperationRequest
    --------------------------------------------------------------------}
  
  TResumeTransferOperationRequest = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TResumeTransferOperationRequestClass = Class of TResumeTransferOperationRequest;
  
  { --------------------------------------------------------------------
    TOperationTypemetadata
    --------------------------------------------------------------------}
  
  TOperationTypemetadata = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TOperationTypemetadataClass = Class of TOperationTypemetadata;
  
  { --------------------------------------------------------------------
    TOperationTyperesponse
    --------------------------------------------------------------------}
  
  TOperationTyperesponse = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TOperationTyperesponseClass = Class of TOperationTyperesponse;
  
  { --------------------------------------------------------------------
    TOperation
    --------------------------------------------------------------------}
  
  TOperation = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fmetadata : TOperationTypemetadata;
    Fdone : boolean;
    Ferror : TStatus;
    Fresponse : TOperationTyperesponse;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmetadata(AIndex : Integer; const AValue : TOperationTypemetadata); virtual;
    Procedure Setdone(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Seterror(AIndex : Integer; const AValue : TStatus); virtual;
    Procedure Setresponse(AIndex : Integer; const AValue : TOperationTyperesponse); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property metadata : TOperationTypemetadata Index 8 Read Fmetadata Write Setmetadata;
    Property done : boolean Index 16 Read Fdone Write Setdone;
    Property error : TStatus Index 24 Read Ferror Write Seterror;
    Property response : TOperationTyperesponse Index 32 Read Fresponse Write Setresponse;
  end;
  TOperationClass = Class of TOperation;
  
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
    TListOperationsResponse
    --------------------------------------------------------------------}
  
  TListOperationsResponse = Class(TGoogleBaseObject)
  Private
    Foperations : TListOperationsResponseTypeoperationsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setoperations(AIndex : Integer; const AValue : TListOperationsResponseTypeoperationsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property operations : TListOperationsResponseTypeoperationsArray Index 0 Read Foperations Write Setoperations;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListOperationsResponseClass = Class of TListOperationsResponse;
  
  { --------------------------------------------------------------------
    TTransferOperation
    --------------------------------------------------------------------}
  
  TTransferOperation = Class(TGoogleBaseObject)
  Private
    Fname : String;
    FprojectId : String;
    FtransferSpec : TTransferSpec;
    FstartTime : String;
    FendTime : String;
    Fstatus : String;
    Fcounters : TTransferCounters;
    FerrorBreakdowns : TTransferOperationTypeerrorBreakdownsArray;
    FtransferJobName : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
    Procedure SettransferSpec(AIndex : Integer; const AValue : TTransferSpec); virtual;
    Procedure SetstartTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetendTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcounters(AIndex : Integer; const AValue : TTransferCounters); virtual;
    Procedure SeterrorBreakdowns(AIndex : Integer; const AValue : TTransferOperationTypeerrorBreakdownsArray); virtual;
    Procedure SettransferJobName(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property projectId : String Index 8 Read FprojectId Write SetprojectId;
    Property transferSpec : TTransferSpec Index 16 Read FtransferSpec Write SettransferSpec;
    Property startTime : String Index 24 Read FstartTime Write SetstartTime;
    Property endTime : String Index 32 Read FendTime Write SetendTime;
    Property status : String Index 40 Read Fstatus Write Setstatus;
    Property counters : TTransferCounters Index 48 Read Fcounters Write Setcounters;
    Property errorBreakdowns : TTransferOperationTypeerrorBreakdownsArray Index 56 Read FerrorBreakdowns Write SeterrorBreakdowns;
    Property transferJobName : String Index 64 Read FtransferJobName Write SettransferJobName;
  end;
  TTransferOperationClass = Class of TTransferOperation;
  
  { --------------------------------------------------------------------
    TTransferCounters
    --------------------------------------------------------------------}
  
  TTransferCounters = Class(TGoogleBaseObject)
  Private
    FobjectsFoundFromSource : String;
    FbytesFoundFromSource : String;
    FobjectsFoundOnlyFromSink : String;
    FbytesFoundOnlyFromSink : String;
    FobjectsFromSourceSkippedBySync : String;
    FbytesFromSourceSkippedBySync : String;
    FobjectsCopiedToSink : String;
    FbytesCopiedToSink : String;
    FobjectsDeletedFromSource : String;
    FbytesDeletedFromSource : String;
    FobjectsDeletedFromSink : String;
    FbytesDeletedFromSink : String;
    FobjectsFromSourceFailed : String;
    FbytesFromSourceFailed : String;
    FobjectsFailedToDeleteFromSink : String;
    FbytesFailedToDeleteFromSink : String;
  Protected
    //Property setters
    Procedure SetobjectsFoundFromSource(AIndex : Integer; const AValue : String); virtual;
    Procedure SetbytesFoundFromSource(AIndex : Integer; const AValue : String); virtual;
    Procedure SetobjectsFoundOnlyFromSink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetbytesFoundOnlyFromSink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetobjectsFromSourceSkippedBySync(AIndex : Integer; const AValue : String); virtual;
    Procedure SetbytesFromSourceSkippedBySync(AIndex : Integer; const AValue : String); virtual;
    Procedure SetobjectsCopiedToSink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetbytesCopiedToSink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetobjectsDeletedFromSource(AIndex : Integer; const AValue : String); virtual;
    Procedure SetbytesDeletedFromSource(AIndex : Integer; const AValue : String); virtual;
    Procedure SetobjectsDeletedFromSink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetbytesDeletedFromSink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetobjectsFromSourceFailed(AIndex : Integer; const AValue : String); virtual;
    Procedure SetbytesFromSourceFailed(AIndex : Integer; const AValue : String); virtual;
    Procedure SetobjectsFailedToDeleteFromSink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetbytesFailedToDeleteFromSink(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property objectsFoundFromSource : String Index 0 Read FobjectsFoundFromSource Write SetobjectsFoundFromSource;
    Property bytesFoundFromSource : String Index 8 Read FbytesFoundFromSource Write SetbytesFoundFromSource;
    Property objectsFoundOnlyFromSink : String Index 16 Read FobjectsFoundOnlyFromSink Write SetobjectsFoundOnlyFromSink;
    Property bytesFoundOnlyFromSink : String Index 24 Read FbytesFoundOnlyFromSink Write SetbytesFoundOnlyFromSink;
    Property objectsFromSourceSkippedBySync : String Index 32 Read FobjectsFromSourceSkippedBySync Write SetobjectsFromSourceSkippedBySync;
    Property bytesFromSourceSkippedBySync : String Index 40 Read FbytesFromSourceSkippedBySync Write SetbytesFromSourceSkippedBySync;
    Property objectsCopiedToSink : String Index 48 Read FobjectsCopiedToSink Write SetobjectsCopiedToSink;
    Property bytesCopiedToSink : String Index 56 Read FbytesCopiedToSink Write SetbytesCopiedToSink;
    Property objectsDeletedFromSource : String Index 64 Read FobjectsDeletedFromSource Write SetobjectsDeletedFromSource;
    Property bytesDeletedFromSource : String Index 72 Read FbytesDeletedFromSource Write SetbytesDeletedFromSource;
    Property objectsDeletedFromSink : String Index 80 Read FobjectsDeletedFromSink Write SetobjectsDeletedFromSink;
    Property bytesDeletedFromSink : String Index 88 Read FbytesDeletedFromSink Write SetbytesDeletedFromSink;
    Property objectsFromSourceFailed : String Index 96 Read FobjectsFromSourceFailed Write SetobjectsFromSourceFailed;
    Property bytesFromSourceFailed : String Index 104 Read FbytesFromSourceFailed Write SetbytesFromSourceFailed;
    Property objectsFailedToDeleteFromSink : String Index 112 Read FobjectsFailedToDeleteFromSink Write SetobjectsFailedToDeleteFromSink;
    Property bytesFailedToDeleteFromSink : String Index 120 Read FbytesFailedToDeleteFromSink Write SetbytesFailedToDeleteFromSink;
  end;
  TTransferCountersClass = Class of TTransferCounters;
  
  { --------------------------------------------------------------------
    TErrorSummary
    --------------------------------------------------------------------}
  
  TErrorSummary = Class(TGoogleBaseObject)
  Private
    FerrorCode : String;
    FerrorCount : String;
    FerrorLogEntries : TErrorSummaryTypeerrorLogEntriesArray;
  Protected
    //Property setters
    Procedure SeterrorCode(AIndex : Integer; const AValue : String); virtual;
    Procedure SeterrorCount(AIndex : Integer; const AValue : String); virtual;
    Procedure SeterrorLogEntries(AIndex : Integer; const AValue : TErrorSummaryTypeerrorLogEntriesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property errorCode : String Index 0 Read FerrorCode Write SeterrorCode;
    Property errorCount : String Index 8 Read FerrorCount Write SeterrorCount;
    Property errorLogEntries : TErrorSummaryTypeerrorLogEntriesArray Index 16 Read FerrorLogEntries Write SeterrorLogEntries;
  end;
  TErrorSummaryClass = Class of TErrorSummary;
  
  { --------------------------------------------------------------------
    TErrorLogEntry
    --------------------------------------------------------------------}
  
  TErrorLogEntry = Class(TGoogleBaseObject)
  Private
    Furl : String;
    FerrorDetails : TStringArray;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
    Procedure SeterrorDetails(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property url : String Index 0 Read Furl Write Seturl;
    Property errorDetails : TStringArray Index 8 Read FerrorDetails Write SeterrorDetails;
  end;
  TErrorLogEntryClass = Class of TErrorLogEntry;
  
  { --------------------------------------------------------------------
    TGoogleServiceAccountsResource
    --------------------------------------------------------------------}
  
  TGoogleServiceAccountsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(projectId: string) : TGoogleServiceAccount;
  end;
  
  
  { --------------------------------------------------------------------
    TV1Resource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TV1Resource, method GetGoogleServiceAccount
  
  TV1GetGoogleServiceAccountOptions = Record
    projectId : String;
  end;
  
  TV1Resource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function GetGoogleServiceAccount(AQuery : string  = '') : TGoogleServiceAccount;
    Function GetGoogleServiceAccount(AQuery : TV1getGoogleServiceAccountOptions) : TGoogleServiceAccount;
  end;
  
  
  { --------------------------------------------------------------------
    TTransferJobsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTransferJobsResource, method Get
  
  TTransferJobsGetOptions = Record
    projectId : String;
  end;
  
  
  //Optional query Options for TTransferJobsResource, method List
  
  TTransferJobsListOptions = Record
    filter : String;
    pageSize : integer;
    pageToken : String;
  end;
  
  TTransferJobsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(aTransferJob : TTransferJob) : TTransferJob;overload;
    Function Patch(jobName: string; aUpdateTransferJobRequest : TUpdateTransferJobRequest) : TTransferJob;
    Function Get(jobName: string; AQuery : string  = '') : TTransferJob;
    Function Get(jobName: string; AQuery : TTransferJobsgetOptions) : TTransferJob;
    Function List(AQuery : string  = '') : TListTransferJobsResponse;
    Function List(AQuery : TTransferJobslistOptions) : TListTransferJobsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TTransferOperationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTransferOperationsResource, method List
  
  TTransferOperationsListOptions = Record
    filter : String;
    pageSize : integer;
    pageToken : String;
  end;
  
  TTransferOperationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Pause(_name: string; aPauseTransferOperationRequest : TPauseTransferOperationRequest) : TEmpty;
    Function Resume(_name: string; aResumeTransferOperationRequest : TResumeTransferOperationRequest) : TEmpty;
    Function Get(_name: string) : TOperation;
    Function List(_name: string; AQuery : string  = '') : TListOperationsResponse;
    Function List(_name: string; AQuery : TTransferOperationslistOptions) : TListOperationsResponse;
    Function Cancel(_name: string) : TEmpty;
    Function Delete(_name: string) : TEmpty;
  end;
  
  
  { --------------------------------------------------------------------
    TStoragetransferAPI
    --------------------------------------------------------------------}
  
  TStoragetransferAPI = Class(TGoogleAPI)
  Private
    FGoogleServiceAccountsInstance : TGoogleServiceAccountsResource;
    FV1Instance : TV1Resource;
    FTransferJobsInstance : TTransferJobsResource;
    FTransferOperationsInstance : TTransferOperationsResource;
    Function GetGoogleServiceAccountsInstance : TGoogleServiceAccountsResource;virtual;
    Function GetV1Instance : TV1Resource;virtual;
    Function GetTransferJobsInstance : TTransferJobsResource;virtual;
    Function GetTransferOperationsInstance : TTransferOperationsResource;virtual;
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
    Function CreateGoogleServiceAccountsResource(AOwner : TComponent) : TGoogleServiceAccountsResource;virtual;overload;
    Function CreateGoogleServiceAccountsResource : TGoogleServiceAccountsResource;virtual;overload;
    Function CreateV1Resource(AOwner : TComponent) : TV1Resource;virtual;overload;
    Function CreateV1Resource : TV1Resource;virtual;overload;
    Function CreateTransferJobsResource(AOwner : TComponent) : TTransferJobsResource;virtual;overload;
    Function CreateTransferJobsResource : TTransferJobsResource;virtual;overload;
    Function CreateTransferOperationsResource(AOwner : TComponent) : TTransferOperationsResource;virtual;overload;
    Function CreateTransferOperationsResource : TTransferOperationsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property GoogleServiceAccountsResource : TGoogleServiceAccountsResource Read GetGoogleServiceAccountsInstance;
    Property V1Resource : TV1Resource Read GetV1Instance;
    Property TransferJobsResource : TTransferJobsResource Read GetTransferJobsInstance;
    Property TransferOperationsResource : TTransferOperationsResource Read GetTransferOperationsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TGoogleServiceAccount
  --------------------------------------------------------------------}


Procedure TGoogleServiceAccount.SetaccountEmail(AIndex : Integer; const AValue : String); 

begin
  If (FaccountEmail=AValue) then exit;
  FaccountEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTransferJob
  --------------------------------------------------------------------}


Procedure TTransferJob.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferJob.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferJob.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferJob.SettransferSpec(AIndex : Integer; const AValue : TTransferSpec); 

begin
  If (FtransferSpec=AValue) then exit;
  FtransferSpec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferJob.Setschedule(AIndex : Integer; const AValue : TSchedule); 

begin
  If (Fschedule=AValue) then exit;
  Fschedule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferJob.Setstatus(AIndex : Integer; const AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferJob.SetcreationTime(AIndex : Integer; const AValue : String); 

begin
  If (FcreationTime=AValue) then exit;
  FcreationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferJob.SetlastModificationTime(AIndex : Integer; const AValue : String); 

begin
  If (FlastModificationTime=AValue) then exit;
  FlastModificationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferJob.SetdeletionTime(AIndex : Integer; const AValue : String); 

begin
  If (FdeletionTime=AValue) then exit;
  FdeletionTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTransferSpec
  --------------------------------------------------------------------}


Procedure TTransferSpec.SetgcsDataSource(AIndex : Integer; const AValue : TGcsData); 

begin
  If (FgcsDataSource=AValue) then exit;
  FgcsDataSource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferSpec.SetawsS3DataSource(AIndex : Integer; const AValue : TAwsS3Data); 

begin
  If (FawsS3DataSource=AValue) then exit;
  FawsS3DataSource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferSpec.SethttpDataSource(AIndex : Integer; const AValue : THttpData); 

begin
  If (FhttpDataSource=AValue) then exit;
  FhttpDataSource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferSpec.SetgcsDataSink(AIndex : Integer; const AValue : TGcsData); 

begin
  If (FgcsDataSink=AValue) then exit;
  FgcsDataSink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferSpec.SetobjectConditions(AIndex : Integer; const AValue : TObjectConditions); 

begin
  If (FobjectConditions=AValue) then exit;
  FobjectConditions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferSpec.SettransferOptions(AIndex : Integer; const AValue : TTransferOptions); 

begin
  If (FtransferOptions=AValue) then exit;
  FtransferOptions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGcsData
  --------------------------------------------------------------------}


Procedure TGcsData.SetbucketName(AIndex : Integer; const AValue : String); 

begin
  If (FbucketName=AValue) then exit;
  FbucketName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAwsS3Data
  --------------------------------------------------------------------}


Procedure TAwsS3Data.SetbucketName(AIndex : Integer; const AValue : String); 

begin
  If (FbucketName=AValue) then exit;
  FbucketName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAwsS3Data.SetawsAccessKey(AIndex : Integer; const AValue : TAwsAccessKey); 

begin
  If (FawsAccessKey=AValue) then exit;
  FawsAccessKey:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAwsAccessKey
  --------------------------------------------------------------------}


Procedure TAwsAccessKey.SetaccessKeyId(AIndex : Integer; const AValue : String); 

begin
  If (FaccessKeyId=AValue) then exit;
  FaccessKeyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAwsAccessKey.SetsecretAccessKey(AIndex : Integer; const AValue : String); 

begin
  If (FsecretAccessKey=AValue) then exit;
  FsecretAccessKey:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  THttpData
  --------------------------------------------------------------------}


Procedure THttpData.SetlistUrl(AIndex : Integer; const AValue : String); 

begin
  If (FlistUrl=AValue) then exit;
  FlistUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TObjectConditions
  --------------------------------------------------------------------}


Procedure TObjectConditions.SetminTimeElapsedSinceLastModification(AIndex : Integer; const AValue : String); 

begin
  If (FminTimeElapsedSinceLastModification=AValue) then exit;
  FminTimeElapsedSinceLastModification:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectConditions.SetmaxTimeElapsedSinceLastModification(AIndex : Integer; const AValue : String); 

begin
  If (FmaxTimeElapsedSinceLastModification=AValue) then exit;
  FmaxTimeElapsedSinceLastModification:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectConditions.SetincludePrefixes(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FincludePrefixes=AValue) then exit;
  FincludePrefixes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectConditions.SetexcludePrefixes(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FexcludePrefixes=AValue) then exit;
  FexcludePrefixes:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TObjectConditions.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'includeprefixes' : SetLength(FincludePrefixes,ALength);
  'excludeprefixes' : SetLength(FexcludePrefixes,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTransferOptions
  --------------------------------------------------------------------}


Procedure TTransferOptions.SetoverwriteObjectsAlreadyExistingInSink(AIndex : Integer; const AValue : boolean); 

begin
  If (FoverwriteObjectsAlreadyExistingInSink=AValue) then exit;
  FoverwriteObjectsAlreadyExistingInSink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferOptions.SetdeleteObjectsUniqueInSink(AIndex : Integer; const AValue : boolean); 

begin
  If (FdeleteObjectsUniqueInSink=AValue) then exit;
  FdeleteObjectsUniqueInSink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferOptions.SetdeleteObjectsFromSourceAfterTransfer(AIndex : Integer; const AValue : boolean); 

begin
  If (FdeleteObjectsFromSourceAfterTransfer=AValue) then exit;
  FdeleteObjectsFromSourceAfterTransfer:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSchedule
  --------------------------------------------------------------------}


Procedure TSchedule.SetscheduleStartDate(AIndex : Integer; const AValue : TDate); 

begin
  If (FscheduleStartDate=AValue) then exit;
  FscheduleStartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSchedule.SetscheduleEndDate(AIndex : Integer; const AValue : TDate); 

begin
  If (FscheduleEndDate=AValue) then exit;
  FscheduleEndDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSchedule.SetstartTimeOfDay(AIndex : Integer; const AValue : TTimeOfDay); 

begin
  If (FstartTimeOfDay=AValue) then exit;
  FstartTimeOfDay:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDate
  --------------------------------------------------------------------}


Procedure TDate.Setyear(AIndex : Integer; const AValue : integer); 

begin
  If (Fyear=AValue) then exit;
  Fyear:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDate.Setmonth(AIndex : Integer; const AValue : integer); 

begin
  If (Fmonth=AValue) then exit;
  Fmonth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDate.Setday(AIndex : Integer; const AValue : integer); 

begin
  If (Fday=AValue) then exit;
  Fday:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTimeOfDay
  --------------------------------------------------------------------}


Procedure TTimeOfDay.Sethours(AIndex : Integer; const AValue : integer); 

begin
  If (Fhours=AValue) then exit;
  Fhours:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeOfDay.Setminutes(AIndex : Integer; const AValue : integer); 

begin
  If (Fminutes=AValue) then exit;
  Fminutes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeOfDay.Setseconds(AIndex : Integer; const AValue : integer); 

begin
  If (Fseconds=AValue) then exit;
  Fseconds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeOfDay.Setnanos(AIndex : Integer; const AValue : integer); 

begin
  If (Fnanos=AValue) then exit;
  Fnanos:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUpdateTransferJobRequest
  --------------------------------------------------------------------}


Procedure TUpdateTransferJobRequest.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateTransferJobRequest.SettransferJob(AIndex : Integer; const AValue : TTransferJob); 

begin
  If (FtransferJob=AValue) then exit;
  FtransferJob:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateTransferJobRequest.SetupdateTransferJobFieldMask(AIndex : Integer; const AValue : String); 

begin
  If (FupdateTransferJobFieldMask=AValue) then exit;
  FupdateTransferJobFieldMask:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListTransferJobsResponse
  --------------------------------------------------------------------}


Procedure TListTransferJobsResponse.SettransferJobs(AIndex : Integer; const AValue : TListTransferJobsResponseTypetransferJobsArray); 

begin
  If (FtransferJobs=AValue) then exit;
  FtransferJobs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListTransferJobsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListTransferJobsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'transferjobs' : SetLength(FtransferJobs,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPauseTransferOperationRequest
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TEmpty
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TResumeTransferOperationRequest
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TOperationTypemetadata
  --------------------------------------------------------------------}


Class Function TOperationTypemetadata.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TOperationTyperesponse
  --------------------------------------------------------------------}


Class Function TOperationTyperesponse.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TOperation
  --------------------------------------------------------------------}


Procedure TOperation.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setmetadata(AIndex : Integer; const AValue : TOperationTypemetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setdone(AIndex : Integer; const AValue : boolean); 

begin
  If (Fdone=AValue) then exit;
  Fdone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Seterror(AIndex : Integer; const AValue : TStatus); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setresponse(AIndex : Integer; const AValue : TOperationTyperesponse); 

begin
  If (Fresponse=AValue) then exit;
  Fresponse:=AValue;
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
  TListOperationsResponse
  --------------------------------------------------------------------}


Procedure TListOperationsResponse.Setoperations(AIndex : Integer; const AValue : TListOperationsResponseTypeoperationsArray); 

begin
  If (Foperations=AValue) then exit;
  Foperations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListOperationsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListOperationsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'operations' : SetLength(Foperations,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTransferOperation
  --------------------------------------------------------------------}


Procedure TTransferOperation.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferOperation.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferOperation.SettransferSpec(AIndex : Integer; const AValue : TTransferSpec); 

begin
  If (FtransferSpec=AValue) then exit;
  FtransferSpec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferOperation.SetstartTime(AIndex : Integer; const AValue : String); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferOperation.SetendTime(AIndex : Integer; const AValue : String); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferOperation.Setstatus(AIndex : Integer; const AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferOperation.Setcounters(AIndex : Integer; const AValue : TTransferCounters); 

begin
  If (Fcounters=AValue) then exit;
  Fcounters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferOperation.SeterrorBreakdowns(AIndex : Integer; const AValue : TTransferOperationTypeerrorBreakdownsArray); 

begin
  If (FerrorBreakdowns=AValue) then exit;
  FerrorBreakdowns:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferOperation.SettransferJobName(AIndex : Integer; const AValue : String); 

begin
  If (FtransferJobName=AValue) then exit;
  FtransferJobName:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTransferOperation.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errorbreakdowns' : SetLength(FerrorBreakdowns,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTransferCounters
  --------------------------------------------------------------------}


Procedure TTransferCounters.SetobjectsFoundFromSource(AIndex : Integer; const AValue : String); 

begin
  If (FobjectsFoundFromSource=AValue) then exit;
  FobjectsFoundFromSource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferCounters.SetbytesFoundFromSource(AIndex : Integer; const AValue : String); 

begin
  If (FbytesFoundFromSource=AValue) then exit;
  FbytesFoundFromSource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferCounters.SetobjectsFoundOnlyFromSink(AIndex : Integer; const AValue : String); 

begin
  If (FobjectsFoundOnlyFromSink=AValue) then exit;
  FobjectsFoundOnlyFromSink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferCounters.SetbytesFoundOnlyFromSink(AIndex : Integer; const AValue : String); 

begin
  If (FbytesFoundOnlyFromSink=AValue) then exit;
  FbytesFoundOnlyFromSink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferCounters.SetobjectsFromSourceSkippedBySync(AIndex : Integer; const AValue : String); 

begin
  If (FobjectsFromSourceSkippedBySync=AValue) then exit;
  FobjectsFromSourceSkippedBySync:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferCounters.SetbytesFromSourceSkippedBySync(AIndex : Integer; const AValue : String); 

begin
  If (FbytesFromSourceSkippedBySync=AValue) then exit;
  FbytesFromSourceSkippedBySync:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferCounters.SetobjectsCopiedToSink(AIndex : Integer; const AValue : String); 

begin
  If (FobjectsCopiedToSink=AValue) then exit;
  FobjectsCopiedToSink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferCounters.SetbytesCopiedToSink(AIndex : Integer; const AValue : String); 

begin
  If (FbytesCopiedToSink=AValue) then exit;
  FbytesCopiedToSink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferCounters.SetobjectsDeletedFromSource(AIndex : Integer; const AValue : String); 

begin
  If (FobjectsDeletedFromSource=AValue) then exit;
  FobjectsDeletedFromSource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferCounters.SetbytesDeletedFromSource(AIndex : Integer; const AValue : String); 

begin
  If (FbytesDeletedFromSource=AValue) then exit;
  FbytesDeletedFromSource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferCounters.SetobjectsDeletedFromSink(AIndex : Integer; const AValue : String); 

begin
  If (FobjectsDeletedFromSink=AValue) then exit;
  FobjectsDeletedFromSink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferCounters.SetbytesDeletedFromSink(AIndex : Integer; const AValue : String); 

begin
  If (FbytesDeletedFromSink=AValue) then exit;
  FbytesDeletedFromSink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferCounters.SetobjectsFromSourceFailed(AIndex : Integer; const AValue : String); 

begin
  If (FobjectsFromSourceFailed=AValue) then exit;
  FobjectsFromSourceFailed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferCounters.SetbytesFromSourceFailed(AIndex : Integer; const AValue : String); 

begin
  If (FbytesFromSourceFailed=AValue) then exit;
  FbytesFromSourceFailed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferCounters.SetobjectsFailedToDeleteFromSink(AIndex : Integer; const AValue : String); 

begin
  If (FobjectsFailedToDeleteFromSink=AValue) then exit;
  FobjectsFailedToDeleteFromSink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTransferCounters.SetbytesFailedToDeleteFromSink(AIndex : Integer; const AValue : String); 

begin
  If (FbytesFailedToDeleteFromSink=AValue) then exit;
  FbytesFailedToDeleteFromSink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TErrorSummary
  --------------------------------------------------------------------}


Procedure TErrorSummary.SeterrorCode(AIndex : Integer; const AValue : String); 

begin
  If (FerrorCode=AValue) then exit;
  FerrorCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorSummary.SeterrorCount(AIndex : Integer; const AValue : String); 

begin
  If (FerrorCount=AValue) then exit;
  FerrorCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorSummary.SeterrorLogEntries(AIndex : Integer; const AValue : TErrorSummaryTypeerrorLogEntriesArray); 

begin
  If (FerrorLogEntries=AValue) then exit;
  FerrorLogEntries:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TErrorSummary.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errorlogentries' : SetLength(FerrorLogEntries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TErrorLogEntry
  --------------------------------------------------------------------}


Procedure TErrorLogEntry.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorLogEntry.SeterrorDetails(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FerrorDetails=AValue) then exit;
  FerrorDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TErrorLogEntry.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errordetails' : SetLength(FerrorDetails,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGoogleServiceAccountsResource
  --------------------------------------------------------------------}


Class Function TGoogleServiceAccountsResource.ResourceName : String;

begin
  Result:='googleServiceAccounts';
end;

Class Function TGoogleServiceAccountsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TstoragetransferAPI;
end;

Function TGoogleServiceAccountsResource.Get(projectId: string) : TGoogleServiceAccount;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/googleServiceAccounts/{projectId}';
  _Methodid   = 'storagetransfer.googleServiceAccounts.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TGoogleServiceAccount) as TGoogleServiceAccount;
end;



{ --------------------------------------------------------------------
  TV1Resource
  --------------------------------------------------------------------}


Class Function TV1Resource.ResourceName : String;

begin
  Result:='v1';
end;

Class Function TV1Resource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TstoragetransferAPI;
end;

Function TV1Resource.GetGoogleServiceAccount(AQuery : string = '') : TGoogleServiceAccount;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1:getGoogleServiceAccount';
  _Methodid   = 'storagetransfer.getGoogleServiceAccount';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TGoogleServiceAccount) as TGoogleServiceAccount;
end;


Function TV1Resource.GetGoogleServiceAccount(AQuery : TV1getGoogleServiceAccountOptions) : TGoogleServiceAccount;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'projectId',AQuery.projectId);
  Result:=GetGoogleServiceAccount(_Q);
end;



{ --------------------------------------------------------------------
  TTransferJobsResource
  --------------------------------------------------------------------}


Class Function TTransferJobsResource.ResourceName : String;

begin
  Result:='transferJobs';
end;

Class Function TTransferJobsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TstoragetransferAPI;
end;

Function TTransferJobsResource.Create(aTransferJob : TTransferJob) : TTransferJob;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/transferJobs';
  _Methodid   = 'storagetransfer.transferJobs.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aTransferJob,TTransferJob) as TTransferJob;
end;

Function TTransferJobsResource.Patch(jobName: string; aUpdateTransferJobRequest : TUpdateTransferJobRequest) : TTransferJob;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'v1/{+jobName}';
  _Methodid   = 'storagetransfer.transferJobs.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobName',jobName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aUpdateTransferJobRequest,TTransferJob) as TTransferJob;
end;

Function TTransferJobsResource.Get(jobName: string; AQuery : string = '') : TTransferJob;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+jobName}';
  _Methodid   = 'storagetransfer.transferJobs.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobName',jobName]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTransferJob) as TTransferJob;
end;


Function TTransferJobsResource.Get(jobName: string; AQuery : TTransferJobsgetOptions) : TTransferJob;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'projectId',AQuery.projectId);
  Result:=Get(jobName,_Q);
end;

Function TTransferJobsResource.List(AQuery : string = '') : TListTransferJobsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/transferJobs';
  _Methodid   = 'storagetransfer.transferJobs.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TListTransferJobsResponse) as TListTransferJobsResponse;
end;


Function TTransferJobsResource.List(AQuery : TTransferJobslistOptions) : TListTransferJobsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TTransferOperationsResource
  --------------------------------------------------------------------}


Class Function TTransferOperationsResource.ResourceName : String;

begin
  Result:='transferOperations';
end;

Class Function TTransferOperationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TstoragetransferAPI;
end;

Function TTransferOperationsResource.Pause(_name: string; aPauseTransferOperationRequest : TPauseTransferOperationRequest) : TEmpty;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+name}:pause';
  _Methodid   = 'storagetransfer.transferOperations.pause';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aPauseTransferOperationRequest,TEmpty) as TEmpty;
end;

Function TTransferOperationsResource.Resume(_name: string; aResumeTransferOperationRequest : TResumeTransferOperationRequest) : TEmpty;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+name}:resume';
  _Methodid   = 'storagetransfer.transferOperations.resume';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aResumeTransferOperationRequest,TEmpty) as TEmpty;
end;

Function TTransferOperationsResource.Get(_name: string) : TOperation;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+name}';
  _Methodid   = 'storagetransfer.transferOperations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TTransferOperationsResource.List(_name: string; AQuery : string = '') : TListOperationsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+name}';
  _Methodid   = 'storagetransfer.transferOperations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListOperationsResponse) as TListOperationsResponse;
end;


Function TTransferOperationsResource.List(_name: string; AQuery : TTransferOperationslistOptions) : TListOperationsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_name,_Q);
end;

Function TTransferOperationsResource.Cancel(_name: string) : TEmpty;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+name}:cancel';
  _Methodid   = 'storagetransfer.transferOperations.cancel';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;

Function TTransferOperationsResource.Delete(_name: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/{+name}';
  _Methodid   = 'storagetransfer.transferOperations.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;



{ --------------------------------------------------------------------
  TStoragetransferAPI
  --------------------------------------------------------------------}

Class Function TStoragetransferAPI.APIName : String;

begin
  Result:='storagetransfer';
end;

Class Function TStoragetransferAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TStoragetransferAPI.APIRevision : String;

begin
  Result:='20150811';
end;

Class Function TStoragetransferAPI.APIID : String;

begin
  Result:='storagetransfer:v1';
end;

Class Function TStoragetransferAPI.APITitle : String;

begin
  Result:='Google Storage Transfer API';
end;

Class Function TStoragetransferAPI.APIDescription : String;

begin
  Result:='Transfers data from external data sources to a Google Cloud Storage bucket or between Google Cloud Storage buckets.';
end;

Class Function TStoragetransferAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TStoragetransferAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TStoragetransferAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TStoragetransferAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TStoragetransferAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/storage/transfer';
end;

Class Function TStoragetransferAPI.APIrootUrl : string;

begin
  Result:='https://storagetransfer.googleapis.com/';
end;

Class Function TStoragetransferAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TStoragetransferAPI.APIbaseURL : String;

begin
  Result:='https://storagetransfer.googleapis.com/';
end;

Class Function TStoragetransferAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TStoragetransferAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TStoragetransferAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TStoragetransferAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  
end;

Class Function TStoragetransferAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TStoragetransferAPI.RegisterAPIResources;

begin
  TGoogleServiceAccount.RegisterObject;
  TTransferJob.RegisterObject;
  TTransferSpec.RegisterObject;
  TGcsData.RegisterObject;
  TAwsS3Data.RegisterObject;
  TAwsAccessKey.RegisterObject;
  THttpData.RegisterObject;
  TObjectConditions.RegisterObject;
  TTransferOptions.RegisterObject;
  TSchedule.RegisterObject;
  TDate.RegisterObject;
  TTimeOfDay.RegisterObject;
  TUpdateTransferJobRequest.RegisterObject;
  TListTransferJobsResponse.RegisterObject;
  TPauseTransferOperationRequest.RegisterObject;
  TEmpty.RegisterObject;
  TResumeTransferOperationRequest.RegisterObject;
  TOperationTypemetadata.RegisterObject;
  TOperationTyperesponse.RegisterObject;
  TOperation.RegisterObject;
  TStatusTypedetailsItem.RegisterObject;
  TStatus.RegisterObject;
  TListOperationsResponse.RegisterObject;
  TTransferOperation.RegisterObject;
  TTransferCounters.RegisterObject;
  TErrorSummary.RegisterObject;
  TErrorLogEntry.RegisterObject;
end;


Function TStoragetransferAPI.GetGoogleServiceAccountsInstance : TGoogleServiceAccountsResource;

begin
  if (FGoogleServiceAccountsInstance=Nil) then
    FGoogleServiceAccountsInstance:=CreateGoogleServiceAccountsResource;
  Result:=FGoogleServiceAccountsInstance;
end;

Function TStoragetransferAPI.CreateGoogleServiceAccountsResource : TGoogleServiceAccountsResource;

begin
  Result:=CreateGoogleServiceAccountsResource(Self);
end;


Function TStoragetransferAPI.CreateGoogleServiceAccountsResource(AOwner : TComponent) : TGoogleServiceAccountsResource;

begin
  Result:=TGoogleServiceAccountsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TStoragetransferAPI.GetV1Instance : TV1Resource;

begin
  if (FV1Instance=Nil) then
    FV1Instance:=CreateV1Resource;
  Result:=FV1Instance;
end;

Function TStoragetransferAPI.CreateV1Resource : TV1Resource;

begin
  Result:=CreateV1Resource(Self);
end;


Function TStoragetransferAPI.CreateV1Resource(AOwner : TComponent) : TV1Resource;

begin
  Result:=TV1Resource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TStoragetransferAPI.GetTransferJobsInstance : TTransferJobsResource;

begin
  if (FTransferJobsInstance=Nil) then
    FTransferJobsInstance:=CreateTransferJobsResource;
  Result:=FTransferJobsInstance;
end;

Function TStoragetransferAPI.CreateTransferJobsResource : TTransferJobsResource;

begin
  Result:=CreateTransferJobsResource(Self);
end;


Function TStoragetransferAPI.CreateTransferJobsResource(AOwner : TComponent) : TTransferJobsResource;

begin
  Result:=TTransferJobsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TStoragetransferAPI.GetTransferOperationsInstance : TTransferOperationsResource;

begin
  if (FTransferOperationsInstance=Nil) then
    FTransferOperationsInstance:=CreateTransferOperationsResource;
  Result:=FTransferOperationsInstance;
end;

Function TStoragetransferAPI.CreateTransferOperationsResource : TTransferOperationsResource;

begin
  Result:=CreateTransferOperationsResource(Self);
end;


Function TStoragetransferAPI.CreateTransferOperationsResource(AOwner : TComponent) : TTransferOperationsResource;

begin
  Result:=TTransferOperationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TStoragetransferAPI.RegisterAPI;
end.
