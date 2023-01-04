unit googletoolresults;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAny = Class;
  TDuration = Class;
  TExecution = Class;
  TFailureDetail = Class;
  TFileReference = Class;
  THistory = Class;
  TImage = Class;
  TInconclusiveDetail = Class;
  TListExecutionsResponse = Class;
  TListHistoriesResponse = Class;
  TListStepThumbnailsResponse = Class;
  TListStepsResponse = Class;
  TOutcome = Class;
  TProjectSettings = Class;
  TPublishXunitXmlFilesRequest = Class;
  TSkippedDetail = Class;
  TStackTrace = Class;
  TStatus = Class;
  TStep = Class;
  TStepDimensionValueEntry = Class;
  TStepLabelsEntry = Class;
  TSuccessDetail = Class;
  TTestCaseReference = Class;
  TTestExecutionStep = Class;
  TTestIssue = Class;
  TTestSuiteOverview = Class;
  TTestTiming = Class;
  TThumbnail = Class;
  TTimestamp = Class;
  TToolExecution = Class;
  TToolExecutionStep = Class;
  TToolExitCode = Class;
  TToolOutputReference = Class;
  TAnyArray = Array of TAny;
  TDurationArray = Array of TDuration;
  TExecutionArray = Array of TExecution;
  TFailureDetailArray = Array of TFailureDetail;
  TFileReferenceArray = Array of TFileReference;
  THistoryArray = Array of THistory;
  TImageArray = Array of TImage;
  TInconclusiveDetailArray = Array of TInconclusiveDetail;
  TListExecutionsResponseArray = Array of TListExecutionsResponse;
  TListHistoriesResponseArray = Array of TListHistoriesResponse;
  TListStepThumbnailsResponseArray = Array of TListStepThumbnailsResponse;
  TListStepsResponseArray = Array of TListStepsResponse;
  TOutcomeArray = Array of TOutcome;
  TProjectSettingsArray = Array of TProjectSettings;
  TPublishXunitXmlFilesRequestArray = Array of TPublishXunitXmlFilesRequest;
  TSkippedDetailArray = Array of TSkippedDetail;
  TStackTraceArray = Array of TStackTrace;
  TStatusArray = Array of TStatus;
  TStepArray = Array of TStep;
  TStepDimensionValueEntryArray = Array of TStepDimensionValueEntry;
  TStepLabelsEntryArray = Array of TStepLabelsEntry;
  TSuccessDetailArray = Array of TSuccessDetail;
  TTestCaseReferenceArray = Array of TTestCaseReference;
  TTestExecutionStepArray = Array of TTestExecutionStep;
  TTestIssueArray = Array of TTestIssue;
  TTestSuiteOverviewArray = Array of TTestSuiteOverview;
  TTestTimingArray = Array of TTestTiming;
  TThumbnailArray = Array of TThumbnail;
  TTimestampArray = Array of TTimestamp;
  TToolExecutionArray = Array of TToolExecution;
  TToolExecutionStepArray = Array of TToolExecutionStep;
  TToolExitCodeArray = Array of TToolExitCode;
  TToolOutputReferenceArray = Array of TToolOutputReference;
  //Anonymous types, using auto-generated names
  TListExecutionsResponseTypeexecutionsArray = Array of TExecution;
  TListHistoriesResponseTypehistoriesArray = Array of THistory;
  TListStepThumbnailsResponseTypethumbnailsArray = Array of TImage;
  TListStepsResponseTypestepsArray = Array of TStep;
  TPublishXunitXmlFilesRequestTypexunitXmlFilesArray = Array of TFileReference;
  TStatusTypedetailsArray = Array of TAny;
  TStepTypedimensionValueArray = Array of TStepDimensionValueEntry;
  TStepTypelabelsArray = Array of TStepLabelsEntry;
  TTestExecutionStepTypetestIssuesArray = Array of TTestIssue;
  TTestExecutionStepTypetestSuiteOverviewsArray = Array of TTestSuiteOverview;
  TToolExecutionTypetoolLogsArray = Array of TFileReference;
  TToolExecutionTypetoolOutputsArray = Array of TToolOutputReference;
  
  { --------------------------------------------------------------------
    TAny
    --------------------------------------------------------------------}
  
  TAny = Class(TGoogleBaseObject)
  Private
    FtypeUrl : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure SettypeUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property typeUrl : String Index 0 Read FtypeUrl Write SettypeUrl;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TAnyClass = Class of TAny;
  
  { --------------------------------------------------------------------
    TDuration
    --------------------------------------------------------------------}
  
  TDuration = Class(TGoogleBaseObject)
  Private
    Fnanos : integer;
    Fseconds : String;
  Protected
    //Property setters
    Procedure Setnanos(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setseconds(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property nanos : integer Index 0 Read Fnanos Write Setnanos;
    Property seconds : String Index 8 Read Fseconds Write Setseconds;
  end;
  TDurationClass = Class of TDuration;
  
  { --------------------------------------------------------------------
    TExecution
    --------------------------------------------------------------------}
  
  TExecution = Class(TGoogleBaseObject)
  Private
    FcompletionTime : TTimestamp;
    FcreationTime : TTimestamp;
    FexecutionId : String;
    Foutcome : TOutcome;
    Fstate : String;
    FtestExecutionMatrixId : String;
  Protected
    //Property setters
    Procedure SetcompletionTime(AIndex : Integer; const AValue : TTimestamp); virtual;
    Procedure SetcreationTime(AIndex : Integer; const AValue : TTimestamp); virtual;
    Procedure SetexecutionId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setoutcome(AIndex : Integer; const AValue : TOutcome); virtual;
    Procedure Setstate(AIndex : Integer; const AValue : String); virtual;
    Procedure SettestExecutionMatrixId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property completionTime : TTimestamp Index 0 Read FcompletionTime Write SetcompletionTime;
    Property creationTime : TTimestamp Index 8 Read FcreationTime Write SetcreationTime;
    Property executionId : String Index 16 Read FexecutionId Write SetexecutionId;
    Property outcome : TOutcome Index 24 Read Foutcome Write Setoutcome;
    Property state : String Index 32 Read Fstate Write Setstate;
    Property testExecutionMatrixId : String Index 40 Read FtestExecutionMatrixId Write SettestExecutionMatrixId;
  end;
  TExecutionClass = Class of TExecution;
  
  { --------------------------------------------------------------------
    TFailureDetail
    --------------------------------------------------------------------}
  
  TFailureDetail = Class(TGoogleBaseObject)
  Private
    Fcrashed : boolean;
    FnotInstalled : boolean;
    FotherNativeCrash : boolean;
    FtimedOut : boolean;
    FunableToCrawl : boolean;
  Protected
    //Property setters
    Procedure Setcrashed(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetnotInstalled(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetotherNativeCrash(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SettimedOut(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetunableToCrawl(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property crashed : boolean Index 0 Read Fcrashed Write Setcrashed;
    Property notInstalled : boolean Index 8 Read FnotInstalled Write SetnotInstalled;
    Property otherNativeCrash : boolean Index 16 Read FotherNativeCrash Write SetotherNativeCrash;
    Property timedOut : boolean Index 24 Read FtimedOut Write SettimedOut;
    Property unableToCrawl : boolean Index 32 Read FunableToCrawl Write SetunableToCrawl;
  end;
  TFailureDetailClass = Class of TFailureDetail;
  
  { --------------------------------------------------------------------
    TFileReference
    --------------------------------------------------------------------}
  
  TFileReference = Class(TGoogleBaseObject)
  Private
    FfileUri : String;
  Protected
    //Property setters
    Procedure SetfileUri(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property fileUri : String Index 0 Read FfileUri Write SetfileUri;
  end;
  TFileReferenceClass = Class of TFileReference;
  
  { --------------------------------------------------------------------
    THistory
    --------------------------------------------------------------------}
  
  THistory = Class(TGoogleBaseObject)
  Private
    FdisplayName : String;
    FhistoryId : String;
    Fname : String;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure SethistoryId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property displayName : String Index 0 Read FdisplayName Write SetdisplayName;
    Property historyId : String Index 8 Read FhistoryId Write SethistoryId;
    Property name : String Index 16 Read Fname Write Setname;
  end;
  THistoryClass = Class of THistory;
  
  { --------------------------------------------------------------------
    TImage
    --------------------------------------------------------------------}
  
  TImage = Class(TGoogleBaseObject)
  Private
    Ferror : TStatus;
    FsourceImage : TToolOutputReference;
    FstepId : String;
    Fthumbnail : TThumbnail;
  Protected
    //Property setters
    Procedure Seterror(AIndex : Integer; const AValue : TStatus); virtual;
    Procedure SetsourceImage(AIndex : Integer; const AValue : TToolOutputReference); virtual;
    Procedure SetstepId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setthumbnail(AIndex : Integer; const AValue : TThumbnail); virtual;
  Public
  Published
    Property error : TStatus Index 0 Read Ferror Write Seterror;
    Property sourceImage : TToolOutputReference Index 8 Read FsourceImage Write SetsourceImage;
    Property stepId : String Index 16 Read FstepId Write SetstepId;
    Property thumbnail : TThumbnail Index 24 Read Fthumbnail Write Setthumbnail;
  end;
  TImageClass = Class of TImage;
  
  { --------------------------------------------------------------------
    TInconclusiveDetail
    --------------------------------------------------------------------}
  
  TInconclusiveDetail = Class(TGoogleBaseObject)
  Private
    FabortedByUser : boolean;
    FinfrastructureFailure : boolean;
    FnativeCrash : boolean;
  Protected
    //Property setters
    Procedure SetabortedByUser(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetinfrastructureFailure(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetnativeCrash(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property abortedByUser : boolean Index 0 Read FabortedByUser Write SetabortedByUser;
    Property infrastructureFailure : boolean Index 8 Read FinfrastructureFailure Write SetinfrastructureFailure;
    Property nativeCrash : boolean Index 16 Read FnativeCrash Write SetnativeCrash;
  end;
  TInconclusiveDetailClass = Class of TInconclusiveDetail;
  
  { --------------------------------------------------------------------
    TListExecutionsResponse
    --------------------------------------------------------------------}
  
  TListExecutionsResponse = Class(TGoogleBaseObject)
  Private
    Fexecutions : TListExecutionsResponseTypeexecutionsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setexecutions(AIndex : Integer; const AValue : TListExecutionsResponseTypeexecutionsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property executions : TListExecutionsResponseTypeexecutionsArray Index 0 Read Fexecutions Write Setexecutions;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListExecutionsResponseClass = Class of TListExecutionsResponse;
  
  { --------------------------------------------------------------------
    TListHistoriesResponse
    --------------------------------------------------------------------}
  
  TListHistoriesResponse = Class(TGoogleBaseObject)
  Private
    Fhistories : TListHistoriesResponseTypehistoriesArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Sethistories(AIndex : Integer; const AValue : TListHistoriesResponseTypehistoriesArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property histories : TListHistoriesResponseTypehistoriesArray Index 0 Read Fhistories Write Sethistories;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListHistoriesResponseClass = Class of TListHistoriesResponse;
  
  { --------------------------------------------------------------------
    TListStepThumbnailsResponse
    --------------------------------------------------------------------}
  
  TListStepThumbnailsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Fthumbnails : TListStepThumbnailsResponseTypethumbnailsArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setthumbnails(AIndex : Integer; const AValue : TListStepThumbnailsResponseTypethumbnailsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property thumbnails : TListStepThumbnailsResponseTypethumbnailsArray Index 8 Read Fthumbnails Write Setthumbnails;
  end;
  TListStepThumbnailsResponseClass = Class of TListStepThumbnailsResponse;
  
  { --------------------------------------------------------------------
    TListStepsResponse
    --------------------------------------------------------------------}
  
  TListStepsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Fsteps : TListStepsResponseTypestepsArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsteps(AIndex : Integer; const AValue : TListStepsResponseTypestepsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property steps : TListStepsResponseTypestepsArray Index 8 Read Fsteps Write Setsteps;
  end;
  TListStepsResponseClass = Class of TListStepsResponse;
  
  { --------------------------------------------------------------------
    TOutcome
    --------------------------------------------------------------------}
  
  TOutcome = Class(TGoogleBaseObject)
  Private
    FfailureDetail : TFailureDetail;
    FinconclusiveDetail : TInconclusiveDetail;
    FskippedDetail : TSkippedDetail;
    FsuccessDetail : TSuccessDetail;
    Fsummary : String;
  Protected
    //Property setters
    Procedure SetfailureDetail(AIndex : Integer; const AValue : TFailureDetail); virtual;
    Procedure SetinconclusiveDetail(AIndex : Integer; const AValue : TInconclusiveDetail); virtual;
    Procedure SetskippedDetail(AIndex : Integer; const AValue : TSkippedDetail); virtual;
    Procedure SetsuccessDetail(AIndex : Integer; const AValue : TSuccessDetail); virtual;
    Procedure Setsummary(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property failureDetail : TFailureDetail Index 0 Read FfailureDetail Write SetfailureDetail;
    Property inconclusiveDetail : TInconclusiveDetail Index 8 Read FinconclusiveDetail Write SetinconclusiveDetail;
    Property skippedDetail : TSkippedDetail Index 16 Read FskippedDetail Write SetskippedDetail;
    Property successDetail : TSuccessDetail Index 24 Read FsuccessDetail Write SetsuccessDetail;
    Property summary : String Index 32 Read Fsummary Write Setsummary;
  end;
  TOutcomeClass = Class of TOutcome;
  
  { --------------------------------------------------------------------
    TProjectSettings
    --------------------------------------------------------------------}
  
  TProjectSettings = Class(TGoogleBaseObject)
  Private
    FdefaultBucket : String;
    Fname : String;
  Protected
    //Property setters
    Procedure SetdefaultBucket(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property defaultBucket : String Index 0 Read FdefaultBucket Write SetdefaultBucket;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TProjectSettingsClass = Class of TProjectSettings;
  
  { --------------------------------------------------------------------
    TPublishXunitXmlFilesRequest
    --------------------------------------------------------------------}
  
  TPublishXunitXmlFilesRequest = Class(TGoogleBaseObject)
  Private
    FxunitXmlFiles : TPublishXunitXmlFilesRequestTypexunitXmlFilesArray;
  Protected
    //Property setters
    Procedure SetxunitXmlFiles(AIndex : Integer; const AValue : TPublishXunitXmlFilesRequestTypexunitXmlFilesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property xunitXmlFiles : TPublishXunitXmlFilesRequestTypexunitXmlFilesArray Index 0 Read FxunitXmlFiles Write SetxunitXmlFiles;
  end;
  TPublishXunitXmlFilesRequestClass = Class of TPublishXunitXmlFilesRequest;
  
  { --------------------------------------------------------------------
    TSkippedDetail
    --------------------------------------------------------------------}
  
  TSkippedDetail = Class(TGoogleBaseObject)
  Private
    FincompatibleAppVersion : boolean;
    FincompatibleArchitecture : boolean;
    FincompatibleDevice : boolean;
  Protected
    //Property setters
    Procedure SetincompatibleAppVersion(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetincompatibleArchitecture(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetincompatibleDevice(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property incompatibleAppVersion : boolean Index 0 Read FincompatibleAppVersion Write SetincompatibleAppVersion;
    Property incompatibleArchitecture : boolean Index 8 Read FincompatibleArchitecture Write SetincompatibleArchitecture;
    Property incompatibleDevice : boolean Index 16 Read FincompatibleDevice Write SetincompatibleDevice;
  end;
  TSkippedDetailClass = Class of TSkippedDetail;
  
  { --------------------------------------------------------------------
    TStackTrace
    --------------------------------------------------------------------}
  
  TStackTrace = Class(TGoogleBaseObject)
  Private
    Fexception : String;
  Protected
    //Property setters
    Procedure Setexception(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property exception : String Index 0 Read Fexception Write Setexception;
  end;
  TStackTraceClass = Class of TStackTrace;
  
  { --------------------------------------------------------------------
    TStatus
    --------------------------------------------------------------------}
  
  TStatus = Class(TGoogleBaseObject)
  Private
    Fcode : integer;
    Fdetails : TStatusTypedetailsArray;
    Fmessage : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setdetails(AIndex : Integer; const AValue : TStatusTypedetailsArray); virtual;
    Procedure Setmessage(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property code : integer Index 0 Read Fcode Write Setcode;
    Property details : TStatusTypedetailsArray Index 8 Read Fdetails Write Setdetails;
    Property message : String Index 16 Read Fmessage Write Setmessage;
  end;
  TStatusClass = Class of TStatus;
  
  { --------------------------------------------------------------------
    TStep
    --------------------------------------------------------------------}
  
  TStep = Class(TGoogleBaseObject)
  Private
    FcompletionTime : TTimestamp;
    FcreationTime : TTimestamp;
    Fdescription : String;
    FdeviceUsageDuration : TDuration;
    FdimensionValue : TStepTypedimensionValueArray;
    FhasImages : boolean;
    Flabels : TStepTypelabelsArray;
    Fname : String;
    Foutcome : TOutcome;
    FrunDuration : TDuration;
    Fstate : String;
    FstepId : String;
    FtestExecutionStep : TTestExecutionStep;
    FtoolExecutionStep : TToolExecutionStep;
  Protected
    //Property setters
    Procedure SetcompletionTime(AIndex : Integer; const AValue : TTimestamp); virtual;
    Procedure SetcreationTime(AIndex : Integer; const AValue : TTimestamp); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdeviceUsageDuration(AIndex : Integer; const AValue : TDuration); virtual;
    Procedure SetdimensionValue(AIndex : Integer; const AValue : TStepTypedimensionValueArray); virtual;
    Procedure SethasImages(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setlabels(AIndex : Integer; const AValue : TStepTypelabelsArray); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setoutcome(AIndex : Integer; const AValue : TOutcome); virtual;
    Procedure SetrunDuration(AIndex : Integer; const AValue : TDuration); virtual;
    Procedure Setstate(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstepId(AIndex : Integer; const AValue : String); virtual;
    Procedure SettestExecutionStep(AIndex : Integer; const AValue : TTestExecutionStep); virtual;
    Procedure SettoolExecutionStep(AIndex : Integer; const AValue : TToolExecutionStep); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property completionTime : TTimestamp Index 0 Read FcompletionTime Write SetcompletionTime;
    Property creationTime : TTimestamp Index 8 Read FcreationTime Write SetcreationTime;
    Property description : String Index 16 Read Fdescription Write Setdescription;
    Property deviceUsageDuration : TDuration Index 24 Read FdeviceUsageDuration Write SetdeviceUsageDuration;
    Property dimensionValue : TStepTypedimensionValueArray Index 32 Read FdimensionValue Write SetdimensionValue;
    Property hasImages : boolean Index 40 Read FhasImages Write SethasImages;
    Property labels : TStepTypelabelsArray Index 48 Read Flabels Write Setlabels;
    Property name : String Index 56 Read Fname Write Setname;
    Property outcome : TOutcome Index 64 Read Foutcome Write Setoutcome;
    Property runDuration : TDuration Index 72 Read FrunDuration Write SetrunDuration;
    Property state : String Index 80 Read Fstate Write Setstate;
    Property stepId : String Index 88 Read FstepId Write SetstepId;
    Property testExecutionStep : TTestExecutionStep Index 96 Read FtestExecutionStep Write SettestExecutionStep;
    Property toolExecutionStep : TToolExecutionStep Index 104 Read FtoolExecutionStep Write SettoolExecutionStep;
  end;
  TStepClass = Class of TStep;
  
  { --------------------------------------------------------------------
    TStepDimensionValueEntry
    --------------------------------------------------------------------}
  
  TStepDimensionValueEntry = Class(TGoogleBaseObject)
  Private
    Fkey : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property key : String Index 0 Read Fkey Write Setkey;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TStepDimensionValueEntryClass = Class of TStepDimensionValueEntry;
  
  { --------------------------------------------------------------------
    TStepLabelsEntry
    --------------------------------------------------------------------}
  
  TStepLabelsEntry = Class(TGoogleBaseObject)
  Private
    Fkey : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property key : String Index 0 Read Fkey Write Setkey;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TStepLabelsEntryClass = Class of TStepLabelsEntry;
  
  { --------------------------------------------------------------------
    TSuccessDetail
    --------------------------------------------------------------------}
  
  TSuccessDetail = Class(TGoogleBaseObject)
  Private
    FotherNativeCrash : boolean;
  Protected
    //Property setters
    Procedure SetotherNativeCrash(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property otherNativeCrash : boolean Index 0 Read FotherNativeCrash Write SetotherNativeCrash;
  end;
  TSuccessDetailClass = Class of TSuccessDetail;
  
  { --------------------------------------------------------------------
    TTestCaseReference
    --------------------------------------------------------------------}
  
  TTestCaseReference = Class(TGoogleBaseObject)
  Private
    FclassName : String;
    Fname : String;
    FtestSuiteName : String;
  Protected
    //Property setters
    Procedure SetclassName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SettestSuiteName(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property className : String Index 0 Read FclassName Write SetclassName;
    Property name : String Index 8 Read Fname Write Setname;
    Property testSuiteName : String Index 16 Read FtestSuiteName Write SettestSuiteName;
  end;
  TTestCaseReferenceClass = Class of TTestCaseReference;
  
  { --------------------------------------------------------------------
    TTestExecutionStep
    --------------------------------------------------------------------}
  
  TTestExecutionStep = Class(TGoogleBaseObject)
  Private
    FtestIssues : TTestExecutionStepTypetestIssuesArray;
    FtestSuiteOverviews : TTestExecutionStepTypetestSuiteOverviewsArray;
    FtestTiming : TTestTiming;
    FtoolExecution : TToolExecution;
  Protected
    //Property setters
    Procedure SettestIssues(AIndex : Integer; const AValue : TTestExecutionStepTypetestIssuesArray); virtual;
    Procedure SettestSuiteOverviews(AIndex : Integer; const AValue : TTestExecutionStepTypetestSuiteOverviewsArray); virtual;
    Procedure SettestTiming(AIndex : Integer; const AValue : TTestTiming); virtual;
    Procedure SettoolExecution(AIndex : Integer; const AValue : TToolExecution); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property testIssues : TTestExecutionStepTypetestIssuesArray Index 0 Read FtestIssues Write SettestIssues;
    Property testSuiteOverviews : TTestExecutionStepTypetestSuiteOverviewsArray Index 8 Read FtestSuiteOverviews Write SettestSuiteOverviews;
    Property testTiming : TTestTiming Index 16 Read FtestTiming Write SettestTiming;
    Property toolExecution : TToolExecution Index 24 Read FtoolExecution Write SettoolExecution;
  end;
  TTestExecutionStepClass = Class of TTestExecutionStep;
  
  { --------------------------------------------------------------------
    TTestIssue
    --------------------------------------------------------------------}
  
  TTestIssue = Class(TGoogleBaseObject)
  Private
    FerrorMessage : String;
    FstackTrace : TStackTrace;
  Protected
    //Property setters
    Procedure SeterrorMessage(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstackTrace(AIndex : Integer; const AValue : TStackTrace); virtual;
  Public
  Published
    Property errorMessage : String Index 0 Read FerrorMessage Write SeterrorMessage;
    Property stackTrace : TStackTrace Index 8 Read FstackTrace Write SetstackTrace;
  end;
  TTestIssueClass = Class of TTestIssue;
  
  { --------------------------------------------------------------------
    TTestSuiteOverview
    --------------------------------------------------------------------}
  
  TTestSuiteOverview = Class(TGoogleBaseObject)
  Private
    FerrorCount : integer;
    FfailureCount : integer;
    Fname : String;
    FskippedCount : integer;
    FtotalCount : integer;
    FxmlSource : TFileReference;
  Protected
    //Property setters
    Procedure SeterrorCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetfailureCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetskippedCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure SettotalCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetxmlSource(AIndex : Integer; const AValue : TFileReference); virtual;
  Public
  Published
    Property errorCount : integer Index 0 Read FerrorCount Write SeterrorCount;
    Property failureCount : integer Index 8 Read FfailureCount Write SetfailureCount;
    Property name : String Index 16 Read Fname Write Setname;
    Property skippedCount : integer Index 24 Read FskippedCount Write SetskippedCount;
    Property totalCount : integer Index 32 Read FtotalCount Write SettotalCount;
    Property xmlSource : TFileReference Index 40 Read FxmlSource Write SetxmlSource;
  end;
  TTestSuiteOverviewClass = Class of TTestSuiteOverview;
  
  { --------------------------------------------------------------------
    TTestTiming
    --------------------------------------------------------------------}
  
  TTestTiming = Class(TGoogleBaseObject)
  Private
    FtestProcessDuration : TDuration;
  Protected
    //Property setters
    Procedure SettestProcessDuration(AIndex : Integer; const AValue : TDuration); virtual;
  Public
  Published
    Property testProcessDuration : TDuration Index 0 Read FtestProcessDuration Write SettestProcessDuration;
  end;
  TTestTimingClass = Class of TTestTiming;
  
  { --------------------------------------------------------------------
    TThumbnail
    --------------------------------------------------------------------}
  
  TThumbnail = Class(TGoogleBaseObject)
  Private
    FcontentType : String;
    Fdata : String;
    FheightPx : integer;
    FwidthPx : integer;
  Protected
    //Property setters
    Procedure SetcontentType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdata(AIndex : Integer; const AValue : String); virtual;
    Procedure SetheightPx(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetwidthPx(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property contentType : String Index 0 Read FcontentType Write SetcontentType;
    Property data : String Index 8 Read Fdata Write Setdata;
    Property heightPx : integer Index 16 Read FheightPx Write SetheightPx;
    Property widthPx : integer Index 24 Read FwidthPx Write SetwidthPx;
  end;
  TThumbnailClass = Class of TThumbnail;
  
  { --------------------------------------------------------------------
    TTimestamp
    --------------------------------------------------------------------}
  
  TTimestamp = Class(TGoogleBaseObject)
  Private
    Fnanos : integer;
    Fseconds : String;
  Protected
    //Property setters
    Procedure Setnanos(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setseconds(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property nanos : integer Index 0 Read Fnanos Write Setnanos;
    Property seconds : String Index 8 Read Fseconds Write Setseconds;
  end;
  TTimestampClass = Class of TTimestamp;
  
  { --------------------------------------------------------------------
    TToolExecution
    --------------------------------------------------------------------}
  
  TToolExecution = Class(TGoogleBaseObject)
  Private
    FcommandLineArguments : TStringArray;
    FexitCode : TToolExitCode;
    FtoolLogs : TToolExecutionTypetoolLogsArray;
    FtoolOutputs : TToolExecutionTypetoolOutputsArray;
  Protected
    //Property setters
    Procedure SetcommandLineArguments(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetexitCode(AIndex : Integer; const AValue : TToolExitCode); virtual;
    Procedure SettoolLogs(AIndex : Integer; const AValue : TToolExecutionTypetoolLogsArray); virtual;
    Procedure SettoolOutputs(AIndex : Integer; const AValue : TToolExecutionTypetoolOutputsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property commandLineArguments : TStringArray Index 0 Read FcommandLineArguments Write SetcommandLineArguments;
    Property exitCode : TToolExitCode Index 8 Read FexitCode Write SetexitCode;
    Property toolLogs : TToolExecutionTypetoolLogsArray Index 16 Read FtoolLogs Write SettoolLogs;
    Property toolOutputs : TToolExecutionTypetoolOutputsArray Index 24 Read FtoolOutputs Write SettoolOutputs;
  end;
  TToolExecutionClass = Class of TToolExecution;
  
  { --------------------------------------------------------------------
    TToolExecutionStep
    --------------------------------------------------------------------}
  
  TToolExecutionStep = Class(TGoogleBaseObject)
  Private
    FtoolExecution : TToolExecution;
  Protected
    //Property setters
    Procedure SettoolExecution(AIndex : Integer; const AValue : TToolExecution); virtual;
  Public
  Published
    Property toolExecution : TToolExecution Index 0 Read FtoolExecution Write SettoolExecution;
  end;
  TToolExecutionStepClass = Class of TToolExecutionStep;
  
  { --------------------------------------------------------------------
    TToolExitCode
    --------------------------------------------------------------------}
  
  TToolExitCode = Class(TGoogleBaseObject)
  Private
    Fnumber : integer;
  Protected
    //Property setters
    Procedure Setnumber(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property number : integer Index 0 Read Fnumber Write Setnumber;
  end;
  TToolExitCodeClass = Class of TToolExitCode;
  
  { --------------------------------------------------------------------
    TToolOutputReference
    --------------------------------------------------------------------}
  
  TToolOutputReference = Class(TGoogleBaseObject)
  Private
    FcreationTime : TTimestamp;
    Foutput : TFileReference;
    FtestCase : TTestCaseReference;
  Protected
    //Property setters
    Procedure SetcreationTime(AIndex : Integer; const AValue : TTimestamp); virtual;
    Procedure Setoutput(AIndex : Integer; const AValue : TFileReference); virtual;
    Procedure SettestCase(AIndex : Integer; const AValue : TTestCaseReference); virtual;
  Public
  Published
    Property creationTime : TTimestamp Index 0 Read FcreationTime Write SetcreationTime;
    Property output : TFileReference Index 8 Read Foutput Write Setoutput;
    Property testCase : TTestCaseReference Index 16 Read FtestCase Write SettestCase;
  end;
  TToolOutputReferenceClass = Class of TToolOutputReference;
  
  { --------------------------------------------------------------------
    TProjectsHistoriesExecutionsStepsThumbnailsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsHistoriesExecutionsStepsThumbnailsResource, method List
  
  TProjectsHistoriesExecutionsStepsThumbnailsListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  TProjectsHistoriesExecutionsStepsThumbnailsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(executionId: string; historyId: string; projectId: string; stepId: string; AQuery : string  = '') : TListStepThumbnailsResponse;
    Function List(executionId: string; historyId: string; projectId: string; stepId: string; AQuery : TProjectsHistoriesExecutionsStepsThumbnailslistOptions) : TListStepThumbnailsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsHistoriesExecutionsStepsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsHistoriesExecutionsStepsResource, method Create
  
  TProjectsHistoriesExecutionsStepsCreateOptions = Record
    requestId : String;
  end;
  
  
  //Optional query Options for TProjectsHistoriesExecutionsStepsResource, method List
  
  TProjectsHistoriesExecutionsStepsListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  
  //Optional query Options for TProjectsHistoriesExecutionsStepsResource, method Patch
  
  TProjectsHistoriesExecutionsStepsPatchOptions = Record
    requestId : String;
  end;
  
  TProjectsHistoriesExecutionsStepsResource = Class(TGoogleResource)
  Private
    FThumbnailsInstance : TProjectsHistoriesExecutionsStepsThumbnailsResource;
    Function GetThumbnailsInstance : TProjectsHistoriesExecutionsStepsThumbnailsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(executionId: string; historyId: string; projectId: string; aStep : TStep; AQuery : string  = '') : TStep;overload;
    Function Create(executionId: string; historyId: string; projectId: string; aStep : TStep; AQuery : TProjectsHistoriesExecutionsStepscreateOptions) : TStep;overload;
    Function Get(executionId: string; historyId: string; projectId: string; stepId: string) : TStep;
    Function List(executionId: string; historyId: string; projectId: string; AQuery : string  = '') : TListStepsResponse;
    Function List(executionId: string; historyId: string; projectId: string; AQuery : TProjectsHistoriesExecutionsStepslistOptions) : TListStepsResponse;
    Function Patch(executionId: string; historyId: string; projectId: string; stepId: string; aStep : TStep; AQuery : string  = '') : TStep;
    Function Patch(executionId: string; historyId: string; projectId: string; stepId: string; aStep : TStep; AQuery : TProjectsHistoriesExecutionsStepspatchOptions) : TStep;
    Function PublishXunitXmlFiles(executionId: string; historyId: string; projectId: string; stepId: string; aPublishXunitXmlFilesRequest : TPublishXunitXmlFilesRequest) : TStep;
    Function CreateThumbnailsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsStepsThumbnailsResource;virtual;overload;
    Function CreateThumbnailsResource : TProjectsHistoriesExecutionsStepsThumbnailsResource;virtual;overload;
    Property ThumbnailsResource : TProjectsHistoriesExecutionsStepsThumbnailsResource Read GetThumbnailsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsHistoriesExecutionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsHistoriesExecutionsResource, method Create
  
  TProjectsHistoriesExecutionsCreateOptions = Record
    requestId : String;
  end;
  
  
  //Optional query Options for TProjectsHistoriesExecutionsResource, method List
  
  TProjectsHistoriesExecutionsListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  
  //Optional query Options for TProjectsHistoriesExecutionsResource, method Patch
  
  TProjectsHistoriesExecutionsPatchOptions = Record
    requestId : String;
  end;
  
  TProjectsHistoriesExecutionsResource = Class(TGoogleResource)
  Private
    FStepsThumbnailsInstance : TProjectsHistoriesExecutionsStepsThumbnailsResource;
    FStepsInstance : TProjectsHistoriesExecutionsStepsResource;
    Function GetStepsThumbnailsInstance : TProjectsHistoriesExecutionsStepsThumbnailsResource;virtual;
    Function GetStepsInstance : TProjectsHistoriesExecutionsStepsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(historyId: string; projectId: string; aExecution : TExecution; AQuery : string  = '') : TExecution;overload;
    Function Create(historyId: string; projectId: string; aExecution : TExecution; AQuery : TProjectsHistoriesExecutionscreateOptions) : TExecution;overload;
    Function Get(executionId: string; historyId: string; projectId: string) : TExecution;
    Function List(historyId: string; projectId: string; AQuery : string  = '') : TListExecutionsResponse;
    Function List(historyId: string; projectId: string; AQuery : TProjectsHistoriesExecutionslistOptions) : TListExecutionsResponse;
    Function Patch(executionId: string; historyId: string; projectId: string; aExecution : TExecution; AQuery : string  = '') : TExecution;
    Function Patch(executionId: string; historyId: string; projectId: string; aExecution : TExecution; AQuery : TProjectsHistoriesExecutionspatchOptions) : TExecution;
    Function CreateStepsThumbnailsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsStepsThumbnailsResource;virtual;overload;
    Function CreateStepsThumbnailsResource : TProjectsHistoriesExecutionsStepsThumbnailsResource;virtual;overload;
    Function CreateStepsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsStepsResource;virtual;overload;
    Function CreateStepsResource : TProjectsHistoriesExecutionsStepsResource;virtual;overload;
    Property StepsThumbnailsResource : TProjectsHistoriesExecutionsStepsThumbnailsResource Read GetStepsThumbnailsInstance;
    Property StepsResource : TProjectsHistoriesExecutionsStepsResource Read GetStepsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsHistoriesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsHistoriesResource, method Create
  
  TProjectsHistoriesCreateOptions = Record
    requestId : String;
  end;
  
  
  //Optional query Options for TProjectsHistoriesResource, method List
  
  TProjectsHistoriesListOptions = Record
    filterByName : String;
    pageSize : integer;
    pageToken : String;
  end;
  
  TProjectsHistoriesResource = Class(TGoogleResource)
  Private
    FExecutionsStepsThumbnailsInstance : TProjectsHistoriesExecutionsStepsThumbnailsResource;
    FExecutionsStepsInstance : TProjectsHistoriesExecutionsStepsResource;
    FExecutionsInstance : TProjectsHistoriesExecutionsResource;
    Function GetExecutionsStepsThumbnailsInstance : TProjectsHistoriesExecutionsStepsThumbnailsResource;virtual;
    Function GetExecutionsStepsInstance : TProjectsHistoriesExecutionsStepsResource;virtual;
    Function GetExecutionsInstance : TProjectsHistoriesExecutionsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(projectId: string; aHistory : THistory; AQuery : string  = '') : THistory;overload;
    Function Create(projectId: string; aHistory : THistory; AQuery : TProjectsHistoriescreateOptions) : THistory;overload;
    Function Get(historyId: string; projectId: string) : THistory;
    Function List(projectId: string; AQuery : string  = '') : TListHistoriesResponse;
    Function List(projectId: string; AQuery : TProjectsHistorieslistOptions) : TListHistoriesResponse;
    Function CreateExecutionsStepsThumbnailsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsStepsThumbnailsResource;virtual;overload;
    Function CreateExecutionsStepsThumbnailsResource : TProjectsHistoriesExecutionsStepsThumbnailsResource;virtual;overload;
    Function CreateExecutionsStepsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsStepsResource;virtual;overload;
    Function CreateExecutionsStepsResource : TProjectsHistoriesExecutionsStepsResource;virtual;overload;
    Function CreateExecutionsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsResource;virtual;overload;
    Function CreateExecutionsResource : TProjectsHistoriesExecutionsResource;virtual;overload;
    Property ExecutionsStepsThumbnailsResource : TProjectsHistoriesExecutionsStepsThumbnailsResource Read GetExecutionsStepsThumbnailsInstance;
    Property ExecutionsStepsResource : TProjectsHistoriesExecutionsStepsResource Read GetExecutionsStepsInstance;
    Property ExecutionsResource : TProjectsHistoriesExecutionsResource Read GetExecutionsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Private
    FHistoriesExecutionsStepsThumbnailsInstance : TProjectsHistoriesExecutionsStepsThumbnailsResource;
    FHistoriesExecutionsStepsInstance : TProjectsHistoriesExecutionsStepsResource;
    FHistoriesExecutionsInstance : TProjectsHistoriesExecutionsResource;
    FHistoriesInstance : TProjectsHistoriesResource;
    Function GetHistoriesExecutionsStepsThumbnailsInstance : TProjectsHistoriesExecutionsStepsThumbnailsResource;virtual;
    Function GetHistoriesExecutionsStepsInstance : TProjectsHistoriesExecutionsStepsResource;virtual;
    Function GetHistoriesExecutionsInstance : TProjectsHistoriesExecutionsResource;virtual;
    Function GetHistoriesInstance : TProjectsHistoriesResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function GetSettings(projectId: string) : TProjectSettings;
    Function InitializeSettings(projectId: string) : TProjectSettings;
    Function CreateHistoriesExecutionsStepsThumbnailsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsStepsThumbnailsResource;virtual;overload;
    Function CreateHistoriesExecutionsStepsThumbnailsResource : TProjectsHistoriesExecutionsStepsThumbnailsResource;virtual;overload;
    Function CreateHistoriesExecutionsStepsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsStepsResource;virtual;overload;
    Function CreateHistoriesExecutionsStepsResource : TProjectsHistoriesExecutionsStepsResource;virtual;overload;
    Function CreateHistoriesExecutionsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsResource;virtual;overload;
    Function CreateHistoriesExecutionsResource : TProjectsHistoriesExecutionsResource;virtual;overload;
    Function CreateHistoriesResource(AOwner : TComponent) : TProjectsHistoriesResource;virtual;overload;
    Function CreateHistoriesResource : TProjectsHistoriesResource;virtual;overload;
    Property HistoriesExecutionsStepsThumbnailsResource : TProjectsHistoriesExecutionsStepsThumbnailsResource Read GetHistoriesExecutionsStepsThumbnailsInstance;
    Property HistoriesExecutionsStepsResource : TProjectsHistoriesExecutionsStepsResource Read GetHistoriesExecutionsStepsInstance;
    Property HistoriesExecutionsResource : TProjectsHistoriesExecutionsResource Read GetHistoriesExecutionsInstance;
    Property HistoriesResource : TProjectsHistoriesResource Read GetHistoriesInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TToolresultsAPI
    --------------------------------------------------------------------}
  
  TToolresultsAPI = Class(TGoogleAPI)
  Private
    FProjectsHistoriesExecutionsStepsThumbnailsInstance : TProjectsHistoriesExecutionsStepsThumbnailsResource;
    FProjectsHistoriesExecutionsStepsInstance : TProjectsHistoriesExecutionsStepsResource;
    FProjectsHistoriesExecutionsInstance : TProjectsHistoriesExecutionsResource;
    FProjectsHistoriesInstance : TProjectsHistoriesResource;
    FProjectsInstance : TProjectsResource;
    Function GetProjectsHistoriesExecutionsStepsThumbnailsInstance : TProjectsHistoriesExecutionsStepsThumbnailsResource;virtual;
    Function GetProjectsHistoriesExecutionsStepsInstance : TProjectsHistoriesExecutionsStepsResource;virtual;
    Function GetProjectsHistoriesExecutionsInstance : TProjectsHistoriesExecutionsResource;virtual;
    Function GetProjectsHistoriesInstance : TProjectsHistoriesResource;virtual;
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
    Function CreateProjectsHistoriesExecutionsStepsThumbnailsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsStepsThumbnailsResource;virtual;overload;
    Function CreateProjectsHistoriesExecutionsStepsThumbnailsResource : TProjectsHistoriesExecutionsStepsThumbnailsResource;virtual;overload;
    Function CreateProjectsHistoriesExecutionsStepsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsStepsResource;virtual;overload;
    Function CreateProjectsHistoriesExecutionsStepsResource : TProjectsHistoriesExecutionsStepsResource;virtual;overload;
    Function CreateProjectsHistoriesExecutionsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsResource;virtual;overload;
    Function CreateProjectsHistoriesExecutionsResource : TProjectsHistoriesExecutionsResource;virtual;overload;
    Function CreateProjectsHistoriesResource(AOwner : TComponent) : TProjectsHistoriesResource;virtual;overload;
    Function CreateProjectsHistoriesResource : TProjectsHistoriesResource;virtual;overload;
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ProjectsHistoriesExecutionsStepsThumbnailsResource : TProjectsHistoriesExecutionsStepsThumbnailsResource Read GetProjectsHistoriesExecutionsStepsThumbnailsInstance;
    Property ProjectsHistoriesExecutionsStepsResource : TProjectsHistoriesExecutionsStepsResource Read GetProjectsHistoriesExecutionsStepsInstance;
    Property ProjectsHistoriesExecutionsResource : TProjectsHistoriesExecutionsResource Read GetProjectsHistoriesExecutionsInstance;
    Property ProjectsHistoriesResource : TProjectsHistoriesResource Read GetProjectsHistoriesInstance;
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAny
  --------------------------------------------------------------------}


Procedure TAny.SettypeUrl(AIndex : Integer; const AValue : String); 

begin
  If (FtypeUrl=AValue) then exit;
  FtypeUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAny.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDuration
  --------------------------------------------------------------------}


Procedure TDuration.Setnanos(AIndex : Integer; const AValue : integer); 

begin
  If (Fnanos=AValue) then exit;
  Fnanos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDuration.Setseconds(AIndex : Integer; const AValue : String); 

begin
  If (Fseconds=AValue) then exit;
  Fseconds:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExecution
  --------------------------------------------------------------------}


Procedure TExecution.SetcompletionTime(AIndex : Integer; const AValue : TTimestamp); 

begin
  If (FcompletionTime=AValue) then exit;
  FcompletionTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExecution.SetcreationTime(AIndex : Integer; const AValue : TTimestamp); 

begin
  If (FcreationTime=AValue) then exit;
  FcreationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExecution.SetexecutionId(AIndex : Integer; const AValue : String); 

begin
  If (FexecutionId=AValue) then exit;
  FexecutionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExecution.Setoutcome(AIndex : Integer; const AValue : TOutcome); 

begin
  If (Foutcome=AValue) then exit;
  Foutcome:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExecution.Setstate(AIndex : Integer; const AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExecution.SettestExecutionMatrixId(AIndex : Integer; const AValue : String); 

begin
  If (FtestExecutionMatrixId=AValue) then exit;
  FtestExecutionMatrixId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFailureDetail
  --------------------------------------------------------------------}


Procedure TFailureDetail.Setcrashed(AIndex : Integer; const AValue : boolean); 

begin
  If (Fcrashed=AValue) then exit;
  Fcrashed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFailureDetail.SetnotInstalled(AIndex : Integer; const AValue : boolean); 

begin
  If (FnotInstalled=AValue) then exit;
  FnotInstalled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFailureDetail.SetotherNativeCrash(AIndex : Integer; const AValue : boolean); 

begin
  If (FotherNativeCrash=AValue) then exit;
  FotherNativeCrash:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFailureDetail.SettimedOut(AIndex : Integer; const AValue : boolean); 

begin
  If (FtimedOut=AValue) then exit;
  FtimedOut:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFailureDetail.SetunableToCrawl(AIndex : Integer; const AValue : boolean); 

begin
  If (FunableToCrawl=AValue) then exit;
  FunableToCrawl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFileReference
  --------------------------------------------------------------------}


Procedure TFileReference.SetfileUri(AIndex : Integer; const AValue : String); 

begin
  If (FfileUri=AValue) then exit;
  FfileUri:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  THistory
  --------------------------------------------------------------------}


Procedure THistory.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THistory.SethistoryId(AIndex : Integer; const AValue : String); 

begin
  If (FhistoryId=AValue) then exit;
  FhistoryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THistory.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImage
  --------------------------------------------------------------------}


Procedure TImage.Seterror(AIndex : Integer; const AValue : TStatus); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImage.SetsourceImage(AIndex : Integer; const AValue : TToolOutputReference); 

begin
  If (FsourceImage=AValue) then exit;
  FsourceImage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImage.SetstepId(AIndex : Integer; const AValue : String); 

begin
  If (FstepId=AValue) then exit;
  FstepId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImage.Setthumbnail(AIndex : Integer; const AValue : TThumbnail); 

begin
  If (Fthumbnail=AValue) then exit;
  Fthumbnail:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInconclusiveDetail
  --------------------------------------------------------------------}


Procedure TInconclusiveDetail.SetabortedByUser(AIndex : Integer; const AValue : boolean); 

begin
  If (FabortedByUser=AValue) then exit;
  FabortedByUser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInconclusiveDetail.SetinfrastructureFailure(AIndex : Integer; const AValue : boolean); 

begin
  If (FinfrastructureFailure=AValue) then exit;
  FinfrastructureFailure:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInconclusiveDetail.SetnativeCrash(AIndex : Integer; const AValue : boolean); 

begin
  If (FnativeCrash=AValue) then exit;
  FnativeCrash:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListExecutionsResponse
  --------------------------------------------------------------------}


Procedure TListExecutionsResponse.Setexecutions(AIndex : Integer; const AValue : TListExecutionsResponseTypeexecutionsArray); 

begin
  If (Fexecutions=AValue) then exit;
  Fexecutions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListExecutionsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListExecutionsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'executions' : SetLength(Fexecutions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListHistoriesResponse
  --------------------------------------------------------------------}


Procedure TListHistoriesResponse.Sethistories(AIndex : Integer; const AValue : TListHistoriesResponseTypehistoriesArray); 

begin
  If (Fhistories=AValue) then exit;
  Fhistories:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListHistoriesResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListHistoriesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'histories' : SetLength(Fhistories,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListStepThumbnailsResponse
  --------------------------------------------------------------------}


Procedure TListStepThumbnailsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListStepThumbnailsResponse.Setthumbnails(AIndex : Integer; const AValue : TListStepThumbnailsResponseTypethumbnailsArray); 

begin
  If (Fthumbnails=AValue) then exit;
  Fthumbnails:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListStepThumbnailsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'thumbnails' : SetLength(Fthumbnails,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListStepsResponse
  --------------------------------------------------------------------}


Procedure TListStepsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListStepsResponse.Setsteps(AIndex : Integer; const AValue : TListStepsResponseTypestepsArray); 

begin
  If (Fsteps=AValue) then exit;
  Fsteps:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListStepsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'steps' : SetLength(Fsteps,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TOutcome
  --------------------------------------------------------------------}


Procedure TOutcome.SetfailureDetail(AIndex : Integer; const AValue : TFailureDetail); 

begin
  If (FfailureDetail=AValue) then exit;
  FfailureDetail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOutcome.SetinconclusiveDetail(AIndex : Integer; const AValue : TInconclusiveDetail); 

begin
  If (FinconclusiveDetail=AValue) then exit;
  FinconclusiveDetail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOutcome.SetskippedDetail(AIndex : Integer; const AValue : TSkippedDetail); 

begin
  If (FskippedDetail=AValue) then exit;
  FskippedDetail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOutcome.SetsuccessDetail(AIndex : Integer; const AValue : TSuccessDetail); 

begin
  If (FsuccessDetail=AValue) then exit;
  FsuccessDetail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOutcome.Setsummary(AIndex : Integer; const AValue : String); 

begin
  If (Fsummary=AValue) then exit;
  Fsummary:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProjectSettings
  --------------------------------------------------------------------}


Procedure TProjectSettings.SetdefaultBucket(AIndex : Integer; const AValue : String); 

begin
  If (FdefaultBucket=AValue) then exit;
  FdefaultBucket:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProjectSettings.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPublishXunitXmlFilesRequest
  --------------------------------------------------------------------}


Procedure TPublishXunitXmlFilesRequest.SetxunitXmlFiles(AIndex : Integer; const AValue : TPublishXunitXmlFilesRequestTypexunitXmlFilesArray); 

begin
  If (FxunitXmlFiles=AValue) then exit;
  FxunitXmlFiles:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPublishXunitXmlFilesRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'xunitxmlfiles' : SetLength(FxunitXmlFiles,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSkippedDetail
  --------------------------------------------------------------------}


Procedure TSkippedDetail.SetincompatibleAppVersion(AIndex : Integer; const AValue : boolean); 

begin
  If (FincompatibleAppVersion=AValue) then exit;
  FincompatibleAppVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSkippedDetail.SetincompatibleArchitecture(AIndex : Integer; const AValue : boolean); 

begin
  If (FincompatibleArchitecture=AValue) then exit;
  FincompatibleArchitecture:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSkippedDetail.SetincompatibleDevice(AIndex : Integer; const AValue : boolean); 

begin
  If (FincompatibleDevice=AValue) then exit;
  FincompatibleDevice:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStackTrace
  --------------------------------------------------------------------}


Procedure TStackTrace.Setexception(AIndex : Integer; const AValue : String); 

begin
  If (Fexception=AValue) then exit;
  Fexception:=AValue;
  MarkPropertyChanged(AIndex);
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



Procedure TStatus.Setdetails(AIndex : Integer; const AValue : TStatusTypedetailsArray); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStatus.Setmessage(AIndex : Integer; const AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
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
  TStep
  --------------------------------------------------------------------}


Procedure TStep.SetcompletionTime(AIndex : Integer; const AValue : TTimestamp); 

begin
  If (FcompletionTime=AValue) then exit;
  FcompletionTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStep.SetcreationTime(AIndex : Integer; const AValue : TTimestamp); 

begin
  If (FcreationTime=AValue) then exit;
  FcreationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStep.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStep.SetdeviceUsageDuration(AIndex : Integer; const AValue : TDuration); 

begin
  If (FdeviceUsageDuration=AValue) then exit;
  FdeviceUsageDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStep.SetdimensionValue(AIndex : Integer; const AValue : TStepTypedimensionValueArray); 

begin
  If (FdimensionValue=AValue) then exit;
  FdimensionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStep.SethasImages(AIndex : Integer; const AValue : boolean); 

begin
  If (FhasImages=AValue) then exit;
  FhasImages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStep.Setlabels(AIndex : Integer; const AValue : TStepTypelabelsArray); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStep.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStep.Setoutcome(AIndex : Integer; const AValue : TOutcome); 

begin
  If (Foutcome=AValue) then exit;
  Foutcome:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStep.SetrunDuration(AIndex : Integer; const AValue : TDuration); 

begin
  If (FrunDuration=AValue) then exit;
  FrunDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStep.Setstate(AIndex : Integer; const AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStep.SetstepId(AIndex : Integer; const AValue : String); 

begin
  If (FstepId=AValue) then exit;
  FstepId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStep.SettestExecutionStep(AIndex : Integer; const AValue : TTestExecutionStep); 

begin
  If (FtestExecutionStep=AValue) then exit;
  FtestExecutionStep:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStep.SettoolExecutionStep(AIndex : Integer; const AValue : TToolExecutionStep); 

begin
  If (FtoolExecutionStep=AValue) then exit;
  FtoolExecutionStep:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TStep.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'dimensionvalue' : SetLength(FdimensionValue,ALength);
  'labels' : SetLength(Flabels,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TStepDimensionValueEntry
  --------------------------------------------------------------------}


Procedure TStepDimensionValueEntry.Setkey(AIndex : Integer; const AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStepDimensionValueEntry.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStepLabelsEntry
  --------------------------------------------------------------------}


Procedure TStepLabelsEntry.Setkey(AIndex : Integer; const AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStepLabelsEntry.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSuccessDetail
  --------------------------------------------------------------------}


Procedure TSuccessDetail.SetotherNativeCrash(AIndex : Integer; const AValue : boolean); 

begin
  If (FotherNativeCrash=AValue) then exit;
  FotherNativeCrash:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTestCaseReference
  --------------------------------------------------------------------}


Procedure TTestCaseReference.SetclassName(AIndex : Integer; const AValue : String); 

begin
  If (FclassName=AValue) then exit;
  FclassName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTestCaseReference.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTestCaseReference.SettestSuiteName(AIndex : Integer; const AValue : String); 

begin
  If (FtestSuiteName=AValue) then exit;
  FtestSuiteName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTestExecutionStep
  --------------------------------------------------------------------}


Procedure TTestExecutionStep.SettestIssues(AIndex : Integer; const AValue : TTestExecutionStepTypetestIssuesArray); 

begin
  If (FtestIssues=AValue) then exit;
  FtestIssues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTestExecutionStep.SettestSuiteOverviews(AIndex : Integer; const AValue : TTestExecutionStepTypetestSuiteOverviewsArray); 

begin
  If (FtestSuiteOverviews=AValue) then exit;
  FtestSuiteOverviews:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTestExecutionStep.SettestTiming(AIndex : Integer; const AValue : TTestTiming); 

begin
  If (FtestTiming=AValue) then exit;
  FtestTiming:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTestExecutionStep.SettoolExecution(AIndex : Integer; const AValue : TToolExecution); 

begin
  If (FtoolExecution=AValue) then exit;
  FtoolExecution:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTestExecutionStep.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'testissues' : SetLength(FtestIssues,ALength);
  'testsuiteoverviews' : SetLength(FtestSuiteOverviews,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTestIssue
  --------------------------------------------------------------------}


Procedure TTestIssue.SeterrorMessage(AIndex : Integer; const AValue : String); 

begin
  If (FerrorMessage=AValue) then exit;
  FerrorMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTestIssue.SetstackTrace(AIndex : Integer; const AValue : TStackTrace); 

begin
  If (FstackTrace=AValue) then exit;
  FstackTrace:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTestSuiteOverview
  --------------------------------------------------------------------}


Procedure TTestSuiteOverview.SeterrorCount(AIndex : Integer; const AValue : integer); 

begin
  If (FerrorCount=AValue) then exit;
  FerrorCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTestSuiteOverview.SetfailureCount(AIndex : Integer; const AValue : integer); 

begin
  If (FfailureCount=AValue) then exit;
  FfailureCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTestSuiteOverview.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTestSuiteOverview.SetskippedCount(AIndex : Integer; const AValue : integer); 

begin
  If (FskippedCount=AValue) then exit;
  FskippedCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTestSuiteOverview.SettotalCount(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalCount=AValue) then exit;
  FtotalCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTestSuiteOverview.SetxmlSource(AIndex : Integer; const AValue : TFileReference); 

begin
  If (FxmlSource=AValue) then exit;
  FxmlSource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTestTiming
  --------------------------------------------------------------------}


Procedure TTestTiming.SettestProcessDuration(AIndex : Integer; const AValue : TDuration); 

begin
  If (FtestProcessDuration=AValue) then exit;
  FtestProcessDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TThumbnail
  --------------------------------------------------------------------}


Procedure TThumbnail.SetcontentType(AIndex : Integer; const AValue : String); 

begin
  If (FcontentType=AValue) then exit;
  FcontentType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThumbnail.Setdata(AIndex : Integer; const AValue : String); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThumbnail.SetheightPx(AIndex : Integer; const AValue : integer); 

begin
  If (FheightPx=AValue) then exit;
  FheightPx:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThumbnail.SetwidthPx(AIndex : Integer; const AValue : integer); 

begin
  If (FwidthPx=AValue) then exit;
  FwidthPx:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTimestamp
  --------------------------------------------------------------------}


Procedure TTimestamp.Setnanos(AIndex : Integer; const AValue : integer); 

begin
  If (Fnanos=AValue) then exit;
  Fnanos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimestamp.Setseconds(AIndex : Integer; const AValue : String); 

begin
  If (Fseconds=AValue) then exit;
  Fseconds:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TToolExecution
  --------------------------------------------------------------------}


Procedure TToolExecution.SetcommandLineArguments(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FcommandLineArguments=AValue) then exit;
  FcommandLineArguments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TToolExecution.SetexitCode(AIndex : Integer; const AValue : TToolExitCode); 

begin
  If (FexitCode=AValue) then exit;
  FexitCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TToolExecution.SettoolLogs(AIndex : Integer; const AValue : TToolExecutionTypetoolLogsArray); 

begin
  If (FtoolLogs=AValue) then exit;
  FtoolLogs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TToolExecution.SettoolOutputs(AIndex : Integer; const AValue : TToolExecutionTypetoolOutputsArray); 

begin
  If (FtoolOutputs=AValue) then exit;
  FtoolOutputs:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TToolExecution.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'commandlinearguments' : SetLength(FcommandLineArguments,ALength);
  'toollogs' : SetLength(FtoolLogs,ALength);
  'tooloutputs' : SetLength(FtoolOutputs,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TToolExecutionStep
  --------------------------------------------------------------------}


Procedure TToolExecutionStep.SettoolExecution(AIndex : Integer; const AValue : TToolExecution); 

begin
  If (FtoolExecution=AValue) then exit;
  FtoolExecution:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TToolExitCode
  --------------------------------------------------------------------}


Procedure TToolExitCode.Setnumber(AIndex : Integer; const AValue : integer); 

begin
  If (Fnumber=AValue) then exit;
  Fnumber:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TToolOutputReference
  --------------------------------------------------------------------}


Procedure TToolOutputReference.SetcreationTime(AIndex : Integer; const AValue : TTimestamp); 

begin
  If (FcreationTime=AValue) then exit;
  FcreationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TToolOutputReference.Setoutput(AIndex : Integer; const AValue : TFileReference); 

begin
  If (Foutput=AValue) then exit;
  Foutput:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TToolOutputReference.SettestCase(AIndex : Integer; const AValue : TTestCaseReference); 

begin
  If (FtestCase=AValue) then exit;
  FtestCase:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProjectsHistoriesExecutionsStepsThumbnailsResource
  --------------------------------------------------------------------}


Class Function TProjectsHistoriesExecutionsStepsThumbnailsResource.ResourceName : String;

begin
  Result:='thumbnails';
end;

Class Function TProjectsHistoriesExecutionsStepsThumbnailsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtoolresultsAPI;
end;

Function TProjectsHistoriesExecutionsStepsThumbnailsResource.List(executionId: string; historyId: string; projectId: string; stepId: string; AQuery : string = '') : TListStepThumbnailsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{projectId}/histories/{historyId}/executions/{executionId}/steps/{stepId}/thumbnails';
  _Methodid   = 'toolresults.projects.histories.executions.steps.thumbnails.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['executionId',executionId,'historyId',historyId,'projectId',projectId,'stepId',stepId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListStepThumbnailsResponse) as TListStepThumbnailsResponse;
end;


Function TProjectsHistoriesExecutionsStepsThumbnailsResource.List(executionId: string; historyId: string; projectId: string; stepId: string; AQuery : TProjectsHistoriesExecutionsStepsThumbnailslistOptions) : TListStepThumbnailsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(executionId,historyId,projectId,stepId,_Q);
end;



{ --------------------------------------------------------------------
  TProjectsHistoriesExecutionsStepsResource
  --------------------------------------------------------------------}


Class Function TProjectsHistoriesExecutionsStepsResource.ResourceName : String;

begin
  Result:='steps';
end;

Class Function TProjectsHistoriesExecutionsStepsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtoolresultsAPI;
end;

Function TProjectsHistoriesExecutionsStepsResource.Create(executionId: string; historyId: string; projectId: string; aStep : TStep; AQuery : string = '') : TStep;

Const
  _HTTPMethod = 'POST';
  _Path       = '{projectId}/histories/{historyId}/executions/{executionId}/steps';
  _Methodid   = 'toolresults.projects.histories.executions.steps.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['executionId',executionId,'historyId',historyId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aStep,TStep) as TStep;
end;


Function TProjectsHistoriesExecutionsStepsResource.Create(executionId: string; historyId: string; projectId: string; aStep : TStep; AQuery : TProjectsHistoriesExecutionsStepscreateOptions) : TStep;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'requestId',AQuery.requestId);
  Result:=Create(executionId,historyId,projectId,aStep,_Q);
end;

Function TProjectsHistoriesExecutionsStepsResource.Get(executionId: string; historyId: string; projectId: string; stepId: string) : TStep;

Const
  _HTTPMethod = 'GET';
  _Path       = '{projectId}/histories/{historyId}/executions/{executionId}/steps/{stepId}';
  _Methodid   = 'toolresults.projects.histories.executions.steps.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['executionId',executionId,'historyId',historyId,'projectId',projectId,'stepId',stepId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TStep) as TStep;
end;

Function TProjectsHistoriesExecutionsStepsResource.List(executionId: string; historyId: string; projectId: string; AQuery : string = '') : TListStepsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{projectId}/histories/{historyId}/executions/{executionId}/steps';
  _Methodid   = 'toolresults.projects.histories.executions.steps.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['executionId',executionId,'historyId',historyId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListStepsResponse) as TListStepsResponse;
end;


Function TProjectsHistoriesExecutionsStepsResource.List(executionId: string; historyId: string; projectId: string; AQuery : TProjectsHistoriesExecutionsStepslistOptions) : TListStepsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(executionId,historyId,projectId,_Q);
end;

Function TProjectsHistoriesExecutionsStepsResource.Patch(executionId: string; historyId: string; projectId: string; stepId: string; aStep : TStep; AQuery : string = '') : TStep;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{projectId}/histories/{historyId}/executions/{executionId}/steps/{stepId}';
  _Methodid   = 'toolresults.projects.histories.executions.steps.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['executionId',executionId,'historyId',historyId,'projectId',projectId,'stepId',stepId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aStep,TStep) as TStep;
end;


Function TProjectsHistoriesExecutionsStepsResource.Patch(executionId: string; historyId: string; projectId: string; stepId: string; aStep : TStep; AQuery : TProjectsHistoriesExecutionsStepspatchOptions) : TStep;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'requestId',AQuery.requestId);
  Result:=Patch(executionId,historyId,projectId,stepId,aStep,_Q);
end;

Function TProjectsHistoriesExecutionsStepsResource.PublishXunitXmlFiles(executionId: string; historyId: string; projectId: string; stepId: string; aPublishXunitXmlFilesRequest : TPublishXunitXmlFilesRequest) : TStep;

Const
  _HTTPMethod = 'POST';
  _Path       = '{projectId}/histories/{historyId}/executions/{executionId}/steps/{stepId}:publishXunitXmlFiles';
  _Methodid   = 'toolresults.projects.histories.executions.steps.publishXunitXmlFiles';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['executionId',executionId,'historyId',historyId,'projectId',projectId,'stepId',stepId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aPublishXunitXmlFilesRequest,TStep) as TStep;
end;



Function TProjectsHistoriesExecutionsStepsResource.GetThumbnailsInstance : TProjectsHistoriesExecutionsStepsThumbnailsResource;

begin
  if (FThumbnailsInstance=Nil) then
    FThumbnailsInstance:=CreateThumbnailsResource;
  Result:=FThumbnailsInstance;
end;

Function TProjectsHistoriesExecutionsStepsResource.CreateThumbnailsResource : TProjectsHistoriesExecutionsStepsThumbnailsResource;

begin
  Result:=CreateThumbnailsResource(Self);
end;


Function TProjectsHistoriesExecutionsStepsResource.CreateThumbnailsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsStepsThumbnailsResource;

begin
  Result:=TProjectsHistoriesExecutionsStepsThumbnailsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TProjectsHistoriesExecutionsResource
  --------------------------------------------------------------------}


Class Function TProjectsHistoriesExecutionsResource.ResourceName : String;

begin
  Result:='executions';
end;

Class Function TProjectsHistoriesExecutionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtoolresultsAPI;
end;

Function TProjectsHistoriesExecutionsResource.Create(historyId: string; projectId: string; aExecution : TExecution; AQuery : string = '') : TExecution;

Const
  _HTTPMethod = 'POST';
  _Path       = '{projectId}/histories/{historyId}/executions';
  _Methodid   = 'toolresults.projects.histories.executions.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['historyId',historyId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aExecution,TExecution) as TExecution;
end;


Function TProjectsHistoriesExecutionsResource.Create(historyId: string; projectId: string; aExecution : TExecution; AQuery : TProjectsHistoriesExecutionscreateOptions) : TExecution;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'requestId',AQuery.requestId);
  Result:=Create(historyId,projectId,aExecution,_Q);
end;

Function TProjectsHistoriesExecutionsResource.Get(executionId: string; historyId: string; projectId: string) : TExecution;

Const
  _HTTPMethod = 'GET';
  _Path       = '{projectId}/histories/{historyId}/executions/{executionId}';
  _Methodid   = 'toolresults.projects.histories.executions.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['executionId',executionId,'historyId',historyId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TExecution) as TExecution;
end;

Function TProjectsHistoriesExecutionsResource.List(historyId: string; projectId: string; AQuery : string = '') : TListExecutionsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{projectId}/histories/{historyId}/executions';
  _Methodid   = 'toolresults.projects.histories.executions.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['historyId',historyId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListExecutionsResponse) as TListExecutionsResponse;
end;


Function TProjectsHistoriesExecutionsResource.List(historyId: string; projectId: string; AQuery : TProjectsHistoriesExecutionslistOptions) : TListExecutionsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(historyId,projectId,_Q);
end;

Function TProjectsHistoriesExecutionsResource.Patch(executionId: string; historyId: string; projectId: string; aExecution : TExecution; AQuery : string = '') : TExecution;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{projectId}/histories/{historyId}/executions/{executionId}';
  _Methodid   = 'toolresults.projects.histories.executions.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['executionId',executionId,'historyId',historyId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aExecution,TExecution) as TExecution;
end;


Function TProjectsHistoriesExecutionsResource.Patch(executionId: string; historyId: string; projectId: string; aExecution : TExecution; AQuery : TProjectsHistoriesExecutionspatchOptions) : TExecution;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'requestId',AQuery.requestId);
  Result:=Patch(executionId,historyId,projectId,aExecution,_Q);
end;



Function TProjectsHistoriesExecutionsResource.GetStepsThumbnailsInstance : TProjectsHistoriesExecutionsStepsThumbnailsResource;

begin
  if (FStepsThumbnailsInstance=Nil) then
    FStepsThumbnailsInstance:=CreateStepsThumbnailsResource;
  Result:=FStepsThumbnailsInstance;
end;

Function TProjectsHistoriesExecutionsResource.CreateStepsThumbnailsResource : TProjectsHistoriesExecutionsStepsThumbnailsResource;

begin
  Result:=CreateStepsThumbnailsResource(Self);
end;


Function TProjectsHistoriesExecutionsResource.CreateStepsThumbnailsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsStepsThumbnailsResource;

begin
  Result:=TProjectsHistoriesExecutionsStepsThumbnailsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsHistoriesExecutionsResource.GetStepsInstance : TProjectsHistoriesExecutionsStepsResource;

begin
  if (FStepsInstance=Nil) then
    FStepsInstance:=CreateStepsResource;
  Result:=FStepsInstance;
end;

Function TProjectsHistoriesExecutionsResource.CreateStepsResource : TProjectsHistoriesExecutionsStepsResource;

begin
  Result:=CreateStepsResource(Self);
end;


Function TProjectsHistoriesExecutionsResource.CreateStepsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsStepsResource;

begin
  Result:=TProjectsHistoriesExecutionsStepsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TProjectsHistoriesResource
  --------------------------------------------------------------------}


Class Function TProjectsHistoriesResource.ResourceName : String;

begin
  Result:='histories';
end;

Class Function TProjectsHistoriesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtoolresultsAPI;
end;

Function TProjectsHistoriesResource.Create(projectId: string; aHistory : THistory; AQuery : string = '') : THistory;

Const
  _HTTPMethod = 'POST';
  _Path       = '{projectId}/histories';
  _Methodid   = 'toolresults.projects.histories.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aHistory,THistory) as THistory;
end;


Function TProjectsHistoriesResource.Create(projectId: string; aHistory : THistory; AQuery : TProjectsHistoriescreateOptions) : THistory;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'requestId',AQuery.requestId);
  Result:=Create(projectId,aHistory,_Q);
end;

Function TProjectsHistoriesResource.Get(historyId: string; projectId: string) : THistory;

Const
  _HTTPMethod = 'GET';
  _Path       = '{projectId}/histories/{historyId}';
  _Methodid   = 'toolresults.projects.histories.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['historyId',historyId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,THistory) as THistory;
end;

Function TProjectsHistoriesResource.List(projectId: string; AQuery : string = '') : TListHistoriesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{projectId}/histories';
  _Methodid   = 'toolresults.projects.histories.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListHistoriesResponse) as TListHistoriesResponse;
end;


Function TProjectsHistoriesResource.List(projectId: string; AQuery : TProjectsHistorieslistOptions) : TListHistoriesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filterByName',AQuery.filterByName);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(projectId,_Q);
end;



Function TProjectsHistoriesResource.GetExecutionsStepsThumbnailsInstance : TProjectsHistoriesExecutionsStepsThumbnailsResource;

begin
  if (FExecutionsStepsThumbnailsInstance=Nil) then
    FExecutionsStepsThumbnailsInstance:=CreateExecutionsStepsThumbnailsResource;
  Result:=FExecutionsStepsThumbnailsInstance;
end;

Function TProjectsHistoriesResource.CreateExecutionsStepsThumbnailsResource : TProjectsHistoriesExecutionsStepsThumbnailsResource;

begin
  Result:=CreateExecutionsStepsThumbnailsResource(Self);
end;


Function TProjectsHistoriesResource.CreateExecutionsStepsThumbnailsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsStepsThumbnailsResource;

begin
  Result:=TProjectsHistoriesExecutionsStepsThumbnailsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsHistoriesResource.GetExecutionsStepsInstance : TProjectsHistoriesExecutionsStepsResource;

begin
  if (FExecutionsStepsInstance=Nil) then
    FExecutionsStepsInstance:=CreateExecutionsStepsResource;
  Result:=FExecutionsStepsInstance;
end;

Function TProjectsHistoriesResource.CreateExecutionsStepsResource : TProjectsHistoriesExecutionsStepsResource;

begin
  Result:=CreateExecutionsStepsResource(Self);
end;


Function TProjectsHistoriesResource.CreateExecutionsStepsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsStepsResource;

begin
  Result:=TProjectsHistoriesExecutionsStepsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsHistoriesResource.GetExecutionsInstance : TProjectsHistoriesExecutionsResource;

begin
  if (FExecutionsInstance=Nil) then
    FExecutionsInstance:=CreateExecutionsResource;
  Result:=FExecutionsInstance;
end;

Function TProjectsHistoriesResource.CreateExecutionsResource : TProjectsHistoriesExecutionsResource;

begin
  Result:=CreateExecutionsResource(Self);
end;


Function TProjectsHistoriesResource.CreateExecutionsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsResource;

begin
  Result:=TProjectsHistoriesExecutionsResource.Create(AOwner);
  Result.API:=Self.API;
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
  Result:=TtoolresultsAPI;
end;

Function TProjectsResource.GetSettings(projectId: string) : TProjectSettings;

Const
  _HTTPMethod = 'GET';
  _Path       = '{projectId}/settings';
  _Methodid   = 'toolresults.projects.getSettings';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TProjectSettings) as TProjectSettings;
end;

Function TProjectsResource.InitializeSettings(projectId: string) : TProjectSettings;

Const
  _HTTPMethod = 'POST';
  _Path       = '{projectId}:initializeSettings';
  _Methodid   = 'toolresults.projects.initializeSettings';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TProjectSettings) as TProjectSettings;
end;



Function TProjectsResource.GetHistoriesExecutionsStepsThumbnailsInstance : TProjectsHistoriesExecutionsStepsThumbnailsResource;

begin
  if (FHistoriesExecutionsStepsThumbnailsInstance=Nil) then
    FHistoriesExecutionsStepsThumbnailsInstance:=CreateHistoriesExecutionsStepsThumbnailsResource;
  Result:=FHistoriesExecutionsStepsThumbnailsInstance;
end;

Function TProjectsResource.CreateHistoriesExecutionsStepsThumbnailsResource : TProjectsHistoriesExecutionsStepsThumbnailsResource;

begin
  Result:=CreateHistoriesExecutionsStepsThumbnailsResource(Self);
end;


Function TProjectsResource.CreateHistoriesExecutionsStepsThumbnailsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsStepsThumbnailsResource;

begin
  Result:=TProjectsHistoriesExecutionsStepsThumbnailsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetHistoriesExecutionsStepsInstance : TProjectsHistoriesExecutionsStepsResource;

begin
  if (FHistoriesExecutionsStepsInstance=Nil) then
    FHistoriesExecutionsStepsInstance:=CreateHistoriesExecutionsStepsResource;
  Result:=FHistoriesExecutionsStepsInstance;
end;

Function TProjectsResource.CreateHistoriesExecutionsStepsResource : TProjectsHistoriesExecutionsStepsResource;

begin
  Result:=CreateHistoriesExecutionsStepsResource(Self);
end;


Function TProjectsResource.CreateHistoriesExecutionsStepsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsStepsResource;

begin
  Result:=TProjectsHistoriesExecutionsStepsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetHistoriesExecutionsInstance : TProjectsHistoriesExecutionsResource;

begin
  if (FHistoriesExecutionsInstance=Nil) then
    FHistoriesExecutionsInstance:=CreateHistoriesExecutionsResource;
  Result:=FHistoriesExecutionsInstance;
end;

Function TProjectsResource.CreateHistoriesExecutionsResource : TProjectsHistoriesExecutionsResource;

begin
  Result:=CreateHistoriesExecutionsResource(Self);
end;


Function TProjectsResource.CreateHistoriesExecutionsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsResource;

begin
  Result:=TProjectsHistoriesExecutionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetHistoriesInstance : TProjectsHistoriesResource;

begin
  if (FHistoriesInstance=Nil) then
    FHistoriesInstance:=CreateHistoriesResource;
  Result:=FHistoriesInstance;
end;

Function TProjectsResource.CreateHistoriesResource : TProjectsHistoriesResource;

begin
  Result:=CreateHistoriesResource(Self);
end;


Function TProjectsResource.CreateHistoriesResource(AOwner : TComponent) : TProjectsHistoriesResource;

begin
  Result:=TProjectsHistoriesResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TToolresultsAPI
  --------------------------------------------------------------------}

Class Function TToolresultsAPI.APIName : String;

begin
  Result:='toolresults';
end;

Class Function TToolresultsAPI.APIVersion : String;

begin
  Result:='v1beta3';
end;

Class Function TToolresultsAPI.APIRevision : String;

begin
  Result:='20160523';
end;

Class Function TToolresultsAPI.APIID : String;

begin
  Result:='toolresults:v1beta3';
end;

Class Function TToolresultsAPI.APITitle : String;

begin
  Result:='Cloud Tool Results API';
end;

Class Function TToolresultsAPI.APIDescription : String;

begin
  Result:='Reads and publishes results from Cloud Test Lab.';
end;

Class Function TToolresultsAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TToolresultsAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TToolresultsAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TToolresultsAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TToolresultsAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/cloud-test-lab/';
end;

Class Function TToolresultsAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TToolresultsAPI.APIbasePath : string;

begin
  Result:='/toolresults/v1beta3/projects/';
end;

Class Function TToolresultsAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/toolresults/v1beta3/projects/';
end;

Class Function TToolresultsAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TToolresultsAPI.APIservicePath : string;

begin
  Result:='toolresults/v1beta3/projects/';
end;

Class Function TToolresultsAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TToolresultsAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  
end;

Class Function TToolresultsAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TToolresultsAPI.RegisterAPIResources;

begin
  TAny.RegisterObject;
  TDuration.RegisterObject;
  TExecution.RegisterObject;
  TFailureDetail.RegisterObject;
  TFileReference.RegisterObject;
  THistory.RegisterObject;
  TImage.RegisterObject;
  TInconclusiveDetail.RegisterObject;
  TListExecutionsResponse.RegisterObject;
  TListHistoriesResponse.RegisterObject;
  TListStepThumbnailsResponse.RegisterObject;
  TListStepsResponse.RegisterObject;
  TOutcome.RegisterObject;
  TProjectSettings.RegisterObject;
  TPublishXunitXmlFilesRequest.RegisterObject;
  TSkippedDetail.RegisterObject;
  TStackTrace.RegisterObject;
  TStatus.RegisterObject;
  TStep.RegisterObject;
  TStepDimensionValueEntry.RegisterObject;
  TStepLabelsEntry.RegisterObject;
  TSuccessDetail.RegisterObject;
  TTestCaseReference.RegisterObject;
  TTestExecutionStep.RegisterObject;
  TTestIssue.RegisterObject;
  TTestSuiteOverview.RegisterObject;
  TTestTiming.RegisterObject;
  TThumbnail.RegisterObject;
  TTimestamp.RegisterObject;
  TToolExecution.RegisterObject;
  TToolExecutionStep.RegisterObject;
  TToolExitCode.RegisterObject;
  TToolOutputReference.RegisterObject;
end;


Function TToolresultsAPI.GetProjectsHistoriesExecutionsStepsThumbnailsInstance : TProjectsHistoriesExecutionsStepsThumbnailsResource;

begin
  if (FProjectsHistoriesExecutionsStepsThumbnailsInstance=Nil) then
    FProjectsHistoriesExecutionsStepsThumbnailsInstance:=CreateProjectsHistoriesExecutionsStepsThumbnailsResource;
  Result:=FProjectsHistoriesExecutionsStepsThumbnailsInstance;
end;

Function TToolresultsAPI.CreateProjectsHistoriesExecutionsStepsThumbnailsResource : TProjectsHistoriesExecutionsStepsThumbnailsResource;

begin
  Result:=CreateProjectsHistoriesExecutionsStepsThumbnailsResource(Self);
end;


Function TToolresultsAPI.CreateProjectsHistoriesExecutionsStepsThumbnailsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsStepsThumbnailsResource;

begin
  Result:=TProjectsHistoriesExecutionsStepsThumbnailsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TToolresultsAPI.GetProjectsHistoriesExecutionsStepsInstance : TProjectsHistoriesExecutionsStepsResource;

begin
  if (FProjectsHistoriesExecutionsStepsInstance=Nil) then
    FProjectsHistoriesExecutionsStepsInstance:=CreateProjectsHistoriesExecutionsStepsResource;
  Result:=FProjectsHistoriesExecutionsStepsInstance;
end;

Function TToolresultsAPI.CreateProjectsHistoriesExecutionsStepsResource : TProjectsHistoriesExecutionsStepsResource;

begin
  Result:=CreateProjectsHistoriesExecutionsStepsResource(Self);
end;


Function TToolresultsAPI.CreateProjectsHistoriesExecutionsStepsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsStepsResource;

begin
  Result:=TProjectsHistoriesExecutionsStepsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TToolresultsAPI.GetProjectsHistoriesExecutionsInstance : TProjectsHistoriesExecutionsResource;

begin
  if (FProjectsHistoriesExecutionsInstance=Nil) then
    FProjectsHistoriesExecutionsInstance:=CreateProjectsHistoriesExecutionsResource;
  Result:=FProjectsHistoriesExecutionsInstance;
end;

Function TToolresultsAPI.CreateProjectsHistoriesExecutionsResource : TProjectsHistoriesExecutionsResource;

begin
  Result:=CreateProjectsHistoriesExecutionsResource(Self);
end;


Function TToolresultsAPI.CreateProjectsHistoriesExecutionsResource(AOwner : TComponent) : TProjectsHistoriesExecutionsResource;

begin
  Result:=TProjectsHistoriesExecutionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TToolresultsAPI.GetProjectsHistoriesInstance : TProjectsHistoriesResource;

begin
  if (FProjectsHistoriesInstance=Nil) then
    FProjectsHistoriesInstance:=CreateProjectsHistoriesResource;
  Result:=FProjectsHistoriesInstance;
end;

Function TToolresultsAPI.CreateProjectsHistoriesResource : TProjectsHistoriesResource;

begin
  Result:=CreateProjectsHistoriesResource(Self);
end;


Function TToolresultsAPI.CreateProjectsHistoriesResource(AOwner : TComponent) : TProjectsHistoriesResource;

begin
  Result:=TProjectsHistoriesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TToolresultsAPI.GetProjectsInstance : TProjectsResource;

begin
  if (FProjectsInstance=Nil) then
    FProjectsInstance:=CreateProjectsResource;
  Result:=FProjectsInstance;
end;

Function TToolresultsAPI.CreateProjectsResource : TProjectsResource;

begin
  Result:=CreateProjectsResource(Self);
end;


Function TToolresultsAPI.CreateProjectsResource(AOwner : TComponent) : TProjectsResource;

begin
  Result:=TProjectsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TToolresultsAPI.RegisterAPI;
end.
