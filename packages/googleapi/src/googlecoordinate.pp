unit googlecoordinate;
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
//Generated on: 9-5-15 13:22:51
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TCustomField = class;
  TCustomFieldDef = class;
  TCustomFieldDefListResponse = class;
  TCustomFields = class;
  TEnumItemDef = class;
  TJob = class;
  TJobChange = class;
  TJobListResponse = class;
  TJobState = class;
  TLocation = class;
  TLocationListResponse = class;
  TLocationRecord = class;
  TSchedule = class;
  TTeam = class;
  TTeamListResponse = class;
  TTokenPagination = class;
  TWorker = class;
  TWorkerListResponse = class;
  TCustomFieldArray = Array of TCustomField;
  TCustomFieldDefArray = Array of TCustomFieldDef;
  TCustomFieldDefListResponseArray = Array of TCustomFieldDefListResponse;
  TCustomFieldsArray = Array of TCustomFields;
  TEnumItemDefArray = Array of TEnumItemDef;
  TJobArray = Array of TJob;
  TJobChangeArray = Array of TJobChange;
  TJobListResponseArray = Array of TJobListResponse;
  TJobStateArray = Array of TJobState;
  TLocationArray = Array of TLocation;
  TLocationListResponseArray = Array of TLocationListResponse;
  TLocationRecordArray = Array of TLocationRecord;
  TScheduleArray = Array of TSchedule;
  TTeamArray = Array of TTeam;
  TTeamListResponseArray = Array of TTeamListResponse;
  TTokenPaginationArray = Array of TTokenPagination;
  TWorkerArray = Array of TWorker;
  TWorkerListResponseArray = Array of TWorkerListResponse;
  //Anonymous types, using auto-generated names
  TCustomFieldDefTypeenumitemsArray = Array of TEnumItemDef;
  TCustomFieldDefListResponseTypeitemsArray = Array of TCustomFieldDef;
  TCustomFieldsTypecustomFieldArray = Array of TCustomField;
  TJobTypejobChangeArray = Array of TJobChange;
  TJobListResponseTypeitemsArray = Array of TJob;
  TLocationListResponseTypeitemsArray = Array of TLocationRecord;
  TTeamListResponseTypeitemsArray = Array of TTeam;
  TWorkerListResponseTypeitemsArray = Array of TWorker;
  
  { --------------------------------------------------------------------
    TCustomField
    --------------------------------------------------------------------}
  
  TCustomField = Class(TGoogleBaseObject)
  Private
    FcustomFieldId : String;
    Fkind : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure SetcustomFieldId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property customFieldId : String Index 0 Read FcustomFieldId Write SetcustomFieldId;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property value : String Index 16 Read Fvalue Write Setvalue;
  end;
  TCustomFieldClass = Class of TCustomField;
  
  { --------------------------------------------------------------------
    TCustomFieldDef
    --------------------------------------------------------------------}
  
  TCustomFieldDef = Class(TGoogleBaseObject)
  Private
    Fenabled : boolean;
    Fenumitems : TCustomFieldDefTypeenumitemsArray;
    Fid : String;
    Fkind : String;
    Fname : String;
    FrequiredForCheckout : boolean;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setenabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setenumitems(AIndex : Integer; AValue : TCustomFieldDefTypeenumitemsArray); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetrequiredForCheckout(AIndex : Integer; AValue : boolean); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property enabled : boolean Index 0 Read Fenabled Write Setenabled;
    Property enumitems : TCustomFieldDefTypeenumitemsArray Index 8 Read Fenumitems Write Setenumitems;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property name : String Index 32 Read Fname Write Setname;
    Property requiredForCheckout : boolean Index 40 Read FrequiredForCheckout Write SetrequiredForCheckout;
    Property _type : String Index 48 Read F_type Write Set_type;
  end;
  TCustomFieldDefClass = Class of TCustomFieldDef;
  
  { --------------------------------------------------------------------
    TCustomFieldDefListResponse
    --------------------------------------------------------------------}
  
  TCustomFieldDefListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TCustomFieldDefListResponseTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TCustomFieldDefListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TCustomFieldDefListResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TCustomFieldDefListResponseClass = Class of TCustomFieldDefListResponse;
  
  { --------------------------------------------------------------------
    TCustomFields
    --------------------------------------------------------------------}
  
  TCustomFields = Class(TGoogleBaseObject)
  Private
    FcustomField : TCustomFieldsTypecustomFieldArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetcustomField(AIndex : Integer; AValue : TCustomFieldsTypecustomFieldArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property customField : TCustomFieldsTypecustomFieldArray Index 0 Read FcustomField Write SetcustomField;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TCustomFieldsClass = Class of TCustomFields;
  
  { --------------------------------------------------------------------
    TEnumItemDef
    --------------------------------------------------------------------}
  
  TEnumItemDef = Class(TGoogleBaseObject)
  Private
    Factive : boolean;
    Fkind : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setactive(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property active : boolean Index 0 Read Factive Write Setactive;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property value : String Index 16 Read Fvalue Write Setvalue;
  end;
  TEnumItemDefClass = Class of TEnumItemDef;
  
  { --------------------------------------------------------------------
    TJob
    --------------------------------------------------------------------}
  
  TJob = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FjobChange : TJobTypejobChangeArray;
    Fkind : String;
    Fstate : TJobState;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetjobChange(AIndex : Integer; AValue : TJobTypejobChangeArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setstate(AIndex : Integer; AValue : TJobState); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property jobChange : TJobTypejobChangeArray Index 8 Read FjobChange Write SetjobChange;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property state : TJobState Index 24 Read Fstate Write Setstate;
  end;
  TJobClass = Class of TJob;
  
  { --------------------------------------------------------------------
    TJobChange
    --------------------------------------------------------------------}
  
  TJobChange = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fstate : TJobState;
    Ftimestamp : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setstate(AIndex : Integer; AValue : TJobState); virtual;
    Procedure Settimestamp(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property state : TJobState Index 8 Read Fstate Write Setstate;
    Property timestamp : String Index 16 Read Ftimestamp Write Settimestamp;
  end;
  TJobChangeClass = Class of TJobChange;
  
  { --------------------------------------------------------------------
    TJobListResponse
    --------------------------------------------------------------------}
  
  TJobListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TJobListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TJobListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TJobListResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TJobListResponseClass = Class of TJobListResponse;
  
  { --------------------------------------------------------------------
    TJobState
    --------------------------------------------------------------------}
  
  TJobState = Class(TGoogleBaseObject)
  Private
    Fassignee : String;
    FcustomFields : TCustomFields;
    FcustomerName : String;
    FcustomerPhoneNumber : String;
    Fkind : String;
    Flocation : TLocation;
    Fnote : TStringArray;
    Fprogress : String;
    Ftitle : String;
  Protected
    //Property setters
    Procedure Setassignee(AIndex : Integer; AValue : String); virtual;
    Procedure SetcustomFields(AIndex : Integer; AValue : TCustomFields); virtual;
    Procedure SetcustomerName(AIndex : Integer; AValue : String); virtual;
    Procedure SetcustomerPhoneNumber(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : TLocation); virtual;
    Procedure Setnote(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setprogress(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property assignee : String Index 0 Read Fassignee Write Setassignee;
    Property customFields : TCustomFields Index 8 Read FcustomFields Write SetcustomFields;
    Property customerName : String Index 16 Read FcustomerName Write SetcustomerName;
    Property customerPhoneNumber : String Index 24 Read FcustomerPhoneNumber Write SetcustomerPhoneNumber;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property location : TLocation Index 40 Read Flocation Write Setlocation;
    Property note : TStringArray Index 48 Read Fnote Write Setnote;
    Property progress : String Index 56 Read Fprogress Write Setprogress;
    Property title : String Index 64 Read Ftitle Write Settitle;
  end;
  TJobStateClass = Class of TJobState;
  
  { --------------------------------------------------------------------
    TLocation
    --------------------------------------------------------------------}
  
  TLocation = Class(TGoogleBaseObject)
  Private
    FaddressLine : TStringArray;
    Fkind : String;
    Flat : double;
    Flng : double;
  Protected
    //Property setters
    Procedure SetaddressLine(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlat(AIndex : Integer; AValue : double); virtual;
    Procedure Setlng(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property addressLine : TStringArray Index 0 Read FaddressLine Write SetaddressLine;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property lat : double Index 16 Read Flat Write Setlat;
    Property lng : double Index 24 Read Flng Write Setlng;
  end;
  TLocationClass = Class of TLocation;
  
  { --------------------------------------------------------------------
    TLocationListResponse
    --------------------------------------------------------------------}
  
  TLocationListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TLocationListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FtokenPagination : TTokenPagination;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TLocationListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SettokenPagination(AIndex : Integer; AValue : TTokenPagination); virtual;
  Public
  Published
    Property items : TLocationListResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
    Property tokenPagination : TTokenPagination Index 24 Read FtokenPagination Write SettokenPagination;
  end;
  TLocationListResponseClass = Class of TLocationListResponse;
  
  { --------------------------------------------------------------------
    TLocationRecord
    --------------------------------------------------------------------}
  
  TLocationRecord = Class(TGoogleBaseObject)
  Private
    FcollectionTime : String;
    FconfidenceRadius : double;
    Fkind : String;
    Flatitude : double;
    Flongitude : double;
  Protected
    //Property setters
    Procedure SetcollectionTime(AIndex : Integer; AValue : String); virtual;
    Procedure SetconfidenceRadius(AIndex : Integer; AValue : double); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlatitude(AIndex : Integer; AValue : double); virtual;
    Procedure Setlongitude(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property collectionTime : String Index 0 Read FcollectionTime Write SetcollectionTime;
    Property confidenceRadius : double Index 8 Read FconfidenceRadius Write SetconfidenceRadius;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property latitude : double Index 24 Read Flatitude Write Setlatitude;
    Property longitude : double Index 32 Read Flongitude Write Setlongitude;
  end;
  TLocationRecordClass = Class of TLocationRecord;
  
  { --------------------------------------------------------------------
    TSchedule
    --------------------------------------------------------------------}
  
  TSchedule = Class(TGoogleBaseObject)
  Private
    FallDay : boolean;
    Fduration : String;
    FendTime : String;
    Fkind : String;
    FstartTime : String;
  Protected
    //Property setters
    Procedure SetallDay(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setduration(AIndex : Integer; AValue : String); virtual;
    Procedure SetendTime(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property allDay : boolean Index 0 Read FallDay Write SetallDay;
    Property duration : String Index 8 Read Fduration Write Setduration;
    Property endTime : String Index 16 Read FendTime Write SetendTime;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property startTime : String Index 32 Read FstartTime Write SetstartTime;
  end;
  TScheduleClass = Class of TSchedule;
  
  { --------------------------------------------------------------------
    TTeam
    --------------------------------------------------------------------}
  
  TTeam = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property name : String Index 16 Read Fname Write Setname;
  end;
  TTeamClass = Class of TTeam;
  
  { --------------------------------------------------------------------
    TTeamListResponse
    --------------------------------------------------------------------}
  
  TTeamListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TTeamListResponseTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TTeamListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TTeamListResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TTeamListResponseClass = Class of TTeamListResponse;
  
  { --------------------------------------------------------------------
    TTokenPagination
    --------------------------------------------------------------------}
  
  TTokenPagination = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    FpreviousPageToken : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetpreviousPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property previousPageToken : String Index 16 Read FpreviousPageToken Write SetpreviousPageToken;
  end;
  TTokenPaginationClass = Class of TTokenPagination;
  
  { --------------------------------------------------------------------
    TWorker
    --------------------------------------------------------------------}
  
  TWorker = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TWorkerClass = Class of TWorker;
  
  { --------------------------------------------------------------------
    TWorkerListResponse
    --------------------------------------------------------------------}
  
  TWorkerListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TWorkerListResponseTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TWorkerListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TWorkerListResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TWorkerListResponseClass = Class of TWorkerListResponse;
  
  { --------------------------------------------------------------------
    TCustomFieldDefResource
    --------------------------------------------------------------------}
  
  TCustomFieldDefResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(teamId: string) : TCustomFieldDefListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TJobsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TJobsResource, method Insert
  
  TJobsInsertOptions = Record
    address : String;
    assignee : String;
    customField : String;
    customerName : String;
    customerPhoneNumber : String;
    lat : double;
    lng : double;
    note : String;
    title : String;
  end;
  
  
  //Optional query Options for TJobsResource, method List
  
  TJobsListOptions = Record
    maxResults : integer;
    minModifiedTimestampMs : String;
    pageToken : String;
  end;
  
  
  //Optional query Options for TJobsResource, method Patch
  
  TJobsPatchOptions = Record
    address : String;
    assignee : String;
    customField : String;
    customerName : String;
    customerPhoneNumber : String;
    lat : double;
    lng : double;
    note : String;
    progress : String;
    title : String;
  end;
  
  
  //Optional query Options for TJobsResource, method Update
  
  TJobsUpdateOptions = Record
    address : String;
    assignee : String;
    customField : String;
    customerName : String;
    customerPhoneNumber : String;
    lat : double;
    lng : double;
    note : String;
    progress : String;
    title : String;
  end;
  
  TJobsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(jobId: string; teamId: string) : TJob;
    Function Insert(teamId: string; aJob : TJob; AQuery : string  = '') : TJob;
    Function Insert(teamId: string; aJob : TJob; AQuery : TJobsinsertOptions) : TJob;
    Function List(teamId: string; AQuery : string  = '') : TJobListResponse;
    Function List(teamId: string; AQuery : TJobslistOptions) : TJobListResponse;
    Function Patch(jobId: string; teamId: string; aJob : TJob; AQuery : string  = '') : TJob;
    Function Patch(jobId: string; teamId: string; aJob : TJob; AQuery : TJobspatchOptions) : TJob;
    Function Update(jobId: string; teamId: string; aJob : TJob; AQuery : string  = '') : TJob;
    Function Update(jobId: string; teamId: string; aJob : TJob; AQuery : TJobsupdateOptions) : TJob;
  end;
  
  
  { --------------------------------------------------------------------
    TLocationResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TLocationResource, method List
  
  TLocationListOptions = Record
    maxResults : integer;
    pageToken : String;
    startTimestampMs : String;
  end;
  
  TLocationResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(teamId: string; workerEmail: string; AQuery : string  = '') : TLocationListResponse;
    Function List(teamId: string; workerEmail: string; AQuery : TLocationlistOptions) : TLocationListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TScheduleResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TScheduleResource, method Patch
  
  TSchedulePatchOptions = Record
    allDay : boolean;
    duration : String;
    endTime : String;
    startTime : String;
  end;
  
  
  //Optional query Options for TScheduleResource, method Update
  
  TScheduleUpdateOptions = Record
    allDay : boolean;
    duration : String;
    endTime : String;
    startTime : String;
  end;
  
  TScheduleResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(jobId: string; teamId: string) : TSchedule;
    Function Patch(jobId: string; teamId: string; aSchedule : TSchedule; AQuery : string  = '') : TSchedule;
    Function Patch(jobId: string; teamId: string; aSchedule : TSchedule; AQuery : TSchedulepatchOptions) : TSchedule;
    Function Update(jobId: string; teamId: string; aSchedule : TSchedule; AQuery : string  = '') : TSchedule;
    Function Update(jobId: string; teamId: string; aSchedule : TSchedule; AQuery : TScheduleupdateOptions) : TSchedule;
  end;
  
  
  { --------------------------------------------------------------------
    TTeamResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTeamResource, method List
  
  TTeamListOptions = Record
    admin : boolean;
    dispatcher : boolean;
    worker : boolean;
  end;
  
  TTeamResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TTeamListResponse;
    Function List(AQuery : TTeamlistOptions) : TTeamListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TWorkerResource
    --------------------------------------------------------------------}
  
  TWorkerResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(teamId: string) : TWorkerListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TCoordinateAPI
    --------------------------------------------------------------------}
  
  TCoordinateAPI = Class(TGoogleAPI)
  Private
    FCustomFieldDefInstance : TCustomFieldDefResource;
    FJobsInstance : TJobsResource;
    FLocationInstance : TLocationResource;
    FScheduleInstance : TScheduleResource;
    FTeamInstance : TTeamResource;
    FWorkerInstance : TWorkerResource;
    Function GetCustomFieldDefInstance : TCustomFieldDefResource;virtual;
    Function GetJobsInstance : TJobsResource;virtual;
    Function GetLocationInstance : TLocationResource;virtual;
    Function GetScheduleInstance : TScheduleResource;virtual;
    Function GetTeamInstance : TTeamResource;virtual;
    Function GetWorkerInstance : TWorkerResource;virtual;
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
    Function CreateCustomFieldDefResource(AOwner : TComponent) : TCustomFieldDefResource;virtual;overload;
    Function CreateCustomFieldDefResource : TCustomFieldDefResource;virtual;overload;
    Function CreateJobsResource(AOwner : TComponent) : TJobsResource;virtual;overload;
    Function CreateJobsResource : TJobsResource;virtual;overload;
    Function CreateLocationResource(AOwner : TComponent) : TLocationResource;virtual;overload;
    Function CreateLocationResource : TLocationResource;virtual;overload;
    Function CreateScheduleResource(AOwner : TComponent) : TScheduleResource;virtual;overload;
    Function CreateScheduleResource : TScheduleResource;virtual;overload;
    Function CreateTeamResource(AOwner : TComponent) : TTeamResource;virtual;overload;
    Function CreateTeamResource : TTeamResource;virtual;overload;
    Function CreateWorkerResource(AOwner : TComponent) : TWorkerResource;virtual;overload;
    Function CreateWorkerResource : TWorkerResource;virtual;overload;
    //Add default on-demand instances for resources
    Property CustomFieldDefResource : TCustomFieldDefResource Read GetCustomFieldDefInstance;
    Property JobsResource : TJobsResource Read GetJobsInstance;
    Property LocationResource : TLocationResource Read GetLocationInstance;
    Property ScheduleResource : TScheduleResource Read GetScheduleInstance;
    Property TeamResource : TTeamResource Read GetTeamInstance;
    Property WorkerResource : TWorkerResource Read GetWorkerInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TCustomField
  --------------------------------------------------------------------}


Procedure TCustomField.SetcustomFieldId(AIndex : Integer; AValue : String); 

begin
  If (FcustomFieldId=AValue) then exit;
  FcustomFieldId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomField.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomField.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCustomFieldDef
  --------------------------------------------------------------------}


Procedure TCustomFieldDef.Setenabled(AIndex : Integer; AValue : boolean); 

begin
  If (Fenabled=AValue) then exit;
  Fenabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomFieldDef.Setenumitems(AIndex : Integer; AValue : TCustomFieldDefTypeenumitemsArray); 

begin
  If (Fenumitems=AValue) then exit;
  Fenumitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomFieldDef.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomFieldDef.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomFieldDef.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomFieldDef.SetrequiredForCheckout(AIndex : Integer; AValue : boolean); 

begin
  If (FrequiredForCheckout=AValue) then exit;
  FrequiredForCheckout:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomFieldDef.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCustomFieldDef.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TCustomFieldDefListResponse
  --------------------------------------------------------------------}


Procedure TCustomFieldDefListResponse.Setitems(AIndex : Integer; AValue : TCustomFieldDefListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomFieldDefListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCustomFields
  --------------------------------------------------------------------}


Procedure TCustomFields.SetcustomField(AIndex : Integer; AValue : TCustomFieldsTypecustomFieldArray); 

begin
  If (FcustomField=AValue) then exit;
  FcustomField:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomFields.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEnumItemDef
  --------------------------------------------------------------------}


Procedure TEnumItemDef.Setactive(AIndex : Integer; AValue : boolean); 

begin
  If (Factive=AValue) then exit;
  Factive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnumItemDef.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnumItemDef.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJob
  --------------------------------------------------------------------}


Procedure TJob.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetjobChange(AIndex : Integer; AValue : TJobTypejobChangeArray); 

begin
  If (FjobChange=AValue) then exit;
  FjobChange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setstate(AIndex : Integer; AValue : TJobState); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobChange
  --------------------------------------------------------------------}


Procedure TJobChange.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobChange.Setstate(AIndex : Integer; AValue : TJobState); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobChange.Settimestamp(AIndex : Integer; AValue : String); 

begin
  If (Ftimestamp=AValue) then exit;
  Ftimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobListResponse
  --------------------------------------------------------------------}


Procedure TJobListResponse.Setitems(AIndex : Integer; AValue : TJobListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobState
  --------------------------------------------------------------------}


Procedure TJobState.Setassignee(AIndex : Integer; AValue : String); 

begin
  If (Fassignee=AValue) then exit;
  Fassignee:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobState.SetcustomFields(AIndex : Integer; AValue : TCustomFields); 

begin
  If (FcustomFields=AValue) then exit;
  FcustomFields:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobState.SetcustomerName(AIndex : Integer; AValue : String); 

begin
  If (FcustomerName=AValue) then exit;
  FcustomerName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobState.SetcustomerPhoneNumber(AIndex : Integer; AValue : String); 

begin
  If (FcustomerPhoneNumber=AValue) then exit;
  FcustomerPhoneNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobState.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobState.Setlocation(AIndex : Integer; AValue : TLocation); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobState.Setnote(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fnote=AValue) then exit;
  Fnote:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobState.Setprogress(AIndex : Integer; AValue : String); 

begin
  If (Fprogress=AValue) then exit;
  Fprogress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobState.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLocation
  --------------------------------------------------------------------}


Procedure TLocation.SetaddressLine(AIndex : Integer; AValue : TStringArray); 

begin
  If (FaddressLine=AValue) then exit;
  FaddressLine:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocation.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocation.Setlat(AIndex : Integer; AValue : double); 

begin
  If (Flat=AValue) then exit;
  Flat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocation.Setlng(AIndex : Integer; AValue : double); 

begin
  If (Flng=AValue) then exit;
  Flng:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLocationListResponse
  --------------------------------------------------------------------}


Procedure TLocationListResponse.Setitems(AIndex : Integer; AValue : TLocationListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocationListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocationListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocationListResponse.SettokenPagination(AIndex : Integer; AValue : TTokenPagination); 

begin
  If (FtokenPagination=AValue) then exit;
  FtokenPagination:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLocationRecord
  --------------------------------------------------------------------}


Procedure TLocationRecord.SetcollectionTime(AIndex : Integer; AValue : String); 

begin
  If (FcollectionTime=AValue) then exit;
  FcollectionTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocationRecord.SetconfidenceRadius(AIndex : Integer; AValue : double); 

begin
  If (FconfidenceRadius=AValue) then exit;
  FconfidenceRadius:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocationRecord.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocationRecord.Setlatitude(AIndex : Integer; AValue : double); 

begin
  If (Flatitude=AValue) then exit;
  Flatitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocationRecord.Setlongitude(AIndex : Integer; AValue : double); 

begin
  If (Flongitude=AValue) then exit;
  Flongitude:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSchedule
  --------------------------------------------------------------------}


Procedure TSchedule.SetallDay(AIndex : Integer; AValue : boolean); 

begin
  If (FallDay=AValue) then exit;
  FallDay:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSchedule.Setduration(AIndex : Integer; AValue : String); 

begin
  If (Fduration=AValue) then exit;
  Fduration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSchedule.SetendTime(AIndex : Integer; AValue : String); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSchedule.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSchedule.SetstartTime(AIndex : Integer; AValue : String); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTeam
  --------------------------------------------------------------------}


Procedure TTeam.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTeam.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTeam.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTeamListResponse
  --------------------------------------------------------------------}


Procedure TTeamListResponse.Setitems(AIndex : Integer; AValue : TTeamListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTeamListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTokenPagination
  --------------------------------------------------------------------}


Procedure TTokenPagination.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTokenPagination.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTokenPagination.SetpreviousPageToken(AIndex : Integer; AValue : String); 

begin
  If (FpreviousPageToken=AValue) then exit;
  FpreviousPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWorker
  --------------------------------------------------------------------}


Procedure TWorker.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorker.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWorkerListResponse
  --------------------------------------------------------------------}


Procedure TWorkerListResponse.Setitems(AIndex : Integer; AValue : TWorkerListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCustomFieldDefResource
  --------------------------------------------------------------------}


Class Function TCustomFieldDefResource.ResourceName : String;

begin
  Result:='customFieldDef';
end;

Class Function TCustomFieldDefResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcoordinateAPI;
end;

Function TCustomFieldDefResource.List(teamId: string) : TCustomFieldDefListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'teams/{teamId}/custom_fields';
  _Methodid   = 'coordinate.customFieldDef.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['teamId',teamId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCustomFieldDefListResponse) as TCustomFieldDefListResponse;
end;



{ --------------------------------------------------------------------
  TJobsResource
  --------------------------------------------------------------------}


Class Function TJobsResource.ResourceName : String;

begin
  Result:='jobs';
end;

Class Function TJobsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcoordinateAPI;
end;

Function TJobsResource.Get(jobId: string; teamId: string) : TJob;

Const
  _HTTPMethod = 'GET';
  _Path       = 'teams/{teamId}/jobs/{jobId}';
  _Methodid   = 'coordinate.jobs.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobId',jobId,'teamId',teamId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TJob) as TJob;
end;

Function TJobsResource.Insert(teamId: string; aJob : TJob; AQuery : string = '') : TJob;

Const
  _HTTPMethod = 'POST';
  _Path       = 'teams/{teamId}/jobs';
  _Methodid   = 'coordinate.jobs.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['teamId',teamId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aJob,TJob) as TJob;
end;


Function TJobsResource.Insert(teamId: string; aJob : TJob; AQuery : TJobsinsertOptions) : TJob;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'address',AQuery.address);
  AddToQuery(_Q,'assignee',AQuery.assignee);
  AddToQuery(_Q,'customField',AQuery.customField);
  AddToQuery(_Q,'customerName',AQuery.customerName);
  AddToQuery(_Q,'customerPhoneNumber',AQuery.customerPhoneNumber);
  AddToQuery(_Q,'lat',AQuery.lat);
  AddToQuery(_Q,'lng',AQuery.lng);
  AddToQuery(_Q,'note',AQuery.note);
  AddToQuery(_Q,'title',AQuery.title);
  Result:=Insert(teamId,aJob,_Q);
end;

Function TJobsResource.List(teamId: string; AQuery : string = '') : TJobListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'teams/{teamId}/jobs';
  _Methodid   = 'coordinate.jobs.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['teamId',teamId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TJobListResponse) as TJobListResponse;
end;


Function TJobsResource.List(teamId: string; AQuery : TJobslistOptions) : TJobListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'minModifiedTimestampMs',AQuery.minModifiedTimestampMs);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(teamId,_Q);
end;

Function TJobsResource.Patch(jobId: string; teamId: string; aJob : TJob; AQuery : string = '') : TJob;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'teams/{teamId}/jobs/{jobId}';
  _Methodid   = 'coordinate.jobs.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobId',jobId,'teamId',teamId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aJob,TJob) as TJob;
end;


Function TJobsResource.Patch(jobId: string; teamId: string; aJob : TJob; AQuery : TJobspatchOptions) : TJob;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'address',AQuery.address);
  AddToQuery(_Q,'assignee',AQuery.assignee);
  AddToQuery(_Q,'customField',AQuery.customField);
  AddToQuery(_Q,'customerName',AQuery.customerName);
  AddToQuery(_Q,'customerPhoneNumber',AQuery.customerPhoneNumber);
  AddToQuery(_Q,'lat',AQuery.lat);
  AddToQuery(_Q,'lng',AQuery.lng);
  AddToQuery(_Q,'note',AQuery.note);
  AddToQuery(_Q,'progress',AQuery.progress);
  AddToQuery(_Q,'title',AQuery.title);
  Result:=Patch(jobId,teamId,aJob,_Q);
end;

Function TJobsResource.Update(jobId: string; teamId: string; aJob : TJob; AQuery : string = '') : TJob;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'teams/{teamId}/jobs/{jobId}';
  _Methodid   = 'coordinate.jobs.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobId',jobId,'teamId',teamId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aJob,TJob) as TJob;
end;


Function TJobsResource.Update(jobId: string; teamId: string; aJob : TJob; AQuery : TJobsupdateOptions) : TJob;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'address',AQuery.address);
  AddToQuery(_Q,'assignee',AQuery.assignee);
  AddToQuery(_Q,'customField',AQuery.customField);
  AddToQuery(_Q,'customerName',AQuery.customerName);
  AddToQuery(_Q,'customerPhoneNumber',AQuery.customerPhoneNumber);
  AddToQuery(_Q,'lat',AQuery.lat);
  AddToQuery(_Q,'lng',AQuery.lng);
  AddToQuery(_Q,'note',AQuery.note);
  AddToQuery(_Q,'progress',AQuery.progress);
  AddToQuery(_Q,'title',AQuery.title);
  Result:=Update(jobId,teamId,aJob,_Q);
end;



{ --------------------------------------------------------------------
  TLocationResource
  --------------------------------------------------------------------}


Class Function TLocationResource.ResourceName : String;

begin
  Result:='location';
end;

Class Function TLocationResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcoordinateAPI;
end;

Function TLocationResource.List(teamId: string; workerEmail: string; AQuery : string = '') : TLocationListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'teams/{teamId}/workers/{workerEmail}/locations';
  _Methodid   = 'coordinate.location.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['teamId',teamId,'workerEmail',workerEmail]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TLocationListResponse) as TLocationListResponse;
end;


Function TLocationResource.List(teamId: string; workerEmail: string; AQuery : TLocationlistOptions) : TLocationListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'startTimestampMs',AQuery.startTimestampMs);
  Result:=List(teamId,workerEmail,_Q);
end;



{ --------------------------------------------------------------------
  TScheduleResource
  --------------------------------------------------------------------}


Class Function TScheduleResource.ResourceName : String;

begin
  Result:='schedule';
end;

Class Function TScheduleResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcoordinateAPI;
end;

Function TScheduleResource.Get(jobId: string; teamId: string) : TSchedule;

Const
  _HTTPMethod = 'GET';
  _Path       = 'teams/{teamId}/jobs/{jobId}/schedule';
  _Methodid   = 'coordinate.schedule.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobId',jobId,'teamId',teamId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TSchedule) as TSchedule;
end;

Function TScheduleResource.Patch(jobId: string; teamId: string; aSchedule : TSchedule; AQuery : string = '') : TSchedule;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'teams/{teamId}/jobs/{jobId}/schedule';
  _Methodid   = 'coordinate.schedule.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobId',jobId,'teamId',teamId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aSchedule,TSchedule) as TSchedule;
end;


Function TScheduleResource.Patch(jobId: string; teamId: string; aSchedule : TSchedule; AQuery : TSchedulepatchOptions) : TSchedule;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'allDay',AQuery.allDay);
  AddToQuery(_Q,'duration',AQuery.duration);
  AddToQuery(_Q,'endTime',AQuery.endTime);
  AddToQuery(_Q,'startTime',AQuery.startTime);
  Result:=Patch(jobId,teamId,aSchedule,_Q);
end;

Function TScheduleResource.Update(jobId: string; teamId: string; aSchedule : TSchedule; AQuery : string = '') : TSchedule;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'teams/{teamId}/jobs/{jobId}/schedule';
  _Methodid   = 'coordinate.schedule.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobId',jobId,'teamId',teamId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aSchedule,TSchedule) as TSchedule;
end;


Function TScheduleResource.Update(jobId: string; teamId: string; aSchedule : TSchedule; AQuery : TScheduleupdateOptions) : TSchedule;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'allDay',AQuery.allDay);
  AddToQuery(_Q,'duration',AQuery.duration);
  AddToQuery(_Q,'endTime',AQuery.endTime);
  AddToQuery(_Q,'startTime',AQuery.startTime);
  Result:=Update(jobId,teamId,aSchedule,_Q);
end;



{ --------------------------------------------------------------------
  TTeamResource
  --------------------------------------------------------------------}


Class Function TTeamResource.ResourceName : String;

begin
  Result:='team';
end;

Class Function TTeamResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcoordinateAPI;
end;

Function TTeamResource.List(AQuery : string = '') : TTeamListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'teams';
  _Methodid   = 'coordinate.team.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TTeamListResponse) as TTeamListResponse;
end;


Function TTeamResource.List(AQuery : TTeamlistOptions) : TTeamListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'admin',AQuery.admin);
  AddToQuery(_Q,'dispatcher',AQuery.dispatcher);
  AddToQuery(_Q,'worker',AQuery.worker);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TWorkerResource
  --------------------------------------------------------------------}


Class Function TWorkerResource.ResourceName : String;

begin
  Result:='worker';
end;

Class Function TWorkerResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcoordinateAPI;
end;

Function TWorkerResource.List(teamId: string) : TWorkerListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'teams/{teamId}/workers';
  _Methodid   = 'coordinate.worker.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['teamId',teamId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TWorkerListResponse) as TWorkerListResponse;
end;



{ --------------------------------------------------------------------
  TCoordinateAPI
  --------------------------------------------------------------------}

Class Function TCoordinateAPI.APIName : String;

begin
  Result:='coordinate';
end;

Class Function TCoordinateAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TCoordinateAPI.APIRevision : String;

begin
  Result:='20141215';
end;

Class Function TCoordinateAPI.APIID : String;

begin
  Result:='coordinate:v1';
end;

Class Function TCoordinateAPI.APITitle : String;

begin
  Result:='Google Maps Coordinate API';
end;

Class Function TCoordinateAPI.APIDescription : String;

begin
  Result:='Lets you view and manage jobs in a Coordinate team.';
end;

Class Function TCoordinateAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TCoordinateAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TCoordinateAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TCoordinateAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TCoordinateAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/coordinate/';
end;

Class Function TCoordinateAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TCoordinateAPI.APIbasePath : string;

begin
  Result:='/coordinate/v1/';
end;

Class Function TCoordinateAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/coordinate/v1/';
end;

Class Function TCoordinateAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TCoordinateAPI.APIservicePath : string;

begin
  Result:='coordinate/v1/';
end;

Class Function TCoordinateAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TCoordinateAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/coordinate';
  Result[0].Description:='View and manage your Google Maps Coordinate jobs';
  Result[1].Name:='https://www.googleapis.com/auth/coordinate.readonly';
  Result[1].Description:='View your Google Coordinate jobs';
  
end;

Class Function TCoordinateAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TCoordinateAPI.RegisterAPIResources;

begin
  TCustomField.RegisterObject;
  TCustomFieldDef.RegisterObject;
  TCustomFieldDefListResponse.RegisterObject;
  TCustomFields.RegisterObject;
  TEnumItemDef.RegisterObject;
  TJob.RegisterObject;
  TJobChange.RegisterObject;
  TJobListResponse.RegisterObject;
  TJobState.RegisterObject;
  TLocation.RegisterObject;
  TLocationListResponse.RegisterObject;
  TLocationRecord.RegisterObject;
  TSchedule.RegisterObject;
  TTeam.RegisterObject;
  TTeamListResponse.RegisterObject;
  TTokenPagination.RegisterObject;
  TWorker.RegisterObject;
  TWorkerListResponse.RegisterObject;
end;


Function TCoordinateAPI.GetCustomFieldDefInstance : TCustomFieldDefResource;

begin
  if (FCustomFieldDefInstance=Nil) then
    FCustomFieldDefInstance:=CreateCustomFieldDefResource;
  Result:=FCustomFieldDefInstance;
end;

Function TCoordinateAPI.CreateCustomFieldDefResource : TCustomFieldDefResource;

begin
  Result:=CreateCustomFieldDefResource(Self);
end;


Function TCoordinateAPI.CreateCustomFieldDefResource(AOwner : TComponent) : TCustomFieldDefResource;

begin
  Result:=TCustomFieldDefResource.Create(AOwner);
  Result.API:=Self;
end;



Function TCoordinateAPI.GetJobsInstance : TJobsResource;

begin
  if (FJobsInstance=Nil) then
    FJobsInstance:=CreateJobsResource;
  Result:=FJobsInstance;
end;

Function TCoordinateAPI.CreateJobsResource : TJobsResource;

begin
  Result:=CreateJobsResource(Self);
end;


Function TCoordinateAPI.CreateJobsResource(AOwner : TComponent) : TJobsResource;

begin
  Result:=TJobsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TCoordinateAPI.GetLocationInstance : TLocationResource;

begin
  if (FLocationInstance=Nil) then
    FLocationInstance:=CreateLocationResource;
  Result:=FLocationInstance;
end;

Function TCoordinateAPI.CreateLocationResource : TLocationResource;

begin
  Result:=CreateLocationResource(Self);
end;


Function TCoordinateAPI.CreateLocationResource(AOwner : TComponent) : TLocationResource;

begin
  Result:=TLocationResource.Create(AOwner);
  Result.API:=Self;
end;



Function TCoordinateAPI.GetScheduleInstance : TScheduleResource;

begin
  if (FScheduleInstance=Nil) then
    FScheduleInstance:=CreateScheduleResource;
  Result:=FScheduleInstance;
end;

Function TCoordinateAPI.CreateScheduleResource : TScheduleResource;

begin
  Result:=CreateScheduleResource(Self);
end;


Function TCoordinateAPI.CreateScheduleResource(AOwner : TComponent) : TScheduleResource;

begin
  Result:=TScheduleResource.Create(AOwner);
  Result.API:=Self;
end;



Function TCoordinateAPI.GetTeamInstance : TTeamResource;

begin
  if (FTeamInstance=Nil) then
    FTeamInstance:=CreateTeamResource;
  Result:=FTeamInstance;
end;

Function TCoordinateAPI.CreateTeamResource : TTeamResource;

begin
  Result:=CreateTeamResource(Self);
end;


Function TCoordinateAPI.CreateTeamResource(AOwner : TComponent) : TTeamResource;

begin
  Result:=TTeamResource.Create(AOwner);
  Result.API:=Self;
end;



Function TCoordinateAPI.GetWorkerInstance : TWorkerResource;

begin
  if (FWorkerInstance=Nil) then
    FWorkerInstance:=CreateWorkerResource;
  Result:=FWorkerInstance;
end;

Function TCoordinateAPI.CreateWorkerResource : TWorkerResource;

begin
  Result:=CreateWorkerResource(Self);
end;


Function TCoordinateAPI.CreateWorkerResource(AOwner : TComponent) : TWorkerResource;

begin
  Result:=TWorkerResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TCoordinateAPI.RegisterAPI;
end.
