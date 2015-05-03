unit googlecoordinate;
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
  TCustomField = class;
  TCustomFieldArray = Array of TCustomField;
  TCustomFieldDef = class;
  TCustomFieldDefArray = Array of TCustomFieldDef;
  TCustomFieldDefenumitems = class;
  TCustomFieldDefenumitemsArray = Array of TCustomFieldDefenumitems;
  TCustomFieldDefListResponse = class;
  TCustomFieldDefListResponseArray = Array of TCustomFieldDefListResponse;
  TCustomFieldDefListResponseitems = class;
  TCustomFieldDefListResponseitemsArray = Array of TCustomFieldDefListResponseitems;
  TCustomFields = class;
  TCustomFieldsArray = Array of TCustomFields;
  TCustomFieldscustomField = class;
  TCustomFieldscustomFieldArray = Array of TCustomFieldscustomField;
  TEnumItemDef = class;
  TEnumItemDefArray = Array of TEnumItemDef;
  TJob = class;
  TJobArray = Array of TJob;
  TJobjobChange = class;
  TJobjobChangeArray = Array of TJobjobChange;
  TJobChange = class;
  TJobChangeArray = Array of TJobChange;
  TJobListResponse = class;
  TJobListResponseArray = Array of TJobListResponse;
  TJobListResponseitems = class;
  TJobListResponseitemsArray = Array of TJobListResponseitems;
  TJobState = class;
  TJobStateArray = Array of TJobState;
  TJobStatenote = class;
  TJobStatenoteArray = Array of TJobStatenote;
  TLocation = class;
  TLocationArray = Array of TLocation;
  TLocationaddressLine = class;
  TLocationaddressLineArray = Array of TLocationaddressLine;
  TLocationListResponse = class;
  TLocationListResponseArray = Array of TLocationListResponse;
  TLocationListResponseitems = class;
  TLocationListResponseitemsArray = Array of TLocationListResponseitems;
  TLocationRecord = class;
  TLocationRecordArray = Array of TLocationRecord;
  TSchedule = class;
  TScheduleArray = Array of TSchedule;
  TTeam = class;
  TTeamArray = Array of TTeam;
  TTeamListResponse = class;
  TTeamListResponseArray = Array of TTeamListResponse;
  TTeamListResponseitems = class;
  TTeamListResponseitemsArray = Array of TTeamListResponseitems;
  TTokenPagination = class;
  TTokenPaginationArray = Array of TTokenPagination;
  TWorker = class;
  TWorkerArray = Array of TWorker;
  TWorkerListResponse = class;
  TWorkerListResponseArray = Array of TWorkerListResponse;
  TWorkerListResponseitems = class;
  TWorkerListResponseitemsArray = Array of TWorkerListResponseitems;
  
  { --------------------------------------------------------------------
    TCustomField
    --------------------------------------------------------------------}
  
  TCustomField = Class(TGoogleBaseObject)
  Private
    FcustomFieldId : string;
    Fkind : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure SetcustomFieldId(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property customFieldId : string Index 0 Read FcustomFieldId Write SetcustomFieldId;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property value : string Index 16 Read Fvalue Write Setvalue;
  end;
  TCustomFieldClass = Class of TCustomField;
  
  { --------------------------------------------------------------------
    TCustomFieldDef
    --------------------------------------------------------------------}
  
  TCustomFieldDef = Class(TGoogleBaseObject)
  Private
    Fenabled : boolean;
    Fenumitems : TCustomFieldDefenumitems;
    Fid : string;
    Fkind : string;
    Fname : string;
    FrequiredForCheckout : boolean;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setenabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setenumitems(AIndex : Integer; AValue : TCustomFieldDefenumitems); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetrequiredForCheckout(AIndex : Integer; AValue : boolean); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property enabled : boolean Index 0 Read Fenabled Write Setenabled;
    Property enumitems : TCustomFieldDefenumitems Index 8 Read Fenumitems Write Setenumitems;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property name : string Index 32 Read Fname Write Setname;
    Property requiredForCheckout : boolean Index 40 Read FrequiredForCheckout Write SetrequiredForCheckout;
    Property _type : string Index 48 Read F_type Write Set_type;
  end;
  TCustomFieldDefClass = Class of TCustomFieldDef;
  
  { --------------------------------------------------------------------
    TCustomFieldDefenumitems
    --------------------------------------------------------------------}
  
  TCustomFieldDefenumitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCustomFieldDefenumitemsClass = Class of TCustomFieldDefenumitems;
  
  { --------------------------------------------------------------------
    TCustomFieldDefListResponse
    --------------------------------------------------------------------}
  
  TCustomFieldDefListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TCustomFieldDefListResponseitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TCustomFieldDefListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TCustomFieldDefListResponseitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TCustomFieldDefListResponseClass = Class of TCustomFieldDefListResponse;
  
  { --------------------------------------------------------------------
    TCustomFieldDefListResponseitems
    --------------------------------------------------------------------}
  
  TCustomFieldDefListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCustomFieldDefListResponseitemsClass = Class of TCustomFieldDefListResponseitems;
  
  { --------------------------------------------------------------------
    TCustomFields
    --------------------------------------------------------------------}
  
  TCustomFields = Class(TGoogleBaseObject)
  Private
    FcustomField : TCustomFieldscustomField;
    Fkind : string;
  Protected
    //Property setters
    Procedure SetcustomField(AIndex : Integer; AValue : TCustomFieldscustomField); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property customField : TCustomFieldscustomField Index 0 Read FcustomField Write SetcustomField;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TCustomFieldsClass = Class of TCustomFields;
  
  { --------------------------------------------------------------------
    TCustomFieldscustomField
    --------------------------------------------------------------------}
  
  TCustomFieldscustomField = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCustomFieldscustomFieldClass = Class of TCustomFieldscustomField;
  
  { --------------------------------------------------------------------
    TEnumItemDef
    --------------------------------------------------------------------}
  
  TEnumItemDef = Class(TGoogleBaseObject)
  Private
    Factive : boolean;
    Fkind : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setactive(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property active : boolean Index 0 Read Factive Write Setactive;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property value : string Index 16 Read Fvalue Write Setvalue;
  end;
  TEnumItemDefClass = Class of TEnumItemDef;
  
  { --------------------------------------------------------------------
    TJob
    --------------------------------------------------------------------}
  
  TJob = Class(TGoogleBaseObject)
  Private
    Fid : string;
    FjobChange : TJobjobChange;
    Fkind : string;
    Fstate : TJobState;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetjobChange(AIndex : Integer; AValue : TJobjobChange); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setstate(AIndex : Integer; AValue : TJobState); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property jobChange : TJobjobChange Index 8 Read FjobChange Write SetjobChange;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property state : TJobState Index 24 Read Fstate Write Setstate;
  end;
  TJobClass = Class of TJob;
  
  { --------------------------------------------------------------------
    TJobjobChange
    --------------------------------------------------------------------}
  
  TJobjobChange = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TJobjobChangeClass = Class of TJobjobChange;
  
  { --------------------------------------------------------------------
    TJobChange
    --------------------------------------------------------------------}
  
  TJobChange = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    Fstate : TJobState;
    Ftimestamp : string;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setstate(AIndex : Integer; AValue : TJobState); virtual;
    Procedure Settimestamp(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property state : TJobState Index 8 Read Fstate Write Setstate;
    Property timestamp : string Index 16 Read Ftimestamp Write Settimestamp;
  end;
  TJobChangeClass = Class of TJobChange;
  
  { --------------------------------------------------------------------
    TJobListResponse
    --------------------------------------------------------------------}
  
  TJobListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TJobListResponseitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TJobListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TJobListResponseitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TJobListResponseClass = Class of TJobListResponse;
  
  { --------------------------------------------------------------------
    TJobListResponseitems
    --------------------------------------------------------------------}
  
  TJobListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TJobListResponseitemsClass = Class of TJobListResponseitems;
  
  { --------------------------------------------------------------------
    TJobState
    --------------------------------------------------------------------}
  
  TJobState = Class(TGoogleBaseObject)
  Private
    Fassignee : string;
    FcustomFields : TCustomFields;
    FcustomerName : string;
    FcustomerPhoneNumber : string;
    Fkind : string;
    Flocation : TLocation;
    Fnote : TJobStatenote;
    Fprogress : string;
    Ftitle : string;
  Protected
    //Property setters
    Procedure Setassignee(AIndex : Integer; AValue : string); virtual;
    Procedure SetcustomFields(AIndex : Integer; AValue : TCustomFields); virtual;
    Procedure SetcustomerName(AIndex : Integer; AValue : string); virtual;
    Procedure SetcustomerPhoneNumber(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : TLocation); virtual;
    Procedure Setnote(AIndex : Integer; AValue : TJobStatenote); virtual;
    Procedure Setprogress(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property assignee : string Index 0 Read Fassignee Write Setassignee;
    Property customFields : TCustomFields Index 8 Read FcustomFields Write SetcustomFields;
    Property customerName : string Index 16 Read FcustomerName Write SetcustomerName;
    Property customerPhoneNumber : string Index 24 Read FcustomerPhoneNumber Write SetcustomerPhoneNumber;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property location : TLocation Index 40 Read Flocation Write Setlocation;
    Property note : TJobStatenote Index 48 Read Fnote Write Setnote;
    Property progress : string Index 56 Read Fprogress Write Setprogress;
    Property title : string Index 64 Read Ftitle Write Settitle;
  end;
  TJobStateClass = Class of TJobState;
  
  { --------------------------------------------------------------------
    TJobStatenote
    --------------------------------------------------------------------}
  
  TJobStatenote = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TJobStatenoteClass = Class of TJobStatenote;
  
  { --------------------------------------------------------------------
    TLocation
    --------------------------------------------------------------------}
  
  TLocation = Class(TGoogleBaseObject)
  Private
    FaddressLine : TLocationaddressLine;
    Fkind : string;
    Flat : double;
    Flng : double;
  Protected
    //Property setters
    Procedure SetaddressLine(AIndex : Integer; AValue : TLocationaddressLine); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlat(AIndex : Integer; AValue : double); virtual;
    Procedure Setlng(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property addressLine : TLocationaddressLine Index 0 Read FaddressLine Write SetaddressLine;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property lat : double Index 16 Read Flat Write Setlat;
    Property lng : double Index 24 Read Flng Write Setlng;
  end;
  TLocationClass = Class of TLocation;
  
  { --------------------------------------------------------------------
    TLocationaddressLine
    --------------------------------------------------------------------}
  
  TLocationaddressLine = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLocationaddressLineClass = Class of TLocationaddressLine;
  
  { --------------------------------------------------------------------
    TLocationListResponse
    --------------------------------------------------------------------}
  
  TLocationListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TLocationListResponseitems;
    Fkind : string;
    FnextPageToken : string;
    FtokenPagination : TTokenPagination;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TLocationListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SettokenPagination(AIndex : Integer; AValue : TTokenPagination); virtual;
  Public
  Published
    Property items : TLocationListResponseitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
    Property tokenPagination : TTokenPagination Index 24 Read FtokenPagination Write SettokenPagination;
  end;
  TLocationListResponseClass = Class of TLocationListResponse;
  
  { --------------------------------------------------------------------
    TLocationListResponseitems
    --------------------------------------------------------------------}
  
  TLocationListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLocationListResponseitemsClass = Class of TLocationListResponseitems;
  
  { --------------------------------------------------------------------
    TLocationRecord
    --------------------------------------------------------------------}
  
  TLocationRecord = Class(TGoogleBaseObject)
  Private
    FcollectionTime : string;
    FconfidenceRadius : double;
    Fkind : string;
    Flatitude : double;
    Flongitude : double;
  Protected
    //Property setters
    Procedure SetcollectionTime(AIndex : Integer; AValue : string); virtual;
    Procedure SetconfidenceRadius(AIndex : Integer; AValue : double); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlatitude(AIndex : Integer; AValue : double); virtual;
    Procedure Setlongitude(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property collectionTime : string Index 0 Read FcollectionTime Write SetcollectionTime;
    Property confidenceRadius : double Index 8 Read FconfidenceRadius Write SetconfidenceRadius;
    Property kind : string Index 16 Read Fkind Write Setkind;
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
    Fduration : string;
    FendTime : string;
    Fkind : string;
    FstartTime : string;
  Protected
    //Property setters
    Procedure SetallDay(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setduration(AIndex : Integer; AValue : string); virtual;
    Procedure SetendTime(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property allDay : boolean Index 0 Read FallDay Write SetallDay;
    Property duration : string Index 8 Read Fduration Write Setduration;
    Property endTime : string Index 16 Read FendTime Write SetendTime;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property startTime : string Index 32 Read FstartTime Write SetstartTime;
  end;
  TScheduleClass = Class of TSchedule;
  
  { --------------------------------------------------------------------
    TTeam
    --------------------------------------------------------------------}
  
  TTeam = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fkind : string;
    Fname : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property name : string Index 16 Read Fname Write Setname;
  end;
  TTeamClass = Class of TTeam;
  
  { --------------------------------------------------------------------
    TTeamListResponse
    --------------------------------------------------------------------}
  
  TTeamListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TTeamListResponseitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TTeamListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TTeamListResponseitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TTeamListResponseClass = Class of TTeamListResponse;
  
  { --------------------------------------------------------------------
    TTeamListResponseitems
    --------------------------------------------------------------------}
  
  TTeamListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTeamListResponseitemsClass = Class of TTeamListResponseitems;
  
  { --------------------------------------------------------------------
    TTokenPagination
    --------------------------------------------------------------------}
  
  TTokenPagination = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    FnextPageToken : string;
    FpreviousPageToken : string;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetpreviousPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
    Property previousPageToken : string Index 16 Read FpreviousPageToken Write SetpreviousPageToken;
  end;
  TTokenPaginationClass = Class of TTokenPagination;
  
  { --------------------------------------------------------------------
    TWorker
    --------------------------------------------------------------------}
  
  TWorker = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TWorkerClass = Class of TWorker;
  
  { --------------------------------------------------------------------
    TWorkerListResponse
    --------------------------------------------------------------------}
  
  TWorkerListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TWorkerListResponseitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TWorkerListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TWorkerListResponseitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TWorkerListResponseClass = Class of TWorkerListResponse;
  
  { --------------------------------------------------------------------
    TWorkerListResponseitems
    --------------------------------------------------------------------}
  
  TWorkerListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TWorkerListResponseitemsClass = Class of TWorkerListResponseitems;
  
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
    address : string;
    assignee : string;
    customField : string;
    customerName : string;
    customerPhoneNumber : string;
    lat : double;
    lng : double;
    note : string;
    title : string;
  end;
  
  
  //Optional query Options for TJobsResource, method List
  
  TJobsListOptions = Record
    maxResults : integer;
    minModifiedTimestampMs : string;
    pageToken : string;
  end;
  
  
  //Optional query Options for TJobsResource, method Patch
  
  TJobsPatchOptions = Record
    address : string;
    assignee : string;
    customField : string;
    customerName : string;
    customerPhoneNumber : string;
    lat : double;
    lng : double;
    note : string;
    progress : string;
    title : string;
  end;
  
  
  //Optional query Options for TJobsResource, method Update
  
  TJobsUpdateOptions = Record
    address : string;
    assignee : string;
    customField : string;
    customerName : string;
    customerPhoneNumber : string;
    lat : double;
    lng : double;
    note : string;
    progress : string;
    title : string;
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
    pageToken : string;
    startTimestampMs : string;
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
    duration : string;
    endTime : string;
    startTime : string;
  end;
  
  
  //Optional query Options for TScheduleResource, method Update
  
  TScheduleUpdateOptions = Record
    allDay : boolean;
    duration : string;
    endTime : string;
    startTime : string;
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


Procedure TCustomField.SetcustomFieldId(AIndex : Integer; AValue : string); 

begin
  If (FcustomFieldId=AValue) then exit;
  FcustomFieldId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomField.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomField.Setvalue(AIndex : Integer; AValue : string); 

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



Procedure TCustomFieldDef.Setenumitems(AIndex : Integer; AValue : TCustomFieldDefenumitems); 

begin
  If (Fenumitems=AValue) then exit;
  Fenumitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomFieldDef.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomFieldDef.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomFieldDef.Setname(AIndex : Integer; AValue : string); 

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



Procedure TCustomFieldDef.Set_type(AIndex : Integer; AValue : string); 

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
  TCustomFieldDefenumitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCustomFieldDefListResponse
  --------------------------------------------------------------------}


Procedure TCustomFieldDefListResponse.Setitems(AIndex : Integer; AValue : TCustomFieldDefListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomFieldDefListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCustomFieldDefListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCustomFields
  --------------------------------------------------------------------}


Procedure TCustomFields.SetcustomField(AIndex : Integer; AValue : TCustomFieldscustomField); 

begin
  If (FcustomField=AValue) then exit;
  FcustomField:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomFields.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCustomFieldscustomField
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TEnumItemDef
  --------------------------------------------------------------------}


Procedure TEnumItemDef.Setactive(AIndex : Integer; AValue : boolean); 

begin
  If (Factive=AValue) then exit;
  Factive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnumItemDef.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnumItemDef.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJob
  --------------------------------------------------------------------}


Procedure TJob.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetjobChange(AIndex : Integer; AValue : TJobjobChange); 

begin
  If (FjobChange=AValue) then exit;
  FjobChange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setkind(AIndex : Integer; AValue : string); 

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
  TJobjobChange
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TJobChange
  --------------------------------------------------------------------}


Procedure TJobChange.Setkind(AIndex : Integer; AValue : string); 

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



Procedure TJobChange.Settimestamp(AIndex : Integer; AValue : string); 

begin
  If (Ftimestamp=AValue) then exit;
  Ftimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobListResponse
  --------------------------------------------------------------------}


Procedure TJobListResponse.Setitems(AIndex : Integer; AValue : TJobListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TJobState
  --------------------------------------------------------------------}


Procedure TJobState.Setassignee(AIndex : Integer; AValue : string); 

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



Procedure TJobState.SetcustomerName(AIndex : Integer; AValue : string); 

begin
  If (FcustomerName=AValue) then exit;
  FcustomerName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobState.SetcustomerPhoneNumber(AIndex : Integer; AValue : string); 

begin
  If (FcustomerPhoneNumber=AValue) then exit;
  FcustomerPhoneNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobState.Setkind(AIndex : Integer; AValue : string); 

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



Procedure TJobState.Setnote(AIndex : Integer; AValue : TJobStatenote); 

begin
  If (Fnote=AValue) then exit;
  Fnote:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobState.Setprogress(AIndex : Integer; AValue : string); 

begin
  If (Fprogress=AValue) then exit;
  Fprogress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobState.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobStatenote
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLocation
  --------------------------------------------------------------------}


Procedure TLocation.SetaddressLine(AIndex : Integer; AValue : TLocationaddressLine); 

begin
  If (FaddressLine=AValue) then exit;
  FaddressLine:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocation.Setkind(AIndex : Integer; AValue : string); 

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
  TLocationaddressLine
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLocationListResponse
  --------------------------------------------------------------------}


Procedure TLocationListResponse.Setitems(AIndex : Integer; AValue : TLocationListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocationListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocationListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

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
  TLocationListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLocationRecord
  --------------------------------------------------------------------}


Procedure TLocationRecord.SetcollectionTime(AIndex : Integer; AValue : string); 

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



Procedure TLocationRecord.Setkind(AIndex : Integer; AValue : string); 

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



Procedure TSchedule.Setduration(AIndex : Integer; AValue : string); 

begin
  If (Fduration=AValue) then exit;
  Fduration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSchedule.SetendTime(AIndex : Integer; AValue : string); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSchedule.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSchedule.SetstartTime(AIndex : Integer; AValue : string); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTeam
  --------------------------------------------------------------------}


Procedure TTeam.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTeam.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTeam.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTeamListResponse
  --------------------------------------------------------------------}


Procedure TTeamListResponse.Setitems(AIndex : Integer; AValue : TTeamListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTeamListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTeamListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTokenPagination
  --------------------------------------------------------------------}


Procedure TTokenPagination.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTokenPagination.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTokenPagination.SetpreviousPageToken(AIndex : Integer; AValue : string); 

begin
  If (FpreviousPageToken=AValue) then exit;
  FpreviousPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWorker
  --------------------------------------------------------------------}


Procedure TWorker.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorker.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWorkerListResponse
  --------------------------------------------------------------------}


Procedure TWorkerListResponse.Setitems(AIndex : Integer; AValue : TWorkerListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWorkerListResponseitems
  --------------------------------------------------------------------}




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
  TCustomFieldDefenumitems.RegisterObject;
  TCustomFieldDefListResponse.RegisterObject;
  TCustomFieldDefListResponseitems.RegisterObject;
  TCustomFields.RegisterObject;
  TCustomFieldscustomField.RegisterObject;
  TEnumItemDef.RegisterObject;
  TJob.RegisterObject;
  TJobjobChange.RegisterObject;
  TJobChange.RegisterObject;
  TJobListResponse.RegisterObject;
  TJobListResponseitems.RegisterObject;
  TJobState.RegisterObject;
  TJobStatenote.RegisterObject;
  TLocation.RegisterObject;
  TLocationaddressLine.RegisterObject;
  TLocationListResponse.RegisterObject;
  TLocationListResponseitems.RegisterObject;
  TLocationRecord.RegisterObject;
  TSchedule.RegisterObject;
  TTeam.RegisterObject;
  TTeamListResponse.RegisterObject;
  TTeamListResponseitems.RegisterObject;
  TTokenPagination.RegisterObject;
  TWorker.RegisterObject;
  TWorkerListResponse.RegisterObject;
  TWorkerListResponseitems.RegisterObject;
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
