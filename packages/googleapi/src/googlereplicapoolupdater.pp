unit googlereplicapoolupdater;
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
  TInstanceUpdate = class;
  TInstanceUpdateArray = Array of TInstanceUpdate;
  TInstanceUpdateerror = class;
  TInstanceUpdateerrorArray = Array of TInstanceUpdateerror;
  TInstanceUpdateerrorerrors = class;
  TInstanceUpdateerrorerrorsArray = Array of TInstanceUpdateerrorerrors;
  TInstanceUpdateList = class;
  TInstanceUpdateListArray = Array of TInstanceUpdateList;
  TInstanceUpdateListitems = class;
  TInstanceUpdateListitemsArray = Array of TInstanceUpdateListitems;
  TOperation = class;
  TOperationArray = Array of TOperation;
  TOperationerror = class;
  TOperationerrorArray = Array of TOperationerror;
  TOperationerrorerrors = class;
  TOperationerrorerrorsArray = Array of TOperationerrorerrors;
  TOperationwarnings = class;
  TOperationwarningsArray = Array of TOperationwarnings;
  TOperationwarningsdata = class;
  TOperationwarningsdataArray = Array of TOperationwarningsdata;
  TRollingUpdate = class;
  TRollingUpdateArray = Array of TRollingUpdate;
  TRollingUpdateerror = class;
  TRollingUpdateerrorArray = Array of TRollingUpdateerror;
  TRollingUpdateerrorerrors = class;
  TRollingUpdateerrorerrorsArray = Array of TRollingUpdateerrorerrors;
  TRollingUpdatepolicy = class;
  TRollingUpdatepolicyArray = Array of TRollingUpdatepolicy;
  TRollingUpdateList = class;
  TRollingUpdateListArray = Array of TRollingUpdateList;
  TRollingUpdateListitems = class;
  TRollingUpdateListitemsArray = Array of TRollingUpdateListitems;
  
  { --------------------------------------------------------------------
    TInstanceUpdate
    --------------------------------------------------------------------}
  
  TInstanceUpdate = Class(TGoogleBaseObject)
  Private
    Ferror : TInstanceUpdateerror;
    Finstance : string;
    Fstatus : string;
  Protected
    //Property setters
    Procedure Seterror(AIndex : Integer; AValue : TInstanceUpdateerror); virtual;
    Procedure Setinstance(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property error : TInstanceUpdateerror Index 0 Read Ferror Write Seterror;
    Property instance : string Index 8 Read Finstance Write Setinstance;
    Property status : string Index 16 Read Fstatus Write Setstatus;
  end;
  TInstanceUpdateClass = Class of TInstanceUpdate;
  
  { --------------------------------------------------------------------
    TInstanceUpdateerror
    --------------------------------------------------------------------}
  
  TInstanceUpdateerror = Class(TGoogleBaseObject)
  Private
    Ferrors : TInstanceUpdateerrorerrors;
  Protected
    //Property setters
    Procedure Seterrors(AIndex : Integer; AValue : TInstanceUpdateerrorerrors); virtual;
  Public
  Published
    Property errors : TInstanceUpdateerrorerrors Index 0 Read Ferrors Write Seterrors;
  end;
  TInstanceUpdateerrorClass = Class of TInstanceUpdateerror;
  
  { --------------------------------------------------------------------
    TInstanceUpdateerrorerrors
    --------------------------------------------------------------------}
  
  TInstanceUpdateerrorerrors = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Flocation : string;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : string); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property location : string Index 8 Read Flocation Write Setlocation;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TInstanceUpdateerrorerrorsClass = Class of TInstanceUpdateerrorerrors;
  
  { --------------------------------------------------------------------
    TInstanceUpdateList
    --------------------------------------------------------------------}
  
  TInstanceUpdateList = Class(TGoogleBaseObject)
  Private
    Fitems : TInstanceUpdateListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TInstanceUpdateListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TInstanceUpdateListitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 24 Read FselfLink Write SetselfLink;
  end;
  TInstanceUpdateListClass = Class of TInstanceUpdateList;
  
  { --------------------------------------------------------------------
    TInstanceUpdateListitems
    --------------------------------------------------------------------}
  
  TInstanceUpdateListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInstanceUpdateListitemsClass = Class of TInstanceUpdateListitems;
  
  { --------------------------------------------------------------------
    TOperation
    --------------------------------------------------------------------}
  
  TOperation = Class(TGoogleBaseObject)
  Private
    FclientOperationId : string;
    FcreationTimestamp : string;
    FendTime : string;
    Ferror : TOperationerror;
    FhttpErrorMessage : string;
    FhttpErrorStatusCode : integer;
    Fid : string;
    FinsertTime : string;
    Fkind : string;
    Fname : string;
    FoperationType : string;
    Fprogress : integer;
    Fregion : string;
    FselfLink : string;
    FstartTime : string;
    Fstatus : string;
    FstatusMessage : string;
    FtargetId : string;
    FtargetLink : string;
    Fuser : string;
    Fwarnings : TOperationwarnings;
    Fzone : string;
  Protected
    //Property setters
    Procedure SetclientOperationId(AIndex : Integer; AValue : string); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure SetendTime(AIndex : Integer; AValue : string); virtual;
    Procedure Seterror(AIndex : Integer; AValue : TOperationerror); virtual;
    Procedure SethttpErrorMessage(AIndex : Integer; AValue : string); virtual;
    Procedure SethttpErrorStatusCode(AIndex : Integer; AValue : integer); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetinsertTime(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetoperationType(AIndex : Integer; AValue : string); virtual;
    Procedure Setprogress(AIndex : Integer; AValue : integer); virtual;
    Procedure Setregion(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure SetstatusMessage(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetId(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setuser(AIndex : Integer; AValue : string); virtual;
    Procedure Setwarnings(AIndex : Integer; AValue : TOperationwarnings); virtual;
    Procedure Setzone(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property clientOperationId : string Index 0 Read FclientOperationId Write SetclientOperationId;
    Property creationTimestamp : string Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property endTime : string Index 16 Read FendTime Write SetendTime;
    Property error : TOperationerror Index 24 Read Ferror Write Seterror;
    Property httpErrorMessage : string Index 32 Read FhttpErrorMessage Write SethttpErrorMessage;
    Property httpErrorStatusCode : integer Index 40 Read FhttpErrorStatusCode Write SethttpErrorStatusCode;
    Property id : string Index 48 Read Fid Write Setid;
    Property insertTime : string Index 56 Read FinsertTime Write SetinsertTime;
    Property kind : string Index 64 Read Fkind Write Setkind;
    Property name : string Index 72 Read Fname Write Setname;
    Property operationType : string Index 80 Read FoperationType Write SetoperationType;
    Property progress : integer Index 88 Read Fprogress Write Setprogress;
    Property region : string Index 96 Read Fregion Write Setregion;
    Property selfLink : string Index 104 Read FselfLink Write SetselfLink;
    Property startTime : string Index 112 Read FstartTime Write SetstartTime;
    Property status : string Index 120 Read Fstatus Write Setstatus;
    Property statusMessage : string Index 128 Read FstatusMessage Write SetstatusMessage;
    Property targetId : string Index 136 Read FtargetId Write SettargetId;
    Property targetLink : string Index 144 Read FtargetLink Write SettargetLink;
    Property user : string Index 152 Read Fuser Write Setuser;
    Property warnings : TOperationwarnings Index 160 Read Fwarnings Write Setwarnings;
    Property zone : string Index 168 Read Fzone Write Setzone;
  end;
  TOperationClass = Class of TOperation;
  
  { --------------------------------------------------------------------
    TOperationerror
    --------------------------------------------------------------------}
  
  TOperationerror = Class(TGoogleBaseObject)
  Private
    Ferrors : TOperationerrorerrors;
  Protected
    //Property setters
    Procedure Seterrors(AIndex : Integer; AValue : TOperationerrorerrors); virtual;
  Public
  Published
    Property errors : TOperationerrorerrors Index 0 Read Ferrors Write Seterrors;
  end;
  TOperationerrorClass = Class of TOperationerror;
  
  { --------------------------------------------------------------------
    TOperationerrorerrors
    --------------------------------------------------------------------}
  
  TOperationerrorerrors = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Flocation : string;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : string); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property location : string Index 8 Read Flocation Write Setlocation;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TOperationerrorerrorsClass = Class of TOperationerrorerrors;
  
  { --------------------------------------------------------------------
    TOperationwarnings
    --------------------------------------------------------------------}
  
  TOperationwarnings = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fdata : TOperationwarningsdata;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TOperationwarningsdata); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property data : TOperationwarningsdata Index 8 Read Fdata Write Setdata;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TOperationwarningsClass = Class of TOperationwarnings;
  
  { --------------------------------------------------------------------
    TOperationwarningsdata
    --------------------------------------------------------------------}
  
  TOperationwarningsdata = Class(TGoogleBaseObject)
  Private
    Fkey : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property key : string Index 0 Read Fkey Write Setkey;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TOperationwarningsdataClass = Class of TOperationwarningsdata;
  
  { --------------------------------------------------------------------
    TRollingUpdate
    --------------------------------------------------------------------}
  
  TRollingUpdate = Class(TGoogleBaseObject)
  Private
    FactionType : string;
    FcreationTimestamp : string;
    Fdescription : string;
    Ferror : TRollingUpdateerror;
    Fid : string;
    FinstanceGroup : string;
    FinstanceGroupManager : string;
    FinstanceTemplate : string;
    Fkind : string;
    Fpolicy : TRollingUpdatepolicy;
    Fprogress : integer;
    FselfLink : string;
    Fstatus : string;
    FstatusMessage : string;
    Fuser : string;
  Protected
    //Property setters
    Procedure SetactionType(AIndex : Integer; AValue : string); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Seterror(AIndex : Integer; AValue : TRollingUpdateerror); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetinstanceGroup(AIndex : Integer; AValue : string); virtual;
    Procedure SetinstanceGroupManager(AIndex : Integer; AValue : string); virtual;
    Procedure SetinstanceTemplate(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setpolicy(AIndex : Integer; AValue : TRollingUpdatepolicy); virtual;
    Procedure Setprogress(AIndex : Integer; AValue : integer); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure SetstatusMessage(AIndex : Integer; AValue : string); virtual;
    Procedure Setuser(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property actionType : string Index 0 Read FactionType Write SetactionType;
    Property creationTimestamp : string Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 16 Read Fdescription Write Setdescription;
    Property error : TRollingUpdateerror Index 24 Read Ferror Write Seterror;
    Property id : string Index 32 Read Fid Write Setid;
    Property instanceGroup : string Index 40 Read FinstanceGroup Write SetinstanceGroup;
    Property instanceGroupManager : string Index 48 Read FinstanceGroupManager Write SetinstanceGroupManager;
    Property instanceTemplate : string Index 56 Read FinstanceTemplate Write SetinstanceTemplate;
    Property kind : string Index 64 Read Fkind Write Setkind;
    Property policy : TRollingUpdatepolicy Index 72 Read Fpolicy Write Setpolicy;
    Property progress : integer Index 80 Read Fprogress Write Setprogress;
    Property selfLink : string Index 88 Read FselfLink Write SetselfLink;
    Property status : string Index 96 Read Fstatus Write Setstatus;
    Property statusMessage : string Index 104 Read FstatusMessage Write SetstatusMessage;
    Property user : string Index 112 Read Fuser Write Setuser;
  end;
  TRollingUpdateClass = Class of TRollingUpdate;
  
  { --------------------------------------------------------------------
    TRollingUpdateerror
    --------------------------------------------------------------------}
  
  TRollingUpdateerror = Class(TGoogleBaseObject)
  Private
    Ferrors : TRollingUpdateerrorerrors;
  Protected
    //Property setters
    Procedure Seterrors(AIndex : Integer; AValue : TRollingUpdateerrorerrors); virtual;
  Public
  Published
    Property errors : TRollingUpdateerrorerrors Index 0 Read Ferrors Write Seterrors;
  end;
  TRollingUpdateerrorClass = Class of TRollingUpdateerror;
  
  { --------------------------------------------------------------------
    TRollingUpdateerrorerrors
    --------------------------------------------------------------------}
  
  TRollingUpdateerrorerrors = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Flocation : string;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : string); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property location : string Index 8 Read Flocation Write Setlocation;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TRollingUpdateerrorerrorsClass = Class of TRollingUpdateerrorerrors;
  
  { --------------------------------------------------------------------
    TRollingUpdatepolicy
    --------------------------------------------------------------------}
  
  TRollingUpdatepolicy = Class(TGoogleBaseObject)
  Private
    FautoPauseAfterInstances : integer;
    FinstanceStartupTimeoutSec : integer;
    FmaxNumConcurrentInstances : integer;
    FmaxNumFailedInstances : integer;
    FminInstanceUpdateTimeSec : integer;
  Protected
    //Property setters
    Procedure SetautoPauseAfterInstances(AIndex : Integer; AValue : integer); virtual;
    Procedure SetinstanceStartupTimeoutSec(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmaxNumConcurrentInstances(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmaxNumFailedInstances(AIndex : Integer; AValue : integer); virtual;
    Procedure SetminInstanceUpdateTimeSec(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property autoPauseAfterInstances : integer Index 0 Read FautoPauseAfterInstances Write SetautoPauseAfterInstances;
    Property instanceStartupTimeoutSec : integer Index 8 Read FinstanceStartupTimeoutSec Write SetinstanceStartupTimeoutSec;
    Property maxNumConcurrentInstances : integer Index 16 Read FmaxNumConcurrentInstances Write SetmaxNumConcurrentInstances;
    Property maxNumFailedInstances : integer Index 24 Read FmaxNumFailedInstances Write SetmaxNumFailedInstances;
    Property minInstanceUpdateTimeSec : integer Index 32 Read FminInstanceUpdateTimeSec Write SetminInstanceUpdateTimeSec;
  end;
  TRollingUpdatepolicyClass = Class of TRollingUpdatepolicy;
  
  { --------------------------------------------------------------------
    TRollingUpdateList
    --------------------------------------------------------------------}
  
  TRollingUpdateList = Class(TGoogleBaseObject)
  Private
    Fitems : TRollingUpdateListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TRollingUpdateListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TRollingUpdateListitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 24 Read FselfLink Write SetselfLink;
  end;
  TRollingUpdateListClass = Class of TRollingUpdateList;
  
  { --------------------------------------------------------------------
    TRollingUpdateListitems
    --------------------------------------------------------------------}
  
  TRollingUpdateListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRollingUpdateListitemsClass = Class of TRollingUpdateListitems;
  
  { --------------------------------------------------------------------
    TRollingUpdatesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TRollingUpdatesResource, method List
  
  TRollingUpdatesListOptions = Record
    filter : string;
    instanceGroupManager : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TRollingUpdatesResource, method ListInstanceUpdates
  
  TRollingUpdatesListInstanceUpdatesOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TRollingUpdatesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Cancel(project: string; rollingUpdate: string; zone: string) : TOperation;
    Function Get(project: string; rollingUpdate: string; zone: string) : TRollingUpdate;
    Function Insert(project: string; zone: string; aRollingUpdate : TRollingUpdate) : TOperation;
    Function List(project: string; zone: string; AQuery : string  = '') : TRollingUpdateList;
    Function List(project: string; zone: string; AQuery : TRollingUpdateslistOptions) : TRollingUpdateList;
    Function ListInstanceUpdates(project: string; rollingUpdate: string; zone: string; AQuery : string  = '') : TInstanceUpdateList;
    Function ListInstanceUpdates(project: string; rollingUpdate: string; zone: string; AQuery : TRollingUpdateslistInstanceUpdatesOptions) : TInstanceUpdateList;
    Function Pause(project: string; rollingUpdate: string; zone: string) : TOperation;
    Function Resume(project: string; rollingUpdate: string; zone: string) : TOperation;
    Function Rollback(project: string; rollingUpdate: string; zone: string) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TZoneOperationsResource
    --------------------------------------------------------------------}
  
  TZoneOperationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(operation: string; project: string; zone: string) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TReplicapoolupdaterAPI
    --------------------------------------------------------------------}
  
  TReplicapoolupdaterAPI = Class(TGoogleAPI)
  Private
    FRollingUpdatesInstance : TRollingUpdatesResource;
    FZoneOperationsInstance : TZoneOperationsResource;
    Function GetRollingUpdatesInstance : TRollingUpdatesResource;virtual;
    Function GetZoneOperationsInstance : TZoneOperationsResource;virtual;
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
    Function CreateRollingUpdatesResource(AOwner : TComponent) : TRollingUpdatesResource;virtual;overload;
    Function CreateRollingUpdatesResource : TRollingUpdatesResource;virtual;overload;
    Function CreateZoneOperationsResource(AOwner : TComponent) : TZoneOperationsResource;virtual;overload;
    Function CreateZoneOperationsResource : TZoneOperationsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property RollingUpdatesResource : TRollingUpdatesResource Read GetRollingUpdatesInstance;
    Property ZoneOperationsResource : TZoneOperationsResource Read GetZoneOperationsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TInstanceUpdate
  --------------------------------------------------------------------}


Procedure TInstanceUpdate.Seterror(AIndex : Integer; AValue : TInstanceUpdateerror); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceUpdate.Setinstance(AIndex : Integer; AValue : string); 

begin
  If (Finstance=AValue) then exit;
  Finstance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceUpdate.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceUpdateerror
  --------------------------------------------------------------------}


Procedure TInstanceUpdateerror.Seterrors(AIndex : Integer; AValue : TInstanceUpdateerrorerrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceUpdateerrorerrors
  --------------------------------------------------------------------}


Procedure TInstanceUpdateerrorerrors.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceUpdateerrorerrors.Setlocation(AIndex : Integer; AValue : string); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceUpdateerrorerrors.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceUpdateList
  --------------------------------------------------------------------}


Procedure TInstanceUpdateList.Setitems(AIndex : Integer; AValue : TInstanceUpdateListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceUpdateList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceUpdateList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceUpdateList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceUpdateListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TOperation
  --------------------------------------------------------------------}


Procedure TOperation.SetclientOperationId(AIndex : Integer; AValue : string); 

begin
  If (FclientOperationId=AValue) then exit;
  FclientOperationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetendTime(AIndex : Integer; AValue : string); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Seterror(AIndex : Integer; AValue : TOperationerror); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SethttpErrorMessage(AIndex : Integer; AValue : string); 

begin
  If (FhttpErrorMessage=AValue) then exit;
  FhttpErrorMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SethttpErrorStatusCode(AIndex : Integer; AValue : integer); 

begin
  If (FhttpErrorStatusCode=AValue) then exit;
  FhttpErrorStatusCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetinsertTime(AIndex : Integer; AValue : string); 

begin
  If (FinsertTime=AValue) then exit;
  FinsertTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetoperationType(AIndex : Integer; AValue : string); 

begin
  If (FoperationType=AValue) then exit;
  FoperationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setprogress(AIndex : Integer; AValue : integer); 

begin
  If (Fprogress=AValue) then exit;
  Fprogress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setregion(AIndex : Integer; AValue : string); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetstartTime(AIndex : Integer; AValue : string); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetstatusMessage(AIndex : Integer; AValue : string); 

begin
  If (FstatusMessage=AValue) then exit;
  FstatusMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetId(AIndex : Integer; AValue : string); 

begin
  If (FtargetId=AValue) then exit;
  FtargetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetLink(AIndex : Integer; AValue : string); 

begin
  If (FtargetLink=AValue) then exit;
  FtargetLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setuser(AIndex : Integer; AValue : string); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setwarnings(AIndex : Integer; AValue : TOperationwarnings); 

begin
  If (Fwarnings=AValue) then exit;
  Fwarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setzone(AIndex : Integer; AValue : string); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationerror
  --------------------------------------------------------------------}


Procedure TOperationerror.Seterrors(AIndex : Integer; AValue : TOperationerrorerrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationerrorerrors
  --------------------------------------------------------------------}


Procedure TOperationerrorerrors.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationerrorerrors.Setlocation(AIndex : Integer; AValue : string); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationerrorerrors.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationwarnings
  --------------------------------------------------------------------}


Procedure TOperationwarnings.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationwarnings.Setdata(AIndex : Integer; AValue : TOperationwarningsdata); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationwarnings.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationwarningsdata
  --------------------------------------------------------------------}


Procedure TOperationwarningsdata.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationwarningsdata.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRollingUpdate
  --------------------------------------------------------------------}


Procedure TRollingUpdate.SetactionType(AIndex : Integer; AValue : string); 

begin
  If (FactionType=AValue) then exit;
  FactionType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.Seterror(AIndex : Integer; AValue : TRollingUpdateerror); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.SetinstanceGroup(AIndex : Integer; AValue : string); 

begin
  If (FinstanceGroup=AValue) then exit;
  FinstanceGroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.SetinstanceGroupManager(AIndex : Integer; AValue : string); 

begin
  If (FinstanceGroupManager=AValue) then exit;
  FinstanceGroupManager:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.SetinstanceTemplate(AIndex : Integer; AValue : string); 

begin
  If (FinstanceTemplate=AValue) then exit;
  FinstanceTemplate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.Setpolicy(AIndex : Integer; AValue : TRollingUpdatepolicy); 

begin
  If (Fpolicy=AValue) then exit;
  Fpolicy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.Setprogress(AIndex : Integer; AValue : integer); 

begin
  If (Fprogress=AValue) then exit;
  Fprogress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.SetstatusMessage(AIndex : Integer; AValue : string); 

begin
  If (FstatusMessage=AValue) then exit;
  FstatusMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.Setuser(AIndex : Integer; AValue : string); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRollingUpdateerror
  --------------------------------------------------------------------}


Procedure TRollingUpdateerror.Seterrors(AIndex : Integer; AValue : TRollingUpdateerrorerrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRollingUpdateerrorerrors
  --------------------------------------------------------------------}


Procedure TRollingUpdateerrorerrors.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdateerrorerrors.Setlocation(AIndex : Integer; AValue : string); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdateerrorerrors.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRollingUpdatepolicy
  --------------------------------------------------------------------}


Procedure TRollingUpdatepolicy.SetautoPauseAfterInstances(AIndex : Integer; AValue : integer); 

begin
  If (FautoPauseAfterInstances=AValue) then exit;
  FautoPauseAfterInstances:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdatepolicy.SetinstanceStartupTimeoutSec(AIndex : Integer; AValue : integer); 

begin
  If (FinstanceStartupTimeoutSec=AValue) then exit;
  FinstanceStartupTimeoutSec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdatepolicy.SetmaxNumConcurrentInstances(AIndex : Integer; AValue : integer); 

begin
  If (FmaxNumConcurrentInstances=AValue) then exit;
  FmaxNumConcurrentInstances:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdatepolicy.SetmaxNumFailedInstances(AIndex : Integer; AValue : integer); 

begin
  If (FmaxNumFailedInstances=AValue) then exit;
  FmaxNumFailedInstances:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdatepolicy.SetminInstanceUpdateTimeSec(AIndex : Integer; AValue : integer); 

begin
  If (FminInstanceUpdateTimeSec=AValue) then exit;
  FminInstanceUpdateTimeSec:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRollingUpdateList
  --------------------------------------------------------------------}


Procedure TRollingUpdateList.Setitems(AIndex : Integer; AValue : TRollingUpdateListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdateList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdateList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdateList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRollingUpdateListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRollingUpdatesResource
  --------------------------------------------------------------------}


Class Function TRollingUpdatesResource.ResourceName : String;

begin
  Result:='rollingUpdates';
end;

Class Function TRollingUpdatesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TreplicapoolupdaterAPI;
end;

Function TRollingUpdatesResource.Cancel(project: string; rollingUpdate: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/rollingUpdates/{rollingUpdate}/cancel';
  _Methodid   = 'replicapoolupdater.rollingUpdates.cancel';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'rollingUpdate',rollingUpdate,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TRollingUpdatesResource.Get(project: string; rollingUpdate: string; zone: string) : TRollingUpdate;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/rollingUpdates/{rollingUpdate}';
  _Methodid   = 'replicapoolupdater.rollingUpdates.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'rollingUpdate',rollingUpdate,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TRollingUpdate) as TRollingUpdate;
end;

Function TRollingUpdatesResource.Insert(project: string; zone: string; aRollingUpdate : TRollingUpdate) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/rollingUpdates';
  _Methodid   = 'replicapoolupdater.rollingUpdates.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aRollingUpdate,TOperation) as TOperation;
end;

Function TRollingUpdatesResource.List(project: string; zone: string; AQuery : string = '') : TRollingUpdateList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/rollingUpdates';
  _Methodid   = 'replicapoolupdater.rollingUpdates.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TRollingUpdateList) as TRollingUpdateList;
end;


Function TRollingUpdatesResource.List(project: string; zone: string; AQuery : TRollingUpdateslistOptions) : TRollingUpdateList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'instanceGroupManager',AQuery.instanceGroupManager);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,zone,_Q);
end;

Function TRollingUpdatesResource.ListInstanceUpdates(project: string; rollingUpdate: string; zone: string; AQuery : string = '') : TInstanceUpdateList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/rollingUpdates/{rollingUpdate}/instanceUpdates';
  _Methodid   = 'replicapoolupdater.rollingUpdates.listInstanceUpdates';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'rollingUpdate',rollingUpdate,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TInstanceUpdateList) as TInstanceUpdateList;
end;


Function TRollingUpdatesResource.ListInstanceUpdates(project: string; rollingUpdate: string; zone: string; AQuery : TRollingUpdateslistInstanceUpdatesOptions) : TInstanceUpdateList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=ListInstanceUpdates(project,rollingUpdate,zone,_Q);
end;

Function TRollingUpdatesResource.Pause(project: string; rollingUpdate: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/rollingUpdates/{rollingUpdate}/pause';
  _Methodid   = 'replicapoolupdater.rollingUpdates.pause';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'rollingUpdate',rollingUpdate,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TRollingUpdatesResource.Resume(project: string; rollingUpdate: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/rollingUpdates/{rollingUpdate}/resume';
  _Methodid   = 'replicapoolupdater.rollingUpdates.resume';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'rollingUpdate',rollingUpdate,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TRollingUpdatesResource.Rollback(project: string; rollingUpdate: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/rollingUpdates/{rollingUpdate}/rollback';
  _Methodid   = 'replicapoolupdater.rollingUpdates.rollback';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'rollingUpdate',rollingUpdate,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;



{ --------------------------------------------------------------------
  TZoneOperationsResource
  --------------------------------------------------------------------}


Class Function TZoneOperationsResource.ResourceName : String;

begin
  Result:='zoneOperations';
end;

Class Function TZoneOperationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TreplicapoolupdaterAPI;
end;

Function TZoneOperationsResource.Get(operation: string; project: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/operations/{operation}';
  _Methodid   = 'replicapoolupdater.zoneOperations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['operation',operation,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;



{ --------------------------------------------------------------------
  TReplicapoolupdaterAPI
  --------------------------------------------------------------------}

Class Function TReplicapoolupdaterAPI.APIName : String;

begin
  Result:='replicapoolupdater';
end;

Class Function TReplicapoolupdaterAPI.APIVersion : String;

begin
  Result:='v1beta1';
end;

Class Function TReplicapoolupdaterAPI.APIRevision : String;

begin
  Result:='20150326';
end;

Class Function TReplicapoolupdaterAPI.APIID : String;

begin
  Result:='replicapoolupdater:v1beta1';
end;

Class Function TReplicapoolupdaterAPI.APITitle : String;

begin
  Result:='Google Compute Engine Instance Group Updater API';
end;

Class Function TReplicapoolupdaterAPI.APIDescription : String;

begin
  Result:='The Google Compute Engine Instance Group Updater API provides services for updating groups of Compute Engine Instances.';
end;

Class Function TReplicapoolupdaterAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TReplicapoolupdaterAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TReplicapoolupdaterAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TReplicapoolupdaterAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TReplicapoolupdaterAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/compute/docs/instance-groups/manager/#applying_rolling_updates_using_the_updater_service';
end;

Class Function TReplicapoolupdaterAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TReplicapoolupdaterAPI.APIbasePath : string;

begin
  Result:='/replicapoolupdater/v1beta1/projects/';
end;

Class Function TReplicapoolupdaterAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/replicapoolupdater/v1beta1/projects/';
end;

Class Function TReplicapoolupdaterAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TReplicapoolupdaterAPI.APIservicePath : string;

begin
  Result:='replicapoolupdater/v1beta1/projects/';
end;

Class Function TReplicapoolupdaterAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TReplicapoolupdaterAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,3);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/replicapool';
  Result[1].Description:='View and manage replica pools';
  Result[2].Name:='https://www.googleapis.com/auth/replicapool.readonly';
  Result[2].Description:='View replica pools';
  
end;

Class Function TReplicapoolupdaterAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TReplicapoolupdaterAPI.RegisterAPIResources;

begin
  TInstanceUpdate.RegisterObject;
  TInstanceUpdateerror.RegisterObject;
  TInstanceUpdateerrorerrors.RegisterObject;
  TInstanceUpdateList.RegisterObject;
  TInstanceUpdateListitems.RegisterObject;
  TOperation.RegisterObject;
  TOperationerror.RegisterObject;
  TOperationerrorerrors.RegisterObject;
  TOperationwarnings.RegisterObject;
  TOperationwarningsdata.RegisterObject;
  TRollingUpdate.RegisterObject;
  TRollingUpdateerror.RegisterObject;
  TRollingUpdateerrorerrors.RegisterObject;
  TRollingUpdatepolicy.RegisterObject;
  TRollingUpdateList.RegisterObject;
  TRollingUpdateListitems.RegisterObject;
end;


Function TReplicapoolupdaterAPI.GetRollingUpdatesInstance : TRollingUpdatesResource;

begin
  if (FRollingUpdatesInstance=Nil) then
    FRollingUpdatesInstance:=CreateRollingUpdatesResource;
  Result:=FRollingUpdatesInstance;
end;

Function TReplicapoolupdaterAPI.CreateRollingUpdatesResource : TRollingUpdatesResource;

begin
  Result:=CreateRollingUpdatesResource(Self);
end;


Function TReplicapoolupdaterAPI.CreateRollingUpdatesResource(AOwner : TComponent) : TRollingUpdatesResource;

begin
  Result:=TRollingUpdatesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TReplicapoolupdaterAPI.GetZoneOperationsInstance : TZoneOperationsResource;

begin
  if (FZoneOperationsInstance=Nil) then
    FZoneOperationsInstance:=CreateZoneOperationsResource;
  Result:=FZoneOperationsInstance;
end;

Function TReplicapoolupdaterAPI.CreateZoneOperationsResource : TZoneOperationsResource;

begin
  Result:=CreateZoneOperationsResource(Self);
end;


Function TReplicapoolupdaterAPI.CreateZoneOperationsResource(AOwner : TComponent) : TZoneOperationsResource;

begin
  Result:=TZoneOperationsResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TReplicapoolupdaterAPI.RegisterAPI;
end.
