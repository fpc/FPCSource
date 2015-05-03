unit googlereplicapool;
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
  TInstanceGroupManager = class;
  TInstanceGroupManagerArray = Array of TInstanceGroupManager;
  TInstanceGroupManagertargetPools = class;
  TInstanceGroupManagertargetPoolsArray = Array of TInstanceGroupManagertargetPools;
  TInstanceGroupManagerList = class;
  TInstanceGroupManagerListArray = Array of TInstanceGroupManagerList;
  TInstanceGroupManagerListitems = class;
  TInstanceGroupManagerListitemsArray = Array of TInstanceGroupManagerListitems;
  TInstanceGroupManagersAbandonInstancesRequest = class;
  TInstanceGroupManagersAbandonInstancesRequestArray = Array of TInstanceGroupManagersAbandonInstancesRequest;
  TInstanceGroupManagersAbandonInstancesRequestinstances = class;
  TInstanceGroupManagersAbandonInstancesRequestinstancesArray = Array of TInstanceGroupManagersAbandonInstancesRequestinstances;
  TInstanceGroupManagersDeleteInstancesRequest = class;
  TInstanceGroupManagersDeleteInstancesRequestArray = Array of TInstanceGroupManagersDeleteInstancesRequest;
  TInstanceGroupManagersDeleteInstancesRequestinstances = class;
  TInstanceGroupManagersDeleteInstancesRequestinstancesArray = Array of TInstanceGroupManagersDeleteInstancesRequestinstances;
  TInstanceGroupManagersRecreateInstancesRequest = class;
  TInstanceGroupManagersRecreateInstancesRequestArray = Array of TInstanceGroupManagersRecreateInstancesRequest;
  TInstanceGroupManagersRecreateInstancesRequestinstances = class;
  TInstanceGroupManagersRecreateInstancesRequestinstancesArray = Array of TInstanceGroupManagersRecreateInstancesRequestinstances;
  TInstanceGroupManagersSetInstanceTemplateRequest = class;
  TInstanceGroupManagersSetInstanceTemplateRequestArray = Array of TInstanceGroupManagersSetInstanceTemplateRequest;
  TInstanceGroupManagersSetTargetPoolsRequest = class;
  TInstanceGroupManagersSetTargetPoolsRequestArray = Array of TInstanceGroupManagersSetTargetPoolsRequest;
  TInstanceGroupManagersSetTargetPoolsRequesttargetPools = class;
  TInstanceGroupManagersSetTargetPoolsRequesttargetPoolsArray = Array of TInstanceGroupManagersSetTargetPoolsRequesttargetPools;
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
  TOperationList = class;
  TOperationListArray = Array of TOperationList;
  TOperationListitems = class;
  TOperationListitemsArray = Array of TOperationListitems;
  
  { --------------------------------------------------------------------
    TInstanceGroupManager
    --------------------------------------------------------------------}
  
  TInstanceGroupManager = Class(TGoogleBaseObject)
  Private
    FbaseInstanceName : string;
    FcreationTimestamp : string;
    FcurrentSize : integer;
    Fdescription : string;
    Ffingerprint : string;
    Fgroup : string;
    Fid : string;
    FinstanceTemplate : string;
    Fkind : string;
    Fname : string;
    FselfLink : string;
    FtargetPools : TInstanceGroupManagertargetPools;
    FtargetSize : integer;
  Protected
    //Property setters
    Procedure SetbaseInstanceName(AIndex : Integer; AValue : string); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure SetcurrentSize(AIndex : Integer; AValue : integer); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : string); virtual;
    Procedure Setgroup(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetinstanceTemplate(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetPools(AIndex : Integer; AValue : TInstanceGroupManagertargetPools); virtual;
    Procedure SettargetSize(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property baseInstanceName : string Index 0 Read FbaseInstanceName Write SetbaseInstanceName;
    Property creationTimestamp : string Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property currentSize : integer Index 16 Read FcurrentSize Write SetcurrentSize;
    Property description : string Index 24 Read Fdescription Write Setdescription;
    Property fingerprint : string Index 32 Read Ffingerprint Write Setfingerprint;
    Property group : string Index 40 Read Fgroup Write Setgroup;
    Property id : string Index 48 Read Fid Write Setid;
    Property instanceTemplate : string Index 56 Read FinstanceTemplate Write SetinstanceTemplate;
    Property kind : string Index 64 Read Fkind Write Setkind;
    Property name : string Index 72 Read Fname Write Setname;
    Property selfLink : string Index 80 Read FselfLink Write SetselfLink;
    Property targetPools : TInstanceGroupManagertargetPools Index 88 Read FtargetPools Write SettargetPools;
    Property targetSize : integer Index 96 Read FtargetSize Write SettargetSize;
  end;
  TInstanceGroupManagerClass = Class of TInstanceGroupManager;
  
  { --------------------------------------------------------------------
    TInstanceGroupManagertargetPools
    --------------------------------------------------------------------}
  
  TInstanceGroupManagertargetPools = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInstanceGroupManagertargetPoolsClass = Class of TInstanceGroupManagertargetPools;
  
  { --------------------------------------------------------------------
    TInstanceGroupManagerList
    --------------------------------------------------------------------}
  
  TInstanceGroupManagerList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TInstanceGroupManagerListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TInstanceGroupManagerListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TInstanceGroupManagerListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TInstanceGroupManagerListClass = Class of TInstanceGroupManagerList;
  
  { --------------------------------------------------------------------
    TInstanceGroupManagerListitems
    --------------------------------------------------------------------}
  
  TInstanceGroupManagerListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInstanceGroupManagerListitemsClass = Class of TInstanceGroupManagerListitems;
  
  { --------------------------------------------------------------------
    TInstanceGroupManagersAbandonInstancesRequest
    --------------------------------------------------------------------}
  
  TInstanceGroupManagersAbandonInstancesRequest = Class(TGoogleBaseObject)
  Private
    Finstances : TInstanceGroupManagersAbandonInstancesRequestinstances;
  Protected
    //Property setters
    Procedure Setinstances(AIndex : Integer; AValue : TInstanceGroupManagersAbandonInstancesRequestinstances); virtual;
  Public
  Published
    Property instances : TInstanceGroupManagersAbandonInstancesRequestinstances Index 0 Read Finstances Write Setinstances;
  end;
  TInstanceGroupManagersAbandonInstancesRequestClass = Class of TInstanceGroupManagersAbandonInstancesRequest;
  
  { --------------------------------------------------------------------
    TInstanceGroupManagersAbandonInstancesRequestinstances
    --------------------------------------------------------------------}
  
  TInstanceGroupManagersAbandonInstancesRequestinstances = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInstanceGroupManagersAbandonInstancesRequestinstancesClass = Class of TInstanceGroupManagersAbandonInstancesRequestinstances;
  
  { --------------------------------------------------------------------
    TInstanceGroupManagersDeleteInstancesRequest
    --------------------------------------------------------------------}
  
  TInstanceGroupManagersDeleteInstancesRequest = Class(TGoogleBaseObject)
  Private
    Finstances : TInstanceGroupManagersDeleteInstancesRequestinstances;
  Protected
    //Property setters
    Procedure Setinstances(AIndex : Integer; AValue : TInstanceGroupManagersDeleteInstancesRequestinstances); virtual;
  Public
  Published
    Property instances : TInstanceGroupManagersDeleteInstancesRequestinstances Index 0 Read Finstances Write Setinstances;
  end;
  TInstanceGroupManagersDeleteInstancesRequestClass = Class of TInstanceGroupManagersDeleteInstancesRequest;
  
  { --------------------------------------------------------------------
    TInstanceGroupManagersDeleteInstancesRequestinstances
    --------------------------------------------------------------------}
  
  TInstanceGroupManagersDeleteInstancesRequestinstances = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInstanceGroupManagersDeleteInstancesRequestinstancesClass = Class of TInstanceGroupManagersDeleteInstancesRequestinstances;
  
  { --------------------------------------------------------------------
    TInstanceGroupManagersRecreateInstancesRequest
    --------------------------------------------------------------------}
  
  TInstanceGroupManagersRecreateInstancesRequest = Class(TGoogleBaseObject)
  Private
    Finstances : TInstanceGroupManagersRecreateInstancesRequestinstances;
  Protected
    //Property setters
    Procedure Setinstances(AIndex : Integer; AValue : TInstanceGroupManagersRecreateInstancesRequestinstances); virtual;
  Public
  Published
    Property instances : TInstanceGroupManagersRecreateInstancesRequestinstances Index 0 Read Finstances Write Setinstances;
  end;
  TInstanceGroupManagersRecreateInstancesRequestClass = Class of TInstanceGroupManagersRecreateInstancesRequest;
  
  { --------------------------------------------------------------------
    TInstanceGroupManagersRecreateInstancesRequestinstances
    --------------------------------------------------------------------}
  
  TInstanceGroupManagersRecreateInstancesRequestinstances = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInstanceGroupManagersRecreateInstancesRequestinstancesClass = Class of TInstanceGroupManagersRecreateInstancesRequestinstances;
  
  { --------------------------------------------------------------------
    TInstanceGroupManagersSetInstanceTemplateRequest
    --------------------------------------------------------------------}
  
  TInstanceGroupManagersSetInstanceTemplateRequest = Class(TGoogleBaseObject)
  Private
    FinstanceTemplate : string;
  Protected
    //Property setters
    Procedure SetinstanceTemplate(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property instanceTemplate : string Index 0 Read FinstanceTemplate Write SetinstanceTemplate;
  end;
  TInstanceGroupManagersSetInstanceTemplateRequestClass = Class of TInstanceGroupManagersSetInstanceTemplateRequest;
  
  { --------------------------------------------------------------------
    TInstanceGroupManagersSetTargetPoolsRequest
    --------------------------------------------------------------------}
  
  TInstanceGroupManagersSetTargetPoolsRequest = Class(TGoogleBaseObject)
  Private
    Ffingerprint : string;
    FtargetPools : TInstanceGroupManagersSetTargetPoolsRequesttargetPools;
  Protected
    //Property setters
    Procedure Setfingerprint(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetPools(AIndex : Integer; AValue : TInstanceGroupManagersSetTargetPoolsRequesttargetPools); virtual;
  Public
  Published
    Property fingerprint : string Index 0 Read Ffingerprint Write Setfingerprint;
    Property targetPools : TInstanceGroupManagersSetTargetPoolsRequesttargetPools Index 8 Read FtargetPools Write SettargetPools;
  end;
  TInstanceGroupManagersSetTargetPoolsRequestClass = Class of TInstanceGroupManagersSetTargetPoolsRequest;
  
  { --------------------------------------------------------------------
    TInstanceGroupManagersSetTargetPoolsRequesttargetPools
    --------------------------------------------------------------------}
  
  TInstanceGroupManagersSetTargetPoolsRequesttargetPools = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInstanceGroupManagersSetTargetPoolsRequesttargetPoolsClass = Class of TInstanceGroupManagersSetTargetPoolsRequesttargetPools;
  
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
    TOperationList
    --------------------------------------------------------------------}
  
  TOperationList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TOperationListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TOperationListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TOperationListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TOperationListClass = Class of TOperationList;
  
  { --------------------------------------------------------------------
    TOperationListitems
    --------------------------------------------------------------------}
  
  TOperationListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TOperationListitemsClass = Class of TOperationListitems;
  
  { --------------------------------------------------------------------
    TInstanceGroupManagersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TInstanceGroupManagersResource, method Insert
  
  TInstanceGroupManagersInsertOptions = Record
    size : integer;
  end;
  
  
  //Optional query Options for TInstanceGroupManagersResource, method List
  
  TInstanceGroupManagersListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TInstanceGroupManagersResource, method Resize
  
  TInstanceGroupManagersResizeOptions = Record
    size : integer;
  end;
  
  TInstanceGroupManagersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function AbandonInstances(instanceGroupManager: string; project: string; zone: string; aInstanceGroupManagersAbandonInstancesRequest : TInstanceGroupManagersAbandonInstancesRequest) : TOperation;
    Function Delete(instanceGroupManager: string; project: string; zone: string) : TOperation;
    Function DeleteInstances(instanceGroupManager: string; project: string; zone: string; aInstanceGroupManagersDeleteInstancesRequest : TInstanceGroupManagersDeleteInstancesRequest) : TOperation;
    Function Get(instanceGroupManager: string; project: string; zone: string) : TInstanceGroupManager;
    Function Insert(project: string; zone: string; aInstanceGroupManager : TInstanceGroupManager; AQuery : string  = '') : TOperation;
    Function Insert(project: string; zone: string; aInstanceGroupManager : TInstanceGroupManager; AQuery : TInstanceGroupManagersinsertOptions) : TOperation;
    Function List(project: string; zone: string; AQuery : string  = '') : TInstanceGroupManagerList;
    Function List(project: string; zone: string; AQuery : TInstanceGroupManagerslistOptions) : TInstanceGroupManagerList;
    Function RecreateInstances(instanceGroupManager: string; project: string; zone: string; aInstanceGroupManagersRecreateInstancesRequest : TInstanceGroupManagersRecreateInstancesRequest) : TOperation;
    Function Resize(instanceGroupManager: string; project: string; zone: string; AQuery : string  = '') : TOperation;
    Function Resize(instanceGroupManager: string; project: string; zone: string; AQuery : TInstanceGroupManagersresizeOptions) : TOperation;
    Function SetInstanceTemplate(instanceGroupManager: string; project: string; zone: string; aInstanceGroupManagersSetInstanceTemplateRequest : TInstanceGroupManagersSetInstanceTemplateRequest) : TOperation;
    Function SetTargetPools(instanceGroupManager: string; project: string; zone: string; aInstanceGroupManagersSetTargetPoolsRequest : TInstanceGroupManagersSetTargetPoolsRequest) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TZoneOperationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TZoneOperationsResource, method List
  
  TZoneOperationsListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TZoneOperationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(operation: string; project: string; zone: string) : TOperation;
    Function List(project: string; zone: string; AQuery : string  = '') : TOperationList;
    Function List(project: string; zone: string; AQuery : TZoneOperationslistOptions) : TOperationList;
  end;
  
  
  { --------------------------------------------------------------------
    TReplicapoolAPI
    --------------------------------------------------------------------}
  
  TReplicapoolAPI = Class(TGoogleAPI)
  Private
    FInstanceGroupManagersInstance : TInstanceGroupManagersResource;
    FZoneOperationsInstance : TZoneOperationsResource;
    Function GetInstanceGroupManagersInstance : TInstanceGroupManagersResource;virtual;
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
    Function CreateInstanceGroupManagersResource(AOwner : TComponent) : TInstanceGroupManagersResource;virtual;overload;
    Function CreateInstanceGroupManagersResource : TInstanceGroupManagersResource;virtual;overload;
    Function CreateZoneOperationsResource(AOwner : TComponent) : TZoneOperationsResource;virtual;overload;
    Function CreateZoneOperationsResource : TZoneOperationsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property InstanceGroupManagersResource : TInstanceGroupManagersResource Read GetInstanceGroupManagersInstance;
    Property ZoneOperationsResource : TZoneOperationsResource Read GetZoneOperationsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TInstanceGroupManager
  --------------------------------------------------------------------}


Procedure TInstanceGroupManager.SetbaseInstanceName(AIndex : Integer; AValue : string); 

begin
  If (FbaseInstanceName=AValue) then exit;
  FbaseInstanceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.SetcurrentSize(AIndex : Integer; AValue : integer); 

begin
  If (FcurrentSize=AValue) then exit;
  FcurrentSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.Setfingerprint(AIndex : Integer; AValue : string); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.Setgroup(AIndex : Integer; AValue : string); 

begin
  If (Fgroup=AValue) then exit;
  Fgroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.SetinstanceTemplate(AIndex : Integer; AValue : string); 

begin
  If (FinstanceTemplate=AValue) then exit;
  FinstanceTemplate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.SettargetPools(AIndex : Integer; AValue : TInstanceGroupManagertargetPools); 

begin
  If (FtargetPools=AValue) then exit;
  FtargetPools:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.SettargetSize(AIndex : Integer; AValue : integer); 

begin
  If (FtargetSize=AValue) then exit;
  FtargetSize:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceGroupManagertargetPools
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInstanceGroupManagerList
  --------------------------------------------------------------------}


Procedure TInstanceGroupManagerList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManagerList.Setitems(AIndex : Integer; AValue : TInstanceGroupManagerListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManagerList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManagerList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManagerList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceGroupManagerListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInstanceGroupManagersAbandonInstancesRequest
  --------------------------------------------------------------------}


Procedure TInstanceGroupManagersAbandonInstancesRequest.Setinstances(AIndex : Integer; AValue : TInstanceGroupManagersAbandonInstancesRequestinstances); 

begin
  If (Finstances=AValue) then exit;
  Finstances:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceGroupManagersAbandonInstancesRequestinstances
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInstanceGroupManagersDeleteInstancesRequest
  --------------------------------------------------------------------}


Procedure TInstanceGroupManagersDeleteInstancesRequest.Setinstances(AIndex : Integer; AValue : TInstanceGroupManagersDeleteInstancesRequestinstances); 

begin
  If (Finstances=AValue) then exit;
  Finstances:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceGroupManagersDeleteInstancesRequestinstances
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInstanceGroupManagersRecreateInstancesRequest
  --------------------------------------------------------------------}


Procedure TInstanceGroupManagersRecreateInstancesRequest.Setinstances(AIndex : Integer; AValue : TInstanceGroupManagersRecreateInstancesRequestinstances); 

begin
  If (Finstances=AValue) then exit;
  Finstances:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceGroupManagersRecreateInstancesRequestinstances
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInstanceGroupManagersSetInstanceTemplateRequest
  --------------------------------------------------------------------}


Procedure TInstanceGroupManagersSetInstanceTemplateRequest.SetinstanceTemplate(AIndex : Integer; AValue : string); 

begin
  If (FinstanceTemplate=AValue) then exit;
  FinstanceTemplate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceGroupManagersSetTargetPoolsRequest
  --------------------------------------------------------------------}


Procedure TInstanceGroupManagersSetTargetPoolsRequest.Setfingerprint(AIndex : Integer; AValue : string); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManagersSetTargetPoolsRequest.SettargetPools(AIndex : Integer; AValue : TInstanceGroupManagersSetTargetPoolsRequesttargetPools); 

begin
  If (FtargetPools=AValue) then exit;
  FtargetPools:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceGroupManagersSetTargetPoolsRequesttargetPools
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
  TOperationList
  --------------------------------------------------------------------}


Procedure TOperationList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.Setitems(AIndex : Integer; AValue : TOperationListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInstanceGroupManagersResource
  --------------------------------------------------------------------}


Class Function TInstanceGroupManagersResource.ResourceName : String;

begin
  Result:='instanceGroupManagers';
end;

Class Function TInstanceGroupManagersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TreplicapoolAPI;
end;

Function TInstanceGroupManagersResource.AbandonInstances(instanceGroupManager: string; project: string; zone: string; aInstanceGroupManagersAbandonInstancesRequest : TInstanceGroupManagersAbandonInstancesRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}/abandonInstances';
  _Methodid   = 'replicapool.instanceGroupManagers.abandonInstances';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instanceGroupManager',instanceGroupManager,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInstanceGroupManagersAbandonInstancesRequest,TOperation) as TOperation;
end;

Function TInstanceGroupManagersResource.Delete(instanceGroupManager: string; project: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}';
  _Methodid   = 'replicapool.instanceGroupManagers.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instanceGroupManager',instanceGroupManager,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TInstanceGroupManagersResource.DeleteInstances(instanceGroupManager: string; project: string; zone: string; aInstanceGroupManagersDeleteInstancesRequest : TInstanceGroupManagersDeleteInstancesRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}/deleteInstances';
  _Methodid   = 'replicapool.instanceGroupManagers.deleteInstances';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instanceGroupManager',instanceGroupManager,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInstanceGroupManagersDeleteInstancesRequest,TOperation) as TOperation;
end;

Function TInstanceGroupManagersResource.Get(instanceGroupManager: string; project: string; zone: string) : TInstanceGroupManager;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}';
  _Methodid   = 'replicapool.instanceGroupManagers.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instanceGroupManager',instanceGroupManager,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TInstanceGroupManager) as TInstanceGroupManager;
end;

Function TInstanceGroupManagersResource.Insert(project: string; zone: string; aInstanceGroupManager : TInstanceGroupManager; AQuery : string = '') : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instanceGroupManagers';
  _Methodid   = 'replicapool.instanceGroupManagers.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aInstanceGroupManager,TOperation) as TOperation;
end;


Function TInstanceGroupManagersResource.Insert(project: string; zone: string; aInstanceGroupManager : TInstanceGroupManager; AQuery : TInstanceGroupManagersinsertOptions) : TOperation;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'size',AQuery.size);
  Result:=Insert(project,zone,aInstanceGroupManager,_Q);
end;

Function TInstanceGroupManagersResource.List(project: string; zone: string; AQuery : string = '') : TInstanceGroupManagerList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/instanceGroupManagers';
  _Methodid   = 'replicapool.instanceGroupManagers.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TInstanceGroupManagerList) as TInstanceGroupManagerList;
end;


Function TInstanceGroupManagersResource.List(project: string; zone: string; AQuery : TInstanceGroupManagerslistOptions) : TInstanceGroupManagerList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,zone,_Q);
end;

Function TInstanceGroupManagersResource.RecreateInstances(instanceGroupManager: string; project: string; zone: string; aInstanceGroupManagersRecreateInstancesRequest : TInstanceGroupManagersRecreateInstancesRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}/recreateInstances';
  _Methodid   = 'replicapool.instanceGroupManagers.recreateInstances';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instanceGroupManager',instanceGroupManager,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInstanceGroupManagersRecreateInstancesRequest,TOperation) as TOperation;
end;

Function TInstanceGroupManagersResource.Resize(instanceGroupManager: string; project: string; zone: string; AQuery : string = '') : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}/resize';
  _Methodid   = 'replicapool.instanceGroupManagers.resize';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instanceGroupManager',instanceGroupManager,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TOperation) as TOperation;
end;


Function TInstanceGroupManagersResource.Resize(instanceGroupManager: string; project: string; zone: string; AQuery : TInstanceGroupManagersresizeOptions) : TOperation;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'size',AQuery.size);
  Result:=Resize(instanceGroupManager,project,zone,_Q);
end;

Function TInstanceGroupManagersResource.SetInstanceTemplate(instanceGroupManager: string; project: string; zone: string; aInstanceGroupManagersSetInstanceTemplateRequest : TInstanceGroupManagersSetInstanceTemplateRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}/setInstanceTemplate';
  _Methodid   = 'replicapool.instanceGroupManagers.setInstanceTemplate';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instanceGroupManager',instanceGroupManager,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInstanceGroupManagersSetInstanceTemplateRequest,TOperation) as TOperation;
end;

Function TInstanceGroupManagersResource.SetTargetPools(instanceGroupManager: string; project: string; zone: string; aInstanceGroupManagersSetTargetPoolsRequest : TInstanceGroupManagersSetTargetPoolsRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}/setTargetPools';
  _Methodid   = 'replicapool.instanceGroupManagers.setTargetPools';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instanceGroupManager',instanceGroupManager,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInstanceGroupManagersSetTargetPoolsRequest,TOperation) as TOperation;
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
  Result:=TreplicapoolAPI;
end;

Function TZoneOperationsResource.Get(operation: string; project: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/operations/{operation}';
  _Methodid   = 'replicapool.zoneOperations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['operation',operation,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TZoneOperationsResource.List(project: string; zone: string; AQuery : string = '') : TOperationList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/operations';
  _Methodid   = 'replicapool.zoneOperations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TOperationList) as TOperationList;
end;


Function TZoneOperationsResource.List(project: string; zone: string; AQuery : TZoneOperationslistOptions) : TOperationList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,zone,_Q);
end;



{ --------------------------------------------------------------------
  TReplicapoolAPI
  --------------------------------------------------------------------}

Class Function TReplicapoolAPI.APIName : String;

begin
  Result:='replicapool';
end;

Class Function TReplicapoolAPI.APIVersion : String;

begin
  Result:='v1beta2';
end;

Class Function TReplicapoolAPI.APIRevision : String;

begin
  Result:='20150223';
end;

Class Function TReplicapoolAPI.APIID : String;

begin
  Result:='replicapool:v1beta2';
end;

Class Function TReplicapoolAPI.APITitle : String;

begin
  Result:='Google Compute Engine Instance Group Manager API';
end;

Class Function TReplicapoolAPI.APIDescription : String;

begin
  Result:='The Google Compute Engine Instance Group Manager API provides groups of homogenous Compute Engine Instances.';
end;

Class Function TReplicapoolAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TReplicapoolAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TReplicapoolAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TReplicapoolAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TReplicapoolAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/compute/docs/instance-groups/manager/v1beta2';
end;

Class Function TReplicapoolAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TReplicapoolAPI.APIbasePath : string;

begin
  Result:='/replicapool/v1beta2/projects/';
end;

Class Function TReplicapoolAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/replicapool/v1beta2/projects/';
end;

Class Function TReplicapoolAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TReplicapoolAPI.APIservicePath : string;

begin
  Result:='replicapool/v1beta2/projects/';
end;

Class Function TReplicapoolAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TReplicapoolAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,3);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/compute';
  Result[1].Description:='View and manage your Google Compute Engine resources';
  Result[2].Name:='https://www.googleapis.com/auth/compute.readonly';
  Result[2].Description:='View your Google Compute Engine resources';
  
end;

Class Function TReplicapoolAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TReplicapoolAPI.RegisterAPIResources;

begin
  TInstanceGroupManager.RegisterObject;
  TInstanceGroupManagertargetPools.RegisterObject;
  TInstanceGroupManagerList.RegisterObject;
  TInstanceGroupManagerListitems.RegisterObject;
  TInstanceGroupManagersAbandonInstancesRequest.RegisterObject;
  TInstanceGroupManagersAbandonInstancesRequestinstances.RegisterObject;
  TInstanceGroupManagersDeleteInstancesRequest.RegisterObject;
  TInstanceGroupManagersDeleteInstancesRequestinstances.RegisterObject;
  TInstanceGroupManagersRecreateInstancesRequest.RegisterObject;
  TInstanceGroupManagersRecreateInstancesRequestinstances.RegisterObject;
  TInstanceGroupManagersSetInstanceTemplateRequest.RegisterObject;
  TInstanceGroupManagersSetTargetPoolsRequest.RegisterObject;
  TInstanceGroupManagersSetTargetPoolsRequesttargetPools.RegisterObject;
  TOperation.RegisterObject;
  TOperationerror.RegisterObject;
  TOperationerrorerrors.RegisterObject;
  TOperationwarnings.RegisterObject;
  TOperationwarningsdata.RegisterObject;
  TOperationList.RegisterObject;
  TOperationListitems.RegisterObject;
end;


Function TReplicapoolAPI.GetInstanceGroupManagersInstance : TInstanceGroupManagersResource;

begin
  if (FInstanceGroupManagersInstance=Nil) then
    FInstanceGroupManagersInstance:=CreateInstanceGroupManagersResource;
  Result:=FInstanceGroupManagersInstance;
end;

Function TReplicapoolAPI.CreateInstanceGroupManagersResource : TInstanceGroupManagersResource;

begin
  Result:=CreateInstanceGroupManagersResource(Self);
end;


Function TReplicapoolAPI.CreateInstanceGroupManagersResource(AOwner : TComponent) : TInstanceGroupManagersResource;

begin
  Result:=TInstanceGroupManagersResource.Create(AOwner);
  Result.API:=Self;
end;



Function TReplicapoolAPI.GetZoneOperationsInstance : TZoneOperationsResource;

begin
  if (FZoneOperationsInstance=Nil) then
    FZoneOperationsInstance:=CreateZoneOperationsResource;
  Result:=FZoneOperationsInstance;
end;

Function TReplicapoolAPI.CreateZoneOperationsResource : TZoneOperationsResource;

begin
  Result:=CreateZoneOperationsResource(Self);
end;


Function TReplicapoolAPI.CreateZoneOperationsResource(AOwner : TComponent) : TZoneOperationsResource;

begin
  Result:=TZoneOperationsResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TReplicapoolAPI.RegisterAPI;
end.
