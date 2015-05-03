unit googledeploymentmanager;
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
  TDeployment = class;
  TDeploymentArray = Array of TDeployment;
  TDeploymentsListResponse = class;
  TDeploymentsListResponseArray = Array of TDeploymentsListResponse;
  TDeploymentsListResponsedeployments = class;
  TDeploymentsListResponsedeploymentsArray = Array of TDeploymentsListResponsedeployments;
  TManifest = class;
  TManifestArray = Array of TManifest;
  TManifestsListResponse = class;
  TManifestsListResponseArray = Array of TManifestsListResponse;
  TManifestsListResponsemanifests = class;
  TManifestsListResponsemanifestsArray = Array of TManifestsListResponsemanifests;
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
  TOperationsListResponse = class;
  TOperationsListResponseArray = Array of TOperationsListResponse;
  TOperationsListResponseoperations = class;
  TOperationsListResponseoperationsArray = Array of TOperationsListResponseoperations;
  TResource = class;
  TResourceArray = Array of TResource;
  TResourceerrors = class;
  TResourceerrorsArray = Array of TResourceerrors;
  TResourcesListResponse = class;
  TResourcesListResponseArray = Array of TResourcesListResponse;
  TResourcesListResponseresources = class;
  TResourcesListResponseresourcesArray = Array of TResourcesListResponseresources;
  TType = class;
  TTypeArray = Array of TType;
  TTypesListResponse = class;
  TTypesListResponseArray = Array of TTypesListResponse;
  TTypesListResponsetypes = class;
  TTypesListResponsetypesArray = Array of TTypesListResponsetypes;
  
  { --------------------------------------------------------------------
    TDeployment
    --------------------------------------------------------------------}
  
  TDeployment = Class(TGoogleBaseObject)
  Private
    Fdescription : string;
    Fid : string;
    Fmanifest : string;
    Fname : string;
    FtargetConfig : string;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setmanifest(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetConfig(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property description : string Index 0 Read Fdescription Write Setdescription;
    Property id : string Index 8 Read Fid Write Setid;
    Property manifest : string Index 16 Read Fmanifest Write Setmanifest;
    Property name : string Index 24 Read Fname Write Setname;
    Property targetConfig : string Index 32 Read FtargetConfig Write SettargetConfig;
  end;
  TDeploymentClass = Class of TDeployment;
  
  { --------------------------------------------------------------------
    TDeploymentsListResponse
    --------------------------------------------------------------------}
  
  TDeploymentsListResponse = Class(TGoogleBaseObject)
  Private
    Fdeployments : TDeploymentsListResponsedeployments;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setdeployments(AIndex : Integer; AValue : TDeploymentsListResponsedeployments); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property deployments : TDeploymentsListResponsedeployments Index 0 Read Fdeployments Write Setdeployments;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TDeploymentsListResponseClass = Class of TDeploymentsListResponse;
  
  { --------------------------------------------------------------------
    TDeploymentsListResponsedeployments
    --------------------------------------------------------------------}
  
  TDeploymentsListResponsedeployments = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDeploymentsListResponsedeploymentsClass = Class of TDeploymentsListResponsedeployments;
  
  { --------------------------------------------------------------------
    TManifest
    --------------------------------------------------------------------}
  
  TManifest = Class(TGoogleBaseObject)
  Private
    Fconfig : string;
    FevaluatedConfig : string;
    Fid : string;
    Fname : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setconfig(AIndex : Integer; AValue : string); virtual;
    Procedure SetevaluatedConfig(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property config : string Index 0 Read Fconfig Write Setconfig;
    Property evaluatedConfig : string Index 8 Read FevaluatedConfig Write SetevaluatedConfig;
    Property id : string Index 16 Read Fid Write Setid;
    Property name : string Index 24 Read Fname Write Setname;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TManifestClass = Class of TManifest;
  
  { --------------------------------------------------------------------
    TManifestsListResponse
    --------------------------------------------------------------------}
  
  TManifestsListResponse = Class(TGoogleBaseObject)
  Private
    Fmanifests : TManifestsListResponsemanifests;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setmanifests(AIndex : Integer; AValue : TManifestsListResponsemanifests); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property manifests : TManifestsListResponsemanifests Index 0 Read Fmanifests Write Setmanifests;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TManifestsListResponseClass = Class of TManifestsListResponse;
  
  { --------------------------------------------------------------------
    TManifestsListResponsemanifests
    --------------------------------------------------------------------}
  
  TManifestsListResponsemanifests = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TManifestsListResponsemanifestsClass = Class of TManifestsListResponsemanifests;
  
  { --------------------------------------------------------------------
    TOperation
    --------------------------------------------------------------------}
  
  TOperation = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : string;
    FendTime : string;
    Ferror : TOperationerror;
    FhttpErrorMessage : string;
    FhttpErrorStatusCode : integer;
    Fid : string;
    FinsertTime : string;
    Fname : string;
    FoperationType : string;
    Fprogress : integer;
    FselfLink : string;
    FstartTime : string;
    Fstatus : string;
    FstatusMessage : string;
    FtargetId : string;
    FtargetLink : string;
    Fuser : string;
    Fwarnings : TOperationwarnings;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure SetendTime(AIndex : Integer; AValue : string); virtual;
    Procedure Seterror(AIndex : Integer; AValue : TOperationerror); virtual;
    Procedure SethttpErrorMessage(AIndex : Integer; AValue : string); virtual;
    Procedure SethttpErrorStatusCode(AIndex : Integer; AValue : integer); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetinsertTime(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetoperationType(AIndex : Integer; AValue : string); virtual;
    Procedure Setprogress(AIndex : Integer; AValue : integer); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure SetstatusMessage(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetId(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setuser(AIndex : Integer; AValue : string); virtual;
    Procedure Setwarnings(AIndex : Integer; AValue : TOperationwarnings); virtual;
  Public
  Published
    Property creationTimestamp : string Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property endTime : string Index 8 Read FendTime Write SetendTime;
    Property error : TOperationerror Index 16 Read Ferror Write Seterror;
    Property httpErrorMessage : string Index 24 Read FhttpErrorMessage Write SethttpErrorMessage;
    Property httpErrorStatusCode : integer Index 32 Read FhttpErrorStatusCode Write SethttpErrorStatusCode;
    Property id : string Index 40 Read Fid Write Setid;
    Property insertTime : string Index 48 Read FinsertTime Write SetinsertTime;
    Property name : string Index 56 Read Fname Write Setname;
    Property operationType : string Index 64 Read FoperationType Write SetoperationType;
    Property progress : integer Index 72 Read Fprogress Write Setprogress;
    Property selfLink : string Index 80 Read FselfLink Write SetselfLink;
    Property startTime : string Index 88 Read FstartTime Write SetstartTime;
    Property status : string Index 96 Read Fstatus Write Setstatus;
    Property statusMessage : string Index 104 Read FstatusMessage Write SetstatusMessage;
    Property targetId : string Index 112 Read FtargetId Write SettargetId;
    Property targetLink : string Index 120 Read FtargetLink Write SettargetLink;
    Property user : string Index 128 Read Fuser Write Setuser;
    Property warnings : TOperationwarnings Index 136 Read Fwarnings Write Setwarnings;
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
    Fcode : TJSONSchema;
    Fdata : TOperationwarningsdata;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : TJSONSchema); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TOperationwarningsdata); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : TJSONSchema Index 0 Read Fcode Write Setcode;
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
    TOperationsListResponse
    --------------------------------------------------------------------}
  
  TOperationsListResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : string;
    Foperations : TOperationsListResponseoperations;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setoperations(AIndex : Integer; AValue : TOperationsListResponseoperations); virtual;
  Public
  Published
    Property nextPageToken : string Index 0 Read FnextPageToken Write SetnextPageToken;
    Property operations : TOperationsListResponseoperations Index 8 Read Foperations Write Setoperations;
  end;
  TOperationsListResponseClass = Class of TOperationsListResponse;
  
  { --------------------------------------------------------------------
    TOperationsListResponseoperations
    --------------------------------------------------------------------}
  
  TOperationsListResponseoperations = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TOperationsListResponseoperationsClass = Class of TOperationsListResponseoperations;
  
  { --------------------------------------------------------------------
    TResource
    --------------------------------------------------------------------}
  
  TResource = Class(TGoogleBaseObject)
  Private
    Ferrors : TResourceerrors;
    Fid : string;
    Fintent : string;
    Fmanifest : string;
    Fname : string;
    Fstate : string;
    F_type : string;
    Furl : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Seterrors(AIndex : Integer; AValue : TResourceerrors); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setintent(AIndex : Integer; AValue : string); virtual;
    Procedure Setmanifest(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setstate(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property errors : TResourceerrors Index 0 Read Ferrors Write Seterrors;
    Property id : string Index 8 Read Fid Write Setid;
    Property intent : string Index 16 Read Fintent Write Setintent;
    Property manifest : string Index 24 Read Fmanifest Write Setmanifest;
    Property name : string Index 32 Read Fname Write Setname;
    Property state : string Index 40 Read Fstate Write Setstate;
    Property _type : string Index 48 Read F_type Write Set_type;
    Property url : string Index 56 Read Furl Write Seturl;
  end;
  TResourceClass = Class of TResource;
  
  { --------------------------------------------------------------------
    TResourceerrors
    --------------------------------------------------------------------}
  
  TResourceerrors = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TResourceerrorsClass = Class of TResourceerrors;
  
  { --------------------------------------------------------------------
    TResourcesListResponse
    --------------------------------------------------------------------}
  
  TResourcesListResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : string;
    Fresources : TResourcesListResponseresources;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TResourcesListResponseresources); virtual;
  Public
  Published
    Property nextPageToken : string Index 0 Read FnextPageToken Write SetnextPageToken;
    Property resources : TResourcesListResponseresources Index 8 Read Fresources Write Setresources;
  end;
  TResourcesListResponseClass = Class of TResourcesListResponse;
  
  { --------------------------------------------------------------------
    TResourcesListResponseresources
    --------------------------------------------------------------------}
  
  TResourcesListResponseresources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TResourcesListResponseresourcesClass = Class of TResourcesListResponseresources;
  
  { --------------------------------------------------------------------
    TType
    --------------------------------------------------------------------}
  
  TType = Class(TGoogleBaseObject)
  Private
    Fname : string;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
  end;
  TTypeClass = Class of TType;
  
  { --------------------------------------------------------------------
    TTypesListResponse
    --------------------------------------------------------------------}
  
  TTypesListResponse = Class(TGoogleBaseObject)
  Private
    Ftypes : TTypesListResponsetypes;
  Protected
    //Property setters
    Procedure Settypes(AIndex : Integer; AValue : TTypesListResponsetypes); virtual;
  Public
  Published
    Property types : TTypesListResponsetypes Index 0 Read Ftypes Write Settypes;
  end;
  TTypesListResponseClass = Class of TTypesListResponse;
  
  { --------------------------------------------------------------------
    TTypesListResponsetypes
    --------------------------------------------------------------------}
  
  TTypesListResponsetypes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTypesListResponsetypesClass = Class of TTypesListResponsetypes;
  
  { --------------------------------------------------------------------
    TDeploymentsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDeploymentsResource, method List
  
  TDeploymentsListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  TDeploymentsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(deployment: string; project: string) : TOperation;
    Function Get(deployment: string; project: string) : TDeployment;
    Function Insert(project: string; aDeployment : TDeployment) : TOperation;
    Function List(project: string; AQuery : string  = '') : TDeploymentsListResponse;
    Function List(project: string; AQuery : TDeploymentslistOptions) : TDeploymentsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TManifestsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TManifestsResource, method List
  
  TManifestsListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  TManifestsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(deployment: string; manifest: string; project: string) : TManifest;
    Function List(deployment: string; project: string; AQuery : string  = '') : TManifestsListResponse;
    Function List(deployment: string; project: string; AQuery : TManifestslistOptions) : TManifestsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TOperationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TOperationsResource, method List
  
  TOperationsListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  TOperationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(operation: string; project: string) : TOperation;
    Function List(project: string; AQuery : string  = '') : TOperationsListResponse;
    Function List(project: string; AQuery : TOperationslistOptions) : TOperationsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TResourcesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TResourcesResource, method List
  
  TResourcesListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  TResourcesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(deployment: string; project: string; resource: string) : TResource;
    Function List(deployment: string; project: string; AQuery : string  = '') : TResourcesListResponse;
    Function List(deployment: string; project: string; AQuery : TResourceslistOptions) : TResourcesListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TTypesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTypesResource, method List
  
  TTypesListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  TTypesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(project: string; AQuery : string  = '') : TTypesListResponse;
    Function List(project: string; AQuery : TTypeslistOptions) : TTypesListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TDeploymentmanagerAPI
    --------------------------------------------------------------------}
  
  TDeploymentmanagerAPI = Class(TGoogleAPI)
  Private
    FDeploymentsInstance : TDeploymentsResource;
    FManifestsInstance : TManifestsResource;
    FOperationsInstance : TOperationsResource;
    FResourcesInstance : TResourcesResource;
    FTypesInstance : TTypesResource;
    Function GetDeploymentsInstance : TDeploymentsResource;virtual;
    Function GetManifestsInstance : TManifestsResource;virtual;
    Function GetOperationsInstance : TOperationsResource;virtual;
    Function GetResourcesInstance : TResourcesResource;virtual;
    Function GetTypesInstance : TTypesResource;virtual;
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
    Function CreateDeploymentsResource(AOwner : TComponent) : TDeploymentsResource;virtual;overload;
    Function CreateDeploymentsResource : TDeploymentsResource;virtual;overload;
    Function CreateManifestsResource(AOwner : TComponent) : TManifestsResource;virtual;overload;
    Function CreateManifestsResource : TManifestsResource;virtual;overload;
    Function CreateOperationsResource(AOwner : TComponent) : TOperationsResource;virtual;overload;
    Function CreateOperationsResource : TOperationsResource;virtual;overload;
    Function CreateResourcesResource(AOwner : TComponent) : TResourcesResource;virtual;overload;
    Function CreateResourcesResource : TResourcesResource;virtual;overload;
    Function CreateTypesResource(AOwner : TComponent) : TTypesResource;virtual;overload;
    Function CreateTypesResource : TTypesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property DeploymentsResource : TDeploymentsResource Read GetDeploymentsInstance;
    Property ManifestsResource : TManifestsResource Read GetManifestsInstance;
    Property OperationsResource : TOperationsResource Read GetOperationsInstance;
    Property ResourcesResource : TResourcesResource Read GetResourcesInstance;
    Property TypesResource : TTypesResource Read GetTypesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TDeployment
  --------------------------------------------------------------------}


Procedure TDeployment.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.Setmanifest(AIndex : Integer; AValue : string); 

begin
  If (Fmanifest=AValue) then exit;
  Fmanifest:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.SettargetConfig(AIndex : Integer; AValue : string); 

begin
  If (FtargetConfig=AValue) then exit;
  FtargetConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDeploymentsListResponse
  --------------------------------------------------------------------}


Procedure TDeploymentsListResponse.Setdeployments(AIndex : Integer; AValue : TDeploymentsListResponsedeployments); 

begin
  If (Fdeployments=AValue) then exit;
  Fdeployments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeploymentsListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDeploymentsListResponsedeployments
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TManifest
  --------------------------------------------------------------------}


Procedure TManifest.Setconfig(AIndex : Integer; AValue : string); 

begin
  If (Fconfig=AValue) then exit;
  Fconfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManifest.SetevaluatedConfig(AIndex : Integer; AValue : string); 

begin
  If (FevaluatedConfig=AValue) then exit;
  FevaluatedConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManifest.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManifest.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManifest.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TManifestsListResponse
  --------------------------------------------------------------------}


Procedure TManifestsListResponse.Setmanifests(AIndex : Integer; AValue : TManifestsListResponsemanifests); 

begin
  If (Fmanifests=AValue) then exit;
  Fmanifests:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManifestsListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TManifestsListResponsemanifests
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TOperation
  --------------------------------------------------------------------}


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


Procedure TOperationwarnings.Setcode(AIndex : Integer; AValue : TJSONSchema); 

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
  TOperationsListResponse
  --------------------------------------------------------------------}


Procedure TOperationsListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationsListResponse.Setoperations(AIndex : Integer; AValue : TOperationsListResponseoperations); 

begin
  If (Foperations=AValue) then exit;
  Foperations:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationsListResponseoperations
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TResource
  --------------------------------------------------------------------}


Procedure TResource.Seterrors(AIndex : Integer; AValue : TResourceerrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResource.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResource.Setintent(AIndex : Integer; AValue : string); 

begin
  If (Fintent=AValue) then exit;
  Fintent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResource.Setmanifest(AIndex : Integer; AValue : string); 

begin
  If (Fmanifest=AValue) then exit;
  Fmanifest:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResource.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResource.Setstate(AIndex : Integer; AValue : string); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResource.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResource.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TResource.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TResourceerrors
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TResourcesListResponse
  --------------------------------------------------------------------}


Procedure TResourcesListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourcesListResponse.Setresources(AIndex : Integer; AValue : TResourcesListResponseresources); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResourcesListResponseresources
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TType
  --------------------------------------------------------------------}


Procedure TType.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTypesListResponse
  --------------------------------------------------------------------}


Procedure TTypesListResponse.Settypes(AIndex : Integer; AValue : TTypesListResponsetypes); 

begin
  If (Ftypes=AValue) then exit;
  Ftypes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTypesListResponsetypes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDeploymentsResource
  --------------------------------------------------------------------}


Class Function TDeploymentsResource.ResourceName : String;

begin
  Result:='deployments';
end;

Class Function TDeploymentsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdeploymentmanagerAPI;
end;

Function TDeploymentsResource.Delete(deployment: string; project: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/global/deployments/{deployment}';
  _Methodid   = 'deploymentmanager.deployments.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['deployment',deployment,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TDeploymentsResource.Get(deployment: string; project: string) : TDeployment;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/deployments/{deployment}';
  _Methodid   = 'deploymentmanager.deployments.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['deployment',deployment,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDeployment) as TDeployment;
end;

Function TDeploymentsResource.Insert(project: string; aDeployment : TDeployment) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/deployments';
  _Methodid   = 'deploymentmanager.deployments.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDeployment,TOperation) as TOperation;
end;

Function TDeploymentsResource.List(project: string; AQuery : string = '') : TDeploymentsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/deployments';
  _Methodid   = 'deploymentmanager.deployments.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TDeploymentsListResponse) as TDeploymentsListResponse;
end;


Function TDeploymentsResource.List(project: string; AQuery : TDeploymentslistOptions) : TDeploymentsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;



{ --------------------------------------------------------------------
  TManifestsResource
  --------------------------------------------------------------------}


Class Function TManifestsResource.ResourceName : String;

begin
  Result:='manifests';
end;

Class Function TManifestsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdeploymentmanagerAPI;
end;

Function TManifestsResource.Get(deployment: string; manifest: string; project: string) : TManifest;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/deployments/{deployment}/manifests/{manifest}';
  _Methodid   = 'deploymentmanager.manifests.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['deployment',deployment,'manifest',manifest,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TManifest) as TManifest;
end;

Function TManifestsResource.List(deployment: string; project: string; AQuery : string = '') : TManifestsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/deployments/{deployment}/manifests';
  _Methodid   = 'deploymentmanager.manifests.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['deployment',deployment,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TManifestsListResponse) as TManifestsListResponse;
end;


Function TManifestsResource.List(deployment: string; project: string; AQuery : TManifestslistOptions) : TManifestsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(deployment,project,_Q);
end;



{ --------------------------------------------------------------------
  TOperationsResource
  --------------------------------------------------------------------}


Class Function TOperationsResource.ResourceName : String;

begin
  Result:='operations';
end;

Class Function TOperationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdeploymentmanagerAPI;
end;

Function TOperationsResource.Get(operation: string; project: string) : TOperation;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/operations/{operation}';
  _Methodid   = 'deploymentmanager.operations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['operation',operation,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TOperationsResource.List(project: string; AQuery : string = '') : TOperationsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/operations';
  _Methodid   = 'deploymentmanager.operations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TOperationsListResponse) as TOperationsListResponse;
end;


Function TOperationsResource.List(project: string; AQuery : TOperationslistOptions) : TOperationsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;



{ --------------------------------------------------------------------
  TResourcesResource
  --------------------------------------------------------------------}


Class Function TResourcesResource.ResourceName : String;

begin
  Result:='resources';
end;

Class Function TResourcesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdeploymentmanagerAPI;
end;

Function TResourcesResource.Get(deployment: string; project: string; resource: string) : TResource;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/deployments/{deployment}/resources/{resource}';
  _Methodid   = 'deploymentmanager.resources.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['deployment',deployment,'project',project,'resource',resource]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TResource) as TResource;
end;

Function TResourcesResource.List(deployment: string; project: string; AQuery : string = '') : TResourcesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/deployments/{deployment}/resources';
  _Methodid   = 'deploymentmanager.resources.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['deployment',deployment,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TResourcesListResponse) as TResourcesListResponse;
end;


Function TResourcesResource.List(deployment: string; project: string; AQuery : TResourceslistOptions) : TResourcesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(deployment,project,_Q);
end;



{ --------------------------------------------------------------------
  TTypesResource
  --------------------------------------------------------------------}


Class Function TTypesResource.ResourceName : String;

begin
  Result:='types';
end;

Class Function TTypesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdeploymentmanagerAPI;
end;

Function TTypesResource.List(project: string; AQuery : string = '') : TTypesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/types';
  _Methodid   = 'deploymentmanager.types.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTypesListResponse) as TTypesListResponse;
end;


Function TTypesResource.List(project: string; AQuery : TTypeslistOptions) : TTypesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;



{ --------------------------------------------------------------------
  TDeploymentmanagerAPI
  --------------------------------------------------------------------}

Class Function TDeploymentmanagerAPI.APIName : String;

begin
  Result:='deploymentmanager';
end;

Class Function TDeploymentmanagerAPI.APIVersion : String;

begin
  Result:='v2beta1';
end;

Class Function TDeploymentmanagerAPI.APIRevision : String;

begin
  Result:='20150311';
end;

Class Function TDeploymentmanagerAPI.APIID : String;

begin
  Result:='deploymentmanager:v2beta1';
end;

Class Function TDeploymentmanagerAPI.APITitle : String;

begin
  Result:='Google Cloud Deployment Manager API V2';
end;

Class Function TDeploymentmanagerAPI.APIDescription : String;

begin
  Result:='The Deployment Manager API allows users to declaratively configure, deploy and run complex solutions on the Google Cloud Platform.';
end;

Class Function TDeploymentmanagerAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TDeploymentmanagerAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TDeploymentmanagerAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TDeploymentmanagerAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TDeploymentmanagerAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/deployment-manager/';
end;

Class Function TDeploymentmanagerAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TDeploymentmanagerAPI.APIbasePath : string;

begin
  Result:='/deploymentmanager/v2beta1/projects/';
end;

Class Function TDeploymentmanagerAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/deploymentmanager/v2beta1/projects/';
end;

Class Function TDeploymentmanagerAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TDeploymentmanagerAPI.APIservicePath : string;

begin
  Result:='deploymentmanager/v2beta1/projects/';
end;

Class Function TDeploymentmanagerAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TDeploymentmanagerAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,3);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/ndev.cloudman';
  Result[1].Description:='View and manage your Google Cloud Platform management resources and deployment status information';
  Result[2].Name:='https://www.googleapis.com/auth/ndev.cloudman.readonly';
  Result[2].Description:='View your Google Cloud Platform management resources and deployment status information';
  
end;

Class Function TDeploymentmanagerAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TDeploymentmanagerAPI.RegisterAPIResources;

begin
  TDeployment.RegisterObject;
  TDeploymentsListResponse.RegisterObject;
  TDeploymentsListResponsedeployments.RegisterObject;
  TManifest.RegisterObject;
  TManifestsListResponse.RegisterObject;
  TManifestsListResponsemanifests.RegisterObject;
  TOperation.RegisterObject;
  TOperationerror.RegisterObject;
  TOperationerrorerrors.RegisterObject;
  TOperationwarnings.RegisterObject;
  TOperationwarningsdata.RegisterObject;
  TOperationsListResponse.RegisterObject;
  TOperationsListResponseoperations.RegisterObject;
  TResource.RegisterObject;
  TResourceerrors.RegisterObject;
  TResourcesListResponse.RegisterObject;
  TResourcesListResponseresources.RegisterObject;
  TType.RegisterObject;
  TTypesListResponse.RegisterObject;
  TTypesListResponsetypes.RegisterObject;
end;


Function TDeploymentmanagerAPI.GetDeploymentsInstance : TDeploymentsResource;

begin
  if (FDeploymentsInstance=Nil) then
    FDeploymentsInstance:=CreateDeploymentsResource;
  Result:=FDeploymentsInstance;
end;

Function TDeploymentmanagerAPI.CreateDeploymentsResource : TDeploymentsResource;

begin
  Result:=CreateDeploymentsResource(Self);
end;


Function TDeploymentmanagerAPI.CreateDeploymentsResource(AOwner : TComponent) : TDeploymentsResource;

begin
  Result:=TDeploymentsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDeploymentmanagerAPI.GetManifestsInstance : TManifestsResource;

begin
  if (FManifestsInstance=Nil) then
    FManifestsInstance:=CreateManifestsResource;
  Result:=FManifestsInstance;
end;

Function TDeploymentmanagerAPI.CreateManifestsResource : TManifestsResource;

begin
  Result:=CreateManifestsResource(Self);
end;


Function TDeploymentmanagerAPI.CreateManifestsResource(AOwner : TComponent) : TManifestsResource;

begin
  Result:=TManifestsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDeploymentmanagerAPI.GetOperationsInstance : TOperationsResource;

begin
  if (FOperationsInstance=Nil) then
    FOperationsInstance:=CreateOperationsResource;
  Result:=FOperationsInstance;
end;

Function TDeploymentmanagerAPI.CreateOperationsResource : TOperationsResource;

begin
  Result:=CreateOperationsResource(Self);
end;


Function TDeploymentmanagerAPI.CreateOperationsResource(AOwner : TComponent) : TOperationsResource;

begin
  Result:=TOperationsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDeploymentmanagerAPI.GetResourcesInstance : TResourcesResource;

begin
  if (FResourcesInstance=Nil) then
    FResourcesInstance:=CreateResourcesResource;
  Result:=FResourcesInstance;
end;

Function TDeploymentmanagerAPI.CreateResourcesResource : TResourcesResource;

begin
  Result:=CreateResourcesResource(Self);
end;


Function TDeploymentmanagerAPI.CreateResourcesResource(AOwner : TComponent) : TResourcesResource;

begin
  Result:=TResourcesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDeploymentmanagerAPI.GetTypesInstance : TTypesResource;

begin
  if (FTypesInstance=Nil) then
    FTypesInstance:=CreateTypesResource;
  Result:=FTypesInstance;
end;

Function TDeploymentmanagerAPI.CreateTypesResource : TTypesResource;

begin
  Result:=CreateTypesResource(Self);
end;


Function TDeploymentmanagerAPI.CreateTypesResource(AOwner : TComponent) : TTypesResource;

begin
  Result:=TTypesResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TDeploymentmanagerAPI.RegisterAPI;
end.
