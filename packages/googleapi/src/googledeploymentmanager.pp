unit googledeploymentmanager;
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
//Generated on: 9-5-15 13:22:52
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TDeployment = class;
  TDeploymentUpdate = class;
  TDeploymentsListResponse = class;
  TImportFile = class;
  TManifest = class;
  TManifestsListResponse = class;
  TOperation = class;
  TOperationsListResponse = class;
  TResource = class;
  TResourceUpdate = class;
  TResourcesListResponse = class;
  TTargetConfiguration = class;
  TType = class;
  TTypesListResponse = class;
  TDeploymentArray = Array of TDeployment;
  TDeploymentUpdateArray = Array of TDeploymentUpdate;
  TDeploymentsListResponseArray = Array of TDeploymentsListResponse;
  TImportFileArray = Array of TImportFile;
  TManifestArray = Array of TManifest;
  TManifestsListResponseArray = Array of TManifestsListResponse;
  TOperationArray = Array of TOperation;
  TOperationsListResponseArray = Array of TOperationsListResponse;
  TResourceArray = Array of TResource;
  TResourceUpdateArray = Array of TResourceUpdate;
  TResourcesListResponseArray = Array of TResourcesListResponse;
  TTargetConfigurationArray = Array of TTargetConfiguration;
  TTypeArray = Array of TType;
  TTypesListResponseArray = Array of TTypesListResponse;
  //Anonymous types, using auto-generated names
  TOperationTypeerrorTypeerrorsItem = class;
  TOperationTypeerror = class;
  TOperationTypewarningsItemTypedataItem = class;
  TOperationTypewarningsItem = class;
  TDeploymentsListResponseTypedeploymentsArray = Array of TDeployment;
  TManifestTypeimportsArray = Array of TImportFile;
  TManifestsListResponseTypemanifestsArray = Array of TManifest;
  TOperationTypeerrorTypeerrorsArray = Array of TOperationTypeerrorTypeerrorsItem;
  TOperationTypewarningsItemTypedataArray = Array of TOperationTypewarningsItemTypedataItem;
  TOperationTypewarningsArray = Array of TOperationTypewarningsItem;
  TOperationsListResponseTypeoperationsArray = Array of TOperation;
  TResourcesListResponseTyperesourcesArray = Array of TResource;
  TTargetConfigurationTypeimportsArray = Array of TImportFile;
  TTypesListResponseTypetypesArray = Array of TType;
  
  { --------------------------------------------------------------------
    TDeployment
    --------------------------------------------------------------------}
  
  TDeployment = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : String;
    Fdescription : String;
    Ffingerprint : String;
    Fid : String;
    Fintent : String;
    Fmanifest : String;
    Fname : String;
    Fstate : String;
    Ftarget : TTargetConfiguration;
    Fupdate : TDeploymentUpdate;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setintent(AIndex : Integer; AValue : String); virtual;
    Procedure Setmanifest(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setstate(AIndex : Integer; AValue : String); virtual;
    Procedure Settarget(AIndex : Integer; AValue : TTargetConfiguration); virtual;
    Procedure Setupdate(AIndex : Integer; AValue : TDeploymentUpdate); virtual;
  Public
  Published
    Property creationTimestamp : String Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property fingerprint : String Index 16 Read Ffingerprint Write Setfingerprint;
    Property id : String Index 24 Read Fid Write Setid;
    Property intent : String Index 32 Read Fintent Write Setintent;
    Property manifest : String Index 40 Read Fmanifest Write Setmanifest;
    Property name : String Index 48 Read Fname Write Setname;
    Property state : String Index 56 Read Fstate Write Setstate;
    Property target : TTargetConfiguration Index 64 Read Ftarget Write Settarget;
    Property update : TDeploymentUpdate Index 72 Read Fupdate Write Setupdate;
  end;
  TDeploymentClass = Class of TDeployment;
  
  { --------------------------------------------------------------------
    TDeploymentUpdate
    --------------------------------------------------------------------}
  
  TDeploymentUpdate = Class(TGoogleBaseObject)
  Private
    Ferrors : TStringArray;
    Fmanifest : String;
  Protected
    //Property setters
    Procedure Seterrors(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setmanifest(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property errors : TStringArray Index 0 Read Ferrors Write Seterrors;
    Property manifest : String Index 8 Read Fmanifest Write Setmanifest;
  end;
  TDeploymentUpdateClass = Class of TDeploymentUpdate;
  
  { --------------------------------------------------------------------
    TDeploymentsListResponse
    --------------------------------------------------------------------}
  
  TDeploymentsListResponse = Class(TGoogleBaseObject)
  Private
    Fdeployments : TDeploymentsListResponseTypedeploymentsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setdeployments(AIndex : Integer; AValue : TDeploymentsListResponseTypedeploymentsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property deployments : TDeploymentsListResponseTypedeploymentsArray Index 0 Read Fdeployments Write Setdeployments;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TDeploymentsListResponseClass = Class of TDeploymentsListResponse;
  
  { --------------------------------------------------------------------
    TImportFile
    --------------------------------------------------------------------}
  
  TImportFile = Class(TGoogleBaseObject)
  Private
    Fcontent : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setcontent(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property content : String Index 0 Read Fcontent Write Setcontent;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TImportFileClass = Class of TImportFile;
  
  { --------------------------------------------------------------------
    TManifest
    --------------------------------------------------------------------}
  
  TManifest = Class(TGoogleBaseObject)
  Private
    Fconfig : String;
    FcreationTimestamp : String;
    FevaluatedConfig : String;
    Fid : String;
    Fimports : TManifestTypeimportsArray;
    Flayout : String;
    Fname : String;
    FselfLink : String;
  Protected
    //Property setters
    Procedure Setconfig(AIndex : Integer; AValue : String); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure SetevaluatedConfig(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setimports(AIndex : Integer; AValue : TManifestTypeimportsArray); virtual;
    Procedure Setlayout(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property config : String Index 0 Read Fconfig Write Setconfig;
    Property creationTimestamp : String Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property evaluatedConfig : String Index 16 Read FevaluatedConfig Write SetevaluatedConfig;
    Property id : String Index 24 Read Fid Write Setid;
    Property imports : TManifestTypeimportsArray Index 32 Read Fimports Write Setimports;
    Property layout : String Index 40 Read Flayout Write Setlayout;
    Property name : String Index 48 Read Fname Write Setname;
    Property selfLink : String Index 56 Read FselfLink Write SetselfLink;
  end;
  TManifestClass = Class of TManifest;
  
  { --------------------------------------------------------------------
    TManifestsListResponse
    --------------------------------------------------------------------}
  
  TManifestsListResponse = Class(TGoogleBaseObject)
  Private
    Fmanifests : TManifestsListResponseTypemanifestsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setmanifests(AIndex : Integer; AValue : TManifestsListResponseTypemanifestsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property manifests : TManifestsListResponseTypemanifestsArray Index 0 Read Fmanifests Write Setmanifests;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TManifestsListResponseClass = Class of TManifestsListResponse;
  
  { --------------------------------------------------------------------
    TOperationTypeerrorTypeerrorsItem
    --------------------------------------------------------------------}
  
  TOperationTypeerrorTypeerrorsItem = Class(TGoogleBaseObject)
  Private
    Fcode : String;
    Flocation : String;
    Fmessage : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : String); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property code : String Index 0 Read Fcode Write Setcode;
    Property location : String Index 8 Read Flocation Write Setlocation;
    Property message : String Index 16 Read Fmessage Write Setmessage;
  end;
  TOperationTypeerrorTypeerrorsItemClass = Class of TOperationTypeerrorTypeerrorsItem;
  
  { --------------------------------------------------------------------
    TOperationTypeerror
    --------------------------------------------------------------------}
  
  TOperationTypeerror = Class(TGoogleBaseObject)
  Private
    Ferrors : TOperationTypeerrorTypeerrorsArray;
  Protected
    //Property setters
    Procedure Seterrors(AIndex : Integer; AValue : TOperationTypeerrorTypeerrorsArray); virtual;
  Public
  Published
    Property errors : TOperationTypeerrorTypeerrorsArray Index 0 Read Ferrors Write Seterrors;
  end;
  TOperationTypeerrorClass = Class of TOperationTypeerror;
  
  { --------------------------------------------------------------------
    TOperationTypewarningsItemTypedataItem
    --------------------------------------------------------------------}
  
  TOperationTypewarningsItemTypedataItem = Class(TGoogleBaseObject)
  Private
    Fkey : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property key : String Index 0 Read Fkey Write Setkey;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TOperationTypewarningsItemTypedataItemClass = Class of TOperationTypewarningsItemTypedataItem;
  
  { --------------------------------------------------------------------
    TOperationTypewarningsItem
    --------------------------------------------------------------------}
  
  TOperationTypewarningsItem = Class(TGoogleBaseObject)
  Private
    Fcode : String;
    Fdata : TOperationTypewarningsItemTypedataArray;
    Fmessage : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : String); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TOperationTypewarningsItemTypedataArray); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property code : String Index 0 Read Fcode Write Setcode;
    Property data : TOperationTypewarningsItemTypedataArray Index 8 Read Fdata Write Setdata;
    Property message : String Index 16 Read Fmessage Write Setmessage;
  end;
  TOperationTypewarningsItemClass = Class of TOperationTypewarningsItem;
  
  { --------------------------------------------------------------------
    TOperation
    --------------------------------------------------------------------}
  
  TOperation = Class(TGoogleBaseObject)
  Private
    FclientOperationId : String;
    FcreationTimestamp : String;
    FendTime : String;
    Ferror : TOperationTypeerror;
    FhttpErrorMessage : String;
    FhttpErrorStatusCode : integer;
    Fid : String;
    FinsertTime : String;
    Fkind : String;
    Fname : String;
    FoperationType : String;
    Fprogress : integer;
    Fregion : String;
    FselfLink : String;
    FstartTime : String;
    Fstatus : String;
    FstatusMessage : String;
    FtargetId : String;
    FtargetLink : String;
    Fuser : String;
    Fwarnings : TOperationTypewarningsArray;
    Fzone : String;
  Protected
    //Property setters
    Procedure SetclientOperationId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure SetendTime(AIndex : Integer; AValue : String); virtual;
    Procedure Seterror(AIndex : Integer; AValue : TOperationTypeerror); virtual;
    Procedure SethttpErrorMessage(AIndex : Integer; AValue : String); virtual;
    Procedure SethttpErrorStatusCode(AIndex : Integer; AValue : integer); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetinsertTime(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetoperationType(AIndex : Integer; AValue : String); virtual;
    Procedure Setprogress(AIndex : Integer; AValue : integer); virtual;
    Procedure Setregion(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetstatusMessage(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetId(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setuser(AIndex : Integer; AValue : String); virtual;
    Procedure Setwarnings(AIndex : Integer; AValue : TOperationTypewarningsArray); virtual;
    Procedure Setzone(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property clientOperationId : String Index 0 Read FclientOperationId Write SetclientOperationId;
    Property creationTimestamp : String Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property endTime : String Index 16 Read FendTime Write SetendTime;
    Property error : TOperationTypeerror Index 24 Read Ferror Write Seterror;
    Property httpErrorMessage : String Index 32 Read FhttpErrorMessage Write SethttpErrorMessage;
    Property httpErrorStatusCode : integer Index 40 Read FhttpErrorStatusCode Write SethttpErrorStatusCode;
    Property id : String Index 48 Read Fid Write Setid;
    Property insertTime : String Index 56 Read FinsertTime Write SetinsertTime;
    Property kind : String Index 64 Read Fkind Write Setkind;
    Property name : String Index 72 Read Fname Write Setname;
    Property operationType : String Index 80 Read FoperationType Write SetoperationType;
    Property progress : integer Index 88 Read Fprogress Write Setprogress;
    Property region : String Index 96 Read Fregion Write Setregion;
    Property selfLink : String Index 104 Read FselfLink Write SetselfLink;
    Property startTime : String Index 112 Read FstartTime Write SetstartTime;
    Property status : String Index 120 Read Fstatus Write Setstatus;
    Property statusMessage : String Index 128 Read FstatusMessage Write SetstatusMessage;
    Property targetId : String Index 136 Read FtargetId Write SettargetId;
    Property targetLink : String Index 144 Read FtargetLink Write SettargetLink;
    Property user : String Index 152 Read Fuser Write Setuser;
    Property warnings : TOperationTypewarningsArray Index 160 Read Fwarnings Write Setwarnings;
    Property zone : String Index 168 Read Fzone Write Setzone;
  end;
  TOperationClass = Class of TOperation;
  
  { --------------------------------------------------------------------
    TOperationsListResponse
    --------------------------------------------------------------------}
  
  TOperationsListResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Foperations : TOperationsListResponseTypeoperationsArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setoperations(AIndex : Integer; AValue : TOperationsListResponseTypeoperationsArray); virtual;
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property operations : TOperationsListResponseTypeoperationsArray Index 8 Read Foperations Write Setoperations;
  end;
  TOperationsListResponseClass = Class of TOperationsListResponse;
  
  { --------------------------------------------------------------------
    TResource
    --------------------------------------------------------------------}
  
  TResource = Class(TGoogleBaseObject)
  Private
    FfinalProperties : String;
    Fid : String;
    Fmanifest : String;
    Fname : String;
    Fproperties : String;
    F_type : String;
    Fupdate : TResourceUpdate;
    Furl : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetfinalProperties(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setmanifest(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setproperties(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdate(AIndex : Integer; AValue : TResourceUpdate); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property finalProperties : String Index 0 Read FfinalProperties Write SetfinalProperties;
    Property id : String Index 8 Read Fid Write Setid;
    Property manifest : String Index 16 Read Fmanifest Write Setmanifest;
    Property name : String Index 24 Read Fname Write Setname;
    Property properties : String Index 32 Read Fproperties Write Setproperties;
    Property _type : String Index 40 Read F_type Write Set_type;
    Property update : TResourceUpdate Index 48 Read Fupdate Write Setupdate;
    Property url : String Index 56 Read Furl Write Seturl;
  end;
  TResourceClass = Class of TResource;
  
  { --------------------------------------------------------------------
    TResourceUpdate
    --------------------------------------------------------------------}
  
  TResourceUpdate = Class(TGoogleBaseObject)
  Private
    Ferrors : TStringArray;
    FfinalProperties : String;
    Fintent : String;
    Fmanifest : String;
    Fproperties : String;
    Fstate : String;
  Protected
    //Property setters
    Procedure Seterrors(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetfinalProperties(AIndex : Integer; AValue : String); virtual;
    Procedure Setintent(AIndex : Integer; AValue : String); virtual;
    Procedure Setmanifest(AIndex : Integer; AValue : String); virtual;
    Procedure Setproperties(AIndex : Integer; AValue : String); virtual;
    Procedure Setstate(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property errors : TStringArray Index 0 Read Ferrors Write Seterrors;
    Property finalProperties : String Index 8 Read FfinalProperties Write SetfinalProperties;
    Property intent : String Index 16 Read Fintent Write Setintent;
    Property manifest : String Index 24 Read Fmanifest Write Setmanifest;
    Property properties : String Index 32 Read Fproperties Write Setproperties;
    Property state : String Index 40 Read Fstate Write Setstate;
  end;
  TResourceUpdateClass = Class of TResourceUpdate;
  
  { --------------------------------------------------------------------
    TResourcesListResponse
    --------------------------------------------------------------------}
  
  TResourcesListResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Fresources : TResourcesListResponseTyperesourcesArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TResourcesListResponseTyperesourcesArray); virtual;
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property resources : TResourcesListResponseTyperesourcesArray Index 8 Read Fresources Write Setresources;
  end;
  TResourcesListResponseClass = Class of TResourcesListResponse;
  
  { --------------------------------------------------------------------
    TTargetConfiguration
    --------------------------------------------------------------------}
  
  TTargetConfiguration = Class(TGoogleBaseObject)
  Private
    Fconfig : String;
    Fimports : TTargetConfigurationTypeimportsArray;
  Protected
    //Property setters
    Procedure Setconfig(AIndex : Integer; AValue : String); virtual;
    Procedure Setimports(AIndex : Integer; AValue : TTargetConfigurationTypeimportsArray); virtual;
  Public
  Published
    Property config : String Index 0 Read Fconfig Write Setconfig;
    Property imports : TTargetConfigurationTypeimportsArray Index 8 Read Fimports Write Setimports;
  end;
  TTargetConfigurationClass = Class of TTargetConfiguration;
  
  { --------------------------------------------------------------------
    TType
    --------------------------------------------------------------------}
  
  TType = Class(TGoogleBaseObject)
  Private
    Fname : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
  end;
  TTypeClass = Class of TType;
  
  { --------------------------------------------------------------------
    TTypesListResponse
    --------------------------------------------------------------------}
  
  TTypesListResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Ftypes : TTypesListResponseTypetypesArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Settypes(AIndex : Integer; AValue : TTypesListResponseTypetypesArray); virtual;
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property types : TTypesListResponseTypetypesArray Index 8 Read Ftypes Write Settypes;
  end;
  TTypesListResponseClass = Class of TTypesListResponse;
  
  { --------------------------------------------------------------------
    TDeploymentsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDeploymentsResource, method List
  
  TDeploymentsListOptions = Record
    filter : String;
    maxResults : integer;
    pageToken : String;
  end;
  
  
  //Optional query Options for TDeploymentsResource, method Patch
  
  TDeploymentsPatchOptions = Record
    createPolicy : String;
    deletePolicy : String;
    updatePolicy : String;
  end;
  
  
  //Optional query Options for TDeploymentsResource, method Update
  
  TDeploymentsUpdateOptions = Record
    createPolicy : String;
    deletePolicy : String;
    updatePolicy : String;
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
    Function Patch(deployment: string; project: string; aDeployment : TDeployment; AQuery : string  = '') : TOperation;
    Function Patch(deployment: string; project: string; aDeployment : TDeployment; AQuery : TDeploymentspatchOptions) : TOperation;
    Function Update(deployment: string; project: string; aDeployment : TDeployment; AQuery : string  = '') : TOperation;
    Function Update(deployment: string; project: string; aDeployment : TDeployment; AQuery : TDeploymentsupdateOptions) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TManifestsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TManifestsResource, method List
  
  TManifestsListOptions = Record
    filter : String;
    maxResults : integer;
    pageToken : String;
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
    filter : String;
    maxResults : integer;
    pageToken : String;
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
    filter : String;
    maxResults : integer;
    pageToken : String;
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
    filter : String;
    maxResults : integer;
    pageToken : String;
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


Procedure TDeployment.SetcreationTimestamp(AIndex : Integer; AValue : String); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.Setfingerprint(AIndex : Integer; AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.Setintent(AIndex : Integer; AValue : String); 

begin
  If (Fintent=AValue) then exit;
  Fintent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.Setmanifest(AIndex : Integer; AValue : String); 

begin
  If (Fmanifest=AValue) then exit;
  Fmanifest:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.Setstate(AIndex : Integer; AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.Settarget(AIndex : Integer; AValue : TTargetConfiguration); 

begin
  If (Ftarget=AValue) then exit;
  Ftarget:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.Setupdate(AIndex : Integer; AValue : TDeploymentUpdate); 

begin
  If (Fupdate=AValue) then exit;
  Fupdate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDeploymentUpdate
  --------------------------------------------------------------------}


Procedure TDeploymentUpdate.Seterrors(AIndex : Integer; AValue : TStringArray); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeploymentUpdate.Setmanifest(AIndex : Integer; AValue : String); 

begin
  If (Fmanifest=AValue) then exit;
  Fmanifest:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDeploymentsListResponse
  --------------------------------------------------------------------}


Procedure TDeploymentsListResponse.Setdeployments(AIndex : Integer; AValue : TDeploymentsListResponseTypedeploymentsArray); 

begin
  If (Fdeployments=AValue) then exit;
  Fdeployments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeploymentsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImportFile
  --------------------------------------------------------------------}


Procedure TImportFile.Setcontent(AIndex : Integer; AValue : String); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImportFile.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TManifest
  --------------------------------------------------------------------}


Procedure TManifest.Setconfig(AIndex : Integer; AValue : String); 

begin
  If (Fconfig=AValue) then exit;
  Fconfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManifest.SetcreationTimestamp(AIndex : Integer; AValue : String); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManifest.SetevaluatedConfig(AIndex : Integer; AValue : String); 

begin
  If (FevaluatedConfig=AValue) then exit;
  FevaluatedConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManifest.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManifest.Setimports(AIndex : Integer; AValue : TManifestTypeimportsArray); 

begin
  If (Fimports=AValue) then exit;
  Fimports:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManifest.Setlayout(AIndex : Integer; AValue : String); 

begin
  If (Flayout=AValue) then exit;
  Flayout:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManifest.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManifest.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TManifestsListResponse
  --------------------------------------------------------------------}


Procedure TManifestsListResponse.Setmanifests(AIndex : Integer; AValue : TManifestsListResponseTypemanifestsArray); 

begin
  If (Fmanifests=AValue) then exit;
  Fmanifests:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManifestsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationTypeerrorTypeerrorsItem
  --------------------------------------------------------------------}


Procedure TOperationTypeerrorTypeerrorsItem.Setcode(AIndex : Integer; AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypeerrorTypeerrorsItem.Setlocation(AIndex : Integer; AValue : String); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypeerrorTypeerrorsItem.Setmessage(AIndex : Integer; AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationTypeerror
  --------------------------------------------------------------------}


Procedure TOperationTypeerror.Seterrors(AIndex : Integer; AValue : TOperationTypeerrorTypeerrorsArray); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationTypewarningsItemTypedataItem
  --------------------------------------------------------------------}


Procedure TOperationTypewarningsItemTypedataItem.Setkey(AIndex : Integer; AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypewarningsItemTypedataItem.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationTypewarningsItem
  --------------------------------------------------------------------}


Procedure TOperationTypewarningsItem.Setcode(AIndex : Integer; AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypewarningsItem.Setdata(AIndex : Integer; AValue : TOperationTypewarningsItemTypedataArray); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypewarningsItem.Setmessage(AIndex : Integer; AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperation
  --------------------------------------------------------------------}


Procedure TOperation.SetclientOperationId(AIndex : Integer; AValue : String); 

begin
  If (FclientOperationId=AValue) then exit;
  FclientOperationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetcreationTimestamp(AIndex : Integer; AValue : String); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetendTime(AIndex : Integer; AValue : String); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Seterror(AIndex : Integer; AValue : TOperationTypeerror); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SethttpErrorMessage(AIndex : Integer; AValue : String); 

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



Procedure TOperation.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetinsertTime(AIndex : Integer; AValue : String); 

begin
  If (FinsertTime=AValue) then exit;
  FinsertTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetoperationType(AIndex : Integer; AValue : String); 

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



Procedure TOperation.Setregion(AIndex : Integer; AValue : String); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetstartTime(AIndex : Integer; AValue : String); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetstatusMessage(AIndex : Integer; AValue : String); 

begin
  If (FstatusMessage=AValue) then exit;
  FstatusMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetId(AIndex : Integer; AValue : String); 

begin
  If (FtargetId=AValue) then exit;
  FtargetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetLink(AIndex : Integer; AValue : String); 

begin
  If (FtargetLink=AValue) then exit;
  FtargetLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setuser(AIndex : Integer; AValue : String); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setwarnings(AIndex : Integer; AValue : TOperationTypewarningsArray); 

begin
  If (Fwarnings=AValue) then exit;
  Fwarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setzone(AIndex : Integer; AValue : String); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationsListResponse
  --------------------------------------------------------------------}


Procedure TOperationsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationsListResponse.Setoperations(AIndex : Integer; AValue : TOperationsListResponseTypeoperationsArray); 

begin
  If (Foperations=AValue) then exit;
  Foperations:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResource
  --------------------------------------------------------------------}


Procedure TResource.SetfinalProperties(AIndex : Integer; AValue : String); 

begin
  If (FfinalProperties=AValue) then exit;
  FfinalProperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResource.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResource.Setmanifest(AIndex : Integer; AValue : String); 

begin
  If (Fmanifest=AValue) then exit;
  Fmanifest:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResource.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResource.Setproperties(AIndex : Integer; AValue : String); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResource.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResource.Setupdate(AIndex : Integer; AValue : TResourceUpdate); 

begin
  If (Fupdate=AValue) then exit;
  Fupdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResource.Seturl(AIndex : Integer; AValue : String); 

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
  TResourceUpdate
  --------------------------------------------------------------------}


Procedure TResourceUpdate.Seterrors(AIndex : Integer; AValue : TStringArray); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceUpdate.SetfinalProperties(AIndex : Integer; AValue : String); 

begin
  If (FfinalProperties=AValue) then exit;
  FfinalProperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceUpdate.Setintent(AIndex : Integer; AValue : String); 

begin
  If (Fintent=AValue) then exit;
  Fintent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceUpdate.Setmanifest(AIndex : Integer; AValue : String); 

begin
  If (Fmanifest=AValue) then exit;
  Fmanifest:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceUpdate.Setproperties(AIndex : Integer; AValue : String); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceUpdate.Setstate(AIndex : Integer; AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResourcesListResponse
  --------------------------------------------------------------------}


Procedure TResourcesListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourcesListResponse.Setresources(AIndex : Integer; AValue : TResourcesListResponseTyperesourcesArray); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTargetConfiguration
  --------------------------------------------------------------------}


Procedure TTargetConfiguration.Setconfig(AIndex : Integer; AValue : String); 

begin
  If (Fconfig=AValue) then exit;
  Fconfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTargetConfiguration.Setimports(AIndex : Integer; AValue : TTargetConfigurationTypeimportsArray); 

begin
  If (Fimports=AValue) then exit;
  Fimports:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TType
  --------------------------------------------------------------------}


Procedure TType.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTypesListResponse
  --------------------------------------------------------------------}


Procedure TTypesListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTypesListResponse.Settypes(AIndex : Integer; AValue : TTypesListResponseTypetypesArray); 

begin
  If (Ftypes=AValue) then exit;
  Ftypes:=AValue;
  MarkPropertyChanged(AIndex);
end;





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
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;

Function TDeploymentsResource.Patch(deployment: string; project: string; aDeployment : TDeployment; AQuery : string = '') : TOperation;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{project}/global/deployments/{deployment}';
  _Methodid   = 'deploymentmanager.deployments.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['deployment',deployment,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aDeployment,TOperation) as TOperation;
end;


Function TDeploymentsResource.Patch(deployment: string; project: string; aDeployment : TDeployment; AQuery : TDeploymentspatchOptions) : TOperation;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'createPolicy',AQuery.createPolicy);
  AddToQuery(_Q,'deletePolicy',AQuery.deletePolicy);
  AddToQuery(_Q,'updatePolicy',AQuery.updatePolicy);
  Result:=Patch(deployment,project,aDeployment,_Q);
end;

Function TDeploymentsResource.Update(deployment: string; project: string; aDeployment : TDeployment; AQuery : string = '') : TOperation;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{project}/global/deployments/{deployment}';
  _Methodid   = 'deploymentmanager.deployments.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['deployment',deployment,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aDeployment,TOperation) as TOperation;
end;


Function TDeploymentsResource.Update(deployment: string; project: string; aDeployment : TDeployment; AQuery : TDeploymentsupdateOptions) : TOperation;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'createPolicy',AQuery.createPolicy);
  AddToQuery(_Q,'deletePolicy',AQuery.deletePolicy);
  AddToQuery(_Q,'updatePolicy',AQuery.updatePolicy);
  Result:=Update(deployment,project,aDeployment,_Q);
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
  AddToQuery(_Q,'filter',AQuery.filter);
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
  AddToQuery(_Q,'filter',AQuery.filter);
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
  AddToQuery(_Q,'filter',AQuery.filter);
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
  AddToQuery(_Q,'filter',AQuery.filter);
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
  Result:='v2beta2';
end;

Class Function TDeploymentmanagerAPI.APIRevision : String;

begin
  Result:='20150427';
end;

Class Function TDeploymentmanagerAPI.APIID : String;

begin
  Result:='deploymentmanager:v2beta2';
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
  Result:='/deploymentmanager/v2beta2/projects/';
end;

Class Function TDeploymentmanagerAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/deploymentmanager/v2beta2/projects/';
end;

Class Function TDeploymentmanagerAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TDeploymentmanagerAPI.APIservicePath : string;

begin
  Result:='deploymentmanager/v2beta2/projects/';
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
  TDeploymentUpdate.RegisterObject;
  TDeploymentsListResponse.RegisterObject;
  TImportFile.RegisterObject;
  TManifest.RegisterObject;
  TManifestsListResponse.RegisterObject;
  TOperationTypeerrorTypeerrorsItem.RegisterObject;
  TOperationTypeerror.RegisterObject;
  TOperationTypewarningsItemTypedataItem.RegisterObject;
  TOperationTypewarningsItem.RegisterObject;
  TOperation.RegisterObject;
  TOperationsListResponse.RegisterObject;
  TResource.RegisterObject;
  TResourceUpdate.RegisterObject;
  TResourcesListResponse.RegisterObject;
  TTargetConfiguration.RegisterObject;
  TType.RegisterObject;
  TTypesListResponse.RegisterObject;
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
