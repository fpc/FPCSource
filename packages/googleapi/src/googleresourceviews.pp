unit googleresourceviews;
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
//Generated on: 16-5-15 08:53:07
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TLabel = Class;
  TListResourceResponseItem = Class;
  TOperation = Class;
  TOperationList = Class;
  TResourceView = Class;
  TServiceEndpoint = Class;
  TZoneViewsAddResourcesRequest = Class;
  TZoneViewsGetServiceResponse = Class;
  TZoneViewsList = Class;
  TZoneViewsListResourcesResponse = Class;
  TZoneViewsRemoveResourcesRequest = Class;
  TZoneViewsSetServiceRequest = Class;
  TLabelArray = Array of TLabel;
  TListResourceResponseItemArray = Array of TListResourceResponseItem;
  TOperationArray = Array of TOperation;
  TOperationListArray = Array of TOperationList;
  TResourceViewArray = Array of TResourceView;
  TServiceEndpointArray = Array of TServiceEndpoint;
  TZoneViewsAddResourcesRequestArray = Array of TZoneViewsAddResourcesRequest;
  TZoneViewsGetServiceResponseArray = Array of TZoneViewsGetServiceResponse;
  TZoneViewsListArray = Array of TZoneViewsList;
  TZoneViewsListResourcesResponseArray = Array of TZoneViewsListResourcesResponse;
  TZoneViewsRemoveResourcesRequestArray = Array of TZoneViewsRemoveResourcesRequest;
  TZoneViewsSetServiceRequestArray = Array of TZoneViewsSetServiceRequest;
  //Anonymous types, using auto-generated names
  TListResourceResponseItemTypeendpoints = Class;
  TOperationTypeerrorTypeerrorsItem = Class;
  TOperationTypeerror = Class;
  TOperationTypewarningsItemTypedataItem = Class;
  TOperationTypewarningsItem = Class;
  TOperationTypeerrorTypeerrorsArray = Array of TOperationTypeerrorTypeerrorsItem;
  TOperationTypewarningsItemTypedataArray = Array of TOperationTypewarningsItemTypedataItem;
  TOperationTypewarningsArray = Array of TOperationTypewarningsItem;
  TOperationListTypeitemsArray = Array of TOperation;
  TResourceViewTypeendpointsArray = Array of TServiceEndpoint;
  TResourceViewTypelabelsArray = Array of TLabel;
  TZoneViewsGetServiceResponseTypeendpointsArray = Array of TServiceEndpoint;
  TZoneViewsListTypeitemsArray = Array of TResourceView;
  TZoneViewsListResourcesResponseTypeitemsArray = Array of TListResourceResponseItem;
  TZoneViewsSetServiceRequestTypeendpointsArray = Array of TServiceEndpoint;
  
  { --------------------------------------------------------------------
    TLabel
    --------------------------------------------------------------------}
  
  TLabel = Class(TGoogleBaseObject)
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
  TLabelClass = Class of TLabel;
  
  { --------------------------------------------------------------------
    TListResourceResponseItemTypeendpoints
    --------------------------------------------------------------------}
  
  TListResourceResponseItemTypeendpoints = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TListResourceResponseItemTypeendpointsClass = Class of TListResourceResponseItemTypeendpoints;
  
  { --------------------------------------------------------------------
    TListResourceResponseItem
    --------------------------------------------------------------------}
  
  TListResourceResponseItem = Class(TGoogleBaseObject)
  Private
    Fendpoints : TListResourceResponseItemTypeendpoints;
    Fresource : String;
  Protected
    //Property setters
    Procedure Setendpoints(AIndex : Integer; AValue : TListResourceResponseItemTypeendpoints); virtual;
    Procedure Setresource(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property endpoints : TListResourceResponseItemTypeendpoints Index 0 Read Fendpoints Write Setendpoints;
    Property resource : String Index 8 Read Fresource Write Setresource;
  end;
  TListResourceResponseItemClass = Class of TListResourceResponseItem;
  
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
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
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
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
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
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
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
    TOperationList
    --------------------------------------------------------------------}
  
  TOperationList = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fitems : TOperationListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FselfLink : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TOperationListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property items : TOperationListTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : String Index 32 Read FselfLink Write SetselfLink;
  end;
  TOperationListClass = Class of TOperationList;
  
  { --------------------------------------------------------------------
    TResourceView
    --------------------------------------------------------------------}
  
  TResourceView = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : String;
    Fdescription : String;
    Fendpoints : TResourceViewTypeendpointsArray;
    Ffingerprint : String;
    Fid : String;
    Fkind : String;
    Flabels : TResourceViewTypelabelsArray;
    Fname : String;
    Fnetwork : String;
    Fresources : TStringArray;
    FselfLink : String;
    Fsize : integer;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setendpoints(AIndex : Integer; AValue : TResourceViewTypeendpointsArray); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlabels(AIndex : Integer; AValue : TResourceViewTypelabelsArray); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setnetwork(AIndex : Integer; AValue : String); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setsize(AIndex : Integer; AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property creationTimestamp : String Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property endpoints : TResourceViewTypeendpointsArray Index 16 Read Fendpoints Write Setendpoints;
    Property fingerprint : String Index 24 Read Ffingerprint Write Setfingerprint;
    Property id : String Index 32 Read Fid Write Setid;
    Property kind : String Index 40 Read Fkind Write Setkind;
    Property labels : TResourceViewTypelabelsArray Index 48 Read Flabels Write Setlabels;
    Property name : String Index 56 Read Fname Write Setname;
    Property network : String Index 64 Read Fnetwork Write Setnetwork;
    Property resources : TStringArray Index 72 Read Fresources Write Setresources;
    Property selfLink : String Index 80 Read FselfLink Write SetselfLink;
    Property size : integer Index 88 Read Fsize Write Setsize;
  end;
  TResourceViewClass = Class of TResourceView;
  
  { --------------------------------------------------------------------
    TServiceEndpoint
    --------------------------------------------------------------------}
  
  TServiceEndpoint = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fport : integer;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setport(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property port : integer Index 8 Read Fport Write Setport;
  end;
  TServiceEndpointClass = Class of TServiceEndpoint;
  
  { --------------------------------------------------------------------
    TZoneViewsAddResourcesRequest
    --------------------------------------------------------------------}
  
  TZoneViewsAddResourcesRequest = Class(TGoogleBaseObject)
  Private
    Fresources : TStringArray;
  Protected
    //Property setters
    Procedure Setresources(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property resources : TStringArray Index 0 Read Fresources Write Setresources;
  end;
  TZoneViewsAddResourcesRequestClass = Class of TZoneViewsAddResourcesRequest;
  
  { --------------------------------------------------------------------
    TZoneViewsGetServiceResponse
    --------------------------------------------------------------------}
  
  TZoneViewsGetServiceResponse = Class(TGoogleBaseObject)
  Private
    Fendpoints : TZoneViewsGetServiceResponseTypeendpointsArray;
    Ffingerprint : String;
  Protected
    //Property setters
    Procedure Setendpoints(AIndex : Integer; AValue : TZoneViewsGetServiceResponseTypeendpointsArray); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property endpoints : TZoneViewsGetServiceResponseTypeendpointsArray Index 0 Read Fendpoints Write Setendpoints;
    Property fingerprint : String Index 8 Read Ffingerprint Write Setfingerprint;
  end;
  TZoneViewsGetServiceResponseClass = Class of TZoneViewsGetServiceResponse;
  
  { --------------------------------------------------------------------
    TZoneViewsList
    --------------------------------------------------------------------}
  
  TZoneViewsList = Class(TGoogleBaseObject)
  Private
    Fitems : TZoneViewsListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FselfLink : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TZoneViewsListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TZoneViewsListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : String Index 24 Read FselfLink Write SetselfLink;
  end;
  TZoneViewsListClass = Class of TZoneViewsList;
  
  { --------------------------------------------------------------------
    TZoneViewsListResourcesResponse
    --------------------------------------------------------------------}
  
  TZoneViewsListResourcesResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TZoneViewsListResourcesResponseTypeitemsArray;
    Fnetwork : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TZoneViewsListResourcesResponseTypeitemsArray); virtual;
    Procedure Setnetwork(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TZoneViewsListResourcesResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property network : String Index 8 Read Fnetwork Write Setnetwork;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TZoneViewsListResourcesResponseClass = Class of TZoneViewsListResourcesResponse;
  
  { --------------------------------------------------------------------
    TZoneViewsRemoveResourcesRequest
    --------------------------------------------------------------------}
  
  TZoneViewsRemoveResourcesRequest = Class(TGoogleBaseObject)
  Private
    Fresources : TStringArray;
  Protected
    //Property setters
    Procedure Setresources(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property resources : TStringArray Index 0 Read Fresources Write Setresources;
  end;
  TZoneViewsRemoveResourcesRequestClass = Class of TZoneViewsRemoveResourcesRequest;
  
  { --------------------------------------------------------------------
    TZoneViewsSetServiceRequest
    --------------------------------------------------------------------}
  
  TZoneViewsSetServiceRequest = Class(TGoogleBaseObject)
  Private
    Fendpoints : TZoneViewsSetServiceRequestTypeendpointsArray;
    Ffingerprint : String;
    FresourceName : String;
  Protected
    //Property setters
    Procedure Setendpoints(AIndex : Integer; AValue : TZoneViewsSetServiceRequestTypeendpointsArray); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : String); virtual;
    Procedure SetresourceName(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property endpoints : TZoneViewsSetServiceRequestTypeendpointsArray Index 0 Read Fendpoints Write Setendpoints;
    Property fingerprint : String Index 8 Read Ffingerprint Write Setfingerprint;
    Property resourceName : String Index 16 Read FresourceName Write SetresourceName;
  end;
  TZoneViewsSetServiceRequestClass = Class of TZoneViewsSetServiceRequest;
  
  { --------------------------------------------------------------------
    TZoneOperationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TZoneOperationsResource, method List
  
  TZoneOperationsListOptions = Record
    filter : String;
    maxResults : integer;
    pageToken : String;
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
    TZoneViewsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TZoneViewsResource, method GetService
  
  TZoneViewsGetServiceOptions = Record
    _resourceName : String;
  end;
  
  
  //Optional query Options for TZoneViewsResource, method List
  
  TZoneViewsListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  
  //Optional query Options for TZoneViewsResource, method ListResources
  
  TZoneViewsListResourcesOptions = Record
    format : String;
    listState : String;
    maxResults : integer;
    pageToken : String;
    serviceName : String;
  end;
  
  TZoneViewsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function AddResources(project: string; resourceView: string; zone: string; aZoneViewsAddResourcesRequest : TZoneViewsAddResourcesRequest) : TOperation;
    Function Delete(project: string; resourceView: string; zone: string) : TOperation;
    Function Get(project: string; resourceView: string; zone: string) : TResourceView;
    Function GetService(project: string; resourceView: string; zone: string; AQuery : string  = '') : TZoneViewsGetServiceResponse;
    Function GetService(project: string; resourceView: string; zone: string; AQuery : TZoneViewsgetServiceOptions) : TZoneViewsGetServiceResponse;
    Function Insert(project: string; zone: string; aResourceView : TResourceView) : TOperation;
    Function List(project: string; zone: string; AQuery : string  = '') : TZoneViewsList;
    Function List(project: string; zone: string; AQuery : TZoneViewslistOptions) : TZoneViewsList;
    Function ListResources(project: string; resourceView: string; zone: string; AQuery : string  = '') : TZoneViewsListResourcesResponse;
    Function ListResources(project: string; resourceView: string; zone: string; AQuery : TZoneViewslistResourcesOptions) : TZoneViewsListResourcesResponse;
    Function RemoveResources(project: string; resourceView: string; zone: string; aZoneViewsRemoveResourcesRequest : TZoneViewsRemoveResourcesRequest) : TOperation;
    Function SetService(project: string; resourceView: string; zone: string; aZoneViewsSetServiceRequest : TZoneViewsSetServiceRequest) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TResourceviewsAPI
    --------------------------------------------------------------------}
  
  TResourceviewsAPI = Class(TGoogleAPI)
  Private
    FZoneOperationsInstance : TZoneOperationsResource;
    FZoneViewsInstance : TZoneViewsResource;
    Function GetZoneOperationsInstance : TZoneOperationsResource;virtual;
    Function GetZoneViewsInstance : TZoneViewsResource;virtual;
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
    Function CreateZoneOperationsResource(AOwner : TComponent) : TZoneOperationsResource;virtual;overload;
    Function CreateZoneOperationsResource : TZoneOperationsResource;virtual;overload;
    Function CreateZoneViewsResource(AOwner : TComponent) : TZoneViewsResource;virtual;overload;
    Function CreateZoneViewsResource : TZoneViewsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ZoneOperationsResource : TZoneOperationsResource Read GetZoneOperationsInstance;
    Property ZoneViewsResource : TZoneViewsResource Read GetZoneViewsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TLabel
  --------------------------------------------------------------------}


Procedure TLabel.Setkey(AIndex : Integer; AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabel.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListResourceResponseItemTypeendpoints
  --------------------------------------------------------------------}


Class Function TListResourceResponseItemTypeendpoints.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TListResourceResponseItem
  --------------------------------------------------------------------}


Procedure TListResourceResponseItem.Setendpoints(AIndex : Integer; AValue : TListResourceResponseItemTypeendpoints); 

begin
  If (Fendpoints=AValue) then exit;
  Fendpoints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListResourceResponseItem.Setresource(AIndex : Integer; AValue : String); 

begin
  If (Fresource=AValue) then exit;
  Fresource:=AValue;
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


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TOperationTypeerror.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errors' : SetLength(Ferrors,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TOperationTypewarningsItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'data' : SetLength(Fdata,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TOperation.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'warnings' : SetLength(Fwarnings,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TOperationList
  --------------------------------------------------------------------}


Procedure TOperationList.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.Setitems(AIndex : Integer; AValue : TOperationListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TOperationList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TResourceView
  --------------------------------------------------------------------}


Procedure TResourceView.SetcreationTimestamp(AIndex : Integer; AValue : String); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceView.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceView.Setendpoints(AIndex : Integer; AValue : TResourceViewTypeendpointsArray); 

begin
  If (Fendpoints=AValue) then exit;
  Fendpoints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceView.Setfingerprint(AIndex : Integer; AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceView.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceView.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceView.Setlabels(AIndex : Integer; AValue : TResourceViewTypelabelsArray); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceView.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceView.Setnetwork(AIndex : Integer; AValue : String); 

begin
  If (Fnetwork=AValue) then exit;
  Fnetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceView.Setresources(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceView.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceView.Setsize(AIndex : Integer; AValue : integer); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TResourceView.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'endpoints' : SetLength(Fendpoints,ALength);
  'labels' : SetLength(Flabels,ALength);
  'resources' : SetLength(Fresources,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TServiceEndpoint
  --------------------------------------------------------------------}


Procedure TServiceEndpoint.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TServiceEndpoint.Setport(AIndex : Integer; AValue : integer); 

begin
  If (Fport=AValue) then exit;
  Fport:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TZoneViewsAddResourcesRequest
  --------------------------------------------------------------------}


Procedure TZoneViewsAddResourcesRequest.Setresources(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TZoneViewsAddResourcesRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'resources' : SetLength(Fresources,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TZoneViewsGetServiceResponse
  --------------------------------------------------------------------}


Procedure TZoneViewsGetServiceResponse.Setendpoints(AIndex : Integer; AValue : TZoneViewsGetServiceResponseTypeendpointsArray); 

begin
  If (Fendpoints=AValue) then exit;
  Fendpoints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneViewsGetServiceResponse.Setfingerprint(AIndex : Integer; AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TZoneViewsGetServiceResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'endpoints' : SetLength(Fendpoints,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TZoneViewsList
  --------------------------------------------------------------------}


Procedure TZoneViewsList.Setitems(AIndex : Integer; AValue : TZoneViewsListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneViewsList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneViewsList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneViewsList.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TZoneViewsList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TZoneViewsListResourcesResponse
  --------------------------------------------------------------------}


Procedure TZoneViewsListResourcesResponse.Setitems(AIndex : Integer; AValue : TZoneViewsListResourcesResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneViewsListResourcesResponse.Setnetwork(AIndex : Integer; AValue : String); 

begin
  If (Fnetwork=AValue) then exit;
  Fnetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneViewsListResourcesResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TZoneViewsListResourcesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TZoneViewsRemoveResourcesRequest
  --------------------------------------------------------------------}


Procedure TZoneViewsRemoveResourcesRequest.Setresources(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TZoneViewsRemoveResourcesRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'resources' : SetLength(Fresources,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TZoneViewsSetServiceRequest
  --------------------------------------------------------------------}


Procedure TZoneViewsSetServiceRequest.Setendpoints(AIndex : Integer; AValue : TZoneViewsSetServiceRequestTypeendpointsArray); 

begin
  If (Fendpoints=AValue) then exit;
  Fendpoints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneViewsSetServiceRequest.Setfingerprint(AIndex : Integer; AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneViewsSetServiceRequest.SetresourceName(AIndex : Integer; AValue : String); 

begin
  If (FresourceName=AValue) then exit;
  FresourceName:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TZoneViewsSetServiceRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'endpoints' : SetLength(Fendpoints,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TZoneOperationsResource
  --------------------------------------------------------------------}


Class Function TZoneOperationsResource.ResourceName : String;

begin
  Result:='zoneOperations';
end;

Class Function TZoneOperationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TresourceviewsAPI;
end;

Function TZoneOperationsResource.Get(operation: string; project: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/operations/{operation}';
  _Methodid   = 'resourceviews.zoneOperations.get';

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
  _Methodid   = 'resourceviews.zoneOperations.list';

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
  TZoneViewsResource
  --------------------------------------------------------------------}


Class Function TZoneViewsResource.ResourceName : String;

begin
  Result:='zoneViews';
end;

Class Function TZoneViewsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TresourceviewsAPI;
end;

Function TZoneViewsResource.AddResources(project: string; resourceView: string; zone: string; aZoneViewsAddResourcesRequest : TZoneViewsAddResourcesRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/resourceViews/{resourceView}/addResources';
  _Methodid   = 'resourceviews.zoneViews.addResources';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'resourceView',resourceView,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aZoneViewsAddResourcesRequest,TOperation) as TOperation;
end;

Function TZoneViewsResource.Delete(project: string; resourceView: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/zones/{zone}/resourceViews/{resourceView}';
  _Methodid   = 'resourceviews.zoneViews.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'resourceView',resourceView,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TZoneViewsResource.Get(project: string; resourceView: string; zone: string) : TResourceView;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/resourceViews/{resourceView}';
  _Methodid   = 'resourceviews.zoneViews.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'resourceView',resourceView,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TResourceView) as TResourceView;
end;

Function TZoneViewsResource.GetService(project: string; resourceView: string; zone: string; AQuery : string = '') : TZoneViewsGetServiceResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/resourceViews/{resourceView}/getService';
  _Methodid   = 'resourceviews.zoneViews.getService';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'resourceView',resourceView,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TZoneViewsGetServiceResponse) as TZoneViewsGetServiceResponse;
end;


Function TZoneViewsResource.GetService(project: string; resourceView: string; zone: string; AQuery : TZoneViewsgetServiceOptions) : TZoneViewsGetServiceResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'resourceName',AQuery._resourceName);
  Result:=GetService(project,resourceView,zone,_Q);
end;

Function TZoneViewsResource.Insert(project: string; zone: string; aResourceView : TResourceView) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/resourceViews';
  _Methodid   = 'resourceviews.zoneViews.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aResourceView,TOperation) as TOperation;
end;

Function TZoneViewsResource.List(project: string; zone: string; AQuery : string = '') : TZoneViewsList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/resourceViews';
  _Methodid   = 'resourceviews.zoneViews.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TZoneViewsList) as TZoneViewsList;
end;


Function TZoneViewsResource.List(project: string; zone: string; AQuery : TZoneViewslistOptions) : TZoneViewsList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,zone,_Q);
end;

Function TZoneViewsResource.ListResources(project: string; resourceView: string; zone: string; AQuery : string = '') : TZoneViewsListResourcesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/resourceViews/{resourceView}/resources';
  _Methodid   = 'resourceviews.zoneViews.listResources';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'resourceView',resourceView,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TZoneViewsListResourcesResponse) as TZoneViewsListResourcesResponse;
end;


Function TZoneViewsResource.ListResources(project: string; resourceView: string; zone: string; AQuery : TZoneViewslistResourcesOptions) : TZoneViewsListResourcesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'format',AQuery.format);
  AddToQuery(_Q,'listState',AQuery.listState);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'serviceName',AQuery.serviceName);
  Result:=ListResources(project,resourceView,zone,_Q);
end;

Function TZoneViewsResource.RemoveResources(project: string; resourceView: string; zone: string; aZoneViewsRemoveResourcesRequest : TZoneViewsRemoveResourcesRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/resourceViews/{resourceView}/removeResources';
  _Methodid   = 'resourceviews.zoneViews.removeResources';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'resourceView',resourceView,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aZoneViewsRemoveResourcesRequest,TOperation) as TOperation;
end;

Function TZoneViewsResource.SetService(project: string; resourceView: string; zone: string; aZoneViewsSetServiceRequest : TZoneViewsSetServiceRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/resourceViews/{resourceView}/setService';
  _Methodid   = 'resourceviews.zoneViews.setService';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'resourceView',resourceView,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aZoneViewsSetServiceRequest,TOperation) as TOperation;
end;



{ --------------------------------------------------------------------
  TResourceviewsAPI
  --------------------------------------------------------------------}

Class Function TResourceviewsAPI.APIName : String;

begin
  Result:='resourceviews';
end;

Class Function TResourceviewsAPI.APIVersion : String;

begin
  Result:='v1beta2';
end;

Class Function TResourceviewsAPI.APIRevision : String;

begin
  Result:='20150302';
end;

Class Function TResourceviewsAPI.APIID : String;

begin
  Result:='resourceviews:v1beta2';
end;

Class Function TResourceviewsAPI.APITitle : String;

begin
  Result:='Google Compute Engine Instance Groups API';
end;

Class Function TResourceviewsAPI.APIDescription : String;

begin
  Result:='The Resource View API allows users to create and manage logical sets of Google Compute Engine instances.';
end;

Class Function TResourceviewsAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TResourceviewsAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TResourceviewsAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TResourceviewsAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TResourceviewsAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/compute/';
end;

Class Function TResourceviewsAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com:443/';
end;

Class Function TResourceviewsAPI.APIbasePath : string;

begin
  Result:='/resourceviews/v1beta2/projects/';
end;

Class Function TResourceviewsAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/resourceviews/v1beta2/projects/';
end;

Class Function TResourceviewsAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TResourceviewsAPI.APIservicePath : string;

begin
  Result:='resourceviews/v1beta2/projects/';
end;

Class Function TResourceviewsAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TResourceviewsAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,5);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/compute';
  Result[1].Description:='View and manage your Google Compute Engine resources';
  Result[2].Name:='https://www.googleapis.com/auth/compute.readonly';
  Result[2].Description:='View your Google Compute Engine resources';
  Result[3].Name:='https://www.googleapis.com/auth/ndev.cloudman';
  Result[3].Description:='View and manage your Google Cloud Platform management resources and deployment status information';
  Result[4].Name:='https://www.googleapis.com/auth/ndev.cloudman.readonly';
  Result[4].Description:='View your Google Cloud Platform management resources and deployment status information';
  
end;

Class Function TResourceviewsAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TResourceviewsAPI.RegisterAPIResources;

begin
  TLabel.RegisterObject;
  TListResourceResponseItemTypeendpoints.RegisterObject;
  TListResourceResponseItem.RegisterObject;
  TOperationTypeerrorTypeerrorsItem.RegisterObject;
  TOperationTypeerror.RegisterObject;
  TOperationTypewarningsItemTypedataItem.RegisterObject;
  TOperationTypewarningsItem.RegisterObject;
  TOperation.RegisterObject;
  TOperationList.RegisterObject;
  TResourceView.RegisterObject;
  TServiceEndpoint.RegisterObject;
  TZoneViewsAddResourcesRequest.RegisterObject;
  TZoneViewsGetServiceResponse.RegisterObject;
  TZoneViewsList.RegisterObject;
  TZoneViewsListResourcesResponse.RegisterObject;
  TZoneViewsRemoveResourcesRequest.RegisterObject;
  TZoneViewsSetServiceRequest.RegisterObject;
end;


Function TResourceviewsAPI.GetZoneOperationsInstance : TZoneOperationsResource;

begin
  if (FZoneOperationsInstance=Nil) then
    FZoneOperationsInstance:=CreateZoneOperationsResource;
  Result:=FZoneOperationsInstance;
end;

Function TResourceviewsAPI.CreateZoneOperationsResource : TZoneOperationsResource;

begin
  Result:=CreateZoneOperationsResource(Self);
end;


Function TResourceviewsAPI.CreateZoneOperationsResource(AOwner : TComponent) : TZoneOperationsResource;

begin
  Result:=TZoneOperationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TResourceviewsAPI.GetZoneViewsInstance : TZoneViewsResource;

begin
  if (FZoneViewsInstance=Nil) then
    FZoneViewsInstance:=CreateZoneViewsResource;
  Result:=FZoneViewsInstance;
end;

Function TResourceviewsAPI.CreateZoneViewsResource : TZoneViewsResource;

begin
  Result:=CreateZoneViewsResource(Self);
end;


Function TResourceviewsAPI.CreateZoneViewsResource(AOwner : TComponent) : TZoneViewsResource;

begin
  Result:=TZoneViewsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TResourceviewsAPI.RegisterAPI;
end.
