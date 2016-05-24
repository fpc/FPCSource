unit googleresourceviews;
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
  TLabel = class;
  TLabelArray = Array of TLabel;
  TListResourceResponseItem = class;
  TListResourceResponseItemArray = Array of TListResourceResponseItem;
  TListResourceResponseItemendpoints = class;
  TListResourceResponseItemendpointsArray = Array of TListResourceResponseItemendpoints;
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
  TResourceView = class;
  TResourceViewArray = Array of TResourceView;
  TResourceViewendpoints = class;
  TResourceViewendpointsArray = Array of TResourceViewendpoints;
  TResourceViewlabels = class;
  TResourceViewlabelsArray = Array of TResourceViewlabels;
  TResourceViewresources = class;
  TResourceViewresourcesArray = Array of TResourceViewresources;
  TServiceEndpoint = class;
  TServiceEndpointArray = Array of TServiceEndpoint;
  TZoneViewsAddResourcesRequest = class;
  TZoneViewsAddResourcesRequestArray = Array of TZoneViewsAddResourcesRequest;
  TZoneViewsAddResourcesRequestresources = class;
  TZoneViewsAddResourcesRequestresourcesArray = Array of TZoneViewsAddResourcesRequestresources;
  TZoneViewsGetServiceResponse = class;
  TZoneViewsGetServiceResponseArray = Array of TZoneViewsGetServiceResponse;
  TZoneViewsGetServiceResponseendpoints = class;
  TZoneViewsGetServiceResponseendpointsArray = Array of TZoneViewsGetServiceResponseendpoints;
  TZoneViewsList = class;
  TZoneViewsListArray = Array of TZoneViewsList;
  TZoneViewsListitems = class;
  TZoneViewsListitemsArray = Array of TZoneViewsListitems;
  TZoneViewsListResourcesResponse = class;
  TZoneViewsListResourcesResponseArray = Array of TZoneViewsListResourcesResponse;
  TZoneViewsListResourcesResponseitems = class;
  TZoneViewsListResourcesResponseitemsArray = Array of TZoneViewsListResourcesResponseitems;
  TZoneViewsRemoveResourcesRequest = class;
  TZoneViewsRemoveResourcesRequestArray = Array of TZoneViewsRemoveResourcesRequest;
  TZoneViewsRemoveResourcesRequestresources = class;
  TZoneViewsRemoveResourcesRequestresourcesArray = Array of TZoneViewsRemoveResourcesRequestresources;
  TZoneViewsSetServiceRequest = class;
  TZoneViewsSetServiceRequestArray = Array of TZoneViewsSetServiceRequest;
  TZoneViewsSetServiceRequestendpoints = class;
  TZoneViewsSetServiceRequestendpointsArray = Array of TZoneViewsSetServiceRequestendpoints;
  
  { --------------------------------------------------------------------
    TLabel
    --------------------------------------------------------------------}
  
  TLabel = Class(TGoogleBaseObject)
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
  TLabelClass = Class of TLabel;
  
  { --------------------------------------------------------------------
    TListResourceResponseItem
    --------------------------------------------------------------------}
  
  TListResourceResponseItem = Class(TGoogleBaseObject)
  Private
    Fendpoints : TListResourceResponseItemendpoints;
    Fresource : string;
  Protected
    //Property setters
    Procedure Setendpoints(AIndex : Integer; AValue : TListResourceResponseItemendpoints); virtual;
    Procedure Setresource(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property endpoints : TListResourceResponseItemendpoints Index 0 Read Fendpoints Write Setendpoints;
    Property resource : string Index 8 Read Fresource Write Setresource;
  end;
  TListResourceResponseItemClass = Class of TListResourceResponseItem;
  
  { --------------------------------------------------------------------
    TListResourceResponseItemendpoints
    --------------------------------------------------------------------}
  
  TListResourceResponseItemendpoints = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TListResourceResponseItemendpointsClass = Class of TListResourceResponseItemendpoints;
  
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
    TResourceView
    --------------------------------------------------------------------}
  
  TResourceView = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : string;
    Fdescription : string;
    Fendpoints : TResourceViewendpoints;
    Ffingerprint : string;
    Fid : string;
    Fkind : string;
    Flabels : TResourceViewlabels;
    Fname : string;
    Fnetwork : string;
    Fresources : TResourceViewresources;
    FselfLink : string;
    Fsize : integer;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setendpoints(AIndex : Integer; AValue : TResourceViewendpoints); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlabels(AIndex : Integer; AValue : TResourceViewlabels); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setnetwork(AIndex : Integer; AValue : string); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TResourceViewresources); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setsize(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property creationTimestamp : string Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property endpoints : TResourceViewendpoints Index 16 Read Fendpoints Write Setendpoints;
    Property fingerprint : string Index 24 Read Ffingerprint Write Setfingerprint;
    Property id : string Index 32 Read Fid Write Setid;
    Property kind : string Index 40 Read Fkind Write Setkind;
    Property labels : TResourceViewlabels Index 48 Read Flabels Write Setlabels;
    Property name : string Index 56 Read Fname Write Setname;
    Property network : string Index 64 Read Fnetwork Write Setnetwork;
    Property resources : TResourceViewresources Index 72 Read Fresources Write Setresources;
    Property selfLink : string Index 80 Read FselfLink Write SetselfLink;
    Property size : integer Index 88 Read Fsize Write Setsize;
  end;
  TResourceViewClass = Class of TResourceView;
  
  { --------------------------------------------------------------------
    TResourceViewendpoints
    --------------------------------------------------------------------}
  
  TResourceViewendpoints = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TResourceViewendpointsClass = Class of TResourceViewendpoints;
  
  { --------------------------------------------------------------------
    TResourceViewlabels
    --------------------------------------------------------------------}
  
  TResourceViewlabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TResourceViewlabelsClass = Class of TResourceViewlabels;
  
  { --------------------------------------------------------------------
    TResourceViewresources
    --------------------------------------------------------------------}
  
  TResourceViewresources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TResourceViewresourcesClass = Class of TResourceViewresources;
  
  { --------------------------------------------------------------------
    TServiceEndpoint
    --------------------------------------------------------------------}
  
  TServiceEndpoint = Class(TGoogleBaseObject)
  Private
    Fname : string;
    Fport : integer;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setport(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
    Property port : integer Index 8 Read Fport Write Setport;
  end;
  TServiceEndpointClass = Class of TServiceEndpoint;
  
  { --------------------------------------------------------------------
    TZoneViewsAddResourcesRequest
    --------------------------------------------------------------------}
  
  TZoneViewsAddResourcesRequest = Class(TGoogleBaseObject)
  Private
    Fresources : TZoneViewsAddResourcesRequestresources;
  Protected
    //Property setters
    Procedure Setresources(AIndex : Integer; AValue : TZoneViewsAddResourcesRequestresources); virtual;
  Public
  Published
    Property resources : TZoneViewsAddResourcesRequestresources Index 0 Read Fresources Write Setresources;
  end;
  TZoneViewsAddResourcesRequestClass = Class of TZoneViewsAddResourcesRequest;
  
  { --------------------------------------------------------------------
    TZoneViewsAddResourcesRequestresources
    --------------------------------------------------------------------}
  
  TZoneViewsAddResourcesRequestresources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TZoneViewsAddResourcesRequestresourcesClass = Class of TZoneViewsAddResourcesRequestresources;
  
  { --------------------------------------------------------------------
    TZoneViewsGetServiceResponse
    --------------------------------------------------------------------}
  
  TZoneViewsGetServiceResponse = Class(TGoogleBaseObject)
  Private
    Fendpoints : TZoneViewsGetServiceResponseendpoints;
    Ffingerprint : string;
  Protected
    //Property setters
    Procedure Setendpoints(AIndex : Integer; AValue : TZoneViewsGetServiceResponseendpoints); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property endpoints : TZoneViewsGetServiceResponseendpoints Index 0 Read Fendpoints Write Setendpoints;
    Property fingerprint : string Index 8 Read Ffingerprint Write Setfingerprint;
  end;
  TZoneViewsGetServiceResponseClass = Class of TZoneViewsGetServiceResponse;
  
  { --------------------------------------------------------------------
    TZoneViewsGetServiceResponseendpoints
    --------------------------------------------------------------------}
  
  TZoneViewsGetServiceResponseendpoints = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TZoneViewsGetServiceResponseendpointsClass = Class of TZoneViewsGetServiceResponseendpoints;
  
  { --------------------------------------------------------------------
    TZoneViewsList
    --------------------------------------------------------------------}
  
  TZoneViewsList = Class(TGoogleBaseObject)
  Private
    Fitems : TZoneViewsListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TZoneViewsListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TZoneViewsListitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 24 Read FselfLink Write SetselfLink;
  end;
  TZoneViewsListClass = Class of TZoneViewsList;
  
  { --------------------------------------------------------------------
    TZoneViewsListitems
    --------------------------------------------------------------------}
  
  TZoneViewsListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TZoneViewsListitemsClass = Class of TZoneViewsListitems;
  
  { --------------------------------------------------------------------
    TZoneViewsListResourcesResponse
    --------------------------------------------------------------------}
  
  TZoneViewsListResourcesResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TZoneViewsListResourcesResponseitems;
    Fnetwork : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TZoneViewsListResourcesResponseitems); virtual;
    Procedure Setnetwork(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TZoneViewsListResourcesResponseitems Index 0 Read Fitems Write Setitems;
    Property network : string Index 8 Read Fnetwork Write Setnetwork;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TZoneViewsListResourcesResponseClass = Class of TZoneViewsListResourcesResponse;
  
  { --------------------------------------------------------------------
    TZoneViewsListResourcesResponseitems
    --------------------------------------------------------------------}
  
  TZoneViewsListResourcesResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TZoneViewsListResourcesResponseitemsClass = Class of TZoneViewsListResourcesResponseitems;
  
  { --------------------------------------------------------------------
    TZoneViewsRemoveResourcesRequest
    --------------------------------------------------------------------}
  
  TZoneViewsRemoveResourcesRequest = Class(TGoogleBaseObject)
  Private
    Fresources : TZoneViewsRemoveResourcesRequestresources;
  Protected
    //Property setters
    Procedure Setresources(AIndex : Integer; AValue : TZoneViewsRemoveResourcesRequestresources); virtual;
  Public
  Published
    Property resources : TZoneViewsRemoveResourcesRequestresources Index 0 Read Fresources Write Setresources;
  end;
  TZoneViewsRemoveResourcesRequestClass = Class of TZoneViewsRemoveResourcesRequest;
  
  { --------------------------------------------------------------------
    TZoneViewsRemoveResourcesRequestresources
    --------------------------------------------------------------------}
  
  TZoneViewsRemoveResourcesRequestresources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TZoneViewsRemoveResourcesRequestresourcesClass = Class of TZoneViewsRemoveResourcesRequestresources;
  
  { --------------------------------------------------------------------
    TZoneViewsSetServiceRequest
    --------------------------------------------------------------------}
  
  TZoneViewsSetServiceRequest = Class(TGoogleBaseObject)
  Private
    Fendpoints : TZoneViewsSetServiceRequestendpoints;
    Ffingerprint : string;
    FresourceName : string;
  Protected
    //Property setters
    Procedure Setendpoints(AIndex : Integer; AValue : TZoneViewsSetServiceRequestendpoints); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : string); virtual;
    Procedure SetresourceName(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property endpoints : TZoneViewsSetServiceRequestendpoints Index 0 Read Fendpoints Write Setendpoints;
    Property fingerprint : string Index 8 Read Ffingerprint Write Setfingerprint;
    Property resourceName : string Index 16 Read FresourceName Write SetresourceName;
  end;
  TZoneViewsSetServiceRequestClass = Class of TZoneViewsSetServiceRequest;
  
  { --------------------------------------------------------------------
    TZoneViewsSetServiceRequestendpoints
    --------------------------------------------------------------------}
  
  TZoneViewsSetServiceRequestendpoints = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TZoneViewsSetServiceRequestendpointsClass = Class of TZoneViewsSetServiceRequestendpoints;
  
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
    TZoneViewsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TZoneViewsResource, method GetService
  
  TZoneViewsGetServiceOptions = Record
    _resourceName : string;
  end;
  
  
  //Optional query Options for TZoneViewsResource, method List
  
  TZoneViewsListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TZoneViewsResource, method ListResources
  
  TZoneViewsListResourcesOptions = Record
    format : string;
    listState : string;
    maxResults : integer;
    pageToken : string;
    serviceName : string;
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


Procedure TLabel.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabel.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListResourceResponseItem
  --------------------------------------------------------------------}


Procedure TListResourceResponseItem.Setendpoints(AIndex : Integer; AValue : TListResourceResponseItemendpoints); 

begin
  If (Fendpoints=AValue) then exit;
  Fendpoints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListResourceResponseItem.Setresource(AIndex : Integer; AValue : string); 

begin
  If (Fresource=AValue) then exit;
  Fresource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListResourceResponseItemendpoints
  --------------------------------------------------------------------}


Class Function TListResourceResponseItemendpoints.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



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
  TResourceView
  --------------------------------------------------------------------}


Procedure TResourceView.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceView.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceView.Setendpoints(AIndex : Integer; AValue : TResourceViewendpoints); 

begin
  If (Fendpoints=AValue) then exit;
  Fendpoints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceView.Setfingerprint(AIndex : Integer; AValue : string); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceView.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceView.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceView.Setlabels(AIndex : Integer; AValue : TResourceViewlabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceView.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceView.Setnetwork(AIndex : Integer; AValue : string); 

begin
  If (Fnetwork=AValue) then exit;
  Fnetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceView.Setresources(AIndex : Integer; AValue : TResourceViewresources); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceView.SetselfLink(AIndex : Integer; AValue : string); 

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





{ --------------------------------------------------------------------
  TResourceViewendpoints
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TResourceViewlabels
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TResourceViewresources
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TServiceEndpoint
  --------------------------------------------------------------------}


Procedure TServiceEndpoint.Setname(AIndex : Integer; AValue : string); 

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


Procedure TZoneViewsAddResourcesRequest.Setresources(AIndex : Integer; AValue : TZoneViewsAddResourcesRequestresources); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TZoneViewsAddResourcesRequestresources
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TZoneViewsGetServiceResponse
  --------------------------------------------------------------------}


Procedure TZoneViewsGetServiceResponse.Setendpoints(AIndex : Integer; AValue : TZoneViewsGetServiceResponseendpoints); 

begin
  If (Fendpoints=AValue) then exit;
  Fendpoints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneViewsGetServiceResponse.Setfingerprint(AIndex : Integer; AValue : string); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TZoneViewsGetServiceResponseendpoints
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TZoneViewsList
  --------------------------------------------------------------------}


Procedure TZoneViewsList.Setitems(AIndex : Integer; AValue : TZoneViewsListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneViewsList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneViewsList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneViewsList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TZoneViewsListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TZoneViewsListResourcesResponse
  --------------------------------------------------------------------}


Procedure TZoneViewsListResourcesResponse.Setitems(AIndex : Integer; AValue : TZoneViewsListResourcesResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneViewsListResourcesResponse.Setnetwork(AIndex : Integer; AValue : string); 

begin
  If (Fnetwork=AValue) then exit;
  Fnetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneViewsListResourcesResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TZoneViewsListResourcesResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TZoneViewsRemoveResourcesRequest
  --------------------------------------------------------------------}


Procedure TZoneViewsRemoveResourcesRequest.Setresources(AIndex : Integer; AValue : TZoneViewsRemoveResourcesRequestresources); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TZoneViewsRemoveResourcesRequestresources
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TZoneViewsSetServiceRequest
  --------------------------------------------------------------------}


Procedure TZoneViewsSetServiceRequest.Setendpoints(AIndex : Integer; AValue : TZoneViewsSetServiceRequestendpoints); 

begin
  If (Fendpoints=AValue) then exit;
  Fendpoints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneViewsSetServiceRequest.Setfingerprint(AIndex : Integer; AValue : string); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneViewsSetServiceRequest.SetresourceName(AIndex : Integer; AValue : string); 

begin
  If (FresourceName=AValue) then exit;
  FresourceName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TZoneViewsSetServiceRequestendpoints
  --------------------------------------------------------------------}




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
  Result:='https://www.googleapis.com/';
end;

Class Function TResourceviewsAPI.APIbasePath : string;

begin
  Result:='/resourceviews/v1beta2/projects/';
end;

Class Function TResourceviewsAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/resourceviews/v1beta2/projects/';
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
  TListResourceResponseItem.RegisterObject;
  TListResourceResponseItemendpoints.RegisterObject;
  TOperation.RegisterObject;
  TOperationerror.RegisterObject;
  TOperationerrorerrors.RegisterObject;
  TOperationwarnings.RegisterObject;
  TOperationwarningsdata.RegisterObject;
  TOperationList.RegisterObject;
  TOperationListitems.RegisterObject;
  TResourceView.RegisterObject;
  TResourceViewendpoints.RegisterObject;
  TResourceViewlabels.RegisterObject;
  TResourceViewresources.RegisterObject;
  TServiceEndpoint.RegisterObject;
  TZoneViewsAddResourcesRequest.RegisterObject;
  TZoneViewsAddResourcesRequestresources.RegisterObject;
  TZoneViewsGetServiceResponse.RegisterObject;
  TZoneViewsGetServiceResponseendpoints.RegisterObject;
  TZoneViewsList.RegisterObject;
  TZoneViewsListitems.RegisterObject;
  TZoneViewsListResourcesResponse.RegisterObject;
  TZoneViewsListResourcesResponseitems.RegisterObject;
  TZoneViewsRemoveResourcesRequest.RegisterObject;
  TZoneViewsRemoveResourcesRequestresources.RegisterObject;
  TZoneViewsSetServiceRequest.RegisterObject;
  TZoneViewsSetServiceRequestendpoints.RegisterObject;
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
  Result.API:=Self;
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
  Result.API:=Self;
end;



initialization
  TResourceviewsAPI.RegisterAPI;
end.
