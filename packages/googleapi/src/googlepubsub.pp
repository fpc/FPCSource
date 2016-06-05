unit googlepubsub;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TSetIamPolicyRequest = Class;
  TPolicy = Class;
  TBinding = Class;
  TTestIamPermissionsRequest = Class;
  TTestIamPermissionsResponse = Class;
  TTopic = Class;
  TPublishRequest = Class;
  TPubsubMessage = Class;
  TPublishResponse = Class;
  TListTopicsResponse = Class;
  TListTopicSubscriptionsResponse = Class;
  TEmpty = Class;
  TSubscription = Class;
  TPushConfig = Class;
  TListSubscriptionsResponse = Class;
  TModifyAckDeadlineRequest = Class;
  TAcknowledgeRequest = Class;
  TPullRequest = Class;
  TPullResponse = Class;
  TReceivedMessage = Class;
  TModifyPushConfigRequest = Class;
  TSetIamPolicyRequestArray = Array of TSetIamPolicyRequest;
  TPolicyArray = Array of TPolicy;
  TBindingArray = Array of TBinding;
  TTestIamPermissionsRequestArray = Array of TTestIamPermissionsRequest;
  TTestIamPermissionsResponseArray = Array of TTestIamPermissionsResponse;
  TTopicArray = Array of TTopic;
  TPublishRequestArray = Array of TPublishRequest;
  TPubsubMessageArray = Array of TPubsubMessage;
  TPublishResponseArray = Array of TPublishResponse;
  TListTopicsResponseArray = Array of TListTopicsResponse;
  TListTopicSubscriptionsResponseArray = Array of TListTopicSubscriptionsResponse;
  TEmptyArray = Array of TEmpty;
  TSubscriptionArray = Array of TSubscription;
  TPushConfigArray = Array of TPushConfig;
  TListSubscriptionsResponseArray = Array of TListSubscriptionsResponse;
  TModifyAckDeadlineRequestArray = Array of TModifyAckDeadlineRequest;
  TAcknowledgeRequestArray = Array of TAcknowledgeRequest;
  TPullRequestArray = Array of TPullRequest;
  TPullResponseArray = Array of TPullResponse;
  TReceivedMessageArray = Array of TReceivedMessage;
  TModifyPushConfigRequestArray = Array of TModifyPushConfigRequest;
  //Anonymous types, using auto-generated names
  TPubsubMessageTypeattributes = Class;
  TPushConfigTypeattributes = Class;
  TPolicyTypebindingsArray = Array of TBinding;
  TPublishRequestTypemessagesArray = Array of TPubsubMessage;
  TListTopicsResponseTypetopicsArray = Array of TTopic;
  TListSubscriptionsResponseTypesubscriptionsArray = Array of TSubscription;
  TPullResponseTypereceivedMessagesArray = Array of TReceivedMessage;
  
  { --------------------------------------------------------------------
    TSetIamPolicyRequest
    --------------------------------------------------------------------}
  
  TSetIamPolicyRequest = Class(TGoogleBaseObject)
  Private
    Fpolicy : TPolicy;
  Protected
    //Property setters
    Procedure Setpolicy(AIndex : Integer; const AValue : TPolicy); virtual;
  Public
  Published
    Property policy : TPolicy Index 0 Read Fpolicy Write Setpolicy;
  end;
  TSetIamPolicyRequestClass = Class of TSetIamPolicyRequest;
  
  { --------------------------------------------------------------------
    TPolicy
    --------------------------------------------------------------------}
  
  TPolicy = Class(TGoogleBaseObject)
  Private
    Fversion : integer;
    Fbindings : TPolicyTypebindingsArray;
    Fetag : String;
  Protected
    //Property setters
    Procedure Setversion(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setbindings(AIndex : Integer; const AValue : TPolicyTypebindingsArray); virtual;
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property version : integer Index 0 Read Fversion Write Setversion;
    Property bindings : TPolicyTypebindingsArray Index 8 Read Fbindings Write Setbindings;
    Property etag : String Index 16 Read Fetag Write Setetag;
  end;
  TPolicyClass = Class of TPolicy;
  
  { --------------------------------------------------------------------
    TBinding
    --------------------------------------------------------------------}
  
  TBinding = Class(TGoogleBaseObject)
  Private
    Frole : String;
    Fmembers : TStringArray;
  Protected
    //Property setters
    Procedure Setrole(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmembers(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property role : String Index 0 Read Frole Write Setrole;
    Property members : TStringArray Index 8 Read Fmembers Write Setmembers;
  end;
  TBindingClass = Class of TBinding;
  
  { --------------------------------------------------------------------
    TTestIamPermissionsRequest
    --------------------------------------------------------------------}
  
  TTestIamPermissionsRequest = Class(TGoogleBaseObject)
  Private
    Fpermissions : TStringArray;
  Protected
    //Property setters
    Procedure Setpermissions(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property permissions : TStringArray Index 0 Read Fpermissions Write Setpermissions;
  end;
  TTestIamPermissionsRequestClass = Class of TTestIamPermissionsRequest;
  
  { --------------------------------------------------------------------
    TTestIamPermissionsResponse
    --------------------------------------------------------------------}
  
  TTestIamPermissionsResponse = Class(TGoogleBaseObject)
  Private
    Fpermissions : TStringArray;
  Protected
    //Property setters
    Procedure Setpermissions(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property permissions : TStringArray Index 0 Read Fpermissions Write Setpermissions;
  end;
  TTestIamPermissionsResponseClass = Class of TTestIamPermissionsResponse;
  
  { --------------------------------------------------------------------
    TTopic
    --------------------------------------------------------------------}
  
  TTopic = Class(TGoogleBaseObject)
  Private
    Fname : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
  end;
  TTopicClass = Class of TTopic;
  
  { --------------------------------------------------------------------
    TPublishRequest
    --------------------------------------------------------------------}
  
  TPublishRequest = Class(TGoogleBaseObject)
  Private
    Fmessages : TPublishRequestTypemessagesArray;
  Protected
    //Property setters
    Procedure Setmessages(AIndex : Integer; const AValue : TPublishRequestTypemessagesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property messages : TPublishRequestTypemessagesArray Index 0 Read Fmessages Write Setmessages;
  end;
  TPublishRequestClass = Class of TPublishRequest;
  
  { --------------------------------------------------------------------
    TPubsubMessageTypeattributes
    --------------------------------------------------------------------}
  
  TPubsubMessageTypeattributes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TPubsubMessageTypeattributesClass = Class of TPubsubMessageTypeattributes;
  
  { --------------------------------------------------------------------
    TPubsubMessage
    --------------------------------------------------------------------}
  
  TPubsubMessage = Class(TGoogleBaseObject)
  Private
    Fdata : String;
    Fattributes : TPubsubMessageTypeattributes;
    FmessageId : String;
    FpublishTime : String;
  Protected
    //Property setters
    Procedure Setdata(AIndex : Integer; const AValue : String); virtual;
    Procedure Setattributes(AIndex : Integer; const AValue : TPubsubMessageTypeattributes); virtual;
    Procedure SetmessageId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpublishTime(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property data : String Index 0 Read Fdata Write Setdata;
    Property attributes : TPubsubMessageTypeattributes Index 8 Read Fattributes Write Setattributes;
    Property messageId : String Index 16 Read FmessageId Write SetmessageId;
    Property publishTime : String Index 24 Read FpublishTime Write SetpublishTime;
  end;
  TPubsubMessageClass = Class of TPubsubMessage;
  
  { --------------------------------------------------------------------
    TPublishResponse
    --------------------------------------------------------------------}
  
  TPublishResponse = Class(TGoogleBaseObject)
  Private
    FmessageIds : TStringArray;
  Protected
    //Property setters
    Procedure SetmessageIds(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property messageIds : TStringArray Index 0 Read FmessageIds Write SetmessageIds;
  end;
  TPublishResponseClass = Class of TPublishResponse;
  
  { --------------------------------------------------------------------
    TListTopicsResponse
    --------------------------------------------------------------------}
  
  TListTopicsResponse = Class(TGoogleBaseObject)
  Private
    Ftopics : TListTopicsResponseTypetopicsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Settopics(AIndex : Integer; const AValue : TListTopicsResponseTypetopicsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property topics : TListTopicsResponseTypetopicsArray Index 0 Read Ftopics Write Settopics;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListTopicsResponseClass = Class of TListTopicsResponse;
  
  { --------------------------------------------------------------------
    TListTopicSubscriptionsResponse
    --------------------------------------------------------------------}
  
  TListTopicSubscriptionsResponse = Class(TGoogleBaseObject)
  Private
    Fsubscriptions : TStringArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setsubscriptions(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property subscriptions : TStringArray Index 0 Read Fsubscriptions Write Setsubscriptions;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListTopicSubscriptionsResponseClass = Class of TListTopicSubscriptionsResponse;
  
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
    TSubscription
    --------------------------------------------------------------------}
  
  TSubscription = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Ftopic : String;
    FpushConfig : TPushConfig;
    FackDeadlineSeconds : integer;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Settopic(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpushConfig(AIndex : Integer; const AValue : TPushConfig); virtual;
    Procedure SetackDeadlineSeconds(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property topic : String Index 8 Read Ftopic Write Settopic;
    Property pushConfig : TPushConfig Index 16 Read FpushConfig Write SetpushConfig;
    Property ackDeadlineSeconds : integer Index 24 Read FackDeadlineSeconds Write SetackDeadlineSeconds;
  end;
  TSubscriptionClass = Class of TSubscription;
  
  { --------------------------------------------------------------------
    TPushConfigTypeattributes
    --------------------------------------------------------------------}
  
  TPushConfigTypeattributes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TPushConfigTypeattributesClass = Class of TPushConfigTypeattributes;
  
  { --------------------------------------------------------------------
    TPushConfig
    --------------------------------------------------------------------}
  
  TPushConfig = Class(TGoogleBaseObject)
  Private
    FpushEndpoint : String;
    Fattributes : TPushConfigTypeattributes;
  Protected
    //Property setters
    Procedure SetpushEndpoint(AIndex : Integer; const AValue : String); virtual;
    Procedure Setattributes(AIndex : Integer; const AValue : TPushConfigTypeattributes); virtual;
  Public
  Published
    Property pushEndpoint : String Index 0 Read FpushEndpoint Write SetpushEndpoint;
    Property attributes : TPushConfigTypeattributes Index 8 Read Fattributes Write Setattributes;
  end;
  TPushConfigClass = Class of TPushConfig;
  
  { --------------------------------------------------------------------
    TListSubscriptionsResponse
    --------------------------------------------------------------------}
  
  TListSubscriptionsResponse = Class(TGoogleBaseObject)
  Private
    Fsubscriptions : TListSubscriptionsResponseTypesubscriptionsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setsubscriptions(AIndex : Integer; const AValue : TListSubscriptionsResponseTypesubscriptionsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property subscriptions : TListSubscriptionsResponseTypesubscriptionsArray Index 0 Read Fsubscriptions Write Setsubscriptions;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListSubscriptionsResponseClass = Class of TListSubscriptionsResponse;
  
  { --------------------------------------------------------------------
    TModifyAckDeadlineRequest
    --------------------------------------------------------------------}
  
  TModifyAckDeadlineRequest = Class(TGoogleBaseObject)
  Private
    FackIds : TStringArray;
    FackDeadlineSeconds : integer;
  Protected
    //Property setters
    Procedure SetackIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetackDeadlineSeconds(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property ackIds : TStringArray Index 0 Read FackIds Write SetackIds;
    Property ackDeadlineSeconds : integer Index 8 Read FackDeadlineSeconds Write SetackDeadlineSeconds;
  end;
  TModifyAckDeadlineRequestClass = Class of TModifyAckDeadlineRequest;
  
  { --------------------------------------------------------------------
    TAcknowledgeRequest
    --------------------------------------------------------------------}
  
  TAcknowledgeRequest = Class(TGoogleBaseObject)
  Private
    FackIds : TStringArray;
  Protected
    //Property setters
    Procedure SetackIds(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property ackIds : TStringArray Index 0 Read FackIds Write SetackIds;
  end;
  TAcknowledgeRequestClass = Class of TAcknowledgeRequest;
  
  { --------------------------------------------------------------------
    TPullRequest
    --------------------------------------------------------------------}
  
  TPullRequest = Class(TGoogleBaseObject)
  Private
    FreturnImmediately : boolean;
    FmaxMessages : integer;
  Protected
    //Property setters
    Procedure SetreturnImmediately(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetmaxMessages(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property returnImmediately : boolean Index 0 Read FreturnImmediately Write SetreturnImmediately;
    Property maxMessages : integer Index 8 Read FmaxMessages Write SetmaxMessages;
  end;
  TPullRequestClass = Class of TPullRequest;
  
  { --------------------------------------------------------------------
    TPullResponse
    --------------------------------------------------------------------}
  
  TPullResponse = Class(TGoogleBaseObject)
  Private
    FreceivedMessages : TPullResponseTypereceivedMessagesArray;
  Protected
    //Property setters
    Procedure SetreceivedMessages(AIndex : Integer; const AValue : TPullResponseTypereceivedMessagesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property receivedMessages : TPullResponseTypereceivedMessagesArray Index 0 Read FreceivedMessages Write SetreceivedMessages;
  end;
  TPullResponseClass = Class of TPullResponse;
  
  { --------------------------------------------------------------------
    TReceivedMessage
    --------------------------------------------------------------------}
  
  TReceivedMessage = Class(TGoogleBaseObject)
  Private
    FackId : String;
    Fmessage : TPubsubMessage;
  Protected
    //Property setters
    Procedure SetackId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmessage(AIndex : Integer; const AValue : TPubsubMessage); virtual;
  Public
  Published
    Property ackId : String Index 0 Read FackId Write SetackId;
    Property message : TPubsubMessage Index 8 Read Fmessage Write Setmessage;
  end;
  TReceivedMessageClass = Class of TReceivedMessage;
  
  { --------------------------------------------------------------------
    TModifyPushConfigRequest
    --------------------------------------------------------------------}
  
  TModifyPushConfigRequest = Class(TGoogleBaseObject)
  Private
    FpushConfig : TPushConfig;
  Protected
    //Property setters
    Procedure SetpushConfig(AIndex : Integer; const AValue : TPushConfig); virtual;
  Public
  Published
    Property pushConfig : TPushConfig Index 0 Read FpushConfig Write SetpushConfig;
  end;
  TModifyPushConfigRequestClass = Class of TModifyPushConfigRequest;
  
  { --------------------------------------------------------------------
    TProjectsTopicsSubscriptionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsTopicsSubscriptionsResource, method List
  
  TProjectsTopicsSubscriptionsListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  TProjectsTopicsSubscriptionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(topic: string; AQuery : string  = '') : TListTopicSubscriptionsResponse;
    Function List(topic: string; AQuery : TProjectsTopicsSubscriptionslistOptions) : TListTopicSubscriptionsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsTopicsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsTopicsResource, method List
  
  TProjectsTopicsListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  TProjectsTopicsResource = Class(TGoogleResource)
  Private
    FSubscriptionsInstance : TProjectsTopicsSubscriptionsResource;
    Function GetSubscriptionsInstance : TProjectsTopicsSubscriptionsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function SetIamPolicy(resource: string; aSetIamPolicyRequest : TSetIamPolicyRequest) : TPolicy;
    Function GetIamPolicy(resource: string) : TPolicy;
    Function TestIamPermissions(resource: string; aTestIamPermissionsRequest : TTestIamPermissionsRequest) : TTestIamPermissionsResponse;
    Function Create(_name: string; aTopic : TTopic) : TTopic;overload;
    Function Publish(topic: string; aPublishRequest : TPublishRequest) : TPublishResponse;
    Function Get(topic: string) : TTopic;
    Function List(project: string; AQuery : string  = '') : TListTopicsResponse;
    Function List(project: string; AQuery : TProjectsTopicslistOptions) : TListTopicsResponse;
    Function Delete(topic: string) : TEmpty;
    Function CreateSubscriptionsResource(AOwner : TComponent) : TProjectsTopicsSubscriptionsResource;virtual;overload;
    Function CreateSubscriptionsResource : TProjectsTopicsSubscriptionsResource;virtual;overload;
    Property SubscriptionsResource : TProjectsTopicsSubscriptionsResource Read GetSubscriptionsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsSubscriptionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsSubscriptionsResource, method List
  
  TProjectsSubscriptionsListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  TProjectsSubscriptionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function SetIamPolicy(resource: string; aSetIamPolicyRequest : TSetIamPolicyRequest) : TPolicy;
    Function GetIamPolicy(resource: string) : TPolicy;
    Function TestIamPermissions(resource: string; aTestIamPermissionsRequest : TTestIamPermissionsRequest) : TTestIamPermissionsResponse;
    Function Create(_name: string; aSubscription : TSubscription) : TSubscription;overload;
    Function Get(subscription: string) : TSubscription;
    Function List(project: string; AQuery : string  = '') : TListSubscriptionsResponse;
    Function List(project: string; AQuery : TProjectsSubscriptionslistOptions) : TListSubscriptionsResponse;
    Function Delete(subscription: string) : TEmpty;
    Function ModifyAckDeadline(subscription: string; aModifyAckDeadlineRequest : TModifyAckDeadlineRequest) : TEmpty;
    Function Acknowledge(subscription: string; aAcknowledgeRequest : TAcknowledgeRequest) : TEmpty;
    Function Pull(subscription: string; aPullRequest : TPullRequest) : TPullResponse;
    Function ModifyPushConfig(subscription: string; aModifyPushConfigRequest : TModifyPushConfigRequest) : TEmpty;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Private
    FTopicsSubscriptionsInstance : TProjectsTopicsSubscriptionsResource;
    FTopicsInstance : TProjectsTopicsResource;
    FSubscriptionsInstance : TProjectsSubscriptionsResource;
    Function GetTopicsSubscriptionsInstance : TProjectsTopicsSubscriptionsResource;virtual;
    Function GetTopicsInstance : TProjectsTopicsResource;virtual;
    Function GetSubscriptionsInstance : TProjectsSubscriptionsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateTopicsSubscriptionsResource(AOwner : TComponent) : TProjectsTopicsSubscriptionsResource;virtual;overload;
    Function CreateTopicsSubscriptionsResource : TProjectsTopicsSubscriptionsResource;virtual;overload;
    Function CreateTopicsResource(AOwner : TComponent) : TProjectsTopicsResource;virtual;overload;
    Function CreateTopicsResource : TProjectsTopicsResource;virtual;overload;
    Function CreateSubscriptionsResource(AOwner : TComponent) : TProjectsSubscriptionsResource;virtual;overload;
    Function CreateSubscriptionsResource : TProjectsSubscriptionsResource;virtual;overload;
    Property TopicsSubscriptionsResource : TProjectsTopicsSubscriptionsResource Read GetTopicsSubscriptionsInstance;
    Property TopicsResource : TProjectsTopicsResource Read GetTopicsInstance;
    Property SubscriptionsResource : TProjectsSubscriptionsResource Read GetSubscriptionsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TPubsubAPI
    --------------------------------------------------------------------}
  
  TPubsubAPI = Class(TGoogleAPI)
  Private
    FProjectsTopicsSubscriptionsInstance : TProjectsTopicsSubscriptionsResource;
    FProjectsTopicsInstance : TProjectsTopicsResource;
    FProjectsSubscriptionsInstance : TProjectsSubscriptionsResource;
    FProjectsInstance : TProjectsResource;
    Function GetProjectsTopicsSubscriptionsInstance : TProjectsTopicsSubscriptionsResource;virtual;
    Function GetProjectsTopicsInstance : TProjectsTopicsResource;virtual;
    Function GetProjectsSubscriptionsInstance : TProjectsSubscriptionsResource;virtual;
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
    Function CreateProjectsTopicsSubscriptionsResource(AOwner : TComponent) : TProjectsTopicsSubscriptionsResource;virtual;overload;
    Function CreateProjectsTopicsSubscriptionsResource : TProjectsTopicsSubscriptionsResource;virtual;overload;
    Function CreateProjectsTopicsResource(AOwner : TComponent) : TProjectsTopicsResource;virtual;overload;
    Function CreateProjectsTopicsResource : TProjectsTopicsResource;virtual;overload;
    Function CreateProjectsSubscriptionsResource(AOwner : TComponent) : TProjectsSubscriptionsResource;virtual;overload;
    Function CreateProjectsSubscriptionsResource : TProjectsSubscriptionsResource;virtual;overload;
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ProjectsTopicsSubscriptionsResource : TProjectsTopicsSubscriptionsResource Read GetProjectsTopicsSubscriptionsInstance;
    Property ProjectsTopicsResource : TProjectsTopicsResource Read GetProjectsTopicsInstance;
    Property ProjectsSubscriptionsResource : TProjectsSubscriptionsResource Read GetProjectsSubscriptionsInstance;
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TSetIamPolicyRequest
  --------------------------------------------------------------------}


Procedure TSetIamPolicyRequest.Setpolicy(AIndex : Integer; const AValue : TPolicy); 

begin
  If (Fpolicy=AValue) then exit;
  Fpolicy:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPolicy
  --------------------------------------------------------------------}


Procedure TPolicy.Setversion(AIndex : Integer; const AValue : integer); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPolicy.Setbindings(AIndex : Integer; const AValue : TPolicyTypebindingsArray); 

begin
  If (Fbindings=AValue) then exit;
  Fbindings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPolicy.Setetag(AIndex : Integer; const AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPolicy.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'bindings' : SetLength(Fbindings,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBinding
  --------------------------------------------------------------------}


Procedure TBinding.Setrole(AIndex : Integer; const AValue : String); 

begin
  If (Frole=AValue) then exit;
  Frole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBinding.Setmembers(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fmembers=AValue) then exit;
  Fmembers:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBinding.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'members' : SetLength(Fmembers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTestIamPermissionsRequest
  --------------------------------------------------------------------}


Procedure TTestIamPermissionsRequest.Setpermissions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTestIamPermissionsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'permissions' : SetLength(Fpermissions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTestIamPermissionsResponse
  --------------------------------------------------------------------}


Procedure TTestIamPermissionsResponse.Setpermissions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTestIamPermissionsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'permissions' : SetLength(Fpermissions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTopic
  --------------------------------------------------------------------}


Procedure TTopic.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPublishRequest
  --------------------------------------------------------------------}


Procedure TPublishRequest.Setmessages(AIndex : Integer; const AValue : TPublishRequestTypemessagesArray); 

begin
  If (Fmessages=AValue) then exit;
  Fmessages:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPublishRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'messages' : SetLength(Fmessages,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPubsubMessageTypeattributes
  --------------------------------------------------------------------}


Class Function TPubsubMessageTypeattributes.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TPubsubMessage
  --------------------------------------------------------------------}


Procedure TPubsubMessage.Setdata(AIndex : Integer; const AValue : String); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPubsubMessage.Setattributes(AIndex : Integer; const AValue : TPubsubMessageTypeattributes); 

begin
  If (Fattributes=AValue) then exit;
  Fattributes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPubsubMessage.SetmessageId(AIndex : Integer; const AValue : String); 

begin
  If (FmessageId=AValue) then exit;
  FmessageId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPubsubMessage.SetpublishTime(AIndex : Integer; const AValue : String); 

begin
  If (FpublishTime=AValue) then exit;
  FpublishTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPublishResponse
  --------------------------------------------------------------------}


Procedure TPublishResponse.SetmessageIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FmessageIds=AValue) then exit;
  FmessageIds:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPublishResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'messageids' : SetLength(FmessageIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListTopicsResponse
  --------------------------------------------------------------------}


Procedure TListTopicsResponse.Settopics(AIndex : Integer; const AValue : TListTopicsResponseTypetopicsArray); 

begin
  If (Ftopics=AValue) then exit;
  Ftopics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListTopicsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListTopicsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'topics' : SetLength(Ftopics,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListTopicSubscriptionsResponse
  --------------------------------------------------------------------}


Procedure TListTopicSubscriptionsResponse.Setsubscriptions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fsubscriptions=AValue) then exit;
  Fsubscriptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListTopicSubscriptionsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListTopicSubscriptionsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'subscriptions' : SetLength(Fsubscriptions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEmpty
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSubscription
  --------------------------------------------------------------------}


Procedure TSubscription.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.Settopic(AIndex : Integer; const AValue : String); 

begin
  If (Ftopic=AValue) then exit;
  Ftopic:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.SetpushConfig(AIndex : Integer; const AValue : TPushConfig); 

begin
  If (FpushConfig=AValue) then exit;
  FpushConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.SetackDeadlineSeconds(AIndex : Integer; const AValue : integer); 

begin
  If (FackDeadlineSeconds=AValue) then exit;
  FackDeadlineSeconds:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPushConfigTypeattributes
  --------------------------------------------------------------------}


Class Function TPushConfigTypeattributes.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TPushConfig
  --------------------------------------------------------------------}


Procedure TPushConfig.SetpushEndpoint(AIndex : Integer; const AValue : String); 

begin
  If (FpushEndpoint=AValue) then exit;
  FpushEndpoint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPushConfig.Setattributes(AIndex : Integer; const AValue : TPushConfigTypeattributes); 

begin
  If (Fattributes=AValue) then exit;
  Fattributes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListSubscriptionsResponse
  --------------------------------------------------------------------}


Procedure TListSubscriptionsResponse.Setsubscriptions(AIndex : Integer; const AValue : TListSubscriptionsResponseTypesubscriptionsArray); 

begin
  If (Fsubscriptions=AValue) then exit;
  Fsubscriptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListSubscriptionsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListSubscriptionsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'subscriptions' : SetLength(Fsubscriptions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TModifyAckDeadlineRequest
  --------------------------------------------------------------------}


Procedure TModifyAckDeadlineRequest.SetackIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FackIds=AValue) then exit;
  FackIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TModifyAckDeadlineRequest.SetackDeadlineSeconds(AIndex : Integer; const AValue : integer); 

begin
  If (FackDeadlineSeconds=AValue) then exit;
  FackDeadlineSeconds:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TModifyAckDeadlineRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'ackids' : SetLength(FackIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAcknowledgeRequest
  --------------------------------------------------------------------}


Procedure TAcknowledgeRequest.SetackIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FackIds=AValue) then exit;
  FackIds:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAcknowledgeRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'ackids' : SetLength(FackIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPullRequest
  --------------------------------------------------------------------}


Procedure TPullRequest.SetreturnImmediately(AIndex : Integer; const AValue : boolean); 

begin
  If (FreturnImmediately=AValue) then exit;
  FreturnImmediately:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPullRequest.SetmaxMessages(AIndex : Integer; const AValue : integer); 

begin
  If (FmaxMessages=AValue) then exit;
  FmaxMessages:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPullResponse
  --------------------------------------------------------------------}


Procedure TPullResponse.SetreceivedMessages(AIndex : Integer; const AValue : TPullResponseTypereceivedMessagesArray); 

begin
  If (FreceivedMessages=AValue) then exit;
  FreceivedMessages:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPullResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'receivedmessages' : SetLength(FreceivedMessages,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TReceivedMessage
  --------------------------------------------------------------------}


Procedure TReceivedMessage.SetackId(AIndex : Integer; const AValue : String); 

begin
  If (FackId=AValue) then exit;
  FackId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReceivedMessage.Setmessage(AIndex : Integer; const AValue : TPubsubMessage); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TModifyPushConfigRequest
  --------------------------------------------------------------------}


Procedure TModifyPushConfigRequest.SetpushConfig(AIndex : Integer; const AValue : TPushConfig); 

begin
  If (FpushConfig=AValue) then exit;
  FpushConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProjectsTopicsSubscriptionsResource
  --------------------------------------------------------------------}


Class Function TProjectsTopicsSubscriptionsResource.ResourceName : String;

begin
  Result:='subscriptions';
end;

Class Function TProjectsTopicsSubscriptionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TpubsubAPI;
end;

Function TProjectsTopicsSubscriptionsResource.List(topic: string; AQuery : string = '') : TListTopicSubscriptionsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+topic}/subscriptions';
  _Methodid   = 'pubsub.projects.topics.subscriptions.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['topic',topic]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListTopicSubscriptionsResponse) as TListTopicSubscriptionsResponse;
end;


Function TProjectsTopicsSubscriptionsResource.List(topic: string; AQuery : TProjectsTopicsSubscriptionslistOptions) : TListTopicSubscriptionsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(topic,_Q);
end;



{ --------------------------------------------------------------------
  TProjectsTopicsResource
  --------------------------------------------------------------------}


Class Function TProjectsTopicsResource.ResourceName : String;

begin
  Result:='topics';
end;

Class Function TProjectsTopicsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TpubsubAPI;
end;

Function TProjectsTopicsResource.SetIamPolicy(resource: string; aSetIamPolicyRequest : TSetIamPolicyRequest) : TPolicy;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+resource}:setIamPolicy';
  _Methodid   = 'pubsub.projects.topics.setIamPolicy';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['resource',resource]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSetIamPolicyRequest,TPolicy) as TPolicy;
end;

Function TProjectsTopicsResource.GetIamPolicy(resource: string) : TPolicy;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+resource}:getIamPolicy';
  _Methodid   = 'pubsub.projects.topics.getIamPolicy';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['resource',resource]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPolicy) as TPolicy;
end;

Function TProjectsTopicsResource.TestIamPermissions(resource: string; aTestIamPermissionsRequest : TTestIamPermissionsRequest) : TTestIamPermissionsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+resource}:testIamPermissions';
  _Methodid   = 'pubsub.projects.topics.testIamPermissions';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['resource',resource]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTestIamPermissionsRequest,TTestIamPermissionsResponse) as TTestIamPermissionsResponse;
end;

Function TProjectsTopicsResource.Create(_name: string; aTopic : TTopic) : TTopic;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v1/{+name}';
  _Methodid   = 'pubsub.projects.topics.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTopic,TTopic) as TTopic;
end;

Function TProjectsTopicsResource.Publish(topic: string; aPublishRequest : TPublishRequest) : TPublishResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+topic}:publish';
  _Methodid   = 'pubsub.projects.topics.publish';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['topic',topic]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aPublishRequest,TPublishResponse) as TPublishResponse;
end;

Function TProjectsTopicsResource.Get(topic: string) : TTopic;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+topic}';
  _Methodid   = 'pubsub.projects.topics.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['topic',topic]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTopic) as TTopic;
end;

Function TProjectsTopicsResource.List(project: string; AQuery : string = '') : TListTopicsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+project}/topics';
  _Methodid   = 'pubsub.projects.topics.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListTopicsResponse) as TListTopicsResponse;
end;


Function TProjectsTopicsResource.List(project: string; AQuery : TProjectsTopicslistOptions) : TListTopicsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;

Function TProjectsTopicsResource.Delete(topic: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/{+topic}';
  _Methodid   = 'pubsub.projects.topics.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['topic',topic]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;



Function TProjectsTopicsResource.GetSubscriptionsInstance : TProjectsTopicsSubscriptionsResource;

begin
  if (FSubscriptionsInstance=Nil) then
    FSubscriptionsInstance:=CreateSubscriptionsResource;
  Result:=FSubscriptionsInstance;
end;

Function TProjectsTopicsResource.CreateSubscriptionsResource : TProjectsTopicsSubscriptionsResource;

begin
  Result:=CreateSubscriptionsResource(Self);
end;


Function TProjectsTopicsResource.CreateSubscriptionsResource(AOwner : TComponent) : TProjectsTopicsSubscriptionsResource;

begin
  Result:=TProjectsTopicsSubscriptionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TProjectsSubscriptionsResource
  --------------------------------------------------------------------}


Class Function TProjectsSubscriptionsResource.ResourceName : String;

begin
  Result:='subscriptions';
end;

Class Function TProjectsSubscriptionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TpubsubAPI;
end;

Function TProjectsSubscriptionsResource.SetIamPolicy(resource: string; aSetIamPolicyRequest : TSetIamPolicyRequest) : TPolicy;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+resource}:setIamPolicy';
  _Methodid   = 'pubsub.projects.subscriptions.setIamPolicy';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['resource',resource]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSetIamPolicyRequest,TPolicy) as TPolicy;
end;

Function TProjectsSubscriptionsResource.GetIamPolicy(resource: string) : TPolicy;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+resource}:getIamPolicy';
  _Methodid   = 'pubsub.projects.subscriptions.getIamPolicy';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['resource',resource]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPolicy) as TPolicy;
end;

Function TProjectsSubscriptionsResource.TestIamPermissions(resource: string; aTestIamPermissionsRequest : TTestIamPermissionsRequest) : TTestIamPermissionsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+resource}:testIamPermissions';
  _Methodid   = 'pubsub.projects.subscriptions.testIamPermissions';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['resource',resource]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTestIamPermissionsRequest,TTestIamPermissionsResponse) as TTestIamPermissionsResponse;
end;

Function TProjectsSubscriptionsResource.Create(_name: string; aSubscription : TSubscription) : TSubscription;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v1/{+name}';
  _Methodid   = 'pubsub.projects.subscriptions.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSubscription,TSubscription) as TSubscription;
end;

Function TProjectsSubscriptionsResource.Get(subscription: string) : TSubscription;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+subscription}';
  _Methodid   = 'pubsub.projects.subscriptions.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['subscription',subscription]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TSubscription) as TSubscription;
end;

Function TProjectsSubscriptionsResource.List(project: string; AQuery : string = '') : TListSubscriptionsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+project}/subscriptions';
  _Methodid   = 'pubsub.projects.subscriptions.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListSubscriptionsResponse) as TListSubscriptionsResponse;
end;


Function TProjectsSubscriptionsResource.List(project: string; AQuery : TProjectsSubscriptionslistOptions) : TListSubscriptionsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;

Function TProjectsSubscriptionsResource.Delete(subscription: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/{+subscription}';
  _Methodid   = 'pubsub.projects.subscriptions.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['subscription',subscription]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;

Function TProjectsSubscriptionsResource.ModifyAckDeadline(subscription: string; aModifyAckDeadlineRequest : TModifyAckDeadlineRequest) : TEmpty;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+subscription}:modifyAckDeadline';
  _Methodid   = 'pubsub.projects.subscriptions.modifyAckDeadline';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['subscription',subscription]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aModifyAckDeadlineRequest,TEmpty) as TEmpty;
end;

Function TProjectsSubscriptionsResource.Acknowledge(subscription: string; aAcknowledgeRequest : TAcknowledgeRequest) : TEmpty;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+subscription}:acknowledge';
  _Methodid   = 'pubsub.projects.subscriptions.acknowledge';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['subscription',subscription]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAcknowledgeRequest,TEmpty) as TEmpty;
end;

Function TProjectsSubscriptionsResource.Pull(subscription: string; aPullRequest : TPullRequest) : TPullResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+subscription}:pull';
  _Methodid   = 'pubsub.projects.subscriptions.pull';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['subscription',subscription]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aPullRequest,TPullResponse) as TPullResponse;
end;

Function TProjectsSubscriptionsResource.ModifyPushConfig(subscription: string; aModifyPushConfigRequest : TModifyPushConfigRequest) : TEmpty;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+subscription}:modifyPushConfig';
  _Methodid   = 'pubsub.projects.subscriptions.modifyPushConfig';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['subscription',subscription]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aModifyPushConfigRequest,TEmpty) as TEmpty;
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
  Result:=TpubsubAPI;
end;



Function TProjectsResource.GetTopicsSubscriptionsInstance : TProjectsTopicsSubscriptionsResource;

begin
  if (FTopicsSubscriptionsInstance=Nil) then
    FTopicsSubscriptionsInstance:=CreateTopicsSubscriptionsResource;
  Result:=FTopicsSubscriptionsInstance;
end;

Function TProjectsResource.CreateTopicsSubscriptionsResource : TProjectsTopicsSubscriptionsResource;

begin
  Result:=CreateTopicsSubscriptionsResource(Self);
end;


Function TProjectsResource.CreateTopicsSubscriptionsResource(AOwner : TComponent) : TProjectsTopicsSubscriptionsResource;

begin
  Result:=TProjectsTopicsSubscriptionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetTopicsInstance : TProjectsTopicsResource;

begin
  if (FTopicsInstance=Nil) then
    FTopicsInstance:=CreateTopicsResource;
  Result:=FTopicsInstance;
end;

Function TProjectsResource.CreateTopicsResource : TProjectsTopicsResource;

begin
  Result:=CreateTopicsResource(Self);
end;


Function TProjectsResource.CreateTopicsResource(AOwner : TComponent) : TProjectsTopicsResource;

begin
  Result:=TProjectsTopicsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetSubscriptionsInstance : TProjectsSubscriptionsResource;

begin
  if (FSubscriptionsInstance=Nil) then
    FSubscriptionsInstance:=CreateSubscriptionsResource;
  Result:=FSubscriptionsInstance;
end;

Function TProjectsResource.CreateSubscriptionsResource : TProjectsSubscriptionsResource;

begin
  Result:=CreateSubscriptionsResource(Self);
end;


Function TProjectsResource.CreateSubscriptionsResource(AOwner : TComponent) : TProjectsSubscriptionsResource;

begin
  Result:=TProjectsSubscriptionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TPubsubAPI
  --------------------------------------------------------------------}

Class Function TPubsubAPI.APIName : String;

begin
  Result:='pubsub';
end;

Class Function TPubsubAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TPubsubAPI.APIRevision : String;

begin
  Result:='20160317';
end;

Class Function TPubsubAPI.APIID : String;

begin
  Result:='pubsub:v1';
end;

Class Function TPubsubAPI.APITitle : String;

begin
  Result:='Google Cloud Pub/Sub API';
end;

Class Function TPubsubAPI.APIDescription : String;

begin
  Result:='Provides reliable, many-to-many, asynchronous messaging between applications.';
end;

Class Function TPubsubAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TPubsubAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TPubsubAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TPubsubAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TPubsubAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/pubsub/docs';
end;

Class Function TPubsubAPI.APIrootUrl : string;

begin
  Result:='https://pubsub.googleapis.com/';
end;

Class Function TPubsubAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TPubsubAPI.APIbaseURL : String;

begin
  Result:='https://pubsub.googleapis.com/';
end;

Class Function TPubsubAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TPubsubAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TPubsubAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TPubsubAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/pubsub';
  Result[1].Description:='View and manage Pub/Sub topics and subscriptions';
  
end;

Class Function TPubsubAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TPubsubAPI.RegisterAPIResources;

begin
  TSetIamPolicyRequest.RegisterObject;
  TPolicy.RegisterObject;
  TBinding.RegisterObject;
  TTestIamPermissionsRequest.RegisterObject;
  TTestIamPermissionsResponse.RegisterObject;
  TTopic.RegisterObject;
  TPublishRequest.RegisterObject;
  TPubsubMessageTypeattributes.RegisterObject;
  TPubsubMessage.RegisterObject;
  TPublishResponse.RegisterObject;
  TListTopicsResponse.RegisterObject;
  TListTopicSubscriptionsResponse.RegisterObject;
  TEmpty.RegisterObject;
  TSubscription.RegisterObject;
  TPushConfigTypeattributes.RegisterObject;
  TPushConfig.RegisterObject;
  TListSubscriptionsResponse.RegisterObject;
  TModifyAckDeadlineRequest.RegisterObject;
  TAcknowledgeRequest.RegisterObject;
  TPullRequest.RegisterObject;
  TPullResponse.RegisterObject;
  TReceivedMessage.RegisterObject;
  TModifyPushConfigRequest.RegisterObject;
end;


Function TPubsubAPI.GetProjectsTopicsSubscriptionsInstance : TProjectsTopicsSubscriptionsResource;

begin
  if (FProjectsTopicsSubscriptionsInstance=Nil) then
    FProjectsTopicsSubscriptionsInstance:=CreateProjectsTopicsSubscriptionsResource;
  Result:=FProjectsTopicsSubscriptionsInstance;
end;

Function TPubsubAPI.CreateProjectsTopicsSubscriptionsResource : TProjectsTopicsSubscriptionsResource;

begin
  Result:=CreateProjectsTopicsSubscriptionsResource(Self);
end;


Function TPubsubAPI.CreateProjectsTopicsSubscriptionsResource(AOwner : TComponent) : TProjectsTopicsSubscriptionsResource;

begin
  Result:=TProjectsTopicsSubscriptionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TPubsubAPI.GetProjectsTopicsInstance : TProjectsTopicsResource;

begin
  if (FProjectsTopicsInstance=Nil) then
    FProjectsTopicsInstance:=CreateProjectsTopicsResource;
  Result:=FProjectsTopicsInstance;
end;

Function TPubsubAPI.CreateProjectsTopicsResource : TProjectsTopicsResource;

begin
  Result:=CreateProjectsTopicsResource(Self);
end;


Function TPubsubAPI.CreateProjectsTopicsResource(AOwner : TComponent) : TProjectsTopicsResource;

begin
  Result:=TProjectsTopicsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TPubsubAPI.GetProjectsSubscriptionsInstance : TProjectsSubscriptionsResource;

begin
  if (FProjectsSubscriptionsInstance=Nil) then
    FProjectsSubscriptionsInstance:=CreateProjectsSubscriptionsResource;
  Result:=FProjectsSubscriptionsInstance;
end;

Function TPubsubAPI.CreateProjectsSubscriptionsResource : TProjectsSubscriptionsResource;

begin
  Result:=CreateProjectsSubscriptionsResource(Self);
end;


Function TPubsubAPI.CreateProjectsSubscriptionsResource(AOwner : TComponent) : TProjectsSubscriptionsResource;

begin
  Result:=TProjectsSubscriptionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TPubsubAPI.GetProjectsInstance : TProjectsResource;

begin
  if (FProjectsInstance=Nil) then
    FProjectsInstance:=CreateProjectsResource;
  Result:=FProjectsInstance;
end;

Function TPubsubAPI.CreateProjectsResource : TProjectsResource;

begin
  Result:=CreateProjectsResource(Self);
end;


Function TPubsubAPI.CreateProjectsResource(AOwner : TComponent) : TProjectsResource;

begin
  Result:=TProjectsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TPubsubAPI.RegisterAPI;
end.
