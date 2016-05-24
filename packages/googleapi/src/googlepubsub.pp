unit googlepubsub;
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
  TAcknowledgeRequest = class;
  TAcknowledgeRequestArray = Array of TAcknowledgeRequest;
  TAcknowledgeRequestackIds = class;
  TAcknowledgeRequestackIdsArray = Array of TAcknowledgeRequestackIds;
  TEmpty = class;
  TEmptyArray = Array of TEmpty;
  TListSubscriptionsResponse = class;
  TListSubscriptionsResponseArray = Array of TListSubscriptionsResponse;
  TListSubscriptionsResponsesubscriptions = class;
  TListSubscriptionsResponsesubscriptionsArray = Array of TListSubscriptionsResponsesubscriptions;
  TListTopicSubscriptionsResponse = class;
  TListTopicSubscriptionsResponseArray = Array of TListTopicSubscriptionsResponse;
  TListTopicSubscriptionsResponsesubscriptions = class;
  TListTopicSubscriptionsResponsesubscriptionsArray = Array of TListTopicSubscriptionsResponsesubscriptions;
  TListTopicsResponse = class;
  TListTopicsResponseArray = Array of TListTopicsResponse;
  TListTopicsResponsetopics = class;
  TListTopicsResponsetopicsArray = Array of TListTopicsResponsetopics;
  TModifyAckDeadlineRequest = class;
  TModifyAckDeadlineRequestArray = Array of TModifyAckDeadlineRequest;
  TModifyPushConfigRequest = class;
  TModifyPushConfigRequestArray = Array of TModifyPushConfigRequest;
  TPublishRequest = class;
  TPublishRequestArray = Array of TPublishRequest;
  TPublishRequestmessages = class;
  TPublishRequestmessagesArray = Array of TPublishRequestmessages;
  TPublishResponse = class;
  TPublishResponseArray = Array of TPublishResponse;
  TPublishResponsemessageIds = class;
  TPublishResponsemessageIdsArray = Array of TPublishResponsemessageIds;
  TPubsubMessage = class;
  TPubsubMessageArray = Array of TPubsubMessage;
  TPubsubMessageattributes = class;
  TPubsubMessageattributesArray = Array of TPubsubMessageattributes;
  TPullRequest = class;
  TPullRequestArray = Array of TPullRequest;
  TPullResponse = class;
  TPullResponseArray = Array of TPullResponse;
  TPullResponsereceivedMessages = class;
  TPullResponsereceivedMessagesArray = Array of TPullResponsereceivedMessages;
  TPushConfig = class;
  TPushConfigArray = Array of TPushConfig;
  TPushConfigattributes = class;
  TPushConfigattributesArray = Array of TPushConfigattributes;
  TReceivedMessage = class;
  TReceivedMessageArray = Array of TReceivedMessage;
  TSubscription = class;
  TSubscriptionArray = Array of TSubscription;
  TTopic = class;
  TTopicArray = Array of TTopic;
  
  { --------------------------------------------------------------------
    TAcknowledgeRequest
    --------------------------------------------------------------------}
  
  TAcknowledgeRequest = Class(TGoogleBaseObject)
  Private
    FackIds : TAcknowledgeRequestackIds;
  Protected
    //Property setters
    Procedure SetackIds(AIndex : Integer; AValue : TAcknowledgeRequestackIds); virtual;
  Public
  Published
    Property ackIds : TAcknowledgeRequestackIds Index 0 Read FackIds Write SetackIds;
  end;
  TAcknowledgeRequestClass = Class of TAcknowledgeRequest;
  
  { --------------------------------------------------------------------
    TAcknowledgeRequestackIds
    --------------------------------------------------------------------}
  
  TAcknowledgeRequestackIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAcknowledgeRequestackIdsClass = Class of TAcknowledgeRequestackIds;
  
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
    TListSubscriptionsResponse
    --------------------------------------------------------------------}
  
  TListSubscriptionsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : string;
    Fsubscriptions : TListSubscriptionsResponsesubscriptions;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setsubscriptions(AIndex : Integer; AValue : TListSubscriptionsResponsesubscriptions); virtual;
  Public
  Published
    Property nextPageToken : string Index 0 Read FnextPageToken Write SetnextPageToken;
    Property subscriptions : TListSubscriptionsResponsesubscriptions Index 8 Read Fsubscriptions Write Setsubscriptions;
  end;
  TListSubscriptionsResponseClass = Class of TListSubscriptionsResponse;
  
  { --------------------------------------------------------------------
    TListSubscriptionsResponsesubscriptions
    --------------------------------------------------------------------}
  
  TListSubscriptionsResponsesubscriptions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListSubscriptionsResponsesubscriptionsClass = Class of TListSubscriptionsResponsesubscriptions;
  
  { --------------------------------------------------------------------
    TListTopicSubscriptionsResponse
    --------------------------------------------------------------------}
  
  TListTopicSubscriptionsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : string;
    Fsubscriptions : TListTopicSubscriptionsResponsesubscriptions;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setsubscriptions(AIndex : Integer; AValue : TListTopicSubscriptionsResponsesubscriptions); virtual;
  Public
  Published
    Property nextPageToken : string Index 0 Read FnextPageToken Write SetnextPageToken;
    Property subscriptions : TListTopicSubscriptionsResponsesubscriptions Index 8 Read Fsubscriptions Write Setsubscriptions;
  end;
  TListTopicSubscriptionsResponseClass = Class of TListTopicSubscriptionsResponse;
  
  { --------------------------------------------------------------------
    TListTopicSubscriptionsResponsesubscriptions
    --------------------------------------------------------------------}
  
  TListTopicSubscriptionsResponsesubscriptions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListTopicSubscriptionsResponsesubscriptionsClass = Class of TListTopicSubscriptionsResponsesubscriptions;
  
  { --------------------------------------------------------------------
    TListTopicsResponse
    --------------------------------------------------------------------}
  
  TListTopicsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : string;
    Ftopics : TListTopicsResponsetopics;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Settopics(AIndex : Integer; AValue : TListTopicsResponsetopics); virtual;
  Public
  Published
    Property nextPageToken : string Index 0 Read FnextPageToken Write SetnextPageToken;
    Property topics : TListTopicsResponsetopics Index 8 Read Ftopics Write Settopics;
  end;
  TListTopicsResponseClass = Class of TListTopicsResponse;
  
  { --------------------------------------------------------------------
    TListTopicsResponsetopics
    --------------------------------------------------------------------}
  
  TListTopicsResponsetopics = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListTopicsResponsetopicsClass = Class of TListTopicsResponsetopics;
  
  { --------------------------------------------------------------------
    TModifyAckDeadlineRequest
    --------------------------------------------------------------------}
  
  TModifyAckDeadlineRequest = Class(TGoogleBaseObject)
  Private
    FackDeadlineSeconds : integer;
    FackId : string;
  Protected
    //Property setters
    Procedure SetackDeadlineSeconds(AIndex : Integer; AValue : integer); virtual;
    Procedure SetackId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property ackDeadlineSeconds : integer Index 0 Read FackDeadlineSeconds Write SetackDeadlineSeconds;
    Property ackId : string Index 8 Read FackId Write SetackId;
  end;
  TModifyAckDeadlineRequestClass = Class of TModifyAckDeadlineRequest;
  
  { --------------------------------------------------------------------
    TModifyPushConfigRequest
    --------------------------------------------------------------------}
  
  TModifyPushConfigRequest = Class(TGoogleBaseObject)
  Private
    FpushConfig : TPushConfig;
  Protected
    //Property setters
    Procedure SetpushConfig(AIndex : Integer; AValue : TPushConfig); virtual;
  Public
  Published
    Property pushConfig : TPushConfig Index 0 Read FpushConfig Write SetpushConfig;
  end;
  TModifyPushConfigRequestClass = Class of TModifyPushConfigRequest;
  
  { --------------------------------------------------------------------
    TPublishRequest
    --------------------------------------------------------------------}
  
  TPublishRequest = Class(TGoogleBaseObject)
  Private
    Fmessages : TPublishRequestmessages;
  Protected
    //Property setters
    Procedure Setmessages(AIndex : Integer; AValue : TPublishRequestmessages); virtual;
  Public
  Published
    Property messages : TPublishRequestmessages Index 0 Read Fmessages Write Setmessages;
  end;
  TPublishRequestClass = Class of TPublishRequest;
  
  { --------------------------------------------------------------------
    TPublishRequestmessages
    --------------------------------------------------------------------}
  
  TPublishRequestmessages = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPublishRequestmessagesClass = Class of TPublishRequestmessages;
  
  { --------------------------------------------------------------------
    TPublishResponse
    --------------------------------------------------------------------}
  
  TPublishResponse = Class(TGoogleBaseObject)
  Private
    FmessageIds : TPublishResponsemessageIds;
  Protected
    //Property setters
    Procedure SetmessageIds(AIndex : Integer; AValue : TPublishResponsemessageIds); virtual;
  Public
  Published
    Property messageIds : TPublishResponsemessageIds Index 0 Read FmessageIds Write SetmessageIds;
  end;
  TPublishResponseClass = Class of TPublishResponse;
  
  { --------------------------------------------------------------------
    TPublishResponsemessageIds
    --------------------------------------------------------------------}
  
  TPublishResponsemessageIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPublishResponsemessageIdsClass = Class of TPublishResponsemessageIds;
  
  { --------------------------------------------------------------------
    TPubsubMessage
    --------------------------------------------------------------------}
  
  TPubsubMessage = Class(TGoogleBaseObject)
  Private
    Fattributes : TPubsubMessageattributes;
    Fdata : string;
    FmessageId : string;
  Protected
    //Property setters
    Procedure Setattributes(AIndex : Integer; AValue : TPubsubMessageattributes); virtual;
    Procedure Setdata(AIndex : Integer; AValue : string); virtual;
    Procedure SetmessageId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property attributes : TPubsubMessageattributes Index 0 Read Fattributes Write Setattributes;
    Property data : string Index 8 Read Fdata Write Setdata;
    Property messageId : string Index 16 Read FmessageId Write SetmessageId;
  end;
  TPubsubMessageClass = Class of TPubsubMessage;
  
  { --------------------------------------------------------------------
    TPubsubMessageattributes
    --------------------------------------------------------------------}
  
  TPubsubMessageattributes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TPubsubMessageattributesClass = Class of TPubsubMessageattributes;
  
  { --------------------------------------------------------------------
    TPullRequest
    --------------------------------------------------------------------}
  
  TPullRequest = Class(TGoogleBaseObject)
  Private
    FmaxMessages : integer;
    FreturnImmediately : boolean;
  Protected
    //Property setters
    Procedure SetmaxMessages(AIndex : Integer; AValue : integer); virtual;
    Procedure SetreturnImmediately(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property maxMessages : integer Index 0 Read FmaxMessages Write SetmaxMessages;
    Property returnImmediately : boolean Index 8 Read FreturnImmediately Write SetreturnImmediately;
  end;
  TPullRequestClass = Class of TPullRequest;
  
  { --------------------------------------------------------------------
    TPullResponse
    --------------------------------------------------------------------}
  
  TPullResponse = Class(TGoogleBaseObject)
  Private
    FreceivedMessages : TPullResponsereceivedMessages;
  Protected
    //Property setters
    Procedure SetreceivedMessages(AIndex : Integer; AValue : TPullResponsereceivedMessages); virtual;
  Public
  Published
    Property receivedMessages : TPullResponsereceivedMessages Index 0 Read FreceivedMessages Write SetreceivedMessages;
  end;
  TPullResponseClass = Class of TPullResponse;
  
  { --------------------------------------------------------------------
    TPullResponsereceivedMessages
    --------------------------------------------------------------------}
  
  TPullResponsereceivedMessages = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPullResponsereceivedMessagesClass = Class of TPullResponsereceivedMessages;
  
  { --------------------------------------------------------------------
    TPushConfig
    --------------------------------------------------------------------}
  
  TPushConfig = Class(TGoogleBaseObject)
  Private
    Fattributes : TPushConfigattributes;
    FpushEndpoint : string;
  Protected
    //Property setters
    Procedure Setattributes(AIndex : Integer; AValue : TPushConfigattributes); virtual;
    Procedure SetpushEndpoint(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property attributes : TPushConfigattributes Index 0 Read Fattributes Write Setattributes;
    Property pushEndpoint : string Index 8 Read FpushEndpoint Write SetpushEndpoint;
  end;
  TPushConfigClass = Class of TPushConfig;
  
  { --------------------------------------------------------------------
    TPushConfigattributes
    --------------------------------------------------------------------}
  
  TPushConfigattributes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TPushConfigattributesClass = Class of TPushConfigattributes;
  
  { --------------------------------------------------------------------
    TReceivedMessage
    --------------------------------------------------------------------}
  
  TReceivedMessage = Class(TGoogleBaseObject)
  Private
    FackId : string;
    Fmessage : TPubsubMessage;
  Protected
    //Property setters
    Procedure SetackId(AIndex : Integer; AValue : string); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : TPubsubMessage); virtual;
  Public
  Published
    Property ackId : string Index 0 Read FackId Write SetackId;
    Property message : TPubsubMessage Index 8 Read Fmessage Write Setmessage;
  end;
  TReceivedMessageClass = Class of TReceivedMessage;
  
  { --------------------------------------------------------------------
    TSubscription
    --------------------------------------------------------------------}
  
  TSubscription = Class(TGoogleBaseObject)
  Private
    FackDeadlineSeconds : integer;
    Fname : string;
    FpushConfig : TPushConfig;
    Ftopic : string;
  Protected
    //Property setters
    Procedure SetackDeadlineSeconds(AIndex : Integer; AValue : integer); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetpushConfig(AIndex : Integer; AValue : TPushConfig); virtual;
    Procedure Settopic(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property ackDeadlineSeconds : integer Index 0 Read FackDeadlineSeconds Write SetackDeadlineSeconds;
    Property name : string Index 8 Read Fname Write Setname;
    Property pushConfig : TPushConfig Index 16 Read FpushConfig Write SetpushConfig;
    Property topic : string Index 24 Read Ftopic Write Settopic;
  end;
  TSubscriptionClass = Class of TSubscription;
  
  { --------------------------------------------------------------------
    TTopic
    --------------------------------------------------------------------}
  
  TTopic = Class(TGoogleBaseObject)
  Private
    Fname : string;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
  end;
  TTopicClass = Class of TTopic;
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
  end;
  
  
  { --------------------------------------------------------------------
    TPubsubAPI
    --------------------------------------------------------------------}
  
  TPubsubAPI = Class(TGoogleAPI)
  Private
    FProjectsInstance : TProjectsResource;
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
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAcknowledgeRequest
  --------------------------------------------------------------------}


Procedure TAcknowledgeRequest.SetackIds(AIndex : Integer; AValue : TAcknowledgeRequestackIds); 

begin
  If (FackIds=AValue) then exit;
  FackIds:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAcknowledgeRequestackIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TEmpty
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListSubscriptionsResponse
  --------------------------------------------------------------------}


Procedure TListSubscriptionsResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListSubscriptionsResponse.Setsubscriptions(AIndex : Integer; AValue : TListSubscriptionsResponsesubscriptions); 

begin
  If (Fsubscriptions=AValue) then exit;
  Fsubscriptions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListSubscriptionsResponsesubscriptions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListTopicSubscriptionsResponse
  --------------------------------------------------------------------}


Procedure TListTopicSubscriptionsResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListTopicSubscriptionsResponse.Setsubscriptions(AIndex : Integer; AValue : TListTopicSubscriptionsResponsesubscriptions); 

begin
  If (Fsubscriptions=AValue) then exit;
  Fsubscriptions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListTopicSubscriptionsResponsesubscriptions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListTopicsResponse
  --------------------------------------------------------------------}


Procedure TListTopicsResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListTopicsResponse.Settopics(AIndex : Integer; AValue : TListTopicsResponsetopics); 

begin
  If (Ftopics=AValue) then exit;
  Ftopics:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListTopicsResponsetopics
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TModifyAckDeadlineRequest
  --------------------------------------------------------------------}


Procedure TModifyAckDeadlineRequest.SetackDeadlineSeconds(AIndex : Integer; AValue : integer); 

begin
  If (FackDeadlineSeconds=AValue) then exit;
  FackDeadlineSeconds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TModifyAckDeadlineRequest.SetackId(AIndex : Integer; AValue : string); 

begin
  If (FackId=AValue) then exit;
  FackId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TModifyPushConfigRequest
  --------------------------------------------------------------------}


Procedure TModifyPushConfigRequest.SetpushConfig(AIndex : Integer; AValue : TPushConfig); 

begin
  If (FpushConfig=AValue) then exit;
  FpushConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPublishRequest
  --------------------------------------------------------------------}


Procedure TPublishRequest.Setmessages(AIndex : Integer; AValue : TPublishRequestmessages); 

begin
  If (Fmessages=AValue) then exit;
  Fmessages:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPublishRequestmessages
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPublishResponse
  --------------------------------------------------------------------}


Procedure TPublishResponse.SetmessageIds(AIndex : Integer; AValue : TPublishResponsemessageIds); 

begin
  If (FmessageIds=AValue) then exit;
  FmessageIds:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPublishResponsemessageIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPubsubMessage
  --------------------------------------------------------------------}


Procedure TPubsubMessage.Setattributes(AIndex : Integer; AValue : TPubsubMessageattributes); 

begin
  If (Fattributes=AValue) then exit;
  Fattributes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPubsubMessage.Setdata(AIndex : Integer; AValue : string); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPubsubMessage.SetmessageId(AIndex : Integer; AValue : string); 

begin
  If (FmessageId=AValue) then exit;
  FmessageId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPubsubMessageattributes
  --------------------------------------------------------------------}


Class Function TPubsubMessageattributes.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TPullRequest
  --------------------------------------------------------------------}


Procedure TPullRequest.SetmaxMessages(AIndex : Integer; AValue : integer); 

begin
  If (FmaxMessages=AValue) then exit;
  FmaxMessages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPullRequest.SetreturnImmediately(AIndex : Integer; AValue : boolean); 

begin
  If (FreturnImmediately=AValue) then exit;
  FreturnImmediately:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPullResponse
  --------------------------------------------------------------------}


Procedure TPullResponse.SetreceivedMessages(AIndex : Integer; AValue : TPullResponsereceivedMessages); 

begin
  If (FreceivedMessages=AValue) then exit;
  FreceivedMessages:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPullResponsereceivedMessages
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPushConfig
  --------------------------------------------------------------------}


Procedure TPushConfig.Setattributes(AIndex : Integer; AValue : TPushConfigattributes); 

begin
  If (Fattributes=AValue) then exit;
  Fattributes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPushConfig.SetpushEndpoint(AIndex : Integer; AValue : string); 

begin
  If (FpushEndpoint=AValue) then exit;
  FpushEndpoint:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPushConfigattributes
  --------------------------------------------------------------------}


Class Function TPushConfigattributes.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TReceivedMessage
  --------------------------------------------------------------------}


Procedure TReceivedMessage.SetackId(AIndex : Integer; AValue : string); 

begin
  If (FackId=AValue) then exit;
  FackId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReceivedMessage.Setmessage(AIndex : Integer; AValue : TPubsubMessage); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSubscription
  --------------------------------------------------------------------}


Procedure TSubscription.SetackDeadlineSeconds(AIndex : Integer; AValue : integer); 

begin
  If (FackDeadlineSeconds=AValue) then exit;
  FackDeadlineSeconds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.SetpushConfig(AIndex : Integer; AValue : TPushConfig); 

begin
  If (FpushConfig=AValue) then exit;
  FpushConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.Settopic(AIndex : Integer; AValue : string); 

begin
  If (Ftopic=AValue) then exit;
  Ftopic:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTopic
  --------------------------------------------------------------------}


Procedure TTopic.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
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



{ --------------------------------------------------------------------
  TPubsubAPI
  --------------------------------------------------------------------}

Class Function TPubsubAPI.APIName : String;

begin
  Result:='pubsub';
end;

Class Function TPubsubAPI.APIVersion : String;

begin
  Result:='v1beta2';
end;

Class Function TPubsubAPI.APIRevision : String;

begin
  Result:='20150326';
end;

Class Function TPubsubAPI.APIID : String;

begin
  Result:='pubsub:v1beta2';
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
  Result:='';
end;

Class Function TPubsubAPI.APIrootUrl : string;

begin
  Result:='https://pubsub.googleapis.com/';
end;

Class Function TPubsubAPI.APIbasePath : string;

begin
  Result:='/v1beta2/';
end;

Class Function TPubsubAPI.APIbaseURL : String;

begin
  Result:='https://pubsub.googleapis.com/v1beta2/';
end;

Class Function TPubsubAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TPubsubAPI.APIservicePath : string;

begin
  Result:='v1beta2/';
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
  TAcknowledgeRequest.RegisterObject;
  TAcknowledgeRequestackIds.RegisterObject;
  TEmpty.RegisterObject;
  TListSubscriptionsResponse.RegisterObject;
  TListSubscriptionsResponsesubscriptions.RegisterObject;
  TListTopicSubscriptionsResponse.RegisterObject;
  TListTopicSubscriptionsResponsesubscriptions.RegisterObject;
  TListTopicsResponse.RegisterObject;
  TListTopicsResponsetopics.RegisterObject;
  TModifyAckDeadlineRequest.RegisterObject;
  TModifyPushConfigRequest.RegisterObject;
  TPublishRequest.RegisterObject;
  TPublishRequestmessages.RegisterObject;
  TPublishResponse.RegisterObject;
  TPublishResponsemessageIds.RegisterObject;
  TPubsubMessage.RegisterObject;
  TPubsubMessageattributes.RegisterObject;
  TPullRequest.RegisterObject;
  TPullResponse.RegisterObject;
  TPullResponsereceivedMessages.RegisterObject;
  TPushConfig.RegisterObject;
  TPushConfigattributes.RegisterObject;
  TReceivedMessage.RegisterObject;
  TSubscription.RegisterObject;
  TTopic.RegisterObject;
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
  Result.API:=Self;
end;



initialization
  TPubsubAPI.RegisterAPI;
end.
