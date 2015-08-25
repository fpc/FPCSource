unit googlepubsub;
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
  TAcknowledgeRequest = Class;
  TEmpty = Class;
  TListSubscriptionsResponse = Class;
  TListTopicSubscriptionsResponse = Class;
  TListTopicsResponse = Class;
  TModifyAckDeadlineRequest = Class;
  TModifyPushConfigRequest = Class;
  TPublishRequest = Class;
  TPublishResponse = Class;
  TPubsubMessage = Class;
  TPullRequest = Class;
  TPullResponse = Class;
  TPushConfig = Class;
  TReceivedMessage = Class;
  TSubscription = Class;
  TTopic = Class;
  TAcknowledgeRequestArray = Array of TAcknowledgeRequest;
  TEmptyArray = Array of TEmpty;
  TListSubscriptionsResponseArray = Array of TListSubscriptionsResponse;
  TListTopicSubscriptionsResponseArray = Array of TListTopicSubscriptionsResponse;
  TListTopicsResponseArray = Array of TListTopicsResponse;
  TModifyAckDeadlineRequestArray = Array of TModifyAckDeadlineRequest;
  TModifyPushConfigRequestArray = Array of TModifyPushConfigRequest;
  TPublishRequestArray = Array of TPublishRequest;
  TPublishResponseArray = Array of TPublishResponse;
  TPubsubMessageArray = Array of TPubsubMessage;
  TPullRequestArray = Array of TPullRequest;
  TPullResponseArray = Array of TPullResponse;
  TPushConfigArray = Array of TPushConfig;
  TReceivedMessageArray = Array of TReceivedMessage;
  TSubscriptionArray = Array of TSubscription;
  TTopicArray = Array of TTopic;
  //Anonymous types, using auto-generated names
  TPubsubMessageTypeattributes = Class;
  TPushConfigTypeattributes = Class;
  TListSubscriptionsResponseTypesubscriptionsArray = Array of TSubscription;
  TListTopicsResponseTypetopicsArray = Array of TTopic;
  TPublishRequestTypemessagesArray = Array of TPubsubMessage;
  TPullResponseTypereceivedMessagesArray = Array of TReceivedMessage;
  
  { --------------------------------------------------------------------
    TAcknowledgeRequest
    --------------------------------------------------------------------}
  
  TAcknowledgeRequest = Class(TGoogleBaseObject)
  Private
    FackIds : TStringArray;
  Protected
    //Property setters
    Procedure SetackIds(AIndex : Integer; AValue : TStringArray); virtual;
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
    FnextPageToken : String;
    Fsubscriptions : TListSubscriptionsResponseTypesubscriptionsArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setsubscriptions(AIndex : Integer; AValue : TListSubscriptionsResponseTypesubscriptionsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property subscriptions : TListSubscriptionsResponseTypesubscriptionsArray Index 8 Read Fsubscriptions Write Setsubscriptions;
  end;
  TListSubscriptionsResponseClass = Class of TListSubscriptionsResponse;
  
  { --------------------------------------------------------------------
    TListTopicSubscriptionsResponse
    --------------------------------------------------------------------}
  
  TListTopicSubscriptionsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Fsubscriptions : TStringArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setsubscriptions(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property subscriptions : TStringArray Index 8 Read Fsubscriptions Write Setsubscriptions;
  end;
  TListTopicSubscriptionsResponseClass = Class of TListTopicSubscriptionsResponse;
  
  { --------------------------------------------------------------------
    TListTopicsResponse
    --------------------------------------------------------------------}
  
  TListTopicsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Ftopics : TListTopicsResponseTypetopicsArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Settopics(AIndex : Integer; AValue : TListTopicsResponseTypetopicsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property topics : TListTopicsResponseTypetopicsArray Index 8 Read Ftopics Write Settopics;
  end;
  TListTopicsResponseClass = Class of TListTopicsResponse;
  
  { --------------------------------------------------------------------
    TModifyAckDeadlineRequest
    --------------------------------------------------------------------}
  
  TModifyAckDeadlineRequest = Class(TGoogleBaseObject)
  Private
    FackDeadlineSeconds : integer;
    FackId : String;
  Protected
    //Property setters
    Procedure SetackDeadlineSeconds(AIndex : Integer; AValue : integer); virtual;
    Procedure SetackId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property ackDeadlineSeconds : integer Index 0 Read FackDeadlineSeconds Write SetackDeadlineSeconds;
    Property ackId : String Index 8 Read FackId Write SetackId;
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
    Fmessages : TPublishRequestTypemessagesArray;
  Protected
    //Property setters
    Procedure Setmessages(AIndex : Integer; AValue : TPublishRequestTypemessagesArray); virtual;
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
    TPublishResponse
    --------------------------------------------------------------------}
  
  TPublishResponse = Class(TGoogleBaseObject)
  Private
    FmessageIds : TStringArray;
  Protected
    //Property setters
    Procedure SetmessageIds(AIndex : Integer; AValue : TStringArray); virtual;
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
    Fattributes : TPubsubMessageTypeattributes;
    Fdata : String;
    FmessageId : String;
  Protected
    //Property setters
    Procedure Setattributes(AIndex : Integer; AValue : TPubsubMessageTypeattributes); virtual;
    Procedure Setdata(AIndex : Integer; AValue : String); virtual;
    Procedure SetmessageId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property attributes : TPubsubMessageTypeattributes Index 0 Read Fattributes Write Setattributes;
    Property data : String Index 8 Read Fdata Write Setdata;
    Property messageId : String Index 16 Read FmessageId Write SetmessageId;
  end;
  TPubsubMessageClass = Class of TPubsubMessage;
  
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
    FreceivedMessages : TPullResponseTypereceivedMessagesArray;
  Protected
    //Property setters
    Procedure SetreceivedMessages(AIndex : Integer; AValue : TPullResponseTypereceivedMessagesArray); virtual;
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
    Fattributes : TPushConfigTypeattributes;
    FpushEndpoint : String;
  Protected
    //Property setters
    Procedure Setattributes(AIndex : Integer; AValue : TPushConfigTypeattributes); virtual;
    Procedure SetpushEndpoint(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property attributes : TPushConfigTypeattributes Index 0 Read Fattributes Write Setattributes;
    Property pushEndpoint : String Index 8 Read FpushEndpoint Write SetpushEndpoint;
  end;
  TPushConfigClass = Class of TPushConfig;
  
  { --------------------------------------------------------------------
    TReceivedMessage
    --------------------------------------------------------------------}
  
  TReceivedMessage = Class(TGoogleBaseObject)
  Private
    FackId : String;
    Fmessage : TPubsubMessage;
  Protected
    //Property setters
    Procedure SetackId(AIndex : Integer; AValue : String); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : TPubsubMessage); virtual;
  Public
  Published
    Property ackId : String Index 0 Read FackId Write SetackId;
    Property message : TPubsubMessage Index 8 Read Fmessage Write Setmessage;
  end;
  TReceivedMessageClass = Class of TReceivedMessage;
  
  { --------------------------------------------------------------------
    TSubscription
    --------------------------------------------------------------------}
  
  TSubscription = Class(TGoogleBaseObject)
  Private
    FackDeadlineSeconds : integer;
    Fname : String;
    FpushConfig : TPushConfig;
    Ftopic : String;
  Protected
    //Property setters
    Procedure SetackDeadlineSeconds(AIndex : Integer; AValue : integer); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetpushConfig(AIndex : Integer; AValue : TPushConfig); virtual;
    Procedure Settopic(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property ackDeadlineSeconds : integer Index 0 Read FackDeadlineSeconds Write SetackDeadlineSeconds;
    Property name : String Index 8 Read Fname Write Setname;
    Property pushConfig : TPushConfig Index 16 Read FpushConfig Write SetpushConfig;
    Property topic : String Index 24 Read Ftopic Write Settopic;
  end;
  TSubscriptionClass = Class of TSubscription;
  
  { --------------------------------------------------------------------
    TTopic
    --------------------------------------------------------------------}
  
  TTopic = Class(TGoogleBaseObject)
  Private
    Fname : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
  end;
  TTopicClass = Class of TTopic;
  
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
    Function Acknowledge(subscription: string; aAcknowledgeRequest : TAcknowledgeRequest) : TEmpty;
    Function Create(_name: string; aSubscription : TSubscription) : TSubscription;overload;
    Function Delete(subscription: string) : TEmpty;
    Function Get(subscription: string) : TSubscription;
    Function List(project: string; AQuery : string  = '') : TListSubscriptionsResponse;
    Function List(project: string; AQuery : TProjectsSubscriptionslistOptions) : TListSubscriptionsResponse;
    Function ModifyAckDeadline(subscription: string; aModifyAckDeadlineRequest : TModifyAckDeadlineRequest) : TEmpty;
    Function ModifyPushConfig(subscription: string; aModifyPushConfigRequest : TModifyPushConfigRequest) : TEmpty;
    Function Pull(subscription: string; aPullRequest : TPullRequest) : TPullResponse;
  end;
  
  
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
    Function Create(_name: string; aTopic : TTopic) : TTopic;overload;
    Function Delete(topic: string) : TEmpty;
    Function Get(topic: string) : TTopic;
    Function List(project: string; AQuery : string  = '') : TListTopicsResponse;
    Function List(project: string; AQuery : TProjectsTopicslistOptions) : TListTopicsResponse;
    Function Publish(topic: string; aPublishRequest : TPublishRequest) : TPublishResponse;
    Function CreateSubscriptionsResource(AOwner : TComponent) : TProjectsTopicsSubscriptionsResource;virtual;overload;
    Function CreateSubscriptionsResource : TProjectsTopicsSubscriptionsResource;virtual;overload;
    Property SubscriptionsResource : TProjectsTopicsSubscriptionsResource Read GetSubscriptionsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Private
    FSubscriptionsInstance : TProjectsSubscriptionsResource;
    FTopicsSubscriptionsInstance : TProjectsTopicsSubscriptionsResource;
    FTopicsInstance : TProjectsTopicsResource;
    Function GetSubscriptionsInstance : TProjectsSubscriptionsResource;virtual;
    Function GetTopicsSubscriptionsInstance : TProjectsTopicsSubscriptionsResource;virtual;
    Function GetTopicsInstance : TProjectsTopicsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateSubscriptionsResource(AOwner : TComponent) : TProjectsSubscriptionsResource;virtual;overload;
    Function CreateSubscriptionsResource : TProjectsSubscriptionsResource;virtual;overload;
    Function CreateTopicsSubscriptionsResource(AOwner : TComponent) : TProjectsTopicsSubscriptionsResource;virtual;overload;
    Function CreateTopicsSubscriptionsResource : TProjectsTopicsSubscriptionsResource;virtual;overload;
    Function CreateTopicsResource(AOwner : TComponent) : TProjectsTopicsResource;virtual;overload;
    Function CreateTopicsResource : TProjectsTopicsResource;virtual;overload;
    Property SubscriptionsResource : TProjectsSubscriptionsResource Read GetSubscriptionsInstance;
    Property TopicsSubscriptionsResource : TProjectsTopicsSubscriptionsResource Read GetTopicsSubscriptionsInstance;
    Property TopicsResource : TProjectsTopicsResource Read GetTopicsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TPubsubAPI
    --------------------------------------------------------------------}
  
  TPubsubAPI = Class(TGoogleAPI)
  Private
    FProjectsSubscriptionsInstance : TProjectsSubscriptionsResource;
    FProjectsTopicsSubscriptionsInstance : TProjectsTopicsSubscriptionsResource;
    FProjectsTopicsInstance : TProjectsTopicsResource;
    FProjectsInstance : TProjectsResource;
    Function GetProjectsSubscriptionsInstance : TProjectsSubscriptionsResource;virtual;
    Function GetProjectsTopicsSubscriptionsInstance : TProjectsTopicsSubscriptionsResource;virtual;
    Function GetProjectsTopicsInstance : TProjectsTopicsResource;virtual;
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
    Function CreateProjectsSubscriptionsResource(AOwner : TComponent) : TProjectsSubscriptionsResource;virtual;overload;
    Function CreateProjectsSubscriptionsResource : TProjectsSubscriptionsResource;virtual;overload;
    Function CreateProjectsTopicsSubscriptionsResource(AOwner : TComponent) : TProjectsTopicsSubscriptionsResource;virtual;overload;
    Function CreateProjectsTopicsSubscriptionsResource : TProjectsTopicsSubscriptionsResource;virtual;overload;
    Function CreateProjectsTopicsResource(AOwner : TComponent) : TProjectsTopicsResource;virtual;overload;
    Function CreateProjectsTopicsResource : TProjectsTopicsResource;virtual;overload;
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ProjectsSubscriptionsResource : TProjectsSubscriptionsResource Read GetProjectsSubscriptionsInstance;
    Property ProjectsTopicsSubscriptionsResource : TProjectsTopicsSubscriptionsResource Read GetProjectsTopicsSubscriptionsInstance;
    Property ProjectsTopicsResource : TProjectsTopicsResource Read GetProjectsTopicsInstance;
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAcknowledgeRequest
  --------------------------------------------------------------------}


Procedure TAcknowledgeRequest.SetackIds(AIndex : Integer; AValue : TStringArray); 

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
  TEmpty
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListSubscriptionsResponse
  --------------------------------------------------------------------}


Procedure TListSubscriptionsResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListSubscriptionsResponse.Setsubscriptions(AIndex : Integer; AValue : TListSubscriptionsResponseTypesubscriptionsArray); 

begin
  If (Fsubscriptions=AValue) then exit;
  Fsubscriptions:=AValue;
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
  TListTopicSubscriptionsResponse
  --------------------------------------------------------------------}


Procedure TListTopicSubscriptionsResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListTopicSubscriptionsResponse.Setsubscriptions(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fsubscriptions=AValue) then exit;
  Fsubscriptions:=AValue;
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
  TListTopicsResponse
  --------------------------------------------------------------------}


Procedure TListTopicsResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListTopicsResponse.Settopics(AIndex : Integer; AValue : TListTopicsResponseTypetopicsArray); 

begin
  If (Ftopics=AValue) then exit;
  Ftopics:=AValue;
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
  TModifyAckDeadlineRequest
  --------------------------------------------------------------------}


Procedure TModifyAckDeadlineRequest.SetackDeadlineSeconds(AIndex : Integer; AValue : integer); 

begin
  If (FackDeadlineSeconds=AValue) then exit;
  FackDeadlineSeconds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TModifyAckDeadlineRequest.SetackId(AIndex : Integer; AValue : String); 

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


Procedure TPublishRequest.Setmessages(AIndex : Integer; AValue : TPublishRequestTypemessagesArray); 

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
  TPublishResponse
  --------------------------------------------------------------------}


Procedure TPublishResponse.SetmessageIds(AIndex : Integer; AValue : TStringArray); 

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
  TPubsubMessageTypeattributes
  --------------------------------------------------------------------}


Class Function TPubsubMessageTypeattributes.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TPubsubMessage
  --------------------------------------------------------------------}


Procedure TPubsubMessage.Setattributes(AIndex : Integer; AValue : TPubsubMessageTypeattributes); 

begin
  If (Fattributes=AValue) then exit;
  Fattributes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPubsubMessage.Setdata(AIndex : Integer; AValue : String); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPubsubMessage.SetmessageId(AIndex : Integer; AValue : String); 

begin
  If (FmessageId=AValue) then exit;
  FmessageId:=AValue;
  MarkPropertyChanged(AIndex);
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


Procedure TPullResponse.SetreceivedMessages(AIndex : Integer; AValue : TPullResponseTypereceivedMessagesArray); 

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
  TPushConfigTypeattributes
  --------------------------------------------------------------------}


Class Function TPushConfigTypeattributes.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TPushConfig
  --------------------------------------------------------------------}


Procedure TPushConfig.Setattributes(AIndex : Integer; AValue : TPushConfigTypeattributes); 

begin
  If (Fattributes=AValue) then exit;
  Fattributes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPushConfig.SetpushEndpoint(AIndex : Integer; AValue : String); 

begin
  If (FpushEndpoint=AValue) then exit;
  FpushEndpoint:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReceivedMessage
  --------------------------------------------------------------------}


Procedure TReceivedMessage.SetackId(AIndex : Integer; AValue : String); 

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



Procedure TSubscription.Setname(AIndex : Integer; AValue : String); 

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



Procedure TSubscription.Settopic(AIndex : Integer; AValue : String); 

begin
  If (Ftopic=AValue) then exit;
  Ftopic:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTopic
  --------------------------------------------------------------------}


Procedure TTopic.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
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

Function TProjectsSubscriptionsResource.Acknowledge(subscription: string; aAcknowledgeRequest : TAcknowledgeRequest) : TEmpty;

Const
  _HTTPMethod = 'POST';
  _Path       = '{+subscription}:acknowledge';
  _Methodid   = 'pubsub.projects.subscriptions.acknowledge';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['subscription',subscription]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAcknowledgeRequest,TEmpty) as TEmpty;
end;

Function TProjectsSubscriptionsResource.Create(_name: string; aSubscription : TSubscription) : TSubscription;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{+name}';
  _Methodid   = 'pubsub.projects.subscriptions.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSubscription,TSubscription) as TSubscription;
end;

Function TProjectsSubscriptionsResource.Delete(subscription: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{+subscription}';
  _Methodid   = 'pubsub.projects.subscriptions.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['subscription',subscription]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;

Function TProjectsSubscriptionsResource.Get(subscription: string) : TSubscription;

Const
  _HTTPMethod = 'GET';
  _Path       = '{+subscription}';
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
  _Path       = '{+project}/subscriptions';
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

Function TProjectsSubscriptionsResource.ModifyAckDeadline(subscription: string; aModifyAckDeadlineRequest : TModifyAckDeadlineRequest) : TEmpty;

Const
  _HTTPMethod = 'POST';
  _Path       = '{+subscription}:modifyAckDeadline';
  _Methodid   = 'pubsub.projects.subscriptions.modifyAckDeadline';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['subscription',subscription]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aModifyAckDeadlineRequest,TEmpty) as TEmpty;
end;

Function TProjectsSubscriptionsResource.ModifyPushConfig(subscription: string; aModifyPushConfigRequest : TModifyPushConfigRequest) : TEmpty;

Const
  _HTTPMethod = 'POST';
  _Path       = '{+subscription}:modifyPushConfig';
  _Methodid   = 'pubsub.projects.subscriptions.modifyPushConfig';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['subscription',subscription]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aModifyPushConfigRequest,TEmpty) as TEmpty;
end;

Function TProjectsSubscriptionsResource.Pull(subscription: string; aPullRequest : TPullRequest) : TPullResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{+subscription}:pull';
  _Methodid   = 'pubsub.projects.subscriptions.pull';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['subscription',subscription]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aPullRequest,TPullResponse) as TPullResponse;
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
  _Path       = '{+topic}/subscriptions';
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

Function TProjectsTopicsResource.Create(_name: string; aTopic : TTopic) : TTopic;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{+name}';
  _Methodid   = 'pubsub.projects.topics.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTopic,TTopic) as TTopic;
end;

Function TProjectsTopicsResource.Delete(topic: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{+topic}';
  _Methodid   = 'pubsub.projects.topics.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['topic',topic]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;

Function TProjectsTopicsResource.Get(topic: string) : TTopic;

Const
  _HTTPMethod = 'GET';
  _Path       = '{+topic}';
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
  _Path       = '{+project}/topics';
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

Function TProjectsTopicsResource.Publish(topic: string; aPublishRequest : TPublishRequest) : TPublishResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{+topic}:publish';
  _Methodid   = 'pubsub.projects.topics.publish';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['topic',topic]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aPublishRequest,TPublishResponse) as TPublishResponse;
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
  Result:='20150427';
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
  TEmpty.RegisterObject;
  TListSubscriptionsResponse.RegisterObject;
  TListTopicSubscriptionsResponse.RegisterObject;
  TListTopicsResponse.RegisterObject;
  TModifyAckDeadlineRequest.RegisterObject;
  TModifyPushConfigRequest.RegisterObject;
  TPublishRequest.RegisterObject;
  TPublishResponse.RegisterObject;
  TPubsubMessageTypeattributes.RegisterObject;
  TPubsubMessage.RegisterObject;
  TPullRequest.RegisterObject;
  TPullResponse.RegisterObject;
  TPushConfigTypeattributes.RegisterObject;
  TPushConfig.RegisterObject;
  TReceivedMessage.RegisterObject;
  TSubscription.RegisterObject;
  TTopic.RegisterObject;
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
