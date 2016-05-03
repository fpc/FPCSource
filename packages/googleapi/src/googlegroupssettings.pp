unit googlegroupssettings;
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
//Generated on: 16-5-15 08:53:05
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TGroups = Class;
  TGroupsArray = Array of TGroups;
  //Anonymous types, using auto-generated names
  
  { --------------------------------------------------------------------
    TGroups
    --------------------------------------------------------------------}
  
  TGroups = Class(TGoogleBaseObject)
  Private
    FallowExternalMembers : String;
    FallowGoogleCommunication : String;
    FallowWebPosting : String;
    FarchiveOnly : String;
    FcustomReplyTo : String;
    FdefaultMessageDenyNotificationText : String;
    Fdescription : String;
    Femail : String;
    FincludeInGlobalAddressList : String;
    FisArchived : String;
    Fkind : String;
    FmaxMessageBytes : integer;
    FmembersCanPostAsTheGroup : String;
    FmessageDisplayFont : String;
    FmessageModerationLevel : String;
    Fname : String;
    FprimaryLanguage : String;
    FreplyTo : String;
    FsendMessageDenyNotification : String;
    FshowInGroupDirectory : String;
    FspamModerationLevel : String;
    FwhoCanContactOwner : String;
    FwhoCanInvite : String;
    FwhoCanJoin : String;
    FwhoCanLeaveGroup : String;
    FwhoCanPostMessage : String;
    FwhoCanViewGroup : String;
    FwhoCanViewMembership : String;
  Protected
    //Property setters
    Procedure SetallowExternalMembers(AIndex : Integer; AValue : String); virtual;
    Procedure SetallowGoogleCommunication(AIndex : Integer; AValue : String); virtual;
    Procedure SetallowWebPosting(AIndex : Integer; AValue : String); virtual;
    Procedure SetarchiveOnly(AIndex : Integer; AValue : String); virtual;
    Procedure SetcustomReplyTo(AIndex : Integer; AValue : String); virtual;
    Procedure SetdefaultMessageDenyNotificationText(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; AValue : String); virtual;
    Procedure SetincludeInGlobalAddressList(AIndex : Integer; AValue : String); virtual;
    Procedure SetisArchived(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmaxMessageBytes(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmembersCanPostAsTheGroup(AIndex : Integer; AValue : String); virtual;
    Procedure SetmessageDisplayFont(AIndex : Integer; AValue : String); virtual;
    Procedure SetmessageModerationLevel(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetprimaryLanguage(AIndex : Integer; AValue : String); virtual;
    Procedure SetreplyTo(AIndex : Integer; AValue : String); virtual;
    Procedure SetsendMessageDenyNotification(AIndex : Integer; AValue : String); virtual;
    Procedure SetshowInGroupDirectory(AIndex : Integer; AValue : String); virtual;
    Procedure SetspamModerationLevel(AIndex : Integer; AValue : String); virtual;
    Procedure SetwhoCanContactOwner(AIndex : Integer; AValue : String); virtual;
    Procedure SetwhoCanInvite(AIndex : Integer; AValue : String); virtual;
    Procedure SetwhoCanJoin(AIndex : Integer; AValue : String); virtual;
    Procedure SetwhoCanLeaveGroup(AIndex : Integer; AValue : String); virtual;
    Procedure SetwhoCanPostMessage(AIndex : Integer; AValue : String); virtual;
    Procedure SetwhoCanViewGroup(AIndex : Integer; AValue : String); virtual;
    Procedure SetwhoCanViewMembership(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property allowExternalMembers : String Index 0 Read FallowExternalMembers Write SetallowExternalMembers;
    Property allowGoogleCommunication : String Index 8 Read FallowGoogleCommunication Write SetallowGoogleCommunication;
    Property allowWebPosting : String Index 16 Read FallowWebPosting Write SetallowWebPosting;
    Property archiveOnly : String Index 24 Read FarchiveOnly Write SetarchiveOnly;
    Property customReplyTo : String Index 32 Read FcustomReplyTo Write SetcustomReplyTo;
    Property defaultMessageDenyNotificationText : String Index 40 Read FdefaultMessageDenyNotificationText Write SetdefaultMessageDenyNotificationText;
    Property description : String Index 48 Read Fdescription Write Setdescription;
    Property email : String Index 56 Read Femail Write Setemail;
    Property includeInGlobalAddressList : String Index 64 Read FincludeInGlobalAddressList Write SetincludeInGlobalAddressList;
    Property isArchived : String Index 72 Read FisArchived Write SetisArchived;
    Property kind : String Index 80 Read Fkind Write Setkind;
    Property maxMessageBytes : integer Index 88 Read FmaxMessageBytes Write SetmaxMessageBytes;
    Property membersCanPostAsTheGroup : String Index 96 Read FmembersCanPostAsTheGroup Write SetmembersCanPostAsTheGroup;
    Property messageDisplayFont : String Index 104 Read FmessageDisplayFont Write SetmessageDisplayFont;
    Property messageModerationLevel : String Index 112 Read FmessageModerationLevel Write SetmessageModerationLevel;
    Property name : String Index 120 Read Fname Write Setname;
    Property primaryLanguage : String Index 128 Read FprimaryLanguage Write SetprimaryLanguage;
    Property replyTo : String Index 136 Read FreplyTo Write SetreplyTo;
    Property sendMessageDenyNotification : String Index 144 Read FsendMessageDenyNotification Write SetsendMessageDenyNotification;
    Property showInGroupDirectory : String Index 152 Read FshowInGroupDirectory Write SetshowInGroupDirectory;
    Property spamModerationLevel : String Index 160 Read FspamModerationLevel Write SetspamModerationLevel;
    Property whoCanContactOwner : String Index 168 Read FwhoCanContactOwner Write SetwhoCanContactOwner;
    Property whoCanInvite : String Index 176 Read FwhoCanInvite Write SetwhoCanInvite;
    Property whoCanJoin : String Index 184 Read FwhoCanJoin Write SetwhoCanJoin;
    Property whoCanLeaveGroup : String Index 192 Read FwhoCanLeaveGroup Write SetwhoCanLeaveGroup;
    Property whoCanPostMessage : String Index 200 Read FwhoCanPostMessage Write SetwhoCanPostMessage;
    Property whoCanViewGroup : String Index 208 Read FwhoCanViewGroup Write SetwhoCanViewGroup;
    Property whoCanViewMembership : String Index 216 Read FwhoCanViewMembership Write SetwhoCanViewMembership;
  end;
  TGroupsClass = Class of TGroups;
  
  { --------------------------------------------------------------------
    TGroupsResource
    --------------------------------------------------------------------}
  
  TGroupsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(groupUniqueId: string) : TGroups;
    Function Patch(groupUniqueId: string; aGroups : TGroups) : TGroups;
    Function Update(groupUniqueId: string; aGroups : TGroups) : TGroups;
  end;
  
  
  { --------------------------------------------------------------------
    TGroupssettingsAPI
    --------------------------------------------------------------------}
  
  TGroupssettingsAPI = Class(TGoogleAPI)
  Private
    FGroupsInstance : TGroupsResource;
    Function GetGroupsInstance : TGroupsResource;virtual;
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
    Function CreateGroupsResource(AOwner : TComponent) : TGroupsResource;virtual;overload;
    Function CreateGroupsResource : TGroupsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property GroupsResource : TGroupsResource Read GetGroupsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TGroups
  --------------------------------------------------------------------}


Procedure TGroups.SetallowExternalMembers(AIndex : Integer; AValue : String); 

begin
  If (FallowExternalMembers=AValue) then exit;
  FallowExternalMembers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetallowGoogleCommunication(AIndex : Integer; AValue : String); 

begin
  If (FallowGoogleCommunication=AValue) then exit;
  FallowGoogleCommunication:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetallowWebPosting(AIndex : Integer; AValue : String); 

begin
  If (FallowWebPosting=AValue) then exit;
  FallowWebPosting:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetarchiveOnly(AIndex : Integer; AValue : String); 

begin
  If (FarchiveOnly=AValue) then exit;
  FarchiveOnly:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetcustomReplyTo(AIndex : Integer; AValue : String); 

begin
  If (FcustomReplyTo=AValue) then exit;
  FcustomReplyTo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetdefaultMessageDenyNotificationText(AIndex : Integer; AValue : String); 

begin
  If (FdefaultMessageDenyNotificationText=AValue) then exit;
  FdefaultMessageDenyNotificationText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.Setemail(AIndex : Integer; AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetincludeInGlobalAddressList(AIndex : Integer; AValue : String); 

begin
  If (FincludeInGlobalAddressList=AValue) then exit;
  FincludeInGlobalAddressList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetisArchived(AIndex : Integer; AValue : String); 

begin
  If (FisArchived=AValue) then exit;
  FisArchived:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetmaxMessageBytes(AIndex : Integer; AValue : integer); 

begin
  If (FmaxMessageBytes=AValue) then exit;
  FmaxMessageBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetmembersCanPostAsTheGroup(AIndex : Integer; AValue : String); 

begin
  If (FmembersCanPostAsTheGroup=AValue) then exit;
  FmembersCanPostAsTheGroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetmessageDisplayFont(AIndex : Integer; AValue : String); 

begin
  If (FmessageDisplayFont=AValue) then exit;
  FmessageDisplayFont:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetmessageModerationLevel(AIndex : Integer; AValue : String); 

begin
  If (FmessageModerationLevel=AValue) then exit;
  FmessageModerationLevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetprimaryLanguage(AIndex : Integer; AValue : String); 

begin
  If (FprimaryLanguage=AValue) then exit;
  FprimaryLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetreplyTo(AIndex : Integer; AValue : String); 

begin
  If (FreplyTo=AValue) then exit;
  FreplyTo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetsendMessageDenyNotification(AIndex : Integer; AValue : String); 

begin
  If (FsendMessageDenyNotification=AValue) then exit;
  FsendMessageDenyNotification:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetshowInGroupDirectory(AIndex : Integer; AValue : String); 

begin
  If (FshowInGroupDirectory=AValue) then exit;
  FshowInGroupDirectory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetspamModerationLevel(AIndex : Integer; AValue : String); 

begin
  If (FspamModerationLevel=AValue) then exit;
  FspamModerationLevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetwhoCanContactOwner(AIndex : Integer; AValue : String); 

begin
  If (FwhoCanContactOwner=AValue) then exit;
  FwhoCanContactOwner:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetwhoCanInvite(AIndex : Integer; AValue : String); 

begin
  If (FwhoCanInvite=AValue) then exit;
  FwhoCanInvite:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetwhoCanJoin(AIndex : Integer; AValue : String); 

begin
  If (FwhoCanJoin=AValue) then exit;
  FwhoCanJoin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetwhoCanLeaveGroup(AIndex : Integer; AValue : String); 

begin
  If (FwhoCanLeaveGroup=AValue) then exit;
  FwhoCanLeaveGroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetwhoCanPostMessage(AIndex : Integer; AValue : String); 

begin
  If (FwhoCanPostMessage=AValue) then exit;
  FwhoCanPostMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetwhoCanViewGroup(AIndex : Integer; AValue : String); 

begin
  If (FwhoCanViewGroup=AValue) then exit;
  FwhoCanViewGroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetwhoCanViewMembership(AIndex : Integer; AValue : String); 

begin
  If (FwhoCanViewMembership=AValue) then exit;
  FwhoCanViewMembership:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupsResource
  --------------------------------------------------------------------}


Class Function TGroupsResource.ResourceName : String;

begin
  Result:='groups';
end;

Class Function TGroupsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgroupssettingsAPI;
end;

Function TGroupsResource.Get(groupUniqueId: string) : TGroups;

Const
  _HTTPMethod = 'GET';
  _Path       = '{groupUniqueId}';
  _Methodid   = 'groupsSettings.groups.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['groupUniqueId',groupUniqueId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TGroups) as TGroups;
end;

Function TGroupsResource.Patch(groupUniqueId: string; aGroups : TGroups) : TGroups;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{groupUniqueId}';
  _Methodid   = 'groupsSettings.groups.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['groupUniqueId',groupUniqueId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aGroups,TGroups) as TGroups;
end;

Function TGroupsResource.Update(groupUniqueId: string; aGroups : TGroups) : TGroups;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{groupUniqueId}';
  _Methodid   = 'groupsSettings.groups.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['groupUniqueId',groupUniqueId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aGroups,TGroups) as TGroups;
end;



{ --------------------------------------------------------------------
  TGroupssettingsAPI
  --------------------------------------------------------------------}

Class Function TGroupssettingsAPI.APIName : String;

begin
  Result:='groupssettings';
end;

Class Function TGroupssettingsAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TGroupssettingsAPI.APIRevision : String;

begin
  Result:='20140428';
end;

Class Function TGroupssettingsAPI.APIID : String;

begin
  Result:='groupssettings:v1';
end;

Class Function TGroupssettingsAPI.APITitle : String;

begin
  Result:='Groups Settings API';
end;

Class Function TGroupssettingsAPI.APIDescription : String;

begin
  Result:='Lets you manage permission levels and related settings of a group.';
end;

Class Function TGroupssettingsAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TGroupssettingsAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TGroupssettingsAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TGroupssettingsAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TGroupssettingsAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/google-apps/groups-settings/get_started';
end;

Class Function TGroupssettingsAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com:443/';
end;

Class Function TGroupssettingsAPI.APIbasePath : string;

begin
  Result:='/groups/v1/groups/';
end;

Class Function TGroupssettingsAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/groups/v1/groups/';
end;

Class Function TGroupssettingsAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TGroupssettingsAPI.APIservicePath : string;

begin
  Result:='groups/v1/groups/';
end;

Class Function TGroupssettingsAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TGroupssettingsAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/apps.groups.settings';
  Result[0].Description:='View and manage the settings of a Google Apps Group';
  
end;

Class Function TGroupssettingsAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TGroupssettingsAPI.RegisterAPIResources;

begin
  TGroups.RegisterObject;
end;


Function TGroupssettingsAPI.GetGroupsInstance : TGroupsResource;

begin
  if (FGroupsInstance=Nil) then
    FGroupsInstance:=CreateGroupsResource;
  Result:=FGroupsInstance;
end;

Function TGroupssettingsAPI.CreateGroupsResource : TGroupsResource;

begin
  Result:=CreateGroupsResource(Self);
end;


Function TGroupssettingsAPI.CreateGroupsResource(AOwner : TComponent) : TGroupsResource;

begin
  Result:=TGroupsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TGroupssettingsAPI.RegisterAPI;
end.
