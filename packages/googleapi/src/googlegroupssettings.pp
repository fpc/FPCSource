unit googlegroupssettings;
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
  TGroups = class;
  TGroupsArray = Array of TGroups;
  
  { --------------------------------------------------------------------
    TGroups
    --------------------------------------------------------------------}
  
  TGroups = Class(TGoogleBaseObject)
  Private
    FallowExternalMembers : string;
    FallowGoogleCommunication : string;
    FallowWebPosting : string;
    FarchiveOnly : string;
    FcustomReplyTo : string;
    FdefaultMessageDenyNotificationText : string;
    Fdescription : string;
    Femail : string;
    FincludeInGlobalAddressList : string;
    FisArchived : string;
    Fkind : string;
    FmaxMessageBytes : integer;
    FmembersCanPostAsTheGroup : string;
    FmessageDisplayFont : string;
    FmessageModerationLevel : string;
    Fname : string;
    FprimaryLanguage : string;
    FreplyTo : string;
    FsendMessageDenyNotification : string;
    FshowInGroupDirectory : string;
    FspamModerationLevel : string;
    FwhoCanContactOwner : string;
    FwhoCanInvite : string;
    FwhoCanJoin : string;
    FwhoCanLeaveGroup : string;
    FwhoCanPostMessage : string;
    FwhoCanViewGroup : string;
    FwhoCanViewMembership : string;
  Protected
    //Property setters
    Procedure SetallowExternalMembers(AIndex : Integer; AValue : string); virtual;
    Procedure SetallowGoogleCommunication(AIndex : Integer; AValue : string); virtual;
    Procedure SetallowWebPosting(AIndex : Integer; AValue : string); virtual;
    Procedure SetarchiveOnly(AIndex : Integer; AValue : string); virtual;
    Procedure SetcustomReplyTo(AIndex : Integer; AValue : string); virtual;
    Procedure SetdefaultMessageDenyNotificationText(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure SetincludeInGlobalAddressList(AIndex : Integer; AValue : string); virtual;
    Procedure SetisArchived(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmaxMessageBytes(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmembersCanPostAsTheGroup(AIndex : Integer; AValue : string); virtual;
    Procedure SetmessageDisplayFont(AIndex : Integer; AValue : string); virtual;
    Procedure SetmessageModerationLevel(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetprimaryLanguage(AIndex : Integer; AValue : string); virtual;
    Procedure SetreplyTo(AIndex : Integer; AValue : string); virtual;
    Procedure SetsendMessageDenyNotification(AIndex : Integer; AValue : string); virtual;
    Procedure SetshowInGroupDirectory(AIndex : Integer; AValue : string); virtual;
    Procedure SetspamModerationLevel(AIndex : Integer; AValue : string); virtual;
    Procedure SetwhoCanContactOwner(AIndex : Integer; AValue : string); virtual;
    Procedure SetwhoCanInvite(AIndex : Integer; AValue : string); virtual;
    Procedure SetwhoCanJoin(AIndex : Integer; AValue : string); virtual;
    Procedure SetwhoCanLeaveGroup(AIndex : Integer; AValue : string); virtual;
    Procedure SetwhoCanPostMessage(AIndex : Integer; AValue : string); virtual;
    Procedure SetwhoCanViewGroup(AIndex : Integer; AValue : string); virtual;
    Procedure SetwhoCanViewMembership(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property allowExternalMembers : string Index 0 Read FallowExternalMembers Write SetallowExternalMembers;
    Property allowGoogleCommunication : string Index 8 Read FallowGoogleCommunication Write SetallowGoogleCommunication;
    Property allowWebPosting : string Index 16 Read FallowWebPosting Write SetallowWebPosting;
    Property archiveOnly : string Index 24 Read FarchiveOnly Write SetarchiveOnly;
    Property customReplyTo : string Index 32 Read FcustomReplyTo Write SetcustomReplyTo;
    Property defaultMessageDenyNotificationText : string Index 40 Read FdefaultMessageDenyNotificationText Write SetdefaultMessageDenyNotificationText;
    Property description : string Index 48 Read Fdescription Write Setdescription;
    Property email : string Index 56 Read Femail Write Setemail;
    Property includeInGlobalAddressList : string Index 64 Read FincludeInGlobalAddressList Write SetincludeInGlobalAddressList;
    Property isArchived : string Index 72 Read FisArchived Write SetisArchived;
    Property kind : string Index 80 Read Fkind Write Setkind;
    Property maxMessageBytes : integer Index 88 Read FmaxMessageBytes Write SetmaxMessageBytes;
    Property membersCanPostAsTheGroup : string Index 96 Read FmembersCanPostAsTheGroup Write SetmembersCanPostAsTheGroup;
    Property messageDisplayFont : string Index 104 Read FmessageDisplayFont Write SetmessageDisplayFont;
    Property messageModerationLevel : string Index 112 Read FmessageModerationLevel Write SetmessageModerationLevel;
    Property name : string Index 120 Read Fname Write Setname;
    Property primaryLanguage : string Index 128 Read FprimaryLanguage Write SetprimaryLanguage;
    Property replyTo : string Index 136 Read FreplyTo Write SetreplyTo;
    Property sendMessageDenyNotification : string Index 144 Read FsendMessageDenyNotification Write SetsendMessageDenyNotification;
    Property showInGroupDirectory : string Index 152 Read FshowInGroupDirectory Write SetshowInGroupDirectory;
    Property spamModerationLevel : string Index 160 Read FspamModerationLevel Write SetspamModerationLevel;
    Property whoCanContactOwner : string Index 168 Read FwhoCanContactOwner Write SetwhoCanContactOwner;
    Property whoCanInvite : string Index 176 Read FwhoCanInvite Write SetwhoCanInvite;
    Property whoCanJoin : string Index 184 Read FwhoCanJoin Write SetwhoCanJoin;
    Property whoCanLeaveGroup : string Index 192 Read FwhoCanLeaveGroup Write SetwhoCanLeaveGroup;
    Property whoCanPostMessage : string Index 200 Read FwhoCanPostMessage Write SetwhoCanPostMessage;
    Property whoCanViewGroup : string Index 208 Read FwhoCanViewGroup Write SetwhoCanViewGroup;
    Property whoCanViewMembership : string Index 216 Read FwhoCanViewMembership Write SetwhoCanViewMembership;
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


Procedure TGroups.SetallowExternalMembers(AIndex : Integer; AValue : string); 

begin
  If (FallowExternalMembers=AValue) then exit;
  FallowExternalMembers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetallowGoogleCommunication(AIndex : Integer; AValue : string); 

begin
  If (FallowGoogleCommunication=AValue) then exit;
  FallowGoogleCommunication:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetallowWebPosting(AIndex : Integer; AValue : string); 

begin
  If (FallowWebPosting=AValue) then exit;
  FallowWebPosting:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetarchiveOnly(AIndex : Integer; AValue : string); 

begin
  If (FarchiveOnly=AValue) then exit;
  FarchiveOnly:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetcustomReplyTo(AIndex : Integer; AValue : string); 

begin
  If (FcustomReplyTo=AValue) then exit;
  FcustomReplyTo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetdefaultMessageDenyNotificationText(AIndex : Integer; AValue : string); 

begin
  If (FdefaultMessageDenyNotificationText=AValue) then exit;
  FdefaultMessageDenyNotificationText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.Setemail(AIndex : Integer; AValue : string); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetincludeInGlobalAddressList(AIndex : Integer; AValue : string); 

begin
  If (FincludeInGlobalAddressList=AValue) then exit;
  FincludeInGlobalAddressList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetisArchived(AIndex : Integer; AValue : string); 

begin
  If (FisArchived=AValue) then exit;
  FisArchived:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.Setkind(AIndex : Integer; AValue : string); 

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



Procedure TGroups.SetmembersCanPostAsTheGroup(AIndex : Integer; AValue : string); 

begin
  If (FmembersCanPostAsTheGroup=AValue) then exit;
  FmembersCanPostAsTheGroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetmessageDisplayFont(AIndex : Integer; AValue : string); 

begin
  If (FmessageDisplayFont=AValue) then exit;
  FmessageDisplayFont:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetmessageModerationLevel(AIndex : Integer; AValue : string); 

begin
  If (FmessageModerationLevel=AValue) then exit;
  FmessageModerationLevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetprimaryLanguage(AIndex : Integer; AValue : string); 

begin
  If (FprimaryLanguage=AValue) then exit;
  FprimaryLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetreplyTo(AIndex : Integer; AValue : string); 

begin
  If (FreplyTo=AValue) then exit;
  FreplyTo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetsendMessageDenyNotification(AIndex : Integer; AValue : string); 

begin
  If (FsendMessageDenyNotification=AValue) then exit;
  FsendMessageDenyNotification:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetshowInGroupDirectory(AIndex : Integer; AValue : string); 

begin
  If (FshowInGroupDirectory=AValue) then exit;
  FshowInGroupDirectory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetspamModerationLevel(AIndex : Integer; AValue : string); 

begin
  If (FspamModerationLevel=AValue) then exit;
  FspamModerationLevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetwhoCanContactOwner(AIndex : Integer; AValue : string); 

begin
  If (FwhoCanContactOwner=AValue) then exit;
  FwhoCanContactOwner:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetwhoCanInvite(AIndex : Integer; AValue : string); 

begin
  If (FwhoCanInvite=AValue) then exit;
  FwhoCanInvite:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetwhoCanJoin(AIndex : Integer; AValue : string); 

begin
  If (FwhoCanJoin=AValue) then exit;
  FwhoCanJoin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetwhoCanLeaveGroup(AIndex : Integer; AValue : string); 

begin
  If (FwhoCanLeaveGroup=AValue) then exit;
  FwhoCanLeaveGroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetwhoCanPostMessage(AIndex : Integer; AValue : string); 

begin
  If (FwhoCanPostMessage=AValue) then exit;
  FwhoCanPostMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetwhoCanViewGroup(AIndex : Integer; AValue : string); 

begin
  If (FwhoCanViewGroup=AValue) then exit;
  FwhoCanViewGroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetwhoCanViewMembership(AIndex : Integer; AValue : string); 

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
  Result:='https://www.googleapis.com/';
end;

Class Function TGroupssettingsAPI.APIbasePath : string;

begin
  Result:='/groups/v1/groups/';
end;

Class Function TGroupssettingsAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/groups/v1/groups/';
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
  Result.API:=Self;
end;



initialization
  TGroupssettingsAPI.RegisterAPI;
end.
