unit googleappsactivity;
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
  TActivity = class;
  TActivityArray = Array of TActivity;
  TActivitysingleEvents = class;
  TActivitysingleEventsArray = Array of TActivitysingleEvents;
  TEvent = class;
  TEventArray = Array of TEvent;
  TEventadditionalEventTypes = class;
  TEventadditionalEventTypesArray = Array of TEventadditionalEventTypes;
  TEventpermissionChanges = class;
  TEventpermissionChangesArray = Array of TEventpermissionChanges;
  TListActivitiesResponse = class;
  TListActivitiesResponseArray = Array of TListActivitiesResponse;
  TListActivitiesResponseactivities = class;
  TListActivitiesResponseactivitiesArray = Array of TListActivitiesResponseactivities;
  TMove = class;
  TMoveArray = Array of TMove;
  TMoveaddedParents = class;
  TMoveaddedParentsArray = Array of TMoveaddedParents;
  TMoveremovedParents = class;
  TMoveremovedParentsArray = Array of TMoveremovedParents;
  TParent = class;
  TParentArray = Array of TParent;
  TPermission = class;
  TPermissionArray = Array of TPermission;
  TPermissionChange = class;
  TPermissionChangeArray = Array of TPermissionChange;
  TPermissionChangeaddedPermissions = class;
  TPermissionChangeaddedPermissionsArray = Array of TPermissionChangeaddedPermissions;
  TPermissionChangeremovedPermissions = class;
  TPermissionChangeremovedPermissionsArray = Array of TPermissionChangeremovedPermissions;
  TPhoto = class;
  TPhotoArray = Array of TPhoto;
  TRename = class;
  TRenameArray = Array of TRename;
  TTarget = class;
  TTargetArray = Array of TTarget;
  TUser = class;
  TUserArray = Array of TUser;
  
  { --------------------------------------------------------------------
    TActivity
    --------------------------------------------------------------------}
  
  TActivity = Class(TGoogleBaseObject)
  Private
    FcombinedEvent : TEvent;
    FsingleEvents : TActivitysingleEvents;
  Protected
    //Property setters
    Procedure SetcombinedEvent(AIndex : Integer; AValue : TEvent); virtual;
    Procedure SetsingleEvents(AIndex : Integer; AValue : TActivitysingleEvents); virtual;
  Public
  Published
    Property combinedEvent : TEvent Index 0 Read FcombinedEvent Write SetcombinedEvent;
    Property singleEvents : TActivitysingleEvents Index 8 Read FsingleEvents Write SetsingleEvents;
  end;
  TActivityClass = Class of TActivity;
  
  { --------------------------------------------------------------------
    TActivitysingleEvents
    --------------------------------------------------------------------}
  
  TActivitysingleEvents = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TActivitysingleEventsClass = Class of TActivitysingleEvents;
  
  { --------------------------------------------------------------------
    TEvent
    --------------------------------------------------------------------}
  
  TEvent = Class(TGoogleBaseObject)
  Private
    FadditionalEventTypes : TEventadditionalEventTypes;
    FeventTimeMillis : string;
    FfromUserDeletion : boolean;
    Fmove : TMove;
    FpermissionChanges : TEventpermissionChanges;
    FprimaryEventType : string;
    Frename : TRename;
    Ftarget : TTarget;
    Fuser : TUser;
  Protected
    //Property setters
    Procedure SetadditionalEventTypes(AIndex : Integer; AValue : TEventadditionalEventTypes); virtual;
    Procedure SeteventTimeMillis(AIndex : Integer; AValue : string); virtual;
    Procedure SetfromUserDeletion(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setmove(AIndex : Integer; AValue : TMove); virtual;
    Procedure SetpermissionChanges(AIndex : Integer; AValue : TEventpermissionChanges); virtual;
    Procedure SetprimaryEventType(AIndex : Integer; AValue : string); virtual;
    Procedure Setrename(AIndex : Integer; AValue : TRename); virtual;
    Procedure Settarget(AIndex : Integer; AValue : TTarget); virtual;
    Procedure Setuser(AIndex : Integer; AValue : TUser); virtual;
  Public
  Published
    Property additionalEventTypes : TEventadditionalEventTypes Index 0 Read FadditionalEventTypes Write SetadditionalEventTypes;
    Property eventTimeMillis : string Index 8 Read FeventTimeMillis Write SeteventTimeMillis;
    Property fromUserDeletion : boolean Index 16 Read FfromUserDeletion Write SetfromUserDeletion;
    Property move : TMove Index 24 Read Fmove Write Setmove;
    Property permissionChanges : TEventpermissionChanges Index 32 Read FpermissionChanges Write SetpermissionChanges;
    Property primaryEventType : string Index 40 Read FprimaryEventType Write SetprimaryEventType;
    Property rename : TRename Index 48 Read Frename Write Setrename;
    Property target : TTarget Index 56 Read Ftarget Write Settarget;
    Property user : TUser Index 64 Read Fuser Write Setuser;
  end;
  TEventClass = Class of TEvent;
  
  { --------------------------------------------------------------------
    TEventadditionalEventTypes
    --------------------------------------------------------------------}
  
  TEventadditionalEventTypes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEventadditionalEventTypesClass = Class of TEventadditionalEventTypes;
  
  { --------------------------------------------------------------------
    TEventpermissionChanges
    --------------------------------------------------------------------}
  
  TEventpermissionChanges = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEventpermissionChangesClass = Class of TEventpermissionChanges;
  
  { --------------------------------------------------------------------
    TListActivitiesResponse
    --------------------------------------------------------------------}
  
  TListActivitiesResponse = Class(TGoogleBaseObject)
  Private
    Factivities : TListActivitiesResponseactivities;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setactivities(AIndex : Integer; AValue : TListActivitiesResponseactivities); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property activities : TListActivitiesResponseactivities Index 0 Read Factivities Write Setactivities;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListActivitiesResponseClass = Class of TListActivitiesResponse;
  
  { --------------------------------------------------------------------
    TListActivitiesResponseactivities
    --------------------------------------------------------------------}
  
  TListActivitiesResponseactivities = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListActivitiesResponseactivitiesClass = Class of TListActivitiesResponseactivities;
  
  { --------------------------------------------------------------------
    TMove
    --------------------------------------------------------------------}
  
  TMove = Class(TGoogleBaseObject)
  Private
    FaddedParents : TMoveaddedParents;
    FremovedParents : TMoveremovedParents;
  Protected
    //Property setters
    Procedure SetaddedParents(AIndex : Integer; AValue : TMoveaddedParents); virtual;
    Procedure SetremovedParents(AIndex : Integer; AValue : TMoveremovedParents); virtual;
  Public
  Published
    Property addedParents : TMoveaddedParents Index 0 Read FaddedParents Write SetaddedParents;
    Property removedParents : TMoveremovedParents Index 8 Read FremovedParents Write SetremovedParents;
  end;
  TMoveClass = Class of TMove;
  
  { --------------------------------------------------------------------
    TMoveaddedParents
    --------------------------------------------------------------------}
  
  TMoveaddedParents = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMoveaddedParentsClass = Class of TMoveaddedParents;
  
  { --------------------------------------------------------------------
    TMoveremovedParents
    --------------------------------------------------------------------}
  
  TMoveremovedParents = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMoveremovedParentsClass = Class of TMoveremovedParents;
  
  { --------------------------------------------------------------------
    TParent
    --------------------------------------------------------------------}
  
  TParent = Class(TGoogleBaseObject)
  Private
    Fid : string;
    FisRoot : boolean;
    Ftitle : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetisRoot(AIndex : Integer; AValue : boolean); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property isRoot : boolean Index 8 Read FisRoot Write SetisRoot;
    Property title : string Index 16 Read Ftitle Write Settitle;
  end;
  TParentClass = Class of TParent;
  
  { --------------------------------------------------------------------
    TPermission
    --------------------------------------------------------------------}
  
  TPermission = Class(TGoogleBaseObject)
  Private
    Fname : string;
    FpermissionId : string;
    Frole : string;
    F_type : string;
    Fuser : TUser;
    FwithLink : boolean;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetpermissionId(AIndex : Integer; AValue : string); virtual;
    Procedure Setrole(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setuser(AIndex : Integer; AValue : TUser); virtual;
    Procedure SetwithLink(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
    Property permissionId : string Index 8 Read FpermissionId Write SetpermissionId;
    Property role : string Index 16 Read Frole Write Setrole;
    Property _type : string Index 24 Read F_type Write Set_type;
    Property user : TUser Index 32 Read Fuser Write Setuser;
    Property withLink : boolean Index 40 Read FwithLink Write SetwithLink;
  end;
  TPermissionClass = Class of TPermission;
  
  { --------------------------------------------------------------------
    TPermissionChange
    --------------------------------------------------------------------}
  
  TPermissionChange = Class(TGoogleBaseObject)
  Private
    FaddedPermissions : TPermissionChangeaddedPermissions;
    FremovedPermissions : TPermissionChangeremovedPermissions;
  Protected
    //Property setters
    Procedure SetaddedPermissions(AIndex : Integer; AValue : TPermissionChangeaddedPermissions); virtual;
    Procedure SetremovedPermissions(AIndex : Integer; AValue : TPermissionChangeremovedPermissions); virtual;
  Public
  Published
    Property addedPermissions : TPermissionChangeaddedPermissions Index 0 Read FaddedPermissions Write SetaddedPermissions;
    Property removedPermissions : TPermissionChangeremovedPermissions Index 8 Read FremovedPermissions Write SetremovedPermissions;
  end;
  TPermissionChangeClass = Class of TPermissionChange;
  
  { --------------------------------------------------------------------
    TPermissionChangeaddedPermissions
    --------------------------------------------------------------------}
  
  TPermissionChangeaddedPermissions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPermissionChangeaddedPermissionsClass = Class of TPermissionChangeaddedPermissions;
  
  { --------------------------------------------------------------------
    TPermissionChangeremovedPermissions
    --------------------------------------------------------------------}
  
  TPermissionChangeremovedPermissions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPermissionChangeremovedPermissionsClass = Class of TPermissionChangeremovedPermissions;
  
  { --------------------------------------------------------------------
    TPhoto
    --------------------------------------------------------------------}
  
  TPhoto = Class(TGoogleBaseObject)
  Private
    Furl : string;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property url : string Index 0 Read Furl Write Seturl;
  end;
  TPhotoClass = Class of TPhoto;
  
  { --------------------------------------------------------------------
    TRename
    --------------------------------------------------------------------}
  
  TRename = Class(TGoogleBaseObject)
  Private
    FnewTitle : string;
    FoldTitle : string;
  Protected
    //Property setters
    Procedure SetnewTitle(AIndex : Integer; AValue : string); virtual;
    Procedure SetoldTitle(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property newTitle : string Index 0 Read FnewTitle Write SetnewTitle;
    Property oldTitle : string Index 8 Read FoldTitle Write SetoldTitle;
  end;
  TRenameClass = Class of TRename;
  
  { --------------------------------------------------------------------
    TTarget
    --------------------------------------------------------------------}
  
  TTarget = Class(TGoogleBaseObject)
  Private
    Fid : string;
    FmimeType : string;
    Fname : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetmimeType(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property mimeType : string Index 8 Read FmimeType Write SetmimeType;
    Property name : string Index 16 Read Fname Write Setname;
  end;
  TTargetClass = Class of TTarget;
  
  { --------------------------------------------------------------------
    TUser
    --------------------------------------------------------------------}
  
  TUser = Class(TGoogleBaseObject)
  Private
    Fname : string;
    Fphoto : TPhoto;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setphoto(AIndex : Integer; AValue : TPhoto); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
    Property photo : TPhoto Index 8 Read Fphoto Write Setphoto;
  end;
  TUserClass = Class of TUser;
  
  { --------------------------------------------------------------------
    TActivitiesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TActivitiesResource, method List
  
  TActivitiesListOptions = Record
    driveancestorId : string;
    drivefileId : string;
    groupingStrategy : string;
    pageSize : integer;
    pageToken : string;
    source : string;
    userId : string;
  end;
  
  TActivitiesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TListActivitiesResponse;
    Function List(AQuery : TActivitieslistOptions) : TListActivitiesResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TAppsactivityAPI
    --------------------------------------------------------------------}
  
  TAppsactivityAPI = Class(TGoogleAPI)
  Private
    FActivitiesInstance : TActivitiesResource;
    Function GetActivitiesInstance : TActivitiesResource;virtual;
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
    Function CreateActivitiesResource(AOwner : TComponent) : TActivitiesResource;virtual;overload;
    Function CreateActivitiesResource : TActivitiesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ActivitiesResource : TActivitiesResource Read GetActivitiesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TActivity
  --------------------------------------------------------------------}


Procedure TActivity.SetcombinedEvent(AIndex : Integer; AValue : TEvent); 

begin
  If (FcombinedEvent=AValue) then exit;
  FcombinedEvent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.SetsingleEvents(AIndex : Integer; AValue : TActivitysingleEvents); 

begin
  If (FsingleEvents=AValue) then exit;
  FsingleEvents:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivitysingleEvents
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TEvent
  --------------------------------------------------------------------}


Procedure TEvent.SetadditionalEventTypes(AIndex : Integer; AValue : TEventadditionalEventTypes); 

begin
  If (FadditionalEventTypes=AValue) then exit;
  FadditionalEventTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SeteventTimeMillis(AIndex : Integer; AValue : string); 

begin
  If (FeventTimeMillis=AValue) then exit;
  FeventTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetfromUserDeletion(AIndex : Integer; AValue : boolean); 

begin
  If (FfromUserDeletion=AValue) then exit;
  FfromUserDeletion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setmove(AIndex : Integer; AValue : TMove); 

begin
  If (Fmove=AValue) then exit;
  Fmove:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetpermissionChanges(AIndex : Integer; AValue : TEventpermissionChanges); 

begin
  If (FpermissionChanges=AValue) then exit;
  FpermissionChanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetprimaryEventType(AIndex : Integer; AValue : string); 

begin
  If (FprimaryEventType=AValue) then exit;
  FprimaryEventType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setrename(AIndex : Integer; AValue : TRename); 

begin
  If (Frename=AValue) then exit;
  Frename:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Settarget(AIndex : Integer; AValue : TTarget); 

begin
  If (Ftarget=AValue) then exit;
  Ftarget:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setuser(AIndex : Integer; AValue : TUser); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEventadditionalEventTypes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TEventpermissionChanges
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListActivitiesResponse
  --------------------------------------------------------------------}


Procedure TListActivitiesResponse.Setactivities(AIndex : Integer; AValue : TListActivitiesResponseactivities); 

begin
  If (Factivities=AValue) then exit;
  Factivities:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListActivitiesResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListActivitiesResponseactivities
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMove
  --------------------------------------------------------------------}


Procedure TMove.SetaddedParents(AIndex : Integer; AValue : TMoveaddedParents); 

begin
  If (FaddedParents=AValue) then exit;
  FaddedParents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMove.SetremovedParents(AIndex : Integer; AValue : TMoveremovedParents); 

begin
  If (FremovedParents=AValue) then exit;
  FremovedParents:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMoveaddedParents
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMoveremovedParents
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TParent
  --------------------------------------------------------------------}


Procedure TParent.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParent.SetisRoot(AIndex : Integer; AValue : boolean); 

begin
  If (FisRoot=AValue) then exit;
  FisRoot:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParent.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPermission
  --------------------------------------------------------------------}


Procedure TPermission.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.SetpermissionId(AIndex : Integer; AValue : string); 

begin
  If (FpermissionId=AValue) then exit;
  FpermissionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Setrole(AIndex : Integer; AValue : string); 

begin
  If (Frole=AValue) then exit;
  Frole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Setuser(AIndex : Integer; AValue : TUser); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.SetwithLink(AIndex : Integer; AValue : boolean); 

begin
  If (FwithLink=AValue) then exit;
  FwithLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPermission.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPermissionChange
  --------------------------------------------------------------------}


Procedure TPermissionChange.SetaddedPermissions(AIndex : Integer; AValue : TPermissionChangeaddedPermissions); 

begin
  If (FaddedPermissions=AValue) then exit;
  FaddedPermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermissionChange.SetremovedPermissions(AIndex : Integer; AValue : TPermissionChangeremovedPermissions); 

begin
  If (FremovedPermissions=AValue) then exit;
  FremovedPermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPermissionChangeaddedPermissions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPermissionChangeremovedPermissions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPhoto
  --------------------------------------------------------------------}


Procedure TPhoto.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRename
  --------------------------------------------------------------------}


Procedure TRename.SetnewTitle(AIndex : Integer; AValue : string); 

begin
  If (FnewTitle=AValue) then exit;
  FnewTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRename.SetoldTitle(AIndex : Integer; AValue : string); 

begin
  If (FoldTitle=AValue) then exit;
  FoldTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTarget
  --------------------------------------------------------------------}


Procedure TTarget.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTarget.SetmimeType(AIndex : Integer; AValue : string); 

begin
  If (FmimeType=AValue) then exit;
  FmimeType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTarget.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUser
  --------------------------------------------------------------------}


Procedure TUser.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setphoto(AIndex : Integer; AValue : TPhoto); 

begin
  If (Fphoto=AValue) then exit;
  Fphoto:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivitiesResource
  --------------------------------------------------------------------}


Class Function TActivitiesResource.ResourceName : String;

begin
  Result:='activities';
end;

Class Function TActivitiesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TappsactivityAPI;
end;

Function TActivitiesResource.List(AQuery : string = '') : TListActivitiesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'activities';
  _Methodid   = 'appsactivity.activities.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TListActivitiesResponse) as TListActivitiesResponse;
end;


Function TActivitiesResource.List(AQuery : TActivitieslistOptions) : TListActivitiesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'drive.ancestorId',AQuery.driveancestorId);
  AddToQuery(_Q,'drive.fileId',AQuery.drivefileId);
  AddToQuery(_Q,'groupingStrategy',AQuery.groupingStrategy);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'source',AQuery.source);
  AddToQuery(_Q,'userId',AQuery.userId);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TAppsactivityAPI
  --------------------------------------------------------------------}

Class Function TAppsactivityAPI.APIName : String;

begin
  Result:='appsactivity';
end;

Class Function TAppsactivityAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TAppsactivityAPI.APIRevision : String;

begin
  Result:='20140828';
end;

Class Function TAppsactivityAPI.APIID : String;

begin
  Result:='appsactivity:v1';
end;

Class Function TAppsactivityAPI.APITitle : String;

begin
  Result:='Google Apps Activity API';
end;

Class Function TAppsactivityAPI.APIDescription : String;

begin
  Result:='Provides a historical view of activity.';
end;

Class Function TAppsactivityAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TAppsactivityAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TAppsactivityAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TAppsactivityAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TAppsactivityAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/google-apps/activity/';
end;

Class Function TAppsactivityAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TAppsactivityAPI.APIbasePath : string;

begin
  Result:='/appsactivity/v1/';
end;

Class Function TAppsactivityAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/appsactivity/v1/';
end;

Class Function TAppsactivityAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TAppsactivityAPI.APIservicePath : string;

begin
  Result:='appsactivity/v1/';
end;

Class Function TAppsactivityAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TAppsactivityAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,4);
  Result[0].Name:='https://www.googleapis.com/auth/activity';
  Result[0].Description:='View the activity history of your Google Apps';
  Result[1].Name:='https://www.googleapis.com/auth/drive';
  Result[1].Description:='View and manage the files in your Google Drive';
  Result[2].Name:='https://www.googleapis.com/auth/drive.metadata.readonly';
  Result[2].Description:='View metadata for files in your Google Drive';
  Result[3].Name:='https://www.googleapis.com/auth/drive.readonly';
  Result[3].Description:='View the files in your Google Drive';
  
end;

Class Function TAppsactivityAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TAppsactivityAPI.RegisterAPIResources;

begin
  TActivity.RegisterObject;
  TActivitysingleEvents.RegisterObject;
  TEvent.RegisterObject;
  TEventadditionalEventTypes.RegisterObject;
  TEventpermissionChanges.RegisterObject;
  TListActivitiesResponse.RegisterObject;
  TListActivitiesResponseactivities.RegisterObject;
  TMove.RegisterObject;
  TMoveaddedParents.RegisterObject;
  TMoveremovedParents.RegisterObject;
  TParent.RegisterObject;
  TPermission.RegisterObject;
  TPermissionChange.RegisterObject;
  TPermissionChangeaddedPermissions.RegisterObject;
  TPermissionChangeremovedPermissions.RegisterObject;
  TPhoto.RegisterObject;
  TRename.RegisterObject;
  TTarget.RegisterObject;
  TUser.RegisterObject;
end;


Function TAppsactivityAPI.GetActivitiesInstance : TActivitiesResource;

begin
  if (FActivitiesInstance=Nil) then
    FActivitiesInstance:=CreateActivitiesResource;
  Result:=FActivitiesInstance;
end;

Function TAppsactivityAPI.CreateActivitiesResource : TActivitiesResource;

begin
  Result:=CreateActivitiesResource(Self);
end;


Function TAppsactivityAPI.CreateActivitiesResource(AOwner : TComponent) : TActivitiesResource;

begin
  Result:=TActivitiesResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TAppsactivityAPI.RegisterAPI;
end.
