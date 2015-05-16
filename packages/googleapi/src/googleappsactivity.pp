unit googleappsactivity;
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
//Generated on: 16-5-15 08:52:58
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TActivity = Class;
  TEvent = Class;
  TListActivitiesResponse = Class;
  TMove = Class;
  TParent = Class;
  TPermission = Class;
  TPermissionChange = Class;
  TPhoto = Class;
  TRename = Class;
  TTarget = Class;
  TUser = Class;
  TActivityArray = Array of TActivity;
  TEventArray = Array of TEvent;
  TListActivitiesResponseArray = Array of TListActivitiesResponse;
  TMoveArray = Array of TMove;
  TParentArray = Array of TParent;
  TPermissionArray = Array of TPermission;
  TPermissionChangeArray = Array of TPermissionChange;
  TPhotoArray = Array of TPhoto;
  TRenameArray = Array of TRename;
  TTargetArray = Array of TTarget;
  TUserArray = Array of TUser;
  //Anonymous types, using auto-generated names
  TActivityTypesingleEventsArray = Array of TEvent;
  TEventTypepermissionChangesArray = Array of TPermissionChange;
  TListActivitiesResponseTypeactivitiesArray = Array of TActivity;
  TMoveTypeaddedParentsArray = Array of TParent;
  TMoveTyperemovedParentsArray = Array of TParent;
  TPermissionChangeTypeaddedPermissionsArray = Array of TPermission;
  TPermissionChangeTyperemovedPermissionsArray = Array of TPermission;
  
  { --------------------------------------------------------------------
    TActivity
    --------------------------------------------------------------------}
  
  TActivity = Class(TGoogleBaseObject)
  Private
    FcombinedEvent : TEvent;
    FsingleEvents : TActivityTypesingleEventsArray;
  Protected
    //Property setters
    Procedure SetcombinedEvent(AIndex : Integer; AValue : TEvent); virtual;
    Procedure SetsingleEvents(AIndex : Integer; AValue : TActivityTypesingleEventsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property combinedEvent : TEvent Index 0 Read FcombinedEvent Write SetcombinedEvent;
    Property singleEvents : TActivityTypesingleEventsArray Index 8 Read FsingleEvents Write SetsingleEvents;
  end;
  TActivityClass = Class of TActivity;
  
  { --------------------------------------------------------------------
    TEvent
    --------------------------------------------------------------------}
  
  TEvent = Class(TGoogleBaseObject)
  Private
    FadditionalEventTypes : TStringArray;
    FeventTimeMillis : String;
    FfromUserDeletion : boolean;
    Fmove : TMove;
    FpermissionChanges : TEventTypepermissionChangesArray;
    FprimaryEventType : String;
    Frename : TRename;
    Ftarget : TTarget;
    Fuser : TUser;
  Protected
    //Property setters
    Procedure SetadditionalEventTypes(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SeteventTimeMillis(AIndex : Integer; AValue : String); virtual;
    Procedure SetfromUserDeletion(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setmove(AIndex : Integer; AValue : TMove); virtual;
    Procedure SetpermissionChanges(AIndex : Integer; AValue : TEventTypepermissionChangesArray); virtual;
    Procedure SetprimaryEventType(AIndex : Integer; AValue : String); virtual;
    Procedure Setrename(AIndex : Integer; AValue : TRename); virtual;
    Procedure Settarget(AIndex : Integer; AValue : TTarget); virtual;
    Procedure Setuser(AIndex : Integer; AValue : TUser); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property additionalEventTypes : TStringArray Index 0 Read FadditionalEventTypes Write SetadditionalEventTypes;
    Property eventTimeMillis : String Index 8 Read FeventTimeMillis Write SeteventTimeMillis;
    Property fromUserDeletion : boolean Index 16 Read FfromUserDeletion Write SetfromUserDeletion;
    Property move : TMove Index 24 Read Fmove Write Setmove;
    Property permissionChanges : TEventTypepermissionChangesArray Index 32 Read FpermissionChanges Write SetpermissionChanges;
    Property primaryEventType : String Index 40 Read FprimaryEventType Write SetprimaryEventType;
    Property rename : TRename Index 48 Read Frename Write Setrename;
    Property target : TTarget Index 56 Read Ftarget Write Settarget;
    Property user : TUser Index 64 Read Fuser Write Setuser;
  end;
  TEventClass = Class of TEvent;
  
  { --------------------------------------------------------------------
    TListActivitiesResponse
    --------------------------------------------------------------------}
  
  TListActivitiesResponse = Class(TGoogleBaseObject)
  Private
    Factivities : TListActivitiesResponseTypeactivitiesArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setactivities(AIndex : Integer; AValue : TListActivitiesResponseTypeactivitiesArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property activities : TListActivitiesResponseTypeactivitiesArray Index 0 Read Factivities Write Setactivities;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListActivitiesResponseClass = Class of TListActivitiesResponse;
  
  { --------------------------------------------------------------------
    TMove
    --------------------------------------------------------------------}
  
  TMove = Class(TGoogleBaseObject)
  Private
    FaddedParents : TMoveTypeaddedParentsArray;
    FremovedParents : TMoveTyperemovedParentsArray;
  Protected
    //Property setters
    Procedure SetaddedParents(AIndex : Integer; AValue : TMoveTypeaddedParentsArray); virtual;
    Procedure SetremovedParents(AIndex : Integer; AValue : TMoveTyperemovedParentsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property addedParents : TMoveTypeaddedParentsArray Index 0 Read FaddedParents Write SetaddedParents;
    Property removedParents : TMoveTyperemovedParentsArray Index 8 Read FremovedParents Write SetremovedParents;
  end;
  TMoveClass = Class of TMove;
  
  { --------------------------------------------------------------------
    TParent
    --------------------------------------------------------------------}
  
  TParent = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FisRoot : boolean;
    Ftitle : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetisRoot(AIndex : Integer; AValue : boolean); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property isRoot : boolean Index 8 Read FisRoot Write SetisRoot;
    Property title : String Index 16 Read Ftitle Write Settitle;
  end;
  TParentClass = Class of TParent;
  
  { --------------------------------------------------------------------
    TPermission
    --------------------------------------------------------------------}
  
  TPermission = Class(TGoogleBaseObject)
  Private
    Fname : String;
    FpermissionId : String;
    Frole : String;
    F_type : String;
    Fuser : TUser;
    FwithLink : boolean;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetpermissionId(AIndex : Integer; AValue : String); virtual;
    Procedure Setrole(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setuser(AIndex : Integer; AValue : TUser); virtual;
    Procedure SetwithLink(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property permissionId : String Index 8 Read FpermissionId Write SetpermissionId;
    Property role : String Index 16 Read Frole Write Setrole;
    Property _type : String Index 24 Read F_type Write Set_type;
    Property user : TUser Index 32 Read Fuser Write Setuser;
    Property withLink : boolean Index 40 Read FwithLink Write SetwithLink;
  end;
  TPermissionClass = Class of TPermission;
  
  { --------------------------------------------------------------------
    TPermissionChange
    --------------------------------------------------------------------}
  
  TPermissionChange = Class(TGoogleBaseObject)
  Private
    FaddedPermissions : TPermissionChangeTypeaddedPermissionsArray;
    FremovedPermissions : TPermissionChangeTyperemovedPermissionsArray;
  Protected
    //Property setters
    Procedure SetaddedPermissions(AIndex : Integer; AValue : TPermissionChangeTypeaddedPermissionsArray); virtual;
    Procedure SetremovedPermissions(AIndex : Integer; AValue : TPermissionChangeTyperemovedPermissionsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property addedPermissions : TPermissionChangeTypeaddedPermissionsArray Index 0 Read FaddedPermissions Write SetaddedPermissions;
    Property removedPermissions : TPermissionChangeTyperemovedPermissionsArray Index 8 Read FremovedPermissions Write SetremovedPermissions;
  end;
  TPermissionChangeClass = Class of TPermissionChange;
  
  { --------------------------------------------------------------------
    TPhoto
    --------------------------------------------------------------------}
  
  TPhoto = Class(TGoogleBaseObject)
  Private
    Furl : String;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property url : String Index 0 Read Furl Write Seturl;
  end;
  TPhotoClass = Class of TPhoto;
  
  { --------------------------------------------------------------------
    TRename
    --------------------------------------------------------------------}
  
  TRename = Class(TGoogleBaseObject)
  Private
    FnewTitle : String;
    FoldTitle : String;
  Protected
    //Property setters
    Procedure SetnewTitle(AIndex : Integer; AValue : String); virtual;
    Procedure SetoldTitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property newTitle : String Index 0 Read FnewTitle Write SetnewTitle;
    Property oldTitle : String Index 8 Read FoldTitle Write SetoldTitle;
  end;
  TRenameClass = Class of TRename;
  
  { --------------------------------------------------------------------
    TTarget
    --------------------------------------------------------------------}
  
  TTarget = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FmimeType : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetmimeType(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property mimeType : String Index 8 Read FmimeType Write SetmimeType;
    Property name : String Index 16 Read Fname Write Setname;
  end;
  TTargetClass = Class of TTarget;
  
  { --------------------------------------------------------------------
    TUser
    --------------------------------------------------------------------}
  
  TUser = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fphoto : TPhoto;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setphoto(AIndex : Integer; AValue : TPhoto); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property photo : TPhoto Index 8 Read Fphoto Write Setphoto;
  end;
  TUserClass = Class of TUser;
  
  { --------------------------------------------------------------------
    TActivitiesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TActivitiesResource, method List
  
  TActivitiesListOptions = Record
    driveancestorId : String;
    drivefileId : String;
    groupingStrategy : String;
    pageSize : integer;
    pageToken : String;
    source : String;
    userId : String;
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



Procedure TActivity.SetsingleEvents(AIndex : Integer; AValue : TActivityTypesingleEventsArray); 

begin
  If (FsingleEvents=AValue) then exit;
  FsingleEvents:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TActivity.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'singleevents' : SetLength(FsingleEvents,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEvent
  --------------------------------------------------------------------}


Procedure TEvent.SetadditionalEventTypes(AIndex : Integer; AValue : TStringArray); 

begin
  If (FadditionalEventTypes=AValue) then exit;
  FadditionalEventTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SeteventTimeMillis(AIndex : Integer; AValue : String); 

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



Procedure TEvent.SetpermissionChanges(AIndex : Integer; AValue : TEventTypepermissionChangesArray); 

begin
  If (FpermissionChanges=AValue) then exit;
  FpermissionChanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetprimaryEventType(AIndex : Integer; AValue : String); 

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


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TEvent.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'additionaleventtypes' : SetLength(FadditionalEventTypes,ALength);
  'permissionchanges' : SetLength(FpermissionChanges,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListActivitiesResponse
  --------------------------------------------------------------------}


Procedure TListActivitiesResponse.Setactivities(AIndex : Integer; AValue : TListActivitiesResponseTypeactivitiesArray); 

begin
  If (Factivities=AValue) then exit;
  Factivities:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListActivitiesResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListActivitiesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'activities' : SetLength(Factivities,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMove
  --------------------------------------------------------------------}


Procedure TMove.SetaddedParents(AIndex : Integer; AValue : TMoveTypeaddedParentsArray); 

begin
  If (FaddedParents=AValue) then exit;
  FaddedParents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMove.SetremovedParents(AIndex : Integer; AValue : TMoveTyperemovedParentsArray); 

begin
  If (FremovedParents=AValue) then exit;
  FremovedParents:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TMove.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'addedparents' : SetLength(FaddedParents,ALength);
  'removedparents' : SetLength(FremovedParents,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TParent
  --------------------------------------------------------------------}


Procedure TParent.Setid(AIndex : Integer; AValue : String); 

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



Procedure TParent.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPermission
  --------------------------------------------------------------------}


Procedure TPermission.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.SetpermissionId(AIndex : Integer; AValue : String); 

begin
  If (FpermissionId=AValue) then exit;
  FpermissionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Setrole(AIndex : Integer; AValue : String); 

begin
  If (Frole=AValue) then exit;
  Frole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Set_type(AIndex : Integer; AValue : String); 

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


Procedure TPermissionChange.SetaddedPermissions(AIndex : Integer; AValue : TPermissionChangeTypeaddedPermissionsArray); 

begin
  If (FaddedPermissions=AValue) then exit;
  FaddedPermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermissionChange.SetremovedPermissions(AIndex : Integer; AValue : TPermissionChangeTyperemovedPermissionsArray); 

begin
  If (FremovedPermissions=AValue) then exit;
  FremovedPermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPermissionChange.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'addedpermissions' : SetLength(FaddedPermissions,ALength);
  'removedpermissions' : SetLength(FremovedPermissions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPhoto
  --------------------------------------------------------------------}


Procedure TPhoto.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRename
  --------------------------------------------------------------------}


Procedure TRename.SetnewTitle(AIndex : Integer; AValue : String); 

begin
  If (FnewTitle=AValue) then exit;
  FnewTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRename.SetoldTitle(AIndex : Integer; AValue : String); 

begin
  If (FoldTitle=AValue) then exit;
  FoldTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTarget
  --------------------------------------------------------------------}


Procedure TTarget.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTarget.SetmimeType(AIndex : Integer; AValue : String); 

begin
  If (FmimeType=AValue) then exit;
  FmimeType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTarget.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUser
  --------------------------------------------------------------------}


Procedure TUser.Setname(AIndex : Integer; AValue : String); 

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
  Result:='20150326';
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
  Result:='https://www.googleapis.com:443/';
end;

Class Function TAppsactivityAPI.APIbasePath : string;

begin
  Result:='/appsactivity/v1/';
end;

Class Function TAppsactivityAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/appsactivity/v1/';
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
  SetLength(Result,5);
  Result[0].Name:='https://www.googleapis.com/auth/activity';
  Result[0].Description:='View the activity history of your Google Apps';
  Result[1].Name:='https://www.googleapis.com/auth/drive';
  Result[1].Description:='View and manage the files in your Google Drive';
  Result[2].Name:='https://www.googleapis.com/auth/drive.metadata';
  Result[2].Description:='View and manage metadata of files in your Google Drive';
  Result[3].Name:='https://www.googleapis.com/auth/drive.metadata.readonly';
  Result[3].Description:='View metadata for files in your Google Drive';
  Result[4].Name:='https://www.googleapis.com/auth/drive.readonly';
  Result[4].Description:='View the files in your Google Drive';
  
end;

Class Function TAppsactivityAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TAppsactivityAPI.RegisterAPIResources;

begin
  TActivity.RegisterObject;
  TEvent.RegisterObject;
  TListActivitiesResponse.RegisterObject;
  TMove.RegisterObject;
  TParent.RegisterObject;
  TPermission.RegisterObject;
  TPermissionChange.RegisterObject;
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
  Result.API:=Self.API;
end;



initialization
  TAppsactivityAPI.RegisterAPI;
end.
