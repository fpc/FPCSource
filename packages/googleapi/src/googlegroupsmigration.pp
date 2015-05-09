unit googlegroupsmigration;
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
//Generated on: 9-5-15 13:22:55
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TGroups = class;
  TGroupsArray = Array of TGroups;
  //Anonymous types, using auto-generated names
  
  { --------------------------------------------------------------------
    TGroups
    --------------------------------------------------------------------}
  
  TGroups = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FresponseCode : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetresponseCode(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property responseCode : String Index 8 Read FresponseCode Write SetresponseCode;
  end;
  TGroupsClass = Class of TGroups;
  
  { --------------------------------------------------------------------
    TArchiveResource
    --------------------------------------------------------------------}
  
  TArchiveResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Insert(groupId: string) : TGroups;
  end;
  
  
  { --------------------------------------------------------------------
    TGroupsmigrationAPI
    --------------------------------------------------------------------}
  
  TGroupsmigrationAPI = Class(TGoogleAPI)
  Private
    FArchiveInstance : TArchiveResource;
    Function GetArchiveInstance : TArchiveResource;virtual;
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
    Function CreateArchiveResource(AOwner : TComponent) : TArchiveResource;virtual;overload;
    Function CreateArchiveResource : TArchiveResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ArchiveResource : TArchiveResource Read GetArchiveInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TGroups
  --------------------------------------------------------------------}


Procedure TGroups.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetresponseCode(AIndex : Integer; AValue : String); 

begin
  If (FresponseCode=AValue) then exit;
  FresponseCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TArchiveResource
  --------------------------------------------------------------------}


Class Function TArchiveResource.ResourceName : String;

begin
  Result:='archive';
end;

Class Function TArchiveResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgroupsmigrationAPI;
end;

Function TArchiveResource.Insert(groupId: string) : TGroups;

Const
  _HTTPMethod = 'POST';
  _Path       = '{groupId}/archive';
  _Methodid   = 'groupsmigration.archive.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['groupId',groupId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TGroups) as TGroups;
end;



{ --------------------------------------------------------------------
  TGroupsmigrationAPI
  --------------------------------------------------------------------}

Class Function TGroupsmigrationAPI.APIName : String;

begin
  Result:='groupsmigration';
end;

Class Function TGroupsmigrationAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TGroupsmigrationAPI.APIRevision : String;

begin
  Result:='20140416';
end;

Class Function TGroupsmigrationAPI.APIID : String;

begin
  Result:='groupsmigration:v1';
end;

Class Function TGroupsmigrationAPI.APITitle : String;

begin
  Result:='Groups Migration API';
end;

Class Function TGroupsmigrationAPI.APIDescription : String;

begin
  Result:='Groups Migration Api.';
end;

Class Function TGroupsmigrationAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TGroupsmigrationAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TGroupsmigrationAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/discussions-16.gif';
end;

Class Function TGroupsmigrationAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/discussions-32.gif';
end;

Class Function TGroupsmigrationAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/google-apps/groups-migration/';
end;

Class Function TGroupsmigrationAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TGroupsmigrationAPI.APIbasePath : string;

begin
  Result:='/groups/v1/groups/';
end;

Class Function TGroupsmigrationAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/groups/v1/groups/';
end;

Class Function TGroupsmigrationAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TGroupsmigrationAPI.APIservicePath : string;

begin
  Result:='groups/v1/groups/';
end;

Class Function TGroupsmigrationAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TGroupsmigrationAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/apps.groups.migration';
  Result[0].Description:='Manage messages in groups on your domain';
  
end;

Class Function TGroupsmigrationAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TGroupsmigrationAPI.RegisterAPIResources;

begin
  TGroups.RegisterObject;
end;


Function TGroupsmigrationAPI.GetArchiveInstance : TArchiveResource;

begin
  if (FArchiveInstance=Nil) then
    FArchiveInstance:=CreateArchiveResource;
  Result:=FArchiveInstance;
end;

Function TGroupsmigrationAPI.CreateArchiveResource : TArchiveResource;

begin
  Result:=CreateArchiveResource(Self);
end;


Function TGroupsmigrationAPI.CreateArchiveResource(AOwner : TComponent) : TArchiveResource;

begin
  Result:=TArchiveResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TGroupsmigrationAPI.RegisterAPI;
end.
