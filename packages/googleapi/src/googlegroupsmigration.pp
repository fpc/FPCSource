unit googlegroupsmigration;
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
    Fkind : string;
    FresponseCode : string;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetresponseCode(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property responseCode : string Index 8 Read FresponseCode Write SetresponseCode;
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


Procedure TGroups.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroups.SetresponseCode(AIndex : Integer; AValue : string); 

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
