unit googlekgsearch;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TSearchResponse = Class;
  TSearchResponseArray = Array of TSearchResponse;
  //Anonymous types, using auto-generated names
  
  { --------------------------------------------------------------------
    TSearchResponse
    --------------------------------------------------------------------}
  
  TSearchResponse = Class(TGoogleBaseObject)
  Private
    Fcontext : TJSONSchema;
    F_type : TJSONSchema;
    FitemListElement : TTJSONSchemaArray;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcontext(AIndex : Integer; const AValue : TJSONSchema); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : TJSONSchema); virtual;
    Procedure SetitemListElement(AIndex : Integer; const AValue : TTJSONSchemaArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property context : TJSONSchema Index 0 Read Fcontext Write Setcontext;
    Property _type : TJSONSchema Index 8 Read F_type Write Set_type;
    Property itemListElement : TTJSONSchemaArray Index 16 Read FitemListElement Write SetitemListElement;
  end;
  TSearchResponseClass = Class of TSearchResponse;
  
  { --------------------------------------------------------------------
    TEntitiesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TEntitiesResource, method Search
  
  TEntitiesSearchOptions = Record
    query : String;
    ids : String;
    languages : String;
    types : String;
    indent : boolean;
    prefix : boolean;
    limit : integer;
  end;
  
  TEntitiesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Search(AQuery : string  = '') : TSearchResponse;
    Function Search(AQuery : TEntitiessearchOptions) : TSearchResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TKgsearchAPI
    --------------------------------------------------------------------}
  
  TKgsearchAPI = Class(TGoogleAPI)
  Private
    FEntitiesInstance : TEntitiesResource;
    Function GetEntitiesInstance : TEntitiesResource;virtual;
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
    Function CreateEntitiesResource(AOwner : TComponent) : TEntitiesResource;virtual;overload;
    Function CreateEntitiesResource : TEntitiesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property EntitiesResource : TEntitiesResource Read GetEntitiesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TSearchResponse
  --------------------------------------------------------------------}


Procedure TSearchResponse.Setcontext(AIndex : Integer; const AValue : TJSONSchema); 

begin
  If (Fcontext=AValue) then exit;
  Fcontext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchResponse.Set_type(AIndex : Integer; const AValue : TJSONSchema); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchResponse.SetitemListElement(AIndex : Integer; const AValue : TTJSONSchemaArray); 

begin
  If (FitemListElement=AValue) then exit;
  FitemListElement:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TSearchResponse.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'itemlistelement' : SetLength(FitemListElement,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEntitiesResource
  --------------------------------------------------------------------}


Class Function TEntitiesResource.ResourceName : String;

begin
  Result:='entities';
end;

Class Function TEntitiesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TkgsearchAPI;
end;

Function TEntitiesResource.Search(AQuery : string = '') : TSearchResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/entities:search';
  _Methodid   = 'kgsearch.entities.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TSearchResponse) as TSearchResponse;
end;


Function TEntitiesResource.Search(AQuery : TEntitiessearchOptions) : TSearchResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'query',AQuery.query);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'languages',AQuery.languages);
  AddToQuery(_Q,'types',AQuery.types);
  AddToQuery(_Q,'indent',AQuery.indent);
  AddToQuery(_Q,'prefix',AQuery.prefix);
  AddToQuery(_Q,'limit',AQuery.limit);
  Result:=Search(_Q);
end;



{ --------------------------------------------------------------------
  TKgsearchAPI
  --------------------------------------------------------------------}

Class Function TKgsearchAPI.APIName : String;

begin
  Result:='kgsearch';
end;

Class Function TKgsearchAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TKgsearchAPI.APIRevision : String;

begin
  Result:='20151215';
end;

Class Function TKgsearchAPI.APIID : String;

begin
  Result:='kgsearch:v1';
end;

Class Function TKgsearchAPI.APITitle : String;

begin
  Result:='Knowledge Graph Search API';
end;

Class Function TKgsearchAPI.APIDescription : String;

begin
  Result:='Knowledge Graph Search API allows developers to search the Google Knowledge Graph for entities.';
end;

Class Function TKgsearchAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TKgsearchAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TKgsearchAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TKgsearchAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TKgsearchAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/knowledge-graph/';
end;

Class Function TKgsearchAPI.APIrootUrl : string;

begin
  Result:='https://kgsearch.googleapis.com/';
end;

Class Function TKgsearchAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TKgsearchAPI.APIbaseURL : String;

begin
  Result:='https://kgsearch.googleapis.com/';
end;

Class Function TKgsearchAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TKgsearchAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TKgsearchAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TKgsearchAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,0);
  
end;

Class Function TKgsearchAPI.APINeedsAuth : Boolean;

begin
  Result:=False;
end;

Class Procedure TKgsearchAPI.RegisterAPIResources;

begin
  TSearchResponse.RegisterObject;
end;


Function TKgsearchAPI.GetEntitiesInstance : TEntitiesResource;

begin
  if (FEntitiesInstance=Nil) then
    FEntitiesInstance:=CreateEntitiesResource;
  Result:=FEntitiesInstance;
end;

Function TKgsearchAPI.CreateEntitiesResource : TEntitiesResource;

begin
  Result:=CreateEntitiesResource(Self);
end;


Function TKgsearchAPI.CreateEntitiesResource(AOwner : TComponent) : TEntitiesResource;

begin
  Result:=TEntitiesResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TKgsearchAPI.RegisterAPI;
end.
