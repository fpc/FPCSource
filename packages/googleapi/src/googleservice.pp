{ Base Google service API classes

  Copyright (C) 2015 Michael Van Canneyt michael@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit googleservice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, restbase, googlebase, fpwebclient, fpjson, googleclient;

Type
  TGoogleAPI = Class;
  TGoogleAPIClass = Class of TGoogleAPI;
  TGoogleAPIArray = Array of TGoogleAPI;

  TGoogleResource = Class;
  TGoogleResourceClass = Class of TGoogleResource;
  TGoogleResourceArray = Array of TGoogleResource;

  TGoogleAPIFactory = Class;
  TGoogleAPIFactoryClass = Class of TGoogleAPIFactory;

  { TGoogleAPI }
  TScopeInfo = Record
    Name : string;
    Description : string;
  end;
  TScopeInfoArray = Array of TScopeInfo;

  TAPIInfo = Record
    Name : string;
    Version : String;
    Revision : string;
    id : string;
    title : String;
    description : string;
    ownerDomain : String;
    ownerName : String;
    icon16 : String;
    icon32 : String;
    documentationLink : String;
    rootUrl : string;
    basePath : string;
    baseURL : String;
    Protocol : string;
    servicePath : string;
    batchPath : String;
    AuthScopes : TScopeInfoArray;
  end;

  TGoogleAPI = Class(TComponent)
  private
    FGoogleClient: TGoogleClient;
    procedure SetGoogleClient(AValue: TGoogleClient);
  Protected
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  Public
    // All info in one fell swoop
    Class Function APIInfo : TAPIInfo; virtual;
    Class Function APIName : String; virtual;
    Class Function APIVersion : String; virtual; abstract;
    Class Function APIRevision : String; virtual; abstract;
    Class Function APIID : String; virtual; abstract;
    Class Function APITitle : String; virtual; abstract;
    Class Function APIDescription : String; virtual; abstract;
    Class Function APIOwnerDomain : String; virtual; abstract;
    Class Function APIOwnerName : String; virtual; abstract;
    Class Function APIIcon16 : String; virtual; abstract;
    Class Function APIIcon32 : String; virtual; abstract;
    Class Function APIdocumentationLink : String; virtual; abstract;
    Class Function APIrootUrl : string; virtual; abstract;
    Class Function APIbasePath : string;virtual; abstract;
    Class Function APIbaseURL : String;virtual; abstract;
    Class Function APIProtocol : string;virtual; abstract;
    Class Function APIservicePath : string;virtual; abstract;
    Class Function APIbatchPath : String;virtual; abstract;
    Class Function APIAuthScopes : TScopeInfoArray;virtual; abstract;
    Class Function APINeedsAuth : Boolean ;virtual;
    Class Procedure RegisterAPI; virtual;
    Class Procedure RegisterAPIResources; virtual;
    Function ServiceCall(Const AResource : TGoogleResource; AMethod, APath, AQuery : String; AInput : TGoogleBaseObject; AReturnClass : TGoogleBaseObjectClass) : TGoogleBaseObject; virtual;
    Function ServiceCall(Const AResource : TGoogleResource; AMethod, APath, AQuery, AInput : String) : String; virtual;
    Function SubstitutePath(Const AResource,APath : String; Const Args : Array of const) : String;virtual;
    Function CreateResource(AClass : TGoogleResourceClass) : TGoogleResource; virtual;
    Function CreateResource(const Resource : String) : TGoogleResource; virtual;
  Published
    Property GoogleClient : TGoogleClient Read FGoogleClient Write SetGoogleClient;
  end;

  { TGoogleResource }

  TGoogleResource = Class(TComponent)
  private
    FAPI: TGoogleAPI;
    Procedure SetAPI(AAPI : TGoogleAPI);
  Protected
    Procedure CheckAPI;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  Public
    Class Procedure AddToQuery(Var Q : String;  Const AName , AValue : String);virtual;
    Class Procedure AddToQuery(Var Q : String;  Const AName : String; AValue : Int64);virtual;
    Class Procedure AddToQuery(Var Q : String;  Const AName : String; AValue : TDateTime);virtual;
    Class Procedure AddToQuery(Var Q : String;  Const AName : String; AValue : Boolean);virtual;
    Function SubstitutePath(Const APath : String; Const Args : Array of const): String;
    Function ServiceCall(Const AMethod, APath, AQuery: String; AInput: TGoogleBaseObject;  AReturnClass: TGoogleBaseObjectClass): TGoogleBaseObject; virtual;
  PubliC
    Class Function ResourceName : String; virtual;
    Class Function DefaultAPI : TGoogleAPIClass; virtual;
    Property API : TGoogleAPI Read FAPI Write SetAPI;
  end;


  { TGoogleAPIFactory }

  TGoogleAPIFactory = Class(TComponent)
  Private
    FAPIs : TClassList;
    FResources : TClassList;
    function GetA(AIndex : Integer): TGoogleAPIClass;
    function GetACount: Integer;
    function GetR(AIndex : Integer): TGoogleResourceClass;
    function GetRCount: Integer;
  Public
    Class var
       DefaultFactoryClass : TGoogleAPIFactoryClass;
       DefaultFactory : TGoogleAPIFactory;
  Public
    Constructor Create(Aowner : TComponent); override;
    Destructor Destroy; override;
    // Resource methods
    Procedure RegisterResource(Resource : TGoogleResourceClass); virtual;
    Function IndexOfResource(Const Resource : String) : Integer;
    Function FindResourceClass(Const Resource : String) : TGoogleResourceClass; virtual;
    Function GetResourceClass(Const Resource : String) : TGoogleResourceClass;
    Property ResourceClass[AIndex : Integer] : TGoogleResourceClass Read GetR;
    Property ResourceCount : Integer Read GetRCount;
    // API methods
    Procedure RegisterAPI(AAPI : TGoogleAPIClass); virtual;
    Function IndexOfAPI(Const API : String) : Integer;
    Function FindAPIClass(Const API : String) : TGoogleAPIClass; virtual;
    Function GetAPIClass(Const API : String) : TGoogleAPIClass;
    Property APIClass[AIndex : Integer] :TGoogleAPIClass Read GetA;
    Property APICount : Integer Read GetACount;
  end;

Function APIFactory : TGoogleAPIFactory;

implementation

uses httpdefs;

Function APIFactory : TGoogleAPIFactory;
Var
  AClass : TGoogleAPIFactoryClass;
begin
  If TGoogleAPIFactory.DefaultFactory=Nil then
    begin
    AClass:=TGoogleAPIFactory.DefaultFactoryClass;
    If AClass=Nil then
      AClass:=TGoogleAPIFactory;
    TGoogleAPIFactory.DefaultFactory:=AClass.Create(Nil);
    end;
  Result:=TGoogleAPIFactory.DefaultFactory;
end;

{ TGoogleAPIFactory }

function TGoogleAPIFactory.GetR(AIndex : Integer): TGoogleResourceClass;
begin
  Result:=TGoogleResourceClass(FResources[Aindex]);
end;

function TGoogleAPIFactory.GetA(AIndex : Integer): TGoogleAPIClass;
begin
  Result:=TGoogleAPIClass(FAPIs[AIndex])
end;

function TGoogleAPIFactory.GetACount: Integer;
begin
  Result:=FAPIS.Count;
end;

function TGoogleAPIFactory.GetRCount: Integer;
begin
  Result:=FResources.Count;
end;

Constructor TGoogleAPIFactory.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
  FAPIs:=TClassList.Create;
  FResources:=TClassList.Create;
end;

Destructor TGoogleAPIFactory.Destroy;
begin
  FreeAndNil(FAPIs);
  FreeAndNil(FResources);
  inherited Destroy;
end;

Procedure TGoogleAPIFactory.RegisterAPI(AAPI: TGoogleAPIClass);

begin
  FAPIs.Add(AAPI);
end;

Function TGoogleAPIFactory.IndexOfAPI(Const API: String): Integer;

begin
  Result:=FAPIs.Count-1;
  While (Result>=0) and (CompareText(TGoogleAPIClass(FAPIs[Result]).APIName,API)<>0) do
    Dec(Result);
end;

Function TGoogleAPIFactory.FindAPIClass(Const API: String): TGoogleAPIClass;
Var
  I : Integer;

begin
  I:=IndexOfAPI(API);
  if I=-1 then
    Result:=Nil
  else
    Result:=GetA(I);
end;

Function TGoogleAPIFactory.GetAPIClass(Const API: String): TGoogleAPIClass;
begin
  Result:=FindAPIClass(API);
  if Result=Nil then
    Raise EGoogleAPI.CreateFmt('Unknown API : "%s"',[API]);
end;

Procedure TGoogleAPIFactory.RegisterResource(Resource: TGoogleResourceClass);
begin
  FResourceS.Add(Resource);
end;

Function TGoogleAPIFactory.IndexOfResource(Const Resource: String): Integer;
begin
  Result:=FResources.Count-1;
  While (Result>=0) and (CompareText(TGoogleResourceClass(FResources[Result]).ResourceName,Resource)<>0) do
    Dec(Result);
end;

Function TGoogleAPIFactory.FindResourceClass(Const Resource: String): TGoogleResourceClass;

Var
  I : Integer;

begin
  I:=IndexOfResource(Resource);
  if I=-1 then
    Result:=Nil
  else
    Result:=GetR(I);
end;

Function TGoogleAPIFactory.GetResourceClass(Const Resource: String): TGoogleResourceClass;
begin
  Result:=FindResourceClass(Resource);
  if Result=Nil then
    Raise EGoogleAPI.CreateFmt('Unknown resource : "%s"',[Resource]);
end;

{ TGoogleResource }

Procedure TGoogleResource.CheckAPI;
begin
  If (API=nil) then
    Raise EGoogleAPI.Create('Cannot perform this method, API is not assigned');
end;

Procedure TGoogleResource.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
  If Operation=opRemove then
    if FAPI=AComponent then
      FAPI:=Nil;
end;

Procedure TGoogleResource.SetAPI(AAPI: TGoogleAPI);
begin
  If Assigned(FAPI) then
    FAPI.RemoveFreeNotification(Self);
  FAPI:=AAPI;
  If Assigned(FAPI) then
    FAPI.FreeNotification(Self);
end;

Class Procedure TGoogleResource.AddToQuery(Var Q: String; Const AName,AValue: String);
begin
  If AValue='' then
    exit;
  if (Q<>'') then
    Q:=Q+'&';
  Q:=Q+Aname+'='+HTTPEncode(AValue);
end;

Class Procedure TGoogleResource.AddToQuery(Var Q: String; Const AName : String; AValue: Int64);
begin
  if AValue=0 then exit;
  if (Q<>'') then
    Q:=Q+'&';
  Q:=Q+Aname+'='+IntToStr(AValue);
end;

Class Procedure TGoogleResource.AddToQuery(Var Q: String; Const AName : String;  AValue: TDateTime);
begin
  if AValue=0 then exit;
  if (Q<>'') then
    Q:=Q+'&';
  Q:=Q+Aname+'='+DateTimeToRFC3339(AValue);
end;

Class Procedure TGoogleResource.AddToQuery(Var Q: String;  Const AName : String; AValue: Boolean);
begin
  if (Q<>'') then
    Q:=Q+'&';
  Q:=Q+Aname+'='+BoolToStr(AValue,'true','false');
end;

Function TGoogleResource.SubstitutePath(Const APath: String;
  Const Args: Array of const): String;
begin
  CheckAPI;
  Result:=API.SubstitutePath(ResourceName,APath,Args);
end;

Function TGoogleResource.ServiceCall(Const AMethod, APath, AQuery: String;
  AInput: TGoogleBaseObject; AReturnClass: TGoogleBaseObjectClass): TGoogleBaseObject;
begin
  CheckAPI;
  Result:=API.ServiceCall(Self,AMethod,APath,AQuery,AInput,AReturnClass);
end;

Class Function TGoogleResource.ResourceName: String;
begin
  Result:=ClassName;
  if UpCase(Result[1])='T' then
    Delete(Result,1,1);
  If CompareText(Copy(Result,Length(Result)-7,8),'Resource')=0 then
    Result:=Copy(Result,1,Length(Result)-8);
end;

Class Function TGoogleResource.DefaultAPI: TGoogleAPIClass;
begin
  Result:=Nil;
end;

{ TGoogleAPI }

class function TGoogleAPI.APIName: String;
begin
  Result:=ClassName;
  if UpCase(Result[1])='T' then
    Delete(Result,1,1);
  If CompareText(Copy(Result,Length(Result)-6,7),'API')=0 then
    Result:=Copy(Result,1,Length(Result)-7);
end;

class function TGoogleAPI.APINeedsAuth: Boolean;
begin
  Result:=Length(APIAuthScopes)<>0;
end;

procedure TGoogleAPI.SetGoogleClient(AValue: TGoogleClient);
begin
  if FGoogleClient=AValue then Exit;
  If Assigned(FGoogleClient) then
    FGoogleClient.RemoveFreeNotification(Self);
  FGoogleClient:=AValue;
  If Assigned(FGoogleClient) then
    FGoogleClient.FreeNotification(Self);
end;

procedure TGoogleAPI.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent=FGoogleClient) then
    FGoogleClient:=Nil;
end;

class function TGoogleAPI.APIInfo: TAPIInfo;
begin
  Result.Name:=APIName;
  Result.Version:=APIVersion;
  Result.Revision:=APIRevision;
  Result.ID:=APIID;
  Result.Title:=APITitle;
  Result.Description:=APIDescription;
  Result.OwnerDomain:=APIOwnerDomain;
  Result.OwnerName:=APIOwnerName;
  Result.Icon16:=APIIcon16;
  Result.Icon32:=APIIcon32;
  Result.documentationLink:=APIdocumentationLink;
  Result.rootUrl:=APIrootUrl;
  Result.basePath:=APIbasePath;
  Result.baseURL:=APIbaseURL;
  Result.Protocol:=APIProtocol;
  Result.servicePath:=APIservicePath;
  Result.batchPath:=APIbatchPath;
  Result.AuthScopes:=APIAuthScopes;
end;

class procedure TGoogleAPI.RegisterAPI;
begin
  APIFactory.RegisterAPI(Self);
end;

class procedure TGoogleAPI.RegisterAPIResources;
begin
  // needs to be implemented in descendents
end;

function TGoogleAPI.ServiceCall(const AResource: TGoogleResource; AMethod,
  APath, AQuery: String; AInput: TGoogleBaseObject;
  AReturnClass: TGoogleBaseObjectClass): TGoogleBaseObject;

Var
  D : TJSONData;
  R,S : String;
  C : TGoogleBaseObjectClass;
  BC : TBaseObjectClass;


begin
  Result:=Nil;
  if Assigned(AInput) then
    begin
    D:=TJSONObject.Create;
    AInput.SaveToJSON(TJSONObject(D));
    try
      S:=D.AsJSON;
    finally
      D.Free;
    end;
    end
  else
    S:='';
  R:=ServiceCall(AResource,AMethod,APAth,AQuery,S);
  if (R<>'') then
    begin
    D:=GetJSON(R);
    try
      C:=Nil;
      if Assigned(D) and (D.JSONType=jtObject) then
        begin
        S:=TJSONObject(D).Get('kind','');
        if (S<>'') then
          begin
          BC:=GoogleFactory.GetObjectClass(s);
          If BC.InheritsFrom(TGoogleBaseObject) then
            C:=TGoogleBaseObjectClass(BC)
          else
            C:=Nil;
          end;
        end;
      if C=Nil then
        C:=AReturnClass;
      Result:=C.Create;
      try
        Result.LoadFromJSON(D as TJSONObject);
      except
        FreeAndNil(Result);
        Raise;
      end;
    finally
      D.Free;
    end;
    end;
end;

function TGoogleAPI.ServiceCall(const AResource: TGoogleResource; AMethod,
  APath, AQuery, AInput: String): String;

Var
  URL : String;
  Req : TWebClientRequest;
  Resp  : TWebClientResponse;

begin
  URL:=APIBaseURL+APath;
  if AQuery<>'' then
    URL:=URL+'?'+AQuery;
  Result:='';
  Req:=Nil;
  Resp:=Nil;
  try
    Req:=googleclient.WebClient.CreateRequest;
    if (AInput<>'') then
      Req.Headers.Values['Content-type']:='application/json';
    Req.SetContentFromString(AInput);
    If Not APINeedsAuth then
      Resp:=googleclient.WebClient.ExecuteRequest(AMethod,URL,Req)
    else
      Resp:=googleclient.WebClient.ExecuteSignedRequest(AMethod,URL,Req);
    If (Resp.StatusCode div 100)<>2 then
      Raise EGoogleAPI.CreateFmt('%d error executing request :  %s',[Resp.StatusCode,Resp.StatusText]);
    Result:=Resp.GetContentAsString;
  finally
    Req.Free;
    Resp.Free;
  end;
end;

function TGoogleAPI.SubstitutePath(const AResource, APath: String;
  const Args: array of const): String;

Var
  N,V : String;
  I : Integer;

begin
  Result:=APath;
  I:=0;
  While I<High(Args) do
    begin
    if Args[i].VType<>vtAnsiString then
      Raise EGoogleAPI.CreateFmt('Expected name argument at position %d',[i]);
    N:=ansistring(Args[i].VAnsiString);
    Inc(I);
    With Args[i] do
      Case VType of
         vtInteger    : V:=IntToStr(VInteger);
         vtBoolean    : V:=BoolToStr(VBoolean,'true','false');
         vtChar       : V:=VChar;
{$ifndef FPUNONE}
         vtExtended   : system.Str(VExtended^,V);
{$endif}
         vtString     : V:=VString^;
         vtPChar      : V:=VPChar;
         vtWideChar   : V:=VWideChar;
         vtPWideChar  : V:=VPWideChar;
         vtAnsiString : V:=ansistring(VAnsiString);
         vtCurrency   : Str(VCurrency^,V);
         vtVariant    : V:=VVariant^;
         vtWideString : V:=Widestring(VWideString);
         vtInt64      : V:=IntToStr(vInt64^);
         vtQWord      : V:=IntToStr(vQWord^);
         vtUnicodeString : V:=UnicodeString(VUnicodeString);
      end;
    Inc(i);
    Result:=StringReplace(Result,'{'+N+'}',V,[]);
    end;
end;

function TGoogleAPI.CreateResource(AClass: TGoogleResourceClass
  ): TGoogleResource;
begin
  Result:=AClass.Create(Self);
  Result.SetAPI(Self);
end;

function TGoogleAPI.CreateResource(const Resource: String): TGoogleResource;
begin
  Result:=CreateResource(APIFactory.GetResourceClass(Resource));
end;

finalization
  FreeAndNil(TGoogleAPIFactory.DefaultFactory);
end.


