{ **********************************************************************
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2015 The free pascal team.

    Base OData service API classes

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit odataservice;

{$mode objfpc}{$H+}
{$DEFINE DEBUGODATASERVICE}

interface

uses
  Classes, SysUtils, contnrs, restbase, odatabase, fpwebclient, fpjson;

Type
  TODataService = Class;
  TODataServiceClass = Class of TODataService;
  TODataServiceArray = Array of TODataService;

  TODataEntityContainer = Class;
  TODataEntityContainerClass = Class of TODataEntityContainer;
  TODataEntityContainerArray = Array of TODataEntityContainer;

  TODataEntitySet = Class;
  TODataEntitySetClass = Class of TODataEntitySet;
  TODataEntitySetArray = Array of TODataEntitySet;

  { TODataEntity }

  TODataEntity = Class;
  TODataEntityClass = Class of TODataEntity;
  TODataEntityArray = Array of TODataEntity;


  TODataServiceFactory = Class;
  TODataServiceFactoryClass = Class of TODataServiceFactory;

  TQueryParams = Record
    Filter : String;
    OrderBy : String;
    Search : String;
    Skip : Integer;
    Top : Integer;
  end;

  TODataArrayResult = Record
    NextLink : String;
    Data : TObjectArray;
  end;

  { TODataService }
  TScopeInfo = Record
    Name : string;
    Description : string;
  end;
  TScopeInfoArray = Array of TScopeInfo;

  { TODataHeaders }

  TODataHeaders = Class(TPersistent)
  private
    FAccept: String;
    FAcceptCharset: String;
    FAcceptLanguage: String;
    FContentType: String;
    FIfMatch: String;
    FIfNoneMatch: String;
    FODataIsolation: String;
    FODataMaxversion: String;
    FPrefer: String;
  Protected
    Procedure GetHeaders(Headers : TStrings); virtual;
  Public
    Procedure Assign(Source : TPersistent); override;
  Published
    Property IfMatch : String Read FIfMatch write FIfMatch;
    Property IfNoneMatch : String Read FIfNoneMatch write FIfNoneMatch;
    Property Accept : String Read FAccept write FAccept;
    Property AcceptCharset : String Read FAcceptCharset write FAcceptCharset;
    Property AcceptLanguage : String Read FAcceptLanguage write FAcceptLanguage;
    Property ODataIsolation : String Read FODataIsolation write FODataIsolation;
    Property ODataMaxversion : String Read FODataMaxversion Write FODataMaxversion;
    Property Prefer : String Read FPrefer Write FPrefer;
    Property ContentType : String Read FContentType Write FContentType;
  end;

  TServiceLogEvent = Procedure(Sender : TObject; Const Msg : String) of Object;

  TODataMetaData = (omEmpty, omNone, omMinimal, omFull);
  TODataService = Class(TComponent)
  private
    FAPINeedsAuth: Boolean;
    FODataHeaders: TODataHeaders;
    FODataMetaData: TODataMetaData;
    FOnLog: TServiceLogEvent;
    FResponseHeaders: TStrings;
    FServiceURL: String;
    FWebClient: TAbstractWebClient;
    Function RedirectURL(Old, Location: String): String;
    procedure SetODataHeaders(AValue: TODataHeaders);
    procedure SetWebClient(AValue: TAbstractWebClient);
  Protected
    Procedure DoLog(Const Msg : String);
    Procedure DoLog(Const Fmt : String; Const Args : Array of const);
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Function ComposeURL(Const APath, AQuery: String): String;
    Function Service : TODataService; // Used when creating EntityContainers.
    Function JSONToODataError(O: TJSONObject): TODataError; virtual;
    Function RespToError(Resp: TWebClientResponse): EOData; virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    class function ObjectRestKind : String; virtual;
    Class Function ServiceName : String; virtual;
    Class Procedure RegisterService; virtual;
    Class Procedure RegisterEntityContainers; virtual;
    Class Procedure AddToQuery(Var Q : String;  Const AName , AValue : String);virtual;
    Class Procedure AddToQuery(Var Q : String;  Const AName : String; AValue : Int64);virtual;
    Class Procedure AddToQuery(Var Q : String;  Const AName : String; AValue : TDateTime);virtual;
    Class Procedure AddToQuery(Var Q : String;  Const AName : String; AValue : Boolean);virtual;
    Class Function QueryParamsToString(Const Q : TQueryParams) : String; virtual;
    // The is the basic call to which all calls redirect. AURL must be a full URL
    Procedure ServiceCall(Const AMethod, AURL : String; AInput, AOutput : TStream); virtual;
    // Helper calls.
    Procedure ServiceCall(Const AMethod, APath,AQuery : String; AInput, AOutput : TStream); virtual;
    Function ServiceCall(Const AMethod, AURL, AInput : String) : String; virtual;
    Function ServiceCall(Const AMethod, APath, AQuery, AInput : String) : String; virtual;
    Function ServiceCall(Const AMethod, APath, AQuery : String; AInput : TODataObject; AReturnClass : TODataObjectClass) : TODataObject; virtual;
    Procedure GetStream(Const APath, AQuery, AContentType: String; AStream: TStream); virtual;
    Procedure SetStream(Const APath, AQuery, AContentType: String; AStream: TStream); virtual;
    // For GET only.
    Function ArrayServiceCall(Const AURL: String; AElementClass: TODataObjectClass): TODataArrayResult; virtual;
    Function ArrayServiceCall(Const APath, AQuery: String; AElementClass: TODataObjectClass): TODataArrayResult; virtual;
    Function SingleServiceCall(Const AMethod,APath, AQuery, AInput: String; AObjectClass: TODataObjectClass): TODataObject; virtual;
    Function SingleServiceCall(Const APath, AQuery: String; AObjectClass: TODataObjectClass): TODataObject; virtual;
    Function GetMulti(Const AURL : String; AClass : TODataObjectClass; FetchAll : Boolean; Out NextLink : String) : TODataObjectArray;
    Function GetMulti(Const APath,AQuery : String; AClass : TODataObjectClass; FetchAll : Boolean; Out NextLink : String) : TODataObjectArray;
    Function CreateEntityContainer(AClass : TODataEntityContainerClass) : TODataEntityContainer; virtual;
    Function CreateEntityContainer(const EntityContainer : String) : TODataEntityContainer; virtual;
    Function GetEntityClass(Const Entity : String) : TODataEntityClass; virtual;
  Public
    Property OnLog : TServiceLogEvent Read FOnLog Write FOnLog;
    Property WebClient : TAbstractWebClient Read FWebClient Write SetWebClient;
    Property ServiceURL : String Read FServiceURL Write FServiceURL;
    Property APINeedsAuth : Boolean Read FAPINeedsAuth Write FAPINeedsAuth;
    Property ODataRequestHeaders : TODataHeaders Read FODataHeaders Write SetODataHeaders;
    Property LastResponseHeaders : TStrings Read FResponseHeaders;
    // Not used yet
    Property ODataMetaData : TODataMetaData Read FODataMetaData Write FODataMetaData;
  end;

  TODataEntity = Class(TODataObject)
  Private
    FBasePath : String;
  Protected
    Function CreateContainedEntitySet(AService : TODataService; APath : String; AClass : TODataEntitySetClass) : TODataEntitySet;
    Function GetContainedSingleTon(AService : TODataService; APath : String; AClass : TODataEntityClass) : TODataEntity;
    Function DoPost(AService : TODataservice) : TODataEntity;
    Function DoPut(AService : TODataservice) : TODataEntity;
    Function DoPatch(AService : TODataservice) : TODataEntity;
    Function GetMulti(AService : TODataservice; Const AURL : String; AType : TODataEntityClass; FetchAll : Boolean; Out NextLink : String) : TODataEntityArray; virtual;
    Procedure DoGetStream(AService : TODataservice; AContentType : String; AStream : TStream);
    Procedure DoSetStream(AService : TODataservice; AContentType : String; AStream : TStream);
  public
    Class Function EntityName : String; virtual;
    Function BaseURL(AService : TODataService) : String; virtual;
    Function KeyAsURLPart : string; virtual;
    Function Delete(AService : TODataService) : Boolean;
    Property BasePath : String Read FBasePath Write FBasePath;
    Procedure Post(AService : TODataservice);
    Procedure Put(AService : TODataservice);
    Procedure Patch(AService : TODataservice);
  end;

  { TODataEntitySet }

  TODataEntitySet = Class(TComponent)
  private
    FContainedPath: String;
    FContainer: TODataEntityContainer;
    FContainerURL: String;
    FODataService: TODataService;
    procedure SetContainer(AValue: TODataEntityContainer);
  Protected
    Function GetBaseURL: String;
    Procedure CheckService;
    Procedure CheckContainer;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Function GetSingle(Const AKey : String) : TODataEntity; virtual;
    Function GetMulti(Const AQuery : String; FetchAll : Boolean; Out NextLink : String) : TODataEntityArray; virtual;
    Function GetMulti(Const AQuery : TQueryParams; FetchAll : Boolean; Out NextLink : String) : TODataEntityArray;
    Property ContainerURL : String Read FContainerURL Write FContainerURL;
    Property ContainedPath : String Read FContainedPath Write FContainedPath;
    Property Service : TODataService Read FODataService Write FODataService;
  Public
    class function ObjectRestKind : String;  virtual;
    Class function EntityClass : TODataEntityClass; virtual; abstract;
    Function GetService : TODataService;
    Property Container : TODataEntityContainer Read FContainer Write SetContainer;
  end;


  { TODataEntityContainer }

  TODataEntityContainer = Class(TComponent)
  private
    FService: TODataService;
    Procedure SetService(AService : TODataService);
  Protected
    Procedure CheckService;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  PubliC
    class function ObjectRestKind : String; virtual;
    Class Function EntityContainerName : String; virtual;
    Class Function DefaultService : TODataServiceClass; virtual;
    Function CreateEntitySet(AClass : TODataEntitySetClass) : TODataEntitySet; virtual;
    Property Service : TODataService Read FService Write SetService;
  end;


  { TODataServiceFactory }

  TODataServiceFactory = Class(TComponent)
  Private
    FServices : TClassList;
    FEntitySets : TClassList;
    FEntities : TClassList;
    function GetA(AIndex : Integer): TODataServiceClass;
    function GetACount: Integer;
    function GetE(AIndex : Integer): TODataEntityClass;
    function GetECount: Integer;
    function GetR(AIndex : Integer): TODataEntityContainerClass;
    function GetRCount: Integer;
  Public
    Class var
       DefaultFactoryClass : TODataServiceFactoryClass;
       DefaultFactory : TODataServiceFactory;
  Public
    Constructor Create(Aowner : TComponent); override;
    Destructor Destroy; override;
    // EntityContainer methods
    Procedure RegisterEntityContainer(EntityContainer : TODataEntityContainerClass); virtual;
    Function IndexOfEntityContainer(Const EntityContainer : String) : Integer;
    Function FindEntityContainerClass(Const EntityContainer : String) : TODataEntityContainerClass; virtual;
    Function GetEntityContainerClass(Const EntityContainer : String) : TODataEntityContainerClass;
    Property EntityContainerClass[AIndex : Integer] : TODataEntityContainerClass Read GetR;
    Property EntityContainerCount : Integer Read GetRCount;
    // Entity methods
    Function IndexOfEntityClass(Const Entity : String) : Integer;
    Function FindEntityClass(Const Entity : String) : TODataEntityClass;
    Function GetEntityClass(Const Entity : String) : TODataEntityClass;
    Procedure RegisterEntityClass(AClass : TODataEntityClass); virtual;
    Property EntityClass[AIndex : Integer] :TODataEntityClass Read GetE;
    Property EntityCount : Integer Read GetECount;
    // Service methods
    Procedure RegisterService(AService : TODataServiceClass); virtual;
    Function IndexOfService(Const Service : String) : Integer;
    Function FindServiceClass(Const Service : String) : TODataServiceClass; virtual;
    Function GetServiceClass(Const Service : String) : TODataServiceClass;
    Property ServiceClass[AIndex : Integer] :TODataServiceClass Read GetA;
    Property ServiceCount : Integer Read GetACount;
  end;

Function ODataFactory : TODataServiceFactory;

implementation

uses uriparser,httpdefs;

{ ---------------------------------------------------------------------
  TODataServiceFactory
  ---------------------------------------------------------------------}

Function ODataFactory : TODataServiceFactory;

Var
  AClass : TODataServiceFactoryClass;

begin
  If TODataServiceFactory.DefaultFactory=Nil then
    begin
    AClass:=TODataServiceFactory.DefaultFactoryClass;
    If AClass=Nil then
      AClass:=TODataServiceFactory;
    TODataServiceFactory.DefaultFactory:=AClass.Create(Nil);
    end;
  Result:=TODataServiceFactory.DefaultFactory;
end;

{ TODataHeaders }

Procedure TODataHeaders.GetHeaders(Headers: TStrings);

  Procedure MaybeAdd(Const AName,AValue : String) ;

  begin
    if (AValue<>'') then
      Headers.Values[AName]:=' '+AValue;
  end;

begin
  MaybeAdd('If-Match',IfMatch);
  MaybeAdd('If-None-Match',IfNoneMatch);
  MaybeAdd('Accept',Accept);
  MaybeAdd('Accept-Charset',AcceptCharset);
  MaybeAdd('Accept-Language',AcceptLanguage);
  MaybeAdd('OData-Isolation',ODataIsolation);
  MaybeAdd('OData-Maxversion',ODataMaxversion);
  MaybeAdd('Prefer',Prefer);
  MaybeAdd('Content-Type',ContentType);
end;


Procedure TODataHeaders.Assign(Source: TPersistent);

Var
  H : TODataHeaders;

begin
  if (Source is TODataHeaders) then
    begin
    H:=Source as TODataHeaders;
    IfMatch:=H.IfMatch;
    IfNoneMatch:=H.IfNoneMatch;
    Accept:=H.Accept;
    AcceptCharset:=H.AcceptCharset;
    AcceptLanguage:=H.AcceptLanguage;
    ODataIsolation:=H.ODataIsolation;
    ODataMaxversion:=H.ODataMaxversion;
    Prefer:=H.Prefer;
    end
  else
    inherited Assign(Source);
end;


function TODataServiceFactory.GetR(AIndex : Integer): TODataEntityContainerClass;
begin
  Result:=TODataEntityContainerClass(FEntitySets[Aindex]);
end;

function TODataServiceFactory.GetA(AIndex : Integer): TODataServiceClass;
begin
  Result:=TODataServiceClass(FServices[AIndex])
end;

function TODataServiceFactory.GetACount: Integer;
begin
  Result:=FServices.Count;
end;

function TODataServiceFactory.GetE(AIndex : Integer): TODataEntityClass;
begin
  Result:=TODataEntityClass(FEntities[AIndex]);
end;

function TODataServiceFactory.GetECount: Integer;
begin
  Result:=FEntities.Count;
end;

function TODataServiceFactory.GetRCount: Integer;
begin
  Result:=FServices.Count;
end;

Constructor TODataServiceFactory.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
  FServices:=TClassList.Create;
  FEntitySets:=TClassList.Create;
  FEntities:=TClassList.Create; // This can probably be done faster.
end;

Destructor TODataServiceFactory.Destroy;
begin
  FreeAndNil(FEntities);
  FreeAndNil(FServices);
  FreeAndNil(FEntitySets);
  inherited Destroy;
end;

Procedure TODataServiceFactory.RegisterService(AService : TODataServiceClass);

begin
  FServices.Add(AService);
end;

Function TODataServiceFactory.IndexOfService(Const Service: String): Integer;

begin
  Result:=FServices.Count-1;
  While (Result>=0) and (CompareText(TODataServiceClass(FServices[Result]).ServiceName,Service)<>0) do
    Dec(Result);
end;

Function TODataServiceFactory.FindServiceClass(Const Service: String): TODataServiceClass;
Var
  I : Integer;

begin
  I:=IndexOfService(Service);
  if I=-1 then
    Result:=Nil
  else
    Result:=GetA(I);
end;

Function TODataServiceFactory.GetServiceClass(Const Service : String): TODataServiceClass;
begin
  Result:=FindServiceClass(Service);
  if Result=Nil then
    Raise EOData.CreateFmt('Unknown API : "%s"',[Service]);
end;

Procedure TODataServiceFactory.RegisterEntityContainer(EntityContainer: TODataEntityContainerClass);
begin
  FEntitySets.Add(EntityContainer);
end;

Function TODataServiceFactory.IndexOfEntityContainer(Const EntityContainer: String): Integer;
begin
  Result:=FEntitySets.Count-1;
  While (Result>=0) and (CompareText(TODataEntityContainerClass(FEntitySets[Result]).EntityContainerName,EntityContainer)<>0) do
    Dec(Result);
end;

Function TODataServiceFactory.FindEntityContainerClass(Const EntityContainer: String): TODataEntityContainerClass;

Var
  I : Integer;

begin
  I:=IndexOfEntityContainer(EntityContainer);
  if I=-1 then
    Result:=Nil
  else
    Result:=GetR(I);
end;

Function TODataServiceFactory.GetEntityContainerClass(Const EntityContainer: String): TODataEntityContainerClass;
begin
  Result:=FindEntityContainerClass(EntityContainer);
  if Result=Nil then
    Raise EOData.CreateFmt('Unknown EntityContainer : "%s"',[EntityContainer]);
end;

Function TODataServiceFactory.IndexOfEntityClass(Const Entity: String): Integer;
begin
  Result:=FEntitySets.Count-1;
  While (Result>=0) and (CompareText(TOdataEntityClass(FEntities[Result]).EntityName,Entity)<>0) do
    Dec(Result);
end;

Function TODataServiceFactory.FindEntityClass(Const Entity: String
  ): TODataEntityClass;
Var
  I : Integer;

begin
  I:=IndexOfEntityClass(Entity);
  if I=-1 then
    Result:=Nil
  else
    Result:=GetE(I);
end;

Function TODataServiceFactory.GetEntityClass(Const Entity: String
  ): TODataEntityClass;
begin
  Result:=FindEntityClass(Entity);
  if Result=Nil then
    Raise EOData.CreateFmt('Unknown Entity : "%s"',[Entity]);
end;

Procedure TODataServiceFactory.RegisterEntityClass(AClass: TODataEntityClass);
begin
  FEntities.Add(AClass);
end;


{ ---------------------------------------------------------------------
  TODataEntitySet
  ---------------------------------------------------------------------}

procedure TODataEntitySet.SetContainer(AValue: TODataEntityContainer);
begin
  if FContainer=AValue then Exit;
  FContainer:=AValue;
end;

Procedure TODataEntitySet.CheckContainer;
begin
  If not assigned(Container) then
     Raise EOData.Create('Cannot perform this method, container is not assigned');
end;

Procedure TODataEntitySet.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  If Operation=opRemove then
    if FContainer=AComponent then
      FContainer:=Nil;
end;

Function TODataEntitySet.GetSingle(Const AKey: String): TODataEntity;

Var
  URL : String;

begin
  CheckService;
  URL:=GetBaseURL+'('+AKey+')';
  Result:=TODataEntity(GetService.SingleServiceCall(URL,'',EntityClass));
  if ContainedPath<>'' then
    Result.BasePath:=URL;
//  Writeln(ClassName,' created  single ',Result.ClassName,' with Basepath : ', Result.BasePath);
end;

Function TODataEntitySet.GetBaseURL : String;

Var
  P : String;

begin
  If (ContainerURL<>'') then
    begin
    P:=ContainedPath;
    Result:=IncludeHTTPPathDelimiter(ContainerURL);
    end
  else
    begin
    CheckService;
    Result:=IncludeHTTPPathDelimiter(GetService.ServiceURL);
    end;
  if (P='') then
    P:=ObjectRestKind;
  Result:=Result+P;
end;

Function TODataEntitySet.GetMulti(Const AQuery : String; FetchAll : Boolean; Out NextLink : String) : TODataEntityArray;

Var
  EC : TODataEntityClass;
  AURL : String;


begin
  CheckService;
  AURL:=GetBaseURL;
  EC:=EntityClass;
  Result:=TODataEntityArray(GetService.GetMulti(AURL,AQuery,EC,FetchAll,NextLink));
end;

Function TODataEntitySet.GetMulti(Const AQuery : TQueryParams; FetchAll : Boolean; Out NextLink : String) : TODataEntityArray;

begin
  CheckService;
  Result:=GetMulti(TODataService.QueryParamsToString(AQuery),FetchAll,NextLink);
end;


class function TODataEntitySet.ObjectRestKind: String;
begin
  Result:=Classname;
end;

Function TODataEntitySet.GetService: TODataService;
begin
  Result:=Service;
  if not Assigned(Service) then
    begin
    CheckContainer;
    Result:=Container.Service;
    end;
end;

Procedure TODataEntitySet.CheckService;
begin
  If (GetService=nil) then
    Raise EOData.Create('Cannot perform this method, Service is not assigned');
end;

{ ---------------------------------------------------------------------
  TODataEntityContainer
  ---------------------------------------------------------------------}

Procedure TODataEntityContainer.CheckService;
begin
  If (Service=nil) then
    Raise EOData.Create('Cannot perform this method, Service is not assigned');
end;

Procedure TODataEntityContainer.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
  If Operation=opRemove then
    if FService=AComponent then
      FService:=Nil;
end;

Procedure TODataEntityContainer.SetService(AService: TODataService);
begin
  If Assigned(FService) then
    FService.RemoveFreeNotification(Self);
  FService:=AService;
  If Assigned(FService) then
    FService.FreeNotification(Self);
end;


class function TODataEntityContainer.ObjectRestKind: String;
begin
  Result:=ClassName;
end;

Class Function TODataEntityContainer.EntityContainerName: String;
begin
  Result:=ClassName;
  if UpCase(Result[1])='T' then
    Delete(Result,1,1);
  If CompareText(Copy(Result,Length(Result)-7,8),'EntityContainer')=0 then
    Result:=Copy(Result,1,Length(Result)-8);
end;

Class Function TODataEntityContainer.DefaultService: TODataServiceClass;
begin
  Result:=Nil;
end;

Function TODataEntityContainer.CreateEntitySet(AClass: TODataEntitySetClass
  ): TODataEntitySet;
begin
  Result:=AClass.Create(Self);
  Result.Container:=Self;
end;

{ ---------------------------------------------------------------------
  TODataService
  ---------------------------------------------------------------------}


class procedure TODataService.AddToQuery(var Q: String; const AName,
  AValue: String);
begin
  If AValue='' then
    exit;
  if (Q<>'') then
    Q:=Q+'&';
  Q:=Q+Aname+'='+HTTPEncode(AValue);
end;

class procedure TODataService.AddToQuery(var Q: String; const AName: String;
  AValue: Int64);
begin
  if AValue=0 then exit;
  if (Q<>'') then
    Q:=Q+'&';
  Q:=Q+Aname+'='+IntToStr(AValue);
end;

class procedure TODataService.AddToQuery(var Q: String; const AName: String;
  AValue: TDateTime);
begin
  if AValue=0 then exit;
  if (Q<>'') then
    Q:=Q+'&';
  Q:=Q+Aname+'='+DateTimeToRFC3339(AValue);
end;

class procedure TODataService.AddToQuery(var Q: String; const AName: String;
  AValue: Boolean);
begin
  if (Q<>'') then
    Q:=Q+'&';
  Q:=Q+Aname+'='+BoolToStr(AValue,'true','false');
end;

class function TODataService.QueryParamsToString(const Q: TQueryParams): String;
begin
  Result:='';
  AddToQuery(Result,'$filter',Q.Filter);
  AddToQuery(Result,'$search',Q.Search);
  AddToQuery(Result,'$orderby',Q.OrderBy);
  AddToQuery(Result,'$skip',Q.Skip);
  AddToQuery(Result,'$top',Q.Top);
end;
procedure TODataService.SetWebClient(AValue: TAbstractWebClient);
begin
  if FWebClient=AValue then Exit;
  If Assigned(FWebClient) then
    FWebClient.RemoveFreeNotification(Self);
  FWebClient:=AValue;
  If Assigned(FWebClient) then
    FWebClient.FreeNotification(Self);
end;

procedure TODataService.DoLog(const Msg: String);
begin
  If Assigned(FOnLog) then
    FOnLog(Self,Msg);
end;

procedure TODataService.DoLog(const Fmt: String; const Args: array of const);
begin
  If Assigned(FOnLog) then
    FOnLog(Self,Format(Fmt,Args));
end;

procedure TODataService.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent=FWebClient) then
    FWebClient:=Nil;
end;

class function TODataService.ServiceName: String;

Const
  SService = 'Service';
  LService = Length(SService);

Var
  L : integer;

begin
  Result:=ClassName;
  If (Result<>'') and (Result[1]='T') then
    Delete(Result,1,1);
  L:=Length(Result);
  If (Copy(Result,L-LService+1,LService)=SService) then
    Result:=Copy(Result,1,L-LService);
end;

function TODataService.Service: TODataService;
begin
  Result:=Self;
end;

class function TODataService.ObjectRestKind: String;
begin
  Result:=ClassName;
end;

class procedure TODataService.RegisterService;
begin
  ODataFactory.RegisterService(Self);
end;

class procedure TODataService.RegisterEntityContainers;
begin
  // needs to be implemented in descendents
end;

function TODataService.ServiceCall(const AMethod, APath, AQuery: String;
  AInput: TODataObject; AReturnClass: TODataObjectClass): TODataObject;

Var
  D : TJSONData;
  R,S : String;
  C : TODataObjectClass;

begin
  Result:=Nil;
  ODataRequestHeaders.ContentType:='application/json';
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
  R:=ServiceCall(AMethod,APAth,AQuery,S);
  if (R<>'') then
    begin
    D:=GetJSON(R);
    try
      C:=Nil;
      if Assigned(D) and (D.JSONType=jtObject) then
        begin
        S:=TJSONObject(D).Get('kind','');
        if (S<>'') then
          C:=Service.GetEntityClass(s);
        end;
      if C=Nil then
        C:=AReturnClass;
      if (C<>Nil) then
        begin
        Result:=C.Create;
        try
          Result.LoadFromJSON(D as TJSONObject);
        except
          FreeAndNil(Result);
          Raise;
        end;
        end;
    finally
      D.Free;
    end;
    end;
end;

function TODataService.RedirectURL(Old, Location: String): String;


begin
  if not ResolveRelativeURI(Old,Location,Result) then
    Result:='';
end;

procedure TODataService.SetODataHeaders(AValue: TODataHeaders);
begin
  if FODataHeaders=AValue then Exit;
  FODataHeaders.Assign(AValue);
end;

function TODataService.ServiceCall(const AMethod, APath, AQuery, AInput: String
  ): String;

begin
  Result:=ServiceCall(AMethod,ComposeURL(APath,AQuery),AInput);
end;

procedure TODataService.GetStream(const APath, AQuery, AContentType: String;
  AStream: TStream);

Var
  CT : String;


begin
  CT:=ODataRequestHeaders.ContentType;
  try
    ODataRequestHeaders.ContentType:=AContentType;
    ServiceCall('GET',ComposeURL(APath,AQuery),Nil,AStream);
  finally
    ODataRequestHeaders.ContentType:=CT;
  end;
end;

procedure TODataService.SetStream(const APath, AQuery, AContentType: String;
  AStream: TStream);

Var
  CT : String;

begin
  CT:=ODataRequestHeaders.ContentType;
  try
    ODataRequestHeaders.ContentType:=AContentType;
    ServiceCall('PUT',ComposeURL(APath,AQuery),AStream,Nil);
  finally
    ODataRequestHeaders.ContentType:=CT;
  end;
end;

function TODataService.JSONToODataError(O: TJSONObject): TODataError;

Var
  A : TJSONArray;
  En : TJSONEnum;
  Det : TODataErrorDetails;

begin
  Result:=TODataError.Create;
  Result.Message:=O.get('message','');
  Result.Code:=O.get('code','');
  Result.Target:=O.get('target','');
  If (O.IndexOfName('innererror')<>-1) then
    Result.InnerError:=O.Elements['innererror'].AsJSON;
  A:=O.Get('details',TJSONArray(Nil));
  If Assigned(A) then
    begin
    SetLength(Det,A.Count);
    For En in A do
      begin
      O:=En.Value as TJSONObject;
      Det[EN.KeyNum].Message:=O.Get('message');
      Det[EN.KeyNum].Code:=O.Get('code');
      Det[EN.KeyNum].Target:=O.Get('target');
      end;
    end;
  Result.Details:=Det;
end;

function TODataService.RespToError(Resp: TWebClientResponse): EOData;

Var
  D : TJSONData;
  O : TJSONObject;

begin
  Result:=EOData.CreateFmt('%d error executing request :  %s',[Resp.StatusCode,Resp.StatusText]);
  D:=Nil;
  if Resp.Content.Size>0 then
    begin
    try
      Resp.Content.Position:=0;
      D:=GetJSON(Resp.Content);
    except
      D:=Nil; // No json.
    end;
    try
      if D is TJSONObject then
        begin
        O:=TJSONOBject(D).Get('error',TJSONObject(Nil));
        if Assigned(O) then
          Result.Error:=JSONToODataError(O);
        end;
    finally
      D.Free;
    end;
  end;
end;

constructor TODataService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FODataHeaders:=TODataHeaders.Create;
  FResponseHeaders:=TStringList.Create;
end;

destructor TODataService.Destroy;
begin
  FreeAndNil(FResponseHeaders);
  FreeAndNil(FODataHeaders);
  inherited Destroy;
end;

function TODataService.ServiceCall(const AMethod, AURL, AInput: String): String;

Var
  Sin,Sout: TStringStream;

begin
  {$IFDEF DEBUGODATASERVICE}DoLog('Entering TODataService.ServiceCall(%s,%s,%s)',[AMethod, AURL,AInput]);{$ENDIF}
  SOut:=Nil;
  Sin:=TStringStream.Create(AInput);
  Try
    SOut:=TStringStream.Create('');
    SIn.Position:=0;
    ServiceCall(AMethod,AURL,Sin,Sout);
//    Writeln('Result : ',Sout.DataString);
    Result:=Sout.DataString;
  Finally
    Sin.Free;
    SOut.Free;
  end;
  {$IFDEF DEBUGODATASERVICE}DoLog('Exiting TODataService.ServiceCall(%s,%s,%s)',[AMethod, AURL,AInput]);{$ENDIF}
end;

procedure TODataService.ServiceCall(const AMethod, AURL: String; AInput,
  AOutput: TStream);

Var
  URL : String;
  Req : TWebClientRequest;
  Resp  : TWebClientResponse;
  IsRedirect : Boolean;

begin
{$IFDEF DEBUGODATASERVICE}DoLog('Entering TODataService.ServiceCall(%s,%s,%s,%s)',[AMethod, AURL,BoolToStr(Assigned(AInput),'True','False'),BoolToStr(Assigned(AOutput),'True','False')]);{$ENDIF}
  FResponseHeaders.Clear;
  URL:=AURL;
  Repeat
    Resp:=Nil;
    try
      {$IFDEF DEBUGODATASERVICE}DoLog('TODataService.ServiceCall : Creating request "%s" URL: "%s"',[AMethod, URL]);{$ENDIF}
      Req:=WebClient.CreateRequest;
      ODataRequestHeaders.GetHeaders(Req.Headers);
      if Assigned(AInput) then
        begin
        AInput.Position:=0;
        Req.Content.CopyFrom(AInput,0);
        Req.Content.Position:=0;
        Req.Headers.Values['Content-Length']:=IntToStr(AInput.Size);
        end;
      {$IFDEF DEBUGODATASERVICE}DoLog('Headers: %s',[Req.Headers.Text]);{$ENDIF}
      isRedirect:=False;
      {$IFDEF DEBUGODATASERVICE}DoLog('Have webclient: %d, webclient.requestsigner: %d',[Ord(Assigned(Webclient)),Ord(Assigned(WebClient) and Assigned(WebClient.RequestSigner))]);{$ENDIF}
      If Not APINeedsAuth then
        Resp:=WebClient.ExecuteRequest(AMethod,URL,Req)
      else
        Resp:=WebClient.ExecuteSignedRequest(AMethod,URL,Req);
      {$IFDEF DEBUGODATASERVICE}DoLog('TODataService.ServiceCall : Executed request %d',[Resp.StatusCode]);{$ENDIF}
      If ((Resp.StatusCode div 100)=3) and (Byte(Resp.StatusCode mod 100) in [1,2,3,7])  then
        begin
        isRedirect:=True;
        URL:=RedirectURl(URl,Trim(Resp.Headers.Values['Location']));
        end
      else If (Resp.StatusCode div 100)<>2 then
        Raise RespToError(Resp)
      else
        begin
        If Assigned(AOutput) then
          begin
          Resp.Content.Position:=0;
          AOutput.CopyFrom(Resp.Content,0);
          end;
        FResponseHeaders.Assign(Resp.Headers);
        end;
    finally
      Req.Free;
      Resp.Free;
    end;
  until (Not IsRedirect) or (URL='');
  {$IFDEF DEBUGODATASERVICE}DoLog('Exiting TODataService.ServiceCall(%s,%s)',[AMethod, AURL]);{$ENDIF}
end;

procedure TODataService.ServiceCall(const AMethod, APath, AQuery: String;
  AInput, AOutput: TStream);

begin
  {$IFDEF DEBUGODATASERVICE}DoLog('Entering TODataService.ServiceCall(%s,%s,%s)',[AMethod, APath,AQuery]);{$ENDIF}
  ServiceCall(AMethod,ComposeURL(APath,AQuery),AInput,AOutput);
  {$IFDEF DEBUGODATASERVICE}DoLog('Exiting TODataService.ServiceCall(%s,%s,%s)',[AMethod, APath,AQuery]);{$ENDIF}
end;


function TODataService.ArrayServiceCall(const APath, AQuery: String;
  AElementClass: TODataObjectClass): TODataArrayResult;

begin
  {$IFDEF DEBUGODATASERVICE}DoLog('Entering TODataService.ArrayServiceCall(%s,%s)',[ APath,AQuery]);{$ENDIF}
  Result:=ArrayServiceCall(ComposeURL(APath,AQuery),AElementClass);
  {$IFDEF DEBUGODATASERVICE}DoLog('Exiting TODataService.ArrayServiceCall(%s,%s)',[ APath,AQuery]);{$ENDIF}
end;


function TODataService.ComposeURL(const APath, AQuery: String): String;

begin
  {$IFDEF DEBUGODATASERVICE}DoLog('Entering TODataService.ComposeURL(%s,%s)',[ APath,AQuery]);{$ENDIF}
  if Pos('://',APath)<>0 then
    Result:=APath
  else
    Result:=ServiceURL+APath;
  if AQuery<>'' then
    Result:=Result+'?'+AQuery;
  {$IFDEF DEBUGODATASERVICE}DoLog('Exiting TODataService.ComposeURL(%s,%s) : %s',[ APath,AQuery,Result]);{$ENDIF}
end;

function TODataService.ArrayServiceCall(const AURL: String;
  AElementClass: TODataObjectClass): TODataArrayResult;

Var
  D : TJSONData;
  A : TJSONArray;
  R : String;
  I : integer;

begin
  {$IFDEF DEBUGODATASERVICE}DoLog('Entering TODataService.ArrayServiceCall(%s,%s)',[AURL,AElementClass.ClassName]);{$ENDIF}
  SetLength(Result.Data,0);
  R:=ServiceCall('GET',AURL,'');
  if (R<>'') then
    begin
//    Writeln('Response :',R);
    D:=GetJSON(R);
    try
      // Writeln('JSON : ');
      // Writeln(D.FormatJSON());
      // Writeln('Processing');
      If (D.JSONType=jtArray) then
        A:=D as TJSONArray
      else If (D.JSONType=jtObject) then
        begin
        A:=(D as TJSONObject).Get('value',TJSONArray(Nil));
        Result.NextLink:=(D as TJSONObject).Get('@odata.nextLink','');
        // Writeln('NextLink detected : ',Result.NextLink);
        end
      else
        A:=Nil;
      if Assigned(A) then
        begin
        SetLength(Result.Data,A.Count);
        For I:=0 to A.Count-1 do
          begin
          Result.Data[I]:=AElementClass.Create;
          try
            Result.Data[i].LoadFromJSON(A.Objects[i]);
          except
            FreeAndNil(Result.Data[i]);
            Raise;
          end
          end;
        end;
      finally
      D.Free;
    end;
    end;
  {$IFDEF DEBUGODATASERVICE}DoLog('Exiting TODataService.ArrayServiceCall(%s,%s)',[AURL,AElementClass.ClassName]);{$ENDIF}
end;

function TODataService.SingleServiceCall(const AMethod, APath, AQuery,
  AInput: String; AObjectClass: TODataObjectClass): TODataObject;

Var
  D : TJSONData;
  S : String;
  C : TODataObjectClass;
  I,R : TStringStream;
  CT : String;

begin
  C:=Nil;
  D:=Nil;
  I:=Nil;
  R:=TStringStream.Create('');
  try
    if (AInput<>'') then
      begin
      I:=TStringStream.Create(AInput);
      if ODataRequestHeaders.ContentType='' then
        ODataRequestHeaders.ContentType:='application/json';
      end;
    ServiceCall(AMethod,APath,AQuery,I,R);
    if R.Size>0 then
      begin
      R.Position:=0;
      D:=GetJSON(R);
      if Assigned(D) and (D.JSONType=jtObject) then
        begin
        S:=TJSONObject(D).Get('kind','');
        if (S<>'') then
          C:=Service.GetEntityClass(s);
        end;
      if C=Nil then
        C:=AObjectClass;
      Result:=C.Create;
      try
        Result.LoadFromJSON(D as TJSONObject);
      except
        FreeAndNil(Result);
        Raise;
      end;
      end;
  finally
    I.Free;
    D.Free;
    R.Free;
  end;
end;


function TODataService.SingleServiceCall(const APath, AQuery: String;
  AObjectClass: TODataObjectClass): TODataObject;

begin
  Result:=SingleServiceCall('GET',APath,AQuery,'',AObjectClass);
end;

function TODataService.GetMulti(const AURL: String; AClass: TODataObjectClass;
  FetchAll: Boolean; out NextLink: String): TODataObjectArray;
Var
  R : TODataArrayResult;
  L,I : integer;
  EC : TODataObjectClass;

begin
  SetLength(Result,0);
  R.NextLink:='';
  EC:=AClass;
  Repeat
    if (R.NextLink<>'') then
      R:=ArrayServiceCall(R.NextLink,EC)
    else
      R:=ArrayServiceCall(AURL,EC);
    L:=Length(Result);
    if l=0 then
      Result:=TODataObjectArray(R.Data)
    else
      begin
      SetLength(Result,L+Length(R.Data));
      For I:=0 to Length(R.Data)-1 do
        begin
        Result[L+I]:=TODataObject(R.Data[i]);
        R.Data[i]:=Nil;
        end;
      end;
  until (Not FetchAll) or (R.NextLink='');
end;

function TODataService.GetMulti(const APath, AQuery: String;
  AClass: TODataObjectClass; FetchAll: Boolean; out NextLink: String
  ): TODataObjectArray;

begin
  Result:=GetMulti(ComposeURL(APath,AQuery),AClass,FetchAll,NextLink);
end;


function TODataService.CreateEntityContainer(AClass: TODataEntityContainerClass
  ): TODataEntityContainer;
begin
  Result:=AClass.Create(Self);
  Result.SetService(Self);
end;

function TODataService.CreateEntityContainer(const EntityContainer: String
  ): TODataEntityContainer;
begin
  Result:=CreateEntityContainer(ODataFactory.GetEntityContainerClass(EntityContainer));
end;

function TODataService.GetEntityClass(const Entity: String): TODataEntityClass;
begin
  Result:=ODataFactory.GetEntityClass(Entity);
end;

{ ---------------------------------------------------------------------
  TODataEntity
  ---------------------------------------------------------------------}

Class Function TODataEntity.EntityName: String;
begin
  Result:=ClassName;
  If (Result<>'') and (Result[1]='T') then
    System.Delete(Result,1,1);
end;

Function TODataEntity.BaseURL(AService : TODataService): String;

Var
  D : TJSONData;
  K : String;

begin
  Result:='';
  D:=ODataAnnotationValues['@odata.id'];
  If Assigned(D) then
    Result:=D.AsString;
  if Result<>'' then
    Exit;
  if assigned(AService) then
    begin
    if (FBasePath<>'') then
      K:=FBasePath
    else
      begin
      K:=KeyAsURLPart;
      if (K<>'') then
        K:='('+K+')';
      K:=EntityName+K;
      end;
    if Pos('://',K)=0 then
      Result:=IncludeHTTPPathDelimiter(AService.ServiceURL)+K
    else
      Result:=K;
    end
  else
    Raise EOData.Createfmt('Cannot construct base URL for %s without Service',[EntityName]);
end;

Function TODataEntity.KeyAsURLPart: string;
begin
  Result:=''
end;


Function TODataEntity.CreateContainedEntitySet(AService: TODataService;
  APath: String; AClass: TODataEntitySetClass): TODataEntitySet;
begin
  Result:=AClass.Create(AService);
  Result.Service:=AService;
  Result.ContainedPath:=APath;
  Result.ContainerURL:=BaseURL(AService);
//  Writeln('Created contained entityset ',Result.ClassName,' with containerURL : ',Result.ContainerURL, 'and baseURL : ',Result.GetBaseURL);
{  if (BasePath<>'') then
    Result.BasePath:=BasePath+'/'+APath;}
end;

Function TODataEntity.GetContainedSingleTon(AService: TODataService;
  APath: String; AClass: TODataEntityClass): TODataEntity;

Var
  URL : String;

begin
  URL:=IncludeHTTPPathDelimiter(BaseURL(AService))+APath;
  Result:=TODataEntity(AService.SingleServiceCall(URL,'',AClass));
  if (BasePath<>'') then
    Result.BasePath:=BasePath+'/'+APath;
//  Writeln('Created contained singleton ',Result.ClassName,' with basepath : ',Result.BasePath);
end;

Function TODataEntity.DoPost(AService: TODataservice): TODataEntity;
begin
  Result:=TODataEntity(AService.ServiceCall('POST',BaseURL(AService),'',Self,TODataEntityClass(Self.ClassType)));
end;

Function TODataEntity.DoPut(AService: TODataservice): TODataEntity;
begin
  ClearPropertyChanges;
  Result:=TODataEntity(AService.ServiceCall('PUT',BaseURL(AService),'',Self,TODataEntityClass(Self.ClassType)));
end;

Function TODataEntity.DoPatch(AService: TODataservice): TODataEntity;
begin
  Result:=TODataEntity(AService.ServiceCall('PATCH',BaseURL(AService),'',Self,TODataEntityClass(Self.ClassType)));
end;

Function TODataEntity.GetMulti(AService: TODataservice; Const AURL: String;
  AType: TODataEntityClass; FetchAll: Boolean; Out NextLink: String
  ): TODataEntityArray;

begin
  Result:=TODataEntityArray(AService.GetMulti(AURL,'',AType,FetchAll,NextLink));
end;

Procedure TODataEntity.DoGetStream(AService: TODataservice;
  AContentType: String; AStream: TStream);

Var
  R : String;

begin
  R:=BaseURL(AService)+'/$value';
  AService.GetStream(R,'',AContentType,AStream);
end;

Procedure TODataEntity.DoSetStream(AService: TODataservice;
  AContentType: String; AStream: TStream);
Var
  R : String;

begin
  R:=BaseURL(AService)+'/$value';
  AService.SetStream(R,'',AContentType,AStream);
end;

Function TODataEntity.Delete(AService: TODataService): Boolean;

Var
  IM,S : String;

begin
  IM:=AService.ODataRequestHeaders.IfMatch;
  try
    S:=ODataAnnotationValues['@odata.etag'].AsString;
    if (S<>'') then
      AService.ODataRequestHeaders.IfMatch:=S
    else
      AService.ODataRequestHeaders.IfMatch:='*';
    Result:=AService.ServiceCall('DELETE',BaseURL(AService),'','')='';
  finally
    AService.ODataRequestHeaders.IfMatch:=IM;
  end;
end;

Procedure TODataEntity.Post(AService: TODataservice);
begin
  // By default, OData does not return the changed object.
  StopRecordPropertyChanges;
  AService.ServiceCall('POST',BaseURL(AService),'',Self,Nil);
end;

Procedure TODataEntity.Put(AService: TODataservice);
begin
  StopRecordPropertyChanges;
  AService.ServiceCall('PUT',BaseURL(AService),'',Self,Nil);
end;

Procedure TODataEntity.Patch(AService: TODataservice);
begin
  AService.ServiceCall('PATCH',BaseURL(AService),'',Self,Nil);
end;


finalization
  FreeAndNil(TODataServiceFactory.DefaultFactory);
end.


