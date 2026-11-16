{ rest.base - Generic REST CRUD handler for OPF entities
  =======================================================
  Eliminates boilerplate by parameterizing the 5 CRUD operations
  via callbacks for entity-specific logic (factory, ID setter,
  JSON serialization/deserialization).

  Each entity unit provides thin callback wrappers and a single
  Register call. The handler does the rest: routing, error handling,
  persistence state management, JSON envelope wrapping.
}
unit rest.base;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, contnrs, fpjson, httpdefs, httproute,
  Opf.Manager;

type
  { Callback types - each entity unit provides these }
  TEntityFactory = function: TObject;
  TSetEntityID = procedure(aObj: TObject; aID: Integer);
  TEntityToJSON = function(aObj: TObject): TJSONObject;
  TJSONToEntity = procedure(aJSON: TJSONObject; aObj: TObject);
  TAfterLoad = procedure(aObj: TObject);

  { TRESTEntityHandler - generic CRUD handler for one entity type.
    Owns the 5 route handler methods and registers them on HTTPRouter. }

  TRESTEntityHandler = class
  private
    FEntityClass: TClass;
    FEntityName: String;
    FBasePath: String;
    FFactory: TEntityFactory;
    FSetID: TSetEntityID;
    FToJSON: TEntityToJSON;
    FFromJSON: TJSONToEntity;
    FAfterLoad: TAfterLoad;
  public
    constructor Create(const aBasePath, aEntityName: String;
      aClass: TClass;
      aFactory: TEntityFactory;
      aSetID: TSetEntityID;
      aToJSON: TEntityToJSON;
      aFromJSON: TJSONToEntity;
      aAfterLoad: TAfterLoad);
    procedure HandleList(ARequest: TRequest; AResponse: TResponse);
    procedure HandleGet(ARequest: TRequest; AResponse: TResponse);
    procedure HandleCreate(ARequest: TRequest; AResponse: TResponse);
    procedure HandleUpdate(ARequest: TRequest; AResponse: TResponse);
    procedure HandleDelete(ARequest: TRequest; AResponse: TResponse);
    procedure RegisterRoutes;
  end;

{ Response helpers - reduce 3-line response pattern to 1 call }
procedure SendJSON(AResponse: TResponse; aCode: Integer; const aJSON: String);
procedure SendError(AResponse: TResponse; aCode: Integer; const aMessage: String);
procedure SendData(AResponse: TResponse; aCode: Integer; aData: TJSONData);

implementation

uses
  jsonparser, rest.setup;

{ --- Response helpers --- }

procedure SendJSON(AResponse: TResponse; aCode: Integer; const aJSON: String);
begin
  AResponse.Code := aCode;
  AResponse.ContentType := 'application/json';
  AResponse.Content := aJSON;
end;

procedure SendError(AResponse: TResponse; aCode: Integer; const aMessage: String);
begin
  SendJSON(AResponse, aCode, ErrorJSON(aMessage));
end;

procedure SendData(AResponse: TResponse; aCode: Integer; aData: TJSONData);
begin
  SendJSON(AResponse, aCode, EnvelopeJSON(aData));
end;

{ --- TRESTEntityHandler --- }

constructor TRESTEntityHandler.Create(const aBasePath, aEntityName: String;
  aClass: TClass;
  aFactory: TEntityFactory;
  aSetID: TSetEntityID;
  aToJSON: TEntityToJSON;
  aFromJSON: TJSONToEntity;
  aAfterLoad: TAfterLoad);
begin
  inherited Create;
  FBasePath := aBasePath;
  FEntityName := aEntityName;
  FEntityClass := aClass;
  FFactory := aFactory;
  FSetID := aSetID;
  FToJSON := aToJSON;
  FFromJSON := aFromJSON;
  FAfterLoad := aAfterLoad;
end;

procedure TRESTEntityHandler.HandleList(ARequest: TRequest; AResponse: TResponse);
var
  List: TObjectList;
  Arr: TJSONArray;
begin
  List := TObjectList.Create(True);
  try
    OPManager.Load(List, FEntityClass);
    Arr := ListToJSONArray(List, FEntityClass);
    try
      SendData(AResponse, 200, Arr);
    finally
      Arr.Free;
    end;
  finally
    List.Free;
  end;
end;

procedure TRESTEntityHandler.HandleGet(ARequest: TRequest; AResponse: TResponse);
var
  Obj: TObject;
  ID: Integer;
  J: TJSONObject;
begin
  ID := StrToIntDef(ARequest.RouteParams['id'], 0);
  if ID = 0 then
  begin
    SendError(AResponse, 400, 'Invalid ' + FEntityName + ' ID');
    Exit;
  end;
  Obj := FFactory();
  try
    FSetID(Obj, ID);
    try
      OPManager.Load(Obj);
    except
      SendError(AResponse, 404, FEntityName + ' not found');
      Exit;
    end;
    if Assigned(FAfterLoad) then
      FAfterLoad(Obj);
    J := FToJSON(Obj);
    try
      SendData(AResponse, 200, J);
    finally
      J.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TRESTEntityHandler.HandleCreate(ARequest: TRequest; AResponse: TResponse);
var
  Obj: TObject;
  IPS: IOPPersistenceState;
  ReqJSON, RespJSON: TJSONObject;
  Parser: TJSONParser;
begin
  if ARequest.Content = '' then
  begin
    SendError(AResponse, 400, 'Request body is empty');
    Exit;
  end;
  Obj := FFactory();
  try
    Parser := TJSONParser.Create(ARequest.Content);
    try
      ReqJSON := Parser.Parse as TJSONObject;
    finally
      Parser.Free;
    end;
    try
      FFromJSON(ReqJSON, Obj);
    finally
      ReqJSON.Free;
    end;
    if Obj.GetInterface('{84DE4D96-0D52-0DC1-3129-1C576A55493A}', IPS) then
      IPS.SetPersistenceState(opsNew);
    OPManager.Save(Obj);
    RespJSON := FToJSON(Obj);
    try
      SendData(AResponse, 201, RespJSON);
    finally
      RespJSON.Free;
    end;
  except
    on E: Exception do
    begin
      Obj.Free;
      SendError(AResponse, 400, E.Message);
      Exit;
    end;
  end;
  Obj.Free;
end;

procedure TRESTEntityHandler.HandleUpdate(ARequest: TRequest; AResponse: TResponse);
var
  Obj: TObject;
  IPS: IOPPersistenceState;
  ID: Integer;
  ReqJSON, RespJSON: TJSONObject;
  Parser: TJSONParser;
begin
  ID := StrToIntDef(ARequest.RouteParams['id'], 0);
  if ID = 0 then
  begin
    SendError(AResponse, 400, 'Invalid ' + FEntityName + ' ID');
    Exit;
  end;
  Obj := FFactory();
  try
    FSetID(Obj, ID);
    try
      OPManager.Load(Obj);
    except
      SendError(AResponse, 404, FEntityName + ' not found');
      Obj.Free;
      Exit;
    end;
    Parser := TJSONParser.Create(ARequest.Content);
    try
      ReqJSON := Parser.Parse as TJSONObject;
    finally
      Parser.Free;
    end;
    try
      FFromJSON(ReqJSON, Obj);
    finally
      ReqJSON.Free;
    end;
    if Obj.GetInterface('{84DE4D96-0D52-0DC1-3129-1C576A55493A}', IPS) then
      IPS.SetPersistenceState(opsUpdate);
    OPManager.Save(Obj);
    RespJSON := FToJSON(Obj);
    try
      SendData(AResponse, 200, RespJSON);
    finally
      RespJSON.Free;
    end;
  except
    on E: Exception do
    begin
      Obj.Free;
      SendError(AResponse, 500, E.Message);
      Exit;
    end;
  end;
  Obj.Free;
end;

procedure TRESTEntityHandler.HandleDelete(ARequest: TRequest; AResponse: TResponse);
var
  Obj: TObject;
  ID: Integer;
begin
  ID := StrToIntDef(ARequest.RouteParams['id'], 0);
  if ID = 0 then
  begin
    SendError(AResponse, 400, 'Invalid ' + FEntityName + ' ID');
    Exit;
  end;
  Obj := FFactory();
  try
    FSetID(Obj, ID);
    try
      OPManager.Load(Obj);
    except
      SendError(AResponse, 404, FEntityName + ' not found');
      Exit;
    end;
    OPManager.Delete(Obj);
    AResponse.Code := 204;
  finally
    Obj.Free;
  end;
end;

procedure TRESTEntityHandler.RegisterRoutes;
begin
  HTTPRouter.RegisterRoute(FBasePath, rmGet, @HandleList);
  HTTPRouter.RegisterRoute(FBasePath + '/:id', rmGet, @HandleGet);
  HTTPRouter.RegisterRoute(FBasePath, rmPost, @HandleCreate);
  HTTPRouter.RegisterRoute(FBasePath + '/:id', rmPut, @HandleUpdate);
  HTTPRouter.RegisterRoute(FBasePath + '/:id', rmDelete, @HandleDelete);
end;

end.
