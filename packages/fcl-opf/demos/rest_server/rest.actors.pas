{ rest.actors - Actor CRUD route handlers
  ========================================
  Thin callback wrappers + RegisterRoutes via TRESTEntityHandler.
}
unit rest.actors;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

procedure RegisterActorRoutes;

implementation

uses
  fpjson,
  filmmodels,
  rest.setup,
  rest.base;

function CreateActor: TObject;
begin
  Result := TActor.Create;
end;

procedure SetActorID(aObj: TObject; aID: Integer);
begin
  TActor(aObj).ID := aID;
end;

function ActToJSON(aObj: TObject): TJSONObject;
begin
  Result := ActorToJSON(TActor(aObj));
end;

procedure JSONToAct(aJSON: TJSONObject; aObj: TObject);
begin
  JSONToActor(aJSON, TActor(aObj));
end;

procedure RegisterActorRoutes;
begin
  TRESTEntityHandler.Create('/actors', 'Actor',
    TActor, @CreateActor, @SetActorID, @ActToJSON, @JSONToAct,
    nil).RegisterRoutes;
end;

end.
