{ rest.associations - M:N association management route handlers
  ==============================================================
  POST/DELETE routes for managing Film-Director, Film-Actor,
  Film-Genre associations. Uses SendData/SendError from rest.base.
}
unit rest.associations;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  httproute;

procedure RegisterAssociationRoutes;

implementation

uses
  Classes, SysUtils, fpjson, httpdefs,
  Opf.Manager,
  filmmodels,
  rest.setup,
  rest.base;

{ Generic association handler - works for any Film <-> Entity M:N }
procedure DoAddAssociation(ARequest: TRequest; AResponse: TResponse;
  aEntityClass: TClass; const aEntityName, aIdParam: String);
var
  Film: TFilm;
  Entity: TObject;
  FilmID, EntityID: Integer;
  Obj: TJSONObject;
begin
  FilmID := StrToIntDef(ARequest.RouteParams['filmId'], 0);
  EntityID := StrToIntDef(ARequest.RouteParams[aIdParam], 0);
  if (FilmID = 0) or (EntityID = 0) then
  begin
    SendError(AResponse, 400, 'Invalid film or ' + aEntityName + ' ID');
    Exit;
  end;
  Film := TFilm.Create;
  try
    Film.ID := FilmID;
    try
      OPManager.Load(Film);
    except
      SendError(AResponse, 404, 'Film not found');
      Exit;
    end;
    if aEntityClass = TDirector then
    begin
      Entity := TDirector.Create;
      TDirector(Entity).ID := EntityID;
    end
    else if aEntityClass = TActor then
    begin
      Entity := TActor.Create;
      TActor(Entity).ID := EntityID;
    end
    else
    begin
      Entity := TGenre.Create;
      TGenre(Entity).ID := EntityID;
    end;
    try
      try
        OPManager.Load(Entity);
      except
        SendError(AResponse, 404, aEntityName + ' not found');
        Exit;
      end;
      OPManager.AddAssociation(Film, Entity);
      Obj := TJSONObject.Create;
      try
        Obj.Add('filmId', FilmID);
        Obj.Add(LowerCase(aEntityName) + 'Id', EntityID);
        SendData(AResponse, 201, Obj);
      finally
        Obj.Free;
      end;
    finally
      Entity.Free;
    end;
  finally
    Film.Free;
  end;
end;

procedure DoRemoveAssociation(ARequest: TRequest; AResponse: TResponse;
  aEntityClass: TClass; const aEntityName, aIdParam: String);
var
  Film: TFilm;
  Entity: TObject;
  FilmID, EntityID: Integer;
begin
  FilmID := StrToIntDef(ARequest.RouteParams['filmId'], 0);
  EntityID := StrToIntDef(ARequest.RouteParams[aIdParam], 0);
  if (FilmID = 0) or (EntityID = 0) then
  begin
    SendError(AResponse, 400, 'Invalid film or ' + aEntityName + ' ID');
    Exit;
  end;
  Film := TFilm.Create;
  try
    Film.ID := FilmID;
    try
      OPManager.Load(Film);
    except
      SendError(AResponse, 404, 'Film not found');
      Exit;
    end;
    if aEntityClass = TDirector then
    begin
      Entity := TDirector.Create;
      TDirector(Entity).ID := EntityID;
    end
    else if aEntityClass = TActor then
    begin
      Entity := TActor.Create;
      TActor(Entity).ID := EntityID;
    end
    else
    begin
      Entity := TGenre.Create;
      TGenre(Entity).ID := EntityID;
    end;
    try
      try
        OPManager.Load(Entity);
      except
        SendError(AResponse, 404, aEntityName + ' not found');
        Exit;
      end;
      OPManager.RemoveAssociation(Film, Entity);
      AResponse.Code := 204;
    finally
      Entity.Free;
    end;
  finally
    Film.Free;
  end;
end;

procedure HandleAddDirector(ARequest: TRequest; AResponse: TResponse);
begin
  DoAddAssociation(ARequest, AResponse, TDirector, 'Director', 'directorId');
end;

procedure HandleRemoveDirector(ARequest: TRequest; AResponse: TResponse);
begin
  DoRemoveAssociation(ARequest, AResponse, TDirector, 'Director', 'directorId');
end;

procedure HandleAddActor(ARequest: TRequest; AResponse: TResponse);
begin
  DoAddAssociation(ARequest, AResponse, TActor, 'Actor', 'actorId');
end;

procedure HandleRemoveActor(ARequest: TRequest; AResponse: TResponse);
begin
  DoRemoveAssociation(ARequest, AResponse, TActor, 'Actor', 'actorId');
end;

procedure HandleAddGenre(ARequest: TRequest; AResponse: TResponse);
begin
  DoAddAssociation(ARequest, AResponse, TGenre, 'Genre', 'genreId');
end;

procedure HandleRemoveGenre(ARequest: TRequest; AResponse: TResponse);
begin
  DoRemoveAssociation(ARequest, AResponse, TGenre, 'Genre', 'genreId');
end;

procedure RegisterAssociationRoutes;
begin
  HTTPRouter.RegisterRoute('/films/:filmId/directors/:directorId', rmPost, @HandleAddDirector);
  HTTPRouter.RegisterRoute('/films/:filmId/directors/:directorId', rmDelete, @HandleRemoveDirector);
  HTTPRouter.RegisterRoute('/films/:filmId/actors/:actorId', rmPost, @HandleAddActor);
  HTTPRouter.RegisterRoute('/films/:filmId/actors/:actorId', rmDelete, @HandleRemoveActor);
  HTTPRouter.RegisterRoute('/films/:filmId/genres/:genreId', rmPost, @HandleAddGenre);
  HTTPRouter.RegisterRoute('/films/:filmId/genres/:genreId', rmDelete, @HandleRemoveGenre);
end;

end.
