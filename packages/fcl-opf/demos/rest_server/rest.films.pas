{ rest.films - Film CRUD route handlers
  ======================================
  Uses TRESTEntityHandler from rest.base for standard CRUD.
  Overrides HandleList for search support and uses AfterLoad
  to populate relations on GET /films/:id.
}
unit rest.films;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  httproute;

procedure RegisterFilmRoutes;

implementation

uses
  Classes, SysUtils, contnrs, fpjson, httpdefs,
  Opf.Manager,
  filmmodels,
  rest.setup,
  rest.base;

{  Callbacks for TRESTEntityHandler  }

function CreateFilm: TObject;
begin
  Result := TFilm.Create;
end;

procedure SetFilmID(aObj: TObject; aID: Integer);
begin
  TFilm(aObj).ID := aID;
end;

function FilmToJSONCb(aObj: TObject): TJSONObject;
begin
  { Always include relations - lists are empty if not loaded, populated after AfterLoad }
  Result := FilmToJSON(TFilm(aObj), True);
end;

procedure JSONToFilmCb(aJSON: TJSONObject; aObj: TObject);
begin
  JSONToFilm(aJSON, TFilm(aObj));
end;

procedure AfterLoadFilm(aObj: TObject);
begin
  OPManager.LoadAssociations(aObj, 'DirectorList');
  OPManager.LoadAssociations(aObj, 'ActorList');
  OPManager.LoadAssociations(aObj, 'GenreList');
  OPManager.LoadChildren(aObj, 'LoanList');
end;

{  Custom list handler with search support  }

var
  FilmHandler: TRESTEntityHandler;

procedure HandleListFilmsWithSearch(ARequest: TRequest; AResponse: TResponse);
var
  Films: TObjectList;
  Arr: TJSONArray;
  Search: String;
begin
  Search := ARequest.QueryFields.Values['search'];
  if Search <> '' then
  begin
    Films := TObjectList.Create(True);
    try
      OPManager.Load(Films, TFilm, 'Title LIKE ''%' + Search + '%''');
      Arr := ListToJSONArray(Films, TFilm);
      try
        SendData(AResponse, 200, Arr);
      finally
        Arr.Free;
      end;
    finally
      Films.Free;
    end;
  end
  else
    FilmHandler.HandleList(ARequest, AResponse);
end;

{  Route registration  }

procedure RegisterFilmRoutes;
begin
  FilmHandler := TRESTEntityHandler.Create('/films', 'Film',
    TFilm, @CreateFilm, @SetFilmID, @FilmToJSONCb, @JSONToFilmCb,
    @AfterLoadFilm);
  { Register individual routes - override list for search support }
  HTTPRouter.RegisterRoute('/films', rmGet, @HandleListFilmsWithSearch);
  HTTPRouter.RegisterRoute('/films/:id', rmGet, @FilmHandler.HandleGet);
  HTTPRouter.RegisterRoute('/films', rmPost, @FilmHandler.HandleCreate);
  HTTPRouter.RegisterRoute('/films/:id', rmPut, @FilmHandler.HandleUpdate);
  HTTPRouter.RegisterRoute('/films/:id', rmDelete, @FilmHandler.HandleDelete);
end;

end.
