{ rest.genres - Genre CRUD route handlers
  ========================================
  Thin callback wrappers + RegisterRoutes via TRESTEntityHandler.
}
unit rest.genres;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

procedure RegisterGenreRoutes;

implementation

uses
  fpjson,
  filmmodels,
  rest.setup,
  rest.base;

function CreateGenre: TObject;
begin
  Result := TGenre.Create;
end;

procedure SetGenreID(aObj: TObject; aID: Integer);
begin
  TGenre(aObj).ID := aID;
end;

function GenToJSON(aObj: TObject): TJSONObject;
begin
  Result := GenreToJSON(TGenre(aObj));
end;

procedure JSONToGen(aJSON: TJSONObject; aObj: TObject);
begin
  JSONToGenre(aJSON, TGenre(aObj));
end;

procedure RegisterGenreRoutes;
begin
  TRESTEntityHandler.Create('/genres', 'Genre',
    TGenre, @CreateGenre, @SetGenreID, @GenToJSON, @JSONToGen,
    nil).RegisterRoutes;
end;

end.
