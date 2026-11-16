{ rest.directors - Director CRUD route handlers
  ===============================================
  Thin callback wrappers + RegisterRoutes via TRESTEntityHandler.
}
unit rest.directors;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

procedure RegisterDirectorRoutes;

implementation

uses
  fpjson,
  filmmodels,
  rest.setup,
  rest.base;

function CreateDirector: TObject;
begin
  Result := TDirector.Create;
end;

procedure SetDirectorID(aObj: TObject; aID: Integer);
begin
  TDirector(aObj).ID := aID;
end;

function DirToJSON(aObj: TObject): TJSONObject;
begin
  Result := DirectorToJSON(TDirector(aObj));
end;

procedure JSONToDir(aJSON: TJSONObject; aObj: TObject);
begin
  JSONToDirector(aJSON, TDirector(aObj));
end;

procedure RegisterDirectorRoutes;
begin
  TRESTEntityHandler.Create('/directors', 'Director',
    TDirector, @CreateDirector, @SetDirectorID, @DirToJSON, @JSONToDir,
    nil).RegisterRoutes;
end;

end.
