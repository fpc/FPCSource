unit genrelistform;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, Forms, Controls, Grids, StdCtrls, ExtCtrls,
  filmmodels;

type

  { TGenreListForm }

  TGenreListForm = class(TForm)
    BtnAdd: TButton;
    BtnClose: TButton;
    BtnDelete: TButton;
    BtnEdit: TButton;
    ButtonPanel: TPanel;
    GenreGrid: TStringGrid;
    procedure BtnAddClick(Sender: TObject);
    procedure BtnEditClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure RefreshGenreGrid;
    function GetSelectedGenreID: Integer;
  end;

var
  GenreListFormInstance: TGenreListForm;

implementation

uses
  contnrs, Dialogs,
  Opf.Manager, opfdatamodule;

{$R *.lfm}

{ TGenreListForm }

procedure TGenreListForm.FormShow(Sender: TObject);
begin
  RefreshGenreGrid;
end;

function TGenreListForm.GetSelectedGenreID: Integer;
begin
  if (GenreGrid.Row < 1) or (GenreGrid.Row >= GenreGrid.RowCount) then
    Result := -1
  else
    Result := StrToIntDef(GenreGrid.Cells[0, GenreGrid.Row], -1);
end;

procedure TGenreListForm.RefreshGenreGrid;
var
  List: TObjectList;
  Genre: TGenre;
  I: Integer;
begin
  List := TObjectList.Create(True);
  try
    OPManager.Load(List, TGenre);
    GenreGrid.RowCount := List.Count + 1;
    for I := 0 to List.Count - 1 do
    begin
      Genre := TGenre(List[I]);
      GenreGrid.Cells[0, I + 1] := IntToStr(Genre.ID);
      GenreGrid.Cells[1, I + 1] := Genre.Name;
    end;
  finally
    List.Free;
  end;
end;

procedure TGenreListForm.BtnAddClick(Sender: TObject);
var
  Genre: TGenre;
  AName: String;
begin
  AName := '';
  if not InputQuery('Add Genre', 'Name:', AName) then Exit;
  if AName = '' then Exit;
  Genre := TGenre.Create;
  try
    Genre.Name := AName;
    OPManager.Save(Genre, opsNew);
    RefreshGenreGrid;
  finally
    Genre.Free;
  end;
end;

procedure TGenreListForm.BtnEditClick(Sender: TObject);
var
  Genre: TGenre;
  AName: String;
  ID: Integer;
begin
  ID := GetSelectedGenreID;
  if ID < 0 then
  begin
    ShowMessage('No genre selected.');
    Exit;
  end;
  Genre := TGenre.Create;
  try
    Genre.ID := ID;
    OPManager.Load(Genre);
    AName := Genre.Name;
    if not InputQuery('Edit Genre', 'Name:', AName) then Exit;
    if AName = '' then Exit;
    Genre.Name := AName;
    OPManager.Save(Genre, opsUpdate);
    RefreshGenreGrid;
  finally
    Genre.Free;
  end;
end;

procedure TGenreListForm.BtnDeleteClick(Sender: TObject);
var
  Genre: TGenre;
  ID: Integer;
begin
  ID := GetSelectedGenreID;
  if ID < 0 then
  begin
    ShowMessage('No genre selected.');
    Exit;
  end;
  Genre := TGenre.Create;
  try
    Genre.ID := ID;
    OPManager.Load(Genre);
    if MessageDlg('Delete Genre',
      'Are you sure you want to delete "' + Genre.Name + '"?' + LineEnding +
      'Junction rows will be removed but films will be preserved.',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      OPManager.Delete(Genre);
      RefreshGenreGrid;
    end;
  finally
    Genre.Free;
  end;
end;

end.
