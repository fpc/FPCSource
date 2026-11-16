unit directorlistform;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, Forms, Controls, Grids, StdCtrls, ExtCtrls,
  filmmodels;

type

  { TDirectorListForm }

  TDirectorListForm = class(TForm)
    BtnAdd: TButton;
    BtnClose: TButton;
    BtnDelete: TButton;
    BtnEdit: TButton;
    ButtonPanel: TPanel;
    DirectorGrid: TStringGrid;
    procedure BtnAddClick(Sender: TObject);
    procedure BtnEditClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure RefreshDirectorGrid;
    function GetSelectedDirectorID: Integer;
  end;

var
  DirectorListFormInstance: TDirectorListForm;

implementation

uses
  contnrs, Dialogs,
  Opf.Manager, opfdatamodule;

{$R *.lfm}

{ TDirectorListForm }

procedure TDirectorListForm.FormShow(Sender: TObject);
begin
  RefreshDirectorGrid;
end;

function TDirectorListForm.GetSelectedDirectorID: Integer;
begin
  if (DirectorGrid.Row < 1) or (DirectorGrid.Row >= DirectorGrid.RowCount) then
    Result := -1
  else
    Result := StrToIntDef(DirectorGrid.Cells[0, DirectorGrid.Row], -1);
end;

procedure TDirectorListForm.RefreshDirectorGrid;
var
  List: TObjectList;
  Dir: TDirector;
  I: Integer;
begin
  List := TObjectList.Create(True);
  try
    OPManager.Load(List, TDirector);
    DirectorGrid.RowCount := List.Count + 1;
    for I := 0 to List.Count - 1 do
    begin
      Dir := TDirector(List[I]);
      DirectorGrid.Cells[0, I + 1] := IntToStr(Dir.ID);
      DirectorGrid.Cells[1, I + 1] := Dir.Name;
      if Dir.CountryStored then
        DirectorGrid.Cells[2, I + 1] := Dir.Country
      else
        DirectorGrid.Cells[2, I + 1] := '';
    end;
  finally
    List.Free;
  end;
end;

procedure TDirectorListForm.BtnAddClick(Sender: TObject);
var
  Dir: TDirector;
  AName, ACountry: String;
begin
  AName := '';
  ACountry := '';
  if not InputQuery('Add Director', 'Name:', AName) then Exit;
  if AName = '' then Exit;
  InputQuery('Add Director', 'Country (optional):', ACountry);
  Dir := TDirector.Create;
  try
    Dir.Name := AName;
    if ACountry <> '' then
    begin
      Dir.Country := ACountry;
      Dir.CountryStored := True;
    end;
    OPManager.Save(Dir, opsNew);
    RefreshDirectorGrid;
  finally
    Dir.Free;
  end;
end;

procedure TDirectorListForm.BtnEditClick(Sender: TObject);
var
  Dir: TDirector;
  AName, ACountry: String;
  ID: Integer;
begin
  ID := GetSelectedDirectorID;
  if ID < 0 then
  begin
    ShowMessage('No director selected.');
    Exit;
  end;
  Dir := TDirector.Create;
  try
    Dir.ID := ID;
    OPManager.Load(Dir);
    AName := Dir.Name;
    ACountry := Dir.Country;
    if not InputQuery('Edit Director', 'Name:', AName) then Exit;
    if AName = '' then Exit;
    InputQuery('Edit Director', 'Country (optional):', ACountry);
    Dir.Name := AName;
    if ACountry <> '' then
    begin
      Dir.Country := ACountry;
      Dir.CountryStored := True;
    end
    else
      Dir.CountryStored := False;
    OPManager.Save(Dir, opsUpdate);
    RefreshDirectorGrid;
  finally
    Dir.Free;
  end;
end;

procedure TDirectorListForm.BtnDeleteClick(Sender: TObject);
var
  Dir: TDirector;
  ID: Integer;
begin
  ID := GetSelectedDirectorID;
  if ID < 0 then
  begin
    ShowMessage('No director selected.');
    Exit;
  end;
  Dir := TDirector.Create;
  try
    Dir.ID := ID;
    OPManager.Load(Dir);
    if MessageDlg('Delete Director',
      'Are you sure you want to delete "' + Dir.Name + '"?' + LineEnding +
      'Junction rows will be removed but films will be preserved.',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      OPManager.Delete(Dir);
      RefreshDirectorGrid;
    end;
  finally
    Dir.Free;
  end;
end;

end.
