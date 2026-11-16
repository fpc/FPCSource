unit actorlistform;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, Forms, Controls, Grids, StdCtrls, ExtCtrls,
  filmmodels;

type

  { TActorListForm }

  TActorListForm = class(TForm)
    BtnAdd: TButton;
    BtnClose: TButton;
    BtnDelete: TButton;
    BtnEdit: TButton;
    ButtonPanel: TPanel;
    ActorGrid: TStringGrid;
    procedure BtnAddClick(Sender: TObject);
    procedure BtnEditClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure RefreshActorGrid;
    function GetSelectedActorID: Integer;
  end;

var
  ActorListFormInstance: TActorListForm;

implementation

uses
  contnrs, Dialogs,
  Opf.Manager, opfdatamodule;

{$R *.lfm}

{ TActorListForm }

procedure TActorListForm.FormShow(Sender: TObject);
begin
  RefreshActorGrid;
end;

function TActorListForm.GetSelectedActorID: Integer;
begin
  if (ActorGrid.Row < 1) or (ActorGrid.Row >= ActorGrid.RowCount) then
    Result := -1
  else
    Result := StrToIntDef(ActorGrid.Cells[0, ActorGrid.Row], -1);
end;

procedure TActorListForm.RefreshActorGrid;
var
  List: TObjectList;
  Act: TActor;
  I: Integer;
begin
  List := TObjectList.Create(True);
  try
    OPManager.Load(List, TActor);
    ActorGrid.RowCount := List.Count + 1;
    for I := 0 to List.Count - 1 do
    begin
      Act := TActor(List[I]);
      ActorGrid.Cells[0, I + 1] := IntToStr(Act.ID);
      ActorGrid.Cells[1, I + 1] := Act.Name;
      if Act.BirthYearStored then
        ActorGrid.Cells[2, I + 1] := IntToStr(Act.BirthYear)
      else
        ActorGrid.Cells[2, I + 1] := '';
    end;
  finally
    List.Free;
  end;
end;

procedure TActorListForm.BtnAddClick(Sender: TObject);
var
  Act: TActor;
  AName, ABirthYear: String;
begin
  AName := '';
  ABirthYear := '';
  if not InputQuery('Add Actor', 'Name:', AName) then Exit;
  if AName = '' then Exit;
  InputQuery('Add Actor', 'Birth Year (optional):', ABirthYear);
  Act := TActor.Create;
  try
    Act.Name := AName;
    if ABirthYear <> '' then
    begin
      Act.BirthYear := StrToIntDef(ABirthYear, 0);
      Act.BirthYearStored := True;
    end;
    OPManager.Save(Act, opsNew);
    RefreshActorGrid;
  finally
    Act.Free;
  end;
end;

procedure TActorListForm.BtnEditClick(Sender: TObject);
var
  Act: TActor;
  AName, ABirthYear: String;
  ID: Integer;
begin
  ID := GetSelectedActorID;
  if ID < 0 then
  begin
    ShowMessage('No actor selected.');
    Exit;
  end;
  Act := TActor.Create;
  try
    Act.ID := ID;
    OPManager.Load(Act);
    AName := Act.Name;
    if Act.BirthYearStored then
      ABirthYear := IntToStr(Act.BirthYear)
    else
      ABirthYear := '';
    if not InputQuery('Edit Actor', 'Name:', AName) then Exit;
    if AName = '' then Exit;
    InputQuery('Edit Actor', 'Birth Year (optional):', ABirthYear);
    Act.Name := AName;
    if ABirthYear <> '' then
    begin
      Act.BirthYear := StrToIntDef(ABirthYear, 0);
      Act.BirthYearStored := True;
    end
    else
      Act.BirthYearStored := False;
    OPManager.Save(Act, opsUpdate);
    RefreshActorGrid;
  finally
    Act.Free;
  end;
end;

procedure TActorListForm.BtnDeleteClick(Sender: TObject);
var
  Act: TActor;
  ID: Integer;
begin
  ID := GetSelectedActorID;
  if ID < 0 then
  begin
    ShowMessage('No actor selected.');
    Exit;
  end;
  Act := TActor.Create;
  try
    Act.ID := ID;
    OPManager.Load(Act);
    if MessageDlg('Delete Actor',
      'Are you sure you want to delete "' + Act.Name + '"?' + LineEnding +
      'Junction rows will be removed but films will be preserved.',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      OPManager.Delete(Act);
      RefreshActorGrid;
    end;
  finally
    Act.Free;
  end;
end;

end.
