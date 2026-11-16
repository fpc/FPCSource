unit reportform;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, contnrs, Forms, Controls, Grids, StdCtrls, ExtCtrls,
  filmmodels;

type

  { TReportForm }

  TReportForm = class(TForm)
    BtnClose: TButton;
    ButtonPanel: TPanel;
    ReportGrid: TStringGrid;
    procedure FormShow(Sender: TObject);
  private
    FList: TObjectList;
  public
    procedure LoadReport(const aTitle: String; aList: TObjectList);
  end;

var
  ReportFormInstance: TReportForm;

implementation

{$R *.lfm}

{ TReportForm }

procedure TReportForm.FormShow(Sender: TObject);
var
  Row: TReportRow;
  I: Integer;
begin
  if FList = nil then Exit;
  ReportGrid.RowCount := FList.Count + 1;
  for I := 0 to FList.Count - 1 do
  begin
    Row := TReportRow(FList[I]);
    ReportGrid.Cells[0, I + 1] := Row.Name;
    ReportGrid.Cells[1, I + 1] := IntToStr(Row.FilmCount);
    ReportGrid.Cells[2, I + 1] := Row.FilmTitles;
  end;
end;

procedure TReportForm.LoadReport(const aTitle: String; aList: TObjectList);
begin
  Caption := aTitle;
  FList := aList;
end;

end.
