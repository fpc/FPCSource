unit fpfixedexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, fpDBExport;
  
  { TFixedLengthExportFieldItem }

Type
  TFixedLengthExportFieldItem = Class(TExportFieldItem)
  private
    FWidth: Integer;
    FAlignField: TAlignField;
  Public
    Procedure Assign(Source : TPersistent); override;
  Published
    Property Width : Integer Read FWidth Write FWidth;
    Property AlignField: TAlignField Read FAlignField write FAlignField;
  end;

  { TCustomFixedLengthExporter }

  TCustomFixedLengthExporter = Class(TCustomFileExporter)
  Private
    FCurrentRow : String;
    procedure OutputRow(const ARow: String);
  Protected
    Procedure BuildDefaultFieldMap(AMap : TExportFields); override;
    Function  CreateExportFields : TExportFields; override;
    Procedure DoBeforeExecute; override;
    Procedure DoAfterExecute; override;
    Procedure DoDataRowStart; override;
    Procedure ExportField(EF : TExportFieldItem); override;
    Procedure DoDataRowEnd; override;
  end;

  TFixedLengthExporter = Class(TCustomFixedLengthExporter)
  Published
    Property FileName;
    Property Dataset;
    Property ExportFields;
    Property FromCurrent;
    Property RestorePosition;
    Property FormatSettings;
    Property OnExportRow;
  end;

Procedure RegisterFixedExportFormat;
Procedure UnRegisterFixedExportFormat;

Const
  SFixedLengthExport      = 'Fixed';
  SFixedLengthExtensions  = '.txt';

Resourcestring
  SFixedLengthDescription = 'Text file with fixed length records';



implementation

{ TFixedLengthExportFieldItem }

procedure TFixedLengthExportFieldItem.Assign(Source: TPersistent);

Var
  FL : TFixedLengthExportFieldItem;

begin
  If Source is TFixedLengthExportFieldItem then
    begin
    FL:=Source as TFixedLengthExportFieldItem;
    Width:=FL.Width;
    AlignField:=FL.AlignFIeld;
    end;
  inherited Assign(Source);
end;

{ TCustomFixedLengthExporter }

procedure TCustomFixedLengthExporter.OutputRow(const ARow: String);
begin
  Writeln(TextFile,ARow);
end;

procedure TCustomFixedLengthExporter.BuildDefaultFieldMap(AMap: TExportFields);

{  TFieldType = (ftUnknown, ftString, ftSmallint, ftInteger, ftWord,
    ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate,  ftTime, ftDateTime,
    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar,
    ftWideString, ftLargeint, ftADT, ftArray, ftReference,
    ftDataSet, ftOraBlob, ftOraClob, ftVariant, ftInterface,
    ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, ftFixedWideChar, ftWideMemo);
}
Const
  FieldWidths : Array[TFieldType] of integer
              = (-1,0,3,10,5,
                  1,20,20,20,10,8,20,
                  0,0,10,0,0,0,0,
                  0,0,0,0,0,
                  0,0,0,0,0,
                  0,0,0,0,0,
                  0,0,0,0,0,0);

Var
  I,W : Integer;
  F : TField;
  FL : TFixedLengthExportFieldItem;

begin
  inherited BuildDefaultFieldMap(AMap);
  For I:=0 to AMap.Count-1 do
    begin
    FL:=TFixedLengthExportFieldItem(AMAP[i]);
    F:=Dataset.Fields[i];
    W:= FieldWidths[F.DataType];
    If (W>0) then
      FL.Width:=W
    else if (W=0) then
      begin
      if (F.DataType in StringFieldTypes) then
        FL.Width:=F.Size;
      end;
    If (F.DataType in IntFieldTypes) then
      Fl.AlignField:=afRight;
    end;
end;

function TCustomFixedLengthExporter.CreateExportFields: TExportFields;
begin
  Result:=TExportFields.Create(TFixedLengthExportFieldItem);
end;

procedure TCustomFixedLengthExporter.DoBeforeExecute;
begin
  inherited DoBeforeExecute;
  OpenTextFile;
end;

procedure TCustomFixedLengthExporter.DoAfterExecute;
begin
  CloseTextFile;
  inherited DoAfterExecute;
end;


procedure TCustomFixedLengthExporter.DoDataRowStart;
begin
  FCurrentRow:='';
end;

procedure TCustomFixedLengthExporter.ExportField(EF: TExportFieldItem);

Var
  S,SS : String;
  W,L : Integer;
  FL : TFixedLengthExportFieldItem;

begin
  S:=FormatField(EF.Field);
  If EF is TFixedLengthExportFieldItem then
    begin
    FL:=TFixedLengthExportFieldItem(EF);
    W:=FL.Width;
    end
  else
    W:=Length(S);
  L:=Length(S);
  If L>W then
    begin
    If (FL.AlignField=afLeft) then
      S:=Copy(S,1,W)
    else
      Delete(S,1,L-W);
    end
  else if (L<W) then
    begin
    SS:=StringOfChar(' ',W-L);
    If FL.AlignField=afRight then
      S:=SS+S
    else
      S:=S+SS;
    end;
  FCurrentRow:=FCurrentRow+S;
end;

procedure TCustomFixedLengthExporter.DoDataRowEnd;
begin
  OutputRow(FCurrentRow);
  FCurrentRow:='';
end;

Procedure RegisterFixedExportFormat;

begin
  ExportFormats.RegisterExportFormat(SFixedLengthExport,SFixedLengthDescription,SFixedLengthExtensions,TFixedLengthExporter);
end;

Procedure UnRegisterFixedExportFormat;

begin
  Exportformats.UnregisterExportFormat(SFixedLengthExport);
end;

end.

