unit fpdbfexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, dbf, fpdbexport;
  
Type

  { TDBFExportFieldItem }

  TDBFExportFieldItem = Class(TExportFieldItem)
  private
    FDestField: TField;
  Protected
    Property DestField : TField Read FDestField;
  end;

  { TDBFExportFormatSettings }

  TTableFormat = (tfDBaseIII,tfDBaseIV,tfDBaseVII,tfFoxPro);
  
  TDBFExportFormatSettings = class(TExportFormatSettings)
  private
    FAutoRename: Boolean;
    FTableFormat: TTableFormat;
  public
    Procedure Assign(Source : TPersistent); override;
  Published
    Property TableFormat : TTableFormat Read FTableFormat Write FTableFormat;
    Property AutoRenameFields : Boolean Read FAutoRename Write FAutoRename;
  end;
  { TFPCustomDBFExport }

  TFPCustomDBFExport = Class(TCustomDatasetExporter)
  Private
    FDBF : TDBF;
    FFileName: String;
    FAppendData: Boolean;
    function GetSettings: TDBFExportFormatSettings;
    procedure SetSettings(const AValue: TDBFExportFormatSettings);
  Protected
    Procedure CheckExportFieldNames(const MaxFieldNameLength: integer); virtual;
    Function BindFields : Boolean; override;
    Function CreateFormatSettings : TCustomExportFormatSettings; override;

    Function CreateExportFields : TExportFields; override;
    Procedure DoBeforeExecute; override;
    Procedure DoAfterExecute; override;
    Procedure DoDataRowStart; override;
    Procedure DoDataRowEnd; override;
    Procedure ExportField(EF : TExportFieldItem); override;
    Property FileName : String Read FFileName Write FFileName;
    Property AppendData : Boolean Read FAppendData Write FAppendData;
    Property DBF : TDBF Read FDBF;
  public
    Property FormatSettings : TDBFExportFormatSettings Read GetSettings Write SetSettings;
  end;

  TFPDBFExport = Class(TFPCustomDBFExport)
  Published
    Property FileName;
    Property Dataset;
    Property ExportFields;
    Property FromCurrent;
    Property RestorePosition;
    Property FormatSettings;
    Property OnExportRow;
  end;
  
Procedure RegisterDBFExportFormat;
Procedure UnRegisterDBFExportFormat;

Const
  SDBFExport = 'DBF';
  SDBFFilter = '*.dbf';
  
ResourceString
  SErrFailedToDeleteFile = 'Failed to delete existing DBF file: %s';
  SDBFDescription = 'DBF files';

implementation


{ TFPCustomDBFExport }

function TFPCustomDBFExport.GetSettings: TDBFExportFormatSettings;
begin
  Result:=TDBFExportFormatSettings(Inherited FormatSettings);
end;

procedure TFPCustomDBFExport.SetSettings(const AValue: TDBFExportFormatSettings
  );
begin
  Inherited FormatSettings.Assign(AValue);
end;

procedure TFPCustomDBFExport.CheckExportFieldNames(const MaxFieldNameLength: integer);

Var
  i,NameCounter : Integer;
  EF : TExportFieldItem;
  NewFieldName : String;
  
begin
  For i:=0 to ExportFields.Count-1 do
    begin
    EF:=ExportFields[i];
    { Cut off field name at max length, and
      rename if it already exists:}
    If (Length(EF.ExportedName)>MaxFieldNameLength) then
      begin
      NewFieldName:=Copy(EF.ExportedName,1,MaxFieldNameLength);
      If ExportFields.IndexOfExportedName(NewFieldName)<>-1 then
        begin
        NameCounter:=1;
        Repeat
          NewFieldName:=Copy(EF.ExportedName,1,8)+Format('%.2d',[NameCounter]);
        Until (ExportFIelds.IndexOfExportedName(NewFieldName)=-1);
        end;
      EF.ExportedName:=NewFieldName;
      end;
    end;
end;

function TFPCustomDBFExport.BindFields: Boolean;

Const
  Levels : Array[TTableFormat] of integer = (3,4,7,25);
  
Var
  EF : TDBFExportFieldItem;
  i : Integer;
  
begin
  // DBase III,IV, and FoxPro have a 10 character field length limit.
  If FormatSettings.AutoRenameFields and (FormatSettings.TableFormat in [tfDbaseIII,tfDbaseIV,tfFoxPro]) then
    CheckExportFieldNames(10);
  // DBase VII has a 32 character field length limit.
  If FormatSettings.AutoRenameFields and (FormatSettings.TableFormat=tfDbaseVII) then
    CheckExportFieldNames(32);
  Result:=Inherited;
  try
    with FDBF.FieldDefs do
      begin
      Clear;
      For i:=0 to ExportFields.Count-1 do
        begin
        EF:=ExportFields[i] as TDBFExportFieldItem;
        If EF.Enabled and Assigned(EF.Field) then
          Add(EF.ExportedName,EF.Field.DataType,EF.Field.Size);
        end;
      FDBF.TableLevel:=Levels[FormatSettings.TableFormat];
      FDBF.CreateTable;
      FDBF.Exclusive := true;
      FDBF.Open;
      end;
    For i:=0 to ExportFields.Count-1 do
      begin
      EF:=ExportFIelds[i] as TDBFExportFieldItem;
      If EF.Enabled then
        EF.FDestField:=FDBF.FieldByName(EF.ExportedName);
      end;
  except
    UnBindFields;
    Raise;
  end;
end;

function TFPCustomDBFExport.CreateFormatSettings: TCustomExportFormatSettings;
begin
  Result:=TDBFExportFormatSettings.Create(False);
end;

function TFPCustomDBFExport.CreateExportFields: TExportFields;
begin
  Result:=TExportFields.Create(TDBFExportFieldItem);
end;

procedure TFPCustomDBFExport.DoBeforeExecute;

Var
  FE : Boolean;
  
begin
  Inherited;
  FDBF:=TDBF.Create(Self);
  FDBF.TableName:=FFileName;
  FE:=FileExists(FFileName);
  If FAppendData and FE then
    FDBF.Open
  else
    begin
    If FE and Not AppendData then
      begin
      If not DeleteFile(FFileName) then
        Raise EDataExporter.CreateFmt(SErrFailedToDeleteFile,[FFileName]);
      end;
    end;
end;

procedure TFPCustomDBFExport.DoAfterExecute;
begin
  try
    FreeAndNil(FDBF);
  finally
    Inherited;
  end;
end;

procedure TFPCustomDBFExport.DoDataRowStart;
begin
  FDBF.Append;
end;

procedure TFPCustomDBFExport.DoDataRowEnd;
begin
  FDBF.Post;
end;

procedure TFPCustomDBFExport.ExportField(EF: TExportFieldItem);

Var
  F : TDBFExportFieldItem;
  
begin
  F:=EF as TDBFExportFieldItem;
  With F do
    // Export depending on field datatype;
    // convert to dbf data types where necessary.
    // Fall back to string if unknown datatype
    If FIeld.IsNull then
      DestField.Clear
    else if Field.Datatype in (IntFieldTypes+[ftAutoInc,ftLargeInt]) then
      DestField.AsInteger:=Field.AsInteger
    else if Field.Datatype in [ftBCD,ftCurrency,ftFloat,ftFMTBcd] then
      DestField.AsFloat:=Field.AsFloat
    else if Field.DataType in [ftString,ftFixedChar] then
      DestField.AsString:=Field.AsString
    else if (Field.DataType in ([ftWideMemo,ftWideString,ftFixedWideChar]+BlobFieldTypes)) then
      DestField.AsWideString:=Field.AsWideString
      { Note: we test for the wide text fields before the MemoFieldTypes, in order to
      let ftWideMemo end up at the right place }
    else if Field.DataType in MemoFieldTypes then
      DestField.AsString:=Field.AsString
    else if Field.DataType=ftBoolean then
      DestField.AsBoolean:=Field.AsBoolean
    else if field.DataType in DateFieldTypes then
      DestField.AsDatetime:=Field.AsDateTime
    else
      DestField.AsString:=Field.AsString
end;

Procedure RegisterDBFExportFormat;
begin
  RegisterExportFormat(SDBFExport,SDBFDescription,SDBFFilter,TFPDBFExport);
end;

Procedure UnRegisterDBFExportFormat;
begin
  UnregisterExportFormat(SDBFExport);
end;

{ TDBFExportFormatSettings }

procedure TDBFExportFormatSettings.Assign(Source: TPersistent);

Var
  FS : TDBFExportFormatSettings;
  
begin
  If Source is TDBFExportFormatSettings then
    begin
    FS:=Source as TDBFExportFormatSettings;
    AutoRenameFields:=FS.AutoRenameFields;
    TableFormat:=FS.TableFormat;
    end;
  inherited Assign(Source);
end;

end.

