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
  TCharMode = (cmANSI,cmUTF8,cmUTF16);

  { TFixedExportFormatSettings }

  TFixedExportFormatSettings = Class (TCustomExportFormatSettings)
  private
    FCharMode: TCharMode;
  Public
    Procedure Assign(Source: TPersistent); override;
  Published
    Property CharMode : TCharMode Read FCharMode Write FCharMode;
  end;

  TCustomFixedLengthExporter = Class(TCustomFileExporter)
  Private
    FCurrentRow : RawByteString;
    FCurrentRowUnicode : UnicodeString;
    function GetCharMode: TCharMode;
    function GeTFixedExportFormatSettings: TFixedExportFormatSettings;
    procedure SeTFixedExportFormatSettings(AValue: TFixedExportFormatSettings);
  Protected
    function ExportFieldAsUniCodeString(EF: TExportFieldItem): UnicodeString; virtual;
    procedure ExportFieldAnsi(EF: TExportFieldItem); virtual;
    procedure ExportFieldUTF16(EF: TExportFieldItem); virtual;
    procedure ExportFieldUTF8(EF: TExportFieldItem); virtual;
    Procedure BuildDefaultFieldMap(AMap : TExportFields); override;
    Function  CreateExportFields : TExportFields; override;
    Function  CreateFormatSettings: TCustomExportFormatSettings; override;
    Procedure DoBeforeExecute; override;
    Procedure DoAfterExecute; override;
    Procedure DoDataRowStart; override;
    Procedure ExportField(EF : TExportFieldItem); override;
    Procedure DoDataRowEnd; override;
    Property CharMode : TCharMode Read GetCharMode;
    Property FixedFormatSettings : TFixedExportFormatSettings Read GeTFixedExportFormatSettings Write SeTFixedExportFormatSettings;
  end;

  TFixedLengthExporter = Class(TCustomFixedLengthExporter)
  Public
    Property FixedFormatSettings;
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

{ TFixedExportFormatSettings }

procedure TFixedExportFormatSettings.Assign(Source: TPersistent);
begin
  if (Source is TFixedExportFormatSettings) then
    CharMode:=TFixedExportFormatSettings(Source).CharMode;
  inherited Assign(Source);
end;

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


procedure TCustomFixedLengthExporter.SeTFixedExportFormatSettings(AValue: TFixedExportFormatSettings);
begin
  FormatSettings:=AValue;
end;

function TCustomFixedLengthExporter.GetCharMode: TCharMode;
begin
  Result:=FixedFormatSettings.CharMode;
end;

function TCustomFixedLengthExporter.GeTFixedExportFormatSettings: TFixedExportFormatSettings;
begin
  Result:=Formatsettings as TFixedExportFormatSettings;
end;

procedure TCustomFixedLengthExporter.BuildDefaultFieldMap(AMap: TExportFields);

Const
  RightAlignedFields = IntFieldTypes+FloatFieldTypes;

  // Mapping to TFieldType
  FieldWidths : Array[TFieldType] of integer =
    (
    {ftUnknown} -1,
    {ftString} 0,
    {ftSmallint} 3,
    {ftInteger} 10,
    {ftWord} 5,
    {ftBoolean} 1,
    {ftFloat} 20,
    {ftCurrency} 20,
    {ftBCD} 20,
    {ftDate} 10,
    {ftTime} 8,
    {ftDateTime} 20,
    {ftBytes} 0,
    {ftVarBytes} 0,
    {ftAutoInc} 10,
    {ftBlob} 0,
    {ftMemo} 0,
    {ftGraphic} 0,
    {ftFmtMemo} 0,
    {ftParadoxOle} 0,
    {ftDBaseOle} 0,
    {ftTypedBinary} 0,
    {ftCursor} 0,
    {ftFixedChar} 0,
    {ftWideString} 0,
    {ftLargeint} 0,
    {ftADT} 0,
    {ftArray} 0,
    {ftReference} 0,
    {ftDataSet} 0,
    {ftOraBlob} 0,
    {ftOraClob} 0,
    {ftVariant} 0,
    {ftInterface} 0,
    {ftIDispatch} 0,
    {ftGuid} 0,
    {ftTimeStamp} 0,
    {ftFMTBcd} 0,
    {ftFixedWideChar} 0,
    {ftWideMemo} 0
    );

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
    If (F.DataType in RightAlignedFields) then
      Fl.AlignField:=afRight;
    end;
end;

function TCustomFixedLengthExporter.CreateExportFields: TExportFields;
begin
  Result:=TExportFields.Create(TFixedLengthExportFieldItem);
end;

function TCustomFixedLengthExporter.CreateFormatSettings: TCustomExportFormatSettings;
begin
  Result:=TFixedExportFormatSettings.Create(True);
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

begin
  Case CharMode of
    cmANSI : ExportFieldAnsi(EF);
    cmUTF8 : ExportFieldUTF8(EF);
    cmUTF16 : ExportFieldUTF16(EF);
  end;
end;


Function TCustomFixedLengthExporter.ExportFieldAsUniCodeString(EF: TExportFieldItem) : UnicodeString;

Var
  S,SS : UnicodeString;
  FL : TFixedLengthExportFieldItem;
  L,W : Integer;

begin
  S:=UTF8Decode(FormatField(EF.Field));
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
  Result:=S;
end;

procedure TCustomFixedLengthExporter.ExportFieldUTF16(EF: TExportFieldItem);

begin
  FCurrentRowUnicode:=FCurrentRowUnicode+ExportFieldAsUnicodeString(EF);
end;


procedure TCustomFixedLengthExporter.ExportFieldUTF8(EF: TExportFieldItem);


begin
  FCurrentRow:=FCurrentRow+UTF8Encode(ExportFieldAsUnicodeString(EF));
end;

procedure TCustomFixedLengthExporter.ExportFieldAnsi(EF: TExportFieldItem);

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
  if (CharMode<>cmUTF16) then
    Writeln(TextFile,FCurrentRow)
  else
    Writeln(TextFile,FCurrentRowUnicode);
  FCurrentRow:='';
  FCurrentRowUnicode:='';
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

