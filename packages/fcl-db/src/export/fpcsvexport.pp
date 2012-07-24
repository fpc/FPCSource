unit fpcsvexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, fpDBExport;

Type
  { TCSVFormatSettings }

  TCSVFormatSettings = Class(TExportFormatSettings)
  Private
    FDelimiter: String;
    FHeaderRow: Boolean;
    FQuoteStrings: TQuoteStrings;
    FRowDelimiter: String;
    FStringQuoteChar: String;
  Public
    Constructor Create(DoInitSettings : Boolean); override;
    Procedure Assign(Source : TPersistent); override;
  Published
    // Properties
    Property FieldDelimiter : String Read FDelimiter Write FDelimiter;
    Property RowDelimiter : String Read FRowDelimiter Write FRowDelimiter;
    Property HeaderRow : Boolean Read FHeaderRow Write FHeaderRow default true;
    Property QuoteStrings : TQuoteStrings Read FQuoteStrings Write FQuoteStrings;
    Property StringQuoteChar : String Read FStringQuoteChar Write FStringQuoteChar;
  end;

  { TCustomCSVExporter }

  TCustomCSVExporter = Class(TCustomFileExporter)
  private
    FCurrentRow:String;
    function GetCSVFormatsettings: TCSVFormatSettings;
    procedure OutputRow(const ARow: String);
    procedure SetCSVFormatSettings(const AValue: TCSVFormatSettings);
  Protected
    Function CreateFormatSettings : TCustomExportFormatSettings; override;
    Procedure DoBeforeExecute; override;
    Procedure DoAfterExecute; override;
    Procedure DoDataHeader; override;
    Procedure DoDataRowStart; override;
    Procedure ExportField(EF : TExportFieldItem); override;
    Procedure DoDataRowEnd; override;
  Public
    Constructor Create(Aowner : TComponent); override;
    Property FormatSettings : TCSVFormatSettings Read GetCSVFormatsettings Write SetCSVFormatSettings;
  end;

  { TCSVExporter }
  

  TCSVExporter = Class(TCustomCSVExporter)
  Published
    Property FileName;
    Property Dataset;
    Property ExportFields;
    Property FromCurrent;
    Property RestorePosition;
    Property FormatSettings;
    Property OnExportRow;
  end;

Procedure RegisterCSVExportFormat;
Procedure UnRegisterCSVExportFormat;

Const
  SCSVExport      = 'CSV';
  SCSVExtensions  = '.csv;.txt';

ResourceString
  SCSVDescription = 'Comma-Separated Values (CSV)';


implementation

{ TCustomCSVExporter }

procedure TCustomCSVExporter.DoBeforeExecute;
begin
  inherited DoBeforeExecute;
  OpenTextFile;
end;

procedure TCustomCSVExporter.DoAfterExecute;
begin
  CloseTextFile;
  inherited DoAfterExecute;
end;

procedure TCustomCSVExporter.OutputRow(Const ARow : String);

Var
  RD : String;

begin
  RD:=FormatSettings.RowDelimiter;
  If (RD='') then
    Writeln(TextFile,ARow)
  else
    Write(TextFile,ARow,RD)
end;

function TCustomCSVExporter.GetCSVFormatsettings: TCSVFormatSettings;
begin
  Result:=TCSVFormatSettings(Inherited FormatSettings)
end;

procedure TCustomCSVExporter.SetCSVFormatSettings(
  const AValue: TCSVFormatSettings);
begin
  Inherited FormatSettings:=AValue;
end;

function TCustomCSVExporter.CreateFormatSettings: TCustomExportFormatSettings;
begin
  Result:=TCSVFormatSettings.Create(False)
end;


procedure TCustomCSVExporter.DoDataHeader;

Var
  S : String;
  I : Integer;

begin
  If FormatSettings.HeaderRow then
    begin
    S:='';
    For I:=0 to ExportFields.Count-1 do
      begin
      If (S<>'') then
        S:=S+FormatSettings.FieldDelimiter;
      S:=S+ExportFields[i].ExportedName;
      end;
    OutputRow(S);
    end;
  inherited DoDataHeader;
end;


procedure TCustomCSVExporter.DoDataRowStart;
begin
  FCurrentRow:='';
end;

procedure TCustomCSVExporter.ExportField(EF: TExportFieldItem);

  Function HaveSpace(Const S : String;QS : TQuoteStrings) : Boolean;

  begin
    Result:=(qsSpace in QS) and (Pos(' ',S)<>0)
  end;

  Function HaveDelimiter(Const S : String;QS : TQuoteStrings) : Boolean;

  Var
    FD : String;

  begin
    Result:=(qsDelimiter in QS);
    If Result then
      begin
      FD:=FormatSettings.FieldDelimiter;
      Result:=(FD<>'') and (Pos(FD,S)<>0);
      end;
  end;

Var
  S,C : String;
  QS  : TQuoteStrings;

begin
  S:=FormatField(EF.Field);
  QS:=FormatSettings.QuoteStrings;
  {If specified, quote everything that can contain delimiters;
  leave numeric, date fields alone:}
  If (
  (EF.Field.DataType in StringFieldTypes) or
  (EF.Field.DataType in MemoFieldTypes) or
  (EF.Field.DataType in BlobFieldTypes)
  )
  and (QS<>[]) then
    begin
    If (qsAlways in QS) or HaveSpace(S,QS) or HaveDelimiter(S,QS) then
      begin
      C:=FormatSettings.StringQuoteChar;
      S:=C+S+C;
      end;
    end;
  If (FCurrentRow<>'') then
    FCurrentRow:=FCurrentRow+FormatSettings.FieldDelimiter;
  FCurrentRow:=FCurrentRow+S;
end;


procedure TCustomCSVExporter.DoDataRowEnd;
begin
  OutputRow(FCurrentRow);
  FCurrentRow:='';
end;

constructor TCustomCSVExporter.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
end;

{ TCSVFormatSettings }

constructor TCSVFormatSettings.Create(DoInitSettings: Boolean);
begin
  inherited Create(DoInitSettings);
  FHeaderRow:=True;
  FDelimiter:=',';
  FStringQuoteChar:='"';
  FQuoteStrings:=[qsSpace, qsDelimiter];
  {Sensible defaults as reading unquoted strings with delimiters/spaces will
  either fail by creating phantom fields (qsDelimiter) or delete leading or
  trailing data/spaces (qsSpace)}
end;

procedure TCSVFormatSettings.Assign(Source: TPersistent);

Var
  FS : TCSVFormatsettings;

begin
  If (Source is TCSVFormatSettings) then
    begin
    FS:=Source as TCSVFormatSettings;
    FDelimiter:=FS.FDelimiter;
    FHeaderRow:=FS.FHEaderRow;
    FQuoteStrings:=FS.FQuoteStrings;
    FRowDelimiter:=FS.FRowDelimiter;
    FStringQuoteChar:=FS.FStringQuoteChar;
    end;
  inherited Assign(Source);
end;

Procedure RegisterCSVExportFormat;

begin
  ExportFormats.RegisterExportFormat(SCSVExport,SCSVDescription,SCSVExtensions,TCSVExporter);
end;

Procedure UnRegisterCSVExportFormat;

begin
end;


end.

