unit TestDBExport;

{
  Unit tests which are common to all datasets. Tests export to various formats.
}

{$IFDEF FPC}
  {$mode Delphi}{$H+}
{$ENDIF}

interface

uses
  fpcunit, testregistry,
  Classes, SysUtils, db, ToolsUnit, bufdataset,
  fpDBExport,
  fpXMLXSDExport,
  fpdbfexport,
  fpcsvexport,
  fpfixedexport,
  fpSimpleXMLExport,
  fpsimplejsonexport,
  fpSQLExport,
  fptexexport,
  fprtfexport;


type
  TDetailedExportFormats = (efDBaseIII, efDBaseIV, efDBaseVII, efCSV, efFixedLengthText, efFoxpro,
    efJSON, efRTF, efSQL, efTeX, efXML, efXMLXSDAccess, efXMLXSDADONet, efXMLXSDClientDataset, efXMLXSDExcel);
const
  TDetailedExportExtensions: array [TDetailedExportFormats] of string[5] =
    ('.dbf','.dbf','.dbf','.csv','.txt','.dbf','.json','.rtf','.sql','.tex',
    '.xml','.xml','.xml','.xml','.xml'); //File extension for the corresponding TDetailedExportFormats
type
  { TTestDBExport }
  TTestDBExport = class(TTestCase)
  private
    FExportTempDir: string; //directory where test files are placed
    FKeepFilesAfterTest: boolean; //remove files after testing?
    function FieldSupported(const FieldType: TFieldType;
      const ExportSubFormat: TDetailedExportFormats): boolean; //Checks if output dataset supports a certain field type
    procedure GenericExportTest(Exporter: TCustomDatasetExporter; ExportFormat: TDetailedExportFormats);
    function GetFileSize(const FileName: string): integer; //Gets a file's size
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDBFExport_DBaseIV;
    procedure TestDBFExport_DBaseVII;
    procedure TestDBFExport_FoxPro;
    procedure TestCSVExport; //tests csv export with default values
    procedure TestCSVExport_RFC4180WithHeader; //tests csv export with settings that match RFC4180
    procedure TestCSVExport_TweakSettingsSemicolon; //tests semicolon delimited, custom country values
    procedure TestFixedTextExport;
    procedure TestJSONExport;
    procedure TestRTFExport;
    procedure TestSQLExport;
    procedure TestTeXExport;
    procedure TestXMLExport; //tests simple xml export
    procedure TestXSDExport_Access_NoXSD_DecimalOverride; //tests xmlxsd export
    procedure TestXSDExport_Access_NoXSD_NoDecimalOverride; //tests xmlxsd export
    procedure TestXSDExport_Access_XSD_DecimalOverride; //tests xmlxsd export
    procedure TestXSDExport_Access_XSD_NoDecimalOverride; //tests xmlxsd export
    procedure TestXSDExport_ADONET_NoXSD; //tests xmlxsd export
    procedure TestXSDExport_ADONET_XSD; //tests xmlxsd export
    procedure TestXSDExport_DelphiClientDataset; //tests xmlxsd export
    procedure TestXSDExport_Excel; //tests xmlxsd export
  end;

implementation

function TTestDBExport.FieldSupported(const FieldType: TFieldType;
  const ExportSubFormat: TDetailedExportFormats): boolean;
const
  // Alphabetically sorted for quick review:
  DBaseVIIUnsupported=[ftADT,ftArray,ftBCD,ftBytes,ftCurrency,ftCursor,ftDataSet,
    ftFixedWideChar,
    ftFMTBcd,ftFmtMemo,ftGraphic,ftGuid,ftIDispatch,ftInterface,ftOraBlob,
    ftOraClob,ftParadoxOle,ftReference,ftTime,ftTimeStamp,ftTypedBinary,
    ftUnknown,ftVarBytes,ftVariant,ftWidememo,ftWideString];
  FoxProUnsupported=  [ftADT,ftArray,      ftBytes,           ftCursor,ftDataSet,
    ftFixedWideChar,
    ftFMTBcd,ftFmtMemo,ftGraphic,ftGuid,ftIDispatch,ftInterface,ftOraBlob,
    ftOraClob,ftParadoxOle,ftReference,ftTime,ftTimeStamp,ftTypedBinary,
    ftUnknown,ftVarBytes,ftVariant,ftWideMemo,ftWideString];
begin
  result:=true;
  case ExportSubFormat of
    efDBaseIII: if FieldType in DBaseVIIUnsupported+[ftAutoInc] then result:=false;
    efDBaseIV: if FieldType in DBaseVIIUnsupported+[ftAutoInc] then result:=false;
    efDBaseVII: if FieldType in DBaseVIIUnsupported then result:=false;
    efCSV: result:=true;
    efFixedLengthText: result:=true; //todo: verify if all fields are really supported. Quick glance would indicate so
    efFoxpro: if FieldType in FoxProUnsupported then result:=false;
    efJSON: result:=true;
    efRTF: result:=true;
    efSQL: result:=true;
    efTeX: result:=true;
    efXML: result:=true;
    efXMLXSDAccess, efXMLXSDADONet, efXMLXSDClientDataset, efXMLXSDExcel: result:=true;
    else
    begin
      result:=false;
      Fail('Error in test code itself: FieldSupported unknown ExportSubFormat '+inttostr(ord(ExportSubFormat)));
    end;
  end;
end;

procedure TTestDBExport.GenericExportTest(Exporter: TCustomDatasetExporter; ExportFormat: TDetailedExportFormats);
var
  FieldMapping: TExportFields;
  NumberExported: integer;
  i: integer;
begin
  FieldMapping:=TExportFields.Create(Exporter.ExportFields.ItemClass);
  try
    Exporter.Dataset := DBConnector.GetFieldDataset;
    Exporter.Dataset.Open;
    Exporter.BuildDefaultFieldMap(FieldMapping);
    // Remove unsupported data types in export from the mapping.
    // Cannot use FieldMapping[i].Field.DataType as
    // the field hasn't been set by BindFields yet... assume the
    // order of original fields and their mapping match
    for i:=Exporter.Dataset.Fields.Count-1 downto 0 do
    begin
      if not FieldSupported(
        Exporter.Dataset.Fields[i].DataType,
        ExportFormat) then
          FieldMapping.Delete(i);
    end;
    for i:=0 to FieldMapping.Count-1 do
      Exporter.ExportFields.Add.Assign(FieldMapping[i]);
    NumberExported := Exporter.Execute;
    Exporter.Dataset.Last;
    Exporter.Dataset.First;
    AssertEquals('Number of records exported matches recordcount', NumberExported,
      Exporter.Dataset.RecordCount);
    Exporter.Dataset.Close;
  finally
    FieldMapping.Free;
  end;
end;

function TTestDBExport.GetFileSize(const FileName: string): integer;
Var
  F : file of byte;
begin
  result:=0;
  assign (F,FileName);
  try
    reset(F);
    result:=filesize(F);
  finally
    close(F);
  end;
end;

procedure TTestDBExport.SetUp;
begin
  inherited SetUp;
  InitialiseDBConnector;
  DBConnector.StartTest; //is this needed?
  FExportTempDir:=IncludeTrailingPathDelimiter(ExpandFileName(''))+'exporttests'+PathDelim; //Store output in subdirectory
  ForceDirectories(FExportTempDir);
  FKeepFilesAfterTest:=true; //keep test files; consistent with other units right now
end;

procedure TTestDBExport.TearDown;
begin
  inherited TearDown;
  DBConnector.StopTest; //is this needed?
  FreeDBConnector;
end;

procedure TTestDBExport.TestDBFExport_DBaseVII;
var
  Exporter: TFPDBFExport;
  ExportFormat: TDetailedExportFormats;
  ExportSettings:TDBFExportFormatSettings;
begin
  Exporter := TFPDBFExport.Create(nil);
  ExportSettings:=TDBFExportFormatSettings.Create(true);
  try
    ExportFormat:=efDBaseVII;
    ExportSettings.TableFormat:=tfDBaseVII;
    ExportSettings.AutoRenameFields:=true; //rename conflicting column names
    // Use export subtype position to differentiate output filenames:
    Exporter.FileName := FExportTempDir + inttostr(ord(ExportFormat)) +
      lowercase(rightstr(TestName,5)) +
      TDetailedExportExtensions[ExportFormat];
    Exporter.FormatSettings:=ExportSettings;
    GenericExportTest(Exporter, ExportFormat);
    AssertTrue('Output file must be created', FileExists(Exporter.FileName));
    AssertFalse('Output file must not be empty', (GetFileSize(Exporter.FileName) = 0));
  finally
    if (FKeepFilesAfterTest = False) then
      DeleteFile(Exporter.FileName);
    ExportSettings.Free;
    Exporter.Free;
  end;
end;

procedure TTestDBExport.TestDBFExport_DBaseIV;
var
  Exporter: TFPDBFExport;
  ExportFormat: TDetailedExportFormats;
  ExportSettings:TDBFExportFormatSettings;
begin
  Exporter := TFPDBFExport.Create(nil);
  ExportSettings:=TDBFExportFormatSettings.Create(true);
  try
    ExportFormat:=efDBaseIV;
    ExportSettings.TableFormat:=tfDBaseIV;
    ExportSettings.AutoRenameFields:=true; //rename conflicting column names
    Exporter.FileName := FExportTempDir + inttostr(ord(ExportFormat)) +
      lowercase(rightstr(TestName,5)) +
      TDetailedExportExtensions[ExportFormat];
    Exporter.FormatSettings:=ExportSettings;
    GenericExportTest(Exporter, ExportFormat);
    AssertTrue('Output file must be created', FileExists(Exporter.FileName));
    AssertFalse('Output file must not be empty', (GetFileSize(Exporter.FileName) = 0));
  finally
    if (FKeepFilesAfterTest = False) then
      DeleteFile(Exporter.FileName);
    ExportSettings.Free;
    Exporter.Free;
  end;
end;

procedure TTestDBExport.TestDBFExport_FoxPro;
var
  Exporter: TFPDBFExport;
  ExportFormat: TDetailedExportFormats;
  ExportSettings:TDBFExportFormatSettings;
begin
  Exporter := TFPDBFExport.Create(nil);
  ExportSettings:=TDBFExportFormatSettings.Create(true);
  try
    ExportFormat:=efFoxpro;
    ExportSettings.TableFormat:=tfFoxPro;
    ExportSettings.AutoRenameFields:=true; //rename conflicting column names
    Exporter.FileName := FExportTempDir + inttostr(ord(ExportFormat)) +
      lowercase(rightstr(TestName,5)) +
      TDetailedExportExtensions[ExportFormat];
    Exporter.FormatSettings:=ExportSettings;
    GenericExportTest(Exporter, ExportFormat);
    AssertTrue('Output file must be created', FileExists(Exporter.FileName));
    AssertFalse('Output file must not be empty', (GetFileSize(Exporter.FileName) = 0));
  finally
    if (FKeepFilesAfterTest = False) then
      DeleteFile(Exporter.FileName);
    ExportSettings.Free;
    Exporter.Free;
  end;
end;

procedure TTestDBExport.TestXSDExport_Access_NoXSD_DecimalOverride;
var
  Exporter: TXMLXSDExporter;
  ExportFormat: TDetailedExportFormats;
  ExportSettings:TXMLXSDFormatSettings;
begin
  Exporter := TXMLXSDExporter.Create(nil);
  ExportSettings:=TXMLXSDFormatSettings.Create(true);
  try
    ExportSettings.ExportFormat:=AccessCompatible;
    ExportFormat:=efXMLXSDAccess;
    ExportSettings.CreateXSD:=false;
    ExportSettings.DecimalSeparator:='.'; //override
    Exporter.FileName := FExportTempDir + inttostr(ord(ExportFormat)) +
      lowercase(rightstr(TestName,5)) +
      TDetailedExportExtensions[ExportFormat];
    Exporter.FormatSettings:=ExportSettings;
    GenericExportTest(Exporter, ExportFormat);
    AssertTrue('Output file must be created', FileExists(Exporter.FileName));
    AssertFalse('Output file must not be empty', (GetFileSize(Exporter.FileName) = 0));
  finally
    if (FKeepFilesAfterTest = False) then
      DeleteFile(Exporter.FileName);
    ExportSettings.Free;
    Exporter.Free;
  end;
end;

procedure TTestDBExport.TestXSDExport_Access_NoXSD_NoDecimalOverride;
var
  Exporter: TXMLXSDExporter;
  ExportFormat: TDetailedExportFormats;
  ExportSettings:TXMLXSDFormatSettings;
begin
  Exporter := TXMLXSDExporter.Create(nil);
  ExportSettings:=TXMLXSDFormatSettings.Create(true);
  try
    ExportSettings.ExportFormat:=AccessCompatible;
    ExportFormat:=efXMLXSDAccess;
    ExportSettings.CreateXSD:=false;
    ExportSettings.DecimalSeparator:=char(''); //don't override
    Exporter.FileName := FExportTempDir + inttostr(ord(ExportFormat)) +
      lowercase(rightstr(TestName,5)) +
      TDetailedExportExtensions[ExportFormat];
    Exporter.FormatSettings:=ExportSettings;
    GenericExportTest(Exporter, ExportFormat);
    AssertTrue('Output file must be created', FileExists(Exporter.FileName));
    AssertFalse('Output file must not be empty', (GetFileSize(Exporter.FileName) = 0));
  finally
    if (FKeepFilesAfterTest = False) then
      DeleteFile(Exporter.FileName);
    ExportSettings.Free;
    Exporter.Free;
  end;
end;

procedure TTestDBExport.TestXSDExport_Access_XSD_DecimalOverride;
var
  Exporter: TXMLXSDExporter;
  ExportFormat: TDetailedExportFormats;
  ExportSettings:TXMLXSDFormatSettings;
begin
  Exporter := TXMLXSDExporter.Create(nil);
  ExportSettings:=TXMLXSDFormatSettings.Create(true);
  try
    ExportSettings.ExportFormat:=AccessCompatible;
    ExportFormat:=efXMLXSDAccess;
    ExportSettings.CreateXSD:=true;
    ExportSettings.DecimalSeparator:='.'; //override
    Exporter.FileName := FExportTempDir + inttostr(ord(ExportFormat)) +
      lowercase(rightstr(TestName,5)) +
      TDetailedExportExtensions[ExportFormat];
    Exporter.FormatSettings:=ExportSettings;
    GenericExportTest(Exporter, ExportFormat);
    AssertTrue('Output file must be created', FileExists(Exporter.FileName));
    AssertFalse('Output file must not be empty', (GetFileSize(Exporter.FileName) = 0));
  finally
    if (FKeepFilesAfterTest = False) then
      DeleteFile(Exporter.FileName);
    ExportSettings.Free;
    Exporter.Free;
  end;
end;

procedure TTestDBExport.TestXSDExport_Access_XSD_NoDecimalOverride;
var
  Exporter: TXMLXSDExporter;
  ExportFormat: TDetailedExportFormats;
  ExportSettings:TXMLXSDFormatSettings;
begin
  Exporter := TXMLXSDExporter.Create(nil);
  ExportSettings:=TXMLXSDFormatSettings.Create(true);
  try
    ExportSettings.ExportFormat:=AccessCompatible;
    ExportFormat:=efXMLXSDAccess;
    ExportSettings.CreateXSD:=true;
    ExportSettings.DecimalSeparator:=char(''); //don't override
    Exporter.FileName := FExportTempDir + inttostr(ord(ExportFormat)) +
      lowercase(rightstr(TestName,5)) +
      TDetailedExportExtensions[ExportFormat];
    Exporter.FormatSettings:=ExportSettings;
    GenericExportTest(Exporter, ExportFormat);
    AssertTrue('Output file must be created', FileExists(Exporter.FileName));
    AssertFalse('Output file must not be empty', (GetFileSize(Exporter.FileName) = 0));
  finally
    if (FKeepFilesAfterTest = False) then
      DeleteFile(Exporter.FileName);
    ExportSettings.Free;
    Exporter.Free;
  end;
end;

procedure TTestDBExport.TestXSDExport_ADONET_NoXSD;
var
  Exporter: TXMLXSDExporter;
  ExportFormat: TDetailedExportFormats;
  ExportSettings:TXMLXSDFormatSettings;
begin
  Exporter := TXMLXSDExporter.Create(nil);
  ExportSettings:=TXMLXSDFormatSettings.Create(true);
  try
    ExportSettings.ExportFormat:=ADONETCompatible;
    ExportFormat:=efXMLXSDADONet;
    ExportSettings.CreateXSD:=false;
    Exporter.FileName := FExportTempDir + inttostr(ord(ExportFormat)) +
      lowercase(rightstr(TestName,5)) +
      TDetailedExportExtensions[ExportFormat];
    Exporter.FormatSettings:=ExportSettings;
    GenericExportTest(Exporter, ExportFormat);
    AssertTrue('Output file must be created', FileExists(Exporter.FileName));
    AssertFalse('Output file must not be empty', (GetFileSize(Exporter.FileName) = 0));
  finally
    if (FKeepFilesAfterTest = False) then
      DeleteFile(Exporter.FileName);
    ExportSettings.Free;
    Exporter.Free;
  end;
end;

procedure TTestDBExport.TestXSDExport_ADONET_XSD;
var
  Exporter: TXMLXSDExporter;
  ExportFormat: TDetailedExportFormats;
  ExportSettings:TXMLXSDFormatSettings;
begin
  Exporter := TXMLXSDExporter.Create(nil);
  ExportSettings:=TXMLXSDFormatSettings.Create(true);
  try
    ExportSettings.ExportFormat:=ADONETCompatible;
    ExportFormat:=efXMLXSDADONet;
    ExportSettings.CreateXSD:=true;
    Exporter.FileName := FExportTempDir + inttostr(ord(ExportFormat)) +
      lowercase(rightstr(TestName,5)) +
      TDetailedExportExtensions[ExportFormat];
    Exporter.FormatSettings:=ExportSettings;
    GenericExportTest(Exporter, ExportFormat);
    AssertTrue('Output file must be created', FileExists(Exporter.FileName));
    AssertFalse('Output file must not be empty', (GetFileSize(Exporter.FileName) = 0));
  finally
    if (FKeepFilesAfterTest = False) then
      DeleteFile(Exporter.FileName);
    ExportSettings.Free;
    Exporter.Free;
  end;
end;

procedure TTestDBExport.TestCSVExport;
var
  Exporter: TCSVExporter;
  ExportFormat: TDetailedExportFormats;
  ExportSettings: TCSVFormatSettings;
begin
  Exporter := TCSVExporter.Create(nil);
  ExportSettings:=TCSVFormatSettings.Create(true);
  try
    ExportFormat:=efCSV;
    Exporter.FileName := FExportTempDir + inttostr(ord(ExportFormat)) +
      lowercase(rightstr(TestName,5)) +
      TDetailedExportExtensions[ExportFormat];
    Exporter.FormatSettings:=ExportSettings;
    GenericExportTest(Exporter, ExportFormat);
    AssertTrue('Output file must be created', FileExists(Exporter.FileName));
    AssertFalse('Output file must not be empty', (GetFileSize(Exporter.FileName) = 0));
  finally
    if (FKeepFilesAfterTest = False) then
      DeleteFile(Exporter.FileName);
    ExportSettings.Free;
    Exporter.Free;
  end;
end;

procedure TTestDBExport.TestCSVExport_RFC4180WithHeader;
var
  Exporter: TCSVExporter;
  ExportFormat: TDetailedExportFormats;
  ExportSettings: TCSVFormatSettings;
begin
  Exporter := TCSVExporter.Create(nil);
  ExportSettings:=TCSVFormatSettings.Create(true);
  try
    ExportSettings.FieldDelimiter:=','; //RFC 4180 specified commas as delimiter
    ExportSettings.HeaderRow:=true; //...allows an optional header line
    ExportSettings.StringQuoteChar:='"'; //...requires quoting with " (if quoting)
    // Fields containing line breaks (CRLF), double quotes,
    // and commas should be enclosed in double-quotes.
    // => this probably won't get tested with this test set.
    ExportFormat:=efCSV;
    Exporter.FileName := FExportTempDir + inttostr(ord(ExportFormat)) +
      lowercase(rightstr(TestName,5)) +
      TDetailedExportExtensions[ExportFormat];
    Exporter.FormatSettings:=ExportSettings;
    GenericExportTest(Exporter, ExportFormat);
    AssertTrue('Output file must be created', FileExists(Exporter.FileName));
    AssertFalse('Output file must not be empty', (GetFileSize(Exporter.FileName) = 0));
  finally
    if (FKeepFilesAfterTest = False) then
      DeleteFile(Exporter.FileName);
    ExportSettings.Free;
    Exporter.Free;
  end;
end;

procedure TTestDBExport.TestCSVExport_TweakSettingsSemicolon;
var
  Exporter: TCSVExporter;
  ExportFormat: TDetailedExportFormats;
  ExportSettings: TCSVFormatSettings;
begin
  Exporter := TCSVExporter.Create(nil);
  ExportSettings:=TCSVFormatSettings.Create(true);
  try
    ExportSettings.FieldDelimiter:=';';
    ExportSettings.QuoteStrings:=[qsAlways,qsSpace,qsDelimiter]; //quote everything we can
    ExportSettings.StringQuoteChar:='"'; //try explicit assignment
    ExportSettings.RowDelimiter:=#10; //Unix/Linux format
    ExportSettings.BooleanFalse:='onwaar'; //why not a Dutch output format?
    ExportSettings.BooleanTrue:='waar'; //why not a Dutch output format?
    ExportSettings.CurrencyDigits:=3;
    ExportSettings.CurrencySymbol:='€'; //euro sign
    ExportSettings.DateFormat:='d-mm-yyyy'; //Dutch setting
    ExportSettings.DateTimeFormat:='d-mm-yyyy hh:nn:ss'; //Dutch setting
    ExportSettings.DecimalSeparator:=','; //another Dutch setting
    ExportSettings.TimeFormat:='hh:nn:ss'; //Dutch setting
    ExportSettings.IntegerFormat:='0000';//Strange but nice ;)
    ExportFormat:=efCSV;
    Exporter.FileName := FExportTempDir + inttostr(ord(ExportFormat)) +
      lowercase(rightstr(TestName,5)) +
      TDetailedExportExtensions[ExportFormat];
    Exporter.FormatSettings:=ExportSettings;
    GenericExportTest(Exporter, ExportFormat);
    AssertTrue('Output file must be created', FileExists(Exporter.FileName));
    AssertFalse('Output file must not be empty', (GetFileSize(Exporter.FileName) = 0));
  finally
    if (FKeepFilesAfterTest = False) then
      DeleteFile(Exporter.FileName);
    ExportSettings.Free;
    Exporter.Free;
  end;
end;

procedure TTestDBExport.TestFixedTextExport;
var
  Exporter: TFixedLengthExporter;
  ExportFormat: TDetailedExportFormats;
begin
  Exporter := TFixedLengthExporter.Create(nil);
  try
    ExportFormat:=efFixedLengthText;
    Exporter.FileName := FExportTempDir + inttostr(ord(ExportFormat)) +
      lowercase(rightstr(TestName,5)) +
      TDetailedExportExtensions[ExportFormat];
    GenericExportTest(Exporter, ExportFormat);
    AssertTrue('Output file must be created', FileExists(Exporter.FileName));
    AssertFalse('Output file must not be empty', (GetFileSize(Exporter.FileName) = 0));
  finally
    if (FKeepFilesAfterTest = False) then
      DeleteFile(Exporter.FileName);
    Exporter.Free;
  end;
end;

procedure TTestDBExport.TestJSONExport;
var
  Exporter: TSimpleJSONExporter;
  ExportFormat: TDetailedExportFormats;
  ExportSettings:TSimpleJSONFormatSettings;
begin
  Exporter := TSimpleJSONExporter.Create(nil);
  ExportSettings:=TSimpleJSONFormatSettings.Create(true);
  try
    ExportFormat:=efJSON;
    Exporter.FileName := FExportTempDir + inttostr(ord(ExportFormat)) + lowercase(rightstr(TestName,5))+
     inttostr(ord(ExportFormat))+
     TDetailedExportExtensions[ExportFormat];    Exporter.FormatSettings:=ExportSettings;
    GenericExportTest(Exporter, ExportFormat);
    AssertTrue('Output file must be created', FileExists(Exporter.FileName));
    AssertFalse('Output file must not be empty', (GetFileSize(Exporter.FileName) = 0));
  finally
    if (FKeepFilesAfterTest = False) then
      DeleteFile(Exporter.FileName);
    ExportSettings.Free;
    Exporter.Free;
  end;
end;

procedure TTestDBExport.TestRTFExport;
var
  Exporter: TRTFExporter;
  ExportFormat: TDetailedExportFormats;
  ExportSettings:TRTFExportFormatSettings;
begin
  Exporter := TRTFExporter.Create(nil);
  ExportSettings:=TRTFExportFormatSettings.Create(true);
  try
    ExportFormat:=efRTF;
    Exporter.FileName := FExportTempDir + inttostr(ord(ExportFormat)) +
      lowercase(rightstr(TestName,5))+
      TDetailedExportExtensions[ExportFormat];
    Exporter.FormatSettings:=ExportSettings;
    GenericExportTest(Exporter, ExportFormat);
    AssertTrue('Output file must be created', FileExists(Exporter.FileName));
    AssertFalse('Output file must not be empty', (GetFileSize(Exporter.FileName) = 0));
  finally
    if (FKeepFilesAfterTest = False) then
      DeleteFile(Exporter.FileName);
    ExportSettings.Free;
    Exporter.Free;
  end;
end;

procedure TTestDBExport.TestSQLExport;
var
  Exporter: TSQLExporter;
  ExportFormat: TDetailedExportFormats;
  ExportSettings:TSQLFormatSettings;
begin
  Exporter := TSQLExporter.Create(nil);
  ExportSettings:=TSQLFormatSettings.Create(true);
  try
    ExportSettings.TableName:='ATABLE'; //required for export to succeed
    ExportFormat:=efSQL;
    Exporter.FileName := FExportTempDir + inttostr(ord(ExportFormat)) +
      lowercase(rightstr(TestName,5))+
      TDetailedExportExtensions[ExportFormat];
    Exporter.FormatSettings:=ExportSettings;
    GenericExportTest(Exporter, ExportFormat);
    AssertTrue('Output file must be created', FileExists(Exporter.FileName));
    AssertFalse('Output file must not be empty', (GetFileSize(Exporter.FileName) = 0));
  finally
    if (FKeepFilesAfterTest = False) then
      DeleteFile(Exporter.FileName);
    ExportSettings.Free;
    Exporter.Free;
  end;
end;

procedure TTestDBExport.TestTeXExport;
var
  Exporter: TTexExporter;
  ExportFormat: TDetailedExportFormats;
  ExportSettings:TTeXExportFormatSettings;
begin
  Exporter := TTexExporter.Create(nil);
  ExportSettings:=TTeXExportFormatSettings.Create(true);
  try
    ExportFormat:=efTeX;
    Exporter.FileName := FExportTempDir +
      inttostr(ord(ExportFormat)) + lowercase(rightstr(TestName,5))+
      TDetailedExportExtensions[ExportFormat];
    Exporter.FormatSettings:=ExportSettings;
    GenericExportTest(Exporter, ExportFormat);
    AssertTrue('Output file must be created', FileExists(Exporter.FileName));
    AssertFalse('Output file must not be empty', (GetFileSize(Exporter.FileName) = 0));
  finally
    if (FKeepFilesAfterTest = False) then
      DeleteFile(Exporter.FileName);
    ExportSettings.Free;
    Exporter.Free;
  end;
end;

procedure TTestDBExport.TestXMLExport;
var
  Exporter: TSimpleXMLExporter;
  ExportFormat: TDetailedExportFormats;
  ExportSettings:TSimpleXMLFormatSettings;
begin
  Exporter := TSimpleXMLExporter.Create(nil);
  ExportSettings:=TSimpleXMLFormatSettings.Create(true);
  try
    ExportFormat:=efXML;
    Exporter.FileName := FExportTempDir + inttostr(ord(ExportFormat)) +
     lowercase(rightstr(TestName,5)) +
     TDetailedExportExtensions[ExportFormat];
    Exporter.FormatSettings:=ExportSettings;
    GenericExportTest(Exporter, ExportFormat);
    AssertTrue('Output file must be created', FileExists(Exporter.FileName));
    AssertFalse('Output file must not be empty', (GetFileSize(Exporter.FileName) = 0));
  finally
    if (FKeepFilesAfterTest = False) then
      DeleteFile(Exporter.FileName);
    ExportSettings.Free;
    Exporter.Free;
  end;
end;

procedure TTestDBExport.TestXSDExport_DelphiClientDataset;
var
  Exporter: TXMLXSDExporter;
  ExportFormat: TDetailedExportFormats;
  ExportSettings:TXMLXSDFormatSettings;
begin
  Exporter := TXMLXSDExporter.Create(nil);
  ExportSettings:=TXMLXSDFormatSettings.Create(true);
  try
    ExportSettings.ExportFormat:=DelphiClientDataset;
    ExportFormat:=efXMLXSDClientDataset;
    Exporter.FileName := FExportTempDir + inttostr(ord(ExportFormat)) +
      lowercase(rightstr(TestName,5)) +
      TDetailedExportExtensions[ExportFormat];
    Exporter.FormatSettings:=ExportSettings;
    GenericExportTest(Exporter, ExportFormat);
    AssertTrue('Output file must be created', FileExists(Exporter.FileName));
    AssertFalse('Output file must not be empty', (GetFileSize(Exporter.FileName) = 0));
  finally
    if (FKeepFilesAfterTest = False) then
      DeleteFile(Exporter.FileName);
    ExportSettings.Free;
    Exporter.Free;
  end;
end;

procedure TTestDBExport.TestXSDExport_Excel;
var
  Exporter: TXMLXSDExporter;
  ExportFormat: TDetailedExportFormats;
  ExportSettings:TXMLXSDFormatSettings;
begin
  Exporter := TXMLXSDExporter.Create(nil);
  ExportSettings:=TXMLXSDFormatSettings.Create(true);
  try
    ExportSettings.ExportFormat:=ExcelCompatible;
    ExportFormat:=efXMLXSDExcel;
    Exporter.FileName := FExportTempDir + inttostr(ord(ExportFormat)) +
      lowercase(rightstr(TestName,5)) +
      TDetailedExportExtensions[ExportFormat];
    Exporter.FormatSettings:=ExportSettings;
    GenericExportTest(Exporter, ExportFormat);
    AssertTrue('Output file must be created', FileExists(Exporter.FileName));
    AssertFalse('Output file must not be empty', (GetFileSize(Exporter.FileName) = 0));
  finally
    if (FKeepFilesAfterTest = False) then
      DeleteFile(Exporter.FileName);
    ExportSettings.Free;
    Exporter.Free;
  end;
end;


initialization
  RegisterTest(TTestDBExport);
end.
