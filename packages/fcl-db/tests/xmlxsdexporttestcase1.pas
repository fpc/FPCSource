unit XMLXSDExportTestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Fpcunit, Testutils, Testregistry, DB, fpXMLXSDExport,
  BufDataset, dateutils;

type

  Ttestxmlxsdexport1 = class(Ttestcase)
  private
    procedure FillTestData;
  protected
    FTestDataset: TBufDataset;
    FExportTempDir: string; //where we store exported files in these tests
    procedure Setup; override;
    procedure Teardown; override;
  published
    procedure TestXSDExport_Access_NoXSD_Decimal;
    procedure TestXSDExport_Access_NoXSD_NoDecimal;
    procedure TestXSDExport_Access_XSD_Decimal;
    procedure TestXSDExport_Access_XSD_NoDecimal;
    procedure TestXSDExport_ADONET_NoXSD;
    procedure TestXSDExport_ADONET_XSD;
    procedure TestXSDExport_DelphiClientDataset;
    procedure TestXSDExport_Excel;
  end;

implementation

procedure Ttestxmlxsdexport1.TestXSDExport_Access_NoXSD_NoDecimal;

var
  Export: TXMLXSDExporter;
  Settings: TXMLXSDFormatSettings;
  NumberExported: integer;

begin
  Export := TXMLXSDExporter.Create(nil);
  Settings := TXMLXSDFormatSettings.Create(True);
  try
    //Don't override decimal separator
    Settings.CreateXSD := False;
    Settings.DecimalSeparator := Char(''); //Don't override decimalseparator
    Settings.ExportFormat := AccessCompatible;
    Export.Dataset := FTestDataset;
    Export.FormatSettings := Settings;
    Export.FileName := FExportTempDir + 'Access_NoXSD_NoDecimal.xml';
    NumberExported := Export.Execute;
    FTestDataset.Close;
    AssertEquals('Number of records exported', NumberExported, FTestDataset.RecordCount);
    //we should have exported 2 records.
  finally
    Settings.Free;
    { TODO 9 -oAnyone -cNice to have : When initial testing is complete, delete export files. }
    //DeleteFile(Export.FileName); //todo: fix this for release
    Export.Free;
  end;
end;

procedure Ttestxmlxsdexport1.TestXSDExport_Access_NoXSD_Decimal;
var
  Export: TXMLXSDExporter;
  Settings: TXMLXSDFormatSettings;
  NumberExported: integer;

begin
  Export := TXMLXSDExporter.Create(nil);
  Settings := TXMLXSDFormatSettings.Create(True);
  try
    Settings.DecimalSeparator := ',';
    //Try to override decimal separator specified by fpXMLXSDExport
    Settings.CreateXSD := False;
    Settings.ExportFormat := AccessCompatible;
    Export.Dataset := FTestDataset;
    Export.FormatSettings := Settings;
    Export.FileName := FExportTempDir + 'Access_NoXSD_Decimal.xml';
    NumberExported := Export.Execute;
    FTestDataset.Close;
    AssertEquals('Number of records exported', NumberExported, FTestDataset.RecordCount);
    //we should have exported 2 records.
  finally
    Settings.Free;
    //DeleteFile(Export.FileName); //todo: fix this for release
    Export.Free;
  end;
end;

procedure Ttestxmlxsdexport1.TestXSDExport_Access_XSD_NoDecimal;
var
  Export: TXMLXSDExporter;
  Settings: TXMLXSDFormatSettings;
  NumberExported: integer;

begin
  Export := TXMLXSDExporter.Create(nil);
  Settings := TXMLXSDFormatSettings.Create(True);
  try
    //Settings.DecimalSeparator := ','; //Don't override decimal separator
    Settings.CreateXSD := True;
    Settings.ExportFormat := AccessCompatible;
    Export.Dataset := FTestDataset;
    Export.FormatSettings := Settings;
    Export.FileName := FExportTempDir + 'Access_XSD_NoDecimal.xml';
    NumberExported := Export.Execute;
    FTestDataset.Close;
    AssertEquals('Number of records exported', NumberExported, FTestDataset.RecordCount);
    //we should have exported 2 records.
  finally
    Settings.Free;
    //DeleteFile(Export.FileName); //todo: fix this for release
    Export.Free;
  end;
end;

procedure Ttestxmlxsdexport1.TestXSDExport_Access_XSD_Decimal;
var
  Export: TXMLXSDExporter;
  Settings: TXMLXSDFormatSettings;
  NumberExported: integer;

begin
  Export := TXMLXSDExporter.Create(nil);
  Settings := TXMLXSDFormatSettings.Create(True);
  try
    Settings.DecimalSeparator := ','; //Try to override decimal separator
    Settings.CreateXSD := True;
    Settings.ExportFormat := AccessCompatible;
    Export.Dataset := FTestDataset;
    Export.FormatSettings := Settings;
    Export.FileName := FExportTempDir + 'Access_XSD_Decimal.xml';
    NumberExported := Export.Execute;
    FTestDataset.Close;
    AssertEquals('Number of records exported', NumberExported, FTestDataset.RecordCount);
    //we should have exported 2 records.
  finally
    Settings.Free;
    //DeleteFile(Export.FileName); //todo: fix this for release
    Export.Free;
  end;
end;

procedure Ttestxmlxsdexport1.TestXSDExport_ADONET_NoXSD;
var
  Export: TXMLXSDExporter;
  Settings: TXMLXSDFormatSettings;
  NumberExported: integer;

begin
  Export := TXMLXSDExporter.Create(nil);
  Settings := TXMLXSDFormatSettings.Create(True);
  try
    Settings.CreateXSD := False;
    Settings.ExportFormat := ADONETCompatible;
    Export.Dataset := FTestDataset;
    Export.FormatSettings := Settings;
    Export.FileName := FExportTempDir + 'ADONET_NoXSD.xml';
    NumberExported := Export.Execute;
    FTestDataset.Close;
    AssertEquals('Number of records exported', NumberExported, FTestDataset.RecordCount);
    //we should have exported 2 records.
  finally
    Settings.Free;
    //DeleteFile(Export.FileName); //todo: fix this for release
    Export.Free;
  end;
end;

procedure Ttestxmlxsdexport1.TestXSDExport_ADONET_XSD;
var
  Export: TXMLXSDExporter;
  Settings: TXMLXSDFormatSettings;
  NumberExported: integer;

begin
  Export := TXMLXSDExporter.Create(nil);
  Settings := TXMLXSDFormatSettings.Create(True);
  try
    Settings.CreateXSD := True;
    Settings.ExportFormat := ADONETCompatible;
    Export.Dataset := FTestDataset;
    Export.FormatSettings := Settings;
    Export.FileName := FExportTempDir + 'ADONET_XSD.xml';
    NumberExported := Export.Execute;
    FTestDataset.Close;
    AssertEquals('Number of records exported', NumberExported, FTestDataset.RecordCount);
    //we should have exported 2 records.
  finally
    Settings.Free;
    //DeleteFile(Export.FileName); //todo: fix this for release
    Export.Free;
  end;
end;

procedure Ttestxmlxsdexport1.TestXSDExport_DelphiClientDataset;
var
  Export: TXMLXSDExporter;
  Settings: TXMLXSDFormatSettings;
  NumberExported: integer;

begin
  Export := TXMLXSDExporter.Create(nil);
  Settings := TXMLXSDFormatSettings.Create(True);
  try
    Settings.ExportFormat := DelphiClientDataset;
    Export.Dataset := FTestDataset;
    Export.FormatSettings := Settings;
    Export.FileName := FExportTempDir + 'DelphiClientDataset.xml';
    NumberExported := Export.Execute;
    FTestDataset.Close;
    AssertEquals('Number of records exported', NumberExported, FTestDataset.RecordCount);
    //we should have exported 2 records.
  finally
    Settings.Free;
    //DeleteFile(Export.FileName); //todo: fix this for release
    Export.Free;
  end;
end;

procedure Ttestxmlxsdexport1.TestXSDExport_Excel;
var
  Export: TXMLXSDExporter;
  Settings: TXMLXSDFormatSettings;
  NumberExported: integer;

begin
  Export := TXMLXSDExporter.Create(nil);
  Settings := TXMLXSDFormatSettings.Create(True);
  try
    Settings.ExportFormat := ExcelCompatible;
    Export.Dataset := FTestDataset;
    Export.FormatSettings := Settings;
    Export.FileName := FExportTempDir + 'Excel.xml';
    NumberExported := Export.Execute;
    FTestDataset.Close;
    AssertEquals('Number of records exported', NumberExported, FTestDataset.RecordCount);
    //we should have exported 2 records.
  finally
    Settings.Free;
    //DeleteFile(Export.FileName); //todo: fix this for release
    Export.Free;
  end;
end;


procedure Ttestxmlxsdexport1.FillTestData;
var
  Utf8teststring: string;
begin
  FTestDataset.Close;
  //for memds:
  //FTestDataset.Clear(False); //memds: clear out any data
  //FTestDataset.Fields.Clear; //bufds: clear out any data, but also FIELDDEFS: don't use
  FTestDataset.Open;

  // Fill some test data
  // First row
  FTestDataset.Append;
  FTestDataset.FieldByName('IDINTEGER').AsInteger := 42;
  FTestDataset.FieldByName('NAMESTRING').AsString := 'Douglas Adams';
  FTestDataset.FieldByName('REMARKSMEMO').AsString :=
    'So long, and thanks for all the fish.';
  FTestDataset.FieldByName('BIRTHDAYDATE').AsDateTime :=
    ScanDateTime('yyyymmdd', '19520311', 1);
  FTestDataset.FieldByName('CVBLOB').AsString :=
    'A very succesful and funny writer. He died some years ago.';
  FTestDataset.FieldByName('POCKETMONEYCURRENCY').AsCurrency := 42.42;
  FTestDataset.FieldByName('NAUGHTYBOOLEAN').AsBoolean := False;
  FTestDataset.Post;

  // Second row
  FTestDataset.Append;
  FTestDataset.FieldByName('IDINTEGER').AsInteger := 444;
  FTestDataset.FieldByName('NAMESTRING').AsString := 'Captain Haddock';
  UTF8TestString :=
    'Drinks rosé (ros, e accent aigu), водка (wodka cyrillic) and ούζο (ouzo Greek) but prefers Loch Lomond whiskey.';
  FTestDataset.FieldByName('REMARKSMEMO').AsString := (UTF8TestString);
  //examples of UTF8 code
  FTestDataset.FieldByName('BIRTHDAYDATE').AsDateTime :=
    ScanDateTime('yyyymmdd', '19401017', 1);
  FTestDataset.FieldByName('CVBLOB').AsString := '';
  FTestDataset.FieldByName('POCKETMONEYCURRENCY').AsCurrency := 12.4866666;
  FTestDataset.FieldByName('NAUGHTYBOOLEAN').AsBoolean := True;
  FTestDataset.Post;
  FTestDataset.Last;
  FTestDataset.First;
  AssertEquals('Number of records in test dataset', 2, FTestDataset.RecordCount);
end;

procedure Ttestxmlxsdexport1.Setup;
var
  FieldDef: TFieldDef;
begin
  FExportTempDir := GetTempDir(False);
  FTestDataset := TBufDataset.Create(nil);
  {for memds - or can we use the lines below, too?:
  FTestDataset.FieldDefs.Add('IDINTEGER', ftInteger);
  FTestDataset.FieldDefs.Add('NAMESTRING', ftString, 50);
  //memds probably won't support memo
  //TestDataset.FieldDefs.Add('REMARKSMEMO', ftMemo);
  TestDataset.FieldDefs.Add('BIRTHDAYDATE', ftDate);
  // No support for blobs in this dataset.
  //TestDataset.FieldDefs.Add('CVBLOB', ftBlob); // A blob containing e.g. text
  TestDataset.FieldDefs.Add('POCKETMONEYCURRENCY', ftCurrency);
  }
  //for Bufdataset:
  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'IDINTEGER';
  FieldDef.DataType := ftInteger;

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'NAMESTRING';
  FieldDef.DataType := ftString;
  FieldDef.Size := 50;

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'REMARKSMEMO';
  FieldDef.DataType := ftMemo;

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'BIRTHDAYDATE';
  FieldDef.DataType := ftDate;

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'CVBLOB';
  FieldDef.DataType := ftBlob;
  FieldDef.Size := 16384;//large but hopefully not too large for memory.

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'POCKETMONEYCURRENCY';
  FieldDef.DataType := ftCurrency;

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'NAUGHTYBOOLEAN';
  FieldDef.DataType := ftBoolean;

  //Createtable is needed if you use a memds
  //FTestDataset.CreateTable;
  //CreateDataset is needed if you use a bufdataset
  FTestDataset.CreateDataSet;

  // Fill dataset with test data
  FillTestData;
end;

procedure Ttestxmlxsdexport1.Teardown;
begin
  FTestDataset.Free;
end;

initialization
  Registertest(Ttestxmlxsdexport1);
end.

