unit dbfexporttestcase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Fpcunit, Testutils, Testregistry, DB, fpdbfexport,
  BufDataset, dateutils;

type

  { TTestDBFExport1 }

  TTestDBFExport1 = class(Ttestcase)
  const
    KeepFilesAfterTest = false;
    //Change if you want to keep export files for further testing
  private
    procedure FillTestData;
  protected
    FTestDataset: TBufDataset;
    FExportTempDir: string; //where we store exported files in these tests
    procedure FillRecord(const RowNumber: integer; const Teststring: string;
      const TestGUID: string; const TestInteger: integer;
      const TestExtended: extended; const TestDatetime: Tdatetime;
      const TestBoolean: boolean);
    procedure Setup; override;
    procedure Teardown; override;
  published
    procedure TestDBExportRuns;
  end;

implementation

function FileSize(FileName: string): integer;
  // LCL has similar function, but we don't want to depend on that.
var
  SearchResult: TSearchRec;
begin
  Result := 0;
  if FindFirst(FileName, faAnyFile, SearchResult) = 0 then
  begin
    try
      Result := SearchResult.Size;
    finally
      FindClose(SearchResult);
    end;
  end;
end;

procedure TTestDBFExport1.TestDBExportRuns;

var
  Export: TFPDBFExport;
  ExportSettings: TDBFExportFormatSettings;
  NumberExported: integer;
begin
  Export := TFPDBFExport.Create(nil);
  ExportSettings:=TDBFExportFormatSettings.Create(true);
  try
    //Don't override decimal separator
    ExportSettings.TableFormat:=tfDBaseVII; //dbase IV seems to have a 10 character field name limit
    Export.FormatSettings:=ExportSettings;
    Export.Dataset := FTestDataset;
    Export.FileName := FExportTempDir + 'dbfexporttest.dbf';
    NumberExported := Export.Execute;
    FTestDataset.Close;
    AssertEquals('Number of records exported', NumberExported, FTestDataset.RecordCount);
    AssertTrue('Output file created', FileExists(Export.FileName));
    AssertTrue('Output file has contents', (FileSize(Export.FileName) > 0));
  finally
    if (KeepFilesAfterTest = False) then
    begin
      DeleteFile(Export.FileName);
    end;
    ExportSettings.Free;
    Export.Free;
  end;
end;


procedure TTestDBFExport1.FillTestData;
var
  RowNumber: integer; //Keep track of how many rows we inserted
  TestBoolean: boolean;
  TestDateTime: TDateTime;
  TestExtended: extended;
  //yes, a lot of precision; we can convert to single/double if required
  TestInteger: integer;
  TestGuid: string;
  TestString: string;
begin
  FTestDataset.Close;
  RowNumber := 0;
  //for memds:
  //FTestDataset.Clear(False); //memds: clear out any data
  //FTestDataset.Fields.Clear; //bufds: clear out any data, but also FIELDDEFS: don't use
  FTestDataset.Open;

  // Fill some test data
  // First row: positive numerical values, late dates/times, strings with special chars (tab, linefeed, ; > <)
  FTestDataset.Append;
  TestBoolean := True;
  TestDateTime := EncodeDate(9999, 12, 31) + EncodeTime(23, 59, 59, 999);
  TestExtended := 42.424242424242424242424242424242;
  TestInteger := Round(TestExtended);
  TestGuid := '{21EC2020-3AEA-1069-A2DD-08002B30309D}';
  TestString := 'Douglas Adams less than: < greater than > tab:' +
    #9 + 'crlf:' + #13 + #10 +
    '國缺界广欠廣界界东缺. Haddock drinks rosé (ros, e accent aigu), водка (wodka cyrillic) and ούζο (ouzo Greek) but prefers Loch Lomond whiskey.';
  RowNumber := RowNumber + 1;
  FillRecord(RowNumber, Teststring, TestGUID, Testinteger, Testextended,
    Testdatetime, Testboolean);
  FTestDataset.Post;

  // Second row: negative numerical values, early dates/times, strings with maximum field width and Greek, east asian (multibyte) characters
  FTestDataset.Append;
  TestBoolean := False;
  TestDateTime := EncodeDate(1, 1, 1) + EncodeTime(0, 0, 0, 1);
  TestExtended := -42.424242424242424242424242424242;
  TestInteger := Round(TestExtended);
  TestGuid := '{FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF}';
  TestString := 'ARMA virumque cano, Troiae qui primus ab oris' +
    #13 + #10 + 'Italiam, fato profugus, Laviniaque venit' + #13 +
    #10 + 'litora, multum ille et terris iactatus et alto' + #13 +
    #10 + 'vi superum saevae memorem Iunonis ob iram;' + #13 + #10 +
    'multa quoque et bello passus, dum conderet urbem,' + #13 + #10 +
    'inferretque deos Latio, genus unde Latinum,' + #13 + #10 +
    'Albanique patres, atque altae moenia Romae.' + #13 + #10 + #13 +
    #10 + 'Musa, mihi causas memora, quo numine laeso,' + #13 + #10 +
    'quidve dolens, regina deum tot volvere casus' + #13 + #10 +
    'insignem pietate virum, tot adire labores' + #13 + #10 +
    'impulerit.  Tantaene animis caelestibus irae?' + #13 + #10 +
    #13 + #10 + 'Urbs antiqua fuit, Tyrii tenuere coloni,' + #13 +
    #10 + 'Karthago, Italiam contra Tiberinaque longe' + #13 + #10 +
    'ostia, dives opum studiisque asperrima belli;' + #13 + #10 +
    'quam Iuno fertur terris magis omnibus unam' + #13 + #10 +
    'posthabita coluisse Samo; hic illius arma,' + #13 + #10 +
    'hic currus fuit; hoc regnum dea gentibus esse,' + #13 + #10 +
    'si qua fata sinant, iam tum tenditque fovetque.' + #13 + #10 +
    'Progeniem sed enim Troiano a sanguine duci' + #13 + #10 +
    'audierat, Tyrias olim quae verteret arces;' + #13 + #10 +
    'hinc populum late regem belloque superbum' + #13 + #10 +
    'venturum excidio Libyae:  sic volvere Parcas.' + #13 + #10 +
    'Id metuens, veterisque memor Saturnia belli,' + #13 + #10 +
    'prima quod ad Troiam pro caris gesserat Argis---' + #13 + #10 +
    'necdum etiam causae irarum saevique dolores' + #13 + #10 +
    'exciderant animo:  manet alta mente repostum' + #13 + #10 +
    'iudicium Paridis spretaeque iniuria formae,' + #13 + #10 +
    'et genus invisum, et rapti Ganymedis honores.' + #13 + #10 +
    'His accensa super, iactatos aequore toto' + #13 + #10 +
    'Troas, reliquias Danaum atque immitis Achilli,' + #13 + #10 +
    'arcebat longe Latio, multosque per annos' + #13 + #10 +
    'errabant, acti fatis, maria omnia circum.' + #13 + #10 +
    'Tantae molis erat Romanam condere gentem!';
  RowNumber := RowNumber + 1;
  FillRecord(RowNumber, Teststring, TestGUID, Testinteger, Testextended,
    Testdatetime, Testboolean);
  FTestDataset.Post;

  // Third row: empty/zero numerical values, dates/times, strings
  FTestDataset.Append;
  TestBoolean := False;
  TestDateTime := EncodeDate(1, 1, 1) + EncodeTime(0, 0, 0, 0);
  TestExtended := 0;
  TestInteger := Round(TestExtended);
  TestGuid := '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}';
  TestString := '';
  RowNumber := RowNumber + 1;
  FillRecord(RowNumber, Teststring, TestGUID, Testinteger, Testextended,
    Testdatetime, Testboolean);
  FTestDataset.Post;

  // Fourth row: plausible data
  FTestDataset.Append;
  TestBoolean := True;
  TestDateTime := EncodeDate(2005, 9, 10) + EncodeTime(13, 52, 18, 0);
  TestExtended := 42;
  TestInteger := Round(TestExtended);
  TestString := 'The answer to life, the universe, and everything';
  RowNumber := RowNumber + 1;
  FillRecord(RowNumber, Teststring, TestGUID, Testinteger, Testextended,
    Testdatetime, Testboolean);
  FTestDataset.Post;

  // Make sure recordcount is correct:
  FTestDataset.Last;
  FTestDataset.First;
  AssertEquals('Number of records in test dataset', RowNumber, FTestDataset.RecordCount);
end;

procedure TTestDBFExport1.Setup;
const
  NumberOfDecimals = 2;
  NumberOfBytes = 10;
var
  FieldDef: TFieldDef;
begin
  FExportTempDir := GetTempDir(False);
  FTestDataset := TBufDataset.Create(nil);
  {Tweaked for dbf export}
  {We should cover all data types defined in FPC:

  FPC maps "external" types such as ftOracleBlob to
  internal types, but that can be overridden, which is done
  by e.g. IBX and mseide.
  So it makes sense to keep as many datatypes in the exporter code as possible: it documents the mappings and allows other people to use these types without the exporter breaking.
  }
  {Sorted by datatype; commented out what doesn't work at the moment in bufdataset
  See http://docwiki.embarcadero.com/VCL/en/DB.TField.Size for overview of field sizes in the competition product ;)
  Apparently ftGuid also needs size...
  }

  {
  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftADT';
  FieldDef.DataType := ftADT;
  FieldDef.Size := 4096;//large but hopefully not too large for memory.
  }

  {
  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftArray';
  FieldDef.DataType := ftArray;
  FieldDef.Size := 10;//the number of elements in the array

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftAutoInc';
  FieldDef.DataType := ftAutoInc;
  }

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftBCD';
  FieldDef.DataType := ftBCD;
  FieldDef.Size := NumberOfDecimals;
  //Size is the number of digits after the decimal place

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  //Note dbf 3 has a 10 character field length limit
  FieldDef.Name := 'ftBlob_4096';
  FieldDef.DataType := ftBlob;
  FieldDef.Size := 4096;//large but hopefully not too large for memory.

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftBoolean';
  FieldDef.DataType := ftBoolean;

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftBytes';
  FieldDef.DataType := ftBytes;
  FieldDef.Size := NumberOfBytes;

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftCurrency';
  FieldDef.DataType := ftCurrency;

  {
  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftCursor';
  FieldDef.DataType := ftCursor;
  }

  {
  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftDataSet';
  FieldDef.DataType := ftDataSet;
  }

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftDate';
  FieldDef.DataType := ftDate;

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftDateTime';
  FieldDef.DataType := ftDateTime;

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftDBaseOle';
  FieldDef.DataType := ftDBaseOle;
  FieldDef.Size := 4096;//large but hopefully not too large for memory.

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftFixedChar_2';
  FieldDef.DataType := ftFixedChar;
  FieldDef.Size := NumberOfDecimals;

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftFixedWideChar_2';
  FieldDef.DataType := ftFixedWideChar;
  FieldDef.Size := NumberOfBytes;

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftFloat';
  FieldDef.DataType := ftFloat;

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftFMTBcd';
  FieldDef.DataType := ftFMTBcd;
  FieldDef.Size := NumberOfDecimals; //the number of digits after the decimal place.

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftFmtMemo';
  FieldDef.DataType := ftFmtMemo;
  FieldDef.Size := 4096;//large but hopefully not too large for memory.

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftGraphic';
  FieldDef.DataType := ftGraphic;
  FieldDef.Size := 4096;//large but hopefully not too large for memory.

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftGuid';
  FieldDef.DataType := ftGuid;
  FieldDef.Size := 38;
  //Apparently right answer is not 42; had to look up 38 in source code.

  {
  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftIDispatch';
  FieldDef.DataType := ftIDispatch;
  }

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftInteger';
  FieldDef.DataType := ftInteger;

  {
  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftInterface';
  FieldDef.DataType := ftInterface;
  }

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftLargeint';
  FieldDef.DataType := ftLargeint;

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftMemo';
  FieldDef.DataType := ftMemo;
  FieldDef.Size := 4096;//large but hopefully not too large for memory.

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftOraBlob';
  FieldDef.DataType := ftOraBlob;

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftOraClob';
  FieldDef.DataType := ftOraClob;

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftParadoxOle';
  FieldDef.DataType := ftParadoxOle;
  FieldDef.Size := 4096;//large but hopefully not too large for memory.

  {
  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftReference';
  FieldDef.DataType := ftReference;
  }

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftSmallInt';
  FieldDef.DataType := ftInteger;

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftString_1';
  FieldDef.DataType := ftString;
  FieldDef.Size := 1;

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftString_256'; //1 character more than many db string types support
  FieldDef.DataType := ftString;
  FieldDef.Size := 256;

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftTime';
  FieldDef.DataType := ftTime;

  {
  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftTimeStamp';
  FieldDef.DataType := ftTimeStamp;
  }

  {
  //Bufdataset probably doesn't support this
  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  // DBF 10 character limit comes into play here:
  FieldDef.Name := 'ftTypedBin';
  FieldDef.DataType := ftTypedBinary;
  FieldDef.Size := 4096;//large but hopefully not too large for memory.
  }

  FieldDef.Name := 'ftVariant';
  FieldDef.DataType := ftVariant;
  FieldDef.Size := NumberOfBytes;

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftVarBytes';
  FieldDef.DataType := ftVarBytes;
  FieldDef.Size := NumberOfBytes;

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftWideMemo';
  FieldDef.DataType := ftWideMemo;

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftWideString256';
  FieldDef.DataType := ftWideString;
  FieldDef.Size := 256;

  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'ftWord';
  FieldDef.DataType := ftWord;
  
  //Finally, a long field name that should trigger
  //field renaming code in dbf export
  //(dbase VII supports up to 32 characters, others up to 10)
  FieldDef := FTestDataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'AVeryLongFieldDataTypeDoesNotMatter';
  FieldDef.DataType := ftString;
  FieldDef.Size := 256;

  //Createtable is needed if you use a memds
  //FTestDataset.CreateTable;
  //CreateDataset is needed if you use a bufdataset
  FTestDataset.CreateDataSet;

  // Fill dataset with test data
  FillTestData;
end;

procedure TTestDBFExport1.FillRecord(const RowNumber: integer;
  const TestString: string; const TestGUID: string; const TestInteger: integer;
  const TestExtended: extended; const TestDatetime: Tdatetime;
  const TestBoolean: boolean);
var
  FieldCounter: integer;
begin
  {As our bufdataset doesn't support these datatypes, don't use them:
ftAutoInc -> exists but doesn't seem to return any data.
ftCursor
ftDataSet
ftInterface
ftReference
ftTimeStamp}

  FTestDataset.FieldByName('ftBCD').AsFloat := Testextended;
  FTestDataset.FieldByName('ftBlob_4096').AsString := Teststring;
  FTestDataset.FieldByName('ftBoolean').AsBoolean := Testboolean;
  FTestDataset.FieldByName('ftBytes').AsString := Teststring;
  FTestDataset.FieldByName('ftCurrency').Ascurrency := Testextended;
  FTestDataset.FieldByName('ftDate').AsDateTime := Testdatetime;
  FTestDataset.FieldByName('ftDateTime').AsDateTime := Testdatetime;
  FTestDataset.FieldByName('ftDBaseOle').AsString := Teststring;
  FTestDataset.FieldByName('ftFixedChar_2').AsString := Teststring;
  FTestDataset.FieldByName('ftFixedWideChar_2').AsString := Teststring;
  FTestDataset.FieldByName('ftFloat').AsFloat := Testextended;
  FTestDataset.FieldByName('ftFMTBcd').AsFloat := Testextended;
  FTestDataset.FieldByName('ftFmtMemo').AsString := Teststring;
  FTestDataset.FieldByName('ftGraphic').AsString := Teststring;
  FTestDataset.FieldByName('ftGuid').AsString := TestGUID;
  FTestDataset.FieldByName('ftInteger').AsInteger := Testinteger;
  FTestDataset.FieldByName('ftLargeint').AsInteger := Testinteger;
  FTestDataset.FieldByName('ftMemo').AsString := Teststring;
  FTestDataset.FieldByName('ftOraBlob').AsString := Teststring;
  {
  FTestDataset.FieldByName('ftOraClob').AsString := Teststring;
  }
  FTestDataset.FieldByName('ftParadoxOle').AsString := Teststring;
  FTestDataset.FieldByName('ftSmallInt').AsInteger := Testinteger;
  FTestDataset.FieldByName('ftString_1').AsString := Teststring;
  FTestDataset.FieldByName('ftString_256').AsString := Teststring;
  FTestDataset.FieldByName('ftTime').AsDateTime := Testdatetime;
  {
  FTestDataset.FieldByName('ftTypedBin').AsString := Teststring;
  }
  FTestDataSet.FieldByName('ftVarBytes').AsString := TestString;
  FTestDataSet.FieldByName('ftVariant').AsString := TestString;
  FTestDataset.FieldByName('ftWideMemo').AsString := Teststring;
  FTestDataset.FieldByName('ftWideString256').AsString := Teststring;
  FTestDataset.FieldByName('ftWord').AsInteger := Abs(Testinteger);
  FTestDataset.FieldByName('AVeryLongFieldDataTypeDoesNotMatter').AsString := Teststring;
end;

procedure TTestDBFExport1.Teardown;
begin
  FTestDataset.Free;
end;

initialization
  Registertest(TTestDBFExport1);
end.

