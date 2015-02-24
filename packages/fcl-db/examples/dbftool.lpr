program dbftool;

{
Reads and exports DBF files.
Can create a set of 2 demo DBF files in current directory to test with.

Demonstrates creating DBF tables, filling it with data,
and exporting datasets.
}

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  DB,
  dbf,
  dbf_fields,
  dbf_common,
  dateutils,
  fpdbexport,
  fpcsvexport,
  fpdbfexport,
  fpfixedexport,
  fprtfexport,
  fpsimplejsonexport,
  fpsimplexmlexport,
  fpsqlexport,
  fptexexport,
  fpxmlxsdexport;


type

  { TDBFTool }

  TDBFTool = class(TCustomApplication)
  private
    // Exports recordset to specified format
    procedure ExportDBF(var MyDbf: TDbf; ExportFormat: string);
    // Executable name without path
    function GetExeName: string;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  // Creates 2 demonstration DBFs in Directory
  // with dbase compatibility level TableLevel
  procedure CreateDemoDBFs(Directory: string; TableLevel: integer);
  // Data structure and data adapted from Firebird employee sample database that
  // are also used in the SQLDB tutorials on Lazarus wiki/demo directory.
  var
    CurDir: string; //current directory
    NewDBF: TDBF;
    i: integer;
  begin
    // Get current working directory (need not be application directory):
    CurDir := '';
    GetDir(0,CurDir);

    NewDBF := TDBF.Create(nil);
    try
      if Directory = '' then
      begin
        NewDBF.FilePathFull := ExpandFileName(CurDir);
      end
      else
        NewDBF.FilePathFull := ExpandFileName(Directory) {full absolute path};
      if TableLevel <= 0 then
        NewDBF.TableLevel := 4 {default to DBase IV}
      else
        NewDBF.TableLevel := TableLevel;

      NewDBF.TableName := 'customer.dbf';
      writeln('Creating ', NewDBF.TableName, ' with table level ', NewDBF.TableLevel);			
      if TableLevel >= 30 {Visual FoxPro} then
      begin
        NewDBF.FieldDefs.Add('CUST_NO', ftAutoInc);
      end
      else
        NewDBF.FieldDefs.Add('CUST_NO', ftInteger);
      NewDBF.FieldDefs.Add('CUSTOMER', ftString, 25);
      NewDBF.FieldDefs.Add('CITY', ftString, 25);
      NewDBF.FieldDefs.Add('COUNTRY', ftString, 15);
      NewDBF.CreateTable;
      NewDBF.Open;

      for i := 1 to 5 do //keep size manageable until we have working files
      begin
        NewDBF.Append;
        if (NewDBF.FieldDefs.Find('CUST_NO').DataType <> ftAutoInc) then
          NewDBF.FieldByName('CUST_NO').AsInteger := i;
        case i of
          1:
          begin
            NewDBF.FieldByName('CUSTOMER').AsString := 'Michael Design';
            NewDBF.FieldByName('CITY').AsString := 'San Diego';
            NewDBF.FieldByName('COUNTRY').AsString := 'USA';
          end;
          2: //Let's try a duplicate row
          begin
            NewDBF.FieldByName('CUSTOMER').AsString := 'Michael Design';
            NewDBF.FieldByName('CITY').AsString := 'San Diego';
            NewDBF.FieldByName('COUNTRY').AsString := 'USA';
          end;
          3:
          begin
            NewDBF.FieldByName('CUSTOMER').AsString := 'VC Technologies';
            NewDBF.FieldByName('CITY').AsString := 'Dallas';
            NewDBF.FieldByName('COUNTRY').AsString := 'USA';
          end;
          4:
          begin
            NewDBF.FieldByName('CUSTOMER').AsString := 'Klämpfl, Van Canneyt';
            NewDBF.FieldByName('CITY').AsString := 'Boston';
            NewDBF.FieldByName('COUNTRY').AsString := 'USA';
          end;
          5:
          begin
            NewDBF.FieldByName('CUSTOMER').AsString := 'Felipe''s Bank';
            NewDBF.FieldByName('CITY').AsString := 'Manchester';
            NewDBF.FieldByName('COUNTRY').AsString := 'England';
          end;
        end;
        NewDBF.Post;
      end;
      NewDBF.Close;
    finally
      NewDBF.Free;
    end;

    NewDBF := TDBF.Create(nil);
    try
      if Directory = '' then
        NewDBF.FilePathFull := ExpandFileName(CurDir)
      else
        NewDBF.FilePathFull := ExpandFileName(Directory) {full absolute path};
      if TableLevel <= 0 then
        NewDBF.TableLevel := 4 {default to DBase IV}
      else
        NewDBF.TableLevel := TableLevel;

      NewDBF.TableName := 'employee.dbf';
      writeln('Creating ', NewDBF.TableName, ' with table level ', NewDBF.TableLevel);
      if TableLevel >= 30 {Visual FoxPro} then
      begin
        NewDBF.FieldDefs.Add('EMP_NO', ftAutoInc);
      end
      else
        NewDBF.FieldDefs.Add('EMP_NO', ftInteger);
      NewDBF.FieldDefs.Add('FIRST_NAME', ftString, 15);
      NewDBF.FieldDefs.Add('LAST_NAME', ftString, 20);
      NewDBF.FieldDefs.Add('PHONE_EXT', ftString, 4);
      NewDBF.FieldDefs.Add('JOB_CODE', ftString, 5);
      NewDBF.FieldDefs.Add('JOB_GRADE', ftInteger);
      NewDBF.FieldDefs.Add('JOB_COUNTR', ftString, 15); //Note 10 character limit for table/field names in most DBases
      NewDBF.FieldDefs.Add('SALARY', ftFloat);
      NewDBF.CreateTable;
      NewDBF.Open;

      for i := 1 to 5 do //keep size manageable until we have working files
      begin
        NewDBF.Append;
        if (NewDBF.FieldDefs.Find('EMP_NO').DataType <> ftAutoInc) then
          NewDBF.FieldByName('EMP_NO').AsInteger := i;
        case i of
          1:
          begin
            NewDBF.FieldByName('FIRST_NAME').AsString := 'William';
            NewDBF.FieldByName('LAST_NAME').AsString := 'Shatner';
            NewDBF.FieldByName('PHONE_EXT').AsString := '1702';
            NewDBF.FieldByName('JOB_CODE').AsString := 'CEO';
            NewDBF.FieldByName('JOB_GRADE').AsInteger := 1;
            NewDBF.FieldByName('JOB_COUNTR').AsString := 'USA';
            NewDBF.FieldByName('SALARY').AsFloat := 48000;
          end;
          2:
          begin
            NewDBF.FieldByName('FIRST_NAME').AsString := 'Ivan';
            NewDBF.FieldByName('LAST_NAME').AsString := 'Ishenin';
            NewDBF.FieldByName('PHONE_EXT').AsString := '9802';
            NewDBF.FieldByName('JOB_CODE').AsString := 'Eng';
            NewDBF.FieldByName('JOB_GRADE').AsInteger := 2;
            NewDBF.FieldByName('JOB_COUNTR').AsString := 'Russia';
            NewDBF.FieldByName('SALARY').AsFloat := 38000;
          end;
          3:
          begin
            NewDBF.FieldByName('FIRST_NAME').AsString := 'Erin';
            NewDBF.FieldByName('LAST_NAME').AsString := 'Powell';
            NewDBF.FieldByName('PHONE_EXT').AsString := '1703';
            NewDBF.FieldByName('JOB_CODE').AsString := 'Admin';
            NewDBF.FieldByName('JOB_GRADE').AsInteger := 2;
            NewDBF.FieldByName('JOB_COUNTR').AsString := 'USA';
            NewDBF.FieldByName('SALARY').AsFloat := 45368;
          end;
          4:
          begin
            NewDBF.FieldByName('FIRST_NAME').AsString := 'Margaret';
            NewDBF.FieldByName('LAST_NAME').AsString := 'Tetchy';
            NewDBF.FieldByName('PHONE_EXT').AsString := '3804';
            NewDBF.FieldByName('JOB_CODE').AsString := 'Eng';
            NewDBF.FieldByName('JOB_GRADE').AsInteger := 3;
            NewDBF.FieldByName('JOB_COUNTR').AsString := 'England';
            NewDBF.FieldByName('SALARY').AsFloat := 28045;
          end;
          5:
          begin
            NewDBF.FieldByName('FIRST_NAME').AsString := 'Sergey';
            NewDBF.FieldByName('LAST_NAME').AsString := 'Bron';
            NewDBF.FieldByName('PHONE_EXT').AsString := '3807';
            NewDBF.FieldByName('JOB_CODE').AsString := 'Admin';
            NewDBF.FieldByName('JOB_GRADE').AsInteger := 3;
            NewDBF.FieldByName('JOB_COUNTR').AsString := 'England';
            NewDBF.FieldByName('SALARY').AsFloat := 24468;
          end;
        end;
        NewDBF.Post;
      end;
      NewDBF.Close;
    finally
      NewDBF.Free;
    end;
  end;

  // Gets list of all .dbf files in current directory and its subdirectories.
  procedure GetDBFList(Results: TStringList);
  var
    r: TSearchRec;
  begin
    results.Clear;
    if FindFirst('*', faAnyFile - faDirectory -
{$WARNINGS OFF}
      faVolumeID - faSymLink
{$WARNINGS ON}
      , r) = 0 then
    begin
      repeat
      begin
        // Cater for both case-sensitive and case-insensitive filesystems
        // ignore any directories
        if ((r.Attr and faDirectory) <> faDirectory) and
          (LowerCase(ExtractFileExt(r.Name))='.dbf') then
          results.add(expandfilename(r.Name));
      end;
      until (FindNext(r) <> 0);
      findclose(r);
    end;
  end;

  // Convert binary field contents to strings with hexadecimal representation.
  // Useful for displaying binary field contents.
  function BinFieldToHex(BinarySource: TField): string;
  var
    HexValue: PChar;
  begin
    Result := '';
    HexValue := StrAlloc(Length(BinarySource.AsBytes));
    try
      try
        BinToHex(PChar(BinarySource.AsBytes), HexValue, Length(BinarySource.AsBytes));
        Result := 'size: ' + IntToStr(Length(BinarySource.AsBytes)) + '; hex: ' + HexValue;
      except
        on E: Exception do
        begin
          Result := 'exception: ' + E.ClassName + '/' + E.Message;
        end;
      end;
    finally
      StrDispose(HexValue);
    end;
  end;

  // Writes contents of available records to screen
  procedure PrintRecords(DBf: TDBf);
  var
    i: integer;
    RecordCount: integer;
  begin
    Dbf.First;
    RecordCount:=0;
    while not (Dbf.EOF) do
    begin
      RecordCount := RecordCount + 1;
      writeln('Record ' + IntToStr(RecordCount));
      for i := 0 to DBf.Fields.Count - 1 do
      begin
        if DBF.fields[i].IsNull then
          writeln('Field ', DBf.Fields[i].FieldName, ' is          ***NULL***')
        else
        if DBF.Fields[i].DataType in [ftVarBytes, ftBytes] then
          writeln('Field ', DBF.Fields[i].FieldName, ' has value: binary ' + BinFieldToHex(DBF.Fields[i]))
        else
          writeln('Field ', DBf.Fields[i].FieldName, ' has value: ' + DBf.fields[i].AsString);
      end;
      DBF.Next;
      writeln('');
    end;
  end;

  { TDBFTool }

  procedure TDBFTool.ExportDBF(var MyDbf: TDbf; ExportFormat: string);
  var
    ExportSettings: TCustomExportFormatSettings;
    Exporter: TCustomFileExporter;
  begin
    try
      case UpperCase(ExportFormat) of
        'ACCESS', 'MSACCESS':
        begin
          Exporter := TXMLXSDExporter.Create(nil);
          ExportSettings := TXMLXSDFormatSettings.Create(true);
          (ExportSettings as TXMLXSDFormatSettings).CreateXSD := true;
          (ExportSettings as TXMLXSDFormatSettings).ExportFormat :=
            AccessCompatible;
          (ExportSettings as TXMLXSDFormatSettings).DecimalSeparator := '.';
          Exporter.FileName := MyDBF.FilePathFull + ChangeFileExt(MyDBF.TableName, '.xml');
        end;
        'ADO', 'ADONET', 'ADO.NET':
        begin
          Exporter := TXMLXSDExporter.Create(nil);
          ExportSettings := TXMLXSDFormatSettings.Create(true);
          (ExportSettings as TXMLXSDFormatSettings).CreateXSD := true;
          (ExportSettings as TXMLXSDFormatSettings).ExportFormat :=
            ADONETCompatible;
          (ExportSettings as TXMLXSDFormatSettings).DecimalSeparator := '.';
          Exporter.FileName := MyDBF.FilePathFull + ChangeFileExt(MyDBF.TableName, '.xml');
        end;
        'CSVEXCEL', 'EXCELCSV', 'CREATIVYST':
        begin
          Exporter := TCSVExporter.Create(nil);
          ExportSettings := TCSVFormatSettings.Create(true);
          (ExportSettings as TCSVFormatSettings).RowDelimiter:=LineEnding;
          //todo: delimiter?
          Exporter.FileName := MyDBF.FilePathFull + ChangeFileExt(MyDBF.TableName, '.csv');
        end;
        'CSV', 'CSVRFC4180', 'CSVLIBRE', 'CSVLIBREOFFICE', 'CSVOPENOFFICE':
        begin
          Exporter := TCSVExporter.Create(nil);
          ExportSettings := TCSVFormatSettings.Create(true);
          (ExportSettings as TCSVFormatSettings).DecimalSeparator := '.';
          (ExportSettings as TCSVFormatSettings).StringQuoteChar := '"';
          Exporter.FileName := MyDBF.FilePathFull + ChangeFileExt(MyDBF.TableName, '.csv');
        end;
        'DATASET', 'DELPHI':
        begin
          Exporter := TXMLXSDExporter.Create(nil);
          ExportSettings := TXMLXSDFormatSettings.Create(true);
          (ExportSettings as TXMLXSDFormatSettings).ExportFormat :=
            DelphiClientDataset;
          (ExportSettings as TXMLXSDFormatSettings).DecimalSeparator := '.';
          Exporter.FileName := MyDBF.FilePathFull + ChangeFileExt(MyDBF.TableName, '.xml');
        end;
        'EXCEL', 'EXCELXML':
        begin
          Exporter := TXMLXSDExporter.Create(nil);
          ExportSettings := TXMLXSDFormatSettings.Create(true);
          (ExportSettings as TXMLXSDFormatSettings).ExportFormat := ExcelCompatible;
          (ExportSettings as TXMLXSDFormatSettings).DecimalSeparator := '.';
          Exporter.FileName := MyDBF.FilePathFull + ChangeFileExt(MyDBF.TableName, '.xml');
        end;
        'JSON':
        begin
          Exporter := TSimpleJSONExporter.Create(nil);
          ExportSettings := TSimpleJSONFormatSettings.Create(true);
          Exporter.FileName := MyDBF.FilePathFull + ChangeFileExt(MyDBF.TableName, '.json');
        end;
        'SIMPLEXML', 'XML':
        begin
          Exporter := TSimpleXMLExporter.Create(nil);
          ExportSettings := TSimpleXMLFormatSettings.Create(true);
          Exporter.FileName := MyDBF.FilePathFull + ChangeFileExt(MyDBF.TableName, '.xml');
        end;
        'RTF':
        begin
          Exporter := TRTFExporter.Create(nil);
          ExportSettings := TSimpleXMLFormatSettings.Create(true);
          Exporter.FileName := MyDBF.FilePathFull + ChangeFileExt(MyDBF.TableName, '.rtf');
        end;
        'SQL':
        begin
          Exporter := TSQLExporter.Create(nil);
          ExportSettings := TSQLFormatSettings.Create(true);
          (ExportSettings as TSQLFormatSettings).QuoteChar := '"';
          (ExportSettings as TSQLFormatSettings).DecimalSeparator := '.';
          (ExportSettings as TSQLFormatSettings).TableName := ChangeFileExt(MyDBF.TableName,'');
          (ExportSettings as TSQLFormatSettings).DateFormat := 'yyyy"-"mm"-"dd'; //ISO 8601, yyyy-mm-dd
          (ExportSettings as TSQLFormatSettings).TimeFormat := 'hh":"nn":"ss';   //ISO 8601, hh:mm:ss;
          (ExportSettings as TSQLFormatSettings).DateTimeFormat :=
            (ExportSettings as TSQLFormatSettings).DateFormat + '"T"' + (ExportSettings as TSQLFormatSettings).TimeFormat; //ISO 8601
          Exporter.FileName := MyDBF.FilePathFull + ChangeFileExt(MyDBF.TableName, '.sql');
        end;
        'TEX', 'LATEX':
        begin
          Exporter := TTeXExporter.Create(nil);
          ExportSettings := TTeXExportFormatSettings.Create(true);
          Exporter.FileName := MyDBF.FilePathFull + ChangeFileExt(MyDBF.TableName, '.tex');
        end;
        'TEXT', 'FIXED', 'FIXEDTEXT':
        begin
          Exporter := TFixedLengthExporter.Create(nil);
          ExportSettings := nil;
          Exporter.FileName := MyDBF.FilePathFull + ChangeFileExt(MyDBF.TableName, '.txt');
        end
        else
        begin
          writeln('***Error: Unknown export format ' + ExportFormat + ' specified' + '. Aborting');
          Exporter := nil;
          ExportSettings := nil;
          Terminate;
          Exit;
        end;
      end;
      if assigned(ExportSettings) then
        Exporter.FormatSettings := ExportSettings;
      Exporter.Dataset := MyDBF;
      MyDBF.First; // we've just read the last record - make sure export starts at beginning
      Exporter.Execute;
      writeln('Completed export to ' + Exporter.FileName);
    finally
      if assigned(Exporter) then
        Exporter.Free;
      if assigned(ExportSettings) then
        ExportSettings.Free;
    end;
  end;

  function TDBFTool.GetExeName: string;
  begin
    result := ExtractFileName(ExeName);
  end;

  procedure TDBFTool.DoRun;
  var
    DBFs: TStringList;
    Demo: boolean;
    ErrorMsg: string;
    FileNo: integer;
    MyDbf: TDbf;
    TableLevel: integer;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('h', 'createdemo exportformat: help tablelevel:');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    DBFs := TStringList.Create;
    try
      Demo := false;
      if HasOption('createdemo') then
        Demo := true;

      TableLevel := 4; //DBF
      if HasOption('tablelevel') then
        TableLevel := StrToIntDef(GetOptionValue('tablelevel'), 4);

      if Demo then
      begin
        try
          CreateDemoDBFs('', TableLevel);
        except
          on E: Exception do
          begin
            writeln('*** Error creating demo databases: ' + E.Message);
            Terminate;
            Exit;
          end;
        end;
      end;

      // Process all dbfs if no files specified
      if DBFs.Count = 0 then
        GetDBFList(DBFs);

      if DBFs.Count = 0 then
      begin
        writeln('Could not find any dbf files.');
        writeln('Use ' + GetExeName + ' --createdemo to create some test DBF files.');
      end;

      for FileNo := 0 to DBFs.Count - 1 do
      begin
        if not (fileexists(DBFs[FileNo])) then
        begin
          writeln('Sorry, file ',DBFs[FileNo],' does not exist. Ignoring it.');
          continue;
        end;
        MyDbf := TDbf.Create(nil);
        try
          try
            MyDbf.FilePath := ExtractFilePath(DBFs[FileNo]);
            MyDbf.TableName := ExtractFileName(DBFs[FileNo]);
            MyDbf.ReadOnly := true;
            writeln('*** Opening: ' + DBFs[FileNo]);
            MyDbf.Open;
            writeln('Database tablelevel: ' + IntToStr(MyDbf.TableLevel));
            writeln('Database codepage:   ' + IntToStr(MyDBF.CodePage));

            PrintRecords(MyDBF);

            if HasOption('exportformat') then
            begin
              try
                ExportDBF(MyDbf,GetOptionValue('exportformat'));
              except
                on E: Exception do
                begin
                  writeln('*** Problem exporting file ', FileNo, ': ', E.Message);
                end;
              end;
            end;

            MyDbf.Close;
          except
            on E: Exception do
            begin
              writeln('*** Error reading file ', FileNo, ': ', E.Message);
            end;
          end;
        finally
          MyDbf.Free;
        end;
      end;
    finally
      DBFs.Free;
    end;

    // stop program loop
    Terminate;
  end;

  constructor TDBFTool.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := true;
  end;

  destructor TDBFTool.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TDBFTool.WriteHelp;
  begin
    writeln('Read/print all dbfs in current directory');
    writeln('Usage info: ', GetExeName, ' -h');
    writeln('');
    writeln('--createdemo          create demo database in current directory');
    writeln('--tablelevel=<n>      optional: desired tablelevel for demo db');
    writeln('  3                    DBase III');
    writeln('  4                    DBase IV (default if no tablelevel given)');
    writeln('  7                    Visual DBase 7');
    writeln(' 25                    FoxPro 2.x');
    writeln(' 30                    Visual FoxPro');
    writeln('--exportformat=<text> export dbfs to format. Format can be:');
    writeln(' access                Microsoft Access XML');
    writeln(' adonet                ADO.Net dataset XML');
    writeln(' csvexcel              Excel/Creativyst format CSV text file ');
    writeln('                       (with locale dependent output)');
    writeln(' csvRFC4180            LibreOffice/RFC4180 format CSV text file');
    writeln(' dataset               Delphi dataset XML');
    writeln(' excel                 Microsoft Excel XML');
    writeln(' fixedtext             Fixed length text file');
    writeln(' json                  JSON file');
    writeln(' rtf                   Rich Text Format');
    writeln(' simplexml             Simple XML');
    writeln(' sql                   SQL insert statements');
    writeln(' tex                   LaTeX file');
  end;

var
  Application: TDBFTool;
begin
  Application := TDBFTool.Create(nil);
  Application.Title := 'DBFTool';
  Application.Run;
  Application.Free;
end.
