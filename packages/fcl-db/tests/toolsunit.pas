unit ToolsUnit;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB;
  
Const MaxDataSet = 35;
  
type

  { TDBConnector }
  TDBConnectorClass = class of TDBConnector;
  TDBConnector = class(TPersistent)
     private
       FChangedDatasets : array[0..MaxDataSet] of boolean;
       FUsedDatasets : TFPList;
       FChangedFieldDataset : boolean;
     protected
       // These methods should be implemented by any descendents
       // They are called eacht time a test need a TDataset descendent
       Function InternalGetNDataset(n : integer) : TDataset;  virtual; abstract;
       Function InternalGetFieldDataset : TDataSet; virtual; abstract;

       // These methods should be implemented by any descendents
       // They are called only once in the constructor. They can be used
       // to create the tables on disk, or on a DB-Server
       procedure CreateNDatasets; virtual; abstract;
       procedure CreateFieldDataset; virtual; abstract;

       // These methods are called after each test in which a dataset is used
       // by calling GetXXXDataset with Achange=true
       // They should reset all data to their right/initial values.
       procedure ResetNDatasets; virtual;
       procedure ResetFieldDataset; virtual;
       
       // These methods are called only once in the destructor.
       // They should clean up all mess, like tables on disk or on a DB-server
       procedure DropNDatasets; virtual; abstract;
       procedure DropFieldDataset; virtual; abstract;
     public
       constructor create; virtual;
       destructor destroy; override;

       procedure DataEvent(dataset :TDataset);

       Function GetNDataset(n : integer) : TDataset;  overload;
       Function GetNDataset(AChange : Boolean; n : integer) : TDataset;  overload;
       Function GetFieldDataset : TDataSet; overload;
       Function GetFieldDataset(AChange : Boolean) : TDataSet; overload;

       procedure StartTest;
       procedure StopTest;
     end;


{ TTestDataLink }

  TTestDataLink = class(TDataLink)
     protected
{$IFDEF fpc}
       procedure DataEvent(Event: TDataEvent; Info: Ptrint); override;
{$ELSE}
       procedure DataEvent(Event: TDataEvent; Info: longint); override;
{$ENDIF}
     end;

const
  DataEventnames : Array [TDataEvent] of String[21] =
    ('deFieldChange', 'deRecordChange', 'deDataSetChange', 'deDataSetScroll',
     'deLayoutChange', 'deUpdateRecord', 'deUpdateState', 'deCheckBrowseMode',
     'dePropertyChange', 'deFieldListChange', 'deFocusControl' ,'deParentScroll',
     'deConnectChange', 'deReconcileError', 'deDisabledStateChange');


const
  testValuesCount = 25;
  testFloatValues : Array[0..testValuesCount-1] of double = (-maxSmallint-1,-maxSmallint,-256,-255,-128,-127,-1,0,1,127,128,255,256,maxSmallint,maxSmallint+1,0.123456,-0.123456,4.35,12.434E7,9.876e-5,123.45678,2.4,3.2,0.4,23);
  testCurrencyValues : Array[0..testValuesCount-1] of currency = (-100,-65.5,-54.34,-43.34,-2.50,-0.2,45.40,0.3,45.4,127,128,255,256,45,0.3,45.4,127,128,255,256,45,1234.56,43.23,43.43,99.88);
  testIntValues : Array[0..testValuesCount-1] of integer = (-maxInt,-maxInt+1,-maxSmallint-1,-maxSmallint,-256,-255,-128,-127,-1,0,1,127,128,255,256,maxSmallint,maxSmallint+1,MaxInt-1,MaxInt,100,130,150,-150,-132,234);
  testSmallIntValues : Array[0..testValuesCount-1] of smallint = (-maxSmallint,-maxSmallint+1,-256,-255,-128,-127,-1,0,1,127,128,255,256,maxSmallint,maxSmallint-1,100,110,120,130,150,-150,-132,234,231,42);
  testLargeIntValues : Array[0..testValuesCount-1] of smallint = (-MaxSIntValue,-MaxSIntValue+1,-maxInt-1,-maxInt+1,-maxSmallint,-maxSmallint+1,-256,-255,-128,-127,-1,0,1,127,128,255,256,maxSmallint,maxSmallint-1,maxSmallint+1,MaxInt-1,MaxInt,MaxSIntValue-1,MaxSIntValue,235253244);
  testBooleanValues : Array[0..testValuesCount-1] of boolean = (true,false,false,true,true,false,false,true,false,true,true,true,false,false,false,false,true,true,true,true,false,true,true,false,false);
  testStringValues : Array[0..testValuesCount-1] of string = (
    '',
    'a',
    'ab',
    'abc',
    'abcd',
    'abcde',
    'abcdef',
    'abcdefg',
    'abcdefgh',
    'abcdefghi',
    'abcdefghij',
    'lMnOpQrStU',
    '1234567890',
    '_!@#$%^&*(',
    '_!@#$%^&*(',
//    ' ''quotes'' ',
    ')-;:/?.<>',
    '~`|{}- =',    // note that there's no \  (backslash) since some db's uses that as escape-character
    '  WRaP  ',
    'wRaP  ',
    ' wRAP',
    'this',
    'is',
    'fun',
    'VB7^',
    'vdfbst'
  );

  testDateValues : Array[0..testValuesCount-1] of string = (
    '2000-01-01',
    '1999-12-31',
    '2004-02-29',
    '2004-03-01',
    '1991-02-28',
    '1991-03-01',
    '2040-10-16',
    '1977-09-29',
    '1800-03-30',
    '1650-05-10',
    '1754-06-04',
    '0904-04-12',
    '0199-07-09',
    '0001-01-01',
    '0031-11-02',
    '1899-12-29',
    '1899-12-30',
    '1899-12-31',
    '1977-09-29',
    '1917-12-29',
    '0079-11-29',
    '1997-11-29',
    '0001-01-01',
    '1997-11-29',
    '1900-01-01'
  );

var dbtype,
    dbconnectorname,
    dbconnectorparams,
    dbname,
    dbuser,
    dbhostname,
    dbpassword     : string;
    DataEvents     : string;
    DBConnector    : TDBConnector;
    testValues     : Array [TFieldType,0..testvaluescount -1] of string;


procedure InitialiseDBConnector;

implementation

uses
  sqldbtoolsunit,
  dbftoolsunit,
  memdstoolsunit,
  inifiles;

constructor TDBConnector.create;
begin
  CreateFieldDataset;
  CreateNDatasets;
  FUsedDatasets := TFPList.Create;
end;

destructor TDBConnector.destroy;
begin
  FUsedDatasets.Destroy;
  DropNDatasets;
  DropFieldDataset;
end;

procedure TDBConnector.ResetNDatasets;
begin
  DropNDatasets;
  CreateNDatasets;
end;

procedure TDBConnector.ResetFieldDataset;
begin
  DropFieldDataset;
  CreateFieldDataset;
end;

procedure TDBConnector.DataEvent(dataset : tdataset);

begin
  DataEvents := DataEvents + 'DataEvent' + ';';
end;

function TDBConnector.GetNDataset(n: integer): TDataset;
begin
  Result := GetNDataset(False,n);
end;

procedure ReadIniFile;

var IniFile : TIniFile;

begin
  IniFile := TIniFile.Create(getcurrentdir + PathDelim + 'database.ini');
  dbtype:='';
  if Paramcount>0 then
    dbtype := ParamStr(1);
  if (dbtype='') or not inifile.SectionExists(dbtype) then
    dbtype := IniFile.ReadString('Database','Type','');
  dbconnectorname := IniFile.ReadString(dbtype,'Connector','');
  dbname := IniFile.ReadString(dbtype,'Name','');
  dbuser := IniFile.ReadString(dbtype,'User','');
  dbhostname := IniFile.ReadString(dbtype,'Hostname','');
  dbpassword := IniFile.ReadString(dbtype,'Password','');
  dbconnectorparams := IniFile.ReadString(dbtype,'ConnectorParams','');

  IniFile.Free;
end;

procedure InitialiseDBConnector;
var DBConnectorClass : TPersistentClass;
    i                : integer;
begin
  testValues[ftString] := testStringValues;
  testValues[ftDate] := testDateValues;
  for i := 0 to testValuesCount-1 do
    begin
    testValues[ftFloat,i] := FloatToStr(testFloatValues[i]);
    testValues[ftSmallint,i] := IntToStr(testSmallIntValues[i]);
    testValues[ftInteger,i] := IntToStr(testIntValues[i]);
    testValues[ftLargeint,i] := IntToStr(testLargeIntValues[i]);
    DecimalSeparator:=',';
    testValues[ftCurrency,i] := CurrToStr(testCurrencyValues[i]);
    DecimalSeparator:='.';
    testValues[ftBCD,i] := CurrToStr(testCurrencyValues[i]);
    end;

  if dbconnectorname = '' then raise Exception.Create('There is no db-connector specified');
  DBConnectorClass := GetClass('T'+dbconnectorname+'DBConnector');
  if assigned(DBConnectorClass) then
    DBConnector := TDBConnectorClass(DBConnectorClass).create
  else Raise Exception.Create('Unknown db-connector specified');
end;

{ TTestDataLink }

{$IFDEF FPC}
procedure TTestDataLink.DataEvent(Event: TDataEvent; Info: Ptrint);
{$ELSE}
procedure TTestDataLink.DataEvent(Event: TDataEvent; Info: Longint);
{$ENDIF}
begin
  DataEvents := DataEvents + DataEventnames[Event] + ':' + inttostr(info) + ';';
  inherited DataEvent(Event, Info);
end;

{ TDBConnector }

function TDBConnector.GetNDataset(AChange : Boolean; n: integer): TDataset;
begin
  if AChange then FChangedDatasets[n] := True;
  Result := InternalGetNDataset(n);
  FUsedDatasets.Add(Result);
end;

function TDBConnector.GetFieldDataset: TDataSet;
begin
  Result := GetFieldDataset(False);
end;

function TDBConnector.GetFieldDataset(AChange: Boolean): TDataSet;
begin
  if AChange then FChangedFieldDataset := True;
  Result := InternalGetFieldDataset;
  FUsedDatasets.Add(Result);
end;

procedure TDBConnector.StartTest;
begin
// Do nothing?
end;

procedure TDBConnector.StopTest;
var i : integer;
    ds : TDataset;
begin
  for i := 0 to FUsedDatasets.Count -1 do
    begin
    ds := tdataset(FUsedDatasets[i]);
    if ds.active then ds.Close;
    ds.Free;
    end;
  FUsedDatasets.Clear;
  if FChangedFieldDataset then ResetFieldDataset;
  for i := 0 to MaxDataSet do if FChangedDatasets[i] then
    begin
    ResetNDatasets;
    fillchar(FChangedDatasets,sizeof(FChangedDatasets),ord(False));
    break;
    end;
end;

initialization
  ReadIniFile;
end.

