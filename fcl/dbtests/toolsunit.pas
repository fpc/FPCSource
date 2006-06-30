unit ToolsUnit;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

{$I settings.inc}

interface

uses
  Classes, SysUtils, DB;
  
Const MaxDataSet = 35;
  
type

  { TDBConnector }

  TDBConnector = class(TObject)
     private
       FDatasets : array[0..MaxDataset] of TDataset;
     protected
       Procedure FreeNDataset(var ds : TDataset); virtual; abstract;
       Function CreateNDataset(n : integer) : TDataset; virtual; abstract;
     public
       procedure DataEvent(dataset :TDataset);
       Function GetNDataset(n : integer) : TDataset; virtual;
       procedure InitialiseDatasets; virtual;
       procedure FreeDatasets; virtual;
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

type TDBTypes=(mysql40,mysql41,mysql50,interbase,postgresql,odbc,oracle,dbf);

const
  DBTypesNames : Array [TDBTypes] of String[19] =
     ('mysql40','mysql41','mysql50','interbase','postgresql','odbc','oracle','dbf');

  DataEventnames : Array [TDataEvent] of String[19] =
    ('deFieldChange', 'deRecordChange', 'deDataSetChange', 'deDataSetScroll',
     'deLayoutChange', 'deUpdateRecord', 'deUpdateState', 'deCheckBrowseMode',
     'dePropertyChange', 'deFieldListChange', 'deFocusControl' ,'deParentScroll'
{$IFNDEF VER2_0_2}, 'deConnectChange','deReconcileError','deDisabledStateChange'{$ENDIF}
    );

var dbtype          : TDBTypes;
    dbname,
    dbuser,
    dbhostname,
    dbpassword      : string;

    DBConnector     : TDBConnector;
    
    DataEvents      : string;

procedure InitialiseDBConnector;

resourcestring
  SIgnoreAssertion = 'You can safely ignore this failure. This function is just not supported by the selected database';

implementation

uses
{$IFDEF SQLDB_AVAILABLE}
  sqldbtoolsunit,
{$ENDIF}
{$IFDEF DBF_AVAILABLE}
  dbftoolsunit,
{$ENDIF}
  inifiles;

procedure TDBConnector.DataEvent(dataset : tdataset);

begin
  DataEvents := DataEvents + 'DataEvent' + ';';
end;

procedure ReadIniFile;

var IniFile : TIniFile;
    s       : string;
    i       : TDBTypes;
begin
  IniFile := TIniFile.Create(getcurrentdir + PathDelim + 'database.ini');
  s := IniFile.ReadString('Database','Type','');
  for i := low(DBTypesNames) to high(DBTypesNames) do
    if s = DBTypesNames[i] then dbtype := i;

  dbname := IniFile.ReadString(s,'Name','');
  dbuser := IniFile.ReadString(s,'User','');
  dbhostname := IniFile.ReadString(s,'Hostname','');
  dbpassword := IniFile.ReadString(s,'Password','');
  IniFile.Free;
end;

procedure InitialiseDBConnector;

begin
//  ReadIniFile;
  if (1 <> 1) then begin end
{$IFDEF SQLDB_AVAILABLE}
  else if (dbtype in SQLDBdbTypes)  then DBConnector := TSQLDBConnector.Create
{$ENDIF}
{$IFDEF DBF_AVAILABLE}
  else if dbtype = dbf then DBConnector := TDBFDBConnector.Create
{$ENDIF}
  else Raise Exception.Create('Invalid database-type specified');
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

function TDBConnector.GetNDataset(n: integer): TDataset;
begin
  Result := FDatasets[n];
end;

procedure TDBConnector.InitialiseDatasets;
var count : integer;
begin
  for count := 0 to MaxDataSet do
    FDatasets[count] := CreateNDataset(count);
end;

procedure TDBConnector.FreeDatasets;
var count : integer;
begin
  for count := 0 to MaxDataSet do if assigned(FDatasets[count]) then
    FreeNDataset(FDatasets[count]);
end;

initialization
  ReadIniFile;
end.

