unit utc.opf.registrycontracts;

{ Tests for registry, configuration and serialization contracts:
  - connection definitions: streams and files with the cipher, a failed save
    keeps the old file, INI files without stale data, params next to settings,
    port and role applied to the connection
  - variants and JSON: ragged arrays, dates, QWord, exact currency, NaN,
    a collection kept on a type mismatch
  - list registry: subclasses of registered list classes
  - field maps: AddField with foVersion
  - logger: only a TOPLogger, a failing OnLog does not replace the error
  - singletons: created once when several threads ask at the same time }

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Variants, System.Math, System.Contnrs,
  Data.Db, Data.Sqldb, Data.SqlDb.Sqlite3, FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, Variants, Math, contnrs, DB, sqldb, sqlite3conn, fpcunit, testregistry,
{$ENDIF}
{$IFDEF UNIX}
{$IFDEF FPC_DOTTEDUNITS}
  UnixApi.Base,
{$ELSE}
  baseunix,
{$ENDIF}
{$ENDIF}
  Opf.Types, Opf.Fields, Opf.Connections, Opf.Variants, Opf.Json, Opf.ListReg,
  Opf.Logging, Opf.DDL, Opf.Registry, Opf.Relations, Opf.Persister.RTTI;

type

  { TFailingCipher: Encrypt raises }

  TFailingCipher = class(TObject, IOPPasswordCipher)
  public
    function Encrypt(const aPlain: UTF8String): UTF8String;
    function Decrypt(const aCipher: UTF8String): UTF8String;
  end;

  TRCItem = class(TCollectionItem)
  private
    FName: String;
  published
    property Name: String read FName write FName;
  end;

  TRCComponentListManipulator = class(TOPListManipulator)
  public
    procedure AddItem(aList: TObject; aItem: TObject); override;
    procedure ClearList(aList: TObject); override;
    function ItemCount(aList: TObject): Integer; override;
    function GetItem(aList: TObject; aIndex: Integer): TObject; override;
  end;

  TRCLogComponent = class(TOPBaseComponent)
  protected
    class function GetExceptionClass: EOPFClass; override;
  end;

  TRCParent = class(TPersistent)
  private
    FID: Integer;
  published
    property ID: Integer read FID write FID;
  end;

  TRCChild = class(TPersistent)
  private
    FID: Integer;
    FParentID: Integer;
  published
    property ID: Integer read FID write FID;
    property ParentID: Integer read FParentID write FParentID;
  end;

  { TTestRegistryContracts }

  TTestRegistryContracts = class(TTestCase)
  private
    FDir: String;
    FList: TSQLDBConnectionList;
    FXor: TOPXorCipher;
    procedure FillList;
    function FileText(const aName: String): String;
    procedure RaisingOnLog(Sender: TObject; aLevel: TLogKind; const Msg: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Connection definitions
    procedure TestStreamEncryptsPasswords;
    procedure TestStreamRoundTrip;
    procedure TestSaveKeepsPasswordsInMemory;
    procedure TestFailedSaveKeepsOldFile;
    procedure TestIniRemovedConnectionGone;
    procedure TestIniParamNamedPassword;
    procedure TestIniOldFormatRead;
    procedure TestConfigConnectionPortAndRole;
{$IFDEF UNIX}
    // R2-M18: permissions, symbolic links
    procedure TestSaveKeepsFileMode;
    procedure TestSaveNewFileIsPrivate;
    procedure TestSaveFollowsSymlink;
{$ENDIF}
    // Variants and JSON
    procedure TestRaggedTwoLevels;
    procedure TestRaggedDeeperLater;
    procedure TestRectangularThreeLevels;
    procedure TestDateAsText;
    procedure TestQWord;
    procedure TestCurrencyExact;
    procedure TestNaNRaises;
    procedure TestUnicodeString;
    procedure TestCollectionKeptOnMismatch;
    // R2-M17 and round 1 (7.5): the read side
    procedure TestQWordRoundTrip;
    procedure TestTaggedTypesRoundTrip;
    procedure TestTaggedOffWritesPlainJSON;
    procedure TestCollectionKeptOnElementError;
    // R2-L19
    procedure TestArrayOfArraysRoundTrip;
    // List registry
    procedure TestSubclassOfRegisteredList;
    // Field maps
    procedure TestAddFieldWithVersion;
    procedure TestAddFieldBadOptionsLeavesNothing;
    // Round 1 (7.7): foVersion without kind, checked when the kind is set
    procedure TestAddFieldVersionWithoutKind;
    // R2-L12
    procedure TestAddFieldEmptyFieldNameDuplicate;
    procedure TestKindChangeAfterOptionsChecked;
    // R2-L18
    procedure TestAsArrayAfterReorder;
    // Logger
    procedure TestLoggerMustBeOPLogger;
    procedure TestFailingOnLogKeepsError;
    // R2-M16
    procedure TestLogFileFromThreads;
    // R2-M14
    procedure TestClearInstanceRemovesGlobalRelations;
    // Singletons
    procedure TestSingletonOneInstanceUnderThreads;
  end;

implementation

{ TFailingCipher }

function TFailingCipher.Encrypt(const aPlain: UTF8String): UTF8String;
begin
  raise Exception.Create('cipher failure');
end;

function TFailingCipher.Decrypt(const aCipher: UTF8String): UTF8String;
begin
  Result:=aCipher;
end;

{ TRCComponentListManipulator }

procedure TRCComponentListManipulator.AddItem(aList: TObject; aItem: TObject);
begin
  TComponentList(aList).Add(TComponent(aItem));
end;

procedure TRCComponentListManipulator.ClearList(aList: TObject);
begin
  TComponentList(aList).Clear;
end;

function TRCComponentListManipulator.ItemCount(aList: TObject): Integer;
begin
  Result:=TComponentList(aList).Count;
end;

function TRCComponentListManipulator.GetItem(aList: TObject; aIndex: Integer): TObject;
begin
  Result:=TComponentList(aList)[aIndex];
end;

{ TRCLogComponent }

class function TRCLogComponent.GetExceptionClass: EOPFClass;
begin
  Result:=EOPF;
end;

{ TTestRegistryContracts }

procedure TTestRegistryContracts.SetUp;
begin
  inherited SetUp;
  FDir:=IncludeTrailingPathDelimiter(GetTempDir(False))+'opfrc'+IntToStr(GetProcessID)+PathDelim;
  ForceDirectories(FDir);
  FList:=TSQLDBConnectionList.Create(TSQLDBConnectionDef);
  FXor:=TOPXorCipher.Create('k3y');
end;

procedure TTestRegistryContracts.TearDown;
var
  SR: TSearchRec;
begin
  FreeAndNil(FList);
  FreeAndNil(FXor);
  if FindFirst(FDir+'*', faAnyFile, SR)=0 then
  begin
    repeat
      if (SR.Attr and faDirectory)=0 then
        DeleteFile(FDir+SR.Name);
    until FindNext(SR)<>0;
    FindClose(SR);
  end;
  RemoveDir(FDir);
  inherited TearDown;
end;

procedure TTestRegistryContracts.FillList;
var
  C: TSQLDBConnectionDef;
begin
  C:=FList.AddConnection('postgresql', 'host1', 'db1', 'user1', 'secret1');
  C.Name:='main';
  C.Port:=5433;
  C.Params.Add('sslmode=require');
  C:=FList.AddConnection('firebird', 'host2', 'db2', 'user2', 'secret2');
  C.Name:='other';
end;

function TTestRegistryContracts.FileText(const aName: String): String;
var
  L: TStringList;
begin
  L:=TStringList.Create;
  try
    L.LoadFromFile(aName);
    Result:=L.Text;
  finally
    L.Free;
  end;
end;

procedure TTestRegistryContracts.RaisingOnLog(Sender: TObject; aLevel: TLogKind; const Msg: String);
begin
  raise Exception.Create('log handler failure');
end;

procedure TTestRegistryContracts.TestStreamEncryptsPasswords;
var
  S: TStringStream;
begin
  FillList;
  FList.PasswordCipher:=FXor;
  S:=TStringStream.Create('');
  try
    FList.SaveToStream(S);
    AssertEquals('No plain password in the stream', 0, Pos('secret1', S.DataString));
  finally
    S.Free;
  end;
end;

procedure TTestRegistryContracts.TestStreamRoundTrip;
var
  S: TStringStream;
  L: TSQLDBConnectionList;
begin
  FillList;
  FList.PasswordCipher:=FXor;
  S:=TStringStream.Create('');
  L:=TSQLDBConnectionList.Create(TSQLDBConnectionDef);
  try
    FList.SaveToStream(S);
    S.Position:=0;
    L.PasswordCipher:=FXor;
    L.LoadFromStream(S);
    AssertEquals('Decrypted on load', 'secret1', L.FindConnection('main').Password);
  finally
    L.Free;
    S.Free;
  end;
end;

procedure TTestRegistryContracts.TestSaveKeepsPasswordsInMemory;
begin
  FillList;
  FList.PasswordCipher:=FXor;
  FList.SaveToFile(FDir+'c.json');
  AssertEquals('In memory: plain', 'secret1', FList.FindConnection('main').Password);
  AssertEquals('On disk: not plain', 0, Pos('secret1', FileText(FDir+'c.json')));
end;

{$IFDEF UNIX}
function FileMode(const aName: String): Integer;
var
  Info: Stat;
begin
  if fpStat(aName, Info)<>0 then
    Exit(-1);
  Result:=Info.st_mode and &7777;
end;

procedure TTestRegistryContracts.TestSaveKeepsFileMode;
begin
  FillList;
  FList.SaveToFile(FDir+'m.json');
  fpChmod(FDir+'m.json', &640);
  FList.SaveToFile(FDir+'m.json');
  AssertEquals('Mode kept', &640, FileMode(FDir+'m.json'));
end;

procedure TTestRegistryContracts.TestSaveNewFileIsPrivate;
begin
  FillList;
  FList.SaveToFile(FDir+'n.ini');
  AssertEquals('Owner only', &600, FileMode(FDir+'n.ini'));
end;

procedure TTestRegistryContracts.TestSaveFollowsSymlink;
var
  Info: Stat;
  L: TSQLDBConnectionList;
begin
  FillList;
  FList.SaveToFile(FDir+'target.json');
  AssertEquals('Link made', 0, fpSymlink(PChar('target.json'), PChar(FDir+'link.json')));
  FList.FindConnection('main').HostName:='viaLink';
  FList.SaveToFile(FDir+'link.json');
  AssertEquals('lstat', 0, fpLStat(FDir+'link.json', Info));
  AssertTrue('Still a link', fpS_ISLNK(Info.st_mode));
  L:=TSQLDBConnectionList.Create(TSQLDBConnectionDef);
  try
    L.LoadFromFile(FDir+'target.json');
    AssertEquals('Target written', 'viaLink', L.FindConnection('main').HostName);
  finally
    L.Free;
  end;
end;
{$ENDIF}

procedure TTestRegistryContracts.TestFailedSaveKeepsOldFile;
var
  Before: String;
  Failing: TFailingCipher;
  Raised: Boolean;
begin
  FillList;
  FList.SaveToFile(FDir+'c.json');
  Before:=FileText(FDir+'c.json');
  Failing:=TFailingCipher.Create;
  try
    FList.PasswordCipher:=Failing;
    FList.FindConnection('main').HostName:='changed';
    Raised:=False;
    try
      FList.SaveToFile(FDir+'c.json');
    except
      on E: Exception do
        Raised:=True;
    end;
    AssertTrue('Save failed', Raised);
    AssertEquals('Old file intact', Before, FileText(FDir+'c.json'));
    AssertEquals('Passwords in memory unchanged', 'secret1', FList.FindConnection('main').Password);
    FList.PasswordCipher:=Nil;
  finally
    Failing.Free;
  end;
end;

procedure TTestRegistryContracts.TestIniRemovedConnectionGone;
var
  L: TSQLDBConnectionList;
begin
  FillList;
  FList.SaveToFile(FDir+'c.ini');
  FList.Delete(FList.IndexOfConnection('other'));
  FList.FindConnection('main').Params.Clear;
  FList.SaveToFile(FDir+'c.ini');
  L:=TSQLDBConnectionList.Create(TSQLDBConnectionDef);
  try
    L.LoadFromFile(FDir+'c.ini');
    AssertEquals('Removed connection gone', 1, L.Count);
    AssertEquals('Removed param gone', 0, L.FindConnection('main').Params.Count);
  finally
    L.Free;
  end;
end;

procedure TTestRegistryContracts.TestIniParamNamedPassword;
var
  L: TSQLDBConnectionList;
begin
  FillList;
  FList.FindConnection('main').Params.Add('Password=param-value');
  FList.SaveToFile(FDir+'c.ini');
  L:=TSQLDBConnectionList.Create(TSQLDBConnectionDef);
  try
    L.LoadFromFile(FDir+'c.ini');
    AssertEquals('Password setting kept', 'secret1', L.FindConnection('main').Password);
    AssertEquals('Param kept', 'param-value', L.FindConnection('main').Params.Values['Password']);
    AssertEquals('Other param', 'require', L.FindConnection('main').Params.Values['sslmode']);
  finally
    L.Free;
  end;
end;

procedure TTestRegistryContracts.TestIniOldFormatRead;
var
  T: TStringList;
  L: TSQLDBConnectionList;
begin
  T:=TStringList.Create;
  try
    T.Add('[old]');
    T.Add('Host=h');
    T.Add('Password=p');
    T.Add('Params=sslmode');
    T.Add('sslmode=disable');
    T.SaveToFile(FDir+'old.ini');
  finally
    T.Free;
  end;
  L:=TSQLDBConnectionList.Create(TSQLDBConnectionDef);
  try
    L.LoadFromFile(FDir+'old.ini');
    AssertEquals('h', L.FindConnection('old').HostName);
    AssertEquals('Params read', 'disable', L.FindConnection('old').Params.Values['sslmode']);
  finally
    L.Free;
  end;
end;

procedure TTestRegistryContracts.TestConfigConnectionPortAndRole;
var
  C: TSQLDBConnectionDef;
  Conn: TSQLite3Connection;
begin
  FillList;
  C:=FList.FindConnection('main');
  C.Role:='admin';
  Conn:=TSQLite3Connection.Create(Nil);
  try
    C.ConfigConnection(Conn);
    AssertEquals('Port', '5433', Conn.Params.Values['Port']);
    AssertEquals('Role', 'admin', Conn.Role);
    AssertEquals('Other params', 'require', Conn.Params.Values['sslmode']);
  finally
    Conn.Free;
  end;
end;

procedure TTestRegistryContracts.TestRaggedTwoLevels;
var
  V: Variant;
begin
  V:=JSONToVariant('[[1,2],[3]]');
  AssertEquals('1-D of rows', 1, VarArrayDimCount(V));
  AssertEquals(2, VarArrayHighBound(V, 1)+1);
end;

procedure TTestRegistryContracts.TestRaggedDeeperLater;
var
  V: Variant;
begin
  // The second child is deeper than the first: used to write past the end
  V:=JSONToVariant('[[1,2],[[3],[4]]]');
  AssertEquals('1-D', 1, VarArrayDimCount(V));
  V:=JSONToVariant('[[[1],[2]],[[1,2],[3,4]]]');
  AssertEquals('Different inner lengths: 1-D', 1, VarArrayDimCount(V));
end;

procedure TTestRegistryContracts.TestRectangularThreeLevels;
var
  V: Variant;
begin
  V:=JSONToVariant('[[[1,2],[3,4]],[[5,6],[7,8]]]');
  AssertEquals('3-D', 3, VarArrayDimCount(V));
  AssertEquals(8, Integer(V[1,1,1]));
end;

procedure TTestRegistryContracts.TestDateAsText;
var
  D: TDateTime;
begin
  D:=EncodeDate(2024,2,29)+EncodeTime(13,14,15,16);
  AssertEquals('Tagged', '{"$vartype": "date", "value": "2024-02-29T13:14:15.016"}',
    VariantToJSON(VarFromDateTime(D)));
  OPFVariantJSONTypeTags:=False;
  try
    AssertEquals('Plain', '"2024-02-29T13:14:15.016"', VariantToJSON(VarFromDateTime(D)));
  finally
    OPFVariantJSONTypeTags:=True;
  end;
end;

procedure TTestRegistryContracts.TestQWord;
var
  Q: QWord;
  V: Variant;
begin
  Q:=High(QWord);
  V:=Q;
  AssertEquals('18446744073709551615', VariantToJSON(V));
end;

procedure TTestRegistryContracts.TestCurrencyExact;
var
  C: Currency;
  V: Variant;
begin
  C:=123456789012345.6789;
  V:=C;
  AssertEquals('Tagged', '{"$vartype": "currency", "value": "123456789012345.6789"}', VariantToJSON(V));
  OPFVariantJSONTypeTags:=False;
  try
    AssertEquals('Plain', '123456789012345.6789', VariantToJSON(V));
  finally
    OPFVariantJSONTypeTags:=True;
  end;
end;

procedure TTestRegistryContracts.TestNaNRaises;
var
  Raised: Boolean;
  V: Variant;
begin
  V:=Double(NaN);
  Raised:=False;
  try
    VariantToJSON(V);
  except
    on E: EOPF do
      Raised:=True;
  end;
  AssertTrue(Raised);
end;

procedure TTestRegistryContracts.TestUnicodeString;
var
  U: UnicodeString;
  V: Variant;
begin
  U:='a'+WideChar($6F22);
  V:=U;
  AssertEquals('"a'+#$E6#$BC#$A2+'"', VariantToJSON(V));
end;

procedure TTestRegistryContracts.TestCollectionKeptOnMismatch;
var
  C: TCollection;
begin
  C:=TCollection.Create(TRCItem);
  try
    TRCItem(C.Add).Name:='keep';
    try
      OPSerializer.Deserialize('{"a":1}', C);
    except
      on E: Exception do ;
    end;
    AssertEquals('Not cleared', 1, C.Count);
  finally
    C.Free;
  end;
end;

procedure TTestRegistryContracts.TestQWordRoundTrip;
var
  Q: QWord;
  V: Variant;
begin
  Q:=High(QWord);
  V:=Q;
  V:=JSONToVariant(VariantToJSON(V));
  AssertEquals('QWord type', varQWord, VarType(V));
  AssertTrue('Value', QWord(V)=High(QWord));
  V:=JSONToVariant('9223372036854775807');
  AssertTrue('Int64', Int64(V)=High(Int64));
end;

procedure TTestRegistryContracts.TestTaggedTypesRoundTrip;
var
  C: Currency;
  D: TDateTime;
  V: Variant;
begin
  AssertTrue('Type tags on by default', OPFVariantJSONTypeTags);
  C:=123456789012345.6789;
  V:=JSONToVariant(VariantToJSON(C));
  AssertEquals('Currency type', varCurrency, VarType(V));
  AssertTrue('Currency exact', Currency(V)=C);
  D:=EncodeDate(1850,2,28)+EncodeTime(13,14,15,16);
  V:=JSONToVariant(VariantToJSON(VarFromDateTime(D)));
  AssertEquals('Date type', varDate, VarType(V));
  AssertEquals('Date value', D, TDateTime(V), 1E-9);
  D:=EncodeDate(2024,2,29)+EncodeTime(1,2,3,4);
  V:=JSONToVariant(VariantToJSON(VarFromDateTime(D)));
  AssertEquals('Date value after 1899', D, TDateTime(V), 1E-9);
  V:=JSONToVariant(VariantToJSON(Unassigned));
  AssertTrue('Unassigned', VarIsEmpty(V));
  V:=JSONToVariant(VariantToJSON(Null));
  AssertTrue('Null', VarIsNull(V));
end;

procedure TTestRegistryContracts.TestTaggedOffWritesPlainJSON;
var
  C: Currency;
  V: Variant;
begin
  OPFVariantJSONTypeTags:=False;
  try
    C:=1.5;
    V:=C;
    AssertEquals('Currency', '1.5', VariantToJSON(V));
    AssertEquals('Unassigned', 'null', VariantToJSON(Unassigned));
    // Tagged values are always read
    V:=JSONToVariant('{"$vartype": "currency", "value": "2.25"}');
    AssertEquals('Read tagged', varCurrency, VarType(V));
  finally
    OPFVariantJSONTypeTags:=True;
  end;
end;

procedure TTestRegistryContracts.TestCollectionKeptOnElementError;
var
  C: TCollection;
  Raised: Boolean;
begin
  C:=TCollection.Create(TRCItem);
  try
    TRCItem(C.Add).Name:='keep';
    Raised:=False;
    try
      // The second element is no object
      OPSerializer.Deserialize('[{"Name":"new"}, 5]', C);
    except
      on E: Exception do
        Raised:=True;
    end;
    AssertTrue('Raised', Raised);
    AssertEquals('Old items back', 1, C.Count);
    AssertEquals('Old item', 'keep', TRCItem(C.Items[0]).Name);
  finally
    C.Free;
  end;
end;

procedure TTestRegistryContracts.TestArrayOfArraysRoundTrip;
var
  V, R: Variant;
begin
  V:=VarArrayOf([VarArrayOf([1, 2]), VarArrayOf([3, 4])]);
  R:=JSONToVariant(VariantToJSON(V));
  AssertEquals('Still 1-D', 1, VarArrayDimCount(R));
  AssertTrue('Element is an array', VarIsArray(R[0]));
  AssertEquals('Element value', 4, Integer(R[1][1]));
  // A real 2-D array still reads back as 2-D
  V:=VarArrayCreate([0, 1, 0, 1], varVariant);
  V[0, 0]:=1; V[0, 1]:=2; V[1, 0]:=3; V[1, 1]:=4;
  R:=JSONToVariant(VariantToJSON(V));
  AssertEquals('2-D', 2, VarArrayDimCount(R));
end;

procedure TTestRegistryContracts.TestAddFieldVersionWithoutKind;
var
  L: TOPFieldMapList;
  F: TOPFieldMap;
  Raised: Boolean;
begin
  L:=TOPFieldMapList.Create(TOPFieldMap);
  try
    F:=L.AddField('ver', 'Version', [foVersion]);
    AssertNotNull('Added', L.FindVersionField);
    F.PropertyTypeInfo:=TypeInfo(Integer);
    AssertTrue('Kind set', F.PropertyKind=tkInteger);
    F:=L.AddField('del', 'Deleted', [foSoftDelete]);
    Raised:=False;
    try
      F.PropertyTypeInfo:=TypeInfo(String);
    except
      on E: EFieldMap do
        Raised:=True;
    end;
    AssertTrue('Wrong kind rejected when set', Raised);
  finally
    L.Free;
  end;
end;

procedure TTestRegistryContracts.TestAddFieldEmptyFieldNameDuplicate;
var
  L: TOPFieldMapList;
  Raised: Boolean;
begin
  L:=TOPFieldMapList.Create(TOPFieldMap);
  try
    L.AddField('Name', 'Other');
    Raised:=False;
    try
      // Column Name again: the property name is the column name
      L.AddField('', 'Name');
    except
      on E: EFieldMap do
        Raised:=True;
    end;
    AssertTrue('Duplicate column rejected', Raised);
    AssertEquals('Nothing added', 1, L.Count);
  finally
    L.Free;
  end;
end;

procedure TTestRegistryContracts.TestKindChangeAfterOptionsChecked;
var
  L: TOPFieldMapList;
  F: TOPFieldMap;
  Raised: Boolean;
begin
  L:=TOPFieldMapList.Create(TOPFieldMap);
  try
    F:=L.AddField('ver', 'Version', tkInteger, [foVersion]);
    Raised:=False;
    try
      F.PropertyKind:=tkFloat;
    except
      on E: EFieldMap do
        Raised:=True;
    end;
    AssertTrue('Kind checked against the options', Raised);
    AssertTrue('Kind unchanged', F.PropertyKind=tkInteger);
  finally
    L.Free;
  end;
end;

procedure TTestRegistryContracts.TestAsArrayAfterReorder;
var
  L: TOPFieldMapList;
  A: TOPFieldMapArray;
begin
  L:=TOPFieldMapList.Create(TOPFieldMap);
  try
    L.AddField('a', 'A');
    L.AddField('b', 'B');
    L.AddField('c', 'C');
    A:=L.AsArray;
    AssertEquals('Before', 'A', A[0].PropertyName);
    L[2].Index:=0;
    A:=L.AsArray;
    AssertEquals('Moved first', 'C', A[0].PropertyName);
    AssertEquals('Then', 'A', A[1].PropertyName);
    L.Exchange(0, 2);
    A:=L.AsArray;
    AssertEquals('Exchanged', 'B', A[0].PropertyName);
  finally
    L.Free;
  end;
end;

type
  TLogThread = class(TThread)
  public
    Logger: TOPLogger;
    procedure Execute; override;
  end;

const
  LogThreadLines = 200;

procedure TLogThread.Execute;
var
  I: Integer;
begin
  for I:=1 to LogThreadLines do
    Logger.LogInfo(Nil, 'line %d of a log thread', [I]);
end;

procedure TTestRegistryContracts.TestLogFileFromThreads;
var
  L: TOPLogger;
  T: array[0..3] of TLogThread;
  I, N: Integer;
  Lines: TStringList;
begin
  L:=TOPLogger.Create(Nil);
  try
    L.LogFileName:=FDir+'threads.log';
    for I:=0 to High(T) do
    begin
      T[I]:=TLogThread.Create(True);
      T[I].Logger:=L;
    end;
    try
      for I:=0 to High(T) do
        T[I].Start;
      for I:=0 to High(T) do
        T[I].WaitFor;
    finally
      for I:=0 to High(T) do
        T[I].Free;
    end;
    L.LogFileName:='';
  finally
    L.Free;
  end;
  Lines:=TStringList.Create;
  try
    Lines.LoadFromFile(FDir+'threads.log');
    AssertEquals('Every line written', Length(T)*LogThreadLines, Lines.Count);
    N:=0;
    for I:=0 to Lines.Count-1 do
      if Pos('of a log thread', Lines[I])>0 then
        Inc(N);
    AssertEquals('No line mixed up', Lines.Count, N);
  finally
    Lines.Free;
  end;
end;

procedure TTestRegistryContracts.TestClearInstanceRemovesGlobalRelations;
var
  Raised: Boolean;
begin
  TOPRegistry.ClearInstance;
  TOPRelationRegistry.ClearInstance;
  try
    OPRegistry.RegisterPersister(TRCParent, TRTTIPersister);
    OPRegistry.RegisterPersister(TRCChild, TRTTIPersister);
    OPRelationRegistry.RegisterRelationship(TRCParent, TRCChild, 'ParentID');
    TOPRegistry.ClearInstance;
    AssertEquals('No stale relations', 0, Length(OPRelationRegistry.FindChildRelations(TRCParent)));
    // Registered again: no duplicate relationship
    OPRegistry.RegisterPersister(TRCParent, TRTTIPersister);
    OPRegistry.RegisterPersister(TRCChild, TRTTIPersister);
    Raised:=False;
    try
      OPRelationRegistry.RegisterRelationship(TRCParent, TRCChild, 'ParentID');
    except
      on E: Exception do
        Raised:=True;
    end;
    AssertFalse('Registered again', Raised);
  finally
    TOPRegistry.ClearInstance;
    TOPRelationRegistry.ClearInstance;
  end;
end;

procedure TTestRegistryContracts.TestSubclassOfRegisteredList;
var
  R: TOPListRegistry;
  M: TOPListManipulator;
begin
  R:=TOPListRegistry.Create(Nil);
  try
    R.RegisterListClass(TObjectList, TRCComponentListManipulator.Create);
    M:=TRCComponentListManipulator.Create;
    R.RegisterListClass(TComponentList, M);
    AssertSame('Own manipulator preferred', M, R.FindManipulator(TComponentList));
  finally
    R.Free;
  end;
end;

procedure TTestRegistryContracts.TestAddFieldWithVersion;
var
  L: TOPFieldMapList;
begin
  L:=TOPFieldMapList.Create(TOPFieldMap);
  try
    L.AddField('ver', 'Version', tkInteger, [foVersion]);
    AssertNotNull(L.FindVersionField);
  finally
    L.Free;
  end;
end;

procedure TTestRegistryContracts.TestAddFieldBadOptionsLeavesNothing;
var
  L: TOPFieldMapList;
begin
  L:=TOPFieldMapList.Create(TOPFieldMap);
  try
    try
      // Conflicting options
      L.AddField('ver', 'Version', [foRequired, foNullable]);
    except
      on E: Exception do ;
    end;
    AssertEquals('No half-made field left', 0, L.Count);
  finally
    L.Free;
  end;
end;

procedure TTestRegistryContracts.TestLoggerMustBeOPLogger;
var
  C: TRCLogComponent;
  O: TComponent;
  Raised: Boolean;
begin
  C:=TRCLogComponent.Create(Nil);
  O:=TComponent.Create(Nil);
  try
    Raised:=False;
    try
      C.Logger:=O;
    except
      on E: EOPF do
        Raised:=True;
    end;
    AssertTrue(Raised);
  finally
    O.Free;
    C.Free;
  end;
end;

procedure TTestRegistryContracts.TestFailingOnLogKeepsError;
var
  C: TRCLogComponent;
  L: TOPLogger;
begin
  C:=TRCLogComponent.Create(Nil);
  L:=TOPLogger.Create(Nil);
  try
    L.LogKinds:=[lkTrace];
    L.OnLog:=@RaisingOnLog;
    C.Logger:=L;
    try
      C.RaiseError('Test', 'the real error');
      Fail('Expected an error');
    except
      on E: EOPF do
        AssertTrue('Original error: '+E.Message, Pos('the real error', E.Message)>0);
    end;
  finally
    C.Free;
    L.Free;
  end;
end;

type
  TInstanceThread = class(TThread)
  public
    Got: TOPDDLBuilder;
    procedure Execute; override;
  end;

procedure TInstanceThread.Execute;
begin
  Got:=TOPDDLBuilder.Instance;
end;

procedure TTestRegistryContracts.TestSingletonOneInstanceUnderThreads;
var
  T: array[0..7] of TInstanceThread;
  I: Integer;
begin
  TOPDDLBuilder.ClearInstance;
  for I:=0 to High(T) do
    T[I]:=TInstanceThread.Create(True);
  try
    for I:=0 to High(T) do
      T[I].Start;
    for I:=0 to High(T) do
      T[I].WaitFor;
    for I:=1 to High(T) do
      AssertSame('Same instance', T[0].Got, T[I].Got);
    AssertSame('The singleton', TOPDDLBuilder.Instance, T[0].Got);
  finally
    for I:=0 to High(T) do
      T[I].Free;
  end;
end;

initialization
  RegisterTest(TTestRegistryContracts);
end.
