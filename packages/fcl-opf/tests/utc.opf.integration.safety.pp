unit utc.opf.integration.safety;

{ Integration tests for the fixes of the review, against every configured
  backend (SQLite always; PostgreSQL, Firebird and MySQL when configured in
  databases.ini). Tables are created with CreateSchema, so the generated DDL
  is exercised as well. Identifiers use the rwsQuoteOnDemand strategy:
  reserved words (a table named Order, columns named User and Group) are
  quoted, which is case sensitive on PostgreSQL and Firebird. }

{$mode objfpc}{$H+}
{$INTERFACES CORBA}
{$modeswitch advancedrecords}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.DateUtils, System.StrUtils, System.Contnrs, Data.Db, Data.Sqldb,
  FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, DateUtils, StrUtils, contnrs, DB, sqldb,
  fpcunit, testregistry,
{$ENDIF}
  Opf.Types, Opf.Fields, Opf.Registry, Opf.Relations, Opf.M2M, Opf.Driver, Opf.Sql, Opf.DDL,
  Opf.Criteria, Opf.Manager, Opf.Migration, Opf.Schema, Opf.Persister.RTTI,
  utc.opf.integration.base;

type

  { TSafeItem }

  TSafeItem = class(TPersistent, IOPPersistenceState)
  private
    FBig: Int64;
    FBorn: TDateTime;
    FCard: Cardinal;
    FUQ: QWord;
    FText: UnicodeString;
    FDay: TDate;
    FDeleted: Boolean;
    FID: Integer;
    FName: String;
    FPrice: Currency;
    FQty: Integer;
    FVersion: Integer;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    property State: TObjectPersistenceState read FState write FState;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Qty: Integer read FQty write FQty;
    property Price: Currency read FPrice write FPrice;
    property Born: TDateTime read FBorn write FBorn;
    property Day: TDate read FDay write FDay;
    property Big: Int64 read FBig write FBig;
    property Card: Cardinal read FCard write FCard;
    property UQ: QWord read FUQ write FUQ;
    property Text: UnicodeString read FText write FText;
    property Deleted: Boolean read FDeleted write FDeleted;
    property Version: Integer read FVersion write FVersion;
  end;

  { TSafeParent: string key }

  TSafeParent = class(TPersistent, IOPPersistenceState)
  private
    FCode: String;
    FName: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    property State: TObjectPersistenceState read FState write FState;
  published
    // Named so the registry recognises it as the primary key
    property SafeParentID: String read FCode write FCode;
    property Name: String read FName write FName;
  end;

  { TSafeChild }

  TSafeChild = class(TPersistent, IOPPersistenceState)
  private
    FID: Integer;
    FInfo: String;
    FParentCode: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    property State: TObjectPersistenceState read FState write FState;
  published
    property ID: Integer read FID write FID;
    property ParentCode: String read FParentCode write FParentCode;
    property Info: String read FInfo write FInfo;
  end;

  { TSafeTree: a relation to itself }

  TSafeTree = class(TPersistent, IOPPersistenceState)
  private
    FID: Integer;
    FParentTreeID: Integer;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    property State: TObjectPersistenceState read FState write FState;
  published
    property ID: Integer read FID write FID;
    property ParentTreeID: Integer read FParentTreeID write FParentTreeID;
  end;

  { TSafeNote: added to the model after the schema was created }

  TSafeNote = class(TPersistent)
  private
    FID: Integer;
    FItemID: Integer;
    FNote: String;
  published
    property ID: Integer read FID write FID;
    property ItemID: Integer read FItemID write FItemID;
    property Note: String read FNote write FNote;
  end;

  { TSafeLoopA / TSafeLoopB: reference each other }

  TSafeLoopA = class(TPersistent)
  private
    FID: Integer;
    FLoopBID: Integer;
  published
    property ID: Integer read FID write FID;
    property LoopBID: Integer read FLoopBID write FLoopBID;
  end;

  TSafeLoopB = class(TPersistent)
  private
    FID: Integer;
    FLoopAID: Integer;
  published
    property ID: Integer read FID write FID;
    property LoopAID: Integer read FLoopAID write FLoopAID;
  end;

  { TOrder: reserved words for the table and its columns }

  TOrder = class(TPersistent, IOPPersistenceState)
  private
    FGroup: Integer;
    FID: Integer;
    FLevel: Integer;
    FUser: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    property State: TObjectPersistenceState read FState write FState;
  published
    property ID: Integer read FID write FID;
    property User: String read FUser write FUser;
    property Group: Integer read FGroup write FGroup;
    property Level: Integer read FLevel write FLevel;
  end;

  { TTestSafetyCommon }

  TTestSafetyCommon = class(TTestIntegration)
  private
    FRegistry: TOPRegistry;
    FRelations: TOPRelationRegistry;
    FM2M: TOPManyToManyRegistry;
    FSQLBuilder: TOPSQLBuilder;
    FDDLBuilder: TOPDDLBuilder;
    FManager: TOPManager;
    FMigrator: TOPSchemaMigrator;
    FObjects: TFPObjectList;
    function Dialect: TSQLDialect;
    procedure EndConnectionTransaction;
    procedure DropTables;
    function Count(const aTable: UTF8String; const aWhere: UTF8String = ''): Integer;
    function NewItem(const aName: String; aQty: Integer; aSave: Boolean = True): TSafeItem;
    function NewParent(const aCode: String; aSave: Boolean = True): TSafeParent;
    function NewChild(const aParent, aInfo: String): TSafeChild;
    function LoadItems(aCriteria: TOPCriteria): TFPObjectList;
    function ItemNames(aList: TFPObjectList): String;
    function TrySave(aObject: TObject): Boolean;
  protected
    class function HelperClass: TDatabaseSetupHelperClass; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Criteria and soft delete
    procedure TestOrPlusSoftDeleteExcludesDeleted;
    procedure TestOrThenAndGrouping;
    procedure TestIncludeDeletedWithOr;
    procedure TestNotGroupWithSoftDelete;
    procedure TestOrderByLimitWithSoftDelete;
    procedure TestEmptyWhereIn;
    procedure TestWhereRaw;
    // Types
    procedure TestDateCurrencyInt64RoundTrip;
    procedure TestUnsignedAndUnicodeRoundTrip;
    procedure TestUnicodeCriteria;
    procedure TestDateCriteria;
    procedure TestAutoIncSequentialIDs;
    // Reserved words
    procedure TestReservedWordTableCRUD;
    procedure TestReservedWordVersionConflict;
    procedure TestReservedWordCriteriaOrderBy;
    // Parent keys as parameters
    procedure TestParentKeyWithQuotes;
    procedure TestParentKeyInjectionAttempt;
    procedure TestUnsavedParentHasNoChildren;
    // Object state after failures
    procedure TestFailedUpdateRetryNoFalseConflict;
    procedure TestDuplicateKeyInsertStaysNew;
    procedure TestExplicitRollbackRestoresAndResave;
    procedure TestSaveListPartialFailureThenRetry;
    procedure TestStaleActiveTransactionRaises;
    // Dialect and builder
    procedure TestDialectDetection;
    procedure TestManagerBuilderIndependentOfGlobal;
    // Schema and migration
    procedure TestSchemaRoundTripNoDifferences;
    procedure TestMigrateWidensVarchar;
    procedure TestMigrationTrackingTable;
    procedure TestMigrateNarrowingNeedsOption;
    procedure TestMigrateNewTableGetsIndexAndForeignKey;
    procedure TestMigrateNamedForeignKey;
    procedure TestFailedMigrationLeavesNothing;
    procedure TestCreateSchemaMutualRelations;
    procedure TestTreeCascadeDelete;
    procedure TestContainsLiteralWildcards;
    procedure TestDeleteStaleVersionRaises;
    procedure TestDuplicateKeyErrorDetails;
  end;

  TSQLiteSafety = class(TTestSafetyCommon)
  protected
    class function HelperClass: TDatabaseSetupHelperClass; override;
  end;

  TPgSQLSafety = class(TTestSafetyCommon)
  protected
    class function HelperClass: TDatabaseSetupHelperClass; override;
  end;

  TFirebirdSafety = class(TTestSafetyCommon)
  protected
    class function HelperClass: TDatabaseSetupHelperClass; override;
  end;

  TMySQLSafety = class(TTestSafetyCommon)
  protected
    class function HelperClass: TDatabaseSetupHelperClass; override;
  end;

  TOracleSafety = class(TTestSafetyCommon)
  protected
    class function HelperClass: TDatabaseSetupHelperClass; override;
  end;

implementation

const
  cTrackingTable = 'safe_migrations';

{ Persistence state boilerplate }

function TSafeItem.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TSafeItem.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

function TSafeParent.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TSafeParent.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

function TSafeChild.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TSafeChild.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

function TSafeTree.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TSafeTree.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

function TOrder.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TOrder.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

{ TTestSafetyCommon }

function TTestSafetyCommon.Dialect: TSQLDialect;
begin
  Result:=HelperClass.GetDialect;
end;

procedure TTestSafetyCommon.EndConnectionTransaction;
begin
  if Assigned(FConn) and Assigned(FConn.Transaction) and FConn.Transaction.Active then
    FConn.Transaction.Commit;
end;

procedure TTestSafetyCommon.DropTables;
const
  // Typed: an untyped array constant takes the length of its first element
  Tables: array[0..8] of String = ('SafeTree', 'SafeNote', 'SafeChild', 'SafeParent', 'SafeItem', '"Order"',
    'SafeLoopA', 'SafeLoopB', cTrackingTable);
  LoopKeys: array[0..1] of String = ('SafeLoopA:fk_SafeLoopA_SafeLoopB', 'SafeLoopB:fk_SafeLoopB_SafeLoopA');
var
  T: String;
begin
  EndConnectionTransaction;
  // Tables that reference each other: the foreign keys first
  if Dialect<>dSQLite then
    for T in LoopKeys do
      try
        FConn.ExecuteDirect('ALTER TABLE '+Copy(T, 1, Pos(':', T)-1)+' DROP CONSTRAINT '+Copy(T, Pos(':', T)+1, 100));
        EndConnectionTransaction;
      except
        on Exception do
          if Assigned(FConn.Transaction) and FConn.Transaction.Active then
            FConn.Transaction.Rollback;
      end;
  // Children first: they reference the parents
  for T in Tables do
    try
      DeleteTables([T]);
    except
      on Exception do
        if Assigned(FConn.Transaction) and FConn.Transaction.Active then
          FConn.Transaction.Rollback;
    end;
  EndConnectionTransaction;
end;

function TTestSafetyCommon.Count(const aTable: UTF8String; const aWhere: UTF8String): Integer;
var
  Q: TSQLQuery;
begin
  EndConnectionTransaction;
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=FConn;
    Q.Transaction:=FConn.Transaction;
    Q.SQL.Text:='SELECT COUNT(*) FROM '+FSQLBuilder.Identifier(aTable, itTable);
    if aWhere<>'' then
      Q.SQL.Add('WHERE '+aWhere);
    FConn.Transaction.StartTransaction;
    Q.Open;
    Result:=Q.Fields[0].AsInteger;
    Q.Close;
    FConn.Transaction.Commit;
  finally
    Q.Free;
  end;
end;

function TTestSafetyCommon.NewItem(const aName: String; aQty: Integer; aSave: Boolean): TSafeItem;
begin
  Result:=TSafeItem.Create;
  FObjects.Add(Result);
  Result.Name:=aName;
  Result.Qty:=aQty;
  Result.State:=opsNew;
  if aSave then
    FManager.Save(Result);
end;

function TTestSafetyCommon.NewParent(const aCode: String; aSave: Boolean): TSafeParent;
begin
  Result:=TSafeParent.Create;
  FObjects.Add(Result);
  Result.SafeParentID:=aCode;
  Result.Name:='Parent '+aCode;
  Result.State:=opsNew;
  if aSave then
    FManager.Save(Result);
end;

function TTestSafetyCommon.NewChild(const aParent, aInfo: String): TSafeChild;
begin
  Result:=TSafeChild.Create;
  FObjects.Add(Result);
  Result.ParentCode:=aParent;
  Result.Info:=aInfo;
  Result.State:=opsNew;
  FManager.Save(Result);
end;

function TTestSafetyCommon.LoadItems(aCriteria: TOPCriteria): TFPObjectList;
begin
  Result:=TFPObjectList.Create(True);
  try
    FManager.Load(Result, TSafeItem, aCriteria);
  except
    Result.Free;
    raise;
  end;
end;

function TTestSafetyCommon.ItemNames(aList: TFPObjectList): String;
var
  I: Integer;
  L: TStringList;
begin
  L:=TStringList.Create;
  try
    for I:=0 to aList.Count-1 do
      L.Add(TSafeItem(aList[I]).Name);
    L.Sort;
    Result:=L.CommaText;
  finally
    L.Free;
  end;
end;

function TTestSafetyCommon.TrySave(aObject: TObject): Boolean;
begin
  Result:=True;
  try
    FManager.Save(aObject);
  except
    on Exception do
      Result:=False;
  end;
end;

procedure TTestSafetyCommon.SetUp;
var
  D: TPersisterDef;
  F: TOPFieldMap;
  C: TOPConnection;
begin
  inherited SetUp;
  SetUpDatabase(HelperClass);
  FObjects:=TFPObjectList.Create(True);
  FRegistry:=TOPRegistry.Create(Nil);
  // Items
  D:=FRegistry.RegisterPersister(TSafeItem, TRTTIPersister);
  F:=D.FieldMap.FindByPropertyName('ID');
  F.Options:=F.Options+[foInKey, foAutoInc]-[foInInsert];
  F:=D.FieldMap.FindByPropertyName('Name');
  F.MaxSize:=50;
  F.Options:=F.Options+[foRequired];
  F:=D.FieldMap.FindByPropertyName('Deleted');
  F.Options:=F.Options+[foSoftDelete, foRequired];
  F:=D.FieldMap.FindByPropertyName('Version');
  F.Options:=F.Options+[foVersion];
  D.FieldMap.FindByPropertyName('Text').MaxSize:=50;
  // Parents with a string key
  D:=FRegistry.RegisterPersister(TSafeParent, TRTTIPersister);
  F:=D.FieldMap.FindByPropertyName('SafeParentID');
  F.MaxSize:=40;
  F.Options:=F.Options+[foInKey, foRequired];
  F:=D.FieldMap.FindByPropertyName('Name');
  F.MaxSize:=60;
  // Children
  D:=FRegistry.RegisterPersister(TSafeChild, TRTTIPersister);
  F:=D.FieldMap.FindByPropertyName('ID');
  F.Options:=F.Options+[foInKey, foAutoInc]-[foInInsert];
  F:=D.FieldMap.FindByPropertyName('ParentCode');
  F.MaxSize:=40;
  F:=D.FieldMap.FindByPropertyName('Info');
  F.MaxSize:=50;
  // Reserved words
  D:=FRegistry.RegisterPersister(TOrder, TRTTIPersister);
  F:=D.FieldMap.FindByPropertyName('ID');
  F.Options:=F.Options+[foInKey, foAutoInc]-[foInInsert];
  F:=D.FieldMap.FindByPropertyName('User');
  F.MaxSize:=30;
  F:=D.FieldMap.FindByPropertyName('Level');
  F.Options:=F.Options+[foVersion];
  FRelations:=TOPRelationRegistry.Create(Nil);
  FRelations.Registry:=FRegistry;
  // A tree: nullable parent key, root rows have none
  D:=FRegistry.RegisterPersister(TSafeTree, TRTTIPersister);
  F:=D.FieldMap.FindByPropertyName('ID');
  F.Options:=F.Options+[foInKey];
  F:=D.FieldMap.FindByPropertyName('ParentTreeID');
  F.Options:=F.Options+[foNullable];
  FRelations.RegisterRelationship(TSafeParent, TSafeChild, 'ParentCode');
  FRelations.RegisterRelationship(TSafeTree, TSafeTree, 'ParentTreeID');
  FM2M:=TOPManyToManyRegistry.Create(Nil);
  FM2M.Registry:=FRegistry;
  // Builders: same dialect and reserved word strategy for DDL and DML
  FSQLBuilder:=TOPSQLBuilder.Create(Nil);
  FSQLBuilder.SQLDialect:=Dialect;
  FSQLBuilder.Registry:=FRegistry;
  FSQLBuilder.Driver.ReservedWordStrategy:=rwsQuoteOnDemand;
  FDDLBuilder:=TOPDDLBuilder.Create(Nil);
  FDDLBuilder.SQLDialect:=Dialect;
  FDDLBuilder.Registry:=FRegistry;
  FDDLBuilder.Driver.ReservedWordStrategy:=rwsQuoteOnDemand;
  DropTables;
  // SQLite cannot add a foreign key to an existing table
  if Dialect=dSQLite then
    FDDLBuilder.CreateSchema(FConn, FRegistry, FRelations, FM2M, [], [])
  else
    FDDLBuilder.CreateSchema(FConn, FRegistry, FRelations, FM2M, [], [scoForeignKeys]);
  FManager:=TOPManager.Create(Nil);
  FManager.Registry:=FRegistry;
  FManager.RelationRegistry:=FRelations;
  FManager.M2MRegistry:=FM2M;
  FManager.SQLBuilder:=FSQLBuilder;
  C:=FManager.Connections.Add as TOPConnection;
  C.SingleConnection:=FConn;
  C.IsDefault:=True;
  FMigrator:=TOPSchemaMigrator.Create(Nil);
  FMigrator.Registry:=FRegistry;
  FMigrator.RelationRegistry:=FRelations;
  FMigrator.M2MRegistry:=FM2M;
  FMigrator.SQLDialect:=Dialect;
  FMigrator.ReservedWordStrategy:=rwsQuoteOnDemand;
  FMigrator.MigrationTableName:=cTrackingTable;
end;

procedure TTestSafetyCommon.TearDown;
begin
  if Assigned(FManager) then
    while FManager.InTransaction do
      try
        FManager.Rollback;
      except
        on Exception do ;
      end;
  FreeAndNil(FObjects);
  FreeAndNil(FManager);
  FreeAndNil(FMigrator);
  if Assigned(FConn) then
    DropTables;
  FreeAndNil(FDDLBuilder);
  FreeAndNil(FSQLBuilder);
  FreeAndNil(FM2M);
  FreeAndNil(FRelations);
  FreeAndNil(FRegistry);
  TearDownDatabase;
  inherited TearDown;
end;

procedure TTestSafetyCommon.TestOrPlusSoftDeleteExcludesDeleted;
var
  C: TOPCriteria;
  L: TFPObjectList;
begin
  FManager.Delete(NewItem('a', 1));
  NewItem('b', 2);
  NewItem('c', 3);
  C:=TOPCriteria.Create;
  try
    C.Where('Name', opEqual, 'a').AddOr('Name', opEqual, 'b');
    L:=LoadItems(C);
    try
      // Before the fix: Name='a' OR (Name='b' AND not deleted) returned a as well
      AssertEquals('Deleted row excluded', 'b', ItemNames(L));
    finally
      L.Free;
    end;
  finally
    C.Free;
  end;
end;

procedure TTestSafetyCommon.TestOrThenAndGrouping;
var
  C: TOPCriteria;
  L: TFPObjectList;
begin
  NewItem('a', 1);
  NewItem('b', 2);
  NewItem('c', 1);
  C:=TOPCriteria.Create;
  try
    // (a OR b) AND Qty=1: only a; read as a OR (b AND Qty=1) it would also be a
    C.Where('Name', opEqual, 'b').AddOr('Name', opEqual, 'c').AddAnd('Qty', opEqual, 1);
    L:=LoadItems(C);
    try
      AssertEquals('(b OR c) AND Qty=1', 'c', ItemNames(L));
    finally
      L.Free;
    end;
  finally
    C.Free;
  end;
end;

procedure TTestSafetyCommon.TestIncludeDeletedWithOr;
var
  C: TOPCriteria;
  L: TFPObjectList;
begin
  FManager.Delete(NewItem('a', 1));
  NewItem('b', 2);
  C:=TOPCriteria.Create;
  try
    C.Where('Name', opEqual, 'a').AddOr('Name', opEqual, 'b').IncludeDeleted;
    L:=LoadItems(C);
    try
      AssertEquals('Both', 'a,b', ItemNames(L));
    finally
      L.Free;
    end;
  finally
    C.Free;
  end;
end;

procedure TTestSafetyCommon.TestNotGroupWithSoftDelete;
var
  C: TOPCriteria;
  L: TFPObjectList;
begin
  FManager.Delete(NewItem('a', 1));
  NewItem('b', 2);
  NewItem('c', 3);
  C:=TOPCriteria.Create;
  try
    C.Where('Name', opEqual, 'b').AddOr('Name', opEqual, 'c').Not_;
    L:=LoadItems(C);
    try
      AssertEquals('NOT (b OR c), deleted a excluded', '', ItemNames(L));
    finally
      L.Free;
    end;
  finally
    C.Free;
  end;
end;

procedure TTestSafetyCommon.TestOrderByLimitWithSoftDelete;
var
  C: TOPCriteria;
  L: TFPObjectList;
begin
  FManager.Delete(NewItem('a', 9));
  NewItem('b', 2);
  NewItem('c', 3);
  NewItem('d', 1);
  C:=TOPCriteria.Create;
  try
    C.OrderBy('Qty DESC').Limit(2);
    L:=LoadItems(C);
    try
      AssertEquals('Two rows', 2, L.Count);
      AssertEquals('Highest not deleted first', 'c', TSafeItem(L[0]).Name);
      AssertEquals('Then', 'b', TSafeItem(L[1]).Name);
    finally
      L.Free;
    end;
  finally
    C.Free;
  end;
end;

procedure TTestSafetyCommon.TestEmptyWhereIn;
var
  C: TOPCriteria;
  L: TFPObjectList;
begin
  NewItem('a', 1);
  C:=TOPCriteria.Create;
  try
    C.WhereIn('Qty', []);
    L:=LoadItems(C);
    try
      AssertEquals('IN () matches nothing, no syntax error', 0, L.Count);
    finally
      L.Free;
    end;
  finally
    C.Free;
  end;
end;

procedure TTestSafetyCommon.TestWhereRaw;
var
  C: TOPCriteria;
  L: TFPObjectList;
begin
  NewItem('Alpha', 1);
  NewItem('beta', 2);
  C:=TOPCriteria.Create;
  try
    C.WhereRaw('UPPER('+FSQLBuilder.Identifier('Name')+') = ?', ['ALPHA']);
    L:=LoadItems(C);
    try
      AssertEquals('Raw expression with parameter', 'Alpha', ItemNames(L));
    finally
      L.Free;
    end;
  finally
    C.Free;
  end;
end;

procedure TTestSafetyCommon.TestDateCurrencyInt64RoundTrip;
var
  O, L: TSafeItem;
  Price: Currency;
begin
  // SQLite keeps DECIMAL as a double: use a value with fewer digits there
  if Dialect=dSQLite then
    Price:=12345678.1234
  else
    Price:=123456789012.3456;
  O:=NewItem('t', 1, False);
  O.Born:=EncodeDateTime(2026,10,15,13,14,15,678);
  O.Day:=EncodeDate(1999,12,31);
  O.Price:=Price;
  O.Big:=Int64(1) shl 40 + 1;
  FManager.Save(O);
  L:=TSafeItem.Create;
  FObjects.Add(L);
  L.ID:=O.ID;
  FManager.Load(L);
  AssertTrue('Currency exact: '+CurrToStr(L.Price), L.Price=Price);
  AssertEquals('Int64', O.Big, L.Big);
  AssertEquals('Date', EncodeDate(1999,12,31), L.Day, 1e-9);
  AssertEquals('Milliseconds', 678, MilliSecondOf(L.Born));
  AssertEquals('Seconds', 15, SecondOf(L.Born));
end;

{ 'Zoë', a CJK character and an emoji }

function SafeUnicodeText: UnicodeString;
begin
  Result:='Zo'+WideChar($EB)+' '+WideChar($6F22)+' '+WideChar($D83D)+WideChar($DE00);
end;

procedure TTestSafetyCommon.TestUnsignedAndUnicodeRoundTrip;
var
  O, L: TSafeItem;
begin
  O:=NewItem('u', 1, False);
  O.Card:=High(Cardinal);
  O.UQ:=QWord(High(Int64));
  O.Text:=SafeUnicodeText;
  FManager.Save(O);
  L:=TSafeItem.Create;
  FObjects.Add(L);
  L.ID:=O.ID;
  FManager.Load(L);
  AssertEquals('Cardinal', Int64(High(Cardinal)), Int64(L.Card));
  AssertTrue('QWord', L.UQ=QWord(High(Int64)));
  AssertTrue('Unicode text', L.Text=SafeUnicodeText);
end;

procedure TTestSafetyCommon.TestUnicodeCriteria;
var
  C: TOPCriteria;
  L: TFPObjectList;
  O: TSafeItem;
begin
  O:=NewItem('a', 1, False);
  O.Text:=SafeUnicodeText;
  FManager.Save(O);
  O:=NewItem('b', 1, False);
  O.Text:='plain';
  FManager.Save(O);
  C:=TOPCriteria.Create;
  try
    C.WhereIn('Text', [SafeUnicodeText]);
    L:=LoadItems(C);
    try
      AssertEquals('Found by unicode value', 'a', ItemNames(L));
    finally
      L.Free;
    end;
  finally
    C.Free;
  end;
end;

procedure TTestSafetyCommon.TestDateCriteria;
var
  C: TOPCriteria;
  L: TFPObjectList;
  O: TSafeItem;
begin
  O:=NewItem('old', 1, False);
  O.Born:=EncodeDate(2020,1,1);
  FManager.Save(O);
  O:=NewItem('new', 1, False);
  O.Born:=EncodeDate(2025,6,1);
  FManager.Save(O);
  C:=TOPCriteria.Create;
  try
    C.Where('Born', opGreater, EncodeDate(2023,1,1));
    L:=LoadItems(C);
    try
      AssertEquals('Compared as dates', 'new', ItemNames(L));
    finally
      L.Free;
    end;
  finally
    C.Free;
  end;
end;

procedure TTestSafetyCommon.TestAutoIncSequentialIDs;
var
  A, B, C: TSafeItem;
begin
  A:=NewItem('a', 1);
  B:=NewItem('b', 1);
  C:=NewItem('c', 1);
  AssertTrue('Generated key', A.ID>0);
  AssertEquals('Sequential', A.ID+1, B.ID);
  AssertEquals('Sequential', B.ID+1, C.ID);
end;

procedure TTestSafetyCommon.TestReservedWordTableCRUD;
var
  O, L: TOrder;
begin
  O:=TOrder.Create;
  FObjects.Add(O);
  O.User:='bob';
  O.Group:=7;
  O.State:=opsNew;
  FManager.Save(O);
  AssertTrue('Key', O.ID>0);
  AssertEquals('Initial version', 1, O.Level);
  L:=TOrder.Create;
  FObjects.Add(L);
  L.ID:=O.ID;
  FManager.Load(L);
  AssertEquals('Loaded user', 'bob', L.User);
  AssertEquals('Loaded group', 7, L.Group);
  L.User:='alice';
  L.State:=opsUpdate;
  FManager.Save(L);
  AssertEquals('Version incremented', 2, L.Level);
  FManager.Delete(L);
  AssertEquals('Deleted', 0, Count('Order'));
end;

procedure TTestSafetyCommon.TestReservedWordVersionConflict;
var
  O, A, B: TOrder;
  Raised: Boolean;
begin
  O:=TOrder.Create;
  FObjects.Add(O);
  O.User:='x';
  O.State:=opsNew;
  FManager.Save(O);
  A:=TOrder.Create;
  FObjects.Add(A);
  A.ID:=O.ID;
  FManager.Load(A);
  B:=TOrder.Create;
  FObjects.Add(B);
  B.ID:=O.ID;
  FManager.Load(B);
  A.User:='first';
  A.State:=opsUpdate;
  FManager.Save(A);
  B.User:='second';
  B.State:=opsUpdate;
  Raised:=False;
  try
    FManager.Save(B);
  except
    on EOPF do
      Raised:=True;
  end;
  AssertTrue('Stale update detected through the quoted version column', Raised);
  AssertEquals('Version of the stale object unchanged', 1, B.Level);
end;

procedure TTestSafetyCommon.TestReservedWordCriteriaOrderBy;
var
  C: TOPCriteria;
  L: TFPObjectList;
  I: Integer;
  O: TOrder;
begin
  for I:=1 to 3 do
  begin
    O:=TOrder.Create;
    FObjects.Add(O);
    O.User:='u';
    O.Group:=I;
    O.State:=opsNew;
    FManager.Save(O);
  end;
  C:=TOPCriteria.Create;
  L:=TFPObjectList.Create(True);
  try
    C.Where('User', opEqual, 'u').OrderBy('Group DESC');
    FManager.Load(L, TOrder, C);
    AssertEquals('All rows', 3, L.Count);
    AssertEquals('Ordered by the quoted column', 3, TOrder(L[0]).Group);
    AssertEquals('Ordered by the quoted column', 1, TOrder(L[2]).Group);
  finally
    L.Free;
    C.Free;
  end;
end;

procedure TTestSafetyCommon.TestParentKeyWithQuotes;
var
  P1: TSafeParent;
  L: TFPObjectList;
begin
  P1:=NewParent('O''Brien');
  NewParent('OBrien');
  NewChild('O''Brien', 'mine');
  NewChild('OBrien', 'other');
  L:=TFPObjectList.Create(True);
  try
    FManager.LoadChildren(P1, TSafeChild, L);
    AssertEquals('One child', 1, L.Count);
    AssertEquals('The right one', 'mine', TSafeChild(L[0]).Info);
  finally
    L.Free;
  end;
end;

procedure TTestSafetyCommon.TestParentKeyInjectionAttempt;
var
  P: TSafeParent;
  L: TFPObjectList;
begin
  // With MySQL backslash escapes, the old literal ended the string here
  P:=NewParent('a\'' OR 1=1 -- ');
  NewParent('zzz');
  NewChild('a\'' OR 1=1 -- ', 'mine');
  NewChild('zzz', 'other');
  L:=TFPObjectList.Create(True);
  try
    FManager.LoadChildren(P, TSafeChild, L);
    AssertEquals('Only its own child', 1, L.Count);
    AssertEquals('The right one', 'mine', TSafeChild(L[0]).Info);
  finally
    L.Free;
  end;
end;

procedure TTestSafetyCommon.TestUnsavedParentHasNoChildren;
var
  P: TSafeParent;
  L: TFPObjectList;
begin
  NewParent('real');
  NewChild('real', 'c');
  P:=NewParent('', False);
  L:=TFPObjectList.Create(True);
  try
    FManager.LoadChildren(P, TSafeChild, L);
    AssertEquals('No children for an empty key', 0, L.Count);
  finally
    L.Free;
  end;
end;

procedure TTestSafetyCommon.TestFailedUpdateRetryNoFalseConflict;
var
  O: TSafeItem;
begin
  if Dialect=dSQLite then
    Ignore('SQLite does not enforce VARCHAR lengths');
  O:=NewItem('ok', 1);
  AssertEquals('Version after insert', 1, O.Version);
  O.Name:=StringOfChar('x', 60); // Longer than VARCHAR(50)
  O.State:=opsUpdate;
  AssertFalse('Update fails in the database', TrySave(O));
  AssertEquals('Version not incremented', 1, O.Version);
  O.Name:='fine';
  FManager.Save(O);
  AssertEquals('No false concurrency conflict', 2, O.Version);
end;

procedure TTestSafetyCommon.TestDuplicateKeyInsertStaysNew;
var
  P: TSafeParent;
begin
  NewParent('dup');
  P:=NewParent('dup', False);
  AssertFalse('Duplicate key', TrySave(P));
  AssertEquals('Still new', Ord(opsNew), Ord(P.State));
  P.SafeParentID:='dup2';
  AssertTrue('Retry with another key', TrySave(P));
  AssertEquals('Two parents', 2, Count('SafeParent'));
end;

procedure TTestSafetyCommon.TestExplicitRollbackRestoresAndResave;
var
  O: TSafeItem;
begin
  O:=NewItem('r', 1, False);
  FManager.BeginTransaction;
  FManager.Save(O);
  AssertTrue('Key in the transaction', O.ID>0);
  FManager.Rollback;
  AssertEquals('Key restored', 0, O.ID);
  AssertEquals('State restored', Ord(opsNew), Ord(O.State));
  AssertEquals('Version restored', 0, O.Version);
  AssertEquals('No row', 0, Count('SafeItem'));
  FManager.Save(O);
  AssertEquals('Saved again', 1, Count('SafeItem'));
end;

procedure TTestSafetyCommon.TestSaveListPartialFailureThenRetry;
var
  L: TFPObjectList;
  I: Integer;
begin
  L:=TFPObjectList.Create(False);
  try
    L.Add(NewParent('p1', False));
    L.Add(NewParent('p2', False));
    L.Add(NewParent('p1', False)); // Duplicate key in the same list
    AssertFalse('List save fails', TrySave(L));
    for I:=0 to 2 do
      AssertEquals('Item '+IntToStr(I)+' new again', Ord(opsNew), Ord(TSafeParent(L[I]).State));
    AssertEquals('Nothing saved', 0, Count('SafeParent'));
    TSafeParent(L[2]).SafeParentID:='p3';
    AssertTrue('Retry succeeds', TrySave(L));
    AssertEquals('All saved', 3, Count('SafeParent'));
  finally
    L.Free;
  end;
end;

procedure TTestSafetyCommon.TestStaleActiveTransactionRaises;
var
  O: TSafeItem;
  Raised: Boolean;
begin
  O:=NewItem('s', 1, False);
  EndConnectionTransaction;
  FConn.Transaction.StartTransaction;
  Raised:=False;
  try
    FManager.Save(O);
  except
    on EOPFTransaction do
      Raised:=True;
  end;
  AssertTrue('Active connection transaction reported', Raised);
  AssertTrue('Left alone', FConn.Transaction.Active);
  FConn.Transaction.Rollback;
  FManager.Save(O);
  AssertEquals('Saved after the transaction ended', 1, Count('SafeItem'));
end;

procedure TTestSafetyCommon.TestDialectDetection;
var
  Info: String;
begin
  Info:='';
  try
    Info:=' (server '+FConn.GetConnectionInfo(citServerVersionString)+
          ' / '+FConn.GetConnectionInfo(citServerVersion)+')';
  except
    on Exception do ;
  end;
  AssertEquals('Detected dialect'+Info, HelperClass.GetDialect.ToString,
    DetectDialectFromConnection(FConn).ToString);
end;

procedure TTestSafetyCommon.TestManagerBuilderIndependentOfGlobal;
var
  O: TSafeItem;
begin
  // The global builder has another dialect than the manager's builder
  if Dialect=dSQLite then
    OPSQLBuilder.SQLDialect:=dPostGres
  else
    OPSQLBuilder.SQLDialect:=dSQLite;
  O:=NewItem('g', 1);
  AssertTrue('Key retrieved with the manager''s dialect', O.ID>0);
end;

procedure TTestSafetyCommon.TestSchemaRoundTripNoDifferences;
var
  R: TOPMigrationResult;
  S: String;
  I: Integer;
begin
  R:=FMigrator.MigrateSchema(FConn, 2, [moDryRun]);
  S:='';
  for I:=0 to High(R.SQL) do
    S:=S+LineEnding+R.SQL[I];
  AssertEquals('Schema created by CreateSchema matches the model:'+S, 0, Length(R.SQL));
end;

procedure TTestSafetyCommon.TestMigrateWidensVarchar;
var
  R: TOPMigrationResult;
  F: TOPFieldMap;
  O: TSafeItem;
begin
  if Dialect=dSQLite then
    Ignore('SQLite cannot alter a column type');
  F:=FRegistry.FindPersister(TSafeItem).FieldMap.FindByPropertyName('Name');
  F.MaxSize:=80;
  R:=FMigrator.MigrateSchema(FConn, 2);
  AssertEquals('One statement: '+string(Concat(R.SQL, [''])[0]), 1, Length(R.SQL));
  AssertEquals('Executed', 1, R.StatementsExecuted);
  AssertEquals('Version recorded', 2, R.VersionAfter);
  R:=FMigrator.MigrateSchema(FConn, 3, [moDryRun]);
  AssertEquals('No differences left', 0, Length(R.SQL));
  O:=NewItem(StringOfChar('w', 75), 1);
  AssertTrue('Longer value accepted', O.ID>0);
end;

{ Introspected table, -1 when it does not exist; the caller frees aSchema }

function FindIntrospectedTable(aConn: TSQLConnection; aBuilder: TOPDDLBuilder;
  const aTable: String; out aSchema: TOPSchemaInfo): Integer;
begin
  aSchema:=aBuilder.Driver.IntrospectSchema(aConn);
  if Assigned(aConn.Transaction) and aConn.Transaction.Active then
    aConn.Transaction.Commit;
  Result:=aSchema.FindTable(aTable);
end;

procedure TTestSafetyCommon.TestMigrateNarrowingNeedsOption;
var
  R: TOPMigrationResult;
  O: TSafeItem;
begin
  if Dialect=dSQLite then
    Ignore('SQLite cannot alter a column type');
  FRegistry.FindPersister(TSafeItem).FieldMap.FindByPropertyName('Name').MaxSize:=40;
  R:=FMigrator.MigrateSchema(FConn, 2);
  // DiffsFound also counts tables of other tests (drop candidates, filtered)
  AssertTrue('Narrowing found', R.DiffsFound>=1);
  AssertEquals('But not applied', 0, Length(R.SQL));
  O:=NewItem(StringOfChar('n', 45), 1);
  AssertTrue('Column still accepts 45 characters', O.ID>0);
  // Firebird cannot shorten a VARCHAR at all
  if Dialect in [dFirebird, dFirebird3] then
    Exit;
  // Soft delete keeps the row: remove it, or the database refuses to shorten
  FConn.ExecuteDirect('DELETE FROM '+FSQLBuilder.Identifier('SafeItem', itTable));
  EndConnectionTransaction;
  R:=FMigrator.MigrateSchema(FConn, 3, [moAllowNarrowing]);
  AssertEquals('Applied with moAllowNarrowing', 1, R.StatementsExecuted);
end;

procedure TTestSafetyCommon.TestMigrateNewTableGetsIndexAndForeignKey;
var
  D: TPersisterDef;
  Idx: TOPIndexDef;
  Schema: TOPSchemaInfo;
  T: Integer;
begin
  D:=FRegistry.RegisterPersister(TSafeNote, TRTTIPersister);
  D.FieldMap.FindByPropertyName('ID').Options:=D.FieldMap.FindByPropertyName('ID').Options+[foInKey, foAutoInc]-[foInInsert];
  D.FieldMap.FindByPropertyName('Note').MaxSize:=40;
  Idx.IndexName:='idx_safenote_note';
  Idx.FieldNames:='Note';
  D.Indexes:=[Idx];
  FRelations.RegisterRelationship(TSafeItem, TSafeNote, 'ItemID');
  FMigrator.MigrateSchema(FConn, 2);
  T:=FindIntrospectedTable(FConn, FDDLBuilder, 'SafeNote', Schema);
  try
    AssertTrue('Table created', T>=0);
    AssertTrue('Index created', Length(Schema.Tables[T].Indexes)>0);
    // SQLite cannot add a foreign key to an existing table: skipped there
    if Dialect<>dSQLite then
      AssertEquals('Foreign key created', 1, Length(Schema.Tables[T].ForeignKeys));
  finally
    Schema.Free;
  end;
end;

procedure TTestSafetyCommon.TestMigrateNamedForeignKey;
var
  D: TPersisterDef;
  Schema: TOPSchemaInfo;
  T: Integer;
  R: TOPMigrationResult;
begin
  if Dialect=dSQLite then
    Ignore('SQLite cannot add a foreign key to an existing table');
  D:=FRegistry.RegisterPersister(TSafeNote, TRTTIPersister);
  D.FieldMap.FindByPropertyName('ID').Options:=D.FieldMap.FindByPropertyName('ID').Options+[foInKey, foAutoInc]-[foInInsert];
  D.FieldMap.FindByPropertyName('Note').MaxSize:=40;
  FRelations.RegisterRelationship(TSafeItem, TSafeNote, 'ItemID').ConstraintName:='fk_safenote_named';
  FMigrator.MigrateSchema(FConn, 2);
  T:=FindIntrospectedTable(FConn, FDDLBuilder, 'SafeNote', Schema);
  try
    AssertTrue('Table created', T>=0);
    AssertEquals('One foreign key', 1, Length(Schema.Tables[T].ForeignKeys));
    AssertTrue('Constraint has the given name: '+Schema.Tables[T].ForeignKeys[0].ConstraintName,
      SameText('fk_safenote_named', Schema.Tables[T].ForeignKeys[0].ConstraintName));
  finally
    Schema.Free;
  end;
  // The named constraint is recognised: nothing left to migrate
  R:=FMigrator.MigrateSchema(FConn, 3, [moDryRun]);
  AssertEquals('No differences', 0, Length(R.SQL));
end;

procedure TTestSafetyCommon.TestFailedMigrationLeavesNothing;
var
  D: TPersisterDef;
  Idx: TOPIndexDef;
  Raised: Boolean;
  Schema: TOPSchemaInfo;
  T: Integer;
begin
  if not FDDLBuilder.Driver.SupportsTransactionalDDL then
    Ignore(Dialect.ToString+' has no transactional DDL');
  // The index name is taken: the last statement of the migration fails
  FConn.ExecuteDirect('CREATE INDEX idx_taken ON '+FSQLBuilder.Identifier('SafeParent', itTable)+' ('+FSQLBuilder.Identifier('Name')+')');
  EndConnectionTransaction;
  D:=FRegistry.RegisterPersister(TSafeNote, TRTTIPersister);
  D.FieldMap.FindByPropertyName('ID').Options:=D.FieldMap.FindByPropertyName('ID').Options+[foInKey, foAutoInc]-[foInInsert];
  D.FieldMap.FindByPropertyName('Note').MaxSize:=40;
  Idx.IndexName:='idx_taken';
  Idx.FieldNames:='Note';
  D.Indexes:=[Idx];
  Raised:=False;
  try
    FMigrator.MigrateSchema(FConn, 2);
  except
    on E: EOPF do
      Raised:=True;
  end;
  AssertTrue('Migration failed', Raised);
  AssertEquals('No version recorded', 0, Count(cTrackingTable, 'Version=2'));
  T:=FindIntrospectedTable(FConn, FDDLBuilder, 'SafeNote', Schema);
  Schema.Free;
  AssertEquals('Table rolled back', -1, T);
end;

procedure TTestSafetyCommon.TestCreateSchemaMutualRelations;
var
  Reg: TOPRegistry;
  Rel: TOPRelationRegistry;
  M2M: TOPManyToManyRegistry;
  D: TPersisterDef;
  Schema: TOPSchemaInfo;
  T: Integer;
begin
  if Dialect=dSQLite then
    Ignore('SQLite cannot add foreign keys to existing tables');
  Reg:=TOPRegistry.Create(Nil);
  Rel:=TOPRelationRegistry.Create(Nil);
  M2M:=TOPManyToManyRegistry.Create(Nil);
  try
    Rel.Registry:=Reg;
    M2M.Registry:=Reg;
    D:=Reg.RegisterPersister(TSafeLoopA, TRTTIPersister);
    D.FieldMap.FindByPropertyName('ID').Options:=D.FieldMap.FindByPropertyName('ID').Options+[foInKey, foRequired];
    D:=Reg.RegisterPersister(TSafeLoopB, TRTTIPersister);
    D.FieldMap.FindByPropertyName('ID').Options:=D.FieldMap.FindByPropertyName('ID').Options+[foInKey, foRequired];
    Rel.RegisterRelationship(TSafeLoopA, TSafeLoopB, 'LoopAID');
    Rel.RegisterRelationship(TSafeLoopB, TSafeLoopA, 'LoopBID');
    FDDLBuilder.CreateSchema(FConn, Reg, Rel, M2M, [], [scoForeignKeys]);
    T:=FindIntrospectedTable(FConn, FDDLBuilder, 'SafeLoopA', Schema);
    try
      AssertTrue('Table A', T>=0);
      AssertEquals('A references B', 1, Length(Schema.Tables[T].ForeignKeys));
      T:=Schema.FindTable('SafeLoopB');
      AssertTrue('Table B', T>=0);
      AssertEquals('B references A', 1, Length(Schema.Tables[T].ForeignKeys));
    finally
      Schema.Free;
    end;
  finally
    M2M.Free;
    Rel.Free;
    Reg.Free;
  end;
end;

procedure TTestSafetyCommon.TestContainsLiteralWildcards;

  function Find(const aText: UTF8String): String;
  var
    C: TOPCriteria;
    L: TFPObjectList;
  begin
    C:=TOPCriteria.Create;
    try
      C.Where('Name', opContains, aText).OrderBy('Name');
      L:=LoadItems(C);
      try
        Result:=ItemNames(L);
      finally
        L.Free;
      end;
    finally
      C.Free;
    end;
  end;

begin
  NewItem('50% off', 1);
  NewItem('50 off', 1);
  NewItem('a_b', 1);
  NewItem('axb', 1);
  NewItem('wow!', 1);
  AssertEquals('Percent is literal', '"50% off"', Find('50%'));
  AssertEquals('Underscore is literal', 'a_b', Find('_'));
  AssertEquals('Escape character is literal', 'wow!', Find('!'));
end;

procedure TTestSafetyCommon.TestTreeCascadeDelete;
var
  T: TSafeTree;
  Tbl: String;

  procedure AddNode(aID: Integer; const aParent: String);
  begin
    FConn.ExecuteDirect(Format('INSERT INTO %s (%s, %s) VALUES (%d, %s)',
      [Tbl, FSQLBuilder.Identifier('ID'), FSQLBuilder.Identifier('ParentTreeID'), aID, aParent]));
    EndConnectionTransaction;
  end;

begin
  Tbl:=FSQLBuilder.Identifier('SafeTree', itTable);
  // 1 - 2 - 4 - 5, 1 - 3, and a separate root 6. The foreign key to itself
  // (not on SQLite) requires the deepest rows to go first.
  AddNode(1, 'NULL');
  AddNode(2, '1');
  AddNode(3, '1');
  AddNode(4, '2');
  AddNode(5, '4');
  AddNode(6, 'NULL');
  T:=TSafeTree.Create;
  FObjects.Add(T);
  T.ID:=1;
  T.State:=opsClean;
  FManager.Delete(T);
  AssertEquals('Only the other root remains', 1, Count('SafeTree'));
end;

procedure TTestSafetyCommon.TestDeleteStaleVersionRaises;
var
  O: TSafeItem;
  Raised: Boolean;
begin
  O:=NewItem('v', 1);
  // Someone else changes the row
  FConn.ExecuteDirect(Format('UPDATE %s SET %s = %s + 1',
    [FSQLBuilder.Identifier('SafeItem', itTable), FSQLBuilder.Identifier('Version'), FSQLBuilder.Identifier('Version')]));
  EndConnectionTransaction;
  Raised:=False;
  try
    FManager.Delete(O);
  except
    on E: EOPF do
      Raised:=True;
  end;
  AssertTrue('Stale delete raises', Raised);
  AssertEquals('Row not soft-deleted', 0, Count('SafeItem', FSQLBuilder.Identifier('Deleted')+' = '+
    IfThen(FSQLBuilder.Driver.GetBooleanKeyword=kwBoolean, 'TRUE', '1')));
end;

procedure TTestSafetyCommon.TestDuplicateKeyErrorDetails;
var
  E1: TSafeParent;
begin
  NewParent('dup');
  E1:=NewParent('dup', False);
  try
    FManager.Save(E1);
    Fail('Duplicate key must fail');
  except
    on E: EOPF do
    begin
      AssertTrue('Error code or state: '+IntToStr(E.ErrorCode)+' '+E.SQLState,
        (E.ErrorCode<>0) or (E.SQLState<>''));
      if Dialect=dPostGres then
        AssertEquals('Unique violation', '23505', E.SQLState);
    end;
  end;
end;

procedure TTestSafetyCommon.TestMigrationTrackingTable;
var
  T: TOPMigrationTracker;
  H: TOPMigrationArray;
begin
  FMigrator.MigrationTableName:='_opf_migrations';
  FMigrator.MigrateSchema(FConn, 1);
  FMigrator.MigrateSchema(FConn, 2);
  T:=TOPMigrationTracker.Create;
  try
    T.Connection:=FConn;
    T.Dialect:=Dialect;
    H:=T.GetMigrationHistory;
    AssertEquals('Two versions recorded', 2, Length(H));
    AssertEquals('Current version', 2, T.GetCurrentVersion);
  finally
    T.Free;
    // The default name may need quotes (Firebird): drop it through the tracker's quoting
    EndConnectionTransaction;
    try
      T:=TOPMigrationTracker.Create;
      try
        T.Dialect:=Dialect;
        FDDLBuilder.ExecDDL(FConn, 'DROP TABLE '+T.QuotedTableName);
      finally
        T.Free;
      end;
    except
      on Exception do ;
    end;
  end;
end;

{ Backends }

class function TSQLiteSafety.HelperClass: TDatabaseSetupHelperClass;
begin
  Result:=TSQLiteSetupHelper;
end;

class function TPgSQLSafety.HelperClass: TDatabaseSetupHelperClass;
begin
  Result:=TPgSQLSetupHelper;
end;

class function TFirebirdSafety.HelperClass: TDatabaseSetupHelperClass;
begin
  Result:=TFirebirdSetupHelper;
end;

class function TMySQLSafety.HelperClass: TDatabaseSetupHelperClass;
begin
  Result:=TMySQLSetupHelper;
end;

class function TOracleSafety.HelperClass: TDatabaseSetupHelperClass;
begin
  Result:=TOracleSetupHelper;
end;

initialization
  RegisterTests('Safety', [TSQLiteSafety, TPgSQLSafety, TFirebirdSafety, TMySQLSafety, TOracleSafety]);
end.
