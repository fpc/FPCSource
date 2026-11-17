unit utc.opf.manager;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, fpcunit, testregistry,
{$ENDIF}
  Opf.Types, Opf.Manager, Opf.Fields;

type
  TMyManager = Class(TOPManager)
  end;

  { TMyOverrideManager }

  TMyOverrideManager = Class(TOPManager)
  public
    class var SaveOverrideCalled: Boolean;
    Procedure Save(aObject : TObject; aState : TObjectPersistenceState); override;
  end;

  { TTestCustomPersisterClass - test class for persister registration }

  TTestCustomPersisterClass = class(TPersistent)
  private
    FID: Integer;
  published
    property ID: Integer read FID write FID;
  end;

  { TTestSaveContact }

  TTestSaveContact = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  { TTestNoState }

  TTestNoState = class(TObject)
  end;

  { TTestManager }

  TTestManager = class(TTestCase)
  private
    procedure SetinstanceClass;
  protected
    FManager : TOPManager;
    procedure SetUp; override;
    procedure TearDown; override;
    Property Manager : TOPManager read FManager;
  published
    procedure TestHookUp;
    Procedure TestRegistry;
    Procedure TestSQLBuilder;
    Procedure TestGlobalInstance;
    procedure TestGlobalInstanceClass;
    procedure TestCustomManagerOverride;
    procedure TestCustomPersisterRegistration;
    procedure TestCustomPersisterLookup;
  end;

  { TTestManagerSave }

  TTestManagerSave = class(TTestCase)
  private
    FManager: TOPManager;
    FObj: TObject;
    procedure DoSaveNil;
    procedure DoSaveNoState;
    procedure DoSaveNoPersister;
    procedure DoSaveNoConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSaveNil;
    procedure TestSaveNoStateSupport;
    procedure TestSaveNoPersister;
    procedure TestSaveNoDefaultConnection;
  end;

  { TTestManagerDelete }

  TTestManagerDelete = class(TTestCase)
  private
    FManager: TOPManager;
    FObj: TObject;
    procedure DoDeleteNil;
    procedure DoDeleteNoState;
    procedure DoDeleteNoPersister;
    procedure DoDeleteNoConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDeleteNil;
    procedure TestDeleteNoStateSupport;
    procedure TestDeleteNoPersister;
    procedure TestDeleteNoDefaultConnection;
  end;

  { TTestManagerLoad }

  TTestManagerLoad = class(TTestCase)
  private
    FManager: TOPManager;
    FObj: TObject;
    procedure DoLoadNil;
    procedure DoLoadNoState;
    procedure DoLoadNoPersister;
    procedure DoLoadNoConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadNil;
    procedure TestLoadNoStateSupport;
    procedure TestLoadNoPersister;
    procedure TestLoadNoDefaultConnection;
  end;

  { TTestManagerErrorDiagnostics }

  TTestManagerErrorDiagnostics = class(TTestCase)
  private
    FManager: TOPManager;
    FObj: TObject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSaveUnregisteredClassMessage;
    procedure TestLoadUnregisteredClassMessage;
    procedure TestDeleteUnregisteredClassMessage;
    procedure TestSaveNoStateSupportMessage;
    procedure TestLoadNoStateSupportMessage;
    procedure TestDeleteNoStateSupportMessage;
    procedure TestSaveNoDefaultConnectionMessage;
    procedure TestLoadNoDefaultConnectionMessage;
    procedure TestDeleteNoDefaultConnectionMessage;
  end;

  { TTestManagerLoadList }

  TTestManagerLoadList = class(TTestCase)
  private
    FManager: TOPManager;
    procedure DoLoadListCriteriaSemicolon;
    procedure DoLoadListCriteriaComment;
    procedure DoLoadListCriteriaBlockComment;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadListNilListMessage;
    procedure TestLoadListNilClassMessage;
    procedure TestLoadListUnregisteredClassMessage;
    procedure TestLoadListNoManipulatorMessage;
    procedure TestLoadListNoDefaultConnectionMessage;
    procedure TestLoadListCriteriaSemicolonRejected;
    procedure TestLoadListCriteriaCommentRejected;
    procedure TestLoadListCriteriaBlockCommentRejected;
  end;

  { TTestManagerTransaction }

  TTestManagerTransaction = class(TTestCase)
  private
    FManager: TOPManager;
    procedure DoCommitWithoutBegin;
    procedure DoRollbackWithoutBegin;
    procedure DoNestedBeginTransaction;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCommitWithoutBeginRaises;
    procedure TestRollbackWithoutBeginRaises;
    procedure TestNestedBeginTransactionRaises;
    procedure TestBeginTransactionCreatesTransaction;
    procedure TestCommitEndsTransaction;
    procedure TestRollbackEndsTransaction;
  end;

  { TTestManagerLoadSQL }

  TTestManagerLoadSQL = class(TTestCase)
  private
    FManager: TOPManager;
    FObj: TObject;
    procedure DoLoadSQLNoState;
    procedure DoLoadSQLNoPersister;
    procedure DoLoadSQLNoConnection;
    procedure DoLoadSQLListNilList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadSQLNoStateSupport;
    procedure TestLoadSQLNoPersister;
    procedure TestLoadSQLNoDefaultConnection;
    procedure TestLoadSQLListNilList;
    procedure TestLoadSQLListNilClass;
  end;

  { TTestHookManager — exposes Do* methods for testing }

  THookCall = record
    Name: UTF8String;
    Obj: TObject;
    State: TObjectPersistenceState;
  end;
  THookCallArray = array of THookCall;

  TTestHookManager = Class(TOPManager)
  Public
    Calls: THookCallArray;
    Procedure DoBeforeSave(aObject: TObject; aState: TObjectPersistenceState); override;
    Procedure DoAfterSave(aObject: TObject; aState: TObjectPersistenceState); override;
    Procedure DoAfterLoad(aObject: TObject); override;
    Procedure DoBeforeDelete(aObject: TObject); override;
    Procedure DoAfterDelete(aObject: TObject); override;
    procedure ClearCalls;
    procedure AddCall(const aName: UTF8String; aObj: TObject; aState: TObjectPersistenceState);
  end;

  { TTestManagerHooks }

  TTestManagerHooks = class(TTestCase)
  private
    FManager: TTestHookManager;
    FObj: TTestSaveContact;
    // Event handler recording
    FEventCalls: THookCallArray;
    procedure AddEventCall(const aName: UTF8String; aObj: TObject; aState: TObjectPersistenceState);
    procedure HandleBeforeSave(Sender: TObject; aObject: TObject; aState: TObjectPersistenceState);
    procedure HandleAfterSave(Sender: TObject; aObject: TObject; aState: TObjectPersistenceState);
    procedure HandleAfterLoad(Sender: TObject; aObject: TObject);
    procedure HandleBeforeDelete(Sender: TObject; aObject: TObject);
    procedure HandleAfterDelete(Sender: TObject; aObject: TObject);
    procedure ClearEventCalls;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Do* override tests — verify the virtual method mechanism
    procedure TestDoBeforeSaveCallsEvent;
    procedure TestDoAfterSaveCallsEvent;
    procedure TestDoAfterLoadCallsEvent;
    procedure TestDoBeforeDeleteCallsEvent;
    procedure TestDoAfterDeleteCallsEvent;
    procedure TestDoBeforeSaveNoEvent;
    procedure TestDoAfterLoadNoEvent;
    // Event receives correct parameters
    procedure TestBeforeSaveEventReceivesOpsNew;
    procedure TestBeforeSaveEventReceivesOpsUpdate;
    procedure TestAfterSaveEventReceivesState;
    procedure TestAfterLoadEventReceivesObject;
    procedure TestBeforeDeleteEventReceivesObject;
    procedure TestAfterDeleteEventReceivesObject;
    // Override without inherited skips event
    procedure TestOverrideRecordsCalls;
  end;

  { TTestAuditManager — exposes protected ApplyAuditFields for testing }

  TTestAuditManager = Class(TOPManager)
  Public
    Procedure TestApplyAuditFields(aObject: TObject; aState: TObjectPersistenceState; aFieldMap: TOPFieldMapList);
  end;

  { TTestAuditContact — test object with audit fields }

  TTestAuditContact = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FCreatedAt: TDateTime;
    FUpdatedAt: TDateTime;
    FCreatedBy: String;
    FUpdatedBy: String;
    FCreatedByInt: Integer;
    FUpdatedByInt: Integer;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property UpdatedAt: TDateTime read FUpdatedAt write FUpdatedAt;
    property CreatedBy: String read FCreatedBy write FCreatedBy;
    property UpdatedBy: String read FUpdatedBy write FUpdatedBy;
    property CreatedByInt: Integer read FCreatedByInt write FCreatedByInt;
    property UpdatedByInt: Integer read FUpdatedByInt write FUpdatedByInt;
  end;

  { TTestManagerAudit }

  TTestManagerAudit = class(TTestCase)
  private
    FManager: TTestAuditManager;
    FObj: TTestAuditContact;
    FFieldMap: TOPFieldMapList;
    function AddField(const aPropName: String; aOptions: TOPFieldOptions): TOPFieldMap;
    procedure DoApplyAudit(aState: TObjectPersistenceState);
    procedure DoApplyAuditCreatedByNoUser;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Timestamp tests
    procedure TestCreatedAtOnInsert;
    procedure TestCreatedAtNotOnUpdate;
    procedure TestUpdatedAtOnUpdate;
    procedure TestUpdatedAtOnInsertWithRequired;
    procedure TestUpdatedAtOnInsertWithoutRequired;
    procedure TestTimestampUTC;
    // User tracking tests
    procedure TestCreatedByStringOnInsert;
    procedure TestCreatedByNumericalOnInsert;
    procedure TestCreatedByObjectKeyAfterFree;
    procedure TestCreatedByNumericalToStringConversion;
    procedure TestCreatedByNotOnUpdate;
    procedure TestUpdatedByOnUpdate;
    procedure TestUpdatedByOnInsertWithRequired;
    procedure TestUpdatedByOnInsertWithoutRequired;
    // Error cases
    procedure TestCreatedByRaisesWhenUserNotSet;
    // No-op
    procedure TestNoAuditFieldsNoOp;
  end;

  { TTestSoftDeleteManager — exposes protected methods for testing }

  TTestSoftDeleteManager = Class(TOPManager)
  Public
    Function TestGetSoftDeleteFilter(aFieldMap: TOPFieldMapList; const aTablePrefix: UTF8String = ''): UTF8String;
  end;

  { TTestManagerSoftDelete }

  TTestManagerSoftDelete = class(TTestCase)
  private
    FManager: TTestSoftDeleteManager;
    FFieldMap: TOPFieldMapList;
    function AddField(const aPropName: String; aKind: TTypeKind; aOptions: TOPFieldOptions): TOPFieldMap;
    procedure DoAddDuplicateSoftDelete;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // GetSoftDeleteFilter tests
    procedure TestFilterBoolRequired;
    procedure TestFilterBoolNullable;
    procedure TestFilterDateTime;
    procedure TestFilterNoSoftDelete;
    procedure TestFilterWithTablePrefix;
    // Validation
    procedure TestDuplicateSoftDeleteRaises;
  end;

  { TTestOPConnection }

  TTestOPConnection = class(TTestCase)
  private
    FList: TOPConnectionList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property List: TOPConnectionList read FList;
  published
    procedure TestIsDefaultSingle;
    procedure TestIsDefaultMutualExclusivity;
    procedure TestIsDefaultClearAll;
    procedure TestGetDefaultConnection;
    procedure TestSwitchDefaultConnection;
    procedure TestSaveLoadStreamWithDefault;
    procedure TestSaveLoadStreamNoDefault;
  end;

  { TTestRegistryErrorDiagnostics }

  TTestRegistryErrorDiagnostics = class(TTestCase)
  protected
    procedure TearDown; override;
  published
    procedure TestDuplicatePersisterMessage;
    procedure TestNilClassRegistrationMessage;
  end;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  Data.Sqldb, Data.SqlDb.Sqlite3,
{$ELSE}
  sqldb, sqlite3conn,
{$ENDIF}
  Opf.Registry, Opf.Sql, Opf.Driver,
  Opf.Persister.RTTI,
  Opf.ListReg, contnrs;

{ TTestManagerDelete }

procedure TTestManagerDelete.DoDeleteNil;
begin
  FManager.Delete(nil);
end;

procedure TTestManagerDelete.DoDeleteNoState;
begin
  FManager.Delete(FObj);
end;

procedure TTestManagerDelete.DoDeleteNoPersister;
begin
  FManager.Delete(FObj);
end;

procedure TTestManagerDelete.DoDeleteNoConnection;
begin
  FManager.Delete(FObj);
end;

procedure TTestManagerDelete.SetUp;
begin
  inherited;
  FManager:=TOPManager.Create(Nil);
  FObj:=Nil;
  TOPRegistry.ClearInstance;
end;

procedure TTestManagerDelete.TearDown;
begin
  FreeAndNil(FObj);
  TOPRegistry.ClearInstance;
  FreeAndNil(FManager);
  inherited;
end;

procedure TTestManagerDelete.TestDeleteNil;
begin
  AssertException('Nil object', EOPF, @DoDeleteNil);
end;

procedure TTestManagerDelete.TestDeleteNoStateSupport;
begin
  FObj:=TTestNoState.Create;
  AssertException('No IOPPersistenceState', EOPF, @DoDeleteNoState);
end;

procedure TTestManagerDelete.TestDeleteNoPersister;
begin
  FObj:=TTestSaveContact.Create;
  TTestSaveContact(FObj).FState:=opsNew;
  AssertException('No persister registered', EOPF, @DoDeleteNoPersister);
end;

procedure TTestManagerDelete.TestDeleteNoDefaultConnection;
begin
  OPRegistry.RegisterPersister(TTestSaveContact, TRTTIPersister);
  FObj:=TTestSaveContact.Create;
  TTestSaveContact(FObj).FState:=opsNew;
  AssertException('No default connection', EOPF, @DoDeleteNoConnection);
end;

{ TTestSaveContact }

function TTestSaveContact.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TTestSaveContact.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

{ TTestManager }

procedure TTestManager.TestHookUp;

begin
  AssertNotNull('Have manager',Manager);
  AssertNotNull('Manager has connections instance',Manager.Connections);
  AssertEquals('Manager has no connections',0,Manager.Connections.Count);
end;

procedure TTestManager.TestRegistry;

Var
  R : TOPRegistry;

begin
  AssertSame('Default',OPRegistry,Manager.Registry);
  R:=TOPRegistry.Create(Nil);
  try
    Manager.Registry:=R;
    AssertSame('Assigned',R,Manager.Registry);
    Manager.Registry:=Nil;
    AssertSame('Again Default',OPRegistry,Manager.Registry);
  finally
    R.Free;
  end;
end;

procedure TTestManager.TestSQLBuilder;
Var
  R : TOPSQLBuilder;

begin
  AssertSame('Default',OPSQLBuilder,Manager.SQLBuilder);
  R:=TOPSQLBuilder.Create(Nil);
  try
    Manager.SQLBuilder:=R;
    AssertSame('Assigned',R,Manager.SQLBuilder);
    Manager.SQLBuilder:=Nil;
    AssertSame('Again Default',OPSQLBuilder,Manager.SQLBuilder);
  finally
    R.Free;
  end;
end;

procedure TTestManager.TestGlobalInstance;
var
  M : TOPManager;
begin
  M:=TOPManager.Instance;
  AssertNotNull('Have instance',M);
  TOPManager.ClearInstance;
  AssertNotNull('Have instance',TOPManager.Instance);
end;

procedure TTestManager.SetinstanceClass;
begin
  TOPManager.SetInstanceClass(TMyManager);
end;

procedure TTestManager.TestGlobalInstanceClass;

Var
  M : TOPManager;

begin
  TOPManager.ClearInstance;
  Self.SetinstanceClass;
  try
    M:=TOPManager.Instance;
    AssertNotNull('Have instance',M);
    AssertEquals('Correct class',TMyManager,M.ClassType);
    AssertException('Cannot set class if instance is active',EOPF,@Self.SetinstanceClass);
  finally
    TOPManager.ClearInstance;
    TOPManager.SetInstanceClass(Nil);
  end;
end;


{ TMyOverrideManager }

procedure TMyOverrideManager.Save(aObject: TObject; aState: TObjectPersistenceState);
begin
  SaveOverrideCalled:=True;
end;

procedure TTestManager.TestCustomManagerOverride;
begin
  TOPManager.ClearInstance;
  TOPManager.SetInstanceClass(TMyOverrideManager);
  try
    TMyOverrideManager.SaveOverrideCalled:=False;
    TOPManager.Instance.Save(Nil, opsNew);
    AssertTrue('Custom Save override was called', TMyOverrideManager.SaveOverrideCalled);
  finally
    TOPManager.ClearInstance;
    TOPManager.SetInstanceClass(Nil);
  end;
end;

procedure TTestManager.TestCustomPersisterRegistration;
var
  D: TPersisterDef;
begin
  TOPRegistry.ClearInstance;
  try
    D:=OPRegistry.RegisterPersister(TTestCustomPersisterClass, TRTTIPersister);
    AssertNotNull('Registered persister def', D);
    AssertEquals('PersisterClass', TRTTIPersister, D.PersisterClass);
    AssertEquals('InstanceClass', TTestCustomPersisterClass, D.InstanceClass);
  finally
    TOPRegistry.ClearInstance;
  end;
end;

procedure TTestManager.TestCustomPersisterLookup;
var
  D: TPersisterDef;
begin
  TOPRegistry.ClearInstance;
  try
    OPRegistry.RegisterPersister(TTestCustomPersisterClass, TRTTIPersister);
    D:=OPRegistry.FindPersister(TTestCustomPersisterClass);
    AssertNotNull('Found persister def', D);
    AssertEquals('PersisterClass matches', TRTTIPersister, D.PersisterClass);
  finally
    TOPRegistry.ClearInstance;
  end;
end;

procedure TTestManager.SetUp;
begin
  Inherited;
  FManager:=TOPManager.Create(Nil);
  TOPManager.ClearInstance;
end;

procedure TTestManager.TearDown;
begin
  TOPManager.ClearInstance;
  TOPManager.SetInstanceClass(Nil);
  FreeAndNil(FManager);
  Inherited;
end;

{ TTestManagerSave }

procedure TTestManagerSave.DoSaveNil;
begin
  FManager.Save(nil);
end;

procedure TTestManagerSave.DoSaveNoState;
begin
  FManager.Save(FObj);
end;

procedure TTestManagerSave.DoSaveNoPersister;
begin
  FManager.Save(FObj);
end;

procedure TTestManagerSave.DoSaveNoConnection;
begin
  FManager.Save(FObj);
end;

procedure TTestManagerSave.SetUp;
begin
  inherited;
  FManager:=TOPManager.Create(Nil);
  FObj:=Nil;
  TOPRegistry.ClearInstance;
end;

procedure TTestManagerSave.TearDown;
begin
  FreeAndNil(FObj);
  TOPRegistry.ClearInstance;
  FreeAndNil(FManager);
  inherited;
end;

procedure TTestManagerSave.TestSaveNil;
begin
  AssertException('Nil object', EOPF, @DoSaveNil);
end;

procedure TTestManagerSave.TestSaveNoStateSupport;
begin
  FObj:=TTestNoState.Create;
  AssertException('No IOPPersistenceState', EOPF, @DoSaveNoState);
end;

procedure TTestManagerSave.TestSaveNoPersister;
begin
  FObj:=TTestSaveContact.Create;
  TTestSaveContact(FObj).FState:=opsNew;
  AssertException('No persister registered', EOPF, @DoSaveNoPersister);
end;

procedure TTestManagerSave.TestSaveNoDefaultConnection;
begin
  OPRegistry.RegisterPersister(TTestSaveContact, TRTTIPersister);
  FObj:=TTestSaveContact.Create;
  TTestSaveContact(FObj).FState:=opsNew;
  AssertException('No default connection', EOPF, @DoSaveNoConnection);
end;

{ TTestManagerLoad }

procedure TTestManagerLoad.DoLoadNil;
begin
  FManager.Load(nil);
end;

procedure TTestManagerLoad.DoLoadNoState;
begin
  FManager.Load(FObj);
end;

procedure TTestManagerLoad.DoLoadNoPersister;
begin
  FManager.Load(FObj);
end;

procedure TTestManagerLoad.DoLoadNoConnection;
begin
  FManager.Load(FObj);
end;

procedure TTestManagerLoad.SetUp;
begin
  inherited;
  FManager:=TOPManager.Create(Nil);
  FObj:=Nil;
  TOPRegistry.ClearInstance;
end;

procedure TTestManagerLoad.TearDown;
begin
  FreeAndNil(FObj);
  TOPRegistry.ClearInstance;
  FreeAndNil(FManager);
  inherited;
end;

procedure TTestManagerLoad.TestLoadNil;
begin
  AssertException('Nil object', EOPF, @DoLoadNil);
end;

procedure TTestManagerLoad.TestLoadNoStateSupport;
begin
  FObj:=TTestNoState.Create;
  AssertException('No IOPPersistenceState', EOPF, @DoLoadNoState);
end;

procedure TTestManagerLoad.TestLoadNoPersister;
begin
  FObj:=TTestSaveContact.Create;
  AssertException('No persister registered', EOPF, @DoLoadNoPersister);
end;

procedure TTestManagerLoad.TestLoadNoDefaultConnection;
begin
  OPRegistry.RegisterPersister(TTestSaveContact, TRTTIPersister);
  FObj:=TTestSaveContact.Create;
  AssertException('No default connection', EOPF, @DoLoadNoConnection);
end;

{ TTestManagerErrorDiagnostics }

procedure TTestManagerErrorDiagnostics.SetUp;
begin
  inherited;
  FManager:=TOPManager.Create(Nil);
  FObj:=Nil;
  TOPRegistry.ClearInstance;
end;

procedure TTestManagerErrorDiagnostics.TearDown;
begin
  FreeAndNil(FObj);
  TOPRegistry.ClearInstance;
  FreeAndNil(FManager);
  inherited;
end;

procedure TTestManagerErrorDiagnostics.TestSaveUnregisteredClassMessage;
begin
  FObj:=TTestSaveContact.Create;
  TTestSaveContact(FObj).FState:=opsNew;
  try
    FManager.Save(FObj);
    Fail('Expected EOPF');
  except
    on E: EOPF do
    begin
      AssertTrue('Message contains operation', Pos('Save', E.Message) > 0);
      AssertTrue('Message contains class name', Pos('TTestSaveContact', E.Message) > 0);
      AssertTrue('Message contains reason', Pos('No persister registered', E.Message) > 0);
    end;
  end;
end;

procedure TTestManagerErrorDiagnostics.TestLoadUnregisteredClassMessage;
begin
  FObj:=TTestSaveContact.Create;
  try
    FManager.Load(FObj);
    Fail('Expected EOPF');
  except
    on E: EOPF do
    begin
      AssertTrue('Message contains operation', Pos('Load', E.Message) > 0);
      AssertTrue('Message contains class name', Pos('TTestSaveContact', E.Message) > 0);
      AssertTrue('Message contains reason', Pos('No persister registered', E.Message) > 0);
    end;
  end;
end;

procedure TTestManagerErrorDiagnostics.TestDeleteUnregisteredClassMessage;
begin
  FObj:=TTestSaveContact.Create;
  TTestSaveContact(FObj).FState:=opsNew;
  try
    FManager.Delete(FObj);
    Fail('Expected EOPF');
  except
    on E: EOPF do
    begin
      AssertTrue('Message contains operation', Pos('Delete', E.Message) > 0);
      AssertTrue('Message contains class name', Pos('TTestSaveContact', E.Message) > 0);
      AssertTrue('Message contains reason', Pos('No persister registered', E.Message) > 0);
    end;
  end;
end;

procedure TTestManagerErrorDiagnostics.TestSaveNoStateSupportMessage;
begin
  FObj:=TTestNoState.Create;
  try
    FManager.Save(FObj);
    Fail('Expected EOPF');
  except
    on E: EOPF do
    begin
      AssertTrue('Message contains operation', Pos('Save', E.Message) > 0);
      AssertTrue('Message contains class name', Pos('TTestNoState', E.Message) > 0);
      AssertTrue('Message contains reason', Pos('persistent state support', E.Message) > 0);
    end;
  end;
end;

procedure TTestManagerErrorDiagnostics.TestLoadNoStateSupportMessage;
begin
  FObj:=TTestNoState.Create;
  try
    FManager.Load(FObj);
    Fail('Expected EOPF');
  except
    on E: EOPF do
    begin
      AssertTrue('Message contains operation', Pos('Load', E.Message) > 0);
      AssertTrue('Message contains class name', Pos('TTestNoState', E.Message) > 0);
      AssertTrue('Message contains reason', Pos('persistent state support', E.Message) > 0);
    end;
  end;
end;

procedure TTestManagerErrorDiagnostics.TestDeleteNoStateSupportMessage;
begin
  FObj:=TTestNoState.Create;
  try
    FManager.Delete(FObj);
    Fail('Expected EOPF');
  except
    on E: EOPF do
    begin
      AssertTrue('Message contains operation', Pos('Delete', E.Message) > 0);
      AssertTrue('Message contains class name', Pos('TTestNoState', E.Message) > 0);
      AssertTrue('Message contains reason', Pos('persistent state support', E.Message) > 0);
    end;
  end;
end;

procedure TTestManagerErrorDiagnostics.TestSaveNoDefaultConnectionMessage;
begin
  OPRegistry.RegisterPersister(TTestSaveContact, TRTTIPersister);
  FObj:=TTestSaveContact.Create;
  TTestSaveContact(FObj).FState:=opsNew;
  try
    FManager.Save(FObj);
    Fail('Expected EOPF');
  except
    on E: EOPF do
    begin
      AssertTrue('Message contains class name', Pos('TOPManager', E.Message) > 0);
      AssertTrue('Message contains operation', Pos('GetDefaultConnection', E.Message) > 0);
      AssertTrue('Message contains reason', Pos('No default connection', E.Message) > 0);
    end;
  end;
end;

procedure TTestManagerErrorDiagnostics.TestLoadNoDefaultConnectionMessage;
begin
  OPRegistry.RegisterPersister(TTestSaveContact, TRTTIPersister);
  FObj:=TTestSaveContact.Create;
  try
    FManager.Load(FObj);
    Fail('Expected EOPF');
  except
    on E: EOPF do
    begin
      AssertTrue('Message contains class name', Pos('TOPManager', E.Message) > 0);
      AssertTrue('Message contains operation', Pos('GetDefaultConnection', E.Message) > 0);
      AssertTrue('Message contains reason', Pos('No default connection', E.Message) > 0);
    end;
  end;
end;

procedure TTestManagerErrorDiagnostics.TestDeleteNoDefaultConnectionMessage;
begin
  OPRegistry.RegisterPersister(TTestSaveContact, TRTTIPersister);
  FObj:=TTestSaveContact.Create;
  TTestSaveContact(FObj).FState:=opsNew;
  try
    FManager.Delete(FObj);
    Fail('Expected EOPF');
  except
    on E: EOPF do
    begin
      AssertTrue('Message contains class name', Pos('TOPManager', E.Message) > 0);
      AssertTrue('Message contains operation', Pos('GetDefaultConnection', E.Message) > 0);
      AssertTrue('Message contains reason', Pos('No default connection', E.Message) > 0);
    end;
  end;
end;

{ TTestManagerLoadList }

procedure TTestManagerLoadList.SetUp;
begin
  inherited;
  FManager:=TOPManager.Create(Nil);
  TOPRegistry.ClearInstance;
  TOPListRegistry.ClearInstance;
end;

procedure TTestManagerLoadList.TearDown;
begin
  TOPListRegistry.ClearInstance;
  TOPRegistry.ClearInstance;
  FreeAndNil(FManager);
  inherited;
end;

procedure TTestManagerLoadList.TestLoadListNilListMessage;
begin
  try
    FManager.Load(Nil, TTestSaveContact, '');
    Fail('Expected EOPF');
  except
    on E: EOPF do
    begin
      AssertTrue('Message contains operation', Pos('Load', E.Message) > 0);
      AssertTrue('Message contains reason', Pos('List object is nil', E.Message) > 0);
    end;
  end;
end;

procedure TTestManagerLoadList.TestLoadListNilClassMessage;
var
  List: TObjectList;
begin
  List:=TObjectList.Create(True);
  try
    try
      FManager.Load(List, Nil, '');
      Fail('Expected EOPF');
    except
      on E: EOPF do
      begin
        AssertTrue('Message contains operation', Pos('Load', E.Message) > 0);
        AssertTrue('Message contains reason', Pos('Object class is nil', E.Message) > 0);
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TTestManagerLoadList.TestLoadListUnregisteredClassMessage;
var
  List: TObjectList;
begin
  List:=TObjectList.Create(True);
  try
    try
      FManager.Load(List, TTestSaveContact, '');
      Fail('Expected EOPF');
    except
      on E: EOPF do
      begin
        AssertTrue('Message contains operation', Pos('Load', E.Message) > 0);
        AssertTrue('Message contains reason', Pos('No persister registered', E.Message) > 0);
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TTestManagerLoadList.TestLoadListNoManipulatorMessage;
var
  List: TStringList;
begin
  OPRegistry.RegisterPersister(TTestSaveContact, TRTTIPersister);
  List:=TStringList.Create;
  try
    try
      FManager.Load(List, TTestSaveContact, '');
      Fail('Expected EOPF');
    except
      on E: EOPF do
      begin
        AssertTrue('Message contains operation', Pos('Load', E.Message) > 0);
        AssertTrue('Message contains reason', Pos('No list manipulator registered', E.Message) > 0);
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TTestManagerLoadList.TestLoadListNoDefaultConnectionMessage;
var
  List: TObjectList;
begin
  OPRegistry.RegisterPersister(TTestSaveContact, TRTTIPersister);
  List:=TObjectList.Create(True);
  try
    try
      FManager.Load(List, TTestSaveContact, '');
      Fail('Expected EOPF');
    except
      on E: EOPF do
      begin
        AssertTrue('Message contains class name', Pos('TOPManager', E.Message) > 0);
        AssertTrue('Message contains operation', Pos('GetDefaultConnection', E.Message) > 0);
        AssertTrue('Message contains reason', Pos('No default connection', E.Message) > 0);
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TTestManagerLoadList.DoLoadListCriteriaSemicolon;
var
  List: TObjectList;
begin
  List:=TObjectList.Create(True);
  try
    FManager.Load(List, TTestSaveContact, 'ID=1; DROP TABLE Contact');
  finally
    List.Free;
  end;
end;

procedure TTestManagerLoadList.DoLoadListCriteriaComment;
var
  List: TObjectList;
begin
  List:=TObjectList.Create(True);
  try
    FManager.Load(List, TTestSaveContact, 'ID=1 -- bypass');
  finally
    List.Free;
  end;
end;

procedure TTestManagerLoadList.DoLoadListCriteriaBlockComment;
var
  List: TObjectList;
begin
  List:=TObjectList.Create(True);
  try
    FManager.Load(List, TTestSaveContact, 'ID=1 /* inject */');
  finally
    List.Free;
  end;
end;

procedure TTestManagerLoadList.TestLoadListCriteriaSemicolonRejected;
begin
  AssertException('Semicolon rejected', EOPF, @DoLoadListCriteriaSemicolon);
end;

procedure TTestManagerLoadList.TestLoadListCriteriaCommentRejected;
begin
  AssertException('Line comment rejected', EOPF, @DoLoadListCriteriaComment);
end;

procedure TTestManagerLoadList.TestLoadListCriteriaBlockCommentRejected;
begin
  AssertException('Block comment rejected', EOPF, @DoLoadListCriteriaBlockComment);
end;

{ TTestManagerTransaction }

procedure TTestManagerTransaction.DoCommitWithoutBegin;
begin
  FManager.Commit;
end;

procedure TTestManagerTransaction.DoRollbackWithoutBegin;
begin
  FManager.Rollback;
end;

procedure TTestManagerTransaction.DoNestedBeginTransaction;
begin
  FManager.BeginTransaction;
end;

procedure TTestManagerTransaction.SetUp;
var
  Conn: TOPConnection;
  SQLiteConn: TSQLite3Connection;
begin
  inherited SetUp;
  TOPManager.ClearInstance;
  FManager:=TOPManager.Instance;
  SQLiteConn:=TSQLite3Connection.Create(FManager);
  SQLiteConn.DatabaseName:=':memory:';
  SQLiteConn.Open;
  Conn:=FManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=SQLiteConn;
  Conn.IsDefault:=True;
end;

procedure TTestManagerTransaction.TearDown;
begin
  TOPManager.ClearInstance;
  FManager:=Nil;
  inherited TearDown;
end;

procedure TTestManagerTransaction.TestCommitWithoutBeginRaises;
begin
  AssertException('Commit without Begin', EOPF, @DoCommitWithoutBegin);
end;

procedure TTestManagerTransaction.TestRollbackWithoutBeginRaises;
begin
  AssertException('Rollback without Begin', EOPF, @DoRollbackWithoutBegin);
end;

procedure TTestManagerTransaction.TestNestedBeginTransactionRaises;
begin
  // Nested BeginTransaction is now valid — counter-based design allows nesting.
  // Verify it does NOT raise and counter increments correctly.
  FManager.BeginTransaction;
  FManager.BeginTransaction; // nested — should not raise
  AssertTrue('Still in transaction after nested begin', FManager.InTransaction);
  FManager.Rollback; // counter -> 1, sets doom
  FManager.Rollback; // counter -> 0, rollback all
  AssertFalse('Not in transaction after all rollbacks', FManager.InTransaction);
end;

procedure TTestManagerTransaction.TestBeginTransactionCreatesTransaction;
begin
  AssertFalse('Not in transaction before', FManager.InTransaction);
  FManager.BeginTransaction;
  AssertTrue('In transaction after Begin', FManager.InTransaction);
  FManager.Rollback;
end;

procedure TTestManagerTransaction.TestCommitEndsTransaction;
begin
  FManager.BeginTransaction;
  AssertTrue('In transaction after Begin', FManager.InTransaction);
  FManager.Commit;
  AssertFalse('Not in transaction after Commit', FManager.InTransaction);
end;

procedure TTestManagerTransaction.TestRollbackEndsTransaction;
begin
  FManager.BeginTransaction;
  AssertTrue('In transaction after Begin', FManager.InTransaction);
  FManager.Rollback;
  AssertFalse('Not in transaction after Rollback', FManager.InTransaction);
end;

{ TTestManagerLoadSQL }

procedure TTestManagerLoadSQL.DoLoadSQLNoState;
begin
  FManager.LoadSQL(FObj, 'SELECT ID, Name FROM Test WHERE ID = :ID', ['ID', 1]);
end;

procedure TTestManagerLoadSQL.DoLoadSQLNoPersister;
begin
  FManager.LoadSQL(FObj, 'SELECT ID, Name FROM Test WHERE ID = :ID', ['ID', 1]);
end;

procedure TTestManagerLoadSQL.DoLoadSQLNoConnection;
begin
  FManager.LoadSQL(FObj, 'SELECT ID, Name FROM Test WHERE ID = :ID', ['ID', 1]);
end;

procedure TTestManagerLoadSQL.DoLoadSQLListNilList;
begin
  FManager.LoadSQL(TObject(nil), TTestSaveContact, 'SELECT ID, Name FROM Test');
end;

procedure TTestManagerLoadSQL.SetUp;
begin
  inherited;
  FManager:=TOPManager.Create(Nil);
  FObj:=Nil;
  TOPRegistry.ClearInstance;
  TOPListRegistry.ClearInstance;
end;

procedure TTestManagerLoadSQL.TearDown;
begin
  FreeAndNil(FObj);
  TOPListRegistry.ClearInstance;
  TOPRegistry.ClearInstance;
  FreeAndNil(FManager);
  inherited;
end;

procedure TTestManagerLoadSQL.TestLoadSQLNoStateSupport;
begin
  FObj:=TTestNoState.Create;
  AssertException('No IOPPersistenceState', EOPF, @DoLoadSQLNoState);
end;

procedure TTestManagerLoadSQL.TestLoadSQLNoPersister;
begin
  FObj:=TTestSaveContact.Create;
  AssertException('No persister registered', EOPF, @DoLoadSQLNoPersister);
end;

procedure TTestManagerLoadSQL.TestLoadSQLNoDefaultConnection;
begin
  OPRegistry.RegisterPersister(TTestSaveContact, TRTTIPersister);
  FObj:=TTestSaveContact.Create;
  AssertException('No default connection', EOPF, @DoLoadSQLNoConnection);
end;

procedure TTestManagerLoadSQL.TestLoadSQLListNilList;
begin
  AssertException('Nil list', EOPF, @DoLoadSQLListNilList);
end;

procedure TTestManagerLoadSQL.TestLoadSQLListNilClass;
var
  List: TObjectList;
begin
  List:=TObjectList.Create(True);
  try
    try
      FManager.LoadSQL(List, TClass(nil), 'SELECT ID, Name FROM Test');
      Fail('Expected EOPF');
    except
      on E: EOPF do
        AssertTrue('Message contains reason', Pos('Object class is nil', E.Message) > 0);
    end;
  finally
    List.Free;
  end;
end;

{ TTestRegistryErrorDiagnostics }

procedure TTestRegistryErrorDiagnostics.TearDown;
begin
  TOPRegistry.ClearInstance;
  inherited;
end;

procedure TTestRegistryErrorDiagnostics.TestDuplicatePersisterMessage;
begin
  OPRegistry.RegisterPersister(TTestSaveContact, TRTTIPersister);
  try
    OPRegistry.RegisterPersister(TTestSaveContact, TRTTIPersister);
    Fail('Expected EOPFRegistry');
  except
    on E: EOPFRegistry do
    begin
      AssertTrue('Message contains reason', Pos('Duplicate persister', E.Message) > 0);
      AssertTrue('Message contains class name', Pos('TTestSaveContact', E.Message) > 0);
    end;
  end;
end;

procedure TTestRegistryErrorDiagnostics.TestNilClassRegistrationMessage;
begin
  try
    OPRegistry.RegisterPersister(Nil, TRTTIPersister);
    Fail('Expected EOPFRegistry');
  except
    on E: EOPFRegistry do
    begin
      AssertTrue('Message contains reason', Pos('InstanceClass may not be Nil', E.Message) > 0);
    end;
  end;
end;

{ TTestOPConnection }

procedure TTestOPConnection.SetUp;
begin
  inherited SetUp;
  FList:=TOPConnectionList.Create(TOPConnection);
end;

procedure TTestOPConnection.TearDown;
begin
  FreeAndNil(FList);
  inherited TearDown;
end;

procedure TTestOPConnection.TestIsDefaultSingle;
var
  C: TOPConnection;
begin
  C:=List.Add as TOPConnection;
  C.Name:='DB1';
  AssertFalse('Not default initially', C.IsDefault);
  C.IsDefault:=True;
  AssertTrue('Default after set', C.IsDefault);
end;

procedure TTestOPConnection.TestIsDefaultMutualExclusivity;
var
  A, B: TOPConnection;
begin
  A:=List.Add as TOPConnection;
  A.Name:='DB_A';
  B:=List.Add as TOPConnection;
  B.Name:='DB_B';
  A.IsDefault:=True;
  AssertTrue('A is default', A.IsDefault);
  AssertFalse('B not default', B.IsDefault);
  B.IsDefault:=True;
  AssertFalse('A no longer default', A.IsDefault);
  AssertTrue('B is now default', B.IsDefault);
end;

procedure TTestOPConnection.TestIsDefaultClearAll;
var
  A, B: TOPConnection;
begin
  A:=List.Add as TOPConnection;
  A.Name:='DB_A';
  B:=List.Add as TOPConnection;
  B.Name:='DB_B';
  A.IsDefault:=True;
  AssertTrue('A is default', A.IsDefault);
  A.IsDefault:=False;
  AssertFalse('A cleared', A.IsDefault);
  AssertFalse('B still not default', B.IsDefault);
end;

procedure TTestOPConnection.TestGetDefaultConnection;
var
  M: TOPManager;
  ConnA, ConnB: TOPConnection;
  SqlA, SqlB: TSQLite3Connection;
begin
  TOPManager.ClearInstance;
  M:=TOPManager.Instance;
  try
    SqlA:=TSQLite3Connection.Create(M);
    SqlA.DatabaseName:=':memory:';
    SqlA.Open;
    SqlB:=TSQLite3Connection.Create(M);
    SqlB.DatabaseName:=':memory:';
    SqlB.Open;
    ConnA:=M.Connections.Add as TOPConnection;
    ConnA.Name:='A';
    ConnA.SingleConnection:=SqlA;
    ConnB:=M.Connections.Add as TOPConnection;
    ConnB.Name:='B';
    ConnB.SingleConnection:=SqlB;
    ConnA.IsDefault:=True;
    // Verify BeginTransaction creates handler (lazy — no SQL transaction yet)
    M.BeginTransaction;
    AssertTrue('Manager reports in transaction', M.InTransaction);
    M.Rollback;
    AssertFalse('Manager reports not in transaction after rollback', M.InTransaction);
  finally
    TOPManager.ClearInstance;
  end;
end;

procedure TTestOPConnection.TestSwitchDefaultConnection;
var
  M: TOPManager;
  ConnA, ConnB: TOPConnection;
  SqlA, SqlB: TSQLite3Connection;
begin
  TOPManager.ClearInstance;
  M:=TOPManager.Instance;
  try
    SqlA:=TSQLite3Connection.Create(M);
    SqlA.DatabaseName:=':memory:';
    SqlA.Open;
    SqlB:=TSQLite3Connection.Create(M);
    SqlB.DatabaseName:=':memory:';
    SqlB.Open;
    ConnA:=M.Connections.Add as TOPConnection;
    ConnA.Name:='A';
    ConnA.SingleConnection:=SqlA;
    ConnB:=M.Connections.Add as TOPConnection;
    ConnB.Name:='B';
    ConnB.SingleConnection:=SqlB;
    ConnA.IsDefault:=True;
    // Begin on A, verify handler is created (lazy — no SQL transaction yet)
    M.BeginTransaction;
    AssertTrue('Manager in transaction', M.InTransaction);
    M.Rollback;
    // Switch default to B
    ConnB.IsDefault:=True;
    AssertFalse('A no longer default', ConnA.IsDefault);
    M.BeginTransaction;
    AssertTrue('Manager in transaction on B', M.InTransaction);
    M.Rollback;
  finally
    TOPManager.ClearInstance;
  end;
end;

procedure TTestOPConnection.TestSaveLoadStreamWithDefault;
var
  SrcList, DstList: TOPConnectionList;
  Stream: TMemoryStream;
  A, B: TOPConnection;
begin
  SrcList:=TOPConnectionList.Create(TOPConnection);
  DstList:=TOPConnectionList.Create(TOPConnection);
  Stream:=TMemoryStream.Create;
  try
    A:=SrcList.Add as TOPConnection;
    A.Name:='ConnA';
    A.ConnectionType:='sqlite';
    A.IsDefault:=False;
    B:=SrcList.Add as TOPConnection;
    B.Name:='ConnB';
    B.ConnectionType:='pgsql';
    B.IsDefault:=True;
    SrcList.SaveToStream(Stream);
    Stream.Position:=0;
    DstList.LoadFromStream(Stream);
    AssertEquals('Count', 2, DstList.Count);
    AssertEquals('Name 0', 'ConnA', (DstList[0] as TOPConnection).Name);
    AssertEquals('Name 1', 'ConnB', (DstList[1] as TOPConnection).Name);
    AssertFalse('A not default', (DstList[0] as TOPConnection).IsDefault);
    AssertTrue('B is default', (DstList[1] as TOPConnection).IsDefault);
  finally
    Stream.Free;
    DstList.Free;
    SrcList.Free;
  end;
end;

procedure TTestOPConnection.TestSaveLoadStreamNoDefault;
var
  SrcList, DstList: TOPConnectionList;
  Stream: TMemoryStream;
  A, B: TOPConnection;
begin
  SrcList:=TOPConnectionList.Create(TOPConnection);
  DstList:=TOPConnectionList.Create(TOPConnection);
  Stream:=TMemoryStream.Create;
  try
    A:=SrcList.Add as TOPConnection;
    A.Name:='ConnA';
    A.ConnectionType:='sqlite';
    B:=SrcList.Add as TOPConnection;
    B.Name:='ConnB';
    B.ConnectionType:='pgsql';
    SrcList.SaveToStream(Stream);
    Stream.Position:=0;
    DstList.LoadFromStream(Stream);
    AssertEquals('Count', 2, DstList.Count);
    AssertFalse('A not default', (DstList[0] as TOPConnection).IsDefault);
    AssertFalse('B not default', (DstList[1] as TOPConnection).IsDefault);
  finally
    Stream.Free;
    DstList.Free;
    SrcList.Free;
  end;
end;

{ TTestHookManager }

procedure TTestHookManager.DoBeforeSave(aObject: TObject; aState: TObjectPersistenceState);
begin
  AddCall('DoBeforeSave', aObject, aState);
  inherited DoBeforeSave(aObject, aState);
end;

procedure TTestHookManager.DoAfterSave(aObject: TObject; aState: TObjectPersistenceState);
begin
  AddCall('DoAfterSave', aObject, aState);
  inherited DoAfterSave(aObject, aState);
end;

procedure TTestHookManager.DoAfterLoad(aObject: TObject);
begin
  AddCall('DoAfterLoad', aObject, opsClean);
  inherited DoAfterLoad(aObject);
end;

procedure TTestHookManager.DoBeforeDelete(aObject: TObject);
begin
  AddCall('DoBeforeDelete', aObject, opsDelete);
  inherited DoBeforeDelete(aObject);
end;

procedure TTestHookManager.DoAfterDelete(aObject: TObject);
begin
  AddCall('DoAfterDelete', aObject, opsDeleted);
  inherited DoAfterDelete(aObject);
end;

procedure TTestHookManager.ClearCalls;
begin
  SetLength(Calls, 0);
end;

procedure TTestHookManager.AddCall(const aName: UTF8String; aObj: TObject; aState: TObjectPersistenceState);
var
  L: Integer;
begin
  L:=Length(Calls);
  SetLength(Calls, L + 1);
  Calls[L].Name:=aName;
  Calls[L].Obj:=aObj;
  Calls[L].State:=aState;
end;

{ TTestManagerHooks }

procedure TTestManagerHooks.AddEventCall(const aName: UTF8String; aObj: TObject; aState: TObjectPersistenceState);
var
  L: Integer;
begin
  L:=Length(FEventCalls);
  SetLength(FEventCalls, L + 1);
  FEventCalls[L].Name:=aName;
  FEventCalls[L].Obj:=aObj;
  FEventCalls[L].State:=aState;
end;

procedure TTestManagerHooks.HandleBeforeSave(Sender: TObject; aObject: TObject; aState: TObjectPersistenceState);
begin
  AddEventCall('BeforeSave', aObject, aState);
end;

procedure TTestManagerHooks.HandleAfterSave(Sender: TObject; aObject: TObject; aState: TObjectPersistenceState);
begin
  AddEventCall('AfterSave', aObject, aState);
end;

procedure TTestManagerHooks.HandleAfterLoad(Sender: TObject; aObject: TObject);
begin
  AddEventCall('AfterLoad', aObject, opsClean);
end;

procedure TTestManagerHooks.HandleBeforeDelete(Sender: TObject; aObject: TObject);
begin
  AddEventCall('BeforeDelete', aObject, opsDelete);
end;

procedure TTestManagerHooks.HandleAfterDelete(Sender: TObject; aObject: TObject);
begin
  AddEventCall('AfterDelete', aObject, opsDeleted);
end;

procedure TTestManagerHooks.ClearEventCalls;
begin
  SetLength(FEventCalls, 0);
end;

procedure TTestManagerHooks.SetUp;
begin
  inherited SetUp;
  FManager:=TTestHookManager.Create(Nil);
  FObj:=TTestSaveContact.Create;
  ClearEventCalls;
end;

procedure TTestManagerHooks.TearDown;
begin
  FreeAndNil(FObj);
  FreeAndNil(FManager);
  inherited TearDown;
end;

procedure TTestManagerHooks.TestDoBeforeSaveCallsEvent;
begin
  FManager.BeforeSave:=@HandleBeforeSave;
  FManager.DoBeforeSave(FObj, opsNew);
  AssertEquals('Event call count', 1, Length(FEventCalls));
  AssertEquals('Event name', 'BeforeSave', FEventCalls[0].Name);
end;

procedure TTestManagerHooks.TestDoAfterSaveCallsEvent;
begin
  FManager.AfterSave:=@HandleAfterSave;
  FManager.DoAfterSave(FObj, opsUpdate);
  AssertEquals('Event call count', 1, Length(FEventCalls));
  AssertEquals('Event name', 'AfterSave', FEventCalls[0].Name);
end;

procedure TTestManagerHooks.TestDoAfterLoadCallsEvent;
begin
  FManager.AfterLoad:=@HandleAfterLoad;
  FManager.DoAfterLoad(FObj);
  AssertEquals('Event call count', 1, Length(FEventCalls));
  AssertEquals('Event name', 'AfterLoad', FEventCalls[0].Name);
end;

procedure TTestManagerHooks.TestDoBeforeDeleteCallsEvent;
begin
  FManager.BeforeDelete:=@HandleBeforeDelete;
  FManager.DoBeforeDelete(FObj);
  AssertEquals('Event call count', 1, Length(FEventCalls));
  AssertEquals('Event name', 'BeforeDelete', FEventCalls[0].Name);
end;

procedure TTestManagerHooks.TestDoAfterDeleteCallsEvent;
begin
  FManager.AfterDelete:=@HandleAfterDelete;
  FManager.DoAfterDelete(FObj);
  AssertEquals('Event call count', 1, Length(FEventCalls));
  AssertEquals('Event name', 'AfterDelete', FEventCalls[0].Name);
end;

procedure TTestManagerHooks.TestDoBeforeSaveNoEvent;
begin
  FManager.DoBeforeSave(FObj, opsNew);
  AssertEquals('No event calls', 0, Length(FEventCalls));
end;

procedure TTestManagerHooks.TestDoAfterLoadNoEvent;
begin
  FManager.DoAfterLoad(FObj);
  AssertEquals('No event calls', 0, Length(FEventCalls));
end;

procedure TTestManagerHooks.TestBeforeSaveEventReceivesOpsNew;
begin
  FManager.BeforeSave:=@HandleBeforeSave;
  FManager.DoBeforeSave(FObj, opsNew);
  AssertEquals('State is opsNew', Ord(opsNew), Ord(FEventCalls[0].State));
  AssertTrue('Object matches', FEventCalls[0].Obj = FObj);
end;

procedure TTestManagerHooks.TestBeforeSaveEventReceivesOpsUpdate;
begin
  FManager.BeforeSave:=@HandleBeforeSave;
  FManager.DoBeforeSave(FObj, opsUpdate);
  AssertEquals('State is opsUpdate', Ord(opsUpdate), Ord(FEventCalls[0].State));
  AssertTrue('Object matches', FEventCalls[0].Obj = FObj);
end;

procedure TTestManagerHooks.TestAfterSaveEventReceivesState;
begin
  FManager.AfterSave:=@HandleAfterSave;
  FManager.DoAfterSave(FObj, opsUpdate);
  AssertEquals('State is opsUpdate', Ord(opsUpdate), Ord(FEventCalls[0].State));
  AssertTrue('Object matches', FEventCalls[0].Obj = FObj);
end;

procedure TTestManagerHooks.TestAfterLoadEventReceivesObject;
begin
  FManager.AfterLoad:=@HandleAfterLoad;
  FManager.DoAfterLoad(FObj);
  AssertTrue('Object matches', FEventCalls[0].Obj = FObj);
end;

procedure TTestManagerHooks.TestBeforeDeleteEventReceivesObject;
begin
  FManager.BeforeDelete:=@HandleBeforeDelete;
  FManager.DoBeforeDelete(FObj);
  AssertTrue('Object matches', FEventCalls[0].Obj = FObj);
end;

procedure TTestManagerHooks.TestAfterDeleteEventReceivesObject;
begin
  FManager.AfterDelete:=@HandleAfterDelete;
  FManager.DoAfterDelete(FObj);
  AssertTrue('Object matches', FEventCalls[0].Obj = FObj);
end;

procedure TTestManagerHooks.TestOverrideRecordsCalls;
begin
  // No events assigned — just test that the override fires
  FManager.ClearCalls;
  FManager.DoBeforeSave(FObj, opsNew);
  FManager.DoAfterSave(FObj, opsNew);
  FManager.DoAfterLoad(FObj);
  FManager.DoBeforeDelete(FObj);
  FManager.DoAfterDelete(FObj);
  AssertEquals('Override call count', 5, Length(FManager.Calls));
  AssertEquals('Call 0', 'DoBeforeSave', FManager.Calls[0].Name);
  AssertEquals('Call 1', 'DoAfterSave', FManager.Calls[1].Name);
  AssertEquals('Call 2', 'DoAfterLoad', FManager.Calls[2].Name);
  AssertEquals('Call 3', 'DoBeforeDelete', FManager.Calls[3].Name);
  AssertEquals('Call 4', 'DoAfterDelete', FManager.Calls[4].Name);
end;

{ TTestAuditManager }

procedure TTestAuditManager.TestApplyAuditFields(aObject: TObject; aState: TObjectPersistenceState; aFieldMap: TOPFieldMapList);
begin
  ApplyAuditFields(aObject, aState, aFieldMap);
end;

{ TTestAuditContact }

function TTestAuditContact.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TTestAuditContact.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

{ TTestManagerAudit }

function TTestManagerAudit.AddField(const aPropName: String; aOptions: TOPFieldOptions): TOPFieldMap;
begin
  Result := FFieldMap.Add as TOPFieldMap;
  Result.PropertyName := aPropName;
  Result.FieldName := aPropName;
  Result.Options := aOptions;
  Result.PropertyTypeInfo := TTestAuditContact.ClassInfo;
  // Set PropertyKind based on the property name
  if (Pos('At', aPropName) > 0) then
    Result.PropertyKind := tkFloat
  else if (Pos('Int', aPropName) > 0) then
    Result.PropertyKind := tkInteger
  else
    Result.PropertyKind := tkAString;
end;

procedure TTestManagerAudit.DoApplyAudit(aState: TObjectPersistenceState);
begin
  FManager.TestApplyAuditFields(FObj, aState, FFieldMap);
end;

procedure TTestManagerAudit.DoApplyAuditCreatedByNoUser;
begin
  DoApplyAudit(opsNew);
end;

procedure TTestManagerAudit.SetUp;
begin
  inherited SetUp;
  FManager := TTestAuditManager.Create(Nil);
  FObj := TTestAuditContact.Create;
  FFieldMap := TOPFieldMapList.Create(TOPFieldMap);
end;

procedure TTestManagerAudit.TearDown;
begin
  FreeAndNil(FFieldMap);
  FreeAndNil(FObj);
  FreeAndNil(FManager);
  inherited TearDown;
end;

procedure TTestManagerAudit.TestCreatedAtOnInsert;
var
  Before, After: TDateTime;
begin
  AddField('CreatedAt', [foCreatedAt]);
  Before := Now;
  DoApplyAudit(opsNew);
  After := Now;
  AssertTrue('CreatedAt set', FObj.CreatedAt >= Before);
  AssertTrue('CreatedAt not future', FObj.CreatedAt <= After);
end;

procedure TTestManagerAudit.TestCreatedAtNotOnUpdate;
begin
  AddField('CreatedAt', [foCreatedAt]);
  FObj.CreatedAt := 0;
  DoApplyAudit(opsUpdate);
  AssertEquals('CreatedAt unchanged', 0.0, FObj.CreatedAt);
end;

procedure TTestManagerAudit.TestUpdatedAtOnUpdate;
var
  Before, After: TDateTime;
begin
  AddField('UpdatedAt', [foUpdatedAt]);
  Before := Now;
  DoApplyAudit(opsUpdate);
  After := Now;
  AssertTrue('UpdatedAt set', FObj.UpdatedAt >= Before);
  AssertTrue('UpdatedAt not future', FObj.UpdatedAt <= After);
end;

procedure TTestManagerAudit.TestUpdatedAtOnInsertWithRequired;
var
  Before, After: TDateTime;
begin
  AddField('UpdatedAt', [foUpdatedAt, foRequired]);
  Before := Now;
  DoApplyAudit(opsNew);
  After := Now;
  AssertTrue('UpdatedAt set on insert with required', FObj.UpdatedAt >= Before);
  AssertTrue('UpdatedAt not future', FObj.UpdatedAt <= After);
end;

procedure TTestManagerAudit.TestUpdatedAtOnInsertWithoutRequired;
begin
  AddField('UpdatedAt', [foUpdatedAt]);
  FObj.UpdatedAt := 0;
  DoApplyAudit(opsNew);
  AssertEquals('UpdatedAt unchanged', 0.0, FObj.UpdatedAt);
end;

procedure TTestManagerAudit.TestTimestampUTC;
var
  LocalNow, UTCResult: TDateTime;
begin
  AddField('CreatedAt', [foCreatedAt]);
  FManager.AuditInfo.TimestampKind := tkUTC;
  LocalNow := Now;
  DoApplyAudit(opsNew);
  UTCResult := FObj.CreatedAt;
  // UTC should be <= local time (or equal in UTC+0 zones)
  // Just verify it was set to something reasonable
  AssertTrue('UTC timestamp set', UTCResult > 0);
  // In most zones, UTC <= local, but just check it's within 24h of Now
  AssertTrue('UTC within range', Abs(UTCResult - LocalNow) < 1.0);
end;

procedure TTestManagerAudit.TestCreatedByStringOnInsert;
begin
  AddField('CreatedBy', [foCreatedBy]);
  FManager.AuditInfo.StringID := 'admin';
  DoApplyAudit(opsNew);
  AssertEquals('CreatedBy set', 'admin', FObj.CreatedBy);
end;

procedure TTestManagerAudit.TestCreatedByNumericalOnInsert;
begin
  AddField('CreatedByInt', [foCreatedBy]);
  FManager.AuditInfo.NumericalID := 42;
  DoApplyAudit(opsNew);
  AssertEquals('CreatedByInt set', 42, FObj.CreatedByInt);
end;

procedure TTestManagerAudit.TestCreatedByObjectKeyAfterFree;
var
  R: TOPRegistry;
  U: TTestAuditContact;
begin
  R:=TOPRegistry.Create(Nil);
  try
    R.RegisterPersister(TTestAuditContact, TRTTIPersister);
    FManager.Registry:=R;
    AddField('CreatedByInt', [foCreatedBy]);
    U:=TTestAuditContact.Create;
    U.ID:=5;
    FManager.AuditInfo.ObjectID:=U;
    // Only the key is kept: the user object may go
    U.Free;
    DoApplyAudit(opsNew);
    AssertEquals('CreatedByInt from the key', 5, FObj.CreatedByInt);
  finally
    FManager.Registry:=Nil;
    R.Free;
  end;
end;

procedure TTestManagerAudit.TestCreatedByNumericalToStringConversion;
begin
  AddField('CreatedBy', [foCreatedBy]);
  FManager.AuditInfo.NumericalID := 99;
  DoApplyAudit(opsNew);
  AssertEquals('CreatedBy converted from int', '99', FObj.CreatedBy);
end;

procedure TTestManagerAudit.TestCreatedByNotOnUpdate;
begin
  AddField('CreatedBy', [foCreatedBy]);
  FManager.AuditInfo.StringID := 'admin';
  FObj.CreatedBy := 'original';
  DoApplyAudit(opsUpdate);
  AssertEquals('CreatedBy unchanged', 'original', FObj.CreatedBy);
end;

procedure TTestManagerAudit.TestUpdatedByOnUpdate;
begin
  AddField('UpdatedBy', [foUpdatedBy]);
  FManager.AuditInfo.StringID := 'editor';
  DoApplyAudit(opsUpdate);
  AssertEquals('UpdatedBy set', 'editor', FObj.UpdatedBy);
end;

procedure TTestManagerAudit.TestUpdatedByOnInsertWithRequired;
begin
  AddField('UpdatedBy', [foUpdatedBy, foRequired]);
  FManager.AuditInfo.StringID := 'creator';
  DoApplyAudit(opsNew);
  AssertEquals('UpdatedBy set on insert with required', 'creator', FObj.UpdatedBy);
end;

procedure TTestManagerAudit.TestUpdatedByOnInsertWithoutRequired;
begin
  AddField('UpdatedBy', [foUpdatedBy]);
  FManager.AuditInfo.StringID := 'editor';
  FObj.UpdatedBy := '';
  DoApplyAudit(opsNew);
  AssertEquals('UpdatedBy unchanged', '', FObj.UpdatedBy);
end;

procedure TTestManagerAudit.TestCreatedByRaisesWhenUserNotSet;
begin
  AddField('CreatedBy', [foCreatedBy]);
  // UserIDKind is ukNone by default
  AssertException('Raises on no user', EOPF, @DoApplyAuditCreatedByNoUser);
end;

procedure TTestManagerAudit.TestNoAuditFieldsNoOp;
begin
  AddField('Name', [foInSelect, foInInsert]);
  FObj.Name := 'test';
  FObj.CreatedAt := 0;
  DoApplyAudit(opsNew);
  AssertEquals('Name unchanged', 'test', FObj.Name);
  AssertEquals('CreatedAt unchanged', 0.0, FObj.CreatedAt);
end;

{ TTestSoftDeleteManager }

function TTestSoftDeleteManager.TestGetSoftDeleteFilter(aFieldMap: TOPFieldMapList; const aTablePrefix: UTF8String): UTF8String;
begin
  Result := GetSoftDeleteFilter(aFieldMap, aTablePrefix);
end;

{ TTestManagerSoftDelete }

function TTestManagerSoftDelete.AddField(const aPropName: String; aKind: TTypeKind; aOptions: TOPFieldOptions): TOPFieldMap;
begin
  Result := FFieldMap.Add as TOPFieldMap;
  Result.PropertyName := aPropName;
  Result.FieldName := aPropName;
  Result.PropertyKind := aKind;
  Result.Options := aOptions;
end;

procedure TTestManagerSoftDelete.DoAddDuplicateSoftDelete;
begin
  AddField('Deleted1', tkBool, [foSoftDelete]);
  AddField('Deleted2', tkBool, [foSoftDelete]);
end;

procedure TTestManagerSoftDelete.SetUp;
begin
  inherited SetUp;
  FManager := TTestSoftDeleteManager.Create(Nil);
  FFieldMap := TOPFieldMapList.Create(TOPFieldMap);
  // Ensure SQLBuilder has a dialect set for boolean keyword resolution
  OPSQLBuilder.SQLDialect := dSQLite;
end;

procedure TTestManagerSoftDelete.TearDown;
begin
  FreeAndNil(FFieldMap);
  FreeAndNil(FManager);
  inherited TearDown;
end;

procedure TTestManagerSoftDelete.TestFilterBoolRequired;
var
  Filter: UTF8String;
begin
  AddField('ID', tkInteger, [foInKey]);
  AddField('Deleted', tkBool, [foSoftDelete, foRequired]);
  Filter := FManager.TestGetSoftDeleteFilter(FFieldMap);
  // SQLite uses BOOLEAN dialect → FALSE keyword
  AssertEquals('Bool required filter', 'Deleted = FALSE', Filter);
end;

procedure TTestManagerSoftDelete.TestFilterBoolNullable;
var
  Filter: UTF8String;
begin
  AddField('ID', tkInteger, [foInKey]);
  AddField('Deleted', tkBool, [foSoftDelete]);
  Filter := FManager.TestGetSoftDeleteFilter(FFieldMap);
  AssertEquals('Bool nullable filter', '(Deleted IS NULL OR Deleted = FALSE)', Filter);
end;

procedure TTestManagerSoftDelete.TestFilterDateTime;
var
  Filter: UTF8String;
begin
  AddField('ID', tkInteger, [foInKey]);
  AddField('DeletedAt', tkFloat, [foSoftDelete]);
  Filter := FManager.TestGetSoftDeleteFilter(FFieldMap);
  AssertEquals('DateTime filter', 'DeletedAt IS NULL', Filter);
end;

procedure TTestManagerSoftDelete.TestFilterNoSoftDelete;
var
  Filter: UTF8String;
begin
  AddField('ID', tkInteger, [foInKey]);
  AddField('Name', tkAString, [foInSelect]);
  Filter := FManager.TestGetSoftDeleteFilter(FFieldMap);
  AssertEquals('No soft-delete field', '', Filter);
end;

procedure TTestManagerSoftDelete.TestFilterWithTablePrefix;
var
  Filter: UTF8String;
begin
  AddField('ID', tkInteger, [foInKey]);
  AddField('DeletedAt', tkFloat, [foSoftDelete]);
  Filter := FManager.TestGetSoftDeleteFilter(FFieldMap, 'MyTable');
  AssertEquals('Table-prefixed filter', 'MyTable.DeletedAt IS NULL', Filter);
end;

procedure TTestManagerSoftDelete.TestDuplicateSoftDeleteRaises;
begin
  AssertException('Duplicate foSoftDelete', EFieldMap, @DoAddDuplicateSoftDelete);
end;

initialization
  RegisterTest(TTestManager);
  RegisterTest(TTestManagerSave);
  RegisterTest(TTestManagerDelete);
  RegisterTest(TTestManagerLoad);
  RegisterTest(TTestManagerErrorDiagnostics);
  RegisterTest(TTestManagerLoadList);
  RegisterTest(TTestManagerTransaction);
  RegisterTest(TTestManagerLoadSQL);
  RegisterTest(TTestRegistryErrorDiagnostics);
  RegisterTest(TTestOPConnection);
  RegisterTest(TTestManagerHooks);
  RegisterTest(TTestManagerAudit);
  RegisterTest(TTestManagerSoftDelete);
end.
