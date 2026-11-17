unit utc.opf.integration.common;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Data.Db, Data.Sqldb,
  FpcUnit.Test,
{$ELSE}
  Classes, SysUtils, db, sqldb,
  fpcunit,
{$ENDIF}
  contnrs, typinfo, Variants,
  Opf.Types, Opf.Fields, Opf.Driver, Opf.Sql, Opf.DDL, Opf.Executor,
  Opf.Transaction, Opf.Manager,
  Opf.Persister.Intf, Opf.Persister.RTTI,
  Opf.Nullable,
  Opf.M2M,
  Opf.Proxy,
  utc.opf.executor,
  utc.opf.integration.base;

Type

  { ===== Fixture / Domain classes ===== }

  { TTestMgrSaveContact }

  TTestMgrSaveContact = class(TPersistent, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    FState: TObjectPersistenceState;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  { TTestCascadeParent }

  TTestCascadeParent = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    FState: TObjectPersistenceState;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  { TTestCascadeChild }

  TTestCascadeChild = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FParentID: Integer;
    FVal: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property ParentID: Integer read FParentID write FParentID;
    property Val: String read FVal write FVal;
  end;

  { TTestCascadeGrandchild }

  TTestCascadeGrandchild = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FChildID: Integer;
    FData: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property ChildID: Integer read FChildID write FChildID;
    property Data: String read FData write FData;
  end;

  { TTestMappedContact }

  TTestMappedContact = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FFullName: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property FullName: String read FFullName write FFullName;
  end;

  { TTestJsonAddress }

  TTestJsonAddress = class(TPersistent)
  private
    FStreet: String;
    FCity: String;
  published
    property Street: String read FStreet write FStreet;
    property City: String read FCity write FCity;
  end;

  { TTestJsonContact }

  TTestJsonContact = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FAddress: TTestJsonAddress;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Address: TTestJsonAddress read FAddress write FAddress;
  end;

  { TTestAutoIncContact }

  TTestAutoIncContact = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    FState: TObjectPersistenceState;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  { TTestUUIDContact }

  TTestUUIDContact = class(TObject, IOPPersistenceState)
  private
    FID: String;
    FName: String;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    FState: TObjectPersistenceState;
  published
    property ID: String read FID write FID;
    property Name: String read FName write FName;
  end;

  { TTestCustomGenContact }

  TTestCustomGenContact = class(TObject, IOPPersistenceState)
  private
    FID: String;
    FName: String;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    FState: TObjectPersistenceState;
  published
    property ID: String read FID write FID;
    property Name: String read FName write FName;
  end;

  { TTestContactWithChildren }

  TTestContactWithChildren = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FState: TObjectPersistenceState;
    FAddressList: TObjectList;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property AddressList: TObjectList read FAddressList;
  end;

  { TTestAddress }

  TTestAddress = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FContactID: Integer;
    FStreet: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property ContactID: Integer read FContactID write FContactID;
    property Street: String read FStreet write FStreet;
  end;

  { TTestIntfProduct }

  TTestIntfProduct = class(TObject, IOPFieldProvider, IOPPersistenceState)
  private
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    FID: Integer;
    FName: UTF8String;
    FPrice: Double;
    FState: TObjectPersistenceState;
    procedure GetFieldDefs(aMap: TOPFieldMapList);
    function ApplyField(aFieldMap: TOPFieldMap; aField: TField): Boolean;
    function PersistField(aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
    property ID: Integer read FID write FID;
    property Name: UTF8String read FName write FName;
    property Price: Double read FPrice write FPrice;
  end;

  { TTestExtendedTypeContact }

  TExtColor = (ecRed, ecGreen, ecBlue);
  TExtFlags = set of (efBold, efItalic, efUnderline);

  TTestExtendedTypeContact = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FColor: TExtColor;
    FFlags: TExtFlags;
    FExtra: Variant;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    FState: TObjectPersistenceState;
    constructor Create;
  published
    property ID: Integer read FID write FID;
    property Color: TExtColor read FColor write FColor;
    property Flags: TExtFlags read FFlags write FFlags;
    property Extra: Variant read FExtra write FExtra;
  end;

  { TTestNullableIntContact }

  TTestNullableIntContact = class(TPersistent)
  private
    FID: Integer;
    FName: String;
    FAge: Integer;
    FAgeStored: Boolean;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge stored FAgeStored;
  end;

  { TTestIntNullableObj }

  TTestIntNullableObj = class(TOPFNullableObject)
  private
    FID: Integer;
    FName: String;
    FAge: Integer;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

  { TTestSequenceContact }

  TTestSequenceContact = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    FState: TObjectPersistenceState;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  { TTestM2MStudent }

  TTestM2MStudent = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FCourseList: TObjectList;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property CourseList: TObjectList read FCourseList;
  end;

  { TTestM2MCourse }

  TTestM2MCourse = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FTitle: String;
    FStudentList: TObjectList;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ID: Integer read FID write FID;
    property Title: String read FTitle write FTitle;
    property StudentList: TObjectList read FStudentList;
  end;

  { TTestM2MClub }

  TTestM2MClub = class(TObject, IOPPersistenceState)
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

  { ===== Abstract Test Base Classes ===== }

  { TTestIntegrationCommon — basic CRUD via Executor }

  TTestIntegrationCommon = class(TTestIntegration)
  protected
    FFieldMap: TOPFieldMapList;
    FPersister: TTestPersister;
    FObj: TTestObject;
    FExecutor: TOPExecutor;
    function CountRows: Integer;
    procedure DirectInsert(aID: Integer; const aName: String; aAge: Integer);
    procedure DirectSelect(aID: Integer; out aName: String; out aAge: Integer);
  protected
    procedure CreateTestTable; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInsert;
    procedure TestSelect;
    procedure TestUpdate;
    procedure TestDelete;
    procedure TestTransactionImplicit;
    procedure TestTransactionRollback;
  end;

  { TTestManagerSaveCommon }

  TTestManagerSaveCommon = class(TTestIntegration)
  protected
    function CountContactRows: Integer;
    procedure DirectSelectContact(aID: Integer; out aName: String);
    procedure DirectInsertContact(aID: Integer; const aName: String);
  protected
    procedure CreateContactTable; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSaveInsert;
    procedure TestSaveUpdate;
    procedure TestSaveUpdateOnlyTargetRow;
  end;

  { TTestManagerDeleteCommon }

  TTestManagerDeleteCommon = class(TTestIntegration)
  protected
    function CountContactRows: Integer;
    procedure DirectSelectContact(aID: Integer; out aName: String);
    procedure DirectInsertContact(aID: Integer; const aName: String);
  protected
    procedure CreateContactTable; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDeleteObject;
    procedure TestDeleteOnlyTargetRow;
  end;

  { TTestCascadeDeleteCommon }

  TTestCascadeDeleteCommon = class(TTestIntegration)
  protected
    procedure DirectInsertParent(aID: Integer; const aName: String);
    procedure DirectInsertChild(aID, aParentID: Integer; const aValue: String);
    procedure DirectInsertGrandchild(aID, aChildID: Integer; const aData: String);
    function CountRows(const aTable: String): Integer;
  protected
    procedure CreateCascadeTables; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCascadeDeleteParentWithChildren;
    procedure TestCascadeDeleteParentNoChildren;
    procedure TestCascadeDeleteMultipleChildren;
    procedure TestCascadeDeletePreservesOtherParents;
    procedure TestCascadeDeleteNestedRelationships;
    procedure TestCascadeDeleteRollbackOnFailure;
    procedure TestCascadeDeleteDBCascadeSkipsChildDelete;
    procedure TestCascadeDeleteMixedDBAndORMCascade;
  end;

  { TTestManagerLoadCommon }

  TTestManagerLoadCommon = class(TTestIntegration)
  protected
    FObj: TTestMgrSaveContact;
    procedure DirectInsertContact(aID: Integer; const aName: String);
    procedure DoLoadNotFound;
  protected
    procedure CreateLoadTable; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadByPK;
    procedure TestLoadNotFound;
  end;

  { TTestManagerLoadListCommon }

  TTestManagerLoadListCommon = class(TTestIntegration)
  protected
    procedure DirectInsertContact(aID: Integer; const aName: String);
  protected
    procedure CreateContactTable; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadListMultipleRows;
    procedure TestLoadListEmptyResult;
    procedure TestLoadListWithCriteria;
    procedure TestLoadListOwnership;
  end;

  { TTestMappedCRUDCommon }

  TTestMappedCRUDCommon = class(TTestIntegration)
  protected
    function CountMappedRows: Integer;
    procedure DirectSelectMapped(aID: Integer; out aFullName: String);
    procedure DirectInsertMapped(aID: Integer; const aFullName: String);
  protected
    procedure CreateMappedTable; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSaveMapped;
    procedure TestLoadMapped;
    procedure TestUpdateMapped;
    procedure TestDeleteMapped;
  end;

  { TTestJsonBlobCRUDCommon }

  TTestJsonBlobCRUDCommon = class(TTestIntegration)
  protected
    procedure DirectInsertJsonContact(aID: Integer; const aName, aAddressJSON: String);
    procedure DirectSelectJsonContact(aID: Integer; out aName, aAddress: String);
  protected
    procedure CreateJsonContactTable; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSaveJsonBlob;
    procedure TestLoadJsonBlob;
    procedure TestRoundTripJsonBlob;
    procedure TestSaveNilJsonProperty;
  end;

  { TTestAutoIncCommon }

  TTestAutoIncCommon = class(TTestIntegration)
  protected
    function CountAutoIncRows: Integer;
    procedure DirectSelectAutoInc(aID: Integer; out aName: String);
  protected
    procedure CreateAutoIncTable; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAutoIncInsertPopulatesID;
    procedure TestAutoIncInsertMultipleRows;
    procedure TestAutoIncInsertPreservesOtherFields;
    procedure TestAutoIncUpdateIgnoresAutoInc;
  end;

  { TTestUUIDCommon }

  TTestUUIDCommon = class(TTestIntegration)
  protected
    function CountUUIDRows: Integer;
    procedure DirectSelectUUID(const aID: String; out aName: String);
  protected
    procedure CreateUUIDTable; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUUIDInsertPopulatesID;
    procedure TestUUIDInsertMultipleUnique;
    procedure TestUUIDInsertPreservesOtherFields;
    procedure TestUUIDInsertLoadRoundTrip;
    procedure TestUUIDInsertPreservesPresetID;
  end;

  { TTestCustomGenCommon }

  TTestCustomGenCommon = class(TTestIntegration)
  protected
    FNextID: Integer;
    function CountCustomGenRows: Integer;
    procedure DirectSelectCustomGen(const aID: String; out aName: String);
    procedure GenerateInvoiceID(aInstance: TObject; aField: TOPFieldMap; aConnection: TObject);
  protected
    procedure CreateCustomGenTable; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCustomGenInsertPopulatesID;
    procedure TestCustomGenInsertMultipleSequential;
    procedure TestCustomGenInsertPreservesOtherFields;
    procedure TestCustomGenInsertLoadRoundTrip;
  end;

  { TTestLoadChildrenCommon }

  TTestLoadChildrenCommon = class(TTestIntegration)
  protected
    procedure DirectInsertContact(aID: Integer; const aName: String);
    procedure DirectInsertAddress(aID, aContactID: Integer; const aStreet: String);
    procedure LoadChildrenNoRelations;
    procedure LoadChildrenAmbiguous;
  protected
    procedure CreateLoadChildrenTables; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadChildrenByPropertyName;
    procedure TestLoadChildrenExplicitClassExternalList;
    procedure TestLoadAllChildren;
    procedure TestLoadChildrenRegisteredPropertyName;
    procedure TestLoadChildrenNoRelationsRaises;
    procedure TestLoadChildrenEmptyResult;
  end;

  { TTestInterfacePersisterCommon }

  TTestInterfacePersisterCommon = class(TTestIntegration)
  protected
    function CountProductRows: Integer;
    procedure DirectSelectProduct(aID: Integer; out aName: String; out aPrice: Double);
  protected
    procedure CreateProductTable; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSaveAndLoadInterfacePersister;
    procedure TestUpdateInterfacePersister;
    procedure TestDeleteInterfacePersister;
  end;

  { TTestExplicitTransactionCommon }

  TTestExplicitTransactionCommon = class(TTestIntegration)
  protected
    function CountContactRows: Integer;
    procedure DirectInsertContact(aID: Integer; const aName: String);
    procedure DirectSelectContact(aID: Integer; out aName: String);
  protected
    procedure CreateContactTable; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExplicitCommitPersists;
    procedure TestExplicitRollbackReverts;
    procedure TestSaveAfterRolledBackSaveReachesDatabase;
  end;

  { TTestCustomSQLCommon }

  TTestCustomSQLCommon = class(TTestIntegration)
  protected
    procedure DirectInsertContact(aID: Integer; const aName: String; aDeptID: Integer);
    procedure DirectInsertDept(aID: Integer; const aDeptName: String);
    procedure DoLoadSQLNotFound;
  protected
    procedure CreateCustomSQLTables; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadSQLSingleObject;
    procedure TestLoadSQLWithJoin;
    procedure TestLoadSQLList;
    procedure TestLoadSQLUnmappedColumnsIgnored;
    procedure TestLoadSQLNoParams;
    procedure TestLoadSQLListNoResults;
    procedure TestLoadSQLNotFoundRaises;
    procedure TestLoadSQLWithTParams;
    procedure TestLoadSQLListWithTParams;
    procedure TestLoadSQLListMultipleParams;
  end;

  { TTestExtendedTypesCRUDCommon }

  TTestExtendedTypesCRUDCommon = class(TTestIntegration)
  protected
    procedure CreateExtendedTable; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSaveLoadSetEnum;
    procedure TestSaveLoadVariant;
  end;

  { TTestErrorDiagnosticsCommon }

  TTestErrorDiagnosticsCommon = class(TTestIntegration)
  protected
    FObj: TTestMgrSaveContact;
  protected
    procedure CreateContactTable; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadNotFoundMessage;
    procedure TestSaveUnsupportedStateMessage;
  end;

  { TTestNullableRoundTripCommon }

  TTestNullableRoundTripCommon = class(TTestIntegration)
  protected
    FPersister: TRTTIPersister;
    FMap: TOPFieldMapList;
    function MakeQuery(const aSQL: string): TSQLQuery;
    procedure CreateNullableTable; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNullableRoundTrip;
  end;

  { TTestIOPFNullableRoundTripCommon }

  TTestIOPFNullableRoundTripCommon = class(TTestIntegration)
  protected
    FPersister: TRTTIPersister;
    FMap: TOPFieldMapList;
    function MakeQuery(const aSQL: string): TSQLQuery;
    procedure CreateIOPFNullableTable; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIOPFNullableRoundTrip;
  end;

  { TTestSequenceCommon }

  TTestSequenceCommon = class(TTestIntegration)
  protected
    procedure CreateSequenceTable; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSequenceInsertPopulatesID;
    procedure TestSequenceInsertMultipleSequential;
  end;

  { TTestDDLExecCommon }

  TTestDDLExecCommon = class(TTestIntegration)
  protected
    FDDLBuilder: TOPDDLBuilder;
    FMap: TOPFieldMapList;
  protected
    function GetDDLTableName: String; virtual; abstract;
    function GetManualTableName: String; virtual; abstract;
    function GetManualDDL: String; virtual; abstract;
    procedure CleanupDDLTables; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateTableCreatesTable;
    procedure TestCreateTableIfNotExistsTwice;
    procedure TestExecDDLCreatesTable;
    procedure TestCreateTableAutoCommits;
  end;

  { TTestM2MLoadAssociationsCommon }

  TTestM2MLoadAssociationsCommon = class(TTestIntegration)
  protected
    procedure DirectInsertStudent(aID: Integer; const aName: String);
    procedure DirectInsertCourse(aID: Integer; const aTitle: String);
    procedure DirectInsertJunction(aStudentID, aCourseID: Integer);
  protected
    procedure CreateM2MTables; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadAssociationsByPropertyName;
    procedure TestLoadAssociationsBidirectional;
    procedure TestLoadAssociationsExplicitClass;
    procedure TestLoadAssociationsEmptyResult;
    procedure TestLoadAssociationsMultiple;
    procedure TestLoadAssociationsOpsClean;
  end;

  { TTestM2MAssociationsCommon }

  TTestM2MAssociationsCommon = class(TTestIntegration)
  protected
    function CountJunctionRows(aStudentID, aCourseID: Integer): Integer;
  protected
    procedure CreateM2MTables; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAssociation;
    procedure TestAddAssociationReverseOrder;
    procedure TestRemoveAssociation;
    procedure TestRemoveAssociationReverseOrder;
    procedure TestAddDuplicateAssociationRaises;
    procedure TestTransactionRollbackOnFailure;
    procedure TestAddAssociationUnregisteredPairRaises;
    procedure TestRemoveAssociationUnregisteredPairRaises;
  end;

  { TTestM2MCascadeDeleteCommon }

  TTestM2MCascadeDeleteCommon = class(TTestIntegration)
  protected
    procedure DirectInsertStudent(aID: Integer; const aName: String);
    procedure DirectInsertCourse(aID: Integer; const aTitle: String);
    procedure DirectInsertJunction(const aTable: String; aFK1Value, aFK2Value: Integer; const aFK1Name, aFK2Name: String);
    function CountJunctionRows(const aTable: String): Integer;
  protected
    procedure CreateM2MTables; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCascadeDeleteRemovesJunctionRows;
    procedure TestCascadeDeletePreservesAssociatedEntities;
    procedure TestCascadeDeletePreservesOtherAssociations;
    procedure TestCascadeDeleteMultipleM2MDefs;
    procedure TestCascadeDeleteDBCascadeSkip;
    procedure TestCascadeDeleteTransactionAtomicity;
  end;

  { ===== Lazy loading test domain classes ===== }

  TOPLazyObjectList = specialize TOPLazy<TObjectList>;

  { TTestLazyDepartment — parent with lazy children via IOPLazyInit }

  TTestLazyDepartment = class(TObject, IOPPersistenceState, IOPLazyInit)
  private
    FID: Integer;
    FName: String;
    FState: TObjectPersistenceState;
    FEmployeeList: TObjectList;
    FLazyEmployees: TOPLazyObjectList;
    FManager: TObject;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
    function GetEmployeeList: TObjectList;
    procedure DoLoadEmployees(aTarget: TObject);
    procedure InitLazyLoaders(aManager: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property EmployeeList: TObjectList read GetEmployeeList;
  end;

  { TTestLazyEmployee — child entity for 1:N lazy loading }

  TTestLazyEmployee = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FDepartmentID: Integer;
    FEmpName: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property DepartmentID: Integer read FDepartmentID write FDepartmentID;
    property EmpName: String read FEmpName write FEmpName;
  end;

  { TTestLazyLoadChildrenCommon }

  TTestLazyLoadChildrenCommon = class(TTestIntegration)
  protected
    procedure DirectInsertDept(aID: Integer; const aName: String);
    procedure DirectInsertEmployee(aID, aDeptID: Integer; const aEmpName: String);
  protected
    procedure CreateLazyTables; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLazyChildrenNotLoadedBeforeAccess;
    procedure TestLazyChildrenLoadedOnAccess;
    procedure TestLazyChildrenMatchExplicitLoad;
    procedure TestLazyInvalidateReloads;
    procedure TestLazyNoLoaderReturnsEmpty;
  end;

  { TTestLazyM2MStudent — student with lazy M:N courses via IOPLazyInit }

  TTestLazyM2MStudent = class(TObject, IOPPersistenceState, IOPLazyInit)
  private
    FID: Integer;
    FName: String;
    FState: TObjectPersistenceState;
    FCourseList: TObjectList;
    FLazyCourses: TOPLazyObjectList;
    FManager: TObject;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
    function GetCourseList: TObjectList;
    procedure DoLoadCourses(aTarget: TObject);
    procedure InitLazyLoaders(aManager: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property CourseList: TObjectList read GetCourseList;
  end;

  { TTestLazyM2MCourse — reuse existing TTestM2MCourse for the associated side }

  { TTestLazyM2MCommon }

  TTestLazyM2MCommon = class(TTestIntegration)
  protected
    procedure DirectInsertStudent(aID: Integer; const aName: String);
    procedure DirectInsertCourse(aID: Integer; const aTitle: String);
    procedure DirectInsertJunction(aStudentID, aCourseID: Integer);
  protected
    procedure CreateLazyM2MTables; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLazyAssociationsLoadedOnAccess;
    procedure TestLazyAssociationsMatchExplicit;
  end;

  { TTestSaveListCommon — tests Save(list) feature }

  TTestSaveListCommon = class(TTestIntegration)
  protected
    procedure DirectInsertContact(aID: Integer; const aName: String);
    procedure DirectSelectContact(aID: Integer; out aName: String);
    function CountContacts: Integer;
  protected
    procedure CreateSaveListTables; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSaveListInsertsNewItems;
    procedure TestSaveListUpdatesModifiedItems;
    procedure TestSaveListDeletesMarkedItems;
    procedure TestSaveListSkipsCleanItems;
    procedure TestSaveListMixedStates;
  end;

implementation

uses Opf.Registry, Opf.ListReg, Opf.Relations, Opf.Snapshot;

{ ===== SQL Utility Functions ===== }

{ All utilities use C.Transaction (the connection's single transaction)
  with a simple StartTransaction / Commit cycle. }

procedure SetParamValue(P: TParam; const V: Variant);
begin
  case VarType(V) and varTypeMask of
    varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64:
      P.AsInteger:=V;
    varDouble, varSingle, varCurrency:
      P.AsFloat:=V;
  else
    P.AsString:=VarToStr(V);
  end;
end;

function SQLCount(C: TSQLConnection; const aTable: String): Integer;
var Q: TSQLQuery;
begin
  if C.Transaction.Active then
    C.Transaction.Rollback;
  C.Transaction.StartTransaction;
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=C; Q.Transaction:=C.Transaction;
    Q.SQL.Text:='SELECT COUNT(*) FROM ' + aTable;
    Q.Open;
    Result:=Q.Fields[0].AsInteger;
    Q.Close;
  finally Q.Free; end;
  C.Transaction.Commit;
end;

procedure SQLExec(C: TSQLConnection; const aSQL: String);
var Q: TSQLQuery;
begin
  if C.Transaction.Active then
    C.Transaction.Rollback;
  C.Transaction.StartTransaction;
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=C; Q.Transaction:=C.Transaction;
    Q.SQL.Text:=aSQL;
    Q.ExecSQL;
  finally Q.Free; end;
  C.Transaction.Commit;
end;

procedure SQLInsert(C: TSQLConnection; const aSQL: String;
  const aNames: array of String; const aValues: array of Variant);
var Q: TSQLQuery; I: Integer;
begin
  if C.Transaction.Active then
    C.Transaction.Rollback;
  C.Transaction.StartTransaction;
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=C; Q.Transaction:=C.Transaction;
    Q.SQL.Text:=aSQL;
    for I:=0 to High(aNames) do
      SetParamValue(Q.Params.ParamByName(aNames[I]), aValues[I]);
    Q.ExecSQL;
  finally Q.Free; end;
  C.Transaction.Commit;
end;

function SQLQueryStr(C: TSQLConnection; const aSQL, aParamName: String;
  aParamValue: Variant; const aFieldName: String): String;
var Q: TSQLQuery;
begin
  Result:='';
  if C.Transaction.Active then
    C.Transaction.Rollback;
  C.Transaction.StartTransaction;
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=C; Q.Transaction:=C.Transaction;
    Q.SQL.Text:=aSQL;
    if aParamName<>'' then
      SetParamValue(Q.Params.ParamByName(aParamName), aParamValue);
    Q.Open;
    if not Q.EOF then
      Result:=Q.FieldByName(aFieldName).AsString;
    Q.Close;
  finally Q.Free; end;
  C.Transaction.Commit;
end;

function SQLQueryInt(C: TSQLConnection; const aSQL, aParamName: String;
  aParamValue: Variant; const aFieldName: String): Integer;
var Q: TSQLQuery;
begin
  Result:=0;
  if C.Transaction.Active then
    C.Transaction.Rollback;
  C.Transaction.StartTransaction;
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=C; Q.Transaction:=C.Transaction;
    Q.SQL.Text:=aSQL;
    if aParamName<>'' then
      SetParamValue(Q.Params.ParamByName(aParamName), aParamValue);
    Q.Open;
    if not Q.EOF then
      Result:=Q.FieldByName(aFieldName).AsInteger;
    Q.Close;
  finally Q.Free; end;
  C.Transaction.Commit;
end;

function SQLQueryFloat(C: TSQLConnection; const aSQL, aParamName: String;
  aParamValue: Variant; const aFieldName: String): Double;
var Q: TSQLQuery;
begin
  Result:=0;
  if C.Transaction.Active then
    C.Transaction.Rollback;
  C.Transaction.StartTransaction;
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=C; Q.Transaction:=C.Transaction;
    Q.SQL.Text:=aSQL;
    if aParamName<>'' then
      SetParamValue(Q.Params.ParamByName(aParamName), aParamValue);
    Q.Open;
    if not Q.EOF then
      Result:=Q.FieldByName(aFieldName).AsFloat;
    Q.Close;
  finally Q.Free; end;
  C.Transaction.Commit;
end;

procedure SQLSelect2(C: TSQLConnection; const aSQL, aParamName: String;
  aParamValue: Variant; const aField1, aField2: String;
  out aVal1: String; out aVal2: Variant);
var Q: TSQLQuery;
begin
  aVal1:=''; aVal2:=Null;
  if C.Transaction.Active then
    C.Transaction.Rollback;
  C.Transaction.StartTransaction;
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=C; Q.Transaction:=C.Transaction;
    Q.SQL.Text:=aSQL;
    if aParamName<>'' then
      SetParamValue(Q.Params.ParamByName(aParamName), aParamValue);
    Q.Open;
    if not Q.EOF then
    begin
      aVal1:=Q.FieldByName(aField1).AsString;
      aVal2:=Q.FieldByName(aField2).Value;
    end;
    Q.Close;
  finally Q.Free; end;
  C.Transaction.Commit;
end;

{ ===== Fixture Class Implementations ===== }

function TTestMgrSaveContact.GetPersistenceState: TObjectPersistenceState; begin Result:=FState; end;
procedure TTestMgrSaveContact.SetPersistenceState(aValue: TObjectPersistenceState); begin FState:=aValue; end;

function TTestCascadeParent.GetPersistenceState: TObjectPersistenceState; begin Result:=FState; end;
procedure TTestCascadeParent.SetPersistenceState(aValue: TObjectPersistenceState); begin FState:=aValue; end;

function TTestCascadeChild.GetPersistenceState: TObjectPersistenceState; begin Result:=FState; end;
procedure TTestCascadeChild.SetPersistenceState(aValue: TObjectPersistenceState); begin FState:=aValue; end;

function TTestCascadeGrandchild.GetPersistenceState: TObjectPersistenceState; begin Result:=FState; end;
procedure TTestCascadeGrandchild.SetPersistenceState(aValue: TObjectPersistenceState); begin FState:=aValue; end;

function TTestMappedContact.GetPersistenceState: TObjectPersistenceState; begin Result:=FState; end;
procedure TTestMappedContact.SetPersistenceState(aValue: TObjectPersistenceState); begin FState:=aValue; end;

constructor TTestJsonContact.Create; begin inherited Create; FAddress:=TTestJsonAddress.Create; end;
destructor TTestJsonContact.Destroy; begin FAddress.Free; inherited Destroy; end;
function TTestJsonContact.GetPersistenceState: TObjectPersistenceState; begin Result:=FState; end;
procedure TTestJsonContact.SetPersistenceState(aValue: TObjectPersistenceState); begin FState:=aValue; end;

function TTestAutoIncContact.GetPersistenceState: TObjectPersistenceState; begin Result:=FState; end;
procedure TTestAutoIncContact.SetPersistenceState(aValue: TObjectPersistenceState); begin FState:=aValue; end;

function TTestUUIDContact.GetPersistenceState: TObjectPersistenceState; begin Result:=FState; end;
procedure TTestUUIDContact.SetPersistenceState(aValue: TObjectPersistenceState); begin FState:=aValue; end;

function TTestCustomGenContact.GetPersistenceState: TObjectPersistenceState; begin Result:=FState; end;
procedure TTestCustomGenContact.SetPersistenceState(aValue: TObjectPersistenceState); begin FState:=aValue; end;

constructor TTestContactWithChildren.Create; begin inherited Create; FAddressList:=TObjectList.Create(True); end;
destructor TTestContactWithChildren.Destroy; begin FreeAndNil(FAddressList); inherited Destroy; end;
function TTestContactWithChildren.GetPersistenceState: TObjectPersistenceState; begin Result:=FState; end;
procedure TTestContactWithChildren.SetPersistenceState(aValue: TObjectPersistenceState); begin FState:=aValue; end;

function TTestAddress.GetPersistenceState: TObjectPersistenceState; begin Result:=FState; end;
procedure TTestAddress.SetPersistenceState(aValue: TObjectPersistenceState); begin FState:=aValue; end;

function TTestIntfProduct.GetPersistenceState: TObjectPersistenceState; begin Result:=FState; end;
procedure TTestIntfProduct.SetPersistenceState(aValue: TObjectPersistenceState); begin FState:=aValue; end;

procedure TTestIntfProduct.GetFieldDefs(aMap: TOPFieldMapList);
var F: TOPFieldMap;
begin
  F:=aMap.AddField('ID', 'ID'); F.Options:=F.Options + [foInKey, foAutoInc]; F.PropertyTypeInfo:=TypeInfo(Integer);
  F:=aMap.AddField('Name', 'Name'); F.PropertyTypeInfo:=TypeInfo(UTF8String);
  F:=aMap.AddField('Price', 'Price'); F.PropertyTypeInfo:=TypeInfo(Double);
end;

function TTestIntfProduct.ApplyField(aFieldMap: TOPFieldMap; aField: TField): Boolean;
begin
  Result:=True;
  if aField.IsNull then
  begin
    if aFieldMap.PropertyName='ID' then FID:=0
    else if aFieldMap.PropertyName='Name' then FName:=''
    else if aFieldMap.PropertyName='Price' then FPrice:=0.0
    else Result:=False;
    Exit;
  end;
  if aFieldMap.PropertyName='ID' then FID:=aField.AsInteger
  else if aFieldMap.PropertyName='Name' then FName:=aField.AsString
  else if aFieldMap.PropertyName='Price' then FPrice:=aField.AsFloat
  else Result:=False;
end;

function TTestIntfProduct.PersistField(aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
begin
  Result:=True;
  if aFieldMap.PropertyName='ID' then aParam.AsInteger:=FID
  else if aFieldMap.PropertyName='Name' then aParam.AsString:=FName
  else if aFieldMap.PropertyName='Price' then aParam.AsFloat:=FPrice
  else Result:=False;
end;

constructor TTestExtendedTypeContact.Create; begin inherited Create; FState:=opsNew; end;
function TTestExtendedTypeContact.GetPersistenceState: TObjectPersistenceState; begin Result:=FState; end;
procedure TTestExtendedTypeContact.SetPersistenceState(aValue: TObjectPersistenceState); begin FState:=aValue; end;

function TTestSequenceContact.GetPersistenceState: TObjectPersistenceState; begin Result:=FState; end;
procedure TTestSequenceContact.SetPersistenceState(aValue: TObjectPersistenceState); begin FState:=aValue; end;

constructor TTestM2MStudent.Create; begin inherited Create; FCourseList:=TObjectList.Create(True); end;
destructor TTestM2MStudent.Destroy; begin FreeAndNil(FCourseList); inherited Destroy; end;
function TTestM2MStudent.GetPersistenceState: TObjectPersistenceState; begin Result:=FState; end;
procedure TTestM2MStudent.SetPersistenceState(aValue: TObjectPersistenceState); begin FState:=aValue; end;

constructor TTestM2MCourse.Create; begin inherited Create; FStudentList:=TObjectList.Create(True); end;
destructor TTestM2MCourse.Destroy; begin FreeAndNil(FStudentList); inherited Destroy; end;
function TTestM2MCourse.GetPersistenceState: TObjectPersistenceState; begin Result:=FState; end;
procedure TTestM2MCourse.SetPersistenceState(aValue: TObjectPersistenceState); begin FState:=aValue; end;

function TTestM2MClub.GetPersistenceState: TObjectPersistenceState; begin Result:=FState; end;
procedure TTestM2MClub.SetPersistenceState(aValue: TObjectPersistenceState); begin FState:=aValue; end;

{ ===== TTestIntegrationCommon ===== }

function TTestIntegrationCommon.CountRows: Integer;
begin Result:=SQLCount(FConn, 'TestIntPerson'); end;

procedure TTestIntegrationCommon.DirectInsert(aID: Integer; const aName: String; aAge: Integer);
begin
  SQLInsert(FConn, 'INSERT INTO TestIntPerson (ID, Name, Age) VALUES (:ID, :Name, :Age)',
    ['ID', 'Name', 'Age'], [aID, aName, aAge]);
end;

procedure TTestIntegrationCommon.DirectSelect(aID: Integer; out aName: String; out aAge: Integer);
begin
  aName:=SQLQueryStr(FConn, 'SELECT Name, Age FROM TestIntPerson WHERE ID=:ID', 'ID', aID, 'Name');
  aAge:=SQLQueryInt(FConn, 'SELECT Name, Age FROM TestIntPerson WHERE ID=:ID', 'ID', aID, 'Age');
end;

procedure TTestIntegrationCommon.SetUp;
begin
  inherited SetUp;
  CreateTestTable;
  FFieldMap:=TOPFieldMapList.Create(TOPFieldMap);
  FFieldMap.AddField('ID', 'ID', [foInKey, foInInsert, foInSelect]);
  FFieldMap.AddField('Name', 'Name', [foInInsert, foInUpdate, foInSelect]);
  FFieldMap.AddField('Age', 'Age', [foInInsert, foInUpdate, foInSelect]);
  FPersister:=TTestPersister.Create;
  FObj:=TTestObject.Create;
  FObj.ID:=1; FObj.Name:='Alice'; FObj.Age:=30;
  FExecutor:=TOPExecutor.Create(Nil);
end;

procedure TTestIntegrationCommon.TearDown;
begin
  FreeAndNil(FExecutor);
  FreeAndNil(FObj);
  FreeAndNil(FPersister);
  FreeAndNil(FFieldMap);
  DeleteTables(['TestIntPerson']);
  inherited TearDown;
end;

procedure TTestIntegrationCommon.TestInsert;
var SQL: UTF8String; SelName: String; SelAge: Integer;
    Handler: TTransactionHandler;
begin
  SQL:=OPSQLBuilder.GetInsertSQL('TestIntPerson', FFieldMap.AsArray);
  Handler:=TTransactionHandler.Create;
  try
    Handler.BeginTransaction;
    Handler.EnsureConnection(FConn);
    FExecutor.ExecuteSQL(FObj, FPersister, FFieldMap, SQL, FConn);
    Handler.Commit;
  finally
    Handler.Free;
  end;
  AssertEquals('Row count after insert', 1, CountRows);
  DirectSelect(1, SelName, SelAge);
  AssertEquals('Inserted name', 'Alice', SelName);
  AssertEquals('Inserted age', 30, SelAge);
end;

procedure TTestIntegrationCommon.TestSelect;
var SQL: UTF8String;
begin
  DirectInsert(1, 'Alice', 30);
  FObj.ID:=0; FObj.Name:=''; FObj.Age:=0;
  SQL:=OPSQLBuilder.GetSelectSQL('TestIntPerson', FFieldMap.AsArray);
  FExecutor.QuerySQL(FObj, FPersister, FFieldMap, SQL, FConn);
  AssertEquals('ID loaded', 1, FObj.ID);
  AssertEquals('Name loaded', 'Alice', FObj.Name);
  AssertEquals('Age loaded', 30, FObj.Age);
end;

procedure TTestIntegrationCommon.TestUpdate;
var InsertSQL, UpdateSQL: UTF8String; SelName: String; SelAge: Integer;
    Handler: TTransactionHandler;
begin
  InsertSQL:=OPSQLBuilder.GetInsertSQL('TestIntPerson', FFieldMap.AsArray);
  Handler:=TTransactionHandler.Create;
  try
    Handler.BeginTransaction;
    Handler.EnsureConnection(FConn);
    FExecutor.ExecuteSQL(FObj, FPersister, FFieldMap, InsertSQL, FConn);
    FObj.Name:='Bob'; FObj.Age:=25;
    UpdateSQL:=OPSQLBuilder.GetUpdateSQL('TestIntPerson', FFieldMap.AsArray);
    FExecutor.ExecuteSQL(FObj, FPersister, FFieldMap, UpdateSQL, FConn);
    Handler.Commit;
  finally
    Handler.Free;
  end;
  DirectSelect(1, SelName, SelAge);
  AssertEquals('Updated name', 'Bob', SelName);
  AssertEquals('Updated age', 25, SelAge);
end;

procedure TTestIntegrationCommon.TestDelete;
var InsertSQL, DeleteSQL: UTF8String;
    Handler: TTransactionHandler;
begin
  InsertSQL:=OPSQLBuilder.GetInsertSQL('TestIntPerson', FFieldMap.AsArray);
  Handler:=TTransactionHandler.Create;
  try
    Handler.BeginTransaction;
    Handler.EnsureConnection(FConn);
    FExecutor.ExecuteSQL(FObj, FPersister, FFieldMap, InsertSQL, FConn);
    Handler.Commit;
  finally
    Handler.Free;
  end;
  AssertEquals('Row count after insert', 1, CountRows);
  DeleteSQL:=OPSQLBuilder.GetDeleteSQL('TestIntPerson', FFieldMap.AsArray);
  Handler:=TTransactionHandler.Create;
  try
    Handler.BeginTransaction;
    Handler.EnsureConnection(FConn);
    FExecutor.ExecuteSQL(FObj, FPersister, FFieldMap, DeleteSQL, FConn);
    Handler.Commit;
  finally
    Handler.Free;
  end;
  AssertEquals('Row count after delete', 0, CountRows);
end;

procedure TTestIntegrationCommon.TestTransactionImplicit;
var
  SQL: UTF8String;
  Handler: TTransactionHandler;
begin
  SQL:=OPSQLBuilder.GetInsertSQL('TestIntPerson', FFieldMap.AsArray);
  Handler:=TTransactionHandler.Create;
  try
    Handler.BeginTransaction;
    Handler.EnsureConnection(FConn);
    FExecutor.ExecuteSQL(FObj, FPersister, FFieldMap, SQL, FConn);
    Handler.Commit;
  finally
    Handler.Free;
  end;
  AssertEquals('Row persists after commit', 1, CountRows);
end;

procedure TTestIntegrationCommon.TestTransactionRollback;
var
  SQL: UTF8String;
  Handler: TTransactionHandler;
begin
  SQL:=OPSQLBuilder.GetInsertSQL('TestIntPerson', FFieldMap.AsArray);
  Handler:=TTransactionHandler.Create;
  try
    Handler.BeginTransaction;
    Handler.EnsureConnection(FConn);
    FExecutor.ExecuteSQL(FObj, FPersister, FFieldMap, SQL, FConn);
    Handler.Rollback;
  finally
    Handler.Free;
  end;
  AssertEquals('Row NOT persisted after rollback', 0, CountRows);
end;

{ ===== TTestManagerSaveCommon ===== }

function TTestManagerSaveCommon.CountContactRows: Integer;
begin Result:=SQLCount(FConn, 'TestMgrSaveContact'); end;

procedure TTestManagerSaveCommon.DirectSelectContact(aID: Integer; out aName: String);
begin aName:=SQLQueryStr(FConn, 'SELECT Name FROM TestMgrSaveContact WHERE ID=:ID', 'ID', aID, 'Name'); end;

procedure TTestManagerSaveCommon.DirectInsertContact(aID: Integer; const aName: String);
begin SQLInsert(FConn, 'INSERT INTO TestMgrSaveContact (ID, Name) VALUES (:ID, :Name)', ['ID', 'Name'], [aID, aName]); end;

procedure TTestManagerSaveCommon.SetUp;
var Conn: TOPConnection;
begin
  inherited SetUp;
  CreateContactTable;
  OPRegistry.RegisterPersister(TTestMgrSaveContact, TRTTIPersister);
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn;
  Conn.IsDefault:=True;
end;

procedure TTestManagerSaveCommon.TearDown;
begin
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  DeleteTables(['TestMgrSaveContact']);
  inherited TearDown;
end;

procedure TTestManagerSaveCommon.TestSaveInsert;
var Obj: TTestMgrSaveContact; SelName: String;
begin
  Obj:=TTestMgrSaveContact.Create;
  try
    Obj.ID:=1; Obj.Name:='Alice'; Obj.FState:=opsNew;
    OPManager.Save(Obj);
    AssertEquals('Row count after save', 1, CountContactRows);
    DirectSelectContact(1, SelName);
    AssertEquals('Saved name', 'Alice', SelName);
    AssertEquals('State after save', Ord(opsClean), Ord(Obj.GetPersistenceState));
  finally Obj.Free; end;
end;

procedure TTestManagerSaveCommon.TestSaveUpdate;
var Obj: TTestMgrSaveContact; SelName: String;
begin
  Obj:=TTestMgrSaveContact.Create;
  try
    Obj.ID:=1; Obj.Name:='Alice'; Obj.FState:=opsNew;
    OPManager.Save(Obj);
    Obj.Name:='Bob'; Obj.FState:=opsUpdate;
    OPManager.Save(Obj);
    DirectSelectContact(1, SelName);
    AssertEquals('Updated name', 'Bob', SelName);
    AssertEquals('Row count', 1, CountContactRows);
    AssertEquals('State after update', Ord(opsClean), Ord(Obj.GetPersistenceState));
  finally Obj.Free; end;
end;

procedure TTestManagerSaveCommon.TestSaveUpdateOnlyTargetRow;
var Obj: TTestMgrSaveContact; SelName: String;
begin
  DirectInsertContact(1, 'Alice');
  DirectInsertContact(2, 'Carol');
  Obj:=TTestMgrSaveContact.Create;
  try
    Obj.ID:=1; Obj.Name:='AliceUpdated'; Obj.FState:=opsUpdate;
    OPManager.Save(Obj);
    DirectSelectContact(1, SelName);
    AssertEquals('Target row updated', 'AliceUpdated', SelName);
    DirectSelectContact(2, SelName);
    AssertEquals('Other row unchanged', 'Carol', SelName);
  finally Obj.Free; end;
end;

{ ===== TTestManagerDeleteCommon ===== }

function TTestManagerDeleteCommon.CountContactRows: Integer;
begin Result:=SQLCount(FConn, 'TestMgrSaveContact'); end;

procedure TTestManagerDeleteCommon.DirectSelectContact(aID: Integer; out aName: String);
begin aName:=SQLQueryStr(FConn, 'SELECT Name FROM TestMgrSaveContact WHERE ID=:ID', 'ID', aID, 'Name'); end;

procedure TTestManagerDeleteCommon.DirectInsertContact(aID: Integer; const aName: String);
begin SQLInsert(FConn, 'INSERT INTO TestMgrSaveContact (ID, Name) VALUES (:ID, :Name)', ['ID', 'Name'], [aID, aName]); end;

procedure TTestManagerDeleteCommon.SetUp;
var Conn: TOPConnection;
begin
  inherited SetUp;
  CreateContactTable;
  OPRegistry.RegisterPersister(TTestMgrSaveContact, TRTTIPersister);
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn;
  Conn.IsDefault:=True;
end;

procedure TTestManagerDeleteCommon.TearDown;
begin
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  inherited TearDown;
end;

procedure TTestManagerDeleteCommon.TestDeleteObject;
var Obj: TTestMgrSaveContact;
begin
  Obj:=TTestMgrSaveContact.Create;
  try
    Obj.ID:=1; Obj.Name:='Alice'; Obj.FState:=opsNew;
    OPManager.Save(Obj);
    AssertEquals('Row exists after save', 1, CountContactRows);
    OPManager.Delete(Obj);
    AssertEquals('Row count after delete', 0, CountContactRows);
    AssertEquals('State after delete', Ord(opsDeleted), Ord(Obj.GetPersistenceState));
  finally Obj.Free; end;
end;

procedure TTestManagerDeleteCommon.TestDeleteOnlyTargetRow;
var Obj: TTestMgrSaveContact; SelName: String;
begin
  DirectInsertContact(1, 'Alice');
  DirectInsertContact(2, 'Carol');
  Obj:=TTestMgrSaveContact.Create;
  try
    Obj.ID:=1; Obj.Name:='Alice'; Obj.FState:=opsClean;
    OPManager.Delete(Obj);
    AssertEquals('Row count after delete', 1, CountContactRows);
    DirectSelectContact(2, SelName);
    AssertEquals('Other row unchanged', 'Carol', SelName);
    AssertEquals('State after delete', Ord(opsDeleted), Ord(Obj.GetPersistenceState));
  finally Obj.Free; end;
end;

{ ===== TTestCascadeDeleteCommon ===== }

procedure TTestCascadeDeleteCommon.DirectInsertParent(aID: Integer; const aName: String);
begin SQLInsert(FConn, 'INSERT INTO TestCascadeParent (ID, Name) VALUES (:ID, :Name)', ['ID', 'Name'], [aID, aName]); end;

procedure TTestCascadeDeleteCommon.DirectInsertChild(aID, aParentID: Integer; const aValue: String);
begin SQLInsert(FConn, 'INSERT INTO TestCascadeChild (ID, ParentID, Val) VALUES (:ID, :ParentID, :Val)', ['ID', 'ParentID', 'Val'], [aID, aParentID, aValue]); end;

procedure TTestCascadeDeleteCommon.DirectInsertGrandchild(aID, aChildID: Integer; const aData: String);
begin SQLInsert(FConn, 'INSERT INTO TestCascadeGrandchild (ID, ChildID, Data) VALUES (:ID, :ChildID, :Data)', ['ID', 'ChildID', 'Data'], [aID, aChildID, aData]); end;

function TTestCascadeDeleteCommon.CountRows(const aTable: String): Integer;
begin Result:=SQLCount(FConn, aTable); end;

procedure TTestCascadeDeleteCommon.SetUp;
var Conn: TOPConnection;
begin
  inherited SetUp;
  CreateCascadeTables;
  OPRegistry.RegisterPersister(TTestCascadeParent, TRTTIPersister);
  OPRegistry.RegisterPersister(TTestCascadeChild, TRTTIPersister);
  OPRegistry.RegisterPersister(TTestCascadeGrandchild, TRTTIPersister);
  OPRelationRegistry.RegisterRelationship(TTestCascadeParent, TTestCascadeChild, 'ParentID');
  OPRelationRegistry.RegisterRelationship(TTestCascadeChild, TTestCascadeGrandchild, 'ChildID');
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn;
  Conn.IsDefault:=True;
end;

procedure TTestCascadeDeleteCommon.TearDown;
begin
  TOPManager.ClearInstance;
  TOPRelationRegistry.ClearInstance;
  TOPRegistry.ClearInstance;
  inherited TearDown;
end;

procedure TTestCascadeDeleteCommon.TestCascadeDeleteParentWithChildren;
var Obj: TTestCascadeParent;
begin
  DirectInsertParent(1, 'Alice');
  DirectInsertChild(10, 1, 'C1');
  DirectInsertChild(11, 1, 'C2');
  Obj:=TTestCascadeParent.Create;
  try
    Obj.ID:=1; Obj.Name:='Alice'; Obj.FState:=opsClean;
    OPManager.Delete(Obj);
    AssertEquals('Parent rows', 0, CountRows('TestCascadeParent'));
    AssertEquals('Child rows', 0, CountRows('TestCascadeChild'));
    AssertEquals('State after delete', Ord(opsDeleted), Ord(Obj.GetPersistenceState));
  finally Obj.Free; end;
end;

procedure TTestCascadeDeleteCommon.TestCascadeDeleteParentNoChildren;
var Obj: TTestCascadeParent;
begin
  DirectInsertParent(1, 'Alice');
  Obj:=TTestCascadeParent.Create;
  try
    Obj.ID:=1; Obj.Name:='Alice'; Obj.FState:=opsClean;
    OPManager.Delete(Obj);
    AssertEquals('Parent rows after delete', 0, CountRows('TestCascadeParent'));
    AssertEquals('State after delete', Ord(opsDeleted), Ord(Obj.GetPersistenceState));
  finally Obj.Free; end;
end;

procedure TTestCascadeDeleteCommon.TestCascadeDeleteMultipleChildren;
var Obj: TTestCascadeParent;
begin
  DirectInsertParent(1, 'Alice');
  DirectInsertChild(10, 1, 'C1'); DirectInsertChild(11, 1, 'C2'); DirectInsertChild(12, 1, 'C3');
  Obj:=TTestCascadeParent.Create;
  try
    Obj.ID:=1; Obj.Name:='Alice'; Obj.FState:=opsClean;
    OPManager.Delete(Obj);
    AssertEquals('Parent rows', 0, CountRows('TestCascadeParent'));
    AssertEquals('Child rows', 0, CountRows('TestCascadeChild'));
  finally Obj.Free; end;
end;

procedure TTestCascadeDeleteCommon.TestCascadeDeletePreservesOtherParents;
var Obj: TTestCascadeParent;
begin
  DirectInsertParent(1, 'Alice'); DirectInsertParent(2, 'Bob');
  DirectInsertChild(10, 1, 'C1-A'); DirectInsertChild(11, 1, 'C2-A');
  DirectInsertChild(20, 2, 'C1-B'); DirectInsertChild(21, 2, 'C2-B');
  Obj:=TTestCascadeParent.Create;
  try
    Obj.ID:=1; Obj.Name:='Alice'; Obj.FState:=opsClean;
    OPManager.Delete(Obj);
    AssertEquals('Parent rows after delete', 1, CountRows('TestCascadeParent'));
    AssertEquals('Child rows after delete', 2, CountRows('TestCascadeChild'));
  finally Obj.Free; end;
end;

procedure TTestCascadeDeleteCommon.TestCascadeDeleteNestedRelationships;
var Obj: TTestCascadeParent;
begin
  DirectInsertParent(1, 'Alice');
  DirectInsertChild(10, 1, 'C1'); DirectInsertChild(11, 1, 'C2');
  DirectInsertGrandchild(100, 10, 'GC1'); DirectInsertGrandchild(101, 10, 'GC2');
  DirectInsertGrandchild(102, 11, 'GC3');
  Obj:=TTestCascadeParent.Create;
  try
    Obj.ID:=1; Obj.Name:='Alice'; Obj.FState:=opsClean;
    OPManager.Delete(Obj);
    AssertEquals('Parent rows', 0, CountRows('TestCascadeParent'));
    AssertEquals('Child rows', 0, CountRows('TestCascadeChild'));
    AssertEquals('Grandchild rows', 0, CountRows('TestCascadeGrandchild'));
  finally Obj.Free; end;
end;

procedure TTestCascadeDeleteCommon.TestCascadeDeleteRollbackOnFailure;
var Obj: TTestCascadeParent; Raised: Boolean;
begin
  DirectInsertParent(1, 'Alice');
  DirectInsertChild(10, 1, 'C1');
  // Drop child table to force cascade failure
  ExecDDL('DROP TABLE TestCascadeChild');
  Obj:=TTestCascadeParent.Create;
  try
    Obj.ID:=1; Obj.Name:='Alice'; Obj.FState:=opsClean;
    Raised:=False;
    try OPManager.Delete(Obj); except on E: Exception do Raised:=True; end;
    AssertTrue('Exception raised on cascade failure', Raised);
    AssertEquals('Parent row still exists after rollback', 1, CountRows('TestCascadeParent'));
  finally Obj.Free; end;
end;

procedure TTestCascadeDeleteCommon.TestCascadeDeleteDBCascadeSkipsChildDelete;
var Obj: TTestCascadeParent;
begin
  TOPRelationRegistry.ClearInstance;
  OPRelationRegistry.RegisterRelationship(TTestCascadeParent, TTestCascadeChild, 'ParentID', True);
  OPRelationRegistry.RegisterRelationship(TTestCascadeChild, TTestCascadeGrandchild, 'ChildID');
  DirectInsertParent(1, 'Alice');
  DirectInsertChild(10, 1, 'C1'); DirectInsertChild(11, 1, 'C2');
  Obj:=TTestCascadeParent.Create;
  try
    Obj.ID:=1; Obj.Name:='Alice'; Obj.FState:=opsClean;
    OPManager.Delete(Obj);
    AssertEquals('Parent rows after delete', 0, CountRows('TestCascadeParent'));
    AssertEquals('Child rows remain (ORM skipped, no DB FK)', 2, CountRows('TestCascadeChild'));
    AssertEquals('State after delete', Ord(opsDeleted), Ord(Obj.GetPersistenceState));
  finally Obj.Free; end;
end;

procedure TTestCascadeDeleteCommon.TestCascadeDeleteMixedDBAndORMCascade;
var Obj: TTestCascadeParent;
begin
  TOPRelationRegistry.ClearInstance;
  OPRelationRegistry.RegisterRelationship(TTestCascadeParent, TTestCascadeChild, 'ParentID', True);
  OPRelationRegistry.RegisterRelationship(TTestCascadeParent, TTestCascadeGrandchild, 'ChildID', False);
  DirectInsertParent(1, 'Alice');
  DirectInsertChild(10, 1, 'C1'); DirectInsertChild(11, 1, 'C2');
  DirectInsertGrandchild(100, 1, 'GC-ORM1'); DirectInsertGrandchild(101, 1, 'GC-ORM2');
  Obj:=TTestCascadeParent.Create;
  try
    Obj.ID:=1; Obj.Name:='Alice'; Obj.FState:=opsClean;
    OPManager.Delete(Obj);
    AssertEquals('Parent rows', 0, CountRows('TestCascadeParent'));
    AssertEquals('DB-cascade child rows remain', 2, CountRows('TestCascadeChild'));
    AssertEquals('ORM-cascade grandchild rows deleted', 0, CountRows('TestCascadeGrandchild'));
  finally Obj.Free; end;
end;

{ ===== TTestManagerLoadCommon ===== }

procedure TTestManagerLoadCommon.DirectInsertContact(aID: Integer; const aName: String);
begin SQLInsert(FConn, 'INSERT INTO TestMgrSaveContact (ID, Name) VALUES (:ID, :Name)', ['ID', 'Name'], [aID, aName]); end;

procedure TTestManagerLoadCommon.DoLoadNotFound;
begin OPManager.Load(FObj); end;

procedure TTestManagerLoadCommon.SetUp;
var Conn: TOPConnection;
begin
  inherited SetUp;
  CreateLoadTable;
  OPRegistry.RegisterPersister(TTestMgrSaveContact, TRTTIPersister);
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn; Conn.IsDefault:=True;
  FObj:=Nil;
end;

procedure TTestManagerLoadCommon.TearDown;
begin
  FreeAndNil(FObj);
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  inherited TearDown;
end;

procedure TTestManagerLoadCommon.TestLoadByPK;
begin
  DirectInsertContact(1, 'Alice');
  FObj:=TTestMgrSaveContact.Create;
  FObj.ID:=1;
  OPManager.Load(FObj);
  AssertEquals('Name loaded', 'Alice', FObj.Name);
  AssertEquals('State after load', Ord(opsClean), Ord(FObj.GetPersistenceState));
end;

procedure TTestManagerLoadCommon.TestLoadNotFound;
begin
  FObj:=TTestMgrSaveContact.Create;
  FObj.ID:=999;
  AssertException('Load not found raises EOPF', EOPF, @DoLoadNotFound);
end;

{ ===== TTestManagerLoadListCommon ===== }

procedure TTestManagerLoadListCommon.DirectInsertContact(aID: Integer; const aName: String);
begin SQLInsert(FConn, 'INSERT INTO TestMgrSaveContact (ID, Name) VALUES (:ID, :Name)', ['ID', 'Name'], [aID, aName]); end;

procedure TTestManagerLoadListCommon.SetUp;
var Conn: TOPConnection;
begin
  inherited SetUp;
  CreateContactTable;
  OPRegistry.RegisterPersister(TTestMgrSaveContact, TRTTIPersister);
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn; Conn.IsDefault:=True;
end;

procedure TTestManagerLoadListCommon.TearDown;
begin
  TOPManager.ClearInstance;
  TOPListRegistry.ClearInstance;
  TOPRegistry.ClearInstance;
  DeleteTables(['TestMgrSaveContact']);
  inherited TearDown;
end;

procedure TTestManagerLoadListCommon.TestLoadListMultipleRows;
var List: TObjectList; Obj: TTestMgrSaveContact;
begin
  DirectInsertContact(1, 'Alice'); DirectInsertContact(2, 'Bob'); DirectInsertContact(3, 'Carol');
  List:=TObjectList.Create(True);
  try
    OPManager.Load(List, TTestMgrSaveContact);
    AssertEquals('List count', 3, List.Count);
    Obj:=TTestMgrSaveContact(List[0]);
    AssertEquals('First ID', 1, Obj.ID);
    AssertEquals('First Name', 'Alice', Obj.Name);
  finally List.Free; end;
end;

procedure TTestManagerLoadListCommon.TestLoadListEmptyResult;
var List: TObjectList;
begin
  List:=TObjectList.Create(True);
  try
    OPManager.Load(List, TTestMgrSaveContact);
    AssertEquals('Empty list count', 0, List.Count);
  finally List.Free; end;
end;

procedure TTestManagerLoadListCommon.TestLoadListWithCriteria;
var List: TObjectList;
begin
  DirectInsertContact(1, 'Alice'); DirectInsertContact(2, 'Bob'); DirectInsertContact(3, 'Carol');
  List:=TObjectList.Create(True);
  try
    OPManager.Load(List, TTestMgrSaveContact, 'ID > 1');
    AssertEquals('Filtered list count', 2, List.Count);
  finally List.Free; end;
end;

procedure TTestManagerLoadListCommon.TestLoadListOwnership;
var List: TObjectList;
begin
  DirectInsertContact(1, 'Alice'); DirectInsertContact(2, 'Bob');
  List:=TObjectList.Create(True);
  try
    OPManager.Load(List, TTestMgrSaveContact);
    AssertEquals('List count', 2, List.Count);
  finally List.Free; end;
end;

{ ===== TTestMappedCRUDCommon ===== }

function TTestMappedCRUDCommon.CountMappedRows: Integer;
begin Result:=SQLCount(FConn, 'MappedContact'); end;

procedure TTestMappedCRUDCommon.DirectSelectMapped(aID: Integer; out aFullName: String);
begin aFullName:=SQLQueryStr(FConn, 'SELECT full_name FROM MappedContact WHERE contact_id=:ID', 'ID', aID, 'full_name'); end;

procedure TTestMappedCRUDCommon.DirectInsertMapped(aID: Integer; const aFullName: String);
begin SQLInsert(FConn, 'INSERT INTO MappedContact (contact_id, full_name) VALUES (:ID, :Name)', ['ID', 'Name'], [aID, aFullName]); end;

procedure TTestMappedCRUDCommon.SetUp;
var Conn: TOPConnection; Def: TPersisterDef;
begin
  inherited SetUp;
  CreateMappedTable;
  Def:=OPRegistry.RegisterPersister(TTestMappedContact, TRTTIPersister);
  Def.TableName:='MappedContact';
  Def.FieldMap.FindByPropertyName('ID').FieldName:='contact_id';
  Def.FieldMap.FindByPropertyName('FullName').FieldName:='full_name';
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn; Conn.IsDefault:=True;
end;

procedure TTestMappedCRUDCommon.TearDown;
begin
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  inherited TearDown;
end;

procedure TTestMappedCRUDCommon.TestSaveMapped;
var Obj: TTestMappedContact; SelName: String;
begin
  Obj:=TTestMappedContact.Create;
  try
    Obj.ID:=1; Obj.FullName:='Alice Smith'; Obj.FState:=opsNew;
    OPManager.Save(Obj);
    AssertEquals('Row count after save', 1, CountMappedRows);
    DirectSelectMapped(1, SelName);
    AssertEquals('Saved full_name', 'Alice Smith', SelName);
  finally Obj.Free; end;
end;

procedure TTestMappedCRUDCommon.TestLoadMapped;
var Obj: TTestMappedContact;
begin
  DirectInsertMapped(1, 'Bob Jones');
  Obj:=TTestMappedContact.Create;
  try
    Obj.ID:=1; OPManager.Load(Obj);
    AssertEquals('Loaded FullName', 'Bob Jones', Obj.FullName);
  finally Obj.Free; end;
end;

procedure TTestMappedCRUDCommon.TestUpdateMapped;
var Obj: TTestMappedContact; SelName: String;
begin
  Obj:=TTestMappedContact.Create;
  try
    Obj.ID:=1; Obj.FullName:='Alice Smith'; Obj.FState:=opsNew;
    OPManager.Save(Obj);
    Obj.FullName:='Alice Updated'; Obj.FState:=opsUpdate;
    OPManager.Save(Obj);
    DirectSelectMapped(1, SelName);
    AssertEquals('Updated full_name', 'Alice Updated', SelName);
  finally Obj.Free; end;
end;

procedure TTestMappedCRUDCommon.TestDeleteMapped;
var Obj: TTestMappedContact;
begin
  Obj:=TTestMappedContact.Create;
  try
    Obj.ID:=1; Obj.FullName:='Alice Smith'; Obj.FState:=opsNew;
    OPManager.Save(Obj);
    AssertEquals('Row exists after save', 1, CountMappedRows);
    OPManager.Delete(Obj);
    AssertEquals('Row count after delete', 0, CountMappedRows);
  finally Obj.Free; end;
end;

{ ===== TTestJsonBlobCRUDCommon ===== }

procedure TTestJsonBlobCRUDCommon.DirectInsertJsonContact(aID: Integer; const aName, aAddressJSON: String);
begin SQLInsert(FConn, 'INSERT INTO TestJsonContact (ID, Name, Address) VALUES (:ID, :Name, :Address)', ['ID', 'Name', 'Address'], [aID, aName, aAddressJSON]); end;

procedure TTestJsonBlobCRUDCommon.DirectSelectJsonContact(aID: Integer; out aName, aAddress: String);
var V: Variant;
begin
  SQLSelect2(FConn, 'SELECT Name, Address FROM TestJsonContact WHERE ID=:ID', 'ID', aID, 'Name', 'Address', aName, V);
  if VarIsNull(V) then aAddress:='' else aAddress:=VarToStr(V);
end;

procedure TTestJsonBlobCRUDCommon.SetUp;
var Conn: TOPConnection;
begin
  inherited SetUp;
  CreateJsonContactTable;
  OPRegistry.RegisterPersister(TTestJsonContact, TRTTIPersister);
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn; Conn.IsDefault:=True;
end;

procedure TTestJsonBlobCRUDCommon.TearDown;
begin
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  inherited TearDown;
end;

procedure TTestJsonBlobCRUDCommon.TestSaveJsonBlob;
var Obj: TTestJsonContact; SelName, SelAddress: String;
begin
  Obj:=TTestJsonContact.Create;
  try
    Obj.ID:=1; Obj.Name:='Alice';
    Obj.Address.Street:='123 Main St'; Obj.Address.City:='Springfield';
    Obj.FState:=opsNew;
    OPManager.Save(Obj);
    DirectSelectJsonContact(1, SelName, SelAddress);
    AssertEquals('Saved name', 'Alice', SelName);
    AssertTrue('Address JSON contains Street', Pos('123 Main St', SelAddress) > 0);
  finally Obj.Free; end;
end;

procedure TTestJsonBlobCRUDCommon.TestLoadJsonBlob;
var Obj: TTestJsonContact;
begin
  DirectInsertJsonContact(1, 'Bob', '{"Street":"Oak Ave","City":"Portland"}');
  Obj:=TTestJsonContact.Create;
  try
    Obj.ID:=1; OPManager.Load(Obj);
    AssertEquals('Loaded Name', 'Bob', Obj.Name);
    AssertEquals('Loaded Street', 'Oak Ave', Obj.Address.Street);
    AssertEquals('Loaded City', 'Portland', Obj.Address.City);
  finally Obj.Free; end;
end;

procedure TTestJsonBlobCRUDCommon.TestRoundTripJsonBlob;
var Src, Dest: TTestJsonContact;
begin
  Src:=TTestJsonContact.Create;
  try
    Src.ID:=1; Src.Name:='Carol';
    Src.Address.Street:='Elm St'; Src.Address.City:='Salem';
    Src.FState:=opsNew;
    OPManager.Save(Src);
  finally Src.Free; end;
  Dest:=TTestJsonContact.Create;
  try
    Dest.ID:=1; OPManager.Load(Dest);
    AssertEquals('Name round-trip', 'Carol', Dest.Name);
    AssertEquals('Street round-trip', 'Elm St', Dest.Address.Street);
    AssertEquals('City round-trip', 'Salem', Dest.Address.City);
  finally Dest.Free; end;
end;

procedure TTestJsonBlobCRUDCommon.TestSaveNilJsonProperty;
var Obj: TTestJsonContact; Q: TSQLQuery; IsNull: Boolean;
begin
  Obj:=TTestJsonContact.Create;
  try
    Obj.ID:=1; Obj.Name:='Dave';
    Obj.Address.Free; Obj.Address:=nil;
    Obj.FState:=opsNew;
    OPManager.Save(Obj);
  finally Obj.Free; end;
  if FConn.Transaction.Active then
    FConn.Transaction.Rollback;
  FConn.Transaction.StartTransaction;
  Q:=TSQLQuery.Create(nil);
  try
    Q.Database:=FConn; Q.Transaction:=FConn.Transaction;
    Q.SQL.Text:='SELECT Address FROM TestJsonContact WHERE ID=1';
    Q.Open;
    IsNull:=Q.FieldByName('Address').IsNull;
    Q.Close;
  finally Q.Free; end;
  FConn.Transaction.Commit;
  AssertTrue('Address column is NULL', IsNull);
end;

{ ===== TTestAutoIncCommon ===== }

function TTestAutoIncCommon.CountAutoIncRows: Integer;
begin Result:=SQLCount(FConn, 'TestAutoInc'); end;

procedure TTestAutoIncCommon.DirectSelectAutoInc(aID: Integer; out aName: String);
begin aName:=SQLQueryStr(FConn, 'SELECT Name FROM TestAutoInc WHERE ID=:ID', 'ID', aID, 'Name'); end;

procedure TTestAutoIncCommon.SetUp;
var Conn: TOPConnection; Field: TOPFieldMap;
begin
  inherited SetUp;
  CreateAutoIncTable;
  OPRegistry.RegisterPersister(TTestAutoIncContact, TRTTIPersister);
  OPRegistry.FindPersister(TTestAutoIncContact).TableName:='TestAutoInc';
  Field:=OPRegistry.FindPersister(TTestAutoIncContact).FieldMap.FindByPropertyName('ID');
  Field.Options:=Field.Options + [foAutoInc, foInKey] - [foInInsert];
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn; Conn.IsDefault:=True;
end;

procedure TTestAutoIncCommon.TearDown;
begin
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  DeleteTables(['TestAutoInc']);
  inherited TearDown;
end;

procedure TTestAutoIncCommon.TestAutoIncInsertPopulatesID;
var Obj: TTestAutoIncContact;
begin
  Obj:=TTestAutoIncContact.Create;
  try
    Obj.ID:=0; Obj.Name:='Alice'; Obj.FState:=opsNew;
    OPManager.Save(Obj);
    AssertTrue('ID populated (>0)', Obj.ID > 0);
    AssertEquals('First row ID', 1, Obj.ID);
    AssertEquals('State after save', Ord(opsClean), Ord(Obj.FState));
  finally Obj.Free; end;
end;

procedure TTestAutoIncCommon.TestAutoIncInsertMultipleRows;
var Obj1, Obj2, Obj3: TTestAutoIncContact;
begin
  Obj1:=TTestAutoIncContact.Create; Obj2:=TTestAutoIncContact.Create; Obj3:=TTestAutoIncContact.Create;
  try
    Obj1.Name:='Alice'; Obj1.FState:=opsNew;
    Obj2.Name:='Bob';   Obj2.FState:=opsNew;
    Obj3.Name:='Carol'; Obj3.FState:=opsNew;
    OPManager.Save(Obj1); OPManager.Save(Obj2); OPManager.Save(Obj3);
    AssertEquals('Alice ID', 1, Obj1.ID);
    AssertEquals('Bob ID', 2, Obj2.ID);
    AssertEquals('Carol ID', 3, Obj3.ID);
    AssertEquals('Row count', 3, CountAutoIncRows);
  finally Obj3.Free; Obj2.Free; Obj1.Free; end;
end;

procedure TTestAutoIncCommon.TestAutoIncInsertPreservesOtherFields;
var Obj: TTestAutoIncContact; SelName: String;
begin
  Obj:=TTestAutoIncContact.Create;
  try
    Obj.Name:='Alice'; Obj.FState:=opsNew;
    OPManager.Save(Obj);
    DirectSelectAutoInc(Obj.ID, SelName);
    AssertEquals('Name preserved', 'Alice', SelName);
  finally Obj.Free; end;
end;

procedure TTestAutoIncCommon.TestAutoIncUpdateIgnoresAutoInc;
var Obj: TTestAutoIncContact; SelName: String;
begin
  Obj:=TTestAutoIncContact.Create;
  try
    Obj.Name:='Alice'; Obj.FState:=opsNew;
    OPManager.Save(Obj);
    AssertEquals('Initial ID', 1, Obj.ID);
    Obj.Name:='Updated'; Obj.FState:=opsUpdate;
    OPManager.Save(Obj);
    DirectSelectAutoInc(1, SelName);
    AssertEquals('Name updated', 'Updated', SelName);
    AssertEquals('ID unchanged', 1, Obj.ID);
  finally Obj.Free; end;
end;

{ ===== TTestUUIDCommon ===== }

function TTestUUIDCommon.CountUUIDRows: Integer;
begin Result:=SQLCount(FConn, 'TestUUID'); end;

procedure TTestUUIDCommon.DirectSelectUUID(const aID: String; out aName: String);
begin aName:=SQLQueryStr(FConn, 'SELECT Name FROM TestUUID WHERE ID=:ID', 'ID', aID, 'Name'); end;

procedure TTestUUIDCommon.SetUp;
var Conn: TOPConnection; Field: TOPFieldMap;
begin
  inherited SetUp;
  CreateUUIDTable;
  OPRegistry.RegisterPersister(TTestUUIDContact, TRTTIPersister);
  OPRegistry.FindPersister(TTestUUIDContact).TableName:='TestUUID';
  Field:=OPRegistry.FindPersister(TTestUUIDContact).FieldMap.FindByPropertyName('ID');
  Field.Options:=Field.Options + [foUUID, foInKey];
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn; Conn.IsDefault:=True;
end;

procedure TTestUUIDCommon.TearDown;
begin
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  inherited TearDown;
end;

procedure TTestUUIDCommon.TestUUIDInsertPopulatesID;
var Obj: TTestUUIDContact;
begin
  Obj:=TTestUUIDContact.Create;
  try
    Obj.ID:=''; Obj.Name:='Alice'; Obj.FState:=opsNew;
    OPManager.Save(Obj);
    AssertTrue('ID populated (not empty)', Obj.ID <> '');
    AssertEquals('UUID length (36 chars)', 36, Length(Obj.ID));
    AssertEquals('Row count', 1, CountUUIDRows);
  finally Obj.Free; end;
end;

procedure TTestUUIDCommon.TestUUIDInsertMultipleUnique;
var Obj1, Obj2, Obj3: TTestUUIDContact;
begin
  Obj1:=TTestUUIDContact.Create; Obj2:=TTestUUIDContact.Create; Obj3:=TTestUUIDContact.Create;
  try
    Obj1.Name:='Alice'; Obj1.FState:=opsNew;
    Obj2.Name:='Bob';   Obj2.FState:=opsNew;
    Obj3.Name:='Carol'; Obj3.FState:=opsNew;
    OPManager.Save(Obj1); OPManager.Save(Obj2); OPManager.Save(Obj3);
    AssertTrue('IDs are unique (1<>2)', Obj1.ID <> Obj2.ID);
    AssertTrue('IDs are unique (2<>3)', Obj2.ID <> Obj3.ID);
    AssertEquals('Row count', 3, CountUUIDRows);
  finally Obj3.Free; Obj2.Free; Obj1.Free; end;
end;

procedure TTestUUIDCommon.TestUUIDInsertPreservesOtherFields;
var Obj: TTestUUIDContact; SelName: String;
begin
  Obj:=TTestUUIDContact.Create;
  try
    Obj.ID:=''; Obj.Name:='Alice'; Obj.FState:=opsNew;
    OPManager.Save(Obj);
    DirectSelectUUID(Obj.ID, SelName);
    AssertEquals('Name preserved', 'Alice', SelName);
  finally Obj.Free; end;
end;

procedure TTestUUIDCommon.TestUUIDInsertLoadRoundTrip;
var Obj, Loaded: TTestUUIDContact; SavedID: String;
begin
  Obj:=TTestUUIDContact.Create;
  try
    Obj.ID:=''; Obj.Name:='Alice'; Obj.FState:=opsNew;
    OPManager.Save(Obj); SavedID:=Obj.ID;
  finally Obj.Free; end;
  Loaded:=TTestUUIDContact.Create;
  try
    Loaded.ID:=SavedID; Loaded.FState:=opsKey;
    OPManager.Load(Loaded);
    AssertEquals('Loaded ID matches', SavedID, Loaded.ID);
    AssertEquals('Loaded Name matches', 'Alice', Loaded.Name);
  finally Loaded.Free; end;
end;

procedure TTestUUIDCommon.TestUUIDInsertPreservesPresetID;
var Obj: TTestUUIDContact; SelName: String;
const PresetID = 'my-custom-uuid-value-already-set-ok';
begin
  Obj:=TTestUUIDContact.Create;
  try
    Obj.ID:=PresetID; Obj.Name:='Bob'; Obj.FState:=opsNew;
    OPManager.Save(Obj);
    AssertEquals('Pre-set ID preserved', PresetID, Obj.ID);
    DirectSelectUUID(PresetID, SelName);
    AssertEquals('Name stored correctly', 'Bob', SelName);
  finally Obj.Free; end;
end;

{ ===== TTestCustomGenCommon ===== }

function TTestCustomGenCommon.CountCustomGenRows: Integer;
begin Result:=SQLCount(FConn, 'TestCustomGen'); end;

procedure TTestCustomGenCommon.DirectSelectCustomGen(const aID: String; out aName: String);
begin aName:=SQLQueryStr(FConn, 'SELECT Name FROM TestCustomGen WHERE ID=:ID', 'ID', aID, 'Name'); end;

procedure TTestCustomGenCommon.GenerateInvoiceID(aInstance: TObject; aField: TOPFieldMap; aConnection: TObject);
begin
  Inc(FNextID);
  SetStrProp(aInstance, aField.PropertyName, Format('INV-%4.4d', [FNextID]));
end;

procedure TTestCustomGenCommon.SetUp;
var Conn: TOPConnection; Field: TOPFieldMap;
begin
  inherited SetUp;
  FNextID:=0;
  CreateCustomGenTable;
  OPRegistry.RegisterPersister(TTestCustomGenContact, TRTTIPersister);
  OPRegistry.FindPersister(TTestCustomGenContact).TableName:='TestCustomGen';
  Field:=OPRegistry.FindPersister(TTestCustomGenContact).FieldMap.FindByPropertyName('ID');
  Field.Options:=Field.Options + [foInKey];
  Field.OnGenerateValue:=@Self.GenerateInvoiceID;
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn; Conn.IsDefault:=True;
end;

procedure TTestCustomGenCommon.TearDown;
begin
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  inherited TearDown;
end;

procedure TTestCustomGenCommon.TestCustomGenInsertPopulatesID;
var Obj: TTestCustomGenContact;
begin
  Obj:=TTestCustomGenContact.Create;
  try
    Obj.ID:=''; Obj.Name:='Alice'; Obj.FState:=opsNew;
    OPManager.Save(Obj);
    AssertEquals('ID populated by custom generator', 'INV-0001', Obj.ID);
    AssertEquals('Row count', 1, CountCustomGenRows);
  finally Obj.Free; end;
end;

procedure TTestCustomGenCommon.TestCustomGenInsertMultipleSequential;
var Obj1, Obj2, Obj3: TTestCustomGenContact;
begin
  Obj1:=TTestCustomGenContact.Create; Obj2:=TTestCustomGenContact.Create; Obj3:=TTestCustomGenContact.Create;
  try
    Obj1.Name:='Alice'; Obj1.FState:=opsNew;
    Obj2.Name:='Bob';   Obj2.FState:=opsNew;
    Obj3.Name:='Carol'; Obj3.FState:=opsNew;
    OPManager.Save(Obj1); OPManager.Save(Obj2); OPManager.Save(Obj3);
    AssertEquals('First ID', 'INV-0001', Obj1.ID);
    AssertEquals('Second ID', 'INV-0002', Obj2.ID);
    AssertEquals('Third ID', 'INV-0003', Obj3.ID);
  finally Obj3.Free; Obj2.Free; Obj1.Free; end;
end;

procedure TTestCustomGenCommon.TestCustomGenInsertPreservesOtherFields;
var Obj: TTestCustomGenContact; SelName: String;
begin
  Obj:=TTestCustomGenContact.Create;
  try
    Obj.ID:=''; Obj.Name:='Alice'; Obj.FState:=opsNew;
    OPManager.Save(Obj);
    DirectSelectCustomGen(Obj.ID, SelName);
    AssertEquals('Name preserved', 'Alice', SelName);
  finally Obj.Free; end;
end;

procedure TTestCustomGenCommon.TestCustomGenInsertLoadRoundTrip;
var Obj, Loaded: TTestCustomGenContact; SavedID: String;
begin
  Obj:=TTestCustomGenContact.Create;
  try
    Obj.ID:=''; Obj.Name:='Alice'; Obj.FState:=opsNew;
    OPManager.Save(Obj); SavedID:=Obj.ID;
  finally Obj.Free; end;
  Loaded:=TTestCustomGenContact.Create;
  try
    Loaded.ID:=SavedID; Loaded.FState:=opsKey;
    OPManager.Load(Loaded);
    AssertEquals('Loaded ID matches', SavedID, Loaded.ID);
    AssertEquals('Loaded Name matches', 'Alice', Loaded.Name);
  finally Loaded.Free; end;
end;

{ ===== TTestLoadChildrenCommon ===== }

procedure TTestLoadChildrenCommon.DirectInsertContact(aID: Integer; const aName: String);
begin SQLInsert(FConn, 'INSERT INTO TestContactWithChildren (ID, Name) VALUES (:ID, :Name)', ['ID', 'Name'], [aID, aName]); end;

procedure TTestLoadChildrenCommon.DirectInsertAddress(aID, aContactID: Integer; const aStreet: String);
begin SQLInsert(FConn, 'INSERT INTO TestAddress (ID, ContactID, Street) VALUES (:ID, :ContactID, :Street)', ['ID', 'ContactID', 'Street'], [aID, aContactID, aStreet]); end;

procedure TTestLoadChildrenCommon.LoadChildrenNoRelations;
var Obj: TTestMgrSaveContact;
begin Obj:=TTestMgrSaveContact.Create; try OPManager.LoadChildren(Obj, 'SomeList'); finally Obj.Free; end; end;

procedure TTestLoadChildrenCommon.LoadChildrenAmbiguous;
var Obj: TTestContactWithChildren; L: TObjectList;
begin
  Obj:=TTestContactWithChildren.Create; L:=TObjectList.Create(True);
  try Obj.ID:=1; OPManager.LoadChildren(Obj, L); finally L.Free; Obj.Free; end;
end;

procedure TTestLoadChildrenCommon.SetUp;
var Conn: TOPConnection; Def: TPersisterDef; Idx: Integer;
begin
  inherited SetUp;
  CreateLoadChildrenTables;
  OPRegistry.RegisterPersister(TTestContactWithChildren, TRTTIPersister);
  Def:=OPRegistry.FindPersister(TTestContactWithChildren);
  Idx:=Def.FieldMap.IndexOfPropertyName('AddressList');
  if Idx >= 0 then Def.FieldMap.Delete(Idx);
  OPRegistry.RegisterPersister(TTestAddress, TRTTIPersister);
  OPRelationRegistry.RegisterRelationship(TTestContactWithChildren, TTestAddress, 'ContactID', False, 'AddressList');
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn; Conn.IsDefault:=True;
end;

procedure TTestLoadChildrenCommon.TearDown;
begin
  TOPManager.ClearInstance;
  TOPRelationRegistry.ClearInstance;
  TOPRegistry.ClearInstance;
  inherited TearDown;
end;

procedure TTestLoadChildrenCommon.TestLoadChildrenByPropertyName;
var Obj: TTestContactWithChildren; Addr: TTestAddress; IPS: IOPPersistenceState;
begin
  DirectInsertContact(1, 'Alice');
  DirectInsertAddress(10, 1, 'Main St'); DirectInsertAddress(11, 1, 'Oak Ave');
  Obj:=TTestContactWithChildren.Create;
  try
    Obj.ID:=1; Obj.FState:=opsKey; OPManager.Load(Obj);
    OPManager.LoadChildren(Obj, 'AddressList');
    AssertEquals('AddressList count', 2, Obj.AddressList.Count);
    Addr:=TTestAddress(Obj.AddressList[0]);
    AssertTrue('Child is TTestAddress', Addr is TTestAddress);
    AssertTrue('Child has opsClean', Supports(Addr, IOPPersistenceState, IPS) and (IPS.PersistenceState = opsClean));
  finally Obj.Free; end;
end;

procedure TTestLoadChildrenCommon.TestLoadChildrenExplicitClassExternalList;
var Obj: TTestContactWithChildren; ExtList: TObjectList;
begin
  DirectInsertContact(1, 'Bob');
  DirectInsertAddress(20, 1, 'Elm St'); DirectInsertAddress(21, 1, 'Pine Rd');
  Obj:=TTestContactWithChildren.Create; ExtList:=TObjectList.Create(True);
  try
    Obj.ID:=1; Obj.FState:=opsKey; OPManager.Load(Obj);
    OPManager.LoadChildren(Obj, TTestAddress, ExtList);
    AssertEquals('External list count', 2, ExtList.Count);
  finally ExtList.Free; Obj.Free; end;
end;

procedure TTestLoadChildrenCommon.TestLoadAllChildren;
var Obj: TTestContactWithChildren;
begin
  DirectInsertContact(1, 'Carol'); DirectInsertAddress(30, 1, 'Birch Ln');
  Obj:=TTestContactWithChildren.Create;
  try
    Obj.ID:=1; Obj.FState:=opsKey; OPManager.Load(Obj);
    OPManager.LoadAllChildren(Obj);
    AssertEquals('AddressList count after LoadAll', 1, Obj.AddressList.Count);
    AssertEquals('Street', 'Birch Ln', TTestAddress(Obj.AddressList[0]).Street);
  finally Obj.Free; end;
end;

procedure TTestLoadChildrenCommon.TestLoadChildrenRegisteredPropertyName;
var Obj: TTestContactWithChildren; Conn: TOPConnection;
begin
  TOPRelationRegistry.ClearInstance;
  OPRelationRegistry.RegisterRelationship(TTestContactWithChildren, TTestAddress, 'ContactID', False, 'AddressList');
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn; Conn.IsDefault:=True;
  DirectInsertContact(1, 'Dave'); DirectInsertAddress(40, 1, 'Cedar Dr');
  Obj:=TTestContactWithChildren.Create;
  try
    Obj.ID:=1; Obj.FState:=opsKey; OPManager.Load(Obj);
    OPManager.LoadChildren(Obj, 'AddressList');
    AssertEquals('Registered property populated', 1, Obj.AddressList.Count);
  finally Obj.Free; end;
end;

procedure TTestLoadChildrenCommon.TestLoadChildrenNoRelationsRaises;
begin
  AssertException('No relations raises', EOPF, @LoadChildrenNoRelations);
end;

procedure TTestLoadChildrenCommon.TestLoadChildrenEmptyResult;
var Obj: TTestContactWithChildren;
begin
  DirectInsertContact(1, 'Eve');
  Obj:=TTestContactWithChildren.Create;
  try
    Obj.ID:=1; Obj.FState:=opsKey; OPManager.Load(Obj);
    OPManager.LoadChildren(Obj, 'AddressList');
    AssertEquals('Empty AddressList', 0, Obj.AddressList.Count);
  finally Obj.Free; end;
end;

{ ===== TTestInterfacePersisterCommon ===== }

function TTestInterfacePersisterCommon.CountProductRows: Integer;
begin Result:=SQLCount(FConn, 'TestIntfProduct'); end;

procedure TTestInterfacePersisterCommon.DirectSelectProduct(aID: Integer; out aName: String; out aPrice: Double);
begin
  aName:=SQLQueryStr(FConn, 'SELECT Name, Price FROM TestIntfProduct WHERE ID=:ID', 'ID', aID, 'Name');
  aPrice:=SQLQueryFloat(FConn, 'SELECT Name, Price FROM TestIntfProduct WHERE ID=:ID', 'ID', aID, 'Price');
end;

procedure TTestInterfacePersisterCommon.SetUp;
var Conn: TOPConnection;
begin
  inherited SetUp;
  CreateProductTable;
  OPRegistry.RegisterPersister(TTestIntfProduct, TInterfacePersister);
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn; Conn.IsDefault:=True;
end;

procedure TTestInterfacePersisterCommon.TearDown;
begin
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  DeleteTables(['TestIntfProduct']);
  inherited TearDown;
end;

procedure TTestInterfacePersisterCommon.TestSaveAndLoadInterfacePersister;
var Obj, Loaded: TTestIntfProduct; SelName: String; SelPrice: Double;
begin
  Obj:=TTestIntfProduct.Create;
  try
    Obj.FName:='Widget'; Obj.FPrice:=19.99; Obj.FState:=opsNew;
    OPManager.Save(Obj);
    AssertEquals('Row count after save', 1, CountProductRows);
    AssertTrue('ID assigned by auto-inc', Obj.ID > 0);
    DirectSelectProduct(Obj.ID, SelName, SelPrice);
    AssertEquals('Saved name', 'Widget', SelName);
    AssertEquals('Saved price', 19.99, SelPrice);
    Loaded:=TTestIntfProduct.Create;
    try
      Loaded.FID:=Obj.ID; Loaded.FState:=opsKey;
      OPManager.Load(Loaded);
      AssertEquals('Loaded name', 'Widget', Loaded.Name);
      AssertEquals('Loaded price', 19.99, Loaded.Price);
    finally Loaded.Free; end;
  finally Obj.Free; end;
end;

procedure TTestInterfacePersisterCommon.TestUpdateInterfacePersister;
var Obj, Loaded: TTestIntfProduct;
begin
  Obj:=TTestIntfProduct.Create;
  try
    Obj.FName:='Gadget'; Obj.FPrice:=9.99; Obj.FState:=opsNew;
    OPManager.Save(Obj);
    Obj.FName:='SuperGadget'; Obj.FPrice:=29.99; Obj.FState:=opsUpdate;
    OPManager.Save(Obj);
    Loaded:=TTestIntfProduct.Create;
    try
      Loaded.FID:=Obj.ID; Loaded.FState:=opsKey;
      OPManager.Load(Loaded);
      AssertEquals('Updated name', 'SuperGadget', Loaded.Name);
      AssertEquals('Updated price', 29.99, Loaded.Price);
    finally Loaded.Free; end;
  finally Obj.Free; end;
end;

procedure TTestInterfacePersisterCommon.TestDeleteInterfacePersister;
var Obj: TTestIntfProduct;
begin
  Obj:=TTestIntfProduct.Create;
  try
    Obj.FName:='Doomed'; Obj.FPrice:=5.00; Obj.FState:=opsNew;
    OPManager.Save(Obj);
    AssertEquals('Row count after save', 1, CountProductRows);
    Obj.FState:=opsClean;
    OPManager.Delete(Obj);
    AssertEquals('Row count after delete', 0, CountProductRows);
  finally Obj.Free; end;
end;

{ ===== TTestExplicitTransactionCommon ===== }

function TTestExplicitTransactionCommon.CountContactRows: Integer;
begin Result:=SQLCount(FConn, 'TestMgrSaveContact'); end;

procedure TTestExplicitTransactionCommon.DirectInsertContact(aID: Integer; const aName: String);
begin SQLInsert(FConn, 'INSERT INTO TestMgrSaveContact (ID, Name) VALUES (:ID, :Name)', ['ID', 'Name'], [aID, aName]); end;

procedure TTestExplicitTransactionCommon.DirectSelectContact(aID: Integer; out aName: String);
begin aName:=SQLQueryStr(FConn, 'SELECT Name FROM TestMgrSaveContact WHERE ID=:ID', 'ID', aID, 'Name'); end;

procedure TTestExplicitTransactionCommon.SetUp;
var Conn: TOPConnection;
begin
  inherited SetUp;
  CreateContactTable;
  OPRegistry.RegisterPersister(TTestMgrSaveContact, TRTTIPersister);
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn; Conn.IsDefault:=True;
end;

procedure TTestExplicitTransactionCommon.TearDown;
begin
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  DeleteTables(['TestMgrSaveContact']);
  inherited TearDown;
end;

procedure TTestExplicitTransactionCommon.TestExplicitCommitPersists;
var Obj: TTestMgrSaveContact;
begin
  OPManager.BeginTransaction;
  Obj:=TTestMgrSaveContact.Create;
  try Obj.ID:=1; Obj.Name:='Alice'; Obj.FState:=opsNew; OPManager.Save(Obj); finally Obj.Free; end;
  Obj:=TTestMgrSaveContact.Create;
  try Obj.ID:=2; Obj.Name:='Bob'; Obj.FState:=opsNew; OPManager.Save(Obj); finally Obj.Free; end;
  OPManager.Commit;
  AssertEquals('Both rows persisted', 2, CountContactRows);
end;

procedure TTestExplicitTransactionCommon.TestExplicitRollbackReverts;
var Obj: TTestMgrSaveContact;
begin
  OPManager.BeginTransaction;
  Obj:=TTestMgrSaveContact.Create;
  try Obj.ID:=1; Obj.Name:='Alice'; Obj.FState:=opsNew; OPManager.Save(Obj); finally Obj.Free; end;
  Obj:=TTestMgrSaveContact.Create;
  try Obj.ID:=2; Obj.Name:='Bob'; Obj.FState:=opsNew; OPManager.Save(Obj); finally Obj.Free; end;
  OPManager.Rollback;
  AssertEquals('Zero rows after rollback', 0, CountContactRows);
end;

procedure TTestExplicitTransactionCommon.TestSaveAfterRolledBackSaveReachesDatabase;
var
  lTracker: TOPSnapshotManager;
  lObj: TTestMgrSaveContact;
  lNames: TUTF8StringArray;
  lStored: String;
begin
  lTracker:=TOPSnapshotManager.Create(Nil);
  try
    lTracker.Mode:=smExplicit;
    lTracker.RegisterClass(TTestMgrSaveContact);
    AssertTrue('Tracker attached to the manager under test', Assigned(OPManager.ChangeTracker));
    lObj:=TTestMgrSaveContact.Create;
    try
      lObj.ID:=1;
      lObj.Name:='Alice';
      lObj.FState:=opsNew;
      OPManager.Save(lObj);
      DirectSelectContact(1, lStored);
      AssertEquals('Row inserted', 'Alice', lStored);
      AssertTrue('Tracked after the first save', OPManager.GetDirtyFields(lObj, lNames));
      OPManager.BeginTransaction;
      lObj.Name:='Bob';
      lObj.FState:=opsUpdate;
      OPManager.Save(lObj);
      OPManager.Rollback;
      DirectSelectContact(1, lStored);
      AssertEquals('Rollback restored the stored value', 'Alice', lStored);
      AssertFalse('Untracked after the rollback', OPManager.GetDirtyFields(lObj, lNames));
      lObj.FState:=opsUpdate;
      OPManager.Save(lObj);
      DirectSelectContact(1, lStored);
      AssertEquals('Save after a rolled-back save reaches the database', 'Bob', lStored);
    finally
      lObj.Free;
    end;
  finally
    lTracker.Free;
  end;
end;

{ ===== TTestCustomSQLCommon ===== }

procedure TTestCustomSQLCommon.DirectInsertContact(aID: Integer; const aName: String; aDeptID: Integer);
begin SQLInsert(FConn, 'INSERT INTO TestCustomSQLContact (ID, Name, DeptID) VALUES (:ID, :Name, :DeptID)', ['ID', 'Name', 'DeptID'], [aID, aName, aDeptID]); end;

procedure TTestCustomSQLCommon.DirectInsertDept(aID: Integer; const aDeptName: String);
begin SQLInsert(FConn, 'INSERT INTO TestCustomSQLDept (ID, DeptName) VALUES (:ID, :DeptName)', ['ID', 'DeptName'], [aID, aDeptName]); end;

procedure TTestCustomSQLCommon.DoLoadSQLNotFound;
var Obj: TTestMgrSaveContact;
begin
  Obj:=TTestMgrSaveContact.Create;
  try OPManager.LoadSQL(Obj, 'SELECT ID, Name FROM TestCustomSQLContact WHERE ID = :ID', ['ID', 999]);
  finally Obj.Free; end;
end;

procedure TTestCustomSQLCommon.SetUp;
var Conn: TOPConnection;
begin
  inherited SetUp;
  CreateCustomSQLTables;
  OPRegistry.RegisterPersister(TTestMgrSaveContact, TRTTIPersister);
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn; Conn.IsDefault:=True;
end;

procedure TTestCustomSQLCommon.TearDown;
begin
  TOPManager.ClearInstance;
  TOPListRegistry.ClearInstance;
  TOPRegistry.ClearInstance;
  inherited TearDown;
end;

procedure TTestCustomSQLCommon.TestLoadSQLSingleObject;
var Obj: TTestMgrSaveContact;
begin
  DirectInsertContact(1, 'Alice', 10);
  Obj:=TTestMgrSaveContact.Create;
  try
    OPManager.LoadSQL(Obj, 'SELECT ID, Name FROM TestCustomSQLContact WHERE ID = :ID', ['ID', 1]);
    AssertEquals('ID loaded', 1, Obj.ID);
    AssertEquals('Name loaded', 'Alice', Obj.Name);
  finally Obj.Free; end;
end;

procedure TTestCustomSQLCommon.TestLoadSQLWithJoin;
var Obj: TTestMgrSaveContact;
begin
  DirectInsertDept(10, 'Engineering'); DirectInsertDept(20, 'Sales');
  DirectInsertContact(1, 'Alice', 10); DirectInsertContact(2, 'Bob', 20);
  Obj:=TTestMgrSaveContact.Create;
  try
    OPManager.LoadSQL(Obj,
      'SELECT c.ID, c.Name FROM TestCustomSQLContact c JOIN TestCustomSQLDept d ON c.DeptID = d.ID WHERE d.DeptName = :dept',
      ['dept', 'Engineering']);
    AssertEquals('Correct contact from JOIN', 1, Obj.ID);
  finally Obj.Free; end;
end;

procedure TTestCustomSQLCommon.TestLoadSQLList;
var List: TObjectList;
begin
  DirectInsertContact(1, 'Alice', 10); DirectInsertContact(2, 'Bob', 10); DirectInsertContact(3, 'Carol', 20);
  List:=TObjectList.Create(True);
  try
    OPManager.LoadSQL(List, TTestMgrSaveContact,
      'SELECT ID, Name FROM TestCustomSQLContact WHERE DeptID = :dept', ['dept', 10]);
    AssertEquals('List count', 2, List.Count);
  finally List.Free; end;
end;

procedure TTestCustomSQLCommon.TestLoadSQLUnmappedColumnsIgnored;
var Obj: TTestMgrSaveContact;
begin
  DirectInsertContact(1, 'Alice', 10);
  Obj:=TTestMgrSaveContact.Create;
  try
    OPManager.LoadSQL(Obj,
      'SELECT ID, Name, DeptID AS ExtraCol FROM TestCustomSQLContact WHERE ID = :ID', ['ID', 1]);
    AssertEquals('ID loaded', 1, Obj.ID);
    AssertEquals('Name loaded', 'Alice', Obj.Name);
  finally Obj.Free; end;
end;

procedure TTestCustomSQLCommon.TestLoadSQLNoParams;
var Obj: TTestMgrSaveContact;
begin
  DirectInsertContact(1, 'Alice', 10);
  Obj:=TTestMgrSaveContact.Create;
  try
    OPManager.LoadSQL(Obj, 'SELECT ID, Name FROM TestCustomSQLContact WHERE ID = 1');
    AssertEquals('ID loaded', 1, Obj.ID);
  finally Obj.Free; end;
end;

procedure TTestCustomSQLCommon.TestLoadSQLListNoResults;
var List: TObjectList;
begin
  List:=TObjectList.Create(True);
  try
    OPManager.LoadSQL(List, TTestMgrSaveContact, 'SELECT ID, Name FROM TestCustomSQLContact WHERE 1=0');
    AssertEquals('Empty list', 0, List.Count);
  finally List.Free; end;
end;

procedure TTestCustomSQLCommon.TestLoadSQLNotFoundRaises;
begin
  AssertException('LoadSQL not found raises EOPF', EOPF, @DoLoadSQLNotFound);
end;

procedure TTestCustomSQLCommon.TestLoadSQLWithTParams;
var Obj: TTestMgrSaveContact; Params: TParams;
begin
  DirectInsertContact(1, 'Alice', 10);
  Obj:=TTestMgrSaveContact.Create;
  try
    Params:=TParams.Create;
    try
      Params.CreateParam(ftInteger, 'ID', ptInput).AsInteger:=1;
      OPManager.LoadSQL(Obj, 'SELECT ID, Name FROM TestCustomSQLContact WHERE ID = :ID', Params);
      AssertEquals('ID loaded', 1, Obj.ID);
    finally Params.Free; end;
  finally Obj.Free; end;
end;

procedure TTestCustomSQLCommon.TestLoadSQLListWithTParams;
var List: TObjectList; Params: TParams;
begin
  DirectInsertContact(1, 'Alice', 10); DirectInsertContact(2, 'Bob', 10);
  List:=TObjectList.Create(True);
  try
    Params:=TParams.Create;
    try
      Params.CreateParam(ftInteger, 'dept', ptInput).AsInteger:=10;
      OPManager.LoadSQL(List, TTestMgrSaveContact,
        'SELECT ID, Name FROM TestCustomSQLContact WHERE DeptID = :dept', Params);
      AssertEquals('List count', 2, List.Count);
    finally Params.Free; end;
  finally List.Free; end;
end;

procedure TTestCustomSQLCommon.TestLoadSQLListMultipleParams;
var List: TObjectList;
begin
  DirectInsertContact(1, 'Alice', 10); DirectInsertContact(2, 'Bob', 10); DirectInsertContact(3, 'Carol', 20);
  List:=TObjectList.Create(True);
  try
    OPManager.LoadSQL(List, TTestMgrSaveContact,
      'SELECT ID, Name FROM TestCustomSQLContact WHERE DeptID = :dept AND Name LIKE :pattern',
      ['dept', 10, 'pattern', 'A%']);
    AssertEquals('List count', 1, List.Count);
    AssertEquals('Name', 'Alice', TTestMgrSaveContact(List[0]).Name);
  finally List.Free; end;
end;

{ ===== TTestExtendedTypesCRUDCommon ===== }

procedure TTestExtendedTypesCRUDCommon.SetUp;
var Conn: TOPConnection;
begin
  inherited SetUp;
  CreateExtendedTable;
  OPRegistry.RegisterPersister(TTestExtendedTypeContact, TRTTIPersister);
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn; Conn.IsDefault:=True;
end;

procedure TTestExtendedTypesCRUDCommon.TearDown;
begin
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  inherited TearDown;
end;

procedure TTestExtendedTypesCRUDCommon.TestSaveLoadSetEnum;
var Obj, Loaded: TTestExtendedTypeContact;
begin
  Obj:=TTestExtendedTypeContact.Create;
  try
    Obj.ID:=1; Obj.Color:=ecGreen; Obj.Flags:=[efBold, efUnderline]; Obj.Extra:=Integer(99);
    OPManager.Save(Obj);
  finally Obj.Free; end;
  Loaded:=TTestExtendedTypeContact.Create;
  try
    Loaded.ID:=1; Loaded.FState:=opsKey; OPManager.Load(Loaded);
    AssertEquals('Color round-trip', Ord(ecGreen), Ord(Loaded.Color));
    AssertTrue('efBold in Flags', efBold in Loaded.Flags);
    AssertTrue('efUnderline in Flags', efUnderline in Loaded.Flags);
    AssertFalse('efItalic not in Flags', efItalic in Loaded.Flags);
  finally Loaded.Free; end;
end;

procedure TTestExtendedTypesCRUDCommon.TestSaveLoadVariant;
var Obj, Loaded: TTestExtendedTypeContact;
begin
  Obj:=TTestExtendedTypeContact.Create;
  try
    Obj.ID:=2; Obj.Color:=ecRed; Obj.Flags:=[]; Obj.Extra:='test value';
    OPManager.Save(Obj);
  finally Obj.Free; end;
  Loaded:=TTestExtendedTypeContact.Create;
  try
    Loaded.ID:=2; Loaded.FState:=opsKey; OPManager.Load(Loaded);
    AssertEquals('Variant string round-trip', 'test value', String(Loaded.Extra));
  finally Loaded.Free; end;
end;

{ ===== TTestErrorDiagnosticsCommon ===== }

procedure TTestErrorDiagnosticsCommon.SetUp;
var Conn: TOPConnection;
begin
  inherited SetUp;
  CreateContactTable;
  OPRegistry.RegisterPersister(TTestMgrSaveContact, TRTTIPersister);
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn; Conn.IsDefault:=True;
  FObj:=Nil;
end;

procedure TTestErrorDiagnosticsCommon.TearDown;
begin
  FreeAndNil(FObj);
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  inherited TearDown;
end;

procedure TTestErrorDiagnosticsCommon.TestLoadNotFoundMessage;
begin
  FObj:=TTestMgrSaveContact.Create; FObj.ID:=999;
  try
    OPManager.Load(FObj); Fail('Expected EOPF');
  except
    on E: EOPF do
    begin
      AssertTrue('Message contains class name', Pos('TTestMgrSaveContact', E.Message) > 0);
      AssertTrue('Message contains reason', Pos('No record found', E.Message) > 0);
    end;
  end;
end;

procedure TTestErrorDiagnosticsCommon.TestSaveUnsupportedStateMessage;
begin
  FObj:=TTestMgrSaveContact.Create; FObj.FState:=opsClean;
  try
    OPManager.Save(FObj, opsClean); Fail('Expected EOPF');
  except
    on E: EOPF do
    begin
      AssertTrue('Message contains class name', Pos('TOPManager', E.Message) > 0);
      AssertTrue('Message contains reason', Pos('Unsupported persistence state for Save', E.Message) > 0);
    end;
  end;
end;

{ ===== TTestNullableRoundTripCommon ===== }

procedure TTestNullableRoundTripCommon.SetUp;
begin
  inherited SetUp;
  FPersister:=TRTTIPersister.Create;
  FMap:=TOPFieldMapList.Create(TOPFieldMap);
end;

procedure TTestNullableRoundTripCommon.TearDown;
begin
  FreeAndNil(FMap); FreeAndNil(FPersister);
  inherited TearDown;
end;

function TTestNullableRoundTripCommon.MakeQuery(const aSQL: string): TSQLQuery;
begin
  Result:=TSQLQuery.Create(nil);
  Result.Database:=FConn; Result.Transaction:=FConn.Transaction; Result.SQL.Text:=aSQL;
end;

procedure TTestNullableRoundTripCommon.TestNullableRoundTrip;
var Obj: TTestNullableIntContact; Q: TSQLQuery; FM_ID, FM_Name, FM_Age: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestNullableIntContact, FMap);
  CreateNullableTable;
  Obj:=TTestNullableIntContact.Create;
  try
    Obj.ID:=1; Obj.Name:='Alice'; Obj.Age:=42; Obj.FAgeStored:=False;
    FM_ID:=FMap.FindByPropertyName('ID');
    FM_Name:=FMap.FindByPropertyName('Name');
    FM_Age:=FMap.FindByPropertyName('Age');
    Q:=MakeQuery('INSERT INTO TestNullableInt (ID, Name, Age) VALUES (:ID, :Name, :Age)');
    try
      if not FConn.Transaction.Active then FConn.Transaction.StartTransaction;
      FPersister.PersistProperty(Obj, FM_ID, Q.Params.ParamByName('ID'));
      FPersister.PersistProperty(Obj, FM_Name, Q.Params.ParamByName('Name'));
      FPersister.PersistProperty(Obj, FM_Age, Q.Params.ParamByName('Age'));
      AssertTrue('Age param is null', Q.Params.ParamByName('Age').IsNull);
      Q.ExecSQL; FConn.Transaction.Commit;
    finally Q.Free; end;
  finally Obj.Free; end;
  Q:=MakeQuery('SELECT Age FROM TestNullableInt WHERE ID=1');
  try
    if not FConn.Transaction.Active then FConn.Transaction.StartTransaction;
    Q.Open; AssertTrue('Age is NULL in DB', Q.FieldByName('Age').IsNull); Q.Close;
  finally Q.Free; end;
  Obj:=TTestNullableIntContact.Create;
  try
    Obj.Age:=99; Obj.FAgeStored:=True;
    Q:=MakeQuery('SELECT ID, Name, Age FROM TestNullableInt WHERE ID=1');
    try
      if not FConn.Transaction.Active then FConn.Transaction.StartTransaction;
      Q.Open;
      FPersister.ApplyProperty(Obj, FM_ID, Q.FieldByName('ID'));
      FPersister.ApplyProperty(Obj, FM_Name, Q.FieldByName('Name'));
      FPersister.ApplyProperty(Obj, FM_Age, Q.FieldByName('Age'));
      AssertEquals('ID loaded', 1, Obj.ID);
      AssertFalse('FAgeStored is False after loading NULL', Obj.FAgeStored);
      AssertEquals('Age is default (0)', 0, Obj.Age);
      Q.Close;
    finally Q.Free; end;
  finally Obj.Free; end;
end;

{ ===== TTestIOPFNullableRoundTripCommon ===== }

procedure TTestIOPFNullableRoundTripCommon.SetUp;
begin
  inherited SetUp;
  FPersister:=TRTTIPersister.Create;
  FMap:=TOPFieldMapList.Create(TOPFieldMap);
end;

procedure TTestIOPFNullableRoundTripCommon.TearDown;
begin
  FreeAndNil(FMap); FreeAndNil(FPersister);
  inherited TearDown;
end;

function TTestIOPFNullableRoundTripCommon.MakeQuery(const aSQL: string): TSQLQuery;
begin
  Result:=TSQLQuery.Create(nil);
  Result.Database:=FConn; Result.Transaction:=FConn.Transaction; Result.SQL.Text:=aSQL;
end;

procedure TTestIOPFNullableRoundTripCommon.TestIOPFNullableRoundTrip;
var Obj: TTestIntNullableObj; Q: TSQLQuery; FM_ID, FM_Name, FM_Age: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestIntNullableObj, FMap);
  CreateIOPFNullableTable;
  Obj:=TTestIntNullableObj.Create;
  try
    Obj.ID:=1; Obj.Name:='Alice'; Obj.Age:=42; Obj.SetNull('Age');
    FM_ID:=FMap.FindByPropertyName('ID');
    FM_Name:=FMap.FindByPropertyName('Name');
    FM_Age:=FMap.FindByPropertyName('Age');
    Q:=MakeQuery('INSERT INTO TestIntNullableObj (ID, Name, Age) VALUES (:ID, :Name, :Age)');
    try
      if not FConn.Transaction.Active then FConn.Transaction.StartTransaction;
      FPersister.PersistProperty(Obj, FM_ID, Q.Params.ParamByName('ID'));
      FPersister.PersistProperty(Obj, FM_Name, Q.Params.ParamByName('Name'));
      FPersister.PersistProperty(Obj, FM_Age, Q.Params.ParamByName('Age'));
      AssertTrue('Age param is null', Q.Params.ParamByName('Age').IsNull);
      Q.ExecSQL; FConn.Transaction.Commit;
    finally Q.Free; end;
  finally Obj.Free; end;
  Q:=MakeQuery('SELECT Age FROM TestIntNullableObj WHERE ID=1');
  try
    if not FConn.Transaction.Active then FConn.Transaction.StartTransaction;
    Q.Open; AssertTrue('Age is NULL in DB', Q.FieldByName('Age').IsNull); Q.Close;
  finally Q.Free; end;
  Obj:=TTestIntNullableObj.Create;
  try
    Obj.Age:=99;
    Q:=MakeQuery('SELECT ID, Name, Age FROM TestIntNullableObj WHERE ID=1');
    try
      if not FConn.Transaction.Active then FConn.Transaction.StartTransaction;
      Q.Open;
      FPersister.ApplyProperty(Obj, FM_ID, Q.FieldByName('ID'));
      FPersister.ApplyProperty(Obj, FM_Name, Q.FieldByName('Name'));
      FPersister.ApplyProperty(Obj, FM_Age, Q.FieldByName('Age'));
      AssertEquals('ID loaded', 1, Obj.ID);
      AssertTrue('IsNull(Age) is True after loading NULL', Obj.IsNull('Age'));
      AssertEquals('Age is default (0)', 0, Obj.Age);
      Q.Close;
    finally Q.Free; end;
  finally Obj.Free; end;
end;

{ ===== TTestSequenceCommon ===== }

procedure TTestSequenceCommon.SetUp;
begin
  inherited SetUp;
  CreateSequenceTable;
end;

procedure TTestSequenceCommon.TearDown;
begin
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  inherited TearDown;
end;

procedure TTestSequenceCommon.TestSequenceInsertPopulatesID;
var Obj: TTestSequenceContact;
begin
  Obj:=TTestSequenceContact.Create;
  try
    Obj.ID:=0; Obj.Name:='Alice'; Obj.FState:=opsNew;
    OPManager.Save(Obj);
    AssertTrue('ID populated (>0)', Obj.ID > 0);
    AssertEquals('State after save', Ord(opsClean), Ord(Obj.FState));
  finally Obj.Free; end;
end;

procedure TTestSequenceCommon.TestSequenceInsertMultipleSequential;
var Obj1, Obj2, Obj3: TTestSequenceContact;
begin
  Obj1:=TTestSequenceContact.Create; Obj2:=TTestSequenceContact.Create; Obj3:=TTestSequenceContact.Create;
  try
    Obj1.Name:='Alice'; Obj1.FState:=opsNew;
    Obj2.Name:='Bob';   Obj2.FState:=opsNew;
    Obj3.Name:='Carol'; Obj3.FState:=opsNew;
    OPManager.Save(Obj1); OPManager.Save(Obj2); OPManager.Save(Obj3);
    AssertTrue('IDs are sequential', (Obj2.ID = Obj1.ID + 1) and (Obj3.ID = Obj2.ID + 1));
  finally Obj3.Free; Obj2.Free; Obj1.Free; end;
end;

{ ===== TTestDDLExecCommon ===== }

procedure TTestDDLExecCommon.SetUp;
begin
  inherited SetUp;
  FDDLBuilder:=TOPDDLBuilder.Create(nil);
  FDDLBuilder.SQLDialect:=OPDDLBuilder.SQLDialect;
  FDDLBuilder.PKSuffix:='_PK';
  FMap:=TOPFieldMapList.Create(TOPFieldMap);
  CleanupDDLTables;
end;

procedure TTestDDLExecCommon.TearDown;
begin
  FreeAndNil(FMap);
  FreeAndNil(FDDLBuilder);
  CleanupDDLTables;
  inherited TearDown;
end;

procedure TTestDDLExecCommon.TestCreateTableCreatesTable;
begin
  with FMap.AddField('ID', 'ID', [foInKey, foInSelect]) do PropertyKind:=tkInteger;
  with FMap.AddField('Name', 'Name', [foInInsert, foInUpdate, foInSelect]) do PropertyKind:=tkAString;
  AssertFalse('Table should not exist before', utc.opf.integration.base.TableExists(FConn, GetDDLTableName));
  FDDLBuilder.CreateTable(FConn, GetDDLTableName, FMap.AsArray);
  AssertTrue('Table should exist after CreateTable', utc.opf.integration.base.TableExists(FConn, GetDDLTableName));
end;

procedure TTestDDLExecCommon.TestCreateTableIfNotExistsTwice;
begin
  with FMap.AddField('ID', 'ID', [foInKey, foInSelect]) do PropertyKind:=tkInteger;
  with FMap.AddField('Name', 'Name', [foInInsert, foInUpdate, foInSelect]) do PropertyKind:=tkAString;
  FDDLBuilder.CreateTable(FConn, GetDDLTableName, FMap.AsArray, [coIfNotExists]);
  FDDLBuilder.CreateTable(FConn, GetDDLTableName, FMap.AsArray, [coIfNotExists]);
  AssertTrue('Table should exist after two CreateTable calls', utc.opf.integration.base.TableExists(FConn, GetDDLTableName));
end;

procedure TTestDDLExecCommon.TestExecDDLCreatesTable;
begin
  FDDLBuilder.ExecDDL(FConn, GetManualDDL);
  AssertTrue('Table should exist after ExecDDL', utc.opf.integration.base.TableExists(FConn, GetManualTableName));
end;

procedure TTestDDLExecCommon.TestCreateTableAutoCommits;
var Q: TSQLQuery;
begin
  with FMap.AddField('ID', 'ID', [foInKey, foInInsert, foInSelect]) do PropertyKind:=tkInteger;
  with FMap.AddField('Name', 'Name', [foInInsert, foInUpdate, foInSelect]) do PropertyKind:=tkAString;
  FDDLBuilder.CreateTable(FConn, GetDDLTableName, FMap.AsArray);
  { Verify data persists by inserting and reading back using the connection's transaction }
  if FConn.Transaction.Active then
    FConn.Transaction.Rollback;
  FConn.Transaction.StartTransaction;
  Q:=TSQLQuery.Create(nil);
  try
    Q.Database:=FConn; Q.Transaction:=FConn.Transaction;
    Q.SQL.Text:='INSERT INTO ' + GetDDLTableName + ' (ID, Name) VALUES (1, ''Test'')';
    Q.ExecSQL;
  finally Q.Free; end;
  FConn.Transaction.Commit;
  FConn.Transaction.StartTransaction;
  Q:=TSQLQuery.Create(nil);
  try
    Q.Database:=FConn; Q.Transaction:=FConn.Transaction;
    Q.SQL.Text:='SELECT COUNT(*) FROM ' + GetDDLTableName;
    Q.Open;
    AssertEquals('Row persists after CreateTable auto-commit', 1, Q.Fields[0].AsInteger);
    Q.Close;
  finally Q.Free; end;
  FConn.Transaction.Commit;
end;

{ ===== TTestM2MLoadAssociationsCommon ===== }

procedure TTestM2MLoadAssociationsCommon.DirectInsertStudent(aID: Integer; const aName: String);
begin SQLInsert(FConn, 'INSERT INTO TestStudent (ID, Name) VALUES (:ID, :Name)', ['ID', 'Name'], [aID, aName]); end;

procedure TTestM2MLoadAssociationsCommon.DirectInsertCourse(aID: Integer; const aTitle: String);
begin SQLInsert(FConn, 'INSERT INTO TestCourse (ID, Title) VALUES (:ID, :Title)', ['ID', 'Title'], [aID, aTitle]); end;

procedure TTestM2MLoadAssociationsCommon.DirectInsertJunction(aStudentID, aCourseID: Integer);
begin SQLInsert(FConn, 'INSERT INTO student_course (student_id, course_id) VALUES (:SID, :CID)', ['SID', 'CID'], [aStudentID, aCourseID]); end;

procedure TTestM2MLoadAssociationsCommon.SetUp;
var Conn: TOPConnection; Def: TPersisterDef; Idx: Integer;
begin
  inherited SetUp;
  CreateM2MTables;
  OPRegistry.RegisterPersister(TTestM2MStudent, TRTTIPersister);
  Def:=OPRegistry.FindPersister(TTestM2MStudent); Def.TableName:='TestStudent';
  Idx:=Def.FieldMap.IndexOfPropertyName('CourseList'); if Idx >= 0 then Def.FieldMap.Delete(Idx);
  OPRegistry.RegisterPersister(TTestM2MCourse, TRTTIPersister);
  Def:=OPRegistry.FindPersister(TTestM2MCourse); Def.TableName:='TestCourse';
  Idx:=Def.FieldMap.IndexOfPropertyName('StudentList'); if Idx >= 0 then Def.FieldMap.Delete(Idx);
  OPM2MRegistry.RegisterManyToMany(TTestM2MStudent, TTestM2MCourse, 'student_course', 'student_id', 'course_id', False, 'CourseList', 'StudentList');
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn; Conn.IsDefault:=True;
end;

procedure TTestM2MLoadAssociationsCommon.TearDown;
begin
  TOPManager.ClearInstance;
  TOPManyToManyRegistry.ClearInstance;
  TOPRelationRegistry.ClearInstance;
  TOPRegistry.ClearInstance;
  inherited TearDown;
end;

procedure TTestM2MLoadAssociationsCommon.TestLoadAssociationsByPropertyName;
var Student: TTestM2MStudent;
begin
  DirectInsertStudent(1, 'Alice'); DirectInsertCourse(10, 'Math'); DirectInsertCourse(11, 'Physics');
  DirectInsertJunction(1, 10); DirectInsertJunction(1, 11);
  Student:=TTestM2MStudent.Create;
  try
    Student.ID:=1; Student.FState:=opsKey; OPManager.Load(Student);
    OPManager.LoadAssociations(Student, 'CourseList');
    AssertEquals('CourseList count', 2, Student.CourseList.Count);
    AssertTrue('Item is TTestM2MCourse', Student.CourseList[0] is TTestM2MCourse);
  finally Student.Free; end;
end;

procedure TTestM2MLoadAssociationsCommon.TestLoadAssociationsBidirectional;
var Course: TTestM2MCourse;
begin
  DirectInsertStudent(1, 'Alice'); DirectInsertStudent(2, 'Bob');
  DirectInsertCourse(10, 'Math');
  DirectInsertJunction(1, 10); DirectInsertJunction(2, 10);
  Course:=TTestM2MCourse.Create;
  try
    Course.ID:=10; Course.FState:=opsKey; OPManager.Load(Course);
    OPManager.LoadAssociations(Course, 'StudentList');
    AssertEquals('StudentList count', 2, Course.StudentList.Count);
  finally Course.Free; end;
end;

procedure TTestM2MLoadAssociationsCommon.TestLoadAssociationsExplicitClass;
var Student: TTestM2MStudent; ExtList: TObjectList;
begin
  DirectInsertStudent(1, 'Carol'); DirectInsertCourse(10, 'History'); DirectInsertCourse(11, 'Art');
  DirectInsertJunction(1, 10); DirectInsertJunction(1, 11);
  Student:=TTestM2MStudent.Create; ExtList:=TObjectList.Create(True);
  try
    Student.ID:=1; Student.FState:=opsKey; OPManager.Load(Student);
    OPManager.LoadAssociations(Student, TTestM2MCourse, ExtList);
    AssertEquals('External list count', 2, ExtList.Count);
  finally ExtList.Free; Student.Free; end;
end;

procedure TTestM2MLoadAssociationsCommon.TestLoadAssociationsEmptyResult;
var Student: TTestM2MStudent;
begin
  DirectInsertStudent(1, 'Dave');
  Student:=TTestM2MStudent.Create;
  try
    Student.ID:=1; Student.FState:=opsKey; OPManager.Load(Student);
    OPManager.LoadAssociations(Student, 'CourseList');
    AssertEquals('Empty CourseList', 0, Student.CourseList.Count);
  finally Student.Free; end;
end;

procedure TTestM2MLoadAssociationsCommon.TestLoadAssociationsMultiple;
var Student: TTestM2MStudent;
begin
  DirectInsertStudent(1, 'Eve');
  DirectInsertCourse(10, 'Bio'); DirectInsertCourse(11, 'Chem'); DirectInsertCourse(12, 'Eng');
  DirectInsertJunction(1, 10); DirectInsertJunction(1, 11); DirectInsertJunction(1, 12);
  Student:=TTestM2MStudent.Create;
  try
    Student.ID:=1; Student.FState:=opsKey; OPManager.Load(Student);
    OPManager.LoadAssociations(Student, 'CourseList');
    AssertEquals('CourseList has 3 items', 3, Student.CourseList.Count);
  finally Student.Free; end;
end;

procedure TTestM2MLoadAssociationsCommon.TestLoadAssociationsOpsClean;
var Student: TTestM2MStudent; IPS: IOPPersistenceState; I: Integer;
begin
  DirectInsertStudent(1, 'Frank'); DirectInsertCourse(10, 'Music'); DirectInsertCourse(11, 'Drama');
  DirectInsertJunction(1, 10); DirectInsertJunction(1, 11);
  Student:=TTestM2MStudent.Create;
  try
    Student.ID:=1; Student.FState:=opsKey; OPManager.Load(Student);
    OPManager.LoadAssociations(Student, 'CourseList');
    for I:=0 to Student.CourseList.Count-1 do
    begin
      AssertTrue('Supports IOPPersistenceState', Supports(Student.CourseList[I], IOPPersistenceState, IPS));
      AssertTrue('State is opsClean', IPS.PersistenceState = opsClean);
    end;
  finally Student.Free; end;
end;

{ ===== TTestM2MAssociationsCommon ===== }

function TTestM2MAssociationsCommon.CountJunctionRows(aStudentID, aCourseID: Integer): Integer;
var Q: TSQLQuery;
begin
  if FConn.Transaction.Active then
    FConn.Transaction.Rollback;
  FConn.Transaction.StartTransaction;
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=FConn; Q.Transaction:=FConn.Transaction;
    Q.SQL.Text:='SELECT COUNT(*) FROM student_course WHERE student_id = :sid AND course_id = :cid';
    Q.Params.ParamByName('sid').AsInteger:=aStudentID;
    Q.Params.ParamByName('cid').AsInteger:=aCourseID;
    Q.Open; Result:=Q.Fields[0].AsInteger; Q.Close;
  finally Q.Free; end;
  FConn.Transaction.Commit;
end;

procedure TTestM2MAssociationsCommon.SetUp;
var Conn: TOPConnection; Def: TPersisterDef; Idx: Integer;
begin
  inherited SetUp;
  CreateM2MTables;
  OPRegistry.RegisterPersister(TTestM2MStudent, TRTTIPersister);
  Def:=OPRegistry.FindPersister(TTestM2MStudent); Def.TableName:='TestStudent';
  Idx:=Def.FieldMap.IndexOfPropertyName('CourseList'); if Idx >= 0 then Def.FieldMap.Delete(Idx);
  OPRegistry.RegisterPersister(TTestM2MCourse, TRTTIPersister);
  Def:=OPRegistry.FindPersister(TTestM2MCourse); Def.TableName:='TestCourse';
  Idx:=Def.FieldMap.IndexOfPropertyName('StudentList'); if Idx >= 0 then Def.FieldMap.Delete(Idx);
  OPM2MRegistry.RegisterManyToMany(TTestM2MStudent, TTestM2MCourse, 'student_course', 'student_id', 'course_id', False, 'CourseList', 'StudentList');
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn; Conn.IsDefault:=True;
end;

procedure TTestM2MAssociationsCommon.TearDown;
begin
  TOPManager.ClearInstance;
  TOPManyToManyRegistry.ClearInstance;
  TOPRelationRegistry.ClearInstance;
  TOPRegistry.ClearInstance;
  inherited TearDown;
end;

procedure TTestM2MAssociationsCommon.TestAddAssociation;
var Student: TTestM2MStudent; Course: TTestM2MCourse;
begin
  Student:=TTestM2MStudent.Create;
  try
    Student.ID:=1; Student.Name:='Alice'; Student.FState:=opsNew; OPManager.Save(Student);
    Course:=TTestM2MCourse.Create;
    try
      Course.ID:=10; Course.Title:='Math'; Course.FState:=opsNew; OPManager.Save(Course);
      OPManager.AddAssociation(Student, Course);
      AssertEquals('Junction row exists', 1, CountJunctionRows(1, 10));
    finally Course.Free; end;
  finally Student.Free; end;
end;

procedure TTestM2MAssociationsCommon.TestAddAssociationReverseOrder;
var Student: TTestM2MStudent; Course: TTestM2MCourse;
begin
  Student:=TTestM2MStudent.Create;
  try
    Student.ID:=1; Student.Name:='Alice'; Student.FState:=opsNew; OPManager.Save(Student);
    Course:=TTestM2MCourse.Create;
    try
      Course.ID:=10; Course.Title:='Math'; Course.FState:=opsNew; OPManager.Save(Course);
      OPManager.AddAssociation(Course, Student);
      AssertEquals('Junction row exists', 1, CountJunctionRows(1, 10));
    finally Course.Free; end;
  finally Student.Free; end;
end;

procedure TTestM2MAssociationsCommon.TestRemoveAssociation;
var Student: TTestM2MStudent; Course: TTestM2MCourse;
begin
  Student:=TTestM2MStudent.Create;
  try
    Student.ID:=1; Student.Name:='Alice'; Student.FState:=opsNew; OPManager.Save(Student);
    Course:=TTestM2MCourse.Create;
    try
      Course.ID:=10; Course.Title:='Math'; Course.FState:=opsNew; OPManager.Save(Course);
      OPManager.AddAssociation(Student, Course);
      AssertEquals('Junction row before remove', 1, CountJunctionRows(1, 10));
      OPManager.RemoveAssociation(Student, Course);
      AssertEquals('Junction row after remove', 0, CountJunctionRows(1, 10));
    finally Course.Free; end;
  finally Student.Free; end;
end;

procedure TTestM2MAssociationsCommon.TestRemoveAssociationReverseOrder;
var Student: TTestM2MStudent; Course: TTestM2MCourse;
begin
  Student:=TTestM2MStudent.Create;
  try
    Student.ID:=1; Student.Name:='Alice'; Student.FState:=opsNew; OPManager.Save(Student);
    Course:=TTestM2MCourse.Create;
    try
      Course.ID:=10; Course.Title:='Math'; Course.FState:=opsNew; OPManager.Save(Course);
      OPManager.AddAssociation(Student, Course);
      OPManager.RemoveAssociation(Course, Student);
      AssertEquals('Junction row after remove', 0, CountJunctionRows(1, 10));
    finally Course.Free; end;
  finally Student.Free; end;
end;

procedure TTestM2MAssociationsCommon.TestAddDuplicateAssociationRaises;
var Student: TTestM2MStudent; Course: TTestM2MCourse; Raised: Boolean;
begin
  Student:=TTestM2MStudent.Create;
  try
    Student.ID:=1; Student.Name:='Alice'; Student.FState:=opsNew; OPManager.Save(Student);
    Course:=TTestM2MCourse.Create;
    try
      Course.ID:=10; Course.Title:='Math'; Course.FState:=opsNew; OPManager.Save(Course);
      OPManager.AddAssociation(Student, Course);
      Raised:=False;
      try OPManager.AddAssociation(Student, Course); except on E: EOPF do Raised:=True; end;
      AssertTrue('Duplicate add should raise', Raised);
    finally Course.Free; end;
  finally Student.Free; end;
end;

procedure TTestM2MAssociationsCommon.TestTransactionRollbackOnFailure;
var Student: TTestM2MStudent; Course: TTestM2MCourse; Raised: Boolean;
begin
  Student:=TTestM2MStudent.Create;
  try
    Student.ID:=1; Student.Name:='Alice'; Student.FState:=opsNew; OPManager.Save(Student);
    Course:=TTestM2MCourse.Create;
    try
      Course.ID:=10; Course.Title:='Math'; Course.FState:=opsNew; OPManager.Save(Course);
      OPManager.AddAssociation(Student, Course);
      Raised:=False;
      try OPManager.AddAssociation(Student, Course); except on E: EOPF do Raised:=True; end;
      AssertTrue('Duplicate add should raise', Raised);
      AssertEquals('Original junction row preserved', 1, CountJunctionRows(1, 10));
    finally Course.Free; end;
  finally Student.Free; end;
end;

procedure TTestM2MAssociationsCommon.TestAddAssociationUnregisteredPairRaises;
var A, B: TTestM2MStudent; Raised: Boolean;
begin
  A:=TTestM2MStudent.Create;
  try
    B:=TTestM2MStudent.Create;
    try
      Raised:=False;
      try OPManager.AddAssociation(A, B); except on E: EOPF do Raised:=True; end;
      AssertTrue('AddAssociation with unregistered pair should raise', Raised);
    finally B.Free; end;
  finally A.Free; end;
end;

procedure TTestM2MAssociationsCommon.TestRemoveAssociationUnregisteredPairRaises;
var A, B: TTestM2MStudent; Raised: Boolean;
begin
  A:=TTestM2MStudent.Create;
  try
    B:=TTestM2MStudent.Create;
    try
      Raised:=False;
      try OPManager.RemoveAssociation(A, B); except on E: EOPF do Raised:=True; end;
      AssertTrue('RemoveAssociation with unregistered pair should raise', Raised);
    finally B.Free; end;
  finally A.Free; end;
end;

{ ===== TTestM2MCascadeDeleteCommon ===== }

procedure TTestM2MCascadeDeleteCommon.DirectInsertStudent(aID: Integer; const aName: String);
begin
  SQLInsert(FConn, 'INSERT INTO TestStudent (ID, Name) VALUES (:id, :name)', ['id', 'name'], [aID, aName]);
end;

procedure TTestM2MCascadeDeleteCommon.DirectInsertCourse(aID: Integer; const aTitle: String);
begin
  SQLInsert(FConn, 'INSERT INTO TestCourse (ID, Title) VALUES (:id, :title)', ['id', 'title'], [aID, aTitle]);
end;

procedure TTestM2MCascadeDeleteCommon.DirectInsertJunction(const aTable: String; aFK1Value, aFK2Value: Integer; const aFK1Name, aFK2Name: String);
begin
  SQLInsert(FConn, Format('INSERT INTO %s (%s, %s) VALUES (:fk1, :fk2)', [aTable, aFK1Name, aFK2Name]),
    ['fk1', 'fk2'], [aFK1Value, aFK2Value]);
end;

function TTestM2MCascadeDeleteCommon.CountJunctionRows(const aTable: String): Integer;
begin
  Result:=SQLCount(FConn, aTable);
end;

procedure TTestM2MCascadeDeleteCommon.SetUp;
var Conn: TOPConnection; Def: TPersisterDef; Idx: Integer;
begin
  inherited SetUp;
  CreateM2MTables;
  OPRegistry.RegisterPersister(TTestM2MStudent, TRTTIPersister);
  Def:=OPRegistry.FindPersister(TTestM2MStudent); Def.TableName:='TestStudent';
  Idx:=Def.FieldMap.IndexOfPropertyName('CourseList'); if Idx >= 0 then Def.FieldMap.Delete(Idx);
  OPRegistry.RegisterPersister(TTestM2MCourse, TRTTIPersister);
  Def:=OPRegistry.FindPersister(TTestM2MCourse); Def.TableName:='TestCourse';
  Idx:=Def.FieldMap.IndexOfPropertyName('StudentList'); if Idx >= 0 then Def.FieldMap.Delete(Idx);
  OPRegistry.RegisterPersister(TTestM2MClub, TRTTIPersister);
  Def:=OPRegistry.FindPersister(TTestM2MClub); Def.TableName:='TestClub';
  OPM2MRegistry.RegisterManyToMany(TTestM2MStudent, TTestM2MCourse, 'student_course', 'student_id', 'course_id', False, 'CourseList', 'StudentList');
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn; Conn.IsDefault:=True;
end;

procedure TTestM2MCascadeDeleteCommon.TearDown;
begin
  TOPManager.ClearInstance;
  TOPManyToManyRegistry.ClearInstance;
  TOPRelationRegistry.ClearInstance;
  TOPRegistry.ClearInstance;
  inherited TearDown;
end;

procedure TTestM2MCascadeDeleteCommon.TestCascadeDeleteRemovesJunctionRows;
var Student: TTestM2MStudent; Course: TTestM2MCourse;
begin
  Student:=TTestM2MStudent.Create;
  try
    Student.ID:=1; Student.Name:='Alice'; Student.FState:=opsNew; OPManager.Save(Student);
    Course:=TTestM2MCourse.Create;
    try
      Course.ID:=10; Course.Title:='Math'; Course.FState:=opsNew; OPManager.Save(Course);
      OPManager.AddAssociation(Student, Course);
      AssertEquals('Junction row exists before delete', 1, CountJunctionRows('student_course'));
      OPManager.Delete(Student);
      AssertEquals('Junction rows removed after delete', 0, CountJunctionRows('student_course'));
    finally Course.Free; end;
  finally Student.Free; end;
end;

procedure TTestM2MCascadeDeleteCommon.TestCascadeDeletePreservesAssociatedEntities;
var Student: TTestM2MStudent; Course, Course2: TTestM2MCourse;
begin
  Student:=TTestM2MStudent.Create;
  try
    Student.ID:=1; Student.Name:='Alice'; Student.FState:=opsNew; OPManager.Save(Student);
    Course:=TTestM2MCourse.Create;
    try
      Course.ID:=10; Course.Title:='Math'; Course.FState:=opsNew; OPManager.Save(Course);
      OPManager.AddAssociation(Student, Course);
      OPManager.Delete(Student);
      Course2:=TTestM2MCourse.Create;
      try
        Course2.ID:=10; Course2.FState:=opsKey; OPManager.Load(Course2);
        AssertEquals('Course still exists', 'Math', Course2.Title);
      finally Course2.Free; end;
    finally Course.Free; end;
  finally Student.Free; end;
end;

procedure TTestM2MCascadeDeleteCommon.TestCascadeDeletePreservesOtherAssociations;
var Student1, Student2: TTestM2MStudent; Course: TTestM2MCourse;
begin
  Student1:=TTestM2MStudent.Create;
  try
    Student1.ID:=1; Student1.Name:='Alice'; Student1.FState:=opsNew; OPManager.Save(Student1);
    Student2:=TTestM2MStudent.Create;
    try
      Student2.ID:=2; Student2.Name:='Bob'; Student2.FState:=opsNew; OPManager.Save(Student2);
      Course:=TTestM2MCourse.Create;
      try
        Course.ID:=10; Course.Title:='Math'; Course.FState:=opsNew; OPManager.Save(Course);
        OPManager.AddAssociation(Student1, Course);
        OPManager.AddAssociation(Student2, Course);
        AssertEquals('Two junction rows before delete', 2, CountJunctionRows('student_course'));
        OPManager.Delete(Student1);
        AssertEquals('One junction row after delete', 1, CountJunctionRows('student_course'));
      finally Course.Free; end;
    finally Student2.Free; end;
  finally Student1.Free; end;
end;

procedure TTestM2MCascadeDeleteCommon.TestCascadeDeleteMultipleM2MDefs;
var Student: TTestM2MStudent; Course: TTestM2MCourse; Club: TTestM2MClub;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MStudent, TTestM2MClub, 'student_club', 'student_id', 'club_id');
  Student:=TTestM2MStudent.Create;
  try
    Student.ID:=1; Student.Name:='Alice'; Student.FState:=opsNew; OPManager.Save(Student);
    Course:=TTestM2MCourse.Create;
    try
      Course.ID:=10; Course.Title:='Math'; Course.FState:=opsNew; OPManager.Save(Course);
      Club:=TTestM2MClub.Create;
      try
        Club.ID:=20; Club.Name:='Chess'; Club.FState:=opsNew; OPManager.Save(Club);
        OPManager.AddAssociation(Student, Course);
        OPManager.AddAssociation(Student, Club);
        OPManager.Delete(Student);
        AssertEquals('student_course junction rows removed', 0, CountJunctionRows('student_course'));
        AssertEquals('student_club junction rows removed', 0, CountJunctionRows('student_club'));
      finally Club.Free; end;
    finally Course.Free; end;
  finally Student.Free; end;
end;

procedure TTestM2MCascadeDeleteCommon.TestCascadeDeleteDBCascadeSkip;
var Student: TTestM2MStudent;
begin
  TOPManyToManyRegistry.ClearInstance;
  OPM2MRegistry.RegisterManyToMany(TTestM2MStudent, TTestM2MCourse, 'student_course', 'student_id', 'course_id', True, 'CourseList', 'StudentList');
  DirectInsertStudent(1, 'Alice'); DirectInsertCourse(1, 'Math');
  DirectInsertJunction('student_course', 1, 1, 'student_id', 'course_id');
  Student:=TTestM2MStudent.Create;
  try
    Student.ID:=1; Student.FState:=opsKey; OPManager.Load(Student);
    OPManager.Delete(Student);
    AssertEquals('Junction row preserved (DBCascade skip)', 1, CountJunctionRows('student_course'));
  finally Student.Free; end;
end;

procedure TTestM2MCascadeDeleteCommon.TestCascadeDeleteTransactionAtomicity;
var Student: TTestM2MStudent; Raised: Boolean;
begin
  DirectInsertStudent(1, 'Alice'); DirectInsertCourse(1, 'Math');
  DirectInsertJunction('student_course', 1, 1, 'student_id', 'course_id');
  // Drop TestStudent table to force parent delete failure
  ExecDDL('DROP TABLE TestStudent');
  Student:=TTestM2MStudent.Create;
  try
    Student.ID:=1; Student.FState:=opsClean;
    Raised:=False;
    try OPManager.Delete(Student); except on E: Exception do Raised:=True; end;
    AssertTrue('Exception raised on parent delete failure', Raised);
    AssertEquals('Junction row preserved after rollback', 1, CountJunctionRows('student_course'));
  finally Student.Free; end;
end;

{ ===== TTestLazyDepartment ===== }

constructor TTestLazyDepartment.Create;
begin
  inherited Create;
  FEmployeeList := TObjectList.Create(True);
  FLazyEmployees.Init(FEmployeeList);
  FState := opsNew;
  FManager := nil;
end;

destructor TTestLazyDepartment.Destroy;
begin
  FEmployeeList.Free;
  inherited Destroy;
end;

function TTestLazyDepartment.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TTestLazyDepartment.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

function TTestLazyDepartment.GetEmployeeList: TObjectList;
begin
  Result := FLazyEmployees.Value;
end;

procedure TTestLazyDepartment.DoLoadEmployees(aTarget: TObject);
begin
  TOPManager(FManager).LoadChildren(Self, TTestLazyEmployee, aTarget);
end;

procedure TTestLazyDepartment.InitLazyLoaders(aManager: TObject);
begin
  FManager := aManager;
  FLazyEmployees.SetLoader(@DoLoadEmployees);
end;

{ TTestLazyEmployee }

function TTestLazyEmployee.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TTestLazyEmployee.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

{ ===== TTestLazyLoadChildrenCommon ===== }

procedure TTestLazyLoadChildrenCommon.DirectInsertDept(aID: Integer; const aName: String);
begin
  SQLInsert(FConn, 'INSERT INTO TestLazyDepartment (ID, Name) VALUES (:ID, :Name)',
    ['ID', 'Name'], [aID, aName]);
end;

procedure TTestLazyLoadChildrenCommon.DirectInsertEmployee(aID, aDeptID: Integer; const aEmpName: String);
begin
  SQLInsert(FConn, 'INSERT INTO TestLazyEmployee (ID, DepartmentID, EmpName) VALUES (:ID, :DeptID, :EmpName)',
    ['ID', 'DeptID', 'EmpName'], [aID, aDeptID, aEmpName]);
end;

procedure TTestLazyLoadChildrenCommon.SetUp;
var
  Conn: TOPConnection;
  Def: TPersisterDef;
  Idx: Integer;
begin
  inherited SetUp;
  CreateLazyTables;
  OPRegistry.RegisterPersister(TTestLazyDepartment, TRTTIPersister);
  Def := OPRegistry.FindPersister(TTestLazyDepartment);
  Idx := Def.FieldMap.IndexOfPropertyName('EmployeeList');
  if Idx >= 0 then
    Def.FieldMap.Delete(Idx);
  OPRegistry.RegisterPersister(TTestLazyEmployee, TRTTIPersister);
  OPRelationRegistry.RegisterRelationship(
    TTestLazyDepartment, TTestLazyEmployee, 'DepartmentID', False, 'EmployeeList');
  TOPManager.ClearInstance;
  Conn := OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection := FConn;
  Conn.IsDefault := True;
end;

procedure TTestLazyLoadChildrenCommon.TearDown;
begin
  TOPManager.ClearInstance;
  TOPRelationRegistry.ClearInstance;
  TOPRegistry.ClearInstance;
  inherited TearDown;
end;

procedure TTestLazyLoadChildrenCommon.TestLazyChildrenNotLoadedBeforeAccess;
var
  Dept: TTestLazyDepartment;
begin
  DirectInsertDept(1, 'Engineering');
  DirectInsertEmployee(10, 1, 'Alice');
  Dept := TTestLazyDepartment.Create;
  try
    Dept.ID := 1;
    Dept.FState := opsKey;
    OPManager.Load(Dept);
    { After Load, lazy field is not yet loaded }
    AssertFalse('Not loaded before access', Dept.FLazyEmployees.IsLoaded);
  finally
    Dept.Free;
  end;
end;

procedure TTestLazyLoadChildrenCommon.TestLazyChildrenLoadedOnAccess;
var
  Dept: TTestLazyDepartment;
begin
  DirectInsertDept(1, 'Engineering');
  DirectInsertEmployee(10, 1, 'Alice');
  DirectInsertEmployee(11, 1, 'Bob');
  Dept := TTestLazyDepartment.Create;
  try
    Dept.ID := 1;
    Dept.FState := opsKey;
    OPManager.Load(Dept);
    { Accessing EmployeeList triggers lazy load }
    AssertEquals('EmployeeList count', 2, Dept.EmployeeList.Count);
    AssertTrue('IsLoaded after access', Dept.FLazyEmployees.IsLoaded);
    AssertTrue('Item is TTestLazyEmployee', Dept.EmployeeList[0] is TTestLazyEmployee);
  finally
    Dept.Free;
  end;
end;

procedure TTestLazyLoadChildrenCommon.TestLazyChildrenMatchExplicitLoad;
var
  DeptLazy, DeptExplicit: TTestLazyDepartment;
  ExplicitList: TObjectList;
  I: Integer;
begin
  DirectInsertDept(1, 'Engineering');
  DirectInsertEmployee(10, 1, 'Alice');
  DirectInsertEmployee(11, 1, 'Bob');
  DirectInsertEmployee(12, 1, 'Carol');
  DeptLazy := TTestLazyDepartment.Create;
  DeptExplicit := TTestLazyDepartment.Create;
  ExplicitList := TObjectList.Create(True);
  try
    { Lazy load }
    DeptLazy.ID := 1;
    DeptLazy.FState := opsKey;
    OPManager.Load(DeptLazy);
    { Explicit load for comparison }
    DeptExplicit.ID := 1;
    DeptExplicit.FState := opsKey;
    OPManager.Load(DeptExplicit);
    OPManager.LoadChildren(DeptExplicit, TTestLazyEmployee, ExplicitList);
    { Compare counts }
    AssertEquals('Lazy count matches explicit',
      ExplicitList.Count, DeptLazy.EmployeeList.Count);
    { Compare names }
    for I := 0 to ExplicitList.Count - 1 do
      AssertEquals('Employee name matches',
        TTestLazyEmployee(ExplicitList[I]).EmpName,
        TTestLazyEmployee(DeptLazy.EmployeeList[I]).EmpName);
  finally
    ExplicitList.Free;
    DeptExplicit.Free;
    DeptLazy.Free;
  end;
end;

procedure TTestLazyLoadChildrenCommon.TestLazyInvalidateReloads;
var
  Dept: TTestLazyDepartment;
begin
  DirectInsertDept(1, 'Engineering');
  DirectInsertEmployee(10, 1, 'Alice');
  Dept := TTestLazyDepartment.Create;
  try
    Dept.ID := 1;
    Dept.FState := opsKey;
    OPManager.Load(Dept);
    AssertEquals('First load count', 1, Dept.EmployeeList.Count);
    { Insert another employee }
    DirectInsertEmployee(11, 1, 'Bob');
    { Invalidate and re-access — should reload }
    Dept.FLazyEmployees.Invalidate;
    Dept.FEmployeeList.Clear;
    AssertEquals('Reloaded count', 2, Dept.EmployeeList.Count);
  finally
    Dept.Free;
  end;
end;

procedure TTestLazyLoadChildrenCommon.TestLazyNoLoaderReturnsEmpty;
var
  Dept: TTestLazyDepartment;
begin
  DirectInsertDept(1, 'Engineering');
  DirectInsertEmployee(10, 1, 'Alice');
  Dept := TTestLazyDepartment.Create;
  try
    Dept.ID := 1;
    Dept.FState := opsKey;
    { Do NOT call OPManager.Load — so InitLazyLoaders is never called.
      Accessing EmployeeList should return empty list, not crash. }
    AssertEquals('Empty list without loader', 0, Dept.EmployeeList.Count);
  finally
    Dept.Free;
  end;
end;

{ ===== TTestLazyM2MStudent ===== }

constructor TTestLazyM2MStudent.Create;
begin
  inherited Create;
  FCourseList := TObjectList.Create(True);
  FLazyCourses.Init(FCourseList);
  FState := opsNew;
  FManager := nil;
end;

destructor TTestLazyM2MStudent.Destroy;
begin
  FCourseList.Free;
  inherited Destroy;
end;

function TTestLazyM2MStudent.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TTestLazyM2MStudent.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

function TTestLazyM2MStudent.GetCourseList: TObjectList;
begin
  Result := FLazyCourses.Value;
end;

procedure TTestLazyM2MStudent.DoLoadCourses(aTarget: TObject);
begin
  TOPManager(FManager).LoadAssociations(Self, TTestM2MCourse, aTarget);
end;

procedure TTestLazyM2MStudent.InitLazyLoaders(aManager: TObject);
begin
  FManager := aManager;
  FLazyCourses.SetLoader(@DoLoadCourses);
end;

{ ===== TTestLazyM2MCommon ===== }

procedure TTestLazyM2MCommon.DirectInsertStudent(aID: Integer; const aName: String);
begin
  SQLInsert(FConn, 'INSERT INTO TestLazyStudent (ID, Name) VALUES (:ID, :Name)',
    ['ID', 'Name'], [aID, aName]);
end;

procedure TTestLazyM2MCommon.DirectInsertCourse(aID: Integer; const aTitle: String);
begin
  SQLInsert(FConn, 'INSERT INTO TestLazyCourse (ID, Title) VALUES (:ID, :Title)',
    ['ID', 'Title'], [aID, aTitle]);
end;

procedure TTestLazyM2MCommon.DirectInsertJunction(aStudentID, aCourseID: Integer);
begin
  SQLInsert(FConn, 'INSERT INTO lazy_student_course (student_id, course_id) VALUES (:SID, :CID)',
    ['SID', 'CID'], [aStudentID, aCourseID]);
end;

procedure TTestLazyM2MCommon.SetUp;
var
  Conn: TOPConnection;
  Def: TPersisterDef;
  Idx: Integer;
begin
  inherited SetUp;
  CreateLazyM2MTables;
  OPRegistry.RegisterPersister(TTestLazyM2MStudent, TRTTIPersister);
  Def := OPRegistry.FindPersister(TTestLazyM2MStudent);
  Def.TableName := 'TestLazyStudent';
  Idx := Def.FieldMap.IndexOfPropertyName('CourseList');
  if Idx >= 0 then
    Def.FieldMap.Delete(Idx);
  OPRegistry.RegisterPersister(TTestM2MCourse, TRTTIPersister);
  Def := OPRegistry.FindPersister(TTestM2MCourse);
  Def.TableName := 'TestLazyCourse';
  Idx := Def.FieldMap.IndexOfPropertyName('StudentList');
  if Idx >= 0 then
    Def.FieldMap.Delete(Idx);
  OPM2MRegistry.RegisterManyToMany(
    TTestLazyM2MStudent, TTestM2MCourse,
    'lazy_student_course', 'student_id', 'course_id',
    False, 'CourseList', 'StudentList');
  TOPManager.ClearInstance;
  Conn := OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection := FConn;
  Conn.IsDefault := True;
end;

procedure TTestLazyM2MCommon.TearDown;
begin
  TOPManager.ClearInstance;
  TOPManyToManyRegistry.ClearInstance;
  TOPRelationRegistry.ClearInstance;
  TOPRegistry.ClearInstance;
  inherited TearDown;
end;

procedure TTestLazyM2MCommon.TestLazyAssociationsLoadedOnAccess;
var
  Student: TTestLazyM2MStudent;
begin
  DirectInsertStudent(1, 'Alice');
  DirectInsertCourse(10, 'Math');
  DirectInsertCourse(11, 'Physics');
  DirectInsertJunction(1, 10);
  DirectInsertJunction(1, 11);
  Student := TTestLazyM2MStudent.Create;
  try
    Student.ID := 1;
    Student.FState := opsKey;
    OPManager.Load(Student);
    AssertFalse('Not loaded before access', Student.FLazyCourses.IsLoaded);
    AssertEquals('CourseList count', 2, Student.CourseList.Count);
    AssertTrue('IsLoaded after access', Student.FLazyCourses.IsLoaded);
    AssertTrue('Item is TTestM2MCourse', Student.CourseList[0] is TTestM2MCourse);
  finally
    Student.Free;
  end;
end;

procedure TTestLazyM2MCommon.TestLazyAssociationsMatchExplicit;
var
  StudentLazy: TTestLazyM2MStudent;
  StudentExplicit: TTestLazyM2MStudent;
  ExplicitList: TObjectList;
  I: Integer;
begin
  DirectInsertStudent(1, 'Bob');
  DirectInsertCourse(10, 'History');
  DirectInsertCourse(11, 'Art');
  DirectInsertCourse(12, 'Music');
  DirectInsertJunction(1, 10);
  DirectInsertJunction(1, 11);
  DirectInsertJunction(1, 12);
  StudentLazy := TTestLazyM2MStudent.Create;
  StudentExplicit := TTestLazyM2MStudent.Create;
  ExplicitList := TObjectList.Create(True);
  try
    StudentLazy.ID := 1;
    StudentLazy.FState := opsKey;
    OPManager.Load(StudentLazy);
    StudentExplicit.ID := 1;
    StudentExplicit.FState := opsKey;
    OPManager.Load(StudentExplicit);
    OPManager.LoadAssociations(StudentExplicit, TTestM2MCourse, ExplicitList);
    AssertEquals('Lazy count matches explicit',
      ExplicitList.Count, StudentLazy.CourseList.Count);
    for I := 0 to ExplicitList.Count - 1 do
      AssertEquals('Course title matches',
        TTestM2MCourse(ExplicitList[I]).Title,
        TTestM2MCourse(StudentLazy.CourseList[I]).Title);
  finally
    ExplicitList.Free;
    StudentExplicit.Free;
    StudentLazy.Free;
  end;
end;

{ ===== TTestSaveListCommon ===== }

procedure TTestSaveListCommon.DirectInsertContact(aID: Integer; const aName: String);
begin
  SQLInsert(FConn, 'INSERT INTO TestMgrSaveContact (ID, Name) VALUES (:ID, :Name)',
    ['ID', 'Name'], [aID, aName]);
end;

procedure TTestSaveListCommon.DirectSelectContact(aID: Integer; out aName: String);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConn;
    Q.Transaction := FConn.Transaction;
    if not FConn.Transaction.Active then
      FConn.Transaction.StartTransaction;
    Q.SQL.Text := 'SELECT Name FROM TestMgrSaveContact WHERE ID = :ID';
    Q.Params.ParamByName('ID').AsInteger := aID;
    Q.Open;
    if Q.EOF then
      aName := ''
    else
      aName := Q.FieldByName('Name').AsString;
    Q.Close;
    FConn.Transaction.Commit;
  finally
    Q.Free;
  end;
end;

function TTestSaveListCommon.CountContacts: Integer;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConn;
    Q.Transaction := FConn.Transaction;
    if not FConn.Transaction.Active then
      FConn.Transaction.StartTransaction;
    Q.SQL.Text := 'SELECT COUNT(*) FROM TestMgrSaveContact';
    Q.Open;
    Result := Q.Fields[0].AsInteger;
    Q.Close;
    FConn.Transaction.Commit;
  finally
    Q.Free;
  end;
end;

procedure TTestSaveListCommon.SetUp;
var
  Conn: TOPConnection;
begin
  inherited SetUp;
  CreateSaveListTables;
  OPRegistry.RegisterPersister(TTestMgrSaveContact, TRTTIPersister);
  TOPManager.ClearInstance;
  Conn := OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection := FConn;
  Conn.IsDefault := True;
end;

procedure TTestSaveListCommon.TearDown;
begin
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  inherited TearDown;
end;

procedure TTestSaveListCommon.TestSaveListInsertsNewItems;
var
  List: TObjectList;
  C: TTestMgrSaveContact;
  N: String;
begin
  List := TObjectList.Create(True);
  try
    C := TTestMgrSaveContact.Create;
    C.ID := 1;
    C.Name := 'Alice';
    C.FState := opsNew;
    List.Add(C);
    C := TTestMgrSaveContact.Create;
    C.ID := 2;
    C.Name := 'Bob';
    C.FState := opsNew;
    List.Add(C);
    OPManager.Save(List);
    AssertEquals('Two rows inserted', 2, CountContacts);
    DirectSelectContact(1, N);
    AssertEquals('Alice', N);
    DirectSelectContact(2, N);
    AssertEquals('Bob', N);
  finally
    List.Free;
  end;
end;

procedure TTestSaveListCommon.TestSaveListUpdatesModifiedItems;
var
  List: TObjectList;
  C: TTestMgrSaveContact;
  N: String;
begin
  DirectInsertContact(1, 'Alice');
  List := TObjectList.Create(True);
  try
    C := TTestMgrSaveContact.Create;
    C.ID := 1;
    C.FState := opsKey;
    OPManager.Load(C);
    C.Name := 'Alice Updated';
    C.FState := opsUpdate;
    List.Add(C);
    OPManager.Save(List);
    DirectSelectContact(1, N);
    AssertEquals('Alice Updated', N);
  finally
    List.Free;
  end;
end;

procedure TTestSaveListCommon.TestSaveListDeletesMarkedItems;
var
  List: TObjectList;
  C: TTestMgrSaveContact;
begin
  DirectInsertContact(1, 'Alice');
  DirectInsertContact(2, 'Bob');
  AssertEquals('Two rows before delete', 2, CountContacts);
  List := TObjectList.Create(True);
  try
    C := TTestMgrSaveContact.Create;
    C.ID := 1;
    C.FState := opsDelete;
    List.Add(C);
    OPManager.Save(List);
    AssertEquals('One row after delete', 1, CountContacts);
  finally
    List.Free;
  end;
end;

procedure TTestSaveListCommon.TestSaveListSkipsCleanItems;
var
  List: TObjectList;
  C: TTestMgrSaveContact;
  N: String;
begin
  DirectInsertContact(1, 'Alice');
  List := TObjectList.Create(True);
  try
    C := TTestMgrSaveContact.Create;
    C.ID := 1;
    C.FState := opsKey;
    OPManager.Load(C);
    { State is now opsClean — Save should skip this item }
    List.Add(C);
    OPManager.Save(List);
    DirectSelectContact(1, N);
    AssertEquals('Unchanged', 'Alice', N);
  finally
    List.Free;
  end;
end;

procedure TTestSaveListCommon.TestSaveListMixedStates;
var
  List: TObjectList;
  C: TTestMgrSaveContact;
  N: String;
begin
  DirectInsertContact(1, 'Alice');
  DirectInsertContact(2, 'Bob');
  List := TObjectList.Create(True);
  try
    { New item }
    C := TTestMgrSaveContact.Create;
    C.ID := 3;
    C.Name := 'Carol';
    C.FState := opsNew;
    List.Add(C);
    { Updated item }
    C := TTestMgrSaveContact.Create;
    C.ID := 1;
    C.FState := opsKey;
    OPManager.Load(C);
    C.Name := 'Alice Modified';
    C.FState := opsUpdate;
    List.Add(C);
    { Deleted item }
    C := TTestMgrSaveContact.Create;
    C.ID := 2;
    C.FState := opsDelete;
    List.Add(C);
    OPManager.Save(List);
    AssertEquals('Two rows after mixed save', 2, CountContacts);
    DirectSelectContact(1, N);
    AssertEquals('Alice Modified', N);
    DirectSelectContact(3, N);
    AssertEquals('Carol', N);
  finally
    List.Free;
  end;
end;

end.
