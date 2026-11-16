unit utc.opf.attributes;

{$mode objfpc}{$H+}
{$modeswitch prefixedattributes}

interface

uses
  Classes, SysUtils, typinfo, Variants, db, sqldb, sqlite3conn,
  fpcunit, testregistry,
  Opf.Types, Opf.Fields, Opf.Registry, Opf.Attributes,
  Opf.Persister.ExtRTTI, Opf.Persister.RTTI, Opf.DDL, Opf.Driver,
  Opf.Relations;

type

  { Test model classes }

  {$RTTI EXPLICIT PROPERTIES([vcPublished]) FIELDS([vcPrivate, vcPublished])}

  [OPTableName('people')]
  [OPPrimaryKey('ID')]
  TAttrBasicPerson = class(TPersistent)
  private
    FID: Integer;
    FName: String;
    FAge: Integer;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

  [OPTableName('custom_fields')]
  [OPPrimaryKey('ID')]
  TAttrCustomFields = class(TPersistent)
  private
    FID: Integer;
    FTitle: String;
    FSecret: String;
  published
    property ID: Integer read FID write FID;
    [OPFieldName('display_title')]
    property Title: String read FTitle write FTitle;
    [OPSkip]
    property Secret: String read FSecret write FSecret;
  end;

  [OPTableName('opts')]
  [OPPrimaryKey('ID', 'pk_opts')]
  TAttrWithOptions = class(TPersistent)
  private
    FID: Integer;
    FCode: String;
  published
    [OPFieldOptions([foAutoInc])]
    property ID: Integer read FID write FID;
    [OPSize(50)]
    property Code: String read FCode write FCode;
  end;

  [OPTableName('indexed')]
  [OPPrimaryKey('ID')]
  [OPIndex('Name,Code', 'idx_name_code')]
  [OPIndex('Code', 'idx_code')]
  TAttrIndexed = class(TPersistent)
  private
    FID: Integer;
    FName: String;
    FCode: String;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Code: String read FCode write FCode;
  end;

  [OPTableName('parents')]
  [OPPrimaryKey('ID')]
  TAttrParent = class(TPersistent)
  private
    FID: Integer;
    FName: String;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  [OPTableName('children')]
  [OPPrimaryKey('ID')]
  TAttrChild = class(TPersistent)
  private
    FID: Integer;
    FParentID: Integer;
  published
    property ID: Integer read FID write FID;
    [OPRelation('TAttrParent')]
    property ParentID: Integer read FParentID write FParentID;
  end;

  [OPTableName('conflict')]
  [OPPrimaryKey('ID')]
  TAttrSkipFieldNameConflict = class(TPersistent)
  private
    FID: Integer;
    FBad: String;
  published
    property ID: Integer read FID write FID;
    [OPSkip]
    [OPFieldName('bad_col')]
    property Bad: String read FBad write FBad;
  end;

  [OPTableName('seqtest')]
  [OPPrimaryKey('ID')]
  TAttrWithSequence = class(TPersistent)
  private
    FID: Integer;
    FName: String;
  published
    [OPSequence('seq_id')]
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  [OPTableName('uniqdeftest')]
  [OPPrimaryKey('ID')]
  TAttrUniqueDefault = class(TPersistent)
  private
    FID: Integer;
    FEmail: String;
    FScore: Integer;
  published
    property ID: Integer read FID write FID;
    [OPUnique]
    [OPSize(200)]
    property Email: String read FEmail write FEmail;
    [OPDefault('42')]
    property Score: Integer read FScore write FScore;
  end;

  { Test cases }

  TTestAttributes = class(TTestCase)
  private
    FRegistry: TOPRegistry;
    procedure DoRegisterConflict;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTableNameOverride;
    procedure TestFieldNameOverride;
    procedure TestSkipField;
    procedure TestFieldOptions;
    procedure TestPrimaryKey;
    procedure TestPKConstraintName;
    procedure TestIndexCollection;
    procedure TestRelationRegistration;
    procedure TestSkipFieldNameConflictRaises;
    procedure TestSequenceName;
    procedure TestSizeAttribute;
    procedure TestDefaultBehaviorSameAsExtRTTI;
    procedure TestUniqueAttribute;
    procedure TestDefaultAttribute;
  end;

  TTestDDLIndex = class(TTestCase)
  published
    procedure TestGetCreateIndexSQL;
    procedure TestGetCreateSQLWithPKConstraintName;
  end;

implementation

{ TTestAttributes }

procedure TTestAttributes.SetUp;
begin
  inherited;
  TOPRegistry.ClearInstance;
  FRegistry:=TOPRegistry.Instance;
end;

procedure TTestAttributes.TearDown;
begin
  TOPRelationRegistry.ClearInstance;
  TOPRegistry.ClearInstance;
  FRegistry:=nil;
  inherited;
end;

procedure TTestAttributes.TestTableNameOverride;
var
  Def: TPersisterDef;
begin
  Def:=FRegistry.RegisterPersister(TAttrBasicPerson, TAttributePersister);
  AssertEquals('Table name', 'people', Def.TableName);
end;

procedure TTestAttributes.TestFieldNameOverride;
var
  Def: TPersisterDef;
  F: TOPFieldMap;
begin
  Def:=FRegistry.RegisterPersister(TAttrCustomFields, TAttributePersister);
  F:=Def.FieldMap.FindByPropertyName('Title');
  AssertNotNull('Title field should exist', F);
  AssertEquals('Column name overridden', 'display_title', F.FieldName);
end;

procedure TTestAttributes.TestSkipField;
var
  Def: TPersisterDef;
  F: TOPFieldMap;
begin
  Def:=FRegistry.RegisterPersister(TAttrCustomFields, TAttributePersister);
  F:=Def.FieldMap.FindByPropertyName('Secret');
  AssertNull('Secret field should be skipped', F);
  AssertTrue('Should have ID and Title', Def.FieldMap.Count >= 2);
end;

procedure TTestAttributes.TestFieldOptions;
var
  Def: TPersisterDef;
  F: TOPFieldMap;
begin
  Def:=FRegistry.RegisterPersister(TAttrWithOptions, TAttributePersister);
  F:=Def.FieldMap.FindByPropertyName('ID');
  AssertNotNull('ID field', F);
  AssertTrue('foAutoInc set', foAutoInc in F.Options);
end;

procedure TTestAttributes.TestPrimaryKey;
var
  Def: TPersisterDef;
  F: TOPFieldMap;
begin
  Def:=FRegistry.RegisterPersister(TAttrBasicPerson, TAttributePersister);
  F:=Def.FieldMap.FindByPropertyName('ID');
  AssertNotNull('ID field', F);
  AssertTrue('foInKey set', foInKey in F.Options);
end;

procedure TTestAttributes.TestPKConstraintName;
var
  Def: TPersisterDef;
begin
  Def:=FRegistry.RegisterPersister(TAttrWithOptions, TAttributePersister);
  AssertEquals('PK constraint name', 'pk_opts', Def.PKConstraintName);
end;

procedure TTestAttributes.TestIndexCollection;
var
  Def: TPersisterDef;
begin
  Def:=FRegistry.RegisterPersister(TAttrIndexed, TAttributePersister);
  AssertEquals('Should have 2 indexes', 2, Length(Def.Indexes));
  AssertEquals('First index name', 'idx_name_code', Def.Indexes[0].IndexName);
  AssertEquals('First index fields', 'Name,Code', Def.Indexes[0].FieldNames);
  AssertEquals('Second index name', 'idx_code', Def.Indexes[1].IndexName);
end;

procedure TTestAttributes.TestRelationRegistration;
var
  Def: TPersisterDef;
  Relations: TOPRelationDefArray;
begin
  // Register parent first
  FRegistry.RegisterPersister(TAttrParent, TAttributePersister);
  // Register child — should auto-register relationship
  Def:=FRegistry.RegisterPersister(TAttrChild, TAttributePersister);
  Relations:=TOPRelationRegistry.Instance.FindChildRelations(TAttrParent);
  AssertEquals('Should have 1 child relation', 1, Length(Relations));
  AssertTrue('Child class', Relations[0].ChildClass = TAttrChild);
  AssertEquals('FK field', 'ParentID', Relations[0].ForeignKeyFieldName);
end;

procedure TTestAttributes.DoRegisterConflict;
begin
  FRegistry.RegisterPersister(TAttrSkipFieldNameConflict, TAttributePersister);
end;

procedure TTestAttributes.TestSkipFieldNameConflictRaises;
begin
  AssertException('OPSkip + OPFieldName should raise', EFieldMap, @DoRegisterConflict);
end;

procedure TTestAttributes.TestSequenceName;
var
  Def: TPersisterDef;
  F: TOPFieldMap;
begin
  Def:=FRegistry.RegisterPersister(TAttrWithSequence, TAttributePersister);
  F:=Def.FieldMap.FindByPropertyName('ID');
  AssertNotNull('ID field', F);
  AssertEquals('Sequence name', 'seq_id', F.SequenceName);
end;

procedure TTestAttributes.TestSizeAttribute;
var
  Def: TPersisterDef;
  F: TOPFieldMap;
begin
  Def:=FRegistry.RegisterPersister(TAttrWithOptions, TAttributePersister);
  F:=Def.FieldMap.FindByPropertyName('Code');
  AssertNotNull('Code field', F);
  AssertEquals('MaxSize', 50, F.MaxSize);
end;

procedure TTestAttributes.TestDefaultBehaviorSameAsExtRTTI;
var
  DefAttr, DefExt: TPersisterDef;
  RegExt: TOPRegistry;
begin
  // Register with TAttributePersister (no attributes on TAttrBasicPerson except OPTableName/OPPrimaryKey)
  DefAttr:=FRegistry.RegisterPersister(TAttrBasicPerson, TAttributePersister);
  // Create separate registry for comparison
  RegExt:=TOPRegistry.Create(nil);
  try
    DefExt:=RegExt.RegisterPersister(TAttrBasicPerson, TExtRTTIPersister);
    AssertEquals('Same field count', DefExt.FieldMap.Count, DefAttr.FieldMap.Count);
  finally
    RegExt.Free;
  end;
end;

procedure TTestAttributes.TestUniqueAttribute;
var
  Def: TPersisterDef;
  F: TOPFieldMap;
begin
  Def:=FRegistry.RegisterPersister(TAttrUniqueDefault, TAttributePersister);
  F:=Def.FieldMap.FindByPropertyName('Email');
  AssertNotNull('Email field', F);
  AssertTrue('foUnique set', foUnique in F.Options);
end;

procedure TTestAttributes.TestDefaultAttribute;
var
  Def: TPersisterDef;
  F: TOPFieldMap;
begin
  Def:=FRegistry.RegisterPersister(TAttrUniqueDefault, TAttributePersister);
  F:=Def.FieldMap.FindByPropertyName('Score');
  AssertNotNull('Score field', F);
  AssertFalse('DefaultValue not null', VarIsNull(F.DefaultValue) or VarIsEmpty(F.DefaultValue));
  AssertEquals('DefaultValue', 42, Integer(F.DefaultValue));
end;

{ TTestDDLIndex }

procedure TTestDDLIndex.TestGetCreateIndexSQL;
var
  DDL: TOPDDLBuilder;
  Idx: TOPIndexDef;
  SQL: UTF8String;
begin
  DDL:=TOPDDLBuilder.Create(nil);
  try
    DDL.SQLDialect:=dSQLite;
    Idx.FieldNames:='Name,Code';
    Idx.IndexName:='idx_test';
    SQL:=DDL.GetCreateIndexSQL('mytable', Idx);
    AssertTrue('Contains CREATE INDEX', Pos('CREATE INDEX', SQL) > 0);
    AssertTrue('Contains index name', Pos('idx_test', SQL) > 0);
    AssertTrue('Contains ON', Pos('ON', SQL) > 0);
    AssertTrue('Contains table', Pos('mytable', SQL) > 0);
  finally
    DDL.Free;
  end;
end;

procedure TTestDDLIndex.TestGetCreateSQLWithPKConstraintName;
var
  DDL: TOPDDLBuilder;
  Map: TOPFieldMapList;
  F: TOPFieldMap;
  SQL: UTF8String;
begin
  DDL:=TOPDDLBuilder.Create(nil);
  try
    DDL.SQLDialect:=dSQLite;
    Map:=TOPFieldMapList.Create(TOPFieldMap);
    try
      F:=Map.AddField('ID', 'ID', [foInKey, foInSelect, foInInsert]);
      F.PropertyKind:=tkInteger;
      F:=Map.AddField('Name', 'Name', [foInSelect, foInInsert]);
      F.PropertyKind:=tkAString;
      SQL:=DDL.GetCreateSQL('test_table', Map.AsArray, 'my_custom_pk');
      AssertTrue('Contains custom PK name', Pos('my_custom_pk', SQL) > 0);
      AssertTrue('Contains PRIMARY KEY', Pos('PRIMARY KEY', SQL) > 0);
    finally
      Map.Free;
    end;
  finally
    DDL.Free;
  end;
end;

initialization
  RegisterTest(TTestAttributes);
  RegisterTest(TTestDDLIndex);

end.
