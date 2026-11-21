unit utc.opf.mapping.json;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}
{$IFNDEF VER3_2}
{$modeswitch prefixedattributes}
{$ENDIF}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Variants, FpcUnit.Test, FpcUnit.TestRegistry, FpJson.Data, FpJson.Parser,
{$ELSE}
  Classes, SysUtils, variants, fpcunit, testregistry, fpjson, jsonparser,
{$ENDIF}
{$IFNDEF VER3_2}
  Opf.Attributes, Opf.Persister.ExtRTTI,
{$ENDIF}
  Opf.Types, Opf.Fields, Opf.Registry, Opf.Relations, Opf.M2M, Opf.Persister.RTTI, Opf.Mapping.Json;

type

  TMapDepartment = class(TPersistent)
  private
    FID: Integer;
    FName: String;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  TMapEmployee = class(TPersistent)
  private
    FID: Integer;
    FName: String;
    FSalary: Double;
    FDepartmentID: Integer;
    FNotes: String;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Salary: Double read FSalary write FSalary;
    property DepartmentID: Integer read FDepartmentID write FDepartmentID;
    property Notes: String read FNotes write FNotes;
  end;

  TMapNoConvKey = class(TPersistent)
  private
    FPersonNr: Integer;
    FName: String;
  published
    property PersonNr: Integer read FPersonNr write FPersonNr;
    property Name: String read FName write FName;
  end;

  TMapTag = class(TPersistent)
  private
    FID: Integer;
    FName: String;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

{$IFNDEF VER3_2}
  TMapAttrEmployee = class(TPersistent)
  private
    FID: Integer;
    FDepartmentID: Integer;
  published
    property ID: Integer read FID write FID;
    [OPRelation('TMapDepartment')]
    property DepartmentID: Integer read FDepartmentID write FDepartmentID;
  end;
{$ENDIF}

  { TTestJSONMapping }

  TTestJSONMapping = class(TTestCase)
  private
    FReg: TOPRegistry;
    FM2M: TOPManyToManyRegistry;
    FMapping: TOPJSONMapping;
    procedure Load(const aJSON: String; aPersisterClass: TOPPersisterClass = Nil);
    function Save: TJSONObject;
    function LoadRaises(const aJSON: String; aPersisterClass: TOPPersisterClass = Nil): Boolean;
    function Relations: TOPRelationRegistry;
    function Def(aClass: TClass): TPersisterDef;
    function Field(aClass: TClass; const aProperty: String): TOPFieldMap;
    function SaveDefault(const aClasses: array of TClass): TJSONObject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSaveWritesClassAndFields;
    procedure TestSaveWritesRelationsAndLinks;
    procedure TestSaveLoadRoundTrip;
    procedure TestApplyToRegisteredClass;
    procedure TestApplyToClassRegisteredLater;
    procedure TestKeyBeforeRegistration;
    procedure TestKeyReplacesConvention;
    procedure TestOptionsAreCompleteSet;
    procedure TestAbsentKeysKeepDefaults;
    procedure TestSkipRemovesField;
    procedure TestIndexes;
    procedure TestIndexFieldsAreColumns;
    procedure TestUnknownIndexFieldRaises;
    procedure TestUnknownPropertyRaises;
    procedure TestUnknownKeyRaises;
    procedure TestUnknownOptionRaises;
    procedure TestWrongTypeRaises;
    procedure TestInvalidJSONRaises;
    procedure TestUnsupportedVersionRaises;
    procedure TestNoAutoCreateWithoutPersisterClass;
    procedure TestAutoCreate;
    procedure TestAutoCreatedIsReplaced;
    procedure TestDuplicateRegistrationStillRaises;
    procedure TestAutoCreateUnknownClassRaises;
    procedure TestQualifiedClassName;
    procedure TestRelationFromMapping;
    procedure TestRelationReplacesCodeRelation;
    procedure TestRelationWaitsForParent;
    procedure TestRelationForeignKeyByProperty;
    procedure TestRelationUnknownParentRaisesOnAutoCreate;
    procedure TestManyToMany;
    procedure TestFailedMappingUndoesRegistration;
    procedure TestFreeRemovesListener;
    procedure TestSaveDefaultMapping;
    procedure TestSaveDefaultMappingMarksFirstField;
{$IFNDEF VER3_2}
    procedure TestAttributeRelationOfReplacedClass;
{$ENDIF}
  end;

implementation

{ True if aArray has a string element equal to aValue }

function ArrayHasString(aArray: TJSONArray; const aValue: String): Boolean;

var
  I: Integer;

begin
  Result:=False;
  for I:=0 to aArray.Count-1 do
    if (aArray.Items[I].JSONType=jtString) and (aArray.Items[I].AsString=aValue) then
      Exit(True);
end;


{ TTestJSONMapping }

procedure TTestJSONMapping.SetUp;

begin
  FReg:=TOPRegistry.Create(Nil);
  FM2M:=TOPManyToManyRegistry.Create(Nil);
  FM2M.Registry:=FReg;
  FMapping:=TOPJSONMapping.Create(Nil);
  FMapping.Registry:=FReg;
  FMapping.M2MRegistry:=FM2M;
end;


procedure TTestJSONMapping.TearDown;

begin
  FreeAndNil(FMapping);
  FreeAndNil(FM2M);
  FreeAndNil(FReg);
end;


procedure TTestJSONMapping.Load(const aJSON: String; aPersisterClass: TOPPersisterClass);

var
  lStream: TStringStream;

begin
  lStream:=TStringStream.Create(aJSON);
  try
    FMapping.LoadFromStream(lStream, aPersisterClass);
  finally
    lStream.Free;
  end;
end;


function TTestJSONMapping.Save: TJSONObject;

var
  lStream: TStringStream;

begin
  lStream:=TStringStream.Create('');
  try
    FMapping.SaveToStream(lStream);
    Result:=GetJSON(lStream.DataString) as TJSONObject;
  finally
    lStream.Free;
  end;
end;


function TTestJSONMapping.LoadRaises(const aJSON: String; aPersisterClass: TOPPersisterClass): Boolean;

begin
  Result:=False;
  try
    Load(aJSON, aPersisterClass);
  except
    on E: EOPFMapping do
      Result:=True;
  end;
end;


function TTestJSONMapping.Relations: TOPRelationRegistry;

begin
  Result:=TOPRelationRegistry(FReg.RelationRegistry);
end;


function TTestJSONMapping.Def(aClass: TClass): TPersisterDef;

begin
  Result:=FReg.FindPersister(aClass);
  AssertNotNull('Class '+aClass.ClassName+' is registered', Result);
end;


function TTestJSONMapping.Field(aClass: TClass; const aProperty: String): TOPFieldMap;

begin
  Result:=Def(aClass).FieldMap.FindByPropertyName(aProperty);
  AssertNotNull('Property '+aProperty+' is mapped', Result);
end;


procedure TTestJSONMapping.TestSaveWritesClassAndFields;

var
  lDoc, lEntry: TJSONObject;
  lOptions: TJSONArray;

begin
  FReg.RegisterPersister(TMapEmployee, TRTTIPersister);
  lDoc:=Save;
  try
    AssertEquals('Version', 1, lDoc.Integers['version']);
    lEntry:=lDoc.Objects['classes'].Objects['TMapEmployee'];
    AssertEquals('Table name', 'MapEmployee', lEntry.Strings['table']);
    AssertEquals('Column of Name', 'Name', lEntry.Objects['fields'].Objects['Name'].Strings['column']);
    lOptions:=lEntry.Objects['fields'].Objects['ID'].Arrays['options'];
    AssertTrue('ID options have foInKey', ArrayHasString(lOptions, 'foInKey'));
    AssertTrue('Default is null', lEntry.Objects['fields'].Objects['Name'].Nulls['default']);
  finally
    lDoc.Free;
  end;
end;


procedure TTestJSONMapping.TestSaveWritesRelationsAndLinks;

var
  lDoc: TJSONObject;
  lRel, lLink: TJSONObject;

begin
  FReg.RegisterPersister(TMapDepartment, TRTTIPersister);
  FReg.RegisterPersister(TMapEmployee, TRTTIPersister);
  FReg.RegisterPersister(TMapTag, TRTTIPersister);
  Relations.RegisterRelationship(TMapDepartment, TMapEmployee, 'DepartmentID', True).ConstraintName:='FK_EMP';
  FM2M.RegisterManyToMany(TMapEmployee, TMapTag, 'EMP_TAG', 'EMP_ID', 'TAG_ID');
  lDoc:=Save;
  try
    lRel:=lDoc.Objects['classes'].Objects['TMapEmployee'].Arrays['relations'].Objects[0];
    AssertEquals('Relation parent', 'TMapDepartment', lRel.Strings['parent']);
    AssertEquals('Relation foreign key', 'DepartmentID', lRel.Strings['foreignKey']);
    AssertEquals('Relation constraint', 'FK_EMP', lRel.Strings['constraint']);
    AssertTrue('Relation cascade', lRel.Booleans['dbCascade']);
    lLink:=lDoc.Arrays['manyToMany'].Objects[0];
    AssertEquals('Link junction table', 'EMP_TAG', lLink.Strings['junctionTable']);
    AssertEquals('Link class B', 'TMapTag', lLink.Strings['classB']);
  finally
    lDoc.Free;
  end;
end;


procedure TTestJSONMapping.TestSaveLoadRoundTrip;

var
  lDoc: TJSONObject;
  lEntry: TJSONObject;

begin
  FReg.RegisterPersister(TMapEmployee, TRTTIPersister);
  lDoc:=Save;
  try
    lEntry:=lDoc.Objects['classes'].Objects['TMapEmployee'];
    lEntry.Strings['table']:='EMPLOYEES';
    lEntry.Objects['fields'].Objects['Name'].Strings['column']:='EMP_NAME';
    lEntry.Objects['fields'].Objects['Name'].Integers['maxSize']:=60;
    Load(lDoc.AsJSON);
  finally
    lDoc.Free;
  end;
  AssertEquals('Table name', 'EMPLOYEES', Def(TMapEmployee).TableName);
  AssertEquals('Column of Name', 'EMP_NAME', Field(TMapEmployee, 'Name').FieldName);
  AssertEquals('Size of Name', 60, Field(TMapEmployee, 'Name').MaxSize);
  AssertTrue('ID is still the key', foInKey in Field(TMapEmployee, 'ID').Options);
  AssertEquals('Field count', 5, Def(TMapEmployee).FieldMap.Count);
end;


procedure TTestJSONMapping.TestApplyToRegisteredClass;

begin
  FReg.RegisterPersister(TMapEmployee, TRTTIPersister);
  Load('{"classes":{"TMapEmployee":{"table":"EMP","pkConstraint":"PK_EMP","fields":{"Name":'+
       '{"column":"EMP_NAME","maxSize":80,"minSize":2,"sequence":"SEQ_EMP","default":"none"}}}}}');
  AssertEquals('Table name', 'EMP', Def(TMapEmployee).TableName);
  AssertEquals('Primary key constraint', 'PK_EMP', Def(TMapEmployee).PKConstraintName);
  AssertEquals('Column', 'EMP_NAME', Field(TMapEmployee, 'Name').FieldName);
  AssertEquals('Max size', 80, Field(TMapEmployee, 'Name').MaxSize);
  AssertEquals('Min size', 2, Field(TMapEmployee, 'Name').MinSize);
  AssertEquals('Sequence', 'SEQ_EMP', Field(TMapEmployee, 'Name').SequenceName);
  AssertEquals('Default', 'none', VarToStr(Field(TMapEmployee, 'Name').DefaultValue));
end;


procedure TTestJSONMapping.TestApplyToClassRegisteredLater;

begin
  Load('{"classes":{"TMapEmployee":{"table":"EMP","fields":{"Name":{"column":"EMP_NAME"},"Salary":{"default":1.5}}}}}');
  AssertNull('Not registered by loading', FReg.FindPersister(TMapEmployee));
  FReg.RegisterPersister(TMapEmployee, TRTTIPersister);
  AssertEquals('Table name', 'EMP', Def(TMapEmployee).TableName);
  AssertEquals('Column', 'EMP_NAME', Field(TMapEmployee, 'Name').FieldName);
  AssertEquals('Float default', 1.5, Double(Field(TMapEmployee, 'Salary').DefaultValue), 1e-9);
  AssertFalse('Not auto-created', Def(TMapEmployee).AutoCreated);
end;


procedure TTestJSONMapping.TestKeyBeforeRegistration;

begin
  Load('{"classes":{"TMapNoConvKey":{"fields":{"PersonNr":{"options":["foInKey","foInSelect","foInInsert","foInUpdate"]}}}}}');
  FReg.RegisterPersister(TMapNoConvKey, TRTTIPersister);
  AssertTrue('PersonNr is the key', foInKey in Field(TMapNoConvKey, 'PersonNr').Options);
  AssertFalse('Name is not a key', foInKey in Field(TMapNoConvKey, 'Name').Options);
end;


procedure TTestJSONMapping.TestKeyReplacesConvention;

begin
  FReg.RegisterPersister(TMapEmployee, TRTTIPersister);
  AssertTrue('ID is the key by convention', foInKey in Field(TMapEmployee, 'ID').Options);
  Load('{"classes":{"TMapEmployee":{"fields":{"Name":{"options":["foInKey","foInSelect","foInInsert","foInUpdate"]}}}}}');
  AssertTrue('Name is the key', foInKey in Field(TMapEmployee, 'Name').Options);
  AssertFalse('ID is no longer a key', foInKey in Field(TMapEmployee, 'ID').Options);
end;


procedure TTestJSONMapping.TestOptionsAreCompleteSet;

begin
  FReg.RegisterPersister(TMapEmployee, TRTTIPersister);
  Load('{"classes":{"TMapEmployee":{"fields":{"Notes":{"options":["foInSelect"]}}}}}');
  AssertTrue('Options replaced', Field(TMapEmployee, 'Notes').Options=[foInSelect]);
end;


procedure TTestJSONMapping.TestAbsentKeysKeepDefaults;

begin
  FReg.RegisterPersister(TMapEmployee, TRTTIPersister);
  Load('{"classes":{"TMapEmployee":{"fields":{"Notes":{"maxSize":200}}}}}');
  AssertEquals('Column unchanged', 'Notes', Field(TMapEmployee, 'Notes').FieldName);
  AssertTrue('Options unchanged', Field(TMapEmployee, 'Notes').Options=[foInSelect, foInInsert, foInUpdate]);
  AssertEquals('Table unchanged', 'MapEmployee', Def(TMapEmployee).TableName);
end;


procedure TTestJSONMapping.TestSkipRemovesField;

begin
  FReg.RegisterPersister(TMapEmployee, TRTTIPersister);
  Load('{"classes":{"TMapEmployee":{"fields":{"Notes":{"skip":true}}}}}');
  AssertNull('Notes is not mapped', Def(TMapEmployee).FieldMap.FindByPropertyName('Notes'));
  AssertEquals('Field count', 4, Def(TMapEmployee).FieldMap.Count);
end;


procedure TTestJSONMapping.TestIndexes;

begin
  FReg.RegisterPersister(TMapEmployee, TRTTIPersister);
  Load('{"classes":{"TMapEmployee":{"indexes":[{"name":"IX_EMP_NAME","fields":["Name","Salary"]}]}}}');
  AssertEquals('Index count', 1, Length(Def(TMapEmployee).Indexes));
  AssertEquals('Index name', 'IX_EMP_NAME', Def(TMapEmployee).Indexes[0].IndexName);
  AssertEquals('Index fields', 'Name,Salary', Def(TMapEmployee).Indexes[0].FieldNames);
end;


procedure TTestJSONMapping.TestIndexFieldsAreColumns;

begin
  FReg.RegisterPersister(TMapEmployee, TRTTIPersister);
  Load('{"classes":{"TMapEmployee":{"fields":{"Name":{"column":"EMP_NAME"}},'+
       '"indexes":[{"name":"IX_EMP_NAME","fields":["Name","Salary"]}]}}}');
  AssertEquals('Index fields are column names', 'EMP_NAME,Salary', Def(TMapEmployee).Indexes[0].FieldNames);
end;


procedure TTestJSONMapping.TestUnknownIndexFieldRaises;

begin
  FReg.RegisterPersister(TMapEmployee, TRTTIPersister);
  AssertTrue('Unknown index field raises',
    LoadRaises('{"classes":{"TMapEmployee":{"indexes":[{"name":"IX","fields":["Bogus"]}]}}}'));
end;


procedure TTestJSONMapping.TestUnknownPropertyRaises;

begin
  FReg.RegisterPersister(TMapEmployee, TRTTIPersister);
  AssertTrue('Unknown property raises',
    LoadRaises('{"classes":{"TMapEmployee":{"fields":{"Bogus":{"column":"X"}}}}}'));
end;


procedure TTestJSONMapping.TestUnknownKeyRaises;

begin
  FReg.RegisterPersister(TMapEmployee, TRTTIPersister);
  AssertTrue('Misspelled key raises',
    LoadRaises('{"classes":{"TMapEmployee":{"fields":{"Name":{"colum":"X"}}}}}'));
end;


procedure TTestJSONMapping.TestUnknownOptionRaises;

begin
  FReg.RegisterPersister(TMapEmployee, TRTTIPersister);
  AssertTrue('Unknown option raises',
    LoadRaises('{"classes":{"TMapEmployee":{"fields":{"Name":{"options":["foBogus"]}}}}}'));
end;


procedure TTestJSONMapping.TestWrongTypeRaises;

begin
  FReg.RegisterPersister(TMapEmployee, TRTTIPersister);
  AssertTrue('String size raises',
    LoadRaises('{"classes":{"TMapEmployee":{"fields":{"Name":{"maxSize":"80"}}}}}'));
end;


procedure TTestJSONMapping.TestInvalidJSONRaises;

begin
  AssertTrue('Invalid JSON raises', LoadRaises('{"classes":'));
  AssertTrue('Array raises', LoadRaises('[]'));
end;


procedure TTestJSONMapping.TestUnsupportedVersionRaises;

begin
  AssertTrue('Version 2 raises', LoadRaises('{"version":2,"classes":{}}'));
end;


procedure TTestJSONMapping.TestNoAutoCreateWithoutPersisterClass;

begin
  Load('{"classes":{"TMapDepartment":{"table":"DEPT"}}}');
  AssertNull('Not registered', FReg.FindPersister(TMapDepartment));
end;


procedure TTestJSONMapping.TestAutoCreate;

begin
  Load('{"classes":{"TMapDepartment":{"table":"DEPT"}}}', TRTTIPersister);
  AssertTrue('Auto-created', Def(TMapDepartment).AutoCreated);
  AssertEquals('Table name', 'DEPT', Def(TMapDepartment).TableName);
  AssertTrue('Persister class', Def(TMapDepartment).PersisterClass=TRTTIPersister);
end;


procedure TTestJSONMapping.TestAutoCreatedIsReplaced;

begin
  Load('{"classes":{"TMapDepartment":{"table":"DEPT"}}}', TRTTIPersister);
  FReg.RegisterPersister(TMapDepartment, TRTTIPersister);
  AssertEquals('One definition', 1, FReg.PersisterCount);
  AssertFalse('No longer auto-created', Def(TMapDepartment).AutoCreated);
  AssertEquals('Mapping applied to the new definition', 'DEPT', Def(TMapDepartment).TableName);
end;


procedure TTestJSONMapping.TestDuplicateRegistrationStillRaises;

var
  lRaised: Boolean;

begin
  FReg.RegisterPersister(TMapDepartment, TRTTIPersister);
  lRaised:=False;
  try
    FReg.RegisterPersister(TMapDepartment, TRTTIPersister);
  except
    on E: EOPFRegistry do
      lRaised:=True;
  end;
  AssertTrue('Second registration raises', lRaised);
end;


procedure TTestJSONMapping.TestAutoCreateUnknownClassRaises;

begin
  AssertTrue('Unknown class raises', LoadRaises('{"classes":{"TNoSuchMappedClass":{}}}', TRTTIPersister));
end;


procedure TTestJSONMapping.TestQualifiedClassName;

begin
  Load('{"classes":{"utc.opf.mapping.json.TMapDepartment":{"table":"DEPT"}}}', TRTTIPersister);
  AssertEquals('Table name', 'DEPT', Def(TMapDepartment).TableName);
  AssertTrue('Wrong unit raises', LoadRaises('{"classes":{"nosuchunit.TMapTag":{}}}', TRTTIPersister));
end;


procedure TTestJSONMapping.TestRelationFromMapping;

begin
  FReg.RegisterPersister(TMapDepartment, TRTTIPersister);
  FReg.RegisterPersister(TMapEmployee, TRTTIPersister);
  Load('{"classes":{"TMapEmployee":{"relations":[{"parent":"TMapDepartment","foreignKey":"DepartmentID",'+
       '"constraint":"FK_EMP_DEPT","dbCascade":true,"listProperty":"Employees"}]}}}');
  AssertEquals('One relation', 1, Relations.RelationCount);
  AssertTrue('Parent', Relations[0].ParentClass=TMapDepartment);
  AssertEquals('Constraint', 'FK_EMP_DEPT', Relations[0].ConstraintName);
  AssertTrue('Cascade', Relations[0].DBCascade);
  AssertEquals('List property', 'Employees', Relations[0].ListPropertyName);
end;


procedure TTestJSONMapping.TestRelationReplacesCodeRelation;

begin
  FReg.RegisterPersister(TMapDepartment, TRTTIPersister);
  FReg.RegisterPersister(TMapEmployee, TRTTIPersister);
  Relations.RegisterRelationship(TMapDepartment, TMapEmployee, 'DepartmentID', False, 'Staff');
  Load('{"classes":{"TMapEmployee":{"relations":[{"parent":"TMapDepartment","foreignKey":"DepartmentID","dbCascade":true}]}}}');
  AssertEquals('Still one relation', 1, Relations.RelationCount);
  AssertTrue('Cascade replaced', Relations[0].DBCascade);
  AssertEquals('Absent list property kept', 'Staff', Relations[0].ListPropertyName);
end;


procedure TTestJSONMapping.TestRelationWaitsForParent;

begin
  Load('{"classes":{"TMapEmployee":{"relations":[{"parent":"TMapDepartment","foreignKey":"DepartmentID"}]}}}');
  FReg.RegisterPersister(TMapEmployee, TRTTIPersister);
  AssertEquals('No relation before the parent', 0, Relations.RelationCount);
  FReg.RegisterPersister(TMapDepartment, TRTTIPersister);
  AssertEquals('Relation once the parent is registered', 1, Relations.RelationCount);
end;


procedure TTestJSONMapping.TestRelationForeignKeyByProperty;

begin
  FReg.RegisterPersister(TMapDepartment, TRTTIPersister);
  FReg.RegisterPersister(TMapEmployee, TRTTIPersister);
  Load('{"classes":{"TMapEmployee":{"fields":{"DepartmentID":{"column":"DEPT_ID"}},'+
       '"relations":[{"parent":"TMapDepartment","foreignKey":"DepartmentID"}]}}}');
  AssertEquals('Foreign key is the column', 'DEPT_ID', Relations[0].ForeignKeyFieldName);
end;


procedure TTestJSONMapping.TestRelationUnknownParentRaisesOnAutoCreate;

begin
  AssertTrue('Unknown parent raises',
    LoadRaises('{"classes":{"TMapEmployee":{"relations":[{"parent":"TNoSuchParent","foreignKey":"DepartmentID"}]}}}',
               TRTTIPersister));
end;


procedure TTestJSONMapping.TestManyToMany;

begin
  FReg.RegisterPersister(TMapEmployee, TRTTIPersister);
  FReg.RegisterPersister(TMapTag, TRTTIPersister);
  Load('{"classes":{},"manyToMany":[{"classA":"TMapEmployee","classB":"TMapTag","junctionTable":"EMP_TAG",'+
       '"foreignKeyA":"EMP_ID","foreignKeyB":"TAG_ID","listPropertyA":"Tags"}]}');
  AssertEquals('One link', 1, FM2M.DefCount);
  AssertEquals('Junction table', 'EMP_TAG', FM2M[0].JunctionTable);
  AssertEquals('List property A', 'Tags', FM2M[0].ListPropertyNameA);
  Load('{"manyToMany":[{"classA":"TMapEmployee","classB":"TMapTag","junctionTable":"EMP_TAGS",'+
       '"foreignKeyA":"EMP_ID","foreignKeyB":"TAG_ID"}]}');
  AssertEquals('Still one link', 1, FM2M.DefCount);
  AssertEquals('Junction table replaced', 'EMP_TAGS', FM2M[0].JunctionTable);
end;


procedure TTestJSONMapping.TestFailedMappingUndoesRegistration;

var
  lRaised: Boolean;

begin
  Load('{"classes":{"TMapEmployee":{"fields":{"Bogus":{"column":"X"}}}}}');
  lRaised:=False;
  try
    FReg.RegisterPersister(TMapEmployee, TRTTIPersister);
  except
    on E: EOPFMapping do
      lRaised:=True;
  end;
  AssertTrue('Registration raises', lRaised);
  AssertNull('Class is not registered', FReg.FindPersister(TMapEmployee));
end;


procedure TTestJSONMapping.TestFreeRemovesListener;

begin
  Load('{"classes":{}}');
  AssertNotNull('Listener set', Pointer(FReg.RegistrationListener));
  FreeAndNil(FMapping);
  AssertNull('Listener removed', Pointer(FReg.RegistrationListener));
end;


function TTestJSONMapping.SaveDefault(const aClasses: array of TClass): TJSONObject;

var
  lStream: TStringStream;

begin
  lStream:=TStringStream.Create('');
  try
    SaveDefaultMapping(lStream, aClasses, TRTTIPersister);
    Result:=GetJSON(lStream.DataString) as TJSONObject;
  finally
    lStream.Free;
  end;
end;


procedure TTestJSONMapping.TestSaveDefaultMapping;

var
  lDoc: TJSONObject;
  lFields: TJSONObject;

begin
  lDoc:=SaveDefault([TMapEmployee, TMapDepartment]);
  try
    AssertEquals('Two classes', 2, lDoc.Objects['classes'].Count);
    lFields:=lDoc.Objects['classes'].Objects['TMapEmployee'].Objects['fields'];
    AssertTrue('ID is the key by convention', ArrayHasString(lFields.Objects['ID'].Arrays['options'], 'foInKey'));
    AssertFalse('Name is not a key', ArrayHasString(lFields.Objects['Name'].Arrays['options'], 'foInKey'));
  finally
    lDoc.Free;
  end;
  AssertEquals('Registry of the test is not used', 0, FReg.PersisterCount);
end;


procedure TTestJSONMapping.TestSaveDefaultMappingMarksFirstField;

var
  lDoc: TJSONObject;
  lFields: TJSONObject;

begin
  lDoc:=SaveDefault([TMapNoConvKey]);
  try
    lFields:=lDoc.Objects['classes'].Objects['TMapNoConvKey'].Objects['fields'];
    AssertTrue('First field is the key', ArrayHasString(lFields.Objects['PersonNr'].Arrays['options'], 'foInKey'));
    AssertFalse('Second field is not a key', ArrayHasString(lFields.Objects['Name'].Arrays['options'], 'foInKey'));
  finally
    lDoc.Free;
  end;
end;


{$IFNDEF VER3_2}
procedure TTestJSONMapping.TestAttributeRelationOfReplacedClass;

begin
  Load('{"classes":{"TMapDepartment":{},"TMapAttrEmployee":{"relations":[{"parent":"TMapDepartment",'+
       '"foreignKey":"DepartmentID","constraint":"FK_FILE"}]}}}', TAttributePersister);
  AssertEquals('One relation after loading', 1, Relations.RelationCount);
  FReg.RegisterPersister(TMapAttrEmployee, TAttributePersister);
  AssertEquals('Two definitions', 2, FReg.PersisterCount);
  AssertEquals('Still one relation', 1, Relations.RelationCount);
  AssertEquals('Constraint of the mapping', 'FK_FILE', Relations[0].ConstraintName);
end;
{$ENDIF}


initialization
  RegisterClasses([TMapDepartment, TMapEmployee, TMapNoConvKey, TMapTag]);
{$IFNDEF VER3_2}
  RegisterClass(TMapAttrEmployee);
{$ENDIF}
  RegisterTest(TTestJSONMapping);
end.
