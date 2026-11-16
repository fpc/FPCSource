unit utc.opf.fields;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.TypInfo, FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, typinfo, fpcunit, testregistry,
{$ENDIF}
  Opf.Types, Opf.Fields;

type

  { TTestOPFieldMap }

  TTestOPFieldMap = class(TTestCase)
  private
    class procedure AssignDefValues(F: TOPFieldMap);
    procedure Testprops(F: TOPFieldMap);
  protected
    FField : TOPFieldMap;
    procedure SetUp; override;
    procedure TearDown; override;
    Property Field : TOPFieldMap Read FField;
  published
    procedure TestEmpty;
    procedure TestAssign;
    Procedure TestFieldNameDefault;
    Procedure TestDisplayName;
  end;

  { TTestOPFieldMapList }

  TTestOPFieldMapList = class(TTestCase)
  private
  protected
    FList : TOPFieldMapList;
    Procedure Fill;
    Procedure DoAdd;
    procedure SetUp; override;
    procedure TearDown; override;
    Property List : TOPFieldMapList Read FList;
  published
    procedure TestEmpty;
    Procedure TestAdd;
    Procedure TestFill;
    Procedure TestIndexOfFieldName;
    Procedure TestFindFieldName;
    Procedure TestIndexOfPropertyName;
    Procedure TestFindPropertyName;
    Procedure TestDuplicate;
    Procedure TestAsArray;
    Procedure TestAsArrayChange;
    Procedure TestEnumerator;
    Procedure TestAddDefault;
    Procedure TestFindVersionField;
    Procedure TestDuplicateVersionRaises;
    Procedure TestVersionFieldBadTypeRaises;
    Procedure TestVersionFieldInKeyRaises;
  end;

implementation

{ TTestOPFieldMapList }

procedure TTestOPFieldMapList.Fill;
begin
  doAdd;
  List.AddField('F2','N2',[foInInsert]).TableName:='a';
  List.AddField('F3','N3',[foInUpdate]).TableName:='a';
end;

procedure TTestOPFieldMapList.DoAdd;
begin
  List.AddField('F1','N1',[foInKey]).TableName:='a';
end;

procedure TTestOPFieldMapList.SetUp;
begin
  inherited SetUp;
  FList:=TOPFieldMapList.Create(TOPFieldMap);
end;

procedure TTestOPFieldMapList.TearDown;
begin
  FreeAndNil(FList);
  inherited TearDown;
end;

procedure TTestOPFieldMapList.TestEmpty;
begin
  AssertNotNull('Have list',List);
  AssertEquals('Empty',0,List.Count);
end;

procedure TTestOPFieldMapList.TestAdd;

Var
  F : TOPFieldMap;

begin
  F:=List.AddField('F1','N1',[foInKey]);
  F.TableName:='a';
  AssertEquals('List count',1,List.Count);
  AssertSame('Field OK',F,List[0]);
  AssertEquals('Fieldname','F1',F.FieldName);
  AssertEquals('PropertyName','N1',F.PropertyName);
  AssertEquals('MinSize',0,F.MinSize);
  AssertEquals('MaxSize',0,F.MaxSize);
  AssertEquals('SequenceName','',F.SequenceName);
  AssertEquals('TableName','a',F.TableName);
  AssertTrue('options',[foInKey]=F.Options);
  AssertNull('Data',F.Data);
  AssertEquals('ReferencedPropertyName','N1',F.ReferencedPropertyName);
  AssertNull('ReferencedPropertyName',F.PropertyTypeInfo);
end;

procedure TTestOPFieldMapList.TestFill;
begin
  Fill;
  AssertEquals('Count',3,List.Count);
  AssertEquals('1','F1',List[0].FieldName);
  AssertEquals('2','F2',List[1].FieldName);
  AssertEquals('3','F3',List[2].FieldName);
end;

procedure TTestOPFieldMapList.TestIndexOfFieldName;
begin
  Fill;
  AssertEquals('Nonexist',-1,List.indexOfFieldName('N1'));
  AssertEquals('First ',0,List.indexOfFieldName('F1'));
  AssertEquals('First CI',0,List.indexOfFieldName('f1'));
  AssertEquals('Last ',2,List.indexOfFieldName('F3'));
  AssertEquals('Last CI',2,List.indexOfFieldName('f3'));
  List[1].FieldName:='';
  AssertEquals('Middle, prop ',1,List.indexOfFieldName('N2'));
  AssertEquals('Middle, prop, CI',1,List.indexOfFieldName('n2'));

end;

procedure TTestOPFieldMapList.TestFindFieldName;
begin
  Fill;
  AssertSame('Nonexist',Nil,List.FindByFieldName('N1'));
  AssertSame('First ',List[0],List.FindByFieldName('F1'));
  AssertSame('First CI',List[0],List.FindByFieldName('f1'));
  AssertSame('Last ',List[2],List.FindByFieldName('F3'));
  AssertSame('Last CI',List[2],List.FindByFieldName('f3'));
  List[1].FieldName:='';
  AssertSame('Middle, prop ',List[1],List.FindByFieldName('N2'));
  AssertSame('Middle, prop, CI',List[1],List.FindByFieldName('n2'));
end;

procedure TTestOPFieldMapList.TestIndexOfPropertyName;
begin
  Fill;
  AssertEquals('Nonexist',-1,List.indexOfPropertyName('F1'));
  AssertEquals('First ',0,List.indexOfPropertyName('N1'));
  AssertEquals('First CI',0,List.indexOfPropertyName('n1'));
  AssertEquals('Last ',2,List.indexOfPropertyName('N3'));
  AssertEquals('Last CI',2,List.indexOfPropertyName('n3'));
end;

procedure TTestOPFieldMapList.TestFindPropertyName;
begin
  Fill;
  AssertSame('Nonexist',Nil,List.FindByPropertyName('F1'));
  AssertSame('First ',List[0],List.FindByPropertyName('N1'));
  AssertSame('First CI',List[0],List.FindByPropertyName('n1'));
  AssertSame('Last ',List[2],List.FindByPropertyName('N3'));
  AssertSame('Last CI',List[2],List.FindByPropertyName('n3'));
end;

procedure TTestOPFieldMapList.TestDuplicate;
begin
  Fill;
  AssertException('No duplicates',EFieldMap,@DoAdd)
end;

procedure TTestOPFieldMapList.TestAsArray;

Var
  A : TOPFieldMapArray;
  I : integer;

begin
  Fill;
  A:=List.AsArray;
  AssertEquals('Length',List.Count,Length(A));
  For I:=0 to List.Count-1 do
    AssertSame('Item '+IntToStr(I),List[i],A[i]);
end;

procedure TTestOPFieldMapList.TestAsArrayChange;
Var
  A : TOPFieldMapArray;
  I : integer;

begin
  Fill;
  A:=List.AsArray;
  AssertEquals('1 Length',List.Count,Length(A));
  For I:=0 to List.Count-1 do
    AssertSame('1 Item '+IntToStr(I),List[i],A[i]);
  List.Delete(2);
  A:=List.AsArray;
  AssertEquals('2 Length',List.Count,Length(A));
  For I:=0 to List.Count-1 do
    AssertSame('2 Item '+IntToStr(I),List[i],A[i]);
  List.AddField('FF','PP',[]);
  List.AddField('FA','PA',[]);
  A:=List.AsArray;
  AssertEquals('3 Length',List.Count,Length(A));
  For I:=0 to List.Count-1 do
    AssertSame('3 Item '+IntToStr(I),List[i],A[i]);
end;

procedure TTestOPFieldMapList.TestEnumerator;
Var
  A : TOPFieldMap;
  I : integer;

begin
  Fill;
  I:=0;
  For A in List do
    begin
    AssertSame('Item '+IntToStr(I),List[i],A);
    Inc(I);
    end;
end;

procedure TTestOPFieldMapList.TestAddDefault;
begin
  List.DefaultFieldOptions:=[foInInsert];
  AssertTrue('Correct options',[foInInsert]=List.AddField('a','b').Options);
end;


{ TTestOPFieldMap }

procedure TTestOPFieldMap.SetUp;

begin
  inherited SetUp;
  FField:=TOPFieldMap.Create(Nil);
  AssignDefValues(FField);
end;

class procedure TTestOPFieldMap.AssignDefValues(F : TOPFieldMap);

begin
  F.FieldName:='F1';
  F.SequenceName:='G1';
  F.PropertyName:='N1';
  F.Options:=[foInUpdate];
  F.MinSize:=1;
  F.MaxSize:=2;
  F.Data:=Pointer(PtrInt(23));
  F.TableName:='a';
  F.ReferencedPropertyName:='R';
  F.PropertyTypeInfo:=TypeInfo(TOPFieldMap);
  // Overwrites F.PropertyTypeInfo !
  F.PropertyKind:=tkInteger;
end;

procedure TTestOPFieldMap.TearDown;
begin
  FreeAndNil(FField);
  inherited TearDown;
end;

procedure TTestOPFieldMap.Testprops(F: TOPFieldMap);

begin
  AssertEquals('TableName','a',Field.TableName);
  AssertEquals('FieldName','F1',Field.FieldName);
  AssertEquals('PropertyName','N1',Field.PropertyName);
  AssertEquals('SequenceName','G1',Field.SequenceName);
  Asserttrue('PropertyType',tkInteger=F.PropertyKind);
  Asserttrue('Option',Field.Options=[foInUpdate]);
  AssertEquals('MinSize',1,Field.MinSize);
  AssertEquals('MaxSize',2,Field.MaxSize);
  AssertSame('Data',Pointer(PtrInt(23)),Field.Data);
  AssertEquals('ReferencedPropertyName','R',F.ReferencedPropertyName);
  AssertSame('TypeInfo',  TypeInfo(TOPFieldMap), F.PropertyTypeInfo);
end;

procedure TTestOPFieldMap.TestEmpty;
begin
  AssertNotNull('Have field Field');
  TestProps(Field);
end;

procedure TTestOPFieldMap.TestAssign;

Var
  F : TOPFieldMap;

begin
  F:=TOPFieldMap.Create(Nil);
  try
    F.Assign(Field);
    TestProps(F);
  finally
    F.Free;
  end;
end;

procedure TTestOPFieldMap.TestFieldNameDefault;
begin
  Field.FieldName:='';
  AssertEquals('Fieldame','N1',Field.FieldName);
end;

procedure TTestOPFieldMap.TestDisplayName;
begin
  AssertEquals('Get',Field.PropertyName,Field.DisplayName);
  Field.DisplayName:='A';
  // Cannot set displayname.
  AssertEquals('Set',Field.PropertyName,Field.DisplayName);
end;

procedure TTestOPFieldMapList.TestFindVersionField;
var
  F: TOPFieldMap;
begin
  F := List.AddField('ver', 'Version');
  F.PropertyKind := tkInteger;
  F.Options := [foInSelect, foInInsert, foInUpdate, foVersion];
  AssertNotNull('FindVersionField', List.FindVersionField);
  AssertEquals('Correct field', 'Version', List.FindVersionField.PropertyName);
end;

procedure TTestOPFieldMapList.TestDuplicateVersionRaises;
var
  F: TOPFieldMap;
begin
  F := List.AddField('ver1', 'Version1');
  F.PropertyKind := tkInteger;
  F.Options := [foInSelect, foInInsert, foInUpdate, foVersion];
  F := List.AddField('ver2', 'Version2');
  F.PropertyKind := tkInteger;
  try
    F.Options := [foInSelect, foInInsert, foInUpdate, foVersion];
    Fail('Expected exception for duplicate foVersion');
  except
    on E: EFieldMap do
      ; // expected
  end;
end;

procedure TTestOPFieldMapList.TestVersionFieldBadTypeRaises;
var
  F: TOPFieldMap;
begin
  F := List.AddField('ver', 'Version');
  F.PropertyKind := tkFloat;
  try
    F.Options := [foInSelect, foInInsert, foInUpdate, foVersion];
    Fail('Expected exception for bad version field type');
  except
    on E: EFieldMap do
      ; // expected
  end;
end;

procedure TTestOPFieldMapList.TestVersionFieldInKeyRaises;
var
  F: TOPFieldMap;
begin
  F := List.AddField('ver', 'Version');
  F.PropertyKind := tkInteger;
  try
    F.Options := [foInKey, foInSelect, foInInsert, foInUpdate, foVersion];
    Fail('Expected exception for foVersion + foInKey');
  except
    on E: EFieldMap do
      ; // expected
  end;
end;

initialization
  RegisterTests([TTestOPFieldMap,TTestOPFieldMapList]);
end.

