unit tcjsontocode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, fpjsontopas;

type

  { TTestGenCode }

  TTestGenCode= class(TTestCase)
  private
    FPos : Integer;
    FGen: TJSONToPascal;
    procedure AssertDelphiLoadArray(AElementType, AJSONtype: String);
    procedure AssertDelphiPropertyAssignmentLoop;
    procedure AssertDestructorImplementation(AClassName: String; ObjectFields: array of string);
    procedure AssertLine(Msg: String; AExpected: String);
    procedure GenCode(AJSON: String);
    class function GetDataName(IsDelphi: Boolean): string;
    function NextLine: String;
    function Pos(const What, Where: String): Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure AssertArrayCreator(const ArrayTypeName, AElementType: String; IsDelphi: Boolean=False);
    procedure AssertArraySaver(const ArrayTypeName, AElementType: String; IsDelphi: Boolean=False);
    procedure AssertArrayCreatorImplementation(const ArrayTypeName, AElementType: String; AObjectName: String=''; IsDelphi: Boolean=False);
    procedure AssertArraySaverImplementation(const ArrayTypeName, AElementType: String; AObjectName: String=''; IsDelphi: Boolean=False);
    procedure AssertLoadArray(AElementType, AJSONtype: String; IsDelphi : Boolean = False);
    procedure AssertSaveArray(AElementType, AJSONtype: String; IsDelphi: Boolean = False);
    procedure AssertPropertyAssignmentLoop;
    procedure AssertType;
    procedure AssertClassComment(const Msg, AName: String);
    procedure AssertLoadConstructorDeclaration(AType: String);
    procedure AssertLoaderDeclaration(AType: String);
    procedure AssertSaverDeclaration;
    procedure AssertLoaderImplementationEnd(IsDelphi : Boolean = False);
    procedure AssertLoadConstructorImplementationStart(Const ATypeName, ADataName: String);
    procedure AssertLoaderImplementationStart(Const ATypeName, ADataName: String; IsDelphi : Boolean = False);
    procedure AssertSaverImplementationStart(Const ATypeName: String; IsDelphi : Boolean = False);
    procedure AssertArrayLoaderImplementationStart(Const ATypeName, ADataName, ArrayName, ArrayTypeName, ArrayElementType : String; IsDelphi : Boolean = False);
    procedure AssertObjectLoaderImplementationStart(Const ATypeName, ADataName, ArrayName, ArrayTypeName, ArrayElementType : String; IsDelphi : Boolean = False);
    Procedure AssertUnitHeader;
    Procedure AssertBegin;
    Procedure AssertEnd(Const Msg : String = '');
    Procedure AssertUnitEnd;
    Procedure AssertImplementation;
    procedure AssertProperty(const AName, AType: String; Setter : Boolean = False);
    procedure AssertSetter(const AName, AType: String);
    Procedure AssertClassHeader(Const AName : String; AParentName : String);
    Procedure AssertSetterImplementation(Const AClassType,AName,AType : String; IsObject : Boolean = False);
    Procedure AssertVisibility(Const AVisibility : String);
    Procedure AssertDestructor;
    Procedure AssertField(Const AName,AType : String; Prefix : String = '');
    Procedure AssertArrayType(Const AName,AItemType : String);
    Procedure AssertPropertyMap(Const APath,ATypeName,APropertyName,AParentTypeName : String);
    Property Gen : TJSONToPascal Read FGen;
  published
    procedure TestEmpty;
    Procedure TestSimple;
    Procedure TestClassName;
    Procedure TestParentClassName;
    Procedure TestIntegerProperty;
    Procedure Test2IntegersProperty;
    Procedure TestBooleanProperty;
    Procedure TestStringProperty;
    Procedure TestFloatProperty;
    Procedure TestInt64Property;
    Procedure TestPropertySetter;
    Procedure TestObjectProperty;
    Procedure TestObjectPropertySetter;
    Procedure TestObjectPropertySuffix;
    Procedure TestObjectPropertySkip;
    Procedure TestObjectPropertyRecurse;
    Procedure TestObjectPropertyRecurseSuffix;
    Procedure TestObjectPropertyRecurseSkip;
    Procedure TestObjectPropertyRecurseSkipB;
    Procedure TestStringArrayProperty;
    Procedure TestIntegerArrayProperty;
    Procedure TestBooleanArrayProperty;
    Procedure TestFloatArrayProperty;
    Procedure TestInt64ArrayProperty;
    Procedure TestStringArrayPropertySuffix;
    Procedure TestObjectArrayProperty;
    procedure TestObjectArrayPropertySuffix;
    procedure TestArrayArrayProperty;
    procedure TestObjectArrayArrayProperty;
    Procedure TestLoadIntegerProperty;
    Procedure TestLoad2IntegersProperty;
    Procedure TestLoadIntegerWithErrorProperty;
    Procedure TestLoadIntegerCaseInsensitiveProperty;
    Procedure TestLoadStringProperty;
    Procedure TestLoadBooleanProperty;
    Procedure TestLoadInt64Property;
    Procedure TestLoadFloatProperty;
    Procedure TestLoadObjectProperty;
    Procedure TestLoadStringArrayProperty;
    Procedure TestLoadBooleanArrayProperty;
    Procedure TestLoadIntegerArrayProperty;
    Procedure TestLoadInt64ArrayProperty;
    Procedure TestLoadFloatArrayProperty;
    Procedure TestLoadObjectArrayProperty;
    Procedure TestLoadDelphiIntegerProperty;
    Procedure TestLoadDelphi2IntegersProperty;
    Procedure TestLoadDelphiIntegerWithErrorProperty;
    Procedure TestLoadDelphiIntegerCaseInsensitiveProperty;
    Procedure TestLoadDelphiStringProperty;
    Procedure TestLoadDelphiBooleanProperty;
    Procedure TestLoadDelphiInt64Property;
    Procedure TestLoadDelphiFloatProperty;
    procedure TestLoadDelphiObjectProperty;
    Procedure TestLoadDelphiStringArrayProperty;
    Procedure TestLoadDelphiBooleanArrayProperty;
    Procedure TestLoadDelphiIntegerArrayProperty;
    Procedure TestLoadDelphiInt64ArrayProperty;
    Procedure TestLoadDelphiFloatArrayProperty;
    procedure TestLoadDelphiObjectArrayProperty;
    Procedure TestSaveIntegerProperty;
    Procedure TestSave2IntegersProperty;
    Procedure TestSaveStringProperty;
    Procedure TestSaveBooleanProperty;
    Procedure TestSaveInt64Property;
    Procedure TestSaveFloatProperty;
    Procedure TestSaveObjectProperty;
    Procedure TestSaveStringArrayProperty;
    Procedure TestSaveBooleanArrayProperty;
    Procedure TestSaveIntegerArrayProperty;
    Procedure TestSaveInt64ArrayProperty;
    Procedure TestSaveFloatArrayProperty;
    Procedure TestSaveObjectArrayProperty;
    Procedure TestSaveDelphiIntegerProperty;
    Procedure TestSaveDelphi2IntegersProperty;
    Procedure TestSaveDelphiStringProperty;
    Procedure TestSaveDelphiBooleanProperty;
    Procedure TestSaveDelphiInt64Property;
    Procedure TestSaveDelphiFloatProperty;
    Procedure TestSaveDelphiObjectProperty;
    Procedure TestSaveDelphiStringArrayProperty;
    Procedure TestSaveDelphiBooleanArrayProperty;
    Procedure TestSaveDelphiIntegerArrayProperty;
    Procedure TestSaveDelphiInt64ArrayProperty;
    Procedure TestSaveDelphiFloatArrayProperty;
    Procedure TestSaveDelphiObjectArrayProperty;
  end;

Var
  TestUnitDir : String;

implementation

procedure TTestGenCode.SetUp;
begin
  FGen:=TJSONToPascal.Create(Nil);
end;

procedure TTestGenCode.TearDown;
begin
  FreeAndNil(FGen)
end;

function TTestGenCode.NextLine: String;

begin
  Result:='';
  While (Result='') do
    begin
    Inc(FPos);
    AssertTrue('In scope',FPos<FGen.Code.Count);
    Result:=Trim(FGen.Code[FPos]);
    end;
end;

procedure TTestGenCode.AssertUnitHeader;

Var
  S: String;

begin
  S:=NextLine;
  AssertTrue('Have unit',Pos('unit ',S)=1);
  S:=NextLine;
  AssertTrue('Have interface',Pos('interface',S)=1);
  S:=NextLine;
  AssertTrue('Have uses',Pos('uses ',S)=1);
  S:=NextLine;
  AssertTrue('Type line',Pos('Type',S)=1);
end;

procedure TTestGenCode.AssertBegin;
begin
  AssertTrue('Have begin',pos('begin',nextline)>0);
end;

procedure TTestGenCode.AssertEnd(const Msg: String);
begin
  AssertTrue('Have end:'+Msg,pos('end;',nextline)>0);
end;

procedure TTestGenCode.AssertUnitEnd;
begin
  AssertTrue('Have end.',pos('end.',nextline)>0);
end;

procedure TTestGenCode.AssertImplementation;
begin
  AssertTrue('Have implementation',CompareText(NextLine,'implementation')=0);
end;

function TTestGenCode.Pos(const What, Where: String): Integer;

begin
  Result:=system.Pos(lowercase(what),lowercase(where));
end;

procedure TTestGenCode.AssertClassComment(const Msg,AName: String);

Var
  S : String;

begin
  S:=NextLine;
  AssertTrue(Msg+' ('+AName+'): Class header comment start',Pos('{ --',S)>0);
  S:=NextLine;
  AssertTrue(Msg+' ('+AName+'): Class header comment class nam',Pos(AName,S)>0);
  S:=NextLine;
  AssertTrue(Msg+' ('+AName+'): Class header comment end',Pos('}',S)>0);
end;

procedure TTestGenCode.AssertClassHeader(const AName: String; AParentName: String);

Var
  P : Integer;
  S : String;

begin
  AssertClassComment('Class declarationheader for '+AName,AName);
  S:=NextLine;
  P:=Pos(AName+' = class(',S);
  AssertTrue('class type ',P>0);
  P:=Pos(AParentName+')',S);
  AssertTrue('Class parent type ',P>0);
  AssertVisibility('private');
end;

procedure TTestGenCode.AssertSetterImplementation(const AClassType, AName,
  AType: String; IsObject: Boolean);

Var
  S,PS : String;
  P : Integer;

begin
  S:=NextLine;
  PS:='Procedure '+AClassType+'.Set'+Aname+'(AValue';
  AssertTrue('Have declaration start',Pos(PS,S)>0);
  Delete(S,1,Length(PS));
  P:=Pos(':',S);
  AssertTrue('Have colon' ,p>0);
  Delete(S,1,P);
  AssertTrue('Have type',Pos(AType,S)>0);
  AssertTrue('Have );',Pos(');',S)>0);
  AssertTrue('Terminated on semicolon',S[Length(S)]=';');
  AssertBegin;
  AssertTrue('Have change check',Pos('if ('+Gen.FieldPrefix+AName+'=AValue) then exit;',NextLine)>0);
  if IsObject then
    AssertTrue('Have free of previous value',Pos('FreeAndNil('+Gen.FieldPrefix+AName+');',NextLine)>0);
  AssertTrue('Have Assignment',Pos(Gen.FieldPrefix+AName+':=AValue;',NextLine)>0);
  AssertEnd;
end;

procedure TTestGenCode.AssertVisibility(const AVisibility: String);

begin
  AssertTrue('Have visibility section '+AVisibility,Pos(AVisibility,NextLine)>0);
end;

procedure TTestGenCode.AssertDestructor;
begin
  AssertTrue('Have destructor declaration',Pos('Destructor Destroy; override;',NextLine)>0);
end;


procedure TTestGenCode.AssertDestructorImplementation(AClassName: String;
  ObjectFields: array of string);

Var
  F : String;

begin
  AssertTrue('Have destructor implementation',Pos(Format('Destructor %s.Destroy;',[AClassName]),NextLine)>0);
  AssertBegin;
  For F in ObjectFields do
    AssertTrue('Have destructor for F'+F,Pos('FreeAndNil(F'+F+');',NextLine)>0);
  AssertTrue('Have inherited call'+F,Pos('Inherited;',NextLine)>0);
  AssertEnd;
end;

procedure TTestGenCode.AssertField(const AName, AType: String; Prefix : String = '');

Var
  F,S : String;
  P : Integer;

begin
  F:=Prefix;
  if F='' then
    F:='F';
  S:=NextLine;
  AssertTrue('Field Name',Pos(F+AName,S)=1);
  P:=Pos(':',S);
  AssertTrue('Colon after field name',P>Length(F+AName));
  AssertTrue('Field type after colon',Pos(AType,S)>P);
  AssertTrue('Terminated on semicolon',S[Length(S)]=';');
end;

procedure TTestGenCode.AssertSetter(const AName, AType: String);

Var
  N,S,PD : String;
  P,p2 : Integer;

begin
  S:=NextLine;
  N:='Setter declaration for '+AName+' : ';
  PD:='Procedure Set'+AName;
  AssertTrue(N+'Setter name',Pos(PD,S)=1);
  P:=Pos('(',S);
  AssertTrue(N+'( after parameter name',P>Length(PD));
  P:=Pos(':',S);
  AssertTrue(N+'Colon after parameter name',P>Length(PD));
  Delete(S,1,P);
  P2:=Pos(AType,S);
  AssertTrue(N+'Field type after colon '+AType+' : '+S,P2>0);
  P:=Pos(');',S);
  AssertTrue(N+'); type after parameter type',P>P2);
  P2:=Pos('virtual',S);
  AssertTrue(N+'virtual after ); ',P2>P);
  AssertTrue(N+'Terminated on semicolon',S[Length(S)]=';');
end;

procedure TTestGenCode.AssertArrayType(const AName, AItemType: String);

Var
  P,p2 : Integer;
  S : String;

begin
  S:=NextLine;
  AssertTrue('Type Name',Pos(AName,S)=1);
  P:=Pos('=',S);
  AssertTrue('Equal token after type Name',P>Pos(AName,S));
  P2:=Pos('Array of',S);
  AssertTrue('Array of after Equal token after type Name',P2>P);
  P:=Pos(AItemType,S);
  AssertTrue('Item type name after array of',P>P2);
  AssertTrue('Terminated on semicolon',S[Length(S)]=';');
end;

procedure TTestGenCode.AssertPropertyMap(const APath, ATypeName, APropertyName,
  AParentTypeName: String);

Var
  M : TPropertyMapItem;

begin
  M:=Gen.PropertyMap.FindPath(APath);
  AssertNotNull('Have property map "'+APath+'"',M);
  AssertEquals('Have type name ',ATypeName,M.TypeName);
  AssertEquals('Have property name ',APropertyName,M.PropertyName);
  AssertEquals('Have parent type name ',AParentTypeName,M.ParentTypeName);
end;

procedure TTestGenCode.AssertProperty(const AName, AType: String; Setter : Boolean = False);

Var
  S : String;
  P,P2 : Integer;

begin
  S:=NextLine;
  AssertTrue('Property Name',Pos('Property '+AName,S)=1);
  P:=Pos(':',S);
  AssertTrue('Colon after property name',P>Length('Property '+AName));
  P2:=Pos(AType,S);
  AssertTrue('Field type after colon',P2>P);
  P:=pos(' read ',S);
  AssertTrue('Read specifier after type ',P>P2);
  P2:=Pos('F'+AName,S);
  AssertTrue('Field name for read specifier',P2>P);
  P:=pos(' write ',S);
  AssertTrue('Write specifier after type ',P>P2);
  if Setter Then
    P2:=Pos('write Set'+AName,S)
  else
    P2:=Pos('write F'+AName,S);
  AssertTrue('Field name for write specifier',P2>P);

  AssertTrue('Terminated on semicolon',S[Length(S)]=';');
end;


procedure TTestGenCode.GenCode(AJSON : String);

Var
  F : Text;

begin
  Gen.JSON:=AJSON;
  Gen.DestUnitName:='u'+TestName;
  Gen.Execute;
  if (TestUnitDir<>'') then
    begin
    Assign(F,IncludeTrailingPathDelimiter(TestUnitDir)+Gen.DestUnitName+'.pp');
    Rewrite(F);
    Writeln(F,'// ',Self.TestName);
    Writeln(F,Gen.Code.Text);
    Close(F);
    Assign(F,IncludeTrailingPathDelimiter(TestUnitDir)+Gen.DestUnitName+'.json');
    Rewrite(F);
    Writeln(F,AJSON);
    Close(F);
    end
  else
    begin
    Writeln('// ',Self.TestName);
    Writeln('(* JSON: '+AJSON+' *)');
    Writeln(Gen.Code.Text);
    end;

  FPos:=-1;
end;

procedure TTestGenCode.TestEmpty;
begin
  AssertNotNull('Have generator',Gen);
  AssertNotNull('Generator property map exists',Gen.PropertyMap);
  AssertNotNull('Generator property code exists',Gen.Code);
  AssertNull('Generator JSON empty',Gen.JSONData);
  AssertNull('Generator JSON stream empty',Gen.JSONStream);
  AssertEquals('Generator JSON empty','',Gen.JSON);
  AssertEquals('Generator property map empty',0,Gen.PropertyMap.Count);
end;

procedure TTestGenCode.TestSimple;
begin
  GenCode('{}');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertVisibility('public');
  AssertEnd;
  AssertPropertyMap('','TMyObject','','TObject');
end;

procedure TTestGenCode.TestClassName;
begin
  Gen.PropertyMap.AddPath('','TSomeObject');
  GenCode('{}');
  AssertUnitHeader;
  AssertClassHeader('TSomeObject','TObject');
  AssertVisibility('public');
  AssertEnd;
  AssertPropertyMap('','TSomeObject','','TObject');
end;

procedure TTestGenCode.TestParentClassName;
begin
  Gen.PropertyMap.AddPath('','TSomeObject');
  Gen.DefaultParentName:='TMyObject';
  GenCode('{}');
  AssertUnitHeader;
  AssertClassHeader('TSomeObject','TMyObject');
  AssertVisibility('public');
  AssertEnd;
  AssertPropertyMap('','TSomeObject','','TMyObject');
end;

procedure TTestGenCode.TestIntegerProperty;
begin
  GenCode('{ "a" : 1 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','integer');
  AssertVisibility('public');
  AssertProperty('a','integer');
  AssertEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Integer','a','');
end;

procedure TTestGenCode.Test2IntegersProperty;
begin
  GenCode('{ "a" : 1, "b" : 2 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','integer');
  AssertField('b','integer');
  AssertVisibility('public');
  AssertProperty('a','integer');
  AssertProperty('b','integer');
  AssertEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Integer','a','');
  AssertPropertyMap('b','Integer','b','');
end;

procedure TTestGenCode.TestBooleanProperty;
begin
  GenCode('{ "a" : true }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','boolean');
  AssertVisibility('public');
  AssertProperty('a','boolean');
  AssertEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Boolean','a','');
end;

procedure TTestGenCode.TestStringProperty;
begin
  GenCode('{ "a" : "abce" }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','string');
  AssertVisibility('public');
  AssertProperty('a','string');
  AssertEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','String','a','');
end;

procedure TTestGenCode.TestFloatProperty;
begin
  GenCode('{ "a" : 1.1 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','double');
  AssertVisibility('public');
  AssertProperty('a','double');
  AssertEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Double','a','');
end;

procedure TTestGenCode.TestInt64Property;
begin
  GenCode('{ "a" : 1234567890123 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','int64');
  AssertVisibility('public');
  AssertProperty('a','int64');
  AssertEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Int64','a','');
end;

procedure TTestGenCode.TestPropertySetter;
begin
  Gen.Options:=[jpoUseSetter];
  GenCode('{ "a" : 1234567890123 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','int64');
  AssertVisibility('protected');
  AssertSetter('A','int64');
  AssertVisibility('public');
  AssertProperty('a','int64',True);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertSetterImplementation('TMyObject','a','int64');
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Int64','a','');
end;

procedure TTestGenCode.TestObjectProperty;
begin
  GenCode('{ "a" : {} }');
  AssertUnitHeader;
  AssertClassHeader('TA','TObject');
  AssertVisibility('public');
  AssertEnd;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Ta');
  AssertVisibility('public');
  AssertDestructor;
  AssertProperty('a','Ta');
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Comment for class TA','Ta');
  AssertClassComment('Comment for class TMyObject','TMyObject');
  AssertDestructorImplementation('TMyObject',['a']);
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','TObject');
end;

procedure TTestGenCode.TestObjectPropertySetter;
begin
  Gen.Options:=[jpoUseSetter];
  GenCode('{ "a" : {} }');
  AssertUnitHeader;
  AssertClassHeader('TA','TObject');
  AssertVisibility('protected');
  AssertVisibility('public');
  AssertEnd;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Ta');
  AssertVisibility('protected');
  AssertSetter('a','Ta');
  AssertVisibility('Public');
  AssertDestructor;
  AssertProperty('a','Ta',True);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Comment for class TA','Ta');
  AssertClassComment('Comment for class TMyObject','TMyObject');
  AssertDestructorImplementation('TMyObject',['a']);
  AssertSetterImplementation('TMyObject','a','Ta',True);
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','TObject');
end;

procedure TTestGenCode.TestObjectPropertySuffix;
begin
  Gen.PropertyTypeSuffix:='Type';
  GenCode('{ "a" : {} }');
  AssertUnitHeader;
  AssertClassHeader('TAType','TObject');
  AssertVisibility('public');
  AssertEnd;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','TaType');
  AssertVisibility('public');
  AssertDestructor;
  AssertProperty('a','TaType');
  AssertEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','TaType','a','TObject');
end;

procedure TTestGenCode.TestObjectPropertySkip;
begin
  Gen.PropertyTypeSuffix:='Type';
  Gen.PropertyMap.AddPath('a','me').SkipType:=true;
  GenCode('{ "a" : {} }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','me');
  AssertVisibility('public');
  AssertDestructor;
  AssertProperty('a','me');
  AssertEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','me','a','');
end;

procedure TTestGenCode.TestObjectPropertyRecurse;
begin
  GenCode('{ "a" : { "b" : {} } }');
  AssertUnitHeader;
  AssertClassHeader('TAB','TObject');
  AssertVisibility('public');
  AssertEnd;
  AssertClassHeader('TA','TObject');
  AssertField('b','TaB');
  AssertVisibility('public');
  AssertDestructor;
  AssertProperty('b','TaB');
  AssertEnd;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Ta');
  AssertVisibility('public');
  AssertDestructor;
  AssertProperty('a','Ta');
  AssertEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','TObject');
  AssertPropertyMap('a.b','Tab','b','TObject');
end;

procedure TTestGenCode.TestObjectPropertyRecurseSuffix;
begin
  Gen.PropertyTypeSuffix:='Type';
  GenCode('{ "a" : { "b" : {} } }');
  AssertUnitHeader;
  AssertClassHeader('TABType','TObject');
  AssertVisibility('public');
  AssertEnd;
  AssertClassHeader('TAType','TObject');
  AssertField('b','TaBType');
  AssertVisibility('public');
  AssertDestructor;
  AssertProperty('b','TaBType');
  AssertEnd;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','TaType');
  AssertVisibility('public');
  AssertDestructor;
  AssertProperty('a','TaType');
  AssertEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','TaType','a','TObject');
  AssertPropertyMap('a.b','TabType','b','TObject');
end;

procedure TTestGenCode.TestObjectPropertyRecurseSkip;
begin
  Gen.PropertyMap.AddPath('a','me').SkipType:=true;
  GenCode('{ "a" : { "b" : {} } }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','me');
  AssertVisibility('public');
  AssertDestructor;
  AssertProperty('a','me');
  AssertEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','me','a','');
end;

procedure TTestGenCode.TestObjectPropertyRecurseSkipB;
begin
  Gen.PropertyMap.AddPath('a.b','me').SkipType:=true;
  GenCode('{ "a" : { "b" : {} } }');
  AssertUnitHeader;
  AssertClassHeader('TA','TObject');
  AssertField('b','me');
  AssertVisibility('public');
  AssertDestructor;
  AssertProperty('b','me');
  AssertEnd;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Ta');
  AssertVisibility('public');
  AssertDestructor;
  AssertProperty('a','Ta');
  AssertEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','TObject');
  AssertPropertyMap('a.b','me','b','');
end;

procedure TTestGenCode.TestStringArrayProperty;
begin
  GenCode('{ "a" : [ "" ] }');
  AssertUnitHeader;
  AssertArrayType('Ta','string');
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Ta');
  AssertVisibility('public');
  AssertProperty('a','Ta');
  AssertEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','');
  AssertPropertyMap('a[0]','String','','');
end;

procedure TTestGenCode.TestIntegerArrayProperty;
begin
  GenCode('{ "a" : [ 1 ] }');
  AssertUnitHeader;
  AssertArrayType('Ta','integer');
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Ta');
  AssertVisibility('public');
  AssertProperty('a','Ta');
  AssertEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','');
  AssertPropertyMap('a[0]','Integer','','');
end;

procedure TTestGenCode.TestBooleanArrayProperty;
begin
  GenCode('{ "a" : [ true ] }');
  AssertUnitHeader;
  AssertArrayType('Ta','Boolean');
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Ta');
  AssertVisibility('public');
  AssertProperty('a','Ta');
  AssertEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','');
  AssertPropertyMap('a[0]','Boolean','','');
end;

procedure TTestGenCode.TestFloatArrayProperty;
begin
  GenCode('{ "a" : [ 1.2 ] }');
  AssertUnitHeader;
  AssertArrayType('Ta','Double');
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Ta');
  AssertVisibility('public');
  AssertProperty('a','Ta');
  AssertEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','');
  AssertPropertyMap('a[0]','Double','','');
end;

procedure TTestGenCode.TestInt64ArrayProperty;
begin
  GenCode('{ "a" : [ 1234567890123 ] }');
  AssertUnitHeader;
  AssertArrayType('Ta','Int64');
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Ta');
  AssertVisibility('public');
  AssertProperty('a','Ta');
  AssertEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','');
  AssertPropertyMap('a[0]','Int64','','');
end;

procedure TTestGenCode.TestStringArrayPropertySuffix;
begin
  Gen.PropertyTypeSuffix:='Type';
  GenCode('{ "a" : [ "" ] }');
  AssertUnitHeader;
  AssertArrayType('TaType','string');
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','TaType');
  AssertVisibility('public');
  AssertProperty('a','TaType');
  AssertEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','TaType','a','');
  AssertPropertyMap('a[0]','String','','');
end;

procedure TTestGenCode.TestObjectArrayProperty;
begin
  GenCode('{ "a" : [ {} ] }');
  AssertUnitHeader;
  AssertClassHeader('TaItem','TObject');
  AssertVisibility('public');
  AssertEnd;
  AssertArrayType('Ta','TaItem');
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Ta');
  AssertVisibility('public');
  AssertProperty('a','Ta');
  AssertEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','');
  AssertPropertyMap('a[0]','TaItem','','TObject');
end;

procedure TTestGenCode.TestObjectArrayPropertySuffix;

begin
  Gen.PropertyTypeSuffix:='Type';
  GenCode('{ "a" : [ {} ] }');
  AssertUnitHeader;
  AssertClassHeader('TaItemType','TObject');
  AssertVisibility('public');
  AssertEnd;
  AssertArrayType('TaType','TaItemType');
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','TaType');
  AssertVisibility('public');
  AssertProperty('a','TaType');
  AssertEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','TaType','a','');
  AssertPropertyMap('a[0]','TaItemType','','TObject');
end;

procedure TTestGenCode.TestArrayArrayProperty;
begin
  GenCode('{ "a" : [ [ "" ] ] }');
  AssertUnitHeader;
  AssertArrayType('TaItem','String');
  AssertArrayType('Ta','TaItem');
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Ta');
  AssertVisibility('public');
  AssertProperty('a','Ta');
  AssertEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','');
  AssertPropertyMap('a[0]','TaItem','','');
  AssertPropertyMap('a[0][0]','String','','');
end;

procedure TTestGenCode.TestObjectArrayArrayProperty;
begin
  GenCode('{ "a" : [ [ {} ] ] }');
  AssertUnitHeader;
  AssertClassHeader('TaItemItem','TObject');
  AssertVisibility('public');
  AssertEnd;
  AssertArrayType('TaItem','TaItemItem');
  AssertArrayType('Ta','TaItem');
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','');
  AssertPropertyMap('a[0]','TaItem','','');
  AssertPropertyMap('a[0][0]','TaItemItem','','TObject');
end;

procedure TTestGenCode.AssertLoadConstructorDeclaration(AType: String);

Var
  S : String;

begin
  S:=NextLine;
  AssertTrue('Load Constructor declaration in '+S,Pos('Constructor CreateFromJSON(AJSON : '+AType+'); virtual;',S)>0);
end;

procedure TTestGenCode.AssertLoaderDeclaration(AType : String);

Var
  S : String;

begin
  S:=NextLine;
  AssertTrue('LoadFromJSON declaration in '+S,Pos('Procedure LoadFromJSON(AJSON : '+AType+'); virtual;',S)>0);
end;

procedure TTestGenCode.AssertSaverDeclaration;

Var
  S : String;

begin
  S:=NextLine;
  AssertTrue('SaveToJSON function declaration in '+S,Pos('Function SaveToJSON : TJSONObject;',S)>0);
  S:=NextLine;
  AssertTrue('SaveToJSON procedure declaration in '+S,Pos('Procedure SaveToJSON(AJSON : TJSONObject)',S)>0);
end;

procedure TTestGenCode.AssertLoaderImplementationEnd(IsDelphi : Boolean = False);

begin
  if Not IsDelphi then
    AssertEnd('Case');// Case
  AssertEnd('for');// For
  AssertEnd('procedure');// Routine
end;

procedure TTestGenCode.AssertArrayLoaderImplementationStart(const ATypeName,
  ADataName, ArrayName, ArrayTypeName, ArrayElementType: String; IsDelphi : Boolean = False);

Var
  S : String;
begin
  S:=NextLine;
  AssertTrue('Have loader start: '+ATypeName+','+ADataName,Pos('Procedure '+ATypeName+'.LoadFromJSON(AJSON : '+ADataName+');',S)>0);
  if isDelphi then
    AssertDelphiPropertyAssignmentLoop
  else
    AssertPropertyAssignmentLoop;
end;

procedure TTestGenCode.AssertPropertyAssignmentLoop;

begin
  AssertTrue('Have var',Pos('var',NextLine)>0);
  AssertTrue('Have P enum',Pos('E : TJSONEnum;',NextLine)>0);
  AssertBegin;
  AssertTrue('Have E for enum',Pos('For E in AJSON do',NextLine)>0);
  AssertBegin;
  if (jpoLoadCaseInsensitive in Gen.Options) then
    AssertTrue('Have E for enum',Pos('case LowerCase(E.key) of',NextLine)>0)
  else
    AssertTrue('Have E for enum',Pos('case E.key of',NextLine)>0);
end;

procedure TTestGenCode.AssertDelphiPropertyAssignmentLoop;

Var
  S : String;

begin
  AssertTrue('Have var',Pos('var',NextLine)>0);
  AssertTrue('Have pair',Pos('P : TJSONPair;',NextLine)>0);
  AssertTrue('Have obj',Pos('O : TJSONObject;',NextLine)>0);
  AssertTrue('Have Propertyname var',Pos('PN : String;',NextLine)>0);
  AssertBegin;
  S:=NextLine;
  AssertTrue('Have JSONObject check in '+S,Pos('not (AJSON is TJSONObject)',S)>0);
  if jpoUnknownLoadPropsError in gen.Options then
    AssertTrue('Have raise statement',Pos('Raise EJSONException',NextLine)>0);
  AssertTrue('Have typecast',Pos('O:=AJSON as TJSONObject',NextLine)>0);
  AssertTrue('Have P for enum',Pos('For P in O do',NextLine)>0);
  AssertBegin;
  if jpoLoadCaseInsensitive in Gen.Options then
    AssertTrue('Have case insensitive propertyname assign',Pos('PN:=LowerCase(P.JSONString.Value)',NextLine)>0)
  else
    AssertTrue('Have propertyname assign',Pos('PN:=P.JSONString.Value',NextLine)>0);
end;

procedure TTestGenCode.AssertObjectLoaderImplementationStart(const ATypeName,
  ADataName, ArrayName, ArrayTypeName, ArrayElementType: String; IsDelphi : Boolean = False);
Var
  S : String;
begin
  S:=NextLine;
  AssertTrue('Have loader start: '+ATypeName+','+ADataName,Pos('Procedure '+ATypeName+'.LoadFromJSON(AJSON : '+ADataName+');',S)>0);
  if isDelphi then
    AssertDelphiPropertyAssignmentLoop
  else
    AssertPropertyAssignmentLoop;
end;

procedure TTestGenCode.AssertSaverImplementationStart(const ATypeName: String;
  IsDelphi: Boolean);

Var
  S,N : String;

begin
  N:='SaveToJSONFunction '+ATypeName+' : ';
  S:=NextLine;
  AssertTrue(N+'header',Pos('Function  '+ATypeName+'.SaveToJSON : TJSONObject;',S)>0);
  AssertBegin;
  AssertTrue(N+'Create',Pos('Result:=TJSONObject.Create',NextLine)>0);
  AssertTrue(N+'Try',Pos('Try',NextLine)>0);
  AssertTrue(N+'Save',Pos('SaveToJSON(Result);',NextLine)>0);
  AssertTrue(N+'except',Pos('except',NextLine)>0);
  AssertTrue(N+'FreeAndNil',Pos('FreeAndNil(Result);',NextLine)>0);
  AssertTrue(N+'Reraise',Pos('Raise;',NextLine)>0);
  AssertTrue(N+'end;',Pos('End;',NextLine)>0);
  AssertTrue(N+'end;',Pos('End;',NextLine)>0);
  AssertTrue(N+'proc header',Pos('Procedure '+ATypeName+'.SaveToJSON(AJSON : TJSONObject);',NextLine)>0);
  AssertBegin;
end;


procedure TTestGenCode.AssertLoaderImplementationStart(const ATypeName,
  ADataName: String; IsDelphi : Boolean = False);

begin
  AssertTrue(Pos('Procedure '+ATypeName+'.LoadFromJSON(AJSON : '+ADataName+');',NextLine)>0);
  if isDelphi then
    AssertDelphiPropertyAssignmentLoop
  else
    AssertPropertyAssignmentLoop;
end;

procedure TTestGenCode.AssertLoadConstructorImplementationStart(const ATypeName,
  ADataName: String);

begin
  AssertTrue('Have constructor call',Pos('Constructor '+ATypeName+'.CreateFromJSON(AJSON : '+ADataName+');',NextLine)>0);
  AssertBegin;
  AssertTrue('Call create constructor',Pos('create();',NextLine)>0);
  AssertTrue('Call LoadFromJSON',Pos('LoadFromJSON(AJSON);',NextLine)>0);
  AssertEnd;
end;

procedure TTestGenCode.TestLoadIntegerProperty;
begin
  Gen.Options:=[jpoGenerateLoad];
  GenCode('{ "a" : 1234 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','integer');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONData');
  AssertLoaderDeclaration('TJSONData');
  AssertProperty('a','integer',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertLoadConstructorImplementationStart('TMyObject','TJSONData');
  AssertLoaderImplementationStart('TMyObject','TJSONData');
  AssertTrue('Have "a" integer property case',Pos('''a'':',NextLine)>0);
  AssertTrue('Have "a" integer property set', Pos('a:=E.Value.AsInteger;',NextLine)>0);
  AssertLoaderImplementationEnd;
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Integer','a','');
end;

procedure TTestGenCode.TestLoad2IntegersProperty;
begin
  Gen.Options:=[jpoGenerateLoad];
  GenCode('{ "a" : 1234, "b" : 5678 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','integer');
  AssertField('b','integer');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONData');
  AssertLoaderDeclaration('TJSONData');
  AssertProperty('a','integer',False);
  AssertProperty('b','integer',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertLoadConstructorImplementationStart('TMyObject','TJSONData');
  AssertLoaderImplementationStart('TMyObject','TJSONData');
  AssertTrue('Have "a" integer property case',Pos('''a'':',NextLine)>0);
  AssertTrue('Have "a" integer property set', Pos('a:=E.Value.AsInteger;',NextLine)>0);
  AssertTrue('Have "b" integer property case',Pos('''b'':',NextLine)>0);
  AssertTrue('Have "b" integer property set', Pos('b:=E.Value.AsInteger;',NextLine)>0);
  AssertLoaderImplementationEnd;
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Integer','a','');
  AssertPropertyMap('b','Integer','b','');
end;

procedure TTestGenCode.TestLoadIntegerWithErrorProperty;
begin
  Gen.Options:=[jpoGenerateLoad,jpoUnknownLoadPropsError];
  GenCode('{ "a" : 1234, "b" : 5678 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','integer');
  AssertField('b','integer');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONData');
  AssertLoaderDeclaration('TJSONData');
  AssertProperty('a','integer',False);
  AssertProperty('b','integer',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertLoadConstructorImplementationStart('TMyObject','TJSONData');
  AssertLoaderImplementationStart('TMyObject','TJSONData');
  AssertTrue('Have "a" integer property case',Pos('''a'':',NextLine)>0);
  AssertTrue('Have "a" integer property set', Pos('a:=E.Value.AsInteger;',NextLine)>0);
  AssertTrue('Have "b" integer property case',Pos('''b'':',NextLine)>0);
  AssertTrue('Have "b" integer property set', Pos('b:=E.Value.AsInteger;',NextLine)>0);
  AssertTrue('Have case else',Pos('else',NextLine)>0);
  AssertTrue('Have raise statement', Pos('Raise EJSON.CreateFmt',NextLine)>0);
  AssertLoaderImplementationEnd;
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Integer','a','');
  AssertPropertyMap('b','Integer','b','');
end;

procedure TTestGenCode.TestLoadIntegerCaseInsensitiveProperty;
begin
  Gen.Options:=[jpoGenerateLoad,jpoLoadCaseInsensitive];
  GenCode('{ "A" : 1234 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('A','integer');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONData');
  AssertLoaderDeclaration('TJSONData');
  AssertProperty('A','integer',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertLoadConstructorImplementationStart('TMyObject','TJSONData');
  AssertLoaderImplementationStart('TMyObject','TJSONData',False);
  AssertTrue('Have "a" integer property case',Pos('''a'':',NextLine)>0);
  AssertTrue('Have "a" integer property set', Pos('A:=E.Value.AsInteger;',NextLine)>0);
  AssertLoaderImplementationEnd;
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('A','Integer','A','');
end;

procedure TTestGenCode.TestLoadStringProperty;
begin
  Gen.Options:=[jpoGenerateLoad];
  GenCode('{ "a" : "1234" }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','string');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONData');
  AssertLoaderDeclaration('TJSONData');
  AssertProperty('a','string',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertLoadConstructorImplementationStart('TMyObject','TJSONData');
  AssertLoaderImplementationStart('TMyObject','TJSONData');
  AssertTrue('Have "a" string property case',Pos('''a'':',NextLine)>0);
  AssertTrue('Have "a" string property set', Pos('a:=E.Value.AsString;',NextLine)>0);
  AssertLoaderImplementationEnd;
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','String','a','');
end;

procedure TTestGenCode.TestLoadBooleanProperty;
begin
  Gen.Options:=[jpoGenerateLoad];
  GenCode('{ "a" : true }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','boolean');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONData');
  AssertLoaderDeclaration('TJSONData');
  AssertProperty('a','boolean',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertLoadConstructorImplementationStart('TMyObject','TJSONData');
  AssertLoaderImplementationStart('TMyObject','TJSONData');
  AssertTrue('Have "a" boolean property case',Pos('''a'':',NextLine)>0);
  AssertTrue('Have "a" boolean property set', Pos('a:=E.Value.AsBoolean;',NextLine)>0);
  AssertLoaderImplementationEnd;
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Boolean','a','');
end;

procedure TTestGenCode.TestLoadInt64Property;
begin
  Gen.Options:=[jpoGenerateLoad];
  GenCode('{ "a" : 1234567890123 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Int64');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONData');
  AssertLoaderDeclaration('TJSONData');
  AssertProperty('a','Int64',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertLoadConstructorImplementationStart('TMyObject','TJSONData');
  AssertLoaderImplementationStart('TMyObject','TJSONData');
  AssertTrue('Have "a" Int64 property case',Pos('''a'':',NextLine)>0);
  AssertTrue('Have "a" Int64 property set', Pos('a:=E.Value.AsInt64;',NextLine)>0);
  AssertLoaderImplementationEnd;
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Int64','a','');
end;

procedure TTestGenCode.TestLoadFloatProperty;
begin
  Gen.Options:=[jpoGenerateLoad];
  GenCode('{ "a" : 1.1 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Double');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONData');
  AssertLoaderDeclaration('TJSONData');
  AssertProperty('a','Double',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertLoadConstructorImplementationStart('TMyObject','TJSONData');
  AssertLoaderImplementationStart('TMyObject','TJSONData');
  AssertTrue('Have "a" Double property case',Pos('''a'':',NextLine)>0);
  AssertTrue('Have "a" Double property set', Pos('a:=E.Value.AsFloat;',NextLine)>0);
  AssertLoaderImplementationEnd;
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Double','a','');
end;

procedure TTestGenCode.TestLoadObjectProperty;
begin
  Gen.Options:=[jpoGenerateLoad];
  GenCode('{ "a" : { "b" : "abc" } }');
  AssertUnitHeader;
  AssertClassHeader('Ta','TObject');
  AssertField('b','String');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONData');
  AssertLoaderDeclaration('TJSONData');
  AssertProperty('b','String',False);
  AssertEnd;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Ta');
  AssertVisibility('public');
  AssertDestructor;
  AssertLoadConstructorDeclaration('TJSONData');
  AssertLoaderDeclaration('TJSONData');
  AssertProperty('a','ta',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','Ta');
  AssertLoadConstructorImplementationStart('Ta','TJSONData');
  AssertLoaderImplementationStart('Ta','TJSONData');
  AssertTrue('Have "b" string property case',Pos('''b'':',NextLine)>0);
  AssertTrue('Have "b" string property set', Pos('b:=E.Value.AsString;',NextLine)>0);
  AssertLoaderImplementationEnd;
  AssertClassComment('Object Implementation','TMyObject');
  AssertDestructorImplementation('TMyObject',['a']);
  AssertLoadConstructorImplementationStart('TMyObject','TJSONData');
  AssertObjectLoaderImplementationStart('TMyObject','TJSONData','a','Ta','');
  AssertTrue('Have "a" object property case',Pos('''a'':',NextLine)>0);
  AssertTrue('Have "a" object create createfromjson', Pos('a:=ta.CreateFromJSON(E.Value);',NextLine)>0);
  AssertLoaderImplementationEnd;
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','TObject');
end;

procedure TTestGenCode.AssertArrayCreator(const ArrayTypeName,
  AElementType: String; IsDelphi: Boolean);

Var
  S : String;

begin
  S:=NextLine;
  AssertTrue('Have array creator in '+S,Pos('Function Create'+ArrayTypeName+'(AJSON : '+GetDataName(IsDelphi)+') : '+ArrayTypeName,S)>0);
end;

procedure TTestGenCode.AssertArraySaver(const ArrayTypeName,
  AElementType: String; IsDelphi: Boolean);

Var
  E,S : String;

begin
  S:=NextLine;
  E:='Procedure Save'+ArrayTypeName+'ToJSON(AnArray : '+ArrayTypeName+'; AJSONArray : TJSONArray);';
  AssertTrue('Have proc array saver in '+S,Pos(E,S)>0);
  S:=NextLine;
  E:='Function Save'+ArrayTypeName+'ToJSON(AnArray : '+ArrayTypeName+') : TJSONArray;';
  AssertTrue('Have func array saver in '+S,Pos(E,S)>0);
end;

procedure TTestGenCode.AssertArrayCreatorImplementation(const ArrayTypeName,
  AElementType: String; AObjectName: String; IsDelphi: Boolean);

Var
  S,E,AN : String;

begin
  S:=NextLine;
  E:='Function Create'+ARrayTypeName+'(AJSON : '+GetDataName(IsDelphi)+') : '+ArrayTypeName;
  AssertTrue('Have array creator header '+S+'Expected : '+E ,Pos(E,S)>0);
  AssertTrue('Have var',Pos('var',NextLine)>0);
  AssertTrue('Have loop var',Pos('I : Integer;',NextLine)>0);
  if IsDelphi then
    begin
    AssertTrue('Have Array var',Pos('A : TJSONArray;',NextLine)>0);
    AN:='A'
    end
  else
    AN:='AJSON';
  AssertBegin;
  if IsDelphi then
    AssertTrue('Have Array assignnment',Pos('A:=AJSON as TJSONArray;',NextLine)>0);
  AssertTrue('Have array setlength ',Pos('SetLength(Result,'+AN+'.Count);',NextLine)>0);
  AssertTrue('Have loop ',Pos('for i:=0 to '+AN+'.Count-1 do',NextLine)>0);
  if AObjectName='' then
    begin
    if IsDelphi then
      AssertTrue('Have element assignment : '+AElementType,Pos('Result[i]:='+AN+'.Items[i].GetValue<'+AElementType+'>;',NextLine)>0)
    else
      AssertTrue('Have element assignment : '+AElementType,Pos('Result[i]:='+AN+'.Items[i].'+AElementType+';',NextLine)>0)
    end
  else
    AssertTrue('Have element assignment : '+AElementType,Pos('Result[i]:='+AObjectName+'.CreateFromJSON('+AN+'.Items[i]);',NextLine)>0);
  AssertEnd;
end;

procedure TTestGenCode.AssertLine(Msg : String; AExpected : String);

Var
  N,DMsg : String;

begin
  N:=NextLine;
  DMsg:=Msg+', Expected: "'+AExpected+'", Actual: "'+N+'"';
  AssertTrue(Dmsg,Pos(AExpected,N)>0);
end;

procedure TTestGenCode.AssertArraySaverImplementation(const ArrayTypeName,
  AElementType: String; AObjectName: String; IsDelphi: Boolean);
Var
  N,S,E,AN : String;

begin
  N:=ArrayTypeName+'Saver : ';
  S:=NextLine;
  E:='Function Save'+ArrayTypeName+'ToJSON(AnArray : '+ArrayTypeName+') : TJSONArray;';
  AssertTrue(N+'header',Pos(E,S)>0);
  AssertBegin;
  AssertTrue(N+'Create',Pos('Result:=TJSONArray.Create',NextLine)>0);
  AssertTrue(N+'Try',Pos('Try',NextLine)>0);
  S:=NextLine;
  E:='Save'+ArrayTypeName+'ToJSON(AnArray,Result);';
  AssertTrue(N+'Save',Pos(E,S)>0);
  AssertTrue(N+'except',Pos('except',NextLine)>0);
  AssertTrue(N+'FreeAndNil',Pos('FreeAndNil(Result);',NextLine)>0);
  AssertTrue(N+'Reraise',Pos('Raise;',NextLine)>0);
  AssertTrue(N+'end;',Pos('End;',NextLine)>0);
  AssertTrue(N+'end;',Pos('End;',NextLine)>0);
  S:=NextLine;
  E:='Procedure Save'+ArrayTypeName+'ToJSON(AnArray : '+ArrayTypeName+'; AJSONArray : TJSONArray);';
  AssertTrue('Have array saver header '+S+'Expected : '+E ,Pos(E,S)>0);
  AssertTrue('Have var',Pos('var',NextLine)>0);
  AssertTrue('Have loop var',Pos('I : Integer;',NextLine)>0);
  AssertBegin;
  AssertTrue('Have loop ',Pos('for i:=0 to Length(AnArray)-1 do',NextLine)>0);
  if AObjectName='' then
    AssertLine('Have element assignment : '+AElementType,'AJSONArray.Add(AnArray[i]);')
{  else if AObjectName='' then
    AssertLine('Have element assignment : '+AElementType,'AJSONArray.Add('+AN+'[i]);')}
  else
     AssertTrue('Have element assignment : '+AElementType,Pos('AJSONArray.Add(AnArray[i].SaveToJSON);',NextLine)>0);
  AssertEnd;
end;

procedure TTestGenCode.AssertType;

begin
  AssertTrue('Have Type keyword',Pos('Type',NextLine)>0);
end;

procedure TTestGenCode.AssertDelphiLoadArray(AElementType, AJSONtype : String);

begin
  AssertUnitHeader;
  AssertArrayType('Ta',AElementType);
  AssertArrayCreator('Ta',AElementType,true);
  AssertType;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Ta');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONValue');
  AssertLoaderDeclaration('TJSONValue');
  AssertProperty('a','ta',False);
  AssertEnd;
  AssertImplementation;
  AssertArrayCreatorImplementation('Ta',AJSONType,'',True);
  AssertClassComment('Object Implementation','TMyObject');
  AssertLoadConstructorImplementationStart('TMyObject','TJSONValue');
  AssertArrayLoaderImplementationStart('TMyObject','TJSONValue','a','Ta',AJSONType);
  AssertTrue('Have "a" property if',Pos('If (PN=''a'') then',NextLine)>0);
  AssertTrue('Have "a" property set with createarray', Pos('a:=CreateTa(P.Value);',NextLine)>0);
  AssertLoaderImplementationEnd;
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','');
end;

class function TTestGenCode.GetDataName(IsDelphi: Boolean): string;

begin
  if IsDelphi then
    Result:='TJSONValue'
  else
    Result:='TJSONData';
end;

procedure TTestGenCode.AssertLoadArray(AElementType, AJSONtype: String;
  IsDelphi: Boolean = False);

Var
  DN : String;

begin
  AssertUnitHeader;
  DN:=GetDataName(IsDelphi);
  AssertArrayType('Ta',AElementType);
  AssertArrayCreator('Ta',AElementType,IsDelphi);
  AssertType;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Ta');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration(DN);
  AssertLoaderDeclaration(DN);
  AssertProperty('a','ta',False);
  AssertEnd;
  AssertImplementation;
  AssertArrayCreatorImplementation('Ta',AJSONType,'',IsDelphi);
  AssertClassComment('Object Implementation','TMyObject');
  AssertLoadConstructorImplementationStart('TMyObject',DN);
  AssertArrayLoaderImplementationStart('TMyObject',DN,'a','Ta',AJSONType,isDelphi);
  if IsDelphi then
    begin
    AssertTrue('Have "a" property if',Pos('If (PN=''a'') then',NextLine)>0);
    AssertTrue('Have "a" property set with createarray', Pos('a:=CreateTa(P.JSONValue);',NextLine)>0);
    end
  else
    begin
    AssertTrue('Have "a" array property case',Pos('''a'':',NextLine)>0);
    AssertTrue('Have "a" property set with createarray', Pos('a:=CreateTa(E.Value);',NextLine)>0);
    end;
  AssertLoaderImplementationEnd(IsDelphi);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','');
end;

procedure TTestGenCode.AssertSaveArray(AElementType, AJSONtype: String; IsDelphi: Boolean = False);

Var
  DN : String;

begin
  AssertUnitHeader;
  DN:=GetDataName(IsDelphi);
  AssertArrayType('Ta',AElementType);
  AssertArraySaver('Ta',AElementType,IsDelphi);
  AssertType;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Ta');
  AssertVisibility('public');
  AssertSaverDeclaration;
  AssertProperty('a','ta',False);
  AssertEnd;
  AssertImplementation;
  AssertArraySaverImplementation('Ta',AJSONType,'',IsDelphi);
  AssertClassComment('Object Implementation','TMyObject');
  AssertSaverImplementationStart('TMyObject');
  if IsDelphi then
    AssertTrue('Array save statement', Pos('AJSON.AddPair(''a'',SaveTaToJSON(a));',NextLine)>0)
  else
    AssertTrue('Array save statement', Pos('AJSON.Add(''a'',SaveTaToJSON(a));',NextLine)>0);
  AssertEnd('Saver');
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','');
end;

procedure TTestGenCode.TestLoadStringArrayProperty;
begin
  Gen.Options:=[jpoGenerateLoad];
  GenCode('{ "a" : [ "abc" ] }');
  AssertLoadArray('string','AsString');
end;

procedure TTestGenCode.TestLoadBooleanArrayProperty;
begin
  Gen.Options:=[jpoGenerateLoad];
  GenCode('{ "a" : [ true ] }');
  AssertLoadArray('boolean','AsBoolean');
end;

procedure TTestGenCode.TestLoadIntegerArrayProperty;
begin
  Gen.Options:=[jpoGenerateLoad];
  GenCode('{ "a" : [ 123 ] }');
  AssertLoadArray('Integer','AsInteger');
end;

procedure TTestGenCode.TestLoadInt64ArrayProperty;
begin
  Gen.Options:=[jpoGenerateLoad];
  GenCode('{ "a" : [ 1234567890123 ] }');
  AssertLoadArray('Int64','AsInt64');
end;

procedure TTestGenCode.TestLoadFloatArrayProperty;
begin
  Gen.Options:=[jpoGenerateLoad];
  GenCode('{ "a" : [ 12.34 ] }');
  AssertLoadArray('Double','AsFloat');
end;

procedure TTestGenCode.TestLoadObjectArrayProperty;
begin
  Gen.Options:=[jpoGenerateLoad];
  GenCode('{ "a" :  [ { "b" : "abc" } ] }');
  AssertUnitHeader;
  AssertClassHeader('TaItem','TObject');
  AssertField('b','String');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONData');
  AssertLoaderDeclaration('TJSONData');
  AssertProperty('b','String',False);
  AssertEnd;
  AssertArrayType('Ta','TaItem');
  AssertArrayCreator('Ta','TaItem');
  AssertType;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Ta');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONData');
  AssertLoaderDeclaration('TJSONData');
  AssertProperty('a','ta',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TaItem');
  AssertLoadConstructorImplementationStart('TAItem','TJSONData');
  AssertLoaderImplementationStart('TaItem','TJSONData');
  AssertTrue('Have "b" string property case',Pos('''b'':',NextLine)>0);
  AssertTrue('Have "b" string property set', Pos('b:=E.Value.AsString;',NextLine)>0);
  AssertLoaderImplementationEnd;
  AssertArrayCreatorImplementation('Ta','','TaItem');
  AssertClassComment('Object Implementation','TMyObject');
  AssertLoadConstructorImplementationStart('TMyObject','TJSONData');
  AssertObjectLoaderImplementationStart('TMyObject','TJSONData','a','Ta','');
  AssertTrue('Have "a" stringarray property case',Pos('''a'':',NextLine)>0);
  AssertTrue('Have "a" property set with createarray', Pos('a:=CreateTa(E.Value);',NextLine)>0);
  AssertLoaderImplementationEnd;
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','');
end;


procedure TTestGenCode.TestLoadDelphiIntegerProperty;

Var
  S : String;

begin
  Gen.Options:=[jpoGenerateLoad,jpoDelphiJSON];
  GenCode('{ "a" : 1234 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','integer');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONValue');
  AssertLoaderDeclaration('TJSONValue');
  AssertProperty('a','integer',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertLoadConstructorImplementationStart('TMyObject','TJSONValue');
  AssertLoaderImplementationStart('TMyObject','TJSONValue',True);
  AssertTrue('Have "a" integer property case ',Pos('If (PN=''a'') then',NextLine)>0);
  AssertTrue('Have "a" integer property set', Pos('a:=P.JSONValue.GetValue<Integer>;',NextLine)>0);
  AssertLoaderImplementationEnd(True);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Integer','a','');
end;

procedure TTestGenCode.TestLoadDelphi2IntegersProperty;

Var
  S : String;

begin
  Gen.Options:=[jpoGenerateLoad,jpoDelphiJSON];
  GenCode('{ "a" : 1234, "b" : 5678 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','integer');
  AssertField('b','integer');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONValue');
  AssertLoaderDeclaration('TJSONValue');
  AssertProperty('a','integer',False);
  AssertProperty('b','integer',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertLoadConstructorImplementationStart('TMyObject','TJSONValue');
  AssertLoaderImplementationStart('TMyObject','TJSONValue',True);
  AssertTrue('Have "a" integer property case ',Pos('If (PN=''a'') then',NextLine)>0);
  S:=NextLine;
  AssertTrue('Have "a" integer property set', Pos('a:=P.JSONValue.GetValue<Integer>',S)>0);
  AssertTrue('Have no semicolon', Pos(';',S)=0);
  AssertTrue('Have else  "b" integer property case ',Pos('Else If (PN=''b'') then',NextLine)>0);
  AssertTrue('Have "b" integer property set', Pos('b:=P.JSONValue.GetValue<Integer>;',NextLine)>0);
  AssertLoaderImplementationEnd(True);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Integer','a','');
  AssertPropertyMap('b','Integer','b','');
end;

procedure TTestGenCode.TestLoadDelphiIntegerWithErrorProperty;

Var
  S : String;

begin
  Gen.Options:=[jpoGenerateLoad,jpoDelphiJSON,jpoUnknownLoadPropsError];
  GenCode('{ "a" : 1234, "b" : 5678 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','integer');
  AssertField('b','integer');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONValue');
  AssertLoaderDeclaration('TJSONValue');
  AssertProperty('a','integer',False);
  AssertProperty('b','integer',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertLoadConstructorImplementationStart('TMyObject','TJSONValue');
  AssertLoaderImplementationStart('TMyObject','TJSONValue',True);
  AssertTrue('Have "a" integer property case ',Pos('If (PN=''a'') then',NextLine)>0);
  S:=NextLine;
  AssertTrue('Have "a" integer property set', Pos('a:=P.JSONValue.GetValue<Integer>',S)>0);
  AssertTrue('Have no semicolon for a', Pos(';',S)=0);
  AssertTrue('Have "b" integer property case ',Pos('If (PN=''b'') then',NextLine)>0);
  S:=NextLine;
  AssertTrue('Have "b" integer property set', Pos('b:=P.JSONValue.GetValue<Integer>',S)>0);
  AssertTrue('Have no semicolon for b', Pos(';',S)=0);
  AssertTrue('Have case else',Pos('else',NextLine)>0);
  AssertTrue('Have raise statement', Pos('Raise EJSONException.CreateFmt',NextLine)>0);
  AssertLoaderImplementationEnd(True);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Integer','a','');
  AssertPropertyMap('b','Integer','b','');
end;

procedure TTestGenCode.TestLoadDelphiIntegerCaseInsensitiveProperty;
begin
  Gen.Options:=[jpoGenerateLoad,jpoDelphiJSON,jpoLoadCaseInsensitive];
  GenCode('{ "A" : 1234 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('A','integer');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONValue');
  AssertLoaderDeclaration('TJSONValue');
  AssertProperty('A','integer',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertLoadConstructorImplementationStart('TMyObject','TJSONValue');
  AssertLoaderImplementationStart('TMyObject','TJSONValue',True);
  AssertTrue('Have "a" integer property case',Pos('If (PN=''a'') then',NextLine)>0);
  AssertTrue('Have "A" integer property set', Pos('A:=P.JSONValue.GetValue<Integer>;',NextLine)>0);
  AssertLoaderImplementationEnd(True);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('A','Integer','A','');
end;

procedure TTestGenCode.TestLoadDelphiStringProperty;
begin
  Gen.Options:=[jpoGenerateLoad,jpoDelphiJSON];
  GenCode('{ "a" : "1234" }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','String');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONValue');
  AssertLoaderDeclaration('TJSONValue');
  AssertProperty('a','string',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertLoadConstructorImplementationStart('TMyObject','TJSONValue');
  AssertLoaderImplementationStart('TMyObject','TJSONValue',True);
  AssertTrue('Have "a" integer property case',Pos('If (PN=''a'') then',NextLine)>0);
  AssertTrue('Have "a" integer property set', Pos('a:=P.JSONValue.GetValue<String>;',NextLine)>0);
  AssertLoaderImplementationEnd(True);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','String','a','');
end;

procedure TTestGenCode.TestLoadDelphiBooleanProperty;
begin
  Gen.Options:=[jpoGenerateLoad,jpoDelphiJSON];
  GenCode('{ "a" : true }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','boolean');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONValue');
  AssertLoaderDeclaration('TJSONValue');
  AssertProperty('a','boolean',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertLoadConstructorImplementationStart('TMyObject','TJSONValue');
  AssertLoaderImplementationStart('TMyObject','TJSONValue',True);
  AssertTrue('Have "a" integer property case',Pos('If (PN=''a'') then',NextLine)>0);
  AssertTrue('Have "a" integer property set',Pos('a:=P.JSONValue.GetValue<Boolean>;',NextLine)>0);
  AssertLoaderImplementationEnd(True);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Boolean','a','');
end;

procedure TTestGenCode.TestLoadDelphiInt64Property;
begin
  Gen.Options:=[jpoGenerateLoad,jpoDelphiJSON];
  GenCode('{ "a" : 1234567890123 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Int64');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONValue');
  AssertLoaderDeclaration('TJSONValue');
  AssertProperty('a','Int64',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertLoadConstructorImplementationStart('TMyObject','TJSONValue');
  AssertLoaderImplementationStart('TMyObject','TJSONValue',True);
  AssertTrue('Have "a" integer property case',Pos('If (PN=''a'') then',NextLine)>0);
  AssertTrue('Have "a" integer property set',Pos('a:=P.JSONValue.GetValue<Int64>;',NextLine)>0);
  AssertLoaderImplementationEnd(True);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Int64','a','');
end;

procedure TTestGenCode.TestLoadDelphiFloatProperty;
begin
  Gen.Options:=[jpoGenerateLoad,jpoDelphiJSON];
  GenCode('{ "a" : 1.1 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Double');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONValue');
  AssertLoaderDeclaration('TJSONValue');
  AssertProperty('a','Double',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertLoadConstructorImplementationStart('TMyObject','TJSONValue');
  AssertLoaderImplementationStart('TMyObject','TJSONValue',True);
  AssertTrue('Have "a" integer property case',Pos('If (PN=''a'') then',NextLine)>0);
  AssertTrue('Have "a" integer property set',Pos('a:=P.JSONValue.GetValue<Double>;',NextLine)>0);
  AssertLoaderImplementationEnd(True);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Double','a','');
end;

procedure TTestGenCode.TestLoadDelphiObjectProperty;
begin
  Gen.Options:=[jpoGenerateLoad,jpoDelphiJSON];
  GenCode('{ "a" : { "b" : "abc" } }');
  AssertUnitHeader;
  AssertClassHeader('Ta','TObject');
  AssertField('b','String');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONValue');
  AssertLoaderDeclaration('TJSONValue');
  AssertProperty('b','String',False);
  AssertEnd;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Ta');
  AssertVisibility('public');
  AssertDestructor;
  AssertLoadConstructorDeclaration('TJSONValue');
  AssertLoaderDeclaration('TJSONValue');
  AssertProperty('a','ta',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','Ta');
  AssertLoadConstructorImplementationStart('Ta','TJSONValue');
  AssertLoaderImplementationStart('Ta','TJSONValue',True);
  AssertTrue('Have "b" string property case',Pos('If (PN=''b'') then',NextLine)>0);
  AssertTrue('Have "b" string property set', Pos('b:=P.JSONValue.GetValue<String>;',NextLine)>0);
  AssertLoaderImplementationEnd(True);
  AssertClassComment('Object Implementation','TMyObject');
  AssertDestructorImplementation('TMyObject',['a']);
  AssertLoadConstructorImplementationStart('TMyObject','TJSONValue');
  AssertObjectLoaderImplementationStart('TMyObject','TJSONValue','a','Ta','',True);
  AssertTrue('Have "a" object property case',Pos('If (PN=''a'') then',NextLine)>0);
  AssertTrue('Have "a" object create createfromjson', Pos('a:=ta.CreateFromJSON(P.JSONValue);',NextLine)>0);
  AssertLoaderImplementationEnd(True);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','TObject');
end;

procedure TTestGenCode.TestLoadDelphiObjectArrayProperty;
begin
  Gen.Options:=[jpoGenerateLoad,jpoDelphiJSON];
  GenCode('{ "a" : [ { "b" : "abc" } ] }');
  AssertUnitHeader;
  AssertClassHeader('TaItem','TObject');
  AssertField('b','String');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONValue');
  AssertLoaderDeclaration('TJSONValue');
  AssertProperty('b','String',False);
  AssertEnd;
  AssertArrayType('Ta','TaItem');
  AssertArrayCreator('Ta','TaItem',True);
  AssertType;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Ta');
  AssertVisibility('public');
  AssertLoadConstructorDeclaration('TJSONValue');
  AssertLoaderDeclaration('TJSONValue');
  AssertProperty('a','ta',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TaItem');
  AssertLoadConstructorImplementationStart('TAItem','TJSONValue');
  AssertLoaderImplementationStart('TaItem','TJSONValue',True);
  AssertTrue('Have "b" object property case',Pos('If (PN=''b'') then',NextLine)>0);
  AssertTrue('Have "b" object property set', Pos('b:=P.JSONValue.GetValue<String>;',NextLine)>0);
  AssertLoaderImplementationEnd(True);
  AssertArrayCreatorImplementation('Ta','','TaItem',True);
  AssertClassComment('Object Implementation','TMyObject');
  AssertLoadConstructorImplementationStart('TMyObject','TJSONValue');
  AssertObjectLoaderImplementationStart('TMyObject','TJSONValue','a','Ta','',True);
  AssertTrue('Have "a" object property case',Pos('If (PN=''a'') then',NextLine)>0);
  AssertTrue('Have "a" property set with createarray', Pos('a:=CreateTa(P.JSONValue);',NextLine)>0);
  AssertLoaderImplementationEnd(True);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','');
end;

procedure TTestGenCode.TestSaveIntegerProperty;
begin
  Gen.Options:=[jpoGenerateSave];
  GenCode('{ "a" : 1234 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','integer');
  AssertVisibility('public');
  AssertSaverDeclaration;
  AssertProperty('a','integer',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertSaverImplementationStart('TMyObject');
  AssertTrue('Have "a" integer property save', Pos('AJSON.Add(''a'',a);',NextLine)>0);
  AssertTrue('end',Pos('end;',NextLine)>0);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Integer','a','');
end;

procedure TTestGenCode.TestSave2IntegersProperty;
begin
  Gen.Options:=[jpoGenerateSave];
  GenCode('{ "a" : 1234, "b" : 5678 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','integer');
  AssertField('b','integer');
  AssertVisibility('public');
  AssertSaverDeclaration;
  AssertProperty('a','integer',False);
  AssertProperty('b','integer',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertSaverImplementationStart('TMyObject');
  AssertTrue('Have "a" integer property save', Pos('AJSON.Add(''a'',a);',NextLine)>0);
  AssertTrue('Have "b" integer property save', Pos('AJSON.Add(''b'',b);',NextLine)>0);
  AssertTrue('end',Pos('end;',NextLine)>0);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Integer','a','');
  AssertPropertyMap('b','Integer','b','');
end;

procedure TTestGenCode.TestSaveStringProperty;
begin
  Gen.Options:=[jpoGenerateSave];
  GenCode('{ "a" : "1234" }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','string');
  AssertVisibility('public');
  AssertSaverDeclaration;
  AssertProperty('a','string',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertSaverImplementationStart('TMyObject');
  AssertTrue('Have "a" integer property save', Pos('AJSON.Add(''a'',a);',NextLine)>0);
  AssertTrue('end',Pos('end;',NextLine)>0);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','String','a','');
end;

procedure TTestGenCode.TestSaveBooleanProperty;
begin
  Gen.Options:=[jpoGenerateSave];
  GenCode('{ "a" : true }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Boolean');
  AssertVisibility('public');
  AssertSaverDeclaration;
  AssertProperty('a','Boolean',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertSaverImplementationStart('TMyObject');
  AssertTrue('Have "a" boolean property save', Pos('AJSON.Add(''a'',a);',NextLine)>0);
  AssertTrue('end',Pos('end;',NextLine)>0);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Boolean','a','');
end;

procedure TTestGenCode.TestSaveInt64Property;
begin
  Gen.Options:=[jpoGenerateSave];
  GenCode('{ "a" : 1234567890123 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Int64');
  AssertVisibility('public');
  AssertSaverDeclaration;
  AssertProperty('a','Int64',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertSaverImplementationStart('TMyObject');
  AssertTrue('Have "a" int64 property save', Pos('AJSON.Add(''a'',a);',NextLine)>0);
  AssertTrue('end',Pos('end;',NextLine)>0);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Int64','a','');
end;

procedure TTestGenCode.TestSaveFloatProperty;
begin
  Gen.Options:=[jpoGenerateSave];
  GenCode('{ "a" : 1.2 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','double');
  AssertVisibility('public');
  AssertSaverDeclaration;
  AssertProperty('a','double',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertSaverImplementationStart('TMyObject');
  AssertTrue('Have "a" integer property save', Pos('AJSON.Add(''a'',a);',NextLine)>0);
  AssertTrue('end',Pos('end;',NextLine)>0);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Double','a','');

end;

procedure TTestGenCode.TestSaveObjectProperty;
begin
  Gen.Options:=[jpoGenerateSave];
  GenCode('{ "a" : { "b" : "abc" } }');
  AssertUnitHeader;
  AssertClassHeader('Ta','TObject');
  AssertField('b','String');
  AssertVisibility('public');
  AssertSaverDeclaration;
  AssertProperty('b','String',False);
  AssertEnd;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Ta');
  AssertVisibility('public');
  AssertDestructor;
  AssertSaverDeclaration;
  AssertProperty('a','ta',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','Ta');
  AssertSaverImplementationStart('Ta');
  AssertTrue('Have "b" property save', Pos('AJSON.Add(''b'',b);',NextLine)>0);
  AssertEnd;
  AssertClassComment('Object Implementation','TMyObject');
  AssertDestructorImplementation('TMyObject',['a']);
  AssertSaverImplementationStart('TMyObject');
  AssertTrue('Have check for assigned object property save', Pos('if Assigned(a) then',NextLine)>0);
  AssertTrue('Have "a" object property save', Pos('AJSON.Add(''a'',a.SaveToJSON);',NextLine)>0);
  AssertEnd;
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','TObject');
end;

procedure TTestGenCode.TestSaveStringArrayProperty;
begin
  Gen.Options:=[jpoGenerateSave];
  GenCode('{ "a" : [ "abc" ] }');
  AssertSaveArray('string','');
end;

procedure TTestGenCode.TestSaveBooleanArrayProperty;
begin
  Gen.Options:=[jpoGenerateSave];
  GenCode('{ "a" : [ true ] }');
  AssertSaveArray('boolean','');
end;

procedure TTestGenCode.TestSaveIntegerArrayProperty;
begin
  Gen.Options:=[jpoGenerateSave];
  GenCode('{ "a" : [ 123 ] }');
  AssertSaveArray('Integer','');
end;

procedure TTestGenCode.TestSaveInt64ArrayProperty;
begin
  Gen.Options:=[jpoGenerateSave];
  GenCode('{ "a" : [ 1234567890123 ] }');
  AssertSaveArray('Int64','');
end;

procedure TTestGenCode.TestSaveFloatArrayProperty;
begin
  Gen.Options:=[jpoGenerateSave];
  GenCode('{ "a" : [ 1.23] }');
  AssertSaveArray('Double','');
end;

procedure TTestGenCode.TestSaveObjectArrayProperty;
begin
  Gen.Options:=[jpoGenerateSave];
  GenCode('{ "a" :  [ { "b" : "abc" } ] }');
  AssertUnitHeader;
  AssertClassHeader('TaItem','TObject');
  AssertField('b','String');
  AssertVisibility('public');
  AssertSaverDeclaration;
  AssertProperty('b','String',False);
  AssertEnd;
  AssertArrayType('Ta','TaItem');
  AssertArraySaver('Ta','TaItem');
  AssertType;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Ta');
  AssertVisibility('public');
  AssertSaverDeclaration;
  AssertProperty('a','ta',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TaItem');
  AssertSaverImplementationStart('TaItem');
  AssertTrue('Have "b" string property save', Pos('AJSON.Add(''b'',b);',NextLine)>0);
  AssertTrue('end',Pos('end;',NextLine)>0);
  AssertArraySaverImplementation('Ta','','TaItem');
  AssertClassComment('Object Implementation','TMyObject');
  AssertSaverImplementationStart('TMyObject');
  AssertTrue('Have "a" array property save', Pos('AJSON.Add(''a'',SaveTaToJSON(a));',NextLine)>0);
  AssertEnd('Loader TMyObject');
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','');
end;

procedure TTestGenCode.TestSaveDelphiIntegerProperty;
begin
  Gen.Options:=[jpoGenerateSave,jpoDelphiJSON];
  GenCode('{ "a" : 1234 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','integer');
  AssertVisibility('public');
  AssertSaverDeclaration;
  AssertProperty('a','integer',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertSaverImplementationStart('TMyObject');
  AssertTrue('Have "a" integer property save', Pos('AJSON.AddPair(''a'',TJSONNumber.Create(a));',NextLine)>0);
  AssertTrue('end',Pos('end;',NextLine)>0);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Integer','a','');
end;

procedure TTestGenCode.TestSaveDelphi2IntegersProperty;
begin
  Gen.Options:=[jpoGenerateSave,jpoDelphiJSON];
  GenCode('{ "a" : 1234, "b" : 5678 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','integer');
  AssertField('b','integer');
  AssertVisibility('public');
  AssertSaverDeclaration;
  AssertProperty('a','integer',False);
  AssertProperty('b','integer',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertSaverImplementationStart('TMyObject');
  AssertTrue('Have "a" integer property save', Pos('AJSON.AddPair(''a'',TJSONNumber.Create(a));',NextLine)>0);
  AssertTrue('Have "b" integer property save', Pos('AJSON.AddPair(''b'',TJSONNumber.Create(b));',NextLine)>0);
  AssertTrue('end',Pos('end;',NextLine)>0);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Integer','a','');
  AssertPropertyMap('b','Integer','b','');
end;

procedure TTestGenCode.TestSaveDelphiStringProperty;
begin
  Gen.Options:=[jpoGenerateSave,jpoDelphiJSON];
  GenCode('{ "a" : "1234" }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','string');
  AssertVisibility('public');
  AssertSaverDeclaration;
  AssertProperty('a','string',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertSaverImplementationStart('TMyObject');
  AssertTrue('Have "a" string property save', Pos('AJSON.AddPair(''a'',TJSONString.Create(a));',NextLine)>0);
  AssertTrue('end',Pos('end;',NextLine)>0);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','String','a','');
end;

procedure TTestGenCode.TestSaveDelphiBooleanProperty;
begin
  Gen.Options:=[jpoGenerateSave,jpoDelphiJSON];
  GenCode('{ "a" : true }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Boolean');
  AssertVisibility('public');
  AssertSaverDeclaration;
  AssertProperty('a','Boolean',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertSaverImplementationStart('TMyObject');
  AssertTrue('Have "a" Boolean property save', Pos('AJSON.AddPair(''a'',TJSONBoolean.Create(a));',NextLine)>0);
  AssertTrue('end',Pos('end;',NextLine)>0);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Boolean','a','');
end;

procedure TTestGenCode.TestSaveDelphiInt64Property;
begin
  Gen.Options:=[jpoGenerateSave,jpoDelphiJSON];
  GenCode('{ "a" : 1234567890123 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Int64');
  AssertVisibility('public');
  AssertSaverDeclaration;
  AssertProperty('a','Int64',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertSaverImplementationStart('TMyObject');
  AssertTrue('Have "a" int64 property save', Pos('AJSON.AddPair(''a'',TJSONNumber.Create(a));',NextLine)>0);
  AssertTrue('end',Pos('end;',NextLine)>0);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Int64','a','');
end;

procedure TTestGenCode.TestSaveDelphiFloatProperty;
Var
  S : String;
begin
  Gen.Options:=[jpoGenerateSave,jpoDelphiJSON];
  GenCode('{ "a" : 1.2 }');
  AssertUnitHeader;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','double');
  AssertVisibility('public');
  AssertSaverDeclaration;
  AssertProperty('a','double',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TMyObject');
  AssertSaverImplementationStart('TMyObject');
  S:=NextLine;
  AssertTrue('Have "a" float property save', Pos('AJSON.AddPair(''a'',TJSONNumber.Create(a));',S)>0);
  AssertTrue('end',Pos('end;',NextLine)>0);
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Double','a','');
end;

procedure TTestGenCode.TestSaveDelphiObjectProperty;
Var
  S : String;
begin
  Gen.Options:=[jpoGenerateSave,jpoDelphiJSON];
  GenCode('{ "a" : { "b" : "abc" } }');
  AssertUnitHeader;
  AssertClassHeader('Ta','TObject');
  AssertField('b','String');
  AssertVisibility('public');
  AssertSaverDeclaration;
  AssertProperty('b','String',False);
  AssertEnd;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Ta');
  AssertVisibility('public');
  AssertDestructor;
  AssertSaverDeclaration;
  AssertProperty('a','ta',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','Ta');
  AssertSaverImplementationStart('Ta');
  AssertTrue('Have "b" string property save', Pos('AJSON.AddPair(''b'',TJSONString.Create(b));',NextLine)>0);
  AssertEnd;
  AssertClassComment('Object Implementation','TMyObject');
  AssertDestructorImplementation('TMyObject',['a']);
  AssertSaverImplementationStart('TMyObject');
  AssertTrue('Have check for assigned object property save', Pos('if Assigned(a) then',NextLine)>0);
  S:=NextLine;
  AssertTrue('Have "a" object property save', Pos('AJSON.AddPair(''a'',a.SaveToJSON);',S)>0);
  AssertEnd;
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','TObject');
end;

procedure TTestGenCode.TestSaveDelphiStringArrayProperty;
begin
  Gen.Options:=[jpoGenerateSave,jpoDelphiJSON];
  GenCode('{ "a" : [ "abc" ] }');
  AssertSaveArray('string','',True);
end;

procedure TTestGenCode.TestSaveDelphiBooleanArrayProperty;
begin
  Gen.Options:=[jpoGenerateSave,jpoDelphiJSON];
  GenCode('{ "a" : [ true ] }');
  AssertSaveArray('boolean','',True);
end;

procedure TTestGenCode.TestSaveDelphiIntegerArrayProperty;
begin
  Gen.Options:=[jpoGenerateSave,jpoDelphiJSON];
  GenCode('{ "a" : [ 123 ] }');
  AssertSaveArray('Integer','',True);
end;

procedure TTestGenCode.TestSaveDelphiInt64ArrayProperty;
begin
  Gen.Options:=[jpoGenerateSave,jpoDelphiJSON];
  GenCode('{ "a" : [ 1234567890123 ] }');
  AssertSaveArray('Int64','',True);
end;

procedure TTestGenCode.TestSaveDelphiFloatArrayProperty;
begin
  Gen.Options:=[jpoGenerateSave,jpoDelphiJSON];
  GenCode('{ "a" : [ 1.23] }');
  AssertSaveArray('Double','',True);
end;

procedure TTestGenCode.TestSaveDelphiObjectArrayProperty;
begin
  Gen.Options:=[jpoGenerateSave,jpoDelphiJSON];
  GenCode('{ "a" :  [ { "b" : "abc" } ] }');
  AssertUnitHeader;
  AssertClassHeader('TaItem','TObject');
  AssertField('b','String');
  AssertVisibility('public');
  AssertSaverDeclaration;
  AssertProperty('b','String',False);
  AssertEnd;
  AssertArrayType('Ta','TaItem');
  AssertArraySaver('Ta','TaItem',True);
  AssertType;
  AssertClassHeader('TMyObject','TObject');
  AssertField('a','Ta');
  AssertVisibility('public');
  AssertSaverDeclaration;
  AssertProperty('a','ta',False);
  AssertEnd;
  AssertImplementation;
  AssertClassComment('Object Implementation','TaItem');
  AssertSaverImplementationStart('TaItem',True);
  AssertTrue('Have "b" string property save', Pos('AJSON.AddPair(''b'',TJSONString.Create(b));',NextLine)>0);
  AssertTrue('end',Pos('end;',NextLine)>0);
  AssertArraySaverImplementation('Ta','','TaItem',True);
  AssertClassComment('Object Implementation','TMyObject');
  AssertSaverImplementationStart('TMyObject');
  AssertTrue('Have "a" array property save', Pos('AJSON.AddPair(''a'',SaveTaToJSON(a));',NextLine)>0);
  AssertEnd('Loader TMyObject');
  AssertUnitEnd;
  AssertPropertyMap('','TMyObject','','TObject');
  AssertPropertyMap('a','Ta','a','');
end;

procedure TTestGenCode.TestLoadDelphiStringArrayProperty;
begin
  Gen.Options:=[jpoGenerateLoad, jpoDelphiJSON];
  GenCode('{ "a" : [ "abc" ] }');
  AssertLoadArray('string','String',True);
end;

procedure TTestGenCode.TestLoadDelphiBooleanArrayProperty;
begin
  Gen.Options:=[jpoGenerateLoad, jpoDelphiJSON];
  GenCode('{ "a" : [ true ] }');
  AssertLoadArray('boolean','Boolean',True);
end;

procedure TTestGenCode.TestLoadDelphiIntegerArrayProperty;
begin
  Gen.Options:=[jpoGenerateLoad, jpoDelphiJSON];
  GenCode('{ "a" : [ 12 ] }');
  AssertLoadArray('integer','Integer',True);
end;

procedure TTestGenCode.TestLoadDelphiInt64ArrayProperty;
begin
  Gen.Options:=[jpoGenerateLoad, jpoDelphiJSON];
  GenCode('{ "a" : [ 1234567890123 ] }');
  AssertLoadArray('int64','Int64',True);
end;

procedure TTestGenCode.TestLoadDelphiFloatArrayProperty;
begin
  Gen.Options:=[jpoGenerateLoad, jpoDelphiJSON];
  GenCode('{ "a" : [ 1.1 ] }');
  AssertLoadArray('double','Double',True);
end;


initialization

  RegisterTest(TTestGenCode);
end.

