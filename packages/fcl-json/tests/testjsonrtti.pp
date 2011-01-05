unit testjsonrtti;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, typinfo, fpjson,
  testcomps, testjsondata, fpjsonrtti;

type

  { TCJSONStreamer }

  TCJSONStreamer = class(TTestJSON)
  private
    FRJ : TJSONStreamer;
    FSR : TJSONObject;
    FToFree : TObject;
    FCalled : Boolean;
    procedure DoStreamProperty1(Sender: TObject; AObject: TObject; Info: PPropInfo; var Res: TJSONData);
  protected
    procedure SetUp; override; 
    procedure TearDown; override;
    Procedure AssertEquals(AMessage : String; Expected,Actual : TJSONType); overload;
    Procedure AssertPropCount(ACount : Integer);
    Function  AssertProperty(APropName : String; AType : TJSONType) : TJSONData;
    Procedure AssertProp(APropName : String; AValue : Boolean);
    Procedure AssertProp(APropName : String; AValue : Integer);
    procedure AssertProp(APropName : String; AValue: String);
    procedure AssertProp(APropName : String; AValue: TJSONFloat);
    procedure AssertProp(APropName : String; AValue : Array of String);
    procedure AssertProp(APropName : String; AValue : Array of Integer);
    function CreateVariantComp : TVariantComponent;
    procedure AssertNullProp(APropName : String);
    Function AssertObjectProp(APropName : String) : TJSONObject;
    Function AssertArrayProp(APropName : String) : TJSONArray;
    Function StreamObject(AObject : TObject) : TJSONObject;
    Property RJ : TJSONStreamer read FRJ;
    Property SR : TJSONObject Read FSR Write FSR;
  published
    procedure TestNil;
    procedure TestEmpty;
    procedure TestEmptyComponent;
    procedure TestWriteBoolean;
    procedure TestWriteInteger;
    procedure TestWriteString;
    procedure TestWriteFloat;
    procedure TestWriteFloat2;
    procedure TestWriteFloat3;
    procedure TestWriteFloat4;
    procedure TestWriteFloat5;
    procedure TestEnum1;
    procedure TestEnum2;
    Procedure TestSet1;
    Procedure TestSet2;
    Procedure TestSet3;
    Procedure TestSet4;
    Procedure TestObjectNil;
    Procedure TestComponentProp1;
    Procedure TestComponentProp2;
    Procedure TestCollectionProp1;
    Procedure TestCollectionProp2;
    Procedure TestPersistentProp1;
    Procedure TestStringsProp1;
    Procedure TestStringsProp2;
    procedure TestStringsProp3;
    procedure TestStringsProp4;
    procedure TestStringsArray;
    procedure TestStringsObject;
    procedure TestStringsStream1;
    procedure TestStringsStream2;
    procedure TestStringsStream3;
    procedure TestStringsStream4;
    procedure TestStringsStream5;
    procedure TestCollectionStream;
    procedure TestCollectionStream2;
    procedure TestOnStreamProperty;
    Procedure TestDateTimeProp;
    Procedure TestDateTimeProp2;
    Procedure TestDateTimeProp3;
    procedure TestDateTimeProp4;
    procedure TestDateTimeProp5;
    procedure TestDateTimeProp6;
    procedure TestDateTimeProp7;
    Procedure TestVariantShortint;
    Procedure TestVariantbyte;
    Procedure TestVariantword;
    Procedure TestVariantsmallint;
    Procedure TestVariantinteger;
    Procedure TestVariantlongword;
    Procedure TestVariantint64;
    Procedure TestVariantqword;
    Procedure TestVariantsingle;
    Procedure TestVariantdouble;
    Procedure TestVariantCurrency;
    Procedure TestVariantString;
    Procedure TestVariantolestr;
    Procedure TestVariantboolean;
    Procedure TestVariantDate;
    procedure TestVariantDate2;
    Procedure TestVariantArray;
    Procedure TestMultipleProps;
    Procedure TestObjectToJSONString;
    Procedure TestStringsToJSONString;
    Procedure TestCollectionToJSONString;
    Procedure TestChildren;
    Procedure TestChildren2;
  end;

  { TCJSONDeStreamer }

  TCJSONDeStreamer = class(TTestJSON)
  private
    FDS : TJSONDeStreamer;
    FJD : TJSONData;
    FToFree : TObject;
    FCalled : Boolean;
    procedure DeStream(JSON: TJSONStringType; AObject: TObject);
    procedure DeStream(JSON: TJSONObject; AObject: TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Procedure AssertVarType(Msg : String; AVarType : TVarType; Const Variant : Variant);
    Property DS : TJSONDeStreamer Read FDS;
    Property JD : TJSONData Read FJD Write FJD;
    Property Tofree : TObject Read FToFree Write FToFree;
  published
    Procedure TestVariantInteger;
    Procedure TestVariantFloat;
    Procedure TestVariantInt64;
    Procedure TestVariantBoolean;
    Procedure TestVariantNull;
    Procedure TestVariantString;
    Procedure TestVariantArray;
    procedure TestEmpty;
    procedure TestBoolean;
    procedure TestInteger;
    procedure TestString;
    procedure TestFloat;
    procedure TestFloat2;
    procedure TestFloat3;
    procedure TestFloat4;
    procedure TestFloat5;
    procedure TestEnum1;
    procedure TestEnum2;
    Procedure TestSet1;
    Procedure TestSet2;
    Procedure TestSet3;
    Procedure TestSet4;
    Procedure TestVariantProp;
    procedure TestCollection;
    procedure TestCollection2;
    procedure TestCollection3;
    procedure TestCollection4;
    procedure TestCollection5;
    procedure TestCollection6;
    procedure TestCollectionProp;
    procedure TestCollectionProp2;
    procedure TestStrings;
    procedure TestStrings2;
    procedure TestStrings3;
  end;

implementation

uses variants;

{ TCJSONDeStreamer }

procedure TCJSONDeStreamer.SetUp;
begin
  inherited SetUp;
  FDS:=TJSONDeStreamer.Create(Nil)
end;

procedure TCJSONDeStreamer.TearDown;
begin
  FreeAndNil(FDS);
  FreeAndNil(FJD);
  FreeAndNil(FToFree);
  inherited TearDown;
end;

procedure TCJSONDeStreamer.AssertVarType(Msg: String; AVarType: TVarType;
  const Variant: Variant);
begin
  AssertEquals(Msg,VarTypeAsText(AVarType),VarTypeAsText(VarType(Variant)));
end;

procedure TCJSONDeStreamer.TestVariantInteger;

Var
  V : Variant;

begin
  JD:=TJSOnIntegerNumber.Create(12);
  V:=DS.JSONToVariant(JD);
  AssertVarType('Integer data',varInteger,V);
  AssertEquals('Integer value',12,V);
end;

procedure TCJSONDeStreamer.TestVariantFloat;
Var
  V : Variant;

begin
  JD:=TJSOnFloatNumber.Create(1.2);
  V:=DS.JSONToVariant(JD);
  AssertVarType('Double data',varDouble,V);
  AssertEquals('Float value',1.2,V);
end;

procedure TCJSONDeStreamer.TestVariantInt64;
Var
  V : Variant;

begin
  JD:=TJSONInt64Number.Create(123);
  V:=DS.JSONToVariant(JD);
  AssertVarType('Int64 data',varInt64,V);
  AssertEquals('Int64 value',123,V);
end;

procedure TCJSONDeStreamer.TestVariantBoolean;
Var
  V : Variant;

begin
  JD:=TJSONBoolean.Create(True);
  V:=DS.JSONToVariant(JD);
  AssertVarType('Boolean data',varBoolean,V);
  AssertEquals('Boolean value',True,V);
end;

procedure TCJSONDeStreamer.TestVariantNull;
Var
  V : Variant;

begin
  JD:=TJSONNull.Create();
  V:=DS.JSONToVariant(JD);
  AssertVarType('Null data',varNull,V);
end;

procedure TCJSONDeStreamer.TestVariantString;
Var
  V : Variant;

begin
  JD:=TJSONString.Create('A string');
  V:=DS.JSONToVariant(JD);
  AssertVarType('String data',varString,V);
  AssertEquals('String data','A string',V);
end;

procedure TCJSONDeStreamer.TestVariantArray;
Var
  V : Variant;
begin
  JD:=TJSONArray.Create([1,2,3]);
  V:=DS.JSONToVariant(JD);
  AssertEQuals('Variant is array',true,VarIsArray(V));
  AssertEquals('Lower bound is zero ',0,VarArrayLowBound(V,1));
  AssertEquals('Higher bound is count-1 ',2,VarArrayHighBound(V,1));
  AssertEquals('Element 0 value correct ',1,V[0]);
  AssertEquals('Element 1 value correct ',2,V[1]);
  AssertEquals('Element 2 value correct ',3,V[2]);
end;

procedure TCJSONDeStreamer.TestEmpty;
begin
  FTofree:=TComponent.Create(Nil);
  DS.JSONToObject('{}',FTofree);
  AssertEquals('Empty name','',TComponent(FToFree).Name);
  AssertEquals('Empty Tag',0,TComponent(FToFree).Tag);
end;

procedure TCJSONDeStreamer.DeStream(JSON : TJSONStringType; AObject : TObject);

begin
  FToFree:=AObject;
  DS.JSONToObject(JSON,FTofree);
end;

procedure TCJSONDeStreamer.DeStream(JSON: TJSONObject; AObject: TObject);
begin
  FToFree:=AObject;
  JD:=JSON;
  DS.JSONToObject(JSON,FTofree);
end;

procedure TCJSONDeStreamer.TestBoolean;

Var
  B : TBooleanComponent;

begin
  B:=TBooleanComponent.Create(Nil);
  DeStream('{ "BooleanProp" : true }',B);
  AssertEquals('Correct boolean value',true,B.BooleanProp);
end;

procedure TCJSONDeStreamer.TestInteger;

Var
  B : TIntegerComponent;

begin
  B:=TIntegerComponent.Create(Nil);
  DeStream('{ "IntProp" : 22 }',B);
  AssertEquals('Correct integer value',22,B.IntProp);
end;

procedure TCJSONDeStreamer.TestString;

Var
  B : TStringComponent;

begin
  B:=TStringComponent.Create(Nil);
  DeStream('{ "StringProp" : "A nice string"}',B);
  AssertEquals('Correct string value','A nice string',B.StringProp);
end;

procedure TCJSONDeStreamer.TestFloat;

Var
  B : TSingleComponent;

begin
  B:=TSingleComponent.Create(Nil);
  DeStream('{ "SingleProp" : 2.34 }',B);
  AssertEquals('Correct single value',2.34,B.SingleProp);
end;

procedure TCJSONDeStreamer.TestFloat2;

Var
  B : TDoubleComponent;

begin
  B:=TDoubleComponent.Create(Nil);
  DeStream('{ "DoubleProp" : 3.45 }',B);
  AssertEquals('Correct Double value',3.45,B.DoubleProp);
end;

procedure TCJSONDeStreamer.TestFloat3;
Var
  B : TExtendedComponent;

begin
  B:=TExtendedComponent.Create(Nil);
  DeStream('{ "ExtendedProp" : 4.56 }',B);
  AssertEquals('Correct extended value',4.56,B.ExtendedProp);
end;

procedure TCJSONDeStreamer.TestFloat4;

Var
  B : TCompComponent;

begin
  B:=TCompComponent.Create(Nil);
  DeStream('{ "ExtendedProp" : 5.67 }',B);
{$ifdef CPUX86_64}
  AssertEquals('Correct comp value',round(5.67),B.ExtendedProp);
{$else}
  AssertEquals('Correct extended value',5.67,B.ExtendedProp);
{$endif}
end;

procedure TCJSONDeStreamer.TestFloat5;
Var
  B : TCurrencyComponent;

begin
  B:=TCurrencyComponent.Create(Nil);
  DeStream('{ "CurrencyProp" : 5.67 }',B);
  AssertEquals('Correct string value',5.67,B.CurrencyProp);
end;

procedure TCJSONDeStreamer.TestEnum1;

Var
  E : TEnumcomponent;

begin
  E:=TEnumComponent.Create(Nil);
  DeStream('{ "Dice" : 2 }',E);
  AssertEquals('Correct value',2,Ord(E.Dice));
end;

procedure TCJSONDeStreamer.TestEnum2;

Var
  E : TEnumcomponent;

begin
  E:=TEnumComponent.Create(Nil);
  DeStream('{ "Dice" : "three" }',E);
  AssertEquals('Correct value',GetEnumName(TypeInfo(TDice),Ord(Three)),GetEnumName(TypeInfo(TDice),Ord(E.Dice)));
end;

procedure TCJSONDeStreamer.TestSet1;

Var
  T : TSetComponent;

begin
  T:=TSetComponent.Create(Nil);
  DeStream('{ "Throw" : "one,two" }',T);
  If not (T.Throw=[one,two]) then
    Fail('Correct value for throw');
end;

procedure TCJSONDeStreamer.TestSet2;

Var
  T : TSetComponent;

begin
  T:=TSetComponent.Create(Nil);
  DeStream('{ "Throw" : "[one,two]" }',T);
  If not (T.Throw=[one,two]) then
    Fail('Correct value for throw');
end;

procedure TCJSONDeStreamer.TestSet3;

Var
  T : TSetComponent;

begin
  T:=TSetComponent.Create(Nil);
  DeStream('{ "Throw" : [ "one", "two"] }',T);
  If not (T.Throw=[one,two]) then
    Fail('Correct value for throw');
end;

procedure TCJSONDeStreamer.TestSet4;

Var
  T : TSetComponent;

begin
  T:=TSetComponent.Create(Nil);
  DeStream('{ "Throw" : [ 0 , 1 ] }',T);
  If not (T.Throw=[one,two]) then
    Fail('Correct value for throw');
end;

procedure TCJSONDeStreamer.TestVariantProp;
Var
  V : TVariantComponent;

begin
  V:=TVariantComponent.Create(Nil);
  DeStream('{ "VariantProp" : "A string" }',V);
  AssertEquals('Variant property value','A string',V.VariantProp);
end;

procedure TCJSONDeStreamer.TestCollection;

Var
  C : TTestCollection;

begin
  C:=TTestCollection.Create;
  DeStream('[ { "StrProp" : "one" }, { "StrProp" : "two" } ]',C);
  AssertEquals('Item count',2,C.Count);
  AssertEquals('Class item 0',TTestItem,C.Items[0].ClassType);
  AssertEquals('Class item 1',TTestItem,C.Items[1].ClassType);
  AssertEquals('Class item 0','one',TTestItem(C.Items[0]).StrProp);
  AssertEquals('Class item 1','two',TTestItem(C.Items[1]).StrProp);
end;

procedure TCJSONDeStreamer.TestCollection2;

Var
  C : TTestCollection;

begin
  C:=TTestCollection.Create;
  DeStream('{ "Items" : [ { "StrProp" : "one" }, { "StrProp" : "two" } ] }',C);
  AssertEquals('Item count',2,C.Count);
  AssertEquals('Class item 0',TTestItem,C.Items[0].ClassType);
  AssertEquals('Class item 1',TTestItem,C.Items[1].ClassType);
  AssertEquals('Class item 0','one',TTestItem(C.Items[0]).StrProp);
  AssertEquals('Class item 1','two',TTestItem(C.Items[1]).StrProp);
end;

procedure TCJSONDeStreamer.TestCollection3;

Var
  C : TTestCollection;

begin
  C:=TTestCollection.Create;
  FTofree:=C;
  DS.JSONToCollection('{ "Items" : [ { "StrProp" : "one" }, { "StrProp" : "two" } ] }',C);
  AssertEquals('Item count',2,C.Count);
  AssertEquals('Class item 0',TTestItem,C.Items[0].ClassType);
  AssertEquals('Class item 1',TTestItem,C.Items[1].ClassType);
  AssertEquals('Class item 0','one',TTestItem(C.Items[0]).StrProp);
  AssertEquals('Class item 1','two',TTestItem(C.Items[1]).StrProp);
end;

procedure TCJSONDeStreamer.TestCollection4;

Var
  C : TTestCollection;

begin
  C:=TTestCollection.Create;
  FTofree:=C;
  DS.JSONToCollection('[ { "StrProp" : "one" }, { "StrProp" : "two" } ]',C);
  AssertEquals('Item count',2,C.Count);
  AssertEquals('Class item 0',TTestItem,C.Items[0].ClassType);
  AssertEquals('Class item 1',TTestItem,C.Items[1].ClassType);
  AssertEquals('Class item 0','one',TTestItem(C.Items[0]).StrProp);
  AssertEquals('Class item 1','two',TTestItem(C.Items[1]).StrProp);
end;

procedure TCJSONDeStreamer.TestCollection5;

Var
  C : TTestCollection;

begin
  C:=TTestCollection.Create;
  FTofree:=C;
  JD:=TJSONArray.Create([TJSONObject.Create(['StrProp','one']),TJSONObject.Create(['StrProp','two'])]);
  DS.JSONToCollection(JD,C);
  AssertEquals('Item count',2,C.Count);
  AssertEquals('Class item 0',TTestItem,C.Items[0].ClassType);
  AssertEquals('Class item 1',TTestItem,C.Items[1].ClassType);
  AssertEquals('Class item 0','one',TTestItem(C.Items[0]).StrProp);
  AssertEquals('Class item 1','two',TTestItem(C.Items[1]).StrProp);
end;

procedure TCJSONDeStreamer.TestCollection6;
Var
  C : TTestCollection;

begin
  C:=TTestCollection.Create;
  FTofree:=C;
  JD:=TJSONObject.Create(['Items',TJSONArray.Create([TJSONObject.Create(['StrProp','one']),TJSONObject.Create(['StrProp','two'])])]);
  DS.JSONToCollection(JD,C);
  AssertEquals('Item count',2,C.Count);
  AssertEquals('Class item 0',TTestItem,C.Items[0].ClassType);
  AssertEquals('Class item 1',TTestItem,C.Items[1].ClassType);
  AssertEquals('Class item 0','one',TTestItem(C.Items[0]).StrProp);
  AssertEquals('Class item 1','two',TTestItem(C.Items[1]).StrProp);
end;

procedure TCJSONDeStreamer.TestCollectionProp;

Var
  C : TCollection;

begin
  JD:=TJSONObject.Create(['Coll',TJSONArray.Create([TJSONObject.Create(['StrProp','one']),TJSONObject.Create(['StrProp','two'])])]);
  DeStream(JD as TJSONObject,TCollectionComponent.Create(Nil));
  C:=TCollectionComponent(ToFree).Coll;
  AssertEquals('Item count',2,C.Count);
  AssertEquals('Class item 0',TTestItem,C.Items[0].ClassType);
  AssertEquals('Class item 1',TTestItem,C.Items[1].ClassType);
  AssertEquals('Class item 0','one',TTestItem(C.Items[0]).StrProp);
  AssertEquals('Class item 1','two',TTestItem(C.Items[1]).StrProp);
end;

procedure TCJSONDeStreamer.TestCollectionProp2;

Var
  C : TCollection;

begin

  DeStream('{ "Coll" : [ { "StrProp" : "one" }, { "StrProp" : "two" } ]}',TCollectionComponent.Create(Nil));
  C:=TCollectionComponent(ToFree).Coll;
  AssertEquals('Item count',2,C.Count);
  AssertEquals('Class item 0',TTestItem,C.Items[0].ClassType);
  AssertEquals('Class item 1',TTestItem,C.Items[1].ClassType);
  AssertEquals('Class item 0','one',TTestItem(C.Items[0]).StrProp);
  AssertEquals('Class item 1','two',TTestItem(C.Items[1]).StrProp);
end;

procedure TCJSONDeStreamer.TestStrings;

Var
  S : TStrings;

begin
  S:=TStringList.Create;
  FTofree:=S;
  DS.JSONToStrings('[ "one" , "two" ]',S);
  AssertEquals('Item count',2,S.Count);
  AssertEquals('First item','one',S[0]);
  AssertEquals('First item','two',S[1]);
end;

procedure TCJSONDeStreamer.TestStrings2;

Var
  S : TStrings;

begin
  S:=TStringList.Create;
  FTofree:=S;
  DS.JSONToStrings('{ "Strings" : [ "one" , "two" ] }',S);
  AssertEquals('Item count',2,S.Count);
  AssertEquals('First item','one',S[0]);
  AssertEquals('First item','two',S[1]);
end;

procedure TCJSONDeStreamer.TestStrings3;
Var
  S : TStrings;

begin
  S:=TStringList.Create;
  FTofree:=S;
  DS.JSONToStrings('{ "Strings" : [ "one" , "two" ] }',S);
  AssertEquals('Item count',2,S.Count);
  AssertEquals('First item','one',S[0]);
  AssertEquals('First item','two',S[1]);
end;

{ TCJSONStreamer }

function TCJSONStreamer.StreamObject(AObject: TObject): TJSONObject;
begin
  FToFree:=AObject;
  FSR:=FRJ.ObjectToJSON(AObject);
  Result:=FSR;
end;

procedure TCJSONStreamer.DoStreamProperty1(Sender: TObject; AObject: TObject;
  Info: PPropInfo; var Res: TJSONData);
begin
  If (info^.name<>'IntProp') and (info^.name<>'Name') and (info^.name<>'Tag') then
    Fail('Wrong property');
  If (info^.name='IntProp') then
    FreeAndNil(Res);
  FCalled:=true;
end;

procedure TCJSONStreamer.SetUp;
begin
  Inherited;
  FRJ:=TJSONStreamer.Create(Nil);
end;

procedure TCJSONStreamer.TearDown;
begin
  FreeAndNil(FSR);
  FreeAndNil(FRJ);
  FreeAndNil(FToFree);
  Inherited;
end;

procedure TCJSONStreamer.AssertEquals(AMessage: String; Expected, Actual: TJSONType);
begin
  AssertEquals(AMessage,GetEnumName(TypeInfo(TJSONType),Ord(Expected)),
                        GetEnumName(TypeInfo(TJSONType),Ord(Actual)));
end;

procedure TCJSONStreamer.AssertPropCount(ACount: Integer);
begin
  AssertNotNull('Result of streaming available',FSR);
  If FToFree is TComponent then
    ACount:=ACount+2; // Tag + Name
  Writeln(FSR.ASJSON);
  AssertEquals('Property count correct',ACount,FSR.Count);
end;

function TCJSONStreamer.AssertProperty(APropName: String; AType: TJSONType
  ): TJSONData;

Var
  i : Integer;

begin
  I:=FSR.IndexOfName(APropName);
  If (I=-1) then
    Fail('No property "'+APropName+'" available');
  Result:=FSR.Items[i];
  AssertEquals('Property "'+APropName+'" has correct type',GetEnumName(TypeInfo(TJSONType),Ord(AType)),
                                                           GetEnumName(TypeInfo(TJSONType),Ord(Result.JSONType)));
end;

procedure TCJSONStreamer.AssertProp(APropName: String; AValue: Boolean);
begin
  AssertNotNull('Result of streaming available',FSR);
  AssertEquals('Result of streaming is TJSONObject',TJSONObject,FSR.ClassType);
  AssertEquals('Correct value',AValue,AssertProperty(APropName,jtBoolean).AsBoolean);
end;

procedure TCJSONStreamer.AssertProp(APropName: String; AValue: Integer);
begin
  AssertNotNull('Result of streaming available',FSR);
  AssertEquals('Result of streaming is TJSONObject',TJSONObject,FSR.ClassType);
  AssertEquals('Correct value',AValue,AssertProperty(APropName,jtNumber).AsInteger);
end;

procedure TCJSONStreamer.AssertProp(APropName: String; AValue: String);
begin
  AssertNotNull('Result of streaming available',FSR);
  AssertEquals('Result of streaming is TJSONObject',TJSONObject,FSR.ClassType);
  AssertEquals('Correct value',AValue,AssertProperty(APropName,jtString).AsString);
end;

procedure TCJSONStreamer.AssertProp(APropName: String; AValue: TJSONFloat);
begin
  AssertNotNull('Result of streaming available',FSR);
  AssertEquals('Result of streaming is TJSONObject',TJSONObject,FSR.ClassType);
  AssertEquals('Correct value',AValue,AssertProperty(APropName,jtNumber).AsFloat);
end;

procedure TCJSONStreamer.AssertProp(APropName: String; AValue: array of String
  );
Var
  a : TJSONArray;
  i : integer;

begin
  a:=AssertArrayProp(APropName);
  For I:=Low(AValue) to High(Avalue) do
    begin
    AssertEquals('Array element type',jtString,A.Types[i]);
    AssertEquals('Array value',AValue[i],A.strings[i]);
    end;
end;

procedure TCJSONStreamer.AssertProp(APropName: String; AValue: array of Integer
  );
Var
  a : TJSONArray;
  i : integer;

begin
  a:=AssertArrayProp(APropName);
  For I:=Low(AValue) to High(Avalue) do
    begin
    AssertEquals('Array element type',jtNumber,A.Types[i]);
    AssertEquals('Array value',AValue[i],A.Integers[i]);
    end;
end;

function TCJSONStreamer.CreateVariantComp: TVariantComponent;
begin
  Result:=TVariantComponent.Create(Nil);
  FTofree:=Result;
end;

procedure TCJSONStreamer.AssertNullProp(APropName: String);
begin
  AssertProperty(APropName,jtNull);
end;

function TCJSONStreamer.AssertObjectProp(APropName: String): TJSONObject;
begin
  Result:=AssertProperty(APropName,jtObject) as TJSONObject;
end;

function TCJSONStreamer.AssertArrayProp(APropName: String): TJSONArray;
begin
  Result:=AssertProperty(APropName,jtArray) as TJSONArray;
end;

procedure TCJSONStreamer.TestNil;
begin
  AssertNull('Nil returns nil',StreamObject(Nil));
end;

procedure TCJSONStreamer.TestEmpty;
begin
  StreamObject(TemptyPersistent.Create);
  AssertPropCount(0);
end;

procedure TCJSONStreamer.TestEmptyComponent;
begin
  StreamObject(TComponent.Create(nil));
  AssertPropCount(0);
end;

procedure TCJSONStreamer.TestWriteBoolean;

begin
  StreamObject(TBooleanComponent.Create(nil));
  AssertPropCount(1);
  AssertProp('BooleanProp',False);
end;

procedure TCJSONStreamer.TestWriteInteger;
begin
  StreamObject(TIntegerComponent.Create(Nil));
  AssertPropCount(1);
  AssertProp('IntProp',3);
end;

procedure TCJSONStreamer.TestWriteString;
begin
  StreamObject(TStringComponent.Create(Nil));
  AssertPropCount(1);
  AssertProp('StringProp','A string');
end;

procedure TCJSONStreamer.TestWriteFloat;
begin
  StreamObject(TSingleComponent.Create(Nil));
  AssertPropCount(1);
  AssertProp('SingleProp',1.23);
end;

procedure TCJSONStreamer.TestWriteFloat2;
begin
  StreamObject(TDoubleComponent.Create(Nil));
  AssertPropCount(1);
  AssertProp('DoubleProp',2.34);
end;

procedure TCJSONStreamer.TestWriteFloat3;
begin
  StreamObject(TExtendedComponent.Create(Nil));
  AssertPropCount(1);
  AssertProp('ExtendedProp',3.45);
end;

procedure TCJSONStreamer.TestWriteFloat4;
begin
  StreamObject(TCompComponent.Create(Nil));
  AssertPropCount(1);
  // Extended is correct, propname is wrong
  {$ifdef CPUX86_64}
    AssertProp('ExtendedProp',TJSONFloat(5));
  {$else}
    AssertProp('ExtendedProp',4.56);
  {$endif}
end;

procedure TCJSONStreamer.TestWriteFloat5;
begin
  StreamObject(TCurrencyComponent.Create(Nil));
  AssertPropCount(1);
  AssertProp('CurrencyProp',5.67);
end;

procedure TCJSONStreamer.TestEnum1;
begin
  StreamObject(TEnumComponent3.Create(Nil));
  AssertPropCount(1);
  AssertProp('Dice',GetEnumName(TypeInfo(TDice),Ord(three)));
end;

procedure TCJSONStreamer.TestEnum2;
begin
  RJ.Options:=[jsoEnumeratedAsInteger];
  StreamObject(TEnumComponent3.Create(Nil));
  AssertProp('Dice',Ord(three));
end;

procedure TCJSONStreamer.TestSet1;
begin
  StreamObject(TSetComponent.Create(Nil));
  AssertPropCount(1);
  AssertProp('Throw',['two','five']);
end;

procedure TCJSONStreamer.TestSet2;
begin
  RJ.Options:=[jsoSetAsString];
  StreamObject(TSetComponent.Create(Nil));
  AssertPropCount(1);
  AssertProp('Throw','two,five');
end;

procedure TCJSONStreamer.TestSet3;
begin
  RJ.Options:=[jsoSetAsString,jsoSetBrackets];
  StreamObject(TSetComponent.Create(Nil));
  AssertPropCount(1);
  AssertProp('Throw','[two,five]');
end;

procedure TCJSONStreamer.TestSet4;
begin
  RJ.Options:=[jsoSetEnumeratedAsInteger];
  StreamObject(TSetComponent.Create(Nil));
  AssertPropCount(1);
  AssertProp('Throw',[Ord(two),Ord(five)]);
end;

procedure TCJSONStreamer.TestObjectNil;

Var
  P : TOwnedComponent;

begin
  P:=TOwnedComponent.Create(Nil);
  P.CompProp.Free;
  P.CompProp:=Nil;
  StreamObject(P);
  AssertPropCount(1);
  AssertNullProp('CompProp');
end;

procedure TCJSONStreamer.TestComponentProp1;
begin
  StreamObject(TOwnedComponent.Create(Nil));
  AssertPropCount(1);
  AssertProp('CompProp','SubComponent');
end;

procedure TCJSONStreamer.TestComponentProp2;

Var
  C : TOwnedComponent;
  F : TJSONObject;

begin
  RJ.Options:=[jsoComponentsInline];
  C:=TOwnedComponent.Create(Nil);
  StreamObject(C);
  AssertPropCount(1);
  F:=SR;
  try
    SR:=AssertObjectProp('CompProp');
    AssertPropCount(1);
    AssertProp('Name','SubComponent');
    Assertprop('Tag',0);
    AssertProp('IntProp',3);
  finally
    SR:=F;
  end;
end;

procedure TCJSONStreamer.TestCollectionProp1;

Var
  C : TCollectionComponent;
  F : TJSONObject;
  A : TJSONArray;

begin
  C:=TCollectionComponent2.Create(Nil);
  StreamObject(C);
  AssertPropCount(1);
  F:=SR;
  try
    A:=AssertArrayProp('Coll');
    AssertEquals('Collection item cound',3,A.Count);
    AssertEquals('Item 0 is object',jtObject,A.Types[0]);
    SR:=A.Objects[0];
    FToFree:=SR;
    AssertPropCount(1);
    AssertProp('StrProp','First');
    AssertEquals('Item 1 is object',jtObject,A.Types[1]);
    SR:=A.Objects[1];
    FToFree:=SR;
    AssertPropCount(1);
    AssertProp('StrProp','Second');
    AssertEquals('Item 2 is object',jtObject,A.Types[2]);
    SR:=A.Objects[2];
    FToFree:=SR;
    AssertPropCount(1);
    AssertProp('StrProp','Third');
  finally
    SR:=F;
    FToFree:=C;
  end;
end;

procedure TCJSONStreamer.TestCollectionProp2;

Var
  C : TCollectionComponent;
  F : TJSONObject;
  A : TJSONArray;

begin
  C:=TCollectionComponent.Create(Nil);
  StreamObject(C);
  AssertPropCount (1);
  A:=AssertArrayProp('Coll');
  AssertEquals('Collection item count',0,A.Count);
end;

procedure TCJSONStreamer.TestPersistentProp1;

var
  P : TPersistentComponent;
  F : TJSONObject;

begin
  P:=TPersistentComponent.Create(Nil);
  StreamObject(P);
  AssertPropCount(1);
  F:=SR;
  try
    SR:=AssertObjectProp('Persist');
    FToFree:=P.Persist;
    AssertPropCount(2);
    AssertProp('AString','A persistent string');
    AssertProp('AInteger',3);
  finally
    FToFree:=P;
    SR:=F;
  end;
end;

procedure TCJSONStreamer.TestStringsProp1;

Var
  A : TJSONArray;
begin
  RJ.Options:=[jsoTstringsAsArray];
  StreamObject(TStringsCOmponent.Create(Nil));
  AssertPropCount(1);
  AssertProp('StringsProp',['One','Two','Three']);
end;

procedure TCJSONStreamer.TestStringsProp2;

Var
  A : TJSONArray;
begin
  StreamObject(TStringsCOmponent.Create(Nil));
  AssertPropCount(1);
  AssertProp('StringsProp','One'+sLineBreak+'Two'+sLineBreak+'Three'+sLineBreak);
end;

procedure TCJSONStreamer.TestStringsProp3;

Var
  O : TJSONObject;
  S : TStringsComponent;

begin
  S:=TStringsCOmponent.Create(Nil);
  RJ.Options:=[jsoTstringsAsObject];
  StreamObject(S);
  AssertPropCount(1);
  O:=SR;
  SR:=AssertObjectprop('StringsProp');
  FTofree:=Nil;
  try
    AssertNullProp('One');
    AssertNullProp('Two');
    AssertNullProp('Three');
  finally
    SR:=o;
    FToFree:=S;
  end;
end;

procedure TCJSONStreamer.TestStringsProp4;

Var
  O,SP : TJSONObject;
  S : TStringsComponent;

begin
  S:=TStringsCOmponent.Create(Nil);
  S.StringsProp.Objects[0]:=TEmptyPersistent.Create;
  S.StringsProp.Objects[1]:=TEmptyPersistent.Create;
  S.StringsProp.Objects[2]:=TEmptyPersistent.Create;
  try
    RJ.Options:=[jsoTstringsAsObject];
    StreamObject(S);
    AssertPropCount(1);
    O:=SR;
    SP:=AssertObjectprop('StringsProp');
    SR:=SP;
    FTofree:=Nil;
    try
      SR:=AssertObjectProp('One');
      AssertPropCount(0);
      SR:=SP;
      SR:=AssertObjectProp('Two');
      AssertPropCount(0);
      SR:=SP;
      SR:=AssertObjectProp('Three');
      AssertPropCount(0);
    finally
      SR:=o;
      FToFree:=S;
    end;
  finally
    S.StringsProp.Objects[0].Free;
    S.StringsProp.Objects[1].Free;
    S.StringsProp.Objects[2].Free;
  end;
end;

procedure TCJSONStreamer.TestStringsArray;

Var
  O : TJSONArray;
  S : TStringList;

begin
  S:=TStringList.create;
  try
    S.Add('one');
    O:=RJ.StreamTStringsArray(S);
    try
      AssertEquals('one element',1,O.Count);
      AssertEquals('string type',jtString,O.Types[0]);
      AssertEquals('string value','one',O.Strings[0]);
    finally
      FreeAndNil(O);
    end;
  finally
    S.Free;
  end;
end;

procedure TCJSONStreamer.TestStringsObject;

Var
  O : TJSONObject;
  S : TStringList;

begin
  S:=TStringList.create;
  try
    S.AddObject('one',TEmptyPersistent.Create);
    O:=RJ.StreamTStringsObject(S);
    try
      AssertEquals('one element',1,O.Count);
      AssertEquals('Have property',0,O.IndexOfName('one'));
      AssertEquals('string type',jtObject,O.Types['one']);
      AssertEquals('string value','one',O.Names[0]);
    finally
      FreeAndNil(O);
    end;
  finally
    S.Objects[0].FRee;
    S.Free;
  end;
end;

procedure TCJSONStreamer.TestStringsStream1;

Var
  D: TJSONData;
  O : TJSONArray;
  S : TStringList;

begin
  S:=TStringList.create;
  try
    S.Add('one');
    RJ.Options:=[jsoTstringsAsArray];
    D:=RJ.StreamTStrings(S);
    try
      AssertEquals('Correct type',jtArray,D.JSONType);
      O:=D as TJSONArray;
      AssertEquals('one element',1,O.Count);
      AssertEquals('string type',jtString,O.Types[0]);
      AssertEquals('string value','one',O.Strings[0]);
    finally
      FreeAndNil(O);
    end;
  finally
    S.Free;
  end;
end;

procedure TCJSONStreamer.TestStringsStream2;

Var
  D : TJSONData;
  O : TJSONObject;
  S : TStringList;

begin
  S:=TStringList.create;
  try
    S.AddObject('one',TEmptyPersistent.Create);
    RJ.Options:=[jsoTstringsAsObject];
    D:=RJ.StreamTstrings(S);
    try
      AssertEquals('Correct type',jtObject,D.JSONType);
      O:=D as TJSONObject;
      AssertEquals('one element',1,O.Count);
      AssertEquals('Have property',0,O.IndexOfName('one'));
      AssertEquals('string type',jtObject,O.Types['one']);
      AssertEquals('string value','one',O.Names[0]);
    finally
      SR:=O;
    end;
  finally
    S.Objects[0].FRee;
    S.Free;
  end;
end;

procedure TCJSONStreamer.TestStringsStream3;
Var
  O : TJSONObject;
  S : TStringList;

begin
  S:=TStringList.create;
  try
    S.AddObject('one',TEmptyPersistent.Create);
    RJ.Options:=[jsoTstringsAsObject];
    SR:=RJ.ObjectToJSON(S);
    O:=AssertObjectProp('Strings');
    AssertEquals('one element',1,O.Count);
    AssertEquals('Have property',0,O.IndexOfName('one'));
    AssertEquals('string type',jtObject,O.Types['one']);
    AssertEquals('string value','one',O.Names[0]);
  finally
    S.Objects[0].FRee;
    S.Free;
  end;
end;

procedure TCJSONStreamer.TestStringsStream4;
Var
  O : TJSONObject;
  S : TStringList;

begin
  S:=TStringList.create;
  try
    S.AddObject('one',TEmptyPersistent.Create);
    SR:=RJ.ObjectToJSON(S);
    AssertProp('Strings','one'+sLinebreak);
  finally
    S.Objects[0].FRee;
    S.Free;
  end;
end;

procedure TCJSONStreamer.TestStringsStream5;
Var
  D : TJSONData;
  S : TStringList;

begin
  S:=TStringList.create;
  try
    S.AddObject('one',TEmptyPersistent.Create);
    D:=RJ.StreamTstrings(S);
    try
      AssertEquals('String data',jtString,D.JSONType);
      AssertEquals('String value','one'+sLineBreak,D.AsString);
    finally
      D.free;
    end;
  finally
    S.Objects[0].FRee;
    S.Free;
  end;
end;

procedure TCJSONStreamer.TestCollectionStream;

Var
  C : TTestCollection;
  A : TJSONArray;

begin
  C:=TTestCollection.Create;
  FToFree:=C;
  TTestItem(C.Add).StrProp:='One';
  TTestItem(C.Add).StrProp:='Two';
  A:=RJ.StreamCollection(C);
  try
    AssertNotNull('Have result',A);
    AssertEquals('2 items',2,A.Count);
    AssertEquals('Type item 0,',jtObject,A.Types[0]);
    AssertEquals('Type item 1,',jtObject,A.Types[1]);
    SR:=A.Objects[0];
    AssertPropCount(1);
    AssertProp('StrProp','One');
    SR:=A.Objects[1];
    AssertPropCount(1);
    AssertProp('StrProp','Two');
    SR:=Nil;
  finally
    FreeAndNil(A);
  end;
end;

procedure TCJSONStreamer.TestCollectionStream2;

Var
  C : TTestCollection;
  A : TJSONArray;
  o : TJSONObject;

begin
  C:=TTestCollection.Create;
  TTestItem(C.Add).StrProp:='One';
  TTestItem(C.Add).StrProp:='Two';
  FToFree:=C;
  StreamObject(C);
  O:=SR;
  try
    A:=AssertProperty('Items',jtArray) as TJSONArray;
    AssertNotNull('Have result',A);
    AssertEquals('2 items',2,A.Count);
    AssertEquals('Type item 0,',jtObject,A.Types[0]);
    AssertEquals('Type item 1,',jtObject,A.Types[1]);
    SR:=A.Objects[0];
    AssertPropCount(1);
    AssertProp('StrProp','One');
    SR:=A.Objects[1];
    AssertPropCount(1);
    AssertProp('StrProp','Two');
    SR:=Nil;
  finally
    SR:=O;
  end;
end;

procedure TCJSONStreamer.TestOnStreamProperty;
begin
  RJ.OnStreamProperty:=@DoStreamProperty1;
  StreamObject(TIntegerComponent.Create(Nil));
  AssertPropCount(0);
end;

procedure TCJSONStreamer.TestDateTimeProp;

Var
  D : Double;
begin
  StreamObject(TDateTimeComponent.Create(Nil));
  D:=EncodeDate(1996,8,1);
  AssertPropCount(1);
  AssertProp('DateTimeProp',D);
end;

procedure TCJSONStreamer.TestDateTimeProp2;
Var
  D : Double;
begin
  StreamObject(TDateTimeComponent2.Create(Nil));
  D:=EncodeTime(23,20,0,0);
  AssertPropCount(1);
  AssertProp('DateTimeProp',D);
end;

procedure TCJSONStreamer.TestDateTimeProp3;
Var
  D : Double;
begin
  StreamObject(TDateTimeComponent3.Create(Nil));
  D:=EncodeDate(1996,8,1)+EncodeTime(23,20,0,0);
  AssertPropCount(1);
  AssertProp('DateTimeProp',D);
end;

procedure TCJSONStreamer.TestDateTimeProp4;

begin
  RJ.Options:=[jsoDateTimeAsString];
  StreamObject(TDateTimeComponent.Create(Nil));
  AssertPropCount(1);
  AssertProp('DateTimeProp',DateToStr(EncodeDate(1996,8,1)));
end;

procedure TCJSONStreamer.TestDateTimeProp5;

begin
  RJ.Options:=[jsoDateTimeAsString];
  StreamObject(TDateTimeComponent2.Create(Nil));
  AssertPropCount(1);
  AssertProp('DateTimeProp',TimeToStr(EncodeTime(23,20,0,0)));
end;

procedure TCJSONStreamer.TestDateTimeProp6;

begin
  RJ.Options:=[jsoDateTimeAsString];
  StreamObject(TDateTimeComponent3.Create(Nil));
  AssertPropCount(1);
  AssertProp('DateTimeProp',DateTimeToStr(EncodeDate(1996,8,1)+EncodeTime(23,20,0,0)));
end;

procedure TCJSONStreamer.TestDateTimeProp7;
begin
  RJ.Options:=[jsoDateTimeAsString];
  RJ.DateTimeFormat:='hh:nn';
  StreamObject(TDateTimeComponent3.Create(Nil));
  AssertPropCount(1);
  AssertProp('DateTimeProp',FormatDateTime('hh:nn',EncodeDate(1996,8,1)+EncodeTime(23,20,0,0)));
end;

procedure TCJSONStreamer.TestVariantShortint;

Var
  i : ShortInt;
  C : TVariantComponent;

begin
  i:=3;
  C:=CreateVariantComp;
  C.VariantProp:=i;
  AssertEquals('Variant type',VarTypeAsText(varshortint),VarTypeAsText(VarType(C.VariantProp)));
  StreamObject(FTofree);
  AssertPropCount(1);
  AssertProp('VariantProp',3);
end;

procedure TCJSONStreamer.TestVariantbyte;
Var
  i : Byte;
  C : TVariantComponent;

begin
  i:=3;
  C:=CreateVariantComp;
  C.VariantProp:=i;
  AssertEquals('Variant type',VarTypeAsText(varByte),VarTypeAsText(VarType(C.VariantProp)));
  StreamObject(FTofree);
  AssertPropCount(1);
  AssertProp('VariantProp',3);
end;

procedure TCJSONStreamer.TestVariantword;

Var
  i : Word;
  C : TVariantComponent;

begin
  i:=3;
  C:=CreateVariantComp;
  C.VariantProp:=i;
  AssertEquals('Variant type',VarTypeAsText(varWord),VarTypeAsText(VarType(C.VariantProp)));
  StreamObject(FTofree);
  AssertPropCount(1);
  AssertProp('VariantProp',3);
end;

procedure TCJSONStreamer.TestVariantsmallint;

Var
  i : Smallint;
  C : TVariantComponent;

begin
  i:=3;
  C:=CreateVariantComp;
  C.VariantProp:=i;
  AssertEquals('Variant type',VarTypeAsText(varSmallint),VarTypeAsText(VarType(C.VariantProp)));
  StreamObject(FTofree);
  AssertPropCount(1);
  AssertProp('VariantProp',3);
end;

procedure TCJSONStreamer.TestVariantinteger;
Var
  i : Integer;
  C : TVariantComponent;

begin
  i:=3;
  C:=CreateVariantComp;
  C.VariantProp:=i;
  AssertEquals('Variant type',VarTypeAsText(varInteger),VarTypeAsText(VarType(C.VariantProp)));
  StreamObject(FTofree);
  AssertPropCount(1);
  AssertProp('VariantProp',3);
end;

procedure TCJSONStreamer.TestVariantlongword;

Var
  i : Cardinal;
  C : TVariantComponent;

begin
  i:=3;
  C:=CreateVariantComp;
  C.VariantProp:=i;
  AssertEquals('Variant type',VarTypeAsText(varLongword),VarTypeAsText(VarType(C.VariantProp)));
  StreamObject(FTofree);
  AssertPropCount(1);
  AssertProp('VariantProp',3);
end;

procedure TCJSONStreamer.TestVariantint64;
Var
  i : Int64;
  C : TVariantComponent;

begin
  i:=3;
  C:=CreateVariantComp;
  C.VariantProp:=i;
  AssertEquals('Variant type',VarTypeAsText(varInt64),VarTypeAsText(VarType(C.VariantProp)));
  StreamObject(FTofree);
  AssertPropCount(1);
  AssertProp('VariantProp',3);
end;

procedure TCJSONStreamer.TestVariantqword;
Var
  i : QWord;
  C : TVariantComponent;

begin
  i:=3;
  C:=CreateVariantComp;
  C.VariantProp:=i;
  AssertEquals('Variant type',VarTypeAsText(varQWord),VarTypeAsText(VarType(C.VariantProp)));
  StreamObject(FTofree);
  AssertPropCount(1);
  AssertProp('VariantProp',3);
end;

procedure TCJSONStreamer.TestVariantsingle;
Var
  i : Single;
  C : TVariantComponent;

begin
  i:=3.14;
  C:=CreateVariantComp;
  C.VariantProp:=VarAsType(3.14,varSingle);
  AssertEquals('Variant type',VarTypeAsText(varSingle),VarTypeAsText(VarType(C.VariantProp)));
  StreamObject(FTofree);
  AssertPropCount(1);
  AssertProp('VariantProp',3.14);
end;

procedure TCJSONStreamer.TestVariantdouble;

Var
  i : Double;
  C : TVariantComponent;

begin
  i:=3.14;
  C:=CreateVariantComp;
  C.VariantProp:=i;
  AssertEquals('Variant type',VarTypeAsText(varDouble),VarTypeAsText(VarType(C.VariantProp)));
  StreamObject(FTofree);
  AssertPropCount(1);
  AssertProp('VariantProp',3.14);
end;

procedure TCJSONStreamer.TestVariantCurrency;
Var
  i : Currency;
  C : TVariantComponent;

begin
  i:=3.14;
  C:=CreateVariantComp;
  C.VariantProp:=i;
  AssertEquals('Variant type',VarTypeAsText(varCurrency),VarTypeAsText(VarType(C.VariantProp)));
  StreamObject(FTofree);
  AssertPropCount(1);
  AssertProp('VariantProp',3.14);
end;

procedure TCJSONStreamer.TestVariantString;

Var
  i : String;
  C : TVariantComponent;

begin
  i:='3.14';
  C:=CreateVariantComp;
  C.VariantProp:=i;
  AssertEquals('Variant type',VarTypeAsText(varString),VarTypeAsText(VarType(C.VariantProp)));
  StreamObject(FTofree);
  AssertPropCount(1);
  AssertProp('VariantProp','3.14');
end;

procedure TCJSONStreamer.TestVariantolestr;

Var
  i : String;
  C : TVariantComponent;

begin
  i:='3.14';
  C:=CreateVariantComp;
  C.VariantProp:=VarAsType(i,varOleStr);
  AssertEquals('Variant type',VarTypeAsText(varOleStr),VarTypeAsText(VarType(C.VariantProp)));
  StreamObject(FTofree);
  AssertPropCount(1);
  AssertProp('VariantProp','3.14');
end;

procedure TCJSONStreamer.TestVariantboolean;
Var
  i : Boolean;
  C : TVariantComponent;

begin
  i:=True;
  C:=CreateVariantComp;
  C.VariantProp:=i;
  AssertEquals('Variant type',VarTypeAsText(varBoolean),VarTypeAsText(VarType(C.VariantProp)));
  StreamObject(FTofree);
  AssertPropCount(1);
  AssertProp('VariantProp',True);
end;

procedure TCJSONStreamer.TestVariantDate;

Var
  i : TDateTime;
  C : TVariantComponent;

begin
  i:=EncodeDate(2010,12,23);
  C:=CreateVariantComp;
  C.VariantProp:=i;
  AssertEquals('Variant type',VarTypeAsText(varDate),VarTypeAsText(VarType(C.VariantProp)));
  StreamObject(FTofree);
  AssertPropCount(1);
  AssertProp('VariantProp',EncodeDate(2010,12,23));
end;

procedure TCJSONStreamer.TestVariantDate2;

Var
  i : TDateTime;
  C : TVariantComponent;

begin
  RJ.Options:=[jsoDateTimeAsString];
  i:=EncodeDate(2010,12,23);
  C:=CreateVariantComp;
  C.VariantProp:=i;
  AssertEquals('Variant type',VarTypeAsText(varDate),VarTypeAsText(VarType(C.VariantProp)));
  StreamObject(FTofree);
  AssertPropCount(1);
  AssertProp('VariantProp',DateToStr(EncodeDate(2010,12,23)));
end;

procedure TCJSONStreamer.TestVariantArray;
Var
  i : Integer;
  V : Variant;
  C : TVariantComponent;
  A : TJSONArray;

begin
  V:=VarArrayCreate([1,10],varInteger);
  For I:=1 to 10 do
    V[i]:=11-I;
  C:=CreateVariantComp;
  C.VariantProp:=V;
  StreamObject(FTofree);
  AssertPropCount(1);
  A:=AssertProperty('VariantProp',jtArray) as TJSONArray;
  AssertEquals('10 elements in array',10,A.Count);
  For I:=1 to 10 do
    begin
    assertEquals('Type of element',jtNumber,A.Types[i-1]);
    AssertEquals('Value of element',11-i,A.Integers[i-1]);
    end;
end;

procedure TCJSONStreamer.TestMultipleProps;
begin
  StreamObject(TMultipleComponent.Create(Nil));
  AssertPropCount(5);
  AssertProp('IntProp',1);
  Assertprop('StringProp','A String');
  AssertProp('CurrencyProp',2.3);
  AssertProp('Throw',['three','four']);
  AssertProp('Dice','two');
end;

procedure TCJSONStreamer.TestObjectToJSONString;
begin
  StreamObject(TIntegerComponent.Create(Nil));
  AssertEquals('Correct stream',SR.AsJSON,RJ.ObjectToJSONString(FToFree));
end;

procedure TCJSONStreamer.TestStringsToJSONString;
Var
  S : TStrings;
begin
  S:=TStringList.Create;
  try
    S.Add('one');
    S.Add('two');
    S.Add('three');
    AssertEquals('StringsToJSONString','["one", "two", "three"]',RJ.StringsToJSON(S));
    AssertEquals('StringsToJSONString','{ "one" : null, "two" : null, "three" : null }',RJ.StringsToJSON(S,True));
  finally
    FreeAndNil(S);
  end;
end;

procedure TCJSONStreamer.TestCollectionToJSONString;

Var
  C : TTestCollection;

begin
  C:=TTestCollection.Create;
  try
    (C.Add as TTestItem).StrProp:='one';
    (C.Add as TTestItem).StrProp:='two';
    (C.Add as TTestItem).StrProp:='three';
    AssertEquals('CollectionToJSON','[{ "StrProp" : "one" }, { "StrProp" : "two" }, { "StrProp" : "three" }]',RJ.CollectionToJSON(C));
  finally
    FreeAndNil(C);
  end;
end;

procedure TCJSONStreamer.TestChildren;

Var
  C : TChildrenComponent;

begin
  C:=TChildrenComponent.Create(Nil);
  TComponent.Create(C).Name:='Child1';
  TComponent.Create(C).Name:='Child2';
  StreamObject(C);
  If SR.IndexOfName('Children')<>-1 then
    Fail('Children streamed with default options');

end;

procedure TCJSONStreamer.TestChildren2;
Var
  C : TChildrenComponent;
  A : TJSONArray;
  O : TJSONObject;

begin
  C:=TChildrenComponent.Create(Nil);
  TComponent.Create(C).Name:='Child1';
  TComponent.Create(C).Name:='Child2';
  RJ.Options:=[jsoStreamChildren];
  StreamObject(C);
  AssertPropCount(1);
  A:=AssertProperty('Children',jtArray) as TJSONArray;
  O:=SR;
  try
    AssertEquals('2 Elements in array',2,A.Count);
    AssertEquals('First in array is object',jtObject,A.Types[0]);
    AssertEquals('Second in array is object',jtObject,A.Types[1]);
    SR:=A.Objects[0];
    AssertProp('Name','Child1');
    SR:=A.Objects[1];
    AssertProp('Name','Child2');
  finally
    SR:=O;
  end;
end;

initialization

  RegisterTests([TCJSONStreamer,TCJSONDeStreamer]);
end.

