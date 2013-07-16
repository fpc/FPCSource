unit tests_rtti;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

interface

uses
{$IFDEF FPC}
  fpcunit,testregistry, testutils,
{$ELSE FPC}
  TestFramework,
{$ENDIF FPC}
  Classes, SysUtils, typinfo,
  Rtti;

type

  { TTestCase1 }

  TTestCase1= class(TTestCase)
  published
    procedure GetTypes;
    procedure GetTypeInteger;
    procedure GetClassProperties;

    procedure GetClassAttributes;
    procedure GetClassPropertiesAttributes;

    procedure GetClassPropertiesValue;

    procedure TestTRttiTypeProperties;
    procedure TestPropGetValueString;
    procedure TestPropGetValueInteger;
    procedure TestPropGetValueBoolean;
    procedure TestGetValueStringCastError;
  end;

implementation

type
  { TIntAttribute }

  TIntAttribute = class(TCustomAttribute)
  private
    FInt: Integer;
  public
    constructor create(AInt: integer); virtual;
    property Int: integer read FInt;
  end;

  [TIntAttribute(1)]
  [TIntAttribute(2)]
  TGetClassProperties = class
  private
    FPubPropRO: integer;
    FPubPropRW: integer;
  published
    property PubPropRO: integer read FPubPropRO;
    [TIntAttribute(3)]
    property PubPropRW: integer read FPubPropRW write FPubPropRW;
    property PubPropSetRO: integer read FPubPropRO;
    [TIntAttribute(4)]
    property PubPropSetRW: integer read FPubPropRW write FPubPropRW;
  end;

  TTestValueClass = class
  private
    FAInteger: integer;
    FAString: string;
    FABoolean: boolean;
  published
    property AInteger: Integer read FAInteger write FAInteger;
    property AString: string read FAString write FAString;
    property ABoolean: boolean read FABoolean write FABoolean;
  end;


{ TIntAttribute }

constructor TIntAttribute.create(AInt: integer);
begin
  FInt:=AInt;
end;

procedure TTestCase1.GetTypes;
var
  LContext: TRttiContext;
  LType: TRttiType;
  IsTestCaseClassFound: boolean;
begin
  LContext := TRttiContext.Create;

  { Enumerate all types declared in the application }
  for LType in LContext.GetTypes() do
    begin
    if LType.Name='TTestCase1' then
      IsTestCaseClassFound:=true;
    end;
  LContext.Free;
  CheckTrue(IsTestCaseClassFound, 'RTTI information does not contain class of testcase.');
end;

procedure TTestCase1.TestGetValueStringCastError;
var
  ATestClass : TTestValueClass;
  c: TRttiContext;
  ARttiType: TRttiType;
  AValue: TValue;
  i: integer;
  HadException: boolean;
begin
  c := TRttiContext.Create;
  try
    ATestClass := TTestValueClass.Create;
    ATestClass.AString := '12';
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);
      AValue := ARttiType.GetProperty('astring').GetValue(ATestClass);
      HadException := false;
      try
        i := AValue.AsInteger;
      except
        on E: Exception do
          if E.ClassType=EInvalidCast then
            HadException := true;
      end;
      Check(HadException, 'No or invalid exception on invalid cast');
    finally
      AtestClass.Free;
    end;
  finally
    c.Free;
  end;
end;

procedure TTestCase1.TestPropGetValueBoolean;
var
  ATestClass : TTestValueClass;
  c: TRttiContext;
  ARttiType: TRttiType;
  AProperty: TRttiProperty;
  AValue: TValue;
begin
  c := TRttiContext.Create;
  try
    ATestClass := TTestValueClass.Create;
    ATestClass.ABoolean := true;
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);
      Check(assigned(ARttiType));
      AProperty := ARttiType.GetProperty('aBoolean');
      AValue := AProperty.GetValue(ATestClass);
      CheckEquals(true,AValue.AsBoolean);
      ATestClass.ABoolean := false;
      CheckEquals(true, AValue.AsBoolean);
      CheckEquals('True', AValue.ToString);
      CheckEquals(True, AValue.IsOrdinal);
      CheckEquals(1, AValue.AsOrdinal);
    finally
      AtestClass.Free;
    end;
      CheckEquals(True,AValue.AsBoolean);
  finally
    c.Free;
  end;
end;

procedure TTestCase1.TestPropGetValueInteger;
var
  ATestClass : TTestValueClass;
  c: TRttiContext;
  ARttiType: TRttiType;
  AProperty: TRttiProperty;
  AValue: TValue;
begin
  c := TRttiContext.Create;
  try
    ATestClass := TTestValueClass.Create;
    ATestClass.AInteger := 472349;
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);
      Check(assigned(ARttiType));
      AProperty := ARttiType.GetProperty('ainteger');
      AValue := AProperty.GetValue(ATestClass);
      CheckEquals(472349,AValue.AsInteger);
      ATestClass.AInteger := 12;
      CheckEquals(472349, AValue.AsInteger);
      CheckEquals('472349', AValue.ToString);
      CheckEquals(True, AValue.IsOrdinal);
    finally
      AtestClass.Free;
    end;
      CheckEquals(472349,AValue.AsInteger);
  finally
    c.Free;
  end;
end;

procedure TTestCase1.TestPropGetValueString;
var
  ATestClass : TTestValueClass;
  c: TRttiContext;
  ARttiType: TRttiType;
  AProperty: TRttiProperty;
  AValue: TValue;
  i: int64;
begin
  c := TRttiContext.Create;
  try
    ATestClass := TTestValueClass.Create;
    ATestClass.AString := 'Hello World';
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);
      Check(assigned(ARttiType));
      AProperty := ARttiType.GetProperty('astring');
      AValue := AProperty.GetValue(ATestClass);
      CheckEquals('Hello World',AValue.AsString);
      ATestClass.AString := 'Goodbye World';
      CheckEquals('Hello World',AValue.AsString);
      CheckEquals('Hello World',AValue.ToString);
      Check(TypeInfo(string)=AValue.TypeInfo);
      Check(AValue.TypeData=GetTypeData(AValue.TypeInfo));
//      Check(AValue.IsEmpty=false);
      Check(AValue.IsObject=false);
      Check(AValue.IsClass=false);
      CheckEquals(AValue.IsOrdinal, false);
      CheckEquals(AValue.TryAsOrdinal(i), false);
      CheckEquals(AValue.IsType(TypeInfo(string)), true);
      CheckEquals(AValue.IsType(TypeInfo(integer)), false);
      CheckEquals(AValue.IsArray, false);
    finally
      AtestClass.Free;
    end;
    CheckEquals('Hello World',AValue.AsString);
  finally
    c.Free;
  end;
end;

procedure TTestCase1.TestTRttiTypeProperties;
var
  c: TRttiContext;
  ARttiType: TRttiType;

begin
  c := TRttiContext.Create;
  try
    ARttiType := c.GetType(TTestValueClass);
    Check(assigned(ARttiType));
    CheckEquals(ARttiType.Name,'TTestValueClass');
    Check(ARttiType.TypeKind=tkClass);
//    CheckEquals(ARttiType.IsPublicType,false);
    CheckEquals(ARttiType.TypeSize,4);
    CheckEquals(ARttiType.IsManaged,false);
    CheckEquals(ARttiType.BaseType.classname,'TRttiInstanceType');
    CheckEquals(ARttiType.IsInstance,True);
    CheckEquals(ARttiType.AsInstance.DeclaringUnitName,'tests_rtti');
    Check(ARttiType.BaseType.Name='TObject');
    Check(ARttiType.AsInstance.BaseType.Name='TObject');
    CheckEquals(ARttiType.IsOrdinal,False);
    CheckEquals(ARttiType.IsRecord,False);
    CheckEquals(ARttiType.IsSet,False);
  finally
    c.Free;
  end;

end;

procedure TTestCase1.GetTypeInteger;
var
  LContext: TRttiContext;
  LType: TRttiType;
begin
  LContext := TRttiContext.Create;

  LType := LContext.GetType(TypeInfo(integer));
  CheckEquals(LType.Name, 'LongInt');

  LContext.Free;
end;

procedure TTestCase1.GetClassProperties;
var
  LContext: TRttiContext;
  LType: TRttiType;
  PropList: {$ifdef fpc}specialize{$endif} TArray<TRttiProperty>;
begin
  LContext := TRttiContext.Create;

  LType := LContext.GetType(TypeInfo(TGetClassProperties));
  PropList := LType.GetProperties;

  CheckEquals(4, length(PropList));
  CheckEquals('PubPropRO', PropList[0].Name);
  CheckEquals('PubPropRW', PropList[1].Name);
  CheckEquals('PubPropSetRO', PropList[2].Name);
  CheckEquals('PubPropSetRW', PropList[3].Name);

  LContext.Free;
end;

procedure TTestCase1.GetClassAttributes;
var
  LContext: TRttiContext;
  LType: TRttiType;
  AttrList: {$ifdef fpc}specialize{$endif} TArray<TCustomAttribute>;
begin
  LContext := TRttiContext.Create;

  LType := LContext.GetType(TypeInfo(TGetClassProperties));
  AttrList := LType.GetAttributes;

  CheckEquals(2, length(AttrList));
  CheckEquals('TIntAttribute', AttrList[0].ClassName);
  CheckEquals('TIntAttribute', AttrList[1].ClassName);
  CheckEquals(1, TIntAttribute(AttrList[0]).Int);
  CheckEquals(2, TIntAttribute(AttrList[1]).Int);

  LContext.Free;
end;

procedure TTestCase1.GetClassPropertiesAttributes;
var
  LContext: TRttiContext;
  LType: TRttiType;
  PropList: {$ifdef fpc}specialize{$endif} TArray<TRttiProperty>;
  AttrList: {$ifdef fpc}specialize{$endif} TArray<TCustomAttribute>;
begin
  LContext := TRttiContext.Create;

  LType := LContext.GetType(TypeInfo(TGetClassProperties));
  PropList := LType.GetProperties;

  AttrList := PropList[1].GetAttributes;
  CheckEquals(1, length(AttrList));
  CheckEquals(3, TIntAttribute(AttrList[0]).Int);

  AttrList := PropList[3].GetAttributes;
  CheckEquals(1, length(AttrList));
  CheckEquals(4, TIntAttribute(AttrList[0]).Int);

  LContext.Free;
end;

procedure TTestCase1.GetClassPropertiesValue;
var
  AGetClassProperties: TGetClassProperties;
  LContext: TRttiContext;
  LType: TRttiType;
  AValue: TValue;
begin
  LContext := TRttiContext.Create;

  LType := LContext.GetType(TGetClassProperties);

  AGetClassProperties := TGetClassProperties.Create;
  try
    AGetClassProperties.PubPropRW:=12345;

    AValue := LType.GetProperty('PubPropRW').GetValue(AGetClassProperties);
    CheckEquals(12345, AValue.AsInteger);

  finally
    AGetClassProperties.Free;
  end;

  LContext.Free;
end;

initialization
{$ifdef fpc}
  RegisterTest(TTestCase1);
{$else fpc}
  RegisterTest(TTestCase1.Suite);
{$endif fpc}
end.

