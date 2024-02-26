unit tests.rtti;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
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

  { TTestRTTI }

  TTestRTTI= class(TTestCase)
  published
    //procedure GetTypes;
    procedure GetTypeInteger;
    procedure GetTypePointer;
    procedure GetClassProperties;

    procedure GetClassPropertiesValue;

    procedure TestTRttiTypeProperties;
    procedure TestPropGetValueString;
    procedure TestPropGetValueInteger;
    procedure TestPropGetValueBoolean;
    procedure TestPropGetValueShortString;
    procedure TestPropGetValueProcString;
    procedure TestPropGetValueProcInteger;
    procedure TestPropGetValueProcBoolean;
    procedure TestPropGetValueProcShortString;
    procedure TestPropGetValueObject;
    procedure TestPropGetValueInterface;
    procedure TestPropGetValueFloat;
    procedure TestPropGetValueDynArray;
    procedure TestPropGetValueEnumeration;
    procedure TestPropGetValueChars;

    procedure TestPropSetValueString;
    procedure TestPropSetValueInteger;
    procedure TestPropSetValueBoolean;
    procedure TestPropSetValueShortString;
    procedure TestPropSetValueObject;
    procedure TestPropSetValueInterface;
    procedure TestPropSetValueFloat;
    procedure TestPropSetValueDynArray;
    procedure TestPropSetValueEnumeration;
    procedure TestPropSetValueChars;

    procedure TestGetValueStringCastError;
    procedure TestGetIsReadable;
    procedure TestIsWritable;

    procedure TestGetAttribute;

    procedure TestInterface;
{$ifdef fpc}
    procedure TestInterfaceRaw;
{$endif}

    procedure TestArray;
    procedure TestDynArray;

    procedure TestProcVar;
    procedure TestMethod;

    procedure TestRawThunk;

  private
{$ifndef fpc}
    procedure Ignore(const aMsg: String);
{$endif}
  end;

  { TTestExtendedRTTI }
  // Note: the tests assume that TObject has no RTTI associated with it.
  // The tests need to be adapted so they will work in both cases.
  TTestExtendedRTTI = class(TTestCase)
  Private
    FCtx: TRttiContext;
    Procedure AssertEquals(Msg : String; aExpected,aActual : TMemberVisibility); overload;
    Procedure AssertEquals(Msg : String; aExpected,aActual : TTypeKind);overload;
    procedure CheckField(aIdx: Integer; aData: TRttiField; aName: String; aKind: TTypeKind; aVisibility: TMemberVisibility;
      aStrict: Boolean=False);
    procedure CheckMethod(aPrefix: string; aIdx: Integer; aData: TRttiMethod; aName: String; aVisibility: TMemberVisibility;
      aStrict: Boolean=False);
    procedure CheckProperty(aIdx: Integer; aData: TRttiProperty; aName: String; aKind: TTypeKind; aVisibility: TMemberVisibility;
      isStrict: Boolean=False);
  public
    Procedure Setup; override;
    Procedure TearDown; override;
  end;

  { TTestClassExtendedRTTI }

  TTestClassExtendedRTTI = class(TTestExtendedRtti)
  published
    Procedure TestFields;
    Procedure TestProperties;
    Procedure TestDeclaredMethods;
    Procedure TestMethods;
    Procedure TestPrivateFieldAttributes;
    Procedure TestProtectedFieldAttributes;
    Procedure TestPublicFieldAttributes;
    Procedure TestPrivatePropertyAttributes;
    Procedure TestProtectedPropertyAttributes;
    Procedure TestPublicPropertyAttributes;
    Procedure TestPublishedPropertyAttributes;
  end;

  { TTestRecordExtendedRTTI }

  TTestRecordExtendedRTTI = class(TTestExtendedRtti)
  published
    Procedure TestFields;
    Procedure TestProperties;
    Procedure TestDeclaredMethods;
    Procedure TestMethods;
    Procedure TestPrivateFieldAttributes;
    Procedure TestPublicFieldAttributes;
  end;


implementation

uses
  Tests.Rtti.Util, {tests.rtti.exttypes, } tests.rtti.attrtypes, tests.rtti.types;




{ Note: GetTypes currently only returns those types that had been acquired using
        GetType, so GetTypes itself can't be really tested currently }
(*procedure TTestRTTI.GetTypes;
var
  LContext: TRttiContext;
  LType: TRttiType;
  IsTestCaseClassFound: boolean;
begin
  LContext := TRttiContext.Create;

  { Enumerate all types declared in the application }
  for LType in LContext.GetTypes() do
    begin
    if LType.Name='TTestRTTI' then
      IsTestCaseClassFound:=true;
    end;
  LContext.Free;
  CheckTrue(IsTestCaseClassFound, 'RTTI information does not contain class of testcase.');
end;*)

{$ifndef fpc}
procedure TTestRTTI.Ignore(const aMsg: string);
begin
  { empty }
end;
{$endif}

procedure TTestRTTI.TestGetValueStringCastError;
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


procedure TTestRTTI.TestGetIsReadable;
var
  c: TRttiContext;
  ARttiType: TRttiType;
  AProperty: TRttiProperty;
begin
  c := TRttiContext.Create;
  try
    ARttiType := c.GetType(TTestValueClass);
    AProperty := ARttiType.GetProperty('aBoolean');
    CheckEquals(AProperty.IsReadable, true);
    AProperty := ARttiType.GetProperty('aGetBoolean');
    CheckEquals(AProperty.IsReadable, true);
    AProperty := ARttiType.GetProperty('aWriteOnly');
    CheckEquals(AProperty.IsReadable, False);
  finally
    c.Free;
  end;
end;

procedure TTestRTTI.TestIsWritable;
var
  c: TRttiContext;
  ARttiType: TRttiType;
  AProperty: TRttiProperty;
begin
  c := TRttiContext.Create;
  try
    ARttiType := c.GetType(TTestValueClass);
    AProperty := ARttiType.GetProperty('aBoolean');
    CheckEquals(AProperty.IsWritable, true);
    AProperty := ARttiType.GetProperty('aGetBoolean');
    CheckEquals(AProperty.IsWritable, false);
    AProperty := ARttiType.GetProperty('aWriteOnly');
    CheckEquals(AProperty.IsWritable, True);
  finally
    c.Free;
  end;
end;

procedure TTestRTTI.TestGetAttribute;
// TMyAnnotatedClass
// TMyAttribute

var
  c: TRttiContext;
  aType: TRttiType;
  aClass : TMyAnnotatedClass;
  custAttr : TCustomAttribute;
  myAttr : TMyAttribute absolute custattr;

begin
  aType:=nil;
  custAttr:=Nil;
  c := TRttiContext.Create;
  try
    aClass:=TMyAnnotatedClass.Create;
    aType := c.GetType(aClass.ClassInfo);
    custAttr:=aType.GetAttribute(TMyAttribute);
    CheckEquals(custAttr.ClassType,TMyAttribute,'Correct class');
    CheckEquals('something',MyAttr.value,'Correct value');
  finally
    aClass.Free;
//    custAttr.Free;
    C.Free;
  end;
end;


procedure TTestRTTI.TestPropGetValueBoolean;
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

procedure TTestRTTI.TestPropGetValueShortString;
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
    ATestClass.AShortString := 'Hello World';
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);
      Check(assigned(ARttiType));
      AProperty := ARttiType.GetProperty('aShortString');
      AValue := AProperty.GetValue(ATestClass);
      CheckEquals('Hello World',AValue.AsString);
      ATestClass.AShortString := 'Foobar';
      CheckEquals('Hello World', AValue.AsString);
      CheckEquals(False, AValue.IsOrdinal);
      CheckEquals(False, AValue.IsObject);
      CheckEquals(False, AValue.IsArray);
      CheckEquals(False, AValue.IsClass);
    finally
      AtestClass.Free;
    end;
    CheckEquals('Hello World',AValue.AsString);
  finally
    c.Free;
  end;
end;

procedure TTestRTTI.TestPropGetValueInteger;
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

procedure TTestRTTI.TestPropGetValueString;
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
      Check(AValue.IsEmpty=false);
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

procedure TTestRTTI.TestPropGetValueProcBoolean;
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
      AProperty := ARttiType.GetProperty('aGetBoolean');
      AValue := AProperty.GetValue(ATestClass);
      CheckEquals(true,AValue.AsBoolean);
    finally
      AtestClass.Free;
    end;
      CheckEquals(True,AValue.AsBoolean);
  finally
    c.Free;
  end;
end;

procedure TTestRTTI.TestPropGetValueProcShortString;
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
    ATestClass.AShortString := 'Hello World';
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);
      Check(assigned(ARttiType));
      AProperty := ARttiType.GetProperty('aGetShortString');
      AValue := AProperty.GetValue(ATestClass);
      CheckEquals('Hello World',AValue.AsString);
    finally
      AtestClass.Free;
    end;
    CheckEquals('Hello World',AValue.AsString);
  finally
    c.Free;
  end;
end;

procedure TTestRTTI.TestPropGetValueObject;
var
  ATestClass : TTestValueClass;
  c: TRttiContext;
  ARttiType: TRttiType;
  AProperty: TRttiProperty;
  AValue: TValue;
  O: TObject;
begin
  c := TRttiContext.Create;
  O := TObject.Create;
  try
    ATestClass := TTestValueClass.Create;
    ATestClass.AObject := O;
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);
      Check(assigned(ARttiType));
      AProperty := ARttiType.GetProperty('AObject');
      AValue := AProperty.GetValue(ATestClass);
      CheckEquals(O.GetHashCode, AValue.AsObject.GetHashCode);
    finally
      AtestClass.Free;
    end;
    CheckEquals(O.GetHashCode, AValue.AsObject.GetHashCode);
  finally
    c.Free;
    O.Free;
  end;
end;

procedure TTestRTTI.TestPropGetValueInterface;
var
  ATestClass : TTestValueClass;
  c: TRttiContext;
  ARttiType: TRttiType;
  AProperty: TRttiProperty;
  AValue: TValue;
  i: IInterface;
begin
  c := TRttiContext.Create;
  i := TInterfacedObject.Create;
  try
    ATestClass := TTestValueClass.Create;
    ATestClass.AUnknown := i;
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);
      Check(assigned(ARttiType));
      AProperty := ARttiType.GetProperty('AUnknown');
      AValue := AProperty.GetValue(ATestClass);
      Check(i = AValue.AsInterface);
    finally
      AtestClass.Free;
    end;
    Check(i = AValue.AsInterface);
  finally
    c.Free;
  end;
end;

procedure TTestRTTI.TestPropGetValueFloat;
var
  ATestClass : TTestValueClass;
  c: TRttiContext;
  ARttiType: TRttiType;
  AProperty: TRttiProperty;
  AValueS, AValueD, AValueE, AValueC, AValueCm: TValue;
begin
  c := TRttiContext.Create;
  try
    ATestClass := TTestValueClass.Create;
    ATestClass.ASingle := 1.1;
    ATestClass.ADouble := 2.2;
    ATestClass.AExtended := 3.3;
    ATestClass.ACurrency := 4;
    ATestClass.AComp := 5;
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);
      Check(assigned(ARttiType));

      AProperty := ARttiType.GetProperty('ASingle');
      AValueS := AProperty.GetValue(ATestClass);
      CheckEquals(1.1, AValueS.AsExtended, 0.001);

      AProperty := ARttiType.GetProperty('ADouble');
      AValueD := AProperty.GetValue(ATestClass);
      CheckEquals(2.2, AValueD.AsExtended, 0.001);

      AProperty := ARttiType.GetProperty('AExtended');
      AValueE := AProperty.GetValue(ATestClass);
      CheckEquals(3.3, AValueE.AsExtended, 0.001);

      AProperty := ARttiType.GetProperty('ACurrency');
      AValueC := AProperty.GetValue(ATestClass);
      CheckEquals(4.0, AValueC.AsExtended, 0.001);

      AProperty := ARttiType.GetProperty('AComp');
      AValueCm := AProperty.GetValue(ATestClass);
      CheckEquals(5.0, AValueCm.AsExtended, 0.001);
    finally
      AtestClass.Free;
    end;

    CheckEquals(1.1, AValueS.AsExtended, 0.001);
    CheckEquals(2.2, AValueD.AsExtended, 0.001);
    CheckEquals(3.3, AValueE.AsExtended, 0.001);
    CheckEquals(4.0, AValueC.AsExtended, 0.001);
    CheckEquals(5.0, AValueCm.AsExtended, 0.001);
  finally
    c.Free;
  end;
end;

procedure TTestRTTI.TestPropGetValueDynArray;
var
  ATestClass : TTestValueClass;
  c: TRttiContext;
  ARttiType: TRttiType;
  AProperty: TRttiProperty;
  AValue: TValue;
  A: TTestDynArray;
begin
  c := TRttiContext.Create;
  A := [1, 2, 3, 4];
  try
    ATestClass := TTestValueClass.Create;
    ATestClass.AArray := A;
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);
      Check(assigned(ARttiType));
      AProperty := ARttiType.GetProperty('AArray');
      AValue := AProperty.GetValue(ATestClass);

      CheckEquals(A[0], AValue.GetArrayElement(0).AsInteger);
      CheckEquals(A[1], AValue.GetArrayElement(1).AsInteger);
      CheckEquals(A[2], AValue.GetArrayElement(2).AsInteger);
      CheckEquals(A[3], AValue.GetArrayElement(3).AsInteger);
    finally
      AtestClass.Free;
    end;
  finally
    c.Free;
  end;
end;

procedure TTestRTTI.TestPropGetValueEnumeration;
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
    ATestClass.AEnumeration := en3;
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);
      Check(assigned(ARttiType));
      AProperty := ARttiType.GetProperty('AEnumeration');
      AValue := AProperty.GetValue(ATestClass);
      CheckEquals(Ord(en3),AValue.AsOrdinal);
      ATestClass.AEnumeration := en1;
      CheckEquals(Ord(en3), AValue.AsOrdinal);
      CheckEquals('en3', AValue.ToString);
      CheckEquals(True, AValue.IsOrdinal);
    finally
      AtestClass.Free;
    end;

    CheckEquals(Ord(en3),AValue.AsOrdinal);
  finally
    c.Free;
  end;
end;

procedure TTestRTTI.TestPropGetValueChars;
var
  ATestClass : TTestValueClass;
  c: TRttiContext;
  ARttiType: TRttiType;
  AProperty: TRttiProperty;
  AValueC, AValueW: TValue;
begin
  c := TRttiContext.Create;
  try
    ATestClass := TTestValueClass.Create;
    ATestClass.AChar := 'C';
    ATestClass.AWideChar := 'W';
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);
      Check(assigned(ARttiType));

      AProperty := ARttiType.GetProperty('AChar');
      AValueC := AProperty.GetValue(ATestClass);
      CheckEquals('C',AValueC.AsAnsiChar);
      ATestClass.AChar := 'N';
      CheckEquals('C', AValueC.AsAnsiChar);
      CheckEquals('C', AValueC.ToString);
      CheckEquals(True, AValueC.IsOrdinal);

      AProperty := ARttiType.GetProperty('AWideChar');
      AValueW := AProperty.GetValue(ATestClass);
      CheckEquals('W',AValueW.AsWideChar);
      ATestClass.AWideChar := 'Z';
      CheckEquals('W', AValueW.AsWideChar);
      CheckEquals('W', AValueW.ToString);
      CheckEquals(True, AValueW.IsOrdinal);
    finally
      AtestClass.Free;
    end;

    CheckEquals('C',AValueC.AsAnsiChar);
    CheckEquals('W',AValueW.AsWideChar);
  finally
    c.Free;
  end;
end;

procedure TTestRTTI.TestPropSetValueString;
var
  ATestClass : TTestValueClass;
  c: TRttiContext;
  ARttiType: TRttiType;
  AProperty: TRttiProperty;
  AValue: TValue;
  s: string;
begin
  c := TRttiContext.Create;
  try
    ATestClass := TTestValueClass.Create;
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);
      AProperty := ARttiType.GetProperty('astring');

      s := 'ipse lorem or something like that';
      TValue.Make(@s, TypeInfo(string), AValue);
      AProperty.SetValue(ATestClass, AValue);
      CheckEquals(ATestClass.AString, s);
      s := 'Another string';
      CheckEquals(ATestClass.AString, 'ipse lorem or something like that');
    finally
      AtestClass.Free;
    end;
  finally
    c.Free;
  end;
end;

procedure TTestRTTI.TestPropSetValueInteger;
var
  ATestClass : TTestValueClass;
  c: TRttiContext;
  ARttiType: TRttiType;
  AProperty: TRttiProperty;
  AValue: TValue;
  i: integer;
begin
  c := TRttiContext.Create;
  try
    ATestClass := TTestValueClass.Create;
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);
      AProperty := ARttiType.GetProperty('aInteger');

      i := -43573;
      TValue.Make(@i, TypeInfo(Integer), AValue);
      AProperty.SetValue(ATestClass, AValue);
      CheckEquals(ATestClass.AInteger, i);
      i := 1;
      CheckEquals(ATestClass.AInteger, -43573);
    finally
      AtestClass.Free;
    end;
  finally
    c.Free;
  end;
end;

procedure TTestRTTI.TestPropSetValueBoolean;
var
  ATestClass : TTestValueClass;
  c: TRttiContext;
  ARttiType: TRttiType;
  AProperty: TRttiProperty;
  AValue: TValue;
  b: boolean;
begin
  c := TRttiContext.Create;
  try
    ATestClass := TTestValueClass.Create;
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);
      AProperty := ARttiType.GetProperty('aboolean');

      b := true;
      TValue.Make(@b, TypeInfo(Boolean), AValue);
      AProperty.SetValue(ATestClass, AValue);
      CheckEquals(ATestClass.ABoolean, b);
      b := false;
      CheckEquals(ATestClass.ABoolean, true);
      TValue.Make(@b, TypeInfo(Boolean), AValue);
      AProperty.SetValue(ATestClass, AValue);
      CheckEquals(ATestClass.ABoolean, false);
    finally
      AtestClass.Free;
    end;
  finally
    c.Free;
  end;
end;

procedure TTestRTTI.TestPropSetValueShortString;
var
  ATestClass : TTestValueClass;
  c: TRttiContext;
  ARttiType: TRttiType;
  AProperty: TRttiProperty;
  AValue: TValue;
  s: string;
  ss: ShortString;
begin
  c := TRttiContext.Create;
  try
    ATestClass := TTestValueClass.Create;
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);
      AProperty := ARttiType.GetProperty('aShortString');

      s := 'ipse lorem or something like that';
      TValue.Make(@s, TypeInfo(String), AValue);
      AProperty.SetValue(ATestClass, AValue);
      CheckEquals(ATestClass.AShortString, s);
      s := 'Another string';
      CheckEquals(ATestClass.AShortString, 'ipse lorem or something like that');

      ss := 'Hello World';
      TValue.Make(@ss, TypeInfo(ShortString), AValue);
      AProperty.SetValue(ATestClass, AValue);
      CheckEquals(ATestClass.AShortString, ss);
      ss := 'Foobar';
      CheckEquals(ATestClass.AShortString, 'Hello World');

      AProperty.SetValue(ATestClass, 'Another string');
      CheckEquals(ATestClass.AShortString, 'Another string');
    finally
      AtestClass.Free;
    end;
  finally
    c.Free;
  end;
end;

procedure TTestRTTI.TestPropSetValueObject;
var
  ATestClass : TTestValueClass;
  c: TRttiContext;
  ARttiType: TRttiType;
  AProperty: TRttiProperty;
  AValue: TValue;
  O: TObject;
  TypeInfo: PTypeInfo;
begin
  c := TRttiContext.Create;
  try
    ATestClass := TTestValueClass.Create;
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);
      AProperty := ARttiType.GetProperty('AObject');
      TypeInfo := GetPropInfo(ATestClass, 'AObject')^.PropType{$ifndef fpc}^{$endif};

      O := TPersistent.Create;
      TValue.Make(@O, TypeInfo, AValue);
      AProperty.SetValue(ATestClass, AValue);
      CheckEquals(ATestClass.AObject.GetHashCode, O.GetHashCode);
      O.Free;

      O := TPersistent.Create;
      AProperty.SetValue(ATestClass, O);
      CheckEquals(ATestClass.AObject.GetHashCode, O.GetHashCode);
      O.Free;
    finally
      AtestClass.Free;
    end;
  finally
    c.Free;
  end;
end;

procedure TTestRTTI.TestPropSetValueInterface;
var
  ATestClass : TTestValueClass;
  c: TRttiContext;
  ARttiType: TRttiType;
  AProperty: TRttiProperty;
  AValue: TValue;
  TypeInfo: PTypeInfo;
  i: IInterface;
begin
  c := TRttiContext.Create;
  try
    ATestClass := TTestValueClass.Create;
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);
      AProperty := ARttiType.GetProperty('AUnknown');
      TypeInfo := GetPropInfo(ATestClass, 'AUnknown')^.PropType{$ifndef fpc}^{$endif};

      i := TInterfacedObject.Create;
      TValue.Make(@i, TypeInfo, AValue);
      AProperty.SetValue(ATestClass, AValue);
      Check(ATestClass.AUnknown = i);

    {$ifdef fpc}
      { Delphi does not provide an implicit assignment overload for IUnknown }
      i := TInterfacedObject.Create;
      AProperty.SetValue(ATestClass, i);
      Check(ATestClass.AUnknown = i);
    {$endif}
    finally
      AtestClass.Free;
    end;
  finally
    c.Free;
  end;
end;

procedure TTestRTTI.TestPropSetValueFloat;
var
  ATestClass : TTestValueClass;
  c: TRttiContext;
  ARttiType: TRttiType;
  AProperty: TRttiProperty;
  AValue: TValue;
  TypeInfo: PTypeInfo;
  S: Single;
  D: Double;
  E: Extended;
  Cur: Currency;
  Cmp: Comp;
begin
  c := TRttiContext.Create;
  try
    ATestClass := TTestValueClass.Create;
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);

      AProperty := ARttiType.GetProperty('ASingle');
      TypeInfo := GetPropInfo(ATestClass, 'ASingle')^.PropType{$ifndef fpc}^{$endif};

      S := 1.1;
      TValue.Make(@S, TypeInfo, AValue);
      AProperty.SetValue(ATestClass, AValue);
      CheckEquals(S, ATestClass.ASingle, 0.001);

      S := 1.2;
      AProperty.SetValue(ATestClass, S);
      CheckEquals(S, ATestClass.ASingle, 0.001);

      AProperty := ARttiType.GetProperty('ADouble');
      TypeInfo := GetPropInfo(ATestClass, 'ADouble')^.PropType{$ifndef fpc}^{$endif};

      D := 2.1;
      TValue.Make(@D, TypeInfo, AValue);
      AProperty.SetValue(ATestClass, AValue);
      CheckEquals(D, ATestClass.ADouble, 0.001);

      D := 2.2;
      AProperty.SetValue(ATestClass, D);
      CheckEquals(D, ATestClass.ADouble, 0.001);

      AProperty := ARttiType.GetProperty('AExtended');
      TypeInfo := GetPropInfo(ATestClass, 'AExtended')^.PropType{$ifndef fpc}^{$endif};

      E := 3.1;
      TValue.Make(@E, TypeInfo, AValue);
      AProperty.SetValue(ATestClass, AValue);
      CheckEquals(E, ATestClass.AExtended, 0.001);

      E := 3.2;
      AProperty.SetValue(ATestClass, E);
      CheckEquals(E, ATestClass.AExtended, 0.001);

      AProperty := ARttiType.GetProperty('ACurrency');
      TypeInfo := GetPropInfo(ATestClass, 'ACurrency')^.PropType{$ifndef fpc}^{$endif};

      Cur := 40;
      TValue.Make(@Cur, TypeInfo, AValue);
      AProperty.SetValue(ATestClass, AValue);
      CheckEquals(Cur, ATestClass.ACurrency, 0.001);

      Cur := 41;
      AProperty.SetValue(ATestClass, Cur);
      CheckEquals(Cur, ATestClass.ACurrency, 0.001);

      AProperty := ARttiType.GetProperty('AComp');
      TypeInfo := GetPropInfo(ATestClass, 'AComp')^.PropType{$ifndef fpc}^{$endif};

      Cmp := 50;
      TValue.Make(@Cmp, TypeInfo, AValue);
      AProperty.SetValue(ATestClass, AValue);
      CheckEquals(Cmp, ATestClass.AComp, 0.001);

      Cmp := 51;
      AProperty.SetValue(ATestClass, Cmp);
      CheckEquals(Cmp, ATestClass.AComp, 0.001);
    finally
      AtestClass.Free;
    end;
  finally
    c.Free;
  end;
end;

procedure TTestRTTI.TestPropSetValueDynArray;
var
  ATestClass : TTestValueClass;
  c: TRttiContext;
  ARttiType: TRttiType;
  AProperty: TRttiProperty;
  AValue: TValue;
  A: TTestDynArray;
  TypeInfo: PTypeInfo;
  i: Integer;
begin
  c := TRttiContext.Create;
  try
    ATestClass := TTestValueClass.Create;
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);
      AProperty := ARttiType.GetProperty('AArray');
      TypeInfo := GetPropInfo(ATestClass, 'AArray')^.PropType{$ifndef fpc}^{$endif};

      A := [1, 2, 3, 4, 5];
      TValue.Make(@A, TypeInfo, AValue);
      AProperty.SetValue(ATestClass, AValue);

      for i := 0 to High(A) do
        CheckEquals(A[i], ATestClass.AArray[i]);
    finally
      AtestClass.Free;
    end;
  finally
    c.Free;
  end;
end;

procedure TTestRTTI.TestPropSetValueEnumeration;
var
  ATestClass : TTestValueClass;
  c: TRttiContext;
  ARttiType: TRttiType;
  AProperty: TRttiProperty;
  AValue: TValue;
  E: TTestEnumeration;
begin
  c := TRttiContext.Create;
  try
    ATestClass := TTestValueClass.Create;
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);
      AProperty := ARttiType.GetProperty('AEnumeration');

      E := en2;
      TValue.Make(@E, TypeInfo(TTestEnumeration), AValue);
      AProperty.SetValue(ATestClass, AValue);
      CheckEquals(Ord(E), Ord(ATestClass.AEnumeration));
    finally
      AtestClass.Free;
    end;
  finally
    c.Free;
  end;
end;

procedure TTestRTTI.TestPropSetValueChars;
var
  ATestClass : TTestValueClass;
  c: TRttiContext;
  ARttiType: TRttiType;
  AProperty: TRttiProperty;
  AValueC, AValueW: TValue;
begin
  c := TRttiContext.Create;
  try
    ATestClass := TTestValueClass.Create;
    ATestClass.AChar := 'C';
    ATestClass.AWideChar := 'W';
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);
      Check(assigned(ARttiType));

      AProperty := ARttiType.GetProperty('AChar');
      AValueC := AProperty.GetValue(ATestClass);
      CheckEquals('C', AValueC.AsAnsiChar);

      AProperty := ARttiType.GetProperty('AWideChar');
      AValueW := AProperty.GetValue(ATestClass);
      CheckEquals('W', AValueW.AsWideChar);
    finally
      AtestClass.Free;
    end;
      CheckEquals('C', AValueC.AsAnsiChar);
      CheckEquals('W', AValueW.AsWideChar);
  finally
    c.Free;
  end;
end;

procedure TTestRTTI.TestPropGetValueProcInteger;
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
      AProperty := ARttiType.GetProperty('agetinteger');
      AValue := AProperty.GetValue(ATestClass);
      CheckEquals(472349,AValue.AsInteger);
    finally
      AtestClass.Free;
    end;
      CheckEquals(472349,AValue.AsInteger);
  finally
    c.Free;
  end;
end;

procedure TTestRTTI.TestPropGetValueProcString;
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
    ATestClass.AString := 'Hello World';
    try
      ARttiType := c.GetType(ATestClass.ClassInfo);
      Check(assigned(ARttiType));
      AProperty := ARttiType.GetProperty('agetstring');
      AValue := AProperty.GetValue(ATestClass);
      CheckEquals('Hello World',AValue.AsString);
    finally
      AtestClass.Free;
    end;
    CheckEquals('Hello World',AValue.AsString);
  finally
    c.Free;
  end;
end;


procedure TTestRTTI.TestTRttiTypeProperties;
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
    CheckEquals(ARttiType.TypeSize,SizeOf(TObject));
    CheckEquals(ARttiType.IsManaged,false);
    CheckEquals(ARttiType.BaseType.classname,'TRttiInstanceType');
    CheckEquals(ARttiType.IsInstance,True);
    CheckEquals(ARttiType.AsInstance.DeclaringUnitName,'tests.rtti.types');
    Check(ARttiType.BaseType.Name='TObject');
    Check(ARttiType.AsInstance.BaseType.Name='TObject');
    CheckEquals(ARttiType.IsOrdinal,False);
    CheckEquals(ARttiType.IsRecord,False);
    CheckEquals(ARttiType.IsSet,False);
  finally
    c.Free;
  end;

end;

procedure TTestRTTI.GetTypeInteger;
var
  LContext: TRttiContext;
  LType: TRttiType;
begin
  LContext := TRttiContext.Create;

  LType := LContext.GetType(TypeInfo(integer));
{$ifdef fpc}
  CheckEquals(LType.Name, 'LongInt');
{$else}
  CheckEquals(LType.Name, 'Integer');
{$endif}

  LContext.Free;
end;

procedure TTestRTTI.GetTypePointer;
var
  context: TRttiContext;
  t: TRttiType;
  p: TRttiPointerType absolute t;
begin
  context := TRttiContext.Create;
  try
    t := context.GetType(TypeInfo(Pointer));
    Assert(t is TRttiPointerType, 'Type of Pointer is not a TRttiPointerType');
    Assert(not Assigned(p.ReferredType), 'ReferredType of Pointer is not Nil');
    t := context.GetType(TypeInfo(PLongInt));
    Assert(t is TRttiPointerType, 'Type of Pointer is not a TRttiPointerType');
    Assert(Assigned(p.ReferredType), 'ReferredType of PLongInt is Nil');
    Assert(p.ReferredType = context.GetType(TypeInfo(LongInt)), 'ReferredType of PLongInt is not a LongInt');
    t := context.GetType(TypeInfo(PWideChar));
    Assert(t is TRttiPointerType, 'Type of Pointer is not a TRttiPointerType');
    Assert(Assigned(p.ReferredType), 'ReferredType of PWideChar is Nil');
    Assert(p.ReferredType = context.GetType(TypeInfo(WideChar)), 'ReferredType of PWideChar is not a WideChar');
  finally
    context.Free;
  end;
end;

procedure TTestRTTI.GetClassProperties;
var
  LContext: TRttiContext;
  LType: TRttiType;
  PropList, PropList2: {$ifdef fpc}specialize{$endif} TArray<TRttiProperty>;
  i: LongInt;
begin
  LContext := TRttiContext.Create;

  LType := LContext.GetType(TypeInfo(TGetClassProperties));
  PropList := LType.GetProperties;

  CheckEquals(4, length(PropList));
  CheckEquals('PubPropRO', PropList[0].Name);
  CheckEquals('PubPropRW', PropList[1].Name);
  CheckEquals('PubPropSetRO', PropList[2].Name);
  CheckEquals('PubPropSetRW', PropList[3].Name);

  LType := LContext.GetType(TypeInfo(TGetClassPropertiesSub));
  PropList2 := LType.GetProperties;

  CheckEquals(Length(PropList), Length(PropList2));
  for i := 0 to High(PropList) do
    Check(PropList[i] = PropList2[i], 'Property instances are not equal');

  LContext.Free;
end;

procedure TTestRTTI.GetClassPropertiesValue;
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


procedure TTestRTTI.TestInterface;
var
  context: TRttiContext;
  t: TRttiType;
  ti1, ti2: TRttiInterfaceType;
  methods: {$ifdef fpc}specialize{$endif} TArray<TRttiMethod>;
  params: {$ifdef fpc}specialize{$endif} TArray<TRttiParameter>;
  method: TRttiMethod;
  param: TRttiParameter;
  flag: TParamFlag;
begin
  context := TRttiContext.Create;
  try
    t := context.GetType(TypeInfo(IInterface));
    Check(t is TRttiInterfaceType, 'Type is not an interface type');

    Check(not Assigned(t.BaseType), 'Base type is assigned');

    ti1 := TRttiInterfaceType(t);
    Check(not Assigned(ti1.BaseType), 'Base type is assigned');

    methods := t.GetMethods;
    CheckEquals(0, Length(methods), 'Overall method count does not match');

    methods := t.GetDeclaredMethods;
    CheckEquals(0, Length(methods), 'Declared method conut does not match');

    t := context.GetType(TypeInfo(ITestInterface));
    Check(t is TRttiInterfaceType, 'Type is not an interface type');

    Check(Assigned(t.BaseType), 'Base type is not assigned');
    Check(t.BaseType = TRttiType(ti1), 'Base type does not match');

    ti2 := TRttiInterfaceType(t);
    Check(Assigned(ti2.BaseType), 'Base type is not assigned');
    Check(ti2.BaseType = ti1, 'Base type does not match');

    methods := t.GetMethods;
    CheckEquals(4, Length(methods), 'Overall method count does not match');

    methods := t.GetDeclaredMethods;
    CheckEquals(4, Length(methods), 'Declared method count does not match');

    method := methods[0];
    CheckEquals(method.Name, 'Test', 'Method name of Test does not match');
    Check(method.CallingConvention = DefaultCC, 'Calling convention of Test does not match');
    Check(method.MethodKind = mkProcedure, 'Method kind of Test does not match');
    Check(method.DispatchKind = dkInterface, 'Dispatch kind of Test does not match');
    Check(not Assigned(method.CodeAddress), 'Code address of Test is not Nil');
    CheckEquals(method.VirtualIndex, 3, 'Virtual index of Test does not match');
    Check(not Assigned(method.ReturnType), 'Return type of Test is not Nil');
    params := method.GetParameters;
    CheckEquals(0, Length(params), 'Parameter count of Test does not match');

    method := methods[1];
    CheckEquals(method.Name, 'Test2', 'Method name of Test2 does not match');
    Check(method.CallingConvention = DefaultCC, 'Calling convention of Test2 does not match');
    Check(method.MethodKind = mkFunction, 'Method kind of Test2 does not match');
    Check(method.DispatchKind = dkInterface, 'Dispatch kind of Test2 does not match');
    Check(not Assigned(method.CodeAddress), 'Code address of Test2 is not Nil');
    CheckEquals(method.VirtualIndex, 4, 'Virtual index of Test2 does not match');
    Check(Assigned(method.ReturnType), 'Return type of Test2 is Nil');
    Check(method.ReturnType.TypeKind = tkInteger, 'Return type of Test2 is not an ordinal');
    params := method.GetParameters;
    CheckEquals(0, Length(params), 'Parameter count of Test2 does not match');

    method := methods[2];
    CheckEquals(method.Name, 'Test3', 'Method name of Test3 does not match');
    Check(method.CallingConvention = DefaultCC, 'Calling convention of Test3 does not match');
    Check(method.MethodKind = mkProcedure, 'Method kind of Test3 does not match');
    Check(method.DispatchKind = dkInterface, 'Dispatch kind of Test3 does not match');
    Check(not Assigned(method.CodeAddress), 'Code address of Test3 is not Nil');
    CheckEquals(method.VirtualIndex, 5, 'Virtual index of Test3 does not match');
    Check(not Assigned(method.ReturnType), 'Return type of Test3 is not Nil');

    params := method.GetParameters;
    CheckEquals(4, Length(params), 'Parameter count of Test3 does not match');

    param := params[0];
    CheckEquals(param.Name, 'aArg1', 'Parameter name of Test3.aArg1 does not match');
    Check(param.Flags = [], 'Parameter flags of Test3.aArg1 do not match');
    Check(Assigned(param.ParamType), 'Parameter type of Test3.aArg1 is Nil');
    Check(param.ParamType.TypeKind = tkInteger, 'Parameter type of Test3.aArg1 is not an ordinal');

    param := params[1];
    CheckEquals(param.Name, 'aArg2', 'Parameter name of Test3.aArg2 does not match');
    Check(param.Flags = [pfConst], 'Parameter flags of Test3.aArg2 do not match');
    Check(Assigned(param.ParamType), 'Parameter type of Test3.aArg2 is Nil');
    Check(param.ParamType.TypeKind = tkAnsiString, 'Parameter type of Test3.aArg2 is not a string');

    param := params[2];
    CheckEquals(param.Name, 'aArg3', 'Parameter name of Test3.aArg3 does not match');
    Check(param.Flags = [pfVar], 'Parameter flags of Test3.aArg3 do not match');
    Check(Assigned(param.ParamType), 'Parameter type of Test3.aArg3 is Nil');
    Check(param.ParamType.TypeKind = {$ifdef fpc}tkBool{$else}tkEnumeration{$endif}, 'Parameter type of Test3.aArg3 is not a boolean');

    param := params[3];
    CheckEquals(param.Name, 'aArg4', 'Parameter name of Test3.aArg4 does not match');
    Check(param.Flags = [pfOut], 'Parameter flags of Test3.aArg4 do not match');
    Check(Assigned(param.ParamType), 'Parameter type of Test3.aArg4 is Nil');
    Check(param.ParamType.TypeKind = tkInteger, 'Parameter type of Test3.aArg4 is not a string');

    method := methods[3];
    CheckEquals(method.Name, 'Test4', 'Method name of Test4 does not match');
    Check(method.CallingConvention = DefaultCC, 'Calling convention of Test4 does not match');
    Check(method.MethodKind = mkFunction, 'Method kind of Test4 does not match');
    Check(method.DispatchKind = dkInterface, 'Dispatch kind of Test4 does not match');
    Check(not Assigned(method.CodeAddress), 'Code address of Test4 is not Nil');
    CheckEquals(method.VirtualIndex, 6, 'Virtual index of Test4 does not match');
    Check(Assigned(method.ReturnType), 'Return type of Test4 is not Nil');
    Check(method.ReturnType.TypeKind = tkAnsiString, 'Return type of Test4 is not a string');

    params := method.GetParameters;
    CheckEquals(2, Length(params), 'Parameter count of Test4 does not match');

    param := params[0];
    CheckEquals(param.Name, 'aArg1', 'Parameter name of Test4.aArg1 does not match');
    Check(param.Flags = [pfArray, pfReference], 'Parameter flags of Test4.aArg1 do not match');
    Check(Assigned(param.ParamType), 'Parameter type of Test4.aArg1 is Nil');
    Check(param.ParamType.TypeKind = tkInteger, 'Parameter type of Test4.aArg1 is not an ordinal');

    param := params[1];
    CheckEquals(param.Name, 'aArg2', 'Parameter name of Test4.aArg2 does not match');
    Check(param.Flags = [pfArray, pfReference], 'Parameter flags of Test4.aArg2 do not match');
    Check(Assigned(param.ParamType), 'Parameter type of Test4.aArg2 is Nil');
    Check(param.ParamType.TypeKind = tkRecord, 'Parameter type of Test4.aArg2 is not a record');
  finally
    context.Free;
  end;
end;

procedure TTestRTTI.TestRawThunk;
var
  intf: IInterface;
begin
  { we test the raw thunking by instantiating a TVirtualInterface of IInterface }
  { this does not require a function call manager as the thunking is implemented
    directly inside the RTTI unit }
  try
    intf := TVirtualInterface.Create(PTypeInfo(TypeInfo(IInterface))) as IInterface;
  except
    on e: ENotImplemented do
      Ignore('RawThunk not implemented');
  end;
  { if all went well QueryInterface and _AddRef were called and now we call
    _Release as well }
  intf := Nil;
end;

{$ifdef fpc}
procedure TTestRTTI.TestInterfaceRaw;
var
  context: TRttiContext;
  t: TRttiType;
  ti: TRttiInterfaceType;
begin
  context := TRttiContext.Create;
  try
    t := context.GetType(TypeInfo(ICORBATest));
    Check(t is TRttiInterfaceType, 'Type is not a raw interface type');

    Check(not Assigned(t.BaseType), 'Base type is assigned');

    ti := TRttiInterfaceType(t);
    Check(not Assigned(ti.BaseType), 'Base type is assigned');
  finally
    context.Free;
  end;
end;
{$endif}

procedure TTestRTTI.TestArray;
var
  context: TRttiContext;
  t, el: TRttiType;
  a: TRttiArrayType;
  o: TRttiOrdinalType;
begin
  context := TRttiContext.Create;
  try
    t := context.GetType(PTypeInfo(TypeInfo(TArrayOfLongintStatic)));
    Check(t is TRttiArrayType, 'Type is not a TRttiArrayType');

    a := TRttiArrayType(t);
    CheckEquals(1, a.DimensionCount, 'Dimension count does not match');
    CheckEquals(4, a.TotalElementCount, 'Total element count does not match');

    el := a.ElementType;
    Check(el is TRttiOrdinalType, 'Element type is not a TRttiOrdinalType');
    Check(el = context.GetType(PTypeInfo(TypeInfo(LongInt))), 'Element type is not a LongInt');

    t := a.Dimensions[0];
    {$ifdef fpc}
    Check(t is TRttiOrdinalType, 'Index type is not a TRttiOrdinalType');

    o := TRttiOrdinalType(t);
    { Currently this is a full type :/ }
    {CheckEquals(0, o.MinValue, 'Minimum value of 1st dimension does not match');
    CheckEquals(3, o.MaxValue, 'Maximum value of 1st dimension does not match');}
    {$else}
    Check(t = Nil, 'Index type is not Nil');
    {$endif}

    t := context.GetType(PTypeInfo(TypeInfo(TArrayOfLongint2DStatic)));
    Check(t is TRttiArrayType, 'Type is not a TRttiArrayType');

    a := TRttiArrayType(t);
    CheckEquals(2, a.DimensionCount, 'Dimension count does not match');
    CheckEquals(4 * 3, a.TotalElementCount, 'Total element count does not match');

    el := a.ElementType;
    Check(el is TRttiOrdinalType, 'Element type is not a TRttiOrdinalType');
    Check(el = context.GetType(PTypeInfo(TypeInfo(LongInt))), 'Element type is not a LongInt');

    t := a.Dimensions[0];
    {$ifdef fpc}
    Check(t is TRttiOrdinalType, 'Index type is not a TRttiOrdinalType');

    o := TRttiOrdinalType(t);
    { Currently this is a full type :/ }
    {CheckEquals(0, o.MinValue, 'Minimum value of 1st dimension does not match');
    CheckEquals(3, o.MaxValue, 'Maximum value of 1st dimension does not match');}
    {$else}
    Check(t = Nil, 'Index type is not Nil');
    {$endif}

    t := a.Dimensions[1];
    {$ifdef fpc}
    Check(t is TRttiOrdinalType, 'Index type is not a TRttiOrdinalType');

    o := TRttiOrdinalType(t);
    { Currently this is a full type :/ }
    {CheckEquals(2, o.MinValue, 'Minimum value of 1st dimension does not match');
    CheckEquals(4, o.MaxValue, 'Maximum value of 1st dimension does not match');}
    {$else}
    Check(t = Nil, 'Index type is not Nil');
    {$endif}
  finally
    context.Free;
  end;
end;

procedure TTestRTTI.TestDynArray;
var
  context: TRttiContext;
  t, el: TRttiType;
  a: TRttiDynamicArrayType;
begin
  context := TRttiContext.Create;
  try
    t := context.GetType(PTypeInfo(TypeInfo(TArrayOfLongintDyn)));
    Check(t is TRttiDynamicArrayType, 'Type is not a TRttiDynamicArrayType');

    a := TRttiDynamicArrayType(t);

    CheckEquals('tests.rtti.types', LowerCase(a.DeclaringUnitName), 'Unit type does not match for dynamic array');
    CheckEquals(a.ElementSize, SizeUInt(SizeOf(LongInt)), 'Element size does not match for dynamic array');

    el := a.ElementType;
    Check(el is TRttiOrdinalType, 'Element type is not a TRttiOrdinalType');

    Check(el = context.GetType(PTypeInfo(TypeInfo(LongInt))), 'Element type is not a LongInt');

    { ToDo: check OLE type }
  finally
    context.Free;
  end;
end;

procedure TTestRTTI.TestProcVar;
var
  context: TRttiContext;
  t: TRttiType;
  p: TRttiProcedureType;
  params: {$ifdef fpc}specialize{$endif} TArray<TRttiParameter>;
begin
  context := TRttiContext.Create;
  try
    t := context.GetType(PTypeInfo(TypeInfo(TTestProc)));
    Check(Assigned(t), 'Rtti Type is Nil');
    Check(t is TRttiInvokableType, 'Rtti Type is not an invokeable');
    Check(t is TRttiProcedureType, 'Rtti Type is not a procedure type');

    p := t as TRttiProcedureType;
    Check(p.CallingConvention = DefaultCC, 'Calling convention does not match');
    Check(not Assigned(p.ReturnType), 'Return type is assigned');
    CheckEquals(0, Length(p.GetParameters), 'Procedure variable has parameters');

    t := context.GetType(PTypeInfo(TypeInfo(TTestFunc1)));
    Check(Assigned(t), 'Rtti Type is Nil');
    Check(t is TRttiInvokableType, 'Rtti Type is not an invokeable');
    Check(t is TRttiProcedureType, 'Rtti Type is not a procedure type');

    p := t as TRttiProcedureType;
    Check(p.CallingConvention = DefaultCC, 'Calling convention does not match');
    Check(Assigned(p.ReturnType), 'Return type is not assigned');
    //Check(p.ReturnType is TRttiOrdinalType, 'Return type is not an ordinal type');
    CheckEquals(0, Length(p.GetParameters), 'Procedure variable has parameters');

    t := context.GetType(PTypeInfo(TypeInfo(TTestFunc2)));
    Check(Assigned(t), 'Rtti Type is Nil');
    Check(t is TRttiInvokableType, 'Rtti Type is not an invokeable');
    Check(t is TRttiProcedureType, 'Rtti Type is not a procedure type');

    p := t as TRttiProcedureType;
    Check(p.CallingConvention = DefaultCC, 'Calling convention does not match');
    Check(Assigned(p.ReturnType), 'Return type is not assigned');
    Check(p.ReturnType is TRttiStringType, 'Return type is not a string type');

    params := p.GetParameters;
    CheckEquals(2, Length(params), 'Procedure variable has incorrect amount of parameters');

    Check(params[0].ParamType.TypeKind in [tkInteger, tkInt64], 'Parameter 1 is not an ordinal type');
    //Check(params[0].ParamType is TRttiOrdinalType, 'Parameter 1 is not an ordinal type');
    Check(pfArray in params[1].Flags, 'Parameter 2 is not an array');
    Check(params[1].ParamType.TypeKind in [tkInteger, tkInt64], 'Parameter 2 is not an ordinal array');
  finally
    context.Free;
  end;
end;

procedure TTestRTTI.TestMethod;
var
  context: TRttiContext;
  t: TRttiType;
  m: TRttiMethodType;
  params: {$ifdef fpc}specialize{$endif} TArray<TRttiParameter>;
begin
  context := TRttiContext.Create;
  try
    t := context.GetType(PTypeInfo(TypeInfo(TTestMethod)));
    Check(Assigned(t), 'Rtti Type is Nil');
    Check(t is TRttiInvokableType, 'Rtti Type is not an invokeable');
    Check(t is TRttiMethodType, 'Rtti Type is not a method type');

    m := t as TRttiMethodType;
    Check(m.CallingConvention = DefaultCC, 'Calling convention does not match');
    Check(not Assigned(m.ReturnType), 'Return type is assigned');
    CheckEquals(0, Length(m.GetParameters), 'Method variable has parameters');

    t := context.GetType(PTypeInfo(TypeInfo(TTestMethod1)));
    Check(Assigned(t), 'Rtti Type is Nil');
    Check(t is TRttiInvokableType, 'Rtti Type is not an invokeable');
    Check(t is TRttiMethodType, 'Rtti Type is not a method type');

    m := t as TRttiMethodType;
    Check(m.CallingConvention = DefaultCC, 'Calling convention does not match');
    Check(Assigned(m.ReturnType), 'Return type is not assigned');
    //Check(p.ReturnType is TRttiOrdinalType, 'Return type is not an ordinal type');
    CheckEquals(0, Length(m.GetParameters), 'Method variable has parameters');

    t := context.GetType(PTypeInfo(TypeInfo(TTestMethod2)));
    Check(Assigned(t), 'Rtti Type is Nil');
    Check(t is TRttiInvokableType, 'Rtti Type is not an invokeable');
    Check(t is TRttiMethodType, 'Rtti Type is not a method type');

    m := t as TRttiMethodType;
    Check(m.CallingConvention = DefaultCC, 'Calling convention does not match');
    Check(Assigned(m.ReturnType), 'Return type is not assigned');
    Check(m.ReturnType is TRttiStringType, 'Return type is not a string type');

    params := m.GetParameters;
    CheckEquals(2, Length(params), 'Method variable has incorrect amount of parameters');

    Check(params[0].ParamType.TypeKind in [tkInteger, tkInt64], 'Parameter 1 is not an ordinal type');
    //Check(params[0].ParamType is TRttiOrdinalType, 'Parameter 1 is not an ordinal type');
    Check(pfArray in params[1].Flags, 'Parameter 2 is not an array');
    Check(params[1].ParamType.TypeKind in [tkInteger, tkInt64], 'Parameter 2 is not an ordinal array');
  finally
    context.Free;
  end;
end;

{ TTestExtendedRTTI }

procedure TTestExtendedRTTI.AssertEquals(Msg: String; aExpected, aActual: TMemberVisibility);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TMemberVisibility),Ord(aExpected)),
                   GetEnumName(TypeInfo(TMemberVisibility),Ord(aActual)));
end;

procedure TTestExtendedRTTI.AssertEquals(Msg: String; aExpected, aActual: TTypeKind);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TTypeKind),Ord(aExpected)),
                   GetEnumName(TypeInfo(TTypeKind),Ord(aActual)));
end;


procedure TTestExtendedRTTI.Setup;

begin
  Inherited;
  FCtx:=TRttiContext.Create;
  FCtx.UsePublishedOnly:=False;
end;

procedure TTestExtendedRTTI.TearDown;
begin
  FCtx.Free;
  inherited TearDown;
end;

Procedure TTestExtendedRTTI.CheckField(aIdx : Integer; aData: TRttiField; aName : String; aKind : TTypeKind; aVisibility : TMemberVisibility; aStrict : Boolean = False);

Var
  Msg : String;

begin
  Msg:='Checking field '+IntToStr(aIdx)+' ('+aName+') ';
  AssertNotNull(Msg+'Have data',AData);
  AssertEquals(Msg+'name',aName,aData.Name);
  AssertEquals(Msg+'kind',aKind,aData.FieldType.TypeKind);
  AssertEquals(Msg+'visibility',aVisibility,aData.Visibility);
  AssertEquals(Msg+'strict',aStrict,aData.StrictVisibility);
end;

Procedure TTestExtendedRTTI.CheckProperty(aIdx : Integer; aData: TRttiProperty; aName : String; aKind : TTypeKind; aVisibility : TMemberVisibility; isStrict : Boolean = False);

Var
  Msg : String;

begin
  Msg:='Checking prop '+IntToStr(aIdx)+' ('+aName+') ';
  AssertNotNull(Msg+'Have data',AData);
  AssertEquals(Msg+'name',aName, aData.Name);
  AssertEquals(Msg+'kind',aKind, aData.PropertyType.TypeKind);
  AssertEquals(Msg+'visibility',aVisibility,aData.Visibility);
  AssertEquals(Msg+'strict',isStrict,aData.StrictVisibility);
end;

Procedure TTestExtendedRTTI.CheckMethod(aPrefix : string; aIdx : Integer; aData: TRttiMethod; aName : String; aVisibility : TMemberVisibility; aStrict : Boolean = False);

Var
  Msg : String;

begin
  Msg:=aPrefix+': Checking method '+IntToStr(aIdx)+' ('+aName+') ';
  AssertNotNull(Msg+'Have data',AData);
  AssertEquals(Msg+'name',aData.Name,aName);
  AssertEquals(Msg+'visibility',aVisibility,aData.Visibility);
  AssertEquals(Msg+'strict',aData.StrictVisibility,aStrict);
end;

procedure TTestClassExtendedRTTI.TestFields;

Var
  Obj : TRttiObject;
  RttiData : TRttiInstanceType absolute obj;
  A : TRttiFieldArray;
  t : TFieldRTTI;

begin
  Obj:=FCtx.GetType(TFieldRTTI.ClassInfo);
  AssertEquals('Correct class type',TRttiInstanceType,Obj.ClassType);
  A:=RttiData.GetFields;
  AssertEquals('Class field Count',10,Length(A));
  CheckField(0, A[0],'FPrivateA',tkInteger,mvPrivate);
  CheckField(1, A[1],'FPrivateB',tkInteger,mvPrivate,True);
  CheckField(2, A[2],'FProtectedA',tkInteger,mvProtected);
  CheckField(3, A[3],'FProtectedB',tkInteger,mvProtected,True);
  CheckField(4, A[4],'FPublicA',tkInteger,mvPublic);
  CheckField(5, A[5],'FPublicB',tkInteger,mvPublic);
  CheckField(6, A[6],'FPublishedA',tkInteger,mvPrivate);
  CheckField(7, A[7],'FPublishedB',tkInteger,mvPrivate);
  CheckField(8, A[8],'FPublishedC',tkClass,mvPublished);
  CheckField(9, A[9],'FPublishedD',tkClass,mvPublished);

  t := TFieldRTTI.Create;
  AssertEquals('Legacy Field 0', A[8].Offset, Integer(PByte(t.FieldAddress('FPublishedC')) - PByte(t)));
  AssertEquals('Legacy Field 1', A[9].Offset, Integer(PByte(t.FieldAddress('FPublishedD')) - PByte(t)));
  T.Free;
end;

procedure TTestClassExtendedRTTI.TestProperties;

Var
  A : TRttiPropertyArray;
  Obj : TRttiObject;
  RttiData : TRttiInstanceType absolute obj;
  aCount : Integer;


begin
  Obj:=FCtx.GetType(TFieldRTTI.ClassInfo);
  AssertEquals('Correct class type',TRttiInstanceType,Obj.ClassType);
  A:=RttiData.GetProperties;
  aCount:=Length(A);
  AssertEquals('Property Count',8,aCount);
  CheckProperty(0, A[0],'PrivateA',tkInteger,mvPrivate);
  CheckProperty(1, A[1],'PrivateB',tkInteger,mvPrivate,True);
  CheckProperty(2, A[2],'ProtectedA',tkInteger,mvProtected);
  CheckProperty(3, A[3],'ProtectedB',tkInteger,mvProtected,True);
  CheckProperty(4, A[4],'PublicA',tkInteger,mvPublic);
  CheckProperty(5, A[5],'PublicB',tkInteger,mvPublic);
  CheckProperty(6, A[6],'PublishedA',tkInteger,mvPublished);
  CheckProperty(7, A[7],'PublishedB',tkInteger,mvPublished);
end;

procedure TTestClassExtendedRTTI.TestDeclaredMethods;

Var
  A : TRttiMethodArray;
  Obj : TRttiObject;
  RttiData : TRttiInstanceType absolute obj;
  Parms : TRttiParameterArray;
  aCount : Integer;

begin
  Obj:=FCtx.GetType(TMethodClassRTTI.ClassInfo);
  AssertEquals('Correct class type',TRttiInstanceType,Obj.ClassType);
  A:=RttiData.GetDeclaredMethods;
  aCount:=Length(A);
  AssertEquals('Full Count',12,aCount);
  CheckMethod('Full',0, A[0],'PrivateMethodA',mvPrivate);
  CheckMethod('Full',1, A[1],'PrivateMethodB',mvPrivate,True);
  CheckMethod('Full',2, A[2],'PrivateMethodC',mvPrivate);
  CheckMethod('Full',3, A[3],'ProtectedMethodA',mvProtected);
  CheckMethod('Full',4, A[4],'ProtectedMethodB',mvProtected,True);
  CheckMethod('Full',5, A[5],'ProtectedMethodC',mvProtected);
  CheckMethod('Full',6, A[6],'PublicMethodA',mvPublic);
  CheckMethod('Full',7, A[7],'PublicMethodB',mvPublic);
  CheckMethod('Full',8, A[8],'PublicMethodC',mvPublic);
  CheckMethod('Full',9, A[9],'PublishedMethodA',mvPublished);
  CheckMethod('Full',10, A[10],'PublishedMethodB',mvPublished);
  CheckMethod('Full',11, A[11],'PublishedMethodC',mvPublished);
  Parms:=A[9].GetParameters;
  AssertEquals('Parameter length',1,Length(Parms));
  AssertEquals('Parameter name','a',Parms[0].Name);

end;

procedure TTestClassExtendedRTTI.TestMethods;
Var
  A : TRttiMethodArray;
  Obj : TRttiObject;
  RttiData : TRttiInstanceType absolute obj;
  aCount : Integer;

begin
  Obj:=FCtx.GetType(TAdditionalMethodClassRTTI.ClassInfo);
  AssertEquals('Correct class type',TRttiInstanceType,Obj.ClassType);
  A:=RttiData.GetMethods;
  aCount:=Length(A);
  AssertEquals('Full Count',13,aCount);
  CheckMethod('Full',12, A[12],'PublicAdditionalMethod',mvPublic);

end;

procedure TTestClassExtendedRTTI.TestPrivateFieldAttributes;

var
  Obj : TRttiObject;
  RttiData : TRttiInstanceType absolute obj;
  Attrs : TCustomAttributeArray;
  Fld : TRttiField;
  O : TCustomAttribute;
  M2 : My2Attribute absolute O;

begin
  Obj:=FCtx.GetType(TypeInfo(TFieldObject));
  AssertEquals('Correct class type',TRttiInstanceType,Obj.ClassType);
  Fld:=RttiData.GetField('PrivateField');
  AssertNotNull('Have field',Fld);
  Attrs:=Fld.GetAttributes;
  AssertNotNull('Have attribute data',Pointer(Attrs));
  AssertEquals('attribute count',3,Length(Attrs));
  AssertEquals('Attribute 1 name','WeakAttribute',Attrs[0].ClassName);
  AssertEquals('Attribute 2 name','MyAttribute',Attrs[1].ClassName);
  AssertEquals('Attribute 2 name','My2Attribute',Attrs[2].ClassName);
  O:=Attrs[2];
  AssertNotNull('Attribute class ',O);
  AssertEquals('Attribute class ',O.ClassType,My2Attribute);
  AssertEquals('Attribute value ',2,M2.Int);
end;

procedure TTestClassExtendedRTTI.TestProtectedFieldAttributes;

var
  Obj : TRttiObject;
  RttiData : TRttiInstanceType absolute obj;
  Attrs : TCustomAttributeArray;
  Fld : TRttiField;
  O : TCustomAttribute;
  M2 : My2Attribute absolute O;

begin
  Obj:=FCtx.GetType(TypeInfo(TFieldObject));
  AssertEquals('Correct class type',TRttiInstanceType,Obj.ClassType);
  Fld:=RttiData.GetField('ProtectedField');
  AssertNotNull('Have field',Fld);
  Attrs:=Fld.GetAttributes;
  AssertNotNull('Have attribute data',Pointer(Attrs));
  AssertEquals('attribute count',1,Length(Attrs));
  AssertEquals('Attribute 1 name','My2Attribute',Attrs[0].ClassName);
  O:=Attrs[0];
  AssertNotNull('Attribute class ',O);
  AssertEquals('Attribute class ',O.ClassType,My2Attribute);
  AssertEquals('Attribute value ',3,M2.Int);
end;

Procedure TTestClassExtendedRTTI.TestPublicFieldAttributes;

var
  Obj : TRttiObject;
  RttiData : TRttiInstanceType absolute obj;
  Attrs : TCustomAttributeArray;
  Fld : TRttiField;
  O : TCustomAttribute;
  M3 : My3Attribute absolute O;
  aCount : Integer;

begin
  Obj:=FCtx.GetType(TypeInfo(TFieldObject));
  AssertEquals('Correct class type',TRttiInstanceType,Obj.ClassType);
  aCount:=0;
  For Fld in RttiData.GetFields do
    if Fld.Visibility=mvPublic then
      inc(aCount);
  AssertEquals('Field count',3,aCount);
  // PublicField
  Fld:=RttiData.GetField('PublicField');
  AssertNotNull('Have field',Fld);
  Attrs:=Fld.GetAttributes;
  AssertNotNull('Have attribute data',Pointer(Attrs));
  AssertEquals('attribute count',1,Length(Attrs));
  AssertEquals('Attribute 1 name','My3Attribute',Attrs[0].ClassName);
  O:=Attrs[0];
  AssertNotNull('Attribute class ',O);
  AssertEquals('Attribute class ',O.ClassType,My3Attribute);
  AssertEquals('Attribute value ',4,M3.Int);
  // A
  Fld:=RttiData.GetField('A');
  AssertNotNull('A Have field',Fld);
  Attrs:=Fld.GetAttributes;
  AssertNotNull('A Have attribute data',Pointer(Attrs));
  AssertEquals('A Attribute count',1,Length(Attrs));
  AssertEquals('A Attribute 1 name','My3Attribute',Attrs[0].ClassName);
  O:=Attrs[0];
  AssertNotNull('A: Attribute class ',O);
  AssertEquals('A: Attribute class ',O.ClassType,My3Attribute);
  AssertEquals('A: Attribute value ',4,M3.Int);
  // B
  Fld:=RttiData.GetField('B');
  AssertNotNull('B Have field',Fld);
  Attrs:=Fld.GetAttributes;
  AssertNotNull('B Have attribute data',Pointer(Attrs));
  AssertEquals('A Attribute count',1,Length(Attrs));
  AssertEquals('A Attribute 1 name','My3Attribute',Attrs[0].ClassName);
  O:=Attrs[0];
  AssertNotNull('B: Attribute class ',O);
  AssertEquals('B: Attribute class ',O.ClassType,My3Attribute);
  AssertEquals('B: Attribute value ',4,M3.Int);
end;

Procedure TTestClassExtendedRTTI.TestPrivatePropertyAttributes;

var
  Obj : TRttiObject;
  RttiData : TRttiInstanceType absolute obj;
  Attrs : TCustomAttributeArray;
  Prop : TRttiProperty;
  O : TCustomAttribute;
  aCount : Integer;
  M2 : My2Attribute absolute O;

begin
  Obj:=FCtx.GetType(TypeInfo(TPropertyObject));
  AssertEquals('Correct class type',TRttiInstanceType,Obj.ClassType);
  aCount:=0;
  Prop:=RttiData.GetProperty('PrivateProperty');
  AssertNotNull('Have property',Prop);
  Attrs:=Prop.GetAttributes;
  AssertNotNull('Have attribute data',Pointer(Attrs));
  AssertEquals('attribute count',3,Length(Attrs));
  AssertEquals('Attribute 1 name','WeakAttribute',Attrs[0].ClassName);
  AssertEquals('Attribute 2 name','MyAttribute',Attrs[1].ClassName);
  AssertEquals('Attribute 2 name','My2Attribute',Attrs[2].ClassName);
  O:=Attrs[2];
  AssertNotNull('Attribute class ',O);
  AssertEquals('Attribute class ',O.ClassType,My2Attribute);
  AssertEquals('Attribute value ',2,M2.Int);
end;

Procedure TTestClassExtendedRTTI.TestProtectedPropertyAttributes;

var
  Obj : TRttiObject;
  RttiData : TRttiInstanceType absolute obj;
  Attrs : TCustomAttributeArray;
  Prop : TRttiProperty;
  O : TCustomAttribute;
  M2 : My2Attribute absolute O;

begin
  Obj:=FCtx.GetType(TypeInfo(TPropertyObject));
  AssertEquals('Correct class type',TRttiInstanceType,Obj.ClassType);
  Prop:=RttiData.GetProperty('ProtectedProperty');
  AssertNotNull('Have property',Prop);
  Attrs:=Prop.GetAttributes;
  AssertNotNull('Have attribute data',Pointer(Attrs));
  AssertEquals('attribute count',1,Length(Attrs));
  AssertEquals('Attribute 1 name','My2Attribute',Attrs[0].ClassName);
  O:=Attrs[0];
  AssertNotNull('Attribute class ',O);
  AssertEquals('Attribute class ',O.ClassType,My2Attribute);
  AssertEquals('Attribute value ',3,M2.Int);
end;

Procedure TTestClassExtendedRTTI.TestPublicPropertyAttributes;

var
  Obj : TRttiObject;
  RttiData : TRttiInstanceType absolute obj;
  Attrs : TCustomAttributeArray;
  Prop : TRttiProperty;
  O : TCustomAttribute;
  M3 : My3Attribute absolute O;

begin
  Obj:=FCtx.GetType(TypeInfo(TPropertyObject));
  AssertEquals('Correct class type',TRttiInstanceType,Obj.ClassType);
  Prop:=RttiData.GetProperty('PublicProperty');
  AssertNotNull('Have property',Prop);
  Attrs:=Prop.GetAttributes;
  AssertNotNull('Have attribute data',Pointer(Attrs));
  AssertEquals('attribute count',1,Length(Attrs));
  AssertEquals('Attribute 1 name','My3Attribute',Attrs[0].ClassName);
  O:=Attrs[0];
  AssertNotNull('Attribute class ',O);
  AssertEquals('Attribute class ',O.ClassType,My3Attribute);
  AssertEquals('Attribute value ',4,M3.Int);
end;

Procedure TTestClassExtendedRTTI.TestPublishedPropertyAttributes ;

var
  Obj : TRttiObject;
  RttiData : TRttiInstanceType absolute obj;
  Attrs : TCustomAttributeArray;
  Prop : TRttiProperty;
  O : TCustomAttribute;
  M3 : My3Attribute absolute O;

begin
  Obj:=FCtx.GetType(TypeInfo(TPropertyObject));
  AssertEquals('Correct class type',TRttiInstanceType,Obj.ClassType);
  Prop:=RttiData.GetProperty('PublishedProperty');
  AssertNotNull('Have property',Prop);
  Attrs:=Prop.GetAttributes;
  AssertNotNull('Have attribute data',Pointer(Attrs));
  AssertEquals('attribute count',1,Length(Attrs));
  AssertEquals('Attribute 1 name','My3Attribute',Attrs[0].ClassName);
  O:=Attrs[0];
  AssertNotNull('Attribute class ',O);
  AssertEquals('Attribute class ',O.ClassType,My3Attribute);
  AssertEquals('Attribute value ',5,M3.Int);
end;


{ TTestRecordExtendedRTTI }

procedure TTestRecordExtendedRTTI.TestFields;

Var
  A : TRttiFieldArray;
  Obj : TRttiObject;
  RttiData : TRttiRecordType absolute obj;
  aCount : Integer;

begin
  Obj:=FCtx.GetType(TypeInfo(TRecordFieldRTTI));
  AssertEquals('Correct class type',TRttiRecordType,Obj.ClassType);
  A:=RttiData.GetFields;
  aCount:=Length(A);
  AssertEquals('Record fields Count',4,aCount);
  CheckField(0, A[0],'FRPrivateA',tkInteger,mvPrivate);
  CheckField(1, A[1],'FRPrivateB',tkInteger,mvPrivate);
  CheckField(4, A[2],'FRPublicA',tkInteger,mvPublic);
  CheckField(5, A[3],'FRPublicB',tkInteger,mvPublic);


  Obj:=FCtx.GetType(TypeInfo(TRecordFieldRTTIMixed));
  AssertEquals('Correct class type',TRttiRecordType,Obj.ClassType);
  A:=RttiData.GetFields;
  aCount:=Length(A);
  AssertEquals('Mixed record fields Count',4,aCount);
  CheckField(0, A[0],'FRPrivateA',tkInteger,mvPrivate);
  CheckField(1, A[1],'FRPrivateB',tkInteger,mvPrivate);
  CheckField(4, A[2],'FRPublicA',tkInteger,mvPublic);
  CheckField(5, A[3],'FRPublicB',tkInteger,mvPublic);

end;

procedure TTestRecordExtendedRTTI.TestProperties;

Var
  A : TRttiPropertyArray;
  Obj : TRttiObject;
  RttiData : TRttiRecordType absolute obj;
  aCount : Integer;

begin
  // TRecordFieldRTTI
  Obj:=FCtx.GetType(TypeInfo(TRecordFieldRTTI));
  AssertEquals('Correct class type',TRttiRecordType,Obj.ClassType);
  A:=RttiData.GetProperties;
  aCount:=Length(A);
  AssertEquals('Record property Count',4,aCount);
  CheckProperty(0, A[0],'RPrivateA',tkInteger,mvPrivate);
  CheckProperty(1, A[1],'RPrivateB',tkInteger,mvPrivate);
  CheckProperty(2, A[2],'RPublicA',tkInteger,mvPublic);
  CheckProperty(3, A[3],'RPublicB',tkInteger,mvPublic);
  // TRecordFieldRTTIMixed
  Obj:=FCtx.GetType(TypeInfo(TRecordFieldRTTIMixed));
  AssertEquals('Correct class type',TRttiRecordType,Obj.ClassType);
  A:=RttiData.GetProperties;
  aCount:=Length(A);
  AssertEquals('Record mixed property Count',4,aCount);
  CheckProperty(0, A[0],'RPrivateA',tkInteger,mvPrivate);
  CheckProperty(1, A[1],'RPrivateB',tkInteger,mvPrivate);
  CheckProperty(2, A[2],'RPublicA',tkInteger,mvPublic);
  CheckProperty(3, A[3],'RPublicB',tkInteger,mvPublic);
end;

procedure TTestRecordExtendedRTTI.TestDeclaredMethods;
Var
  A : TRttiMethodArray;
  Obj : TRttiObject;
  RttiData : TRttiRecordType absolute obj;
  aCount : Integer;
  Parms : TRttiParameterArray;

begin
  Obj:=FCtx.GetType(TypeInfo(TRecordMethodRTTI));
  AssertEquals('Correct class type',TRttiRecordType,Obj.ClassType);
  A:=RttiData.GetDeclaredMethods;
  aCount:=Length(A);
  AssertEquals('Method Full Count',4,aCount);
  CheckMethod('Full',0, A[0],'PrivateMethodA',mvPrivate);
  CheckMethod('Full',1, A[1],'PrivateMethodB',mvPrivate);
  CheckMethod('Full',2, A[2],'PublicMethodA',mvPublic);
  CheckMethod('Full',3, A[3],'PublicMethodB',mvPublic);
  Parms:=A[3].GetParameters;
  AssertEquals('Parameter length',1,Length(Parms));
  AssertNotNull('Have Parameter',Parms[0]);
  AssertEquals('Parameter name','I',Parms[0].Name);
end;

procedure TTestRecordExtendedRTTI.TestMethods;
Var
  A : TRttiMethodArray;
  Obj : TRttiObject;
  RttiData : TRttiRecordType absolute obj;
  aCount : Integer;
begin
  Obj:=FCtx.GetType(TypeInfo(TRecordMethodRTTI));
  AssertEquals('Correct class type',TRttiRecordType,Obj.ClassType);
  A:=RttiData.GetDeclaredMethods;
  aCount:=Length(A);
  // Just check that the count is correct
  AssertEquals('Method Full Count',4,aCount);
end;

Procedure TTestRecordExtendedRTTI.TestPrivateFieldAttributes;

var
  Obj : TRttiObject;
  RttiData : TRttiRecordType absolute obj;
  Attrs : TCustomAttributeArray;
  Fld : TRttiField;
  O : TCustomAttribute;
  M2 : My2Attribute absolute O;

begin
  Obj:=FCtx.GetType(TypeInfo(TFieldRecord));
  AssertEquals('Correct class type',TRttiRecordType,Obj.ClassType);
  Fld:=RttiData.GetField('PrivateField');
  AssertNotNull('Have field',Fld);
  Attrs:=Fld.GetAttributes;
  AssertNotNull('Have attribute data',Pointer(Attrs));
  AssertEquals('attribute count',3,Length(Attrs));
  AssertEquals('Attribute 1 name','WeakAttribute',Attrs[0].ClassName);
  AssertEquals('Attribute 2 name','MyAttribute',Attrs[1].ClassName);
  AssertEquals('Attribute 2 name','My2Attribute',Attrs[2].ClassName);
  O:=Attrs[2];
  AssertNotNull('Attribute class ',O);
  AssertEquals('Attribute class ',O.ClassType,My2Attribute);
  AssertEquals('Attribute value ',2,M2.Int);
end;

Procedure TTestRecordExtendedRTTI.TestPublicFieldAttributes;

var
  Obj : TRttiObject;
  RttiData : TRttiRecordType absolute obj;
  Attrs : TCustomAttributeArray;
  Fld : TRttiField;
  O : TCustomAttribute;
  M3 : My3Attribute absolute O;
  aCount : Integer;

begin
  Obj:=FCtx.GetType(TypeInfo(TFieldRecord));
  AssertEquals('Correct class type',TRttiRecordType,Obj.ClassType);
  aCount:=0;
  For Fld in RttiData.GetFields do
    if Fld.Visibility=mvPublic then
      inc(aCount);
  AssertEquals('Field count',3,aCount);
  // PublicField
  Fld:=RttiData.GetField('PublicField');
  AssertNotNull('Have field',Fld);
  Attrs:=Fld.GetAttributes;
  AssertNotNull('Have attribute data',Pointer(Attrs));
  AssertEquals('attribute count',1,Length(Attrs));
  AssertEquals('Attribute 1 name','My3Attribute',Attrs[0].ClassName);
  O:=Attrs[0];
  AssertNotNull('Attribute class ',O);
  AssertEquals('Attribute class ',O.ClassType,My3Attribute);
  AssertEquals('Attribute value ',3,M3.Int);
  // A
  Fld:=RttiData.GetField('A');
  AssertNotNull('A Have field',Fld);
  Attrs:=Fld.GetAttributes;
  AssertNotNull('A Have attribute data',Pointer(Attrs));
  AssertEquals('A Attribute count',1,Length(Attrs));
  AssertEquals('A Attribute 1 name','My3Attribute',Attrs[0].ClassName);
  O:=Attrs[0];
  AssertNotNull('A: Attribute class ',O);
  AssertEquals('A: Attribute class ',O.ClassType,My3Attribute);
  AssertEquals('A: Attribute value ',4,M3.Int);
  // B
  Fld:=RttiData.GetField('B');
  AssertNotNull('B Have field',Fld);
  Attrs:=Fld.GetAttributes;
  AssertNotNull('B Have attribute data',Pointer(Attrs));
  AssertEquals('A Attribute count',1,Length(Attrs));
  AssertEquals('A Attribute 1 name','My3Attribute',Attrs[0].ClassName);
  O:=Attrs[0];
  AssertNotNull('B: Attribute class ',O);
  AssertEquals('B: Attribute class ',O.ClassType,My3Attribute);
  AssertEquals('B: Attribute value ',4,M3.Int);
end;



initialization
{$ifdef fpc}
  RegisterTest(TTestRTTI);
  RegisterTest(TTestClassExtendedRTTI);
  RegisterTest(TTestRecordExtendedRTTI);
{$else fpc}
  RegisterTest(TTestRTTI.Suite);
  RegisterTest(TTestClassExtendedRTTI.suite);
  RegisterTest(TTestRecordExtendedRTTI.Suite);
{$endif fpc}
end.

