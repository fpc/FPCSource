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

  { TTestCase1 }

  TTestCase1= class(TTestCase)
  published
    //procedure GetTypes;
    procedure GetTypeInteger;
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

    procedure TestPropSetValueString;
    procedure TestPropSetValueInteger;
    procedure TestPropSetValueBoolean;
    procedure TestPropSetValueShortString;

    procedure TestGetValueStringCastError;
    procedure TestMakeObject;
    procedure TestGetIsReadable;
    procedure TestIsWritable;

    procedure TestIsManaged;
  end;

implementation

type

  {$M+}
  TGetClassProperties = class
  private
    FPubPropRO: integer;
    FPubPropRW: integer;
  published
    property PubPropRO: integer read FPubPropRO;
    property PubPropRW: integer read FPubPropRW write FPubPropRW;
    property PubPropSetRO: integer read FPubPropRO;
    property PubPropSetRW: integer read FPubPropRW write FPubPropRW;
  end;
  {$M-}

  { TTestValueClass }

  {$M+}
  TTestValueClass = class
  private
    FAInteger: integer;
    FAString: string;
    FABoolean: boolean;
    FAShortString: ShortString;
    function GetAInteger: integer;
    function GetAString: string;
    function GetABoolean: boolean;
    function GetAShortString: ShortString;
    procedure SetWriteOnly(AValue: integer);
  published
    property AInteger: Integer read FAInteger write FAInteger;
    property AString: string read FAString write FAString;
    property ABoolean: boolean read FABoolean write FABoolean;
    property AShortString: ShortString read FAShortString write FAShortString;
    property AGetInteger: Integer read GetAInteger;
    property AGetString: string read GetAString;
    property AGetBoolean: boolean read GetABoolean;
    property AGetShortString: ShortString read GetAShortString;
    property AWriteOnly: integer write SetWriteOnly;
  end;
  {$M-}

  TManagedRec = record
    s: string;
  end;

{$ifdef fpc}
  TManagedRecOp = record
    class operator AddRef(var a: TManagedRecOp);
  end;
{$endif}

  TNonManagedRec = record
    i: Integer;
  end;

  TManagedObj = object
    i: IInterface;
  end;

  TNonManagedObj = object
    d: double;
  end;

  TTestEnum = (te1, te2, te3, te4, te5);
  TTestSet = set of TTestEnum;

  TTestProc = procedure;
  TTestMethod = procedure of object;
  TTestHelper = class helper for TObject
  end;

  TArrayOfString = array[0..0] of string;
  TArrayOfManagedRec = array[0..0] of TManagedRec;
  TArrayOfNonManagedRec = array[0..0] of TNonManagedRec;
  TArrayOfByte = array[0..0] of byte;

{$ifdef fpc}
{$PUSH}
{$INTERFACES CORBA}

  ICORBATest = interface
  end;

{$POP}
{$endif}

{$ifdef fpc}
class operator TManagedRecOp.AddRef(var  a: TManagedRecOp);
begin
end;
{$endif}

{ TTestValueClass }

function TTestValueClass.GetAInteger: integer;
begin
  result := FAInteger;
end;

function TTestValueClass.GetAString: string;
begin
  result := FAString;
end;

function TTestValueClass.GetABoolean: boolean;
begin
  result := FABoolean;
end;

function TTestValueClass.GetAShortString: ShortString;
begin
  Result := FAShortString;
end;

procedure TTestValueClass.SetWriteOnly(AValue: integer);
begin
  // Do nothing
end;

{ Note: GetTypes currently only returns those types that had been acquired using
        GetType, so GetTypes itself can't be really tested currently }
(*procedure TTestCase1.GetTypes;
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
end;*)

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

procedure TTestCase1.TestMakeObject;
var
  AValue: TValue;
  ATestClass: TTestValueClass;
begin
  ATestClass := TTestValueClass.Create;
  ATestClass.AInteger := 54329;
  TValue.Make(@ATestClass, TypeInfo(TTestValueClass),AValue);
  CheckEquals(AValue.IsClass, False);
  CheckEquals(AValue.IsObject, True);
  Check(AValue.AsObject=ATestClass);
  CheckEquals(TTestValueClass(AValue.AsObject).AInteger, 54329);
  ATestClass.Free;
end;

procedure TTestCase1.TestGetIsReadable;
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

procedure TTestCase1.TestIsWritable;
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

procedure TTestCase1.TestPropGetValueShortString;
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

procedure TTestCase1.TestPropGetValueProcBoolean;
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

procedure TTestCase1.TestPropGetValueProcShortString;
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

procedure TTestCase1.TestPropSetValueString;
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

procedure TTestCase1.TestPropSetValueInteger;
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

procedure TTestCase1.TestPropSetValueBoolean;
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

procedure TTestCase1.TestPropSetValueShortString;
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
    finally
      AtestClass.Free;
    end;
  finally
    c.Free;
  end;
end;

procedure TTestCase1.TestPropGetValueProcInteger;
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

procedure TTestCase1.TestPropGetValueProcString;
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
    CheckEquals(ARttiType.TypeSize,SizeOf(TObject));
    CheckEquals(ARttiType.IsManaged,false);
    CheckEquals(ARttiType.BaseType.classname,'TRttiInstanceType');
    CheckEquals(ARttiType.IsInstance,True);
    CheckEquals(ARttiType.AsInstance.DeclaringUnitName,'tests.rtti');
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
{$ifdef fpc}
  CheckEquals(LType.Name, 'LongInt');
{$else}
  CheckEquals(LType.Name, 'Integer');
{$endif}

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

procedure TTestCase1.TestIsManaged;
begin
  CheckEquals(true, IsManaged(TypeInfo(ansistring)), 'IsManaged for tkAString');
  CheckEquals(true, IsManaged(TypeInfo(widestring)), 'IsManaged for tkWString');
  CheckEquals(true, IsManaged(TypeInfo(Variant)), 'IsManaged for tkVariant');
  CheckEquals(true, IsManaged(TypeInfo(TArrayOfManagedRec)),
    'IsManaged for tkArray (with managed ElType)');
  CheckEquals(true, IsManaged(TypeInfo(TArrayOfString)),
    'IsManaged for tkArray (with managed ElType)');
  CheckEquals(true, IsManaged(TypeInfo(TManagedRec)), 'IsManaged for tkRecord');
  {$ifdef fpc}
  CheckEquals(true, IsManaged(TypeInfo(TManagedRecOp)), 'IsManaged for tkRecord');
  {$endif}
  CheckEquals(true, IsManaged(TypeInfo(IInterface)), 'IsManaged for tkInterface');
  CheckEquals(true, IsManaged(TypeInfo(TManagedObj)), 'IsManaged for tkObject');
  {$ifdef fpc}
  CheckEquals(true, IsManaged(TypeInfo(specialize TArray<byte>)), 'IsManaged for tkDynArray');
  {$else}
  CheckEquals(true, IsManaged(TypeInfo(TArray<byte>)), 'IsManaged for tkDynArray');
  {$endif}
  CheckEquals(true, IsManaged(TypeInfo(unicodestring)), 'IsManaged for tkUString');
  CheckEquals(false, IsManaged(TypeInfo(shortstring)), 'IsManaged for tkSString');
  CheckEquals(false, IsManaged(TypeInfo(Byte)), 'IsManaged for tkInteger');
  CheckEquals(false, IsManaged(TypeInfo(Char)), 'IsManaged for tkChar');
  CheckEquals(false, IsManaged(TypeInfo(TTestEnum)), 'IsManaged for tkEnumeration');
  CheckEquals(false, IsManaged(TypeInfo(Single)), 'IsManaged for tkFloat');
  CheckEquals(false, IsManaged(TypeInfo(TTestSet)), 'IsManaged for tkSet');
  CheckEquals(false, IsManaged(TypeInfo(TTestMethod)), 'IsManaged for tkMethod');
  CheckEquals(false, IsManaged(TypeInfo(TArrayOfByte)),
    'IsManaged for tkArray (with non managed ElType)');
  CheckEquals(false, IsManaged(TypeInfo(TArrayOfNonManagedRec)),
    'IsManaged for tkArray (with non managed ElType)');
  CheckEquals(false, IsManaged(TypeInfo(TNonManagedRec)), 'IsManaged for tkRecord');
  CheckEquals(false, IsManaged(TypeInfo(TObject)), 'IsManaged for tkClass');
  CheckEquals(false, IsManaged(TypeInfo(TNonManagedObj)), 'IsManaged for tkObject');
  CheckEquals(false, IsManaged(TypeInfo(WideChar)), 'IsManaged for tkWChar');
  CheckEquals(false, IsManaged(TypeInfo(Boolean)), 'IsManaged for tkBool');
  CheckEquals(false, IsManaged(TypeInfo(Int64)), 'IsManaged for tkInt64');
  CheckEquals(false, IsManaged(TypeInfo(UInt64)), 'IsManaged for tkQWord');
  {$ifdef fpc}
  CheckEquals(false, IsManaged(TypeInfo(ICORBATest)), 'IsManaged for tkInterfaceRaw');
  {$endif}
  CheckEquals(false, IsManaged(TypeInfo(TTestProc)), 'IsManaged for tkProcVar');
  CheckEquals(false, IsManaged(TypeInfo(TTestHelper)), 'IsManaged for tkHelper');
  {$ifdef fpc}
  CheckEquals(false, IsManaged(TypeInfo(file)), 'IsManaged for tkFile');
  {$endif}
  CheckEquals(false, IsManaged(TypeInfo(TClass)), 'IsManaged for tkClassRef');
  CheckEquals(false, IsManaged(TypeInfo(Pointer)), 'IsManaged for tkPointer');
  CheckEquals(false, IsManaged(nil), 'IsManaged for nil');
end;

initialization
{$ifdef fpc}
  RegisterTest(TTestCase1);
{$else fpc}
  RegisterTest(TTestCase1.Suite);
{$endif fpc}
end.

