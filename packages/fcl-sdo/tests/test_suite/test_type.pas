{$INCLUDE sdo_global.inc}
unit test_type;

interface
uses
  SysUtils, Classes
{$IFDEF FPC}
  ,fpcunit, testutils, testregistry
{$ENDIF}
{$IFNDEF FPC}
  ,TestFrameWork
{$ENDIF}
  , sdo, sdo_type, sdo_types ;

type

  TTSDOSimpleType_Test = class(TTestCase)
  private
    FDataFactory : ISDODataFactory;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  protected
    function get_datafactory() : ISDODataFactory;virtual;
    function Create_Type() : ISDOType;virtual;abstract;
    class function GetTestSuitePath() : string;
    class function GetPredefinedAlias(AList : TStrings) : Integer;virtual;
  published
    procedure test_getName();
	  procedure getBaseType();
	  procedure getURI();
	  procedure isDataObjectType();
	  procedure isSequencedType();
	  procedure isOpenType();
	  procedure isAbstractType();
    procedure isDataType();
    procedure getTypeEnum();
    procedure Alias_procs();
    procedure Used();
    procedure equals();
    procedure getPropertyIndex();
    procedure inherits();
  end;

  TSDOBooleanType_Test = class(TTSDOSimpleType_Test)
  protected
    function Create_Type() : ISDOType;override;
  end;

  TSDOByteType_Test = class(TTSDOSimpleType_Test)
  protected
    function Create_Type() : ISDOType;override;
  end;

{$IFDEF HAS_SDO_BYTES}  
  TSDOBytesType_Test = class(TTSDOSimpleType_Test)
  protected
    function Create_Type() : ISDOType;override;
    class function GetPredefinedAlias(AList : TStrings) : Integer;override;
  end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
  TSDOCharacterType_Test = class(TTSDOSimpleType_Test)
  protected
    function Create_Type() : ISDOType;override;
    class function GetPredefinedAlias(AList : TStrings) : Integer;override;
  end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
  TSDOCurrencyType_Test = class(TTSDOSimpleType_Test)
  protected
    function Create_Type() : ISDOType;override;
  end;
{$ENDIF HAS_SDO_CURRENCY}

  TSDODateTimeType_Test = class(TTSDOSimpleType_Test)
  protected
    function Create_Type() : ISDOType;override;
    class function GetPredefinedAlias(AList : TStrings) : Integer;override;
  end;

{$IFDEF HAS_SDO_DOUBLE}
  TSDODoubleType_Test = class(TTSDOSimpleType_Test)
  protected
    function Create_Type() : ISDOType;override;
  end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
  TSDOFloatType_Test = class(TTSDOSimpleType_Test)
  protected
    function Create_Type() : ISDOType;override;
  end;
{$ENDIF HAS_SDO_FLOAT}

  TSDOIntegerType_Test = class(TTSDOSimpleType_Test)
  protected
    function Create_Type() : ISDOType;override;
    class function GetPredefinedAlias(AList : TStrings) : Integer;override;
  end;

{$IFDEF HAS_SDO_LONG}
  TSDOLongType_Test = class(TTSDOSimpleType_Test)
  protected
    function Create_Type() : ISDOType;override;
  end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
  TSDOShortType_Test = class(TTSDOSimpleType_Test)
  protected
    function Create_Type() : ISDOType;override;
  end;
{$ENDIF HAS_SDO_SHORT}

  TSDOStringType_Test = class(TTSDOSimpleType_Test)
  protected
    function Create_Type() : ISDOType;override;
    class function GetPredefinedAlias(AList : TStrings) : Integer;override;
  end;

  TTSDOBaseObjectType_Test = class(TTestCase)
  private
    FDataFactory : ISDODataFactory;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  protected
    function get_datafactory() : ISDODataFactory;virtual;
  protected
    function Create_Type(
      const AName       : string   = 'AObjType';
      const AUri        : string   = sdo_namespace;
      const ASequenced  : Boolean  = False ;
      const AOpened     : Boolean  = False ;
      const AAbstract   : Boolean  = False
    ) : ISDOObjectType;virtual;abstract;
    class function GetTestSuitePath() : string;
  published
    procedure test_getName();
	  procedure getBaseType();
	  procedure getURI();
	  procedure getPropertyIndex();
	  procedure isDataObjectType();
	  procedure isSequencedType();
	  procedure isOpenType();
	  procedure isAbstractType();
    procedure isDataType();
    procedure getTypeEnum();
    procedure equals();
    procedure Alias_procs();
    procedure Used();
  end;

  TSDOObjectType_Test = class(TTSDOBaseObjectType_Test)
  protected
    function Create_Type(
      const AName       : string   = 'AObjType';
      const AUri        : string   = sdo_namespace;
      const ASequenced  : Boolean  = False ;
      const AOpened     : Boolean  = False ;
      const AAbstract   : Boolean  = False
    ) : ISDOObjectType;override;
  end;

  TSDOUserDefinedSimpleType_Test = class(TTestCase)
  private
    FDataFactory : ISDODataFactory;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  protected
    function get_datafactory() : ISDODataFactory;virtual;
    function Create_Type(
      const AName       : string   = 'AObjType';
      const AUri        : string   = sdo_namespace;
      const AAbstract   : Boolean  = False
    ) : ISDOTypeEx;virtual;
    class function GetTestSuitePath() : string;
  published
    procedure test_getName();
	  procedure getBaseType();
	  procedure getURI();
	  procedure getPropertyIndex(const APropertyName : string);
	  procedure isDataObjectType();
	  procedure isSequencedType();
	  procedure isOpenType();
	  procedure isAbstractType();
    procedure isDataType();
    procedure getTypeEnum();
    procedure equals(const AOther : ISDOType);
    procedure Alias_procs();
    procedure Used();
  end;

  TTSDOChangeSummaryType_Test = class(TTestCase)
  private
    FDataFactory : ISDODataFactory;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  protected
    function get_datafactory() : ISDODataFactory;virtual;
  protected
    function Create_Type() : ISDOType;
    class function GetTestSuitePath() : string;
    class function GetPredefinedAlias(AList : TStrings) : Integer;
  published
    procedure test_getName();
	  procedure getBaseType();
	  procedure getURI();
	  procedure isDataObjectType();
	  procedure isSequencedType();
	  procedure isOpenType();
	  procedure isAbstractType();
    procedure isDataType();
    procedure getTypeEnum();
    procedure Alias_procs();
    procedure Used();
    procedure equals();
    procedure getPropertyIndex();
  end;

  TSDOBaseDataFactory_Test = class(TTestCase)
  protected
    function Create_Factory(
    ) : ISDODataFactory;virtual;
    class function GetTestSuitePath() : string;
  published
    procedure AddType();
    procedure setBaseType();
    procedure setAlias();
    procedure addPropertyToType();
    procedure addPropertyToType_Duplicate();
    procedure addPropertyToType_DuplicateDerived();
    procedure addPropertyToType_ChangeSummary();
    procedure createNew();
    procedure inherits();
    procedure inherits_root();
    procedure type_equals();

    procedure check_circular_dependy();
    procedure check_circular_dependy_1();
    procedure check_circular_dependy_2();
    procedure check_circular_dependy_3();
    procedure check_circular_dependy_4();
  end;

implementation

uses sdo_datafactory;

{ TTSDOSimpleType_Test }

procedure TTSDOSimpleType_Test.equals();
begin
  CheckEquals(True,Create_Type().equals(Create_Type()));
end;

procedure TTSDOSimpleType_Test.getBaseType();
var
  t, bt : ISDOType;
begin
  t := Create_Type();
  bt := t.getBaseType();
  CheckNull(bt);
end;

procedure TTSDOSimpleType_Test.test_getName();
begin
  CheckEquals(True,Create_Type().getName() <> '');
end;

procedure TTSDOSimpleType_Test.getPropertyIndex();
var
  ok : Boolean;
  t : ISDOType;
begin
  ok := False;
  try
    t := Create_Type();
    t.getPropertyIndex('a');
  except
    on e : ESDOUnsupportedOperationException do begin
      ok := True;
    end;
  end;
  Check(ok);
end;

procedure TTSDOSimpleType_Test.getTypeEnum();
begin
  CheckEquals(
    True,
    Create_Type().getTypeEnum() in
      [ BooleanType,ByteType{$IFDEF HAS_SDO_BYTES},BytesType{$ENDIF}
        {$IFDEF HAS_SDO_CHAR},CharacterType{$ENDIF}
        ,DateTimeType
        {$IFDEF HAS_SDO_DOUBLE},DoubleType{$ENDIF}
        {$IFDEF HAS_SDO_FLOAT},FloatType{$ENDIF}
        ,IntegerType
        {$IFDEF HAS_SDO_LONG},LongType{$ENDIF}
        {$IFDEF HAS_SDO_SHORT},ShortType{$ENDIF}
        ,StringType 
      ]
    );
end;

procedure TTSDOSimpleType_Test.getURI();
begin
  CheckEquals(True,Create_Type().getURI() <> '');
end;

procedure TTSDOSimpleType_Test.isAbstractType() ;
begin
  CheckEquals(False,Create_Type().isAbstractType());
end;

procedure TTSDOSimpleType_Test.isDataObjectType() ;
begin
  CheckEquals(False,Create_Type().isDataObjectType());
end;

procedure TTSDOSimpleType_Test.isDataType() ;
begin
  CheckEquals(True,Create_Type().isDataType());
end;

procedure TTSDOSimpleType_Test.isOpenType() ;
begin
  CheckEquals(False,Create_Type().isOpenType());
end;

procedure TTSDOSimpleType_Test.isSequencedType() ;
begin
  CheckEquals(False,Create_Type().isSequencedType());
end;

class function TTSDOSimpleType_Test.GetTestSuitePath: string;
begin
  Result := 'Metadata';
end;

procedure TTSDOSimpleType_Test.Alias_procs();
var
  obj : ISDOTypeEx;

  function AliasExists(const AAlias : TSDOString) : Boolean;
  var
    k : Integer;
  begin
    Result := False;
    for k := 0 to (obj.getAliasCount() - 1) do begin
      if (AAlias = obj.getAlias(k)) then begin
        Result := True;
        Break;
      end;
    end;
  end;

var
  ok : Boolean;
  nativeAliasList : TStringList;
  nativeAliasCount, i : Integer;
  s : string;
begin
  nativeAliasList := TStringList.Create();
  try
    obj := Create_Type() as ISDOTypeEx;

    nativeAliasCount := GetPredefinedAlias(nativeAliasList);
    CheckEquals(nativeAliasCount,obj.getAliasCount(),'getAliasCount');
    if ( nativeAliasCount > 0 ) then begin
      for i := 0 to Pred(nativeAliasCount) do begin
        s := nativeAliasList[i];
        CheckEquals(True,AliasExists(s),Format('Unable to find predefined alias : "%s"',[s]));
      end;
    end;

    ok := False;
    try
      obj.getAlias(nativeAliasCount);
    except
      on e : ESDOIndexOutOfRangeException do begin
        ok := True;
      end;
    end;
    Check(ok,Format('getAlias(%d) : ESDOIndexOutOfRangeException expected.',[nativeAliasCount]));

    obj.SetAlias('aaaaaaaaaaaa');
    CheckEquals(True,AliasExists('aaaaaaaaaaaa'),'Alias not found.');
    CheckEquals(nativeAliasCount + 1,obj.getAliasCount(),'getAliasCount');
    ok := False;
    try
      obj.getAlias(nativeAliasCount + 1);
    except
      on e : ESDOIndexOutOfRangeException do begin
        ok := True;
      end;
    end;
    Check(ok,Format('getAlias(%d) : ESDOIndexOutOfRangeException expected.',[nativeAliasCount + 1]));

    obj.SetAlias('aaaaaaaaaaaa');
    CheckEquals(True,AliasExists('aaaaaaaaaaaa'),'Alias not found.');
    CheckEquals(nativeAliasCount + 1,obj.getAliasCount(),'getAliasCount');

    obj.SetAlias('aaaaaaaaaaaab');
    CheckEquals(nativeAliasCount + 2,obj.getAliasCount(),'getAliasCount');
    CheckEquals(True,AliasExists('aaaaaaaaaaaab'),'Alias not found.');
    CheckEquals(True,AliasExists('aaaaaaaaaaaa'),'Alias not found.');
    ok := False;
    try
      obj.getAlias(nativeAliasCount + 2);
    except
      on e : ESDOIndexOutOfRangeException do begin
        ok := True;
      end;
    end;
    Check(ok,Format('getAlias(%d) : ESDOIndexOutOfRangeException expected.',[nativeAliasCount + 2]));
  finally
    FreeAndNil(nativeAliasList);
  end;
end;

procedure TTSDOSimpleType_Test.Used();
var
  obj : ISDOTypeEx;
  ok : Boolean;
begin
  obj := Create_Type() as ISDOTypeEx;
  CheckEquals(False,obj.isUsed());

  obj.setUsedFlag(False);
    CheckEquals(False,obj.isUsed());
  obj.setUsedFlag(True);
    CheckEquals(True,obj.isUsed());
  obj.setUsedFlag(False);
    CheckEquals(False,obj.isUsed());

  obj.setUsedFlag(True);
    CheckEquals(True,obj.isUsed());
    ok := False;
    try
      obj.setAlias('azerty_qwerty');
    except
      on e : ESDOUnsupportedOperationException do begin
        ok := True;
      end;
    end;
    Check(ok,'Modify an used type.');
end;

class function TTSDOSimpleType_Test.GetPredefinedAlias(AList: TStrings): Integer;
begin
  AList.Clear();
  Result := AList.Count;
end;

procedure TTSDOSimpleType_Test.inherits();
var
  tX : ISDOTypeEx;
begin
  tX := Create_Type() as ISDOTypeEx;
  Check(not tx.inherits(nil));
  Check(not tx.inherits(tx));
end;

function TTSDOSimpleType_Test.get_datafactory() : ISDODataFactory;
begin
  if ( FDataFactory = nil ) then
    FDataFactory := TSDODataFactory.Create() as ISDODataFactory;
  Result := FDataFactory;
end;

procedure TTSDOSimpleType_Test.SetUp;
begin
  inherited;
  FDataFactory := nil;
end;

procedure TTSDOSimpleType_Test.TearDown;
begin
  FDataFactory := nil;
  inherited;
end;

{ TSDOBooleanType_Test }

function TSDOBooleanType_Test.Create_Type() : ISDOType;
begin
  Result := TSDOBooleanType.Create(get_datafactory()) as ISDOType;
end;

{ TSDOByteType_Test }

function TSDOByteType_Test.Create_Type() : ISDOType;
begin
  Result := TSDOByteType.Create(get_datafactory()) as ISDOType;
end;

{$IFDEF HAS_SDO_BYTES}
{ TSDOBytesType_Test }

function TSDOBytesType_Test.Create_Type: ISDOType;
begin
  Result := TSDOBytesType.Create(get_datafactory()) as ISDOType;
end;

class function TSDOBytesType_Test.GetPredefinedAlias(AList: TStrings): Integer;
begin
  AList.Clear();
  AList.Add('hexBinary');
  Result := AList.Count;
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
{ TSDOCharacterType_Test }

function TSDOCharacterType_Test.Create_Type() : ISDOType;
begin
  Result := TSDOCharacterType.Create(get_datafactory()) as ISDOType;
end;

class function TSDOCharacterType_Test.GetPredefinedAlias(AList: TStrings): Integer;
begin
  AList.Clear();
  AList.Add('Char');
  AList.Add('WideChar');
  AList.Add('AnsiChar');
  Result := AList.Count;
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
{ TSDOCurrencyType_Test }

function TSDOCurrencyType_Test.Create_Type() : ISDOType;
begin
  Result := TSDOCurrencyType.Create(get_datafactory()) as ISDOType;
end;
{$ENDIF HAS_SDO_CURRENCY}

{ TSDODateTimeType_Test }

function TSDODateTimeType_Test.Create_Type() : ISDOType;
begin
  Result := TSDODateTimeType.Create(get_datafactory()) as ISDOType;
end;

class function TSDODateTimeType_Test.GetPredefinedAlias(AList: TStrings): Integer;
begin
  AList.Clear();
  AList.Add('Date');
  Result := AList.Count;
end;

{$IFDEF HAS_SDO_DOUBLE}
{ TSDODoubleType_Test }

function TSDODoubleType_Test.Create_Type() : ISDOType;
begin
  Result := TSDODoubleType.Create(get_datafactory()) as ISDOType;
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
{ TSDOFloatType_Test }

function TSDOFloatType_Test.Create_Type() : ISDOType;
begin
  Result := TSDOFloatType.Create(get_datafactory()) as ISDOType;
end;
{$ENDIF HAS_SDO_FLOAT}

{ TSDOIntegerType_Test }

function TSDOIntegerType_Test.Create_Type() : ISDOType;
begin
  Result := TSDOIntegerType.Create(get_datafactory()) as ISDOType;
end;

class function TSDOIntegerType_Test.GetPredefinedAlias(AList: TStrings): Integer;
begin
  AList.Clear();
  AList.Add('Int');
  Result := AList.Count;
end;

{$IFDEF HAS_SDO_LONG}
{ TSDOLongType_Test }

function TSDOLongType_Test.Create_Type() : ISDOType;
begin
  Result := TSDOLongType.Create(get_datafactory()) as ISDOType;
end;
{$ENDIF HAS_SDO_LONG}

{ TSDOStringType_Test }

function TSDOStringType_Test.Create_Type() : ISDOType;
begin
  Result := TSDOStringType.Create(get_datafactory()) as ISDOType;
end;

class function TSDOStringType_Test.GetPredefinedAlias(AList: TStrings): Integer;
begin
  AList.Clear();
  AList.Add('Strings');
  Result := AList.Count;
end;

{ TTSDOBaseObjectType_Test }

procedure TTSDOBaseObjectType_Test.Alias_procs();
var
  obj : ISDOTypeEx;
  ok : Boolean;
begin
  obj := Create_Type() as ISDOTypeEx;
  CheckEquals(0,obj.getAliasCount(),'getAliasCount');

  ok := False;
  try
    obj.getAlias(1);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  Check(ok,'getAlias(1) : ESDOIndexOutOfRangeException expected.');

  obj.SetAlias('a');
  CheckEquals('a',obj.getAlias(0),'getAlias');
  CheckEquals(1,obj.getAliasCount(),'getAliasCount');
  ok := False;
  try
    obj.getAlias(1);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  Check(ok,'getAlias(1) : ESDOIndexOutOfRangeException expected.');

  obj.SetAlias('a');
  CheckEquals('a',obj.getAlias(0),'getAlias');
  CheckEquals(1,obj.getAliasCount(),'getAliasCount');

  obj.SetAlias('b');
  CheckEquals(2,obj.getAliasCount(),'getAliasCount');
  CheckEquals('a',obj.getAlias(0),'getAlias');
  CheckEquals('b',obj.getAlias(1),'getAlias');
  ok := False;
  try
    obj.getAlias(2);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  Check(ok,'getAlias(2) : ESDOIndexOutOfRangeException expected.');
end;

procedure TTSDOBaseObjectType_Test.equals();
var
  a, b : ISDOType;
  ls : IInterfaceList;
begin
  ls := TInterfaceList.Create();

  a := Create_Type('A', 'uri-a'); ls.Add(a);
  b := Create_Type('A', 'uri-a'); ls.Add(b);
  CheckEquals(True,a.equals(b));

  a := Create_Type('A', 'uri-a'); ls.Add(a);
  b := Create_Type('B', 'uri-a'); ls.Add(b);
  CheckEquals(False,a.equals(b));

  a := Create_Type('A', 'uri-a'); ls.Add(a);
  b := Create_Type('A', 'uri-b'); ls.Add(b);
  CheckEquals(False,a.equals(b));

  a := Create_Type('A', 'uri-a'); ls.Add(a);
  b := Create_Type('B', 'uri-b'); ls.Add(b);
  CheckEquals(False,a.equals(b));
end;

procedure TTSDOBaseObjectType_Test.getBaseType();
var
  baseObj, obj : ISDOObjectType;
begin
  obj := Create_Type();
  CheckNull(obj.getBaseType());
  baseObj := Create_Type('BaseAQZ');
  obj.SetBaseType(baseObj);
  Check(baseObj = obj.getBaseType());
  obj.SetBaseType(nil);
  CheckNull(obj.getBaseType());
end;

procedure TTSDOBaseObjectType_Test.getPropertyIndex();
var
  baseObj, obj : ISDOObjectType;
  ok : Boolean;
  boolType : ISDOType;
  i : Integer;
begin
  boolType := TSDOBooleanType.Create(get_datafactory());

  obj := Create_Type('Z');
  ok := False;
  try
    obj.getPropertyIndex('a') ;
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  Check(ok);

  obj.AddProperty('a',boolType,[]);
  i := obj.getPropertyIndex('a') ;
  CheckEquals(i,obj.getPropertyIndex('a'));
  CheckEquals(i,obj.getProperties().getIndex('a'));

  obj.AddProperty('b',boolType,[]);
  i := obj.getPropertyIndex('b') ;
  CheckEquals(i,obj.getPropertyIndex('b'));
  CheckEquals(i,obj.getProperties().getIndex('b'));

  i := obj.getPropertyIndex('a') ;
  CheckEquals(i,obj.getPropertyIndex('a'));
  CheckEquals(i,obj.getProperties().getIndex('a'));

  CheckNotEquals(obj.getPropertyIndex('a') , obj.getPropertyIndex('b'));
  CheckNotEquals(obj.getProperties().getIndex('a') , obj.getProperties().getIndex('b'));

  CheckEquals(True,obj.getPropertyIndex('a') < obj.getPropertyIndex('b'), 'Index(a) < Index(b)');
  CheckEquals(True,obj.getProperties().getIndex('a') < obj.getProperties().getIndex('b'), 'getProperties : Index(a) < Index(b)');

  baseObj := Create_Type('ZZ');
  baseObj.AddProperty('za',boolType,[]);
  obj.SetBaseType(baseObj);

  i := obj.getPropertyIndex('za') ;
  CheckEquals(i,obj.getPropertyIndex('za'));
  CheckEquals(i,obj.getProperties().getIndex('za'));

  CheckNotEquals(obj.getPropertyIndex('za') , obj.getPropertyIndex('a'));
  CheckNotEquals(obj.getProperties().getIndex('za') , obj.getProperties().getIndex('a'));

  CheckNotEquals(obj.getPropertyIndex('a') , obj.getPropertyIndex('b'));
  CheckNotEquals(obj.getProperties().getIndex('a') , obj.getProperties().getIndex('b'));

  CheckEquals(True,obj.getPropertyIndex('za') < obj.getPropertyIndex('a'), 'Index(za) < Index(a)');
  CheckEquals(True,obj.getProperties().getIndex('za') < obj.getProperties().getIndex('a'), 'getProperties : Index(za) < Index(a)');

  CheckEquals(True,obj.getPropertyIndex('a') < obj.getPropertyIndex('b'), 'Index(a) < Index(b)');
  CheckEquals(True,obj.getProperties().getIndex('a') < obj.getProperties().getIndex('b'), 'getProperties : Index(a) < Index(b)');
 
end;

class function TTSDOBaseObjectType_Test.GetTestSuitePath: string;
begin
  Result := 'Metadata';
end;

procedure TTSDOBaseObjectType_Test.getTypeEnum();
begin
  CheckEquals(True,Create_Type().getTypeEnum() = ObjectType);
end;

procedure TTSDOBaseObjectType_Test.getURI();
const sTYPE_NAME = 'aqwsdfgg'; sTYPE_URI = 'ldhdsyhjhn';
var
  obj : ISDOObjectType;
begin
  obj := Create_Type(sTYPE_NAME,sTYPE_URI);
  CheckEquals(sTYPE_NAME,obj.getName());
  CheckEquals(sTYPE_URI,obj.getURI());
end;

function TTSDOBaseObjectType_Test.get_datafactory(): ISDODataFactory;
begin
  if ( FDataFactory = nil ) then
    FDataFactory := TSDODataFactory.Create() as ISDODataFactory;
  Result := FDataFactory;
end;

procedure TTSDOBaseObjectType_Test.isAbstractType();
const sTYPE_NAME = 'aqwsdfgg'; sTYPE_URI = 'ldhdsyhjhn';
var
  obj : ISDOObjectType;
begin
  obj := Create_Type(sTYPE_NAME,sTYPE_URI,False,False,False);
  CheckEquals(False,obj.isAbstractType());

  obj := Create_Type(sTYPE_NAME,sTYPE_URI,False,False,True);
  CheckEquals(True,obj.isAbstractType());
end;

procedure TTSDOBaseObjectType_Test.isDataObjectType();
begin
  CheckEquals(True,Create_Type().isDataObjectType());
end;

procedure TTSDOBaseObjectType_Test.isDataType();
begin
  CheckEquals(False,Create_Type().isDataType());
end;

procedure TTSDOBaseObjectType_Test.isOpenType();
const sTYPE_NAME = 'aqwsdfgg'; sTYPE_URI = 'ldhdsyhjhn';
var
  obj : ISDOObjectType;
begin
  obj := Create_Type(sTYPE_NAME,sTYPE_URI,False,False,False);
  CheckEquals(False,obj.isOpenType());

  obj := Create_Type(sTYPE_NAME,sTYPE_URI,False,True,False);
  CheckEquals(True,obj.isOpenType());
end;

procedure TTSDOBaseObjectType_Test.isSequencedType();
const sTYPE_NAME = 'aqwsdfgg'; sTYPE_URI = 'ldhdsyhjhn';
var
  obj : ISDOObjectType;
begin
  obj := Create_Type(sTYPE_NAME,sTYPE_URI,False,False,False);
  CheckEquals(False,obj.isSequencedType());

  obj := Create_Type(sTYPE_NAME,sTYPE_URI,True,False,False);
  CheckEquals(True,obj.isSequencedType());
end;

procedure TTSDOBaseObjectType_Test.SetUp;
begin
  inherited;
  FDataFactory := nil;
end;

procedure TTSDOBaseObjectType_Test.TearDown;
begin
  FDataFactory := nil;
  inherited;
end;

procedure TTSDOBaseObjectType_Test.test_getName();
const sNAME = 'AQZ';
begin
  CheckEquals(sNAME,Create_Type(sNAME).getName());
end;

procedure TTSDOBaseObjectType_Test.Used();
var
  obj, obj2, baseObj : ISDOObjectType;
  ok : Boolean;
  boolType, intType, strType : ISDOTypeEx;
begin
  obj := Create_Type() as ISDOObjectType;
  obj2 := Create_Type() as ISDOObjectType;
  CheckEquals(False,obj.isUsed());

  obj.setUsedFlag(False);
    CheckEquals(False,obj.isUsed());
  obj.setUsedFlag(True);
    CheckEquals(True,obj.isUsed());
  obj.setUsedFlag(False);
    CheckEquals(False,obj.isUsed());

  obj.setUsedFlag(True);
    CheckEquals(True,obj.isUsed());
    ok := False;
    try
      obj.setAlias('azerty_qwerty');
    except
      on e : ESDOUnsupportedOperationException do begin
        ok := True;
      end;
    end;
    Check(ok,'Modify an used type (setAlias).');

    ok := False;
    try
      obj.setBaseType(obj2 as ISDOType);
    except
      on e : ESDOUnsupportedOperationException do begin
        ok := True;
      end;
    end;
    Check(ok,'Modify an used type(setBaseType).');

    ok := False;
    try
      obj.AddProperty('a',TSDOBooleanType.Create(get_datafactory()) as ISDOType,[]);
    except
      on e : ESDOUnsupportedOperationException do begin
        ok := True;
      end;
    end;
    Check(ok,'Modify an used type (AddProperty).');

  //--------------------------------------------------
  boolType := TSDOBooleanType.Create(get_datafactory());
  intType := TSDOIntegerType.Create(get_datafactory());
  strType := TSDOStringType.Create(get_datafactory());

  obj := Create_Type('Z');
  obj.AddProperty('a',boolType,[]);
  obj.AddProperty('b',intType,[]);
  obj.setUsedFlag(True);
    CheckEquals(True,obj.isUsed());
    CheckEquals(True,boolType.isUsed());
    CheckEquals(True,intType.isUsed());

  obj.setUsedFlag(False);

  baseObj := Create_Type('ZZ');
  baseObj.AddProperty('za',strType,[]);
  obj.SetBaseType(baseObj);

  obj.setUsedFlag(True);
    CheckEquals(True,obj.isUsed());
    CheckEquals(True,boolType.isUsed());
    CheckEquals(True,intType.isUsed());
    CheckEquals(True,baseObj.isUsed());
    CheckEquals(True,strType.isUsed());

end;

{ TSDOObjectType_Test }

function TSDOObjectType_Test.Create_Type(const AName, AUri: string;
  const ASequenced, AOpened, AAbstract: Boolean): ISDOObjectType;
begin
  Result := TSDOObjectType.Create(get_datafactory(),AName,AUri,ASequenced,AOpened,AAbstract);
end;

{ TSDOUserDefinedSimpleType_Test }

procedure TSDOUserDefinedSimpleType_Test.Alias_procs;
var
  obj : ISDOTypeEx;
  ok : Boolean;
begin
  obj := Create_Type() as ISDOTypeEx;
  CheckEquals(0,obj.getAliasCount(),'getAliasCount');

  ok := False;
  try
    obj.getAlias(1);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  Check(ok,'getAlias(1) : ESDOIndexOutOfRangeException expected.');

  obj.SetAlias('a');
  CheckEquals('a',obj.getAlias(0),'getAlias');
  CheckEquals(1,obj.getAliasCount(),'getAliasCount');
  ok := False;
  try
    obj.getAlias(1);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  Check(ok,'getAlias(1) : ESDOIndexOutOfRangeException expected.');

  obj.SetAlias('a');
  CheckEquals('a',obj.getAlias(0),'getAlias');
  CheckEquals(1,obj.getAliasCount(),'getAliasCount');

  obj.SetAlias('b');
  CheckEquals(2,obj.getAliasCount(),'getAliasCount');
  CheckEquals('a',obj.getAlias(0),'getAlias');
  CheckEquals('b',obj.getAlias(1),'getAlias');
  ok := False;
  try
    obj.getAlias(2);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  Check(ok,'getAlias(2) : ESDOIndexOutOfRangeException expected.');
end;

function TSDOUserDefinedSimpleType_Test.Create_Type(const AName,
  AUri: string; const AAbstract: Boolean): ISDOTypeEx;
begin
  Result := TSDOUserDefinedSimpleType.Create(get_datafactory(),AName,AUri,AAbstract);
end;

procedure TSDOUserDefinedSimpleType_Test.equals(const AOther: ISDOType);
begin
  CheckEquals(True,Create_Type().equals(Create_Type()));
end;

procedure TSDOUserDefinedSimpleType_Test.getBaseType();
var
  baseObj, obj : ISDOTypeEx;
  ok : Boolean;
begin
  obj := TSDOUserDefinedSimpleType.Create(get_datafactory(), 'a',sdo_namespace,False);
  CheckNull(obj.getBaseType());
  baseObj := TSDOByteType.Create(get_datafactory());
  obj.SetBaseType(baseObj);
  Check(baseObj=obj.getBaseType());
  obj.SetBaseType(nil);
  CheckNull(obj.getBaseType());

  baseObj := TSDOObjectType.Create(get_datafactory(),'x',sdo_namespace,False,False,False);
  ok := False;
  try
    obj.setBaseType(baseObj);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Can not derive from ObjectType.');
end;

procedure TSDOUserDefinedSimpleType_Test.getPropertyIndex(const APropertyName: string);
var
  ok : Boolean;
  t : ISDOType;
begin
  ok := False;
  try
    t := Create_Type();
    t.getPropertyIndex('a');
  except
    on e : ESDOUnsupportedOperationException do begin
      ok := True;
    end;
  end;
  Check(ok);
end;

class function TSDOUserDefinedSimpleType_Test.GetTestSuitePath: string;
begin
  Result := 'Metadata';
end;

procedure TSDOUserDefinedSimpleType_Test.getTypeEnum();
var
  baseObj, obj : ISDOTypeEx;
  ok : Boolean;
begin
  obj := Create_Type();
  ok := False;
  try
    obj.getTypeEnum();
  except
    on e : ESDOIncompleteTypeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Incomplete Type.');

  baseObj := TSDOByteType.Create(get_datafactory());
  obj.SetBaseType(baseObj);
  CheckEquals(True,baseObj.getTypeEnum() = obj.getTypeEnum());

  obj.setBaseType(nil);
  try
    obj.getTypeEnum();
  except
    on e : ESDOIncompleteTypeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Incomplete Type.');
end;

procedure TSDOUserDefinedSimpleType_Test.getURI();
begin
  CheckEquals(sdo_namespace,Create_Type().getURI());
end;

function TSDOUserDefinedSimpleType_Test.get_datafactory: ISDODataFactory;
begin
  if ( FDataFactory = nil ) then
    FDataFactory := TSDODataFactory.Create() as ISDODataFactory;
  Result := FDataFactory;
end;

procedure TSDOUserDefinedSimpleType_Test.isAbstractType();
begin
  CheckEquals(False,Create_Type('a',sdo_namespace,False).isAbstractType());
  CheckEquals(True,Create_Type('a',sdo_namespace,True).isAbstractType());
end;

procedure TSDOUserDefinedSimpleType_Test.isDataObjectType();
begin
  CheckEquals(False,Create_Type().isDataObjectType());
end;

procedure TSDOUserDefinedSimpleType_Test.isDataType();
begin
  CheckEquals(True,Create_Type().isDataType());
end;

procedure TSDOUserDefinedSimpleType_Test.isOpenType();
begin
  CheckEquals(False,Create_Type().isOpenType());
end;

procedure TSDOUserDefinedSimpleType_Test.isSequencedType();
begin
  CheckEquals(False,Create_Type().isSequencedType());
end;

procedure TSDOUserDefinedSimpleType_Test.SetUp;
begin
  inherited;
  FDataFactory := nil;
end;

procedure TSDOUserDefinedSimpleType_Test.TearDown;
begin
  FDataFactory := nil;
  inherited;
end;

procedure TSDOUserDefinedSimpleType_Test.test_getName();
const sNAME = 'AQZ';
begin
  CheckEquals(sNAME,Create_Type(sNAME).getName());
end;

procedure TSDOUserDefinedSimpleType_Test.Used();
var
  obj : ISDOTypeEx;
  ok : Boolean;
begin
  obj := Create_Type() as ISDOTypeEx;
  CheckEquals(False,obj.isUsed());

  obj.setUsedFlag(False);
    CheckEquals(False,obj.isUsed());
  obj.setUsedFlag(True);
    CheckEquals(True,obj.isUsed());
  obj.setUsedFlag(False);
    CheckEquals(False,obj.isUsed());

  obj.setUsedFlag(True);
    CheckEquals(True,obj.isUsed());
    ok := False;
    try
      obj.setAlias('azerty_qwerty');
    except
      on e : ESDOUnsupportedOperationException do begin
        ok := True;
      end;
    end;
    Check(ok,'Modify an used type (setAlias).');

    ok := False;
    try
      obj.setBaseType(TSDOBooleanType.Create(get_datafactory()) as ISDOType);
    except
      on e : ESDOUnsupportedOperationException do begin
        ok := True;
      end;
    end;
    Check(ok,'Modify an used type(setBaseType).');
end;

{ TSDOBaseDataFactory_Test }

procedure TSDOBaseDataFactory_Test.addPropertyToType();
const s_URI_1  = 'uri:1'; s_TYPE_1 = 'type1';
      s_PROP = 'aprop'; s_PROP_2 = 'aprop2'; s_PROP_3 = 'aprop3';
      s_URI_2  = 'uri:2'; s_TYPE_2 = 'type2';
var
  obj : ISDODataFactory;
  typ : ISDOType;
  prpList : ISDOPropertyList;
  ok : Boolean;
begin
  obj := Create_Factory();

  ok := False;
  try
    obj.addProperty(nil,s_PROP,sdo_namespace,'String',[]);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'nil Type.');

  ok := False;
  try
    obj.addProperty(s_URI_1,s_TYPE_1,s_PROP,sdo_namespace,'String',[]);
  except
    on e : ESDOTypeNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Type not created.');

  obj.AddType(s_URI_1,s_TYPE_1,[]);
  obj.AddType(s_URI_2,s_TYPE_2,[]);
    ok := False;
    try
      obj.addProperty(s_URI_1,s_TYPE_1,s_PROP,nil,[]);
    except
      on e : ESDOIllegalArgumentException do begin
        ok := True;
      end;
    end;
    CheckEquals(True,ok,'property Type = nil.');

    ok := False;
    try
      obj.addProperty(s_URI_1,s_TYPE_1,s_PROP,sdo_namespace,'String',[pfIsContainment]);
    except
      on e : ESDOIllegalArgumentException do begin
        ok := True;
      end;
    end;
    CheckEquals(True,ok,'Adding a boolean as Object.');

    obj.addProperty(s_URI_1,s_TYPE_1,s_PROP,sdo_namespace,'String',[]);
    typ := obj.getType(s_URI_1,s_TYPE_1);
    prpList := typ.getProperties();
    Check(Assigned(prpList));
    CheckEquals(True,prpList.getCount() > 0,'prpList.getCount() > 0');
    Check(nil <> prpList.find(s_PROP));
    Check(obj.getType(sdo_namespace,'String') = prpList.find(s_PROP).getType());

    obj.addProperty(typ,s_PROP_2,obj.getType(sdo_namespace,'Integer'),[]);
    typ := obj.getType(s_URI_1,s_TYPE_1);
    prpList := typ.getProperties();
    Check(nil <> prpList);
    CheckEquals(True,prpList.getCount() > 1,'prpList.getCount() > 0');
    Check(nil <> prpList.find(s_PROP_2));
    Check(obj.getType(sdo_namespace,'Integer') = prpList.find(s_PROP_2).getType());

end;

procedure TSDOBaseDataFactory_Test.addPropertyToType_ChangeSummary();
const s_uri  = 'uri:1'; s_type = 'type1';
      s_integer_prop = 'int_prop'; s_bool_prop = 'bool_prop'; s_string_prop = 'string_prop';
      s_changesummary_prop = 'change_summary_prop';
var
  obj : ISDODataFactory;
  typ : ISDOType;
  prpList : ISDOPropertyList;
  prp : ISDOProperty;
  ok : Boolean;
begin
  obj := Create_Factory();

  obj.AddType(s_uri,s_type,[]);
    typ := obj.getType(s_uri,s_type);
    obj.addProperty(typ,s_integer_prop,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
    obj.addProperty(typ,s_bool_prop,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[]);
    obj.addProperty(typ,s_string_prop,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);

    ok := False;
    try
      obj.addProperty(typ,s_changesummary_prop,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsMany]);
    except
      on e : ESDOIllegalArgumentException do begin
        ok := True;
      end;
    end;
    CheckEquals(True, ok, 'ChangeSummary property can not be a multi-value property.');

    ok := False;
    try
      obj.addProperty(typ,s_changesummary_prop,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[]);
    except
      on e : ESDOIllegalArgumentException do begin
        ok := True;
      end;
    end;
    CheckEquals(True, ok, 'ChangeSummary property must be read-only.');

    obj.addProperty(typ,s_changesummary_prop,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

    prpList := typ.getProperties();
    Check(Assigned(prpList));
    CheckEquals(4,prpList.getCount(),'prpList.getCount() > 0');
    prp := typ.getProperty(s_changesummary_prop);
    CheckNotEquals(PtrUInt(nil),PtrUInt(prp));
    CheckEquals(Ord(ChangeSummaryType), Ord(prp.getTypeEnum()));
end;

procedure TSDOBaseDataFactory_Test.addPropertyToType_Duplicate();
const s_uri = 'uri1'; s_type = 'type1'; s_prop = 'prop1';
var
  obj : ISDODataFactory;
  ok : Boolean;
begin
  obj := Create_Factory();
  obj.AddType(s_uri,s_type,[]);

  obj.addProperty(s_uri,s_type,s_prop,sdo_namespace,'String',[]);
  ok := False;
  try
    obj.addProperty(s_uri,s_type,s_prop,sdo_namespace,'String',[]);
  except
    on e : ESDODuplicatedItemException do begin
      ok := True;
    end;
  end;
  Check(ok);
end;

procedure TSDOBaseDataFactory_Test.addPropertyToType_DuplicateDerived();
const s_uri = 'uri1'; s_type = 'type1'; s_type2 = 'type2';  s_prop = 'prop1';
var
  obj : ISDODataFactory;
  ok : Boolean;
begin
  obj := Create_Factory();
  obj.AddType(s_uri,s_type,[]);
  obj.AddType(s_uri,s_type2,[]);
  obj.setBaseType(s_uri,s_type2,s_uri,s_type);

  obj.addProperty(s_uri,s_type,s_prop,sdo_namespace,'String',[]);
  ok := False;
  try
    obj.addProperty(s_uri,s_type2,s_prop,sdo_namespace,'String',[]);
  except
    on e : ESDODuplicatedItemException do begin
      ok := True;
    end;
  end;
  Check(ok);
end;

procedure TSDOBaseDataFactory_Test.AddType();
const s_URI_1  = 'uri:1'; s_URI_2  = 'uri:2';
      s_TYPE_1 = 'type1'; s_TYPE_2 = 'type2';
var
  obj : ISDODataFactory;
  typ, typ2 : ISDOType;
  typLst : ISDOTypeList;
  ok : Boolean;
begin
  obj := Create_Factory();

  ok := False;
  try
    obj.AddType(s_URI_1,s_TYPE_1,[tfIsSequenced,tfIsDataType]);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'IllegalArgument :  Sequence = True; DataType = True');

  ok := False;
  try
    obj.AddType(s_URI_1,s_TYPE_1,[tfIsOpen,tfIsDataType]);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'IllegalArgument :  Open = True; DataType = True');

  ok := False;
  try
    obj.AddType(s_URI_1,s_TYPE_1,[tfIsSequenced,tfIsOpen,tfIsDataType]);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'IllegalArgument :  Sequence = True; Open = True; DataType = True');

  ok := False;
  try
    obj.AddType(s_URI_1,'',[]);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'IllegalArgument :  Name = ""');

  ok := False;
  try
    obj.AddType(s_URI_1,'1az',[]);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'IllegalArgument :  Name = "1az"');

  typLst := obj.getTypes();
  Check(nil <> typLst,'getTypes()');

  obj.AddType(s_URI_1,s_TYPE_1, [tfIsDataType]);
  typ := typLst.find(s_URI_1,s_TYPE_1);
  Check(nil <> typ,'typLst.find(s_URI_1,s_TYPE_1)');
  Check(nil <> obj.getTypes().find(s_URI_1,s_TYPE_1),'getTypes().find(s_URI_1,s_TYPE_1)');
  Check(typLst=obj.getTypes(),'typLst = obj.getTypes()');
  CheckEquals(s_URI_1,typ.getURI());
  CheckEquals(s_TYPE_1,typ.getName());
  CheckEquals(False,typ.isDataObjectType(), 'isDataObjectType');
  CheckEquals(False,typ.isSequencedType(),'isSequencedType');
  CheckEquals(True,typ.isDataType(),'isDataType');
  CheckNull(typ.getBaseType(),'getBaseType()');

  obj.AddType(s_URI_2,s_TYPE_2,[]);
  typ2 := typLst.find(s_URI_2,s_TYPE_2);
  Check(nil <> typ2,'typLst.find(s_URI_2,s_TYPE_2)');
  Check(nil <> obj.getTypes().find(s_URI_2,s_TYPE_2),'getTypes().find(s_URI_2,s_TYPE_2)');
  Check(typLst=obj.getTypes(),'typLst = obj.getTypes()');
  CheckEquals(s_URI_2,typ2.getURI());
  CheckEquals(s_TYPE_2,typ2.getName());
  CheckEquals(True,typ2.isDataObjectType(), 'isDataObjectType');
  CheckEquals(False,typ2.isSequencedType(),'isSequencedType');
  CheckEquals(False,typ2.isDataType(),'isDataType');
  CheckNull(typ2.getBaseType(),'getBaseType()');

  typ := typLst.find(s_URI_1,s_TYPE_1);
  Check(nil <> typ,'typLst.find(s_URI_1,s_TYPE_1)');
end;

procedure TSDOBaseDataFactory_Test.check_circular_dependy();
const
  s_uri = 'uri';
var
  locFac : ISDODataFactory;
begin
  // Auto reference should be allowed!
  locFac := Create_Factory();
  locFac.AddType(s_uri,'A',[]);
    locFac.addProperty(s_uri,'A','p_circular',s_uri,'A',[]);

  locFac.createNew(s_uri,'A');
end;

procedure TSDOBaseDataFactory_Test.check_circular_dependy_1();
const
  s_uri = 'uri';
var
  locFac : ISDODataFactory;
  ok : Boolean;
  locTypeX : ISDOObjectType;
begin  
  { Auto reference should be allowed!
  locFac := Create_Factory();
  locFac.AddType(s_uri,'A',[]);
  locFac.AddType(s_uri,'B',[]);

  locFac.addProperty(s_uri,'A','p_AB',s_uri,'B',[]);
    locFac.addProperty(s_uri,'B','p_BB',s_uri,'B',[]);
      ok := False;
      try
        locFac.createNew(s_uri,'A');
      except
        on e : ESDOCircularDependencyTypeException do begin
          ok := True;
        end;
      end;
      Check(ok, 'Succefully create a object who''s type tree contains circular dependency.');
      locTypeX := locFac.getType(s_uri,'B') as ISDOObjectType;
      locTypeX.DropProperty(locTypeX.getProperty('p_BB'));
      locFac := nil;}

  locFac := Create_Factory();
  locFac.AddType(s_uri,'A',[]);
  locFac.AddType(s_uri,'B',[]);

  locFac.addProperty(s_uri,'A','p_AB',s_uri,'B',[]);
    locFac.addProperty(s_uri,'B','p_BA',s_uri,'A',[]);
      ok := False;
      try
        locFac.createNew(s_uri,'A');
      except
        on e : ESDOCircularDependencyTypeException do begin
          ok := True;
        end;
      end;
      Check(ok, 'Succefully create a object who''s type tree contains circular dependency.');
      locTypeX := locFac.getType(s_uri,'B') as ISDOObjectType;
      locTypeX.DropProperty(locTypeX.getProperty('p_BA'));
      locFac := nil;
end;

procedure TSDOBaseDataFactory_Test.check_circular_dependy_2();
const
  s_uri = 'uri';
var
  locFac : ISDODataFactory;
  ok : Boolean;
  locTypeX : ISDOObjectType;
begin
  locFac := Create_Factory();
  locFac.AddType(s_uri,'A',[]);
  locFac.AddType(s_uri,'B',[]);
  locFac.AddType(s_uri,'C',[]);
    locFac.addProperty(s_uri,'A','p_AB',s_uri,'B',[]);
      locFac.addProperty(s_uri,'B','p_BC',s_uri,'C',[]);
        locFac.addProperty(s_uri,'C','p_CA',s_uri,'A',[]);
        ok := False;
        try
          locFac.createNew(s_uri,'A');
        except
          on e : ESDOCircularDependencyTypeException do begin
            ok := True;
          end;
        end;
        Check(ok, 'Succefully create a object who''s type tree contains circular dependency.');
        locTypeX := locFac.getType(s_uri,'C') as ISDOObjectType;
        locTypeX.DropProperty(locTypeX.getProperty('p_CA'));
        locFac := nil;

  locFac := Create_Factory();
  locFac.AddType(s_uri,'A',[]);
  locFac.AddType(s_uri,'B',[]);
  locFac.AddType(s_uri,'C',[]);
    locFac.addProperty(s_uri,'A','p_AB',s_uri,'B',[]);
      locFac.addProperty(s_uri,'B','p_BC',s_uri,'C',[]);
        locFac.addProperty(s_uri,'C','p_CB',s_uri,'B',[]);
        ok := False;
        try
          locFac.createNew(s_uri,'A');
        except
          on e : ESDOCircularDependencyTypeException do begin
            ok := True;
          end;
        end;
        Check(ok, 'Succefully create a object who''s type tree contains circular dependency.');
        locTypeX := locFac.getType(s_uri,'C') as ISDOObjectType;
        locTypeX.DropProperty(locTypeX.getProperty('p_CB'));
        locFac := nil;
end;

procedure TSDOBaseDataFactory_Test.check_circular_dependy_3();
const
  s_uri = 'uri';
var
  locFac : ISDODataFactory;
  ok : Boolean;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'X',[]);
    locFac.AddType(s_uri,'A',[]);
      locFac.setBaseType(s_uri,'A',s_uri,'X');
    locFac.AddType(s_uri,'B',[]);
    locFac.AddType(s_uri,'C',[]);
      locFac.addProperty(s_uri,'A','p_ab',s_uri,'B',[pfIsContainment]);
      locFac.addProperty(s_uri,'B','p_bc',s_uri,'C',[pfIsContainment]);
      locFac.addProperty(s_uri,'C','p_ca_circular',s_uri,'X',[pfIsContainment]);

      ok := False;
      try
        locFac.createNew(s_uri,'A');
      except
        on  e : ESDOCircularDependencyTypeException do
          ok := True
      end;
      Check(ok, 'Succefully create a object who''s type tree contains circular dependency.');
end;

procedure TSDOBaseDataFactory_Test.check_circular_dependy_4();
const
  s_uri = 'uri';
var
  locFac : ISDODataFactory;
  ok : Boolean;
  locTypeX : ISDOObjectType;
begin
  locFac := Create_Factory();
  locFac.AddType(s_uri,'X',[]);
    locFac.AddType(s_uri,'A',[]);
    locFac.setBaseType(s_uri,'A',s_uri,'X');
      locFac.addProperty(s_uri,'A','p_circular',s_uri,'X',[]);

  ok := False;
  try
    locFac.createNew(s_uri,'A');
  except
    on e : ESDOCircularDependencyTypeException do begin
      ok := True;
    end;
  end;
  Check(ok, 'Succefully create a object who''s type contains circular dependency.');
  locTypeX := locFac.getType(s_uri,'A') as ISDOObjectType;
  locTypeX.DropProperty(locTypeX.getProperty('p_circular'));
end;

procedure TSDOBaseDataFactory_Test.createNew();
const s_URI_1  = 'uri:1'; s_TYPE_1 = 'type1'; s_TYPE_2 = 'type2';
var
  obj : ISDODataFactory;
  typ : ISDOType;
  dataObj : ISDODataObject;
  ok : Boolean;
begin
  obj := Create_Factory();

  obj.AddType(s_URI_1,s_TYPE_1,[]);
  typ := obj.getType(s_URI_1,s_TYPE_1);
  obj.addProperty(typ,'propboolean',sdo_namespace,'Boolean',[]);
  obj.addProperty(typ,'propinteger',sdo_namespace,'Integer',[]);

  dataObj := obj.createNew(typ);
  Check(nil <> dataObj);
  Check(typ=dataObj.getType(),'getType');
  Check(nil <> dataObj.getProperty('propboolean'),'getProperty');
  Check(nil <> dataObj.getProperty('propboolean').getType(),'getProperty().getType');
  Check(dataObj.getProperty('propboolean').getType()=obj.getType(sdo_namespace,'Boolean'));
  Check(nil <> dataObj.getProperty('propinteger'),'getProperty');
  Check(nil <> dataObj.getProperty('propinteger').getType(),'getProperty().getType');
  Check(dataObj.getProperty('propinteger').getType()=obj.getType(sdo_namespace,'Integer'));

  dataObj := obj.createNew(s_URI_1,s_TYPE_1);
  Check(nil <> dataObj);
  Check(typ=dataObj.getType(),'getType');
  Check(nil <> dataObj.getProperty('propboolean'),'getProperty');
  Check(nil <> dataObj.getProperty('propboolean').getType(),'getProperty().getType');
  Check(dataObj.getProperty('propboolean').getType()=obj.getType(sdo_namespace,'Boolean'));
  Check(nil <> dataObj.getProperty('propinteger'),'getProperty');
  Check(nil <> dataObj.getProperty('propinteger').getType(),'getProperty().getType');
  Check(dataObj.getProperty('propinteger').getType()=obj.getType(sdo_namespace,'Integer'));

  obj.AddType(s_URI_1,s_TYPE_2,[tfIsAbstract]);
  typ := obj.getType(s_URI_1,s_TYPE_2);
  obj.addProperty(typ,'propboolean',sdo_namespace,'Boolean',[]);
  obj.addProperty(typ,'propinteger',sdo_namespace,'Integer',[]);
  ok := False;
  try
    dataObj := obj.createNew(s_URI_1,s_TYPE_2);
  except
    on e : ESDOAbstractTypeException do begin
      ok := True;
    end;
  end;
  Check(ok, 'Create abtract type instance.');
end;

function TSDOBaseDataFactory_Test.Create_Factory() : ISDODataFactory;
begin
  Result := TSDOBaseDataFactory.Create();
end;

class function TSDOBaseDataFactory_Test.GetTestSuitePath() : string;
begin
  Result := 'Metadata';
end;

procedure TSDOBaseDataFactory_Test.inherits();
const
  s_uri = 'u';
var
  locFac : ISDODataFactory;
  locTypeX : ISDOTypeEx;
begin
  locFac := Create_Factory();
  locFac.AddType(s_uri,'a',[]);
    locFac.AddType(s_uri,'a_b_1',[]);
      locFac.setBaseType(s_uri,'a_b_1',s_uri,'a');
    locFac.AddType(s_uri,'a_b_2',[]);
      locFac.setBaseType(s_uri,'a_b_2',s_uri,'a');
      locFac.AddType(s_uri,'a_b_2_c',[]);
        locFac.setBaseType(s_uri,'a_b_2_c',s_uri,'a_b_2');

  locFac.AddType(s_uri,'b',[]);
    locFac.AddType(s_uri,'b_c',[]);
      locFac.setBaseType(s_uri,'b_c',s_uri,'b');
  locFac.AddType(s_uri,'c',[]);

  locTypeX := locFac.getType(s_uri,'a') as ISDOTypeEx;
  Check(locTypeX.inherits(locTypeX));

  locTypeX := locFac.getType(s_uri,'a_b_1') as ISDOTypeEx;
  Check(locTypeX.inherits(locTypeX));
  Check(locTypeX.inherits(locFac.getType(s_uri,'a')));

  locTypeX := locFac.getType(s_uri,'a_b_2') as ISDOTypeEx;
  Check(locTypeX.inherits(locTypeX));
  Check(locTypeX.inherits(locFac.getType(s_uri,'a')));
    locTypeX := locFac.getType(s_uri,'a_b_2_c') as ISDOTypeEx;
    Check(locTypeX.inherits(locFac.getType(s_uri,'a_b_2')));
    Check(locTypeX.inherits(locFac.getType(s_uri,'a')));

  locTypeX := locFac.getType(s_uri,'b_c') as ISDOTypeEx;
  Check(locTypeX.inherits(locTypeX));
  Check(locTypeX.inherits(locFac.getType(s_uri,'b')));

  locTypeX := locFac.getType(s_uri,'a') as ISDOTypeEx;
    Check(not locTypeX.inherits(locFac.getType(s_uri,'a_b_1')));
    Check(not locTypeX.inherits(locFac.getType(s_uri,'a_b_2')));
    Check(not locTypeX.inherits(locFac.getType(s_uri,'a_b_2_c')));
    Check(not locTypeX.inherits(locFac.getType(s_uri,'b')));
    Check(not locTypeX.inherits(locFac.getType(s_uri,'b_c')));
    Check(not locTypeX.inherits(locFac.getType(s_uri,'c')));

  locTypeX := locFac.getType(s_uri,'b') as ISDOTypeEx;
    Check(not locTypeX.inherits(locFac.getType(s_uri,'a_b_1')));
    Check(not locTypeX.inherits(locFac.getType(s_uri,'a_b_2')));
    Check(not locTypeX.inherits(locFac.getType(s_uri,'a_b_2_c')));
    Check(not locTypeX.inherits(locFac.getType(s_uri,'b_c')));
    Check(not locTypeX.inherits(locFac.getType(s_uri,'c')));

  locTypeX := locFac.getType(s_uri,'c') as ISDOTypeEx;
    Check(not locTypeX.inherits(locFac.getType(s_uri,'a_b_1')));
    Check(not locTypeX.inherits(locFac.getType(s_uri,'a_b_2')));
    Check(not locTypeX.inherits(locFac.getType(s_uri,'a_b_2_c')));
    Check(not locTypeX.inherits(locFac.getType(s_uri,'b')));
    Check(not locTypeX.inherits(locFac.getType(s_uri,'b_c')));
end;

procedure TSDOBaseDataFactory_Test.inherits_root();
const
  s_uri = 'u';
var
  locFac : ISDODataFactory;
  locTypeX : ISDOTypeEx;
begin
  locFac := Create_Factory();
  locFac.AddType(s_uri,'a',[]);
  locTypeX := locFac.getType(s_uri,'a') as ISDOTypeEx;

  Check(
    locTypeX.inherits(locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])),
    Format('Every data object inherits from the base DataObject( uri = "%s", name = "%s" )',[sdo_namespace,SDOTypeDefaultTypeNames[ObjectType]])
  );
end;

procedure TSDOBaseDataFactory_Test.setAlias();
const s_URI_1  = 'uri:1'; s_TYPE_1  = 'type1';
var
  df : ISDODataFactory;
  obj, obj2 : ISDOType;
  ok : Boolean;
begin
  df := Create_Factory();
  df.AddType(s_URI_1,s_TYPE_1,[]);
  obj := df.getType(s_URI_1,s_TYPE_1);
  CheckEquals(0,obj.getAliasCount(),'getAliasCount');

  ok := False;
  try
    obj.getAlias(1);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  Check(ok,'getAlias(1) : ESDOIndexOutOfRangeException expected.');

  df.setAlias(s_URI_1,s_TYPE_1,'a');
  CheckEquals('a',obj.getAlias(0),'getAlias');
  CheckEquals(1,obj.getAliasCount(),'getAliasCount');
  ok := False;
  try
    obj.getAlias(1);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  Check(ok,'getAlias(1) : ESDOIndexOutOfRangeException expected.');

  obj2 := df.getType(s_URI_1,'a');
  Check(obj = obj2);

  CheckEquals('a',obj.getAlias(0),'getAlias');
  CheckEquals(1,obj.getAliasCount(),'getAliasCount');

  df.setAlias(obj.getURI(),obj.getName(),'b');
  CheckEquals(2,obj.getAliasCount(),'getAliasCount');
  CheckEquals('a',obj.getAlias(0),'getAlias');
  CheckEquals('b',obj.getAlias(1),'getAlias');
  ok := False;
  try
    obj.getAlias(2);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  Check(ok,'getAlias(2) : ESDOIndexOutOfRangeException expected.');
end;

procedure TSDOBaseDataFactory_Test.setBaseType();
const s_URI_1  = 'uri:1'; s_URI_2  = 'uri:2'; s_TYPE_3 = 'type3';
      s_TYPE_1 = 'type1'; s_TYPE_2 = 'type2';
var
  obj : ISDODataFactory;
  typ, typ2, typ3 : ISDOType;
  typLst : ISDOTypeList;
begin
  obj := Create_Factory();
  typLst := obj.getTypes();

  obj.AddType(s_URI_1,s_TYPE_1,[tfIsDataType]);
  typ := obj.getType(s_URI_1,s_TYPE_1);
  CheckNull(typ.getBaseType(),'getBaseType()');

  Check(nil <> typLst.find(sdo_namespace,'Boolean'),'find(sdo_namespace,''Boolean'')');
  obj.setBaseType(typ,obj.getType(sdo_namespace,'Boolean'));
  Check(nil <> typ.getBaseType(),'getBaseType()');
  Check(obj.getType(sdo_namespace,'Boolean')=typ.getBaseType());
  CheckEquals(True,obj.getType(sdo_namespace,'Boolean').getTypeEnum() = typ.getBaseType().getTypeEnum(), 'getTypeEnum()');

  obj.setBaseType(typ,nil);
  CheckNull(typ.getBaseType(),'getBaseType()');

  obj.AddType(s_URI_2,s_TYPE_2,[]);
  typ2 := typLst.find(s_URI_2,s_TYPE_2);
  CheckNull(typ2.getBaseType(),'getBaseType()');

  obj.AddType(s_URI_2,s_TYPE_3,[]);
  typ3 := typLst.find(s_URI_2,s_TYPE_3);
  CheckNull(typ3.getBaseType(),'getBaseType()');

  obj.setBaseType(typ2,typ3);
  Check(nil <> typ2.getBaseType(),'getBaseType()');
  Check(typ3=typ2.getBaseType());

  obj.setBaseType(typ2,nil);
  CheckNull(typ2.getBaseType(),'getBaseType()');

  obj.setBaseType(s_URI_2,s_TYPE_2,s_URI_2,s_TYPE_3);
  Check(nil <> typ2.getBaseType(),'getBaseType()');
  Check(typ3=typ2.getBaseType());
end;


procedure TSDOBaseDataFactory_Test.type_equals();
const
  s_uri_1 = 'u1';
  s_uri_2 = 'u2';
var
  locFac, locFac2 : ISDODataFactory;
begin
  locFac := Create_Factory();
  locFac.AddType(s_uri_1,'a',[]);
  locFac.AddType(s_uri_1,'b',[]);
    Check(not locFac.getType(s_uri_1,'a').equals(locFac.getType(s_uri_1,'b')));

  locFac := Create_Factory();
  locFac.AddType(s_uri_1,'a',[]);
  locFac.AddType(s_uri_2,'a',[]);
    Check(not locFac.getType(s_uri_1,'a').equals(locFac.getType(s_uri_2,'a')));

  locFac := Create_Factory();
  locFac2 := Create_Factory();
  locFac.AddType(s_uri_1,'a',[]);
    locFac.setAlias(s_uri_1,'a','b');
  locFac2.AddType(s_uri_1,'b',[]);
    Check(locFac.getType(s_uri_1,'a').equals(locFac2.getType(s_uri_1,'b')));

  locFac := Create_Factory();
  locFac2 := Create_Factory();
  locFac.AddType(s_uri_1,'a',[]);
  locFac2.AddType(s_uri_1,'b',[]);
    locFac2.setAlias(s_uri_1,'b','a');
    Check(locFac.getType(s_uri_1,'a').equals(locFac2.getType(s_uri_1,'b')));
end;

{ TTSDOChangeSummaryType_Test }

procedure TTSDOChangeSummaryType_Test.Alias_procs();
var
  obj : ISDOTypeEx;
  ok : Boolean;
  nativeAliasList : TStringList;
  nativeAliasCount, i, j : Integer;
  s : string;
begin
  nativeAliasList := TStringList.Create();
  try
    obj := Create_Type() as ISDOTypeEx;

    nativeAliasCount := GetPredefinedAlias(nativeAliasList);
    CheckEquals(nativeAliasCount,obj.getAliasCount(),'getAliasCount');
    if ( nativeAliasCount > 0 ) then begin
      for i := 0 to Pred(nativeAliasCount) do begin
        s := nativeAliasList[i];
        ok := False;
        for j := 0 to Pred(obj.getAliasCount()) do begin
          if  ( s = obj.getAlias(j) ) then begin
            ok := True;
            Break;
          end;
        end;
        CheckEquals(True,ok,Format('Unable to find predefined alias : "%s"',[s]));
      end;
    end;

    ok := False;
    try
      obj.getAlias(nativeAliasCount);
    except
      on e : ESDOIndexOutOfRangeException do begin
        ok := True;
      end;
    end;
    Check(ok,Format('getAlias(%d) : ESDOIndexOutOfRangeException expected.',[nativeAliasCount]));

    obj.SetAlias('aaaaaaaaaaaa');
    CheckEquals('aaaaaaaaaaaa',obj.getAlias(0),'getAlias'); // because it is sorted !!!!
    CheckEquals(nativeAliasCount + 1,obj.getAliasCount(),'getAliasCount');
    ok := False;
    try
      obj.getAlias(nativeAliasCount + 1);
    except
      on e : ESDOIndexOutOfRangeException do begin
        ok := True;
      end;
    end;
    Check(ok,Format('getAlias(%d) : ESDOIndexOutOfRangeException expected.',[nativeAliasCount + 1]));

    obj.SetAlias('aaaaaaaaaaaa');
    CheckEquals('aaaaaaaaaaaa',obj.getAlias(0),'getAlias');
    CheckEquals(nativeAliasCount + 1,obj.getAliasCount(),'getAliasCount');

    obj.SetAlias('aaaaaaaaaaaab');
    CheckEquals(nativeAliasCount + 2,obj.getAliasCount(),'getAliasCount');
    CheckEquals('aaaaaaaaaaaa',obj.getAlias(0),'getAlias');
    CheckEquals('aaaaaaaaaaaab',obj.getAlias(1),'getAlias');
    ok := False;
    try
      obj.getAlias(nativeAliasCount + 2);
    except
      on e : ESDOIndexOutOfRangeException do begin
        ok := True;
      end;
    end;
    Check(ok,Format('getAlias(%d) : ESDOIndexOutOfRangeException expected.',[nativeAliasCount + 2]));
  finally
    FreeAndNil(nativeAliasList);
  end;
end;

function TTSDOChangeSummaryType_Test.Create_Type() : ISDOType;
begin
  Result := TSDOChangeSummaryType.Create(get_datafactory()) as ISDOType;
end;

procedure TTSDOChangeSummaryType_Test.equals();
begin
  CheckEquals(True,Create_Type().equals(Create_Type()));
end;

procedure TTSDOChangeSummaryType_Test.getBaseType();
var
  t, bt : ISDOType;
begin
  t := Create_Type();
  bt := t.getBaseType();
  CheckNull(bt);
end;

class function TTSDOChangeSummaryType_Test.GetPredefinedAlias(AList: TStrings): Integer;
begin
  AList.Clear();
  Result := AList.Count;
end;

procedure TTSDOChangeSummaryType_Test.getPropertyIndex();
var
  ok : Boolean;
  t : ISDOType;
begin
  ok := False;
  try
    t := Create_Type();
    t.getPropertyIndex('a');
  except
    on e : ESDOUnsupportedOperationException do begin
      ok := True;
    end;
  end;
  Check(ok);
end;

class function TTSDOChangeSummaryType_Test.GetTestSuitePath() : string;
begin
  Result := 'Metadata';
end;

procedure TTSDOChangeSummaryType_Test.getTypeEnum();
begin
  CheckEquals(Ord(ChangeSummaryType),Ord(Create_Type().getTypeEnum()));
end;

procedure TTSDOChangeSummaryType_Test.getURI();
begin
  CheckEquals(sdo_namespace,Create_Type().getURI());
end;

function TTSDOChangeSummaryType_Test.get_datafactory: ISDODataFactory;
begin
  if ( FDataFactory = nil ) then
    FDataFactory := TSDODataFactory.Create() as ISDODataFactory;
  Result := FDataFactory;
end;

procedure TTSDOChangeSummaryType_Test.isAbstractType();
begin
  CheckEquals(False,Create_Type().isAbstractType());
end;

procedure TTSDOChangeSummaryType_Test.isDataObjectType();
begin
  CheckEquals(False,Create_Type().isDataObjectType());
end;

procedure TTSDOChangeSummaryType_Test.isDataType();
begin
  CheckEquals(True,Create_Type().isDataType());
end;

procedure TTSDOChangeSummaryType_Test.isOpenType();
begin
  CheckEquals(False,Create_Type().isOpenType());
end;

procedure TTSDOChangeSummaryType_Test.isSequencedType();
begin
  CheckEquals(False,Create_Type().isSequencedType());
end;

procedure TTSDOChangeSummaryType_Test.SetUp;
begin
  inherited;
  FDataFactory := nil;
end;

procedure TTSDOChangeSummaryType_Test.TearDown;
begin
  FDataFactory := nil;
  inherited;
end;

procedure TTSDOChangeSummaryType_Test.test_getName();
begin
  CheckEquals(SDOTypeDefaultTypeNames[ChangeSummaryType],Create_Type().getName());
end;

procedure TTSDOChangeSummaryType_Test.Used();
var
  obj : ISDOTypeEx;
  ok : Boolean;
begin
  obj := Create_Type() as ISDOTypeEx;
  CheckEquals(False,obj.isUsed());

  obj.setUsedFlag(False);
    CheckEquals(False,obj.isUsed());
  obj.setUsedFlag(True);
    CheckEquals(True,obj.isUsed());
  obj.setUsedFlag(False);
    CheckEquals(False,obj.isUsed());

  obj.setUsedFlag(True);
    CheckEquals(True,obj.isUsed());
    ok := False;
    try
      obj.setAlias('azerty_qwerty');
    except
      on e : ESDOUnsupportedOperationException do begin
        ok := True;
      end;
    end;
    Check(ok,'Modify an used type.');
end;

{$IFDEF HAS_SDO_SHORT}
{ TSDOShortType_Test }

function TSDOShortType_Test.Create_Type() : ISDOType;
begin
  Result := TSDOShortType.Create(get_datafactory()) as ISDOType;
end;
{$ENDIF HAS_SDO_SHORT}

initialization
  RegisterTest(TSDOBooleanType_Test.GetTestSuitePath(),TSDOBooleanType_Test.Suite);
  RegisterTest(TSDOByteType_Test.GetTestSuitePath(),TSDOByteType_Test.Suite);
{$IFDEF HAS_SDO_BYTES}
  RegisterTest(TSDOBytesType_Test.GetTestSuitePath(),TSDOBytesType_Test.Suite);
{$ENDIF HAS_SDO_BYTES}
  RegisterTest(TTSDOChangeSummaryType_Test.GetTestSuitePath(),TTSDOChangeSummaryType_Test.Suite);
{$IFDEF HAS_SDO_CHAR}
  RegisterTest(TSDOCharacterType_Test.GetTestSuitePath(),TSDOCharacterType_Test.Suite);
{$ENDIF HAS_SDO_CHAR}
  RegisterTest(TSDODateTimeType_Test.GetTestSuitePath(),TSDODateTimeType_Test.Suite);
{$IFDEF HAS_SDO_DOUBLE}
  RegisterTest(TSDODoubleType_Test.GetTestSuitePath(),TSDODoubleType_Test.Suite);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
  RegisterTest(TSDOFloatType_Test.GetTestSuitePath(),TSDOFloatType_Test.Suite);
{$ENDIF HAS_SDO_FLOAT}
  RegisterTest(TSDOIntegerType_Test.GetTestSuitePath(),TSDOIntegerType_Test.Suite);
{$IFDEF HAS_SDO_LONG}
  RegisterTest(TSDOLongType_Test.GetTestSuitePath(),TSDOLongType_Test.Suite);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
  RegisterTest(TSDOShortType_Test.GetTestSuitePath(),TSDOShortType_Test.Suite);
{$ENDIF HAS_SDO_SHORT}
  RegisterTest(TSDOStringType_Test.GetTestSuitePath(),TSDOStringType_Test.Suite);
  RegisterTest(TSDOObjectType_Test.GetTestSuitePath(),TSDOObjectType_Test.Suite);
  RegisterTest(TSDOUserDefinedSimpleType_Test.GetTestSuitePath(),TSDOUserDefinedSimpleType_Test.Suite);
  RegisterTest(TSDOBaseDataFactory_Test.GetTestSuitePath(),TSDOBaseDataFactory_Test.Suite);

end.
