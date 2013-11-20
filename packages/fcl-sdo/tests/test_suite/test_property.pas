{$INCLUDE sdo_global.inc}
unit test_property;

interface
uses SysUtils
{$IFDEF FPC}
  ,fpcunit, testutils, testregistry
{$ENDIF}
{$IFNDEF FPC}
  ,TestFrameWork
{$ENDIF}
  ,test_suite_utils, sdo, sdo_type, sdo_types;

type

  TSDOPropertyAsbtract_Test = class(TWstBaseTest)
  protected
    class function create_obj(
      const AName  : string;
      const AType  : ISDOType;
      const AMany  : Boolean;
      const AContained : Boolean;
      const AContainingType : ISDOType;
      const AReadOnly : Boolean;
      const AIsAttribute : Boolean
    ) : ISDOProperty;virtual;abstract;
  public
    procedure CheckEquals(expected, actual: TSDODate; msg: string = ''; const AStrict : Boolean = True); overload;
  published
    procedure test_create();

    procedure setDefault_boolean();
    procedure setDefault_byte();
    procedure setDefault_integer();
    procedure setDefault_string();
    procedure setDefault_object();
    procedure setDefault_date();

{$IFDEF HAS_SDO_BYTES}
    procedure setDefault_bytes();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure setDefault_char();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure setDefault_currency();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure setDefault_double();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure setDefault_float();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure setDefault_long();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure setDefault_short();
{$ENDIF HAS_SDO_SHORT}
  end;

  TSDOProperty_Test = class(TSDOPropertyAsbtract_Test)
  protected
    class function create_obj(
      const AName  : string;
      const AType  : ISDOType;
      const AMany  : Boolean;
      const AContained : Boolean;
      const AContainingType : ISDOType;
      const AReadOnly : Boolean;
      const AIsAttribute : Boolean
    ) : ISDOProperty;override;
  end;

implementation

uses sdo_datafactory, Math, DateUtils, sdo_date_utils;

const
  s_uri = 'test-uri';
  s_type_object_A = 'Object_A'; s_type_object_B = 'Object_B';

{ TSDOPropertyAsbtract_Test }

procedure TSDOPropertyAsbtract_Test.CheckEquals(expected, actual: TSDODate;
  msg: string; const AStrict: Boolean
);
var
  e, a : TDateTime;
  e_y, e_m, e_d, e_h, e_mn, e_ss, e_ms : Word;
  a_y, a_m, a_d, a_h, a_mn, a_ss, a_ms : Word;
begin
  if AStrict then begin
    Check(CompareMem(@expected, @actual, SizeOf(TSDODate)), msg);
  end else begin
    e := NormalizeToUTC(expected);
    a := NormalizeToUTC(actual);
    DecodeDateTime(e, e_y, e_m, e_d, e_h, e_mn, e_ss, e_ms);
    DecodeDateTime(a, a_y, a_m, a_d, a_h, a_mn, a_ss, a_ms);
    CheckEquals(e_y,a_y,msg);
    CheckEquals(e_m,a_m,msg);
    CheckEquals(e_d,a_d,msg);
    CheckEquals(e_h,a_h,msg);
    CheckEquals(e_mn,a_mn,msg);
    CheckEquals(e_ss,a_ss,msg);
    CheckEquals(e_ms,a_ms,msg);
  end;
end;

procedure TSDOPropertyAsbtract_Test.setDefault_boolean();
var
  locFac : ISDODataFactory;
  locObj : ISDOProperty;
  tmpVal : TSDOBoolean;
  ok : Boolean;
begin
  Randomize();
  locFac := TSDODataFactory.Create() as ISDODataFactory;
    locFac.AddType(s_uri,s_type_object_A,[]);
    locFac.AddType(s_uri,s_type_object_B,[]);

  locObj := create_obj(
              (*Name*)            'p_bool',
              (*Type*)            locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[BooleanType]),
              (* Many *)          False,
              (*Contained*)       False,
              (*ContainingType*)  locFac.getType(s_uri,s_type_object_A),
              (*ReadOnly*)        False,
              (*Attribute*)       True
            );
  CheckEquals(False, locObj.isDefaulted());
  CheckEquals(False, locObj.getBooleanDefault());
  CheckEquals(0, locObj.getIntegerDefault());
  CheckEquals('', locObj.getStringDefault());

  tmpVal := False;
  locObj.setDefault(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getBooleanDefault());
  tmpVal := True;
  locObj.setDefault(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getBooleanDefault());

  locFac.createNew(s_uri,s_type_object_A);
    ok := False;
    try
      locObj.setDefault(False);
    except
      on e : ESDOUnsupportedOperationException do
        ok := True;
    end;
    Check(ok, 'Once an instance has been created, the metadata becomes read-only.');
end;

procedure TSDOPropertyAsbtract_Test.setDefault_byte();
const
  PROP_TYPE = ByteType;
var
  locFac : ISDODataFactory;
  locObj : ISDOProperty;
  tmpVal : TSDOByte;
  ok : Boolean;
begin
  Randomize();
  locFac := TSDODataFactory.Create() as ISDODataFactory;
    locFac.AddType(s_uri,s_type_object_A,[]);
    locFac.AddType(s_uri,s_type_object_B,[]);

  locObj := create_obj(
              (*Name*)            'p_sample',
              (*Type*)            locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE]),
              (* Many *)          False,
              (*Contained*)       False,
              (*ContainingType*)  locFac.getType(s_uri,s_type_object_A),
              (*ReadOnly*)        False,
              (*Attribute*)       True
            );
  CheckEquals(False, locObj.isDefaulted());
  CheckEquals(False, locObj.getBooleanDefault());
  CheckEquals(0, locObj.getByteDefault());
  CheckEquals(0, locObj.getIntegerDefault());
  CheckEquals('', locObj.getStringDefault());

  tmpVal := RandomRange(Low(TSDOByte),High(TSDOByte));
  locObj.setDefault(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getByteDefault());
  tmpVal := RandomRange(Low(TSDOByte),High(TSDOByte));
  locObj.setDefault(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getByteDefault());

  locFac.createNew(s_uri,s_type_object_A);
    ok := False;
    try
      locObj.setDefault(TSDOByte(123));
    except
      on e : ESDOUnsupportedOperationException do
        ok := True;
    end;
    Check(ok, 'Once an instance has been created, the metadata becomes read-only.');
end;

procedure TSDOPropertyAsbtract_Test.setDefault_date();
var
  locFac : ISDODataFactory;
  locObj : ISDOProperty;
  tmpVal : TSDODate;
  ok : Boolean;
begin
  Randomize();
  locFac := TSDODataFactory.Create() as ISDODataFactory;
    locFac.AddType(s_uri,s_type_object_A,[]);
    locFac.AddType(s_uri,s_type_object_B,[]);

  locObj := create_obj(
              (*Name*)            'p_dt',
              (*Type*)            locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType]),
              (* Many *)          False,
              (*Contained*)       False,
              (*ContainingType*)  locFac.getType(s_uri,s_type_object_A),
              (*ReadOnly*)        False,
              (*Attribute*)       True
            );
  CheckEquals(False, locObj.isDefaulted());
  CheckEquals(False, locObj.getBooleanDefault());
  CheckEquals(0, locObj.getByteDefault());
  CheckEquals(0, locObj.getIntegerDefault());
  CheckEquals('', locObj.getStringDefault());
  CheckEquals(ZERO_DATE,locObj.getDateDefault());

  tmpVal := xsd_StrToDate('1976-10-12T23:34:56.7',xdkDateTime);
  locObj.setDefault(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getDateDefault());
  tmpVal := xsd_StrToDate('1987-11-12',xdkDateTime);
  locObj.setDefault(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getDateDefault());

  locFac.createNew(s_uri,s_type_object_A);
    ok := False;
    try
      locObj.setDefault(xsd_StrToDate('1987-11-12',xdkDateTime));
    except
      on e : ESDOUnsupportedOperationException do
        ok := True;
    end;
    Check(ok, 'Once an instance has been created, the metadata becomes read-only.');
end;

procedure TSDOPropertyAsbtract_Test.setDefault_integer();
var
  locFac : ISDODataFactory;
  locObj : ISDOProperty;
  tmpVal : TSDOInteger;
  ok : Boolean;
begin
  Randomize();
  locFac := TSDODataFactory.Create() as ISDODataFactory;
    locFac.AddType(s_uri,s_type_object_A,[]);
    locFac.AddType(s_uri,s_type_object_B,[]);

  locObj := create_obj(
              (*Name*)            'p_int',
              (*Type*)            locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[IntegerType]),
              (* Many *)          False,
              (*Contained*)       False,
              (*ContainingType*)  locFac.getType(s_uri,s_type_object_A),
              (*ReadOnly*)        False,
              (*Attribute*)       True
            );
  CheckEquals(False, locObj.isDefaulted());
  CheckEquals(False, locObj.getBooleanDefault());
  CheckEquals(0, locObj.getIntegerDefault());
  CheckEquals('', locObj.getStringDefault());

  tmpVal := RandomRange(-12345,12345);
  locObj.setDefault(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getIntegerDefault());
  tmpVal := RandomRange(-12345,12345);
  locObj.setDefault(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getIntegerDefault());

  locFac.createNew(s_uri,s_type_object_A);
    ok := False;
    try
      locObj.setDefault(123);
    except
      on e : ESDOUnsupportedOperationException do
        ok := True;
    end;
    Check(ok, 'Once an instance has been created, the metadata becomes read-only.');
end;

{$IFDEF HAS_SDO_BYTES}
procedure TSDOPropertyAsbtract_Test.setDefault_bytes();
const
  PROP_TYPE = BytesType;
var
  locFac : ISDODataFactory;
  locObj : ISDOProperty;
  tmpVal : TSDOBytes;
  ok : Boolean;
begin
  Randomize();
  locFac := TSDODataFactory.Create() as ISDODataFactory;
    locFac.AddType(s_uri,s_type_object_A,[]);
    locFac.AddType(s_uri,s_type_object_B,[]);

  locObj := create_obj(
              (*Name*)            'p_sample',
              (*Type*)            locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE]),
              (* Many *)          False,
              (*Contained*)       False,
              (*ContainingType*)  locFac.getType(s_uri,s_type_object_A),
              (*ReadOnly*)        False,
              (*Attribute*)       True
            );
  CheckEquals(False, locObj.isDefaulted());
  CheckEquals(False, locObj.getBooleanDefault());
  CheckEquals(TSDOBytes(nil), locObj.getBytesDefault());
  CheckEquals('', locObj.getStringDefault());

  tmpVal := RandomBytes(100);
  locObj.setDefault(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getBytesDefault());
    CheckEquals(BytesToString(tmpVal), locObj.getStringDefault());
  tmpVal := RandomBytes(100);
  locObj.setDefault(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getBytesDefault());
    CheckEquals(BytesToString(tmpVal), locObj.getStringDefault());

  locFac.createNew(s_uri,s_type_object_A);
    ok := False;
    try
      locObj.setDefault(RandomBytes(100));
    except
      on e : ESDOUnsupportedOperationException do
        ok := True;
    end;
    Check(ok, 'Once an instance has been created, the metadata becomes read-only.');
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
procedure TSDOPropertyAsbtract_Test.setDefault_char();
const
  PROP_TYPE = CharacterType;
var
  locFac : ISDODataFactory;
  locObj : ISDOProperty;
  tmpVal : TSDOChar;
  ok : Boolean;
begin
  Randomize();
  locFac := TSDODataFactory.Create() as ISDODataFactory;
    locFac.AddType(s_uri,s_type_object_A,[]);
    locFac.AddType(s_uri,s_type_object_B,[]);

  locObj := create_obj(
              (*Name*)            'p_sample',
              (*Type*)            locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE]),
              (* Many *)          False,
              (*Contained*)       False,
              (*ContainingType*)  locFac.getType(s_uri,s_type_object_A),
              (*ReadOnly*)        False,
              (*Attribute*)       True
            );
  CheckEquals(False, locObj.isDefaulted());
  CheckEquals(False, locObj.getBooleanDefault());
  CheckEquals(0, locObj.getByteDefault());
{$IFDEF HAS_SDO_CHAR}
  CheckEquals(#0, locObj.getCharacterDefault());
{$ENDIF HAS_SDO_CHAR}
  CheckEquals(0, locObj.getIntegerDefault());
{$IFDEF HAS_SDO_LONG}
  CheckEquals(0, locObj.getLongDefault());
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
  CheckEquals(0, locObj.getShortDefault());
{$ENDIF HAS_SDO_SHORT}
  CheckEquals('', locObj.getStringDefault());

  tmpVal := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));
  locObj.setDefault(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getCharacterDefault());
  tmpVal := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));
  locObj.setDefault(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getCharacterDefault());

  locFac.createNew(s_uri,s_type_object_A);
    ok := False;
    try
      locObj.setDefault(TSDOChar('s'));
    except
      on e : ESDOUnsupportedOperationException do
        ok := True;
    end;
    Check(ok, 'Once an instance has been created, the metadata becomes read-only.');
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
procedure TSDOPropertyAsbtract_Test.setDefault_currency();
const
  PROP_TYPE = CurrencyType;
  VAL_1 : TSDOCurrency = 1238653223453;
var
  locFac : ISDODataFactory;
  locObj : ISDOProperty;
  tmpVal : TSDOCurrency;
  ok : Boolean;
begin
  Randomize();
  locFac := TSDODataFactory.Create() as ISDODataFactory;
    locFac.AddType(s_uri,s_type_object_A,[]);
    locFac.AddType(s_uri,s_type_object_B,[]);

  locObj := create_obj(
              (*Name*)            'p_sample',
              (*Type*)            locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE]),
              (* Many *)          False,
              (*Contained*)       False,
              (*ContainingType*)  locFac.getType(s_uri,s_type_object_A),
              (*ReadOnly*)        False,
              (*Attribute*)       True
            );
  CheckEquals(False, locObj.isDefaulted());
  CheckEquals(False, locObj.getBooleanDefault());
  CheckEquals(0, locObj.getByteDefault());
{$IFDEF HAS_SDO_CHAR}
  CheckEquals(#0, locObj.getCharacterDefault());
{$ENDIF HAS_SDO_CHAR}  
 
{$IFDEF HAS_SDO_DOUBLE}
  CheckEquals(0, locObj.getDoubleDefault());
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
  CheckEquals(0, locObj.getFloatDefault());
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}  
  CheckEquals(0, locObj.getLongDefault());
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
  CheckEquals(0, locObj.getShortDefault());
{$ENDIF HAS_SDO_SHORT}
  CheckEquals(0, locObj.getIntegerDefault()); 
  CheckEquals('', locObj.getStringDefault());
  
  CheckEquals(0, locObj.getCurrencyDefault());

  tmpVal := test_suite_utils.RandomRange(Low(Word),High(Word));
  locObj.setDefaultCurrency(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getCurrencyDefault());
  tmpVal := test_suite_utils.RandomRange(Low(Word),High(Word));
  locObj.setDefaultCurrency(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getCurrencyDefault());

  locFac.createNew(s_uri,s_type_object_A);
    ok := False;
    try
      locObj.setDefaultCurrency(VAL_1);
    except
      on e : ESDOUnsupportedOperationException do
        ok := True;
    end;
    Check(ok, 'Once an instance has been created, the metadata becomes read-only.');
end;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
procedure TSDOPropertyAsbtract_Test.setDefault_double();
const
  PROP_TYPE = DoubleType;
  VAL_1 : TSDODouble = 1238653223453;
var
  locFac : ISDODataFactory;
  locObj : ISDOProperty;
  tmpVal : TSDODouble;
  ok : Boolean;
begin
  Randomize();
  locFac := TSDODataFactory.Create() as ISDODataFactory;
    locFac.AddType(s_uri,s_type_object_A,[]);
    locFac.AddType(s_uri,s_type_object_B,[]);

  locObj := create_obj(
              (*Name*)            'p_sample',
              (*Type*)            locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE]),
              (* Many *)          False,
              (*Contained*)       False,
              (*ContainingType*)  locFac.getType(s_uri,s_type_object_A),
              (*ReadOnly*)        False,
              (*Attribute*)       True
            );
  CheckEquals(False, locObj.isDefaulted());
  CheckEquals(False, locObj.getBooleanDefault());
  CheckEquals(0, locObj.getByteDefault());
{$IFDEF HAS_SDO_CHAR}
  CheckEquals(#0, locObj.getCharacterDefault());
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
  CheckEquals(0, locObj.getCurrencyDefault());
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_FLOAT}
  CheckEquals(0, locObj.getFloatDefault());
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}  
  CheckEquals(0, locObj.getLongDefault());
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
  CheckEquals(0, locObj.getShortDefault());
{$ENDIF HAS_SDO_SHORT}
  CheckEquals(0, locObj.getIntegerDefault()); 
  CheckEquals('', locObj.getStringDefault());
  
  CheckEquals(0, locObj.getDoubleDefault());

  tmpVal := test_suite_utils.RandomRange(Low(Word),High(Word));
  locObj.setDefault(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getDoubleDefault());
  tmpVal := test_suite_utils.RandomRange(Low(Word),High(Word));
  locObj.setDefault(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getDoubleDefault());

  locFac.createNew(s_uri,s_type_object_A);
    ok := False;
    try
      locObj.setDefault(VAL_1);
    except
      on e : ESDOUnsupportedOperationException do
        ok := True;
    end;
    Check(ok, 'Once an instance has been created, the metadata becomes read-only.');
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
procedure TSDOPropertyAsbtract_Test.setDefault_float();
const
  PROP_TYPE = FloatType;
  VAL_1 : TSDOFloat = 1238653223453;
var
  locFac : ISDODataFactory;
  locObj : ISDOProperty;
  tmpVal : TSDOFloat;
  ok : Boolean;
begin
  Randomize();
  locFac := TSDODataFactory.Create() as ISDODataFactory;
    locFac.AddType(s_uri,s_type_object_A,[]);
    locFac.AddType(s_uri,s_type_object_B,[]);

  locObj := create_obj(
              (*Name*)            'p_sample',
              (*Type*)            locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE]),
              (* Many *)          False,
              (*Contained*)       False,
              (*ContainingType*)  locFac.getType(s_uri,s_type_object_A),
              (*ReadOnly*)        False,
              (*Attribute*)       True
            );
  CheckEquals(False, locObj.isDefaulted());
  CheckEquals(False, locObj.getBooleanDefault());
  CheckEquals(0, locObj.getByteDefault());
{$IFDEF HAS_SDO_CHAR}
  CheckEquals(#0, locObj.getCharacterDefault());
{$ENDIF HAS_SDO_CHAR}  
{$IFDEF HAS_SDO_CURRENCY}  
  CheckEquals(0, locObj.getCurrencyDefault());
{$ENDIF HAS_SDO_CURRENCY}  
{$IFDEF HAS_SDO_DOUBLE}
  CheckEquals(0, locObj.getDoubleDefault());
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_LONG}  
  CheckEquals(0, locObj.getLongDefault());
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
  CheckEquals(0, locObj.getShortDefault());
{$ENDIF HAS_SDO_SHORT}
  CheckEquals(0, locObj.getIntegerDefault()); 
  CheckEquals('', locObj.getStringDefault());
  
  CheckEquals(0, locObj.getFloatDefault());

  tmpVal := test_suite_utils.RandomRange(Low(Word),High(Word));
  locObj.setDefault(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getFloatDefault());
  tmpVal := test_suite_utils.RandomRange(Low(Word),High(Word));
  locObj.setDefault(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getFloatDefault());

  locFac.createNew(s_uri,s_type_object_A);
    ok := False;
    try
      locObj.setDefault(VAL_1);
    except
      on e : ESDOUnsupportedOperationException do
        ok := True;
    end;
    Check(ok, 'Once an instance has been created, the metadata becomes read-only.');
end;
{$ENDIF HAS_SDO_FLOAT}

{$IFDEF HAS_SDO_LONG}
procedure TSDOPropertyAsbtract_Test.setDefault_long();
const
  PROP_TYPE = LongType;
var
  locFac : ISDODataFactory;
  locObj : ISDOProperty;
  tmpVal : TSDOLong;
  ok : Boolean;
begin
  Randomize();
  locFac := TSDODataFactory.Create() as ISDODataFactory;
    locFac.AddType(s_uri,s_type_object_A,[]);
    locFac.AddType(s_uri,s_type_object_B,[]);

  locObj := create_obj(
              (*Name*)            'p_sample',
              (*Type*)            locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE]),
              (* Many *)          False,
              (*Contained*)       False,
              (*ContainingType*)  locFac.getType(s_uri,s_type_object_A),
              (*ReadOnly*)        False,
              (*Attribute*)       True
            );
  CheckEquals(False, locObj.isDefaulted());
  CheckEquals(False, locObj.getBooleanDefault());
  CheckEquals(0, locObj.getByteDefault());
{$IFDEF HAS_SDO_CHAR}
  CheckEquals(#0, locObj.getCharacterDefault());
{$ENDIF HAS_SDO_CHAR}    
  CheckEquals(0, locObj.getIntegerDefault());
{$IFDEF HAS_SDO_LONG}  
  CheckEquals(0, locObj.getLongDefault());
{$ENDIF HAS_SDO_LONG}  
{$IFDEF HAS_SDO_SHORT}  
  CheckEquals(0, locObj.getShortDefault());
{$ENDIF HAS_SDO_SHORT}  
  CheckEquals('', locObj.getStringDefault());

  tmpVal := test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong));
  locObj.setDefault(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getLongDefault());
  tmpVal := test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong));
  locObj.setDefault(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getLongDefault());

  locFac.createNew(s_uri,s_type_object_A);
    ok := False;
    try
      locObj.setDefault(TSDOLong(123865322345363636));
    except
      on e : ESDOUnsupportedOperationException do
        ok := True;
    end;
    Check(ok, 'Once an instance has been created, the metadata becomes read-only.');
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure TSDOPropertyAsbtract_Test.setDefault_short();
const
  PROP_TYPE = ShortType;
var
  locFac : ISDODataFactory;
  locObj : ISDOProperty;
  tmpVal : TSDOShort;
  ok : Boolean;
begin
  Randomize();
  locFac := TSDODataFactory.Create() as ISDODataFactory;
    locFac.AddType(s_uri,s_type_object_A,[]);
    locFac.AddType(s_uri,s_type_object_B,[]);

  locObj := create_obj(
              (*Name*)            'p_sample',
              (*Type*)            locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE]),
              (* Many *)          False,
              (*Contained*)       False,
              (*ContainingType*)  locFac.getType(s_uri,s_type_object_A),
              (*ReadOnly*)        False,
              (*Attribute*)       True
            );
  CheckEquals(False, locObj.isDefaulted());
  CheckEquals(False, locObj.getBooleanDefault());
  CheckEquals(0, locObj.getByteDefault());
{$IFDEF HAS_SDO_CHAR}
  CheckEquals(#0, locObj.getCharacterDefault());
{$ENDIF HAS_SDO_CHAR}    
  CheckEquals(0, locObj.getIntegerDefault());
{$IFDEF HAS_SDO_LONG}
  CheckEquals(0, locObj.getLongDefault());
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}  
  CheckEquals(0, locObj.getShortDefault());
{$ENDIF HAS_SDO_SHORT}  
  CheckEquals('', locObj.getStringDefault());

  tmpVal := RandomRange(Low(TSDOShort),High(TSDOShort));
  locObj.setDefault(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getShortDefault());
  tmpVal := RandomRange(Low(TSDOShort),High(TSDOShort));
  locObj.setDefault(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getShortDefault());

  locFac.createNew(s_uri,s_type_object_A);
    ok := False;
    try
      locObj.setDefault(TSDOShort(12386));
    except
      on e : ESDOUnsupportedOperationException do
        ok := True;
    end;
    Check(ok, 'Once an instance has been created, the metadata becomes read-only.');
end;
{$ENDIF HAS_SDO_SHORT}

procedure TSDOPropertyAsbtract_Test.setDefault_object;
var
  locFac : ISDODataFactory;
  locObj : ISDOProperty;
  ok : boolean;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
    locFac.AddType(s_uri,s_type_object_A,[]);
    locFac.AddType(s_uri,s_type_object_B,[]);

  locObj := create_obj(
              (*Name*)            'p_string',
              (*Type*)            locFac.getType(s_uri,s_type_object_B),
              (* Many *)          False,
              (*Contained*)       False,
              (*ContainingType*)  locFac.getType(s_uri,s_type_object_A),
              (*ReadOnly*)        False,
              (*Attribute*)       False
            );
  CheckEquals(False, locObj.isDefaulted());
  ok := False;
  try
    locObj.setDefault('asa');
  except
    on e : ESDOUnsupportedOperationException do
      ok := True;
  end;
  Check(ok, 'setDefault(string)');

  ok := False;
  try
    locObj.setDefault(True);
  except
    on e : ESDOUnsupportedOperationException do
      ok := True;
  end;
  Check(ok, 'setDefault(Boolean)');

  ok := False;
  try
    locObj.setDefault(1210);
  except
    on e : ESDOUnsupportedOperationException do
      ok := True;
  end;
  Check(ok, 'setDefault(Integer)');
end;

procedure TSDOPropertyAsbtract_Test.setDefault_string();
var
  locFac : ISDODataFactory;
  locObj : ISDOProperty;
  tmpVal : TSDOString;
  ok : Boolean;
begin
  Randomize();
  locFac := TSDODataFactory.Create() as ISDODataFactory;
    locFac.AddType(s_uri,s_type_object_A,[]);
    locFac.AddType(s_uri,s_type_object_B,[]);

  locObj := create_obj(
              (*Name*)            'p_string',
              (*Type*)            locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[StringType]),
              (* Many *)          False,
              (*Contained*)       False,
              (*ContainingType*)  locFac.getType(s_uri,s_type_object_A),
              (*ReadOnly*)        False,
              (*Attribute*)       True
            );
  CheckEquals(False, locObj.isDefaulted());
  CheckEquals(False, locObj.getBooleanDefault());
  CheckEquals(0, locObj.getIntegerDefault());
  CheckEquals('', locObj.getStringDefault());

  tmpVal := RandomString(RandomRange(1,1000));
  locObj.setDefault(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getStringDefault());
  tmpVal := RandomString(RandomRange(1,1000));
  locObj.setDefault(tmpVal);
    CheckEquals(True, locObj.isDefaulted());
    CheckEquals(tmpVal, locObj.getStringDefault());

  locFac.createNew(s_uri,s_type_object_A);
    ok := False;
    try
      locObj.setDefault('azerty');
    except
      on e : ESDOUnsupportedOperationException do
        ok := True;
    end;
    Check(ok, 'Once an instance has been created, the metadata becomes read-only.');
end;

procedure TSDOPropertyAsbtract_Test.test_create();
  procedure check_invalid_args(
    const AName  : string;
    const AType  : ISDOType;
    const AMany  : Boolean;
    const AContained : Boolean;
    const AContainingType : ISDOType;
    const AReadOnly : Boolean;
    const AIsAttribute : Boolean;

    const AMsg : string
  );
  var
    ok : Boolean;
  begin
    ok := False;
    try
      create_obj(AName,AType,AMany,AContained,AContainingType,AReadOnly,AIsAttribute);
    except
      on e : ESDOIllegalArgumentException do
        ok := True;
    end;
    Check(ok, AMsg);
  end;

var
  locFac : ISDODataFactory;
  locObj : ISDOProperty;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
    locFac.AddType(s_uri,s_type_object_A,[]);
    locFac.AddType(s_uri,s_type_object_B,[]);
  check_invalid_args(
    (*Name*)            '',
    (*Type*)            locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[StringType]),
    (* Many *)          False,
    (*Contained*)       False,
    (*ContainingType*)  locFac.getType(s_uri,s_type_object_A),
    (*ReadOnly*)        False,
    (*Attribute*)       True,
    'Invalid Name.'
  );

  check_invalid_args(
    (*Name*)            '1az',
    (*Type*)            locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[StringType]),
    (* Many *)          False,
    (*Contained*)       False,
    (*ContainingType*)  locFac.getType(s_uri,s_type_object_A),
    (*ReadOnly*)        False,
    (*Attribute*)       True,
    'Invalid Name.'
  );

  check_invalid_args(
    (*Name*)            'ab[1]',
    (*Type*)            locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[StringType]),
    (* Many *)          False,
    (*Contained*)       False,
    (*ContainingType*)  locFac.getType(s_uri,s_type_object_A),
    (*ReadOnly*)        False,
    (*Attribute*)       True,
    'Invalid Name.'
  );

  check_invalid_args(
    (*Name*)            'azerty',
    (*Type*)            nil,
    (* Many *)          False,
    (*Contained*)       False,
    (*ContainingType*)  locFac.getType(s_uri,s_type_object_A),
    (*ReadOnly*)        False,
    (*Attribute*)       True,
    'Invalid type.'
  );

  check_invalid_args(
    (*Name*)            'azerty',
    (*Type*)            locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[StringType]),
    (* Many *)          False,
    (*Contained*)       True,
    (*ContainingType*)  locFac.getType(s_uri,s_type_object_A),
    (*ReadOnly*)        False,
    (*Attribute*)       True,
    'Invalid Contained.'
  );

  check_invalid_args(
    (*Name*)            'azerty',
    (*Type*)            locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[StringType]),
    (* Many *)          False,
    (*Contained*)       True,
    (*ContainingType*)  locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[IntegerType]),
    (*ReadOnly*)        False,
    (*Attribute*)       True,
    'Invalid containingType : The containing type must be a DataObject.'
  );

  locObj := create_obj(
              (*Name*)            'p_string',
              (*Type*)            locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[StringType]),
              (* Many *)          False,
              (*Contained*)       False,
              (*ContainingType*)  locFac.getType(s_uri,s_type_object_A),
              (*ReadOnly*)        False,
              (*Attribute*)       True
            );
    CheckEquals('p_string', locObj.getName(), 'Name');
    Check(locObj.getType().equals(locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[StringType])), 'Type');
    CheckEquals(False, locObj.isMany(), 'isMany');
    CheckEquals(False, locObj.isReadOnly(), 'isReadOnly');
    CheckEquals(False, locObj.isContainment(), 'isContainment');
    CheckEquals(Ord(locObj.getType().getTypeEnum), Ord(locObj.getTypeEnum()), 'getTypeEnum');
    CheckEquals(False, locObj.isReference(), 'isReference');
    CheckEquals(True, locObj.isAttribute(), 'isAttribute');
    CheckEquals(False, locObj.isDefaulted(), 'isDefaulted');   

  locObj := create_obj(
              (*Name*)            'p_string',
              (*Type*)            locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[StringType]),
              (* Many *)          False,
              (*Contained*)       False,
              (*ContainingType*)  locFac.getType(s_uri,s_type_object_A),
              (*ReadOnly*)        False,
              (*Attribute*)       False
            );
    CheckEquals('p_string', locObj.getName(), 'Name');
    Check(locObj.getType().equals(locFac.getType(sdo_namespace,SDOTypeDefaultTypeNames[StringType])), 'Type');
    CheckEquals(False, locObj.isMany(), 'isMany');
    CheckEquals(False, locObj.isReadOnly(), 'isReadOnly');
    CheckEquals(False, locObj.isContainment(), 'isContainment');
    CheckEquals(Ord(locObj.getType().getTypeEnum), Ord(locObj.getTypeEnum()), 'getTypeEnum');
    CheckEquals(False, locObj.isReference(), 'isReference');
    CheckEquals(False, locObj.isAttribute(), 'isAttribute');
    CheckEquals(False, locObj.isDefaulted(), 'isDefaulted');

  locObj := create_obj(
              (*Name*)            'p_ab',
              (*Type*)            locFac.getType(s_uri,s_type_object_B),
              (* Many *)          False,
              (*Contained*)       False,
              (*ContainingType*)  locFac.getType(s_uri,s_type_object_A),
              (*ReadOnly*)        False,
              (*Attribute*)       False
            );
    CheckEquals('p_ab', locObj.getName(), 'Name');
    Check(locObj.getType().equals(locFac.getType(s_uri,s_type_object_B)), 'Type');
    CheckEquals(False, locObj.isMany(), 'isMany');
    CheckEquals(False, locObj.isReadOnly(), 'isReadOnly');
    CheckEquals(False, locObj.isContainment(), 'isContainment');
    CheckEquals(Ord(locObj.getType().getTypeEnum), Ord(locObj.getTypeEnum()), 'getTypeEnum');
    CheckEquals(True, locObj.isReference(), 'isReference');
    CheckEquals(False, locObj.isDefaulted(), 'isDefaulted');

  locObj := create_obj(
              (*Name*)            'p_ab2',
              (*Type*)            locFac.getType(s_uri,s_type_object_B),
              (* Many *)          False,
              (*Contained*)       True,
              (*ContainingType*)  locFac.getType(s_uri,s_type_object_A),
              (*ReadOnly*)        False,
              (*Attribute*)       False
            );
    CheckEquals('p_ab2', locObj.getName(), 'Name');
    Check(locObj.getType().equals(locFac.getType(s_uri,s_type_object_B)), 'Type');
    CheckEquals(False, locObj.isMany(), 'isMany');
    CheckEquals(False, locObj.isReadOnly(), 'isReadOnly');
    CheckEquals(True, locObj.isContainment(), 'isContainment');
    CheckEquals(Ord(locObj.getType().getTypeEnum), Ord(locObj.getTypeEnum()), 'getTypeEnum');
    CheckEquals(False, locObj.isReference(), 'isReference');
    CheckEquals(False, locObj.isDefaulted(), 'isDefaulted');
end;

{ TSDOProperty_Test }

class function TSDOProperty_Test.create_obj(
  const AName: string;
  const AType: ISDOType;
  const AMany, AContained: Boolean;
  const AContainingType: ISDOType;
  const AReadOnly: Boolean;
      const AIsAttribute : Boolean
): ISDOProperty;
var
  f : TPropertyFlags;
begin
  f := [];
  if AMany then
    Include(f, pfIsMany);
  if AContained then
    Include(f, pfIsContainment);
  if AReadOnly then
    Include(f, pfIsReadOnly);    
  if AIsAttribute then
    Include(f, pfIsAttribute);

  Result := TSDOProperty.Create(AName,AType,f,AContainingType) as ISDOProperty;
end;

initialization
  RegisterTest('Metadata',TSDOProperty_Test.Suite);

end.
