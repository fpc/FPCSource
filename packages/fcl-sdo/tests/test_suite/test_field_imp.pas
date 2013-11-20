{$INCLUDE sdo_global.inc}
unit test_field_imp;

interface
uses SysUtils, Classes
{$IFDEF FPC}
  ,fpcunit, testutils, testregistry
{$ENDIF}
{$IFNDEF FPC}
  ,TestFrameWork
{$ENDIF}
  ,test_suite_utils, sdo_types, sdo, sdo_field_imp ;

type

  TSDOField_Test = class(TWstBaseTest)
  protected
    function Create_Field() : ISDOField;virtual;abstract;
    class function GetTestSuitePath() : string;
  published
    procedure isSet();
    procedure unset();virtual;
    procedure isNull();
    procedure setNull();virtual;
  end;

  TSDOBooleanField_Test = class(TSDOField_Test)
  protected
    function Create_Field() : ISDOField;override;
  published
    procedure getBoolean();
    procedure setBoolean();
    procedure getString();
    procedure setString();
    procedure getInteger();
    procedure setInteger();
    procedure getByte();
    procedure setByte();
    procedure getLong();
    procedure setLong();
    procedure getShort();
    procedure setShort();
    procedure getCharacter();
    procedure setCharacter();
  end;

  TSDOIntegerField_Test = class(TSDOField_Test)
  protected
    function Create_Field() : ISDOField;override;
  published
    procedure getBoolean();
    procedure setBoolean();
    procedure getString();
    procedure setString();
    procedure getInteger();
    procedure setInteger();
    procedure getByte();
    procedure setByte();
    procedure getLong();
    procedure setLong();
    procedure getShort();
    procedure setShort();
    procedure getCharacter();
    procedure setCharacter();
  end;

  TSDOStringField_Test = class(TSDOField_Test)
  protected
    function Create_Field() : ISDOField;override;
    class procedure CleanUpBuffer(var ABuffer : PPSDOString);
  published
    procedure getBoolean();
    procedure setBoolean();
    procedure getString();
    procedure setString();
    procedure getInteger();
    procedure setInteger();
    procedure getByte();
    procedure setByte();
    procedure getLong();
    procedure setLong();
    procedure getShort();
    procedure setShort();
    procedure getCharacter();
    procedure setCharacter();

    procedure unset();override;
    procedure setNull();override;

    procedure setString_unset_setStrin();
  end;

  TSDOBaseDataObject_Test = class(TSDOField_Test)
  private
    FFactory : ISDODataFactory;
  protected
    class function Create_Factory() : ISDODataFactory;
    procedure SetUp(); override;
    procedure TearDown(); override;
  protected
    function Create_Field() : ISDOField;override;
  published
    procedure getDataObject();
    procedure setDataObject();
    procedure unset();override;
    procedure setNull();override;
  end;

  TSDOChangeSummaryField_Test = class(TSDOField_Test)
  private
    FFactory : ISDODataFactory;
  protected
    class function Create_Factory() : ISDODataFactory;
    procedure SetUp(); override;
    procedure TearDown(); override;
  protected
    function Create_Field() : ISDOField;override;
  published
    procedure getChangeSummary();
    procedure setChangeSummary();
    procedure unset();override;
    procedure setNull();override;
  end;

  TSDOByteField_Test = class(TSDOField_Test)
  protected
    function Create_Field() : ISDOField;override;
  published
    procedure getBoolean();
    procedure setBoolean();
    procedure getString();
    procedure setString();
    procedure getInteger();
    procedure setInteger();
    procedure getByte();
    procedure setByte();
    procedure getCharacter();
    procedure setCharacter();
    procedure getLong();
    procedure setLong();
    procedure getShort();
    procedure setShort();
  end;

  TSDODateField_Test = class(TSDOField_Test)
  protected
    function Create_Field() : ISDOField;override;
  public
    procedure CheckEquals(expected, actual: TSDODate; msg: string = ''; const AStrict : Boolean = True); overload;
  published
    procedure getDate();
    procedure setDate();
    procedure getString();
    procedure setString();
  end;

{$IFDEF HAS_SDO_BYTES}
  TSDOBytesField_Test = class(TSDOField_Test)
  protected
    function Create_Field() : ISDOField;override;
    class procedure CleanUpBuffer(var ABuffer : PPSDOBytes);
  published
    procedure getBytes();
    procedure setBytes();
    procedure getString();
    procedure setString();
  end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
  TSDOCharField_Test = class(TSDOField_Test)
  protected
    function Create_Field() : ISDOField;override;
  published
    procedure getBoolean();
    procedure setBoolean();
    procedure getString();
    procedure setString();
    procedure getInteger();
    procedure setInteger();
    procedure getByte();
    procedure setByte();
    procedure getCharacter();
    procedure setCharacter();
    procedure getLong();
    procedure setLong();
    procedure getShort();
    procedure setShort();
  end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
  TSDOCurrencyField_Test = class(TSDOField_Test)
  protected
    function Create_Field() : ISDOField;override;
  published
    procedure getCurrency();
    procedure setCurrency();
    procedure getDouble();
    procedure setDouble();
    procedure getFloat();
    procedure setFloat();
    procedure getString();
    procedure setString();
  end;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
  TSDODoubleField_Test = class(TSDOField_Test)
  protected
    function Create_Field() : ISDOField;override;
  published
    procedure getCurrency();
    procedure setCurrency();
    procedure getDouble();
    procedure setDouble();
    procedure getFloat();
    procedure setFloat();
    procedure getString();
    procedure setString();
  end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
  TSDOFloatField_Test = class(TSDOField_Test)
  protected
    function Create_Field() : ISDOField;override;
  published
    procedure getCurrency();
    procedure setCurrency();
    procedure getDouble();
    procedure setDouble();
    procedure getFloat();
    procedure setFloat();
    procedure getString();
    procedure setString();
  end;
{$ENDIF HAS_SDO_FLOAT}

{$IFDEF HAS_SDO_LONG}
  TSDOLongField_Test = class(TSDOField_Test)
  protected
    function Create_Field() : ISDOField;override;
  published
    procedure getBoolean();
    procedure setBoolean();
    procedure getString();
    procedure setString();
    procedure getInteger();
    procedure setInteger();
    procedure getByte();
    procedure setByte();
    procedure getCharacter();
    procedure setCharacter();
    procedure getLong();
    procedure setLong();
    procedure getShort();
    procedure setShort();
  end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
  TSDOShortField_Test = class(TSDOField_Test)
  protected
    function Create_Field() : ISDOField;override;
  published
    procedure getBoolean();
    procedure setBoolean();
    procedure getString();
    procedure setString();
    procedure getInteger();
    procedure setInteger();
    procedure getByte();
    procedure setByte();
    procedure getCharacter();
    procedure setCharacter();
    procedure getLong();
    procedure setLong();
    procedure getShort();
    procedure setShort();
  end;
{$ENDIF HAS_SDO_SHORT}

implementation

uses
  sdo_imp_utils, sdo_datafactory, sdo_changesummary, sdo_date_utils, DateUtils;

const s_URI_1  = 'uri:1'; s_URI_3 = 'uri:3';
      s_TYPE_1 = 'type1'; s_TYPE_2 = 'type2'; s_TYPE_3 = 'type3';
      s_PROP_BOOL_1 = 'propboolean1';  s_PROP_INTEGER_1 = 'propinteger1';
      s_PROP_BOOL_2 = 'propboolean2';  s_PROP_INTEGER_2 = 'propinteger2';
      s_PROP_STR_1 = 'propStr1';  s_PROP_STR_2 = 'propStr2';
      s_PROP_OBJ_CONT = 'propobj_cont'; s_PROP_OBJ_REF = 'propobj_ref';
      s_PROP_BOOL_A = 'propbooleanA';  s_PROP_INTEGER_A = 'propintegerA';
      s_PROP_STR_A = 'propStrA';
      s_PROP_CHANGE_SUMMARY = 'propChangeSummary';

{ TSDOBooleanField_Test }

function TSDOBooleanField_Test.Create_Field() : ISDOField;
begin
  Result := TSDOBooleanField.Create();
end;

procedure TSDOBooleanField_Test.getBoolean();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  intVal : DWord;
  buffer, valBuffer, attributeBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  valBuffer := buffer;
  attributeBuffer := buffer;
  Inc(valBuffer);
  obj := Create_Field();

  valBuffer^ := 1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_0));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_0));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));

  valBuffer^ := 0;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_0));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_0));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(valBuffer);
  Inc(attributeBuffer);
    valBuffer^ := 1;
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
          CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_1));
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
          CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
          CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_1));
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
          CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));

    valBuffer^ := 0;
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
          CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_1));
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
          CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
          CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_1));
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
          CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_1));
end;

procedure TSDOBooleanField_Test.getByte();
const F_OFFSET = 0;
var
  obj : ISDOField;
  intVal : DWord;
  buffer, valBuffer, attributeBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  attributeBuffer := buffer;
  valBuffer := buffer;
  Inc(valBuffer);
  obj := Create_Field();

  valBuffer^ := 1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(1,obj.getByte(buffer,F_OFFSET));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(1,obj.getByte(buffer,F_OFFSET));

  valBuffer^ := 0;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET));

  valBuffer^ := 1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(1,obj.getByte(buffer,F_OFFSET));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(1,obj.getByte(buffer,F_OFFSET));
end;

procedure TSDOBooleanField_Test.getCharacter();
const F_OFFSET = 0;
var
  obj : ISDOField;
  intVal : DWord;
  buffer, valBuffer, attributeBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  attributeBuffer := buffer;
  valBuffer := buffer;
  Inc(valBuffer);
  obj := Create_Field();

  valBuffer^ := 1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals('1',obj.getCharacter(buffer,F_OFFSET));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals('1',obj.getCharacter(buffer,F_OFFSET));

  valBuffer^ := 0;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals('0',obj.getCharacter(buffer,F_OFFSET));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals('0',obj.getCharacter(buffer,F_OFFSET));

  valBuffer^ := 1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals('1',obj.getCharacter(buffer,F_OFFSET));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals('1',obj.getCharacter(buffer,F_OFFSET));
end;

procedure TSDOBooleanField_Test.getInteger();
const F_OFFSET = 0;
var
  obj : ISDOField;
  intVal : DWord;
  buffer, valBuffer, attributeBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  attributeBuffer := buffer;
  valBuffer := buffer;
  Inc(valBuffer);
  obj := Create_Field();

  valBuffer^ := 1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(1,obj.getInteger(buffer,F_OFFSET));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(1,obj.getInteger(buffer,F_OFFSET));

  valBuffer^ := 0;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET));

  valBuffer^ := 1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(1,obj.getInteger(buffer,F_OFFSET));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(1,obj.getInteger(buffer,F_OFFSET));
end;

procedure TSDOBooleanField_Test.getLong();
const F_OFFSET = 0;
var
  obj : ISDOField;
  intVal : DWord;
  buffer, valBuffer, attributeBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  attributeBuffer := buffer;
  valBuffer := buffer;
  Inc(valBuffer);
  obj := Create_Field();

  valBuffer^ := 1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(1,obj.getLong(buffer,F_OFFSET));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(1,obj.getLong(buffer,F_OFFSET));

  valBuffer^ := 0;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET));

  valBuffer^ := 1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(1,obj.getLong(buffer,F_OFFSET));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(1,obj.getLong(buffer,F_OFFSET));
end;

procedure TSDOBooleanField_Test.getShort();
const F_OFFSET = 0;
var
  obj : ISDOField;
  intVal : DWord;
  buffer, valBuffer, attributeBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  attributeBuffer := buffer;
  valBuffer := buffer;
  Inc(valBuffer);
  obj := Create_Field();

  valBuffer^ := 1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(1,obj.getShort(buffer,F_OFFSET));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(1,obj.getShort(buffer,F_OFFSET));

  valBuffer^ := 0;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET));

  valBuffer^ := 1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(1,obj.getShort(buffer,F_OFFSET));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(1,obj.getShort(buffer,F_OFFSET));
end;

procedure TSDOBooleanField_Test.getString();
const F_OFFSET = 0;
var
  obj : ISDOField;
  intVal : DWord;
  buffer, valBuffer, attributeBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  attributeBuffer := buffer;
  valBuffer := buffer;
  Inc(valBuffer);
  obj := Create_Field();

  valBuffer^ := 1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals('1',obj.getString(buffer,F_OFFSET));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals('1',obj.getString(buffer,F_OFFSET));

  valBuffer^ := 0;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals('0',obj.getString(buffer,F_OFFSET));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals('0',obj.getString(buffer,F_OFFSET));

  valBuffer^ := 1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals('1',obj.getString(buffer,F_OFFSET));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals('1',obj.getString(buffer,F_OFFSET));
end;

procedure TSDOBooleanField_Test.setBoolean();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  intVal : DWord;
  buffer, valBuffer, attributeBuffer: PByte;
begin
  intVal := 0;
  buffer := @intVal;
  valBuffer := buffer;
  attributeBuffer := buffer;
  Inc(valBuffer);
  obj := Create_Field();

  obj.setBoolean(buffer,F_OFFSET_0,True);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(1,valBuffer^);

  obj.setBoolean(buffer,F_OFFSET_0,False);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(0,valBuffer^);

  intVal := 0;
  Inc(valBuffer);
  Inc(attributeBuffer);
    obj.setBoolean(buffer,F_OFFSET_1,True);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(1,valBuffer^);

    obj.setBoolean(buffer,F_OFFSET_1,False);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(0,valBuffer^);
end;

procedure TSDOBooleanField_Test.setByte();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  intVal : DWord;
  buffer, valBuffer, attributeBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  valBuffer := buffer;
  attributeBuffer := buffer;
  Inc(valBuffer);
  obj := Create_Field();

  obj.setByte(buffer,F_OFFSET_0,1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(1,valBuffer^);

  obj.setByte(buffer,F_OFFSET_0,0);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(0,valBuffer^);

  intVal := 0;
  Inc(valBuffer);
  Inc(attributeBuffer);
    obj.setByte(buffer,F_OFFSET_1,123);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(1,valBuffer^);

    obj.setByte(buffer,F_OFFSET_1,0);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(0,valBuffer^);
end;

procedure TSDOBooleanField_Test.setCharacter();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  intVal : DWord;
  buffer, valBuffer, attributeBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  attributeBuffer := buffer;
  valBuffer := buffer;
  Inc(valBuffer);
  obj := Create_Field();

  obj.setCharacter(buffer,F_OFFSET_0,'T');
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(1,valBuffer^);

  obj.setCharacter(buffer,F_OFFSET_0,#0);
  CheckEquals(0,valBuffer^);

  intVal := 0;
  Inc(valBuffer);
  Inc(attributeBuffer);
    obj.setCharacter(buffer,F_OFFSET_1,'T');
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(1,valBuffer^);

    obj.setCharacter(buffer,F_OFFSET_1,#0);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(0,valBuffer^);
end;

procedure TSDOBooleanField_Test.setInteger();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  intVal : DWord;
  buffer, valBuffer, attributeBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  valBuffer := buffer;
  attributeBuffer := buffer;
  Inc(valBuffer);
  obj := Create_Field();

  obj.setInteger(buffer,F_OFFSET_0,1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(1,valBuffer^);

  obj.setInteger(buffer,F_OFFSET_0,0);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(0,valBuffer^);

  intVal := 0;
  Inc(valBuffer);
  Inc(attributeBuffer);
    obj.setInteger(buffer,F_OFFSET_1,300);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(1,valBuffer^);

    obj.setInteger(buffer,F_OFFSET_1,0);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(0,valBuffer^);
end;

procedure TSDOBooleanField_Test.setLong();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  intVal : DWord;
  buffer, valBuffer, attributeBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  valBuffer := buffer;
  attributeBuffer := buffer;
  Inc(valBuffer);
  obj := Create_Field();

  obj.setLong(buffer,F_OFFSET_0,1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(1,valBuffer^);

  obj.setLong(buffer,F_OFFSET_0,0);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(0,valBuffer^);

  intVal := 0;
  Inc(valBuffer);
  Inc(attributeBuffer);
    obj.setLong(buffer,F_OFFSET_1,123);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(1,valBuffer^);

    obj.setLong(buffer,F_OFFSET_1,0);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(0,valBuffer^);
end;

procedure TSDOBooleanField_Test.setShort();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  intVal : DWord;
  buffer, valBuffer, attributeBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  valBuffer := buffer;
  attributeBuffer := buffer;
  Inc(valBuffer);
  obj := Create_Field();

  obj.setShort(buffer,F_OFFSET_0,1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(1,valBuffer^);

  obj.setShort(buffer,F_OFFSET_0,0);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(0,valBuffer^);

  intVal := 0;
  Inc(valBuffer);
  Inc(attributeBuffer);
    obj.setShort(buffer,F_OFFSET_1,123);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(1,valBuffer^);

    obj.setShort(buffer,F_OFFSET_1,0);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(0,valBuffer^);
end;

procedure TSDOBooleanField_Test.setString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  intVal : DWord;
  buffer, valBuffer, attributeBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  attributeBuffer := buffer;
  valBuffer := buffer;
  Inc(valBuffer);
  obj := Create_Field();

  obj.setString(buffer,F_OFFSET_0,'True');
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(1,valBuffer^);

  obj.setString(buffer,F_OFFSET_0,'False');
  CheckEquals(0,valBuffer^);

  intVal := 0;
  Inc(valBuffer);
  Inc(attributeBuffer);
    obj.setString(buffer,F_OFFSET_1,'True');
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(1,valBuffer^);

    obj.setString(buffer,F_OFFSET_1,'False');
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(0,valBuffer^);
end;


{ TSDOField_Test }

class function TSDOField_Test.GetTestSuitePath: string;
begin
  Result := 'Field';
end;

procedure TSDOField_Test.isNull();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  intVal : DWord;
  buffer, valBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  valBuffer := buffer;
  obj := Create_Field();

  valBuffer^ := 0;
  SetBit(valBuffer^,BIT_ORDER_SET,True);
  SetBit(valBuffer^,BIT_ORDER_NULL,True);
  CheckEquals(True,obj.isSet(buffer,F_OFFSET_0));
  CheckEquals(True,obj.isNull(buffer,F_OFFSET_0));

  valBuffer^ := 0;
  CheckEquals(False,obj.isSet(buffer,F_OFFSET_0));
  CheckEquals(False,obj.isNull(buffer,F_OFFSET_0));

  valBuffer^ := 0;
  SetBit(valBuffer^,BIT_ORDER_SET,False);
  SetBit(valBuffer^,BIT_ORDER_NULL,True);
  CheckEquals(False,obj.isSet(buffer,F_OFFSET_0));
  CheckEquals(True,obj.isNull(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(valBuffer);
    valBuffer^ := 0;
    SetBit(valBuffer^,BIT_ORDER_SET,True);
    SetBit(valBuffer^,BIT_ORDER_NULL,True);
    CheckEquals(True,obj.isSet(buffer,F_OFFSET_1));
    CheckEquals(True,obj.isNull(buffer,F_OFFSET_1));

    valBuffer^ := 0;
    CheckEquals(False,obj.isSet(buffer,F_OFFSET_1));
    CheckEquals(False,obj.isNull(buffer,F_OFFSET_1));

    valBuffer^ := 0;
    SetBit(valBuffer^,BIT_ORDER_SET,False);
    SetBit(valBuffer^,BIT_ORDER_NULL,True);
    CheckEquals(False,obj.isSet(buffer,F_OFFSET_1));
    CheckEquals(True,obj.isNull(buffer,F_OFFSET_1));
end;

procedure TSDOField_Test.isSet();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  intVal : DWord;
  buffer, valBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  valBuffer := buffer;
  obj := Create_Field();

  SetBit(valBuffer^,BIT_ORDER_SET,True);// valBuffer^ := 2;
  CheckEquals(True,obj.isSet(buffer,F_OFFSET_0));

  valBuffer^ := 0;
  CheckEquals(False,obj.isSet(buffer,F_OFFSET_0));


  intVal := 0;
  Inc(valBuffer);
    SetBit(valBuffer^,BIT_ORDER_SET,True);
    SetBit(valBuffer^,BIT_ORDER_SET + 1,True); //valBuffer^ := 6;
    CheckEquals(True,obj.isSet(buffer,F_OFFSET_1));

    valBuffer^ := 0;
    CheckEquals(False,obj.isSet(buffer,F_OFFSET_1));
end;

procedure TSDOField_Test.setNull();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  intVal : DWord;
  buffer, valBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  valBuffer := buffer;
  obj := Create_Field();

  obj.setNull(buffer,F_OFFSET_0);
  CheckEquals(True,IsBitON(valBuffer^,BIT_ORDER_SET));
  CheckEquals(True,IsBitON(valBuffer^,BIT_ORDER_NULL));

  SetBit(valBuffer^,BIT_ORDER_SET,True);
  SetBit(valBuffer^,BIT_ORDER_SET,False);
  obj.setNull(buffer,F_OFFSET_0);
  CheckEquals(True,IsBitON(valBuffer^,BIT_ORDER_SET));
  CheckEquals(True,IsBitON(valBuffer^,BIT_ORDER_NULL));

  intVal := 0;
  Inc(valBuffer);
    obj.setNull(buffer,F_OFFSET_1);
    CheckEquals(True,IsBitON(valBuffer^,BIT_ORDER_SET));
    CheckEquals(True,IsBitON(valBuffer^,BIT_ORDER_NULL));

    SetBit(valBuffer^,BIT_ORDER_SET,True);
    SetBit(valBuffer^,BIT_ORDER_SET,False);
    obj.setNull(buffer,F_OFFSET_1);
    CheckEquals(True,IsBitON(valBuffer^,BIT_ORDER_SET));
    CheckEquals(True,IsBitON(valBuffer^,BIT_ORDER_NULL));
end;

procedure TSDOField_Test.unset();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  intVal : DWord;
  buffer, valBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  valBuffer := buffer;
  obj := Create_Field();

  obj.unset(buffer,F_OFFSET_0);
  CheckEquals(False,IsBitON(valBuffer^,BIT_ORDER_SET));

  SetBit(valBuffer^,BIT_ORDER_SET,True);//valBuffer^ := 2;
  obj.unset(buffer,F_OFFSET_0);
  CheckEquals(False,IsBitON(valBuffer^,BIT_ORDER_SET));

  SetBit(valBuffer^,BIT_ORDER_SET,True);//valBuffer^ := 6;
  SetBit(valBuffer^,BIT_ORDER_SET + 1,True);
  obj.unset(buffer,F_OFFSET_0);
  CheckEquals(False,IsBitON(valBuffer^,BIT_ORDER_SET));
  CheckEquals(True,IsBitON(valBuffer^,BIT_ORDER_SET + 1));

  intVal := 0;
  Inc(valBuffer);  
    obj.unset(buffer,F_OFFSET_1);
    CheckEquals(False,IsBitON(valBuffer^,BIT_ORDER_SET));

    SetBit(valBuffer^,BIT_ORDER_SET,True);//valBuffer^ := 2;
    obj.unset(buffer,F_OFFSET_1);
    CheckEquals(False,IsBitON(valBuffer^,BIT_ORDER_SET));

    SetBit(valBuffer^,BIT_ORDER_SET,True);//valBuffer^ := 6;
    SetBit(valBuffer^,BIT_ORDER_SET + 1,True);//
    obj.unset(buffer,F_OFFSET_1);
    CheckEquals(False,IsBitON(valBuffer^,BIT_ORDER_SET));
    CheckEquals(True,IsBitON(valBuffer^,BIT_ORDER_SET + 1));
end;

{ TSDOIntegerField_Test }

function TSDOIntegerField_Test.Create_Field() : ISDOField;
begin
  Result := TSDOIntegerField.Create();
end;

procedure TSDOIntegerField_Test.getBoolean();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 12345; VAL_2 = -9876; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOInteger;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOInteger(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOInteger(tmpBuffer);
    valBuffer^ := VAL_1;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));

    valBuffer^ := VAL_2;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_1));
end;

procedure TSDOIntegerField_Test.getByte();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 123; VAL_2 = 45; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOInteger;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOInteger(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getByte(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getByte(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getByte(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getByte(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getByte(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getByte(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOInteger(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getByte(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getByte(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getByte(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getByte(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getByte(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getByte(buffer,F_OFFSET_1));
end;

procedure TSDOIntegerField_Test.getCharacter();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = Ord(TSDOChar('I')); VAL_2 = Ord(TSDOChar('W')); VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOInteger;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOInteger(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_1),obj.getCharacter(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_1),obj.getCharacter(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_2),obj.getCharacter(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_2),obj.getCharacter(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_3),obj.getCharacter(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_3),obj.getCharacter(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOInteger(tmpBuffer);
    valBuffer^ := VAL_1;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_1),obj.getCharacter(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_1),obj.getCharacter(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_2),obj.getCharacter(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_2),obj.getCharacter(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_3),obj.getCharacter(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_3),obj.getCharacter(buffer,F_OFFSET_1));
end;

procedure TSDOIntegerField_Test.getInteger();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 12345; VAL_2 = -9876; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOInteger;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOInteger(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getInteger(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getInteger(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getInteger(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getInteger(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getInteger(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getInteger(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOInteger(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getInteger(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getInteger(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getInteger(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getInteger(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getInteger(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getInteger(buffer,F_OFFSET_1));
end;

procedure TSDOIntegerField_Test.getLong();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 12345; VAL_2 = -98765; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOInteger;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOInteger(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getLong(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getLong(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getLong(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getLong(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getLong(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getLong(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOInteger(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getLong(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getLong(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getLong(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getLong(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getLong(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getLong(buffer,F_OFFSET_1));
end;

procedure TSDOIntegerField_Test.getShort();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 1234; VAL_2 = -987; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOInteger;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOInteger(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getShort(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getShort(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getShort(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getShort(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getShort(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getShort(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOInteger(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getShort(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getShort(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getShort(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getShort(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getShort(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getShort(buffer,F_OFFSET_1));
end;

procedure TSDOIntegerField_Test.getString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 12345; VAL_2 = -9876; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOInteger;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOInteger(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_1),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_1),obj.getString(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_2),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_2),obj.getString(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_3),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_3),obj.getString(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOInteger(tmpBuffer);
    valBuffer^ := VAL_1;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_1),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_1),obj.getString(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_2),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_2),obj.getString(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_3),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_3),obj.getString(buffer,F_OFFSET_1));
end;

procedure TSDOIntegerField_Test.setBoolean();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOInteger;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOInteger(tmpBuffer);
  obj := Create_Field();

  obj.setBoolean(buffer,F_OFFSET_0,True);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(1,valBuffer^);

  obj.setBoolean(buffer,F_OFFSET_0,False);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(0,valBuffer^);

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOInteger(tmpBuffer);

    obj.setBoolean(buffer,F_OFFSET_1,True);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(1,valBuffer^);

    obj.setBoolean(buffer,F_OFFSET_1,False);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(0,valBuffer^);
end;

procedure TSDOIntegerField_Test.setByte();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 123; VAL_2 = 45; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOInteger;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOInteger(tmpBuffer);
  obj := Create_Field();

  obj.setByte(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setByte(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setByte(buffer,F_OFFSET_0,VAL_3);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOInteger(tmpBuffer);

    obj.setByte(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setByte(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setByte(buffer,F_OFFSET_1,VAL_3);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOIntegerField_Test.setCharacter();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = Ord(TSDOChar('I')); VAL_2 = Ord(TSDOChar('W')); VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOInteger;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOInteger(tmpBuffer);
  obj := Create_Field();

  obj.setCharacter(buffer,F_OFFSET_0,TSDOChar(VAL_1));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setCharacter(buffer,F_OFFSET_0,TSDOChar(VAL_2));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOInteger(tmpBuffer);

    obj.setCharacter(buffer,F_OFFSET_1,TSDOChar(VAL_1));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setCharacter(buffer,F_OFFSET_1,TSDOChar(VAL_2));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);
end;

procedure TSDOIntegerField_Test.setInteger();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 12345; VAL_2 = -9876; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOInteger;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOInteger(tmpBuffer);
  obj := Create_Field();

  obj.setInteger(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setInteger(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setInteger(buffer,F_OFFSET_0,VAL_3);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOInteger(tmpBuffer);

    obj.setInteger(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setInteger(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setInteger(buffer,F_OFFSET_1,VAL_3);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOIntegerField_Test.setLong();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 12345; VAL_2 = -9876; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOInteger;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOInteger(tmpBuffer);
  obj := Create_Field();

  obj.setLong(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setLong(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setLong(buffer,F_OFFSET_0,VAL_3);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOInteger(tmpBuffer);

    obj.setLong(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setLong(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setLong(buffer,F_OFFSET_1,VAL_3);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOIntegerField_Test.setShort();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 123; VAL_2 = -9876; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOInteger;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOInteger(tmpBuffer);
  obj := Create_Field();

  obj.setShort(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setShort(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setShort(buffer,F_OFFSET_0,VAL_3);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOInteger(tmpBuffer);

    obj.setShort(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setShort(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setShort(buffer,F_OFFSET_1,VAL_3);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOIntegerField_Test.setString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 12345; VAL_2 = -9876; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOInteger;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOInteger(tmpBuffer);
  obj := Create_Field();

  obj.setString(buffer,F_OFFSET_0,IntToStr(VAL_1));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setString(buffer,F_OFFSET_0,IntToStr(VAL_2));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOInteger(tmpBuffer);

    obj.setString(buffer,F_OFFSET_1,IntToStr(VAL_1));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setString(buffer,F_OFFSET_1,IntToStr(VAL_2));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);
end;

{ TSDOStringField_Test }

class procedure TSDOStringField_Test.CleanUpBuffer(var ABuffer : PPSDOString);
begin
  if ( ABuffer <> nil ) then begin
    ABuffer^^ := '';
    FreeMem(ABuffer^,SizeOf(PSDOString));
    ABuffer^ := nil;
  end;
end;

function TSDOStringField_Test.Create_Field() : ISDOField;
begin
  Result := TSDOBaseStringField.Create();
end;

procedure TSDOStringField_Test.getBoolean();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 'azerty12345'; VAL_2 = 'true'; VAL_3 = 'false'; VAL_4 = '1'; VAL_5 = '0';
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDOString;
  ok : Boolean;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);
  obj := Create_Field();

  ok := False;
  try
    obj.getBoolean(buffer,F_OFFSET_0);
  except
    on e : ESDOInvalidConversionException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Invalid conversion of empty string');

  SetBit(startBuffer^,BIT_ORDER_SET,True);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,True);

  GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
  try
    valBuffer^^ := VAL_1;
    ok := False;
    try
      obj.getBoolean(buffer,F_OFFSET_0);
    except
      on e : ESDOInvalidConversionException do begin
        ok := True;
      end;
    end;
    CheckEquals(True,ok,'Invalid conversion : ' + VAL_1);


    valBuffer^^ := VAL_2;
    CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));

    valBuffer^^ := VAL_3;
    CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_0));

    valBuffer^^ := VAL_4;
    CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));

    valBuffer^^ := VAL_5;
    CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_0));

    valBuffer^^ := '';

  finally
    FreeMem(valBuffer^,SizeOf(PSDOString));
    valBuffer^ := nil;
  end;

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);

    ok := False;
    try
      obj.getInteger(buffer,F_OFFSET_1);
    except
      on e : ESDOInvalidConversionException do begin
        ok := True;
      end;
    end;
    CheckEquals(True,ok,'Invalid Conversion');

    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
    try
      valBuffer^^ := VAL_1;
      valBuffer^^ := VAL_1;
      ok := False;
      try
        obj.getBoolean(buffer,F_OFFSET_1);
      except
        on e : ESDOInvalidConversionException do begin
          ok := True;
        end;
      end;
      CheckEquals(True,ok,'Invalid conversion : ' + VAL_1);


      valBuffer^^ := VAL_2;
      CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));

      valBuffer^^ := VAL_3;
      CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_1));

      valBuffer^^ := VAL_4;
      CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));

      valBuffer^^ := VAL_5;
      CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_1));

      
    finally
      valBuffer^^ := '';
      FreeMem(valBuffer^,SizeOf(PSDOString));
      valBuffer^ := nil;
    end;
end;

procedure TSDOStringField_Test.getByte();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 'azerty12345'; VAL_2 = '12'; VAL_3 = '34'; VAL_4 = '0';
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDOString;
  ok : Boolean;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);
  obj := Create_Field();

  ok := False;
  try
    obj.getByte(buffer,F_OFFSET_0);
  except
    on e : ESDOInvalidConversionException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Invalid conversion');

  SetBit(startBuffer^,BIT_ORDER_SET,True);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,True);

  GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
  try
    valBuffer^^ := VAL_1;
    ok := False;
    try
      obj.getByte(buffer,F_OFFSET_0);
    except
      on e : ESDOInvalidConversionException do begin
        ok := True;
      end;
    end;
    CheckEquals(True,ok,'Invalid conversion');


    valBuffer^^ := VAL_2;
    CheckEquals(StrToInt(VAL_2),obj.getByte(buffer,F_OFFSET_0));

    valBuffer^^ := VAL_3;
    CheckEquals(StrToInt(VAL_3),obj.getByte(buffer,F_OFFSET_0));

    valBuffer^^ := VAL_4;
    CheckEquals(StrToInt(VAL_4),obj.getByte(buffer,F_OFFSET_0));

    valBuffer^^ := '';

  finally
    FreeMem(valBuffer^,SizeOf(PSDOString));
    valBuffer^ := nil;
  end;

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);

    ok := False;
    try
      obj.getByte(buffer,F_OFFSET_1);
    except
      on e : ESDOInvalidConversionException do begin
        ok := True;
      end;
    end;
    CheckEquals(True,ok,'Invalid Conversion');

    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
    try
      valBuffer^^ := VAL_1;
      ok := False;
      try
        obj.getByte(buffer,F_OFFSET_1);
      except
        on e : ESDOInvalidConversionException do begin
          ok := True;
        end;
      end;
      CheckEquals(True,ok,'Invalid conversion');


      valBuffer^^ := VAL_2;
      CheckEquals(StrToInt(VAL_2),obj.getByte(buffer,F_OFFSET_1));

      valBuffer^^ := VAL_3;
      CheckEquals(StrToInt(VAL_3),obj.getByte(buffer,F_OFFSET_1));

      valBuffer^^ := VAL_4;
      CheckEquals(StrToInt(VAL_4),obj.getByte(buffer,F_OFFSET_1));

      valBuffer^^ := '';
    finally
      FreeMem(valBuffer^,SizeOf(PSDOString));
      valBuffer^ := nil;
    end;
end;

procedure TSDOStringField_Test.getCharacter();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 'K'; VAL_2 = 'w'; VAL_3 = #0;
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDOString;
  ok : Boolean;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);
  obj := Create_Field();

  ok := False;
  try
    obj.getCharacter(buffer,F_OFFSET_0);
  except
    on e : ESDOInvalidConversionException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Invalid conversion');

  SetBit(startBuffer^,BIT_ORDER_SET,True);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,True);

  GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
  try
    valBuffer^^ := VAL_1;
    CheckEquals(VAL_1,obj.getCharacter(buffer,F_OFFSET_0));

    valBuffer^^ := VAL_2;
    CheckEquals(VAL_2,obj.getCharacter(buffer,F_OFFSET_0));

    valBuffer^^ := VAL_3;
    CheckEquals(VAL_3,obj.getCharacter(buffer,F_OFFSET_0));

    valBuffer^^ := '';

  finally
    FreeMem(valBuffer^,SizeOf(PSDOString));
    valBuffer^ := nil;
  end;

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);

    ok := False;
    try
      obj.getCharacter(buffer,F_OFFSET_1);
    except
      on e : ESDOInvalidConversionException do begin
        ok := True;
      end;
    end;
    CheckEquals(True,ok,'Invalid Conversion');

    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
    try
      valBuffer^^ := VAL_1;
      CheckEquals(VAL_1,obj.getCharacter(buffer,F_OFFSET_1));

      valBuffer^^ := VAL_2;
      CheckEquals(VAL_2,obj.getCharacter(buffer,F_OFFSET_1));

      valBuffer^^ := VAL_3;
      CheckEquals(VAL_3,obj.getCharacter(buffer,F_OFFSET_1));

      valBuffer^^ := '';
    finally
      FreeMem(valBuffer^,SizeOf(PSDOString));
      valBuffer^ := nil;
    end;
end;

procedure TSDOStringField_Test.getInteger();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 'azerty12345'; VAL_2 = '-9876'; VAL_3 = '567'; VAL_4 = '0';
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDOString;
  ok : Boolean;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);
  obj := Create_Field();

  ok := False;
  try
    obj.getInteger(buffer,F_OFFSET_0);
  except
    on e : ESDOInvalidConversionException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Invalid conversion');

  SetBit(startBuffer^,BIT_ORDER_SET,True);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,True);

  GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
  try
    valBuffer^^ := VAL_1;
    ok := False;
    try
      obj.getInteger(buffer,F_OFFSET_0);
    except
      on e : ESDOInvalidConversionException do begin
        ok := True;
      end;
    end;
    CheckEquals(True,ok,'Invalid conversion');


    valBuffer^^ := VAL_2;
    CheckEquals(StrToInt(VAL_2),obj.getInteger(buffer,F_OFFSET_0));

    valBuffer^^ := VAL_3;
    CheckEquals(StrToInt(VAL_3),obj.getInteger(buffer,F_OFFSET_0));

    valBuffer^^ := VAL_4;
    CheckEquals(StrToInt(VAL_4),obj.getInteger(buffer,F_OFFSET_0));

    valBuffer^^ := '';

  finally
    FreeMem(valBuffer^,SizeOf(PSDOString));
    valBuffer^ := nil;
  end;

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);

    ok := False;
    try
      obj.getInteger(buffer,F_OFFSET_1);
    except
      on e : ESDOInvalidConversionException do begin
        ok := True;
      end;
    end;
    CheckEquals(True,ok,'Invalid Conversion');

    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
    try
      valBuffer^^ := VAL_1;
      ok := False;
      try
        obj.getInteger(buffer,F_OFFSET_1);
      except
        on e : ESDOInvalidConversionException do begin
          ok := True;
        end;
      end;
      CheckEquals(True,ok,'Invalid conversion');


      valBuffer^^ := VAL_2;
      CheckEquals(StrToInt(VAL_2),obj.getInteger(buffer,F_OFFSET_1));

      valBuffer^^ := VAL_3;
      CheckEquals(StrToInt(VAL_3),obj.getInteger(buffer,F_OFFSET_1));

      valBuffer^^ := VAL_4;
      CheckEquals(StrToInt(VAL_4),obj.getInteger(buffer,F_OFFSET_1));

      valBuffer^^ := '';
    finally
      FreeMem(valBuffer^,SizeOf(PSDOString));
      valBuffer^ := nil;
    end;
end;

procedure TSDOStringField_Test.getLong();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 'azerty12345'; VAL_2 = '123456852'; VAL_3 = '-3489962255'; VAL_4 = '0';
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDOString;
  ok : Boolean;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);
  obj := Create_Field();

  ok := False;
  try
    obj.getLong(buffer,F_OFFSET_0);
  except
    on e : ESDOInvalidConversionException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Invalid conversion');

  SetBit(startBuffer^,BIT_ORDER_SET,True);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,True);

  GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
  try
    valBuffer^^ := VAL_1;
    ok := False;
    try
      obj.getLong(buffer,F_OFFSET_0);
    except
      on e : ESDOInvalidConversionException do begin
        ok := True;
      end;
    end;
    CheckEquals(True,ok,'Invalid conversion');


    valBuffer^^ := VAL_2;
    CheckEquals(StrToInt64(VAL_2),obj.getLong(buffer,F_OFFSET_0));

    valBuffer^^ := VAL_3;
    CheckEquals(StrToInt64(VAL_3),obj.getLong(buffer,F_OFFSET_0));

    valBuffer^^ := VAL_4;
    CheckEquals(StrToInt64(VAL_4),obj.getLong(buffer,F_OFFSET_0));

    valBuffer^^ := '';

  finally
    FreeMem(valBuffer^,SizeOf(PSDOString));
    valBuffer^ := nil;
  end;

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);

    ok := False;
    try
      obj.getLong(buffer,F_OFFSET_1);
    except
      on e : ESDOInvalidConversionException do begin
        ok := True;
      end;
    end;
    CheckEquals(True,ok,'Invalid Conversion');

    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
    try
      valBuffer^^ := VAL_1;
      ok := False;
      try
        obj.getLong(buffer,F_OFFSET_1);
      except
        on e : ESDOInvalidConversionException do begin
          ok := True;
        end;
      end;
      CheckEquals(True,ok,'Invalid conversion');


      valBuffer^^ := VAL_2;
      CheckEquals(StrToInt64(VAL_2),obj.getLong(buffer,F_OFFSET_1));

      valBuffer^^ := VAL_3;
      CheckEquals(StrToInt64(VAL_3),obj.getLong(buffer,F_OFFSET_1));

      valBuffer^^ := VAL_4;
      CheckEquals(StrToInt64(VAL_4),obj.getLong(buffer,F_OFFSET_1));

      valBuffer^^ := '';
    finally
      FreeMem(valBuffer^,SizeOf(PSDOString));
      valBuffer^ := nil;
    end;
end;

procedure TSDOStringField_Test.getShort();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 'azerty12345'; VAL_2 = '5678'; VAL_3 = '-3489'; VAL_4 = '0';
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDOString;
  ok : Boolean;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);
  obj := Create_Field();

  ok := False;
  try
    obj.getShort(buffer,F_OFFSET_0);
  except
    on e : ESDOInvalidConversionException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Invalid conversion');

  SetBit(startBuffer^,BIT_ORDER_SET,True);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,True);

  GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
  try
    valBuffer^^ := VAL_1;
    ok := False;
    try
      obj.getShort(buffer,F_OFFSET_0);
    except
      on e : ESDOInvalidConversionException do begin
        ok := True;
      end;
    end;
    CheckEquals(True,ok,'Invalid conversion');


    valBuffer^^ := VAL_2;
    CheckEquals(StrToInt(VAL_2),obj.getShort(buffer,F_OFFSET_0));

    valBuffer^^ := VAL_3;
    CheckEquals(StrToInt(VAL_3),obj.getShort(buffer,F_OFFSET_0));

    valBuffer^^ := VAL_4;
    CheckEquals(StrToInt(VAL_4),obj.getShort(buffer,F_OFFSET_0));

    valBuffer^^ := '';

  finally
    FreeMem(valBuffer^,SizeOf(PSDOString));
    valBuffer^ := nil;
  end;

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);

    ok := False;
    try
      obj.getShort(buffer,F_OFFSET_1);
    except
      on e : ESDOInvalidConversionException do begin
        ok := True;
      end;
    end;
    CheckEquals(True,ok,'Invalid Conversion');

    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
    try
      valBuffer^^ := VAL_1;
      ok := False;
      try
        obj.getShort(buffer,F_OFFSET_1);
      except
        on e : ESDOInvalidConversionException do begin
          ok := True;
        end;
      end;
      CheckEquals(True,ok,'Invalid conversion');


      valBuffer^^ := VAL_2;
      CheckEquals(StrToInt(VAL_2),obj.getShort(buffer,F_OFFSET_1));

      valBuffer^^ := VAL_3;
      CheckEquals(StrToInt(VAL_3),obj.getShort(buffer,F_OFFSET_1));

      valBuffer^^ := VAL_4;
      CheckEquals(StrToInt(VAL_4),obj.getShort(buffer,F_OFFSET_1));

      valBuffer^^ := '';
    finally
      FreeMem(valBuffer^,SizeOf(PSDOString));
      valBuffer^ := nil;
    end;
end;

procedure TSDOStringField_Test.getString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 'azerty12345'; VAL_2 = 'xyz-9876'; VAL_3 = '';
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDOString;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);
  obj := Create_Field();

  CheckEquals('',obj.getString(buffer,F_OFFSET_0));

  GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
  try
    valBuffer^^ := VAL_1;
      SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
        SetBit(startBuffer^,BIT_ORDER_SET,True);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals('',obj.getString(buffer,F_OFFSET_0));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(VAL_1,obj.getString(buffer,F_OFFSET_0));
        SetBit(startBuffer^,BIT_ORDER_SET,False);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals('',obj.getString(buffer,F_OFFSET_0));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(VAL_1,obj.getString(buffer,F_OFFSET_0));
      SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
        SetBit(startBuffer^,BIT_ORDER_SET,True);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals('',obj.getString(buffer,F_OFFSET_0));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals('',obj.getString(buffer,F_OFFSET_0));
        SetBit(startBuffer^,BIT_ORDER_SET,False);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals('',obj.getString(buffer,F_OFFSET_0));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals('',obj.getString(buffer,F_OFFSET_0));

    valBuffer^^ := VAL_2;
      SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
        SetBit(startBuffer^,BIT_ORDER_SET,True);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals('',obj.getString(buffer,F_OFFSET_0));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(VAL_2,obj.getString(buffer,F_OFFSET_0));
        SetBit(startBuffer^,BIT_ORDER_SET,False);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals('',obj.getString(buffer,F_OFFSET_0));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(VAL_2,obj.getString(buffer,F_OFFSET_0));
      SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
        SetBit(startBuffer^,BIT_ORDER_SET,True);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals('',obj.getString(buffer,F_OFFSET_0));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals('',obj.getString(buffer,F_OFFSET_0));
        SetBit(startBuffer^,BIT_ORDER_SET,False);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals('',obj.getString(buffer,F_OFFSET_0));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals('',obj.getString(buffer,F_OFFSET_0));

    valBuffer^^ := VAL_3;
      SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
        SetBit(startBuffer^,BIT_ORDER_SET,True);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals('',obj.getString(buffer,F_OFFSET_0));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(VAL_3,obj.getString(buffer,F_OFFSET_0));
        SetBit(startBuffer^,BIT_ORDER_SET,False);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals('',obj.getString(buffer,F_OFFSET_0));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(VAL_3,obj.getString(buffer,F_OFFSET_0));
      SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
        SetBit(startBuffer^,BIT_ORDER_SET,True);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals('',obj.getString(buffer,F_OFFSET_0));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals('',obj.getString(buffer,F_OFFSET_0));
        SetBit(startBuffer^,BIT_ORDER_SET,False);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals('',obj.getString(buffer,F_OFFSET_0));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals('',obj.getString(buffer,F_OFFSET_0));

    valBuffer^^ := '';
  finally
    FreeMem(valBuffer^,SizeOf(PSDOString));
    valBuffer^ := nil;
  end;

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);
    CheckEquals('',obj.getString(buffer,F_OFFSET_0));

    GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
    try
      valBuffer^^ := VAL_1;
        SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
          SetBit(startBuffer^,BIT_ORDER_SET,True);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals('',obj.getString(buffer,F_OFFSET_1));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(VAL_1,obj.getString(buffer,F_OFFSET_1));
          SetBit(startBuffer^,BIT_ORDER_SET,False);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals('',obj.getString(buffer,F_OFFSET_1));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(VAL_1,obj.getString(buffer,F_OFFSET_1));
        SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
          SetBit(startBuffer^,BIT_ORDER_SET,True);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals('',obj.getString(buffer,F_OFFSET_1));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals('',obj.getString(buffer,F_OFFSET_1));
          SetBit(startBuffer^,BIT_ORDER_SET,False);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals('',obj.getString(buffer,F_OFFSET_1));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals('',obj.getString(buffer,F_OFFSET_1));

      valBuffer^^ := VAL_2;
        SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
          SetBit(startBuffer^,BIT_ORDER_SET,True);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals('',obj.getString(buffer,F_OFFSET_1));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(VAL_2,obj.getString(buffer,F_OFFSET_1));
          SetBit(startBuffer^,BIT_ORDER_SET,False);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals('',obj.getString(buffer,F_OFFSET_1));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(VAL_2,obj.getString(buffer,F_OFFSET_1));
        SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
          SetBit(startBuffer^,BIT_ORDER_SET,True);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals('',obj.getString(buffer,F_OFFSET_1));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals('',obj.getString(buffer,F_OFFSET_1));
          SetBit(startBuffer^,BIT_ORDER_SET,False);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals('',obj.getString(buffer,F_OFFSET_1));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals('',obj.getString(buffer,F_OFFSET_1));

      valBuffer^^ := VAL_3;
        SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
          SetBit(startBuffer^,BIT_ORDER_SET,True);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals('',obj.getString(buffer,F_OFFSET_1));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(VAL_3,obj.getString(buffer,F_OFFSET_1));
          SetBit(startBuffer^,BIT_ORDER_SET,False);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals('',obj.getString(buffer,F_OFFSET_1));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(VAL_3,obj.getString(buffer,F_OFFSET_1));
        SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
          SetBit(startBuffer^,BIT_ORDER_SET,True);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals('',obj.getString(buffer,F_OFFSET_1));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals('',obj.getString(buffer,F_OFFSET_1));
          SetBit(startBuffer^,BIT_ORDER_SET,False);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals('',obj.getString(buffer,F_OFFSET_1));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals('',obj.getString(buffer,F_OFFSET_1));

      valBuffer^^ := '';
    finally
      FreeMem(valBuffer^,SizeOf(PSDOString));
      valBuffer^ := nil;
    end;
end;

procedure TSDOStringField_Test.setBoolean();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer : PByte;
  valBuffer : PPSDOString;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);
  obj := Create_Field();

  obj.setBoolean(buffer,F_OFFSET_0,True);
  CheckEquals('1',valBuffer^^);

  obj.setBoolean(buffer,F_OFFSET_0,False);
  CheckEquals('0',valBuffer^^);
  // Clean up to avoid false MEM-LEAK
    CleanUpBuffer(valBuffer);

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);

    obj.setBoolean(buffer,F_OFFSET_1,True);
    CheckEquals('1',valBuffer^^);

    obj.setBoolean(buffer,F_OFFSET_1,False);
    CheckEquals('0',valBuffer^^);

    // Clean up to avoid false MEM-LEAK
    CleanUpBuffer(valBuffer);
end;

procedure TSDOStringField_Test.setByte();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = '123'; VAL_2 = '45'; VAL_3 = '0';
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDOString;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);
  obj := Create_Field();

  obj.setByte(buffer,F_OFFSET_0,StrToInt(VAL_1));
  CheckEquals(VAL_1,obj.getString(buffer,F_OFFSET_0));

  obj.setByte(buffer,F_OFFSET_0,StrToInt(VAL_2));
  CheckEquals(VAL_2,obj.getString(buffer,F_OFFSET_0));

  obj.setByte(buffer,F_OFFSET_0,StrToInt(VAL_3));
  CheckEquals(VAL_3,obj.getString(buffer,F_OFFSET_0));

  // Clean up to avoid false MEM-LEAK
  CleanUpBuffer(valBuffer);

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);

    obj.setByte(buffer,F_OFFSET_1,StrToInt(VAL_1));
    CheckEquals(VAL_1,obj.getString(buffer,F_OFFSET_1));

    obj.setByte(buffer,F_OFFSET_1,StrToInt(VAL_2));
    CheckEquals(VAL_2,obj.getString(buffer,F_OFFSET_1));

    obj.setByte(buffer,F_OFFSET_1,StrToInt(VAL_3));
    CheckEquals(VAL_3,obj.getString(buffer,F_OFFSET_1));

  // Clean up to avoid false MEM-LEAK
  CleanUpBuffer(valBuffer);
end;

procedure TSDOStringField_Test.setCharacter();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 'K'; VAL_2 = 'W'; VAL_3 = #0;
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDOString;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);
  obj := Create_Field();

  obj.setCharacter(buffer,F_OFFSET_0,VAL_1);
  CheckEquals(VAL_1,obj.getString(buffer,F_OFFSET_0));

  obj.setCharacter(buffer,F_OFFSET_0,VAL_2);
  CheckEquals(VAL_2,obj.getString(buffer,F_OFFSET_0));

  obj.setCharacter(buffer,F_OFFSET_0,VAL_3);
  CheckEquals(VAL_3,obj.getString(buffer,F_OFFSET_0));

  // Clean up to avoid false MEM-LEAK
  CleanUpBuffer(valBuffer);

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);

    obj.setCharacter(buffer,F_OFFSET_1,VAL_1);
    CheckEquals(VAL_1,obj.getString(buffer,F_OFFSET_1));

    obj.setCharacter(buffer,F_OFFSET_1,VAL_2);
    CheckEquals(VAL_2,obj.getString(buffer,F_OFFSET_1));

    obj.setCharacter(buffer,F_OFFSET_1,VAL_3);
    CheckEquals(VAL_3,obj.getString(buffer,F_OFFSET_1));

  // Clean up to avoid false MEM-LEAK
  CleanUpBuffer(valBuffer);
end;

procedure TSDOStringField_Test.setInteger();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = '12345'; VAL_2 = '-3476'; VAL_3 = '0';
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDOString;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);
  obj := Create_Field();

  obj.setInteger(buffer,F_OFFSET_0,StrToInt(VAL_1));
  CheckEquals(VAL_1,obj.getString(buffer,F_OFFSET_0));

  obj.setInteger(buffer,F_OFFSET_0,StrToInt(VAL_2));
  CheckEquals(VAL_2,obj.getString(buffer,F_OFFSET_0));

  obj.setInteger(buffer,F_OFFSET_0,StrToInt(VAL_3));
  CheckEquals(VAL_3,obj.getString(buffer,F_OFFSET_0));

  // Clean up to avoid false MEM-LEAK
  CleanUpBuffer(valBuffer);

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);

    obj.setInteger(buffer,F_OFFSET_1,StrToInt(VAL_1));
    CheckEquals(VAL_1,obj.getString(buffer,F_OFFSET_1));

    obj.setInteger(buffer,F_OFFSET_1,StrToInt(VAL_2));
    CheckEquals(VAL_2,obj.getString(buffer,F_OFFSET_1));

    obj.setInteger(buffer,F_OFFSET_1,StrToInt(VAL_3));
    CheckEquals(VAL_3,obj.getString(buffer,F_OFFSET_1));

  // Clean up to avoid false MEM-LEAK
  CleanUpBuffer(valBuffer);

end;

procedure TSDOStringField_Test.setLong();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = '123456852'; VAL_2 = '3489962255'; VAL_3 = '0';
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDOString;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);
  obj := Create_Field();

  obj.setLong(buffer,F_OFFSET_0,StrToInt64(VAL_1));
  CheckEquals(VAL_1,obj.getString(buffer,F_OFFSET_0));

  obj.setLong(buffer,F_OFFSET_0,StrToInt64(VAL_2));
  CheckEquals(VAL_2,obj.getString(buffer,F_OFFSET_0));

  obj.setLong(buffer,F_OFFSET_0,StrToInt64(VAL_3));
  CheckEquals(VAL_3,obj.getString(buffer,F_OFFSET_0));

  // Clean up to avoid false MEM-LEAK
  CleanUpBuffer(valBuffer);

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);

    obj.setLong(buffer,F_OFFSET_1,StrToInt64(VAL_1));
    CheckEquals(VAL_1,obj.getString(buffer,F_OFFSET_1));

    obj.setLong(buffer,F_OFFSET_1,StrToInt64(VAL_2));
    CheckEquals(VAL_2,obj.getString(buffer,F_OFFSET_1));

    obj.setLong(buffer,F_OFFSET_1,StrToInt64(VAL_3));
    CheckEquals(VAL_3,obj.getString(buffer,F_OFFSET_1));

  // Clean up to avoid false MEM-LEAK
  CleanUpBuffer(valBuffer);
end;

procedure TSDOStringField_Test.setNull();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 'azerty12345'; VAL_2 = 'xyz-9876'; VAL_3 = '';
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDOString;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);
  obj := Create_Field();

  SetBit(startBuffer^,BIT_ORDER_SET,False);
  SetBit(startBuffer^,BIT_ORDER_NULL,False);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
    obj.setNull(buffer,F_OFFSET_0);
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

  SetBit(startBuffer^,BIT_ORDER_SET,True);
  SetBit(startBuffer^,BIT_ORDER_NULL,False);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
    obj.setNull(buffer,F_OFFSET_0);
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

  SetBit(startBuffer^,BIT_ORDER_SET,True);
  SetBit(startBuffer^,BIT_ORDER_NULL,True);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
    obj.setNull(buffer,F_OFFSET_0);
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));


    GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := VAL_1;
    obj.setNull(buffer,F_OFFSET_0);
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

    GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := VAL_2;
    obj.setNull(buffer,F_OFFSET_0);
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL)); 
    Check(valBuffer^ = nil);

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);

    SetBit(startBuffer^,BIT_ORDER_SET,False);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
      obj.setNull(buffer,F_OFFSET_1);
        CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
        CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
      obj.setNull(buffer,F_OFFSET_1);
        CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
        CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,True);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
      obj.setNull(buffer,F_OFFSET_1);
        CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
        CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

    GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := VAL_1;
    obj.setNull(buffer,F_OFFSET_1);
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

    GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := VAL_2;
    obj.setNull(buffer,F_OFFSET_1);
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);
end;

procedure TSDOStringField_Test.setShort();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = '6852'; VAL_2 = '-255'; VAL_3 = '0';
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDOString;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);
  obj := Create_Field();

  obj.setShort(buffer,F_OFFSET_0,StrToInt(VAL_1));
  CheckEquals(VAL_1,obj.getString(buffer,F_OFFSET_0));

  obj.setShort(buffer,F_OFFSET_0,StrToInt(VAL_2));
  CheckEquals(VAL_2,obj.getString(buffer,F_OFFSET_0));

  obj.setShort(buffer,F_OFFSET_0,StrToInt(VAL_3));
  CheckEquals(VAL_3,obj.getString(buffer,F_OFFSET_0));

  // Clean up to avoid false MEM-LEAK
  CleanUpBuffer(valBuffer);

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);

    obj.setShort(buffer,F_OFFSET_1,StrToInt(VAL_1));
    CheckEquals(VAL_1,obj.getString(buffer,F_OFFSET_1));

    obj.setShort(buffer,F_OFFSET_1,StrToInt(VAL_2));
    CheckEquals(VAL_2,obj.getString(buffer,F_OFFSET_1));

    obj.setShort(buffer,F_OFFSET_1,StrToInt(VAL_3));
    CheckEquals(VAL_3,obj.getString(buffer,F_OFFSET_1));

  // Clean up to avoid false MEM-LEAK
  CleanUpBuffer(valBuffer);
end;

procedure TSDOStringField_Test.setString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 'azerty12345'; VAL_2 = 'xyz-9876'; VAL_3 = '';
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDOString;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);
  obj := Create_Field();

  obj.setString(buffer,F_OFFSET_0,'');
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
  CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_BUFFER), 'set bit');

  SetBit(startBuffer^,BIT_ORDER_NULL,True);
  obj.setString(buffer,F_OFFSET_0,VAL_1);
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
  CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_BUFFER), 'set bit');
  CheckEquals(True, ( valBuffer^ <> nil ), 'string buffer' );
  CheckEquals(VAL_1,valBuffer^^);

  obj.setString(buffer,F_OFFSET_0,VAL_2);
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
  CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_BUFFER), 'set bit');
  CheckEquals(True, ( valBuffer^ <> nil ), 'string buffer' );
  CheckEquals(VAL_2,valBuffer^^);

  SetBit(startBuffer^,BIT_ORDER_NULL,True);
  obj.setString(buffer,F_OFFSET_0,VAL_3);
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
  CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_BUFFER), 'set bit');
  CheckEquals(True, ( valBuffer^ <> nil ), 'string buffer' );
  CheckEquals(VAL_3,valBuffer^^);

  // Clean up to avoid false MEM-LEAK
  CleanUpBuffer(valBuffer);

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);

    SetBit(startBuffer^,BIT_ORDER_NULL,True);
    obj.setString(buffer,F_OFFSET_1,'');
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
    CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_BUFFER), 'set bit');

    obj.setString(buffer,F_OFFSET_1,VAL_1);
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
    CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_BUFFER), 'set bit');
    CheckEquals(True, ( valBuffer^ <> nil ), 'string buffer' );
    CheckEquals(VAL_1,valBuffer^^);

    SetBit(startBuffer^,BIT_ORDER_NULL,True);
    obj.setString(buffer,F_OFFSET_1,VAL_2);
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
    CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_BUFFER), 'set bit');
    CheckEquals(True, ( valBuffer^ <> nil ), 'string buffer' );
    CheckEquals(VAL_2,valBuffer^^);

    obj.setString(buffer,F_OFFSET_1,VAL_3);
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
    CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_BUFFER), 'set bit');
    CheckEquals(True, ( valBuffer^ <> nil ), 'string buffer' );
    CheckEquals(VAL_3,valBuffer^^);
    // Clean up to avoid false MEM-LEAK
    CleanUpBuffer(valBuffer);

end;

procedure TSDOStringField_Test.setString_unset_setStrin;
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 'azerty12345'; VAL_2 = 'xyz-9876'; VAL_3 = '';
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDOString;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);
  obj := Create_Field();

  obj.setString(buffer,F_OFFSET_0,'');
    obj.unset(buffer,F_OFFSET_0);
      obj.setString(buffer,F_OFFSET_0,'');
        CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
        CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
        CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_BUFFER), 'set bit');
  valBuffer^^ := ''; FreeMem(valBuffer^,SizeOf(PSDOString));
end;

procedure TSDOStringField_Test.unset();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 'azerty12345'; VAL_2 = 'xyz-9876'; VAL_3 = '';
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDOString;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);
  obj := Create_Field();


  SetBit(startBuffer^,BIT_ORDER_SET,False);
  SetBit(startBuffer^,BIT_ORDER_NULL,False);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
    obj.unset(buffer,F_OFFSET_0);
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

  SetBit(startBuffer^,BIT_ORDER_SET,True);
  SetBit(startBuffer^,BIT_ORDER_NULL,False);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
    obj.unset(buffer,F_OFFSET_0);
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

  SetBit(startBuffer^,BIT_ORDER_SET,True);
  SetBit(startBuffer^,BIT_ORDER_NULL,True);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
    obj.unset(buffer,F_OFFSET_0);
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

    GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := VAL_1;
    obj.unset(buffer,F_OFFSET_0);
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(
        True(*this is necessary for default value management*),
        IsBitON(startBuffer^,BIT_ORDER_BUFFER),
        'BIT_ORDER_SET, this is necessary for default value management'
      );
      CheckNotEquals(PtrUInt(nil), PtrUInt(valBuffer^), 'The buffer must remains, this is necessary for default value management');
      CheckEquals(VAL_1, valBuffer^^);
      valBuffer^^ := ''; FreeMem(valBuffer^,SizeOf(PSDOString));

    GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);//startBuffer^ := 2;
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := VAL_2;
    obj.unset(buffer,F_OFFSET_0);
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_BUFFER),'BIT_ORDER_SET, this is necessary for default value management');
      CheckNotEquals(PtrUInt(nil), PtrUInt(valBuffer^), 'The buffer must remains, this is necessary for default value management');
      CheckEquals(VAL_2, valBuffer^^);
      valBuffer^^ := ''; FreeMem(valBuffer^,SizeOf(PSDOString));

    GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);//startBuffer^ := 2;
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := VAL_3;
    obj.unset(buffer,F_OFFSET_0);
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_BUFFER),'BIT_ORDER_SET, this is necessary for default value management');
      CheckNotEquals(PtrUInt(nil), PtrUInt(valBuffer^), 'The buffer must remains, this is necessary for default value management');
      CheckEquals(VAL_3, valBuffer^^);
      valBuffer^^ := ''; FreeMem(valBuffer^,SizeOf(PSDOString));

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOString(tmpBuffer);

    SetBit(startBuffer^,BIT_ORDER_SET,False);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
      obj.unset(buffer,F_OFFSET_1);
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
      obj.unset(buffer,F_OFFSET_1);
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,True);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
      obj.unset(buffer,F_OFFSET_1);
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

    GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := VAL_1;
    obj.unset(buffer,F_OFFSET_1);
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_BUFFER),'BIT_ORDER_SET, this is necessary for default value management');
      CheckNotEquals(PtrUInt(nil), PtrUInt(valBuffer^), 'The buffer must remains, this is necessary for default value management');
      CheckEquals(VAL_1, valBuffer^^);
      valBuffer^^ := ''; FreeMem(valBuffer^,SizeOf(PSDOString));


    GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);//startBuffer^ := 2;
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := VAL_2;
    obj.unset(buffer,F_OFFSET_1);
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_BUFFER),'BIT_ORDER_SET, this is necessary for default value management');
      CheckNotEquals(PtrUInt(nil), PtrUInt(valBuffer^), 'The buffer must remains, this is necessary for default value management');
      CheckEquals(VAL_2, valBuffer^^);
      valBuffer^^ := ''; FreeMem(valBuffer^,SizeOf(PSDOString));

    GetMem(valBuffer^,SizeOf(PSDOString)); FillChar(valBuffer^^,SizeOf(TSDOString),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := VAL_3;
    obj.unset(buffer,F_OFFSET_1);
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_BUFFER),'BIT_ORDER_SET, this is necessary for default value management');
      CheckNotEquals(PtrUInt(nil), PtrUInt(valBuffer^), 'The buffer must remains, this is necessary for default value management');
      CheckEquals(VAL_3, valBuffer^^);
      valBuffer^^ := ''; FreeMem(valBuffer^,SizeOf(PSDOString));

end;

{ TSDOBaseDataObject_Test }

class function TSDOBaseDataObject_Test.Create_Factory() : ISDODataFactory;
var
  typ, typ2 : ISDOType;
begin
  Result := TSDODataFactory.Create();
  Result.AddType(s_URI_1,s_TYPE_1,[]);
  Result.AddType(s_URI_1,s_TYPE_2,[]);
  Result.AddType(s_URI_3,s_TYPE_3,[]);
  typ := Result.getType(s_URI_1,s_TYPE_1);
    Result.addProperty(typ,s_PROP_BOOL_1,sdo_namespace,'Boolean',[]);
    Result.addProperty(typ,s_PROP_INTEGER_1,sdo_namespace,'Integer',[]);
    Result.addProperty(typ,s_PROP_STR_1,sdo_namespace,'String',[]);

    Result.addProperty(typ,s_PROP_BOOL_2,sdo_namespace,'Boolean',[]);
    Result.addProperty(typ,s_PROP_INTEGER_2,sdo_namespace,'Integer',[]);
    Result.addProperty(typ,s_PROP_STR_2,sdo_namespace,'String',[]);
    Result.addProperty(typ,s_PROP_OBJ_CONT,s_URI_1,s_TYPE_2,[]);
    Result.addProperty(typ,s_PROP_OBJ_REF,s_URI_1,s_TYPE_2,[]);

  typ2 := Result.getType(s_URI_1,s_TYPE_2);
    Result.addProperty(typ2,s_PROP_BOOL_A,sdo_namespace,'Boolean',[]);
    Result.addProperty(typ2,s_PROP_INTEGER_A,sdo_namespace,'Integer',[]);
    Result.addProperty(typ2,s_PROP_STR_A,sdo_namespace,'String',[]);
    Result.addProperty(typ2,s_PROP_OBJ_CONT,s_URI_3,s_TYPE_3,[]);
end;

function TSDOBaseDataObject_Test.Create_Field() : ISDOField;
begin
  Result := TSDOObjectField.Create() as ISDOField;
end;

procedure TSDOBaseDataObject_Test.getDataObject();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDODataObject;
  val_1, val_2, val_3 : ISDODataObject;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDODataObject(tmpBuffer);
  obj := Create_Field();
  val_1 := FFactory.createNew(s_URI_1,s_TYPE_1) as ISDODataObject;
  val_2 := FFactory.createNew(s_URI_1,s_TYPE_2) as ISDODataObject;
  val_3 := nil;

  Check(nil = obj.getDataObject(buffer,F_OFFSET_0));

  SetBit(startBuffer^,BIT_ORDER_SET,True);

  GetMem(valBuffer^,SizeOf(PSDODataObject)); FillChar(valBuffer^^,SizeOf(ISDODataObject),#0);
  try
    valBuffer^^ := val_1;
      SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
        SetBit(startBuffer^,BIT_ORDER_SET,True);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(VAL_1),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));
        SetBit(startBuffer^,BIT_ORDER_SET,False);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(VAL_1),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));
      SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
        SetBit(startBuffer^,BIT_ORDER_SET,True);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));
        SetBit(startBuffer^,BIT_ORDER_SET,False);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));

    valBuffer^^ := val_2;
      SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
        SetBit(startBuffer^,BIT_ORDER_SET,True);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(val_2),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));
        SetBit(startBuffer^,BIT_ORDER_SET,False);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(val_2),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));
      SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
        SetBit(startBuffer^,BIT_ORDER_SET,True);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));
        SetBit(startBuffer^,BIT_ORDER_SET,False);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));

    valBuffer^^ := val_3;
      SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
        SetBit(startBuffer^,BIT_ORDER_SET,True);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(val_3),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));
        SetBit(startBuffer^,BIT_ORDER_SET,False);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(val_3),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));
      SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
        SetBit(startBuffer^,BIT_ORDER_SET,True);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));
        SetBit(startBuffer^,BIT_ORDER_SET,False);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_0)));

    valBuffer^^ := nil;

  finally
    FreeMem(valBuffer^,SizeOf(PSDODataObject));
    valBuffer^ := nil;
  end;

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDODataObject(tmpBuffer);
    Check(nil = obj.getDataObject(buffer,F_OFFSET_0));

    GetMem(valBuffer^,SizeOf(PSDODataObject)); FillChar(valBuffer^^,SizeOf(ISDODataObject),#0);
    try
      SetBit(startBuffer^,BIT_ORDER_SET,True);
      valBuffer^^ := val_1;
        SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
          SetBit(startBuffer^,BIT_ORDER_SET,True);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(val_1),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));
          SetBit(startBuffer^,BIT_ORDER_SET,False);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(val_1),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));
        SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
          SetBit(startBuffer^,BIT_ORDER_SET,True);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));
          SetBit(startBuffer^,BIT_ORDER_SET,False);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));

      valBuffer^^ := val_2;
        SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
          SetBit(startBuffer^,BIT_ORDER_SET,True);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(val_2),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));
          SetBit(startBuffer^,BIT_ORDER_SET,False);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(val_2),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));
        SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
          SetBit(startBuffer^,BIT_ORDER_SET,True);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));
          SetBit(startBuffer^,BIT_ORDER_SET,False);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));

      valBuffer^^ := val_3;
        SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
          SetBit(startBuffer^,BIT_ORDER_SET,True);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(val_3),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));
          SetBit(startBuffer^,BIT_ORDER_SET,False);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(val_3),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));
        SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
          SetBit(startBuffer^,BIT_ORDER_SET,True);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));
          SetBit(startBuffer^,BIT_ORDER_SET,False);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(nil),PtrInt(obj.getDataObject(buffer,F_OFFSET_1)));
    finally
      FreeMem(valBuffer^,SizeOf(PSDODataObject));
      valBuffer^ := nil;
    end;
end;

procedure TSDOBaseDataObject_Test.setDataObject();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDODataObject;
  val_1, val_2, val_3 : ISDODataObject;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDODataObject(tmpBuffer);
  obj := Create_Field();
  val_1 := FFactory.createNew(s_URI_1,s_TYPE_1) as ISDODataObject;
  val_2 := FFactory.createNew(s_URI_1,s_TYPE_2) as ISDODataObject;
  val_3 := nil;

  obj.setDataObject(buffer,F_OFFSET_0,nil);
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
  CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');

  obj.setDataObject(buffer,F_OFFSET_0,val_1);
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
  CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
  CheckEquals(True, ( valBuffer^ <> nil ), 'object buffer' );
  Check(val_1  = valBuffer^^);

  obj.setDataObject(buffer,F_OFFSET_0,val_2);
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
  CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
  CheckEquals(True, ( valBuffer^ <> nil ), 'object buffer' );
  Check(val_2 = valBuffer^^);

  obj.setDataObject(buffer,F_OFFSET_0,val_3);
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
  CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
  CheckEquals(True, ( valBuffer^ <> nil ), 'object buffer' );
  Check(val_3 = valBuffer^^);

  valBuffer^^ := nil;
  FreeMem(valBuffer^,SizeOf(PSDODataObject));
  valBuffer^ := nil;

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDODataObject(tmpBuffer);

    obj.setDataObject(buffer,F_OFFSET_1,nil);
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');

    obj.setDataObject(buffer,F_OFFSET_1,val_1);
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
    CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
    CheckEquals(True, ( valBuffer^ <> nil ), 'object buffer' );
    Check(val_1 = valBuffer^^);

    obj.setDataObject(buffer,F_OFFSET_1,val_2);
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
    CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
    CheckEquals(True, ( valBuffer^ <> nil ), 'object buffer' );
    Check(val_2 = valBuffer^^);

    obj.setDataObject(buffer,F_OFFSET_1,val_3);
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
    CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
    CheckEquals(True, ( valBuffer^ <> nil ), 'object buffer' );
    Check(val_3 = valBuffer^^);

  valBuffer^^ := nil;
  FreeMem(valBuffer^,SizeOf(PSDODataObject));
  valBuffer^ := nil;    
end;

procedure TSDOBaseDataObject_Test.setNull();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDODataObject;
  val_1, val_2, val_3 : ISDODataObject;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDODataObject(tmpBuffer);
  obj := Create_Field();
  val_1 := FFactory.createNew(s_URI_1,s_TYPE_1) as ISDODataObject;
  val_2 := FFactory.createNew(s_URI_1,s_TYPE_2) as ISDODataObject;
  val_3 := nil;


  SetBit(startBuffer^,BIT_ORDER_SET,False);
  SetBit(startBuffer^,BIT_ORDER_NULL,False);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
    obj.setNull(buffer,F_OFFSET_0);
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

  SetBit(startBuffer^,BIT_ORDER_SET,True);
  SetBit(startBuffer^,BIT_ORDER_NULL,False);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
    obj.setNull(buffer,F_OFFSET_0);
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

  SetBit(startBuffer^,BIT_ORDER_SET,True);
  SetBit(startBuffer^,BIT_ORDER_NULL,True);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
    obj.setNull(buffer,F_OFFSET_0);
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));


    GetMem(valBuffer^,SizeOf(PSDODataObject)); FillChar(valBuffer^^,SizeOf(ISDODataObject),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_1;
    obj.setNull(buffer,F_OFFSET_0);
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

    GetMem(valBuffer^,SizeOf(PSDODataObject)); FillChar(valBuffer^^,SizeOf(ISDODataObject),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_2;
    obj.setNull(buffer,F_OFFSET_0);
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

    GetMem(valBuffer^,SizeOf(PSDODataObject)); FillChar(valBuffer^^,SizeOf(ISDODataObject),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_3;
    obj.setNull(buffer,F_OFFSET_0);
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDODataObject(tmpBuffer);

    SetBit(startBuffer^,BIT_ORDER_SET,False);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
      obj.setNull(buffer,F_OFFSET_1);
        CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
        CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
      obj.setNull(buffer,F_OFFSET_1);
        CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
        CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,True);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
      obj.setNull(buffer,F_OFFSET_1);
        CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
        CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

    GetMem(valBuffer^,SizeOf(PSDODataObject)); FillChar(valBuffer^^,SizeOf(ISDODataObject),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_1;
    obj.setNull(buffer,F_OFFSET_1);
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

    GetMem(valBuffer^,SizeOf(PSDODataObject)); FillChar(valBuffer^^,SizeOf(ISDODataObject),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_2;
    obj.setNull(buffer,F_OFFSET_1);
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

    GetMem(valBuffer^,SizeOf(PSDODataObject)); FillChar(valBuffer^^,SizeOf(ISDODataObject),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_3;
    obj.setNull(buffer,F_OFFSET_1);
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);
end;

procedure TSDOBaseDataObject_Test.SetUp();
begin
  inherited;
  FFactory := Create_Factory();
end;

procedure TSDOBaseDataObject_Test.TearDown;
begin
  FFactory := nil;
  inherited;
end;

procedure TSDOBaseDataObject_Test.unset();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDODataObject;
  val_1, val_2, val_3 : ISDODataObject;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDODataObject(tmpBuffer);
  obj := Create_Field();
  val_1 := FFactory.createNew(s_URI_1,s_TYPE_1) as ISDODataObject;
  val_2 := FFactory.createNew(s_URI_1,s_TYPE_2) as ISDODataObject;
  val_3 := nil;

  SetBit(startBuffer^,BIT_ORDER_SET,False);
  SetBit(startBuffer^,BIT_ORDER_NULL,False);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
    obj.unset(buffer,F_OFFSET_0);
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

  SetBit(startBuffer^,BIT_ORDER_SET,True);
  SetBit(startBuffer^,BIT_ORDER_NULL,False);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
    obj.unset(buffer,F_OFFSET_0);
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

  SetBit(startBuffer^,BIT_ORDER_SET,True);
  SetBit(startBuffer^,BIT_ORDER_NULL,True);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
    obj.unset(buffer,F_OFFSET_0);
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));


    GetMem(valBuffer^,SizeOf(PSDODataObject)); FillChar(valBuffer^^,SizeOf(ISDODataObject),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_1;
    obj.unset(buffer,F_OFFSET_0);
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

    GetMem(valBuffer^,SizeOf(PSDODataObject)); FillChar(valBuffer^^,SizeOf(ISDODataObject),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_2;
    obj.unset(buffer,F_OFFSET_0);
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

    GetMem(valBuffer^,SizeOf(PSDODataObject)); FillChar(valBuffer^^,SizeOf(ISDODataObject),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_3;
    obj.unset(buffer,F_OFFSET_0);
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDODataObject(tmpBuffer);

    SetBit(startBuffer^,BIT_ORDER_SET,False);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
      obj.unset(buffer,F_OFFSET_1);
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
      obj.unset(buffer,F_OFFSET_1);
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,True);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
      obj.unset(buffer,F_OFFSET_1);
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
  
    GetMem(valBuffer^,SizeOf(PSDODataObject)); FillChar(valBuffer^^,SizeOf(ISDODataObject),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_1;
    obj.unset(buffer,F_OFFSET_1);
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

    GetMem(valBuffer^,SizeOf(PSDODataObject)); FillChar(valBuffer^^,SizeOf(ISDODataObject),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_2;
    obj.unset(buffer,F_OFFSET_1);
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

    GetMem(valBuffer^,SizeOf(PSDODataObject)); FillChar(valBuffer^^,SizeOf(ISDODataObject),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_3;
    obj.unset(buffer,F_OFFSET_1);
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

end;

{ TSDOChangeSummaryField_Test }

class function TSDOChangeSummaryField_Test.Create_Factory: ISDODataFactory;
var
  typ, typ2 : ISDOType;
begin
  Result := TSDODataFactory.Create();
  Result.AddType(s_URI_1,s_TYPE_1,[]);
  Result.AddType(s_URI_1,s_TYPE_2,[]);
  Result.AddType(s_URI_3,s_TYPE_3,[]);
  typ := Result.getType(s_URI_1,s_TYPE_1);
    Result.addProperty(typ,s_PROP_BOOL_1,sdo_namespace,'Boolean',[]);
    Result.addProperty(typ,s_PROP_INTEGER_1,sdo_namespace,'Integer',[]);
    Result.addProperty(typ,s_PROP_STR_1,sdo_namespace,'String',[]);
    Result.addProperty(typ,s_PROP_CHANGE_SUMMARY,sdo_namespace,'ChangeSummary',[pfIsReadOnly]);

    Result.addProperty(typ,s_PROP_BOOL_2,sdo_namespace,'Boolean',[]);
    Result.addProperty(typ,s_PROP_INTEGER_2,sdo_namespace,'Integer',[]);
    Result.addProperty(typ,s_PROP_STR_2,sdo_namespace,'String',[]);
    Result.addProperty(typ,s_PROP_OBJ_CONT,s_URI_1,s_TYPE_2,[]);
    Result.addProperty(typ,s_PROP_OBJ_REF,s_URI_1,s_TYPE_2,[]);

  typ2 := Result.getType(s_URI_1,s_TYPE_2);
    Result.addProperty(typ2,s_PROP_BOOL_A,sdo_namespace,'Boolean',[]);
    Result.addProperty(typ2,s_PROP_INTEGER_A,sdo_namespace,'Integer',[]);
    Result.addProperty(typ2,s_PROP_STR_A,sdo_namespace,'String',[]);
    Result.addProperty(typ2,s_PROP_OBJ_CONT,s_URI_1,s_TYPE_1,[]);
end;

function TSDOChangeSummaryField_Test.Create_Field() : ISDOField;
begin
  Result := TSDOChangeSummaryField.Create();
end;

procedure TSDOChangeSummaryField_Test.getChangeSummary();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDOChangeSummary;
  val_1, val_2, val_3 : ISDOChangeSummary;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOChangeSummary(tmpBuffer);
  obj := Create_Field();
  val_1 := TSDOChangeSummary.Create(TSDOChangedDataObjectList.Create());;
  val_2 := TSDOChangeSummary.Create(TSDOChangedDataObjectList.Create());;
  val_3 := nil;

  Check(nil = obj.getChangeSummary(buffer,F_OFFSET_0));

  SetBit(startBuffer^,BIT_ORDER_SET,True);

  GetMem(valBuffer^,SizeOf(PSDOChangeSummary)); FillChar(valBuffer^^,SizeOf(ISDOChangeSummary),#0);
  try
    valBuffer^^ := val_1;
      SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
        SetBit(startBuffer^,BIT_ORDER_SET,True);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(VAL_1),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));
        SetBit(startBuffer^,BIT_ORDER_SET,False);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(VAL_1),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));
      SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
        SetBit(startBuffer^,BIT_ORDER_SET,True);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));
        SetBit(startBuffer^,BIT_ORDER_SET,False);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));

    valBuffer^^ := val_2;
      SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
        SetBit(startBuffer^,BIT_ORDER_SET,True);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(val_2),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));
        SetBit(startBuffer^,BIT_ORDER_SET,False);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(val_2),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));
      SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
        SetBit(startBuffer^,BIT_ORDER_SET,True);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));
        SetBit(startBuffer^,BIT_ORDER_SET,False);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));

    valBuffer^^ := val_3;
      SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
        SetBit(startBuffer^,BIT_ORDER_SET,True);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(val_3),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));
        SetBit(startBuffer^,BIT_ORDER_SET,False);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(val_3),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));
      SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
        SetBit(startBuffer^,BIT_ORDER_SET,True);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));
        SetBit(startBuffer^,BIT_ORDER_SET,False);
          SetBit(startBuffer^,BIT_ORDER_NULL,True);
            CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));
          SetBit(startBuffer^,BIT_ORDER_NULL,False);
            CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_0)));

    valBuffer^^ := nil;

  finally
    FreeMem(valBuffer^,SizeOf(PSDOChangeSummary));
    valBuffer^ := nil;
  end;

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOChangeSummary(tmpBuffer);
    Check(nil = obj.getChangeSummary(buffer,F_OFFSET_0));

    GetMem(valBuffer^,SizeOf(PSDOChangeSummary)); FillChar(valBuffer^^,SizeOf(ISDOChangeSummary),#0);
    try
      SetBit(startBuffer^,BIT_ORDER_SET,True);
      valBuffer^^ := val_1;
        SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
          SetBit(startBuffer^,BIT_ORDER_SET,True);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(val_1),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));
          SetBit(startBuffer^,BIT_ORDER_SET,False);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(val_1),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));
        SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
          SetBit(startBuffer^,BIT_ORDER_SET,True);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));
          SetBit(startBuffer^,BIT_ORDER_SET,False);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));

      valBuffer^^ := val_2;
        SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
          SetBit(startBuffer^,BIT_ORDER_SET,True);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(val_2),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));
          SetBit(startBuffer^,BIT_ORDER_SET,False);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(val_2),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));
        SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
          SetBit(startBuffer^,BIT_ORDER_SET,True);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));
          SetBit(startBuffer^,BIT_ORDER_SET,False);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));

      valBuffer^^ := val_3;
        SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
          SetBit(startBuffer^,BIT_ORDER_SET,True);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(val_3),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));
          SetBit(startBuffer^,BIT_ORDER_SET,False);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(val_3),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));
        SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
          SetBit(startBuffer^,BIT_ORDER_SET,True);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));
          SetBit(startBuffer^,BIT_ORDER_SET,False);
            SetBit(startBuffer^,BIT_ORDER_NULL,True);
              CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));
            SetBit(startBuffer^,BIT_ORDER_NULL,False);
              CheckEquals(PtrInt(nil),PtrInt(obj.getChangeSummary(buffer,F_OFFSET_1)));
    finally
      valBuffer^^ := nil;
      FreeMem(valBuffer^,SizeOf(PSDOChangeSummary));
      valBuffer^ := nil;
    end;  
end;

procedure TSDOChangeSummaryField_Test.setChangeSummary();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDOChangeSummary;
  val_1, val_2, val_3 : ISDOChangeSummary;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOChangeSummary(tmpBuffer);
  obj := Create_Field();
  val_1 := TSDOChangeSummary.Create(TSDOChangedDataObjectList.Create()) as ISDOChangeSummary;
  val_2 := TSDOChangeSummary.Create(TSDOChangedDataObjectList.Create()) as ISDOChangeSummary;
  val_3 := nil;

  obj.setChangeSummary(buffer,F_OFFSET_0,nil);
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
  CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');

  obj.setChangeSummary(buffer,F_OFFSET_0,val_1);
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
  CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
  CheckEquals(True, ( valBuffer^ <> nil ), 'object buffer' );
  Check(val_1  = valBuffer^^);

  obj.setChangeSummary(buffer,F_OFFSET_0,val_2);
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
  CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
  CheckEquals(True, ( valBuffer^ <> nil ), 'object buffer' );
  Check(val_2 = valBuffer^^);

  obj.setChangeSummary(buffer,F_OFFSET_0,val_3);
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
  CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
  CheckEquals(True, ( valBuffer^ <> nil ), 'object buffer' );
  Check(val_3 = valBuffer^^);

  valBuffer^^ := nil;
  FreeMem(valBuffer^,SizeOf(PSDOChangeSummary));
  valBuffer^ := nil;

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOChangeSummary(tmpBuffer);

    obj.setChangeSummary(buffer,F_OFFSET_1,nil);
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');

    obj.setChangeSummary(buffer,F_OFFSET_1,val_1);
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
    CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
    CheckEquals(True, ( valBuffer^ <> nil ), 'object buffer' );
    Check(val_1 = valBuffer^^);

    obj.setChangeSummary(buffer,F_OFFSET_1,val_2);
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
    CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
    CheckEquals(True, ( valBuffer^ <> nil ), 'object buffer' );
    Check(val_2 = valBuffer^^);

    obj.setChangeSummary(buffer,F_OFFSET_1,val_3);
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
    CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
    CheckEquals(True, ( valBuffer^ <> nil ), 'object buffer' );
    Check(val_3 = valBuffer^^);

  valBuffer^^ := nil;
  FreeMem(valBuffer^,SizeOf(PSDOChangeSummary));
  valBuffer^ := nil;    

end;

procedure TSDOChangeSummaryField_Test.setNull();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDOChangeSummary;
  val_1, val_2, val_3 : ISDOChangeSummary;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOChangeSummary(tmpBuffer);
  obj := Create_Field();
  val_1 := TSDOChangeSummary.Create(TSDOChangedDataObjectList.Create()) as ISDOChangeSummary;
  val_2 := TSDOChangeSummary.Create(TSDOChangedDataObjectList.Create()) as ISDOChangeSummary;
  val_3 := nil;


  SetBit(startBuffer^,BIT_ORDER_SET,False);
  SetBit(startBuffer^,BIT_ORDER_NULL,False);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
    obj.setNull(buffer,F_OFFSET_0);
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

  SetBit(startBuffer^,BIT_ORDER_SET,True);
  SetBit(startBuffer^,BIT_ORDER_NULL,False);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
    obj.setNull(buffer,F_OFFSET_0);
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

  SetBit(startBuffer^,BIT_ORDER_SET,True);
  SetBit(startBuffer^,BIT_ORDER_NULL,True);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
    obj.setNull(buffer,F_OFFSET_0);
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));


    GetMem(valBuffer^,SizeOf(PSDOChangeSummary)); FillChar(valBuffer^^,SizeOf(ISDOChangeSummary),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_1;
    obj.setNull(buffer,F_OFFSET_0);
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

    GetMem(valBuffer^,SizeOf(PSDOChangeSummary)); FillChar(valBuffer^^,SizeOf(ISDOChangeSummary),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_2;
    obj.setNull(buffer,F_OFFSET_0);
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

    GetMem(valBuffer^,SizeOf(PSDOChangeSummary)); FillChar(valBuffer^^,SizeOf(ISDOChangeSummary),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_3;
    obj.setNull(buffer,F_OFFSET_0);
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOChangeSummary(tmpBuffer);

    SetBit(startBuffer^,BIT_ORDER_SET,False);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
      obj.setNull(buffer,F_OFFSET_1);
        CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
        CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
      obj.setNull(buffer,F_OFFSET_1);
        CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
        CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,True);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
      obj.setNull(buffer,F_OFFSET_1);
        CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
        CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

    GetMem(valBuffer^,SizeOf(PSDOChangeSummary)); FillChar(valBuffer^^,SizeOf(ISDOChangeSummary),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_1;
    obj.setNull(buffer,F_OFFSET_1);
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

    GetMem(valBuffer^,SizeOf(PSDOChangeSummary)); FillChar(valBuffer^^,SizeOf(ISDOChangeSummary),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_2;
    obj.setNull(buffer,F_OFFSET_1);
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

    GetMem(valBuffer^,SizeOf(PSDOChangeSummary)); FillChar(valBuffer^^,SizeOf(ISDOChangeSummary),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_3;
    obj.setNull(buffer,F_OFFSET_1);
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(True,IsBitON(startBuffer^,BIT_ORDER_NULL));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);
end;

procedure TSDOChangeSummaryField_Test.SetUp;
begin
  inherited;
  FFactory := Create_Factory();
end;

procedure TSDOChangeSummaryField_Test.TearDown;
begin
  FFactory := nil;
  inherited;
end;

procedure TSDOChangeSummaryField_Test.unset();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  trueBuffer : array[0..200] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDOChangeSummary;
  val_1, val_2, val_3 : ISDOChangeSummary;
begin
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOChangeSummary(tmpBuffer);
  obj := Create_Field();
  val_1 := TSDOChangeSummary.Create(TSDOChangedDataObjectList.Create()) as ISDOChangeSummary;
  val_2 := TSDOChangeSummary.Create(TSDOChangedDataObjectList.Create()) as ISDOChangeSummary;
  val_3 := nil;

  SetBit(startBuffer^,BIT_ORDER_SET,False);
  SetBit(startBuffer^,BIT_ORDER_NULL,False);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
    obj.unset(buffer,F_OFFSET_0);
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

  SetBit(startBuffer^,BIT_ORDER_SET,True);
  SetBit(startBuffer^,BIT_ORDER_NULL,False);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
    obj.unset(buffer,F_OFFSET_0);
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

  SetBit(startBuffer^,BIT_ORDER_SET,True);
  SetBit(startBuffer^,BIT_ORDER_NULL,True);
  SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
    obj.unset(buffer,F_OFFSET_0);
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));


    GetMem(valBuffer^,SizeOf(PSDOChangeSummary)); FillChar(valBuffer^^,SizeOf(ISDOChangeSummary),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_1;
    obj.unset(buffer,F_OFFSET_0);
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

    GetMem(valBuffer^,SizeOf(PSDOChangeSummary)); FillChar(valBuffer^^,SizeOf(ISDOChangeSummary),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_2;
    obj.unset(buffer,F_OFFSET_0);
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

    GetMem(valBuffer^,SizeOf(PSDOChangeSummary)); FillChar(valBuffer^^,SizeOf(ISDOChangeSummary),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_3;
    obj.unset(buffer,F_OFFSET_0);
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOChangeSummary(tmpBuffer);

    SetBit(startBuffer^,BIT_ORDER_SET,False);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
      obj.unset(buffer,F_OFFSET_1);
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
      obj.unset(buffer,F_OFFSET_1);
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,True);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,False);
      obj.unset(buffer,F_OFFSET_1);
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
        CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));

    GetMem(valBuffer^,SizeOf(PSDOChangeSummary)); FillChar(valBuffer^^,SizeOf(ISDOChangeSummary),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_1;
    obj.unset(buffer,F_OFFSET_1);
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

    GetMem(valBuffer^,SizeOf(PSDOChangeSummary)); FillChar(valBuffer^^,SizeOf(ISDOChangeSummary),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_2;
    obj.unset(buffer,F_OFFSET_1);
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

    GetMem(valBuffer^,SizeOf(PSDOChangeSummary)); FillChar(valBuffer^^,SizeOf(ISDOChangeSummary),#0);
    SetBit(startBuffer^,BIT_ORDER_SET,True);
    SetBit(startBuffer^,BIT_ORDER_NULL,False);
    SetBit(startBuffer^,BIT_ORDER_BUFFER,True);
    valBuffer^^ := val_3;
    obj.unset(buffer,F_OFFSET_1);
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(startBuffer^,BIT_ORDER_BUFFER));
    Check(valBuffer^ = nil);

end;

{ TSDOByteField_Test }

function TSDOByteField_Test.Create_Field() : ISDOField;
begin
  Result := TSDOByteField.Create();
end;

procedure TSDOByteField_Test.getBoolean();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 123; VAL_2 = 45; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PByte(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PByte(tmpBuffer);
    valBuffer^ := VAL_1;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));

    valBuffer^ := VAL_2;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_1));
end;

procedure TSDOByteField_Test.getByte;
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 123; VAL_2 = 45; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PByte(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getByte(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getByte(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getByte(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getByte(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getByte(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getByte(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PByte(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getByte(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getByte(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getByte(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getByte(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getByte(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getByte(buffer,F_OFFSET_1));
end;

procedure TSDOByteField_Test.getCharacter();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = Ord(TSDOChar(125)); VAL_2 = Ord(TSDOChar(45)); VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PByte(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_1),obj.getCharacter(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_1),obj.getCharacter(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_2),obj.getCharacter(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_2),obj.getCharacter(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_3),obj.getCharacter(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_3),obj.getCharacter(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PByte(tmpBuffer);
    valBuffer^ := VAL_1;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_1),obj.getCharacter(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_1),obj.getCharacter(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_2),obj.getCharacter(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_2),obj.getCharacter(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_3),obj.getCharacter(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_3),obj.getCharacter(buffer,F_OFFSET_1));
end;

procedure TSDOByteField_Test.getInteger();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 123; VAL_2 = 45; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PByte(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getInteger(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getInteger(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getInteger(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getInteger(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getInteger(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getInteger(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PByte(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getInteger(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getInteger(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getInteger(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getInteger(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getInteger(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getInteger(buffer,F_OFFSET_1));
end;

procedure TSDOByteField_Test.getLong();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 123; VAL_2 = 45; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PByte(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getLong(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getLong(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getLong(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getLong(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getLong(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getLong(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PByte(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getLong(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getLong(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getLong(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getLong(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getLong(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getLong(buffer,F_OFFSET_1));
end;

procedure TSDOByteField_Test.getShort();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 123; VAL_2 = 55; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PByte(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getShort(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getShort(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getShort(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getShort(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getShort(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getShort(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PByte(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getShort(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getShort(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getShort(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getShort(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getShort(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getShort(buffer,F_OFFSET_1));
end;

procedure TSDOByteField_Test.getString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 123; VAL_2 = 45; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PByte(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_1),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_1),obj.getString(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_2),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_2),obj.getString(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_3),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_3),obj.getString(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PByte(tmpBuffer);
    valBuffer^ := VAL_1;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_1),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_1),obj.getString(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_2),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_2),obj.getString(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_3),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_3),obj.getString(buffer,F_OFFSET_1));
end;

procedure TSDOByteField_Test.setBoolean();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PByte(tmpBuffer);
  obj := Create_Field();

  obj.setBoolean(buffer,F_OFFSET_0,True);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(1,valBuffer^);

  obj.setBoolean(buffer,F_OFFSET_0,False);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(0,valBuffer^);

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PByte(tmpBuffer);

    obj.setBoolean(buffer,F_OFFSET_1,True);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(1,valBuffer^);

    obj.setBoolean(buffer,F_OFFSET_1,False);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(0,valBuffer^);
end;

procedure TSDOByteField_Test.setByte();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 123; VAL_2 = 45; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PByte(tmpBuffer);
  obj := Create_Field();

  obj.setByte(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setByte(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setByte(buffer,F_OFFSET_0,VAL_3);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PByte(tmpBuffer);

    obj.setByte(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setByte(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setByte(buffer,F_OFFSET_1,VAL_3);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOByteField_Test.setCharacter();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = Ord(TSDOChar(125)); VAL_2 = Ord(TSDOChar(45)); VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PByte(tmpBuffer);
  obj := Create_Field();

  obj.setCharacter(buffer,F_OFFSET_0,TSDOChar(VAL_1));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setCharacter(buffer,F_OFFSET_0,TSDOChar(VAL_2));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PByte(tmpBuffer);

    obj.setCharacter(buffer,F_OFFSET_1,TSDOChar(VAL_1));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setCharacter(buffer,F_OFFSET_1,TSDOChar(VAL_2));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);
end;

procedure TSDOByteField_Test.setInteger();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 123; VAL_2 = 45; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PByte(tmpBuffer);
  obj := Create_Field();

  obj.setInteger(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setInteger(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setInteger(buffer,F_OFFSET_0,VAL_3);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PByte(tmpBuffer);

    obj.setInteger(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setInteger(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setInteger(buffer,F_OFFSET_1,VAL_3);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOByteField_Test.setLong();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 123; VAL_2 = 35; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PByte(tmpBuffer);
  obj := Create_Field();

  obj.setLong(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setLong(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setLong(buffer,F_OFFSET_0,VAL_3);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PByte(tmpBuffer);

    obj.setLong(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setLong(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setShort(buffer,F_OFFSET_1,VAL_3);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOByteField_Test.setShort();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 123; VAL_2 = 35; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PByte(tmpBuffer);
  obj := Create_Field();

  obj.setShort(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setShort(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setShort(buffer,F_OFFSET_0,VAL_3);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PByte(tmpBuffer);

    obj.setShort(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setShort(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setShort(buffer,F_OFFSET_1,VAL_3);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOByteField_Test.setString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 123; VAL_2 = 45; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PByte;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PByte(tmpBuffer);
  obj := Create_Field();

  obj.setString(buffer,F_OFFSET_0,IntToStr(VAL_1));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setString(buffer,F_OFFSET_0,IntToStr(VAL_2));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PByte(tmpBuffer);

    obj.setString(buffer,F_OFFSET_1,IntToStr(VAL_1));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setString(buffer,F_OFFSET_1,IntToStr(VAL_2));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);
end;

{ TSDODateField_Test }

procedure TSDODateField_Test.CheckEquals(expected, actual: TSDODate; msg: string; const AStrict : Boolean);
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

function TSDODateField_Test.Create_Field() : ISDOField;
begin
  Result := TSDODateField.Create() as ISDOField;
end;

procedure TSDODateField_Test.getDate();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_2 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_3 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );

  procedure SetConstants();
  var
    d : TSDODate;
  begin
    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(1976,10,12,23,34,45,56);
    d.HourOffset := 5;
    d.MinuteOffset := 6;
    PSDODate(@VAL_1)^ := d;

    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(2008,7,8,9,10,11,12);
    d.HourOffset := 0;
    d.MinuteOffset := 13;
    PSDODate(@VAL_3)^ := d;
  end;

var
  obj : ISDOField;
  trueBuffer : array[0..( 3 * SizeOf(TSDODate))] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDODate;
begin
  SetConstants();

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDODate(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(ZERO_DATE,obj.getDate(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getDate(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(ZERO_DATE,obj.getDate(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getDate(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(ZERO_DATE,obj.getDate(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getDate(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(ZERO_DATE,obj.getDate(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getDate(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(ZERO_DATE,obj.getDate(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getDate(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(ZERO_DATE,obj.getDate(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getDate(buffer,F_OFFSET_0));

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDODate(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(ZERO_DATE,obj.getDate(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getDate(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(ZERO_DATE,obj.getDate(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getDate(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(ZERO_DATE,obj.getDate(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getDate(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(ZERO_DATE,obj.getDate(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getDate(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(ZERO_DATE,obj.getDate(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getDate(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(ZERO_DATE,obj.getDate(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getDate(buffer,F_OFFSET_1));
end;

procedure TSDODateField_Test.getString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_2 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_3 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );

  procedure SetConstants();
  var
    d : TSDODate;
  begin
    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(1976,10,12,23,34,45,56);
    d.HourOffset := 5;
    d.MinuteOffset := 6;
    PSDODate(@VAL_1)^ := d;

    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(2008,7,8,9,10,11,12);
    d.HourOffset := 0;
    d.MinuteOffset := 13;
    PSDODate(@VAL_3)^ := d;
  end;

var
  obj : ISDOField;
  trueBuffer : array[0..( 3 * SizeOf(TSDODate))] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDODate;
begin
  SetConstants();

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDODate(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(xsd_DateTimeToStr(VAL_1,xdkDateTime),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(xsd_DateTimeToStr(VAL_1,xdkDateTime),obj.getString(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(xsd_DateTimeToStr(VAL_2,xdkDateTime),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(xsd_DateTimeToStr(VAL_2,xdkDateTime),obj.getString(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(xsd_DateTimeToStr(VAL_3,xdkDateTime),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(xsd_DateTimeToStr(VAL_3,xdkDateTime),obj.getString(buffer,F_OFFSET_0));

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDODate(tmpBuffer);
    valBuffer^ := VAL_1;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(xsd_DateTimeToStr(VAL_1,xdkDateTime),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(xsd_DateTimeToStr(VAL_1,xdkDateTime),obj.getString(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(xsd_DateTimeToStr(VAL_2,xdkDateTime),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(xsd_DateTimeToStr(VAL_2,xdkDateTime),obj.getString(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(xsd_DateTimeToStr(VAL_3,xdkDateTime),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(xsd_DateTimeToStr(VAL_3,xdkDateTime),obj.getString(buffer,F_OFFSET_1));
end;

procedure TSDODateField_Test.setDate();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_2 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_3 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );

  procedure SetConstants();
  var
    d : TSDODate;
  begin
    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(1976,10,12,23,34,45,56);
    d.HourOffset := 5;
    d.MinuteOffset := 6;
    PSDODate(@VAL_1)^ := d;

    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(2008,7,8,9,10,11,12);
    d.HourOffset := 0;
    d.MinuteOffset := 13;
    PSDODate(@VAL_3)^ := d;
  end;

var
  obj : ISDOField;
  trueBuffer : array[0..( 3 * SizeOf(TSDODate))] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDODate;
begin
  SetConstants();

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDODate(tmpBuffer);
  obj := Create_Field();

  obj.setDate(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setDate(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setDate(buffer,F_OFFSET_0,VAL_3);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDODate(tmpBuffer);

    obj.setDate(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setDate(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setDate(buffer,F_OFFSET_1,VAL_3);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDODateField_Test.setString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_2 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_3 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );

  procedure SetConstants();
  var
    d : TSDODate;
  begin
    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(1976,10,12,23,34,45,56);
    d.HourOffset := 5;
    d.MinuteOffset := 6;
    PSDODate(@VAL_1)^ := d;

    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(2008,7,8,9,10,11,12);
    d.HourOffset := 0;
    d.MinuteOffset := 13;
    PSDODate(@VAL_3)^ := d;
  end;

var
  obj : ISDOField;
  trueBuffer : array[0..( 3 * SizeOf(TSDODate))] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDODate;
begin
  SetConstants();

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDODate(tmpBuffer);
  obj := Create_Field();

  obj.setString(buffer,F_OFFSET_0,xsd_DateTimeToStr(VAL_1,xdkDateTime));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^,'',False);

  obj.setString(buffer,F_OFFSET_0,xsd_DateTimeToStr(VAL_2,xdkDateTime));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDODate(tmpBuffer);

    obj.setString(buffer,F_OFFSET_1,xsd_DateTimeToStr(VAL_1,xdkDateTime));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^,'',False);

    obj.setString(buffer,F_OFFSET_1,xsd_DateTimeToStr(VAL_2,xdkDateTime));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);
end;

{$IFDEF HAS_SDO_CHAR}
{ TSDOCharField_Test }

function TSDOCharField_Test.Create_Field() : ISDOField;
begin
  Result := TSDOCharField.Create() as ISDOField;
end;

procedure TSDOCharField_Test.getBoolean();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = TSDOChar('X'); VAL_2 = TSDOChar('Y'); VAL_3 = TSDOChar(#0);
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOChar;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOChar(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOChar(tmpBuffer);
    valBuffer^ := VAL_1;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));

    valBuffer^ := VAL_2;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_1));
end;

procedure TSDOCharField_Test.getByte();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = TSDOChar(#123); VAL_2 = TSDOChar(#45); VAL_3 = TSDOChar(#0);
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOChar;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOChar(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(VAL_1),obj.getByte(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(VAL_1),obj.getByte(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(VAL_2),obj.getByte(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(VAL_2),obj.getByte(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(VAL_3),obj.getByte(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(VAL_3),obj.getByte(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOChar(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_1),obj.getByte(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_1),obj.getByte(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_2),obj.getByte(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_2),obj.getByte(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_3),obj.getByte(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_3),obj.getByte(buffer,F_OFFSET_1));
end;

procedure TSDOCharField_Test.getCharacter();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = TSDOChar('a'); VAL_2 = TSDOChar('i'); VAL_3 = TSDOChar('1');
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOChar;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOChar(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(#0,obj.getCharacter(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getCharacter(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(#0,obj.getCharacter(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getCharacter(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(#0,obj.getCharacter(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getCharacter(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(#0,obj.getCharacter(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getCharacter(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(#0,obj.getCharacter(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getCharacter(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(#0,obj.getCharacter(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getCharacter(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOChar(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(#0,obj.getCharacter(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getCharacter(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(#0,obj.getCharacter(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getCharacter(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(#0,obj.getCharacter(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getCharacter(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(#0,obj.getCharacter(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getCharacter(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(#0,obj.getCharacter(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getCharacter(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(#0,obj.getCharacter(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getCharacter(buffer,F_OFFSET_1));
end;

procedure TSDOCharField_Test.getInteger();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = TSDOChar(#97); VAL_2 = TSDOChar(#48); VAL_3 = TSDOChar(#100);
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOChar;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOChar(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(VAL_1),obj.getInteger(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(Ord(VAL_1)),obj.getInteger(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(VAL_2),obj.getInteger(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(VAL_2),obj.getInteger(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(VAL_3),obj.getInteger(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(VAL_3),obj.getInteger(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOChar(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_1),obj.getInteger(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_1),obj.getInteger(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_2),obj.getInteger(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_2),obj.getInteger(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_3),obj.getInteger(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_3),obj.getInteger(buffer,F_OFFSET_1));
end;

procedure TSDOCharField_Test.getLong();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = TSDOChar(#97); VAL_2 = TSDOChar(#48); VAL_3 = TSDOChar(#100);
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOChar;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOChar(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(VAL_1),obj.getLong(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(Ord(VAL_1)),obj.getLong(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(VAL_2),obj.getLong(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(VAL_2),obj.getLong(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(VAL_3),obj.getLong(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(VAL_3),obj.getLong(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOChar(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_1),obj.getLong(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_1),obj.getLong(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_2),obj.getLong(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_2),obj.getLong(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_3),obj.getLong(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_3),obj.getLong(buffer,F_OFFSET_1));
end;

procedure TSDOCharField_Test.getShort();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = TSDOChar(#97); VAL_2 = TSDOChar(#48); VAL_3 = TSDOChar(#100);
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOChar;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOChar(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(VAL_1),obj.getShort(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(Ord(VAL_1)),obj.getShort(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(VAL_2),obj.getShort(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(VAL_2),obj.getShort(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(VAL_3),obj.getShort(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(Ord(VAL_3),obj.getShort(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOChar(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_1),obj.getShort(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_1),obj.getShort(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_2),obj.getShort(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_2),obj.getShort(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_3),obj.getShort(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(Ord(VAL_3),obj.getShort(buffer,F_OFFSET_1));
end;

procedure TSDOCharField_Test.getString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = TSDOChar('a'); VAL_2 = TSDOChar('b'); VAL_3 = TSDOChar('c');
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOChar;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOChar(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getString(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getString(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getString(buffer,F_OFFSET_0));

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOChar(tmpBuffer);
    valBuffer^ := VAL_1;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getString(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getString(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getString(buffer,F_OFFSET_1));
end;

procedure TSDOCharField_Test.setBoolean();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOChar;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOChar(tmpBuffer);
  obj := Create_Field();

  obj.setBoolean(buffer,F_OFFSET_0,True);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals('1',valBuffer^);

  obj.setBoolean(buffer,F_OFFSET_0,False);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals('0',valBuffer^);

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOChar(tmpBuffer);

    obj.setBoolean(buffer,F_OFFSET_1,True);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals('1',valBuffer^);

    obj.setBoolean(buffer,F_OFFSET_1,False);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals('0',valBuffer^);
end;

procedure TSDOCharField_Test.setByte();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = TSDOChar(#98); VAL_2 = TSDOChar(#56); VAL_3 = TSDOChar(#110);
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOChar;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOChar(tmpBuffer);
  obj := Create_Field();

  obj.setByte(buffer,F_OFFSET_0,Ord(VAL_1));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setByte(buffer,F_OFFSET_0,Ord(VAL_2));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setByte(buffer,F_OFFSET_0,Ord(VAL_3));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOChar(tmpBuffer);

    obj.setByte(buffer,F_OFFSET_1,Ord(VAL_1));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setByte(buffer,F_OFFSET_1,Ord(VAL_2));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setByte(buffer,F_OFFSET_1,Ord(VAL_3));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOCharField_Test.setCharacter();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = TSDOChar('A'); VAL_2 = TSDOChar('b'); VAL_3 = TSDOChar('1');
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOChar;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOChar(tmpBuffer);
  obj := Create_Field();

  obj.setCharacter(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setCharacter(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setCharacter(buffer,F_OFFSET_0,VAL_3);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOChar(tmpBuffer);

    obj.setCharacter(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setCharacter(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setCharacter(buffer,F_OFFSET_1,VAL_3);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOCharField_Test.setInteger();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = TSDOChar(#98); VAL_2 = TSDOChar(#56); VAL_3 = TSDOChar(#110);
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOChar;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOChar(tmpBuffer);
  obj := Create_Field();

  obj.setInteger(buffer,F_OFFSET_0,Ord(VAL_1));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setInteger(buffer,F_OFFSET_0,Ord(VAL_2));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setInteger(buffer,F_OFFSET_0,Ord(VAL_3));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOChar(tmpBuffer);

    obj.setInteger(buffer,F_OFFSET_1,Ord(VAL_1));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setInteger(buffer,F_OFFSET_1,Ord(VAL_2));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setInteger(buffer,F_OFFSET_1,Ord(VAL_3));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOCharField_Test.setLong();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = TSDOChar(#98); VAL_2 = TSDOChar(#56); VAL_3 = TSDOChar(#110);
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOChar;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOChar(tmpBuffer);
  obj := Create_Field();

  obj.setLong(buffer,F_OFFSET_0,Ord(VAL_1));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setLong(buffer,F_OFFSET_0,Ord(VAL_2));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setLong(buffer,F_OFFSET_0,Ord(VAL_3));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOChar(tmpBuffer);

    obj.setLong(buffer,F_OFFSET_1,Ord(VAL_1));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setLong(buffer,F_OFFSET_1,Ord(VAL_2));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setLong(buffer,F_OFFSET_1,Ord(VAL_3));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOCharField_Test.setShort();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = TSDOChar(#98); VAL_2 = TSDOChar(#56); VAL_3 = TSDOChar(#110);
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOChar;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOChar(tmpBuffer);
  obj := Create_Field();

  obj.setShort(buffer,F_OFFSET_0,Ord(VAL_1));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setShort(buffer,F_OFFSET_0,Ord(VAL_2));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setShort(buffer,F_OFFSET_0,Ord(VAL_3));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOChar(tmpBuffer);

    obj.setShort(buffer,F_OFFSET_1,Ord(VAL_1));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setShort(buffer,F_OFFSET_1,Ord(VAL_2));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setShort(buffer,F_OFFSET_1,Ord(VAL_3));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOCharField_Test.setString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = TSDOChar('a'); VAL_2 = TSDOChar('b'); VAL_3 = TSDOChar('c');
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOChar;
begin
  intVal := 0;
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOChar(tmpBuffer);
  obj := Create_Field();

  obj.setString(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setString(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  intVal := 0;
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOChar(tmpBuffer);

    obj.setString(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setString(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_LONG}
{ TSDOLongField_Test }

function TSDOLongField_Test.Create_Field() : ISDOField;
begin
  Result := TSDOLongField.Create() as ISDOField;
end;

procedure TSDOLongField_Test.getBoolean();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 12345678912345; VAL_2 = -12345678912345; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOLong;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOLong(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_0));

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOLong(tmpBuffer);
    valBuffer^ := VAL_1;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));

    valBuffer^ := VAL_2;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_1));
end;

procedure TSDOLongField_Test.getByte();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 12; VAL_2 = 123; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOLong;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOLong(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getByte(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getByte(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getByte(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getByte(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getByte(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getByte(buffer,F_OFFSET_0));

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOLong(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getByte(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getByte(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getByte(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getByte(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getByte(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getByte(buffer,F_OFFSET_1));
end;

procedure TSDOLongField_Test.getCharacter();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = Ord(TSDOChar('I')); VAL_2 = Ord(TSDOChar('W')); VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOLong;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOLong(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_1),obj.getCharacter(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_1),obj.getCharacter(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_2),obj.getCharacter(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_2),obj.getCharacter(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_3),obj.getCharacter(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_3),obj.getCharacter(buffer,F_OFFSET_0));

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOLong(tmpBuffer);
    valBuffer^ := VAL_1;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_1),obj.getCharacter(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_1),obj.getCharacter(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_2),obj.getCharacter(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_2),obj.getCharacter(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_3),obj.getCharacter(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_3),obj.getCharacter(buffer,F_OFFSET_1));
end;

procedure TSDOLongField_Test.getInteger();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 12345678; VAL_2 = -78945612; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOLong;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOLong(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getInteger(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getInteger(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getInteger(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getInteger(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getInteger(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getInteger(buffer,F_OFFSET_0));

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOLong(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getInteger(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getInteger(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getInteger(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getInteger(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getInteger(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getInteger(buffer,F_OFFSET_1));
end;

procedure TSDOLongField_Test.getLong();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 1234586791234567; VAL_2 = -98765532166547; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOLong;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOLong(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getLong(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getLong(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getLong(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getLong(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getLong(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getLong(buffer,F_OFFSET_0));

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOLong(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getLong(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getLong(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getLong(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getLong(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getLong(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getLong(buffer,F_OFFSET_1));
end;

procedure TSDOLongField_Test.getShort();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 1234; VAL_2 = -987; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOLong;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOLong(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getShort(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getShort(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getShort(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getShort(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getShort(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getShort(buffer,F_OFFSET_0));

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOLong(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getShort(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getShort(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getShort(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getShort(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getShort(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getShort(buffer,F_OFFSET_1));
end;

procedure TSDOLongField_Test.getString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 123456789123456; VAL_2 = -9876543211234; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOLong;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOLong(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_1),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_1),obj.getString(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_2),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_2),obj.getString(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_3),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_3),obj.getString(buffer,F_OFFSET_0));

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOLong(tmpBuffer);
    valBuffer^ := VAL_1;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_1),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_1),obj.getString(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_2),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_2),obj.getString(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_3),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_3),obj.getString(buffer,F_OFFSET_1));
end;

procedure TSDOLongField_Test.setBoolean();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOLong;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOLong(tmpBuffer);
  obj := Create_Field();

  obj.setBoolean(buffer,F_OFFSET_0,True);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(1,valBuffer^);

  obj.setBoolean(buffer,F_OFFSET_0,False);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(0,valBuffer^);

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOLong(tmpBuffer);

    obj.setBoolean(buffer,F_OFFSET_1,True);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(1,valBuffer^);

    obj.setBoolean(buffer,F_OFFSET_1,False);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(0,valBuffer^);
end;

procedure TSDOLongField_Test.setByte();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 12; VAL_2 = 123; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOLong;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOLong(tmpBuffer);
  obj := Create_Field();

  obj.setByte(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setByte(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setByte(buffer,F_OFFSET_0,VAL_3);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOLong(tmpBuffer);

    obj.setByte(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setByte(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setByte(buffer,F_OFFSET_1,VAL_3);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOLongField_Test.setCharacter();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = Ord(TSDOChar('I')); VAL_2 = Ord(TSDOChar('W')); VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOLong;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOLong(tmpBuffer);
  obj := Create_Field();

  obj.setCharacter(buffer,F_OFFSET_0,TSDOChar(VAL_1));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setCharacter(buffer,F_OFFSET_0,TSDOChar(VAL_2));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOLong(tmpBuffer);

    obj.setCharacter(buffer,F_OFFSET_1,TSDOChar(VAL_1));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setCharacter(buffer,F_OFFSET_1,TSDOChar(VAL_2));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);
end;

procedure TSDOLongField_Test.setInteger();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 12345678; VAL_2 = -98765432; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOLong;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOLong(tmpBuffer);
  obj := Create_Field();

  obj.setInteger(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setInteger(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setInteger(buffer,F_OFFSET_0,VAL_3);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOLong(tmpBuffer);

    obj.setInteger(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setInteger(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setInteger(buffer,F_OFFSET_1,VAL_3);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOLongField_Test.setLong();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 1234586791234567; VAL_2 = -98765532166547; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOLong;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOLong(tmpBuffer);
  obj := Create_Field();

  obj.setLong(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setLong(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setLong(buffer,F_OFFSET_0,VAL_3);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOLong(tmpBuffer);

    obj.setLong(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setLong(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setLong(buffer,F_OFFSET_1,VAL_3);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOLongField_Test.setShort();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 123; VAL_2 = -9876; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOLong;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOLong(tmpBuffer);
  obj := Create_Field();

  obj.setShort(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setShort(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setShort(buffer,F_OFFSET_0,VAL_3);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOLong(tmpBuffer);

    obj.setShort(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setShort(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setShort(buffer,F_OFFSET_1,VAL_3);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOLongField_Test.setString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 12345678912345; VAL_2 = -987654321654321; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOLong;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOLong(tmpBuffer);
  obj := Create_Field();

  obj.setString(buffer,F_OFFSET_0,IntToStr(VAL_1));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setString(buffer,F_OFFSET_0,IntToStr(VAL_2));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOLong(tmpBuffer);

    obj.setString(buffer,F_OFFSET_1,IntToStr(VAL_1));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setString(buffer,F_OFFSET_1,IntToStr(VAL_2));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
{ TSDOShortField_Test }

function TSDOShortField_Test.Create_Field() : ISDOField;
begin
  Result := TSDOShortField.Create() as ISDOField;
end;

procedure TSDOShortField_Test.getBoolean();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 12345; VAL_2 = -9876; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOShort;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOShort(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_0));

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOShort(tmpBuffer);
    valBuffer^ := VAL_1;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));

    valBuffer^ := VAL_2;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(True,obj.getBoolean(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(False,obj.getBoolean(buffer,F_OFFSET_1));
end;

procedure TSDOShortField_Test.getByte();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 12; VAL_2 = 123; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOShort;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOShort(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getByte(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getByte(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getByte(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getByte(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getByte(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getByte(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getByte(buffer,F_OFFSET_0));

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOShort(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getByte(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getByte(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getByte(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getByte(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getByte(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getByte(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getByte(buffer,F_OFFSET_1));
end;

procedure TSDOShortField_Test.getCharacter();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = Ord(TSDOChar('I')); VAL_2 = Ord(TSDOChar('W')); VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOShort;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOShort(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_1),obj.getCharacter(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_1),obj.getCharacter(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_2),obj.getCharacter(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_2),obj.getCharacter(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_3),obj.getCharacter(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOChar(VAL_3),obj.getCharacter(buffer,F_OFFSET_0));

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOShort(tmpBuffer);
    valBuffer^ := VAL_1;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_1),obj.getCharacter(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_1),obj.getCharacter(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_2),obj.getCharacter(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_2),obj.getCharacter(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_3),obj.getCharacter(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOChar(VAL_3),obj.getCharacter(buffer,F_OFFSET_1));
end;

procedure TSDOShortField_Test.getInteger();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 1234; VAL_2 = -7894; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOShort;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOShort(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getInteger(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getInteger(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getInteger(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getInteger(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getInteger(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getInteger(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getInteger(buffer,F_OFFSET_0));

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOShort(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getInteger(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getInteger(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getInteger(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getInteger(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getInteger(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getInteger(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getInteger(buffer,F_OFFSET_1));
end;

procedure TSDOShortField_Test.getLong();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 1234; VAL_2 = -987; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOLong;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOLong(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getLong(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getLong(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getLong(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getLong(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getLong(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getLong(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getLong(buffer,F_OFFSET_0));

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOLong(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getLong(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getLong(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getLong(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getLong(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getLong(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getLong(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getLong(buffer,F_OFFSET_1));
end;

procedure TSDOShortField_Test.getShort();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 1234; VAL_2 = -9876; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOShort;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOShort(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getShort(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getShort(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getShort(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getShort(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getShort(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getShort(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getShort(buffer,F_OFFSET_0));

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOShort(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getShort(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getShort(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getShort(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getShort(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getShort(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getShort(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getShort(buffer,F_OFFSET_1));
end;

procedure TSDOShortField_Test.getString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 1234; VAL_2 = -9876; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOShort;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOShort(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_1),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_1),obj.getString(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_2),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_2),obj.getString(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_3),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(IntToStr(VAL_3),obj.getString(buffer,F_OFFSET_0));

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOShort(tmpBuffer);
    valBuffer^ := VAL_1;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_1),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_1),obj.getString(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_2),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_2),obj.getString(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_3),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(IntToStr(VAL_3),obj.getString(buffer,F_OFFSET_1));
end;

procedure TSDOShortField_Test.setBoolean();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOShort;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOShort(tmpBuffer);
  obj := Create_Field();

  obj.setBoolean(buffer,F_OFFSET_0,True);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(1,valBuffer^);

  obj.setBoolean(buffer,F_OFFSET_0,False);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(0,valBuffer^);

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOShort(tmpBuffer);

    obj.setBoolean(buffer,F_OFFSET_1,True);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(1,valBuffer^);

    obj.setBoolean(buffer,F_OFFSET_1,False);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(0,valBuffer^);
end;

procedure TSDOShortField_Test.setByte();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 12; VAL_2 = 123; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOShort;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOShort(tmpBuffer);
  obj := Create_Field();

  obj.setByte(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setByte(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setByte(buffer,F_OFFSET_0,VAL_3);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOShort(tmpBuffer);

    obj.setByte(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setByte(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setByte(buffer,F_OFFSET_1,VAL_3);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOShortField_Test.setCharacter();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = Ord(TSDOChar('I')); VAL_2 = Ord(TSDOChar('W')); VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOShort;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOShort(tmpBuffer);
  obj := Create_Field();

  obj.setCharacter(buffer,F_OFFSET_0,TSDOChar(VAL_1));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setCharacter(buffer,F_OFFSET_0,TSDOChar(VAL_2));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOShort(tmpBuffer);

    obj.setCharacter(buffer,F_OFFSET_1,TSDOChar(VAL_1));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setCharacter(buffer,F_OFFSET_1,TSDOChar(VAL_2));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);
end;

procedure TSDOShortField_Test.setInteger();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 1234; VAL_2 = -9876; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOShort;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOShort(tmpBuffer);
  obj := Create_Field();

  obj.setInteger(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setInteger(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setInteger(buffer,F_OFFSET_0,VAL_3);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOShort(tmpBuffer);

    obj.setInteger(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setInteger(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setInteger(buffer,F_OFFSET_1,VAL_3);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOShortField_Test.setLong();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 1234; VAL_2 = -9876; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOShort;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOShort(tmpBuffer);
  obj := Create_Field();

  obj.setLong(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setLong(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setLong(buffer,F_OFFSET_0,VAL_3);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOShort(tmpBuffer);

    obj.setLong(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setLong(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setLong(buffer,F_OFFSET_1,VAL_3);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOShortField_Test.setShort();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 1234; VAL_2 = -9876; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : QWord;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOShort;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOShort(tmpBuffer);
  obj := Create_Field();

  obj.setShort(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setShort(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setShort(buffer,F_OFFSET_0,VAL_3);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOShort(tmpBuffer);

    obj.setShort(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setShort(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setShort(buffer,F_OFFSET_1,VAL_3);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOShortField_Test.setString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 1234; VAL_2 = -9876; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOShort;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOShort(tmpBuffer);
  obj := Create_Field();

  obj.setString(buffer,F_OFFSET_0,IntToStr(VAL_1));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setString(buffer,F_OFFSET_0,IntToStr(VAL_2));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOShort(tmpBuffer);

    obj.setString(buffer,F_OFFSET_1,IntToStr(VAL_1));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setString(buffer,F_OFFSET_1,IntToStr(VAL_2));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);
end;
{$ENDIF HAS_SDO_SHORT}

{$IFDEF HAS_SDO_BYTES}
{ TSDOBytesField_Test }

class procedure TSDOBytesField_Test.CleanUpBuffer(var ABuffer: PPSDOBytes);
begin
  if ( ABuffer <> nil ) then begin
    ABuffer^^ := nil;
    FreeMem(ABuffer^,SizeOf(PSDOBytes));
    ABuffer^ := nil;
  end;
end;

function TSDOBytesField_Test.Create_Field() : ISDOField;
begin
  Result := TSDOBytesField.Create() as ISDOField;
end;

procedure TSDOBytesField_Test.getBytes();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1; ZERO_BYTES : TSDOBytes = nil;
var
      VAL_1, VAL_2, VAL_3 : TSDOBytes;

  procedure SetConstants();
  var
    v : TSDOBytes;
    k : Integer;
  begin
    SetLength(v,100);
    for k := 0 to High(v) do
      v[k] := k mod High(Byte);
    VAL_1 := v;
    v := nil;

    VAL_2 := nil;    

    SetLength(v,200);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
    VAL_3 := v;     
  end;

var
  obj : ISDOField;
  trueBuffer : array[0..( 3 * SizeOf(TSDOBytes))] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PPSDOBytes;
begin
  SetConstants();

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOBytes(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := @VAL_1; SetBit(attributeBuffer^,BIT_ORDER_BUFFER,True);
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(ZERO_BYTES,obj.getBytes(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getBytes(buffer,F_OFFSET_0));

  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(ZERO_BYTES,obj.getBytes(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getBytes(buffer,F_OFFSET_0));
  valBuffer^ := nil;
  
  valBuffer^ := @VAL_2; SetBit(attributeBuffer^,BIT_ORDER_BUFFER,True);
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(ZERO_BYTES,obj.getBytes(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getBytes(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(ZERO_BYTES,obj.getBytes(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getBytes(buffer,F_OFFSET_0));
  valBuffer^ := nil;
  
  valBuffer^ := @VAL_3; SetBit(attributeBuffer^,BIT_ORDER_BUFFER,True);
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(ZERO_BYTES,obj.getBytes(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getBytes(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(ZERO_BYTES,obj.getBytes(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getBytes(buffer,F_OFFSET_0));
  valBuffer^ := nil;

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PPSDOBytes(tmpBuffer);
    valBuffer^ := @VAL_1; SetBit(attributeBuffer^,BIT_ORDER_BUFFER,True);
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(ZERO_BYTES,obj.getBytes(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getBytes(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(ZERO_BYTES,obj.getBytes(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getBytes(buffer,F_OFFSET_1));
    valBuffer^ := nil;

    valBuffer^ := @VAL_2; SetBit(attributeBuffer^,BIT_ORDER_BUFFER,True);
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(ZERO_BYTES,obj.getBytes(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getBytes(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(ZERO_BYTES,obj.getBytes(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getBytes(buffer,F_OFFSET_1));
    valBuffer^ := nil;

    valBuffer^ := @VAL_3; SetBit(attributeBuffer^,BIT_ORDER_BUFFER,True);
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(ZERO_BYTES,obj.getBytes(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getBytes(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(ZERO_BYTES,obj.getBytes(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getBytes(buffer,F_OFFSET_1));
    valBuffer^ := nil;
end;

procedure TSDOBytesField_Test.getString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1; ZERO_BYTES : TSDOBytes = nil;
var
      VAL_1, VAL_2, VAL_3 : TSDOBytes;

  procedure SetConstants();
  var
    v : TSDOBytes;
    k : Integer;
  begin
    SetLength(v,100);
    for k := 0 to High(v) do
      v[k] := k mod High(Byte);
    VAL_1 := v;
    v := nil;

    VAL_2 := nil;

    SetLength(v,200);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
    VAL_3 := v;     
  end;

var
  obj : ISDOField;
  trueBuffer : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PPSDOBytes;
  s : TSDOString;
begin
  SetConstants();

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOBytes(tmpBuffer);
  obj := Create_Field();

  GetMem(valBuffer^,SizeOf(PSDOBytes)); FillChar(valBuffer^^,SizeOf(TSDOBytes),#0);
  SetBit(attributeBuffer^,BIT_ORDER_BUFFER,True);
  try
    valBuffer^^ := VAL_1;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(BytesToString(VAL_1),obj.getString(buffer,F_OFFSET_0));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(BytesToString(VAL_1),obj.getString(buffer,F_OFFSET_0));
    valBuffer^^ := nil;

    valBuffer^^ := VAL_2;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        s := obj.getString(buffer,F_OFFSET_0);
        CheckEquals(BytesToString(VAL_2),s,'x1');
        //CheckEquals(BytesToString(VAL_2),obj.getString(buffer,F_OFFSET_0),'1');
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(BytesToString(VAL_2),obj.getString(buffer,F_OFFSET_0));
    valBuffer^^ := nil;
  
    valBuffer^^ := VAL_3;
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(BytesToString(VAL_3),obj.getString(buffer,F_OFFSET_0));
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(BytesToString(VAL_3),obj.getString(buffer,F_OFFSET_0));
    valBuffer^^ := nil;
  finally
    FreeMem(valBuffer^,SizeOf(PSDOBytes));
    valBuffer^ := nil;
  end;

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PPSDOBytes(tmpBuffer);
  GetMem(valBuffer^,SizeOf(PSDOBytes)); FillChar(valBuffer^^,SizeOf(TSDOBytes),#0);
  SetBit(attributeBuffer^,BIT_ORDER_BUFFER,True);
  try
    valBuffer^^ := VAL_1;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(BytesToString(VAL_1),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(BytesToString(VAL_1),obj.getString(buffer,F_OFFSET_1));
    valBuffer^^ := nil;

    valBuffer^^ := VAL_2;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        s := obj.getString(buffer,F_OFFSET_0);
        CheckEquals(BytesToString(VAL_2),s);
        //CheckEquals(BytesToString(VAL_2),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(BytesToString(VAL_2),obj.getString(buffer,F_OFFSET_1));
    valBuffer^^ := nil;
    
    valBuffer^^ := VAL_3;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(BytesToString(VAL_3),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(BytesToString(VAL_3),obj.getString(buffer,F_OFFSET_1));
    valBuffer^^ := nil;
  finally
    FreeMem(valBuffer^,SizeOf(PSDOBytes));
  end;
end;

procedure TSDOBytesField_Test.setBytes();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1; ZERO_BYTES : TSDOBytes = nil;
var
      VAL_1, VAL_2, VAL_3 : TSDOBytes;

  procedure SetConstants();
  var
    v : TSDOBytes;
    k : Integer;
  begin
    SetLength(v,100);
    for k := 0 to High(v) do
      v[k] := k mod High(Byte);
    VAL_1 := v;
    v := nil;

    VAL_2 := nil;
    
    SetLength(v,200);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
    VAL_3 := v;     
  end;
  
var
  obj : ISDOField;
  trueBuffer : array[0..100] of Byte;
  buffer, tmpBuffer, startBuffer : PByte;
  valBuffer : PPSDOBytes;
begin
  SetConstants();
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOBytes(tmpBuffer);
  obj := Create_Field();

  obj.setBytes(buffer,F_OFFSET_0,nil);
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
  CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_BUFFER), 'set bit');

  SetBit(startBuffer^,BIT_ORDER_NULL,True);
  obj.setBytes(buffer,F_OFFSET_0,VAL_1);
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
  CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_BUFFER), 'set bit');
  CheckEquals(True, ( valBuffer^ <> nil ), 'bytes buffer' );
  CheckEquals(VAL_1,valBuffer^^);

  obj.setBytes(buffer,F_OFFSET_0,VAL_2);
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
  CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_BUFFER), 'set bit');
  CheckEquals(True, ( valBuffer^ <> nil ), 'bytes buffer' );
  CheckEquals(VAL_2,valBuffer^^);

  SetBit(startBuffer^,BIT_ORDER_NULL,True);
  obj.setBytes(buffer,F_OFFSET_0,VAL_3);
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
  CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
  CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_BUFFER), 'set bit');
  CheckEquals(True, ( valBuffer^ <> nil ), 'bytes buffer' );
  CheckEquals(VAL_3,valBuffer^^);

  // Clean up to avoid false MEM-LEAK
  CleanUpBuffer(valBuffer);

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  startBuffer := tmpBuffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOBytes(tmpBuffer);

    SetBit(startBuffer^,BIT_ORDER_NULL,True);
    obj.setBytes(buffer,F_OFFSET_1,nil);
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
    CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_BUFFER), 'set bit');

    obj.setBytes(buffer,F_OFFSET_1,VAL_1);
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
    CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_BUFFER), 'set bit');
    CheckEquals(True, ( valBuffer^ <> nil ), 'bytes buffer' );
    CheckEquals(VAL_1,valBuffer^^);

    SetBit(startBuffer^,BIT_ORDER_NULL,True);
    obj.setBytes(buffer,F_OFFSET_1,VAL_2);
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
    CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_BUFFER), 'set bit');
    CheckEquals(True, ( valBuffer^ <> nil ), 'bytes buffer' );
    CheckEquals(VAL_2,valBuffer^^);

    obj.setBytes(buffer,F_OFFSET_1,VAL_3);
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_SET), 'set bit');
    CheckEquals(False, IsBitON(startBuffer^,BIT_ORDER_NULL), 'set bit');
    CheckEquals(True, IsBitON(startBuffer^,BIT_ORDER_BUFFER), 'set bit');
    CheckEquals(True, ( valBuffer^ <> nil ), 'bytes buffer' );
    CheckEquals(VAL_3,valBuffer^^);
    // Clean up to avoid false MEM-LEAK
    CleanUpBuffer(valBuffer);

end;

procedure TSDOBytesField_Test.setString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1; ZERO_BYTES : TSDOBytes = nil;
var
      VAL_1, VAL_2, VAL_3 : TSDOBytes;

  procedure SetConstants();
  var
    v : TSDOBytes;
    k : Integer;
  begin
    SetLength(v,100);
    for k := 0 to High(v) do
      v[k] := k mod High(Byte);
    VAL_1 := v;
    v := nil;    

    VAL_2 := nil;

    SetLength(v,200);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
    VAL_3 := v;     
  end;

var
  obj : ISDOField;
  trueBuffer : array[0..( 3 * SizeOf(TSDOBytes))] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PPSDOBytes;
begin
  SetConstants();

  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  buffer := @(trueBuffer[0]);
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PPSDOBytes(tmpBuffer);
  obj := Create_Field();

  obj.setString(buffer,F_OFFSET_0,BytesToString(VAL_1));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^^);

  obj.setString(buffer,F_OFFSET_0,BytesToString(VAL_2));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^^);

  CleanUpBuffer(valBuffer);
  FillChar(trueBuffer,SizeOf(trueBuffer),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PPSDOBytes(tmpBuffer);

    obj.setString(buffer,F_OFFSET_1,BytesToString(VAL_1));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^^);

    obj.setString(buffer,F_OFFSET_1,BytesToString(VAL_2));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^^);
      
  CleanUpBuffer(valBuffer);
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CURRENCY}
function TSDOCurrencyField_Test.Create_Field() : ISDOField;
begin
  Result := TSDOCurrencyField.Create() as ISDOField;
end;

procedure TSDOCurrencyField_Test.getCurrency();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 : TSDOCurrency = 123458679123.1234; VAL_2 : TSDOCurrency = -98765532166547.9876; VAL_3 : TSDOCurrency = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOCurrency;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOCurrency(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getCurrency(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getCurrency(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getCurrency(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getCurrency(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getCurrency(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getCurrency(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getCurrency(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getCurrency(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getCurrency(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getCurrency(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getCurrency(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getCurrency(buffer,F_OFFSET_0));

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOCurrency(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getCurrency(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getCurrency(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getCurrency(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getCurrency(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getCurrency(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getCurrency(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getCurrency(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getCurrency(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getCurrency(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getCurrency(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getCurrency(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getCurrency(buffer,F_OFFSET_1));
end;

procedure TSDOCurrencyField_Test.setCurrency();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 : TSDOCurrency = 12345; VAL_2 : TSDOCurrency = -98765; VAL_3 : TSDOCurrency = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOCurrency;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOCurrency(tmpBuffer);
  obj := Create_Field();

  obj.setDouble(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setDouble(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setDouble(buffer,F_OFFSET_0,VAL_3);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOCurrency(tmpBuffer);

    obj.setDouble(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setDouble(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setDouble(buffer,F_OFFSET_1,VAL_3);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOCurrencyField_Test.getDouble();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 : TSDOCurrency = 12345; VAL_2 : TSDOCurrency = -98765; VAL_3 : TSDOCurrency = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOCurrency;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOCurrency(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getDouble(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getDouble(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getDouble(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getDouble(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getDouble(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getDouble(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getDouble(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getDouble(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getDouble(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getDouble(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getDouble(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getDouble(buffer,F_OFFSET_0));

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOCurrency(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getDouble(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getDouble(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getDouble(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getDouble(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getDouble(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getDouble(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getDouble(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getDouble(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getDouble(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getDouble(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getDouble(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getDouble(buffer,F_OFFSET_1));
end;

procedure TSDOCurrencyField_Test.setDouble();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 : TSDOCurrency = 12345; VAL_2 : TSDOCurrency = -98765; VAL_3 : TSDOCurrency = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte; 
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOCurrency;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOCurrency(tmpBuffer);
  obj := Create_Field();

  obj.setDouble(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setDouble(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setDouble(buffer,F_OFFSET_0,VAL_3);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOCurrency(tmpBuffer);

    obj.setDouble(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setDouble(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setDouble(buffer,F_OFFSET_1,VAL_3);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOCurrencyField_Test.getFloat();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 : TSDOCurrency = 12345; VAL_2 : TSDOCurrency = -98765; VAL_3 : TSDOCurrency = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOCurrency;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOCurrency(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getFloat(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getFloat(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getFloat(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getFloat(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getFloat(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getFloat(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getFloat(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getFloat(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getFloat(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getFloat(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getFloat(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getFloat(buffer,F_OFFSET_0));

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOCurrency(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getFloat(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getFloat(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getFloat(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getFloat(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getFloat(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getFloat(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getFloat(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getFloat(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getFloat(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getFloat(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getFloat(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getFloat(buffer,F_OFFSET_1));
end;

procedure TSDOCurrencyField_Test.setFloat();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 : TSDOCurrency = 12345; VAL_2 : TSDOCurrency = -98765; VAL_3 : TSDOCurrency = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte; 
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOCurrency;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOCurrency(tmpBuffer);
  obj := Create_Field();

  obj.setFloat(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setFloat(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setFloat(buffer,F_OFFSET_0,VAL_3);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOCurrency(tmpBuffer);

    obj.setFloat(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setFloat(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setFloat(buffer,F_OFFSET_1,VAL_3);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDOCurrencyField_Test.getString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 = 12345.6789; VAL_2 = -987654.3211; VAL_3 = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOCurrency;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOCurrency(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOConvertHelper.CurrencyToString(VAL_1),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOConvertHelper.CurrencyToString(VAL_1),obj.getString(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOConvertHelper.CurrencyToString(VAL_2),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOConvertHelper.CurrencyToString(VAL_2),obj.getString(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOConvertHelper.CurrencyToString(VAL_3),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOConvertHelper.CurrencyToString(VAL_3),obj.getString(buffer,F_OFFSET_0));

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOCurrency(tmpBuffer);
    valBuffer^ := VAL_1;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOConvertHelper.CurrencyToString(VAL_1),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOConvertHelper.CurrencyToString(VAL_1),obj.getString(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOConvertHelper.CurrencyToString(VAL_2),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOConvertHelper.CurrencyToString(VAL_2),obj.getString(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOConvertHelper.CurrencyToString(VAL_3),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOConvertHelper.CurrencyToString(VAL_3),obj.getString(buffer,F_OFFSET_1));
end;

procedure TSDOCurrencyField_Test.setString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 : TSDOCurrency = 12345; VAL_2 : TSDOCurrency = -98765; VAL_3 : TSDOCurrency = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOCurrency;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOCurrency(tmpBuffer);
  obj := Create_Field();

  obj.setString(buffer,F_OFFSET_0,TSDOConvertHelper.CurrencyToString(VAL_1));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setString(buffer,F_OFFSET_0,TSDOConvertHelper.CurrencyToString(VAL_2));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOCurrency(tmpBuffer);

    obj.setString(buffer,F_OFFSET_1,TSDOConvertHelper.CurrencyToString(VAL_1));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setString(buffer,F_OFFSET_1,TSDOConvertHelper.CurrencyToString(VAL_2));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);
end;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
function TSDODoubleField_Test.Create_Field() : ISDOField;
begin
  Result := TSDODoubleField.Create() as ISDOField;
end;

procedure TSDODoubleField_Test.getCurrency();
begin

end;

procedure TSDODoubleField_Test.setCurrency();
begin

end;

procedure TSDODoubleField_Test.getDouble();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 : TSDODouble = 123458679123.1234; VAL_2 : TSDODouble = -98765532166547.9876; VAL_3 : TSDODouble = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDODouble;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDODouble(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getDouble(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getDouble(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getDouble(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_1,obj.getDouble(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getDouble(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getDouble(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getDouble(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_2,obj.getDouble(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
  SetBit(attributeBuffer^,BIT_ORDER_SET,False);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getDouble(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getDouble(buffer,F_OFFSET_0));
  SetBit(attributeBuffer^,BIT_ORDER_SET,True);
    SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
      CheckEquals(0,obj.getDouble(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(VAL_3,obj.getDouble(buffer,F_OFFSET_0));

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDODouble(tmpBuffer);
    valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getDouble(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getDouble(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getDouble(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_1,obj.getDouble(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getDouble(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getDouble(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getDouble(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_2,obj.getDouble(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getDouble(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getDouble(buffer,F_OFFSET_1));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,True);
        CheckEquals(0,obj.getDouble(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(VAL_3,obj.getDouble(buffer,F_OFFSET_1));
end;

procedure TSDODoubleField_Test.setDouble();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 : TSDODouble = 12345; VAL_2 : TSDODouble = -98765; VAL_3 : TSDODouble = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDODouble;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDODouble(tmpBuffer);
  obj := Create_Field();

  obj.setDouble(buffer,F_OFFSET_0,VAL_1);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setDouble(buffer,F_OFFSET_0,VAL_2);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  obj.setDouble(buffer,F_OFFSET_0,VAL_3);
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_3,valBuffer^);

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDODouble(tmpBuffer);

    obj.setDouble(buffer,F_OFFSET_1,VAL_1);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setDouble(buffer,F_OFFSET_1,VAL_2);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);

    obj.setDouble(buffer,F_OFFSET_1,VAL_3);
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_3,valBuffer^);
end;

procedure TSDODoubleField_Test.getFloat();
begin

end;

procedure TSDODoubleField_Test.setFloat();
begin

end;

procedure TSDODoubleField_Test.getString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 : TSDODouble = 12345; VAL_2 : TSDODouble = -987654; VAL_3 : TSDODouble = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDODouble;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDODouble(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOConvertHelper.FloatToString(VAL_1),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOConvertHelper.FloatToString(VAL_1),obj.getString(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOConvertHelper.FloatToString(VAL_2),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOConvertHelper.FloatToString(VAL_2),obj.getString(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOConvertHelper.FloatToString(VAL_3),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOConvertHelper.FloatToString(VAL_3),obj.getString(buffer,F_OFFSET_0));

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDODouble(tmpBuffer);
    valBuffer^ := VAL_1;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOConvertHelper.FloatToString(VAL_1),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOConvertHelper.FloatToString(VAL_1),obj.getString(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOConvertHelper.FloatToString(VAL_2),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOConvertHelper.FloatToString(VAL_2),obj.getString(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOConvertHelper.FloatToString(VAL_3),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOConvertHelper.FloatToString(VAL_3),obj.getString(buffer,F_OFFSET_1));
end;

procedure TSDODoubleField_Test.setString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 : TSDODouble = 12345; VAL_2 : TSDODouble = -98765; VAL_3 : TSDODouble = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDODouble;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDODouble(tmpBuffer);
  obj := Create_Field();

  obj.setString(buffer,F_OFFSET_0,TSDOConvertHelper.FloatToString(VAL_1));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setString(buffer,F_OFFSET_0,TSDOConvertHelper.FloatToString(VAL_2));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDODouble(tmpBuffer);

    obj.setString(buffer,F_OFFSET_1,TSDOConvertHelper.FloatToString(VAL_1));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setString(buffer,F_OFFSET_1,TSDOConvertHelper.FloatToString(VAL_2));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
function TSDOFloatField_Test.Create_Field() : ISDOField;
begin
  Result := TSDOFloatField.Create() as ISDOField;
end;

procedure TSDOFloatField_Test.getCurrency();
begin

end;

procedure TSDOFloatField_Test.setCurrency();
begin

end;

procedure TSDOFloatField_Test.getDouble();
begin

end;

procedure TSDOFloatField_Test.setDouble();
begin

end;

procedure TSDOFloatField_Test.getFloat();
begin

end;

procedure TSDOFloatField_Test.setFloat();
begin

end;

procedure TSDOFloatField_Test.getString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 : TSDOFloat = 12345; VAL_2 : TSDOFloat = -987654; VAL_3 : TSDOFloat = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOFloat;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOFloat(tmpBuffer);
  obj := Create_Field();

  valBuffer^ := VAL_1;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOConvertHelper.FloatToString(VAL_1),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOConvertHelper.FloatToString(VAL_1),obj.getString(buffer,F_OFFSET_0));

  valBuffer^ := VAL_2;
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOConvertHelper.FloatToString(VAL_2),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOConvertHelper.FloatToString(VAL_2),obj.getString(buffer,F_OFFSET_0));

  valBuffer^ := VAL_3;
    SetBit(attributeBuffer^,BIT_ORDER_SET,False);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOConvertHelper.FloatToString(VAL_3),obj.getString(buffer,F_OFFSET_0));
    SetBit(attributeBuffer^,BIT_ORDER_SET,True);
      SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
      CheckEquals(TSDOConvertHelper.FloatToString(VAL_3),obj.getString(buffer,F_OFFSET_0));

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOFloat(tmpBuffer);
    valBuffer^ := VAL_1;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOConvertHelper.FloatToString(VAL_1),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOConvertHelper.FloatToString(VAL_1),obj.getString(buffer,F_OFFSET_1));


    valBuffer^ := VAL_2;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOConvertHelper.FloatToString(VAL_2),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOConvertHelper.FloatToString(VAL_2),obj.getString(buffer,F_OFFSET_1));

    valBuffer^ := VAL_3;
      SetBit(attributeBuffer^,BIT_ORDER_SET,True);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOConvertHelper.FloatToString(VAL_3),obj.getString(buffer,F_OFFSET_1));
      SetBit(attributeBuffer^,BIT_ORDER_SET,False);
        SetBit(attributeBuffer^,BIT_ORDER_NULL,False);
        CheckEquals(TSDOConvertHelper.FloatToString(VAL_3),obj.getString(buffer,F_OFFSET_1));
end;

procedure TSDOFloatField_Test.setString();
const F_OFFSET_0 = 0; F_OFFSET_1 = 1;
      VAL_1 : TSDOFloat = 12345; VAL_2 : TSDOFloat = -98765; VAL_3 : TSDOFloat = 0;
var
  obj : ISDOField;
  intVal : array[0..100] of Byte;
  buffer, tmpBuffer, attributeBuffer : PByte;
  valBuffer : PSDOFloat;
begin
  FillChar(intVal,SizeOf(intVal),#0);
  buffer := @intVal;
  tmpBuffer := buffer;
  attributeBuffer := buffer;
  Inc(tmpBuffer);
  valBuffer := PSDOFloat(tmpBuffer);
  obj := Create_Field();

  obj.setString(buffer,F_OFFSET_0,TSDOConvertHelper.FloatToString(VAL_1));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_1,valBuffer^);

  obj.setString(buffer,F_OFFSET_0,TSDOConvertHelper.FloatToString(VAL_2));
    CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
    CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
    CheckEquals(VAL_2,valBuffer^);

  FillChar(intVal,SizeOf(intVal),#0);
  Inc(tmpBuffer);
  Inc(attributeBuffer);
  valBuffer := PSDOFloat(tmpBuffer);

    obj.setString(buffer,F_OFFSET_1,TSDOConvertHelper.FloatToString(VAL_1));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_1,valBuffer^);

    obj.setString(buffer,F_OFFSET_1,TSDOConvertHelper.FloatToString(VAL_2));
      CheckEquals(True,IsBitON(attributeBuffer^,BIT_ORDER_SET));
      CheckEquals(False,IsBitON(attributeBuffer^,BIT_ORDER_NULL));
      CheckEquals(VAL_2,valBuffer^);
end;
{$ENDIF HAS_SDO_FLOAT}

initialization
  RegisterTest(TSDOBooleanField_Test.GetTestSuitePath(),TSDOBooleanField_Test.Suite);
  RegisterTest(TSDOIntegerField_Test.GetTestSuitePath(),TSDOIntegerField_Test.Suite);
{$IFDEF HAS_SDO_LONG}
  RegisterTest(TSDOLongField_Test.GetTestSuitePath(),TSDOLongField_Test.Suite);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
  RegisterTest(TSDOShortField_Test.GetTestSuitePath(),TSDOShortField_Test.Suite);
{$ENDIF HAS_SDO_SHORT}
  RegisterTest(TSDOStringField_Test.GetTestSuitePath(),TSDOStringField_Test.Suite);
  RegisterTest(TSDOBaseDataObject_Test.GetTestSuitePath(),TSDOBaseDataObject_Test.Suite);
  RegisterTest(TSDOChangeSummaryField_Test.GetTestSuitePath(),TSDOChangeSummaryField_Test.Suite);
  RegisterTest(TSDOByteField_Test.GetTestSuitePath(),TSDOByteField_Test.Suite);
  RegisterTest(TSDODateField_Test.GetTestSuitePath(),TSDODateField_Test.Suite);
{$IFDEF HAS_SDO_BYTES}
  RegisterTest(TSDOBytesField_Test.GetTestSuitePath(),TSDOBytesField_Test.Suite);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
  RegisterTest(TSDOCharField_Test.GetTestSuitePath(),TSDOCharField_Test.Suite);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
  RegisterTest(TSDOCurrencyField_Test.GetTestSuitePath(),TSDOCurrencyField_Test.Suite);
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
  RegisterTest(TSDODoubleField_Test.GetTestSuitePath(),TSDODoubleField_Test.Suite);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
  RegisterTest(TSDOFloatField_Test.GetTestSuitePath(),TSDOFloatField_Test.Suite);
{$ENDIF HAS_SDO_FLOAT}
end.
