{$INCLUDE sdo_global.inc}
unit test_copyhelper;

interface
uses
  SysUtils, Classes//, Dialogs
{$IFDEF FPC}
  ,fpcunit, testutils, testregistry
{$ENDIF}
{$IFNDEF FPC}
  ,TestFrameWork
{$ENDIF}
  , sdo, sdo_types, test_suite_utils ;

type

  TSDOCopyHelper_Test = class(TWstBaseTest)
  private
    FFactory : ISDODataFactory;
  private
    function CreateFactory() : ISDODataFactory;
    function CreateCopyHelper() : TSDOCopyHelperClass;
    function createObjA(const AUri : string; const AFact : ISDODataFactory) : ISDODataObject;
    function createObjB(const AUri : string; const AFact : ISDODataFactory) : ISDODataObject;
    function createObjC(const AUri : string; const AFact : ISDODataFactory) : ISDODataObject;
  protected
    procedure SetUp(); override;
    procedure TearDown(); override;
  published
    procedure shallow_copy_nil_obj();
    procedure shallow_copy_empty_obj();
    procedure shallow_copy();
    procedure shallow_copy_cursorPos();

    procedure copy_nil_obj();
    procedure copy_empty_obj();
    procedure copy();
    procedure copy_cursorPos();
  end;

implementation
uses sdo_datafactory, sdo_linked_list;

const
  s_uri              = 'urn-test';
  s_type_object_A    = 'objectA';
  s_type_object_B    = 'objectB';
  s_type_object_C    = 'objectC';
  s_type_object_D    = 'objectD';
  s_bool_prop        = 'bool_prop';            s_bool_propList      = 'bool_prop_list';
  s_byte_prop        = 'byte_prop';            s_byte_propList      = 'byte_prop_list';
  s_bytes_prop       = 'bytes_prop';           s_bytes_propList     = 'bytes_prop_list';
  s_char_prop        = 'char_prop';            s_char_propList      = 'char_prop_list';
  s_currency_prop    = 'currency_prop';        s_currency_propList  = 'currency_prop_list';
  s_date_prop        = 'date_prop';            s_date_propList      = 'date_prop_list';
  s_double_prop      = 'double_prop';          s_double_propList    = 'double_prop_list';
  s_float_prop       = 'float_prop';           s_float_propList     = 'float_prop_list';
  s_integer_prop     = 'integer_prop';         s_integer_propList   = 'integer_prop_list';
  s_long_prop        = 'long_prop';            s_long_propList      = 'long_prop_list';
  s_short_prop       = 'short_prop';           s_short_propList     = 'short_prop_list';
  s_string_prop      = 'string_prop';          s_string_propList    = 'string_prop_list';
  s_object_prop      = 'object_prop';          s_object_propList    = 'object_prop_list';
  s_object_ref_prop      = 'object_ref_prop';

  CURRENCY_CONT_ARRAY : array[0..3] of TSDOCurrency = (
    123456789.3258, 0, -1478369.9876, -5869.9831
  );
  
  DOUBLE_CONT_ARRAY : array[0..3] of TSDODouble = (
    123456789.3258942121, 0, -147258369.987654321, -58369.98321
  );

  FLOAT_CONT_ARRAY : array[0..3] of TSDOFloat = (
    -123789.325121, -96321.558, 0, 83629.5851
  );

{ TSDOCopyHelper_Test }

procedure TSDOCopyHelper_Test.copy();
var
  locA, locB, locC, locD : ISDODataObject;
  locA1, locB1, locC1, locD1 : ISDODataObject;
begin
  locA := createObjA(s_uri,FFactory);
  locB := createObjB(s_uri,FFactory);
  locC := createObjC(s_uri,FFactory);
  locD := FFactory.createNew(s_uri,s_type_object_D);

  locA1 := TSDOCopyHelper.copyShallow(locA);
    CheckEquals(True, TSDOEqualityHelper.equalShallow(locA,locA1));

  locB1 := TSDOCopyHelper.copyShallow(locB);
    CheckEquals(True, TSDOEqualityHelper.equalShallow(locB,locB1));

  locC1 := TSDOCopyHelper.copyShallow(locC);
    CheckEquals(True, TSDOEqualityHelper.equalShallow(locC,locC1));

  locD1 := TSDOCopyHelper.copyShallow(locD);
    CheckEquals(True, TSDOEqualityHelper.equalShallow(locD,locD1));
end;

procedure TSDOCopyHelper_Test.copy_empty_obj();
var
  locA, locB, locC, locD : ISDODataObject;
  locA1, locB1, locC1, locD1 : ISDODataObject;
begin
  locA := FFactory.createNew(s_uri,s_type_object_A);
  locB := FFactory.createNew(s_uri,s_type_object_B);
  locC := FFactory.createNew(s_uri,s_type_object_C);
  locD := FFactory.createNew(s_uri,s_type_object_D);

  locA1 := TSDOCopyHelper.copyShallow(locA);
    CheckEquals(True, TSDOEqualityHelper.equalShallow(locA,locA1));

  locB1 := TSDOCopyHelper.copyShallow(locB);
    CheckEquals(True, TSDOEqualityHelper.equalShallow(locB,locB1));

  locC1 := TSDOCopyHelper.copyShallow(locC);
    CheckEquals(True, TSDOEqualityHelper.equalShallow(locC,locC1));

  locD1 := TSDOCopyHelper.copyShallow(locD);
    CheckEquals(True, TSDOEqualityHelper.equalShallow(locD,locD1));
end;

procedure TSDOCopyHelper_Test.copy_nil_obj();
var
  locX : ISDODataObject;
begin
  locX := CreateCopyHelper().copy(nil);
  CheckEquals(PtrUInt(nil), PtrUInt(locX));
end;

function TSDOCopyHelper_Test.CreateCopyHelper() : TSDOCopyHelperClass;
begin
  Result := TSDOCopyHelper;
end;

function TSDOCopyHelper_Test.CreateFactory() : ISDODataFactory;
var
  locFactory : ISDODataFactory;

  procedure Add_ObjectA(const AUri : string);
  var
    locObj : ISDOType;
  begin
    locFactory.AddType(AUri,s_type_object_A,[]);
    locObj := locFactory.getType(AUri,s_type_object_A);
      locFactory.addProperty(locObj,s_bool_prop,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType], []);
        locFactory.addProperty(locObj,s_bool_propList,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsMany]);
      locFactory.addProperty(locObj,s_byte_prop,sdo_namespace,SDOTypeDefaultTypeNames[ByteType], []);
        locFactory.addProperty(locObj,s_byte_propList,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[pfIsMany]);
{$IFDEF HAS_SDO_BYTES}
      locFactory.addProperty(locObj,s_bytes_prop,sdo_namespace,SDOTypeDefaultTypeNames[BytesType], []);
        locFactory.addProperty(locObj,s_bytes_propList,sdo_namespace,SDOTypeDefaultTypeNames[BytesType],[pfIsMany]);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
      locFactory.addProperty(locObj,s_char_prop,sdo_namespace,SDOTypeDefaultTypeNames[CharacterType], []);
        locFactory.addProperty(locObj,s_char_propList,sdo_namespace,SDOTypeDefaultTypeNames[CharacterType],[pfIsMany]);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
      locFactory.addProperty(locObj,s_currency_prop,sdo_namespace,SDOTypeDefaultTypeNames[CurrencyType], []);
        locFactory.addProperty(locObj,s_currency_propList,sdo_namespace,SDOTypeDefaultTypeNames[CurrencyType],[pfIsMany]);
{$ENDIF HAS_SDO_CURRENCY} 
      locFactory.addProperty(locObj,s_date_prop,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType], []);
        locFactory.addProperty(locObj,s_date_propList,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType],[pfIsMany]);
{$IFDEF HAS_SDO_DOUBLE}
      locFactory.addProperty(locObj,s_double_prop,sdo_namespace,SDOTypeDefaultTypeNames[DoubleType], []);
        locFactory.addProperty(locObj,s_double_propList,sdo_namespace,SDOTypeDefaultTypeNames[DoubleType],[pfIsMany]);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
      locFactory.addProperty(locObj,s_float_prop,sdo_namespace,SDOTypeDefaultTypeNames[FloatType], []);
        locFactory.addProperty(locObj,s_float_propList,sdo_namespace,SDOTypeDefaultTypeNames[FloatType],[pfIsMany]);
{$ENDIF HAS_SDO_FLOAT}
      locFactory.addProperty(locObj,s_integer_prop,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType], []);
        locFactory.addProperty(locObj,s_integer_propList,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsMany]);
{$IFDEF HAS_SDO_LONG}
      locFactory.addProperty(locObj,s_long_prop,sdo_namespace,SDOTypeDefaultTypeNames[LongType], []);
        locFactory.addProperty(locObj,s_long_propList,sdo_namespace,SDOTypeDefaultTypeNames[LongType],[pfIsMany]);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      locFactory.addProperty(locObj,s_short_prop,sdo_namespace,SDOTypeDefaultTypeNames[ShortType], []);
        locFactory.addProperty(locObj,s_short_propList,sdo_namespace,SDOTypeDefaultTypeNames[ShortType],[pfIsMany]);
{$ENDIF HAS_SDO_SHORT}        
      locFactory.addProperty(locObj,s_string_prop,sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
        locFactory.addProperty(locObj,s_string_propList,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsMany]);
  end;

  procedure Add_ObjectB();
  var
    locObj : ISDOType;
  begin
    locFactory.AddType(s_uri,s_type_object_B,[]);
    locObj := locFactory.getType(s_uri,s_type_object_B);
      locFactory.addProperty(locObj,s_bool_prop,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType], []);
        locFactory.addProperty(locObj,s_bool_propList,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsMany]);
      locFactory.addProperty(locObj,s_byte_prop,sdo_namespace,SDOTypeDefaultTypeNames[ByteType], []);
        locFactory.addProperty(locObj,s_byte_propList,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[pfIsMany]);
{$IFDEF HAS_SDO_BYTES}
      locFactory.addProperty(locObj,s_bytes_prop,sdo_namespace,SDOTypeDefaultTypeNames[BytesType], []);
        locFactory.addProperty(locObj,s_bytes_propList,sdo_namespace,SDOTypeDefaultTypeNames[BytesType],[pfIsMany]);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
      locFactory.addProperty(locObj,s_char_prop,sdo_namespace,SDOTypeDefaultTypeNames[CharacterType], []);
        locFactory.addProperty(locObj,s_char_propList,sdo_namespace,SDOTypeDefaultTypeNames[CharacterType],[pfIsMany]);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
      locFactory.addProperty(locObj,s_currency_prop,sdo_namespace,SDOTypeDefaultTypeNames[CurrencyType], []);
        locFactory.addProperty(locObj,s_currency_propList,sdo_namespace,SDOTypeDefaultTypeNames[CurrencyType],[pfIsMany]);
{$ENDIF HAS_SDO_CURRENCY} 
      locFactory.addProperty(locObj,s_date_prop,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType], []);
        locFactory.addProperty(locObj,s_date_propList,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType],[pfIsMany]);
{$IFDEF HAS_SDO_DOUBLE}
      locFactory.addProperty(locObj,s_double_prop,sdo_namespace,SDOTypeDefaultTypeNames[DoubleType], []);
        locFactory.addProperty(locObj,s_double_propList,sdo_namespace,SDOTypeDefaultTypeNames[DoubleType],[pfIsMany]);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
      locFactory.addProperty(locObj,s_float_prop,sdo_namespace,SDOTypeDefaultTypeNames[FloatType], []);
        locFactory.addProperty(locObj,s_float_propList,sdo_namespace,SDOTypeDefaultTypeNames[FloatType],[pfIsMany]);
{$ENDIF HAS_SDO_FLOAT}
      locFactory.addProperty(locObj,s_integer_prop,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType], []);
{$IFDEF HAS_SDO_LONG}
      locFactory.addProperty(locObj,s_long_prop,sdo_namespace,SDOTypeDefaultTypeNames[LongType], []);
        locFactory.addProperty(locObj,s_long_propList,sdo_namespace,SDOTypeDefaultTypeNames[LongType],[pfIsMany]);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      locFactory.addProperty(locObj,s_short_prop,sdo_namespace,SDOTypeDefaultTypeNames[ShortType], []);
        locFactory.addProperty(locObj,s_short_propList,sdo_namespace,SDOTypeDefaultTypeNames[ShortType],[pfIsMany]);
{$ENDIF HAS_SDO_SHORT}
        locFactory.addProperty(locObj,s_integer_propList,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsMany]);
      locFactory.addProperty(locObj,s_string_prop,sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
        locFactory.addProperty(locObj,s_string_propList,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsMany]);

      locFactory.addProperty(locObj,s_object_prop,s_uri,s_type_object_A,[pfIsContainment]);
      locFactory.addProperty(locObj,s_object_ref_prop,s_uri,s_type_object_A,[]);
      locFactory.addProperty(locObj,s_object_propList,s_uri,s_type_object_A,[pfIsMany,pfIsContainment]);
  end;

  procedure Add_ObjectC();
  var
    locObj : ISDOType;
  begin
    locFactory.AddType(s_uri,s_type_object_C,[]);
    locObj := locFactory.getType(s_uri,s_type_object_C);
      locFactory.addProperty(locObj,s_bool_prop,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType], []);
        locFactory.addProperty(locObj,s_bool_propList,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsMany]);
      locFactory.addProperty(locObj,s_byte_prop,sdo_namespace,SDOTypeDefaultTypeNames[ByteType], []);
        locFactory.addProperty(locObj,s_byte_propList,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[pfIsMany]);
{$IFDEF HAS_SDO_BYTES}
      locFactory.addProperty(locObj,s_bytes_prop,sdo_namespace,SDOTypeDefaultTypeNames[BytesType], []);
        locFactory.addProperty(locObj,s_bytes_propList,sdo_namespace,SDOTypeDefaultTypeNames[BytesType],[pfIsMany]);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
      locFactory.addProperty(locObj,s_char_prop,sdo_namespace,SDOTypeDefaultTypeNames[CharacterType], []);
        locFactory.addProperty(locObj,s_char_propList,sdo_namespace,SDOTypeDefaultTypeNames[CharacterType],[pfIsMany]);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
      locFactory.addProperty(locObj,s_currency_prop,sdo_namespace,SDOTypeDefaultTypeNames[CurrencyType], []);
        locFactory.addProperty(locObj,s_currency_propList,sdo_namespace,SDOTypeDefaultTypeNames[CurrencyType],[pfIsMany]);
{$ENDIF HAS_SDO_CURRENCY}
      locFactory.addProperty(locObj,s_date_prop,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType], []);
        locFactory.addProperty(locObj,s_date_propList,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType],[pfIsMany]);
{$IFDEF HAS_SDO_DOUBLE}
      locFactory.addProperty(locObj,s_double_prop,sdo_namespace,SDOTypeDefaultTypeNames[DoubleType], []);
        locFactory.addProperty(locObj,s_double_propList,sdo_namespace,SDOTypeDefaultTypeNames[DoubleType],[pfIsMany]);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
      locFactory.addProperty(locObj,s_float_prop,sdo_namespace,SDOTypeDefaultTypeNames[FloatType], []);
        locFactory.addProperty(locObj,s_float_propList,sdo_namespace,SDOTypeDefaultTypeNames[FloatType],[pfIsMany]);
{$ENDIF HAS_SDO_FLOAT}        
      locFactory.addProperty(locObj,s_integer_prop,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType], []);
        locFactory.addProperty(locObj,s_integer_propList,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsMany]);
{$IFDEF HAS_SDO_LONG}
      locFactory.addProperty(locObj,s_long_prop,sdo_namespace,SDOTypeDefaultTypeNames[LongType], []);
        locFactory.addProperty(locObj,s_long_propList,sdo_namespace,SDOTypeDefaultTypeNames[LongType],[pfIsMany]);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      locFactory.addProperty(locObj,s_short_prop,sdo_namespace,SDOTypeDefaultTypeNames[ShortType], []);
        locFactory.addProperty(locObj,s_short_propList,sdo_namespace,SDOTypeDefaultTypeNames[ShortType],[pfIsMany]);
{$ENDIF HAS_SDO_SHORT}        
      locFactory.addProperty(locObj,s_string_prop,sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
        locFactory.addProperty(locObj,s_string_propList,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsMany]);
  end;

  procedure Add_ObjectD();
  begin
    locFactory.AddType(s_uri,s_type_object_D,[]);
  end;

begin
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
  Add_ObjectA(s_uri);
  Add_ObjectB();
  Add_ObjectC();
  Add_ObjectD();
  Result := locFactory;
end;

function TSDOCopyHelper_Test.createObjA(const AUri: string; const AFact: ISDODataFactory): ISDODataObject;
const
  VAL_1 : TSDODate = ( Date : 35000; HourOffset : 3; MinuteOffset : 4; );
  VAL_2 : TSDODate = ( Date : 40000; HourOffset : 0; MinuteOffset : 0; );
  VAL_3 : TSDODate = ( Date : 38000; HourOffset : 5; MinuteOffset : 45; );
  VAL_4 : TSDODate = ( Date : 39000; HourOffset : 6; MinuteOffset : 55; );
var
  VAL_1_BYTES, VAL_2_BYTES, VAL_3_BYTES, VAL_4_BYTES : TSDOBytes;

  procedure SetConstants();
  var
    v : TSDOBytes;
    k : Integer;
  begin
    SetLength(v,10);
    for k := 0 to High(v) do
      v[k] := k mod High(TSDOByte);
    VAL_1_BYTES := v;
    v := nil;    

    VAL_2_BYTES := nil;

    SetLength(v,20);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(TSDOByte);
    VAL_3_BYTES := v;
    v := nil;  

    SetLength(v,30);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(TSDOByte);
    VAL_4_BYTES := v;
    v := nil;
  end;

var
  ls : ISDODataObjectList;
begin
  SetConstants();
  Result := AFact.createNew(AUri,s_type_object_A);
  Result.setBoolean(s_bool_prop,True);
  Result.setByte(s_byte_prop,123);
{$IFDEF HAS_SDO_BYTES}
  Result.setBytes(s_bytes_prop,VAL_1_BYTES);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
  Result.setCharacter(s_char_prop,'x');
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
  Result.setCurrency(s_currency_prop,123.4567);
{$ENDIF HAS_SDO_CURRENCY}
  Result.setDate(s_date_prop,VAL_1);
{$IFDEF HAS_SDO_DOUBLE}
  Result.setDouble(s_double_prop,78.3654);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
  Result.setFloat(s_float_prop,-159.753);
{$ENDIF HAS_SDO_FLOAT}
  Result.setInteger(s_integer_prop,1210);
{$IFDEF HAS_SDO_LONG}
  Result.setLong(s_long_prop,123456789987654321);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
  Result.setShort(s_short_prop,6541);
{$ENDIF HAS_SDO_SHORT}
  Result.setString(s_string_prop,'inoussa wst fpc');

  ls := Result.getList(s_bool_propList);
    ls.append(True);
    ls.append(True);
    ls.append(False);
    ls.append(True);
    ls.append(False);

  ls := Result.getList(s_byte_propList);
    ls.append(TSDOByte(1));
    ls.append(TSDOByte(2));
    ls.append(TSDOByte(3));

{$IFDEF HAS_SDO_BYTES}
  ls := Result.getList(s_bytes_propList);
    ls.appendBytes(VAL_1_BYTES);
    ls.appendBytes(VAL_2_BYTES);
    ls.appendBytes(VAL_3_BYTES);
    ls.appendBytes(VAL_4_BYTES);
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
  ls := Result.getList(s_char_propList);
    ls.append(TSDOChar('a'));
    ls.append(TSDOChar('b'));
    ls.append(TSDOChar('p'));
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
  ls := Result.getList(s_currency_propList);
    ls.appendCurrency(CURRENCY_CONT_ARRAY[0]);
    ls.appendCurrency(CURRENCY_CONT_ARRAY[1]);
    ls.appendCurrency(CURRENCY_CONT_ARRAY[2]);
    ls.appendCurrency(CURRENCY_CONT_ARRAY[3]);
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
  ls := Result.getList(s_double_propList);
    ls.append(DOUBLE_CONT_ARRAY[0]);
    ls.append(DOUBLE_CONT_ARRAY[1]);
    ls.append(DOUBLE_CONT_ARRAY[2]);
    ls.append(DOUBLE_CONT_ARRAY[3]);
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
  ls := Result.getList(s_float_propList);
    ls.append(FLOAT_CONT_ARRAY[0]);
    ls.append(FLOAT_CONT_ARRAY[1]);
    ls.append(FLOAT_CONT_ARRAY[2]);
    ls.append(FLOAT_CONT_ARRAY[3]);
{$ENDIF HAS_SDO_FLOAT}

  ls := Result.getList(s_date_propList);
    ls.append(VAL_1);
    ls.append(VAL_2);
    ls.append(VAL_3);
    ls.append(VAL_4);

  ls := Result.getList(s_integer_propList);
    ls.append(TSDOInteger(12));
    ls.append(TSDOInteger(10));
    ls.append(TSDOInteger(76));
    ls.append(TSDOInteger(-123));
    ls.append(TSDOInteger(0));

{$IFDEF HAS_SDO_LONG}
  ls := Result.getList(s_long_propList);
    ls.append(TSDOLong(748159623849516236));
    ls.append(TSDOLong(0));
    ls.append(TSDOLong(-3692581479137842655));
    ls.append(TSDOLong(112233445566778899));
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
  ls := Result.getList(s_short_propList);
    ls.append(TSDOLong(7481));
    ls.append(TSDOLong(0));
    ls.append(TSDOLong(-3655));
    ls.append(TSDOLong(8899));
{$ENDIF HAS_SDO_ShORT}

  ls := Result.getList(s_string_propList);
    ls.append(TSDOString('object'));
    ls.append(TSDOString(''));
    ls.append(TSDOString('pascal'));
    ls.append(TSDOString('Lazarus'));
    ls.append(TSDOString('Delphi'));
end;

function TSDOCopyHelper_Test.createObjB(const AUri: string; const AFact: ISDODataFactory): ISDODataObject;
const
  VAL_1 : TSDODate = ( Date : 45678; HourOffset : 3; MinuteOffset : 4; );
  VAL_2 : TSDODate = ( Date : 41234; HourOffset : 0; MinuteOffset : 0; );
  VAL_3 : TSDODate = ( Date : 38976; HourOffset : 5; MinuteOffset : 45; );
  VAL_4 : TSDODate = ( Date : 39254; HourOffset : 6; MinuteOffset : 55; );
var
  VAL_1_BYTES, VAL_2_BYTES, VAL_3_BYTES, VAL_4_BYTES : TSDOBytes;

  procedure SetConstants();
  var
    v : TSDOBytes;
    k : Integer;
  begin
    SetLength(v,5);
    for k := 0 to High(v) do
      v[k] := k mod High(TSDOByte);
    VAL_1_BYTES := v;
    v := nil;    

    VAL_2_BYTES := nil;

    SetLength(v,23);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(TSDOByte);
    VAL_3_BYTES := v;
    v := nil;  

    SetLength(v,35);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(TSDOByte);
    VAL_4_BYTES := v;
    v := nil;
  end;

var
  ls : ISDODataObjectList;
begin
  SetConstants();
  Result := AFact.createNew(AUri,s_type_object_B);
  Result.setBoolean(s_bool_prop,True);
  Result.setByte(s_byte_prop,123);
{$IFDEF HAS_SDO_BYTES}
  Result.setBytes(s_bytes_prop,VAL_1_BYTES);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
  Result.setCharacter(s_char_prop,'l');
{$ENDIF HAS_SDO_CHAR}
  Result.setDate(s_date_prop,VAL_2);
{$IFDEF HAS_SDO_DOUBLE}
  Result.setDouble(s_double_prop,8521.0255);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
  Result.setFloat(s_float_prop,-5852.655753);
{$ENDIF HAS_SDO_FLOAT}
  Result.setInteger(s_integer_prop,1210);
{$IFDEF HAS_SDO_LONG}
  Result.setLong(s_long_prop,123456789987654321);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
  Result.setShort(s_short_prop,6541);
{$ENDIF HAS_SDO_SHORT}
  Result.setString(s_string_prop,'inoussa wst fpc');

  ls := Result.getList(s_bool_propList);
    ls.append(True);
    ls.append(True);
    ls.append(False);
    ls.append(True);
    ls.append(False);

  ls := Result.getList(s_byte_propList);
    ls.append(1);
    ls.append(2);
    ls.append(3);

{$IFDEF HAS_SDO_BYTES}
  ls := Result.getList(s_bytes_propList);
    ls.appendBytes(VAL_1_BYTES);
    ls.appendBytes(VAL_2_BYTES);
    ls.appendBytes(VAL_3_BYTES);
    ls.appendBytes(VAL_4_BYTES);
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
  ls := Result.getList(s_char_propList);
    ls.append(TSDOChar('a'));
    ls.append(TSDOChar('b'));
    ls.append(TSDOChar('p'));
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
  ls := Result.getList(s_currency_propList);
    ls.appendCurrency(CURRENCY_CONT_ARRAY[0]);
    ls.appendCurrency(CURRENCY_CONT_ARRAY[1]);
    ls.appendCurrency(CURRENCY_CONT_ARRAY[2]);
    ls.appendCurrency(CURRENCY_CONT_ARRAY[3]);
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
  ls := Result.getList(s_double_propList);
    ls.append(DOUBLE_CONT_ARRAY[0]);
    ls.append(DOUBLE_CONT_ARRAY[1]);
    ls.append(DOUBLE_CONT_ARRAY[2]);
    ls.append(DOUBLE_CONT_ARRAY[3]);
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
  ls := Result.getList(s_float_propList);
    ls.append(FLOAT_CONT_ARRAY[0]);
    ls.append(FLOAT_CONT_ARRAY[1]);
    ls.append(FLOAT_CONT_ARRAY[2]);
    ls.append(FLOAT_CONT_ARRAY[3]);
{$ENDIF HAS_SDO_FLOAT}

  ls := Result.getList(s_date_propList);
    ls.append(VAL_1);
    ls.append(VAL_2);
    ls.append(VAL_3);
    ls.append(VAL_4);
        
  ls := Result.getList(s_integer_propList);
    ls.append(12);
    ls.append(10);
    ls.append(76);
    ls.append(-123);
    ls.append(0);

{$IFDEF HAS_SDO_LONG}
  ls := Result.getList(s_long_propList);
    ls.append(TSDOLong(7481596238495162375));
    ls.append(TSDOLong(0));
    ls.append(TSDOLong(-3692581479137842655));
    ls.append(TSDOLong(112233445566778899));
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
  ls := Result.getList(s_short_propList);
    ls.append(TSDOLong(7481));
    ls.append(TSDOLong(0));
    ls.append(TSDOLong(-3655));
    ls.append(TSDOLong(8899));
{$ENDIF HAS_SDO_ShORT}

  ls := Result.getList(s_string_propList);
    ls.append('object');
    ls.append('');
    ls.append('pascal');
    ls.append('Lazarus');
    ls.append('Delphi');

  Result.setDataObject(s_object_prop,AFact.createNew(s_uri,s_type_object_A));
    Result.getDataObject(s_object_prop).setString(s_string_prop,'B->A->' + s_string_prop);
  Result.setDataObject(s_object_ref_prop,AFact.createNew(s_uri,s_type_object_A));
    Result.getDataObject(s_object_ref_prop).setString(s_string_prop,'ref B->A->' + s_string_prop);
  ls := Result.getList(s_object_propList);
    ls.append(AFact.createNew(AUri,s_type_object_A));
    ls.append(AFact.createNew(AUri,s_type_object_A));
    ls.getCursor().MoveFirst();
      ls.getDataObject().setString(s_string_prop,'B->A[0]->' + s_string_prop);
    ls.getCursor().MoveNext();
      ls.getDataObject().setString(s_string_prop,'B->A[1]->' + s_string_prop);
end;

function TSDOCopyHelper_Test.createObjC(const AUri: string; const AFact: ISDODataFactory): ISDODataObject;
const
  VAL_1 : TSDODate = ( Date : 38678; HourOffset : 3; MinuteOffset : 4; );
  VAL_2 : TSDODate = ( Date : 39234; HourOffset : 0; MinuteOffset : 0; );
  VAL_3 : TSDODate = ( Date : 40976; HourOffset : 5; MinuteOffset : 45; );
  VAL_4 : TSDODate = ( Date : 41254; HourOffset : 6; MinuteOffset : 55; );
var
  ls : ISDODataObjectList;
begin
  Result := AFact.createNew(AUri,s_type_object_A);
  Result.setBoolean(s_bool_prop,True);
  Result.setByte(s_byte_prop,123);
  Result.setDate(s_date_prop,VAL_3);
  Result.setInteger(s_integer_prop,1210);
  Result.setString(s_string_prop,'inoussa wst fpc');

  ls := Result.getList(s_bool_propList);
    ls.append(True);
    ls.append(True);
    ls.append(False);
    ls.append(True);
    ls.append(False);

  ls := Result.getList(s_byte_propList);
    ls.append(1);
    ls.append(2);
    ls.append(3);

  ls := Result.getList(s_date_propList);
    ls.append(VAL_1);
    ls.append(VAL_2);
    ls.append(VAL_3);
    ls.append(VAL_4);    
        
  ls := Result.getList(s_integer_propList);
    ls.append(12);
    ls.append(10);
    ls.append(76);
    ls.append(-123);
    ls.append(0);

  ls := Result.getList(s_string_propList);
    ls.append('object');
    ls.append('');
    ls.append('pascal');
    ls.append('Lazarus');
    ls.append('Delphi');
end;

procedure TSDOCopyHelper_Test.shallow_copy_cursorPos();
var
  x, y : ISDODataObject;
  lsB, lsI : ISDODataObjectList;
  bmB, bmI : TLinkedListBookmark;
begin
  x := createObjA(s_uri, FFactory);
    lsB := x.getList(s_bool_propList);
      lsB.getCursor().MoveFirst();
      lsB.getCursor().MoveNext();
      lsB.getCursor().MoveNext();
    lsI := x.getList(s_integer_propList);
      lsI.getCursor().MoveLast();
      lsI.getCursor().MovePrevious();
    bmB := lsB.getCursor().GetBookmark();
    bmI := lsI.getCursor().GetBookmark();

  y := TSDOCopyHelper.copyShallow(x);
    CheckEquals(bmB,lsB.getCursor().GetBookmark());
    CheckEquals(bmI,lsI.getCursor().GetBookmark());
end;

procedure TSDOCopyHelper_Test.SetUp();
begin
  inherited;
  FFactory := CreateFactory();
end;

procedure TSDOCopyHelper_Test.shallow_copy();
var
  locA, locB, locC, locD : ISDODataObject;
  locA1, locB1, locC1, locD1 : ISDODataObject;
begin
  locA := createObjA(s_uri,FFactory);
  locB := createObjB(s_uri,FFactory);
  locC := createObjC(s_uri,FFactory);
  locD := FFactory.createNew(s_uri,s_type_object_D);

  locA1 := TSDOCopyHelper.copyShallow(locA);
    CheckEquals(True, TSDOEqualityHelper.equalShallow(locA,locA1));

  locB1 := TSDOCopyHelper.copyShallow(locB);
    CheckEquals(True, TSDOEqualityHelper.equalShallow(locB,locB1));

  locC1 := TSDOCopyHelper.copyShallow(locC);
    CheckEquals(True, TSDOEqualityHelper.equalShallow(locC,locC1));

  locD1 := TSDOCopyHelper.copyShallow(locD);
    CheckEquals(True, TSDOEqualityHelper.equalShallow(locD,locD1));
end;

procedure TSDOCopyHelper_Test.shallow_copy_empty_obj();
var
  locA, locB, locC, locD : ISDODataObject;
  locA1, locB1, locC1, locD1 : ISDODataObject;
begin
  locA := FFactory.createNew(s_uri,s_type_object_A);
  locB := FFactory.createNew(s_uri,s_type_object_B);
  locC := FFactory.createNew(s_uri,s_type_object_C);
  locD := FFactory.createNew(s_uri,s_type_object_D);

  locA1 := TSDOCopyHelper.copyShallow(locA);
    CheckEquals(True, TSDOEqualityHelper.equalShallow(locA,locA1));

  locB1 := TSDOCopyHelper.copyShallow(locB);
    CheckEquals(True, TSDOEqualityHelper.equalShallow(locB,locB1));

  locC1 := TSDOCopyHelper.copyShallow(locC);
    CheckEquals(True, TSDOEqualityHelper.equalShallow(locC,locC1));

  locD1 := TSDOCopyHelper.copyShallow(locD);
    CheckEquals(True, TSDOEqualityHelper.equalShallow(locD,locD1));
end;

procedure TSDOCopyHelper_Test.shallow_copy_nil_obj();
var
  locX : ISDODataObject;
begin
  locX := CreateCopyHelper().copy(nil);
  CheckEquals(PtrUInt(nil), PtrUInt(locX));
end;

procedure TSDOCopyHelper_Test.TearDown();
begin
  FFactory := nil;
  inherited;
end;

procedure TSDOCopyHelper_Test.copy_cursorPos();
var
  x, y : ISDODataObject;
  lsB, lsI : ISDODataObjectList;
  bmB, bmI : TLinkedListBookmark;
begin
  x := createObjA(s_uri, FFactory);
    lsB := x.getList(s_bool_propList);
      lsB.getCursor().MoveFirst();
      lsB.getCursor().MoveNext();
      lsB.getCursor().MoveNext();
    lsI := x.getList(s_integer_propList);
      lsI.getCursor().MoveLast();
      lsI.getCursor().MovePrevious();
    bmB := lsB.getCursor().GetBookmark();
    bmI := lsI.getCursor().GetBookmark();

  y := TSDOCopyHelper.copyShallow(x);
    CheckEquals(bmB,lsB.getCursor().GetBookmark());
    CheckEquals(bmI,lsI.getCursor().GetBookmark());
end;

initialization
  RegisterTest('Helpers',TSDOCopyHelper_Test.Suite);

end.
