{$INCLUDE sdo_global.inc}
unit test_equalityhelper;

interface
uses
  SysUtils, Classes//, Dialogs
{$IFDEF FPC}
  ,fpcunit, testutils, testregistry
{$ENDIF}
{$IFNDEF FPC}
  ,TestFrameWork
{$ENDIF}
  , sdo, sdo_types, test_suite_utils;

type

  TSDOEqualityHelper_Test = class(TWstBaseTest)
  private
    function CreateFactory() : ISDODataFactory;
    function CreateEqualityHelper() : TSDOEqualityHelperClass;
    function createObjA(const AUri : string; const AFact : ISDODataFactory) : ISDODataObject;
    function createObjB(const AUri : string; const AFact : ISDODataFactory) : ISDODataObject;
    function createObjC(const AUri : string; const AFact : ISDODataFactory) : ISDODataObject;
  published
    procedure equalShallow_empty_object();
    procedure equalShallow_object();
    procedure equalShallow_object_not_equal_type();
    procedure equalShallow_object_not_equal_value();
    procedure equalShallow_cursorPos();

    procedure equal_empty_object();
    procedure equal_object();
    procedure equal_object_not_equal_type();
    procedure equal_object_not_equal_value();
    procedure equal_cursorPos();
  end;

implementation

uses sdo_datafactory, sdo_linked_list;

const
  s_uri              = 'urn-test'; s_uri_2              = 'urn-test2';
  s_type_object_A    = 'objectA';
  s_type_object_B    = 'objectB';
  s_type_object_C    = 'objectC';
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

{ TSDOEqualityHelper_Test }

function TSDOEqualityHelper_Test.CreateEqualityHelper() : TSDOEqualityHelperClass;
begin
  Result := TSDOEqualityHelper;
end;

function TSDOEqualityHelper_Test.CreateFactory() : ISDODataFactory;
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
        locFactory.addProperty(locObj,s_object_propList,s_uri,s_type_object_A,[pfIsMany,pfIsContainment]);
  end;

  procedure Add_ObjectC();
  var
    locObj : ISDOType;
  begin
    locFactory.AddType(s_uri_2,s_type_object_C,[]);
    locObj := locFactory.getType(s_uri_2,s_type_object_C);
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

begin
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
  Add_ObjectA(s_uri);
  Add_ObjectA(s_uri_2);
  Add_ObjectB();
  Add_ObjectC();
  Result := locFactory;
end;

function TSDOEqualityHelper_Test.createObjA(const AUri: string; const AFact: ISDODataFactory): ISDODataObject;
const
  VAL_1 : TSDODate = ( Date : 35000; HourOffset : 3; MinuteOffset : 4; );
  VAL_2 : TSDODate = ( Date : 40000; HourOffset : 0; MinuteOffset : 0; );
  VAL_3 : TSDODate = ( Date : 38000; HourOffset : 5; MinuteOffset : 45; );
  VAL_4 : TSDODate = ( Date : 39000; HourOffset : 6; MinuteOffset : 55; );
{$IFDEF HAS_SDO_DOUBLE}
  DOUBLE_VALUES_REPEATED_DIGITED : array[0..3] of TSDODouble = (
    123456789.3258942121, 0, -147258369.987654321, -58369.98321
  );
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
  FLOAT_VALUES_REPEATED_DIGITED : array[0..3] of TSDOFloat = (
    -123789.325121, -96321.558, 0, 83629.5851
  );
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_CURRENCY}
  CURRENCY_VALUES_REPEATED_DIGITED : array[0..3] of TSDOCurrency = (
    6789.3258, 0, -258369.9876, -542424242369.9421
  );  
{$ENDIF HAS_SDO_CURRENCY}
var
  VAL_1_BYTES, VAL_2_BYTES, VAL_3_BYTES, VAL_4_BYTES : TSDOBytes;

  procedure SetConstants();
  var
    v : TSDOBytes;
    k : Integer;
  begin
    SetLength(v,10);
    for k := 0 to High(v) do
      v[k] := k mod High(Byte);
    VAL_1_BYTES := v;
    v := nil;    

    VAL_2_BYTES := nil;

    SetLength(v,20);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
    VAL_3_BYTES := v;
    v := nil;  

    SetLength(v,30);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
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
    ls.append(TSDOByte(12));
    ls.append(TSDOByte(23));

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
    ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[0]);
    ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[1]);
    ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[2]);
    ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[3]);
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
  ls := Result.getList(s_double_propList);
    ls.append(DOUBLE_VALUES_REPEATED_DIGITED[0]);
    ls.append(DOUBLE_VALUES_REPEATED_DIGITED[1]);
    ls.append(DOUBLE_VALUES_REPEATED_DIGITED[2]);
    ls.append(DOUBLE_VALUES_REPEATED_DIGITED[3]);
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
  ls := Result.getList(s_float_propList);
    ls.append(FLOAT_VALUES_REPEATED_DIGITED[0]);
    ls.append(FLOAT_VALUES_REPEATED_DIGITED[1]);
    ls.append(FLOAT_VALUES_REPEATED_DIGITED[2]);
    ls.append(FLOAT_VALUES_REPEATED_DIGITED[3]);
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
    ls.append('object');
    ls.append('');
    ls.append('pascal');
    ls.append('Lazarus');
    ls.append('Delphi');
end;

function TSDOEqualityHelper_Test.createObjB(const AUri: string; const AFact: ISDODataFactory): ISDODataObject;
const
  VAL_1 : TSDODate = ( Date : 45678; HourOffset : 3; MinuteOffset : 4; );
  VAL_2 : TSDODate = ( Date : 41234; HourOffset : 0; MinuteOffset : 0; );
  VAL_3 : TSDODate = ( Date : 38976; HourOffset : 5; MinuteOffset : 45; );
  VAL_4 : TSDODate = ( Date : 39254; HourOffset : 6; MinuteOffset : 55; );
{$IFDEF HAS_SDO_DOUBLE}
  DOUBLE_VALUES_REPEATED_DIGITED : array[0..3] of TSDODouble = (
    123456789.3258942121, 0, -147258369.987654321, -58369.98321
  );
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
  FLOAT_VALUES_REPEATED_DIGITED : array[0..3] of TSDOFloat = (
    -123789.325121, -96321.558, 0, 83629.5851
  );
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_CURRENCY}
  CURRENCY_VALUES_REPEATED_DIGITED : array[0..3] of TSDOCurrency = (
    6789.3258, 0, -258369.9876, -542424242369.9421
  );  
{$ENDIF HAS_SDO_CURRENCY}
var
  VAL_1_BYTES, VAL_2_BYTES, VAL_3_BYTES, VAL_4_BYTES : TSDOBytes;

  procedure SetConstants();
  var
    v : TSDOBytes;
    k : Integer;
  begin
    SetLength(v,5);
    for k := 0 to High(v) do
      v[k] := k mod High(Byte);
    VAL_1_BYTES := v;
    v := nil;    

    VAL_2_BYTES := nil;

    SetLength(v,23);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
    VAL_3_BYTES := v;
    v := nil;  

    SetLength(v,35);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
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
{$IFDEF HAS_SDO_CURRENCY}
  Result.setCurrency(s_currency_prop,23.45);
{$ENDIF HAS_SDO_CURRENCY}
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
    ls.append(TSDOByte(1));
    ls.append(TSDOByte(12));
    ls.append(TSDOByte(23));

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
    ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[0]);
    ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[1]);
    ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[2]);
    ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[3]);
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
  ls := Result.getList(s_double_propList);
    ls.append(DOUBLE_VALUES_REPEATED_DIGITED[0]);
    ls.append(DOUBLE_VALUES_REPEATED_DIGITED[1]);
    ls.append(DOUBLE_VALUES_REPEATED_DIGITED[2]);
    ls.append(DOUBLE_VALUES_REPEATED_DIGITED[3]);
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
  ls := Result.getList(s_float_propList);
    ls.append(FLOAT_VALUES_REPEATED_DIGITED[0]);
    ls.append(FLOAT_VALUES_REPEATED_DIGITED[1]);
    ls.append(FLOAT_VALUES_REPEATED_DIGITED[2]);
    ls.append(FLOAT_VALUES_REPEATED_DIGITED[3]);
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
  ls := Result.getList(s_object_propList);
    ls.append(AFact.createNew(AUri,s_type_object_A));
    ls.append(AFact.createNew(AUri,s_type_object_A));
    ls.getCursor().MoveFirst();
      ls.getDataObject().setString(s_string_prop,'B->A[0]->' + s_string_prop);
    ls.getCursor().MoveNext();
      ls.getDataObject().setString(s_string_prop,'B->A[1]->' + s_string_prop);
end;

function TSDOEqualityHelper_Test.createObjC(const AUri: string; const AFact: ISDODataFactory): ISDODataObject;
const
  VAL_1 : TSDODate = ( Date : 38678; HourOffset : 3; MinuteOffset : 4; );
  VAL_2 : TSDODate = ( Date : 39234; HourOffset : 0; MinuteOffset : 0; );
  VAL_3 : TSDODate = ( Date : 40976; HourOffset : 5; MinuteOffset : 45; );
  VAL_4 : TSDODate = ( Date : 41254; HourOffset : 6; MinuteOffset : 55; );
var
  ls : ISDODataObjectList;
begin
  Result := AFact.createNew(AUri,s_type_object_A);
  Result.setByte(s_byte_prop,123);
  Result.setDate(s_date_prop,VAL_3);
  Result.setBoolean(s_bool_prop,True);
  Result.setInteger(s_integer_prop,1210);
  Result.setString(s_string_prop,'inoussa wst fpc');

  ls := Result.getList(s_bool_propList);
    ls.append(True);
    ls.append(True);
    ls.append(False);
    ls.append(True);
    ls.append(False);

  ls := Result.getList(s_byte_propList);
    ls.append(TSDOByte(1));
    ls.append(TSDOByte(12));
    ls.append(TSDOByte(23));

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

procedure TSDOEqualityHelper_Test.equal_empty_object();
var
  fA, fB : ISDODataFactory;
  x, y : ISDODataObject;
  eh : TSDOEqualityHelperClass;
begin
  fA := CreateFactory();
  fB := CreateFactory();
  eh := CreateEqualityHelper();

  x := fA.createNew(s_uri,s_type_object_A);
  y := fA.createNew(s_uri,s_type_object_A);
  CheckEquals(True,eh.equal(x,y));
  CheckEquals(True,eh.equal(y,x));

  x := fA.createNew(s_uri,s_type_object_A);
  y := fB.createNew(s_uri,s_type_object_A);
  CheckEquals(True,eh.equal(x,y));
  CheckEquals(True,eh.equal(y,x));

  x := fA.createNew(s_uri,s_type_object_B);
  y := fA.createNew(s_uri,s_type_object_B);
  CheckEquals(True,eh.equal(x,y));
  CheckEquals(True,eh.equal(y,x));

  x := fA.createNew(s_uri,s_type_object_B);
  y := fB.createNew(s_uri,s_type_object_B);
  CheckEquals(True,eh.equal(x,y));
  CheckEquals(True,eh.equal(y,x));

  x := fA.createNew(s_uri_2,s_type_object_C);
  y := fB.createNew(s_uri_2,s_type_object_C);
  CheckEquals(True,eh.equal(x,y));
  CheckEquals(True,eh.equal(y,x));
end;

procedure TSDOEqualityHelper_Test.equalShallow_empty_object();
var
  fA, fB : ISDODataFactory;
  x, y : ISDODataObject;
  eh : TSDOEqualityHelperClass;
begin
  fA := CreateFactory();
  fB := CreateFactory();
  eh := CreateEqualityHelper();

  x := fA.createNew(s_uri,s_type_object_A);
  y := fA.createNew(s_uri,s_type_object_A);
  CheckEquals(True,eh.equalShallow(x,y));
  CheckEquals(True,eh.equalShallow(y,x));

  x := fA.createNew(s_uri,s_type_object_A);
  y := fB.createNew(s_uri,s_type_object_A);
  CheckEquals(True,eh.equalShallow(x,y));
  CheckEquals(True,eh.equalShallow(y,x));

  x := fA.createNew(s_uri,s_type_object_B);
  y := fA.createNew(s_uri,s_type_object_B);
  CheckEquals(True,eh.equalShallow(x,y));
  CheckEquals(True,eh.equalShallow(y,x));

  x := fA.createNew(s_uri,s_type_object_B);
  y := fB.createNew(s_uri,s_type_object_B);
  CheckEquals(True,eh.equalShallow(x,y));
  CheckEquals(True,eh.equalShallow(y,x));

  x := fA.createNew(s_uri_2,s_type_object_C);
  y := fB.createNew(s_uri_2,s_type_object_C);
  CheckEquals(True,eh.equalShallow(x,y));
  CheckEquals(True,eh.equalShallow(y,x));
end;

procedure TSDOEqualityHelper_Test.equalShallow_object();
var
  fA, fB : ISDODataFactory;
  x, y : ISDODataObject;
  eh : TSDOEqualityHelperClass;
begin
  fA := CreateFactory();
  fB := CreateFactory();
  eh := CreateEqualityHelper();

  x := createObjA(s_uri, fA);
  y := createObjA(s_uri, fA);
  CheckEquals(True,eh.equalShallow(x,y));
  CheckEquals(True,eh.equalShallow(y,x));

  x := createObjA(s_uri, fA);
  y := createObjA(s_uri, fB);
  CheckEquals(True,eh.equalShallow(x,y));
  CheckEquals(True,eh.equalShallow(y,x));
end;

procedure TSDOEqualityHelper_Test.equalShallow_object_not_equal_type();
var
  fA, fB : ISDODataFactory;
  x, y : ISDODataObject;
  eh : TSDOEqualityHelperClass;
begin
  fA := CreateFactory();
  fB := CreateFactory();
  eh := CreateEqualityHelper();

  x := createObjA(s_uri, fA);
  y := createObjA(s_uri_2, fA);
  CheckEquals(False,eh.equalShallow(x,y));
  CheckEquals(False,eh.equalShallow(y,x));

  x := createObjA(s_uri, fA);
  y := createObjC(s_uri_2, fA);
  CheckEquals(False,eh.equalShallow(x,y));
  CheckEquals(False,eh.equalShallow(y,x));


  x := createObjA(s_uri, fA);
  y := createObjA(s_uri_2, fB);
  CheckEquals(False,eh.equalShallow(x,y));
  CheckEquals(False,eh.equalShallow(y,x));

  x := createObjA(s_uri, fA);
  y := createObjC(s_uri_2, fB);
  CheckEquals(False,eh.equalShallow(x,y));
  CheckEquals(False,eh.equalShallow(y,x));
end;

procedure TSDOEqualityHelper_Test.equalShallow_object_not_equal_value();
var
  fA, fB : ISDODataFactory;
  x, y : ISDODataObject;
  eh : TSDOEqualityHelperClass;
  ls : ISDODataObjectList;
begin
  fA := CreateFactory();
  fB := CreateFactory();
  eh := CreateEqualityHelper();

// ObjectA
  x := createObjA(s_uri, fA);
  y := createObjA(s_uri, fA);
  CheckEquals(True,eh.equalShallow(x,y));
  CheckEquals(True,eh.equalShallow(y,x));

  x.setBoolean(s_bool_prop, not x.getBoolean(s_bool_prop));
    CheckEquals(False,eh.equalShallow(x,y));
    CheckEquals(False,eh.equalShallow(y,x));
  x.setBoolean(s_bool_prop, not x.getBoolean(s_bool_prop));

  x.setByte(s_byte_prop, 98);
    CheckEquals(False,eh.equalShallow(x,y));
    CheckEquals(False,eh.equalShallow(y,x));
  x.setByte(s_byte_prop, y.getByte(s_byte_prop));

  x.setInteger(s_integer_prop, 98766);
    CheckEquals(False,eh.equalShallow(x,y));
    CheckEquals(False,eh.equalShallow(y,x));
  x.setInteger(s_integer_prop, y.getInteger(s_integer_prop));

  x.setString(s_string_prop, 'mlkqsdf');
    CheckEquals(False,eh.equalShallow(x,y));
    CheckEquals(False,eh.equalShallow(y,x));
  x.setString(s_string_prop, y.getString(s_string_prop));

// ObjectB
  x := createObjB(s_uri, fA);
  y := createObjB(s_uri, fA);
  CheckEquals(True,eh.equalShallow(x,y));
  CheckEquals(True,eh.equalShallow(y,x));

  x.setBoolean(s_bool_prop, not x.getBoolean(s_bool_prop));
    CheckEquals(False,eh.equalShallow(x,y));
    CheckEquals(False,eh.equalShallow(y,x));
  x.setBoolean(s_bool_prop, not x.getBoolean(s_bool_prop));

  x.setByte(s_byte_prop, 98);
    CheckEquals(False,eh.equalShallow(x,y));
    CheckEquals(False,eh.equalShallow(y,x));
  x.setByte(s_byte_prop, y.getByte(s_byte_prop));

  x.setInteger(s_integer_prop, 98766);
    CheckEquals(False,eh.equalShallow(x,y));
    CheckEquals(False,eh.equalShallow(y,x));
  x.setInteger(s_integer_prop, y.getInteger(s_integer_prop));

  x.setString(s_string_prop, 'mlkqsdf');
    CheckEquals(False,eh.equalShallow(x,y));
    CheckEquals(False,eh.equalShallow(y,x));
  x.setString(s_string_prop, y.getString(s_string_prop));

  x.getDataObject(s_object_prop).setByte(s_byte_prop,51);
    CheckEquals(True,eh.equalShallow(x,y),'<equalShallow> should not compare object properties.');
    CheckEquals(True,eh.equalShallow(y,x),'<equalShallow> should not compare object properties.');
  x.getDataObject(s_object_prop).setByte(s_byte_prop,y.getDataObject(s_object_prop).getInteger(s_byte_prop));

  x.getDataObject(s_object_prop).setInteger(s_integer_prop,66666);
    CheckEquals(True,eh.equalShallow(x,y),'<equalShallow> should not compare object properties.');
    CheckEquals(True,eh.equalShallow(y,x),'<equalShallow> should not compare object properties.');
  x.getDataObject(s_object_prop).setInteger(s_integer_prop,y.getDataObject(s_object_prop).getInteger(s_integer_prop));

  ls := x.getList(s_integer_propList);
  ls.getCursor().MoveLast();
  ls.setInteger(-777);
    CheckEquals(False,eh.equalShallow(x,y));
    CheckEquals(False,eh.equalShallow(y,x));
end;

procedure TSDOEqualityHelper_Test.equal_object();
var
  fA, fB : ISDODataFactory;
  x, y : ISDODataObject;
  eh : TSDOEqualityHelperClass;
begin
  fA := CreateFactory();
  fB := CreateFactory();
  eh := CreateEqualityHelper();

  x := createObjA(s_uri, fA);
  y := createObjA(s_uri, fA);
  CheckEquals(True,eh.equalShallow(x,y));
  CheckEquals(True,eh.equalShallow(y,x));

  x := createObjA(s_uri, fA);
  y := createObjA(s_uri, fB);
  CheckEquals(True,eh.equalShallow(x,y));
  CheckEquals(True,eh.equalShallow(y,x));
end;

procedure TSDOEqualityHelper_Test.equal_object_not_equal_type();
var
  fA, fB : ISDODataFactory;
  x, y : ISDODataObject;
  eh : TSDOEqualityHelperClass;
begin
  fA := CreateFactory();
  fB := CreateFactory();
  eh := CreateEqualityHelper();

  x := createObjA(s_uri, fA);
  y := createObjA(s_uri_2, fA);
  CheckEquals(False,eh.equal(x,y));
  CheckEquals(False,eh.equal(y,x));

  x := createObjA(s_uri, fA);
  y := createObjC(s_uri_2, fA);
  CheckEquals(False,eh.equal(x,y));
  CheckEquals(False,eh.equal(y,x));


  x := createObjA(s_uri, fA);
  y := createObjA(s_uri_2, fB);
  CheckEquals(False,eh.equal(x,y));
  CheckEquals(False,eh.equal(y,x));

  x := createObjA(s_uri, fA);
  y := createObjC(s_uri_2, fB);
  CheckEquals(False,eh.equal(x,y));
  CheckEquals(False,eh.equal(y,x));
end;

procedure TSDOEqualityHelper_Test.equal_object_not_equal_value();
var
  fA, fB : ISDODataFactory;
  x, y : ISDODataObject;
  eh : TSDOEqualityHelperClass;
  ls : ISDODataObjectList;
  tmpInt : TSDOInteger;
begin
  fA := CreateFactory();
  fB := CreateFactory();
  eh := CreateEqualityHelper();

// ObjectA
  x := createObjA(s_uri, fA);
  y := createObjA(s_uri, fA);
  CheckEquals(True,eh.equal(x,y));
  CheckEquals(True,eh.equal(y,x));

  x.setBoolean(s_bool_prop, not x.getBoolean(s_bool_prop));
    CheckEquals(False,eh.equal(x,y));
    CheckEquals(False,eh.equal(y,x));
  x.setBoolean(s_bool_prop, not x.getBoolean(s_bool_prop));

  x.setByte(s_byte_prop, 64);
    CheckEquals(False,eh.equal(x,y));
    CheckEquals(False,eh.equal(y,x));
  x.setByte(s_byte_prop, y.getByte(s_byte_prop));

  x.setInteger(s_integer_prop, 98766);
    CheckEquals(False,eh.equal(x,y));
    CheckEquals(False,eh.equal(y,x));
  x.setInteger(s_integer_prop, y.getInteger(s_integer_prop));

  x.setString(s_string_prop, 'mlkqsdf');
    CheckEquals(False,eh.equal(x,y));
    CheckEquals(False,eh.equal(y,x));
  x.setString(s_string_prop, y.getString(s_string_prop));

// ObjectB
  x := createObjB(s_uri, fA);
  y := createObjB(s_uri, fA);
  CheckEquals(True,eh.equal(x,y));
  CheckEquals(True,eh.equal(y,x));

  x.setBoolean(s_bool_prop, not x.getBoolean(s_bool_prop));
    CheckEquals(False,eh.equal(x,y));
    CheckEquals(False,eh.equal(y,x));
  x.setBoolean(s_bool_prop, not x.getBoolean(s_bool_prop));

  x.setByte(s_byte_prop, 64);
    CheckEquals(False,eh.equal(x,y));
    CheckEquals(False,eh.equal(y,x));
  x.setByte(s_byte_prop, y.getByte(s_byte_prop));
    
  CheckEquals(True,eh.equal(x,y));
  x.setInteger(s_integer_prop, 98766);
    CheckEquals(False,eh.equal(x,y));
    CheckEquals(False,eh.equal(y,x));
  x.setInteger(s_integer_prop, y.getInteger(s_integer_prop));

  CheckEquals(True,eh.equal(x,y));
  x.setString(s_string_prop, 'mlkqsdf');
    CheckEquals(False,eh.equal(x,y));
    CheckEquals(False,eh.equal(y,x));
  x.setString(s_string_prop, y.getString(s_string_prop));

  CheckEquals(True,eh.equal(x,y));
  x.getDataObject(s_object_prop).setInteger(s_integer_prop,66666);
    CheckEquals(False,eh.equal(x,y),'<equalShallow> should compare object properties.');
    CheckEquals(False,eh.equal(y,x),'<equalShallow> should compare object properties.');
  if y.getDataObject(s_object_prop).isSet(s_integer_prop) then
    x.getDataObject(s_object_prop).setInteger(s_integer_prop,y.getDataObject(s_object_prop).getInteger(s_integer_prop))
  else
    x.getDataObject(s_object_prop).unset(s_integer_prop);

  CheckEquals(True,eh.equal(x,y));
  ls := x.getList(s_integer_propList);
  ls.getCursor().MoveLast();
  tmpInt := ls.getInteger();
  ls.setInteger(-777);
    CheckEquals(False,eh.equalShallow(x,y));
    CheckEquals(False,eh.equalShallow(y,x));
  ls.setInteger(tmpInt);
end;

procedure TSDOEqualityHelper_Test.equalShallow_cursorPos();
var
  fA : ISDODataFactory;
  x, y : ISDODataObject;
  lsB, lsI : ISDODataObjectList;
  eh : TSDOEqualityHelperClass;
  bmB, bmI : TLinkedListBookmark;
begin
  fA := CreateFactory();
  eh := CreateEqualityHelper();

  x := createObjA(s_uri, fA);
    lsB := x.getList(s_bool_propList);
      lsB.getCursor().MoveFirst();
      lsB.getCursor().MoveNext();
      lsB.getCursor().MoveNext();
    lsI := x.getList(s_integer_propList);
      lsI.getCursor().MoveLast();
      lsI.getCursor().MovePrevious();
  y := createObjA(s_uri, fA);
  bmB := lsB.getCursor().GetBookmark();
  bmI := lsI.getCursor().GetBookmark();

  eh.equalShallow(x,y);
    CheckEquals(bmB,lsB.getCursor().GetBookmark());
    CheckEquals(bmI,lsI.getCursor().GetBookmark());

  eh.equalShallow(y,x);
    CheckEquals(bmB,lsB.getCursor().GetBookmark());
    CheckEquals(bmI,lsI.getCursor().GetBookmark());
end;

procedure TSDOEqualityHelper_Test.equal_cursorPos();
var
  fA : ISDODataFactory;
  x, y : ISDODataObject;
  lsB, lsI : ISDODataObjectList;
  eh : TSDOEqualityHelperClass;
  bmB, bmI : TLinkedListBookmark;
begin
  fA := CreateFactory();
  eh := CreateEqualityHelper();

  x := createObjA(s_uri, fA);
    lsB := x.getList(s_bool_propList);
      lsB.getCursor().MoveFirst();
      lsB.getCursor().MoveNext();
      lsB.getCursor().MoveNext();
    lsI := x.getList(s_integer_propList);
      lsI.getCursor().MoveLast();
      lsI.getCursor().MovePrevious();
  y := createObjA(s_uri, fA);
  bmB := lsB.getCursor().GetBookmark();
  bmI := lsI.getCursor().GetBookmark();

  eh.equal(x,y);
    CheckEquals(bmB,lsB.getCursor().GetBookmark());
    CheckEquals(bmI,lsI.getCursor().GetBookmark());

  eh.equal(y,x);
    CheckEquals(bmB,lsB.getCursor().GetBookmark());
    CheckEquals(bmI,lsI.getCursor().GetBookmark());
end;

initialization
  RegisterTest('Helpers',TSDOEqualityHelper_Test.Suite);

end.
