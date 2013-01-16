{$INCLUDE sdo_global.inc}
unit test_dataobject;

interface
uses SysUtils
{$IFDEF FPC}
  ,fpcunit, testutils, testregistry
{$ENDIF}
{$IFNDEF FPC}
  ,TestFrameWork
{$ENDIF}
  , test_suite_utils, sdo, sdo_type, sdo_types, sdo_field_imp ;

type

  TSDOBaseDataObject_Test = class(TWstBaseTest)
  private
    FFactory : ISDODataFactory;
  protected
    class function is_open_type() : Boolean;virtual;abstract;
    class function Create_Factory() : ISDODataFactory;
    class function GetTestSuitePath() : string;
    function Create_Object() : ISDODataObject;
    procedure check_xpath_value(
      const AExpected : TSDOInteger;
      const AObject : ISDODataObject;
      const AXPath : string
    );overload;
    procedure check_xpath_value(
      const AExpected : TSDOByte;
      const AObject : ISDODataObject;
      const AXPath : string
    );overload;
    procedure check_xpath_value(
      const AExpected : TSDOBoolean;
      const AObject : ISDODataObject;
      const AXPath : string
    );overload;
    procedure check_xpath_value(
      const AExpected : TSDODateTime;
      const AObject : ISDODataObject;
      const AXPath : string
    );overload;
    procedure check_xpath_value(
      const AExpected : TSDOString;
      const AObject : ISDODataObject;
      const AXPath : string
    );overload;
{$IFDEF HAS_SDO_BYTES}
    procedure check_xpath_value(
      const AExpected : TSDOBytes;
      const AObject : ISDODataObject;
      const AXPath : string
    );overload;
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure check_xpath_value(
      const AExpected : TSDOChar;
      const AObject : ISDODataObject;
      const AXPath : string
    );overload;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure check_xpath_value_currency(
      const AExpected : TSDOCurrency;
      const AObject : ISDODataObject;
      const AXPath : string
    );overload;
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure check_xpath_value(
      const AExpected : TSDODouble;
      const AObject : ISDODataObject;
      const AXPath : string
    );overload;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure check_xpath_value(
      const AExpected : TSDOFloat;
      const AObject : ISDODataObject;
      const AXPath : string
    );overload;
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure check_xpath_value(
      const AExpected : TSDOLong;
      const AObject : ISDODataObject;
      const AXPath : string
    );overload;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure check_xpath_value(
      const AExpected : TSDOShort;
      const AObject : ISDODataObject;
      const AXPath : string
    );overload;
{$ENDIF HAS_SDO_SHORT}
  protected
    procedure SetUp(); override;
    procedure TearDown(); override;
  public
    procedure CheckEquals(expected, actual: TSDODateTime; msg: string = ''; const AStrict : Boolean = True); overload;
  published
    procedure IsInstanceOf();
    procedure object_destroy_order();
    procedure object_getChangeSummary();
    procedure object_getChangeSummary_multiprop();
    procedure object_setDataObject_cycle_containment();
    procedure object_setDataObject_ref();
    procedure object_setDataObject_ref_nil();
    procedure object_setDataObject_ref_nil_nested();
    procedure object_setDataObject_ref_unset();
    procedure object_setDataObject_ref_unset_nested();
    procedure object_setDataObject_ref_setnull();
    procedure object_setDataObject_ref_setnull_nested();


    procedure boolean_procs();
    procedure byte_procs();
    procedure integer_procs();
    procedure string_procs();
    procedure object_procs();
    procedure date_procs();
{$IFDEF HAS_SDO_BYTES}
    procedure bytes_procs();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure char_procs();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure currency_procs();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure double_procs();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure float_procs();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure long_procs();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure short_procs();
{$ENDIF HAS_SDO_SHORT}


    procedure byte_multivalue();
    procedure date_multivalue();
    procedure integer_multivalue();
    procedure object_ref_multivalue();
    procedure object_cont_multivalue();
{$IFDEF HAS_SDO_BYTES}
    procedure bytes_multivalue();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure char_multivalue();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure currency_multivalue();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure double_multivalue();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure float_multivalue();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure long_multivalue();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure short_multivalue();
{$ENDIF HAS_SDO_SHORT}

    procedure integer_unset_isset();
    procedure boolean_unset_isset();
    procedure byte_unset_isset();
    procedure string_unset_isset();
    procedure object_unset_isset();
    procedure date_unset_isset();
{$IFDEF HAS_SDO_BYTES}
    procedure bytes_unset_isset();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure char_unset_isset();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure currency_unset_isset();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure double_unset_isset();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure float_unset_isset();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure long_unset_isset();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure short_unset_isset();
{$ENDIF HAS_SDO_SHORT}


    procedure integer_setnull_isnull();
    procedure boolean_setnull_isnull();
    procedure byte_setnull_isnull();
    procedure string_setnull_isnull();
    procedure date_setnull_isnull();
{$IFDEF HAS_SDO_BYTES}
    procedure bytes_setnull_isnull();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure char_setnull_isnull();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure currency_setnull_isnull();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure double_setnull_isnull();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure float_setnull_isnull();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure long_setnull_isnull();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure short_setnull_isnull();
{$ENDIF HAS_SDO_SHORT}

    procedure property_default_value_integer();
    procedure property_default_value_bool();
    procedure property_default_value_byte();
    procedure property_default_value_string();
    procedure property_default_value_date();
{$IFDEF HAS_SDO_BYTES}
    procedure property_default_value_bytes();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure property_default_value_char();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure property_default_value_currency();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure property_default_value_double();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure property_default_value_float();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure property_default_value_long();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure property_default_value_short();
{$ENDIF HAS_SDO_SHORT}


    procedure property_default_value_unset_integer();
    procedure property_default_value_unset_bool();
    procedure property_default_value_unset_byte();
    procedure property_default_value_unset_string();
    procedure property_default_value_unset_date();
{$IFDEF HAS_SDO_BYTES}
    procedure property_default_value_unset_bytes();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure property_default_value_unset_char();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure property_default_value_unset_currency();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure property_default_value_unset_double();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure property_default_value_unset_float();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure property_default_value_unset_long();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure property_default_value_unset_short();
{$ENDIF HAS_SDO_SHORT}

    procedure get_bool_xpath();
    procedure get_byte_xpath();
    procedure get_date_xpath();
    procedure get_integer_xpath();
    procedure get_string_xpath();
{$IFDEF HAS_SDO_BYTES}
    procedure get_bytes_xpath();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure get_char_xpath();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure get_currency_xpath();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure get_double_xpath();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure get_float_xpath();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure get_long_xpath();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure get_short_xpath();
{$ENDIF HAS_SDO_SHORT}
  end;

  TSDODataObject_Test = class(TSDOBaseDataObject_Test)
  protected
    class function is_open_type() : Boolean;override;
  end;

  TSDOOpenedDataObject_Test = class(TSDOBaseDataObject_Test)
  protected
    class function is_open_type() : Boolean;override;
    procedure check_property(
      const APropList : ISDOPropertyList;
      const AName : string;
      const AType : ISDOType;
      const AFlags : TPropertyFlags
    );
  published
    procedure addProperty();
    procedure addProperty_error();
    procedure addProperty_byte();
    procedure addProperty_date();
{$IFDEF HAS_SDO_BYTES}
    procedure addProperty_bytes();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure addProperty_char();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure addProperty_currency();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure addProperty_double();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure addProperty_float();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure addProperty_long();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure addProperty_short();
{$ENDIF HAS_SDO_SHORT}


    procedure addProperty_multi_value();
    procedure addProperty_multi_value_byte();
    procedure addProperty_multi_value_date();
{$IFDEF HAS_SDO_BYTES}
    procedure addProperty_multi_value_bytes();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure addProperty_multi_value_char();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure addProperty_multi_value_currency();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure addProperty_multi_value_double();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure addProperty_multi_value_float();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure addProperty_multi_value_long();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure addProperty_multi_value_short();
{$ENDIF HAS_SDO_SHORT}

    procedure implicit_add_property();
    procedure implicit_add_property_byte();
    procedure implicit_add_property_date();
{$IFDEF HAS_SDO_BYTES}
    procedure implicit_add_property_bytes();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure implicit_add_property_char();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure implicit_add_property_currency();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure implicit_add_property_double();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure implicit_add_property_float();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure implicit_add_property_long();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure implicit_add_property_short();
{$ENDIF HAS_SDO_SHORT}
  end;

  TObserver_Test = class(TTestCase)
  published
    procedure ObserverInfo_create();

    procedure ObserverList_add_find();
  end;

implementation

uses
  sdo_datafactory, Math, sdo_linked_list, sdo_dataobject,
  sdo_consts, DateUtils, sdo_date_utils;

const s_URI_1  = 'uri:1'; s_URI_3 = 'uri:3';
      s_TYPE_1 = 'type1'; s_TYPE_2 = 'type2'; s_TYPE_3 = 'type3'; s_TYPE_4 = 'type4';
      s_PROP_BOOL_1 = 'propboolean1';
        s_PROP_BOOL_2 = 'propboolean2';
        s_PROP_BOOL_3 = 'propboolean3';
      s_PROP_BYTE_1 = 'propbyte1';
        s_PROP_BYTE_2 = 'propbyte2';
        s_PROP_BYTE_3 = 'propbyte3';
      s_PROP_BYTES_1 = 'propbytes1';
        s_PROP_BYTES_2 = 'propbytes2';
        s_PROP_BYTES_3 = 'propbytes3';
      s_PROP_CHAR_1 = 'propchar1';
        s_PROP_CHAR_2 = 'propchar2';
        s_PROP_CHAR_3 = 'propchar3';
      s_PROP_CURRENCY_1 = 'propcurrency1';
        s_PROP_CURRENCY_2 = 'propcurrency2';
        s_PROP_CURRENCY_3 = 'propcurrency3';
      s_PROP_DATE_1 = 'propdate1';
        s_PROP_DATE_2 = 'propdate2';
        s_PROP_DATE_3 = 'propdate3';
      s_PROP_DOUBLE_1 = 'propdouble1';
        s_PROP_DOUBLE_2 = 'propdouble2';
        s_PROP_DOUBLE_3 = 'propdouble3';
      s_PROP_FLOAT_1 = 'propfloat1';
        s_PROP_FLOAT_2 = 'propfloat2';
        s_PROP_FLOAT_3 = 'propfloat3';
      s_PROP_INTEGER_1 = 'proplong1';
        s_PROP_INTEGER_2 = 'proplong2';
        s_PROP_INTEGER_3 = 'proplong3';
      s_PROP_LONG_1 = 'propinteger1';
        s_PROP_LONG_2 = 'propinteger2';
        s_PROP_LONG_3 = 'propinteger3';
      s_PROP_SHORT_1 = 'propshort1';
        s_PROP_SHORT_2 = 'propshort2';
        s_PROP_SHORT_3 = 'propshort3';
      s_PROP_STR_1 = 'propStr1';
        s_PROP_STR_2 = 'propStr2';
        s_PROP_STR_3 = 'propStr3';
      s_PROP_OBJ_CONT = 'propobj_cont';
        s_PROP_OBJ_REF = 'propobj_ref';
        s_PROP_OBJ_CONT_LIST = 'propobj_cont_list';
        s_PROP_OBJ_REF_LIST = 'propobj_ref_list';

      s_PROP_BOOL_A = 'propbooleanA';
        s_PROP_BOOL_B = 'propbooleanB';
      s_PROP_BYTE_A = 'propbyteA';
        s_PROP_BYTE_B = 'propbyteB';
      s_PROP_BYTES_A = 'propbytesA';
        s_PROP_BYTES_B = 'propbytesB';
      s_PROP_CHAR_A = 'propcharA';
        s_PROP_CHAR_B = 'propcharB';
      s_PROP_CURRENCY_A = 'propcurrencyA';
        s_PROP_CURRENCY_B = 'propcurrencyB';
      s_PROP_DATE_A = 'propdateA';
        s_PROP_DATE_B = 'propdateB';
      s_PROP_DOUBLE_A = 'propdoubleA';
        s_PROP_DOUBLE_B = 'propdoubleB';
      s_PROP_FLOAT_A = 'propfloatA';
        s_PROP_FLOAT_B = 'propfloatB';
      s_PROP_INTEGER_A = 'propintegerA';
        s_PROP_INTEGER_B = 'propintegerB';
      s_PROP_LONG_A = 'proplongA';
        s_PROP_LONG_B = 'proplongB';
      s_PROP_SHORT_A = 'propshortA';
        s_PROP_SHORT_B = 'propshortB';
      s_PROP_STR_A = 'propStrA';
        s_PROP_STR_B = 'propStrB';

{ TSDOBaseDataObject_Test }

function TSDOBaseDataObject_Test.Create_Object() : ISDODataObject;
begin
  Result := FFactory.createNew(s_URI_1,s_TYPE_1);
end;

class function TSDOBaseDataObject_Test.Create_Factory() : ISDODataFactory;
var
  typ, typ2, typ3 : ISDOType;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  Result := TSDODataFactory.Create();
  Result.AddType(s_URI_1,s_TYPE_1,tfg);
  Result.AddType(s_URI_1,s_TYPE_2,tfg);
  Result.AddType(s_URI_3,s_TYPE_3,tfg);
  Result.AddType(s_URI_1,s_TYPE_4,tfg);
  typ := Result.getType(s_URI_1,s_TYPE_1);
    Result.addProperty(typ,s_PROP_BOOL_1,sdo_namespace,'Boolean',[]);
    Result.addProperty(typ,s_PROP_BYTE_1,sdo_namespace,'Byte',[]);
{$IFDEF HAS_SDO_BYTES}
    Result.addProperty(typ,s_PROP_BYTES_1,sdo_namespace,'Bytes',[]);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    Result.addProperty(typ,s_PROP_CHAR_1,sdo_namespace,'Character',[]);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    Result.addProperty(typ,s_PROP_CURRENCY_1,sdo_namespace,'Currency',[]);
{$ENDIF HAS_SDO_CURRENCY}
    Result.addProperty(typ,s_PROP_DATE_1,sdo_namespace,'Date',[]);
{$IFDEF HAS_SDO_DOUBLE}
    Result.addProperty(typ,s_PROP_DOUBLE_1,sdo_namespace,'Double',[]);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    Result.addProperty(typ,s_PROP_FLOAT_1,sdo_namespace,'Float',[]);
{$ENDIF HAS_SDO_FLOAT}
    Result.addProperty(typ,s_PROP_INTEGER_1,sdo_namespace,'Integer',[]);
{$IFDEF HAS_SDO_LONG}
    Result.addProperty(typ,s_PROP_LONG_1,sdo_namespace,'Long',[]);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    Result.addProperty(typ,s_PROP_SHORT_1,sdo_namespace,'Short',[]);
{$ENDIF HAS_SDO_SHORT}
    Result.addProperty(typ,s_PROP_STR_1,sdo_namespace,'String',[]);

    Result.addProperty(typ,s_PROP_BOOL_2,sdo_namespace,'Boolean',[]);
    Result.addProperty(typ,s_PROP_BYTE_2,sdo_namespace,'Byte',[]);
{$IFDEF HAS_SDO_BYTES}
    Result.addProperty(typ,s_PROP_BYTES_2,sdo_namespace,'Bytes',[]);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    Result.addProperty(typ,s_PROP_CHAR_2,sdo_namespace,'Character',[]);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    Result.addProperty(typ,s_PROP_CURRENCY_2,sdo_namespace,'Currency',[]);
{$ENDIF HAS_SDO_CURRENCY}
    Result.addProperty(typ,s_PROP_DATE_2,sdo_namespace,'Date',[]);
{$IFDEF HAS_SDO_DOUBLE}
    Result.addProperty(typ,s_PROP_DOUBLE_2,sdo_namespace,'Double',[]);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    Result.addProperty(typ,s_PROP_FLOAT_2,sdo_namespace,'Float',[]);
{$ENDIF HAS_SDO_FLOAT}
    Result.addProperty(typ,s_PROP_INTEGER_2,sdo_namespace,'Integer',[]);
{$IFDEF HAS_SDO_LONG}
    Result.addProperty(typ,s_PROP_LONG_2,sdo_namespace,'Long',[]);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    Result.addProperty(typ,s_PROP_SHORT_2,sdo_namespace,'Short',[]);
{$ENDIF HAS_SDO_SHORT}
    Result.addProperty(typ,s_PROP_STR_2,sdo_namespace,'String',[]);
    Result.addProperty(typ,s_PROP_OBJ_CONT,s_URI_1,s_TYPE_2,[pfIsContainment]);
    Result.addProperty(typ,s_PROP_OBJ_REF,s_URI_1,s_TYPE_2,[]);
    Result.addProperty(typ,s_PROP_OBJ_CONT_LIST,s_URI_1,s_TYPE_2,[pfIsMany,pfIsContainment]);
    Result.addProperty(typ,s_PROP_OBJ_REF_LIST,s_URI_1,s_TYPE_2,[pfIsMany]);

    Result.addProperty(typ,s_PROP_BOOL_3,sdo_namespace,'Boolean',[pfIsMany]);
    Result.addProperty(typ,s_PROP_BYTE_3,sdo_namespace,'Byte',[pfIsMany]);
{$IFDEF HAS_SDO_BYTES}
    Result.addProperty(typ,s_PROP_BYTES_3,sdo_namespace,'Bytes',[pfIsMany]);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    Result.addProperty(typ,s_PROP_CHAR_3,sdo_namespace,'Character',[pfIsMany]);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    Result.addProperty(typ,s_PROP_CURRENCY_3,sdo_namespace,'Currency',[pfIsMany]);
{$ENDIF HAS_SDO_CURRENCY}
    Result.addProperty(typ,s_PROP_DATE_3,sdo_namespace,'Date',[pfIsMany]);
{$IFDEF HAS_SDO_DOUBLE}
    Result.addProperty(typ,s_PROP_DOUBLE_3,sdo_namespace,'Double',[pfIsMany]);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    Result.addProperty(typ,s_PROP_FLOAT_3,sdo_namespace,'Float',[pfIsMany]);
{$ENDIF HAS_SDO_FLOAT}
    Result.addProperty(typ,s_PROP_INTEGER_3,sdo_namespace,'Integer',[pfIsMany]);
{$IFDEF HAS_SDO_LONG}
    Result.addProperty(typ,s_PROP_LONG_3,sdo_namespace,'Long',[pfIsMany]);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    Result.addProperty(typ,s_PROP_SHORT_3,sdo_namespace,'Short',[pfIsMany]);
{$ENDIF HAS_SDO_SHORT}
    Result.addProperty(typ,s_PROP_STR_3,sdo_namespace,'String',[pfIsMany]);

  typ2 := Result.getType(s_URI_1,s_TYPE_2);
    Result.addProperty(typ2,s_PROP_BOOL_A,sdo_namespace,'Boolean',[]);
    Result.addProperty(typ2,s_PROP_BYTE_A,sdo_namespace,'Byte',[]);
{$IFDEF HAS_SDO_BYTES}
    Result.addProperty(typ2,s_PROP_BYTES_A,sdo_namespace,'Bytes',[]);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    Result.addProperty(typ2,s_PROP_CHAR_A,sdo_namespace,'Character',[]);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    Result.addProperty(typ2,s_PROP_CURRENCY_A,sdo_namespace,'Currency',[]);
{$ENDIF HAS_SDO_CURRENCY}
    Result.addProperty(typ2,s_PROP_DATE_A,sdo_namespace,'Date',[]);
{$IFDEF HAS_SDO_DOUBLE}
    Result.addProperty(typ2,s_PROP_DOUBLE_A,sdo_namespace,'Double',[]);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    Result.addProperty(typ2,s_PROP_FLOAT_A,sdo_namespace,'Float',[]);
{$ENDIF HAS_SDO_FLOAT}
    Result.addProperty(typ2,s_PROP_INTEGER_A,sdo_namespace,'Integer',[]);
{$IFDEF HAS_SDO_LONG}
    Result.addProperty(typ2,s_PROP_LONG_A,sdo_namespace,'Long',[]);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    Result.addProperty(typ2,s_PROP_SHORT_A,sdo_namespace,'Short',[]);
{$ENDIF HAS_SDO_SHORT}
    Result.addProperty(typ2,s_PROP_STR_A,sdo_namespace,'String',[]);
    Result.addProperty(typ2,s_PROP_OBJ_CONT,s_URI_3,s_TYPE_3,[pfIsContainment]);

  typ3 := Result.getType(s_URI_3,s_TYPE_3);
    Result.addProperty(typ3,s_PROP_BOOL_2,sdo_namespace,'Boolean',[]);
    Result.addProperty(typ3,s_PROP_BYTE_2,sdo_namespace,'Byte',[]);
{$IFDEF HAS_SDO_BYTES}
    Result.addProperty(typ3,s_PROP_BYTES_2,sdo_namespace,'Bytes',[]);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    Result.addProperty(typ3,s_PROP_CHAR_2,sdo_namespace,'Character',[]);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    Result.addProperty(typ3,s_PROP_CURRENCY_2,sdo_namespace,'Currency',[]);
{$ENDIF HAS_SDO_CURRENCY}
    Result.addProperty(typ3,s_PROP_DATE_2,sdo_namespace,'Date',[]);
{$IFDEF HAS_SDO_DOUBLE}
    Result.addProperty(typ3,s_PROP_DOUBLE_2,sdo_namespace,'Double',[]);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    Result.addProperty(typ3,s_PROP_FLOAT_2,sdo_namespace,'Float',[]);
{$ENDIF HAS_SDO_FLOAT}
    Result.addProperty(typ3,s_PROP_INTEGER_2,sdo_namespace,'Integer',[]);
{$IFDEF HAS_SDO_LONG}
    Result.addProperty(typ3,s_PROP_LONG_2,sdo_namespace,'Long',[]);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    Result.addProperty(typ3,s_PROP_SHORT_2,sdo_namespace,'Short',[]);
{$ENDIF HAS_SDO_SHORT}
    Result.addProperty(typ3,s_PROP_STR_2,sdo_namespace,'String',[]);
    Result.addProperty(typ3,s_PROP_OBJ_CONT,s_URI_1,s_TYPE_4,[pfIsContainment]);
end;

class function TSDOBaseDataObject_Test.GetTestSuitePath() : string;
begin
  Result := 'object';
end;

procedure TSDOBaseDataObject_Test.SetUp();
begin
  inherited;
  FFactory := Create_Factory();
end;

procedure TSDOBaseDataObject_Test.TearDown();
begin
  FFactory := nil;
  inherited;
end;

procedure TSDOBaseDataObject_Test.boolean_procs();
const VAL_1 = True; VAL_2 = False;
var
  obj : ISDODataObject;
  ok : Boolean;
begin
  obj := Create_Object();

  ok := False;
  try
    obj.setBoolean(obj.getProperty('qsdc'),VAL_1);
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.getBoolean(obj.getProperty('qsdc'));
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.setBoolean(obj.getProperty(s_PROP_BOOL_3),VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOIllegalArgumentException.');

  obj.setBoolean(obj.getProperty(s_PROP_BOOL_1),VAL_1);
  CheckEquals(VAL_1,obj.getBoolean(obj.getProperty(s_PROP_BOOL_1)));
  CheckEquals(VAL_1,obj.getBoolean(s_PROP_BOOL_1));
  CheckEquals(VAL_1,obj.getBoolean(obj.getPropertyIndex(obj.getProperty(s_PROP_BOOL_1))));

  obj.setBoolean(obj.getProperty(s_PROP_BOOL_1),VAL_2);
  CheckEquals(VAL_2,obj.getBoolean(obj.getProperty(s_PROP_BOOL_1)));
  CheckEquals(VAL_2,obj.getBoolean(s_PROP_BOOL_1));
  CheckEquals(VAL_2,obj.getBoolean(obj.getPropertyIndex(obj.getProperty(s_PROP_BOOL_1))));

  obj.setBoolean(obj.getProperty(s_PROP_BOOL_2),VAL_1);
  CheckEquals(VAL_1,obj.getBoolean(obj.getProperty(s_PROP_BOOL_2)));
  CheckEquals(VAL_1,obj.getBoolean(s_PROP_BOOL_2));
  CheckEquals(VAL_1,obj.getBoolean(obj.getPropertyIndex(obj.getProperty(s_PROP_BOOL_2))));

  obj.setBoolean(obj.getProperty(s_PROP_BOOL_2),VAL_2);
  CheckEquals(VAL_2,obj.getBoolean(obj.getProperty(s_PROP_BOOL_2)));
  CheckEquals(VAL_2,obj.getBoolean(s_PROP_BOOL_2));
  CheckEquals(VAL_2,obj.getBoolean(obj.getPropertyIndex(obj.getProperty(s_PROP_BOOL_2))));
end;

procedure TSDOBaseDataObject_Test.integer_procs();
const VAL_1 = 1210; VAL_2 = -97456; VAL_3 = 0;
var
  obj : ISDODataObject;
  ok : Boolean;
begin
  obj := Create_Object();

  ok := False;
  try
    obj.setInteger(obj.getProperty('qsdc'),VAL_1);
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.getInteger(obj.getProperty('qsdc'));
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.setInteger(obj.getProperty(s_PROP_INTEGER_3),VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOIllegalArgumentException.');

  obj.setInteger(obj.getProperty(s_PROP_INTEGER_1),VAL_1);
  CheckEquals(VAL_1,obj.getInteger(obj.getProperty(s_PROP_INTEGER_1)));
  CheckEquals(VAL_1,obj.getInteger(s_PROP_INTEGER_1));
  CheckEquals(VAL_1,obj.getInteger(obj.getPropertyIndex(obj.getProperty(s_PROP_INTEGER_1))));

  obj.setInteger(obj.getProperty(s_PROP_INTEGER_1),VAL_2);
  CheckEquals(VAL_2,obj.getInteger(obj.getProperty(s_PROP_INTEGER_1)));
  CheckEquals(VAL_2,obj.getInteger(s_PROP_INTEGER_1));
  CheckEquals(VAL_2,obj.getInteger(obj.getPropertyIndex(obj.getProperty(s_PROP_INTEGER_1))));

  obj.setInteger(obj.getProperty(s_PROP_INTEGER_1),VAL_3);
  CheckEquals(VAL_3,obj.getInteger(obj.getProperty(s_PROP_INTEGER_1)));
  CheckEquals(VAL_3,obj.getInteger(s_PROP_INTEGER_1));
  CheckEquals(VAL_3,obj.getInteger(obj.getPropertyIndex(obj.getProperty(s_PROP_INTEGER_1))));


  obj.setInteger(obj.getProperty(s_PROP_INTEGER_2),VAL_1);
  CheckEquals(VAL_1,obj.getInteger(obj.getProperty(s_PROP_INTEGER_2)));
  CheckEquals(VAL_1,obj.getInteger(s_PROP_INTEGER_2));
  CheckEquals(VAL_1,obj.getInteger(obj.getPropertyIndex(obj.getProperty(s_PROP_INTEGER_2))));

  obj.setInteger(obj.getProperty(s_PROP_INTEGER_2),VAL_2);
  CheckEquals(VAL_2,obj.getInteger(obj.getProperty(s_PROP_INTEGER_2)));
  CheckEquals(VAL_2,obj.getInteger(s_PROP_INTEGER_2));
  CheckEquals(VAL_2,obj.getInteger(obj.getPropertyIndex(obj.getProperty(s_PROP_INTEGER_2))));

  obj.setInteger(obj.getProperty(s_PROP_INTEGER_2),VAL_3);
  CheckEquals(VAL_3,obj.getInteger(obj.getProperty(s_PROP_INTEGER_2)));
  CheckEquals(VAL_3,obj.getInteger(s_PROP_INTEGER_2));
  CheckEquals(VAL_3,obj.getInteger(obj.getPropertyIndex(obj.getProperty(s_PROP_INTEGER_2))));
end;

procedure TSDOBaseDataObject_Test.string_procs();
const VAL_1 = 'klmhgf[]1210'; VAL_2 = ''; VAL_3 = 'wxyz';
var
  obj : ISDODataObject;
  ok : Boolean;
begin
  obj := Create_Object();

  ok := False;
  try
    obj.setString(obj.getProperty('qsdc'),VAL_1);
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.setString(obj.getProperty('qsdc'),VAL_1);
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  obj.setString(obj.getProperty(s_PROP_STR_1),VAL_1);
  CheckEquals(VAL_1,obj.getString(obj.getProperty(s_PROP_STR_1)));
  CheckEquals(VAL_1,obj.getString(s_PROP_STR_1));
  CheckEquals(VAL_1,obj.getString(obj.getPropertyIndex(obj.getProperty(s_PROP_STR_1))));

  obj.setString(obj.getProperty(s_PROP_STR_1),VAL_2);
  CheckEquals(VAL_2,obj.getString(obj.getProperty(s_PROP_STR_1)));
  CheckEquals(VAL_2,obj.getString(s_PROP_STR_1));
  CheckEquals(VAL_2,obj.getString(obj.getPropertyIndex(obj.getProperty(s_PROP_STR_1))));

  obj.setString(obj.getProperty(s_PROP_STR_1),VAL_3);
  CheckEquals(VAL_3,obj.getString(obj.getProperty(s_PROP_STR_1)));
  CheckEquals(VAL_3,obj.getString(s_PROP_STR_1));
  CheckEquals(VAL_3,obj.getString(obj.getPropertyIndex(obj.getProperty(s_PROP_STR_1))));


  obj.setString(obj.getProperty(s_PROP_STR_2),VAL_1);
  CheckEquals(VAL_1,obj.getString(obj.getProperty(s_PROP_STR_2)));
  CheckEquals(VAL_1,obj.getString(s_PROP_STR_2));
  CheckEquals(VAL_1,obj.getString(obj.getPropertyIndex(obj.getProperty(s_PROP_STR_2))));

  obj.setString(obj.getProperty(s_PROP_STR_2),VAL_2);
  CheckEquals(VAL_2,obj.getString(obj.getProperty(s_PROP_STR_2)));
  CheckEquals(VAL_2,obj.getString(s_PROP_STR_2));
  CheckEquals(VAL_2,obj.getString(obj.getPropertyIndex(obj.getProperty(s_PROP_STR_2))));

  obj.setString(obj.getProperty(s_PROP_STR_2),VAL_3);
  CheckEquals(VAL_3,obj.getString(obj.getProperty(s_PROP_STR_2)));
  CheckEquals(VAL_3,obj.getString(s_PROP_STR_2));
  CheckEquals(VAL_3,obj.getString(obj.getPropertyIndex(obj.getProperty(s_PROP_STR_2))));
end;

procedure TSDOBaseDataObject_Test.object_procs();
var
  ok : Boolean;
  obj1, obj2, obj2_2, obj3 : ISDODataObject;
  p1, p2 : ISDOProperty;
begin
  obj1 := FFactory.createNew(s_URI_1,s_TYPE_1);
  Check(nil = obj1.getDataObject(s_PROP_OBJ_CONT));

  obj3 := FFactory.createNew(s_URI_3,s_TYPE_3);

  ok := False;
  try
    obj1.setDataObject(s_PROP_OBJ_CONT,obj3);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  Check(ok,'incomptatible types.');

  ok := False;
  try
    obj1.getContainmentProperty();
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  Check(ok,'Containment property.');

  Check(nil = obj1.getDataObject(s_PROP_OBJ_CONT));

  obj2 := obj1.createDataObject(s_PROP_OBJ_CONT);
    Check(nil <> obj2);
    Check(nil <> obj1.getDataObject(s_PROP_OBJ_CONT));
    Check(obj2 = obj1.getDataObject(s_PROP_OBJ_CONT));
    Check(obj1 = obj2.getContainer());
    p1 := obj1.getProperty(s_PROP_OBJ_CONT) as ISDOProperty;
    p2 := obj2.getContainmentProperty() as ISDOProperty;
    Check( p1 = p2 );
    ok := False;
    try
      obj1.setDataObject(s_PROP_OBJ_CONT,obj3);
    except
      on e : ESDOIllegalArgumentException do begin
        ok := True;
      end;
    end;
    Check(ok,'incomptatible types.');
    Check(obj2 = obj1.getDataObject(s_PROP_OBJ_CONT), 'A failed action must not modify data');
    Check(obj1 = obj2.getContainer(), 'A failed action must not modify data');
    p1 := obj1.getProperty(s_PROP_OBJ_CONT) as ISDOProperty;
    p2 := obj2.getContainmentProperty() as ISDOProperty;
    Check( p1 = p2 , 'A failed action must not modify data');


  obj1.setDataObject(s_PROP_OBJ_CONT,obj2);
    Check(nil = obj3.getDataObject(s_PROP_OBJ_CONT));
    Check(nil <> obj1.getDataObject(s_PROP_OBJ_CONT));
    Check(obj2 = obj1.getDataObject(s_PROP_OBJ_CONT));
    Check(obj1 = obj2.getContainer());
    p1 := obj1.getProperty(s_PROP_OBJ_CONT) as ISDOProperty;
    p2 := obj2.getContainmentProperty() as ISDOProperty;
    Check( p1 = p2 );

  obj2_2 := FFactory.createNew(s_URI_1,s_TYPE_2);
  obj1.setDataObject(s_PROP_OBJ_REF,obj2_2);
    Check(nil = obj2_2.getContainer());
    Check(nil <> obj1.getDataObject(s_PROP_OBJ_REF));
    Check(obj2_2 = obj1.getDataObject(s_PROP_OBJ_REF));

  ok := False;
  try
    obj2_2.getContainmentProperty();
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  Check(ok,'Containment property.');
end;

procedure TSDOBaseDataObject_Test.integer_multivalue();
const LOCAL_PROP = s_PROP_INTEGER_3; LOCAL_ARRAY_LEN = 100;
var
  val_list : array[0..Pred(LOCAL_ARRAY_LEN)] of TSDOInteger;

  procedure PrepareArray();
  var
    k : Integer;
  begin
    Randomize();
    for k := 0 to Pred(LOCAL_ARRAY_LEN) do begin
      val_list[k] := Random(200000000);
      if ( ( k mod 5 ) = 0 ) then
        val_list[k] := - val_list[k];
    end;
  end;

var
  obj : ISDODataObject;
  ls, ls1 : ISDODataObjectList;
  i : Integer;
  crs : ILinkedListCursor;
  ok : Boolean;
begin
  PrepareArray();
  obj := Create_Object();

  ok := False;
  try
    ls := obj.getList(s_PROP_BOOL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  Check(ok,Format('"%s" is not a multivalue property.',[s_PROP_BOOL_1]));

  ls := obj.getList(LOCAL_PROP);
    Check(ls <> nil);
  ls1 := obj.getList(LOCAL_PROP);
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Name)');

  ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');
  ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');

  ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');
  ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');

  CheckEquals(0,ls.size(),'Size');
  for i := 0 to Pred(LOCAL_ARRAY_LEN) do begin
    ls.append(val_list[i]);
  end;

  crs := ls.getCursor();
  Check(crs <> nil,'ls.getCursor()');
  crs.Reset();
  CheckEquals(LOCAL_ARRAY_LEN,ls.size(),'Size');
  for i := 0 to Pred(LOCAL_ARRAY_LEN) do begin
    crs.MoveNext();
    CheckEquals(val_list[i],ls.getInteger(),'append() <> getInteger()');
  end;
end;

procedure TSDOBaseDataObject_Test.integer_unset_isset();
const
  LOCAL_PROP = s_PROP_INTEGER_1;
  LOCAL_PROP_ARRAY = s_PROP_INTEGER_3;
  VAL_1  = 1210;  VAL_2 = 76;
var
  obj : ISDODataObject;
  ls : ISDODataObjectList;
begin
  obj := Create_Object();

  CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 1');

  obj.setInteger(LOCAL_PROP, VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
    obj.unset(LOCAL_PROP);
      CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
      CheckEquals(obj.getProperty(LOCAL_PROP).getIntegerDefault(),obj.getInteger(LOCAL_PROP),'"unSet" should restore the default value');
    obj.setInteger(LOCAL_PROP, VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
      obj.unset(LOCAL_PROP);
        CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);

  ls := obj.getList(LOCAL_PROP_ARRAY);
  Check(ls <> nil);
  CheckEquals(0,ls.size());
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 2');
  ls.append(VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 3');
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 4');
      CheckEquals(0,ls.size());
    ls.append(VAL_1);
    ls.append(VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 5');
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 6');
      CheckEquals(2,ls.size());
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 7');
      CheckEquals(0,ls.size());
end;

procedure TSDOBaseDataObject_Test.boolean_unset_isset();
const
  LOCAL_PROP = s_PROP_BOOL_1;
  LOCAL_PROP_ARRAY = s_PROP_BOOL_3;
  VAL_1  = True;  VAL_2 = False;
var
  obj : ISDODataObject;
  ls : ISDODataObjectList;
begin
  obj := Create_Object();

  CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);

  obj.setBoolean(LOCAL_PROP, VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
    obj.unset(LOCAL_PROP);
      CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
    obj.setBoolean(LOCAL_PROP, VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
      obj.unset(LOCAL_PROP);
        CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);

  ls := obj.getList(LOCAL_PROP_ARRAY);
  Check(ls <> nil);
  CheckEquals(0,ls.size());
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);
  ls.append(VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);
      CheckEquals(0,ls.size());
    ls.append(VAL_1);
    ls.append(VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);
      CheckEquals(2,ls.size());
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);
      CheckEquals(0,ls.size());
end;

procedure TSDOBaseDataObject_Test.string_unset_isset();
const
  LOCAL_PROP = s_PROP_STR_1;
  LOCAL_PROP_ARRAY = s_PROP_STR_3;
  VAL_1  = 'wwssxx';  VAL_2 = '';
var
  obj : ISDODataObject;
  ls : ISDODataObjectList;
begin
  obj := Create_Object();

  CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);

  obj.setString(LOCAL_PROP, VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
    obj.unset(LOCAL_PROP);
      CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
    obj.setString(LOCAL_PROP, VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
      obj.unset(LOCAL_PROP);
        CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);

  ls := obj.getList(LOCAL_PROP_ARRAY);
  Check(ls <> nil);
  CheckEquals(0,ls.size());
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);
  ls.append(VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);
      CheckEquals(0,ls.size());
    ls.append(VAL_1);
    ls.append(VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);
      CheckEquals(2,ls.size());
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);
      CheckEquals(0,ls.size());
end;

procedure TSDOBaseDataObject_Test.object_unset_isset();
const
  LOCAL_PROP = s_PROP_OBJ_CONT;
  LOCAL_PROP_ARRAY = s_PROP_OBJ_CONT_LIST;
var
  obj, VAL_A, VAL_B : ISDODataObject;
  ls : ISDODataObjectList;
begin
  obj := Create_Object();

  CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP + ' 1');
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);

  VAL_A := obj.createDataObject(LOCAL_PROP);
  VAL_B := obj.createDataObject(LOCAL_PROP);

  obj.setDataObject(LOCAL_PROP, VAL_A);
    CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP + ' 4');
    obj.unset(LOCAL_PROP);
      CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP + ' 5');
    obj.setDataObject(LOCAL_PROP, VAL_B);
      CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP + ' 6');
      obj.unset(LOCAL_PROP);
        CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP + ' 7');

  ls := obj.getList(LOCAL_PROP_ARRAY);
  Check(ls <> nil);
  CheckEquals(0,ls.size());
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);
  ls.append(obj.createDataObject(LOCAL_PROP_ARRAY));
    CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);
      CheckEquals(0,ls.size());
    ls.append(obj.createDataObject(LOCAL_PROP_ARRAY));
    ls.append(obj.createDataObject(LOCAL_PROP_ARRAY));
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);
      CheckEquals(2,ls.size());
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);
      CheckEquals(0,ls.size());
end;

procedure TSDOBaseDataObject_Test.integer_setnull_isnull();
const
  LOCAL_PROP = s_PROP_INTEGER_1;
  LOCAL_PROP_ARRAY = s_PROP_INTEGER_3;
  VAL_1  = 1210;
var
  obj : ISDODataObject;
begin
  obj := Create_Object();

  // these _does_ depend on the property's default
  CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isNull(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);

  obj.setInteger(LOCAL_PROP, VAL_1);
    CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
    obj.setNull(LOCAL_PROP);
      CheckEquals(True,obj.isNull(LOCAL_PROP),LOCAL_PROP);
      CheckEquals(0,obj.getInteger(LOCAL_PROP),LOCAL_PROP);
end;

procedure TSDOBaseDataObject_Test.boolean_setnull_isnull();
const
  LOCAL_PROP = s_PROP_BOOL_1;
  LOCAL_PROP_ARRAY = s_PROP_BOOL_3;
  VAL_1  = True; VAL_2 = False;
var
  obj : ISDODataObject;
begin
  obj := Create_Object();

  // these _does_ depend on the property's default
  CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isNull(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);

  obj.setBoolean(LOCAL_PROP, VAL_1);
    CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
    obj.setNull(LOCAL_PROP);
      CheckEquals(True,obj.isNull(LOCAL_PROP),LOCAL_PROP);
      CheckEquals(False,obj.getBoolean(LOCAL_PROP),LOCAL_PROP);

  obj.setBoolean(LOCAL_PROP, VAL_2);
    CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
    obj.setNull(LOCAL_PROP);
      CheckEquals(True,obj.isNull(LOCAL_PROP),LOCAL_PROP);
      CheckEquals(False,obj.getBoolean(LOCAL_PROP),LOCAL_PROP);
end;

procedure TSDOBaseDataObject_Test.string_setnull_isnull();
const
  LOCAL_PROP = s_PROP_STR_1;
  LOCAL_PROP_ARRAY = s_PROP_STR_3;
  VAL_1  = 'wwssxx';  VAL_2 = '';
var
  obj : ISDODataObject;
begin
  obj := Create_Object();

  CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isNull(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);

  obj.setString(LOCAL_PROP, VAL_1);
    CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
    obj.setNull(LOCAL_PROP);
      CheckEquals(True,obj.isNull(LOCAL_PROP),LOCAL_PROP);
    obj.setString(LOCAL_PROP, VAL_2);
      CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
      obj.setNull(LOCAL_PROP);
        CheckEquals(True,obj.isNull(LOCAL_PROP),LOCAL_PROP);
end;

procedure TSDOBaseDataObject_Test.object_setDataObject_ref_nil();
var
  locA, locA1, locB : ISDODataObject;
begin
  locA := FFactory.createNew(s_URI_1,s_TYPE_1);
    locB := locA.createDataObject(s_PROP_OBJ_CONT);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locB));
    CheckEquals(PtrUInt(locB), PtrUInt(locA.getDataObject(s_PROP_OBJ_CONT)));
  locA1 := FFactory.createNew(s_URI_1,s_TYPE_1);
    locA1.setDataObject(s_PROP_OBJ_REF,locB);
    CheckEquals(PtrUInt(locB), PtrUInt(locA.getDataObject(s_PROP_OBJ_CONT)));
    CheckEquals(PtrUInt(locB), PtrUInt(locA1.getDataObject(s_PROP_OBJ_REF)));

    locA.setDataObject(s_PROP_OBJ_CONT,nil);
      CheckEquals(PtrUInt(nil), PtrUInt(locA.getDataObject(s_PROP_OBJ_CONT)));
      CheckEquals(PtrUInt(nil), PtrUInt(locA1.getDataObject(s_PROP_OBJ_REF)), 'This reference should be NIL because the object has been deleted by his container.');
end;

procedure TSDOBaseDataObject_Test.object_setDataObject_ref_unset();
var
  locA, locA1, locB : ISDODataObject;
begin
  locA := FFactory.createNew(s_URI_1,s_TYPE_1);
    locB := locA.createDataObject(s_PROP_OBJ_CONT);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locB));
    CheckEquals(PtrUInt(locB), PtrUInt(locA.getDataObject(s_PROP_OBJ_CONT)));
  locA1 := FFactory.createNew(s_URI_1,s_TYPE_1);
    locA1.setDataObject(s_PROP_OBJ_REF,locB);
    CheckEquals(PtrUInt(locB), PtrUInt(locA.getDataObject(s_PROP_OBJ_CONT)));
    CheckEquals(PtrUInt(locB), PtrUInt(locA1.getDataObject(s_PROP_OBJ_REF)));

    locA.unset(s_PROP_OBJ_CONT);
      CheckEquals(PtrUInt(nil), PtrUInt(locA.getDataObject(s_PROP_OBJ_CONT)));
      CheckEquals(PtrUInt(nil), PtrUInt(locA1.getDataObject(s_PROP_OBJ_REF)), 'This reference should be NIL because the object has been deleted by his container.');
end;

procedure TSDOBaseDataObject_Test.object_setDataObject_ref_setnull();
var
  locA, locA1, locB : ISDODataObject;
begin
  locA := FFactory.createNew(s_URI_1,s_TYPE_1);
    locB := locA.createDataObject(s_PROP_OBJ_CONT);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locB));
    CheckEquals(PtrUInt(locB), PtrUInt(locA.getDataObject(s_PROP_OBJ_CONT)));
  locA1 := FFactory.createNew(s_URI_1,s_TYPE_1);
    locA1.setDataObject(s_PROP_OBJ_REF,locB);
    CheckEquals(PtrUInt(locB), PtrUInt(locA.getDataObject(s_PROP_OBJ_CONT)));
    CheckEquals(PtrUInt(locB), PtrUInt(locA1.getDataObject(s_PROP_OBJ_REF)));

    locA.setNull(s_PROP_OBJ_CONT);
      CheckEquals(PtrUInt(nil), PtrUInt(locA.getDataObject(s_PROP_OBJ_CONT)));
      CheckEquals(PtrUInt(nil), PtrUInt(locA1.getDataObject(s_PROP_OBJ_REF)), 'This reference should be NIL because the object has been deleted by his container.');
end;

procedure TSDOBaseDataObject_Test.object_setDataObject_ref_nil_nested();
var
  locFac : ISDODataFactory;
  locA, locB, locC, locD : ISDODataObject;

  procedure InitAndCreateObject();
  begin
    locA := nil;
    locB := nil;
    locC := nil;
    locD := nil;
    locA := locFac.createNew(s_URI_1,'A');
      locB := locA.createDataObject('Pab');
        locC := locB.createDataObject('Pbc');
          locD := locC.createDataObject('Pcd');
      locA.setDataObject('Pad_ref',locD);
  end;

var
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
    locFac.AddType(s_URI_1,'A',tfg);
    locFac.AddType(s_URI_1,'B',tfg);
    locFac.AddType(s_URI_1,'C',tfg);
    locFac.AddType(s_URI_1,'D',tfg);
      locFac.addProperty(s_URI_1,'A','Pab',s_URI_1,'B',[pfIsContainment]);        //  <<-- Containment
        locFac.addProperty(s_URI_1,'B','Pbc',s_URI_1,'C',[pfIsContainment]);      //  <<-- Containment
          locFac.addProperty(s_URI_1,'C','Pcd',s_URI_1,'D',[pfIsContainment]);    //  <<-- Containment
      locFac.addProperty(s_URI_1,'A','Pad_ref',s_URI_1,'D',[]);       //  <<-- Reference

  InitAndCreateObject();
    locC.setDataObject('Pcd',nil);
    CheckEquals(PtrUInt(0), PtrUInt(locA.getDataObject('Pad_ref')));

  InitAndCreateObject();
    locB.setDataObject('Pbc',nil);
    CheckEquals(PtrUInt(0), PtrUInt(locA.getDataObject('Pad_ref')));

  InitAndCreateObject();
    locA.setDataObject('Pab',nil);
    CheckEquals(PtrUInt(0), PtrUInt(locA.getDataObject('Pad_ref')));
end;

procedure TSDOBaseDataObject_Test.object_setDataObject_ref_setnull_nested;
var
  locFac : ISDODataFactory;
  locA, locB, locC, locD : ISDODataObject;

  procedure InitAndCreateObject();
  begin
    locA := nil;
    locB := nil;
    locC := nil;
    locD := nil;
    locA := locFac.createNew(s_URI_1,'A');
      locB := locA.createDataObject('Pab');
        locC := locB.createDataObject('Pbc');
          locD := locC.createDataObject('Pcd');
      locA.setDataObject('Pad_ref',locD);
  end;

var
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
    locFac.AddType(s_URI_1,'A',tfg);
    locFac.AddType(s_URI_1,'B',tfg);
    locFac.AddType(s_URI_1,'C',tfg);
    locFac.AddType(s_URI_1,'D',tfg);
      locFac.addProperty(s_URI_1,'A','Pab',s_URI_1,'B',[pfIsContainment]);        //  <<-- Containment
        locFac.addProperty(s_URI_1,'B','Pbc',s_URI_1,'C',[pfIsContainment]);      //  <<-- Containment
          locFac.addProperty(s_URI_1,'C','Pcd',s_URI_1,'D',[pfIsContainment]);    //  <<-- Containment
      locFac.addProperty(s_URI_1,'A','Pad_ref',s_URI_1,'D',[]);       //  <<-- Reference

  InitAndCreateObject();
    locC.setNull('Pcd');
    CheckEquals(PtrUInt(0), PtrUInt(locA.getDataObject('Pad_ref')));

  InitAndCreateObject();
    locB.setNull('Pbc');
    CheckEquals(PtrUInt(0), PtrUInt(locA.getDataObject('Pad_ref')));

  InitAndCreateObject();
    locA.setNull('Pab');
    CheckEquals(PtrUInt(0), PtrUInt(locA.getDataObject('Pad_ref')));
end;

procedure TSDOBaseDataObject_Test.object_setDataObject_ref_unset_nested;
var
  locFac : ISDODataFactory;
  locA, locB, locC, locD : ISDODataObject;

  procedure InitAndCreateObject();
  begin
    locA := nil;
    locB := nil;
    locC := nil;
    locD := nil;
    locA := locFac.createNew(s_URI_1,'A');
      locB := locA.createDataObject('Pab');
        locC := locB.createDataObject('Pbc');
          locD := locC.createDataObject('Pcd');
      locA.setDataObject('Pad_ref',locD);
  end;

var
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
    locFac.AddType(s_URI_1,'A',tfg);
    locFac.AddType(s_URI_1,'B',tfg);
    locFac.AddType(s_URI_1,'C',tfg);
    locFac.AddType(s_URI_1,'D',tfg);
      locFac.addProperty(s_URI_1,'A','Pab',s_URI_1,'B',[pfIsContainment]);       //  <<-- Containment
        locFac.addProperty(s_URI_1,'B','Pbc',s_URI_1,'C',[pfIsContainment]);     //  <<-- Containment
          locFac.addProperty(s_URI_1,'C','Pcd',s_URI_1,'D',[pfIsContainment]);    //  <<-- Containment
      locFac.addProperty(s_URI_1,'A','Pad_ref',s_URI_1,'D',[]);       //  <<-- Reference

  InitAndCreateObject();
    locC.unset('Pcd');
    CheckEquals(PtrUInt(0), PtrUInt(locA.getDataObject('Pad_ref')));

  InitAndCreateObject();
    locB.unset('Pbc');
    CheckEquals(PtrUInt(0), PtrUInt(locA.getDataObject('Pad_ref')));

  InitAndCreateObject();
    locA.unset('Pab');
    CheckEquals(PtrUInt(0), PtrUInt(locA.getDataObject('Pad_ref')));
end;

procedure TSDOBaseDataObject_Test.object_setDataObject_cycle_containment();
var
  locFac : ISDODataFactory;
  locA, locB, locC : ISDODataObject;
  ok : Boolean;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_URI_1,'X',tfg);
    locFac.AddType(s_URI_1,'A',tfg);
      locFac.setBaseType(s_URI_1,'A',s_URI_1,'X');
    locFac.AddType(s_URI_1,'B',tfg);
    locFac.AddType(s_URI_1,'C',tfg);
      locFac.addProperty(s_URI_1,'A','p_ab',s_URI_1,'B',[pfIsContainment]);
      locFac.addProperty(s_URI_1,'B','p_bc',s_URI_1,'C',[pfIsContainment]);
      locFac.addProperty(s_URI_1,'C','p_ca',sdo_namespace,SDOTypeDefaultTypeNames[ObjectType],[pfIsContainment]);

  locA := locFac.createNew(s_URI_1,'A');
    locB := locA.createDataObject('p_ab');
      locC := locB.createDataObject('p_bc');
        ok := False;
        try
          locC.setDataObject('p_ca',locA);
        except
          on  e : ESDOCycleContainmentException do
            ok := True
        end;
        Check(ok,'Cycle Containment.');
end;

procedure TSDOBaseDataObject_Test.IsInstanceOf();
var
  locFac : ISDODataFactory;
  locO : ISDODataObjectEx;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_URI_1,'A',tfg);
    locFac.AddType(s_URI_1,'A_B_1',tfg);
      locFac.setBaseType(s_URI_1,'A_B_1',s_URI_1,'A');
    locFac.AddType(s_URI_1,'A_B_2',tfg);
      locFac.setBaseType(s_URI_1,'A_B_2',s_URI_1,'A');
      locFac.AddType(s_URI_1,'A_B_2_C',tfg);
        locFac.setBaseType(s_URI_1,'A_B_2_C',s_URI_1,'A_B_2');
        locFac.AddType(s_URI_1,'A_B_2_C_D',tfg);
          locFac.setBaseType(s_URI_1,'A_B_2_C_D',s_URI_1,'A_B_2_C');
  locFac.AddType(s_URI_1,'B',tfg);
  locFac.AddType(s_URI_1,'C',tfg);

  locO := locFac.createNew(s_URI_1,'A') as ISDODataObjectEx;
    CheckEquals(True, locO.IsInstanceOf(locFac.getType(s_URI_1,'A')));
    CheckEquals(False, locO.IsInstanceOf(locFac.getType(s_URI_1,'B')));
    CheckEquals(False, locO.IsInstanceOf(locFac.getType(s_URI_1,'A_B_1')));
    CheckEquals(False, locO.IsInstanceOf(locFac.getType(s_URI_1,'A_B_2')));
    CheckEquals(False, locO.IsInstanceOf(locFac.getType(s_URI_1,'A_B_2_C')));
    CheckEquals(False, locO.IsInstanceOf(locFac.getType(s_URI_1,'A_B_2_C_D')));

  locO := locFac.createNew(s_URI_1,'A_B_1') as ISDODataObjectEx;
    CheckEquals(True, locO.IsInstanceOf(locFac.getType(s_URI_1,'A')));
    CheckEquals(True, locO.IsInstanceOf(locFac.getType(s_URI_1,'A_B_1')));
    CheckEquals(False, locO.IsInstanceOf(locFac.getType(s_URI_1,'B')));
    CheckEquals(False, locO.IsInstanceOf(locFac.getType(s_URI_1,'A_B_2')));
    CheckEquals(False, locO.IsInstanceOf(locFac.getType(s_URI_1,'A_B_2_C')));
    CheckEquals(False, locO.IsInstanceOf(locFac.getType(s_URI_1,'A_B_2_C_D')));

  locO := locFac.createNew(s_URI_1,'A_B_2') as ISDODataObjectEx;
    CheckEquals(True, locO.IsInstanceOf(locFac.getType(s_URI_1,'A')));
    CheckEquals(True, locO.IsInstanceOf(locFac.getType(s_URI_1,'A_B_2')));
    CheckEquals(False, locO.IsInstanceOf(locFac.getType(s_URI_1,'B')));
    CheckEquals(False, locO.IsInstanceOf(locFac.getType(s_URI_1,'A_B_1')));
    CheckEquals(False, locO.IsInstanceOf(locFac.getType(s_URI_1,'A_B_2_C')));
    CheckEquals(False, locO.IsInstanceOf(locFac.getType(s_URI_1,'A_B_2_C_D')));

  locO := locFac.createNew(s_URI_1,'A_B_2_C') as ISDODataObjectEx;
    CheckEquals(True, locO.IsInstanceOf(locFac.getType(s_URI_1,'A')));
    CheckEquals(True, locO.IsInstanceOf(locFac.getType(s_URI_1,'A_B_2')));
    CheckEquals(True, locO.IsInstanceOf(locFac.getType(s_URI_1,'A_B_2_C')));
    CheckEquals(False, locO.IsInstanceOf(locFac.getType(s_URI_1,'B')));
    CheckEquals(False, locO.IsInstanceOf(locFac.getType(s_URI_1,'A_B_1')));
    CheckEquals(False, locO.IsInstanceOf(locFac.getType(s_URI_1,'A_B_2_C_D')));

  locO := locFac.createNew(s_URI_1,'A_B_2_C_D') as ISDODataObjectEx;
    CheckEquals(True, locO.IsInstanceOf(locFac.getType(s_URI_1,'A')));
    CheckEquals(True, locO.IsInstanceOf(locFac.getType(s_URI_1,'A_B_2')));
    CheckEquals(True, locO.IsInstanceOf(locFac.getType(s_URI_1,'A_B_2_C')));
    CheckEquals(True, locO.IsInstanceOf(locFac.getType(s_URI_1,'A_B_2_C_D')));
    CheckEquals(False, locO.IsInstanceOf(locFac.getType(s_URI_1,'B')));
    CheckEquals(False, locO.IsInstanceOf(locFac.getType(s_URI_1,'A_B_1')));

end;

procedure TSDOBaseDataObject_Test.object_setDataObject_ref();
const
  s_uri = 'u';
var
  locFac : ISDODataFactory;
  a, b : ISDODataObject;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',tfg);
  locFac.AddType(s_uri,'b',tfg);
  locFac.addProperty(s_uri,'a','p_ab',s_uri,'b',[]);

  a := locFac.createNew(s_uri,'a');
  b := locFac.createNew(s_uri,'b');
  a.setDataObject('p_ab',b);
end;

procedure TSDOBaseDataObject_Test.get_integer_xpath();
  function local_create_factory() : ISDODataFactory;
  var
    res : ISDODataFactory;
    tfg : TTypeFlags;
  begin
    tfg := [];
    if is_open_type() then
      Include(tfg,tfIsOpen);
    res := TSDODataFactory.Create() as ISDODataFactory;
    res.AddType(s_URI_1,s_TYPE_1,tfg);
    res.AddType(s_URI_1,s_TYPE_2,tfg);
    res.AddType(s_URI_1,s_TYPE_3,tfg);

    res.addProperty(s_URI_1,s_TYPE_1,s_PROP_INTEGER_1,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
    res.addProperty(s_URI_1,s_TYPE_1,s_PROP_INTEGER_2,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
    res.addProperty(s_URI_1,s_TYPE_1,s_PROP_OBJ_CONT,s_URI_1,s_TYPE_2,[pfIsContainment]);

    res.addProperty(s_URI_1,s_TYPE_2,s_PROP_INTEGER_A,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
    res.addProperty(s_URI_1,s_TYPE_2,s_PROP_INTEGER_B,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);

    Result := res;
  end;

var
  locFac : ISDODataFactory;
  o1, o2 : ISDODataObject;
  o1_val_1, o1_val_2, o2_val_A, o2_val_B : TSDOInteger;
begin
  o1_val_1 := RandomRange(-12345,12345);
  o1_val_2 := RandomRange(-12345,12345);
  o2_val_A := RandomRange(-12345,12345);
  o2_val_B := RandomRange(-12345,12345);
  locFac := local_create_factory();
  o1 := locFac.createNew(s_URI_1,s_TYPE_1);
  o1.setInteger(s_PROP_INTEGER_1,o1_val_1);
  o1.setInteger(s_PROP_INTEGER_2,o1_val_2);
    o2 := O1.createDataObject(s_PROP_OBJ_CONT);
      o2.setInteger(s_PROP_INTEGER_A,o2_val_A);
      o2.setInteger(s_PROP_INTEGER_B,o2_val_B);

  check_xpath_value(o1_val_1, o1,s_PROP_INTEGER_1);
  check_xpath_value(o1_val_2, o1, s_PROP_INTEGER_2);
  check_xpath_value(o2_val_A, o1, Format('%s/%s',[s_PROP_OBJ_CONT,s_PROP_INTEGER_A]));
  check_xpath_value(o2_val_B, o1, Format('%s/%s',[s_PROP_OBJ_CONT,s_PROP_INTEGER_B]));
  check_xpath_value(o1_val_1, o2, Format('%s/%s',['..',s_PROP_INTEGER_1]));
  check_xpath_value(o1_val_2, o2, Format('%s/%s',['..',s_PROP_INTEGER_2]));
end;

procedure TSDOBaseDataObject_Test.check_xpath_value(
  const AExpected: TSDOInteger;
  const AObject: ISDODataObject;
  const AXPath: string
);
begin
  CheckEquals(AExpected,AObject.getInteger(AXPath), AXPath);
end;

procedure TSDOBaseDataObject_Test.property_default_value_integer();
const
  VAL_1 : TSDOInteger = 1210;
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_INTEGER_1,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_INTEGER_1);
        locProp.setDefault(VAL_1);

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  CheckEquals(VAL_1, locA.getInteger(locProp), 'getInteger');
  CheckEquals(False, locA.isSet(locProp), 'isSet');
end;

procedure TSDOBaseDataObject_Test.property_default_value_unset_integer();
const
  VAL_1 = 1210; VAL_2 = 789;
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_INTEGER_1,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_INTEGER_1);
        locProp.setDefault(TSDOInteger(VAL_1));

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  locA.setInteger(locProp, VAL_2);
    CheckEquals(True, locA.isSet(locProp), 'isSet');
    CheckEquals(VAL_2, locA.getInteger(locProp), 'getInteger');
    locA.unset(locProp);
      CheckEquals(False, locA.isSet(locProp), 'isSet');
      CheckEquals(VAL_1, locA.getInteger(locProp), 'getInteger');
end;

procedure TSDOBaseDataObject_Test.property_default_value_bool();
const
  VAL_1 = True;
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_BOOL_1,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_BOOL_1);
        locProp.setDefault(VAL_1);

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  CheckEquals(VAL_1, locA.getBoolean(locProp), 'getBoolean');
  CheckEquals(False, locA.isSet(locProp), 'isSet');
end;

procedure TSDOBaseDataObject_Test.property_default_value_unset_bool();
const
  VAL_1 = True; VAL_2 = False;
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_BOOL_1,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_BOOL_1);
        locProp.setDefault(VAL_1);

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  locA.setBoolean(locProp, VAL_2);
    CheckEquals(True, locA.isSet(locProp), 'isSet');
    CheckEquals(VAL_2, locA.getBoolean(locProp), 'getBoolean');
    locA.unset(locProp);
      CheckEquals(False, locA.isSet(locProp), 'isSet');
      CheckEquals(VAL_1, locA.getBoolean(locProp), 'getBoolean');
end;

procedure TSDOBaseDataObject_Test.property_default_value_string();
const
  VAL_1 = 'Kiswendsida O.';
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_STR_1,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_STR_1);
        locProp.setDefault(VAL_1);

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  CheckEquals(VAL_1, locA.getString(locProp), 'getString');
  CheckEquals(False, locA.isSet(locProp), 'isSet');
end;

procedure TSDOBaseDataObject_Test.property_default_value_unset_string;
const
  VAL_1 = 'O. A. Kiswensida'; VAL_2 = 'sssszzeerrff';
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_STR_1,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_STR_1);
        locProp.setDefault(VAL_1);

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  locA.setString(locProp, VAL_2);
    CheckEquals(True, locA.isSet(locProp), 'isSet');
    CheckEquals(VAL_2, locA.getString(locProp), 'getString');
    locA.unset(locProp);
      CheckEquals(False, locA.isSet(locProp), 'isSet');
      CheckEquals(VAL_1, locA.getString(locProp), 'getString');
end;

procedure TSDOBaseDataObject_Test.object_ref_multivalue();
const LOCAL_PROP = s_PROP_OBJ_REF_LIST;
var
  val_list : array of ISDODataObject;
  obj : ISDODataObject;
  ls, ls1 : ISDODataObjectList;
  local_array_len, i : PtrInt;
  crs : ILinkedListCursor;
  ok : Boolean;
  xtraObj : ISDODataObject;
begin
  Randomize();
  local_array_len := RandomRange(0,100);
  SetLength(val_list,local_array_len);
  try
    for i := 0 to Pred(local_array_len) do
      val_list[i] := FFactory.createNew(s_URI_1,s_TYPE_2);
    obj := Create_Object();

    ok := False;
    try
      ls := obj.getList(s_PROP_BOOL_1);
    except
      on e : ESDOIllegalArgumentException do begin
        ok := True;
      end;
    end;
    Check(ok,Format('"%s" is not a multivalue property.',[s_PROP_BOOL_1]));

    ls := obj.getList(LOCAL_PROP);
      Check(ls <> nil);
    ls1 := obj.getList(LOCAL_PROP);
      CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Name)');

    ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
      CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');
    ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
      CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');

    ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
      CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');
    ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
      CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');

    CheckEquals(0,ls.size(),'Size');
    for i := 0 to Pred(local_array_len) do begin
      ls.append(val_list[i]);
    end;

    crs := ls.getCursor();
    Check(crs <> nil,'ls.getCursor()');
    crs.Reset();
    CheckEquals(local_array_len,ls.size(),'Size');
    for i := 0 to Pred(local_array_len) do begin
      crs.MoveNext();
      CheckEquals(PtrUInt(val_list[i]),PtrUInt(ls.getDataObject()),Format('append(%d) <> getDataObject(%d)',[i,i]));
      CheckEquals(PtrUInt(nil),PtrUInt(ls.getDataObject().getContainer()),Format('nil <> getDataObject(%d).getContainer()',[i]));
    end;

    xtraObj := FFactory.createNew(s_URI_3,s_TYPE_3);
    ok := False;
    try
      ls.append(xtraObj);
    except
      on e : ESDOIllegalArgumentException do
        ok := True;
    end;
    Check(ok, 'Adding an different object type.');
  finally
    SetLength(val_list,0);
  end;
end;

procedure TSDOBaseDataObject_Test.object_cont_multivalue();
const LOCAL_PROP = s_PROP_OBJ_CONT_LIST;
var
  val_list : array of ISDODataObject;
  obj : ISDODataObject;
  ls, ls1 : ISDODataObjectList;
  local_array_len, i : PtrInt;
  crs : ILinkedListCursor;
  ok : Boolean;
  xtraObj : ISDODataObject;
begin
  Randomize();
  local_array_len := RandomRange(0,100);
  SetLength(val_list,local_array_len);
  try
    for i := 0 to Pred(local_array_len) do
      val_list[i] := FFactory.createNew(s_URI_1,s_TYPE_2);
    obj := Create_Object();

    ok := False;
    try
      ls := obj.getList(s_PROP_BOOL_1);
    except
      on e : ESDOIllegalArgumentException do begin
        ok := True;
      end;
    end;
    Check(ok,Format('"%s" is not a multivalue property.',[s_PROP_BOOL_1]));

    ls := obj.getList(LOCAL_PROP);
      Check(ls <> nil);
    ls1 := obj.getList(LOCAL_PROP);
      CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Name)');

    ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
      CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');
    ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
      CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');

    ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
      CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');
    ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
      CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');

    CheckEquals(0,ls.size(),'Size');
    for i := 0 to Pred(local_array_len) do begin
      ls.append(val_list[i]);
    end;

    crs := ls.getCursor();
    Check(crs <> nil,'ls.getCursor()');
    crs.Reset();
    CheckEquals(local_array_len,ls.size(),'Size');
    for i := 0 to Pred(local_array_len) do begin
      crs.MoveNext();
      CheckEquals(PtrUInt(val_list[i]),PtrUInt(ls.getDataObject()),Format('append(%d) <> getDataObject(%d)',[i,i]));
      CheckEquals(PtrUInt(obj),PtrUInt(ls.getDataObject().getContainer()),Format('obj <> getDataObject(%d).getContainer()',[i]));
    end;

    xtraObj := FFactory.createNew(s_URI_3,s_TYPE_3);
    ok := False;
    try
      ls.append(xtraObj);
    except
      on e : ESDOIllegalArgumentException do
        ok := True;
    end;
    Check(ok, 'Adding an different object type.');
  finally
    SetLength(val_list,0);
  end;
end;

procedure TSDOBaseDataObject_Test.object_destroy_order();
var
  locFac : ISDODataFactory;
  locA, locB : ISDODataObject;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_URI_1,'a',[]);
  locFac.AddType(s_URI_1,'b',[]);
    locFac.addProperty(s_URI_1,'a','p_ab',s_URI_1,'b',[]);

  locA := locFac.createNew(s_URI_1,'a');
  locB := locFac.createNew(s_URI_1,'b');
  locA.setDataObject('p_ab', locB);
  locA := nil;
  locB := nil;
end;

procedure TSDOBaseDataObject_Test.object_getChangeSummary();
var
  locFac : ISDODataFactory;
  locA, locB_Ref, locB_Cont : ISDODataObject;
  locCS : ISDOChangeSummary;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_URI_1,'a',[]);
  locFac.AddType(s_URI_1,'b',[]);
    locFac.addProperty(s_URI_1,'a','p_ab_ref',s_URI_1,'b',[]);
    locFac.addProperty(s_URI_1,'a','p_ab_cont',s_URI_1,'b',[pfIsContainment]);
    locFac.addProperty(s_URI_1,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

  locA := locFac.createNew(s_URI_1,'a');
  locB_Cont := locFac.createNew(s_URI_1,'b');
  locB_Ref := locFac.createNew(s_URI_1,'b');
  locCS := locA.getChangeSummary();

  CheckNotEquals(PtrUInt(nil), PtrUInt(locCS));
  CheckEquals(PtrUInt(nil), PtrUInt(locB_Cont.getChangeSummary()));
  CheckEquals(PtrUInt(nil), PtrUInt(locB_Ref.getChangeSummary()));

  locA.setDataObject('p_ab_cont', locB_Cont);
    CheckEquals(PtrUInt(locCS), PtrUInt(locB_Cont.getChangeSummary()));
  locA.setDataObject('p_ab_cont', nil);
    CheckEquals(PtrUInt(nil), PtrUInt(locB_Cont.getChangeSummary()));

  locA.setDataObject('p_ab_ref', locB_Ref);
    CheckEquals(PtrUInt(nil), PtrUInt(locB_Cont.getChangeSummary()));
  locA.setDataObject('p_ab_ref', nil);
    CheckEquals(PtrUInt(nil), PtrUInt(locB_Ref.getChangeSummary()));
end;

procedure TSDOBaseDataObject_Test.object_getChangeSummary_multiprop();
var
  locFac : ISDODataFactory;
  locA, locB_Ref, locB_Cont : ISDODataObject;
  locCS : ISDOChangeSummary;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_URI_1,'a',[]);
  locFac.AddType(s_URI_1,'b',[]);
    locFac.addProperty(s_URI_1,'a','p_ab_ref',s_URI_1,'b',[pfIsMany]);
    locFac.addProperty(s_URI_1,'a','p_ab_cont',s_URI_1,'b',[pfIsContainment,pfIsMany]);
    locFac.addProperty(s_URI_1,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

  locA := locFac.createNew(s_URI_1,'a');
  locB_Cont := locFac.createNew(s_URI_1,'b');
  locB_Ref := locFac.createNew(s_URI_1,'b');
  locCS := locA.getChangeSummary();

  CheckNotEquals(PtrUInt(nil), PtrUInt(locCS));
  CheckEquals(PtrUInt(nil), PtrUInt(locB_Cont.getChangeSummary()));
  CheckEquals(PtrUInt(nil), PtrUInt(locB_Ref.getChangeSummary()));

  locA.getList('p_ab_cont').append(locB_Cont);
    CheckEquals(PtrUInt(locCS), PtrUInt(locB_Cont.getChangeSummary()));
  locA.getList('p_ab_cont').delete(0);
    CheckEquals(PtrUInt(nil), PtrUInt(locB_Cont.getChangeSummary()));

  locA.getList('p_ab_ref').append(locB_Ref);
    CheckEquals(PtrUInt(nil), PtrUInt(locB_Cont.getChangeSummary()));
  locA.getList('p_ab_ref').delete(0);
    CheckEquals(PtrUInt(nil), PtrUInt(locB_Ref.getChangeSummary()));
end;

procedure TSDOBaseDataObject_Test.byte_procs();
const VAL_1 : TSDOByte = 12; VAL_2 : TSDOByte = 56; VAL_3 : TSDOByte = 0;
var
  obj : ISDODataObject;
  ok : Boolean;
begin
  obj := Create_Object();

  ok := False;
  try
    obj.setByte(obj.getProperty('qsdc'),VAL_1);
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.getByte(obj.getProperty('qsdc'));
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.setByte(obj.getProperty(s_PROP_BYTE_3),VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOIllegalArgumentException.');

  obj.setByte(obj.getProperty(s_PROP_BYTE_1),VAL_1);
  CheckEquals(VAL_1,obj.getByte(obj.getProperty(s_PROP_BYTE_1)));
  CheckEquals(VAL_1,obj.getByte(s_PROP_BYTE_1));
  CheckEquals(VAL_1,obj.getByte(obj.getPropertyIndex(obj.getProperty(s_PROP_BYTE_1))));

  obj.setByte(obj.getProperty(s_PROP_BYTE_1),VAL_2);
  CheckEquals(VAL_2,obj.getByte(obj.getProperty(s_PROP_BYTE_1)));
  CheckEquals(VAL_2,obj.getByte(s_PROP_BYTE_1));
  CheckEquals(VAL_2,obj.getByte(obj.getPropertyIndex(obj.getProperty(s_PROP_BYTE_1))));

  obj.setByte(obj.getProperty(s_PROP_BYTE_1),VAL_3);
  CheckEquals(VAL_3,obj.getByte(obj.getProperty(s_PROP_BYTE_1)));
  CheckEquals(VAL_3,obj.getByte(s_PROP_BYTE_1));
  CheckEquals(VAL_3,obj.getByte(obj.getPropertyIndex(obj.getProperty(s_PROP_BYTE_1))));


  obj.setByte(obj.getProperty(s_PROP_BYTE_2),VAL_1);
  CheckEquals(VAL_1,obj.getByte(obj.getProperty(s_PROP_BYTE_2)));
  CheckEquals(VAL_1,obj.getByte(s_PROP_BYTE_2));
  CheckEquals(VAL_1,obj.getByte(obj.getPropertyIndex(obj.getProperty(s_PROP_BYTE_2))));

  obj.setByte(obj.getProperty(s_PROP_BYTE_2),VAL_2);
  CheckEquals(VAL_2,obj.getByte(obj.getProperty(s_PROP_BYTE_2)));
  CheckEquals(VAL_2,obj.getByte(s_PROP_BYTE_2));
  CheckEquals(VAL_2,obj.getByte(obj.getPropertyIndex(obj.getProperty(s_PROP_BYTE_2))));

  obj.setByte(obj.getProperty(s_PROP_BYTE_2),VAL_3);
  CheckEquals(VAL_3,obj.getByte(obj.getProperty(s_PROP_BYTE_2)));
  CheckEquals(VAL_3,obj.getByte(s_PROP_BYTE_2));
  CheckEquals(VAL_3,obj.getByte(obj.getPropertyIndex(obj.getProperty(s_PROP_BYTE_2))));
end;

procedure TSDOBaseDataObject_Test.byte_multivalue();
const LOCAL_PROP = s_PROP_BYTE_3; LOCAL_ARRAY_LEN = 100;
var
  val_list : array[0..Pred(LOCAL_ARRAY_LEN)] of TSDOByte;

  procedure PrepareArray();
  var
    k : Integer;
  begin
    Randomize();
    for k := 0 to Pred(LOCAL_ARRAY_LEN) do begin
      val_list[k] := Random(High(TSDOByte));
    end;
  end;

var
  obj : ISDODataObject;
  ls, ls1 : ISDODataObjectList;
  i : Integer;
  crs : ILinkedListCursor;
  ok : Boolean;
begin
  PrepareArray();
  obj := Create_Object();

  ok := False;
  try
    ls := obj.getList(s_PROP_BOOL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  Check(ok,Format('"%s" is not a multivalue property.',[s_PROP_BOOL_1]));

  ls := obj.getList(LOCAL_PROP);
    Check(ls <> nil);
  ls1 := obj.getList(LOCAL_PROP);
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Name)');

  ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');
  ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');

  ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');
  ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');

  CheckEquals(0,ls.size(),'Size');
  for i := 0 to Pred(LOCAL_ARRAY_LEN) do begin
    ls.append(val_list[i]);
  end;

  crs := ls.getCursor();
  Check(crs <> nil,'ls.getCursor()');
  crs.Reset();
  CheckEquals(LOCAL_ARRAY_LEN,ls.size(),'Size');
  for i := 0 to Pred(LOCAL_ARRAY_LEN) do begin
    crs.MoveNext();
    CheckEquals(val_list[i],ls.getByte(),'append() <> getByte()');
  end;
end;

procedure TSDOBaseDataObject_Test.byte_unset_isset();
const
  LOCAL_PROP = s_PROP_BYTE_1;
  LOCAL_PROP_ARRAY = s_PROP_BYTE_3;
  VAL_1 : TSDOByte = 12;  VAL_2 : TSDOByte = 76;
var
  obj : ISDODataObject;
  ls : ISDODataObjectList;
begin
  obj := Create_Object();

  CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 1');

  obj.setByte(LOCAL_PROP, VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
    obj.unset(LOCAL_PROP);
      CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
      CheckEquals(obj.getProperty(LOCAL_PROP).getByteDefault(),obj.getByte(LOCAL_PROP),'"unSet" should restore the default value');
    obj.setByte(LOCAL_PROP, VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
      obj.unset(LOCAL_PROP);
        CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);

  ls := obj.getList(LOCAL_PROP_ARRAY);
  Check(ls <> nil);
  CheckEquals(0,ls.size());
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 2');
  ls.append(VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 3');
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 4');
      CheckEquals(0,ls.size());
    ls.append(VAL_1);
    ls.append(VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 5');
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 6');
      CheckEquals(2,ls.size());
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 7');
      CheckEquals(0,ls.size());
end;

procedure TSDOBaseDataObject_Test.byte_setnull_isnull();
const
  LOCAL_PROP = s_PROP_BYTE_1;
  LOCAL_PROP_ARRAY = s_PROP_BYTE_3;
  VAL_1 : TSDOByte = 12;
var
  obj : ISDODataObject;
begin
  obj := Create_Object();

  // these _does_ depend on the property's default
  CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isNull(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);

  obj.setByte(LOCAL_PROP, VAL_1);
    CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
    obj.setNull(LOCAL_PROP);
      CheckEquals(True,obj.isNull(LOCAL_PROP),LOCAL_PROP);
      CheckEquals(0,obj.getByte(LOCAL_PROP),LOCAL_PROP);
end;

procedure TSDOBaseDataObject_Test.property_default_value_byte();
const
  VAL_1 : TSDOByte = 12;
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_BYTE_1,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_BYTE_1);
        locProp.setDefault(VAL_1);

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  CheckEquals(VAL_1, locA.getByte(locProp), 'getByte');
  CheckEquals(False, locA.isSet(locProp), 'isSet');
end;

procedure TSDOBaseDataObject_Test.property_default_value_unset_byte();
const
  VAL_1 : TSDOByte = 12; VAL_2 : TSDOByte = 89;
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_BYTE_1,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_BYTE_1);
        locProp.setDefault(TSDOByte(VAL_1));

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  locA.setByte(locProp, VAL_2);
    CheckEquals(True, locA.isSet(locProp), 'isSet');
    CheckEquals(VAL_2, locA.getByte(locProp), 'getByte');
    locA.unset(locProp);
      CheckEquals(False, locA.isSet(locProp), 'isSet');
      CheckEquals(VAL_1, locA.getByte(locProp), 'getByte');
end;

{$IFDEF HAS_SDO_BYTES}
procedure TSDOBaseDataObject_Test.bytes_procs();
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
  obj : ISDODataObject;
  ok : Boolean;
begin
  SetConstants();
  obj := Create_Object();

  ok := False;
  try
    obj.setBytes(obj.getProperty('qsdc'),VAL_1);
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.getBytes(obj.getProperty('qsdc'));
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.setBytes(obj.getProperty(s_PROP_BYTES_3),VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOIllegalArgumentException.');

  obj.setBytes(obj.getProperty(s_PROP_BYTES_1),VAL_1);
  CheckEquals(VAL_1,obj.getBytes(obj.getProperty(s_PROP_BYTES_1)));
  CheckEquals(VAL_1,obj.getBytes(s_PROP_BYTES_1));
  CheckEquals(VAL_1,obj.getBytes(obj.getPropertyIndex(obj.getProperty(s_PROP_BYTES_1))));

  obj.setBytes(obj.getProperty(s_PROP_BYTES_1),VAL_2);
  CheckEquals(VAL_2,obj.getBytes(obj.getProperty(s_PROP_BYTES_1)));
  CheckEquals(VAL_2,obj.getBytes(s_PROP_BYTES_1));
  CheckEquals(VAL_2,obj.getBytes(obj.getPropertyIndex(obj.getProperty(s_PROP_BYTES_1))));

  obj.setBytes(obj.getProperty(s_PROP_BYTES_1),VAL_3);
  CheckEquals(VAL_3,obj.getBytes(obj.getProperty(s_PROP_BYTES_1)));
  CheckEquals(VAL_3,obj.getBytes(s_PROP_BYTES_1));
  CheckEquals(VAL_3,obj.getBytes(obj.getPropertyIndex(obj.getProperty(s_PROP_BYTES_1))));


  obj.setBytes(obj.getProperty(s_PROP_BYTES_2),VAL_1);
  CheckEquals(VAL_1,obj.getBytes(obj.getProperty(s_PROP_BYTES_2)));
  CheckEquals(VAL_1,obj.getBytes(s_PROP_BYTES_2));
  CheckEquals(VAL_1,obj.getBytes(obj.getPropertyIndex(obj.getProperty(s_PROP_BYTES_2))));

  obj.setBytes(obj.getProperty(s_PROP_BYTES_2),VAL_2);
  CheckEquals(VAL_2,obj.getBytes(obj.getProperty(s_PROP_BYTES_2)));
  CheckEquals(VAL_2,obj.getBytes(s_PROP_BYTES_2));
  CheckEquals(VAL_2,obj.getBytes(obj.getPropertyIndex(obj.getProperty(s_PROP_BYTES_2))));

  obj.setBytes(obj.getProperty(s_PROP_BYTES_2),VAL_3);
  CheckEquals(VAL_3,obj.getBytes(obj.getProperty(s_PROP_BYTES_2)));
  CheckEquals(VAL_3,obj.getBytes(s_PROP_BYTES_2));
  CheckEquals(VAL_3,obj.getBytes(obj.getPropertyIndex(obj.getProperty(s_PROP_BYTES_2))));
end;

procedure TSDOBaseDataObject_Test.bytes_multivalue();
const LOCAL_PROP = s_PROP_BYTES_3; LOCAL_ARRAY_LEN = 100;
var
  val_list : array[0..Pred(LOCAL_ARRAY_LEN)] of TSDOBytes;

  procedure PrepareArray();
  var
    k, h : Integer;
    e : TSDOBytes;
  begin
    Randomize();
    for k := 0 to Pred(LOCAL_ARRAY_LEN) do begin
      e := nil;
      SetLength(e,RandomRange(0,1000));
      if ( Length(e) > 0 ) then begin
        for h := Low(e) to High(e) do
          e[h] := RandomRange(Low(Byte),High(Byte));
      end;
      val_list[k] := e;
    end;
  end;

var
  obj : ISDODataObject;
  ls, ls1 : ISDODataObjectList;
  i : Integer;
  crs : ILinkedListCursor;
  ok : Boolean;
begin
  PrepareArray();
  obj := Create_Object();

  ok := False;
  try
    ls := obj.getList(s_PROP_BOOL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  Check(ok,Format('"%s" is not a multivalue property.',[s_PROP_BOOL_1]));

  ls := obj.getList(LOCAL_PROP);
    Check(ls <> nil);
  ls1 := obj.getList(LOCAL_PROP);
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Name)');

  ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');
  ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');

  ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');
  ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');

  CheckEquals(0,ls.size(),'Size');
  for i := 0 to Pred(LOCAL_ARRAY_LEN) do begin
    ls.appendBytes(val_list[i]);
  end;

  crs := ls.getCursor();
  Check(crs <> nil,'ls.getCursor()');
  crs.Reset();
  CheckEquals(LOCAL_ARRAY_LEN,ls.size(),'Size');
  for i := 0 to Pred(LOCAL_ARRAY_LEN) do begin
    crs.MoveNext();
    CheckEquals(val_list[i],ls.getBytes(),'append() <> getBytes()');
  end;
end;

procedure TSDOBaseDataObject_Test.bytes_unset_isset();
const
  LOCAL_PROP = s_PROP_BYTES_1;
  LOCAL_PROP_ARRAY = s_PROP_BYTES_3;
var
  VAL_1, VAL_2 : TSDOBytes;

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

    SetLength(v,200);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
    VAL_2 := v;     
  end;  
  
var
  obj : ISDODataObject;
  ls : ISDODataObjectList;
begin
  SetConstants();
  obj := Create_Object();

  CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 1');

  obj.setBytes(LOCAL_PROP, VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
    obj.unset(LOCAL_PROP);
      CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
      CheckEquals(obj.getProperty(LOCAL_PROP).getBytesDefault(),obj.getBytes(LOCAL_PROP),'"unSet" should restore the default value');
    obj.setBytes(LOCAL_PROP, VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
      obj.unset(LOCAL_PROP);
        CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);

  ls := obj.getList(LOCAL_PROP_ARRAY);
  Check(ls <> nil);
  CheckEquals(0,ls.size());
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 2');
  ls.appendBytes(VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 3');
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 4');
      CheckEquals(0,ls.size());
    ls.appendBytes(VAL_1);
    ls.appendBytes(VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 5');
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 6');
      CheckEquals(2,ls.size());
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 7');
      CheckEquals(0,ls.size());
end;

procedure TSDOBaseDataObject_Test.bytes_setnull_isnull();
const
  LOCAL_PROP = s_PROP_BYTES_1;
  LOCAL_PROP_ARRAY = s_PROP_BYTES_3;  
var
  VAL_1 : TSDOBytes;

  procedure SetConstants();
  var
    v : TSDOBytes;
    k : Integer;
  begin
    SetLength(v,100);
    for k := 0 to High(v) do
      v[k] := k mod High(Byte);
    VAL_1 := v;
  end;  
  
var
  obj : ISDODataObject;
begin
  SetConstants();
  obj := Create_Object();

  // these _does_ depend on the property's default
  CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isNull(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);

  obj.setBytes(LOCAL_PROP, VAL_1);
    CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
    obj.setNull(LOCAL_PROP);
      CheckEquals(True,obj.isNull(LOCAL_PROP),LOCAL_PROP);
      CheckEquals(TSDOBytes(nil),obj.getBytes(LOCAL_PROP),LOCAL_PROP);
end;

procedure TSDOBaseDataObject_Test.property_default_value_bytes();
var
  VAL_1 : TSDOBytes;

  procedure SetConstants();
  var
    v : TSDOBytes;
    k : Integer;
  begin
    SetLength(v,100);
    for k := 0 to High(v) do
      v[k] := k mod High(Byte);
    VAL_1 := v;
  end;  
  
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  SetConstants();
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_BYTES_1,sdo_namespace,SDOTypeDefaultTypeNames[BytesType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_BYTES_1);
        locProp.setDefault(VAL_1);

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  CheckEquals(VAL_1, locA.getBytes(locProp), 'getBytes');
  CheckEquals(False, locA.isSet(locProp), 'isSet');
end;

procedure TSDOBaseDataObject_Test.property_default_value_unset_bytes();
var
  VAL_1, VAL_2 : TSDOBytes;

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

    SetLength(v,200);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
    VAL_2 := v;     
  end; 
  
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  SetConstants();
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_BYTES_1,sdo_namespace,SDOTypeDefaultTypeNames[BytesType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_BYTES_1);
        locProp.setDefault(TSDOBytes(VAL_1));

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  locA.setBytes(locProp, VAL_2);
    CheckEquals(True, locA.isSet(locProp), 'isSet');
    CheckEquals(VAL_2, locA.getBytes(locProp), 'getBytes');
    locA.unset(locProp);
      CheckEquals(False, locA.isSet(locProp), 'isSet');
      CheckEquals(VAL_1, locA.getBytes(locProp), 'getBytes');
end;

procedure TSDOBaseDataObject_Test.get_bytes_xpath();
const
  LOCAL_PROP_TYPE = BytesType;
  LOCAL_PROP_1_NAME = s_PROP_BYTES_1;
  LOCAL_PROP_2_NAME = s_PROP_BYTES_2;
  LOCAL_PROP_A_NAME = s_PROP_BYTES_A;
  LOCAL_PROP_B_NAME = s_PROP_BYTES_B;

  function local_create_factory() : ISDODataFactory;
  var
    res : ISDODataFactory;
    tfg : TTypeFlags;
  begin
    tfg := [];
    if is_open_type() then
      Include(tfg,tfIsOpen);
    res := TSDODataFactory.Create() as ISDODataFactory;
    res.AddType(s_URI_1,s_TYPE_1,tfg);
    res.AddType(s_URI_1,s_TYPE_2,tfg);
    res.AddType(s_URI_1,s_TYPE_3,tfg);

    res.addProperty(s_URI_1,s_TYPE_1,LOCAL_PROP_1_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_1,LOCAL_PROP_2_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_1,s_PROP_OBJ_CONT,s_URI_1,s_TYPE_2,[pfIsContainment]);

    res.addProperty(s_URI_1,s_TYPE_2,LOCAL_PROP_A_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_2,LOCAL_PROP_B_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);

    Result := res;
  end;

var
  o1_val_1, o1_val_2, o2_val_A, o2_val_B : TSDOBytes;

  procedure SetConstants();
  var
    v : TSDOBytes;
    k : Integer;
  begin
    SetLength(v,100);
    for k := 0 to High(v) do
      v[k] := k mod High(Byte);
    o1_val_1 := v;
    v := nil;

    SetLength(v,200);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
    o1_val_2 := v;
    
    o2_val_A := nil;

    SetLength(v,200);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
    o2_val_B := v;
  end;

var
  locFac : ISDODataFactory;
  o1, o2 : ISDODataObject;
begin
  SetConstants();
  locFac := local_create_factory();
  o1 := locFac.createNew(s_URI_1,s_TYPE_1);
  o1.setBytes(LOCAL_PROP_1_NAME,o1_val_1);
  o1.setBytes(LOCAL_PROP_2_NAME,o1_val_2);
    o2 := O1.createDataObject(s_PROP_OBJ_CONT);
      o2.setBytes(LOCAL_PROP_A_NAME,o2_val_A);
      o2.setBytes(LOCAL_PROP_B_NAME,o2_val_B);

  check_xpath_value(o1_val_1, o1,LOCAL_PROP_1_NAME);
  check_xpath_value(o1_val_2, o1, LOCAL_PROP_2_NAME);
  check_xpath_value(o2_val_A, o1, Format('%s/%s',[s_PROP_OBJ_CONT,LOCAL_PROP_A_NAME]));
  check_xpath_value(o2_val_B, o1, Format('%s/%s',[s_PROP_OBJ_CONT,LOCAL_PROP_B_NAME]));
  check_xpath_value(o1_val_1, o2, Format('%s/%s',['..',LOCAL_PROP_1_NAME]));
  check_xpath_value(o1_val_2, o2, Format('%s/%s',['..',LOCAL_PROP_2_NAME]));
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
procedure TSDOBaseDataObject_Test.char_procs();
const 
  LOCAL_PROP_1 = s_PROP_CHAR_1;
  LOCAL_PROP_2 = s_PROP_CHAR_2;
  LOCAL_PROP_3 = s_PROP_CHAR_3;
  VAL_1 : TSDOChar = 'W'; VAL_2 : TSDOChar = 's'; VAL_3 : TSDOChar = #0;
var
  obj : ISDODataObject;
  ok : Boolean;
begin
  obj := Create_Object();

  ok := False;
  try
    obj.setCharacter(obj.getProperty('qsdc'),VAL_1);
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.getCharacter(obj.getProperty('qsdc'));
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.setCharacter(obj.getProperty(LOCAL_PROP_3),VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOIllegalArgumentException.');

  obj.setCharacter(obj.getProperty(LOCAL_PROP_1),VAL_1);
  CheckEquals(VAL_1,obj.getCharacter(obj.getProperty(LOCAL_PROP_1)));
  CheckEquals(VAL_1,obj.getCharacter(LOCAL_PROP_1));
  CheckEquals(VAL_1,obj.getCharacter(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_1))));

  obj.setCharacter(obj.getProperty(LOCAL_PROP_1),VAL_2);
  CheckEquals(VAL_2,obj.getCharacter(obj.getProperty(LOCAL_PROP_1)));
  CheckEquals(VAL_2,obj.getCharacter(LOCAL_PROP_1));
  CheckEquals(VAL_2,obj.getCharacter(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_1))));

  obj.setCharacter(obj.getProperty(LOCAL_PROP_1),VAL_3);
  CheckEquals(VAL_3,obj.getCharacter(obj.getProperty(LOCAL_PROP_1)));
  CheckEquals(VAL_3,obj.getCharacter(LOCAL_PROP_1));
  CheckEquals(VAL_3,obj.getCharacter(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_1))));


  obj.setCharacter(obj.getProperty(LOCAL_PROP_2),VAL_1);
  CheckEquals(VAL_1,obj.getCharacter(obj.getProperty(LOCAL_PROP_2)));
  CheckEquals(VAL_1,obj.getCharacter(LOCAL_PROP_2));
  CheckEquals(VAL_1,obj.getCharacter(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_2))));

  obj.setCharacter(obj.getProperty(LOCAL_PROP_2),VAL_2);
  CheckEquals(VAL_2,obj.getCharacter(obj.getProperty(LOCAL_PROP_2)));
  CheckEquals(VAL_2,obj.getCharacter(LOCAL_PROP_2));
  CheckEquals(VAL_2,obj.getCharacter(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_2))));

  obj.setCharacter(obj.getProperty(LOCAL_PROP_2),VAL_3);
  CheckEquals(VAL_3,obj.getCharacter(obj.getProperty(LOCAL_PROP_2)));
  CheckEquals(VAL_3,obj.getCharacter(LOCAL_PROP_2));
  CheckEquals(VAL_3,obj.getCharacter(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_2))));
end;

procedure TSDOBaseDataObject_Test.char_multivalue();
const LOCAL_PROP = s_PROP_CHAR_3; LOCAL_ARRAY_LEN = 100;
var
  val_list : array[0..Pred(LOCAL_ARRAY_LEN)] of TSDOChar;

  procedure PrepareArray();
  var
    k : Integer;
  begin
    Randomize();
    for k := 0 to Pred(LOCAL_ARRAY_LEN) do begin
      val_list[k] := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));
    end;
  end;

var
  obj : ISDODataObject;
  ls, ls1 : ISDODataObjectList;
  i : Integer;
  crs : ILinkedListCursor;
  ok : Boolean;
begin
  PrepareArray();
  obj := Create_Object();

  ok := False;
  try
    ls := obj.getList(s_PROP_BOOL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  Check(ok,Format('"%s" is not a multivalue property.',[s_PROP_BOOL_1]));

  ls := obj.getList(LOCAL_PROP);
    Check(ls <> nil);
  ls1 := obj.getList(LOCAL_PROP);
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Name)');

  ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');
  ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');

  ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');
  ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');

  CheckEquals(0,ls.size(),'Size');
  for i := 0 to Pred(LOCAL_ARRAY_LEN) do begin
    ls.append(val_list[i]);
  end;

  crs := ls.getCursor();
  Check(crs <> nil,'ls.getCursor()');
  crs.Reset();
  CheckEquals(LOCAL_ARRAY_LEN,ls.size(),'Size');
  for i := 0 to Pred(LOCAL_ARRAY_LEN) do begin
    crs.MoveNext();
    CheckEquals(val_list[i],ls.getCharacter(),'append() <> getCharacter()');
  end;
end;

procedure TSDOBaseDataObject_Test.char_unset_isset();
const
  LOCAL_PROP = s_PROP_CHAR_1;
  LOCAL_PROP_ARRAY = s_PROP_CHAR_3;
  VAL_1 : TSDOChar = 'k';  VAL_2 : TSDOChar = 'x';
var
  obj : ISDODataObject;
  ls : ISDODataObjectList;
begin
  obj := Create_Object();

  CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 1');

  obj.setCharacter(LOCAL_PROP, VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
    obj.unset(LOCAL_PROP);
      CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
      CheckEquals(obj.getProperty(LOCAL_PROP).getCharacterDefault(),obj.getCharacter(LOCAL_PROP),'"unSet" should restore the default value');
    obj.setCharacter(LOCAL_PROP, VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
      obj.unset(LOCAL_PROP);
        CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);

  ls := obj.getList(LOCAL_PROP_ARRAY);
  Check(ls <> nil);
  CheckEquals(0,ls.size());
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 2');
  ls.append(VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 3');
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 4');
      CheckEquals(0,ls.size());
    ls.append(VAL_1);
    ls.append(VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 5');
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 6');
      CheckEquals(2,ls.size());
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 7');
      CheckEquals(0,ls.size());
end;

procedure TSDOBaseDataObject_Test.char_setnull_isnull();
const
  LOCAL_PROP = s_PROP_CHAR_1;
  LOCAL_PROP_ARRAY = s_PROP_CHAR_3;
  VAL_1 : TSDOChar = 'z';
var
  obj : ISDODataObject;
begin
  obj := Create_Object();

  // these _does_ depend on the property's default
  CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isNull(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);

  obj.setCharacter(LOCAL_PROP, VAL_1);
    CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
    obj.setNull(LOCAL_PROP);
      CheckEquals(True,obj.isNull(LOCAL_PROP),LOCAL_PROP);
      CheckEquals(#0,obj.getCharacter(LOCAL_PROP),LOCAL_PROP);
end;

procedure TSDOBaseDataObject_Test.property_default_value_char();
const
  VAL_1 : TSDOChar = 'q';
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_CHAR_1,sdo_namespace,SDOTypeDefaultTypeNames[CharacterType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_CHAR_1);
        locProp.setDefault(VAL_1);

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  CheckEquals(VAL_1, locA.getCharacter(locProp), 'getCharacter');
  CheckEquals(False, locA.isSet(locProp), 'isSet');
end;

procedure TSDOBaseDataObject_Test.property_default_value_unset_char();
const
  VAL_1 : TSDOChar = 'e'; VAL_2 : TSDOChar = 'r';
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_CHAR_1,sdo_namespace,SDOTypeDefaultTypeNames[CharacterType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_CHAR_1);
        locProp.setDefault(TSDOChar(VAL_1));

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  locA.setCharacter(locProp, VAL_2);
    CheckEquals(True, locA.isSet(locProp), 'isSet');
    CheckEquals(VAL_2, locA.getCharacter(locProp), 'getCharacter');
    locA.unset(locProp);
      CheckEquals(False, locA.isSet(locProp), 'isSet');
      CheckEquals(VAL_1, locA.getCharacter(locProp), 'getCharacter');
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
procedure TSDOBaseDataObject_Test.currency_procs();
const 
  LOCAL_PROP_1 = s_PROP_CURRENCY_1;
  LOCAL_PROP_2 = s_PROP_CURRENCY_2;
  LOCAL_PROP_3 = s_PROP_CURRENCY_3;
  VAL_1 : TSDOCurrency = 12345678.9101; VAL_2 : TSDOCurrency = -9879879.8765; VAL_3 : TSDOCurrency = 0;
var
  obj : ISDODataObject;
  ok : Boolean;
begin
  obj := Create_Object();

  ok := False;
  try
    obj.setCurrency(obj.getProperty('qsdc'),VAL_1);
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.getCurrency(obj.getProperty('qsdc'));
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.setCurrency(obj.getProperty(LOCAL_PROP_3),VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOIllegalArgumentException.');

  obj.setCurrency(obj.getProperty(LOCAL_PROP_1),VAL_1);
  CheckEquals(VAL_1,obj.getCurrency(obj.getProperty(LOCAL_PROP_1)));
  CheckEquals(VAL_1,obj.getCurrency(LOCAL_PROP_1));
  CheckEquals(VAL_1,obj.getCurrency(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_1))));

  obj.setCurrency(obj.getProperty(LOCAL_PROP_1),VAL_2);
  CheckEquals(VAL_2,obj.getCurrency(obj.getProperty(LOCAL_PROP_1)));
  CheckEquals(VAL_2,obj.getCurrency(LOCAL_PROP_1));
  CheckEquals(VAL_2,obj.getCurrency(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_1))));

  obj.setCurrency(obj.getProperty(LOCAL_PROP_1),VAL_3);
  CheckEquals(VAL_3,obj.getCurrency(obj.getProperty(LOCAL_PROP_1)));
  CheckEquals(VAL_3,obj.getCurrency(LOCAL_PROP_1));
  CheckEquals(VAL_3,obj.getCurrency(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_1))));


  obj.setCurrency(obj.getProperty(LOCAL_PROP_2),VAL_1);
  CheckEquals(VAL_1,obj.getCurrency(obj.getProperty(LOCAL_PROP_2)));
  CheckEquals(VAL_1,obj.getCurrency(LOCAL_PROP_2));
  CheckEquals(VAL_1,obj.getCurrency(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_2))));

  obj.setCurrency(obj.getProperty(LOCAL_PROP_2),VAL_2);
  CheckEquals(VAL_2,obj.getCurrency(obj.getProperty(LOCAL_PROP_2)));
  CheckEquals(VAL_2,obj.getCurrency(LOCAL_PROP_2));
  CheckEquals(VAL_2,obj.getCurrency(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_2))));

  obj.setCurrency(obj.getProperty(LOCAL_PROP_2),VAL_3);
  CheckEquals(VAL_3,obj.getCurrency(obj.getProperty(LOCAL_PROP_2)));
  CheckEquals(VAL_3,obj.getCurrency(LOCAL_PROP_2));
  CheckEquals(VAL_3,obj.getCurrency(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_2))));
end;

procedure TSDOBaseDataObject_Test.currency_multivalue();
const LOCAL_PROP = s_PROP_CURRENCY_3; LOCAL_ARRAY_LEN = 100;
var
  val_list : array[0..Pred(LOCAL_ARRAY_LEN)] of TSDOCurrency;

  procedure PrepareArray();
  var
    k : Integer;
  begin
    Randomize();
    for k := 0 to Pred(LOCAL_ARRAY_LEN) do begin
      val_list[k] := RandomRange(Low(TSDOInteger),High(TSDOInteger));
      if ( ( k mod 3 ) = 0 ) then
        val_list[k] := val_list[k] + High(TSDOInteger)
      else if ( ( k mod 4 ) = 0 ) then
        val_list[k] := val_list[k] - High(TSDOInteger)
    end;
  end;

var
  obj : ISDODataObject;
  ls, ls1 : ISDODataObjectList;
  i : Integer;
  crs : ILinkedListCursor;
  ok : Boolean;
begin
  PrepareArray();
  obj := Create_Object();

  ok := False;
  try
    ls := obj.getList(s_PROP_BOOL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  Check(ok,Format('"%s" is not a multivalue property.',[s_PROP_BOOL_1]));

  ls := obj.getList(LOCAL_PROP);
    Check(ls <> nil);
  ls1 := obj.getList(LOCAL_PROP);
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Name)');

  ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');
  ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');

  ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');
  ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');

  CheckEquals(0,ls.size(),'Size');
  for i := 0 to Pred(LOCAL_ARRAY_LEN) do begin
    ls.appendCurrency(val_list[i]);
  end;

  crs := ls.getCursor();
  Check(crs <> nil,'ls.getCursor()');
  crs.Reset();
  CheckEquals(LOCAL_ARRAY_LEN,ls.size(),'Size');
  for i := 0 to Pred(LOCAL_ARRAY_LEN) do begin
    crs.MoveNext();
    CheckEquals(val_list[i],ls.getCurrency(),'appendCurrency() <> getCurrency()');
  end;
end;

procedure TSDOBaseDataObject_Test.currency_unset_isset();
const
  LOCAL_PROP = s_PROP_CURRENCY_1;
  LOCAL_PROP_ARRAY = s_PROP_CURRENCY_3;
  VAL_1 : TSDOCurrency = 12345678.9101; VAL_2 : TSDOCurrency = -9879879.8765; 
var
  obj : ISDODataObject;
  ls : ISDODataObjectList;
begin
  obj := Create_Object();

  CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 1');

  obj.setCurrency(LOCAL_PROP, VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
    obj.unset(LOCAL_PROP);
      CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
      CheckEquals(obj.getProperty(LOCAL_PROP).getCurrencyDefault(),obj.getCurrency(LOCAL_PROP),'"unSet" should restore the default value');
    obj.setCurrency(LOCAL_PROP, VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
      obj.unset(LOCAL_PROP);
        CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);

  ls := obj.getList(LOCAL_PROP_ARRAY);
  Check(ls <> nil);
  CheckEquals(0,ls.size());
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 2');
  ls.appendCurrency(VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 3');
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 4');
      CheckEquals(0,ls.size());
    ls.appendCurrency(VAL_1);
    ls.appendCurrency(VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 5');
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 6');
      CheckEquals(2,ls.size());
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 7');
      CheckEquals(0,ls.size());
end;

procedure TSDOBaseDataObject_Test.currency_setnull_isnull();
const
  LOCAL_PROP = s_PROP_CURRENCY_1;
  LOCAL_PROP_ARRAY = s_PROP_CURRENCY_3;
  VAL_1 : TSDOCurrency = 8529637418529.6341;
var
  obj : ISDODataObject;
begin
  obj := Create_Object();

  // these _does_ depend on the property's default
  CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isNull(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);

  obj.setCurrency(LOCAL_PROP, VAL_1);
    CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
    obj.setNull(LOCAL_PROP);
      CheckEquals(True,obj.isNull(LOCAL_PROP),LOCAL_PROP);
      CheckEquals(0,obj.getCurrency(LOCAL_PROP),LOCAL_PROP);
end;

procedure TSDOBaseDataObject_Test.property_default_value_currency();
const
  VAL_1 : TSDOCurrency = 987654321741.8563;
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_CURRENCY_1,sdo_namespace,SDOTypeDefaultTypeNames[CurrencyType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_CURRENCY_1);
        locProp.setDefaultCurrency(VAL_1);

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  CheckEqualsCurrency(VAL_1, locA.getCurrency(locProp), 'getCurrency');
  CheckEquals(False, locA.isSet(locProp), 'isSet');
end;

procedure TSDOBaseDataObject_Test.property_default_value_unset_currency();
const
  VAL_1 : TSDOCurrency = 7481592639867.5321; VAL_2 : TSDOCurrency = -193742865123.6574;
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_CURRENCY_1,sdo_namespace,SDOTypeDefaultTypeNames[CurrencyType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_CURRENCY_1);
        locProp.setDefaultCurrency(VAL_1);

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  locA.setCurrency(locProp, VAL_2);
    CheckEquals(True, locA.isSet(locProp), 'isSet');
    CheckEqualsCurrency(VAL_2, locA.getCurrency(locProp), 'getCurrency');
    locA.unset(locProp);
      CheckEquals(False, locA.isSet(locProp), 'isSet');
      CheckEqualsCurrency(VAL_1, locA.getCurrency(locProp), 'getCurrency');
end;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
procedure TSDOBaseDataObject_Test.double_procs();
const 
  LOCAL_PROP_1 = s_PROP_DOUBLE_1;
  LOCAL_PROP_2 = s_PROP_DOUBLE_2;
  LOCAL_PROP_3 = s_PROP_DOUBLE_3;
  VAL_1 : TSDODouble = 12345678.91011; VAL_2 : TSDODouble = -9879879.87654; VAL_3 : TSDODouble = 0;
var
  obj : ISDODataObject;
  ok : Boolean;
begin
  obj := Create_Object();

  ok := False;
  try
    obj.setDouble(obj.getProperty('qsdc'),VAL_1);
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.getDouble(obj.getProperty('qsdc'));
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.setDouble(obj.getProperty(LOCAL_PROP_3),VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOIllegalArgumentException.');

  obj.setDouble(obj.getProperty(LOCAL_PROP_1),VAL_1);
  CheckEquals(VAL_1,obj.getDouble(obj.getProperty(LOCAL_PROP_1)));
  CheckEquals(VAL_1,obj.getDouble(LOCAL_PROP_1));
  CheckEquals(VAL_1,obj.getDouble(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_1))));

  obj.setDouble(obj.getProperty(LOCAL_PROP_1),VAL_2);
  CheckEquals(VAL_2,obj.getDouble(obj.getProperty(LOCAL_PROP_1)));
  CheckEquals(VAL_2,obj.getDouble(LOCAL_PROP_1));
  CheckEquals(VAL_2,obj.getDouble(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_1))));

  obj.setDouble(obj.getProperty(LOCAL_PROP_1),VAL_3);
  CheckEquals(VAL_3,obj.getDouble(obj.getProperty(LOCAL_PROP_1)));
  CheckEquals(VAL_3,obj.getDouble(LOCAL_PROP_1));
  CheckEquals(VAL_3,obj.getDouble(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_1))));


  obj.setDouble(obj.getProperty(LOCAL_PROP_2),VAL_1);
  CheckEquals(VAL_1,obj.getDouble(obj.getProperty(LOCAL_PROP_2)));
  CheckEquals(VAL_1,obj.getDouble(LOCAL_PROP_2));
  CheckEquals(VAL_1,obj.getDouble(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_2))));

  obj.setDouble(obj.getProperty(LOCAL_PROP_2),VAL_2);
  CheckEquals(VAL_2,obj.getDouble(obj.getProperty(LOCAL_PROP_2)));
  CheckEquals(VAL_2,obj.getDouble(LOCAL_PROP_2));
  CheckEquals(VAL_2,obj.getDouble(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_2))));

  obj.setDouble(obj.getProperty(LOCAL_PROP_2),VAL_3);
  CheckEquals(VAL_3,obj.getDouble(obj.getProperty(LOCAL_PROP_2)));
  CheckEquals(VAL_3,obj.getDouble(LOCAL_PROP_2));
  CheckEquals(VAL_3,obj.getDouble(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_2))));
end;

procedure TSDOBaseDataObject_Test.double_multivalue();
const LOCAL_PROP = s_PROP_DOUBLE_3; LOCAL_ARRAY_LEN = 100;
var
  val_list : array[0..Pred(LOCAL_ARRAY_LEN)] of TSDODouble;

  procedure PrepareArray();
  var
    k : Integer;
  begin
    Randomize();
    for k := 0 to Pred(LOCAL_ARRAY_LEN) do begin
      val_list[k] := RandomRange(Low(TSDOInteger),High(TSDOInteger));
      if ( ( k mod 3 ) = 0 ) then
        val_list[k] := val_list[k] + High(TSDOInteger)
      else if ( ( k mod 4 ) = 0 ) then
        val_list[k] := val_list[k] - High(TSDOInteger)
    end;
  end;

var
  obj : ISDODataObject;
  ls, ls1 : ISDODataObjectList;
  i : Integer;
  crs : ILinkedListCursor;
  ok : Boolean;
begin
  PrepareArray();
  obj := Create_Object();

  ok := False;
  try
    ls := obj.getList(s_PROP_BOOL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  Check(ok,Format('"%s" is not a multivalue property.',[s_PROP_BOOL_1]));

  ls := obj.getList(LOCAL_PROP);
    Check(ls <> nil);
  ls1 := obj.getList(LOCAL_PROP);
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Name)');

  ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');
  ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');

  ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');
  ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');

  CheckEquals(0,ls.size(),'Size');
  for i := 0 to Pred(LOCAL_ARRAY_LEN) do begin
    ls.append(val_list[i]);
  end;

  crs := ls.getCursor();
  Check(crs <> nil,'ls.getCursor()');
  crs.Reset();
  CheckEquals(LOCAL_ARRAY_LEN,ls.size(),'Size');
  for i := 0 to Pred(LOCAL_ARRAY_LEN) do begin
    crs.MoveNext();
    CheckEquals(val_list[i],ls.getDouble(),'append() <> getDouble()');
  end;
end;

procedure TSDOBaseDataObject_Test.double_unset_isset();
const
  LOCAL_PROP = s_PROP_DOUBLE_1;
  LOCAL_PROP_ARRAY = s_PROP_DOUBLE_3;
  VAL_1 : TSDODouble = 12345678.91011; VAL_2 : TSDODouble = -9879879.87654; 
var
  obj : ISDODataObject;
  ls : ISDODataObjectList;
begin
  obj := Create_Object();

  CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 1');

  obj.setDouble(LOCAL_PROP, VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
    obj.unset(LOCAL_PROP);
      CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
      CheckEquals(obj.getProperty(LOCAL_PROP).getDoubleDefault(),obj.getDouble(LOCAL_PROP),'"unSet" should restore the default value');
    obj.setDouble(LOCAL_PROP, VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
      obj.unset(LOCAL_PROP);
        CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);

  ls := obj.getList(LOCAL_PROP_ARRAY);
  Check(ls <> nil);
  CheckEquals(0,ls.size());
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 2');
  ls.append(VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 3');
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 4');
      CheckEquals(0,ls.size());
    ls.append(VAL_1);
    ls.append(VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 5');
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 6');
      CheckEquals(2,ls.size());
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 7');
      CheckEquals(0,ls.size());
end;

procedure TSDOBaseDataObject_Test.double_setnull_isnull();
const
  LOCAL_PROP = s_PROP_DOUBLE_1;
  LOCAL_PROP_ARRAY = s_PROP_DOUBLE_3;
  VAL_1 : TSDODouble = 8529637418529.63741;
var
  obj : ISDODataObject;
begin
  obj := Create_Object();

  // these _does_ depend on the property's default
  CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isNull(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);

  obj.setDouble(LOCAL_PROP, VAL_1);
    CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
    obj.setNull(LOCAL_PROP);
      CheckEquals(True,obj.isNull(LOCAL_PROP),LOCAL_PROP);
      CheckEquals(0,obj.getDouble(LOCAL_PROP),LOCAL_PROP);
end;

procedure TSDOBaseDataObject_Test.property_default_value_double();
const
  VAL_1 : TSDODouble = 987654321741.852963;
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_DOUBLE_1,sdo_namespace,SDOTypeDefaultTypeNames[DoubleType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_DOUBLE_1);
        locProp.setDefault(VAL_1);

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  CheckEquals(VAL_1, locA.getDouble(locProp), 'getDouble');
  CheckEquals(False, locA.isSet(locProp), 'isSet');
end;

procedure TSDOBaseDataObject_Test.property_default_value_unset_double();
const
  VAL_1 : TSDODouble = 7481592639867.53421; VAL_2 : TSDODouble = -193742865123.6549874;
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_DOUBLE_1,sdo_namespace,SDOTypeDefaultTypeNames[DoubleType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_DOUBLE_1);
        locProp.setDefault(TSDODouble(VAL_1));

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  locA.setDouble(locProp, VAL_2);
    CheckEquals(True, locA.isSet(locProp), 'isSet');
    CheckEquals(VAL_2, locA.getDouble(locProp), 'getDouble');
    locA.unset(locProp);
      CheckEquals(False, locA.isSet(locProp), 'isSet');
      CheckEquals(VAL_1, locA.getDouble(locProp), 'getDouble');
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
procedure TSDOBaseDataObject_Test.float_procs();
const 
  LOCAL_PROP_1 = s_PROP_FLOAT_1;
  LOCAL_PROP_2 = s_PROP_FLOAT_2;
  LOCAL_PROP_3 = s_PROP_FLOAT_3;
  VAL_1 : TSDOFloat = 12345678.91011; VAL_2 : TSDOFloat = -9879879.87654; VAL_3 : TSDOFloat = 0;
var
  obj : ISDODataObject;
  ok : Boolean;
begin
  obj := Create_Object();

  ok := False;
  try
    obj.setFloat(obj.getProperty('qsdc'),VAL_1);
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.getFloat(obj.getProperty('qsdc'));
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.setFloat(obj.getProperty(LOCAL_PROP_3),VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOIllegalArgumentException.');

  obj.setFloat(obj.getProperty(LOCAL_PROP_1),VAL_1);
  CheckEquals(VAL_1,obj.getFloat(obj.getProperty(LOCAL_PROP_1)));
  CheckEquals(VAL_1,obj.getFloat(LOCAL_PROP_1));
  CheckEquals(VAL_1,obj.getFloat(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_1))));

  obj.setFloat(obj.getProperty(LOCAL_PROP_1),VAL_2);
  CheckEquals(VAL_2,obj.getFloat(obj.getProperty(LOCAL_PROP_1)));
  CheckEquals(VAL_2,obj.getFloat(LOCAL_PROP_1));
  CheckEquals(VAL_2,obj.getFloat(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_1))));

  obj.setFloat(obj.getProperty(LOCAL_PROP_1),VAL_3);
  CheckEquals(VAL_3,obj.getFloat(obj.getProperty(LOCAL_PROP_1)));
  CheckEquals(VAL_3,obj.getFloat(LOCAL_PROP_1));
  CheckEquals(VAL_3,obj.getFloat(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_1))));


  obj.setFloat(obj.getProperty(LOCAL_PROP_2),VAL_1);
  CheckEquals(VAL_1,obj.getFloat(obj.getProperty(LOCAL_PROP_2)));
  CheckEquals(VAL_1,obj.getFloat(LOCAL_PROP_2));
  CheckEquals(VAL_1,obj.getFloat(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_2))));

  obj.setFloat(obj.getProperty(LOCAL_PROP_2),VAL_2);
  CheckEquals(VAL_2,obj.getFloat(obj.getProperty(LOCAL_PROP_2)));
  CheckEquals(VAL_2,obj.getFloat(LOCAL_PROP_2));
  CheckEquals(VAL_2,obj.getFloat(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_2))));

  obj.setFloat(obj.getProperty(LOCAL_PROP_2),VAL_3);
  CheckEquals(VAL_3,obj.getFloat(obj.getProperty(LOCAL_PROP_2)));
  CheckEquals(VAL_3,obj.getFloat(LOCAL_PROP_2));
  CheckEquals(VAL_3,obj.getFloat(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_2))));
end;

procedure TSDOBaseDataObject_Test.float_multivalue();
const LOCAL_PROP = s_PROP_FLOAT_3; LOCAL_ARRAY_LEN = 100;
var
  val_list : array[0..Pred(LOCAL_ARRAY_LEN)] of TSDOFloat;

  procedure PrepareArray();
  var
    k : Integer;
  begin
    Randomize();
    for k := 0 to Pred(LOCAL_ARRAY_LEN) do begin
      val_list[k] := RandomRange(Low(TSDOInteger),High(TSDOInteger));
      if ( ( k mod 3 ) = 0 ) then
        val_list[k] := val_list[k] + High(TSDOInteger)
      else if ( ( k mod 4 ) = 0 ) then
        val_list[k] := val_list[k] - High(TSDOInteger)
    end;
  end;

var
  obj : ISDODataObject;
  ls, ls1 : ISDODataObjectList;
  i : Integer;
  crs : ILinkedListCursor;
  ok : Boolean;
begin
  PrepareArray();
  obj := Create_Object();

  ok := False;
  try
    ls := obj.getList(s_PROP_BOOL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  Check(ok,Format('"%s" is not a multivalue property.',[s_PROP_BOOL_1]));

  ls := obj.getList(LOCAL_PROP);
    Check(ls <> nil);
  ls1 := obj.getList(LOCAL_PROP);
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Name)');

  ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');
  ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');

  ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');
  ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');

  CheckEquals(0,ls.size(),'Size');
  for i := 0 to Pred(LOCAL_ARRAY_LEN) do begin
    ls.append(val_list[i]);
  end;

  crs := ls.getCursor();
  Check(crs <> nil,'ls.getCursor()');
  crs.Reset();
  CheckEquals(LOCAL_ARRAY_LEN,ls.size(),'Size');
  for i := 0 to Pred(LOCAL_ARRAY_LEN) do begin
    crs.MoveNext();
    CheckEquals(val_list[i],ls.getFloat(),'append() <> getFloat()');
  end;
end;

procedure TSDOBaseDataObject_Test.float_unset_isset();
const
  LOCAL_PROP = s_PROP_FLOAT_1;
  LOCAL_PROP_ARRAY = s_PROP_FLOAT_3;
  VAL_1 : TSDOFloat = 12345678.91011; VAL_2 : TSDOFloat = -9879879.87654; 
var
  obj : ISDODataObject;
  ls : ISDODataObjectList;
begin
  obj := Create_Object();

  CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 1');

  obj.setFloat(LOCAL_PROP, VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
    obj.unset(LOCAL_PROP);
      CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
      CheckEquals(obj.getProperty(LOCAL_PROP).getFloatDefault(),obj.getFloat(LOCAL_PROP),'"unSet" should restore the default value');
    obj.setFloat(LOCAL_PROP, VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
      obj.unset(LOCAL_PROP);
        CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);

  ls := obj.getList(LOCAL_PROP_ARRAY);
  Check(ls <> nil);
  CheckEquals(0,ls.size());
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 2');
  ls.append(VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 3');
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 4');
      CheckEquals(0,ls.size());
    ls.append(VAL_1);
    ls.append(VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 5');
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 6');
      CheckEquals(2,ls.size());
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 7');
      CheckEquals(0,ls.size());
end;

procedure TSDOBaseDataObject_Test.float_setnull_isnull();
const
  LOCAL_PROP = s_PROP_FLOAT_1;
  LOCAL_PROP_ARRAY = s_PROP_FLOAT_3;
  VAL_1 : TSDOFloat = 8529637418529.63741;
var
  obj : ISDODataObject;
begin
  obj := Create_Object();

  // these _does_ depend on the property's default
  CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isNull(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);

  obj.setFloat(LOCAL_PROP, VAL_1);
    CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
    obj.setNull(LOCAL_PROP);
      CheckEquals(True,obj.isNull(LOCAL_PROP),LOCAL_PROP);
      CheckEquals(0,obj.getFloat(LOCAL_PROP),LOCAL_PROP);
end;

procedure TSDOBaseDataObject_Test.property_default_value_float();
const
  VAL_1 : TSDOFloat = 987654321741.852963;
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_FLOAT_1,sdo_namespace,SDOTypeDefaultTypeNames[FloatType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_FLOAT_1);
        locProp.setDefault(VAL_1);

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  CheckEquals(VAL_1, locA.getFloat(locProp), 'getFloat');
  CheckEquals(False, locA.isSet(locProp), 'isSet');
end;

procedure TSDOBaseDataObject_Test.property_default_value_unset_float();
const
  VAL_1 : TSDOFloat = 7481592639867.53421; VAL_2 : TSDOFloat = -193742865123.6549874;
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_FLOAT_1,sdo_namespace,SDOTypeDefaultTypeNames[FloatType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_FLOAT_1);
        locProp.setDefault(TSDOFloat(VAL_1));

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  locA.setFloat(locProp, VAL_2);
    CheckEquals(True, locA.isSet(locProp), 'isSet');
    CheckEquals(VAL_2, locA.getFloat(locProp), 'getFloat');
    locA.unset(locProp);
      CheckEquals(False, locA.isSet(locProp), 'isSet');
      CheckEquals(VAL_1, locA.getFloat(locProp), 'getFloat');
end;
{$ENDIF HAS_SDO_FLOAT}

{$IFDEF HAS_SDO_LONG}
procedure TSDOBaseDataObject_Test.long_procs();
const 
  LOCAL_PROP_1 = s_PROP_LONG_1;
  LOCAL_PROP_2 = s_PROP_LONG_2;
  LOCAL_PROP_3 = s_PROP_LONG_3;
  VAL_1 : TSDOLong = 1234567891011; VAL_2 : TSDOLong = -987987987654; VAL_3 : TSDOLong = 0;
var
  obj : ISDODataObject;
  ok : Boolean;
begin
  obj := Create_Object();

  ok := False;
  try
    obj.setLong(obj.getProperty('qsdc'),VAL_1);
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.getLong(obj.getProperty('qsdc'));
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.setLong(obj.getProperty(LOCAL_PROP_3),VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOIllegalArgumentException.');

  obj.setLong(obj.getProperty(LOCAL_PROP_1),VAL_1);
  CheckEquals(VAL_1,obj.getLong(obj.getProperty(LOCAL_PROP_1)));
  CheckEquals(VAL_1,obj.getLong(LOCAL_PROP_1));
  CheckEquals(VAL_1,obj.getLong(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_1))));

  obj.setLong(obj.getProperty(LOCAL_PROP_1),VAL_2);
  CheckEquals(VAL_2,obj.getLong(obj.getProperty(LOCAL_PROP_1)));
  CheckEquals(VAL_2,obj.getLong(LOCAL_PROP_1));
  CheckEquals(VAL_2,obj.getLong(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_1))));

  obj.setLong(obj.getProperty(LOCAL_PROP_1),VAL_3);
  CheckEquals(VAL_3,obj.getLong(obj.getProperty(LOCAL_PROP_1)));
  CheckEquals(VAL_3,obj.getLong(LOCAL_PROP_1));
  CheckEquals(VAL_3,obj.getLong(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_1))));


  obj.setLong(obj.getProperty(LOCAL_PROP_2),VAL_1);
  CheckEquals(VAL_1,obj.getLong(obj.getProperty(LOCAL_PROP_2)));
  CheckEquals(VAL_1,obj.getLong(LOCAL_PROP_2));
  CheckEquals(VAL_1,obj.getLong(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_2))));

  obj.setLong(obj.getProperty(LOCAL_PROP_2),VAL_2);
  CheckEquals(VAL_2,obj.getLong(obj.getProperty(LOCAL_PROP_2)));
  CheckEquals(VAL_2,obj.getLong(LOCAL_PROP_2));
  CheckEquals(VAL_2,obj.getLong(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_2))));

  obj.setLong(obj.getProperty(LOCAL_PROP_2),VAL_3);
  CheckEquals(VAL_3,obj.getLong(obj.getProperty(LOCAL_PROP_2)));
  CheckEquals(VAL_3,obj.getLong(LOCAL_PROP_2));
  CheckEquals(VAL_3,obj.getLong(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_2))));
end;

procedure TSDOBaseDataObject_Test.long_multivalue();
const LOCAL_PROP = s_PROP_LONG_3; LOCAL_ARRAY_LEN = 100;
var
  val_list : array[0..Pred(LOCAL_ARRAY_LEN)] of TSDOLong;

  procedure PrepareArray();
  var
    k : Integer;
  begin
    Randomize();
    for k := 0 to Pred(LOCAL_ARRAY_LEN) do begin
      val_list[k] := RandomRange(Low(TSDOInteger),High(TSDOInteger));
      if ( ( k mod 3 ) = 0 ) then
        val_list[k] := val_list[k] + High(TSDOInteger)
      else if ( ( k mod 4 ) = 0 ) then
        val_list[k] := val_list[k] - High(TSDOInteger)
    end;
  end;

var
  obj : ISDODataObject;
  ls, ls1 : ISDODataObjectList;
  i : Integer;
  crs : ILinkedListCursor;
  ok : Boolean;
begin
  PrepareArray();
  obj := Create_Object();

  ok := False;
  try
    ls := obj.getList(s_PROP_BOOL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  Check(ok,Format('"%s" is not a multivalue property.',[s_PROP_BOOL_1]));

  ls := obj.getList(LOCAL_PROP);
    Check(ls <> nil);
  ls1 := obj.getList(LOCAL_PROP);
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Name)');

  ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');
  ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');

  ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');
  ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');

  CheckEquals(0,ls.size(),'Size');
  for i := 0 to Pred(LOCAL_ARRAY_LEN) do begin
    ls.append(val_list[i]);
  end;

  crs := ls.getCursor();
  Check(crs <> nil,'ls.getCursor()');
  crs.Reset();
  CheckEquals(LOCAL_ARRAY_LEN,ls.size(),'Size');
  for i := 0 to Pred(LOCAL_ARRAY_LEN) do begin
    crs.MoveNext();
    CheckEquals(val_list[i],ls.getLong(),'append() <> getLong()');
  end;
end;

procedure TSDOBaseDataObject_Test.long_unset_isset();
const
  LOCAL_PROP = s_PROP_LONG_1;
  LOCAL_PROP_ARRAY = s_PROP_LONG_3;
  VAL_1 : TSDOLong = 123456789125525;  VAL_2 : TSDOLong = -123987321654987;
var
  obj : ISDODataObject;
  ls : ISDODataObjectList;
begin
  obj := Create_Object();

  CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 1');

  obj.setLong(LOCAL_PROP, VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
    obj.unset(LOCAL_PROP);
      CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
      CheckEquals(obj.getProperty(LOCAL_PROP).getLongDefault(),obj.getLong(LOCAL_PROP),'"unSet" should restore the default value');
    obj.setLong(LOCAL_PROP, VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
      obj.unset(LOCAL_PROP);
        CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);

  ls := obj.getList(LOCAL_PROP_ARRAY);
  Check(ls <> nil);
  CheckEquals(0,ls.size());
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 2');
  ls.append(VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 3');
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 4');
      CheckEquals(0,ls.size());
    ls.append(VAL_1);
    ls.append(VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 5');
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 6');
      CheckEquals(2,ls.size());
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 7');
      CheckEquals(0,ls.size());
end;

procedure TSDOBaseDataObject_Test.long_setnull_isnull();
const
  LOCAL_PROP = s_PROP_LONG_1;
  LOCAL_PROP_ARRAY = s_PROP_LONG_3;
  VAL_1 : TSDOLong = 852963741852963741;
var
  obj : ISDODataObject;
begin
  obj := Create_Object();

  // these _does_ depend on the property's default
  CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isNull(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);

  obj.setLong(LOCAL_PROP, VAL_1);
    CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
    obj.setNull(LOCAL_PROP);
      CheckEquals(True,obj.isNull(LOCAL_PROP),LOCAL_PROP);
      CheckEquals(0,obj.getLong(LOCAL_PROP),LOCAL_PROP);
end;

procedure TSDOBaseDataObject_Test.property_default_value_long();
const
  VAL_1 : TSDOLong = 987654321741852963;
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_LONG_1,sdo_namespace,SDOTypeDefaultTypeNames[LongType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_LONG_1);
        locProp.setDefault(VAL_1);

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  CheckEquals(VAL_1, locA.getLong(locProp), 'getLong');
  CheckEquals(False, locA.isSet(locProp), 'isSet');
end;

procedure TSDOBaseDataObject_Test.property_default_value_unset_long();
const
  VAL_1 : TSDOLong = 748159263986753421; VAL_2 : TSDOLong = -1937428651236549874;
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_LONG_1,sdo_namespace,SDOTypeDefaultTypeNames[LongType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_LONG_1);
        locProp.setDefault(TSDOLong(VAL_1));

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  locA.setLong(locProp, VAL_2);
    CheckEquals(True, locA.isSet(locProp), 'isSet');
    CheckEquals(VAL_2, locA.getLong(locProp), 'getLong');
    locA.unset(locProp);
      CheckEquals(False, locA.isSet(locProp), 'isSet');
      CheckEquals(VAL_1, locA.getLong(locProp), 'getLong');
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure TSDOBaseDataObject_Test.short_procs();
const
  LOCAL_PROP_1 = s_PROP_SHORT_1;
  LOCAL_PROP_2 = s_PROP_SHORT_2;
  LOCAL_PROP_3 = s_PROP_SHORT_3;
  VAL_1 : TSDOShort = 4567; VAL_2 : TSDOShort = -9856; VAL_3 : TSDOShort = 0;
var
  obj : ISDODataObject;
  ok : Boolean;
begin
  obj := Create_Object();

  ok := False;
  try
    obj.setShort(obj.getProperty('qsdc'),VAL_1);
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.getShort(obj.getProperty('qsdc'));
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.setShort(obj.getProperty(LOCAL_PROP_3),VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOIllegalArgumentException.');

  obj.setShort(obj.getProperty(LOCAL_PROP_1),VAL_1);
  CheckEquals(VAL_1,obj.getShort(obj.getProperty(LOCAL_PROP_1)));
  CheckEquals(VAL_1,obj.getShort(LOCAL_PROP_1));
  CheckEquals(VAL_1,obj.getShort(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_1))));

  obj.setShort(obj.getProperty(LOCAL_PROP_1),VAL_2);
  CheckEquals(VAL_2,obj.getShort(obj.getProperty(LOCAL_PROP_1)));
  CheckEquals(VAL_2,obj.getShort(LOCAL_PROP_1));
  CheckEquals(VAL_2,obj.getShort(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_1))));

  obj.setShort(obj.getProperty(LOCAL_PROP_1),VAL_3);
  CheckEquals(VAL_3,obj.getShort(obj.getProperty(LOCAL_PROP_1)));
  CheckEquals(VAL_3,obj.getShort(LOCAL_PROP_1));
  CheckEquals(VAL_3,obj.getShort(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_1))));


  obj.setShort(obj.getProperty(LOCAL_PROP_2),VAL_1);
  CheckEquals(VAL_1,obj.getShort(obj.getProperty(LOCAL_PROP_2)));
  CheckEquals(VAL_1,obj.getShort(LOCAL_PROP_2));
  CheckEquals(VAL_1,obj.getShort(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_2))));

  obj.setShort(obj.getProperty(LOCAL_PROP_2),VAL_2);
  CheckEquals(VAL_2,obj.getShort(obj.getProperty(LOCAL_PROP_2)));
  CheckEquals(VAL_2,obj.getShort(LOCAL_PROP_2));
  CheckEquals(VAL_2,obj.getShort(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_2))));

  obj.setShort(obj.getProperty(LOCAL_PROP_2),VAL_3);
  CheckEquals(VAL_3,obj.getShort(obj.getProperty(LOCAL_PROP_2)));
  CheckEquals(VAL_3,obj.getShort(LOCAL_PROP_2));
  CheckEquals(VAL_3,obj.getShort(obj.getPropertyIndex(obj.getProperty(LOCAL_PROP_2))));
end;

procedure TSDOBaseDataObject_Test.short_multivalue();
const LOCAL_PROP = s_PROP_SHORT_3; LOCAL_ARRAY_LEN = 100;
var
  val_list : array[0..Pred(LOCAL_ARRAY_LEN)] of TSDOShort;

  procedure PrepareArray();
  var
    k : Integer;
  begin
    Randomize();
    for k := 0 to Pred(LOCAL_ARRAY_LEN) do begin
      val_list[k] := RandomRange(Low(TSDOShort),High(TSDOShort));
    end;
  end;

var
  obj : ISDODataObject;
  ls, ls1 : ISDODataObjectList;
  i : Integer;
  crs : ILinkedListCursor;
  ok : Boolean;
begin
  PrepareArray();
  obj := Create_Object();

  ok := False;
  try
    ls := obj.getList(s_PROP_BOOL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  Check(ok,Format('"%s" is not a multivalue property.',[s_PROP_BOOL_1]));

  ls := obj.getList(LOCAL_PROP);
    Check(ls <> nil);
  ls1 := obj.getList(LOCAL_PROP);
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Name)');

  ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');
  ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');

  ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');
  ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');

  CheckEquals(0,ls.size(),'Size');
  for i := 0 to Pred(LOCAL_ARRAY_LEN) do begin
    ls.append(val_list[i]);
  end;

  crs := ls.getCursor();
  Check(crs <> nil,'ls.getCursor()');
  crs.Reset();
  CheckEquals(LOCAL_ARRAY_LEN,ls.size(),'Size');
  for i := 0 to Pred(LOCAL_ARRAY_LEN) do begin
    crs.MoveNext();
    CheckEquals(val_list[i],ls.getShort(),'append() <> getShort()');
  end;
end;

procedure TSDOBaseDataObject_Test.short_unset_isset();
const
  LOCAL_PROP = s_PROP_SHORT_1;
  LOCAL_PROP_ARRAY = s_PROP_SHORT_3;
  VAL_1 : TSDOShort = 789;  VAL_2 : TSDOShort = -654;
var
  obj : ISDODataObject;
  ls : ISDODataObjectList;
begin
  obj := Create_Object();

  CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 1');

  obj.setShort(LOCAL_PROP, VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
    obj.unset(LOCAL_PROP);
      CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
      CheckEquals(obj.getProperty(LOCAL_PROP).getShortDefault(),obj.getShort(LOCAL_PROP),'"unSet" should restore the default value');
    obj.setShort(LOCAL_PROP, VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
      obj.unset(LOCAL_PROP);
        CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);

  ls := obj.getList(LOCAL_PROP_ARRAY);
  Check(ls <> nil);
  CheckEquals(0,ls.size());
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 2');
  ls.append(VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 3');
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 4');
      CheckEquals(0,ls.size());
    ls.append(VAL_1);
    ls.append(VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 5');
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 6');
      CheckEquals(2,ls.size());
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 7');
      CheckEquals(0,ls.size());
end;

procedure TSDOBaseDataObject_Test.short_setnull_isnull();
const
  LOCAL_PROP = s_PROP_SHORT_1;
  LOCAL_PROP_ARRAY = s_PROP_SHORT_3;
  VAL_1 : TSDOShort = 1597;
var
  obj : ISDODataObject;
begin
  obj := Create_Object();

  // these _does_ depend on the property's default
  CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isNull(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);

  obj.setShort(LOCAL_PROP, VAL_1);
    CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
    obj.setNull(LOCAL_PROP);
      CheckEquals(True,obj.isNull(LOCAL_PROP),LOCAL_PROP);
      CheckEquals(0,obj.getShort(LOCAL_PROP),LOCAL_PROP);
end;

procedure TSDOBaseDataObject_Test.property_default_value_short();
const
  VAL_1 : TSDOShort = 6547;
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_SHORT_1,sdo_namespace,SDOTypeDefaultTypeNames[ShortType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_SHORT_1);
        locProp.setDefault(VAL_1);

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  CheckEquals(VAL_1, locA.getShort(locProp), 'getShort');
  CheckEquals(False, locA.isSet(locProp), 'isSet');
end;

procedure TSDOBaseDataObject_Test.property_default_value_unset_short();
const
  VAL_1 : TSDOShort = 20123; VAL_2 : TSDOShort = -5648;
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_SHORT_1,sdo_namespace,SDOTypeDefaultTypeNames[ShortType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_SHORT_1);
        locProp.setDefault(TSDOShort(VAL_1));

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  locA.setShort(locProp, VAL_2);
    CheckEquals(True, locA.isSet(locProp), 'isSet');
    CheckEquals(VAL_2, locA.getShort(locProp), 'getShort');
    locA.unset(locProp);
      CheckEquals(False, locA.isSet(locProp), 'isSet');
      CheckEquals(VAL_1, locA.getShort(locProp), 'getShort');
end;
{$ENDIF HAS_SDO_SHORT}

{$IFDEF HAS_SDO_BYTES}
procedure TSDOBaseDataObject_Test.check_xpath_value(
  const AExpected: TSDOBytes;
  const AObject: ISDODataObject;
  const AXPath: string
);
begin
  CheckEquals(AExpected,AObject.getBytes(AXPath), AXPath);
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
procedure TSDOBaseDataObject_Test.check_xpath_value(
  const AExpected: TSDOChar;
  const AObject: ISDODataObject;
  const AXPath: string
);
begin
  CheckEquals(AExpected,AObject.getCharacter(AXPath), AXPath);
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
procedure TSDOBaseDataObject_Test.check_xpath_value_currency(
  const AExpected: TSDOCurrency;
  const AObject: ISDODataObject;
  const AXPath: string
);
begin
  CheckEqualsCurrency(AExpected,AObject.getCurrency(AXPath), 0.0001, AXPath);
end;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
procedure TSDOBaseDataObject_Test.check_xpath_value(
  const AExpected: TSDODouble;
  const AObject: ISDODataObject;
  const AXPath: string
);
begin
  CheckEquals(AExpected,AObject.getDouble(AXPath), 0.0001, AXPath);
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
procedure TSDOBaseDataObject_Test.check_xpath_value(
  const AExpected: TSDOFloat;
  const AObject: ISDODataObject;
  const AXPath: string
);
begin
  CheckEquals(AExpected,AObject.getFloat(AXPath), 0.0001, AXPath);
end;
{$ENDIF HAS_SDO_FLOAT}

{$IFDEF HAS_SDO_LONG}
procedure TSDOBaseDataObject_Test.check_xpath_value(
  const AExpected: TSDOLong;
  const AObject: ISDODataObject;
  const AXPath: string
);
begin
  CheckEquals(AExpected,AObject.getLong(AXPath), AXPath);
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure TSDOBaseDataObject_Test.check_xpath_value(
  const AExpected: TSDOShort;
  const AObject: ISDODataObject;
  const AXPath: string
);
begin
  CheckEquals(AExpected,AObject.getShort(AXPath), AXPath);
end;
{$ENDIF HAS_SDO_SHORT}

procedure TSDOBaseDataObject_Test.check_xpath_value(
  const AExpected: TSDOByte;
  const AObject: ISDODataObject;
  const AXPath: string
);
begin
  CheckEquals(AExpected,AObject.getByte(AXPath), AXPath);
end;

procedure TSDOBaseDataObject_Test.check_xpath_value(
  const AExpected: TSDODateTime;
  const AObject: ISDODataObject;
  const AXPath: string
);
begin
  CheckEquals(AExpected,AObject.getDate(AXPath), AXPath, False);
end;

procedure TSDOBaseDataObject_Test.get_byte_xpath();
const
  LOCAL_PROP_TYPE = ByteType;
  LOCAL_PROP_1_NAME = s_PROP_BYTE_1;
  LOCAL_PROP_2_NAME = s_PROP_BYTE_2;
  LOCAL_PROP_A_NAME = s_PROP_BYTE_A;
  LOCAL_PROP_B_NAME = s_PROP_BYTE_B;

  function local_create_factory() : ISDODataFactory;
  var
    res : ISDODataFactory;
    tfg : TTypeFlags;
  begin
    tfg := [];
    if is_open_type() then
      Include(tfg,tfIsOpen);
    res := TSDODataFactory.Create() as ISDODataFactory;
    res.AddType(s_URI_1,s_TYPE_1,tfg);
    res.AddType(s_URI_1,s_TYPE_2,tfg);
    res.AddType(s_URI_1,s_TYPE_3,tfg);

    res.addProperty(s_URI_1,s_TYPE_1,LOCAL_PROP_1_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_1,LOCAL_PROP_2_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_1,s_PROP_OBJ_CONT,s_URI_1,s_TYPE_2,[pfIsContainment]);

    res.addProperty(s_URI_1,s_TYPE_2,LOCAL_PROP_A_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_2,LOCAL_PROP_B_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);

    Result := res;
  end;

var
  locFac : ISDODataFactory;
  o1, o2 : ISDODataObject;
  o1_val_1, o1_val_2, o2_val_A, o2_val_B : TSDOByte;
begin
  o1_val_1 := RandomRange(Low(TSDOByte),High(TSDOByte));
  o1_val_2 := RandomRange(Low(TSDOByte),High(TSDOByte));
  o2_val_A := RandomRange(Low(TSDOByte),High(TSDOByte));
  o2_val_B := RandomRange(Low(TSDOByte),High(TSDOByte));
  locFac := local_create_factory();
  o1 := locFac.createNew(s_URI_1,s_TYPE_1);
  o1.setByte(LOCAL_PROP_1_NAME,o1_val_1);
  o1.setByte(LOCAL_PROP_2_NAME,o1_val_2);
    o2 := O1.createDataObject(s_PROP_OBJ_CONT);
      o2.setByte(LOCAL_PROP_A_NAME,o2_val_A);
      o2.setByte(LOCAL_PROP_B_NAME,o2_val_B);

  check_xpath_value(o1_val_1, o1,LOCAL_PROP_1_NAME);
  check_xpath_value(o1_val_2, o1, LOCAL_PROP_2_NAME);
  check_xpath_value(o2_val_A, o1, Format('%s/%s',[s_PROP_OBJ_CONT,LOCAL_PROP_A_NAME]));
  check_xpath_value(o2_val_B, o1, Format('%s/%s',[s_PROP_OBJ_CONT,LOCAL_PROP_B_NAME]));
  check_xpath_value(o1_val_1, o2, Format('%s/%s',['..',LOCAL_PROP_1_NAME]));
  check_xpath_value(o1_val_2, o2, Format('%s/%s',['..',LOCAL_PROP_2_NAME]));
end;

{$IFDEF HAS_SDO_CHAR}
procedure TSDOBaseDataObject_Test.get_char_xpath();
const
  LOCAL_PROP_TYPE = CharacterType;
  LOCAL_PROP_1_NAME = s_PROP_CHAR_1;
  LOCAL_PROP_2_NAME = s_PROP_CHAR_2;
  LOCAL_PROP_A_NAME = s_PROP_CHAR_A;
  LOCAL_PROP_B_NAME = s_PROP_CHAR_B;

  function local_create_factory() : ISDODataFactory;
  var
    res : ISDODataFactory;
    tfg : TTypeFlags;
  begin
    tfg := [];
    if is_open_type() then
      Include(tfg,tfIsOpen);
    res := TSDODataFactory.Create() as ISDODataFactory;
    res.AddType(s_URI_1,s_TYPE_1,tfg);
    res.AddType(s_URI_1,s_TYPE_2,tfg);
    res.AddType(s_URI_1,s_TYPE_3,tfg);

    res.addProperty(s_URI_1,s_TYPE_1,LOCAL_PROP_1_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_1,LOCAL_PROP_2_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_1,s_PROP_OBJ_CONT,s_URI_1,s_TYPE_2,[pfIsContainment]);

    res.addProperty(s_URI_1,s_TYPE_2,LOCAL_PROP_A_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_2,LOCAL_PROP_B_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);

    Result := res;
  end;

var
  locFac : ISDODataFactory;
  o1, o2 : ISDODataObject;
  o1_val_1, o1_val_2, o2_val_A, o2_val_B : TSDOChar;
begin
  o1_val_1 := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));
  o1_val_2 := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));
  o2_val_A := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));
  o2_val_B := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));
  locFac := local_create_factory();
  o1 := locFac.createNew(s_URI_1,s_TYPE_1);
  o1.setCharacter(LOCAL_PROP_1_NAME,o1_val_1);
  o1.setCharacter(LOCAL_PROP_2_NAME,o1_val_2);
    o2 := O1.createDataObject(s_PROP_OBJ_CONT);
      o2.setCharacter(LOCAL_PROP_A_NAME,o2_val_A);
      o2.setCharacter(LOCAL_PROP_B_NAME,o2_val_B);

  check_xpath_value(o1_val_1, o1,LOCAL_PROP_1_NAME);
  check_xpath_value(o1_val_2, o1, LOCAL_PROP_2_NAME);
  check_xpath_value(o2_val_A, o1, Format('%s/%s',[s_PROP_OBJ_CONT,LOCAL_PROP_A_NAME]));
  check_xpath_value(o2_val_B, o1, Format('%s/%s',[s_PROP_OBJ_CONT,LOCAL_PROP_B_NAME]));
  check_xpath_value(o1_val_1, o2, Format('%s/%s',['..',LOCAL_PROP_1_NAME]));
  check_xpath_value(o1_val_2, o2, Format('%s/%s',['..',LOCAL_PROP_2_NAME]));
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
procedure TSDOBaseDataObject_Test.get_currency_xpath();
const
  LOCAL_PROP_TYPE = CurrencyType;
  LOCAL_PROP_1_NAME = s_PROP_CURRENCY_1;
  LOCAL_PROP_2_NAME = s_PROP_CURRENCY_2;
  LOCAL_PROP_A_NAME = s_PROP_CURRENCY_A;
  LOCAL_PROP_B_NAME = s_PROP_CURRENCY_B;

  function local_create_factory() : ISDODataFactory;
  var
    res : ISDODataFactory;
    tfg : TTypeFlags;
  begin
    tfg := [];
    if is_open_type() then
      Include(tfg,tfIsOpen);
    res := TSDODataFactory.Create() as ISDODataFactory;
    res.AddType(s_URI_1,s_TYPE_1,tfg);
    res.AddType(s_URI_1,s_TYPE_2,tfg);
    res.AddType(s_URI_1,s_TYPE_3,tfg);

    res.addProperty(s_URI_1,s_TYPE_1,LOCAL_PROP_1_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_1,LOCAL_PROP_2_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_1,s_PROP_OBJ_CONT,s_URI_1,s_TYPE_2,[pfIsContainment]);

    res.addProperty(s_URI_1,s_TYPE_2,LOCAL_PROP_A_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_2,LOCAL_PROP_B_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);

    Result := res;
  end;

var
  locFac : ISDODataFactory;
  o1, o2 : ISDODataObject;
  o1_val_1, o1_val_2, o2_val_A, o2_val_B : TSDOCurrency;
begin
  o1_val_1 := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  o1_val_2 := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  o2_val_A := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  o2_val_B := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  locFac := local_create_factory();
  o1 := locFac.createNew(s_URI_1,s_TYPE_1);
  o1.setCurrency(LOCAL_PROP_1_NAME,o1_val_1);
  o1.setCurrency(LOCAL_PROP_2_NAME,o1_val_2);
    o2 := O1.createDataObject(s_PROP_OBJ_CONT);
      o2.setCurrency(LOCAL_PROP_A_NAME,o2_val_A);
      o2.setCurrency(LOCAL_PROP_B_NAME,o2_val_B);

  check_xpath_value_currency(o1_val_1, o1,LOCAL_PROP_1_NAME);
  check_xpath_value_currency(o1_val_2, o1, LOCAL_PROP_2_NAME);
  check_xpath_value_currency(o2_val_A, o1, Format('%s/%s',[s_PROP_OBJ_CONT,LOCAL_PROP_A_NAME]));
  check_xpath_value_currency(o2_val_B, o1, Format('%s/%s',[s_PROP_OBJ_CONT,LOCAL_PROP_B_NAME]));
  check_xpath_value_currency(o1_val_1, o2, Format('%s/%s',['..',LOCAL_PROP_1_NAME]));
  check_xpath_value_currency(o1_val_2, o2, Format('%s/%s',['..',LOCAL_PROP_2_NAME]));
end;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
procedure TSDOBaseDataObject_Test.get_double_xpath();
const
  LOCAL_PROP_TYPE = DoubleType;
  LOCAL_PROP_1_NAME = s_PROP_DOUBLE_1;
  LOCAL_PROP_2_NAME = s_PROP_DOUBLE_2;
  LOCAL_PROP_A_NAME = s_PROP_DOUBLE_A;
  LOCAL_PROP_B_NAME = s_PROP_DOUBLE_B;

  function local_create_factory() : ISDODataFactory;
  var
    res : ISDODataFactory;
    tfg : TTypeFlags;
  begin
    tfg := [];
    if is_open_type() then
      Include(tfg,tfIsOpen);
    res := TSDODataFactory.Create() as ISDODataFactory;
    res.AddType(s_URI_1,s_TYPE_1,tfg);
    res.AddType(s_URI_1,s_TYPE_2,tfg);
    res.AddType(s_URI_1,s_TYPE_3,tfg);

    res.addProperty(s_URI_1,s_TYPE_1,LOCAL_PROP_1_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_1,LOCAL_PROP_2_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_1,s_PROP_OBJ_CONT,s_URI_1,s_TYPE_2,[pfIsContainment]);

    res.addProperty(s_URI_1,s_TYPE_2,LOCAL_PROP_A_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_2,LOCAL_PROP_B_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);

    Result := res;
  end;

var
  locFac : ISDODataFactory;
  o1, o2 : ISDODataObject;
  o1_val_1, o1_val_2, o2_val_A, o2_val_B : TSDODouble;
begin
  o1_val_1 := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  o1_val_2 := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  o2_val_A := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  o2_val_B := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  locFac := local_create_factory();
  o1 := locFac.createNew(s_URI_1,s_TYPE_1);
  o1.setDouble(LOCAL_PROP_1_NAME,o1_val_1);
  o1.setDouble(LOCAL_PROP_2_NAME,o1_val_2);
    o2 := O1.createDataObject(s_PROP_OBJ_CONT);
      o2.setDouble(LOCAL_PROP_A_NAME,o2_val_A);
      o2.setDouble(LOCAL_PROP_B_NAME,o2_val_B);

  check_xpath_value(o1_val_1, o1,LOCAL_PROP_1_NAME);
  check_xpath_value(o1_val_2, o1, LOCAL_PROP_2_NAME);
  check_xpath_value(o2_val_A, o1, Format('%s/%s',[s_PROP_OBJ_CONT,LOCAL_PROP_A_NAME]));
  check_xpath_value(o2_val_B, o1, Format('%s/%s',[s_PROP_OBJ_CONT,LOCAL_PROP_B_NAME]));
  check_xpath_value(o1_val_1, o2, Format('%s/%s',['..',LOCAL_PROP_1_NAME]));
  check_xpath_value(o1_val_2, o2, Format('%s/%s',['..',LOCAL_PROP_2_NAME]));
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
procedure TSDOBaseDataObject_Test.get_float_xpath();
const
  LOCAL_PROP_TYPE = FloatType;
  LOCAL_PROP_1_NAME = s_PROP_FLOAT_1;
  LOCAL_PROP_2_NAME = s_PROP_FLOAT_2;
  LOCAL_PROP_A_NAME = s_PROP_FLOAT_A;
  LOCAL_PROP_B_NAME = s_PROP_FLOAT_B;

  function local_create_factory() : ISDODataFactory;
  var
    res : ISDODataFactory;
    tfg : TTypeFlags;
  begin
    tfg := [];
    if is_open_type() then
      Include(tfg,tfIsOpen);
    res := TSDODataFactory.Create() as ISDODataFactory;
    res.AddType(s_URI_1,s_TYPE_1,tfg);
    res.AddType(s_URI_1,s_TYPE_2,tfg);
    res.AddType(s_URI_1,s_TYPE_3,tfg);

    res.addProperty(s_URI_1,s_TYPE_1,LOCAL_PROP_1_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_1,LOCAL_PROP_2_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_1,s_PROP_OBJ_CONT,s_URI_1,s_TYPE_2,[pfIsContainment]);

    res.addProperty(s_URI_1,s_TYPE_2,LOCAL_PROP_A_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_2,LOCAL_PROP_B_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);

    Result := res;
  end;

var
  locFac : ISDODataFactory;
  o1, o2 : ISDODataObject;
  o1_val_1, o1_val_2, o2_val_A, o2_val_B : TSDOFloat;
begin
  o1_val_1 := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  o1_val_2 := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  o2_val_A := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  o2_val_B := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  locFac := local_create_factory();
  o1 := locFac.createNew(s_URI_1,s_TYPE_1);
  o1.setFloat(LOCAL_PROP_1_NAME,o1_val_1);
  o1.setFloat(LOCAL_PROP_2_NAME,o1_val_2);
    o2 := O1.createDataObject(s_PROP_OBJ_CONT);
      o2.setFloat(LOCAL_PROP_A_NAME,o2_val_A);
      o2.setFloat(LOCAL_PROP_B_NAME,o2_val_B);

  check_xpath_value(o1_val_1, o1,LOCAL_PROP_1_NAME);
  check_xpath_value(o1_val_2, o1, LOCAL_PROP_2_NAME);
  check_xpath_value(o2_val_A, o1, Format('%s/%s',[s_PROP_OBJ_CONT,LOCAL_PROP_A_NAME]));
  check_xpath_value(o2_val_B, o1, Format('%s/%s',[s_PROP_OBJ_CONT,LOCAL_PROP_B_NAME]));
  check_xpath_value(o1_val_1, o2, Format('%s/%s',['..',LOCAL_PROP_1_NAME]));
  check_xpath_value(o1_val_2, o2, Format('%s/%s',['..',LOCAL_PROP_2_NAME]));
end;
{$ENDIF HAS_SDO_FLOAT}

procedure TSDOBaseDataObject_Test.get_date_xpath();
const
  LOCAL_PROP_TYPE = DateTimeType;
  LOCAL_PROP_1_NAME = s_PROP_DATE_1;
  LOCAL_PROP_2_NAME = s_PROP_DATE_2;
  LOCAL_PROP_A_NAME = s_PROP_DATE_A;
  LOCAL_PROP_B_NAME = s_PROP_DATE_B;

  function local_create_factory() : ISDODataFactory;
  var
    res : ISDODataFactory;
    tfg : TTypeFlags;
  begin
    tfg := [];
    if is_open_type() then
      Include(tfg,tfIsOpen);
    res := TSDODataFactory.Create() as ISDODataFactory;
    res.AddType(s_URI_1,s_TYPE_1,tfg);
    res.AddType(s_URI_1,s_TYPE_2,tfg);
    res.AddType(s_URI_1,s_TYPE_3,tfg);

    res.addProperty(s_URI_1,s_TYPE_1,LOCAL_PROP_1_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_1,LOCAL_PROP_2_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_1,s_PROP_OBJ_CONT,s_URI_1,s_TYPE_2,[pfIsContainment]);

    res.addProperty(s_URI_1,s_TYPE_2,LOCAL_PROP_A_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_2,LOCAL_PROP_B_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);

    Result := res;
  end;

var
  locFac : ISDODataFactory;
  o1, o2 : ISDODataObject;
  o1_val_1, o1_val_2, o2_val_A, o2_val_B : TSDODateTime;
begin
  o1_val_1 := RandomDate();
  o1_val_2 := RandomDate();
  o2_val_A := RandomDate();
  o2_val_B := RandomDate();
  locFac := local_create_factory();
  o1 := locFac.createNew(s_URI_1,s_TYPE_1);
  o1.setDate(LOCAL_PROP_1_NAME,o1_val_1);
  o1.setDate(LOCAL_PROP_2_NAME,o1_val_2);
    o2 := O1.createDataObject(s_PROP_OBJ_CONT);
      o2.setDate(LOCAL_PROP_A_NAME,o2_val_A);
      o2.setDate(LOCAL_PROP_B_NAME,o2_val_B);

  check_xpath_value(o1_val_1, o1,LOCAL_PROP_1_NAME);
  check_xpath_value(o1_val_2, o1, LOCAL_PROP_2_NAME);
  check_xpath_value(o2_val_A, o1, Format('%s/%s',[s_PROP_OBJ_CONT,LOCAL_PROP_A_NAME]));
  check_xpath_value(o2_val_B, o1, Format('%s/%s',[s_PROP_OBJ_CONT,LOCAL_PROP_B_NAME]));
  check_xpath_value(o1_val_1, o2, Format('%s/%s',['..',LOCAL_PROP_1_NAME]));
  check_xpath_value(o1_val_2, o2, Format('%s/%s',['..',LOCAL_PROP_2_NAME]));
end;

{$IFDEF HAS_SDO_LONG}
procedure TSDOBaseDataObject_Test.get_long_xpath();
const
  LOCAL_PROP_TYPE = LongType;
  LOCAL_PROP_1_NAME = s_PROP_LONG_1;
  LOCAL_PROP_2_NAME = s_PROP_LONG_2;
  LOCAL_PROP_A_NAME = s_PROP_LONG_A;
  LOCAL_PROP_B_NAME = s_PROP_LONG_B;

  function local_create_factory() : ISDODataFactory;
  var
    res : ISDODataFactory;
    tfg : TTypeFlags;
  begin
    tfg := [];
    if is_open_type() then
      Include(tfg,tfIsOpen);
    res := TSDODataFactory.Create() as ISDODataFactory;
    res.AddType(s_URI_1,s_TYPE_1,tfg);
    res.AddType(s_URI_1,s_TYPE_2,tfg);
    res.AddType(s_URI_1,s_TYPE_3,tfg);

    res.addProperty(s_URI_1,s_TYPE_1,LOCAL_PROP_1_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_1,LOCAL_PROP_2_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_1,s_PROP_OBJ_CONT,s_URI_1,s_TYPE_2,[pfIsContainment]);

    res.addProperty(s_URI_1,s_TYPE_2,LOCAL_PROP_A_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_2,LOCAL_PROP_B_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);

    Result := res;
  end;

var
  locFac : ISDODataFactory;
  o1, o2 : ISDODataObject;
  o1_val_1, o1_val_2, o2_val_A, o2_val_B : TSDOLong;
begin
  o1_val_1 := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  o1_val_2 := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  o2_val_A := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  o2_val_B := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  locFac := local_create_factory();
  o1 := locFac.createNew(s_URI_1,s_TYPE_1);
  o1.setLong(LOCAL_PROP_1_NAME,o1_val_1);
  o1.setLong(LOCAL_PROP_2_NAME,o1_val_2);
    o2 := O1.createDataObject(s_PROP_OBJ_CONT);
      o2.setLong(LOCAL_PROP_A_NAME,o2_val_A);
      o2.setLong(LOCAL_PROP_B_NAME,o2_val_B);

  check_xpath_value(o1_val_1, o1,LOCAL_PROP_1_NAME);
  check_xpath_value(o1_val_2, o1, LOCAL_PROP_2_NAME);
  check_xpath_value(o2_val_A, o1, Format('%s/%s',[s_PROP_OBJ_CONT,LOCAL_PROP_A_NAME]));
  check_xpath_value(o2_val_B, o1, Format('%s/%s',[s_PROP_OBJ_CONT,LOCAL_PROP_B_NAME]));
  check_xpath_value(o1_val_1, o2, Format('%s/%s',['..',LOCAL_PROP_1_NAME]));
  check_xpath_value(o1_val_2, o2, Format('%s/%s',['..',LOCAL_PROP_2_NAME]));
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure TSDOBaseDataObject_Test.get_short_xpath();
const
  LOCAL_PROP_TYPE = ShortType;
  LOCAL_PROP_1_NAME = s_PROP_SHORT_1;
  LOCAL_PROP_2_NAME = s_PROP_SHORT_2;
  LOCAL_PROP_A_NAME = s_PROP_SHORT_A;
  LOCAL_PROP_B_NAME = s_PROP_SHORT_B;

  function local_create_factory() : ISDODataFactory;
  var
    res : ISDODataFactory;
    tfg : TTypeFlags;
  begin
    tfg := [];
    if is_open_type() then
      Include(tfg,tfIsOpen);
    res := TSDODataFactory.Create() as ISDODataFactory;
    res.AddType(s_URI_1,s_TYPE_1,tfg);
    res.AddType(s_URI_1,s_TYPE_2,tfg);
    res.AddType(s_URI_1,s_TYPE_3,tfg);

    res.addProperty(s_URI_1,s_TYPE_1,LOCAL_PROP_1_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_1,LOCAL_PROP_2_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_1,s_PROP_OBJ_CONT,s_URI_1,s_TYPE_2,[pfIsContainment]);

    res.addProperty(s_URI_1,s_TYPE_2,LOCAL_PROP_A_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_2,LOCAL_PROP_B_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);

    Result := res;
  end;

var
  locFac : ISDODataFactory;
  o1, o2 : ISDODataObject;
  o1_val_1, o1_val_2, o2_val_A, o2_val_B : TSDOShort;
begin
  o1_val_1 := RandomRange(Low(TSDOShort),High(TSDOShort));
  o1_val_2 := RandomRange(Low(TSDOShort),High(TSDOShort));
  o2_val_A := RandomRange(Low(TSDOShort),High(TSDOShort));
  o2_val_B := RandomRange(Low(TSDOShort),High(TSDOShort));
  locFac := local_create_factory();
  o1 := locFac.createNew(s_URI_1,s_TYPE_1);
  o1.setShort(LOCAL_PROP_1_NAME,o1_val_1);
  o1.setShort(LOCAL_PROP_2_NAME,o1_val_2);
    o2 := O1.createDataObject(s_PROP_OBJ_CONT);
      o2.setShort(LOCAL_PROP_A_NAME,o2_val_A);
      o2.setShort(LOCAL_PROP_B_NAME,o2_val_B);

  check_xpath_value(o1_val_1, o1,LOCAL_PROP_1_NAME);
  check_xpath_value(o1_val_2, o1, LOCAL_PROP_2_NAME);
  check_xpath_value(o2_val_A, o1, Format('%s/%s',[s_PROP_OBJ_CONT,LOCAL_PROP_A_NAME]));
  check_xpath_value(o2_val_B, o1, Format('%s/%s',[s_PROP_OBJ_CONT,LOCAL_PROP_B_NAME]));
  check_xpath_value(o1_val_1, o2, Format('%s/%s',['..',LOCAL_PROP_1_NAME]));
  check_xpath_value(o1_val_2, o2, Format('%s/%s',['..',LOCAL_PROP_2_NAME]));
end;
{$ENDIF HAS_SDO_SHORT}

procedure TSDOBaseDataObject_Test.check_xpath_value(
  const AExpected: TSDOBoolean;
  const AObject: ISDODataObject;
  const AXPath: string
);
begin
  CheckEquals(AExpected,AObject.getBoolean(AXPath), AXPath);
end;

procedure TSDOBaseDataObject_Test.get_bool_xpath();
const
  LOCAL_PROP_TYPE = BooleanType;
  LOCAL_PROP_1_NAME = s_PROP_BOOL_1;
  LOCAL_PROP_2_NAME = s_PROP_BOOL_2;
  LOCAL_PROP_A_NAME = s_PROP_BOOL_A;
  LOCAL_PROP_B_NAME = s_PROP_BOOL_B;

  function local_create_factory() : ISDODataFactory;
  var
    res : ISDODataFactory;
    tfg : TTypeFlags;
  begin
    tfg := [];
    if is_open_type() then
      Include(tfg,tfIsOpen);
    res := TSDODataFactory.Create() as ISDODataFactory;
    res.AddType(s_URI_1,s_TYPE_1,tfg);
    res.AddType(s_URI_1,s_TYPE_2,tfg);
    res.AddType(s_URI_1,s_TYPE_3,tfg);

    res.addProperty(s_URI_1,s_TYPE_1,LOCAL_PROP_1_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_1,LOCAL_PROP_2_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_1,s_PROP_OBJ_CONT,s_URI_1,s_TYPE_2,[pfIsContainment]);

    res.addProperty(s_URI_1,s_TYPE_2,LOCAL_PROP_A_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_2,LOCAL_PROP_B_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);

    Result := res;
  end;

var
  locFac : ISDODataFactory;
  o1, o2 : ISDODataObject;
  o1_val_1, o1_val_2, o2_val_A, o2_val_B : TSDOBoolean;
begin
  o1_val_1 := ( RandomRange(-1234,56789) mod 3 ) = 0;
  o1_val_2 := ( RandomRange(-1234,56789) mod 3 ) = 0;
  o2_val_A := ( RandomRange(-1234,56789) mod 3 ) = 0;
  o2_val_B := ( RandomRange(-1234,56789) mod 3 ) = 0;
  locFac := local_create_factory();
  o1 := locFac.createNew(s_URI_1,s_TYPE_1);
  o1.setBoolean(LOCAL_PROP_1_NAME,o1_val_1);
  o1.setBoolean(LOCAL_PROP_2_NAME,o1_val_2);
    o2 := O1.createDataObject(s_PROP_OBJ_CONT);
      o2.setBoolean(LOCAL_PROP_A_NAME,o2_val_A);
      o2.setBoolean(LOCAL_PROP_B_NAME,o2_val_B);

  check_xpath_value(o1_val_1, o1,LOCAL_PROP_1_NAME);
  check_xpath_value(o1_val_2, o1, LOCAL_PROP_2_NAME);
  check_xpath_value(o2_val_A, o1, Format('%s/%s',[s_PROP_OBJ_CONT,LOCAL_PROP_A_NAME]));
  check_xpath_value(o2_val_B, o1, Format('%s/%s',[s_PROP_OBJ_CONT,LOCAL_PROP_B_NAME]));
  check_xpath_value(o1_val_1, o2, Format('%s/%s',['..',LOCAL_PROP_1_NAME]));
  check_xpath_value(o1_val_2, o2, Format('%s/%s',['..',LOCAL_PROP_2_NAME]));
end;

procedure TSDOBaseDataObject_Test.check_xpath_value(
  const AExpected: TSDOString;
  const AObject: ISDODataObject;
  const AXPath: string
);
begin
  CheckEquals(AExpected,AObject.getString(AXPath), AXPath);
end;

procedure TSDOBaseDataObject_Test.get_string_xpath();
const
  LOCAL_PROP_TYPE = StringType;
  LOCAL_PROP_1_NAME = s_PROP_STR_1;
  LOCAL_PROP_2_NAME = s_PROP_STR_2;
  LOCAL_PROP_A_NAME = s_PROP_STR_A;
  LOCAL_PROP_B_NAME = s_PROP_STR_B;

  function local_create_factory() : ISDODataFactory;
  var
    res : ISDODataFactory;
    tfg : TTypeFlags;
  begin
    tfg := [];
    if is_open_type() then
      Include(tfg,tfIsOpen);
    res := TSDODataFactory.Create() as ISDODataFactory;
    res.AddType(s_URI_1,s_TYPE_1,tfg);
    res.AddType(s_URI_1,s_TYPE_2,tfg);
    res.AddType(s_URI_1,s_TYPE_3,tfg);

    res.addProperty(s_URI_1,s_TYPE_1,LOCAL_PROP_1_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_1,LOCAL_PROP_2_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_1,s_PROP_OBJ_CONT,s_URI_1,s_TYPE_2,[pfIsContainment]);

    res.addProperty(s_URI_1,s_TYPE_2,LOCAL_PROP_A_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);
    res.addProperty(s_URI_1,s_TYPE_2,LOCAL_PROP_B_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LOCAL_PROP_TYPE],[]);

    Result := res;
  end;

var
  locFac : ISDODataFactory;
  o1, o2 : ISDODataObject;
  o1_val_1, o1_val_2, o2_val_A, o2_val_B : TSDOString;
begin
  o1_val_1 := RandomString(RandomRange(0,1000));
  o1_val_2 := RandomString(RandomRange(0,1000));
  o2_val_A := RandomString(RandomRange(0,1000));
  o2_val_B := RandomString(RandomRange(0,1000));
  locFac := local_create_factory();
  o1 := locFac.createNew(s_URI_1,s_TYPE_1);
  o1.setString(LOCAL_PROP_1_NAME,o1_val_1);
  o1.setString(LOCAL_PROP_2_NAME,o1_val_2);
    o2 := O1.createDataObject(s_PROP_OBJ_CONT);
      o2.setString(LOCAL_PROP_A_NAME,o2_val_A);
      o2.setString(LOCAL_PROP_B_NAME,o2_val_B);

  check_xpath_value(o1_val_1, o1,LOCAL_PROP_1_NAME);
  check_xpath_value(o1_val_2, o1, LOCAL_PROP_2_NAME);
  check_xpath_value(o2_val_A, o1, Format('%s/%s',[s_PROP_OBJ_CONT,LOCAL_PROP_A_NAME]));
  check_xpath_value(o2_val_B, o1, Format('%s/%s',[s_PROP_OBJ_CONT,LOCAL_PROP_B_NAME]));
  check_xpath_value(o1_val_1, o2, Format('%s/%s',['..',LOCAL_PROP_1_NAME]));
  check_xpath_value(o1_val_2, o2, Format('%s/%s',['..',LOCAL_PROP_2_NAME]));
end;

procedure TSDOBaseDataObject_Test.date_procs();
const VAL_1 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
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
  obj : ISDODataObject;
  ok : Boolean;
begin
  SetConstants();
  obj := Create_Object();

  ok := False;
  try
    obj.setDate(obj.getProperty('qsdc'),VAL_1);
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.getDate(obj.getProperty('qsdc'));
  except
    on e : ESDOPropertyNotFoundException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOPropertyNotFoundException.');

  ok := False;
  try
    obj.setDate(obj.getProperty(s_PROP_DATE_3),VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Expect ESDOIllegalArgumentException.');

  obj.setDate(obj.getProperty(s_PROP_DATE_1),VAL_1);
  CheckEquals(VAL_1,obj.getDate(obj.getProperty(s_PROP_DATE_1)));
  CheckEquals(VAL_1,obj.getDate(s_PROP_DATE_1));
  CheckEquals(VAL_1,obj.getDate(obj.getPropertyIndex(obj.getProperty(s_PROP_DATE_1))));

  obj.setDate(obj.getProperty(s_PROP_DATE_1),VAL_2);
  CheckEquals(VAL_2,obj.getDate(obj.getProperty(s_PROP_DATE_1)));
  CheckEquals(VAL_2,obj.getDate(s_PROP_DATE_1));
  CheckEquals(VAL_2,obj.getDate(obj.getPropertyIndex(obj.getProperty(s_PROP_DATE_1))));

  obj.setDate(obj.getProperty(s_PROP_DATE_1),VAL_3);
  CheckEquals(VAL_3,obj.getDate(obj.getProperty(s_PROP_DATE_1)));
  CheckEquals(VAL_3,obj.getDate(s_PROP_DATE_1));
  CheckEquals(VAL_3,obj.getDate(obj.getPropertyIndex(obj.getProperty(s_PROP_DATE_1))));


  obj.setDate(obj.getProperty(s_PROP_DATE_2),VAL_1);
  CheckEquals(VAL_1,obj.getDate(obj.getProperty(s_PROP_DATE_2)));
  CheckEquals(VAL_1,obj.getDate(s_PROP_DATE_2));
  CheckEquals(VAL_1,obj.getDate(obj.getPropertyIndex(obj.getProperty(s_PROP_DATE_2))));

  obj.setDate(obj.getProperty(s_PROP_DATE_2),VAL_2);
  CheckEquals(VAL_2,obj.getDate(obj.getProperty(s_PROP_DATE_2)));
  CheckEquals(VAL_2,obj.getDate(s_PROP_DATE_2));
  CheckEquals(VAL_2,obj.getDate(obj.getPropertyIndex(obj.getProperty(s_PROP_DATE_2))));

  obj.setDate(obj.getProperty(s_PROP_DATE_2),VAL_3);
  CheckEquals(VAL_3,obj.getDate(obj.getProperty(s_PROP_DATE_2)));
  CheckEquals(VAL_3,obj.getDate(s_PROP_DATE_2));
  CheckEquals(VAL_3,obj.getDate(obj.getPropertyIndex(obj.getProperty(s_PROP_DATE_2))));
end;

procedure TSDOBaseDataObject_Test.CheckEquals(expected, actual: TSDODate;
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

procedure TSDOBaseDataObject_Test.date_multivalue();
const LOCAL_PROP = s_PROP_DATE_3; LOCAL_ARRAY_LEN = 100;
var
  val_list : array[0..Pred(LOCAL_ARRAY_LEN)] of TSDODate;

  procedure PrepareArray();
  var
    k : Integer;
  begin
    Randomize();
    for k := 0 to Pred(LOCAL_ARRAY_LEN) do begin
      val_list[k].Date := EncodeDateTime(
                            RandomRange(1900,2009),RandomRange(1,12),RandomRange(1,28),
                            RandomRange(1,23),RandomRange(0,59),RandomRange(0,59), RandomRange(0,999)
                          );
      val_list[k].HourOffset := RandomRange(0,10);
      val_list[k].MinuteOffset := RandomRange(0,59);
    end;
  end;

var
  obj : ISDODataObject;
  ls, ls1 : ISDODataObjectList;
  i : Integer;
  crs : ILinkedListCursor;
  ok : Boolean;
begin
  PrepareArray();
  obj := Create_Object();

  ok := False;
  try
    ls := obj.getList(s_PROP_DATE_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  Check(ok,Format('"%s" is not a multivalue property.',[s_PROP_DATE_1]));

  ls := obj.getList(LOCAL_PROP);
    Check(ls <> nil);
  ls1 := obj.getList(LOCAL_PROP);
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Name)');

  ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');
  ls1 := obj.getList(obj.getType().getPropertyIndex(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Index)');

  ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');
  ls1 := obj.getList(obj.getType().getProperty(LOCAL_PROP));
    CheckEquals(PtrInt(ls),PtrInt(ls1), 'getList(Name) <> getList(Property)');

  CheckEquals(0,ls.size(),'Size');
  for i := 0 to Pred(LOCAL_ARRAY_LEN) do begin
    ls.append(val_list[i]);
  end;

  crs := ls.getCursor();
  Check(crs <> nil,'ls.getCursor()');
  crs.Reset();
  CheckEquals(LOCAL_ARRAY_LEN,ls.size(),'Size');
  for i := 0 to Pred(LOCAL_ARRAY_LEN) do begin
    crs.MoveNext();
    CheckEquals(val_list[i],ls.getDate(),'append() <> getByte()');
  end;
end;

procedure TSDOBaseDataObject_Test.date_unset_isset();
const
  LOCAL_PROP = s_PROP_DATE_1;
  LOCAL_PROP_ARRAY = s_PROP_DATE_3;
  VAL_1 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
  VAL_2 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );

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
    PSDODate(@VAL_2)^ := d;
  end;

var
  obj : ISDODataObject;
  ls : ISDODataObjectList;
begin
  SetConstants();
  obj := Create_Object();

  CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 1');

  obj.setDate(LOCAL_PROP, VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
    obj.unset(LOCAL_PROP);
      CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);
      CheckEquals(obj.getProperty(LOCAL_PROP).getDateDefault(),obj.getDate(LOCAL_PROP),'"unSet" should restore the default value');
    obj.setDate(LOCAL_PROP, VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP),LOCAL_PROP);
      obj.unset(LOCAL_PROP);
        CheckEquals(False,obj.isSet(LOCAL_PROP),LOCAL_PROP);

  ls := obj.getList(LOCAL_PROP_ARRAY);
  Check(ls <> nil);
  CheckEquals(0,ls.size());
  CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 2');
  ls.append(VAL_1);
    CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 3');
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 4');
      CheckEquals(0,ls.size());
    ls.append(VAL_1);
    ls.append(VAL_2);
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 5');
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(True,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 6');
      CheckEquals(2,ls.size());
    obj.unset(LOCAL_PROP_ARRAY);
      CheckEquals(PtrInt(ls),PtrInt(obj.getList(LOCAL_PROP_ARRAY)));
      CheckEquals(False,obj.isSet(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY + ' 7');
      CheckEquals(0,ls.size());
end;

procedure TSDOBaseDataObject_Test.date_setnull_isnull();
const
  LOCAL_PROP = s_PROP_DATE_1;
  LOCAL_PROP_ARRAY = s_PROP_DATE_3;
  VAL_1 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
var
  obj : ISDODataObject;
begin
  PSDODate(@VAL_1)^.Date := Now();
  obj := Create_Object();

  // these _does_ depend on the property's default
  CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
  CheckEquals(False,obj.isNull(LOCAL_PROP_ARRAY),LOCAL_PROP_ARRAY);

  obj.setDate(LOCAL_PROP, VAL_1);
    CheckEquals(False,obj.isNull(LOCAL_PROP),LOCAL_PROP);
    obj.setNull(LOCAL_PROP);
      CheckEquals(True,obj.isNull(LOCAL_PROP),LOCAL_PROP);
      CheckEquals(ZERO_DATE,obj.getDate(LOCAL_PROP),LOCAL_PROP);
end;

procedure TSDOBaseDataObject_Test.property_default_value_date();
const
  VAL_1 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  PSDODate(@VAL_1)^.Date := Now();
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_DATE_1,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_DATE_1);
        locProp.setDefault(VAL_1);

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  CheckEquals(VAL_1, locA.getDate(locProp), 'getDate');
  CheckEquals(False, locA.isSet(locProp), 'isSet');
end;

procedure TSDOBaseDataObject_Test.property_default_value_unset_date();
const
  VAL_1 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
  VAL_2 : TSDODate = ( Date : 0; HourOffset : 3; MinuteOffset : 4; );
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  tfg : TTypeFlags;
begin
  PSDODate(@VAL_1)^.Date := Now();
  PSDODate(@VAL_2)^.Date := Date() - 100;
  tfg := [];
  if is_open_type() then
    Include(tfg,tfIsOpen);
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,tfg);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_DATE_1,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType],[]);
      locProp := locFactory.getType(s_URI_1,s_TYPE_1).getProperty(s_PROP_DATE_1);
        locProp.setDefault(VAL_1);

  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
  locA.setDate(locProp, VAL_2);
    CheckEquals(True, locA.isSet(locProp), 'isSet');
    CheckEquals(VAL_2, locA.getDate(locProp), 'getDate');
    locA.unset(locProp);
      CheckEquals(False, locA.isSet(locProp), 'isSet');
      CheckEquals(VAL_1, locA.getDate(locProp), 'getDate');
end;

{ TObserver_Test }

procedure TObserver_Test.ObserverInfo_create();
var
  locFactory : ISDODataFactory;
  locA : ISDODataObject;
  locProp : ISDOProperty;
  locRef : TObserverInfo;
begin
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,[]);
    locFactory.AddType(s_URI_1,s_TYPE_2,[]);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_OBJ_REF,s_URI_1,s_TYPE_2,[]);
  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
    locProp := locA.getProperty(s_PROP_OBJ_REF);
  locRef := TObserverInfo.Create(locA,locProp);
  try
    CheckEquals(PtrUInt(locA), PtrUInt(locRef.GetDataObject()));
    CheckEquals(PtrUInt(locProp), PtrUInt(locRef.GetRefProperty()));
  finally
    FreeAndNil(locRef);
  end;
end;

procedure TObserver_Test.ObserverList_add_find();
var
  locFactory : ISDODataFactory;
  locA, locB : ISDODataObject;
  locProp, locPropB : ISDOProperty;
  locRef : TObserverInfo;
  locRefList : TDataObjectObserverList;
begin
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
    locFactory.AddType(s_URI_1,s_TYPE_1,[]);
    locFactory.AddType(s_URI_1,s_TYPE_2,[]);
    locFactory.AddType(s_URI_1,s_TYPE_3,[]);
      locFactory.addProperty(s_URI_1,s_TYPE_1,s_PROP_OBJ_REF,s_URI_1,s_TYPE_2,[]);
      locFactory.addProperty(s_URI_1,s_TYPE_2,s_PROP_OBJ_REF,s_URI_1,s_TYPE_3,[]);
  locA := locFactory.createNew(s_URI_1,s_TYPE_1);
    locProp := locA.getProperty(s_PROP_OBJ_REF);
  locRefList := TDataObjectObserverList.Create();
  try
    CheckEquals(0, locRefList.GetCount());
    locRef := locRefList.Find(locA,locProp);
    CheckEquals(PtrUInt(0), PtrUInt(locRef));

    locRefList.Add(locA,locProp);
      CheckEquals(1, locRefList.GetCount());
      locRef := locRefList.Find(locA,locProp);
      CheckNotEquals(PtrUInt(0), PtrUInt(locRef));
      CheckEquals(PtrUInt(locA), PtrUInt(locRef.GetDataObject()));
      CheckEquals(PtrUInt(locProp), PtrUInt(locRef.GetRefProperty()));
      CheckEquals(PtrUInt(locRef), PtrUInt(locRefList.GetItem(0)));

      locRefList.Add(locA,locProp);
        CheckEquals(PtrUInt(locRef), PtrUInt(locRefList.Find(locA,locProp)));
        CheckEquals(1, locRefList.GetCount());

      locB := locFactory.createNew(s_URI_1,s_TYPE_2);
      locPropB := locB.getProperty(s_PROP_OBJ_REF);
      locRefList.Add(locB,locPropB);
        CheckEquals(2, locRefList.GetCount());
        locRef := locRefList.Find(locA,locProp);
          CheckNotEquals(PtrUInt(0), PtrUInt(locRef));
          CheckEquals(PtrUInt(locA), PtrUInt(locRef.GetDataObject()));
          CheckEquals(PtrUInt(locProp), PtrUInt(locRef.GetRefProperty()));
        locRef := locRefList.Find(locB,locPropB);
          CheckNotEquals(PtrUInt(0), PtrUInt(locRef));
          CheckEquals(PtrUInt(locB), PtrUInt(locRef.GetDataObject()));
          CheckEquals(PtrUInt(locPropB), PtrUInt(locRef.GetRefProperty()));

  finally
    FreeAndNil(locRefList);
  end;
end;

{ TSDODataObject_Test }

class function TSDODataObject_Test.is_open_type() : Boolean;
begin
  Result := False;
end;

{ TSDOOpenedDataObject_Test }

procedure TSDOOpenedDataObject_Test.addProperty();
const
  s_open_bool = 'open_bool_prop';
  s_open_byte = 'open_byte_prop';
  s_open_int = 'open_int_prop';
  s_open_str = 'open_str_prop';
  s_open_obj = 'open_obj_prop';
var
  locO : ISDODataObject;
  locOX : ISDODataObjectEx;
  val_b : TSDOBoolean;
  val_byte : TSDOByte;
  val_i : TSDOInteger;
  val_s : TSDOString;
  val_o : ISDODataObject;
  val_open_bool : TSDOBoolean;
  val_open_byte : TSDOByte;
  val_open_int : TSDOInteger;
  val_open_str : TSDOString;
  val_open_obj : ISDODataObject;
  staticPropCount : PtrInt;
begin
  Randomize();
  val_b := ( ( RandomRange(-123,456) mod 3 ) = 0 );
  val_byte := RandomRange(Low(TSDOByte),High(TSDOByte));
  val_i := RandomRange(-123,456);
  val_s := RandomString(123);
  val_o := FFactory.createNew(s_URI_1,s_TYPE_2);
  val_open_bool := ( ( RandomRange(-1234,4567) mod 2 ) = 0 );
  val_open_byte := RandomRange(Low(TSDOByte),High(TSDOByte));
  val_open_int := RandomRange(-12345,678910);
  val_open_str := RandomString(1234);
  val_open_obj := FFactory.createNew(s_URI_1,s_TYPE_2);

  locO := Create_Object();
    locO.setBoolean(s_PROP_BOOL_1,val_b);
    locO.setByte(s_PROP_BYTE_1,val_byte);
    locO.setInteger(s_PROP_INTEGER_1,val_i);
    locO.setString(s_PROP_STR_1,val_s);
    locO.setDataObject(s_PROP_OBJ_CONT,val_o);
  locOX := locO as ISDODataObjectEx;
    staticPropCount := locO.getInstanceProperties().getCount();
    locOX.addProperty(s_open_int,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[IntegerType]),[]);
      CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_int, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[IntegerType]), []);
    locO.setInteger(s_open_int,val_open_int);
      CheckEquals(val_b, locO.getBoolean(s_PROP_BOOL_1), s_PROP_BOOL_1);
      CheckEquals(val_i, locO.getInteger(s_PROP_INTEGER_1), s_PROP_INTEGER_1);
      CheckEquals(val_s, locO.getString(s_PROP_STR_1), s_PROP_STR_1);
      CheckEquals(PtrUInt(val_o), PtrUInt(locO.getDataObject(s_PROP_OBJ_CONT)), s_PROP_OBJ_CONT);
      CheckEquals(val_open_int, locO.getInteger(s_open_int), s_open_int);

    locOX.addProperty(s_open_bool,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[BooleanType]),[]);
      CheckEquals( ( staticPropCount + 2), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_bool, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[BooleanType]), []);
    locOX.addProperty(s_open_str,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[StringType]),[]);
      CheckEquals( ( staticPropCount + 3), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_str, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[StringType]), []);
    locO.setBoolean(s_open_bool,val_open_bool);
    locO.setString(s_open_str,val_open_str);
      CheckEquals(val_b, locO.getBoolean(s_PROP_BOOL_1), s_PROP_BOOL_1);
      CheckEquals(val_i, locO.getInteger(s_PROP_INTEGER_1), s_PROP_INTEGER_1);
      CheckEquals(val_s, locO.getString(s_PROP_STR_1), s_PROP_STR_1);
      CheckEquals(PtrUInt(val_o), PtrUInt(locO.getDataObject(s_PROP_OBJ_CONT)), s_PROP_OBJ_CONT);
      CheckEquals(val_open_str, locO.getString(s_open_str), s_open_str);

    locOX.addProperty(s_open_obj,FFactory.getType(s_URI_1,s_TYPE_2),[]);
      CheckEquals( ( staticPropCount + 4), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_obj, FFactory.getType(s_URI_1,s_TYPE_2), []);
    locO.setDataObject(s_open_obj,val_open_obj);
      CheckEquals(val_b, locO.getBoolean(s_PROP_BOOL_1), s_PROP_BOOL_1);
      CheckEquals(val_i, locO.getInteger(s_PROP_INTEGER_1), s_PROP_INTEGER_1);
      CheckEquals(val_s, locO.getString(s_PROP_STR_1), s_PROP_STR_1);
      CheckEquals(PtrUInt(val_o), PtrUInt(locO.getDataObject(s_PROP_OBJ_CONT)), s_PROP_OBJ_CONT);
      CheckEquals(val_open_bool, locO.getBoolean(s_open_bool), s_open_bool);
      CheckEquals(val_open_int, locO.getInteger(s_open_int), s_open_int);
      CheckEquals(val_open_str, locO.getString(s_open_str), s_open_str);
      CheckEquals(PtrUInt(val_open_obj), PtrUInt(locO.getDataObject(s_open_obj)), s_open_obj);

    locOX.addProperty(s_open_byte,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ByteType]),[]);
      CheckEquals( ( staticPropCount + 5), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_byte, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ByteType]), []);
    locO.setByte(s_open_byte,val_open_byte);
      CheckEquals(val_b, locO.getBoolean(s_PROP_BOOL_1), s_PROP_BOOL_1);
      CheckEquals(val_byte, locO.getByte(s_PROP_BYTE_1), s_PROP_BYTE_1);
      CheckEquals(val_i, locO.getInteger(s_PROP_INTEGER_1), s_PROP_INTEGER_1);
      CheckEquals(val_s, locO.getString(s_PROP_STR_1), s_PROP_STR_1);
      CheckEquals(PtrUInt(val_o), PtrUInt(locO.getDataObject(s_PROP_OBJ_CONT)), s_PROP_OBJ_CONT);
      CheckEquals(val_open_int, locO.getInteger(s_open_int), s_open_int);
      CheckEquals(val_open_byte, locO.getByte(s_open_byte), s_open_byte);
end;

procedure TSDOOpenedDataObject_Test.addProperty_byte();
const
  s_open_prop = 'open_prop';
  s_static_prop = s_PROP_BYTE_1;
  prop_type = ByteType;
var
  locO : ISDODataObject;
  locOX : ISDODataObjectEx;
  locVal : TSDOByte;
  val_open : TSDOByte;
  staticPropCount : PtrInt;
begin
  Randomize();
  locVal := RandomRange(Low(TSDOByte),High(TSDOByte));
  val_open := RandomRange(Low(TSDOByte),High(TSDOByte));

  locO := Create_Object();
  locOX := locO as ISDODataObjectEx;
    locO.setByte(s_static_prop,locVal);
    staticPropCount := locO.getInstanceProperties().getCount();
    locOX.addProperty(s_open_prop,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[prop_type]),[]);
      CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[prop_type]), []);
    locO.setByte(s_open_prop,val_open);
      CheckEquals(locVal, locO.getByte(s_static_prop), s_static_prop);
      CheckEquals(val_open, locO.getByte(s_open_prop), s_open_prop);
end;

procedure TSDOOpenedDataObject_Test.addProperty_date();
const
  s_open_prop = 'open_prop';
  s_static_prop = s_PROP_DATE_1;
  prop_type = DateTimeType;
var
  locO : ISDODataObject;
  locOX : ISDODataObjectEx;
  locVal : TSDODateTime;
  val_open : TSDODateTime;
  staticPropCount : PtrInt;
begin
  Randomize();
  locVal := RandomDate();
  val_open := RandomDate();

  locO := Create_Object();
  locOX := locO as ISDODataObjectEx;
    locO.setDate(s_static_prop,locVal);
    staticPropCount := locO.getInstanceProperties().getCount();
    locOX.addProperty(s_open_prop,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[prop_type]),[]);
      CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[prop_type]), []);
    locO.setDate(s_open_prop,val_open);
      CheckEquals(locVal, locO.getDate(s_static_prop), s_static_prop);
      CheckEquals(val_open, locO.getDate(s_open_prop), s_open_prop);
end;

procedure TSDOOpenedDataObject_Test.addProperty_error();
const
  s_open_bool = 'open_bool_prop';
  s_open_byte = 'open_byte_prop';
  s_open_int = 'open_int_prop';
  s_open_str = 'open_str_prop';
  s_open_obj = 'open_obj_prop';
var
  locO : ISDODataObject;
  locOX : ISDODataObjectEx;

  procedure check_add_2_time(const AName : string; const AType : ISDOType; const AFlags : TPropertyFlags);
  var
    ok : boolean;
  begin
    locOX.addProperty(AName,AType,AFlags);
    ok := False;
    try
      locOX.addProperty(AName,AType,AFlags);
    except
      on e : ESDODuplicatedItemException do
        ok := True;
    end;
    CheckEquals(True, ok);
  end;

begin
  locO := Create_Object();
  locOX := locO as ISDODataObjectEx;
    check_add_2_time(s_open_int,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[IntegerType]),[]);
    check_add_2_time(s_open_bool,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[BooleanType]),[]);
    check_add_2_time(s_open_byte,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ByteType]),[]);
    check_add_2_time(s_open_str,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[StringType]),[]);
    check_add_2_time(s_open_obj,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType]),[]);
end;

procedure TSDOOpenedDataObject_Test.addProperty_multi_value();
const
  s_open_bool = 'open_bool_prop';
  s_open_int = 'open_int_prop';
  s_open_str = 'open_str_prop';
  s_open_obj_cont = 'open_obj_prop_cont';
  s_open_obj_ref = 'open_obj_prop_ref';
var
  locO : ISDODataObject;
  locOX : ISDODataObjectEx;
  val_b : TSDOBoolean;
  val_i : TSDOInteger;
  val_s : TSDOString;
  val_open_bool : array of TSDOBoolean;
  val_open_int : array of TSDOInteger;
  val_open_str : array of TSDOString;
  val_open_obj_cont, val_open_obj_ref : array of ISDODataObject;
  staticPropCount : PtrInt;
  i, c : PtrInt;
  ls : ISDODataObjectList;
begin
  Randomize();
  val_b := ( ( RandomRange(-123,456) mod 3 ) = 0 );
  val_i := RandomRange(-123,456);
  val_s := RandomString(123);
  c := RandomRange(1,100);
  SetLength(val_open_bool,c);
  for i := 0 to Pred(Length(val_open_bool)) do
    val_open_bool[i] := ( ( RandomRange(-1234,4567) mod 3 ) = 0 );
  c := RandomRange(1,100);
  SetLength(val_open_int,c);
  for i := 0 to Pred(Length(val_open_int)) do
    val_open_int[i] := RandomRange(-1234, 546789);
  c := RandomRange(1,100);
  SetLength(val_open_str,c);
  for i := 0 to Pred(Length(val_open_str)) do
    val_open_str[i] := RandomString(RandomRange(0,100));
  c := RandomRange(1,100);
  SetLength(val_open_obj_cont,c);
  for i := 0 to Pred(Length(val_open_obj_cont)) do
    val_open_obj_cont[i] := FFactory.createNew(s_URI_1,s_TYPE_2);
  c := RandomRange(1,100);
  SetLength(val_open_obj_ref,c);
  for i := 0 to Pred(Length(val_open_obj_ref)) do
    val_open_obj_ref[i] := FFactory.createNew(s_URI_1,s_TYPE_2);

  try
    locO := Create_Object();
      locO.setBoolean(s_PROP_BOOL_1,val_b);
      locO.setInteger(s_PROP_INTEGER_1,val_i);
      locO.setString(s_PROP_STR_1,val_s);
    locOX := locO as ISDODataObjectEx;
      staticPropCount := locO.getInstanceProperties().getCount();
      locOX.addProperty(s_open_int,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[IntegerType]),[pfIsMany]);
        CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
        check_property(locO.getInstanceProperties(), s_open_int, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[IntegerType]), [pfIsMany]);
      ls := locO.getList(s_open_int);
        CheckNotEquals(PtrUInt(nil), PtrUInt(ls), 'getList(s_open_int)');
        for i := 0 to Pred(Length(val_open_int)) do
          ls.append(val_open_int[i]);
        CheckEquals(val_b, locO.getBoolean(s_PROP_BOOL_1), s_PROP_BOOL_1);
        CheckEquals(val_i, locO.getInteger(s_PROP_INTEGER_1), s_PROP_INTEGER_1);
        CheckEquals(val_s, locO.getString(s_PROP_STR_1), s_PROP_STR_1);
        for i := 0 to Pred(Length(val_open_int)) do
          CheckEquals(val_open_int[i], ls.getInteger(i), s_open_int);

      locOX.addProperty(s_open_bool,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[BooleanType]),[pfIsMany]);
        CheckEquals( ( staticPropCount + 2), locO.getInstanceProperties().getCount(), 'Property count');
        check_property(locO.getInstanceProperties(), s_open_bool, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[BooleanType]), [pfIsMany]);
      locOX.addProperty(s_open_str,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[StringType]),[pfIsMany]);
        CheckEquals( ( staticPropCount + 3), locO.getInstanceProperties().getCount(), 'Property count');
        check_property(locO.getInstanceProperties(), s_open_str, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[StringType]), [pfIsMany]);
      locOX.addProperty(s_open_obj_cont,FFactory.getType(s_URI_1,s_TYPE_2),[pfIsMany,pfIsContainment]);
        CheckEquals( ( staticPropCount + 4), locO.getInstanceProperties().getCount(), 'Property count');
        check_property(locO.getInstanceProperties(), s_open_obj_cont, FFactory.getType(s_URI_1,s_TYPE_2), [pfIsMany,pfIsContainment]);
      locOX.addProperty(s_open_obj_ref,FFactory.getType(s_URI_1,s_TYPE_2),[pfIsMany]);
        CheckEquals( ( staticPropCount + 5), locO.getInstanceProperties().getCount(), 'Property count');
        check_property(locO.getInstanceProperties(), s_open_obj_ref, FFactory.getType(s_URI_1,s_TYPE_2), [pfIsMany]);

      ls := locO.getList(s_open_bool);
        CheckNotEquals(PtrUInt(nil), PtrUInt(ls), 'getList(s_open_bool)');
        for i := 0 to Pred(Length(val_open_bool)) do
          ls.append(val_open_bool[i]);
      ls := locO.getList(s_open_str);
        CheckNotEquals(PtrUInt(nil), PtrUInt(ls), 'getList(s_open_str)');
        for i := 0 to Pred(Length(val_open_str)) do
          ls.append(val_open_str[i]);
      ls := locO.getList(s_open_obj_cont);
        CheckNotEquals(PtrUInt(nil), PtrUInt(ls), 'getList(s_open_obj_cont)');
        for i := 0 to Pred(Length(val_open_obj_cont)) do
          ls.append(val_open_obj_cont[i]);
      ls := locO.getList(s_open_obj_ref);
        CheckNotEquals(PtrUInt(nil), PtrUInt(ls), 'getList(s_open_obj_ref)');
        for i := 0 to Pred(Length(val_open_obj_ref)) do
          ls.append(val_open_obj_ref[i]);

        CheckEquals(val_b, locO.getBoolean(s_PROP_BOOL_1), s_PROP_BOOL_1);
        CheckEquals(val_i, locO.getInteger(s_PROP_INTEGER_1), s_PROP_INTEGER_1);
        CheckEquals(val_s, locO.getString(s_PROP_STR_1), s_PROP_STR_1);
        ls := locO.getList(s_open_bool);
          CheckNotEquals(PtrUInt(nil), PtrUInt(ls), 'getList(s_open_bool)');
          for i := 0 to Pred(Length(val_open_bool)) do
            CheckEquals(val_open_bool[i], ls.getBoolean(i), s_open_bool);
        ls := locO.getList(s_open_int);
          CheckNotEquals(PtrUInt(nil), PtrUInt(ls), 'getList(s_open_int)');
          for i := 0 to Pred(Length(val_open_int)) do
            CheckEquals(val_open_int[i], ls.getInteger(i), s_open_int);
        ls := locO.getList(s_open_str);
          CheckNotEquals(PtrUInt(nil), PtrUInt(ls), 'getList(s_open_str)');
          for i := 0 to Pred(Length(val_open_str)) do
            CheckEquals(val_open_str[i], ls.getString(i), s_open_str);
        ls := locO.getList(s_open_obj_ref);
          CheckNotEquals(PtrUInt(nil), PtrUInt(ls), 'getList(s_open_obj_ref)');
          for i := 0 to Pred(Length(val_open_obj_ref)) do begin
            CheckEquals(PtrUInt(val_open_obj_ref[i]), PtrUInt(ls.getDataObject(i)), s_open_obj_ref);
            CheckEquals(PtrUInt(nil), PtrUInt(ls.getDataObject(i).getContainer()), 'Containment, s_open_obj_ref');
          end;
        ls := locO.getList(s_open_obj_cont);
          CheckNotEquals(PtrUInt(nil), PtrUInt(ls), 'getList(s_open_obj_cont)');
          for i := 0 to Pred(Length(val_open_obj_cont)) do begin
            CheckEquals(PtrUInt(val_open_obj_cont[i]), PtrUInt(ls.getDataObject(i)), s_open_obj_cont);
            CheckEquals(PtrUInt(locO), PtrUInt(ls.getDataObject(i).getContainer()), 'Containment, s_open_obj_cont');
          end;

  finally
    SetLength(val_open_obj_cont,0);
    SetLength(val_open_obj_ref,0);
    SetLength(val_open_str,0);
    SetLength(val_open_int,0);
    SetLength(val_open_bool,0);
  end;
end;

procedure TSDOOpenedDataObject_Test.addProperty_multi_value_byte();
const
  s_open_prop = 'open_prop';
  s_static_prop = s_PROP_BYTE_1;
  item_data_type = ByteType;
var
  locO : ISDODataObject;
  locOX : ISDODataObjectEx;
  val_static : TSDOByte;
  val_open : array of TSDOByte;
  staticPropCount : PtrInt;
  i, c : PtrInt;
  ls : ISDODataObjectList;
begin
  Randomize();
  val_static := RandomRange(Low(TSDOByte),High(TSDOByte));
  c := RandomRange(1,100);
  SetLength(val_open,c);
  for i := 0 to Pred(Length(val_open)) do
    val_open[i] := RandomRange(Low(TSDOByte),High(TSDOByte));

  try
    locO := Create_Object();
      locO.setByte(s_static_prop,val_static);
    locOX := locO as ISDODataObjectEx;
      staticPropCount := locO.getInstanceProperties().getCount();
      locOX.addProperty(s_open_prop,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[item_data_type]),[pfIsMany]);
        CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
        check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[item_data_type]), [pfIsMany]);
      ls := locO.getList(s_open_prop);
        CheckNotEquals(PtrUInt(nil), PtrUInt(ls), 'getList(s_open_prop)');
        for i := 0 to Pred(Length(val_open)) do
          ls.append(val_open[i]);
        CheckEquals(val_static, locO.getByte(s_static_prop), s_static_prop);
        for i := 0 to Pred(Length(val_open)) do
          CheckEquals(val_open[i], ls.getByte(i), s_open_prop);

  finally
    SetLength(val_open,0);
  end;
end;

procedure TSDOOpenedDataObject_Test.addProperty_multi_value_date();
const
  s_open_prop = 'open_prop';
  s_static_prop = s_PROP_DATE_1;
  item_data_type = DateTimeType;
var
  locO : ISDODataObject;
  locOX : ISDODataObjectEx;
  val_static : TSDODateTime;
  val_open : array of TSDODateTime;
  staticPropCount : PtrInt;
  i, c : PtrInt;
  ls : ISDODataObjectList;
begin
  Randomize();
  val_static := RandomDate();
  c := RandomRange(1,100);
  SetLength(val_open,c);
  for i := 0 to Pred(Length(val_open)) do
    val_open[i] := RandomDate();

  try
    locO := Create_Object();
      locO.setDate(s_static_prop,val_static);
    locOX := locO as ISDODataObjectEx;
      staticPropCount := locO.getInstanceProperties().getCount();
      locOX.addProperty(s_open_prop,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[item_data_type]),[pfIsMany]);
        CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
        check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[item_data_type]), [pfIsMany]);
      ls := locO.getList(s_open_prop);
        CheckNotEquals(PtrUInt(nil), PtrUInt(ls), 'getList(s_open_prop)');
        for i := 0 to Pred(Length(val_open)) do
          ls.append(val_open[i]);
        CheckEquals(val_static, locO.getDate(s_static_prop), s_static_prop);
        for i := 0 to Pred(Length(val_open)) do
          CheckEquals(val_open[i], ls.getDate(i), s_open_prop);

  finally
    SetLength(val_open,0);
  end;
end;

procedure TSDOOpenedDataObject_Test.check_property(
  const APropList: ISDOPropertyList; const AName: string;
  const AType: ISDOType; const AFlags: TPropertyFlags
);
var
  pp : ISDOProperty;
begin
  pp := APropList.find(AName);
  CheckNotEquals(PtrUInt(nil), PtrUInt(pp), Format('Property not found : "%s".',[AName]));
  CheckEquals(AName, pp.getName());
  Check(AType.equals(pp.getType()), Format('Property type differs : "%s".',[AName]));
  CheckEquals((pfIsMany in AFlags), pp.isMany(), Format('Property isMany differs : "%s".',[AName]));
  CheckEquals((pfIsReadOnly in AFlags), pp.isReadOnly(), Format('Property isReadOnly differs : "%s".',[AName]));
  CheckEquals((pfIsContainment in AFlags), pp.isContainment(), Format('Property isContainment differs : "%s".',[AName]));
end;

procedure TSDOOpenedDataObject_Test.implicit_add_property();
const
  s_open_bool = 'open_bool_prop';
  s_open_int = 'open_int_prop';
  s_open_str = 'open_str_prop';
  s_open_obj_cont = 'open_obj_prop_cont';
  s_open_obj_ref = 'open_obj_prop_ref';
var
  locO : ISDODataObject;
  val_b : TSDOBoolean;
  val_i : TSDOInteger;
  val_s : TSDOString;
  val_o : ISDODataObject;
  val_open_bool : TSDOBoolean;
  val_open_int : TSDOInteger;
  val_open_str : TSDOString;
  val_open_obj_cont, val_open_obj_ref : ISDODataObject;
  staticPropCount : PtrInt;
begin
  Randomize();
  val_b := ( ( RandomRange(-123,456) mod 3 ) = 0 );
  val_i := RandomRange(-123,456);
  val_s := RandomString(123);
  val_o := FFactory.createNew(s_URI_1,s_TYPE_2);
  val_open_bool := ( ( RandomRange(-1234,4567) mod 2 ) = 0 );
  val_open_int := RandomRange(-12345,678910);
  val_open_str := RandomString(1234);
  val_open_obj_cont := FFactory.createNew(s_URI_1,s_TYPE_2);
  val_open_obj_ref := val_o;

  locO := Create_Object();
    locO.setBoolean(s_PROP_BOOL_1,val_b);
    locO.setInteger(s_PROP_INTEGER_1,val_i);
    locO.setString(s_PROP_STR_1,val_s);
    locO.setDataObject(s_PROP_OBJ_CONT,val_o);
    staticPropCount := locO.getInstanceProperties().getCount();
    locO.setInteger(s_open_int,val_open_int);
      CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_int, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[IntegerType]), []);
      CheckEquals(val_b, locO.getBoolean(s_PROP_BOOL_1), s_PROP_BOOL_1);
      CheckEquals(val_i, locO.getInteger(s_PROP_INTEGER_1), s_PROP_INTEGER_1);
      CheckEquals(val_s, locO.getString(s_PROP_STR_1), s_PROP_STR_1);
      CheckEquals(PtrUInt(val_o), PtrUInt(locO.getDataObject(s_PROP_OBJ_CONT)), s_PROP_OBJ_CONT);
      CheckEquals(val_open_int, locO.getInteger(s_open_int), s_open_int);

    locO.setBoolean(s_open_bool,val_open_bool);
      CheckEquals( ( staticPropCount + 2), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_bool, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[BooleanType]), []);
    locO.setString(s_open_str,val_open_str);
      CheckEquals( ( staticPropCount + 3), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_str, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[StringType]), []);
    locO.setDataObject(s_open_obj_cont,val_open_obj_cont);
      CheckEquals( ( staticPropCount + 4), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_obj_cont, FFactory.getType(s_URI_1,s_TYPE_2), [pfIsContainment]);
    locO.setDataObject(s_open_obj_ref,val_open_obj_ref);
      CheckEquals( ( staticPropCount + 5), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_obj_ref, FFactory.getType(s_URI_1,s_TYPE_2), []);

      CheckEquals(val_b, locO.getBoolean(s_PROP_BOOL_1), s_PROP_BOOL_1);
      CheckEquals(val_i, locO.getInteger(s_PROP_INTEGER_1), s_PROP_INTEGER_1);
      CheckEquals(val_s, locO.getString(s_PROP_STR_1), s_PROP_STR_1);
      CheckEquals(PtrUInt(val_o), PtrUInt(locO.getDataObject(s_PROP_OBJ_CONT)), s_PROP_OBJ_CONT);
      CheckEquals(val_open_bool, locO.getBoolean(s_open_bool), s_open_bool);
      CheckEquals(val_open_int, locO.getInteger(s_open_int), s_open_int);
      CheckEquals(val_open_str, locO.getString(s_open_str), s_open_str);
      CheckEquals(PtrUInt(val_open_obj_cont), PtrUInt(locO.getDataObject(s_open_obj_cont)), s_open_obj_cont);
      CheckEquals(PtrUInt(val_open_obj_ref), PtrUInt(locO.getDataObject(s_open_obj_ref)), s_open_obj_ref);
end;

procedure TSDOOpenedDataObject_Test.implicit_add_property_byte();
const
  s_static_prop = s_PROP_BYTE_1;
  s_open_prop = 'open_prop';
var
  locO : ISDODataObject;
  val_static : TSDOByte;
  val_open : TSDOByte;
  staticPropCount : PtrInt;
begin
  Randomize();
  val_static := RandomRange(Low(TSDOByte),High(TSDOByte));
  val_open := RandomRange(Low(TSDOByte),High(TSDOByte));

  locO := Create_Object();
    locO.setByte(s_static_prop,val_static);
    staticPropCount := locO.getInstanceProperties().getCount();
    locO.setByte(s_open_prop,val_open);
      CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ByteType]), []);
      CheckEquals(val_static, locO.getByte(s_static_prop), s_static_prop);
      CheckEquals(val_open, locO.getByte(s_open_prop), s_open_prop);
end;

procedure TSDOOpenedDataObject_Test.implicit_add_property_date();
const
  s_static_prop = s_PROP_DATE_1;
  s_open_prop = 'open_prop';
var
  locO : ISDODataObject;
  val_static, val_open : TSDODateTime;
  staticPropCount : PtrInt;
begin
  Randomize();
  val_static := RandomDate();
  val_open := RandomDate();

  locO := Create_Object();
    locO.setDate(s_static_prop,val_static);
    staticPropCount := locO.getInstanceProperties().getCount();
    locO.setDate(s_open_prop,val_open);
      CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[DateType]), []);
      CheckEquals(val_static, locO.getDate(s_static_prop), s_static_prop);
      CheckEquals(val_open, locO.getDate(s_open_prop), s_open_prop);
end;

{$IFDEF HAS_SDO_BYTES}
procedure TSDOOpenedDataObject_Test.addProperty_bytes();
const
  s_open_prop = 'open_prop';
  s_static_prop = s_PROP_BYTES_1;
  prop_type = BytesType;
  
var
  locVal : TSDOBytes;
  val_open : TSDOBytes;
  
  procedure SetConstants();
  var
    v : TSDOBytes;
    k : Integer;
  begin
    SetLength(v,100);
    for k := 0 to High(v) do
      v[k] := k mod High(Byte);
    locVal := v;
    v := nil;

    SetLength(v,200);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
    val_open := v;
  end;
  
var
  locO : ISDODataObject;
  locOX : ISDODataObjectEx;
  staticPropCount : PtrInt;
begin
  Randomize();
  SetConstants();

  locO := Create_Object();
  locOX := locO as ISDODataObjectEx;
    locO.setBytes(s_static_prop,locVal);
    staticPropCount := locO.getInstanceProperties().getCount();
    locOX.addProperty(s_open_prop,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[prop_type]),[]);
      CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[prop_type]), []);
    locO.setBytes(s_open_prop,val_open);
      CheckEquals(locVal, locO.getBytes(s_static_prop), s_static_prop);
      CheckEquals(val_open, locO.getBytes(s_open_prop), s_open_prop);
end;

procedure TSDOOpenedDataObject_Test.addProperty_multi_value_bytes();
const
  s_open_prop = 'open_prop';
  s_static_prop = s_PROP_BYTES_1;
  item_data_type = BytesType;
var
  val_static : TSDOBytes;
  val_open : array of TSDOBytes;
  
  procedure SetConstants();
  var
    v, e : TSDOBytes;
    k, h : Integer;
  begin
    SetLength(v,100);
    for k := 0 to High(v) do
      v[k] := k mod High(Byte);
    val_static := v;
    v := nil;

    SetLength(val_open,123);
    for k := 0 to High(val_open) do begin
      e := nil;
      SetLength(e,RandomRange(0,1000));
      if ( Length(e) > 0 ) then begin
        for h := Low(e) to High(e) do
          e[h] := RandomRange(Low(Byte),High(Byte));
      end;
      val_open[k] := e;
    end;
  end;
  
var
  locO : ISDODataObject;
  locOX : ISDODataObjectEx;  
  staticPropCount : PtrInt;
  i : PtrInt;
  ls : ISDODataObjectList;
begin
  Randomize();
  SetConstants();

  try
    locO := Create_Object();
      locO.setBytes(s_static_prop,val_static);
    locOX := locO as ISDODataObjectEx;
      staticPropCount := locO.getInstanceProperties().getCount();
      locOX.addProperty(s_open_prop,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[item_data_type]),[pfIsMany]);
        CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
        check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[item_data_type]), [pfIsMany]);
      ls := locO.getList(s_open_prop);
        CheckNotEquals(PtrUInt(nil), PtrUInt(ls), 'getList(s_open_prop)');
        for i := 0 to Pred(Length(val_open)) do
          ls.appendBytes(val_open[i]);
        CheckEquals(val_static, locO.getBytes(s_static_prop), s_static_prop);
        for i := 0 to Pred(Length(val_open)) do
          CheckEquals(val_open[i], ls.getBytes(i), s_open_prop);

  finally
    SetLength(val_open,0);
  end;
end;

procedure TSDOOpenedDataObject_Test.implicit_add_property_bytes();
const
  s_static_prop = s_PROP_BYTES_1;
  s_open_prop = 'open_prop';
var  
  val_open, val_static : TSDOBytes;
  
  procedure SetConstants();
  var
    v : TSDOBytes;
    k : Integer;
  begin
    SetLength(v,100);
    for k := 0 to High(v) do
      v[k] := k mod High(Byte);
    val_open := v;
    v := nil;

    SetLength(v,200);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
    val_static := v;
  end;
  
var
  locO : ISDODataObject;
  staticPropCount : PtrInt;
begin
  Randomize();
  SetConstants();

  locO := Create_Object();
    locO.setBytes(s_static_prop,val_static);
    staticPropCount := locO.getInstanceProperties().getCount();
    locO.setBytes(s_open_prop,val_open);
      CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[BytesType]), []);
      CheckEquals(val_static, locO.getBytes(s_static_prop), s_static_prop);
      CheckEquals(val_open, locO.getBytes(s_open_prop), s_open_prop);
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
procedure TSDOOpenedDataObject_Test.addProperty_char();
const
  s_open_prop = 'open_prop';
  s_static_prop = s_PROP_CHAR_1;
  prop_type = CharacterType;
var
  locO : ISDODataObject;
  locOX : ISDODataObjectEx;
  locVal : TSDOChar;
  val_open : TSDOChar;
  staticPropCount : PtrInt;
begin
  Randomize();
  locVal := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));
  val_open := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));

  locO := Create_Object();
  locOX := locO as ISDODataObjectEx;
    locO.setCharacter(s_static_prop,locVal);
    staticPropCount := locO.getInstanceProperties().getCount();
    locOX.addProperty(s_open_prop,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[prop_type]),[]);
      CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[prop_type]), []);
    locO.setCharacter(s_open_prop,val_open);
      CheckEquals(locVal, locO.getCharacter(s_static_prop), s_static_prop);
      CheckEquals(val_open, locO.getCharacter(s_open_prop), s_open_prop);
end;

procedure TSDOOpenedDataObject_Test.addProperty_multi_value_char();
const
  s_open_prop = 'open_prop';
  s_static_prop = s_PROP_CHAR_1;
  item_data_type = CharacterType;
var
  locO : ISDODataObject;
  locOX : ISDODataObjectEx;
  val_static : TSDOChar;
  val_open : array of TSDOChar;
  staticPropCount : PtrInt;
  i, c : PtrInt;
  ls : ISDODataObjectList;
begin
  Randomize();
  val_static := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));
  c := RandomRange(1,100);
  SetLength(val_open,c);
  for i := 0 to Pred(Length(val_open)) do
    val_open[i] := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));

  try
    locO := Create_Object();
      locO.setCharacter(s_static_prop,val_static);
    locOX := locO as ISDODataObjectEx;
      staticPropCount := locO.getInstanceProperties().getCount();
      locOX.addProperty(s_open_prop,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[item_data_type]),[pfIsMany]);
        CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
        check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[item_data_type]), [pfIsMany]);
      ls := locO.getList(s_open_prop);
        CheckNotEquals(PtrUInt(nil), PtrUInt(ls), 'getList(s_open_prop)');
        for i := 0 to Pred(Length(val_open)) do
          ls.append(val_open[i]);
        CheckEquals(val_static, locO.getCharacter(s_static_prop), s_static_prop);
        for i := 0 to Pred(Length(val_open)) do
          CheckEquals(val_open[i], ls.getCharacter(i), s_open_prop);

  finally
    SetLength(val_open,0);
  end;
end;

procedure TSDOOpenedDataObject_Test.implicit_add_property_char();
const
  s_static_prop = s_PROP_CHAR_1;
  s_open_prop = 'open_prop';
var
  locO : ISDODataObject;
  val_static : TSDOChar;
  val_open : TSDOChar;
  staticPropCount : PtrInt;
begin
  Randomize();
  val_static := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));
  val_open := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));

  locO := Create_Object();
    locO.setCharacter(s_static_prop,val_static);
    staticPropCount := locO.getInstanceProperties().getCount();
    locO.setCharacter(s_open_prop,val_open);
      CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[CharacterType]), []);
      CheckEquals(val_static, locO.getCharacter(s_static_prop), s_static_prop);
      CheckEquals(val_open, locO.getCharacter(s_open_prop), s_open_prop);
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
procedure TSDOOpenedDataObject_Test.addProperty_currency();
const
  s_open_prop = 'open_prop';
  s_static_prop = s_PROP_CURRENCY_1;
  prop_type = CurrencyType;
var
  locO : ISDODataObject;
  locOX : ISDODataObjectEx;
  locVal : TSDOCurrency;
  val_open : TSDOCurrency;
  staticPropCount : PtrInt;
begin
  Randomize();
  locVal := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  val_open := RandomRange(Low(TSDOInteger),High(TSDOInteger));

  locO := Create_Object();
  locOX := locO as ISDODataObjectEx;
    locO.setCurrency(s_static_prop,locVal);
    staticPropCount := locO.getInstanceProperties().getCount();
    locOX.addProperty(s_open_prop,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[prop_type]),[]);
      CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[prop_type]), []);
    locO.setCurrency(s_open_prop,val_open);
      CheckEqualsCurrency(locVal, locO.getCurrency(s_static_prop), s_static_prop);
      CheckEqualsCurrency(val_open, locO.getCurrency(s_open_prop), s_open_prop);
end;

procedure TSDOOpenedDataObject_Test.addProperty_multi_value_currency();
const
  s_open_prop = 'open_prop';
  s_static_prop = s_PROP_CURRENCY_1;
  item_data_type = CurrencyType;
var
  locO : ISDODataObject;
  locOX : ISDODataObjectEx;
  val_static : TSDOCurrency;
  val_open : array of TSDOCurrency;
  staticPropCount : PtrInt;
  i, c : PtrInt;
  ls : ISDODataObjectList;
begin
  Randomize();
  val_static := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  c := RandomRange(1,100);
  SetLength(val_open,c);
  for i := 0 to Pred(Length(val_open)) do
    val_open[i] := RandomRange(Low(TSDOInteger),High(TSDOInteger));

  try
    locO := Create_Object();
      locO.setCurrency(s_static_prop,val_static);
    locOX := locO as ISDODataObjectEx;
      staticPropCount := locO.getInstanceProperties().getCount();
      locOX.addProperty(s_open_prop,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[item_data_type]),[pfIsMany]);
        CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
        check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[item_data_type]), [pfIsMany]);
      ls := locO.getList(s_open_prop);
        CheckNotEquals(PtrUInt(nil), PtrUInt(ls), 'getList(s_open_prop)');
        for i := 0 to Pred(Length(val_open)) do
          ls.appendCurrency(val_open[i]);
        CheckEquals(val_static, locO.getCurrency(s_static_prop), s_static_prop);
        for i := 0 to Pred(Length(val_open)) do
          CheckEquals(val_open[i], ls.getCurrency(i), s_open_prop);

  finally
    SetLength(val_open,0);
  end;
end;

procedure TSDOOpenedDataObject_Test.implicit_add_property_currency();
const
  s_static_prop = s_PROP_CURRENCY_1;
  s_open_prop = 'open_prop';
var
  locO : ISDODataObject;
  val_static : TSDOCurrency;
  val_open : TSDOCurrency;
  staticPropCount : PtrInt;
begin
  Randomize();
  val_static := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  val_open := RandomRange(Low(TSDOInteger),High(TSDOInteger));

  locO := Create_Object();
    locO.setCurrency(s_static_prop,val_static);
    staticPropCount := locO.getInstanceProperties().getCount();
    locO.setCurrency(s_open_prop,val_open);
      CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[CurrencyType]), []);
      CheckEquals(val_static, locO.getCurrency(s_static_prop), s_static_prop);
      CheckEquals(val_open, locO.getCurrency(s_open_prop), s_open_prop);
end;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
procedure TSDOOpenedDataObject_Test.addProperty_double();
const
  s_open_prop = 'open_prop';
  s_static_prop = s_PROP_DOUBLE_1;
  prop_type = DoubleType;
var
  locO : ISDODataObject;
  locOX : ISDODataObjectEx;
  locVal : TSDODouble;
  val_open : TSDODouble;
  staticPropCount : PtrInt;
begin
  Randomize();
  locVal := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  val_open := RandomRange(Low(TSDOInteger),High(TSDOInteger));

  locO := Create_Object();
  locOX := locO as ISDODataObjectEx;
    locO.setDouble(s_static_prop,locVal);
    staticPropCount := locO.getInstanceProperties().getCount();
    locOX.addProperty(s_open_prop,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[prop_type]),[]);
      CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[prop_type]), []);
    locO.setDouble(s_open_prop,val_open);
      CheckEquals(locVal, locO.getDouble(s_static_prop), s_static_prop);
      CheckEquals(val_open, locO.getDouble(s_open_prop), s_open_prop);
end;

procedure TSDOOpenedDataObject_Test.addProperty_multi_value_double();
const
  s_open_prop = 'open_prop';
  s_static_prop = s_PROP_DOUBLE_1;
  item_data_type = DoubleType;
var
  locO : ISDODataObject;
  locOX : ISDODataObjectEx;
  val_static : TSDODouble;
  val_open : array of TSDODouble;
  staticPropCount : PtrInt;
  i, c : PtrInt;
  ls : ISDODataObjectList;
begin
  Randomize();
  val_static := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  c := RandomRange(1,100);
  SetLength(val_open,c);
  for i := 0 to Pred(Length(val_open)) do
    val_open[i] := RandomRange(Low(TSDOInteger),High(TSDOInteger));

  try
    locO := Create_Object();
      locO.setDouble(s_static_prop,val_static);
    locOX := locO as ISDODataObjectEx;
      staticPropCount := locO.getInstanceProperties().getCount();
      locOX.addProperty(s_open_prop,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[item_data_type]),[pfIsMany]);
        CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
        check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[item_data_type]), [pfIsMany]);
      ls := locO.getList(s_open_prop);
        CheckNotEquals(PtrUInt(nil), PtrUInt(ls), 'getList(s_open_prop)');
        for i := 0 to Pred(Length(val_open)) do
          ls.append(val_open[i]);
        CheckEquals(val_static, locO.getDouble(s_static_prop), s_static_prop);
        for i := 0 to Pred(Length(val_open)) do
          CheckEquals(val_open[i], ls.getDouble(i), s_open_prop);

  finally
    SetLength(val_open,0);
  end;
end;

procedure TSDOOpenedDataObject_Test.implicit_add_property_double();
const
  s_static_prop = s_PROP_DOUBLE_1;
  s_open_prop = 'open_prop';
var
  locO : ISDODataObject;
  val_static : TSDODouble;
  val_open : TSDODouble;
  staticPropCount : PtrInt;
begin
  Randomize();
  val_static := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  val_open := RandomRange(Low(TSDOInteger),High(TSDOInteger));

  locO := Create_Object();
    locO.setDouble(s_static_prop,val_static);
    staticPropCount := locO.getInstanceProperties().getCount();
    locO.setDouble(s_open_prop,val_open);
      CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[DoubleType]), []);
      CheckEquals(val_static, locO.getDouble(s_static_prop), s_static_prop);
      CheckEquals(val_open, locO.getDouble(s_open_prop), s_open_prop);
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
procedure TSDOOpenedDataObject_Test.addProperty_float();
const
  s_open_prop = 'open_prop';
  s_static_prop = s_PROP_FLOAT_1;
  prop_type = FloatType;
var
  locO : ISDODataObject;
  locOX : ISDODataObjectEx;
  locVal : TSDOFloat;
  val_open : TSDOFloat;
  staticPropCount : PtrInt;
begin
  Randomize();
  locVal := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  val_open := RandomRange(Low(TSDOInteger),High(TSDOInteger));

  locO := Create_Object();
  locOX := locO as ISDODataObjectEx;
    locO.setFloat(s_static_prop,locVal);
    staticPropCount := locO.getInstanceProperties().getCount();
    locOX.addProperty(s_open_prop,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[prop_type]),[]);
      CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[prop_type]), []);
    locO.setFloat(s_open_prop,val_open);
      CheckEquals(locVal, locO.getFloat(s_static_prop), s_static_prop);
      CheckEquals(val_open, locO.getFloat(s_open_prop), s_open_prop);
end;

procedure TSDOOpenedDataObject_Test.addProperty_multi_value_float();
const
  s_open_prop = 'open_prop';
  s_static_prop = s_PROP_FLOAT_1;
  item_data_type = FloatType;
var
  locO : ISDODataObject;
  locOX : ISDODataObjectEx;
  val_static : TSDOFloat;
  val_open : array of TSDOFloat;
  staticPropCount : PtrInt;
  i, c : PtrInt;
  ls : ISDODataObjectList;
begin
  Randomize();
  val_static := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  c := RandomRange(1,100);
  SetLength(val_open,c);
  for i := 0 to Pred(Length(val_open)) do
    val_open[i] := RandomRange(Low(TSDOInteger),High(TSDOInteger));

  try
    locO := Create_Object();
      locO.setFloat(s_static_prop,val_static);
    locOX := locO as ISDODataObjectEx;
      staticPropCount := locO.getInstanceProperties().getCount();
      locOX.addProperty(s_open_prop,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[item_data_type]),[pfIsMany]);
        CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
        check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[item_data_type]), [pfIsMany]);
      ls := locO.getList(s_open_prop);
        CheckNotEquals(PtrUInt(nil), PtrUInt(ls), 'getList(s_open_prop)');
        for i := 0 to Pred(Length(val_open)) do
          ls.append(val_open[i]);
        CheckEquals(val_static, locO.getFloat(s_static_prop), s_static_prop);
        for i := 0 to Pred(Length(val_open)) do
          CheckEquals(val_open[i], ls.getFloat(i), s_open_prop);

  finally
    SetLength(val_open,0);
  end;
end;

procedure TSDOOpenedDataObject_Test.implicit_add_property_float();
const
  s_static_prop = s_PROP_FLOAT_1;
  s_open_prop = 'open_prop';
var
  locO : ISDODataObject;
  val_static : TSDOFloat;
  val_open : TSDOFloat;
  staticPropCount : PtrInt;
begin
  Randomize();
  val_static := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  val_open := RandomRange(Low(TSDOInteger),High(TSDOInteger));

  locO := Create_Object();
    locO.setFloat(s_static_prop,val_static);
    staticPropCount := locO.getInstanceProperties().getCount();
    locO.setFloat(s_open_prop,val_open);
      CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[FloatType]), []);
      CheckEquals(val_static, locO.getFloat(s_static_prop), s_static_prop);
      CheckEquals(val_open, locO.getFloat(s_open_prop), s_open_prop);
end;
{$ENDIF HAS_SDO_FLOAT}

{$IFDEF HAS_SDO_LONG}
procedure TSDOOpenedDataObject_Test.addProperty_long();
const
  s_open_prop = 'open_prop';
  s_static_prop = s_PROP_LONG_1;
  prop_type = LongType;
var
  locO : ISDODataObject;
  locOX : ISDODataObjectEx;
  locVal : TSDOLong;
  val_open : TSDOLong;
  staticPropCount : PtrInt;
begin
  Randomize();
  locVal := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  val_open := RandomRange(Low(TSDOInteger),High(TSDOInteger));

  locO := Create_Object();
  locOX := locO as ISDODataObjectEx;
    locO.setLong(s_static_prop,locVal);
    staticPropCount := locO.getInstanceProperties().getCount();
    locOX.addProperty(s_open_prop,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[prop_type]),[]);
      CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[prop_type]), []);
    locO.setLong(s_open_prop,val_open);
      CheckEquals(locVal, locO.getLong(s_static_prop), s_static_prop);
      CheckEquals(val_open, locO.getLong(s_open_prop), s_open_prop);
end;

procedure TSDOOpenedDataObject_Test.addProperty_multi_value_long();
const
  s_open_prop = 'open_prop';
  s_static_prop = s_PROP_LONG_1;
  item_data_type = LongType;
var
  locO : ISDODataObject;
  locOX : ISDODataObjectEx;
  val_static : TSDOLong;
  val_open : array of TSDOLong;
  staticPropCount : PtrInt;
  i, c : PtrInt;
  ls : ISDODataObjectList;
begin
  Randomize();
  val_static := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  c := RandomRange(1,100);
  SetLength(val_open,c);
  for i := 0 to Pred(Length(val_open)) do
    val_open[i] := RandomRange(Low(TSDOInteger),High(TSDOInteger));

  try
    locO := Create_Object();
      locO.setLong(s_static_prop,val_static);
    locOX := locO as ISDODataObjectEx;
      staticPropCount := locO.getInstanceProperties().getCount();
      locOX.addProperty(s_open_prop,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[item_data_type]),[pfIsMany]);
        CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
        check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[item_data_type]), [pfIsMany]);
      ls := locO.getList(s_open_prop);
        CheckNotEquals(PtrUInt(nil), PtrUInt(ls), 'getList(s_open_prop)');
        for i := 0 to Pred(Length(val_open)) do
          ls.append(val_open[i]);
        CheckEquals(val_static, locO.getLong(s_static_prop), s_static_prop);
        for i := 0 to Pred(Length(val_open)) do
          CheckEquals(val_open[i], ls.getLong(i), s_open_prop);

  finally
    SetLength(val_open,0);
  end;
end;

procedure TSDOOpenedDataObject_Test.implicit_add_property_long();
const
  s_static_prop = s_PROP_LONG_1;
  s_open_prop = 'open_prop';
var
  locO : ISDODataObject;
  val_static : TSDOLong;
  val_open : TSDOLong;
  staticPropCount : PtrInt;
begin
  Randomize();
  val_static := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  val_open := RandomRange(Low(TSDOInteger),High(TSDOInteger));

  locO := Create_Object();
    locO.setLong(s_static_prop,val_static);
    staticPropCount := locO.getInstanceProperties().getCount();
    locO.setLong(s_open_prop,val_open);
      CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[LongType]), []);
      CheckEquals(val_static, locO.getLong(s_static_prop), s_static_prop);
      CheckEquals(val_open, locO.getLong(s_open_prop), s_open_prop);
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure TSDOOpenedDataObject_Test.addProperty_short();
const
  s_open_prop = 'open_prop';
  s_static_prop = s_PROP_SHORT_1;
  prop_type = ShortType;
var
  locO : ISDODataObject;
  locOX : ISDODataObjectEx;
  locVal : TSDOShort;
  val_open : TSDOShort;
  staticPropCount : PtrInt;
begin
  Randomize();
  locVal := RandomRange(Low(TSDOShort),High(TSDOShort));
  val_open := RandomRange(Low(TSDOShort),High(TSDOShort));

  locO := Create_Object();
  locOX := locO as ISDODataObjectEx;
    locO.setShort(s_static_prop,locVal);
    staticPropCount := locO.getInstanceProperties().getCount();
    locOX.addProperty(s_open_prop,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[prop_type]),[]);
      CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[prop_type]), []);
    locO.setShort(s_open_prop,val_open);
      CheckEquals(locVal, locO.getShort(s_static_prop), s_static_prop);
      CheckEquals(val_open, locO.getShort(s_open_prop), s_open_prop);
end;

procedure TSDOOpenedDataObject_Test.addProperty_multi_value_short();
const
  s_open_prop = 'open_prop';
  s_static_prop = s_PROP_SHORT_1;
  item_data_type = ShortType;
var
  locO : ISDODataObject;
  locOX : ISDODataObjectEx;
  val_static : TSDOShort;
  val_open : array of TSDOShort;
  staticPropCount : PtrInt;
  i, c : PtrInt;
  ls : ISDODataObjectList;
begin
  Randomize();
  val_static := RandomRange(Low(TSDOShort),High(TSDOShort));
  c := RandomRange(1,100);
  SetLength(val_open,c);
  for i := 0 to Pred(Length(val_open)) do
    val_open[i] := RandomRange(Low(TSDOShort),High(TSDOShort));

  try
    locO := Create_Object();
      locO.setShort(s_static_prop,val_static);
    locOX := locO as ISDODataObjectEx;
      staticPropCount := locO.getInstanceProperties().getCount();
      locOX.addProperty(s_open_prop,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[item_data_type]),[pfIsMany]);
        CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
        check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[item_data_type]), [pfIsMany]);
      ls := locO.getList(s_open_prop);
        CheckNotEquals(PtrUInt(nil), PtrUInt(ls), 'getList(s_open_prop)');
        for i := 0 to Pred(Length(val_open)) do
          ls.append(val_open[i]);
        CheckEquals(val_static, locO.getShort(s_static_prop), s_static_prop);
        for i := 0 to Pred(Length(val_open)) do
          CheckEquals(val_open[i], ls.getShort(i), s_open_prop);

  finally
    SetLength(val_open,0);
  end;
end;

procedure TSDOOpenedDataObject_Test.implicit_add_property_short();
const
  s_static_prop = s_PROP_SHORT_1;
  s_open_prop = 'open_prop';
var
  locO : ISDODataObject;
  val_static : TSDOShort;
  val_open : TSDOShort;
  staticPropCount : PtrInt;
begin
  Randomize();
  val_static := RandomRange(Low(TSDOShort),High(TSDOShort));
  val_open := RandomRange(Low(TSDOShort),High(TSDOShort));

  locO := Create_Object();
    locO.setShort(s_static_prop,val_static);
    staticPropCount := locO.getInstanceProperties().getCount();
    locO.setShort(s_open_prop,val_open);
      CheckEquals( ( staticPropCount + 1), locO.getInstanceProperties().getCount(), 'Property count');
      check_property(locO.getInstanceProperties(), s_open_prop, FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ShortType]), []);
      CheckEquals(val_static, locO.getShort(s_static_prop), s_static_prop);
      CheckEquals(val_open, locO.getShort(s_open_prop), s_open_prop);
end;
{$ENDIF HAS_SDO_SHORT}

class function TSDOOpenedDataObject_Test.is_open_type() : Boolean;
begin
  Result := True;
end;

initialization
  RegisterTest(TSDODataObject_Test.GetTestSuitePath(),TSDODataObject_Test.Suite);
  RegisterTest(TSDOOpenedDataObject_Test.GetTestSuitePath(),TSDOOpenedDataObject_Test.Suite);
  RegisterTest('object\observer', TObserver_Test.Suite);

end.
