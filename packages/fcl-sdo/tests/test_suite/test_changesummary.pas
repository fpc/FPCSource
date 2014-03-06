{$INCLUDE sdo_global.inc}
unit test_changesummary;

interface
uses
  SysUtils, Classes, Contnrs
{$IFDEF FPC}
  ,fpcunit, testutils, testregistry
{$ENDIF}
{$IFNDEF FPC}
  ,TestFrameWork
{$ENDIF}
  , test_suite_utils, sdo_types, sdo, sdo_changesummary;

type

  TValueSetting_Test = class(TWstBaseTest)
  private
    class function CreateFactory() : ISDODataFactory;
  protected
    procedure CheckEquals(expected, actual: TSDODate; msg: string = ''; const AStrict : Boolean = True); overload;
  published
    procedure create_boolean();
    procedure create_byte();
    procedure create_date();
    procedure create_integer();
    procedure create_string();
{$IFDEF HAS_SDO_BYTES}
    procedure create_bytes();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure create_char();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure create_currency();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure create_double();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure create_float();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure create_long();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure create_short();
{$ENDIF HAS_SDO_SHORT}
  end;

  (*TDicho_Test = class(TTestCase)
  published
    procedure find();
    procedure build();
  end; *)

  TSDOSettingList_Test = class(TWstBaseTest)
  private
    class function CreateFactory() : ISDODataFactory;
  published
    procedure size();
    procedure insert();
    procedure append();
    procedure remove();
    procedure getItem();
  end;

  TSDOChangedDataObjectList_Test = class(TWstBaseTest)
  private
    class function CreateFactory() : ISDODataFactory;
    class function CreateObject() : ISDOChangedDataObjectListEx;
  published
    procedure size();

    procedure append();
    procedure find();
  end;

  TChangeRecorder_Test = class(TWstBaseTest)
  protected
    FRecorder : TChangeRecorder;
    FChangeSummary : ISDOChangeSummary;
    FFactory : ISDODataFactory;
  protected
    procedure CheckEquals(expected, actual: TSDODate; msg: string = ''; const AStrict : Boolean = True); overload;
    procedure SetUp(); override;
    procedure TearDown(); override;
    procedure InitRecorder();
  protected
    function CreateRecorder() : TChangeRecorder;
    class function Create_Factory() : ISDODataFactory;
    procedure record_int(const ADataObject : ISDODataObject; const APropName : string);
    procedure record_bool(const ADataObject : ISDODataObject; const APropName : string);
    procedure record_byte(const ADataObject : ISDODataObject; const APropName : string);
{$IFDEF HAS_SDO_BYTES}
    procedure record_bytes(const ADataObject : ISDODataObject; const APropName : string);
{$ENDIF HAS_SDO_BYTES}
    procedure record_date(const ADataObject : ISDODataObject; const APropName : string);
    procedure record_string(const ADataObject : ISDODataObject; const APropName : string);
    procedure record_obj(const ADataObject : ISDODataObject; const APropName : string);
{$IFDEF HAS_SDO_CHAR}
    procedure record_char(const ADataObject : ISDODataObject; const APropName : string);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure record_currency(const ADataObject : ISDODataObject; const APropName : string);
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure record_double(const ADataObject : ISDODataObject; const APropName : string);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure record_float(const ADataObject : ISDODataObject; const APropName : string);
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure record_long(const ADataObject : ISDODataObject; const APropName : string);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure record_short(const ADataObject : ISDODataObject; const APropName : string);
{$ENDIF HAS_SDO_SHORT}

    procedure record_list_int(const ADataObject : ISDODataObject; const APropName : string);
  published
    procedure multi_call_int();
    procedure multi_call_boolean();
    procedure multi_call_byte();
    procedure multi_call_date();
{$IFDEF HAS_SDO_BYTES}
    procedure multi_call_bytes();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure multi_call_char();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure multi_call_currency();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure multi_call_double();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure multi_call_float();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure multi_call_long();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure multi_call_short();
{$ENDIF HAS_SDO_SHORT}
    procedure multi_call_string();


    procedure int_change();
    procedure bool_change();
    procedure byte_change();
    procedure date_change();
{$IFDEF HAS_SDO_BYTES}
    procedure bytes_change();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure char_change();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure currency_change();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure double_change();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure float_change();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure long_change();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure short_change();
{$ENDIF HAS_SDO_SHORT}
    procedure string_change();

    procedure object_change_contained_prop();
    procedure object_change_referenced_prop();
  end;

  TSDOChangeSummary_Test = class(TWstBaseTest)
  protected
    FRecorder : TChangeRecorder;
    FFactory : ISDODataFactory;
    FFactoryX : ISDODataFactory;
    FChangeSummary : ISDOChangeSummary;
  protected
    procedure CheckEquals(expected, actual: TSDODate; msg: string = ''; const AStrict : Boolean = True); overload;
    procedure SetUp(); override;
    procedure TearDown(); override;
    procedure InitRecorder();
  protected
    function CreateRecorder() : TChangeRecorder;
    class function Create_Factory() : ISDODataFactory;
    class function Create_FactoryX() : ISDODataFactory;
  private
    procedure check_value(
      const AObj : ISDODataObject;
      const AProp : ISDOProperty;
      const AValue : TValueSetting
    );
  published
    procedure logging_state();
    procedure logging_state_2();
    procedure isCreated();
    procedure isCreated_create_delete();
    procedure isDeleted();
    procedure isDeleted_nested();
    procedure isModified();

    procedure getChangedDataObjects();
    procedure getChangedDataObjects_contained_delete();
    procedure getChangedDataObjects_multi_value_prop();

    procedure getOldValues();
    procedure getOldValues_bool();
    procedure getOldValues_byte();
    procedure getOldValues_date();
    procedure getOldValues_integer();
    procedure getOldValues_object_prop();
    procedure getOldValues_string();
{$IFDEF HAS_SDO_BYTES}
    procedure getOldValues_bytes();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure getOldValues_char();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure getOldValues_currency();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure getOldValues_double();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure getOldValues_float();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure getOldValues_long();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure getOldValues_short();
{$ENDIF HAS_SDO_SHORT}

    procedure getOldValue();
    procedure getOldValue_bool();
    procedure getOldValue_byte();
    procedure getOldValue_date();
    procedure getOldValue_integer();
    procedure getOldValue_string();
{$IFDEF HAS_SDO_BYTES}
    procedure getOldValue_bytes();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure getOldValue_char();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure getOldValue_currency();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure getOldValue_double();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure getOldValue_float();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure getOldValue_long();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure getOldValue_short();
{$ENDIF HAS_SDO_SHORT}


    procedure getOldValue_object_prop();
    procedure getOldContainer();
    procedure getOldContainmentProperty();

    procedure undoChanges_simple() ;
    procedure undoChanges_simple_bool() ;
    procedure undoChanges_simple_byte() ;
    procedure undoChanges_simple_date() ;
    procedure undoChanges_simple_integer() ;
    procedure undoChanges_simple_string() ;
{$IFDEF HAS_SDO_BYTES}
    procedure undoChanges_simple_bytes();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure undoChanges_simple_char();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure undoChanges_simple_currency();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure undoChanges_simple_double();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure undoChanges_simple_float();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure undoChanges_simple_long();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure undoChanges_simple_short();
{$ENDIF HAS_SDO_SHORT}


    procedure undoChanges_simple_unset() ;
    procedure undoChanges_simple_unset_bool() ;
    procedure undoChanges_simple_unset_byte() ;
    procedure undoChanges_simple_unset_date() ;
    procedure undoChanges_simple_unset_integer() ;
    procedure undoChanges_simple_unset_string() ;
{$IFDEF HAS_SDO_BYTES}
    procedure undoChanges_simple_unset_bytes();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure undoChanges_simple_unset_char();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure undoChanges_simple_unset_currency();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure undoChanges_simple_unset_double();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure undoChanges_simple_unset_float();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure undoChanges_simple_unset_long();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure undoChanges_simple_unset_short();
{$ENDIF HAS_SDO_SHORT}


    procedure undoChanges_simple_setnull() ;
    procedure undoChanges_simple_setnull_bool() ;
    procedure undoChanges_simple_setnull_byte() ;
    procedure undoChanges_simple_setnull_date() ;
    procedure undoChanges_simple_setnull_integer() ;
    procedure undoChanges_simple_setnull_string() ;
{$IFDEF HAS_SDO_BYTES}
    procedure undoChanges_simple_setnull_bytes();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure undoChanges_simple_setnull_char();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure undoChanges_simple_setnull_currency();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure undoChanges_simple_setnull_double();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure undoChanges_simple_setnull_float();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure undoChanges_simple_setnull_long();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure undoChanges_simple_setnull_short();
{$ENDIF HAS_SDO_SHORT}


    procedure undoChanges_object_contained_startWith_OBJ_OBJ();
    procedure undoChanges_object_contained_startWith_OBJ_OBJ_OBJ() ;
    procedure undoChanges_object_contained_startWith_NIL_OBJ_OBJ() ;
    procedure undoChanges_object_referenced_startWIth_OBJ_OBJ() ;
    procedure undoChanges_object_referenced_startWith_NIL_OBJ() ;
    procedure undoChanges_delete_contained_object();

    procedure getOldXpath();
    procedure getOldXpath_1();
    procedure getOldValues_created_object_settinglist_is_empty();
    procedure getOldXpath_nested_deleted_object();
  end;

  TSDODataObjectCS_Test = class(TWstBaseTest)
  private
    FFactory : ISDODataFactory;
  protected
    class function Create_Factory() : ISDODataFactory;
  protected
    procedure SetUp(); override;
    procedure TearDown(); override;
    procedure CheckEquals(expected, actual: TSDODate; msg: string = ''; const AStrict : Boolean = True); overload;
    procedure check_bool_logging(const AObj : ISDODataObject; const APropName : string);
    procedure check_byte_logging(const AObj : ISDODataObject; const APropName : string);
    procedure check_date_logging(const AObj : ISDODataObject; const APropName : string);
    procedure check_int_logging(const AObj : ISDODataObject; const APropName : string);
    procedure check_string_logging(const AObj : ISDODataObject; const APropName : string);
{$IFDEF HAS_SDO_BYTES}
    procedure check_bytes_logging(const AObj : ISDODataObject; const APropName : string);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure check_char_logging(const AObj : ISDODataObject; const APropName : string);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure check_currency_logging(const AObj : ISDODataObject; const APropName : string);
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure check_double_logging(const AObj : ISDODataObject; const APropName : string);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure check_float_logging(const AObj : ISDODataObject; const APropName : string);
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure check_long_logging(const AObj : ISDODataObject; const APropName : string);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure check_short_logging(const AObj : ISDODataObject; const APropName : string);
{$ENDIF HAS_SDO_SHORT}
  published
    procedure create_default_props();
    procedure logging_bool();
    procedure logging_byte();
    procedure logging_date();
    procedure logging_int();
    procedure logging_string();
{$IFDEF HAS_SDO_BYTES}
    procedure logging_bytes();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure logging_char();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure logging_currency();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure logging_double();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure logging_float();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure logging_long();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure logging_short();
{$ENDIF HAS_SDO_SHORT}

    procedure getChangeSummary();
  end;

  TSDOChangeSummaryMultiValueProps_Test = class(TWstBaseTest)
  private
    FFactory : ISDODataFactory;
  protected
    procedure SetUp(); override;
    procedure TearDown(); override;
    procedure CheckEquals(expected, actual: TSDODate; msg: string = ''; const AStrict : Boolean = True); overload;
  published
    procedure getOldValues_bool();
    procedure undoChanges_bool();
    procedure undoChanges_bool_deleted();
    procedure undoChanges_nested_bool();

    procedure getOldValues_byte();
    procedure undoChanges_byte();
    procedure undoChanges_byte_deleted();
    procedure undoChanges_nested_byte();

    procedure getOldValues_date();
    procedure undoChanges_date();
    procedure undoChanges_date_deleted();
    procedure undoChanges_nested_date();

    procedure getOldValues_integer();
    procedure undoChanges_integer();
    procedure undoChanges_integer_deleted();
    procedure undoChanges_nested_integer();

    procedure getOldValues_objects();
    procedure undoChanges_object();
    procedure undoChanges_object_deleted();
    procedure undoChanges_nested_object();

    procedure getOldValues_string();
    procedure undoChanges_string();
    procedure undoChanges_string_deleted();
    procedure undoChanges_nested_string();

{$IFDEF HAS_SDO_BYTES}
    procedure getOldValues_bytes();
    procedure undoChanges_bytes();
    procedure undoChanges_bytes_deleted();
    procedure undoChanges_nested_bytes();
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
    procedure getOldValues_char();
    procedure undoChanges_char();
    procedure undoChanges_char_deleted();
    procedure undoChanges_nested_char();
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
    procedure getOldValues_currency();
    procedure undoChanges_currency();
    procedure undoChanges_currency_deleted();
    procedure undoChanges_nested_currency();
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
    procedure getOldValues_double();
    procedure undoChanges_double();
    procedure undoChanges_double_deleted();
    procedure undoChanges_nested_double();
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
    procedure getOldValues_float();
    procedure undoChanges_float();
    procedure undoChanges_float_deleted();
    procedure undoChanges_nested_float();
{$ENDIF HAS_SDO_FLOAT}

{$IFDEF HAS_SDO_LONG}
    procedure getOldValues_long();
    procedure undoChanges_long();
    procedure undoChanges_long_deleted();
    procedure undoChanges_nested_long();
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
    procedure getOldValues_short();
    procedure undoChanges_short();
    procedure undoChanges_short_deleted();
    procedure undoChanges_nested_short();
{$ENDIF HAS_SDO_SHORT}

    procedure isDeleted();
    procedure isDeleted_nested();

    procedure getChangedDataObjects_contained_delete_multiprop_1();
    procedure getChangedDataObjects_contained_delete_multiprop_2();
    procedure getChangedDataObjects_contained_delete_multiprop_3();
  end;

implementation

uses
  sdo_datafactory, Math,
  sdo_consts, DateUtils, sdo_date_utils;

const
  s_uri              = 'urn-test';
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
  s_object_ref_prop      = 'object_ref_prop';
  s_changesummary_prop = 'change_summary_prop';

  s_Employee = 'Employee';
  s_EmployeeType = 'EmployeeType';
  s_Department = 'Department';
  s_location = 'location';
  s_manager = 'manager';
  s_name = 'name';
  s_number = 'number';
  s_sn = 'SN';

function CastExtractor(const AList : TObjectList; const AIndex : PtrInt) : PtrInt;
begin
  Result := PtrInt(AList[AIndex]);
end;

function sortComparer(Item1, Item2: Pointer): Integer;
begin
  Result := PtrInt(Item2) - PtrInt(Item1);
end;

function SortInterfaceList(const AIL : IInterfaceList) : IInterfaceList;
var
  ls : TList;
  i : PtrInt;
begin
  Result := TInterfaceList.Create();
  if ( Assigned(AIL) and ( AIL.Count > 0 ) ) then begin
    ls := TList.Create();
    try
      for i := 0 to Pred(AIL.Count) do begin
        ls.Add(Pointer(AIL.Items[i] as IInterface));
      end;
      ls.Sort(@sortComparer);
      for i := 0 to Pred(AIL.Count) do begin
        Result.Add(IInterface(ls[i]));
      end;
    finally
      ls.Free();
    end;
  end;
end;


{ TValueSetting_Test }

procedure TValueSetting_Test.CheckEquals(expected, actual: TSDODate; msg: string; const AStrict: Boolean);
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

class function TValueSetting_Test.CreateFactory() : ISDODataFactory;
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
begin
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
  Add_ObjectA(s_uri);

  Result := locFactory;
end;

procedure TValueSetting_Test.create_boolean();
var
  ok : Boolean;
  boolVal : TSDOBoolean;
  fact : ISDODataFactory;
  objectAType : ISDOType;
  localObj : TValueSetting;
  prp : ISDOProperty;
begin
  fact := CreateFactory();
  objectAType := fact.getType(s_uri,s_type_object_A);
  ok := False;
  try
    TValueSetting.Create(False,False,boolVal,nil,0);
  except
    on e : ESDOIllegalArgumentException do
      ok := True;
  end;
  CheckEquals(True,ok,'Invalid property parametter');

  // simple value
  prp := objectAType.getProperty(s_bool_prop) as ISDOProperty;
  boolVal := False;
  localObj := TValueSetting.Create(True,True,boolVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( False, localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( TSDOConvertHelper.BoolToByte(localObj.getBooleanValue()), localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.BoolToInteger(localObj.getBooleanValue()), localObj.getIntegerValue(), 'getIntegerValue');
    CheckEquals( TSDOConvertHelper.BoolToString(localObj.getBooleanValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,boolVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( False, localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( TSDOConvertHelper.BoolToByte(localObj.getBooleanValue()), localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.BoolToInteger(localObj.getBooleanValue()), localObj.getIntegerValue(), 'getIntegerValue');
    CheckEquals( TSDOConvertHelper.BoolToString(localObj.getBooleanValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  boolVal := False;
  localObj := TValueSetting.Create(True,False,boolVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( boolVal, localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( TSDOConvertHelper.BoolToByte(localObj.getBooleanValue()), localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.BoolToInteger(localObj.getBooleanValue()), localObj.getIntegerValue(), 'getIntegerValue');
    CheckEquals( TSDOConvertHelper.BoolToString(localObj.getBooleanValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  boolVal := True;
  localObj := TValueSetting.Create(True,False,boolVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( boolVal, localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( TSDOConvertHelper.BoolToByte(localObj.getBooleanValue()), localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.BoolToInteger(localObj.getBooleanValue()), localObj.getIntegerValue(), 'getIntegerValue');
    CheckEquals( TSDOConvertHelper.BoolToString(localObj.getBooleanValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  // multi value
  prp := objectAType.getProperty(s_bool_propList) as ISDOProperty;
  boolVal := True;
  localObj := TValueSetting.Create(True,True,boolVal,prp,12);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 12, localObj.getIndex(), 'getIndex' );
    CheckEquals( False, localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( TSDOConvertHelper.BoolToByte(localObj.getBooleanValue()), localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.BoolToInteger(localObj.getBooleanValue()), localObj.getIntegerValue(), 'getIntegerValue');
    CheckEquals( TSDOConvertHelper.BoolToString(localObj.getBooleanValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  boolVal := True;
  localObj := TValueSetting.Create(False,True,boolVal,prp,23);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 23, localObj.getIndex(), 'getIndex' );
    CheckEquals( False, localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( TSDOConvertHelper.BoolToByte(localObj.getBooleanValue()), localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.BoolToInteger(localObj.getBooleanValue()), localObj.getIntegerValue(), 'getIntegerValue');
    CheckEquals( TSDOConvertHelper.BoolToString(localObj.getBooleanValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  boolVal := False;
  localObj := TValueSetting.Create(True,False,boolVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( boolVal, localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( TSDOConvertHelper.BoolToByte(localObj.getBooleanValue()), localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.BoolToInteger(localObj.getBooleanValue()), localObj.getIntegerValue(), 'getIntegerValue');
    CheckEquals( TSDOConvertHelper.BoolToString(localObj.getBooleanValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  boolVal := True;
  localObj := TValueSetting.Create(True,False,boolVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( boolVal, localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( TSDOConvertHelper.BoolToByte(localObj.getBooleanValue()), localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.BoolToInteger(localObj.getBooleanValue()), localObj.getIntegerValue(), 'getIntegerValue');
    CheckEquals( TSDOConvertHelper.BoolToString(localObj.getBooleanValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;
end;

procedure TValueSetting_Test.create_byte();
var
  ok : Boolean;
  locVal : TSDOByte;
  fact : ISDODataFactory;
  objectAType : ISDOType;
  localObj : TValueSetting;
  prp : ISDOProperty;
begin
  locVal := 123;
  fact := CreateFactory();
  objectAType := fact.getType(s_uri,s_type_object_A);
  ok := False;
  try
    TValueSetting.Create(False,False,locVal,nil,0);
  except
    on e : ESDOIllegalArgumentException do
      ok := True;
  end;
  CheckEquals(True,ok,'Invalid property parametter');

  // simple value
  prp := objectAType.getProperty(s_byte_prop) as ISDOProperty;
  localObj := TValueSetting.Create(True,True,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.ByteToBool(localObj.getByteValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( localObj.getByteValue(), localObj.getIntegerValue(), 'getIntegerValue');
    CheckEquals( TSDOConvertHelper.ByteToString(localObj.getByteValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.ByteToBool(localObj.getByteValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( localObj.getByteValue(), localObj.getIntegerValue(), 'getIntegerValue');
    CheckEquals( TSDOConvertHelper.ByteToString(localObj.getByteValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.ByteToBool(localObj.getByteValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( localObj.getByteValue(), localObj.getIntegerValue(), 'getIntegerValue');
    CheckEquals( TSDOConvertHelper.ByteToString(localObj.getByteValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := 67;
  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.ByteToBool(localObj.getByteValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( localObj.getByteValue(), localObj.getIntegerValue(), 'getIntegerValue');
    CheckEquals( TSDOConvertHelper.ByteToString(localObj.getByteValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  // multi value
  prp := objectAType.getProperty(s_byte_propList) as ISDOProperty;
  localObj := TValueSetting.Create(True,True,locVal,prp,12);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 12, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.ByteToBool(localObj.getByteValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( localObj.getByteValue(), localObj.getIntegerValue(), 'getIntegerValue');
    CheckEquals( TSDOConvertHelper.ByteToString(localObj.getByteValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,locVal,prp,23);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 23, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.ByteToBool(localObj.getByteValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( localObj.getByteValue(), localObj.getIntegerValue(), 'getIntegerValue');
    CheckEquals( TSDOConvertHelper.ByteToString(localObj.getByteValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := 34;
  localObj := TValueSetting.Create(True,False,locVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.ByteToBool(localObj.getByteValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( localObj.getByteValue(), localObj.getIntegerValue(), 'getIntegerValue');
    CheckEquals( TSDOConvertHelper.ByteToString(localObj.getByteValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;
end;

{$IFDEF HAS_SDO_BYTES}
procedure TValueSetting_Test.create_bytes();
const
  PROP_NAME = s_bytes_prop;
  LIST_PROP_NAME = s_bytes_propList;
  VAL_1 : array[0..2] of TSDOByte = ( 2, 24, 12 );
  VAL_2 : array[0..5] of TSDOByte = ( 1, 07, 6, 2, 24, 12 );
  VAL_3 : array[0..1] of TSDOByte = ( 24,6 );


  function LocalCopy(const AValue : array of TSDOByte) : TSDOBytes;
  begin
    SetLength(Result,Length(AValue));
    if ( Length(Result) > 0 ) then
      Move(AValue[0],Result[0],Length(Result));
  end;

var
  ok : Boolean;
  locVal : TSDOBytes;
  fact : ISDODataFactory;
  objectAType : ISDOType;
  localObj : TValueSetting;
  prp : ISDOProperty;
begin
  locVal := LocalCopy(VAL_1);
  fact := CreateFactory();
  objectAType := fact.getType(s_uri,s_type_object_A);
  ok := False;
  try
    TValueSetting.Create(False,False,locVal,nil,0);
  except
    on e : ESDOIllegalArgumentException do
      ok := True;
  end;
  CheckEquals(True,ok,'Invalid property parametter');

  // simple value
  prp := objectAType.getProperty(PROP_NAME) as ISDOProperty;
  localObj := TValueSetting.Create(True,True,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( nil, localObj.getBytesValue(), 'getBytesValue');
    CheckEquals( TSDOConvertHelper.BytesToString(localObj.getBytesValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( nil, localObj.getBytesValue(), 'getBytesValue');
    CheckEquals( TSDOConvertHelper.BytesToString(localObj.getBytesValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getBytesValue(), 'getBytesValue');
    CheckEquals( TSDOConvertHelper.BytesToString(localObj.getBytesValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := LocalCopy(VAL_2);
  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getBytesValue(), 'getBytesValue');
    CheckEquals( TSDOConvertHelper.BytesToString(localObj.getBytesValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  // multi value
  prp := objectAType.getProperty(LIST_PROP_NAME) as ISDOProperty;
  localObj := TValueSetting.Create(True,True,locVal,prp,12);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 12, localObj.getIndex(), 'getIndex' );
    CheckEquals( nil, localObj.getBytesValue(), 'getBytesValue');
    CheckEquals( TSDOConvertHelper.BytesToString(localObj.getBytesValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,locVal,prp,23);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 23, localObj.getIndex(), 'getIndex' );
    CheckEquals( nil, localObj.getBytesValue(), 'getBytesValue');
    CheckEquals( TSDOConvertHelper.BytesToString(localObj.getBytesValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := LocalCopy(VAL_3);
  localObj := TValueSetting.Create(True,False,locVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getBytesValue(), 'getBytesValue');
    CheckEquals( TSDOConvertHelper.BytesToString(localObj.getBytesValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
procedure TValueSetting_Test.create_char();
const
  PROP_NAME = s_char_prop;
  LIST_PROP_NAME = s_char_propList;
var
  ok : Boolean;
  locVal : TSDOChar;
  fact : ISDODataFactory;
  objectAType : ISDOType;
  localObj : TValueSetting;
  prp : ISDOProperty;
begin
  locVal := 'K';
  fact := CreateFactory();
  objectAType := fact.getType(s_uri,s_type_object_A);
  ok := False;
  try
    TValueSetting.Create(False,False,locVal,nil,0);
  except
    on e : ESDOIllegalArgumentException do
      ok := True;
  end;
  CheckEquals(True,ok,'Invalid property parametter');

  // simple value
  prp := objectAType.getProperty(PROP_NAME) as ISDOProperty;
  localObj := TValueSetting.Create(True,True,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( #0, localObj.getCharacterValue(), 'getCharacterValue');
    CheckEquals( TSDOConvertHelper.CharToBool(localObj.getCharacterValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( TSDOConvertHelper.CharToByte(localObj.getCharacterValue()), localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.CharToInteger(localObj.getCharacterValue()), localObj.getIntegerValue(), 'getIntegerValue');
{$IFDEF HAS_SDO_LONG}
    CheckEquals( TSDOConvertHelper.CharToLong(localObj.getCharacterValue()), localObj.getLongValue(), 'getLongValue');
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    CheckEquals( TSDOConvertHelper.CharToShort(localObj.getCharacterValue()), localObj.getShortValue(), 'getShortValue');
{$ENDIF HAS_SDO_SHORT}
    CheckEquals( localObj.getCharacterValue(), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( #0, localObj.getCharacterValue(), 'getCharacterValue');
    CheckEquals( TSDOConvertHelper.CharToBool(localObj.getCharacterValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( TSDOConvertHelper.CharToByte(localObj.getCharacterValue()), localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.CharToInteger(localObj.getCharacterValue()), localObj.getIntegerValue(), 'getIntegerValue');
{$IFDEF HAS_SDO_LONG}
    CheckEquals( TSDOConvertHelper.CharToLong(localObj.getCharacterValue()), localObj.getLongValue(), 'getLongValue');
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    CheckEquals( TSDOConvertHelper.CharToShort(localObj.getCharacterValue()), localObj.getShortValue(), 'getShortValue');
{$ENDIF HAS_SDO_SHORT}
    CheckEquals( localObj.getCharacterValue(), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getCharacterValue(), 'getCharacterValue');
    CheckEquals( TSDOConvertHelper.CharToBool(localObj.getCharacterValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( TSDOConvertHelper.CharToByte(localObj.getCharacterValue()), localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.CharToInteger(localObj.getCharacterValue()), localObj.getIntegerValue(), 'getIntegerValue');
{$IFDEF HAS_SDO_LONG}
    CheckEquals( TSDOConvertHelper.CharToLong(localObj.getCharacterValue()), localObj.getLongValue(), 'getLongValue');
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    CheckEquals( TSDOConvertHelper.CharToShort(localObj.getCharacterValue()), localObj.getShortValue(), 'getShortValue');
{$ENDIF HAS_SDO_SHORT}
    CheckEquals( localObj.getCharacterValue(), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := 'h';
  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getCharacterValue(), 'getCharacterValue');
    CheckEquals( TSDOConvertHelper.CharToBool(localObj.getCharacterValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( TSDOConvertHelper.CharToByte(localObj.getCharacterValue()), localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.CharToInteger(localObj.getCharacterValue()), localObj.getIntegerValue(), 'getIntegerValue');
{$IFDEF HAS_SDO_LONG}
    CheckEquals( TSDOConvertHelper.CharToLong(localObj.getCharacterValue()), localObj.getLongValue(), 'getLongValue');
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    CheckEquals( TSDOConvertHelper.CharToShort(localObj.getCharacterValue()), localObj.getShortValue(), 'getShortValue');
{$ENDIF HAS_SDO_SHORT}
    CheckEquals( localObj.getCharacterValue(), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  // multi value
  prp := objectAType.getProperty(LIST_PROP_NAME) as ISDOProperty;
  localObj := TValueSetting.Create(True,True,locVal,prp,12);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 12, localObj.getIndex(), 'getIndex' );
    CheckEquals( #0, localObj.getCharacterValue(), 'getCharacterValue');
    CheckEquals( TSDOConvertHelper.CharToBool(localObj.getCharacterValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( TSDOConvertHelper.CharToByte(localObj.getCharacterValue()), localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.CharToInteger(localObj.getCharacterValue()), localObj.getIntegerValue(), 'getIntegerValue');
{$IFDEF HAS_SDO_LONG}
    CheckEquals( TSDOConvertHelper.CharToLong(localObj.getCharacterValue()), localObj.getLongValue(), 'getLongValue');
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    CheckEquals( TSDOConvertHelper.CharToShort(localObj.getCharacterValue()), localObj.getShortValue(), 'getShortValue');
{$ENDIF HAS_SDO_SHORT}
    CheckEquals( localObj.getCharacterValue(), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,locVal,prp,23);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 23, localObj.getIndex(), 'getIndex' );
    CheckEquals( #0, localObj.getCharacterValue(), 'getCharacterValue');
    CheckEquals( TSDOConvertHelper.CharToBool(localObj.getCharacterValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( TSDOConvertHelper.CharToByte(localObj.getCharacterValue()), localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.CharToInteger(localObj.getCharacterValue()), localObj.getIntegerValue(), 'getIntegerValue');
{$IFDEF HAS_SDO_LONG}
    CheckEquals( TSDOConvertHelper.CharToLong(localObj.getCharacterValue()), localObj.getLongValue(), 'getLongValue');
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    CheckEquals( TSDOConvertHelper.CharToShort(localObj.getCharacterValue()), localObj.getShortValue(), 'getShortValue');
{$ENDIF HAS_SDO_SHORT}
    CheckEquals( localObj.getCharacterValue(), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := '1';
  localObj := TValueSetting.Create(True,False,locVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getCharacterValue(), 'getCharacterValue');
    CheckEquals( TSDOConvertHelper.CharToBool(localObj.getCharacterValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( TSDOConvertHelper.CharToByte(localObj.getCharacterValue()), localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.CharToInteger(localObj.getCharacterValue()), localObj.getIntegerValue(), 'getIntegerValue');
{$IFDEF HAS_SDO_LONG}
    CheckEquals( TSDOConvertHelper.CharToLong(localObj.getCharacterValue()), localObj.getLongValue(), 'getLongValue');
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    CheckEquals( TSDOConvertHelper.CharToShort(localObj.getCharacterValue()), localObj.getShortValue(), 'getShortValue');
{$ENDIF HAS_SDO_SHORT}
    CheckEquals( localObj.getCharacterValue(), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;
end;
{$ENDIF HAS_SDO_CHAR}

procedure TValueSetting_Test.create_date();
var
  ok : Boolean;
  locVal : TSDODateTime;
  fact : ISDODataFactory;
  objectAType : ISDOType;
  localObj : TValueSetting;
  prp : ISDOProperty;
begin
  FillChar(locVal,SizeOf(locVal),#0);

  fact := CreateFactory();
  objectAType := fact.getType(s_uri,s_type_object_A);
  ok := False;
  try
    TValueSetting.Create(False,False,locVal,nil,0);
  except
    on e : ESDOIllegalArgumentException do
      ok := True;
  end;
  CheckEquals(True,ok,'Invalid property parametter');

  // simple value
  prp := objectAType.getProperty(s_date_prop) as ISDOProperty;
  localObj := TValueSetting.Create(True,True,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( ZERO_DATE, localObj.getDateValue(), 'getDateValue');
    CheckEquals( TSDOConvertHelper.DateToString(localObj.getDateValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( ZERO_DATE, localObj.getDateValue(), 'getDateValue');
    CheckEquals( TSDOConvertHelper.DateToString(localObj.getDateValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( ZERO_DATE, localObj.getDateValue(), 'getDateValue');
    CheckEquals( TSDOConvertHelper.DateToString(localObj.getDateValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal.Date := 39000;
  locVal.HourOffset := 1;
  locVal.MinuteOffset := 2;
  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getDateValue(), 'getDateValue');
    CheckEquals( TSDOConvertHelper.DateToString(localObj.getDateValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  // multi value
  prp := objectAType.getProperty(s_date_propList) as ISDOProperty;
  localObj := TValueSetting.Create(True,True,locVal,prp,12);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 12, localObj.getIndex(), 'getIndex' );
    CheckEquals( ZERO_DATE, localObj.getDateValue(), 'getDateValue');
    CheckEquals( TSDOConvertHelper.DateToString(localObj.getDateValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,locVal,prp,23);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 23, localObj.getIndex(), 'getIndex' );
    CheckEquals( ZERO_DATE, localObj.getDateValue(), 'getDateValue');
    CheckEquals( TSDOConvertHelper.DateToString(localObj.getDateValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(True,False,locVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getDateValue(), 'getDateValue');
    CheckEquals( TSDOConvertHelper.DateToString(localObj.getDateValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;
end;

{$IFDEF HAS_SDO_CURRENCY}
procedure TValueSetting_Test.create_currency();
const
  PROP_NAME = s_currency_prop;
  LIST_PROP_NAME = s_currency_propList;
var
  ok : Boolean;
  locVal : TSDOCurrency;
  fact : ISDODataFactory;
  objectAType : ISDOType;
  localObj : TValueSetting;
  prp : ISDOProperty;
begin
  locVal := 67;
  fact := CreateFactory();
  objectAType := fact.getType(s_uri,s_type_object_A);
  ok := False;
  try
    TValueSetting.Create(False,False,locVal,nil,0);
  except
    on e : ESDOIllegalArgumentException do
      ok := True;
  end;
  CheckEquals(True,ok,'Invalid property parametter');

  // simple value
  prp := objectAType.getProperty(PROP_NAME) as ISDOProperty;
  localObj := TValueSetting.Create(True,True,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getCurrencyValue(), 'getCurrencyValue');
{$IFDEF HAS_SDO_FLOAT}
    CheckEquals( localObj.getCurrencyValue(), localObj.getFloatValue(), 'getFloatValue');
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_DOUBLE}
    CheckEquals( localObj.getCurrencyValue(), localObj.getDoubleValue(), 'getDoubleValue');
{$ENDIF HAS_SDO_DOUBLE}
    CheckEquals( TSDOConvertHelper.CurrencyToString(localObj.getCurrencyValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getCurrencyValue(), 'getCurrencyValue');
{$IFDEF HAS_SDO_FLOAT}
    CheckEquals( localObj.getCurrencyValue(), localObj.getFloatValue(), 'getFloatValue');
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_DOUBLE}
    CheckEquals( localObj.getCurrencyValue(), localObj.getDoubleValue(), 'getDoubleValue');
{$ENDIF HAS_SDO_DOUBLE}
    CheckEquals( TSDOConvertHelper.CurrencyToString(localObj.getCurrencyValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getCurrencyValue(), 'getCurrencyValue');
{$IFDEF HAS_SDO_FLOAT}
    CheckEquals( localObj.getCurrencyValue(), localObj.getFloatValue(), 'getFloatValue');
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_DOUBLE}
    CheckEquals( localObj.getCurrencyValue(), localObj.getDoubleValue(), 'getDoubleValue');
{$ENDIF HAS_SDO_DOUBLE}
    CheckEquals( TSDOConvertHelper.CurrencyToString(localObj.getCurrencyValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := 45;
  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getCurrencyValue(), 'getCurrencyValue');
{$IFDEF HAS_SDO_FLOAT}
    CheckEquals( localObj.getCurrencyValue(), localObj.getFloatValue(), 'getFloatValue');
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_DOUBLE}
    CheckEquals( localObj.getCurrencyValue(), localObj.getDoubleValue(), 'getDoubleValue');
{$ENDIF HAS_SDO_DOUBLE}
    CheckEquals( TSDOConvertHelper.CurrencyToString(localObj.getCurrencyValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := 1236891478.2522;
  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getCurrencyValue(), 'getCurrencyValue');
    CheckEquals( TSDOConvertHelper.CurrencyToString(localObj.getCurrencyValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := -225544778.8622;
  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getCurrencyValue(), 'getCurrencyValue');
    CheckEquals( TSDOConvertHelper.CurrencyToString(localObj.getCurrencyValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  // multi value
  prp := objectAType.getProperty(LIST_PROP_NAME) as ISDOProperty;
  localObj := TValueSetting.Create(True,True,locVal,prp,12);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 12, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getCurrencyValue(), 'getCurrencyValue');
{$IFDEF HAS_SDO_FLOAT}
    CheckEquals( localObj.getCurrencyValue(), localObj.getFloatValue(), 'getFloatValue');
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_DOUBLE}
    CheckEquals( localObj.getCurrencyValue(), localObj.getDoubleValue(), 'getDoubleValue');
{$ENDIF HAS_SDO_DOUBLE}
    CheckEquals( TSDOConvertHelper.CurrencyToString(localObj.getCurrencyValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,locVal,prp,23);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 23, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getCurrencyValue(), 'getCurrencyValue');
{$IFDEF HAS_SDO_FLOAT}
    CheckEquals( localObj.getCurrencyValue(), localObj.getFloatValue(), 'getFloatValue');
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_DOUBLE}
    CheckEquals( localObj.getCurrencyValue(), localObj.getDoubleValue(), 'getDoubleValue');
{$ENDIF HAS_SDO_DOUBLE}
    CheckEquals( TSDOConvertHelper.CurrencyToString(localObj.getCurrencyValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := 99;
  localObj := TValueSetting.Create(True,False,locVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getCurrencyValue(), 'getCurrencyValue');
{$IFDEF HAS_SDO_FLOAT}
    CheckEquals( localObj.getCurrencyValue(), localObj.getFloatValue(), 'getFloatValue');
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_DOUBLE}
    CheckEquals( localObj.getCurrencyValue(), localObj.getDoubleValue(), 'getDoubleValue');
{$ENDIF HAS_SDO_DOUBLE}
    CheckEquals( TSDOConvertHelper.CurrencyToString(localObj.getCurrencyValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := 24667733.1155;
  localObj := TValueSetting.Create(True,False,locVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getCurrencyValue(), 'getCurrencyValue');
    CheckEquals( TSDOConvertHelper.CurrencyToString(localObj.getCurrencyValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := -5588663211424;
  localObj := TValueSetting.Create(True,False,locVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getCurrencyValue(), 'getCurrencyValue');
    CheckEquals( TSDOConvertHelper.CurrencyToString(localObj.getCurrencyValue()), localObj.getStringValue(), 'getStringValue'); 
  finally
    FreeAndNil(localObj);
  end;
end;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
procedure TValueSetting_Test.create_double();
const
  PROP_NAME = s_double_prop;
  LIST_PROP_NAME = s_double_propList;
var
  ok : Boolean;
  locVal : TSDODouble;
  fact : ISDODataFactory;
  objectAType : ISDOType;
  localObj : TValueSetting;
  prp : ISDOProperty;
begin
  locVal := 67;
  fact := CreateFactory();
  objectAType := fact.getType(s_uri,s_type_object_A);
  ok := False;
  try
    TValueSetting.Create(False,False,locVal,nil,0);
  except
    on e : ESDOIllegalArgumentException do
      ok := True;
  end;
  CheckEquals(True,ok,'Invalid property parametter');

  // simple value
  prp := objectAType.getProperty(PROP_NAME) as ISDOProperty;
  localObj := TValueSetting.Create(True,True,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getDoubleValue(), 'getDoubleValue');
{$IFDEF HAS_SDO_CURRENCY}
    CheckEquals( localObj.getDoubleValue(), localObj.getCurrencyValue(), 'getCurrencyValue');
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_FLOAT}
    CheckEquals( localObj.getDoubleValue(), localObj.getFloatValue(), 'getFloatValue');
{$ENDIF HAS_SDO_FLOAT}
    CheckEquals( TSDOConvertHelper.FloatToString(localObj.getDoubleValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getDoubleValue(), 'getDoubleValue');
{$IFDEF HAS_SDO_CURRENCY}
    CheckEquals( localObj.getDoubleValue(), localObj.getCurrencyValue(), 'getCurrencyValue');
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_FLOAT}
    CheckEquals( localObj.getDoubleValue(), localObj.getFloatValue(), 'getFloatValue');
{$ENDIF HAS_SDO_FLOAT}
    CheckEquals( TSDOConvertHelper.FloatToString(localObj.getDoubleValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getDoubleValue(), 'getDoubleValue');
{$IFDEF HAS_SDO_CURRENCY}
    CheckEquals( localObj.getDoubleValue(), localObj.getCurrencyValue(), 'getCurrencyValue');
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_FLOAT}
    CheckEquals( localObj.getDoubleValue(), localObj.getFloatValue(), 'getFloatValue');
{$ENDIF HAS_SDO_FLOAT}
    CheckEquals( TSDOConvertHelper.FloatToString(localObj.getDoubleValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := 45;
  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getDoubleValue(), 'getDoubleValue');
{$IFDEF HAS_SDO_CURRENCY}
    CheckEquals( localObj.getDoubleValue(), localObj.getCurrencyValue(), 'getCurrencyValue');
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_FLOAT}
    CheckEquals( localObj.getDoubleValue(), localObj.getFloatValue(), 'getFloatValue');
{$ENDIF HAS_SDO_FLOAT}
    CheckEquals( TSDOConvertHelper.FloatToString(localObj.getDoubleValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := 12365479;
  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getDoubleValue(), 'getDoubleValue');
    CheckEquals( TSDOConvertHelper.FloatToString(localObj.getDoubleValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := -225544721;
  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getDoubleValue(), 'getDoubleValue');
    CheckEquals( TSDOConvertHelper.FloatToString(localObj.getDoubleValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  // multi value
  prp := objectAType.getProperty(LIST_PROP_NAME) as ISDOProperty;
  localObj := TValueSetting.Create(True,True,locVal,prp,12);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 12, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getDoubleValue(), 'getDoubleValue');
{$IFDEF HAS_SDO_CURRENCY}
    CheckEquals( localObj.getDoubleValue(), localObj.getCurrencyValue(), 'getCurrencyValue');
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_FLOAT}
    CheckEquals( localObj.getDoubleValue(), localObj.getFloatValue(), 'getFloatValue');
{$ENDIF HAS_SDO_FLOAT}
    CheckEquals( TSDOConvertHelper.FloatToString(localObj.getDoubleValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,locVal,prp,23);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 23, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getDoubleValue(), 'getDoubleValue');
{$IFDEF HAS_SDO_CURRENCY}
    CheckEquals( localObj.getDoubleValue(), localObj.getCurrencyValue(), 'getCurrencyValue');
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_FLOAT}
    CheckEquals( localObj.getDoubleValue(), localObj.getFloatValue(), 'getFloatValue');
{$ENDIF HAS_SDO_FLOAT}
    CheckEquals( TSDOConvertHelper.FloatToString(localObj.getDoubleValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := 99;
  localObj := TValueSetting.Create(True,False,locVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getDoubleValue(), 'getDoubleValue');
{$IFDEF HAS_SDO_CURRENCY}
    CheckEquals( localObj.getDoubleValue(), localObj.getCurrencyValue(), 'getCurrencyValue');
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_FLOAT}
    CheckEquals( localObj.getDoubleValue(), localObj.getFloatValue(), 'getFloatValue');
{$ENDIF HAS_SDO_FLOAT}
    CheckEquals( TSDOConvertHelper.FloatToString(localObj.getDoubleValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := 8855224;
  localObj := TValueSetting.Create(True,False,locVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getDoubleValue(), 'getDoubleValue');
    CheckEquals( TSDOConvertHelper.FloatToString(localObj.getDoubleValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := -55886424;
  localObj := TValueSetting.Create(True,False,locVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getDoubleValue(), 'getDoubleValue');
    CheckEquals( TSDOConvertHelper.FloatToString(localObj.getDoubleValue()), localObj.getStringValue(), 'getStringValue'); 
  finally
    FreeAndNil(localObj);
  end;
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
procedure TValueSetting_Test.create_float();
const
  PROP_NAME = s_float_prop;
  LIST_PROP_NAME = s_float_propList;
var
  ok : Boolean;
  locVal : TSDOFloat;
  fact : ISDODataFactory;
  objectAType : ISDOType;
  localObj : TValueSetting;
  prp : ISDOProperty;
begin
  locVal := 67;
  fact := CreateFactory();
  objectAType := fact.getType(s_uri,s_type_object_A);
  ok := False;
  try
    TValueSetting.Create(False,False,locVal,nil,0);
  except
    on e : ESDOIllegalArgumentException do
      ok := True;
  end;
  CheckEquals(True,ok,'Invalid property parametter');

  // simple value
  prp := objectAType.getProperty(PROP_NAME) as ISDOProperty;
  localObj := TValueSetting.Create(True,True,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getFloatValue(), 'getFloatValue');
{$IFDEF HAS_SDO_CURRENCY}
    CheckEquals( localObj.getFloatValue(), localObj.getCurrencyValue(), 'getCurrencyValue');
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    CheckEquals( localObj.getFloatValue(), localObj.getDoubleValue(), 'getDoubleValue');
{$ENDIF HAS_SDO_DOUBLE}
    CheckEquals( TSDOConvertHelper.FloatToString(localObj.getFloatValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getFloatValue(), 'getFloatValue');
{$IFDEF HAS_SDO_CURRENCY}
    CheckEquals( localObj.getFloatValue(), localObj.getCurrencyValue(), 'getCurrencyValue');
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    CheckEquals( localObj.getFloatValue(), localObj.getDoubleValue(), 'getDoubleValue');
{$ENDIF HAS_SDO_DOUBLE}
    CheckEquals( TSDOConvertHelper.FloatToString(localObj.getFloatValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getFloatValue(), 'getFloatValue');
{$IFDEF HAS_SDO_CURRENCY}
    CheckEquals( localObj.getFloatValue(), localObj.getCurrencyValue(), 'getCurrencyValue');
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    CheckEquals( localObj.getFloatValue(), localObj.getDoubleValue(), 'getDoubleValue');
{$ENDIF HAS_SDO_DOUBLE}
    CheckEquals( TSDOConvertHelper.FloatToString(localObj.getFloatValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := 45;
  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getFloatValue(), 'getFloatValue');
{$IFDEF HAS_SDO_CURRENCY}
    CheckEquals( localObj.getFloatValue(), localObj.getCurrencyValue(), 'getCurrencyValue');
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    CheckEquals( localObj.getFloatValue(), localObj.getDoubleValue(), 'getDoubleValue');
{$ENDIF HAS_SDO_DOUBLE}
    CheckEquals( TSDOConvertHelper.FloatToString(localObj.getFloatValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := 1236547891;
  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getFloatValue(), 'getFloatValue');
    CheckEquals( TSDOConvertHelper.FloatToString(localObj.getFloatValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := -2255442;
  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getFloatValue(), 'getFloatValue');
    CheckEquals( TSDOConvertHelper.FloatToString(localObj.getFloatValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  // multi value
  prp := objectAType.getProperty(LIST_PROP_NAME) as ISDOProperty;
  localObj := TValueSetting.Create(True,True,locVal,prp,12);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 12, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getFloatValue(), 'getFloatValue');
{$IFDEF HAS_SDO_CURRENCY}
    CheckEquals( localObj.getFloatValue(), localObj.getCurrencyValue(), 'getCurrencyValue');
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    CheckEquals( localObj.getFloatValue(), localObj.getDoubleValue(), 'getDoubleValue');
{$ENDIF HAS_SDO_DOUBLE}
    CheckEquals( TSDOConvertHelper.FloatToString(localObj.getFloatValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,locVal,prp,23);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 23, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getFloatValue(), 'getFloatValue');
{$IFDEF HAS_SDO_CURRENCY}
    CheckEquals( localObj.getFloatValue(), localObj.getCurrencyValue(), 'getCurrencyValue');
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    CheckEquals( localObj.getFloatValue(), localObj.getDoubleValue(), 'getDoubleValue');
{$ENDIF HAS_SDO_DOUBLE}
    CheckEquals( TSDOConvertHelper.FloatToString(localObj.getFloatValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := 99;
  localObj := TValueSetting.Create(True,False,locVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getFloatValue(), 'getFloatValue');
{$IFDEF HAS_SDO_CURRENCY}
    CheckEquals( localObj.getFloatValue(), localObj.getCurrencyValue(), 'getCurrencyValue');
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    CheckEquals( localObj.getFloatValue(), localObj.getDoubleValue(), 'getDoubleValue');
{$ENDIF HAS_SDO_DOUBLE}
    CheckEquals( TSDOConvertHelper.FloatToString(localObj.getFloatValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := 88552255;
  localObj := TValueSetting.Create(True,False,locVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getFloatValue(), 'getFloatValue');
    CheckEquals( TSDOConvertHelper.FloatToString(localObj.getFloatValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := -55886624;
  localObj := TValueSetting.Create(True,False,locVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getFloatValue(), 'getFloatValue');
    CheckEquals( TSDOConvertHelper.FloatToString(localObj.getFloatValue()), localObj.getStringValue(), 'getStringValue'); 
  finally
    FreeAndNil(localObj);
  end;
end;
{$ENDIF HAS_SDO_FLOAT}

procedure TValueSetting_Test.create_integer();
var
  ok : Boolean;
  intVal : TSDOInteger;
  fact : ISDODataFactory;
  objectAType : ISDOType;
  localObj : TValueSetting;
  prp : ISDOProperty;
begin
  intVal := 1210;
  fact := CreateFactory();
  objectAType := fact.getType(s_uri,s_type_object_A);
  ok := False;
  try
    TValueSetting.Create(False,False,intVal,nil,0);
  except
    on e : ESDOIllegalArgumentException do
      ok := True;
  end;
  CheckEquals(True,ok,'Invalid property parametter');

  // simple value
  prp := objectAType.getProperty(s_integer_prop) as ISDOProperty;
  localObj := TValueSetting.Create(True,True,intVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getIntegerValue(), 'getIntegerValue');
    CheckEquals( TSDOConvertHelper.IntegerToBool(localObj.getIntegerValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( TSDOConvertHelper.IntegerToString(localObj.getIntegerValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,intVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getIntegerValue(), 'getIntegerValue');
    CheckEquals( TSDOConvertHelper.IntegerToBool(localObj.getIntegerValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( TSDOConvertHelper.IntegerToString(localObj.getIntegerValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(True,False,intVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( intVal, localObj.getIntegerValue(), 'getIntegerValue');
    CheckEquals( TSDOConvertHelper.IntegerToBool(localObj.getIntegerValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( TSDOConvertHelper.IntegerToString(localObj.getIntegerValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  intVal := 67;
  localObj := TValueSetting.Create(True,False,intVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( intVal, localObj.getIntegerValue(), 'getIntegerValue');
    CheckEquals( TSDOConvertHelper.IntegerToBool(localObj.getIntegerValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( localObj.getByteValue(), localObj.getIntegerValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.IntegerToString(localObj.getIntegerValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  // multi value
  prp := objectAType.getProperty(s_integer_propList) as ISDOProperty;
  localObj := TValueSetting.Create(True,True,intVal,prp,12);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 12, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getIntegerValue(), 'getIntegerValue');
    CheckEquals( TSDOConvertHelper.IntegerToBool(localObj.getIntegerValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( TSDOConvertHelper.IntegerToString(localObj.getIntegerValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,intVal,prp,23);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 23, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getIntegerValue(), 'getIntegerValue');
    CheckEquals( TSDOConvertHelper.IntegerToBool(localObj.getIntegerValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( TSDOConvertHelper.IntegerToString(localObj.getIntegerValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  intVal := 345;
  localObj := TValueSetting.Create(True,False,intVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( intVal, localObj.getIntegerValue(), 'getIntegerValue');
    CheckEquals( TSDOConvertHelper.IntegerToBool(localObj.getIntegerValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( TSDOConvertHelper.IntegerToString(localObj.getIntegerValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;
end;

{$IFDEF HAS_SDO_LONG}
procedure TValueSetting_Test.create_long();
const
  PROP_NAME = s_long_prop;
  LIST_PROP_NAME = s_long_propList;
var
  ok : Boolean;
  locVal : TSDOLong;
  fact : ISDODataFactory;
  objectAType : ISDOType;
  localObj : TValueSetting;
  prp : ISDOProperty;
begin
  locVal := 67;
  fact := CreateFactory();
  objectAType := fact.getType(s_uri,s_type_object_A);
  ok := False;
  try
    TValueSetting.Create(False,False,locVal,nil,0);
  except
    on e : ESDOIllegalArgumentException do
      ok := True;
  end;
  CheckEquals(True,ok,'Invalid property parametter');

  // simple value
  prp := objectAType.getProperty(PROP_NAME) as ISDOProperty;
  localObj := TValueSetting.Create(True,True,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getLongValue(), 'getLongValue');
{$IFDEF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.LongToChar(localObj.getLongValue()), localObj.getCharacterValue(), 'getCharacterValue');
{$ENDIF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.LongToBool(localObj.getLongValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( localObj.getLongValue(), localObj.getByteValue(), 'getByteValue');
    CheckEquals( localObj.getLongValue(), localObj.getIntegerValue(), 'getIntegerValue');
{$IFDEF HAS_SDO_SHORT}
    CheckEquals( localObj.getLongValue(), localObj.getShortValue(), 'getShortValue');
{$ENDIF HAS_SDO_SHORT}
    CheckEquals( TSDOConvertHelper.LongToString(localObj.getLongValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getLongValue(), 'getLongValue');
{$IFDEF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.LongToChar(localObj.getLongValue()), localObj.getCharacterValue(), 'getCharacterValue');
{$ENDIF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.LongToBool(localObj.getLongValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( localObj.getLongValue(), localObj.getByteValue(), 'getByteValue');
    CheckEquals( localObj.getLongValue(), localObj.getIntegerValue(), 'getIntegerValue');
{$IFDEF HAS_SDO_SHORT}
    CheckEquals( localObj.getLongValue(), localObj.getShortValue(), 'getShortValue');
{$ENDIF HAS_SDO_SHORT}
    CheckEquals( TSDOConvertHelper.LongToString(localObj.getLongValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getLongValue(), 'getLongValue');
{$IFDEF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.LongToChar(localObj.getLongValue()), localObj.getCharacterValue(), 'getCharacterValue');
{$ENDIF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.LongToBool(localObj.getLongValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( localObj.getLongValue(), localObj.getByteValue(), 'getByteValue');
    CheckEquals( localObj.getLongValue(), localObj.getIntegerValue(), 'getIntegerValue');
{$IFDEF HAS_SDO_SHORT}
    CheckEquals( localObj.getLongValue(), localObj.getShortValue(), 'getShortValue');
{$ENDIF HAS_SDO_SHORT}
    CheckEquals( TSDOConvertHelper.LongToString(localObj.getLongValue()), localObj.getStringValue(), 'getStringValue');  finally
    FreeAndNil(localObj);
  end;

  locVal := 45;
  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getLongValue(), 'getLongValue');
{$IFDEF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.LongToChar(localObj.getLongValue()), localObj.getCharacterValue(), 'getCharacterValue');
{$ENDIF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.LongToBool(localObj.getLongValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( localObj.getLongValue(), localObj.getByteValue(), 'getByteValue');
    CheckEquals( localObj.getLongValue(), localObj.getIntegerValue(), 'getIntegerValue');
{$IFDEF HAS_SDO_SHORT}
    CheckEquals( localObj.getLongValue(), localObj.getShortValue(), 'getShortValue');
{$ENDIF HAS_SDO_SHORT}
    CheckEquals( TSDOConvertHelper.LongToString(localObj.getLongValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := 12365478914782522;
  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getLongValue(), 'getLongValue');
    CheckEquals( TSDOConvertHelper.LongToString(localObj.getLongValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := -22554477886221222;
  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getLongValue(), 'getLongValue');
    CheckEquals( TSDOConvertHelper.LongToString(localObj.getLongValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  // multi value
  prp := objectAType.getProperty(LIST_PROP_NAME) as ISDOProperty;
  localObj := TValueSetting.Create(True,True,locVal,prp,12);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 12, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getLongValue(), 'getLongValue');
{$IFDEF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.LongToChar(localObj.getLongValue()), localObj.getCharacterValue(), 'getCharacterValue');
{$ENDIF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.LongToBool(localObj.getLongValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( localObj.getLongValue(), localObj.getByteValue(), 'getByteValue');
    CheckEquals( localObj.getLongValue(), localObj.getIntegerValue(), 'getIntegerValue');
{$IFDEF HAS_SDO_SHORT}
    CheckEquals( localObj.getLongValue(), localObj.getShortValue(), 'getShortValue');
{$ENDIF HAS_SDO_SHORT}
    CheckEquals( TSDOConvertHelper.LongToString(localObj.getLongValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,locVal,prp,23);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 23, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getLongValue(), 'getLongValue');
{$IFDEF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.LongToChar(localObj.getLongValue()), localObj.getCharacterValue(), 'getCharacterValue');
{$ENDIF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.LongToBool(localObj.getLongValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( localObj.getLongValue(), localObj.getByteValue(), 'getByteValue');
    CheckEquals( localObj.getLongValue(), localObj.getIntegerValue(), 'getIntegerValue');
{$IFDEF HAS_SDO_SHORT}
    CheckEquals( localObj.getLongValue(), localObj.getShortValue(), 'getShortValue');
{$ENDIF HAS_SDO_SHORT}
    CheckEquals( TSDOConvertHelper.LongToString(localObj.getLongValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := 99;
  localObj := TValueSetting.Create(True,False,locVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getLongValue(), 'getLongValue');
{$IFDEF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.LongToChar(localObj.getLongValue()), localObj.getCharacterValue(), 'getCharacterValue');
{$ENDIF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.LongToBool(localObj.getLongValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( localObj.getLongValue(), localObj.getByteValue(), 'getByteValue');
    CheckEquals( localObj.getLongValue(), localObj.getIntegerValue(), 'getIntegerValue');
{$IFDEF HAS_SDO_SHORT}
    CheckEquals( localObj.getLongValue(), localObj.getShortValue(), 'getShortValue');
{$ENDIF HAS_SDO_SHORT}
    CheckEquals( TSDOConvertHelper.LongToString(localObj.getLongValue()), localObj.getStringValue(), 'getStringValue');  
  finally
    FreeAndNil(localObj);
  end;

  locVal := 88552246677331155;
  localObj := TValueSetting.Create(True,False,locVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getLongValue(), 'getLongValue');
    CheckEquals( TSDOConvertHelper.LongToString(localObj.getLongValue()), localObj.getStringValue(), 'getStringValue');  
  finally
    FreeAndNil(localObj);
  end;

  locVal := -5588663211424;
  localObj := TValueSetting.Create(True,False,locVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getLongValue(), 'getLongValue');
    CheckEquals( TSDOConvertHelper.LongToString(localObj.getLongValue()), localObj.getStringValue(), 'getStringValue');  
  finally
    FreeAndNil(localObj);
  end;
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure TValueSetting_Test.create_short();
const
  PROP_NAME = s_short_prop;
  LIST_PROP_NAME = s_short_propList;
var
  ok : Boolean;
  locVal : TSDOShort;
  fact : ISDODataFactory;
  objectAType : ISDOType;
  localObj : TValueSetting;
  prp : ISDOProperty;
begin
  locVal := 67;
  fact := CreateFactory();
  objectAType := fact.getType(s_uri,s_type_object_A);
  ok := False;
  try
    TValueSetting.Create(False,False,locVal,nil,0);
  except
    on e : ESDOIllegalArgumentException do
      ok := True;
  end;
  CheckEquals(True,ok,'Invalid property parametter');

  // simple value
  prp := objectAType.getProperty(PROP_NAME) as ISDOProperty;
  localObj := TValueSetting.Create(True,True,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getShortValue(), 'getShortValue');
{$IFDEF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.ShortToChar(localObj.getShortValue()), localObj.getCharacterValue(), 'getCharacterValue');
{$ENDIF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.ShortToBool(localObj.getShortValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( localObj.getShortValue(), localObj.getByteValue(), 'getByteValue');
    CheckEquals( localObj.getShortValue(), localObj.getIntegerValue(), 'getIntegerValue');
{$IFDEF HAS_SDO_LONG}
    CheckEquals( localObj.getShortValue(), localObj.getLongValue(), 'getLongValue');
{$ENDIF HAS_SDO_LONG}
    CheckEquals( TSDOConvertHelper.ShortToString(localObj.getShortValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getShortValue(), 'getShortValue');
{$IFDEF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.ShortToChar(localObj.getShortValue()), localObj.getCharacterValue(), 'getCharacterValue');
{$ENDIF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.ShortToBool(localObj.getShortValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( localObj.getShortValue(), localObj.getByteValue(), 'getByteValue');
    CheckEquals( localObj.getShortValue(), localObj.getIntegerValue(), 'getIntegerValue');
{$IFDEF HAS_SDO_LONG}
    CheckEquals( localObj.getShortValue(), localObj.getLongValue(), 'getLongValue');
{$ENDIF HAS_SDO_LONG}
    CheckEquals( TSDOConvertHelper.ShortToString(localObj.getShortValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getShortValue(), 'getShortValue');
{$IFDEF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.ShortToChar(localObj.getShortValue()), localObj.getCharacterValue(), 'getCharacterValue');
{$ENDIF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.ShortToBool(localObj.getShortValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( localObj.getShortValue(), localObj.getByteValue(), 'getByteValue');
    CheckEquals( localObj.getShortValue(), localObj.getIntegerValue(), 'getIntegerValue');
{$IFDEF HAS_SDO_LONG}
    CheckEquals( localObj.getShortValue(), localObj.getLongValue(), 'getLongValue');
{$ENDIF HAS_SDO_LONG}
    CheckEquals( TSDOConvertHelper.ShortToString(localObj.getShortValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := 45;
  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getShortValue(), 'getShortValue');
{$IFDEF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.ShortToChar(localObj.getShortValue()), localObj.getCharacterValue(), 'getCharacterValue');
{$ENDIF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.ShortToBool(localObj.getShortValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( localObj.getShortValue(), localObj.getByteValue(), 'getByteValue');
    CheckEquals( localObj.getShortValue(), localObj.getIntegerValue(), 'getIntegerValue');
{$IFDEF HAS_SDO_LONG}
    CheckEquals( localObj.getShortValue(), localObj.getLongValue(), 'getLongValue');
{$ENDIF HAS_SDO_LONG}
    CheckEquals( TSDOConvertHelper.ShortToString(localObj.getShortValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := 23567;
  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getShortValue(), 'getShortValue');
    CheckEquals( TSDOConvertHelper.ShortToString(localObj.getShortValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := -22552;
  localObj := TValueSetting.Create(True,False,locVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getShortValue(), 'getShortValue');
    CheckEquals( TSDOConvertHelper.ShortToString(localObj.getShortValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  // multi value
  prp := objectAType.getProperty(LIST_PROP_NAME) as ISDOProperty;
  localObj := TValueSetting.Create(True,True,locVal,prp,12);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 12, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getShortValue(), 'getShortValue');
{$IFDEF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.ShortToChar(localObj.getShortValue()), localObj.getCharacterValue(), 'getCharacterValue');
{$ENDIF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.ShortToBool(localObj.getShortValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( localObj.getShortValue(), localObj.getByteValue(), 'getByteValue');
    CheckEquals( localObj.getShortValue(), localObj.getIntegerValue(), 'getIntegerValue');
{$IFDEF HAS_SDO_LONG}
    CheckEquals( localObj.getShortValue(), localObj.getLongValue(), 'getLongValue');
{$ENDIF HAS_SDO_LONG}
    CheckEquals( TSDOConvertHelper.ShortToString(localObj.getShortValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,locVal,prp,23);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 23, localObj.getIndex(), 'getIndex' );
    CheckEquals( 0, localObj.getShortValue(), 'getShortValue');
{$IFDEF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.ShortToChar(localObj.getShortValue()), localObj.getCharacterValue(), 'getCharacterValue');
{$ENDIF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.ShortToBool(localObj.getShortValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( localObj.getShortValue(), localObj.getByteValue(), 'getByteValue');
    CheckEquals( localObj.getShortValue(), localObj.getIntegerValue(), 'getIntegerValue');
{$IFDEF HAS_SDO_LONG}
    CheckEquals( localObj.getShortValue(), localObj.getLongValue(), 'getLongValue');
{$ENDIF HAS_SDO_LONG}
    CheckEquals( TSDOConvertHelper.ShortToString(localObj.getShortValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := 99;
  localObj := TValueSetting.Create(True,False,locVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getShortValue(), 'getShortValue');
{$IFDEF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.ShortToChar(localObj.getShortValue()), localObj.getCharacterValue(), 'getCharacterValue');
{$ENDIF HAS_SDO_CHAR}
    CheckEquals( TSDOConvertHelper.ShortToBool(localObj.getShortValue()), localObj.getBooleanValue(), 'getBooleanValue');
    CheckEquals( localObj.getShortValue(), localObj.getByteValue(), 'getByteValue');
    CheckEquals( localObj.getShortValue(), localObj.getIntegerValue(), 'getIntegerValue');
{$IFDEF HAS_SDO_LONG}
    CheckEquals( localObj.getShortValue(), localObj.getLongValue(), 'getLongValue');
{$ENDIF HAS_SDO_LONG}
    CheckEquals( TSDOConvertHelper.ShortToString(localObj.getShortValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := 8855;
  localObj := TValueSetting.Create(True,False,locVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getShortValue(), 'getShortValue');
    CheckEquals( TSDOConvertHelper.ShortToString(localObj.getShortValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  locVal := -11424;
  localObj := TValueSetting.Create(True,False,locVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( locVal, localObj.getShortValue(), 'getShortValue');
    CheckEquals( TSDOConvertHelper.ShortToString(localObj.getShortValue()), localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;
end;
{$ENDIF HAS_SDO_SHORT}

procedure TValueSetting_Test.create_string();
var
  ok : Boolean;
  strVal : TSDOString;
  fact : ISDODataFactory;
  objectAType : ISDOType;
  localObj : TValueSetting;
  prp : ISDOProperty;
begin
  strVal := 'sdo sample string';
  fact := CreateFactory();
  objectAType := fact.getType(s_uri,s_type_object_A);
  ok := False;
  try
    TValueSetting.Create(False,False,strVal,nil,0);
  except
    on e : ESDOIllegalArgumentException do
      ok := True;
  end;
  CheckEquals(True,ok,'Invalid property parametter');

  // simple value
  prp := objectAType.getProperty(s_string_prop) as ISDOProperty;
  localObj := TValueSetting.Create(True,True,strVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( '', localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,strVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( '', localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(True,False,strVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( strVal, localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  strVal := '123';
  localObj := TValueSetting.Create(True,False,strVal,prp,0);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 0, localObj.getIndex(), 'getIndex' );
    CheckEquals( strVal, localObj.getStringValue(), 'getStringValue');
    CheckEquals( TSDOConvertHelper.StringToByte(localObj.getStringValue()), localObj.getByteValue(), 'getByteValue');
    CheckEquals( TSDOConvertHelper.StringToInteger(localObj.getStringValue()), localObj.getIntegerValue(), 'getIntegerValue');
{$IFDEF HAS_SDO_BYTES}
    CheckEquals( TSDOConvertHelper.StringToBytes(localObj.getStringValue()), localObj.getBytesValue(), 'getBytesValue');
{$ENDIF HAS_SDO_BYTES}
  finally
    FreeAndNil(localObj);
  end;

  // multi value
  prp := objectAType.getProperty(s_string_propList) as ISDOProperty;
  localObj := TValueSetting.Create(True,True,strVal,prp,12);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 12, localObj.getIndex(), 'getIndex' );
    CheckEquals( '', localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(False,True,strVal,prp,23);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( False, localObj.isSet(), 'isSet' );
    CheckEquals( True, localObj.isNull(), 'isNull' );
    CheckEquals( 23, localObj.getIndex(), 'getIndex' );
    CheckEquals( '', localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  localObj := TValueSetting.Create(True,False,strVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( strVal, localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;

  strVal := 'qwsdff';
  localObj := TValueSetting.Create(True,False,strVal,prp,34);
  try
    CheckEquals( PtrInt(prp), PtrInt(localObj.getProperty()), 'getProperty' );
    CheckEquals( True, localObj.isSet(), 'isSet' );
    CheckEquals( False, localObj.isNull(), 'isNull' );
    CheckEquals( 34, localObj.getIndex(), 'getIndex' );
    CheckEquals( strVal, localObj.getStringValue(), 'getStringValue');
  finally
    FreeAndNil(localObj);
  end;
end;

{ TSDOSettingList_Test }

procedure TSDOSettingList_Test.append();
var
  ok : Boolean;
  intVal : TSDOInteger;
  fact : ISDODataFactory;
  objectAType : ISDOType;
  stg0, stg1, stg2, stg3 : TValueSetting;
  localObj : ISDOSettingList;
  prp : ISDOProperty;
begin
  intVal := 1210;
  fact := CreateFactory();
  objectAType := fact.getType(s_uri,s_type_object_A);
  prp := objectAType.getProperty(s_integer_prop) as ISDOProperty;

  localObj := TSDOSettingList.Create();
  ok := False;
  try
    localObj.append(nil);
  except
    on e : ESDOIllegalArgumentException do
      ok := True;
  end;
  CheckEquals(True,ok,'append(0,nil)');

  stg0 := TValueSetting.Create(True,False,intVal,prp,0);
  localObj.append(stg0);
    CheckEquals(1,localObj.size());
    CheckSame(stg0,localObj.getItem(0));
  localObj.append(stg0);
    CheckEquals(1,localObj.size(),'appended twice');
    CheckSame(stg0,localObj.getItem(0),'appended twice');

  stg1 := TValueSetting.Create(True,False,intVal,prp,0);
  localObj.append(stg1);
    CheckEquals(2,localObj.size());
    CheckSame(stg0,localObj.getItem(0));
    CheckSame(stg1,localObj.getItem(1));

  stg2 := TValueSetting.Create(True,False,intVal,prp,0);
  localObj.append(stg2);
    CheckEquals(3,localObj.size());
    CheckSame(stg0,localObj.getItem(0));
    CheckSame(stg1,localObj.getItem(1));
    CheckSame(stg2,localObj.getItem(2));
  localObj.append(stg2);
    CheckEquals(3,localObj.size(),'appended twice');
    CheckSame(stg0,localObj.getItem(0),'appended twice');
    CheckSame(stg1,localObj.getItem(1),'appended twice');
    CheckSame(stg2,localObj.getItem(2),'appended twice');

  stg3 := TValueSetting.Create(True,False,intVal,prp,0);
  localObj.append(stg3);
    CheckEquals(4,localObj.size());
    CheckSame(stg0,localObj.getItem(0));
    CheckSame(stg1,localObj.getItem(1));
    CheckSame(stg2,localObj.getItem(2));
    CheckSame(stg3,localObj.getItem(3));
end;

class function TSDOSettingList_Test.CreateFactory() : ISDODataFactory;
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
      locFactory.addProperty(locObj,s_integer_prop,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType], []);
      locFactory.addProperty(locObj,s_integer_propList,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsMany]);
      locFactory.addProperty(locObj,s_string_prop,sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
      locFactory.addProperty(locObj,s_string_propList,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsMany]);
  end;
begin
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
  Add_ObjectA(s_uri);

  Result := locFactory;
end;

procedure TSDOSettingList_Test.getItem();
var
  ok : Boolean;
  intVal : TSDOInteger;
  fact : ISDODataFactory;
  objectAType : ISDOType;
  stg0, stg1, stg2, stg3 : TValueSetting;
  localObj : ISDOSettingList;
  prp : ISDOProperty;
begin
  intVal := 1210;
  fact := CreateFactory();
  objectAType := fact.getType(s_uri,s_type_object_A);
  prp := objectAType.getProperty(s_integer_prop) as ISDOProperty;

  localObj := TSDOSettingList.Create();
  ok := False;
  try
    localObj.getItem(0);
  except
    on e : ESDOIndexOutOfRangeException do
      ok := True;
  end;
  CheckEquals(True,ok,'getItem(0,nil)');

  stg0 := TValueSetting.Create(True,False,intVal,prp,0);
  localObj.insert(0,stg0);
    CheckEquals(1,localObj.size());
    CheckSame(stg0,localObj.getItem(0));

  stg1 := TValueSetting.Create(True,False,intVal,prp,0);
  localObj.insert(0,stg1);
    CheckEquals(2,localObj.size());
    CheckSame(stg0,localObj.getItem(1));
    CheckSame(stg1,localObj.getItem(0));

  stg2 := TValueSetting.Create(True,False,intVal,prp,0);
  localObj.insert(1,stg2);
    CheckEquals(3,localObj.size());
    CheckSame(stg0,localObj.getItem(2));
    CheckSame(stg1,localObj.getItem(0));
    CheckSame(stg2,localObj.getItem(1));

  stg3 := TValueSetting.Create(True,False,intVal,prp,0);
  localObj.insert((localObj.size() - 1),stg3);
    CheckEquals(4,localObj.size());
    CheckSame(stg0,localObj.getItem(3));
    CheckSame(stg1,localObj.getItem(0));
    CheckSame(stg2,localObj.getItem(1));
    CheckSame(stg3,localObj.getItem(2));

  localObj.remove(0);
    CheckSame(stg0,localObj.getItem(2));
    CheckSame(stg2,localObj.getItem(0));
    CheckSame(stg3,localObj.getItem(1));

  localObj.remove(2);
    CheckSame(stg2,localObj.getItem(0));
    CheckSame(stg3,localObj.getItem(1));

  stg0 := TValueSetting.Create(True,False,intVal,prp,0);
  localObj.append(stg0);
    CheckSame(stg2,localObj.getItem(0));
    CheckSame(stg3,localObj.getItem(1));
    CheckSame(stg0,localObj.getItem(2));

  localObj.remove(1);
    CheckSame(stg2,localObj.getItem(0));
    CheckSame(stg0,localObj.getItem(1));

  localObj.remove(1);
  localObj.remove(0);
    ok := False;
    try
      localObj.getItem(0);
    except
      on e : ESDOIndexOutOfRangeException do
        ok := True;
    end;
    CheckEquals(True,ok,'getItem(0,nil)');
end;

procedure TSDOSettingList_Test.insert();
var
  ok : Boolean;
  intVal : TSDOInteger;
  fact : ISDODataFactory;
  objectAType : ISDOType;
  stg0, stg1, stg2, stg3 : TValueSetting;
  localObj : ISDOSettingList;
  prp : ISDOProperty;
begin
  intVal := 1210;
  fact := CreateFactory();
  objectAType := fact.getType(s_uri,s_type_object_A);
  prp := objectAType.getProperty(s_integer_prop) as ISDOProperty;

  localObj := TSDOSettingList.Create();
  ok := False;
  try
    localObj.insert(0,nil);
  except
    on e : ESDOIllegalArgumentException do
      ok := True;
  end;
  CheckEquals(True,ok,'insert(0,nil)');

  stg0 := TValueSetting.Create(True,False,intVal,prp,0);
  localObj.insert(0,stg0);
    CheckEquals(1,localObj.size());
    CheckSame(stg0,localObj.getItem(0));
  localObj.insert(0,stg0);
    CheckEquals(1,localObj.size(),'inserted twice');
    CheckSame(stg0,localObj.getItem(0),'inserted twice');

  stg1 := TValueSetting.Create(True,False,intVal,prp,0);
  localObj.insert(0,stg1);
    CheckEquals(2,localObj.size());
    CheckSame(stg0,localObj.getItem(1));
    CheckSame(stg1,localObj.getItem(0));

  stg2 := TValueSetting.Create(True,False,intVal,prp,0);
  localObj.insert(1,stg2);
    CheckEquals(3,localObj.size());
    CheckSame(stg0,localObj.getItem(2));
    CheckSame(stg1,localObj.getItem(0));
    CheckSame(stg2,localObj.getItem(1));
  localObj.insert(1,stg2);
    CheckEquals(3,localObj.size(),'inserted twice');
    CheckSame(stg0,localObj.getItem(2),'inserted twice');
    CheckSame(stg1,localObj.getItem(0),'inserted twice');
    CheckSame(stg2,localObj.getItem(1),'inserted twice');

  stg3 := TValueSetting.Create(True,False,intVal,prp,0);
  localObj.insert((localObj.size() - 1),stg3);
    CheckEquals(4,localObj.size());
    CheckSame(stg0,localObj.getItem(3));
    CheckSame(stg1,localObj.getItem(0));
    CheckSame(stg2,localObj.getItem(1));
    CheckSame(stg3,localObj.getItem(2));
end;

procedure TSDOSettingList_Test.remove();
var
  ok : Boolean;
  intVal : TSDOInteger;
  fact : ISDODataFactory;
  objectAType : ISDOType;
  stg0, stg1 : TValueSetting;
  localObj : ISDOSettingList;
  prp : ISDOProperty;
begin
  intVal := 1210;
  fact := CreateFactory();
  objectAType := fact.getType(s_uri,s_type_object_A);
  prp := objectAType.getProperty(s_integer_prop) as ISDOProperty;

  localObj := TSDOSettingList.Create();
  ok := False;
  try
    localObj.remove(0);
  except
    on e : ESDOIndexOutOfRangeException do
      ok := True;
  end;
  CheckEquals(True,ok,'remove(0,nil)');

  stg0 := TValueSetting.Create(True,False,intVal,prp,0);
  localObj.append(stg0);
  ok := False;
  try
    localObj.remove(1);
  except
    on e : ESDOIndexOutOfRangeException do
      ok := True;
  end;
  CheckEquals(True,ok,'remove(1,nil)');

  localObj.remove(0);
    CheckEquals(0,localObj.size());
    ok := False;
    try
      localObj.remove(0);
    except
      on e : ESDOIndexOutOfRangeException do
        ok := True;
    end;
    CheckEquals(True,ok,'remove(0,nil)');


  stg0 := TValueSetting.Create(True,False,intVal,prp,0);
  localObj.append(stg0);
  stg1 := TValueSetting.Create(True,False,intVal,prp,0);
  localObj.append(stg1);
    ok := False;
    try
      localObj.remove(2);
    except
      on e : ESDOIndexOutOfRangeException do
        ok := True;
    end;
    CheckEquals(True,ok,'remove(2,nil)');

    localObj.remove(1);
      CheckSame(stg0,localObj.getItem(0));
        stg1 := TValueSetting.Create(True,False,intVal,prp,0);
        localObj.append(stg1);
        CheckSame(stg0,localObj.getItem(0));
        CheckSame(stg1,localObj.getItem(1));
      localObj.remove(0);
      localObj.remove(0);
        CheckEquals(0,localObj.size());
end;

procedure TSDOSettingList_Test.size();
var
  intVal : TSDOInteger;
  fact : ISDODataFactory;
  objectAType : ISDOType;
  stg : TValueSetting;
  localObj : ISDOSettingList;
  prp : ISDOProperty;
begin
  intVal := 1210;
  fact := CreateFactory();
  objectAType := fact.getType(s_uri,s_type_object_A);
  prp := objectAType.getProperty(s_integer_prop) as ISDOProperty;

  localObj := TSDOSettingList.Create();
  CheckEquals(0,localObj.size());
  CheckEquals(0,localObj.size());

  stg := TValueSetting.Create(True,False,intVal,prp,0);
  localObj.append(stg);
    CheckEquals(1,localObj.size());
  stg := TValueSetting.Create(True,False,intVal,prp,0);
  localObj.append(stg);
    CheckEquals(2,localObj.size());
  localObj.remove(1);
    CheckEquals(1,localObj.size());
  localObj.remove(0);
    CheckEquals(0,localObj.size());

  stg := TValueSetting.Create(True,False,intVal,prp,0);
  localObj.insert(0,stg);
    CheckEquals(1,localObj.size());

  stg := TValueSetting.Create(True,False,intVal,prp,0);
  localObj.insert(0,stg);
    CheckEquals(2,localObj.size());
end;

{ TDicho_Test }
(*
procedure TDicho_Test.build();
var
  ls : TObjectList;
  NBR, i, j, k : PtrInt;
  msg, order : string;
begin
  j := 0;
  Randomize();
  NBR := RandomRange(100,500);
  ls := TObjectList.Create(False);
  try
    order := '';
    for i := 1 to NBR do begin
      while True do begin
        j := RandomRange(0,1000);
        if ( ls.IndexOf(TObject(j)) = -1 ) then
          Break;
      end;
      CheckEquals(False,findDicho(ls,j,k,@CastExtractor));
      ls.Insert(k,TObject(j));
      order := order + ';' + IntToStr(j);
    end;
    for i := 1 to Pred(NBR) do begin
      if ( PtrInt(ls[i-1]) > PtrInt(ls[i]) ) then begin
        msg := '';
        for j := 0 to Pred(NBR) do begin
          if (j > 0) then
            msg := msg + '; ';
          msg := Format('%sls[%d] = %d',[msg,j,PtrInt(ls[j])]);
        end;
        Check(False,msg + '  ; Order = ' + order);
      end;
      {Check(
        PtrInt(ls[i-1]) < PtrInt(ls[i]),
        Format('i = %d; ls[i] = %d; ls[i-1] = %d',[i, PtrInt(ls[i]), PtrInt(ls[i-1])])
      );}
    end;
  finally
    ls.Free();
  end;
end;

procedure TDicho_Test.find();
var
  offset, value, index, lngth, i : PtrInt;
  passed : Boolean;
  v : TObjectList;
begin
  v := TObjectList.Create(False);
  try
    for offset := 1 to 5 do begin
      for lngth := 1 to 2049 do begin
        v.Clear();
        v.Capacity := lngth;
        for i := 0 to Pred(lngth) do begin
          v.Add(TObject(i*offset));
        end;
        for value := 0 to Pred(lngth) do begin
          passed := findDicho(v,value*offset,index,@CastExtractor);
          passed := passed and ( index = value );
          CheckEquals(True,passed);
        end;
      end;
    end;
  finally
    v.Free();
  end;
end; *)

{ TSDOChangedDataObjectList_Test }

procedure TSDOChangedDataObjectList_Test.append();
var
  localObj : ISDOChangedDataObjectListEx;
  fact : ISDODataFactory;
  i, j, k : PtrInt;
  il(*, ilSorted*) : IInterfaceList;
  obj : ISDODataObject;
  ct : TChangeType;
  ok : Boolean;
  NBR_INST : PtrInt;
begin
  k := 0;
  Randomize();
  NBR_INST := RandomRange(100,1000);
  fact := CreateFactory();
  il := TInterfaceList.Create();

  localObj := CreateObject();

  for i := 0 to Pred(NBR_INST) do begin
    ct := TChangeType( ( i mod 3 ) + 1 );
    obj := fact.createNew(s_uri,s_type_object_A);
    localObj.append(obj,ct);
    il.Add(obj);
  end;
  {// The list is no longer sorted!
  ilSorted := SortInterfaceList(il);
  for i := 0 to Pred(NBR_INST) do begin
    CheckEquals(PtrInt(ilSorted[i]),PtrInt(localObj.getDataObject(Pred(NBR_INST) - i)));
  end; }
  for i := 0 to Pred(NBR_INST) do begin
    ok := False;
    for j := 0 to Pred(NBR_INST) do begin
      if ( il[i] = localObj.getDataObject(j) ) then begin
        k := j;
        ok := True;
        Break;
      end;
    end;
    CheckEquals(True, ok, 'getDataObject');
    CheckEquals(PtrInt(TChangeType( ( i mod 3 ) + 1 )), PtrInt(Ord(localObj.getType(k))), 'getType()');
  end;

  for i := 0 to Pred(NBR_INST) do begin
    obj := localObj.getDataObject(i);
    ok := False;
    try
      localObj.append(obj,ctCreate);
    except
      on e : ESDODuplicatedItemException do
        ok := True;
    end;
    CheckEquals(True,ok,Format('Duplicated item, i = %d',[i]));

    ok := False;
    try
      localObj.append(obj,ctChange);
    except
      on e : ESDODuplicatedItemException do
        ok := True;
    end;
    CheckEquals(True,ok,Format('Duplicated item, i = %d',[i]));

    ok := False;
    try
      localObj.append(obj,ctDelete);
    except
      on e : ESDODuplicatedItemException do
        ok := True;
    end;
    CheckEquals(True,ok,Format('Duplicated item, i = %d',[i]));
  end;

  { The list is no longer sorted!
  for i := 0 to Pred(NBR_INST - 1) do begin
    obj := localObj.getDataObject(i);
    CheckEquals(True,PtrInt(obj) < PtrInt(localObj.getDataObject(i+1)));
  end;
  for i := ( 0 + 1 ) to Pred(NBR_INST) do begin
    obj := localObj.getDataObject(i);
    CheckEquals(True,PtrInt(obj) > PtrInt(localObj.getDataObject(i-1)));
  end;}
end;

class function TSDOChangedDataObjectList_Test.CreateFactory() : ISDODataFactory;
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
      locFactory.addProperty(locObj,s_integer_prop,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType], []);
      locFactory.addProperty(locObj,s_integer_propList,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsMany]);
      locFactory.addProperty(locObj,s_string_prop,sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
      locFactory.addProperty(locObj,s_string_propList,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsMany]);
  end;
begin
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
  Add_ObjectA(s_uri);

  Result := locFactory;
end;

class function TSDOChangedDataObjectList_Test.CreateObject() : ISDOChangedDataObjectListEx;
begin
  Result := TSDOChangedDataObjectList.Create();
end;

procedure TSDOChangedDataObjectList_Test.find();
var
  localObj : ISDOChangedDataObjectListEx;
  fact : ISDODataFactory;
  i, j : PtrInt;
  il: IInterfaceList;
  obj : ISDODataObject;
  NBR_INST : PtrInt;
begin
  Randomize();
  NBR_INST := RandomRange(100,1000);
  fact := CreateFactory();
  il := TInterfaceList.Create();

  localObj := CreateObject();

  for i := 0 to Pred(NBR_INST) do begin
    obj := fact.createNew(s_uri,s_type_object_A);
    localObj.append(obj,TChangeType( ( i mod 3 ) + 1 ));
    il.Add(obj);
  end;

  for i := 0 to Pred(NBR_INST) do begin
    CheckEquals(True,localObj.find(il[i] as ISDODataObject,j));
    Check(j >= 0);
    Check(j < NBR_INST);
  end;
end;

procedure TSDOChangedDataObjectList_Test.size();
var
  localObj : ISDOChangedDataObjectListEx;
  fact : ISDODataFactory;
  il : IInterfaceList;
begin
  fact := CreateFactory();
  localObj := CreateObject();

  CheckEquals(0,localObj.size());
  CheckEquals(0,localObj.size());

  il := TInterfaceList.Create();
  il.Add(fact.createNew(s_uri,s_type_object_A));
  localObj.append(il[il.Count - 1] as ISDODataObject,ctDelete);
    CheckEquals(1,localObj.size());
  il.Add(fact.createNew(s_uri,s_type_object_A));
  localObj.append(il[il.Count - 1] as ISDODataObject,ctCreate);
    CheckEquals(2,localObj.size());
  il.Add(fact.createNew(s_uri,s_type_object_A));
  localObj.append(il[il.Count - 1] as ISDODataObject,ctChange);
    CheckEquals(3,localObj.size());
end;

{ TChangeRecorder_Test }

procedure TChangeRecorder_Test.bool_change();
const
  LOCAL_PROP = s_bool_prop;
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);

  record_bool(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setBoolean(LOCAL_PROP,False);
    record_bool(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setBoolean(LOCAL_PROP,True);
    record_bool(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setNull(LOCAL_PROP);
    record_bool(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setBoolean(LOCAL_PROP,False);
    record_bool(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.unset(LOCAL_PROP);
    record_bool(locObj,LOCAL_PROP);
end;

procedure TChangeRecorder_Test.byte_change();
const
  LOCAL_PROP = s_byte_prop;
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);

  record_byte(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setByte(LOCAL_PROP,123);
    record_byte(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setByte(LOCAL_PROP,76);
    record_byte(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setNull(LOCAL_PROP);
    record_byte(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setByte(LOCAL_PROP,123);
    record_byte(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.unset(LOCAL_PROP);
    record_byte(locObj,LOCAL_PROP);
end;

procedure TChangeRecorder_Test.CheckEquals(expected, actual: TSDODate;
  msg: string; const AStrict: Boolean);
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

function TChangeRecorder_Test.CreateRecorder() : TChangeRecorder;
var
  ls : ISDOChangedDataObjectList;
begin
  ls := TSDOChangedDataObjectList.Create();
  FChangeSummary := TSDOChangeSummary.Create(ls);
  Result := TChangeRecorder.Create(ls, FChangeSummary);
end;

function IndexOf(const AObject : ISDODataObject; const AList : ISDOChangedDataObjectList) : PtrInt;
var
  i : PtrInt;
begin
  Result := -1;
  for i := 0 to Pred(AList.size()) do begin
    if ( AList.getDataObject(i) = AObject ) then begin
      Result := i;
      Break;
    end;
  end;
end;

class function TChangeRecorder_Test.Create_Factory() : ISDODataFactory;
var
  locFactory : ISDODataFactory;

  procedure Add_ObjectA(const AUri : string);
  var
    locObj : ISDOType;
  begin
    locFactory.AddType(AUri,s_type_object_A,[]);
    locFactory.AddType(AUri,s_type_object_B,[]);
    locFactory.AddType(AUri,s_type_object_C,[]);
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
      locFactory.addProperty(locObj,s_object_prop,AUri,s_type_object_B,[pfIsContainment]);
        locFactory.addProperty(locObj,s_object_ref_prop,AUri,s_type_object_C,[]);
  end;
begin
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
  Add_ObjectA(s_uri);

  Result := locFactory;
end;

procedure TChangeRecorder_Test.date_change();
const
  LOCAL_PROP = s_date_prop;
  VAL_1 : TSDODate = ( Date : 39000; HourOffset : 1; MinuteOffset : 2; );
  VAL_2 : TSDODate = ( Date : 45678; HourOffset : 9; MinuteOffset : 10; );
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);

  record_date(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setDate(LOCAL_PROP,VAL_1);
    record_date(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setDate(LOCAL_PROP,VAL_2);
    record_date(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setNull(LOCAL_PROP);
    record_date(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setDate(LOCAL_PROP,VAL_1);
    record_date(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.unset(LOCAL_PROP);
    record_date(locObj,LOCAL_PROP);
end;

procedure TChangeRecorder_Test.InitRecorder();
begin
  FreeAndNil(FRecorder);
  FChangeSummary := nil;
  FRecorder := CreateRecorder();
end;

procedure TChangeRecorder_Test.int_change();
const
  LOCAL_PROP = s_integer_prop;
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);

  record_int(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setInteger(LOCAL_PROP,1210);
    record_int(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setInteger(LOCAL_PROP,-76);
    record_int(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setNull(LOCAL_PROP);
    record_int(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setInteger(LOCAL_PROP,1210);
    record_int(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.unset(LOCAL_PROP);
    record_int(locObj,LOCAL_PROP);
end;

procedure TChangeRecorder_Test.multi_call_boolean();
var
  locObj : ISDODataObject;
  locProp : ISDOProperty;
  locChangeInfo : TDataObjectChangeInfo;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locProp := locObj.getProperty(s_bool_prop);

  FRecorder.recordChange(locObj,locProp);
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  locObj.setBoolean(locProp,False);
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  FRecorder.recordChange(locObj,locProp);
  CheckEquals(1,locChangeInfo.ChangeList.size());
end;

procedure TChangeRecorder_Test.multi_call_byte();
var
  locObj : ISDODataObject;
  locProp : ISDOProperty;
  locChangeInfo : TDataObjectChangeInfo;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locProp := locObj.getProperty(s_byte_prop);

  FRecorder.recordChange(locObj,locProp);
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  locObj.setByte(locProp,78);
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  FRecorder.recordChange(locObj,locProp);
  CheckEquals(1,locChangeInfo.ChangeList.size());
end;

procedure TChangeRecorder_Test.multi_call_date();
const
  VAL_1 : TSDODate = ( Date : 39000; HourOffset : 1; MinuteOffset : 2; );
var
  locObj : ISDODataObject;
  locProp : ISDOProperty;
  locChangeInfo : TDataObjectChangeInfo;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locProp := locObj.getProperty(s_date_prop);

  FRecorder.recordChange(locObj,locProp);
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  locObj.setDate(locProp,VAL_1);
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  FRecorder.recordChange(locObj,locProp);
  CheckEquals(1,locChangeInfo.ChangeList.size());
end;

procedure TChangeRecorder_Test.multi_call_int();
var
  locObj : ISDODataObject;
  locProp : ISDOProperty;
  locChangeInfo : TDataObjectChangeInfo;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locProp := locObj.getProperty(s_integer_prop);

  FRecorder.recordChange(locObj,locProp);
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  locObj.setInteger(locProp,1210);
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  FRecorder.recordChange(locObj,locProp);
  CheckEquals(1,locChangeInfo.ChangeList.size());
end;

procedure TChangeRecorder_Test.multi_call_string();
var
  locObj : ISDODataObject;
  locProp : ISDOProperty;
  locChangeInfo : TDataObjectChangeInfo;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locProp := locObj.getProperty(s_string_prop);

  FRecorder.recordChange(locObj,locProp);
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  locObj.setString(locProp,'qwerty#azerty');
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  FRecorder.recordChange(locObj,locProp);
  CheckEquals(1,locChangeInfo.ChangeList.size());
end;

{$IFDEF HAS_SDO_CHAR}
procedure TChangeRecorder_Test.record_char(
  const ADataObject: ISDODataObject;
  const APropName: string
);
var
  locProp : ISDOProperty;
  locValue : TSDOChar;
  locSetting : TValueSetting;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
  locChangeInfo : TDataObjectChangeInfo;
begin
  locProp := ADataObject.getProperty(APropName);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locProp),Format('Invalid property : "%s"',[APropName]));

  i := IndexOf(ADataObject,FRecorder.Store);
  CheckEquals(-1,i);

  locValue := ADataObject.getCharacter(APropName);
  FRecorder.recordChange(ADataObject,locProp);
  CheckEquals(locValue,ADataObject.getCharacter(APropName), 'The change recorder should not modify the object.');

  i := IndexOf(ADataObject,FRecorder.Store);
  Check( ( i >= 0 ) );

  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo));
  CheckEquals(PtrUInt(ADataObject), PtrUInt(locChangeInfo.DataObject));
  CheckEquals(Ord(ctChange), Ord(locChangeInfo.ChangeType));
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo.ChangeList));

  CheckEquals(1, locChangeInfo.ChangeList.size());
  locSetting := locChangeInfo.ChangeList.getItem(0);
  CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
  CheckEquals(ADataObject.isNull(locProp), locSetting.isNull());
  CheckEquals(ADataObject.isSet(locProp), locSetting.isSet());
  CheckEquals(ADataObject.getCharacter(locProp), locSetting.getCharacterValue());
end;

procedure TChangeRecorder_Test.multi_call_char();
const
  LOCAL_PROP = s_char_prop;
var
  locObj : ISDODataObject;
  locProp : ISDOProperty;
  locChangeInfo : TDataObjectChangeInfo;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locProp := locObj.getProperty(LOCAL_PROP);

  FRecorder.recordChange(locObj,locProp);
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  locObj.setCharacter(locProp,TSDOChar(78));
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  FRecorder.recordChange(locObj,locProp);
  CheckEquals(1,locChangeInfo.ChangeList.size());
end;

procedure TChangeRecorder_Test.char_change();
const
  LOCAL_PROP = s_char_prop;
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);

  record_char(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setCharacter(LOCAL_PROP,TSDOChar(123));
    record_char(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setCharacter(LOCAL_PROP,TSDOChar(76));
    record_char(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setNull(LOCAL_PROP);
    record_char(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setCharacter(LOCAL_PROP,TSDOChar(123));
    record_char(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.unset(LOCAL_PROP);
    record_char(locObj,LOCAL_PROP);
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_BYTES}
procedure TChangeRecorder_Test.record_bytes(
  const ADataObject: ISDODataObject;
  const APropName: string
);
var
  locProp : ISDOProperty;
  locValue : TSDOBytes;
  locSetting : TValueSetting;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
  locChangeInfo : TDataObjectChangeInfo;
begin
  locProp := ADataObject.getProperty(APropName);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locProp),Format('Invalid property : "%s"',[APropName]));

  i := IndexOf(ADataObject,FRecorder.Store);
  CheckEquals(-1,i);

  locValue := ADataObject.getBytes(APropName);
  FRecorder.recordChange(ADataObject,locProp);
  CheckEquals(locValue,ADataObject.getBytes(APropName), 'The change recorder should not modify the object.');

  i := IndexOf(ADataObject,FRecorder.Store);
  Check( ( i >= 0 ) );

  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo));
  CheckEquals(PtrUInt(ADataObject), PtrUInt(locChangeInfo.DataObject));
  CheckEquals(Ord(ctChange), Ord(locChangeInfo.ChangeType));
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo.ChangeList));

  CheckEquals(1, locChangeInfo.ChangeList.size());
  locSetting := locChangeInfo.ChangeList.getItem(0);
  CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
  CheckEquals(ADataObject.isNull(locProp), locSetting.isNull());
  CheckEquals(ADataObject.isSet(locProp), locSetting.isSet());
  CheckEquals(ADataObject.getBytes(locProp), locSetting.getBytesValue());
end;

procedure TChangeRecorder_Test.multi_call_bytes();
const
  LOCAL_PROP = s_bytes_prop;
var
  VAL_1 : TSDOBytes;

  procedure SetConstants();
  var
    v : TSDOBytes;
    k : Integer;
  begin
    SetLength(v,10);
    for k := 0 to High(v) do
      v[k] := k mod High(TSDOByte);
    VAL_1 := v;
  end;  
  
var
  locObj : ISDODataObject;
  locProp : ISDOProperty;
  locChangeInfo : TDataObjectChangeInfo;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
begin
  SetConstants();
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locProp := locObj.getProperty(LOCAL_PROP);

  FRecorder.recordChange(locObj,locProp);
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  locObj.setBytes(locProp,VAL_1);
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  FRecorder.recordChange(locObj,locProp);
  CheckEquals(1,locChangeInfo.ChangeList.size());
end;

procedure TChangeRecorder_Test.bytes_change();
const
  LOCAL_PROP = s_bytes_prop;
var
  VAL_1, VAL_2, VAL_3 : TSDOBytes;

  procedure SetConstants();
  var
    v : TSDOBytes;
    k : Integer;
  begin
    SetLength(v,10);
    for k := 0 to High(v) do
      v[k] := k mod High(TSDOByte);
    VAL_1 := v;
    v := nil;    

    VAL_2 := nil;

    SetLength(v,20);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(TSDOByte);
    VAL_3 := v; 
  end;
  
var
  locObj : ISDODataObject;
begin
  SetConstants();
  locObj := FFactory.createNew(s_uri,s_type_object_A);

  record_bytes(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setBytes(LOCAL_PROP,VAL_1);
    record_bytes(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setBytes(LOCAL_PROP,VAL_2);
    record_bytes(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setNull(LOCAL_PROP);
    record_bytes(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setBytes(LOCAL_PROP,VAL_3);
    record_bytes(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.unset(LOCAL_PROP);
    record_bytes(locObj,LOCAL_PROP);
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CURRENCY}
procedure TChangeRecorder_Test.record_currency(
  const ADataObject: ISDODataObject;
  const APropName: string
);
var
  locProp : ISDOProperty;
  locValue : TSDOCurrency;
  locSetting : TValueSetting;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
  locChangeInfo : TDataObjectChangeInfo;
begin
  locProp := ADataObject.getProperty(APropName);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locProp),Format('Invalid property : "%s"',[APropName]));

  i := IndexOf(ADataObject,FRecorder.Store);
  CheckEquals(-1,i);

  locValue := ADataObject.getCurrency(APropName);
  FRecorder.recordChange(ADataObject,locProp);
  CheckEquals(locValue,ADataObject.getCurrency(APropName), 'The change recorder should not modify the object.');

  i := IndexOf(ADataObject,FRecorder.Store);
  Check( ( i >= 0 ) );

  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo));
  CheckEquals(PtrUInt(ADataObject), PtrUInt(locChangeInfo.DataObject));
  CheckEquals(Ord(ctChange), Ord(locChangeInfo.ChangeType));
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo.ChangeList));

  CheckEquals(1, locChangeInfo.ChangeList.size());
  locSetting := locChangeInfo.ChangeList.getItem(0);
  CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
  CheckEquals(ADataObject.isNull(locProp), locSetting.isNull());
  CheckEquals(ADataObject.isSet(locProp), locSetting.isSet());
  CheckEquals(ADataObject.getCurrency(locProp), locSetting.getCurrencyValue());
end;

procedure TChangeRecorder_Test.multi_call_currency();
const
  LOCAL_PROP = s_currency_prop;
var
  locObj : ISDODataObject;
  locProp : ISDOProperty;
  locChangeInfo : TDataObjectChangeInfo;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locProp := locObj.getProperty(LOCAL_PROP);

  FRecorder.recordChange(locObj,locProp);
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  locObj.setCurrency(locProp,7886556852);
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  FRecorder.recordChange(locObj,locProp);
  CheckEquals(1,locChangeInfo.ChangeList.size());
end;

procedure TChangeRecorder_Test.currency_change();
const
  LOCAL_PROP = s_currency_prop;
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);

  record_currency(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setCurrency(LOCAL_PROP,123963258741);
    record_currency(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setCurrency(LOCAL_PROP,7614785236654);
    record_currency(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setNull(LOCAL_PROP);
    record_currency(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setCurrency(LOCAL_PROP,123321652);
    record_currency(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.unset(LOCAL_PROP);
    record_currency(locObj,LOCAL_PROP);
end;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
procedure TChangeRecorder_Test.record_double(
  const ADataObject: ISDODataObject;
  const APropName: string
);
var
  locProp : ISDOProperty;
  locValue : TSDODouble;
  locSetting : TValueSetting;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
  locChangeInfo : TDataObjectChangeInfo;
begin
  locProp := ADataObject.getProperty(APropName);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locProp),Format('Invalid property : "%s"',[APropName]));

  i := IndexOf(ADataObject,FRecorder.Store);
  CheckEquals(-1,i);

  locValue := ADataObject.getDouble(APropName);
  FRecorder.recordChange(ADataObject,locProp);
  CheckEquals(locValue,ADataObject.getDouble(APropName), 'The change recorder should not modify the object.');

  i := IndexOf(ADataObject,FRecorder.Store);
  Check( ( i >= 0 ) );

  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo));
  CheckEquals(PtrUInt(ADataObject), PtrUInt(locChangeInfo.DataObject));
  CheckEquals(Ord(ctChange), Ord(locChangeInfo.ChangeType));
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo.ChangeList));

  CheckEquals(1, locChangeInfo.ChangeList.size());
  locSetting := locChangeInfo.ChangeList.getItem(0);
  CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
  CheckEquals(ADataObject.isNull(locProp), locSetting.isNull());
  CheckEquals(ADataObject.isSet(locProp), locSetting.isSet());
  CheckEquals(ADataObject.getDouble(locProp), locSetting.getDoubleValue());
end;

procedure TChangeRecorder_Test.multi_call_double();
const
  LOCAL_PROP = s_double_prop;
var
  locObj : ISDODataObject;
  locProp : ISDOProperty;
  locChangeInfo : TDataObjectChangeInfo;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locProp := locObj.getProperty(LOCAL_PROP);

  FRecorder.recordChange(locObj,locProp);
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  locObj.setDouble(locProp,7886556852);
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  FRecorder.recordChange(locObj,locProp);
  CheckEquals(1,locChangeInfo.ChangeList.size());
end;

procedure TChangeRecorder_Test.double_change();
const
  LOCAL_PROP = s_double_prop;
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);

  record_double(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setDouble(LOCAL_PROP,123963258741);
    record_double(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setDouble(LOCAL_PROP,7614785236654);
    record_double(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setNull(LOCAL_PROP);
    record_double(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setDouble(LOCAL_PROP,123321652);
    record_double(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.unset(LOCAL_PROP);
    record_double(locObj,LOCAL_PROP);
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
procedure TChangeRecorder_Test.record_float(
  const ADataObject: ISDODataObject;
  const APropName: string
);
var
  locProp : ISDOProperty;
  locValue : TSDOFloat;
  locSetting : TValueSetting;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
  locChangeInfo : TDataObjectChangeInfo;
begin
  locProp := ADataObject.getProperty(APropName);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locProp),Format('Invalid property : "%s"',[APropName]));

  i := IndexOf(ADataObject,FRecorder.Store);
  CheckEquals(-1,i);

  locValue := ADataObject.getFloat(APropName);
  FRecorder.recordChange(ADataObject,locProp);
  CheckEquals(locValue,ADataObject.getFloat(APropName), 'The change recorder should not modify the object.');

  i := IndexOf(ADataObject,FRecorder.Store);
  Check( ( i >= 0 ) );

  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo));
  CheckEquals(PtrUInt(ADataObject), PtrUInt(locChangeInfo.DataObject));
  CheckEquals(Ord(ctChange), Ord(locChangeInfo.ChangeType));
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo.ChangeList));

  CheckEquals(1, locChangeInfo.ChangeList.size());
  locSetting := locChangeInfo.ChangeList.getItem(0);
  CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
  CheckEquals(ADataObject.isNull(locProp), locSetting.isNull());
  CheckEquals(ADataObject.isSet(locProp), locSetting.isSet());
  CheckEquals(ADataObject.getFloat(locProp), locSetting.getFloatValue());
end;

procedure TChangeRecorder_Test.multi_call_float();
const
  LOCAL_PROP = s_float_prop;
var
  locObj : ISDODataObject;
  locProp : ISDOProperty;
  locChangeInfo : TDataObjectChangeInfo;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locProp := locObj.getProperty(LOCAL_PROP);

  FRecorder.recordChange(locObj,locProp);
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  locObj.setFloat(locProp,7886556852);
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  FRecorder.recordChange(locObj,locProp);
  CheckEquals(1,locChangeInfo.ChangeList.size());
end;

procedure TChangeRecorder_Test.float_change();
const
  LOCAL_PROP = s_float_prop;
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);

  record_float(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setFloat(LOCAL_PROP,123963258741);
    record_float(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setFloat(LOCAL_PROP,7614785236654);
    record_float(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setNull(LOCAL_PROP);
    record_float(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setFloat(LOCAL_PROP,123321652);
    record_float(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.unset(LOCAL_PROP);
    record_float(locObj,LOCAL_PROP);
end;
{$ENDIF HAS_SDO_FLOAT}

{$IFDEF HAS_SDO_LONG}
procedure TChangeRecorder_Test.record_long(
  const ADataObject: ISDODataObject;
  const APropName: string
);
var
  locProp : ISDOProperty;
  locValue : TSDOLong;
  locSetting : TValueSetting;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
  locChangeInfo : TDataObjectChangeInfo;
begin
  locProp := ADataObject.getProperty(APropName);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locProp),Format('Invalid property : "%s"',[APropName]));

  i := IndexOf(ADataObject,FRecorder.Store);
  CheckEquals(-1,i);

  locValue := ADataObject.getLong(APropName);
  FRecorder.recordChange(ADataObject,locProp);
  CheckEquals(locValue,ADataObject.getLong(APropName), 'The change recorder should not modify the object.');

  i := IndexOf(ADataObject,FRecorder.Store);
  Check( ( i >= 0 ) );

  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo));
  CheckEquals(PtrUInt(ADataObject), PtrUInt(locChangeInfo.DataObject));
  CheckEquals(Ord(ctChange), Ord(locChangeInfo.ChangeType));
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo.ChangeList));

  CheckEquals(1, locChangeInfo.ChangeList.size());
  locSetting := locChangeInfo.ChangeList.getItem(0);
  CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
  CheckEquals(ADataObject.isNull(locProp), locSetting.isNull());
  CheckEquals(ADataObject.isSet(locProp), locSetting.isSet());
  CheckEquals(ADataObject.getLong(locProp), locSetting.getLongValue());
end;

procedure TChangeRecorder_Test.multi_call_long();
const
  LOCAL_PROP = s_long_prop;
var
  locObj : ISDODataObject;
  locProp : ISDOProperty;
  locChangeInfo : TDataObjectChangeInfo;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locProp := locObj.getProperty(LOCAL_PROP);

  FRecorder.recordChange(locObj,locProp);
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  locObj.setLong(locProp,TSDOLong(78865235820256852));
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  FRecorder.recordChange(locObj,locProp);
  CheckEquals(1,locChangeInfo.ChangeList.size());
end;

procedure TChangeRecorder_Test.long_change();
const
  LOCAL_PROP = s_long_prop;
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);

  record_long(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setLong(LOCAL_PROP,TSDOLong(123963258741));
    record_long(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setLong(LOCAL_PROP,TSDOLong(761478523699123654));
    record_long(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setNull(LOCAL_PROP);
    record_long(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setLong(LOCAL_PROP,TSDOLong(1233216549874741225));
    record_long(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.unset(LOCAL_PROP);
    record_long(locObj,LOCAL_PROP);
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure TChangeRecorder_Test.record_short(
  const ADataObject: ISDODataObject;
  const APropName: string
);
var
  locProp : ISDOProperty;
  locValue : TSDOShort;
  locSetting : TValueSetting;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
  locChangeInfo : TDataObjectChangeInfo;
begin
  locProp := ADataObject.getProperty(APropName);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locProp),Format('Invalid property : "%s"',[APropName]));

  i := IndexOf(ADataObject,FRecorder.Store);
  CheckEquals(-1,i);

  locValue := ADataObject.getShort(APropName);
  FRecorder.recordChange(ADataObject,locProp);
  CheckEquals(locValue,ADataObject.getShort(APropName), 'The change recorder should not modify the object.');

  i := IndexOf(ADataObject,FRecorder.Store);
  Check( ( i >= 0 ) );

  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo));
  CheckEquals(PtrUInt(ADataObject), PtrUInt(locChangeInfo.DataObject));
  CheckEquals(Ord(ctChange), Ord(locChangeInfo.ChangeType));
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo.ChangeList));

  CheckEquals(1, locChangeInfo.ChangeList.size());
  locSetting := locChangeInfo.ChangeList.getItem(0);
  CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
  CheckEquals(ADataObject.isNull(locProp), locSetting.isNull());
  CheckEquals(ADataObject.isSet(locProp), locSetting.isSet());
  CheckEquals(ADataObject.getShort(locProp), locSetting.getShortValue());
end;

procedure TChangeRecorder_Test.multi_call_short();
const
  LOCAL_PROP = s_short_prop;
var
  locObj : ISDODataObject;
  locProp : ISDOProperty;
  locChangeInfo : TDataObjectChangeInfo;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locProp := locObj.getProperty(LOCAL_PROP);

  FRecorder.recordChange(locObj,locProp);
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  locObj.setShort(locProp,TSDOShort(7882));
  i := IndexOf(locObj,FRecorder.Store);
  Check( ( i >= 0 ) );
  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckEquals(1,locChangeInfo.ChangeList.size());

  FRecorder.recordChange(locObj,locProp);
  FRecorder.recordChange(locObj,locProp);
  CheckEquals(1,locChangeInfo.ChangeList.size());
end;

procedure TChangeRecorder_Test.short_change();
const
  LOCAL_PROP = s_short_prop;
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);

  record_short(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setShort(LOCAL_PROP,TSDOShort(1239));
    record_short(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setShort(LOCAL_PROP,TSDOShort(-7614));
    record_short(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setNull(LOCAL_PROP);
    record_short(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setShort(LOCAL_PROP,TSDOShort(1225));
    record_short(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.unset(LOCAL_PROP);
    record_short(locObj,LOCAL_PROP);
end;
{$ENDIF HAS_SDO_SHORT}

procedure TChangeRecorder_Test.object_change_contained_prop();
const
  LOCAL_PROP = s_object_prop;
  LOCAL_PROP_CLASS = s_type_object_B;
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);

  record_obj(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setDataObject(LOCAL_PROP,FFactory.createNew(s_uri,LOCAL_PROP_CLASS));
    record_obj(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setDataObject(LOCAL_PROP,nil);
    record_obj(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setNull(LOCAL_PROP);
    record_obj(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setDataObject(LOCAL_PROP,FFactory.createNew(s_uri,LOCAL_PROP_CLASS));
    record_obj(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.unset(LOCAL_PROP);
    record_obj(locObj,LOCAL_PROP);
end;

procedure TChangeRecorder_Test.object_change_referenced_prop();
const
  LOCAL_PROP = s_object_ref_prop;
  LOCAL_PROP_CLASS = s_type_object_C;
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);

  record_obj(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setDataObject(LOCAL_PROP,FFactory.createNew(s_uri,LOCAL_PROP_CLASS));
    record_obj(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setDataObject(LOCAL_PROP,nil);
    record_obj(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setNull(LOCAL_PROP);
    record_obj(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setDataObject(LOCAL_PROP,FFactory.createNew(s_uri,LOCAL_PROP_CLASS));
    record_obj(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.unset(LOCAL_PROP);
    record_obj(locObj,LOCAL_PROP);
end;

procedure TChangeRecorder_Test.record_bool(
  const ADataObject: ISDODataObject;
  const APropName: string
);
var
  locProp : ISDOProperty;
  locValue : TSDOBoolean;
  locSetting : TValueSetting;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
  locChangeInfo : TDataObjectChangeInfo;
begin
  locProp := ADataObject.getProperty(APropName);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locProp),Format('Invalid property : "%s"',[APropName]));

  i := IndexOf(ADataObject,FRecorder.Store);
  CheckEquals(-1,i);

  locValue := ADataObject.getBoolean(APropName);
  FRecorder.recordChange(ADataObject,locProp);
  CheckEquals(locValue,ADataObject.getBoolean(APropName), 'The change recorder should not modify the object.');

  i := IndexOf(ADataObject,FRecorder.Store);
  Check( ( i >= 0 ) );

  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo));
  CheckEquals(PtrUInt(ADataObject), PtrUInt(locChangeInfo.DataObject));
  CheckEquals(Ord(ctChange), Ord(locChangeInfo.ChangeType));
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo.ChangeList));

  CheckEquals(1, locChangeInfo.ChangeList.size());
  locSetting := locChangeInfo.ChangeList.getItem(0);
  CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
  CheckEquals(ADataObject.isNull(locProp), locSetting.isNull());
  CheckEquals(ADataObject.isSet(locProp), locSetting.isSet());
  CheckEquals(ADataObject.getBoolean(locProp), locSetting.getBooleanValue());
end;

procedure TChangeRecorder_Test.record_byte(
  const ADataObject: ISDODataObject;
  const APropName: string
);
var
  locProp : ISDOProperty;
  locValue : TSDOByte;
  locSetting : TValueSetting;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
  locChangeInfo : TDataObjectChangeInfo;
begin
  locProp := ADataObject.getProperty(APropName);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locProp),Format('Invalid property : "%s"',[APropName]));

  i := IndexOf(ADataObject,FRecorder.Store);
  CheckEquals(-1,i);

  locValue := ADataObject.getByte(APropName);
  FRecorder.recordChange(ADataObject,locProp);
  CheckEquals(locValue,ADataObject.getByte(APropName), 'The change recorder should not modify the object.');

  i := IndexOf(ADataObject,FRecorder.Store);
  Check( ( i >= 0 ) );

  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo));
  CheckEquals(PtrUInt(ADataObject), PtrUInt(locChangeInfo.DataObject));
  CheckEquals(Ord(ctChange), Ord(locChangeInfo.ChangeType));
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo.ChangeList));

  CheckEquals(1, locChangeInfo.ChangeList.size());
  locSetting := locChangeInfo.ChangeList.getItem(0);
  CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
  CheckEquals(ADataObject.isNull(locProp), locSetting.isNull());
  CheckEquals(ADataObject.isSet(locProp), locSetting.isSet());
  CheckEquals(ADataObject.getByte(locProp), locSetting.getByteValue());
end;

procedure TChangeRecorder_Test.record_date(
  const ADataObject: ISDODataObject;
  const APropName: string
);
var
  locProp : ISDOProperty;
  locValue : TSDODateTime;
  locSetting : TValueSetting;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
  locChangeInfo : TDataObjectChangeInfo;
begin
  locProp := ADataObject.getProperty(APropName);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locProp),Format('Invalid property : "%s"',[APropName]));

  i := IndexOf(ADataObject,FRecorder.Store);
  CheckEquals(-1,i);

  locValue := ADataObject.getDate(APropName);
  FRecorder.recordChange(ADataObject,locProp);
  CheckEquals(locValue,ADataObject.getDate(APropName), 'The change recorder should not modify the object.');

  i := IndexOf(ADataObject,FRecorder.Store);
  Check( ( i >= 0 ) );

  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo));
  CheckEquals(PtrUInt(ADataObject), PtrUInt(locChangeInfo.DataObject));
  CheckEquals(Ord(ctChange), Ord(locChangeInfo.ChangeType));
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo.ChangeList));

  CheckEquals(1, locChangeInfo.ChangeList.size());
  locSetting := locChangeInfo.ChangeList.getItem(0);
  CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
  CheckEquals(ADataObject.isNull(locProp), locSetting.isNull());
  CheckEquals(ADataObject.isSet(locProp), locSetting.isSet());
  CheckEquals(ADataObject.getDate(locProp), locSetting.getDateValue());
end;

procedure TChangeRecorder_Test.record_int(
  const ADataObject : ISDODataObject;
  const APropName : string
);
var
  locProp : ISDOProperty;
  locValue : TSDOInteger;
  locSetting : TValueSetting;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
  locChangeInfo : TDataObjectChangeInfo;
begin
  locProp := ADataObject.getProperty(APropName);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locProp),Format('Invalid property : "%s"',[APropName]));

  i := IndexOf(ADataObject,FRecorder.Store);
  CheckEquals(-1,i);

  locValue := ADataObject.getInteger(APropName);
  FRecorder.recordChange(ADataObject,locProp);
  CheckEquals(locValue,ADataObject.getInteger(APropName), 'The change recorder should not modify the object.');

  i := IndexOf(ADataObject,FRecorder.Store);
  Check( ( i >= 0 ) );

  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo));
  CheckEquals(PtrUInt(ADataObject), PtrUInt(locChangeInfo.DataObject));
  CheckEquals(Ord(ctChange), Ord(locChangeInfo.ChangeType));
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo.ChangeList));

  CheckEquals(1, locChangeInfo.ChangeList.size());
  locSetting := locChangeInfo.ChangeList.getItem(0);
  CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
  CheckEquals(ADataObject.isNull(locProp), locSetting.isNull());
  CheckEquals(ADataObject.isSet(locProp), locSetting.isSet());
  CheckEquals(ADataObject.getInteger(locProp), locSetting.getIntegerValue());
end;

procedure TChangeRecorder_Test.record_list_int(   
  const ADataObject: ISDODataObject;
  const APropName: string
);
{var
  locProp : ISDOProperty;
  locValueList : ISDODataObjectList;
  locSetting : TValueSetting;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
  locChangeInfo : TDataObjectChangeInfo;}
begin
  {locProp := ADataObject.getProperty(APropName);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locProp),Format('Invalid property : "%s"',[APropName]));
  CheckEquals(True, locProp.isMany());

  i := IndexOf(ADataObject,FRecorder.Store);
  CheckEquals(-1,i);

  locValue := ADataObject.getInteger(APropName);
  FRecorder.recordChange(ADataObject,locProp);
  CheckEquals(locValue,ADataObject.getInteger(APropName), 'The change recorder should not modify the object.');

  i := IndexOf(ADataObject,FRecorder.Store);
  Check( ( i >= 0 ) );

  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo));
  CheckEquals(PtrUInt(ADataObject), PtrUInt(locChangeInfo.DataObject));
  CheckEquals(Ord(ctChange), Ord(locChangeInfo.ChangeType));
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo.ChangeList));

  CheckEquals(1, locChangeInfo.ChangeList.size());
  locSetting := locChangeInfo.ChangeList.getItem(0);
  CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
  CheckEquals(ADataObject.isNull(locProp), locSetting.isNull());
  CheckEquals(ADataObject.isSet(locProp), locSetting.isSet());
  CheckEquals(ADataObject.getInteger(locProp), locSetting.getIntegerValue());}
end;

procedure TChangeRecorder_Test.record_obj(
  const ADataObject: ISDODataObject;
  const APropName: string
);
var
  locProp : ISDOProperty;
  locValue : ISDODataObject;
  locSetting : TValueSetting;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
  locChangeInfo : TDataObjectChangeInfo;
begin
  locProp := ADataObject.getProperty(APropName);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locProp),Format('Invalid property : "%s"',[APropName]));

  i := IndexOf(ADataObject,FRecorder.Store);
  CheckEquals(-1,i);

  locValue := ADataObject.getDataObject(APropName);
  FRecorder.recordChange(ADataObject,locProp);
  CheckEquals(PtrUInt(locValue),PtrUInt(ADataObject.getDataObject(APropName)), 'The change recorder should not modify the object.');

  i := IndexOf(ADataObject,FRecorder.Store);
  Check( ( i >= 0 ) );

  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo));
  CheckEquals(PtrUInt(ADataObject), PtrUInt(locChangeInfo.DataObject));
  CheckEquals(Ord(ctChange), Ord(locChangeInfo.ChangeType));
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo.ChangeList));

  CheckEquals(1, locChangeInfo.ChangeList.size());
  locSetting := locChangeInfo.ChangeList.getItem(0);
  CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
  CheckEquals(ADataObject.isNull(locProp), locSetting.isNull());
  CheckEquals(ADataObject.isSet(locProp), locSetting.isSet());
  CheckEquals(PtrUInt(ADataObject.getDataObject(locProp)), PtrUInt(locSetting.getDataObjectValue()));
end;

procedure TChangeRecorder_Test.record_string(
  const ADataObject: ISDODataObject;
  const APropName: string
);
var
  locProp : ISDOProperty;
  locValue : TSDOString;
  locSetting : TValueSetting;
  i : PtrInt;
  locListX : ISDOChangedDataObjectListEx;
  locChangeInfo : TDataObjectChangeInfo;
begin
  locProp := ADataObject.getProperty(APropName);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locProp),Format('Invalid property : "%s"',[APropName]));

  i := IndexOf(ADataObject,FRecorder.Store);
  CheckEquals(-1,i);

  locValue := ADataObject.getString(APropName);
  FRecorder.recordChange(ADataObject,locProp);
  CheckEquals(locValue,ADataObject.getString(APropName), 'The change recorder should not modify the object.');

  i := IndexOf(ADataObject,FRecorder.Store);
  Check( ( i >= 0 ) );

  locListX := FRecorder.Store as ISDOChangedDataObjectListEx;
  locChangeInfo := locListX.getInfo(i);
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo));
  CheckEquals(PtrUInt(ADataObject), PtrUInt(locChangeInfo.DataObject));
  CheckEquals(Ord(ctChange), Ord(locChangeInfo.ChangeType));
  CheckNotEquals(PtrUInt(nil),PtrUInt(locChangeInfo.ChangeList));

  CheckEquals(1, locChangeInfo.ChangeList.size());
  locSetting := locChangeInfo.ChangeList.getItem(0);
  CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
  CheckEquals(ADataObject.isNull(locProp), locSetting.isNull());
  CheckEquals(ADataObject.isSet(locProp), locSetting.isSet());
  CheckEquals(ADataObject.getString(locProp), locSetting.getStringValue());
end;

procedure TChangeRecorder_Test.SetUp();
begin
  inherited;
  FRecorder := CreateRecorder();
  FFactory := Create_Factory();
end;

procedure TChangeRecorder_Test.string_change();
const
  LOCAL_PROP = s_string_prop;
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);

  record_string(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setString(LOCAL_PROP,RandomString(RandomRange(0,1000)));
    record_string(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setString(LOCAL_PROP,'');
    record_string(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setNull(LOCAL_PROP);
    record_string(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.setString(LOCAL_PROP,RandomString(RandomRange(0,1000)));
    record_string(locObj,LOCAL_PROP);

  InitRecorder();
  locObj.unset(LOCAL_PROP);
    record_string(locObj,LOCAL_PROP);
end;

procedure TChangeRecorder_Test.TearDown();
begin
  FFactory := nil;
  FreeAndNil(FRecorder);
  inherited;
end;

{ TSDOChangeSummary_Test }

class function TSDOChangeSummary_Test.Create_Factory() : ISDODataFactory;
var
  locFactory : ISDODataFactory;

  procedure Add_ObjectA(const AUri : string);
  var
    locObj : ISDOType;
  begin
    locFactory.AddType(AUri,s_type_object_A,[]);
    locObj := locFactory.getType(AUri,s_type_object_A);
      locFactory.addProperty(locObj,s_bool_prop,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[]);
        locFactory.addProperty(locObj,s_bool_propList,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsMany]);
      locFactory.addProperty(locObj,s_byte_prop,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
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
      locFactory.addProperty(locObj,s_date_prop,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType],[]);
        locFactory.addProperty(locObj,s_date_propList,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType],[pfIsMany]);
{$IFDEF HAS_SDO_DOUBLE}
      locFactory.addProperty(locObj,s_double_prop,sdo_namespace,SDOTypeDefaultTypeNames[DoubleType], []);
        locFactory.addProperty(locObj,s_double_propList,sdo_namespace,SDOTypeDefaultTypeNames[DoubleType],[pfIsMany]);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
      locFactory.addProperty(locObj,s_float_prop,sdo_namespace,SDOTypeDefaultTypeNames[FloatType], []);
        locFactory.addProperty(locObj,s_float_propList,sdo_namespace,SDOTypeDefaultTypeNames[FloatType],[pfIsMany]);
{$ENDIF HAS_SDO_FLOAT}
      locFactory.addProperty(locObj,s_integer_prop,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
        locFactory.addProperty(locObj,s_integer_propList,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsMany]);
{$IFDEF HAS_SDO_LONG}
      locFactory.addProperty(locObj,s_long_prop,sdo_namespace,SDOTypeDefaultTypeNames[LongType], []);
        locFactory.addProperty(locObj,s_long_propList,sdo_namespace,SDOTypeDefaultTypeNames[LongType],[pfIsMany]);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      locFactory.addProperty(locObj,s_short_prop,sdo_namespace,SDOTypeDefaultTypeNames[ShortType], []);
        locFactory.addProperty(locObj,s_short_propList,sdo_namespace,SDOTypeDefaultTypeNames[ShortType],[pfIsMany]);
{$ENDIF HAS_SDO_SHORT}
      locFactory.addProperty(locObj,s_string_prop,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
        locFactory.addProperty(locObj,s_string_propList,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsMany]);
  end;
begin
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
  Add_ObjectA(s_uri);

  Result := locFactory;
end;

function TSDOChangeSummary_Test.CreateRecorder(): TChangeRecorder;
var
  ls : ISDOChangedDataObjectList;
begin
  ls := TSDOChangedDataObjectList.Create();
  FChangeSummary := TSDOChangeSummary.Create(ls);
  Result := TChangeRecorder.Create(ls, FChangeSummary);
end;

procedure TSDOChangeSummary_Test.InitRecorder();
begin
  FChangeSummary := nil;
  FreeAndNil(FRecorder);
  FRecorder := CreateRecorder();
  //FChangeSummary := TSDOChangeSummary.Create(FRecorder.Store);
end;

procedure TSDOChangeSummary_Test.SetUp();
begin
  inherited;
  FRecorder := CreateRecorder();
  FFactory := Create_Factory();
  FFactoryX := Create_FactoryX();
  InitRecorder();
end;

procedure TSDOChangeSummary_Test.TearDown();
begin
  FChangeSummary := nil;
  FreeAndNil(FRecorder);
  FFactoryX := nil;
  FFactory := nil;
  inherited;
end;

procedure TSDOChangeSummary_Test.logging_state();
begin
  CheckEquals(False,FChangeSummary.isLogging(),'By default, the logging should b OFF');

  FChangeSummary.endLogging();
    CheckEquals(False,FChangeSummary.isLogging());
  FChangeSummary.endLogging();
    FChangeSummary.endLogging();
    CheckEquals(False,FChangeSummary.isLogging());

  FChangeSummary.beginLogging();
    CheckEquals(True,FChangeSummary.isLogging());
  FChangeSummary.beginLogging();
  FChangeSummary.beginLogging();
    CheckEquals(True,FChangeSummary.isLogging());

  FChangeSummary.endLogging();
    CheckEquals(False,FChangeSummary.isLogging());

  FChangeSummary.beginLogging();
    CheckEquals(True,FChangeSummary.isLogging());
end;

procedure TSDOChangeSummary_Test.isCreated();
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  CheckEquals(False,FChangeSummary.isCreated(nil));
  CheckEquals(False,FChangeSummary.isCreated(locObj));
  FRecorder.recordCreation(locObj);
    CheckEquals(True,FChangeSummary.isCreated(locObj));

  //-------------------------
  InitRecorder();
  FChangeSummary.beginLogging();
  CheckEquals(False,FChangeSummary.isCreated(locObj));
  FRecorder.recordChange(locObj,locObj.getProperty(s_integer_prop));
    CheckEquals(False,FChangeSummary.isCreated(locObj));

  //-------------------------
  InitRecorder();
  FChangeSummary.beginLogging();
  CheckEquals(False,FChangeSummary.isCreated(locObj));
  FRecorder.recordDeletion(locObj);
    CheckEquals(False,FChangeSummary.isCreated(locObj));
end;

procedure TSDOChangeSummary_Test.isModified();
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  CheckEquals(False,FChangeSummary.isModified(nil));
  CheckEquals(False,FChangeSummary.isModified(locObj));
  FRecorder.recordChange(locObj,locObj.getProperty(s_integer_prop));
    CheckEquals(True,FChangeSummary.isModified(locObj));

  //-------------------------
  InitRecorder();
  FChangeSummary.beginLogging();
  CheckEquals(False,FChangeSummary.isModified(locObj));
  FRecorder.recordCreation(locObj);
    CheckEquals(False,FChangeSummary.isModified(locObj));

  //-------------------------
  InitRecorder();
  FChangeSummary.beginLogging();
  CheckEquals(False,FChangeSummary.isModified(locObj));
  FRecorder.recordDeletion(locObj);
    CheckEquals(False,FChangeSummary.isModified(locObj));
end;

procedure TSDOChangeSummary_Test.isDeleted();
var
  locObj : ISDODataObject;
  ls : ISDOChangedDataObjectList;
  i, c, locFoundIdx : PtrInt;
  locFound : Boolean;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locObj.setInteger(s_integer_prop,RandomRange(-1210,1210));
  locObj.getList(s_integer_propList).append(RandomRange(-1210,1210));
  locObj.getList(s_integer_propList).append(RandomRange(-1210,1210));
  locObj.setString(s_string_prop,RandomString(1000));
  locObj.getList(s_string_propList).append(RandomString(1210));
  locObj.getList(s_string_propList).append(RandomString(1210));

  FChangeSummary.beginLogging();
    CheckEquals(False,FChangeSummary.isDeleted(nil));
    CheckEquals(False,FChangeSummary.isDeleted(locObj));
    FRecorder.recordDeletion(locObj);
      CheckEquals(True,FChangeSummary.isDeleted(locObj));
      ls := FChangeSummary.getChangedDataObjects();
      Check( ls.size() >  0 );
      c := ls.size();
      locFoundIdx := -1;
      locFound := False;
      for i := 0 to Pred(c) do begin
        if ( ls.getType(i) = ctDelete ) and ( locObj = ls.getDataObject(i) ) then begin
          locFoundIdx := i;
          locFound := True;
          Break;
        end;
      end;
      CheckEquals(True,locFound);
      if ( locFoundIdx < Pred(c) ) then begin
        locFound := False;
        for i := Succ(locFoundIdx) to Pred(c) do begin
          if ( ls.getType(i) = ctDelete ) and ( locObj = ls.getDataObject(i) ) then begin
            locFound := True;
            Break;
          end;
        end;
        CheckEquals(False, locFound, 'There should be _only one copy_ of the deleted object.');
      end;
end;

procedure TSDOChangeSummary_Test.getChangedDataObjects();
const LOCAL_PROP = s_integer_prop;
var
  ls : ISDOChangedDataObjectList;
  vobj : array of ISDODataObject;
  vct  : array of TChangeType;
  l, i, j : PtrInt;
  locProp : ISDOProperty;
begin
  ls := FChangeSummary.getChangedDataObjects();
  CheckNotEquals(PtrUInt(nil),PtrUInt(ls));
  CheckEquals(0,ls.size());

  l := RandomRange(0,1000);
  if ( l > 0 ) then begin
    SetLength(vobj,l);
    try
      SetLength(vct,l);
      for i := 0 to Pred(l) do begin
        vobj[i] := FFactory.createNew(s_uri,s_type_object_A);
        vobj[i].setInteger(s_integer_prop,i + 1);
        vct[i] := TChangeType(RandomRange(Ord(ctCreate),Ord(ctDelete)));
      end;
      locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP);
      for i := 0 to Pred(l) do begin
        case vct[i] of
          ctCreate   : FRecorder.recordCreation(vobj[i]);
          ctChange   : FRecorder.recordChange(vobj[i],vobj[i].getProperty(s_integer_prop));
          ctDelete   : FRecorder.recordDeletion(vobj[i]);
        end;
      end;

      ls := FChangeSummary.getChangedDataObjects();
      CheckEquals(l,ls.size());
      for i := 0 to Pred(l) do begin
        j := IndexOf(vobj[i],ls);
        Check( ( j >= 0 ) );
        CheckEquals(Ord(vct[i]), Ord(ls.getType(j)));
      end;
    finally
      SetLength(vct,0);
      SetLength(vobj,0);
    end;
  end;
end;

procedure TSDOChangeSummary_Test.getOldValues();
var
  locObjA, locObjB : ISDODataObject;
  ls : ISDOSettingList;
  locPropInt, locPropString : ISDOProperty;
begin
  locPropInt := FFactory.getType(s_uri,s_type_object_A).getProperty(s_integer_prop);
  locPropString := FFactory.getType(s_uri,s_type_object_A).getProperty(s_string_prop);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  ls := FChangeSummary.getOldValues(locObjA);
  CheckEquals(0, ls.size());
  ls := FChangeSummary.getOldValues(locObjB);
  CheckEquals(0, ls.size());

  locObjA.setInteger(locPropInt,1210);
  FRecorder.recordChange(locObjA,locPropInt);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

  locObjB.setInteger(locPropInt,1210);
  FRecorder.recordChange(locObjB,locPropInt);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

    ls := FChangeSummary.getOldValues(locObjB);
    CheckEquals(1,ls.size());

  locObjA.setString(locPropString,RandomString(1000));
  FRecorder.recordChange(locObjA,locPropString);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(2,ls.size());

    ls := FChangeSummary.getOldValues(locObjB);
    CheckEquals(1,ls.size());

  locObjB.setString(locPropString,RandomString(1000));
  FRecorder.recordChange(locObjB,locPropString);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(2,ls.size());

    ls := FChangeSummary.getOldValues(locObjB);
    CheckEquals(2,ls.size());
end;

procedure TSDOChangeSummary_Test.getOldValue();
var
  locObjA, locObjB : ISDODataObject;
  locPropInt, locPropString : ISDOProperty;
  locSetting : TValueSetting;
begin
  locPropInt := FFactory.getType(s_uri,s_type_object_A).getProperty(s_integer_prop);
  locPropString := FFactory.getType(s_uri,s_type_object_A).getProperty(s_string_prop);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  CheckEquals(PtrUInt(nil), PtrUInt(FChangeSummary.getOldValue(locObjA,locPropInt)));
  CheckEquals(PtrUInt(nil), PtrUInt(FChangeSummary.getOldValue(locObjA,locPropString)));

  locObjA.setInteger(locPropInt,1210);
  FRecorder.recordChange(locObjA,locPropInt);
    locSetting := FChangeSummary.getOldValue(locObjA,locPropInt);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locPropInt), PtrUInt(locSetting.getProperty()));
    CheckEquals(PtrUInt(nil), PtrUInt(FChangeSummary.getOldValue(locObjA,locPropString)));

  locObjB.setInteger(locPropInt,1210);
  FRecorder.recordChange(locObjB,locPropInt);
    locSetting := FChangeSummary.getOldValue(locObjB,locPropInt);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locPropInt), PtrUInt(locSetting.getProperty()));
    CheckEquals(PtrUInt(nil), PtrUInt(FChangeSummary.getOldValue(locObjB,locPropString)));


  locObjA.setString(locPropString,RandomString(1000));
  FRecorder.recordChange(locObjA,locPropString);
    locSetting := FChangeSummary.getOldValue(locObjA,locPropInt);
      CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
      CheckEquals(PtrUInt(locPropInt), PtrUInt(locSetting.getProperty()));
    locSetting := FChangeSummary.getOldValue(locObjA,locPropString);
      CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
      CheckEquals(PtrUInt(locPropString), PtrUInt(locSetting.getProperty()));


  locObjB.setString(locPropString,RandomString(1000));
  FRecorder.recordChange(locObjB,locPropString);
    locSetting := FChangeSummary.getOldValue(locObjB,locPropInt);
      CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
      CheckEquals(PtrUInt(locPropInt), PtrUInt(locSetting.getProperty()));
    locSetting := FChangeSummary.getOldValue(locObjB,locPropString);
      CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
      CheckEquals(PtrUInt(locPropString), PtrUInt(locSetting.getProperty()));
end;

type TAccesMode = ( amShallow, amDeep );
function find(
  const ADataObject : ISDODataObject;
  const AList : ISDOChangedDataObjectList;
  const ACompareMode : TAccesMode
) : Integer;
var
  i, c : PtrInt;
begin
  Result := -1;
  c := AList.size();
  if ( c > 0 ) then begin
    if ( ACompareMode = amShallow ) then begin
      for i := 0 to Pred(c) do begin
        if TSDOEqualityHelper.equalShallow(ADataObject,AList.getDataObject(i)) then begin
          Result := i;
          Break;
        end;
      end;
    end else begin
      for i := 0 to Pred(c) do begin
        if TSDOEqualityHelper.equal(ADataObject,AList.getDataObject(i)) then begin
          Result := i;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TSDOChangeSummary_Test.getOldContainer();
var
  locA, locB, locB1, locC : ISDODataObject;
  locCS : ISDOChangeSummary;
begin
  locA := FFactoryX.createNew(s_uri,s_type_object_A);
    locCS := locA.getChangeSummary();
    locCS.beginLogging();
  locB := FFactoryX.createNew(s_uri,s_type_object_B);
  locB1 := FFactoryX.createNew(s_uri,s_type_object_B);
  locC := FFactoryX.createNew(s_uri,s_type_object_C);

  CheckEquals(PtrUInt(nil), PtrUInt(locCS.getOldContainer(locA)));
  CheckEquals(PtrUInt(nil), PtrUInt(locCS.getOldContainer(locB)));
  CheckEquals(PtrUInt(nil), PtrUInt(locCS.getOldContainer(locC)));

  locA.setDataObject(s_object_prop,locB);
    CheckEquals(PtrUInt(nil), PtrUInt(locCS.getOldContainer(locA)));
    CheckEquals(PtrUInt(locA), PtrUInt(locCS.getOldContainer(locB)));
    CheckEquals(PtrUInt(nil), PtrUInt(locCS.getOldContainer(locC)));

  locA.setDataObject(s_object_prop,locB1);
    CheckEquals(PtrUInt(nil), PtrUInt(locCS.getOldContainer(locA)));
    CheckEquals(PtrUInt(nil), PtrUInt(locCS.getOldContainer(locB)), '"locB" should no longer be referenced by the ChangeSummary at this stade( create + delete = 0 ).');
    CheckEquals(PtrUInt(nil), PtrUInt(locCS.getOldContainer(locC)));

    locB1.setDataObject(s_object_prop,locC);
      CheckEquals(PtrUInt(nil), PtrUInt(locCS.getOldContainer(locA)));
      CheckEquals(PtrUInt(nil), PtrUInt(locCS.getOldContainer(locB)), '"locB" should no longer be referenced by the ChangeSummary at this stade( create + delete = 0 ).');
      CheckEquals(PtrUInt(locB1), PtrUInt(locCS.getOldContainer(locC)));
    locB1.setDataObject(s_object_prop,nil);
      CheckEquals(PtrUInt(nil), PtrUInt(locCS.getOldContainer(locA)));
      CheckEquals(PtrUInt(nil), PtrUInt(locCS.getOldContainer(locB)), '"locB" should no longer be referenced by the ChangeSummary at this stade( create + delete = 0 ).');
      CheckEquals(PtrUInt(nil), PtrUInt(locCS.getOldContainer(locC)), '"locC" should no longer be referenced by the ChangeSummary at this stade( create + delete = 0 ).');

end;

procedure TSDOChangeSummary_Test.getOldContainmentProperty();

  procedure Check_dont_have_old_prop(const ADataObject : ISDODataObject; const ACS : ISDOChangeSummary);
  var
    ok : Boolean;
  begin
    ok := False;
    try
      ACS.getOldContainmentProperty(ADataObject);
    except
      on e : ESDOInvalidStateOperationException do
        ok := True;
    end;
    Check(ok, 'this object should not have a Old Containment Property.');
  end;

var
  locA, locB, locB1, locC : ISDODataObject;
  locCS : ISDOChangeSummary;
begin
  locA := FFactoryX.createNew(s_uri,s_type_object_A);
    locCS := locA.getChangeSummary();
    locCS.beginLogging();
  locB := FFactoryX.createNew(s_uri,s_type_object_B);
  locB1 := FFactoryX.createNew(s_uri,s_type_object_B);
  locC := FFactoryX.createNew(s_uri,s_type_object_C);

  Check_dont_have_old_prop(locA,locCS);
  Check_dont_have_old_prop(locB,locCS);
  Check_dont_have_old_prop(locC,locCS);

  locA.setDataObject(s_object_prop,locB);
    Check_dont_have_old_prop(locA,locCS);
    Check_dont_have_old_prop(locC,locCS);

  locA.setDataObject(s_object_prop,locB1);
    Check_dont_have_old_prop(locA,locCS);
    Check_dont_have_old_prop(locB,locCS);
    Check_dont_have_old_prop(locC,locCS);

    locB1.setDataObject(s_object_prop,locC);
      Check_dont_have_old_prop(locA,locCS);
      Check_dont_have_old_prop(locB,locCS);
    locB1.setDataObject(s_object_prop,nil);
      Check_dont_have_old_prop(locA,locCS);
      Check_dont_have_old_prop(locB,locCS);
      Check_dont_have_old_prop(locC,locCS);
end;

class function TSDOChangeSummary_Test.Create_FactoryX(): ISDODataFactory;
var
  locFactory : ISDODataFactory;

  procedure Add_Objects(const AUri : string);
  var
    locObj : ISDOType;
  begin
    locFactory.AddType(AUri,s_type_object_C,[]);
    locObj := locFactory.getType(AUri,s_type_object_C);
      locFactory.addProperty(locObj,s_bool_prop,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType], []);
      locFactory.addProperty(locObj,s_byte_prop,sdo_namespace,SDOTypeDefaultTypeNames[ByteType], []);
{$IFDEF HAS_SDO_BYTES}
      locFactory.addProperty(locObj,s_bytes_prop,sdo_namespace,SDOTypeDefaultTypeNames[BytesType], []);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
      locFactory.addProperty(locObj,s_char_prop,sdo_namespace,SDOTypeDefaultTypeNames[CharacterType], []);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
      locFactory.addProperty(locObj,s_currency_prop,sdo_namespace,SDOTypeDefaultTypeNames[CurrencyType], []);
{$ENDIF HAS_SDO_CURRENCY}
      locFactory.addProperty(locObj,s_date_prop,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType], []);
{$IFDEF HAS_SDO_DOUBLE}
      locFactory.addProperty(locObj,s_double_prop,sdo_namespace,SDOTypeDefaultTypeNames[DoubleType], []);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
      locFactory.addProperty(locObj,s_float_prop,sdo_namespace,SDOTypeDefaultTypeNames[FloatType], []);
{$ENDIF HAS_SDO_FLOAT}
      locFactory.addProperty(locObj,s_integer_prop,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType], []);
{$IFDEF HAS_SDO_LONG}
      locFactory.addProperty(locObj,s_long_prop,sdo_namespace,SDOTypeDefaultTypeNames[LongType], []);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      locFactory.addProperty(locObj,s_short_prop,sdo_namespace,SDOTypeDefaultTypeNames[ShortType], []);
{$ENDIF HAS_SDO_SHORT}
      locFactory.addProperty(locObj,s_string_prop,sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);

    locFactory.AddType(AUri,s_type_object_B,[]);
    locObj := locFactory.getType(AUri,s_type_object_B);
      locFactory.addProperty(locObj,s_bool_prop,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType], []);
      locFactory.addProperty(locObj,s_byte_prop,sdo_namespace,SDOTypeDefaultTypeNames[ByteType], []);
{$IFDEF HAS_SDO_BYTES}
      locFactory.addProperty(locObj,s_bytes_prop,sdo_namespace,SDOTypeDefaultTypeNames[BytesType], []);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
      locFactory.addProperty(locObj,s_char_prop,sdo_namespace,SDOTypeDefaultTypeNames[CharacterType], []);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
      locFactory.addProperty(locObj,s_currency_prop,sdo_namespace,SDOTypeDefaultTypeNames[CurrencyType], []);
{$ENDIF HAS_SDO_CURRENCY}
      locFactory.addProperty(locObj,s_date_prop,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType], []);
{$IFDEF HAS_SDO_DOUBLE}
      locFactory.addProperty(locObj,s_double_prop,sdo_namespace,SDOTypeDefaultTypeNames[DoubleType], []);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
      locFactory.addProperty(locObj,s_float_prop,sdo_namespace,SDOTypeDefaultTypeNames[FloatType], []);
{$ENDIF HAS_SDO_FLOAT}
      locFactory.addProperty(locObj,s_integer_prop,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType], []);
{$IFDEF HAS_SDO_LONG}
      locFactory.addProperty(locObj,s_long_prop,sdo_namespace,SDOTypeDefaultTypeNames[LongType], []);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      locFactory.addProperty(locObj,s_short_prop,sdo_namespace,SDOTypeDefaultTypeNames[ShortType], []);
{$ENDIF HAS_SDO_SHORT}
      locFactory.addProperty(locObj,s_string_prop,sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
      locFactory.addProperty(locObj,s_object_prop,s_uri,s_type_object_C,[pfIsContainment]);

    locFactory.AddType(AUri,s_type_object_A,[]);
    locObj := locFactory.getType(AUri,s_type_object_A);
      locFactory.addProperty(locObj,s_bool_prop,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType], []);
        locFactory.addProperty(locObj,s_bool_propList,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsMany]);
      locFactory.addProperty(locObj,s_byte_prop,sdo_namespace,SDOTypeDefaultTypeNames[ByteType], []);
        locFactory.addProperty(locObj,s_byte_propList,sdo_namespace,SDOTypeDefaultTypeNames[ByteType], [pfIsMany]);
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
        locFactory.addProperty(locObj,s_date_propList,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType], [pfIsMany]);
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
      locFactory.addProperty(locObj,s_object_prop,s_uri,s_type_object_B,[pfIsContainment]);
      locFactory.addProperty(locObj,s_object_ref_prop,s_uri,s_type_object_B,[]);
      locFactory.addProperty(locObj,s_changesummary_prop,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

  end;
begin
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
  Add_Objects(s_uri);

  Result := locFactory;
end;

procedure TSDOChangeSummary_Test.isDeleted_nested();
  procedure Check_DeletedObject(const AObj : ISDODataObject; const AList : ISDOChangedDataObjectList);
  var
    i, c, locFoundIdx : PtrInt;
    locFound : Boolean;
  begin
    Check( AList.size() >  0 );
    c := AList.size();
    locFoundIdx := -1;
    locFound := False;
    for i := 0 to Pred(c) do begin
      if ( AList.getType(i) = ctDelete ) and ( AObj = AList.getDataObject(i) ) then begin
        locFoundIdx := i;
        locFound := True;
        Break;
      end;
    end;
    CheckEquals(True,locFound);
    if ( locFoundIdx < Pred(c) ) then begin
      locFound := False;
      for i := Succ(locFoundIdx) to Pred(c) do begin
        if ( AList.getType(i) = ctDelete ) and ( AObj = AList.getDataObject(i) ) then begin
          locFound := True;
          Break;
        end;
      end;
      CheckEquals(False, locFound, 'There should be _only one copy_ of the deleted object.');
    end;
  end;


var
  locA, locB, locC : ISDODataObject;
  ls : ISDOChangedDataObjectList;
begin
  locC := FFactoryX.createNew(s_uri,s_type_object_C);
    locC.setInteger(s_integer_prop,RandomRange(-12345,12456));
    locC.setString(s_string_prop,RandomString(1000));
  locB := FFactoryX.createNew(s_uri,s_type_object_B);
    locB.setInteger(s_integer_prop,RandomRange(-12345,12456));
    locB.setString(s_string_prop,RandomString(1000));
    locB.setDataObject(s_object_prop,locC);
  locA := FFactoryX.createNew(s_uri,s_type_object_A);
    locA.setInteger(s_integer_prop,RandomRange(-1210,1210));
    locA.getList(s_integer_propList).append(RandomRange(-1210,1210));
    locA.getList(s_integer_propList).append(RandomRange(-1210,1210));
    locA.setString(s_string_prop,RandomString(1000));
    locA.getList(s_string_propList).append(RandomString(1210));
    locA.getList(s_string_propList).append(RandomString(1210));
    locA.setDataObject(s_object_prop,locB);

  FChangeSummary.beginLogging();

    CheckEquals(False,FChangeSummary.isDeleted(locA));
    CheckEquals(False,FChangeSummary.isDeleted(locB));
    CheckEquals(False,FChangeSummary.isDeleted(locC));

    FRecorder.recordDeletion(locB);
      ls := FChangeSummary.getChangedDataObjects();
      Check_DeletedObject(locB,ls);

    FRecorder.recordDeletion(locA);
      CheckEquals(True,FChangeSummary.isDeleted(locA));
      CheckEquals(True,FChangeSummary.isDeleted(locB));
      CheckEquals(True,FChangeSummary.isDeleted(locC));
      ls := FChangeSummary.getChangedDataObjects();
      Check_DeletedObject(locA,ls);
end;

procedure TSDOChangeSummary_Test.undoChanges_simple();
var
  locObjA : ISDODataObject;
  locPropBool, locPropInt, locPropString : ISDOProperty;
  locSettingBool, locSettingInt, locSettingString : TValueSetting;
  ibuffer : TSDOInteger;
  sbuffer : TSDOString;
  bbuffer : TSDOBoolean;
  locCS : ISDOChangeSummary;
begin
  locPropBool := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_bool_prop);
  locPropInt := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_integer_prop);
  locPropString := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_string_prop);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setInteger(locPropInt,RandomRange(-1210,1210));
    locObjA.setString(locPropString,RandomString(1000));
    locSettingBool := nil;
    locSettingInt := nil;
    locSettingString := nil;
  try
    bbuffer := locObjA.getBoolean(locPropBool);
      locSettingBool := TValueSetting.Create(locObjA.isSet(locPropBool),locObjA.isNull(locPropBool),bbuffer,locPropBool,0);
    ibuffer := locObjA.getInteger(locPropInt);
      locSettingInt := TValueSetting.Create(locObjA.isSet(locPropInt),locObjA.isNull(locPropInt),ibuffer,locPropInt,0);
    sbuffer := locObjA.getString(locPropString);
      locSettingString := TValueSetting.Create(locObjA.isSet(locPropString),locObjA.isNull(locPropString),sbuffer,locPropString,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setBoolean(locPropBool,( ( RandomRange(0,1000) mod 2 ) = 0 ) );
        locObjA.setInteger(locPropInt,RandomRange(-1210,1210));
        locObjA.setString(locPropString,RandomString(1000));
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locPropBool,locSettingBool);
        check_value(locObjA,locPropInt,locSettingInt);
        check_value(locObjA,locPropString,locSettingString);

    FreeAndNil(locSettingBool);
    FreeAndNil(locSettingInt);
    FreeAndNil(locSettingString);
    bbuffer := locObjA.getBoolean(locPropBool);
      locSettingBool := TValueSetting.Create(locObjA.isSet(locPropBool),locObjA.isNull(locPropBool),bbuffer,locPropBool,0);
    ibuffer := locObjA.getInteger(locPropInt);
      locSettingInt := TValueSetting.Create(locObjA.isSet(locPropInt),locObjA.isNull(locPropInt),ibuffer,locPropInt,0);
    sbuffer := locObjA.getString(locPropString);
      locSettingString := TValueSetting.Create(locObjA.isSet(locPropString),locObjA.isNull(locPropString),sbuffer,locPropString,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setBoolean(locPropBool,( ( RandomRange(0,1000) mod 2 ) = 0 ) );
          locObjA.setBoolean(locPropBool,( ( RandomRange(0,1000) mod 2 ) = 0 ) );
        locObjA.setInteger(locPropInt,RandomRange(-1210,1210));
          locObjA.setInteger(locPropInt,RandomRange(-1210,1210));
        locObjA.setString(locPropString,RandomString(1000));
          locObjA.setString(locPropString,RandomString(1000));
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locPropBool,locSettingBool);
        check_value(locObjA,locPropInt,locSettingInt);
        check_value(locObjA,locPropString,locSettingString);
  finally
    FreeAndNil(locSettingBool);
    FreeAndNil(locSettingInt);
    FreeAndNil(locSettingString);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_object_contained_startWith_OBJ_OBJ();
var
  locObjA, locB, locBB, locA1, locB1 : ISDODataObject;
  locCS : ISDOChangeSummary;
begin
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setBoolean(s_bool_prop,( ( RandomRange(0,1000) mod 2 ) = 0 ) );
    locObjA.setByte(s_byte_prop,RandomRange(Low(TSDOByte),High(TSDOByte)));
    locObjA.setInteger(s_integer_prop,RandomRange(-1210,1210));
    locObjA.setString(s_string_prop,RandomString(1000));
    locB := FFactoryX.createNew(s_uri,s_type_object_B);
      locB.setBoolean(locB.getProperty(s_bool_prop),( ( RandomRange(0,1000) mod 2 ) = 0 ));
      locB.setByte(s_byte_prop,RandomRange(Low(TSDOByte),High(TSDOByte)));
      locB.setInteger(locB.getProperty(s_integer_prop),RandomRange(-1210,1210));
      locB.setString(locB.getProperty(s_string_prop),RandomString(1210));
    locObjA.setDataObject(s_object_prop,locB);
  locBB := FFactoryX.createNew(s_uri,s_type_object_B);
    locBB.setBoolean(locBB.getProperty(s_bool_prop),( ( RandomRange(0,1000) mod 2 ) = 0 ));
    locBB.setByte(s_byte_prop,RandomRange(Low(TSDOByte),High(TSDOByte)));
    locBB.setInteger(locBB.getProperty(s_integer_prop),RandomRange(-1210,1210));
    locBB.setString(locBB.getProperty(s_string_prop),RandomString(1210));

  locA1 := TSDOCopyHelper.copy(locObjA);
  locB1 := TSDOCopyHelper.copy(locB);

  locCS := locObjA.getChangeSummary();
    locCS.beginLogging();
      locObjA.setBoolean(s_bool_prop,( ( RandomRange(0,1000) mod 2 ) = 0 ) );
      locObjA.setByte(s_byte_prop,RandomRange(Low(TSDOByte),High(TSDOByte)));
      locObjA.setInteger(s_integer_prop,RandomRange(-1210,1210));
      locObjA.setString(s_string_prop,RandomString(1000));
        locB.setBoolean(locB.getProperty(s_bool_prop),( ( RandomRange(0,1000) mod 2 ) = 0 ));
        locB.setByte(s_byte_prop,RandomRange(Low(TSDOByte),High(TSDOByte)));
        locB.setInteger(locB.getProperty(s_integer_prop),RandomRange(-1210,1210));
        locB.setString(locB.getProperty(s_string_prop),RandomString(1210));
      locObjA.setDataObject(s_object_prop,locBB);
    locCS.endLogging();
    locCS.undoChanges();
      CheckEquals(0,locCS.getChangedDataObjects().size());
      Check(TSDOEqualityHelper.equal(locObjA,locA1));
      Check(TSDOEqualityHelper.equal(locObjA.getDataObject(s_object_prop),locB1));
end;

procedure TSDOChangeSummary_Test.undoChanges_object_contained_startWith_NIL_OBJ_OBJ();
var
  locObjA, locB, locBB, locA1, locB1 : ISDODataObject;
  locPropBool, locPropInt, locPropString, locPropObj_propB : ISDOProperty;
  locCS : ISDOChangeSummary;
begin
  locPropBool := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_bool_prop);
  locPropInt := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_integer_prop);
  locPropString := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_string_prop);
  locPropObj_propB := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_object_prop);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
  locB := FFactoryX.createNew(s_uri,s_type_object_B);

  locA1 := TSDOCopyHelper.copy(locObjA);
  locB1 := TSDOCopyHelper.copy(locB);

  locCS := locObjA.getChangeSummary();
  locCS.beginLogging();
      locObjA.setBoolean(locPropBool,( ( RandomRange(0,1000) mod 2 ) = 0 ) );
      locObjA.setInteger(locPropInt,RandomRange(-1210,1210));
      locObjA.setString(locPropString,RandomString(1000));

        locB.setBoolean(locB.getProperty(s_bool_prop),( ( RandomRange(0,1000) mod 2 ) = 0 ));
        locB.setInteger(locB.getProperty(s_integer_prop),RandomRange(-1210,1210));
        locB.setString(locB.getProperty(s_string_prop),RandomString(1210));
      locObjA.setDataObject(locPropObj_propB,locB);
    locBB := locObjA.createDataObject(s_object_prop);
      locBB.setBoolean(locBB.getProperty(s_bool_prop),( ( RandomRange(0,1000) mod 2 ) = 0 ));
      locBB.setInteger(locBB.getProperty(s_integer_prop),RandomRange(-1210,1210));
      locBB.setString(locBB.getProperty(s_string_prop),RandomString(1210));

  locCS.endLogging();
  locCS.undoChanges();
      CheckEquals(0,locCS.getChangedDataObjects().size());
      Check(TSDOEqualityHelper.equal(locObjA,locA1));
      Check(TSDOEqualityHelper.equal(locObjA.getDataObject(locPropObj_propB),nil));
end;

procedure TSDOChangeSummary_Test.getOldXpath();
var
  locObjA, locB, locC, locBB : ISDODataObject;
  locPropObj_propB : ISDOProperty;
  locCS : ISDOChangeSummary;
begin
  locPropObj_propB := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_object_prop);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
  locB := FFactoryX.createNew(s_uri,s_type_object_B);

  locCS := locObjA.getChangeSummary();
  locCS.beginLogging();
    CheckEquals('',locCS.getOldXpath(locObjA));
    CheckEquals('',locCS.getOldXpath(locB));
    locObjA.setDataObject(locPropObj_propB,locB);
      CheckEquals('',locCS.getOldXpath(locObjA));
      CheckEquals('',locCS.getOldXpath(locB));
      locBB := locObjA.createDataObject(locPropObj_propB);
        CheckEquals('',locCS.getOldXpath(locObjA));
        CheckEquals('',locCS.getOldXpath(locB));
        CheckEquals('',locCS.getOldXpath(locBB));
        locC := locBB.createDataObject(s_object_prop);
          CheckEquals('',locCS.getOldXpath(locC));
          locBB.setDataObject(s_object_prop,nil);
          CheckEquals('',locCS.getOldXpath(locB));
          CheckEquals('',locCS.getOldXpath(locC));
end;

procedure TSDOChangeSummary_Test.logging_state_2();
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();
    FRecorder.recordCreation(locObj);
      Check(FChangeSummary.getChangedDataObjects().size() > 0);
  FChangeSummary.endLogging();
    Check(FChangeSummary.getChangedDataObjects().size() > 0);
    FChangeSummary.beginLogging();
      CheckEquals(0,FChangeSummary.getChangedDataObjects().size(), 'After beginLoggin->endLogging->begginLoggin the changes list must be empty');
end;

procedure TSDOChangeSummary_Test.check_value(
  const AObj: ISDODataObject;
  const AProp: ISDOProperty;
  const AValue: TValueSetting
);
begin
  CheckEquals(AValue.isSet(), AObj.isSet(AProp));
  CheckEquals(AValue.isNull(), AObj.isNull(AProp));
  if not AValue.isNull() then begin
    case AProp.getTypeEnum() of
      BooleanType  : CheckEquals(AValue.getBooleanValue(), AObj.getBoolean(AProp));
      ByteType     : CheckEquals(AValue.getByteValue(), AObj.getByte(AProp));
{$IFDEF HAS_SDO_BYTES}
      BytesType    : CheckEquals(AValue.getBytesValue(), AObj.getBytes(AProp));
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
      CharacterType: CheckEquals(AValue.getCharacterValue(), AObj.getCharacter(AProp));
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
      CurrencyType : CheckEquals(AValue.getCurrencyValue(), AObj.getCurrency(AProp));
{$ENDIF HAS_SDO_CURRENCY}
      DateTimeType : CheckEquals(AValue.getDateValue(), AObj.getDate(AProp));
{$IFDEF HAS_SDO_DOUBLE}
      DoubleType   : CheckEquals(AValue.getDoubleValue(), AObj.getDouble(AProp));
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
      FloatType    : CheckEquals(AValue.getFloatValue(), AObj.getFloat(AProp));
{$ENDIF HAS_SDO_FLOAT}
      IntegerType  : CheckEquals(AValue.getIntegerValue(), AObj.getInteger(AProp));
{$IFDEF HAS_SDO_LONG}
      LongType     : CheckEquals(AValue.getLongValue(), AObj.getLong(AProp));
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      ShortType    : CheckEquals(AValue.getShortValue(), AObj.getShort(AProp));
{$ENDIF HAS_SDO_SHORT}
      ObjectType   : Check(TSDOEqualityHelper.equal(AValue.getDataObjectValue(),AObj.getDataObject(AProp)));
      StringType   : CheckEquals(AValue.getStringValue(), AObj.getString(AProp));
      else
        Assert(False,'NOT implemented.');
    end;
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_unset();
var
  locObjA : ISDODataObject;
  locPropBool, locPropInt, locPropString : ISDOProperty;
  locSettingBool, locSettingInt, locSettingString : TValueSetting;
  ibuffer : TSDOInteger;
  sbuffer : TSDOString;
  bbuffer : TSDOBoolean;
  locCS : ISDOChangeSummary;
begin
  locPropBool := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_bool_prop);
  locPropInt := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_integer_prop);
  locPropString := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_string_prop);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setBoolean(locPropBool,( ( RandomRange(0,1000) mod 2 ) = 0 ) );
    locObjA.setInteger(locPropInt,RandomRange(-1210,1210));
    locObjA.setString(locPropString,RandomString(1000));
    locSettingBool := nil;
    locSettingInt := nil;
    locSettingString := nil;
  try
    bbuffer := locObjA.getBoolean(locPropBool);
      locSettingBool := TValueSetting.Create(locObjA.isSet(locPropBool),locObjA.isNull(locPropBool),bbuffer,locPropBool,0);
    ibuffer := locObjA.getInteger(locPropInt);
      locSettingInt := TValueSetting.Create(locObjA.isSet(locPropInt),locObjA.isNull(locPropInt),ibuffer,locPropInt,0);
    sbuffer := locObjA.getString(locPropString);
      locSettingString := TValueSetting.Create(locObjA.isSet(locPropString),locObjA.isNull(locPropString),sbuffer,locPropString,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.unset(locPropBool);
        locObjA.unset(locPropInt);
        locObjA.unset(locPropString);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locPropBool,locSettingBool);
        check_value(locObjA,locPropInt,locSettingInt);
        check_value(locObjA,locPropString,locSettingString);
  finally
    FreeAndNil(locSettingBool);
    FreeAndNil(locSettingInt);
    FreeAndNil(locSettingString);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_setnull();
var
  locObjA : ISDODataObject;
  locPropBool, locPropInt, locPropString : ISDOProperty;
  locSettingBool, locSettingInt, locSettingString : TValueSetting;
  ibuffer : TSDOInteger;
  sbuffer : TSDOString;
  bbuffer : TSDOBoolean;
  locCS : ISDOChangeSummary;
begin
  locPropBool := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_bool_prop);
  locPropInt := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_integer_prop);
  locPropString := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_string_prop);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setBoolean(locPropBool,( ( RandomRange(0,1000) mod 2 ) = 0 ) );
    locObjA.setInteger(locPropInt,RandomRange(-1210,1210));
    locObjA.setString(locPropString,RandomString(1000));
    locSettingBool := nil;
    locSettingInt := nil;
    locSettingString := nil;
  try
    bbuffer := locObjA.getBoolean(locPropBool);
      locSettingBool := TValueSetting.Create(locObjA.isSet(locPropBool),locObjA.isNull(locPropBool),bbuffer,locPropBool,0);
    ibuffer := locObjA.getInteger(locPropInt);
      locSettingInt := TValueSetting.Create(locObjA.isSet(locPropInt),locObjA.isNull(locPropInt),ibuffer,locPropInt,0);
    sbuffer := locObjA.getString(locPropString);
      locSettingString := TValueSetting.Create(locObjA.isSet(locPropString),locObjA.isNull(locPropString),sbuffer,locPropString,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setNull(locPropBool);
        locObjA.setNull(locPropInt);
        locObjA.setNull(locPropString);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locPropBool,locSettingBool);
        check_value(locObjA,locPropInt,locSettingInt);
        check_value(locObjA,locPropString,locSettingString);
  finally
    FreeAndNil(locSettingBool);
    FreeAndNil(locSettingInt);
    FreeAndNil(locSettingString);
  end;
end;

procedure TSDOChangeSummary_Test.getOldValue_object_prop();
var
  locObj1, locObj2 : ISDODataObject;
  locPropCont, locPropRef : ISDOProperty;
  locSetting : TValueSetting;
begin
  locPropCont := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_object_prop);
  locPropRef := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_object_ref_prop);
  locObj1 := FFactoryX.createNew(s_uri,s_type_object_A);
  locObj2 := FFactoryX.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  CheckEquals(PtrUInt(nil), PtrUInt(FChangeSummary.getOldValue(locObj1,locPropCont)));
  CheckEquals(PtrUInt(nil), PtrUInt(FChangeSummary.getOldValue(locObj1,locPropRef)));

  locObj1.setDataObject(locPropCont,FFactoryX.createNew(s_uri,s_type_object_B));
  FRecorder.recordChange(locObj1,locPropCont);
    locSetting := FChangeSummary.getOldValue(locObj1,locPropCont);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locPropCont), PtrUInt(locSetting.getProperty()));
    CheckEquals(PtrUInt(nil), PtrUInt(FChangeSummary.getOldValue(locObj1,locPropRef)));

  locObj2.setDataObject(locPropCont,FFactoryX.createNew(s_uri,s_type_object_B));
  FRecorder.recordChange(locObj2,locPropCont);
    locSetting := FChangeSummary.getOldValue(locObj2,locPropCont);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locPropCont), PtrUInt(locSetting.getProperty()));
    CheckEquals(PtrUInt(nil), PtrUInt(FChangeSummary.getOldValue(locObj2,locPropRef)));

  locObj1.setDataObject(locPropRef,FFactoryX.createNew(s_uri,s_type_object_B));
  FRecorder.recordChange(locObj1,locPropRef);
    locSetting := FChangeSummary.getOldValue(locObj1,locPropCont);
      CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
      CheckEquals(PtrUInt(locPropCont), PtrUInt(locSetting.getProperty()));
    locSetting := FChangeSummary.getOldValue(locObj1,locPropRef);
      CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
      CheckEquals(PtrUInt(locPropRef), PtrUInt(locSetting.getProperty()));

  locObj2.setDataObject(locPropRef,FFactoryX.createNew(s_uri,s_type_object_B));
  FRecorder.recordChange(locObj2,locPropRef);
    locSetting := FChangeSummary.getOldValue(locObj2,locPropCont);
      CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
      CheckEquals(PtrUInt(locPropCont), PtrUInt(locSetting.getProperty()));
    locSetting := FChangeSummary.getOldValue(locObj2,locPropRef);
      CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
      CheckEquals(PtrUInt(locPropRef), PtrUInt(locSetting.getProperty()));
end;

procedure TSDOChangeSummary_Test.getOldValues_object_prop();
var
  locObjA, locObjB : ISDODataObject;
  ls : ISDOSettingList;
  locPropInt, locPropString, locPropObjCont, locPropObjRef : ISDOProperty;
begin
  locPropInt := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_integer_prop);
  locPropString := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_string_prop);
  locPropObjCont := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_object_prop);
  locPropObjRef := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_object_ref_prop);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
  locObjB := FFactoryX.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  ls := FChangeSummary.getOldValues(locObjA);
  CheckEquals(0, ls.size());
  ls := FChangeSummary.getOldValues(locObjB);
  CheckEquals(0, ls.size());

  locObjA.setInteger(locPropInt,1210);
  FRecorder.recordChange(locObjA,locPropInt);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

  locObjB.setInteger(locPropInt,1210);
  FRecorder.recordChange(locObjB,locPropInt);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

    ls := FChangeSummary.getOldValues(locObjB);
    CheckEquals(1,ls.size());

  locObjA.setString(locPropString,RandomString(1000));
  FRecorder.recordChange(locObjA,locPropString);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(2,ls.size());

    ls := FChangeSummary.getOldValues(locObjB);
    CheckEquals(1,ls.size());

  locObjB.setString(locPropString,RandomString(1000));
  FRecorder.recordChange(locObjB,locPropString);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(2,ls.size());

    ls := FChangeSummary.getOldValues(locObjB);
    CheckEquals(2,ls.size());

  locObjA.setDataObject(locPropObjCont,FFactoryX.createNew(s_uri,s_type_object_B));
  FRecorder.recordChange(locObjA,locPropObjCont);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(3,ls.size());

    ls := FChangeSummary.getOldValues(locObjB);
    CheckEquals(2,ls.size());

  locObjB.setDataObject(locPropObjCont,FFactoryX.createNew(s_uri,s_type_object_B));
  FRecorder.recordChange(locObjB,locPropObjCont);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(3,ls.size());

    ls := FChangeSummary.getOldValues(locObjB);
    CheckEquals(3,ls.size());

  locObjA.setDataObject(locPropObjRef,FFactoryX.createNew(s_uri,s_type_object_B));
  FRecorder.recordChange(locObjA,locPropObjRef);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(4,ls.size());

    ls := FChangeSummary.getOldValues(locObjB);
    CheckEquals(3,ls.size());

  locObjB.setDataObject(locPropObjRef,FFactoryX.createNew(s_uri,s_type_object_B));
  FRecorder.recordChange(locObjB,locPropObjRef);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(4,ls.size());

    ls := FChangeSummary.getOldValues(locObjB);
    CheckEquals(4,ls.size());
end;

procedure TSDOChangeSummary_Test.undoChanges_object_referenced_startWIth_OBJ_OBJ();
var
  locObjA, locB, locBB, locA1, locB1 : ISDODataObject;
  locPropBool, locPropInt, locPropString, locPropObj_propB : ISDOProperty;
  locCS : ISDOChangeSummary;
begin
  locPropBool := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_bool_prop);
  locPropInt := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_integer_prop);
  locPropString := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_string_prop);
  locPropObj_propB := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_object_ref_prop);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setBoolean(locPropBool,( ( RandomRange(0,1000) mod 2 ) = 0 ) );
    locObjA.setInteger(locPropInt,RandomRange(-1210,1210));
    locObjA.setString(locPropString,RandomString(1000));
    locB := (*the object must be in the containment tree*)
               locObjA.createDataObject(s_object_prop);
      locB.setBoolean(locB.getProperty(s_bool_prop),( ( RandomRange(0,1000) mod 2 ) = 0 ));
      locB.setInteger(locB.getProperty(s_integer_prop),RandomRange(-1210,1210));
      locB.setString(locB.getProperty(s_string_prop),RandomString(1210));
    (* This is necessary because locObjA.createDataObject(s_object_prop) is for "s_object_prop" not for "s_object_ref_prop"*)
    locObjA.setDataObject(locPropObj_propB,locB);
  locBB := FFactoryX.createNew(s_uri,s_type_object_B);
    locBB.setBoolean(locBB.getProperty(s_bool_prop),( ( RandomRange(0,1000) mod 2 ) = 0 ));
    locBB.setInteger(locBB.getProperty(s_integer_prop),RandomRange(-1210,1210));
    locBB.setString(locBB.getProperty(s_string_prop),RandomString(1210));

  locA1 := TSDOCopyHelper.copy(locObjA);
  locB1 := TSDOCopyHelper.copy(locB);

  locCS := locObjA.getChangeSummary();
    locCS.beginLogging();
      locObjA.setBoolean(locPropBool,( ( RandomRange(0,1000) mod 2 ) = 0 ) );
      locObjA.setInteger(locPropInt,RandomRange(-1210,1210));
      locObjA.setString(locPropString,RandomString(1000));
      locObjA.setDataObject(locPropObj_propB,locBB);
    locCS.endLogging();
    locCS.undoChanges();
      CheckEquals(0,locCS.getChangedDataObjects().size());
      //Check(TSDOEqualityHelper.equal(locObjA,locA1));
      CheckEquals(PtrUInt(locObjA.getDataObject(s_object_prop)), PtrUInt(locObjA.getDataObject(locPropObj_propB)));
      //Check(TSDOEqualityHelper.equal(locObjA.getDataObject(locPropObj_propB),locB1));
end;

procedure TSDOChangeSummary_Test.undoChanges_object_referenced_startWith_NIL_OBJ();
var
  locObjA, locB, locBB, locA1, locB1 : ISDODataObject;
  locPropBool, locPropInt, locPropString, locPropObj_propB : ISDOProperty;
  locCS : ISDOChangeSummary;
begin
  locPropBool := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_bool_prop);
  locPropInt := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_integer_prop);
  locPropString := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_string_prop);
  locPropObj_propB := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_object_ref_prop);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
  locBB := (*the object must be in the containment tree*)
             locObjA.createDataObject(s_object_prop);

  locA1 := TSDOCopyHelper.copy(locObjA);
  locB1 := TSDOCopyHelper.copy(locB);

  locCS := locObjA.getChangeSummary();
  locCS.beginLogging();
      locObjA.setBoolean(locPropBool,( ( RandomRange(0,1000) mod 2 ) = 0 ) );
      locObjA.setInteger(locPropInt,RandomRange(-1210,1210));
      locObjA.setString(locPropString,RandomString(1000));
      locObjA.setDataObject(locPropObj_propB,locB);
    locObjA.setDataObject(locPropObj_propB,locBB);
  locCS.endLogging();
  locCS.undoChanges();
      CheckEquals(0,locCS.getChangedDataObjects().size());
      Check(TSDOEqualityHelper.equal(locObjA,locA1));
      Check(TSDOEqualityHelper.equal(locObjA.getDataObject(locPropObj_propB),nil));
end;

procedure TSDOChangeSummary_Test.undoChanges_object_contained_startWith_OBJ_OBJ_OBJ();
var
  locObjA, locB, locBB, locA1, locB1 : ISDODataObject;
  locPropBool, locPropInt, locPropString, locPropObj_propB : ISDOProperty;
  locCS : ISDOChangeSummary;
begin
  locPropBool := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_bool_prop);
  locPropInt := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_integer_prop);
  locPropString := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_string_prop);
  locPropObj_propB := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_object_prop);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setBoolean(locPropBool,( ( RandomRange(0,1000) mod 2 ) = 0 ) );
    locObjA.setInteger(locPropInt,RandomRange(-1210,1210));
    locObjA.setString(locPropString,RandomString(1000));
    locB := FFactoryX.createNew(s_uri,s_type_object_B);
      locB.setBoolean(locB.getProperty(s_bool_prop),( ( RandomRange(0,1000) mod 2 ) = 0 ));
      locB.setInteger(locB.getProperty(s_integer_prop),RandomRange(-1210,1210));
      locB.setString(locB.getProperty(s_string_prop),RandomString(1210));
    locObjA.setDataObject(locPropObj_propB,locB);
  locBB := FFactoryX.createNew(s_uri,s_type_object_B);
    locBB.setBoolean(locBB.getProperty(s_bool_prop),( ( RandomRange(0,1000) mod 2 ) = 0 ));
    locBB.setInteger(locBB.getProperty(s_integer_prop),RandomRange(-1210,1210));
    locBB.setString(locBB.getProperty(s_string_prop),RandomString(1210));

  locA1 := TSDOCopyHelper.copy(locObjA);
  locB1 := TSDOCopyHelper.copy(locB);

  locCS := locObjA.getChangeSummary();
    locCS.beginLogging();
      locObjA.setBoolean(locPropBool,( ( RandomRange(0,1000) mod 2 ) = 0 ) );
      locObjA.setInteger(locPropInt,RandomRange(-1210,1210));
      locObjA.setString(locPropString,RandomString(1000));
        {locB.setBoolean(locB.getProperty(s_bool_prop),( ( RandomRange(0,1000) mod 2 ) = 0 ));
        locB.setInteger(locB.getProperty(s_integer_prop),RandomRange(-1210,1210));
        locB.setString(locB.getProperty(s_string_prop),RandomString(1210)); }
      locObjA.setDataObject(locPropObj_propB,locBB);
      locObjA.createDataObject(locPropObj_propB);
    locCS.endLogging();
    locCS.undoChanges();
      CheckEquals(0,locCS.getChangedDataObjects().size());
      Check(TSDOEqualityHelper.equal(locObjA,locA1));
      Check(TSDOEqualityHelper.equal(locObjA.getDataObject(locPropObj_propB),locB1));
end;

procedure TSDOChangeSummary_Test.isCreated_create_delete();
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  CheckEquals(False,FChangeSummary.isCreated(nil));
  CheckEquals(False,FChangeSummary.isCreated(locObj));
  FRecorder.recordCreation(locObj);
    CheckEquals(True,FChangeSummary.isCreated(locObj));
  FRecorder.recordDeletion(locObj);
    CheckEquals(False,FChangeSummary.isDeleted(locObj));
end;

procedure TSDOChangeSummary_Test.undoChanges_delete_contained_object();
var
  locA, locB0, locB1, locC0, locC1 : ISDODataObject;
  locCopyA : ISDODataObject;
  locCS : ISDOChangeSummary;
begin
  locA := FFactoryX.createNew(s_uri,s_type_object_A);
    locA.setString(s_string_prop,RandomString(1000));
    locB0 := locA.createDataObject(s_object_prop);
      locB0.setString(s_string_prop,RandomString(1000));
      locC0 := locB0.createDataObject(s_object_prop);
        locC0.setString(s_string_prop,RandomString(1000));

  locCopyA := TSDOCopyHelper.copy(locA);

  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locB1 := locA.createDataObject(s_object_prop);
      locB1.setString(s_string_prop,RandomString(1000));
      locC1 := locB1.createDataObject(s_object_prop);
        locC1.setString(s_string_prop,RandomString(1000));
  locCS.endLogging();
  locCS.undoChanges();
      CheckEquals(0,locCS.getChangedDataObjects().size());
      Check(TSDOEqualityHelper.equal(locCopyA,locA));
end;

procedure TSDOChangeSummary_Test.getOldXpath_1();
var
  locObjA, locB, locBB : ISDODataObject;
  locPropObj_propB : ISDOProperty;
  locCS : ISDOChangeSummary;
begin
  locPropObj_propB := FFactoryX.getType(s_uri,s_type_object_A).getProperty(s_object_prop);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
  locB := FFactoryX.createNew(s_uri,s_type_object_B);
    locObjA.setDataObject(locPropObj_propB,locB);

  locCS := locObjA.getChangeSummary();
  locCS.beginLogging();
    CheckEquals('',locCS.getOldXpath(locObjA));
    CheckEquals('',locCS.getOldXpath(locB));
    locBB := locObjA.createDataObject(locPropObj_propB);
      CheckEquals('',locCS.getOldXpath(locObjA));
      CheckEquals(locPropObj_propB.getName(),locCS.getOldXpath(locB));
      CheckEquals('',locCS.getOldXpath(locBB));
      locObjA.createDataObject(locPropObj_propB);
        CheckEquals(locPropObj_propB.getName(),locCS.getOldXpath(locB));
end;

procedure TSDOChangeSummary_Test.getOldXpath_nested_deleted_object();
var
  locFac : ISDODataFactory;
  locA, locB, locBB, locC, locD : ISDODataObject;
  locCS : ISDOChangeSummary;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
  locFac.AddType(s_uri,'b',[]);
  locFac.AddType(s_uri,'c',[]);
  locFac.AddType(s_uri,'d',[]);
    locFac.addProperty(s_uri,'a','p_ab',s_uri,'b',[pfIsContainment]);
    locFac.addProperty(s_uri,'a','p_ac',s_uri,'c',[]);
    locFac.addProperty(s_uri,'a','p_ad',s_uri,'d',[]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);
      locFac.addProperty(s_uri,'b','p_bc',s_uri,'c',[pfIsContainment]);
        locFac.addProperty(s_uri,'c','p_cd',s_uri,'d',[pfIsContainment]);

  locA := locFac.createNew(s_uri,'a');
    locB := locA.createDataObject('p_ab');
      locC := locB.createDataObject('p_bc');
      locD := locC.createDataObject('p_cd');

  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locBB := locA.createDataObject('p_ab');
    CheckEquals('p_ab',locCS.getOldXpath(locB));
    CheckEquals('p_ab/p_bc',locCS.getOldXpath(locC));
    CheckEquals('p_ab/p_bc/p_cd',locCS.getOldXpath(locD));
end;

procedure TSDOChangeSummary_Test.getOldValues_created_object_settinglist_is_empty();
var
  locA, locB : ISDODataObject;
  locCS : ISDOChangeSummary;
begin
  locA := FFactoryX.createNew(s_uri,s_type_object_A);
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locB := locA.createDataObject(s_object_prop);
    locB.setInteger(s_integer_prop,1210);
    locB.setString(s_string_prop, 'kis');
    CheckEquals(0, locCS.getOldValues(locB).size());
end;

procedure TSDOChangeSummary_Test.getChangedDataObjects_contained_delete;
var
  locFac : ISDODataFactory;
  locA, locB, locC, locD : ISDODataObject;
  locCS : ISDOChangeSummary;
  i : PtrInt;
  ls : ISDOChangedDataObjectList;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
  locFac.AddType(s_uri,'b',[]);
  locFac.AddType(s_uri,'c',[]);
  locFac.AddType(s_uri,'d',[]);
    locFac.addProperty(s_uri,'a','p_ab',s_uri,'b',[pfIsContainment]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);
      locFac.addProperty(s_uri,'b','p_bc',s_uri,'c',[pfIsContainment]);
        locFac.addProperty(s_uri,'c','p_cd',s_uri,'d',[pfIsContainment]);
          locFac.addProperty(s_uri,'d','p_int',sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);

  locA := locFac.createNew(s_uri,'a');
  locCS := locA.getChangeSummary();
  locCS.endLogging();
    locB := locA.createDataObject('p_ab');
      locC := locB.createDataObject('p_bc');
        locD := locC.createDataObject('p_cd');
  locCS.beginLogging();
    locD.setInteger('p_int',1210);
    locA.setDataObject('p_ab',nil);

  ls := locCS.getChangedDataObjects();
  i := IndexOf(locD,ls);
  Check( ( i < 0 ) or ( Ord(ls.getType(i)) = Ord(ctDelete) ) );
  Check(locCS.isDeleted(locD));
end;

procedure TSDOChangeSummary_Test.getChangedDataObjects_multi_value_prop;
var
  locFac : ISDODataFactory;
  locA, locB : ISDODataObject;
  locLs : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  i : PtrInt;
  ls : ISDOChangedDataObjectList;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
  locFac.AddType(s_uri,'b',[]);
    locFac.addProperty(s_uri,'a','p_ab',s_uri,'b',[pfIsContainment,pfIsMany]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);
      locFac.addProperty(s_uri,'b','p_int',sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);

  locA := locFac.createNew(s_uri,'a');
  locCS := locA.getChangeSummary();
  locCS.endLogging();
    locLs := locA.getList('p_ab');
    for i := 1 to 5 do begin
      locB := locFac.createNew(s_uri,'b');
      locB.setInteger('p_int',i);
      locLs.append(locB);
    end;
  locCS.beginLogging();
    locLs.delete(2);
    locLs.delete(0);

  ls := locCS.getChangedDataObjects();
  CheckEquals(3, ls.size());
  CheckEquals(Ord(ctChange), Ord(ls.getType(0)), 'a');
  CheckEquals(Ord(ctDelete), Ord(ls.getType(1)), 'a.p_ab[2]');
  CheckEquals(Ord(ctDelete), Ord(ls.getType(2)), 'a.p_ab[3]');
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_bool();
const
  LOCAL_PROP_NAME = s_bool_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOBoolean;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setBoolean(locProp,False);
    locSetting := nil;
  try
    buffer := locObjA.getBoolean(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setBoolean(locProp,True);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);

    FreeAndNil(locSetting);
    buffer := locObjA.getBoolean(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setBoolean(locProp,True);
          locObjA.setBoolean(locProp,False);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_byte();
const
  LOCAL_PROP_NAME = s_byte_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOByte;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setByte(locProp,RandomRange(Low(TSDOByte),High(TSDOByte)));
    locSetting := nil;
  try
    buffer := locObjA.getByte(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setByte(locProp,RandomRange(Low(TSDOByte),High(TSDOByte)));
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);

    FreeAndNil(locSetting);
    buffer := locObjA.getByte(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setByte(locProp,RandomRange(Low(TSDOByte),High(TSDOByte)));
          locObjA.setByte(locProp,RandomRange(Low(TSDOByte),High(TSDOByte)));
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_unset_bool();
const
  LOCAL_PROP_NAME = s_bool_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOBoolean;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setBoolean(locProp,True);
    locSetting := nil;
  try
    buffer := locObjA.getBoolean(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.unset(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_unset_byte();
const
  LOCAL_PROP_NAME = s_byte_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOByte;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setByte(locProp,RandomRange(Low(TSDOByte),High(TSDOByte)));
    locSetting := nil;
  try
    buffer := locObjA.getByte(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.unset(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_unset_integer();
const
  LOCAL_PROP_NAME = s_integer_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOInteger;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setInteger(locProp,456789);
    locSetting := nil;
  try
    buffer := locObjA.getInteger(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.unset(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_unset_string();
const
  LOCAL_PROP_NAME = s_string_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOString;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setString(locProp,'X-PASCAL');
    locSetting := nil;
  try
    buffer := locObjA.getString(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.unset(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_setnull_bool();
const
  LOCAL_PROP_NAME = s_bool_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOBoolean;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setBoolean(locProp,True);
    locSetting := nil;
  try
    buffer := locObjA.getBoolean(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setNull(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_setnull_byte();
const
  LOCAL_PROP_NAME = s_byte_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOByte;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setByte(locProp,RandomRange(Low(TSDOByte),High(TSDOByte)));
    locSetting := nil;
  try
    buffer := locObjA.getByte(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setNull(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_setnull_integer();
const
  LOCAL_PROP_NAME = s_integer_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOInteger;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setInteger(locProp,78910);
    locSetting := nil;
  try
    buffer := locObjA.getInteger(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setNull(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_setnull_string();
const
  LOCAL_PROP_NAME = s_string_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOString;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setString(locProp,'FGHJKLM');
    locSetting := nil;
  try
    buffer := locObjA.getString(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setNull(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.getOldValues_bool();
const
  VAL_1 : TSDOBoolean = False;
  PROP_NAME = s_bool_prop;
var
  locObjA, locObjB : ISDODataObject;
  ls : ISDOSettingList;
  locProp : ISDOProperty;
begin
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(PROP_NAME);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  ls := FChangeSummary.getOldValues(locObjA);
  CheckEquals(0, ls.size());
  ls := FChangeSummary.getOldValues(locObjB);
  CheckEquals(0, ls.size());

  locObjA.setBoolean(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

  locObjB.setBoolean(locProp,VAL_1);
  FRecorder.recordChange(locObjB,locProp);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

    ls := FChangeSummary.getOldValues(locObjB);
    CheckEquals(1,ls.size());
end;

procedure TSDOChangeSummary_Test.getOldValues_byte();
const
  VAL_1 : TSDOByte = 123;
  PROP_NAME = s_byte_prop;
var
  locObjA, locObjB : ISDODataObject;
  ls : ISDOSettingList;
  locProp : ISDOProperty;
begin
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(PROP_NAME);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  ls := FChangeSummary.getOldValues(locObjA);
  CheckEquals(0, ls.size());
  ls := FChangeSummary.getOldValues(locObjB);
  CheckEquals(0, ls.size());

  locObjA.setByte(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

  locObjB.setByte(locProp,VAL_1);
  FRecorder.recordChange(locObjB,locProp);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

    ls := FChangeSummary.getOldValues(locObjB);
    CheckEquals(1,ls.size());
end;

procedure TSDOChangeSummary_Test.getOldValue_bool();
const
  VAL_1 : TSDOBoolean = True;
  VAL_2 : TSDOBoolean = False;
  PROP_NAME = s_bool_prop;
var
  locObjA, locObjB : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
begin
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(PROP_NAME);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  CheckEquals(PtrUInt(nil), PtrUInt(FChangeSummary.getOldValue(locObjA,locProp)));

  locObjA.setBoolean(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    locSetting := FChangeSummary.getOldValue(locObjA,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));

  locObjB.setBoolean(locProp,VAL_2);
  FRecorder.recordChange(locObjB,locProp);
    locSetting := FChangeSummary.getOldValue(locObjB,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
end;

procedure TSDOChangeSummary_Test.getOldValue_byte();
const
  VAL_1 : TSDOByte = 123;
  VAL_2 : TSDOByte = 78;
  PROP_NAME = s_byte_prop;
var
  locObjA, locObjB : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
begin
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(PROP_NAME);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  CheckEquals(PtrUInt(nil), PtrUInt(FChangeSummary.getOldValue(locObjA,locProp)));

  locObjA.setByte(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    locSetting := FChangeSummary.getOldValue(locObjA,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));

  locObjB.setByte(locProp,VAL_2);
  FRecorder.recordChange(locObjB,locProp);
    locSetting := FChangeSummary.getOldValue(locObjB,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
end;

procedure TSDOChangeSummary_Test.getOldValues_date();
const
  VAL_1 : TSDODateTime = ( Date : 39000; HourOffset : 5; MinuteOffset : 6; );
  VAL_2 : TSDODateTime = ( Date : 34567; HourOffset : 8; MinuteOffset : 9; );
var
  locObjA, locObjB : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
begin
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(s_date_prop);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  CheckEquals(PtrUInt(nil), PtrUInt(FChangeSummary.getOldValue(locObjA,locProp)));

  locObjA.setDate(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    locSetting := FChangeSummary.getOldValue(locObjA,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));

  locObjB.setDate(locProp,VAL_2);
  FRecorder.recordChange(locObjB,locProp);
    locSetting := FChangeSummary.getOldValue(locObjB,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
end;

procedure TSDOChangeSummary_Test.getOldValues_integer();
const
  VAL_1 : TSDOInteger = 123456;
  PROP_NAME = s_integer_prop;
var
  locObjA, locObjB : ISDODataObject;
  ls : ISDOSettingList;
  locProp : ISDOProperty;
begin
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(PROP_NAME);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  ls := FChangeSummary.getOldValues(locObjA);
  CheckEquals(0, ls.size());
  ls := FChangeSummary.getOldValues(locObjB);
  CheckEquals(0, ls.size());

  locObjA.setInteger(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

  locObjB.setInteger(locProp,VAL_1);
  FRecorder.recordChange(locObjB,locProp);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

    ls := FChangeSummary.getOldValues(locObjB);
    CheckEquals(1,ls.size());
end;

procedure TSDOChangeSummary_Test.getOldValues_string();
const
  VAL_1 : TSDOString = 'sdo.standart.AZERTY';
  PROP_NAME = s_string_prop;
var
  locObjA, locObjB : ISDODataObject;
  ls : ISDOSettingList;
  locProp : ISDOProperty;
begin
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(PROP_NAME);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  ls := FChangeSummary.getOldValues(locObjA);
  CheckEquals(0, ls.size());
  ls := FChangeSummary.getOldValues(locObjB);
  CheckEquals(0, ls.size());

  locObjA.setString(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

  locObjB.setString(locProp,VAL_1);
  FRecorder.recordChange(locObjB,locProp);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

    ls := FChangeSummary.getOldValues(locObjB);
    CheckEquals(1,ls.size());
end;

{$IFDEF HAS_SDO_BYTES}
procedure TSDOChangeSummary_Test.getOldValues_bytes();
const
  PROP_NAME = s_bytes_prop;
var
  VAL_1 : TSDOBytes;

  procedure SetConstants();
  var
    v : TSDOBytes;
    k : Integer;
  begin
    SetLength(v,10);
    for k := 0 to High(v) do
      v[k] := k mod High(TSDOByte);
    VAL_1 := v;
  end;  
  
var
  locObjA, locObjB : ISDODataObject;
  ls : ISDOSettingList;
  locProp : ISDOProperty;
begin
  SetConstants();
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(PROP_NAME);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  ls := FChangeSummary.getOldValues(locObjA);
  CheckEquals(0, ls.size());
  ls := FChangeSummary.getOldValues(locObjB);
  CheckEquals(0, ls.size());

  locObjA.setBytes(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

  locObjB.setBytes(locProp,VAL_1);
  FRecorder.recordChange(locObjB,locProp);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

    ls := FChangeSummary.getOldValues(locObjB);
    CheckEquals(1,ls.size());
end;

procedure TSDOChangeSummary_Test.getOldValue_bytes();
const
  PROP_NAME = s_bytes_prop;
var
  VAL_1, VAL_2 : TSDOBytes;

  procedure SetConstants();
  var
    v : TSDOBytes;
    k : Integer;
  begin
    SetLength(v,10);
    for k := 0 to High(v) do
      v[k] := k mod High(TSDOByte);
    VAL_1 := v;
    v := nil;    

    SetLength(v,20);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(TSDOByte);
    VAL_2 := v;
  end;  
  
var
  locObjA, locObjB : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
begin
  SetConstants();
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(PROP_NAME);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  CheckEquals(PtrUInt(nil), PtrUInt(FChangeSummary.getOldValue(locObjA,locProp)));

  locObjA.setBytes(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    locSetting := FChangeSummary.getOldValue(locObjA,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));

  locObjB.setBytes(locProp,VAL_2);
  FRecorder.recordChange(locObjB,locProp);
    locSetting := FChangeSummary.getOldValue(locObjB,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_bytes();
const
  LOCAL_PROP_NAME = s_bytes_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOBytes;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setBytes(locProp,RandomBytes(100));
    locSetting := nil;
  try
    buffer := locObjA.getBytes(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setBytes(locProp,RandomBytes(100));
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);

    FreeAndNil(locSetting);
    buffer := locObjA.getBytes(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setBytes(locProp,RandomBytes(100));
          locObjA.setBytes(locProp,RandomBytes(100));
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_unset_bytes();
const
  LOCAL_PROP_NAME = s_bytes_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOBytes;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setBytes(locProp,RandomBytes(100));
    locSetting := nil;
  try
    buffer := locObjA.getBytes(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.unset(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_setnull_bytes();
const
  LOCAL_PROP_NAME = s_bytes_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOBytes;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setBytes(locProp,RandomBytes(100));
    locSetting := nil;
  try
    buffer := locObjA.getBytes(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setNull(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
procedure TSDOChangeSummary_Test.getOldValues_char();
const
  VAL_1 : TSDOChar = 'c';
  PROP_NAME = s_char_prop;
var
  locObjA, locObjB : ISDODataObject;
  ls : ISDOSettingList;
  locProp : ISDOProperty;
begin
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(PROP_NAME);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  ls := FChangeSummary.getOldValues(locObjA);
  CheckEquals(0, ls.size());
  ls := FChangeSummary.getOldValues(locObjB);
  CheckEquals(0, ls.size());

  locObjA.setCharacter(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

  locObjB.setCharacter(locProp,VAL_1);
  FRecorder.recordChange(locObjB,locProp);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

    ls := FChangeSummary.getOldValues(locObjB);
    CheckEquals(1,ls.size());
end;

procedure TSDOChangeSummary_Test.getOldValue_char();
const
  VAL_1 : TSDOChar = 'd';
  VAL_2 : TSDOChar = 'i';
  PROP_NAME = s_char_prop;
var
  locObjA, locObjB : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
begin
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(PROP_NAME);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  CheckEquals(PtrUInt(nil), PtrUInt(FChangeSummary.getOldValue(locObjA,locProp)));

  locObjA.setCharacter(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    locSetting := FChangeSummary.getOldValue(locObjA,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));

  locObjB.setCharacter(locProp,VAL_2);
  FRecorder.recordChange(locObjB,locProp);
    locSetting := FChangeSummary.getOldValue(locObjB,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_char();
const
  LOCAL_PROP_NAME = s_char_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOChar;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setCharacter(locProp,TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
    locSetting := nil;
  try
    buffer := locObjA.getCharacter(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setCharacter(locProp,TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);

    FreeAndNil(locSetting);
    buffer := locObjA.getCharacter(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setCharacter(locProp,TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
          locObjA.setCharacter(locProp,TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_unset_char();
const
  LOCAL_PROP_NAME = s_char_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOChar;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setCharacter(locProp,TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
    locSetting := nil;
  try
    buffer := locObjA.getCharacter(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.unset(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_setnull_char();
const
  LOCAL_PROP_NAME = s_char_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOChar;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setCharacter(locProp,TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
    locSetting := nil;
  try
    buffer := locObjA.getCharacter(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setNull(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
procedure TSDOChangeSummary_Test.getOldValues_currency();
const
  VAL_1 : TSDOCurrency = 1238527419247;
  PROP_NAME = s_currency_prop;
var
  locObjA, locObjB : ISDODataObject;
  ls : ISDOSettingList;
  locProp : ISDOProperty;
begin
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(PROP_NAME);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  ls := FChangeSummary.getOldValues(locObjA);
  CheckEquals(0, ls.size());
  ls := FChangeSummary.getOldValues(locObjB);
  CheckEquals(0, ls.size());

  locObjA.setCurrency(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

  locObjB.setCurrency(locProp,VAL_1);
  FRecorder.recordChange(locObjB,locProp);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

    ls := FChangeSummary.getOldValues(locObjB);
    CheckEquals(1,ls.size());
end;

procedure TSDOChangeSummary_Test.getOldValue_currency();
const
  VAL_1 : TSDOCurrency = 98765432178225;
  VAL_2 : TSDOCurrency = -4587412254554;
  PROP_NAME = s_currency_prop;
var
  locObjA, locObjB : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
begin
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(PROP_NAME);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  CheckEquals(PtrUInt(nil), PtrUInt(FChangeSummary.getOldValue(locObjA,locProp)));

  locObjA.setCurrency(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    locSetting := FChangeSummary.getOldValue(locObjA,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));

  locObjB.setCurrency(locProp,VAL_2);
  FRecorder.recordChange(locObjB,locProp);
    locSetting := FChangeSummary.getOldValue(locObjB,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_currency();
const
  LOCAL_PROP_NAME = s_currency_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOCurrency;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setCurrency(locProp,test_suite_utils.RandomRange(Low(Word),High(Word)));
    locSetting := nil;
  try
    buffer := locObjA.getCurrency(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setCurrency(locProp,test_suite_utils.RandomRange(Low(Word),High(Word)));
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);

    FreeAndNil(locSetting);
    buffer := locObjA.getCurrency(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setCurrency(locProp,test_suite_utils.RandomRange(Low(Word),High(Word)));
          locObjA.setCurrency(locProp,test_suite_utils.RandomRange(Low(Word),High(Word)));
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_unset_currency();
const
  LOCAL_PROP_NAME = s_currency_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOCurrency;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setCurrency(locProp,test_suite_utils.RandomRange(Low(Word),High(Word)));
    locSetting := nil;
  try
    buffer := locObjA.getCurrency(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.unset(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_setnull_currency();
const
  LOCAL_PROP_NAME = s_currency_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOCurrency;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setCurrency(locProp,test_suite_utils.RandomRange(Low(Word),High(Word)));
    locSetting := nil;
  try
    buffer := locObjA.getCurrency(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setNull(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
procedure TSDOChangeSummary_Test.getOldValues_double();
const
  VAL_1 : TSDODouble = 1238527419247;
  PROP_NAME = s_double_prop;
var
  locObjA, locObjB : ISDODataObject;
  ls : ISDOSettingList;
  locProp : ISDOProperty;
begin
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(PROP_NAME);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  ls := FChangeSummary.getOldValues(locObjA);
  CheckEquals(0, ls.size());
  ls := FChangeSummary.getOldValues(locObjB);
  CheckEquals(0, ls.size());

  locObjA.setDouble(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

  locObjB.setDouble(locProp,VAL_1);
  FRecorder.recordChange(locObjB,locProp);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

    ls := FChangeSummary.getOldValues(locObjB);
    CheckEquals(1,ls.size());
end;

procedure TSDOChangeSummary_Test.getOldValue_double();
const
  VAL_1 : TSDODouble = 9876543211478225;
  VAL_2 : TSDODouble = -4587412282254554;
  PROP_NAME = s_double_prop;
var
  locObjA, locObjB : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
begin
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(PROP_NAME);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  CheckEquals(PtrUInt(nil), PtrUInt(FChangeSummary.getOldValue(locObjA,locProp)));

  locObjA.setDouble(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    locSetting := FChangeSummary.getOldValue(locObjA,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));

  locObjB.setDouble(locProp,VAL_2);
  FRecorder.recordChange(locObjB,locProp);
    locSetting := FChangeSummary.getOldValue(locObjB,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_double();
const
  LOCAL_PROP_NAME = s_double_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDODouble;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setDouble(locProp,test_suite_utils.RandomRange(Low(Word),High(Word)));
    locSetting := nil;
  try
    buffer := locObjA.getDouble(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setDouble(locProp,test_suite_utils.RandomRange(Low(Word),High(Word)));
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);

    FreeAndNil(locSetting);
    buffer := locObjA.getDouble(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setDouble(locProp,test_suite_utils.RandomRange(Low(Word),High(Word)));
          locObjA.setDouble(locProp,test_suite_utils.RandomRange(Low(Word),High(Word)));
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_unset_double();
const
  LOCAL_PROP_NAME = s_double_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDODouble;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setDouble(locProp,test_suite_utils.RandomRange(Low(Word),High(Word)));
    locSetting := nil;
  try
    buffer := locObjA.getDouble(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.unset(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_setnull_double();
const
  LOCAL_PROP_NAME = s_double_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDODouble;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setDouble(locProp,test_suite_utils.RandomRange(Low(Word),High(Word)));
    locSetting := nil;
  try
    buffer := locObjA.getDouble(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setNull(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
procedure TSDOChangeSummary_Test.getOldValues_float();
const
  VAL_1 : TSDOFloat = 1238527419247;
  PROP_NAME = s_float_prop;
var
  locObjA, locObjB : ISDODataObject;
  ls : ISDOSettingList;
  locProp : ISDOProperty;
begin
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(PROP_NAME);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  ls := FChangeSummary.getOldValues(locObjA);
  CheckEquals(0, ls.size());
  ls := FChangeSummary.getOldValues(locObjB);
  CheckEquals(0, ls.size());

  locObjA.setFloat(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

  locObjB.setFloat(locProp,VAL_1);
  FRecorder.recordChange(locObjB,locProp);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

    ls := FChangeSummary.getOldValues(locObjB);
    CheckEquals(1,ls.size());
end;

procedure TSDOChangeSummary_Test.getOldValue_float();
const
  VAL_1 : TSDOFloat = 9876543211478225;
  VAL_2 : TSDOFloat = -4587412282254554;
  PROP_NAME = s_float_prop;
var
  locObjA, locObjB : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
begin
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(PROP_NAME);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  CheckEquals(PtrUInt(nil), PtrUInt(FChangeSummary.getOldValue(locObjA,locProp)));

  locObjA.setFloat(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    locSetting := FChangeSummary.getOldValue(locObjA,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));

  locObjB.setFloat(locProp,VAL_2);
  FRecorder.recordChange(locObjB,locProp);
    locSetting := FChangeSummary.getOldValue(locObjB,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_float();
const
  LOCAL_PROP_NAME = s_float_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOFloat;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setFloat(locProp,test_suite_utils.RandomRange(Low(Word),High(Word)));
    locSetting := nil;
  try
    buffer := locObjA.getFloat(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setFloat(locProp,test_suite_utils.RandomRange(Low(Word),High(Word)));
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);

    FreeAndNil(locSetting);
    buffer := locObjA.getFloat(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setFloat(locProp,test_suite_utils.RandomRange(Low(Word),High(Word)));
          locObjA.setFloat(locProp,test_suite_utils.RandomRange(Low(Word),High(Word)));
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_unset_float();
const
  LOCAL_PROP_NAME = s_float_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOFloat;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setFloat(locProp,test_suite_utils.RandomRange(Low(Word),High(Word)));
    locSetting := nil;
  try
    buffer := locObjA.getFloat(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.unset(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_setnull_float();
const
  LOCAL_PROP_NAME = s_float_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOFloat;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setFloat(locProp,test_suite_utils.RandomRange(Low(Word),High(Word)));
    locSetting := nil;
  try
    buffer := locObjA.getFloat(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setNull(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;
{$ENDIF HAS_SDO_FLOAT}

{$IFDEF HAS_SDO_LONG}
procedure TSDOChangeSummary_Test.getOldValues_long();
const
  VAL_1 : TSDOLong = 123852741963654247;
  PROP_NAME = s_long_prop;
var
  locObjA, locObjB : ISDODataObject;
  ls : ISDOSettingList;
  locProp : ISDOProperty;
begin
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(PROP_NAME);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  ls := FChangeSummary.getOldValues(locObjA);
  CheckEquals(0, ls.size());
  ls := FChangeSummary.getOldValues(locObjB);
  CheckEquals(0, ls.size());

  locObjA.setLong(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

  locObjB.setLong(locProp,VAL_1);
  FRecorder.recordChange(locObjB,locProp);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

    ls := FChangeSummary.getOldValues(locObjB);
    CheckEquals(1,ls.size());
end;

procedure TSDOChangeSummary_Test.getOldValue_long();
const
  VAL_1 : TSDOLong = 9876543211478225;
  VAL_2 : TSDOLong = -4587412282254554;
  PROP_NAME = s_long_prop;
var
  locObjA, locObjB : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
begin
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(PROP_NAME);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  CheckEquals(PtrUInt(nil), PtrUInt(FChangeSummary.getOldValue(locObjA,locProp)));

  locObjA.setLong(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    locSetting := FChangeSummary.getOldValue(locObjA,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));

  locObjB.setLong(locProp,VAL_2);
  FRecorder.recordChange(locObjB,locProp);
    locSetting := FChangeSummary.getOldValue(locObjB,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_long();
const
  LOCAL_PROP_NAME = s_long_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOLong;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setLong(locProp,test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
    locSetting := nil;
  try
    buffer := locObjA.getLong(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setLong(locProp,test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);

    FreeAndNil(locSetting);
    buffer := locObjA.getLong(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setLong(locProp,test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
          locObjA.setLong(locProp,test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_unset_long();
const
  LOCAL_PROP_NAME = s_long_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOLong;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setLong(locProp,test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
    locSetting := nil;
  try
    buffer := locObjA.getLong(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.unset(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_setnull_long();
const
  LOCAL_PROP_NAME = s_long_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOLong;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setLong(locProp,test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
    locSetting := nil;
  try
    buffer := locObjA.getLong(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setNull(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure TSDOChangeSummary_Test.getOldValues_short();
const
  VAL_1 : TSDOShort = 12385;
  PROP_NAME = s_short_prop;
var
  locObjA, locObjB : ISDODataObject;
  ls : ISDOSettingList;
  locProp : ISDOProperty;
begin
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(PROP_NAME);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  ls := FChangeSummary.getOldValues(locObjA);
  CheckEquals(0, ls.size());
  ls := FChangeSummary.getOldValues(locObjB);
  CheckEquals(0, ls.size());

  locObjA.setShort(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

  locObjB.setShort(locProp,VAL_1);
  FRecorder.recordChange(locObjB,locProp);
    ls := FChangeSummary.getOldValues(locObjA);
    CheckEquals(1,ls.size());

    ls := FChangeSummary.getOldValues(locObjB);
    CheckEquals(1,ls.size());
end;

procedure TSDOChangeSummary_Test.getOldValue_short();
const
  VAL_1 : TSDOShort = 9876;
  VAL_2 : TSDOShort = -4587;
  PROP_NAME = s_short_prop;
var
  locObjA, locObjB : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
begin
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(PROP_NAME);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  CheckEquals(PtrUInt(nil), PtrUInt(FChangeSummary.getOldValue(locObjA,locProp)));

  locObjA.setShort(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    locSetting := FChangeSummary.getOldValue(locObjA,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));

  locObjB.setShort(locProp,VAL_2);
  FRecorder.recordChange(locObjB,locProp);
    locSetting := FChangeSummary.getOldValue(locObjB,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_short();
const
  LOCAL_PROP_NAME = s_short_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOShort;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setShort(locProp,RandomRange(Low(TSDOShort),High(TSDOShort)));
    locSetting := nil;
  try
    buffer := locObjA.getShort(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setShort(locProp,RandomRange(Low(TSDOShort),High(TSDOShort)));
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);

    FreeAndNil(locSetting);
    buffer := locObjA.getShort(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setShort(locProp,RandomRange(Low(TSDOShort),High(TSDOShort)));
          locObjA.setShort(locProp,RandomRange(Low(TSDOShort),High(TSDOShort)));
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_unset_short();
const
  LOCAL_PROP_NAME = s_short_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOShort;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setShort(locProp,RandomRange(Low(TSDOShort),High(TSDOShort)));
    locSetting := nil;
  try
    buffer := locObjA.getShort(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.unset(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_setnull_short();
const
  LOCAL_PROP_NAME = s_short_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOShort;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setShort(locProp,RandomRange(Low(TSDOShort),High(TSDOShort)));
    locSetting := nil;
  try
    buffer := locObjA.getShort(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setNull(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;
{$ENDIF HAS_SDO_SHORT}

procedure TSDOChangeSummary_Test.getOldValue_date();
const
  VAL_1 : TSDODateTime = ( Date : 39000; HourOffset : 5; MinuteOffset : 6; );
  VAL_2 : TSDODateTime = ( Date : 34567; HourOffset : 8; MinuteOffset : 9; );
var
  locObjA, locObjB : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
begin
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(s_date_prop);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  CheckEquals(PtrUInt(nil), PtrUInt(FChangeSummary.getOldValue(locObjA,locProp)));

  locObjA.setDate(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    locSetting := FChangeSummary.getOldValue(locObjA,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));

  locObjB.setDate(locProp,VAL_2);
  FRecorder.recordChange(locObjB,locProp);
    locSetting := FChangeSummary.getOldValue(locObjB,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
end;

procedure TSDOChangeSummary_Test.getOldValue_Integer();
const
  VAL_1 : TSDOInteger = 123456;
  VAL_2 : TSDOInteger = -987;
  PROP_NAME = s_integer_prop;
var
  locObjA, locObjB : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
begin
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(PROP_NAME);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  CheckEquals(PtrUInt(nil), PtrUInt(FChangeSummary.getOldValue(locObjA,locProp)));

  locObjA.setInteger(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    locSetting := FChangeSummary.getOldValue(locObjA,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));

  locObjB.setInteger(locProp,VAL_2);
  FRecorder.recordChange(locObjB,locProp);
    locSetting := FChangeSummary.getOldValue(locObjB,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
end;

procedure TSDOChangeSummary_Test.getOldValue_string();
const
  VAL_1 : TSDOString = 'Inoussa.O';
  VAL_2 : TSDOString = 'SDO.AZERTY';
  PROP_NAME = s_string_prop;
var
  locObjA, locObjB : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
begin
  locProp := FFactory.getType(s_uri,s_type_object_A).getProperty(PROP_NAME);
  locObjA := FFactory.createNew(s_uri,s_type_object_A);
  locObjB := FFactory.createNew(s_uri,s_type_object_A);
  FChangeSummary.beginLogging();

  CheckEquals(PtrUInt(nil), PtrUInt(FChangeSummary.getOldValue(locObjA,locProp)));

  locObjA.setString(locProp,VAL_1);
  FRecorder.recordChange(locObjA,locProp);
    locSetting := FChangeSummary.getOldValue(locObjA,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));

  locObjB.setString(locProp,VAL_2);
  FRecorder.recordChange(locObjB,locProp);
    locSetting := FChangeSummary.getOldValue(locObjB,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting));
    CheckEquals(PtrUInt(locProp), PtrUInt(locSetting.getProperty()));
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_date();
const
  LOCAL_PROP_NAME = s_date_prop;
  VAL_1 : TSDODateTime = ( Date : 39000; HourOffset : 5; MinuteOffset : 6; );
  VAL_2 : TSDODateTime = ( Date : 34567; HourOffset : 8; MinuteOffset : 9; );
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDODateTime;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setDate(locProp,VAL_1);
    locSetting := nil;
  try
    buffer := locObjA.getDate(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setDate(locProp,VAL_2);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);

    FreeAndNil(locSetting);
    buffer := locObjA.getDate(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setDate(locProp,VAL_1);
          locObjA.setDate(locProp,VAL_2);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_integer();
const
  LOCAL_PROP_NAME = s_integer_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOInteger;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setInteger(locProp,RandomRange(-978456,123456));
    locSetting := nil;
  try
    buffer := locObjA.getInteger(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setInteger(locProp,RandomRange(-978456,123456));
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);

    FreeAndNil(locSetting);
    buffer := locObjA.getInteger(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setInteger(locProp,RandomRange(-978456,123456));
          locObjA.setInteger(locProp,RandomRange(-978456,123456));
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_string();
const
  LOCAL_PROP_NAME = s_string_prop;
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDOString;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setString(locProp,RandomString(RandomRange(0,1000)));
    locSetting := nil;
  try
    buffer := locObjA.getString(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setString(locProp,RandomString(RandomRange(0,1000)));
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);

    FreeAndNil(locSetting);
    buffer := locObjA.getString(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setString(locProp,RandomString(RandomRange(0,1000)));
          locObjA.setString(locProp,RandomString(RandomRange(0,1000)));
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.CheckEquals(expected, actual: TSDODate; msg: string; const AStrict: Boolean);
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

procedure TSDOChangeSummary_Test.undoChanges_simple_unset_date();
const
  LOCAL_PROP_NAME = s_date_prop;
  VAL_1 : TSDODateTime = ( Date : 39000; HourOffset : 5; MinuteOffset : 6; );
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDODateTime;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setDate(locProp,VAL_1);
    locSetting := nil;
  try
    buffer := locObjA.getDate(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.unset(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

procedure TSDOChangeSummary_Test.undoChanges_simple_setnull_date();
const
  LOCAL_PROP_NAME = s_date_prop;
  VAL_1 : TSDODateTime = ( Date : 39000; HourOffset : 5; MinuteOffset : 6; );
var
  locObjA : ISDODataObject;
  locProp : ISDOProperty;
  locSetting : TValueSetting;
  buffer : TSDODateTime;
  locCS : ISDOChangeSummary;
begin
  locProp := FFactoryX.getType(s_uri,s_type_object_A).getProperty(LOCAL_PROP_NAME);
  locObjA := FFactoryX.createNew(s_uri,s_type_object_A);
    locObjA.setDate(locProp,VAL_1);
    locSetting := nil;
  try
    buffer := locObjA.getDate(locProp);
      locSetting := TValueSetting.Create(locObjA.isSet(locProp),locObjA.isNull(locProp),buffer,locProp,0);

    locCS := locObjA.getChangeSummary();
      locCS.beginLogging();
        locObjA.setNull(locProp);
      locCS.endLogging();
      locCS.undoChanges();
        CheckEquals(0,locCS.getChangedDataObjects().size());
        check_value(locObjA,locProp,locSetting);
  finally
    FreeAndNil(locSetting);
  end;
end;

{ TSDODataObjectCS_Test }
type
  TValueState = record
    IsSet : Boolean;
    IsNull : Boolean;
    Value : TValueBuffer;
  end;

procedure TSDODataObjectCS_Test.CheckEquals(expected, actual: TSDODate;
  msg: string; const AStrict: Boolean);
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

procedure TSDODataObjectCS_Test.check_bool_logging(
  const AObj : ISDODataObject;
  const APropName : string
);
var
  locProp : ISDOProperty;
  locCS : ISDOChangeSummary;
  locSetting : TValueSetting;
  locNewValue : TSDOBoolean;
  locOldState : TValueState;
begin
  locProp := AObj.getProperty(APropName);
  locCS := AObj.getChangeSummary();
  locNewValue := ( ( RandomRange(Low(TSDOByte),High(TSDOByte)) mod 2 ) = 0);

  locOldState.Value.BooleanValue := AObj.getBoolean(locProp);
  locOldState.IsSet := AObj.isSet(locProp);
  locOldState.IsNull := AObj.isNull(locProp);
  AObj.setBoolean(locProp,locNewValue);
    locSetting := locCS.getOldValue(AObj,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
    CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
    CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
    CheckEquals(locOldState.Value.BooleanValue,locSetting.getBooleanValue(),'getBooleanValue');

  locNewValue := ( ( RandomRange(Low(Byte),High(Byte)) mod 2 ) = 0);
  AObj.setBoolean(locProp,locNewValue);  // the recorder should keep the first recorded old values
    locSetting := locCS.getOldValue(AObj,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
    CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
    CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
    CheckEquals(locOldState.Value.BooleanValue,locSetting.getBooleanValue(),'getBooleanValue');
end;

procedure TSDODataObjectCS_Test.check_byte_logging(
  const AObj : ISDODataObject;
  const APropName : string
);
var
  locProp : ISDOProperty;
  locCS : ISDOChangeSummary;
  locSetting : TValueSetting;
  locNewValue : TSDOByte;
  locOldState : TValueState;
begin
  locProp := AObj.getProperty(APropName);
  locCS := AObj.getChangeSummary();
  locNewValue := RandomRange(Low(TSDOByte),High(TSDOByte));

  locOldState.Value.ByteValue := AObj.getByte(locProp);
  locOldState.IsSet := AObj.isSet(locProp);
  locOldState.IsNull := AObj.isNull(locProp);
  AObj.setByte(locProp,locNewValue);
    locSetting := locCS.getOldValue(AObj,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
    CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
    CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
    CheckEquals(locOldState.Value.ByteValue,locSetting.getByteValue(),'getByteValue');

  locNewValue := RandomRange(Low(TSDOByte),High(TSDOByte));
  AObj.setByte(locProp,locNewValue);  // the recorder should keep the first recorded old values
    locSetting := locCS.getOldValue(AObj,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
    CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
    CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
    CheckEquals(locOldState.Value.ByteValue,locSetting.getByteValue(),'getByteValue');
end;

procedure TSDODataObjectCS_Test.check_date_logging(
  const AObj : ISDODataObject;
  const APropName : string
);
var
  locProp : ISDOProperty;
  locCS : ISDOChangeSummary;
  locSetting : TValueSetting;
  locNewValue : TSDODateTime;
  locOldState : TValueState;
begin
  locProp := AObj.getProperty(APropName);
  locCS := AObj.getChangeSummary();
  locNewValue.Date := 39123.45;
  locNewValue.HourOffset := 2;
  locNewValue.MinuteOffset := 3;

  locOldState.Value.DateValue := AObj.getDate(locProp);
  locOldState.IsSet := AObj.isSet(locProp);
  locOldState.IsNull := AObj.isNull(locProp);
  AObj.setDate(locProp,locNewValue);
    locSetting := locCS.getOldValue(AObj,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
    CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
    CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
    CheckEquals(locOldState.Value.DateValue,locSetting.getDateValue(),'getDateValue');

  locNewValue.Date := 45000;
  locNewValue.HourOffset := -5;
  locNewValue.MinuteOffset := 0;
  AObj.setDate(locProp,locNewValue);  // the recorder should keep the first recorded old values
    locSetting := locCS.getOldValue(AObj,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
    CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
    CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
    CheckEquals(locOldState.Value.DateValue,locSetting.getDateValue(),'getDateValue');
end;

procedure TSDODataObjectCS_Test.check_int_logging(
  const AObj : ISDODataObject;
  const APropName : string
);
var
  locProp : ISDOProperty;
  locCS : ISDOChangeSummary;
  locSetting : TValueSetting;
  locNewValue : TSDOInteger;
  locOldState : TValueState;
begin
  locProp := AObj.getProperty(APropName);
  locCS := AObj.getChangeSummary();
  locNewValue := RandomRange(-121076,121076);

  locOldState.Value.IntegerValue := AObj.getInteger(locProp);
  locOldState.IsSet := AObj.isSet(locProp);
  locOldState.IsNull := AObj.isNull(locProp);
  AObj.setInteger(locProp,locNewValue);
    locSetting := locCS.getOldValue(AObj,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
    CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
    CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
    CheckEquals(locOldState.Value.IntegerValue,locSetting.getIntegerValue(),'getIntegerValue');

  locNewValue := RandomRange(-121076,121076);
  AObj.setInteger(locProp,locNewValue);  // the recorder should keep the first recorded old values
    locSetting := locCS.getOldValue(AObj,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
    CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
    CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
    CheckEquals(locOldState.Value.IntegerValue,locSetting.getIntegerValue(),'getIntegerValue');
end;

procedure TSDODataObjectCS_Test.check_string_logging(
  const AObj : ISDODataObject;
  const APropName : string
);
var
  locProp : ISDOProperty;
  locCS : ISDOChangeSummary;
  locSetting : TValueSetting;
  locNewValue : TSDOString;
  locOldState : TValueState;
begin
  locProp := AObj.getProperty(APropName);
  locCS := AObj.getChangeSummary();
  locNewValue := RandomString(RandomRange(10,1000));

  New(locOldState.Value.StringValue);
  try
    locOldState.Value.StringValue^ := AObj.getString(locProp);
    locOldState.IsSet := AObj.isSet(locProp);
    locOldState.IsNull := AObj.isNull(locProp);
    AObj.setString(locProp,locNewValue);
      locSetting := locCS.getOldValue(AObj,locProp);
      CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
      CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
      CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
      CheckEquals(locOldState.Value.StringValue^,locSetting.getStringValue(),'getStringValue');

    locNewValue := RandomString(RandomRange(10,1000));
    AObj.setString(locProp,locNewValue);  // the recorder should keep the first recorded old values
      locSetting := locCS.getOldValue(AObj,locProp);
      CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
      CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
      CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
      CheckEquals(locOldState.Value.StringValue^,locSetting.getStringValue(),'getStringValue');
  finally
    Dispose(locOldState.Value.StringValue);
  end;
end;

procedure TSDODataObjectCS_Test.create_default_props();
var
  locObj : ISDODataObject;
  locCS : ISDOChangeSummary;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locCS := locObj.getChangeSummary();

  CheckNotEquals(PtrUInt(nil),PtrUInt(locCS), 'getChangeSummary()');
  CheckEquals(False, locCS.isLogging());
end;

class function TSDODataObjectCS_Test.Create_Factory(): ISDODataFactory;
var
  locFactory : ISDODataFactory;

  procedure Add_Objects(const AUri : string);
  var
    locObj : ISDOType;
  begin
    locFactory.AddType(AUri,s_type_object_C,[]);
    locObj := locFactory.getType(AUri,s_type_object_C);
      locFactory.addProperty(locObj,s_bool_prop,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType], []);
      locFactory.addProperty(locObj,s_byte_prop,sdo_namespace,SDOTypeDefaultTypeNames[ByteType], []);
{$IFDEF HAS_SDO_BYTES}
      locFactory.addProperty(locObj,s_bytes_prop,sdo_namespace,SDOTypeDefaultTypeNames[BytesType], []);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
      locFactory.addProperty(locObj,s_char_prop,sdo_namespace,SDOTypeDefaultTypeNames[CharacterType], []);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
      locFactory.addProperty(locObj,s_currency_prop,sdo_namespace,SDOTypeDefaultTypeNames[CurrencyType], []);
{$ENDIF HAS_SDO_CURRENCY}
      locFactory.addProperty(locObj,s_date_prop,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType], []);
{$IFDEF HAS_SDO_DOUBLE}
      locFactory.addProperty(locObj,s_double_prop,sdo_namespace,SDOTypeDefaultTypeNames[DoubleType], []);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
      locFactory.addProperty(locObj,s_float_prop,sdo_namespace,SDOTypeDefaultTypeNames[FloatType], []);
{$ENDIF HAS_SDO_FLOAT}
      locFactory.addProperty(locObj,s_integer_prop,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType], []);
{$IFDEF HAS_SDO_LONG}
      locFactory.addProperty(locObj,s_long_prop,sdo_namespace,SDOTypeDefaultTypeNames[LongType], []);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      locFactory.addProperty(locObj,s_short_prop,sdo_namespace,SDOTypeDefaultTypeNames[ShortType], []);
{$ENDIF HAS_SDO_SHORT}
      locFactory.addProperty(locObj,s_string_prop,sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);

    locFactory.AddType(AUri,s_type_object_B,[]);
    locObj := locFactory.getType(AUri,s_type_object_B);
      locFactory.addProperty(locObj,s_bool_prop,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType], []);
      locFactory.addProperty(locObj,s_byte_prop,sdo_namespace,SDOTypeDefaultTypeNames[ByteType], []);
{$IFDEF HAS_SDO_BYTES}
      locFactory.addProperty(locObj,s_bytes_prop,sdo_namespace,SDOTypeDefaultTypeNames[BytesType], []);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
      locFactory.addProperty(locObj,s_char_prop,sdo_namespace,SDOTypeDefaultTypeNames[CharacterType], []);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
      locFactory.addProperty(locObj,s_currency_prop,sdo_namespace,SDOTypeDefaultTypeNames[CurrencyType], []);
{$ENDIF HAS_SDO_CURRENCY}
      locFactory.addProperty(locObj,s_date_prop,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType], []);
{$IFDEF HAS_SDO_DOUBLE}
      locFactory.addProperty(locObj,s_double_prop,sdo_namespace,SDOTypeDefaultTypeNames[DoubleType], []);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
      locFactory.addProperty(locObj,s_float_prop,sdo_namespace,SDOTypeDefaultTypeNames[FloatType], []);
{$ENDIF HAS_SDO_FLOAT}
      locFactory.addProperty(locObj,s_integer_prop,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType], []);
{$IFDEF HAS_SDO_LONG}
      locFactory.addProperty(locObj,s_long_prop,sdo_namespace,SDOTypeDefaultTypeNames[LongType], []);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      locFactory.addProperty(locObj,s_short_prop,sdo_namespace,SDOTypeDefaultTypeNames[ShortType], []);
{$ENDIF HAS_SDO_SHORT}
      locFactory.addProperty(locObj,s_string_prop,sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
      locFactory.addProperty(locObj,s_object_prop,s_uri,s_type_object_C,[pfIsContainment]);

    locFactory.AddType(AUri,s_type_object_A,[]);
    locObj := locFactory.getType(AUri,s_type_object_A);
      locFactory.addProperty(locObj,s_bool_prop,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType], []);
        locFactory.addProperty(locObj,s_bool_propList,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsMany]);
      locFactory.addProperty(locObj,s_byte_prop,sdo_namespace,SDOTypeDefaultTypeNames[ByteType], []);
        locFactory.addProperty(locObj,s_byte_propList,sdo_namespace,SDOTypeDefaultTypeNames[ByteType], [pfIsMany]);
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
        locFactory.addProperty(locObj,s_date_propList,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType], [pfIsMany]);
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
      locFactory.addProperty(locObj,s_object_prop,s_uri,s_type_object_B,[pfIsContainment]);
      locFactory.addProperty(locObj,s_object_ref_prop,s_uri,s_type_object_B,[]);
      locFactory.addProperty(locObj,s_changesummary_prop,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

  end;
begin
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
  Add_Objects(s_uri);

  Result := locFactory;
end;

procedure TSDODataObjectCS_Test.getChangeSummary();
var
  locA, locB, locC : ISDODataObject;
begin
  locA := FFactory.createNew(s_uri,s_type_object_A);
  locB := FFactory.createNew(s_uri,s_type_object_B);
  locC := FFactory.createNew(s_uri,s_type_object_C);

  CheckNotEquals(PtrUInt(nil), PtrUInt(locA.getChangeSummary()));
  CheckEquals(PtrUInt(nil), PtrUInt(locB.getChangeSummary()));
  CheckEquals(PtrUInt(nil), PtrUInt(locB.getChangeSummary()));

  locA.setDataObject(s_object_prop,locB);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locA.getChangeSummary()));
    CheckNotEquals(PtrUInt(nil), PtrUInt(locB.getChangeSummary()));
    CheckEquals(PtrUInt(nil), PtrUInt(locC.getChangeSummary()));

    locB.setDataObject(s_object_prop,locC);
      CheckNotEquals(PtrUInt(nil), PtrUInt(locA.getChangeSummary()));
      CheckNotEquals(PtrUInt(nil), PtrUInt(locB.getChangeSummary()));
      CheckNotEquals(PtrUInt(nil), PtrUInt(locC.getChangeSummary()));
end;

procedure TSDODataObjectCS_Test.logging_bool();
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locObj.getChangeSummary().beginLogging();

  check_bool_logging(locObj,s_bool_prop);
end;

procedure TSDODataObjectCS_Test.logging_byte();
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locObj.getChangeSummary().beginLogging();

  check_byte_logging(locObj,s_byte_prop);
end;

procedure TSDODataObjectCS_Test.logging_date();
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locObj.getChangeSummary().beginLogging();

  check_date_logging(locObj,s_date_prop);
end;

procedure TSDODataObjectCS_Test.logging_int();
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locObj.getChangeSummary().beginLogging();

  check_int_logging(locObj,s_integer_prop);
end;

procedure TSDODataObjectCS_Test.logging_string();
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locObj.getChangeSummary().beginLogging();

  check_string_logging(locObj,s_string_prop);
end;

{$IFDEF HAS_SDO_BYTES}
procedure TSDODataObjectCS_Test.check_bytes_logging(
  const AObj : ISDODataObject;
  const APropName : string
);
var
  locProp : ISDOProperty;
  locCS : ISDOChangeSummary;
  locSetting : TValueSetting;
  locNewValue : TSDOBytes;
  locOldState : TValueState;
begin
  locProp := AObj.getProperty(APropName);
  locCS := AObj.getChangeSummary();
  locNewValue := RandomBytes(100);

  New(locOldState.Value.BytesValue);
  try
    locOldState.Value.BytesValue^ := AObj.getBytes(locProp);
    locOldState.IsSet := AObj.isSet(locProp);
    locOldState.IsNull := AObj.isNull(locProp);
    AObj.setBytes(locProp,locNewValue);
      locSetting := locCS.getOldValue(AObj,locProp);
      CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
      CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
      CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
      CheckEquals(locOldState.Value.BytesValue^,locSetting.getBytesValue(),'getBytesValue');

    locNewValue := RandomBytes(100);
    AObj.setBytes(locProp,locNewValue);  // the recorder should keep the first recorded old values
      locSetting := locCS.getOldValue(AObj,locProp);
      CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
      CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
      CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
      CheckEquals(locOldState.Value.BytesValue^,locSetting.getBytesValue(),'getBytesValue');
  finally
    Dispose(locOldState.Value.BytesValue);
  end;
end;

procedure TSDODataObjectCS_Test.logging_bytes();
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locObj.getChangeSummary().beginLogging();

  check_bytes_logging(locObj,s_bytes_prop);
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
procedure TSDODataObjectCS_Test.check_char_logging(
  const AObj : ISDODataObject;
  const APropName : string
);
var
  locProp : ISDOProperty;
  locCS : ISDOChangeSummary;
  locSetting : TValueSetting;
  locNewValue : TSDOChar;
  locOldState : TValueState;
begin
  locProp := AObj.getProperty(APropName);
  locCS := AObj.getChangeSummary();
  locNewValue := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));

  locOldState.Value.CharValue := AObj.getCharacter(locProp);
  locOldState.IsSet := AObj.isSet(locProp);
  locOldState.IsNull := AObj.isNull(locProp);
  AObj.setCharacter(locProp,locNewValue);
    locSetting := locCS.getOldValue(AObj,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
    CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
    CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
    CheckEquals(locOldState.Value.CharValue,locSetting.getCharacterValue(),'getCharacterValue');

  locNewValue := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));
  AObj.setCharacter(locProp,locNewValue);  // the recorder should keep the first recorded old values
    locSetting := locCS.getOldValue(AObj,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
    CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
    CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
    CheckEquals(locOldState.Value.CharValue,locSetting.getCharacterValue(),'getCharacterValue');
end;

procedure TSDODataObjectCS_Test.logging_char();
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locObj.getChangeSummary().beginLogging();

  check_char_logging(locObj,s_char_prop);
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
procedure TSDODataObjectCS_Test.check_currency_logging(
  const AObj : ISDODataObject;
  const APropName : string
);
var
  locProp : ISDOProperty;
  locCS : ISDOChangeSummary;
  locSetting : TValueSetting;
  locNewValue : TSDOCurrency;
  locOldState : TValueState;
begin
  locProp := AObj.getProperty(APropName);
  locCS := AObj.getChangeSummary();
  locNewValue := test_suite_utils.RandomRange(Low(Word),High(Word));

  locOldState.Value.CurrencyValue := AObj.getCurrency(locProp);
  locOldState.IsSet := AObj.isSet(locProp);
  locOldState.IsNull := AObj.isNull(locProp);
  AObj.setCurrency(locProp,locNewValue);
    locSetting := locCS.getOldValue(AObj,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
    CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
    CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
    CheckEquals(locOldState.Value.CurrencyValue,locSetting.getCurrencyValue(),'getCurrencyValue');

  locNewValue := test_suite_utils.RandomRange(Low(Word),High(Word));
  AObj.setCurrency(locProp,locNewValue);  // the recorder should keep the first recorded old values
    locSetting := locCS.getOldValue(AObj,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
    CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
    CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
    CheckEquals(locOldState.Value.CurrencyValue,locSetting.getCurrencyValue(),'getCurrencyValue');
end;

procedure TSDODataObjectCS_Test.logging_currency();
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locObj.getChangeSummary().beginLogging();

  check_currency_logging(locObj,s_currency_prop);
end;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
procedure TSDODataObjectCS_Test.check_double_logging(
  const AObj : ISDODataObject;
  const APropName : string
);
var
  locProp : ISDOProperty;
  locCS : ISDOChangeSummary;
  locSetting : TValueSetting;
  locNewValue : TSDODouble;
  locOldState : TValueState;
begin
  locProp := AObj.getProperty(APropName);
  locCS := AObj.getChangeSummary();
  locNewValue := test_suite_utils.RandomRange(Low(Word),High(Word));

  locOldState.Value.DoubleValue := AObj.getDouble(locProp);
  locOldState.IsSet := AObj.isSet(locProp);
  locOldState.IsNull := AObj.isNull(locProp);
  AObj.setDouble(locProp,locNewValue);
    locSetting := locCS.getOldValue(AObj,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
    CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
    CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
    CheckEquals(locOldState.Value.DoubleValue,locSetting.getDoubleValue(),'getDoubleValue');

  locNewValue := test_suite_utils.RandomRange(Low(Word),High(Word));
  AObj.setDouble(locProp,locNewValue);  // the recorder should keep the first recorded old values
    locSetting := locCS.getOldValue(AObj,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
    CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
    CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
    CheckEquals(locOldState.Value.DoubleValue,locSetting.getDoubleValue(),'getDoubleValue');
end;

procedure TSDODataObjectCS_Test.logging_double();
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locObj.getChangeSummary().beginLogging();

  check_double_logging(locObj,s_double_prop);
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
procedure TSDODataObjectCS_Test.check_float_logging(
  const AObj : ISDODataObject;
  const APropName : string
);
var
  locProp : ISDOProperty;
  locCS : ISDOChangeSummary;
  locSetting : TValueSetting;
  locNewValue : TSDOFloat;
  locOldState : TValueState;
begin
  locProp := AObj.getProperty(APropName);
  locCS := AObj.getChangeSummary();
  locNewValue := test_suite_utils.RandomRange(Low(Word),High(Word));

  locOldState.Value.FloatValue := AObj.getFloat(locProp);
  locOldState.IsSet := AObj.isSet(locProp);
  locOldState.IsNull := AObj.isNull(locProp);
  AObj.setFloat(locProp,locNewValue);
    locSetting := locCS.getOldValue(AObj,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
    CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
    CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
    CheckEquals(locOldState.Value.FloatValue,locSetting.getFloatValue(),'getFloatValue');

  locNewValue := test_suite_utils.RandomRange(Low(Word),High(Word));
  AObj.setFloat(locProp,locNewValue);  // the recorder should keep the first recorded old values
    locSetting := locCS.getOldValue(AObj,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
    CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
    CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
    CheckEquals(locOldState.Value.FloatValue,locSetting.getFloatValue(),'getFloatValue');
end;

procedure TSDODataObjectCS_Test.logging_float();
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locObj.getChangeSummary().beginLogging();

  check_float_logging(locObj,s_float_prop);
end;
{$ENDIF HAS_SDO_FLOAT}

{$IFDEF HAS_SDO_LONG}
procedure TSDODataObjectCS_Test.check_long_logging(
  const AObj : ISDODataObject;
  const APropName : string
);
var
  locProp : ISDOProperty;
  locCS : ISDOChangeSummary;
  locSetting : TValueSetting;
  locNewValue : TSDOLong;
  locOldState : TValueState;
begin
  locProp := AObj.getProperty(APropName);
  locCS := AObj.getChangeSummary();
  locNewValue := test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong));

  locOldState.Value.LongValue := AObj.getLong(locProp);
  locOldState.IsSet := AObj.isSet(locProp);
  locOldState.IsNull := AObj.isNull(locProp);
  AObj.setLong(locProp,locNewValue);
    locSetting := locCS.getOldValue(AObj,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
    CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
    CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
    CheckEquals(locOldState.Value.LongValue,locSetting.getLongValue(),'getLongValue');

  locNewValue := test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong));
  AObj.setLong(locProp,locNewValue);  // the recorder should keep the first recorded old values
    locSetting := locCS.getOldValue(AObj,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
    CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
    CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
    CheckEquals(locOldState.Value.LongValue,locSetting.getLongValue(),'getLongValue');
end;

procedure TSDODataObjectCS_Test.logging_long();
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locObj.getChangeSummary().beginLogging();

  check_long_logging(locObj,s_long_prop);
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure TSDODataObjectCS_Test.check_short_logging(
  const AObj : ISDODataObject;
  const APropName : string
);
var
  locProp : ISDOProperty;
  locCS : ISDOChangeSummary;
  locSetting : TValueSetting;
  locNewValue : TSDOShort;
  locOldState : TValueState;
begin
  locProp := AObj.getProperty(APropName);
  locCS := AObj.getChangeSummary();
  locNewValue := RandomRange(Low(TSDOShort),High(TSDOShort));

  locOldState.Value.ShortValue := AObj.getShort(locProp);
  locOldState.IsSet := AObj.isSet(locProp);
  locOldState.IsNull := AObj.isNull(locProp);
  AObj.setShort(locProp,locNewValue);
    locSetting := locCS.getOldValue(AObj,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
    CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
    CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
    CheckEquals(locOldState.Value.ShortValue,locSetting.getShortValue(),'getShortValue');

  locNewValue := RandomRange(Low(TSDOShort),High(TSDOShort));
  AObj.setShort(locProp,locNewValue);  // the recorder should keep the first recorded old values
    locSetting := locCS.getOldValue(AObj,locProp);
    CheckNotEquals(PtrUInt(nil), PtrUInt(locSetting), 'getOldValue()');
    CheckEquals(locOldState.IsSet,locSetting.isSet,'isSet');
    CheckEquals(locOldState.IsNull,locSetting.isNull,'ISNull');
    CheckEquals(locOldState.Value.ShortValue,locSetting.getShortValue(),'getShortValue');
end;

procedure TSDODataObjectCS_Test.logging_short();
var
  locObj : ISDODataObject;
begin
  locObj := FFactory.createNew(s_uri,s_type_object_A);
  locObj.getChangeSummary().beginLogging();

  check_short_logging(locObj,s_short_prop);
end;
{$ENDIF HAS_SDO_SHORT}

procedure TSDODataObjectCS_Test.SetUp();
begin
  inherited;
  FFactory := Create_Factory();
end;

procedure TSDODataObjectCS_Test.TearDown();
begin
  FFactory := nil;
  inherited;
end;

{ TSDOChangeSummaryMultiValueProps_Test }

procedure TSDOChangeSummaryMultiValueProps_Test.CheckEquals(expected,
  actual: TSDODate; msg: string; const AStrict: Boolean);
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

procedure TSDOChangeSummaryMultiValueProps_Test.getChangedDataObjects_contained_delete_multiprop_1();
var
  locFac : ISDODataFactory;
  locPL, locPJ0, locPJ1, locP1, locP2, locP3 : ISDODataObject;
  locCS : ISDOChangeSummary;
  i : PtrInt;
  ls : ISDOChangedDataObjectList;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'ProjectList',[]);
  locFac.AddType(s_uri,'ProjectType',[]);
  locFac.AddType(s_uri,'Person',[]);
    locFac.addProperty(s_uri,'ProjectList','Project',s_uri,'ProjectType',[pfIsContainment,pfIsMany]);
    locFac.addProperty(s_uri,'ProjectList',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);
      locFac.addProperty(s_uri,'ProjectType','Member',s_uri,'Person',[pfIsContainment,pfIsMany]);
        locFac.addProperty(s_uri,'Person','Manager',s_uri,'Person',[]);
          locFac.addProperty(s_uri,'Person','Name',sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);

  locPL := locFac.createNew(s_uri,'ProjectList');
  locCS := locPL.getChangeSummary();
  locCS.endLogging();
    locPJ0 := locPL.createDataObject('Project');
    locPL.getList('Project').append(locPJ0);
    locPJ1 := locPL.createDataObject('Project');
    locPL.getList('Project').append(locPJ1);
      locP1 := locPJ1.createDataObject('Member');
      locPJ1.getList('Member').append(locP1);
        locP1.setString('Name', 'P1 person');
      locP2 := locPJ1.createDataObject('Member');
      locPJ1.getList('Member').append(locP2);
        locP2.setString('Name', 'P2 person');
        locP2.setDataObject('Manager', locPJ1.getDataObject('Member[Name="P1 person"]'));
      locP3 := locPJ1.createDataObject('Member');
      locPJ1.getList('Member').append(locP3);
        locP3.setString('Name', 'P3 person');
        locP1.setDataObject('Manager', locPJ1.getDataObject('Member[Name="P3 person"]'));

  locCS.beginLogging();
    //locPJ2 := locPL
    locPL.getList('Project').delete(1);

  ls := locCS.getChangedDataObjects();
  i := IndexOf(locP1,ls);
  Check( ( i < 0 ) or ( Ord(ls.getType(i)) = Ord(ctDelete) ) );
  Check(locCS.isDeleted(locPJ1));

  i := IndexOf(locP2,ls);
  Check( ( i < 0 ) or ( Ord(ls.getType(i)) = Ord(ctDelete) ) );
  Check(locCS.isDeleted(locPJ1));
end;

procedure TSDOChangeSummaryMultiValueProps_Test.getChangedDataObjects_contained_delete_multiprop_2;
var
  locFac : ISDODataFactory;
  locA, locB, locC : ISDODataObject;
  locCS : ISDOChangeSummary;
  ls : ISDOChangedDataObjectList;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
  locFac.AddType(s_uri,'b',[]);
  locFac.AddType(s_uri,'c',[]);
    locFac.addProperty(s_uri,'a','p_ab',s_uri,'b',[pfIsContainment]);
    locFac.addProperty(s_uri,'b','p_bc',s_uri,'c',[pfIsContainment,pfIsMany]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

  locA := locFac.createNew(s_uri,'a');
  locCS := locA.getChangeSummary();
  locCS.endLogging();
    locB := locA.createDataObject('p_ab');
  locCS.beginLogging();
    locC := locFac.createNew(s_uri,'c');
    locB.getList('p_bc').append(locC);
  locA.setDataObject('p_ab',nil);

  ls := locCS.getChangedDataObjects();
  CheckEquals(2, ls.size());
end;

procedure TSDOChangeSummaryMultiValueProps_Test.getChangedDataObjects_contained_delete_multiprop_3();
var
  locFac : ISDODataFactory;
  locA, locB, locC : ISDODataObject;
  locCS : ISDOChangeSummary;
  ls : ISDOChangedDataObjectList;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
  locFac.AddType(s_uri,'b',[]);
  locFac.AddType(s_uri,'c',[]);
    locFac.addProperty(s_uri,'a','p_ab',s_uri,'b',[pfIsContainment,pfIsMany]);
    locFac.addProperty(s_uri,'b','p_bc',s_uri,'c',[pfIsContainment,pfIsMany]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

  locA := locFac.createNew(s_uri,'a');
  locCS := locA.getChangeSummary();
  locCS.endLogging();
    locB := locA.createDataObject('p_ab');
    locA.getList('p_ab').append(locB);
  locCS.beginLogging();
    locC := locFac.createNew(s_uri,'c');
    locB.getList('p_bc').append(locC);
  locA.getList('p_ab').delete(0);

  ls := locCS.getChangedDataObjects();
  CheckEquals(2, ls.size());
end;

procedure TSDOChangeSummaryMultiValueProps_Test.getOldValues_bool();
const
  LOCAL_PROP = s_bool_propList;

  procedure check_empty_list(const AList : ISDOSettingList);
  begin
    CheckNotEquals(PtrUInt(nil), PtrUInt(AList));
    CheckEquals(0, AList.size());
  end;

var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  ls : ISDOSettingList;
  vls : ISDODataObjectList;
  vvals : array of TSDOBoolean;
  vvals_length, i : PtrInt;
  s : TValueSetting;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();
  check_empty_list(cs.getOldValues(x));

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := ( ( RandomRange(-1234,56789) mod 3 ) = 0 );
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.append(vvals[i]);
      check_empty_list(cs.getOldValues(x));

    cs.beginLogging();
      vls.append(( RandomRange(-1234,56789) mod 3 ) = 0);
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(( RandomRange(-1234,56789) mod 3 ) = 0);
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), ( RandomRange(-1234,56789) mod 3 ) = 0);
      vls.setBoolean(RandomRange(0, ( vls.size() - 1 ) ), ( RandomRange(-1234,56789) mod 3 ) = 0);
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(( RandomRange(-1234,56789) mod 3 ) = 0);
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      ls := cs.getOldValues(x);
      CheckNotEquals(PtrUInt(nil), PtrUInt(ls));
      CheckEquals(vvals_length, ls.size(), 'ls.size()');
      for i := 0 to Pred(vvals_length) do begin
        s := ls.getItem(i);
        CheckEquals(PtrUInt(x.getProperty(LOCAL_PROP)), PtrUInt(s.getProperty()), Format('i = %d; getProperty',[i]));
        CheckEquals(i, s.getIndex(), Format('i = %d; getIndex',[i]));
        CheckEquals(True, s.isSet(), Format('i = %d; isSet',[i]));
        CheckEquals(False, s.isNull(), Format('i = %d; isNull',[i]));
        CheckEquals(vvals[i], s.getBooleanValue(), Format('i = %d; getBooleanValue',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.getOldValues_byte;
const
  LOCAL_PROP = s_byte_propList;

  procedure check_empty_list(const AList : ISDOSettingList);
  begin
    CheckNotEquals(PtrUInt(nil), PtrUInt(AList));
    CheckEquals(0, AList.size());
  end;

var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  ls : ISDOSettingList;
  vls : ISDODataObjectList;
  vvals : array of TSDOByte;
  vvals_length, i : PtrInt;
  s : TValueSetting;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();
  check_empty_list(cs.getOldValues(x));

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := RandomRange(Low(TSDOByte),High(TSDOByte));
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.append(vvals[i]);
      check_empty_list(cs.getOldValues(x));

    cs.beginLogging();
      vls.append(RandomRange(Low(TSDOByte),High(TSDOByte)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomRange(Low(TSDOByte),High(TSDOByte)));
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(Low(TSDOByte),High(TSDOByte)));
      vls.setByte(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(Low(TSDOByte),High(TSDOByte)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomRange(Low(TSDOByte),High(TSDOByte)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      ls := cs.getOldValues(x);
      CheckNotEquals(PtrUInt(nil), PtrUInt(ls));
      CheckEquals(vvals_length, ls.size(), 'ls.size()');
      for i := 0 to Pred(vvals_length) do begin
        s := ls.getItem(i);
        CheckEquals(PtrUInt(x.getProperty(LOCAL_PROP)), PtrUInt(s.getProperty()), Format('i = %d; getProperty',[i]));
        CheckEquals(i, s.getIndex(), Format('i = %d; getIndex',[i]));
        CheckEquals(True, s.isSet(), Format('i = %d; isSet',[i]));
        CheckEquals(False, s.isNull(), Format('i = %d; isNull',[i]));
        CheckEquals(vvals[i], s.getByteValue(), Format('i = %d; getByteValue',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.getOldValues_date();
const
  LOCAL_PROP = s_date_propList;

  procedure check_empty_list(const AList : ISDOSettingList);
  begin
    CheckNotEquals(PtrUInt(nil), PtrUInt(AList));
    CheckEquals(0, AList.size());
  end;

var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  ls : ISDOSettingList;
  vls : ISDODataObjectList;
  vvals : array of TSDODateTime;
  vvals_length, i : PtrInt;
  s : TValueSetting;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();
  check_empty_list(cs.getOldValues(x));

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := RandomDate();
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.append(vvals[i]);
      check_empty_list(cs.getOldValues(x));

    cs.beginLogging();
      vls.append(RandomDate());
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomDate());
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomDate());
      vls.setDate(RandomRange(0, ( vls.size() - 1 ) ), RandomDate());
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomDate());
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      ls := cs.getOldValues(x);
      CheckNotEquals(PtrUInt(nil), PtrUInt(ls));
      CheckEquals(vvals_length, ls.size(), 'ls.size()');
      for i := 0 to Pred(vvals_length) do begin
        s := ls.getItem(i);
        CheckEquals(PtrUInt(x.getProperty(LOCAL_PROP)), PtrUInt(s.getProperty()), Format('i = %d; getProperty',[i]));
        CheckEquals(i, s.getIndex(), Format('i = %d; getIndex',[i]));
        CheckEquals(True, s.isSet(), Format('i = %d; isSet',[i]));
        CheckEquals(False, s.isNull(), Format('i = %d; isNull',[i]));
        CheckEquals(vvals[i], s.getDateValue(), Format('i = %d; getDateValue',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.getOldValues_integer;
const
  LOCAL_PROP = s_integer_propList;

  procedure check_empty_list(const AList : ISDOSettingList);
  begin
    CheckNotEquals(PtrUInt(nil), PtrUInt(AList));
    CheckEquals(0, AList.size());
  end;

var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  ls : ISDOSettingList;
  vls : ISDODataObjectList;
  vvals : array of TSDOInteger;
  vvals_length, i : PtrInt;
  s : TValueSetting;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();
  check_empty_list(cs.getOldValues(x));

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := RandomRange(-1234,56789);
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.append(vvals[i]);
      check_empty_list(cs.getOldValues(x));

    cs.beginLogging();
      vls.append(RandomRange(-1234,56789));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomRange(-1234,56789));
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(-1234,56789));
      vls.setInteger(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(-1234,56789));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomRange(-1234,56789));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      ls := cs.getOldValues(x);
      CheckNotEquals(PtrUInt(nil), PtrUInt(ls));
      CheckEquals(vvals_length, ls.size(), 'ls.size()');
      for i := 0 to Pred(vvals_length) do begin
        s := ls.getItem(i);
        CheckEquals(PtrUInt(x.getProperty(LOCAL_PROP)), PtrUInt(s.getProperty()), Format('i = %d; getProperty',[i]));
        CheckEquals(i, s.getIndex(), Format('i = %d; getIndex',[i]));
        CheckEquals(True, s.isSet(), Format('i = %d; isSet',[i]));
        CheckEquals(False, s.isNull(), Format('i = %d; isNull',[i]));
        CheckEquals(vvals[i], s.getIntegerValue(), Format('i = %d; getIntegerValue',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.getOldValues_objects();
const
  LOCAL_PROP = s_Employee;

  function create_factory () : ISDODataFactory;
  var
    locFac : ISDODataFactory;
  begin
    locFac := TSDODataFactory.Create() as ISDODataFactory;
    locFac.AddType(s_uri,s_EmployeeType,[]);
      locFac.addProperty(s_uri, s_EmployeeType,'name',sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
      locFac.addProperty(s_uri, s_EmployeeType,'SN',sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.AddType(s_uri,s_Department,[]);
      locFac.addProperty(s_uri,s_Department,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
      locFac.addProperty(s_uri,s_Department,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
      locFac.addProperty(s_uri,s_Department,s_Employee,s_uri,s_EmployeeType,[pfIsMany,pfIsContainment]);
      locFac.addProperty(s_uri,s_Department,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);
    Result := locFac;
  end;

  procedure check_empty_list(const AList : ISDOSettingList);
  begin
    CheckNotEquals(PtrUInt(nil), PtrUInt(AList));
    CheckEquals(0, AList.size());
  end;

  function create_employee(
    const AFac : ISDODataFactory;
    const AName, ASN : TSDOString
  ) : ISDODataObject;
  begin
    Result := AFac.createNew(s_uri, s_EmployeeType);
    Result.setString(s_name, AName);
    Result.setString(s_sn, ASN);
  end;

  procedure check_employee_equal(const A, B : ISDODataObject; const AMsgPrefix : string);
  begin
    Check(
      ( ( A = nil ) and ( B = nil ) ) or
      ( ( A <> nil ) and ( B <> nil ) ),
      AMsgPrefix
    );
    if ( A <> nil ) then begin
      CheckEquals(A.getString(s_name), B.getString(s_name), Format('%s %s',[AMsgPrefix, s_name]));
      CheckEquals(A.getString(s_sn), B.getString(s_sn), Format('%s %s',[AMsgPrefix, s_sn]));
    end;
  end;

var
  locFac : ISDODataFactory;
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  ls : ISDOSettingList;
  vls : ISDODataObjectList;
  vvals : array of ISDODataObject;
  vvals_length, i : PtrInt;
  s : TValueSetting;
begin
  Randomize();
  locFac := create_factory();
  x := locFac.createNew(s_uri,s_Department);
  cs := x.getChangeSummary();
  check_empty_list(cs.getOldValues(x));

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := create_employee(locFac, RandomString(RandomRange(0,100)), RandomString(RandomRange(0,100)));
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.append(vvals[i]);
      check_empty_list(cs.getOldValues(x));

    cs.beginLogging();
      vls.append(create_employee(locFac, 'sdo', '001'));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(create_employee(locFac, RandomString(RandomRange(0,100)), RandomString(RandomRange(0,100))));
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), create_employee(locFac, RandomString(RandomRange(0,100)), RandomString(RandomRange(0,100))));
      vls.setDataObject(RandomRange(0, ( vls.size() - 1 ) ), create_employee(locFac, RandomString(RandomRange(0,100)), RandomString(RandomRange(0,100))));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(create_employee(locFac, RandomString(RandomRange(0,100)), RandomString(RandomRange(0,100))));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), create_employee(locFac, RandomString(RandomRange(0,100)), RandomString(RandomRange(0,100))));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      ls := cs.getOldValues(x);
      CheckNotEquals(PtrUInt(nil), PtrUInt(ls));
      CheckEquals(vvals_length, ls.size(), 'ls.size()');
      for i := 0 to Pred(vvals_length) do begin
        s := ls.getItem(i);
        CheckEquals(PtrUInt(x.getProperty(LOCAL_PROP)), PtrUInt(s.getProperty()), Format('i = %d; getProperty',[i]));
        CheckEquals(i, s.getIndex(), Format('i = %d; getIndex',[i]));
        CheckEquals(True, s.isSet(), Format('i = %d; isSet',[i]));
        CheckEquals(False, s.isNull(), Format('i = %d; isNull',[i]));
        check_employee_equal(vvals[i],s.getDataObjectValue(), Format('Object[%d]',[i]));
        CheckEquals(True, TSDOEqualityHelper.equal(vvals[i],s.getDataObjectValue()), Format('i = %d; getDataObjectValue',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.getOldValues_string();
const
  LOCAL_PROP = s_string_propList;

  procedure check_empty_list(const AList : ISDOSettingList);
  begin
    CheckNotEquals(PtrUInt(nil), PtrUInt(AList));
    CheckEquals(0, AList.size());
  end;

var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  ls : ISDOSettingList;
  vls : ISDODataObjectList;
  vvals : array of TSDOString;
  vvals_length, i : PtrInt;
  s : TValueSetting;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();
  check_empty_list(cs.getOldValues(x));

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := RandomString(RandomRange(0,100));
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.append(vvals[i]);
      check_empty_list(cs.getOldValues(x));

    cs.beginLogging();
      vls.append(RandomString(RandomRange(0,100)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomString(RandomRange(0,100)));
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomString(RandomRange(0,100)));
      vls.setString(RandomRange(0, ( vls.size() - 1 ) ), RandomString(RandomRange(0,100)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomString(RandomRange(0,100)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      ls := cs.getOldValues(x);
      CheckNotEquals(PtrUInt(nil), PtrUInt(ls));
      CheckEquals(vvals_length, ls.size(), 'ls.size()');
      for i := 0 to Pred(vvals_length) do begin
        s := ls.getItem(i);
        CheckEquals(PtrUInt(x.getProperty(LOCAL_PROP)), PtrUInt(s.getProperty()), Format('i = %d; getProperty',[i]));
        CheckEquals(i, s.getIndex(), Format('i = %d; getIndex',[i]));
        CheckEquals(True, s.isSet(), Format('i = %d; isSet',[i]));
        CheckEquals(False, s.isNull(), Format('i = %d; isNull',[i]));
        CheckEquals(vvals[i], s.getStringValue(), Format('i = %d; getStringValue',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.isDeleted();
var
  f : ISDODataFactory;
  a, b0, b1, b2 : ISDODataObject;
  ls : ISDODataObjectList;
  cs : ISDOChangeSummary;
begin
  f := TSDODataFactory.Create();
  f.AddType(s_uri,'a',[]);
  f.AddType(s_uri,'b',[]);
  f.addProperty(s_uri,'a','p_ab',s_uri,'b',[pfIsMany,pfIsContainment]);
  f.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

  a := f.createNew(s_uri,'a');
  ls := a.getList('p_ab');
  cs := a.getChangeSummary();
  cs.endLogging();
    b0 := a.createDataObject('p_ab');
    ls.append(b0);
  cs.beginLogging();
    b1 := a.createDataObject('p_ab');
    ls.append(b1);
    b2 := a.createDataObject('p_ab');
    ls.append(b2);
    ls.delete(1);
      CheckEquals(False, cs.isDeleted(b0));
      CheckEquals(False, cs.isDeleted(b1), 'Because it has been created when the cs where ON');
      CheckEquals(False, cs.isDeleted(b2));
    ls.delete(0);
      CheckEquals(True, cs.isDeleted(b0));
      CheckEquals(False, cs.isDeleted(b1), 'Because it has been created when the cs where ON');
      CheckEquals(False, cs.isDeleted(b2));
end;

procedure TSDOChangeSummaryMultiValueProps_Test.isDeleted_nested();
var
  f : ISDODataFactory;
  a, b0, b1, b2 : ISDODataObject;
  c01, c02 : ISDODataObject;
  ls : ISDODataObjectList;
  cs : ISDOChangeSummary;
begin
  f := TSDODataFactory.Create();
  f.AddType(s_uri,'a',[]);
  f.AddType(s_uri,'b',[]);
  f.AddType(s_uri,'c',[]);
  f.addProperty(s_uri,'b','p_bc',s_uri,'c',[pfIsMany,pfIsContainment]);
  f.addProperty(s_uri,'a','p_ab',s_uri,'b',[pfIsMany,pfIsContainment]);
  f.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

  a := f.createNew(s_uri,'a');
  ls := a.getList('p_ab');
  cs := a.getChangeSummary();
  cs.endLogging();
    b0 := a.createDataObject('p_ab');
    ls.append(b0);
      c01 := b0.createDataObject('p_bc'); b0.getList('p_bc').append(c01);
      c02 := b0.createDataObject('p_bc'); b0.getList('p_bc').append(c02);
  cs.beginLogging();
    b1 := a.createDataObject('p_ab'); ls.append(b1);
    b2 := a.createDataObject('p_ab'); ls.append(b2);

    ls.delete(1);
      CheckEquals(False, cs.isDeleted(b0));
      CheckEquals(False, cs.isDeleted(b1), 'Because it has been created when the cs where ON');
      CheckEquals(False, cs.isDeleted(b2));
    ls.delete(0);
      CheckEquals(True, cs.isDeleted(b0));
      CheckEquals(False, cs.isDeleted(b1), 'Because it has been created when the cs where ON');
      CheckEquals(False, cs.isDeleted(b2));
end;

procedure TSDOChangeSummaryMultiValueProps_Test.SetUp();
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
      locFactory.addProperty(locObj,s_string_prop,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
        locFactory.addProperty(locObj,s_string_propList,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsMany]);
      locFactory.addProperty(locObj,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);
  end;

begin
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
  Add_ObjectA(s_uri);
  FFactory := locFactory;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.TearDown();
begin
  FFactory := nil;
  inherited;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_bool();
const
  LOCAL_PROP = s_bool_propList;
var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals : array of TSDOBoolean;
  vvals_length, i : PtrInt;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := ( RandomRange(-1234,56789) mod 3 ) = 0;
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.append(vvals[i]);

    cs.beginLogging();
      vls.append(( RandomRange(-1234,56789) mod 3 ) = 0);
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(( RandomRange(-1234,56789) mod 3 ) = 0);
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), ( RandomRange(-1234,56789) mod 3 ) = 0);
      vls.setBoolean(RandomRange(0, ( vls.size() - 1 ) ), ( RandomRange(-1234,56789) mod 3 ) = 0);
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(( RandomRange(-1234,56789) mod 3 ) = 0);
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      CheckEquals(vvals_length, vls.size(), 'vls.size()');
      for i := 0 to Pred(vvals_length) do begin
        CheckEquals(vvals[i], vls.getBoolean(i), Format('Index = %d',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_bool_deleted;
var
  locFac : ISDODataFactory;
  locA : ISDODataObject;
  locCS : ISDOChangeSummary;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
    locFac.addProperty(s_uri,'a','p_ab',sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsMany]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(True);
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(0);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(True);
    locA.getList('p_ab').append(False);
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(1);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(True);
    locA.getList('p_ab').append(False);
    locA.getList('p_ab').append(False);
    locA.getList('p_ab').append(True);
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(3);
  locCS.getOldValues(locA);
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_byte();
const
  LOCAL_PROP = s_byte_propList;
var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals : array of TSDOByte;
  vvals_length, i : PtrInt;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := RandomRange(Low(TSDOByte),High(TSDOByte));
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.append(vvals[i]);

    cs.beginLogging();
      vls.append(RandomRange(Low(TSDOByte),High(TSDOByte)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomRange(Low(TSDOByte),High(TSDOByte)));
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(Low(TSDOByte),High(TSDOByte)));
      vls.setByte(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(Low(TSDOByte),High(TSDOByte)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomRange(Low(TSDOByte),High(TSDOByte)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      CheckEquals(vvals_length, vls.size(), 'vls.size()');
      for i := 0 to Pred(vvals_length) do begin
        CheckEquals(vvals[i], vls.getByte(i), Format('Index = %d',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_byte_deleted();
var
  locFac : ISDODataFactory;
  locA : ISDODataObject;
  locCS : ISDOChangeSummary;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
    locFac.addProperty(s_uri,'a','p_ab',sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[pfIsMany]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(TSDOByte(123));
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(0);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(TSDOByte(23));
    locA.getList('p_ab').append(TSDOByte(45));
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(1);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(TSDOByte(12));
    locA.getList('p_ab').append(TSDOByte(23));
    locA.getList('p_ab').append(TSDOByte(45));
    locA.getList('p_ab').append(TSDOByte(67));
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(3);
  locCS.getOldValues(locA);
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_date();
const
  LOCAL_PROP = s_date_propList;
var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals : array of TSDODateTime;
  vvals_length, i : PtrInt;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := RandomDate();
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.append(vvals[i]);

    cs.beginLogging();
      vls.append(RandomDate());
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomDate());
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomDate());
      vls.setDate(RandomRange(0, ( vls.size() - 1 ) ), RandomDate());
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomDate());
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      CheckEquals(vvals_length, vls.size(), 'vls.size()');
      for i := 0 to Pred(vvals_length) do begin
        CheckEquals(vvals[i], vls.getDate(i), Format('Index = %d',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_date_deleted();
var
  locFac : ISDODataFactory;
  locA : ISDODataObject;
  locCS : ISDOChangeSummary;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
    locFac.addProperty(s_uri,'a','p_ab',sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType],[pfIsMany]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(RandomDate());
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(0);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(RandomDate());
    locA.getList('p_ab').append(RandomDate());
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(1);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(RandomDate());
    locA.getList('p_ab').append(RandomDate());
    locA.getList('p_ab').append(RandomDate());
    locA.getList('p_ab').append(RandomDate());
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(3);
  locCS.getOldValues(locA);
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_integer();
const
  LOCAL_PROP = s_integer_propList;
var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals : array of TSDOInteger;
  vvals_length, i : PtrInt;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := RandomRange(-1234,56789);
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.append(vvals[i]);

    cs.beginLogging();
      vls.append(RandomRange(-1234,56789));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomRange(-1234,56789));
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(-1234,56789));
      vls.setInteger(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(-1234,56789));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomRange(-1234,56789));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      CheckEquals(vvals_length, vls.size(), 'vls.size()');
      for i := 0 to Pred(vvals_length) do begin
        CheckEquals(vvals[i], vls.getInteger(i), Format('Index = %d',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_integer_deleted();
var
  locFac : ISDODataFactory;
  locA : ISDODataObject;
  locCS : ISDOChangeSummary;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
    locFac.addProperty(s_uri,'a','p_ab',sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsMany]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(1210);
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(0);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(23);
    locA.getList('p_ab').append(45);
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(1);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(12);
    locA.getList('p_ab').append(23);
    locA.getList('p_ab').append(45);
    locA.getList('p_ab').append(67);
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(3);
  locCS.getOldValues(locA);
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_nested_bool();
var
  f : ISDODataFactory;
  x, y : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals_a : array of TSDOBoolean;
  vvals_a_length, i : PtrInt;
  vvals_b : array of TSDOBoolean;
  vvals_b_length : PtrInt;
begin
  Randomize();
  f := TSDODataFactory.Create();
    f.AddType(s_uri, 'a', []);
    f.AddType(s_uri, 'b', []);
    f.addProperty(s_uri, 'a', 'p_i', sdo_namespace, SDOTypeDefaultTypeNames[IntegerType], []);
    f.addProperty(s_uri, 'a', 'p_li', sdo_namespace, SDOTypeDefaultTypeNames[BooleanType], [pfIsMany]);
    f.addProperty(s_uri, 'a', 'p_ab', s_uri, 'b', [pfIsContainment]);
    f.addProperty(s_uri, 'a', s_changesummary_prop, sdo_namespace, SDOTypeDefaultTypeNames[ChangeSummaryType], [pfIsReadOnly]);
    f.addProperty(s_uri, 'b', 'p_b_li', sdo_namespace, SDOTypeDefaultTypeNames[BooleanType], [pfIsMany]);
  x := f.createNew(s_uri,'a');
    x.setInteger('p_i', RandomRange(-1234,56789));
    y := x.createDataObject('p_ab');
  cs := x.getChangeSummary();

  vvals_a_length := RandomRange(1,100);
  vvals_b_length := RandomRange(1,100);
  SetLength(vvals_a,vvals_a_length);
  try
    for i := 0 to Pred(vvals_a_length) do
      vvals_a[i] := ( RandomRange(-1234,56789) mod 3 ) = 0;
    SetLength(vvals_b,vvals_b_length);
    for i := 0 to Pred(vvals_b_length) do
      vvals_b[i] := ( RandomRange(-1234,56789) mod 3 ) = 0;

    vls := x.getList('p_li');
      for i := 0 to Pred(vvals_a_length) do
        vls.append(vvals_a[i]);
    vls := y.getList('p_b_li');
      for i := 0 to Pred(vvals_b_length) do
        vls.append(vvals_b[i]);

    cs.beginLogging();
      vls := x.getList('p_li');
        vls.append(( RandomRange(-1234,56789) mod 3 ) = 0);
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(( RandomRange(-1234,56789) mod 3 ) = 0);
        vls.insert(RandomRange(0, ( vls.size() - 1 ) ), ( RandomRange(-1234,56789) mod 3 ) = 0);
        vls.setBoolean(RandomRange(0, ( vls.size() - 1 ) ), ( RandomRange(-1234,56789) mod 3 ) = 0);
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(( RandomRange(-1234,56789) mod 3 ) = 0);
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls := y.getList('p_b_li');
        vls.insert(RandomRange(0, ( vls.size() - 1 ) ), ( RandomRange(-1234,56789) mod 3 ) = 0);
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(( RandomRange(-1234,56789) mod 3 ) = 0);
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(( RandomRange(-1234,56789) mod 3 ) = 0);
        vls.setBoolean(RandomRange(0, ( vls.size() - 1 ) ), ( RandomRange(-1234,56789) mod 3 ) = 0);
        vls.append(( RandomRange(-1234,56789) mod 3 ) = 0);
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));

    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      vls := x.getList('p_li');
        CheckEquals(vvals_a_length, vls.size(), 'x, vls.size()');
        for i := 0 to Pred(vvals_a_length) do begin
          CheckEquals(vvals_a[i], vls.getBoolean(i), Format('x, Index = %d',[i]));
        end;
      vls := y.getList('p_b_li');
        CheckEquals(vvals_b_length, vls.size(), 'y, vls.size()');
        for i := 0 to Pred(vvals_b_length) do begin
          CheckEquals(vvals_b[i], vls.getBoolean(i), Format('y, Index = %d',[i]));
        end;

  finally
    SetLength(vvals_b,0);
    SetLength(vvals_a,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_nested_byte();
const
  PROP_TYPE = ByteType;
var
  f : ISDODataFactory;
  x, y : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals_a : array of TSDOByte;
  vvals_a_length, i : PtrInt;
  vvals_b : array of TSDOByte;
  vvals_b_length : PtrInt;
begin
  Randomize();
  f := TSDODataFactory.Create();
    f.AddType(s_uri, 'a', []);
    f.AddType(s_uri, 'b', []);
    f.addProperty(s_uri, 'a', 'p_i', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], []);
    f.addProperty(s_uri, 'a', 'p_li', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], [pfIsMany]);
    f.addProperty(s_uri, 'a', 'p_ab', s_uri, 'b', [pfIsContainment]);
    f.addProperty(s_uri, 'a', s_changesummary_prop, sdo_namespace, SDOTypeDefaultTypeNames[ChangeSummaryType], [pfIsReadOnly]);
    f.addProperty(s_uri, 'b', 'p_b_li', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], [pfIsMany]);
  x := f.createNew(s_uri,'a');
    x.setByte('p_i', RandomRange(Low(TSDOByte),High(TSDOByte)));
    y := x.createDataObject('p_ab');
  cs := x.getChangeSummary();

  vvals_a_length := RandomRange(1,100);
  vvals_b_length := RandomRange(1,100);
  SetLength(vvals_a,vvals_a_length);
  try
    for i := 0 to Pred(vvals_a_length) do
      vvals_a[i] := RandomRange(Low(TSDOByte),High(TSDOByte));
    SetLength(vvals_b,vvals_b_length);
    for i := 0 to Pred(vvals_b_length) do
      vvals_b[i] := RandomRange(Low(TSDOByte),High(TSDOByte));

    vls := x.getList('p_li');
      for i := 0 to Pred(vvals_a_length) do
        vls.append(vvals_a[i]);
    vls := y.getList('p_b_li');
      for i := 0 to Pred(vvals_b_length) do
        vls.append(vvals_b[i]);

    cs.beginLogging();
      vls := x.getList('p_li');
        vls.append(RandomRange(Low(TSDOByte),High(TSDOByte)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomRange(Low(TSDOByte),High(TSDOByte)));
        vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(Low(TSDOByte),High(TSDOByte)));
        vls.setByte(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(Low(TSDOByte),High(TSDOByte)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomRange(Low(TSDOByte),High(TSDOByte)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls := y.getList('p_b_li');
        vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(Low(TSDOByte),High(TSDOByte)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomRange(Low(TSDOByte),High(TSDOByte)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomRange(Low(TSDOByte),High(TSDOByte)));
        vls.setByte(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(Low(TSDOByte),High(TSDOByte)));
        vls.append(RandomRange(Low(TSDOByte),High(TSDOByte)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));

    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      vls := x.getList('p_li');
        CheckEquals(vvals_a_length, vls.size(), 'x, vls.size()');
        for i := 0 to Pred(vvals_a_length) do begin
          CheckEquals(vvals_a[i], vls.getByte(i), Format('x, Index = %d',[i]));
        end;
      vls := y.getList('p_b_li');
        CheckEquals(vvals_b_length, vls.size(), 'y, vls.size()');
        for i := 0 to Pred(vvals_b_length) do begin
          CheckEquals(vvals_b[i], vls.getByte(i), Format('y, Index = %d',[i]));
        end;

  finally
    SetLength(vvals_b,0);
    SetLength(vvals_a,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_nested_date();
var
  f : ISDODataFactory;
  x, y : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals_a : array of TSDODateTime;
  vvals_a_length, i : PtrInt;
  vvals_b : array of TSDODateTime;
  vvals_b_length : PtrInt;
begin
  Randomize();
  f := TSDODataFactory.Create();
    f.AddType(s_uri, 'a', []);
    f.AddType(s_uri, 'b', []);
    f.addProperty(s_uri, 'a', 'p_i', sdo_namespace, SDOTypeDefaultTypeNames[DateTimeType], []);
    f.addProperty(s_uri, 'a', 'p_li', sdo_namespace, SDOTypeDefaultTypeNames[DateTimeType], [pfIsMany]);
    f.addProperty(s_uri, 'a', 'p_ab', s_uri, 'b', [pfIsContainment]);
    f.addProperty(s_uri, 'a', s_changesummary_prop, sdo_namespace, SDOTypeDefaultTypeNames[ChangeSummaryType], [pfIsReadOnly]);
    f.addProperty(s_uri, 'b', 'p_b_li', sdo_namespace, SDOTypeDefaultTypeNames[DateTimeType], [pfIsMany]);
  x := f.createNew(s_uri,'a');
    x.setDate('p_i', RandomDate());
    y := x.createDataObject('p_ab');
  cs := x.getChangeSummary();

  vvals_a_length := RandomRange(1,100);
  vvals_b_length := RandomRange(1,100);
  SetLength(vvals_a,vvals_a_length);
  try
    for i := 0 to Pred(vvals_a_length) do
      vvals_a[i] := RandomDate();
    SetLength(vvals_b,vvals_b_length);
    for i := 0 to Pred(vvals_b_length) do
      vvals_b[i] := RandomDate();

    vls := x.getList('p_li');
      for i := 0 to Pred(vvals_a_length) do
        vls.append(vvals_a[i]);
    vls := y.getList('p_b_li');
      for i := 0 to Pred(vvals_b_length) do
        vls.append(vvals_b[i]);

    cs.beginLogging();
      vls := x.getList('p_li');
        vls.append(RandomDate());
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomDate());
        vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomDate());
        vls.setDate(RandomRange(0, ( vls.size() - 1 ) ), RandomDate());
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomDate());
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls := y.getList('p_b_li');
        vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomDate());
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomDate());
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomDate());
        vls.setDate(RandomRange(0, ( vls.size() - 1 ) ), RandomDate());
        vls.append(RandomDate());
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));

    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      vls := x.getList('p_li');
        CheckEquals(vvals_a_length, vls.size(), 'x, vls.size()');
        for i := 0 to Pred(vvals_a_length) do begin
          CheckEquals(vvals_a[i], vls.getDate(i), Format('x, Index = %d',[i]));
        end;
      vls := y.getList('p_b_li');
        CheckEquals(vvals_b_length, vls.size(), 'y, vls.size()');
        for i := 0 to Pred(vvals_b_length) do begin
          CheckEquals(vvals_b[i], vls.getDate(i), Format('y, Index = %d',[i]));
        end;

  finally
    SetLength(vvals_b,0);
    SetLength(vvals_a,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_nested_integer();
var
  f : ISDODataFactory;
  x, y : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals_a : array of TSDOInteger;
  vvals_a_length, i : PtrInt;
  vvals_b : array of TSDOInteger;
  vvals_b_length : PtrInt;
begin
  Randomize();
  f := TSDODataFactory.Create();
    f.AddType(s_uri, 'a', []);
    f.AddType(s_uri, 'b', []);
    f.addProperty(s_uri, 'a', 'p_i', sdo_namespace, SDOTypeDefaultTypeNames[IntegerType], []);
    f.addProperty(s_uri, 'a', 'p_li', sdo_namespace, SDOTypeDefaultTypeNames[IntegerType], [pfIsMany]);
    f.addProperty(s_uri, 'a', 'p_ab', s_uri, 'b', [pfIsContainment]);
    f.addProperty(s_uri, 'a', s_changesummary_prop, sdo_namespace, SDOTypeDefaultTypeNames[ChangeSummaryType], [pfIsReadOnly]);
    f.addProperty(s_uri, 'b', 'p_b_li', sdo_namespace, SDOTypeDefaultTypeNames[IntegerType], [pfIsMany]);
  x := f.createNew(s_uri,'a');
    x.setInteger('p_i', RandomRange(-1234,56789));
    y := x.createDataObject('p_ab');
  cs := x.getChangeSummary();

  vvals_a_length := RandomRange(1,100);
  vvals_b_length := RandomRange(1,100);
  SetLength(vvals_a,vvals_a_length);
  try
    for i := 0 to Pred(vvals_a_length) do
      vvals_a[i] := RandomRange(-1234,56789);
    SetLength(vvals_b,vvals_b_length);
    for i := 0 to Pred(vvals_b_length) do
      vvals_b[i] := RandomRange(-1234,56789);

    vls := x.getList('p_li');
      for i := 0 to Pred(vvals_a_length) do
        vls.append(vvals_a[i]);
    vls := y.getList('p_b_li');
      for i := 0 to Pred(vvals_b_length) do
        vls.append(vvals_b[i]);

    cs.beginLogging();
      vls := x.getList('p_li');
        vls.append(RandomRange(-1234,56789));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomRange(-1234,56789));
        vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(-1234,56789));
        vls.setInteger(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(-1234,56789));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomRange(-1234,56789));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls := y.getList('p_b_li');
        vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(-1234,56789));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomRange(-1234,56789));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomRange(-1234,56789));
        vls.setInteger(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(-1234,56789));
        vls.append(RandomRange(-1234,56789));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));

    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      vls := x.getList('p_li');
        CheckEquals(vvals_a_length, vls.size(), 'x, vls.size()');
        for i := 0 to Pred(vvals_a_length) do begin
          CheckEquals(vvals_a[i], vls.getInteger(i), Format('x, Index = %d',[i]));
        end;
      vls := y.getList('p_b_li');
        CheckEquals(vvals_b_length, vls.size(), 'y, vls.size()');
        for i := 0 to Pred(vvals_b_length) do begin
          CheckEquals(vvals_b[i], vls.getInteger(i), Format('y, Index = %d',[i]));
        end;

  finally
    SetLength(vvals_b,0);
    SetLength(vvals_a,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_nested_object();

  function create_e1(const AFac : ISDODataFactory) : ISDODataObject;
  begin
    Result := AFac.createNew(s_uri, 'e_1');
    Result.setString('p_e1', RandomString(RandomRange(0,100)));
  end;

  function create_e2(const AFac : ISDODataFactory) : ISDODataObject;
  begin
    Result := AFac.createNew(s_uri, 'e_2');
    Result.setString('p_e2', RandomString(RandomRange(0,100)));
  end;

  procedure check_e1_equal(const A, B : ISDODataObject; const AMsgPrefix : string);
  begin
    Check(
      ( ( A = nil ) and ( B = nil ) ) or
      ( ( A <> nil ) and ( B <> nil ) ),
      AMsgPrefix
    );
    if ( A <> nil ) then begin
      CheckEquals(True, TSDOEqualityHelper.equal(A.getDataObject('p_e1'), B.getDataObject('p_e1')), Format('%s %s',[AMsgPrefix, s_name]));
    end;
  end;

  procedure check_e2_equal(const A, B : ISDODataObject; const AMsgPrefix : string);
  begin
    Check(
      ( ( A = nil ) and ( B = nil ) ) or
      ( ( A <> nil ) and ( B <> nil ) ),
      AMsgPrefix
    );
    if ( A <> nil ) then begin
      CheckEquals(True, TSDOEqualityHelper.equal(A.getDataObject('p_e2'), B.getDataObject('p_e2')), Format('%s %s',[AMsgPrefix, s_name]));
    end;
  end;

var
  f : ISDODataFactory;
  x, y : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals_a : array of ISDODataObject;
  vvals_a_length, i : PtrInt;
  vvals_b : array of ISDODataObject;
  vvals_b_length : PtrInt;
begin
  Randomize();
  f := TSDODataFactory.Create();
    f.AddType(s_uri, 'e_1', []);
    f.AddType(s_uri, 'e_2', []);
    f.AddType(s_uri, 'a', []);
    f.AddType(s_uri, 'b', []);
    f.addProperty(s_uri, 'e_1', 'p_e1', sdo_namespace, SDOTypeDefaultTypeNames[StringType], []);
    f.addProperty(s_uri, 'e_2', 'p_e2', sdo_namespace, SDOTypeDefaultTypeNames[StringType], []);
    f.addProperty(s_uri, 'a', 'p_i', sdo_namespace, SDOTypeDefaultTypeNames[IntegerType], []);
    f.addProperty(s_uri, 'a', 'p_li', s_uri, 'e_1', [pfIsMany,pfIsContainment]);
    f.addProperty(s_uri, 'a', 'p_ab', s_uri, 'b', [pfIsContainment]);
    f.addProperty(s_uri, 'a', s_changesummary_prop, sdo_namespace, SDOTypeDefaultTypeNames[ChangeSummaryType], [pfIsReadOnly]);
    f.addProperty(s_uri, 'b', 'p_b_li', s_uri, 'e_2', [pfIsMany,pfIsContainment]);
  x := f.createNew(s_uri,'a');
    x.setInteger('p_i', RandomRange(-1234,56789));
    y := x.createDataObject('p_ab');
  cs := x.getChangeSummary();

  vvals_a_length := RandomRange(1,100);
  vvals_b_length := RandomRange(1,100);
  SetLength(vvals_a,vvals_a_length);
  try
    for i := 0 to Pred(vvals_a_length) do
      vvals_a[i] := create_e1(f);
    SetLength(vvals_b,vvals_b_length);
    for i := 0 to Pred(vvals_b_length) do
      vvals_b[i] := create_e2(f);

    vls := x.getList('p_li');
      for i := 0 to Pred(vvals_a_length) do
        vls.append(vvals_a[i]);
    vls := y.getList('p_b_li');
      for i := 0 to Pred(vvals_b_length) do
        vls.append(vvals_b[i]);

    cs.beginLogging();
      vls := x.getList('p_li');
        vls.append(create_e1(f));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(create_e1(f));
        vls.insert(RandomRange(0, ( vls.size() - 1 ) ), create_e1(f));
        vls.setDataObject(RandomRange(0, ( vls.size() - 1 ) ), create_e1(f));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(create_e1(f));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls := y.getList('p_b_li');
        vls.insert(RandomRange(0, ( vls.size() - 1 ) ), create_e2(f));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(create_e2(f));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(create_e2(f));
        vls.setDataObject(RandomRange(0, ( vls.size() - 1 ) ), create_e2(f));
        vls.append(create_e2(f));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));

    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      vls := x.getList('p_li');
        CheckEquals(vvals_a_length, vls.size(), 'x, vls.size()');
        for i := 0 to Pred(vvals_a_length) do begin
          check_e1_equal(vvals_a[i], vls.getDataObject(i), Format('x, Index = %d',[i]));
        end;
      vls := y.getList('p_b_li');
        CheckEquals(vvals_b_length, vls.size(), 'y, vls.size()');
        for i := 0 to Pred(vvals_b_length) do begin
          check_e2_equal(vvals_b[i], vls.getDataObject(i), Format('y, Index = %d',[i]));
        end;

  finally
    SetLength(vvals_b,0);
    SetLength(vvals_a,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_nested_string();
var
  f : ISDODataFactory;
  x, y : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals_a : array of TSDOString;
  vvals_a_length, i : PtrInt;
  vvals_b : array of TSDOString;
  vvals_b_length : PtrInt;
begin
  Randomize();
  f := TSDODataFactory.Create();
    f.AddType(s_uri, 'a', []);
    f.AddType(s_uri, 'b', []);
    f.addProperty(s_uri, 'a', 'p_i', sdo_namespace, SDOTypeDefaultTypeNames[IntegerType], []);
    f.addProperty(s_uri, 'a', 'p_li', sdo_namespace, SDOTypeDefaultTypeNames[StringType], [pfIsMany]);
    f.addProperty(s_uri, 'a', 'p_ab', s_uri, 'b', [pfIsContainment]);
    f.addProperty(s_uri, 'a', s_changesummary_prop, sdo_namespace, SDOTypeDefaultTypeNames[ChangeSummaryType], [pfIsReadOnly]);
    f.addProperty(s_uri, 'b', 'p_b_li', sdo_namespace, SDOTypeDefaultTypeNames[stringType], [pfIsMany]);
  x := f.createNew(s_uri,'a');
    x.setInteger('p_i', RandomRange(-1234,56789));
    y := x.createDataObject('p_ab');
  cs := x.getChangeSummary();

  vvals_a_length := RandomRange(1,100);
  vvals_b_length := RandomRange(1,100);
  SetLength(vvals_a,vvals_a_length);
  try
    for i := 0 to Pred(vvals_a_length) do
      vvals_a[i] := RandomString(RandomRange(0,100));
    SetLength(vvals_b,vvals_b_length);
    for i := 0 to Pred(vvals_b_length) do
      vvals_b[i] := RandomString(RandomRange(0,100));

    vls := x.getList('p_li');
      for i := 0 to Pred(vvals_a_length) do
        vls.append(vvals_a[i]);
    vls := y.getList('p_b_li');
      for i := 0 to Pred(vvals_b_length) do
        vls.append(vvals_b[i]);

    cs.beginLogging();
      vls := x.getList('p_li');
        vls.append(RandomString(RandomRange(0,100)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomString(RandomRange(0,100)));
        vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomString(RandomRange(0,100)));
        vls.setString(RandomRange(0, ( vls.size() - 1 ) ), RandomString(RandomRange(0,100)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomString(RandomRange(0,100)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls := y.getList('p_b_li');
        vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomString(RandomRange(0,100)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomString(RandomRange(0,100)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomString(RandomRange(0,100)));
        vls.setString(RandomRange(0, ( vls.size() - 1 ) ), RandomString(RandomRange(0,100)));
        vls.append(RandomString(RandomRange(0,100)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));

    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      vls := x.getList('p_li');
        CheckEquals(vvals_a_length, vls.size(), 'x, vls.size()');
        for i := 0 to Pred(vvals_a_length) do begin
          CheckEquals(vvals_a[i], vls.getString(i), Format('x, Index = %d',[i]));
        end;
      vls := y.getList('p_b_li');
        CheckEquals(vvals_b_length, vls.size(), 'y, vls.size()');
        for i := 0 to Pred(vvals_b_length) do begin
          CheckEquals(vvals_b[i], vls.getString(i), Format('y, Index = %d',[i]));
        end;

  finally
    SetLength(vvals_b,0);
    SetLength(vvals_a,0);
  end;
end;

{$IFDEF HAS_SDO_BYTES}
procedure TSDOChangeSummaryMultiValueProps_Test.getOldValues_bytes();
const
  LOCAL_PROP = s_bytes_propList;

  procedure check_empty_list(const AList : ISDOSettingList);
  begin
    CheckNotEquals(PtrUInt(nil), PtrUInt(AList));
    CheckEquals(0, AList.size());
  end;

var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  ls : ISDOSettingList;
  vls : ISDODataObjectList;
  vvals : array of TSDOBytes;
  vvals_length, i : PtrInt;
  s : TValueSetting;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();
  check_empty_list(cs.getOldValues(x));

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := RandomBytes(RandomRange(0,200));
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.appendBytes(vvals[i]);
      check_empty_list(cs.getOldValues(x));

    cs.beginLogging();
      vls.appendBytes(RandomBytes(RandomRange(0,200)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.appendBytes(RandomBytes(RandomRange(0,200)));
      vls.insertBytes(RandomRange(0, ( vls.size() - 1 ) ), RandomBytes(RandomRange(0,200)));
      vls.setBytes(RandomRange(0, ( vls.size() - 1 ) ), RandomBytes(RandomRange(0,200)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.appendBytes(RandomBytes(RandomRange(0,200)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      ls := cs.getOldValues(x);
      CheckNotEquals(PtrUInt(nil), PtrUInt(ls));
      CheckEquals(vvals_length, ls.size(), 'ls.size()');
      for i := 0 to Pred(vvals_length) do begin
        s := ls.getItem(i);
        CheckEquals(PtrUInt(x.getProperty(LOCAL_PROP)), PtrUInt(s.getProperty()), Format('i = %d; getProperty',[i]));
        CheckEquals(i, s.getIndex(), Format('i = %d; getIndex',[i]));
        CheckEquals(True, s.isSet(), Format('i = %d; isSet',[i]));
        CheckEquals(False, s.isNull(), Format('i = %d; isNull',[i]));
        CheckEquals(vvals[i], s.getBytesValue(), Format('i = %d; getBytesValue',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_bytes();
const
  LOCAL_PROP = s_bytes_propList;
var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals : array of TSDOBytes;
  vvals_length, i : PtrInt;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := RandomBytes(RandomRange(0,200));
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.appendBytes(vvals[i]);

    cs.beginLogging();
      vls.appendBytes(RandomBytes(RandomRange(0,200)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.appendBytes(RandomBytes(RandomRange(0,200)));
      vls.insertBytes(RandomRange(0, ( vls.size() - 1 ) ), RandomBytes(RandomRange(0,200)));
      vls.setBytes(RandomRange(0, ( vls.size() - 1 ) ), RandomBytes(RandomRange(0,200)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.appendBytes(RandomBytes(RandomRange(0,200)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      CheckEquals(vvals_length, vls.size(), 'vls.size()');
      for i := 0 to Pred(vvals_length) do begin
        CheckEquals(vvals[i], vls.getBytes(i), Format('Index = %d',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_bytes_deleted();
var
  locFac : ISDODataFactory;
  locA : ISDODataObject;
  locCS : ISDOChangeSummary;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
    locFac.addProperty(s_uri,'a','p_ab',sdo_namespace,SDOTypeDefaultTypeNames[BytesType],[pfIsMany]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').appendBytes(RandomBytes(RandomRange(0,200)));
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(0);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').appendBytes(RandomBytes(RandomRange(0,200)));
    locA.getList('p_ab').appendBytes(RandomBytes(RandomRange(0,200)));
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(1);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').appendBytes(RandomBytes(RandomRange(0,200)));
    locA.getList('p_ab').appendBytes(RandomBytes(RandomRange(0,200)));
    locA.getList('p_ab').appendBytes(RandomBytes(RandomRange(0,200)));
    locA.getList('p_ab').appendBytes(RandomBytes(RandomRange(0,200)));
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(3);
  locCS.getOldValues(locA);
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_nested_bytes();
const
  PROP_TYPE = BytesType;
var
  f : ISDODataFactory;
  x, y : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals_a : array of TSDOBytes;
  vvals_a_length, i : PtrInt;
  vvals_b : array of TSDOBytes;
  vvals_b_length : PtrInt;
begin
  Randomize();
  f := TSDODataFactory.Create();
    f.AddType(s_uri, 'a', []);
    f.AddType(s_uri, 'b', []);
    f.addProperty(s_uri, 'a', 'p_i', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], []);
    f.addProperty(s_uri, 'a', 'p_li', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], [pfIsMany]);
    f.addProperty(s_uri, 'a', 'p_ab', s_uri, 'b', [pfIsContainment]);
    f.addProperty(s_uri, 'a', s_changesummary_prop, sdo_namespace, SDOTypeDefaultTypeNames[ChangeSummaryType], [pfIsReadOnly]);
    f.addProperty(s_uri, 'b', 'p_b_li', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], [pfIsMany]);
  x := f.createNew(s_uri,'a');
    x.setBytes('p_i', RandomBytes(RandomRange(0,200)));
    y := x.createDataObject('p_ab');
  cs := x.getChangeSummary();

  vvals_a_length := RandomRange(1,100);
  vvals_b_length := RandomRange(1,100);
  SetLength(vvals_a,vvals_a_length);
  try
    for i := 0 to Pred(vvals_a_length) do
      vvals_a[i] := RandomBytes(RandomRange(0,200));
    SetLength(vvals_b,vvals_b_length);
    for i := 0 to Pred(vvals_b_length) do
      vvals_b[i] := RandomBytes(RandomRange(0,200));

    vls := x.getList('p_li');
      for i := 0 to Pred(vvals_a_length) do
        vls.appendBytes(vvals_a[i]);
    vls := y.getList('p_b_li');
      for i := 0 to Pred(vvals_b_length) do
        vls.appendBytes(vvals_b[i]);

    cs.beginLogging();
      vls := x.getList('p_li');
        vls.appendBytes(RandomBytes(RandomRange(0,200)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.appendBytes(RandomBytes(RandomRange(0,200)));
        vls.insertBytes(RandomRange(0, ( vls.size() - 1 ) ), RandomBytes(RandomRange(0,200)));
        vls.setBytes(RandomRange(0, ( vls.size() - 1 ) ), RandomBytes(RandomRange(0,200)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.appendBytes(RandomBytes(RandomRange(0,200)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls := y.getList('p_b_li');
        vls.insertBytes(RandomRange(0, ( vls.size() - 1 ) ), RandomBytes(RandomRange(0,200)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.appendBytes(RandomBytes(RandomRange(0,200)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.appendBytes(RandomBytes(RandomRange(0,200)));
        vls.setBytes(RandomRange(0, ( vls.size() - 1 ) ), RandomBytes(RandomRange(0,200)));
        vls.appendBytes(RandomBytes(RandomRange(0,200)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));

    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      vls := x.getList('p_li');
        CheckEquals(vvals_a_length, vls.size(), 'x, vls.size()');
        for i := 0 to Pred(vvals_a_length) do begin
          CheckEquals(vvals_a[i], vls.getBytes(i), Format('x, Index = %d',[i]));
        end;
      vls := y.getList('p_b_li');
        CheckEquals(vvals_b_length, vls.size(), 'y, vls.size()');
        for i := 0 to Pred(vvals_b_length) do begin
          CheckEquals(vvals_b[i], vls.getBytes(i), Format('y, Index = %d',[i]));
        end;

  finally
    SetLength(vvals_b,0);
    SetLength(vvals_a,0);
  end;
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
procedure TSDOChangeSummaryMultiValueProps_Test.getOldValues_char();
const
  LOCAL_PROP = s_char_propList;

  procedure check_empty_list(const AList : ISDOSettingList);
  begin
    CheckNotEquals(PtrUInt(nil), PtrUInt(AList));
    CheckEquals(0, AList.size());
  end;

var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  ls : ISDOSettingList;
  vls : ISDODataObjectList;
  vvals : array of TSDOChar;
  vvals_length, i : PtrInt;
  s : TValueSetting;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();
  check_empty_list(cs.getOldValues(x));

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.append(vvals[i]);
      check_empty_list(cs.getOldValues(x));

    cs.beginLogging();
      vls.append(TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
      vls.setCharacter(RandomRange(0, ( vls.size() - 1 ) ), TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      ls := cs.getOldValues(x);
      CheckNotEquals(PtrUInt(nil), PtrUInt(ls));
      CheckEquals(vvals_length, ls.size(), 'ls.size()');
      for i := 0 to Pred(vvals_length) do begin
        s := ls.getItem(i);
        CheckEquals(PtrUInt(x.getProperty(LOCAL_PROP)), PtrUInt(s.getProperty()), Format('i = %d; getProperty',[i]));
        CheckEquals(i, s.getIndex(), Format('i = %d; getIndex',[i]));
        CheckEquals(True, s.isSet(), Format('i = %d; isSet',[i]));
        CheckEquals(False, s.isNull(), Format('i = %d; isNull',[i]));
        CheckEquals(vvals[i], s.getCharacterValue(), Format('i = %d; getCharacterValue',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_char();
const
  LOCAL_PROP = s_char_propList;
var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals : array of TSDOChar;
  vvals_length, i : PtrInt;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.append(vvals[i]);

    cs.beginLogging();
      vls.append(TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
      vls.setCharacter(RandomRange(0, ( vls.size() - 1 ) ), TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      CheckEquals(vvals_length, vls.size(), 'vls.size()');
      for i := 0 to Pred(vvals_length) do begin
        CheckEquals(vvals[i], vls.getCharacter(i), Format('Index = %d',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_char_deleted();
var
  locFac : ISDODataFactory;
  locA : ISDODataObject;
  locCS : ISDOChangeSummary;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
    locFac.addProperty(s_uri,'a','p_ab',sdo_namespace,SDOTypeDefaultTypeNames[CharacterType],[pfIsMany]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(TSDOChar('z'));
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(0);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(TSDOChar('d'));
    locA.getList('p_ab').append(TSDOChar('f'));
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(1);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(TSDOChar('w'));
    locA.getList('p_ab').append(TSDOChar(#0));
    locA.getList('p_ab').append(TSDOChar('5'));
    locA.getList('p_ab').append(TSDOChar('n'));
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(3);
  locCS.getOldValues(locA);
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_nested_char();
const
  PROP_TYPE = CharacterType;
var
  f : ISDODataFactory;
  x, y : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals_a : array of TSDOChar;
  vvals_a_length, i : PtrInt;
  vvals_b : array of TSDOChar;
  vvals_b_length : PtrInt;
begin
  Randomize();
  f := TSDODataFactory.Create();
    f.AddType(s_uri, 'a', []);
    f.AddType(s_uri, 'b', []);
    f.addProperty(s_uri, 'a', 'p_i', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], []);
    f.addProperty(s_uri, 'a', 'p_li', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], [pfIsMany]);
    f.addProperty(s_uri, 'a', 'p_ab', s_uri, 'b', [pfIsContainment]);
    f.addProperty(s_uri, 'a', s_changesummary_prop, sdo_namespace, SDOTypeDefaultTypeNames[ChangeSummaryType], [pfIsReadOnly]);
    f.addProperty(s_uri, 'b', 'p_b_li', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], [pfIsMany]);
  x := f.createNew(s_uri,'a');
    x.setCharacter('p_i', TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
    y := x.createDataObject('p_ab');
  cs := x.getChangeSummary();

  vvals_a_length := RandomRange(1,100);
  vvals_b_length := RandomRange(1,100);
  SetLength(vvals_a,vvals_a_length);
  try
    for i := 0 to Pred(vvals_a_length) do
      vvals_a[i] := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));
    SetLength(vvals_b,vvals_b_length);
    for i := 0 to Pred(vvals_b_length) do
      vvals_b[i] := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));

    vls := x.getList('p_li');
      for i := 0 to Pred(vvals_a_length) do
        vls.append(vvals_a[i]);
    vls := y.getList('p_b_li');
      for i := 0 to Pred(vvals_b_length) do
        vls.append(vvals_b[i]);

    cs.beginLogging();
      vls := x.getList('p_li');
        vls.append(TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
        vls.insert(RandomRange(0, ( vls.size() - 1 ) ), TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
        vls.setCharacter(RandomRange(0, ( vls.size() - 1 ) ), TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls := y.getList('p_b_li');
        vls.insert(RandomRange(0, ( vls.size() - 1 ) ), TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
        vls.setCharacter(RandomRange(0, ( vls.size() - 1 ) ), TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
        vls.append(TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar)))));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));

    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      vls := x.getList('p_li');
        CheckEquals(vvals_a_length, vls.size(), 'x, vls.size()');
        for i := 0 to Pred(vvals_a_length) do begin
          CheckEquals(vvals_a[i], vls.getCharacter(i), Format('x, Index = %d',[i]));
        end;
      vls := y.getList('p_b_li');
        CheckEquals(vvals_b_length, vls.size(), 'y, vls.size()');
        for i := 0 to Pred(vvals_b_length) do begin
          CheckEquals(vvals_b[i], vls.getCharacter(i), Format('y, Index = %d',[i]));
        end;

  finally
    SetLength(vvals_b,0);
    SetLength(vvals_a,0);
  end;
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
procedure TSDOChangeSummaryMultiValueProps_Test.getOldValues_currency;
const
  LOCAL_PROP = s_currency_propList;

  procedure check_empty_list(const AList : ISDOSettingList);
  begin
    CheckNotEquals(PtrUInt(nil), PtrUInt(AList));
    CheckEquals(0, AList.size());
  end;

var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  ls : ISDOSettingList;
  vls : ISDODataObjectList;
  vvals : array of TSDOCurrency;
  vvals_length, i : PtrInt;
  s : TValueSetting;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();
  check_empty_list(cs.getOldValues(x));

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := test_suite_utils.RandomRange(Low(Word),High(Word));
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.appendCurrency(vvals[i]);
      check_empty_list(cs.getOldValues(x));

    cs.beginLogging();
      vls.appendCurrency(test_suite_utils.RandomRange(Low(Word),High(Word)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.appendCurrency(test_suite_utils.RandomRange(Low(Word),High(Word)));
      vls.insertCurrency(RandomRange(0, ( vls.size() - 1 ) ), test_suite_utils.RandomRange(Low(Word),High(Word)));
      vls.setCurrency(RandomRange(0, ( vls.size() - 1 ) ), test_suite_utils.RandomRange(Low(Word),High(Word)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.appendCurrency(test_suite_utils.RandomRange(Low(Word),High(Word)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      ls := cs.getOldValues(x);
      CheckNotEquals(PtrUInt(nil), PtrUInt(ls));
      CheckEquals(vvals_length, ls.size(), 'ls.size()');
      for i := 0 to Pred(vvals_length) do begin
        s := ls.getItem(i);
        CheckEquals(PtrUInt(x.getProperty(LOCAL_PROP)), PtrUInt(s.getProperty()), Format('i = %d; getProperty',[i]));
        CheckEquals(i, s.getIndex(), Format('i = %d; getIndex',[i]));
        CheckEquals(True, s.isSet(), Format('i = %d; isSet',[i]));
        CheckEquals(False, s.isNull(), Format('i = %d; isNull',[i]));
        CheckEquals(vvals[i], s.getCurrencyValue(), Format('i = %d; getCurrencyValue',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_currency();
const
  LOCAL_PROP = s_currency_propList;
var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals : array of TSDOCurrency;
  vvals_length, i : PtrInt;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := test_suite_utils.RandomRange(Low(Word),High(Word));
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.appendCurrency(vvals[i]);

    cs.beginLogging();
      vls.appendCurrency(test_suite_utils.RandomRange(Low(Word),High(Word)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.appendCurrency(test_suite_utils.RandomRange(Low(Word),High(Word)));
      vls.insertCurrency(RandomRange(0, ( vls.size() - 1 ) ), test_suite_utils.RandomRange(Low(Word),High(Word)));
      vls.setCurrency(RandomRange(0, ( vls.size() - 1 ) ), test_suite_utils.RandomRange(Low(Word),High(Word)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.appendCurrency(test_suite_utils.RandomRange(Low(Word),High(Word)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      CheckEquals(vvals_length, vls.size(), 'vls.size()');
      for i := 0 to Pred(vvals_length) do begin
        CheckEquals(vvals[i], vls.getCurrency(i), Format('Index = %d',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_currency_deleted();
const
  CURRENCY_CONT_ARRAY : array[0..6] of TSDOCurrency = (
    123963244, 23852215, -52245, 1252222, -11122123, -24245, 522552
  );
var
  locFac : ISDODataFactory;
  locA : ISDODataObject;
  locCS : ISDOChangeSummary;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
    locFac.addProperty(s_uri,'a','p_ab',sdo_namespace,SDOTypeDefaultTypeNames[CurrencyType],[pfIsMany]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').appendCurrency(CURRENCY_CONT_ARRAY[0]);
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(0);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').appendCurrency(CURRENCY_CONT_ARRAY[1]);
    locA.getList('p_ab').appendCurrency(CURRENCY_CONT_ARRAY[2]);
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(1);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').appendCurrency(CURRENCY_CONT_ARRAY[3]);
    locA.getList('p_ab').appendCurrency(CURRENCY_CONT_ARRAY[4]);
    locA.getList('p_ab').appendCurrency(CURRENCY_CONT_ARRAY[5]);
    locA.getList('p_ab').appendCurrency(CURRENCY_CONT_ARRAY[6]);
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(3);
  locCS.getOldValues(locA);
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_nested_currency();
const
  PROP_TYPE = CurrencyType;
var
  f : ISDODataFactory;
  x, y : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals_a : array of TSDOCurrency;
  vvals_a_length, i : PtrInt;
  vvals_b : array of TSDOCurrency;
  vvals_b_length : PtrInt;
begin
  Randomize();
  f := TSDODataFactory.Create();
    f.AddType(s_uri, 'a', []);
    f.AddType(s_uri, 'b', []);
    f.addProperty(s_uri, 'a', 'p_i', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], []);
    f.addProperty(s_uri, 'a', 'p_li', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], [pfIsMany]);
    f.addProperty(s_uri, 'a', 'p_ab', s_uri, 'b', [pfIsContainment]);
    f.addProperty(s_uri, 'a', s_changesummary_prop, sdo_namespace, SDOTypeDefaultTypeNames[ChangeSummaryType], [pfIsReadOnly]);
    f.addProperty(s_uri, 'b', 'p_b_li', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], [pfIsMany]);
  x := f.createNew(s_uri,'a');
    x.setCurrency('p_i', test_suite_utils.RandomRange(Low(Word),High(Word)));
    y := x.createDataObject('p_ab');
  cs := x.getChangeSummary();

  vvals_a_length := RandomRange(1,100);
  vvals_b_length := RandomRange(1,100);
  SetLength(vvals_a,vvals_a_length);
  try
    for i := 0 to Pred(vvals_a_length) do
      vvals_a[i] := test_suite_utils.RandomRange(Low(Word),High(Word));
    SetLength(vvals_b,vvals_b_length);
    for i := 0 to Pred(vvals_b_length) do
      vvals_b[i] := test_suite_utils.RandomRange(Low(Word),High(Word));

    vls := x.getList('p_li');
      for i := 0 to Pred(vvals_a_length) do
        vls.appendCurrency(vvals_a[i]);
    vls := y.getList('p_b_li');
      for i := 0 to Pred(vvals_b_length) do
        vls.appendCurrency(vvals_b[i]);

    cs.beginLogging();
      vls := x.getList('p_li');
        vls.appendCurrency(test_suite_utils.RandomRange(Low(Word),High(Word)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.appendCurrency(test_suite_utils.RandomRange(Low(Word),High(Word)));
        vls.insertCurrency(RandomRange(0, ( vls.size() - 1 ) ), test_suite_utils.RandomRange(Low(Word),High(Word)));
        vls.setCurrency(RandomRange(0, ( vls.size() - 1 ) ), test_suite_utils.RandomRange(Low(Word),High(Word)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.appendCurrency(test_suite_utils.RandomRange(Low(Word),High(Word)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls := y.getList('p_b_li');
        vls.insertCurrency(RandomRange(0, ( vls.size() - 1 ) ), test_suite_utils.RandomRange(Low(Word),High(Word)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.appendCurrency(test_suite_utils.RandomRange(Low(Word),High(Word)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.appendCurrency(test_suite_utils.RandomRange(Low(Word),High(Word)));
        vls.setCurrency(RandomRange(0, ( vls.size() - 1 ) ), test_suite_utils.RandomRange(Low(Word),High(Word)));
        vls.appendCurrency(test_suite_utils.RandomRange(Low(Word),High(Word)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));

    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      vls := x.getList('p_li');
        CheckEquals(vvals_a_length, vls.size(), 'x, vls.size()');
        for i := 0 to Pred(vvals_a_length) do begin
          CheckEquals(vvals_a[i], vls.getCurrency(i), Format('x, Index = %d',[i]));
        end;
      vls := y.getList('p_b_li');
        CheckEquals(vvals_b_length, vls.size(), 'y, vls.size()');
        for i := 0 to Pred(vvals_b_length) do begin
          CheckEquals(vvals_b[i], vls.getCurrency(i), Format('y, Index = %d',[i]));
        end;

  finally
    SetLength(vvals_b,0);
    SetLength(vvals_a,0);
  end;
end;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
procedure TSDOChangeSummaryMultiValueProps_Test.getOldValues_double;
const
  LOCAL_PROP = s_double_propList;

  procedure check_empty_list(const AList : ISDOSettingList);
  begin
    CheckNotEquals(PtrUInt(nil), PtrUInt(AList));
    CheckEquals(0, AList.size());
  end;

var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  ls : ISDOSettingList;
  vls : ISDODataObjectList;
  vvals : array of TSDODouble;
  vvals_length, i : PtrInt;
  s : TValueSetting;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();
  check_empty_list(cs.getOldValues(x));

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := RandomDouble(Low(Word),High(Word));
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.append(vvals[i]);
      check_empty_list(cs.getOldValues(x));

    cs.beginLogging();
      vls.append(RandomDouble(Low(Word),High(Word)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomDouble(Low(Word),High(Word)));
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomDouble(Low(Word),High(Word)));
      vls.setDouble(RandomRange(0, ( vls.size() - 1 ) ), RandomDouble(Low(Word),High(Word)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomDouble(Low(Word),High(Word)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      ls := cs.getOldValues(x);
      CheckNotEquals(PtrUInt(nil), PtrUInt(ls));
      CheckEquals(vvals_length, ls.size(), 'ls.size()');
      for i := 0 to Pred(vvals_length) do begin
        s := ls.getItem(i);
        CheckEquals(PtrUInt(x.getProperty(LOCAL_PROP)), PtrUInt(s.getProperty()), Format('i = %d; getProperty',[i]));
        CheckEquals(i, s.getIndex(), Format('i = %d; getIndex',[i]));
        CheckEquals(True, s.isSet(), Format('i = %d; isSet',[i]));
        CheckEquals(False, s.isNull(), Format('i = %d; isNull',[i]));
        CheckEquals(vvals[i], s.getDoubleValue(), Format('i = %d; getDoubleValue',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_double();
const
  LOCAL_PROP = s_double_propList;
var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals : array of TSDODouble;
  vvals_length, i : PtrInt;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := RandomDouble(Low(Word),High(Word));
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.append(vvals[i]);

    cs.beginLogging();
      vls.append(RandomDouble(Low(Word),High(Word)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomDouble(Low(Word),High(Word)));
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomDouble(Low(Word),High(Word)));
      vls.setDouble(RandomRange(0, ( vls.size() - 1 ) ), RandomDouble(Low(Word),High(Word)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomDouble(Low(Word),High(Word)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      CheckEquals(vvals_length, vls.size(), 'vls.size()');
      for i := 0 to Pred(vvals_length) do begin
        CheckEquals(vvals[i], vls.getDouble(i), Format('Index = %d',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_double_deleted();
const
  DOUBLE_CONT_ARRAY : array[0..6] of TSDODouble = (
    123963244, 23852215, -52245, 1252222, -11122123, -24245, 522552
  );
var
  locFac : ISDODataFactory;
  locA : ISDODataObject;
  locCS : ISDOChangeSummary;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
    locFac.addProperty(s_uri,'a','p_ab',sdo_namespace,SDOTypeDefaultTypeNames[DoubleType],[pfIsMany]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(DOUBLE_CONT_ARRAY[0]);
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(0);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(DOUBLE_CONT_ARRAY[1]);
    locA.getList('p_ab').append(DOUBLE_CONT_ARRAY[2]);
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(1);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(DOUBLE_CONT_ARRAY[3]);
    locA.getList('p_ab').append(DOUBLE_CONT_ARRAY[4]);
    locA.getList('p_ab').append(DOUBLE_CONT_ARRAY[5]);
    locA.getList('p_ab').append(DOUBLE_CONT_ARRAY[6]);
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(3);
  locCS.getOldValues(locA);
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_nested_double();
const
  PROP_TYPE = DoubleType;
var
  f : ISDODataFactory;
  x, y : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals_a : array of TSDODouble;
  vvals_a_length, i : PtrInt;
  vvals_b : array of TSDODouble;
  vvals_b_length : PtrInt;
begin
  Randomize();
  f := TSDODataFactory.Create();
    f.AddType(s_uri, 'a', []);
    f.AddType(s_uri, 'b', []);
    f.addProperty(s_uri, 'a', 'p_i', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], []);
    f.addProperty(s_uri, 'a', 'p_li', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], [pfIsMany]);
    f.addProperty(s_uri, 'a', 'p_ab', s_uri, 'b', [pfIsContainment]);
    f.addProperty(s_uri, 'a', s_changesummary_prop, sdo_namespace, SDOTypeDefaultTypeNames[ChangeSummaryType], [pfIsReadOnly]);
    f.addProperty(s_uri, 'b', 'p_b_li', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], [pfIsMany]);
  x := f.createNew(s_uri,'a');
    x.setDouble('p_i', RandomDouble(Low(Word),High(Word)));
    y := x.createDataObject('p_ab');
  cs := x.getChangeSummary();

  vvals_a_length := RandomRange(1,100);
  vvals_b_length := RandomRange(1,100);
  SetLength(vvals_a,vvals_a_length);
  try
    for i := 0 to Pred(vvals_a_length) do
      vvals_a[i] := RandomDouble(Low(Word),High(Word));
    SetLength(vvals_b,vvals_b_length);
    for i := 0 to Pred(vvals_b_length) do
      vvals_b[i] := RandomDouble(Low(Word),High(Word));

    vls := x.getList('p_li');
      for i := 0 to Pred(vvals_a_length) do
        vls.append(vvals_a[i]);
    vls := y.getList('p_b_li');
      for i := 0 to Pred(vvals_b_length) do
        vls.append(vvals_b[i]);

    cs.beginLogging();
      vls := x.getList('p_li');
        vls.append(RandomDouble(Low(Word),High(Word)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomDouble(Low(Word),High(Word)));
        vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomDouble(Low(Word),High(Word)));
        vls.setDouble(RandomRange(0, ( vls.size() - 1 ) ), RandomDouble(Low(Word),High(Word)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomDouble(Low(Word),High(Word)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls := y.getList('p_b_li');
        vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomDouble(Low(Word),High(Word)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomDouble(Low(Word),High(Word)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomDouble(Low(Word),High(Word)));
        vls.setDouble(RandomRange(0, ( vls.size() - 1 ) ), RandomDouble(Low(Word),High(Word)));
        vls.append(RandomDouble(Low(Word),High(Word)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));

    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      vls := x.getList('p_li');
        CheckEquals(vvals_a_length, vls.size(), 'x, vls.size()');
        for i := 0 to Pred(vvals_a_length) do begin
          CheckEquals(vvals_a[i], vls.getDouble(i), Format('x, Index = %d',[i]));
        end;
      vls := y.getList('p_b_li');
        CheckEquals(vvals_b_length, vls.size(), 'y, vls.size()');
        for i := 0 to Pred(vvals_b_length) do begin
          CheckEquals(vvals_b[i], vls.getDouble(i), Format('y, Index = %d',[i]));
        end;

  finally
    SetLength(vvals_b,0);
    SetLength(vvals_a,0);
  end;
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
procedure TSDOChangeSummaryMultiValueProps_Test.getOldValues_float;
const
  LOCAL_PROP = s_float_propList;

  procedure check_empty_list(const AList : ISDOSettingList);
  begin
    CheckNotEquals(PtrUInt(nil), PtrUInt(AList));
    CheckEquals(0, AList.size());
  end;

var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  ls : ISDOSettingList;
  vls : ISDODataObjectList;
  vvals : array of TSDOFloat;
  vvals_length, i : PtrInt;
  s : TValueSetting;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();
  check_empty_list(cs.getOldValues(x));

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := RandomFloat(Low(Word),High(Word));
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.append(vvals[i]);
      check_empty_list(cs.getOldValues(x));

    cs.beginLogging();
      vls.append(RandomFloat(Low(Word),High(Word)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomFloat(Low(Word),High(Word)));
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomFloat(Low(Word),High(Word)));
      vls.setFloat(RandomRange(0, ( vls.size() - 1 ) ), RandomFloat(Low(Word),High(Word)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomFloat(Low(Word),High(Word)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      ls := cs.getOldValues(x);
      CheckNotEquals(PtrUInt(nil), PtrUInt(ls));
      CheckEquals(vvals_length, ls.size(), 'ls.size()');
      for i := 0 to Pred(vvals_length) do begin
        s := ls.getItem(i);
        CheckEquals(PtrUInt(x.getProperty(LOCAL_PROP)), PtrUInt(s.getProperty()), Format('i = %d; getProperty',[i]));
        CheckEquals(i, s.getIndex(), Format('i = %d; getIndex',[i]));
        CheckEquals(True, s.isSet(), Format('i = %d; isSet',[i]));
        CheckEquals(False, s.isNull(), Format('i = %d; isNull',[i]));
        CheckEquals(vvals[i], s.getFloatValue(), Format('i = %d; getFloatValue',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_float();
const
  LOCAL_PROP = s_float_propList;
var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals : array of TSDOFloat;
  vvals_length, i : PtrInt;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := RandomFloat(Low(Word),High(Word));
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.append(vvals[i]);

    cs.beginLogging();
      vls.append(RandomFloat(Low(Word),High(Word)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomFloat(Low(Word),High(Word)));
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomFloat(Low(Word),High(Word)));
      vls.setFloat(RandomRange(0, ( vls.size() - 1 ) ), RandomFloat(Low(Word),High(Word)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomFloat(Low(Word),High(Word)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      CheckEquals(vvals_length, vls.size(), 'vls.size()');
      for i := 0 to Pred(vvals_length) do begin
        CheckEquals(vvals[i], vls.getFloat(i), Format('Index = %d',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_float_deleted();
const
  FLOAT_CONT_ARRAY : array[0..6] of TSDOFloat = (
    123963244, 23852215, -52245, 1252222, -11122123, -24245, 522552
  );
var
  locFac : ISDODataFactory;
  locA : ISDODataObject;
  locCS : ISDOChangeSummary;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
    locFac.addProperty(s_uri,'a','p_ab',sdo_namespace,SDOTypeDefaultTypeNames[FloatType],[pfIsMany]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(FLOAT_CONT_ARRAY[0]);
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(0);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(FLOAT_CONT_ARRAY[1]);
    locA.getList('p_ab').append(FLOAT_CONT_ARRAY[2]);
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(1);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(FLOAT_CONT_ARRAY[3]);
    locA.getList('p_ab').append(FLOAT_CONT_ARRAY[4]);
    locA.getList('p_ab').append(FLOAT_CONT_ARRAY[5]);
    locA.getList('p_ab').append(FLOAT_CONT_ARRAY[6]);
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(3);
  locCS.getOldValues(locA);
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_nested_float();
const
  PROP_TYPE = FloatType;
var
  f : ISDODataFactory;
  x, y : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals_a : array of TSDOFloat;
  vvals_a_length, i : PtrInt;
  vvals_b : array of TSDOFloat;
  vvals_b_length : PtrInt;
begin
  Randomize();
  f := TSDODataFactory.Create();
    f.AddType(s_uri, 'a', []);
    f.AddType(s_uri, 'b', []);
    f.addProperty(s_uri, 'a', 'p_i', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], []);
    f.addProperty(s_uri, 'a', 'p_li', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], [pfIsMany]);
    f.addProperty(s_uri, 'a', 'p_ab', s_uri, 'b', [pfIsContainment]);
    f.addProperty(s_uri, 'a', s_changesummary_prop, sdo_namespace, SDOTypeDefaultTypeNames[ChangeSummaryType], [pfIsReadOnly]);
    f.addProperty(s_uri, 'b', 'p_b_li', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], [pfIsMany]);
  x := f.createNew(s_uri,'a');
    x.setFloat('p_i', RandomFloat(Low(Word),High(Word)));
    y := x.createDataObject('p_ab');
  cs := x.getChangeSummary();

  vvals_a_length := RandomRange(1,100);
  vvals_b_length := RandomRange(1,100);
  SetLength(vvals_a,vvals_a_length);
  try
    for i := 0 to Pred(vvals_a_length) do
      vvals_a[i] := RandomFloat(Low(Word),High(Word));
    SetLength(vvals_b,vvals_b_length);
    for i := 0 to Pred(vvals_b_length) do
      vvals_b[i] := RandomFloat(Low(Word),High(Word));

    vls := x.getList('p_li');
      for i := 0 to Pred(vvals_a_length) do
        vls.append(vvals_a[i]);
    vls := y.getList('p_b_li');
      for i := 0 to Pred(vvals_b_length) do
        vls.append(vvals_b[i]);

    cs.beginLogging();
      vls := x.getList('p_li');
        vls.append(RandomFloat(Low(Word),High(Word)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomFloat(Low(Word),High(Word)));
        vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomFloat(Low(Word),High(Word)));
        vls.setFloat(RandomRange(0, ( vls.size() - 1 ) ), RandomFloat(Low(Word),High(Word)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomFloat(Low(Word),High(Word)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls := y.getList('p_b_li');
        vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomFloat(Low(Word),High(Word)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomFloat(Low(Word),High(Word)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomFloat(Low(Word),High(Word)));
        vls.setFloat(RandomRange(0, ( vls.size() - 1 ) ), RandomFloat(Low(Word),High(Word)));
        vls.append(RandomFloat(Low(Word),High(Word)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));

    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      vls := x.getList('p_li');
        CheckEquals(vvals_a_length, vls.size(), 'x, vls.size()');
        for i := 0 to Pred(vvals_a_length) do begin
          CheckEquals(vvals_a[i], vls.getFloat(i), Format('x, Index = %d',[i]));
        end;
      vls := y.getList('p_b_li');
        CheckEquals(vvals_b_length, vls.size(), 'y, vls.size()');
        for i := 0 to Pred(vvals_b_length) do begin
          CheckEquals(vvals_b[i], vls.getFloat(i), Format('y, Index = %d',[i]));
        end;

  finally
    SetLength(vvals_b,0);
    SetLength(vvals_a,0);
  end;
end;
{$ENDIF HAS_SDO_FLOAT}

{$IFDEF HAS_SDO_LONG}
procedure TSDOChangeSummaryMultiValueProps_Test.getOldValues_long;
const
  LOCAL_PROP = s_long_propList;

  procedure check_empty_list(const AList : ISDOSettingList);
  begin
    CheckNotEquals(PtrUInt(nil), PtrUInt(AList));
    CheckEquals(0, AList.size());
  end;

var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  ls : ISDOSettingList;
  vls : ISDODataObjectList;
  vvals : array of TSDOLong;
  vvals_length, i : PtrInt;
  s : TValueSetting;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();
  check_empty_list(cs.getOldValues(x));

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong));
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.append(vvals[i]);
      check_empty_list(cs.getOldValues(x));

    cs.beginLogging();
      vls.append(test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
      vls.setLong(RandomRange(0, ( vls.size() - 1 ) ), test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      ls := cs.getOldValues(x);
      CheckNotEquals(PtrUInt(nil), PtrUInt(ls));
      CheckEquals(vvals_length, ls.size(), 'ls.size()');
      for i := 0 to Pred(vvals_length) do begin
        s := ls.getItem(i);
        CheckEquals(PtrUInt(x.getProperty(LOCAL_PROP)), PtrUInt(s.getProperty()), Format('i = %d; getProperty',[i]));
        CheckEquals(i, s.getIndex(), Format('i = %d; getIndex',[i]));
        CheckEquals(True, s.isSet(), Format('i = %d; isSet',[i]));
        CheckEquals(False, s.isNull(), Format('i = %d; isNull',[i]));
        CheckEquals(vvals[i], s.getLongValue(), Format('i = %d; getLongValue',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_long();
const
  LOCAL_PROP = s_long_propList;
var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals : array of TSDOLong;
  vvals_length, i : PtrInt;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong));
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.append(vvals[i]);

    cs.beginLogging();
      vls.append(test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
      vls.setLong(RandomRange(0, ( vls.size() - 1 ) ), test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      CheckEquals(vvals_length, vls.size(), 'vls.size()');
      for i := 0 to Pred(vvals_length) do begin
        CheckEquals(vvals[i], vls.getLong(i), Format('Index = %d',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_long_deleted();
var
  locFac : ISDODataFactory;
  locA : ISDODataObject;
  locCS : ISDOChangeSummary;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
    locFac.addProperty(s_uri,'a','p_ab',sdo_namespace,SDOTypeDefaultTypeNames[LongType],[pfIsMany]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(TSDOLong(123963244));
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(0);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(TSDOLong(23852215));
    locA.getList('p_ab').append(TSDOLong(-52245));
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(1);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(TSDOLong(1252222));
    locA.getList('p_ab').append(TSDOLong(-11122123));
    locA.getList('p_ab').append(TSDOLong(-24245));
    locA.getList('p_ab').append(TSDOLong(522552));
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(3);
  locCS.getOldValues(locA);
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_nested_long();
const
  PROP_TYPE = LongType;
var
  f : ISDODataFactory;
  x, y : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals_a : array of TSDOLong;
  vvals_a_length, i : PtrInt;
  vvals_b : array of TSDOLong;
  vvals_b_length : PtrInt;
begin
  Randomize();
  f := TSDODataFactory.Create();
    f.AddType(s_uri, 'a', []);
    f.AddType(s_uri, 'b', []);
    f.addProperty(s_uri, 'a', 'p_i', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], []);
    f.addProperty(s_uri, 'a', 'p_li', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], [pfIsMany]);
    f.addProperty(s_uri, 'a', 'p_ab', s_uri, 'b', [pfIsContainment]);
    f.addProperty(s_uri, 'a', s_changesummary_prop, sdo_namespace, SDOTypeDefaultTypeNames[ChangeSummaryType], [pfIsReadOnly]);
    f.addProperty(s_uri, 'b', 'p_b_li', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], [pfIsMany]);
  x := f.createNew(s_uri,'a');
    x.setLong('p_i', test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
    y := x.createDataObject('p_ab');
  cs := x.getChangeSummary();

  vvals_a_length := RandomRange(1,100);
  vvals_b_length := RandomRange(1,100);
  SetLength(vvals_a,vvals_a_length);
  try
    for i := 0 to Pred(vvals_a_length) do
      vvals_a[i] := test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong));
    SetLength(vvals_b,vvals_b_length);
    for i := 0 to Pred(vvals_b_length) do
      vvals_b[i] := test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong));

    vls := x.getList('p_li');
      for i := 0 to Pred(vvals_a_length) do
        vls.append(vvals_a[i]);
    vls := y.getList('p_b_li');
      for i := 0 to Pred(vvals_b_length) do
        vls.append(vvals_b[i]);

    cs.beginLogging();
      vls := x.getList('p_li');
        vls.append(test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
        vls.insert(RandomRange(0, ( vls.size() - 1 ) ), test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
        vls.setLong(RandomRange(0, ( vls.size() - 1 ) ), test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls := y.getList('p_b_li');
        vls.insert(RandomRange(0, ( vls.size() - 1 ) ), test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
        vls.setLong(RandomRange(0, ( vls.size() - 1 ) ), test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
        vls.append(test_suite_utils.RandomRange(Low(TSDOLong),High(TSDOLong)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));

    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      vls := x.getList('p_li');
        CheckEquals(vvals_a_length, vls.size(), 'x, vls.size()');
        for i := 0 to Pred(vvals_a_length) do begin
          CheckEquals(vvals_a[i], vls.getLong(i), Format('x, Index = %d',[i]));
        end;
      vls := y.getList('p_b_li');
        CheckEquals(vvals_b_length, vls.size(), 'y, vls.size()');
        for i := 0 to Pred(vvals_b_length) do begin
          CheckEquals(vvals_b[i], vls.getLong(i), Format('y, Index = %d',[i]));
        end;

  finally
    SetLength(vvals_b,0);
    SetLength(vvals_a,0);
  end;
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure TSDOChangeSummaryMultiValueProps_Test.getOldValues_short;
const
  LOCAL_PROP = s_short_propList;

  procedure check_empty_list(const AList : ISDOSettingList);
  begin
    CheckNotEquals(PtrUInt(nil), PtrUInt(AList));
    CheckEquals(0, AList.size());
  end;

var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  ls : ISDOSettingList;
  vls : ISDODataObjectList;
  vvals : array of TSDOShort;
  vvals_length, i : PtrInt;
  s : TValueSetting;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();
  check_empty_list(cs.getOldValues(x));

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := RandomRange(Low(TSDOShort),High(TSDOShort));
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.append(vvals[i]);
      check_empty_list(cs.getOldValues(x));

    cs.beginLogging();
      vls.append(RandomRange(Low(TSDOShort),High(TSDOShort)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomRange(Low(TSDOShort),High(TSDOShort)));
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(Low(TSDOShort),High(TSDOShort)));
      vls.setShort(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(Low(TSDOShort),High(TSDOShort)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomRange(Low(TSDOShort),High(TSDOShort)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      ls := cs.getOldValues(x);
      CheckNotEquals(PtrUInt(nil), PtrUInt(ls));
      CheckEquals(vvals_length, ls.size(), 'ls.size()');
      for i := 0 to Pred(vvals_length) do begin
        s := ls.getItem(i);
        CheckEquals(PtrUInt(x.getProperty(LOCAL_PROP)), PtrUInt(s.getProperty()), Format('i = %d; getProperty',[i]));
        CheckEquals(i, s.getIndex(), Format('i = %d; getIndex',[i]));
        CheckEquals(True, s.isSet(), Format('i = %d; isSet',[i]));
        CheckEquals(False, s.isNull(), Format('i = %d; isNull',[i]));
        CheckEquals(vvals[i], s.getShortValue(), Format('i = %d; getShortValue',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_short();
const
  LOCAL_PROP = s_short_propList;
var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals : array of TSDOShort;
  vvals_length, i : PtrInt;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := RandomRange(Low(TSDOShort),High(TSDOShort));
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.append(vvals[i]);

    cs.beginLogging();
      vls.append(RandomRange(Low(TSDOShort),High(TSDOShort)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomRange(Low(TSDOShort),High(TSDOShort)));
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(Low(TSDOShort),High(TSDOShort)));
      vls.setShort(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(Low(TSDOShort),High(TSDOShort)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomRange(Low(TSDOShort),High(TSDOShort)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      CheckEquals(vvals_length, vls.size(), 'vls.size()');
      for i := 0 to Pred(vvals_length) do begin
        CheckEquals(vvals[i], vls.getShort(i), Format('Index = %d',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_short_deleted();
var
  locFac : ISDODataFactory;
  locA : ISDODataObject;
  locCS : ISDOChangeSummary;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
    locFac.addProperty(s_uri,'a','p_ab',sdo_namespace,SDOTypeDefaultTypeNames[ShortType],[pfIsMany]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(TSDOShort(1239));
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(0);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(TSDOShort(2385));
    locA.getList('p_ab').append(TSDOShort(-5225));
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(1);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(TSDOShort(125));
    locA.getList('p_ab').append(TSDOShort(-1113));
    locA.getList('p_ab').append(TSDOShort(-245));
    locA.getList('p_ab').append(TSDOShort(5222));
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(3);
  locCS.getOldValues(locA);
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_nested_short();
const
  PROP_TYPE = ShortType;
var
  f : ISDODataFactory;
  x, y : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals_a : array of TSDOShort;
  vvals_a_length, i : PtrInt;
  vvals_b : array of TSDOShort;
  vvals_b_length : PtrInt;
begin
  Randomize();
  f := TSDODataFactory.Create();
    f.AddType(s_uri, 'a', []);
    f.AddType(s_uri, 'b', []);
    f.addProperty(s_uri, 'a', 'p_i', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], []);
    f.addProperty(s_uri, 'a', 'p_li', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], [pfIsMany]);
    f.addProperty(s_uri, 'a', 'p_ab', s_uri, 'b', [pfIsContainment]);
    f.addProperty(s_uri, 'a', s_changesummary_prop, sdo_namespace, SDOTypeDefaultTypeNames[ChangeSummaryType], [pfIsReadOnly]);
    f.addProperty(s_uri, 'b', 'p_b_li', sdo_namespace, SDOTypeDefaultTypeNames[PROP_TYPE], [pfIsMany]);
  x := f.createNew(s_uri,'a');
    x.setShort('p_i', RandomRange(Low(TSDOShort),High(TSDOShort)));
    y := x.createDataObject('p_ab');
  cs := x.getChangeSummary();

  vvals_a_length := RandomRange(1,100);
  vvals_b_length := RandomRange(1,100);
  SetLength(vvals_a,vvals_a_length);
  try
    for i := 0 to Pred(vvals_a_length) do
      vvals_a[i] := RandomRange(Low(TSDOShort),High(TSDOShort));
    SetLength(vvals_b,vvals_b_length);
    for i := 0 to Pred(vvals_b_length) do
      vvals_b[i] := RandomRange(Low(TSDOShort),High(TSDOShort));

    vls := x.getList('p_li');
      for i := 0 to Pred(vvals_a_length) do
        vls.append(vvals_a[i]);
    vls := y.getList('p_b_li');
      for i := 0 to Pred(vvals_b_length) do
        vls.append(vvals_b[i]);

    cs.beginLogging();
      vls := x.getList('p_li');
        vls.append(RandomRange(Low(TSDOShort),High(TSDOShort)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomRange(Low(TSDOShort),High(TSDOShort)));
        vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(Low(TSDOShort),High(TSDOShort)));
        vls.setShort(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(Low(TSDOShort),High(TSDOShort)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomRange(Low(TSDOShort),High(TSDOShort)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls := y.getList('p_b_li');
        vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(Low(TSDOShort),High(TSDOShort)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomRange(Low(TSDOShort),High(TSDOShort)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.append(RandomRange(Low(TSDOShort),High(TSDOShort)));
        vls.setShort(RandomRange(0, ( vls.size() - 1 ) ), RandomRange(Low(TSDOShort),High(TSDOShort)));
        vls.append(RandomRange(Low(TSDOShort),High(TSDOShort)));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
        vls.delete(RandomRange(0, ( vls.size() - 1 ) ));

    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      vls := x.getList('p_li');
        CheckEquals(vvals_a_length, vls.size(), 'x, vls.size()');
        for i := 0 to Pred(vvals_a_length) do begin
          CheckEquals(vvals_a[i], vls.getShort(i), Format('x, Index = %d',[i]));
        end;
      vls := y.getList('p_b_li');
        CheckEquals(vvals_b_length, vls.size(), 'y, vls.size()');
        for i := 0 to Pred(vvals_b_length) do begin
          CheckEquals(vvals_b[i], vls.getShort(i), Format('y, Index = %d',[i]));
        end;

  finally
    SetLength(vvals_b,0);
    SetLength(vvals_a,0);
  end;
end;
{$ENDIF HAS_SDO_SHORT}

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_object;
const
  LOCAL_PROP = s_Employee;

  function create_factory () : ISDODataFactory;
  var
    locFac : ISDODataFactory;
  begin
    locFac := TSDODataFactory.Create() as ISDODataFactory;
    locFac.AddType(s_uri,s_EmployeeType,[]);
      locFac.addProperty(s_uri, s_EmployeeType,'name',sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
      locFac.addProperty(s_uri, s_EmployeeType,'SN',sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.AddType(s_uri,s_Department,[]);
      locFac.addProperty(s_uri,s_Department,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
      locFac.addProperty(s_uri,s_Department,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
      locFac.addProperty(s_uri,s_Department,s_Employee,s_uri,s_EmployeeType,[pfIsMany,pfIsContainment]);
      locFac.addProperty(s_uri,s_Department,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);
    Result := locFac;
  end;

  procedure check_empty_list(const AList : ISDOSettingList);
  begin
    CheckNotEquals(PtrUInt(nil), PtrUInt(AList));
    CheckEquals(0, AList.size());
  end;

  function create_employee(
    const AFac : ISDODataFactory;
    const AName, ASN : TSDOString
  ) : ISDODataObject;
  begin
    Result := AFac.createNew(s_uri, s_EmployeeType);
    Result.setString(s_name, AName);
    Result.setString(s_sn, ASN);
  end;

  procedure check_employee_equal(const A, B : ISDODataObject; const AMsgPrefix : string);
  begin
    Check(
      ( ( A = nil ) and ( B = nil ) ) or
      ( ( A <> nil ) and ( B <> nil ) ),
      AMsgPrefix
    );
    if ( A <> nil ) then begin
      CheckEquals(A.getString(s_name), B.getString(s_name), Format('%s %s',[AMsgPrefix, s_name]));
      CheckEquals(A.getString(s_sn), B.getString(s_sn), Format('%s %s',[AMsgPrefix, s_sn]));
    end;
  end;

var
  locFac : ISDODataFactory;
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals : array of ISDODataObject;
  vvals_length, i : PtrInt;
begin
  Randomize();
  locFac := create_factory();
  x := locFac.createNew(s_uri,s_Department);
  cs := x.getChangeSummary();
  check_empty_list(cs.getOldValues(x));

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := create_employee(locFac, RandomString(RandomRange(0,100)), RandomString(RandomRange(0,100)));
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.append(vvals[i]);
      check_empty_list(cs.getOldValues(x));

    cs.beginLogging();
      vls.append(create_employee(locFac, 'sdo', '001'));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(create_employee(locFac, RandomString(RandomRange(0,100)), RandomString(RandomRange(0,100))));
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), create_employee(locFac, RandomString(RandomRange(0,100)), RandomString(RandomRange(0,100))));
      vls.setDataObject(RandomRange(0, ( vls.size() - 1 ) ), create_employee(locFac, RandomString(RandomRange(0,100)), RandomString(RandomRange(0,100))));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(create_employee(locFac, RandomString(RandomRange(0,100)), RandomString(RandomRange(0,100))));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), create_employee(locFac, RandomString(RandomRange(0,100)), RandomString(RandomRange(0,100))));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      CheckEquals(vvals_length, vls.size(), 'vls.size()');
      for i := 0 to Pred(vvals_length) do begin
        check_employee_equal(vvals[i],vls.getDataObject(i), Format('Object[%d]',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_object_deleted;
var
  locFac : ISDODataFactory;
  locA : ISDODataObject;
  locCS : ISDOChangeSummary;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
  locFac.AddType(s_uri,'b',[]);
    locFac.addProperty(s_uri,'a','p_ab',s_uri,'b',[pfIsContainment,pfIsMany]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(locA.createDataObject('p_ab'));
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(0);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(locA.createDataObject('p_ab'));
    locA.getList('p_ab').append(locA.createDataObject('p_ab'));
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(1);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append(locA.createDataObject('p_ab'));
    locA.getList('p_ab').append(locA.createDataObject('p_ab'));
    locA.getList('p_ab').append(locA.createDataObject('p_ab'));
    locA.getList('p_ab').append(locA.createDataObject('p_ab'));
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(3);
  locCS.getOldValues(locA);
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_string();
const
  LOCAL_PROP = s_string_propList;
var
  x : ISDODataObject;
  cs : ISDOChangeSummary;
  vls : ISDODataObjectList;
  vvals : array of TSDOString;
  vvals_length, i : PtrInt;
begin
  Randomize();
  x := FFactory.createNew(s_uri,s_type_object_A);
  cs := x.getChangeSummary();

  vvals_length := RandomRange(1,100);
  SetLength(vvals,vvals_length);
  try
    for i := 0 to Pred(vvals_length) do
      vvals[i] := RandomString(RandomRange(0,100));
    vls := x.getList(LOCAL_PROP);
      for i := 0 to Pred(vvals_length) do
        vls.append(vvals[i]);

    cs.beginLogging();
      vls.append(RandomString(RandomRange(0,100)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomString(RandomRange(0,100)));
      vls.insert(RandomRange(0, ( vls.size() - 1 ) ), RandomString(RandomRange(0,100)));
      vls.setString(RandomRange(0, ( vls.size() - 1 ) ), RandomString(RandomRange(0,100)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.append(RandomString(RandomRange(0,100)));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
      vls.delete(RandomRange(0, ( vls.size() - 1 ) ));
    cs.undoChanges();
      CheckEquals(0, cs.getChangedDataObjects().size, 'cs.getChangedDataObjects().size');
      CheckEquals(vvals_length, vls.size(), 'vls.size()');
      for i := 0 to Pred(vvals_length) do begin
        CheckEquals(vvals[i], vls.getString(i), Format('Index = %d',[i]));
      end;
  finally
    SetLength(vvals,0);
  end;
end;

procedure TSDOChangeSummaryMultiValueProps_Test.undoChanges_string_deleted;
var
  locFac : ISDODataFactory;
  locA : ISDODataObject;
  locCS : ISDOChangeSummary;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
    locFac.addProperty(s_uri,'a','p_ab',sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsMany]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append('azerty');
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(0);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append('sdo');
    locA.getList('p_ab').append('fpc');
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(1);
  locCS.getOldValues(locA);

  locA := locFac.createNew(s_uri,'a');
    locA.getList('p_ab').append('Delphi');
    locA.getList('p_ab').append('Lazarus');
    locA.getList('p_ab').append('WST');
    locA.getList('p_ab').append('OI');
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.getList('p_ab').delete(3);
  locCS.getOldValues(locA);
end;

initialization
  RegisterTest('ChangeSummary',TValueSetting_Test.Suite);
  RegisterTest('ChangeSummary',TSDOSettingList_Test.Suite);
  //RegisterTest('ChangeSummary', TDicho_Test.Suite);
  RegisterTest('ChangeSummary', TSDOChangedDataObjectList_Test.Suite);
  RegisterTest('ChangeSummary', TChangeRecorder_Test.Suite);
  RegisterTest('ChangeSummary', TSDOChangeSummary_Test.Suite);
  RegisterTest('ChangeSummary', TSDODataObjectCS_Test.Suite);
  RegisterTest('ChangeSummary', TSDOChangeSummaryMultiValueProps_Test.Suite);

end.
