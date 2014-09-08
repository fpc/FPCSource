{$INCLUDE sdo_global.inc}
unit test_serializer;

interface
uses
  SysUtils, Classes//, Dialogs
{$IFDEF FPC}
  ,fpcunit, testutils, testregistry
{$ENDIF}
{$IFNDEF FPC}
  ,TestFrameWork
{$ENDIF}
  , test_suite_utils, sdo, sdo_types,
  sdo_serialization, sdo_serialization_utils, sdo_changesummary ;

type

  TListCompareOption = ( lcoCompareOrder );
  TListCompareOptions = set of TListCompareOption;

  { TSDOBaseSerializer_Test }

  TSDOBaseSerializer_Test = class(TWstBaseTest)
  protected
    class function CreateSerializerStream() : ISDOSerializerStream; virtual; abstract;
    procedure Compare(const A, B : ISDOChangedDataObjectListEx; const AOptions : TListCompareOptions = []);overload;
    procedure Compare(const A, B : TDataObjectChangeInfo);overload;
    procedure Compare(const A, B : TValueBuffer; const ADataType : TSDOTypeKind);overload;
    procedure Compare(const A, B : TValueSetting);overload;
    procedure Compare(const A, B : TManyValuePropChangesList);overload;
    procedure CheckEquals(expected, actual: TSDODate; msg: string = ''; const AStrict : Boolean = True); overload;
  protected
    function CreateSdoTypes() : ISDODataFactory;
    function CreateCompanyObject(const AFactory : ISDODataFactory) : ISDODataObject;
  end;

  { TSDOSerializer_Test }

  TSDOSerializer_Test = class(TSDOBaseSerializer_Test)
  protected
    //function GetFileName() : string;
    procedure CompareTypesInclude(
      // A included in B ?
      const A,B : ISDOTypeList
    );
  published
    procedure save_to_stream();
    procedure save_to_stream_without_name();
    procedure save_to_file();
    procedure save_to_file_without_name();
    procedure save_to_file_null_bool_prop();
    procedure save_to_file_null_byte_prop();
    procedure save_to_file_null_bytes_prop(); 
    procedure save_to_file_null_char_prop();
    procedure save_to_file_null_currency_prop();
    procedure save_to_file_null_datetime_prop(); 
    procedure save_to_file_null_double_prop();  
    procedure save_to_file_null_float_prop();  
    procedure save_to_file_null_int_prop();    
    procedure save_to_file_null_long_prop();    
    procedure save_to_file_null_object_prop();    
    procedure save_to_file_null_short_prop();  
    procedure save_to_file_null_string_prop();

    procedure load_from_stream_start_with_empty();
    procedure load_from_stream_one_object();
    procedure load_from_stream_two_object();
    procedure load_from_file_start_with_empty();
    procedure load_from_file_one_object();
    procedure load_from_file_two_object();

    // -- tests with ChangeSummary --
    procedure save_to_file_changesummary_simple();
    procedure save_to_file_changesummary_object_modify_nested();

    procedure save_to_file_changesummary_prop_list_bool();
    procedure save_to_file_changesummary_prop_list_byte();
{$IFDEF HAS_SDO_BYTES}
    procedure save_to_file_changesummary_prop_list_bytes();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure save_to_file_changesummary_prop_list_char();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure save_to_file_changesummary_prop_list_currency();
{$ENDIF HAS_SDO_CURRENCY}
    procedure save_to_file_changesummary_prop_list_date();
{$IFDEF HAS_SDO_DOUBLE}
    procedure save_to_file_changesummary_prop_list_double();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure save_to_file_changesummary_prop_list_float();
{$ENDIF HAS_SDO_FLOAT}
    procedure save_to_file_changesummary_prop_list_integer();
{$IFDEF HAS_SDO_LONG}
    procedure save_to_file_changesummary_prop_list_long();
{$ENDIF HAS_SDO_LONG}
    procedure save_to_file_changesummary_prop_list_object();
    procedure save_to_file_changesummary_prop_list_object_nested();
{$IFDEF HAS_SDO_SHORT}
    procedure save_to_file_changesummary_prop_list_short();
{$ENDIF HAS_SDO_SHORT}
    procedure save_to_file_changesummary_prop_list_string();


    procedure save_to_file_changesummary_object_create();
    procedure save_to_file_changesummary_object_create_cont_ref();
    procedure save_to_file_changesummary_object_delete();
    procedure save_to_file_changesummary_object_delete_nested();
    procedure save_to_file_changesummary_object_delete_2_objects_same_type();
    procedure save_to_file_changesummary_object_2_objects_same_type_del_upd();


    procedure load_from_file_changesummary_simple();
    procedure load_from_file_changesummary_bool();
    procedure load_from_file_changesummary_byte();
{$IFDEF HAS_SDO_BYTES}
    procedure load_from_file_changesummary_bytes();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure load_from_file_changesummary_char();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure load_from_file_changesummary_currency();
{$ENDIF HAS_SDO_CURRENCY}
    procedure load_from_file_changesummary_date();
{$IFDEF HAS_SDO_DOUBLE}
    procedure load_from_file_changesummary_double();
{$ENDIF HAS_SDO_DOUBLE}
    procedure load_from_file_changesummary_integer();
{$IFDEF HAS_SDO_LONG}
    procedure load_from_file_changesummary_long();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_FLOAT}
    procedure load_from_file_changesummary_float();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_SHORT}
    procedure load_from_file_changesummary_short();
{$ENDIF HAS_SDO_SHORT}
    procedure load_from_file_changesummary_string();


    procedure load_from_file_changesummary_object_create_cont_ref();
    procedure load_from_file_changesummary_object_delete();
    procedure load_from_file_changesummary_object_delete_nested();
    procedure load_from_file_changesummary_object_delete_2_objects_same_type();
    procedure load_from_file_changesummary_object_2_objects_same_type_del_upd();
    

    procedure load_from_file_changesummary_prop_list_bool();
    procedure load_from_file_changesummary_prop_list_byte();
{$IFDEF HAS_SDO_BYTES}
    procedure load_from_file_changesummary_prop_list_bytes();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure load_from_file_changesummary_prop_list_char();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure load_from_file_changesummary_prop_list_currency();
{$ENDIF HAS_SDO_CURRENCY}
    procedure load_from_file_changesummary_prop_list_date();
{$IFDEF HAS_SDO_DOUBLE}
    procedure load_from_file_changesummary_prop_list_double();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure load_from_file_changesummary_prop_list_float();
{$ENDIF HAS_SDO_FLOAT}
    procedure load_from_file_changesummary_prop_list_integer();
{$IFDEF HAS_SDO_LONG}
    procedure load_from_file_changesummary_prop_list_long();
{$ENDIF HAS_SDO_LONG}
    procedure load_from_file_changesummary_prop_list_object();
    procedure load_from_file_changesummary_prop_list_object_nested();
{$IFDEF HAS_SDO_SHORT}
    procedure load_from_file_changesummary_prop_list_short();
{$ENDIF HAS_SDO_SHORT}
    procedure load_from_file_changesummary_prop_list_string();

    procedure save_to_and_load_file_ref_prop_crash_1();
    procedure save_to_and_load_file_ref_prop_crash_2();

    procedure load_from_file_reference_property();
    procedure save_object_open_type();
    procedure load_object_open_type();
  end;

  TSDOSerializerXML_Test = class(TSDOSerializer_Test)
  protected
    class function CreateSerializerStream() : ISDOSerializerStream; override;
  end;

  { TSDOSerializerBinary_Test }

  TSDOSerializerBinary_Test = class(TSDOBaseSerializer_Test)
  protected
    class function CreateSerializerStream() : ISDOSerializerStream; override;
  published
    procedure to_stream();
    procedure to_stream_without_name();
    procedure to_stream_changesummary_object_modify_nested();
    procedure to_stream_changesummary_simple();
    procedure to_stream_changesummary_prop_list_byte();
    procedure to_stream_changesummary_prop_list_bool();
    procedure to_stream_changesummary_prop_list_string();
    procedure to_stream_changesummary_prop_list_Integer();
    procedure to_stream_changesummary_prop_list_long();
    procedure to_stream_changesummary_prop_list_double();
    procedure to_stream_changesummary_prop_list_float();
    procedure to_stream_changesummary_prop_list_short();
    procedure to_stream_changesummary_prop_list_char();
    procedure to_stream_changesummary_prop_list_date();
    procedure to_stream_changesummary_prop_list_currency();
    procedure to_stream_changesummary_prop_list_bytes();
    procedure to_stream_changesummary_prop_list_objects();
    procedure to_stream_changesummary_object_delete();
    procedure to_stream_changesummary_object_delete_nested();
    procedure to_stream_changesummary_object_create_cont_ref();
    procedure null_bool_prop();
    procedure null_byte_prop();
    procedure null_bytes_prop();
    procedure null_char_prop();
    procedure null_currency_prop();
    procedure null_datetime_prop();
    procedure null_double_prop();
    procedure null_float_prop();
    procedure null_integer_prop();
    procedure null_long_prop();
    procedure null_object_prop();
    procedure null_short_prop();
    procedure null_string_prop();

    procedure changesummary_object_create();
    procedure changesummary_object_create_cont_ref();
    procedure changesummary_object_delete();
    procedure changesummary_object_delete_nested();
    procedure changesummary_object_delete_2_objects_same_type();
  end;

implementation

uses
  typinfo,
  sdo_datafactory, sdo_dataobject,
  sdo_serialization_xml, sdo_serialization_binary
{$IFNDEF FPC}
  , xmldom, sdo_win_xml
{$ELSE}
  , DOM, sdo_fpc_xml, XMLRead, XMLWrite
{$ENDIF}
  , sdo_consts, sdo_utils, Math, DateUtils, sdo_date_utils;

const
  s_uri = 'company.xsd';
  s_Employee = 'Employee';
  s_EmployeeType = 'EmployeeType';
  s_DepartmentType = 'DepartmentType';
  s_CompanyType = 'CompanyType';
  s_company = 'company';
  s_list_bool = 'list_bool';
  s_list_byte = 'list_byte';
  s_list_bytes = 'list_bytes';
  s_list_char = 'list_char';
  s_list_currency = 'list_currency';
  s_list_date = 'list_date';
  s_list_double = 'list_double';
  s_list_float = 'list_float';
  s_list_int = 'list_int';
  s_list_long = 'list_long';
  s_list_object = 'list_object';
  s_list_short = 'list_short';
  s_list_string = 'list_string';
  s_location = 'location';
  s_manager = 'manager';
  s_name = 'name';
  s_number = 'number';
  s_sn = 'SN';
  s_age = 'age';
  s_birthDate = 'birthDate';
  
  s_bool_prop               = 'bool_prop';
  s_byte_prop               = 'byte_prop';
  s_bytes_prop              = 'bytes_prop';
  s_char_prop               = 'char_prop';
  s_currency_prop           = 'currency_prop';
  s_datetime_prop           = 'datetime_prop';
  s_double_prop             = 'double_prop'; 
  s_float_prop              = 'float_prop';      
  s_int_prop                = 'int_prop';       
  s_long_prop               = 'long_prop';   
  s_object_prop             = 'object_prop';
  s_object_type             = 'object_type';
  s_object_type2            = 'object_type2';      
  s_short_prop              = 'short_prop';   
  s_string_prop             = 'string_prop';
  

function CompareNodes(const A,B : TDOMNode) : Boolean;overload;
var
  ca, cb : TDOMNode;
  i : PtrInt;
begin
  if ( A = nil ) and ( B = nil ) then begin
    Result := True;
  end else if ( A <> nil ) and ( B <> nil ) then begin
    Result := False;
    if ( A.NodeName = B.NodeName ) and
       ( A.NodeValue = B.NodeValue )
    then begin
      if ( ( A.FirstChild = nil ) and ( B.FirstChild = nil ) ) or
         ( ( A.FirstChild <> nil ) and ( B.FirstChild <> nil ) )
      then begin
        ca := a.FirstChild;
        cb := b.FirstChild;
        while ( ca <> nil ) do begin
          if not CompareNodes(ca,cb) then
            Exit;
          ca := ca.NextSibling;
          cb := cb.NextSibling;
        end;
        if ( ( A.Attributes = nil ) and ( B.Attributes = nil ) ) or
           ( ( A.Attributes <> nil ) and ( B.Attributes <> nil ) )
        then begin
          if ( A.Attributes <> nil ) then begin
            if ( A.Attributes.Length <> B.Attributes.Length ) then
              Exit;
            if ( A.Attributes.Length > 0 ) then begin
              for i := 0 to Pred(A.Attributes.Length) do begin
                if not CompareNodes(A.Attributes.Item[i],B.Attributes.GetNamedItem(A.Attributes.Item[i].NodeName)) then
                  Exit;
              end;
            end;
          end;
          Result := True;
        end;
      end;
    end;
  end else begin
    Result := False;
  end;
end;

{ TSDOSerializerBinary_Test }

class function TSDOSerializerBinary_Test.CreateSerializerStream: ISDOSerializerStream;
begin
  Result := TSDOSerializationStreamBinary.Create();
end;

procedure TSDOSerializerBinary_Test.to_stream();
var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB : ISDODataObject;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
begin
  locFactoryA := CreateSdoTypes();
  objA := CreateCompanyObject(locFactoryA);

  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(s_CompanyType,objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('company.soap.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.to_stream_without_name();
var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB : ISDODataObject;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
begin
  locFactoryA := CreateSdoTypes();
  objA := CreateCompanyObject(locFactoryA);

  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('company.soap.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.to_stream_changesummary_object_modify_nested();
var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB, locDep, locEmployee : ISDODataObject;
  locCS : ISDOChangeSummary;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
begin
  locFactoryA := TSDODataFactory.Create() as ISDODataFactory;
  locFactoryA.AddType(s_uri,s_DepartmentType,[]);
  locFactoryA.AddType(s_uri,s_Employee,[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_Employee,s_uri,s_Employee,[pfIsContainment]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

    locFactoryA.addProperty(s_uri,s_Employee,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_Employee,s_sn,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_Employee,s_manager,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_Employee,s_age,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[pfIsAttribute]);

  locDep := locFactoryA.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setInteger(s_number,1210);
    locEmployee := locDep.createDataObject(s_Employee);
      locEmployee.setString(s_name,'Inoussa O.');
      locEmployee.setString(s_sn,'002');
      locEmployee.setBoolean(s_manager,True);
      locEmployee.setByte(s_age,12);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
      locEmployee.setString(s_name,'Inoussa OUEDRAOGO');
      locEmployee.setString(s_sn,'001');
      locEmployee.setByte(s_age,32);
  objA := locDep;

  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('change_summary_object_modify_nested.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
    Compare(
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    Compare(
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.to_stream_changesummary_simple();
var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB : ISDODataObject;
  locCS : ISDOChangeSummary;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
begin
  locFactoryA := TSDODataFactory.Create() as ISDODataFactory;
  locFactoryA.AddType(s_uri,s_Employee,[]);
    locFactoryA.addProperty(s_uri,s_Employee,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_Employee,s_sn,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_Employee,s_manager,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_Employee,s_age,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_Employee,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  objA := locFactoryA.createNew(s_uri,s_Employee);
    objA.setString(s_name,'Inoussa O.');
    objA.setString(s_sn,'002');
    objA.setBoolean(s_manager,True);
    objA.setByte(s_age,30);
    locCS := objA.getChangeSummary();

  locCS.beginLogging();
    objA.setString(s_name,'Inoussa OUEDRAOGO');
    objA.setString(s_sn,'001');
    objA.setByte(s_age,32);

  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('change_summary_simple.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
    Compare(
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    Compare(
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.to_stream_changesummary_prop_list_byte();
const
  LIST_PROP_NAME = s_list_byte;
  LIST_PROP_TYPE = ByteType;
var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB : ISDODataObject;
  locCS : ISDOChangeSummary;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
begin
  locFactoryA := TSDODataFactory.Create() as ISDODataFactory;
  locFactoryA.AddType(s_uri,s_DepartmentType,[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,LIST_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LIST_PROP_TYPE],[pfIsMany]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFactoryA.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setByte(s_number,123);
    ls := locDep.getList(LIST_PROP_NAME);
      ls.append(TSDOByte(1));
      ls.append(TSDOByte(2));
      ls.append(TSDOByte(3));
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setByte(0,10);
    ls.append(TSDOByte(123));
    ls.append(TSDOByte(45));
    ls.setByte(1,20);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, TSDOByte(107));
    ls.append(TSDOByte(89));

  objA := locDep;
  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('changesummary_prop_list_byte.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
    Compare(
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    Compare(
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.to_stream_changesummary_prop_list_bool();
const
  LIST_PROP_NAME = s_list_byte;
  LIST_PROP_TYPE = ByteType;
var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB : ISDODataObject;
  locCS : ISDOChangeSummary;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
begin
  locFactoryA := TSDODataFactory.Create() as ISDODataFactory;
  locFactoryA.AddType(s_uri,s_DepartmentType,[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_list_bool,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsMany]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFactoryA.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setInteger(s_number,1210);
    ls := locDep.getList(s_list_bool);
      ls.append(False);
      ls.append(True);
      ls.append(False);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setBoolean(0,True);
    ls.append(True);
    ls.append(True);
    ls.setBoolean(1,False);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, False);
    ls.append(False);

  objA := locDep;
  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('changesummary_prop_list_bool.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
    Compare(
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    Compare(
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.to_stream_changesummary_prop_list_string();
var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB : ISDODataObject;
  locCS : ISDOChangeSummary;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
begin
  locFactoryA := TSDODataFactory.Create() as ISDODataFactory;
  locFactoryA.AddType(s_uri,s_DepartmentType,[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_list_string,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsMany]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFactoryA.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setInteger(s_number,1210);
    ls := locDep.getList(s_list_string);
      ls.append('wst');
      ls.append('sdo');
      ls.append('fpc-lazarus');
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setString(0,'azerty');
    ls.append('Ouagadougou');
    ls.append('BF');
    ls.setString(1,'kis');
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, '107612');
    ls.append('this is a multi words text. Lets test it!');

  objA := locDep;
  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('changesummary_prop_list_string.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
    Compare(
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    Compare(
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.to_stream_changesummary_prop_list_Integer();
var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB : ISDODataObject;
  locCS : ISDOChangeSummary;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
begin
  locFactoryA := TSDODataFactory.Create() as ISDODataFactory;
  locFactoryA.AddType(s_uri,s_DepartmentType,[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_list_int,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsMany]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFactoryA.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setInteger(s_number,1210);
    ls := locDep.getList(s_list_int);
      ls.append(1);
      ls.append(2);
      ls.append(3);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setInteger(0,10);
    ls.append(123);
    ls.append(456);
    ls.setInteger(1,20);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, 1076);
    ls.append(789);

  objA := locDep;
  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('changesummary_prop_list_integer.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
    Compare(
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    Compare(
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.to_stream_changesummary_prop_list_long();
const
  LIST_PROP_NAME = s_list_long;
  LIST_PROP_TYPE = LongType;
var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB : ISDODataObject;
  locCS : ISDOChangeSummary;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
begin
  locFactoryA := TSDODataFactory.Create() as ISDODataFactory;
  locFactoryA.AddType(s_uri,s_DepartmentType,[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,LIST_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LIST_PROP_TYPE],[pfIsMany]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFactoryA.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setByte(s_number,123);
    ls := locDep.getList(LIST_PROP_NAME);
      ls.append(TSDOLong(11111111111111111));
      ls.append(TSDOLong(-2222222222222222));
      ls.append(TSDOLong(333333333333333333));
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setLong(0,4444444444444444444);
    ls.append(TSDOLong(5555555555555555555));
    ls.append(TSDOLong(-6666666666666666666));
    ls.setLong(1,7777777777777777777);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, TSDOLong(8));
    ls.append(TSDOLong(-9));

  objA := locDep;
  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('changesummary_prop_list_long.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
    Compare(
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    Compare(
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
  finally
    strm.Free();
  end;
end;

const
  DOUBLE_VALUES_REPEATED_DIGITED : array[0..8] of TSDODouble = (
    111111111, -22222222, 3333333333, 44444444, 555555555,
    -666666666, 777777777, 8, -9
  );
procedure TSDOSerializerBinary_Test.to_stream_changesummary_prop_list_double();
const
  LIST_PROP_NAME = s_list_double;
  LIST_PROP_TYPE = DoubleType;
var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB : ISDODataObject;
  locCS : ISDOChangeSummary;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
begin
  locFactoryA := TSDODataFactory.Create() as ISDODataFactory;
  locFactoryA.AddType(s_uri,s_DepartmentType,[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,LIST_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LIST_PROP_TYPE],[pfIsMany]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFactoryA.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setByte(s_number,123);
    ls := locDep.getList(LIST_PROP_NAME);
      ls.append(DOUBLE_VALUES_REPEATED_DIGITED[0]);
      ls.append(DOUBLE_VALUES_REPEATED_DIGITED[1]);
      ls.append(DOUBLE_VALUES_REPEATED_DIGITED[2]);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setDouble(0,DOUBLE_VALUES_REPEATED_DIGITED[3]);
    ls.append(DOUBLE_VALUES_REPEATED_DIGITED[4]);
    ls.append(DOUBLE_VALUES_REPEATED_DIGITED[5]);
    ls.setDouble(1,DOUBLE_VALUES_REPEATED_DIGITED[6]);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, DOUBLE_VALUES_REPEATED_DIGITED[7]);
    ls.append(DOUBLE_VALUES_REPEATED_DIGITED[8]);

  objA := locDep;
  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('changesummary_prop_list_double.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
    Compare(
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    Compare(
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
  finally
    strm.Free();
  end;
end;

const
  FLOAT_VALUES_REPEATED_DIGITED : array[0..8] of TSDOFloat = (
    111111111, -222222222, 333333333, 444444444, 5555555555,
    -6666666666, 777777777, 8, -9
  );
procedure TSDOSerializerBinary_Test.to_stream_changesummary_prop_list_float();
const
  LIST_PROP_NAME = s_list_float;
  LIST_PROP_TYPE = FloatType;
var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB : ISDODataObject;
  locCS : ISDOChangeSummary;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
begin
  locFactoryA := TSDODataFactory.Create() as ISDODataFactory;
  locFactoryA.AddType(s_uri,s_DepartmentType,[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,LIST_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LIST_PROP_TYPE],[pfIsMany]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFactoryA.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setByte(s_number,123);
    ls := locDep.getList(LIST_PROP_NAME);
      ls.append(FLOAT_VALUES_REPEATED_DIGITED[0]);
      ls.append(FLOAT_VALUES_REPEATED_DIGITED[1]);
      ls.append(FLOAT_VALUES_REPEATED_DIGITED[2]);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setFloat(0,FLOAT_VALUES_REPEATED_DIGITED[3]);
    ls.append(FLOAT_VALUES_REPEATED_DIGITED[4]);
    ls.append(FLOAT_VALUES_REPEATED_DIGITED[5]);
    ls.setFloat(1,FLOAT_VALUES_REPEATED_DIGITED[6]);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, FLOAT_VALUES_REPEATED_DIGITED[7]);
    ls.append(FLOAT_VALUES_REPEATED_DIGITED[8]);

  objA := locDep;
  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('changesummary_prop_list_float.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
    Compare(
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    Compare(
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.to_stream_changesummary_prop_list_short();
const
  LIST_PROP_NAME = s_list_short;
  LIST_PROP_TYPE = ShortType;
var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB : ISDODataObject;
  locCS : ISDOChangeSummary;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
begin
  locFactoryA := TSDODataFactory.Create() as ISDODataFactory;
  locFactoryA.AddType(s_uri,s_DepartmentType,[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,LIST_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LIST_PROP_TYPE],[pfIsMany]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFactoryA.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setByte(s_number,123);
    ls := locDep.getList(LIST_PROP_NAME);
      ls.append(TSDOShort(1));
      ls.append(TSDOShort(2));
      ls.append(TSDOShort(3));
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setShort(0,10);
    ls.append(TSDOShort(12345));
    ls.append(TSDOShort(-5245));
    ls.setShort(1,20);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, TSDOShort(107));
    ls.append(TSDOShort(89));

  objA := locDep;
  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('changesummary_prop_list_short.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
    Compare(
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    Compare(
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.to_stream_changesummary_prop_list_char();
const
  LIST_PROP_NAME = s_list_char;
  LIST_PROP_TYPE = CharacterType;
var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB : ISDODataObject;
  locCS : ISDOChangeSummary;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
begin
  locFactoryA := TSDODataFactory.Create() as ISDODataFactory;
  locFactoryA.AddType(s_uri,s_DepartmentType,[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,LIST_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LIST_PROP_TYPE],[pfIsMany]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFactoryA.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setByte(s_number,123);
    ls := locDep.getList(LIST_PROP_NAME);
      ls.append(TSDOChar('k'));
      ls.append(TSDOChar('y'));
      ls.append(TSDOChar('g'));
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setCharacter(0,TSDOChar('j'));
    ls.append(TSDOChar('a'));
    ls.append(TSDOChar('x'));
    ls.setCharacter(1,TSDOChar('v'));
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, TSDOChar('A'));
    ls.append(TSDOChar('Z'));

  objA := locDep;
  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('changesummary_prop_list_char.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
    Compare(
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    Compare(
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.to_stream_changesummary_prop_list_date();
const
  LIST_PROP_NAME = s_list_date;
  LIST_PROP_TYPE = DateTimeType;
const VAL_1 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_2 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_3 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_4 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_5 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );

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

    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(2009,9,1,2,3,0,1);
    d.HourOffset := 0;
    d.MinuteOffset := 13;
    PSDODate(@VAL_4)^ := d;

    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(1900,11,8,1,2,0,0);
    d.HourOffset := 0;
    d.MinuteOffset := 13;
    PSDODate(@VAL_5)^ := d;
  end;

var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB : ISDODataObject;
  locCS : ISDOChangeSummary;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
begin
  locFactoryA := TSDODataFactory.Create() as ISDODataFactory;
  locFactoryA.AddType(s_uri,s_DepartmentType,[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_birthDate,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_list_date,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType],[pfIsMany]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFactoryA.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setDate(s_birthDate,VAL_1);
    ls := locDep.getList(s_list_date);
      ls.append(VAL_1);
      ls.append(VAL_2);
      ls.append(VAL_3);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setDate(0,VAL_4);
    ls.append(VAL_5);
    ls.append(VAL_1);
    ls.setDate(1,VAL_2);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, VAL_3);
    ls.append(VAL_4);

  objA := locDep;
  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('changesummary_prop_list_date.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
    Compare(
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    Compare(
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
  finally
    strm.Free();
  end;
end;

const
  CURRENCY_VALUES_REPEATED_DIGITED : array[0..8] of TSDOCurrency = (
    1111111111111.1111, -222222222222.2222, 33333333333333.3333, 444444444444444.4444, 555555555555555.5555,
    -666666666666666.6666, 777777777777777.7777, 8, -9
  );
procedure TSDOSerializerBinary_Test.to_stream_changesummary_prop_list_currency();
const
  LIST_PROP_NAME = s_list_currency;
  LIST_PROP_TYPE = CurrencyType;
var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB : ISDODataObject;
  locCS : ISDOChangeSummary;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
begin
  locFactoryA := TSDODataFactory.Create() as ISDODataFactory;
  locFactoryA.AddType(s_uri,s_DepartmentType,[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,LIST_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LIST_PROP_TYPE],[pfIsMany]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFactoryA.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setByte(s_number,123);
    ls := locDep.getList(LIST_PROP_NAME);
      ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[0]);
      ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[1]);
      ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[2]);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setCurrency(0,CURRENCY_VALUES_REPEATED_DIGITED[3]);
    ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[4]);
    ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[5]);
    ls.setCurrency(1,CURRENCY_VALUES_REPEATED_DIGITED[6]);
    ls.delete(0);
    ls.delete(1);
    ls.insertCurrency(2, CURRENCY_VALUES_REPEATED_DIGITED[7]);
    ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[8]);

  objA := locDep;
  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('changesummary_prop_list_currency.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
    Compare(
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    Compare(
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.to_stream_changesummary_prop_list_bytes();
const
  LIST_PROP_NAME = s_list_bytes;
  LIST_PROP_TYPE = BytesType;

var
  VAL_1, VAL_2, VAL_3, VAL_4, VAL_5 : TSDOBytes;

  procedure SetConstants();
  var
    v : TSDOBytes;
    k : Integer;
  begin
    SetLength(v,10);
    for k := 0 to High(v) do
      v[k] := k mod High(Byte);
    VAL_1 := v;
    v := nil;

    VAL_2 := nil;

    SetLength(v,20);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
    VAL_3 := v;
    v := nil;

    SetLength(v,30);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
    VAL_4 := v;
    v := nil;

    SetLength(v,40);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
    VAL_5 := v;
    v := nil;
  end;

var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB : ISDODataObject;
  locCS : ISDOChangeSummary;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
begin
  locFactoryA := TSDODataFactory.Create() as ISDODataFactory;
  locFactoryA.AddType(s_uri,s_DepartmentType,[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_birthDate,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,LIST_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LIST_PROP_TYPE],[pfIsMany]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFactoryA.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    //locDep.setBytes(s_birthDate,VAL_1);
    ls := locDep.getList(LIST_PROP_NAME);
      ls.appendBytes(VAL_1);
      ls.appendBytes(VAL_2);
      ls.appendBytes(VAL_3);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setBytes(0,VAL_4);
    ls.appendBytes(VAL_5);
    ls.appendBytes(VAL_1);
    ls.setBytes(1,VAL_2);
    ls.delete(0);
    ls.delete(1);
    ls.insertBytes(2, VAL_3);
    ls.appendBytes(VAL_4);

  objA := locDep;
  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('changesummary_prop_list_bytes.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
    Compare(
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    Compare(
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.to_stream_changesummary_prop_list_objects();

  function create_employee(
    const AFac : ISDODataFactory;
    const AName, ASN : TSDOString;
    const AManager : Boolean
  ) : ISDODataObject;
  begin
    Result := AFac.createNew(s_uri, s_EmployeeType);
    Result.setString(s_name, AName);
    Result.setString(s_sn, ASN);
    Result.setBoolean(s_manager, AManager);
  end;

var
  locFactoryA, locFactoryB : ISDODataFactory;
  locDep, e1, e2 : ISDODataObject;
  objA, objB : ISDODataObject;
  locCS : ISDOChangeSummary;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm , x: TMemoryStream;
  ls : ISDODataObjectList;
begin
  locFactoryA := TSDODataFactory.Create() as ISDODataFactory;
  locFactoryA.AddType(s_uri,s_EmployeeType,[]);
    locFactoryA.addProperty(s_uri, s_EmployeeType,'name',sdo_namespace,'string',[]);
    locFactoryA.addProperty(s_uri, s_EmployeeType,'SN',sdo_namespace,'string',[]);
    locFactoryA.addProperty(s_uri, s_EmployeeType,'manager',sdo_namespace,'boolean',[]);
  locFactoryA.AddType(s_uri,s_DepartmentType,[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_list_object,s_uri,s_EmployeeType,[pfIsMany,pfIsContainment]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);


  locDep := locFactoryA.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setInteger(s_number,1210);
    ls := locDep.getList(s_list_object);
      ls.append(create_employee(locFactoryA,'Inoussa O.', '0001', True));
      ls.append(create_employee(locFactoryA,'Kis O.', '0002', False));
      ls.append(create_employee(locFactoryA,'WST', '0003', False));
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    e1 := create_employee(locFactoryA,'FPC', '0010', False);
    e2 := create_employee(locFactoryA,'Lazarus', '0011', False);
    ls.setDataObject(0,e1);
    ls.append(create_employee(locFactoryA,'FPC 2 ', '0020', True));
    ls.insert(3,create_employee(locFactoryA,'FPC 5', '0050', False));
    ls.append(create_employee(locFactoryA,'FPC 3', '0030', False));
    ls.setDataObject(1,e2);
    ls.delete(0);
    ls.delete(1);
    ls.append(create_employee(locFactoryA,'FPC 4', '0040', True));

  objA := locDep;
  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('changesummary_prop_list_object.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);


        x := TMemoryStream.Create();
          s := TSDOSerializer.Create(locFactoryB,TSDOSerializerStreamXML.Create());
          s.save(objB,x);
          x.SaveToFile(sdoExpandLocalFileName('changesummary_prop_list_object.xxx.xml'));
        x.Free();

    Check(TSDOEqualityHelper.equal(objA,objB),'object');
    Compare(
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    Compare(
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.to_stream_changesummary_object_delete();
var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB : ISDODataObject;
  locCS : ISDOChangeSummary;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
  locDep, locEmployee : ISDODataObject;
begin
  locFactoryA := TSDODataFactory.Create() as ISDODataFactory;
  locFactoryA.AddType(s_uri,s_DepartmentType,[]);
  locFactoryA.AddType(s_uri,s_Employee,[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType], [pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_Employee,s_uri,s_Employee,[pfIsContainment]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

    locFactoryA.addProperty(s_uri,s_Employee,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_Employee,s_sn,sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_Employee,s_manager,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType], [pfIsAttribute]);

  locDep := locFactoryA.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locEmployee := locDep.createDataObject(s_Employee);
      locEmployee.setString(s_name,'Inoussa O.');
      locEmployee.setString(s_sn,'002');
      locEmployee.setBoolean(s_manager,True);
  locCS := locDep.getChangeSummary();
    locDep.setString(s_location,'Ouaga, BF');
  locCS.beginLogging();
    locDep.setInteger(s_number,1210);
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    locDep.setDataObject(s_Employee,nil);

  objA := locDep;
  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('changesummary_object_delete.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
    Compare(
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    Compare(
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.to_stream_changesummary_object_delete_nested;
var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB : ISDODataObject;
  locCS : ISDOChangeSummary;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
  locA, locB, locC, locD : ISDODataObject;
begin
  locFactoryA := TSDODataFactory.Create() as ISDODataFactory;
  locFactoryA.AddType(s_uri,'a',[]);
  locFactoryA.AddType(s_uri,'b',[]);
  locFactoryA.AddType(s_uri,'c',[]);
  locFactoryA.AddType(s_uri,'d',[]);
    locFactoryA.addProperty(s_uri,'a','p_a_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
    locFactoryA.addProperty(s_uri,'a','p_ab',s_uri,'b',[pfIsContainment]);
    locFactoryA.addProperty(s_uri,'a','p_ac',s_uri,'c',[]);
    locFactoryA.addProperty(s_uri,'a','p_ad',s_uri,'d',[]);
    locFactoryA.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);
      locFactoryA.addProperty(s_uri,'b','p_bc',s_uri,'c',[pfIsContainment]);
      locFactoryA.addProperty(s_uri,'b','p_b_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
        locFactoryA.addProperty(s_uri,'c','p_cd',s_uri,'d',[pfIsContainment]);
        locFactoryA.addProperty(s_uri,'c','p_c_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
          locFactoryA.addProperty(s_uri,'d','p_d_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);

  locA := locFactoryA.createNew(s_uri,'a');
    locA.setString('p_a_str','sample A'' property.');
    locB := locA.createDataObject('p_ab');
      locB.setString('p_b_str','Inoussa O.');
      locC := locB.createDataObject('p_bc');
      locC.setString('p_c_str','azerty');
      locD := locC.createDataObject('p_cd');
        locD.setString('p_d_str','D value');
    locA.setDataObject('p_ac',locC);
    locA.setDataObject('p_ad',locD);
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locC.setDataObject('p_cd',nil);
    locA.setDataObject('p_ab',nil);

  objA := locA;
  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('changesummary_object_delete_nested.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
    Compare(
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    Compare(
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.to_stream_changesummary_object_create_cont_ref();
var
  locFac : ISDODataFactory;
  locDep, locEmployee, locLoadedDep : ISDODataObject;
  locCS : ISDOChangeSummary;
  s : ISDOSerializer;
  localFileName : string;
begin
  localFileName := sdoExpandLocalFileName('change_summary_object_create_cont_ref.binary');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
  locFac.AddType(s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_DepartmentType,s_Employee,s_uri,s_Employee,[pfIsContainment]);
    locFac.addProperty(s_uri,s_DepartmentType,'employee_ref',s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

    locFac.addProperty(s_uri,s_Employee,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_Employee,s_sn,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_Employee,s_manager,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsAttribute]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
  locCS := locDep.getChangeSummary();
    locDep.setString(s_location,'Ouaga, BF');
  locCS.beginLogging();
    locDep.setInteger(s_number,1210);
    locEmployee := locDep.createDataObject(s_Employee);
      locEmployee.setString(s_name,'Inoussa O.');
      locEmployee.setString(s_sn,'002');
      locEmployee.setBoolean(s_manager,True);
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    locDep.setDataObject('employee_ref',locEmployee);
      locEmployee.setString(s_name,'Inoussa OUEDRAOGO');
      locEmployee.setString(s_sn,'001');
  s := TSDOSerializer.Create(locFac,CreateSerializerStream());
  s.Save(locDep,localFileName);

  s := TSDOSerializer.Create(locFac,CreateSerializerStream());
  locLoadedDep := s.load(localFileName);
    Check(TSDOEqualityHelper.equal(locLoadedDep,locDep),'Object');
    Compare(
      locDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      locLoadedDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    locDep.getChangeSummary().undoChanges();
    locLoadedDep.getChangeSummary().undoChanges();
      CheckEquals( 0, locDep.getChangeSummary().getChangedDataObjects().size());
      CheckEquals( 0, locLoadedDep.getChangeSummary().getChangedDataObjects().size());
      Check(TSDOEqualityHelper.equal(locDep, locLoadedDep),'Object');
end;

procedure TSDOSerializerBinary_Test.null_bool_prop();
const
  TARGET_PROP_NAME = s_bool_prop; TARGET_PROP_TYPE = BooleanType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[TARGET_PROP_TYPE],[]);
  end;

var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    s.save(locInstance,ms);

    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.null_byte_prop();
const
  TARGET_PROP_NAME = s_byte_prop; TARGET_PROP_TYPE = ByteType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[TARGET_PROP_TYPE],[]);
  end;

var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    s.save(locInstance,ms);

    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.null_bytes_prop();
const
  TARGET_PROP_NAME = s_bytes_prop; TARGET_PROP_TYPE = BytesType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[TARGET_PROP_TYPE],[]);
  end;

var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    s.save(locInstance,ms);

    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.null_char_prop();
const
  TARGET_PROP_NAME = s_char_prop; TARGET_PROP_TYPE = CharacterType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[TARGET_PROP_TYPE],[]);
  end;

var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    s.save(locInstance,ms);

    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.null_currency_prop();
const
  TARGET_PROP_NAME = s_currency_prop; TARGET_PROP_TYPE = CurrencyType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[TARGET_PROP_TYPE],[]);
  end;

var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    s.save(locInstance,ms);

    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.null_datetime_prop();
const
  TARGET_PROP_NAME = s_datetime_prop; TARGET_PROP_TYPE = DateTimeType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[TARGET_PROP_TYPE],[]);
  end;

var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    s.save(locInstance,ms);

    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.null_double_prop();
const
  TARGET_PROP_NAME = s_double_prop; TARGET_PROP_TYPE = DoubleType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[TARGET_PROP_TYPE],[]);
  end;

var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    s.save(locInstance,ms);

    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.null_float_prop();
const
  TARGET_PROP_NAME = s_float_prop; TARGET_PROP_TYPE = FloatType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[TARGET_PROP_TYPE],[]);
  end;

var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    s.save(locInstance,ms);

    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.null_integer_prop();
const
  TARGET_PROP_NAME = s_int_prop; TARGET_PROP_TYPE = IntegerType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_string_prop,sdo_namespace,'String',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[TARGET_PROP_TYPE],[]);
  end;

var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setString(s_string_prop,'azerty');

  ms := TMemoryStream.Create();
  try
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    s.save(locInstance,ms);

    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getString(s_string_prop), locInstanceLoaded.getString(s_string_prop),s_string_prop);
  finally
    ms.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.null_long_prop();
const
  TARGET_PROP_NAME = s_long_prop; TARGET_PROP_TYPE = LongType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[TARGET_PROP_TYPE],[]);
  end;

var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    s.save(locInstance,ms);

    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.null_object_prop();
const
  TARGET_PROP_NAME = s_object_prop; TARGET_PROP_TYPE_NAME = s_object_type2;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    Result.AddType(s_uri,s_object_type2,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,s_uri,TARGET_PROP_TYPE_NAME,[pfIsContainment]);
    locObj := Result.getType(s_uri,s_object_type2);
      Result.addProperty(locObj,s_string_prop,sdo_namespace,'String',[]);
  end;

var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    s.save(locInstance,ms);

    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.null_short_prop();
const
  TARGET_PROP_NAME = s_short_prop; TARGET_PROP_TYPE = ShortType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[TARGET_PROP_TYPE],[]);
  end;

var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    s.save(locInstance,ms);

    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.null_string_prop();
const
  TARGET_PROP_NAME = s_string_prop; TARGET_PROP_TYPE = stringType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[TARGET_PROP_TYPE],[]);
  end;

var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    s.save(locInstance,ms);

    s := TSDOSerializer.Create(locFactory,CreateSerializerStream());
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.changesummary_object_create();
var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB : ISDODataObject;
  locCS : ISDOChangeSummary;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
  locDep, locEmployee : ISDODataObject;
  ls : ISDODataObjectList;
begin
  locFactoryA := TSDODataFactory.Create() as ISDODataFactory;
  locFactoryA.AddType(s_uri,s_DepartmentType,[]);
  locFactoryA.AddType(s_uri,s_Employee,[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_Employee,s_uri,s_Employee,[pfIsContainment]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

    locFactoryA.addProperty(s_uri,s_Employee,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_Employee,s_sn,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_Employee,s_manager,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsAttribute]);

  locDep := locFactoryA.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
  locCS := locDep.getChangeSummary();
    locDep.setString(s_location,'Ouaga, BF');
  locCS.beginLogging();
    locDep.setInteger(s_number,1210);
    locEmployee := locDep.createDataObject(s_Employee);
      locEmployee.setString(s_name,'Inoussa O.');
      locEmployee.setString(s_sn,'002');
      locEmployee.setBoolean(s_manager,True);
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
      locEmployee.setString(s_name,'Inoussa OUEDRAOGO');
      locEmployee.setString(s_sn,'001');

  objA := locDep;
  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('changesummary_object_create.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
    Compare(
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    Compare(
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.changesummary_object_create_cont_ref();
var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB : ISDODataObject;
  locCS : ISDOChangeSummary;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
  locDep, locEmployee : ISDODataObject;
  ls : ISDODataObjectList;
begin
  locFactoryA := TSDODataFactory.Create() as ISDODataFactory;
  locFactoryA.AddType(s_uri,s_DepartmentType,[]);
  locFactoryA.AddType(s_uri,s_Employee,[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_Employee,s_uri,s_Employee,[pfIsContainment]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,'employee_ref',s_uri,s_Employee,[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

    locFactoryA.addProperty(s_uri,s_Employee,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_Employee,s_sn,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_Employee,s_manager,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsAttribute]);

  locDep := locFactoryA.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
  locCS := locDep.getChangeSummary();
    locDep.setString(s_location,'Ouaga, BF');
  locCS.beginLogging();
    locDep.setInteger(s_number,1210);
    locEmployee := locDep.createDataObject(s_Employee);
      locEmployee.setString(s_name,'Inoussa O.');
      locEmployee.setString(s_sn,'002');
      locEmployee.setBoolean(s_manager,True);
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    locDep.setDataObject('employee_ref',locEmployee);
      locEmployee.setString(s_name,'Inoussa OUEDRAOGO');
      locEmployee.setString(s_sn,'001');

  objA := locDep;
  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('changesummary_object_create_cont_ref.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
    Compare(
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    Compare(
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.changesummary_object_delete();
var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB : ISDODataObject;
  locCS : ISDOChangeSummary;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
  locDep, locEmployee : ISDODataObject;
  ls : ISDODataObjectList;
begin
  locFactoryA := TSDODataFactory.Create() as ISDODataFactory;
  locFactoryA.AddType(s_uri,s_DepartmentType,[]);
  locFactoryA.AddType(s_uri,s_Employee,[]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType], [pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_Employee,s_uri,s_Employee,[pfIsContainment]);
    locFactoryA.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

    locFactoryA.addProperty(s_uri,s_Employee,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_Employee,s_sn,sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
    locFactoryA.addProperty(s_uri,s_Employee,s_manager,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType], [pfIsAttribute]);

  locDep := locFactoryA.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locEmployee := locDep.createDataObject(s_Employee);
      locEmployee.setString(s_name,'Inoussa O.');
      locEmployee.setString(s_sn,'002');
      locEmployee.setBoolean(s_manager,True);
  locCS := locDep.getChangeSummary();
    locDep.setString(s_location,'Ouaga, BF');
  locCS.beginLogging();
    locDep.setInteger(s_number,1210);
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    locDep.setDataObject(s_Employee,nil);

  objA := locDep;
  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('changesummary_object_delete.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
    Compare(
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    Compare(
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.changesummary_object_delete_nested();
var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB : ISDODataObject;
  locCS : ISDOChangeSummary;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
  locA, locB, locC, locD : ISDODataObject;
  ls : ISDODataObjectList;
begin
  locFactoryA := TSDODataFactory.Create() as ISDODataFactory;
  locFactoryA.AddType(s_uri,'a',[]);
  locFactoryA.AddType(s_uri,'b',[]);
  locFactoryA.AddType(s_uri,'c',[]);
  locFactoryA.AddType(s_uri,'d',[]);
    locFactoryA.addProperty(s_uri,'a','p_a_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
    locFactoryA.addProperty(s_uri,'a','p_ab',s_uri,'b',[pfIsContainment]);
    locFactoryA.addProperty(s_uri,'a','p_ac',s_uri,'c',[]);
    locFactoryA.addProperty(s_uri,'a','p_ad',s_uri,'d',[]);
    locFactoryA.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);
      locFactoryA.addProperty(s_uri,'b','p_bc',s_uri,'c',[pfIsContainment]);
      locFactoryA.addProperty(s_uri,'b','p_b_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
        locFactoryA.addProperty(s_uri,'c','p_cd',s_uri,'d',[pfIsContainment]);
        locFactoryA.addProperty(s_uri,'c','p_c_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
          locFactoryA.addProperty(s_uri,'d','p_d_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);

  locA := locFactoryA.createNew(s_uri,'a');
    locA.setString('p_a_str','sample A'' property.');
    locB := locA.createDataObject('p_ab');
      locB.setString('p_b_str','Inoussa O.');
      locC := locB.createDataObject('p_bc');
      locC.setString('p_c_str','azerty');
      locD := locC.createDataObject('p_cd');
        locD.setString('p_d_str','D value');
    locA.setDataObject('p_ac',locC);
    locA.setDataObject('p_ad',locD);
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locC.setDataObject('p_cd',nil);
    locA.setDataObject('p_ab',nil);

  objA := locA;
  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('changesummary_object_delete_nested.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
    Compare(
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    Compare(
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializerBinary_Test.changesummary_object_delete_2_objects_same_type();
var
  locFactoryA, locFactoryB : ISDODataFactory;
  objA, objB : ISDODataObject;
  locCS : ISDOChangeSummary;
  objList : ISDODataObjectList;
  s : ISDOSerializer;
  strm : TMemoryStream;
  locA, locB : ISDODataObject;
  ls : ISDODataObjectList;
begin
  locFactoryA := TSDODataFactory.Create() as ISDODataFactory;
  locFactoryA.AddType(s_uri,'a',[]);
  locFactoryA.AddType(s_uri,'b',[]);
    locFactoryA.addProperty(s_uri,'a','p_a_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
    locFactoryA.addProperty(s_uri,'a','p_ab1',s_uri,'b',[pfIsContainment]);
    locFactoryA.addProperty(s_uri,'a','p_ab2',s_uri,'b',[pfIsContainment]);
    locFactoryA.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);
      locFactoryA.addProperty(s_uri,'b','p_b_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);

  locA := locFactoryA.createNew(s_uri,'a');
    locA.setString('p_a_str','sample A'' property.');
    locB := locA.createDataObject('p_ab1');
      locB.setString('p_b_str','p_ab1\p_b_str value');
    locB := locA.createDataObject('p_ab2');
      locB.setString('p_b_str','p_ab2\p_b_str value');

  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.setDataObject('p_ab1',nil);
    locA.setDataObject('p_ab2',nil);

  objA := locA;
  s := TSDOSerializer.Create(locFactoryA,CreateSerializerStream());
  strm := TMemoryStream.Create();
  try
    s.save(objA,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('changesummary_object_delete_2_objects_same_type.binary'));
{$ENDIF TEST_GENERATE_FILE}

    locFactoryB := TSDODataFactory.Create();
    s := TSDOSerializer.Create(locFactoryB,CreateSerializerStream());
    strm.Position := 0;
    objList := TSDODataObjectList.Create(
                 locFactoryB.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])
               ) as ISDODataObjectList;
    s.load(strm,objList);
    CheckEquals(1,objList.size(),'objCount');
    objB := objList.getDataObject(0);
    Check(TSDOEqualityHelper.equal(objA,objB),'object');
    Compare(
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    Compare(
      objB.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      objA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
  finally
    strm.Free();
  end;
end;

{ TSDOBaseSerializer_Test }

procedure TSDOBaseSerializer_Test.Compare(
  const A, B: ISDOChangedDataObjectListEx;
  const AOptions : TListCompareOptions
);
var
  i, c, j, k : PtrInt;
  tmpObj : ISDODataObject;
begin
  Check(
    ( ( A = nil ) and ( B = nil ) ) or
    ( ( A <> nil ) and ( B <> nil ) ) ,
    'nil'
  );
  if ( A <> nil ) then begin
    CheckEquals(A.size(),B.size(), 'size()');
    c := A.size();
    if ( c > 0 ) then begin
      if ( lcoCompareOrder in AOptions ) then begin
        for i := 0 to Pred(c) do begin
          CheckEquals(Ord(A.getType(i)), Ord(B.getType(i)), Format('getType(%d)',[i]));
          Check(TSDOEqualityHelper.equal(A.getDataObject(i),B.getDataObject(i)),Format('A.getDataObject(%d)',[i]));
          Compare(A.getInfo(i),B.getInfo(i));
        end;
      end else begin
        for i := 0 to Pred(c) do begin
          tmpObj := A.getDataObject(i);
          k := -1;
          for j := 0 to Pred(B.size()) do begin
            if TSDOEqualityHelper.equal(tmpObj,B.getDataObject(j)) then begin
              k := j;
              Break;
            end;
          end;
          Check( ( k > -1 ), Format('Object not found  : A.getDataObject(%d)',[i]));
          CheckEquals(Ord(A.getType(i)), Ord(B.getType(k)), Format('getType(%d)',[i]));
          Check(TSDOEqualityHelper.equal(A.getDataObject(i),B.getDataObject(k)),Format('A.getDataObject(%d), B.getDataObject(%d)',[i,k]));
          Compare(A.getInfo(i),B.getInfo(k));
        end;
      end;
    end;
  end;
end;

procedure TSDOBaseSerializer_Test.Compare(const A, B: TDataObjectChangeInfo);
var
  i, c, j : PtrInt;
  ok : Boolean;
  x, y : TValueSetting;
begin
  Check( ( A = nil ) or ( B <> nil ), 'nil');
  if ( A <> nil ) then begin
    A.ExtractPendingOldValues();
    B.ExtractPendingOldValues();
    Check(TSDOEqualityHelper.equal(A.DataObject,B.DataObject));
    CheckEquals(Ord(A.ChangeType), Ord(B.ChangeType), 'ChangeType');
    Check(TSDOEqualityHelper.equal(A.OldContainer,B.OldContainer));
    CheckEquals(A.ChangeList.size(),B.ChangeList.size(),'ChangeList.size(), Type='+GetEnumName(TypeInfo(TChangeType),Ord(A.ChangeType)));
    c := A.ChangeList.size();
    if ( c > 0 ) then begin
      y := nil;
      for i := 0 to Pred(c) do begin
        x := A.ChangeList.getItem(i);
        ok := False;
        for j := 0 to ( c - 1 ) do begin
          y := B.ChangeList.getItem(j);
          if ( x.getProperty().getName() = y.getProperty().getName() ) and
             x.getProperty().getType().equals(y.getProperty().getType()) and
             ( x.getIndex() = y.getIndex() )
          then begin
            ok := True;
            Break;
          end;
        end;
        Check(ok, Format('A.ChangeList.getItem(%d) %s, not found.',[i, x.getProperty().getName()]));
        Compare(x,y);
      end;
    end;
    Check(
      ( ( A.ManyValuePropChangesList = nil ) and ( B.ManyValuePropChangesList = nil ) ) or
      ( ( A.ManyValuePropChangesList <> nil ) and ( B.ManyValuePropChangesList <> nil ) ),
      'ManyValuePropChangesList'
    );
    if ( A.ManyValuePropChangesList <> nil ) then begin
      Compare(A.ManyValuePropChangesList, B.ManyValuePropChangesList);
    end;
  end;
end;

procedure TSDOBaseSerializer_Test.Compare(
  const A, B: TValueBuffer; const ADataType: TSDOTypeKind
);
begin
  case ADataType of
    BooleanType   : CheckEquals(A.BooleanValue,B.BooleanValue,'BooleanValue');
    ByteType      : CheckEquals(A.ByteValue,B.ByteValue,'ByteValue');
{$IFDEF HAS_SDO_BYTES}
    BytesType     : CheckEquals(A.BytesValue^,B.BytesValue^,'BytesValue');
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    CharacterType : CheckEquals(A.CharValue,B.CharValue,'CharValue');
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    CurrencyType  : CheckEquals(A.CurrencyValue,B.CurrencyValue,'CurrencyValue');
{$ENDIF HAS_SDO_CURRENCY}
    DateTimeType  : CheckEquals(A.DateValue,B.DateValue,'DateValue',False);
{$IFDEF HAS_SDO_DOUBLE}
    DoubleType    : CheckEquals(A.DoubleValue,B.DoubleValue,'DoubleValue');
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    FloatType     : CheckEquals(A.FloatValue,B.FloatValue,'FloatValue');
{$ENDIF HAS_SDO_FLOAT}
    IntegerType   : CheckEquals(A.IntegerValue,B.IntegerValue,'IntegerValue');
{$IFDEF HAS_SDO_LONG}
    LongType      : CheckEquals(A.LongValue,B.LongValue,'LongValue');
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    ShortType     : CheckEquals(A.ShortValue,B.ShortValue,'ShortValue');
{$ENDIF HAS_SDO_SHORT}
    ObjectType    : Check(TSDOEqualityHelper.equal(A.ObjectValue^,B.ObjectValue^),'ObjectValue');
    StringType    : CheckEquals(A.StringValue^,B.StringValue^,'StringValue');
    else
      Assert(False);
  end;
end;

procedure TSDOBaseSerializer_Test.Compare(const A, B: TValueSetting);
begin
  Check( ( A = nil ) or ( B <> nil ), 'nil');
  if ( A <> nil ) then begin
    CheckEquals(A.getProperty().getName(), B.getProperty().getName(), 'getProperty().getName()');
    Check(A.getProperty().getType().equals(B.getProperty().getType()), 'getProperty().getType()');
    CheckEquals(A.isSet, B.isSet, Format('%s.isSet',[A.getProperty().getName()]));
    CheckEquals(A.isNull, B.isNull, 'isNull');
    case A.getProperty.getTypeEnum() of
      BooleanType   : CheckEquals(A.getBooleanValue(),B.getBooleanValue(),'getBooleanValue()');
      ByteType      : CheckEquals(A.getByteValue(),B.getByteValue(),'getByteValue()');
{$IFDEF HAS_SDO_BYTES}
      BytesType     : CheckEquals(A.getBytesValue(),B.getBytesValue(),'getBytesValue()');
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
      CharacterType : CheckEquals(A.getCharacterValue(),B.getCharacterValue(),'getCharacterValue()');
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
      CurrencyType  : CheckEquals(A.getCurrencyValue(),B.getCurrencyValue(),'getCurrencyValue()');
{$ENDIF HAS_SDO_CURRENCY}
      DateType      : CheckEquals(A.getDateValue(),B.getDateValue(),'getDateValue()',False);
{$IFDEF HAS_SDO_DOUBLE}
      DoubleType    : CheckEquals(A.getDoubleValue(),B.getDoubleValue(),'getDoubleValue()');
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
      FloatType     : CheckEquals(A.getFloatValue(),B.getFloatValue(),'getFloatValue()');
{$ENDIF HAS_SDO_FLOAT}
      IntegerType   : CheckEquals(A.getIntegerValue(),B.getIntegerValue(),'getIntegerValue()');
{$IFDEF HAS_SDO_LONG}
      LongType      : CheckEquals(A.getLongValue(),B.getLongValue(),'getLongValue()');
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      ShortType     : CheckEquals(A.getShortValue(),B.getShortValue(),'getShortValue()');
{$ENDIF HAS_SDO_SHORT}
      ObjectType    : Check(TSDOEqualityHelper.equal(A.getDataObjectValue(),B.getDataObjectValue()),'getDataObjectValue()');
      StringType    : CheckEquals(A.getStringValue(),B.getStringValue(),'getStringValue()');
      else
        Assert(False);
    end;
  end;
end;

procedure TSDOBaseSerializer_Test.Compare(const A, B: TManyValuePropChangesList);
var
  i, c, q, k : PtrInt;
  x, y : TManyValuePropChanges;
  xd, yd : TManyValuePropRecordData;
begin
  Check(
    ( ( A = nil ) and ( B = nil ) ) or
    ( ( A <> nil ) and ( B <> nil ) ) ,
    'nil'
  );
  if ( A <> nil ) then begin
    CheckEquals(A.Count(),B.Count(), 'Count()');
    c := A.Count();
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        x := A.Item[i];
        y := B.Item[i];
        CheckEquals(x.Prop.getName(), y.Prop.getName(), Format('item[%d] : Property ( name )',[i]));
        Check(x.Prop.getType().equals(y.Prop.getType()), Format('item[%d] : Property ( type )',[i]));
        CheckEquals(x.Count, y.Count, Format('item[%d] : Count',[i]));
        q := x.Count;
        if ( q > 0 ) then begin
          for k := 0 to Pred(q) do begin
            xd := x.GetItem(i);
            yd := y.GetItem(i);
            CheckEquals(Ord(xd.Action), Ord(yd.Action), Format('item[%d][%d].Action : Count',[i,k]));
            CheckEquals(xd.Index, yd.Index, Format('item[%d][%d].Index : Count',[i,k]));
            Compare(xd.Value, yd.Value, x.Prop.getTypeEnum());
          end;
        end;
      end;
    end;
  end;
end;

procedure TSDOBaseSerializer_Test.CheckEquals(expected, actual: TSDODate;
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

function TSDOBaseSerializer_Test.CreateSdoTypes() : ISDODataFactory;
var
  locObj : ISDOType;
begin
  Result := TSDODataFactory.Create() as ISDODataFactory;
  Result.AddType(s_uri,s_EmployeeType,[]);
  locObj := Result.getType(s_uri,s_EmployeeType);
    Result.addProperty(locObj,'name',sdo_namespace,'string',[pfIsAttribute]);
    Result.addProperty(locObj,'SN',sdo_namespace,'string',[pfIsAttribute]);
    Result.addProperty(locObj,'manager',sdo_namespace,'boolean',[pfIsAttribute]);
    Result.addProperty(locObj,'age',sdo_namespace,'byte',[pfIsAttribute]);

  Result.AddType(s_uri,s_DepartmentType,[]);
  locObj := Result.getType(s_uri,s_DepartmentType);
    Result.addProperty(locObj,'employees',s_uri,s_EmployeeType,[pfIsMany, pfIsContainment]);
    Result.addProperty(locObj,'name',sdo_namespace,'string',[pfIsAttribute]);
    Result.addProperty(locObj,'location',sdo_namespace,'string',[pfIsAttribute]);
    Result.addProperty(locObj,'number',sdo_namespace,'integer',[pfIsAttribute]);

  Result.AddType(s_uri,s_CompanyType,[]);
  locObj := Result.getType(s_uri,s_CompanyType);
    Result.setAlias(locObj.getURI(),locObj.getName(),s_company);
    Result.addProperty(locObj,'departments',s_uri,s_DepartmentType,[pfIsMany, pfIsContainment]);
    Result.addProperty(locObj,'name',sdo_namespace,'string',[pfIsAttribute]);
    Result.addProperty(locObj,'employeeOfTheMonth',sdo_namespace,'string',[pfIsAttribute]);
    Result.setAlias(s_uri,locObj.getName(),s_company);
end;

function TSDOBaseSerializer_Test.CreateCompanyObject(
  const AFactory : ISDODataFactory
) : ISDODataObject;
var
  locFactory : ISDODataFactory;
  compObj, depObj, empObj : ISDODataObject;
  depLs, empLs : ISDODataObjectList;
begin
  locFactory := AFactory;
  compObj := locFactory.createNew(s_uri,s_company);

  compObj.setString('name','A Sample company');
  compObj.setString('employeeOfTheMonth','Inoussa');

  depLs := compObj.getList('departments');
  depObj := compObj.createDataObject('departments');
  depLs.append(depObj);
  depObj.setString('name','RAD Departement');
  depObj.setString('location','Moon');
  depObj.setInteger('number',2);
    empLs := depObj.getList('employees');
    empObj := depObj.createDataObject('employees');
      empObj.setString('name','inoussa OUEDRAOGO');
      empObj.setString('SN','1122334455667');
      empObj.setBoolean('manager',True);
      empObj.setByte('age',32);
    empLs.append(empObj);

    empObj := depObj.createDataObject('employees');
      empObj.setString('name','SDO man');
      empObj.setString('SN','867787667');
      empObj.setBoolean('manager',False);
      empObj.setByte('age',1);
    empLs.append(empObj);

    empObj := depObj.createDataObject('employees');
      empObj.setString('name','FPC');
      empObj.setString('SN','_e-(''');
      empObj.setBoolean('manager',False);
      empObj.setByte('age',13);
    empLs.append(empObj);


  depObj := compObj.createDataObject('departments');
  depLs.append(depObj);
  depObj.setString('name','Sales Departement');
  depObj.setString('location','Mars');
  depObj.setInteger('number',2);
    empLs := depObj.getList('employees');
    empObj := depObj.createDataObject('employees');
      empObj.setString('name','wst man');
      empObj.setString('SN','e"''fsdfdf');
      empObj.setBoolean('manager',True);
      empObj.setByte('age',2);
    empLs.append(empObj);

    empObj := depObj.createDataObject('employees');
      empObj.setString('name','azerty');
      empObj.setString('SN','jkjk_e5679');
      empObj.setBoolean('manager',False);
    empLs.append(empObj);

    empObj := depObj.createDataObject('employees');
      empObj.setString('name','qwerty');
      empObj.setString('SN','_s-(''');
      empObj.setBoolean('manager',False);
    empLs.append(empObj);

  Result := compObj;
end;

{ TSDOSerializer_Test }

procedure TSDOSerializer_Test.CompareTypesInclude(const A, B: ISDOTypeList);
var
  i, c : PtrInt;
  typA, typB : ISDOType;
begin
  if Assigned(A) then begin
    Check(Assigned(B),'B = nil');
    c := A.getCount();
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        typA := A.getItem(i);
        typB := B.find(typA.getURI(),typA.getName());
        Check(typB.equals(typA),'A.getItem(i) <> B.find(typA.getURI(),typA.getName())');
      end;
    end;
  end;
end;

procedure TSDOSerializer_Test.load_from_stream_one_object();
var
  locFactory, tmpFactory : ISDODataFactory;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  strm : TMemoryStream;
  ol : ISDODataObjectList;
  locComp : ISDODataObject;
begin
  strm := TMemoryStream.Create();
  try
    strm.LoadFromFile(sdoExpandLocalFileName(TestFilesPath + 'company.one.xml'));
    strm.Position := 0;
    f := TSDOSerializerStreamXML.Create();
    tmpFactory := TSDODataFactory.Create();
    s := TSDOSerializer.Create(tmpFactory,f);
    ol := TSDODataObjectList.Create(tmpFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType]));
    s.load(strm,ol);

    locFactory := CreateSdoTypes();
    CompareTypesInclude(tmpFactory.getTypes(), locFactory.getTypes());
    CompareTypesInclude(locFactory.getTypes(), tmpFactory.getTypes());
    CheckEquals(1,ol.size(),'loaded objects count');
    locComp := CreateCompanyObject(locFactory);
    ol.getCursor().MoveFirst();
    CheckEquals(True,TSDOEqualityHelper.equalShallow(locComp, ol.getDataObject()),'equalShallow');
    CheckEquals(True,TSDOEqualityHelper.equal(locComp, ol.getDataObject()),'equal');
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializer_Test.load_from_stream_start_with_empty();
var
  locFactory, tmpFactory : ISDODataFactory;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  strm : TMemoryStream;
  ol : ISDODataObjectList;
begin
  strm := TMemoryStream.Create();
  try
    strm.LoadFromFile(sdoExpandLocalFileName(TestFilesPath + 'company.xml'));
    strm.Position := 0;
    f := TSDOSerializerStreamXML.Create();
    tmpFactory := TSDODataFactory.Create();
    s := TSDOSerializer.Create(tmpFactory,f);
    ol := TSDODataObjectList.Create(tmpFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType]));
    s.load(strm,ol);
    locFactory := CreateSdoTypes();
    CompareTypesInclude(tmpFactory.getTypes(), locFactory.getTypes());
    CompareTypesInclude(locFactory.getTypes(), tmpFactory.getTypes());
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializer_Test.load_from_stream_two_object();
var
  locFactory, tmpFactory : ISDODataFactory;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  strm : TMemoryStream;
  ol : ISDODataObjectList;
  locComp : ISDODataObject;
begin
  strm := TMemoryStream.Create();
  try
    strm.LoadFromFile(sdoExpandLocalFileName(TestFilesPath + 'company.two.xml'));
    strm.Position := 0;
    f := TSDOSerializerStreamXML.Create();
    tmpFactory := TSDODataFactory.Create();
    s := TSDOSerializer.Create(tmpFactory,f);
    ol := TSDODataObjectList.Create(tmpFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType]));
    s.load(strm,ol);

    locFactory := CreateSdoTypes();
    CompareTypesInclude(tmpFactory.getTypes(), locFactory.getTypes());
    CompareTypesInclude(locFactory.getTypes(), tmpFactory.getTypes());
    CheckEquals(2,ol.size(),'loaded objects count');
    locComp := CreateCompanyObject(locFactory);
    locComp.setString('name','A Second Sample company');
    ol.getCursor().MoveFirst();
    CheckEquals(True,TSDOEqualityHelper.equalShallow(locComp, ol.getDataObject()),'equalShallow');
    CheckEquals(True,TSDOEqualityHelper.equal(locComp, ol.getDataObject()),'equal');
  finally
    strm.Free();
  end;
end;

procedure TSDOSerializer_Test.save_to_stream();
var
  locFactory : ISDODataFactory;
  compObj : ISDODataObject;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  strm : TMemoryStream;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  locFactory := CreateSdoTypes();
  compObj := CreateCompanyObject(locFactory);

  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFactory,f);
  strm := TMemoryStream.Create();
  try
    s.save(s_CompanyType,compObj,strm);
{$IFDEF TEST_GENERATE_FILE}
    strm.SaveToFile(sdoExpandLocalFileName('company.soap.xml'));
{$ENDIF TEST_GENERATE_FILE}    

    strm.Position := 0;
    existDoc := nil;
    serialDoc := nil;
    ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + 'company.one.xml'));
    try
      ReadXMLFile(serialDoc,strm);
      CheckEquals(True, CompareNodes(existDoc,serialDoc));
    finally
      ReleaseDomNode(existDoc);
      ReleaseDomNode(serialDoc);
    end;
  finally
    strm.Free();
  end;
end;

{procedure TSDOSerializer_Test.save2();
var
  locFactory : ISDODataFactory;
  compObj, depObj, empObj : ISDODataObject;
  depLs, empLs : ISDODataObjectList;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  strm : TMemoryStream;
  i : Integer;
  dd, ff : TDateTime;
begin
  dd := Now();
  locFactory := CreateSdoTypes();
  compObj := locFactory.createNew(s_uri,s_company);

  compObj.setString('name','A Sample company');
  compObj.setString('employeeOfTheMonth','Inoussa');

  depLs := compObj.getList('departments');
  depObj := compObj.createDataObject('departments');
  depLs.append(depObj);
  depObj.setString('name','RAD Departement');
  depObj.setString('location','Moon');
  depObj.setInteger('number',2);
    empLs := depObj.getList('employees');
    empObj := depObj.createDataObject('employees');
      empObj.setString('name','inoussa OUEDRAOGO');
      empObj.setString('SN','1122334455667');
      empObj.setBoolean('manager',True);
    empLs.append(empObj);

    empObj := depObj.createDataObject('employees');
      empObj.setString('name','SDO man');
      empObj.setString('SN','867787667');
      empObj.setBoolean('manager',False);
    empLs.append(empObj);

    empObj := depObj.createDataObject('employees');
      empObj.setString('name','FPC');
      empObj.setString('SN','_e-(''');
      empObj.setBoolean('manager',False);
    empLs.append(empObj);


  depObj := compObj.createDataObject('departments');
  depLs.append(depObj);
  depObj.setString('name','Sales Departement');
  depObj.setString('location','Mars');
  depObj.setInteger('number',2);
    empLs := depObj.getList('employees');
    empObj := depObj.createDataObject('employees');
      empObj.setString('name','wst man');
      empObj.setString('SN','e"''fsdfdf');
      empObj.setBoolean('manager',True);
    empLs.append(empObj);

    empObj := depObj.createDataObject('employees');
      empObj.setString('name','azerty');
      empObj.setString('SN','jkjk_e5679');
      empObj.setBoolean('manager',False);
    empLs.append(empObj);

    for i := 0 to 10000 do begin
      empObj := depObj.createDataObject('employees');
        empObj.setString('name','Personne N°' + IntToStr(i));
        empObj.setString('SN','#'+ IntToStr(i));
        empObj.setBoolean('manager',( i mod 3 ) = 0 );
      empLs.append(empObj);
    end;

  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFactory,f);
  strm := TMemoryStream.Create();
  s.save('compagnie',compObj,strm);

  strm.Position := 0;
  f := TSDOSerializerStreamXML.Create();
  f.LoadFromStream(strm);
  ff := Now();
  //ShowMessageFmt('%s'#13'%s'#13'Size=%d',[DateTimeToStr(dd),DateTimeToStr(ff),strm.Size]);
  strm.SaveToFile('.' + PathDelim + 'company2.soap.xml');
end; }

procedure TSDOSerializer_Test.save_to_stream_without_name();
var
  locFactory : ISDODataFactory;
  compObj : ISDODataObject;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  strm : TMemoryStream;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  locFactory := CreateSdoTypes();
  compObj := CreateCompanyObject(locFactory);

  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFactory,f);
  strm := TMemoryStream.Create();
  try
    s.save(compObj,strm);

    strm.Position := 0;
    existDoc := nil;
    serialDoc := nil;
    ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + 'company.one.xml'));
    try
      ReadXMLFile(serialDoc,strm);
      CheckEquals(True, CompareNodes(existDoc,serialDoc));
    finally
      ReleaseDomNode(existDoc);
      ReleaseDomNode(serialDoc);       
    end;
  finally
    FreeAndNil(strm);
  end;
end;

procedure TSDOSerializer_Test.save_to_file();
var
  locFactory : ISDODataFactory;
  compObj : ISDODataObject;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  existDoc, serialDoc : TSDOXMLDocument;
  localFileName : string;
begin
  localFileName := sdoExpandLocalFileName('save_to_file.xml');
  locFactory := CreateSdoTypes();
  compObj := CreateCompanyObject(locFactory);

  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFactory,f);
  s.save(s_CompanyType,compObj,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + 'company.one.xml'));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;

procedure TSDOSerializer_Test.save_to_file_without_name();
var
  locFactory : ISDODataFactory;
  compObj : ISDODataObject;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  existDoc, serialDoc : TSDOXMLDocument;
  localFileName : string;
begin
  localFileName := sdoExpandLocalFileName('save_to_file.xml');
  locFactory := CreateSdoTypes();
  compObj := CreateCompanyObject(locFactory);

  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFactory,f);
  s.save(compObj,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + 'company.one.xml'));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;

procedure TSDOSerializer_Test.save_to_file_null_bool_prop();
const
  TARGET_PROP_NAME = s_bool_prop; TARGET_PROP_TYPE_NAME = 'Boolean';
  
  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,TARGET_PROP_TYPE_NAME,[]);
  end;
  
var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    s.save(locInstance,ms);  
    
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end;              
end;

procedure TSDOSerializer_Test.save_to_file_null_byte_prop(); 
const
  TARGET_PROP_NAME = s_byte_prop; TARGET_PROP_TYPE_NAME = 'Byte';
  
  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,TARGET_PROP_TYPE_NAME,[]);
  end;
  
var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    s.save(locInstance,ms);  
    
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end;       
end;

procedure TSDOSerializer_Test.save_to_file_null_bytes_prop(); 
const
  TARGET_PROP_NAME = s_bytes_prop; TARGET_PROP_TYPE_NAME = 'Bytes';
  
  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,TARGET_PROP_TYPE_NAME,[]);
  end;
  
var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    s.save(locInstance,ms);  
    
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end;   
end;

procedure TSDOSerializer_Test.save_to_file_null_char_prop(); 
const
  TARGET_PROP_NAME = s_char_prop; TARGET_PROP_TYPE_NAME = 'Character';
  
  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,TARGET_PROP_TYPE_NAME,[]);
  end;
  
var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    s.save(locInstance,ms);  
    
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end;          
end;

procedure TSDOSerializer_Test.save_to_file_null_currency_prop();  
const
  TARGET_PROP_NAME = s_currency_prop; TARGET_PROP_TYPE_NAME = 'Currency';
  
  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,TARGET_PROP_TYPE_NAME,[]);
  end;
  
var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    s.save(locInstance,ms);  
    
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end; 
end;

procedure TSDOSerializer_Test.save_to_file_null_datetime_prop();   
const
  TARGET_PROP_NAME = s_datetime_prop; TARGET_PROP_TYPE_NAME = 'DateTime';
  
  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,TARGET_PROP_TYPE_NAME,[]);
  end;
  
var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    s.save(locInstance,ms);  
    
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end;         
end;

procedure TSDOSerializer_Test.save_to_file_null_double_prop();
const
  TARGET_PROP_NAME = s_double_prop; TARGET_PROP_TYPE_NAME = 'Double';
  
  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,TARGET_PROP_TYPE_NAME,[]);
  end;
  
var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    s.save(locInstance,ms);  
    
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end;      
end;

procedure TSDOSerializer_Test.save_to_file_null_float_prop();
const
  TARGET_PROP_NAME = s_float_prop; TARGET_PROP_TYPE_NAME = 'Float';
  
  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,TARGET_PROP_TYPE_NAME,[]);
  end;
  
var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    s.save(locInstance,ms);  
    
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end;      
end;

procedure TSDOSerializer_Test.save_to_file_null_int_prop();  
const
  TARGET_PROP_NAME = s_int_prop; TARGET_PROP_TYPE_NAME = 'Integer';
  
  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_string_prop,sdo_namespace,'String',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,TARGET_PROP_TYPE_NAME,[]);
  end;
  
var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setString(s_string_prop,'azerty');

  ms := TMemoryStream.Create();
  try
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    s.save(locInstance,ms);  
    
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getString(s_string_prop), locInstanceLoaded.getString(s_string_prop),s_string_prop);
  finally
    ms.Free();
  end;   
end;

procedure TSDOSerializer_Test.save_to_file_null_long_prop(); 
const
  TARGET_PROP_NAME = s_long_prop; TARGET_PROP_TYPE_NAME = 'Long';
  
  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,TARGET_PROP_TYPE_NAME,[]);
  end;
  
var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    s.save(locInstance,ms);  
    
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end;  
end;

procedure TSDOSerializer_Test.save_to_file_null_object_prop(); 
const
  TARGET_PROP_NAME = s_object_prop; TARGET_PROP_TYPE_NAME = s_object_type2;
  
  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    Result.AddType(s_uri,s_object_type2,[]);           
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,s_uri,TARGET_PROP_TYPE_NAME,[pfIsContainment]);
    locObj := Result.getType(s_uri,s_object_type2);
      Result.addProperty(locObj,s_string_prop,sdo_namespace,'String',[]);
  end;
  
var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    s.save(locInstance,ms);  
    
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end;  
end;

procedure TSDOSerializer_Test.save_to_file_null_short_prop();
const
  TARGET_PROP_NAME = s_short_prop; TARGET_PROP_TYPE_NAME = 'Short';
  
  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,TARGET_PROP_TYPE_NAME,[]);
  end;
  
var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    s.save(locInstance,ms);  
    
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end;  
end;

procedure TSDOSerializer_Test.save_to_file_null_string_prop(); 
const
  TARGET_PROP_NAME = s_string_prop; TARGET_PROP_TYPE_NAME = 'String';
  
  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
  begin
    Result := TSDODataFactory.Create() as ISDODataFactory;
    Result.AddType(s_uri,s_object_type,[]);
    locObj := Result.getType(s_uri,s_object_type);
      Result.addProperty(locObj,s_int_prop,sdo_namespace,'Integer',[]);
      Result.addProperty(locObj,TARGET_PROP_NAME,sdo_namespace,TARGET_PROP_TYPE_NAME,[]);
  end;
  
var
  locFactory : ISDODataFactory;
  locInstance, locInstanceLoaded : ISDODataObject;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  ms : TMemoryStream;
begin
  locFactory := CreateFactory();
  locInstance := locFactory.createNew(s_uri,s_object_type);
  locInstance.setNull(TARGET_PROP_NAME);
  locInstance.setInteger(s_int_prop,123);

  ms := TMemoryStream.Create();
  try
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    s.save(locInstance,ms);  
    
    f := TSDOSerializerStreamXML.Create();
    s := TSDOSerializer.Create(locFactory,f);
    ms.Position := 0;
    locInstanceLoaded := s.load(ms);
    CheckEquals(locInstance.isNull(TARGET_PROP_NAME), locInstanceLoaded.isNull(TARGET_PROP_NAME),Format('isNull(%s)',[TARGET_PROP_NAME]));
    CheckEquals(locInstance.getInteger(s_int_prop), locInstanceLoaded.getInteger(s_int_prop),s_int_prop);
  finally
    ms.Free();
  end; 
end;

procedure TSDOSerializer_Test.load_from_file_start_with_empty();
var
  locFactory, tmpFactory : ISDODataFactory;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  ol : ISDODataObjectList;
  localFileName : string;
begin
  localFileName := sdoExpandLocalFileName(TestFilesPath + 'company.xml');
  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDODataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  ol := TSDODataObjectList.Create(tmpFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType]));
  s.load(localFileName,ol);
  locFactory := CreateSdoTypes();
  CompareTypesInclude(tmpFactory.getTypes(), locFactory.getTypes());
  CompareTypesInclude(locFactory.getTypes(), tmpFactory.getTypes());
end;

procedure TSDOSerializer_Test.load_from_file_one_object();
var
  locFactory, tmpFactory : ISDODataFactory;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  ol : ISDODataObjectList;
  locComp : ISDODataObject;
  localFileName : string;
begin
  localFileName := sdoExpandLocalFileName(TestFilesPath + 'company.one.xml');
  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDODataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  ol := TSDODataObjectList.Create(tmpFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType]));
  s.load(localFileName,ol);

  locFactory := CreateSdoTypes();
  CompareTypesInclude(tmpFactory.getTypes(), locFactory.getTypes());
  CompareTypesInclude(locFactory.getTypes(), tmpFactory.getTypes());
  CheckEquals(1,ol.size(),'loaded objects count');
  locComp := CreateCompanyObject(locFactory);
  ol.getCursor().MoveFirst();
  CheckEquals(True,TSDOEqualityHelper.equalShallow(locComp, ol.getDataObject()),'equalShallow');
  CheckEquals(True,TSDOEqualityHelper.equal(locComp, ol.getDataObject()),'equal');
end;

procedure TSDOSerializer_Test.load_from_file_two_object();
var
  locFactory, tmpFactory : ISDODataFactory;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  ol : ISDODataObjectList;
  locComp : ISDODataObject;
  localFileName : string;
begin
  localFileName := sdoExpandLocalFileName(TestFilesPath + 'company.two.xml');
  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDODataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  ol := TSDODataObjectList.Create(tmpFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType]));
  s.load(localFileName,ol);

  locFactory := CreateSdoTypes();
  CompareTypesInclude(tmpFactory.getTypes(), locFactory.getTypes());
  CompareTypesInclude(locFactory.getTypes(), tmpFactory.getTypes());
  CheckEquals(2,ol.size(),'loaded objects count');
  locComp := CreateCompanyObject(locFactory);
  locComp.setString('name','A Second Sample company');
  ol.getCursor().MoveFirst();
  CheckEquals(True,TSDOEqualityHelper.equalShallow(locComp, ol.getDataObject()),'equalShallow');
  CheckEquals(True,TSDOEqualityHelper.equal(locComp, ol.getDataObject()),'equal');
end;

procedure TSDOSerializer_Test.save_to_file_changesummary_simple();
var
  locFac : ISDODataFactory;
  locEmployee : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  localFileName := sdoExpandLocalFileName('change_summary_simple.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_Employee,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_Employee,s_sn,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_Employee,s_manager,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_Employee,s_age,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_Employee,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locEmployee := locFac.createNew(s_uri,s_Employee);
    locEmployee.setString(s_name,'Inoussa O.');
    locEmployee.setString(s_sn,'002');
    locEmployee.setBoolean(s_manager,True);
    locEmployee.setByte(s_age,30);
    locCS := locEmployee.getChangeSummary();

  locCS.beginLogging();
    locEmployee.setString(s_name,'Inoussa OUEDRAOGO');
    locEmployee.setString(s_sn,'001');
    locEmployee.setByte(s_age,32);

  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  s.save(s_Employee,locEmployee,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + 'change_summary_simple.xml'));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;

procedure TSDOSerializer_Test.load_from_file_changesummary_simple();
var
  locFac, tmpFactory : ISDODataFactory;
  locEmployee : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  ol : ISDODataObjectList;
begin
  localFileName := sdoExpandLocalFileName(TestFilesPath + 'change_summary_simple.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_Employee,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_Employee,s_sn,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_Employee,s_manager,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_Employee,s_age,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_Employee,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locEmployee := locFac.createNew(s_uri,s_Employee);
    locEmployee.setString(s_name,'Inoussa O.');
    locEmployee.setString(s_sn,'002');
    locEmployee.setBoolean(s_manager,True);
    locEmployee.setByte(s_age,30);
    locCS := locEmployee.getChangeSummary();

  locCS.beginLogging();
    locEmployee.setString(s_name,'Inoussa OUEDRAOGO');
    locEmployee.setString(s_sn,'001');
    locEmployee.setByte(s_age,32);

  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDODataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  ol := TSDODataObjectList.Create(tmpFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType]));
  s.load(localFileName,ol);
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
    Compare(
      locEmployee.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      ol.getDataObject(0).getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    locEmployee.getChangeSummary().undoChanges();
    ol.getDataObject(0).getChangeSummary().undoChanges();
      CheckEquals(0, locEmployee.getChangeSummary().getChangedDataObjects().size());
      CheckEquals(0, ol.getDataObject(0).getChangeSummary().getChangedDataObjects().size());
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
end;

procedure TSDOSerializer_Test.save_to_file_changesummary_object_modify_nested();
var
  locFac : ISDODataFactory;
  locDep, locEmployee : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  localFileName := sdoExpandLocalFileName('change_summary_object_modify_nested.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
  locFac.AddType(s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_DepartmentType,s_Employee,s_uri,s_Employee,[pfIsContainment]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

    locFac.addProperty(s_uri,s_Employee,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_Employee,s_sn,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_Employee,s_manager,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_Employee,s_age,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[pfIsAttribute]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setInteger(s_number,1210);
    locEmployee := locDep.createDataObject(s_Employee);
      locEmployee.setString(s_name,'Inoussa O.');
      locEmployee.setString(s_sn,'002');
      locEmployee.setBoolean(s_manager,True);
      locEmployee.setByte(s_age,12);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
      locEmployee.setString(s_name,'Inoussa OUEDRAOGO');
      locEmployee.setString(s_sn,'001');
      locEmployee.setByte(s_age,32);

  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  s.save(s_DepartmentType,locDep,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + 'change_summary_object_modify_nested.xml'));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;

procedure TSDOSerializer_Test.save_to_file_changesummary_prop_list_integer();
const
  FILE_NAME = 'changesummary_prop_list_integer.xml';
var
  locFac : ISDODataFactory;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  localFileName := sdoExpandLocalFileName(FILE_NAME);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_list_int,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setInteger(s_number,1210);
    ls := locDep.getList(s_list_int);
      ls.append(1);
      ls.append(2);
      ls.append(3);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setInteger(0,10);
    ls.append(123);
    ls.append(456);
    ls.setInteger(1,20);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, 1076);
    ls.append(789);
{$IFDEF TEST_GENERATE_FILE}
  (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create()) as ISDOSerializer).save(locDep,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}


  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  s.save(s_DepartmentType,locDep,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + FILE_NAME));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;

procedure TSDOSerializer_Test.save_to_file_changesummary_object_create();
var
  locFac : ISDODataFactory;
  locDep, locEmployee : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  localFileName := sdoExpandLocalFileName('change_summary_object_create.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
  locFac.AddType(s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_DepartmentType,s_Employee,s_uri,s_Employee,[pfIsContainment]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

    locFac.addProperty(s_uri,s_Employee,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_Employee,s_sn,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_Employee,s_manager,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsAttribute]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
  locCS := locDep.getChangeSummary();
    locDep.setString(s_location,'Ouaga, BF');
  locCS.beginLogging();
    locDep.setInteger(s_number,1210);
    locEmployee := locDep.createDataObject(s_Employee);
      locEmployee.setString(s_name,'Inoussa O.');
      locEmployee.setString(s_sn,'002');
      locEmployee.setBoolean(s_manager,True);
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
      locEmployee.setString(s_name,'Inoussa OUEDRAOGO');
      locEmployee.setString(s_sn,'001');

  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  s.save(s_DepartmentType,locDep,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + 'change_summary_object_create.xml'));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;

procedure TSDOSerializer_Test.save_to_file_changesummary_object_create_cont_ref();
var
  locFac : ISDODataFactory;
  locDep, locEmployee : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  localFileName := sdoExpandLocalFileName('change_summary_object_create_cont_ref.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
  locFac.AddType(s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_DepartmentType,s_Employee,s_uri,s_Employee,[pfIsContainment]);
    locFac.addProperty(s_uri,s_DepartmentType,'employee_ref',s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

    locFac.addProperty(s_uri,s_Employee,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_Employee,s_sn,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_Employee,s_manager,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsAttribute]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
  locCS := locDep.getChangeSummary();
    locDep.setString(s_location,'Ouaga, BF');
  locCS.beginLogging();
    locDep.setInteger(s_number,1210);
    locEmployee := locDep.createDataObject(s_Employee);
      locEmployee.setString(s_name,'Inoussa O.');
      locEmployee.setString(s_sn,'002');
      locEmployee.setBoolean(s_manager,True);
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    locDep.setDataObject('employee_ref',locEmployee);
      locEmployee.setString(s_name,'Inoussa OUEDRAOGO');
      locEmployee.setString(s_sn,'001');

  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  s.save(s_DepartmentType,locDep,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + 'change_summary_object_create_cont_ref.xml'));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;

procedure TSDOSerializer_Test.load_from_file_changesummary_object_create_cont_ref();
var
  locFac : ISDODataFactory;
  locDep, locEmployee, locLoadedDep : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
begin
  localFileName := sdoExpandLocalFileName('change_summary_object_create_cont_ref.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
  locFac.AddType(s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_DepartmentType,s_Employee,s_uri,s_Employee,[pfIsContainment]);
    locFac.addProperty(s_uri,s_DepartmentType,'employee_ref',s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

    locFac.addProperty(s_uri,s_Employee,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_Employee,s_sn,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsAttribute]);
    locFac.addProperty(s_uri,s_Employee,s_manager,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsAttribute]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
  locCS := locDep.getChangeSummary();
    locDep.setString(s_location,'Ouaga, BF');
  locCS.beginLogging();
    locDep.setInteger(s_number,1210);
    locEmployee := locDep.createDataObject(s_Employee);
      locEmployee.setString(s_name,'Inoussa O.');
      locEmployee.setString(s_sn,'002');
      locEmployee.setBoolean(s_manager,True);
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    locDep.setDataObject('employee_ref',locEmployee);
      locEmployee.setString(s_name,'Inoussa OUEDRAOGO');
      locEmployee.setString(s_sn,'001');

  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  locLoadedDep := s.load(sdoExpandLocalFileName(TestFilesPath + 'change_summary_object_create_cont_ref.xml'));
    Check(TSDOEqualityHelper.equal(locLoadedDep,locDep),'Object');
    Compare(
      locDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      locLoadedDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    locDep.getChangeSummary().undoChanges();
    locLoadedDep.getChangeSummary().undoChanges();
      CheckEquals( 0, locDep.getChangeSummary().getChangedDataObjects().size());
      CheckEquals( 0, locLoadedDep.getChangeSummary().getChangedDataObjects().size());
      Check(TSDOEqualityHelper.equal(locDep, locLoadedDep),'Object');
end;

procedure TSDOSerializer_Test.save_to_file_changesummary_object_delete();
var
  locFac : ISDODataFactory;
  locDep, locEmployee : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  localFileName := sdoExpandLocalFileName('change_summary_object_delete.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
  locFac.AddType(s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType], [pfIsAttribute]);
    locFac.addProperty(s_uri,s_DepartmentType,s_Employee,s_uri,s_Employee,[pfIsContainment]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

    locFac.addProperty(s_uri,s_Employee,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
    locFac.addProperty(s_uri,s_Employee,s_sn,sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
    locFac.addProperty(s_uri,s_Employee,s_manager,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType], [pfIsAttribute]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locEmployee := locDep.createDataObject(s_Employee);
      locEmployee.setString(s_name,'Inoussa O.');
      locEmployee.setString(s_sn,'002');
      locEmployee.setBoolean(s_manager,True);
  locCS := locDep.getChangeSummary();
    locDep.setString(s_location,'Ouaga, BF');
  locCS.beginLogging();
    locDep.setInteger(s_number,1210);
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    locDep.setDataObject(s_Employee,nil);

  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  s.save(s_DepartmentType,locDep,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + 'change_summary_object_delete.xml'));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;

procedure TSDOSerializer_Test.load_from_file_changesummary_object_delete();
var
  locFac, tmpFactory : ISDODataFactory;
  locDep, locEmployee, locLoadedDep : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
begin
  localFileName := sdoExpandLocalFileName(TestFilesPath + 'change_summary_object_delete.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
  locFac.AddType(s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType], []);
    locFac.addProperty(s_uri,s_DepartmentType,s_Employee,s_uri,s_Employee,[pfIsContainment]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

    locFac.addProperty(s_uri,s_Employee,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
    locFac.addProperty(s_uri,s_Employee,s_sn,sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
    locFac.addProperty(s_uri,s_Employee,s_manager,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType], []);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locEmployee := locDep.createDataObject(s_Employee);
      locEmployee.setString(s_name,'Inoussa O.');
      locEmployee.setString(s_sn,'002');
      locEmployee.setBoolean(s_manager,True);
  locCS := locDep.getChangeSummary();
    locDep.setString(s_location,'Ouaga, BF');
  locCS.beginLogging();
    locDep.setInteger(s_number,1210);
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    locDep.setDataObject(s_Employee,nil);     (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create()) as ISDOSerializer).save(locDep,'change_summary_object_delete.xml');

  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDOBaseDataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  locLoadedDep := s.load(localFileName);

  Check(TSDOEqualityHelper.equal(locDep,locLoadedDep),'Object');
  Compare(
    locDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
    locLoadedDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
  );

  locDep.getChangeSummary().undoChanges();
  locLoadedDep.getChangeSummary().undoChanges();
    CheckEquals( 0, locDep.getChangeSummary().getChangedDataObjects().size());
    CheckEquals( 0, locLoadedDep.getChangeSummary().getChangedDataObjects().size());
    Check(TSDOEqualityHelper.equal(locDep, locLoadedDep),'Object');
end;

procedure TSDOSerializer_Test.save_to_file_changesummary_object_delete_nested();
var
  locFac : ISDODataFactory;
  locA, locB, locC, locD : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  localFileName := sdoExpandLocalFileName('change_summary_object_delete_nested.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
  locFac.AddType(s_uri,'b',[]);
  locFac.AddType(s_uri,'c',[]);
  locFac.AddType(s_uri,'d',[]);
    locFac.addProperty(s_uri,'a','p_a_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
    locFac.addProperty(s_uri,'a','p_ab',s_uri,'b',[pfIsContainment]);
    locFac.addProperty(s_uri,'a','p_ac',s_uri,'c',[]);
    locFac.addProperty(s_uri,'a','p_ad',s_uri,'d',[]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);
      locFac.addProperty(s_uri,'b','p_bc',s_uri,'c',[pfIsContainment]);
      locFac.addProperty(s_uri,'b','p_b_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
        locFac.addProperty(s_uri,'c','p_cd',s_uri,'d',[pfIsContainment]);
        locFac.addProperty(s_uri,'c','p_c_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
          locFac.addProperty(s_uri,'d','p_d_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);

  locA := locFac.createNew(s_uri,'a');
    locA.setString('p_a_str','sample A'' property.');
    locB := locA.createDataObject('p_ab');
      locB.setString('p_b_str','Inoussa O.');
      locC := locB.createDataObject('p_bc');
      locC.setString('p_c_str','azerty');
      locD := locC.createDataObject('p_cd');
        locD.setString('p_d_str','D value');
    locA.setDataObject('p_ac',locC);
    locA.setDataObject('p_ad',locD);
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locC.setDataObject('p_cd',nil);
    locA.setDataObject('p_ab',nil);

  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  s.save('a',locA,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + 'change_summary_object_delete_nested.xml'));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;

procedure TSDOSerializer_Test.load_from_file_changesummary_prop_list_integer();
const
  FILE_NAME = 'changesummary_prop_list_integer.xml';
var
  locFac, tmpFactory : ISDODataFactory;
  locDep, locLoadedDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_list_int,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setInteger(s_number,1210);
    ls := locDep.getList(s_list_int);
      ls.append(1);
      ls.append(2);
      ls.append(3);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setInteger(0,10);
    ls.append(123);
    ls.append(456);
    ls.setInteger(1,20);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, 1076);
    ls.append(789);
{$IFDEF TEST_GENERATE_FILE}
  (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create())as ISDOSerializer).save(locDep,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}  

  localFileName := sdoExpandLocalFileName(TestFilesPath + FILE_NAME);
  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDOBaseDataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  locLoadedDep := s.load(localFileName);

  Check(TSDOEqualityHelper.equal(locDep, locLoadedDep),'Object');
  Compare(
    locDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
    locLoadedDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
  );
end;

{$IFDEF HAS_SDO_LONG}
procedure TSDOSerializer_Test.load_from_file_changesummary_prop_list_long();
const
  PROP_NAME = s_list_long;
  PROP_TYPE = LongType;
  FILE_NAME = 'changesummary_prop_list_long.xml';
var
  locFac, tmpFactory : ISDODataFactory;
  locDep, locLoadedDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setByte(s_number,123);
    ls := locDep.getList(PROP_NAME);
      ls.append(TSDOLong(11111111111111111));
      ls.append(TSDOLong(-2222222222222222));
      ls.append(TSDOLong(333333333333333333));
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setLong(0,4444444444444444444);
    ls.append(TSDOLong(5555555555555555555));
    ls.append(TSDOLong(-6666666666666666666));
    ls.setLong(1,7777777777777777777);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, TSDOLong(8));
    ls.append(TSDOLong(-9));
{$IFDEF TEST_GENERATE_FILE}
  (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create())as ISDOSerializer).save(locDep,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}  
    
  localFileName := sdoExpandLocalFileName(TestFilesPath + FILE_NAME); 
  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDOBaseDataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  locLoadedDep := s.load(localFileName);

  Check(TSDOEqualityHelper.equal(locDep, locLoadedDep),'Object');
  Compare(
    locDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
    locLoadedDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
  );
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure TSDOSerializer_Test.load_from_file_changesummary_prop_list_short();
const
  PROP_NAME = s_list_short;
  PROP_TYPE = ShortType;
  FILE_NAME = 'changesummary_prop_list_short.xml';
var
  locFac, tmpFactory : ISDODataFactory;
  locDep, locLoadedDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setByte(s_number,123);
    ls := locDep.getList(PROP_NAME);
      ls.append(TSDOShort(1));
      ls.append(TSDOShort(2));
      ls.append(TSDOShort(3));
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setShort(0,10);
    ls.append(TSDOShort(12345));
    ls.append(TSDOShort(-5245));
    ls.setShort(1,20);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, TSDOShort(107));
    ls.append(TSDOShort(89));
{$IFDEF TEST_GENERATE_FILE}
  (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create())as ISDOSerializer).save(locDep,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}  
    
  localFileName := sdoExpandLocalFileName(TestFilesPath + FILE_NAME); 
  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDOBaseDataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  locLoadedDep := s.load(localFileName);

  Check(TSDOEqualityHelper.equal(locDep, locLoadedDep),'Object');
  Compare(
    locDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
    locLoadedDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
  );
end;
{$ENDIF HAS_SDO_SHORT}

procedure TSDOSerializer_Test.load_from_file_changesummary_object_delete_nested();
var
  locFac, tmpFactory : ISDODataFactory;
  locA, locB, locC, locD, locLoadedA : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
begin
  localFileName := sdoExpandLocalFileName(TestFilesPath + 'change_summary_object_delete_nested.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
  locFac.AddType(s_uri,'b',[]);
  locFac.AddType(s_uri,'c',[]);
  locFac.AddType(s_uri,'d',[]);
    locFac.addProperty(s_uri,'a','p_a_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
    locFac.addProperty(s_uri,'a','p_ab',s_uri,'b',[pfIsContainment]);
    locFac.addProperty(s_uri,'a','p_ac',s_uri,'c',[]);
    locFac.addProperty(s_uri,'a','p_ad',s_uri,'d',[]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);
      locFac.addProperty(s_uri,'b','p_bc',s_uri,'c',[pfIsContainment]);
      locFac.addProperty(s_uri,'b','p_b_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
        locFac.addProperty(s_uri,'c','p_cd',s_uri,'d',[pfIsContainment]);
        locFac.addProperty(s_uri,'c','p_c_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
          locFac.addProperty(s_uri,'d','p_d_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);

  locA := locFac.createNew(s_uri,'a');
    locA.setString('p_a_str','sample A'' property.');
    locB := locA.createDataObject('p_ab');
      locB.setString('p_b_str','Inoussa O.');
      locC := locB.createDataObject('p_bc');
      locC.setString('p_c_str','azerty');
      locD := locC.createDataObject('p_cd');
        locD.setString('p_d_str','D value');
    locA.setDataObject('p_ac',locC);
    locA.setDataObject('p_ad',locD);
  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locC.setDataObject('p_cd',nil);
    locA.setDataObject('p_ab',nil);

  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDOBaseDataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  locLoadedA := s.load(localFileName);

  Check(TSDOEqualityHelper.equal(locA,locLoadedA),'Object');
  Compare(
    locA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
    locLoadedA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
  );

  locA.getChangeSummary().undoChanges();
  locLoadedA.getChangeSummary().undoChanges();
    CheckEquals( 0, locA.getChangeSummary().getChangedDataObjects().size());
    CheckEquals( 0, locLoadedA.getChangeSummary().getChangedDataObjects().size());
    Check(TSDOEqualityHelper.equal(locA, locLoadedA),'Object');
end;

procedure TSDOSerializer_Test.save_to_file_changesummary_object_delete_2_objects_same_type();
var
  locFac : ISDODataFactory;
  locA, locB : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  localFileName := sdoExpandLocalFileName('changesummary_object_delete_2_objects_same_type.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
  locFac.AddType(s_uri,'b',[]);
    locFac.addProperty(s_uri,'a','p_a_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
    locFac.addProperty(s_uri,'a','p_ab1',s_uri,'b',[pfIsContainment]);
    locFac.addProperty(s_uri,'a','p_ab2',s_uri,'b',[pfIsContainment]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);
      locFac.addProperty(s_uri,'b','p_b_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);

  locA := locFac.createNew(s_uri,'a');
    locA.setString('p_a_str','sample A'' property.');
    locB := locA.createDataObject('p_ab1');
      locB.setString('p_b_str','p_ab1\p_b_str value');
    locB := locA.createDataObject('p_ab2');
      locB.setString('p_b_str','p_ab2\p_b_str value');

  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.setDataObject('p_ab1',nil);
    locA.setDataObject('p_ab2',nil);

  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  s.save('a',locA,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + 'changesummary_object_delete_2_objects_same_type.xml'));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;

procedure TSDOSerializer_Test.load_from_file_changesummary_object_delete_2_objects_same_type();
var
  locFac, tmpFactory : ISDODataFactory;
  locA, locB, locLoadedA : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
begin
  localFileName := sdoExpandLocalFileName(TestFilesPath + 'changesummary_object_delete_2_objects_same_type.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
  locFac.AddType(s_uri,'b',[]);
    locFac.addProperty(s_uri,'a','p_a_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
    locFac.addProperty(s_uri,'a','p_ab1',s_uri,'b',[pfIsContainment]);
    locFac.addProperty(s_uri,'a','p_ab2',s_uri,'b',[pfIsContainment]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);
      locFac.addProperty(s_uri,'b','p_b_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);

  locA := locFac.createNew(s_uri,'a');
    locA.setString('p_a_str','sample A'' property.');
    locB := locA.createDataObject('p_ab1');
      locB.setString('p_b_str','p_ab1\p_b_str value');
    locB := locA.createDataObject('p_ab2');
      locB.setString('p_b_str','p_ab2\p_b_str value');

  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.setDataObject('p_ab1',nil);
    locA.setDataObject('p_ab2',nil);

  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDOBaseDataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  locLoadedA := s.load(localFileName);

  Check(TSDOEqualityHelper.equal(locA,locLoadedA),'Object');
  Compare(
    locA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
    locLoadedA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
  );

  locA.getChangeSummary().undoChanges();
  locLoadedA.getChangeSummary().undoChanges();
    CheckEquals( 0, locA.getChangeSummary().getChangedDataObjects().size());
    CheckEquals( 0, locLoadedA.getChangeSummary().getChangedDataObjects().size());
    Check(TSDOEqualityHelper.equal(locA, locLoadedA),'Object');
end;

procedure TSDOSerializer_Test.save_to_file_changesummary_object_2_objects_same_type_del_upd();
var
  locFac : ISDODataFactory;
  locA, locB : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  localFileName := sdoExpandLocalFileName('changesummary_object_2_objects_same_type_del_upd.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
  locFac.AddType(s_uri,'b',[]);
    locFac.addProperty(s_uri,'a','p_a_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);
    locFac.addProperty(s_uri,'a','p_ab1',s_uri,'b',[pfIsContainment]);
    locFac.addProperty(s_uri,'a','p_ab2',s_uri,'b',[pfIsContainment]);
    locFac.addProperty(s_uri,'a','p_ab3',s_uri,'b',[pfIsContainment]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);
      locFac.addProperty(s_uri,'b','p_b_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], [pfIsAttribute]);

  locA := locFac.createNew(s_uri,'a');
    locA.setString('p_a_str','sample A'' property.');
    locB := locA.createDataObject('p_ab1');
      locB.setString('p_b_str','p_ab1\p_b_str value');
    locB := locA.createDataObject('p_ab2');
      locB.setString('p_b_str','p_ab2\p_b_str value before');
    locB := locA.createDataObject('p_ab3');
      locB.setString('p_b_str','p_ab3\p_b_str value');

  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.setDataObject('p_ab1',nil);
    locA.setString('p_ab2/p_b_str','p_ab3\p_b_str value after');
    locA.setDataObject('p_ab3',nil);

  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  s.save('a',locA,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + 'changesummary_object_2_objects_same_type_del_upd.xml'));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;

procedure TSDOSerializer_Test.load_from_file_changesummary_object_2_objects_same_type_del_upd();
var
  locFac, tmpFactory : ISDODataFactory;
  locA, locB, locLoadedA : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
begin
  localFileName := sdoExpandLocalFileName(TestFilesPath + 'changesummary_object_2_objects_same_type_del_upd.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'a',[]);
  locFac.AddType(s_uri,'b',[]);
    locFac.addProperty(s_uri,'a','p_a_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
    locFac.addProperty(s_uri,'a','p_ab1',s_uri,'b',[pfIsContainment]);
    locFac.addProperty(s_uri,'a','p_ab2',s_uri,'b',[pfIsContainment]);
    locFac.addProperty(s_uri,'a','p_ab3',s_uri,'b',[pfIsContainment]);
    locFac.addProperty(s_uri,'a',s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);
      locFac.addProperty(s_uri,'b','p_b_str',sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);

  locA := locFac.createNew(s_uri,'a');
    locA.setString('p_a_str','sample A'' property.');
    locB := locA.createDataObject('p_ab1');
      locB.setString('p_b_str','p_ab1\p_b_str value');
    locB := locA.createDataObject('p_ab2');
      locB.setString('p_b_str','p_ab2\p_b_str value before');
    locB := locA.createDataObject('p_ab3');
      locB.setString('p_b_str','p_ab3\p_b_str value');

  locCS := locA.getChangeSummary();
  locCS.beginLogging();
    locA.setDataObject('p_ab1',nil);
    locA.setString('p_ab2/p_b_str','p_ab3\p_b_str value after');
    locA.setDataObject('p_ab3',nil);

  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDOBaseDataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  locLoadedA := s.load(localFileName);

  Check(TSDOEqualityHelper.equal(locA,locLoadedA),'Object');
  Compare(
    locA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
    locLoadedA.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
  );

  locA.getChangeSummary().undoChanges();
  locLoadedA.getChangeSummary().undoChanges();
    CheckEquals( 0, locA.getChangeSummary().getChangedDataObjects().size());
    CheckEquals( 0, locLoadedA.getChangeSummary().getChangedDataObjects().size());
    Check(TSDOEqualityHelper.equal(locA, locLoadedA),'Object');
end;

procedure TSDOSerializer_Test.save_to_file_changesummary_prop_list_object();

  function create_employee(
    const AFac : ISDODataFactory;
    const AName, ASN : TSDOString;
    const AManager : Boolean
  ) : ISDODataObject;
  begin
    Result := AFac.createNew(s_uri, s_EmployeeType);
    Result.setString(s_name, AName);
    Result.setString(s_sn, ASN);
    Result.setBoolean(s_manager, AManager);
  end;

var
  locFac : ISDODataFactory;
  locDep, e1, e2 : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  localFileName := sdoExpandLocalFileName('changesummary_prop_list_object.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_EmployeeType,[]);
    locFac.addProperty(s_uri, s_EmployeeType,'name',sdo_namespace,'string',[]);
    locFac.addProperty(s_uri, s_EmployeeType,'SN',sdo_namespace,'string',[]);
    locFac.addProperty(s_uri, s_EmployeeType,'manager',sdo_namespace,'boolean',[]);
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_list_object,s_uri,s_EmployeeType,[pfIsMany,pfIsContainment]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);


  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setInteger(s_number,1210);
    ls := locDep.getList(s_list_object);
      ls.append(create_employee(locFac,'Inoussa O.', '0001', True));
      ls.append(create_employee(locFac,'Kis O.', '0002', False));
      ls.append(create_employee(locFac,'WST', '0003', False));
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    e1 := create_employee(locFac,'FPC', '0010', False);
    e2 := create_employee(locFac,'Lazarus', '0011', False);
    ls.setDataObject(0,e1);
    ls.append(create_employee(locFac,'FPC 2 ', '0020', True));
    ls.insert(3,create_employee(locFac,'FPC 5', '0050', False));
    ls.append(create_employee(locFac,'FPC 3', '0030', False));
    ls.setDataObject(1,e2);
    ls.delete(0);
    ls.delete(1);
    ls.append(create_employee(locFac,'FPC 4', '0040', True));
  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  s.save(s_DepartmentType,locDep,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + 'changesummary_prop_list_object.xml'));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;

procedure TSDOSerializer_Test.load_from_file_changesummary_prop_list_object();

  function create_employee(
    const AFac : ISDODataFactory;
    const AName, ASN : TSDOString;
    const AManager : Boolean
  ) : ISDODataObject;
  begin
    Result := AFac.createNew(s_uri, s_EmployeeType);
    Result.setString(s_name, AName);
    Result.setString(s_sn, ASN);
    Result.setBoolean(s_manager, AManager);
  end;

var
  locFac, tmpFactory : ISDODataFactory;
  locDep, locLoadedDep, e1, e2 : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_EmployeeType,[]);
    locFac.addProperty(s_uri, s_EmployeeType,'name',sdo_namespace,'string',[]);
    locFac.addProperty(s_uri, s_EmployeeType,'SN',sdo_namespace,'string',[]);
    locFac.addProperty(s_uri, s_EmployeeType,'manager',sdo_namespace,'boolean',[]);
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_list_object,s_uri,s_EmployeeType,[pfIsMany,pfIsContainment]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);


  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setInteger(s_number,1210);
    ls := locDep.getList(s_list_object);
      ls.append(create_employee(locFac,'Inoussa O.', '0001', True));
      ls.append(create_employee(locFac,'Kis O.', '0002', False));
      ls.append(create_employee(locFac,'WST', '0003', False));
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    e1 := create_employee(locFac,'FPC', '0010', False);
    e2 := create_employee(locFac,'Lazarus', '0011', False);
    ls.setDataObject(0,e1);
    ls.append(create_employee(locFac,'FPC 2 ', '0020', True));
    ls.insert(3,create_employee(locFac,'FPC 5', '0050', False));
    ls.append(create_employee(locFac,'FPC 3', '0030', False));
    ls.setDataObject(1,e2);
    ls.delete(0);
    ls.delete(1);
    ls.append(create_employee(locFac,'FPC 4', '0040', True));

  localFileName := sdoExpandLocalFileName(TestFilesPath + 'changesummary_prop_list_object.xml');
  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDOBaseDataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  locLoadedDep := s.load(localFileName);
  locDep.getChangeSummary().getOldValues(locDep);
  locLoadedDep.getChangeSummary().getOldValues(locLoadedDep);

  Check(TSDOEqualityHelper.equal(locDep,locLoadedDep),'Object');
  {Compare(
    locDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
    locLoadedDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
  );}

  locDep.getChangeSummary().undoChanges();
  locLoadedDep.getChangeSummary().undoChanges();
    CheckEquals( 0, locDep.getChangeSummary().getChangedDataObjects().size());
    CheckEquals( 0, locLoadedDep.getChangeSummary().getChangedDataObjects().size());
    Check(TSDOEqualityHelper.equal(locDep, locLoadedDep),'Object');
end;

procedure TSDOSerializer_Test.save_to_file_changesummary_prop_list_bool();
var
  locFac : ISDODataFactory;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  localFileName := sdoExpandLocalFileName('changesummary_prop_list_bool.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_list_bool,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setInteger(s_number,1210);
    ls := locDep.getList(s_list_bool);
      ls.append(False);
      ls.append(True);
      ls.append(False);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setBoolean(0,True);
    ls.append(True);
    ls.append(True);
    ls.setBoolean(1,False);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, False);
    ls.append(False);
  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  s.save(s_DepartmentType,locDep,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + 'changesummary_prop_list_bool.xml'));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;

procedure TSDOSerializer_Test.load_from_file_changesummary_prop_list_bool();
var
  locFac, tmpFactory : ISDODataFactory;
  locDep, locLoadedDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_list_bool,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setInteger(s_number,1210);
    ls := locDep.getList(s_list_bool);
      ls.append(False);
      ls.append(True);
      ls.append(False);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setBoolean(0,True);
    ls.append(True);
    ls.append(True);
    ls.setBoolean(1,False);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, False);
    ls.append(False);

  localFileName := sdoExpandLocalFileName(TestFilesPath + 'changesummary_prop_list_bool.xml');
  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDOBaseDataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  locLoadedDep := s.load(localFileName);

  Check(TSDOEqualityHelper.equal(locDep, locLoadedDep),'Object');
  Compare(
    locDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
    locLoadedDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
  );
end;

procedure TSDOSerializer_Test.save_to_file_changesummary_prop_list_string();
var
  locFac : ISDODataFactory;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  localFileName := sdoExpandLocalFileName('changesummary_prop_list_string.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_list_string,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setInteger(s_number,1210);
    ls := locDep.getList(s_list_string);
      ls.append('wst');
      ls.append('sdo');
      ls.append('fpc-lazarus');
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setString(0,'azerty');
    ls.append('Ouagadougou');
    ls.append('BF');
    ls.setString(1,'kis');
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, '107612');
    ls.append('this is a multi words text. Lets test it!');
  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  s.save(s_DepartmentType,locDep,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + 'changesummary_prop_list_string.xml'));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;

procedure TSDOSerializer_Test.load_from_file_changesummary_prop_list_string();
var
  locFac, tmpFactory : ISDODataFactory;
  locDep, locLoadedDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_list_string,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setInteger(s_number,1210);
    ls := locDep.getList(s_list_string);
      ls.append('wst');
      ls.append('sdo');
      ls.append('fpc-lazarus');
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setString(0,'azerty');
    ls.append('Ouagadougou');
    ls.append('BF');
    ls.setString(1,'kis');
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, '107612');
    ls.append('this is a multi words text. Lets test it!');

  localFileName := sdoExpandLocalFileName(TestFilesPath + 'changesummary_prop_list_string.xml');
  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDOBaseDataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  locLoadedDep := s.load(localFileName);

  Check(TSDOEqualityHelper.equal(locDep, locLoadedDep),'Object');
  Compare(
    locDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
    locLoadedDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
  );
end;

procedure TSDOSerializer_Test.save_to_file_changesummary_prop_list_object_nested();

  function create_employee(
    const AFac : ISDODataFactory;
    const AName, ASN : TSDOString;
    const AManager : Boolean;
    const AJobCount : Integer
  ) : ISDODataObject;
  var
    jb : ISDODataObject;
    k : Integer;
  begin
    Result := AFac.createNew(s_uri, s_EmployeeType);
    Result.setString(s_name, AName);
    Result.setString(s_sn, ASN);
    Result.setBoolean(s_manager, AManager);
    for k := 0 to Pred(AJobCount) do begin
      jb := Result.createDataObject('jobs');
        jb.setString('title', Format('%s''s job #%d',[AName,k]));
        jb.setByte('MaxEmployeeCount',( k * AJobCount ) mod High(TSDOByte));
      Result.getList('jobs').append(jb);
    end;
  end;

var
  locFac : ISDODataFactory;
  locDep, e1, e2 : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  localFileName := sdoExpandLocalFileName('changesummary_prop_list_object_nested.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'job',[]);
    locFac.addProperty(s_uri, 'job','title',sdo_namespace,'string',[]);
    locFac.addProperty(s_uri, 'job','MaxEmployeeCount',sdo_namespace,'byte',[]);
  locFac.AddType(s_uri,s_EmployeeType,[]);
    locFac.addProperty(s_uri, s_EmployeeType,'name',sdo_namespace,'string',[]);
    locFac.addProperty(s_uri, s_EmployeeType,'SN',sdo_namespace,'string',[]);
    locFac.addProperty(s_uri, s_EmployeeType,'manager',sdo_namespace,'boolean',[]);
    locFac.addProperty(s_uri, s_EmployeeType,'jobs',s_uri,'job',[pfIsMany,pfIsContainment]);
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_list_object,s_uri,s_EmployeeType,[pfIsMany,pfIsContainment]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);


  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setInteger(s_number,1210);
    ls := locDep.getList(s_list_object);
      ls.append(create_employee(locFac,'Inoussa O.', '0001', True,3));
      ls.append(create_employee(locFac,'Kis O.', '0002', False,2));
      ls.append(create_employee(locFac,'WST', '0003', False,4));
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    e1 := create_employee(locFac,'FPC', '0010', False,1);
    e2 := create_employee(locFac,'Lazarus', '0011', False,3);
    ls.setDataObject(0,e1);
    ls.append(create_employee(locFac,'FPC 2 ', '0020', True,0));
    ls.insert(3,create_employee(locFac,'FPC 5', '0050', False,1));
    ls.append(create_employee(locFac,'FPC 3', '0030', False,0));
    ls.setDataObject(1,e2);
    ls.delete(0);
    ls.delete(1);
    ls.append(create_employee(locFac,'FPC 4', '0040', True,1));
  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  s.save(s_DepartmentType,locDep,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + 'changesummary_prop_list_object_nested.xml'));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;

procedure TSDOSerializer_Test.load_from_file_changesummary_prop_list_object_nested();

  function create_employee(
    const AFac : ISDODataFactory;
    const AName, ASN : TSDOString;
    const AManager : Boolean;
    const AJobCount : Integer
  ) : ISDODataObject;
  var
    jb : ISDODataObject;
    k : Integer;
  begin
    Result := AFac.createNew(s_uri, s_EmployeeType);
    Result.setString(s_name, AName);
    Result.setString(s_sn, ASN);
    Result.setBoolean(s_manager, AManager);
    for k := 0 to Pred(AJobCount) do begin
      jb := Result.createDataObject('jobs');
        jb.setString('title', Format('%s''s job #%d',[AName,k]));
        jb.setByte('MaxEmployeeCount',( k * AJobCount ) mod High(TSDOByte));
      Result.getList('jobs').append(jb);
    end;
  end;

var
  locFac, tmpFactory : ISDODataFactory;
  locDep, e1, e2, locLoadedDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,'job',[]);
    locFac.addProperty(s_uri, 'job','title',sdo_namespace,'string',[]);
    locFac.addProperty(s_uri, 'job','MaxEmployeeCount',sdo_namespace,'byte',[]);
  locFac.AddType(s_uri,s_EmployeeType,[]);
    locFac.addProperty(s_uri, s_EmployeeType,'name',sdo_namespace,'string',[]);
    locFac.addProperty(s_uri, s_EmployeeType,'SN',sdo_namespace,'string',[]);
    locFac.addProperty(s_uri, s_EmployeeType,'manager',sdo_namespace,'boolean',[]);
    locFac.addProperty(s_uri, s_EmployeeType,'jobs',s_uri,'job',[pfIsMany,pfIsContainment]);
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_list_object,s_uri,s_EmployeeType,[pfIsMany,pfIsContainment]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);


  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setInteger(s_number,1210);
    ls := locDep.getList(s_list_object);
      ls.append(create_employee(locFac,'Inoussa O.', '0001', True,3));
      ls.append(create_employee(locFac,'Kis O.', '0002', False,2));
      ls.append(create_employee(locFac,'WST', '0003', False,4));
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    e1 := create_employee(locFac,'FPC', '0010', False,1);
    e2 := create_employee(locFac,'Lazarus', '0011', False,3);
    ls.setDataObject(0,e1);
    ls.append(create_employee(locFac,'FPC 2 ', '0020', True,0));
    ls.insert(3,create_employee(locFac,'FPC 5', '0050', False,1));
    ls.append(create_employee(locFac,'FPC 3', '0030', False,0));
    ls.setDataObject(1,e2);
    ls.delete(0);
    ls.delete(1);
    ls.append(create_employee(locFac,'FPC 4', '0040', True,1));

    localFileName := sdoExpandLocalFileName(TestFilesPath + 'changesummary_prop_list_object_nested.xml');
  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDOBaseDataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  locLoadedDep := s.load(localFileName);
  locDep.getChangeSummary().getOldValues(locDep);
  locLoadedDep.getChangeSummary().getOldValues(locLoadedDep);

  Check(TSDOEqualityHelper.equal(locDep,locLoadedDep),'Object');
  {Compare(
    locDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
    locLoadedDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
  );}

  locDep.getChangeSummary().undoChanges();
  locLoadedDep.getChangeSummary().undoChanges();
    CheckEquals( 0, locDep.getChangeSummary().getChangedDataObjects().size());
    CheckEquals( 0, locLoadedDep.getChangeSummary().getChangedDataObjects().size());
    Check(TSDOEqualityHelper.equal(locDep, locLoadedDep),'Object');
end;

procedure TSDOSerializer_Test.save_to_and_load_file_ref_prop_crash_1();
var
  locFac : ISDODataFactory;
  locPL, locPL1, locPJ0, locPJ1, locP1, locP2, locP3 : ISDODataObject;
  locCS : ISDOChangeSummary;
  serializer : ISDOSerializer;
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
    locPL.getList('Project').delete(1);

  serializer := TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create());
  serializer.save('ProjectList',locPL,'save_to_and_load_file_ref_prop_crash_1.xml');

  serializer := TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create());
  locPL1 := serializer.load('save_to_and_load_file_ref_prop_crash_1.xml');
end;

procedure TSDOSerializer_Test.save_to_and_load_file_ref_prop_crash_2();
var
  locFac : ISDODataFactory;
  locPL, locPL1, locPJ0, locPJ1, locP1, locP2, locP3 : ISDODataObject;
  locCS : ISDOChangeSummary;
  serializer : ISDOSerializer;
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

  locCS.beginLogging();
    locP3 := locPJ1.createDataObject('Member');
    locPJ1.getList('Member').append(locP3);
      locP3.setString('Name', 'P3 person');
      locP1.setDataObject('Manager', locPJ1.getDataObject('Member[Name="P3 person"]'));
    locPL.getList('Project').delete(1);

  serializer := TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create());
  serializer.save('ProjectList',locPL,'save_to_and_load_file_ref_prop_crash_2.xml');

  serializer := TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create());
  locPL1 := serializer.load('save_to_and_load_file_ref_prop_crash_2.xml');
end;

procedure TSDOSerializer_Test.load_from_file_reference_property();
var
  locFac : ISDODataFactory;
  locLoadedObj : ISDODataObject;
  s : ISDOSerializer;
  localFileName : string;
  locType : ISDOType;
  p : ISDOProperty;
  projObj, user1, user2, man : ISDODataObject;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  localFileName := sdoExpandLocalFileName(TestFilesPath + 'load_from_file_reference_property.xml');
  s := TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create());
  locLoadedObj := s.load(localFileName);

  locType := locFac.getType('uri:sample','ProjectType');
  Check(( locType <> nil ),'ProjectType');
    p := locType.getProperty('ProjectLeader');
    Check(p <> nil,'ProjectLeader');
    Check(p.isReference(),'"ProjectLeader" should be a "Reference" property');
    Check(p.getType() = locFac.getType('uri:sample','Person'),'"ProjectLeader" should be a "Person" type property');

  Check(locLoadedObj <> nil,'ProjectList');
  projObj := locLoadedObj.getDataObject('Project[ProjectName="WST"]');
    Check(projObj <> nil,'Project');
    user1 := projObj.getDataObject('Member[Name="wst user 1"]');
      Check(user1 <> nil,'Member[Name="wst user 1"]');
    user2 := projObj.getDataObject('Member[Name="wst user 2"]');
      Check(user1 <> nil,'Member[Name="wst user 2"]');
    man := projObj.getDataObject('Member[Name="Inoussa O."]');
      Check(user1 <> nil,'Member[Name="Inoussa O."]');
    Check( man = projObj.getDataObject('ProjectLeader'),  'ProjectLeader');
    Check( man = user2.getDataObject('Manager'), 'user Manager');
end;

procedure TSDOSerializer_Test.save_object_open_type();
var
  fact : ISDODataFactory;
  dst, row : ISDODataObject;
  ser : ISDOSerializer;
  stream : TMemoryStream;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  fact := TSDODataFactory.Create() as ISDODataFactory;
  fact.AddType(s_uri,'Datarow',[tfIsOpen]);
  fact.AddType(s_uri,'Dataset',[tfIsOpen]);
  fact.addProperty(s_uri,'Dataset','rows',s_uri,'Datarow',[pfIsMany,pfIsContainment]);

  dst := fact.createNew(s_uri,'Dataset');
  row := dst.createDataObject('rows');
    row.setString('Name','SDO-Pascal');
    row.setString('Description','Object Pascal implementation of SDO');
    row.setInteger('Order',1);
  dst.getList('rows').append(row);
  row := dst.createDataObject('rows');
    row.setString('Name','WST');
    row.setString('Description','Object Pascal Web Services Toolkit');
    row.setInteger('Order',2);
  dst.getList('rows').append(row);

  existDoc := nil;
  serialDoc := nil;
  ser := TSDOSerializer.Create(fact,TSDOSerializerStreamXML.Create());
  stream := TMemoryStream.Create();
  try
    ser.save(dst,stream);
{$IFDEF TEST_GENERATE_FILE}
    stream.SaveToFile(sdoExpandLocalFileName('open_type.xml'));
{$ENDIF TEST_GENERATE_FILE}
    stream.Position := 0;
    ReadXMLFile(serialDoc,stream);
    ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + 'open_type.xml'));
    CheckEquals(True, CompareNodes(existDoc,serialDoc),'generated document differs from the existent one.');
  finally
    stream.Free();
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;

procedure TSDOSerializer_Test.load_object_open_type();
var
  fact, factGen : ISDODataFactory;
  ser : ISDOSerializer;
  loadedObj, dstGen, row : ISDODataObject;
begin
  // Load the existing file
  fact := TSDODataFactory.Create();
  ser := TSDOSerializer.Create(fact,TSDOSerializerStreamXML.Create());
  loadedObj := ser.load(sdoExpandLocalFileName(TestFilesPath + 'open_type.xml'));
  Check(( loadedObj <> nil ), 'Root object is NULL.');
  CheckEquals('Dataset',loadedObj.getType().getName());
  CheckEquals(2,loadedObj.getList('rows').size());

  // Generate the object copy
  factGen := TSDODataFactory.Create() as ISDODataFactory;
  factGen.AddType(s_uri,'Datarow',[tfIsOpen]);
  factGen.AddType(s_uri,'Dataset',[tfIsOpen]);
  factGen.addProperty(s_uri,'Dataset','rows',s_uri,'Datarow',[pfIsMany,pfIsContainment]);
  dstGen := factGen.createNew(s_uri,'Dataset');
  row := dstGen.createDataObject('rows');
    row.setString('Name','SDO-Pascal');
    row.setString('Description','Object Pascal implementation of SDO');
    row.setInteger('Order',1);
  dstGen.getList('rows').append(row);
  row := dstGen.createDataObject('rows');
    row.setString('Name','WST');
    row.setString('Description','Object Pascal Web Services Toolkit');
    row.setInteger('Order',2);
  dstGen.getList('rows').append(row);

  // ... compare them now
  Check(TSDOEqualityHelper.equal(dstGen,loadedObj), 'Loaded object differs from generated object');
end;

procedure TSDOSerializer_Test.save_to_file_changesummary_prop_list_byte();
const
  LIST_PROP_NAME = s_list_byte;
  LIST_PROP_TYPE = ByteType;
  FILE_NAME = 'changesummary_prop_list_byte.xml';
var
  locFac : ISDODataFactory;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  localFileName := sdoExpandLocalFileName(FILE_NAME);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,LIST_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LIST_PROP_TYPE],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setByte(s_number,123);
    ls := locDep.getList(LIST_PROP_NAME);
      ls.append(TSDOByte(1));
      ls.append(TSDOByte(2));
      ls.append(TSDOByte(3));
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setByte(0,10);
    ls.append(TSDOByte(123));
    ls.append(TSDOByte(45));
    ls.setByte(1,20);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, TSDOByte(107));
    ls.append(TSDOByte(89));
  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  s.save(s_DepartmentType,locDep,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + FILE_NAME));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;

procedure TSDOSerializer_Test.save_to_file_changesummary_prop_list_date();
const VAL_1 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_2 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_3 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_4 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_5 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );

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

    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(2009,9,1,2,3,0,1);
    d.HourOffset := 0;
    d.MinuteOffset := 13;
    PSDODate(@VAL_4)^ := d;

    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(1900,11,8,1,2,0,0);
    d.HourOffset := 0;
    d.MinuteOffset := 13;
    PSDODate(@VAL_5)^ := d;
  end;

var
  locFac : ISDODataFactory;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  SetConstants();
  localFileName := sdoExpandLocalFileName('changesummary_prop_list_date.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_birthDate,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_list_date,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setDate(s_birthDate,VAL_1);
    ls := locDep.getList(s_list_date);
      ls.append(VAL_1);
      ls.append(VAL_2);
      ls.append(VAL_3);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setDate(0,VAL_4);
    ls.append(VAL_5);
    ls.append(VAL_1);
    ls.setDate(1,VAL_2);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, VAL_3);
    ls.append(VAL_4);
  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  s.save(s_DepartmentType,locDep,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + 'changesummary_prop_list_date.xml'));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;

{$IFDEF HAS_SDO_BYTES}
procedure TSDOSerializer_Test.save_to_file_changesummary_prop_list_bytes();
const
  FILE_NAME = 'changesummary_prop_list_bytes.xml';
  LIST_PROP_NAME = s_list_bytes;
  PROP_TYPE = BytesType;
  
var
  VAL_1, VAL_2, VAL_3, VAL_4, VAL_5 : TSDOBytes;

  procedure SetConstants();
  var
    v : TSDOBytes;
    k : Integer;
  begin
    SetLength(v,10);
    for k := 0 to High(v) do
      v[k] := k mod High(Byte);
    VAL_1 := v;
    v := nil;    

    VAL_2 := nil;

    SetLength(v,20);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
    VAL_3 := v; 
    v := nil;  

    SetLength(v,30);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
    VAL_4 := v; 
    v := nil;   

    SetLength(v,40);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
    VAL_5 := v; 
    v := nil;       
  end;
  
var
  locFac : ISDODataFactory;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  SetConstants();
  localFileName := sdoExpandLocalFileName(FILE_NAME);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_birthDate,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,LIST_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    //locDep.setBytes(s_birthDate,VAL_1);
    ls := locDep.getList(LIST_PROP_NAME);
      ls.appendBytes(VAL_1);
      ls.appendBytes(VAL_2);
      ls.appendBytes(VAL_3);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setBytes(0,VAL_4);
    ls.appendBytes(VAL_5);
    ls.appendBytes(VAL_1);
    ls.setBytes(1,VAL_2);
    ls.delete(0);
    ls.delete(1);
    ls.insertBytes(2, VAL_3);
    ls.appendBytes(VAL_4);
{$IFDEF TEST_GENERATE_FILE}
    (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create()) as ISDOSerializer).save(locDep,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE} 
    
  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  s.save(s_DepartmentType,locDep,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + FILE_NAME));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;

procedure TSDOSerializer_Test.load_from_file_changesummary_bytes();
const
  PROP_NAME = 'sampleProperty';
  PROP_TYPE = BytesType;
  FILE_NAME = 'change_summary_bytes.xml';
var
  VAL_1, VAL_2 : TSDOBytes;

  procedure SetConstants();
  var
    v : TSDOBytes;
    k : Integer;
  begin
    SetLength(v,10);
    for k := 0 to High(v) do
      v[k] := k mod High(Byte);
    VAL_1 := v;
    v := nil;    

    SetLength(v,20);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
    VAL_2 := v;
  end;  

var
  locFac, tmpFactory : ISDODataFactory;
  locEmployee : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  ol : ISDODataObjectList;
begin
  SetConstants();
  localFileName := sdoExpandLocalFileName(TestFilesPath + FILE_NAME);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_Employee,PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[]);
    locFac.addProperty(s_uri,s_Employee,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locEmployee := locFac.createNew(s_uri,s_Employee);
    locEmployee.setBytes(PROP_NAME,VAL_1);
    locCS := locEmployee.getChangeSummary();

  locCS.beginLogging();
    locEmployee.setBytes(PROP_NAME,VAL_2);
{$IFDEF TEST_GENERATE_FILE}
    (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create()) as ISDOSerializer).save(locEmployee,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}     

  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDODataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  ol := TSDODataObjectList.Create(tmpFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType]));
  s.load(localFileName,ol);
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
    Compare(
      locEmployee.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      ol.getDataObject(0).getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    locEmployee.getChangeSummary().undoChanges();
    ol.getDataObject(0).getChangeSummary().undoChanges();
      CheckEquals(0, locEmployee.getChangeSummary().getChangedDataObjects().size());
      CheckEquals(0, ol.getDataObject(0).getChangeSummary().getChangedDataObjects().size());
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
end;

procedure TSDOSerializer_Test.load_from_file_changesummary_prop_list_bytes();
const
  FILE_NAME = 'changesummary_prop_list_bytes.xml';
  LIST_PROP_NAME = s_list_bytes;
  PROP_TYPE = BytesType;
var
  VAL_1, VAL_2, VAL_3, VAL_4, VAL_5 : TSDOBytes;

  procedure SetConstants();
  var
    v : TSDOBytes;
    k : Integer;
  begin
    SetLength(v,10);
    for k := 0 to High(v) do
      v[k] := k mod High(Byte);
    VAL_1 := v;
    v := nil;    

    VAL_2 := nil;

    SetLength(v,20);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
    VAL_3 := v; 
    v := nil;  

    SetLength(v,30);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
    VAL_4 := v; 
    v := nil;   

    SetLength(v,40);
    for k := 0 to High(v) do
      v[k] := ( ( 3 * k ) + 1 ) mod High(Byte);
    VAL_5 := v; 
    v := nil;       
  end;

var
  locFac, tmpFactory : ISDODataFactory;
  locDep, locLoadedDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
begin
  SetConstants();
  localFileName := sdoExpandLocalFileName(FILE_NAME);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_birthDate,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,LIST_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    //locDep.setBytes(s_birthDate,VAL_1);
    ls := locDep.getList(LIST_PROP_NAME);
      ls.appendBytes(VAL_1);
      ls.appendBytes(VAL_2);
      ls.appendBytes(VAL_3);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setBytes(0,VAL_4);
    ls.appendBytes(VAL_5);
    ls.appendBytes(VAL_1);
    ls.setBytes(1,VAL_2);
    ls.delete(0);
    ls.delete(1);
    ls.insertBytes(2, VAL_3);
    ls.appendBytes(VAL_4);

  localFileName := sdoExpandLocalFileName(TestFilesPath + FILE_NAME); 
  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDOBaseDataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  locLoadedDep := s.load(localFileName);

  Check(TSDOEqualityHelper.equal(locDep, locLoadedDep),'Object');
  Compare(
    locDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
    locLoadedDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
  );
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
procedure TSDOSerializer_Test.save_to_file_changesummary_prop_list_char();
const
  LIST_PROP_NAME = s_list_char;
  LIST_PROP_TYPE = CharacterType;
  FILE_NAME = 'changesummary_prop_list_char.xml';
var
  locFac : ISDODataFactory;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  localFileName := sdoExpandLocalFileName(FILE_NAME);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,LIST_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LIST_PROP_TYPE],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setByte(s_number,123);
    ls := locDep.getList(LIST_PROP_NAME);
      ls.append(TSDOChar('k'));
      ls.append(TSDOChar('y'));
      ls.append(TSDOChar('g'));
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setCharacter(0,TSDOChar('j'));
    ls.append(TSDOChar('a'));
    ls.append(TSDOChar('x'));
    ls.setCharacter(1,TSDOChar('v'));
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, TSDOChar('A'));
    ls.append(TSDOChar('Z'));
{$IFDEF TEST_GENERATE_FILE}
  (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create()) as ISDOSerializer).save(locDep,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}
  
  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  s.save(s_DepartmentType,locDep,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + FILE_NAME));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_LONG}
procedure TSDOSerializer_Test.save_to_file_changesummary_prop_list_long();
const
  LIST_PROP_NAME = s_list_long;
  LIST_PROP_TYPE = LongType;
  FILE_NAME = 'changesummary_prop_list_long.xml';
var
  locFac : ISDODataFactory;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  localFileName := sdoExpandLocalFileName(FILE_NAME);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,LIST_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LIST_PROP_TYPE],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setByte(s_number,123);
    ls := locDep.getList(LIST_PROP_NAME);
      ls.append(TSDOLong(11111111111111111));
      ls.append(TSDOLong(-2222222222222222));
      ls.append(TSDOLong(333333333333333333));
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setLong(0,4444444444444444444);
    ls.append(TSDOLong(5555555555555555555));
    ls.append(TSDOLong(-6666666666666666666));
    ls.setLong(1,7777777777777777777);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, TSDOLong(8));
    ls.append(TSDOLong(-9));
{$IFDEF TEST_GENERATE_FILE}
  (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create()) as ISDOSerializer).save(locDep,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}

  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  s.save(s_DepartmentType,locDep,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + FILE_NAME));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure TSDOSerializer_Test.save_to_file_changesummary_prop_list_short();
const
  LIST_PROP_NAME = s_list_short;
  LIST_PROP_TYPE = ShortType;
  FILE_NAME = 'changesummary_prop_list_short.xml';
var
  locFac : ISDODataFactory;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  localFileName := sdoExpandLocalFileName(FILE_NAME);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,LIST_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LIST_PROP_TYPE],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setByte(s_number,123);
    ls := locDep.getList(LIST_PROP_NAME);
      ls.append(TSDOShort(1));
      ls.append(TSDOShort(2));
      ls.append(TSDOShort(3));
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setShort(0,10);
    ls.append(TSDOShort(12345));
    ls.append(TSDOShort(-5245));
    ls.setShort(1,20);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, TSDOShort(107));
    ls.append(TSDOShort(89));
{$IFDEF TEST_GENERATE_FILE}
  (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create()) as ISDOSerializer).save(locDep,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}
  
  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  s.save(s_DepartmentType,locDep,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + FILE_NAME));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;
{$ENDIF HAS_SDO_SHORT}

{$IFDEF HAS_SDO_CURRENCY}
procedure TSDOSerializer_Test.save_to_file_changesummary_prop_list_currency();
const
  LIST_PROP_NAME = s_list_currency;
  LIST_PROP_TYPE = CurrencyType;
  FILE_NAME = 'changesummary_prop_list_currency.xml';
var
  locFac : ISDODataFactory;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  localFileName := sdoExpandLocalFileName(FILE_NAME);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,LIST_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LIST_PROP_TYPE],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setByte(s_number,123);
    ls := locDep.getList(LIST_PROP_NAME);
      ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[0]);
      ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[1]);
      ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[2]);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setCurrency(0,CURRENCY_VALUES_REPEATED_DIGITED[3]);
    ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[4]);
    ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[5]);
    ls.setCurrency(1,CURRENCY_VALUES_REPEATED_DIGITED[6]);
    ls.delete(0);
    ls.delete(1);
    ls.insertCurrency(2, CURRENCY_VALUES_REPEATED_DIGITED[7]);
    ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[8]);
{$IFDEF TEST_GENERATE_FILE}
  (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create()) as ISDOSerializer).save(locDep,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}

  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  s.save(s_DepartmentType,locDep,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + FILE_NAME));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;

procedure TSDOSerializer_Test.load_from_file_changesummary_currency();
const
  PROP_NAME = 'sampleProperty';
  PROP_TYPE = CurrencyType;
  FILE_NAME = 'change_summary_currency.xml';
  VAL_1 : TSDOCurrency = 12398745632145.6987;
  VAL_2 : TSDOCurrency = -45821568.7422;
var
  locFac, tmpFactory : ISDODataFactory;
  locEmployee : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  ol : ISDODataObjectList;
begin
  localFileName := sdoExpandLocalFileName(TestFilesPath + FILE_NAME);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_Employee,PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[]);
    locFac.addProperty(s_uri,s_Employee,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locEmployee := locFac.createNew(s_uri,s_Employee);
    locEmployee.setCurrency(PROP_NAME,VAL_1);
    locCS := locEmployee.getChangeSummary();

  locCS.beginLogging();     
    locEmployee.setCurrency(PROP_NAME,VAL_2);
{$IFDEF TEST_GENERATE_FILE}
  (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create())as ISDOSerializer).save(locEmployee,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}  

  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDODataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  ol := TSDODataObjectList.Create(tmpFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType]));
  s.load(localFileName,ol);
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
    Compare(
      locEmployee.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      ol.getDataObject(0).getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    locEmployee.getChangeSummary().undoChanges();
    ol.getDataObject(0).getChangeSummary().undoChanges();
      CheckEquals(0, locEmployee.getChangeSummary().getChangedDataObjects().size());
      CheckEquals(0, ol.getDataObject(0).getChangeSummary().getChangedDataObjects().size());
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
end;

procedure TSDOSerializer_Test.load_from_file_changesummary_prop_list_currency();
const
  PROP_NAME = s_list_currency;
  PROP_TYPE = CurrencyType;
  FILE_NAME = 'changesummary_prop_list_currency.xml';
var
  locFac, tmpFactory : ISDODataFactory;
  locDep, locLoadedDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setByte(s_number,123);
    ls := locDep.getList(PROP_NAME);
      ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[0]);
      ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[1]);
      ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[2]);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setCurrency(0,CURRENCY_VALUES_REPEATED_DIGITED[3]);
    ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[4]);
    ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[5]);
    ls.setCurrency(1,CURRENCY_VALUES_REPEATED_DIGITED[6]);
    ls.delete(0);
    ls.delete(1);
    ls.insertCurrency(2, CURRENCY_VALUES_REPEATED_DIGITED[7]);
    ls.appendCurrency(CURRENCY_VALUES_REPEATED_DIGITED[8]);
{$IFDEF TEST_GENERATE_FILE}
  (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create())as ISDOSerializer).save(locDep,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}  
    
  localFileName := sdoExpandLocalFileName(TestFilesPath + FILE_NAME); 
  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDOBaseDataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  locLoadedDep := s.load(localFileName);

  Check(TSDOEqualityHelper.equal(locDep, locLoadedDep),'Object');
  Compare(
    locDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
    locLoadedDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
  );
end;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
procedure TSDOSerializer_Test.save_to_file_changesummary_prop_list_double();
const
  LIST_PROP_NAME = s_list_double;
  LIST_PROP_TYPE = DoubleType;
  FILE_NAME = 'changesummary_prop_list_double.xml';
var
  locFac : ISDODataFactory;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  localFileName := sdoExpandLocalFileName(FILE_NAME);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,LIST_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LIST_PROP_TYPE],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setByte(s_number,123);
    ls := locDep.getList(LIST_PROP_NAME);
      ls.append(DOUBLE_VALUES_REPEATED_DIGITED[0]);
      ls.append(DOUBLE_VALUES_REPEATED_DIGITED[1]);
      ls.append(DOUBLE_VALUES_REPEATED_DIGITED[2]);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setDouble(0,DOUBLE_VALUES_REPEATED_DIGITED[3]);
    ls.append(DOUBLE_VALUES_REPEATED_DIGITED[4]);
    ls.append(DOUBLE_VALUES_REPEATED_DIGITED[5]);
    ls.setDouble(1,DOUBLE_VALUES_REPEATED_DIGITED[6]);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, DOUBLE_VALUES_REPEATED_DIGITED[7]);
    ls.append(DOUBLE_VALUES_REPEATED_DIGITED[8]);
{$IFDEF TEST_GENERATE_FILE}
  (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create()) as ISDOSerializer).save(locDep,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}

  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  s.save(s_DepartmentType,locDep,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + FILE_NAME));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;

procedure TSDOSerializer_Test.load_from_file_changesummary_double();
const
  PROP_NAME = 'sampleProperty';
  PROP_TYPE = DoubleType;
  FILE_NAME = 'change_summary_double.xml';
  VAL_1 : TSDODouble = 1239874567;
  VAL_2 : TSDODouble = -4582152;
var
  locFac, tmpFactory : ISDODataFactory;
  locEmployee : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  ol : ISDODataObjectList;
begin
  localFileName := sdoExpandLocalFileName(TestFilesPath + FILE_NAME);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_Employee,PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[]);
    locFac.addProperty(s_uri,s_Employee,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locEmployee := locFac.createNew(s_uri,s_Employee);
    locEmployee.setDouble(PROP_NAME,VAL_1);
    locCS := locEmployee.getChangeSummary();

  locCS.beginLogging();     
    locEmployee.setDouble(PROP_NAME,VAL_2);
{$IFDEF TEST_GENERATE_FILE}
  (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create())as ISDOSerializer).save(locEmployee,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}  

  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDODataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  ol := TSDODataObjectList.Create(tmpFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType]));
  s.load(localFileName,ol);
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
    Compare(
      locEmployee.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      ol.getDataObject(0).getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    locEmployee.getChangeSummary().undoChanges();
    ol.getDataObject(0).getChangeSummary().undoChanges();
      CheckEquals(0, locEmployee.getChangeSummary().getChangedDataObjects().size());
      CheckEquals(0, ol.getDataObject(0).getChangeSummary().getChangedDataObjects().size());
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
end;

procedure TSDOSerializer_Test.load_from_file_changesummary_prop_list_double();
const
  PROP_NAME = s_list_double;
  PROP_TYPE = DoubleType;
  FILE_NAME = 'changesummary_prop_list_double.xml';
var
  locFac, tmpFactory : ISDODataFactory;
  locDep, locLoadedDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setByte(s_number,123);
    ls := locDep.getList(PROP_NAME);
      ls.append(DOUBLE_VALUES_REPEATED_DIGITED[0]);
      ls.append(DOUBLE_VALUES_REPEATED_DIGITED[1]);
      ls.append(DOUBLE_VALUES_REPEATED_DIGITED[2]);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setDouble(0,DOUBLE_VALUES_REPEATED_DIGITED[3]);
    ls.append(DOUBLE_VALUES_REPEATED_DIGITED[4]);
    ls.append(DOUBLE_VALUES_REPEATED_DIGITED[5]);
    ls.setDouble(1,DOUBLE_VALUES_REPEATED_DIGITED[6]);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, DOUBLE_VALUES_REPEATED_DIGITED[7]);
    ls.append(DOUBLE_VALUES_REPEATED_DIGITED[8]);
{$IFDEF TEST_GENERATE_FILE}
  (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create())as ISDOSerializer).save(locDep,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}  
    
  localFileName := sdoExpandLocalFileName(TestFilesPath + FILE_NAME); 
  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDOBaseDataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  locLoadedDep := s.load(localFileName);

  Check(TSDOEqualityHelper.equal(locDep, locLoadedDep),'Object');
  Compare(
    locDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
    locLoadedDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
  );
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
procedure TSDOSerializer_Test.save_to_file_changesummary_prop_list_float();
const
  LIST_PROP_NAME = s_list_float;
  LIST_PROP_TYPE = FloatType;
  FILE_NAME = 'changesummary_prop_list_float.xml';
var
  locFac : ISDODataFactory;
  locDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  existDoc, serialDoc : TSDOXMLDocument;
begin
  localFileName := sdoExpandLocalFileName(FILE_NAME);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,LIST_PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[LIST_PROP_TYPE],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setByte(s_number,123);
    ls := locDep.getList(LIST_PROP_NAME);
      ls.append(FLOAT_VALUES_REPEATED_DIGITED[0]);
      ls.append(FLOAT_VALUES_REPEATED_DIGITED[1]);
      ls.append(FLOAT_VALUES_REPEATED_DIGITED[2]);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setFloat(0,FLOAT_VALUES_REPEATED_DIGITED[3]);
    ls.append(FLOAT_VALUES_REPEATED_DIGITED[4]);
    ls.append(FLOAT_VALUES_REPEATED_DIGITED[5]);
    ls.setFloat(1,FLOAT_VALUES_REPEATED_DIGITED[6]);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, FLOAT_VALUES_REPEATED_DIGITED[7]);
    ls.append(FLOAT_VALUES_REPEATED_DIGITED[8]);
{$IFDEF TEST_GENERATE_FILE}
  (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create()) as ISDOSerializer).save(locDep,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}

  f := TSDOSerializerStreamXML.Create();
  s := TSDOSerializer.Create(locFac,f);
  s.save(s_DepartmentType,locDep,localFileName);

  existDoc := nil;
  serialDoc := nil;
  ReadXMLFile(existDoc,sdoExpandLocalFileName(TestFilesPath + FILE_NAME));
  try
    ReadXMLFile(serialDoc,localFileName);
    CheckEquals(True, CompareNodes(existDoc,serialDoc));
  finally
    ReleaseDomNode(existDoc);
    ReleaseDomNode(serialDoc);
  end;
end;

procedure TSDOSerializer_Test.load_from_file_changesummary_float();
const
  PROP_NAME = 'sampleProperty';
  PROP_TYPE = FloatType;
  FILE_NAME = 'change_summary_float.xml';
  VAL_1 : TSDOFloat = 1239874567;
  VAL_2 : TSDOFloat = -4582152;
var
  locFac, tmpFactory : ISDODataFactory;
  locEmployee : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  ol : ISDODataObjectList;
begin
  localFileName := sdoExpandLocalFileName(TestFilesPath + FILE_NAME);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_Employee,PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[]);
    locFac.addProperty(s_uri,s_Employee,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locEmployee := locFac.createNew(s_uri,s_Employee);
    locEmployee.setFloat(PROP_NAME,VAL_1);
    locCS := locEmployee.getChangeSummary();

  locCS.beginLogging();     
    locEmployee.setFloat(PROP_NAME,VAL_2);
{$IFDEF TEST_GENERATE_FILE}
  (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create())as ISDOSerializer).save(locEmployee,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}  

  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDODataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  ol := TSDODataObjectList.Create(tmpFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType]));
  s.load(localFileName,ol);
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
    Compare(
      locEmployee.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      ol.getDataObject(0).getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    locEmployee.getChangeSummary().undoChanges();
    ol.getDataObject(0).getChangeSummary().undoChanges();
      CheckEquals(0, locEmployee.getChangeSummary().getChangedDataObjects().size());
      CheckEquals(0, ol.getDataObject(0).getChangeSummary().getChangedDataObjects().size());
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
end;

procedure TSDOSerializer_Test.load_from_file_changesummary_prop_list_float();
const
  PROP_NAME = s_list_float;
  PROP_TYPE = FloatType;
  FILE_NAME = 'changesummary_prop_list_float.xml';
var
  locFac, tmpFactory : ISDODataFactory;
  locDep, locLoadedDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setByte(s_number,123);
    ls := locDep.getList(PROP_NAME);
      ls.append(FLOAT_VALUES_REPEATED_DIGITED[0]);
      ls.append(FLOAT_VALUES_REPEATED_DIGITED[1]);
      ls.append(FLOAT_VALUES_REPEATED_DIGITED[2]);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setFloat(0,FLOAT_VALUES_REPEATED_DIGITED[3]);
    ls.append(FLOAT_VALUES_REPEATED_DIGITED[4]);
    ls.append(FLOAT_VALUES_REPEATED_DIGITED[5]);
    ls.setFloat(1,FLOAT_VALUES_REPEATED_DIGITED[6]);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, FLOAT_VALUES_REPEATED_DIGITED[7]);
    ls.append(FLOAT_VALUES_REPEATED_DIGITED[8]);
{$IFDEF TEST_GENERATE_FILE}
  (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create())as ISDOSerializer).save(locDep,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}  
    
  localFileName := sdoExpandLocalFileName(TestFilesPath + FILE_NAME); 
  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDOBaseDataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  locLoadedDep := s.load(localFileName);

  Check(TSDOEqualityHelper.equal(locDep, locLoadedDep),'Object');
  Compare(
    locDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
    locLoadedDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
  );
end;
{$ENDIF HAS_SDO_FLOAT}

procedure TSDOSerializer_Test.load_from_file_changesummary_bool();
const
  PROP_NAME = 'sampleProperty';
  PROP_TYPE = BooleanType;
  VAL_1 : TSDOBoolean = True;
  VAL_2 : TSDOBoolean = False;
var
  locFac, tmpFactory : ISDODataFactory;
  locEmployee : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  ol : ISDODataObjectList;
begin
  localFileName := sdoExpandLocalFileName(TestFilesPath + 'change_summary_bool.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_Employee,PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[]);
    locFac.addProperty(s_uri,s_Employee,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locEmployee := locFac.createNew(s_uri,s_Employee);
    locEmployee.setBoolean(PROP_NAME,VAL_1);
    locCS := locEmployee.getChangeSummary();

  locCS.beginLogging();
    locEmployee.setBoolean(PROP_NAME,VAL_2);
  //(TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create())as ISDOSerializer).save(locEmployee,sdoExpandLocalFileName('change_summary_bool.xml'));

  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDODataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  ol := TSDODataObjectList.Create(tmpFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType]));
  s.load(localFileName,ol);
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
    Compare(
      locEmployee.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      ol.getDataObject(0).getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    locEmployee.getChangeSummary().undoChanges();
    ol.getDataObject(0).getChangeSummary().undoChanges();
      CheckEquals(0, locEmployee.getChangeSummary().getChangedDataObjects().size());
      CheckEquals(0, ol.getDataObject(0).getChangeSummary().getChangedDataObjects().size());
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
end;

procedure TSDOSerializer_Test.load_from_file_changesummary_byte();
const
  PROP_NAME = 'sampleProperty';
  PROP_TYPE = ByteType;
  FILE_NAME = 'change_summary_byte.xml';
  VAL_1 : TSDOByte = 200;
  VAL_2 : TSDOByte = 123;
var
  locFac, tmpFactory : ISDODataFactory;
  locEmployee : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  ol : ISDODataObjectList;
begin
  localFileName := sdoExpandLocalFileName(TestFilesPath + FILE_NAME);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_Employee,PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[]);
    locFac.addProperty(s_uri,s_Employee,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locEmployee := locFac.createNew(s_uri,s_Employee);
    locEmployee.setByte(PROP_NAME,VAL_1);
    locCS := locEmployee.getChangeSummary();

  locCS.beginLogging();     
    locEmployee.setByte(PROP_NAME,VAL_2);
{$IFDEF TEST_GENERATE_FILE}
  (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create())as ISDOSerializer).save(locEmployee,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}  

  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDODataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  ol := TSDODataObjectList.Create(tmpFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType]));
  s.load(localFileName,ol);
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
    Compare(
      locEmployee.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      ol.getDataObject(0).getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    locEmployee.getChangeSummary().undoChanges();
    ol.getDataObject(0).getChangeSummary().undoChanges();
      CheckEquals(0, locEmployee.getChangeSummary().getChangedDataObjects().size());
      CheckEquals(0, ol.getDataObject(0).getChangeSummary().getChangedDataObjects().size());
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
end;

{$IFDEF HAS_SDO_CHAR}
procedure TSDOSerializer_Test.load_from_file_changesummary_char();
const
  PROP_NAME = 'sampleProperty';
  PROP_TYPE = CharacterType;
  FILE_NAME = 'change_summary_char.xml';
  VAL_1 : TSDOChar = 'a';
  VAL_2 : TSDOChar = 'z';
var
  locFac, tmpFactory : ISDODataFactory;
  locEmployee : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  ol : ISDODataObjectList;
begin
  localFileName := sdoExpandLocalFileName(TestFilesPath + FILE_NAME);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_Employee,PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[]);
    locFac.addProperty(s_uri,s_Employee,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locEmployee := locFac.createNew(s_uri,s_Employee);
    locEmployee.setCharacter(PROP_NAME,VAL_1);
    locCS := locEmployee.getChangeSummary();

  locCS.beginLogging();     
    locEmployee.setCharacter(PROP_NAME,VAL_2);
{$IFDEF TEST_GENERATE_FILE}
  (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create())as ISDOSerializer).save(locEmployee,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}  

  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDODataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  ol := TSDODataObjectList.Create(tmpFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType]));
  s.load(localFileName,ol);
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
    Compare(
      locEmployee.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      ol.getDataObject(0).getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    locEmployee.getChangeSummary().undoChanges();
    ol.getDataObject(0).getChangeSummary().undoChanges();
      CheckEquals(0, locEmployee.getChangeSummary().getChangedDataObjects().size());
      CheckEquals(0, ol.getDataObject(0).getChangeSummary().getChangedDataObjects().size());
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
end;
{$ENDIF HAS_SDO_CHAR}

procedure TSDOSerializer_Test.load_from_file_changesummary_date();
const
  PROP_NAME = 'sampleProperty';
  PROP_TYPE = DateTimeType;
  VAL_1 : TSDODate = ( Date : 45123; HourOffset : 0; MinuteOffset : 5; );
  VAL_2 : TSDODate = ( Date : 39000; HourOffset : -8; MinuteOffset : 0; );
var
  locFac, tmpFactory : ISDODataFactory;
  locEmployee : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  ol : ISDODataObjectList;
begin
  localFileName := sdoExpandLocalFileName(TestFilesPath + 'change_summary_date.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_Employee,PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[]);
    locFac.addProperty(s_uri,s_Employee,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locEmployee := locFac.createNew(s_uri,s_Employee);
    locEmployee.setDate(PROP_NAME,VAL_1);
    locCS := locEmployee.getChangeSummary();

  locCS.beginLogging();
    locEmployee.setDate(PROP_NAME,VAL_2);

  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDODataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  ol := TSDODataObjectList.Create(tmpFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType]));
  s.load(localFileName,ol);
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
    Compare(
      locEmployee.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      ol.getDataObject(0).getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    locEmployee.getChangeSummary().undoChanges();
    ol.getDataObject(0).getChangeSummary().undoChanges();
      CheckEquals(0, locEmployee.getChangeSummary().getChangedDataObjects().size());
      CheckEquals(0, ol.getDataObject(0).getChangeSummary().getChangedDataObjects().size());
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
end;

procedure TSDOSerializer_Test.load_from_file_changesummary_integer();
const
  PROP_NAME = 'sampleProperty';
  PROP_TYPE = IntegerType;
  VAL_1 : TSDOInteger = 12345;
  VAL_2 : TSDOInteger = -789;
var
  locFac, tmpFactory : ISDODataFactory;
  locEmployee : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  ol : ISDODataObjectList;
begin
  localFileName := sdoExpandLocalFileName(TestFilesPath + 'change_summary_integer.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_Employee,PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[]);
    locFac.addProperty(s_uri,s_Employee,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locEmployee := locFac.createNew(s_uri,s_Employee);
    locEmployee.setInteger(PROP_NAME,VAL_1);
    locCS := locEmployee.getChangeSummary();

  locCS.beginLogging();
    locEmployee.setInteger(PROP_NAME,VAL_2);
  //(TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create())as ISDOSerializer).save(locEmployee,sdoExpandLocalFileName('change_summary_integer.xml'));

  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDODataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  ol := TSDODataObjectList.Create(tmpFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType]));
  s.load(localFileName,ol);
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
    Compare(
      locEmployee.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      ol.getDataObject(0).getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    locEmployee.getChangeSummary().undoChanges();
    ol.getDataObject(0).getChangeSummary().undoChanges();
      CheckEquals(0, locEmployee.getChangeSummary().getChangedDataObjects().size());
      CheckEquals(0, ol.getDataObject(0).getChangeSummary().getChangedDataObjects().size());
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
end;

{$IFDEF HAS_SDO_LONG}
procedure TSDOSerializer_Test.load_from_file_changesummary_long();
const
  PROP_NAME = 'sampleProperty';
  PROP_TYPE = LongType;
  FILE_NAME = 'change_summary_long.xml';
  VAL_1 : TSDOLong = 123987456321456987;
  VAL_2 : TSDOLong = -458215687422;
var
  locFac, tmpFactory : ISDODataFactory;
  locEmployee : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  ol : ISDODataObjectList;
begin
  localFileName := sdoExpandLocalFileName(TestFilesPath + FILE_NAME);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_Employee,PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[]);
    locFac.addProperty(s_uri,s_Employee,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locEmployee := locFac.createNew(s_uri,s_Employee);
    locEmployee.setLong(PROP_NAME,VAL_1);
    locCS := locEmployee.getChangeSummary();

  locCS.beginLogging();     
    locEmployee.setLong(PROP_NAME,VAL_2);
{$IFDEF TEST_GENERATE_FILE}
  (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create())as ISDOSerializer).save(locEmployee,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}  

  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDODataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  ol := TSDODataObjectList.Create(tmpFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType]));
  s.load(localFileName,ol);
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
    Compare(
      locEmployee.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      ol.getDataObject(0).getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    locEmployee.getChangeSummary().undoChanges();
    ol.getDataObject(0).getChangeSummary().undoChanges();
      CheckEquals(0, locEmployee.getChangeSummary().getChangedDataObjects().size());
      CheckEquals(0, ol.getDataObject(0).getChangeSummary().getChangedDataObjects().size());
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure TSDOSerializer_Test.load_from_file_changesummary_short();
const
  PROP_NAME = 'sampleProperty';
  PROP_TYPE = ShortType;
  FILE_NAME = 'change_summary_short.xml';
  VAL_1 : TSDOShort = 1592;
  VAL_2 : TSDOShort = -9876;
var
  locFac, tmpFactory : ISDODataFactory;
  locEmployee : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  ol : ISDODataObjectList;
begin
  localFileName := sdoExpandLocalFileName(TestFilesPath + FILE_NAME);
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_Employee,PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[]);
    locFac.addProperty(s_uri,s_Employee,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locEmployee := locFac.createNew(s_uri,s_Employee);
    locEmployee.setShort(PROP_NAME,VAL_1);
    locCS := locEmployee.getChangeSummary();

  locCS.beginLogging();     
    locEmployee.setShort(PROP_NAME,VAL_2);
{$IFDEF TEST_GENERATE_FILE}
  (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create())as ISDOSerializer).save(locEmployee,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}  

  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDODataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  ol := TSDODataObjectList.Create(tmpFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType]));
  s.load(localFileName,ol);
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
    Compare(
      locEmployee.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      ol.getDataObject(0).getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    locEmployee.getChangeSummary().undoChanges();
    ol.getDataObject(0).getChangeSummary().undoChanges();
      CheckEquals(0, locEmployee.getChangeSummary().getChangedDataObjects().size());
      CheckEquals(0, ol.getDataObject(0).getChangeSummary().getChangedDataObjects().size());
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
end;
{$ENDIF HAS_SDO_SHORT}

procedure TSDOSerializer_Test.load_from_file_changesummary_string();
const
  PROP_NAME = 'sampleProperty';
  PROP_TYPE = StringType;
  VAL_1 : TSDOString = 'Inoussa.O';
  VAL_2 : TSDOString = 'sdo.fpc.delphi';
var
  locFac, tmpFactory : ISDODataFactory;
  locEmployee : ISDODataObject;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
  ol : ISDODataObjectList;
begin
  localFileName := sdoExpandLocalFileName(TestFilesPath + 'change_summary_string.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_Employee,[]);
    locFac.addProperty(s_uri,s_Employee,PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[]);
    locFac.addProperty(s_uri,s_Employee,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locEmployee := locFac.createNew(s_uri,s_Employee);
    locEmployee.setString(PROP_NAME,VAL_1);
    locCS := locEmployee.getChangeSummary();

  locCS.beginLogging();
    locEmployee.setString(PROP_NAME,VAL_2);
  //(TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create())as ISDOSerializer).save(locEmployee,sdoExpandLocalFileName('change_summary_string.xml'));

  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDODataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  ol := TSDODataObjectList.Create(tmpFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType]));
  s.load(localFileName,ol);
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
    Compare(
      locEmployee.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
      ol.getDataObject(0).getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
    );
    locEmployee.getChangeSummary().undoChanges();
    ol.getDataObject(0).getChangeSummary().undoChanges();
      CheckEquals(0, locEmployee.getChangeSummary().getChangedDataObjects().size());
      CheckEquals(0, ol.getDataObject(0).getChangeSummary().getChangedDataObjects().size());
    Check(TSDOEqualityHelper.equal(locEmployee,ol.getDataObject(0)));
end;

procedure TSDOSerializer_Test.load_from_file_changesummary_prop_list_byte();
const
  PROP_NAME = s_list_byte;
  PROP_TYPE = ByteType;
  FILE_NAME = 'changesummary_prop_list_byte.xml';
var
  locFac, tmpFactory : ISDODataFactory;
  locDep, locLoadedDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setByte(s_number,123);
    ls := locDep.getList(PROP_NAME);
      ls.append(TSDOByte(1));
      ls.append(TSDOByte(2));
      ls.append(TSDOByte(3));
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setByte(0,10);
    ls.append(TSDOByte(123));
    ls.append(TSDOByte(45));
    ls.setByte(1,20);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, TSDOByte(107));
    ls.append(TSDOByte(89));
{$IFDEF TEST_GENERATE_FILE}
  (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create())as ISDOSerializer).save(locDep,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}  
    
  localFileName := sdoExpandLocalFileName(TestFilesPath + FILE_NAME); 
  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDOBaseDataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  locLoadedDep := s.load(localFileName);

  Check(TSDOEqualityHelper.equal(locDep, locLoadedDep),'Object');
  Compare(
    locDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
    locLoadedDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
  );
end;

{$IFDEF HAS_SDO_CHAR}
procedure TSDOSerializer_Test.load_from_file_changesummary_prop_list_char();
const
  PROP_NAME = s_list_char;
  PROP_TYPE = CharacterType;
  FILE_NAME = 'changesummary_prop_list_char.xml';
var
  locFac, tmpFactory : ISDODataFactory;
  locDep, locLoadedDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
begin
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_number,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,PROP_NAME,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setByte(s_number,123);
    ls := locDep.getList(PROP_NAME);
      ls.append(TSDOChar('k'));
      ls.append(TSDOChar('y'));
      ls.append(TSDOChar('g'));
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setCharacter(0,TSDOChar('j'));
    ls.append(TSDOChar('a'));
    ls.append(TSDOChar('x'));
    ls.setCharacter(1,TSDOChar('v'));
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, TSDOChar('A'));
    ls.append(TSDOChar('Z'));
{$IFDEF TEST_GENERATE_FILE}
  (TSDOSerializer.Create(locFac,TSDOSerializerStreamXML.Create())as ISDOSerializer).save(locDep,sdoExpandLocalFileName(FILE_NAME));
{$ENDIF TEST_GENERATE_FILE}  
    
  localFileName := sdoExpandLocalFileName(TestFilesPath + FILE_NAME); 
  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDOBaseDataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  locLoadedDep := s.load(localFileName);

  Check(TSDOEqualityHelper.equal(locDep, locLoadedDep),'Object');
  Compare(
    locDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
    locLoadedDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
  );
end;
{$ENDIF HAS_SDO_CHAR}

procedure TSDOSerializer_Test.load_from_file_changesummary_prop_list_date();
const VAL_1 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_2 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_3 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_4 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_5 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );

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

    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(2009,9,1,2,3,0,1);
    d.HourOffset := 0;
    d.MinuteOffset := 13;
    PSDODate(@VAL_4)^ := d;

    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(1900,11,8,1,2,0,0);
    d.HourOffset := 0;
    d.MinuteOffset := 13;
    PSDODate(@VAL_5)^ := d;
  end;

var
  locFac, tmpFactory : ISDODataFactory;
  locDep, locLoadedDep : ISDODataObject;
  ls : ISDODataObjectList;
  locCS : ISDOChangeSummary;
  f : ISDOSerializerStream;
  s : ISDOSerializer;
  localFileName : string;
begin
  SetConstants();
  localFileName := sdoExpandLocalFileName('changesummary_prop_list_date.xml');
  locFac := TSDODataFactory.Create() as ISDODataFactory;
  locFac.AddType(s_uri,s_DepartmentType,[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_location,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_birthDate,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType],[]);
    locFac.addProperty(s_uri,s_DepartmentType,s_list_date,sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType],[pfIsMany]);
    locFac.addProperty(s_uri,s_DepartmentType,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly,pfIsNotNullable]);

  locDep := locFac.createNew(s_uri,s_DepartmentType);
    locDep.setString(s_name,'R & D');
    locDep.setString(s_location,'Ouaga, BF');
    locDep.setDate(s_birthDate,VAL_1);
    ls := locDep.getList(s_list_date);
      ls.append(VAL_1);
      ls.append(VAL_2);
      ls.append(VAL_3);
    locCS := locDep.getChangeSummary();

  locCS.beginLogging();
    locDep.setString(s_name,'R & D Department');
    locDep.setString(s_location,'Ouaga 01, BF');
    ls.setDate(0,VAL_4);
    ls.append(VAL_5);
    ls.append(VAL_1);
    ls.setDate(1,VAL_2);
    ls.delete(0);
    ls.delete(1);
    ls.insert(2, VAL_3);
    ls.append(VAL_4);

  localFileName := sdoExpandLocalFileName(TestFilesPath + 'changesummary_prop_list_date.xml'); 
  f := TSDOSerializerStreamXML.Create();
  tmpFactory := TSDOBaseDataFactory.Create();
  s := TSDOSerializer.Create(tmpFactory,f);
  locLoadedDep := s.load(localFileName);

  Check(TSDOEqualityHelper.equal(locDep, locLoadedDep),'Object');
  Compare(
    locDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx,
    locLoadedDep.getChangeSummary().getChangedDataObjects() as ISDOChangedDataObjectListEx
  );
end;

{ TSDOSerializerXML_Test }

class function TSDOSerializerXML_Test.CreateSerializerStream: ISDOSerializerStream;
begin
  Result := TSDOSerializerStreamXML.Create();
end;

initialization
  RegisterTest('Helpers',TSDOSerializerXML_Test.Suite);
  RegisterTest('Helpers',TSDOSerializerBinary_Test.Suite);

end.
