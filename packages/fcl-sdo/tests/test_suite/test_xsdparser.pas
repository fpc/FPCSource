{$INCLUDE sdo_global.inc}
unit test_xsdparser;

interface        

uses
  Classes, SysUtils,
{$IFDEF FPC}
  fpcunit, testutils, testregistry, DOM, XmlRead, sdo_fpc_xml,
{$ELSE}
  TestFrameWork, xmldom, sdo_win_xml,
{$ENDIF}
  sdo, sdo_types, sdo_xsdparser;

type

  { TTest_CustomXsdParser }

  TTest_CustomXsdParser = class(TTestCase)
  protected
    function LoadEmptySchema(var AFactory : ISDODataFactory) : ISDODataObject;virtual;abstract;
    function LoadSimpleType_Enum_Schema(var AFactory : ISDODataFactory) : ISDODataObject;virtual;abstract;
    function LoadSimpleType_Enum_Embedded_Schema(var AFactory : ISDODataFactory) : ISDODataObject;virtual;abstract;
    function LoadSimpleType_AliasToNativeType_Schema(var AFactory : ISDODataFactory) : ISDODataObject;virtual;abstract;
    
    function LoadComplexType_Class_Schema(var AFactory : ISDODataFactory) : ISDODataObject;virtual;abstract;
    function LoadComplexType_Class_default_values(var AFactory : ISDODataFactory) : ISDODataObject;virtual;abstract;
    function LoadComplexType_Class_properties_extended_metadata(var AFactory : ISDODataFactory) : ISDODataObject;virtual;abstract;
    function LoadComplexType_Class_properties_extended_metadata2(var AFactory : ISDODataFactory) : ISDODataObject;virtual;abstract;
    function LoadComplexType_Class_Embedded_Schema(var AFactory : ISDODataFactory) : ISDODataObject;virtual;abstract;
    function LoadComplexType_Class_Extend_Simple_Schema(var AFactory : ISDODataFactory) : ISDODataObject;virtual;abstract;
    function LoadComplexType_Class_OpenType(var AFactory : ISDODataFactory) : ISDODataObject;virtual;abstract;
    function LoadComplexType_Class_FalseArray(var AFactory : ISDODataFactory) : ISDODataObject;virtual;abstract;

    function LoadComplexType_ArraySequence_Schema(var AFactory : ISDODataFactory) : ISDODataObject;virtual;abstract;
    function LoadComplexType_ArraySequence_ItemName_Schema(var AFactory : ISDODataFactory) : ISDODataObject;virtual;abstract;
    function LoadComplexType_ArraySequence_Embedded_Schema(var AFactory : ISDODataFactory) : ISDODataObject;virtual;abstract;
    //function LoadComplexType_Array_soaparray() : ISDODataObject;virtual;abstract;

    function load_class_property_composed_name(var AFactory : ISDODataFactory) : ISDODataObject;virtual;abstract;

    function load_schema_import(var AFactory : ISDODataFactory) : ISDODataObject;virtual;abstract;
  published
    procedure EmptySchema();

    procedure SimpleType_Enum();
    procedure SimpleType_Enum_Embedded();
    procedure SimpleType_AliasToNativeType();

    procedure ComplexType_Class();
    procedure ComplexType_Class_default_values();
    procedure ComplexType_Class_properties_extended_metadata();
    procedure ComplexType_Class_properties_extended_metadata2();
    procedure ComplexType_Class_Embedded();
    procedure ComplexType_Class_Extend_Simple_Schema();
    procedure ComplexType_Class_open_type_any();
    procedure ComplexType_Class_open_extension_type_any();
    procedure ComplexType_Class_open_extension_type_anyAttribute();
    procedure ComplexType_Class_sequence_open_type_anyAttribute();
    procedure ComplexType_Class_all_open_type_anyAttribute();
    procedure ComplexType_Class_FalseArray();

    procedure ComplexType_ArraySequence();
    procedure ComplexType_ArraySequence_ItemName_Schema();
    procedure ComplexType_ArraySequence_Embedded();
    //procedure ComplexType_Array_soaparray();

    procedure class_property_composed_name();

    procedure schema_import();      
  end;      

  { TTest_XsdParser }

  TTest_XsdParser = class(TTest_CustomXsdParser)
  private
    function ParseDoc(var AFactory : ISDODataFactory; const ADoc : string) : ISDODataObject;
  protected
    function LoadEmptySchema(var AFactory : ISDODataFactory) : ISDODataObject;override;

    function LoadSimpleType_Enum_Schema(var AFactory : ISDODataFactory) : ISDODataObject;override;
    function LoadSimpleType_Enum_Embedded_Schema(var AFactory : ISDODataFactory) : ISDODataObject;override;
    function LoadSimpleType_AliasToNativeType_Schema(var AFactory : ISDODataFactory) : ISDODataObject;override;

    function LoadComplexType_Class_Schema(var AFactory : ISDODataFactory) : ISDODataObject;override;
    function LoadComplexType_Class_default_values(var AFactory : ISDODataFactory) : ISDODataObject;override;
    function LoadComplexType_Class_properties_extended_metadata(var AFactory : ISDODataFactory) : ISDODataObject;override;
    function LoadComplexType_Class_properties_extended_metadata2(var AFactory : ISDODataFactory) : ISDODataObject;override;
    function LoadComplexType_Class_Embedded_Schema(var AFactory : ISDODataFactory) : ISDODataObject;override;
    function LoadComplexType_Class_Extend_Simple_Schema(var AFactory : ISDODataFactory) : ISDODataObject;override;
    function LoadComplexType_Class_OpenType(var AFactory : ISDODataFactory) : ISDODataObject;override;
    function LoadComplexType_Class_FalseArray(var AFactory : ISDODataFactory) : ISDODataObject;override;

    function LoadComplexType_ArraySequence_Schema(var AFactory : ISDODataFactory) : ISDODataObject;override;
    function LoadComplexType_ArraySequence_ItemName_Schema(var AFactory : ISDODataFactory) : ISDODataObject; override;
    function LoadComplexType_ArraySequence_Embedded_Schema(var AFactory : ISDODataFactory) : ISDODataObject; override;
    //function LoadComplexType_Array_soaparray() : ISDODataObject;override;

    function load_class_property_composed_name(var AFactory : ISDODataFactory) : ISDODataObject;override;

    function load_schema_import(var AFactory : ISDODataFactory) : ISDODataObject;override;  
  end;

implementation
uses
  xsd_consts, sdo_xsdintf, test_suite_utils, sdo_datafactory, sdo_locators, sdo_imp_utils;

const
  x_complexType_SampleArrayIntFieldType     = 'TArrayIntFieldType';
  x_complexType_SampleArrayItemType         = 'TArrayItemType';
  
  x_complexType_SampleCollectionComplexType = 'TComplexType';
  x_complexType_SampleCollectionCollectionComplexType = 'TCollectionComplexType';
  x_complexType_SampleCollectionItemType    = 'TCollectionItemType';

  x_complexType_SampleDerivedType           = 'TClassSampleDerivedType';
  x_complexType_SampleClassType             = 'TClassSampleType';
  x_complexType_SampleClassTypeA            = 'TClassSampleTypeA';
  x_complexType_SampleClassTypeAll          = 'TClassSampleTypeAll';
  x_complexType_SampleClass                 = 'TClassSample';

  x_complexType_SampleRecordType             = 'TRecordSampleType';
  x_complexType_SampleRecordTypeAll          = 'TRecordSampleTypeAll';
  x_complexType_SampleRecord                 = 'TRecordSample';

  x_complexType_array_sequence      = 'complex_array_sequence';
  x_complexType_array_sequence_embedded  = 'complex_array_sequence_embedded';
  x_complexType_array_sequence_collection      = 'complex_array_sequence_collection';
  x_complexType_array_soaparray      = 'complex_array_soaparray';
  
  x_complexType_class               = 'complex_class';
  x_complexType_class_default       = 'complex_class_default';
  x_complexType_class_properties_extended_metadata = 'class_properties_extended_metadata';
  x_complexType_extend_simple = 'complex_class_extend_simple';
  x_complexType_class_embedded      = 'complex_class_embedded';

  x_empty                 = 'empty';

  x_enumSample            = 'EnumSample';
  x_enumSampleType        = 'EnumSampleType';
    x_enumSampleLIST_COUNT = 7;
    x_enumSampleLIST      : array[0..( x_enumSampleLIST_COUNT - 1 )] of string = ( 'esOne', 'esTwo', 'esThree', 'begin', 'finally', 'True', 'False' );
  x_simpleTypeAliasString = 'AliasString';
  x_simpleTypeAliasInt    = 'AliasInt';
  x_simpleTypeAliasWideString = 'AliasWideString';
  x_simpleType            = 'simpletype';
  x_simpleTypeEmbedded    = 'simpletype_embedded';
  x_simpletypeNativeAlias = 'simpletypeNativeAlias';

  x_targetNamespace       = 'urn:wst-test';


  x_byteField  = 'byteField';
  x_charField  = 'charField';
  x_classField = 'classField';
  x_enumField  = 'enumField';
  x_field      = 'field';
  x_floatField = 'floatField';
  x_intField   = 'intField';
  x_longField  = 'longField';
  x_strField   = 'strField';

  x_intAtt     = 'intAtt';
  x_strAtt     = 'strAtt';

  x_Item       = 'Item';


function LoadXmlFile(const AFileName : string) : TXMLDocument;
begin
  Result := nil;
  ReadXMLFile(Result,AFileName);
end;

{ TTest_CustomXsdParser }

procedure TTest_CustomXsdParser.EmptySchema();
var
  fact :ISDODataFactory;
  tr : ISDODataObject;
  mdl : ISDODataObject;
begin
  tr := LoadEmptySchema(fact);
  mdl := FindModule(tr,x_targetNamespace);
  CheckNotNull(mdl);
  CheckEquals(x_empty,mdl.getString(s_Name));
  CheckEquals(x_targetNamespace,mdl.getString(s_NameSpace));
  CheckEquals(0,mdl.getList(s_Type).size());
end;

procedure TTest_CustomXsdParser.SimpleType_Enum();
var
  fact :ISDODataFactory;
  tr : ISDODataObject;
  mdl : ISDODataObject;
  elt : ISDODataObject;
  i : Integer;
begin
  tr := LoadSimpleType_Enum_Schema(fact);

  mdl := FindModule(tr,x_targetNamespace);
  CheckNotNull(mdl);
  CheckEquals(x_simpleType,mdl.getString(s_Name));
  CheckEquals(x_targetNamespace,mdl.getString(s_NameSpace));
  CheckEquals(1,mdl.getList(s_Type).size());
  elt := Find(tr,x_enumSampleType);
    CheckNotNull(elt,x_enumSampleType);
    CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
    CheckEquals(x_enumSampleType,elt.getString(s_Name));
    Check(elt.getList(s_EnumValue).size() > 0,'Should have enums');
    CheckEquals(x_enumSampleLIST_COUNT,elt.getList(s_EnumValue).size());
    for i := 0 to Pred(x_enumSampleLIST_COUNT) do
      CheckEquals(x_enumSampleLIST[i],elt.getList(s_EnumValue).getString(i));

  elt := Find(tr,x_enumSample);
    CheckNotNull(elt,x_enumSample);
    CheckEquals(x_enumSample,elt.getString(s_Name));
    CheckEquals(ELEMENT_KIND_VARIABLE,elt.getByte(s_ElementKind),s_ElementKind);
    CheckNotNull(elt.getDataObject(s_DataType));
    CheckEquals(x_enumSampleType, elt.getDataObject(s_DataType).getString(s_Name));
end;

procedure TTest_CustomXsdParser.SimpleType_Enum_Embedded();
var   
  fact :ISDODataFactory;
  tr : ISDODataObject;
  mdl : ISDODataObject;
  elt : ISDODataObject;
  i : Integer;
begin
  tr := LoadSimpleType_Enum_Embedded_Schema(fact);

  mdl := FindModule(tr,x_targetNamespace);
  CheckNotNull(mdl);
  CheckEquals(x_simpleTypeEmbedded,mdl.getString(s_Name));
  CheckEquals(x_targetNamespace,mdl.getString(s_NameSpace));
  CheckEquals(1,mdl.getList(s_Type).size());
  elt := Find(tr,x_enumSampleType);
    CheckNotNull(elt,x_enumSampleType);
    CheckEquals(x_enumSampleType,elt.getString(s_Name));
    CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
    Check(elt.getList(s_EnumValue).size() > 0,'Should have enums');
    CheckEquals(x_enumSampleLIST_COUNT,elt.getList(s_EnumValue).size());
    for i := 0 to Pred(x_enumSampleLIST_COUNT) do
      CheckEquals(x_enumSampleLIST[i],elt.getList(s_EnumValue).getString(i));
end;

procedure TTest_CustomXsdParser.SimpleType_AliasToNativeType();
var    
  fact :ISDODataFactory;
  tr : ISDODataObject;
  mdl : ISDODataObject;
  elt : ISDODataObject;
begin
  tr := LoadSimpleType_AliasToNativeType_Schema(fact);

  mdl := FindModule(tr,x_targetNamespace);
  CheckNotNull(mdl);
  CheckEquals(x_simpletypeNativeAlias,mdl.getString(s_Name));
  CheckEquals(x_targetNamespace,mdl.getString(s_NameSpace));
  CheckEquals(2,mdl.getList(s_Variable).size());
  elt := Find(tr,x_simpleTypeAliasString);
    CheckNotNull(elt,x_simpleTypeAliasString);
    CheckEquals(x_simpleTypeAliasString,elt.getString(s_Name));
    CheckEquals(ELEMENT_KIND_VARIABLE,elt.getByte(s_ElementKind),s_ElementKind);
    CheckNotNull(elt.getDataObject(s_DataType));
    CheckEquals('string',elt.getDataObject(s_DataType).getString(s_Name));

  elt := Find(tr,x_simpleTypeAliasInt);
    CheckNotNull(elt,x_simpleTypeAliasInt);
    CheckEquals(x_simpleTypeAliasInt,elt.getString(s_Name));
    CheckEquals(ELEMENT_KIND_VARIABLE,elt.getByte(s_ElementKind),s_ElementKind);
    CheckNotNull(elt.getDataObject(s_DataType));
    CheckEquals('int',elt.getDataObject(s_DataType).getString(s_Name));
end;

type
  TPropertyType = ( ptField, ptAttribute );
const
  PropertyType_Att : array[TPropertyType] of Boolean = ( False, True );
procedure TTest_CustomXsdParser.ComplexType_Class();
var
  tr : ISDODataObject;
  clsType : ISDODataObject;

  procedure CheckProperty(const AName,ATypeName : string; const AFieldType : TPropertyType);
  var
    prp : ISDODataObject;
  begin
    prp := clsType.getDataObject(Format('%s[%s = %s]',[s_Property,s_Name,QuotedStr(AName)]));
      CheckNotNull(prp);
      CheckEquals(AName,prp.getString(s_Name));
      CheckNotNull(prp.getDataObject(s_DataType));
      CheckEquals(ATypeName,prp.getDataObject(s_DataType).getString(s_Name));
      CheckEquals(PropertyType_Att[AFieldType],prp.getBoolean(s_IsAttribute),Format('%s.%s, %s',[clsType.getString(s_Name),AName,s_IsAttribute]));
  end;

var
  fact :ISDODataFactory;
  mdl : ISDODataObject;
  elt : ISDODataObject;
  prpLs : ISDODataObjectList;
begin
  tr := LoadComplexType_Class_Schema(fact);

  mdl := FindModule(tr,x_targetNamespace);
  CheckNotNull(mdl);
  CheckEquals(x_complexType_class,mdl.getString(s_Name));
  CheckEquals(x_targetNamespace,mdl.getString(s_NameSpace));
  CheckEquals(3,mdl.getList(s_Type).size());
  CheckEquals(1,mdl.getList(s_Variable).size());
  elt := Find(tr,x_complexType_SampleClassType);
    CheckNotNull(elt,x_complexType_SampleClassType);
    CheckEquals(x_complexType_SampleClassType,elt.getString(s_Name));
    CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
    clsType := elt;
      prpLs := clsType.getList(s_Property);
    CheckEquals(8,prpLs.size());
      CheckProperty(x_intField,'int',ptField);
      CheckProperty(x_strField,'string',ptField);
      CheckProperty(x_floatField,'float',ptField);
      CheckProperty(x_byteField,'byte',ptField);
      CheckProperty(x_charField,'char',ptField);
      CheckProperty(x_longField,'long',ptField);
      CheckProperty(x_strAtt,'string',ptAttribute);
      CheckProperty(x_intAtt,'int',ptAttribute);


  elt := Find(tr,x_complexType_SampleClass);
    CheckNotNull(elt,x_complexType_SampleClass);
    CheckEquals(x_complexType_SampleClass,elt.getString(s_Name));
    CheckEquals(ELEMENT_KIND_VARIABLE,elt.getByte(s_ElementKind),s_ElementKind);
    CheckNotNull(elt.getDataObject(s_DataType));
    CheckEquals(x_complexType_SampleClassType, elt.getDataObject(s_DataType).getString(s_Name));

  elt := Find(tr,x_complexType_SampleClassTypeAll);
    CheckNotNull(elt,x_complexType_SampleClassTypeAll);
    CheckEquals(x_complexType_SampleClassTypeAll,elt.getString(s_Name));
    CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
    clsType := elt;
      prpLs := clsType.getList(s_Property);
    CheckEquals(8,prpLs.size());
      CheckProperty(x_intField,'int',ptField);
      CheckProperty(x_strField,'string',ptField);
      CheckProperty(x_floatField,'float',ptField);
      CheckProperty(x_byteField,'byte',ptField);
      CheckProperty(x_charField,'char',ptField);
      CheckProperty(x_longField,'long',ptField);
      CheckProperty(x_strAtt,'string',ptAttribute);
      CheckProperty(x_intAtt,'int',ptAttribute);

  elt := Find(tr,x_complexType_SampleDerivedType);
    CheckNotNull(elt,x_complexType_SampleDerivedType);
    CheckEquals(x_complexType_SampleDerivedType,elt.getString(s_Name));
    CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
    clsType := elt;
    CheckNotNull(clsType.getDataObject(s_BaseType));
    CheckEquals(x_complexType_SampleClassType,clsType.getDataObject(s_BaseType).getString(s_Name));
      prpLs := clsType.getList(s_Property);
    CheckEquals(4,prpLs.size());
      CheckProperty(x_intField + 'Ex','int',ptField);
      CheckProperty(x_strField + 'Ex','string',ptField);
      CheckProperty(x_strAtt + 'Ex','string',ptAttribute);
      CheckProperty(x_intAtt + 'Ex','int',ptAttribute);
end;

procedure TTest_CustomXsdParser.ComplexType_Class_Embedded();
var
  tr : ISDODataObject;
  nestedClassName, nestedEnumName : string;

  procedure CheckProperty(
    const AName,ATypeName : string;
    const AFieldType : TPropertyType;
    const AClsType : ISDODataObject
  );
  var
    prp : ISDODataObject;
  begin
    prp := AClsType.getDataObject(Format('%s[%s = %s]',[s_Property,s_Name,QuotedStr(AName)]));
      CheckNotNull(prp);
      CheckEquals(AName,prp.getString(s_Name));
      CheckNotNull(prp.getDataObject(s_DataType));
      CheckEquals(ATypeName,prp.getDataObject(s_DataType).getString(s_Name));
      CheckEquals(PropertyType_Att[AFieldType],prp.getBoolean(s_IsAttribute),Format('%s.%s, %s',[AClsType.getString(s_Name),AName,s_IsAttribute]));
  end;

  procedure CheckEmbeddedClassType();
  var
    e : ISDODataObject;
    prpLst : ISDODataObjectList;
    nestedClsType : ISDODataObject;
  begin
    nestedClassName := Format('%s_%s_Type',[x_complexType_SampleClassType,x_classField]);
    e := Find(tr,nestedClassName);
      CheckNotNull(e,nestedClassName);
      CheckEquals(nestedClassName,e.getString(s_Name));
      CheckEquals(ELEMENT_KIND_TYPE,e.getByte(s_ElementKind),s_ElementKind);
      nestedClsType := e;
        prpLst := nestedClsType.getList(s_Property);
      CheckEquals(4,prpLst.size(),nestedClassName + '  properties count.');
        CheckProperty(x_intField + 'E','int',ptField,nestedClsType);
        CheckProperty(x_strField + 'E','string',ptField,nestedClsType);
        CheckProperty(x_strAtt + 'E','string',ptAttribute,nestedClsType);
        CheckProperty(x_intAtt + 'E','int',ptAttribute,nestedClsType);
  end;
  
  procedure CheckEmbeddedEnum();
  var
    e : ISDODataObject;
    enumType : ISDODataObject;
    k : Integer;
  begin
    nestedEnumName := Format('%s_%s_Type',[x_complexType_SampleClassType,x_enumField]);
    e := Find(tr,nestedEnumName);
      CheckNotNull(e,nestedEnumName);
      CheckEquals(nestedEnumName,e.getString(s_Name));
      CheckEquals(ELEMENT_KIND_TYPE,e.getByte(s_ElementKind),s_ElementKind);
      enumType := e;
      CheckEquals(x_enumSampleLIST_COUNT,enumType.getList(s_EnumValue).size());
      for k := 0 to Pred(x_enumSampleLIST_COUNT) do
        CheckEquals(x_enumSampleLIST[k],enumType.getList(s_EnumValue).getString(k));
  end;

var        
  fact :ISDODataFactory;
  mdl : ISDODataObject;
  clsType : ISDODataObject;
  elt : ISDODataObject;
  prpLs : ISDODataObjectList;
begin
  tr := LoadComplexType_Class_Embedded_Schema(fact);

  mdl := FindModule(tr,x_targetNamespace);
  CheckNotNull(mdl);
  CheckEquals(x_complexType_class_embedded,mdl.getString(s_Name));
  CheckEquals(x_targetNamespace,mdl.getString(s_NameSpace));
  CheckEquals(3,mdl.getList(s_Type).size()); 

  CheckEmbeddedClassType();
  CheckEmbeddedEnum();

  elt := Find(tr,x_complexType_SampleClassType);
    CheckNotNull(elt,x_complexType_SampleClassType);
    CheckEquals(x_complexType_SampleClassType,elt.getString(s_Name));
    //CheckIs(elt,TPasClassType);
    clsType := elt;
      prpLs := clsType.getList(s_Property);
    CheckEquals(9,prpLs.size());
      CheckProperty(x_intField,'int',ptField,clsType);
      CheckProperty(x_strField,'string',ptField,clsType);
      CheckProperty(x_floatField,'float',ptField,clsType);
      CheckProperty(x_byteField,'byte',ptField,clsType);
      CheckProperty(x_longField,'long',ptField,clsType);
      CheckProperty(x_strAtt,'string',ptAttribute,clsType);
      CheckProperty(x_intAtt,'int',ptAttribute,clsType);
      CheckProperty(x_classField,nestedClassName,ptField,clsType);
      CheckProperty(x_enumField,nestedEnumName,ptField,clsType);
end;

procedure TTest_CustomXsdParser.ComplexType_Class_Extend_Simple_Schema();
var
  tr : ISDODataObject;
  clsType : ISDODataObject;

  procedure CheckProperty(const AName,ATypeName : string; const AFieldType : TPropertyType);
  var
    prp : ISDODataObject;
  begin
    prp := clsType.getDataObject(Format('%s[%s = %s]',[s_Property,s_Name,QuotedStr(AName)]));
      CheckNotNull(prp);
      CheckEquals(AName,prp.getString(s_Name));
      CheckNotNull(prp.getDataObject(s_DataType));
      CheckEquals(ATypeName,prp.getDataObject(s_DataType).getString(s_Name));
      CheckEquals(PropertyType_Att[AFieldType],prp.getBoolean(s_IsAttribute),Format('%s.%s, %s',[clsType.getString(s_Name),AName,s_IsAttribute]));
  end;

var
  fact :ISDODataFactory;
  mdl : ISDODataObject;
  elt : ISDODataObject;
  prpLs : ISDODataObjectList;
begin
  tr := LoadComplexType_Class_Extend_Simple_Schema(fact);

  mdl := FindModule(tr,x_targetNamespace);
  CheckNotNull(mdl);
  CheckEquals(x_complexType_extend_simple,mdl.getString(s_Name));
  CheckEquals(x_targetNamespace,mdl.getString(s_NameSpace));
  CheckEquals(2,mdl.getList(s_Type).size());
  elt := Find(tr,x_complexType_SampleClassType);
    CheckNotNull(elt,x_complexType_SampleClassType);
    CheckEquals(x_complexType_SampleClassType,elt.getString(s_Name));
    CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
    clsType := elt;
      CheckNotNull(clsType.getDataObject(s_BaseType),'AncestorType is null');
      Check(Find(tr,XSD_NAME_SPACE,'string') = clsType.getDataObject(s_BaseType));

    prpLs := clsType.getList(s_Property);
    CheckEquals(1,prpLs.size());
      CheckProperty(x_intField,'int',ptAttribute);


  elt := Find(tr,x_complexType_SampleClassTypeA);
    CheckNotNull(elt,x_complexType_SampleClassTypeA);
    CheckEquals(x_complexType_SampleClassTypeA,elt.getString(s_Name));
    CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
    clsType := elt;
      CheckNotNull(clsType.getDataObject(s_BaseType),'AncestorType is null');
      Check(Find(tr,XSD_NAME_SPACE,'base64Binary') = clsType.getDataObject(s_BaseType));

    prpLs := clsType.getList(s_Property);
    CheckEquals(1,prpLs.size());
      CheckProperty(x_floatField,'float',ptAttribute);
end;

procedure TTest_CustomXsdParser.ComplexType_Class_open_type_any();
var     
  fact :ISDODataFactory;
  tr : ISDODataObject;
  clsType : ISDODataObject;
  mdl : ISDODataObject;
  elt : ISDODataObject;
  strBuffer : string;
  ls : TStringList;
begin
  tr := LoadComplexType_Class_OpenType(fact);
  mdl := FindModule(tr,x_targetNamespace);
  CheckNotNull(mdl);
  elt := Find(tr,'TClass_1');
  CheckNotNull(elt,'TClass_1');
  CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
  clsType := elt;
  strBuffer := FindTag(clsType,Format('%s#%s',[s_xs,s_any]));
  Check(Length(strBuffer) > 0, s_any);
  ls := TStringList.Create();
  try
    ls.Delimiter := ';';
    ls.DelimitedText := strBuffer;
    CheckEquals('lax',ls.Values[s_processContents]);
    CheckEquals('0',ls.Values[s_minOccurs]);
    CheckEquals(s_unbounded,ls.Values[s_maxOccurs]);
  finally
    ls.Free();
  end;
end;

procedure TTest_CustomXsdParser.ComplexType_Class_open_extension_type_any();
var    
  fact :ISDODataFactory;
  tr : ISDODataObject;
  clsType : ISDODataObject;
  mdl : ISDODataObject;
  elt : ISDODataObject;
  strBuffer : string;
  ls : TStringList;
begin
  tr := LoadComplexType_Class_OpenType(fact);
  mdl := FindModule(tr,x_targetNamespace);
  CheckNotNull(mdl);
  elt := Find(tr,'TClassSampleDerivedType');
  CheckNotNull(elt,'TClassSampleDerivedType');
  CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
  clsType := elt;
  strBuffer := FindTag(clsType,Format('%s#%s',[s_xs,s_any]));
  Check(Length(strBuffer) > 0, s_any);
  ls := TStringList.Create();
  try
    ls.Delimiter := ';';
    ls.DelimitedText := strBuffer;
    CheckEquals('skip',ls.Values[s_processContents]);
    CheckEquals(s_unbounded,ls.Values[s_maxOccurs]);
  finally
    ls.Free();
  end;
end;

procedure TTest_CustomXsdParser.ComplexType_Class_open_extension_type_anyAttribute();
var        
  fact :ISDODataFactory;
  tr : ISDODataObject;
  clsType : ISDODataObject;
  mdl : ISDODataObject;
  elt : ISDODataObject;
  strBuffer : string;
  ls : TStringList;
begin
  tr := LoadComplexType_Class_OpenType(fact);
  mdl := FindModule(tr,x_targetNamespace);
  CheckNotNull(mdl);
  elt := Find(tr,'TClassSampleDerivedType');
  CheckNotNull(elt,'TClassSampleDerivedType');
  CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
  clsType := elt;
  strBuffer := FindTag(clsType,Format('%s#%s',[s_xs,s_anyAttribute]));
  Check(Length(strBuffer) > 0, s_anyAttribute);
  ls := TStringList.Create();
  try
    ls.Delimiter := ';';
    ls.DelimitedText := strBuffer;
    CheckEquals('lax',ls.Values[s_processContents]);
  finally
    ls.Free();
  end;
end;

procedure TTest_CustomXsdParser.ComplexType_Class_sequence_open_type_anyAttribute();
var          
  fact :ISDODataFactory;
  tr : ISDODataObject;
  clsType : ISDODataObject;
  mdl : ISDODataObject;
  elt : ISDODataObject;
  strBuffer : string;
  ls : TStringList;
begin
  tr := LoadComplexType_Class_OpenType(fact);
  mdl := FindModule(tr,x_targetNamespace);
  CheckNotNull(mdl);
  elt := Find(tr,'TClass_1');
  CheckNotNull(elt,'TClass_1');
  CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
  clsType := elt;
  strBuffer := FindTag(clsType,Format('%s#%s',[s_xs,s_anyAttribute]));
  Check(Length(strBuffer) > 0, s_anyAttribute);
  ls := TStringList.Create();
  try
    ls.Delimiter := ';';
    ls.DelimitedText := strBuffer;
    CheckEquals('strict',ls.Values[s_processContents]);
  finally
    ls.Free();
  end;
end;

procedure TTest_CustomXsdParser.ComplexType_Class_all_open_type_anyAttribute();
var      
  fact :ISDODataFactory;
  tr : ISDODataObject;
  clsType : ISDODataObject;
  mdl : ISDODataObject;
  elt : ISDODataObject;
  strBuffer : string;
  ls : TStringList;
begin
  tr := LoadComplexType_Class_OpenType(fact);
  mdl := FindModule(tr,x_targetNamespace);
  CheckNotNull(mdl);
  elt := Find(tr,'TClassSampleTypeAll');
  CheckNotNull(elt,'TClassSampleTypeAll');
  CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
  clsType := elt;                                      
  strBuffer := FindTag(clsType,Format('%s#%s',[s_xs,s_anyAttribute]));
  Check(Length(strBuffer) > 0, s_anyAttribute);
  ls := TStringList.Create();
  try
    ls.Delimiter := ';';
    ls.DelimitedText := strBuffer;
    CheckEquals('skip',ls.Values[s_processContents]);
  finally
    ls.Free();
  end;
end;

procedure TTest_CustomXsdParser.ComplexType_Class_FalseArray();
var
  tr : ISDODataObject;
  clsType : ISDODataObject;

  procedure CheckProperty(const AName,ATypeName : string; const AFieldType : TPropertyType);
  var
    prp : ISDODataObject;
  begin
    prp := clsType.getDataObject(Format('%s[%s = %s]',[s_Property,s_Name,QuotedStr(AName)]));
      CheckNotNull(prp);
      CheckEquals(AName,prp.getString(s_Name));
      CheckNotNull(prp.getDataObject(s_DataType));
      CheckEquals(ATypeName,prp.getDataObject(s_DataType).getString(s_Name));
      CheckEquals(PropertyType_Att[AFieldType],prp.getBoolean(s_IsAttribute),Format('%s.%s, %s',[clsType.getString(s_Name),AName,s_IsAttribute]));
  end;

var                    
  fact :ISDODataFactory;
  mdl : ISDODataObject;
  elt : ISDODataObject;
  prpLs : ISDODataObjectList;
  prp : ISDODataObject;
begin
  tr := LoadComplexType_Class_FalseArray(fact);

  mdl := FindModule(tr,x_targetNamespace);
  CheckNotNull(mdl);
  CheckEquals('complex_class_false_array',mdl.getString(s_Name));
  CheckEquals(x_targetNamespace,mdl.getString(s_NameSpace));
  CheckEquals(2,mdl.getList(s_Type).size());
  elt := Find(tr,x_complexType_SampleClassType);
    CheckNotNull(elt,x_complexType_SampleClassType);
    CheckEquals(x_complexType_SampleClassType,elt.getString(s_Name));
    CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
    clsType := elt;
      prpLs := clsType.getList(s_Property);
    CheckEquals(2,prpLs.size());
      CheckProperty(x_intField,'int',ptField);
      CheckProperty(x_strField,'string',ptField);

  elt := Find(tr,x_complexType_SampleDerivedType);
    CheckNotNull(elt,x_complexType_SampleDerivedType);
    CheckEquals(x_complexType_SampleDerivedType,elt.getString(s_Name));
    CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
    clsType := elt;
    CheckNotNull(clsType.getDataObject(s_BaseType));
    CheckEquals(x_complexType_SampleClassType,clsType.getDataObject(s_BaseType).getString(s_Name));
      prpLs := clsType.getList(s_Property);
    CheckEquals(1,prpLs.size());
      prp := prpLs.getDataObject(0);
      Check(prp.getInteger(s_PropertyMaxOccurs) > 1);
      CheckEquals(x_intField + 'Ex', prp.getString(s_Name));
end;

procedure TTest_CustomXsdParser.ComplexType_ArraySequence();
var
  tr : ISDODataObject;
  clsType : ISDODataObject;

  procedure CheckProperty(
    const AName,
          ATypeName  : string;
    const AFieldType : TPropertyType;
    const AIsMulti   : Boolean = False
  );
  var
    prp : ISDODataObject;
  begin
    prp := clsType.getDataObject(Format('%s[%s = %s]',[s_Property,s_Name,QuotedStr(AName)]));
      CheckNotNull(prp);
      CheckEquals(AName,prp.getString(s_Name));
      CheckNotNull(prp.getDataObject(s_DataType));
      CheckEquals(ATypeName,prp.getDataObject(s_DataType).getString(s_Name));
      CheckEquals(PropertyType_Att[AFieldType],prp.getBoolean(s_IsAttribute),Format('%s.%s, %s',[clsType.getString(s_Name),AName,s_IsAttribute]));
      if AIsMulti then
        Check(prp.getInteger(s_PropertyMaxOccurs) > 1)
      else
        CheckEquals(1, prp.getInteger(s_PropertyMaxOccurs))  ;
  end;

var
  fact :ISDODataFactory;
  mdl : ISDODataObject;
  elt : ISDODataObject;
  prpLs : ISDODataObjectList;
  nestedClassName : string;
begin
  tr := LoadComplexType_ArraySequence_Schema(fact);

  mdl := FindModule(tr,x_targetNamespace);
  CheckNotNull(mdl);
  CheckEquals(x_complexType_array_sequence,mdl.getString(s_Name));
  CheckEquals(x_targetNamespace,mdl.getString(s_NameSpace));
  CheckEquals(3,mdl.getList(s_Type).size());
  elt := Find(tr,x_complexType_SampleArrayIntFieldType);
    CheckNotNull(elt,x_complexType_SampleArrayIntFieldType);
    CheckEquals(x_complexType_SampleArrayIntFieldType,elt.getString(s_Name));
    CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
    clsType := elt;
    CheckEquals(1,clsType.getList(s_Property).size());
    CheckProperty(x_intField,'int',ptField,True);

  nestedClassName := Format('%s_%s_Type',[x_complexType_SampleArrayItemType,x_Item]);
  elt := Find(tr,nestedClassName);
    CheckNotNull(elt,nestedClassName);
    CheckEquals(nestedClassName,elt.getString(s_Name),'Item Name');
    CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
    clsType := elt;
      prpLs := clsType.getList(s_Property);
    CheckEquals(8,prpLs.size());
      CheckProperty(x_intField,'int',ptField);
      CheckProperty(x_strField,'string',ptField);
      CheckProperty(x_floatField,'float',ptField);
      CheckProperty(x_byteField,'byte',ptField);
      CheckProperty(x_charField,'char',ptField);
      CheckProperty(x_longField,'long',ptField);
      CheckProperty(x_strAtt,'string',ptAttribute);
      CheckProperty(x_intAtt,'int',ptAttribute);

  elt := Find(tr,x_complexType_SampleArrayItemType);
    CheckNotNull(elt,x_complexType_SampleArrayItemType);
    CheckEquals(x_complexType_SampleArrayItemType,elt.getString(s_Name), 'Array name');
    CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
    clsType := elt;
    CheckProperty(x_Item,nestedClassName,ptField,True);
end;

procedure TTest_CustomXsdParser.ComplexType_ArraySequence_ItemName_Schema();
var
  clsType : ISDODataObject;

  procedure CheckProperty(
    const AName,
          ATypeName  : string;
    const AFieldType : TPropertyType;
    const AIsMulti   : Boolean = False
  );
  var
    prp : ISDODataObject;
  begin
    prp := clsType.getDataObject(Format('%s[%s = %s]',[s_Property,s_Name,QuotedStr(AName)]));
      CheckNotNull(prp);
      CheckEquals(AName,prp.getString(s_Name));
      CheckNotNull(prp.getDataObject(s_DataType));
      CheckEquals(ATypeName,prp.getDataObject(s_DataType).getString(s_Name));
      CheckEquals(PropertyType_Att[AFieldType],prp.getBoolean(s_IsAttribute),Format('%s.%s, %s',[clsType.getString(s_Name),AName,s_IsAttribute]));
      if AIsMulti then
        Check(prp.getInteger(s_PropertyMaxOccurs) > 1)
      else
        CheckEquals(1, prp.getInteger(s_PropertyMaxOccurs))  ;
  end;

var
  fact :ISDODataFactory;
  tr : ISDODataObject;
  mdl : ISDODataObject;
  elt : ISDODataObject;
begin
  tr := LoadComplexType_ArraySequence_ItemName_Schema(fact);

  mdl := FindModule(tr,x_targetNamespace);
  CheckNotNull(mdl);
  CheckEquals('array_sequence_item_name',mdl.getString(s_Name));
  CheckEquals(x_targetNamespace,mdl.getString(s_NameSpace));
  CheckEquals(2,mdl.getList(s_Type).size());
  elt := Find(tr,'ArrayOfEmailAddress');
    CheckNotNull(elt,'ArrayOfEmailAddress');
    CheckEquals('ArrayOfEmailAddress',elt.getString(s_Name));
    CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
    clsType := elt;
      CheckProperty('EmailAddress','EmailAddress',ptField,True);
end;

procedure TTest_CustomXsdParser.ComplexType_ArraySequence_Embedded();
var
  tr : ISDODataObject;
  clsType : ISDODataObject;

  procedure CheckProperty(
    const AName,
          ATypeName  : string;
    const AFieldType : TPropertyType;
    const AIsMulti   : Boolean = False
  );
  var
    prp : ISDODataObject;
  begin
    prp := clsType.getDataObject(Format('%s[%s = %s]',[s_Property,s_Name,QuotedStr(AName)]));
      CheckNotNull(prp);
      CheckEquals(AName,prp.getString(s_Name));
      CheckNotNull(prp.getDataObject(s_DataType));
      CheckEquals(ATypeName,prp.getDataObject(s_DataType).getString(s_Name));
      CheckEquals(PropertyType_Att[AFieldType],prp.getBoolean(s_IsAttribute),Format('%s.%s, %s',[clsType.getString(s_Name),AName,s_IsAttribute]));
      if AIsMulti then
        Check(prp.getInteger(s_PropertyMaxOccurs) > 1)
      else
        CheckEquals(1, prp.getInteger(s_PropertyMaxOccurs))  ;
  end;

var
  fact :ISDODataFactory;
  mdl : ISDODataObject;
  elt : ISDODataObject;
  nestedClassName : string;
begin
    tr := LoadComplexType_ArraySequence_Schema(fact);

    mdl := FindModule(tr,x_targetNamespace);
    CheckNotNull(mdl);
    CheckEquals(x_complexType_array_sequence,mdl.getString(s_Name));
    CheckEquals(x_targetNamespace,mdl.getString(s_NameSpace));
    CheckEquals(3,mdl.getList(s_Type).size());
    elt := Find(tr,x_complexType_SampleArrayIntFieldType);
      CheckNotNull(elt,x_complexType_SampleArrayIntFieldType);
      CheckEquals(x_complexType_SampleArrayIntFieldType,elt.getString(s_Name));
      CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
      clsType := elt;
      CheckProperty(x_intField,'int',ptField,True);

    nestedClassName := Format('%s_%s_Type',[x_complexType_SampleArrayItemType,x_Item]);
    elt := Find(tr,nestedClassName);
      CheckNotNull(elt,nestedClassName);
      CheckEquals(nestedClassName,elt.getString(s_Name),'Item Name');
      CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
      clsType := elt;
      CheckEquals(8,clsType.getList(s_Property).size());
        CheckProperty(x_intField,'int',ptField);
        CheckProperty(x_strField,'string',ptField);
        CheckProperty(x_floatField,'float',ptField);
        CheckProperty(x_byteField,'byte',ptField);
        CheckProperty(x_charField,'char',ptField);
        CheckProperty(x_longField,'long',ptField);
        CheckProperty(x_strAtt,'string',ptAttribute);
        CheckProperty(x_intAtt,'int',ptAttribute);

    elt := Find(tr,x_complexType_SampleArrayItemType);
      CheckNotNull(elt,x_complexType_SampleArrayItemType);
      CheckEquals(x_complexType_SampleArrayItemType,elt.getString(s_Name), 'Array name');
      CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
      clsType := elt;
      CheckProperty(x_Item,nestedClassName,ptField,True);
end;

{procedure TTest_CustomXsdParser.ComplexType_Array_soaparray();
var
  tr : ISDODataObject;
  mdl : TPasModule;
  ls : TList;
  elt : TPasElement;
  arrayType : TPasArrayType;
begin
  tr := LoadComplexType_Array_soaparray();
  try
    mdl := FindModule(tr,x_targetNamespace);
    CheckNotNull(mdl);
    CheckEquals(x_complexType_array_soaparray,mdl.getString(s_Name));
    CheckEquals(x_targetNamespace,mdl.getString(s_NameSpace));
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(1,ls.Count);
    elt := tr.FindElement(x_complexType_SampleArrayIntFieldType);
      CheckNotNull(elt,x_complexType_SampleArrayIntFieldType);
      CheckEquals(x_complexType_SampleArrayIntFieldType,elt.Name);
      CheckEquals(x_complexType_SampleArrayIntFieldType,tr.GetExternalName(elt));
      CheckIs(elt,TPasArrayType);
      arrayType := elt as TPasArrayType;
      CheckNotNull(arrayType.ElType);
      CheckEquals('int',tr.GetExternalName(arrayType.ElType));
      CheckEquals('item',tr.GetArrayItemName(arrayType));
      CheckEquals('item',tr.GetArrayItemExternalName(arrayType));

    CheckNull(tr.FindElementNS('Array','http://schemas.xmlsoap.org/wsdl/'));
  finally
    tr.Free();
  end;
end;

procedure TTest_CustomXsdParser.class_widestring_property();
const s_class_name = 'TSampleClass';
var
  clsType : TPasClassType;
  tr : ISDODataObject;

  procedure CheckProperty(const AName,ATypeName,ADeclaredTypeName : string; const AFieldType : TPropertyType);
  var
    prp : TPasProperty;
  begin
    prp := FindMember(clsType,AName) as TPasProperty;
      CheckNotNull(prp);
      CheckEquals(AName,prp.Name);
      CheckEquals(AName,tr.GetExternalName(prp));
      CheckNotNull(prp.VarType);
      CheckEquals(ATypeName,prp.VarType.Name,'TypeName');
      CheckEquals(ADeclaredTypeName,tr.GetExternalName(prp.VarType),'DeclaredTypeName');
      CheckEquals(PropertyType_Att[AFieldType],tr.IsAttributeProperty(prp));
  end;

var
  mdl : TPasModule;
  elt : TPasElement;
begin
  tr := load_class_widestring_property();
  try
    mdl := tr.FindModule('class_widestring_property');
    CheckNotNull(mdl,'class_widestring_property');
    elt := tr.FindElement(s_class_name);
      CheckNotNull(elt,s_class_name);
      CheckEquals(s_class_name,elt.Name);
      CheckEquals(s_class_name,tr.GetExternalName(elt));
      CheckIs(elt,TPasClassType);
      clsType := elt as TPasClassType;
      CheckProperty('elementProp','WideString','string',ptField);
      CheckProperty('elementAtt','WideString','string',ptAttribute);
  finally
    tr.Free();
  end;
end;}

procedure TTest_CustomXsdParser.ComplexType_Class_default_values();
var
  tr : ISDODataObject;
  clsType : ISDODataObject;

  procedure CheckProperty(
    const AName,
          ATypeName : string;
    const AFieldType : TPropertyType;
    const ADefault : string
  );
  var
    prp : ISDODataObject;
  begin
    prp := clsType.getDataObject(Format('%s[%s = %s]',[s_Property,s_Name,QuotedStr(AName)]));
      CheckNotNull(prp);
      CheckEquals(AName,prp.getString(s_Name));
      CheckNotNull(prp.getDataObject(s_DataType));
      CheckEquals(ATypeName,prp.getDataObject(s_DataType).getString(s_Name));
      CheckEquals(PropertyType_Att[AFieldType],prp.getBoolean(s_IsAttribute),Format('%s.%s, %s',[clsType.getString(s_Name),AName,s_IsAttribute]));
      CheckEquals(ADefault,prp.getString(s_DefaultValue),Format('%s.%s, default',[clsType.getString(s_Name),AName]));
  end;

var
  fact :ISDODataFactory;
  mdl : ISDODataObject;
  elt : ISDODataObject;
  prpLs : ISDODataObjectList;
begin
  tr := LoadComplexType_Class_default_values(fact);

  mdl := FindModule(tr,x_targetNamespace);
  CheckNotNull(mdl);
  CheckEquals(x_complexType_class_default,mdl.getString(s_Name));
  CheckEquals(x_targetNamespace,mdl.getString(s_NameSpace));
  CheckEquals(1,mdl.getList(s_Type).size());
  elt := Find(tr,x_complexType_SampleClassType);
    CheckNotNull(elt,x_complexType_SampleClassType);
    CheckEquals(x_complexType_SampleClassType,elt.getString(s_Name));
    CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
    clsType := elt;
      prpLs := clsType.getList(s_Property);
    CheckEquals(8,prpLs.size());
      CheckProperty(x_intField,'int',ptField,'1210');
      CheckProperty(x_strField,'string',ptField,'azerty');
      CheckProperty(x_floatField,'float',ptField,'1234');
      CheckProperty(x_byteField,'byte',ptField,'23');
      CheckProperty(x_charField,'char',ptField,'i');
      CheckProperty(x_longField,'long',ptField,'567');
      CheckProperty(x_strAtt,'string',ptAttribute,'attribute azerty');
      CheckProperty(x_intAtt,'int',ptAttribute,'789');
end;

procedure TTest_CustomXsdParser.ComplexType_Class_properties_extended_metadata();
var
  tr : ISDODataObject;
  clsType : ISDODataObject;

  procedure CheckProperty(
    const AName,
          ATypeName : string;
    const AFieldType : TPropertyType;
    const ADefault : string;
    const AExtMetaDataNameSpace,
          AExtMetaDataLocalName,
          AExtMetaDataValue : string
  );
  var
    prp : ISDODataObject;
    locExtMeta : string;
  begin
    prp := clsType.getDataObject(Format('%s[%s = %s]',[s_Property,s_Name,QuotedStr(AName)]));
      CheckNotNull(prp);
      CheckEquals(AName,prp.getString(s_Name));
      CheckNotNull(prp.getDataObject(s_DataType));
      CheckEquals(ATypeName,prp.getDataObject(s_DataType).getString(s_Name));
      CheckEquals(PropertyType_Att[AFieldType],prp.getBoolean(s_IsAttribute),Format('%s.%s, %s',[clsType.getString(s_Name),AName,s_IsAttribute]));
      CheckEquals(ADefault,prp.getString(s_DefaultValue),Format('%s.%s, default',[clsType.getString(s_Name),AName]));
      locExtMeta := Format('%s#%s',[AExtMetaDataNameSpace,AExtMetaDataLocalName]);
      if not IsStrEmpty(locExtMeta) then
        CheckEquals(AExtMetaDataValue, FindTag(prp,locExtMeta), 'extended metadata');
  end;

var
  fact :ISDODataFactory;
  mdl : ISDODataObject;
  elt : ISDODataObject;
  prpLs : ISDODataObjectList;
begin
  tr := LoadComplexType_Class_properties_extended_metadata(fact);

  mdl := FindModule(tr,x_targetNamespace);
  CheckNotNull(mdl);
  CheckEquals(x_complexType_class_properties_extended_metadata,mdl.getString(s_Name));
  CheckEquals(x_targetNamespace,mdl.getString(s_NameSpace));
  CheckEquals(1,mdl.getList(s_Type).size());
  elt := Find(tr,x_complexType_SampleClassType);
    CheckNotNull(elt,x_complexType_SampleClassType);
    CheckEquals(x_complexType_SampleClassType,elt.getString(s_Name));
    CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
    clsType := elt;
      prpLs := clsType.getList(s_Property);
    CheckEquals(5,prpLs.size());
      CheckProperty(x_intField,'int',ptField,'', 'uri-4','a','1210');
        CheckProperty(x_intField,'int',ptField,'', 'uri-4','b','uri-5#xx');
      CheckProperty(x_strField,'string',ptField,'azerty', 'uri-4','a', 'http://www.w3.org/2001/XMLSchema#int');
      CheckProperty(x_strAtt,'string',ptAttribute,'attribute azerty', 'uri-4','a', 'optional');
      CheckProperty(x_intAtt,'int',ptAttribute,'', '', '', '');
end;

procedure TTest_CustomXsdParser.ComplexType_Class_properties_extended_metadata2();
const s_ProjectType = 'ProjectType';
var         
  fact :ISDODataFactory;
  tr : ISDODataObject;
  clsType : ISDODataObject;
  mdl : ISDODataObject;
  elt : ISDODataObject;
  p : ISDODataObject;
begin
  tr := LoadComplexType_Class_properties_extended_metadata2(fact);
  mdl := FindModule(tr,'uri:sample');
  CheckNotNull(mdl);
  elt := Find(tr,s_ProjectType);
    CheckNotNull(elt,s_ProjectType);
    CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
    clsType := elt;
      p := clsType.getDataObject(Format('%s[%s = %s]',[s_Property,s_Name,QuotedStr('ProjectLeader')]));
      CheckNotNull(p,'Property non found : "ProjectLeader"');
      CheckEquals('uri:sample#Person', FindTag(p,'commonj.sdo#propertyType'), 'extended metadata');

      p := clsType.getDataObject(Format('%s[%s = %s]',[s_Property,s_Name,QuotedStr('ProjectLeaderArray')]));
      CheckNotNull(p,'Property non found : "ProjectLeaderArray"');
      CheckEquals('uri:sample#Person', FindTag(p,'commonj.sdo#propertyType'), 'extended metadata');
end;

procedure TTest_CustomXsdParser.class_property_composed_name();
const s_class_name = 'TSampleClass';
var
  clsType : ISDODataObject;
  tr : ISDODataObject;

  procedure CheckProperty(
    const AName,
          ATypeName      : string;
    const AFieldType     : TPropertyType
  );
  var
    prp : ISDODataObject;
  begin
    prp := clsType.getDataObject(Format('%s[%s = %s]',[s_Property,s_Name,QuotedStr(AName)]));
      CheckNotNull(prp);
      CheckEquals(AName,prp.getString(s_Name));
      CheckNotNull(prp.getDataObject(s_DataType));
      CheckEquals(ATypeName,prp.getDataObject(s_DataType).getString(s_Name));
      CheckEquals(PropertyType_Att[AFieldType],prp.getBoolean(s_IsAttribute),Format('%s.%s, %s',[clsType.getString(s_Name),AName,s_IsAttribute]));
  end;

var
  fact :ISDODataFactory;
  mdl : ISDODataObject;
  elt : ISDODataObject;
begin
  tr := load_class_property_composed_name(fact);
  mdl := FindModule(tr,'urn_sample');
  CheckNotNull(mdl,'urn_sample');
  elt := Find(tr,s_class_name);
    CheckNotNull(elt,s_class_name);
    CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
    clsType := elt;
    CheckProperty('one-prop','string',ptField);
    CheckProperty('one-two-prop','string',ptAttribute);
end;

procedure TTest_CustomXsdParser.schema_import();
const
  s_base_namespace = 'urn:base-library';
  s_base_type = 'SampleBase_Type';
  s_second_namespace = 'urn:second-library';
  s_second_type = 'Second_Type';
var     
  fact :ISDODataFactory;
  tr : ISDODataObject;
  mdl : ISDODataObject;
  elt, prpElt : ISDODataObject;
  baseType, scdClass : ISDODataObject;
begin
  tr := load_schema_import(fact);

  mdl := FindModule(tr,s_base_namespace);
  CheckNotNull(mdl,s_base_namespace);
  CheckEquals(1,mdl.getList(s_Type).size());
  elt := Find(tr,s_base_namespace,s_base_type);
    CheckNotNull(elt,s_base_type);
    CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
    baseType := elt;

  mdl := FindModule(tr,s_second_namespace);
  CheckNotNull(mdl,s_second_namespace);
  CheckEquals(1,mdl.getList(s_Type).size());
  elt := Find(tr,s_second_namespace,s_second_type);
    CheckNotNull(elt,s_second_type);
    CheckEquals(ELEMENT_KIND_TYPE,elt.getByte(s_ElementKind),s_ElementKind);
    scdClass := elt;
    prpElt := scdClass.getDataObject(Format('%s[%s = %s]',[s_Property,s_Name,QuotedStr('SampleProperty')]));
    CheckNotNull(prpElt);
    Check(baseType = prpElt.getDataObject(s_DataType));
end;



{ TTest_XsdParser }

function TTest_XsdParser.ParseDoc(var AFactory : ISDODataFactory; const ADoc: string): ISDODataObject;
var
  locDoc : TXMLDocument;
  prs : IXsdParser;
  prsCtx : IParserContext;
  fileName : string;
begin
  if (AFactory = nil) then begin
    AFactory := TSDODataFactory.Create();
    AddTypeTree(AFactory);
  end;
  fileName := sdoExpandLocalFileName(XsdTestFilesPath + ADoc + '.xsd');
  locDoc := LoadXmlFile(fileName);
  try
    Result := AFactory.CreateNew(s_XsdParserNS,s_TypeTreeType);
    prs := TXsdParser.Create(locDoc,Result,ADoc);
    prsCtx := prs as IParserContext;
    prsCtx.SetDocumentLocator(TFileDocumentLocator.Create(ExtractFilePath(fileName)));
    prs.ParseTypes();
  finally
    ReleaseDomNode(locDoc);
  end;
end;

function TTest_XsdParser.LoadEmptySchema(var AFactory : ISDODataFactory): ISDODataObject;
begin
  Result := ParseDoc(AFactory, x_empty);
end;

function TTest_XsdParser.LoadSimpleType_Enum_Schema(var AFactory : ISDODataFactory): ISDODataObject;
begin
  Result := ParseDoc(AFactory, x_simpleType);
end;

function TTest_XsdParser.LoadSimpleType_Enum_Embedded_Schema(var AFactory : ISDODataFactory): ISDODataObject;
begin
  Result := ParseDoc(AFactory, x_simpleTypeEmbedded);
end;

function TTest_XsdParser.LoadSimpleType_AliasToNativeType_Schema(var AFactory : ISDODataFactory) : ISDODataObject;
begin
  Result := ParseDoc(AFactory, x_simpletypeNativeAlias);
end;

function TTest_XsdParser.LoadComplexType_Class_Schema(var AFactory : ISDODataFactory): ISDODataObject;
begin
  Result := ParseDoc(AFactory, x_complexType_class);
end;

function TTest_XsdParser.LoadComplexType_Class_Embedded_Schema(var AFactory : ISDODataFactory): ISDODataObject;
begin
  Result := ParseDoc(AFactory, x_complexType_class_embedded);
end;

function TTest_XsdParser.LoadComplexType_Class_Extend_Simple_Schema(var AFactory : ISDODataFactory) : ISDODataObject;
begin
  Result := ParseDoc(AFactory, x_complexType_extend_simple);
end;

function TTest_XsdParser.LoadComplexType_Class_OpenType(var AFactory : ISDODataFactory): ISDODataObject;
begin
  Result := ParseDoc(AFactory, 'complex_class_open_type');
end;

function TTest_XsdParser.LoadComplexType_ArraySequence_Schema(var AFactory : ISDODataFactory): ISDODataObject;
begin
  Result := ParseDoc(AFactory, x_complexType_array_sequence);
end;

function TTest_XsdParser.LoadComplexType_ArraySequence_Embedded_Schema(var AFactory : ISDODataFactory): ISDODataObject;
begin
  Result := ParseDoc(AFactory, x_complexType_array_sequence_embedded);
end;

{function TTest_XsdParser.LoadComplexType_Array_soaparray() : ISDODataObject;
begin
  Result := ParseDoc(x_complexType_array_soaparray);
end; }

function TTest_XsdParser.LoadComplexType_Class_default_values(var AFactory : ISDODataFactory) : ISDODataObject;
begin
  Result := ParseDoc(AFactory,x_complexType_class_default);
end;

function TTest_XsdParser.LoadComplexType_Class_properties_extended_metadata(var AFactory : ISDODataFactory): ISDODataObject;
begin
  Result := ParseDoc(AFactory, x_complexType_class_properties_extended_metadata);
end;

function TTest_XsdParser.LoadComplexType_Class_properties_extended_metadata2(var AFactory : ISDODataFactory): ISDODataObject;
begin
  Result := ParseDoc(AFactory, x_complexType_class_properties_extended_metadata + '_2');
end;

function TTest_XsdParser.load_class_property_composed_name(var AFactory : ISDODataFactory) : ISDODataObject;
begin
  Result := ParseDoc(AFactory, 'class_property_composed_name');  
end;

function TTest_XsdParser.load_schema_import(var AFactory : ISDODataFactory): ISDODataObject;
begin
  Result := ParseDoc(AFactory, 'import_second_library');
end;

function TTest_XsdParser.LoadComplexType_ArraySequence_ItemName_Schema(var AFactory : ISDODataFactory) : ISDODataObject;
begin
  Result := ParseDoc(AFactory, 'array_sequence_item_name');
end;

function TTest_XsdParser.LoadComplexType_Class_FalseArray(var AFactory : ISDODataFactory) : ISDODataObject;
begin
  Result := ParseDoc(AFactory, 'complex_class_false_array');
end;


initialization
  RegisterTest('XSD parser',TTest_XsdParser.Suite);

end.
