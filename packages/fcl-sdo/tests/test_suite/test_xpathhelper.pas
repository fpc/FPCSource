{$INCLUDE sdo_global.inc}
unit test_xpathhelper;

interface
uses SysUtils
{$IFDEF FPC}
  ,fpcunit, testutils, testregistry
{$ENDIF}
{$IFNDEF FPC}
  ,TestFrameWork
{$ENDIF}
  , test_suite_utils, sdo, sdo_types, sdo_xpath_helper;

type

  TXPathScanner_Test = class(TWstBaseTest)
  published
    procedure simple();
    procedure simple_number();
    procedure composed_1();
  end;

  TXPathParser_Test = class(TWstBaseTest)
  published
    procedure parse_1();
    procedure parse_2();
    procedure parse_3();
    procedure parse_4();
    procedure parse_5();
    procedure parse_6();
    procedure parse_7();
    procedure parse_8();
    procedure parse_9();
    procedure parse_10();
  end;

  TXPathProcessor_Test = class(TWstBaseTest)
  published
    procedure Execute_simple_object();
    procedure Execute_simple_value_type();

    procedure Execute_nested_object();

    procedure Execute_equal();
    procedure Execute_equal_bool();
    procedure Execute_equal_byte();
    procedure Execute_equal_date();
    procedure Execute_equal_integer();
    procedure Execute_equal_string();
{$IFDEF HAS_SDO_BYTES}
    procedure Execute_equal_bytes();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure Execute_equal_char();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure Execute_equal_currency();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure Execute_equal_double();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure Execute_equal_float();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure Execute_equal_long();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure Execute_equal_short();
{$ENDIF HAS_SDO_SHORT}
  end;

  TXPathUtilsFunc_Test = class(TWstBaseTest)
  published
    procedure getXpath_func();
  end;

implementation

uses
  sdo_datafactory, sdo_date_utils;

const
  s_uri              = 'urn-test';
  s_type_object_A    = 'objectA';
  s_type_object_B    = 'objectB';
  s_type_object_C    = 'objectC';
  s_type_object_D    = 'objectD';
  s_bool_prop        = 'bool_prop';            s_bool_propList      = 'bool_prop_list';
  s_integer_prop     = 'integer_prop';         s_integer_propList   = 'integer_prop_list';
  s_string_prop      = 'string_prop';          s_string_propList    = 'string_prop_list';
  s_object_prop      = 'object_prop';          s_object_propList    = 'object_prop_list';
  s_object_ref_prop  = 'object_ref_prop';

{ TXPathProcessor_Test }

procedure TXPathProcessor_Test.Execute_equal();
  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
    locRes : ISDODataFactory;
  begin
    locRes := TSDODataFactory.Create() as ISDODataFactory;
    locRes.AddType(s_uri,s_type_object_A,[]);
    locRes.AddType(s_uri,s_type_object_B,[]);
    locRes.AddType(s_uri,s_type_object_C,[]);
    locRes.AddType(s_uri,s_type_object_D,[]);
    locObj := locRes.getType(s_uri,s_type_object_A);
      locRes.addProperty(locObj,s_bool_prop,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType], []);
      locRes.addProperty(locObj,s_bool_propList,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsMany]);
      locRes.addProperty(locObj,s_integer_prop,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType], []);
      locRes.addProperty(locObj,s_integer_propList,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsMany]);
      locRes.addProperty(locObj,s_string_prop,sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
      locRes.addProperty(locObj,s_string_propList,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsMany]);
      locRes.addProperty(locObj,'p_ab',s_uri,s_type_object_B,[pfIsContainment]);
      locRes.addProperty(locObj,'p_ab_list',s_uri,s_type_object_B,[pfIsMany,pfIsContainment]);
      locRes.addProperty(locObj,'p_ac',s_uri,s_type_object_C,[pfIsContainment]);

    locObj := locRes.getType(s_uri,s_type_object_B);
      locRes.addProperty(locObj,s_bool_prop,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType], []);
      locRes.addProperty(locObj,s_bool_propList,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsMany]);
      locRes.addProperty(locObj,s_integer_prop,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType], []);
      locRes.addProperty(locObj,s_integer_propList,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsMany]);
      locRes.addProperty(locObj,s_string_prop,sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
      locRes.addProperty(locObj,s_string_propList,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsMany]);
      locRes.addProperty(locObj,'p_bc',s_uri,s_type_object_C,[pfIsContainment]);
      locRes.addProperty(locObj,'p_bc_list',s_uri,s_type_object_C,[pfIsMany,pfIsContainment]);

    locObj := locRes.getType(s_uri,s_type_object_C);
      locRes.addProperty(locObj,'p_cd',s_uri,s_type_object_D,[pfIsContainment]);

    Result := locRes;
  end;

  procedure fill_p_ab_list(const AList : ISDODataObjectList; const ACount : Integer; const AFac : ISDODataFactory);
  var
    tmp : ISDODataObject;
    k : Integer;
  begin
    for k := 0 to Pred(ACount) do begin
      tmp := AFac.createNew(s_uri, s_type_object_B);
      tmp.setBoolean(s_bool_prop, ( k mod 3 ) = 0 );
      tmp.setString(s_string_prop, Format('string %d',[k]));
      tmp.setInteger(s_integer_prop, k);
      AList.append(tmp);
    end;
  end;

var
  locFac : ISDODataFactory;
  locExp : TXPathExpression;
  locX : TXPathProcessor;
  locRootObject : ISDODataObject;
  tmpListAB : ISDODataObjectList;
  i : TSDOInteger;
begin
  locFac := CreateFactory();
  locRootObject := locFac.createNew(s_uri,s_type_object_A);
    tmpListAB := locRootObject.getList('p_ab_list');
  fill_p_ab_list(tmpListAB,100,locFac);

  locX := nil;
  locExp := TXPathExpression.Create();
  try
    locX := TXPathProcessor.Create();
    locX.Context.SetObject(locRootObject,nil);
    i := 12;
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%d]',[s_integer_prop,i])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := 10;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%d]',[s_integer_prop,i])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := 0;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%d]',[s_integer_prop,i])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := -123;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%d]',[s_integer_prop,i])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckNull), Ord(locX.Context.ContentKind));
  finally
    FreeAndNil(locX);
    FreeAndNil(locExp);
  end;
end;

procedure TXPathProcessor_Test.Execute_equal_bool();
const
  SIMPLE_PROP_A = 'simpleProperty_A';
  LIST_PROP_A   = 'listProperty_A';
  SIMPLE_PROP_B = 'simpleProperty_B';
  PROP_TYPE   = BooleanType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
    locRes : ISDODataFactory;
  begin
    locRes := TSDODataFactory.Create() as ISDODataFactory;
    locRes.AddType(s_uri,s_type_object_A,[]);
    locRes.AddType(s_uri,s_type_object_B,[]);
    locObj := locRes.getType(s_uri,s_type_object_A);
      locRes.addProperty(locObj,SIMPLE_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);
      locRes.addProperty(locObj,LIST_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[pfIsMany]);
      locRes.addProperty(locObj,'p_ab_list',s_uri,s_type_object_B,[pfIsMany,pfIsContainment]);

    locObj := locRes.getType(s_uri,s_type_object_B);
      locRes.addProperty(locObj,SIMPLE_PROP_B,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);

    Result := locRes;
  end;

  procedure fill_p_ab_list(const AList : ISDODataObjectList; const AFac : ISDODataFactory);
  var
    tmp : ISDODataObject;
  begin
    tmp := AFac.createNew(s_uri, s_type_object_B);
    tmp.setBoolean(SIMPLE_PROP_B,True);
    AList.append(tmp);
    tmp := AFac.createNew(s_uri, s_type_object_B);
    tmp.setBoolean(SIMPLE_PROP_B,False);
    AList.append(tmp);
  end;

var
  locFac : ISDODataFactory;
  locExp : TXPathExpression;
  locX : TXPathProcessor;
  locRootObject : ISDODataObject;
  tmpListAB : ISDODataObjectList;
  i : Integer;
begin
  locFac := CreateFactory();
  locRootObject := locFac.createNew(s_uri,s_type_object_A);
    tmpListAB := locRootObject.getList('p_ab_list');
  fill_p_ab_list(tmpListAB,locFac);

  locX := nil;
  locExp := TXPathExpression.Create();
  try
    locX := TXPathProcessor.Create();
    locX.Context.SetObject(locRootObject,nil);
    i := 1;
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=false]',[SIMPLE_PROP_B])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := 0;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=true]',[SIMPLE_PROP_B])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
  finally
    FreeAndNil(locX);
    FreeAndNil(locExp);
  end;
end;

procedure TXPathProcessor_Test.Execute_equal_byte();
const
  SIMPLE_PROP_A = 'simpleProperty_A';
  LIST_PROP_A   = 'listProperty_A';
  SIMPLE_PROP_B = 'simpleProperty_B';
  PROP_TYPE   = ByteType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
    locRes : ISDODataFactory;
  begin
    locRes := TSDODataFactory.Create() as ISDODataFactory;
    locRes.AddType(s_uri,s_type_object_A,[]);
    locRes.AddType(s_uri,s_type_object_B,[]);
    locObj := locRes.getType(s_uri,s_type_object_A);
      locRes.addProperty(locObj,SIMPLE_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);
      locRes.addProperty(locObj,LIST_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[pfIsMany]);
      locRes.addProperty(locObj,'p_ab_list',s_uri,s_type_object_B,[pfIsMany,pfIsContainment]);

    locObj := locRes.getType(s_uri,s_type_object_B);
      locRes.addProperty(locObj,SIMPLE_PROP_B,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);

    Result := locRes;
  end;

  procedure fill_p_ab_list(const AList : ISDODataObjectList; const ACount : Byte; const AFac : ISDODataFactory);
  var
    tmp : ISDODataObject;
    k : Byte;
  begin
    for k := 0 to Pred(ACount) do begin
      tmp := AFac.createNew(s_uri, s_type_object_B);
      tmp.setByte(SIMPLE_PROP_B,k);
      AList.append(tmp);
    end;
  end;

var
  locFac : ISDODataFactory;
  locExp : TXPathExpression;
  locX : TXPathProcessor;
  locRootObject : ISDODataObject;
  tmpListAB : ISDODataObjectList;
  i : Byte;
begin
  locFac := CreateFactory();
  locRootObject := locFac.createNew(s_uri,s_type_object_A);
    tmpListAB := locRootObject.getList('p_ab_list');
  fill_p_ab_list(tmpListAB,100,locFac);

  locX := nil;
  locExp := TXPathExpression.Create();
  try
    locX := TXPathProcessor.Create();
    locX.Context.SetObject(locRootObject,nil);
    i := 12;
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%d]',[SIMPLE_PROP_B,i])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := 10;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%d]',[SIMPLE_PROP_B,i])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := 0;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%d]',[SIMPLE_PROP_B,i])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := 200;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%d]',[SIMPLE_PROP_B,i])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckNull), Ord(locX.Context.ContentKind));
  finally
    FreeAndNil(locX);
    FreeAndNil(locExp);
  end;
end;

procedure TXPathProcessor_Test.Execute_equal_date();
const
  SIMPLE_PROP_A = 'simpleProperty_A';
  LIST_PROP_A   = 'listProperty_A';
  SIMPLE_PROP_B = 'simpleProperty_B';
  PROP_TYPE     = DateTimeType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
    locRes : ISDODataFactory;
  begin
    locRes := TSDODataFactory.Create() as ISDODataFactory;
    locRes.AddType(s_uri,s_type_object_A,[]);
    locRes.AddType(s_uri,s_type_object_B,[]);
    locObj := locRes.getType(s_uri,s_type_object_A);
      locRes.addProperty(locObj,SIMPLE_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);
      locRes.addProperty(locObj,LIST_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[pfIsMany]);
      locRes.addProperty(locObj,'p_ab_list',s_uri,s_type_object_B,[pfIsMany,pfIsContainment]);

    locObj := locRes.getType(s_uri,s_type_object_B);
      locRes.addProperty(locObj,SIMPLE_PROP_B,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);

    Result := locRes;
  end;

  function ComputeDate(const AIntValue : TSDOInteger) : TSDODateTime;
  begin
    Result.Date := 39000.0 + ( AIntValue * 100 ) + ( AIntValue * 10 ) + AIntValue;
    Result.HourOffset := ( AIntValue mod 12 );
    Result.MinuteOffset := ( AIntValue mod 60 );
    Result := xsd_StrToDate(xsd_DateTimeToStr(Result,xdkDateTime),xdkDateTime);
  end;

  procedure fill_p_ab_list(const AList : ISDODataObjectList; const ACount : Byte; const AFac : ISDODataFactory);
  var
    tmp : ISDODataObject;
    k : TSDOInteger;
  begin
    for k := 0 to Pred(ACount) do begin
      tmp := AFac.createNew(s_uri, s_type_object_B);
      tmp.setDate(SIMPLE_PROP_B,ComputeDate(k));
      AList.append(tmp);
    end;
  end;

var
  locFac : ISDODataFactory;
  locExp : TXPathExpression;
  locX : TXPathProcessor;
  locRootObject : ISDODataObject;
  tmpListAB : ISDODataObjectList;
  i : TSDOInteger;
begin
  locFac := CreateFactory();
  locRootObject := locFac.createNew(s_uri,s_type_object_A);
    tmpListAB := locRootObject.getList('p_ab_list');
  fill_p_ab_list(tmpListAB,100,locFac);

  locX := nil;
  locExp := TXPathExpression.Create();
  try
    locX := TXPathProcessor.Create();
    locX.Context.SetObject(locRootObject,nil);
    i := 12;
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s="%s"]',[SIMPLE_PROP_B,xsd_DateTimeToStr(ComputeDate(i),xdkDateTime)])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := 10;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s="%s"]',[SIMPLE_PROP_B,xsd_DateTimeToStr(ComputeDate(i),xdkDateTime)])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := 56;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s="%s"]',[SIMPLE_PROP_B,xsd_DateTimeToStr(ComputeDate(i),xdkDateTime)])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := -200;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s="%s"]',[SIMPLE_PROP_B,xsd_DateTimeToStr(ComputeDate(i),xdkDateTime)])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckNull), Ord(locX.Context.ContentKind));
  finally
    FreeAndNil(locX);
    FreeAndNil(locExp);
  end;
end;

procedure TXPathProcessor_Test.Execute_equal_integer();
const
  SIMPLE_PROP_A = 'simpleProperty_A';
  LIST_PROP_A   = 'listProperty_A';
  SIMPLE_PROP_B = 'simpleProperty_B';
  PROP_TYPE     = IntegerType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
    locRes : ISDODataFactory;
  begin
    locRes := TSDODataFactory.Create() as ISDODataFactory;
    locRes.AddType(s_uri,s_type_object_A,[]);
    locRes.AddType(s_uri,s_type_object_B,[]);
    locObj := locRes.getType(s_uri,s_type_object_A);
      locRes.addProperty(locObj,SIMPLE_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);
      locRes.addProperty(locObj,LIST_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[pfIsMany]);
      locRes.addProperty(locObj,'p_ab_list',s_uri,s_type_object_B,[pfIsMany,pfIsContainment]);

    locObj := locRes.getType(s_uri,s_type_object_B);
      locRes.addProperty(locObj,SIMPLE_PROP_B,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);

    Result := locRes;
  end;

  procedure fill_p_ab_list(const AList : ISDODataObjectList; const ACount : Byte; const AFac : ISDODataFactory);
  var
    tmp : ISDODataObject;
    k : TSDOInteger;
  begin
    for k := 0 to Pred(ACount) do begin
      tmp := AFac.createNew(s_uri, s_type_object_B);
      tmp.setInteger(SIMPLE_PROP_B,k);
      AList.append(tmp);
    end;
  end;

var
  locFac : ISDODataFactory;
  locExp : TXPathExpression;
  locX : TXPathProcessor;
  locRootObject : ISDODataObject;
  tmpListAB : ISDODataObjectList;
  i : TSDOInteger;
begin
  locFac := CreateFactory();
  locRootObject := locFac.createNew(s_uri,s_type_object_A);
    tmpListAB := locRootObject.getList('p_ab_list');
  fill_p_ab_list(tmpListAB,100,locFac);

  locX := nil;
  locExp := TXPathExpression.Create();
  try
    locX := TXPathProcessor.Create();
    locX.Context.SetObject(locRootObject,nil);
    i := 12;
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%d]',[SIMPLE_PROP_B,i])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := 10;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%d]',[SIMPLE_PROP_B,i])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := 0;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%d]',[SIMPLE_PROP_B,i])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := -200;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%d]',[SIMPLE_PROP_B,i])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckNull), Ord(locX.Context.ContentKind));
  finally
    FreeAndNil(locX);
    FreeAndNil(locExp);
  end;
end;

procedure TXPathProcessor_Test.Execute_equal_string();
const
  SIMPLE_PROP_A = 'simpleProperty_A';
  LIST_PROP_A   = 'listProperty_A';
  SIMPLE_PROP_B = 'simpleProperty_B';
  PROP_TYPE     = StringType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
    locRes : ISDODataFactory;
  begin
    locRes := TSDODataFactory.Create() as ISDODataFactory;
    locRes.AddType(s_uri,s_type_object_A,[]);
    locRes.AddType(s_uri,s_type_object_B,[]);
    locObj := locRes.getType(s_uri,s_type_object_A);
      locRes.addProperty(locObj,SIMPLE_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);
      locRes.addProperty(locObj,LIST_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[pfIsMany]);
      locRes.addProperty(locObj,'p_ab_list',s_uri,s_type_object_B,[pfIsMany,pfIsContainment]);

    locObj := locRes.getType(s_uri,s_type_object_B);
      locRes.addProperty(locObj,SIMPLE_PROP_B,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);

    Result := locRes;
  end;

  procedure fill_p_ab_list(const AList : ISDODataObjectList; const ACount : Byte; const AFac : ISDODataFactory);
  var
    tmp : ISDODataObject;
    k : TSDOInteger;
  begin
    for k := 0 to Pred(ACount) do begin
      tmp := AFac.createNew(s_uri, s_type_object_B);
      tmp.setString(SIMPLE_PROP_B,Format('a sample string #%d',[k]));
      AList.append(tmp);
    end;
  end;

var
  locFac : ISDODataFactory;
  locExp : TXPathExpression;
  locX : TXPathProcessor;
  locRootObject : ISDODataObject;
  tmpListAB : ISDODataObjectList;
  i : TSDOInteger;
begin
  locFac := CreateFactory();
  locRootObject := locFac.createNew(s_uri,s_type_object_A);
    tmpListAB := locRootObject.getList('p_ab_list');
  fill_p_ab_list(tmpListAB,100,locFac);

  locX := nil;
  locExp := TXPathExpression.Create();
  try
    locX := TXPathProcessor.Create();
    locX.Context.SetObject(locRootObject,nil);
    i := 12;
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s="%s"]',[SIMPLE_PROP_B,Format('a sample string #%d',[i])])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := 10;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s="%s"]',[SIMPLE_PROP_B,Format('a sample string #%d',[i])])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := 0;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s="%s"]',[SIMPLE_PROP_B,Format('a sample string #%d',[i])])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := -200;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s="%s"]',[SIMPLE_PROP_B,Format('a sample string #%d',[i])])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckNull), Ord(locX.Context.ContentKind));
  finally
    FreeAndNil(locX);
    FreeAndNil(locExp);
  end;
end;

{$IFDEF HAS_SDO_BYTES}
procedure TXPathProcessor_Test.Execute_equal_bytes();
const
  SIMPLE_PROP_A = 'simpleProperty_A';
  LIST_PROP_A   = 'listProperty_A';
  SIMPLE_PROP_B = 'simpleProperty_B';
  PROP_TYPE     = BytesType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
    locRes : ISDODataFactory;
  begin
    locRes := TSDODataFactory.Create() as ISDODataFactory;
    locRes.AddType(s_uri,s_type_object_A,[]);
    locRes.AddType(s_uri,s_type_object_B,[]);
    locObj := locRes.getType(s_uri,s_type_object_A);
      locRes.addProperty(locObj,SIMPLE_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);
      locRes.addProperty(locObj,LIST_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[pfIsMany]);
      locRes.addProperty(locObj,'p_ab_list',s_uri,s_type_object_B,[pfIsMany,pfIsContainment]);

    locObj := locRes.getType(s_uri,s_type_object_B);
      locRes.addProperty(locObj,SIMPLE_PROP_B,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);

    Result := locRes;
  end;

  function ComputeValue(const AInput : Byte) : TSDOBytes;
  var
    k : Integer;
  begin
    SetLength(Result,AInput);
    if ( AInput > 0 ) then begin
      for k := 1 to AInput do
        Result[k-1] := k;
    end;
  end;
  
  procedure fill_p_ab_list(const AList : ISDODataObjectList; const ACount : Byte; const AFac : ISDODataFactory);
  var
    tmp : ISDODataObject;
    k : Byte;
  begin
    for k := 0 to Pred(ACount) do begin
      tmp := AFac.createNew(s_uri, s_type_object_B);
      tmp.setBytes(SIMPLE_PROP_B,ComputeValue(k));
      AList.append(tmp);
    end;
  end;

var
  locFac : ISDODataFactory;
  locExp : TXPathExpression;
  locX : TXPathProcessor;
  locRootObject : ISDODataObject;
  tmpListAB : ISDODataObjectList;
  i : Byte;
begin
  locFac := CreateFactory();
  locRootObject := locFac.createNew(s_uri,s_type_object_A);
    tmpListAB := locRootObject.getList('p_ab_list');
  fill_p_ab_list(tmpListAB,200,locFac);

  locX := nil;
  locExp := TXPathExpression.Create();
  try
    locX := TXPathProcessor.Create();
    locX.Context.SetObject(locRootObject,nil);
    i := 12;
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s="%s"]',[SIMPLE_PROP_B,BytesToString(ComputeValue(i))])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := 10;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s="%s"]',[SIMPLE_PROP_B,BytesToString(ComputeValue(i))])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
        
    i := 150;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s="%s"]',[SIMPLE_PROP_B,BytesToString(ComputeValue(i))])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
        
    i := 0;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s="%s"]',[SIMPLE_PROP_B,BytesToString(ComputeValue(i))])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());

    locX.Context.SetObject(locRootObject,nil);
    locExp.SetRoot(ParseXPath(Format('p_ab_list[%s="%s"]',[SIMPLE_PROP_B,'!'])));
    locX.Execute(locExp);
      CheckEquals(Ord(xckNull), Ord(locX.Context.ContentKind));
  finally
    FreeAndNil(locX);
    FreeAndNil(locExp);
  end;
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
procedure TXPathProcessor_Test.Execute_equal_char();
const
  SIMPLE_PROP_A = 'simpleProperty_A';
  LIST_PROP_A   = 'listProperty_A';
  SIMPLE_PROP_B = 'simpleProperty_B';
  PROP_TYPE     = CharacterType;
var
  CHAR_LIST : array[0..( (26{a-z} + (26{A-Z}) + (10{0-9})) - 1 )] of TSDOChar;

  procedure PrepareCharList();
  var
    k : Integer;
  begin
    for k := Ord('a') to Ord('z') do
      CHAR_LIST[k - Ord('a')] := TSDOChar(k);
    for k := Ord('A') to Ord('Z') do
      CHAR_LIST[26 + k - Ord('A')] := TSDOChar(k);
    for k := Ord('0') to Ord('9') do
      CHAR_LIST[26 + 26 + k - Ord('0')] := TSDOChar(k);
  end;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
    locRes : ISDODataFactory;
  begin
    locRes := TSDODataFactory.Create() as ISDODataFactory;
    locRes.AddType(s_uri,s_type_object_A,[]);
    locRes.AddType(s_uri,s_type_object_B,[]);
    locObj := locRes.getType(s_uri,s_type_object_A);
      locRes.addProperty(locObj,SIMPLE_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);
      locRes.addProperty(locObj,LIST_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[pfIsMany]);
      locRes.addProperty(locObj,'p_ab_list',s_uri,s_type_object_B,[pfIsMany,pfIsContainment]);

    locObj := locRes.getType(s_uri,s_type_object_B);
      locRes.addProperty(locObj,SIMPLE_PROP_B,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);

    Result := locRes;
  end;

  function ComputeValue(const AInput : Byte) : TSDOChar;
  begin
    Result := CHAR_LIST[AInput];
  end;
  
  procedure fill_p_ab_list(const AList : ISDODataObjectList; const ACount : Byte; const AFac : ISDODataFactory);
  var
    tmp : ISDODataObject;
    k : Byte;
  begin
    for k := 0 to Pred(ACount) do begin
      tmp := AFac.createNew(s_uri, s_type_object_B);
      tmp.setCharacter(SIMPLE_PROP_B,ComputeValue(k));
      AList.append(tmp);
    end;
  end;

var
  locFac : ISDODataFactory;
  locExp : TXPathExpression;
  locX : TXPathProcessor;
  locRootObject : ISDODataObject;
  tmpListAB : ISDODataObjectList;
  i : Byte;
begin
  PrepareCharList();
  locFac := CreateFactory();
  locRootObject := locFac.createNew(s_uri,s_type_object_A);
    tmpListAB := locRootObject.getList('p_ab_list');
  fill_p_ab_list(tmpListAB,High(CHAR_LIST),locFac);

  locX := nil;
  locExp := TXPathExpression.Create();
  try
    locX := TXPathProcessor.Create();
    locX.Context.SetObject(locRootObject,nil);
    i := 12;
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s="%s"]',[SIMPLE_PROP_B,ComputeValue(i)])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := 10;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s="%s"]',[SIMPLE_PROP_B,ComputeValue(i)])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := 0;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s="%s"]',[SIMPLE_PROP_B,ComputeValue(i)])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());

    locX.Context.SetObject(locRootObject,nil);
    locExp.SetRoot(ParseXPath(Format('p_ab_list[%s="%s"]',[SIMPLE_PROP_B,'!'])));
    locX.Execute(locExp);
      CheckEquals(Ord(xckNull), Ord(locX.Context.ContentKind));
  finally
    FreeAndNil(locX);
    FreeAndNil(locExp);
  end;
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
procedure TXPathProcessor_Test.Execute_equal_currency();
const
  SIMPLE_PROP_A = 'simpleProperty_A';
  LIST_PROP_A   = 'listProperty_A';
  SIMPLE_PROP_B = 'simpleProperty_B';
  PROP_TYPE   = CurrencyType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
    locRes : ISDODataFactory;
  begin
    locRes := TSDODataFactory.Create() as ISDODataFactory;
    locRes.AddType(s_uri,s_type_object_A,[]);
    locRes.AddType(s_uri,s_type_object_B,[]);
    locObj := locRes.getType(s_uri,s_type_object_A);
      locRes.addProperty(locObj,SIMPLE_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);
      locRes.addProperty(locObj,LIST_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[pfIsMany]);
      locRes.addProperty(locObj,'p_ab_list',s_uri,s_type_object_B,[pfIsMany,pfIsContainment]);

    locObj := locRes.getType(s_uri,s_type_object_B);
      locRes.addProperty(locObj,SIMPLE_PROP_B,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);

    Result := locRes;
  end;

  function ComputeValue(const AInput : Integer) : TSDOCurrency;
  begin
    Result := AInput * 10000000;
    if ( ( AInput mod 2) = 0 ) then
      Result := -Result;
  end;
  
  procedure fill_p_ab_list(const AList : ISDODataObjectList; const ACount : Byte; const AFac : ISDODataFactory);
  var
    tmp : ISDODataObject;
    k : Byte;
  begin
    for k := 0 to Pred(ACount) do begin
      tmp := AFac.createNew(s_uri, s_type_object_B);
      tmp.setCurrency(SIMPLE_PROP_B,ComputeValue(k));
      AList.append(tmp);
    end;
  end;

var
  locFac : ISDODataFactory;
  locExp : TXPathExpression;
  locX : TXPathProcessor;
  locRootObject : ISDODataObject;
  tmpListAB : ISDODataObjectList;
  v : TSDOCurrency;
  j : Integer;
begin
  locFac := CreateFactory();
  locRootObject := locFac.createNew(s_uri,s_type_object_A);
    tmpListAB := locRootObject.getList('p_ab_list');
  fill_p_ab_list(tmpListAB,100,locFac);

  locX := nil;
  locExp := TXPathExpression.Create();
  try
    locX := TXPathProcessor.Create();
    locX.Context.SetObject(locRootObject,nil);
    j := 12; v := j;
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%s]',[SIMPLE_PROP_B,TSDOConvertHelper.CurrencyToString(ComputeValue(j))])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(j)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(v, tmpListAB.getCursor().GetPosition());
    j := 10; v := j;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%s]',[SIMPLE_PROP_B,TSDOConvertHelper.CurrencyToString(ComputeValue(j))])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(j)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(v, tmpListAB.getCursor().GetPosition());
    j := 0; v := j;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%s]',[SIMPLE_PROP_B,TSDOConvertHelper.CurrencyToString(ComputeValue(j))])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(j)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(v, tmpListAB.getCursor().GetPosition());
    j := 200; 
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%s]',[SIMPLE_PROP_B,TSDOConvertHelper.CurrencyToString(ComputeValue(j))])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckNull), Ord(locX.Context.ContentKind));
  finally
    FreeAndNil(locX);
    FreeAndNil(locExp);
  end;
end;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
procedure TXPathProcessor_Test.Execute_equal_double();
const
  SIMPLE_PROP_A = 'simpleProperty_A';
  LIST_PROP_A   = 'listProperty_A';
  SIMPLE_PROP_B = 'simpleProperty_B';
  PROP_TYPE   = DoubleType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
    locRes : ISDODataFactory;
  begin
    locRes := TSDODataFactory.Create() as ISDODataFactory;
    locRes.AddType(s_uri,s_type_object_A,[]);
    locRes.AddType(s_uri,s_type_object_B,[]);
    locObj := locRes.getType(s_uri,s_type_object_A);
      locRes.addProperty(locObj,SIMPLE_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);
      locRes.addProperty(locObj,LIST_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[pfIsMany]);
      locRes.addProperty(locObj,'p_ab_list',s_uri,s_type_object_B,[pfIsMany,pfIsContainment]);

    locObj := locRes.getType(s_uri,s_type_object_B);
      locRes.addProperty(locObj,SIMPLE_PROP_B,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);

    Result := locRes;
  end;

  function ComputeValue(const AInput : Integer) : TSDODouble;
  begin
    Result := AInput * 10000000;
    if ( ( AInput mod 2) = 0 ) then
      Result := -Result;
  end;
  
  procedure fill_p_ab_list(const AList : ISDODataObjectList; const ACount : Byte; const AFac : ISDODataFactory);
  var
    tmp : ISDODataObject;
    k : Byte;
  begin
    for k := 0 to Pred(ACount) do begin
      tmp := AFac.createNew(s_uri, s_type_object_B);
      tmp.setDouble(SIMPLE_PROP_B,ComputeValue(k));
      AList.append(tmp);
    end;
  end;

var
  locFac : ISDODataFactory;
  locExp : TXPathExpression;
  locX : TXPathProcessor;
  locRootObject : ISDODataObject;
  tmpListAB : ISDODataObjectList;
  v : TSDODouble;
  j : Integer;
begin
  locFac := CreateFactory();
  locRootObject := locFac.createNew(s_uri,s_type_object_A);
    tmpListAB := locRootObject.getList('p_ab_list');
  fill_p_ab_list(tmpListAB,100,locFac);

  locX := nil;
  locExp := TXPathExpression.Create();
  try
    locX := TXPathProcessor.Create();
    locX.Context.SetObject(locRootObject,nil);
    j := 12; v := j;
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%s]',[SIMPLE_PROP_B,TSDOConvertHelper.FloatToString(ComputeValue(j))])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(j)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(v, tmpListAB.getCursor().GetPosition());
    j := 10; v := j;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%s]',[SIMPLE_PROP_B,TSDOConvertHelper.FloatToString(ComputeValue(j))])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(j)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(v, tmpListAB.getCursor().GetPosition());
    j := 0; v := j;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%s]',[SIMPLE_PROP_B,TSDOConvertHelper.FloatToString(ComputeValue(j))])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(j)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(v, tmpListAB.getCursor().GetPosition());
    j := 200;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%s]',[SIMPLE_PROP_B,TSDOConvertHelper.FloatToString(ComputeValue(j))])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckNull), Ord(locX.Context.ContentKind));
  finally
    FreeAndNil(locX);
    FreeAndNil(locExp);
  end;
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
procedure TXPathProcessor_Test.Execute_equal_float();
const
  SIMPLE_PROP_A = 'simpleProperty_A';
  LIST_PROP_A   = 'listProperty_A';
  SIMPLE_PROP_B = 'simpleProperty_B';
  PROP_TYPE   = FloatType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
    locRes : ISDODataFactory;
  begin
    locRes := TSDODataFactory.Create() as ISDODataFactory;
    locRes.AddType(s_uri,s_type_object_A,[]);
    locRes.AddType(s_uri,s_type_object_B,[]);
    locObj := locRes.getType(s_uri,s_type_object_A);
      locRes.addProperty(locObj,SIMPLE_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);
      locRes.addProperty(locObj,LIST_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[pfIsMany]);
      locRes.addProperty(locObj,'p_ab_list',s_uri,s_type_object_B,[pfIsMany,pfIsContainment]);

    locObj := locRes.getType(s_uri,s_type_object_B);
      locRes.addProperty(locObj,SIMPLE_PROP_B,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);

    Result := locRes;
  end;

  function ComputeValue(const AInput : Integer) : TSDOFloat;
  begin
    Result := AInput * 10000000;
    if ( ( AInput mod 2) = 0 ) then
      Result := -Result;
  end;
  
  procedure fill_p_ab_list(const AList : ISDODataObjectList; const ACount : Byte; const AFac : ISDODataFactory);
  var
    tmp : ISDODataObject;
    k : Byte;
  begin
    for k := 0 to Pred(ACount) do begin
      tmp := AFac.createNew(s_uri, s_type_object_B);
      tmp.setFloat(SIMPLE_PROP_B,ComputeValue(k));
      AList.append(tmp);
    end;
  end;

var
  locFac : ISDODataFactory;
  locExp : TXPathExpression;
  locX : TXPathProcessor;
  locRootObject : ISDODataObject;
  tmpListAB : ISDODataObjectList;
  v : TSDOFloat;
  j : Integer;
begin
  locFac := CreateFactory();
  locRootObject := locFac.createNew(s_uri,s_type_object_A);
    tmpListAB := locRootObject.getList('p_ab_list');
  fill_p_ab_list(tmpListAB,100,locFac);

  locX := nil;
  locExp := TXPathExpression.Create();
  try
    locX := TXPathProcessor.Create();
    locX.Context.SetObject(locRootObject,nil);
    j := 12; v := j;
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%s]',[SIMPLE_PROP_B,TSDOConvertHelper.FloatToString(ComputeValue(j))])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(j)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(v, tmpListAB.getCursor().GetPosition());
    j := 10; v := j;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%s]',[SIMPLE_PROP_B,TSDOConvertHelper.FloatToString(ComputeValue(j))])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(j)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(v, tmpListAB.getCursor().GetPosition());
    j := 0; v := j;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%s]',[SIMPLE_PROP_B,TSDOConvertHelper.FloatToString(ComputeValue(j))])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(j)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(v, tmpListAB.getCursor().GetPosition());
    j := 200;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%s]',[SIMPLE_PROP_B,TSDOConvertHelper.FloatToString(ComputeValue(j))])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckNull), Ord(locX.Context.ContentKind));
  finally
    FreeAndNil(locX);
    FreeAndNil(locExp);
  end;
end;
{$ENDIF HAS_SDO_FLOAT}

{$IFDEF HAS_SDO_LONG}
procedure TXPathProcessor_Test.Execute_equal_long();
const
  SIMPLE_PROP_A = 'simpleProperty_A';
  LIST_PROP_A   = 'listProperty_A';
  SIMPLE_PROP_B = 'simpleProperty_B';
  PROP_TYPE   = LongType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
    locRes : ISDODataFactory;
  begin
    locRes := TSDODataFactory.Create() as ISDODataFactory;
    locRes.AddType(s_uri,s_type_object_A,[]);
    locRes.AddType(s_uri,s_type_object_B,[]);
    locObj := locRes.getType(s_uri,s_type_object_A);
      locRes.addProperty(locObj,SIMPLE_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);
      locRes.addProperty(locObj,LIST_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[pfIsMany]);
      locRes.addProperty(locObj,'p_ab_list',s_uri,s_type_object_B,[pfIsMany,pfIsContainment]);

    locObj := locRes.getType(s_uri,s_type_object_B);
      locRes.addProperty(locObj,SIMPLE_PROP_B,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);

    Result := locRes;
  end;

  function ComputeValue(const AInput : TSDOLong) : TSDOLong;
  begin
    Result := AInput * 10000000;
    if ( ( AInput mod 2) = 0 ) then
      Result := -Result;
  end;
  
  procedure fill_p_ab_list(const AList : ISDODataObjectList; const ACount : Byte; const AFac : ISDODataFactory);
  var
    tmp : ISDODataObject;
    k : Byte;
  begin
    for k := 0 to Pred(ACount) do begin
      tmp := AFac.createNew(s_uri, s_type_object_B);
      tmp.setLong(SIMPLE_PROP_B,ComputeValue(k));
      AList.append(tmp);
    end;
  end;

var
  locFac : ISDODataFactory;
  locExp : TXPathExpression;
  locX : TXPathProcessor;
  locRootObject : ISDODataObject;
  tmpListAB : ISDODataObjectList;
  i : TSDOLong;
begin
  locFac := CreateFactory();
  locRootObject := locFac.createNew(s_uri,s_type_object_A);
    tmpListAB := locRootObject.getList('p_ab_list');
  fill_p_ab_list(tmpListAB,100,locFac);

  locX := nil;
  locExp := TXPathExpression.Create();
  try
    locX := TXPathProcessor.Create();
    locX.Context.SetObject(locRootObject,nil);
    i := 12;
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%d]',[SIMPLE_PROP_B,ComputeValue(i)])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := 10;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%d]',[SIMPLE_PROP_B,ComputeValue(i)])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := 0;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%d]',[SIMPLE_PROP_B,ComputeValue(i)])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := 200;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%d]',[SIMPLE_PROP_B,ComputeValue(i)])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckNull), Ord(locX.Context.ContentKind));
  finally
    FreeAndNil(locX);
    FreeAndNil(locExp);
  end;
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure TXPathProcessor_Test.Execute_equal_short();
const
  SIMPLE_PROP_A = 'simpleProperty_A';
  LIST_PROP_A   = 'listProperty_A';
  SIMPLE_PROP_B = 'simpleProperty_B';
  PROP_TYPE   = ShortType;

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
    locRes : ISDODataFactory;
  begin
    locRes := TSDODataFactory.Create() as ISDODataFactory;
    locRes.AddType(s_uri,s_type_object_A,[]);
    locRes.AddType(s_uri,s_type_object_B,[]);
    locObj := locRes.getType(s_uri,s_type_object_A);
      locRes.addProperty(locObj,SIMPLE_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);
      locRes.addProperty(locObj,LIST_PROP_A,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE],[pfIsMany]);
      locRes.addProperty(locObj,'p_ab_list',s_uri,s_type_object_B,[pfIsMany,pfIsContainment]);

    locObj := locRes.getType(s_uri,s_type_object_B);
      locRes.addProperty(locObj,SIMPLE_PROP_B,sdo_namespace,SDOTypeDefaultTypeNames[PROP_TYPE], []);

    Result := locRes;
  end;

  function ComputeValue(const AInput : TSDOShort) : TSDOShort;
  begin
    Result := AInput * 100 + AInput;
    if ( ( AInput mod 2) = 0 ) then
      Result := -Result;
  end;
  
  procedure fill_p_ab_list(const AList : ISDODataObjectList; const ACount : Byte; const AFac : ISDODataFactory);
  var
    tmp : ISDODataObject;
    k : Byte;
  begin
    for k := 0 to Pred(ACount) do begin
      tmp := AFac.createNew(s_uri, s_type_object_B);
      tmp.setShort(SIMPLE_PROP_B,ComputeValue(k));
      AList.append(tmp);
    end;
  end;

var
  locFac : ISDODataFactory;
  locExp : TXPathExpression;
  locX : TXPathProcessor;
  locRootObject : ISDODataObject;
  tmpListAB : ISDODataObjectList;
  i : TSDOShort;
begin
  locFac := CreateFactory();
  locRootObject := locFac.createNew(s_uri,s_type_object_A);
    tmpListAB := locRootObject.getList('p_ab_list');
  fill_p_ab_list(tmpListAB,100,locFac);

  locX := nil;
  locExp := TXPathExpression.Create();
  try
    locX := TXPathProcessor.Create();
    locX.Context.SetObject(locRootObject,nil);
    i := 12;
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%d]',[SIMPLE_PROP_B,ComputeValue(i)])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := 10;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%d]',[SIMPLE_PROP_B,ComputeValue(i)])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := 0;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%d]',[SIMPLE_PROP_B,ComputeValue(i)])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpListAB.getDataObject(i)), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty('p_ab_list')), PtrUInt(locX.Context.CurrentProperty));
        CheckEquals(i, tmpListAB.getCursor().GetPosition());
    i := 200;
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(Format('p_ab_list[%s=%d]',[SIMPLE_PROP_B,ComputeValue(i)])));
      locX.Execute(locExp);
        CheckEquals(Ord(xckNull), Ord(locX.Context.ContentKind));
  finally
    FreeAndNil(locX);
    FreeAndNil(locExp);
  end;
end;
{$ENDIF HAS_SDO_SHORT}

procedure TXPathProcessor_Test.Execute_nested_object;
  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
    locRes : ISDODataFactory;
  begin
    locRes := TSDODataFactory.Create() as ISDODataFactory;
    locRes.AddType(s_uri,s_type_object_A,[]);
    locRes.AddType(s_uri,s_type_object_B,[]);
    locRes.AddType(s_uri,s_type_object_C,[]);
    locRes.AddType(s_uri,s_type_object_D,[]);
    locObj := locRes.getType(s_uri,s_type_object_A);
      locRes.addProperty(locObj,s_bool_prop,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType], []);
      locRes.addProperty(locObj,s_bool_propList,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsMany]);
      locRes.addProperty(locObj,s_integer_prop,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType], []);
      locRes.addProperty(locObj,s_integer_propList,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsMany]);
      locRes.addProperty(locObj,s_string_prop,sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
      locRes.addProperty(locObj,s_string_propList,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsMany]);
      locRes.addProperty(locObj,'p_ab',s_uri,s_type_object_B,[pfIsContainment]);
      locRes.addProperty(locObj,'p_ab_list',s_uri,s_type_object_B,[pfIsMany,pfIsContainment]);
      locRes.addProperty(locObj,'p_ac',s_uri,s_type_object_C,[pfIsContainment]);

    locObj := locRes.getType(s_uri,s_type_object_B);
      locRes.addProperty(locObj,s_bool_prop,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType], []);
      locRes.addProperty(locObj,s_bool_propList,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsMany]);
      locRes.addProperty(locObj,s_integer_prop,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType], []);
      locRes.addProperty(locObj,s_integer_propList,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsMany]);
      locRes.addProperty(locObj,s_string_prop,sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
      locRes.addProperty(locObj,s_string_propList,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsMany]);
      locRes.addProperty(locObj,'p_bc',s_uri,s_type_object_C,[pfIsContainment]);
      locRes.addProperty(locObj,'p_bc_list',s_uri,s_type_object_C,[pfIsMany,pfIsContainment]);

    locObj := locRes.getType(s_uri,s_type_object_C);
      locRes.addProperty(locObj,'p_cd',s_uri,s_type_object_D,[pfIsContainment]);

    Result := locRes;
  end;

var
  locFac : ISDODataFactory;
  locExp : TXPathExpression;
  locX : TXPathProcessor;
  locRootObject, tmpAB, tmpAC, tmpAB_BC, tmpAB_BC_CD : ISDODataObject;
begin
  locFac := CreateFactory();
  locRootObject := locFac.createNew(s_uri,s_type_object_A);
    tmpAC := locRootObject.createDataObject('p_ac');
    tmpAB := locRootObject.createDataObject('p_ab');
      tmpAB_BC := tmpAB.createDataObject('p_bc');
        tmpAB_BC_CD := tmpAB_BC.createDataObject('p_cd');
  locX := nil;
  locExp := TXPathExpression.Create();
  try
    locX := TXPathProcessor.Create();
    locX.Context.SetObject(locRootObject,nil);
    locExp.SetRoot(ParseXPath('p_ab/p_bc/p_cd'));
    locX.Execute(locExp);
      CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
      CheckEquals(PtrUInt(tmpAB_BC_CD), PtrUInt(locX.Context.ObjectItem));
      CheckEquals(PtrUInt(tmpAB_BC.getProperty('p_cd')), PtrUInt(locX.Context.CurrentProperty));
  finally
    FreeAndNil(locX);
    FreeAndNil(locExp);
  end;

  locX := nil;
  locExp := TXPathExpression.Create();
  try
    locX := TXPathProcessor.Create();
    locX.Context.SetObject(locRootObject,nil);
    locExp.SetRoot(ParseXPath('p_ab/p_bc'));
    locX.Execute(locExp);
      CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
      CheckEquals(PtrUInt(tmpAB_BC), PtrUInt(locX.Context.ObjectItem));
      CheckEquals(PtrUInt(tmpAB.getProperty('p_bc')), PtrUInt(locX.Context.CurrentProperty));
  finally
    FreeAndNil(locX);
    FreeAndNil(locExp);
  end;

  locRootObject := nil;
    tmpAC := nil;
    tmpAB := nil;
      tmpAB_BC := nil;
        tmpAB_BC_CD := nil;

  locRootObject := locFac.createNew(s_uri,s_type_object_A);
    tmpAC := nil;
    tmpAB := locRootObject.createDataObject('p_ab');
      tmpAB_BC := tmpAB.createDataObject('p_bc');
        tmpAB_BC_CD := nil;
  locX := nil;
  locExp := TXPathExpression.Create();
  try
    locX := TXPathProcessor.Create();
    locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath('p_ac'));
        locX.Execute(locExp);
          CheckEquals(Ord(xckNull), Ord(locX.Context.ContentKind));
          CheckEquals(PtrUInt(locRootObject.getProperty('p_ac')), PtrUInt(locX.Context.CurrentProperty));
    locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath('p_ab/p_bc'));
      locX.Execute(locExp);
        CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpAB_BC), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(tmpAB.getProperty('p_bc')), PtrUInt(locX.Context.CurrentProperty));
    locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath('p_ab/p_bc/p_cd'));
      locX.Execute(locExp);
        CheckEquals(Ord(xckNull), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(tmpAB_BC.getProperty('p_cd')), PtrUInt(locX.Context.CurrentProperty));
  finally
    FreeAndNil(locX);
    FreeAndNil(locExp);
  end;
end;

procedure TXPathProcessor_Test.Execute_simple_object();
  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
    locRes : ISDODataFactory;
  begin
    locRes := TSDODataFactory.Create() as ISDODataFactory;
    locRes.AddType(s_uri,s_type_object_A,[]);
    locRes.AddType(s_uri,s_type_object_B,[]);
    locObj := locRes.getType(s_uri,s_type_object_A);
      locRes.addProperty(locObj,s_bool_prop,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType], []);
      locRes.addProperty(locObj,s_bool_propList,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsMany]);
      locRes.addProperty(locObj,s_integer_prop,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType], []);
      locRes.addProperty(locObj,s_integer_propList,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsMany]);
      locRes.addProperty(locObj,s_string_prop,sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
      locRes.addProperty(locObj,s_string_propList,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsMany]);
      locRes.addProperty(locObj,s_object_prop,s_uri,s_type_object_B,[pfIsContainment]);
      locRes.addProperty(locObj,s_object_propList,s_uri,s_type_object_B,[pfIsMany,pfIsContainment]);

    Result := locRes;
  end;

var
  locFac : ISDODataFactory;
  locExp : TXPathExpression;
  locX : TXPathProcessor;
  locRootObject : ISDODataObject;
begin
  locFac := CreateFactory();
  locRootObject := locFac.createNew(s_uri,s_type_object_A);
    locRootObject.setBoolean(s_bool_prop,False);
    locRootObject.setInteger(s_integer_prop,1210);
    locRootObject.setString(s_string_prop,'sdo-wst-azerty');
    locRootObject.createDataObject(s_object_prop);

  locX := nil;
  locExp := TXPathExpression.Create();
  try
    locX := TXPathProcessor.Create();
    locX.Context.SetObject(locRootObject,nil);
    locExp.SetRoot(ParseXPath(s_object_prop));
    locX.Execute(locExp);
      CheckEquals(Ord(xckObject), Ord(locX.Context.ContentKind));
      CheckEquals(PtrUInt(locRootObject.getDataObject(s_object_prop)), PtrUInt(locX.Context.ObjectItem));
      CheckEquals(PtrUInt(locRootObject.getProperty(s_object_prop)), PtrUInt(locX.Context.CurrentProperty));
  finally
    FreeAndNil(locX);
    FreeAndNil(locExp);
  end;
end;

procedure TXPathProcessor_Test.Execute_simple_value_type;
const SIMPLE_TYPE_PROPS : array[0..2] of string = ( s_bool_prop, s_integer_prop, s_string_prop);

  function CreateFactory() : ISDODataFactory;
  var
    locObj : ISDOType;
    locRes : ISDODataFactory;
  begin
    locRes := TSDODataFactory.Create() as ISDODataFactory;
    locRes.AddType(s_uri,s_type_object_A,[]);
    locObj := locRes.getType(s_uri,s_type_object_A);
      locRes.addProperty(locObj,s_bool_prop,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType], []);
      locRes.addProperty(locObj,s_bool_propList,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsMany]);
      locRes.addProperty(locObj,s_integer_prop,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType], []);
      locRes.addProperty(locObj,s_integer_propList,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[pfIsMany]);
      locRes.addProperty(locObj,s_string_prop,sdo_namespace,SDOTypeDefaultTypeNames[StringType], []);
      locRes.addProperty(locObj,s_string_propList,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsMany]);

    Result := locRes;
  end;

var
  locFac : ISDODataFactory;
  locExp : TXPathExpression;
  locX : TXPathProcessor;
  locRootObject : ISDODataObject;
  i : PtrInt;
  propName : string;
begin
  locFac := CreateFactory();
  locRootObject := locFac.createNew(s_uri,s_type_object_A);

  for i := Low(SIMPLE_TYPE_PROPS) to High(SIMPLE_TYPE_PROPS) do begin
    propName := SIMPLE_TYPE_PROPS[i];
    locX := nil;
    locExp := TXPathExpression.Create();
    try
      locX := TXPathProcessor.Create();
      locX.Context.SetObject(locRootObject,nil);
      locExp.SetRoot(ParseXPath(propName));
      locX.Execute(locExp);
        CheckEquals(Ord(xckValue), Ord(locX.Context.ContentKind));
        CheckEquals(PtrUInt(locRootObject), PtrUInt(locX.Context.ObjectItem));
        CheckEquals(PtrUInt(locRootObject.getProperty(propName)), PtrUInt(locX.Context.CurrentProperty));
    finally
      FreeAndNil(locX);
      FreeAndNil(locExp);
    end;
  end;
end;

{ TXPathScanner_Test }

procedure TXPathScanner_Test.composed_1();
var
  locObj : TXPathScanner;
begin
  locObj := TXPathScanner.Create('object/@prop_a/prop_b[2]/prop_c[prop_d="azerty"]');
  try
    CheckEquals(Ord(xtkSymbol), Ord(locObj.NextToken()));
      CheckEquals(Ord(xtkSymbol), Ord(locObj.Token));
      CheckEquals('object',locObj.TokenString);
    CheckEquals(Ord(xtkSlash), Ord(locObj.NextToken()));
      CheckEquals(Ord(xtkSlash), Ord(locObj.Token));
      CheckEquals('/',locObj.TokenString);
    CheckEquals(Ord(xtkAt), Ord(locObj.NextToken()));
      CheckEquals(Ord(xtkAt), Ord(locObj.Token));
      CheckEquals('@',locObj.TokenString);
    CheckEquals(Ord(xtkSymbol), Ord(locObj.NextToken()));
      CheckEquals(Ord(xtkSymbol), Ord(locObj.Token));
      CheckEquals('prop_a',locObj.TokenString);
    CheckEquals(Ord(xtkSlash), Ord(locObj.NextToken()));
      CheckEquals(Ord(xtkSlash), Ord(locObj.Token));
      CheckEquals('/',locObj.TokenString);      
    CheckEquals(Ord(xtkSymbol), Ord(locObj.NextToken()));
      CheckEquals(Ord(xtkSymbol), Ord(locObj.Token));
      CheckEquals('prop_b',locObj.TokenString);
    CheckEquals(Ord(xtkSquaredBraceLeft), Ord(locObj.NextToken()));
      CheckEquals(Ord(xtkSquaredBraceLeft), Ord(locObj.Token));
      CheckEquals('[',locObj.TokenString);
    CheckEquals(Ord(xtkNumber), Ord(locObj.NextToken()));
      CheckEquals(Ord(xtkNumber), Ord(locObj.Token));
      CheckEquals(2,locObj.TokenInt);
    CheckEquals(Ord(xtkSquaredBraceRigth), Ord(locObj.NextToken()));
      CheckEquals(Ord(xtkSquaredBraceRigth), Ord(locObj.Token));
      CheckEquals(']',locObj.TokenString);
    CheckEquals(Ord(xtkSlash), Ord(locObj.NextToken()));
      CheckEquals(Ord(xtkSlash), Ord(locObj.Token));
      CheckEquals('/',locObj.TokenString);      
    CheckEquals(Ord(xtkSymbol), Ord(locObj.NextToken()));
      CheckEquals(Ord(xtkSymbol), Ord(locObj.Token));
      CheckEquals('prop_c',locObj.TokenString);
    CheckEquals(Ord(xtkSquaredBraceLeft), Ord(locObj.NextToken()));
      CheckEquals(Ord(xtkSquaredBraceLeft), Ord(locObj.Token));
      CheckEquals('[',locObj.TokenString);
    CheckEquals(Ord(xtkSymbol), Ord(locObj.NextToken()));
      CheckEquals(Ord(xtkSymbol), Ord(locObj.Token));
      CheckEquals('prop_d',locObj.TokenString);      
    CheckEquals(Ord(xtkEqual), Ord(locObj.NextToken()));
      CheckEquals(Ord(xtkEqual), Ord(locObj.Token));
      CheckEquals('=',locObj.TokenString);
    CheckEquals(Ord(xtkString), Ord(locObj.NextToken()));
      CheckEquals(Ord(xtkString), Ord(locObj.Token));
      CheckEquals('azerty',locObj.TokenString);
    CheckEquals(Ord(xtkSquaredBraceRigth), Ord(locObj.NextToken()));
      CheckEquals(Ord(xtkSquaredBraceRigth), Ord(locObj.Token));
      CheckEquals(']',locObj.TokenString);      
  finally
    FreeAndNil(locObj);
  end;
end;

procedure TXPathScanner_Test.simple();
var
  locObj : TXPathScanner;
begin
  locObj := TXPathScanner.Create('');
  try
    CheckEquals(Ord(xtkEof), Ord(locObj.NextToken()));
  finally
    FreeAndNil(locObj);
  end;
  locObj := TXPathScanner.Create('null');
  try
    CheckEquals(Ord(xtkNull), Ord(locObj.NextToken()));
    CheckEquals(Ord(xtkNull), Ord(locObj.Token));
    CheckEquals('null', locObj.TokenString);
    CheckEquals(Ord(xtkEof), Ord(locObj.NextToken()));
  finally
    FreeAndNil(locObj);
  end;
  locObj := TXPathScanner.Create('true');
  try
    CheckEquals(Ord(xtkTrue), Ord(locObj.NextToken()));
    CheckEquals(Ord(xtkTrue), Ord(locObj.Token));
    CheckEquals('true', locObj.TokenString);
    CheckEquals(Ord(xtkEof), Ord(locObj.NextToken()));
  finally
    FreeAndNil(locObj);
  end;
  locObj := TXPathScanner.Create('false');
  try
    CheckEquals(Ord(xtkFalse), Ord(locObj.NextToken()));
    CheckEquals(Ord(xtkFalse), Ord(locObj.Token));
    CheckEquals('false', locObj.TokenString);
    CheckEquals(Ord(xtkEof), Ord(locObj.NextToken()));
  finally
    FreeAndNil(locObj);
  end;
  locObj := TXPathScanner.Create('sampleProperty_123');
  try
    CheckEquals(Ord(xtkSymbol), Ord(locObj.NextToken()));
    CheckEquals(Ord(xtkSymbol), Ord(locObj.Token));
    CheckEquals('sampleProperty_123', locObj.TokenString);
    CheckEquals(Ord(xtkEof), Ord(locObj.NextToken()));
  finally
    FreeAndNil(locObj);
  end;
  locObj := TXPathScanner.Create('.');
  try
    CheckEquals(Ord(xtkPeriod), Ord(locObj.NextToken()));
    CheckEquals(Ord(xtkPeriod), Ord(locObj.Token));
    CheckEquals('.', locObj.TokenString);
    CheckEquals(Ord(xtkEof), Ord(locObj.NextToken()));
  finally
    FreeAndNil(locObj);
  end;
  locObj := TXPathScanner.Create('..');
  try
    CheckEquals(Ord(xtkDoublePeriod), Ord(locObj.NextToken()));
    CheckEquals(Ord(xtkDoublePeriod), Ord(locObj.Token));
    CheckEquals('..', locObj.TokenString);
    CheckEquals(Ord(xtkEof), Ord(locObj.NextToken()));
  finally
    FreeAndNil(locObj);
  end;
  locObj := TXPathScanner.Create('/');
  try
    CheckEquals(Ord(xtkSlash), Ord(locObj.NextToken()));
    CheckEquals(Ord(xtkSlash), Ord(locObj.Token));
    CheckEquals('/', locObj.TokenString);
    CheckEquals(Ord(xtkEof), Ord(locObj.NextToken()));
  finally
    FreeAndNil(locObj);
  end;
  locObj := TXPathScanner.Create('[');
  try
    CheckEquals(Ord(xtkSquaredBraceLeft), Ord(locObj.NextToken()));
    CheckEquals(Ord(xtkSquaredBraceLeft), Ord(locObj.Token));
    CheckEquals('[', locObj.TokenString);
    CheckEquals(Ord(xtkEof), Ord(locObj.NextToken()));
  finally
    FreeAndNil(locObj);
  end;
  locObj := TXPathScanner.Create(']');
  try
    CheckEquals(Ord(xtkSquaredBraceRigth), Ord(locObj.NextToken()));
    CheckEquals(Ord(xtkSquaredBraceRigth), Ord(locObj.Token));
    CheckEquals(']', locObj.TokenString);
    CheckEquals(Ord(xtkEof), Ord(locObj.NextToken()));
  finally
    FreeAndNil(locObj);
  end;
  locObj := TXPathScanner.Create('@');
  try
    CheckEquals(Ord(xtkAt), Ord(locObj.NextToken()));
    CheckEquals(Ord(xtkAt), Ord(locObj.Token));
    CheckEquals('@', locObj.TokenString);
    CheckEquals(Ord(xtkEof), Ord(locObj.NextToken()));
  finally
    FreeAndNil(locObj);
  end;
end;

procedure TXPathScanner_Test.simple_number();
var
  locObj : TXPathScanner;
begin
  locObj := TXPathScanner.Create('0');
  try
    CheckEquals(Ord(xtkNumber), Ord(locObj.NextToken()));
    CheckEquals(Ord(xtkNumber), Ord(locObj.Token));
    CheckEquals('0', locObj.TokenString);
    CheckEquals(0, locObj.TokenInt);
    CheckEquals(0.0, locObj.TokenNumber,0.01);
    CheckEquals(Ord(xtkEof), Ord(locObj.NextToken()));
  finally
    FreeAndNil(locObj);
  end;
  locObj := TXPathScanner.Create('-0');
  try
    CheckEquals(Ord(xtkNumber), Ord(locObj.NextToken()));
    CheckEquals(Ord(xtkNumber), Ord(locObj.Token));
    CheckEquals('-0', locObj.TokenString);
    CheckEquals(0, locObj.TokenInt);
    CheckEquals(0.0, locObj.TokenNumber,0.01);
    CheckEquals(Ord(xtkEof), Ord(locObj.NextToken()));
  finally
    FreeAndNil(locObj);
  end;
  locObj := TXPathScanner.Create('.0');
  try
    CheckEquals(Ord(xtkNumber), Ord(locObj.NextToken()));
    CheckEquals(Ord(xtkNumber), Ord(locObj.Token));
    CheckEquals('0.0', locObj.TokenString);
    CheckEquals(0.0, locObj.TokenNumber,0.01);
    CheckEquals(Ord(xtkEof), Ord(locObj.NextToken()));
  finally
    FreeAndNil(locObj);
  end;
  locObj := TXPathScanner.Create('123');
  try
    CheckEquals(Ord(xtkNumber), Ord(locObj.NextToken()));
    CheckEquals(Ord(xtkNumber), Ord(locObj.Token));
    CheckEquals('123', locObj.TokenString);
    CheckEquals(123, locObj.TokenInt);
    CheckEquals(123.0, locObj.TokenNumber,0.01);
    CheckEquals(Ord(xtkEof), Ord(locObj.NextToken()));
  finally
    FreeAndNil(locObj);
  end;
  locObj := TXPathScanner.Create('1');
  try
    CheckEquals(Ord(xtkNumber), Ord(locObj.NextToken()));
    CheckEquals(Ord(xtkNumber), Ord(locObj.Token));
    CheckEquals('1', locObj.TokenString);
    CheckEquals(1, locObj.TokenInt);
    CheckEquals(1.0, locObj.TokenNumber,0.01);
    CheckEquals(Ord(xtkEof), Ord(locObj.NextToken()));
  finally
    FreeAndNil(locObj);
  end;
  locObj := TXPathScanner.Create('-123');
  try
    CheckEquals(Ord(xtkNumber), Ord(locObj.NextToken()));
    CheckEquals(Ord(xtkNumber), Ord(locObj.Token));
    CheckEquals('-123', locObj.TokenString);
    CheckEquals(-123, locObj.TokenInt);
    CheckEquals(-123.0, locObj.TokenNumber,0.01);
    CheckEquals(Ord(xtkEof), Ord(locObj.NextToken()));
  finally
    FreeAndNil(locObj);
  end;
  locObj := TXPathScanner.Create('-1');
  try
    CheckEquals(Ord(xtkNumber), Ord(locObj.NextToken()));
    CheckEquals(Ord(xtkNumber), Ord(locObj.Token));
    CheckEquals('-1', locObj.TokenString);
    CheckEquals(-1, locObj.TokenInt);
    CheckEquals(-1.0, locObj.TokenNumber,0.01);
    CheckEquals(Ord(xtkEof), Ord(locObj.NextToken()));
  finally
    FreeAndNil(locObj);
  end;
end;

{ TXPathParser_Test }

procedure TXPathParser_Test.parse_1();
var
  locObj : TXPathParser;
  locRootNode, locNode : TXPathNode;
begin
  locRootNode := nil;
  locObj := TXPathParser.Create('object');
  try
    locRootNode := locObj.Parse();
    CheckNotNull(locRootNode, 'root');
    locNode := locRootNode;
      CheckIs(locNode, TXPathContextStepNode);
      CheckEquals(Ord(xcsProperty), Ord(TXPathContextStepNode(locNode).Switch));
      CheckEquals('object', TXPathContextStepNode(locNode).PropertyName);
        CheckNull(locNode.Next);
  finally
    FreeAndNil(locRootNode);
    FreeAndNil(locObj);
  end;
end;

procedure TXPathParser_Test.parse_10();
var
  locObj : TXPathParser;
  locRootNode, locNode, tmpNode : TXPathNode;
begin
  locRootNode := nil;
  //locObj := TXPathParser.Create('object/prop_a/prop_b[2]/prop_c[prop_d="azerty"]');
  locObj := TXPathParser.Create('prop_b[2]/prop_c[prop_d="azerty"]');
  try
    locRootNode := locObj.Parse();
    CheckNotNull(locRootNode, 'root');
    locNode := locRootNode;
      CheckNotNull(locNode);
      CheckIs(locNode, TXPathContextStepNode);
      CheckEquals(Ord(xcsProperty), Ord(TXPathContextStepNode(locNode).Switch));
      CheckEquals('prop_b', TXPathContextStepNode(locNode).PropertyName);
      locNode := locNode.Next;
        CheckNotNull(locNode);
        CheckIs(locNode, TXPathMoveInstructionStepNode);
        tmpNode := TXPathMoveInstructionStepNode(locNode).Distance;
          CheckIs(tmpNode,TXPathNumberConstantNode);
          CheckEquals(2, TXPathNumberConstantNode(tmpNode).Value, 0.01);
        locNode := locNode.Next;
          CheckNotNull(locNode);
          CheckIs(locNode, TXPathContextStepNode);
          CheckEquals(Ord(xcsProperty), Ord(TXPathContextStepNode(locNode).Switch));
          CheckEquals('prop_c', TXPathContextStepNode(locNode).PropertyName);
          locNode := locNode.Next;
            CheckNotNull(locNode);
            CheckIs(locNode, TXPathLocateInstructionStepNode);
            locNode := TXPathLocateInstructionStepNode(locNode).Condition;
            tmpNode := TXPathEqualNode(locNode).Left;
              CheckIs(tmpNode, TXPathValueSymbolNode);
              CheckEquals('prop_d', TXPathValueSymbolNode(tmpNode).Symbol);
            tmpNode := TXPathEqualNode(locNode).Right;
              CheckIs(tmpNode, TXPathStringConstantNode);
              CheckEquals('azerty', TXPathStringConstantNode(tmpNode).Value);

  finally
    FreeAndNil(locRootNode);
    FreeAndNil(locObj);
  end;
end;

procedure TXPathParser_Test.parse_2();
var
  locObj : TXPathParser;
  locRootNode, locNode : TXPathNode;
begin
  locRootNode := nil;
  locObj := TXPathParser.Create('/object');
  try
    locRootNode := locObj.Parse();
    CheckNotNull(locRootNode, 'root');
    locNode := locRootNode;
      CheckIs(locNode, TXPathContextStepNode);
      CheckEquals(Ord(xcsRoot), Ord(TXPathContextStepNode(locNode).Switch));
      CheckEquals('', TXPathContextStepNode(locNode).PropertyName);
      CheckNotNull(locNode.Next);
      locNode := locNode.Next;
        CheckEquals(Ord(xcsProperty), Ord(TXPathContextStepNode(locNode).Switch));
        CheckEquals('object', TXPathContextStepNode(locNode).PropertyName);
          CheckNull(locNode.Next);
  finally
    FreeAndNil(locRootNode);
    FreeAndNil(locObj);
  end;
end;

procedure TXPathParser_Test.parse_3();
var
  locObj : TXPathParser;
  locRootNode, locNode : TXPathNode;
begin
  locRootNode := nil;
  locObj := TXPathParser.Create('object/prop_a');
  try
    locRootNode := locObj.Parse();
    CheckNotNull(locRootNode, 'root');
    locNode := locRootNode;
      CheckIs(locNode, TXPathContextStepNode);
      CheckEquals(Ord(xcsProperty), Ord(TXPathContextStepNode(locNode).Switch));
      CheckEquals('object', TXPathContextStepNode(locNode).PropertyName);
      locNode := locNode.Next;
        CheckNotNull(locNode);
        CheckIs(locNode, TXPathContextStepNode);
        CheckEquals(Ord(xcsProperty), Ord(TXPathContextStepNode(locNode).Switch));
        CheckEquals('prop_a', TXPathContextStepNode(locNode).PropertyName);
        CheckNull(locNode.Next);
  finally
    FreeAndNil(locRootNode);
    FreeAndNil(locObj);
  end;
end;

procedure TXPathParser_Test.parse_4();
var
  locObj : TXPathParser;
  locRootNode, locNode : TXPathNode;
begin
  locRootNode := nil;
  locObj := TXPathParser.Create('/');
  try
    locRootNode := locObj.Parse();
    CheckNotNull(locRootNode, 'root');
    locNode := locRootNode;
      CheckIs(locNode, TXPathContextStepNode);
      CheckEquals(Ord(xcsRoot), Ord(TXPathContextStepNode(locNode).Switch));
      CheckEquals('', TXPathContextStepNode(locNode).PropertyName);
      CheckNull(locNode.Next);
  finally
    FreeAndNil(locRootNode);
    FreeAndNil(locObj);
  end;
end;

procedure TXPathParser_Test.parse_5();
var
  locObj : TXPathParser;
  locRootNode, locNode : TXPathNode;
begin
  locRootNode := nil;
  locObj := TXPathParser.Create('object/prop_a/');
  try
    locRootNode := locObj.Parse();
    CheckNotNull(locRootNode, 'root');
    locNode := locRootNode;
      CheckIs(locNode, TXPathContextStepNode);
      CheckEquals(Ord(xcsProperty), Ord(TXPathContextStepNode(locNode).Switch));
      CheckEquals('object', TXPathContextStepNode(locNode).PropertyName);
      locNode := locNode.Next;
        CheckNotNull(locNode);
        CheckIs(locNode, TXPathContextStepNode);
        CheckEquals(Ord(xcsProperty), Ord(TXPathContextStepNode(locNode).Switch));
        CheckEquals('prop_a', TXPathContextStepNode(locNode).PropertyName);
        CheckNull(locNode.Next);
  finally
    FreeAndNil(locRootNode);
    FreeAndNil(locObj);
  end;
end;

procedure TXPathParser_Test.parse_6();
var
  locObj : TXPathParser;
  locRootNode, locNode, tmpNode : TXPathNode;
begin
  locRootNode := nil;
  locObj := TXPathParser.Create('object/prop_a/prop_b[2]');
  try
    locRootNode := locObj.Parse();
    CheckNotNull(locRootNode, 'root');
    locNode := locRootNode;
      CheckIs(locNode, TXPathContextStepNode);
      CheckEquals(Ord(xcsProperty), Ord(TXPathContextStepNode(locNode).Switch));
      CheckEquals('object', TXPathContextStepNode(locNode).PropertyName);
      locNode := locNode.Next;
        CheckNotNull(locNode);
        CheckIs(locNode, TXPathContextStepNode);
        CheckEquals(Ord(xcsProperty), Ord(TXPathContextStepNode(locNode).Switch));
        CheckEquals('prop_a', TXPathContextStepNode(locNode).PropertyName);
        locNode := locNode.Next;
          CheckNotNull(locNode);
          CheckIs(locNode, TXPathContextStepNode);
          CheckEquals(Ord(xcsProperty), Ord(TXPathContextStepNode(locNode).Switch));
          CheckEquals('prop_b', TXPathContextStepNode(locNode).PropertyName);
          locNode := locNode.Next;
            CheckNotNull(locNode);
            CheckIs(locNode, TXPathMoveInstructionStepNode);
            tmpNode := TXPathMoveInstructionStepNode(locNode).Distance;
              CheckIs(tmpNode,TXPathNumberConstantNode);
              CheckEquals(2, TXPathNumberConstantNode(tmpNode).Value, 0.01);
            CheckNull(locNode.Next);
  finally
    FreeAndNil(locRootNode);
    FreeAndNil(locObj);
  end;
end;

procedure TXPathParser_Test.parse_7();
var
  locObj : TXPathParser;
  locRootNode, locNode : TXPathNode;
begin
  locRootNode := nil;
  locObj := TXPathParser.Create('@object');
  try
    locRootNode := locObj.Parse();
    CheckNotNull(locRootNode, 'root');
    locNode := locRootNode;
      CheckIs(locNode, TXPathContextStepNode);
      CheckEquals(Ord(xcsProperty), Ord(TXPathContextStepNode(locNode).Switch));
      CheckEquals('object', TXPathContextStepNode(locNode).PropertyName);
        CheckNull(locNode.Next);
  finally
    FreeAndNil(locRootNode);
    FreeAndNil(locObj);
  end;
end;

procedure TXPathParser_Test.parse_8();
var
  locObj : TXPathParser;
  locRootNode, locNode : TXPathNode;
begin
  locRootNode := nil;
  locObj := TXPathParser.Create('/@object');
  try
    locRootNode := locObj.Parse();
    CheckNotNull(locRootNode, 'root');
    locNode := locRootNode;
      CheckIs(locNode, TXPathContextStepNode);
      CheckEquals(Ord(xcsRoot), Ord(TXPathContextStepNode(locNode).Switch));
      CheckEquals('', TXPathContextStepNode(locNode).PropertyName);
      CheckNotNull(locNode.Next);
      locNode := locNode.Next;
        CheckEquals(Ord(xcsProperty), Ord(TXPathContextStepNode(locNode).Switch));
        CheckEquals('object', TXPathContextStepNode(locNode).PropertyName);
          CheckNull(locNode.Next);
  finally
    FreeAndNil(locRootNode);
    FreeAndNil(locObj);
  end;
end;

procedure TXPathParser_Test.parse_9();
var
  locObj : TXPathParser;
  locRootNode, locNode : TXPathNode;
begin
  locRootNode := nil;
  locObj := TXPathParser.Create('object/@prop_a');
  try
    locRootNode := locObj.Parse();
    CheckNotNull(locRootNode, 'root');
    locNode := locRootNode;
      CheckIs(locNode, TXPathContextStepNode);
      CheckEquals(Ord(xcsProperty), Ord(TXPathContextStepNode(locNode).Switch));
      CheckEquals('object', TXPathContextStepNode(locNode).PropertyName);
      locNode := locNode.Next;
        CheckNotNull(locNode);
        CheckIs(locNode, TXPathContextStepNode);
        CheckEquals(Ord(xcsProperty), Ord(TXPathContextStepNode(locNode).Switch));
        CheckEquals('prop_a', TXPathContextStepNode(locNode).PropertyName);
        CheckNull(locNode.Next);
  finally
    FreeAndNil(locRootNode);
    FreeAndNil(locObj);
  end;
end;

{ TXPathUtilsFunc_Test }

procedure TXPathUtilsFunc_Test.getXpath_func();
var
  locFactory : ISDODataFactory;

  procedure Add_Objects(const AUri : string);
  var
    locObj : ISDOType;
  begin
    locFactory.AddType(AUri,s_type_object_C,[]);
    locObj := locFactory.getType(AUri,s_type_object_C);

    locFactory.AddType(AUri,s_type_object_B,[]);
    locObj := locFactory.getType(AUri,s_type_object_B);
      locFactory.addProperty(locObj,'p_bc',s_uri,s_type_object_C,[pfIsContainment]);
      locFactory.addProperty(locObj,'p_bc_multi',s_uri,s_type_object_C,[pfIsContainment,pfIsMany]);

    locFactory.AddType(AUri,s_type_object_A,[]);
    locObj := locFactory.getType(AUri,s_type_object_A);
      locFactory.addProperty(locObj,'p_ab',s_uri,s_type_object_B,[pfIsContainment]);
      locFactory.addProperty(locObj,'p_ac',s_uri,s_type_object_C,[pfIsContainment]);
  end;

var
  a , b, c0, c1, c2 : ISDODataObject;
begin
  locFactory := TSDODataFactory.Create() as ISDODataFactory;
  Add_Objects(s_uri);
  a := locFactory.createNew(s_uri,s_type_object_A);
  b := a.createDataObject('p_ab');
  b.createDataObject('p_bc');
  a.createDataObject('p_ac');
  c0 := b.createDataObject('p_bc_multi');
  b.getList('p_bc_multi').append(c0);
  c1 := b.createDataObject('p_bc_multi');
  b.getList('p_bc_multi').append(c1);
  c2 := b.createDataObject('p_bc_multi');
  b.getList('p_bc_multi').append(c2);

  CheckEquals('', getXpath(nil));
  CheckEquals('',getXpath(a));
  CheckEquals('p_ab',getXpath(a.getDataObject('p_ab')));
  CheckEquals('p_ac',getXpath(a.getDataObject('p_ac')));
  CheckEquals('p_ab/p_bc',getXpath(a.getDataObject('p_ab').getDataObject('p_bc')));
  CheckEquals('p_ab/p_bc_multi[1]',getXpath(c1));
  CheckEquals('p_ab/p_bc_multi[0]',getXpath(c0));
  CheckEquals('p_ab/p_bc_multi[2]',getXpath(c2));
end;

initialization
  RegisterTest('Helpers',TXPathScanner_Test.Suite);
  RegisterTest('Helpers',TXPathParser_Test.Suite);
  RegisterTest('Helpers',TXPathProcessor_Test.Suite);
  RegisterTest('Helpers',TXPathUtilsFunc_Test.Suite);

end.
