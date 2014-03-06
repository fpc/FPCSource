{$INCLUDE sdo_global.inc}
unit test_xsdhelper;

interface
uses SysUtils
{$IFDEF FPC}
  ,fpcunit, testutils, testregistry
{$ENDIF}
{$IFNDEF FPC}
  ,TestFrameWork
{$ENDIF}
  , sdo, sdo_types, sdo_xsd_helper ;

type

  TXsdHelper_Test = class(TTestCase)
  private
    function CreateSdoTypes() : ISDODataFactory;
  protected
    function ExpandFileName(const AFileName : string) : string;
    function GetFileName() : string;
    procedure CompareObjectType(
      AExpected : ISDODataObject;
      AValue : ISDOType;
      APasTree : ISDODataObject
    ); overload;
    procedure CompareObjectType(
      AExpected : ISDOType;
      AValue : ISDODataObject;
      APasTree : ISDODataFactory
    ); overload;
    procedure CompareModule(
      AExpected : ISDODataObject;
      AValue : ISDODataFactory;
      APasTree : ISDODataObject
    ); overload;
    procedure CompareModule(
      AExpected : string;
      AValue : ISDODataObject;
      APasTree : ISDODataFactory
    ); overload;
    procedure CompareSdoModule(
      AExpectedFactory : ISDODataFactory;
      AExpectedURI : string;
      AValueFactory : ISDODataFactory
    );
    procedure CompareSdoTypes(AExpected, AValue : ISDOType);
  published
    procedure LoadFromStream();
    procedure LoadFromFile();
    procedure LoadFromString();

    procedure GenerateString();
    procedure GenerateStream();
    procedure GenerateFile();

    procedure LoadFromStream_open_type();
    procedure LoadFromFile_open_type();
    procedure GenerateString_open_type();
    procedure GenerateStream_open_type();

    procedure parse_scoped_array();

    procedure TypeDerivation();
    procedure TypeDerivation2();
  end;

implementation
uses
  Classes,
{$IFDEF DELPHI}
  xmldom, sdo_win_xml,
{$ENDIF DELPHI}
{$IFDEF FPC}
  DOM, sdo_fpc_xml, XMLRead,
{$ENDIF}
  sdo_xsdintf,
  test_suite_utils, sdo_datafactory, sdo_xsdparser, sdo_type, xsd_consts,
  sdo_parserutils;


const
  s_uri = 'company.xsd';
  s_EmployeeType = 'EmployeeType';
  s_DepartmentType = 'DepartmentType';
  s_CompanyType = 'CompanyType';
  s_company = 'company';

{ TXsdHelper_Test }

procedure TXsdHelper_Test.CompareModule(
  AExpected: ISDODataObject;
  AValue: ISDODataFactory;
  APasTree: ISDODataObject
);
var
  i : Integer;
  typeList : ISDODataObjectList;
  e : ISDODataObject;
  t : ISDOType;
begin
  typeList := AExpected.getList(s_type);
  for i := 0 to Pred(typeList.size()) do begin
    e := typeList.getDataObject(i);
    if e.getBoolean(s_IsComplex) then begin
      t :=  AValue.getTypes().find(AExpected.getString(sdo_xsdintf.s_NameSpace),e.getString(sdo_xsdintf.s_Name));
      CompareObjectType(e,t,APasTree);
    end;
  end;
end;

procedure TXsdHelper_Test.CompareObjectType(
  AExpected : ISDODataObject; //TPasClassType;
  AValue : ISDOType;
  APasTree : ISDODataObject
);
var
  pasLs : ISDODataObjectList;
  sls : ISDOPropertyList;
  pasp : ISDODataObject;
  sp  : ISDOProperty;
  i, propCount : PtrInt;
  pt, newpt : ISDODataObject;
  alsCount, j : PtrInt;
  ok : Boolean;
  tmpBuffer, alLs : string;
  parent : ISDODataObject;
begin
  if Assigned(AExpected) then begin
    Check(AValue <> nil, 'Should not be nil');
    CheckEquals(LowerCase(GetTypeNameSpace(AExpected)),LowerCase(AValue.getURI()),'URI');
    CheckEquals(LowerCase(AExpected.getString(s_Name)),LowerCase(AValue.getName()),'Name');
    parent := AExpected.getDataObject(s_BaseType);
    CompareObjectType(parent,AValue.getBaseType(),APasTree);
    if ( parent <> nil ) and
       ( not IsNativeType(parent) )
    then begin
      Check(AValue.getBaseType() <> nil, AExpected.getString(s_Name) + ', BaseType() Should not be nil');
      CheckEquals(LowerCase(AExpected.getString(s_Name)),LowerCase(AValue.getBaseType().getName()),AExpected.getString(s_Name) + ', Name');
      CheckEquals(LowerCase(AExpected.getString(s_NameSpace)),LowerCase(AValue.getBaseType().getURI()),AExpected.getString(s_Name) + ', NameSpace');
    end;

    pasLs := AExpected.getList(s_Property);
    sls := AValue.getProperties();
    propCount := 0;
    for i := 0 to Pred(pasLs.size()) do begin
      pasp := pasLs.getDataObject(i);
      Inc(propCount);
      pt := pasp.getDataObject(s_DataType);
      sp := sls.find(pasp.getString(s_Name));
      Check( ( sp <> nil ), Format('Property not found : %s',[pasp.getString(s_Name)]));
      CheckEquals(LowerCase(pasp.getString(s_Name)),LowerCase(sp.getName()),'Property');
      if pt.getBoolean(s_Unresolved) then begin
        newpt := Find(APasTree,pt.getString(s_NameSpace),pt.getString(s_Name));
        if (newpt <> nil) then
          pt := newpt;
      end;
      newpt := GetUltimeType(pt);
      if (newpt <> nil) then
        pt := newpt;
      CheckEquals(
        (pasp.getInteger(s_PropertyMaxOccurs) > 1),
        sp.isMany(),
        'isMany()'
      );
      alLs := sp.getType().getName();
      ok := ( LowerCase(pt.getString(s_Name)) = LowerCase(sp.getType().getName()) );
      alsCount := sp.getType().getAliasCount();
      if ( not ok ) and ( alsCount > 0 ) then begin
        tmpBuffer := LowerCase(pt.getString(s_Name));
        alLs := alLs + ', ' + tmpBuffer;
        for j := 0 to Pred(alsCount) do begin
          if ( tmpBuffer = LowerCase(sp.getType().getAlias(j)) ) then begin
            ok := True;
            Break;
          end;
        end;
      end;
      CheckEquals(
        True,
        ok,
        Format('Property "%s" Type name ( Alias = "%s"; PT = "%s" )',[pasp.getString(s_Name),alLs,pt.getString(s_Name)])
      );
    end;
    CheckEquals(propCount,sls.getCount(),AExpected.getString(s_Name) + ', Properties count');
  end else begin
    Check(AValue = nil, '"nil" Expected.');
  end;
end;

procedure TXsdHelper_Test.CompareModule(
  AExpected: string;
  AValue: ISDODataObject;
  APasTree: ISDODataFactory
);
var
  i : Integer;
  typeList : ISDOTypeList;
  e : ISDOType;
  t : ISDODataObject;
begin
  typeList := APasTree.getTypes();
  for i := 0 to Pred(typeList.getCount()) do begin
    e := typeList.getItem(i);
    if SameText(AExpected,e.getURI()) and e.isDataObjectType() then begin
      t := FindInModule(AValue,e.getName());
      CompareObjectType(e,t,APasTree);
    end;
  end;
end;

procedure TXsdHelper_Test.CompareObjectType(
  AExpected: ISDOType;
  AValue: ISDODataObject;
  APasTree: ISDODataFactory
);
var
  pasLs : ISDOPropertyList;
  sls : ISDODataObjectList;
  pasp : ISDOProperty;
  sp  : ISDODataObject;
  i, propCount : PtrInt;
  pt, newpt : ISDOType;
  alsCount, j : PtrInt;
  ok : Boolean;
  tmpBuffer, alLs : string;
  parent : ISDOType;
  searchingType : ISDODataObject;
  searchingTypeProps : ISDODataObjectList;
begin
  if Assigned(AExpected) then begin
    Check(AValue <> nil, 'Should not be nil');
    CheckEquals(LowerCase(AExpected.getURI()),LowerCase(GetTypeNameSpace(AValue)),'URI');
    CheckEquals(LowerCase(AExpected.getName()),LowerCase(AValue.getString(s_Name)),'Name');
    parent := AExpected.getBaseType();
    CompareObjectType(parent,AValue.getDataObject(s_BaseType),APasTree);

    pasLs := AExpected.getProperties();
    sls := AValue.getList(s_Property);
    propCount := 0;
    for i := 0 to Pred(pasLs.getCount()) do begin
      pasp := pasLs.getItem(i);
      Inc(propCount);
      pt := pasp.getType();
      sp := nil;
      searchingType := AValue;
      while (searchingType <> nil) do begin
        searchingTypeProps := searchingType.getList(s_Property);
        searchingTypeProps.getCursor().Reset();
        while searchingTypeProps.getCursor().MoveNext() do begin
          if SameText(pasp.getName(),searchingTypeProps.getDataObject().getString(s_Name)) then begin
            sp := searchingTypeProps.getDataObject();
            Break;
          end;
        end;
        if (sp <> nil) then
          Break;
        searchingType := searchingType.getDataObject(s_BaseType);
      end;
      Check( ( sp <> nil ), Format('Property not found : %s',[pasp.getName()]));
      CheckEquals(LowerCase(pasp.getName()),LowerCase(sp.getString(s_Name)),'Property');
      CheckEquals(
        pasp.isMany(),
        (sp.getInteger(s_PropertyMaxOccurs) > 1),
        'isMany()'
      );
      alLs := sp.getType().getName();
      ok := ( LowerCase(pt.getName()) = LowerCase(sp.getDataObject(s_DataType).getString(s_Name)) );
      alsCount := pt.getAliasCount();
      if ( not ok ) and ( alsCount > 0 ) then begin
        tmpBuffer := LowerCase(sp.getDataObject(s_DataType).getString(s_Name));
        alLs := alLs + ', ' + tmpBuffer;
        for j := 0 to Pred(alsCount) do begin
          if ( tmpBuffer = LowerCase(pt.getAlias(j)) ) then begin
            ok := True;
            Break;
          end;
        end;
      end;
      CheckEquals(
        True,
        ok,
        Format('Property "%s" Type name ( Alias = "%s"; PT = "%s" )',[pasp.getName() ,alLs,pt.getName()])
      );
    end;
    i := 0;
    searchingType := AValue;
    while (searchingType <> nil) do begin
      i := i + searchingType.getList(s_Property).size();
      searchingType := searchingType.getDataObject(s_BaseType);
    end;
    CheckEquals(propCount,i,AExpected.getName() + ', Properties count');
  end else begin
    Check(AValue = nil, '"nil" Expected.');
  end;
end;

procedure TXsdHelper_Test.CompareSdoModule(
  AExpectedFactory: ISDODataFactory;
  AExpectedURI: string;
  AValueFactory: ISDODataFactory
);
var
  etl, vtl : ISDOTypeList;
  e, v : ISDOType;
  i : Integer;
begin
  etl := AExpectedFactory.getTypes();
  vtl := AValueFactory.getTypes();
  for i := 0 to etl.getCount() - 1 do begin
    e := etl.getItem(i);
    if SameText(AExpectedURI,e.getURI()) then begin
      v := vtl.find(AExpectedURI,e.getName());
      Check((v <> nil),Format('type not found : "%s"',[e.getName()]));
      CompareSdoTypes(e,v);
    end;
  end;
end;

procedure TXsdHelper_Test.CompareSdoTypes(AExpected, AValue: ISDOType);
begin
  if (AExpected= nil) then begin
    Check(AValue = nil, 'nil expected.');
    exit;
  end;
  Check(AValue <> nil);
  CheckEquals(AExpected.getURI(),AValue.getURI(),'URI');
  CheckEquals(AExpected.getName(),AValue.getName(),'Name');
  Check(AExpected.getFlags() = AValue.getFlags(),'getFlags()');
  if (AExpected.getBaseType() = nil) then begin
    Check(AValue.getBaseType() = nil,'BaseType')
  end else begin
    CompareSdoTypes(AExpected.getBaseType(),AValue.getBaseType());
  end;
end;

function TXsdHelper_Test.CreateSdoTypes() : ISDODataFactory;
var
  locObj : ISDOType;
begin
  Result := TSDODataFactory.Create() as ISDODataFactory;
  Result.AddType(s_uri,s_EmployeeType,[]);
  locObj := Result.getType(s_uri,s_EmployeeType);
    Result.addProperty(locObj,'name',sdo_namespace,'string',[]);
    Result.addProperty(locObj,'SN',sdo_namespace,'string',[]);
    Result.addProperty(locObj,'manager',sdo_namespace,'boolean',[]);

  Result.AddType(s_uri,s_DepartmentType,[]);
  locObj := Result.getType(s_uri,s_DepartmentType);
    Result.addProperty(locObj,'employees',s_uri,s_EmployeeType,[pfIsMany, pfIsContainment]);
    Result.addProperty(locObj,'name',sdo_namespace,'string',[]);
    Result.addProperty(locObj,'location',sdo_namespace,'string',[]);
    Result.addProperty(locObj,'number',sdo_namespace,'int',[]);

  Result.AddType(s_uri,s_CompanyType,[]);
  locObj := Result.getType(s_uri,s_CompanyType);
    Result.addProperty(locObj,'departments',s_uri,s_DepartmentType,[pfIsMany, pfIsContainment]);
    Result.addProperty(locObj,'name',sdo_namespace,'string',[]);
    Result.addProperty(locObj,'employeeOfTheMonth',sdo_namespace,'string',[]);
    (locObj as ISDOTypeEx).setAlias(s_company);
end;

function TXsdHelper_Test.ExpandFileName(const AFileName: string): string;
begin
  Result := sdoExpandLocalFileName(TestFilesPath + AFileName);
end;

procedure TXsdHelper_Test.GenerateFile();
var
  helper : IXSDHelper;
  fct : ISDODataFactory;
  xsdPrsr : IXsdParser;
  locTree : ISDODataObject;
  locDoc : TXMLDocument;
  locFileName : string;
  locPasFactory : ISDODataFactory;
begin
  locFileName := sdoExpandLocalFileName('generatefile.xsd');
  fct := CreateSdoTypes() as ISDODataFactory;
  helper := TXSDHelper.Create(fct) as IXSDHelper;
  helper.Generate(fct.getTypes(),s_uri,locFileName);
  locTree := nil;
  ReadXMLFile(locDoc,locFileName);
  try
    locPasFactory := TSDODataFactory.Create();
    AddTypeTree(locPasFactory);
    locTree := locPasFactory.CreateNew(s_XsdParserNS,s_TypeTreeType);
    xsdPrsr := TXsdParser.Create(locDoc,locTree,'') as IXsdParser;
    xsdPrsr.ParseTypes();
    CompareModule(locTree.getDataObject(s_CurrentModule),fct,locTree);
  finally
    locTree := nil;
    ReleaseDomNode(locDoc);
  end;
end;

procedure TXsdHelper_Test.GenerateStream();
var
  helper : IXSDHelper;
  fct : ISDODataFactory;
  xsdPrsr : IXsdParser;
  locTree : ISDODataObject;
  locDoc : TXMLDocument;
  locStream : TStringStream;
  locPasFactory : ISDODataFactory;
begin
  fct := CreateSdoTypes() as ISDODataFactory;
  helper := TXSDHelper.Create(fct) as IXSDHelper;
  locStream := TStringStream.Create('');
  try
    helper.Generate(fct.getTypes(),locStream,s_uri);
    locStream.Position := 0;
    locTree := nil;
    ReadXMLFile(locDoc,locStream);
    try
      locPasFactory := TSDODataFactory.Create();
      AddTypeTree(locPasFactory);
      locTree := locPasFactory.CreateNew(s_XsdParserNS,s_TypeTreeType);
      xsdPrsr := TXsdParser.Create(locDoc,locTree,'') as IXsdParser;
      xsdPrsr.ParseTypes();
      CompareModule(locTree.getDataObject(s_CurrentModule),fct,locTree);
    finally
      locTree := nil;
      ReleaseDomNode(locDoc);
    end;
  finally
    locStream.Free();
  end;
end;

procedure TXsdHelper_Test.GenerateString();
var
  helper : IXSDHelper;
  fct : ISDODataFactory;
  xsdPrsr : IXsdParser;
  locTree : ISDODataObject;
  locDoc : TXMLDocument;
  locStream : TStringStream;
  locBuffer : string;
  locPasFactory : ISDODataFactory;
begin
  fct := CreateSdoTypes();
  helper := TXSDHelper.Create(fct) as IXSDHelper;
  locStream := nil;
  try
    locBuffer := helper.Generate(fct.getTypes(),s_uri);
    locStream := TStringStream.Create(locBuffer);
    locStream.Position := 0;
    locTree := nil;
    ReadXMLFile(locDoc,locStream);
    try
      locPasFactory := TSDODataFactory.Create();
      AddTypeTree(locPasFactory);
      locTree := locPasFactory.CreateNew(s_XsdParserNS,s_TypeTreeType);
      xsdPrsr := TXsdParser.Create(locDoc,locTree,'') as IXsdParser;
      xsdPrsr.ParseTypes();
      CompareModule(locTree.getDataObject(s_CurrentModule),fct,locTree);
    finally
      locTree := nil;
      ReleaseDomNode(locDoc);
    end;
  finally
    locStream.Free();
  end;
end;

function TXsdHelper_Test.GetFileName() : string;
begin
  Result := ExpandFileName('sdo_company.xsd');
end;

procedure TXsdHelper_Test.LoadFromFile();
var
  helper : IXSDHelper;
  fct : ISDODataFactory;
  xsdPrsr : IXsdParser;
  locTree : ISDODataObject;
  locDoc : TXMLDocument;
  locPasFactory : ISDODataFactory;
begin
  fct := TSDODataFactory.Create() as ISDODataFactory;
  helper := TXSDHelper.Create(fct) as IXSDHelper;
  helper.LoadFromFile(GetFileName());

  locDoc := nil;
  locPasFactory := TSDODataFactory.Create();
  AddTypeTree(locPasFactory);
  locTree := locPasFactory.CreateNew(s_XsdParserNS,s_TypeTreeType);
  try
    ReadXMLFile(locDoc,GetFileName());
    xsdPrsr := TXsdParser.Create(locDoc,locTree,'') as IXsdParser;
    xsdPrsr.ParseTypes();
    CompareModule(locTree.getDataObject(s_CurrentModule),fct,locTree);
  finally
    locTree := nil;
    ReleaseDomNode(locDoc);
  end;
end;

procedure TXsdHelper_Test.LoadFromStream();
var
  helper : IXSDHelper;
  fct : ISDODataFactory;
  xsdPrsr : IXsdParser;
  locTree : ISDODataObject;
  locDoc : TXMLDocument;
  locStream : TMemoryStream;
  locPasFactory : ISDODataFactory;
begin
  fct := TSDODataFactory.Create() as ISDODataFactory;
  helper := TXSDHelper.Create(fct) as IXSDHelper;
  locStream := TMemoryStream.Create();
  try
    locStream.LoadFromFile(GetFileName());
    locStream.Position := 0;
    helper.LoadFromStream(locStream);

    locDoc := nil;
    locPasFactory := TSDODataFactory.Create();
    AddTypeTree(locPasFactory);
    locTree := locPasFactory.CreateNew(s_XsdParserNS,s_TypeTreeType);
    try
      ReadXMLFile(locDoc,GetFileName());
      xsdPrsr := TXsdParser.Create(locDoc,locTree,'') as IXsdParser;
      xsdPrsr.ParseTypes();
      CompareModule(locTree.getDataObject(s_CurrentModule),fct,locTree);
    finally
      locTree := nil;
      locPasFactory := nil;
      ReleaseDomNode(locDoc);
    end;
  finally
    locStream.Free();
  end;
end;

procedure TXsdHelper_Test.LoadFromString();
var
  helper : IXSDHelper;
  fct : ISDODataFactory;
  xsdPrsr : IXsdParser;
  locTree : ISDODataObject;
  locDoc : TXMLDocument;
  locStream : TMemoryStream;
  locBuffer : TStringBufferType;
  locPasFactory : ISDODataFactory;
begin
  fct := TSDODataFactory.Create() as ISDODataFactory;
  helper := TXSDHelper.Create(fct) as IXSDHelper;
  locStream := TMemoryStream.Create();
  try
    locStream.LoadFromFile(GetFileName());
    locStream.Position := 0;
    SetLength(locBuffer,locStream.Size);
    locStream.Read(locBuffer[1],locStream.Size);
    helper.LoadFromString(String(locBuffer));

    locDoc := nil;
    locPasFactory := TSDODataFactory.Create();
    AddTypeTree(locPasFactory);
    locTree := locPasFactory.CreateNew(s_XsdParserNS,s_TypeTreeType);
    try
      ReadXMLFile(locDoc,GetFileName());
      xsdPrsr := TXsdParser.Create(locDoc,locTree,'') as IXsdParser;
      xsdPrsr.ParseTypes();
      CompareModule(locTree.getDataObject(s_CurrentModule),fct,locTree);
    finally
      locTree := nil;
      ReleaseDomNode(locDoc);
    end;
  finally
    locStream.Free();
  end;
end;

procedure TXsdHelper_Test.LoadFromStream_open_type();
const S_TEST_NS = 'urn:wst-test';
var
  helper : IXSDHelper;
  fct : ISDODataFactory;
  locStream : TMemoryStream;
  typ : ISDOType;
begin
  fct := TSDODataFactory.Create() as ISDODataFactory;
  helper := TXSDHelper.Create(fct) as IXSDHelper;
  locStream := TMemoryStream.Create();
  try
    locStream.LoadFromFile(ExpandFileName('complex_class_open_type.xsd'));
    locStream.Position := 0;
    helper.LoadFromStream(locStream);
    typ := fct.getType(S_TEST_NS,'TClass_1');
      CheckEquals(True,typ.isOpenType(),'TClass_1.IsOpenType()');
    typ := fct.getType(S_TEST_NS,'TClassSampleTypeAll');
      CheckEquals(True,typ.isOpenType(),'TClassSampleTypeAll.IsOpenType()');
    typ := fct.getType(S_TEST_NS,'TClass_2');
      CheckEquals(False,typ.isOpenType(),'TClass_2.IsOpenType()');
    typ := fct.getType(S_TEST_NS,'TClassSampleDerivedType');
      CheckEquals(True,typ.isOpenType(),'TClassSampleDerivedType.IsOpenType()');
  finally
    locStream.Free();
  end;
end;

procedure TXsdHelper_Test.LoadFromFile_open_type();
const S_TEST_NS = 'urn:wst-test';
var
  helper : IXSDHelper;
  fct : ISDODataFactory;
  typ : ISDOType;
begin
  fct := TSDODataFactory.Create() as ISDODataFactory;
  helper := TXSDHelper.Create(fct) as IXSDHelper;
  helper.LoadFromFile(ExpandFileName('complex_class_open_type.xsd'));
  typ := fct.getType(S_TEST_NS,'TClass_1');
    CheckEquals(True,typ.isOpenType(),'TClass_1.IsOpenType()');
  typ := fct.getType(S_TEST_NS,'TClassSampleTypeAll');
    CheckEquals(True,typ.isOpenType(),'TClassSampleTypeAll.IsOpenType()');
  typ := fct.getType(S_TEST_NS,'TClass_2');
    CheckEquals(False,typ.isOpenType(),'TClass_2.IsOpenType()');
  typ := fct.getType(S_TEST_NS,'TClassSampleDerivedType');
    CheckEquals(True,typ.isOpenType(),'TClassSampleDerivedType.IsOpenType()');
end;

procedure TXsdHelper_Test.GenerateString_open_type();
const S_TEST_NS = 'urn:open-type';
var
  helper : IXSDHelper;
  fct, genFact : ISDODataFactory;
  typ : ISDOType;
  strBuffer : string;
begin
  genFact := TSDODataFactory.Create() as ISDODataFactory;
  genFact.AddType(S_TEST_NS,'type_open',[tfIsOpen]);
  genFact.AddType(S_TEST_NS,'type_notopen',[]);
  helper := TXSDHelper.Create(genFact) as IXSDHelper;
  strBuffer := helper.Generate(genFact.getTypes(),S_TEST_NS);
  helper := nil;

  fct := TSDODataFactory.Create() as ISDODataFactory;
  helper := TXSDHelper.Create(fct) as IXSDHelper;
  helper.LoadFromString(strBuffer);
  typ := fct.getType(S_TEST_NS,'type_open');
    CheckEquals(True,typ.isOpenType(),'type_open.IsOpenType()');
  typ := fct.getType(S_TEST_NS,'type_notopen');
    CheckEquals(False,typ.isOpenType(),'type_notopen.IsOpenType()');
end;

procedure TXsdHelper_Test.GenerateStream_open_type;
const S_TEST_NS = 'urn:open-type';
var
  helper : IXSDHelper;
  fct, genFact : ISDODataFactory;
  typ : ISDOType;
  locStream : TStream;
begin
  genFact := TSDODataFactory.Create() as ISDODataFactory;
  genFact.AddType(S_TEST_NS,'type_open',[tfIsOpen]);
  genFact.AddType(S_TEST_NS,'type_notopen',[]);
  helper := TXSDHelper.Create(genFact) as IXSDHelper;
  locStream := TMemoryStream.Create();
  try
    helper.Generate(genFact.getTypes(),locStream,S_TEST_NS);
    helper := nil;

    fct := TSDODataFactory.Create() as ISDODataFactory;
    helper := TXSDHelper.Create(fct) as IXSDHelper;
    locStream.Position := 0;
    helper.LoadFromStream(locStream);
    typ := fct.getType(S_TEST_NS,'type_open');
      CheckEquals(True,typ.isOpenType(),'type_open.IsOpenType()');
    typ := fct.getType(S_TEST_NS,'type_notopen');
      CheckEquals(False,typ.isOpenType(),'type_notopen.IsOpenType()');
  finally
    locStream.Free();
  end;
end;

procedure TXsdHelper_Test.parse_scoped_array();
const
  s_xml_text =
         '<schema targetNamespace="company.xsd" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:tns="company.xsd">'  + sLineBreak +
         '  <xsd:complexType name="Datarow">'  + sLineBreak +
         '    <xsd:sequence>'  + sLineBreak +
         '      <xsd:any processContents="lax" maxOccurs="unbounded"/>'  + sLineBreak +
         '    </xsd:sequence>'  + sLineBreak +
         '    <xsd:anyAttribute processContents="lax"/>'  + sLineBreak +
         '  </xsd:complexType>'  + sLineBreak +
         '  <xsd:complexType name="Dataset">'  + sLineBreak +
         '    <xsd:sequence>'  + sLineBreak +
         '      <xsd:element name="rows" type="tns:Datarow" minOccurs="0" maxOccurs="unbounded"/>'  + sLineBreak +
         '      <xsd:any processContents="lax" maxOccurs="unbounded"/>'  + sLineBreak +
         '    </xsd:sequence>'  + sLineBreak +
         '    <xsd:anyAttribute processContents="lax"/>'  + sLineBreak +
         '  </xsd:complexType>'  + sLineBreak +
         '</schema>';
var
  fact : ISDODataFactory;
  helper : IXSDHelper;
  tl : ISDOTypeList;
  typ : ISDOType;
  p : ISDOProperty;
begin
  fact := TSDODataFactory.Create() as ISDODataFactory;
  helper := TXSDHelper.Create(fact);
  helper.LoadFromString(s_xml_text);
  tl := fact.getTypes();
  typ := tl.find(s_uri,'Datarow');
  Check(( typ <> nil ), '"Datarow" not found');
  CheckEquals(True, typ.isOpenType(), 'Datarow.isOpenType()');
  typ := tl.find(s_uri,'Dataset');
  Check(( typ <> nil ), '"Dataset" not found');
  CheckEquals(True, typ.isOpenType(), 'Dataset.isOpenType()');
  p := typ.getProperties().find('rows');
  Check(( p <> nil ), 'Property not found : "rows"');
  CheckEquals(True,p.isMany());
  Check(p.getType().equals(fact.getType(s_uri,'Datarow')));
end;

procedure TXsdHelper_Test.TypeDerivation();
  function CreateTypes(): ISDODataFactory;
  var
    f : ISDODataFactory;
  begin
    f := TSDODataFactory.Create();
    f.AddType(s_uri,'typeA',[]);
      f.AddType(s_uri,'typeAA',[]); f.setBaseType(s_uri,'typeAA',s_uri,'typeA');
      f.AddType(s_uri,'typeAB',[]); f.setBaseType(s_uri,'typeAB',s_uri,'typeA');
        f.AddType(s_uri,'typeABA',[]); f.setBaseType(s_uri,'typeABA',s_uri,'typeAB');

    f.addProperty(s_uri,'typeAA','xi',sdo_namespace,'Int',[pfIsAttribute]);
    f.addProperty(s_uri,'typeAA','xs',sdo_namespace,'String',[]);
    f.addProperty(s_uri,'typeAB','bi',sdo_namespace,'Int',[]);
    f.addProperty(s_uri,'typeAB','bs',sdo_namespace,'String',[pfIsAttribute]);
    f.addProperty(s_uri,'typeABA','bb',sdo_namespace,'Boolean',[pfIsAttribute]);
    f.addProperty(s_uri,'typeABA','bx',sdo_namespace,'Bytes',[]);
    Result := f;
  end;

var
  helper : IXSDHelper;
  fct : ISDODataFactory;
  xsdPrsr : IXsdParser;
  locTree : ISDODataObject;
  locDoc : TXMLDocument;
  locFileName : string;
  locPasFactory : ISDODataFactory;
begin
  locFileName := sdoExpandLocalFileName('typederivation.xsd');
  fct := CreateTypes();
  helper := TXSDHelper.Create(fct) as IXSDHelper;
  helper.Generate(fct.getTypes(),s_uri,locFileName);
  locTree := nil;
  ReadXMLFile(locDoc,locFileName);
  try
    locPasFactory := TSDODataFactory.Create();
    AddTypeTree(locPasFactory);
    locTree := locPasFactory.CreateNew(s_XsdParserNS,s_TypeTreeType);
    xsdPrsr := TXsdParser.Create(locDoc,locTree,'') as IXsdParser;
    xsdPrsr.ParseTypes();
    CompareModule(s_uri,FindModule(locTree,s_uri),fct);
  finally
    locTree := nil;
    ReleaseDomNode(locDoc);
  end;
end;

procedure TXsdHelper_Test.TypeDerivation2();
  function CreateTypes(): ISDODataFactory;
  var
    f : ISDODataFactory;
  begin
    f := TSDODataFactory.Create();
    f.AddType(s_uri,'typeA',[]);
      f.AddType(s_uri,'typeAA',[]); f.setBaseType(s_uri,'typeAA',s_uri,'typeA');
      f.AddType(s_uri,'typeAB',[]); f.setBaseType(s_uri,'typeAB',s_uri,'typeA');
        f.AddType(s_uri,'typeABA',[]); f.setBaseType(s_uri,'typeABA',s_uri,'typeAB');

    f.addProperty(s_uri,'typeAA','xi',sdo_namespace,'Int',[pfIsAttribute]);
    f.addProperty(s_uri,'typeAA','xs',sdo_namespace,'String',[]);
    f.addProperty(s_uri,'typeAB','bi',sdo_namespace,'Int',[]);
    f.addProperty(s_uri,'typeAB','bs',sdo_namespace,'String',[pfIsAttribute]);
    f.addProperty(s_uri,'typeABA','bb',sdo_namespace,'Boolean',[pfIsAttribute]);
    f.addProperty(s_uri,'typeABA','bx',sdo_namespace,'Bytes',[]);
    Result := f;
  end;

var
  helper : IXSDHelper;
  fct, fctLoaded : ISDODataFactory;
  xsdPrsr : IXsdParser;
  locTree : ISDODataObject;
  locFileName : string;
begin
  locFileName := sdoExpandLocalFileName('typederivation2.xsd');
  fct := CreateTypes();
  helper := TXSDHelper.Create(fct) as IXSDHelper;
  helper.Generate(fct.getTypes(),s_uri,locFileName);

  fctLoaded := TSDODataFactory.Create();
  helper := TXSDHelper.Create(fctLoaded) as IXSDHelper;
  helper.LoadFromFile(locFileName);
  CompareSdoModule(fct,s_uri,fctLoaded);
end;

initialization
  RegisterTest('Helpers',TXsdHelper_Test.Suite);

end.
