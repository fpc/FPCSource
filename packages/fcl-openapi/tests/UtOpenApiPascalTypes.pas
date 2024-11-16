unit UtOpenApiPascalTypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson.schema.types, fpjson.schema.pascaltypes,
  fpopenapi.types, fpopenapi.objects, fpopenapi.pascaltypes;

Type

  { TTestPascalTypes }

  TTestPascalTypes = Class(TTestCase)
  private
    FAPI: TOpenAPI;
    FAPIData: TAPIData;
    function AssertService(const Msg: String; aService: TAPIService; const aName: string; aMethods: array of string): TAPIService;
    function AssertServiceMethod(const Msg: string; aMethod: TAPIServiceMethod; aParams: array of string; const aResultName,
      aBodyName: string): TAPIServiceMethod;
    function AssertServiceMethodResult(const Msg: string; aMethod: TAPIServiceMethod; aResultTypeData: TAPITypeData;
      const aResultClassName, aResultInterfaceName: string): TAPIServiceMethod;
    procedure AssertSimpleComponent(const Msg: String; aType: TAPITypeData);
  Public
    class procedure AssertEquals(const Msg: string; aExpected, aActual: TPascalType); overload;
    function AssertPascalTypeData(const Msg: string; aType: TPascalTypeData; aPascaltype: TPascalType; const aSchemaName, aPascalName: String) : TPascalTypeData;
    procedure AssertProperty(Msg: String; aType: TPascalTypeData; aIndex: Integer; const aSchemaName, aPascalName, aPascalTypeName: String; aPascalType: TPascalType; aTypeData: TPascalTypeData);
    procedure Load(const aFileName: string);
    procedure Setup; override;
    procedure TearDown; override;
    Property API : TOpenAPI Read FAPI;
    Property Data : TAPIData Read FAPIData;
  Published
    procedure TestHookup;
    procedure TestFindService;
    procedure TestSimpleComponent;
    procedure TestSimpleService;
    procedure TestSimpleServiceArrayArgument;
    procedure TestDoubleServiceArrayArgument;
    procedure TestServiceNoApplicationJSONResponse;
    procedure TestServiceNoApplicationJSONRequestBody;
  end;

implementation

uses typinfo, fpopenapi.reader;

{ TTestPascalTypes }

class procedure TTestPascalTypes.AssertEquals(const Msg : string; aExpected, aActual : TPascalType);

begin
  AssertEquals(Msg,GetEnumName(Typeinfo(TPascalType),Ord(aExpected)),
                   GetEnumName(Typeinfo(TPascalType),Ord(aActual)));
end;

function TTestPascalTypes.AssertPascalTypeData(const Msg: string; aType: TPascalTypeData; aPascaltype: TPascalType;
  const aSchemaName, aPascalName: String): TPascalTypeData;
begin
  AssertNotNull(Msg+': have type',aType);
  AssertEquals(Msg+' Schema name',aSchemaName,aType.SchemaName);
  AssertEquals(Msg+' Pascal name',aPascalName,aType.PascalName);
  AssertEquals(Msg+' Pascal type',aPascalType,aType.PascalType);
  Result:=aType;
end;


procedure TTestPascalTypes.AssertProperty(Msg : String; aType : TPascalTypeData; aIndex : Integer; const aSchemaName,aPascalName,aPascalTypeName : String; aPascalType : TPascalType; aTypeData: TPascalTypeData);

var
  lProp : TPascalPropertyData;

begin
  AssertNotNull(Msg+': have type',aType);
  AssertTrue(Msg+': have properties',aType.PropertyCount>0);
  AssertTrue(Msg+': have valid index',aIndex<aType.PropertyCount);
  lProp:=aType.Properties[aIndex];
  AssertNotNull(Msg+': have property',lProp);
  AssertEquals(Msg+': schema name',aSchemaName,lProp.SchemaName);
  AssertEquals(Msg+': pascal name',aPascalName,lProp.PascalName);
  AssertEquals(Msg+': Pascal type name',aPascalTypeName,lProp.PascalTypeName);
  AssertEquals(Msg+': Pascal type',aPascalType,lProp.PropertyType);
  AssertSame(Msg+': Type data',aTypeData,lProp.TypeData);
end;


procedure TTestPascalTypes.Setup;
begin
  inherited Setup;
  FAPI:=TOpenAPI.Create;
  FAPIData:=TAPIData.Create(FAPI);
end;

procedure TTestPascalTypes.TearDown;
begin
  FreeAndNil(FAPIData);
  FreeAndNil(FAPI);
  inherited TearDown;
end;

procedure TTestPascalTypes.TestHookup;
begin
  AssertNotNull('Have api',API);
  AssertNotNull('Have data',Data);
  AssertFalse('API empty',API.HasKeyWord(oakComponents) or API.HasKeyWord(oakPaths));
end;

procedure TTestPascalTypes.TestFindService;

var
  lService : TAPIService;

begin
  lService:=Data.AddService('a');
  AssertNotNull('Have service',lService);
  AssertSame('Find correct service',lService,Data.FindService('a'));
  AssertNull('unexisting service',Data.FindService('ab'));
end;

procedure TTestPascalTypes.AssertSimpleComponent(const Msg : String; aType : TAPITypeData);

begin
  AssertNotNull(Msg+': Have type',aType);
  AssertEquals(Msg+': Have type name','a',aType.SchemaName);
  AssertEquals(Msg+': Have pascal name','Ta',aType.PascalName);
  AssertEquals(Msg+': Have 2 properties',2,aType.PropertyCount);
  AssertProperty(Msg+': prop b', aType,0,'b','b','string',ptString,Data.ApiNamedTypes['string']);
  AssertProperty(Msg+': prop c', aType,1,'c','c','integer',ptInteger,Data.ApiNamedTypes['integer']);
end;

procedure TTestPascalTypes.TestSimpleComponent;

begin
  load('simplecomponent');
  AssertTrue('Have components',API.HasKeyWord(oakComponents));
  Data.CreateDefaultTypeMaps;
  Data.CreateDefaultAPITypeMaps;
  AssertEquals('Have 1 API type',1,Data.TypeCount);
  AssertSimpleComponent('First component',Data.APITypes[0]);
end;

function TTestPascalTypes.AssertService(const Msg: String; aService: TAPIService; const aName: string; aMethods: array of string
  ): TAPIService;

var
  I : integer;

begin
  AssertNotNull(Msg+': Have service',aService);
  AssertEquals(Msg+': service name',aName,aService.ServiceName);
  AssertEquals(Msg+': count',Length(aMethods),aService.MethodCount);
  for I:=0 to aService.MethodCount-1 do
    AssertEquals(Msg+': method '+IntToStr(I),aMethods[i],aService.Methods[i].MethodName);
  Result:=aService;
end;

function TTestPascalTypes.AssertServiceMethod(const Msg: string; aMethod: TAPIServiceMethod; aParams: array of string;
  const aResultName, aBodyName: string): TAPIServiceMethod;

var
  I : Integer;

begin
  AssertNotNull(Msg+': have method',aMethod);
  AssertEquals(Msg+': param count',Length(aParams),aMethod.ParamCount);
  for I:=0 to aMethod.ParamCount-1 do
    AssertEquals(Msg+': param '+IntToStr(I),aParams[I],aMethod.Param[I].Name);
  AssertEquals(Msg+': ResultDtoType',aResultName,aMethod.ResultDtoType);
  AssertEquals(Msg+': BodyName',aBodyName,aMethod.RequestBodyType);
  Result:=aMethod;
end;

function TTestPascalTypes.AssertServiceMethodResult(const Msg: string; aMethod: TAPIServiceMethod; aResultTypeData: TAPITypeData;
  const aResultClassName, aResultInterfaceName: string): TAPIServiceMethod;

begin
  AssertNotNull(Msg+': have method',aMethod);
  AssertEquals(Msg+': ResultClassType',aResultClassName,aMethod.ResultClassType);
  AssertEquals(Msg+': ResultInterfaceType',aResultInterfaceName,aMethod.ResultType);
  AssertSame(Msg+': BodyName',aResultTypeData,aMethod.ResultDataType);
  Result:=aMethod;
end;

procedure TTestPascalTypes.TestSimpleService;

var
  lService : TAPIService;
  lMethod : TAPIServiceMethod;
begin
  load('simpleservice');
  AssertTrue('Have components',API.HasKeyWord(oakComponents));
  Data.CreateDefaultTypeMaps;
  Data.CreateDefaultAPITypeMaps;
  Data.CreateServiceDefs;
  AssertEquals('Have 1 API type',1,Data.TypeCount);
  AssertSimpleComponent('First component',Data.APITypes[0]);
  AssertEquals('Have 1 service type',1,Data.ServiceCount);
  lService:=AssertService('First component',Data.Services[0],'SimpleService',['List']);
  lMethod:=AssertServiceMethod('Method List',lService.Methods[0],[],'Ta','');
  AssertServiceMethodResult('Method List',lMethod,Data.APITypes[0],'TaObj','Ia');
end;

procedure TTestPascalTypes.TestSimpleServiceArrayArgument;
var
  lService : TAPIService;
  lMethod : TAPIServiceMethod;
begin
  load('simpleservicearray');
  AssertTrue('Have components',API.HasKeyWord(oakComponents));
  Data.CreateDefaultTypeMaps;
  Data.CreateDefaultAPITypeMaps;
  Data.CreateServiceDefs;
  AssertEquals('Have 2 API type',2,Data.TypeCount);
  AssertSimpleComponent('First component',Data.APITypes[0]);
  AssertPascalTypeData('Element type',Data.APITypes[1],ptArray,'[a]','TaArray');
  AssertEquals('Have 1 service def',1,Data.ServiceCount);
  lService:=AssertService('First service',Data.Services[0],'SimpleService',['List']);
  lMethod:=AssertServiceMethod('Service 0 Method List',lService.Methods[0],[],'TaArray','');
  AssertServiceMethodResult('Service 0 Method List',lMethod,Data.APITypes[1],'TaArray','IaArray');
end;

procedure TTestPascalTypes.TestDoubleServiceArrayArgument;
var
  lService : TAPIService;
  lMethod : TAPIServiceMethod;
begin
  load('doubleservicearray');
  AssertTrue('Have components',API.HasKeyWord(oakComponents));
  Data.CreateDefaultTypeMaps;
  Data.CreateDefaultAPITypeMaps;
  Data.CreateServiceDefs;
  AssertEquals('Have 2 API type',2,Data.TypeCount);
  AssertSimpleComponent('First component',Data.APITypes[0]);
  AssertPascalTypeData('Element type',Data.APITypes[1],ptArray,'[a]','TaArray');
  AssertEquals('Have 2 services',2,Data.ServiceCount);
  lService:=AssertService('First Service',Data.Services[1],'SimpleService',['List']);
  lMethod:=AssertServiceMethod('Service 0 Method List',lService.Methods[0],[],'TaArray','');
  AssertServiceMethodResult('Service 0 Method List',lMethod,Data.APITypes[1],'TaArray','IaArray');
  lService:=AssertService('second service',Data.Services[0],'Simple2Service',['List2']);
  lMethod:=AssertServiceMethod('Service 1 Method List',lService.Methods[0],[],'TaArray','');
  AssertServiceMethodResult('service 1 List',lMethod,Data.APITypes[1],'TaArray','IaArray');
end;

procedure TTestPascalTypes.TestServiceNoApplicationJSONResponse;

begin
  load('noapplicationjsonresponse');
  AssertTrue('Have components',API.HasKeyWord(oakComponents));
  Data.CreateDefaultTypeMaps;
  Data.CreateDefaultAPITypeMaps;
  Data.CreateServiceDefs;
  AssertEquals('API type count',1,Data.TypeCount);
  AssertEquals('Service count',0,Data.ServiceCount);
end;

procedure TTestPascalTypes.TestServiceNoApplicationJSONRequestBody;

begin
  load('noapplicationjsonrequestbody');
  AssertTrue('Have components',API.HasKeyWord(oakComponents));
  Data.CreateDefaultTypeMaps;
  Data.CreateDefaultAPITypeMaps;
  Data.CreateServiceDefs;
  AssertEquals('API type count',1,Data.TypeCount);
  AssertEquals('Service count',0,Data.ServiceCount);
end;

procedure TTestPascalTypes.Load(const aFileName : string);

var
  lReader : TOpenAPIReader;

begin
  lReader:=TOpenAPIReader.Create(Nil);
  try
    lReader.ReadFromFile(API,'data'+PathDelim+aFileName+'.json');
  finally
    lReader.Free;
  end;
end;

initialization
  RegisterTest(TTestPascalTypes);
end.

