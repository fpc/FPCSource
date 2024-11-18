unit testsqldbopenapi;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, fpjson.schema.types, fpjson.schema.schema,
  fpopenapi.types, fpopenapi.objects, sqldbrestschema, sqldbrestopenapi, fpopenapi.writer,
  jsonwriter;


Type

  { TTestSQLDBRestOpenAPI }

  TTestSQLDBRestOpenAPI = class(TTestCase)
  private
    FConverter: TSLQDBRestSchemaToOpenAPI;
    FOpenAPI: TOpenAPI;
    FSchema: TSQLDBRestSchema;
  protected
    procedure AssertGetOperation(aComponent: String);
    procedure AssertPostOperation(aComponent: String);
    procedure AssertListComponent(aComponent: string);
    procedure AssertListOperation(aComponent: String);
    procedure AssertSimpleComponent(aComponent: string; aExtraProperty: TSchemaSimpleType=sstNone);
    procedure Convert;
    function CreateResource(withID: boolean; aSecondFieldType: TRestFieldType): TSQLDBRestResource;
  Public
    Procedure SetUp; override;
    Procedure TearDown; override;
    Property Converter : TSLQDBRestSchemaToOpenAPI Read FConverter;
    Property OpenAPI : TOpenAPI Read FOpenAPI;
    Property Schema : TSQLDBRestSchema Read FSchema;
  Published
    Procedure TestHookup;
    procedure TestResourceReadOnly;
    procedure TestResourceReadOnlyWithID;
    procedure TestResourcePostOnly;
  end;

implementation

{ TTestSQLDBRestOpenAPI }

procedure TTestSQLDBRestOpenAPI.SetUp;
begin
  inherited SetUp;
  FConverter:=TSLQDBRestSchemaToOpenAPI.Create(Nil);
  FOpenAPI:=TOpenAPI.Create;
  FSchema:=TSQLDBRestSchema.Create(Nil);
end;

procedure TTestSQLDBRestOpenAPI.TearDown;
begin
  FreeAndNil(FSchema);
  FreeAndNil(FOpenAPI);
  FreeAndNil(FConverter);
  inherited TearDown;
end;

procedure TTestSQLDBRestOpenAPI.TestHookup;
begin
  AssertNotNull('Have converter',Converter);
end;

function TTestSQLDBRestOpenAPI.CreateResource(withID : boolean; aSecondFieldType: TRestFieldType) : TSQLDBRestResource;

var
  lField : TSQLDBRestField;

begin
  Result:=Schema.Resources.AddResource('simple','simple');
  lField:=Result.Fields.AddField('id',rftInteger,[]);
  if WithID then
    lField.Options:=lField.Options+[foInKey];
  if aSecondFieldType<>rftUnknown then
    Result.Fields.AddField('b',aSecondFieldType,[]);
end;

procedure TTestSQLDBRestOpenAPI.Convert;

var
  Writer : TOpenAPIWriter;
  J : TJSONDataWriter;
  D : TJSONData;

begin
  Converter.Convert(Schema,OpenAPI);
  Writer:=TOpenAPIWriter.Create(Nil);
  J:=TJSONDataWriter.Create;
  try
    Writer.Write(OpenAPI,J);
    Writeln(TestName,' OpenAPI:');
    D:=J.ExtractData;
    Writeln(D.FormatJSON);
  finally
    D.Free;
    J.Free;
  end;
end;

procedure TTestSQLDBRestOpenAPI.AssertGetOperation(aComponent : String);

var
  lPath : TPathItem;
  Op : TAPIOperation;
  Res : TResponse;
  lMedia : TMediaType;

begin
  lPath:=OpenAPI.Paths[aComponent+'/{ID}'];
  AssertNotNull('have '+aComponent+'/{ID} path',lPath);
  AssertTrue('Get Operation',lPath.HasKeyWord(pkGet));
  OP:=lPath.Get;
  AssertEquals('Get OperationID','Get'+aComponent,OP.OperationId);
  AssertEquals('response count',1, OP.Responses.Count);
  AssertNotNull('Get default response',OP.Responses['default']);
  AssertEquals('response count',1, OP.Responses.Count);
  Res:=OP.Responses['default'];
  AssertNotNull('Have default response',Res);
  AssertTrue('Havemedia count',Res.HasKeyWord(rkContent));
  lMedia:=Res.Content.MediaTypes['application/json'];
  AssertNotNull('Have media',lMedia);
  AssertTrue('Have schema',lMedia.HasKeyWord(mtkSchema));
  AssertEquals('Have component ref','#components/schema/'+aComponent,lMedia.Schema.Ref);
end;

procedure TTestSQLDBRestOpenAPI.AssertPostOperation(aComponent: String);
var
  lPath : TPathItem;
  Op : TAPIOperation;
  Res : TResponse;
  lMedia : TMediaType;

begin
  lPath:=OpenAPI.Paths[aComponent];
  AssertNotNull('have '+aComponent+' path',lPath);
  AssertTrue('Post Operation',lPath.HasKeyWord(pkPost));
  OP:=lPath.Post;
  AssertEquals('Get OperationID','Create'+aComponent,OP.OperationId);
  AssertEquals('response count',1, OP.Responses.Count);
  AssertNotNull('Get default response',OP.Responses['default']);
  AssertEquals('response count',1, OP.Responses.Count);
  Res:=OP.Responses['default'];
  AssertNotNull('Have default response',Res);
  AssertTrue('Havemedia count',Res.HasKeyWord(rkContent));
  lMedia:=Res.Content.MediaTypes['application/json'];
  AssertNotNull('Have media',lMedia);
  AssertTrue('Have schema',lMedia.HasKeyWord(mtkSchema));
  AssertEquals('Have component ref','#components/schema/'+aComponent,lMedia.Schema.Ref);
end;

procedure TTestSQLDBRestOpenAPI.AssertListOperation(aComponent : String);

var
  lPath : TPathItem;
  Op : TAPIOperation;
  Res : TResponse;
  lMedia : TMediaType;

begin
  lPath:=OpenAPI.Paths[aComponent];
  AssertNotNull('have '+acomponent+' path',lPath);
  AssertTrue('Get Operation',lPath.HasKeyWord(pkGet));
  OP:=lPath.Get;
  AssertEquals('Get OperationID','List'+aComponent,OP.OperationId);
  AssertEquals('response count',1, OP.Responses.Count);
  AssertNotNull('Get default response',OP.Responses['default']);
  AssertEquals('response count',1, OP.Responses.Count);
  Res:=OP.Responses['default'];
  AssertNotNull('Have default response',Res);
  AssertTrue('Havemedia count',Res.HasKeyWord(rkContent));
  lMedia:=Res.Content.MediaTypes['application/json'];
  AssertNotNull('Have media',lMedia);
  AssertTrue('Have schema',lMedia.HasKeyWord(mtkSchema));
  AssertEquals('Have component ref','#components/schema/'+aComponent+'List',lMedia.Schema.Ref);
end;

Procedure TTestSQLDBRestOpenAPI.AssertSimpleComponent(aComponent : string; aExtraProperty : TSchemaSimpleType = sstNone);

var
  S,el : TJSONSchema;

begin
  AssertTrue('Components',OpenAPI.HasKeyWord(oakComponents));
  AssertTrue('Components.Schemas',OpenAPI.Components.HasKeyWord(ckSchemas));
  S:=OpenAPI.Components.Schemas[aComponent];
  AssertNotNull('Component '+aComponent+' Schema',S);
  AssertTrue(aComponent+' is array',S.Validations.Types=[sstObject]);
  AssertEquals(aComponent+' property count',1+Ord(aExtraProperty<>sstNone),S.properties.Count);
  el:=S.Properties[0];
  AssertNotNull(aComponent+'property 0 is valid',el);
  AssertEquals(aComponent+'property 0 is valid','id',el.Name);
  AssertTrue(aComponent+'property id type',el.Validations.Types=[sstInteger]);
  if aExtraProperty<>sstNone then
    begin
    el:=S.Properties[1];
    AssertNotNull(aComponent+'property 1 is valid',el);
    AssertEquals(aComponent+'property 1 is valid','b',el.Name);
    AssertTrue(aComponent+'property b type',el.Validations.Types=[aExtraProperty]);
    end
end;


Procedure TTestSQLDBRestOpenAPI.AssertListComponent(aComponent : string);

var
  S,el : TJSONSchema;

begin
  AssertTrue('Components',OpenAPI.HasKeyWord(oakComponents));
  AssertTrue('Components.Schemas',OpenAPI.Components.HasKeyWord(ckSchemas));
  S:=OpenAPI.Components.Schemas[aComponent+'List'];
  AssertNotNull('Component '+aComponent+'List Schema',S);
  AssertTrue(aComponent+' is array',S.Validations.Types=[sstArray]);
  AssertTrue(aComponent+' has 1 item',S.items.Count=1);
  el:=S.Items[0];
  AssertNotNull(aComponent+' item is valid',el);
  AssertEquals(aComponent+' reference to component','#components/schemas/'+aComponent,el.ref);

end;

procedure TTestSQLDBRestOpenAPI.TestResourceReadOnly;

var
  R : TSQLDBRestResource;

begin
  R:=CreateResource(False,rftUnknown);
  R.AllowedOperations:=[roGet];
  Convert;
  AssertTrue('Component schemas',OpenAPI.Components.HasKeyWord(ckSchemas));
  AssertEquals('Component Count',2, OpenAPI.Components.Schemas.Count);
  AssertSimpleComponent('simple');
  AssertListComponent('simple');
  AssertTrue('PathItems',OpenAPI.HasKeyWord(oakPaths));
  AssertEquals('Path Count',1, OpenAPI.Paths.Count);
  AssertListOperation('simple');
end;

procedure TTestSQLDBRestOpenAPI.TestResourceReadOnlyWithID;
var
  R : TSQLDBRestResource;

begin
  R:=CreateResource(True,rftUnknown);
  R.AllowedOperations:=[roGet];
  Convert;
  AssertTrue('Components',OpenAPI.HasKeyWord(oakComponents));
  AssertTrue('Component schemas',OpenAPI.Components.HasKeyWord(ckSchemas));
  AssertEquals('Component Count',2, OpenAPI.Components.Schemas.Count);
  AssertSimpleComponent('simple');
  AssertListComponent('simple');
  AssertTrue('PathItems',OpenAPI.HasKeyWord(oakPaths));
  AssertEquals('Path Count',2, OpenAPI.Paths.Count);
  AssertListOperation('simple');
  AssertGetOperation('simple');
end;

procedure TTestSQLDBRestOpenAPI.TestResourcePostOnly;
var
  R : TSQLDBRestResource;

begin
  R:=CreateResource(True,rftUnknown);
  R.AllowedOperations:=[roPost];
  Convert;
  AssertTrue('Components',OpenAPI.HasKeyWord(oakComponents));
  AssertTrue('Component schemas',OpenAPI.Components.HasKeyWord(ckSchemas));
  AssertEquals('Component Count',1, OpenAPI.Components.Schemas.Count);
  AssertSimpleComponent('simple');
//  AssertListComponent('simple');
  AssertTrue('PathItems',OpenAPI.HasKeyWord(oakPaths));
  AssertEquals('Path Count',1, OpenAPI.Paths.Count);
  AssertPostOperation('simple');
end;


initialization
  RegisterTest(TTestSQLDBRestOpenAPI);
end.

