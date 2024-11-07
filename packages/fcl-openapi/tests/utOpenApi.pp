unit utOpenApi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, fpopenapi.objects;

type

  { TTestOpenApi }

  TTestOpenApi= class(TTestCase)
  private
    FOpenAPI: TOpenAPI;
  Public
    procedure SetUp; override;
    procedure TearDown; override;
    Property OpenApi : TOpenAPI Read FOpenAPI;
  published
    procedure TestHookUp;
    procedure TestFind;
  end;

implementation

procedure TTestOpenApi.SetUp;
begin
  inherited SetUp;
  FOpenApi:=TOpenAPi.Create;
end;

procedure TTestOpenApi.TearDown;
begin
  FreeAndNil(FOpenApi);
  inherited TearDown;
end;

procedure TTestOpenApi.TestHookUp;
begin
  AssertNotNull('Have api',OpenApi);
end;

procedure TTestOpenApi.TestFind;

var
  P : TBaseOpenAPIObject;


begin
  AssertNull('Nothing yet',OpenApi.Find('paths'));
  P:=OpenApi.Paths.AddItem('get');
  AssertSame('Have paths',OpenApi.Paths,OpenApi.Find('paths'));
  AssertSame('Have paths/get',P,OpenApi.Find('paths/get'));
end;



initialization

  RegisterTest(TTestOpenApi);
end.

