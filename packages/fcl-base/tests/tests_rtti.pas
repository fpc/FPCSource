unit tests_rtti;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  Rtti;

type

  { TTestCase1 }

  TTestCase1= class(TTestCase)
  published
    procedure GetTypes;
    procedure GetTypeInteger;
    procedure GetClassProperties;

    procedure GetClassAttributes;
    procedure GetClassPropertiesAttributes;

    procedure GetClassPropertiesValue;
  end;

  { TIntAttribute }

  TIntAttribute = class(TCustomAttribute)
  private
    FInt: Integer;
  public
    constructor create(AInt: integer); virtual;
    property Int: integer read FInt;
  end;

  [TIntAttribute(1)]
  [TIntAttribute(2)]
  TGetClassProperties = class
  private
    FPubPropRO: integer;
    FPubPropRW: integer;
  published
    property PubPropRO: integer read FPubPropRO;
    [TIntAttribute(3)]
    property PubPropRW: integer read FPubPropRW write FPubPropRW;
    property PubPropSetRO: integer read FPubPropRO;
    [TIntAttribute(4)]
    property PubPropSetRW: integer read FPubPropRW write FPubPropRW;
  end;

implementation

{ TIntAttribute }

constructor TIntAttribute.create(AInt: integer);
begin
  FInt:=AInt;
end;

procedure TTestCase1.GetTypes;
var
  LContext: TRttiContext;
  LType: TRttiType;
  IsTestCaseClassFound: boolean;
begin
  LContext := TRttiContext.Create;

  { Enumerate all types declared in the application }
  for LType in LContext.GetTypes() do
    begin
    if LType.Name='TTestCase1' then
      IsTestCaseClassFound:=true;
    end;
  LContext.Free;
  CheckTrue(IsTestCaseClassFound, 'RTTI information does not contain class of testcase.');
end;

procedure TTestCase1.GetTypeInteger;
var
  LContext: TRttiContext;
  LType: TRttiType;
begin
  LContext := TRttiContext.Create;

  LType := LContext.GetType(TypeInfo(integer));
  CheckEquals(LType.Name, 'LongInt');

  LContext.Free;
end;

procedure TTestCase1.GetClassProperties;
var
  LContext: TRttiContext;
  LType: TRttiType;
  PropList: TRttiPropertyArray;
begin
  LContext := TRttiContext.Create;

  LType := LContext.GetType(TypeInfo(TGetClassProperties));
  PropList := LType.GetProperties;

  CheckEquals(4, length(PropList));
  CheckEquals('PubPropRO', PropList[0].Name);
  CheckEquals('PubPropRW', PropList[1].Name);
  CheckEquals('PubPropSetRO', PropList[2].Name);
  CheckEquals('PubPropSetRW', PropList[3].Name);

  LContext.Free;
end;

procedure TTestCase1.GetClassAttributes;
var
  LContext: TRttiContext;
  LType: TRttiType;
  AttrList: TAttributeArray;
begin
  LContext := TRttiContext.Create;

  LType := LContext.GetType(TypeInfo(TGetClassProperties));
  AttrList := LType.GetAttributes;

  CheckEquals(2, length(AttrList));
  CheckEquals('TIntAttribute', AttrList[0].ClassName);
  CheckEquals('TIntAttribute', AttrList[1].ClassName);
  CheckEquals(1, TIntAttribute(AttrList[0]).Int);
  CheckEquals(2, TIntAttribute(AttrList[1]).Int);

  LContext.Free;
end;

procedure TTestCase1.GetClassPropertiesAttributes;
var
  LContext: TRttiContext;
  LType: TRttiType;
  PropList: TRttiPropertyArray;
  AttrList: TAttributeArray;
begin
  LContext := TRttiContext.Create;

  LType := LContext.GetType(TypeInfo(TGetClassProperties));
  PropList := LType.GetProperties;

  AttrList := PropList[1].GetAttributes;
  CheckEquals(1, length(AttrList));
  CheckEquals(3, TIntAttribute(AttrList[0]).Int);

  AttrList := PropList[3].GetAttributes;
  CheckEquals(1, length(AttrList));
  CheckEquals(4, TIntAttribute(AttrList[0]).Int);

  LContext.Free;
end;

procedure TTestCase1.GetClassPropertiesValue;
var
  AGetClassProperties: TGetClassProperties;
  LContext: TRttiContext;
  LType: TRttiType;
  AValue: TValue;
begin
  LContext := TRttiContext.Create;

  LType := LContext.GetType(TGetClassProperties);

  AGetClassProperties := TGetClassProperties.Create;
  try
    AGetClassProperties.PubPropRW:=12345;

    AValue := LType.GetProperty('PubPropRW').GetValue(AGetClassProperties);
    CheckEquals(12345, AValue.AsInteger);

  finally
    AGetClassProperties.Free;
  end;

  LContext.Free;
end;

initialization
  RegisterTest(TTestCase1);
end.

