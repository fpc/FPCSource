unit fpjson.schema.testutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, contnrs,fpcunit, fpjson, fpjson.schema.types;

type

  { TTestDef }

  TTestDef = Class(TObject)
  private
    FFileName : String;
    FDescription: string;
    FSchema: TJSONData;
  Public
    constructor create (const aDescription : String; aSchema : TJSONData);
    destructor Destroy; override;
    Property FileName : String Read FFileName Write FFileName;
    Property Description : string read FDescription;
    Property Schema : TJSONData Read FSchema;
  end;

  { TTestDefs }

  TTestDefs = Class(TFPObjectList)
  private
    function GetDef(aIndex : Integer): TTestDef;
  Public
    Property Defs[aIndex : Integer] : TTestDef Read GetDef; default;
  end;

  TSchemaTestCase = class(TTestCase)
  Public
    class Procedure AssertEquals(aMsg : String; aExpected,aActual : TJSONSchemaKeyword); overload;
    class Procedure AssertEquals(aMsg : String; aExpected,aActual : TJSONSubSchema); overload;
    class Procedure AssertEquals(aMsg : String; aExpected,aActual : TStringFormatValidator); overload;
    class Procedure AssertEquals(aMsg : String; aExpected,aActual : TSchemaSimpleType); overload;
  end;

Function ExtractTestsFromStream(aStream : TStream; aList : TTestDefs; const aFileName : string = '') : Integer;
Function ExtractTestsFromFile(const aFileName : String; aList : TTestDefs) : Integer;

implementation

uses typinfo;

function GetTestDef(aObj : TJSONObject) : TTestDef;

var
  Descr : String;
  Schema : TJSONData;

begin
  Descr:=aObj.Get('description','');
  Schema:=aObj.Extract('schema');
  if Schema<>Nil then
    Result:=TTestDef.Create(Descr,Schema);
end;

function ExtractTestsFromStream(aStream: TStream; aList: TTestDefs; const aFileName : string = ''): Integer;

var
  D : TJSONData;
  Enum : TJSONEnum;
  O : TJSONObject absolute D;
  A : TJSONArray absolute D;
  Def : TTestDef;

begin
  Result:=0;
  D:=GetJSON(aStream);
  try
    if D is TJSONArray then
      For Enum in A do
        if Enum.Value is TJSONObject then
          begin
          Def:=GetTestDef(TJSONObject(Enum.Value));
          if Assigned(Def) then
            begin
            Def.FileName:=aFileName;
            inc(Result);
             aList.Add(Def);
            end;
          end;
    if D is TJSONObject then
      begin
      Def:=GetTestDef(O);
      if Assigned(Def) then
        begin
        inc(Result);
        Def.FileName:=aFileName;
        aList.Add(Def);
        end;
      end;
  finally
    D.Free;
  end;
end;

function ExtractTestsFromFile(const aFileName : String; aList: TTestDefs): Integer;

var
  F : TFileStream;

begin
  F:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
  try
    Result:=ExtractTestsFromStream(F,aList,aFileName);
  finally
    F.Free;
  end;
end;

{ TTestDef }

constructor TTestDef.create(const aDescription: String; aSchema: TJSONData);
begin
  FDescription:=aDescription;
  FSchema:=aSchema;
end;

destructor TTestDef.Destroy;
begin
  FreeAndNil(FSchema);
  inherited Destroy;
end;

{ TTestDefs }

function TTestDefs.GetDef(aIndex : Integer): TTestDef;
begin
  Result:=Items[aIndex] as TTestDef;
end;

{ TSchemaTestCase }

class procedure TSchemaTestCase.AssertEquals(aMsg: String; aExpected, aActual: TJSONSchemaKeyword);
begin
  AssertEquals(aMsg,GetEnumName(TypeInfo(TJSONSchemaKeyword),Ord(aExpected)),
                    GetEnumName(TypeInfo(TJSONSchemaKeyword),Ord(aActual)));
end;

class procedure TSchemaTestCase.AssertEquals(aMsg: String; aExpected, aActual: TJSONSubSchema);
begin
  AssertEquals(aMsg,GetEnumName(TypeInfo(TJSONSubSchema),Ord(aExpected)),
                    GetEnumName(TypeInfo(TJSONSubSchema),Ord(aActual)));
end;

class procedure TSchemaTestCase.AssertEquals(aMsg: String; aExpected, aActual: TStringFormatValidator);
begin
  AssertEquals(aMsg,GetEnumName(TypeInfo(TStringFormatValidator),Ord(aExpected)),
                    GetEnumName(TypeInfo(TStringFormatValidator),Ord(aActual)));
end;

class procedure TSchemaTestCase.AssertEquals(aMsg: String; aExpected, aActual: TSchemaSimpleType);
begin
  AssertEquals(aMsg,GetEnumName(TypeInfo(TSchemaSimpleType),Ord(aExpected)),
                    GetEnumName(TypeInfo(TSchemaSimpleType),Ord(aActual)));
end;

end.

