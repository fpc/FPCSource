unit tctstopas;

{$mode ObjFPC}{$H+}
{ $define dumpsource}
interface

uses
  Classes, SysUtils, fpcunit, testregistry, tstopas;

Type

  { TTestTSToPas }
  TMyTypescriptToPas = class(TTypescriptToPas)
  end;

  TTestTSToPas = Class(TTestCase)
  private
    FConverter: TTypescriptToPas;
    function GetConversionOptions: TConversionOptions;
    procedure SetConversionOptions(AValue: TConversionOptions);
  Public
    Procedure Setup; override;
    procedure TearDown; override;
    procedure Convert(aSource : string); overload;
    procedure Convert(aSource : Array of String); overload;
    procedure Convert(aSource : TStrings); overload;
    procedure CheckDeclaration(const aSection, aDeclaration : String);
    procedure CheckDeclaration(const aSection, aDeclaration, aDeclaration2 : String);
    procedure CheckDeclarations(const aSection : String; Const Declarations : Array of string);
    Property Converter : TTypescriptToPas Read FConverter;
    Property ConversionOptions : TConversionOptions Read GetConversionOptions Write SetConversionOptions;
  Published
    Procedure TestEmpty;
    Procedure TestVarDeclaration;
    Procedure Test2VarDeclarations;
    Procedure Test3VarDeclarations;
    Procedure TestVarIndirectType;
    Procedure TestKeywordVarDeclaration;
    Procedure TestSimpleType;
    Procedure TestAliasType;
    Procedure TestAliasAliasedType;
    Procedure TestUnionType;
    procedure TestUnionTypeAllStrings;
    Procedure TestIntersectionType;
    Procedure TestUnionIntersectionType;
    Procedure TestEnumType;
    Procedure TestArrayType;
    Procedure TestTupleType;
    Procedure TestTupleTypeForceUntyped;
    Procedure TestTupleTypeUnbounded;
    Procedure TestTupleTypeForceUntypedUnbounded;
    procedure TestTupleTypeUnequalTypes;
    procedure TestTupleTypeUnequalTypesUnbounded;
    Procedure TestFunctionType;
    Procedure TestFunctionTypeWithArg;
    Procedure TestFunctionTypeWithReturn;
    procedure TestFunctionTypeWithTupleReturn;
    Procedure TestFunctionTypeWithReturnNoArgs;
    Procedure TestFunctionTypeArrayType;
    Procedure TestFunctionTypeArrayTypeObj;
    Procedure TestFunctionTypeArrayTypeArray;
    Procedure TestFunctionCallbackArg;
    Procedure TestFunctionCallbackArgRecursive;
    Procedure TestSimpleFunction;
    Procedure TestSimpleFunctionKeyword;
    Procedure TestExportSimpleFunction;
    Procedure TestFunctionSimpleResult;
    Procedure TestFunctionTypeRefResult;
    procedure TestFunctionOneArg;
    procedure TestFunctionOneArgUntyped;
    procedure TestFunctionTwoArgs;
    Procedure TestFunctionFunctionResult;
    Procedure TestOverloadedProcedures;
    Procedure TestUnionProcedures;
    Procedure TestIndirectUnionProcedures;
    Procedure TestUniqueOverloadedProcedures;
    Procedure TestEmptyNameSpace;
    Procedure TestEmptyNameSpaceFunction;
    Procedure TestExportInterface;
    Procedure TestExportInterfaceAsClass;
    Procedure TestExportInterfaceWithPropertiesAsClass;
    Procedure TestExportInterfacePropertyCallbackArgRecursive;
    Procedure TestInterfaceNamedFunction;
    Procedure TestInterfaceNamedFunctionCallback;
    Procedure TestObjectEmpty;
    procedure TestObjectOneProperty;
    procedure TestObjectOneReadOnlyProperty;
    procedure TestObjectOneReadOnlyPropertyKeyword;
    procedure TestClassOnePrivateProperty;
    procedure TestClassOneMethod;
    Procedure TestClassOneMethodKeyword;
    procedure TestClassOneConstructor;
    procedure TestClassPropertyArrayType;
    procedure TestClassPropertyObjectType;
    procedure TestClassPropertyObjectTypeRecursive;
    procedure TestClassMethodOneCallback;
    procedure TestClassMethodCallBackArrayTuple;
    procedure TestClassMethodTupleReturn;
    procedure TestClassMethodOneCallbackLocalArgTypes;
    procedure TestNameSpaceClassLocalType;
  end;

implementation

{ TTestTSToPas }

function TTestTSToPas.GetConversionOptions: TConversionOptions;
begin
  Result:=FConverter.Options;
end;

procedure TTestTSToPas.SetConversionOptions(AValue: TConversionOptions);
begin
  FConverter.Options:=aValue;
end;

procedure TTestTSToPas.Setup;

begin
  inherited Setup;
  FConverter:=TMyTypescriptToPas.Create(Nil);
  FConverter.Options:=FConverter.Options+[coRaw];
end;

procedure TTestTSToPas.TearDown;

begin
  FreeAndNil(FConverter);
  inherited TearDown;
end;

procedure TTestTSToPas.Convert(aSource: string);


begin
  Convert([aSource]);
end;

procedure TTestTSToPas.Convert(aSource: array of String);
Var
  aSrc : TStrings;

begin
  aSrc:=TStringList.Create;
  try
    TStringList(aSrc).SkipLastLineBreak:=True;
    aSrc.AddStrings(aSource);
{$IFDEF dumpsource}
    if IsConsole then
      begin
      Writeln('--');
      Writeln(aSrc.Text);
      Writeln('--');
      end;
{$ENDIF dumpsource}
    Convert(aSrc);
  finally
    aSrc.Free;
  end;
end;

procedure TTestTSToPas.Convert(aSource: TStrings);
Var
  S : TStream;
begin
  S:=TStringStream.Create(aSource.Text);
  try
    FConverter.InputStream:=S;
    FConverter.Execute;
  finally
    S.Free;
  end;
end;

procedure TTestTSToPas.CheckDeclaration(const aSection, aDeclaration: String);

begin
  CheckDeclarations(aSection,[aDeclaration]);
end;

procedure TTestTSToPas.CheckDeclaration(const aSection, aDeclaration, aDeclaration2: String);

begin
  CheckDeclarations(aSection,[aDeclaration,aDeclaration2]);
end;

procedure TTestTSToPas.CheckDeclarations(const aSection: String; const Declarations: array of string);
Var
  Src : TStrings;
  I,J : Integer;
  D,S,actSrc : String;

begin
  Src:=FConverter.Source;
{$IFDEF dumpsource}
  if IsConsole then
    begin
    Writeln('>>>');
    Writeln(Src.Text);
    Writeln('<<<');
    end;
{$ENDIF dumpsource}
  I:=0;
  While (I<Src.Count) and (Trim(Src[i])='') do
    Inc(I);
  if aSection<>'' then
    begin
    AssertTrue('Section: Not at end',I<Src.Count);
    AssertEquals('Section correct',LowerCase(aSection),LowerCase(Trim(Src[i])));
    Inc(I);
    end;
  For J:=0 to Length(Declarations)-1 do
    begin
    D:=Format('Declaration %d: ',[J]);
    S:=Declarations[J];
    While (I<Src.Count) and (Trim(Src[i])='') do
      Inc(I);
    AssertTrue(D+'Not at end',I<Src.Count);
    actSrc:=Src[i];
    AssertEquals(D+'Declaration correct',LowerCase(S),LowerCase(Trim(actSrc)));
    Inc(I);
    end;
end;

procedure TTestTSToPas.TestEmpty;
begin
  AssertNotNull(Converter);
end;

procedure TTestTSToPas.TestVarDeclaration;
begin
  Convert('declare var x : number;');
  CheckDeclaration('var','x : double; external name ''x'';');
end;

procedure TTestTSToPas.Test2VarDeclarations;
begin
  Convert('declare var x,y : number;');
  CheckDeclaration('var','x : double; external name ''x'';','y : double; external name ''y'';');
end;

procedure TTestTSToPas.Test3VarDeclarations;
begin
  Convert('declare var x,y,z : number;');
  CheckDeclarations('var',['x : double; external name ''x'';','y : double; external name ''y'';','z : double; external name ''z'';']);
end;

procedure TTestTSToPas.TestVarIndirectType;
begin
  Convert('declare var a : { b : string;};');
  CheckDeclarations('type',[
    'TA = class external name ''Object'' (TJSObject)',
    'public',
    'b : string;',
    'end;',
    'var',
    'a : ta; external name ''a'';']);
end;

procedure TTestTSToPas.TestKeywordVarDeclaration;
begin
  Convert('declare var on : string;');
  CheckDeclarations('var',['&on : string; external name ''on'';']);
end;

procedure TTestTSToPas.TestSimpleType;
begin
  Convert('declare type MyType = string;');
  CheckDeclarations('type',['TMyType = string;']);
end;

procedure TTestTSToPas.TestAliasType;
begin
  Convert('declare type MyType = SomeOtherType;');
  CheckDeclarations('type',['TMyType = SomeOtherType;']);
end;

procedure TTestTSToPas.TestAliasAliasedType;
begin
  Converter.TypeAliases.Add('SomeOtherType=TMyOther');
  Convert('declare type MyType = SomeOtherType;');
  CheckDeclarations('type',['TMyType = TMyOther;']);
end;

procedure TTestTSToPas.TestUnionType;
begin
  Convert('declare type MyType = string | number;');
  CheckDeclarations('type',['TMyType = JSValue; // string | number']);
end;

procedure TTestTSToPas.TestUnionTypeAllStrings;
begin
  Convert('declare type MyType = ''string'' | ''number'';');
  CheckDeclarations('type',['TMyType = string; // Restricted values']);
end;

procedure TTestTSToPas.TestIntersectionType;
begin
  Convert('declare type MyType = string & number;');
  CheckDeclarations('type',['TMyType = JSValue; // string & number']);
end;

procedure TTestTSToPas.TestUnionIntersectionType;
begin
  Convert('declare type MyType = number | (string & number) ;');
  CheckDeclarations('type',['TMyType = JSValue; // number | (string & number)']);
end;

procedure TTestTSToPas.TestEnumType;
begin
  Convert('declare enum Color {Red, Green, Blue} ;');
  CheckDeclarations('type',['TColor = (Red, Green, Blue);']);
end;

procedure TTestTSToPas.TestArrayType;
begin
  Convert('declare type A = number[];');
  CheckDeclarations('type',['TA = array of Double;']);
end;

procedure TTestTSToPas.TestTupleType;
begin
  Convert('declare type A = [number,number];');
  CheckDeclarations('type',['TA = array[0..1] of Double;']);
end;

procedure TTestTSToPas.TestTupleTypeForceUntyped;
begin
  ConversionOptions:=ConversionOptions+[coUntypedTuples];
  Convert('declare type A = [number,number];');
  CheckDeclarations('type',['TA = array[0..1] of jsValue;']);
end;

procedure TTestTSToPas.TestTupleTypeUnbounded;
begin
  ConversionOptions:=ConversionOptions+[coDynamicTuples];
  Convert('declare type A = [number,number];');
  CheckDeclarations('type',['TA = array of double;']);
end;

procedure TTestTSToPas.TestTupleTypeForceUntypedUnbounded;
begin
  ConversionOptions:=ConversionOptions+[coDynamicTuples,coUntypedTuples];
  Convert('declare type A = [number,number];');
  CheckDeclarations('type',['TA = tjsvaluedynarray;']);
end;

procedure TTestTSToPas.TestTupleTypeUnequalTypes;
begin
  Convert('declare type A = [number,string];');
  CheckDeclarations('type',['TA = array[0..1] of jsvalue;']);
end;

procedure TTestTSToPas.TestTupleTypeUnequalTypesUnbounded;
begin
  ConversionOptions:=ConversionOptions+[coDynamicTuples];
  Convert('declare type A = [number,string];');
  CheckDeclarations('type',['TA = tjsvaluedynarray;']);
end;

procedure TTestTSToPas.TestFunctionType;
begin
  Convert('declare type A = () => void;');
  CheckDeclarations('type',['TA = procedure;']);
end;

procedure TTestTSToPas.TestFunctionTypeWithArg;
begin
  Convert('declare type A = (B : string) => void;');
  CheckDeclarations('type',['TA = procedure (B : string);']);
end;

procedure TTestTSToPas.TestFunctionTypeWithReturn;
begin
  Convert('declare type A = (B : string) => number;');
  CheckDeclarations('type',['TA = function (B : string): Double;']);
end;

procedure TTestTSToPas.TestFunctionTypeWithTupleReturn;
begin
  Convert('declare type A = (B : string) => [number,number];');
  CheckDeclarations('type',[
    'TTA_Result = array[0..1] of double;',
    'TA = function (B : string): TTA_Result;'
    ]);
end;

procedure TTestTSToPas.TestFunctionTypeWithReturnNoArgs;
begin
  Convert('declare type A = () => number;');
  CheckDeclarations('type',['TA = function: Double;']);
end;

procedure TTestTSToPas.TestFunctionTypeArrayType;
begin
  Convert('declare type A = (B : string[]) => void;');
  CheckDeclarations('type',['TA = procedure (B : array of string);']);
end;

procedure TTestTSToPas.TestFunctionTypeArrayTypeObj;
begin
  Convert('declare function b (a : Array<{}>): string;');
  CheckDeclarations('type',[
    'tb_a_item = class external name ''Object'' (TJSObject)',
    'end;',
    'tb_a = array of tb_a_item;',
    'function b(a : Tb_a): string; external name ''b'';'
  ]);
end;

procedure TTestTSToPas.TestFunctionTypeArrayTypeArray;
begin
  Convert('declare function a(b: string[][]): void;');
  CheckDeclarations('type',[
    'ta_b_item = array of string;',
    'ta_b = array of ta_b_item;',
    'Procedure a(b : Ta_b); external name ''a'';'
  ]);
end;

procedure TTestTSToPas.TestFunctionCallbackArg;
begin
  Convert('declare function b (para1 : (a: number) => string) : string;');
  CheckDeclarations('type',[
    'tb_para1 = function (a : double): string;',
    'function b(para1 : Tb_para1): string; external name ''b'';'
  ]);
end;

procedure TTestTSToPas.TestFunctionCallbackArgRecursive;
begin
  Convert('declare function b (para1 : (a: (c: string) =>void) => string) : string;');
  CheckDeclarations('type',[
    'tb_para1_a = procedure (c : string);',
    'tb_para1 = function (a : tb_para1_a): string;',
    'function b(para1 : Tb_para1): string; external name ''b'';'
  ]);
end;

procedure TTestTSToPas.TestSimpleFunction;
begin
  Convert('declare function A() : void;');
  CheckDeclarations('',['Procedure A; external name ''a'';']);
end;

procedure TTestTSToPas.TestSimpleFunctionKeyword;
begin
  Convert('declare function on() : void;');
  CheckDeclarations('',['Procedure &on; external name ''on'';']);
end;

procedure TTestTSToPas.TestExportSimpleFunction;
begin
  Convert('export function A() : void;');
  CheckDeclarations('',['Procedure A; external name ''A'';']);
end;

procedure TTestTSToPas.TestFunctionSimpleResult;
begin
  Convert('declare function A() : number;');
  CheckDeclarations('',['function A: double; external name ''A'';']);
end;

procedure TTestTSToPas.TestFunctionTypeRefResult;
begin
  Convert(['declare type B = number;','declare function A() : B;']);
  CheckDeclarations('type',['TB = double;','function A: TB; external name ''A'';']);
end;


procedure TTestTSToPas.TestFunctionOneArg;

begin
  Convert('declare function A(b : string) : void;');
  CheckDeclarations('',['procedure A(b : string); external name ''A'';']);
end;

procedure TTestTSToPas.TestFunctionOneArgUntyped;
begin
  Convert('declare function A(b) : void;');
  CheckDeclarations('',['procedure A(b : jsvalue); external name ''A'';']);
end;

procedure TTestTSToPas.TestFunctionTwoArgs;
begin
  Convert('declare function A(b : string, c : number) : void;');
  CheckDeclarations('',['procedure A(b : string; c : double); external name ''A'';']);
end;

procedure TTestTSToPas.TestFunctionFunctionResult;
begin
  convert('declare class A { b(): (c: { d : any }) => void; }');
  CheckDeclarations('Type',[
  '// Forward class definitions',
  'ta = class;',
  'ta_b_result_c = class external name ''object'' (TJSObject)',
  'public',
  'd : jsvalue;',
  'end;',
  'ta_b_result = procedure (c : ta_b_result_c);',
  'ta = class external name ''A'' (TJSObject)',
  'public',
  'function b: ta_b_result;',
  'end;'
  ])
end;

procedure TTestTSToPas.TestOverloadedProcedures;
begin
  Convert(['declare function A() : void;','declare function A(b : string) : void;']);
  CheckDeclarations('',[
    'procedure A; external name ''A''; overload;',
    'procedure A(b : string); external name ''A''; overload;']);
end;

procedure TTestTSToPas.TestUnionProcedures;
begin
  Converter.Options:=Converter.Options+[coExpandUnionTypeArgs];
  Convert(['declare function A(b: number | string) : void;']);
  CheckDeclarations('',[
    'procedure A(b : double); external name ''A''; overload;',
    'procedure A(b : string); external name ''A''; overload;']);
end;

procedure TTestTSToPas.TestIndirectUnionProcedures;
begin
  Converter.Options:=Converter.Options+[coExpandUnionTypeArgs];
  Convert(['declare type U = number | string;','declare function A(b: U) : void;']);
  CheckDeclarations('type',[
    'TU = JSValue; // number | string',
    'procedure A(b : double); external name ''A''; overload;',
    'procedure A(b : string); external name ''A''; overload;'
  ]);
end;

procedure TTestTSToPas.TestUniqueOverloadedProcedures;
begin
  Converter.Options:=Converter.Options+[coExpandUnionTypeArgs];
  Convert(['declare function A(b: number) : void;','declare function A(b: number | string) : void;']);
  CheckDeclarations('',[
    'procedure A(b : double); external name ''A''; overload;',
    'procedure A(b : string); external name ''A''; overload;'
  ]);
end;

procedure TTestTSToPas.TestEmptyNameSpace;
begin
  Convert(['declare namespace A { };']);
  CheckDeclarations('type',['// forward class definitions',
                            'TA = Class;',
                            '// Namespaces',
                            'TA = class external name ''A'' (TJSObject)',
                            'Public',
                            'end;']);
end;

procedure TTestTSToPas.TestEmptyNameSpaceFunction;
begin
  Convert(['declare namespace A { ',
           '  function B() : void;',
           '}']);
  CheckDeclarations('type',['// forward class definitions',
                            'TA = Class;',
                            '// Namespaces',
                            'TA = class external name ''A'' (TJSObject)',
                            'Public',
                            'procedure B;',
                            'end;']);

end;

procedure TTestTSToPas.TestExportInterface;

begin
  Convert('declare interface Color {  b () : string; } ;');
  CheckDeclarations('type',[
     '// Forward class definitions',
     'TColor = interface;',
     'TColor = interface',
     'function b: string;',
     'end;']
  );
end;

procedure TTestTSToPas.TestExportInterfaceAsClass;
begin
  ConversionOptions:=ConversionOptions+[coInterfaceAsClass];
  Convert('declare interface Color {  b () : string; } ;');
  CheckDeclarations('type',[
     '// Forward class definitions',
     'TColor = class;',
     'TColor = class external name ''object'' (TJSObject)',
     'function b: string;',
     'end;']
  );
end;

procedure TTestTSToPas.TestExportInterfaceWithPropertiesAsClass;
begin
  Convert('declare interface Color {  b : string; } ;');
  CheckDeclarations('type',[
     '// Forward class definitions',
     'TColor = class;',
     'TColor = class external name ''object'' (TJSObject)',
     'b : string;',
     'end;']
  );
end;

procedure TTestTSToPas.TestExportInterfacePropertyCallbackArgRecursive;
begin
  Convert('declare interface A { b?: (c: (d: Boolean) => void) => void; }');
  CheckDeclarations('type',[
     '// Forward class definitions',
     'TA = class;',
     'tA_b_c = procedure (d : boolean);',
     'tA_b = procedure (c : tA_b_c);',
     'TA = class external name ''object'' (TJSObject)',
     'b : TA_b;',
     'end;']
  );

end;

procedure TTestTSToPas.TestInterfaceNamedFunction;
begin
  Convert('declare interface a { (b : String, c: string): number; }');
  CheckDeclarations('type',[
  // '// Forward class definitions',
  'TA = function (B : String; C : string): double;'
  ]);
end;

procedure TTestTSToPas.TestInterfaceNamedFunctionCallback;
begin
  Convert('declare interface a { (b : (c: string) => void): number; }');
  CheckDeclarations('type',[
  'Ta__b = procedure (c : string);',
  'TA = function (B : TA__b): double;'
  ]);
end;

procedure TTestTSToPas.TestObjectEmpty;
begin
  Convert('declare type A = {  }');
  CheckDeclarations('type',[
    '// Forward class definitions',
    'TA = class;',
    'TA = class external name ''Object'' (TJSObject)',
    'end;']);
end;

procedure TTestTSToPas.TestObjectOneProperty;

begin
  Convert('declare type A = { prop : string; }');
  CheckDeclarations('type',[
    '// Forward class definitions',
    'TA = class;',
    'TA = class external name ''Object'' (TJSObject)',
    'Public',
    'prop : string;',
    'end;']);
end;

procedure TTestTSToPas.TestClassOnePrivateProperty;
begin
  Convert('declare class A { private prop : string; }');
  CheckDeclarations('type',[
    '// Forward class definitions',
    'TA = class;',
    'TA = class external name ''A'' (TJSObject)',
    'Private',
    'prop : string;',
    'end;']);
end;

procedure TTestTSToPas.TestClassOneMethod;

begin
  Convert(' export class A { b (c: string) : void; }');
  CheckDeclarations('type',[
    '// Forward class definitions',
    'TA = class;',
    'TA = class external name ''A'' (TJSObject)',
    'public',
    'procedure b(c : string);',
    'end;']);
end;

procedure TTestTSToPas.TestClassOneMethodKeyword;
begin
  Convert(' export class A { to() : void; }');
  CheckDeclarations('type',[
    '// Forward class definitions',
    'TA = class;',
    'TA = class external name ''A'' (TJSObject)',
    'public',
    'procedure &to;',
    'end;']);
end;

procedure TTestTSToPas.TestClassOneConstructor;
begin
  Convert(' export class A { constructor (c: string) : void; }');
  CheckDeclarations('type',[
    '// Forward class definitions',
    'TA = class;',
    'TA = class external name ''A'' (TJSObject)',
    'public',
    'constructor new(c : string);',
    'end;']);
end;

procedure TTestTSToPas.TestClassPropertyArrayType;
begin
  Convert(' export class A { b : string[] ; }');
  CheckDeclarations('type',[
    '// Forward class definitions',
    'TA = class;',
    'tA_b = array of string;',
    'TA = class external name ''A'' (TJSObject)',
    'public',
    'b : TA_b;',
    'end;']);
end;

procedure TTestTSToPas.TestClassPropertyObjectType;
begin
  Convert('declare interface A { B: { C : number; };  }');
  CheckDeclarations('type',[
    '// Forward class definitions',
    'TA = class;',
    'TA_b = class external name ''Object'' (TJSObject)',
    'public',
    'c : double;',
    'end;',
    'TA = class external name ''object'' (TJSObject)',
    'b : TA_B;',
    'end;']);
end;

procedure TTestTSToPas.TestClassPropertyObjectTypeRecursive;
begin
  Convert('declare interface A { B: { C: { D : number; }; }; }');
  CheckDeclarations('type',[
    '// Forward class definitions',
    'TA = class;',
    'TA_b_c = class external name ''Object'' (TJSObject)',
    'public',
    'd : double;',
    'end;',
    'TA_b = class external name ''Object'' (TJSObject)',
    'public',
    'c : Ta_b_c;',
    'end;',
    'TA = class external name ''Object'' (TJSObject)',
    'b : TA_B;',
    'end;']);


end;

procedure TTestTSToPas.TestClassMethodOneCallback;

begin
  Convert(' export class A { b (c: (d : number) => string) : void; }');
  CheckDeclarations('type',[
    '// Forward class definitions',
    'TA = class;',
    'tA_b_c = function (d : double): string;',
    'TA = class external name ''A'' (TJSObject)',
    'public',
    'procedure b(c : TA_b_c);',
    'end;']);
end;

procedure TTestTSToPas.TestClassMethodCallBackArrayTuple;
begin
  Convert('declare class A { b() : [number, number][]; }');
  CheckDeclarations('type',[
    '// Forward class definitions',
    'TA = class;',
    'TA_b_Result_Item = array[0..1] of double;',
    'tA_b_Result = array of TA_b_Result_Item;',
    'TA = class external name ''A'' (TJSObject)',
    'public',
    'function b: tA_b_Result;',
    'end;']);
end;

procedure TTestTSToPas.TestClassMethodTupleReturn;
begin
  Convert(' export class A { b () : [number, number]; }');
  CheckDeclarations('type',[
    '// Forward class definitions',
    'TA = class;',
    'tA_b_Result = array[0..1] of double;',
    'TA = class external name ''A'' (TJSObject)',
    'public',
    'function b: tA_b_Result;',
    'end;']);
end;

procedure TTestTSToPas.TestClassMethodOneCallbackLocalArgTypes;
begin
  ConversionOptions:=ConversionOptions+[coLocalArgumentTypes];
  Convert(' export class A { b (c: (d : number) => string) : void; }');
  CheckDeclarations('type',[
    '// Forward class definitions',
    'TA = class;',
    'TA = class external name ''A'' (TJSObject)',
    'public',
    'Type',
    'tb_c = function (d : double): string;',
    'public',
    'procedure b(c : Tb_c);',
    'end;']);
end;

procedure TTestTSToPas.TestNameSpaceClassLocalType;
begin
  Convert('declare module "a" { class b { c(d : string): string[]; }; }');
  CheckDeclarations('type',[
    '// Forward class definitions',
    'TA = class;',
    '// Modules',
    'TA = class external name ''a'' (TJSObject)',
    'Public',
    'Type',
    '// Forward class definitions',
    'TB = class;',
    'TB_c_Result = Array of string;',
    'TB = class external name ''b'' (TJSObject)',
    'Public',
    'function c(d : string): TB_c_Result;',
    'end;'
  ])
end;


procedure TTestTSToPas.TestObjectOneReadOnlyProperty;
begin
  Convert('declare type A = { readonly prop : string; }');
  CheckDeclarations('type',[
    '// Forward class definitions',
    'TA = class;',
    'TA = class external name ''Object'' (TJSObject)',
    'Private',
    'FProp : String; external name ''prop'';',
    'Public',
    'Property prop : string read FProp;',
    'end;']);
end;

procedure TTestTSToPas.TestObjectOneReadOnlyPropertyKeyword;
begin
  Convert('declare type A = { readonly on : string; }');
  CheckDeclarations('type',[
    '// Forward class definitions',
    'TA = class;',
    'TA = class external name ''Object'' (TJSObject)',
    'Private',
    'FOn : String; external name ''on'';',
    'Public',
    'Property on : string read FOn;',
    'end;']);
end;

Initialization
  Registertest(TTestTSToPas);
end.

