{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 Michael Van Canneyt (michael@freepascal.org)

    Test WIT model classes - mainly .ToString functionality.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit utcwitmodel;

{$mode ObjFPC}{$H+}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, WIT.Model;

Type

  { TTestWITModel }

  TTestWITModel = class (TTestCase)
  private
    FEl: TWITBaseElement;
  Public
    Procedure SetUp; override;
    Procedure TearDown; override;
    // This will be freed at the end...
    property Element : TWITBaseElement Read FEl Write FEl;
  Published
    Procedure TestPackageToString;
    procedure TestUseToString;
    procedure TestFunctionTypeToString;
    procedure TestFunctionToString;
    Procedure TestWITTypeToString;
    Procedure TestTypeDefToString;
    Procedure TestListTypeToString;
    Procedure TestTupleTypeToString;
    Procedure TestOptionTypeToString;
    Procedure TestResultTypeToString;
    Procedure TestHandleTypeToString;
    Procedure TestFutureTypeToString;
    Procedure TestStreamTypeToString;
    procedure TestRecordFieldToString;
    Procedure TestRecordToString;
    Procedure TestEnumToString;
    Procedure TestFlagsToString;
    Procedure TestVariantToString;
    Procedure TestResourceToString;
    Procedure TestInterfaceToString;
    procedure TestExchangeIdentifier;
    Procedure TestIncludeToString;
    Procedure TestWorldToString;
  end;

implementation

{ TTestWITModel }

procedure TTestWITModel.SetUp;
begin
  Inherited;
  FreeAndNil(FEl);
end;

procedure TTestWITModel.TearDown;
begin
  FreeAndNil(FEl);
  Inherited;
end;

procedure TTestWITModel.TestPackageToString;

var
  lPackage : TWITPackage;

begin
  lPackage:=TWITPackage.Create;
  Element:=lPackage;
  lPackage.PackageName:='a';
  AssertEquals('Name','package a;',Element.ToString);
  lPackage.Namespace:='b';
  AssertEquals('NameSpace+Name','package b:a;',Element.ToString);
  lPackage.Version:='1.1.1';
  AssertEquals('NameSpace+Name+version','package b:a@1.1.1;',Element.ToString);
  lPackage.Worlds.Add(TWITWorld.Create('d'));
  AssertEquals('world','package b:a@1.1.1;'+sLineBreak+'world d {}'+sLineBreak,Element.ToString);
  lPackage.IsNested:=True;
  AssertEquals('world nested','package b:a@1.1.1 {'+sLineBreak+'world d {}'+sLineBreak+'}'+sLineBreak,Element.ToString);
  lPackage.Worlds.Clear;
  AssertEquals('nested','package b:a@1.1.1 {'+sLineBreak+'}'+sLineBreak,Element.ToString);
end;

procedure TTestWITModel.TestUseToString;
var
  lUse : TWITTopLevelUse;
begin
  lUse:=TWITTopLevelUse.Create;
  Element:=lUse;
  lUse.Path.PackageName:='a';
  AssertEquals('Name','use a;',Element.ToString);
  lUse.Path.Version:='1.1.1';
  AssertEquals('Name@ver','use a@1.1.1;',Element.ToString);
  lUse.Path.Namespaces.Add('b');
  AssertEquals('Namespace:Name@ver','use b:a@1.1.1;',Element.ToString);
  lUse.Path.Namespaces.Add('c');
  AssertEquals('double namespace','use b:c:a@1.1.1;',Element.ToString);
  lUse.Rename:='d';
  AssertEquals('alias','use b:c:a@1.1.1 as d;',Element.ToString);
end;

procedure TTestWITModel.TestInterfaceToString;

var
  lIntf : TWITInterface;
  lUse : TWITUse;

begin
  lIntf:=TWITInterface.Create('a');
  Element:=lIntf;
  AssertEquals('Name','interface a {}',Element.ToString);
  lIntf.AddType(TWITTypeDef.Create('b',TWitType.Create(wtu8)));
  AssertEquals('Type',  'interface a {'+sLinebreak
                                    +'  type b = u8;'+sLinebreak
                                    +'}',Element.ToString);
  lIntf.AddType(TWITTypeDef.Create('c',TWitType.Create(wtu32)));
  AssertEquals('Two Types',  'interface a {'+sLinebreak
                                    +'  type b = u8;'+sLinebreak
                                    +'  type c = u32;'+sLinebreak
                                    +'}',Element.ToString);
  lIntf.AddFunction(TWITFunction.Create('d',TWitFunctionType.Create));
  AssertEquals('Type & func',  'interface a {'+sLinebreak
                                    +'  type b = u8;'+sLinebreak
                                    +'  type c = u32;'+sLinebreak
                                    +'  d : func();'+sLinebreak
                                    +'}',Element.ToString);
  lUse:=TWITUse.Create;
  lUse.Path.PackageName:='e';
  LUse.AddItem('f');
  lIntf.AddUses(lUse);
  AssertEquals('Use, types and func',  'interface a {'+sLinebreak
                                    +'  use e.{f};'+sLinebreak
                                    +'  type b = u8;'+sLinebreak
                                    +'  type c = u32;'+sLinebreak
                                    +'  d : func();'+sLinebreak
                                    +'}',Element.ToString);

end;

procedure TTestWITModel.TestExchangeIdentifier;

var
  lEx : TWITExchangeIdentifier;

begin
  lEx:=TWITExchangeIdentifier.Create(xtImport,'a');
  Element:=lEx;
  AssertEquals('Import simple path','import a;',Element.ToString);
  lEx.Free;
  lEx:=TWITExchangeIdentifier.Create(xtExport,'a');
  Element:=lEx;
  AssertEquals('Export simple path','export a;',Element.ToString)
end;

procedure TTestWITModel.TestIncludeToString;
var
  lInc : TWITInclude;
  lItm : TWitIncludeItem;

begin
  lInc:=TWITInclude.Create;
  linc.Path.Identifier:='a';
  Element:=lInc;
  AssertEquals('Include simple path','include a;',Element.ToString);
  LItm:=TWitIncludeItem.Create('b');
  lInc.Items.Add(LItm);
  AssertEquals('Include item path','include a with {b}',Element.ToString)
end;


procedure TTestWITModel.TestWorldToString;
var
  lWorld : TWITWorld;
  lUse : TWITUse;
  lInclude : TWITInclude;

begin
  lWorld:=TWITWorld.Create('a');
  Element:=lWorld;
  AssertEquals('Name','world a {}',Element.ToString);
  lWorld.AddImport(TWITExchangeIdentifier.Create(xtImport,'b'));
  AssertEquals('Import',  'world a {'+sLinebreak
                                    +'  import b;'+sLinebreak
                                    +'}',Element.ToString);
  lWorld.AddExport(TWITExchangeIdentifier.Create(xtExport,'c'));
  AssertEquals('Import/export',  'world a {'+sLinebreak
                                    +'  import b;'+sLinebreak
                                    +'  export c;'+sLinebreak
                                    +'}',Element.ToString);
  lWorld.AddTypeDef(TWITTypeDef.Create('d',TWitType.Create(wtU8)));
  AssertEquals('Type, Import, Export',  'world a {'+sLinebreak
                                    +'  type d = u8;'+sLinebreak
                                    +'  import b;'+sLinebreak
                                    +'  export c;'+sLinebreak
                                    +'}',Element.ToString);
  lUse:=TWITUse.Create;
  lUse.Path.PackageName:='e';
  lUse.AddItem('f','g');
  lWorld.AddUses(lUse);
  AssertEquals('Use, Type, Import, Export',  'world a {'+sLinebreak
                                    +'  use e.{f as g};'+sLinebreak
                                    +'  type d = u8;'+sLinebreak
                                    +'  import b;'+sLinebreak
                                    +'  export c;'+sLinebreak
                                    +'}',Element.ToString);
  lInclude:=TWITInclude.Create;
  lInclude.Path.PackageName:='h';
  lWorld.AddINclude(lInclude);
  AssertEquals('include, Use, Type, Import, Export',  'world a {'+sLinebreak
                                    +'  include h;'+sLinebreak
                                    +'  use e.{f as g};'+sLinebreak
                                    +'  type d = u8;'+sLinebreak
                                    +'  import b;'+sLinebreak
                                    +'  export c;'+sLinebreak
                                    +'}',Element.ToString);

end;


procedure TTestWITModel.TestFunctionTypeToString;

var
  lFunc : TWITFunctionType;

begin
  lFunc:=TWITFunctionType.Create;
  Element:=LFunc;
  AssertEquals('Empty','func()',Element.ToString);
  lFunc.Flags:=[ffConstructor];
  AssertEquals('constructor','constructor()',Element.ToString);
  lFunc.Flags:=[ffAsync];
  AssertEquals('Async','async func()',Element.ToString);
  lFunc.Flags:=[ffstatic];
  AssertEquals('Static','static func()',Element.ToString);
  lFunc.Flags:=[];
  lFunc.ResultType:=TWitType.Create(wts8);
  AssertEquals('Res','func() -> s8',Element.ToString);
  lFunc.Parameters.Add(TWitFuncParam.Create('b',TWITType.Create(wtu8)));
  AssertEquals('Res','func(b: u8) -> s8',Element.ToString);
  lFunc.Parameters.Add(TWitFuncParam.Create('c',TWITType.Create(wtu32)));
  AssertEquals('Res','func(b: u8, c: u32) -> s8',Element.ToString);
end;

procedure TTestWITModel.TestFunctionToString;

var
  lFuncTyp : TWITFunctionType;
  lFunc : TWITFunction;

begin
  lFuncTyp:=TWITFunctionType.Create;
  lFunc:=TWITFunction.Create('a',lFuncTyp);
  Element:=LFunc;
  AssertEquals('Empty','a : func();',Element.ToString);
end;

procedure TTestWITModel.TestWITTypeToString;
var
  lType : TWITType;
begin
  lType:=TWITType.Create(wtu32);
  Element:=lType;
  AssertEquals('Type','u32',lType.ToString);
end;

procedure TTestWITModel.TestTypeDefToString;
var
  lType : TWITTypeDef;
begin
  lType:=TWITTypeDef.Create('a',TWITType.Create(wtu32));
  Element:=lType;
  AssertEquals('Type','type a = u32;',lType.ToString);
end;

procedure TTestWITModel.TestListTypeToString;
var
  lType : TWITListType;
begin
  lType:=TWITListType.Create(TWITType.Create(wtu32));
  Element:=lType;
  AssertEquals('List type','list<u32>',lType.ToString);
  lType.ItemCount:=3;
  AssertEquals('List type','list<u32,3>',lType.ToString);
end;

procedure TTestWITModel.TestTupleTypeToString;
var
  lType : TWITTupleType;
begin
  lType:=TWITTupleType.Create;
  lType.AddItem(TWITType.Create(wtu32));
  Element:=lType;
  AssertEquals('Tuple type','tuple<u32>',lType.ToString);
  lType.AddItem(TWITType.Create(wtu8));
  AssertEquals('Tuple type 2','tuple<u32,u8>',lType.ToString);
end;

procedure TTestWITModel.TestOptionTypeToString;
var
  lType : TWITOptionType;
begin
  lType:=TWITOptionType.Create(TWITType.Create(wtu32));
  Element:=lType;
  AssertEquals('Option type','option<u32>',lType.ToString);
end;

procedure TTestWITModel.TestResultTypeToString;
var
  lType : TWITResultType;
begin
  lType:=TWITResultType.Create(TWITType.Create(wtu32),TWITType.Create(wtString));
  Element:=lType;
  AssertEquals('Result type','result<u32,string>',lType.ToString);
end;

procedure TTestWITModel.TestHandleTypeToString;
var
  lType : TWITHandleType;
begin
  lType:=TWITHandleType.Create('a',true);
  Element:=lType;
  AssertEquals('Handle type','borrow<a>',lType.ToString);
  lType.Free;
  lType:=TWITHandleType.Create('a',False);
  Element:=lType;
   AssertEquals('Handle type','own<a>',lType.ToString);
end;

procedure TTestWITModel.TestFutureTypeToString;
var
  lType : TWITFutureType;
begin
  lType:=TWITFutureType.Create(TWITType.Create(wtu32));
  Element:=lType;
  AssertEquals('future type','future<u32>',lType.ToString);
end;

procedure TTestWITModel.TestStreamTypeToString;
var
  lType : TWITStreamType;
begin
  lType:=TWITStreamType.Create(TWITType.Create(wtu32));
  Element:=lType;
  AssertEquals('Stream type','stream<u32>',lType.ToString);
end;

procedure TTestWITModel.TestRecordFieldToString;

var
  lField : TWITRecordField;

begin
  lField:=TWITRecordField.Create('fld',TWITType.Create(wtU64));
  Element:=lField;
  AssertEquals('Field','fld : u64',lField.ToString)
end;

procedure TTestWITModel.TestRecordToString;

var
  lRecord : TWITRecordType;

begin
  lRecord:=TWITRecordType.Create;
  Element:=lRecord;
  AssertEquals('Empty','record  {}',lRecord.ToString);
  lRecord.AddField(TWITRecordField.Create('fld',TWITType.Create(wtU64)));
  AssertEquals('One field','record  {'+sLineBreak
                                    +'  fld : u64'+sLineBreak
                                    +'}',lRecord.ToString);
  lRecord.AddField(TWITRecordField.Create('fld2',TWITType.Create(wtString)));
  AssertEquals('Two fields','record  {'+sLineBreak
                           +'  fld : u64,'+sLineBreak
                           +'  fld2 : string'+sLineBreak
                           +'}',lRecord.ToString);
  Element:=TWITTypeDef.Create('a',lRecord);
  AssertEquals('Typedef','record a {'+sLineBreak
                           +'  fld : u64,'+sLineBreak
                           +'  fld2 : string'+sLineBreak
                           +'}',Element.ToString);
end;


procedure TTestWITModel.TestEnumToString;
var
  lEnum : TWITEnumType;

begin
  lEnum:=TWITEnumType.Create;
  Element:=lEnum;
  AssertEquals('Empty','enum  {}',lEnum.ToString);
  lEnum.AddCase('one');
  AssertEquals('One case','enum  {'+sLineBreak
                                    +'  one'+sLineBreak
                                    +'}',lEnum.ToString);
  lEnum.AddCase('two');
  AssertEquals('Two cases','enum  {'+sLineBreak
                           +'  one,'+sLineBreak
                           +'  two'+sLineBreak
                           +'}',lEnum.ToString);
  Element:=TWITTypeDef.Create('a',lEnum);
  AssertEquals('Typedef','enum a {'+sLineBreak
                           +'  one,'+sLineBreak
                           +'  two'+sLineBreak
                           +'}',Element.ToString);
end;

procedure TTestWITModel.TestFlagsToString;
var
  lFlags : TWITFlagsType;

begin
  lFlags:=TWITFlagsType.Create;
  Element:=lFlags;
  AssertEquals('Empty','flags  {}',lFlags.ToString);
  lFlags.AddFlag('one');
  AssertEquals('One flag','flags  {'+sLineBreak
                                    +'  one'+sLineBreak
                                    +'}',lFlags.ToString);
  lFlags.AddFlag('two');
  AssertEquals('Two cases','flags  {'+sLineBreak
                           +'  one,'+sLineBreak
                           +'  two'+sLineBreak
                           +'}',lFlags.ToString);
  Element:=TWITTypeDef.Create('a',lFlags);
  AssertEquals('Typedef','flags a {'+sLineBreak
                           +'  one,'+sLineBreak
                           +'  two'+sLineBreak
                           +'}',Element.ToString);
end;

procedure TTestWITModel.TestVariantToString;
var
  lVar : TWITVariantType;

begin
  lVar:=TWITVariantType.Create;
  Element:=lVar;
  AssertEquals('Empty','variant  {}',Element.ToString);
  lVar.AddCase(TWitVariantCase.Create('one'));
  AssertEquals('One flag','variant  {'+sLineBreak
                                    +'  one'+sLineBreak
                                    +'}',Element.ToString);
  lVar.AddCase(TWitVariantCase.Create('two'));
  AssertEquals('Two cases','variant  {'+sLineBreak
                           +'  one,'+sLineBreak
                           +'  two'+sLineBreak
                           +'}',Element.ToString);
  lVar.AddCase(TWitVariantCase.Create('three',TWitType.Create(wtu8)));
  AssertEquals('Two cases','variant  {'+sLineBreak
                           +'  one,'+sLineBreak
                           +'  two,'+sLineBreak
                           +'  three(u8)'+sLineBreak
                           +'}',Element.ToString);
  Element:=TWITTypeDef.Create('a',lVar);
  AssertEquals('Typedef','variant a {'+sLineBreak
                           +'  one,'+sLineBreak
                           +'  two,'+sLineBreak
                           +'  three(u8)'+sLineBreak
                           +'}',Element.ToString);
end;

procedure TTestWITModel.TestResourceToString;
var
  lRes : TWITResourceType;
  lFunc : TWITFunction;
  lFuncType : TWITFunctionType;
begin
  lRes:=TWITResourceType.Create('a');
  Element:=lRes;
  AssertEquals('empty','resource a;',Element.ToString);
  lFuncType:=TWITFunctionType.Create;
  lFuncType.Flags:=[ffConstructor];
  lFunc:=TWITFunction.Create('constructor',lFuncType);
  lRes.Functions.Add(lFunc);
  AssertEquals('empty','resource a {'+sLineBreak
                      +'  constructor();'+sLineBreak
                      +'}',Element.ToString);
  lFuncType:=TWITFunctionType.Create;
  lFuncType.ResultType:=TWitType.Create(wtU8);
  lFunc:=TWITFunction.Create('b',lFuncType);
  lRes.Functions.Add(lFunc);
  AssertEquals('empty','resource a {'+sLineBreak
                      +'  constructor();'+sLineBreak
                      +'  b : func() -> u8;'+sLineBreak
                      +'}',Element.ToString);
end;

initialization
  RegisterTest(TTestWITModel);
end.

