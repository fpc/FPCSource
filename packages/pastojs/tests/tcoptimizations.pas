{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2017 by Michael Van Canneyt

    Unit tests for Pascal-to-Javascript converter class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

 Examples:
   ./testpas2js --suite=TTestOptimizations
   ./testpas2js --suite=TTestOptimizations.TestOmitLocalVar
}
unit tcoptimizations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, fppas2js, pastree,
  PScanner, PasUseAnalyzer, PasResolver, PasResolveEval,
  tcmodules;

type


  { TCustomTestOptimizations }

  TCustomTestOptimizations = class(TCustomTestModule)
  private
    FAnalyzerModule: TPasAnalyzer;
    FAnalyzerProgram: TPasAnalyzer;
    FWholeProgramOptimization: boolean;
    function OnConverterIsElementUsed(Sender: TObject; El: TPasElement): boolean;
    function OnConverterIsTypeInfoUsed(Sender: TObject; El: TPasElement): boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure ParseModule; override;
    procedure ParseProgram; override;
  public
    property AnalyzerModule: TPasAnalyzer read FAnalyzerModule;
    property AnalyzerProgram: TPasAnalyzer read FAnalyzerProgram;
    property WholeProgramOptimization: boolean read FWholeProgramOptimization
        write FWholeProgramOptimization;
  end;

  { TTestOptimizations }

  TTestOptimizations = class(TCustomTestOptimizations)
  published
    // Whole Program Optimization
    procedure TestWPO_OmitLocalVar;
    procedure TestWPO_OmitLocalProc;
    procedure TestWPO_OmitLocalProcForward;
    procedure TestWPO_OmitProcLocalVar;
    procedure TestWPO_OmitProcLocalConst;
    procedure TestWPO_OmitProcLocalType;
    procedure TestWPO_OmitProcLocalProc;
    procedure TestWPO_OmitProcLocalForwardProc;
    procedure TestWPO_OmitRecordMember;
    procedure TestWPO_OmitNotUsedTObject;
    procedure TestWPO_TObject;
    procedure TestWPO_OmitClassField;
    procedure TestWPO_OmitClassMethod;
    procedure TestWPO_OmitClassClassMethod;
    procedure TestWPO_OmitPropertyGetter1;
    procedure TestWPO_OmitPropertyGetter2;
    procedure TestWPO_OmitPropertySetter1;
    procedure TestWPO_OmitPropertySetter2;
    procedure TestWPO_CallInherited;
    procedure TestWPO_UseUnit;
    procedure TestWPO_ProgramPublicDeclaration;
    procedure TestWPO_RTTI_PublishedField;
    procedure TestWPO_RTTI_TypeInfo;
  end;

implementation

{ TCustomTestOptimizations }

function TCustomTestOptimizations.OnConverterIsElementUsed(Sender: TObject;
  El: TPasElement): boolean;
var
  A: TPasAnalyzer;
begin
  if WholeProgramOptimization then
    A:=AnalyzerProgram
  else
    A:=AnalyzerModule;
  Result:=A.IsUsed(El);
  {$IF defined(VerbosePas2JS) or defined(VerbosePasAnalyzer)}
  writeln('TCustomTestOptimizations.OnConverterIsElementUsed El=',GetObjName(El),' WPO=',WholeProgramOptimization,' Result=',Result);
  {$ENDIF}
end;

function TCustomTestOptimizations.OnConverterIsTypeInfoUsed(Sender: TObject;
  El: TPasElement): boolean;
var
  A: TPasAnalyzer;
begin
  if WholeProgramOptimization then
    A:=AnalyzerProgram
  else
    A:=AnalyzerModule;
  Result:=A.IsTypeInfoUsed(El);
  {$IF defined(VerbosePas2JS) or defined(VerbosePasAnalyzer)}
  writeln('TCustomTestOptimizations.OnConverterIsTypeInfoUsed El=',GetObjName(El),' WPO=',WholeProgramOptimization,' Result=',Result);
  {$ENDIF}
end;

procedure TCustomTestOptimizations.SetUp;
begin
  inherited SetUp;
  FWholeProgramOptimization:=false;
  FAnalyzerModule:=TPasAnalyzer.Create;
  FAnalyzerModule.Resolver:=Engine;
  FAnalyzerProgram:=TPasAnalyzer.Create;
  FAnalyzerProgram.Resolver:=Engine;
  Converter.OnIsElementUsed:=@OnConverterIsElementUsed;
  Converter.OnIsTypeInfoUsed:=@OnConverterIsTypeInfoUsed;
end;

procedure TCustomTestOptimizations.TearDown;
begin
  FreeAndNil(FAnalyzerProgram);
  FreeAndNil(FAnalyzerModule);
  inherited TearDown;
end;

procedure TCustomTestOptimizations.ParseModule;
begin
  inherited ParseModule;
  {$IF defined(VerbosePas2JS) or defined(VerbosePasAnalyzer)}
  writeln('TCustomTestOptimizations.ParseModule START');
  {$ENDIF}
  AnalyzerModule.AnalyzeModule(Module);
  {$IF defined(VerbosePas2JS) or defined(VerbosePasAnalyzer)}
  writeln('TCustomTestOptimizations.ParseModule END');
  {$ENDIF}
end;

procedure TCustomTestOptimizations.ParseProgram;
begin
  WholeProgramOptimization:=true;
  inherited ParseProgram;
  {$IF defined(VerbosePas2JS) or defined(VerbosePasAnalyzer)}
  writeln('TCustomTestOptimizations.ParseProgram START');
  {$ENDIF}
  AnalyzerProgram.AnalyzeWholeProgram(Module as TPasProgram);
  {$IF defined(VerbosePas2JS) or defined(VerbosePasAnalyzer)}
  writeln('TCustomTestOptimizations.ParseProgram START');
  {$ENDIF}
end;

{ TTestOptimizations }

procedure TTestOptimizations.TestWPO_OmitLocalVar;
begin
  StartProgram(false);
  Add('var');
  Add('  a: longint;');
  Add('  b: longint;');
  Add('begin');
  Add('  b:=3;');
  ConvertProgram;
  CheckSource('TestWPO_OmitLocalVar',
    'this.b = 0;',
    '$mod.b = 3;');
end;

procedure TTestOptimizations.TestWPO_OmitLocalProc;
begin
  StartProgram(false);
  Add('procedure DoIt; begin end;');
  Add('procedure NoIt; begin end;');
  Add('begin');
  Add('  DoIt;');
  ConvertProgram;
  CheckSource('TestWPO_OmitLocalProc',
    LinesToStr([
    'this.DoIt = function () {',
    '};',
    '']),
    LinesToStr([
    '$mod.DoIt();',
    '']));
end;

procedure TTestOptimizations.TestWPO_OmitLocalProcForward;
begin
  StartProgram(false);
  Add('procedure DoIt; forward;');
  Add('procedure NoIt; forward;');
  Add('procedure DoIt; begin end;');
  Add('procedure NoIt; begin end;');
  Add('begin');
  Add('  DoIt;');
  ConvertProgram;
  CheckSource('TestWPO_OmitLocalProcForward',
    LinesToStr([
    'this.DoIt = function () {',
    '};',
    '']),
    LinesToStr([
    '$mod.DoIt();',
    '']));
end;

procedure TTestOptimizations.TestWPO_OmitProcLocalVar;
begin
  StartProgram(false);
  Add('function DoIt: longint;');
  Add('var');
  Add('  a: longint;');
  Add('  b: longint;');
  Add('begin');
  Add('  b:=3;');
  Add('  Result:=b;');
  Add('end;');
  Add('begin');
  Add('  DoIt;');
  ConvertProgram;
  CheckSource('TestWPO_OmitProcLocalVar',
    LinesToStr([
    'this.DoIt = function () {',
    '  var Result = 0;',
    '  var b = 0;',
    '  b = 3;',
    '  Result = b;',
    '  return Result;',
    '};',
    '']),
    LinesToStr([
    '$mod.DoIt();',
    '']));
end;

procedure TTestOptimizations.TestWPO_OmitProcLocalConst;
begin
  StartProgram(false);
  Add('function DoIt: longint;');
  Add('const');
  Add('  a = 3;');
  Add('  b = 4;');
  Add('  c: longint = 5;');
  Add('  d: longint = 6;');
  Add('begin');
  Add('  Result:=b+d;');
  Add('end;');
  Add('begin');
  Add('  DoIt;');
  ConvertProgram;
  CheckSource('TestWPO_OmitProcLocalConst',
    LinesToStr([
    'var b = 4;',
    'var d = 6;',
    'this.DoIt = function () {',
    '  var Result = 0;',
    '  Result = b + d;',
    '  return Result;',
    '};',
    '']),
    LinesToStr([
    '$mod.DoIt();',
    '']));
end;

procedure TTestOptimizations.TestWPO_OmitProcLocalType;
begin
  StartProgram(false);
  Add('function DoIt: longint;');
  Add('type');
  Add('  TEnum = (red, green);');
  Add('  TEnums = set of TEnum;');
  Add('begin');
  Add('  Result:=3;');
  Add('end;');
  Add('begin');
  Add('  DoIt;');
  ConvertProgram;
  CheckSource('TestWPO_OmitProcLocalType',
    LinesToStr([
    'this.DoIt = function () {',
    '  var Result = 0;',
    '  Result = 3;',
    '  return Result;',
    '};',
    '']),
    LinesToStr([
    '$mod.DoIt();',
    '']));
end;

procedure TTestOptimizations.TestWPO_OmitProcLocalProc;
begin
  StartProgram(false);
  Add('procedure DoIt;');
  Add('  procedure SubProcA; begin end;');
  Add('  procedure SubProcB; begin end;');
  Add('begin');
  Add('  SubProcB;');
  Add('end;');
  Add('begin');
  Add('  DoIt;');
  ConvertProgram;
  CheckSource('TestWPO_OmitProcLocalProc',
    LinesToStr([
    'this.DoIt = function () {',
    '  function SubProcB() {',
    '  };',
    '  SubProcB();',
    '};',
    '']),
    LinesToStr([
    '$mod.DoIt();',
    '']));
end;

procedure TTestOptimizations.TestWPO_OmitProcLocalForwardProc;
begin
  StartProgram(false);
  Add('procedure DoIt;');
  Add('  procedure SubProcA; forward;');
  Add('  procedure SubProcB; forward;');
  Add('  procedure SubProcA; begin end;');
  Add('  procedure SubProcB; begin end;');
  Add('begin');
  Add('  SubProcB;');
  Add('end;');
  Add('begin');
  Add('  DoIt;');
  ConvertProgram;
  CheckSource('TestWPO_OmitProcLocalForwardProc',
    LinesToStr([
    'this.DoIt = function () {',
    '  function SubProcB() {',
    '  };',
    '  SubProcB();',
    '};',
    '']),
    LinesToStr([
    '$mod.DoIt();',
    '']));
end;

procedure TTestOptimizations.TestWPO_OmitRecordMember;
begin
  StartProgram(false);
  Add('type');
  Add('  TRec = record');
  Add('    a: longint;');
  Add('    b: longint;');
  Add('  end;');
  Add('var r: TRec;');
  Add('begin');
  Add('  r.a:=3;');
  ConvertProgram;
  CheckSource('TestWPO_OmitRecordMember',
    LinesToStr([
    'this.TRec = function (s) {',
    '  if (s) {',
    '    this.a = s.a;',
    '  } else {',
    '    this.a = 0;',
    '  };',
    '  this.$equal = function (b) {',
    '    return this.a == b.a;',
    '  };',
    '};',
    'this.r = new $mod.TRec();',
    '']),
    LinesToStr([
    '$mod.r.a = 3;',
    '']));
end;

procedure TTestOptimizations.TestWPO_OmitNotUsedTObject;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class end;');
  Add('var o: TObject;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestWPO_OmitNotUsedTObject',
    LinesToStr([
    '']),
    LinesToStr([
    '']));
end;

procedure TTestOptimizations.TestWPO_TObject;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure AfterConstruction; virtual;');
  Add('    procedure BeforeDestruction; virtual;');
  Add('  end;');
  Add('procedure TObject.AfterConstruction; begin end;');
  Add('procedure TObject.BeforeDestruction; begin end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  o:=nil;');
  ConvertProgram;
  CheckSource('TestWPO_TObject',
    LinesToStr([
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.AfterConstruction = function () {',
    '  };',
    '  this.BeforeDestruction = function () {',
    '  };',
    '});',
    'this.o = null;',
    '']),
    LinesToStr([
    '$mod.o = null;']));
end;

procedure TTestOptimizations.TestWPO_OmitClassField;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    a: longint;');
  Add('    b: longint;');
  Add('  end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  o.a:=3;');
  ConvertProgram;
  CheckSource('TestWPO_OmitClassField',
    LinesToStr([
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.a = 0;',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'this.o = null;',
    '']),
    LinesToStr([
    '$mod.o.a = 3;']));
end;

procedure TTestOptimizations.TestWPO_OmitClassMethod;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure ProcA;');
  Add('    procedure ProcB;');
  Add('  end;');
  Add('procedure TObject.ProcA; begin end;');
  Add('procedure TObject.ProcB; begin end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  o.ProcB;');
  ConvertProgram;
  CheckSource('TestWPO_OmitClassMethod',
    LinesToStr([
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.ProcB = function () {',
    '  };',
    '});',
    'this.o = null;',
    '']),
    LinesToStr([
    '$mod.o.ProcB();']));
end;

procedure TTestOptimizations.TestWPO_OmitClassClassMethod;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    class procedure ProcA;');
  Add('    class procedure ProcB;');
  Add('  end;');
  Add('class procedure TObject.ProcA; begin end;');
  Add('class procedure TObject.ProcB; begin end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  o.ProcB;');
  ConvertProgram;
  CheckSource('TestWPO_OmitClassMethod',
    LinesToStr([
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.ProcB = function () {',
    '  };',
    '});',
    'this.o = null;',
    '']),
    LinesToStr([
    '$mod.o.$class.ProcB();']));
end;

procedure TTestOptimizations.TestWPO_OmitPropertyGetter1;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FFoo: boolean;');
  Add('    function GetFoo: boolean;');
  Add('    property Foo: boolean read FFoo;');
  Add('    property Foo2: boolean read GetFoo;');
  Add('    FBar: boolean;');
  Add('    function GetBar: boolean;');
  Add('    property Bar: boolean read FBar;');
  Add('    property Bar2: boolean read GetBar;');
  Add('  end;');
  Add('function TObject.GetFoo: boolean; begin Result:=FFoo; end;');
  Add('function TObject.GetBar: boolean; begin Result:=FBar; end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  if o.Foo then;');
  ConvertProgram;
  CheckSource('TestWPO_OmitClassPropertyGetter1',
    LinesToStr([
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.FFoo = false;',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'this.o = null;',
    '']),
    LinesToStr([
    'if ($mod.o.FFoo);',
    '']));
end;

procedure TTestOptimizations.TestWPO_OmitPropertyGetter2;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FFoo: boolean;');
  Add('    function GetFoo: boolean;');
  Add('    property Foo: boolean read FFoo;');
  Add('    property Foo2: boolean read GetFoo;');
  Add('  end;');
  Add('function TObject.GetFoo: boolean; begin Result:=FFoo; end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  if o.Foo2 then;');
  ConvertProgram;
  CheckSource('TestWPO_OmitClassPropertyGetter2',
    LinesToStr([
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.FFoo = false;',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.GetFoo = function () {',
    '    var Result = false;',
    '    Result = this.FFoo;',
    '    return Result;',
    '  };',
    '});',
    'this.o = null;',
    '']),
    LinesToStr([
    'if ($mod.o.GetFoo()) ;',
    '']));
end;

procedure TTestOptimizations.TestWPO_OmitPropertySetter1;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FFoo: boolean;');
  Add('    procedure SetFoo(Value: boolean);');
  Add('    property Foo: boolean write FFoo;');
  Add('    property Foo2: boolean write SetFoo;');
  Add('    FBar: boolean;');
  Add('    procedure SetBar(Value: boolean);');
  Add('    property Bar: boolean write FBar;');
  Add('    property Bar2: boolean write SetBar;');
  Add('  end;');
  Add('procedure TObject.SetFoo(Value: boolean); begin FFoo:=Value; end;');
  Add('procedure TObject.SetBar(Value: boolean); begin FBar:=Value; end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  o.Foo:=true;');
  ConvertProgram;
  CheckSource('TestWPO_OmitClassPropertySetter1',
    LinesToStr([
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.FFoo = false;',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'this.o = null;',
    '']),
    LinesToStr([
    '$mod.o.FFoo = true;',
    '']));
end;

procedure TTestOptimizations.TestWPO_OmitPropertySetter2;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FFoo: boolean;');
  Add('    procedure SetFoo(Value: boolean);');
  Add('    property Foo: boolean write FFoo;');
  Add('    property Foo2: boolean write SetFoo;');
  Add('  end;');
  Add('procedure TObject.SetFoo(Value: boolean); begin FFoo:=Value; end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  o.Foo2:=true;');
  ConvertProgram;
  CheckSource('TestWPO_OmitClassPropertySetter2',
    LinesToStr([
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.FFoo = false;',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.SetFoo = function (Value) {',
    '    this.FFoo = Value;',
    '  };',
    '});',
    'this.o = null;',
    '']),
    LinesToStr([
    '$mod.o.SetFoo(true);',
    '']));
end;

procedure TTestOptimizations.TestWPO_CallInherited;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure DoA;');
  Add('    procedure DoB;');
  Add('  end;');
  Add('  TMobile = class');
  Add('    procedure DoA;');
  Add('    procedure DoC;');
  Add('  end;');
  Add('procedure TObject.DoA; begin end;');
  Add('procedure TObject.DoB; begin end;');
  Add('procedure TMobile.DoA;');
  Add('begin');
  Add('  inherited;');
  Add('end;');
  Add('procedure TMobile.DoC;');
  Add('begin');
  Add('  inherited DoB;');
  Add('end;');
  Add('var o: TMobile;');
  Add('begin');
  Add('  o.DoA;');
  Add('  o.DoC;');
  ConvertProgram;
  CheckSource('TestWPO_CallInherited',
    LinesToStr([
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.DoA = function () {',
    '  };',
    '  this.DoB = function () {',
    '  };',
    '});',
    ' rtl.createClass($mod, "TMobile", $mod.TObject, function () {',
    '  this.DoA$1 = function () {',
    '    $mod.TObject.DoA.apply(this, arguments);',
    '  };',
    '  this.DoC = function () {',
    '    $mod.TObject.DoB.call(this);',
    '  };',
    '});',
    'this.o = null;',
    '']),
    LinesToStr([
    '$mod.o.DoA$1();',
    '$mod.o.DoC();',
    '']));
end;

procedure TTestOptimizations.TestWPO_UseUnit;
var
  ActualSrc, ExpectedSrc: String;
begin
  AddModuleWithIntfImplSrc('unit1.pp',
    LinesToStr([
    'var i: longint;',
    'procedure DoIt;',
    '']),
    LinesToStr([
    'procedure DoIt; begin end;']));

  AddModuleWithIntfImplSrc('unit2.pp',
    LinesToStr([
    'var j: longint;',
    'procedure DoMore;',
    '']),
    LinesToStr([
    'procedure DoMore; begin end;']));

  StartProgram(true);
  Add('uses unit2;');
  Add('begin');
  Add('  j:=3;');
  ConvertProgram;
  ActualSrc:=JSToStr(JSModule);
  ExpectedSrc:=LinesToStr([
    'rtl.module("program", ["system", "unit2"], function () {',
    '  var $mod = this;',
    '  $mod.$main = function () {',
    '    pas.unit2.j = 3;',
    '  };',
    '});',
    '']);
  CheckDiff('TestWPO_UseUnit',ExpectedSrc,ActualSrc);
end;

procedure TTestOptimizations.TestWPO_ProgramPublicDeclaration;
var
  ActualSrc, ExpectedSrc: String;
begin
  StartProgram(true);
  Add('var');
  Add('  vPublic: longint; public;');
  Add('  vPrivate: longint;');
  Add('procedure DoPublic; public; begin end;');
  Add('procedure DoPrivate; begin end;');
  Add('begin');
  ConvertProgram;
  ActualSrc:=JSToStr(JSModule);
  ExpectedSrc:=LinesToStr([
    'rtl.module("program", ["system"], function () {',
    '  var $mod = this;',
    '  this.vPublic = 0;',
    '  this.DoPublic =function(){',
    '  };',
    '  $mod.$main = function () {',
    '  };',
    '});',
    '']);
  CheckDiff('TestWPO_ProgramPublicDeclaration',ExpectedSrc,ActualSrc);
end;

procedure TTestOptimizations.TestWPO_RTTI_PublishedField;
var
  ActualSrc, ExpectedSrc: String;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(true);
  Add('type');
  Add('  TArrA = array of char;');
  Add('  TArrB = array of string;');
  Add('  TObject = class');
  Add('  public');
  Add('    PublicA: TArrA;');
  Add('  published');
  Add('    PublishedB: TArrB;');
  Add('  end;');
  Add('var');
  Add('  C: TObject;');
  Add('begin');
  Add('  C.PublicA:=nil;');
  ConvertProgram;
  ActualSrc:=JSToStr(JSModule);
  ExpectedSrc:=LinesToStr([
    'rtl.module("program", ["system"], function () {',
    '  var $mod = this;',
    '  $mod.$rtti.$DynArray("TArrB", {',
    '    eltype: rtl.string',
    '  });',
    '  rtl.createClass($mod, "TObject", null, function () {',
    '    this.$init = function () {',
    '      this.PublicA = [];',
    '      this.PublishedB = [];',
    '    };',
    '    this.$final = function () {',
    '      this.PublicA = undefined;',
    '      this.PublishedB = undefined;',
    '    };',
    '    var $r = this.$rtti;',
    '    $r.addField("PublishedB", $mod.$rtti["TArrB"]);',
    '  });',
    '  this.C = null;',
    '  $mod.$main = function () {',
    '    $mod.C.PublicA = [];',
    '  };',
    '});',
    '']);
  CheckDiff('TestWPO_RTTI_PublishedField',ExpectedSrc,ActualSrc);
end;

procedure TTestOptimizations.TestWPO_RTTI_TypeInfo;
var
  ActualSrc, ExpectedSrc: String;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(true);
  Add('type');
  Add('  TArrA = array of char;');
  Add('  TArrB = array of string;');
  Add('var');
  Add('  A: TArrA;');
  Add('  B: TArrB;');
  Add('  p: pointer;');
  Add('begin');
  Add('  A:=nil;');
  Add('  p:=typeinfo(B);');
  ConvertProgram;
  ActualSrc:=JSToStr(JSModule);
  ExpectedSrc:=LinesToStr([
    'rtl.module("program", ["system"], function () {',
    '  var $mod = this;',
    '  $mod.$rtti.$DynArray("TArrB", {',
    '    eltype: rtl.string',
    '  });',
    '  this.A = [];',
    '  this.B = [];',
    '  this.p = null;',
    '  $mod.$main = function () {',
    '    $mod.A = [];',
    '    $mod.p = $mod.$rtti["TArrB"];',
    '  };',
    '});',
    '']);
  CheckDiff('TestWPO_RTTI_TypeInfo',ExpectedSrc,ActualSrc);
end;

Initialization
  RegisterTests([TTestOptimizations]);
end.

