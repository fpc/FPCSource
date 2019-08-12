unit TCGenerics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  TCModules;

type

  { TTestGenerics }

  TTestGenerics = class(TCustomTestModule)
  Published
    // generic record
    Procedure TestGeneric_RecordEmpty;

    // generic class
    Procedure TestGeneric_ClassEmpty;
    Procedure TestGeneric_Class_EmptyMethod;

    // generic external class
    procedure TestGen_ExtClass_Array;
  end;

implementation

{ TTestGenerics }

procedure TTestGenerics.TestGeneric_RecordEmpty;
begin
  StartProgram(false);
  Add([
  'type',
  '  generic TRecA<T> = record',
  '  end;',
  'var a,b: specialize TRecA<word>;',
  'begin',
  '  if a=b then ;']);
  ConvertProgram;
  CheckSource('TestGeneric_RecordEmpty',
    LinesToStr([ // statements
    'rtl.recNewT($mod, "TRecA$G1", function () {',
    '  this.$eq = function (b) {',
    '    return true;',
    '  };',
    '  this.$assign = function (s) {',
    '    return this;',
    '  };',
    '});',
    'this.a = $mod.TRecA$G1.$new();',
    'this.b = $mod.TRecA$G1.$new();',
    '']),
    LinesToStr([ // $mod.$main
    'if ($mod.a.$eq($mod.b)) ;'
    ]));
end;

procedure TTestGenerics.TestGeneric_ClassEmpty;
begin
  StartProgram(false);
  Add([
  'type',
  '  TObject = class end;',
  '  generic TBird<T> = class',
  '  end;',
  'var a,b: specialize TBird<word>;',
  'begin',
  '  if a=b then ;']);
  ConvertProgram;
  CheckSource('TestGeneric_ClassEmpty',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'rtl.createClass($mod, "TBird$G1", $mod.TObject, function () {',
    '});',
    'this.a = null;',
    'this.b = null;',
    '']),
    LinesToStr([ // $mod.$main
    'if ($mod.a === $mod.b) ;'
    ]));
end;

procedure TTestGenerics.TestGeneric_Class_EmptyMethod;
begin
  StartProgram(false);
  Add([
  'type',
  '  TObject = class end;',
  '  generic TBird<T> = class',
  '    function Fly(w: T): T;',
  '  end;',
  'function TBird.Fly(w: T): T;',
  'begin',
  'end;',
  'var a: specialize TBird<word>;',
  'begin',
  '  if a.Fly(3)=4 then ;']);
  ConvertProgram;
  CheckSource('TestGeneric_Class_EmptyMethod',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'rtl.createClass($mod, "TBird$G1", $mod.TObject, function () {',
    '  this.Fly = function (w) {',
    '    var Result = 0;',
    '    return Result;',
    '  };',
    '});',
    'this.a = null;',
    '']),
    LinesToStr([ // $mod.$main
    '  if ($mod.a.Fly(3) === 4) ;'
    ]));
end;

procedure TTestGenerics.TestGen_ExtClass_Array;
begin
  StartProgram(false);
  Add([
  '{$mode delphi}',
  '{$ModeSwitch externalclass}',
  'type',
  '  NativeInt = longint;',
  '  TJSGenArray<T> = Class external name ''Array''',
  '  private',
  '    function GetElements(Index: NativeInt): T; external name ''[]'';',
  '    procedure SetElements(Index: NativeInt; const AValue: T); external name ''[]'';',
  '  public',
  '    type TSelfType = TJSGenArray<T>;',
  '  public',
  '    FLength : NativeInt; external name ''length'';',
  '    constructor new; overload;',
  '    constructor new(aLength : NativeInt); overload;',
  '    class function _of() : TSelfType; varargs; external name ''of'';',
  '    function fill(aValue : T) : TSelfType; overload;',
  '    function fill(aValue : T; aStartIndex : NativeInt) : TSelfType; overload;',
  '    function fill(aValue : T; aStartIndex,aEndIndex : NativeInt) : TSelfType; overload;',
  '    property Length : NativeInt Read FLength Write FLength;',
  '    property Elements[Index: NativeInt]: T read GetElements write SetElements; default;',
  '  end;',
  '  TJSWordArray = TJSGenArray<word>;',
  'var',
  '  wa: TJSWordArray;',
  '  w: word;',
  'begin',
  '  wa:=TJSWordArray.new;',
  '  wa:=TJSWordArray.new(3);',
  '  wa:=TJSWordArray._of(4,5);',
  '  wa:=wa.fill(7);',
  '  wa:=wa.fill(7,8,9);',
  '  w:=wa.length;',
  '  wa.length:=10;',
  '  wa[11]:=w;',
  '  w:=wa[12];',
  '']);
  ConvertProgram;
  CheckSource('TestGen_ExtClass_Array',
    LinesToStr([ // statements
    'this.wa = null;',
    'this.w = 0;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.wa = new Array();',
    '$mod.wa = new Array(3);',
    '$mod.wa = Array.of(4, 5);',
    '$mod.wa = $mod.wa.fill(7);',
    '$mod.wa = $mod.wa.fill(7, 8, 9);',
    '$mod.w = $mod.wa.length;',
    '$mod.wa.length = 10;',
    '$mod.wa[11] = $mod.w;',
    '$mod.w = $mod.wa[12];',
    '']));
end;

Initialization
  RegisterTests([TTestGenerics]);
end.

