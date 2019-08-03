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
    Procedure TestGeneric_RecordEmpty;
    Procedure TestGeneric_ClassEmpty;
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

Initialization
  RegisterTests([TTestGenerics]);
end.

