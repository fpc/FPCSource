unit TCOperatorOverload;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  TCModules, FPPas2Js, PScanner, PasResolveEval;

type

  { TTestOperatorOverload }

  TTestOperatorOverload = class(TCustomTestModule)
  published
    procedure TestOpOv_RecordAdd; // todo
  end;

implementation

{ TTestOperatorOverload }

procedure TTestOperatorOverload.TestOpOv_RecordAdd;
begin
  exit;

  StartProgram(false);
  Add([
  'type',
  '  TRec = record X: word; end;',
  'operator +(A, B: TRec): TRec;',
  'begin',
  '  Result.X := A.X + B.X;',
  'end;',
  'var a,b,c: TRec;',
  'begin',
  '  c:=a+b;']);
  ConvertProgram;
  CheckSource('TestOpOv_RecordAdd',
    LinesToStr([ // statements
    'rtl.recNewT(this, "TRec", function () {',
    '  this.$eq = function (b) {',
    '    return this.X === b.X;',
    '  };',
    '  this.$assign = function (s) {',
    '    this.X = s.X;',
    '    return this;',
    '  };',
    '});',
    'this.a = this.TRec.$new();',
    'this.b = this.TRec.$new();',
    'this.c = this.TRec.$new();',
    '']),
    LinesToStr([ // $mod.$main
    ''
    ]));
end;

Initialization
  RegisterTests([TTestOperatorOverload]);
end.

