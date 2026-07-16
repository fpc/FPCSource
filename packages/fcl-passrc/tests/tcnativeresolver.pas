{
  Tests for TPasNativeResolver (pasnativeresolve) — the native-target resolver
  descendant.

  Two levels are covered:

  * Registration (TTestNativeResolver): that TPasNativeResolver registers the
    native-only const-eval intrinsics and type aliases and, crucially, that the
    base TPasResolver (used by pas2js) does NOT — the guarantee that the native
    extensions are opt-in by class and leave pas2js untouched. Inspected
    directly, no parse needed.

  * Const folding (TTestNativeResolverFold): that the registered intrinsics
    actually fold to the correct value when a program is parsed and resolved.
    This drives a full parse+resolve through the fcl-passrc test harness with a
    native engine plugged in via TCustomNativeTestResolver.

  * Element state flags (TTestElementStateFlags): the target-agnostic
    range/overflow-checked marking on TPasElement.States. This is base
    TPasResolver behaviour (not native-specific) — verified on the plain base
    engine — recorded here alongside the rest of the upstreaming work.
}
unit tcnativeresolver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PScanner, PParser, PasTree, PasResolveEval, PasResolver, pasnativeresolve,
  TCResolver;

type

  { TDummyPasResolver }

  TDummyPasResolver = class(TPasResolver)
  public
    function FindUnit(const AName, InFilename: String; NameExpr, InFileExpr: TPasExpr): TPasModule;
      override;
  end;

  { TDummyPasNativeResolver }

  TDummyPasNativeResolver = class(TPasNativeResolver)
  public
    function FindUnit(const AName, InFilename: String; NameExpr, InFileExpr: TPasExpr): TPasModule;
      override;
  end;

  { TTestNativeResolver }

  TTestNativeResolver = class(TTestCase)
  private
    function AliasBaseType(R: TPasResolver; const aName: string;
      out bt: TResolverBaseType): boolean;
  published
    procedure TestNativeRegistersConstEvalIntrinsics;
    procedure TestBaseOmitsConstEvalIntrinsics;
    procedure TestNativeRegistersIntrinsicTypeAliases;
    procedure TestBaseOmitsIntrinsicTypeAliases;
  end;

  { TTestEngineNativeResolver

    A native-target test engine: the same test plumbing (TTestResolverPlumbing)
    the base test engine uses, but resolving with native semantics. The three
    virtual container hooks the parser calls are delegated to the plumbing. }

  TTestEngineNativeResolver = class(TPasNativeResolver)
  private
    FPlumbing: TTestResolverPlumbing;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASrcPos: TPasSourcePos; TypeParams: TFPList = nil): TPasElement;
      overload; override;
    function FindUnit(const AName, InFilename: String; NameExpr,
      InFileExpr: TPasExpr): TPasModule; override;
    procedure UsedInterfacesFinished(Section: TPasSection); override;
    property Plumbing: TTestResolverPlumbing read FPlumbing;
  end;

  { TCustomNativeTestResolver

    Test-harness variant whose engine is a native-target resolver. }

  TCustomNativeTestResolver = class(TCustomTestResolver)
  protected
    function CreateResolverEngine(out Plumbing: TTestResolverPlumbing): TPasResolver; override;
  end;

  { TTestNativeResolverFold }

  TTestNativeResolverFold = class(TCustomNativeTestResolver)
  private
    // Returns the folded value of the program const named aName (caller frees
    // via ReleaseEvalValue).
    function EvalNamedConst(const aName: string): TResEvalValue;
    // Parses "const c = <aExpr>;" as a program, resolves it with the native
    // engine and returns the folded value of c.
    function FoldProgramConst(const aExpr: string): TResEvalValue;
  published
    procedure TestFoldSizeOfByte;
    procedure TestFoldSizeOfPointer;
    procedure TestFoldBitSizeOfByte;
    procedure TestFoldTrunc;
    procedure TestFoldRound;
    procedure TestFoldPointerCast;
    procedure TestFoldNamedPointerCast;
    procedure TestAddressOfDegradesToUnknown;
  end;

  { TTestElementStateFlags

    The target-agnostic range/overflow flag set on TPasElement.States — base
    TPasResolver behaviour, verified on the plain base engine. }

  TTestElementStateFlags = class(TCustomTestResolver)
  private
    // The first statement of the parsed program's main begin..end block.
    function FirstStatement: TPasImplElement;
  published
    procedure TestRangeCheckedMarked;
    procedure TestRangeCheckedOffNotMarked;
    procedure TestOverflowCheckedMarked;
    procedure TestBaseIgnoresPacking;
    procedure TestInterfaceOnlyForwardProcGate;
    procedure TestForwardProcUnresolvedFail;
    procedure TestBaseRejectsTypedPtrArithOff;
    procedure TestBaseAllowsAddrOfBitPackedField;
    procedure TestUnitLevelProperty;
  end;

  { TTestNativePacking

    The native memory-layout packing directives, captured per type at its
    declaration and stored global-free on the element (enum/record scope, set
    resolve-data). Uses the native engine. }

  TTestNativePacking = class(TCustomNativeTestResolver)
  private
    // First program declaration that is (or descends from) aClass.
    function FindDecl(aClass: TClass): TPasElement;
    function NativeEngine: TPasNativeResolver;
  published
    procedure TestMinEnumSizeCaptured;
    procedure TestMinEnumSizeDefault;
    procedure TestPackRecordsCaptured;
    procedure TestPackSetCaptured;
    procedure TestPackRecordsCrossResolver;
    procedure TestPackRecordsGenericSpecialize;
    procedure TestPackRecordsGenericSpecializeCrossResolver;
  end;

  { TTestNativePointerMath

    Native pointer arithmetic matching FPC: under $POINTERMATH OFF, the untyped
    Pointer, a pointer declared under $POINTERMATH ON, and a ^(Ansi)Char pointer
    permit +/- and indexing; Inc/Dec is allowed on any pointer. A plain typed
    pointer with none of these still requires the switch. }

  TTestNativePointerMath = class(TCustomNativeTestResolver)
  published
    procedure TestUntypedPointerAddOff;
    procedure TestTypedPointerDeclaredPointerMathAddOff;
    procedure TestCharPointerAddOff;
    procedure TestIncTypedPointerOff;
    procedure TestPlainTypedPointerAddOffRejected;
  end;

  { TTestNativeBitPacked

    A bit-packed ordinal field/element (sub-byte) cannot have its byte address
    taken: rejected for @, var/out arg, and absolute. A byte-sized/normal field
    is byte-addressable and accepted. }

  TTestNativeBitPacked = class(TCustomNativeTestResolver)
  published
    procedure TestAddrOfBitPackedFieldRejected;
    procedure TestVarParamBitPackedFieldRejected;
    procedure TestAbsoluteOverBitPackedFieldRejected;
    procedure TestAddrOfBitPackedArrayElemRejected;
    procedure TestAddrOfBitPackedByteFieldAccepted;
    procedure TestAddrOfNormalFieldAccepted;
    procedure TestBitPackedOrdinalBitSizeValue;
  end;

implementation

{ TDummyPasResolver }

function TDummyPasResolver.FindUnit(const AName, InFilename: String; NameExpr, InFileExpr: TPasExpr
  ): TPasModule;
begin
  Result:=nil;
  raise Exception.Create('20260716163809');
  if AName='' then ;
  if InFilename='' then ;
  if NameExpr=nil then ;
  if InFileExpr=nil then ;
end;

{ TDummyPasNativeResolver }

function TDummyPasNativeResolver.FindUnit(const AName, InFilename: String; NameExpr,
  InFileExpr: TPasExpr): TPasModule;
begin
  Result:=nil;
  raise Exception.Create('20260716163907');
  if AName='' then ;
  if InFilename='' then ;
  if NameExpr=nil then ;
  if InFileExpr=nil then ;
end;

function TTestNativeResolver.AliasBaseType(R: TPasResolver; const aName: string;
  out bt: TResolverBaseType): boolean;

var
  Ident: TPasIdentifier;

begin
  Result:=false;
  bt:=btNone;
  Ident:=R.DefaultScope.FindIdentifier(aName);
  if (Ident=nil) or (Ident.Element=nil) then exit;
  if not (Ident.Element.CustomData is TResElDataBaseType) then exit;
  bt:=TResElDataBaseType(Ident.Element.CustomData).BaseType;
  Result:=true;
end;


procedure TTestNativeResolver.TestNativeRegistersConstEvalIntrinsics;
// POSITIVE: TPasNativeResolver registers SizeOf/BitSizeOf/Trunc/Round.
var
  R: TPasNativeResolver;
begin
  R:=TDummyPasNativeResolver.Create;
  try
    R.AddObjFPCBuiltInIdentifiers;
    AssertNotNull('SizeOf registered',R.BuiltInProcs[bfSizeOf]);
    AssertNotNull('BitSizeOf registered',R.BuiltInProcs[bfBitSizeOf]);
    AssertNotNull('Trunc registered',R.BuiltInProcs[bfTrunc]);
    AssertNotNull('Round registered',R.BuiltInProcs[bfRound]);
  finally
    R.Free;
  end;
end;


procedure TTestNativeResolver.TestBaseOmitsConstEvalIntrinsics;
// NEGATIVE GUARD: the base TPasResolver (pas2js) leaves them unregistered.
var
  R: TPasResolver;
begin
  R:=TDummyPasResolver.Create;
  try
    R.AddObjFPCBuiltInIdentifiers;
    AssertNull('SizeOf not registered on base',R.BuiltInProcs[bfSizeOf]);
    AssertNull('BitSizeOf not registered on base',R.BuiltInProcs[bfBitSizeOf]);
    AssertNull('Trunc not registered on base',R.BuiltInProcs[bfTrunc]);
    AssertNull('Round not registered on base',R.BuiltInProcs[bfRound]);
  finally
    R.Free;
  end;
end;


procedure TTestNativeResolver.TestNativeRegistersIntrinsicTypeAliases;
// POSITIVE: the FPC compiler-intrinsic type names resolve to the right base type.
var
  R: TPasNativeResolver;
  bt: TResolverBaseType;
begin
  R:=TDummyPasNativeResolver.Create;
  try
    R.AddObjFPCBuiltInIdentifiers;
    AssertTrue('Boolean8 -> btByteBool',AliasBaseType(R,'Boolean8',bt) and (bt=btByteBool));
    AssertTrue('Boolean16 -> btWordBool',AliasBaseType(R,'Boolean16',bt) and (bt=btWordBool));
    AssertTrue('Boolean32 -> btLongBool',AliasBaseType(R,'Boolean32',bt) and (bt=btLongBool));
    AssertTrue('Boolean64 -> btQWordBool',AliasBaseType(R,'Boolean64',bt) and (bt=btQWordBool));
    AssertTrue('OleVariant -> btVariant',AliasBaseType(R,'OleVariant',bt) and (bt=btVariant));
    AssertTrue('TypedFile -> btFile',AliasBaseType(R,'TypedFile',bt) and (bt=btFile));
  finally
    R.Free;
  end;
end;


procedure TTestNativeResolver.TestBaseOmitsIntrinsicTypeAliases;
// NEGATIVE GUARD: the base TPasResolver (pas2js) does not know these names.
var
  R: TPasResolver;
  bt: TResolverBaseType;
begin
  R:=TDummyPasResolver.Create;
  try
    R.AddObjFPCBuiltInIdentifiers;
    AssertFalse('Boolean8 absent on base',AliasBaseType(R,'Boolean8',bt));
    AssertFalse('OleVariant absent on base',AliasBaseType(R,'OleVariant',bt));
    AssertFalse('TypedFile absent on base',AliasBaseType(R,'TypedFile',bt));
  finally
    R.Free;
  end;
end;

{ TTestEngineNativeResolver }

constructor TTestEngineNativeResolver.Create;

begin
  inherited Create;
  StoreSrcColumns:=true;
  FPlumbing:=TTestResolverPlumbing.Create(Self);
end;


destructor TTestEngineNativeResolver.Destroy;

begin
  FPlumbing.ReleaseParserScanner;
  inherited Destroy;
  FreeAndNil(FPlumbing);
end;


function TTestEngineNativeResolver.CreateElement(AClass: TPTreeElement;
  const AName: String; AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASrcPos: TPasSourcePos; TypeParams: TFPList): TPasElement;

begin
  Result:=inherited CreateElement(AClass, AName, AParent, AVisibility, ASrcPos, TypeParams);
  FPlumbing.NoteCreatedElement(Result,AClass);
end;


function TTestEngineNativeResolver.FindUnit(const AName, InFilename: String;
  NameExpr, InFileExpr: TPasExpr): TPasModule;

begin
  Result:=FPlumbing.DoFindUnit(AName,InFilename,NameExpr,InFileExpr);
end;


procedure TTestEngineNativeResolver.UsedInterfacesFinished(Section: TPasSection);

begin
  if Section=nil then ;
  // do not parse recursively, using a queue
end;


{ TCustomNativeTestResolver }

function TCustomNativeTestResolver.CreateResolverEngine(
  out Plumbing: TTestResolverPlumbing): TPasResolver;

var
  lEngine: TTestEngineNativeResolver;

begin
  lEngine:=TTestEngineNativeResolver.Create;
  Plumbing:=lEngine.Plumbing;
  Result:=lEngine;
end;


{ TTestNativeResolverFold }

function TTestNativeResolverFold.EvalNamedConst(const aName: string
  ): TResEvalValue;

var
  i: Integer;
  El: TPasElement;

begin
  Result:=nil;
  for i:=0 to PasProgram.ProgramSection.Declarations.Count-1 do
    begin
    El:=TPasElement(PasProgram.ProgramSection.Declarations[i]);
    if (El is TPasConst) and (CompareText(El.Name,aName)=0) then
      exit(ResolverEngine.Eval(TPasConst(El).Expr,[refConst]));
    end;
  Fail('const '+aName+' not found');
end;


function TTestNativeResolverFold.FoldProgramConst(const aExpr: string
  ): TResEvalValue;

begin
  StartProgram(false);
  Add('const');
  Add('  c = '+aExpr+';');
  Add('begin');
  ParseProgram;
  Result:=EvalNamedConst('c');
end;


procedure TTestNativeResolverFold.TestFoldSizeOfByte;

var
  V: TResEvalValue;

begin
  V:=FoldProgramConst('SizeOf(Byte)');
  try
    AssertNotNull('SizeOf(Byte) folds',V);
    AssertEquals('SizeOf(Byte) is integer',ord(revkInt),ord(V.Kind));
    AssertEquals('SizeOf(Byte)=1',1,TResEvalInt(V).Int);
  finally
    ReleaseEvalValue(V);
  end;
end;


procedure TTestNativeResolverFold.TestFoldSizeOfPointer;

var
  V: TResEvalValue;

begin
  // TargetPointerSize defaults to 0 -> host SizeOf(Pointer); assert that value.
  V:=FoldProgramConst('SizeOf(Pointer)');
  try
    AssertNotNull('SizeOf(Pointer) folds',V);
    AssertEquals('SizeOf(Pointer) is integer',ord(revkInt),ord(V.Kind));
    AssertEquals('SizeOf(Pointer)=host',SizeOf(Pointer),TResEvalInt(V).Int);
  finally
    ReleaseEvalValue(V);
  end;
end;


procedure TTestNativeResolverFold.TestFoldBitSizeOfByte;

var
  V: TResEvalValue;

begin
  V:=FoldProgramConst('BitSizeOf(Byte)');
  try
    AssertNotNull('BitSizeOf(Byte) folds',V);
    AssertEquals('BitSizeOf(Byte) is integer',ord(revkInt),ord(V.Kind));
    AssertEquals('BitSizeOf(Byte)=8',8,TResEvalInt(V).Int);
  finally
    ReleaseEvalValue(V);
  end;
end;


procedure TTestNativeResolverFold.TestFoldTrunc;

var
  V: TResEvalValue;

begin
  // Trunc truncates toward zero.
  V:=FoldProgramConst('Trunc(3.9)');
  try
    AssertNotNull('Trunc(3.9) folds',V);
    AssertEquals('Trunc(3.9) is integer',ord(revkInt),ord(V.Kind));
    AssertEquals('Trunc(3.9)=3',3,TResEvalInt(V).Int);
  finally
    ReleaseEvalValue(V);
  end;
end;


procedure TTestNativeResolverFold.TestFoldRound;

var
  V: TResEvalValue;

begin
  // Round is round-half-to-even: Round(2.5)=2.
  V:=FoldProgramConst('Round(2.5)');
  try
    AssertNotNull('Round(2.5) folds',V);
    AssertEquals('Round(2.5) is integer',ord(revkInt),ord(V.Kind));
    AssertEquals('Round(2.5)=2',2,TResEvalInt(V).Int);
  finally
    ReleaseEvalValue(V);
  end;
end;


procedure TTestNativeResolverFold.TestFoldPointerCast;

var
  V: TResEvalValue;

begin
  // A constant integer cast to the pointer base type folds to its signed value.
  V:=FoldProgramConst('pointer(-1)');
  try
    AssertNotNull('pointer(-1) folds',V);
    AssertEquals('pointer(-1) is integer',ord(revkInt),ord(V.Kind));
    AssertEquals('pointer(-1)=-1',-1,TResEvalInt(V).Int);
  finally
    ReleaseEvalValue(V);
  end;
end;


procedure TTestNativeResolverFold.TestFoldNamedPointerCast;

var
  V: TResEvalValue;

begin
  // A constant integer cast to a NAMED pointer type folds to the integer.
  StartProgram(false);
  Add('type');
  Add('  PByte = ^byte;');
  Add('const');
  Add('  c = PByte(1);');
  Add('begin');
  ParseProgram;
  V:=EvalNamedConst('c');
  try
    AssertNotNull('PByte(1) folds',V);
    AssertEquals('PByte(1) is integer',ord(revkInt),ord(V.Kind));
    AssertEquals('PByte(1)=1',1,TResEvalInt(V).Int);
  finally
    ReleaseEvalValue(V);
  end;
end;


procedure TTestNativeResolverFold.TestAddressOfDegradesToUnknown;

var
  V: TResEvalValue;

begin
  // An address-of degrades to an opaque (unknown) pointer value instead of
  // failing the unit — never a fabricated value.
  StartProgram(false);
  Add('var');
  Add('  x: byte;');
  Add('const');
  Add('  c: pointer = @x;');
  Add('begin');
  ParseProgram;
  V:=EvalNamedConst('c');
  try
    AssertNotNull('@x yields a value',V);
    AssertEquals('@x is unknown (revkNil)',ord(revkNil),ord(V.Kind));
  finally
    ReleaseEvalValue(V);
  end;
end;


{ TTestElementStateFlags }

function TTestElementStateFlags.FirstStatement: TPasImplElement;

begin
  AssertNotNull('program has an initialization section',PasProgram.InitializationSection);
  AssertTrue('program body has a statement',PasProgram.InitializationSection.Elements.Count>0);
  Result:=TPasImplElement(PasProgram.InitializationSection.Elements[0]);
end;


procedure TTestElementStateFlags.TestRangeCheckedMarked;

var
  Stmt: TPasImplElement;

begin
  StartProgram(false);
  Add('{$R+}');
  Add('var i: byte;');
  Add('begin');
  Add('  i:=1;');
  ParseProgram;
  Stmt:=FirstStatement;
  AssertEquals('first statement is an assignment',TPasImplAssign,Stmt.ClassType);
  AssertTrue('assignment under {$R+} is range-checked',
    ResolverEngine.IsRangeChecked(Stmt));
  AssertFalse('assignment is not overflow-checked',
    ResolverEngine.IsOverflowChecked(Stmt));
end;


procedure TTestElementStateFlags.TestRangeCheckedOffNotMarked;

begin
  StartProgram(false);
  Add('{$R-}');
  Add('var i: byte;');
  Add('begin');
  Add('  i:=1;');
  ParseProgram;
  AssertFalse('assignment under {$R-} is not range-checked',
    ResolverEngine.IsRangeChecked(FirstStatement));
end;


procedure TTestElementStateFlags.TestOverflowCheckedMarked;

var
  Assign: TPasImplAssign;

begin
  StartProgram(false);
  Add('{$Q+}');
  Add('var i: byte;');
  Add('begin');
  Add('  i:=i+1;');
  ParseProgram;
  Assign:=FirstStatement as TPasImplAssign;
  AssertTrue('arithmetic under {$Q+} is overflow-checked',
    ResolverEngine.IsOverflowChecked(Assign.Right));
end;


procedure TTestElementStateFlags.TestBaseIgnoresPacking;

var
  i: Integer;
  El: TPasElement;

begin
  // NEGATIVE GUARD: the base (pas2js) resolver records no packing — the Set*
  // seams are no-ops, so GetMinEnumSize stays 0 even under {$MINENUMSIZE}.
  StartProgram(false);
  Add('{$MINENUMSIZE 4}');
  Add('type');
  Add('  TEnum = (red, green, blue);');
  Add('begin');
  ParseProgram;
  El:=nil;
  for i:=0 to PasProgram.ProgramSection.Declarations.Count-1 do
    if TObject(PasProgram.ProgramSection.Declarations[i]) is TPasEnumType then
      begin
      El:=TPasElement(PasProgram.ProgramSection.Declarations[i]);
      break;
      end;
  AssertNotNull('enum type found',El);
  AssertEquals('base resolver ignores {$MINENUMSIZE}',0,ResolverEngine.GetMinEnumSize(El));
end;


procedure TTestElementStateFlags.TestInterfaceOnlyForwardProcGate;
begin
  // POSITIVE (§2): a dependency resolved INTERFACE-ONLY has its implementation
  // skipped, so an interface routine whose body would live there has no ImplProc.
  // FinishModule's forward-proc check is gated under InterfaceOnly, so this does
  // NOT raise — the program resolves clean.
  AddInterfaceOnlyModuleWithIntfImplSrc('dep.pas','procedure Foo;'+LineEnding,'');
  StartProgram(true);
  Add('uses dep;');
  Add('begin');
  ParseProgram;
end;


procedure TTestElementStateFlags.TestForwardProcUnresolvedFail;
begin
  // NEGATIVE GUARD: the SAME dependency parsed NORMALLY (implementation present
  // but without a body for Foo) DOES raise "Forward proc not resolved" — proving
  // Foo genuinely lacks a body, so it is the interface-only gate (§2) that saves
  // the positive case above, not an accident of the fixture.
  AddModuleWithIntfImplSrc('dep.pas','procedure Foo;'+LineEnding,'');
  StartProgram(true);
  Add('uses dep;');
  Add('begin');
  CheckResolverException(sForwardProcNotResolved,nForwardProcNotResolved);
end;


procedure TTestElementStateFlags.TestBaseRejectsTypedPtrArithOff;
begin
  // NEGATIVE GUARD (pas2js base): with {$POINTERMATH OFF}, typed-pointer
  // arithmetic is rejected — the base resolver has no IsPointerMathType, so it
  // stays switch-gated. (The arithmetic is in a procedure so the OFF switch is
  // captured on the proc scope, not the module scope from the program header.)
  StartProgram(false);
  Add('{$POINTERMATH OFF}');
  Add('type PInt = ^longint;');
  Add('procedure Foo;');
  Add('var p: PInt;');
  Add('begin');
  Add('  p := p + 1;');
  Add('end;');
  Add('begin');
  CheckResolverException(sOperatorIsNotOverloadedAOpB,nOperatorIsNotOverloadedAOpB);
end;


procedure TTestElementStateFlags.TestBaseAllowsAddrOfBitPackedField;
begin
  // NEGATIVE GUARD (pas2js base): the base resolver has no IsBitPackedOrdinalAccess,
  // so @ of a bit-packed field is NOT rejected (pas2js has no bit-packing).
  StartProgram(false);
  Add('type TBR = bitpacked record a: 0..3; b: 0..7; end;');
  Add('var br: TBR; p: Pointer;');
  Add('begin');
  Add('  p := @br.a;');
  ParseProgram;
end;


procedure TTestElementStateFlags.TestUnitLevelProperty;
begin
  // A unit-level (global) property backed by read/write functions must resolve
  // (base, target-agnostic tolerance): AddProperty accepts a section-scoped
  // owner and FinishProperty does not hard-cast the section owner to a members
  // type. The RTL uses this (e.g. threadh.inc CPUCount).
  AddModuleWithIntfImplSrc('u.pas',
    'function GetX: integer;'+LineEnding+
    'procedure SetX(v: integer);'+LineEnding+
    'property X: integer read GetX write SetX;'+LineEnding,
    'function GetX: integer; begin Result:=1; end;'+LineEnding+
    'procedure SetX(v: integer); begin if v=0 then ; end;'+LineEnding);
  StartProgram(true);
  Add('uses u;');
  Add('begin');
  ParseProgram;
end;


{ TTestNativePacking }

function TTestNativePacking.FindDecl(aClass: TClass): TPasElement;

var
  i: Integer;
  El: TPasElement;

begin
  Result:=nil;
  for i:=0 to PasProgram.ProgramSection.Declarations.Count-1 do
    begin
    El:=TPasElement(PasProgram.ProgramSection.Declarations[i]);
    if El.InheritsFrom(aClass) then exit(El);
    end;
  Fail('declaration of class '+aClass.ClassName+' not found');
end;


function TTestNativePacking.NativeEngine: TPasNativeResolver;

begin
  Result:=ResolverEngine as TPasNativeResolver;
end;


procedure TTestNativePacking.TestMinEnumSizeCaptured;

var
  EnumType: TPasEnumType;

begin
  StartProgram(false);
  Add('{$MINENUMSIZE 4}');
  Add('type');
  Add('  TEnum = (red, green, blue);');
  Add('begin');
  ParseProgram;
  EnumType:=TPasEnumType(FindDecl(TPasEnumType));
  AssertEquals('min-enum-size captured',4,NativeEngine.GetMinEnumSize(EnumType));
  AssertEquals('enum storage size honours {$MINENUMSIZE}',4,
    NativeEngine.GetEnumTypeSize(EnumType));
end;


procedure TTestNativePacking.TestMinEnumSizeDefault;

var
  EnumType: TPasEnumType;

begin
  StartProgram(false);
  Add('type');
  Add('  TEnum = (red, green, blue);');
  Add('begin');
  ParseProgram;
  EnumType:=TPasEnumType(FindDecl(TPasEnumType));
  AssertEquals('no min-enum-size',0,NativeEngine.GetMinEnumSize(EnumType));
  AssertEquals('natural enum storage size = 1',1,NativeEngine.GetEnumTypeSize(EnumType));
end;


procedure TTestNativePacking.TestPackRecordsCaptured;

var
  RecType: TPasRecordType;

begin
  StartProgram(false);
  Add('{$PACKRECORDS 2}');
  Add('type');
  Add('  TRec = record x: byte; end;');
  Add('begin');
  ParseProgram;
  RecType:=TPasRecordType(FindDecl(TPasRecordType));
  AssertEquals('pack-records captured',2,NativeEngine.GetPackRecords(RecType));
end;


procedure TTestNativePacking.TestPackSetCaptured;

var
  SetType: TPasSetType;

begin
  StartProgram(false);
  Add('{$PACKSET 2}');
  Add('type');
  Add('  TSet = set of byte;');
  Add('begin');
  ParseProgram;
  SetType:=TPasSetType(FindDecl(TPasSetType));
  AssertEquals('pack-set captured',2,NativeEngine.GetPackSet(SetType));
end;


procedure TTestNativePacking.TestPackRecordsCrossResolver;

var
  UnitEngine: TPasResolver;
  UnitModule: TPasModule;
  Decls: TFPList;
  RecType: TPasElement;
  i: Integer;

begin
  // unit1 is parsed by its OWN resolver instance (A): a record under
  // {$PACKRECORDS 2}, so A records the pack value on the record's element.
  AddModuleWithIntfImplSrc('unit1.pas',
    '{$PACKRECORDS 2}'+LineEnding+
    'type'+LineEnding+
    '  TRec = record x: byte; end;'+LineEnding,
    '');
  // The program is parsed by a DIFFERENT resolver instance (B).
  StartProgram(true);
  Add('uses unit1;');
  Add('var r: unit1.TRec;');
  Add('begin');
  ParseProgram;

  // Reach unit1.TRec on the module that resolver A parsed.
  UnitEngine:=FindModuleWithFilename('unit1.pas');
  AssertNotNull('unit1 resolver found',UnitEngine);
  UnitModule:=PlumbingOf(UnitEngine).Module;
  AssertNotNull('unit1 module found',UnitModule);
  RecType:=nil;
  Decls:=UnitModule.InterfaceSection.Declarations;
  for i:=0 to Decls.Count-1 do
    if TObject(Decls[i]) is TPasRecordType then
      begin
      RecType:=TPasElement(Decls[i]);
      break;
      end;
  AssertNotNull('unit1.TRec found',RecType);
  // The fact resolver A stored on the element is readable by resolver B, with no
  // process-global — the property the eliminated globals used to provide.
  AssertEquals('cross-resolver: {$PACKRECORDS} stored by unit1''s resolver is readable by the program''s',
    2,NativeEngine.GetPackRecords(RecType));
end;


procedure TTestNativePacking.TestPackRecordsGenericSpecialize;

var
  i: Integer;
  El: TPasElement;
  Spec: TPasSpecializeType;
  Specialized: TPasGenericType;

begin
  // A generic record template under {$PACKRECORDS 2}, specialized: the pack value
  // must propagate from the template onto the specialized type. Top-level generic
  // types are specialized via SpecializeGenericIntf (not SpecializeElement), so
  // this exercises the SpecializePackValues call added there.
  StartProgram(false);
  Add('{$PACKRECORDS 2}');
  Add('type');
  Add('  generic TGenRec<T> = record x: T; end;');
  Add('  TIntRec = specialize TGenRec<longint>;');
  Add('var');
  Add('  r: TIntRec;');
  Add('begin');
  ParseProgram;
  Spec:=nil;
  for i:=0 to PasProgram.ProgramSection.Declarations.Count-1 do
    begin
    El:=TPasElement(PasProgram.ProgramSection.Declarations[i]);
    if El is TPasSpecializeType then
      begin Spec:=TPasSpecializeType(El); break; end
    else if (El is TPasAliasType) and (TPasAliasType(El).DestType is TPasSpecializeType) then
      begin Spec:=TPasSpecializeType(TPasAliasType(El).DestType); break; end;
    end;
  AssertNotNull('specialize-type found',Spec);
  AssertTrue('specialize-type has resolver data',Spec.CustomData is TPasSpecializeTypeData);
  Specialized:=TPasSpecializeTypeData(Spec.CustomData).SpecializedType;
  AssertNotNull('specialized record exists',Specialized);
  AssertEquals('specialized record carries the template''s {$PACKRECORDS}',
    2,NativeEngine.GetPackRecords(Specialized));
end;


procedure TTestNativePacking.TestPackRecordsGenericSpecializeCrossResolver;

var
  Spec: TPasSpecializeType;
  Specialized: TPasGenericType;

begin
  // AC#5 in full: a generic template parsed by unit1's resolver (A), under
  // {$PACKRECORDS 2}, specialized by the program's resolver (B). The pack value
  // must cross from A's template to B's specialized clone — no global.
  AddModuleWithIntfImplSrc('unit1.pas',
    '{$PACKRECORDS 2}'+LineEnding+
    'type'+LineEnding+
    '  generic TGenRec<T> = record x: T; end;'+LineEnding,
    '');
  StartProgram(true);
  Add('uses unit1;');
  Add('var');
  Add('  r: specialize TGenRec<longint>;');
  Add('begin');
  ParseProgram;
  Spec:=TPasSpecializeType(TPasVariable(FindDecl(TPasVariable)).VarType);
  AssertTrue('var type is a specialize-type',Spec is TPasSpecializeType);
  AssertTrue('specialize-type has resolver data',Spec.CustomData is TPasSpecializeTypeData);
  Specialized:=TPasSpecializeTypeData(Spec.CustomData).SpecializedType;
  AssertNotNull('specialized record exists',Specialized);
  AssertEquals('cross-resolver generic: specialized clone (B) carries unit1 template''s (A) {$PACKRECORDS}',
    2,NativeEngine.GetPackRecords(Specialized));
end;


{ TTestNativePointerMath

  Note: the pointer arithmetic sits inside a procedure so that $POINTERMATH OFF
  is captured on the procedure scope. The main begin..end of a program resolves
  its bool switches from the module scope, which is snapshotted at the program
  header — before any directive — so an OFF there would have no effect. }

procedure TTestNativePointerMath.TestUntypedPointerAddOff;
begin
  // untyped Pointer +/- is always allowed on the native target (FPC: byte-scaled)
  StartProgram(false);
  Add('{$POINTERMATH OFF}');
  Add('procedure Foo;');
  Add('var p: Pointer;');
  Add('begin');
  Add('  p := p + 1;');
  Add('end;');
  Add('begin');
  ParseProgram;
end;


procedure TTestNativePointerMath.TestTypedPointerDeclaredPointerMathAddOff;
begin
  // a typed pointer DECLARED under {$POINTERMATH ON} keeps pointer arithmetic
  // even when used under {$POINTERMATH OFF} (FPC declaration-site flag ->
  // pesfPointerMath).
  StartProgram(false);
  Add('{$POINTERMATH ON}');
  Add('type PInt = ^longint;');
  Add('{$POINTERMATH OFF}');
  Add('procedure Foo;');
  Add('var p: PInt;');
  Add('begin');
  Add('  p := p + 1;');
  Add('end;');
  Add('begin');
  ParseProgram;
end;


procedure TTestNativePointerMath.TestCharPointerAddOff;
begin
  // a ^AnsiChar pointer is always pointer-math (PChar-like), even {$POINTERMATH OFF}
  StartProgram(false);
  Add('{$POINTERMATH OFF}');
  Add('type PAC = ^AnsiChar;');
  Add('procedure Foo;');
  Add('var p: PAC;');
  Add('begin');
  Add('  p := p + 1;');
  Add('end;');
  Add('begin');
  ParseProgram;
end;


procedure TTestNativePointerMath.TestIncTypedPointerOff;
begin
  // Inc/Dec is allowed on any pointer, switch-independent (FPC)
  StartProgram(false);
  Add('{$POINTERMATH OFF}');
  Add('type PInt = ^longint;');
  Add('procedure Foo;');
  Add('var p: PInt;');
  Add('begin');
  Add('  Inc(p);');
  Add('end;');
  Add('begin');
  ParseProgram;
end;


procedure TTestNativePointerMath.TestPlainTypedPointerAddOffRejected;
begin
  // NEGATIVE: a plain typed pointer under {$POINTERMATH OFF}, NOT declared under
  // POINTERMATH and not ^char -> the native resolver still rejects (matches FPC:
  // decl-OFF/use-OFF -> "Operation + not supported"). Proves the flag is required.
  StartProgram(false);
  Add('{$POINTERMATH OFF}');
  Add('type PInt = ^longint;');
  Add('procedure Foo;');
  Add('var p: PInt;');
  Add('begin');
  Add('  p := p + 1;');
  Add('end;');
  Add('begin');
  CheckResolverException(sOperatorIsNotOverloadedAOpB,nOperatorIsNotOverloadedAOpB);
end;


{ TTestNativeBitPacked }

procedure TTestNativeBitPacked.TestAddrOfBitPackedFieldRejected;
begin
  StartProgram(false);
  Add('type TBR = bitpacked record a: 0..3; b: 0..7; end;');
  Add('var br: TBR; p: Pointer;');
  Add('begin');
  Add('  p := @br.a;');
  CheckResolverException(sCannotTakeAddrOfBitPackedElement,nCannotTakeAddrOfBitPackedElement);
end;


procedure TTestNativeBitPacked.TestVarParamBitPackedFieldRejected;
begin
  StartProgram(false);
  Add('type TBR = bitpacked record a: 0..3; end;');
  Add('procedure DoIt(var x); begin if @x=nil then ; end;');
  Add('var br: TBR;');
  Add('begin');
  Add('  DoIt(br.a);');
  CheckResolverException(sCannotTakeAddrOfBitPackedElement,nCannotTakeAddrOfBitPackedElement);
end;


procedure TTestNativeBitPacked.TestAbsoluteOverBitPackedFieldRejected;
begin
  StartProgram(false);
  Add('type TBR = bitpacked record a: 0..3; end;');
  Add('var br: TBR;');
  Add('var x: byte absolute br.a;');
  Add('begin');
  CheckResolverException(sCannotTakeAddrOfBitPackedElement,nCannotTakeAddrOfBitPackedElement);
end;


procedure TTestNativeBitPacked.TestAddrOfBitPackedArrayElemRejected;
begin
  StartProgram(false);
  Add('type TBA = bitpacked array[0..7] of 0..3;');
  Add('var ba: TBA; p: Pointer;');
  Add('begin');
  Add('  p := @ba[2];');
  CheckResolverException(sCannotTakeAddrOfBitPackedElement,nCannotTakeAddrOfBitPackedElement);
end;


procedure TTestNativeBitPacked.TestAddrOfBitPackedByteFieldAccepted;
begin
  // a byte-sized field in a bitpacked record IS byte-addressable -> accepted
  StartProgram(false);
  Add('type TBR = bitpacked record a: byte; b: byte; end;');
  Add('var br: TBR; p: Pointer;');
  Add('begin');
  Add('  p := @br.a;');
  ParseProgram;
end;


procedure TTestNativeBitPacked.TestAddrOfNormalFieldAccepted;
begin
  // a normal (non-bitpacked) record field is addressable -> accepted
  StartProgram(false);
  Add('type TNR = record a: 0..3; b: 0..7; end;');
  Add('var nr: TNR; p: Pointer;');
  Add('begin');
  Add('  p := @nr.a;');
  ParseProgram;
end;


procedure TTestNativeBitPacked.TestBitPackedOrdinalBitSizeValue;
var
  Assign: TPasImplAssign;
begin
  // the relocated native-consumer API: bit width of a bit-packed field access
  StartProgram(false);
  Add('type TBR = bitpacked record a: 0..3; b: 0..7; end;');
  Add('var br: TBR; x: byte;');
  Add('begin');
  Add('  x := br.a;');
  ParseProgram;
  Assign:=TPasImplAssign(PasProgram.InitializationSection.Elements[0]);
  AssertEquals('bit width of br.a (0..3)',2,
    (ResolverEngine as TPasNativeResolver).BitPackedOrdinalBitSize(Assign.Right));
end;


initialization
  RegisterTest(TTestNativeResolver);
  RegisterTest(TTestNativeResolverFold);
  RegisterTest(TTestElementStateFlags);
  RegisterTest(TTestNativePacking);
  RegisterTest(TTestNativePointerMath);
  RegisterTest(TTestNativeBitPacked);
end.
