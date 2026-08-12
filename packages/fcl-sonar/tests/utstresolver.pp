{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the tolerant resolver wrapper

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstResolver;

{ Tolerant resolver wrapper tests. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasTree,
  FpSonar.Types, FpSonar.Issues,
  FpSonar.Resolver, FpSonar.RuleFramework, FpSonar.SourceFile, FpSonar.PpuStub,
  UtstFixtures, UtstCoreFixtures;

type
  { A synthetic SEM rule: emits one sentinel issue carrying its own RuleId when
    it is dispatched (i.e. when the rfResolver feed is available). }
  TSynthSemRule = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Tolerant resolver tests. }
  TResolverTest = class(TTestCase)
  private
    FFix: TTempFixtures;
    function FindDecl(aModule: TPasModule; const aName: string): TPasElement;
    // Finds a declaration by name in an explicit section (interface OR impl).
    function FindDeclInSection(aSection: TPasSection;
      const aName: string): TPasElement;
    // The resolved dependency module named aName from aConsumer's interface
    // uses-clause, or nil if absent/unresolved.
    function FindUsedModule(aConsumer: TPasModule;
      const aName: string): TPasModule;
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // True iff aType is a class whose ancestor chain reaches a root named 'TObject'.
    function AncestryReachesTObject(aType: TPasType): Boolean;
    // Host-RTL-INDEPENDENT fake-tree helpers: a unique temp dir, a unit-file
    // writer, and a recursive tree remover.
    function FreshTempDir: string;
    procedure WriteUnit(const aDir, aFile, aSource: string);
    procedure RemoveTree(const aDir: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ResolvableFixtureResolvesLocalType;
    procedure UntypedQueryReturnsUnresolvedSentinel;
    procedure UnresolvableInputFoldsResolveError;
    procedure SemRuleRunsWhenFeedPresentAndSkipsWhenAbsent;
    // Cross-unit resolution.
    procedure CrossUnitDependencyResolvesAndQueries;
    procedure SystemAliasesResolveViaSyntheticSystem;
    procedure MissingDependencyDegradesToResolveError;
    procedure CircularUsesTerminatesAndDegrades;
    // Enriched System + synthetic RTL stubs.
    procedure EnrichedSystemTypesResolveAndCollapse;
    procedure StubResolutionResolvesAndQueries;
    procedure ResolverBadNowResolvesViaSyntheticSysUtils;
    // Interface-only dependency resolution.
    procedure DependencyInterfaceOnlyResolvesConsumerAndSkipsImpl;
    procedure InterfaceRoutineWithSkippedBodyNotMisreported;
    procedure UnitLevelPropertyResolvesUnderInterfaceOnly;
    procedure InterfaceOnlyOffIsByteIdenticalDefault;
    // Intrinsic const-eval.
    procedure SizeOfFoldsTargetCorrect;
    procedure SizeOfPointerWidthIsTargetDriven;
    procedure AggregateSizeOfDegradesToUnknown;
    procedure TruncRoundPerformRealFloatFold;
    procedure PointerCastFoldsConstIntAndAddressOfResolvesOpaque;
    // The real-RTL-preferred wiring (host-RTL-INDEPENDENT, fake tree).
    procedure RealRtlPreferredDefaultsOff;
    procedure RealRtlPrefersRealSourceOverSyntheticStub;
    procedure RealRtlFallsBackToSyntheticWhenRealFails;
    procedure ObjpasRidesUsesChainUnderRealRtlOnly;
    procedure RealRtlModeWiresResolverKnobsViaSourceFile;
    // Intrinsic-builtin gap + superset file-type regression guard.
    procedure IntrinsicBuiltinsResolveAndFileTypeStillResolves;
    // The resolver-backed declared()/sizeof() cond-directive evaluator
    // (host-RTL-INDEPENDENT: {$if} directives over the unit's OWN declarations
    // on a synthetic fake tree).
    procedure RealRtlDeclaredCondDirectiveResolvesAndDefaultDegrades;
    procedure RealRtlUnknownCondFunctionDegradesUnit;
    procedure RealRtlSizeOfCondDirectiveFoldsTargetCorrect;
    { The synthetic-stub substrate resolves faithful-in-kind with NO host-RTL
      paths (Classes.TFPList, Math.TValueSign, System.TStringArray, SysUtils
      helpers + synthetic Types/contnrs/regexpr). }
    procedure SubstrateStubsResolveHostRtlFree;
    { Deep resolution is the host-independent DEFAULT: the 5-arg
      SourceFile.Analyze (no --real-rtl) resolves a consumer even when its
      dependency's IMPLEMENTATION is unresolvable, because deps are interface-only
      by default. }
    procedure DeepResolutionDefaultResolvesInterfaceOnlyDepViaSourceFile;
    { Auto-detect default off on a bare resolver (== --synthetic-only,
      byte-identical); the in-Pascal generator's output SHAPES resolve + bind
      (host-RTL-INDEPENDENT: small hand-authored FLAT stubs of exactly the shape
      the generator emits, resolved via -Fu — NOT the host's .ppu). }
    procedure PpuAutoDetectDefaultsOff;
    procedure PpuStubFlatStubResolvesConsumerAndBindsMemberTypes;
    procedure PpuStubOmittedMemberDegradesAndOffIsByteIdentical;
    { The ppudump generator's TYPE-HELPER emission (host-RTL-INDEPENDENT: a small
      hand-authored FLAT stub with a `type helper`, NOT the committed generated
      set). A helper-method member access binds to the stub decl with the right
      result type; an omitted helper method degrades the whole unit to silence;
      flag OFF is byte-identical. }
    procedure PpuStubTypeHelperMethodBindsAndDegrades;
    { The vendored-resolver fix accepting a writable string character-index
      l-value (`s[i]`, the ReadBuffer(s[1],n) idiom) as an untyped var/out
      actual, while still rejecting a non-l-value (a literal).
      Host-RTL-INDEPENDENT (both fixtures have no uses clause). }
    procedure UntypedVarOutAcceptsLValueAndRejectsNonLValue;
    { The broad-coverage generator shapes (host-RTL-INDEPENDENT: a small
      hand-authored FLAT stub carrying a record, a set and a procedural type —
      the shapes the broad RTL/FCL surface exercises). Member access binds each
      to its faithful stub type; a consumer touching an omitted shape degrades
      the whole unit to silence; flag OFF is byte-identical. }
    procedure PpuStubBroadShapesBindAndOmittedDegrades;
    { The in-Pascal generator's host-independent pieces: the RTL hybrid emitter
      (synthetic + faithful gap members for SysUtils/Classes; '' for a non-RTL
      name or a missing splice anchor) and the ppudump NaN/Inf JSON repair. }
    procedure HybridRtlSourceEmitsSupersetGapMembers;
    procedure PpudumpJsonRepairQuotesNaNInf;
    { Auto-detect serves the host-independent HYBRID RTL: a consumer reading
      SysUtils.FileExists (a synthetic-absent gap member) resolves under
      PpuAutoDetect (hybrid, no ppudump needed) and DEGRADES under --synthetic-only
      (never-worse-than-synthetic proven by the off case failing). }
    procedure AutoDetectHybridRtlResolvesGapMemberOffDegrades;
  end;


implementation

const
  cMode = 'OBJFPC';
  cDefines: array[0..3] of string = ('FPC', 'CPUX86_64', 'UNIX', 'LINUX');

procedure TSynthSemRule.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

begin
  aCollector.AddIssue(FMetadata.RuleId, aContext.FileName, 1, 1, 1, 1,
    FMetadata.Severity, FMetadata.Category, FMetadata.DefaultConfidence,
    FMetadata.MessageKey, [], 'synthsem');
end;


const
  // Embedded resolver fixtures (Approach A rollout): line i+1 == [i].

  cResolverFixture: array[0..36] of string = (
    'unit ResolverFixture;',
    '',
    '{ 3.23 — self-contained, RESOLVABLE fixture for the tolerant resolver.',
    '',
    '  Deliberately uses ONLY the resolver''s builtin base-type names (Longint /',
    '  Boolean / ShortString) and has NO explicit uses clause, so it resolves CLEAN',
    '  with NO unit search paths at all (it needs no cross-unit dependency; the',
    '  implicit System binds via the synthetic minimal System unit). A local var',
    '  therefore resolves to a concrete type; the untyped argument exercises the',
    '  unresolved-but-no-exception query path. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TLocalInt = Longint;',
    '',
    'var',
    '  GLocalValue: Longint;',
    '  GLocalFlag: Boolean;',
    '  GLocalText: ShortString;',
    '',
    '// An untyped var argument: its computed type is "untyped" — a resolvable unit',
    '// whose specific query has no concrete type (degrade-to-silence at the query).',
    'procedure UntypedSink(var aRaw);',
    '',
    '',
    'implementation',
    '',
    'procedure UntypedSink(var aRaw);',
    '',
    'begin',
    'end;',
    '',
    '',
    'end.');

  cAbsentUses: array[0..26] of string = (
    'unit AbsentUses;',
    '',
    '{ Genuinely-unresolvable resolver fixture.',
    '',
    '  `uses` a unit that does not exist anywhere (no synthetic stub, no file on any',
    '  search path), so the cross-unit FindUnit returns nil and resolution degrades to',
    '  exactly one dkResolveError with the module left nil — degrade-to-silence.',
    '',
    '  This is the degrade fixture: since SysUtils binds synthetically, `uses SysUtils`',
    '  resolves clean, so a genuinely-absent unit is required to exercise the degrade',
    '  path. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  NoSuchUnitXyz;',
    '',
    'var',
    '  GValue: Longint;',
    '',
    '',
    'implementation',
    '',
    '',
    'end.');

  cMainUsesDep: array[0..22] of string = (
    'unit MainUsesDep;',
    '',
    '{ cross-unit resolver fixture: the USING unit.',
    '',
    '  `uses DepUnit` and exposes a var typed by DepUnit''s TDepThing. Resolves clean',
    '  ONLY if the cross-unit FindUnit locates + resolves DepUnit on the unit search',
    '  paths; a query on GMainValue then returns DepUnit''s resolved type. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  DepUnit;',
    '',
    'var',
    '  GMainValue: TDepThing;',
    '',
    '',
    'implementation',
    '',
    '',
    'end.');

  cDepUnit: array[0..24] of string = (
    'unit DepUnit;',
    '',
    '{ Story 3.23 — cross-unit resolver fixture: the DEPENDENCY.',
    '',
    '  A self-contained unit declaring a type that the sibling MainUsesDep fixture',
    '  references through its uses clause, so the cross-unit FindUnit must parse +',
    '  RESOLVE this unit (not merely parse it) for MainUsesDep to resolve clean. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TDepThing = record',
    '    Value: Longint;',
    '  end;',
    '',
    'var',
    '  GDepValue: TDepThing;',
    '',
    '',
    'implementation',
    '',
    '',
    'end.');

  cSysAlias: array[0..22] of string = (
    'unit SysAlias;',
    '',
    '{ Story 3.23 — System-alias resolver fixture.',
    '',
    '  References the System aliases Integer/Cardinal and TObject (NOT resolver',
    '  builtins) with no explicit uses clause, relying on the implicit `uses System`',
    '  bound to the wrapper''s synthetic minimal System. Resolves clean under the',
    '  chosen System strategy without any host-RTL checkout. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'var',
    '  GInt: Integer;',
    '  GCard: Cardinal;',
    '  GObj: TObject;',
    '',
    '',
    'implementation',
    '',
    '',
    'end.');

  cCircularA: array[0..22] of string = (
    'unit CircularA;',
    '',
    '{ Story 3.23 — circular-uses resolver fixture (A).',
    '',
    '  CircularA uses CircularB which uses CircularA: the cross-unit FindUnit''s cycle',
    '  guard must terminate (return nil on the re-entrant edge) and degrade cleanly,',
    '  never loop forever or crash. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  CircularB;',
    '',
    'var',
    '  GA: Longint;',
    '',
    '',
    'implementation',
    '',
    '',
    'end.');

  cCircularB: array[0..18] of string = (
    'unit CircularB;',
    '',
    '{ Story 3.23 — circular-uses resolver fixture (B). See CircularA. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  CircularA;',
    '',
    'var',
    '  GB: Longint;',
    '',
    '',
    'implementation',
    '',
    '',
    'end.');

  cEnrichedSystem: array[0..26] of string = (
    'unit EnrichedSystem;',
    '',
    '{ Enriched-synthetic-System resolver fixture.',
    '',
    '  References System types that are NOT resolver builtins and rely on the enriched',
    '  synthetic System: the alias families (NativeInt/PtrInt/Real/',
    '  UTF8String) collapse to their base type, while the builtin families (Int64/',
    '  Double/AnsiString) resolve to themselves. Resolves clean with NO unit search',
    '  paths and no host-RTL checkout — the implicit "uses System" binds the enriched',
    '  synthetic module. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'var',
    '  GBig: Int64;',
    '  GReal: Double;',
    '  GText: AnsiString;',
    '  GNative: NativeInt;',
    '  GUtf: UTF8String;',
    '',
    '',
    'implementation',
    '',
    '',
    'end.');

  cStubUser: array[0..25] of string = (
    'unit StubUser;',
    '',
    '{ Synthetic-RTL-stub resolver fixture.',
    '',
    '  `uses SysUtils, Classes` and exposes vars typed by stubbed RTL classes. Resolves',
    '  clean with NO host-RTL -Fu paths: SysUtils/Classes bind to the wrapper''s',
    '  in-process synthetic stubs. A query on GErr/GList returns the stub class type,',
    '  whose ancestry is faithful to the real RTL (Exception -> TObject; TStringList ->',
    '  TStrings -> TPersistent -> TObject) — the hierarchy the cast rules walk. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils, Classes;',
    '',
    'var',
    '  GErr: Exception;',
    '  GList: TStringList;',
    '',
    '',
    'implementation',
    '',
    '',
    'end.');

  cResolverBad: array[0..16] of string = (
    'unit ResolverBad;',
    '',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'var',
    '  GValue: Longint;',
    '',
    '',
    'implementation',
    '',
    'end.');

  cSubstrate: array[0..52] of string = (
    'unit Substrate;',
    '',
    '{ Synthetic-stub substrate resolver fixture.',
    '',
    '  `uses SysUtils, Classes, Math, Types, contnrs, regexpr` and exercises the',
    '  faithful-in-kind symbols: Classes.TFPList/TFileStream,',
    '  Math.TValueSign, System.TStringArray, the new synthetic Types/contnrs/regexpr',
    '  units, and the SysUtils path/string helpers. Resolves clean with NO host-RTL',
    '  -Fu paths — every dependency binds to the wrapper''s in-process synthetic stubs.',
    '  A query on GList returns Classes.TFPList, whose ancestry is faithful to the',
    '  real RTL (TFPList -> TObject). }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils, Classes, Math, Types, contnrs, regexpr;',
    '',
    'var',
    '  GList: TFPList;',
    '  GStream: TFileStream;',
    '  GSign: TValueSign;',
    '  GArr: TStringArray;',
    '  GPoint: TPoint;',
    '  GObjs: TFPObjectList;',
    '  GHash: TFPHashList;',
    '  GRe: TRegExpr;',
    '',
    '',
    'implementation',
    '',
    '',
    'procedure UseHelpers;',
    '',
    'var',
    '  ls: AnsiString;',
    '  li: Longint;',
    '',
    'begin',
    '  ls := IncludeTrailingPathDelimiter(''x'');',
    '  ls := TrimLeft(ls);',
    '  ls := TrimRight(ls);',
    '  li := CompareStr(ls, ''y'');',
    '  ls := ExtractFilePath(ls);',
    '  ls := ExtractFileName(ls);',
    '  ls := StringReplace(ls, ''a'', ''b'', [rfReplaceAll]);',
    '  if li > 0 then',
    '    ls := ls + PathDelim;',
    'end;',
    '',
    '',
    'end.');

  cIfaceOnlyMain: array[0..23] of string = (
    'unit IfaceOnlyMain;',
    '',
    '{ interface-only dependency fixture: the USING unit.',
    '',
    '  `uses IfaceOnlyDep` and exposes a var typed by the dependency''s interface type.',
    '  Resolves clean only if the cross-unit FindUnit resolves IfaceOnlyDep and (under',
    '  interface-only) its interface section still yields a section scope so this',
    '  consumer''s uses-clause is satisfied. Mirrors mainusesdep.pas / depunit.pas. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  IfaceOnlyDep;',
    '',
    'var',
    '  GMainThing: TIfaceThing;',
    '',
    '',
    'implementation',
    '',
    '',
    'end.');

  cIfaceOnlyDep: array[0..53] of string = (
    'unit IfaceOnlyDep;',
    '',
    '{ Interface-only dependency fixture: the DEPENDENCY.',
    '',
    '  Its interface declares the three shapes the interface-only mechanism must',
    '  tolerate:',
    '    (a) ComputeThing — a free function whose body lives in the implementation',
    '        (the forward-proc shape: under interface-only the body is skipped,',
    '        so a naive resolver would misreport "Forward function not resolved");',
    '    (b) TIfaceThing — a record type a consumer can reference, giving a',
    '        queryable interface symbol;',
    '    (c) UnitCount — a unit-level / global property with a getter',
    '        (a property whose owner is the unit section, not a',
    '        class/record).',
    '  The implementation carries the function bodies plus cImplOnlyValue, an',
    '  impl-only constant used to prove absence: under interface-only the whole',
    '  implementation section is skipped, so it is absent; under full resolve it is',
    '  present. Host-RTL-independent (implicitly uses synthetic System only). }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TIfaceThing = record',
    '    Value: Longint;',
    '  end;',
    '',
    '// (a) free function — body lives in the (interface-only-skipped) implementation.',
    'function ComputeThing: Longint;',
    '',
    '// getter backing the unit-level property below.',
    'function GetCount: Longint;',
    '',
    '// (c) unit-level / global property.',
    'property UnitCount: Longint read GetCount;',
    '',
    'implementation',
    '',
    'const',
    '  // impl-only symbol: ABSENT under interface-only, present under full.',
    '  cImplOnlyValue = 42;',
    '',
    'function ComputeThing: Longint;',
    'begin',
    '  Result := cImplOnlyValue;',
    'end;',
    '',
    'function GetCount: Longint;',
    'begin',
    '  Result := cImplOnlyValue;',
    'end;',
    '',
    'end.');

  cConstEvalSizes: array[0..27] of string = (
    'unit constevalsizes;',
    '',
    '{ SizeOf/BitSizeOf const-eval fixture.',
    '',
    '  Host-RTL-independent (synthetic System only). SizeInt is a local Int64 alias',
    '  so SizeOf(SizeInt) does not depend on real FPC defines.',
    '  Under the IntrinsicConstEval knob these consts fold target-correctly; with the',
    '  knob off, SizeOf/BitSizeOf are unregistered and the unit fails to resolve',
    '  exactly as stock. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  SizeInt = Int64;',
    '',
    'const',
    '  CInt = SizeOf(Integer);',
    '  CI64 = SizeOf(Int64);',
    '  CByte = SizeOf(Byte);',
    '  CPtr = SizeOf(Pointer);',
    '  CBits = BitSizeOf(Byte);',
    '  CSizeInt = SizeOf(SizeInt);',
    '',
    'implementation',
    '',
    'end.');

  cConstEvalAggregate: array[0..24] of string = (
    'unit constevalaggregate;',
    '',
    '{ SizeOf of an aggregate must DEGRADE.',
    '',
    '  Host-RTL-independent (synthetic System only). SizeOf(TRec) is not determinable',
    '  here, so even with the IntrinsicConstEval knob ON the size helper returns -1',
    '  and the const FAILS to fold (does not resolve) — proving the fold degrades to',
    '  unknown rather than fabricating a wrong number. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TRec = record',
    '    A: Integer;',
    '    B: Integer;',
    '  end;',
    '',
    'const',
    '  CAgg = SizeOf(TRec);',
    '',
    'implementation',
    '',
    'end.');

  cConstEvalTrunc: array[0..21] of string = (
    'unit constevaltrunc;',
    '',
    '{ Trunc/Round const-eval fixture.',
    '',
    '  Host-RTL-independent (synthetic System only). Under the IntrinsicConstEval',
    '  knob these fold with FPC semantics — Trunc toward zero, Round half-to-even',
    '  (banker''s): Trunc(3.9)=3, Trunc(-3.9)=-3, Round(2.5)=2, Round(3.5)=4. With the',
    '  knob off, Trunc/Round are unregistered and the unit fails to resolve. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  CT = Trunc(3.9);',
    '  CTneg = Trunc(-3.9);',
    '  CR = Round(2.5);',
    '  CR2 = Round(3.5);',
    '',
    'implementation',
    '',
    'end.');

  cConstEvalPtr: array[0..29] of string = (
    'unit constevalptr;',
    '',
    '{ Pointer typecast + address-of-const fixture.',
    '',
    '  Host-RTL-independent (synthetic System only; PChar = ^AnsiChar comes from the',
    '  synthetic System). Under the IntrinsicConstEval knob:',
    '    - CP = pointer(-1)  folds the pointer typecast of a constant integer to -1;',
    '    - CPC = PChar(1)    folds to 1;',
    '    - NullPtr: PChar = @GChar is the @-of-typed-const shape: it RESOLVES',
    '      carrying a valid-but-opaque pointer value (the address is never fabricated',
    '      as a number).',
    '  With the knob off, the pointer-cast / address-of paths take the stock raise. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  CP = pointer(-1);',
    '  CPC = PChar(1);',
    '',
    'var',
    '  GChar: AnsiChar;',
    '',
    'const',
    '  NullPtr: PChar = @GChar;',
    '',
    'implementation',
    '',
    'end.');

  cIntrinsicBuiltins: array[0..35] of string = (
    'unit intrinsicbuiltins;',
    '',
    '{ Proves, host-RTL-free on the DEFAULT synthetic-preferred',
    '  path (no real-RTL knobs, no uses clause, no unit search paths):',
    '',
    '    the FPC-3.3.1 intrinsic builtins resolve as base types — the',
    '    sized booleans Boolean8/16/32/64, OleVariant, and the generic',
    '    typed-file type TypedFile — because AddObjFPCBuiltInIdentifiers now',
    '    registers them unconditionally as additive base-type aliases;',
    '',
    '    the superset TPasFileType capability still resolves — a `file of Byte`',
    '    type binds in CreateElement and ComputeElement.',
    '',
    '  Every name here binds through the resolver''s builtin scope alone. Each',
    '  name is registered by the vendored fcl-passrc builtin identifiers. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  PB8  = ^Boolean8;',
    '  PB16 = ^Boolean16;',
    '  PB32 = ^Boolean32;',
    '  PB64 = ^Boolean64;',
    '  POV  = ^OleVariant;',
    '  TByteFile = file of Byte;   // exercises TPasFileType (superset)',
    '',
    'var',
    '  GFlag8: Boolean8;',
    '  GByteFile: TByteFile;',
    '  GTypedFile: TypedFile;      // the generic typed-file intrinsic',
    '',
    'implementation',
    '',
    'end.');

  cLValueVarArg: array[0..60] of string = (
    'unit LValueVarArg;',
    '',
    '{ host-RTL-INDEPENDENT ACCEPT fixture for the untyped var/out',
    '  l-value argument idiom. `Sink` takes an untyped `var` parameter; `OutSink` an',
    '  untyped `out`. `Accepts` passes an indexed STRING l-value (`lText[1]`, the',
    '  `TStream.ReadBuffer(s[1],n)` idiom the [E] units use), plus an array element, a',
    '  record field and a pointer deref — all writable l-values FPC accepts as an',
    '  untyped var/out actual. No uses clause: it resolves via the synthetic minimal',
    '  System with NO host-RTL paths. See lvaluevararg_reject.pas for the reject side. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Accepts;',
    '',
    'implementation',
    '',
    'type',
    '  TRec = record',
    '    F: Longint;',
    '  end;',
    '  PLongint = ^Longint;',
    '',
    'procedure Sink(var aRaw; aLen: Longint);',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure OutSink(out aRaw; aLen: Longint);',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure Accepts;',
    '',
    'var',
    '  lText: string;',
    '  lShort: ShortString;',
    '  lArr: array[0..3] of Longint;',
    '  lRec: TRec;',
    '  lPtr: PLongint;',
    '  lVal: Longint;',
    '',
    'begin',
    '  lText := ''abc'';',
    '  lShort := ''abc'';',
    '  lVal := 0;',
    '  lPtr := @lVal;',
    '  Sink(lText[1], Length(lText));   // AnsiString char-index l-value (the idiom)',
    '  Sink(lShort[1], Length(lShort)); // ShortString char-index l-value',
    '  Sink(lArr[0], 4);                // indexed array l-value',
    '  Sink(lRec.F, 4);                 // record-field l-value',
    '  Sink(lPtr^, 4);                  // pointer-deref l-value',
    '  OutSink(lText[1], 3);            // out param, char-index l-value',
    'end;',
    '',
    '',
    'end.');

  cLValueVarArgReject: array[0..29] of string = (
    'unit LValueVarArg_Reject;',
    '',
    '{ host-RTL-INDEPENDENT REJECT fixture. `Rejects` passes a NON-l-value',
    '  (an integer LITERAL) to an untyped `var` parameter — which FPC rejects and the',
    '  vendored resolver must keep rejecting with "Variable identifier expected". Proves',
    '  the 6-16 broadening did NOT over-accept: it admits writable l-values only. This',
    '  fixture must FAIL resolution (Succeeded=False). }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Rejects;',
    '',
    'implementation',
    '',
    'procedure Sink(var aRaw; aLen: Longint);',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure Rejects;',
    '',
    'begin',
    '  Sink(5, 4);   // literal is not an l-value — must be rejected',
    'end;',
    '',
    '',
    'end.');


procedure TResolverTest.SetUp;

begin
  inherited SetUp;
  // One temp dir per test; fixtures are materialised on demand and removed
  // in TearDown. Fake-tree tests use FreshTempDir/RemoveTree and ignore this.
  FFix := TTempFixtures.Create;
end;


procedure TResolverTest.TearDown;

begin
  FFix.Free;
  FFix := nil;
  inherited TearDown;
end;


function TResolverTest.FindDecl(aModule: TPasModule;
  const aName: string): TPasElement;

var
  lSection: TPasSection;
  i: Integer;
  lDecl: TPasElement;

begin
  Result := nil;
  if aModule = nil then
    Exit;
  lSection := aModule.InterfaceSection;
  if lSection = nil then
    Exit;
  for i := 0 to lSection.Declarations.Count - 1 do
    begin
      lDecl := TPasElement(lSection.Declarations[i]);
      if SameText(lDecl.Name, aName) then
        begin
          Result := lDecl;
          Exit;
        end;
    end;
end;


function TResolverTest.FindDeclInSection(aSection: TPasSection;
  const aName: string): TPasElement;

var
  i: Integer;
  lDecl: TPasElement;

begin
  Result := nil;
  if aSection = nil then
    Exit;
  for i := 0 to aSection.Declarations.Count - 1 do
    begin
      lDecl := TPasElement(aSection.Declarations[i]);
      if SameText(lDecl.Name, aName) then
        begin
          Result := lDecl;
          Exit;
        end;
    end;
end;


function TResolverTest.FindUsedModule(aConsumer: TPasModule;
  const aName: string): TPasModule;

var
  i: Integer;
  lUses: TPasUsesUnit;

begin
  Result := nil;
  if (aConsumer = nil) or (aConsumer.InterfaceSection = nil) then
    Exit;
  for i := 0 to High(aConsumer.InterfaceSection.UsesClause) do
    begin
      lUses := aConsumer.InterfaceSection.UsesClause[i];
      if SameText(lUses.Name, aName) and (lUses.Module is TPasModule) then
        begin
          Result := TPasModule(lUses.Module);
          Exit;
        end;
    end;
end;


function TResolverTest.CountById(const aCollector: TFpSonarIssueCollector;
  const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TResolverTest.AncestryReachesTObject(aType: TPasType): Boolean;

var
  lCls: TPasClassType;
  lGuard: Integer;

begin
  Result := False;
  if not (aType is TPasClassType) then
    Exit;
  lCls := TPasClassType(aType);
  lGuard := 0;
  while (lCls <> nil) and (lGuard < 50) do
    begin
      if SameText(lCls.Name, 'TObject') then
        Exit(True);
      if (lCls.AncestorType <> nil) and (lCls.AncestorType is TPasClassType) then
        lCls := TPasClassType(lCls.AncestorType)
      else
        lCls := nil;
      Inc(lGuard);
    end;
end;


procedure TResolverTest.ResolvableFixtureResolvesLocalType;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;
  lDecl: TPasElement;
  lType: TFpSonarResolvedType;

begin
  lResolver := TFpSonarResolver.Create;
  try
    AssertTrue('resolvable fixture builds',
      lResolver.BuildFor(FFix.Add('resolverfixture.pas', cResolverFixture), cMode, cDefines,
        [], [], lDiag));
    AssertTrue('Succeeded flag set', lResolver.Succeeded);
    AssertNotNull('resolved module non-nil', lResolver.ResolvedModule);

    lDecl := FindDecl(lResolver.ResolvedModule, 'GLocalValue');
    AssertNotNull('found the local var declaration', lDecl);
    AssertTrue('local type resolves', lResolver.TryResolvedType(lDecl, lType));
    AssertNotNull('resolved type element non-nil', lType.TypeEl);
    // The resolver canonicalises the base-type name as 'Longint'.
    // Resolution identity is unchanged.
    AssertEquals('resolved type name', 'Longint', lType.TypeName);
  finally
    lResolver.Free;
  end;
end;


procedure TResolverTest.UntypedQueryReturnsUnresolvedSentinel;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;
  lProc: TPasElement;
  lArg: TPasElement;
  lType: TFpSonarResolvedType;

begin
  lResolver := TFpSonarResolver.Create;
  try
    AssertTrue('resolvable fixture builds',
      lResolver.BuildFor(FFix.Add('resolverfixture.pas', cResolverFixture), cMode, cDefines,
        [], [], lDiag));

    lProc := FindDecl(lResolver.ResolvedModule, 'UntypedSink');
    AssertNotNull('found the untyped-arg procedure', lProc);
    AssertTrue('procedure has an argument',
      TPasProcedure(lProc).ProcType.Args.Count > 0);
    lArg := TPasElement(TPasProcedure(lProc).ProcType.Args[0]);

    // The untyped argument has no concrete type: the query returns the
    // unresolved sentinel and does NOT raise (degrade-to-silence).
    AssertFalse('untyped query is unresolved',
      lResolver.TryResolvedType(lArg, lType));
    AssertNull('sentinel carries no type element', lType.TypeEl);
    AssertEquals('sentinel carries no type name', '', lType.TypeName);
  finally
    lResolver.Free;
  end;
end;


procedure TResolverTest.UnresolvableInputFoldsResolveError;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;
  lCollector: TFpSonarIssueCollector;

begin
  lResolver := TFpSonarResolver.Create;
  lCollector := TFpSonarIssueCollector.Create;
  try
    { absentuses.pas parses, but `uses NoSuchUnitXyz` cannot be located anywhere
      (no synthetic stub, no file on any path) -> resolution degrades to
      dkResolveError. (Re-pointed from resolverbad.pas: SysUtils now binds
      synthetically, so resolverbad resolves clean.) }
    AssertFalse('unresolvable input fails to build',
      lResolver.BuildFor(FFix.Add('absentuses.pas', cAbsentUses), cMode, cDefines,
        [], [], lDiag));
    AssertFalse('Succeeded flag clear', lResolver.Succeeded);
    AssertNull('resolved module is nil', lResolver.ResolvedModule);
    AssertEquals('diagnostic is a resolve error', Ord(dkResolveError),
      Ord(lDiag.Kind));
    AssertTrue('diagnostic carries a message', lDiag.Message <> '');

    // The dkResolveError diagnostic folds into exactly one reserved ResolveError
    // issue through the collector.
    lCollector.CollectDiagnostic(lDiag);
    AssertEquals('exactly one folded issue', 1, lCollector.Count);
    AssertEquals('folded issue is a ResolveError', 'ResolveError',
      lCollector.Issues[0].RuleId);
  finally
    lCollector.Free;
    lResolver.Free;
  end;
end;


procedure TResolverTest.SemRuleRunsWhenFeedPresentAndSkipsWhenAbsent;

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;
  lPresent, lAbsent: TFpSonarIssueCollector;

begin
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  lPresent := TFpSonarIssueCollector.Create;
  lAbsent := TFpSonarIssueCollector.Create;
  try
    lReg.Register(TSynthSemRule.Create(TRuleMetadata.Make('SynthSem', rtSem,
      rfResolver, sevMinor, itCodeSmell, cfHigh, True, '')));

    // Resolvable file -> resolver feed present -> the SEM rule RUNS.
    lEngine.Analyze(FFix.Add('resolverfixture.pas', cResolverFixture), cMode, cDefines,
      lPresent);
    AssertTrue('SEM rule runs when the resolver feed is present',
      CountById(lPresent, 'SynthSem') > 0);

    // Unparseable file -> no resolver built -> the SEM rule is SKIPPED.
    lEngine.Analyze(FFix.Add('faultbad.pas', cFaultBad), cMode, cDefines, lAbsent);
    AssertEquals('SEM rule skipped when the resolver feed is absent', 0,
      CountById(lAbsent, 'SynthSem'));
  finally
    lPresent.Free;
    lAbsent.Free;
    lEngine.Free;
    lReg.Free;
  end;
end;


procedure TResolverTest.CrossUnitDependencyResolvesAndQueries;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;
  lDir: string;
  lDecl: TPasElement;
  lType: TFpSonarResolvedType;

begin
  // The unit search path is the resolved tests/core directory, so DepUnit is
  // locatable for MainUsesDep without any host-RTL checkout.
  FFix.Add('depunit.pas', cDepUnit);
  lDir := ExtractFilePath(FFix.Add('mainusesdep.pas', cMainUsesDep));
  lResolver := TFpSonarResolver.Create;
  try
    AssertTrue('cross-unit fixture builds',
      lResolver.BuildFor(FFix.Add('mainusesdep.pas', cMainUsesDep), cMode, cDefines,
        [lDir], [], lDiag));
    AssertTrue('Succeeded flag set', lResolver.Succeeded);
    AssertNotNull('resolved module non-nil', lResolver.ResolvedModule);

    // The var is typed by the DEPENDENCY's type: the query must return it.
    lDecl := FindDecl(lResolver.ResolvedModule, 'GMainValue');
    AssertNotNull('found the cross-unit-typed var', lDecl);
    AssertTrue('cross-unit type resolves',
      lResolver.TryResolvedType(lDecl, lType));
    AssertNotNull('resolved type element non-nil', lType.TypeEl);
    AssertEquals('resolved cross-unit type name', 'TDepThing', lType.TypeName);
  finally
    lResolver.Free;
  end;
end;


procedure TResolverTest.SystemAliasesResolveViaSyntheticSystem;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;
  lDecl: TPasElement;
  lType: TFpSonarResolvedType;

begin
  // No uses clause and no unit search paths: Integer/Cardinal/TObject bind ONLY
  // through the wrapper's synthetic System (implicit uses), host-RTL-free.
  lResolver := TFpSonarResolver.Create;
  try
    AssertTrue('System-alias fixture builds',
      lResolver.BuildFor(FFix.Add('sysalias.pas', cSysAlias), cMode, cDefines,
        [], [], lDiag));
    AssertTrue('Succeeded flag set', lResolver.Succeeded);

    lDecl := FindDecl(lResolver.ResolvedModule, 'GInt');
    AssertNotNull('found the Integer var', lDecl);
    AssertTrue('Integer alias resolves',
      lResolver.TryResolvedType(lDecl, lType));
    // Integer is a System alias of Longint: it collapses to the base type.
    // (The engine spells the base type 'Longint'; resolution identity is unchanged.)
    AssertEquals('Integer collapses to Longint', 'Longint', lType.TypeName);

    lDecl := FindDecl(lResolver.ResolvedModule, 'GObj');
    AssertNotNull('found the TObject var', lDecl);
    AssertTrue('TObject resolves', lResolver.TryResolvedType(lDecl, lType));
    AssertEquals('TObject resolves to TObject', 'TObject', lType.TypeName);
  finally
    lResolver.Free;
  end;
end;


procedure TResolverTest.MissingDependencyDegradesToResolveError;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;

begin
  { MainUsesDep uses DepUnit, but with NO unit search paths DepUnit cannot be
    found: degrade-to-silence — BuildFor False, one dkResolveError, module nil,
    no crash (degrade rather than fabricate, preserved at wider reach). }
  lResolver := TFpSonarResolver.Create;
  try
    AssertFalse('missing dependency fails to build',
      lResolver.BuildFor(FFix.Add('mainusesdep.pas', cMainUsesDep), cMode, cDefines,
        [], [], lDiag));
    AssertFalse('Succeeded flag clear', lResolver.Succeeded);
    AssertNull('resolved module is nil', lResolver.ResolvedModule);
    AssertEquals('diagnostic is a resolve error', Ord(dkResolveError),
      Ord(lDiag.Kind));
    AssertTrue('diagnostic carries a message', lDiag.Message <> '');
  finally
    lResolver.Free;
  end;
end;


procedure TResolverTest.CircularUsesTerminatesAndDegrades;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;
  lDir: string;

begin
  // CircularA uses CircularB uses CircularA: the cycle guard must terminate and
  // degrade cleanly (no infinite recursion, no crash).
  FFix.Add('circularb.pas', cCircularB);
  lDir := ExtractFilePath(FFix.Add('circulara.pas', cCircularA));
  lResolver := TFpSonarResolver.Create;
  try
    AssertFalse('circular uses degrades (no clean build)',
      lResolver.BuildFor(FFix.Add('circulara.pas', cCircularA), cMode, cDefines,
        [lDir], [], lDiag));
    AssertFalse('Succeeded flag clear', lResolver.Succeeded);
    AssertNull('resolved module is nil', lResolver.ResolvedModule);
    AssertEquals('diagnostic is a resolve error', Ord(dkResolveError),
      Ord(lDiag.Kind));
  finally
    lResolver.Free;
  end;
end;


procedure TResolverTest.EnrichedSystemTypesResolveAndCollapse;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;
  lDecl: TPasElement;
  lType: TFpSonarResolvedType;

begin
  { No uses clause and no unit search paths: the enriched synthetic System binds
    all the referenced families host-RTL-free. Builtin families resolve to
    themselves; the System aliases collapse to their base type. }
  lResolver := TFpSonarResolver.Create;
  try
    AssertTrue('enriched-System fixture builds (no paths)',
      lResolver.BuildFor(FFix.Add('enrichedsystem.pas', cEnrichedSystem), cMode, cDefines,
        [], [], lDiag));
    AssertTrue('Succeeded flag set', lResolver.Succeeded);

    lDecl := FindDecl(lResolver.ResolvedModule, 'GBig');
    AssertNotNull('found the Int64 var', lDecl);
    AssertTrue('Int64 resolves', lResolver.TryResolvedType(lDecl, lType));
    AssertEquals('Int64 resolves to Int64', 'Int64', lType.TypeName);

    lDecl := FindDecl(lResolver.ResolvedModule, 'GReal');
    AssertNotNull('found the Double var', lDecl);
    AssertTrue('Double resolves', lResolver.TryResolvedType(lDecl, lType));
    AssertEquals('Double resolves to Double', 'Double', lType.TypeName);

    lDecl := FindDecl(lResolver.ResolvedModule, 'GText');
    AssertNotNull('found the AnsiString var', lDecl);
    AssertTrue('AnsiString resolves', lResolver.TryResolvedType(lDecl, lType));
    AssertEquals('AnsiString resolves to AnsiString', 'AnsiString',
      lType.TypeName);

    // NativeInt is a System alias of Int64: it collapses to the base type.
    lDecl := FindDecl(lResolver.ResolvedModule, 'GNative');
    AssertNotNull('found the NativeInt var', lDecl);
    AssertTrue('NativeInt resolves', lResolver.TryResolvedType(lDecl, lType));
    AssertEquals('NativeInt collapses to Int64', 'Int64', lType.TypeName);

    // UTF8String is a System alias of AnsiString: it collapses to AnsiString.
    lDecl := FindDecl(lResolver.ResolvedModule, 'GUtf');
    AssertNotNull('found the UTF8String var', lDecl);
    AssertTrue('UTF8String resolves', lResolver.TryResolvedType(lDecl, lType));
    AssertEquals('UTF8String collapses to AnsiString', 'AnsiString',
      lType.TypeName);
  finally
    lResolver.Free;
  end;
end;


procedure TResolverTest.StubResolutionResolvesAndQueries;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;
  lDecl: TPasElement;
  lType: TFpSonarResolvedType;

begin
  { `uses SysUtils, Classes` with NO host-RTL -Fu: the synthetic stubs bind
    path-independently. A query on a stub-typed var returns the stub class,
    whose ancestry is faithful to the real RTL (reaches TObject). }
  lResolver := TFpSonarResolver.Create;
  try
    AssertTrue('stub-using fixture builds with NO host-RTL paths',
      lResolver.BuildFor(FFix.Add('stubuser.pas', cStubUser), cMode, cDefines,
        [], [], lDiag));
    AssertTrue('Succeeded flag set', lResolver.Succeeded);

    lDecl := FindDecl(lResolver.ResolvedModule, 'GErr');
    AssertNotNull('found the Exception var', lDecl);
    AssertTrue('Exception resolves', lResolver.TryResolvedType(lDecl, lType));
    AssertEquals('SysUtils Exception type', 'Exception', lType.TypeName);
    AssertTrue('Exception ancestry reaches TObject',
      AncestryReachesTObject(lType.TypeEl));

    lDecl := FindDecl(lResolver.ResolvedModule, 'GList');
    AssertNotNull('found the TStringList var', lDecl);
    AssertTrue('TStringList resolves', lResolver.TryResolvedType(lDecl, lType));
    AssertEquals('Classes TStringList type', 'TStringList', lType.TypeName);
    AssertTrue('TStringList ancestry reaches TObject',
      AncestryReachesTObject(lType.TypeEl));
  finally
    lResolver.Free;
  end;
end;


procedure TResolverTest.ResolverBadNowResolvesViaSyntheticSysUtils;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;

begin
  { The DELIBERATE FLIP: resolverbad.pas `uses SysUtils` with empty paths
    previously degraded (SysUtils unfindable). Now SysUtils binds via the
    synthetic stub, so the same fixture RESOLVES CLEAN. The degrade test moved to
    absentuses.pas (UnresolvableInputFoldsResolveError). }
  lResolver := TFpSonarResolver.Create;
  try
    AssertTrue('resolverbad now resolves clean via synthetic SysUtils',
      lResolver.BuildFor(FFix.Add('resolverbad.pas', cResolverBad), cMode, cDefines,
        [], [], lDiag));
    AssertTrue('Succeeded flag set', lResolver.Succeeded);
    AssertNotNull('resolved module non-nil', lResolver.ResolvedModule);
  finally
    lResolver.Free;
  end;
end;


procedure TResolverTest.SubstrateStubsResolveHostRtlFree;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;
  lDecl: TPasElement;
  lType: TFpSonarResolvedType;

begin
  { substrate.pas `uses SysUtils, Classes, Math, Types, contnrs, regexpr` and
    references the substrate symbols + the SysUtils path/string helpers in a
    body. With NO host-RTL -Fu paths it resolves CLEAN — proving all six
    synthetic units load+parse+resolve and every referenced identifier
    (Classes.TFPList/TFileStream, Math.TValueSign, System.TStringArray, the new
    Types/contnrs/regexpr types, IncludeTrailingPathDelimiter/StringReplace/… )
    is present and faithful-in-kind. }
  lResolver := TFpSonarResolver.Create;
  try
    AssertTrue('substrate fixture builds with NO host-RTL paths',
      lResolver.BuildFor(FFix.Add('substrate.pas', cSubstrate), cMode, cDefines,
        [], [], lDiag));
    AssertTrue('Succeeded flag set', lResolver.Succeeded);

    // Classes.TFPList resolves and its ancestry is faithful (reaches TObject).
    lDecl := FindDecl(lResolver.ResolvedModule, 'GList');
    AssertNotNull('found the TFPList var', lDecl);
    AssertTrue('TFPList resolves', lResolver.TryResolvedType(lDecl, lType));
    AssertEquals('Classes TFPList type', 'TFPList', lType.TypeName);
    AssertTrue('TFPList ancestry reaches TObject',
      AncestryReachesTObject(lType.TypeEl));

    // TFileStream ancestry is faithful (TFileStream -> THandleStream -> TStream
    // -> TObject).
    lDecl := FindDecl(lResolver.ResolvedModule, 'GStream');
    AssertNotNull('found the TFileStream var', lDecl);
    AssertTrue('TFileStream resolves', lResolver.TryResolvedType(lDecl, lType));
    AssertEquals('Classes TFileStream type', 'TFileStream', lType.TypeName);
    AssertTrue('TFileStream ancestry reaches TObject',
      AncestryReachesTObject(lType.TypeEl));

    // contnrs.TFPObjectList resolves (a new synthetic unit) with faithful ancestry.
    lDecl := FindDecl(lResolver.ResolvedModule, 'GObjs');
    AssertNotNull('found the TFPObjectList var', lDecl);
    AssertTrue('TFPObjectList resolves', lResolver.TryResolvedType(lDecl, lType));
    AssertEquals('contnrs TFPObjectList type', 'TFPObjectList', lType.TypeName);
    AssertTrue('TFPObjectList ancestry reaches TObject',
      AncestryReachesTObject(lType.TypeEl));

    // regexpr.TRegExpr resolves (a new synthetic unit) with faithful ancestry.
    lDecl := FindDecl(lResolver.ResolvedModule, 'GRe');
    AssertNotNull('found the TRegExpr var', lDecl);
    AssertTrue('TRegExpr resolves', lResolver.TryResolvedType(lDecl, lType));
    AssertEquals('regexpr TRegExpr type', 'TRegExpr', lType.TypeName);
    AssertTrue('TRegExpr ancestry reaches TObject',
      AncestryReachesTObject(lType.TypeEl));

    // contnrs.TFPHashList resolves (the second new-unit type) with faithful
    // ancestry (both contnrs list kinds descend TObject).
    lDecl := FindDecl(lResolver.ResolvedModule, 'GHash');
    AssertNotNull('found the TFPHashList var', lDecl);
    AssertTrue('TFPHashList resolves', lResolver.TryResolvedType(lDecl, lType));
    AssertEquals('contnrs TFPHashList type', 'TFPHashList', lType.TypeName);
    AssertTrue('TFPHashList ancestry reaches TObject',
      AncestryReachesTObject(lType.TypeEl));

    // Math.TValueSign resolves. A subrange, so the written name is pinned via
    // NamedTypeName rather than a class ancestry.
    lDecl := FindDecl(lResolver.ResolvedModule, 'GSign');
    AssertNotNull('found the TValueSign var', lDecl);
    AssertTrue('TValueSign resolves', lResolver.TryResolvedType(lDecl, lType));
    AssertEquals('Math TValueSign type', 'TValueSign', lType.NamedTypeName);

    // System.TStringArray resolves (globally visible via implicit uses System).
    lDecl := FindDecl(lResolver.ResolvedModule, 'GArr');
    AssertNotNull('found the TStringArray var', lDecl);
    AssertTrue('TStringArray resolves', lResolver.TryResolvedType(lDecl, lType));
    AssertEquals('System TStringArray type', 'TStringArray', lType.TypeName);

    // Types.TPoint resolves (the third new synthetic unit).
    lDecl := FindDecl(lResolver.ResolvedModule, 'GPoint');
    AssertNotNull('found the TPoint var', lDecl);
    AssertTrue('TPoint resolves', lResolver.TryResolvedType(lDecl, lType));
    AssertEquals('Types TPoint type', 'TPoint', lType.TypeName);
  finally
    lResolver.Free;
  end;
end;


procedure TResolverTest.DependencyInterfaceOnlyResolvesConsumerAndSkipsImpl;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;
  lDir: string;
  lDecl: TPasElement;
  lType: TFpSonarResolvedType;
  lDep: TPasModule;

begin
  { With interface-only enabled on dependency engines, the consumer resolves
    fully (its dep-typed var binds via the dep's interface section scope), while
    the dependency's IMPLEMENTATION is skipped — proven by the impl-only symbol
    being absent (degrade, never invent). }
  FFix.Add('ifaceonlydep.pas', cIfaceOnlyDep);
  lDir := ExtractFilePath(FFix.Add('ifaceonlymain.pas', cIfaceOnlyMain));
  lResolver := TFpSonarResolver.Create;
  try
    lResolver.DependencyInterfaceOnly := True;
    AssertTrue('interface-only cross-unit fixture builds',
      lResolver.BuildFor(FFix.Add('ifaceonlymain.pas', cIfaceOnlyMain), cMode, cDefines,
        [lDir], [], lDiag));
    AssertTrue('Succeeded flag set', lResolver.Succeeded);
    AssertNotNull('resolved module non-nil', lResolver.ResolvedModule);

    // The consumer's dep-typed var resolves: the interface section scope
    // satisfied the uses-clause.
    lDecl := FindDecl(lResolver.ResolvedModule, 'GMainThing');
    AssertNotNull('found the dep-typed var', lDecl);
    AssertTrue('dep interface type resolves',
      lResolver.TryResolvedType(lDecl, lType));
    AssertNotNull('resolved type element non-nil', lType.TypeEl);
    AssertEquals('resolved dep type name', 'TIfaceThing', lType.TypeName);

    // The dependency's implementation was skipped (absence).
    lDep := FindUsedModule(lResolver.ResolvedModule, 'IfaceOnlyDep');
    AssertNotNull('resolved dependency module reachable', lDep);
    AssertNull('dependency implementation section skipped',
      lDep.ImplementationSection);
    AssertNull('impl-only symbol absent under interface-only',
      FindDeclInSection(lDep.ImplementationSection, 'cImplOnlyValue'));
  finally
    lResolver.Free;
  end;
end;


procedure TResolverTest.InterfaceRoutineWithSkippedBodyNotMisreported;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;
  lDir: string;

begin
  { The dependency's interface routine (ComputeThing) has its body in the
    skipped implementation, yet the build must NOT misreport "Forward function
    not resolved". The synthetic System dep (Pos + TObject.Create/Destroy/Free/
    ClassName/ClassType, bodies in its impl) is interface-only here too, so this
    also proves the forward-proc gate covers BOTH CheckPendingForwardProcs arms
    (global procs AND class methods). }
  FFix.Add('ifaceonlydep.pas', cIfaceOnlyDep);
  lDir := ExtractFilePath(FFix.Add('ifaceonlymain.pas', cIfaceOnlyMain));
  lResolver := TFpSonarResolver.Create;
  try
    lResolver.DependencyInterfaceOnly := True;
    AssertTrue('interface routine with skipped body resolves clean',
      lResolver.BuildFor(FFix.Add('ifaceonlymain.pas', cIfaceOnlyMain), cMode, cDefines,
        [lDir], [], lDiag));
    AssertTrue('Succeeded flag set', lResolver.Succeeded);
    AssertNotNull('resolved module non-nil', lResolver.ResolvedModule);
    { No resolver diagnostic was recorded — proves the forward-proc gate
      suppressed the "Forward function not resolved" misreport rather than
      BuildFor merely swallowing it. }
    AssertEquals('no resolver diagnostic recorded', '', lDiag.Message);
  finally
    lResolver.Free;
  end;
end;


procedure TResolverTest.UnitLevelPropertyResolvesUnderInterfaceOnly;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;
  lDir: string;
  lDep: TPasModule;
  lProp: TPasElement;

begin
  { The dependency declares a unit-level / global property (UnitCount) whose
    owner is the unit section, not a class/record. It must
    resolve clean — no EInvalidCast (FinishProperty) and no invalid-scope raise
    (AddProperty) — so the consumer build succeeds. }
  FFix.Add('ifaceonlydep.pas', cIfaceOnlyDep);
  lDir := ExtractFilePath(FFix.Add('ifaceonlymain.pas', cIfaceOnlyMain));
  lResolver := TFpSonarResolver.Create;
  try
    lResolver.DependencyInterfaceOnly := True;
    AssertTrue('unit-level property dependency resolves clean',
      lResolver.BuildFor(FFix.Add('ifaceonlymain.pas', cIfaceOnlyMain), cMode, cDefines,
        [lDir], [], lDiag));
    AssertTrue('Succeeded flag set', lResolver.Succeeded);

    lDep := FindUsedModule(lResolver.ResolvedModule, 'IfaceOnlyDep');
    AssertNotNull('resolved dependency module reachable', lDep);
    lProp := FindDecl(lDep, 'UnitCount');
    AssertNotNull('unit-level property present in interface', lProp);
    AssertTrue('it is a property', lProp is TPasProperty);
  finally
    lResolver.Free;
  end;
end;


procedure TResolverTest.InterfaceOnlyOffIsByteIdenticalDefault;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;
  lDir: string;
  lDecl: TPasElement;
  lType: TFpSonarResolvedType;
  lDep: TPasModule;

begin
  { With DependencyInterfaceOnly OFF (the default), the SAME consumer resolves
    and the dependency resolves in FULL — its implementation is present and the
    impl-only symbol is resolvable. This proves the flag is the only lever and
    the default path is unchanged (the committed corpus stays byte-identical —
    none of it has unit-level properties). }
  FFix.Add('ifaceonlydep.pas', cIfaceOnlyDep);
  lDir := ExtractFilePath(FFix.Add('ifaceonlymain.pas', cIfaceOnlyMain));
  lResolver := TFpSonarResolver.Create;
  try
    AssertFalse('flag defaults off', lResolver.DependencyInterfaceOnly);
    AssertTrue('default full-resolve cross-unit fixture builds',
      lResolver.BuildFor(FFix.Add('ifaceonlymain.pas', cIfaceOnlyMain), cMode, cDefines,
        [lDir], [], lDiag));
    AssertTrue('Succeeded flag set', lResolver.Succeeded);

    lDecl := FindDecl(lResolver.ResolvedModule, 'GMainThing');
    AssertNotNull('found the dep-typed var', lDecl);
    AssertTrue('dep type resolves', lResolver.TryResolvedType(lDecl, lType));
    AssertEquals('resolved dep type name', 'TIfaceThing', lType.TypeName);

    // Full resolve: the dependency's implementation IS present (presence).
    lDep := FindUsedModule(lResolver.ResolvedModule, 'IfaceOnlyDep');
    AssertNotNull('resolved dependency module reachable', lDep);
    AssertNotNull('dependency implementation section present',
      lDep.ImplementationSection);
    AssertNotNull('impl-only symbol resolvable under full resolve',
      FindDeclInSection(lDep.ImplementationSection, 'cImplOnlyValue'));
  finally
    lResolver.Free;
  end;
end;


procedure TResolverTest.SizeOfFoldsTargetCorrect;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;
  lValue: Int64;
  lKnown: Boolean;

begin
  { With the knob on, SizeOf/BitSizeOf fold target-correctly. Scalars are
    target-independent; the pointer-width family is sized from the configured
    target (set to 8 here). Host-RTL-free (synthetic System only, no paths). }
  lResolver := TFpSonarResolver.Create;
  try
    lResolver.IntrinsicConstEval := True;
    lResolver.IntrinsicTargetPointerSize := 8;
    AssertTrue('SizeOf fixture resolves with the knob on',
      lResolver.BuildFor(FFix.Add('constevalsizes.pas', cConstEvalSizes), cMode, cDefines,
        [], [], lDiag));
    AssertTrue('Succeeded flag set', lResolver.Succeeded);

    AssertTrue('CInt resolved', lResolver.TryEvalConstInt('CInt', lValue, lKnown));
    AssertTrue('CInt known', lKnown);
    AssertEquals('SizeOf(Integer)=4', 4, lValue);

    AssertTrue('CI64 resolved', lResolver.TryEvalConstInt('CI64', lValue, lKnown));
    AssertTrue('CI64 known', lKnown);
    AssertEquals('SizeOf(Int64)=8', 8, lValue);

    AssertTrue('CByte resolved', lResolver.TryEvalConstInt('CByte', lValue, lKnown));
    AssertTrue('CByte known', lKnown);
    AssertEquals('SizeOf(Byte)=1', 1, lValue);

    AssertTrue('CBits resolved', lResolver.TryEvalConstInt('CBits', lValue, lKnown));
    AssertTrue('CBits known', lKnown);
    AssertEquals('BitSizeOf(Byte)=8', 8, lValue);

    AssertTrue('CSizeInt resolved',
      lResolver.TryEvalConstInt('CSizeInt', lValue, lKnown));
    AssertTrue('CSizeInt known', lKnown);
    AssertEquals('SizeOf(SizeInt)=8', 8, lValue);
  finally
    lResolver.Free;
  end;
end;


procedure TResolverTest.SizeOfPointerWidthIsTargetDriven;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;
  lValue: Int64;
  lKnown: Boolean;

begin
  { Target-correctness proof: flipping the configured pointer width on the
    SAME fixture changes SizeOf(Pointer) (8 vs 4) — proving the width is sourced
    from the target input, not a hard-coded guess. }
  lResolver := TFpSonarResolver.Create;
  try
    lResolver.IntrinsicConstEval := True;
    lResolver.IntrinsicTargetPointerSize := 8;
    AssertTrue('64-bit-target build resolves',
      lResolver.BuildFor(FFix.Add('constevalsizes.pas', cConstEvalSizes), cMode, cDefines,
        [], [], lDiag));
    AssertTrue('CPtr resolved (64-bit)',
      lResolver.TryEvalConstInt('CPtr', lValue, lKnown));
    AssertTrue('CPtr known (64-bit)', lKnown);
    AssertEquals('SizeOf(Pointer)=8 on a 64-bit target', 8, lValue);
  finally
    lResolver.Free;
  end;

  lResolver := TFpSonarResolver.Create;
  try
    lResolver.IntrinsicConstEval := True;
    lResolver.IntrinsicTargetPointerSize := 4;
    AssertTrue('32-bit-target build resolves',
      lResolver.BuildFor(FFix.Add('constevalsizes.pas', cConstEvalSizes), cMode, cDefines,
        [], [], lDiag));
    AssertTrue('CPtr resolved (32-bit)',
      lResolver.TryEvalConstInt('CPtr', lValue, lKnown));
    AssertTrue('CPtr known (32-bit)', lKnown);
    AssertEquals('SizeOf(Pointer)=4 on a 32-bit target', 4, lValue);
  finally
    lResolver.Free;
  end;
end;


procedure TResolverTest.AggregateSizeOfDegradesToUnknown;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;

begin
  { Negative: SizeOf of an aggregate record is not determinable here, so even
    with the knob ON the size helper returns -1, the const fails to fold and the
    unit does NOT resolve clean — degrade to unknown (absence), never a
    fabricated size. (An untyped const whose initializer does not fold cannot
    infer its type, so resolution fails.) }
  lResolver := TFpSonarResolver.Create;
  try
    lResolver.IntrinsicConstEval := True;
    lResolver.IntrinsicTargetPointerSize := 8;
    AssertFalse('aggregate SizeOf does NOT resolve (degrade)',
      lResolver.BuildFor(FFix.Add('constevalaggregate.pas', cConstEvalAggregate), cMode, cDefines,
        [], [], lDiag));
    AssertFalse('Succeeded flag clear', lResolver.Succeeded);
  finally
    lResolver.Free;
  end;
end;


procedure TResolverTest.TruncRoundPerformRealFloatFold;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;
  lValue: Int64;
  lKnown: Boolean;

begin
  // Trunc truncates toward zero; Round is round-half-to-even (banker's).
  lResolver := TFpSonarResolver.Create;
  try
    lResolver.IntrinsicConstEval := True;
    AssertTrue('Trunc/Round fixture resolves with the knob on',
      lResolver.BuildFor(FFix.Add('constevaltrunc.pas', cConstEvalTrunc), cMode, cDefines,
        [], [], lDiag));
    AssertTrue('Succeeded flag set', lResolver.Succeeded);

    AssertTrue('CT resolved', lResolver.TryEvalConstInt('CT', lValue, lKnown));
    AssertTrue('CT known', lKnown);
    AssertEquals('Trunc(3.9)=3', 3, lValue);

    AssertTrue('CTneg resolved', lResolver.TryEvalConstInt('CTneg', lValue, lKnown));
    AssertTrue('CTneg known', lKnown);
    AssertEquals('Trunc(-3.9)=-3', -3, lValue);

    AssertTrue('CR resolved', lResolver.TryEvalConstInt('CR', lValue, lKnown));
    AssertTrue('CR known', lKnown);
    AssertEquals('Round(2.5)=2 (banker''s)', 2, lValue);

    AssertTrue('CR2 resolved', lResolver.TryEvalConstInt('CR2', lValue, lKnown));
    AssertTrue('CR2 known', lKnown);
    AssertEquals('Round(3.5)=4 (banker''s)', 4, lValue);
  finally
    lResolver.Free;
  end;
end;


procedure TResolverTest.PointerCastFoldsConstIntAndAddressOfResolvesOpaque;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;
  lValue: Int64;
  lKnown: Boolean;

begin
  { A pointer typecast of a constant integer folds to that integer; an
    address-of-typed-const initializer RESOLVES carrying a valid-but-opaque
    pointer value (the address is reported unknown, never a fabricated number). }
  lResolver := TFpSonarResolver.Create;
  try
    lResolver.IntrinsicConstEval := True;
    lResolver.IntrinsicTargetPointerSize := 8;
    AssertTrue('pointer-cast / address-of fixture resolves with the knob on',
      lResolver.BuildFor(FFix.Add('constevalptr.pas', cConstEvalPtr), cMode, cDefines,
        [], [], lDiag));
    AssertTrue('Succeeded flag set', lResolver.Succeeded);

    AssertTrue('CP resolved', lResolver.TryEvalConstInt('CP', lValue, lKnown));
    AssertTrue('CP known', lKnown);
    AssertEquals('pointer(-1) folds to -1', -1, lValue);

    AssertTrue('CPC resolved', lResolver.TryEvalConstInt('CPC', lValue, lKnown));
    AssertTrue('CPC known', lKnown);
    AssertEquals('PChar(1) folds to 1', 1, lValue);

    // The @-of-typed-const RESOLVES but its address value is UNKNOWN (opaque) —
    // never fabricated as a number.
    AssertTrue('NullPtr (address-of) resolved',
      lResolver.TryEvalConstInt('NullPtr', lValue, lKnown));
    AssertFalse('NullPtr address value is unknown (opaque, not invented)', lKnown);
  finally
    lResolver.Free;
  end;
end;


function TResolverTest.FreshTempDir: string;

begin
  // A per-test temp dir under the system temp root; PID-stamped for uniqueness.
  Result := IncludeTrailingPathDelimiter(GetTempDir) + 'fpsonar_realrtl_' +
    IntToStr(GetProcessID);
  RemoveTree(Result);  // clear any leftover from a crashed run
  ForceDirectories(Result);
end;


procedure TResolverTest.WriteUnit(const aDir, aFile, aSource: string);

var
  lStream: TFileStream;

begin
  lStream := TFileStream.Create(IncludeTrailingPathDelimiter(aDir) + aFile,
    fmCreate);
  try
    if aSource <> '' then
      lStream.WriteBuffer(aSource[1], Length(aSource));
  finally
    lStream.Free;
  end;
end;


procedure TResolverTest.RemoveTree(const aDir: string);

var
  lRec: TSearchRec;
  lFull: string;

begin
  if not DirectoryExists(aDir) then
    Exit;
  if FindFirst(IncludeTrailingPathDelimiter(aDir) + '*', faAnyFile, lRec) = 0 then
    try
      repeat
        if (lRec.Name = '.') or (lRec.Name = '..') then
          Continue;
        lFull := IncludeTrailingPathDelimiter(aDir) + lRec.Name;
        if (lRec.Attr and faDirectory) <> 0 then
          RemoveTree(lFull)
        else
          DeleteFile(lFull);
      until FindNext(lRec) <> 0;
    finally
      FindClose(lRec);
    end;
  RemoveDir(aDir);
end;


procedure TResolverTest.RealRtlPreferredDefaultsOff;

var
  lResolver: TFpSonarResolver;

begin
  // Default: the master switch is off out of the box (so the committed
  // synthetic-preferred order + the three knobs stay byte-identical).
  lResolver := TFpSonarResolver.Create;
  try
    AssertFalse('RealRtlPreferred defaults off', lResolver.RealRtlPreferred);
    AssertFalse('DependencyInterfaceOnly defaults off',
      lResolver.DependencyInterfaceOnly);
    AssertFalse('IntrinsicConstEval defaults off', lResolver.IntrinsicConstEval);
    AssertFalse('CondDirectiveEval defaults off', lResolver.CondDirectiveEval);
  finally
    lResolver.Free;
  end;
end;


procedure TResolverTest.RealRtlPrefersRealSourceOverSyntheticStub;

var
  lDir: string;
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;
  lMath: TPasModule;

begin
  { A fake -Fu tree with a unit named 'Math' (which HAS a synthetic stub) carrying
    a marker const, plus a trivial objpas (so the real-RTL implicit uses-chain
    resolves) and a consumer that uses Math. With the mode ON the resolver must
    bind the REAL (fake) Math — the order inversion — not the synthetic stub. }
  lDir := FreshTempDir;
  try
    WriteUnit(lDir, 'objpas.pp',
      'unit objpas;' + LineEnding + 'interface' + LineEnding +
      'implementation' + LineEnding + 'end.' + LineEnding);
    WriteUnit(lDir, 'math.pp',
      'unit Math;' + LineEnding + 'interface' + LineEnding +
      'const cFakeMathMarker = 12345;' + LineEnding +
      'implementation' + LineEnding + 'end.' + LineEnding);
    WriteUnit(lDir, 'mathuser.pas',
      'unit mathuser;' + LineEnding + 'interface' + LineEnding +
      'uses Math;' + LineEnding + 'implementation' + LineEnding + 'end.' +
      LineEnding);

    // Mode ON: real (fake) Math wins.
    lResolver := TFpSonarResolver.Create;
    try
      lResolver.RealRtlPreferred := True;
      lResolver.DependencyInterfaceOnly := True;
      AssertTrue('consumer resolves real-RTL-preferred',
        lResolver.BuildFor(IncludeTrailingPathDelimiter(lDir) + 'mathuser.pas',
          cMode, cDefines, [lDir], [], lDiag));
      lMath := FindUsedModule(lResolver.ResolvedModule, 'Math');
      AssertNotNull('Math dependency resolved', lMath);
      AssertNotNull('REAL (fake) Math won over the synthetic stub',
        FindDecl(lMath, 'cFakeMathMarker'));
    finally
      lResolver.Free;
    end;

    // Mode OFF: synthetic Math wins (no marker), proving the inversion is the lever.
    lResolver := TFpSonarResolver.Create;
    try
      AssertTrue('consumer resolves synthetic-preferred',
        lResolver.BuildFor(IncludeTrailingPathDelimiter(lDir) + 'mathuser.pas',
          cMode, cDefines, [lDir], [], lDiag));
      lMath := FindUsedModule(lResolver.ResolvedModule, 'Math');
      AssertNotNull('Math dependency resolved', lMath);
      AssertNull('synthetic Math won (no fake marker)',
        FindDecl(lMath, 'cFakeMathMarker'));
    finally
      lResolver.Free;
    end;
  finally
    RemoveTree(lDir);
  end;
end;


procedure TResolverTest.RealRtlFallsBackToSyntheticWhenRealFails;

var
  lDir: string;
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;

begin
  { The fake real 'Math' is BROKEN (references an undeclared type), so it fails to
    resolve. The mode must FALL BACK to the synthetic stub, so the consumer still
    resolves — proving the clean real->synthetic fallback. }
  lDir := FreshTempDir;
  try
    WriteUnit(lDir, 'objpas.pp',
      'unit objpas;' + LineEnding + 'interface' + LineEnding +
      'implementation' + LineEnding + 'end.' + LineEnding);
    WriteUnit(lDir, 'math.pp',
      'unit Math;' + LineEnding + 'interface' + LineEnding +
      'var gBroken: TNoSuchTypeAtAll;' + LineEnding +
      'implementation' + LineEnding + 'end.' + LineEnding);
    WriteUnit(lDir, 'mathuser.pas',
      'unit mathuser;' + LineEnding + 'interface' + LineEnding +
      'uses Math;' + LineEnding + 'implementation' + LineEnding + 'end.' +
      LineEnding);

    lResolver := TFpSonarResolver.Create;
    try
      lResolver.RealRtlPreferred := True;
      lResolver.DependencyInterfaceOnly := True;
      AssertTrue('consumer resolves via the synthetic fallback when real Math fails',
        lResolver.BuildFor(IncludeTrailingPathDelimiter(lDir) + 'mathuser.pas',
          cMode, cDefines, [lDir], [], lDiag));
      AssertTrue('Succeeded after fallback', lResolver.Succeeded);
    finally
      lResolver.Free;
    end;
  finally
    RemoveTree(lDir);
  end;
end;


procedure TResolverTest.ObjpasRidesUsesChainUnderRealRtlOnly;

var
  lDir: string;
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;

begin
  { A consumer with NO explicit uses clause. Under the mode, 'objpas' is on the
    implicit uses-chain (NOT a -d define), so it appears as a resolved used module;
    with the mode off it must NOT (byte-identical default). }
  lDir := FreshTempDir;
  try
    WriteUnit(lDir, 'objpas.pp',
      'unit objpas;' + LineEnding + 'interface' + LineEnding +
      'implementation' + LineEnding + 'end.' + LineEnding);
    WriteUnit(lDir, 'plainunit.pas',
      'unit plainunit;' + LineEnding + 'interface' + LineEnding +
      'implementation' + LineEnding + 'end.' + LineEnding);

    lResolver := TFpSonarResolver.Create;
    try
      lResolver.RealRtlPreferred := True;
      lResolver.DependencyInterfaceOnly := True;
      AssertTrue('plain unit resolves under real-RTL',
        lResolver.BuildFor(IncludeTrailingPathDelimiter(lDir) + 'plainunit.pas',
          cMode, cDefines, [lDir], [], lDiag));
      AssertNotNull('objpas is on the implicit uses-chain under real-RTL',
        FindUsedModule(lResolver.ResolvedModule, 'objpas'));
    finally
      lResolver.Free;
    end;

    lResolver := TFpSonarResolver.Create;
    try
      AssertTrue('plain unit resolves synthetic-preferred',
        lResolver.BuildFor(IncludeTrailingPathDelimiter(lDir) + 'plainunit.pas',
          cMode, cDefines, [lDir], [], lDiag));
      AssertNull('objpas is NOT implicit with the mode off',
        FindUsedModule(lResolver.ResolvedModule, 'objpas'));
    finally
      lResolver.Free;
    end;
  finally
    RemoveTree(lDir);
  end;
end;


procedure TResolverTest.RealRtlModeWiresResolverKnobsViaSourceFile;

var
  lSrc: TFpSonarSourceFile;

begin
  { The SourceFile wiring seam: DEEP RESOLUTION is the host-independent DEFAULT
    — DependencyInterfaceOnly + IntrinsicConstEval +
    CondDirectiveEval are wired ON regardless of the real-RTL flag. The --real-rtl
    flag adds ONLY the separate real-source preference (RealRtlPreferred) + its
    target pointer width. A trivial resolvable fixture is enough (the resolver is
    built whenever the parse succeeds). }
  lSrc := TFpSonarSourceFile.Create;
  try
    // --real-rtl ON: deep-resolution knobs + the real-source preference + width.
    lSrc.Analyze(FFix.Add('sysalias.pas', cSysAlias), cMode, cDefines, [], [], True, 8);
    AssertNotNull('resolver built', lSrc.Resolver);
    AssertTrue('RealRtlPreferred wired on under --real-rtl',
      lSrc.Resolver.RealRtlPreferred);
    AssertTrue('DependencyInterfaceOnly wired on',
      lSrc.Resolver.DependencyInterfaceOnly);
    AssertTrue('IntrinsicConstEval wired on', lSrc.Resolver.IntrinsicConstEval);
    AssertTrue('CondDirectiveEval wired on', lSrc.Resolver.CondDirectiveEval);
    AssertEquals('target pointer width wired from the real-RTL target', 8,
      lSrc.Resolver.IntrinsicTargetPointerSize);

    // DEFAULT (no --real-rtl): deep resolution is STILL on host-independently;
    // only the real-source preference is off (and the width stays host default).
    lSrc.Analyze(FFix.Add('sysalias.pas', cSysAlias), cMode, cDefines, [], [], False, 0);
    AssertFalse('RealRtlPreferred OFF in the default path',
      lSrc.Resolver.RealRtlPreferred);
    AssertTrue('DependencyInterfaceOnly ON by default (deep resolution)',
      lSrc.Resolver.DependencyInterfaceOnly);
    AssertTrue('IntrinsicConstEval ON by default (deep resolution)',
      lSrc.Resolver.IntrinsicConstEval);
    AssertTrue('CondDirectiveEval ON by default (deep resolution)',
      lSrc.Resolver.CondDirectiveEval);
    AssertEquals('pointer width stays host default in the default path',
      SizeOf(Pointer), lSrc.Resolver.IntrinsicTargetPointerSize);
  finally
    lSrc.Free;
  end;
end;


procedure TResolverTest.IntrinsicBuiltinsResolveAndFileTypeStillResolves;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;
  lDecl: TPasElement;
  lType: TFpSonarResolvedType;

begin
  { On the DEFAULT synthetic-preferred path (no real-RTL knobs, no uses clause,
    no unit search paths):
     - the intrinsic builtins (Boolean8/16/32/64, OleVariant, TypedFile) are
       registered unconditionally, so a bare parse+resolve reaches them without
       ad-hoc registration;
     - the superset TPasFileType capability still resolves a `file of Byte`
       type. BuildFor succeeding on this fixture requires BOTH. }
  lResolver := TFpSonarResolver.Create;
  try
    AssertFalse('no real-RTL knob is engaged (default path)',
      lResolver.RealRtlPreferred);
    AssertTrue('intrinsic-builtins fixture builds with no paths / no real-RTL',
      lResolver.BuildFor(FFix.Add('intrinsicbuiltins.pas', cIntrinsicBuiltins), cMode, cDefines,
        [], [], lDiag));
    AssertTrue('Succeeded flag set', lResolver.Succeeded);
    AssertNotNull('resolved module non-nil', lResolver.ResolvedModule);

    // The sized-boolean intrinsic resolves as a concrete base type.
    lDecl := FindDecl(lResolver.ResolvedModule, 'GFlag8');
    AssertNotNull('found the Boolean8 var', lDecl);
    AssertTrue('Boolean8 resolves as a base type',
      lResolver.TryResolvedType(lDecl, lType));
    AssertNotNull('Boolean8 resolved type element non-nil', lType.TypeEl);

    // The generic typed-file intrinsic resolves.
    lDecl := FindDecl(lResolver.ResolvedModule, 'GTypedFile');
    AssertNotNull('found the TypedFile var', lDecl);
    AssertTrue('TypedFile intrinsic resolves',
      lResolver.TryResolvedType(lDecl, lType));
    AssertNotNull('TypedFile resolved type element non-nil', lType.TypeEl);

    // A 'file of Byte' type resolves — the TPasFileType capability.
    lDecl := FindDecl(lResolver.ResolvedModule, 'GByteFile');
    AssertNotNull('found the file-typed var', lDecl);
    AssertTrue('file type resolves (TPasFileType capability)',
      lResolver.TryResolvedType(lDecl, lType));
    AssertNotNull('file resolved type element non-nil', lType.TypeEl);
  finally
    lResolver.Free;
  end;
end;


procedure TResolverTest.RealRtlDeclaredCondDirectiveResolvesAndDefaultDegrades;

const
  // A unit whose interface uses {$if declared(...)} over its OWN declarations —
  // host-RTL-independent. declared(TPresent) must fold to 1 (the type is in the
  // live scope by the time the directive is scanned); declared(TAbsentXyzzy) to
  // 0 (genuinely absent). The taken branches leave cPresentSeen + cAbsentMissing.
  cDeclSource =
    'unit declfix;' + LineEnding + 'interface' + LineEnding +
    'type TPresent = Integer;' + LineEnding +
    '{$if declared(TPresent)}' + LineEnding +
    'const cPresentSeen = 1;' + LineEnding +
    '{$else}' + LineEnding +
    'const cPresentMissing = 1;' + LineEnding +
    '{$endif}' + LineEnding +
    '{$if declared(TAbsentXyzzy)}' + LineEnding +
    'const cAbsentSeen = 1;' + LineEnding +
    '{$else}' + LineEnding +
    'const cAbsentMissing = 1;' + LineEnding +
    '{$endif}' + LineEnding +
    'implementation' + LineEnding + 'end.' + LineEnding;

var
  lDir: string;
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;

begin
  lDir := FreshTempDir;
  try
    WriteUnit(lDir, 'objpas.pp',
      'unit objpas;' + LineEnding + 'interface' + LineEnding +
      'implementation' + LineEnding + 'end.' + LineEnding);
    WriteUnit(lDir, 'declfix.pas', cDeclSource);

    // Mode ON: the evaluator answers declared() from the live scope — the unit
    // resolves and the CORRECT branches were taken.
    lResolver := TFpSonarResolver.Create;
    try
      lResolver.RealRtlPreferred := True;
      lResolver.DependencyInterfaceOnly := True;
      lResolver.CondDirectiveEval := True; // the evaluator's own knob
      AssertTrue('declared() fixture resolves with the evaluator wired',
        lResolver.BuildFor(IncludeTrailingPathDelimiter(lDir) + 'declfix.pas',
          cMode, cDefines, [lDir], [], lDiag));
      AssertNotNull('declared(present) -> 1 branch taken',
        FindDecl(lResolver.ResolvedModule, 'cPresentSeen'));
      AssertNull('declared(present) else-branch NOT taken',
        FindDecl(lResolver.ResolvedModule, 'cPresentMissing'));
      AssertNotNull('declared(absent) -> 0 branch taken',
        FindDecl(lResolver.ResolvedModule, 'cAbsentMissing'));
      AssertNull('declared(absent) then-branch NOT taken',
        FindDecl(lResolver.ResolvedModule, 'cAbsentSeen'));
    finally
      lResolver.Free;
    end;

    { Evaluator OFF (a bare-constructed resolver — CondDirectiveEval defaults
      False as the mechanism-level default): OnEvalFunction is unassigned, so the
      scanner cannot answer declared() and the unit fails to build. Proves the
      knob (not RealRtlPreferred) is what wires the evaluator. NOTE: the ANALYSIS
      default now turns this knob ON at the SourceFile seam — this is the
      resolver mechanism's default, not the analysis default. }
    lResolver := TFpSonarResolver.Create;
    try
      AssertFalse('declared() cond-function is NOT answered with the knob off',
        lResolver.BuildFor(IncludeTrailingPathDelimiter(lDir) + 'declfix.pas',
          cMode, cDefines, [lDir], [], lDiag));
    finally
      lResolver.Free;
    end;
  finally
    RemoveTree(lDir);
  end;
end;


procedure TResolverTest.RealRtlUnknownCondFunctionDegradesUnit;

const
  { A cond-function that is neither declared() nor sizeof() must NOT be answered
    (never a fabricated branch): the evaluator returns False, the scanner raises,
    the unit degrades. Proven even with the mode ON. }
  cUnknownSource =
    'unit unkfix;' + LineEnding + 'interface' + LineEnding +
    '{$if frobnicate(x)}' + LineEnding +
    'const cA = 1;' + LineEnding +
    '{$else}' + LineEnding +
    'const cB = 1;' + LineEnding +
    '{$endif}' + LineEnding +
    'implementation' + LineEnding + 'end.' + LineEnding;

var
  lDir: string;
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;

begin
  lDir := FreshTempDir;
  try
    WriteUnit(lDir, 'objpas.pp',
      'unit objpas;' + LineEnding + 'interface' + LineEnding +
      'implementation' + LineEnding + 'end.' + LineEnding);
    WriteUnit(lDir, 'unkfix.pas', cUnknownSource);

    lResolver := TFpSonarResolver.Create;
    try
      lResolver.RealRtlPreferred := True;
      lResolver.DependencyInterfaceOnly := True;
      lResolver.CondDirectiveEval := True; // the evaluator's own knob
      AssertFalse('an unknown cond-function fails the unit (never fabricated)',
        lResolver.BuildFor(IncludeTrailingPathDelimiter(lDir) + 'unkfix.pas',
          cMode, cDefines, [lDir], [], lDiag));
    finally
      lResolver.Free;
    end;
  finally
    RemoveTree(lDir);
  end;
end;


procedure TResolverTest.RealRtlSizeOfCondDirectiveFoldsTargetCorrect;

const
  // {$if sizeof(T)=N} folded from the live scope: AnsiChar is 1 byte
  // (target-independent) and a pointer type is the TARGET width (8 here) — so
  // sizeof() is target-driven, not host-driven. Taken branches: cCharOne +
  // cPtrEight.
  cSizeSource =
    'unit sizefix;' + LineEnding + 'interface' + LineEnding +
    'type TCharAlias = AnsiChar; PInt = ^Integer;' + LineEnding +
    '{$if sizeof(TCharAlias)=1}' + LineEnding +
    'const cCharOne = 1;' + LineEnding +
    '{$else}' + LineEnding +
    'const cCharOther = 1;' + LineEnding +
    '{$endif}' + LineEnding +
    '{$if sizeof(PInt)=8}' + LineEnding +
    'const cPtrEight = 1;' + LineEnding +
    '{$else}' + LineEnding +
    'const cPtrOther = 1;' + LineEnding +
    '{$endif}' + LineEnding +
    'implementation' + LineEnding + 'end.' + LineEnding;

var
  lDir: string;
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;

begin
  lDir := FreshTempDir;
  try
    WriteUnit(lDir, 'objpas.pp',
      'unit objpas;' + LineEnding + 'interface' + LineEnding +
      'implementation' + LineEnding + 'end.' + LineEnding);
    WriteUnit(lDir, 'sizefix.pas', cSizeSource);

    // The full real-RTL knob set (mirrors the SourceFile seam): the const-eval
    // machinery + the target pointer width feed the target-correct size fold.
    lResolver := TFpSonarResolver.Create;
    try
      lResolver.RealRtlPreferred := True;
      lResolver.DependencyInterfaceOnly := True;
      lResolver.IntrinsicConstEval := True;
      lResolver.CondDirectiveEval := True; // the evaluator's own knob
      lResolver.IntrinsicTargetPointerSize := 8;
      AssertTrue('sizeof() fixture resolves with the evaluator wired',
        lResolver.BuildFor(IncludeTrailingPathDelimiter(lDir) + 'sizefix.pas',
          cMode, cDefines, [lDir], [], lDiag));
      AssertNotNull('sizeof(AnsiChar)=1 branch taken',
        FindDecl(lResolver.ResolvedModule, 'cCharOne'));
      AssertNull('sizeof(AnsiChar) else-branch NOT taken',
        FindDecl(lResolver.ResolvedModule, 'cCharOther'));
      AssertNotNull('sizeof(pointer)=8 (target width) branch taken',
        FindDecl(lResolver.ResolvedModule, 'cPtrEight'));
      AssertNull('sizeof(pointer) else-branch NOT taken',
        FindDecl(lResolver.ResolvedModule, 'cPtrOther'));
    finally
      lResolver.Free;
    end;
  finally
    RemoveTree(lDir);
  end;
end;


procedure TResolverTest.DeepResolutionDefaultResolvesInterfaceOnlyDepViaSourceFile;

const
  { The dependency's INTERFACE is clean, but its IMPLEMENTATION references a
    completely undeclared type — a hard resolve failure IF the implementation
    were parsed. Under the default (deps interface-only), the impl is skipped,
    so the consumer still resolves. Host-RTL-independent (no uses beyond the
    implicit System, over its own declarations). }
  cDepSource =
    'unit deepdep;' + LineEnding + 'interface' + LineEnding +
    'type TDeepThing = class' + LineEnding + 'end;' + LineEnding +
    'implementation' + LineEnding +
    'var gBad: TCompletelyUndeclaredType;' + LineEnding +
    'end.' + LineEnding;
  cMainSource =
    'unit deepmain;' + LineEnding + 'interface' + LineEnding +
    'uses deepdep;' + LineEnding +
    'var GThing: TDeepThing;' + LineEnding +
    'implementation' + LineEnding + 'end.' + LineEnding;

var
  lDir: string;
  lSrc: TFpSonarSourceFile;
  lDep: TPasModule;

begin
  lDir := FreshTempDir;
  try
    WriteUnit(lDir, 'deepdep.pas', cDepSource);
    WriteUnit(lDir, 'deepmain.pas', cMainSource);

    { The DEFAULT 5-arg Analyze (no --real-rtl): deep resolution is on, so deps
      resolve interface-only and the consumer resolves despite the dep's broken
      implementation. }
    lSrc := TFpSonarSourceFile.Create;
    try
      lSrc.Analyze(IncludeTrailingPathDelimiter(lDir) + 'deepmain.pas',
        cMode, cDefines, [lDir], [lDir]);
      AssertNotNull('resolver built', lSrc.Resolver);
      AssertTrue('consumer resolves by default despite a broken dep impl',
        lSrc.Resolver.Succeeded);

      { The dependency was resolved interface-only — its implementation (the one
        with the undeclared type) was skipped, which is exactly why the build
        succeeded. }
      lDep := FindUsedModule(lSrc.Resolver.ResolvedModule, 'deepdep');
      AssertNotNull('resolved dependency module reachable', lDep);
      AssertNull('dep implementation skipped under the interface-only default',
        lDep.ImplementationSection);
    finally
      lSrc.Free;
    end;
  finally
    RemoveTree(lDir);
  end;
end;


// --- ppudump-stub seam (host-RTL-INDEPENDENT hand-authored stub) ---

const
  { A small, FAITHFUL, self-contained FLAT interface stub — exactly the shape
    the ppustub generator emits (class with a typed function member + a scalar
    property backed by a private stub field; `uses` only System, TObject via the
    synthetic System). NOT the committed generated set. }
  cFlatStubSource =
    'unit depflat;' + LineEnding +
    '{$mode objfpc}{$H+}' + LineEnding +
    'interface' + LineEnding +
    'type' + LineEnding +
    '  TFlatKind = (fkAlpha, fkBeta);' + LineEnding +
    '  TCrux = class(TObject)' + LineEnding +
    '  private' + LineEnding +
    '    FPpuStub_Count: LongInt;' + LineEnding +
    '  public' + LineEnding +
    '    function Kind: TFlatKind;' + LineEnding +
    '    property Count: LongInt read FPpuStub_Count write FPpuStub_Count;' + LineEnding +
    '  end;' + LineEnding +
    'implementation' + LineEnding +
    'end.' + LineEnding;

  // A consumer that uses the flat stub and does member access on BOTH members.
  cCruxUserSource =
    'unit cruxuser;' + LineEnding +
    '{$mode objfpc}{$H+}' + LineEnding +
    'interface' + LineEnding +
    'uses depflat;' + LineEnding +
    'implementation' + LineEnding +
    'procedure UseIt;' + LineEnding +
    'var lx: TCrux;' + LineEnding +
    'begin' + LineEnding +
    '  lx := nil;' + LineEnding +
    '  lx.Count := 7;' + LineEnding +
    '  if lx.Kind = fkAlpha then ;' + LineEnding +
    'end;' + LineEnding +
    'end.' + LineEnding;

  // A consumer that touches a member ABSENT from the stub (an omitted shape).
  cCruxBadSource =
    'unit cruxbad;' + LineEnding +
    '{$mode objfpc}{$H+}' + LineEnding +
    'interface' + LineEnding +
    'uses depflat;' + LineEnding +
    'implementation' + LineEnding +
    'procedure UseIt;' + LineEnding +
    'var lx: TCrux;' + LineEnding +
    'begin' + LineEnding +
    '  lx := nil;' + LineEnding +
    '  lx.NoSuchMember(3);' + LineEnding +
    'end;' + LineEnding +
    'end.' + LineEnding;

procedure TResolverTest.PpuAutoDetectDefaultsOff;

var
  lResolver: TFpSonarResolver;

begin
  // Default: auto-detect is OFF out of the box, so a bare resolver is
  // byte-identical to the synthetic-only path (dependency resolution unchanged).
  lResolver := TFpSonarResolver.Create;
  try
    AssertFalse('PpuAutoDetect defaults off (== --synthetic-only)',
      lResolver.PpuAutoDetect);
  finally
    lResolver.Free;
  end;
end;


procedure TResolverTest.PpuStubFlatStubResolvesConsumerAndBindsMemberTypes;

  function FindMember(aCls: TPasClassType; const aName: string): TPasElement;
  var
    j: Integer;
  begin
    Result := nil;
    for j := 0 to aCls.Members.Count - 1 do
      if SameText(TPasElement(aCls.Members[j]).Name, aName) then
        Exit(TPasElement(aCls.Members[j]));
  end;

var
  lDir: string;
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;
  lDep: TPasModule;
  lCrux: TPasElement;
  lMember: TPasElement;
  lType: TFpSonarResolvedType;

begin
  { With the stub dir set, the consumer `uses depflat` resolves against the flat
    stub AND each member access binds to the stub decl with the RIGHT type
    (property Count -> LongInt; function Kind -> TFlatKind). No host RTL, no
    unit search paths — depflat is resolvable ONLY via the ppu-stub dir. }
  lDir := FreshTempDir;
  try
    WriteUnit(lDir, 'depflat.pas', cFlatStubSource);
    WriteUnit(lDir, 'cruxuser.pas', cCruxUserSource);

    lResolver := TFpSonarResolver.Create;
    try
      lResolver.DependencyInterfaceOnly := True;
      lResolver.IntrinsicConstEval := True;
      lResolver.CondDirectiveEval := True;
      AssertTrue('consumer resolves via the generator-shaped flat stub on -Fu',
        lResolver.BuildFor(IncludeTrailingPathDelimiter(lDir) + 'cruxuser.pas',
          cMode, cDefines, [lDir], [], lDiag));
      AssertTrue('Succeeded flag set', lResolver.Succeeded);

      lDep := FindUsedModule(lResolver.ResolvedModule, 'depflat');
      AssertNotNull('depflat dependency resolved against the stub', lDep);
      lCrux := FindDecl(lDep, 'TCrux');
      AssertNotNull('TCrux found in the stub', lCrux);
      AssertTrue('TCrux is a class', lCrux is TPasClassType);

      lMember := FindMember(TPasClassType(lCrux), 'Count');
      AssertNotNull('Count property found', lMember);
      AssertTrue('Count property type resolves',
        lResolver.TryResolvedType(lMember, lType));
      AssertEquals('Count binds to Longint', 'Longint', lType.TypeName);

      lMember := FindMember(TPasClassType(lCrux), 'Kind');
      AssertNotNull('Kind function found', lMember);
      AssertTrue('Kind is a function', lMember is TPasFunction);
      { The function's declared result type binds to the faithful stub enum
        (TryResolvedType resolves call/member expressions, not decls, so read the
        result type off the resolved function AST directly). }
      AssertNotNull('Kind has a result type',
        TPasFunction(lMember).FuncType.ResultEl.ResultType);
      AssertEquals('Kind result binds to TFlatKind', 'TFlatKind',
        TPasFunction(lMember).FuncType.ResultEl.ResultType.Name);
    finally
      lResolver.Free;
    end;
  finally
    RemoveTree(lDir);
  end;
end;


procedure TResolverTest.PpuStubOmittedMemberDegradesAndOffIsByteIdentical;

var
  lDir: string;
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;

begin
  lDir := FreshTempDir;
  try
    WriteUnit(lDir, 'depflat.pas', cFlatStubSource);
    WriteUnit(lDir, 'cruxuser.pas', cCruxUserSource);
    WriteUnit(lDir, 'cruxbad.pas', cCruxBadSource);

    { Flag OFF is byte-identical: with no ppu-stub dir and no unit search paths,
      `uses depflat` is unresolvable, so the same consumer that resolved above
      now degrades exactly as it does today (dep not found). }
    lResolver := TFpSonarResolver.Create;
    try
      lResolver.DependencyInterfaceOnly := True;
      lResolver.IntrinsicConstEval := True;
      lResolver.CondDirectiveEval := True;
      // No stub on -Fu (off): the dep is unfindable, exactly as --synthetic-only.
      AssertFalse('flag OFF: consumer degrades (dep unfindable) — byte-identical',
        lResolver.BuildFor(IncludeTrailingPathDelimiter(lDir) + 'cruxuser.pas',
          cMode, cDefines, [], [], lDiag));
      AssertFalse('Succeeded false when off', lResolver.Succeeded);
      AssertNull('ResolvedModule nil when off', lResolver.ResolvedModule);
    finally
      lResolver.Free;
    end;

    { An OMITTED shape degrades to SILENCE, never a wrong binding: with the flag
      ON, a consumer touching a member ABSENT from the faithful stub fails the
      whole unit (Succeeded=False, module nil) rather than binding to a
      fabricated member. }
    lResolver := TFpSonarResolver.Create;
    try
      lResolver.DependencyInterfaceOnly := True;
      lResolver.IntrinsicConstEval := True;
      lResolver.CondDirectiveEval := True;
      AssertFalse('absent member degrades the whole unit to silence',
        lResolver.BuildFor(IncludeTrailingPathDelimiter(lDir) + 'cruxbad.pas',
          cMode, cDefines, [lDir], [], lDiag));
      AssertNull('ResolvedModule nil on the omitted-member degrade',
        lResolver.ResolvedModule);
    finally
      lResolver.Free;
    end;
  finally
    RemoveTree(lDir);
  end;
end;


procedure TResolverTest.PpuStubTypeHelperMethodBindsAndDegrades;

  function FindMember(aCls: TPasClassType; const aName: string): TPasElement;
  var
    j: Integer;
  begin
    Result := nil;
    for j := 0 to aCls.Members.Count - 1 do
      if SameText(TPasElement(aCls.Members[j]).Name, aName) then
        Exit(TPasElement(aCls.Members[j]));
  end;

const
  { A small, FAITHFUL, self-contained FLAT stub — exactly the shape the ppustub
    generator emits for a `type helper`: a helper for AnsiString whose method
    returns a named stub array type. NOT the committed generated set. }
  cHelperStubSource =
    'unit helperdep;' + LineEnding +
    '{$mode objfpc}{$H+}{$modeswitch typehelpers}' + LineEnding +
    'interface' + LineEnding +
    'type' + LineEnding +
    '  TFlatArray = array of AnsiString;' + LineEnding +
    '  TFlatHelper = type helper for AnsiString' + LineEnding +
    '    function Chop(const aSep: array of Char): TFlatArray;' + LineEnding +
    '  end;' + LineEnding +
    'implementation' + LineEnding +
    'end.' + LineEnding;

  // A consumer that does a helper-method member access on a string value.
  cHelperUserSource =
    'unit helperuser;' + LineEnding +
    '{$mode objfpc}{$H+}{$modeswitch typehelpers}' + LineEnding +
    'interface' + LineEnding +
    'uses helperdep;' + LineEnding +
    'implementation' + LineEnding +
    'procedure UseIt;' + LineEnding +
    'var ls: AnsiString; la: TFlatArray;' + LineEnding +
    'begin' + LineEnding +
    '  ls := ''a;b;c'';' + LineEnding +
    '  la := ls.Chop(['';'']);' + LineEnding +
    '  if Length(la) > 0 then ;' + LineEnding +
    'end;' + LineEnding +
    'end.' + LineEnding;

  // A consumer that calls a helper method ABSENT from the stub (an omitted shape).
  cHelperBadSource =
    'unit helperbad;' + LineEnding +
    '{$mode objfpc}{$H+}{$modeswitch typehelpers}' + LineEnding +
    'interface' + LineEnding +
    'uses helperdep;' + LineEnding +
    'implementation' + LineEnding +
    'procedure UseIt;' + LineEnding +
    'var ls: AnsiString;' + LineEnding +
    'begin' + LineEnding +
    '  ls := '''';' + LineEnding +
    '  if Length(ls.NoSuchHelperMethod) > 0 then ;' + LineEnding +
    'end;' + LineEnding +
    'end.' + LineEnding;

var
  lDir: string;
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;
  lDep: TPasModule;
  lHelper: TPasElement;
  lMember: TPasElement;

begin
  lDir := FreshTempDir;
  try
    WriteUnit(lDir, 'helperdep.pas', cHelperStubSource);
    WriteUnit(lDir, 'helperuser.pas', cHelperUserSource);
    WriteUnit(lDir, 'helperbad.pas', cHelperBadSource);

    { With the stub dir set, the consumer `uses helperdep` resolves and the
      TYPE-HELPER method access `ls.Chop([';'])` binds to the stub decl with the
      right result type (TFlatArray). No host RTL, no unit search paths. }
    lResolver := TFpSonarResolver.Create;
    try
      lResolver.DependencyInterfaceOnly := True;
      lResolver.IntrinsicConstEval := True;
      lResolver.CondDirectiveEval := True;
      AssertTrue('consumer resolves; helper-method access binds',
        lResolver.BuildFor(IncludeTrailingPathDelimiter(lDir) + 'helperuser.pas',
          cMode, cDefines, [lDir], [], lDiag));
      AssertTrue('Succeeded flag set', lResolver.Succeeded);

      lDep := FindUsedModule(lResolver.ResolvedModule, 'helperdep');
      AssertNotNull('helperdep dependency resolved against the stub', lDep);
      lHelper := FindDecl(lDep, 'TFlatHelper');
      AssertNotNull('TFlatHelper found in the stub', lHelper);
      AssertTrue('TFlatHelper is a class-type (type helper)',
        lHelper is TPasClassType);
      AssertEquals('TFlatHelper is a type helper', Ord(okTypeHelper),
        Ord(TPasClassType(lHelper).ObjKind));
      lMember := FindMember(TPasClassType(lHelper), 'Chop');
      AssertNotNull('Chop helper method found', lMember);
      AssertTrue('Chop is a function', lMember is TPasFunction);
      AssertNotNull('Chop has a result type',
        TPasFunction(lMember).FuncType.ResultEl.ResultType);
      AssertEquals('Chop result binds to TFlatArray', 'TFlatArray',
        TPasFunction(lMember).FuncType.ResultEl.ResultType.Name);
    finally
      lResolver.Free;
    end;

    { Omitted shape degrades to SILENCE: a consumer calling a helper method
      ABSENT from the faithful stub fails the whole unit rather than binding to a
      fabricated member. }
    lResolver := TFpSonarResolver.Create;
    try
      lResolver.DependencyInterfaceOnly := True;
      lResolver.IntrinsicConstEval := True;
      lResolver.CondDirectiveEval := True;
      AssertFalse('absent helper method degrades the whole unit to silence',
        lResolver.BuildFor(IncludeTrailingPathDelimiter(lDir) + 'helperbad.pas',
          cMode, cDefines, [lDir], [], lDiag));
      AssertNull('ResolvedModule nil on the omitted-member degrade',
        lResolver.ResolvedModule);
    finally
      lResolver.Free;
    end;

    // Flag OFF is byte-identical: with no ppu-stub dir and no search paths,
    // 'uses helperdep' is unresolvable, so the consumer degrades exactly as today.
    lResolver := TFpSonarResolver.Create;
    try
      lResolver.DependencyInterfaceOnly := True;
      lResolver.IntrinsicConstEval := True;
      lResolver.CondDirectiveEval := True;
      // No stub on -Fu (off): the dep is unfindable, exactly as --synthetic-only.
      AssertFalse('flag OFF: consumer degrades (dep unfindable) — byte-identical',
        lResolver.BuildFor(IncludeTrailingPathDelimiter(lDir) + 'helperuser.pas',
          cMode, cDefines, [], [], lDiag));
      AssertNull('ResolvedModule nil when off', lResolver.ResolvedModule);
    finally
      lResolver.Free;
    end;
  finally
    RemoveTree(lDir);
  end;
end;


procedure TResolverTest.UntypedVarOutAcceptsLValueAndRejectsNonLValue;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;

begin
  { ACCEPT: a unit passing a writable string char-index (`lText[1]`), an array
    element, a record field and a pointer deref to an untyped var/out param
    resolves clean — matching FPC (the ReadBuffer(s[1],n) idiom). Before the
    patch the char-index actual raised "Variable identifier expected". }
  lResolver := TFpSonarResolver.Create;
  try
    AssertTrue('writable l-value untyped var/out actuals resolve',
      lResolver.BuildFor(FFix.Add('lvaluevararg.pas', cLValueVarArg), cMode, cDefines,
        [], [], lDiag));
    AssertTrue('Succeeded flag set on accept fixture', lResolver.Succeeded);
    AssertNotNull('resolved module non-nil', lResolver.ResolvedModule);
  finally
    lResolver.Free;
  end;

  { REJECT (no over-accept): a NON-l-value untyped var actual — an integer
    literal — still fails resolution with "Variable identifier expected",
    exactly as before the patch. Guards against the broadening admitting
    non-l-values. }
  lResolver := TFpSonarResolver.Create;
  try
    AssertFalse('literal untyped var actual is still rejected',
      lResolver.BuildFor(FFix.Add('lvaluevararg_reject.pas', cLValueVarArgReject), cMode, cDefines,
        [], [], lDiag));
    AssertFalse('Succeeded flag clear on reject fixture', lResolver.Succeeded);
    AssertEquals('reject diagnostic is a resolve error', Ord(dkResolveError),
      Ord(lDiag.Kind));
    AssertTrue('reject reports the variable-identifier wall',
      Pos('Variable identifier expected', lDiag.Message) > 0);
  finally
    lResolver.Free;
  end;
end;


procedure TResolverTest.PpuStubBroadShapesBindAndOmittedDegrades;

  function FindMember(aCls: TPasClassType; const aName: string): TPasElement;
  var
    j: Integer;
  begin
    Result := nil;
    for j := 0 to aCls.Members.Count - 1 do
      if SameText(TPasElement(aCls.Members[j]).Name, aName) then
        Exit(TPasElement(aCls.Members[j]));
  end;

const
  { A small, FAITHFUL, self-contained FLAT stub carrying the BROAD shapes the
    generator handles: a record type, a set type and a procedural type, plus a
    class exposing them (a record-typed property, a set-typed property, a
    proc-type-returning function). NOT the committed generated set. }
  cBroadStubSource =
    'unit depbroad;' + LineEnding +
    '{$mode objfpc}{$H+}' + LineEnding +
    'interface' + LineEnding +
    'type' + LineEnding +
    '  TStubPoint = record' + LineEnding +
    '    X: LongInt;' + LineEnding +
    '    Y: LongInt;' + LineEnding +
    '  end;' + LineEnding +
    '  TStubFlag = (sfAlpha, sfBeta, sfGamma);' + LineEnding +
    '  TStubFlags = set of TStubFlag;' + LineEnding +
    '  TStubCompare = function(aLeft, aRight: LongInt): LongInt;' + LineEnding +
    '  TBroad = class(TObject)' + LineEnding +
    '  private' + LineEnding +
    '    FPpuStub_Pt: TStubPoint;' + LineEnding +
    '    FPpuStub_Flags: TStubFlags;' + LineEnding +
    '  public' + LineEnding +
    '    function Comparer: TStubCompare;' + LineEnding +
    '    property Pt: TStubPoint read FPpuStub_Pt write FPpuStub_Pt;' + LineEnding +
    '    property Flags: TStubFlags read FPpuStub_Flags write FPpuStub_Flags;' + LineEnding +
    '  end;' + LineEnding +
    'implementation' + LineEnding +
    'end.' + LineEnding;

  // A consumer that does member access on the record + set + proc-type members.
  cBroadUserSource =
    'unit broaduser;' + LineEnding +
    '{$mode objfpc}{$H+}' + LineEnding +
    'interface' + LineEnding +
    'uses depbroad;' + LineEnding +
    'implementation' + LineEnding +
    'procedure UseIt;' + LineEnding +
    'var lx: TBroad; lp: TStubPoint; lf: TStubFlags;' + LineEnding +
    'begin' + LineEnding +
    '  lx := nil;' + LineEnding +
    '  lp := lx.Pt;' + LineEnding +
    '  lp.X := 3;' + LineEnding +
    '  lf := lx.Flags;' + LineEnding +
    '  if sfAlpha in lf then ;' + LineEnding +
    'end;' + LineEnding +
    'end.' + LineEnding;

  // A consumer touching a shape ABSENT from the stub (an omitted generic-style
  // member the faithful stub degraded away).
  cBroadBadSource =
    'unit broadbad;' + LineEnding +
    '{$mode objfpc}{$H+}' + LineEnding +
    'interface' + LineEnding +
    'uses depbroad;' + LineEnding +
    'implementation' + LineEnding +
    'procedure UseIt;' + LineEnding +
    'var lx: TBroad;' + LineEnding +
    'begin' + LineEnding +
    '  lx := nil;' + LineEnding +
    '  lx.NoSuchGenericMethod(1);' + LineEnding +
    'end;' + LineEnding +
    'end.' + LineEnding;

var
  lDir: string;
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;
  lDep: TPasModule;
  lCls: TPasElement;
  lMember: TPasElement;
  lType: TFpSonarResolvedType;

begin
  lDir := FreshTempDir;
  try
    WriteUnit(lDir, 'depbroad.pas', cBroadStubSource);
    WriteUnit(lDir, 'broaduser.pas', cBroadUserSource);
    WriteUnit(lDir, 'broadbad.pas', cBroadBadSource);

    { (a) broad shapes bind: the consumer resolves and each member access binds
      to its faithful stub type — record (Pt -> TStubPoint), set (Flags ->
      TStubFlags), proc-type (Comparer -> TStubCompare result). No host RTL, no
      unit search paths — depbroad is resolvable ONLY via the ppu-stub dir. }
    lResolver := TFpSonarResolver.Create;
    try
      lResolver.DependencyInterfaceOnly := True;
      lResolver.IntrinsicConstEval := True;
      lResolver.CondDirectiveEval := True;
      AssertTrue('consumer resolves; broad-shape member access binds',
        lResolver.BuildFor(IncludeTrailingPathDelimiter(lDir) + 'broaduser.pas',
          cMode, cDefines, [lDir], [], lDiag));
      AssertTrue('Succeeded flag set', lResolver.Succeeded);

      lDep := FindUsedModule(lResolver.ResolvedModule, 'depbroad');
      AssertNotNull('depbroad dependency resolved against the stub', lDep);
      lCls := FindDecl(lDep, 'TBroad');
      AssertNotNull('TBroad found in the stub', lCls);

      lMember := FindMember(TPasClassType(lCls), 'Pt');
      AssertNotNull('Pt property found', lMember);
      AssertTrue('Pt property type resolves',
        lResolver.TryResolvedType(lMember, lType));
      AssertEquals('Pt binds to the record TStubPoint', 'TStubPoint',
        lType.TypeName);

      lMember := FindMember(TPasClassType(lCls), 'Flags');
      AssertNotNull('Flags property found', lMember);
      AssertTrue('Flags property (a set) type resolves',
        lResolver.TryResolvedType(lMember, lType));
      { The resolver has no distinct set kind: a `set of TStubFlag` property
        resolves with its ELEMENT enum name. Binding to TStubFlag proves BOTH the
        set decl and its element enum came faithfully from the stub. }
      AssertEquals('Flags set binds to its element enum TStubFlag', 'TStubFlag',
        lType.TypeName);

      lMember := FindMember(TPasClassType(lCls), 'Comparer');
      AssertNotNull('Comparer function found', lMember);
      AssertEquals('Comparer result binds to the proc-type TStubCompare',
        'TStubCompare',
        TPasFunction(lMember).FuncType.ResultEl.ResultType.Name);
    finally
      lResolver.Free;
    end;

    { (b) an OMITTED shape degrades to SILENCE: a consumer touching a member
      ABSENT from the faithful stub fails the whole unit rather than binding to a
      fabricated member (degrade, never invent). }
    lResolver := TFpSonarResolver.Create;
    try
      lResolver.DependencyInterfaceOnly := True;
      lResolver.IntrinsicConstEval := True;
      lResolver.CondDirectiveEval := True;
      AssertFalse('absent member degrades the whole unit to silence',
        lResolver.BuildFor(IncludeTrailingPathDelimiter(lDir) + 'broadbad.pas',
          cMode, cDefines, [lDir], [], lDiag));
      AssertNull('ResolvedModule nil on the omitted-shape degrade',
        lResolver.ResolvedModule);
    finally
      lResolver.Free;
    end;

    // (c) flag OFF is byte-identical: with no ppu-stub dir and no search paths,
    // 'uses depbroad' is unresolvable, so the consumer degrades exactly as today.
    lResolver := TFpSonarResolver.Create;
    try
      lResolver.DependencyInterfaceOnly := True;
      lResolver.IntrinsicConstEval := True;
      lResolver.CondDirectiveEval := True;
      // No stub on -Fu (off): the dep is unfindable, exactly as --synthetic-only.
      AssertFalse('flag OFF: consumer degrades (dep unfindable) — byte-identical',
        lResolver.BuildFor(IncludeTrailingPathDelimiter(lDir) + 'broaduser.pas',
          cMode, cDefines, [], [], lDiag));
      AssertNull('ResolvedModule nil when off', lResolver.ResolvedModule);
    finally
      lResolver.Free;
    end;
  finally
    RemoveTree(lDir);
  end;
end;


procedure TResolverTest.HybridRtlSourceEmitsSupersetGapMembers;

const
  // A minimal synthetic-shaped SysUtils carrying the splice anchors the hybrid
  // emitter keys on (the {$mode} line + the TFormatSettings/DecimalSeparator shape).
  cSynSysUtils =
    'unit SysUtils;' + LineEnding +
    '{$mode objfpc}{$H+}' + LineEnding +
    'interface' + LineEnding +
    'type' + LineEnding +
    '  TFormatSettings = record' + LineEnding +
    '    DecimalSeparator: AnsiChar;' + LineEnding +
    '  end;' + LineEnding +
    'implementation' + LineEnding +
    'end.' + LineEnding;

var
  lHyb: string;

begin
  // SysUtils hybrid = synthetic verbatim + faithful gap members (superset).
  lHyb := HybridRtlSource('SysUtils', cSynSysUtils);
  AssertTrue('SysUtils hybrid is non-empty', lHyb <> '');
  AssertTrue('hybrid carries the FileExists gap member',
    Pos('function FileExists(', lHyb) > 0);
  AssertTrue('hybrid carries the TStringHelper gap member',
    Pos('TStringHelper = type helper for AnsiString', lHyb) > 0);
  AssertTrue('hybrid carries the Split family', Pos('function Split(', lHyb) > 0);
  AssertTrue('hybrid preserves the synthetic base verbatim',
    Pos('unit SysUtils;', lHyb) > 0);

  // A non-RTL name has no hybrid (=> the caller uses the plain synthetic/source).
  AssertEquals('no hybrid for a non-RTL unit', '',
    HybridRtlSource('SomeUnit', cSynSysUtils));

  // A missing splice anchor degrades to '' (=> fall back to synthetic), never a
  // corrupted/approximated stub.
  AssertEquals('missing anchor => degrade to empty', '',
    HybridRtlSource('SysUtils',
      'unit SysUtils;' + LineEnding + '{$mode objfpc}{$H+}' + LineEnding +
      'interface' + LineEnding + 'implementation' + LineEnding + 'end.' + LineEnding));
end;


procedure TResolverTest.PpudumpJsonRepairQuotesNaNInf;

begin
  // The ppudump bare NaN/Inf float-const bug -> valid JSON (quoted), but ONLY in a
  // value position so string contents are never corrupted.
  AssertEquals('NaN value quoted', '{"a": "NaN"}',
    RepairPpudumpJson('{"a": NaN}'));
  AssertEquals('-Inf value quoted', '{"a": "-Inf"}',
    RepairPpudumpJson('{"a": -Inf}'));
  AssertEquals('Infinity value quoted', '{"a": "Infinity"}',
    RepairPpudumpJson('{"a": Infinity}'));
  // A string CONTAINING these words is untouched (not a value-position bareword).
  AssertEquals('string content untouched', '{"a": "Information"}',
    RepairPpudumpJson('{"a": "Information"}'));
  // Valid JSON with no bareword is returned unchanged.
  AssertEquals('clean json unchanged', '{"a": 1}',
    RepairPpudumpJson('{"a": 1}'));
end;


procedure TResolverTest.AutoDetectHybridRtlResolvesGapMemberOffDegrades;

const
  { A consumer that reads a SysUtils member ABSENT from the synthetic SysUtils but
    PRESENT in the faithful hybrid (FileExists). Host-independent: the hybrid is
    fixed superset text, served with no ppudump/.ppu needed. }
  cGapUserSource =
    'unit gapuser;' + LineEnding +
    '{$mode objfpc}{$H+}' + LineEnding +
    'interface' + LineEnding +
    'uses SysUtils;' + LineEnding +
    'implementation' + LineEnding +
    'procedure UseIt;' + LineEnding +
    'begin' + LineEnding +
    '  if FileExists(''x'') then ;' + LineEnding +
    'end;' + LineEnding +
    'end.' + LineEnding;

var
  lDir, lFile: string;
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;

begin
  lDir := FreshTempDir;
  try
    WriteUnit(lDir, 'gapuser.pas', cGapUserSource);
    lFile := IncludeTrailingPathDelimiter(lDir) + 'gapuser.pas';

    // ON (auto-detect): SysUtils resolves to the HYBRID (synthetic + gap), so the
    // consumer's FileExists call binds -> the whole unit resolves. No ppudump/.ppu.
    lResolver := TFpSonarResolver.Create;
    try
      lResolver.DependencyInterfaceOnly := True;
      lResolver.IntrinsicConstEval := True;
      lResolver.CondDirectiveEval := True;
      lResolver.PpuAutoDetect := True;
      AssertTrue('auto-detect: FileExists resolves via the hybrid SysUtils',
        lResolver.BuildFor(lFile, cMode, cDefines, [], [], lDiag));
      AssertTrue('Succeeded set under auto-detect', lResolver.Succeeded);
    finally
      lResolver.Free;
    end;

    { OFF (--synthetic-only): the synthetic SysUtils lacks FileExists, so the same
      consumer DEGRADES — proving the hybrid is a genuine superset and the default
      is never worse than synthetic (which is the floor, not the ceiling). }
    lResolver := TFpSonarResolver.Create;
    try
      lResolver.DependencyInterfaceOnly := True;
      lResolver.IntrinsicConstEval := True;
      lResolver.CondDirectiveEval := True;
      lResolver.PpuAutoDetect := False;
      AssertFalse('synthetic-only: FileExists is absent => consumer degrades',
        lResolver.BuildFor(lFile, cMode, cDefines, [], [], lDiag));
    finally
      lResolver.Free;
    end;
  finally
    RemoveTree(lDir);
  end;
end;


initialization
  RegisterTest(TResolverTest);

end.
