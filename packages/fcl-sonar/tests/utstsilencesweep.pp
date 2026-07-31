{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    The all-rules silence sweep over the language-surface corpus

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstSilenceSweep;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Config, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.SourceFile,
  { Every rule unit is named here so the sweep populates the global registry on
    its own rather than through whichever other test unit happens to link. }
  FpSonar.Rules.Calls, FpSonar.Rules.Casts, FpSonar.Rules.Classes,
  FpSonar.Rules.CondComp,
  FpSonar.Rules.Consts, FpSonar.Rules.Control, FpSonar.Rules.Eval,
  FpSonar.Rules.Exceptions,
  FpSonar.Rules.Forms, FpSonar.Rules.FpcStyle, FpSonar.Rules.Generics,
  FpSonar.Rules.Imports, FpSonar.Rules.Layout, FpSonar.Rules.Lifetime,
  FpSonar.Rules.Naming, FpSonar.Rules.Parens, FpSonar.Rules.Refs,
  FpSonar.Rules.SemNaming, FpSonar.Rules.Strings, FpSonar.Rules.Structure,
  FpSonar.Rules.Tokens,
  FpSonar.Rules.Trackers, FpSonar.Rules.Unused,
  UtstFixtures, utstLanguageSurface;

type
  // Why a (rule, fixture) pair is allowed to produce findings at all.
  TSweepExpectationKind = (sekIntrinsic,      // the rule fires correctly
    sekFalsePositive                          // the rule misreads correct code
    );

  { One sanctioned (rule, fixture) finding count and the reason it stands. }
  TSweepExpectation = record
    RuleId: string;
    Fixture: string;
    Count: integer;               // never zero: a zero row is noise
    Kind: TSweepExpectationKind;
    Justification: string;
    Ledger: string;               // deferred-work id, required when sekFalsePositive
  end;

  { One (rule, fixture) pair whose degraded count may exceed its full count. }
  TSweepGrowthExemption = record
    RuleId: string;
    Fixture: string;
    Count: integer;               // the sanctioned degraded count, asserted exactly
    Justification: string;
    Ledger: string;
  end;

  { One fixture the withholding itself makes unanalyzable, so the degraded pass
    cannot be held to the same input as the full pass on it. }
  TSweepDegradedCasualty = record
    Fixture: string;
    Justification: string;
    Ledger: string;
  end;

  // What the sweep does not supply, so the rule cannot reach its emit path.
  TSweepUnmeasurableCause = (
    sucNoTargets,                   // an empty targets param the rule exits on
    sucNoProjectIndex               // no project index, so a usProject query abstains
    );

  { One rule the sweep dispatches but cannot make fire on any input, so its
    silence is a skip rather than a measurement. }
  TSweepUnmeasurableRule = record
    RuleId: string;
    Cause: TSweepUnmeasurableCause;
    Justification: string;
    Ledger: string;
  end;


  { The whole registry against the whole corpus, in full and degraded
    resolution, asserted against the expectation table in both directions. }
  TSilenceSweepTest = class(TTestCase)
  private
    function FixtureIsStaged(const aName: string): boolean;
    function RowIndex(const aRuleId, aFixture: string): integer;
    function ExemptionIndex(const aRuleId, aFixture: string): integer;
    function CasualtyIndex(const aFixture: string): integer;
    function UnmeasurableIndex(const aRuleId: string): integer;
    function LedgerIdIsWellFormed(const aLedger: string): boolean;
    function RuleIdIsKnown(const aRuleId: string): boolean;
  published
    procedure EveryRuleIsSilentOnEveryFixture;
    procedure ExpectationTableIsWellFormed;
    procedure NoRuleRaisesOnAnyFixture;
    procedure NoAnalyzerDiagnosticOnAnyFixture;
    procedure SweepSeesTheWholeRegistryAndCorpus;
    procedure DegradedResolutionNeverGrowsTheFindingSet;
    procedure DegradationMechanismActuallyWithholdsResolution;
    procedure GrowthExemptionsAreJustified;
  end;


implementation

type
  { How often one rule fired on one fixture in one pass. }
  TSweepTally = record
    RuleId: string;
    Count: integer;
  end;

  TSweepTallyArray = array of TSweepTally;

  { One fixture's outcome in one pass: the state that made its silence
    meaningful, plus the per-rule finding counts. }
  TSweepFixtureResult = record
    Name: string;
    Family: string;
    Resolution: TLanguageSurfaceResolution;
    ParseSucceeded: boolean;
    ModulePresent: boolean;
    ResolverSucceeded: boolean;
    // Degraded pass only: the re-staged copy holds the same bytes as the
    // original.
    RestagedFaithfully: boolean;
    Tally: TSweepTallyArray;
    // One line per issue carrying a reserved id, so a fault can be named.
    Reserved: TFpSonarStringArray;
  end;

  TSweepPass = array of TSweepFixtureResult;

  { Both passes over the corpus, computed once per process. }
  TSweepCache = class
  private
    FFixtures: TTempFixtures;
    FFull: TSweepPass;
    FDegraded: TSweepPass;
    FProjectIndexSupplied: boolean;
    procedure RunPasses;
  public
    // Stages the corpus and computes the full and the degraded pass.
    constructor Create;
    // Drops the staged corpus directory and both cached passes.
    destructor Destroy; override;
    // The pass analyzing every fixture beside its siblings, resolution intact.
    property Full: TSweepPass read FFull;
    // The pass analyzing every fixture alone, with its dependencies withheld.
    property Degraded: TSweepPass read FDegraded;
    // Whether the dispatching engine carried a project index, which is what
    // decides whether a usProject query can be answered at all.
    property ProjectIndexSupplied: boolean read FProjectIndexSupplied;
  end;

const
  // The registry and the corpus only ever grow.
  cMinRegisteredRules = 268;
  cMinCorpusFixtures = 21;
  // How many fixtures must resolve, i.e. how many of them turn resolver-tier
  // silence into evidence rather than into an artefact of a dark resolver.
  cMinResolvingFixtures = 18;

  // The pointer size the degraded pass declares. It is NOT what withholds
  // objpas -- the separate aRealRtl argument does that. It is pinned to the
  // host pointer size.
  cDegradedPointerSize = SizeOf(Pointer);

  // The ids the analyzer reserves for its own diagnostics and for the engine's
  // per-rule fault boundary. None of them may occur in the full pass.
  cReservedRuleIds: array[0..4] of string = ('RuleError', 'ParseError',
    'ScanError', 'ResolveError', 'FileNotFound');

  // The measured degradation mechanism: each fixture is re-staged alone in its
  // own directory and analyzed with aRealRtl set and empty unit and include
  // paths, which takes all 18 lsrResolves fixtures to Succeeded = False.

  // Shared by the 21 MissingCopyrightHeader rows below.
  cSyntheticProbeHasNoBanner = 'A synthetic language-surface probe rather '
    + 'than a distributed FCL source: no fixture of the corpus carries a '
    + 'licence banner, so what the rule reports is a property of the corpus.';

  // Every (rule, fixture) pair the corpus is allowed to produce findings for.
  // A pair absent from this table must produce none; a row whose count no
  // longer matches, and a row for a pair that has gone silent, both fail.
  cExpectations: array[0..54] of TSweepExpectation = (
    (RuleId: 'DeclareParametersIndividually'; Fixture: 'surfacegenerics.pas';
     Count: 2; Kind: sekIntrinsic;
     Justification: 'The generic routine is witnessed by the declared marker '
       + '"generic function MaxOf<T>(const aLeft, aRight: T): T;", which is '
       + 'the grouped form, in the declaration and in its implementation '
       + 'header; splitting the parameters deletes the marker.';
     Ledger: ''),
    (RuleId: 'DeclareParametersIndividually'; Fixture: 'surfaceoperators.pas';
     Count: 2; Kind: sekIntrinsic;
     Justification: 'The static class method is witnessed by the declared '
       + 'marker "class function Sum(aLeft, aRight: Integer): Integer; '
       + 'static;"; the implementation header repeats the grouped signature.';
     Ledger: ''),
    (RuleId: 'DeclareParametersIndividually'; Fixture: 'surfacemodifiers.pas';
     Count: 2; Kind: sekIntrinsic;
     Justification: 'The forward declaration is witnessed by the declared '
       + 'marker "function Combine(aLeft, aRight: Integer): Integer; '
       + 'forward;"; the implementation header repeats the grouped signature.';
     Ledger: ''),
    (RuleId: 'ConstantNaming'; Fixture: 'surfaceoperators.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: 'The rule default pattern ^[A-Z][A-Za-z0-9_]*$ rejects the '
       + 'c prefix this codebase gives every constant and story 1.3 requires '
       + 'the corpus to follow; the in-class constant is also the declared '
       + 'marker "cStep = 1;".';
     Ledger: ''),
    (RuleId: 'ConstantNaming'; Fixture: 'surfacestorage.pas';
     Count: 2; Kind: sekIntrinsic;
     Justification: 'The rule default pattern ^[A-Z][A-Za-z0-9_]*$ rejects the '
       + 'c prefix this codebase gives every constant; the typed constant '
       + 'cLimits is also the declared marker for section A1 item 5.';
     Ledger: ''),
    (RuleId: 'ConstantNaming'; Fixture: 'surfacepreprocessor.pas';
     Count: 2; Kind: sekIntrinsic;
     Justification: 'The rule default pattern ^[A-Z][A-Za-z0-9_]*$ rejects the '
       + 'c prefix this codebase gives every constant; one of the two sites is '
       + 'in the include payload the fixture exists to pull in.';
     Ledger: ''),
    (RuleId: 'ConstantNaming'; Fixture: 'surfacepreprocessoralt.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: 'The rule default pattern ^[A-Z][A-Za-z0-9_]*$ rejects the '
       + 'c prefix this codebase gives every constant; the constant is also '
       + 'the declared marker "cFallbackDepth = 2;" pinning the taken arm.';
     Ledger: ''),
    (RuleId: 'ConstantNaming'; Fixture: 'surfaceexceptions.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: 'The rule default pattern ^[A-Z][A-Za-z0-9_]*$ rejects the '
       + 'c prefix this codebase gives every constant; the constant is named '
       + 'by the declared marker "else Result := cUnhandledCode;".';
     Ledger: ''),
    (RuleId: 'FieldNaming'; Fixture: 'surfaceoperators.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: 'The rule default pattern ^F[A-Z][A-Za-z0-9]*$ encodes the '
       + 'class private-field convention; this is a public data field of an '
       + 'advanced record, which the operator overloads read and write.';
     Ledger: ''),
    (RuleId: 'FieldNaming'; Fixture: 'surfacehelpers.pas';
     Count: 2; Kind: sekIntrinsic;
     Justification: 'The rule default pattern ^F[A-Z][A-Za-z0-9]*$ encodes the '
       + 'class private-field convention; these are the public data fields of '
       + 'the record the record helper extends.';
     Ledger: ''),
    (RuleId: 'FieldNaming'; Fixture: 'surfacerecords.pas';
     Count: 6; Kind: sekIntrinsic;
     Justification: 'The rule default pattern ^F[A-Z][A-Za-z0-9]*$ encodes the '
       + 'class private-field convention; the variant, packed and bitpacked '
       + 'layouts section A1 item 4 requires are made of public data fields.';
     Ledger: ''),
    (RuleId: 'FieldNaming'; Fixture: 'surfaceflowwith.pas';
     Count: 3; Kind: sekIntrinsic;
     Justification: 'The rule default pattern ^F[A-Z][A-Za-z0-9]*$ encodes the '
       + 'class private-field convention; these are public record fields, and '
       + 'all three are named by the declared markers "with aOuter, '
       + 'aOuter.Inner do" and "Result := Amount * Factor;".';
     Ledger: ''),
    (RuleId: 'ClassNaming'; Fixture: 'surfaceexceptions.pas';
     Count: 2; Kind: sekIntrinsic;
     Justification: 'The rule default pattern ^T[A-Z][A-Za-z0-9]*$ rejects the '
       + 'E prefix this codebase requires of every exception class, which is '
       + 'exactly what the exceptions family declares and raises.';
     Ledger: ''),
    (RuleId: 'NoGoto'; Fixture: 'surfaceflowlabel.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: 'Section A1 item 12 requires label and goto, and the goto '
       + 'is a declared marker; the family cannot drop the construct the rule '
       + 'exists to flag.';
     Ledger: ''),
    (RuleId: 'NoWith'; Fixture: 'surfaceflowwith.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: 'Section A1 item 12 requires a with statement over several '
       + 'expressions, and it is a declared marker; the family cannot drop the '
       + 'construct the rule exists to flag.';
     Ledger: ''),
    (RuleId: 'ProjectFileNoRoutines'; Fixture: 'surfaceprogram.lpr';
     Count: 1; Kind: sekIntrinsic;
     Justification: 'The program module form of section A1 item 15 is '
       + 'witnessed by the declared marker "GBanner := Banner;", which needs '
       + 'the program to declare the routine it calls.';
     Ledger: ''),
    (RuleId: 'ProjectFileNoVariables'; Fixture: 'surfaceprogram.lpr';
     Count: 1; Kind: sekIntrinsic;
     Justification: 'The program module form of section A1 item 15 is '
       + 'witnessed by the declared marker "GBanner := Banner;", which needs '
       + 'the program to declare the variable it assigns.';
     Ledger: ''),
    (RuleId: 'RoutineNaming'; Fixture: 'surfaceoperators.pas';
     Count: 3; Kind: sekFalsePositive;
     Justification: 'The rule applies a routine-name regex to operator '
       + 'overloads, whose reported name is the synthesized signature '
       + '"add(TCounter,TCounter):TCounter" rather than an identifier the '
       + 'author chose; no spelling of "operator +" can satisfy it.';
     Ledger: 'DW-4'),
    (RuleId: 'ConstructorNaming'; Fixture: 'surfaceoperators.pas';
     Count: 2; Kind: sekFalsePositive;
     Justification: 'TPasClassConstructor descends TPasConstructor, so the '
       + 'rule requires a Create-prefixed name of a CLASS constructor, which '
       + 'is a static initializer the caller never names.';
     Ledger: 'DW-5'),
    (RuleId: 'DestructorShouldOverrideDestroy'; Fixture: 'surfaceoperators.pas';
     Count: 1; Kind: sekFalsePositive;
     Justification: 'TPasClassDestructor descends TPasDestructor, so the rule '
       + 'requires a CLASS destructor to be named Destroy and to override, '
       + 'neither of which a class destructor can do.';
     Ledger: 'DW-6'),
    (RuleId: 'CombineTypeSections'; Fixture: 'surfacehelpers.pas';
     Count: 1; Kind: sekFalsePositive;
     Justification: 'The token-tier scan reads the type keyword of '
       + '"TIntegerHelper = type helper for Integer" as a second type section '
       + 'header; there is no second section and nothing to merge.';
     Ledger: 'DW-7'),
    (RuleId: 'CombineTypeSections'; Fixture: 'surfacestrings.pas';
     Count: 1; Kind: sekFalsePositive;
     Justification: 'The token-tier scan reads the type keyword of '
       + '"TWindows1252String = type AnsiString(1252)" as a second type '
       + 'section header; there is no second section and nothing to merge.';
     Ledger: 'DW-8'),
    (RuleId: 'NoIndentUnitLevelKeywords'; Fixture: 'surfacestrings.pas';
     Count: 1; Kind: sekFalsePositive;
     Justification: 'The same type keyword of the strong alias '
       + '"TWindows1252String = type AnsiString(1252)" is read as an indented '
       + 'unit-level section keyword; it is part of a type expression and '
       + 'cannot be unindented.';
     Ledger: 'DW-9'),
    (RuleId: 'MissingDottedUnitsGuard'; Fixture: 'surfacehelpers.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: 'A standalone test fixture rather than a library unit: it '
       + 'imports RTL units under one spelling only and is never built in a '
       + 'dotted configuration, so no directive in it names FPC_DOTTEDUNITS.';
     Ledger: ''),
    (RuleId: 'MissingDottedUnitsGuard'; Fixture: 'surfaceexceptions.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: 'A standalone test fixture rather than a library unit: it '
       + 'imports RTL units under one spelling only and is never built in a '
       + 'dotted configuration, so no directive in it names FPC_DOTTEDUNITS.';
     Ledger: ''),
    (RuleId: 'MissingDottedUnitsGuard'; Fixture: 'surfacemodifiers.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: 'A standalone test fixture rather than a library unit: it '
       + 'imports RTL units under one spelling only and is never built in a '
       + 'dotted configuration, so no directive in it names FPC_DOTTEDUNITS.';
     Ledger: ''),
    (RuleId: 'MissingDottedUnitsGuard'; Fixture: 'surfacemodule.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: 'A standalone test fixture rather than a library unit: it '
       + 'imports RTL units under one spelling only and is never built in a '
       + 'dotted configuration, so no directive in it names FPC_DOTTEDUNITS.';
     Ledger: ''),
    (RuleId: 'MissingDottedUnitsGuard'; Fixture: 'surfaceprogram.lpr';
     Count: 1; Kind: sekIntrinsic;
     Justification: 'A standalone test fixture rather than a library unit: it '
       + 'imports RTL units under one spelling only and is never built in a '
       + 'dotted configuration, so no directive in it names FPC_DOTTEDUNITS.';
     Ledger: ''),
    (RuleId: 'MissingCopyrightHeader'; Fixture: 'surfacegenerics.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: cSyntheticProbeHasNoBanner; Ledger: ''),
    (RuleId: 'MissingCopyrightHeader'; Fixture: 'surfaceoperators.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: cSyntheticProbeHasNoBanner; Ledger: ''),
    (RuleId: 'MissingCopyrightHeader'; Fixture: 'surfacehelpers.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: cSyntheticProbeHasNoBanner; Ledger: ''),
    (RuleId: 'MissingCopyrightHeader'; Fixture: 'surfacerecords.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: cSyntheticProbeHasNoBanner; Ledger: ''),
    (RuleId: 'MissingCopyrightHeader'; Fixture: 'surfacestorage.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: cSyntheticProbeHasNoBanner; Ledger: ''),
    (RuleId: 'MissingCopyrightHeader'; Fixture: 'surfaceclosuresdelphi.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: cSyntheticProbeHasNoBanner; Ledger: ''),
    (RuleId: 'MissingCopyrightHeader'; Fixture: 'surfaceclosuresnested.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: cSyntheticProbeHasNoBanner; Ledger: ''),
    (RuleId: 'MissingCopyrightHeader'; Fixture: 'surfaceattributes.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: cSyntheticProbeHasNoBanner; Ledger: ''),
    (RuleId: 'MissingCopyrightHeader'; Fixture: 'surfaceparameters.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: cSyntheticProbeHasNoBanner; Ledger: ''),
    (RuleId: 'MissingCopyrightHeader'; Fixture: 'surfacepreprocessor.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: cSyntheticProbeHasNoBanner
       + ' Its {$include} sits below the interface keyword, so it precedes no '
       + 'window and this fixture is measured like the other 20.';
     Ledger: ''),
    (RuleId: 'MissingCopyrightHeader'; Fixture: 'surfacepreprocessoralt.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: cSyntheticProbeHasNoBanner; Ledger: ''),
    (RuleId: 'MissingCopyrightHeader'; Fixture: 'surfaceinterfaces.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: cSyntheticProbeHasNoBanner; Ledger: ''),
    (RuleId: 'MissingCopyrightHeader'; Fixture: 'surfacedispinterface.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: cSyntheticProbeHasNoBanner; Ledger: ''),
    (RuleId: 'MissingCopyrightHeader'; Fixture: 'surfaceexceptions.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: cSyntheticProbeHasNoBanner; Ledger: ''),
    (RuleId: 'MissingCopyrightHeader'; Fixture: 'surfaceflowlabel.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: cSyntheticProbeHasNoBanner; Ledger: ''),
    (RuleId: 'MissingCopyrightHeader'; Fixture: 'surfaceflowwith.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: cSyntheticProbeHasNoBanner; Ledger: ''),
    (RuleId: 'MissingCopyrightHeader'; Fixture: 'surfacestrings.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: cSyntheticProbeHasNoBanner; Ledger: ''),
    (RuleId: 'MissingCopyrightHeader'; Fixture: 'surfacemodifiers.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: cSyntheticProbeHasNoBanner; Ledger: ''),
    (RuleId: 'MissingCopyrightHeader'; Fixture: 'surfacemodule.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: cSyntheticProbeHasNoBanner; Ledger: ''),
    (RuleId: 'MissingCopyrightHeader'; Fixture: 'surfacelibrary.lpr';
     Count: 1; Kind: sekIntrinsic;
     Justification: cSyntheticProbeHasNoBanner; Ledger: ''),
    (RuleId: 'MissingCopyrightHeader'; Fixture: 'surfaceprogram.lpr';
     Count: 1; Kind: sekIntrinsic;
     Justification: cSyntheticProbeHasNoBanner; Ledger: ''),
    (RuleId: 'RedundantElseAfterExit'; Fixture: 'surfaceexceptions.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: 'CheckRange is written "if aValue < 0 then begin raise '
       + 'ERangeRejected.Create(''negative''); end else if aValue > 100 then", '
       + 'so the then branch of the outer if at source line 37 ends in a raise '
       + 'and the else after it cannot be reached from that branch, which is '
       + 'exactly the declared shape. The report is a readability finding on '
       + 'correct code: the chain is a deliberate range classification and '
       + 'unindenting the second test changes nothing but the shape.';
     Ledger: ''),
    (RuleId: 'WideStringOnNonWindows'; Fixture: 'surfacestrings.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: 'The string-type matrix declares one value of every string '
       + 'flavour, and "lWide: WideString;" at source line 30 is the WideString '
       + 'entry of that matrix, so the rule reports the declaration the fixture '
       + 'exists to carry. The other five locals are a ShortString, a '
       + 'code-page-tagged AnsiString, a RawByteString, a UnicodeString and a '
       + 'UTF8String, none of which resolves WideString, which is why the count '
       + 'is exactly 1 and not 6.';
     Ledger: ''),
    (RuleId: 'UnknownConditionalSymbol';
     Fixture: 'surfacepreprocessoralt.pas';
     Count: 2; Kind: sekIntrinsic;
     Justification: 'SURFACE_UNSET is deliberately outside the corpus define '
       + 'set — that is what makes this fixture the untaken-branch half of the '
       + 'preprocessor family — and it is no FPC or target symbol either, so '
       + 'the two {$ifdef SURFACE_UNSET} openers at source lines 19 and 30 are '
       + 'precisely what the rule is built to report. The sibling half, '
       + 'surfacepreprocessor.pas, tests LINUX, which the corpus does define, '
       + 'and so stays silent.';
     Ledger: ''),
    (RuleId: 'ConditionalBranchNeverCompiled';
     Fixture: 'surfacepreprocessoralt.pas';
     Count: 2; Kind: sekIntrinsic;
     Justification: 'The same two openers: SURFACE_UNSET is undefined, so each '
       + 'then branch is dead under the corpus define set, and each conditional '
       + 'is closed, carries no {$elseif} and holds a const declaration '
       + 'respectively a Result assignment, which is the whole predicate.';
     Ledger: ''),
    (RuleId: 'SpecializationOfUnconstrainedGeneric';
     Fixture: 'surfacegenerics.pas';
     Count: 1; Kind: sekIntrinsic;
     Justification: 'The declared marker "TIntegerBox = specialize '
       + 'TBox<Integer>;" specializes generic TBox<T>, whose single template '
       + 'parameter carries no constraint at all, which is the whole '
       + 'predicate. The other specialization of the fixture, "specialize '
       + 'MaxOf<Integer>", is an inline-specialize expression rather than a '
       + 'declaration site and is not judged (DW-678).';
     Ledger: ''),
    (RuleId: 'AttributeOnNonRttiMember';
     Fixture: 'surfaceattributes.pas';
     Count: 3; Kind: sekIntrinsic;
     Justification: 'The fixture exists to carry attributes, and TDocumented '
       + 'writes one on each of the three member kinds RTTI knows: the private '
       + 'field "[Description(''a documented field'')] FValue: Integer;", the '
       + 'public method "[Description(''a documented method'')] procedure '
       + 'SetValue(aValue: Integer);" and the public property '
       + '"[Description(''a documented property'')] property Value: Integer '
       + 'read FValue;". None of the three sections is published and the class '
       + 'carries no $RTTI directive, so all three annotations are exactly the '
       + 'unreachable shape the rule reports. The fourth attribute of the '
       + 'fixture decorates the class type itself, which lives in the type '
       + 'section rather than in Members, and so is not judged.';
     Ledger: '')
    );

  // The (rule, fixture) pairs whose degraded count may exceed the full count.
  // Every other pair must shrink or stay equal.
  cGrowthExemptions: array[0..1] of TSweepGrowthExemption = (
    (RuleId: 'ScanError'; Fixture: 'surfacepreprocessor.pas';
     Count: 1;
     Justification: 'The degradation stages every fixture alone, which '
       + 'withholds the include payload this one pulls in, so the scan cannot '
       + 'complete. The growth is the withholding itself, folded into the '
       + 'collector as a diagnostic rather than produced by a rule.';
     Ledger: 'DW-10'),
    (RuleId: 'ParseError'; Fixture: 'surfacepreprocessor.pas';
     Count: 1;
     Justification: 'Same withheld include payload as the ScanError pair: the '
       + 'parse fails on the missing file, the module is nil and every AST and '
       + 'resolver rule is skipped, which is why the rest of this fixture '
       + 'shrinks to nothing.';
     Ledger: 'DW-11')
    );

  // The fixtures the withholding itself makes unanalyzable. Every other
  // fixture must still scan and parse in the degraded pass, and a fixture
  // named here that has started analyzing again fails as a stale entry.
  cDegradedCasualties: array[0..0] of TSweepDegradedCasualty = (
    (Fixture: 'surfacepreprocessor.pas';
     Justification: 'Staging alone withholds the include payload this fixture '
       + 'pulls in, so the scan and the parse both fail on the missing file. '
       + 'That is the same withholding the two growth exemptions record.';
     Ledger: 'DW-11')
    );

  // The rules the sweep dispatches but cannot make fire on ANY input.
  cUnmeasurableRules: array[0..15] of TSweepUnmeasurableRule = (
    (RuleId: 'DisallowedImportByPath'; Cause: sucNoTargets;
     Justification: 'Reports only what a configured disallow-list names, and '
       + 'the sweep configures no params at all.';
     Ledger: 'DW-17'),
    (RuleId: 'DisallowedConstant'; Cause: sucNoTargets;
     Justification: 'Reports only what a configured disallow-list names.';
     Ledger: 'DW-17'),
    (RuleId: 'DisallowedEnumValue'; Cause: sucNoTargets;
     Justification: 'Reports only what a configured disallow-list names.';
     Ledger: 'DW-17'),
    (RuleId: 'DisallowedField'; Cause: sucNoTargets;
     Justification: 'Reports only what a configured disallow-list names.';
     Ledger: 'DW-17'),
    (RuleId: 'DisallowedIdentifier'; Cause: sucNoTargets;
     Justification: 'Reports only what a configured disallow-list names.';
     Ledger: 'DW-17'),
    (RuleId: 'DisallowedProperty'; Cause: sucNoTargets;
     Justification: 'Reports only what a configured disallow-list names.';
     Ledger: 'DW-17'),
    (RuleId: 'DisallowedRoutine'; Cause: sucNoTargets;
     Justification: 'Reports only what a configured disallow-list names.';
     Ledger: 'DW-17'),
    (RuleId: 'DisallowedType'; Cause: sucNoTargets;
     Justification: 'Reports only what a configured disallow-list names.';
     Ledger: 'DW-17'),
    (RuleId: 'TrackTypeAliases'; Cause: sucNoTargets;
     Justification: 'Reports only what a configured target list names.';
     Ledger: 'DW-17'),
    (RuleId: 'RemoveUnusedImports'; Cause: sucNoProjectIndex;
     Justification: 'Exits before reading the module when no project index is '
       + 'attached, because an imported unit interface cannot be resolved '
       + 'without one.';
     Ledger: 'DW-18'),
    (RuleId: 'RemoveUnusedRoutine'; Cause: sucNoProjectIndex;
     Justification: 'Emits only when a usProject reference query answers '
       + 'unused; with no project index the query abstains and the '
       + 'declaration counts as used.';
     Ledger: 'DW-18'),
    (RuleId: 'RemoveUnusedType'; Cause: sucNoProjectIndex;
     Justification: 'Emits only when a usProject reference query answers '
       + 'unused, which cannot happen without a project index.';
     Ledger: 'DW-18'),
    (RuleId: 'RemoveUnusedGlobalVariable'; Cause: sucNoProjectIndex;
     Justification: 'Emits only when a usProject reference query answers '
       + 'unused, which cannot happen without a project index.';
     Ledger: 'DW-18'),
    (RuleId: 'InterfaceUsesTooBroad'; Cause: sucNoProjectIndex;
     Justification: 'Exits before reading the interface uses clause when no '
       + 'project index is attached, because the exported surface of an '
       + 'imported unit is what the index carries and nothing else answers '
       + 'for it.';
     Ledger: 'DW-18'),
    (RuleId: 'UnusedUnitInInterface'; Cause: sucNoProjectIndex;
     Justification: 'Exits before reading the interface uses clause when no '
       + 'project index is attached: the imported unit interface name set is '
       + 'what decides whether the implementation alone needs the import.';
     Ledger: 'DW-18'),
    (RuleId: 'PrivateMemberOnlyUsedByOneMethod'; Cause: sucNoProjectIndex;
     Justification: 'Exits before reading the module when no project index is '
       + 'attached, because the story requires it to stay silent under project '
       + 'uncertainty.';
     Ledger: 'DW-18')
    );

  // The param key the disallow-list rules read their targets from. Named here
  // so the staleness check asks the same question the rules ask.
  cTrackerTargetsKey = 'targets';


var
  // Built on first use, freed in finalization: 8 published tests cost 42
  // analyses rather than 336.
  GSweepCache: TSweepCache;


// Returns the aRuleId entry of aTally, or -1 when the rule did not fire.
function TallyIndex(const aTally: TSweepTallyArray;
  const aRuleId: string): integer;

var
  lIndex: integer;

begin
  Result := -1;
  for lIndex := 0 to High(aTally) do
    if aTally[lIndex].RuleId = aRuleId then
      Exit(lIndex);
end;


// Returns how often aRuleId fired on aResult.
function TallyCount(const aResult: TSweepFixtureResult;
  const aRuleId: string): integer;

var
  lIndex: integer;

begin
  lIndex := TallyIndex(aResult.Tally, aRuleId);
  if lIndex < 0 then
    Result := 0
  else
    Result := aResult.Tally[lIndex].Count;
end;


// Returns the aName entry of aPass, or -1 when the pass never saw it.
function PassIndex(const aPass: TSweepPass; const aName: string): integer;

var
  lIndex: integer;

begin
  Result := -1;
  for lIndex := 0 to High(aPass) do
    if SameText(aPass[lIndex].Name, aName) then
      Exit(lIndex);
end;


// Returns the diagnostic text collected for aRuleId on aResult.
function ReservedNotes(const aResult: TSweepFixtureResult;
  const aRuleId: string): string;

var
  lIndex: integer;

begin
  Result := '';
  for lIndex := 0 to High(aResult.Reserved) do
    if Pos(aRuleId, aResult.Reserved[lIndex]) = 1 then
      Result := Result + ' | ' + aResult.Reserved[lIndex];
end;


// Returns whether aRuleId is one of the analyzer's reserved ids.
function IsReservedRuleId(const aRuleId: string): boolean;

var
  lIndex: integer;

begin
  Result := False;
  for lIndex := Low(cReservedRuleIds) to High(cReservedRuleIds) do
    if cReservedRuleIds[lIndex] = aRuleId then
      Exit(True);
end;


// Returns a config enabling every rule the global registry holds, including the
// rules that ship DefaultEnabled = False, and taking the USE tier off the
// shipped utrOff default (DW-15).
function EnableEveryRegisteredRule: TFpSonarConfig;

var
  lIndex: integer;

begin
  Result := TFpSonarConfig.Default;
  // Rebuilt, not resized: every rule runs with no params, which is what
  // cUnmeasurableRules records for the rules that read one.
  SetLength(Result.Rules, 0);
  SetLength(Result.Rules, RuleRegistry.Count);
  for lIndex := 0 to RuleRegistry.Count - 1 do
  begin
    Result.Rules[lIndex].RuleId := RuleRegistry.Rule(lIndex).Metadata.RuleId;
    Result.Rules[lIndex].HasEnabled := True;
    Result.Rules[lIndex].Enabled := True;
  end;
  Result.UseTierResolution := utrPrefer;
end;


// Returns the lines of aPath, so a staged fixture can be re-staged alone.
function ReadStagedLines(const aPath: string): TFpSonarStringArray;

var
  lText: TStringList;
  lIndex: integer;

begin
  SetLength(Result, 0);
  lText := TStringList.Create;
  try
    lText.LoadFromFile(aPath);
    SetLength(Result, lText.Count);
    for lIndex := 0 to lText.Count - 1 do
      Result[lIndex] := lText[lIndex];
  finally
    lText.Free;
  end;
end;


// Returns whether both paths hold the same bytes.
function FilesAreIdentical(const aLeft, aRight: string): boolean;

var
  lLeft, lRight: TMemoryStream;

begin
  lLeft := TMemoryStream.Create;
  try
    lRight := TMemoryStream.Create;
    try
      lLeft.LoadFromFile(aLeft);
      lRight.LoadFromFile(aRight);
      Result := (lLeft.Size = lRight.Size)
        and ((lLeft.Size = 0)
          or CompareMem(lLeft.Memory, lRight.Memory, lLeft.Size));
    finally
      lRight.Free;
    end;
  finally
    lLeft.Free;
  end;
end;


// Folds one analyzed fixture and its collected issues into a pass entry.
function CollectResult(const aEntry: TLanguageSurfaceEntry;
  const aAnalyzedPath: string; aSource: TFpSonarSourceFile;
  aCollector: TFpSonarIssueCollector): TSweepFixtureResult;

var
  lIssue, lSlot, lArg: integer;
  lNote: string;

begin
  // The file that was actually analyzed, not the entry that was meant to be:
  // otherwise both passes copy the same name and every cross-pass lookup
  // between them holds by construction.
  Result.Name := ExtractFileName(aAnalyzedPath);
  Result.Family := aEntry.Family;
  Result.Resolution := aEntry.Resolution;
  Result.ParseSucceeded := aSource.ParseSucceeded;
  Result.ModulePresent := aSource.Module <> nil;
  Result.ResolverSucceeded := (aSource.Resolver <> nil)
    and aSource.Resolver.Succeeded;
  // The full pass analyzes the staged original, so nothing was re-encoded.
  Result.RestagedFaithfully := True;
  SetLength(Result.Tally, 0);
  SetLength(Result.Reserved, 0);
  for lIssue := 0 to aCollector.Count - 1 do
  begin
    lSlot := TallyIndex(Result.Tally, aCollector.Issues[lIssue].RuleId);
    if lSlot < 0 then
    begin
      lSlot := Length(Result.Tally);
      SetLength(Result.Tally, lSlot + 1);
      Result.Tally[lSlot].RuleId := aCollector.Issues[lIssue].RuleId;
      Result.Tally[lSlot].Count := 0;
    end;
    Inc(Result.Tally[lSlot].Count);
    if not IsReservedRuleId(aCollector.Issues[lIssue].RuleId) then
      Continue;
    lNote := aCollector.Issues[lIssue].RuleId;
    for lArg := 0 to High(aCollector.Issues[lIssue].MessageArgs) do
      lNote := lNote + ' | ' + aCollector.Issues[lIssue].MessageArgs[lArg];
    SetLength(Result.Reserved, Length(Result.Reserved) + 1);
    Result.Reserved[High(Result.Reserved)] := lNote;
  end;
end;


{ TSweepCache }

constructor TSweepCache.Create;

begin
  inherited Create;
  FFixtures := TTempFixtures.Create;
  RunPasses;
end;


destructor TSweepCache.Destroy;

begin
  SetLength(FFull, 0);
  SetLength(FDegraded, 0);
  FFixtures.Free;
  inherited Destroy;
end;


procedure TSweepCache.RunPasses;

var
  lEntries: TLanguageSurfaceEntryArray;
  lEngine: TFpSonarRuleEngine;
  lSource: TFpSonarSourceFile;
  lCollector: TFpSonarIssueCollector;
  lAlone: TTempFixtures;
  lIndex: integer;
  lAlonePath: string;

begin
  lEntries := StageLanguageSurfaceCorpus(FFixtures);
  SetLength(FFull, Length(lEntries));
  SetLength(FDegraded, Length(lEntries));
  lEngine := TFpSonarRuleEngine.Create;
  try
    lEngine.Config := EnableEveryRegisteredRule;
    // Recorded rather than assumed: it is what decides whether a usProject
    // reference query can be answered.
    FProjectIndexSupplied := lEngine.ProjectIndex <> nil;
    for lIndex := 0 to High(lEntries) do
    begin
      lSource := TFpSonarSourceFile.Create;
      try
        lCollector := TFpSonarIssueCollector.Create;
        try
          // No committed expectation may depend on the host's compiled units.
          lSource.PpuAutoDetect := False;
          lSource.Analyze(lEntries[lIndex].Path, lEntries[lIndex].Mode,
            LanguageSurfaceDefines);
          // The staged path, not the bare name: a rule probing the filesystem
          // beside the analyzed file must not reach the process directory.
          lEngine.Dispatch(lSource, lEntries[lIndex].Path,
            lEntries[lIndex].Mode, lCollector);
          FFull[lIndex] := CollectResult(lEntries[lIndex],
            lEntries[lIndex].Path, lSource, lCollector);
        finally
          lCollector.Free;
        end;
      finally
        lSource.Free;
      end;
    end;
    for lIndex := 0 to High(lEntries) do
    begin
      lAlone := TTempFixtures.Create;
      try
        lAlonePath := lAlone.Add(lEntries[lIndex].Name,
          ReadStagedLines(lEntries[lIndex].Path));
        lSource := TFpSonarSourceFile.Create;
        try
          lCollector := TFpSonarIssueCollector.Create;
          try
            lSource.PpuAutoDetect := False;
            lSource.Analyze(lAlonePath, lEntries[lIndex].Mode,
              LanguageSurfaceDefines, [], [], True, cDegradedPointerSize);
            lEngine.Dispatch(lSource, lAlonePath, lEntries[lIndex].Mode,
              lCollector);
            FDegraded[lIndex] := CollectResult(lEntries[lIndex], lAlonePath,
              lSource, lCollector);
            FDegraded[lIndex].RestagedFaithfully :=
              FilesAreIdentical(lEntries[lIndex].Path, lAlonePath);
          finally
            lCollector.Free;
          end;
        finally
          lSource.Free;
        end;
      finally
        lAlone.Free;
      end;
    end;
  finally
    lEngine.Free;
  end;
end;


// Returns the cached sweep, computing both passes on first use.
function Sweep: TSweepCache;

begin
  if GSweepCache = nil then
    GSweepCache := TSweepCache.Create;
  Result := GSweepCache;
end;


{ TSilenceSweepTest }

function TSilenceSweepTest.FixtureIsStaged(const aName: string): boolean;

begin
  Result := PassIndex(Sweep.Full, aName) >= 0;
end;


function TSilenceSweepTest.RowIndex(const aRuleId, aFixture: string): integer;

var
  lIndex: integer;

begin
  Result := -1;
  for lIndex := Low(cExpectations) to High(cExpectations) do
    if (cExpectations[lIndex].RuleId = aRuleId)
      and SameText(cExpectations[lIndex].Fixture, aFixture) then
      Exit(lIndex);
end;


function TSilenceSweepTest.ExemptionIndex(
  const aRuleId, aFixture: string): integer;

var
  lIndex: integer;

begin
  Result := -1;
  for lIndex := Low(cGrowthExemptions) to High(cGrowthExemptions) do
    if (cGrowthExemptions[lIndex].RuleId = aRuleId)
      and SameText(cGrowthExemptions[lIndex].Fixture, aFixture) then
      Exit(lIndex);
end;


function TSilenceSweepTest.CasualtyIndex(const aFixture: string): integer;

var
  lIndex: integer;

begin
  Result := -1;
  for lIndex := Low(cDegradedCasualties) to High(cDegradedCasualties) do
    if SameText(cDegradedCasualties[lIndex].Fixture, aFixture) then
      Exit(lIndex);
end;


function TSilenceSweepTest.UnmeasurableIndex(const aRuleId: string): integer;

var
  lIndex: integer;

begin
  Result := -1;
  for lIndex := Low(cUnmeasurableRules) to High(cUnmeasurableRules) do
    if cUnmeasurableRules[lIndex].RuleId = aRuleId then
      Exit(lIndex);
end;


function TSilenceSweepTest.LedgerIdIsWellFormed(const aLedger: string): boolean;

var
  lIndex: integer;

begin
  // DW-0 and a leading zero are rejected: the ledger numbers from one and an
  // id no entry can carry is worse than no id at all.
  Result := (Length(aLedger) > 3) and (Copy(aLedger, 1, 3) = 'DW-')
    and (aLedger[4] in ['1'..'9']);
  if not Result then
    Exit;
  for lIndex := 5 to Length(aLedger) do
    if not (aLedger[lIndex] in ['0'..'9']) then
      Exit(False);
end;


function TSilenceSweepTest.RuleIdIsKnown(const aRuleId: string): boolean;

begin
  Result := (RuleRegistry.FindById(aRuleId) <> nil)
    or IsReservedRuleId(aRuleId);
end;


procedure TSilenceSweepTest.EveryRuleIsSilentOnEveryFixture;

var
  lPass: TSweepPass;
  lFixture, lRule, lRow, lExpected, lObserved: integer;
  lRuleId: string;

begin
  lPass := Sweep.Full;
  // Direction one: every registered rule against every staged fixture. A pair
  // with no row must be silent; a pair with a row must match it exactly.
  for lFixture := 0 to High(lPass) do
  begin
    for lRule := 0 to RuleRegistry.Count - 1 do
    begin
      lRuleId := RuleRegistry.Rule(lRule).Metadata.RuleId;
      lRow := RowIndex(lRuleId, lPass[lFixture].Name);
      lObserved := TallyCount(lPass[lFixture], lRuleId);
      if lRow < 0 then
        AssertEquals(Format('rule %s must be silent on fixture %s',
          [lRuleId, lPass[lFixture].Name]), 0, lObserved)
      else
      begin
        lExpected := cExpectations[lRow].Count;
        if lObserved = 0 then
          AssertEquals(Format('stale row: rule %s no longer fires on fixture '
            + '%s, delete the row', [lRuleId, lPass[lFixture].Name]),
            lExpected, lObserved)
        else
          AssertEquals(Format('rule %s on fixture %s', [lRuleId,
            lPass[lFixture].Name]), lExpected, lObserved);
      end;
    end;
    // An id no rule in the registry carries is a reserved diagnostic id; it
    // has its own tests, but it must not slip through this one unnoticed.
    for lRule := 0 to High(lPass[lFixture].Tally) do
    begin
      lRuleId := lPass[lFixture].Tally[lRule].RuleId;
      if RuleRegistry.FindById(lRuleId) <> nil then
        Continue;
      AssertEquals(Format('unregistered id %s fired on fixture %s',
        [lRuleId, lPass[lFixture].Name]), 0,
        lPass[lFixture].Tally[lRule].Count);
    end;
  end;
  // Direction two: a row for a pair that has gone silent is stale and must be
  // deleted rather than left standing as documentation of nothing.
  for lRow := Low(cExpectations) to High(cExpectations) do
  begin
    lFixture := PassIndex(lPass, cExpectations[lRow].Fixture);
    AssertTrue(Format('the row for %s names a staged fixture, %s',
      [cExpectations[lRow].RuleId, cExpectations[lRow].Fixture]),
      lFixture >= 0);
    lObserved := TallyCount(lPass[lFixture], cExpectations[lRow].RuleId);
    AssertEquals(Format('stale row: %s no longer fires on %s, delete the row',
      [cExpectations[lRow].RuleId, cExpectations[lRow].Fixture]),
      cExpectations[lRow].Count, lObserved);
  end;
end;


procedure TSilenceSweepTest.ExpectationTableIsWellFormed;

var
  lRow, lOther: integer;

begin
  for lRow := Low(cExpectations) to High(cExpectations) do
  begin
    AssertTrue(Format('row %d names a rule id', [lRow]),
      cExpectations[lRow].RuleId <> '');
    AssertTrue(Format('row %d (%s) names a registered rule',
      [lRow, cExpectations[lRow].RuleId]),
      RuleRegistry.FindById(cExpectations[lRow].RuleId) <> nil);
    AssertTrue(Format('row %d (%s) names a staged fixture, %s',
      [lRow, cExpectations[lRow].RuleId, cExpectations[lRow].Fixture]),
      FixtureIsStaged(cExpectations[lRow].Fixture));
    AssertTrue(Format('row %d (%s on %s) has a non-zero count: a zero row is '
      + 'noise, not an expectation', [lRow, cExpectations[lRow].RuleId,
      cExpectations[lRow].Fixture]), cExpectations[lRow].Count > 0);
    AssertTrue(Format('row %d (%s on %s) is justified',
      [lRow, cExpectations[lRow].RuleId, cExpectations[lRow].Fixture]),
      Trim(cExpectations[lRow].Justification) <> '');
    // The two tables make opposite claims: a row says the rule fires n times,
    // an unmeasurable entry says it cannot fire at all. One of them is wrong.
    AssertEquals(Format('row %d (%s on %s) names a rule declared unmeasurable, '
      + 'so either the row or that declaration is wrong',
      [lRow, cExpectations[lRow].RuleId, cExpectations[lRow].Fixture]), -1,
      UnmeasurableIndex(cExpectations[lRow].RuleId));
    if cExpectations[lRow].Kind = sekFalsePositive then
      AssertTrue(Format('row %d (%s on %s) is a false positive, so it names a '
        + 'deferred-work entry', [lRow, cExpectations[lRow].RuleId,
        cExpectations[lRow].Fixture]),
        LedgerIdIsWellFormed(cExpectations[lRow].Ledger))
    else
      AssertEquals(Format('row %d (%s on %s) fires correctly, so it carries no '
        + 'ledger id', [lRow, cExpectations[lRow].RuleId,
        cExpectations[lRow].Fixture]), '', cExpectations[lRow].Ledger);
    for lOther := lRow + 1 to High(cExpectations) do
      AssertFalse(Format('rows %d and %d are the same pair, %s on %s',
        [lRow, lOther, cExpectations[lRow].RuleId,
        cExpectations[lRow].Fixture]),
        (cExpectations[lRow].RuleId = cExpectations[lOther].RuleId)
        and SameText(cExpectations[lRow].Fixture,
          cExpectations[lOther].Fixture));
  end;
end;


procedure TSilenceSweepTest.NoRuleRaisesOnAnyFixture;

  procedure CheckPass(const aPass: TSweepPass; const aWhich: string);

  var
    lFixture, lNote: integer;

  begin
    for lFixture := 0 to High(aPass) do
    begin
      for lNote := 0 to High(aPass[lFixture].Reserved) do
        AssertFalse(Format('%s pass, %s: a rule raised, %s',
          [aWhich, aPass[lFixture].Name, aPass[lFixture].Reserved[lNote]]),
          Pos('RuleError', aPass[lFixture].Reserved[lNote]) = 1);
      AssertEquals(Format('%s pass, %s: no rule may raise',
        [aWhich, aPass[lFixture].Name]), 0,
        TallyCount(aPass[lFixture], 'RuleError'));
    end;
  end;

begin
  CheckPass(Sweep.Full, 'full');
  // The degraded pass is where a rule is likeliest to raise: the module is nil
  // wherever the parse failed and the resolver is dark everywhere.
  CheckPass(Sweep.Degraded, 'degraded');
end;


procedure TSilenceSweepTest.NoAnalyzerDiagnosticOnAnyFixture;

var
  lPass: TSweepPass;
  lFixture, lKind: integer;

begin
  lPass := Sweep.Full;
  for lFixture := 0 to High(lPass) do
    for lKind := Low(cReservedRuleIds) to High(cReservedRuleIds) do
    begin
      if cReservedRuleIds[lKind] = 'RuleError' then
        Continue;
      // The collected diagnostic text goes in the message: without it a red
      // here names the kind and the fixture but not what actually failed.
      AssertEquals(Format('%s: the analyzer reported %s%s',
        [lPass[lFixture].Name, cReservedRuleIds[lKind],
        ReservedNotes(lPass[lFixture], cReservedRuleIds[lKind])]), 0,
        TallyCount(lPass[lFixture], cReservedRuleIds[lKind]));
    end;
end;


procedure TSilenceSweepTest.SweepSeesTheWholeRegistryAndCorpus;

var
  lPass: TSweepPass;
  lConfig: TFpSonarConfig;
  lFixture, lResolving, lDegrading, lRule, lOther: integer;

begin
  AssertTrue(Format('the registry holds at least %d rules, not %d',
    [cMinRegisteredRules, RuleRegistry.Count]),
    RuleRegistry.Count >= cMinRegisteredRules);
  // A rule whose feed the sweep never makes available is dispatched on no
  // fixture at all, and every silence assertion about it holds vacuously. The
  // sweep builds no project index.
  for lRule := 0 to RuleRegistry.Count - 1 do
    AssertTrue(Format('%s declares a feed the sweep can supply, so its silence '
      + 'is a measurement rather than a skip',
      [RuleRegistry.Rule(lRule).Metadata.RuleId]),
      RuleRegistry.Rule(lRule).Metadata.Feed
        in [rfLineText, rfTokenStream, rfAst, rfResolver]);
  // A rule whose emit path needs something the sweep never configures produces
  // zero on every fixture whatever those fixtures contain. Every such rule is
  // declared, justified and ledgered here.
  lConfig := EnableEveryRegisteredRule;
  for lRule := Low(cUnmeasurableRules) to High(cUnmeasurableRules) do
  begin
    AssertTrue(Format('unmeasurable entry %d (%s) names a registered rule',
      [lRule, cUnmeasurableRules[lRule].RuleId]),
      RuleRegistry.FindById(cUnmeasurableRules[lRule].RuleId) <> nil);
    AssertTrue(Format('unmeasurable entry %d (%s) is justified',
      [lRule, cUnmeasurableRules[lRule].RuleId]),
      Trim(cUnmeasurableRules[lRule].Justification) <> '');
    AssertTrue(Format('unmeasurable entry %d (%s) names a deferred-work entry',
      [lRule, cUnmeasurableRules[lRule].RuleId]),
      LedgerIdIsWellFormed(cUnmeasurableRules[lRule].Ledger));
    for lOther := lRule + 1 to High(cUnmeasurableRules) do
      AssertFalse(Format('unmeasurable entries %d and %d name the same rule, %s',
        [lRule, lOther, cUnmeasurableRules[lRule].RuleId]),
        cUnmeasurableRules[lRule].RuleId = cUnmeasurableRules[lOther].RuleId);
    // A stale entry is worse than none: it excuses a rule that the sweep can
    // now make fire.
    case cUnmeasurableRules[lRule].Cause of
      sucNoTargets:
        AssertEquals(Format('stale entry: the sweep now configures targets for '
          + '%s, so it is measurable and the entry must go',
          [cUnmeasurableRules[lRule].RuleId]), 0,
          Length(lConfig.RuleParamTargets(cUnmeasurableRules[lRule].RuleId,
            cTrackerTargetsKey)));
      sucNoProjectIndex:
        AssertFalse(Format('stale entry: the sweep now supplies a project '
          + 'index, so %s is measurable and the entry must go',
          [cUnmeasurableRules[lRule].RuleId]), Sweep.ProjectIndexSupplied);
    end;
  end;
  lPass := Sweep.Full;
  AssertTrue(Format('the corpus stages at least %d analyzable fixtures, not %d',
    [cMinCorpusFixtures, Length(lPass)]), Length(lPass) >= cMinCorpusFixtures);
  lResolving := 0;
  lDegrading := 0;
  for lFixture := 0 to High(lPass) do
  begin
    AssertTrue(Format('%s parsed', [lPass[lFixture].Name]),
      lPass[lFixture].ParseSucceeded);
    AssertTrue(Format('%s produced a module', [lPass[lFixture].Name]),
      lPass[lFixture].ModulePresent);
    // Silence from a rule whose feed was never available is not evidence.
    AssertEquals(Format('%s resolves exactly as the corpus declares',
      [lPass[lFixture].Name]),
      lPass[lFixture].Resolution = lsrResolves,
      lPass[lFixture].ResolverSucceeded);
    if lPass[lFixture].ResolverSucceeded then
      Inc(lResolving)
    else
      Inc(lDegrading);
  end;
  // The degrading fixtures contribute no resolver-tier evidence at all: every
  // rfResolver rule is skipped on them by construction.
  AssertTrue(Format('at least %d fixtures resolve, so resolver-tier silence is '
    + 'evidence on them; %d do and %d contribute none',
    [cMinResolvingFixtures, lResolving, lDegrading]),
    lResolving >= cMinResolvingFixtures);
end;


procedure TSilenceSweepTest.DegradedResolutionNeverGrowsTheFindingSet;

var
  lFull, lDegraded: TSweepPass;
  lFixture, lSlot, lOther, lExempt, lRule: integer;
  lFullCount, lDegradedCount: integer;
  lRuleId: string;

begin
  lFull := Sweep.Full;
  lDegraded := Sweep.Degraded;
  AssertEquals('both passes cover the same fixtures', Length(lFull),
    Length(lDegraded));
  for lFixture := 0 to High(lDegraded) do
  begin
    lOther := PassIndex(lFull, lDegraded[lFixture].Name);
    AssertTrue(Format('%s appears in the full pass too',
      [lDegraded[lFixture].Name]), lOther >= 0);
    for lSlot := 0 to High(lDegraded[lFixture].Tally) do
    begin
      lRuleId := lDegraded[lFixture].Tally[lSlot].RuleId;
      lDegradedCount := lDegraded[lFixture].Tally[lSlot].Count;
      lFullCount := TallyCount(lFull[lOther], lRuleId);
      if lDegradedCount <= lFullCount then
        Continue;
      lExempt := ExemptionIndex(lRuleId, lDegraded[lFixture].Name);
      AssertTrue(Format('%s on %s grew from %d to %d under degraded '
        + 'resolution and is not a declared growth exemption',
        [lRuleId, lDegraded[lFixture].Name, lFullCount, lDegradedCount]),
        lExempt >= 0);
      // An exemption sanctions a pinned amount of growth, not any amount.
      AssertEquals(Format('%s on %s is an exempt grower, so its degraded count '
        + 'is pinned', [lRuleId, lDegraded[lFixture].Name]),
        cGrowthExemptions[lExempt].Count, lDegradedCount);
    end;
    // The corpus-wide form of DegradesWithoutResolver: with the resolver dark
    // on every fixture, no rule reading the resolver feed may produce anything.
    for lRule := 0 to RuleRegistry.Count - 1 do
    begin
      if RuleRegistry.Rule(lRule).Metadata.Feed <> rfResolver then
        Continue;
      lRuleId := RuleRegistry.Rule(lRule).Metadata.RuleId;
      AssertEquals(Format('%s reads the resolver feed, so it must produce '
        + 'nothing on %s once resolution is withheld',
        [lRuleId, lDegraded[lFixture].Name]), 0,
        TallyCount(lDegraded[lFixture], lRuleId));
    end;
  end;
end;


procedure TSilenceSweepTest.DegradationMechanismActuallyWithholdsResolution;

var
  lFull, lPass: TSweepPass;
  lFixture, lWithheld, lRule, lOther: integer;
  lRuleId, lOffenders: string;

begin
  lPass := Sweep.Degraded;
  lFull := Sweep.Full;
  // A casualty naming a fixture the corpus never stages exempts nothing and
  // would be read as a sanctioned loss that is not happening.
  for lRule := Low(cDegradedCasualties) to High(cDegradedCasualties) do
  begin
    AssertTrue(Format('casualty %d names a staged fixture, %s',
      [lRule, cDegradedCasualties[lRule].Fixture]),
      FixtureIsStaged(cDegradedCasualties[lRule].Fixture));
    AssertTrue(Format('casualty %d (%s) is justified',
      [lRule, cDegradedCasualties[lRule].Fixture]),
      Trim(cDegradedCasualties[lRule].Justification) <> '');
    AssertTrue(Format('casualty %d (%s) names a deferred-work entry',
      [lRule, cDegradedCasualties[lRule].Fixture]),
      LedgerIdIsWellFormed(cDegradedCasualties[lRule].Ledger));
    // CasualtyIndex answers with the first match.
    for lOther := lRule + 1 to High(cDegradedCasualties) do
      AssertFalse(Format('casualties %d and %d name the same fixture, %s',
        [lRule, lOther, cDegradedCasualties[lRule].Fixture]),
        SameText(cDegradedCasualties[lRule].Fixture,
          cDegradedCasualties[lOther].Fixture));
  end;
  lWithheld := 0;
  for lFixture := 0 to High(lPass) do
  begin
    // A re-encoded copy would make the two passes compare different sources.
    AssertTrue(Format('%s was re-staged byte for byte', [lPass[lFixture].Name]),
      lPass[lFixture].RestagedFaithfully);
    // The withholding may kill the analysis of a declared casualty and of
    // nothing else. A degradation that stopped every fixture parsing would
    // empty both tallies and satisfy every "must not grow" assertion there is.
    if CasualtyIndex(lPass[lFixture].Name) >= 0 then
      AssertFalse(Format('stale casualty: %s analyzes again in the degraded '
        + 'pass, delete the entry', [lPass[lFixture].Name]),
        lPass[lFixture].ParseSucceeded)
    else
    begin
      AssertTrue(Format('%s still parses alone: the degradation withholds '
        + 'resolution, not the source', [lPass[lFixture].Name]),
        lPass[lFixture].ParseSucceeded);
      AssertTrue(Format('%s still produces a module alone',
        [lPass[lFixture].Name]), lPass[lFixture].ModulePresent);
    end;
    if lPass[lFixture].Resolution <> lsrResolves then
      Continue;
    AssertFalse(Format('%s resolves in the full pass, so the degraded pass '
      + 'must withhold resolution from it', [lPass[lFixture].Name]),
      lPass[lFixture].ResolverSucceeded);
    Inc(lWithheld);
  end;
  AssertTrue(Format('the degraded pass covers the fixtures that resolve; %d '
    + 'were withheld from', [lWithheld]), lWithheld >= cMinResolvingFixtures);
  // The positive control: a rule that does not read the resolver feed sees a
  // byte-identical file in both passes, so it must produce exactly what it
  // produced in the full pass. Measured: no rule in the registry differs.
  lOffenders := '';
  for lFixture := 0 to High(lPass) do
  begin
    if CasualtyIndex(lPass[lFixture].Name) >= 0 then
      Continue;
    lOther := PassIndex(lFull, lPass[lFixture].Name);
    AssertTrue(Format('%s appears in the full pass too', [lPass[lFixture].Name]),
      lOther >= 0);
    for lRule := 0 to RuleRegistry.Count - 1 do
    begin
      if RuleRegistry.Rule(lRule).Metadata.Feed = rfResolver then
        Continue;
      lRuleId := RuleRegistry.Rule(lRule).Metadata.RuleId;
      if TallyCount(lPass[lFixture], lRuleId)
        = TallyCount(lFull[lOther], lRuleId) then
        Continue;
      lOffenders := lOffenders + Format(' %s on %s (%d -> %d)',
        [lRuleId, lPass[lFixture].Name, TallyCount(lFull[lOther], lRuleId),
        TallyCount(lPass[lFixture], lRuleId)]);
    end;
  end;
  AssertEquals('every rule that does not read the resolver feed must produce '
    + 'the same findings in both passes', '', lOffenders);
end;


procedure TSilenceSweepTest.GrowthExemptionsAreJustified;

var
  lFull, lDegraded: TSweepPass;
  lRow, lOther, lFixture: integer;

begin
  lFull := Sweep.Full;
  lDegraded := Sweep.Degraded;
  for lRow := Low(cGrowthExemptions) to High(cGrowthExemptions) do
  begin
    AssertTrue(Format('exemption %d (%s) names a known rule or reserved id',
      [lRow, cGrowthExemptions[lRow].RuleId]),
      RuleIdIsKnown(cGrowthExemptions[lRow].RuleId));
    AssertTrue(Format('exemption %d (%s) names a staged fixture, %s',
      [lRow, cGrowthExemptions[lRow].RuleId,
      cGrowthExemptions[lRow].Fixture]),
      FixtureIsStaged(cGrowthExemptions[lRow].Fixture));
    AssertTrue(Format('exemption %d (%s on %s) is justified',
      [lRow, cGrowthExemptions[lRow].RuleId,
      cGrowthExemptions[lRow].Fixture]),
      Trim(cGrowthExemptions[lRow].Justification) <> '');
    AssertTrue(Format('exemption %d (%s on %s) names a deferred-work entry',
      [lRow, cGrowthExemptions[lRow].RuleId,
      cGrowthExemptions[lRow].Fixture]),
      LedgerIdIsWellFormed(cGrowthExemptions[lRow].Ledger));
    for lOther := lRow + 1 to High(cGrowthExemptions) do
      AssertFalse(Format('exemptions %d and %d are the same pair, %s on %s',
        [lRow, lOther, cGrowthExemptions[lRow].RuleId,
        cGrowthExemptions[lRow].Fixture]),
        (cGrowthExemptions[lRow].RuleId = cGrowthExemptions[lOther].RuleId)
        and SameText(cGrowthExemptions[lRow].Fixture,
          cGrowthExemptions[lOther].Fixture));
    // A stale exemption is as bad as a stale row: it sanctions growth that no
    // longer happens and would hide the next one.
    lFixture := PassIndex(lDegraded, cGrowthExemptions[lRow].Fixture);
    lOther := PassIndex(lFull, cGrowthExemptions[lRow].Fixture);
    AssertTrue(Format('exemption %d names a fixture both passes cover, %s',
      [lRow, cGrowthExemptions[lRow].Fixture]),
      (lFixture >= 0) and (lOther >= 0));
    AssertTrue(Format('exemption %d (%s on %s) pins a positive degraded count',
      [lRow, cGrowthExemptions[lRow].RuleId, cGrowthExemptions[lRow].Fixture]),
      cGrowthExemptions[lRow].Count > 0);
    AssertTrue(Format('stale exemption: %s on %s no longer grows under '
      + 'degraded resolution, delete the exemption',
      [cGrowthExemptions[lRow].RuleId, cGrowthExemptions[lRow].Fixture]),
      TallyCount(lDegraded[lFixture], cGrowthExemptions[lRow].RuleId)
      > TallyCount(lFull[lOther], cGrowthExemptions[lRow].RuleId));
  end;
end;


initialization
  RegisterTest(TSilenceSweepTest);

finalization
  FreeAndNil(GSweepCache);
end.
