{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the SEM-tier generic, specialization and capture rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesGenerics;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Config, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Resolver, FpSonar.Rules.Generics, FpSonar.Rules.Refs, UtstFixtures;

type
  { Generics rule position, silence, degradation and registration tests. }
  TRulesGenericsTest = class(TTestCase)
  private
    // A config enabling exactly the ids in aIds.
    function EnabledConfig(const aIds: array of string): TFpSonarConfig;
    { Fresh instances carrying the metadata the unit registered globally. }
    function NewConstraintUnused: TRuleBase;
    function NewUnconstrainedSpec: TRuleBase;
    function NewNestingDepth: TRuleBase;
    function NewCapturesLoopVar: TRuleBase;
    function NewCapturesSelf: TRuleBase;
    function NewInlineVarCaptured: TRuleBase;
    function NewAttributeOnNonRtti: TRuleBase;
    // Whether aSrc, materialised as aName, resolves.
    function Resolves(const aName: string;
      const aSrc: array of string): boolean;
    { Runs aRule, the only rule registered and enabled, over the inline source
      aSrc materialised as aName in a temp dir. }
    procedure RunAloneSrc(aRule: TRuleBase; const aRuleId, aName: string;
      const aSrc: array of string; const aCollector: TFpSonarIssueCollector);
    // The same run, driven by a configuration the caller supplies whole.
    procedure RunAloneCfgSrc(aRule: TRuleBase; const aName: string;
      const aSrc: array of string; const aConfig: TFpSonarConfig;
      const aCollector: TFpSonarIssueCollector);
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
  published
    procedure GenericConstraintUnusedPositions;
    procedure GenericConstraintUnusedPositionsPerMode;
    procedure GenericConstraintUnusedSilentShapes;
    procedure GenericConstraintUnusedSilentOnUnresolvedOperand;
    procedure GenericConstraintUnusedDegradesWithoutResolver;
    procedure SpecializationOfUnconstrainedGenericPositions;
    procedure SpecializationOfUnconstrainedGenericSilentShapes;
    procedure SpecializationOfUnconstrainedGenericSilentOnUnresolvedOperand;
    procedure SpecializationOfUnconstrainedGenericDegradesWithoutResolver;
    procedure NestedGenericSpecializationDepthPositions;
    procedure NestedGenericSpecializationDepthCountsWrittenNesting;
    procedure NestedGenericSpecializationDepthPositionsPerMode;
    procedure NestedGenericSpecializationDepthHonoursMaxDepth;
    procedure NestedGenericSpecializationDepthSilentShapes;
    procedure NestedGenericSpecializationDepthSilentOnUnresolvedOperand;
    procedure NestedGenericSpecializationDepthDegradesWithoutResolver;
    procedure AnonymousMethodCapturesLoopVariablePositions;
    procedure AnonymousMethodCapturesLoopVariablePositionsPerMode;
    procedure AnonymousMethodCapturesLoopVariableSilentShapes;
    procedure AnonymousMethodCapturesLoopVariableSilentOnUnresolvedOperand;
    procedure AnonymousMethodCapturesLoopVariableDegradesWithoutResolver;
    procedure AnonymousMethodCapturesSelfPositions;
    procedure AnonymousMethodCapturesSelfPositionsPerMode;
    procedure AnonymousMethodCapturesSelfSilentShapes;
    procedure AnonymousMethodCapturesSelfSilentOnUnresolvedOperand;
    procedure AnonymousMethodCapturesSelfDegradesWithoutResolver;
    procedure AnonymousMethodCaptureRulesAreDisjointFromInlineVarRule;
    procedure AttributeOnNonRttiMemberPositions;
    procedure AttributeOnNonRttiMemberPositionsPerMode;
    procedure AttributeOnNonRttiMemberSilentShapes;
    procedure AttributeOnNonRttiMemberIsAstTierNotResolverBound;
    procedure GenericsRulesSurviveParseFailure;
    procedure GenericsRulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cDefines: array[0..3] of string = ('FPC', 'CPUX86_64', 'UNIX', 'LINUX');
  cConstraintUnusedId = 'GenericConstraintUnused';
  cUnconstrainedSpecId = 'SpecializationOfUnconstrainedGeneric';
  cNestingDepthId = 'NestedGenericSpecializationDepth';
  cCapturesLoopVarId = 'AnonymousMethodCapturesLoopVariable';
  cCapturesSelfId = 'AnonymousMethodCapturesSelf';
  cInlineVarCapturedId = 'NoInlineVarCapturedByAnonMethod';
  cAttributeNonRttiId = 'AttributeOnNonRttiMember';
  cErrorId = 'RuleError';
  cParseErrorId = 'ParseError';
  cMaxDepthParam = 'maxDepth';

  // Noncompliant: line 5 constrains T to TObject and only ever stores a T.
  cConstraintIdle: array[0..15] of string = (
    'unit genprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TIdle<T: TObject> = class(TObject)',
    '  private',
    '    FItem: T;',
    '  public',
    '    procedure Put(const aValue: T);',
    '  end;',
    'implementation',
    'procedure TIdle.Put(const aValue: T);',
    'begin',
    '  FItem := aValue;',
    'end;',
    'end.');

  // Compliant: the body selects a member on a field declared of type T.
  cConstraintRelied: array[0..15] of string = (
    'unit genprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TOwner<T: TObject> = class(TObject)',
    '  private',
    '    FItem: T;',
    '  public',
    '    procedure Clear;',
    '  end;',
    'implementation',
    'procedure TOwner.Clear;',
    'begin',
    '  FItem.Free;',
    'end;',
    'end.');

  { Compliant: the first body constructs through the parameter itself, the
    second tests an unrelated field against it. }
  cConstraintNamedDirectly: array[0..25] of string = (
    'unit genprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TMaker<T: TObject> = class(TObject)',
    '  public',
    '    procedure Make;',
    '  end;',
    '  generic TTester<U: TObject> = class(TObject)',
    '  private',
    '    FAny: TObject;',
    '  public',
    '    function Matches: Boolean;',
    '  end;',
    'implementation',
    'procedure TMaker.Make;',
    'var',
    '  lItem: T;',
    'begin',
    '  lItem := T.Create;',
    'end;',
    'function TTester.Matches: Boolean;',
    'begin',
    '  Result := FAny is U;',
    'end;',
    'end.');

  { The same idle constraint in the Delphi generic syntax, whose implementation
    header spells the owner without the template list. }
  cConstraintIdleDelphi: array[0..15] of string = (
    'unit genprobe;',
    '{$mode delphi}{$H+}',
    'interface',
    'type',
    '  TIdle<T: TObject> = class(TObject)',
    '  private',
    '    FItem: T;',
    '  public',
    '    procedure Put(const aValue: T);',
    '  end;',
    'implementation',
    'procedure TIdle<T>.Put(const aValue: T);',
    'begin',
    '  FItem := aValue;',
    'end;',
    'end.');

  { A with scope elides the selector, so the reliance leaves no member-selection
    expression behind. }
  cConstraintWithScope: array[0..15] of string = (
    'unit genprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TWither<T: TObject> = class(TObject)',
    '  private',
    '    FItem: T;',
    '  public',
    '    procedure Drop;',
    '  end;',
    'implementation',
    'procedure TWither.Drop;',
    'begin',
    '  with FItem do Free;',
    'end;',
    'end.');

  { Compliant: assigning nil to a T-typed field is legal only because of the
    class constraint, so it is reliance. }
  cConstraintNilAssigned: array[0..15] of string = (
    'unit genprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TOwned<T: TObject> = class(TObject)',
    '  private',
    '    FItem: T;',
    '  public',
    '    procedure Clear;',
    '  end;',
    'implementation',
    'procedure TOwned.Clear;',
    'begin',
    '  FItem := nil;',
    'end;',
    'end.');

  // Compliant: a class/record/constructor keyword constraint is never judged.
  cConstraintKeyword: array[0..15] of string = (
    'unit genprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TKeyed<T: class> = class(TObject)',
    '  private',
    '    FItem: T;',
    '  public',
    '    procedure Put(const aValue: T);',
    '  end;',
    'implementation',
    'procedure TKeyed.Put(const aValue: T);',
    'begin',
    '  FItem := aValue;',
    'end;',
    'end.');

  // Compliant: no routine of the generic is implemented.
  cConstraintNoBody: array[0..9] of string = (
    'unit genprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TBag<T: TObject> = class(TObject)',
    '  private',
    '    FItem: T;',
    '  end;',
    'implementation',
    'end.');

  { Compliant: the T-typed local is relied on inside its own routine, and its
    name recurs on a differently typed local of the next one. }
  cConstraintScopedLocal: array[0..22] of string = (
    'unit genprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TScoped<T: TObject> = class(TObject)',
    '  public',
    '    procedure Own;',
    '    procedure Other;',
    '  end;',
    'implementation',
    'procedure TScoped.Own;',
    'var',
    '  lItem: T;',
    'begin',
    '  lItem := nil;',
    'end;',
    'procedure TScoped.Other;',
    'var',
    '  lItem: TObject;',
    'begin',
    '  lItem.Free;',
    'end;',
    'end.');

  // Compliant: an unconstrained parameter carries nothing to judge.
  cConstraintAbsent: array[0..9] of string = (
    'unit genprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TCell<T> = class(TObject)',
    '  private',
    '    FValue: T;',
    '  end;',
    'implementation',
    'end.');

  // Noncompliant: line 9 specializes a generic that constrains nothing.
  cSpecUnconstrained: array[0..10] of string = (
    'unit genprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TCell<T> = class(TObject)',
    '  private',
    '    FValue: T;',
    '  end;',
    '  TIntCell = specialize TCell<Integer>;',
    'implementation',
    'end.');

  // Compliant: the generic declares const parameters and no type parameter.
  cSpecConstParamsOnly: array[0..10] of string = (
    'unit genprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TFixed<const N: Integer> = class(TObject)',
    '  private',
    '    FCount: Integer;',
    '  end;',
    '  TFixed4 = specialize TFixed<4>;',
    'implementation',
    'end.');

  { Noncompliant: line 9 specializes a generic whose only type parameter is
    unconstrained, the const parameter's type annotation notwithstanding. }
  cSpecConstAndTypeParam: array[0..10] of string = (
    'unit genprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TBuf<T; const N: Integer> = class(TObject)',
    '  private',
    '    FValue: T;',
    '  end;',
    '  TBuf4 = specialize TBuf<TObject, 4>;',
    'implementation',
    'end.');

  // Compliant: one of the two parameters carries a constraint.
  cSpecPartlyConstrained: array[0..11] of string = (
    'unit genprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TPair<T: TObject; U> = class(TObject)',
    '  private',
    '    FFirst: T;',
    '    FSecond: U;',
    '  end;',
    '  TPairInst = specialize TPair<TObject, Integer>;',
    'implementation',
    'end.');

  // Noncompliant: line 9 nests four levels of specialization.
  cNestingDeep: array[0..10] of string = (
    'unit genprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TCell<T> = class(TObject)',
    '  private',
    '    FValue: T;',
    '  end;',
    '  TDeep = specialize TCell< specialize TCell< specialize TCell< '
      + 'specialize TCell<Integer> > > >;',
    'implementation',
    'end.');

  // The same four levels written in the Delphi generic syntax.
  cNestingDeepDelphi: array[0..10] of string = (
    'unit genprobe;',
    '{$mode delphi}{$H+}',
    'interface',
    'type',
    '  TCell<T> = class(TObject)',
    '  private',
    '    FValue: T;',
    '  end;',
    '  TDeep = TCell< TCell< TCell< TCell<Integer> > > >;',
    'implementation',
    'end.');

  { Compliant: each declaration specializes one level over the previous one, so
    the chain is deep in the resolved tree and flat in the source. }
  cNestingAliasChain: array[0..13] of string = (
    'unit genprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TCell<T> = class(TObject)',
    '  private',
    '    FValue: T;',
    '  end;',
    '  TA = specialize TCell<Integer>;',
    '  TB = specialize TCell<TA>;',
    '  TC = specialize TCell<TB>;',
    '  TD = specialize TCell<TC>;',
    'implementation',
    'end.');

  // Compliant: two levels sit under the default maximum of three.
  cNestingShallow: array[0..10] of string = (
    'unit genprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TCell<T> = class(TObject)',
    '  private',
    '    FValue: T;',
    '  end;',
    '  TShallow = specialize TCell< specialize TCell<Integer> >;',
    'implementation',
    'end.');

  // One idle constraint (line 5), one unconstrained and four-deep
  // specialization (line 15): the resolved control of the degradation tests.
  cGenericsAllShapes: array[0..20] of string = (
    'unit genprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TIdle<T: TObject> = class(TObject)',
    '  private',
    '    FItem: T;',
    '  public',
    '    procedure Put(const aValue: T);',
    '  end;',
    '  generic TCell<U> = class(TObject)',
    '  private',
    '    FValue: U;',
    '  end;',
    '  TDeep = specialize TCell< specialize TCell< specialize TCell< '
      + 'specialize TCell<Integer> > > >;',
    'implementation',
    'procedure TIdle.Put(const aValue: T);',
    'begin',
    '  FItem := aValue;',
    'end;',
    'end.');

  // The same shapes behind a uses clause naming a unit that cannot be found.
  cGenericsNoResolution: array[0..15] of string = (
    'unit genprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses NoSuchUnitForFpSonar;',
    'type',
    '  generic TIdle<T: TObject> = class(TObject)',
    '  private',
    '    FItem: T;',
    '  end;',
    '  generic TCell<U> = class(TObject)',
    '  private',
    '    FValue: U;',
    '  end;',
    '  TDeep = specialize TCell< specialize TCell< specialize TCell< '
      + 'specialize TCell<Integer> > > >;',
    'implementation',
    'end.');

  // The constrained parameter and the specialized generic both live in the
  // unit the closure cannot reach.
  cGenericsUnresolvedOperand: array[0..11] of string = (
    'unit genprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses NoSuchUnitForFpSonar;',
    'type',
    '  generic TIdle<T: TMissingBase> = class(TObject)',
    '  private',
    '    FItem: T;',
    '  end;',
    '  TDeep = specialize TMissing< specialize TMissing< specialize TMissing< '
      + 'specialize TMissing<Integer> > > >;',
    'implementation',
    'end.');

  // All three shapes behind a syntax error the parser cannot pass.
  cGenericsParseFailure: array[0..15] of string = (
    'unit genbroken;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TBroken = class(;',
    '  generic TIdle<T: TObject> = class(TObject)',
    '  private',
    '    FItem: T;',
    '  end;',
    '  generic TCell<U> = class(TObject)',
    '  private',
    '    FValue: U;',
    '  end;',
    '  TDeep = specialize TCell< specialize TCell< specialize TCell< '
      + 'specialize TCell<Integer> > > >;',
    'implementation',
    'end.');

  // Noncompliant: line 22 captures the control variable of the loop on line 19.
  cCaptureClassicLoop: array[0..24] of string = (
    'unit anonprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    'procedure Demo(aCount: Integer);',
    'implementation',
    'procedure Use(aValue: Integer);',
    'begin',
    'end;',
    'procedure Run(const aCallback: TIntegerCallback);',
    'begin',
    '  aCallback(1);',
    'end;',
    'procedure Demo(aCount: Integer);',
    'var',
    '  lIndex: Integer;',
    'begin',
    '  for lIndex := 1 to aCount do',
    '    Run(procedure(aValue: Integer)',
    '    begin',
    '      Use(lIndex);',
    '    end);',
    'end;',
    'end.');

  // The same capture in Delphi mode, where the modeswitch is implied.
  cCaptureClassicLoopDelphi: array[0..24] of string = (
    'unit anonprobe;',
    '{$mode delphi}{$H+}',
    'interface',
    'type',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    'procedure Demo(aCount: Integer);',
    'implementation',
    'procedure Use(aValue: Integer);',
    'begin',
    'end;',
    'procedure Run(const aCallback: TIntegerCallback);',
    'begin',
    '  aCallback(1);',
    'end;',
    'procedure Demo(aCount: Integer);',
    'var',
    '  lIndex: Integer;',
    'begin',
    '  for lIndex := 1 to aCount do',
    '    Run(procedure(aValue: Integer)',
    '    begin',
    '      Use(lIndex);',
    '    end);',
    'end;',
    'end.');

  // Noncompliant: the loop on line 21 runs inside a closure, and the closure on
  // line 22 captures its control variable.
  cCaptureLoopInsideClosure: array[0..27] of string = (
    'unit anonprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    'procedure Demo(aCount: Integer);',
    'implementation',
    'procedure Use(aValue: Integer);',
    'begin',
    'end;',
    'procedure Run(const aCallback: TIntegerCallback);',
    'begin',
    '  aCallback(1);',
    'end;',
    'procedure Demo(aCount: Integer);',
    'var',
    '  lIndex: Integer;',
    'begin',
    '  Run(procedure(aValue: Integer)',
    '  begin',
    '    for lIndex := 1 to aValue do',
    '      Run(procedure(aInner: Integer)',
    '      begin',
    '        Use(lIndex);',
    '      end);',
    '  end);',
    'end;',
    'end.');

  // Noncompliant: the capture on line 23 sits in a routine the closure declares.
  cCaptureNestedRoutineInClosure: array[0..28] of string = (
    'unit anonprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    'procedure Demo(aCount: Integer);',
    'implementation',
    'procedure Use(aValue: Integer);',
    'begin',
    'end;',
    'procedure Run(const aCallback: TIntegerCallback);',
    'begin',
    '  aCallback(1);',
    'end;',
    'procedure Demo(aCount: Integer);',
    'var',
    '  lIndex: Integer;',
    'begin',
    '  for lIndex := 1 to aCount do',
    '    Run(procedure(aValue: Integer)',
    '      procedure Deep;',
    '      begin',
    '        Use(lIndex);',
    '      end;',
    '    begin',
    '      Deep;',
    '    end);',
    'end;',
    'end.');

  // Noncompliant: a for-in control variable is reused exactly as a counter is.
  cCaptureForInLoop: array[0..25] of string = (
    'unit anonprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    '  TIntegerArray = array of Integer;',
    'procedure Demo(const aItems: TIntegerArray);',
    'implementation',
    'procedure Use(aValue: Integer);',
    'begin',
    'end;',
    'procedure Run(const aCallback: TIntegerCallback);',
    'begin',
    '  aCallback(1);',
    'end;',
    'procedure Demo(const aItems: TIntegerArray);',
    'var',
    '  lItem: Integer;',
    'begin',
    '  for lItem in aItems do',
    '    Run(procedure(aValue: Integer)',
    '    begin',
    '      Use(lItem);',
    '    end);',
    'end;',
    'end.');

  // Noncompliant: line 22 names the one captured variable twice.
  cCaptureLoopVarTwice: array[0..24] of string = (
    'unit anonprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    'procedure Demo(aCount: Integer);',
    'implementation',
    'procedure Use(aValue: Integer);',
    'begin',
    'end;',
    'procedure Run(const aCallback: TIntegerCallback);',
    'begin',
    '  aCallback(1);',
    'end;',
    'procedure Demo(aCount: Integer);',
    'var',
    '  lIndex: Integer;',
    'begin',
    '  for lIndex := 1 to aCount do',
    '    Run(procedure(aValue: Integer)',
    '    begin',
    '      Use(lIndex + lIndex);',
    '    end);',
    'end;',
    'end.');

  // Compliant: the loop variable is read where the loop runs, not in a closure.
  cCaptureLoopVarPlain: array[0..15] of string = (
    'unit anonprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Demo(aCount: Integer);',
    'implementation',
    'procedure Use(aValue: Integer);',
    'begin',
    'end;',
    'procedure Demo(aCount: Integer);',
    'var',
    '  lIndex: Integer;',
    'begin',
    '  for lIndex := 1 to aCount do',
    '    Use(lIndex);',
    'end;',
    'end.');

  // Compliant: the closure declares a local of its own under the same name.
  cCaptureClosureHomonym: array[0..27] of string = (
    'unit anonprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    'procedure Demo(aCount: Integer);',
    'implementation',
    'procedure Use(aValue: Integer);',
    'begin',
    'end;',
    'procedure Run(const aCallback: TIntegerCallback);',
    'begin',
    '  aCallback(1);',
    'end;',
    'procedure Demo(aCount: Integer);',
    'var',
    '  lIndex: Integer;',
    'begin',
    '  for lIndex := 1 to aCount do',
    '    Run(procedure(aValue: Integer)',
    '    var',
    '      lIndex: Integer;',
    '    begin',
    '      lIndex := aValue;',
    '      Use(lIndex);',
    '    end);',
    'end;',
    'end.');

  { Compliant for both capture rules: a routine-level local captured by a
    closure no loop encloses, in a routine that is not a method. }
  cCaptureRoutineLocal: array[0..24] of string = (
    'unit anonprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    'procedure Demo(aCount: Integer);',
    'implementation',
    'procedure Use(aValue: Integer);',
    'begin',
    'end;',
    'procedure Run(const aCallback: TIntegerCallback);',
    'begin',
    '  aCallback(1);',
    'end;',
    'procedure Demo(aCount: Integer);',
    'var',
    '  lTotal: Integer;',
    'begin',
    '  lTotal := aCount;',
    '  Run(procedure(aValue: Integer)',
    '  begin',
    '    Use(lTotal);',
    '  end);',
    'end;',
    'end.');

  // Noncompliant: the closure on line 19 writes a field through Self.
  cCaptureSelfExplicit: array[0..23] of string = (
    'unit anonprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    '  TCounter = class(TObject)',
    '  private',
    '    FCount: Integer;',
    '  public',
    '    procedure Bump;',
    '  end;',
    'implementation',
    'procedure Run(const aCallback: TIntegerCallback);',
    'begin',
    '  aCallback(1);',
    'end;',
    'procedure TCounter.Bump;',
    'begin',
    '  Run(procedure(aValue: Integer)',
    '  begin',
    '    Self.FCount := aValue;',
    '  end);',
    'end;',
    'end.');

  { Noncompliant: the closure on line 24 names a field and an instance method
    without a qualifier, which is the same capture. }
  cCaptureSelfImplicit: array[0..29] of string = (
    'unit anonprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    '  TCounter = class(TObject)',
    '  private',
    '    FCount: Integer;',
    '  public',
    '    procedure Touch;',
    '    procedure Bump;',
    '  end;',
    'implementation',
    'procedure Run(const aCallback: TIntegerCallback);',
    'begin',
    '  aCallback(1);',
    'end;',
    'procedure TCounter.Touch;',
    'begin',
    '  FCount := 0;',
    'end;',
    'procedure TCounter.Bump;',
    'begin',
    '  Run(procedure(aValue: Integer)',
    '  begin',
    '    FCount := aValue;',
    '    Touch;',
    '  end);',
    'end;',
    'end.');

  // The same unqualified capture in Delphi mode.
  cCaptureSelfImplicitDelphi: array[0..29] of string = (
    'unit anonprobe;',
    '{$mode delphi}{$H+}',
    'interface',
    'type',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    '  TCounter = class(TObject)',
    '  private',
    '    FCount: Integer;',
    '  public',
    '    procedure Touch;',
    '    procedure Bump;',
    '  end;',
    'implementation',
    'procedure Run(const aCallback: TIntegerCallback);',
    'begin',
    '  aCallback(1);',
    'end;',
    'procedure TCounter.Touch;',
    'begin',
    '  FCount := 0;',
    'end;',
    'procedure TCounter.Bump;',
    'begin',
    '  Run(procedure(aValue: Integer)',
    '  begin',
    '    FCount := aValue;',
    '    Touch;',
    '  end);',
    'end;',
    'end.');

  // Noncompliant once: three captures of the same instance in one closure.
  cCaptureSelfSeveral: array[0..26] of string = (
    'unit anonprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    '  TCounter = class(TObject)',
    '  private',
    '    FA: Integer;',
    '    FB: Integer;',
    '  public',
    '    procedure Bump;',
    '  end;',
    'implementation',
    'procedure Run(const aCallback: TIntegerCallback);',
    'begin',
    '  aCallback(1);',
    'end;',
    'procedure TCounter.Bump;',
    'begin',
    '  Run(procedure(aValue: Integer)',
    '  begin',
    '    FA := aValue;',
    '    FB := FA;',
    '    Self.FA := FB;',
    '  end);',
    'end;',
    'end.');

  // Compliant: the field the closure writes belongs to another instance.
  cCaptureSelfQualifiedOther: array[0..23] of string = (
    'unit anonprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    '  TCounter = class(TObject)',
    '  private',
    '    FCount: Integer;',
    '  public',
    '    procedure Bump(aOther: TCounter);',
    '  end;',
    'implementation',
    'procedure Run(const aCallback: TIntegerCallback);',
    'begin',
    '  aCallback(1);',
    'end;',
    'procedure TCounter.Bump(aOther: TCounter);',
    'begin',
    '  Run(procedure(aValue: Integer)',
    '  begin',
    '    aOther.FCount := aValue;',
    '  end);',
    'end;',
    'end.');

  // Compliant: a class var, a class const and a class method need no instance.
  cCaptureSelfClassMembers: array[0..30] of string = (
    'unit anonprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    '  TCounter = class(TObject)',
    '  private',
    '    class var FShared: Integer;',
    '  public',
    '    const cStep = 1;',
    '    class procedure Reset;',
    '    procedure Bump;',
    '  end;',
    'implementation',
    'procedure Run(const aCallback: TIntegerCallback);',
    'begin',
    '  aCallback(1);',
    'end;',
    'class procedure TCounter.Reset;',
    'begin',
    '  FShared := 0;',
    'end;',
    'procedure TCounter.Bump;',
    'begin',
    '  Run(procedure(aValue: Integer)',
    '  begin',
    '    FShared := cStep;',
    '    Reset;',
    '  end);',
    'end;',
    'end.');

  // Compliant: in a class method Self is the metaclass, so no instance is held.
  cCaptureSelfInClassMethod: array[0..25] of string = (
    'unit anonprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    '  TCounter = class(TObject)',
    '  public',
    '    class procedure Reset;',
    '    class procedure Spawn;',
    '  end;',
    'implementation',
    'procedure Run(const aCallback: TIntegerCallback);',
    'begin',
    '  aCallback(1);',
    'end;',
    'class procedure TCounter.Reset;',
    'begin',
    'end;',
    'class procedure TCounter.Spawn;',
    'begin',
    '  Run(procedure(aValue: Integer)',
    '  begin',
    '    Self.Reset;',
    '  end);',
    'end;',
    'end.');

  // Noncompliant: the closure on line 19 reaches a field from a raise operand.
  cCaptureSelfInRaise: array[0..23] of string = (
    'unit anonprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    '  TCounter = class(TObject)',
    '  private',
    '    FError: TObject;',
    '  public',
    '    procedure Attach;',
    '  end;',
    'implementation',
    'procedure Run(const aCallback: TIntegerCallback);',
    'begin',
    '  aCallback(1);',
    'end;',
    'procedure TCounter.Attach;',
    'begin',
    '  Run(procedure(aValue: Integer)',
    '  begin',
    '    raise FError;',
    '  end);',
    'end;',
    'end.');

  // One loop capture (line 24) and one Self capture (line 22): the resolved
  // control of the capture degradation tests.
  cCaptureBothShapes: array[0..26] of string = (
    'unit anonprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    '  TCounter = class(TObject)',
    '  private',
    '    FCount: Integer;',
    '  public',
    '    procedure Bump(aCount: Integer);',
    '  end;',
    'implementation',
    'procedure Run(const aCallback: TIntegerCallback);',
    'begin',
    '  aCallback(1);',
    'end;',
    'procedure TCounter.Bump(aCount: Integer);',
    'var',
    '  lIndex: Integer;',
    'begin',
    '  for lIndex := 1 to aCount do',
    '    Run(procedure(aValue: Integer)',
    '    begin',
    '      FCount := lIndex;',
    '    end);',
    'end;',
    'end.');

  // The same two shapes behind a uses clause naming a unit that cannot be found.
  cCaptureNoResolution: array[0..27] of string = (
    'unit anonprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses NoSuchUnitForFpSonar;',
    'type',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    '  TCounter = class(TObject)',
    '  private',
    '    FCount: Integer;',
    '  public',
    '    procedure Bump(aCount: Integer);',
    '  end;',
    'implementation',
    'procedure Run(const aCallback: TIntegerCallback);',
    'begin',
    '  aCallback(1);',
    'end;',
    'procedure TCounter.Bump(aCount: Integer);',
    'var',
    '  lIndex: Integer;',
    'begin',
    '  for lIndex := 1 to aCount do',
    '    Run(procedure(aValue: Integer)',
    '    begin',
    '      FCount := lIndex;',
    '    end);',
    'end;',
    'end.');

  // A classic loop capture (line 22) beside a for var one (line 27).
  cCaptureBothLoopForms: array[0..29] of string = (
    'unit anonprobe;',
    '{$mode delphi}{$H+}',
    'interface',
    'type',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    'procedure Demo(aCount: Integer);',
    'implementation',
    'procedure Use(aValue: Integer);',
    'begin',
    'end;',
    'procedure Run(const aCallback: TIntegerCallback);',
    'begin',
    '  aCallback(1);',
    'end;',
    'procedure Demo(aCount: Integer);',
    'var',
    '  lIndex: Integer;',
    'begin',
    '  for lIndex := 1 to aCount do',
    '    Run(procedure(aValue: Integer)',
    '    begin',
    '      Use(lIndex);',
    '    end);',
    '  for var lInner := 1 to aCount do',
    '    Run(procedure(aValue: Integer)',
    '    begin',
    '      Use(lInner);',
    '    end);',
    'end;',
    'end.');

  // Noncompliant once: line 22 is written once and specialized twice.
  cCaptureInSpecializedGeneric: array[0..29] of string = (
    'unit anonprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    'procedure Drive;',
    'implementation',
    'procedure Use(aValue: Integer);',
    'begin',
    'end;',
    'procedure Run(const aCallback: TIntegerCallback);',
    'begin',
    '  aCallback(1);',
    'end;',
    'generic procedure Emit<T>(aCount: Integer);',
    'var',
    '  lIndex: Integer;',
    'begin',
    '  for lIndex := 1 to aCount do',
    '    Run(procedure(aValue: Integer)',
    '    begin',
    '      Use(lIndex);',
    '    end);',
    'end;',
    'procedure Drive;',
    'begin',
    '  specialize Emit<Integer>(3);',
    '  specialize Emit<string>(4);',
    'end;',
    'end.');

  // Noncompliant: the closure on line 19 is an anonymous function.
  cCaptureAnonFunction: array[0..23] of string = (
    'unit anonprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TIntegerFunc = reference to function(aValue: Integer): Integer;',
    '  TCounter = class(TObject)',
    '  private',
    '    FCount: Integer;',
    '  public',
    '    procedure Attach;',
    '  end;',
    'implementation',
    'procedure Run(const aCallback: TIntegerFunc);',
    'begin',
    '  aCallback(1);',
    'end;',
    'procedure TCounter.Attach;',
    'begin',
    '  Run(function(aValue: Integer): Integer',
    '  begin',
    '    Result := FCount;',
    '  end);',
    'end;',
    'end.');

  // A static field needs no instance, so line 22 captures nothing.
  cCaptureSelfStaticField: array[0..24] of string = (
    'unit anonprobe;',
    '{$mode objfpc}{$H+}',
    '{$static on}',
    'interface',
    'type',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    '  TCounter = class(TObject)',
    '  private',
    '    FShared: Integer; static;',
    '  public',
    '    procedure Bump;',
    '  end;',
    'implementation',
    'procedure Run(const aCallback: TIntegerCallback);',
    'begin',
    '  aCallback(1);',
    'end;',
    'procedure TCounter.Bump;',
    'begin',
    '  Run(procedure(aValue: Integer)',
    '  begin',
    '    FShared := aValue;',
    '  end);',
    'end;',
    'end.');

  // The control variable and both members live in the unit the closure cannot
  // reach.
  cCaptureUnresolvedOperand: array[0..23] of string = (
    'unit anonprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses NoSuchUnitForFpSonar;',
    'type',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    '  TCounter = class(TMissingBase)',
    '  public',
    '    procedure Bump(aCount: Integer);',
    '  end;',
    'implementation',
    'procedure Run(const aCallback: TIntegerCallback);',
    'begin',
    '  aCallback(1);',
    'end;',
    'procedure TCounter.Bump(aCount: Integer);',
    'begin',
    '  for FMissingIndex := 1 to aCount do',
    '    Run(procedure(aValue: Integer)',
    '    begin',
    '      FMissingCount := FMissingIndex;',
    '    end);',
    'end;',
    'end.');


  // Noncompliant: line 7 decorates a field of an explicit private section.
  cAttrPrivateField: array[0..10] of string = (
    'unit attrprobe;',
    '{$mode delphi}{$H+}',
    'interface',
    'type',
    '  TDocumented = class(TObject)',
    '  private',
    '    [Mark]',
    '    FValue: Integer;',
    '  end;',
    'implementation',
    'end.');

  // The same field in objfpc, where the attribute syntax needs the modeswitch.
  cAttrPrivateFieldObjfpc: array[0..11] of string = (
    'unit attrprobe;',
    '{$mode objfpc}{$H+}',
    '{$modeswitch prefixedattributes}',
    'interface',
    'type',
    '  TDocumented = class(TObject)',
    '  private',
    '    [Mark]',
    '    FValue: Integer;',
    '  end;',
    'implementation',
    'end.');

  // Noncompliant three times: lines 7, 10 and 12 decorate the field, the method
  // and the property, none of which is published.
  cAttrThreeKinds: array[0..19] of string = (
    'unit attrprobe;',
    '{$mode delphi}{$H+}',
    'interface',
    'type',
    '  TDocumented = class(TObject)',
    '  private',
    '    [Mark]',
    '    FValue: Integer;',
    '  public',
    '    [Mark]',
    '    procedure SetValue(aValue: Integer);',
    '    [Mark]',
    '    property Value: Integer read FValue;',
    '  end;',
    'implementation',
    'procedure TDocumented.SetValue(aValue: Integer);',
    'begin',
    '  FValue := aValue;',
    'end;',
    'end.');

  // Compliant: a published member carries RTTI whatever the class declares.
  cAttrPublished: array[0..10] of string = (
    'unit attrprobe;',
    '{$mode delphi}{$H+}',
    'interface',
    'type',
    '  TDocumented = class(TObject)',
    '  published',
    '    [Mark]',
    '    FValue: Integer;',
    '  end;',
    'implementation',
    'end.');

  // Compliant: the implicit first section, which {$M+} may publish.
  cAttrImplicitSection: array[0..9] of string = (
    'unit attrprobe;',
    '{$mode delphi}{$H+}',
    'interface',
    'type',
    '  TDocumented = class(TObject)',
    '    [Mark]',
    '    FValue: Integer;',
    '  end;',
    'implementation',
    'end.');

  // The directive widens field RTTI to the private section, which this feed
  // does not carry: measured as one issue rather than zero.
  cAttrRttiWidened: array[0..11] of string = (
    'unit attrprobe;',
    '{$mode delphi}{$H+}',
    '{$RTTI EXPLICIT FIELDS([vcPrivate])}',
    'interface',
    'type',
    '  TDocumented = class(TObject)',
    '  private',
    '    [Mark]',
    '    FValue: Integer;',
    '  end;',
    'implementation',
    'end.');

  // Compliant: the attribute decorates the type, which is not a member.
  cAttrTypeLevel: array[0..10] of string = (
    'unit attrprobe;',
    '{$mode delphi}{$H+}',
    'interface',
    'type',
    '  [Mark]',
    '  TDocumented = class(TObject)',
    '  private',
    '    FValue: Integer;',
    '  end;',
    'implementation',
    'end.');

  // Compliant: a nested type and a nested constant are kinds RTTI never reaches
  // through the three-kind predicate.
  cAttrNestedKinds: array[0..15] of string = (
    'unit attrprobe;',
    '{$mode delphi}{$H+}',
    'interface',
    'type',
    '  TDocumented = class(TObject)',
    '  private',
    '    type',
    '      [Mark]',
    '      TInner = class(TObject)',
    '      end;',
    '    const',
    '      [Mark]',
    '      cStep = 1;',
    '  end;',
    'implementation',
    'end.');

  // Compliant: a record, an object and an interface are not okClass. The
  // interface carries its GUID first: a [ on the header line is parsed as the
  // GUID expression.
  cAttrNonClassContainers: array[0..18] of string = (
    'unit attrprobe;',
    '{$mode delphi}{$H+}',
    'interface',
    'type',
    '  TRec = record',
    '    [Mark]',
    '    Value: Integer;',
    '  end;',
    '  TObj = object',
    '  private',
    '    [Mark]',
    '    FValue: Integer;',
    '  end;',
    '  IThing = interface',
    '    [''{2C0D9A48-6C8F-4B1E-9E3D-6A1D2F4B7C55}'']',
    '    [Mark]',
    '    procedure Go;',
    '  end;',
    'end.');

  // The private-field shape behind a uses clause naming a unit that cannot be
  // found: the verdict rests on no resolved fact.
  cAttrNoResolution: array[0..11] of string = (
    'unit attrprobe;',
    '{$mode delphi}{$H+}',
    'interface',
    'uses NoSuchUnitForFpSonar;',
    'type',
    '  TDocumented = class(TObject)',
    '  private',
    '    [Mark]',
    '    FValue: Integer;',
    '  end;',
    'implementation',
    'end.');

  // The same shape behind a syntax error the parser cannot pass.
  cAttrParseFailure: array[0..11] of string = (
    'unit attrbroken;',
    '{$mode delphi}{$H+}',
    'interface',
    'type',
    '  TBroken = class(;',
    '  TDocumented = class(TObject)',
    '  private',
    '    [Mark]',
    '    FValue: Integer;',
    '  end;',
    'implementation',
    'end.');


function TRulesGenericsTest.EnabledConfig(
  const aIds: array of string): TFpSonarConfig;

var
  i: Integer;

begin
  Result := TFpSonarConfig.Default;
  SetLength(Result.Rules, Length(aIds));
  for i := 0 to High(aIds) do
  begin
    Result.Rules[i].RuleId := aIds[i];
    Result.Rules[i].HasEnabled := True;
    Result.Rules[i].Enabled := True;
  end;
end;


function TRulesGenericsTest.NewConstraintUnused: TRuleBase;

begin
  AssertNotNull(cConstraintUnusedId + ' is registered',
    RuleRegistry.FindById(cConstraintUnusedId));
  Result := TRuleGenericConstraintUnused.Create(
    RuleRegistry.FindById(cConstraintUnusedId).Metadata);
end;


function TRulesGenericsTest.NewUnconstrainedSpec: TRuleBase;

begin
  AssertNotNull(cUnconstrainedSpecId + ' is registered',
    RuleRegistry.FindById(cUnconstrainedSpecId));
  Result := TRuleSpecializationOfUnconstrainedGeneric.Create(
    RuleRegistry.FindById(cUnconstrainedSpecId).Metadata);
end;


function TRulesGenericsTest.NewNestingDepth: TRuleBase;

begin
  AssertNotNull(cNestingDepthId + ' is registered',
    RuleRegistry.FindById(cNestingDepthId));
  Result := TRuleNestedGenericSpecializationDepth.Create(
    RuleRegistry.FindById(cNestingDepthId).Metadata);
end;


function TRulesGenericsTest.NewCapturesLoopVar: TRuleBase;

begin
  AssertNotNull(cCapturesLoopVarId + ' is registered',
    RuleRegistry.FindById(cCapturesLoopVarId));
  Result := TRuleAnonymousMethodCapturesLoopVariable.Create(
    RuleRegistry.FindById(cCapturesLoopVarId).Metadata);
end;


function TRulesGenericsTest.NewCapturesSelf: TRuleBase;

begin
  AssertNotNull(cCapturesSelfId + ' is registered',
    RuleRegistry.FindById(cCapturesSelfId));
  Result := TRuleAnonymousMethodCapturesSelf.Create(
    RuleRegistry.FindById(cCapturesSelfId).Metadata);
end;


function TRulesGenericsTest.NewInlineVarCaptured: TRuleBase;

begin
  AssertNotNull(cInlineVarCapturedId + ' is registered',
    RuleRegistry.FindById(cInlineVarCapturedId));
  Result := TRuleNoInlineVarCapturedByAnonMethod.Create(
    RuleRegistry.FindById(cInlineVarCapturedId).Metadata);
end;


function TRulesGenericsTest.NewAttributeOnNonRtti: TRuleBase;

begin
  AssertNotNull(cAttributeNonRttiId + ' is registered',
    RuleRegistry.FindById(cAttributeNonRttiId));
  Result := TRuleAttributeOnNonRttiMember.Create(
    RuleRegistry.FindById(cAttributeNonRttiId).Metadata);
end;


function TRulesGenericsTest.Resolves(const aName: string;
  const aSrc: array of string): boolean;

var
  lFix: TTempFixtures;
  lRes: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;

begin
  lFix := TTempFixtures.Create;
  lRes := TFpSonarResolver.Create;
  try
    Result := lRes.BuildFor(lFix.Add(aName, aSrc), cMode, cDefines, [], [],
      lDiag) and lRes.Succeeded;
  finally
    lRes.Free;
    lFix.Free;
  end;
end;


procedure TRulesGenericsTest.RunAloneSrc(aRule: TRuleBase;
  const aRuleId, aName: string; const aSrc: array of string;
  const aCollector: TFpSonarIssueCollector);

var
  lFix: TTempFixtures;
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;

begin
  lFix := TTempFixtures.Create;
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    lEngine.Config := EnabledConfig([aRuleId]);
    lEngine.Analyze(lFix.Add(aName, aSrc), cMode, cDefines, aCollector);
  finally
    lEngine.Free;
    lReg.Free;
    lFix.Free;
  end;
end;


procedure TRulesGenericsTest.RunAloneCfgSrc(aRule: TRuleBase;
  const aName: string; const aSrc: array of string;
  const aConfig: TFpSonarConfig; const aCollector: TFpSonarIssueCollector);

var
  lFix: TTempFixtures;
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;

begin
  lFix := TTempFixtures.Create;
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    lEngine.Config := aConfig;
    lEngine.Analyze(lFix.Add(aName, aSrc), cMode, cDefines, aCollector);
  finally
    lEngine.Free;
    lReg.Free;
    lFix.Free;
  end;
end;


function TRulesGenericsTest.CountById(const aCollector: TFpSonarIssueCollector;
  const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesGenericsTest.FirstById(const aCollector: TFpSonarIssueCollector;
  const aId: string): Integer;

var
  i: Integer;

begin
  Result := -1;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Exit(i);
end;


procedure TRulesGenericsTest.GenericConstraintUnusedPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewConstraintUnused, cConstraintUnusedId, 'genprobe.pas',
      cConstraintIdle, lc);
    AssertEquals('the idle constraint draws one issue', 1,
      CountById(lc, cConstraintUnusedId));
    k := FirstById(lc, cConstraintUnusedId);
    AssertEquals('at the template parameter row', 5, lc.Issues[k].StartLine);
    AssertEquals('a point span', lc.Issues[k].StartLine, lc.Issues[k].EndLine);
    AssertEquals('column 1', 1, lc.Issues[k].StartCol);
    AssertEquals('column 1', 1, lc.Issues[k].EndCol);
    AssertEquals('the parameter is the sole argument', 1,
      Length(lc.Issues[k].MessageArgs));
    AssertEquals('the parameter name', 'T', lc.Issues[k].MessageArgs[0]);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.GenericConstraintUnusedPositionsPerMode;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewConstraintUnused, cConstraintUnusedId, 'genprobe.pas',
      cConstraintIdleDelphi, lc);
    AssertEquals('the Delphi syntax yields the same verdict, so the owner '
      + 'prefix survives the template list', 1,
      CountById(lc, cConstraintUnusedId));
    k := FirstById(lc, cConstraintUnusedId);
    AssertEquals('at the same row as the objfpc spelling', 5,
      lc.Issues[k].StartLine);
    AssertEquals('the parameter is the sole argument', 1,
      Length(lc.Issues[k].MessageArgs));
    AssertEquals('the parameter name', 'T', lc.Issues[k].MessageArgs[0]);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.GenericConstraintUnusedSilentShapes;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewConstraintUnused, cConstraintUnusedId, 'genprobe.pas',
      cConstraintRelied, lc);
    AssertEquals('a member selected on a field typed by the parameter => zero',
      0, CountById(lc, cConstraintUnusedId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewConstraintUnused, cConstraintUnusedId, 'genprobe.pas',
      cConstraintNamedDirectly, lc);
    AssertEquals('construction through the parameter and an is-test against it '
      + '=> zero', 0, CountById(lc, cConstraintUnusedId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewConstraintUnused, cConstraintUnusedId, 'genprobe.pas',
      cConstraintAbsent, lc);
    AssertEquals('an unconstrained parameter => zero', 0,
      CountById(lc, cConstraintUnusedId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewConstraintUnused, cConstraintUnusedId, 'genprobe.pas',
      cConstraintNilAssigned, lc);
    AssertEquals('nil assigned to a field typed by the parameter => zero', 0,
      CountById(lc, cConstraintUnusedId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewConstraintUnused, cConstraintUnusedId, 'genprobe.pas',
      cConstraintWithScope, lc);
    AssertEquals('a with scope over a field typed by the parameter => zero', 0,
      CountById(lc, cConstraintUnusedId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewConstraintUnused, cConstraintUnusedId, 'genprobe.pas',
      cConstraintKeyword, lc);
    AssertEquals('a keyword constraint => zero', 0,
      CountById(lc, cConstraintUnusedId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewConstraintUnused, cConstraintUnusedId, 'genprobe.pas',
      cConstraintNoBody, lc);
    AssertEquals('a generic with no implemented routine => zero', 0,
      CountById(lc, cConstraintUnusedId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewConstraintUnused, cConstraintUnusedId, 'genprobe.pas',
      cConstraintScopedLocal, lc);
    AssertEquals('a parameter-typed local relied on in its own routine, its '
      + 'name reused in another => zero', 0,
      CountById(lc, cConstraintUnusedId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.GenericConstraintUnusedSilentOnUnresolvedOperand;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewConstraintUnused, cConstraintUnusedId, 'genprobe.pas',
      cGenericsUnresolvedOperand, lc);
    AssertEquals('a constraint type outside the closure => zero', 0,
      CountById(lc, cConstraintUnusedId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.GenericConstraintUnusedDegradesWithoutResolver;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewConstraintUnused, cConstraintUnusedId, 'genprobe.pas',
      cGenericsAllShapes, lc);
    AssertEquals('the same shape resolved => one issue', 1,
      CountById(lc, cConstraintUnusedId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewConstraintUnused, cConstraintUnusedId, 'genprobe.pas',
      cGenericsNoResolution, lc);
    AssertEquals('an unresolvable closure gates the feed off', 0,
      CountById(lc, cConstraintUnusedId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.SpecializationOfUnconstrainedGenericPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnconstrainedSpec, cUnconstrainedSpecId, 'genprobe.pas',
      cSpecUnconstrained, lc);
    AssertEquals('the unconstrained specialization draws one issue', 1,
      CountById(lc, cUnconstrainedSpecId));
    k := FirstById(lc, cUnconstrainedSpecId);
    AssertEquals('at the specialization row', 9, lc.Issues[k].StartLine);
    AssertEquals('a point span', lc.Issues[k].StartLine, lc.Issues[k].EndLine);
    AssertEquals('column 1', 1, lc.Issues[k].StartCol);
    AssertEquals('column 1', 1, lc.Issues[k].EndCol);
    AssertEquals('the generic is the sole argument', 1,
      Length(lc.Issues[k].MessageArgs));
    AssertEquals('the generic name', 'TCell', lc.Issues[k].MessageArgs[0]);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnconstrainedSpec, cUnconstrainedSpecId, 'genprobe.pas',
      cSpecConstAndTypeParam, lc);
    AssertEquals('a const parameter does not constrain the type parameter', 1,
      CountById(lc, cUnconstrainedSpecId));
    k := FirstById(lc, cUnconstrainedSpecId);
    AssertEquals('at the specialization row', 9, lc.Issues[k].StartLine);
    AssertEquals('the generic name', 'TBuf', lc.Issues[k].MessageArgs[0]);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.SpecializationOfUnconstrainedGenericSilentShapes;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnconstrainedSpec, cUnconstrainedSpecId, 'genprobe.pas',
      cSpecPartlyConstrained, lc);
    AssertEquals('one constrained parameter of two => zero', 0,
      CountById(lc, cUnconstrainedSpecId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnconstrainedSpec, cUnconstrainedSpecId, 'genprobe.pas',
      cConstraintAbsent, lc);
    AssertEquals('a generic nothing specializes => zero', 0,
      CountById(lc, cUnconstrainedSpecId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnconstrainedSpec, cUnconstrainedSpecId, 'genprobe.pas',
      cSpecConstParamsOnly, lc);
    AssertEquals('const parameters and no type parameter => zero', 0,
      CountById(lc, cUnconstrainedSpecId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.
  SpecializationOfUnconstrainedGenericSilentOnUnresolvedOperand;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnconstrainedSpec, cUnconstrainedSpecId, 'genprobe.pas',
      cGenericsUnresolvedOperand, lc);
    AssertEquals('a specialized generic outside the closure => zero', 0,
      CountById(lc, cUnconstrainedSpecId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.
  SpecializationOfUnconstrainedGenericDegradesWithoutResolver;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnconstrainedSpec, cUnconstrainedSpecId, 'genprobe.pas',
      cGenericsAllShapes, lc);
    AssertEquals('the same shape resolved => one issue', 1,
      CountById(lc, cUnconstrainedSpecId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnconstrainedSpec, cUnconstrainedSpecId, 'genprobe.pas',
      cGenericsNoResolution, lc);
    AssertEquals('an unresolvable closure gates the feed off', 0,
      CountById(lc, cUnconstrainedSpecId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.NestedGenericSpecializationDepthPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNestingDepth, cNestingDepthId, 'genprobe.pas',
      cNestingDeep, lc);
    AssertEquals('the outermost specialization only', 1,
      CountById(lc, cNestingDepthId));
    k := FirstById(lc, cNestingDepthId);
    AssertEquals('at the specialization row', 9, lc.Issues[k].StartLine);
    AssertEquals('a point span', lc.Issues[k].StartLine, lc.Issues[k].EndLine);
    AssertEquals('column 1', 1, lc.Issues[k].StartCol);
    AssertEquals('column 1', 1, lc.Issues[k].EndCol);
    AssertEquals('the generic, the depth and the maximum', 3,
      Length(lc.Issues[k].MessageArgs));
    AssertEquals('the generic name', 'TCell', lc.Issues[k].MessageArgs[0]);
    AssertEquals('the measured depth', '4', lc.Issues[k].MessageArgs[1]);
    AssertEquals('the configured maximum', '3', lc.Issues[k].MessageArgs[2]);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.NestedGenericSpecializationDepthCountsWrittenNesting;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNestingDepth, cNestingDepthId, 'genprobe.pas',
      cNestingAliasChain, lc);
    AssertEquals('a chain of separately declared one-level specializations '
      + 'nests nothing', 0, CountById(lc, cNestingDepthId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.NestedGenericSpecializationDepthPositionsPerMode;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNestingDepth, cNestingDepthId, 'genprobe.pas',
      cNestingDeepDelphi, lc);
    AssertEquals('the Delphi syntax yields the same verdict', 1,
      CountById(lc, cNestingDepthId));
    k := FirstById(lc, cNestingDepthId);
    AssertEquals('at the same row as the objfpc spelling', 9,
      lc.Issues[k].StartLine);
    AssertEquals('the generic, the depth and the maximum', 3,
      Length(lc.Issues[k].MessageArgs));
    AssertEquals('the same measured depth', '4', lc.Issues[k].MessageArgs[1]);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.NestedGenericSpecializationDepthHonoursMaxDepth;

var
  lc: TFpSonarIssueCollector;
  lCfg: TFpSonarConfig;
  lErr: string;
  k: Integer;

begin
  AssertTrue('the configuration loads', lCfg.LoadFromJSON(
    '{"rules":{"NestedGenericSpecializationDepth":{"enabled":true,'
    + '"params":{"maxDepth":1}}}}', lErr));
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneCfgSrc(NewNestingDepth, 'genprobe.pas', cNestingShallow, lCfg, lc);
    AssertEquals('a maxDepth under the measured nesting flags the fixture the '
      + 'default leaves clean', 1, CountById(lc, cNestingDepthId));
    k := FirstById(lc, cNestingDepthId);
    AssertEquals('the measured depth', '2', lc.Issues[k].MessageArgs[1]);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.NestedGenericSpecializationDepthSilentShapes;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNestingDepth, cNestingDepthId, 'genprobe.pas',
      cNestingShallow, lc);
    AssertEquals('two levels under the default maximum => zero', 0,
      CountById(lc, cNestingDepthId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNestingDepth, cNestingDepthId, 'genprobe.pas',
      cSpecUnconstrained, lc);
    AssertEquals('a single unnested specialization => zero', 0,
      CountById(lc, cNestingDepthId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.
  NestedGenericSpecializationDepthSilentOnUnresolvedOperand;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNestingDepth, cNestingDepthId, 'genprobe.pas',
      cGenericsUnresolvedOperand, lc);
    AssertEquals('a nested target outside the closure => zero', 0,
      CountById(lc, cNestingDepthId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.
  NestedGenericSpecializationDepthDegradesWithoutResolver;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNestingDepth, cNestingDepthId, 'genprobe.pas',
      cGenericsAllShapes, lc);
    AssertEquals('the same shape resolved => one issue', 1,
      CountById(lc, cNestingDepthId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNestingDepth, cNestingDepthId, 'genprobe.pas',
      cGenericsNoResolution, lc);
    AssertEquals('an unresolvable closure gates the feed off', 0,
      CountById(lc, cNestingDepthId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.AnonymousMethodCapturesLoopVariablePositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesLoopVar, cCapturesLoopVarId, 'anonprobe.pas',
      cCaptureClassicLoop, lc);
    AssertEquals('the captured control variable draws one issue', 1,
      CountById(lc, cCapturesLoopVarId));
    k := FirstById(lc, cCapturesLoopVarId);
    AssertEquals('at the capturing reference row', 22, lc.Issues[k].StartLine);
    AssertEquals('a point span', lc.Issues[k].StartLine, lc.Issues[k].EndLine);
    AssertEquals('column 1', 1, lc.Issues[k].StartCol);
    AssertEquals('column 1', 1, lc.Issues[k].EndCol);
    AssertEquals('the variable is the sole argument', 1,
      Length(lc.Issues[k].MessageArgs));
    AssertEquals('the source spelling', 'lIndex', lc.Issues[k].MessageArgs[0]);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesLoopVar, cCapturesLoopVarId, 'anonprobe.pas',
      cCaptureLoopInsideClosure, lc);
    AssertEquals('a loop written inside a closure is judged too', 1,
      CountById(lc, cCapturesLoopVarId));
    AssertEquals('at the inner capturing reference row', 24,
      lc.Issues[FirstById(lc, cCapturesLoopVarId)].StartLine);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesLoopVar, cCapturesLoopVarId, 'anonprobe.pas',
      cCaptureNestedRoutineInClosure, lc);
    AssertEquals('a routine the closure declares is walked too', 1,
      CountById(lc, cCapturesLoopVarId));
    AssertEquals('at the nested routine row', 23,
      lc.Issues[FirstById(lc, cCapturesLoopVarId)].StartLine);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesLoopVar, cCapturesLoopVarId, 'anonprobe.pas',
      cCaptureForInLoop, lc);
    AssertEquals('a for-in control variable is reused just as a counter is', 1,
      CountById(lc, cCapturesLoopVarId));
    AssertEquals('the source spelling', 'lItem',
      lc.Issues[FirstById(lc, cCapturesLoopVarId)].MessageArgs[0]);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesLoopVar, cCapturesLoopVarId, 'anonprobe.pas',
      cCaptureLoopVarTwice, lc);
    AssertEquals('naming the one capture twice draws one issue, not two', 1,
      CountById(lc, cCapturesLoopVarId));
    AssertEquals('at the capturing reference row', 22,
      lc.Issues[FirstById(lc, cCapturesLoopVarId)].StartLine);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesLoopVar, cCapturesLoopVarId, 'anonprobe.pas',
      cCaptureInSpecializedGeneric, lc);
    AssertEquals('two specializations of one written capture draw one issue', 1,
      CountById(lc, cCapturesLoopVarId));
    AssertEquals('at the written capture row', 22,
      lc.Issues[FirstById(lc, cCapturesLoopVarId)].StartLine);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.AnonymousMethodCapturesLoopVariablePositionsPerMode;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesLoopVar, cCapturesLoopVarId, 'anonprobe.pas',
      cCaptureClassicLoopDelphi, lc);
    AssertEquals('the Delphi mode yields the same verdict', 1,
      CountById(lc, cCapturesLoopVarId));
    k := FirstById(lc, cCapturesLoopVarId);
    AssertEquals('at the same row as the objfpc spelling', 22,
      lc.Issues[k].StartLine);
    AssertEquals('the source spelling', 'lIndex', lc.Issues[k].MessageArgs[0]);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.AnonymousMethodCapturesLoopVariableSilentShapes;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesLoopVar, cCapturesLoopVarId, 'anonprobe.pas',
      cCaptureLoopVarPlain, lc);
    AssertEquals('a loop variable read outside a closure => zero', 0,
      CountById(lc, cCapturesLoopVarId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesLoopVar, cCapturesLoopVarId, 'anonprobe.pas',
      cCaptureClosureHomonym, lc);
    AssertEquals('a closure local of the same name => zero', 0,
      CountById(lc, cCapturesLoopVarId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesLoopVar, cCapturesLoopVarId, 'anonprobe.pas',
      cCaptureRoutineLocal, lc);
    AssertEquals('a routine-level local captured outside any loop => zero', 0,
      CountById(lc, cCapturesLoopVarId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.
  AnonymousMethodCapturesLoopVariableSilentOnUnresolvedOperand;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesLoopVar, cCapturesLoopVarId, 'anonprobe.pas',
      cCaptureUnresolvedOperand, lc);
    AssertEquals('a control variable outside the closure => zero', 0,
      CountById(lc, cCapturesLoopVarId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.
  AnonymousMethodCapturesLoopVariableDegradesWithoutResolver;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesLoopVar, cCapturesLoopVarId, 'anonprobe.pas',
      cCaptureBothShapes, lc);
    AssertEquals('the same shape resolved => one issue', 1,
      CountById(lc, cCapturesLoopVarId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesLoopVar, cCapturesLoopVarId, 'anonprobe.pas',
      cCaptureNoResolution, lc);
    AssertEquals('an unresolvable closure gates the feed off', 0,
      CountById(lc, cCapturesLoopVarId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.AnonymousMethodCapturesSelfPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesSelf, cCapturesSelfId, 'anonprobe.pas',
      cCaptureSelfExplicit, lc);
    AssertEquals('the explicit Self draws one issue', 1,
      CountById(lc, cCapturesSelfId));
    k := FirstById(lc, cCapturesSelfId);
    AssertEquals('at the anonymous procedure row', 19, lc.Issues[k].StartLine);
    AssertEquals('a point span', lc.Issues[k].StartLine, lc.Issues[k].EndLine);
    AssertEquals('column 1', 1, lc.Issues[k].StartCol);
    AssertEquals('column 1', 1, lc.Issues[k].EndCol);
    AssertEquals('the enclosing routine is the sole argument', 1,
      Length(lc.Issues[k].MessageArgs));
    AssertEquals('the enclosing routine name', 'TCounter.Bump',
      lc.Issues[k].MessageArgs[0]);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesSelf, cCapturesSelfId, 'anonprobe.pas',
      cCaptureSelfImplicit, lc);
    AssertEquals('an unqualified field and instance method draw one issue', 1,
      CountById(lc, cCapturesSelfId));
    k := FirstById(lc, cCapturesSelfId);
    AssertEquals('at the anonymous procedure row', 24, lc.Issues[k].StartLine);
    AssertEquals('the enclosing routine name', 'TCounter.Bump',
      lc.Issues[k].MessageArgs[0]);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesSelf, cCapturesSelfId, 'anonprobe.pas',
      cCaptureSelfSeveral, lc);
    AssertEquals('three captures in one closure draw one issue', 1,
      CountById(lc, cCapturesSelfId));
    k := FirstById(lc, cCapturesSelfId);
    AssertEquals('at the anonymous procedure row', 20, lc.Issues[k].StartLine);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesSelf, cCapturesSelfId, 'anonprobe.pas',
      cCaptureSelfInRaise, lc);
    AssertEquals('a raise operand is scanned like any other expression', 1,
      CountById(lc, cCapturesSelfId));
    k := FirstById(lc, cCapturesSelfId);
    AssertEquals('at the anonymous procedure row', 19, lc.Issues[k].StartLine);
    AssertEquals('the enclosing routine name', 'TCounter.Attach',
      lc.Issues[k].MessageArgs[0]);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesSelf, cCapturesSelfId, 'anonprobe.pas',
      cCaptureAnonFunction, lc);
    AssertEquals('an anonymous function is judged like a procedure', 1,
      CountById(lc, cCapturesSelfId));
    k := FirstById(lc, cCapturesSelfId);
    AssertEquals('at the anonymous function row', 19, lc.Issues[k].StartLine);
    AssertEquals('the enclosing routine name', 'TCounter.Attach',
      lc.Issues[k].MessageArgs[0]);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.AnonymousMethodCapturesSelfPositionsPerMode;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesSelf, cCapturesSelfId, 'anonprobe.pas',
      cCaptureSelfImplicitDelphi, lc);
    AssertEquals('the Delphi mode yields the same verdict', 1,
      CountById(lc, cCapturesSelfId));
    k := FirstById(lc, cCapturesSelfId);
    AssertEquals('at the same row as the objfpc spelling', 24,
      lc.Issues[k].StartLine);
    AssertEquals('the enclosing routine name', 'TCounter.Bump',
      lc.Issues[k].MessageArgs[0]);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.AnonymousMethodCapturesSelfSilentShapes;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesSelf, cCapturesSelfId, 'anonprobe.pas',
      cCaptureSelfQualifiedOther, lc);
    AssertEquals('a field reached through another instance => zero', 0,
      CountById(lc, cCapturesSelfId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesSelf, cCapturesSelfId, 'anonprobe.pas',
      cCaptureSelfClassMembers, lc);
    AssertEquals('a class var, a class const and a class method => zero', 0,
      CountById(lc, cCapturesSelfId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesSelf, cCapturesSelfId, 'anonprobe.pas',
      cCaptureRoutineLocal, lc);
    AssertEquals('a closure in a routine that is not a method => zero', 0,
      CountById(lc, cCapturesSelfId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesSelf, cCapturesSelfId, 'anonprobe.pas',
      cCaptureSelfInClassMethod, lc);
    AssertEquals('Self inside a class method is the metaclass => zero', 0,
      CountById(lc, cCapturesSelfId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesSelf, cCapturesSelfId, 'anonprobe.pas',
      cCaptureSelfStaticField, lc);
    AssertEquals('a static field is reached without an instance => zero', 0,
      CountById(lc, cCapturesSelfId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.
  AnonymousMethodCapturesSelfSilentOnUnresolvedOperand;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesSelf, cCapturesSelfId, 'anonprobe.pas',
      cCaptureUnresolvedOperand, lc);
    AssertEquals('members declared outside the closure => zero', 0,
      CountById(lc, cCapturesSelfId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.
  AnonymousMethodCapturesSelfDegradesWithoutResolver;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesSelf, cCapturesSelfId, 'anonprobe.pas',
      cCaptureBothShapes, lc);
    AssertEquals('the same shape resolved => one issue', 1,
      CountById(lc, cCapturesSelfId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesSelf, cCapturesSelfId, 'anonprobe.pas',
      cCaptureNoResolution, lc);
    AssertEquals('an unresolvable closure gates the feed off', 0,
      CountById(lc, cCapturesSelfId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.
  AnonymousMethodCaptureRulesAreDisjointFromInlineVarRule;

var
  lc: TFpSonarIssueCollector;

begin
  AssertTrue('the classic loop fixture resolves, so both verdicts are '
    + 'measured rather than degraded',
    Resolves('anonprobe.pas', cCaptureClassicLoop));

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesLoopVar, cCapturesLoopVarId, 'anonprobe.pas',
      cCaptureClassicLoop, lc);
    AssertEquals('the classic loop site is this rule''s', 1,
      CountById(lc, cCapturesLoopVarId));
    AssertEquals('at the capturing reference row', 22,
      lc.Issues[FirstById(lc, cCapturesLoopVarId)].StartLine);
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewInlineVarCaptured, cInlineVarCapturedId, 'anonprobe.pas',
      cCaptureClassicLoop, lc);
    AssertEquals('the neighbour claims no classic loop site', 0,
      CountById(lc, cInlineVarCapturedId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  AssertFalse('a for var control variable is not declared by this resolver, so '
    + 'the fixture carrying both loop forms does not resolve at all',
    Resolves('anonprobe.pas', cCaptureBothLoopForms));

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesLoopVar, cCapturesLoopVarId, 'anonprobe.pas',
      cCaptureBothLoopForms, lc);
    AssertEquals('so neither rule reports the mixed fixture', 0,
      CountById(lc, cCapturesLoopVarId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewInlineVarCaptured, cInlineVarCapturedId, 'anonprobe.pas',
      cCaptureBothLoopForms, lc);
    AssertEquals('and no row is reported by both', 0,
      CountById(lc, cInlineVarCapturedId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.AttributeOnNonRttiMemberPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewAttributeOnNonRtti, cAttributeNonRttiId, 'attrprobe.pas',
      cAttrPrivateField, lc);
    AssertEquals('the private field draws one issue', 1,
      CountById(lc, cAttributeNonRttiId));
    k := FirstById(lc, cAttributeNonRttiId);
    AssertEquals('at the attribute row', 7, lc.Issues[k].StartLine);
    AssertEquals('a point span', lc.Issues[k].StartLine, lc.Issues[k].EndLine);
    AssertEquals('column 1', 1, lc.Issues[k].StartCol);
    AssertEquals('column 1', 1, lc.Issues[k].EndCol);
    AssertEquals('the member is the sole argument', 1,
      Length(lc.Issues[k].MessageArgs));
    AssertEquals('the member name', 'FValue', lc.Issues[k].MessageArgs[0]);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewAttributeOnNonRtti, cAttributeNonRttiId, 'attrprobe.pas',
      cAttrThreeKinds, lc);
    AssertEquals('the field, the method and the property each draw one', 3,
      CountById(lc, cAttributeNonRttiId));
    AssertEquals('the first at the field attribute row', 7,
      lc.Issues[FirstById(lc, cAttributeNonRttiId)].StartLine);
    AssertEquals('the field name first', 'FValue',
      lc.Issues[FirstById(lc, cAttributeNonRttiId)].MessageArgs[0]);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewAttributeOnNonRtti, cAttributeNonRttiId, 'attrprobe.pas',
      cAttrRttiWidened, lc);
    { Measured, not intended: the AST parse withholds po_CheckDirectiveRTTI, so
      the directive never reaches TPasClassType.RTTIVisibility and HasExtRTTI
      collapses to the published test. }
    AssertEquals('a $RTTI directive widening field RTTI to the private section '
      + 'is not carried by this feed, so the field still reports', 1,
      CountById(lc, cAttributeNonRttiId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.AttributeOnNonRttiMemberPositionsPerMode;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewAttributeOnNonRtti, cAttributeNonRttiId, 'attrprobe.pas',
      cAttrPrivateFieldObjfpc, lc);
    AssertEquals('objfpc with the modeswitch yields the same verdict as the '
      + 'delphi spelling', 1, CountById(lc, cAttributeNonRttiId));
    k := FirstById(lc, cAttributeNonRttiId);
    AssertEquals('at the attribute row the extra directive line shifted', 8,
      lc.Issues[k].StartLine);
    AssertEquals('the member is the sole argument', 1,
      Length(lc.Issues[k].MessageArgs));
    AssertEquals('the member name', 'FValue', lc.Issues[k].MessageArgs[0]);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.AttributeOnNonRttiMemberSilentShapes;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewAttributeOnNonRtti, cAttributeNonRttiId, 'attrprobe.pas',
      cAttrPublished, lc);
    AssertEquals('a published member => zero', 0,
      CountById(lc, cAttributeNonRttiId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewAttributeOnNonRtti, cAttributeNonRttiId, 'attrprobe.pas',
      cAttrImplicitSection, lc);
    AssertEquals('the implicit section, which {$M+} may publish => zero', 0,
      CountById(lc, cAttributeNonRttiId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewAttributeOnNonRtti, cAttributeNonRttiId, 'attrprobe.pas',
      cAttrTypeLevel, lc);
    AssertEquals('an attribute on the type itself => zero', 0,
      CountById(lc, cAttributeNonRttiId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewAttributeOnNonRtti, cAttributeNonRttiId, 'attrprobe.pas',
      cAttrNestedKinds, lc);
    AssertEquals('a nested type and a nested constant => zero', 0,
      CountById(lc, cAttributeNonRttiId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewAttributeOnNonRtti, cAttributeNonRttiId, 'attrprobe.pas',
      cAttrNonClassContainers, lc);
    AssertEquals('a record, an object and an interface => zero', 0,
      CountById(lc, cAttributeNonRttiId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.
  AttributeOnNonRttiMemberIsAstTierNotResolverBound;

var
  lc: TFpSonarIssueCollector;

begin
  AssertFalse('the unit naming a missing dependency does not resolve',
    Resolves('attrprobe.pas', cAttrNoResolution));

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewAttributeOnNonRtti, cAttributeNonRttiId, 'attrprobe.pas',
      cAttrNoResolution, lc);
    AssertEquals('an unresolvable unit still reports, because the verdict uses '
      + 'no resolved fact', 1, CountById(lc, cAttributeNonRttiId));
    AssertEquals('at the attribute row', 8,
      lc.Issues[FirstById(lc, cAttributeNonRttiId)].StartLine);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewAttributeOnNonRtti, cAttributeNonRttiId, 'attrbroken.pas',
      cAttrParseFailure, lc);
    AssertTrue('the fixture really fails to parse',
      CountById(lc, cParseErrorId) > 0);
    AssertEquals('a failed parse leaves no module, so the feed is withheld', 0,
      CountById(lc, cAttributeNonRttiId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.GenericsRulesSurviveParseFailure;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewConstraintUnused, cConstraintUnusedId, 'genbroken.pas',
      cGenericsParseFailure, lc);
    AssertTrue('the fixture really fails to parse',
      CountById(lc, cParseErrorId) > 0);
    AssertEquals('a failed parse is silent', 0,
      CountById(lc, cConstraintUnusedId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnconstrainedSpec, cUnconstrainedSpecId, 'genbroken.pas',
      cGenericsParseFailure, lc);
    AssertEquals('a failed parse is silent', 0,
      CountById(lc, cUnconstrainedSpecId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNestingDepth, cNestingDepthId, 'genbroken.pas',
      cGenericsParseFailure, lc);
    AssertEquals('a failed parse is silent', 0,
      CountById(lc, cNestingDepthId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesLoopVar, cCapturesLoopVarId, 'genbroken.pas',
      cGenericsParseFailure, lc);
    AssertEquals('a failed parse is silent', 0,
      CountById(lc, cCapturesLoopVarId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewCapturesSelf, cCapturesSelfId, 'genbroken.pas',
      cGenericsParseFailure, lc);
    AssertEquals('a failed parse is silent', 0,
      CountById(lc, cCapturesSelfId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewAttributeOnNonRtti, cAttributeNonRttiId, 'genbroken.pas',
      cGenericsParseFailure, lc);
    AssertEquals('a failed parse is silent', 0,
      CountById(lc, cAttributeNonRttiId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesGenericsTest.GenericsRulesSelfRegisterGlobally;

var
  lRule: TRuleBase;
  lSpec: TRuleParamSpec;

begin
  lRule := RuleRegistry.FindById(cConstraintUnusedId);
  AssertTrue(cConstraintUnusedId + ' registered', lRule <> nil);
  AssertFalse(cConstraintUnusedId + ' ships disabled',
    lRule.Metadata.DefaultEnabled);
  AssertTrue(cConstraintUnusedId + ' carries a description',
    lRule.Metadata.Description <> '');
  AssertEquals('tier rtSem', Ord(rtSem), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfResolver', Ord(rfResolver), Ord(lRule.Metadata.Feed));
  AssertEquals('confidence cfLow', Ord(cfLow),
    Ord(lRule.Metadata.DefaultConfidence));
  AssertEquals('severity sevMinor', Ord(sevMinor), Ord(lRule.Metadata.Severity));
  AssertEquals('category itCodeSmell', Ord(itCodeSmell),
    Ord(lRule.Metadata.Category));

  lRule := RuleRegistry.FindById(cUnconstrainedSpecId);
  AssertTrue(cUnconstrainedSpecId + ' registered', lRule <> nil);
  AssertFalse(cUnconstrainedSpecId + ' ships disabled',
    lRule.Metadata.DefaultEnabled);
  AssertTrue(cUnconstrainedSpecId + ' carries a description',
    lRule.Metadata.Description <> '');
  AssertEquals('tier rtSem', Ord(rtSem), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfResolver', Ord(rfResolver), Ord(lRule.Metadata.Feed));
  AssertEquals('confidence cfMedium', Ord(cfMedium),
    Ord(lRule.Metadata.DefaultConfidence));
  AssertEquals('severity sevInfo', Ord(sevInfo), Ord(lRule.Metadata.Severity));
  AssertEquals('category itCodeSmell', Ord(itCodeSmell),
    Ord(lRule.Metadata.Category));

  lRule := RuleRegistry.FindById(cNestingDepthId);
  AssertTrue(cNestingDepthId + ' registered', lRule <> nil);
  AssertFalse(cNestingDepthId + ' ships disabled',
    lRule.Metadata.DefaultEnabled);
  AssertTrue(cNestingDepthId + ' carries a description',
    lRule.Metadata.Description <> '');
  AssertEquals('tier rtSem', Ord(rtSem), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfResolver', Ord(rfResolver), Ord(lRule.Metadata.Feed));
  AssertEquals('confidence cfMedium', Ord(cfMedium),
    Ord(lRule.Metadata.DefaultConfidence));
  AssertEquals('severity sevMinor', Ord(sevMinor), Ord(lRule.Metadata.Severity));
  AssertEquals('category itCodeSmell', Ord(itCodeSmell),
    Ord(lRule.Metadata.Category));
  AssertEquals('one declared parameter', 1,
    Length(lRule.Metadata.ParamSpecs));
  lSpec := lRule.Metadata.ParamSpecs[0];
  AssertEquals('named maxDepth', cMaxDepthParam, lSpec.Name);
  AssertEquals('of integer kind', Ord(rpkInt), Ord(lSpec.Kind));
  AssertTrue('carrying a default', lSpec.DefaultValue <> '');

  lRule := RuleRegistry.FindById(cCapturesLoopVarId);
  AssertTrue(cCapturesLoopVarId + ' registered', lRule <> nil);
  AssertFalse(cCapturesLoopVarId + ' ships disabled',
    lRule.Metadata.DefaultEnabled);
  AssertTrue(cCapturesLoopVarId + ' carries a description',
    lRule.Metadata.Description <> '');
  AssertEquals('tier rtSem', Ord(rtSem), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfResolver', Ord(rfResolver), Ord(lRule.Metadata.Feed));
  AssertEquals('confidence cfMedium', Ord(cfMedium),
    Ord(lRule.Metadata.DefaultConfidence));
  AssertEquals('severity sevMajor', Ord(sevMajor), Ord(lRule.Metadata.Severity));
  AssertEquals('category itBug', Ord(itBug), Ord(lRule.Metadata.Category));
  AssertEquals('no declared parameter', 0, Length(lRule.Metadata.ParamSpecs));

  lRule := RuleRegistry.FindById(cCapturesSelfId);
  AssertTrue(cCapturesSelfId + ' registered', lRule <> nil);
  AssertFalse(cCapturesSelfId + ' ships disabled',
    lRule.Metadata.DefaultEnabled);
  AssertTrue(cCapturesSelfId + ' carries a description',
    lRule.Metadata.Description <> '');
  AssertEquals('tier rtSem', Ord(rtSem), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfResolver', Ord(rfResolver), Ord(lRule.Metadata.Feed));
  AssertEquals('confidence cfLow', Ord(cfLow),
    Ord(lRule.Metadata.DefaultConfidence));
  AssertEquals('severity sevInfo', Ord(sevInfo), Ord(lRule.Metadata.Severity));
  AssertEquals('category itCodeSmell', Ord(itCodeSmell),
    Ord(lRule.Metadata.Category));
  AssertEquals('no declared parameter', 0, Length(lRule.Metadata.ParamSpecs));

  lRule := RuleRegistry.FindById(cAttributeNonRttiId);
  AssertTrue(cAttributeNonRttiId + ' registered', lRule <> nil);
  AssertFalse(cAttributeNonRttiId + ' ships disabled',
    lRule.Metadata.DefaultEnabled);
  AssertTrue(cAttributeNonRttiId + ' carries a description',
    lRule.Metadata.Description <> '');
  AssertEquals('tier rtAst', Ord(rtAst), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfAst', Ord(rfAst), Ord(lRule.Metadata.Feed));
  AssertEquals('confidence cfMedium', Ord(cfMedium),
    Ord(lRule.Metadata.DefaultConfidence));
  AssertEquals('severity sevMinor', Ord(sevMinor), Ord(lRule.Metadata.Severity));
  AssertEquals('category itCodeSmell', Ord(itCodeSmell),
    Ord(lRule.Metadata.Category));
  AssertEquals('no declared parameter', 0, Length(lRule.Metadata.ParamSpecs));
end;


initialization
  RegisterTest(TRulesGenericsTest);

end.
