{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the USE-tier unused-declaration rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesUnused;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Config, FpSonar.Traversal, FpSonar.Engine,
  FpSonar.Rules.Unused, UtstFixtures;

type
  // A factory yielding a fresh rule instance for aId (so noncompliant and the
  // compliant/canary runs each get their own, locally-owned object).
  TRuleBaseClassFactory = function(const aId: string): TRuleBase of object;

  { USE-tier unused-declaration rule position + canary + registration tests. }
  TRulesUnusedTest = class(TTestCase)
  private
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      const aCollector: TFpSonarIssueCollector);
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // Asserts NewRule fires once at aDeclLine, column 1, with arg [aName]; and
    // zero on both the compliant and canary fixtures (the safe- direction
    // guard). Fixtures supplied inline, materialised to a temp dir.
    procedure CheckUnusedRuleSrc(aRuleClass: TRuleBaseClassFactory;
      const aId: string; aDeclLine: Integer; const aName: string;
      const aNoncompliant, aCompliant, aCanary: array of string);
    function NewRule(const aId: string): TRuleBase;
    // Builds a project-wide index over aFiles, caller frees it.
    function BuildIndex(const aFiles: array of string): TFpSonarProjectIndex;
    // Runs aRule over aSubject with aIndex + aConfig attached (project scope).
    procedure RunRuleWithIndex(aRule: TRuleBase; const aSubject: string;
      aIndex: TFpSonarProjectIndex; const aConfig: TFpSonarConfig;
      const aCollector: TFpSonarIssueCollector);
    // Builds index over aFiles, runs aId on aSubject, asserts EXACTLY one finding
    // at aDeclLine col 1 carrying aName (compliant + collision-canary candidates
    // in the same project are silent — proven by the exact count).
    procedure CheckProjectRule(const aId: string; const aFiles: array of string;
      const aSubject: string; aDeclLine: Integer; const aName: string);
    // True iff some issue of aId carries aArg as its first message arg.
    function HasArg(const aCollector: TFpSonarIssueCollector;
      const aId, aArg: string): Boolean;
    // Materialises the six imp_*.pas fixtures of the RemoveUnusedImports
    // synthetic project into aTmp and returns their paths (imp_user first).
    function WriteImportsProject(aTmp: TTempFixtures): TStringArray;
    // Runs aRule over aFixture with useTier.resolution = aResolution threaded
    // into the engine config (mirrors RunRule, plus the config set).
    procedure RunRuleResolved(aRule: TRuleBase; const aFixture: string;
      aResolution: TFpSonarUseTierResolution;
      const aCollector: TFpSonarIssueCollector);
    // TFpSonarConfig.Default with aRuleId enabled and resolution preferred.
    function EnabledConfig(const aRuleId: string): TFpSonarConfig;
    // Runs aRule enabled under utrPrefer, withholding resolution when aWithhold.
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      aWithhold: boolean; const aCollector: TFpSonarIssueCollector); overload;
    // How often aId fires on aSource under utrPrefer, resolution optionally withheld.
    function PreferredCount(const aId: string; const aSource: array of string;
      aWithhold: boolean): Integer;
    // Asserts aId fires once at aDeclLine, column 1, with arg [aName] on
    // aNoncompliant and zero on aCompliant, both under utrPrefer.
    procedure CheckNewRule(const aId: string; aDeclLine: Integer;
      const aName: string; const aNoncompliant, aCompliant: array of string);
    // Runs aId over aSubject with an index built from aFiles.
    function IndexedCount(const aId: string; const aFiles: array of string;
      const aSubject: string): Integer;
    // Asserts aId fires once at aDeclLine, column 1, with arg [aName].
    procedure CheckIndexedRule(const aId: string; const aFiles: array of string;
      const aSubject: string; aDeclLine: Integer; const aName: string);
  published
    procedure ResolutionPrecisionIncrement;
    procedure ResolutionMonotonic;
    procedure ResolutionDegradesWhenResolverFails;
    procedure UnusedLocalVariablePositions;
    procedure UnusedLocalVariableAsmCanary;
    procedure UnusedFieldPositions;
    procedure UnusedPropertyPositions;
    procedure UnusedConstantPositions;
    procedure UnusedRoutinePositions;
    procedure UnusedTypePositions;
    procedure UnusedGlobalVariableProjectScope;
    procedure UnusedTypeProjectScope;
    procedure UnusedRoutineProjectScope;
    procedure UnusedImportsProjectScope;
    procedure UnusedImportsOptInFlags;
    procedure RulesSelfRegisterGlobally;
    procedure RemoveUnusedParameterPositions;
    procedure ParameterAssignedButNeverUsedPositions;
    procedure UnusedExceptionVariablePositions;
    procedure UnusedLabelPositions;
    procedure UnusedGenericParameterPositions;
    procedure UnusedUnitInInterfacePositions;
    procedure PrivateMemberOnlyUsedByOneMethodPositions;
    procedure WriteOnlyVariablePositions;
    procedure NewRulesDegradeWithoutResolver;
    procedure NewRulesSilentOnUnresolvedOperand;
    procedure NewRulesIndirectUseChannels;
  end;


implementation

const
  cMode = 'OBJFPC';
  cDefines: array[0..3] of string = ('FPC', 'CPUX86_64', 'UNIX', 'LINUX');

  cLocalId = 'RemoveUnusedLocalVariable';
  cFieldId = 'RemoveUnusedField';
  cPropertyId = 'RemoveUnusedProperty';
  cConstantId = 'RemoveUnusedConstant';
  cRoutineId = 'RemoveUnusedRoutine';
  cTypeId = 'RemoveUnusedType';
  cImportId = 'RemoveUnusedImports';
  cGlobalId = 'RemoveUnusedGlobalVariable';
  cParamId = 'RemoveUnusedParameter';
  cParamAssignedId = 'ParameterAssignedButNeverUsed';
  cExceptVarId = 'UnusedExceptionVariable';
  cLabelId = 'UnusedLabel';
  cGenericParamId = 'UnusedGenericParameter';
  cIfaceUnitId = 'UnusedUnitInInterface';
  cOneMethodId = 'PrivateMemberOnlyUsedByOneMethod';
  cWriteOnlyId = 'WriteOnlyVariable';

  // Embedded RemoveUnusedImports cross-unit fixtures: imp_user imports six
  // units and only imp_unused is referenced nowhere (uses-clause line 8).
  // Written as siblings. Line i+1 == [i].
  cImpUser: array[0..22] of string = (
    'unit imp_user;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  imp_used,',
    '  imp_unused,',
    '  imp_collision,',
    '  imp_operator,',
    '  imp_sideeffect;',
    '',
    'implementation',
    '',
    'procedure DoStuff;',
    'var',
    '  SharedName: Integer;',
    'begin',
    '  UsedExport;',
    '  SharedName := 1;',
    'end;',
    '',
    'end.');
  cImpUsed: array[0..13] of string = (
    'unit imp_used;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure UsedExport;',
    '',
    'implementation',
    '',
    'procedure UsedExport;',
    'begin',
    'end;',
    '',
    'end.');
  cImpUnused: array[0..13] of string = (
    'unit imp_unused;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure UnusedExport;',
    '',
    'implementation',
    '',
    'procedure UnusedExport;',
    'begin',
    'end;',
    '',
    'end.');
  cImpCollision: array[0..13] of string = (
    'unit imp_collision;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure SharedName;',
    '',
    'implementation',
    '',
    'procedure SharedName;',
    'begin',
    'end;',
    '',
    'end.');
  cImpOperator: array[0..19] of string = (
    'unit imp_operator;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TVec = record',
    '    X: Integer;',
    '  end;',
    '',
    'operator + (a, b: TVec): TVec;',
    '',
    'implementation',
    '',
    'operator + (a, b: TVec): TVec;',
    'begin',
    '  Result.X := a.X + b.X;',
    'end;',
    '',
    'end.');
  cImpSideEffect: array[0..10] of string = (
    'unit imp_sideeffect;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'initialization',
    '  Randomize;',
    '',
    'end.');

  // Embedded RemoveUnused* fixtures: line i+1 == [i].

  cLocalNoncompliant: array[0..14] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure DoWork;',
    'var',
    '  lUnused: Integer;',
    'begin',
    '  Writeln(''hi'');',
    'end;',
    '',
    'end.');

  cLocalCompliant: array[0..15] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure DoWork;',
    'var',
    '  lUsed: Integer;',
    'begin',
    '  lUsed := 1;',
    '  Writeln(lUsed);',
    'end;',
    '',
    'end.');

  cLocalCanary: array[0..23] of string = (
    'unit canary;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TRec = record',
    '    lShared: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure DoWork;',
    'var',
    '  lShared: Integer;',
    '  lR: TRec;',
    'begin',
    '  // `with` member access is ambiguous: lShared could be the local or lR.lShared.',
    '  // The conservative oracle keeps the local used -> NOT flagged.',
    '  with lR do',
    '    lShared := 1;',
    'end;',
    '',
    'end.');

  cLocalAsmCanary: array[0..20] of string = (
    'unit asmcanary;',
    '{$mode objfpc}{$H+}',
    '{$asmmode intel}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure DoWork;',
    'var',
    '  lOnlyInAsm: Integer;',
    'begin',
    '  // The local is referenced ONLY inside an asm block, whose body is a raw token',
    '  // stream the AST does not model as expression nodes. The analyzer harvests asm',
    '  // identifiers, so the local is conservatively kept -> NOT flagged.',
    '  asm',
    '    mov eax, lOnlyInAsm',
    '  end;',
    'end;',
    '',
    'end.');

  cFieldNoncompliant: array[0..19] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class',
    '  private',
    '    FUnused: Integer;',
    '  public',
    '    procedure Go;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TThing.Go;',
    'begin',
    'end;',
    '',
    'end.');

  cFieldCompliant: array[0..20] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class',
    '  private',
    '    FUsed: Integer;',
    '  public',
    '    procedure Go;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TThing.Go;',
    'begin',
    '  FUsed := 1;',
    'end;',
    '',
    'end.');

  cFieldCanary: array[0..27] of string = (
    'unit canary;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TA = class',
    '  private',
    '    FShared: Integer;',
    '  end;',
    '',
    '  TB = class',
    '  private',
    '    FShared: Integer;',
    '  public',
    '    procedure Go;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TB.Go;',
    'begin',
    '  // The name FShared collides across TA and TB; a single reference cannot be',
    '  // attributed, so BOTH same-named fields are conservatively kept.',
    '  FShared := 1;',
    'end;',
    '',
    'end.');

  cPropertyNoncompliant: array[0..21] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class',
    '  private',
    '    FValue: Integer;',
    '    property Unused: Integer read FValue write FValue;',
    '  public',
    '    procedure Go;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TThing.Go;',
    'begin',
    '  FValue := 1;',
    'end;',
    '',
    'end.');

  cPropertyCompliant: array[0..21] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class',
    '  private',
    '    FValue: Integer;',
    '    property Used: Integer read FValue write FValue;',
    '  public',
    '    procedure Go;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TThing.Go;',
    'begin',
    '  Used := 7;',
    'end;',
    '',
    'end.');

  cPropertyCanary: array[0..29] of string = (
    'unit canary;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TA = class',
    '  private',
    '    FValue: Integer;',
    '    property Shared: Integer read FValue write FValue;',
    '  end;',
    '',
    '  TB = class',
    '  private',
    '    FValue: Integer;',
    '    property Shared: Integer read FValue write FValue;',
    '  public',
    '    procedure Go;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TB.Go;',
    'begin',
    '  // Shared collides across TA and TB; the reference cannot be attributed, so',
    '  // BOTH same-named properties are conservatively kept.',
    '  Shared := 1;',
    'end;',
    '',
    'end.');

  cConstantNoncompliant: array[0..14] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'const',
    '  cUnused = 42;',
    '',
    'procedure DoWork;',
    'begin',
    'end;',
    '',
    'end.');

  cConstantCompliant: array[0..15] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'const',
    '  cUsed = 42;',
    '',
    'procedure DoWork;',
    'begin',
    '  Writeln(cUsed);',
    'end;',
    '',
    'end.');

  cConstantCanary: array[0..21] of string = (
    'unit canary;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'const',
    '  cShared = 42;',
    '',
    'procedure DoWork;',
    'var',
    '  cShared: Integer;',
    'begin',
    '  // The local cShared shadows the implementation-section const of the same',
    '  // name; the reference cannot be attributed, so the const is conservatively',
    '  // kept.',
    '  cShared := 1;',
    '  Writeln(cShared);',
    'end;',
    '',
    'end.');

  cRoutineNoncompliant: array[0..23] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class',
    '  private',
    '    procedure Helper;',
    '  public',
    '    procedure Go;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TThing.Helper;',
    'begin',
    'end;',
    '',
    'procedure TThing.Go;',
    'begin',
    'end;',
    '',
    'end.');

  cRoutineCompliant: array[0..24] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class',
    '  private',
    '    procedure Helper;',
    '  public',
    '    procedure Go;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TThing.Helper;',
    'begin',
    'end;',
    '',
    'procedure TThing.Go;',
    'begin',
    '  Helper;',
    'end;',
    '',
    'end.');

  cRoutineCanary: array[0..35] of string = (
    'unit canary;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TA = class',
    '  private',
    '    procedure Shared;',
    '  end;',
    '',
    '  TB = class',
    '  private',
    '    procedure Shared;',
    '  public',
    '    procedure Go;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TA.Shared;',
    'begin',
    'end;',
    '',
    'procedure TB.Shared;',
    'begin',
    'end;',
    '',
    'procedure TB.Go;',
    'begin',
    '  // Shared collides across TA and TB; the call cannot be attributed, so BOTH',
    '  // same-named private methods are conservatively kept.',
    '  Shared;',
    'end;',
    '',
    'end.');

  cRoutineResCollision: array[0..50] of string = (
    'unit rescollision;',
    '',
    '{ the resolution-precision INCREMENT fixture. TThing.Foo is a private',
    '  method that is NEVER referenced, but TOther.Foo (a same-named public method) IS',
    '  referenced. The NAME engine sees the name "Foo" used and abstains (a collision);',
    '  RESOLUTION attributes the o.Foo reference to TOther.Foo by identity, so the',
    '  private TThing.Foo is provably unused. Fully resolvable under the synthetic',
    '  engine (host-RTL-independent). }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class',
    '  private',
    '    procedure Foo;',
    '    procedure Used;',
    '  end;',
    '',
    '  TOther = class',
    '  public',
    '    procedure Foo;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TThing.Foo;',
    'begin',
    'end;',
    '',
    'procedure TThing.Used;',
    'begin',
    'end;',
    '',
    'procedure TOther.Foo;',
    'begin',
    'end;',
    '',
    'procedure Drive;',
    'var',
    '  o: TOther;',
    '  t: TThing;',
    'begin',
    '  o := TOther.Create;',
    '  o.Foo;',
    '  t := TThing.Create;',
    '  t.Used;',
    'end;',
    '',
    'end.');

  cRoutineResPlain: array[0..36] of string = (
    'unit resplain;',
    '',
    '{ the MONOTONIC fixture. TThing.Dead is a plainly-unused private',
    '  method with NO name collision, so the name engine already flags it. Resolution',
    '  must ALSO flag it (never suppress a name-engine finding): name-unused ⊆',
    '  resolution-unused. Fully resolvable under the synthetic engine. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class',
    '  private',
    '    procedure Dead;',
    '    procedure Used;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TThing.Dead;',
    'begin',
    'end;',
    '',
    'procedure TThing.Used;',
    'begin',
    'end;',
    '',
    'procedure Drive;',
    'var',
    '  t: TThing;',
    'begin',
    '  t := TThing.Create;',
    '  t.Used;',
    'end;',
    '',
    'end.');

  cRoutineResDegrade: array[0..43] of string = (
    'unit resdegrade;',
    '',
    '{ The PER-UNIT DEGRADE fixture. Structurally the collision',
    '  case (private TThing.Foo unused, TOther.Foo referenced), but the body references',
    '  an UNDECLARED identifier (Bar987) so the resolver FAILS (Succeeded=False) while',
    '  the bare AST still parses. The factory must then select the name engine, which',
    '  abstains on the "Foo" collision — so NO resolution-only finding may appear even',
    '  under useTier.resolution:prefer. (Proves the resolver-not-succeeded degrade.) }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class',
    '  private',
    '    procedure Foo;',
    '  end;',
    '',
    '  TOther = class',
    '  public',
    '    procedure Foo;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TThing.Foo;',
    'begin',
    'end;',
    '',
    'procedure TOther.Foo;',
    'begin',
    'end;',
    '',
    'procedure Drive;',
    'var',
    '  o: TOther;',
    'begin',
    '  o := TOther.Create;',
    '  o.Foo;',
    '  Bar987;',
    'end;',
    '',
    'end.');

  cRoutinePrDecls: array[0..23] of string = (
    'unit pr_decls;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure UsedProc;',
    'procedure UnusedProc;',
    'procedure CollisionProc;',
    '',
    'implementation',
    '',
    'procedure UsedProc;',
    'begin',
    'end;',
    '',
    'procedure UnusedProc;',
    'begin',
    'end;',
    '',
    'procedure CollisionProc;',
    'begin',
    'end;',
    '',
    'end.');

  cRoutinePrUser: array[0..15] of string = (
    'unit pr_user;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'uses',
    '  pr_decls;',
    '',
    'procedure CallIt;',
    'begin',
    '  UsedProc;',
    'end;',
    '',
    'end.');

  cRoutinePrCollider: array[0..16] of string = (
    'unit pr_collider;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure CollisionProc;',
    'begin',
    'end;',
    '',
    'procedure CallLocal;',
    'begin',
    '  CollisionProc;',
    'end;',
    '',
    'end.');

  cTypeNoncompliant: array[0..16] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'type',
    '  TUnusedRec = record',
    '    X: Integer;',
    '  end;',
    '',
    'procedure DoWork;',
    'begin',
    'end;',
    '',
    'end.');

  cTypeCompliant: array[0..19] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'type',
    '  TUsedRec = record',
    '    X: Integer;',
    '  end;',
    '',
    'procedure DoWork;',
    'var',
    '  lR: TUsedRec;',
    'begin',
    '  lR.X := 1;',
    'end;',
    '',
    'end.');

  cTypeCanary: array[0..32] of string = (
    'unit canary;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  // A PUBLIC (interface-section) wrapper — not a project-scope candidate —',
    '  // carrying a nested private type whose name collides below.',
    '  TWrap = class',
    '  private',
    '    type',
    '      TShared = record',
    '        Y: Integer;',
    '      end;',
    '  end;',
    '',
    'implementation',
    '',
    'type',
    '  TShared = record',
    '    X: Integer;',
    '  end;',
    '',
    'var',
    '  gV: TShared;',
    '',
    'initialization',
    '  // `gV: TShared` references the implementation-section TShared by name; that',
    '  // name collides with TWrap''s nested private TShared, so the unattributable',
    '  // name keeps BOTH same-named types conservatively.',
    '  gV.X := 1;',
    '',
    'end.');

  cTypePtDecls: array[0..18] of string = (
    'unit pt_decls;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TUsedType = record',
    '    Value: Integer;',
    '  end;',
    '  TUnusedType = record',
    '    Value: Integer;',
    '  end;',
    '  TCollisionType = record',
    '    Value: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cTypePtUser: array[0..17] of string = (
    'unit pt_user;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'uses',
    '  pt_decls;',
    '',
    'procedure UseTypes;',
    'var',
    '  v: TUsedType;',
    'begin',
    '  v.Value := 1;',
    'end;',
    '',
    'end.');

  cTypePtCollider: array[0..19] of string = (
    'unit pt_collider;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'type',
    '  TCollisionType = record',
    '    Other: Integer;',
    '  end;',
    '',
    'procedure UseLocal;',
    'var',
    '  c: TCollisionType;',
    'begin',
    '  c.Other := 2;',
    'end;',
    '',
    'end.');

  cGlobalGvDecls: array[0..17] of string = (
    'unit gv_decls;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'var',
    '  GUsedGlobal: Integer;',
    '  GUnusedGlobal: Integer;',
    '  GCollisionGlobal: Integer;',
    '  // Externally-linked + referenced nowhere in the project: must NOT be flagged',
    '  // (the linker / C side may reference it). The exact-count assertion in',
    '  // UnusedGlobalVariableProjectScope stays at one finding only while the',
    '  // external-linkage guard holds; deleting the guard turns it red (2 findings).',
    '  GExternalGlobal: Integer; cvar;',
    '',
    'implementation',
    '',
    'end.');

  cGlobalGvUser: array[0..15] of string = (
    'unit gv_user;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'uses',
    '  gv_decls;',
    '',
    'procedure UseGlobals;',
    'begin',
    '  GUsedGlobal := 1;',
    'end;',
    '',
    'end.');

  cGlobalGvCollider: array[0..14] of string = (
    'unit gv_collider;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure Collide;',
    'var',
    '  GCollisionGlobal: Integer;',
    'begin',
    '  GCollisionGlobal := 5;',
    'end;',
    '',
    'end.');

  // Self-contained fixtures (no uses clause); line i+1 == [i].

  cParamNoncompliant: array[0..18] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure DoWork(aUsed, aDead: Integer);',
    '',
    'implementation',
    '',
    'procedure DoWork(aUsed, aDead: Integer);',
    'var',
    '  lTotal: Integer;',
    'begin',
    '  lTotal := aUsed;',
    '  if lTotal > 0 then',
    '    lTotal := 0;',
    'end;',
    '',
    'end.');

  cParamCompliant: array[0..18] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure DoWork(aUsed, aDead: Integer);',
    '',
    'implementation',
    '',
    'procedure DoWork(aUsed, aDead: Integer);',
    'var',
    '  lTotal: Integer;',
    'begin',
    '  lTotal := aUsed + aDead;',
    '  if lTotal > 0 then',
    '    lTotal := 0;',
    'end;',
    '',
    'end.');

  // The override/dispatch chain fixes both signatures (matrix row 14).
  cParamOverride: array[0..32] of string = (
    'unit chain;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    procedure Handle(aDead: Integer); virtual;',
    '    procedure Notify(var aMsg: Integer); message 1;',
    '  end;',
    '',
    '  TChild = class(TBase)',
    '  public',
    '    procedure Handle(aDead: Integer); override;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TBase.Handle(aDead: Integer);',
    'begin',
    '  aDead := 1;',
    'end;',
    '',
    'procedure TBase.Notify(var aMsg: Integer);',
    'begin',
    'end;',
    '',
    'procedure TChild.Handle(aDead: Integer);',
    'begin',
    'end;',
    '',
    'end.');

  // A published method of an {$M+} class is RTTI-reachable (matrix row 15).
  cParamPublished: array[0..18] of string = (
    'unit rttiobj;',
    '{$mode objfpc}{$H+}',
    '{$M+}',
    '',
    'interface',
    '',
    'type',
    '  TWorker = class(TObject)',
    '  published',
    '    procedure Handle(aDead: Integer);',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TWorker.Handle(aDead: Integer);',
    'begin',
    'end;',
    '',
    'end.');

  // The routine's address is taken into a procedural variable (matrix row 18).
  cParamProcVar: array[0..26] of string = (
    'unit procvar;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWorkProc = procedure(aA, aB: Integer);',
    '',
    'procedure DoWork(aUsed, aDead: Integer);',
    '',
    'implementation',
    '',
    'var',
    '  GProc: TWorkProc;',
    '',
    'procedure DoWork(aUsed, aDead: Integer);',
    'var',
    '  lTotal: Integer;',
    'begin',
    '  lTotal := aUsed;',
    '  if lTotal > 0 then',
    '    lTotal := 0;',
    'end;',
    '',
    'initialization',
    '  GProc := @DoWork;',
    'end.');

  cAssignedNoncompliant: array[0..19] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Drive;',
    '',
    'implementation',
    '',
    'procedure Helper(aSpare: Integer);',
    'begin',
    '  aSpare := 7;',
    'end;',
    '',
    'procedure Drive;',
    'begin',
    '  Helper(1);',
    'end;',
    '',
    'end.');

  cAssignedCompliant: array[0..24] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Drive;',
    '',
    'implementation',
    '',
    'procedure Helper(aSpare: Integer);',
    'var',
    '  lKeep: Integer;',
    'begin',
    '  aSpare := 7;',
    '  lKeep := aSpare;',
    '  if lKeep > 0 then',
    '    lKeep := 0;',
    'end;',
    '',
    'procedure Drive;',
    'begin',
    '  Helper(1);',
    'end;',
    '',
    'end.');

  cExceptVarNoncompliant: array[0..28] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TFailure = class(TObject)',
    '  end;',
    '',
    'procedure Drive;',
    '',
    'implementation',
    '',
    'procedure Drive;',
    'var',
    '  lFlag: Integer;',
    'begin',
    '  lFlag := 0;',
    '  try',
    '    lFlag := 1;',
    '  except',
    '    on E: TFailure do',
    '      lFlag := 2;',
    '  end;',
    '  if lFlag > 0 then',
    '    lFlag := 0;',
    'end;',
    '',
    'end.');

  cExceptVarCompliant: array[0..29] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TFailure = class(TObject)',
    '  end;',
    '',
    'procedure Drive;',
    '',
    'implementation',
    '',
    'procedure Drive;',
    'var',
    '  lFlag: Integer;',
    'begin',
    '  lFlag := 0;',
    '  try',
    '    lFlag := 1;',
    '  except',
    '    on E: TFailure do',
    '      if E <> nil then',
    '        lFlag := 2;',
    '  end;',
    '  if lFlag > 0 then',
    '    lFlag := 0;',
    'end;',
    '',
    'end.');

  // A bare re-raise is a use of the handler variable (matrix row 16).
  cExceptVarReRaise: array[0..31] of string = (
    'unit reraise;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TFailure = class(TObject)',
    '  end;',
    '',
    'procedure Drive;',
    '',
    'implementation',
    '',
    'procedure Drive;',
    'var',
    '  lFlag: Integer;',
    'begin',
    '  lFlag := 0;',
    '  try',
    '    lFlag := 1;',
    '  except',
    '    on E: TFailure do',
    '      begin',
    '        lFlag := 2;',
    '        raise;',
    '      end;',
    '  end;',
    '  if lFlag > 0 then',
    '    lFlag := 0;',
    'end;',
    '',
    'end.');

  cLabelNoncompliant: array[0..21] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    '{$goto on}',
    '',
    'interface',
    '',
    'procedure Drive;',
    '',
    'implementation',
    '',
    'procedure Drive;',
    'label',
    '  lSkip;',
    'var',
    '  lFlag: Integer;',
    'begin',
    '  lFlag := 0;',
    '  if lFlag > 0 then',
    '    lFlag := 1;',
    'end;',
    '',
    'end.');

  // The goto sits two constructs deep, so a shallow walk would miss it.
  cLabelCompliant: array[0..29] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    '{$goto on}',
    '',
    'interface',
    '',
    'procedure Drive;',
    '',
    'implementation',
    '',
    'procedure Drive;',
    'label',
    '  lSkip;',
    'var',
    '  lFlag: Integer;',
    'begin',
    '  lFlag := 0;',
    '  if lFlag = 0 then',
    '  begin',
    '    try',
    '      goto lSkip;',
    '    finally',
    '      lFlag := 2;',
    '    end;',
    '  end;',
    'lSkip:',
    '  lFlag := 3;',
    'end;',
    '',
    'end.');

  cGenericNoncompliant: array[0..13] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  generic TBox<T, U> = class',
    '  private',
    '    FValue: T;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cGenericCompliant: array[0..14] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  generic TBox<T, U> = class',
    '  private',
    '    FValue: T;',
    '    FOther: U;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  // U is named only inside a specialize of a nested generic (matrix row 17).
  cGenericSpecialize: array[0..19] of string = (
    'unit spec;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  generic TInner<X> = class',
    '  private',
    '    FItem: X;',
    '  end;',
    '',
    '  generic TBox<T, U> = class',
    '  private',
    '    FValue: T;',
    '    FPair: specialize TInner<U>;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cIfaceUnitDep: array[0..11] of string = (
    'unit uui_dep;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TDepThing = class(TObject)',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cIfaceUnitUser: array[0..19] of string = (
    'unit uui_user;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  uui_dep;',
    '',
    'implementation',
    '',
    'procedure Consume;',
    'var',
    '  lThing: TDepThing;',
    'begin',
    '  lThing := nil;',
    '  if lThing = nil then',
    '    lThing := nil;',
    'end;',
    '',
    'end.');

  cIfaceUnitUserCompliant: array[0..16] of string = (
    'unit uui_user;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  uui_dep;',
    '',
    'procedure Take(aThing: TDepThing);',
    '',
    'implementation',
    '',
    'procedure Take(aThing: TDepThing);',
    'begin',
    'end;',
    '',
    'end.');

  cOneMethodNoncompliant: array[0..25] of string = (
    'unit pmo_nc;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWorker = class(TObject)',
    '  private',
    '    FCounter: Integer;',
    '  public',
    '    procedure Bump;',
    '    procedure Reset;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TWorker.Bump;',
    'begin',
    '  FCounter := FCounter + 1;',
    'end;',
    '',
    'procedure TWorker.Reset;',
    'begin',
    'end;',
    '',
    'end.');

  cOneMethodCompliant: array[0..26] of string = (
    'unit pmo_c;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWorker = class(TObject)',
    '  private',
    '    FCounter: Integer;',
    '  public',
    '    procedure Bump;',
    '    procedure Reset;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TWorker.Bump;',
    'begin',
    '  FCounter := FCounter + 1;',
    'end;',
    '',
    'procedure TWorker.Reset;',
    'begin',
    '  FCounter := 0;',
    'end;',
    '',
    'end.');

  // The member is a property accessor (matrix row 19).
  cOneMethodAccessor: array[0..21] of string = (
    'unit pmo_ac;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWorker = class(TObject)',
    '  private',
    '    FCounter: Integer;',
    '  public',
    '    procedure Bump;',
    '    property Counter: Integer read FCounter write FCounter;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TWorker.Bump;',
    'begin',
    '  FCounter := 0;',
    'end;',
    '',
    'end.');

  // The member is RTTI-reachable rather than private (matrix row 15).
  cOneMethodPublished: array[0..20] of string = (
    'unit pmo_pb;',
    '{$mode objfpc}{$H+}',
    '{$M+}',
    '',
    'interface',
    '',
    'type',
    '  TWorker = class(TObject)',
    '  published',
    '    FLink: TObject;',
    '    procedure Bump;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TWorker.Bump;',
    'begin',
    '  FLink := nil;',
    'end;',
    '',
    'end.');

  cWriteOnlyNoncompliant: array[0..17] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Drive;',
    '',
    'implementation',
    '',
    'procedure Drive;',
    'var',
    '  lDead: Integer;',
    'begin',
    '  lDead := 1;',
    '  lDead := 2;',
    'end;',
    '',
    'end.');

  cWriteOnlyCompliant: array[0..18] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Drive;',
    '',
    'implementation',
    '',
    'procedure Drive;',
    'var',
    '  lDead: Integer;',
    'begin',
    '  lDead := 1;',
    '  if lDead > 0 then',
    '    lDead := 2;',
    'end;',
    '',
    'end.');

  // A var-parameter operand and an address-of both count as reads (row 20).
  cWriteOnlyIndirect: array[0..31] of string = (
    'unit indirect;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  PInt = ^Integer;',
    '',
    'procedure Drive;',
    '',
    'implementation',
    '',
    'procedure Take(var aSlot: Integer);',
    'begin',
    '  aSlot := 1;',
    'end;',
    '',
    'procedure Drive;',
    'var',
    '  lByRef: Integer;',
    '  lAddr: Integer;',
    '  lPtr: PInt;',
    'begin',
    '  lByRef := 0;',
    '  Take(lByRef);',
    '  lAddr := 0;',
    '  lPtr := @lAddr;',
    '  if lPtr = nil then',
    '    lPtr := nil;',
    'end;',
    '',
    'end.');

  { A written uses entry makes the closure interface-only, so all four
    analyzer-backed rules are silent on it (matrix row 11). }
  cIncompleteClosure: array[0..36] of string = (
    'unit incomplete;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  TFailure = class(TObject)',
    '  end;',
    '',
    'procedure DoWork(aUsed, aDead: Integer);',
    '',
    'implementation',
    '',
    'procedure Helper(aSpare: Integer);',
    'begin',
    '  aSpare := 7;',
    'end;',
    '',
    'procedure DoWork(aUsed, aDead: Integer);',
    'var',
    '  lDead: Integer;',
    'begin',
    '  Helper(aUsed);',
    '  lDead := 1;',
    '  lDead := 2;',
    '  try',
    '    lDead := 3;',
    '  except',
    '    on E: TFailure do',
    '      lDead := 4;',
    '  end;',
    'end;',
    '',
    'end.');

procedure TRulesUnusedTest.RunRule(aRule: TRuleBase; const aFixture: string;
  const aCollector: TFpSonarIssueCollector);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;

begin
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    lEngine.Analyze(aFixture, cMode, cDefines, aCollector);
  finally
    lEngine.Free;
    lReg.Free;
  end;
end;


function TRulesUnusedTest.CountById(const aCollector: TFpSonarIssueCollector;
  const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesUnusedTest.FirstById(const aCollector: TFpSonarIssueCollector;
  const aId: string): Integer;

var
  i: Integer;

begin
  Result := -1;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      begin
        Result := i;
        Exit;
      end;
end;


function TRulesUnusedTest.NewRule(const aId: string): TRuleBase;

var
  lMeta: TRuleMetadata;

  // Rule metadata: ship-disabled, per-rule severity and confidence.
  function NewMeta(aSeverity: TFpSonarSeverity;
    aConfidence: TFpSonarConfidence): TRuleMetadata;
  begin
    Result := TRuleMetadata.Make(aId, rtUse, rfAst, aSeverity, itCodeSmell,
      aConfidence, False, '');
  end;

begin
  // Metadata mirrors the unit's self-registration (rtUse / rfAst / Minor /
  // CodeSmell / cfHigh); empty key defaults to rule.<RuleId>.message.
  lMeta := TRuleMetadata.Make(aId, rtUse, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, '');
  if aId = cLocalId then
    Result := TRuleRemoveUnusedLocalVariable.Create(lMeta)
  else if aId = cFieldId then
    Result := TRuleRemoveUnusedField.Create(lMeta)
  else if aId = cPropertyId then
    Result := TRuleRemoveUnusedProperty.Create(lMeta)
  else if aId = cConstantId then
    Result := TRuleRemoveUnusedConstant.Create(lMeta)
  else if aId = cRoutineId then
    Result := TRuleRemoveUnusedRoutine.Create(lMeta)
  else if aId = cTypeId then
    Result := TRuleRemoveUnusedType.Create(lMeta)
  else if aId = cGlobalId then
    Result := TRuleRemoveUnusedGlobalVariable.Create(lMeta)
  else if aId = cParamId then
    Result := TRuleRemoveUnusedParameter.Create(NewMeta(sevMinor, cfMedium))
  else if aId = cParamAssignedId then
    Result := TRuleParameterAssignedButNeverUsed.Create(
      NewMeta(sevMinor, cfMedium))
  else if aId = cExceptVarId then
    Result := TRuleUnusedExceptionVariable.Create(NewMeta(sevMinor, cfHigh))
  else if aId = cLabelId then
    Result := TRuleUnusedLabel.Create(NewMeta(sevMinor, cfMedium))
  else if aId = cGenericParamId then
    Result := TRuleUnusedGenericParameter.Create(NewMeta(sevMinor, cfMedium))
  else if aId = cIfaceUnitId then
    Result := TRuleUnusedUnitInInterface.Create(NewMeta(sevMinor, cfMedium))
  else if aId = cOneMethodId then
    Result := TRulePrivateMemberOnlyUsedByOneMethod.Create(
      NewMeta(sevInfo, cfLow))
  else if aId = cWriteOnlyId then
    Result := TRuleWriteOnlyVariable.Create(NewMeta(sevMinor, cfMedium))
  else
    begin
      // RemoveUnusedImports declares the two opt-in params (so a config setting
      // them is accepted); the rule reads them from aContext.Config at Apply time.
      lMeta.AddParam('flagOperatorOnlyImports', rpkBool);
      lMeta.AddParam('flagSideEffectImports', rpkBool);
      Result := TRuleRemoveUnusedImports.Create(lMeta);
    end;
end;


procedure TRulesUnusedTest.CheckUnusedRuleSrc(aRuleClass: TRuleBaseClassFactory;
  const aId: string; aDeclLine: Integer; const aName: string;
  const aNoncompliant, aCompliant, aCanary: array of string);

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    // Noncompliant: one issue at the declaration line, column 1, carrying
    // [offending name] as the single arg.
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aRuleClass(aId), lFix.Add('noncompliant.pas', aNoncompliant), lc);
      AssertEquals('one issue for ' + aId, 1, CountById(lc, aId));
      k := FirstById(lc, aId);
      AssertEquals('start line', aDeclLine, lc.Issues[k].StartLine);
      AssertEquals('start col', 1, lc.Issues[k].StartCol);
      AssertEquals('end line', aDeclLine, lc.Issues[k].EndLine);
      AssertEquals('end col', 1, lc.Issues[k].EndCol);
      AssertEquals('key is the dotted rule key', 'rule.' + aId + '.message',
        lc.Issues[k].MessageKey);
      AssertEquals('one message arg', 1, Length(lc.Issues[k].MessageArgs));
      AssertEquals('arg 0 is the offending name', aName,
        lc.Issues[k].MessageArgs[0]);
    finally
      lc.Free;
    end;

    // Compliant: a referenced declaration => nothing flagged.
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aRuleClass(aId), lFix.Add('compliant.pas', aCompliant), lc);
      AssertEquals('compliant => zero for ' + aId, 0, CountById(lc, aId));
    finally
      lc.Free;
    end;

    // Canary: a collision / 'with' member access => conservatively NOT flagged
    // (the safe direction). Deleting the guard would turn this red.
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aRuleClass(aId), lFix.Add('canary.pas', aCanary), lc);
      AssertEquals('canary (collision) => zero for ' + aId, 0,
        CountById(lc, aId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesUnusedTest.UnusedLocalVariablePositions;

begin
  // Noncompliant: local 'lUnused' (decl line 10) is never referenced in DoWork.
  CheckUnusedRuleSrc(@NewRule, cLocalId, 10, 'lUnused',
    cLocalNoncompliant, cLocalCompliant, cLocalCanary);
end;


procedure TRulesUnusedTest.UnusedLocalVariableAsmCanary;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;

begin
  // A local referenced ONLY inside an 'asm ... end' block (a raw token stream,
  // not expression nodes) must be conservatively kept => zero findings. This is
  // the safe-direction guard against deleting code live in inline asm.
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewRule(cLocalId), lFix.Add('asmcanary.pas', cLocalAsmCanary), lc);
      AssertEquals('asm-only local must NOT be flagged', 0,
        CountById(lc, cLocalId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesUnusedTest.UnusedFieldPositions;

begin
  // Noncompliant: private field 'FUnused' (decl line 9) is never referenced.
  CheckUnusedRuleSrc(@NewRule, cFieldId, 9, 'FUnused',
    cFieldNoncompliant, cFieldCompliant, cFieldCanary);
end;


procedure TRulesUnusedTest.UnusedPropertyPositions;

begin
  // Noncompliant: private property 'Unused' (decl line 10) is never referenced.
  CheckUnusedRuleSrc(@NewRule, cPropertyId, 10, 'Unused',
    cPropertyNoncompliant, cPropertyCompliant, cPropertyCanary);
end;


procedure TRulesUnusedTest.UnusedConstantPositions;

begin
  // Noncompliant: implementation-section const 'cUnused' (decl line 9) unused.
  CheckUnusedRuleSrc(@NewRule, cConstantId, 9, 'cUnused',
    cConstantNoncompliant, cConstantCompliant, cConstantCanary);
end;


procedure TRulesUnusedTest.UnusedRoutinePositions;

begin
  // Noncompliant: private method 'Helper' (declaration line 9) is never called.
  CheckUnusedRuleSrc(@NewRule, cRoutineId, 9, 'Helper',
    cRoutineNoncompliant, cRoutineCompliant, cRoutineCanary);
end;


procedure TRulesUnusedTest.UnusedTypePositions;

begin
  // Noncompliant: implementation-section type 'TUnusedRec' (decl line 9) unused.
  CheckUnusedRuleSrc(@NewRule, cTypeId, 9, 'TUnusedRec',
    cTypeNoncompliant, cTypeCompliant, cTypeCanary);
end;


function TRulesUnusedTest.BuildIndex(
  const aFiles: array of string): TFpSonarProjectIndex;

begin
  Result := BuildProjectIndex(aFiles, cMode, cDefines, [], []);
end;


procedure TRulesUnusedTest.RunRuleWithIndex(aRule: TRuleBase;
  const aSubject: string; aIndex: TFpSonarProjectIndex;
  const aConfig: TFpSonarConfig; const aCollector: TFpSonarIssueCollector);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;

begin
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    lEngine.ProjectIndex := aIndex;
    lEngine.Config := aConfig;
    lEngine.Analyze(aSubject, cMode, cDefines, aCollector);
  finally
    lEngine.Free;
    lReg.Free;
  end;
end;


function TRulesUnusedTest.HasArg(const aCollector: TFpSonarIssueCollector;
  const aId, aArg: string): Boolean;

var
  i: Integer;

begin
  Result := False;
  for i := 0 to aCollector.Count - 1 do
    if (aCollector.Issues[i].RuleId = aId)
      and (Length(aCollector.Issues[i].MessageArgs) > 0)
      and (aCollector.Issues[i].MessageArgs[0] = aArg) then
      Exit(True);
end;


procedure TRulesUnusedTest.CheckProjectRule(const aId: string;
  const aFiles: array of string; const aSubject: string; aDeclLine: Integer;
  const aName: string);

var
  lIndex: TFpSonarProjectIndex;
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lIndex := BuildIndex(aFiles);
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRuleWithIndex(NewRule(aId), aSubject, lIndex, TFpSonarConfig.Default, lc);
      // Exactly one finding: the noncompliant candidate. The compliant (used)
      // and the collision-canary candidates in the same project stay silent.
      AssertEquals('one project-scope finding for ' + aId, 1, CountById(lc, aId));
      k := FirstById(lc, aId);
      AssertEquals('start line', aDeclLine, lc.Issues[k].StartLine);
      AssertEquals('start col', 1, lc.Issues[k].StartCol);
      AssertEquals('arg 0 is the offending name', aName,
        lc.Issues[k].MessageArgs[0]);
    finally
      lc.Free;
    end;
  finally
    lIndex.Free;
  end;
end;


function TRulesUnusedTest.WriteImportsProject(aTmp: TTempFixtures): TStringArray;

begin
  // imp_user first (the subject the rule runs over); the rest are its used units,
  // resolved as siblings in aTmp.Dir.
  Result := [
    aTmp.Add('imp_user.pas', cImpUser),
    aTmp.Add('imp_used.pas', cImpUsed),
    aTmp.Add('imp_unused.pas', cImpUnused),
    aTmp.Add('imp_collision.pas', cImpCollision),
    aTmp.Add('imp_operator.pas', cImpOperator),
    aTmp.Add('imp_sideeffect.pas', cImpSideEffect)];
end;


procedure TRulesUnusedTest.UnusedGlobalVariableProjectScope;

var
  lTmp: TTempFixtures;
  lDecls: string;

begin
  // gv_decls declares three interface globals; gv_user references GUsedGlobal and
  // gv_collider references a same-named LOCAL GCollisionGlobal (collision canary).
  // Only GUnusedGlobal (decl line 8) is referenced nowhere in the project.
  lTmp := TTempFixtures.Create;
  try
    lDecls := lTmp.Add('gv_decls.pas', cGlobalGvDecls);
    CheckProjectRule(cGlobalId,
      [lDecls, lTmp.Add('gv_user.pas', cGlobalGvUser),
       lTmp.Add('gv_collider.pas', cGlobalGvCollider)],
      lDecls, 8, 'GUnusedGlobal');
  finally
    lTmp.Free;
  end;
end;


procedure TRulesUnusedTest.UnusedTypeProjectScope;

var
  lTmp: TTempFixtures;
  lDecls: string;

begin
  // pt_decls declares three public types; pt_user uses TUsedType and pt_collider
  // uses a same-named LOCAL TCollisionType (collision canary). Only TUnusedType
  // (decl line 10) is referenced nowhere in the project.
  lTmp := TTempFixtures.Create;
  try
    lDecls := lTmp.Add('pt_decls.pas', cTypePtDecls);
    CheckProjectRule(cTypeId,
      [lDecls, lTmp.Add('pt_user.pas', cTypePtUser),
       lTmp.Add('pt_collider.pas', cTypePtCollider)],
      lDecls, 10, 'TUnusedType');
  finally
    lTmp.Free;
  end;
end;


procedure TRulesUnusedTest.UnusedRoutineProjectScope;

var
  lTmp: TTempFixtures;
  lDecls: string;

begin
  // pr_decls declares three public routines; pr_user calls UsedProc and
  // pr_collider calls a same-named LOCAL CollisionProc (collision canary). Only
  // UnusedProc (interface decl line 7) is referenced nowhere in the project.
  lTmp := TTempFixtures.Create;
  try
    lDecls := lTmp.Add('pr_decls.pas', cRoutinePrDecls);
    CheckProjectRule(cRoutineId,
      [lDecls, lTmp.Add('pr_user.pas', cRoutinePrUser),
       lTmp.Add('pr_collider.pas', cRoutinePrCollider)],
      lDecls, 7, 'UnusedProc');
  finally
    lTmp.Free;
  end;
end;


procedure TRulesUnusedTest.UnusedImportsProjectScope;

var
  lTmp: TTempFixtures;
  lProj: TStringArray;

begin
  // imp_user imports six units: imp_used (referenced), imp_unused (referenced
  // nowhere, the only finding), imp_collision (its export name referenced as a
  // local), imp_operator (operator-only) and imp_sideeffect (init/final). The
  // unused import sits on its own uses-clause line (8).
  lTmp := TTempFixtures.Create;
  try
    lProj := WriteImportsProject(lTmp);
    CheckProjectRule(cImportId, lProj, lProj[0], 8, 'imp_unused');
  finally
    lTmp.Free;
  end;
end;


procedure TRulesUnusedTest.UnusedImportsOptInFlags;

var
  lIndex: TFpSonarProjectIndex;
  lc: TFpSonarIssueCollector;
  lConfig: TFpSonarConfig;
  lSetting: TFpSonarRuleSetting;
  lTmp: TTempFixtures;
  lProj: TStringArray;

begin
  // With both opt-ins on, the operator-only and side-effect imports are no
  // longer skipped.
  lConfig := TFpSonarConfig.Default;
  lSetting.RuleId := cImportId;
  lSetting.HasEnabled := False;
  lSetting.Enabled := False;
  lSetting.HasSeverity := False;
  SetLength(lSetting.Params, 2);
  lSetting.Params[0].Key := 'flagOperatorOnlyImports';
  lSetting.Params[0].Kind := cpkBool;
  lSetting.Params[0].BoolVal := True;
  lSetting.Params[1].Key := 'flagSideEffectImports';
  lSetting.Params[1].Kind := cpkBool;
  lSetting.Params[1].BoolVal := True;
  SetLength(lConfig.Rules, Length(lConfig.Rules) + 1);
  lConfig.Rules[High(lConfig.Rules)] := lSetting;

  lTmp := TTempFixtures.Create;
  try
    lProj := WriteImportsProject(lTmp);
    lIndex := BuildIndex(lProj);
    try
      lc := TFpSonarIssueCollector.Create;
      try
        RunRuleWithIndex(NewRule(cImportId), lProj[0], lIndex, lConfig, lc);
        AssertEquals('three findings with both opt-ins', 3,
          CountById(lc, cImportId));
        AssertTrue('imp_unused still flagged', HasArg(lc, cImportId, 'imp_unused'));
        AssertTrue('imp_operator flagged with opt-in',
          HasArg(lc, cImportId, 'imp_operator'));
        AssertTrue('imp_sideeffect flagged with opt-in',
          HasArg(lc, cImportId, 'imp_sideeffect'));
      finally
        lc.Free;
      end;
    finally
      lIndex.Free;
    end;
  finally
    lTmp.Free;
  end;
end;


procedure TRulesUnusedTest.RulesSelfRegisterGlobally;

begin
  // The production initialization registered all six USE rules into the global
  // registry.
  AssertTrue('RemoveUnusedLocalVariable registered',
    RuleRegistry.FindById(cLocalId) <> nil);
  AssertTrue('RemoveUnusedField registered',
    RuleRegistry.FindById(cFieldId) <> nil);
  AssertTrue('RemoveUnusedProperty registered',
    RuleRegistry.FindById(cPropertyId) <> nil);
  AssertTrue('RemoveUnusedConstant registered',
    RuleRegistry.FindById(cConstantId) <> nil);
  AssertTrue('RemoveUnusedRoutine registered',
    RuleRegistry.FindById(cRoutineId) <> nil);
  AssertTrue('RemoveUnusedType registered',
    RuleRegistry.FindById(cTypeId) <> nil);
  AssertTrue('RemoveUnusedImports registered',
    RuleRegistry.FindById(cImportId) <> nil);
  AssertTrue('RemoveUnusedGlobalVariable registered',
    RuleRegistry.FindById(cGlobalId) <> nil);

  // The eight rules, every one of them shipping disabled.
  AssertTrue(cParamId + ' registered', RuleRegistry.FindById(cParamId) <> nil);
  AssertTrue(cParamAssignedId + ' registered',
    RuleRegistry.FindById(cParamAssignedId) <> nil);
  AssertTrue(cExceptVarId + ' registered',
    RuleRegistry.FindById(cExceptVarId) <> nil);
  AssertTrue(cLabelId + ' registered', RuleRegistry.FindById(cLabelId) <> nil);
  AssertTrue(cGenericParamId + ' registered',
    RuleRegistry.FindById(cGenericParamId) <> nil);
  AssertTrue(cIfaceUnitId + ' registered',
    RuleRegistry.FindById(cIfaceUnitId) <> nil);
  AssertTrue(cOneMethodId + ' registered',
    RuleRegistry.FindById(cOneMethodId) <> nil);
  AssertTrue(cWriteOnlyId + ' registered',
    RuleRegistry.FindById(cWriteOnlyId) <> nil);

  AssertFalse(cParamId + ' ships disabled',
    RuleRegistry.FindById(cParamId).Metadata.DefaultEnabled);
  AssertFalse(cParamAssignedId + ' ships disabled',
    RuleRegistry.FindById(cParamAssignedId).Metadata.DefaultEnabled);
  AssertFalse(cExceptVarId + ' ships disabled',
    RuleRegistry.FindById(cExceptVarId).Metadata.DefaultEnabled);
  AssertFalse(cLabelId + ' ships disabled',
    RuleRegistry.FindById(cLabelId).Metadata.DefaultEnabled);
  AssertFalse(cGenericParamId + ' ships disabled',
    RuleRegistry.FindById(cGenericParamId).Metadata.DefaultEnabled);
  AssertFalse(cIfaceUnitId + ' ships disabled',
    RuleRegistry.FindById(cIfaceUnitId).Metadata.DefaultEnabled);
  AssertFalse(cOneMethodId + ' ships disabled',
    RuleRegistry.FindById(cOneMethodId).Metadata.DefaultEnabled);
  AssertFalse(cWriteOnlyId + ' ships disabled',
    RuleRegistry.FindById(cWriteOnlyId).Metadata.DefaultEnabled);
end;


procedure TRulesUnusedTest.RunRuleResolved(aRule: TRuleBase;
  const aFixture: string; aResolution: TFpSonarUseTierResolution;
  const aCollector: TFpSonarIssueCollector);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;
  lConfig: TFpSonarConfig;

begin
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    lConfig := TFpSonarConfig.Default;
    lConfig.UseTierResolution := aResolution;
    lEngine.Config := lConfig;
    // Analyze builds the resolver (synthetic engine) on parse success.
    lEngine.Analyze(aFixture, cMode, cDefines, aCollector);
  finally
    lEngine.Free;
    lReg.Free;
  end;
end;


procedure TRulesUnusedTest.ResolutionPrecisionIncrement;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lFile: string;

begin
  lFix := TTempFixtures.Create;
  try
    lFile := lFix.Add('rescollision.pas', cRoutineResCollision);
    // rescollision: private TThing.Foo (line 17) is unreferenced, but a same-named
    // public TOther.Foo IS referenced. With the default name engine (utrOff) the
    // collision keeps TThing.Foo "used" => NO finding (byte-identical default).
    lc := TFpSonarIssueCollector.Create;
    try
      RunRuleResolved(NewRule(cRoutineId), lFile, utrOff, lc);
      AssertEquals('default (off) abstains on the collision', 0,
        CountById(lc, cRoutineId));
    finally
      lc.Free;
    end;

    // With useTier.resolution = prefer and a resolved unit, resolution attributes
    // the o.Foo reference to TOther.Foo by identity, proving TThing.Foo unused =>
    // exactly one resolution-only finding at the private method's declaration line.
    lc := TFpSonarIssueCollector.Create;
    try
      RunRuleResolved(NewRule(cRoutineId), lFile, utrPrefer, lc);
      // Also TThing.Used, dead because AnalyzeModule never roots its caller Drive.
      AssertEquals('prefer adds the resolution-only finding', 2,
        CountById(lc, cRoutineId));
      AssertEquals('finding at the private method line', 17,
        lc.Issues[FirstById(lc, cRoutineId)].StartLine);
      AssertEquals('finding names the unused method', 'Foo',
        lc.Issues[FirstById(lc, cRoutineId)].MessageArgs[0]);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesUnusedTest.ResolutionMonotonic;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lFile: string;

begin
  lFix := TTempFixtures.Create;
  try
    lFile := lFix.Add('resplain.pas', cRoutineResPlain);
    // resplain: a plainly-unused private method Dead (line 15), NO collision.
    lc := TFpSonarIssueCollector.Create;
    try
      RunRuleResolved(NewRule(cRoutineId), lFile, utrOff, lc);
      AssertEquals('name engine flags the plain-unused method', 1,
        CountById(lc, cRoutineId));
      AssertEquals('at its declaration line', 15,
        lc.Issues[FirstById(lc, cRoutineId)].StartLine);
    finally
      lc.Free;
    end;

    lc := TFpSonarIssueCollector.Create;
    try
      RunRuleResolved(NewRule(cRoutineId), lFile, utrPrefer, lc);
      // Also TThing.Used, dead because AnalyzeModule never roots its caller Drive.
      AssertEquals('prefer never suppresses the name finding', 2,
        CountById(lc, cRoutineId));
      AssertEquals('still at its declaration line', 15,
        lc.Issues[FirstById(lc, cRoutineId)].StartLine);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesUnusedTest.ResolutionDegradesWhenResolverFails;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;

begin
  // resdegrade: the collision shape, but the body references an undeclared
  // identifier.
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRuleResolved(NewRule(cRoutineId),
        lFix.Add('resdegrade.pas', cRoutineResDegrade), utrPrefer, lc);
      AssertEquals('degrades to the name engine => no upgrade', 0,
        CountById(lc, cRoutineId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


function TRulesUnusedTest.EnabledConfig(
  const aRuleId: string): TFpSonarConfig;

begin
  // These rules all ship disabled, so the dispatcher needs the opt-in.
  Result := TFpSonarConfig.Default;
  SetLength(Result.Rules, 0);
  SetLength(Result.Rules, 1);
  Result.Rules[0].RuleId := aRuleId;
  Result.Rules[0].HasEnabled := True;
  Result.Rules[0].Enabled := True;
  Result.UseTierResolution := utrPrefer;
end;


procedure TRulesUnusedTest.RunRule(aRule: TRuleBase; const aFixture: string;
  aWithhold: boolean; const aCollector: TFpSonarIssueCollector);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;

begin
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lEngine.Config := EnabledConfig(aRule.Metadata.RuleId);
    lReg.Register(aRule);
    // aRealRtl puts objpas in every implicit uses chain; it is absent from the
    // synthetic registry.
    if aWithhold then
      lEngine.Analyze(aFixture, cMode, cDefines, [], [], True, SizeOf(Pointer),
        aCollector)
    else
      lEngine.Analyze(aFixture, cMode, cDefines, aCollector);
  finally
    lEngine.Free;
    lReg.Free;
  end;
end;


function TRulesUnusedTest.PreferredCount(const aId: string;
  const aSource: array of string; aWithhold: boolean): Integer;

var
  lFix: TTempFixtures;
  lc: TFpSonarIssueCollector;

begin
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewRule(aId), lFix.Add('probe.pas', aSource), aWithhold, lc);
      Result := CountById(lc, aId);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesUnusedTest.CheckNewRule(const aId: string; aDeclLine: Integer;
  const aName: string; const aNoncompliant, aCompliant: array of string);

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewRule(aId), lFix.Add('noncompliant.pas', aNoncompliant),
        False, lc);
      AssertEquals('one issue for ' + aId, 1, CountById(lc, aId));
      k := FirstById(lc, aId);
      AssertEquals('start line', aDeclLine, lc.Issues[k].StartLine);
      AssertEquals('start col', 1, lc.Issues[k].StartCol);
      AssertEquals('end line', aDeclLine, lc.Issues[k].EndLine);
      AssertEquals('end col', 1, lc.Issues[k].EndCol);
      AssertEquals('key is the dotted rule key', 'rule.' + aId + '.message',
        lc.Issues[k].MessageKey);
      AssertEquals('one message arg', 1, Length(lc.Issues[k].MessageArgs));
      AssertEquals('arg 0 is the offending name', aName,
        lc.Issues[k].MessageArgs[0]);
    finally
      lc.Free;
    end;

    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewRule(aId), lFix.Add('compliant.pas', aCompliant), False, lc);
      AssertEquals('compliant => zero for ' + aId, 0, CountById(lc, aId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


function TRulesUnusedTest.IndexedCount(const aId: string;
  const aFiles: array of string; const aSubject: string): Integer;

var
  lIndex: TFpSonarProjectIndex;
  lc: TFpSonarIssueCollector;

begin
  lIndex := BuildIndex(aFiles);
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRuleWithIndex(NewRule(aId), aSubject, lIndex, EnabledConfig(aId), lc);
      Result := CountById(lc, aId);
    finally
      lc.Free;
    end;
  finally
    lIndex.Free;
  end;
end;


procedure TRulesUnusedTest.CheckIndexedRule(const aId: string;
  const aFiles: array of string; const aSubject: string; aDeclLine: Integer;
  const aName: string);

var
  lIndex: TFpSonarProjectIndex;
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lIndex := BuildIndex(aFiles);
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRuleWithIndex(NewRule(aId), aSubject, lIndex, EnabledConfig(aId), lc);
      AssertEquals('one issue for ' + aId, 1, CountById(lc, aId));
      k := FirstById(lc, aId);
      AssertEquals('start line', aDeclLine, lc.Issues[k].StartLine);
      AssertEquals('start col', 1, lc.Issues[k].StartCol);
      AssertEquals('end line', aDeclLine, lc.Issues[k].EndLine);
      AssertEquals('end col', 1, lc.Issues[k].EndCol);
      AssertEquals('key is the dotted rule key', 'rule.' + aId + '.message',
        lc.Issues[k].MessageKey);
      AssertEquals('one message arg', 1, Length(lc.Issues[k].MessageArgs));
      AssertEquals('arg 0 is the offending name', aName,
        lc.Issues[k].MessageArgs[0]);
    finally
      lc.Free;
    end;
  finally
    lIndex.Free;
  end;
end;


procedure TRulesUnusedTest.RemoveUnusedParameterPositions;

begin
  // Noncompliant: parameter 'aDead' of the interface declaration (line 6) is
  // named nowhere in DoWork's body.
  CheckNewRule(cParamId, 6, 'aDead', cParamNoncompliant, cParamCompliant);
end;


procedure TRulesUnusedTest.ParameterAssignedButNeverUsedPositions;

begin
  // Noncompliant: value parameter 'aSpare' (line 10) is assigned and discarded.
  CheckNewRule(cParamAssignedId, 10, 'aSpare',
    cAssignedNoncompliant, cAssignedCompliant);
end;


procedure TRulesUnusedTest.UnusedExceptionVariablePositions;

begin
  // Noncompliant: handler variable 'E' (line 22) is never named in the handler.
  CheckNewRule(cExceptVarId, 22, 'E',
    cExceptVarNoncompliant, cExceptVarCompliant);
end;


procedure TRulesUnusedTest.UnusedLabelPositions;

begin
  // Noncompliant: 'lSkip' is declared (label block on line 12) and no goto
  // targets it. The compliant fixture nests its goto inside a try inside an if.
  CheckNewRule(cLabelId, 12, 'lSkip', cLabelNoncompliant, cLabelCompliant);
end;


procedure TRulesUnusedTest.UnusedGenericParameterPositions;

begin
  // Noncompliant: template parameter 'U' of TBox (line 7) is never named.
  CheckNewRule(cGenericParamId, 7, 'U',
    cGenericNoncompliant, cGenericCompliant);
end;


procedure TRulesUnusedTest.UnusedUnitInInterfacePositions;

var
  lTmp: TTempFixtures;
  lUser: string;

begin
  // Noncompliant: uui_user imports uui_dep in its interface (line 7) but only
  // its implementation section names TDepThing.
  lTmp := TTempFixtures.Create;
  try
    lTmp.Add('uui_dep.pas', cIfaceUnitDep);
    lUser := lTmp.Add('uui_user.pas', cIfaceUnitUser);
    CheckIndexedRule(cIfaceUnitId,
      [lUser, lTmp.Dir + PathDelim + 'uui_dep.pas'], lUser, 7, 'uui_dep');
  finally
    lTmp.Free;
  end;

  // Compliant: the interface itself names TDepThing.
  lTmp := TTempFixtures.Create;
  try
    lTmp.Add('uui_dep.pas', cIfaceUnitDep);
    lUser := lTmp.Add('uui_user.pas', cIfaceUnitUserCompliant);
    AssertEquals('an interface-side use keeps the import', 0,
      IndexedCount(cIfaceUnitId,
        [lUser, lTmp.Dir + PathDelim + 'uui_dep.pas'], lUser));
  finally
    lTmp.Free;
  end;
end;


procedure TRulesUnusedTest.PrivateMemberOnlyUsedByOneMethodPositions;

var
  lTmp: TTempFixtures;
  lFile: string;

begin
  // Noncompliant: private field 'FCounter' (line 9) is referenced by Bump alone.
  lTmp := TTempFixtures.Create;
  try
    lFile := lTmp.Add('pmo_nc.pas', cOneMethodNoncompliant);
    // The index needs a second unit: a one-unit index answers rrUnknown.
    CheckIndexedRule(cOneMethodId,
      [lFile, lTmp.Add('uui_dep.pas', cIfaceUnitDep)], lFile, 9, 'FCounter');
  finally
    lTmp.Free;
  end;

  // Compliant: both methods reference it.
  lTmp := TTempFixtures.Create;
  try
    lFile := lTmp.Add('pmo_c.pas', cOneMethodCompliant);
    AssertEquals('two referring methods => zero', 0,
      IndexedCount(cOneMethodId,
        [lFile, lTmp.Add('uui_dep.pas', cIfaceUnitDep)], lFile));
  finally
    lTmp.Free;
  end;
end;


procedure TRulesUnusedTest.WriteOnlyVariablePositions;

begin
  // Noncompliant: local 'lDead' (line 12) is assigned twice and never read.
  CheckNewRule(cWriteOnlyId, 12, 'lDead',
    cWriteOnlyNoncompliant, cWriteOnlyCompliant);
end;


procedure TRulesUnusedTest.NewRulesDegradeWithoutResolver;

begin
  // Matrix row 10: with resolution withheld the six rules that read a resolved
  // fact emit nothing and raise nothing.
  AssertEquals(cParamId + ' degrades', 0,
    PreferredCount(cParamId, cParamNoncompliant, True));
  AssertEquals(cParamAssignedId + ' degrades', 0,
    PreferredCount(cParamAssignedId, cAssignedNoncompliant, True));
  AssertEquals(cExceptVarId + ' degrades', 0,
    PreferredCount(cExceptVarId, cExceptVarNoncompliant, True));
  AssertEquals(cGenericParamId + ' degrades', 0,
    PreferredCount(cGenericParamId, cGenericNoncompliant, True));
  AssertEquals(cWriteOnlyId + ' degrades', 0,
    PreferredCount(cWriteOnlyId, cWriteOnlyNoncompliant, True));
  // UnusedLabel reads the unit's own statement tree.
  AssertEquals(cLabelId + ' is resolution-independent', 1,
    PreferredCount(cLabelId, cLabelNoncompliant, True));
  // The two index-backed rules exit on the missing index before resolution is
  // ever consulted.
  AssertEquals(cOneMethodId + ' degrades', 0,
    PreferredCount(cOneMethodId, cOneMethodNoncompliant, True));
  AssertEquals(cIfaceUnitId + ' degrades', 0,
    PreferredCount(cIfaceUnitId, cIfaceUnitUser, True));
end;


procedure TRulesUnusedTest.NewRulesSilentOnUnresolvedOperand;

begin
  // Matrix row 11: a written uses entry makes the closure interface-only, which
  // is exactly the AC's completeness clause for the four analyzer-backed rules.
  AssertEquals(cParamId + ' is silent on an incomplete closure', 0,
    PreferredCount(cParamId, cIncompleteClosure, False));
  AssertEquals(cParamAssignedId + ' is silent on an incomplete closure', 0,
    PreferredCount(cParamAssignedId, cIncompleteClosure, False));
  AssertEquals(cExceptVarId + ' is silent on an incomplete closure', 0,
    PreferredCount(cExceptVarId, cIncompleteClosure, False));
  AssertEquals(cWriteOnlyId + ' is silent on an incomplete closure', 0,
    PreferredCount(cWriteOnlyId, cIncompleteClosure, False));
  AssertEquals(cGenericParamId + ' is silent on an incomplete closure', 0,
    PreferredCount(cGenericParamId, cIncompleteClosure, False));

  // Matrix row 12: no project index, so neither index-backed rule can decide.
  AssertEquals(cOneMethodId + ' is silent without an index', 0,
    PreferredCount(cOneMethodId, cOneMethodNoncompliant, False));
  AssertEquals(cIfaceUnitId + ' is silent without an index', 0,
    PreferredCount(cIfaceUnitId, cIfaceUnitUser, False));
  // Matrix row 13: n/a for UnusedLabel. It is an rtAst rule over the unit's own
  // statement tree and consults no resolved fact.
end;


procedure TRulesUnusedTest.NewRulesIndirectUseChannels;

var
  lTmp: TTempFixtures;
  lFile: string;

begin
  // Row 14: a virtual, override or message routine's signature is fixed by the
  // chain.
  AssertEquals(cParamId + ' skips the override chain', 0,
    PreferredCount(cParamId, cParamOverride, False));
  AssertEquals(cParamAssignedId + ' skips the override chain', 0,
    PreferredCount(cParamAssignedId, cParamOverride, False));
  // Row 15: a published member of an {$M+} class is RTTI-reachable.
  AssertEquals(cParamId + ' skips a published method', 0,
    PreferredCount(cParamId, cParamPublished, False));
  // Row 16: a bare re-raise is a use of the handler variable.
  AssertEquals(cExceptVarId + ' counts a bare raise as a use', 0,
    PreferredCount(cExceptVarId, cExceptVarReRaise, False));
  { Row 17: a specialize argument is a use of the template parameter, and the
    name set never holds one — it is an unparented TPasUnresolvedTypeRef that
    ForEachCall's CheckParent guard drops (DW-422). }
  AssertEquals(cGenericParamId + ' abstains on a declared specialization', 0,
    PreferredCount(cGenericParamId, cGenericSpecialize, False));
  { Row 18: taking the routine's address does not reach its parameters, so the
    unused list still carries aDead. Measured residue, which is what holds the
    rule at cfMedium instead of reporting it away (DW-419). }
  AssertEquals(cParamId + ' cannot see the address-of channel', 1,
    PreferredCount(cParamId, cParamProcVar, False));
  // Row 20: a var-parameter operand and an address-of both count as reads.
  AssertEquals(cWriteOnlyId + ' counts var-param and address-of reads', 0,
    PreferredCount(cWriteOnlyId, cWriteOnlyIndirect, False));

  // Rows 15 and 19 for the private-member rule, which needs a project index.
  lTmp := TTempFixtures.Create;
  try
    lFile := lTmp.Add('pmo_ac.pas', cOneMethodAccessor);
    AssertEquals(cOneMethodId + ' skips a property accessor', 0,
      IndexedCount(cOneMethodId,
        [lFile, lTmp.Add('uui_dep.pas', cIfaceUnitDep)], lFile));
  finally
    lTmp.Free;
  end;
  lTmp := TTempFixtures.Create;
  try
    lFile := lTmp.Add('pmo_pb.pas', cOneMethodPublished);
    AssertEquals(cOneMethodId + ' skips a published member', 0,
      IndexedCount(cOneMethodId,
        [lFile, lTmp.Add('uui_dep.pas', cIfaceUnitDep)], lFile));
  finally
    lTmp.Free;
  end;
end;


initialization
  RegisterTest(TRulesUnusedTest);

end.
