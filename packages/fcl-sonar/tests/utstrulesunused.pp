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

{ The six USE-tier intra-unit unused-declaration rules:
  RemoveUnusedLocalVariable, RemoveUnusedField, RemoveUnusedProperty,
  RemoveUnusedConstant, RemoveUnusedRoutine, RemoveUnusedType. }

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
    // Asserts NewRule fires exactly once at aDeclLine, column 1, with arg
    // [aName]; and zero on BOTH the compliant AND canary fixtures (the safe-
    // direction guard). Fixtures supplied inline, materialised to a temp dir.
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
    // synthetic project into aTmp and returns their paths (imp_user first). The
    // embedded-fixture (Approach A) pilot: cross-unit resolution finds the used
    // units as siblings in aTmp.Dir, so no on-disk tests/rules/ dir is needed.
    function WriteImportsProject(aTmp: TTempFixtures): TStringArray;
    // Runs aRule over aFixture with useTier.resolution = aResolution threaded
    // into the engine config (mirrors RunRule, plus the config set), so the
    // resolution-backed oracle is selected when utrPrefer + the file resolved.
    procedure RunRuleResolved(aRule: TRuleBase; const aFixture: string;
      aResolution: TFpSonarUseTierResolution;
      const aCollector: TFpSonarIssueCollector);
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

  // Embedded RemoveUnusedImports cross-unit fixtures (Approach A pilot): the
  // subject imp_user imports six units; only imp_unused is referenced nowhere
  // (its uses-clause line 8 => the sole finding). Written as siblings so the
  // resolver's base-directory resolution binds them. Line i+1 == [i].
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

  // Embedded RemoveUnused* fixtures (Approach A rollout): line i+1 == [i].

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
    // Noncompliant: exactly one issue at the declaration line, column 1 (AST
    // nodes carry no column), carrying [offending name] as the single arg.
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
  // imp_user imports six units: imp_used (referenced => used), imp_unused
  // (referenced nowhere => the only finding), imp_collision (its export name is
  // referenced as a LOCAL => collision canary, kept), imp_operator (operator-only
  // => default skip) and imp_sideeffect (init/final => default skip). The unused
  // import sits on its own uses-clause line (8).
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
  // longer skipped, so imp_operator + imp_sideeffect join imp_unused (3 findings).
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
  // The production initialization registered all six USE rules into the GLOBAL
  // registry (this is what the CLI process runs).
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
    // Analyze builds the resolver (synthetic engine) on parse success, so a
    // fully-self-contained fixture resolves with no host RTL.
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
      AssertEquals('prefer adds the resolution-only finding', 1,
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
    // resplain: a plainly-unused private method Dead (line 15), NO collision, so the
    // name engine already flags it. The resolution engine must NEVER suppress a
    // name-engine finding (name-unused ⊆ resolution-unused) — it appears under BOTH.
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
      AssertEquals('prefer never suppresses the name finding', 1,
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
  // identifier, so the resolver fails (Succeeded=False) while the bare AST parses.
  // The factory must select the name engine, which abstains on the collision — so
  // NO resolution-only finding may appear even under prefer (per-unit degrade).
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


initialization
  RegisterTest(TRulesUnusedTest);

end.
