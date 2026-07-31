{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the TOK-tier conditional-compilation and portability rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesCondComp;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Config, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Rules.CondComp, FpSonar.Rules.Casts, UtstFixtures;

type
  { CondComp rule position, silence, degradation and registration tests. }
  TRulesCondCompTest = class(TTestCase)
  private
    // A config enabling exactly the ids in aIds.
    function EnabledConfig(const aIds: array of string): TFpSonarConfig;
    // Fresh instances carrying the metadata the unit registered globally.
    function NewEmptyBranch: TRuleBase;
    function NewNegatedElse: TRuleBase;
    function NewPathSeparator: TRuleBase;
    function NewLineEnding: TRuleBase;
    function NewRecordLayout: TRuleBase;
    function NewOverlay: TRuleBase;
    function NewByteCountWidth: TRuleBase;
    function NewUnknownSymbol: TRuleBase;
    function NewNeverCompiled: TRuleBase;
    { Runs exactly aRules, all enabled, over the inline source aSrc materialised
      as aName in a temp dir. }
    procedure RunRulesSrc(const aRules: array of TRuleBase;
      const aIds: array of string; const aName: string;
      const aSrc: array of string; const aCollector: TFpSonarIssueCollector);
    // As RunAloneSrc, additionally configuring the string param aKey for aRule.
    procedure RunAloneParamSrc(aRule: TRuleBase;
      const aRuleId, aKey, aValue, aName: string;
      const aSrc: array of string; const aCollector: TFpSonarIssueCollector);
    { Runs both rules, both enabled, over the inline source aSrc materialised
      as aName in a temp dir. }
    procedure RunRuleSrc(const aName: string; const aSrc: array of string;
      const aCollector: TFpSonarIssueCollector);
    // As RunRuleSrc, but aRule is the only rule registered and enabled.
    procedure RunAloneSrc(aRule: TRuleBase; const aRuleId, aName: string;
      const aSrc: array of string; const aCollector: TFpSonarIssueCollector);
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // The index of the aNth (0-based) issue carrying aId, or -1.
    function NthById(const aCollector: TFpSonarIssueCollector;
      const aId: string; aNth: Integer): Integer;
    // How often aId fires when both rules run over the inline source aSrc.
    function CountSrc(const aName, aId: string;
      const aSrc: array of string): Integer;
  published
    procedure EmptyConditionalBranchPositions;
    procedure EmptyConditionalBranchBranchContent;
    procedure EmptyConditionalBranchDirectiveForms;
    procedure EmptyConditionalBranchMacPasSpellings;
    procedure EmptyConditionalBranchSilentShapes;
    procedure EmptyConditionalBranchSilentOnUnbalancedFile;
    procedure EmptyConditionalBranchSurvivesParseFailure;
    procedure NegatedConditionalWithEmptyElsePositions;
    procedure NegatedConditionalWithEmptyElseSilentShapes;
    procedure NegatedConditionalWithEmptyElseSurvivesParseFailure;
    procedure EmptyConditionalBranchOwnsNegatedShapeWithoutItsSibling;
    procedure SectionStraddleIsSilent;
    procedure HardcodedPathSeparatorPositions;
    procedure HardcodedPathSeparatorSilentShapes;
    procedure HardcodedPathSeparatorSurvivesParseFailure;
    procedure HardcodedLineEndingPositions;
    procedure HardcodedLineEndingSilentShapes;
    procedure HardcodedLineEndingSurvivesParseFailure;
    procedure PackedRecordFieldAlignmentAssumptionPositions;
    procedure PackedRecordFieldAlignmentAssumptionSilentShapes;
    procedure PackedRecordFieldAlignmentAssumptionSilentOnUnresolvedOperand;
    procedure PackedRecordFieldAlignmentAssumptionDegradesWithoutResolver;
    procedure PackedRecordFieldAlignmentAssumptionSurvivesParseFailure;
    procedure AbsoluteVariableOverlayPositions;
    procedure AbsoluteVariableOverlaySilentShapes;
    procedure AbsoluteVariableOverlaySilentOnUnresolvedOperand;
    procedure AbsoluteVariableOverlayDegradesWithoutResolver;
    procedure AbsoluteVariableOverlaySurvivesParseFailure;
    procedure PointerSizedDatumTruncatedByByteCountPositions;
    procedure PointerSizedDatumTruncatedByByteCountSilentShapes;
    procedure PointerSizedDatumTruncatedByByteCountSilentOnUnresolvedOperand;
    procedure PointerSizedDatumTruncatedByByteCountDegradesWithoutResolver;
    procedure PointerSizedDatumTruncatedByByteCountSurvivesParseFailure;
    procedure UnknownConditionalSymbolPositions;
    procedure UnknownConditionalSymbolSilentShapes;
    procedure UnknownConditionalSymbolDegradesWithoutResolver;
    procedure UnknownConditionalSymbolSurvivesParseFailure;
    procedure ConditionalBranchNeverCompiledPositions;
    procedure ConditionalBranchNeverCompiledSilentShapes;
    procedure ConditionalBranchNeverCompiledDegradesWithoutResolver;
    procedure ConditionalBranchNeverCompiledSurvivesParseFailure;
    procedure TypoFiresBothRules;
    procedure KnownSymbolSeparatesTheTwoRules;
    procedure EmptyParamMakesEverySymbolUnknown;
    procedure DeadBranchIsDisjointFromEmptyBranch;
    procedure CondCompRulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cDefines: array[0..3] of string = ('FPC', 'CPUX86_64', 'UNIX', 'LINUX');
  cEmptyId = 'EmptyConditionalBranch';
  cNegatedId = 'NegatedConditionalWithEmptyElse';
  cSeparatorId = 'HardcodedPathSeparator';
  cLineEndId = 'HardcodedLineEnding';
  cRecordLayoutId = 'PackedRecordFieldAlignmentAssumption';
  cOverlayId = 'AbsoluteVariableOverlay';
  cByteCountWidthId = 'PointerSizedDatumTruncatedByByteCount';
  cPointerArithId = 'PointerArithmeticWithoutModeswitch';
  cCastId = 'PlatformDependentCast';
  cTruncationId = 'PlatformDependentTruncation';
  cUnknownSymbolId = 'UnknownConditionalSymbol';
  cNeverCompiledId = 'ConditionalBranchNeverCompiled';
  cErrorId = 'RuleError';
  cParseErrorId = 'ParseError';
  cScanErrorId = 'ScanError';
  cKnownSymbolsParam = 'knownSymbols';

  // Noncompliant: line 8 opens a conditional line 9 closes with nothing in it.
  cEmptyThen: array[0..10] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef FEATURE_X}',
    '{$endif}',
    'end;',
    'end.');

  // Noncompliant: the else branch opened on line 10 is empty.
  cEmptyElsePositive: array[0..12] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef FEATURE_X}',
    '  WriteLn(''x'');',
    '{$else}',
    '{$endif}',
    'end;',
    'end.');

  cCompliantBothBranches: array[0..13] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef FEATURE_X}',
    '  WriteLn(''x'');',
    '{$else}',
    '  WriteLn(''y'');',
    '{$endif}',
    'end;',
    'end.');

  // Noncompliant: a negated guard whose else branch on line 10 is empty.
  cNegatedEmptyElse: array[0..12] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifndef PAS2JS}',
    '  WriteLn(''x'');',
    '{$else}',
    '{$endif}',
    'end;',
    'end.');

  cCompliantNegated: array[0..13] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifndef PAS2JS}',
    '  WriteLn(''x'');',
    '{$else}',
    '  WriteLn(''y'');',
    '{$endif}',
    'end;',
    'end.');

  // Both branches of a negated guard are empty, so neither is the sibling's.
  cBothBranchesEmptyNegated: array[0..11] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifndef PAS2JS}',
    '{$else}',
    '{$endif}',
    'end;',
    'end.');

  cCommentOnlyBranch: array[0..11] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef FEATURE_X}',
    '  { note }',
    '{$endif}',
    'end;',
    'end.');

  // The inner conditional is the outer branch's content and is itself empty.
  cNestedConditional: array[0..12] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef ALPHA}',
    '{$ifdef BETA}',
    '{$endif}',
    '{$endif}',
    'end;',
    'end.');

  cSwitchDirectiveContent: array[0..11] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef FEATURE_X}',
    '{$H+}',
    '{$endif}',
    'end;',
    'end.');

  // The {$endif} on line 11 closes the {$if}, so line 12 is ALPHA's else.
  cNonIfdefOpenerNests: array[0..14] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef ALPHA}',
    '{$if declared(Run)}',
    '  WriteLn(''x'');',
    '{$endif}',
    '{$else}',
    '{$endif}',
    'end;',
    'end.');

  cNonIfdefOpenerAlone: array[0..10] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$if declared(Run)}',
    '{$endif}',
    'end;',
    'end.');

  cIfOptOpener: array[0..10] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifopt R+}',
    '{$endif}',
    'end;',
    'end.');

  cElseIfChain: array[0..12] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef ALPHA}',
    '{$elseif defined(BETA)}',
    '  WriteLn(''b'');',
    '{$endif}',
    'end;',
    'end.');

  cUnclosedAtEof: array[0..5] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'implementation',
    'end.',
    '{$ifdef FEATURE_X}');

  cStrayEndif: array[0..11] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef FEATURE_X}',
    '{$endif}',
    '{$endif}',
    'end;',
    'end.');

  cStrayElse: array[0..11] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef FEATURE_X}',
    '{$endif}',
    '{$else}',
    'end;',
    'end.');

  cStrayElseIf: array[0..11] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef FEATURE_X}',
    '{$endif}',
    '{$elseif defined(BETA)}',
    'end;',
    'end.');

  cParenDirectiveForm: array[0..10] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '(*$ifdef FEATURE_X*)',
    '(*$endif*)',
    'end;',
    'end.');

  // The opening directive's body runs from line 8 into line 9.
  cMultiLineDirective: array[0..11] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef',
    'FEATURE_X}',
    '{$endif}',
    'end;',
    'end.');

  // The {$endif} on line 8 is inside a string, so lines 9 and 10 still pair.
  cGuardInStringLiteral: array[0..11] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '  WriteLn(''{$endif}'');',
    '{$ifdef FEATURE_X}',
    '{$endif}',
    'end;',
    'end.');

  // Lines 8 to 12 are one nested comment; lines 13 and 14 are the conditional.
  cNestedComment: array[0..15] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{',
    '  {$ifdef ALPHA}',
    '  WriteLn(''a'');',
    '  {$endif}',
    '}',
    '{$ifdef FEATURE_X}',
    '{$endif}',
    'end;',
    'end.');

  cSectionStraddle: array[0..10] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    '{$ifdef FEATURE_X}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    'end;',
    '{$endif}',
    'end.');

  cSameSection: array[0..11] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef FEATURE_X}',
    '  WriteLn(''x'');',
    '{$endif}',
    'end;',
    'end.');

  cMacPasElseSpelling: array[0..8] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'implementation',
    '{$ifdef FEATURE_X}',
    'const cA = 1;',
    '{$elsec}',
    '{$endif}',
    'end.');

  cMacPasEndSpelling: array[0..6] of string = (
    'unit condprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'implementation',
    '{$ifdef FEATURE_X}',
    '{$endc}',
    'end.');

  cEmptyThenParseFailure: array[0..8] of string = (
    'unit condbroken;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TBroken = class(;',
    'implementation',
    '{$ifdef FEATURE_X}',
    '{$endif}',
    'end.');

  cNegatedParseFailure: array[0..10] of string = (
    'unit condbroken;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TBroken = class(;',
    'implementation',
    '{$ifndef PAS2JS}',
    'const cA = 1;',
    '{$else}',
    '{$endif}',
    'end.');

  // Noncompliant: lines 10 to 13 each join a lone separator with +.
  cSeparatorConcatenated: array[0..14] of string = (
    'unit portprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  lDir, lName, lA, lB: string;',
    'begin',
    '  lDir := lDir + ''\'';',
    '  lDir := ''/'' + lName;',
    '  lDir := lA + ''/'' + lB;',
    '  lDir := lDir + { c } ''\'';',
    'end;',
    'end.');

  cSeparatorCompliant: array[0..26] of string = (
    'unit portprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  lDir, lS: string;',
    '  lCh: char;',
    'begin',
    '  lCh := ''a'';',
    '  lS := '''';',
    '  if lCh = ''/'' then',
    '    lDir := lS;',
    '  if Pos(''\'', lS) > 0 then',
    '    lDir := lS;',
    '  if lCh in [''\'', ''/''] then',
    '    lDir := lS;',
    '  case lCh of',
    '    ''/'': lDir := lS;',
    '  end;',
    '  lDir := lDir + ''/usr/local'';',
    '  lDir := lDir + ''C:\tmp'';',
    '  lDir := lDir + #92;',
    '  lDir := lDir + PathDelim;',
    'end;',
    'end.');

  // The separator on line 11 sits in a branch the scanner never emits.
  cSeparatorExcludedBranch: array[0..14] of string = (
    'unit portprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  lDir: string;',
    'begin',
    '{$ifdef NEVER_DEFINED}',
    '  lDir := lDir + ''\'';',
    '{$endif}',
    '  lDir := '''';',
    'end;',
    'end.');

  // Noncompliant: lines 10 to 13 each carry a CR+LF escape pair.
  cLineEndingHardcoded: array[0..14] of string = (
    'unit portprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  lS: string;',
    'begin',
    '  lS := ''a''#13#10;',
    '  lS := #13#10;',
    '  lS := #$0D#$0A;',
    '  lS := ''a''#13#10''b'';',
    'end;',
    'end.');

  cLineEndingCompliant: array[0..15] of string = (
    'unit portprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  lS: string;',
    'begin',
    '  lS := ''ends with #13#10 here'';',
    '  lS := #10;',
    '  lS := #13;',
    '  lS := #10#13;',
    '  lS := ''a'' + LineEnding;',
    'end;',
    'end.');

  // Both noncompliant shapes behind a syntax error the parser cannot pass.
  cPortabilityParseFailure: array[0..13] of string = (
    'unit portbroken;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TBroken = class(;',
    'implementation',
    'procedure Run;',
    'var',
    '  lDir, lS: string;',
    'begin',
    '  lDir := lDir + ''\'';',
    '  lS := ''a''#13#10;',
    'end;',
    'end.');

  // Noncompliant: lines 28 to 30 each count a non-packed record's size for I/O.
  cRecordIOPositive: array[0..31] of string = (
    'unit layoutprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TR = record',
    '    a: Byte;',
    '    b: LongInt;',
    '  end;',
    '  TProbeStream = class(TObject)',
    '  public',
    '    procedure WriteBuffer(var aBuf; aCount: LongInt);',
    '  end;',
    'procedure Run;',
    'implementation',
    'var',
    '  GHandle: LongInt;',
    'procedure BlockWrite(var aHandle: LongInt; var aBuf; aCount: LongInt);',
    'begin',
    'end;',
    'procedure TProbeStream.WriteBuffer(var aBuf; aCount: LongInt);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lR: TR;',
    '  lStream: TProbeStream;',
    'begin',
    '  BlockWrite(GHandle, lR, SizeOf(TR));',
    '  BlockWrite(GHandle, lR, SizeOf(lR));',
    '  lStream.WriteBuffer(lR, SizeOf(TR));',
    'end;',
    'end.');

  cRecordIOCompliant: array[0..50] of string = (
    'unit layoutprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TP = packed record',
    '    a: Byte;',
    '    b: LongInt;',
    '  end;',
    '  TB = bitpacked record',
    '    a: Byte;',
    '    b: LongInt;',
    '  end;',
    '  TA = record',
    '    a: Byte;',
    '    b: LongInt;',
    '  end align 1;',
    '  TR = record',
    '    a: Byte;',
    '    b: LongInt;',
    '  end;',
    'procedure Run;',
    'implementation',
    'var',
    '  GHandle: LongInt;',
    'procedure BlockWrite(var aHandle: LongInt; var aBuf; aCount: LongInt);',
    'begin',
    'end;',
    'procedure FillChar(var aBuf; aCount: LongInt; aValue: Byte);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lP: TP;',
    '  lB: TB;',
    '  lA: TA;',
    '  lR: TR;',
    '  lBuf: array[0..9] of Byte;',
    '  lPtr: Pointer;',
    '  lCount, lN: LongInt;',
    'begin',
    '  BlockWrite(GHandle, lP, SizeOf(TP));',
    '  BlockWrite(GHandle, lB, SizeOf(TB));',
    '  BlockWrite(GHandle, lA, SizeOf(TA));',
    '  BlockWrite(GHandle, lBuf, SizeOf(lBuf));',
    '  BlockWrite(GHandle, lBuf, SizeOf(LongInt));',
    '  BlockWrite(GHandle, lR, lCount * SizeOf(TR));',
    '  lN := SizeOf(TR);',
    '  FillChar(lR, SizeOf(TR), 0);',
    '  GetMem(lPtr, SizeOf(TR));',
    'end;',
    'end.');

  // Operands the rule reaches and skips for kind, plus a record control on line 27.
  cRecordIOSizeOfNonRecord: array[0..28] of string = (
    'unit layoutprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TC = class',
    '  end;',
    '  TR = record',
    '    a: Byte;',
    '    b: LongInt;',
    '  end;',
    'procedure Run;',
    'implementation',
    'var',
    '  GHandle: LongInt;',
    'procedure BlockWrite(var aHandle: LongInt; var aBuf; aCount: LongInt);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lC: TC;',
    '  lArr: array[0..3] of LongInt;',
    '  lR: TR;',
    'begin',
    '  BlockWrite(GHandle, lArr, SizeOf(lArr));',
    '  BlockWrite(GHandle, lC, SizeOf(TC));',
    '  BlockWrite(GHandle, lArr, SizeOf(Byte));',
    '  BlockWrite(GHandle, lR, SizeOf(TR));',
    'end;',
    'end.');

  // TMissingRecord is declared nowhere, so the closure never resolves.
  cRecordIOUnresolved: array[0..16] of string = (
    'unit layoutprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'var',
    '  GHandle: LongInt;',
    'procedure BlockWrite(var aHandle: LongInt; var aBuf; aCount: LongInt);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lR: TMissingRecord;',
    'begin',
    '  BlockWrite(GHandle, lR, SizeOf(TMissingRecord));',
    'end;',
    'end.');

  // Both noncompliant shapes in a unit whose uses clause never resolves.
  cLayoutNoResolution: array[0..24] of string = (
    'unit layoutprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses NoSuchUnitForFpSonar;',
    'type',
    '  TR = record',
    '    a: Byte;',
    '    b: LongInt;',
    '  end;',
    'procedure Run;',
    'implementation',
    'var',
    '  GHandle: LongInt;',
    '  lC: Char;',
    '  lW: Word absolute lC;',
    'procedure BlockWrite(var aHandle: LongInt; var aBuf; aCount: LongInt);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lR: TR;',
    'begin',
    '  BlockWrite(GHandle, lR, SizeOf(TR));',
    'end;',
    'end.');

  // Both noncompliant shapes behind a syntax error the parser cannot pass.
  cLayoutParseFailure: array[0..23] of string = (
    'unit layoutbroken;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TBroken = class(;',
    '  TR = record',
    '    a: Byte;',
    '    b: LongInt;',
    '  end;',
    'implementation',
    'var',
    '  GHandle: LongInt;',
    '  lC: Char;',
    '  lW: Word absolute lC;',
    'procedure BlockWrite(var aHandle: LongInt; var aBuf; aCount: LongInt);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lR: TR;',
    'begin',
    '  BlockWrite(GHandle, lR, SizeOf(TR));',
    'end;',
    'end.');

  // Noncompliant: the overlays on lines 8, 10 and 13 all differ in size.
  cOverlayMismatched: array[0..16] of string = (
    'unit overlayprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run(aB: Byte);',
    'implementation',
    'var',
    '  lC: Char;',
    '  lW: Word absolute lC;',
    '  lI: LongInt;',
    '  lB: Byte absolute lI;',
    'procedure Run(aB: Byte);',
    'var',
    '  lLong: LongInt absolute aB;',
    'begin',
    '  lLong := 0;',
    'end;',
    'end.');

  cOverlayCompliant: array[0..26] of string = (
    'unit overlayprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TR = record',
    '    a: Byte;',
    '    b: LongInt;',
    '  end;',
    '  TProbe = class(TObject)',
    '  end;',
    'procedure Run;',
    'implementation',
    'var',
    '  lI: LongInt;',
    '  lCard: Cardinal absolute lI;',
    '  lB: Byte;',
    '  lS: ShortInt absolute lB;',
    '  lR: TR;',
    '  lOverRec: Byte absolute lR;',
    '  lJ: LongInt;',
    '  lRecOver: TR absolute lJ;',
    '  lD: TObject;',
    '  lO: TProbe absolute lD;',
    'procedure Run;',
    'begin',
    'end;',
    'end.');

  // The resolver refuses an absolute over a numeric address.
  cOverlayAddress: array[0..10] of string = (
    'unit overlayprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'var',
    '  lB: Byte absolute $1234;',
    'procedure Run;',
    'begin',
    'end;',
    'end.');

  cOverlayField: array[0..16] of string = (
    'unit overlayprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TR = record',
    '    a: Byte;',
    '    b: LongInt;',
    '  end;',
    'procedure Run;',
    'implementation',
    'var',
    '  lRec: TR;',
    '  lW: Word absolute lRec.b;',
    'procedure Run;',
    'begin',
    'end;',
    'end.');

  // TMissingWidth is declared nowhere, so the closure never resolves.
  cOverlayUnresolved: array[0..11] of string = (
    'unit overlayprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'var',
    '  lI: LongInt;',
    '  lW: TMissingWidth absolute lI;',
    'procedure Run;',
    'begin',
    'end;',
    'end.');

  // Noncompliant: lines 25 and 26 count a pointer-sized datum in fixed widths.
  cByteCountNoncompliant: array[0..27] of string = (
    'unit widthprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TProbeStream = class(TObject)',
    '  public',
    '    procedure WriteBuffer(var aBuf; aCount: LongInt);',
    '  end;',
    'procedure Run;',
    'implementation',
    'var',
    '  GHandle: LongInt;',
    'procedure BlockWrite(var aHandle: LongInt; var aBuf; aCount: LongInt);',
    'begin',
    'end;',
    'procedure TProbeStream.WriteBuffer(var aBuf; aCount: LongInt);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lPtr: Pointer;',
    '  lPI: PtrInt;',
    '  lStream: TProbeStream;',
    'begin',
    '  BlockWrite(GHandle, lPtr, SizeOf(Integer));',
    '  lStream.WriteBuffer(lPI, SizeOf(Cardinal));',
    'end;',
    'end.');

  // Noncompliant: line 16 counts a pointer in the four-argument BlockWrite.
  cByteCountResultArg: array[0..17] of string = (
    'unit widthprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'var',
    '  GHandle: LongInt;',
    'procedure BlockWrite(var aHandle: LongInt; var aBuf;'
      + ' aCount: LongInt; var aWritten: LongInt);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lPtr: Pointer;',
    '  lWritten: LongInt;',
    'begin',
    '  BlockWrite(GHandle, lPtr, SizeOf(Integer), lWritten);',
    'end;',
    'end.');

  { Every silent shape of the width rule, plus the two casts on lines 39 and 40
    that make its zero a measurement rather than a dark resolver. }
  cByteCountCompliant: array[0..41] of string = (
    'unit widthprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'var',
    '  GHandle: LongInt;',
    'procedure BlockWrite(var aHandle: LongInt; var aBuf; aCount: LongInt);'
      + ' overload;',
    'begin',
    'end;',
    'procedure BlockWrite(var aHandle: LongInt; var aBuf;'
      + ' aCount, aRead: LongInt); overload;',
    'begin',
    'end;',
    'procedure FillChar(var aBuf; aCount: LongInt; aValue: Byte);',
    'begin',
    'end;',
    'procedure Sink(var aBuf);',
    'begin',
    '  BlockWrite(GHandle, aBuf, SizeOf(Integer));',
    'end;',
    'procedure Run;',
    'var',
    '  lPtr: Pointer;',
    '  lPI: PtrInt;',
    '  lI: Integer;',
    '  lW: Int64;',
    '  lN, lC: LongInt;',
    'begin',
    '  BlockWrite(GHandle, lPtr, SizeOf(Pointer));',
    '  BlockWrite(GHandle, lPI, SizeOf(PtrInt));',
    '  BlockWrite(GHandle, lPI, SizeOf(NativeUInt));',
    '  BlockWrite(GHandle, lI, SizeOf(Integer));',
    '  BlockWrite(GHandle, lPtr, SizeOf(Int64));',
    '  BlockWrite(GHandle, lW, SizeOf(Integer));',
    '  BlockWrite(GHandle, lPtr, lN, SizeOf(Integer));',
    '  FillChar(lPtr, SizeOf(Integer), 0);',
    '  lN := SizeOf(Integer);',
    '  BlockWrite(GHandle, lPtr, lC * SizeOf(Integer));',
    '  lN := Integer(lPtr);',
    '  lN := Integer(lPI);',
    'end;',
    'end.');

  // The three-argument overload shape: line 20 counts an Offset, not bytes.
  cByteCountOffsetOverload: array[0..21] of string = (
    'unit widthprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TProbeStream = class(TObject)',
    '  public',
    '    procedure WriteBuffer(var aBuf; aOffset, aCount: LongInt);',
    '  end;',
    'procedure Run;',
    'implementation',
    'procedure TProbeStream.WriteBuffer(var aBuf; aOffset, aCount: LongInt);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lPtr: Pointer;',
    '  lStream: TProbeStream;',
    '  lCount: LongInt;',
    'begin',
    '  lStream.WriteBuffer(lPtr, SizeOf(Integer), lCount);',
    'end;',
    'end.');

  // Pointer arithmetic on lines 17 to 19, and a width control on line 20.
  cByteCountPointerArith: array[0..21] of string = (
    'unit widthprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'var',
    '  GHandle: LongInt;',
    'procedure BlockWrite(var aHandle: LongInt; var aBuf; aCount: LongInt);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lP1, lP2: PByte;',
    '  lD: PtrInt;',
    '  lPtr: Pointer;',
    'begin',
    '  lP1 := lP1 + 1;',
    '  lP2 := lP2 - 1;',
    '  lD := lP1 - lP2;',
    '  BlockWrite(GHandle, lPtr, SizeOf(Integer));',
    'end;',
    'end.');

  // One call the record rule owns on line 21, one the width rule owns on 22.
  cByteCountSharedCall: array[0..23] of string = (
    'unit widthprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TR = record',
    '    a: Byte;',
    '    b: LongInt;',
    '  end;',
    'procedure Run;',
    'implementation',
    'var',
    '  GHandle: LongInt;',
    'procedure BlockWrite(var aHandle: LongInt; var aBuf; aCount: LongInt);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lR: TR;',
    '  lPtr: Pointer;',
    'begin',
    '  BlockWrite(GHandle, lR, SizeOf(TR));',
    '  BlockWrite(GHandle, lPtr, SizeOf(Integer));',
    'end;',
    'end.');

  // TMissingWidth is declared nowhere, so the closure never resolves.
  cByteCountUnresolved: array[0..16] of string = (
    'unit widthprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'var',
    '  GHandle: LongInt;',
    'procedure BlockWrite(var aHandle: LongInt; var aBuf; aCount: LongInt);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lPtr: Pointer;',
    'begin',
    '  BlockWrite(GHandle, lPtr, SizeOf(TMissingWidth));',
    'end;',
    'end.');

  { The untyped var buffer on line 13 has no resolved type while the rest of the
    module, line 19 included, resolves. }
  cByteCountUntypedBuffer: array[0..20] of string = (
    'unit widthprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'var',
    '  GHandle: LongInt;',
    'procedure BlockWrite(var aHandle: LongInt; var aBuf; aCount: LongInt);',
    'begin',
    'end;',
    'procedure Sink(var aBuf);',
    'begin',
    '  BlockWrite(GHandle, aBuf, SizeOf(Integer));',
    'end;',
    'procedure Run;',
    'var',
    '  lPtr: Pointer;',
    'begin',
    '  BlockWrite(GHandle, lPtr, SizeOf(Integer));',
    'end;',
    'end.');

  // The noncompliant shape in a unit whose uses clause never resolves.
  cWidthNoResolution: array[0..17] of string = (
    'unit widthprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses NoSuchUnitForFpSonar;',
    'procedure Run;',
    'implementation',
    'var',
    '  GHandle: LongInt;',
    'procedure BlockWrite(var aHandle: LongInt; var aBuf; aCount: LongInt);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lPtr: Pointer;',
    'begin',
    '  BlockWrite(GHandle, lPtr, SizeOf(Integer));',
    'end;',
    'end.');

  // The noncompliant shape behind a syntax error the parser cannot pass.
  cWidthParseFailure: array[0..17] of string = (
    'unit widthbroken;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TBroken = class(;',
    'implementation',
    'var',
    '  GHandle: LongInt;',
    'procedure BlockWrite(var aHandle: LongInt; var aBuf; aCount: LongInt);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lPtr: Pointer;',
    'begin',
    '  BlockWrite(GHandle, lPtr, SizeOf(Integer));',
    'end;',
    'end.');

  // Noncompliant: WNIDOWS is a typo, so line 8 guards a branch nothing defines.
  cDefineTypo: array[0..11] of string = (
    'unit defineprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef WNIDOWS}',
    '  WriteLn(''x'');',
    '{$endif}',
    'end;',
    'end.');

  // Noncompliant for the branch rule alone: WINDOWS is known but not defined.
  cDefineKnownOther: array[0..11] of string = (
    'unit defineprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef WINDOWS}',
    '  WriteLn(''x'');',
    '{$endif}',
    'end;',
    'end.');

  cDefineDefinedForRun: array[0..11] of string = (
    'unit defineprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef LINUX}',
    '  WriteLn(''x'');',
    '{$endif}',
    'end;',
    'end.');

  cDefineOwnSymbol: array[0..12] of string = (
    'unit defineprobe;',
    '{$mode objfpc}{$H+}',
    '{$define OWN}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef OWN}',
    '  WriteLn(''x'');',
    '{$endif}',
    'end;',
    'end.');

  // A lowercase spelling of a symbol cDefines carries: {$ifdef} is case
  // insensitive.
  cDefineCaseFolded: array[0..11] of string = (
    'unit defineprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef linux}',
    '  WriteLn(''x'');',
    '{$endif}',
    'end;',
    'end.');

  // A macro define lands in Macros rather than Defines; the capture unions both.
  cDefineMacroSymbol: array[0..13] of string = (
    'unit defineprobe;',
    '{$mode objfpc}{$H+}',
    '{$macro on}',
    '{$define MAC := 1}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef MAC}',
    '  WriteLn(''x'');',
    '{$endif}',
    'end;',
    'end.');

  // Noncompliant for the branch rule alone: the negated guard holds.
  cDefineNegatedDefined: array[0..11] of string = (
    'unit defineprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifndef LINUX}',
    '  WriteLn(''x'');',
    '{$endif}',
    'end;',
    'end.');

  // Noncompliant for the symbol rule alone: negated on an absent symbol.
  cDefineNegatedAbsent: array[0..11] of string = (
    'unit defineprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifndef WNIDOWS}',
    '  WriteLn(''x'');',
    '{$endif}',
    'end;',
    'end.');

  cDefineEmptyDead: array[0..9] of string = (
    'unit defineprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef WNIDOWS}{$endif}',
    'end;',
    'end.');

  cDefineElseIfChain: array[0..13] of string = (
    'unit defineprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef WNIDOWS}',
    '  WriteLn(''a'');',
    '{$elseif defined(BETA)}',
    '  WriteLn(''b'');',
    '{$endif}',
    'end;',
    'end.');

  // The conditional sits after end. so the parse still completes.
  cDefineUnclosed: array[0..6] of string = (
    'unit defineprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'implementation',
    'end.',
    '{$ifdef WNIDOWS}',
    'const cX = 1;');

  cDefineSymbolLess: array[0..14] of string = (
    'unit defineprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$if defined(WNIDOWS)}',
    '  WriteLn(''a'');',
    '{$endif}',
    '{$ifopt H+}',
    '  WriteLn(''b'');',
    '{$endif}',
    'end;',
    'end.');

  cDefineCuratedGlob: array[0..11] of string = (
    'unit defineprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef CPU_MADE_UP}',
    '  WriteLn(''x'');',
    '{$endif}',
    'end;',
    'end.');

  cDefineInComment: array[0..10] of string = (
    'unit defineprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '// {$ifdef WNIDOWS}',
    '  WriteLn(''{$ifdef WNIDOWS}'');',
    'end;',
    'end.');

  // The include on line 4 is never staged, so the scan cannot complete.
  cDefineScanFailure: array[0..8] of string = (
    'unit defineprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    '{$i fpsonar93absent.inc}',
    'implementation',
    '{$ifdef WNIDOWS}',
    'const cX = 1;',
    '{$endif}',
    'end.');

  // The noncompliant shape behind a syntax error the parser cannot pass.
  cDefineParseFailure: array[0..9] of string = (
    'unit definebroken;',
    '{$mode objfpc}{$H+}',
    'interface',
    '{$ifdef WNIDOWS}',
    'const cX = 1;',
    '{$endif}',
    'type',
    '  TBroken = class(;',
    'implementation',
    'end.');

  // One empty dead branch on line 8 and one populated dead branch on line 10.
  cDefineEmptyAndPopulatedDead: array[0..13] of string = (
    'unit defineprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    '{$ifdef WNIDOWS}',
    '{$endif}',
    '{$ifdef WNIDOWS}',
    '  WriteLn(''x'');',
    '{$endif}',
    'end;',
    'end.');


function TRulesCondCompTest.EnabledConfig(
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


function TRulesCondCompTest.NewEmptyBranch: TRuleBase;

begin
  AssertNotNull(cEmptyId + ' is registered', RuleRegistry.FindById(cEmptyId));
  Result := TRuleEmptyConditionalBranch.Create(
    RuleRegistry.FindById(cEmptyId).Metadata);
end;


function TRulesCondCompTest.NewNegatedElse: TRuleBase;

begin
  AssertNotNull(cNegatedId + ' is registered', RuleRegistry.FindById(cNegatedId));
  Result := TRuleNegatedConditionalWithEmptyElse.Create(
    RuleRegistry.FindById(cNegatedId).Metadata);
end;


function TRulesCondCompTest.NewPathSeparator: TRuleBase;

begin
  AssertNotNull(cSeparatorId + ' is registered',
    RuleRegistry.FindById(cSeparatorId));
  Result := TRuleHardcodedPathSeparator.Create(
    RuleRegistry.FindById(cSeparatorId).Metadata);
end;


function TRulesCondCompTest.NewLineEnding: TRuleBase;

begin
  AssertNotNull(cLineEndId + ' is registered',
    RuleRegistry.FindById(cLineEndId));
  Result := TRuleHardcodedLineEnding.Create(
    RuleRegistry.FindById(cLineEndId).Metadata);
end;


function TRulesCondCompTest.NewRecordLayout: TRuleBase;

begin
  AssertNotNull(cRecordLayoutId + ' is registered',
    RuleRegistry.FindById(cRecordLayoutId));
  Result := TRulePackedRecordFieldAlignmentAssumption.Create(
    RuleRegistry.FindById(cRecordLayoutId).Metadata);
end;


function TRulesCondCompTest.NewOverlay: TRuleBase;

begin
  AssertNotNull(cOverlayId + ' is registered',
    RuleRegistry.FindById(cOverlayId));
  Result := TRuleAbsoluteVariableOverlay.Create(
    RuleRegistry.FindById(cOverlayId).Metadata);
end;


function TRulesCondCompTest.NewByteCountWidth: TRuleBase;

begin
  AssertNotNull(cByteCountWidthId + ' is registered',
    RuleRegistry.FindById(cByteCountWidthId));
  Result := TRulePointerSizedDatumTruncatedByByteCount.Create(
    RuleRegistry.FindById(cByteCountWidthId).Metadata);
end;


function TRulesCondCompTest.NewUnknownSymbol: TRuleBase;

begin
  AssertNotNull(cUnknownSymbolId + ' is registered',
    RuleRegistry.FindById(cUnknownSymbolId));
  Result := TRuleUnknownConditionalSymbol.Create(
    RuleRegistry.FindById(cUnknownSymbolId).Metadata);
end;


function TRulesCondCompTest.NewNeverCompiled: TRuleBase;

begin
  AssertNotNull(cNeverCompiledId + ' is registered',
    RuleRegistry.FindById(cNeverCompiledId));
  Result := TRuleConditionalBranchNeverCompiled.Create(
    RuleRegistry.FindById(cNeverCompiledId).Metadata);
end;


procedure TRulesCondCompTest.RunRulesSrc(const aRules: array of TRuleBase;
  const aIds: array of string; const aName: string;
  const aSrc: array of string; const aCollector: TFpSonarIssueCollector);

var
  lFix: TTempFixtures;
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;
  i: Integer;

begin
  lFix := TTempFixtures.Create;
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    for i := Low(aRules) to High(aRules) do
      lReg.Register(aRules[i]);
    lEngine.Config := EnabledConfig(aIds);
    lEngine.Analyze(lFix.Add(aName, aSrc), cMode, cDefines, aCollector);
  finally
    lEngine.Free;
    lReg.Free;
    lFix.Free;
  end;
end;


procedure TRulesCondCompTest.RunAloneParamSrc(aRule: TRuleBase;
  const aRuleId, aKey, aValue, aName: string; const aSrc: array of string;
  const aCollector: TFpSonarIssueCollector);

var
  lFix: TTempFixtures;
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;
  lConfig: TFpSonarConfig;

begin
  lFix := TTempFixtures.Create;
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    lConfig := EnabledConfig([aRuleId]);
    SetLength(lConfig.Rules[0].Params, 1);
    lConfig.Rules[0].Params[0].Key := aKey;
    lConfig.Rules[0].Params[0].Kind := cpkStr;
    lConfig.Rules[0].Params[0].StrVal := aValue;
    lEngine.Config := lConfig;
    lEngine.Analyze(lFix.Add(aName, aSrc), cMode, cDefines, aCollector);
  finally
    lEngine.Free;
    lReg.Free;
    lFix.Free;
  end;
end;


procedure TRulesCondCompTest.RunRuleSrc(const aName: string;
  const aSrc: array of string; const aCollector: TFpSonarIssueCollector);

var
  lFix: TTempFixtures;
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;

begin
  lFix := TTempFixtures.Create;
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(NewEmptyBranch);
    lReg.Register(NewNegatedElse);
    lEngine.Config := EnabledConfig([cEmptyId, cNegatedId]);
    lEngine.Analyze(lFix.Add(aName, aSrc), cMode, cDefines, aCollector);
  finally
    lEngine.Free;
    lReg.Free;
    lFix.Free;
  end;
end;


procedure TRulesCondCompTest.RunAloneSrc(aRule: TRuleBase;
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


function TRulesCondCompTest.CountById(const aCollector: TFpSonarIssueCollector;
  const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesCondCompTest.FirstById(const aCollector: TFpSonarIssueCollector;
  const aId: string): Integer;

begin
  Result := NthById(aCollector, aId, 0);
end;


function TRulesCondCompTest.NthById(const aCollector: TFpSonarIssueCollector;
  const aId: string; aNth: Integer): Integer;

var
  i, lSeen: Integer;

begin
  Result := -1;
  lSeen := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
    begin
      if lSeen = aNth then
        Exit(i);
      Inc(lSeen);
    end;
end;


function TRulesCondCompTest.CountSrc(const aName, aId: string;
  const aSrc: array of string): Integer;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(aName, aSrc, lc);
    Result := CountById(lc, aId);
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.EmptyConditionalBranchPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc('condprobe.pas', cEmptyThen, lc);
    AssertEquals('one empty-branch issue', 1, CountById(lc, cEmptyId));
    AssertEquals('the sibling stands down on a positive guard', 0,
      CountById(lc, cNegatedId));
    k := FirstById(lc, cEmptyId);
    AssertEquals('start line', 8, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 8, lc.Issues[k].EndLine);
    AssertEquals('end col', 18, lc.Issues[k].EndCol);
    AssertEquals('message key', 'rule.' + cEmptyId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('message names the guard symbol',
      'Conditional branch on FEATURE_X is empty',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc('condprobe.pas', cEmptyElsePositive, lc);
    AssertEquals('one empty-branch issue', 1, CountById(lc, cEmptyId));
    k := FirstById(lc, cEmptyId);
    AssertEquals('reported at the else delimiter', 10, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 10, lc.Issues[k].EndLine);
    AssertEquals('end col', 7, lc.Issues[k].EndCol);
    AssertEquals('message names the guard symbol',
      'Conditional branch on FEATURE_X is empty',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  AssertEquals('both branches populated => zero', 0,
    CountSrc('condprobe.pas', cEmptyId, cCompliantBothBranches));
end;


procedure TRulesCondCompTest.EmptyConditionalBranchBranchContent;

var
  lc: TFpSonarIssueCollector;
  k, m: Integer;

begin
  AssertEquals('a comment is not source text', 1,
    CountSrc('condprobe.pas', cEmptyId, cCommentOnlyBranch));
  AssertEquals('a directive other than the delimiters is content', 0,
    CountSrc('condprobe.pas', cEmptyId, cSwitchDirectiveContent));

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc('condprobe.pas', cNestedConditional, lc);
    AssertEquals('the inner conditional only', 1, CountById(lc, cEmptyId));
    k := FirstById(lc, cEmptyId);
    AssertEquals('reported at the inner opener', 9, lc.Issues[k].StartLine);
    AssertEquals('message names the inner symbol',
      'Conditional branch on BETA is empty',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc('condprobe.pas', cBothBranchesEmptyNegated, lc);
    AssertEquals('one issue per empty branch', 2, CountById(lc, cEmptyId));
    AssertEquals('the sibling needs a populated then branch', 0,
      CountById(lc, cNegatedId));
    k := NthById(lc, cEmptyId, 0);
    m := NthById(lc, cEmptyId, 1);
    AssertEquals('then branch reported at the opener', 8,
      lc.Issues[k].StartLine);
    AssertEquals('else branch reported at the else', 9, lc.Issues[m].StartLine);
    AssertTrue('the two branches carry distinct fingerprints',
      lc.Issues[k].Fingerprint <> lc.Issues[m].Fingerprint);
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.EmptyConditionalBranchDirectiveForms;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc('condprobe.pas', cParenDirectiveForm, lc);
    AssertEquals('the (*$...*) form is a directive', 1,
      CountById(lc, cEmptyId));
    k := FirstById(lc, cEmptyId);
    AssertEquals('start line', 8, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end col', 20, lc.Issues[k].EndCol);
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc('condprobe.pas', cMultiLineDirective, lc);
    AssertEquals('a directive body may span lines', 1,
      CountById(lc, cEmptyId));
    k := FirstById(lc, cEmptyId);
    AssertEquals('start line', 8, lc.Issues[k].StartLine);
    AssertEquals('end line', 9, lc.Issues[k].EndLine);
    AssertEquals('end col', 10, lc.Issues[k].EndCol);
    AssertEquals('the symbol is read across the break',
      'Conditional branch on FEATURE_X is empty',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc('condprobe.pas', cGuardInStringLiteral, lc);
    AssertEquals('a guard-shaped string is not a directive', 1,
      CountById(lc, cEmptyId));
    k := FirstById(lc, cEmptyId);
    AssertEquals('start line', 9, lc.Issues[k].StartLine);
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc('condprobe.pas', cNestedComment, lc);
    AssertEquals('a commented-out conditional is not a conditional', 1,
      CountById(lc, cEmptyId));
    k := FirstById(lc, cEmptyId);
    AssertEquals('start line', 13, lc.Issues[k].StartLine);
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.EmptyConditionalBranchMacPasSpellings;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc('condprobe.pas', cMacPasElseSpelling, lc);
    AssertEquals('{$elsec} switches branch, it does not make the entry opaque',
      1, CountById(lc, cEmptyId));
    k := FirstById(lc, cEmptyId);
    AssertEquals('start line', 7, lc.Issues[k].StartLine);
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc('condprobe.pas', cMacPasEndSpelling, lc);
    AssertEquals('{$endc} closes the conditional', 1, CountById(lc, cEmptyId));
    k := FirstById(lc, cEmptyId);
    AssertEquals('start line', 5, lc.Issues[k].StartLine);
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.EmptyConditionalBranchSilentShapes;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  AssertEquals('{$if} is not an {$ifdef} => zero', 0,
    CountSrc('condprobe.pas', cEmptyId, cNonIfdefOpenerAlone));
  AssertEquals('{$ifopt} is not an {$ifdef} => zero', 0,
    CountSrc('condprobe.pas', cEmptyId, cIfOptOpener));
  AssertEquals('an {$elseif} chain is opaque => zero', 0,
    CountSrc('condprobe.pas', cEmptyId, cElseIfChain));
  AssertEquals('an unclosed conditional => zero', 0,
    CountSrc('condprobe.pas', cEmptyId, cUnclosedAtEof));
  AssertEquals('a conditional whose only branch is populated => zero', 0,
    CountSrc('condprobe.pas', cEmptyId, cSameSection));

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc('condprobe.pas', cNonIfdefOpenerNests, lc);
    AssertEquals('the opaque opener is still counted for nesting', 1,
      CountById(lc, cEmptyId));
    k := FirstById(lc, cEmptyId);
    AssertEquals('the outer else is what is empty', 12,
      lc.Issues[k].StartLine);
    AssertEquals('message names the outer symbol',
      'Conditional branch on ALPHA is empty',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.EmptyConditionalBranchSilentOnUnbalancedFile;

begin
  AssertEquals('an {$endif} with nothing open => zero', 0,
    CountSrc('condprobe.pas', cEmptyId, cStrayEndif));
  AssertEquals('an {$else} with nothing open => zero', 0,
    CountSrc('condprobe.pas', cEmptyId, cStrayElse));
  AssertEquals('an {$elseif} with nothing open => zero', 0,
    CountSrc('condprobe.pas', cEmptyId, cStrayElseIf));
end;


procedure TRulesCondCompTest.EmptyConditionalBranchSurvivesParseFailure;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc('condbroken.pas', cEmptyThenParseFailure, lc);
    AssertEquals('count unchanged by the parse failure', 1,
      CountById(lc, cEmptyId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    k := FirstById(lc, cEmptyId);
    AssertEquals('same start line', 7, lc.Issues[k].StartLine);
    AssertEquals('same start col', 1, lc.Issues[k].StartCol);
    AssertEquals('same end col', 18, lc.Issues[k].EndCol);
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.NegatedConditionalWithEmptyElsePositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc('condprobe.pas', cNegatedEmptyElse, lc);
    AssertEquals('one negated-else issue', 1, CountById(lc, cNegatedId));
    AssertEquals('the sibling stands down on this shape', 0,
      CountById(lc, cEmptyId));
    k := FirstById(lc, cNegatedId);
    AssertEquals('start line', 8, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 8, lc.Issues[k].EndLine);
    AssertEquals('end col', 16, lc.Issues[k].EndCol);
    AssertEquals('message key', 'rule.' + cNegatedId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('message names the guard symbol',
      'Negated conditional on PAS2JS has an empty else branch',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  AssertEquals('both branches populated => zero', 0,
    CountSrc('condprobe.pas', cNegatedId, cCompliantNegated));
end;


procedure TRulesCondCompTest.NegatedConditionalWithEmptyElseSilentShapes;

begin
  AssertEquals('a positive guard is not this rule''s shape => zero', 0,
    CountSrc('condprobe.pas', cNegatedId, cEmptyElsePositive));
  AssertEquals('no else branch at all => zero', 0,
    CountSrc('condprobe.pas', cNegatedId, cEmptyThen));
  AssertEquals('an empty then branch as well => zero', 0,
    CountSrc('condprobe.pas', cNegatedId, cBothBranchesEmptyNegated));
  AssertEquals('an {$elseif} chain is opaque => zero', 0,
    CountSrc('condprobe.pas', cNegatedId, cElseIfChain));
  AssertEquals('an unclosed conditional => zero', 0,
    CountSrc('condprobe.pas', cNegatedId, cUnclosedAtEof));
  AssertEquals('an {$endif} with nothing open => zero', 0,
    CountSrc('condprobe.pas', cNegatedId, cStrayEndif));
end;


procedure TRulesCondCompTest.NegatedConditionalWithEmptyElseSurvivesParseFailure;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc('condbroken.pas', cNegatedParseFailure, lc);
    AssertEquals('count unchanged by the parse failure', 1,
      CountById(lc, cNegatedId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    k := FirstById(lc, cNegatedId);
    AssertEquals('same start line', 7, lc.Issues[k].StartLine);
    AssertEquals('same start col', 1, lc.Issues[k].StartCol);
    AssertEquals('same end col', 16, lc.Issues[k].EndCol);
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.EmptyConditionalBranchOwnsNegatedShapeWithoutItsSibling;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // The per-rule config corpus-run.sh synthesises enables one rule only.
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewEmptyBranch, cEmptyId, 'condprobe.pas', cNegatedEmptyElse, lc);
    AssertEquals('the shape is reported when the sibling is off', 1,
      CountById(lc, cEmptyId));
    k := FirstById(lc, cEmptyId);
    AssertEquals('reported at the else delimiter', 10, lc.Issues[k].StartLine);
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.SectionStraddleIsSilent;

begin
  AssertEquals('a conditional wrapping a section is legal => zero', 0,
    CountSrc('condprobe.pas', cEmptyId, cSectionStraddle));
  AssertEquals('a conditional wrapping a section is legal => zero', 0,
    CountSrc('condprobe.pas', cNegatedId, cSectionStraddle));
end;


procedure TRulesCondCompTest.HardcodedPathSeparatorPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewPathSeparator, cSeparatorId, 'portprobe.pas',
      cSeparatorConcatenated, lc);
    AssertEquals('one issue per joined separator', 4,
      CountById(lc, cSeparatorId));

    k := NthById(lc, cSeparatorId, 0);
    AssertEquals('right operand start line', 10, lc.Issues[k].StartLine);
    AssertEquals('right operand start col', 18, lc.Issues[k].StartCol);
    AssertEquals('end line', 10, lc.Issues[k].EndLine);
    AssertEquals('end col', 18, lc.Issues[k].EndCol);
    AssertEquals('message key', 'rule.' + cSeparatorId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('message names the literal as written',
      'Path separator ''\'' is hardcoded in a concatenation',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));

    k := NthById(lc, cSeparatorId, 1);
    AssertEquals('left operand start line', 11, lc.Issues[k].StartLine);
    AssertEquals('left operand start col', 11, lc.Issues[k].StartCol);
    AssertEquals('message names the literal as written',
      'Path separator ''/'' is hardcoded in a concatenation',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));

    k := NthById(lc, cSeparatorId, 2);
    AssertEquals('between operands start line', 12, lc.Issues[k].StartLine);
    AssertEquals('between operands start col', 16, lc.Issues[k].StartCol);

    k := NthById(lc, cSeparatorId, 3);
    AssertEquals('trivia start line', 13, lc.Issues[k].StartLine);
    AssertEquals('the comment between + and the literal is stepped over', 24,
      lc.Issues[k].StartCol);
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewPathSeparator, cSeparatorId, 'portprobe.pas',
      cSeparatorCompliant, lc);
    AssertEquals('the compliant fixture => zero', 0,
      CountById(lc, cSeparatorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.HardcodedPathSeparatorSilentShapes;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewPathSeparator, cSeparatorId, 'portprobe.pas',
      cSeparatorCompliant, lc);
    AssertEquals('a separator that is compared, indexed, set-tested, a case '
      + 'label, part of a longer literal, spelled numerically or replaced by '
      + 'PathDelim => zero', 0, CountById(lc, cSeparatorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewPathSeparator, cSeparatorId, 'portprobe.pas',
      cSeparatorExcludedBranch, lc);
    AssertEquals('a separator in a non-taken branch is never scanned => zero',
      0, CountById(lc, cSeparatorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewPathSeparator, cSeparatorId, 'portprobe.pas',
      cLineEndingHardcoded, lc);
    AssertEquals('the line-ending shapes are not this rule''s => zero', 0,
      CountById(lc, cSeparatorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewPathSeparator, cSeparatorId, 'portprobe.pas',
      cLineEndingCompliant, lc);
    AssertEquals('the lone and reversed escapes are silent here too => zero', 0,
      CountById(lc, cSeparatorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.HardcodedPathSeparatorSurvivesParseFailure;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewPathSeparator, cSeparatorId, 'portbroken.pas',
      cPortabilityParseFailure, lc);
    AssertTrue('the fixture really fails to parse',
      CountById(lc, cParseErrorId) > 0);
    AssertEquals('count unchanged by the parse failure', 1,
      CountById(lc, cSeparatorId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    k := FirstById(lc, cSeparatorId);
    AssertEquals('same start line', 11, lc.Issues[k].StartLine);
    AssertEquals('same start col', 18, lc.Issues[k].StartCol);
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.HardcodedLineEndingPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewLineEnding, cLineEndId, 'portprobe.pas',
      cLineEndingHardcoded, lc);
    AssertEquals('one issue per literal carrying the pair', 4,
      CountById(lc, cLineEndId));

    k := NthById(lc, cLineEndId, 0);
    AssertEquals('trailing pair start line', 10, lc.Issues[k].StartLine);
    AssertEquals('reported at the whole literal', 9, lc.Issues[k].StartCol);
    AssertEquals('end line', 10, lc.Issues[k].EndLine);
    AssertEquals('end col', 9, lc.Issues[k].EndCol);
    AssertEquals('message key', 'rule.' + cLineEndId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('message names the pair', 'Line ending #13#10 is hardcoded',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));

    k := NthById(lc, cLineEndId, 1);
    AssertEquals('bare pair start line', 11, lc.Issues[k].StartLine);
    AssertEquals('start col', 9, lc.Issues[k].StartCol);

    k := NthById(lc, cLineEndId, 2);
    AssertEquals('hex pair start line', 12, lc.Issues[k].StartLine);
    AssertEquals('message names the pair as written',
      'Line ending #$0D#$0A is hardcoded',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));

    k := NthById(lc, cLineEndId, 3);
    AssertEquals('embedded pair start line', 13, lc.Issues[k].StartLine);
    AssertEquals('adjacent parts are one token, so one issue', 9,
      lc.Issues[k].StartCol);
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewLineEnding, cLineEndId, 'portprobe.pas',
      cLineEndingCompliant, lc);
    AssertEquals('the compliant fixture => zero', 0, CountById(lc, cLineEndId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.HardcodedLineEndingSilentShapes;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewLineEnding, cLineEndId, 'portprobe.pas',
      cLineEndingCompliant, lc);
    AssertEquals('a pair inside quoted text, a lone or reversed escape and '
      + 'LineEnding itself => zero', 0, CountById(lc, cLineEndId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewLineEnding, cLineEndId, 'portprobe.pas',
      cSeparatorConcatenated, lc);
    AssertEquals('the separator shapes are not this rule''s => zero', 0,
      CountById(lc, cLineEndId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.HardcodedLineEndingSurvivesParseFailure;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewLineEnding, cLineEndId, 'portbroken.pas',
      cPortabilityParseFailure, lc);
    AssertTrue('the fixture really fails to parse',
      CountById(lc, cParseErrorId) > 0);
    AssertEquals('count unchanged by the parse failure', 1,
      CountById(lc, cLineEndId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    k := FirstById(lc, cLineEndId);
    AssertEquals('same start line', 12, lc.Issues[k].StartLine);
    AssertEquals('same start col', 9, lc.Issues[k].StartCol);
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.PackedRecordFieldAlignmentAssumptionPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewRecordLayout, cRecordLayoutId, 'layoutprobe.pas',
      cRecordIOPositive, lc);
    AssertEquals('one issue per counted SizeOf', 3,
      CountById(lc, cRecordLayoutId));

    k := NthById(lc, cRecordLayoutId, 0);
    AssertEquals('type operand start line', 28, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 28, lc.Issues[k].EndLine);
    AssertEquals('end col', 1, lc.Issues[k].EndCol);
    AssertEquals('message key', 'rule.' + cRecordLayoutId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('message names the record',
      'Size of non-packed record TR is used as an I/O byte count',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));

    k := NthById(lc, cRecordLayoutId, 1);
    AssertEquals('variable operand start line', 29, lc.Issues[k].StartLine);
    AssertEquals('the operand resolves to the same record',
      'Size of non-packed record TR is used as an I/O byte count',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));

    k := NthById(lc, cRecordLayoutId, 2);
    AssertEquals('qualified callee start line', 30, lc.Issues[k].StartLine);
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewRecordLayout, cRecordLayoutId, 'layoutprobe.pas',
      cRecordIOCompliant, lc);
    AssertEquals('the compliant fixture => zero', 0,
      CountById(lc, cRecordLayoutId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.PackedRecordFieldAlignmentAssumptionSilentShapes;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewRecordLayout, cRecordLayoutId, 'layoutprobe.pas',
      cRecordIOCompliant, lc);
    AssertEquals('a packed, bit-packed or explicitly aligned record, a '
      + 'non-record operand, an arithmetic byte count and a SizeOf outside any '
      + 'byte-counting I/O call => zero', 0, CountById(lc, cRecordLayoutId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  // Liveness control for the zeros above: the same declarations and stubs in
  // the noncompliant fixture do fire.
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewRecordLayout, cRecordLayoutId, 'layoutprobe.pas',
      cRecordIOPositive, lc);
    AssertTrue('the silent shapes are silent by predicate, not by a dark '
      + 'resolver', CountById(lc, cRecordLayoutId) > 0);
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewRecordLayout, cRecordLayoutId, 'overlayprobe.pas',
      cOverlayMismatched, lc);
    AssertEquals('the overlay shapes are not this rule''s => zero', 0,
      CountById(lc, cRecordLayoutId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.
  PackedRecordFieldAlignmentAssumptionSilentOnUnresolvedOperand;

var
  lc: TFpSonarIssueCollector;

begin
  // Degradation mode 2, the module resolved: the rule reaches the operand and
  // bails on its kind. It never asks for a byte size.
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewRecordLayout, cRecordLayoutId, 'layoutprobe.pas',
      cRecordIOSizeOfNonRecord, lc);
    AssertEquals('an array, a class and a scalar operand are skipped, the '
      + 'record control is not', 1, CountById(lc, cRecordLayoutId));
    AssertEquals('the record control is what fired', 27,
      lc.Issues[FirstById(lc, cRecordLayoutId)].StartLine);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewRecordLayout, cRecordLayoutId, 'layoutprobe.pas',
      cRecordIOUnresolved, lc);
    AssertEquals('a record type outside the closure => zero', 0,
      CountById(lc, cRecordLayoutId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.
  PackedRecordFieldAlignmentAssumptionDegradesWithoutResolver;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewRecordLayout, cRecordLayoutId, 'layoutprobe.pas',
      cRecordIOPositive, lc);
    AssertEquals('the same shape resolved => three issues', 3,
      CountById(lc, cRecordLayoutId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewRecordLayout, cRecordLayoutId, 'layoutprobe.pas',
      cLayoutNoResolution, lc);
    AssertEquals('an unresolvable closure gates the feed off', 0,
      CountById(lc, cRecordLayoutId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.
  PackedRecordFieldAlignmentAssumptionSurvivesParseFailure;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewRecordLayout, cRecordLayoutId, 'layoutbroken.pas',
      cLayoutParseFailure, lc);
    AssertTrue('the fixture really fails to parse',
      CountById(lc, cParseErrorId) > 0);
    AssertEquals('a failed parse is silent', 0,
      CountById(lc, cRecordLayoutId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.AbsoluteVariableOverlayPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewOverlay, cOverlayId, 'overlayprobe.pas',
      cOverlayMismatched, lc);
    AssertEquals('one issue per differently-sized overlay', 3,
      CountById(lc, cOverlayId));

    k := NthById(lc, cOverlayId, 0);
    AssertEquals('larger overlay start line', 8, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 8, lc.Issues[k].EndLine);
    AssertEquals('end col', 1, lc.Issues[k].EndCol);
    AssertEquals('message key', 'rule.' + cOverlayId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('message names both variables',
      'Variable lW overlays lC of a different declared size',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));

    k := NthById(lc, cOverlayId, 1);
    AssertEquals('smaller overlay start line', 10, lc.Issues[k].StartLine);
    AssertEquals('the sizes differ in either direction',
      'Variable lB overlays lI of a different declared size',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));

    k := NthById(lc, cOverlayId, 2);
    AssertEquals('local over argument start line', 13, lc.Issues[k].StartLine);
    AssertEquals('an argument is resolved the same way',
      'Variable lLong overlays aB of a different declared size',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewOverlay, cOverlayId, 'overlayprobe.pas',
      cOverlayCompliant, lc);
    AssertEquals('the compliant fixture => zero', 0, CountById(lc, cOverlayId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.AbsoluteVariableOverlaySilentShapes;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewOverlay, cOverlayId, 'overlayprobe.pas',
      cOverlayCompliant, lc);
    AssertEquals('equal sizes whatever the signedness, a record on either side '
      + 'and two pointer-sized class references => zero', 0,
      CountById(lc, cOverlayId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewOverlay, cOverlayId, 'overlayprobe.pas', cOverlayAddress, lc);
    AssertEquals('an overlay over a numeric address the resolver refuses => zero',
      0, CountById(lc, cOverlayId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewOverlay, cOverlayId, 'overlayprobe.pas', cOverlayField, lc);
    AssertEquals('an overlay over a record field carries no resolved '
      + 'declaration to name => zero', 0, CountById(lc, cOverlayId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewOverlay, cOverlayId, 'layoutprobe.pas',
      cRecordIOPositive, lc);
    AssertEquals('the byte-count shapes are not this rule''s => zero', 0,
      CountById(lc, cOverlayId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.AbsoluteVariableOverlaySilentOnUnresolvedOperand;

var
  lc: TFpSonarIssueCollector;

begin
  // Degradation mode 2 with the module resolved: the record overlays of the
  // compliant fixture are shapes the rule reaches and sizes it cannot get.
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewOverlay, cOverlayId, 'overlayprobe.pas',
      cOverlayCompliant, lc);
    AssertEquals('a side whose declared size is unknown => zero', 0,
      CountById(lc, cOverlayId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewOverlay, cOverlayId, 'overlayprobe.pas',
      cOverlayUnresolved, lc);
    AssertEquals('an overlay type outside the closure => zero', 0,
      CountById(lc, cOverlayId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.AbsoluteVariableOverlayDegradesWithoutResolver;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewOverlay, cOverlayId, 'overlayprobe.pas',
      cOverlayMismatched, lc);
    AssertEquals('the same shape resolved => three issues', 3,
      CountById(lc, cOverlayId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewOverlay, cOverlayId, 'layoutprobe.pas',
      cLayoutNoResolution, lc);
    AssertEquals('an unresolvable closure gates the feed off', 0,
      CountById(lc, cOverlayId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.AbsoluteVariableOverlaySurvivesParseFailure;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewOverlay, cOverlayId, 'layoutbroken.pas',
      cLayoutParseFailure, lc);
    AssertTrue('the fixture really fails to parse',
      CountById(lc, cParseErrorId) > 0);
    AssertEquals('a failed parse is silent', 0, CountById(lc, cOverlayId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.PointerSizedDatumTruncatedByByteCountPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewByteCountWidth, cByteCountWidthId, 'widthprobe.pas',
      cByteCountNoncompliant, lc);
    AssertEquals('one issue per truncated byte count', 2,
      CountById(lc, cByteCountWidthId));

    k := NthById(lc, cByteCountWidthId, 0);
    AssertEquals('pointer buffer start line', 25, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 25, lc.Issues[k].EndLine);
    AssertEquals('end col', 1, lc.Issues[k].EndCol);
    AssertEquals('message key', 'rule.' + cByteCountWidthId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('message names the fixed-width type',
      'Size of fixed-width type Integer is used as the byte count of a '
      + 'pointer-sized datum',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));

    k := NthById(lc, cByteCountWidthId, 1);
    AssertEquals('pointer-sized integer buffer start line', 26,
      lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 26, lc.Issues[k].EndLine);
    AssertEquals('end col', 1, lc.Issues[k].EndCol);
    AssertEquals('message key', 'rule.' + cByteCountWidthId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('the count type is named as written',
      'Size of fixed-width type Cardinal is used as the byte count of a '
      + 'pointer-sized datum',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewByteCountWidth, cByteCountWidthId, 'widthprobe.pas',
      cByteCountResultArg, lc);
    AssertEquals('the four-argument BlockWrite keeps its count at index 2', 1,
      CountById(lc, cByteCountWidthId));
    AssertEquals('the count argument line', 16,
      lc.Issues[FirstById(lc, cByteCountWidthId)].StartLine);
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewByteCountWidth, cByteCountWidthId, 'widthprobe.pas',
      cByteCountCompliant, lc);
    AssertEquals('the compliant fixture => zero', 0,
      CountById(lc, cByteCountWidthId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.PointerSizedDatumTruncatedByByteCountSilentShapes;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewByteCountWidth, cByteCountWidthId, 'widthprobe.pas',
      cByteCountCompliant, lc);
    AssertEquals('a pointer-sized or wide count, a fixed-width or wide datum, '
      + 'and a SizeOf outside the count position => zero', 0,
      CountById(lc, cByteCountWidthId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;

  // Liveness control for the zeros above: the same declarations and stubs in
  // the noncompliant fixture do fire.
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewByteCountWidth, cByteCountWidthId, 'widthprobe.pas',
      cByteCountNoncompliant, lc);
    AssertTrue('the silent shapes are silent by predicate, not by a dark '
      + 'resolver', CountById(lc, cByteCountWidthId) > 0);
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewByteCountWidth, cByteCountWidthId, 'widthprobe.pas',
      cByteCountOffsetOverload, lc);
    AssertEquals('a three-argument overload counts an Offset first => zero', 0,
      CountById(lc, cByteCountWidthId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewByteCountWidth, cByteCountWidthId, 'widthprobe.pas',
      cByteCountPointerArith, lc);
    AssertEquals('pointer arithmetic draws no finding from this story, and the '
      + 'width control on the same resolved module does', 1,
      CountById(lc, cByteCountWidthId));
    AssertEquals('the width control is what fired', 20,
      lc.Issues[FirstById(lc, cByteCountWidthId)].StartLine);
    AssertNull(cPointerArithId + ' was withdrawn',
      RuleRegistry.FindById(cPointerArithId));
  finally
    lc.Free;
  end;

  // Disjoint from the cast rules: the noncompliant fixture carries no cast, and
  // the compliant one carries the two casts they own and this rule ignores.
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(TRulePlatformDependentCast.Create(
      RuleRegistry.FindById(cCastId).Metadata), cCastId, 'widthprobe.pas',
      cByteCountNoncompliant, lc);
    AssertEquals('no cast in the byte-count shape', 0, CountById(lc, cCastId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(TRulePlatformDependentTruncation.Create(
      RuleRegistry.FindById(cTruncationId).Metadata), cTruncationId,
      'widthprobe.pas', cByteCountNoncompliant, lc);
    AssertEquals('no narrowing cast in the byte-count shape', 0,
      CountById(lc, cTruncationId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(TRulePlatformDependentCast.Create(
      RuleRegistry.FindById(cCastId).Metadata), cCastId, 'widthprobe.pas',
      cByteCountCompliant, lc);
    AssertEquals('the pointer cast the compliant fixture carries', 1,
      CountById(lc, cCastId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(TRulePlatformDependentTruncation.Create(
      RuleRegistry.FindById(cTruncationId).Metadata), cTruncationId,
      'widthprobe.pas', cByteCountCompliant, lc);
    AssertEquals('the narrowing cast the compliant fixture carries', 1,
      CountById(lc, cTruncationId));
  finally
    lc.Free;
  end;

  // Disjoint from the record rule: each owns one call of the same fixture.
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewByteCountWidth, cByteCountWidthId, 'widthprobe.pas',
      cByteCountSharedCall, lc);
    AssertEquals('a record count is neither fixed-width nor pointer-sized', 1,
      CountById(lc, cByteCountWidthId));
    AssertEquals('only the pointer call fired', 22,
      lc.Issues[FirstById(lc, cByteCountWidthId)].StartLine);
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewRecordLayout, cRecordLayoutId, 'widthprobe.pas',
      cByteCountSharedCall, lc);
    AssertEquals('the record rule keeps its own call and takes no other', 1,
      CountById(lc, cRecordLayoutId));
    AssertEquals('only the record call fired', 21,
      lc.Issues[FirstById(lc, cRecordLayoutId)].StartLine);
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.
  PointerSizedDatumTruncatedByByteCountSilentOnUnresolvedOperand;

var
  lc: TFpSonarIssueCollector;

begin
  // Degradation mode 2: the untyped var buffer has no resolved type, and the
  // finding on line 19 of the same module is what says the rest of it resolved.
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewByteCountWidth, cByteCountWidthId, 'widthprobe.pas',
      cByteCountUntypedBuffer, lc);
    AssertEquals('an untyped var buffer draws nothing, its resolved neighbour '
      + 'draws one', 1, CountById(lc, cByteCountWidthId));
    AssertEquals('the resolved neighbour is what fired', 19,
      lc.Issues[FirstById(lc, cByteCountWidthId)].StartLine);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    AssertEquals('the fixture parses', 0, CountById(lc, cParseErrorId));
  finally
    lc.Free;
  end;

  // A SizeOf over an undeclared type takes the whole module's resolution with
  // it.
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewByteCountWidth, cByteCountWidthId, 'widthprobe.pas',
      cByteCountUnresolved, lc);
    AssertEquals('a count type outside the closure => zero', 0,
      CountById(lc, cByteCountWidthId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.
  PointerSizedDatumTruncatedByByteCountDegradesWithoutResolver;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewByteCountWidth, cByteCountWidthId, 'widthprobe.pas',
      cByteCountNoncompliant, lc);
    AssertEquals('the same shape resolved => two issues', 2,
      CountById(lc, cByteCountWidthId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewByteCountWidth, cByteCountWidthId, 'widthprobe.pas',
      cWidthNoResolution, lc);
    AssertEquals('an unresolvable closure gates the feed off', 0,
      CountById(lc, cByteCountWidthId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.
  PointerSizedDatumTruncatedByByteCountSurvivesParseFailure;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewByteCountWidth, cByteCountWidthId, 'widthbroken.pas',
      cWidthParseFailure, lc);
    AssertTrue('the fixture really fails to parse',
      CountById(lc, cParseErrorId) > 0);
    AssertEquals('a failed parse is silent', 0,
      CountById(lc, cByteCountWidthId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.UnknownConditionalSymbolPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnknownSymbol, cUnknownSymbolId, 'defineprobe.pas',
      cDefineTypo, lc);
    AssertEquals('one unknown-symbol issue', 1,
      CountById(lc, cUnknownSymbolId));
    k := FirstById(lc, cUnknownSymbolId);
    AssertEquals('start line', 8, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 8, lc.Issues[k].EndLine);
    AssertEquals('end col', 16, lc.Issues[k].EndCol);
    AssertEquals('message key', 'rule.' + cUnknownSymbolId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('message names the guard symbol',
      'Conditional symbol WNIDOWS is neither defined for this analysis nor a '
      + 'known FPC or target symbol',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  // The three shapes the branch rule declines and this one still owns: an empty
  // dead branch, an {$elseif} chain and an unclosed conditional.
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnknownSymbol, cUnknownSymbolId, 'defineprobe.pas',
      cDefineEmptyDead, lc);
    AssertEquals('a typo in an empty branch is still a typo', 1,
      CountById(lc, cUnknownSymbolId));
    AssertEquals('at the opener', 8,
      lc.Issues[FirstById(lc, cUnknownSymbolId)].StartLine);
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnknownSymbol, cUnknownSymbolId, 'defineprobe.pas',
      cDefineElseIfChain, lc);
    AssertEquals('an {$elseif} chain does not hide the symbol', 1,
      CountById(lc, cUnknownSymbolId));
    AssertEquals('at the opener', 8,
      lc.Issues[FirstById(lc, cUnknownSymbolId)].StartLine);
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnknownSymbol, cUnknownSymbolId, 'defineprobe.pas',
      cDefineUnclosed, lc);
    AssertEquals('an unclosed conditional does not hide the symbol', 1,
      CountById(lc, cUnknownSymbolId));
    AssertEquals('at the opener', 6,
      lc.Issues[FirstById(lc, cUnknownSymbolId)].StartLine);
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnknownSymbol, cUnknownSymbolId, 'defineprobe.pas',
      cDefineNegatedAbsent, lc);
    AssertEquals('a negated guard on an absent symbol still names it', 1,
      CountById(lc, cUnknownSymbolId));
    AssertEquals('end col of the {$ifndef} opener', 17,
      lc.Issues[FirstById(lc, cUnknownSymbolId)].EndCol);
    AssertEquals('message names the guard symbol',
      'Conditional symbol WNIDOWS is neither defined for this analysis nor a '
      + 'known FPC or target symbol',
      FormatMessage(lc.Issues[FirstById(lc, cUnknownSymbolId)].MessageKey,
      lc.Issues[FirstById(lc, cUnknownSymbolId)].MessageArgs));
  finally
    lc.Free;
  end;
end;


// PositionsPerMode is n/a for UnknownConditionalSymbol: no part of its verdict
// reads aContext.CompilerMode or any {$mode}-derived fact.
procedure TRulesCondCompTest.UnknownConditionalSymbolSilentShapes;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnknownSymbol, cUnknownSymbolId, 'defineprobe.pas',
      cDefineDefinedForRun, lc);
    AssertEquals('a symbol the run defines => zero', 0,
      CountById(lc, cUnknownSymbolId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnknownSymbol, cUnknownSymbolId, 'defineprobe.pas',
      cDefineOwnSymbol, lc);
    AssertEquals('a symbol the file itself defines => zero', 0,
      CountById(lc, cUnknownSymbolId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnknownSymbol, cUnknownSymbolId, 'defineprobe.pas',
      cDefineCaseFolded, lc);
    AssertEquals('a defined symbol spelled in another case => zero', 0,
      CountById(lc, cUnknownSymbolId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnknownSymbol, cUnknownSymbolId, 'defineprobe.pas',
      cDefineMacroSymbol, lc);
    AssertEquals('a symbol the file defines as a macro => zero', 0,
      CountById(lc, cUnknownSymbolId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnknownSymbol, cUnknownSymbolId, 'defineprobe.pas',
      cDefineNegatedDefined, lc);
    AssertEquals('a negated guard on a defined symbol => zero', 0,
      CountById(lc, cUnknownSymbolId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnknownSymbol, cUnknownSymbolId, 'defineprobe.pas',
      cDefineSymbolLess, lc);
    AssertEquals('{$if} and {$ifopt} carry no symbol => zero', 0,
      CountById(lc, cUnknownSymbolId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnknownSymbol, cUnknownSymbolId, 'defineprobe.pas',
      cDefineCuratedGlob, lc);
    AssertEquals('a symbol the curated CPU family matches => zero', 0,
      CountById(lc, cUnknownSymbolId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnknownSymbol, cUnknownSymbolId, 'defineprobe.pas',
      cDefineInComment, lc);
    AssertEquals('a directive in a comment or a string literal => zero', 0,
      CountById(lc, cUnknownSymbolId));
  finally
    lc.Free;
  end;

  // Liveness control for the zeros above: the same runner over the typo shape
  // fires.
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnknownSymbol, cUnknownSymbolId, 'defineprobe.pas',
      cDefineTypo, lc);
    AssertEquals('the silent shapes are silent by predicate', 1,
      CountById(lc, cUnknownSymbolId));
  finally
    lc.Free;
  end;
end;


// SilentOnUnresolvedOperand is n/a for UnknownConditionalSymbol: it is
// rtTok/rfLineText and consults no resolver fact.
procedure TRulesCondCompTest.UnknownConditionalSymbolDegradesWithoutResolver;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnknownSymbol, cUnknownSymbolId, 'defineprobe.pas',
      cDefineScanFailure, lc);
    AssertTrue('the fixture really fails to scan',
      CountById(lc, cScanErrorId) > 0);
    AssertEquals('no captured define set is silence', 0,
      CountById(lc, cUnknownSymbolId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.UnknownConditionalSymbolSurvivesParseFailure;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewUnknownSymbol, cUnknownSymbolId, 'definebroken.pas',
      cDefineParseFailure, lc);
    AssertTrue('the fixture really fails to parse',
      CountById(lc, cParseErrorId) > 0);
    AssertEquals('the scan, hence the define set, survives the failed parse', 1,
      CountById(lc, cUnknownSymbolId));
    AssertEquals('at the opener', 4,
      lc.Issues[FirstById(lc, cUnknownSymbolId)].StartLine);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.ConditionalBranchNeverCompiledPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNeverCompiled, cNeverCompiledId, 'defineprobe.pas',
      cDefineTypo, lc);
    AssertEquals('one never-compiled issue', 1,
      CountById(lc, cNeverCompiledId));
    k := FirstById(lc, cNeverCompiledId);
    AssertEquals('start line', 8, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 8, lc.Issues[k].EndLine);
    AssertEquals('end col', 16, lc.Issues[k].EndCol);
    AssertEquals('message key', 'rule.' + cNeverCompiledId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('message names the guard symbol',
      'Branch guarded by WNIDOWS is never compiled under the configured defines',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNeverCompiled, cNeverCompiledId, 'defineprobe.pas',
      cDefineNegatedDefined, lc);
    AssertEquals('a negated guard on a defined symbol kills the then branch', 1,
      CountById(lc, cNeverCompiledId));
    k := FirstById(lc, cNeverCompiledId);
    AssertEquals('start line', 8, lc.Issues[k].StartLine);
    AssertEquals('end col of the {$ifndef} opener', 15, lc.Issues[k].EndCol);
    AssertEquals('message names the guard symbol',
      'Branch guarded by LINUX is never compiled under the configured defines',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNeverCompiled, cNeverCompiledId, 'defineprobe.pas',
      cDefineKnownOther, lc);
    AssertEquals('a known other-platform symbol is honest dead code', 1,
      CountById(lc, cNeverCompiledId));
    AssertEquals('message names the guard symbol',
      'Branch guarded by WINDOWS is never compiled under the configured defines',
      FormatMessage(lc.Issues[FirstById(lc, cNeverCompiledId)].MessageKey,
      lc.Issues[FirstById(lc, cNeverCompiledId)].MessageArgs));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNeverCompiled, cNeverCompiledId, 'defineprobe.pas',
      cDefineCuratedGlob, lc);
    AssertEquals('a curated symbol is still absent from the define set', 1,
      CountById(lc, cNeverCompiledId));
    AssertEquals('at the opener', 8,
      lc.Issues[FirstById(lc, cNeverCompiledId)].StartLine);
  finally
    lc.Free;
  end;
end;


// PositionsPerMode is n/a for ConditionalBranchNeverCompiled: no part of its
// verdict reads aContext.CompilerMode or any {$mode}-derived fact.
procedure TRulesCondCompTest.ConditionalBranchNeverCompiledSilentShapes;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNeverCompiled, cNeverCompiledId, 'defineprobe.pas',
      cDefineDefinedForRun, lc);
    AssertEquals('a defined symbol compiles its then branch => zero', 0,
      CountById(lc, cNeverCompiledId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNeverCompiled, cNeverCompiledId, 'defineprobe.pas',
      cDefineOwnSymbol, lc);
    AssertEquals('a symbol the file itself defines => zero', 0,
      CountById(lc, cNeverCompiledId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNeverCompiled, cNeverCompiledId, 'defineprobe.pas',
      cDefineNegatedAbsent, lc);
    AssertEquals('a negated guard on an absent symbol compiles => zero', 0,
      CountById(lc, cNeverCompiledId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNeverCompiled, cNeverCompiledId, 'defineprobe.pas',
      cDefineCaseFolded, lc);
    AssertEquals('a defined symbol spelled in another case compiles => zero', 0,
      CountById(lc, cNeverCompiledId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNeverCompiled, cNeverCompiledId, 'defineprobe.pas',
      cDefineEmptyDead, lc);
    AssertEquals('an empty dead branch belongs to EmptyConditionalBranch => '
      + 'zero', 0, CountById(lc, cNeverCompiledId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNeverCompiled, cNeverCompiledId, 'defineprobe.pas',
      cDefineElseIfChain, lc);
    AssertEquals('an {$elseif} chain is opaque => zero', 0,
      CountById(lc, cNeverCompiledId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNeverCompiled, cNeverCompiledId, 'defineprobe.pas',
      cDefineUnclosed, lc);
    AssertEquals('an unclosed conditional => zero', 0,
      CountById(lc, cNeverCompiledId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNeverCompiled, cNeverCompiledId, 'defineprobe.pas',
      cDefineSymbolLess, lc);
    AssertEquals('{$if} and {$ifopt} carry no symbol => zero', 0,
      CountById(lc, cNeverCompiledId));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNeverCompiled, cNeverCompiledId, 'defineprobe.pas',
      cDefineInComment, lc);
    AssertEquals('a directive in a comment or a string literal => zero', 0,
      CountById(lc, cNeverCompiledId));
  finally
    lc.Free;
  end;

  // Liveness control for the zeros above.
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNeverCompiled, cNeverCompiledId, 'defineprobe.pas',
      cDefineTypo, lc);
    AssertEquals('the silent shapes are silent by predicate', 1,
      CountById(lc, cNeverCompiledId));
  finally
    lc.Free;
  end;
end;


// SilentOnUnresolvedOperand is n/a for ConditionalBranchNeverCompiled: it is
// rtTok/rfLineText and consults no resolver fact.
procedure TRulesCondCompTest.ConditionalBranchNeverCompiledDegradesWithoutResolver;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNeverCompiled, cNeverCompiledId, 'defineprobe.pas',
      cDefineScanFailure, lc);
    AssertTrue('the fixture really fails to scan',
      CountById(lc, cScanErrorId) > 0);
    AssertEquals('no captured define set is silence', 0,
      CountById(lc, cNeverCompiledId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.ConditionalBranchNeverCompiledSurvivesParseFailure;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneSrc(NewNeverCompiled, cNeverCompiledId, 'definebroken.pas',
      cDefineParseFailure, lc);
    AssertTrue('the fixture really fails to parse',
      CountById(lc, cParseErrorId) > 0);
    AssertEquals('the scan, hence the define set, survives the failed parse', 1,
      CountById(lc, cNeverCompiledId));
    AssertEquals('at the opener', 4,
      lc.Issues[FirstById(lc, cNeverCompiledId)].StartLine);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.TypoFiresBothRules;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRulesSrc([NewUnknownSymbol, NewNeverCompiled],
      [cUnknownSymbolId, cNeverCompiledId], 'defineprobe.pas', cDefineTypo, lc);
    AssertEquals('the typo is an unknown symbol', 1,
      CountById(lc, cUnknownSymbolId));
    AssertEquals('and the branch it guards is dead', 1,
      CountById(lc, cNeverCompiledId));
    AssertEquals('both at the same opener row',
      lc.Issues[FirstById(lc, cUnknownSymbolId)].StartLine,
      lc.Issues[FirstById(lc, cNeverCompiledId)].StartLine);
    AssertEquals('both at the same opener column',
      lc.Issues[FirstById(lc, cUnknownSymbolId)].StartCol,
      lc.Issues[FirstById(lc, cNeverCompiledId)].StartCol);
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.KnownSymbolSeparatesTheTwoRules;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRulesSrc([NewUnknownSymbol, NewNeverCompiled],
      [cUnknownSymbolId, cNeverCompiledId], 'defineprobe.pas',
      cDefineKnownOther, lc);
    AssertEquals('the curated list is the whole difference between the two', 0,
      CountById(lc, cUnknownSymbolId));
    AssertEquals('the branch is still dead code', 1,
      CountById(lc, cNeverCompiledId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.EmptyParamMakesEverySymbolUnknown;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneParamSrc(NewUnknownSymbol, cUnknownSymbolId, cKnownSymbolsParam,
      '', 'defineprobe.pas', cDefineKnownOther, lc);
    AssertEquals('an empty curated list makes every absent symbol unknown', 1,
      CountById(lc, cUnknownSymbolId));
    AssertEquals('message names the guard symbol',
      'Conditional symbol WINDOWS is neither defined for this analysis nor a '
      + 'known FPC or target symbol',
      FormatMessage(lc.Issues[FirstById(lc, cUnknownSymbolId)].MessageKey,
      lc.Issues[FirstById(lc, cUnknownSymbolId)].MessageArgs));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunAloneParamSrc(NewUnknownSymbol, cUnknownSymbolId, cKnownSymbolsParam,
      '', 'defineprobe.pas', cDefineCuratedGlob, lc);
    AssertEquals('the glob that silenced the CPU family is gone too', 1,
      CountById(lc, cUnknownSymbolId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.DeadBranchIsDisjointFromEmptyBranch;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRulesSrc([NewEmptyBranch, NewNeverCompiled, NewUnknownSymbol],
      [cEmptyId, cNeverCompiledId, cUnknownSymbolId], 'defineprobe.pas',
      cDefineEmptyAndPopulatedDead, lc);
    AssertEquals('EmptyConditionalBranch takes the empty one only', 1,
      CountById(lc, cEmptyId));
    AssertEquals('and it is the one on line 8', 8,
      lc.Issues[FirstById(lc, cEmptyId)].StartLine);
    AssertEquals('ConditionalBranchNeverCompiled takes the populated one only',
      1, CountById(lc, cNeverCompiledId));
    AssertEquals('and it is the one on line 10', 10,
      lc.Issues[FirstById(lc, cNeverCompiledId)].StartLine);
    AssertEquals('the symbol rule owns neither shape and takes both', 2,
      CountById(lc, cUnknownSymbolId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesCondCompTest.CondCompRulesSelfRegisterGlobally;

var
  lRule: TRuleBase;
  lSpec: TRuleParamSpec;

begin
  lRule := RuleRegistry.FindById(cEmptyId);
  AssertTrue(cEmptyId + ' registered', lRule <> nil);
  AssertFalse(cEmptyId + ' ships disabled', lRule.Metadata.DefaultEnabled);
  AssertTrue(cEmptyId + ' carries a description',
    lRule.Metadata.Description <> '');
  AssertEquals('tier rtTok', Ord(rtTok), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfLineText', Ord(rfLineText), Ord(lRule.Metadata.Feed));
  AssertEquals('confidence cfMedium', Ord(cfMedium),
    Ord(lRule.Metadata.DefaultConfidence));
  AssertEquals('severity sevMinor', Ord(sevMinor), Ord(lRule.Metadata.Severity));
  AssertEquals('category itCodeSmell', Ord(itCodeSmell),
    Ord(lRule.Metadata.Category));

  lRule := RuleRegistry.FindById(cNegatedId);
  AssertTrue(cNegatedId + ' registered', lRule <> nil);
  AssertFalse(cNegatedId + ' ships disabled', lRule.Metadata.DefaultEnabled);
  AssertTrue(cNegatedId + ' carries a description',
    lRule.Metadata.Description <> '');
  AssertEquals('tier rtTok', Ord(rtTok), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfLineText', Ord(rfLineText), Ord(lRule.Metadata.Feed));
  AssertEquals('confidence cfMedium', Ord(cfMedium),
    Ord(lRule.Metadata.DefaultConfidence));
  AssertEquals('severity sevMinor', Ord(sevMinor), Ord(lRule.Metadata.Severity));
  AssertEquals('category itCodeSmell', Ord(itCodeSmell),
    Ord(lRule.Metadata.Category));

  lRule := RuleRegistry.FindById(cSeparatorId);
  AssertTrue(cSeparatorId + ' registered', lRule <> nil);
  AssertFalse(cSeparatorId + ' ships disabled', lRule.Metadata.DefaultEnabled);
  AssertTrue(cSeparatorId + ' carries a description',
    lRule.Metadata.Description <> '');
  AssertEquals('tier rtTok', Ord(rtTok), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfTokenStream', Ord(rfTokenStream),
    Ord(lRule.Metadata.Feed));
  AssertEquals('confidence cfMedium', Ord(cfMedium),
    Ord(lRule.Metadata.DefaultConfidence));
  AssertEquals('severity sevMinor', Ord(sevMinor), Ord(lRule.Metadata.Severity));
  AssertEquals('category itCodeSmell', Ord(itCodeSmell),
    Ord(lRule.Metadata.Category));

  lRule := RuleRegistry.FindById(cLineEndId);
  AssertTrue(cLineEndId + ' registered', lRule <> nil);
  AssertFalse(cLineEndId + ' ships disabled', lRule.Metadata.DefaultEnabled);
  AssertTrue(cLineEndId + ' carries a description',
    lRule.Metadata.Description <> '');
  AssertEquals('tier rtTok', Ord(rtTok), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfTokenStream', Ord(rfTokenStream),
    Ord(lRule.Metadata.Feed));
  AssertEquals('confidence cfMedium', Ord(cfMedium),
    Ord(lRule.Metadata.DefaultConfidence));
  AssertEquals('severity sevMinor', Ord(sevMinor), Ord(lRule.Metadata.Severity));
  AssertEquals('category itCodeSmell', Ord(itCodeSmell),
    Ord(lRule.Metadata.Category));

  lRule := RuleRegistry.FindById(cRecordLayoutId);
  AssertTrue(cRecordLayoutId + ' registered', lRule <> nil);
  AssertFalse(cRecordLayoutId + ' ships disabled',
    lRule.Metadata.DefaultEnabled);
  AssertTrue(cRecordLayoutId + ' carries a description',
    lRule.Metadata.Description <> '');
  AssertEquals('tier rtSem', Ord(rtSem), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfResolver', Ord(rfResolver), Ord(lRule.Metadata.Feed));
  AssertEquals('confidence cfMedium', Ord(cfMedium),
    Ord(lRule.Metadata.DefaultConfidence));
  AssertEquals('severity sevMajor', Ord(sevMajor), Ord(lRule.Metadata.Severity));
  AssertEquals('category itBug', Ord(itBug), Ord(lRule.Metadata.Category));

  lRule := RuleRegistry.FindById(cOverlayId);
  AssertTrue(cOverlayId + ' registered', lRule <> nil);
  AssertFalse(cOverlayId + ' ships disabled', lRule.Metadata.DefaultEnabled);
  AssertTrue(cOverlayId + ' carries a description',
    lRule.Metadata.Description <> '');
  AssertEquals('tier rtSem', Ord(rtSem), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfResolver', Ord(rfResolver), Ord(lRule.Metadata.Feed));
  AssertEquals('confidence cfMedium', Ord(cfMedium),
    Ord(lRule.Metadata.DefaultConfidence));
  AssertEquals('severity sevMajor', Ord(sevMajor), Ord(lRule.Metadata.Severity));
  AssertEquals('category itBug', Ord(itBug), Ord(lRule.Metadata.Category));

  lRule := RuleRegistry.FindById(cByteCountWidthId);
  AssertTrue(cByteCountWidthId + ' registered', lRule <> nil);
  AssertFalse(cByteCountWidthId + ' ships disabled',
    lRule.Metadata.DefaultEnabled);
  AssertTrue(cByteCountWidthId + ' carries a description',
    lRule.Metadata.Description <> '');
  AssertEquals('tier rtSem', Ord(rtSem), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfResolver', Ord(rfResolver), Ord(lRule.Metadata.Feed));
  AssertEquals('confidence cfMedium', Ord(cfMedium),
    Ord(lRule.Metadata.DefaultConfidence));
  AssertEquals('severity sevMajor', Ord(sevMajor), Ord(lRule.Metadata.Severity));
  AssertEquals('category itBug', Ord(itBug), Ord(lRule.Metadata.Category));

  lRule := RuleRegistry.FindById(cUnknownSymbolId);
  AssertTrue(cUnknownSymbolId + ' registered', lRule <> nil);
  AssertFalse(cUnknownSymbolId + ' ships disabled',
    lRule.Metadata.DefaultEnabled);
  AssertTrue(cUnknownSymbolId + ' names the inverted silence contract',
    Pos('inverting the silence contract', lRule.Metadata.Description) > 0);
  AssertEquals('tier rtTok', Ord(rtTok), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfLineText', Ord(rfLineText), Ord(lRule.Metadata.Feed));
  AssertEquals('confidence cfLow', Ord(cfLow),
    Ord(lRule.Metadata.DefaultConfidence));
  AssertEquals('severity sevMajor', Ord(sevMajor), Ord(lRule.Metadata.Severity));
  AssertEquals('category itBug', Ord(itBug), Ord(lRule.Metadata.Category));
  AssertEquals('exactly one declared param', 1,
    Length(lRule.Metadata.ParamSpecs));
  AssertTrue('the declared param is ' + cKnownSymbolsParam,
    lRule.Metadata.FindParam(cKnownSymbolsParam, lSpec));
  AssertEquals('kind rpkString', Ord(rpkString), Ord(lSpec.Kind));
  AssertTrue('its default is non-empty', lSpec.DefaultValue <> '');

  lRule := RuleRegistry.FindById(cNeverCompiledId);
  AssertTrue(cNeverCompiledId + ' registered', lRule <> nil);
  AssertFalse(cNeverCompiledId + ' ships disabled',
    lRule.Metadata.DefaultEnabled);
  AssertTrue(cNeverCompiledId + ' names the inverted silence contract',
    Pos('inverting the silence contract', lRule.Metadata.Description) > 0);
  AssertEquals('tier rtTok', Ord(rtTok), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfLineText', Ord(rfLineText), Ord(lRule.Metadata.Feed));
  AssertEquals('confidence cfLow', Ord(cfLow),
    Ord(lRule.Metadata.DefaultConfidence));
  AssertEquals('severity sevMinor', Ord(sevMinor), Ord(lRule.Metadata.Severity));
  AssertEquals('category itCodeSmell', Ord(itCodeSmell),
    Ord(lRule.Metadata.Category));
  AssertEquals('no declared param', 0, Length(lRule.Metadata.ParamSpecs));

  AssertNull(cPointerArithId + ' is withdrawn, and its absence is the '
    + 'acceptance condition', RuleRegistry.FindById(cPointerArithId));
end;


initialization
  RegisterTest(TRulesCondCompTest);

end.
