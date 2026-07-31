{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the object-lifetime (resolver) rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesLifetime;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Config, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.SourceFile, FpSonar.Rules.Lifetime, UtstFixtures;

type
  { Resolver-tier lifetime-rule position + registration tests. }
  TRulesLifetimeTest = class(TTestCase)
  private
    // Runs aRule over aFixture, collecting issues into aCollector.
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      const aCollector: TFpSonarIssueCollector); overload;
    // aWithhold runs the degraded pass: real-RTL chain, no unit paths.
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      aWithhold: boolean; const aCollector: TFpSonarIssueCollector); overload;
    // Runs aRule over aFixture with aConfig in force.
    procedure RunRuleWithConfig(aRule: TRuleBase; const aFixture: string;
      const aConfig: TFpSonarConfig;
      const aCollector: TFpSonarIssueCollector);
    // A config giving UnbalancedPair the acquire/release table aPatterns.
    function PairTargetsConfig(
      const aPatterns: array of string): TFpSonarConfig;
    // How often UnbalancedPair fires on aSource under the table aPatterns,
    // aRow taking the row of its first issue.
    function PairCountWith(const aPatterns: array of string;
      const aSource: array of string; out aRow: Integer): Integer;
    // How often aRule fires on aSource, staged as its own fixture.
    function RuleCount(aRule: TRuleBase; aWithhold: boolean;
      const aSource: array of string): Integer;
    // Asserts aRule is silent on aSource while aSibling fires once on it.
    procedure CheckSilentWithLiveSibling(aRule, aSibling: TRuleBase;
      const aId, aSiblingId: string; const aSource: array of string);
    // Asserts all five free-state rules are silent on aSource.
    procedure CheckFreeStateSilent(const aSource: array of string);
    // Asserts aPath parses and resolves. The engine discards the resolver's
    // diagnostic.
    procedure AssertFixtureResolves(const aPath: string);
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // Asserts aRule fires once at aDeclLine, column 1, with key
    // rule.<aId>.message and message args = aArgs; and zero on the compliant
    // fixture. Fixtures supplied inline and materialised to a temp dir.
    procedure CheckStmtRuleSrc(aRule, aCompliantRule: TRuleBase;
      const aId: string; aDeclLine: Integer; const aArgs: array of string;
      const aNoncompliant, aCompliant: array of string);
    // Fresh, separately-owned instances of each rule. DefaultEnabled is True in
    // every factory.
    function NewFreeOnInterfaceReference: TRuleBase;
    function NewSelfDestroyedInMethod: TRuleBase;
    function NewNewDisposeMismatch: TRuleBase;
    function NewOwnedFieldNotFreedInDestructor: TRuleBase;
    function NewCreateWithoutTryFinally: TRuleBase;
    function NewExceptionObjectFreedInHandler: TRuleBase;
    function NewRaisedExceptionInstanceReused: TRuleBase;
    function NewLoopVariableUsedAfterLoop: TRuleBase;
    function NewLoopVariableModifiedInBody: TRuleBase;
    function NewLeakOnEarlyExit: TRuleBase;
    function NewStreamNotProtected: TRuleBase;
    function NewUseAfterFree: TRuleBase;
    function NewDoubleFree: TRuleBase;
    function NewFreeNotFreeAndNilOnField: TRuleBase;
    function NewGetMemWithoutFreeMem: TRuleBase;
    function NewObjectCreatedInLoopNotFreed: TRuleBase;
    function NewUnbalancedPair: TRuleBase;
  published
    procedure FreeOnInterfaceReferencePositions;
    procedure FreeOnInterfaceReferenceCountsCastForm;
    procedure FreeOnInterfaceReferenceDegradesWithoutResolver;
    procedure FreeOnInterfaceReferenceSilentOnUnresolvedOperand;
    procedure SelfDestroyedInMethodPositions;
    procedure SelfDestroyedInMethodCountsBareFreeForm;
    procedure SelfDestroyedInMethodDegradesWithoutResolver;
    procedure SelfDestroyedInMethodSilentOnUnresolvedOperand;
    procedure NewDisposeMismatchPositions;
    procedure NewDisposeMismatchCountsGetMemDisposePair;
    procedure NewDisposeMismatchDegradesWithoutResolver;
    procedure NewDisposeMismatchSilentOnUnresolvedOperand;
    procedure OwnedFieldNotFreedInDestructorPositions;
    procedure OwnedFieldNotFreedInDestructorSilentOnHelperRelease;
    procedure OwnedFieldNotFreedInDestructorSilentOnBeforeDestruction;
    procedure OwnedFieldNotFreedInDestructorSilentWithoutOwnDestructor;
    procedure OwnedFieldNotFreedInDestructorDegradesWithoutResolver;
    procedure OwnedFieldNotFreedInDestructorSilentOnUnresolvedOperand;
    procedure CreateWithoutTryFinallyPositions;
    procedure CreateWithoutTryFinallyCountsFreeAndNilForm;
    procedure CreateWithoutTryFinallyCountsBranchRelease;
    procedure CreateWithoutTryFinallyCountsBranchHandover;
    procedure CreateWithoutTryFinallyCountsReacquisitionAfterRelease;
    procedure CreateWithoutTryFinallySilentOnReleaseBeforeAcquisition;
    procedure CreateWithoutTryFinallySilentOnNestedCoveringFinally;
    procedure CreateWithoutTryFinallySilentOnStreamRow;
    procedure CreateWithoutTryFinallySilentForExitAndStreamRules;
    procedure CreateWithoutTryFinallyDegradesWithoutResolver;
    procedure CreateWithoutTryFinallySilentOnUnresolvedOperand;
    procedure LeakOnEarlyExitPositions;
    procedure LeakOnEarlyExitCountsRaiseForm;
    procedure LeakOnEarlyExitCountsUnprotectedCreateOnSameFixture;
    procedure LeakOnEarlyExitSilentOnHandledRaise;
    procedure LeakOnEarlyExitSilentUnderFinally;
    procedure LeakOnEarlyExitSilentOnUnreachableAcquisition;
    procedure LeakOnEarlyExitDegradesWithoutResolver;
    procedure LeakOnEarlyExitSilentOnUnresolvedOperand;
    procedure StreamNotProtectedPositions;
    procedure StreamNotProtectedSilentOnNeverReleasedStream;
    procedure StreamNotProtectedSilentOnFinallySharedByBranches;
    procedure StreamNotProtectedDegradesWithoutResolver;
    procedure StreamNotProtectedSilentOnUnresolvedOperand;
    procedure ExceptionObjectFreedInHandlerPositions;
    procedure ExceptionObjectFreedInHandlerCountsFreeAndNilForm;
    procedure ExceptionObjectFreedInHandlerDegradesWithoutResolver;
    procedure ExceptionObjectFreedInHandlerSilentOnUnresolvedOperand;
    procedure RaisedExceptionInstanceReusedPositions;
    procedure RaisedExceptionInstanceReusedCountsParameterForm;
    procedure RaisedExceptionInstanceReusedSilentOnClearedHandle;
    procedure RaisedExceptionInstanceReusedDegradesWithoutResolver;
    procedure RaisedExceptionInstanceReusedSilentOnUnresolvedOperand;
    procedure LoopVariableUsedAfterLoopPositions;
    procedure LoopVariableUsedAfterLoopSilentOnAssignmentAfterLoop;
    procedure LoopVariableUsedAfterLoopSilentOnCounterReuse;
    procedure LoopVariableUsedAfterLoopCountsNearestLoopOnly;
    procedure LoopVariableUsedAfterLoopDegradesWithoutResolver;
    procedure LoopVariableUsedAfterLoopSilentOnUnresolvedOperand;
    procedure LoopVariableModifiedInBodyPositions;
    procedure LoopVariableModifiedInBodyCountsNestedLoopForm;
    procedure LoopVariableModifiedInBodyDegradesOnParseFailure;
    procedure UseAfterFreePositions;
    procedure UseAfterFreeSilentOnFieldRow;
    procedure UseAfterFreeDegradesWithoutResolver;
    procedure UseAfterFreeSilentOnUnresolvedOperand;
    procedure DoubleFreePositions;
    procedure DoubleFreeDescribesAPathNotEveryPath;
    procedure DoubleFreeDegradesWithoutResolver;
    procedure DoubleFreeSilentOnUnresolvedOperand;
    procedure FreeNotFreeAndNilOnFieldPositions;
    procedure FreeNotFreeAndNilOnFieldSilentOnNilledField;
    procedure FreeNotFreeAndNilOnFieldDegradesWithoutResolver;
    procedure FreeNotFreeAndNilOnFieldSilentOnUnresolvedOperand;
    procedure GetMemWithoutFreeMemPositions;
    procedure GetMemWithoutFreeMemCountsNilledPointer;
    procedure GetMemWithoutFreeMemSilentOnBranchRelease;
    procedure GetMemWithoutFreeMemDegradesWithoutResolver;
    procedure GetMemWithoutFreeMemSilentOnUnresolvedOperand;
    procedure ObjectCreatedInLoopNotFreedPositions;
    procedure ObjectCreatedInLoopNotFreedCountsSelfQualifiedField;
    procedure ObjectCreatedInLoopNotFreedCountsOtherLoopKinds;
    procedure ObjectCreatedInLoopNotFreedSilentOnReleaseInBody;
    procedure ObjectCreatedInLoopNotFreedSilentOnStraightLineReacquire;
    procedure ObjectCreatedInLoopNotFreedDegradesWithoutResolver;
    procedure ObjectCreatedInLoopNotFreedSilentOnUnresolvedOperand;
    procedure FreeStateRulesSilentOnNilledReference;
    procedure FreeStateRulesSilentOnInterveningWrite;
    procedure FreeStateRulesSilentOnAddressEscape;
    procedure FreeStateRulesSilentOnAbsoluteAlias;
    procedure FreeStateRulesSilentOnConstAndAbsoluteDeclaration;
    procedure FreeStateRulesSilentOnSelfQualifiedRelease;
    procedure FreeStateRulesSilentOnForeignQualifiedField;
    procedure FreeStateRulesSilentOnCallHandover;
    procedure FreeStateRulesSilentOnStoreHandover;
    procedure FreeStateRulesSilentOnEnclosingRoutineLocal;
    procedure FreeStateRulesSilentOnNestedRoutineMention;
    procedure FreeStateRulesSilentOnInlineAssembler;
    procedure FreeStateRulesSilentOnUnreachableSite;
    procedure ResourceRulesUnmovedByTheNilledState;
    procedure UnbalancedPairPositions;
    procedure UnbalancedPairSilentOnProtectedPair;
    procedure UnbalancedPairSilentOnNestedCoveringFinally;
    procedure UnbalancedPairSilentWithoutARelease;
    procedure UnbalancedPairSilentOnForeignQualifier;
    procedure UnbalancedPairSilentOnMalformedTableEntry;
    procedure UnbalancedPairFollowsTheConfiguredTable;
    procedure UnbalancedPairDegradesWithoutResolver;
    procedure UnbalancedPairSilentOnInlineAssembler;
    procedure ResourceRulesUnmovedByTheGeneralisedRelease;
    procedure LifetimeRulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cDefines: array[0..3] of string = ('FPC', 'CPUX86_64', 'UNIX', 'LINUX');
  cFreeOnInterfaceReferenceId = 'FreeOnInterfaceReference';
  cSelfDestroyedInMethodId = 'SelfDestroyedInMethod';
  cNewDisposeMismatchId = 'NewDisposeMismatch';
  cOwnedFieldNotFreedInDestructorId = 'OwnedFieldNotFreedInDestructor';
  cCreateWithoutTryFinallyId = 'CreateWithoutTryFinally';
  cLeakOnEarlyExitId = 'LeakOnEarlyExit';
  cStreamNotProtectedId = 'StreamNotProtected';
  cExceptionObjectFreedInHandlerId = 'ExceptionObjectFreedInHandler';
  cRaisedExceptionInstanceReusedId = 'RaisedExceptionInstanceReused';
  cLoopVariableUsedAfterLoopId = 'LoopVariableUsedAfterLoop';
  cLoopVariableModifiedInBodyId = 'LoopVariableModifiedInBody';
  cUseAfterFreeId = 'UseAfterFree';
  cDoubleFreeId = 'DoubleFree';
  cFreeNotFreeAndNilOnFieldId = 'FreeNotFreeAndNilOnField';
  cGetMemWithoutFreeMemId = 'GetMemWithoutFreeMem';
  cObjectCreatedInLoopNotFreedId = 'ObjectCreatedInLoopNotFreed';
  cUnbalancedPairId = 'UnbalancedPair';


  // Embedded lifetime-rule fixtures: line i+1 == [i].

  cFreeIntfNoncompliant: array[0..30] of string = (
    'unit FreeIntfNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A worker contract. }',
    '  IFoo = interface',
    '    // Does the work.',
    '    procedure Work;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lIntf: IFoo;',
    '',
    'begin',
    '  lIntf := nil;',
    '  FreeAndNil(lIntf);',
    'end;',
    '',
    'end.');

  cFreeIntfCompliant: array[0..38] of string = (
    'unit FreeIntfOk;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload holder. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    '  { A specialised payload holder. }',
    '  TBar = class(TFoo)',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lFoo: TFoo;',
    '  lBar: TBar;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  // A class reference is the compliant operand => silent.',
    '  FreeAndNil(lFoo);',
    '  lBar := TBar.Create;',
    '  // A class->class cast is ObjectCastBeforeFree''s row => silent.',
    '  TFoo(lBar).Free;',
    'end;',
    '',
    'end.');

  cFreeIntfCastForm: array[0..27] of string = (
    'unit FreeIntfCast;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A worker contract. }',
    '  IFoo = interface',
    '    // Does the work.',
    '    procedure Work;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lIntf: IFoo;',
    '',
    'begin',
    '  lIntf := nil;',
    '  TObject(lIntf).Free;',
    'end;',
    '',
    'end.');

  cFreeIntfUnresolved: array[0..38] of string = (
    'unit FreeIntfOperand;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure Release(var aObj);',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Release(var aObj);',
    '',
    'begin',
    '  FreeAndNil(aObj);',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  New(lRec);',
    '  FreeMem(lRec);',
    'end;',
    '',
    'end.');

  cSelfFreeNoncompliant: array[0..21] of string = (
    'unit SelfFreeNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload holder that detaches itself. }',
    '  TFoo = class(TObject)',
    '    // Releases the holder.',
    '    procedure Detach;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TFoo.Detach;',
    '',
    'begin',
    '  Self.Free;',
    'end;',
    '',
    'end.');

  cSelfFreeCompliant: array[0..54] of string = (
    'unit SelfFreeOk;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  Classes;',
    '',
    'type',
    '  { A payload holder owning a list. }',
    '  TFoo = class(TObject)',
    '    FList: TStringList;',
    '    // Releases the owned list.',
    '    procedure Detach;',
    '    // Tears the holder down.',
    '    destructor Destroy; override;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure TFoo.Detach;',
    '',
    'begin',
    '  // The operand is not Self => silent.',
    '  FList.Free;',
    '  // In a with block the receiver is the with expression => silent.',
    '  with TStringList.Create do',
    '    Free;',
    'end;',
    '',
    '',
    'destructor TFoo.Destroy;',
    '',
    'begin',
    '  // A destructor is the excluded routine => silent.',
    '  Self.Free;',
    '  inherited Destroy;',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  // No enclosing method, so there is no Self => silent.',
    '  lFoo.Free;',
    'end;',
    '',
    'end.');

  cSelfFreeBareForm: array[0..21] of string = (
    'unit SelfFreeBare;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload holder that detaches itself. }',
    '  TFoo = class(TObject)',
    '    // Releases the holder.',
    '    procedure Detach;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TFoo.Detach;',
    '',
    'begin',
    '  Free;',
    'end;',
    '',
    'end.');

  cSelfFreeUnresolved: array[0..42] of string = (
    'unit SelfFreeNested;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    '  { A payload holder that detaches itself through a nested routine. }',
    '  TFoo = class(TObject)',
    '    // Releases the holder.',
    '    procedure Detach;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TFoo.Detach;',
    '',
    'var',
    '  lRec: PRec;',
    '',
    '  procedure Inner;',
    '',
    '  begin',
    '    // A nested routine records no Self argument, so the fact is missing.',
    '    Self.Free;',
    '  end;',
    '',
    'begin',
    '  Inner;',
    '  New(lRec);',
    '  FreeMem(lRec);',
    'end;',
    '',
    'end.');

  cNewMismatchNoncompliant: array[0..30] of string = (
    'unit NewMismatchNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  New(lRec);',
    '  FreeMem(lRec);',
    'end;',
    '',
    'end.');

  cNewMismatchCompliant: array[0..109] of string = (
    'unit NewMismatchOk;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    '  { A holder whose buffer outlives the routine that allocates it. }',
    '  THolder = class(TObject)',
    '    FBuf: PRec;',
    '    // Allocates the buffer.',
    '    procedure Acquire;',
    '    // Releases the buffer.',
    '    procedure Drop;',
    '  end;',
    '',
    'procedure Matched;',
    'procedure AllocOnly;',
    'procedure ReleaseOnly(aPtr: PRec);',
    'procedure TwoAllocators(aFlag: boolean);',
    'procedure ReuseAfterRelease;',
    '',
    'implementation',
    '',
    'procedure THolder.Acquire;',
    '',
    'begin',
    '  New(FBuf);',
    'end;',
    '',
    '',
    'procedure THolder.Drop;',
    '',
    'begin',
    '  // Pairing is per routine: the allocation sits in another method => silent.',
    '  FreeMem(FBuf);',
    'end;',
    '',
    '',
    'procedure Matched;',
    '',
    'var',
    '  lOne: PRec;',
    '  lTwo: PRec;',
    '',
    'begin',
    '  // Both allocator/deallocator pairs match => silent.',
    '  New(lOne);',
    '  Dispose(lOne);',
    '  GetMem(lTwo, 8);',
    '  FreeMem(lTwo);',
    'end;',
    '',
    '',
    'procedure AllocOnly;',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  // An unpaired allocation is GetMemWithoutFreeMem''s row => silent.',
    '  New(lRec);',
    'end;',
    '',
    '',
    'procedure ReleaseOnly(aPtr: PRec);',
    '',
    'begin',
    '  // Nothing is allocated here, so there is nothing to pair against => silent.',
    '  FreeMem(aPtr);',
    'end;',
    '',
    '',
    'procedure TwoAllocators(aFlag: boolean);',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  if aFlag then',
    '    New(lRec)',
    '  else',
    '    GetMem(lRec, 8);',
    '  // Two allocators for one declaration: the pairing is ambiguous => silent.',
    '  FreeMem(lRec);',
    'end;',
    '',
    '',
    'procedure ReuseAfterRelease;',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  // The Dispose pairs with an allocation in another routine and the GetMem',
    '  // is a fresh block with no release here => silent.',
    '  Dispose(lRec);',
    '  GetMem(lRec, 8);',
    'end;',
    '',
    'end.');

  cNewMismatchGetMemForm: array[0..30] of string = (
    'unit GetMemMismatch;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  GetMem(lRec, 8);',
    '  Dispose(lRec);',
    'end;',
    '',
    'end.');

  cNewMismatchUnresolved: array[0..47] of string = (
    'unit NewMismatchOperand;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    '  { A worker contract. }',
    '  IFoo = interface',
    '    // Does the work.',
    '    procedure Work;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    '// Clears aPtr through the unit''s own bookkeeping.',
    'procedure FreeMem(aPtr: PRec);',
    '',
    'begin',
    '  aPtr^.FName := '''';',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lRec: PRec;',
    '  lIntf: IFoo;',
    '',
    'begin',
    '  New(lRec);',
    '  FreeMem(lRec);',
    '  lIntf := nil;',
    '  FreeAndNil(lIntf);',
    'end;',
    '',
    'end.');

  cOwnedFieldNoncompliant: array[0..37] of string = (
    'unit OwnedFieldNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TBar = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    '  { A holder owning a payload. }',
    '  TFoo = class(TObject)',
    '    FBar: TBar;',
    '    // Builds the holder.',
    '    constructor Create;',
    '    // Tears the holder down.',
    '    destructor Destroy; override;',
    '  end;',
    '',
    'implementation',
    '',
    'constructor TFoo.Create;',
    '',
    'begin',
    '  inherited Create;',
    '  FBar := TBar.Create;',
    'end;',
    '',
    '',
    'destructor TFoo.Destroy;',
    '',
    'begin',
    '  inherited Destroy;',
    'end;',
    '',
    'end.');

  cOwnedFieldCompliant: array[0..78] of string = (
    'unit OwnedFieldOk;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload. }',
    '  TBar = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    '  { A list of owned payloads. }',
    '  TBarList = class(TObject)',
    '    // Number of entries.',
    '    function Count: Integer;',
    '    // The entry at aIndex.',
    '    function Item(aIndex: Integer): TBar;',
    '  end;',
    '',
    '  { A holder releasing everything it owns. }',
    '  TFoo = class(TObject)',
    '    FBar: TBar;',
    '    FOther: TBar;',
    '    FList: TBarList;',
    '    FOwner: TBar;',
    '    // Builds the holder around aOwner.',
    '    constructor Create(aOwner: TBar);',
    '    // Tears the holder down.',
    '    destructor Destroy; override;',
    '  end;',
    '',
    'implementation',
    '',
    'function TBarList.Count: Integer;',
    '',
    'begin',
    '  Result := 0;',
    'end;',
    '',
    '',
    'function TBarList.Item(aIndex: Integer): TBar;',
    '',
    'begin',
    '  Result := nil;',
    'end;',
    '',
    '',
    'constructor TFoo.Create(aOwner: TBar);',
    '',
    'begin',
    '  inherited Create;',
    '  FBar := TBar.Create;',
    '  FOther := TBar.Create;',
    '  FList := TBarList.Create;',
    '  // A field assigned from a parameter is not owned => never recorded.',
    '  FOwner := aOwner;',
    'end;',
    '',
    '',
    'destructor TFoo.Destroy;',
    '',
    'var',
    '  i: Integer;',
    '',
    'begin',
    '  FBar.Free;',
    '  FreeAndNil(FOther);',
    '  // A loop over an owned list mentions the field => silent.',
    '  for i := 0 to FList.Count - 1 do',
    '    FList.Item(i).Free;',
    '  FList.Free;',
    '  inherited Destroy;',
    'end;',
    '',
    'end.');

  cOwnedFieldHelper: array[0..66] of string = (
    'unit OwnedFieldHelper;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TBar = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    '  { A holder releasing through a helper. }',
    '  TFoo = class(TObject)',
    '    FBar: TBar;',
    '    // Releases everything the holder owns.',
    '    procedure ReleaseAll;',
    '    // Builds the holder.',
    '    constructor Create;',
    '    // Tears the holder down.',
    '    destructor Destroy; override;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure TFoo.ReleaseAll;',
    '',
    'begin',
    '  FBar.Free;',
    'end;',
    '',
    '',
    'constructor TFoo.Create;',
    '',
    'begin',
    '  inherited Create;',
    '  FBar := TBar.Create;',
    'end;',
    '',
    '',
    'destructor TFoo.Destroy;',
    '',
    'begin',
    '  ReleaseAll;',
    '  inherited Destroy;',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  New(lRec);',
    '  FreeMem(lRec);',
    'end;',
    '',
    'end.');

  cOwnedFieldHook: array[0..65] of string = (
    'unit OwnedFieldHook;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TBar = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    '  { A holder releasing in a pre-destruction hook. }',
    '  TFoo = class(TObject)',
    '    FBar: TBar;',
    '    // Runs before the holder is taken apart.',
    '    procedure BeforeDestruction;',
    '    // Builds the holder.',
    '    constructor Create;',
    '    // Tears the holder down.',
    '    destructor Destroy; override;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure TFoo.BeforeDestruction;',
    '',
    'begin',
    '  FBar.Free;',
    'end;',
    '',
    '',
    'constructor TFoo.Create;',
    '',
    'begin',
    '  inherited Create;',
    '  FBar := TBar.Create;',
    'end;',
    '',
    '',
    'destructor TFoo.Destroy;',
    '',
    'begin',
    '  inherited Destroy;',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  New(lRec);',
    '  FreeMem(lRec);',
    'end;',
    '',
    'end.');

  cOwnedFieldInherited: array[0..60] of string = (
    'unit OwnedFieldInherited;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TBar = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    '  { A base holder that owns the teardown. }',
    '  TBase = class(TObject)',
    '    // Tears the holder down.',
    '    destructor Destroy; override;',
    '  end;',
    '',
    '  { A holder relying on the ancestor teardown. }',
    '  TFoo = class(TBase)',
    '    FBar: TBar;',
    '    // Builds the holder.',
    '    constructor Create;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'destructor TBase.Destroy;',
    '',
    'begin',
    '  inherited Destroy;',
    'end;',
    '',
    '',
    'constructor TFoo.Create;',
    '',
    'begin',
    '  inherited Create;',
    '  FBar := TBar.Create;',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  New(lRec);',
    '  FreeMem(lRec);',
    'end;',
    '',
    'end.');

  cOwnedFieldUnresolved: array[0..67] of string = (
    'unit OwnedFieldOperand;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    '  { A payload built by a factory method. }',
    '  TBar = class(TObject)',
    '    // Builds a payload.',
    '    class function Create(const aName: string): TBar;',
    '  end;',
    '',
    '  { A holder whose payload comes from a factory. }',
    '  TFoo = class(TObject)',
    '    FBar: TBar;',
    '    // Builds the holder.',
    '    constructor Create;',
    '    // Tears the holder down.',
    '    destructor Destroy; override;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'class function TBar.Create(const aName: string): TBar;',
    '',
    'begin',
    '  Result := nil;',
    'end;',
    '',
    '',
    'constructor TFoo.Create;',
    '',
    'begin',
    '  inherited Create;',
    '  FBar := TBar.Create(''bar'');',
    'end;',
    '',
    '',
    'destructor TFoo.Destroy;',
    '',
    'begin',
    '  inherited Destroy;',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  New(lRec);',
    '  FreeMem(lRec);',
    'end;',
    '',
    'end.');

  cCreateNoTryNoncompliant: array[0..35] of string = (
    'unit CreateNoTryNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  Work(lFoo);',
    '  lFoo.Free;',
    'end;',
    '',
    'end.');

  cCreateNoTryCompliant: array[0..90] of string = (
    'unit CreateNoTryOk;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'var',
    '  GFoo: TFoo;',
    '',
    'function Make: TFoo;',
    'procedure Guarded;',
    'procedure AcquireInsideTry;',
    'procedure HandsOver;',
    'procedure Global;',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'function Make: TFoo;',
    '',
    'begin',
    '  // The construction is the return value, not a local => silent.',
    '  Result := TFoo.Create;',
    'end;',
    '',
    '',
    'procedure Guarded;',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  try',
    '    Work(lFoo);',
    '  finally',
    '    lFoo.Free;',
    '  end;',
    'end;',
    '',
    '',
    'procedure AcquireInsideTry;',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  try',
    '    // TryFinallyAcquireOutsideTry owns this row => silent.',
    '    lFoo := TFoo.Create;',
    '    Work(lFoo);',
    '  finally',
    '    lFoo.Free;',
    '  end;',
    'end;',
    '',
    '',
    'procedure HandsOver;',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  // Nothing releases it here: ownership transfer, not this row.',
    '  lFoo := TFoo.Create;',
    '  Work(lFoo);',
    'end;',
    '',
    '',
    'procedure Global;',
    '',
    'begin',
    '  // A unit-level var is not a local => silent.',
    '  GFoo := TFoo.Create;',
    '  GFoo.Free;',
    'end;',
    '',
    'end.');

  cCreateNoTryFreeAndNilForm: array[0..29] of string = (
    'unit CreateFreeAndNil;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  FreeAndNil(lFoo);',
    'end;',
    '',
    'end.');

  cCreateNoTryUnresolved: array[0..46] of string = (
    'unit CreateNoTryOperand;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure Release(var aObj);',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Release(var aObj);',
    '',
    'begin',
    '  FreeAndNil(aObj);',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lFoo: TFoo;',
    '  lRec: PRec;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  Release(lFoo);',
    '  New(lRec);',
    '  FreeMem(lRec);',
    'end;',
    '',
    'end.');

  cCreateNoTryBranchForm: array[0..27] of string = (
    'unit CreateNoTryBranch;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'implementation',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  if aFlag then',
    '    lFoo.Free;',
    'end;',
    '',
    'end.');

  cCreateNoTryReleaseFirstForm: array[0..36] of string = (
    'unit CreateNoTryReleaseFirst;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure Run(aFoo: TFoo);',
    '',
    'implementation',
    '',
    'procedure Run(aFoo: TFoo);',
    '',
    'var',
    '  lFoo: TFoo;',
    '  lRec: PRec;',
    '',
    'begin',
    '  lFoo := aFoo;',
    '  lFoo.Free;',
    '  lFoo := TFoo.Create;',
    '  New(lRec);',
    '  FreeMem(lRec);',
    'end;',
    '',
    'end.');

  cCreateNoTryHandoverBranchForm: array[0..42] of string = (
    'unit CreateNoTryHandover;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  if aFlag then',
    '  begin',
    '    lFoo := TFoo.Create;',
    '    Work(lFoo);',
    '  end',
    '  else',
    '  begin',
    '    lFoo := TFoo.Create;',
    '    lFoo.Free;',
    '  end;',
    'end;',
    '',
    'end.');

  cCreateNoTryNestedCoverForm: array[0..49] of string = (
    'unit CreateNoTryNestedCover;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '  lRec: PRec;',
    '',
    'begin',
    '  lFoo := nil;',
    '  if aFlag then',
    '    lFoo := TFoo.Create;',
    '  try',
    '    Work(lFoo);',
    '  finally',
    '    lFoo.Free;',
    '  end;',
    '  New(lRec);',
    '  FreeMem(lRec);',
    'end;',
    '',
    'end.');

  cCreateNoTryReacquireForm: array[0..41] of string = (
    'unit CreateNoTryReacquire;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  Work(lFoo);',
    '  lFoo.Free;',
    '  lFoo := TFoo.Create;',
    '  try',
    '    Work(lFoo);',
    '  finally',
    '    lFoo.Free;',
    '  end;',
    'end;',
    '',
    'end.');

  cLeakExitNoncompliant: array[0..37] of string = (
    'unit LeakExitNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  if aFlag then',
    '    Exit;',
    '  Work(lFoo);',
    '  lFoo.Free;',
    'end;',
    '',
    'end.');

  cLeakExitCompliant: array[0..88] of string = (
    'unit LeakExitOk;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Guarded(aFlag: boolean);',
    'procedure HandsOver(aFlag: boolean);',
    'procedure ReleasedFirst(aFlag: boolean);',
    'procedure ReleasedOnBranch(aFlag, aOther: boolean);',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Guarded(aFlag: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  try',
    '    // The finally runs on the exit path => silent.',
    '    if aFlag then',
    '      Exit;',
    '    Work(lFoo);',
    '  finally',
    '    lFoo.Free;',
    '  end;',
    'end;',
    '',
    '',
    'procedure HandsOver(aFlag: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  // Nothing releases it here: ownership transfer, not a leak.',
    '  lFoo := TFoo.Create;',
    '  if aFlag then',
    '    Exit;',
    '  Work(lFoo);',
    'end;',
    '',
    '',
    'procedure ReleasedFirst(aFlag: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  Work(lFoo);',
    '  lFoo.Free;',
    '  if aFlag then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure ReleasedOnBranch(aFlag, aOther: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  // Released on one incoming path, so the merge reads it as released.',
    '  if aFlag then',
    '    lFoo.Free;',
    '  if aOther then',
    '    Exit;',
    '  Work(lFoo);',
    'end;',
    '',
    'end.');

  cLeakExitRaiseForm: array[0..34] of string = (
    'unit LeakExitRaise;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    '  { A failure. }',
    '  EFoo = class(Exception);',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'implementation',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  if aFlag then',
    '    raise EFoo.Create(''bad'');',
    '  lFoo.Free;',
    'end;',
    '',
    'end.');

  cLeakExitHandledRaiseForm: array[0..48] of string = (
    'unit LeakExitHandled;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    '  { A failure. }',
    '  EFoo = class(Exception);',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  try',
    '    if aFlag then',
    '      raise EFoo.Create(''bad'');',
    '    Work(lFoo);',
    '  except',
    '    on E: Exception do',
    '      Work(nil);',
    '  end;',
    '  lFoo.Free;',
    'end;',
    '',
    'end.');

  cLeakExitDeadAcquire: array[0..33] of string = (
    'unit LeakDead;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Run(aFlag: boolean; aOther: boolean);',
    '',
    'implementation',
    '',
    'procedure Run(aFlag: boolean; aOther: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := nil;',
    '  if aFlag then',
    '  begin',
    '    Exit;',
    '    lFoo := TFoo.Create;',
    '  end;',
    '  if aOther then',
    '    Exit;',
    '  lFoo.Free;',
    'end;',
    '',
    'end.');

  cLeakExitUnresolved: array[0..48] of string = (
    'unit LeakExitOperand;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure Release(var aObj);',
    'procedure Run(aFlag: boolean);',
    '',
    'implementation',
    '',
    'procedure Release(var aObj);',
    '',
    'begin',
    '  FreeAndNil(aObj);',
    'end;',
    '',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '  lRec: PRec;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  if aFlag then',
    '    Exit;',
    '  Release(lFoo);',
    '  New(lRec);',
    '  FreeMem(lRec);',
    'end;',
    '',
    'end.');

  cStreamNoncompliant: array[0..23] of string = (
    'unit StreamNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  Classes;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lStm: TFileStream;',
    '',
    'begin',
    '  lStm := TFileStream.Create(''data.bin'', fmOpenRead);',
    '  lStm.Free;',
    'end;',
    '',
    'end.');

  cStreamCompliant: array[0..54] of string = (
    'unit StreamOk;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  Classes;',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Guarded;',
    'procedure Plain;',
    '',
    'implementation',
    '',
    'procedure Work(aStm: TStream);',
    '',
    'begin',
    '  if aStm = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Guarded;',
    '',
    'var',
    '  lStm: TFileStream;',
    '',
    'begin',
    '  lStm := TFileStream.Create(''data.bin'', fmOpenRead);',
    '  try',
    '    Work(lStm);',
    '  finally',
    '    lStm.Free;',
    '  end;',
    'end;',
    '',
    '',
    'procedure Plain;',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  // Not one of the stream classes => silent.',
    '  lFoo := TFoo.Create;',
    '  lFoo.Free;',
    'end;',
    '',
    'end.');

  cStreamNeverReleased: array[0..31] of string = (
    'unit StreamOpen;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  Classes;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Work(aStm: TStream);',
    '',
    'begin',
    '  if aStm = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lStm: TFileStream;',
    '',
    'begin',
    '  lStm := TFileStream.Create(''data.bin'', fmOpenRead);',
    '  Work(lStm);',
    'end;',
    '',
    'end.');

  cStreamUnresolved: array[0..41] of string = (
    'unit StreamOperand;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils, Classes;',
    '',
    'type',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure Release(var aObj);',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Release(var aObj);',
    '',
    'begin',
    '  FreeAndNil(aObj);',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lStm: TFileStream;',
    '  lRec: PRec;',
    '',
    'begin',
    '  lStm := TFileStream.Create(''data.bin'', fmOpenRead);',
    '  Release(lStm);',
    '  New(lRec);',
    '  FreeMem(lRec);',
    'end;',
    '',
    'end.');

  cStreamSharedFinally: array[0..38] of string = (
    'unit StreamArms;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  Classes;',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'implementation',
    '',
    'procedure Work(aStm: TStream);',
    '',
    'begin',
    '  if aStm = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'var',
    '  lStm: TFileStream;',
    '',
    'begin',
    '  if aFlag then',
    '    lStm := TFileStream.Create(''a.bin'', fmOpenRead)',
    '  else',
    '    lStm := TFileStream.Create(''b.bin'', fmOpenRead);',
    '  try',
    '    Work(lStm);',
    '  finally',
    '    lStm.Free;',
    '  end;',
    'end;',
    '',
    'end.');

  cHandlerFreeNoncompliant: array[0..27] of string = (
    'unit HandlerFreeNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lText: string;',
    '',
    'begin',
    '  try',
    '    lText := '''';',
    '  except',
    '    on E: Exception do',
    '      E.Free;',
    '  end;',
    'end;',
    '',
    'end.');

  cHandlerFreeCompliant: array[0..68] of string = (
    'unit HandlerFreeOk;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure ReadsMessage;',
    'procedure FreesAnother;',
    'procedure FreesInFinally;',
    '',
    'implementation',
    '',
    'procedure ReadsMessage;',
    '',
    'var',
    '  lText: string;',
    '',
    'begin',
    '  try',
    '    lText := '''';',
    '  except',
    '    on E: Exception do',
    '      // Reading the message leaves the RTL''s object alone => silent.',
    '      lText := E.Message;',
    '  end;',
    'end;',
    '',
    '',
    'procedure FreesAnother;',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  try',
    '    lFoo.FName := '''';',
    '  except',
    '    on E: Exception do',
    '      // A different object is disposed of => silent.',
    '      lFoo.Free;',
    '  end;',
    'end;',
    '',
    '',
    'procedure FreesInFinally;',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  try',
    '    lFoo.FName := '''';',
    '  finally',
    '    lFoo.Free;',
    '  end;',
    'end;',
    '',
    'end.');

  cHandlerFreeAndNilForm: array[0..27] of string = (
    'unit HandlerFreeAndNil;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lText: string;',
    '',
    'begin',
    '  try',
    '    lText := '''';',
    '  except',
    '    on E: Exception do',
    '      FreeAndNil(E);',
    '  end;',
    'end;',
    '',
    'end.');

  cHandlerFreeUnresolved: array[0..44] of string = (
    'unit HandlerFreeOperand;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure Sink(var aObj);',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Sink(var aObj);',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lRec: PRec;',
    '  lText: string;',
    '',
    'begin',
    '  New(lRec);',
    '  FreeMem(lRec);',
    '  try',
    '    lText := '''';',
    '  except',
    '    on E: Exception do',
    '      Sink(E);',
    '  end;',
    'end;',
    '',
    'end.');

  cRaiseReuseNoncompliant: array[0..25] of string = (
    'unit RaiseReuseNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lErr: Exception;',
    '  lText: string;',
    '',
    'begin',
    '  lErr := Exception.Create(''boom'');',
    '  raise lErr;',
    '  lText := lErr.Message;',
    'end;',
    '',
    'end.');

  cRaiseReuseCompliant: array[0..66] of string = (
    'unit RaiseReuseOk;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A domain failure. }',
    '  EFoo = class(Exception)',
    '  end;',
    '',
    'procedure NoLaterUse;',
    'procedure Constructed;',
    'procedure Rethrown;',
    'procedure OtherVar;',
    '',
    'implementation',
    '',
    'procedure NoLaterUse;',
    '',
    'var',
    '  lErr: Exception;',
    '',
    'begin',
    '  lErr := Exception.Create(''boom'');',
    '  // Nothing references lErr after the raise => silent.',
    '  raise lErr;',
    'end;',
    '',
    '',
    'procedure Constructed;',
    '',
    'begin',
    '  // The operand is a constructor call, not a variable => silent.',
    '  raise EFoo.Create(''x'');',
    'end;',
    '',
    '',
    'procedure Rethrown;',
    '',
    'begin',
    '  try',
    '    raise EFoo.Create(''x'');',
    '  except',
    '    // A bare re-raise carries no operand => silent.',
    '    raise;',
    '  end;',
    'end;',
    '',
    '',
    'procedure OtherVar;',
    '',
    'var',
    '  lErr: Exception;',
    '  lText: string;',
    '',
    'begin',
    '  lErr := Exception.Create(''boom'');',
    '  raise lErr;',
    '  // The later reference is to another variable => silent.',
    '  lText := '''';',
    'end;',
    '',
    'end.');

  cRaiseReuseParameterForm: array[0..23] of string = (
    'unit RaiseReuseParam;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'procedure Fail(aErr: Exception);',
    '',
    'implementation',
    '',
    'procedure Fail(aErr: Exception);',
    '',
    'var',
    '  lText: string;',
    '',
    'begin',
    '  raise aErr;',
    '  lText := aErr.Message;',
    'end;',
    '',
    'end.');

  cRaiseReuseClearedForm: array[0..27] of string = (
    'unit RaiseReuseCleared;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lErr: Exception;',
    '',
    'begin',
    '  lErr := Exception.Create(''boom'');',
    '  try',
    '    raise lErr;',
    '  finally',
    '    lErr := nil;',
    '  end;',
    'end;',
    '',
    'end.');

  cRaiseReuseUnresolved: array[0..41] of string = (
    'unit RaiseReuseOperand;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'function MakeError: Exception;',
    'procedure Run;',
    '',
    'implementation',
    '',
    'function MakeError: Exception;',
    '',
    'begin',
    '  Result := Exception.Create(''boom'');',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lRec: PRec;',
    '  lText: string;',
    '',
    'begin',
    '  New(lRec);',
    '  FreeMem(lRec);',
    '  raise MakeError;',
    '  lText := MakeError.Message;',
    'end;',
    '',
    'end.');

  cLoopAfterNoncompliant: array[0..23] of string = (
    'unit LoopAfterNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'function Run: integer;',
    '',
    'implementation',
    '',
    'function Run: integer;',
    '',
    'var',
    '  lIdx, lSum: integer;',
    '',
    'begin',
    '  lSum := 0;',
    '  for lIdx := 1 to 3 do',
    '    lSum := lSum + 1;',
    '  lSum := lSum + lIdx;',
    '  Result := lSum;',
    'end;',
    '',
    'end.');

  cLoopAfterCompliant: array[0..57] of string = (
    'unit LoopAfterOk;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'var',
    '  GIdx: integer;',
    '',
    'function InsideBody: integer;',
    'function OverCollection: integer;',
    'function OverGlobal: integer;',
    '',
    'implementation',
    '',
    'function InsideBody: integer;',
    '',
    'var',
    '  lIdx, lSum: integer;',
    '',
    'begin',
    '  lSum := 0;',
    '  for lIdx := 1 to 3 do',
    '    // The read is INSIDE the body => silent.',
    '    lSum := lSum + lIdx;',
    '  Result := lSum;',
    'end;',
    '',
    '',
    'function OverCollection: integer;',
    '',
    'var',
    '  lArr: array[0..2] of integer;',
    '  lItem, lSum: integer;',
    '',
    'begin',
    '  lArr[0] := 1;',
    '  lArr[1] := 2;',
    '  lArr[2] := 3;',
    '  lSum := 0;',
    '  // A for..in loop is out of the rule''s reach => silent.',
    '  for lItem in lArr do',
    '    lSum := lSum + 1;',
    '  Result := lSum + lItem;',
    'end;',
    '',
    '',
    'function OverGlobal: integer;',
    '',
    'begin',
    '  Result := 0;',
    '  // The control variable is a unit-level var, not a routine local => silent.',
    '  for GIdx := 1 to 3 do',
    '    Result := Result + 1;',
    '  Result := Result + GIdx;',
    'end;',
    '',
    'end.');

  cLoopAfterWriteForm: array[0..23] of string = (
    'unit LoopAfterWrite;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'function Run: integer;',
    '',
    'implementation',
    '',
    'function Run: integer;',
    '',
    'var',
    '  lIdx, lSum: integer;',
    '',
    'begin',
    '  lSum := 0;',
    '  for lIdx := 1 to 3 do',
    '    lSum := lSum + 1;',
    '  lIdx := 0;',
    '  Result := lSum;',
    'end;',
    '',
    'end.');

  cLoopAfterCounterReuseForm: array[0..24] of string = (
    'unit LoopAfterReuse;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'function Run: integer;',
    '',
    'implementation',
    '',
    'function Run: integer;',
    '',
    'var',
    '  lIdx, lSum: integer;',
    '',
    'begin',
    '  lSum := 0;',
    '  for lIdx := 1 to 3 do',
    '    lSum := lSum + 1;',
    '  for lIdx := 1 to 4 do',
    '    lSum := lSum + lIdx;',
    '  Result := lSum;',
    'end;',
    '',
    'end.');

  cLoopAfterNearestForm: array[0..25] of string = (
    'unit LoopAfterNearest;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'function Run: integer;',
    '',
    'implementation',
    '',
    'function Run: integer;',
    '',
    'var',
    '  lIdx, lSum: integer;',
    '',
    'begin',
    '  lSum := 0;',
    '  for lIdx := 1 to 3 do',
    '    lSum := lSum + 1;',
    '  for lIdx := 1 to 4 do',
    '    lSum := lSum + 2;',
    '  lSum := lSum + lIdx;',
    '  Result := lSum;',
    'end;',
    '',
    'end.');

  cLoopAfterUnresolved: array[0..35] of string = (
    'unit LoopAfterOperand;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A counter record. }',
    '  TRec = record',
    '    lIdx: integer;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'function Run: integer;',
    '',
    'implementation',
    '',
    'function Run: integer;',
    '',
    'var',
    '  lIdx: integer;',
    '  lRec: TRec;',
    '  lPtr: PRec;',
    '',
    'begin',
    '  New(lPtr);',
    '  FreeMem(lPtr);',
    '  lRec.lIdx := 0;',
    '  Result := 0;',
    '  for lIdx := 1 to 3 do',
    '    Result := Result + 1;',
    '  with lRec do',
    '    Result := Result + lIdx;',
    'end;',
    '',
    'end.');

  cLoopModNoncompliant: array[0..24] of string = (
    'unit LoopModNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'function Run: integer;',
    '',
    'implementation',
    '',
    'function Run: integer;',
    '',
    'var',
    '  lIdx: integer;',
    '',
    'begin',
    '  Result := 0;',
    '  for lIdx := 1 to 3 do',
    '  begin',
    '    lIdx := 9;',
    '    Result := Result + 1;',
    '  end;',
    'end;',
    '',
    'end.');

  cLoopModCompliant: array[0..23] of string = (
    'unit LoopModOk;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'function Run: integer;',
    '',
    'implementation',
    '',
    'function Run: integer;',
    '',
    'var',
    '  lIdx, lSum: integer;',
    '',
    'begin',
    '  lSum := 0;',
    '  for lIdx := 1 to 3 do',
    '    // The assignment target is another variable => silent.',
    '    lSum := lSum + lIdx;',
    '  Result := lSum;',
    'end;',
    '',
    'end.');

  cLoopModNestedForm: array[0..22] of string = (
    'unit LoopModNested;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'function Run: integer;',
    '',
    'implementation',
    '',
    'function Run: integer;',
    '',
    'var',
    '  lOuter, lInner: integer;',
    '',
    'begin',
    '  Result := 0;',
    '  for lOuter := 1 to 3 do',
    '    for lInner := 1 to 2 do',
    '      lOuter := 9;',
    'end;',
    '',
    'end.');

  cLoopModUnparseable: array[0..25] of string = (
    'unit LoopModBroken;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject',
    '  end;',
    '',
    'function Run: integer;',
    '',
    'implementation',
    '',
    'function Run: integer;',
    '',
    'var',
    '  lIdx: integer;',
    '',
    'begin',
    '  Result := 0;',
    '  for lIdx := 1 to 3 do',
    '    lIdx := 9;',
    'end;',
    '',
    'end.');


  cUseAfterFreeNoncompliant: array[0..36] of string = (
    'unit UseAfterFreeNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  if aFlag then',
    '    lFoo.Free;',
    '  lFoo.FName := '''';',
    'end;',
    '',
    'end.');

  cUseAfterFreeCompliant: array[0..68] of string = (
    'unit UseAfterFreeOk;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Nilled(aFlag: boolean);',
    'procedure ReleasedAfter;',
    'procedure Reacquired;',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Nilled(aFlag: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  if aFlag then',
    '    FreeAndNil(lFoo);',
    '  // The reference is nil rather than dangling => silent.',
    '  lFoo.FName := '''';',
    'end;',
    '',
    '',
    'procedure ReleasedAfter;',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  lFoo.FName := '''';',
    '  lFoo.Free;',
    'end;',
    '',
    '',
    'procedure Reacquired;',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  lFoo.Free;',
    '  lFoo := TFoo.Create;',
    '  lFoo.FName := '''';',
    '  lFoo.Free;',
    'end;',
    '',
    'end.');

  cDoubleFreeNoncompliant: array[0..36] of string = (
    'unit DoubleFreeNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  lFoo.FName := '''';',
    '  lFoo.Free;',
    '  lFoo.Free;',
    'end;',
    '',
    'end.');

  cDoubleFreeCompliant: array[0..59] of string = (
    'unit DoubleFreeOk;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Nilled;',
    'procedure Cleared;',
    'procedure Reacquired;',
    '',
    'implementation',
    '',
    'procedure Nilled;',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  // Releasing nil is a no-op => silent.',
    '  FreeAndNil(lFoo);',
    '  lFoo.Free;',
    'end;',
    '',
    '',
    'procedure Cleared;',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  lFoo.Free;',
    '  lFoo := nil;',
    '  lFoo.Free;',
    'end;',
    '',
    '',
    'procedure Reacquired;',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  lFoo.Free;',
    '  lFoo := TFoo.Create;',
    '  lFoo.Free;',
    'end;',
    '',
    'end.');

  cFieldFreeNoncompliant: array[0..36] of string = (
    'unit FieldFreeNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    '  { A holder with an owned field. }',
    '  THolder = class(TObject)',
    '    FFoo: TFoo;',
    '    // Releases the field and reports on it.',
    '    procedure Drop;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure THolder.Drop;',
    '',
    'begin',
    '  FFoo.Free;',
    '  FFoo.FName := '''';',
    'end;',
    '',
    'end.');

  cFieldFreeCompliant: array[0..59] of string = (
    'unit FieldFreeOk;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    '  { A holder with an owned field. }',
    '  THolder = class(TObject)',
    '    FFoo: TFoo;',
    '    // Nils the field before reading it again.',
    '    procedure Cleared;',
    '    // Releases the field and reads it nowhere after.',
    '    procedure Quiet;',
    '    // Replaces the field before reading it again.',
    '    procedure Renewed;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure THolder.Cleared;',
    '',
    'begin',
    '  FreeAndNil(FFoo);',
    '  FFoo.FName := '''';',
    'end;',
    '',
    '',
    'procedure THolder.Quiet;',
    '',
    'begin',
    '  FFoo.Free;',
    'end;',
    '',
    '',
    'procedure THolder.Renewed;',
    '',
    'begin',
    '  FFoo.Free;',
    '  FFoo := TFoo.Create;',
    '  FFoo.FName := '''';',
    'end;',
    '',
    'end.');

  cFieldNilledForm: array[0..45] of string = (
    'unit FieldNilled;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    '  { A holder with an owned field. }',
    '  THolder = class(TObject)',
    '    FFoo: TFoo;',
    '    // Nils the field and a local before reading both again.',
    '    procedure Drop;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure THolder.Drop;',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  FreeAndNil(lFoo);',
    '  lFoo.FName := '''';',
    '  FreeAndNil(FFoo);',
    '  FFoo.FName := '''';',
    'end;',
    '',
    'end.');

  cGetMemNoncompliant: array[0..26] of string = (
    'unit GetMemNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  GetMem(lRec, 8);',
    'end;',
    '',
    'end.');

  cGetMemCompliant: array[0..55] of string = (
    'unit GetMemOk;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure Paired;',
    'procedure ReleasedOnBranch(aFlag: boolean);',
    'procedure TwoAllocations(aFlag: boolean);',
    '',
    'implementation',
    '',
    'procedure Paired;',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  GetMem(lRec, 8);',
    '  FreeMem(lRec);',
    'end;',
    '',
    '',
    'procedure ReleasedOnBranch(aFlag: boolean);',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  GetMem(lRec, 8);',
    '  // Released on one path satisfies an absence rule => silent.',
    '  if aFlag then',
    '    FreeMem(lRec);',
    'end;',
    '',
    '',
    'procedure TwoAllocations(aFlag: boolean);',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  if aFlag then',
    '    New(lRec)',
    '  else',
    '    GetMem(lRec, 8);',
    'end;',
    '',
    'end.');

  cLoopCreateNoncompliant: array[0..38] of string = (
    'unit LoopCreateNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  while aFlag do',
    '  begin',
    '    lFoo := TFoo.Create;',
    '    lFoo.FName := '''';',
    '  end;',
    '  lFoo.Free;',
    'end;',
    '',
    'end.');

  cLoopCreateCompliant: array[0..72] of string = (
    'unit LoopCreateOk;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure ReleasedInBody(aFlag: boolean);',
    'procedure GuardedInBody(aFlag: boolean);',
    'procedure StraightLine;',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure ReleasedInBody(aFlag: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  while aFlag do',
    '  begin',
    '    lFoo := TFoo.Create;',
    '    lFoo.FName := '''';',
    '    lFoo.Free;',
    '  end;',
    'end;',
    '',
    '',
    'procedure GuardedInBody(aFlag: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  while aFlag do',
    '  begin',
    '    lFoo := TFoo.Create;',
    '    try',
    '      lFoo.FName := '''';',
    '    finally',
    '      lFoo.Free;',
    '    end;',
    '  end;',
    'end;',
    '',
    '',
    'procedure StraightLine;',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  lFoo.FName := '''';',
    '  lFoo := TFoo.Create;',
    '  lFoo.FName := '''';',
    '  lFoo.Free;',
    'end;',
    '',
    'end.');

  cLoopCreateBodyRelease: array[0..30] of string = (
    'unit LoopCreateBodyFree;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'implementation',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  while aFlag do',
    '  begin',
    '    lFoo := TFoo.Create;',
    '    lFoo.FName := '''';',
    '    lFoo.Free;',
    '  end;',
    'end;',
    '',
    'end.');

  cLoopCreateOtherKinds: array[0..46] of string = (
    'unit LoopCreateKinds;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Repeated(aFlag: boolean);',
    'procedure Counted;',
    '',
    'implementation',
    '',
    'procedure Repeated(aFlag: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  repeat',
    '    lFoo := TFoo.Create;',
    '    lFoo.FName := '''';',
    '  until aFlag;',
    '  lFoo.Free;',
    'end;',
    '',
    '',
    'procedure Counted;',
    '',
    'var',
    '  lFoo: TFoo;',
    '  i: integer;',
    '',
    'begin',
    '  for i := 1 to 3 do',
    '  begin',
    '    lFoo := TFoo.Create;',
    '    lFoo.FName := '''';',
    '  end;',
    '  lFoo.Free;',
    'end;',
    '',
    'end.');

  cFreeStateStoreHandover: array[0..32] of string = (
    'unit FreeStateStoreHand;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'var',
    '  GKeep: TFoo;',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'implementation',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  while aFlag do',
    '  begin',
    '    lFoo := TFoo.Create;',
    '    GKeep := lFoo;',
    '  end;',
    'end;',
    '',
    'end.');

  cLoopCreateStraightLine: array[0..37] of string = (
    'unit LoopCreateStraight;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  lFoo.FName := '''';',
    '  lFoo := TFoo.Create;',
    '  lFoo.FName := '''';',
    '  lFoo.Free;',
    'end;',
    '',
    'end.');

  cFreeStateInterveningWrite: array[0..37] of string = (
    'unit FreeStateWrite;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Run(aOther: TFoo);',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Run(aOther: TFoo);',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  lFoo.Free;',
    '  // An unrecognised write drops the declaration => silent.',
    '  lFoo := aOther;',
    '  lFoo.FName := '''';',
    'end;',
    '',
    'end.');

  cFreeStateAddressEscape: array[0..46] of string = (
    'unit FreeStateAddr;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Track(aPtr: Pointer);',
    '',
    'begin',
    '  if aPtr = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lFoo: TFoo;',
    '  lPtr: Pointer;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  lFoo.Free;',
    '  // The address escapes, so any store through it is invisible => silent.',
    '  lPtr := @lFoo;',
    '  lFoo.FName := '''';',
    'end;',
    '',
    'end.');

  cFreeStateAbsoluteAlias: array[0..38] of string = (
    'unit FreeStateAbs;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lFoo: TFoo;',
    '  lAlias: TFoo absolute lFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  lFoo.Free;',
    '  // An absolute alias can store into the slot => silent.',
    '  lAlias.FName := '''';',
    '  lFoo.FName := '''';',
    'end;',
    '',
    'end.');

  cFreeStateNestedMention: array[0..44] of string = (
    'unit FreeStateNested;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    '  // Reads the enclosing routine''s local.',
    '  procedure Peek;',
    '',
    '  begin',
    '    Work(lFoo);',
    '  end;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  lFoo.Free;',
    '  // A nested routine may store into the slot => silent.',
    '  Peek;',
    '  lFoo.FName := '''';',
    'end;',
    '',
    'end.');

  cFreeStateAsmBody: array[0..38] of string = (
    'unit FreeStateAsm;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  lFoo.Free;',
    '  asm',
    '    nop',
    '  end;',
    '  lFoo.FName := '''';',
    'end;',
    '',
    'end.');

  cFreeStateDeadSite: array[0..40] of string = (
    'unit FreeStateDead;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'implementation',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  if aFlag then',
    '  begin',
    '    Exit;',
    '    lFoo.Free;',
    '  end;',
    '  lFoo.FName := '''';',
    '  lFoo.Free;',
    'end;',
    '',
    'end.');

  cFreeStateUnresolved: array[0..78] of string = (
    'unit FreeStateOperand;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    '  { A holder with an owned field. }',
    '  THolder = class(TObject)',
    '    FFoo: TFoo;',
    '    // Releases the field through the helper and reads it again.',
    '    procedure Drop;',
    '  end;',
    '',
    'procedure Release(var aObj);',
    'procedure Run(aFlag: boolean);',
    '',
    'implementation',
    '',
    'procedure Release(var aObj);',
    '',
    'begin',
    '  FreeAndNil(aObj);',
    'end;',
    '',
    '',
    'procedure Work(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure THolder.Drop;',
    '',
    'begin',
    '  Release(FFoo);',
    '  Work(FFoo);',
    'end;',
    '',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '  lBar: TFoo;',
    '  lOne: PRec;',
    '  lTwo: PRec;',
    '',
    'begin',
    '  lFoo := TFoo.Create;',
    '  Release(lFoo);',
    '  lFoo.Free;',
    '  Work(lFoo);',
    '  while aFlag do',
    '    lBar := TFoo.Create;',
    '  Release(lBar);',
    '  New(lOne);',
    '  Release(lOne);',
    '  New(lTwo);',
    '  FreeMem(lTwo);',
    'end;',
    '',
    'end.');

  cFreeStateSelfQualifiedLeak: array[0..31] of string = (
    'unit FreeStateSelfLeak;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    '  { A holder with an owned field. }',
    '  THolder = class(TObject)',
    '    FFoo: TFoo;',
    '    // Rebuilds the field on every pass and releases it after the loop.',
    '    procedure Cycle(aFlag: boolean);',
    '  end;',
    '',
    'implementation',
    '',
    'procedure THolder.Cycle(aFlag: boolean);',
    '',
    'begin',
    '  while aFlag do',
    '  begin',
    '    Self.FFoo := TFoo.Create;',
    '  end;',
    '  Self.FFoo.Free;',
    'end;',
    '',
    'end.');

  cFreeStateSelfQualified: array[0..31] of string = (
    'unit FreeStateSelfQual;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    '  { A holder with an owned field. }',
    '  THolder = class(TObject)',
    '    FFoo: TFoo;',
    '    // Rebuilds and releases the field on every pass.',
    '    procedure Cycle(aFlag: boolean);',
    '  end;',
    '',
    'implementation',
    '',
    'procedure THolder.Cycle(aFlag: boolean);',
    '',
    'begin',
    '  while aFlag do',
    '  begin',
    '    Self.FFoo := TFoo.Create;',
    '    Self.FFoo.Free;',
    '  end;',
    'end;',
    '',
    'end.');

  cFreeStateForeignField: array[0..38] of string = (
    'unit FreeStateForeign;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    '  { A holder with an owned field. }',
    '  THolder = class(TObject)',
    '    FFoo: TFoo;',
    '    // Fills another instance''s field on every pass.',
    '    procedure Fill(aOther: THolder; aFlag: boolean);',
    '    // Releases its own field and reads another instance''s.',
    '    procedure Drop(aOther: THolder);',
    '  end;',
    '',
    'implementation',
    '',
    'procedure THolder.Fill(aOther: THolder; aFlag: boolean);',
    '',
    'begin',
    '  while aFlag do',
    '    aOther.FFoo := TFoo.Create;',
    'end;',
    '',
    '',
    'procedure THolder.Drop(aOther: THolder);',
    '',
    'begin',
    '  FFoo.Free;',
    '  aOther.FFoo.FName := '''';',
    'end;',
    '',
    'end.');

  cFreeStateHandover: array[0..69] of string = (
    'unit FreeStateHandover;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    '  { A container that takes ownership of what it is given. }',
    '  TBag = class(TObject)',
    '    // Takes ownership of aFoo.',
    '    procedure Add(aFoo: TFoo);',
    '  end;',
    '',
    'procedure Pooled(aBag: TBag; aFlag: boolean);',
    'procedure Handed;',
    '',
    'implementation',
    '',
    'procedure Consume(aRec: PRec);',
    '',
    'begin',
    '  if aRec = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure TBag.Add(aFoo: TFoo);',
    '',
    'begin',
    '  if aFoo = nil then',
    '    Exit;',
    'end;',
    '',
    '',
    'procedure Pooled(aBag: TBag; aFlag: boolean);',
    '',
    'var',
    '  lFoo: TFoo;',
    '',
    'begin',
    '  while aFlag do',
    '  begin',
    '    lFoo := TFoo.Create;',
    '    aBag.Add(lFoo);',
    '  end;',
    'end;',
    '',
    '',
    'procedure Handed;',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  GetMem(lRec, 8);',
    '  Consume(lRec);',
    'end;',
    '',
    'end.');

  cFreeStateNestedLocal: array[0..34] of string = (
    'unit FreeStateNestedLocal;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lRec: PRec;',
    '',
    '  // Allocates the enclosing routine''s local.',
    '  procedure Alloc;',
    '',
    '  begin',
    '    New(lRec);',
    '  end;',
    '',
    'begin',
    '  Alloc;',
    '  Dispose(lRec);',
    'end;',
    '',
    'end.');

  cFreeStateConstAlias: array[0..45] of string = (
    'unit FreeStateConstAlias;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload. }',
    '  TFoo = class(TObject)',
    '    FName: string;',
    '  end;',
    '',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure Reused(aFlag: boolean);',
    'procedure Aliased;',
    '',
    'implementation',
    '',
    'procedure Reused(aFlag: boolean);',
    '',
    'const',
    '  cFoo: TFoo = nil;',
    '',
    'begin',
    '  while aFlag do',
    '    cFoo := TFoo.Create;',
    'end;',
    '',
    '',
    'procedure Aliased;',
    '',
    'var',
    '  lP: PRec;',
    '  lQ: PRec absolute lP;',
    '',
    'begin',
    '  New(lQ);',
    '  FreeMem(lP);',
    'end;',
    '',
    'end.');

  cGetMemNilStore: array[0..27] of string = (
    'unit GetMemNilStore;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  New(lRec);',
    '  lRec := nil;',
    'end;',
    '',
    'end.');

  cGetMemNilAfterBranch: array[0..30] of string = (
    'unit GetMemNilAfterBranch;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'implementation',
    '',
    'procedure Run(aFlag: boolean);',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  GetMem(lRec, 8);',
    '  if aFlag then',
    '    FreeMem(lRec);',
    '  // The exit join carries the release, which satisfies an absence rule.',
    '  lRec := nil;',
    'end;',
    '',
    'end.');

  cGetMemBranchRelease: array[0..28] of string = (
    'unit GetMemBranch;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure ReleasedOnBranch(aFlag: boolean);',
    '',
    'implementation',
    '',
    'procedure ReleasedOnBranch(aFlag: boolean);',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  GetMem(lRec, 8);',
    '  if aFlag then',
    '    FreeMem(lRec);',
    'end;',
    '',
    'end.');


  { The acquire/release fixtures share this preamble; Probe is the live-sibling
    witness and the Run body below line 50 is what each variant changes. }
  cPairNoncompliant: array[0..55] of string = (
    'unit PairNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A lock. }',
    '  TLock = class(TObject)',
    '    procedure Acquire;',
    '    procedure Release;',
    '  end;',
    '',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure Probe;',
    'procedure Run(aLock: TLock);',
    '',
    'implementation',
    '',
    'procedure TLock.Acquire;',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure TLock.Release;',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure Probe;',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  New(lRec);',
    '  FreeMem(lRec);',
    'end;',
    '',
    '',
    'procedure Run(aLock: TLock);',
    '',
    'begin',
    '  aLock.Acquire;',
    '  Probe;',
    '  aLock.Release;',
    'end;',
    '',
    'end.');

  cPairProtected: array[0..58] of string = (
    'unit PairProtected;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A lock. }',
    '  TLock = class(TObject)',
    '    procedure Acquire;',
    '    procedure Release;',
    '  end;',
    '',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure Probe;',
    'procedure Run(aLock: TLock);',
    '',
    'implementation',
    '',
    'procedure TLock.Acquire;',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure TLock.Release;',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure Probe;',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  New(lRec);',
    '  FreeMem(lRec);',
    'end;',
    '',
    '',
    'procedure Run(aLock: TLock);',
    '',
    'begin',
    '  aLock.Acquire;',
    '  try',
    '    Probe;',
    '  finally',
    '    aLock.Release;',
    '  end;',
    'end;',
    '',
    'end.');

  cPairNestedFinally: array[0..62] of string = (
    'unit PairNested;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A lock. }',
    '  TLock = class(TObject)',
    '    procedure Acquire;',
    '    procedure Release;',
    '  end;',
    '',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure Probe;',
    'procedure Run(aLock: TLock);',
    '',
    'implementation',
    '',
    'procedure TLock.Acquire;',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure TLock.Release;',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure Probe;',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  New(lRec);',
    '  FreeMem(lRec);',
    'end;',
    '',
    '',
    'procedure Run(aLock: TLock);',
    '',
    'begin',
    '  try',
    '    try',
    '      aLock.Acquire;',
    '      Probe;',
    '    finally',
    '      aLock.Release;',
    '    end;',
    '  finally',
    '    Probe;',
    '  end;',
    'end;',
    '',
    'end.');

  cPairNoRelease: array[0..54] of string = (
    'unit PairNoRelease;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A lock. }',
    '  TLock = class(TObject)',
    '    procedure Acquire;',
    '    procedure Release;',
    '  end;',
    '',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure Probe;',
    'procedure Run(aLock: TLock);',
    '',
    'implementation',
    '',
    'procedure TLock.Acquire;',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure TLock.Release;',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure Probe;',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  New(lRec);',
    '  FreeMem(lRec);',
    'end;',
    '',
    '',
    'procedure Run(aLock: TLock);',
    '',
    'begin',
    '  aLock.Acquire;',
    '  Probe;',
    'end;',
    '',
    'end.');

  cPairInlineAssembler: array[0..57] of string = (
    'unit PairOperand;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A lock. }',
    '  TLock = class(TObject)',
    '    procedure Acquire;',
    '    procedure Release;',
    '  end;',
    '',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure Probe;',
    'procedure Run(aLock: TLock);',
    '',
    'implementation',
    '',
    'procedure TLock.Acquire;',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure TLock.Release;',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure Probe;',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  New(lRec);',
    '  FreeMem(lRec);',
    'end;',
    '',
    '',
    'procedure Run(aLock: TLock);',
    '',
    'begin',
    '  aLock.Acquire;',
    '  asm',
    '    nop',
    '  end;',
    '  aLock.Release;',
    'end;',
    '',
    'end.');

  { The receiver is reached through a qualifier of its own, so nothing the
    pairing recognises names it. }
  cPairForeign: array[0..52] of string = (
    'unit PairForeign;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A lock. }',
    '  TLock = class(TObject)',
    '    procedure Lock;',
    '    procedure Unlock;',
    '  end;',
    '',
    '  { A holder. }',
    '  THolder = class(TObject)',
    '    Inner: TLock;',
    '  end;',
    '',
    '  { A payload record. }',
    '  TRec = record',
    '    FName: string;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure Run(aHolder: THolder);',
    '',
    'implementation',
    '',
    'procedure TLock.Lock;',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure TLock.Unlock;',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure Run(aHolder: THolder);',
    '',
    'var',
    '  lRec: PRec;',
    '',
    'begin',
    '  New(lRec);',
    '  aHolder.Inner.Lock;',
    '  aHolder.Inner.Unlock;',
    '  FreeMem(lRec);',
    'end;',
    '',
    'end.');

  // Two pairings in one routine: Acquire/Release on line 47, Open/Shut on 49.
  cPairTable: array[0..52] of string = (
    'unit PairTable;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A gate. }',
    '  TGate = class(TObject)',
    '    procedure Acquire;',
    '    procedure Release;',
    '    procedure Open;',
    '    procedure Shut;',
    '  end;',
    '',
    'procedure Run(aGate: TGate);',
    '',
    'implementation',
    '',
    'procedure TGate.Acquire;',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure TGate.Release;',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure TGate.Open;',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure TGate.Shut;',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure Run(aGate: TGate);',
    '',
    'begin',
    '  aGate.Acquire;',
    '  aGate.Release;',
    '  aGate.Open;',
    '  aGate.Shut;',
    'end;',
    '',
    'end.');


procedure TRulesLifetimeTest.RunRule(aRule: TRuleBase; const aFixture: string;
  const aCollector: TFpSonarIssueCollector);

begin
  RunRule(aRule, aFixture, False, aCollector);
end;


procedure TRulesLifetimeTest.RunRuleWithConfig(aRule: TRuleBase;
  const aFixture: string; const aConfig: TFpSonarConfig;
  const aCollector: TFpSonarIssueCollector);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;

begin
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    lEngine.Config := aConfig;
    lEngine.Analyze(aFixture, cMode, cDefines, aCollector);
  finally
    lEngine.Free;
    lReg.Free;
  end;
end;


function TRulesLifetimeTest.PairTargetsConfig(
  const aPatterns: array of string): TFpSonarConfig;

var
  lSetting: TFpSonarRuleSetting;
  lParam: TFpSonarRuleParam;
  i: Integer;

begin
  Result := TFpSonarConfig.Default;
  lSetting.RuleId := cUnbalancedPairId;
  lSetting.HasEnabled := False;
  lSetting.Enabled := True;
  lSetting.HasSeverity := False;
  lSetting.Severity := sevInfo;
  lParam.Key := 'targets';
  lParam.Kind := cpkTargets;
  lParam.IntVal := 0;
  lParam.StrVal := '';
  lParam.BoolVal := False;
  SetLength(lParam.Targets, Length(aPatterns));
  for i := Low(aPatterns) to High(aPatterns) do
  begin
    lParam.Targets[i].Pattern := aPatterns[i];
    lParam.Targets[i].Message := '';
    lParam.Targets[i].Severity := sevInfo;
    lParam.Targets[i].HasSeverity := False;
  end;
  SetLength(lSetting.Params, 1);
  lSetting.Params[0] := lParam;
  SetLength(Result.Rules, 1);
  Result.Rules[0] := lSetting;
end;


function TRulesLifetimeTest.PairCountWith(const aPatterns: array of string;
  const aSource: array of string; out aRow: Integer): Integer;

var
  lFix: TTempFixtures;
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  aRow := 0;
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRuleWithConfig(NewUnbalancedPair, lFix.Add('probe.pas', aSource),
        PairTargetsConfig(aPatterns), lc);
      Result := CountById(lc, cUnbalancedPairId);
      k := FirstById(lc, cUnbalancedPairId);
      if k >= 0 then
        aRow := lc.Issues[k].StartLine;
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesLifetimeTest.RunRule(aRule: TRuleBase; const aFixture: string;
  aWithhold: boolean; const aCollector: TFpSonarIssueCollector);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;

begin
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
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


function TRulesLifetimeTest.RuleCount(aRule: TRuleBase; aWithhold: boolean;
  const aSource: array of string): Integer;

var
  lFix: TTempFixtures;
  lc: TFpSonarIssueCollector;
  lId: string;

begin
  // The registry RunRule builds owns and frees aRule, so read the id first.
  lId := aRule.Metadata.RuleId;
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aRule, lFix.Add('probe.pas', aSource), aWithhold, lc);
      Result := CountById(lc, lId);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesLifetimeTest.CheckSilentWithLiveSibling(aRule,
  aSibling: TRuleBase; const aId, aSiblingId: string;
  const aSource: array of string);

var
  lFix: TTempFixtures;
  lc: TFpSonarIssueCollector;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('operand.pas', aSource);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aRule, lPath, False, lc);
      AssertEquals('an unresolved fact is silent', 0, CountById(lc, aId));
    finally
      lc.Free;
    end;
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aSibling, lPath, False, lc);
      AssertEquals('the resolver was live', 1, CountById(lc, aSiblingId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesLifetimeTest.CheckFreeStateSilent(
  const aSource: array of string);

  procedure CheckOne(aRule: TRuleBase; const aId, aPath: string);
  var
    lc: TFpSonarIssueCollector;
  begin
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aRule, aPath, False, lc);
      AssertEquals(aId + ' silent', 0, CountById(lc, aId));
    finally
      lc.Free;
    end;
  end;

var
  lFix: TTempFixtures;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('probe.pas', aSource);
    // Without this the whole group is silent on a fixture that fails to resolve.
    AssertFixtureResolves(lPath);
    CheckOne(NewUseAfterFree, cUseAfterFreeId, lPath);
    CheckOne(NewDoubleFree, cDoubleFreeId, lPath);
    CheckOne(NewFreeNotFreeAndNilOnField, cFreeNotFreeAndNilOnFieldId, lPath);
    CheckOne(NewGetMemWithoutFreeMem, cGetMemWithoutFreeMemId, lPath);
    CheckOne(NewObjectCreatedInLoopNotFreed, cObjectCreatedInLoopNotFreedId,
      lPath);
  finally
    lFix.Free;
  end;
end;


procedure TRulesLifetimeTest.AssertFixtureResolves(const aPath: string);

var
  lSrc: TFpSonarSourceFile;

begin
  lSrc := TFpSonarSourceFile.Create;
  try
    lSrc.Analyze(aPath, cMode, cDefines);
    AssertTrue('fixture parsed', lSrc.ParseSucceeded);
    AssertTrue('fixture resolved',
      (lSrc.Resolver <> nil) and lSrc.Resolver.Succeeded);
  finally
    lSrc.Free;
  end;
end;


function TRulesLifetimeTest.CountById(
  const aCollector: TFpSonarIssueCollector; const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesLifetimeTest.FirstById(
  const aCollector: TFpSonarIssueCollector; const aId: string): Integer;

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


procedure TRulesLifetimeTest.CheckStmtRuleSrc(aRule, aCompliantRule: TRuleBase;
  const aId: string; aDeclLine: Integer; const aArgs: array of string;
  const aNoncompliant, aCompliant: array of string);

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;
  k, m: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    // Noncompliant: one issue at aDeclLine, column 1, carrying aArgs as the
    // message args.
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aRule, lFix.Add('noncompliant.pas', aNoncompliant), lc);
      AssertEquals('one issue for ' + aId, 1, CountById(lc, aId));
      k := FirstById(lc, aId);
      AssertEquals('start line', aDeclLine, lc.Issues[k].StartLine);
      AssertEquals('start col', 1, lc.Issues[k].StartCol);
      AssertEquals('end line', aDeclLine, lc.Issues[k].EndLine);
      AssertEquals('end col', 1, lc.Issues[k].EndCol);
      AssertEquals('key is the dotted rule key', 'rule.' + aId + '.message',
        lc.Issues[k].MessageKey);
      AssertEquals('arg count', Length(aArgs),
        Length(lc.Issues[k].MessageArgs));
      for m := 0 to High(aArgs) do
        AssertEquals('arg ' + IntToStr(m), aArgs[m],
          lc.Issues[k].MessageArgs[m]);
    finally
      lc.Free;
    end;

    // Compliant: nothing flagged for the rule under test.
    lPath := lFix.Add('compliant.pas', aCompliant);
    AssertFixtureResolves(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aCompliantRule, lPath, lc);
      AssertEquals('compliant => zero', 0, CountById(lc, aId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


function TRulesLifetimeTest.NewFreeOnInterfaceReference: TRuleBase;

begin
  Result := TRuleFreeOnInterfaceReference.Create(TRuleMetadata.Make(
    cFreeOnInterfaceReferenceId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesLifetimeTest.NewSelfDestroyedInMethod: TRuleBase;

begin
  Result := TRuleSelfDestroyedInMethod.Create(TRuleMetadata.Make(
    cSelfDestroyedInMethodId, rtSem, rfResolver, sevMajor, itBug, cfMedium,
    True, ''));
end;


function TRulesLifetimeTest.NewNewDisposeMismatch: TRuleBase;

begin
  Result := TRuleNewDisposeMismatch.Create(TRuleMetadata.Make(
    cNewDisposeMismatchId, rtSem, rfResolver, sevCritical, itBug, cfHigh,
    True, ''));
end;


function TRulesLifetimeTest.NewOwnedFieldNotFreedInDestructor: TRuleBase;

begin
  Result := TRuleOwnedFieldNotFreedInDestructor.Create(TRuleMetadata.Make(
    cOwnedFieldNotFreedInDestructorId, rtSem, rfResolver, sevMajor, itBug,
    cfMedium, True, ''));
end;


function TRulesLifetimeTest.NewCreateWithoutTryFinally: TRuleBase;

begin
  Result := TRuleCreateWithoutTryFinally.Create(TRuleMetadata.Make(
    cCreateWithoutTryFinallyId, rtSem, rfResolver, sevMajor, itBug, cfMedium,
    True, ''));
end;


function TRulesLifetimeTest.NewExceptionObjectFreedInHandler: TRuleBase;

begin
  Result := TRuleExceptionObjectFreedInHandler.Create(TRuleMetadata.Make(
    cExceptionObjectFreedInHandlerId, rtSem, rfResolver, sevCritical, itBug,
    cfHigh, True, ''));
end;


function TRulesLifetimeTest.NewRaisedExceptionInstanceReused: TRuleBase;

begin
  Result := TRuleRaisedExceptionInstanceReused.Create(TRuleMetadata.Make(
    cRaisedExceptionInstanceReusedId, rtSem, rfResolver, sevMajor, itBug,
    cfMedium, True, ''));
end;


function TRulesLifetimeTest.NewLoopVariableUsedAfterLoop: TRuleBase;

begin
  Result := TRuleLoopVariableUsedAfterLoop.Create(TRuleMetadata.Make(
    cLoopVariableUsedAfterLoopId, rtSem, rfResolver, sevMajor, itBug, cfMedium,
    True, ''));
end;


function TRulesLifetimeTest.NewLoopVariableModifiedInBody: TRuleBase;

begin
  Result := TRuleLoopVariableModifiedInBody.Create(TRuleMetadata.Make(
    cLoopVariableModifiedInBodyId, rtAst, rfAst, sevMajor, itBug, cfMedium,
    True, ''));
end;


function TRulesLifetimeTest.NewLeakOnEarlyExit: TRuleBase;

begin
  Result := TRuleLeakOnEarlyExit.Create(TRuleMetadata.Make(
    cLeakOnEarlyExitId, rtSem, rfResolver, sevMajor, itBug, cfMedium,
    True, ''));
end;


function TRulesLifetimeTest.NewStreamNotProtected: TRuleBase;

begin
  Result := TRuleStreamNotProtected.Create(TRuleMetadata.Make(
    cStreamNotProtectedId, rtSem, rfResolver, sevMajor, itBug, cfMedium,
    True, ''));
end;


function TRulesLifetimeTest.NewUseAfterFree: TRuleBase;

begin
  Result := TRuleUseAfterFree.Create(TRuleMetadata.Make(
    cUseAfterFreeId, rtSem, rfResolver, sevMajor, itBug, cfMedium, True, ''));
end;


function TRulesLifetimeTest.NewDoubleFree: TRuleBase;

begin
  Result := TRuleDoubleFree.Create(TRuleMetadata.Make(
    cDoubleFreeId, rtSem, rfResolver, sevMajor, itBug, cfMedium, True, ''));
end;


function TRulesLifetimeTest.NewFreeNotFreeAndNilOnField: TRuleBase;

begin
  Result := TRuleFreeNotFreeAndNilOnField.Create(TRuleMetadata.Make(
    cFreeNotFreeAndNilOnFieldId, rtSem, rfResolver, sevMajor, itBug, cfMedium,
    True, ''));
end;


function TRulesLifetimeTest.NewGetMemWithoutFreeMem: TRuleBase;

begin
  Result := TRuleGetMemWithoutFreeMem.Create(TRuleMetadata.Make(
    cGetMemWithoutFreeMemId, rtSem, rfResolver, sevMajor, itBug, cfMedium,
    True, ''));
end;


function TRulesLifetimeTest.NewObjectCreatedInLoopNotFreed: TRuleBase;

begin
  Result := TRuleObjectCreatedInLoopNotFreed.Create(TRuleMetadata.Make(
    cObjectCreatedInLoopNotFreedId, rtSem, rfResolver, sevMajor, itBug,
    cfMedium, True, ''));
end;


function TRulesLifetimeTest.NewUnbalancedPair: TRuleBase;

var
  lMeta: TRuleMetadata;

begin
  lMeta := TRuleMetadata.Make(cUnbalancedPairId, rtSem, rfResolver, sevMajor,
    itBug, cfMedium, True, '');
  lMeta.AddParam('targets', rpkTargets);
  Result := TRuleUnbalancedPair.Create(lMeta);
end;


procedure TRulesLifetimeTest.FreeOnInterfaceReferencePositions;

begin
  // Noncompliant: 'FreeAndNil(lIntf)' on line 28; the arg is the interface type.
  // The compliant fixture's class-reference FreeAndNil and class->class cast are
  // load-bearing FP guards.
  CheckStmtRuleSrc(NewFreeOnInterfaceReference, NewFreeOnInterfaceReference,
    cFreeOnInterfaceReferenceId, 28, ['IFoo'],
    cFreeIntfNoncompliant, cFreeIntfCompliant);
end;


procedure TRulesLifetimeTest.FreeOnInterfaceReferenceCountsCastForm;

begin
  // 'TObject(lIntf).Free' compiles because the cast target is a class, and the
  // defect is the cast's SOURCE type.
  AssertEquals('a cast off an interface reference => one issue', 1,
    RuleCount(NewFreeOnInterfaceReference, False, cFreeIntfCastForm));
end;


procedure TRulesLifetimeTest.FreeOnInterfaceReferenceDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewFreeOnInterfaceReference, True, cFreeIntfNoncompliant));
end;


procedure TRulesLifetimeTest.FreeOnInterfaceReferenceSilentOnUnresolvedOperand;

begin
  // The disposed operand is an UNTYPED var parameter.
  CheckSilentWithLiveSibling(NewFreeOnInterfaceReference, NewNewDisposeMismatch,
    cFreeOnInterfaceReferenceId, cNewDisposeMismatchId, cFreeIntfUnresolved);
end;


procedure TRulesLifetimeTest.SelfDestroyedInMethodPositions;

begin
  // Noncompliant: 'Self.Free' in TFoo.Detach on line 19; the arg is the method.
  // The compliant fixture's non-Self operand, destructor self-free and free in a
  // plain procedure are load-bearing FP guards.
  CheckStmtRuleSrc(NewSelfDestroyedInMethod, NewSelfDestroyedInMethod,
    cSelfDestroyedInMethodId, 19, ['TFoo.Detach'],
    cSelfFreeNoncompliant, cSelfFreeCompliant);
end;


procedure TRulesLifetimeTest.SelfDestroyedInMethodCountsBareFreeForm;

begin
  // An unqualified 'Free' has no operand expression at all: only the SelfArg
  // route reaches it.
  AssertEquals('bare Free in a method => one issue', 1,
    RuleCount(NewSelfDestroyedInMethod, False, cSelfFreeBareForm));
end;


procedure TRulesLifetimeTest.SelfDestroyedInMethodDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewSelfDestroyedInMethod, True, cSelfFreeNoncompliant));
end;


procedure TRulesLifetimeTest.SelfDestroyedInMethodSilentOnUnresolvedOperand;

begin
  // The self-free sits in a routine NESTED in the method, whose own procedure
  // scope carries no Self argument.
  CheckSilentWithLiveSibling(NewSelfDestroyedInMethod, NewNewDisposeMismatch,
    cSelfDestroyedInMethodId, cNewDisposeMismatchId, cSelfFreeUnresolved);
end;


procedure TRulesLifetimeTest.NewDisposeMismatchPositions;

begin
  // Noncompliant: 'FreeMem(lRec)' on line 28 releases what New allocated; the
  // args are the operand and the two routine names.
  CheckStmtRuleSrc(NewNewDisposeMismatch, NewNewDisposeMismatch,
    cNewDisposeMismatchId, 28, ['lRec', 'New', 'FreeMem'],
    cNewMismatchNoncompliant, cNewMismatchCompliant);
end;


procedure TRulesLifetimeTest.NewDisposeMismatchCountsGetMemDisposePair;

begin
  // The mirror mismatch: GetMem released with Dispose, reported at the release.
  AssertEquals('GetMem released with Dispose => one issue', 1,
    RuleCount(NewNewDisposeMismatch, False, cNewMismatchGetMemForm));
end;


procedure TRulesLifetimeTest.NewDisposeMismatchDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewNewDisposeMismatch, True, cNewMismatchNoncompliant));
end;


procedure TRulesLifetimeTest.NewDisposeMismatchSilentOnUnresolvedOperand;

begin
  // The release binds a USER-declared 'procedure FreeMem', whose owning module
  // is the fixture unit rather than System or SysUtils.
  CheckSilentWithLiveSibling(NewNewDisposeMismatch, NewFreeOnInterfaceReference,
    cNewDisposeMismatchId, cFreeOnInterfaceReferenceId, cNewMismatchUnresolved);
end;


procedure TRulesLifetimeTest.OwnedFieldNotFreedInDestructorPositions;

begin
  // Noncompliant: FBar is constructed in TFoo.Create and TFoo.Destroy never
  // mentions it; the issue sits on the field declaration, line 15.
  CheckStmtRuleSrc(NewOwnedFieldNotFreedInDestructor,
    NewOwnedFieldNotFreedInDestructor, cOwnedFieldNotFreedInDestructorId, 15,
    ['FBar', 'TFoo.Destroy'], cOwnedFieldNoncompliant, cOwnedFieldCompliant);
end;


procedure TRulesLifetimeTest.OwnedFieldNotFreedInDestructorSilentOnHelperRelease;

begin
  // The destructor calls ReleaseAll, a method of the same class: a helper that
  // releases nothing is indistinguishable from one that does.
  CheckSilentWithLiveSibling(NewOwnedFieldNotFreedInDestructor,
    NewNewDisposeMismatch, cOwnedFieldNotFreedInDestructorId,
    cNewDisposeMismatchId, cOwnedFieldHelper);
end;


procedure TRulesLifetimeTest.OwnedFieldNotFreedInDestructorSilentOnBeforeDestruction;

begin
  CheckSilentWithLiveSibling(NewOwnedFieldNotFreedInDestructor,
    NewNewDisposeMismatch, cOwnedFieldNotFreedInDestructorId,
    cNewDisposeMismatchId, cOwnedFieldHook);
end;


procedure TRulesLifetimeTest.OwnedFieldNotFreedInDestructorSilentWithoutOwnDestructor;

begin
  // TFoo declares no destructor of its own; TBase.Destroy is the only one in
  // the module and it belongs to another class.
  CheckSilentWithLiveSibling(NewOwnedFieldNotFreedInDestructor,
    NewNewDisposeMismatch, cOwnedFieldNotFreedInDestructorId,
    cNewDisposeMismatchId, cOwnedFieldInherited);
end;


procedure TRulesLifetimeTest.OwnedFieldNotFreedInDestructorDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewOwnedFieldNotFreedInDestructor, True,
    cOwnedFieldNoncompliant));
end;


procedure TRulesLifetimeTest.OwnedFieldNotFreedInDestructorSilentOnUnresolvedOperand;

begin
  // TBar.Create is a CLASS FUNCTION, not a constructor.
  CheckSilentWithLiveSibling(NewOwnedFieldNotFreedInDestructor,
    NewNewDisposeMismatch, cOwnedFieldNotFreedInDestructorId,
    cNewDisposeMismatchId, cOwnedFieldUnresolved);
end;


procedure TRulesLifetimeTest.CreateWithoutTryFinallyPositions;

begin
  // Noncompliant: 'lFoo := TFoo.Create' on line 31, released later in the
  // routine with nothing protecting it.
  CheckStmtRuleSrc(NewCreateWithoutTryFinally, NewCreateWithoutTryFinally,
    cCreateWithoutTryFinallyId, 31, ['lFoo'],
    cCreateNoTryNoncompliant, cCreateNoTryCompliant);
end;


procedure TRulesLifetimeTest.CreateWithoutTryFinallyCountsFreeAndNilForm;

begin
  AssertEquals('a FreeAndNil release => one issue', 1,
    RuleCount(NewCreateWithoutTryFinally, False, cCreateNoTryFreeAndNilForm));
end;


procedure TRulesLifetimeTest.CreateWithoutTryFinallyCountsBranchRelease;

begin
  // The release sits in an if arm, so it is a grandchild of the acquisition's
  // statement list rather than a sibling: silent before the CFG widening.
  CheckStmtRuleSrc(NewCreateWithoutTryFinally, NewCreateWithoutTryFinally,
    cCreateWithoutTryFinallyId, 23, ['lFoo'],
    cCreateNoTryBranchForm, cCreateNoTryCompliant);
end;


procedure TRulesLifetimeTest.CreateWithoutTryFinallyCountsBranchHandover;

begin
  // Both arms acquire; only the else arm releases. The then arm's acquisition
  // is a handover and must not inherit the other arm's release.
  CheckStmtRuleSrc(NewCreateWithoutTryFinally, NewCreateWithoutTryFinally,
    cCreateWithoutTryFinallyId, 38, ['lFoo'],
    cCreateNoTryHandoverBranchForm, cCreateNoTryCompliant);
end;


procedure TRulesLifetimeTest.CreateWithoutTryFinallyCountsReacquisitionAfterRelease;

begin
  // The try..finally covers the second acquisition only: the first one is
  // released and closed before it.
  CheckStmtRuleSrc(NewCreateWithoutTryFinally, NewCreateWithoutTryFinally,
    cCreateWithoutTryFinallyId, 31, ['lFoo'],
    cCreateNoTryReacquireForm, cCreateNoTryCompliant);
end;


procedure TRulesLifetimeTest.CreateWithoutTryFinallySilentOnReleaseBeforeAcquisition;

begin
  // The only release of lFoo precedes the construction.
  CheckSilentWithLiveSibling(NewCreateWithoutTryFinally, NewNewDisposeMismatch,
    cCreateWithoutTryFinallyId, cNewDisposeMismatchId,
    cCreateNoTryReleaseFirstForm);
end;


procedure TRulesLifetimeTest.CreateWithoutTryFinallySilentOnNestedCoveringFinally;

begin
  // The acquisition sits in an if arm and the covering try..finally follows
  // the if, not the acquisition itself.
  CheckSilentWithLiveSibling(NewCreateWithoutTryFinally, NewNewDisposeMismatch,
    cCreateWithoutTryFinallyId, cNewDisposeMismatchId,
    cCreateNoTryNestedCoverForm);
end;


procedure TRulesLifetimeTest.CreateWithoutTryFinallySilentOnStreamRow;

begin
  AssertEquals('StreamNotProtected owns the row => silent', 0,
    RuleCount(NewCreateWithoutTryFinally, False, cStreamNoncompliant));
end;


procedure TRulesLifetimeTest.CreateWithoutTryFinallySilentForExitAndStreamRules;

begin
  // The matrix's same-block row: the other two ids own nothing here.
  AssertEquals('no exit path => silent', 0,
    RuleCount(NewLeakOnEarlyExit, False, cCreateNoTryNoncompliant));
  AssertEquals('not a stream class => silent', 0,
    RuleCount(NewStreamNotProtected, False, cCreateNoTryNoncompliant));
end;


procedure TRulesLifetimeTest.CreateWithoutTryFinallyDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewCreateWithoutTryFinally, True, cCreateNoTryNoncompliant));
end;


procedure TRulesLifetimeTest.CreateWithoutTryFinallySilentOnUnresolvedOperand;

begin
  // The release goes through Release(var aObj), an UNTYPED var parameter.
  CheckSilentWithLiveSibling(NewCreateWithoutTryFinally, NewNewDisposeMismatch,
    cCreateWithoutTryFinallyId, cNewDisposeMismatchId, cCreateNoTryUnresolved);
end;


procedure TRulesLifetimeTest.LeakOnEarlyExitPositions;

begin
  // Noncompliant: the 'Exit' on line 33 is reached with lFoo still owned and
  // the only release below it.
  CheckStmtRuleSrc(NewLeakOnEarlyExit, NewLeakOnEarlyExit,
    cLeakOnEarlyExitId, 33, ['lFoo'],
    cLeakExitNoncompliant, cLeakExitCompliant);
end;


procedure TRulesLifetimeTest.LeakOnEarlyExitCountsRaiseForm;

begin
  AssertEquals('a raise before the release => one issue', 1,
    RuleCount(NewLeakOnEarlyExit, False, cLeakExitRaiseForm));
end;


procedure TRulesLifetimeTest.LeakOnEarlyExitCountsUnprotectedCreateOnSameFixture;

begin
  // The matrix's early-exit row pins CreateWithoutTryFinally at the
  // acquisition as well as the leak at the Exit.
  AssertEquals('the acquisition is unprotected too => one issue', 1,
    RuleCount(NewCreateWithoutTryFinally, False, cLeakExitNoncompliant));
end;


procedure TRulesLifetimeTest.LeakOnEarlyExitSilentOnHandledRaise;

begin
  // The raise is caught by the try..except around it.
  CheckSilentWithLiveSibling(NewLeakOnEarlyExit, NewCreateWithoutTryFinally,
    cLeakOnEarlyExitId, cCreateWithoutTryFinallyId, cLeakExitHandledRaiseForm);
end;


procedure TRulesLifetimeTest.LeakOnEarlyExitSilentUnderFinally;

begin
  // Every acquisition of the fixture is covered by a finally, released nowhere
  // or released before the exit.
  AssertEquals('a covered or unreleased acquisition => silent', 0,
    RuleCount(NewLeakOnEarlyExit, False, cLeakExitCompliant));
end;


procedure TRulesLifetimeTest.LeakOnEarlyExitSilentOnUnreachableAcquisition;

begin
  { The only acquisition sits after an Exit, so it never runs. The engine still
    merges that node into the live join, so ownership must not come from it. }
  AssertEquals('an acquisition in dead code => silent', 0,
    RuleCount(NewLeakOnEarlyExit, False, cLeakExitDeadAcquire));
end;


procedure TRulesLifetimeTest.LeakOnEarlyExitDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewLeakOnEarlyExit, True, cLeakExitNoncompliant));
end;


procedure TRulesLifetimeTest.LeakOnEarlyExitSilentOnUnresolvedOperand;

begin
  // The release goes through Release(var aObj), an UNTYPED var parameter: spell
  // it lFoo.Free and the Exit above it reports.
  CheckSilentWithLiveSibling(NewLeakOnEarlyExit, NewNewDisposeMismatch,
    cLeakOnEarlyExitId, cNewDisposeMismatchId, cLeakExitUnresolved);
end;


procedure TRulesLifetimeTest.StreamNotProtectedPositions;

begin
  // Noncompliant: 'lStm := TFileStream.Create' on line 20 with no try..finally
  // covering it. The compliant fixture's guarded stream and non-stream class
  // are load-bearing FP guards.
  CheckStmtRuleSrc(NewStreamNotProtected, NewStreamNotProtected,
    cStreamNotProtectedId, 20, ['lStm'],
    cStreamNoncompliant, cStreamCompliant);
end;


procedure TRulesLifetimeTest.StreamNotProtectedSilentOnNeverReleasedStream;

begin
  // A stream the routine never releases is a handover, which every rule here
  // treats as satisfied.
  AssertEquals('a stream handed over => silent', 0,
    RuleCount(NewStreamNotProtected, False, cStreamNeverReleased));
end;


procedure TRulesLifetimeTest.StreamNotProtectedSilentOnFinallySharedByBranches;

begin
  { Both arms acquire lStm and one try..finally after the join releases it, the
    shape fpjson.pp:1268 has. The second arm must not defeat the first's
    coverage. }
  AssertEquals('one finally covering two acquiring arms => silent', 0,
    RuleCount(NewStreamNotProtected, False, cStreamSharedFinally));
end;


procedure TRulesLifetimeTest.StreamNotProtectedDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewStreamNotProtected, True, cStreamNoncompliant));
end;


procedure TRulesLifetimeTest.StreamNotProtectedSilentOnUnresolvedOperand;

begin
  // The stream is released through an UNTYPED var parameter.
  CheckSilentWithLiveSibling(NewStreamNotProtected, NewNewDisposeMismatch,
    cStreamNotProtectedId, cNewDisposeMismatchId, cStreamUnresolved);
end;


procedure TRulesLifetimeTest.ExceptionObjectFreedInHandlerPositions;

begin
  // Noncompliant: 'E.Free' inside 'on E: Exception do' on line 24; the arg is
  // the handler variable.
  CheckStmtRuleSrc(NewExceptionObjectFreedInHandler,
    NewExceptionObjectFreedInHandler, cExceptionObjectFreedInHandlerId, 24,
    ['E'], cHandlerFreeNoncompliant, cHandlerFreeCompliant);
end;


procedure TRulesLifetimeTest.ExceptionObjectFreedInHandlerCountsFreeAndNilForm;

begin
  AssertEquals('FreeAndNil on the handler variable => one issue', 1,
    RuleCount(NewExceptionObjectFreedInHandler, False, cHandlerFreeAndNilForm));
end;


procedure TRulesLifetimeTest.ExceptionObjectFreedInHandlerDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewExceptionObjectFreedInHandler, True,
    cHandlerFreeNoncompliant));
end;


procedure TRulesLifetimeTest.ExceptionObjectFreedInHandlerSilentOnUnresolvedOperand;

begin
  // The handler hands E to Sink(var aObj), an UNTYPED var parameter.
  CheckSilentWithLiveSibling(NewExceptionObjectFreedInHandler,
    NewNewDisposeMismatch, cExceptionObjectFreedInHandlerId,
    cNewDisposeMismatchId, cHandlerFreeUnresolved);
end;


procedure TRulesLifetimeTest.RaisedExceptionInstanceReusedPositions;

begin
  // Noncompliant: 'raise lErr' on line 22, with lErr read again on line 23;
  // the arg is the raised variable.
  CheckStmtRuleSrc(NewRaisedExceptionInstanceReused,
    NewRaisedExceptionInstanceReused, cRaisedExceptionInstanceReusedId, 22,
    ['lErr'], cRaiseReuseNoncompliant, cRaiseReuseCompliant);
end;


procedure TRulesLifetimeTest.RaisedExceptionInstanceReusedCountsParameterForm;

begin
  AssertEquals('a raised parameter read afterwards => one issue', 1,
    RuleCount(NewRaisedExceptionInstanceReused, False,
    cRaiseReuseParameterForm));
end;


procedure TRulesLifetimeTest.RaisedExceptionInstanceReusedSilentOnClearedHandle;

begin
  // The only later reference is 'lErr := nil' in the finally: clearing the
  // transferred handle is hygiene, and the write is excluded through PasTree.
  AssertEquals('a later write to the raised variable => silent', 0,
    RuleCount(NewRaisedExceptionInstanceReused, False, cRaiseReuseClearedForm));
end;


procedure TRulesLifetimeTest.RaisedExceptionInstanceReusedDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewRaisedExceptionInstanceReused, True,
    cRaiseReuseNoncompliant));
end;


procedure TRulesLifetimeTest.RaisedExceptionInstanceReusedSilentOnUnresolvedOperand;

begin
  // The raised operand is a FUNCTION RESULT, so the instance is not a
  // declaration whose later references can be compared.
  CheckSilentWithLiveSibling(NewRaisedExceptionInstanceReused,
    NewNewDisposeMismatch, cRaisedExceptionInstanceReusedId,
    cNewDisposeMismatchId, cRaiseReuseUnresolved);
end;


procedure TRulesLifetimeTest.LoopVariableUsedAfterLoopPositions;

begin
  // Noncompliant: lIdx read on line 20, after its loop on line 18; the arg is
  // the control variable. The compliant fixture's read inside the body, for..in
  // loop and unit-level control variable are load-bearing FP guards.
  CheckStmtRuleSrc(NewLoopVariableUsedAfterLoop, NewLoopVariableUsedAfterLoop,
    cLoopVariableUsedAfterLoopId, 20, ['lIdx'],
    cLoopAfterNoncompliant, cLoopAfterCompliant);
end;


procedure TRulesLifetimeTest.LoopVariableUsedAfterLoopSilentOnAssignmentAfterLoop;

begin
  // 'lIdx := 0' after the loop is a WRITE, and the resolver's access tag and
  // not row order is what separates it from the defect.
  AssertEquals('a write after the loop => silent', 0,
    RuleCount(NewLoopVariableUsedAfterLoop, False, cLoopAfterWriteForm));
end;


procedure TRulesLifetimeTest.LoopVariableUsedAfterLoopSilentOnCounterReuse;

begin
  // A second loop over the same counter recounts it.
  AssertEquals('a read inside a later loop over the same counter => silent', 0,
    RuleCount(NewLoopVariableUsedAfterLoop, False, cLoopAfterCounterReuseForm));
end;


procedure TRulesLifetimeTest.LoopVariableUsedAfterLoopCountsNearestLoopOnly;

begin
  // Two sequential loops over lIdx and one read after both: only the second
  // loop leaves the value behind.
  AssertEquals('a read after two loops over one counter => one issue', 1,
    RuleCount(NewLoopVariableUsedAfterLoop, False, cLoopAfterNearestForm));
end;


procedure TRulesLifetimeTest.LoopVariableUsedAfterLoopDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewLoopVariableUsedAfterLoop, True, cLoopAfterNoncompliant));
end;


procedure TRulesLifetimeTest.LoopVariableUsedAfterLoopSilentOnUnresolvedOperand;

begin
  // The post-loop lIdx resolves to a with-scoped record FIELD of the same name.
  CheckSilentWithLiveSibling(NewLoopVariableUsedAfterLoop,
    NewNewDisposeMismatch, cLoopVariableUsedAfterLoopId, cNewDisposeMismatchId,
    cLoopAfterUnresolved);
end;


procedure TRulesLifetimeTest.LoopVariableModifiedInBodyPositions;

begin
  // Noncompliant: 'lIdx := 9' on line 20 inside lIdx's own loop; the arg is the
  // control variable. That fixture does not resolve -- pasresolver rejects the
  // shape -- which is why the rule is rtAst/rfAst.
  CheckStmtRuleSrc(NewLoopVariableModifiedInBody, NewLoopVariableModifiedInBody,
    cLoopVariableModifiedInBodyId, 20, ['lIdx'],
    cLoopModNoncompliant, cLoopModCompliant);
end;


procedure TRulesLifetimeTest.LoopVariableModifiedInBodyCountsNestedLoopForm;

begin
  // The assignment sits under an INNER for loop.
  AssertEquals('an outer control variable assigned in an inner loop => one issue',
    1, RuleCount(NewLoopVariableModifiedInBody, False, cLoopModNestedForm));
end;


// SilentOnUnresolvedOperand is n/a for LoopVariableModifiedInBody: it is
// rtAst/rfAst and consults no resolved fact.
procedure TRulesLifetimeTest.LoopVariableModifiedInBodyDegradesOnParseFailure;

begin
  AssertEquals('no module => silent', 0,
    RuleCount(NewLoopVariableModifiedInBody, False, cLoopModUnparseable));
end;


procedure TRulesLifetimeTest.UseAfterFreePositions;

begin
  // Noncompliant: the release in the if arm on line 33 reaches the join.
  CheckStmtRuleSrc(NewUseAfterFree, NewUseAfterFree,
    cUseAfterFreeId, 34, ['lFoo'],
    cUseAfterFreeNoncompliant, cUseAfterFreeCompliant);
end;


procedure TRulesLifetimeTest.UseAfterFreeSilentOnFieldRow;

begin
  // A field freed and read afterwards is FreeNotFreeAndNilOnField's row.
  CheckSilentWithLiveSibling(NewUseAfterFree, NewFreeNotFreeAndNilOnField,
    cUseAfterFreeId, cFreeNotFreeAndNilOnFieldId, cFieldFreeNoncompliant);
end;


procedure TRulesLifetimeTest.UseAfterFreeDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewUseAfterFree, True, cUseAfterFreeNoncompliant));
end;


procedure TRulesLifetimeTest.UseAfterFreeSilentOnUnresolvedOperand;

begin
  // The release goes through Release(var aObj), an UNTYPED var parameter, whose
  // write of lFoo no site accounts for: spell it lFoo.Free and the read reports.
  CheckSilentWithLiveSibling(NewUseAfterFree, NewNewDisposeMismatch,
    cUseAfterFreeId, cNewDisposeMismatchId, cFreeStateUnresolved);
end;


procedure TRulesLifetimeTest.DoubleFreePositions;

begin
  // Noncompliant: the second 'lFoo.Free' on line 34 has no assignment between it
  // and the first. The compliant fixture's FreeAndNil, nil store and
  // re-acquisition between the two releases are load-bearing FP guards.
  CheckStmtRuleSrc(NewDoubleFree, NewDoubleFree,
    cDoubleFreeId, 34, ['lFoo'],
    cDoubleFreeNoncompliant, cDoubleFreeCompliant);
end;


procedure TRulesLifetimeTest.DoubleFreeDescribesAPathNotEveryPath;

begin
  // Merge is the pointwise maximum, so one released incoming path suffices.
  AssertEquals('the description states a may-analysis',
    'Flags a release of a reference that a path reaching it already released.',
    RuleRegistry.FindById(cDoubleFreeId).Metadata.Description);
end;


procedure TRulesLifetimeTest.DoubleFreeDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewDoubleFree, True, cDoubleFreeNoncompliant));
end;


procedure TRulesLifetimeTest.DoubleFreeSilentOnUnresolvedOperand;

begin
  CheckSilentWithLiveSibling(NewDoubleFree, NewNewDisposeMismatch,
    cDoubleFreeId, cNewDisposeMismatchId, cFreeStateUnresolved);
end;


procedure TRulesLifetimeTest.FreeNotFreeAndNilOnFieldPositions;

begin
  // Noncompliant: 'FFoo.Free' on line 33 leaves the field dangling and line 34
  // reads it. The compliant fixture's nilled field, field read nowhere after
  // the release and replaced field are load-bearing FP guards.
  CheckStmtRuleSrc(NewFreeNotFreeAndNilOnField, NewFreeNotFreeAndNilOnField,
    cFreeNotFreeAndNilOnFieldId, 33, ['FFoo'],
    cFieldFreeNoncompliant, cFieldFreeCompliant);
end;


procedure TRulesLifetimeTest.FreeNotFreeAndNilOnFieldSilentOnNilledField;

begin
  // FreeAndNil on a field and on a local, both read afterwards: nil is not
  // dangling.
  CheckFreeStateSilent(cFieldNilledForm);
end;


procedure TRulesLifetimeTest.FreeNotFreeAndNilOnFieldDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewFreeNotFreeAndNilOnField, True, cFieldFreeNoncompliant));
end;


procedure TRulesLifetimeTest.FreeNotFreeAndNilOnFieldSilentOnUnresolvedOperand;

begin
  CheckSilentWithLiveSibling(NewFreeNotFreeAndNilOnField, NewNewDisposeMismatch,
    cFreeNotFreeAndNilOnFieldId, cNewDisposeMismatchId, cFreeStateUnresolved);
end;


procedure TRulesLifetimeTest.GetMemWithoutFreeMemPositions;

begin
  // Noncompliant: the 'GetMem' on line 24 is the routine's only allocation and
  // no path releases it. The compliant fixture's paired release, release on one
  // path and two allocators are load-bearing FP guards.
  CheckStmtRuleSrc(NewGetMemWithoutFreeMem, NewGetMemWithoutFreeMem,
    cGetMemWithoutFreeMemId, 24, ['lRec'],
    cGetMemNoncompliant, cGetMemCompliant);
end;


procedure TRulesLifetimeTest.GetMemWithoutFreeMemCountsNilledPointer;

begin
  // Nilling a pointer neither allocates nor frees.
  CheckStmtRuleSrc(NewGetMemWithoutFreeMem, NewGetMemWithoutFreeMem,
    cGetMemWithoutFreeMemId, 24, ['lRec'],
    cGetMemNilStore, cGetMemNilAfterBranch);
end;


procedure TRulesLifetimeTest.GetMemWithoutFreeMemSilentOnBranchRelease;

begin
  { The fixture holds the branch-release shape alone: it releases on one path
    only, and because Merge is the maximum the exit state is released. }
  AssertEquals('a release on any path => silent', 0,
    RuleCount(NewGetMemWithoutFreeMem, False, cGetMemBranchRelease));
end;


procedure TRulesLifetimeTest.GetMemWithoutFreeMemDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewGetMemWithoutFreeMem, True, cGetMemNoncompliant));
end;


procedure TRulesLifetimeTest.GetMemWithoutFreeMemSilentOnUnresolvedOperand;

begin
  CheckSilentWithLiveSibling(NewGetMemWithoutFreeMem, NewNewDisposeMismatch,
    cGetMemWithoutFreeMemId, cNewDisposeMismatchId, cFreeStateUnresolved);
end;


procedure TRulesLifetimeTest.ObjectCreatedInLoopNotFreedPositions;

begin
  // Noncompliant: the create on line 33 is reached by the back edge with the
  // previous iteration's instance still owned, the release sitting after the
  // loop.
  CheckStmtRuleSrc(NewObjectCreatedInLoopNotFreed,
    NewObjectCreatedInLoopNotFreed,
    cObjectCreatedInLoopNotFreedId, 33, ['lFoo'],
    cLoopCreateNoncompliant, cLoopCreateCompliant);
end;


procedure TRulesLifetimeTest.ObjectCreatedInLoopNotFreedCountsSelfQualifiedField;

begin
  { A field qualified by Self is the field of the instance under analysis, so the
    create on line 27 is tracked; the compliant fixture releases it in the body. }
  CheckStmtRuleSrc(NewObjectCreatedInLoopNotFreed,
    NewObjectCreatedInLoopNotFreed,
    cObjectCreatedInLoopNotFreedId, 27, ['FFoo'],
    cFreeStateSelfQualifiedLeak, cFreeStateSelfQualified);
end;


procedure TRulesLifetimeTest.ObjectCreatedInLoopNotFreedCountsOtherLoopKinds;

begin
  { A repeat and a for body are loops the verdict names as well as a while, one
    leaking instance each. }
  AssertEquals('repeat and for bodies report', 2,
    RuleCount(NewObjectCreatedInLoopNotFreed, False, cLoopCreateOtherKinds));
end;


procedure TRulesLifetimeTest.ObjectCreatedInLoopNotFreedSilentOnReleaseInBody;

begin
  // A release in the body makes the back edge carry a released state.
  AssertEquals('a release inside the loop body => silent', 0,
    RuleCount(NewObjectCreatedInLoopNotFreed, False, cLoopCreateBodyRelease));
end;


procedure TRulesLifetimeTest.ObjectCreatedInLoopNotFreedSilentOnStraightLineReacquire;

begin
  // Two acquisitions in sequence leak the first one just as a loop does, but
  // the id names a loop.
  CheckSilentWithLiveSibling(NewObjectCreatedInLoopNotFreed,
    NewCreateWithoutTryFinally, cObjectCreatedInLoopNotFreedId,
    cCreateWithoutTryFinallyId, cLoopCreateStraightLine);
end;


procedure TRulesLifetimeTest.ObjectCreatedInLoopNotFreedDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewObjectCreatedInLoopNotFreed, True, cLoopCreateNoncompliant));
end;


procedure TRulesLifetimeTest.ObjectCreatedInLoopNotFreedSilentOnUnresolvedOperand;

begin
  CheckSilentWithLiveSibling(NewObjectCreatedInLoopNotFreed,
    NewNewDisposeMismatch, cObjectCreatedInLoopNotFreedId,
    cNewDisposeMismatchId, cFreeStateUnresolved);
end;


procedure TRulesLifetimeTest.FreeStateRulesSilentOnNilledReference;

begin
  // FreeAndNil leaves the reference nil, so a later read is not a use after free
  // and a later release is a no-op.
  CheckFreeStateSilent(cUseAfterFreeCompliant);
  CheckFreeStateSilent(cDoubleFreeCompliant);
end;


procedure TRulesLifetimeTest.FreeStateRulesSilentOnInterveningWrite;

begin
  // 'lFoo := aOther' is a write no recognised site accounts for.
  CheckFreeStateSilent(cFreeStateInterveningWrite);
end;


procedure TRulesLifetimeTest.FreeStateRulesSilentOnAddressEscape;

begin
  CheckFreeStateSilent(cFreeStateAddressEscape);
end;


procedure TRulesLifetimeTest.FreeStateRulesSilentOnAbsoluteAlias;

begin
  CheckFreeStateSilent(cFreeStateAbsoluteAlias);
end;


procedure TRulesLifetimeTest.FreeStateRulesSilentOnConstAndAbsoluteDeclaration;

begin
  { A writable typed constant is not a variable slot, and a declaration carrying
    an absolute clause aliases another one's storage. }
  CheckFreeStateSilent(cFreeStateConstAlias);
end;


procedure TRulesLifetimeTest.FreeStateRulesSilentOnSelfQualifiedRelease;

begin
  // 'Self.FFoo.Free' releases what 'Self.FFoo := TFoo.Create' acquired.
  CheckFreeStateSilent(cFreeStateSelfQualified);
end;


procedure TRulesLifetimeTest.FreeStateRulesSilentOnForeignQualifiedField;

begin
  { A field reached through another instance says nothing about the field of the
    instance under analysis, so the declaration leaves the population. }
  CheckFreeStateSilent(cFreeStateForeignField);
end;


procedure TRulesLifetimeTest.FreeStateRulesSilentOnCallHandover;

begin
  // A call the classifier does not recognise may take over the reference.
  CheckFreeStateSilent(cFreeStateHandover);
end;


procedure TRulesLifetimeTest.FreeStateRulesSilentOnStoreHandover;

begin
  // A store of the reference elsewhere may take over the release.
  CheckFreeStateSilent(cFreeStateStoreHandover);
end;


procedure TRulesLifetimeTest.FreeStateRulesSilentOnEnclosingRoutineLocal;

begin
  // The nested routine allocates a local of Run, which Run itself releases.
  CheckFreeStateSilent(cFreeStateNestedLocal);
end;


procedure TRulesLifetimeTest.FreeStateRulesSilentOnNestedRoutineMention;

begin
  CheckFreeStateSilent(cFreeStateNestedMention);
end;


procedure TRulesLifetimeTest.FreeStateRulesSilentOnInlineAssembler;

begin
  // The asm statement cannot be classified, which takes the whole routine out.
  CheckFreeStateSilent(cFreeStateAsmBody);
end;


procedure TRulesLifetimeTest.FreeStateRulesSilentOnUnreachableSite;

begin
  { The release after the Exit never runs. The engine still merges that node into
    the live join, so the release must not reach the read below it. }
  CheckFreeStateSilent(cFreeStateDeadSite);
end;


procedure TRulesLifetimeTest.ResourceRulesUnmovedByTheNilledState;

begin
  // The three rules of the ownership lattice keep their counts, and the added
  // state gives the free-state rules nothing to say on their fixtures.
  AssertEquals('CreateWithoutTryFinally unmoved', 1,
    RuleCount(NewCreateWithoutTryFinally, False, cCreateNoTryNoncompliant));
  AssertEquals('LeakOnEarlyExit unmoved', 1,
    RuleCount(NewLeakOnEarlyExit, False, cLeakExitNoncompliant));
  AssertEquals('StreamNotProtected unmoved', 1,
    RuleCount(NewStreamNotProtected, False, cStreamNoncompliant));
  CheckFreeStateSilent(cCreateNoTryNoncompliant);
  CheckFreeStateSilent(cLeakExitNoncompliant);
  CheckFreeStateSilent(cStreamNoncompliant);
end;


procedure TRulesLifetimeTest.UnbalancedPairPositions;

begin
  // Noncompliant: 'aLock.Acquire;' on line 51, released on 53 outside a
  // finally.
  CheckStmtRuleSrc(NewUnbalancedPair, NewUnbalancedPair, cUnbalancedPairId,
    51, ['aLock', 'Release'], cPairNoncompliant, cPairProtected);
end;


procedure TRulesLifetimeTest.UnbalancedPairSilentOnProtectedPair;

begin
  CheckSilentWithLiveSibling(NewUnbalancedPair, NewNewDisposeMismatch,
    cUnbalancedPairId, cNewDisposeMismatchId, cPairProtected);
end;


procedure TRulesLifetimeTest.UnbalancedPairSilentOnNestedCoveringFinally;

begin
  CheckSilentWithLiveSibling(NewUnbalancedPair, NewNewDisposeMismatch,
    cUnbalancedPairId, cNewDisposeMismatchId, cPairNestedFinally);
end;


procedure TRulesLifetimeTest.UnbalancedPairSilentWithoutARelease;

begin
  CheckSilentWithLiveSibling(NewUnbalancedPair, NewNewDisposeMismatch,
    cUnbalancedPairId, cNewDisposeMismatchId, cPairNoRelease);
end;


procedure TRulesLifetimeTest.UnbalancedPairSilentOnForeignQualifier;

begin
  CheckSilentWithLiveSibling(NewUnbalancedPair, NewNewDisposeMismatch,
    cUnbalancedPairId, cNewDisposeMismatchId, cPairForeign);
end;


procedure TRulesLifetimeTest.UnbalancedPairSilentOnMalformedTableEntry;

var
  lRow: Integer;

begin
  AssertEquals('an entry that is no pair configures nothing', 0,
    PairCountWith(['Acquire'], cPairTable, lRow));
  AssertEquals('a sound entry beside it still applies', 1,
    PairCountWith(['Acquire', 'Open/Shut'], cPairTable, lRow));
  AssertEquals('and it fires on its own acquire row', 49, lRow);
end;


procedure TRulesLifetimeTest.UnbalancedPairFollowsTheConfiguredTable;

var
  lFix: TTempFixtures;
  lc: TFpSonarIssueCollector;
  lRow, k: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewUnbalancedPair, lFix.Add('probe.pas', cPairTable), lc);
      AssertEquals('the default table pairs Acquire with Release', 1,
        CountById(lc, cUnbalancedPairId));
      k := FirstById(lc, cUnbalancedPairId);
      AssertEquals('at the Acquire row', 47, lc.Issues[k].StartLine);
      AssertEquals('two message args', 2, Length(lc.Issues[k].MessageArgs));
      AssertEquals('naming the receiver', 'aGate',
        lc.Issues[k].MessageArgs[0]);
      AssertEquals('naming the release method', 'Release',
        lc.Issues[k].MessageArgs[1]);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
  AssertEquals('a configured table is the only vocabulary', 1,
    PairCountWith(['Open/Shut'], cPairTable, lRow));
  AssertEquals('at the Open row, the default pair now silent', 49, lRow);
end;


procedure TRulesLifetimeTest.UnbalancedPairDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewUnbalancedPair, True, cPairNoncompliant));
end;


procedure TRulesLifetimeTest.UnbalancedPairSilentOnInlineAssembler;

begin
  CheckSilentWithLiveSibling(NewUnbalancedPair, NewNewDisposeMismatch,
    cUnbalancedPairId, cNewDisposeMismatchId, cPairInlineAssembler);
end;


procedure TRulesLifetimeTest.ResourceRulesUnmovedByTheGeneralisedRelease;

begin
  // The three rules read the same recogniser with an empty method.
  CheckStmtRuleSrc(NewCreateWithoutTryFinally, NewCreateWithoutTryFinally,
    cCreateWithoutTryFinallyId, 31, ['lFoo'],
    cCreateNoTryNoncompliant, cCreateNoTryCompliant);
  CheckStmtRuleSrc(NewLeakOnEarlyExit, NewLeakOnEarlyExit,
    cLeakOnEarlyExitId, 33, ['lFoo'],
    cLeakExitNoncompliant, cLeakExitCompliant);
  CheckStmtRuleSrc(NewStreamNotProtected, NewStreamNotProtected,
    cStreamNotProtectedId, 20, ['lStm'],
    cStreamNoncompliant, cStreamCompliant);
  AssertEquals('the pair rule claims none of their rows', 0,
    RuleCount(NewUnbalancedPair, False, cCreateNoTryNoncompliant));
end;


procedure TRulesLifetimeTest.LifetimeRulesSelfRegisterGlobally;

begin
  // The production initialization registered all seventeen lifetime rules into
  // the global registry.
  AssertTrue('FreeOnInterfaceReference registered',
    RuleRegistry.FindById(cFreeOnInterfaceReferenceId) <> nil);
  AssertFalse('FreeOnInterfaceReference ships disabled',
    RuleRegistry.FindById(cFreeOnInterfaceReferenceId).Metadata.DefaultEnabled);
  AssertTrue('SelfDestroyedInMethod registered',
    RuleRegistry.FindById(cSelfDestroyedInMethodId) <> nil);
  AssertFalse('SelfDestroyedInMethod ships disabled',
    RuleRegistry.FindById(cSelfDestroyedInMethodId).Metadata.DefaultEnabled);
  AssertTrue('NewDisposeMismatch registered',
    RuleRegistry.FindById(cNewDisposeMismatchId) <> nil);
  AssertFalse('NewDisposeMismatch ships disabled',
    RuleRegistry.FindById(cNewDisposeMismatchId).Metadata.DefaultEnabled);
  AssertTrue('OwnedFieldNotFreedInDestructor registered',
    RuleRegistry.FindById(cOwnedFieldNotFreedInDestructorId) <> nil);
  AssertFalse('OwnedFieldNotFreedInDestructor ships disabled',
    RuleRegistry.FindById(
    cOwnedFieldNotFreedInDestructorId).Metadata.DefaultEnabled);
  AssertTrue('CreateWithoutTryFinally registered',
    RuleRegistry.FindById(cCreateWithoutTryFinallyId) <> nil);
  AssertFalse('CreateWithoutTryFinally ships disabled',
    RuleRegistry.FindById(cCreateWithoutTryFinallyId).Metadata.DefaultEnabled);
  AssertTrue('ExceptionObjectFreedInHandler registered',
    RuleRegistry.FindById(cExceptionObjectFreedInHandlerId) <> nil);
  AssertFalse('ExceptionObjectFreedInHandler ships disabled',
    RuleRegistry.FindById(
    cExceptionObjectFreedInHandlerId).Metadata.DefaultEnabled);
  AssertTrue('RaisedExceptionInstanceReused registered',
    RuleRegistry.FindById(cRaisedExceptionInstanceReusedId) <> nil);
  AssertFalse('RaisedExceptionInstanceReused ships disabled',
    RuleRegistry.FindById(
    cRaisedExceptionInstanceReusedId).Metadata.DefaultEnabled);
  AssertTrue('LoopVariableUsedAfterLoop registered',
    RuleRegistry.FindById(cLoopVariableUsedAfterLoopId) <> nil);
  AssertFalse('LoopVariableUsedAfterLoop ships disabled',
    RuleRegistry.FindById(cLoopVariableUsedAfterLoopId).Metadata.DefaultEnabled);
  AssertTrue('LoopVariableModifiedInBody registered',
    RuleRegistry.FindById(cLoopVariableModifiedInBodyId) <> nil);
  AssertFalse('LoopVariableModifiedInBody ships disabled',
    RuleRegistry.FindById(
    cLoopVariableModifiedInBodyId).Metadata.DefaultEnabled);
  AssertTrue('LeakOnEarlyExit registered',
    RuleRegistry.FindById(cLeakOnEarlyExitId) <> nil);
  AssertFalse('LeakOnEarlyExit ships disabled',
    RuleRegistry.FindById(cLeakOnEarlyExitId).Metadata.DefaultEnabled);
  AssertTrue('StreamNotProtected registered',
    RuleRegistry.FindById(cStreamNotProtectedId) <> nil);
  AssertFalse('StreamNotProtected ships disabled',
    RuleRegistry.FindById(cStreamNotProtectedId).Metadata.DefaultEnabled);
  AssertTrue('UseAfterFree registered',
    RuleRegistry.FindById(cUseAfterFreeId) <> nil);
  AssertFalse('UseAfterFree ships disabled',
    RuleRegistry.FindById(cUseAfterFreeId).Metadata.DefaultEnabled);
  AssertTrue('DoubleFree registered',
    RuleRegistry.FindById(cDoubleFreeId) <> nil);
  AssertFalse('DoubleFree ships disabled',
    RuleRegistry.FindById(cDoubleFreeId).Metadata.DefaultEnabled);
  AssertTrue('FreeNotFreeAndNilOnField registered',
    RuleRegistry.FindById(cFreeNotFreeAndNilOnFieldId) <> nil);
  AssertFalse('FreeNotFreeAndNilOnField ships disabled',
    RuleRegistry.FindById(
    cFreeNotFreeAndNilOnFieldId).Metadata.DefaultEnabled);
  AssertTrue('GetMemWithoutFreeMem registered',
    RuleRegistry.FindById(cGetMemWithoutFreeMemId) <> nil);
  AssertFalse('GetMemWithoutFreeMem ships disabled',
    RuleRegistry.FindById(cGetMemWithoutFreeMemId).Metadata.DefaultEnabled);
  AssertTrue('ObjectCreatedInLoopNotFreed registered',
    RuleRegistry.FindById(cObjectCreatedInLoopNotFreedId) <> nil);
  AssertFalse('ObjectCreatedInLoopNotFreed ships disabled',
    RuleRegistry.FindById(
    cObjectCreatedInLoopNotFreedId).Metadata.DefaultEnabled);
  AssertTrue('UnbalancedPair registered',
    RuleRegistry.FindById(cUnbalancedPairId) <> nil);
  AssertFalse('UnbalancedPair ships disabled',
    RuleRegistry.FindById(cUnbalancedPairId).Metadata.DefaultEnabled);
end;


initialization
  RegisterTest(TRulesLifetimeTest);

end.
