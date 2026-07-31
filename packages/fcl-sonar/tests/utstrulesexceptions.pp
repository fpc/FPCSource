{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the exception-structure (AST) rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesExceptions;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Rules.Exceptions, UtstFixtures;

type
  { AST-tier exception-rule position + registration tests. }
  TRulesExceptionsTest = class(TTestCase)
  private
    // Runs aRule over aFixture, collecting issues into aCollector.
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      const aCollector: TFpSonarIssueCollector); overload;
    // aWithhold runs the degraded pass: real-RTL chain, no unit paths.
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      aWithhold: boolean; const aCollector: TFpSonarIssueCollector); overload;
    // How often aRule fires on aSource, staged as its own fixture.
    function RuleCount(aRule: TRuleBase; aWithhold: boolean;
      const aSource: array of string): Integer;
    // Asserts aRule is silent on aSource while aSibling fires once on it.
    procedure CheckSilentWithLiveSibling(aRule, aSibling: TRuleBase;
      const aId, aSiblingId: string; const aSource: array of string);
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
    // every factory, including the four rules that ship disabled.
    function NewNoEmptyFinally: TRuleBase;
    function NewExceptionsNotSwallowed: TRuleBase;
    function NewNoExplicitReRaise: TRuleBase;
    function NewExitInsideFinally: TRuleBase;
    function NewRaiseInsideFinally: TRuleBase;
    function NewHandlerOrderShadowsDerived: TRuleBase;
    function NewTryFinallyAcquireOutsideTry: TRuleBase;
    function NewExceptionClassNotDerivedFromException: TRuleBase;
    function NewEmptyTryBody: TRuleBase;
    function NewRaiseInDestructor: TRuleBase;
    function NewAssertUsedForControlFlow: TRuleBase;
  published
    procedure NoEmptyFinallyPositions;
    procedure ExceptionsNotSwallowedPositions;
    procedure NoExplicitReRaisePositions;
    procedure ExitInsideFinallyPositions;
    procedure ExitInsideFinallyCountsCallForm;
    procedure ExitInsideFinallyReportsNestedFinallyOnce;
    procedure ExitInsideFinallyDegradesWithoutResolver;
    procedure ExitInsideFinallySilentOnUnresolvedOperand;
    procedure RaiseInsideFinallyPositions;
    procedure RaiseInsideFinallyDegradesOnParseFailure;
    procedure HandlerOrderShadowsDerivedPositions;
    procedure HandlerOrderShadowsDerivedCountsDuplicateHandler;
    procedure HandlerOrderShadowsDerivedDegradesWithoutResolver;
    procedure HandlerOrderShadowsDerivedSilentOnUnresolvedOperand;
    procedure TryFinallyAcquireOutsideTryPositions;
    procedure TryFinallyAcquireOutsideTryCountsFreeAndNil;
    procedure TryFinallyAcquireOutsideTryDegradesWithoutResolver;
    procedure TryFinallyAcquireOutsideTrySilentOnUnresolvedOperand;
    procedure ExceptionClassNotDerivedFromExceptionPositions;
    procedure ExceptionClassNotDerivedFromExceptionDegradesWithoutResolver;
    procedure ExceptionClassNotDerivedFromExceptionSilentOnUnresolvedOperand;
    procedure EmptyTryBodyPositions;
    procedure EmptyTryBodyCountsExceptHandler;
    procedure EmptyTryBodyDegradesOnParseFailure;
    procedure RaiseInDestructorPositions;
    procedure RaiseInDestructorCountsHandlerReRaise;
    procedure RaiseInDestructorCountsRaiseInTryFinally;
    procedure RaiseInDestructorSkipsClassDestructor;
    procedure RaiseInDestructorDegradesOnParseFailure;
    procedure AssertUsedForControlFlowPositions;
    procedure AssertUsedForControlFlowDegradesWithoutResolver;
    procedure AssertUsedForControlFlowSilentOnUnresolvedOperand;
    procedure RulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cDefines: array[0..3] of string = ('FPC', 'CPUX86_64', 'UNIX', 'LINUX');
  cNoEmptyFinallyId = 'NoEmptyFinally';
  cExceptionsNotSwallowedId = 'ExceptionsNotSwallowed';
  cNoExplicitReRaiseId = 'NoExplicitReRaise';
  cExitInsideFinallyId = 'ExitInsideFinally';
  cRaiseInsideFinallyId = 'RaiseInsideFinally';
  cHandlerOrderShadowsDerivedId = 'HandlerOrderShadowsDerived';
  cTryFinallyAcquireOutsideTryId = 'TryFinallyAcquireOutsideTry';
  cExceptionClassNotDerivedFromExceptionId =
    'ExceptionClassNotDerivedFromException';
  cEmptyTryBodyId = 'EmptyTryBody';
  cRaiseInDestructorId = 'RaiseInDestructor';
  cAssertUsedForControlFlowId = 'AssertUsedForControlFlow';


  // Embedded exception-rule fixtures: line i+1 == [i].

  cNoEmptyFinallyNoncompliant: array[0..19] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  try',
    '    WriteLn(''work'');',
    '  finally',
    '  end;',
    'end;',
    '',
    'end.');

  cNoEmptyFinallyCompliant: array[0..26] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  // A non-empty finally => #5 stays silent.',
    '  try',
    '    WriteLn(''work'');',
    '  finally',
    '    WriteLn(''cleanup'');',
    '  end;',
    '  // An empty except must NOT trip #5 (this rule never touches except handlers).',
    '  try',
    '    WriteLn(''work'');',
    '  except',
    '  end;',
    'end;',
    '',
    'end.');

  cExceptionsNotSwallowedNoncompliant: array[0..19] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  try',
    '    WriteLn(''work'');',
    '  except',
    '  end;',
    'end;',
    '',
    'end.');

  cExceptionsNotSwallowedCompliant: array[0..33] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  // A non-empty except => #49 stays silent.',
    '  try',
    '    WriteLn(''work'');',
    '  except',
    '    WriteLn(''handled'');',
    '  end;',
    '  // An on-handler makes the except block non-empty => #49 stays silent.',
    '  try',
    '    WriteLn(''work'');',
    '  except',
    '    on E: Exception do',
    '      WriteLn(''handled'');',
    '  end;',
    '  // An empty finally must NOT trip #49 (this rule never touches finally).',
    '  try',
    '    WriteLn(''work'');',
    '  finally',
    '  end;',
    'end;',
    '',
    'end.');

  cNoExplicitReRaiseNoncompliant: array[0..21] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  try',
    '    WriteLn(''work'');',
    '  except',
    '    on E: Exception do',
    '      raise E;',
    '  end;',
    'end;',
    '',
    'end.');

  cNoExplicitReRaiseCompliant: array[0..38] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  // A bare `raise;` preserves the original stack => #21 stays silent.',
    '  try',
    '    WriteLn(''work'');',
    '  except',
    '    on E: Exception do',
    '      raise;',
    '  end;',
    '  // Raising a different exception is not a re-raise of the caught var => silent.',
    '  try',
    '    WriteLn(''work'');',
    '  except',
    '    on E: Exception do',
    '      raise EInOutError.Create(''boom'');',
    '  end;',
    '  // A no-variable `on` handler (VarEl = nil) has nothing to compare against =>',
    '  // silent. Load-bearing FP guard: without the VarEl<>nil guard the rule would',
    '  // dereference a nil catch variable.',
    '  try',
    '    WriteLn(''work'');',
    '  except',
    '    on Exception do',
    '      WriteLn(''handled'');',
    '  end;',
    'end;',
    '',
    'end.');

  // Resolution dies on WriteLn under the synthetic RTL.

  cExitInsideFinallyNoncompliant: array[0..23] of string = (
    'unit ExitNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  Classes;',
    '',
    'procedure Run(aList: TStringList);',
    '',
    'implementation',
    '',
    'procedure Run(aList: TStringList);',
    '',
    'begin',
    '  try',
    '    aList.Add(''work'');',
    '  finally',
    '    Exit;',
    '  end;',
    'end;',
    '',
    'end.');

  cExitInsideFinallyCompliant: array[0..31] of string = (
    'unit ExitOk;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  Classes;',
    '',
    'procedure Run(aList: TStringList);',
    '',
    'implementation',
    '',
    'procedure Run(aList: TStringList);',
    '',
    'begin',
    '  // An exit in the TRY body runs before the finally => silent.',
    '  try',
    '    Exit;',
    '  finally',
    '    aList.Add(''cleanup'');',
    '  end;',
    '  // An exit AFTER the try sits inside no finally => silent.',
    '  try',
    '    aList.Add(''work'');',
    '  finally',
    '    aList.Add(''cleanup'');',
    '  end;',
    '  Exit;',
    'end;',
    '',
    'end.');

  cExitInsideFinallyCallForm: array[0..24] of string = (
    'unit ExitCall;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  Classes;',
    '',
    'function Run(aList: TStringList): Integer;',
    '',
    'implementation',
    '',
    'function Run(aList: TStringList): Integer;',
    '',
    'begin',
    '  Result := 1;',
    '  try',
    '    aList.Add(''work'');',
    '  finally',
    '    Exit(0);',
    '  end;',
    'end;',
    '',
    'end.');

  cExitInsideFinallyNestedFinally: array[0..27] of string = (
    'unit ExitNested;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  Classes;',
    '',
    'procedure Run(aList: TStringList);',
    '',
    'implementation',
    '',
    'procedure Run(aList: TStringList);',
    '',
    'begin',
    '  try',
    '    aList.Add(''outer'');',
    '  finally',
    '    try',
    '      aList.Add(''inner'');',
    '    finally',
    '      Exit;',
    '    end;',
    '  end;',
    'end;',
    '',
    'end.');

  cExitInsideFinallyUnresolved: array[0..39] of string = (
    'unit ExitOperand;',
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
    '// A user-declared Exit, which is not the built-in control-flow procedure.',
    'procedure Exit;',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lFoo: TStringList;',
    '',
    'begin',
    '  try',
    '    lFoo := TStringList.Create;',
    '    lFoo.Add(''work'');',
    '  finally',
    '    Exit;',
    '  end;',
    '  try',
    '    lFoo := TStringList.Create;',
    '  finally',
    '    lFoo.Free;',
    '  end;',
    'end;',
    '',
    'end.');

  cRaiseInsideFinallyNoncompliant: array[0..23] of string = (
    'unit RaiseNon;',
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
    'begin',
    '  try',
    '    raise EInOutError.Create(''original'');',
    '  finally',
    '    raise EInOutError.Create(''replacement'');',
    '  end;',
    'end;',
    '',
    'end.');

  cRaiseInsideFinallyCompliant: array[0..37] of string = (
    'unit RaiseOk;',
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
    '  lDone: Boolean;',
    '',
    'begin',
    '  lDone := False;',
    '  // A bare raise in an except handler is the normal re-raise => silent.',
    '  try',
    '    raise EInOutError.Create(''boom'');',
    '  except',
    '    raise;',
    '  end;',
    '  // The raise sits in the try body, not in the finally => silent.',
    '  try',
    '    raise EInOutError.Create(''boom'');',
    '  finally',
    '    lDone := True;',
    '  end;',
    '  // A raise after the try sits inside no finally => silent.',
    '  if lDone then',
    '    raise EInOutError.Create(''after'');',
    'end;',
    '',
    'end.');

  cRaiseInsideFinallyUnparseable: array[0..27] of string = (
    'unit RaiseBroken;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  TWidget = class(TObject',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  try',
    '    Sleep(0);',
    '  finally',
    '    raise EInOutError.Create(''boom'');',
    '  end;',
    'end;',
    '',
    'end.');

  cHandlerOrderNoncompliant: array[0..36] of string = (
    'unit HandlerNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { Raised when a value is rejected. }',
    '  EFoo = class(Exception);',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lCode: Integer;',
    '',
    'begin',
    '  lCode := 0;',
    '  try',
    '    raise EFoo.Create(''boom'');',
    '  except',
    '    on Exception do',
    '      lCode := 1;',
    '    on EFoo do',
    '      lCode := 2;',
    '  end;',
    '  if lCode = 0 then',
    '    lCode := -1;',
    'end;',
    '',
    'end.');

  cHandlerOrderCompliant: array[0..58] of string = (
    'unit HandlerOk;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { Raised when a value falls outside the accepted range. }',
    '  ERangeRejected = class(Exception);',
    '',
    '  { Raised when a value cannot be interpreted at all. }',
    '  EValueUnreadable = class(Exception);',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lCode: Integer;',
    '',
    'begin',
    '  lCode := 0;',
    '  // Derived first, ancestor second: the correct ordering => silent.',
    '  try',
    '    raise ERangeRejected.Create(''boom'');',
    '  except',
    '    on ERangeRejected do',
    '      lCode := 1;',
    '    on Exception do',
    '      lCode := 2;',
    '  end;',
    '  // Unrelated siblings shadow nothing => silent.',
    '  try',
    '    raise EValueUnreadable.Create(''boom'');',
    '  except',
    '    on ERangeRejected do',
    '      lCode := 3;',
    '    on EValueUnreadable do',
    '      lCode := 4;',
    '  end;',
    '  // A bare catch-all after a typed handler is not an on-handler => silent.',
    '  try',
    '    raise ERangeRejected.Create(''boom'');',
    '  except',
    '    on ERangeRejected do',
    '      lCode := 5;',
    '    else',
    '      lCode := 6;',
    '  end;',
    '  if lCode = 0 then',
    '    lCode := -1;',
    'end;',
    '',
    'end.');

  cHandlerOrderDuplicate: array[0..36] of string = (
    'unit HandlerDup;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { Raised when a value is rejected. }',
    '  EFoo = class(Exception);',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lCode: Integer;',
    '',
    'begin',
    '  lCode := 0;',
    '  try',
    '    raise EFoo.Create(''boom'');',
    '  except',
    '    on EFoo do',
    '      lCode := 1;',
    '    on EFoo do',
    '      lCode := 2;',
    '  end;',
    '  if lCode = 0 then',
    '    lCode := -1;',
    'end;',
    '',
    'end.');

  cHandlerOrderUnresolved: array[0..40] of string = (
    'unit HandlerOperand;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils, Classes;',
    '',
    'type',
    '  { Declares no ancestor, so its inheritance link is never written down. }',
    '  TWidget = class',
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
    '  lFoo: TStringList;',
    '',
    'begin',
    '  try',
    '    lFoo := nil;',
    '  except',
    '    on E: TObject do',
    '      lFoo := nil;',
    '    on X: TWidget do',
    '      lFoo := nil;',
    '  end;',
    '  try',
    '    lFoo := TStringList.Create;',
    '  finally',
    '    lFoo.Free;',
    '  end;',
    'end;',
    '',
    'end.');

  cAcquireOutsideTryNoncompliant: array[0..27] of string = (
    'unit AcqNon;',
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
    '  lFoo: TStringList;',
    '',
    'begin',
    '  try',
    '    lFoo := TStringList.Create;',
    '    lFoo.Add(''work'');',
    '  finally',
    '    lFoo.Free;',
    '  end;',
    'end;',
    '',
    'end.');

  cAcquireOutsideTryCompliant: array[0..44] of string = (
    'unit AcqOk;',
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
    '  lFoo: TStringList;',
    '  lOther: TStringList;',
    '',
    'begin',
    '  // Acquired BEFORE the try, the compliant shape => silent.',
    '  lFoo := TStringList.Create;',
    '  try',
    '    lFoo.Add(''work'');',
    '  finally',
    '    lFoo.Free;',
    '  end;',
    '  // The finally releases a different variable => silent.',
    '  lOther := nil;',
    '  try',
    '    lFoo := TStringList.Create;',
    '  finally',
    '    lOther.Free;',
    '  end;',
    '  lFoo.Free;',
    '  // DW-212 false negative: acquired in the try, but nested in an if.',
    '  try',
    '    if lOther = nil then',
    '      lFoo := TStringList.Create;',
    '  finally',
    '    lFoo.Free;',
    '  end;',
    'end;',
    '',
    'end.');

  cAcquireOutsideTryFreeAndNil: array[0..26] of string = (
    'unit AcqNil;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils, Classes;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lFoo: TStringList;',
    '',
    'begin',
    '  try',
    '    lFoo := TStringList.Create;',
    '  finally',
    '    FreeAndNil(lFoo);',
    '  end;',
    'end;',
    '',
    'end.');

  cAcquireOutsideTryUnresolved: array[0..46] of string = (
    'unit AcqOperand;',
    '',
    '{$mode objfpc}{$H+}',
    '{$modeswitch advancedrecords}',
    '',
    'interface',
    '',
    'type',
    '  { A record with a Free method, whose owner is not a TObject class. }',
    '  TSlot = record',
    '    FName: string;',
    '    // Clears the slot''s payload.',
    '    procedure Free;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure TSlot.Free;',
    '',
    'begin',
    '  FName := '''';',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lSlot: TSlot;',
    '  lSource: TSlot;',
    '',
    'begin',
    '  lSource.FName := ''work'';',
    '  try',
    '    lSlot := lSource;',
    '  finally',
    '    lSlot.Free;',
    '  end;',
    '  try',
    '    lSlot.FName := ''more'';',
    '  finally',
    '    Exit;',
    '  end;',
    'end;',
    '',
    'end.');

  cRaisedClassNoncompliant: array[0..35] of string = (
    'unit RaiseClassNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { A payload holder with no exception ancestry. }',
    '  TPlain = class(TObject)',
    '    FName: string;',
    '    // Names the payload.',
    '    constructor Create(const aName: string);',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'constructor TPlain.Create(const aName: string);',
    '',
    'begin',
    '  inherited Create;',
    '  FName := aName;',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'begin',
    '  raise TPlain.Create(''boom'');',
    'end;',
    '',
    'end.');

  cRaisedClassCompliant: array[0..40] of string = (
    'unit RaiseClassOk;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { Raised when a value is rejected. }',
    '  EFoo = class(Exception);',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lErr: EFoo;',
    '',
    'begin',
    '  // An Exception descendant is the compliant shape => silent.',
    '  try',
    '    raise EFoo.Create(''boom'');',
    '  except',
    '    // A bare re-raise constructs no class => silent.',
    '    raise;',
    '  end;',
    '  // The root Exception is NoRaiseRawException''s row => silent.',
    '  try',
    '    raise Exception.Create(''root'');',
    '  except',
    '    lErr := EFoo.Create(''kept'');',
    '    // A raised variable is not the construction form => silent.',
    '    raise lErr;',
    '  end;',
    'end;',
    '',
    'end.');

  cRaisedClassUnresolved: array[0..45] of string = (
    'unit RaiseClassOperand;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  Classes;',
    '',
    'type',
    '  { A payload holder with no exception ancestry. }',
    '  TPlain = class(TObject)',
    '    FName: string;',
    '    // Names the payload.',
    '    constructor Create(const aName: string);',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'constructor TPlain.Create(const aName: string);',
    '',
    'begin',
    '  inherited Create;',
    '  FName := aName;',
    'end;',
    '',
    '',
    'procedure Run;',
    '',
    'var',
    '  lErr: TPlain;',
    '  lFoo: TStringList;',
    '',
    'begin',
    '  lErr := TPlain.Create(''boom'');',
    '  raise lErr;',
    '  try',
    '    lFoo := TStringList.Create;',
    '  finally',
    '    lFoo.Free;',
    '  end;',
    'end;',
    '',
    'end.');

  cEmptyTryBodyNoncompliant: array[0..19] of string = (
    'unit EmptyTryNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  try',
    '  finally',
    '    WriteLn(''cleanup'');',
    '  end;',
    'end;',
    '',
    'end.');

  cEmptyTryBodyCompliant: array[0..29] of string = (
    'unit EmptyTryOk;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  // A non-empty try body leaves the handler something to guard => silent.',
    '  try',
    '    WriteLn(''work'');',
    '  finally',
    '    WriteLn(''cleanup'');',
    '  end;',
    '  // Body and finally both empty: NoEmptyFinally owns that row => silent.',
    '  try',
    '  finally',
    '  end;',
    '  // Body and except both empty: ExceptionsNotSwallowed owns it => silent.',
    '  try',
    '  except',
    '  end;',
    'end;',
    '',
    'end.');

  cEmptyTryBodyExceptHandler: array[0..19] of string = (
    'unit EmptyTryExcept;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  try',
    '  except',
    '    WriteLn(''handled'');',
    '  end;',
    'end;',
    '',
    'end.');

  cEmptyTryBodyUnparseable: array[0..23] of string = (
    'unit EmptyTryBroken;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  try',
    '  finally',
    '    WriteLn(''cleanup'');',
    '  end;',
    'end;',
    '',
    'end.');

  cRaiseInDestructorNoncompliant: array[0..26] of string = (
    'unit DestructorNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { Refuses to be destroyed. }',
    '  TFoo = class(TObject)',
    '  public',
    '    // Releases the instance.',
    '    destructor Destroy; override;',
    '  end;',
    '',
    'implementation',
    '',
    'destructor TFoo.Destroy;',
    '',
    'begin',
    '  raise EInOutError.Create(''boom'');',
    '  inherited Destroy;',
    'end;',
    '',
    'end.');

  cRaiseInDestructorCompliant: array[0..44] of string = (
    'unit DestructorOk;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { Names itself and refuses construction. }',
    '  TFoo = class(TObject)',
    '  private',
    '    FName: string;',
    '  public',
    '    // Builds the instance.',
    '    constructor Create;',
    '    // Releases the instance.',
    '    destructor Destroy; override;',
    '  end;',
    '',
    'implementation',
    '',
    'constructor TFoo.Create;',
    '',
    'begin',
    '  inherited Create;',
    '  // A raise in a CONSTRUCTOR is not this rule''s shape => silent.',
    '  raise EInOutError.Create(''ctor'');',
    'end;',
    '',
    '',
    'destructor TFoo.Destroy;',
    '',
    'begin',
    '  // An except handler in the destructor stops the raise escaping => silent.',
    '  try',
    '    raise EInOutError.Create(''guarded'');',
    '  except',
    '    FName := '''';',
    '  end;',
    '  inherited Destroy;',
    'end;',
    '',
    'end.');

  cRaiseInDestructorHandlerReRaise: array[0..32] of string = (
    'unit DestructorReRaise;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { Re-raises whatever its cleanup fails on. }',
    '  TFoo = class(TObject)',
    '  private',
    '    FName: string;',
    '  public',
    '    // Releases the instance.',
    '    destructor Destroy; override;',
    '  end;',
    '',
    'implementation',
    '',
    'destructor TFoo.Destroy;',
    '',
    'begin',
    '  try',
    '    FName := '''';',
    '  except',
    '    raise;',
    '  end;',
    '  inherited Destroy;',
    'end;',
    '',
    'end.');

  cRaiseInDestructorTryFinally: array[0..32] of string = (
    'unit DestructorFinally;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { Releases its name whatever the raise does. }',
    '  TFoo = class(TObject)',
    '  private',
    '    FName: string;',
    '  public',
    '    // Releases the instance.',
    '    destructor Destroy; override;',
    '  end;',
    '',
    'implementation',
    '',
    'destructor TFoo.Destroy;',
    '',
    'begin',
    '  try',
    '    raise EInOutError.Create(''boom'');',
    '  finally',
    '    FName := '''';',
    '  end;',
    '  inherited Destroy;',
    'end;',
    '',
    'end.');

  cRaiseInDestructorClassDestructor: array[0..37] of string = (
    'unit DestructorClass;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { Raises from both its finalizer and its destructor. }',
    '  TFoo = class(TObject)',
    '  private',
    '    class var FTotal: Integer;',
    '  public',
    '    // Clears the running total.',
    '    class destructor Teardown;',
    '    // Releases the instance.',
    '    destructor Destroy; override;',
    '  end;',
    '',
    'implementation',
    '',
    'class destructor TFoo.Teardown;',
    '',
    'begin',
    '  raise EInOutError.Create(''finalizer'');',
    'end;',
    '',
    '',
    'destructor TFoo.Destroy;',
    '',
    'begin',
    '  raise EInOutError.Create(''boom'');',
    '  inherited Destroy;',
    'end;',
    '',
    'end.');

  cRaiseInDestructorUnparseable: array[0..23] of string = (
    'unit DestructorBroken;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  TFoo = class(TObject',
    '  public',
    '    destructor Destroy; override;',
    '  end;',
    '',
    'implementation',
    '',
    'destructor TFoo.Destroy;',
    '',
    'begin',
    '  raise EInOutError.Create(''boom'');',
    'end;',
    '',
    'end.');

  cAssertControlFlowNoncompliant: array[0..24] of string = (
    'unit AssertFlowNon;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run(aValue: Integer);',
    '',
    'implementation',
    '',
    '// Adds one to aValue.',
    'function Prepare(aValue: Integer): Integer;',
    '',
    'begin',
    '  Result := aValue + 1;',
    'end;',
    '',
    '',
    'procedure Run(aValue: Integer);',
    '',
    'begin',
    '  Assert(Prepare(aValue) > 0);',
    'end;',
    '',
    'end.');

  cAssertControlFlowCompliant: array[0..33] of string = (
    'unit AssertFlowOk;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run(aValue: Integer);',
    '',
    'implementation',
    '',
    '// Renders aValue as text.',
    'function Describe(aValue: Integer): string;',
    '',
    'begin',
    '  Result := ''value'';',
    'end;',
    '',
    '',
    'procedure Run(aValue: Integer);',
    '',
    'var',
    '  lText: string;',
    '',
    'begin',
    '  // A plain comparison calls nothing => silent.',
    '  Assert(aValue > 0, ''aValue must be positive'');',
    '  // Only argument 0 is examined, so a call in the MESSAGE is not the defect.',
    '  Assert(aValue > 0, Describe(aValue));',
    '  // A compiler built-in is not a user-declared routine => silent.',
    '  lText := ''text'';',
    '  Assert(Length(lText) > 0, ''lText must not be empty'');',
    'end;',
    '',
    'end.');

  cAssertControlFlowUnresolved: array[0..42] of string = (
    'unit AssertFlowOperand;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  Classes;',
    '',
    'procedure Run(aValue: Integer);',
    '',
    'implementation',
    '',
    '// Adds one to aValue.',
    'function Prepare(aValue: Integer): Integer;',
    '',
    'begin',
    '  Result := aValue + 1;',
    'end;',
    '',
    '',
    '// A user-declared Assert, which is not the compiler built-in.',
    'procedure Assert(aCondition: Boolean);',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure Run(aValue: Integer);',
    '',
    'var',
    '  lFoo: TStringList;',
    '',
    'begin',
    '  Assert(Prepare(aValue) > 0);',
    '  try',
    '    lFoo := TStringList.Create;',
    '  finally',
    '    lFoo.Free;',
    '  end;',
    'end;',
    '',
    'end.');

procedure TRulesExceptionsTest.RunRule(aRule: TRuleBase; const aFixture: string;
  const aCollector: TFpSonarIssueCollector);

begin
  RunRule(aRule, aFixture, False, aCollector);
end;


procedure TRulesExceptionsTest.RunRule(aRule: TRuleBase; const aFixture: string;
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


function TRulesExceptionsTest.RuleCount(aRule: TRuleBase; aWithhold: boolean;
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


procedure TRulesExceptionsTest.CheckSilentWithLiveSibling(aRule,
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


function TRulesExceptionsTest.CountById(
  const aCollector: TFpSonarIssueCollector; const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesExceptionsTest.FirstById(
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


procedure TRulesExceptionsTest.CheckStmtRuleSrc(aRule, aCompliantRule: TRuleBase;
  const aId: string; aDeclLine: Integer; const aArgs: array of string;
  const aNoncompliant, aCompliant: array of string);

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
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
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aCompliantRule, lFix.Add('compliant.pas', aCompliant), lc);
      AssertEquals('compliant => zero', 0, CountById(lc, aId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


function TRulesExceptionsTest.NewNoEmptyFinally: TRuleBase;

begin
  Result := TRuleNoEmptyFinally.Create(TRuleMetadata.Make(cNoEmptyFinallyId, rtAst,
    rfAst, sevMajor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesExceptionsTest.NewExceptionsNotSwallowed: TRuleBase;

begin
  Result := TRuleExceptionsNotSwallowed.Create(TRuleMetadata.Make(
    cExceptionsNotSwallowedId, rtAst, rfAst, sevMajor, itBug, cfHigh, True, ''));
end;


function TRulesExceptionsTest.NewNoExplicitReRaise: TRuleBase;

begin
  Result := TRuleNoExplicitReRaise.Create(TRuleMetadata.Make(cNoExplicitReRaiseId,
    rtAst, rfAst, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesExceptionsTest.NewExitInsideFinally: TRuleBase;

begin
  Result := TRuleExitInsideFinally.Create(TRuleMetadata.Make(
    cExitInsideFinallyId, rtSem, rfResolver, sevMajor, itBug, cfHigh, True, ''));
end;


function TRulesExceptionsTest.NewRaiseInsideFinally: TRuleBase;

begin
  Result := TRuleRaiseInsideFinally.Create(TRuleMetadata.Make(
    cRaiseInsideFinallyId, rtAst, rfAst, sevMajor, itBug, cfHigh, True, ''));
end;


function TRulesExceptionsTest.NewHandlerOrderShadowsDerived: TRuleBase;

begin
  Result := TRuleHandlerOrderShadowsDerived.Create(TRuleMetadata.Make(
    cHandlerOrderShadowsDerivedId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesExceptionsTest.NewTryFinallyAcquireOutsideTry: TRuleBase;

begin
  Result := TRuleTryFinallyAcquireOutsideTry.Create(TRuleMetadata.Make(
    cTryFinallyAcquireOutsideTryId, rtSem, rfResolver, sevMinor, itCodeSmell,
    cfLow, True, ''));
end;


function TRulesExceptionsTest.NewExceptionClassNotDerivedFromException: TRuleBase;

begin
  Result := TRuleExceptionClassNotDerivedFromException.Create(
    TRuleMetadata.Make(cExceptionClassNotDerivedFromExceptionId, rtSem,
    rfResolver, sevMajor, itBug, cfHigh, True, ''));
end;


function TRulesExceptionsTest.NewEmptyTryBody: TRuleBase;

begin
  Result := TRuleEmptyTryBody.Create(TRuleMetadata.Make(cEmptyTryBodyId, rtAst,
    rfAst, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesExceptionsTest.NewRaiseInDestructor: TRuleBase;

begin
  Result := TRuleRaiseInDestructor.Create(TRuleMetadata.Make(
    cRaiseInDestructorId, rtAst, rfAst, sevMajor, itBug, cfHigh, True, ''));
end;


function TRulesExceptionsTest.NewAssertUsedForControlFlow: TRuleBase;

begin
  Result := TRuleAssertUsedForControlFlow.Create(TRuleMetadata.Make(
    cAssertUsedForControlFlowId, rtSem, rfResolver, sevMajor, itBug, cfLow,
    True, ''));
end;


procedure TRulesExceptionsTest.NoEmptyFinallyPositions;

begin
  // Noncompliant: the empty 'finally' (line 16, CLI-probed); no args. The
  // compliant fixture's empty except is a load-bearing FP guard that must NOT
  // trip NoEmptyFinally.
  CheckStmtRuleSrc(NewNoEmptyFinally, NewNoEmptyFinally, cNoEmptyFinallyId, 16, [],
    cNoEmptyFinallyNoncompliant, cNoEmptyFinallyCompliant);
end;


procedure TRulesExceptionsTest.ExceptionsNotSwallowedPositions;

begin
  // Noncompliant: the empty 'except' (line 16, CLI-probed); no args. The
  // compliant fixture covers a non-empty except, an on-handled except, and an
  // empty finally that must NOT trip ExceptionsNotSwallowed.
  CheckStmtRuleSrc(NewExceptionsNotSwallowed, NewExceptionsNotSwallowed,
    cExceptionsNotSwallowedId, 16, [],
    cExceptionsNotSwallowedNoncompliant, cExceptionsNotSwallowedCompliant);
end;


procedure TRulesExceptionsTest.NoExplicitReRaisePositions;

begin
  // Noncompliant: 'raise E;' inside 'on E: Exception do' (line 18, CLI-probed);
  // arg is the catch-variable name. The compliant fixture covers a bare
  // 'raise;' and a raise of a different exception (load-bearing FP guards).
  CheckStmtRuleSrc(NewNoExplicitReRaise, NewNoExplicitReRaise, cNoExplicitReRaiseId,
    18, ['E'],
    cNoExplicitReRaiseNoncompliant, cNoExplicitReRaiseCompliant);
end;


procedure TRulesExceptionsTest.ExitInsideFinallyPositions;

begin
  // Noncompliant: the bare 'Exit;' in the finally (line 20, CLI-probed); no
  // args. The compliant fixture's exit in the try body and exit after the try
  // are load-bearing FP guards.
  CheckStmtRuleSrc(NewExitInsideFinally, NewExitInsideFinally,
    cExitInsideFinallyId, 20, [],
    cExitInsideFinallyNoncompliant, cExitInsideFinallyCompliant);
end;


procedure TRulesExceptionsTest.ExitInsideFinallyCountsCallForm;

begin
  // 'Exit(0)' binds to the same built-in as the bare form.
  AssertEquals('Exit(value) in a finally => one issue', 1,
    RuleCount(NewExitInsideFinally, False, cExitInsideFinallyCallForm));
end;


procedure TRulesExceptionsTest.ExitInsideFinallyReportsNestedFinallyOnce;

begin
  // The exit belongs to its nearest enclosing finally.
  AssertEquals('exit in a nested finally => one issue', 1,
    RuleCount(NewExitInsideFinally, False, cExitInsideFinallyNestedFinally));
end;


procedure TRulesExceptionsTest.ExitInsideFinallyDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewExitInsideFinally, True, cExitInsideFinallyNoncompliant));
end;


procedure TRulesExceptionsTest.ExitInsideFinallySilentOnUnresolvedOperand;

begin
  // The finally calls a USER-declared 'procedure Exit'.
  CheckSilentWithLiveSibling(NewExitInsideFinally,
    NewTryFinallyAcquireOutsideTry, cExitInsideFinallyId,
    cTryFinallyAcquireOutsideTryId, cExitInsideFinallyUnresolved);
end;


procedure TRulesExceptionsTest.RaiseInsideFinallyPositions;

begin
  // Noncompliant: the raise in the finally (line 20, CLI-probed); no args. The
  // raise in the same fixture's try body must NOT count, and the compliant
  // fixture adds a bare re-raise and a raise after the try.
  CheckStmtRuleSrc(NewRaiseInsideFinally, NewRaiseInsideFinally,
    cRaiseInsideFinallyId, 20, [],
    cRaiseInsideFinallyNoncompliant, cRaiseInsideFinallyCompliant);
end;


// SilentOnUnresolvedOperand is n/a for RaiseInsideFinally: it is rtAst/rfAst
// and consults no resolved fact.
procedure TRulesExceptionsTest.RaiseInsideFinallyDegradesOnParseFailure;

begin
  // The AST tier is what this rule reads, so a failed parse is its degradation.
  AssertEquals('no module => silent', 0,
    RuleCount(NewRaiseInsideFinally, False, cRaiseInsideFinallyUnparseable));
end;


procedure TRulesExceptionsTest.HandlerOrderShadowsDerivedPositions;

begin
  // Noncompliant: 'on EFoo do' (line 30, CLI-probed) behind 'on Exception do';
  // args are the shadowed class and the ancestor the earlier handler catches.
  CheckStmtRuleSrc(NewHandlerOrderShadowsDerived, NewHandlerOrderShadowsDerived,
    cHandlerOrderShadowsDerivedId, 30, ['EFoo', 'Exception'],
    cHandlerOrderNoncompliant, cHandlerOrderCompliant);
end;


procedure TRulesExceptionsTest.HandlerOrderShadowsDerivedCountsDuplicateHandler;

begin
  // Two handlers for the SAME class: identity counts as shadowed.
  AssertEquals('duplicate handler => one issue', 1,
    RuleCount(NewHandlerOrderShadowsDerived, False, cHandlerOrderDuplicate));
end;


procedure TRulesExceptionsTest.HandlerOrderShadowsDerivedDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewHandlerOrderShadowsDerived, True, cHandlerOrderNoncompliant));
end;


procedure TRulesExceptionsTest.HandlerOrderShadowsDerivedSilentOnUnresolvedOperand;

begin
  // TWidget writes no ancestor, so its resolved ancestor chain never reaches
  // TObject and the query withholds whether the earlier handler shadows it.
  CheckSilentWithLiveSibling(NewHandlerOrderShadowsDerived,
    NewTryFinallyAcquireOutsideTry, cHandlerOrderShadowsDerivedId,
    cTryFinallyAcquireOutsideTryId, cHandlerOrderUnresolved);
end;


procedure TRulesExceptionsTest.TryFinallyAcquireOutsideTryPositions;

begin
  // Noncompliant: the acquisition assignment inside the try (line 21,
  // CLI-probed); arg is the resource variable.
  CheckStmtRuleSrc(NewTryFinallyAcquireOutsideTry,
    NewTryFinallyAcquireOutsideTry, cTryFinallyAcquireOutsideTryId, 21,
    ['lFoo'],
    cAcquireOutsideTryNoncompliant, cAcquireOutsideTryCompliant);
end;


procedure TRulesExceptionsTest.TryFinallyAcquireOutsideTryCountsFreeAndNil;

begin
  // FreeAndNil is the lfkFreeAndNil arm of TryFreeCall.
  AssertEquals('FreeAndNil in the finally => one issue', 1,
    RuleCount(NewTryFinallyAcquireOutsideTry, False,
    cAcquireOutsideTryFreeAndNil));
end;


procedure TRulesExceptionsTest.TryFinallyAcquireOutsideTryDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewTryFinallyAcquireOutsideTry, True,
    cAcquireOutsideTryNoncompliant));
end;


procedure TRulesExceptionsTest.TryFinallyAcquireOutsideTrySilentOnUnresolvedOperand;

begin
  // The finally calls TSlot.Free, whose owner is a record and not a TObject
  // class.
  CheckSilentWithLiveSibling(NewTryFinallyAcquireOutsideTry,
    NewExitInsideFinally, cTryFinallyAcquireOutsideTryId,
    cExitInsideFinallyId, cAcquireOutsideTryUnresolved);
end;


procedure TRulesExceptionsTest.ExceptionClassNotDerivedFromExceptionPositions;

begin
  // Noncompliant: 'raise TPlain.Create' (line 33, CLI-probed); arg is the
  // constructed class. The compliant fixture's EFoo descendant, bare re-raise,
  // root Exception and raised variable are load-bearing FP guards.
  CheckStmtRuleSrc(NewExceptionClassNotDerivedFromException,
    NewExceptionClassNotDerivedFromException,
    cExceptionClassNotDerivedFromExceptionId, 33, ['TPlain'],
    cRaisedClassNoncompliant, cRaisedClassCompliant);
end;


procedure TRulesExceptionsTest.ExceptionClassNotDerivedFromExceptionDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewExceptionClassNotDerivedFromException, True,
    cRaisedClassNoncompliant));
end;


procedure TRulesExceptionsTest.ExceptionClassNotDerivedFromExceptionSilentOnUnresolvedOperand;

begin
  // The raised expression is a VARIABLE, so the query recovers no constructed
  // class and abstains although the raised object is a TPlain.
  CheckSilentWithLiveSibling(NewExceptionClassNotDerivedFromException,
    NewTryFinallyAcquireOutsideTry, cExceptionClassNotDerivedFromExceptionId,
    cTryFinallyAcquireOutsideTryId, cRaisedClassUnresolved);
end;


// SilentOnUnresolvedOperand is n/a for EmptyTryBody: it is rtAst/rfAst and
// consults no resolved fact.
procedure TRulesExceptionsTest.EmptyTryBodyPositions;

begin
  // Noncompliant: the empty try body (line 14, CLI-probed); no args. The
  // compliant fixture's non-empty body, empty finally and empty except are
  // load-bearing FP guards, the last two owned by the neighbouring rules.
  CheckStmtRuleSrc(NewEmptyTryBody, NewEmptyTryBody, cEmptyTryBodyId, 14, [],
    cEmptyTryBodyNoncompliant, cEmptyTryBodyCompliant);
end;


procedure TRulesExceptionsTest.EmptyTryBodyCountsExceptHandler;

begin
  // A non-empty except guards nothing exactly as a non-empty finally does.
  AssertEquals('empty body with an except handler => one issue', 1,
    RuleCount(NewEmptyTryBody, False, cEmptyTryBodyExceptHandler));
end;


procedure TRulesExceptionsTest.EmptyTryBodyDegradesOnParseFailure;

begin
  // The AST tier is what this rule reads, so a failed parse is its degradation.
  AssertEquals('no module => silent', 0,
    RuleCount(NewEmptyTryBody, False, cEmptyTryBodyUnparseable));
end;


// SilentOnUnresolvedOperand is n/a for RaiseInDestructor: it is rtAst/rfAst and
// consults no resolved fact.
procedure TRulesExceptionsTest.RaiseInDestructorPositions;

begin
  // Noncompliant: the raise in TFoo.Destroy (line 23, CLI-probed); arg is the
  // destructor. The compliant fixture's guarded raise and constructor raise are
  // load-bearing FP guards.
  CheckStmtRuleSrc(NewRaiseInDestructor, NewRaiseInDestructor,
    cRaiseInDestructorId, 23, ['TFoo.Destroy'],
    cRaiseInDestructorNoncompliant, cRaiseInDestructorCompliant);
end;


procedure TRulesExceptionsTest.RaiseInDestructorCountsHandlerReRaise;

begin
  // A bare re-raise sits INSIDE the handler, so nothing catches what it throws
  // and it leaves the destructor exactly as an unguarded raise does.
  AssertEquals('bare re-raise in the handler => one issue', 1,
    RuleCount(NewRaiseInDestructor, False, cRaiseInDestructorHandlerReRaise));
end;


procedure TRulesExceptionsTest.RaiseInDestructorCountsRaiseInTryFinally;

begin
  // A finally handler runs but catches nothing.
  AssertEquals('raise in a try..finally body => one issue', 1,
    RuleCount(NewRaiseInDestructor, False, cRaiseInDestructorTryFinally));
end;


procedure TRulesExceptionsTest.RaiseInDestructorSkipsClassDestructor;

begin
  // The instance destructor's raise is the one issue; the class destructor's is
  // excluded, and the fixture holds a raise in each so the 1 proves both.
  AssertEquals('class destructor excluded, instance destructor reported', 1,
    RuleCount(NewRaiseInDestructor, False, cRaiseInDestructorClassDestructor));
end;


procedure TRulesExceptionsTest.RaiseInDestructorDegradesOnParseFailure;

begin
  AssertEquals('no module => silent', 0,
    RuleCount(NewRaiseInDestructor, False, cRaiseInDestructorUnparseable));
end;


procedure TRulesExceptionsTest.AssertUsedForControlFlowPositions;

begin
  // Noncompliant: 'Assert(Prepare(aValue) > 0)' (line 22, CLI-probed); arg is
  // the called routine. The compliant fixture's plain comparison, message-
  // argument call and built-in-only assert are load-bearing FP guards.
  CheckStmtRuleSrc(NewAssertUsedForControlFlow, NewAssertUsedForControlFlow,
    cAssertUsedForControlFlowId, 22, ['Prepare'],
    cAssertControlFlowNoncompliant, cAssertControlFlowCompliant);
end;


procedure TRulesExceptionsTest.AssertUsedForControlFlowDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewAssertUsedForControlFlow, True,
    cAssertControlFlowNoncompliant));
end;


procedure TRulesExceptionsTest.AssertUsedForControlFlowSilentOnUnresolvedOperand;

begin
  // The call binds a USER-declared 'procedure Assert', which carries no
  // built-in proc data.
  CheckSilentWithLiveSibling(NewAssertUsedForControlFlow,
    NewTryFinallyAcquireOutsideTry, cAssertUsedForControlFlowId,
    cTryFinallyAcquireOutsideTryId, cAssertControlFlowUnresolved);
end;


procedure TRulesExceptionsTest.RulesSelfRegisterGlobally;

begin
  // The production initialization registered all eleven exception rules into
  // the global registry.
  AssertTrue('NoEmptyFinally registered',
    RuleRegistry.FindById(cNoEmptyFinallyId) <> nil);
  AssertTrue('ExceptionsNotSwallowed registered',
    RuleRegistry.FindById(cExceptionsNotSwallowedId) <> nil);
  AssertTrue('NoExplicitReRaise registered',
    RuleRegistry.FindById(cNoExplicitReRaiseId) <> nil);
  AssertTrue('ExitInsideFinally registered',
    RuleRegistry.FindById(cExitInsideFinallyId) <> nil);
  AssertFalse('ExitInsideFinally ships disabled',
    RuleRegistry.FindById(cExitInsideFinallyId).Metadata.DefaultEnabled);
  AssertTrue('RaiseInsideFinally registered',
    RuleRegistry.FindById(cRaiseInsideFinallyId) <> nil);
  AssertFalse('RaiseInsideFinally ships disabled',
    RuleRegistry.FindById(cRaiseInsideFinallyId).Metadata.DefaultEnabled);
  AssertTrue('HandlerOrderShadowsDerived registered',
    RuleRegistry.FindById(cHandlerOrderShadowsDerivedId) <> nil);
  AssertFalse('HandlerOrderShadowsDerived ships disabled',
    RuleRegistry.FindById(cHandlerOrderShadowsDerivedId).Metadata.DefaultEnabled);
  AssertTrue('TryFinallyAcquireOutsideTry registered',
    RuleRegistry.FindById(cTryFinallyAcquireOutsideTryId) <> nil);
  AssertFalse('TryFinallyAcquireOutsideTry ships disabled',
    RuleRegistry.FindById(cTryFinallyAcquireOutsideTryId).Metadata.DefaultEnabled);
  AssertTrue('ExceptionClassNotDerivedFromException registered',
    RuleRegistry.FindById(cExceptionClassNotDerivedFromExceptionId) <> nil);
  AssertFalse('ExceptionClassNotDerivedFromException ships disabled',
    RuleRegistry.FindById(
    cExceptionClassNotDerivedFromExceptionId).Metadata.DefaultEnabled);
  AssertTrue('EmptyTryBody registered',
    RuleRegistry.FindById(cEmptyTryBodyId) <> nil);
  AssertFalse('EmptyTryBody ships disabled',
    RuleRegistry.FindById(cEmptyTryBodyId).Metadata.DefaultEnabled);
  AssertTrue('RaiseInDestructor registered',
    RuleRegistry.FindById(cRaiseInDestructorId) <> nil);
  AssertFalse('RaiseInDestructor ships disabled',
    RuleRegistry.FindById(cRaiseInDestructorId).Metadata.DefaultEnabled);
  AssertTrue('AssertUsedForControlFlow registered',
    RuleRegistry.FindById(cAssertUsedForControlFlowId) <> nil);
  AssertFalse('AssertUsedForControlFlow ships disabled',
    RuleRegistry.FindById(cAssertUsedForControlFlowId).Metadata.DefaultEnabled);
end;


initialization
  RegisterTest(TRulesExceptionsTest);

end.
