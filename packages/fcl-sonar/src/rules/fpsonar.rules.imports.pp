{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Uses-clause semantic analysis rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.Imports;

{ Uses-clause SEM rules (rtSem / rfResolver): fully-qualify a shadowable
  import; move an implementation-only unit out of the interface uses. }

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  Pascal.Tree,
{$ELSE}
  PasTree,
{$ENDIF}
  FpSonar.Types, FpSonar.Issues,
  FpSonar.RuleFramework, FpSonar.Resolver, FpSonar.Rules.Consts;

type
  { Flags an unqualified reference to a shadowable cross-unit symbol. }
  TRuleFullyQualifiedImports = class(TRuleBase)
  public
    // Emits one issue per shadowable unqualified cross-unit reference.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an interface import used only in the implementation. }
  TRuleMoveImportToImplementation = class(TRuleBase)
  public
    // Emits one issue per movable interface-uses unit.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


implementation

const
  // Dotted message keys (rule.<RuleId>.message), seeded in initialization.
  cKeyFullyQualifiedImports = 'rule.FullyQualifiedImports.message';
  cKeyMoveImportToImplementation = 'rule.MoveImportToImplementation.message';

// Emits one issue at aNode's resolved row, column 1 (SEM nodes carry no column),
// carrying aArgs as the message args (the EmitCast/EmitStmt analogue).
procedure EmitAt(const aMeta: TRuleMetadata; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; aNode: TPasElement;
  const aArgs: array of string; const aSnippet: string);
var
  lLine: integer;
begin
  lLine := aContext.Resolver.SourceRow(aNode);
  aCollector.AddIssue(aMeta.RuleId, aContext.FileName, lLine, 1, lLine, 1,
    aMeta.Severity, aMeta.Category, aMeta.DefaultConfidence, aMeta.MessageKey,
    aArgs, aSnippet);
end;


{ TRuleFullyQualifiedImports }

procedure TRuleFullyQualifiedImports.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lNodes: TPasElementArray;
  lName: string;
  i: integer;
begin
  // The wrapper returns every offending reference node (or False to degrade —
  // emit nothing). The simple name is the reference's own PasTree value.
  if not aContext.Resolver.TryShadowedUnqualifiedRefs(lNodes) then
    Exit;
  for i := 0 to High(lNodes) do
  begin
    lName := TPrimitiveExpr(lNodes[i]).Value;
    EmitAt(FMetadata, aContext, aCollector, lNodes[i], [lName], lName);
  end;
end;


{ TRuleMoveImportToImplementation }

procedure TRuleMoveImportToImplementation.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lNodes: TPasElementArray;
  lName: string;
  i: integer;
begin
  // The wrapper returns every movable interface-uses unit node (or False to
  // degrade). The unit name is the uses-clause entry's own PasTree name.
  if not aContext.Resolver.TryMovableInterfaceUnits(lNodes) then
    Exit;
  for i := 0 to High(lNodes) do
  begin
    lName := TPasUsesUnit(lNodes[i]).Name;
    EmitAt(FMetadata, aContext, aCollector, lNodes[i], [lName], lName);
  end;
end;


initialization
  // Two SEM rules (rtSem / rfResolver), both Minor / CodeSmell / cfHigh — they
  // fire only on a resolved fact.
  RegisterRule(TRuleFullyQualifiedImports.Create(TRuleMetadata.Make(
    'FullyQualifiedImports', rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, cKeyFullyQualifiedImports).WithDescription(
    'Flags a reference that is ambiguous across units; qualify it with its unit name.')));
  RegisterMessage(cKeyFullyQualifiedImports, SFullyQualifiedImports);

  RegisterRule(TRuleMoveImportToImplementation.Create(TRuleMetadata.Make(
    'MoveImportToImplementation', rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, cKeyMoveImportToImplementation).WithDescription(
    'Flags a unit used only in the implementation that can move to the implementation uses clause.')));
  RegisterMessage(cKeyMoveImportToImplementation, SMoveImportToImplementation);

end.
