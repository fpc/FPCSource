{
    This file is part of the Free Component Library

    Pascal data-flow analysis (uninitialized-variable detection)
    Copyright (c) 2026

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
Abstract:
  After running TPasResolver, run this to emit a warning for a simple-typed
  local (or program-global) variable whose first use in execution/pre-order
  precedes its first definition ("Variable %s does not seem to be initialized").

  This complements PasUseAnalyzer, which reports unused declarations and an
  unset function result but does NOT flag uninitialized *variable use*.

Design:
  This reproduces the behaviour of the FPC compiler's -Oodfa switch, which does
  a limited, not a dynamic/correlated, analysis.

  The pass is intentionally optimistic:
  a definition anywhere textually earlier — even inside a single
  conditional branch — suppresses the warning for all later uses.
  It is therefore NOT a sound "definitely uninitialized" analysis:
  it can miss a use that is uninitialized only on some paths.

  This matches FPC: e.g. a variable assigned in one `if` branch and used under
  a later, independent `if` is not flagged.

  Equivalent rule: flag a use if the variable's first pre-order use
  precedes its first pre-order definition.

  Loop back-edges are ignored: a use at the top of a loop body,
  before a later definition in the same body, is still flagged
  — first iteration is uninitialized.

  Only simple typed variables are considered (ordinals, floats, booleans, chars,
  pointers, enums).

  Structured types (records, objects, classes, arrays, strings, sets,
  interfaces, procvars) are never flagged, because they may be
  initialized field-by-field or through a method call.

  Definitions include:
  assignment LHS, compound-assignment LHS, and passing to a
  var/out/untyped parameter (this covers FillChar/Move destinations and the
  implicit Self of an object method).

  The classification relies entirely on the read/write access the resolver
  already records on each reference  (TResolvedReference.Access), so no
  separate control-flow graph is built.

Scope:
  - every procedure/function body is analysed over its own local variables;
  - the main program begin..end block is analysed over the program's globals;
  - unit initialization/finalization sections are NOT analysed (that needs a
    cross-section / whole-program analysis);
  - only the passed-in module is analysed, never the units it uses.

Reporting:
  The warning is emitted through the (overridable) EmitMessage,
  which by default routes to Resolver.LogMsg with mtWarning and
  MsgNumber nUninitializedVariable.
  A consumer that wants -Sew semantics sets
    Scanner.WarnMsgState[nUninitializedVariable] := wmsError;
  before running the analysis;

  LogMsg then raises EPasResolve, aborting the compile — exactly as the
  FPC compiler does for -Oodfa -Sew.

  Consumers that run the analysis after the scanner is gone should
  override EmitMessage.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit PasDataFlow;
{$ENDIF FPC_DOTTEDUNITS}

{$i fcl-passrc.inc}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils,
  Pascal.Tree, Pascal.Scanner, Pascal.ResolveEval, Pascal.Resolver;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils,
  PasTree, PScanner, PasResolveEval, PasResolver;
{$ENDIF FPC_DOTTEDUNITS}

const
  // Matches the FPC compiler message sym_w_uninitialized_local_variable.
  nUninitializedVariable = 5036;
  sUninitializedVariable = 'Variable "%s" does not seem to be initialized';

type

  { TPasDataFlowAnalyzer }

  TPasDataFlowAnalyzer = class
  private
    FResolver: TPasResolver;
    FTracked: TFPList;   // TPasVariable being tracked for the current routine
    FAssigned: TFPList;  // TPasVariable already possibly-assigned
    FReported: TFPList;  // TPasVariable already reported (dedupe)
    function IsTracked(El: TPasElement): Boolean;
    function IsAssigned(V: TPasElement): Boolean;
    procedure MarkAssigned(V: TPasElement);
    procedure ReportUninit(V: TPasVariable; PosEl: TPasElement);
    procedure HandleRef(Expr: TPasExpr);
    procedure ProcessExpr(Expr: TPasExpr);
    procedure ProcessStmt(El: TPasElement);
    procedure ProcessElements(List: TFPList);
    function IsSimpleVarType(V: TPasVariable): Boolean;
    procedure CollectLocals(Decls: TPasDeclarations; List: TFPList);
    procedure AnalyzeRoutine(Body: TPasElement; Locals: TFPList);
    procedure AnalyzeProcs(Decls: TPasDeclarations);
  protected
    // Emit the "not initialized" diagnostic.
    // Default routes through Resolver.LogMsg.
    // Override to decouple from the scanner (e.g. a post-parse batch run).
    procedure EmitMessage(MsgNumber: Integer; const Fmt: String;
      const Args: array of const; PosEl: TPasElement); virtual;
  public
    constructor Create(AResolver: TPasResolver);
    destructor Destroy; override;
    // Analyse the given module for uninitialized-variable use.
    procedure AnalyzeModule(aModule: TPasModule);
    property Resolver: TPasResolver read FResolver;
  end;

implementation

constructor TPasDataFlowAnalyzer.Create(AResolver: TPasResolver);
begin
  FResolver := AResolver;
  FTracked := TFPList.Create;
  FAssigned := TFPList.Create;
  FReported := TFPList.Create;
end;


destructor TPasDataFlowAnalyzer.Destroy;
begin
  FTracked.Free;
  FAssigned.Free;
  FReported.Free;
  inherited Destroy;
end;


procedure TPasDataFlowAnalyzer.EmitMessage(MsgNumber: Integer;
  const Fmt: String; const Args: array of const; PosEl: TPasElement);
begin
  FResolver.LogMsg(20260715120000, mtWarning, MsgNumber, Fmt, Args, PosEl);
end;


function TPasDataFlowAnalyzer.IsTracked(El: TPasElement): Boolean;
begin
  Result := (El <> nil) and (FTracked.IndexOf(El) >= 0);
end;


function TPasDataFlowAnalyzer.IsAssigned(V: TPasElement): Boolean;
begin
  Result := FAssigned.IndexOf(V) >= 0;
end;


procedure TPasDataFlowAnalyzer.MarkAssigned(V: TPasElement);
begin
  if FAssigned.IndexOf(V) < 0 then
    FAssigned.Add(V);
end;


procedure TPasDataFlowAnalyzer.ReportUninit(V: TPasVariable; PosEl: TPasElement);
begin
  if FReported.IndexOf(V) >= 0 then Exit;
  FReported.Add(V);
  EmitMessage(nUninitializedVariable, sUninitializedVariable, [V.Name], PosEl);
end;


procedure TPasDataFlowAnalyzer.HandleRef(Expr: TPasExpr);
var
  Ref: TResolvedReference;
  Decl: TPasElement;
begin
  if not (Expr.CustomData is TResolvedReference) then Exit;
  Ref := TResolvedReference(Expr.CustomData);
  Decl := Ref.Declaration;
  if not IsTracked(Decl) then Exit;
  case Ref.Access of
    rraRead:
      if not IsAssigned(Decl) then
        ReportUninit(TPasVariable(Decl), Expr);
    // Any write-ish access initializes the variable. rraVarParam/rraOutParam
    // (var/out param, incl. FillChar/Move and the Self of an object method) and
    // rraReadAndAssign (inc/dec, +=) are treated as definitions and are NOT
    // flagged as reads — conservatively, to avoid false positives.
    rraAssign, rraReadAndAssign, rraVarParam, rraOutParam,
    rraParamToUnknownProc:
      MarkAssigned(Decl);
  else
    ; // rraNone: not a use of the variable's value
  end;
end;


procedure TPasDataFlowAnalyzer.ProcessExpr(Expr: TPasExpr);
var
  I: Integer;
begin
  if Expr = nil then Exit;

  if Expr is TBinaryExpr then
  begin
    // Left then right = evaluation order (also correct for and/or short-circuit).
    ProcessExpr(TBinaryExpr(Expr).left);
    ProcessExpr(TBinaryExpr(Expr).right);
  end
  else if Expr is TUnaryExpr then
    ProcessExpr(TUnaryExpr(Expr).Operand)
  else if Expr is TParamsExpr then
  begin
    ProcessExpr(TParamsExpr(Expr).Value);
    for I := 0 to High(TParamsExpr(Expr).Params) do
      ProcessExpr(TParamsExpr(Expr).Params[I]);
  end
  else if Expr is TInlineSpecializeExpr then
    ProcessExpr(TInlineSpecializeExpr(Expr).NameExpr)
  else if Expr is TArrayValues then
  begin
    for I := 0 to High(TArrayValues(Expr).Values) do
      ProcessExpr(TArrayValues(Expr).Values[I]);
  end
  else if Expr is TRecordValues then
  begin
    for I := 0 to High(TRecordValues(Expr).Fields) do
      ProcessExpr(TRecordValues(Expr).Fields[I].ValueExp);
  end;

  // A leaf identifier reference (simple var, or the head of a member access)
  // carries the tracked-variable reference directly.
  HandleRef(Expr);
end;


procedure TPasDataFlowAnalyzer.ProcessElements(List: TFPList);
var
  I: Integer;
begin
  if List = nil then Exit;
  for I := 0 to List.Count - 1 do
    ProcessStmt(TPasElement(List[I]));
end;


procedure TPasDataFlowAnalyzer.ProcessStmt(El: TPasElement);
var
  I: Integer;
  CaseSt: TPasImplCaseStatement;
begin
  if El = nil then Exit;

  if El is TPasImplAssign then
  begin
    // RHS is evaluated before the store to the LHS target.
    ProcessExpr(TPasImplAssign(El).Right);
    ProcessExpr(TPasImplAssign(El).Left);
  end
  else if El is TPasImplSimple then
    ProcessExpr(TPasImplSimple(El).Expr)
  else if El is TPasImplIfElse then
  begin
    ProcessExpr(TPasImplIfElse(El).ConditionExpr);
    ProcessStmt(TPasImplIfElse(El).IfBranch);
    ProcessStmt(TPasImplIfElse(El).ElseBranch);
  end
  else if El is TPasImplWhileDo then
  begin
    ProcessExpr(TPasImplWhileDo(El).ConditionExpr);
    ProcessStmt(TPasImplWhileDo(El).Body);
  end
  else if El is TPasImplRepeatUntil then
  begin
    // Body runs at least once, before the until condition.
    ProcessElements(TPasImplRepeatUntil(El).Elements);
    ProcessExpr(TPasImplRepeatUntil(El).ConditionExpr);
  end
  else if El is TPasImplForLoop then
  begin
    ProcessExpr(TPasImplForLoop(El).StartExpr);
    ProcessExpr(TPasImplForLoop(El).EndExpr);
    // The loop control variable is assigned by the loop header.
    ProcessExpr(TPasImplForLoop(El).VariableName);
    ProcessStmt(TPasImplForLoop(El).Body);
  end
  else if El is TPasImplCaseOf then
  begin
    ProcessExpr(TPasImplCaseOf(El).CaseExpr);
    for I := 0 to TPasImplCaseOf(El).Elements.Count - 1 do
    begin
      if TObject(TPasImplCaseOf(El).Elements[I]) is TPasImplCaseStatement then
      begin
        CaseSt := TPasImplCaseStatement(TPasImplCaseOf(El).Elements[I]);
        ProcessStmt(CaseSt.Body);
      end
      else if TObject(TPasImplCaseOf(El).Elements[I]) is TPasImplBlock then
        ProcessElements(TPasImplBlock(TPasImplCaseOf(El).Elements[I]).Elements);
    end;
  end
  else if El is TPasImplWithDo then
  begin
    for I := 0 to TPasImplWithDo(El).Expressions.Count - 1 do
      ProcessExpr(TPasExpr(TPasImplWithDo(El).Expressions[I]));
    ProcessStmt(TPasImplWithDo(El).Body);
  end
  else if El is TPasImplTry then
  begin
    ProcessElements(TPasImplTry(El).Elements);
    ProcessStmt(TPasImplTry(El).FinallyExcept);
    ProcessStmt(TPasImplTry(El).ElseBranch);
  end
  else if El is TPasImplExceptOn then
    ProcessStmt(TPasImplExceptOn(El).Body)
  else if El is TPasImplRaise then
  begin
    ProcessExpr(TPasImplRaise(El).ExceptObject);
    ProcessExpr(TPasImplRaise(El).ExceptAddr);
  end
  else if El is TPasImplBlock then
    // TPasImplBeginBlock, TPasImplTryHandler, TPasImplCaseElse, plain blocks
    ProcessElements(TPasImplBlock(El).Elements);
end;


function TPasDataFlowAnalyzer.IsSimpleVarType(V: TPasVariable): Boolean;
var
  Resolved: TPasResolverResult;
begin
  Result := False;
  if V.VarType = nil then Exit;
  FResolver.ComputeElement(V.VarType, Resolved, [rcType]);
  if Resolved.BaseType in (btAllInteger + btAllFloats + btAllBooleans +
       btAllChars + [btPointer, btCurrency]) then
    Result := True
  else if (Resolved.BaseType = btContext) and
          (Resolved.LoTypeEl is TPasEnumType) then
    Result := True;
end;


procedure TPasDataFlowAnalyzer.CollectLocals(Decls: TPasDeclarations;
  List: TFPList);
var
  I: Integer;
  El: TPasElement;
begin
  if Decls = nil then Exit;
  for I := 0 to Decls.Declarations.Count - 1 do
  begin
    El := TPasElement(Decls.Declarations[I]);
    // Only plain, simple-typed variables (not typed consts, properties, args)
    // without an initializer expression — those are already initialized.
    if (El.ClassType = TPasVariable) and (TPasVariable(El).Expr = nil) and
       IsSimpleVarType(TPasVariable(El)) then
      List.Add(El);
  end;
end;


procedure TPasDataFlowAnalyzer.AnalyzeRoutine(Body: TPasElement; Locals: TFPList);
begin
  if (Body = nil) or (Locals.Count = 0) then Exit;
  FTracked.Clear;
  FTracked.Assign(Locals);
  FAssigned.Clear;
  FReported.Clear;
  ProcessStmt(Body);
end;


procedure TPasDataFlowAnalyzer.AnalyzeProcs(Decls: TPasDeclarations);
var
  I: Integer;
  El: TPasElement;
  Proc: TPasProcedure;
  Locals: TFPList;
begin
  if Decls = nil then Exit;
  for I := 0 to Decls.Declarations.Count - 1 do
  begin
    El := TPasElement(Decls.Declarations[I]);
    if not (El is TPasProcedure) then Continue;
    Proc := TPasProcedure(El);
    if (Proc.Body = nil) then Continue;
    Locals := TFPList.Create;
    try
      CollectLocals(Proc.Body, Locals);
      AnalyzeRoutine(Proc.Body.Body, Locals);
    finally
      Locals.Free;
    end;
    // Nested procedures.
    AnalyzeProcs(Proc.Body);
  end;
end;


procedure TPasDataFlowAnalyzer.AnalyzeModule(aModule: TPasModule);
var
  Globals: TFPList;
begin
  if aModule = nil then Exit;

  if aModule is TPasProgram then
  begin
    // Procedures declared in the program section.
    AnalyzeProcs(TPasProgram(aModule).ProgramSection);
    // The main begin..end block, analysed over the program's global variables.
    Globals := TFPList.Create;
    try
      CollectLocals(TPasProgram(aModule).ProgramSection, Globals);
      AnalyzeRoutine(aModule.InitializationSection, Globals);
    finally
      Globals.Free;
    end;
  end
  else
  begin
    // Unit/library: analyse procedure bodies only (interface + implementation).
    // Init/finalization sections need whole-program analysis and are left alone.
    AnalyzeProcs(aModule.InterfaceSection);
    AnalyzeProcs(aModule.ImplementationSection);
  end;
end;

end.
