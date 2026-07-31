{
    This file is part of the Free Component Library

    Pascal control-flow graph
    Copyright (c) 2026

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
Abstract:
  Control-flow graph over a routine body: branch, loop, control-transfer and
  exceptional edges, plus AsText, a dump numbered from reverse postorder,
  unreachable blocks last, and carrying no pointer value.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit PasCFG;
{$ENDIF FPC_DOTTEDUNITS}

{$i fcl-passrc.inc}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils,
  Pascal.Tree;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils,
  PasTree;
{$ENDIF FPC_DOTTEDUNITS}

type
  // Detail AsText adds on request.
  TPasCFGTextOption = (ctoSourcePositions);
  TPasCFGTextOptions = set of TPasCFGTextOption;

  { TPasCFGNode }

  TPasCFGNode = class
  private
    FIndex: Integer;
    FReachable: Boolean;
    FStatements: TFPList;
    FSuccessors: TFPList;
    procedure AddStatement(aEl: TPasImplElement);
    procedure AddSuccessor(aNode: TPasCFGNode);
    function GetStatement(aIndex: Integer): TPasImplElement;
    function GetStatementCount: Integer;
    function GetSuccessor(aIndex: Integer): TPasCFGNode;
    function GetSuccessorCount: Integer;
  public
    // Creates an empty node.
    constructor Create;
    // Frees the node's lists; the statements and successors are borrowed.
    destructor Destroy; override;
    // Position in reverse postorder from the entry, or after those when unreached.
    property Index: Integer read FIndex;
    // Number of statements in the node.
    property StatementCount: Integer read GetStatementCount;
    // Statements of the node, in source order; a container statement is listed in
    // the node its flow enters, its nested statements in the nodes they belong to.
    property Statements[aIndex: Integer]: TPasImplElement read GetStatement;
    // Number of outgoing edges.
    property SuccessorCount: Integer read GetSuccessorCount;
    // Targets of the outgoing edges: true edge first, case labels in source order,
    // else last, and the exceptional edges after every normal one.
    property Successors[aIndex: Integer]: TPasCFGNode read GetSuccessor;
  end;

  { TPasCFG }

  TPasCFG = class
  private
    // The lists below exist only while Build runs.
    FBreakTargets: TFPList;
    FContinueTargets: TFPList;
    FLabelNodes: TStringList;
    FPendingExcSources: TFPList;
    FPendingExcTargets: TFPList;
    FPendingGotos: TStringList;
    FProtectTargets: TFPList;
    FEntryNode: TPasCFGNode;
    FExitNode: TPasCFGNode;
    FNodes: TFPList;
    function AddNode: TPasCFGNode;
    procedure AddStatement(aNode: TPasCFGNode; aEl: TPasImplElement);
    procedure Build(aBody: TPasImplBlock);
    function GetNode(aIndex: Integer): TPasCFGNode;
    function GetNodeCount: Integer;
    procedure NumberNodes;
    procedure PopLoop;
    procedure PopProtection;
    procedure PushLoop(aBreakTarget,aContinueTarget: TPasCFGNode);
    procedure PushProtection(aHandler: TPasCFGNode);
    procedure ResolveExceptions;
    procedure ResolveGotos;
    procedure VisitNode(aNode: TPasCFGNode; aPostorder: TFPList);
    function WalkBranch(aEl: TPasImplElement; aCurrent: TPasCFGNode): TPasCFGNode;
    function WalkCaseOf(aEl: TPasImplCaseOf; aCurrent: TPasCFGNode): TPasCFGNode;
    function WalkElements(aBlock: TPasImplBlock; aCurrent: TPasCFGNode): TPasCFGNode;
    function WalkGoto(aEl: TPasImplGoto; aCurrent: TPasCFGNode): TPasCFGNode;
    function WalkIfElse(aEl: TPasImplIfElse; aCurrent: TPasCFGNode): TPasCFGNode;
    function WalkLabelMark(aEl: TPasImplLabelMark; aCurrent: TPasCFGNode): TPasCFGNode;
    function WalkLoop(aEl,aBody: TPasImplElement; aCurrent: TPasCFGNode): TPasCFGNode;
    function WalkRepeatUntil(aEl: TPasImplRepeatUntil; aCurrent: TPasCFGNode): TPasCFGNode;
    function WalkStatement(aEl: TPasImplElement; aCurrent: TPasCFGNode): TPasCFGNode;
    function WalkTransfer(aEl: TPasImplElement; aCurrent: TPasCFGNode): TPasCFGNode;
    function WalkTry(aEl: TPasImplTry; aCurrent: TPasCFGNode): TPasCFGNode;
    function WalkWithDo(aEl: TPasImplWithDo; aCurrent: TPasCFGNode): TPasCFGNode;
  public
    // Builds the graph of aBody; the AST is borrowed and never freed here.
    constructor Create(aBody: TPasImplBlock);
    // Frees the graph nodes and leaves the analysed AST untouched.
    destructor Destroy; override;
    // Node holding aEl, or nil when aEl is not a statement of the analysed body.
    function NodeOf(aEl: TPasElement): TPasCFGNode;
    // True when aNode was reached by the walk from the entry node.
    function Reachable(aNode: TPasCFGNode): Boolean;
    // Textual dump of the graph, one line per block, statement and edge.
    function AsText(aOptions: TPasCFGTextOptions): String;
    // Node where execution of the body starts.
    property EntryNode: TPasCFGNode read FEntryNode;
    // Node where execution of the body ends.
    property ExitNode: TPasCFGNode read FExitNode;
    // Number of nodes in the graph, the unreachable ones included.
    property NodeCount: Integer read GetNodeCount;
    // Nodes in Index order, so Nodes[aIndex].Index equals aIndex.
    property Nodes[aIndex: Integer]: TPasCFGNode read GetNode;
  end;

implementation

type
  TPasCFGTransfer = (ctNone,ctBreak,ctContinue,ctExit,ctHalt);

// Break, Continue, Exit or Halt named by aEl, matched on identifier text alone.
function TransferKind(aEl: TPasImplElement): TPasCFGTransfer;

var
  lExpr: TPasExpr;
  lName: String;

begin
  Result:=ctNone;
  if not (aEl is TPasImplSimple) then
    Exit;
  lExpr:=TPasImplSimple(aEl).Expr;
  if (lExpr is TParamsExpr) and (TParamsExpr(lExpr).Kind=pekFuncParams) then
    lExpr:=TParamsExpr(lExpr).Value;
  if not (lExpr is TPrimitiveExpr) then
    Exit;
  if TPrimitiveExpr(lExpr).Kind<>pekIdent then
    Exit;
  lName:=LowerCase(TPrimitiveExpr(lExpr).Value);
  if lName='break' then
    Result:=ctBreak
  else if lName='continue' then
    Result:=ctContinue
  else if lName='exit' then
    Result:=ctExit
  else if lName='halt' then
    Result:=ctHalt;
end;


{ TPasCFGNode }

constructor TPasCFGNode.Create;

begin
  FIndex:=-1;
  FStatements:=TFPList.Create;
  FSuccessors:=TFPList.Create;
end;


destructor TPasCFGNode.Destroy;

begin
  FreeAndNil(FStatements);
  FreeAndNil(FSuccessors);
  inherited Destroy;
end;


procedure TPasCFGNode.AddStatement(aEl: TPasImplElement);

begin
  FStatements.Add(aEl);
end;


procedure TPasCFGNode.AddSuccessor(aNode: TPasCFGNode);

begin
  FSuccessors.Add(aNode);
end;


function TPasCFGNode.GetStatement(aIndex: Integer): TPasImplElement;

begin
  Result:=TPasImplElement(FStatements[aIndex]);
end;


function TPasCFGNode.GetStatementCount: Integer;

begin
  Result:=FStatements.Count;
end;


function TPasCFGNode.GetSuccessor(aIndex: Integer): TPasCFGNode;

begin
  Result:=TPasCFGNode(FSuccessors[aIndex]);
end;


function TPasCFGNode.GetSuccessorCount: Integer;

begin
  Result:=FSuccessors.Count;
end;


{ TPasCFG }

constructor TPasCFG.Create(aBody: TPasImplBlock);

begin
  FNodes:=TFPList.Create;
  Build(aBody);
  NumberNodes;
end;


destructor TPasCFG.Destroy;

var
  I: Integer;

begin
  for I:=0 to FNodes.Count-1 do
    TPasCFGNode(FNodes[I]).Free;
  FreeAndNil(FNodes);
  inherited Destroy;
end;


function TPasCFG.AddNode: TPasCFGNode;

begin
  Result:=TPasCFGNode.Create;
  FNodes.Add(Result);
end;


procedure TPasCFG.AddStatement(aNode: TPasCFGNode; aEl: TPasImplElement);

begin
  aNode.AddStatement(aEl);
  if FProtectTargets.Count>0 then
    begin
    FPendingExcSources.Add(aNode);
    FPendingExcTargets.Add(FProtectTargets[FProtectTargets.Count-1]);
    end;
end;


function TPasCFG.GetNode(aIndex: Integer): TPasCFGNode;

begin
  Result:=TPasCFGNode(FNodes[aIndex]);
end;


function TPasCFG.GetNodeCount: Integer;

begin
  Result:=FNodes.Count;
end;


procedure TPasCFG.Build(aBody: TPasImplBlock);

var
  lBlock: TPasCFGNode;

begin
  FEntryNode:=AddNode;
  FExitNode:=AddNode;
  FBreakTargets:=TFPList.Create;
  FContinueTargets:=TFPList.Create;
  FLabelNodes:=TStringList.Create;
  FPendingGotos:=TStringList.Create;
  FPendingExcSources:=TFPList.Create;
  FPendingExcTargets:=TFPList.Create;
  FProtectTargets:=TFPList.Create;
  try
    // An asm body is a block without Elements: the block itself is the statement.
    if aBody is TPasImplAsmStatement then
      begin
      lBlock:=AddNode;
      lBlock.AddStatement(aBody);
      FEntryNode.AddSuccessor(lBlock);
      lBlock.AddSuccessor(FExitNode);
      end
    else if (aBody<>Nil) and (aBody.Elements.Count>0) then
      begin
      lBlock:=AddNode;
      FEntryNode.AddSuccessor(lBlock);
      lBlock:=WalkElements(aBody,lBlock);
      if lBlock<>Nil then
        lBlock.AddSuccessor(FExitNode);
      ResolveGotos;
      ResolveExceptions;
      end
    else
      FEntryNode.AddSuccessor(FExitNode);
  finally
    FreeAndNil(FProtectTargets);
    FreeAndNil(FPendingExcTargets);
    FreeAndNil(FPendingExcSources);
    FreeAndNil(FPendingGotos);
    FreeAndNil(FLabelNodes);
    FreeAndNil(FContinueTargets);
    FreeAndNil(FBreakTargets);
  end;
end;


procedure TPasCFG.PushLoop(aBreakTarget,aContinueTarget: TPasCFGNode);

begin
  FBreakTargets.Add(aBreakTarget);
  FContinueTargets.Add(aContinueTarget);
end;


procedure TPasCFG.PopLoop;

begin
  FBreakTargets.Delete(FBreakTargets.Count-1);
  FContinueTargets.Delete(FContinueTargets.Count-1);
end;


procedure TPasCFG.PushProtection(aHandler: TPasCFGNode);

begin
  FProtectTargets.Add(aHandler);
end;


procedure TPasCFG.PopProtection;

begin
  FProtectTargets.Delete(FProtectTargets.Count-1);
end;


procedure TPasCFG.ResolveExceptions;

var
  I: Integer;
  lSource,lTarget: TPasCFGNode;

begin
  for I:=0 to FPendingExcSources.Count-1 do
    begin
    lSource:=TPasCFGNode(FPendingExcSources[I]);
    lTarget:=TPasCFGNode(FPendingExcTargets[I]);
    if lSource.FSuccessors.IndexOf(lTarget)<0 then
      lSource.AddSuccessor(lTarget);
    end;
end;


procedure TPasCFG.ResolveGotos;

var
  I,J: Integer;

begin
  for I:=0 to FPendingGotos.Count-1 do
    begin
    J:=FLabelNodes.IndexOf(FPendingGotos[I]);
    if J>=0 then
      TPasCFGNode(FPendingGotos.Objects[I]).AddSuccessor(TPasCFGNode(FLabelNodes.Objects[J]));
    end;
end;


function TPasCFG.WalkElements(aBlock: TPasImplBlock; aCurrent: TPasCFGNode): TPasCFGNode;

var
  I: Integer;

begin
  Result:=aCurrent;
  for I:=0 to aBlock.Elements.Count-1 do
    Result:=WalkStatement(TPasImplElement(aBlock.Elements[I]),Result);
end;


function TPasCFG.WalkBranch(aEl: TPasImplElement; aCurrent: TPasCFGNode): TPasCFGNode;

begin
  if aEl=Nil then
    Result:=aCurrent
  else
    Result:=WalkStatement(aEl,aCurrent);
end;


function TPasCFG.WalkStatement(aEl: TPasImplElement; aCurrent: TPasCFGNode): TPasCFGNode;

var
  lNext: TPasCFGNode;

begin
  if aEl is TPasImplLabelMark then
    Exit(WalkLabelMark(TPasImplLabelMark(aEl),aCurrent));
  // Flow ended before aEl: it opens a block nothing reaches.
  if aCurrent=Nil then
    aCurrent:=AddNode
  // In a protected region every statement gets a block of its own.
  else if (FProtectTargets.Count>0) and (aCurrent.StatementCount>0) then
    begin
    lNext:=AddNode;
    aCurrent.AddSuccessor(lNext);
    aCurrent:=lNext;
    end;
  if aEl is TPasImplBeginBlock then
    begin
    AddStatement(aCurrent,aEl);
    Result:=WalkElements(TPasImplBlock(aEl),aCurrent);
    end
  else if aEl is TPasImplIfElse then
    Result:=WalkIfElse(TPasImplIfElse(aEl),aCurrent)
  else if aEl is TPasImplCaseOf then
    Result:=WalkCaseOf(TPasImplCaseOf(aEl),aCurrent)
  else if aEl is TPasImplWhileDo then
    Result:=WalkLoop(aEl,TPasImplWhileDo(aEl).Body,aCurrent)
  else if aEl is TPasImplForLoop then
    Result:=WalkLoop(aEl,TPasImplForLoop(aEl).Body,aCurrent)
  else if aEl is TPasImplRepeatUntil then
    Result:=WalkRepeatUntil(TPasImplRepeatUntil(aEl),aCurrent)
  else if aEl is TPasImplWithDo then
    Result:=WalkWithDo(TPasImplWithDo(aEl),aCurrent)
  else if aEl is TPasImplGoto then
    Result:=WalkGoto(TPasImplGoto(aEl),aCurrent)
  else if aEl is TPasImplTry then
    Result:=WalkTry(TPasImplTry(aEl),aCurrent)
  else if aEl is TPasImplRaise then
    begin
    AddStatement(aCurrent,aEl);
    Result:=Nil;
    end
  else
    Result:=WalkTransfer(aEl,aCurrent);
end;


function TPasCFG.WalkTransfer(aEl: TPasImplElement; aCurrent: TPasCFGNode): TPasCFGNode;

begin
  AddStatement(aCurrent,aEl);
  Result:=aCurrent;
  case TransferKind(aEl) of
    ctBreak:
      if FBreakTargets.Count>0 then
        begin
        aCurrent.AddSuccessor(TPasCFGNode(FBreakTargets[FBreakTargets.Count-1]));
        Result:=Nil;
        end;
    ctContinue:
      if FContinueTargets.Count>0 then
        begin
        aCurrent.AddSuccessor(TPasCFGNode(FContinueTargets[FContinueTargets.Count-1]));
        Result:=Nil;
        end;
    ctExit:
      begin
      aCurrent.AddSuccessor(FExitNode);
      Result:=Nil;
      end;
    ctHalt:
      Result:=Nil;
  end;
end;


function TPasCFG.WalkIfElse(aEl: TPasImplIfElse; aCurrent: TPasCFGNode): TPasCFGNode;

var
  lThen,lElse,lJoin: TPasCFGNode;

begin
  AddStatement(aCurrent,aEl);
  lThen:=AddNode;
  aCurrent.AddSuccessor(lThen);
  lThen:=WalkBranch(aEl.IfBranch,lThen);
  lElse:=Nil;
  if aEl.ElseBranch<>Nil then
    begin
    lElse:=AddNode;
    aCurrent.AddSuccessor(lElse);
    lElse:=WalkBranch(aEl.ElseBranch,lElse);
    end;
  Result:=Nil;
  if (aEl.ElseBranch=Nil) or (lThen<>Nil) or (lElse<>Nil) then
    begin
    lJoin:=AddNode;
    if aEl.ElseBranch=Nil then
      aCurrent.AddSuccessor(lJoin);
    if lThen<>Nil then
      lThen.AddSuccessor(lJoin);
    if lElse<>Nil then
      lElse.AddSuccessor(lJoin);
    Result:=lJoin;
    end;
end;


function TPasCFG.WalkCaseOf(aEl: TPasImplCaseOf; aCurrent: TPasCFGNode): TPasCFGNode;

var
  I: Integer;
  lEl: TPasImplElement;
  lBranch,lJoin: TPasCFGNode;
  lTails: TFPList;

begin
  AddStatement(aCurrent,aEl);
  lTails:=TFPList.Create;
  try
    for I:=0 to aEl.Elements.Count-1 do
      begin
      lEl:=TPasImplElement(aEl.Elements[I]);
      if not (lEl is TPasImplCaseStatement) then
        Continue;
      lBranch:=AddNode;
      aCurrent.AddSuccessor(lBranch);
      AddStatement(lBranch,lEl);
      lBranch:=WalkBranch(TPasImplCaseStatement(lEl).Body,lBranch);
      if lBranch<>Nil then
        lTails.Add(lBranch);
      end;
    if aEl.ElseBranch<>Nil then
      begin
      lBranch:=AddNode;
      aCurrent.AddSuccessor(lBranch);
      AddStatement(lBranch,aEl.ElseBranch);
      lBranch:=WalkElements(aEl.ElseBranch,lBranch);
      if lBranch<>Nil then
        lTails.Add(lBranch);
      end;
    Result:=Nil;
    if (aEl.ElseBranch=Nil) or (lTails.Count>0) then
      begin
      lJoin:=AddNode;
      // The no-label-matches edge.
      if aEl.ElseBranch=Nil then
        aCurrent.AddSuccessor(lJoin);
      for I:=0 to lTails.Count-1 do
        TPasCFGNode(lTails[I]).AddSuccessor(lJoin);
      Result:=lJoin;
      end;
  finally
    lTails.Free;
  end;
end;


function TPasCFG.WalkTry(aEl: TPasImplTry; aCurrent: TPasCFGNode): TPasCFGNode;

var
  I,lOnCount: Integer;
  lEl: TPasImplElement;
  lHandler,lBody,lBranch,lJoin: TPasCFGNode;
  lTails: TFPList;

begin
  AddStatement(aCurrent,aEl);
  // Without a handler nothing is protected, so the body is a plain statement list.
  if aEl.FinallyExcept=Nil then
    Exit(WalkElements(aEl,aCurrent));
  lHandler:=AddNode;
  PushProtection(lHandler);
  try
    lBody:=WalkElements(aEl,aCurrent);
  finally
    PopProtection;
  end;
  if aEl.FinallyExcept is TPasImplTryFinally then
    begin
    if lBody<>Nil then
      lBody.AddSuccessor(lHandler);
    AddStatement(lHandler,aEl.FinallyExcept);
    Exit(WalkElements(aEl.FinallyExcept,lHandler));
    end;
  AddStatement(lHandler,aEl.FinallyExcept);
  lTails:=TFPList.Create;
  try
    if lBody<>Nil then
      lTails.Add(lBody);
    lOnCount:=0;
    for I:=0 to aEl.FinallyExcept.Elements.Count-1 do
      begin
      lEl:=TPasImplElement(aEl.FinallyExcept.Elements[I]);
      if not (lEl is TPasImplExceptOn) then
        Continue;
      Inc(lOnCount);
      lBranch:=AddNode;
      lHandler.AddSuccessor(lBranch);
      AddStatement(lBranch,lEl);
      lBranch:=WalkBranch(TPasImplExceptOn(lEl).Body,lBranch);
      if lBranch<>Nil then
        lTails.Add(lBranch);
      end;
    // A handler without on elements runs in the dispatch block itself.
    if lOnCount=0 then
      begin
      lBranch:=WalkElements(aEl.FinallyExcept,lHandler);
      if lBranch<>Nil then
        lTails.Add(lBranch);
      end;
    if aEl.ElseBranch<>Nil then
      begin
      lBranch:=AddNode;
      lHandler.AddSuccessor(lBranch);
      AddStatement(lBranch,aEl.ElseBranch);
      lBranch:=WalkElements(aEl.ElseBranch,lBranch);
      if lBranch<>Nil then
        lTails.Add(lBranch);
      end;
    Result:=Nil;
    if lTails.Count>0 then
      begin
      lJoin:=AddNode;
      for I:=0 to lTails.Count-1 do
        TPasCFGNode(lTails[I]).AddSuccessor(lJoin);
      Result:=lJoin;
      end;
  finally
    lTails.Free;
  end;
end;


function TPasCFG.WalkLoop(aEl,aBody: TPasImplElement; aCurrent: TPasCFGNode): TPasCFGNode;

var
  lHeader,lBody,lAfter: TPasCFGNode;

begin
  lHeader:=AddNode;
  aCurrent.AddSuccessor(lHeader);
  AddStatement(lHeader,aEl);
  lBody:=AddNode;
  lAfter:=AddNode;
  lHeader.AddSuccessor(lBody);
  lHeader.AddSuccessor(lAfter);
  PushLoop(lAfter,lHeader);
  try
    lBody:=WalkBranch(aBody,lBody);
  finally
    PopLoop;
  end;
  if lBody<>Nil then
    lBody.AddSuccessor(lHeader);
  Result:=lAfter;
end;


function TPasCFG.WalkRepeatUntil(aEl: TPasImplRepeatUntil; aCurrent: TPasCFGNode): TPasCFGNode;

var
  lBody,lCondition,lAfter,lTail: TPasCFGNode;

begin
  lBody:=AddNode;
  aCurrent.AddSuccessor(lBody);
  lCondition:=AddNode;
  AddStatement(lCondition,aEl);
  lAfter:=AddNode;
  lCondition.AddSuccessor(lAfter);
  lCondition.AddSuccessor(lBody);
  PushLoop(lAfter,lCondition);
  try
    lTail:=WalkElements(aEl,lBody);
  finally
    PopLoop;
  end;
  if lTail<>Nil then
    lTail.AddSuccessor(lCondition);
  Result:=lAfter;
end;


function TPasCFG.WalkWithDo(aEl: TPasImplWithDo; aCurrent: TPasCFGNode): TPasCFGNode;

begin
  AddStatement(aCurrent,aEl);
  Result:=WalkBranch(aEl.Body,aCurrent);
end;


function TPasCFG.WalkGoto(aEl: TPasImplGoto; aCurrent: TPasCFGNode): TPasCFGNode;

begin
  AddStatement(aCurrent,aEl);
  FPendingGotos.AddObject(LowerCase(aEl.LabelName),aCurrent);
  Result:=Nil;
end;


function TPasCFG.WalkLabelMark(aEl: TPasImplLabelMark; aCurrent: TPasCFGNode): TPasCFGNode;

begin
  Result:=AddNode;
  if aCurrent<>Nil then
    aCurrent.AddSuccessor(Result);
  AddStatement(Result,aEl);
  FLabelNodes.AddObject(LowerCase(aEl.LabelId),Result);
end;


procedure TPasCFG.VisitNode(aNode: TPasCFGNode; aPostorder: TFPList);

var
  I: Integer;

begin
  if aNode.FReachable then
    Exit;
  aNode.FReachable:=True;
  for I:=aNode.FSuccessors.Count-1 downto 0 do
    VisitNode(TPasCFGNode(aNode.FSuccessors[I]),aPostorder);
  aPostorder.Add(aNode);
end;


procedure TPasCFG.NumberNodes;

var
  I,lNext: Integer;
  lNode: TPasCFGNode;
  lPostorder,lOrdered: TFPList;

begin
  lPostorder:=TFPList.Create;
  try
    VisitNode(FEntryNode,lPostorder);
    lNext:=0;
    for I:=lPostorder.Count-1 downto 0 do
      begin
      TPasCFGNode(lPostorder[I]).FIndex:=lNext;
      Inc(lNext);
      end;
    for I:=0 to FNodes.Count-1 do
      begin
      lNode:=TPasCFGNode(FNodes[I]);
      if not lNode.FReachable then
        begin
        lNode.FIndex:=lNext;
        Inc(lNext);
        end;
      end;
    lOrdered:=TFPList.Create;
    try
      lOrdered.Count:=FNodes.Count;
      for I:=0 to FNodes.Count-1 do
        lOrdered[TPasCFGNode(FNodes[I]).FIndex]:=FNodes[I];
      FNodes.Assign(lOrdered);
    finally
      lOrdered.Free;
    end;
  finally
    lPostorder.Free;
  end;
end;


function TPasCFG.NodeOf(aEl: TPasElement): TPasCFGNode;

var
  I: Integer;
  lNode: TPasCFGNode;

begin
  Result:=Nil;
  if aEl=Nil then
    Exit;
  for I:=0 to FNodes.Count-1 do
    begin
    lNode:=TPasCFGNode(FNodes[I]);
    if lNode.FStatements.IndexOf(aEl)>=0 then
      Exit(lNode);
    end;
end;


function TPasCFG.Reachable(aNode: TPasCFGNode): Boolean;

begin
  Result:=(aNode<>Nil) and aNode.FReachable;
end;


function TPasCFG.AsText(aOptions: TPasCFGTextOptions): String;

var
  I,J: Integer;
  lNode,lSucc: TPasCFGNode;
  lLine: String;

begin
  Result:='';
  for I:=0 to FNodes.Count-1 do
    begin
    lNode:=TPasCFGNode(FNodes[I]);
    lLine:='block '+IntToStr(lNode.FIndex);
    if lNode=FEntryNode then
      lLine:=lLine+' entry';
    if lNode=FExitNode then
      lLine:=lLine+' exit';
    Result:=Result+lLine+LineEnding;
    for J:=0 to lNode.FStatements.Count-1 do
      begin
      lLine:='  stmt '+TPasImplElement(lNode.FStatements[J]).ClassName;
      if ctoSourcePositions in aOptions then
        lLine:=lLine+'@'+IntToStr(TPasImplElement(lNode.FStatements[J]).SourceLinenumber);
      Result:=Result+lLine+LineEnding;
      end;
    for J:=0 to lNode.FSuccessors.Count-1 do
      begin
      lSucc:=TPasCFGNode(lNode.FSuccessors[J]);
      Result:=Result+'  succ '+IntToStr(lSucc.FIndex)+LineEnding;
      end;
    end;
end;

end.
