{
    This file is part of the Free Component Library

    Implementation of the XML Path Language (XPath) for Free Pascal
    Copyright (c) 2000 - 2003 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit XPath;

interface

uses SysUtils, Classes, DOM;

resourcestring
  { XPath variables type names }
  SNodeSet = 'node set';
  SBoolean = 'boolean';
  SNumber = 'number';
  SString = 'string';

  { Variable errors }
  SVarNoConversion = 'Conversion from %s to %s not possible';

  { Scanner errors }
  SScannerInternalError = 'Internal expression scanner error';
  SScannerQuotStringIsOpen = 'Ending ''"'' for string not found';
  SScannerAposStringIsOpen = 'Ending "''" for string not found';
  SScannerInvalidChar = 'Invalid character';

  { Parser errors }
  SParserExpectedLeftBracket = 'Expected ")"';
  SParserExpectedRightBracket = 'Expected ")"';
  SParserExpectedColonColor = 'Expected "::" after axis specifier';
  SParserExpectedBrackets = 'Expected "()" after NodeType test';
  SParserExpectedRightSquareBracket = 'Expected "]" after predicate';
  SParserInvalidPrimExpr = 'Invalid primary expression';
  SParserGarbageAfterExpression = 'Unrecognized input after expression';
  SParserInvalidNodeTest = 'Invalid node test (syntax error)';
  SParserExpectedVarName = 'Expected variable name after "$"';

  { Evaluation errors }
  SEvalUnknownFunction = 'Unknown function: "%s"';
  SEvalUnknownVariable = 'Unknown variable: "%s"';
  SEvalInvalidArgCount = 'Invalid number of function arguments';
  SEvalFunctionNotImplementedYet = 'Function "%s" has not been implemented yet'; // !!!

type

  TXPathContext = class;
  TXPathEnvironment = class;
  TXPathVariable = class;


{ XPath lexical scanner }

  TXPathToken = (               // [28] - [38]
    tkInvalid,
    tkEndOfStream,
    tkIdentifier,
    tkString,
    tkNumber,
    tkDollar,                   // "$"
    tkLeftBracket,              // "("
    tkRightBracket,             // ")"
    tkAsterisk,                 // "*"
    tkPlus,                     // "+"
    tkComma,                    // ","
    tkMinus,                    // "-"
    tkDot,                      // "."
    tkDotDot,                   // ".."
    tkSlash,                    // "/"
    tkSlashSlash,               // "//"
    tkColon,                    // ":"
    tkColonColon,               // "::"
    tkLess,                     // "<"
    tkLessEqual,                // "<="
    tkEqual,                    // "="
    tkNotEqual,                 // "!="
    tkGreater,                  // ">"
    tkGreaterEqual,             // ">="
    tkAt,                       // "@"
    tkLeftSquareBracket,        // "["
    tkRightSquareBracket,       // "]"
    tkPipe                      // "|"
  );


{ XPath expression parse tree }

  TXPathExprNode = class
  public
    function Evaluate(AContext: TXPathContext;
      AEnvironment: TXPathEnvironment): TXPathVariable; virtual; abstract;
  end;


  TXPathConstantNode = class(TXPathExprNode)
  private
    FValue: TXPathVariable;
  public
    constructor Create(AValue: TXPathVariable);
    destructor Destroy; override;
    function Evaluate(AContext: TXPathContext;
       AEnvironment: TXPathEnvironment): TXPathVariable; override;
  end;


  TXPathVariableNode = class(TXPathExprNode)
  private
    FName: DOMString;
  public
    constructor Create(const AName: DOMString);
    function Evaluate(AContext: TXPathContext;
      AEnvironment: TXPathEnvironment): TXPathVariable; override;
  end;


  TXPathFunctionNode = class(TXPathExprNode)
  private
    FName: DOMString;
    FArgs: TList;
  public
    constructor Create(const AName: DOMString);
    destructor Destroy; override;
    function Evaluate(AContext: TXPathContext;
      AEnvironment: TXPathEnvironment): TXPathVariable; override;
  end;


  TXPathNegationNode = class(TXPathExprNode)
  private
    FOperand: TXPathExprNode;
  public
    constructor Create(AOperand: TXPathExprNode);
    destructor Destroy; override;
    function Evaluate(AContext: TXPathContext;
      AEnvironment: TXPathEnvironment): TXPathVariable; override;
  end;


  // Node for (binary) mathematical operation

  TXPathMathOp = (opAdd, opSubtract, opMultiply, opDivide, opMod);

  TXPathMathOpNode = class(TXPathExprNode)
  private
    FOperand1, FOperand2: TXPathExprNode;
    FOperator: TXPathMathOp;
  public
    constructor Create(AOperator: TXPathMathOp;
      AOperand1, AOperand2: TXPathExprNode);
    destructor Destroy; override;
    function Evaluate(AContext: TXPathContext;
      AEnvironment: TXPathEnvironment): TXPathVariable; override;
  end;


  // Node for boolean operations

  TXPathBooleanOp = (opEqual, opNotEqual, opLess, opLessEqual, opGreater,
    opGreaterEqual, opOr, opAnd);

  TXPathBooleanOpNode = class(TXPathExprNode)
  private
    FOperand1, FOperand2: TXPathExprNode;
    FOperator: TXPathBooleanOp;
  public
    constructor Create(AOperator: TXPathBooleanOp;
      AOperand1, AOperand2: TXPathExprNode);
    destructor Destroy; override;
    function Evaluate(AContext: TXPathContext;
      AEnvironment: TXPathEnvironment): TXPathVariable; override;
  end;


  // Node for unions (see [18])

  TXPathUnionNode = class(TXPathExprNode)
  private
    FOperand1, FOperand2: TXPathExprNode;
  public
    constructor Create(AOperand1, AOperand2: TXPathExprNode);
    destructor Destroy; override;
    function Evaluate(AContext: TXPathContext;
      AEnvironment: TXPathEnvironment): TXPathVariable; override;
  end;


  // Filter node (for [20])

  TXPathFilterNode = class(TXPathExprNode)
  private
    FExpr: TXPathExprNode;
    FPredicates: TList;
  public
    constructor Create(AExpr: TXPathExprNode);
    destructor Destroy; override;
    function Evaluate(AContext: TXPathContext;
      AEnvironment: TXPathEnvironment): TXPathVariable; override;
  end;


  // Node for location paths

  TAxis = (axisInvalid, axisAncestor, axisAncestorOrSelf, axisAttribute,
    axisChild, axisDescendant, axisDescendantOrSelf, axisFollowing,
    axisFollowingSibling, axisNamespace, axisParent, axisPreceding,
    axisPrecedingSibling, axisSelf);

  TNodeTestType = (ntAnyPrincipal, ntName, ntTextNode,
    ntCommentNode, ntPINode, ntAnyNode);

  TStep = class
  public
    constructor Create;
    destructor Destroy; override;
    NextStep: TStep;
    Axis: TAxis;
    NodeTestType: TNodeTestType;
    NodeTestString: DOMString;
    Predicates: TList;
  end;

  TXPathLocationPathNode = class(TXPathExprNode)
  private
    FFirstStep: TStep;
    FIsAbsolutePath: Boolean;
  public
    constructor Create(AIsAbsolutePath: Boolean);
    function Evaluate(AContext: TXPathContext;
      AEnvironment: TXPathEnvironment): TXPathVariable; override;
  end;


  TNodeSet = TList;

{ Exceptions }

  EXPathEvaluationError = class(Exception);

  procedure EvaluationError(const Msg: String);
  procedure EvaluationError(const Msg: String; const Args: array of const);


type

{ XPath variables and results classes }

  TXPathVariable = class
  protected
    FRefCount: Integer;
    procedure Error(const Msg: String; const Args: array of const);
  public
    class function TypeName: String; virtual; abstract;
    procedure Release;
    function AsNodeSet: TNodeSet; virtual;
    function AsBoolean: Boolean; virtual;
    function AsNumber: Extended; virtual;
    function AsText: DOMString; virtual;
  end;

  TXPathNodeSetVariable = class(TXPathVariable)
  private
    FValue: TNodeSet;
  public
    constructor Create(AValue: TNodeSet);
    destructor Destroy; override;
    class function TypeName: String; override;
    function AsNodeSet: TNodeSet; override;
    function AsText: DOMString; override;
    property Value: TNodeSet read FValue;
  end;

  TXPathBooleanVariable = class(TXPathVariable)
  private
    FValue: Boolean;
  public
    constructor Create(AValue: Boolean);
    class function TypeName: String; override;
    function AsBoolean: Boolean; override;
    function AsNumber: Extended; override;
    function AsText: DOMString; override;
    property Value: Boolean read FValue;
  end;

  TXPathNumberVariable = class(TXPathVariable)
  private
    FValue: Extended;
  public
    constructor Create(AValue: Extended);
    class function TypeName: String; override;
    function AsBoolean: Boolean; override;
    function AsNumber: Extended; override;
    function AsText: DOMString; override;
    property Value: Extended read FValue;
  end;

  TXPathStringVariable = class(TXPathVariable)
  private
    FValue: DOMString;
  public
    constructor Create(const AValue: DOMString);
    class function TypeName: String; override;
    function AsBoolean: Boolean; override;
    function AsNumber: Extended; override;
    function AsText: DOMString; override;
    property Value: DOMString read FValue;
  end;


{ XPath lexical scanner }

  TXPathScannerState = class
  private
    FCurData: PWideChar;
    FCurToken: TXPathToken;
    FCurTokenString: DOMString;
    FDoUnget: Boolean;
  end;

  TXPathScanner = class
  private
    FExpressionString, FCurData: PWideChar;
    FCurToken: TXPathToken;
    FCurTokenString: DOMString;
    FDoUnget: Boolean;
    procedure Error(const Msg: String);
    procedure Error(const Msg: String; const Args: array of const);
  public
    constructor Create(const AExpressionString: DOMString);
    function NextToken: TXPathToken;
    procedure UngetToken;
    function SaveState: TXPathScannerState;
    procedure RestoreState(AState: TXPathScannerState);
    property CurToken: TXPathToken read FCurToken;
    property CurTokenString: DOMString read FCurTokenString;
  end;


{ XPath context }

  TXPathContext = class
  public
    constructor Create(AContextNode: TDOMNode;
      AContextPosition, AContextSize: Integer);
    ContextNode: TDOMNode;
    ContextPosition: Integer;
    ContextSize: Integer;
  end;


{ XPath environments (not defined in XPath standard: an environment contains
  the variables and functions, which are part of the context in the official
  standard). }

  TXPathVarList = TList;

  TXPathFunction = function(Context: TXPathContext; Args: TXPathVarList):
    TXPathVariable of object;

  TXPathEnvironment = class
  private
    FFunctions: TList;
    FVariables: TList;
    function GetFunctionCount: Integer;
    function GetVariableCount: Integer;
    function GetFunction(Index: Integer): TXPathFunction;
    function GetFunction(const AName: String): TXPathFunction;
    function GetVariable(Index: Integer): TXPathVariable;
    function GetVariable(const AName: String): TXPathVariable;
  protected
    // XPath Core Function Library:
    function xpLast(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpPosition(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpCount(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpId(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpLocalName(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpNamespaceURI(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpName(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpString(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpConcat(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpStartsWith(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpContains(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpSubstringBefore(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpSubstringAfter(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpSubstring(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpStringLength(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpNormalizeSpace(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpTranslate(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpBoolean(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpNot(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpTrue(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpFalse(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpLang(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpNumber(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpSum(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpFloor(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpCeiling(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
    function xpRound(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
  public
    constructor Create;
    destructor Destroy; override;
    function GetFunctionIndex(const AName: String): Integer;
    function GetVariableIndex(const AName: String): Integer;
    procedure AddFunction(const AName: String; AFunction: TXPathFunction);
    procedure AddVariable(const AName: String; AVariable: TXPathVariable);
    procedure RemoveFunction(Index: Integer);
    procedure RemoveFunction(const AName: String);
    procedure RemoveVariable(Index: Integer);
    procedure RemoveVariable(const AName: String);
    property FunctionCount: Integer read GetFunctionCount;
    property VariableCount: Integer read GetVariableCount;
    property Functions[Index: Integer]: TXPathFunction read GetFunction;
    property FunctionsByName[const AName: String]: TXPathFunction
       read GetFunction;
    property Variables[Index: Integer]: TXPathVariable read GetVariable;
    property VariablesByName[const AName: String]: TXPathVariable read GetVariable;
  end;


{ XPath expressions }

  TXPathExpression = class
  private
    FRootNode: TXPathExprNode;
  public
    { CompleteExpresion specifies wether the parser should check for gargabe
      after the recognised part. True => Throw exception if there is garbage }
    constructor Create(AScanner: TXPathScanner; CompleteExpression: Boolean);
    function Evaluate(AContextNode: TDOMNode): TXPathVariable;
    function Evaluate(AContextNode: TDOMNode;
      AEnvironment: TXPathEnvironment): TXPathVariable;
  end;


function EvaluateXPathExpression(const AExpressionString: DOMString;
  AContextNode: TDOMNode): TXPathVariable;


// ===================================================================
// ===================================================================

implementation

{ Helper functions }

function NodeToText(Node: TDOMNode): DOMString;
var
  Child: TDOMNode;
begin
  case Node.NodeType of
    DOCUMENT_NODE, DOCUMENT_FRAGMENT_NODE{, ELEMENT_NODE}:
      begin
        SetLength(Result, 0);
        Child := Node.FirstChild;
        while Assigned(Child) do
        begin
	  if Result <> '' then
	    Result := Result + LineEnding;
          Result := Result + NodeToText(Child);
          Child := Child.NextSibling;
        end;
      end;
    ELEMENT_NODE:
      Result := Node.NodeName;
    ATTRIBUTE_NODE, PROCESSING_INSTRUCTION_NODE, COMMENT_NODE, TEXT_NODE,
      CDATA_SECTION_NODE, ENTITY_REFERENCE_NODE:
      Result := Node.NodeValue;
  end;
  // !!!: What to do with 'namespace nodes'?
end;


{ XPath parse tree classes }

constructor TXPathConstantNode.Create(AValue: TXPathVariable);
begin
  inherited Create;
  FValue := AValue;
end;

destructor TXPathConstantNode.Destroy;
begin
  FValue.Free;
  inherited Destroy;
end;

function TXPathConstantNode.Evaluate(AContext: TXPathContext;
  AEnvironment: TXPathEnvironment): TXPathVariable;
begin
  Result := FValue;
  Inc(Result.FRefCount);
end;


constructor TXPathVariableNode.Create(const AName: DOMString);
begin
  inherited Create;
  FName := AName;
end;

function TXPathVariableNode.Evaluate(AContext: TXPathContext;
  AEnvironment: TXPathEnvironment): TXPathVariable;
begin
  Result := AEnvironment.VariablesByName[FName];
  if not Assigned(Result) then
    EvaluationError(SEvalUnknownVariable, [FName]);
end;


constructor TXPathFunctionNode.Create(const AName: DOMString);
begin
  inherited Create;
  FName := AName;
  FArgs := TList.Create;
end;

destructor TXPathFunctionNode.Destroy;
var
  i: Integer;
begin
  for i := 0 to FArgs.Count - 1 do
    TXPathExprNode(FArgs[i]).Free;
  FArgs.Free;
  inherited Destroy;
end;

function TXPathFunctionNode.Evaluate(AContext: TXPathContext;
  AEnvironment: TXPathEnvironment): TXPathVariable;
var
  Fn: TXPathFunction;
  Args: TXPathVarList;
  i: Integer;
begin
  Fn := AEnvironment.FunctionsByName[FName];
  if not Assigned(Fn) then
    EvaluationError(SEvalUnknownFunction, [FName]);

  Args := TXPathVarList.Create;
  try
    for i := 0 to FArgs.Count - 1 do
      Args.Add(TXPathExprNode(FArgs[i]).Evaluate(AContext, AEnvironment));
    Result := Fn(AContext, Args);
  finally
    Args.Free;
  end;
end;


constructor TXPathNegationNode.Create(AOperand: TXPathExprNode);
begin
  inherited Create;
  FOperand := AOperand;
end;

destructor TXPathNegationNode.Destroy;
begin
  FOperand.Free;
  inherited Destroy;
end;

function TXPathNegationNode.Evaluate(AContext: TXPathContext;
  AEnvironment: TXPathEnvironment): TXPathVariable;
var
  OpResult: TXPathVariable;
begin
  OpResult := FOperand.Evaluate(AContext, AEnvironment);
  try
    Result := TXPathNumberVariable.Create(-OpResult.AsNumber);
  finally
    OpResult.Release;
  end;
end;


constructor TXPathMathOpNode.Create(AOperator: TXPathMathOp;
  AOperand1, AOperand2: TXPathExprNode);
begin
  inherited Create;
  FOperator := AOperator;
  FOperand1 := AOperand1;
  FOperand2 := AOperand2;
end;

destructor TXPathMathOpNode.Destroy;
begin
  FOperand1.Free;
  FOperand2.Free;
  inherited Destroy;
end;

function TXPathMathOpNode.Evaluate(AContext: TXPathContext;
  AEnvironment: TXPathEnvironment): TXPathVariable;
var
  Op1Result, Op2Result: TXPathVariable;
  Op1, Op2, NumberResult: Extended;
begin
  Op1Result := FOperand1.Evaluate(AContext, AEnvironment);
  try
    Op2Result := FOperand2.Evaluate(AContext, AEnvironment);
    try
      Op1 := Op1Result.AsNumber;
      Op2 := Op2Result.AsNumber;
      case FOperator of
        opAdd:
          NumberResult := Op1 + Op2;
        opSubtract:
          NumberResult := Op1 - Op2;
        opMultiply:
          NumberResult := Op1 * Op2;
        opDivide:
          NumberResult := Op1 / Op2;
        opMod:
          NumberResult := Trunc(Op1) mod Trunc(Op2);
      end;
    finally
      Op2Result.Release;
    end;
  finally
    Op1Result.Release;
  end;
  Result := TXPathNumberVariable.Create(NumberResult);
end;


constructor TXPathBooleanOpNode.Create(AOperator: TXPathBooleanOp;
  AOperand1, AOperand2: TXPathExprNode);
begin
  inherited Create;
  FOperator := AOperator;
  FOperand1 := AOperand1;
  FOperand2 := AOperand2;
end;

destructor TXPathBooleanOpNode.Destroy;
begin
  FOperand1.Free;
  FOperand2.Free;
  inherited Destroy;
end;

function TXPathBooleanOpNode.Evaluate(AContext: TXPathContext;
  AEnvironment: TXPathEnvironment): TXPathVariable;
var
  Op1, Op2: TXPathVariable;

  function EvalEqual: Boolean;
  var
    i, j: Integer;
    NodeSet1, NodeSet2: TNodeSet;
    s: DOMString;
  begin
    // !!!: Doesn't handle nodesets yet!
    if Op1.InheritsFrom(TXPathNodeSetVariable) then
    begin
      NodeSet1 := Op1.AsNodeSet;
      if Op2.InheritsFrom(TXPathNodeSetVariable) then
      begin
        NodeSet2 := Op2.AsNodeSet;
        for i := 0 to NodeSet1.Count - 1 do
        begin
          s := NodeToText(TDOMNode(NodeSet1[i]));
          for j := 0 to NodeSet2.Count - 1 do
            if s = NodeToText(TDOMNode(NodeSet2[j])) then
            begin
              Result := True;
              exit;
            end;
        end;
      end else
      begin
        s := Op2.AsText;
        for i := 0 to NodeSet1.Count - 1 do
        begin
          if NodeToText(TDOMNode(NodeSet1[i])) = s then
          begin
            Result := True;
            exit;
          end;
        end;
      end;
      Result := False;
    end else if Op2.InheritsFrom(TXPathNodeSetVariable) then
    begin
      s := Op1.AsText;
      for i := 0 to NodeSet2.Count - 1 do
        if s = NodeToText(TDOMNode(NodeSet2[i])) then
        begin
          Result := True;
          exit;
        end;
      Result := False;
    end else if Op1.InheritsFrom(TXPathBooleanVariable) or
      Op2.InheritsFrom(TXPathBooleanVariable) then
      Result := Op1.AsBoolean = Op2.AsBoolean
    else if Op1.InheritsFrom(TXPathNumberVariable) or
      Op2.InheritsFrom(TXPathNumberVariable) then
      Result := Op1.AsNumber = Op2.AsNumber
    else
      Result := Op1.AsText = Op2.AsText; // !!!: Attention with Unicode!
  end;

var
  BoolResult: Boolean;
begin
  Op1 := FOperand1.Evaluate(AContext, AEnvironment);
  try
    Op2 := FOperand2.Evaluate(AContext, AEnvironment);
    try
      case FOperator of
        opEqual:
          BoolResult := EvalEqual;
        opNotEqual:
          BoolResult := not EvalEqual;
        opLess:
          BoolResult := Op1.AsNumber < Op2.AsNumber;
        opLessEqual:
          BoolResult := Op1.AsNumber <= Op2.AsNumber;
        opGreater:
          BoolResult := Op1.AsNumber > Op2.AsNumber;
        opGreaterEqual:
          BoolResult := Op1.AsNumber >= Op2.AsNumber;
        opOr:
          BoolResult := Op1.AsBoolean or Op2.AsBoolean;
        opAnd:
          BoolResult := Op1.AsBoolean and Op2.AsBoolean;
      end;
    finally
      Op2.Release;
    end;
  finally
    Op1.Release;
  end;
  Result := TXPathBooleanVariable.Create(BoolResult);
end;


constructor TXPathUnionNode.Create(AOperand1, AOperand2: TXPathExprNode);
begin
  inherited Create;
  FOperand1 := AOperand1;
  FOperand2 := AOperand2;
end;

destructor TXPathUnionNode.Destroy;
begin
  FOperand1.Free;
  FOperand2.Free;
  inherited Destroy;
end;

function TXPathUnionNode.Evaluate(AContext: TXPathContext;
  AEnvironment: TXPathEnvironment): TXPathVariable;
var
  Op1Result, Op2Result: TXPathVariable;
  NodeSet, NodeSet2: TNodeSet;
  CurNode: Pointer;
  i, j: Integer;
  DoAdd: Boolean;
begin
  Op1Result := FOperand1.Evaluate(AContext, AEnvironment);
  try
    Op2Result := FOperand2.Evaluate(AContext, AEnvironment);
    try
      NodeSet := Op1Result.AsNodeSet;
      NodeSet2 := Op2Result.AsNodeSet;
      try
        for i := 0 to NodeSet2.Count - 1 do
        begin
          DoAdd := True;
          CurNode := NodeSet2[i];
          for j := 0 to NodeSet.Count - 1 do
            if NodeSet[j] = CurNode then
            begin
              DoAdd := False;
          break;
            end;
          if DoAdd then
            NodeSet.Add(CurNode);
        end;
      finally
        NodeSet2.Free;
      end;
    finally
      Op2Result.Release;
    end;
  finally
    Op1Result.Release;
  end;
  Result := TXPathNodeSetVariable.Create(NodeSet);
end;


constructor TXPathFilterNode.Create(AExpr: TXPathExprNode);
begin
  inherited Create;
  FExpr := AExpr;
  FPredicates := TList.Create;
end;

destructor TXPathFilterNode.Destroy;
var
  i: Integer;
begin
  for i := 0 to FPredicates.Count - 1 do
    TXPathExprNode(FPredicates[i]).Free;
  FPredicates.Free;
  inherited Destroy;
end;

function TXPathFilterNode.Evaluate(AContext: TXPathContext;
  AEnvironment: TXPathEnvironment): TXPathVariable;
var
  ExprResult, PredicateResult: TXPathVariable;
  NodeSet, NewNodeSet: TNodeSet;
  i, j: Integer;
  CurContextNode: TDOMNode;
  NewContext: TXPathContext;
  DoAdd: Boolean;
begin
  ExprResult := FExpr.Evaluate(AContext, AEnvironment);
  NewContext := nil;
  NewNodeSet := nil;
  try
    NodeSet := ExprResult.AsNodeSet;
    NewContext := TXPathContext.Create(nil, 0, NodeSet.Count);
    NewNodeSet := TNodeSet.Create;
    try
      for i := 0 to NodeSet.Count - 1 do
      begin
        CurContextNode := TDOMNode(NodeSet[i]);
        NewContext.ContextNode := CurContextNode;
        Inc(NewContext.ContextPosition);
        DoAdd := True;
        for j := 0 to FPredicates.Count - 1 do
        begin
          PredicateResult := TXPathExprNode(FPredicates[j]).Evaluate(NewContext,
            AEnvironment);
          try
            if PredicateResult.InheritsFrom(TXPathNumberVariable) then
            begin
              if PredicateResult.AsNumber <> i + 1 then
              begin
                DoAdd := False;
                break;
              end;
            end else if not PredicateResult.AsBoolean then
            begin
              DoAdd := False;
              break;
            end;
          finally
            PredicateResult.Release;
          end;
        end;
        if DoAdd then
          NewNodeSet.Add(CurContextNode);
      end;
    except
      NewNodeSet.Free;
      raise;
    end;
    Result := TXPathNodeSetVariable.Create(NewNodeSet);
  finally
    NewContext.Free;
    ExprResult.Release;
  end;
end;


constructor TStep.Create;
begin
  inherited Create;
  Predicates := TList.Create;
end;

destructor TStep.Destroy;
var
  i: Integer;
begin
  for i := 0 to Predicates.Count - 1 do
    TXPathExprNode(Predicates[i]).Free;
  Predicates.Free;
  inherited Free;
end;

constructor TXPathLocationPathNode.Create(AIsAbsolutePath: Boolean);
begin
  inherited Create;
  FIsAbsolutePath := AIsAbsolutePath;
end;

function TXPathLocationPathNode.Evaluate(AContext: TXPathContext;
  AEnvironment: TXPathEnvironment): TXPathVariable;
var
  ResultNodeSet: TNodeSet;

  procedure EvaluateStep(AStep: TStep; AContext: TXPathContext);
  var
    StepNodes: TList;

    procedure DoNodeTest(Node: TDOMNode);
    var
      i: Integer;
      DoAdd: Boolean;
    begin
      case AStep.NodeTestType of
        ntAnyPrincipal:
          // !!!: Probably this isn't ready for namespace support yet
          if (AStep.Axis <> axisAttribute) and
            (Node.NodeType <> ELEMENT_NODE) then
            exit;
        ntName:
          if Node.NodeName <> AStep.NodeTestString then
            exit;
        ntTextNode:
          if not Node.InheritsFrom(TDOMCharacterData) then
            exit;
        ntCommentNode:
          if Node.NodeType <> COMMENT_NODE then
            exit;
        ntPINode:
          if Node.NodeType <> PROCESSING_INSTRUCTION_NODE then
            exit;
      end;
      DoAdd := True;
      for i := 0 to StepNodes.Count - 1 do
        if TDOMNode(StepNodes[i]) = Node then
        begin
          DoAdd := False;
          break;
        end;
      if DoAdd then
        StepNodes.Add(Node);
    end;

    procedure AddDescendants(CurNode: TDOMNode);
    var
      Child: TDOMNode;
    begin
      Child := CurNode.FirstChild;
      while Assigned(Child) do
      begin
        DoNodeTest(Child);
        AddDescendants(Child);
        Child := Child.NextSibling;
      end;
    end;

  var
    Node, Node2: TDOMNode;
    Attr: TDOMNamedNodeMap;
    i, j: Integer;
    DoAdd: Boolean;

    NewContext: TXPathContext;
    NewStepNodes: TNodeSet;
    Predicate: TXPathExprNode;
    PredicateResult: TXPathVariable;

  begin
    StepNodes := TList.Create;
    // !!!: Protect this with an try/finally block
    case AStep.Axis of
      axisAncestor:
        begin
          Node := AContext.ContextNode.ParentNode;
          while Assigned(Node) do
          begin
            DoNodeTest(Node);
            Node := Node.ParentNode;
          end;
        end;
      axisAncestorOrSelf:
        begin
          Node := AContext.ContextNode;
          repeat
            DoNodeTest(Node);
            Node := Node.ParentNode;
         until not Assigned(Node);
        end;
      axisAttribute:
        begin
          Attr := AContext.ContextNode.Attributes;
          if Assigned(Attr) then
            for i := 0 to Attr.Length - 1 do
              DoNodeTest(Attr[i]);
        end;
      axisChild:
        begin
          Node := AContext.ContextNode.FirstChild;
          while Assigned(Node) do
          begin
            DoNodeTest(Node);
            Node := Node.NextSibling;
          end;
        end;
      axisDescendant:
        AddDescendants(AContext.ContextNode);
      axisDescendantOrSelf:
        begin
          DoNodeTest(AContext.ContextNode);
          AddDescendants(AContext.ContextNode);
        end;
      axisFollowing:
        begin
          Node := AContext.ContextNode;
          repeat
            Node2 := Node.NextSibling;
            while Assigned(Node2) do
            begin
              DoNodeTest(Node2);
              AddDescendants(Node2);
              Node := Node.NextSibling;
            end;
            Node := Node.ParentNode;
          until not Assigned(Node);
        end;
      axisFollowingSibling:
        begin
          Node := AContext.ContextNode.NextSibling;
          while Assigned(Node) do
          begin
            DoNodeTest(Node);
            Node := Node.NextSibling;
          end;
        end;
      {axisNamespace: !!!: Not supported yet}
      axisParent:
        if Assigned(AContext.ContextNode.ParentNode) then
          DoNodeTest(AContext.ContextNode);
      axisPreceding:
        begin
          Node := AContext.ContextNode;
          repeat
            Node2 := Node.PreviousSibling;
            while Assigned(Node2) do
            begin
              DoNodeTest(Node2);
              AddDescendants(Node2);
              Node := Node.PreviousSibling;
            end;
            Node := Node.ParentNode;
          until not Assigned(Node);
        end;
      axisPrecedingSibling:
        begin
          Node := AContext.ContextNode.PreviousSibling;
          while Assigned(Node) do
          begin
            DoNodeTest(Node);
            Node := Node.PreviousSibling;
          end;
        end;
      axisSelf:
        DoNodeTest(AContext.ContextNode);
    end;

    { Filter the nodes of this step using the predicates: The current
      node set (StepNodes) is filtered, all passed nodes will be added
      to NewStepNodes. After one filter has been applied, NewStepNodes
      gets copied to StepNodes, and the next filter will be processed.
      The final result will then be passed to the next step, or added
      to the result of the LocationPath if this is the last step. }

    for i := 0 to AStep.Predicates.Count - 1 do
    begin
      NewContext := TXPathContext.Create(nil, 0, StepNodes.Count);
      NewStepNodes := nil;
      try
        NewStepNodes := TNodeSet.Create;
        Predicate := TXPathExprNode(AStep.Predicates[i]);
        for j := 0 to StepNodes.Count - 1 do
        begin
          Node := TDOMNode(StepNodes[j]);
          NewContext.ContextNode := Node;
          Inc(NewContext.ContextPosition);
          PredicateResult := Predicate.Evaluate(NewContext, AEnvironment);
          try
            if (PredicateResult.InheritsFrom(TXPathNumberVariable) and
              (PredicateResult.AsNumber = j + 1)) or
              PredicateResult.AsBoolean then
                NewStepNodes.Add(Node);
          finally
            PredicateResult.Release;
          end;
        end;
      finally
        NewContext.Free;
        StepNodes.Free;
        StepNodes := NewStepNodes;
      end;
    end;

    if Assigned(AStep.NextStep) then
    begin
      NewContext := TXPathContext.Create(nil, 0, StepNodes.Count);
      try
        for i := 0 to StepNodes.Count - 1 do
        begin
          NewContext.ContextNode := TDOMNode(StepNodes[i]);
          Inc(NewContext.ContextPosition);
          EvaluateStep(AStep.NextStep, NewContext);
        end;
      finally
        NewContext.Free;
      end;
    end else
    begin
      // Only add nodes to result if it isn't duplicate
      for i := 0 to StepNodes.Count - 1 do
      begin
        Node := TDOMNode(StepNodes[i]);
        DoAdd := True;
        for j := 0 to ResultNodeSet.Count - 1 do
          if TDOMNode(ResultNodeSet[j]) = Node then
          begin
            DoAdd := False;
            break;
          end;
        if DoAdd then
          ResultNodeSet.Add(Node);
      end;
    end;

    StepNodes.Free;
  end;

var
  NewContext: TXPathContext;
begin
  ResultNodeSet := TNodeSet.Create;
  try
    if FIsAbsolutePath then
    begin
      NewContext := TXPathContext.Create(AContext.ContextNode.OwnerDocument,
        1, 1);
      try
        EvaluateStep(FFirstStep, NewContext);
      finally
        NewContext.Free;
      end;
    end else
    begin
      EvaluateStep(FFirstStep, AContext);
    end;
  except
    ResultNodeSet.Free;
    raise;
  end;
  Result := TXPathNodeSetVariable.Create(ResultNodeSet);
end;


{ Exceptions }

procedure EvaluationError(const Msg: String);
begin
  raise EXPathEvaluationError.Create(Msg) at get_caller_addr(get_frame);
end;

procedure EvaluationError(const Msg: String; const Args: array of const);
begin
  raise EXPathEvaluationError.CreateFmt(Msg, Args)
    at get_caller_addr(get_frame);
end;


{ TXPathVariable and derived classes}

procedure TXPathVariable.Release;
begin
  if FRefCount <= 0 then
    Free
  else
    Dec(FRefCount);
end;

function TXPathVariable.AsNodeSet: TNodeSet;
begin
  Error(SVarNoConversion, [TypeName, TXPathNodeSetVariable.TypeName]);
  Result := nil;
end;

function TXPathVariable.AsBoolean: Boolean;
begin
  Error(SVarNoConversion, [TypeName, TXPathBooleanVariable.TypeName]);
  Result := False;
end;

function TXPathVariable.AsNumber: Extended;
begin
  Error(SVarNoConversion, [TypeName, TXPathNumberVariable.TypeName]);
  Result := 0;
end;

function TXPathVariable.AsText: DOMString;
begin
  Error(SVarNoConversion, [TypeName, TXPathStringVariable.TypeName]);
  SetLength(Result, 0);
end;

procedure TXPathVariable.Error(const Msg: String; const Args: array of const);
begin
  raise Exception.CreateFmt(Msg, Args) at get_caller_addr(get_frame);
end;


constructor TXPathNodeSetVariable.Create(AValue: TNodeSet);
begin
  inherited Create;
  FValue := AValue;
end;

destructor TXPathNodeSetVariable.Destroy;
begin
  FValue.Free;
  inherited Destroy;
end;

class function TXPathNodeSetVariable.TypeName: String;
begin
  Result := SNodeSet;
end;

function TXPathNodeSetVariable.AsNodeSet: TNodeSet;
begin
  Result := FValue;
end;

function TXPathNodeSetVariable.AsText: DOMString;
var
  i: Integer;
begin
  if FValue.Count = 0 then
    SetLength(Result, 0)
  else
  begin
    Result := '';
    for i := 0 to FValue.Count - 1 do
    begin
      if i > 0 then
        Result := Result + LineEnding;
      Result := Result + NodeToText(TDOMNode(FValue[i]));
    end;
  end;
end;


constructor TXPathBooleanVariable.Create(AValue: Boolean);
begin
  inherited Create;
  FValue := AValue;
end;

class function TXPathBooleanVariable.TypeName: String;
begin
  Result := SBoolean;
end;

function TXPathBooleanVariable.AsBoolean: Boolean;
begin
  Result := FValue;
end;

function TXPathBooleanVariable.AsNumber: Extended;
begin
  if FValue then
    Result := 1
  else
    Result := 0;
end;

function TXPathBooleanVariable.AsText: DOMString;
begin
  if FValue then
    Result := 'true'    // Do not localize!
  else
    Result := 'false';  // Do not localize!
end;


constructor TXPathNumberVariable.Create(AValue: Extended);
begin
  inherited Create;
  FValue := AValue;
end;

class function TXPathNumberVariable.TypeName: String;
begin
  Result := SNumber;
end;

function TXPathNumberVariable.AsBoolean: Boolean;
begin
  // !!!: What about NaNs and so on?
  if FValue = 0 then
    Result := False
  else
    Result := True;
end;

function TXPathNumberVariable.AsNumber: Extended;
begin
  Result := FValue;
end;

function TXPathNumberVariable.AsText: DOMString;
begin
  Result := FloatToStr(FValue);
end;


constructor TXPathStringVariable.Create(const AValue: DOMString);
begin
  inherited Create;
  FValue := AValue;
end;

class function TXPathStringVariable.TypeName: String;
begin
  Result := SString;
end;

function TXPathStringVariable.AsBoolean: Boolean;
begin
  Result := Length(FValue) > 0;
end;

function TXPathStringVariable.AsNumber: Extended;
begin
  Result := StrToFloat(FValue);
end;

function TXPathStringVariable.AsText: DOMString;
begin
  Result := FValue;
end;


{ XPath lexical scanner }

constructor TXPathScanner.Create(const AExpressionString: DOMString);
begin
  inherited Create;
  FExpressionString := PWideChar(AExpressionString);
  FCurData := FExpressionString;
end;

function TXPathScanner.NextToken: TXPathToken;

  procedure GetNumber;
  var
    HasDot: Boolean;
  begin
    HasDot := Pos('.', FCurTokenString) > 0;
    while (FCurData[1] in ['0'..'9']) or ((FCurData[1] = '.') and not HasDot) do
    begin
      Inc(FCurData);
      FCurTokenString := FCurTokenString + FCurData[0];
      if FCurData[0] = '.' then
        HasDot := True;
    end;
    Result := tkNumber;
  end;

const
  IdentifierChars = ['A'..'Z', 'a'..'z', '0'..'9', '.', '-', '_'];
begin
  if FDoUnget then
  begin
    FDoUnget := False;
    Result := FCurToken;
    exit;
  end;

  if FCurToken = tkEndOfStream then
  begin
    Result := tkEndOfStream;
    exit;
  end;

  { No, we cannot use a lookup table here, as future
    versions will use WideStrings  -sg }

  // Skip whitespace
  while FCurData[0] in [#9, #10, #12, #13, ' '] do
    Inc(FCurData);

  FCurTokenString := FCurData[0];

  case FCurData[0] of
    #0:
      Result := tkEndOfStream;
    '!':
      if FCurData[1] = '=' then
      begin
        Inc(FCurData);
        Result := tkNotEqual;
      end;
    '"':
      begin
        SetLength(FCurTokenString, 0);
        Inc(FCurData);
        while FCurData[0] <> '"' do
        begin
          if FCurData[0] = #0 then
            Error(SScannerQuotStringIsOpen);
          FCurTokenString := FCurTokenString + FCurData[0];
          Inc(FCurData);
        end;
        Result := tkString;
      end;
    '$':
      Result := tkDollar;
    '''':
      begin
        SetLength(FCurTokenString, 0);
        Inc(FCurData);
        while FCurData[0] <> '''' do
        begin
          if FCurData[0] = #0 then
            Error(SScannerAposStringIsOpen);
          FCurTokenString := FCurTokenString + FCurData[0];
          Inc(FCurData);
        end;
        Result := tkString;
      end;
    '(':
      Result := tkLeftBracket;
    ')':
      Result := tkRightBracket;
    '*':
      Result := tkAsterisk;
    '+':
      Result := tkPlus;
    ',':
      Result := tkComma;
    '-':
      Result := tkMinus;
    '.':
      if FCurData[1] = '.' then
      begin
        Inc(FCurData);
        Result := tkDotDot;
      end else if FCurData[1] in ['0'..'9'] then
        GetNumber
      else
        Result := tkDot;
    '/':
      if FCurData[1] = '/' then
      begin
        Inc(FCurData);
        Result := tkSlashSlash;
      end else
        Result := tkSlash;
    '0'..'9':
      GetNumber;
    ':':
      if FCurData[1] = ':' then
      begin
        Inc(FCurData);
        Result := tkColonColon;
      end else
        Result := tkColon;
    '<':
      if FCurData[1] = '=' then
      begin
        Inc(FCurData);
        Result := tkLessEqual;
      end else
        Result := tkLess;
    '=':
      Result := tkEqual;
    '>':
      if FCurData[1] = '=' then
      begin
        Inc(FCurData);
        Result := tkGreaterEqual;
      end else
        Result := tkGreater;
    '@':
      Result := tkAt;
    'A'..'Z', 'a'..'z':
      begin
        Result := tkIdentifier;
        while FCurData[1] in IdentifierChars do
        begin
          Inc(FCurData);
          FCurTokenString := FCurTokenString + FCurData[0];
        end;
      end;
    '[':
      Result := tkLeftSquareBracket;
    ']':
      Result := tkRightSquareBracket;
    '|':
      Result := tkPipe;
    else
      Error(SScannerInvalidChar);
  end;

  // We have processed at least one character now; eat it:
  if Result <> tkEndOfStream then
    Inc(FCurData);

  FCurToken := Result;
end;

procedure TXPathScanner.UngetToken;
begin
  if FDoUnget then
    Error(SScannerInternalError, ['Tried to unget token a second time']);
  FDoUnget := True;
end;

function TXPathScanner.SaveState: TXPathScannerState;
begin
  Result := TXPathScannerState.Create;
  Result.FCurData := FCurData;
  Result.FCurToken := FCurToken;
  Result.FCurTokenString := FCurTokenString;
  Result.FDoUnget := FDoUnget;
end;

procedure TXPathScanner.RestoreState(AState: TXPathScannerState);
begin
  FCurData := AState.FCurData;
  FCurToken := AState.FCurToken;
  FCurTokenString := AState.FCurTokenString;
  FDoUnget := AState.FDoUnget;
  AState.Free;
end;

procedure TXPathScanner.Error(const Msg: String);
begin
  raise Exception.Create(Msg) at get_caller_addr(get_frame);
end;

procedure TXPathScanner.Error(const Msg: String; const Args: array of const);
begin
  raise Exception.CreateFmt(Msg, Args) at get_caller_addr(get_frame);
end;


{ TXPathContext }

constructor TXPathContext.Create(AContextNode: TDOMNode;
  AContextPosition, AContextSize: Integer);
begin
  inherited Create;
  ContextNode := AContextNode;
  ContextPosition := AContextPosition;
  ContextSize := AContextSize;
end;


{ TXPathEnvironment }

type
  PFunctionInfo = ^TFunctionInfo;
  TFunctionInfo = record
    Name: String;
    Fn: TXPathFunction;
  end;

  PVariableInfo = ^TVariableInfo;
  TVariableInfo = record
    Name: String;
    Variable: TXPathVariable;
  end;


constructor TXPathEnvironment.Create;
begin
  inherited Create;
  FFunctions := TList.Create;
  FVariables := TList.Create;

  // Add the functions of the XPath Core Function Library

  // Node set functions
  AddFunction('last', @xpLast);
  AddFunction('position', @xpPosition);
  AddFunction('count', @xpCount);
  AddFunction('id', @xpId);
  AddFunction('local-name', @xpLocalName);
  AddFunction('namespace-uri', @xpNamespaceURI);
  AddFunction('name', @xpName);
  // String functions
  AddFunction('string', @xpString);
  AddFunction('concat', @xpConcat);
  AddFunction('starts-with', @xpStartsWith);
  AddFunction('contains', @xpContains);
  AddFunction('substring-before', @xpSubstringBefore);
  AddFunction('substring-after', @xpSubstringAfter);
  AddFunction('substring', @xpSubstring);
  AddFunction('string-length', @xpStringLength);
  AddFunction('normalize-space', @xpNormalizeSpace);
  AddFunction('translate', @xpTranslate);
  // Boolean functions
  AddFunction('boolean', @xpBoolean);
  AddFunction('not', @xpNot);
  AddFunction('true', @xpTrue);
  AddFunction('false', @xpFalse);
  AddFunction('lang', @xpLang);
  // Number functions
  AddFunction('number', @xpNumber);
  AddFunction('sum', @xpSum);
  AddFunction('floor', @xpFloor);
  AddFunction('ceiling', @xpCeiling);
  AddFunction('round', @xpRound);
end;

destructor TXPathEnvironment.Destroy;
var
  i: Integer;
  FunctionInfo: PFunctionInfo;
  VariableInfo: PVariableInfo;
begin
  for i := 0 to FFunctions.Count - 1 do
  begin
    FunctionInfo := PFunctionInfo(FFunctions[i]);
    FreeMem(FunctionInfo);
  end;
  FFunctions.Free;
  for i := 0 to FVariables.Count - 1 do
  begin
    VariableInfo := PVariableInfo(FVariables[i]);
    FreeMem(VariableInfo);
  end;
  FVariables.Free;
  inherited Destroy;
end;

function TXPathEnvironment.GetFunctionIndex(const AName: String): Integer;
var
  i: Integer;
begin
  for i := 0 to FFunctions.Count - 1 do
    if PFunctionInfo(FFunctions[i])^.Name = AName then
    begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

function TXPathEnvironment.GetVariableIndex(const AName: String): Integer;
var
  i: Integer;
begin
  for i := 0 to FVariables.Count - 1 do
    if PVariableInfo(FFunctions[i])^.Name = AName then
    begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

procedure TXPathEnvironment.AddFunction(const AName: String; AFunction: TXPathFunction);
var
  NewFunctionInfo: PFunctionInfo;
begin
  // !!!: Prevent the addition of duplicate functions
  New(NewFunctionInfo);
  NewFunctionInfo^.Name := AName;
  NewFunctionInfo^.Fn := AFunction;
  FFunctions.Add(NewFunctionInfo);
end;

procedure TXPathEnvironment.AddVariable(const AName: String; AVariable: TXPathVariable);
var
  NewVariableInfo: PVariableInfo;
begin
  // !!!: Prevent the addition of duplicate variables
  New(NewVariableInfo);
  NewVariableInfo^.Name := AName;
  NewVariableInfo^.Variable := AVariable;
  FVariables.Add(NewVariableInfo);
end;

procedure TXPathEnvironment.RemoveFunction(Index: Integer);
var
  FunctionInfo: PFunctionInfo;
begin
  FunctionInfo := PFunctionInfo(FFunctions[Index]);
  Dispose(FunctionInfo);
  FFunctions.Delete(Index);
end;

procedure TXPathEnvironment.RemoveFunction(const AName: String);
var
  i: Integer;
begin
  for i := 0 to FFunctions.Count - 1 do
    if PFunctionInfo(FFunctions[i])^.Name = AName then
    begin
      RemoveFunction(i);
      exit;
    end;
end;

procedure TXPathEnvironment.RemoveVariable(Index: Integer);
var
  VariableInfo: PVariableInfo;
begin
  VariableInfo := PVariableInfo(FVariables[Index]);
  Dispose(VariableInfo);
  FVariables.Delete(Index);
end;

procedure TXPathEnvironment.RemoveVariable(const AName: String);
var
  Index: Integer;
begin
  Index := GetVariableIndex(AName);
  if Index >= 0 then
    RemoveVariable(Index);
end;

function TXPathEnvironment.GetFunctionCount: Integer;
begin
  Result := FFunctions.Count;
end;

function TXPathEnvironment.GetVariableCount: Integer;
begin
  Result := FVariables.Count;
end;

function TXPathEnvironment.GetFunction(Index: Integer): TXPathFunction;
begin
  Result := PFunctionInfo(FFunctions[Index])^.Fn;
end;

function TXPathEnvironment.GetFunction(const AName: String): TXPathFunction;
var
  i: Integer;
begin
  for i := 0 to FFunctions.Count - 1 do
    if PFunctionInfo(FFunctions[i])^.Name = AName then
    begin
      Result := PFunctionInfo(FFunctions[i])^.Fn;
      exit;
    end;
  Result := nil;
end;

function TXPathEnvironment.GetVariable(Index: Integer): TXPathVariable;
begin
  Result := PVariableInfo(FVariables[Index])^.Variable;
end;

function TXPathEnvironment.GetVariable(const AName: String): TXPathVariable;
var
  i: Integer;
begin
  for i := 0 to FVariables.Count - 1 do
    if PFunctionInfo(FVariables[i])^.Name = AName then
    begin
      Result := PVariableInfo(FVariables[i])^.Variable;
      exit;
    end;
  Result := nil;
end;

function TXPathEnvironment.xpLast(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if Args.Count <> 0 then
    EvaluationError(SEvalInvalidArgCount);
  Result := TXPathNumberVariable.Create(Context.ContextSize);
end;

function TXPathEnvironment.xpPosition(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if Args.Count <> 0 then
    EvaluationError(SEvalInvalidArgCount);
  Result := TXPathNumberVariable.Create(Context.ContextPosition);
end;

function TXPathEnvironment.xpCount(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if Args.Count <> 1 then
    EvaluationError(SEvalInvalidArgCount);
  Result := TXPathNumberVariable.Create(TXPathVariable(Args[0]).AsNodeSet.Count);
end;

function TXPathEnvironment.xpId(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if Args.Count <> 1 then
    EvaluationError(SEvalInvalidArgCount);
  EvaluationError(SEvalFunctionNotImplementedYet, ['id']); // !!!
end;

function TXPathEnvironment.xpLocalName(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if Args.Count > 1 then
    EvaluationError(SEvalInvalidArgCount);
  EvaluationError(SEvalFunctionNotImplementedYet, ['local-name']); // !!!
end;

function TXPathEnvironment.xpNamespaceURI(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if Args.Count > 1 then
    EvaluationError(SEvalInvalidArgCount);
  EvaluationError(SEvalFunctionNotImplementedYet, ['namespace-uri']); // !!!
end;

function TXPathEnvironment.xpName(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
var
  NodeSet: TNodeSet;
begin
  if Args.Count <> 1 then
    EvaluationError(SEvalInvalidArgCount);
  NodeSet := TXPathVariable(Args[0]).AsNodeSet;
  if NodeSet.Count = 0 then
    Result := TXPathStringVariable.Create('')
  else
    // !!!: Probably not really correct regarding namespaces...
    Result := TXPathStringVariable.Create(TDOMNode(NodeSet[0]).NodeName);
end;

function TXPathEnvironment.xpString(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
var
  s: String;
begin
  if Args.Count > 1 then
    EvaluationError(SEvalInvalidArgCount);
  if Args.Count = 0 then
    s := NodeToText(Context.ContextNode)
  else
    s := TXPathVariable(Args[0]).AsText;
  Result := TXPathStringVariable.Create(s);
end;

function TXPathEnvironment.xpConcat(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
var
  i: Integer;
  s: DOMString;
begin
  if Args.Count < 2 then
    EvaluationError(SEvalInvalidArgCount);
  SetLength(s, 0);
  for i := 0 to Args.Count - 1 do
    s := s + TXPathVariable(Args[i]).AsText;
  Result := TXPathStringVariable.Create(s);
end;

function TXPathEnvironment.xpStartsWith(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if Args.Count <> 2 then
    EvaluationError(SEvalInvalidArgCount);
  EvaluationError(SEvalFunctionNotImplementedYet, ['namespace-uri']); // !!!
end;

function TXPathEnvironment.xpContains(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if Args.Count <> 2 then
    EvaluationError(SEvalInvalidArgCount);
  EvaluationError(SEvalFunctionNotImplementedYet, ['contains']); // !!!
end;

function TXPathEnvironment.xpSubstringBefore(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if Args.Count <> 2 then
    EvaluationError(SEvalInvalidArgCount);
  EvaluationError(SEvalFunctionNotImplementedYet, ['substring-before']); // !!!
end;

function TXPathEnvironment.xpSubstringAfter(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if Args.Count <> 1 then
    EvaluationError(SEvalInvalidArgCount);
  EvaluationError(SEvalFunctionNotImplementedYet, ['substring-after']); // !!!
end;

function TXPathEnvironment.xpSubstring(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if (Args.Count < 2) or (Args.Count > 3) then
    EvaluationError(SEvalInvalidArgCount);
  EvaluationError(SEvalFunctionNotImplementedYet, ['substring']); // !!!
end;

function TXPathEnvironment.xpStringLength(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
var
  s: DOMString;
begin
  if Args.Count < 1 then
    EvaluationError(SEvalInvalidArgCount);
  if Args.Count = 0 then
    s := NodeToText(Context.ContextNode)
  else
    s := TXPathVariable(Args[0]).AsText;
  Result := TXPathNumberVariable.Create(Length(s));
end;

function TXPathEnvironment.xpNormalizeSpace(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if Args.Count < 1 then
    EvaluationError(SEvalInvalidArgCount);
  EvaluationError(SEvalFunctionNotImplementedYet, ['normalize-space']); // !!!
end;

function TXPathEnvironment.xpTranslate(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if Args.Count <> 3 then
    EvaluationError(SEvalInvalidArgCount);
  EvaluationError(SEvalFunctionNotImplementedYet, ['translate']); // !!!
end;

function TXPathEnvironment.xpBoolean(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if Args.Count <> 1 then
    EvaluationError(SEvalInvalidArgCount);
  Result := TXPathBooleanVariable.Create(TXPathVariable(Args[0]).AsBoolean);
end;

function TXPathEnvironment.xpNot(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if Args.Count <> 1 then
    EvaluationError(SEvalInvalidArgCount);
  Result := TXPathBooleanVariable.Create(not TXPathVariable(Args[0]).AsBoolean);
end;

function TXPathEnvironment.xpTrue(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if Args.Count <> 0 then
    EvaluationError(SEvalInvalidArgCount);
  Result := TXPathBooleanVariable.Create(True);
end;

function TXPathEnvironment.xpFalse(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if Args.Count <> 0 then
    EvaluationError(SEvalInvalidArgCount);
  Result := TXPathBooleanVariable.Create(False);
end;

function TXPathEnvironment.xpLang(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if Args.Count <> 1 then
    EvaluationError(SEvalInvalidArgCount);
  EvaluationError(SEvalFunctionNotImplementedYet, ['lang']); // !!!
end;

function TXPathEnvironment.xpNumber(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if Args.Count > 1 then
    EvaluationError(SEvalInvalidArgCount);
  EvaluationError(SEvalFunctionNotImplementedYet, ['number']); // !!!
end;

function TXPathEnvironment.xpSum(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if Args.Count <> 1 then
    EvaluationError(SEvalInvalidArgCount);
  EvaluationError(SEvalFunctionNotImplementedYet, ['sum']); // !!!
end;

function TXPathEnvironment.xpFloor(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if Args.Count <> 1 then
    EvaluationError(SEvalInvalidArgCount);
  EvaluationError(SEvalFunctionNotImplementedYet, ['floor']); // !!!
end;

function TXPathEnvironment.xpCeiling(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if Args.Count <> 1 then
    EvaluationError(SEvalInvalidArgCount);
  EvaluationError(SEvalFunctionNotImplementedYet, ['ceiling']); // !!!
end;

function TXPathEnvironment.xpRound(Context: TXPathContext; Args: TXPathVarList): TXPathVariable;
begin
  if Args.Count <> 1 then
    EvaluationError(SEvalInvalidArgCount);
  EvaluationError(SEvalFunctionNotImplementedYet, ['round']); // !!!
end;



{ TXPathExpression }

constructor TXPathExpression.Create(AScanner: TXPathScanner;
  CompleteExpression: Boolean);

  function ParseLocationPath: TXPathLocationPathNode; forward;  // [1]
  function ParsePrimaryExpr: TXPathExprNode; forward;           // [15]
  function ParseUnionExpr: TXPathExprNode; forward;             // [18]
  function ParsePathExpr: TXPathExprNode; forward;              // [19]
  function ParseFilterExpr: TXPathExprNode; forward;            // [20]
  function ParseOrExpr: TXPathExprNode; forward;                // [21]
  function ParseAndExpr: TXPathExprNode; forward;               // [22]
  function ParseEqualityExpr: TXPathExprNode; forward;          // [23]
  function ParseRelationalExpr: TXPathExprNode; forward;        // [24]
  function ParseAdditiveExpr: TXPathExprNode; forward;          // [25]
  function ParseMultiplicativeExpr: TXPathExprNode; forward;    // [26]
  function ParseUnaryExpr: TXPathExprNode; forward;             // [27]

  procedure Error(const Msg: String);
  begin
    raise Exception.Create(Msg) at get_caller_addr(get_frame);
  end;

  procedure Error(const Msg: String; const Args: array of const);
  begin
    raise Exception.CreateFmt(Msg, Args) at get_caller_addr(get_frame);
  end;

  function ParseLocationPath: TXPathLocationPathNode;  // [1]
  var
    IsAbsolute, NeedColonColon: Boolean;
    FirstStep, CurStep, NextStep: TStep;
    NextToken: TXPathToken;
  begin
    IsAbsolute := False;
    CurStep := nil;
    Result := nil;

    case AScanner.NextToken of
      tkSlash:          // [2] AbsoluteLocationPath, first case
        begin
	  NextToken := AScanner.NextToken;
	  AScanner.UngetToken;
	  if NextToken = tkEndOfStream then
	  begin
            CurStep := TStep.Create;
            CurStep.Axis := axisSelf;
            CurStep.NodeTestType := ntAnyNode;
	  end else if not (NextToken in
            [tkDot, tkDotDot, tkAsterisk, tkAt, tkIdentifier, tkEndOfStream]) then
            exit;
          IsAbsolute := True;
        end;
      tkSlashSlash:     // [10] AbbreviatedAbsoluteLocationPath
        begin
          IsAbsolute := True;
          CurStep := TStep.Create;
          CurStep.Axis := axisDescendantOrSelf;
          CurStep.NodeTestType := ntAnyNode;
        end;
      else
      begin
        AScanner.UngetToken;
        IsAbsolute := False;
      end;
    end;

    // Parse [3] RelativeLocationPath
    FirstStep := CurStep;
    while True do
    begin
      NextToken := AScanner.NextToken;
      if NextToken <> tkEndOfStream then
      begin
        NextStep := TStep.Create;
        if Assigned(CurStep) then
          CurStep.NextStep := NextStep
        else
          FirstStep := NextStep;
        CurStep := NextStep;
      end;

      // Parse [4] Step
      case NextToken of
        tkDot:          // [12] Abbreviated step, first case
          begin
            CurStep.Axis := axisSelf;
            CurStep.NodeTestType := ntAnyNode;
          end;
        tkDotDot:	// [12] Abbreviated step, second case
          begin
            CurStep.Axis := axisParent;
            CurStep.NodeTestType := ntAnyNode;
          end;
        else		// Parse [5] AxisSpecifier
	begin
	  case NextToken of
            tkAt:               // [13] AbbreviatedAxisSpecifier
              CurStep.Axis := axisAttribute;
            tkIdentifier:       // [5] AxisName '::'
              begin
                // Check for [6] AxisName
                NeedColonColon := True;
                if AScanner.CurTokenString = 'ancestor' then
                  CurStep.Axis := axisAncestor
                else if AScanner.CurTokenString = 'ancestor-or-self' then
                  CurStep.Axis := axisAncestorOrSelf
                else if AScanner.CurTokenString = 'attribute' then
                  CurStep.Axis := axisAttribute
                else if AScanner.CurTokenString = 'child' then
                  CurStep.Axis := axisChild
                else if AScanner.CurTokenString = 'descendant' then
                  CurStep.Axis := axisDescendant
                else if AScanner.CurTokenString = 'descendant-or-self' then
                  CurStep.Axis := axisDescendantOrSelf
                else if AScanner.CurTokenString = 'following' then
                  CurStep.Axis := axisFollowing
                else if AScanner.CurTokenString = 'following-sibling' then
                  CurStep.Axis := axisFollowingSibling
                else if AScanner.CurTokenString = 'namespace' then
                  CurStep.Axis := axisNamespace
                else if AScanner.CurTokenString = 'parent' then
                  CurStep.Axis := axisParent
                else if AScanner.CurTokenString = 'preceding' then
                  CurStep.Axis := axisPreceding
                else if AScanner.CurTokenString = 'preceding-sibling' then
                  CurStep.Axis := axisPrecedingSibling
                else if AScanner.CurTokenString = 'self' then
                  CurStep.Axis := axisSelf
                else
                begin
                  NeedColonColon := False;
                  AScanner.UngetToken;
                  CurStep.Axis := axisChild;
                end;
                if NeedColonColon and (AScanner.NextToken <> tkColonColon) then
                  Error(SParserExpectedColonColor);
              end;
            else
            begin
              AScanner.UngetToken;
	      if NextToken <> tkEndOfStream then
                CurStep.Axis := axisChild;
            end;
          end;

          // Parse [7] NodeTest
          case AScanner.NextToken of
            tkAsterisk: // [37] NameTest, first case
              CurStep.NodeTestType := ntAnyPrincipal;
            tkIdentifier:
              begin
                // Check for case [38] NodeType
                if AScanner.CurTokenString = 'comment' then
                begin
                  if (AScanner.NextToken <> tkLeftBracket) or
                    (AScanner.NextToken <> tkRightBracket) then
                    Error(SParserExpectedBrackets);
                  CurStep.NodeTestType := ntCommentNode;
                end else if AScanner.CurTokenString = 'text' then
                begin
                  if (AScanner.NextToken <> tkLeftBracket) or
                    (AScanner.NextToken <> tkRightBracket) then
                    Error(SParserExpectedBrackets);
                  CurStep.NodeTestType := ntTextNode;
                end else if AScanner.CurTokenString = 'processing-instruction' then
                begin
                  if (AScanner.NextToken <> tkLeftBracket) or
                    (AScanner.NextToken <> tkRightBracket) then
                    Error(SParserExpectedBrackets);
                  CurStep.NodeTestType := ntPINode;
                end else if AScanner.CurTokenString = 'node' then
                begin
                  if (AScanner.NextToken <> tkLeftBracket) or
                    (AScanner.NextToken <> tkRightBracket) then
                    Error(SParserExpectedBrackets);
                  CurStep.NodeTestType := ntAnyNode;
                end else  // [37] NameTest, second or third case
                begin
                  // !!!: Doesn't support namespaces yet
                  // (this will have to wait until the DOM unit supports them)
                  CurStep.NodeTestType := ntName;
                  CurStep.NodeTestString := AScanner.CurTokenString;
                end;
              end;
	    tkEndOfStream:	// Enable support of "/" and "//" as path
            else
              Error(SParserInvalidNodeTest);
          end;

          // Parse predicates
          while AScanner.NextToken = tkLeftSquareBracket do
          begin
            CurStep.Predicates.Add(ParseOrExpr);
            if AScanner.NextToken <> tkRightSquareBracket then
              Error(SParserExpectedRightSquareBracket);
          end;
          AScanner.UngetToken;
        end;
      end;

      // Continue with parsing of [3] RelativeLocationPath
      if AScanner.NextToken = tkSlashSlash then
      begin
        // Found abbreviated step ("//" for "descendant-or-self::node()")
        NextStep := TStep.Create;
        CurStep.NextStep := NextStep;
        CurStep := NextStep;
        CurStep.Axis := axisDescendantOrSelf;
        CurStep.NodeTestType := ntAnyNode;
      end else if AScanner.CurToken <> tkSlash then
      begin
        AScanner.UngetToken;
        break;
      end;
    end;

    Result := TXPathLocationPathNode.Create(IsAbsolute);
    TXPathLocationPathNode(Result).FFirstStep := FirstStep;
  end;

  function ParsePrimaryExpr: TXPathExprNode;  // [15]
  var
    IsFirstArg: Boolean;
  begin
    case AScanner.NextToken of
      tkDollar:         // [36] Variable reference
        begin
          if AScanner.NextToken <> tkIdentifier then
            Error(SParserExpectedVarName);
          Result := TXPathVariableNode.Create(AScanner.CurTokenString);
        end;
      tkLeftBracket:
        begin
          Result := ParseOrExpr;
          if AScanner.NextToken <> tkRightBracket then
            Error(SParserExpectedRightBracket);
        end;
      tkString:         // [29] Literal
        Result := TXPathConstantNode.Create(
          TXPathStringVariable.Create(AScanner.CurTokenString));
      tkNumber:         // [30] Number
        Result := TXPathConstantNode.Create(
          TXPathNumberVariable.Create(StrToFloat(AScanner.CurTokenString)));
      tkIdentifier:     // [16] Function call
        begin
          Result := TXPathFunctionNode.Create(AScanner.CurTokenString);
          if AScanner.NextToken <> tkLeftBracket then
            Error(SParserExpectedLeftBracket);
          // Parse argument list
          IsFirstArg := True;
          while AScanner.NextToken <> tkRightBracket do
          begin
            if IsFirstArg then
            begin
              IsFirstArg := False;
              AScanner.UngetToken;
            end else
              if AScanner.CurToken <> tkComma then
                Error(SParserExpectedRightBracket);
            TXPathFunctionNode(Result).FArgs.Add(ParseOrExpr);
          end;
        end;
      else
        Error(SParserInvalidPrimExpr);
    end;
  end;

  function ParseUnionExpr: TXPathExprNode;  // [18]
  begin
    Result := ParsePathExpr;
    while True do
      if AScanner.NextToken = tkPipe then
        Result := TXPathUnionNode.Create(Result, ParsePathExpr)
      else
      begin
        AScanner.UngetToken;
        break;
      end;
  end;

  function ParsePathExpr: TXPathExprNode;  // [19]
  var
    ScannerState: TXPathScannerState;
    IsFunctionCall: Boolean;
  begin
    // Try to detect wether a LocationPath [1] or a FilterExpr [20] follows

    IsFunctionCall := False;
    if (AScanner.NextToken = tkIdentifier) and
      (AScanner.CurTokenString <> 'comment') and
      (AScanner.CurTokenString <> 'text') and
      (AScanner.CurTokenString <> 'processing-instruction') and
      (AScanner.CurTokenString <> 'node') then
    begin
      ScannerState := AScanner.SaveState;
      if AScanner.NextToken = tkLeftBracket then
        IsFunctionCall := True;
      AScanner.RestoreState(ScannerState);
    end;

    if IsFunctionCall or (AScanner.CurToken in
      [tkDollar, tkLeftBracket, tkString, tkNumber]) then
    begin
      // second, third or fourth case of [19]
      AScanner.UngetToken;
      Result := ParseFilterExpr;
      // !!!: Doesn't handle "/" or "//" plus RelativeLocationPath yet!
    end else
    begin
      AScanner.UngetToken;
      Result := ParseLocationPath;
    end;
  end;

  function ParseFilterExpr: TXPathExprNode;  // [20]
  var
    IsFirst: Boolean;
  begin
    Result := ParsePrimaryExpr;
    // Parse predicates
    IsFirst := True;
    while AScanner.NextToken = tkLeftSquareBracket do
    begin
      if IsFirst then
      begin
        Result := TXPathFilterNode.Create(Result);
        IsFirst := False;
      end;
      TXPathFilterNode(Result).FPredicates.Add(ParseOrExpr);
      if AScanner.NextToken <> tkRightSquareBracket then
        Error(SParserExpectedRightSquareBracket);
    end;
    AScanner.UngetToken;
  end;

  function ParseOrExpr: TXPathExprNode;  // [21]
  begin
    Result := ParseAndExpr;
    while True do
      if (AScanner.NextToken = tkIdentifier) and
        (AScanner.CurTokenString = 'or') then
        Result := TXPathBooleanOpNode.Create(opOr, Result, ParseAndExpr)
      else
      begin
        AScanner.UngetToken;
        break;
      end;
  end;

  function ParseAndExpr: TXPathExprNode;  // [22]
  begin
    Result := ParseEqualityExpr;
    while True do
      if (AScanner.NextToken = tkIdentifier) and
        (AScanner.CurTokenString = 'and') then
        Result := TXPathBooleanOpNode.Create(opAnd, Result, ParseEqualityExpr)
      else
      begin
        AScanner.UngetToken;
        break;
      end;
  end;

  function ParseEqualityExpr: TXPathExprNode;  // [23]
  begin
    Result := ParseRelationalExpr;
    while True do
      case AScanner.NextToken of
        tkEqual:
          Result := TXPathBooleanOpNode.Create(opEqual, Result,
            ParseRelationalExpr);
        tkNotEqual:
          Result := TXPathBooleanOpNode.Create(opNotEqual, Result,
            ParseRelationalExpr);
        else
        begin
          AScanner.UngetToken;
          break;
        end;
      end;
  end;

  function ParseRelationalExpr: TXPathExprNode;  // [24]
  begin
    Result := ParseAdditiveExpr;
    while True do
      case AScanner.NextToken of
        tkLess:
          Result := TXPathBooleanOpNode.Create(opLess, Result,
            ParseAdditiveExpr);
        tkLessEqual:
          Result := TXPathBooleanOpNode.Create(opLessEqual, Result,
            ParseAdditiveExpr);
        tkGreater:
          Result := TXPathBooleanOpNode.Create(opGreater, Result,
            ParseAdditiveExpr);
        tkGreaterEqual:
          Result := TXPathBooleanOpNode.Create(opGreaterEqual, Result,
            ParseAdditiveExpr);
        else
        begin
          AScanner.UngetToken;
          break;
        end;
      end;
  end;

  function ParseAdditiveExpr: TXPathExprNode;  // [25]
  begin
    Result := ParseMultiplicativeExpr;
    while True do
      case AScanner.NextToken of
        tkPlus:
          Result := TXPathMathOpNode.Create(opAdd, Result,
            ParseMultiplicativeExpr);
        tkMinus:
          Result := TXPathMathOpNode.Create(opSubtract, Result,
            ParseMultiplicativeExpr);
        else
        begin
          AScanner.UngetToken;
          break;
        end;
      end;
  end;

  function ParseMultiplicativeExpr: TXPathExprNode;  // [26]
  begin
    Result := ParseUnaryExpr;
    while True do
      case AScanner.NextToken of
        tkAsterisk:
          Result := TXPathMathOpNode.Create(opMultiply, Result,
            ParseUnaryExpr);
        tkIdentifier:
          if AScanner.CurTokenString = 'div' then
            Result := TXPathMathOpNode.Create(opDivide, Result,
              ParseUnaryExpr)
          else if AScanner.CurTokenString = 'mod' then
            Result := TXPathMathOpNode.Create(opMod, Result,
              ParseUnaryExpr)
          else
          begin
            AScanner.UngetToken;
            break;
          end;
        else
        begin
          AScanner.UngetToken;
          break;
        end;
      end;
  end;

  function ParseUnaryExpr: TXPathExprNode;  // [27]
  var
    NegCount: Integer;
  begin
    NegCount := 0;
    while AScanner.NextToken = tkMinus do
      Inc(NegCount);
    AScanner.UngetToken;

    Result := ParseUnionExpr;

    if Odd(NegCount) then
      Result := TXPathNegationNode.Create(Result);
  end;

begin
  inherited Create;
  FRootNode := ParseOrExpr;
  if CompleteExpression and (AScanner.NextToken <> tkEndOfStream) then
    Error(SParserGarbageAfterExpression);
end;

function TXPathExpression.Evaluate(AContextNode: TDOMNode): TXPathVariable;
var
  Environment: TXPathEnvironment;
begin
  Environment := TXPathEnvironment.Create;
  try
    Result := Evaluate(AContextNode, Environment);
  finally
    Environment.Free;
  end;
end;

function TXPathExpression.Evaluate(AContextNode: TDOMNode;
  AEnvironment: TXPathEnvironment): TXPathVariable;
var
  Context: TXPathContext;
begin
  if Assigned(FRootNode) then
  begin
    Context := TXPathContext.Create(AContextNode, 1, 1);
    try
      Result := FRootNode.Evaluate(Context, AEnvironment);
    finally
      Context.Free;
    end;
  end else
    Result := nil;
end;


function EvaluateXPathExpression(const AExpressionString: DOMString;
  AContextNode: TDOMNode): TXPathVariable;
var
  Scanner: TXPathScanner;
  Expression: TXPathExpression;
begin
  Scanner := TXPathScanner.Create(AExpressionString);
  try
    Expression := TXPathExpression.Create(Scanner, True);
    try
      Result := Expression.Evaluate(AContextNode);
    finally
      Expression.Free;
    end;
  finally
    Scanner.Free;
  end;
end;

end.
