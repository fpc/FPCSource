unit jstree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jsbase;

Type
  TJSElementFlag = (elIsConst,elIsConstValid);
  TJSElementFlags = set of TJSElementFlag;

  TJSFunctionBody = Class;

  { TJSElement }

  TJSObject = Class(TObject);

  { TJSFuncDef }

  TJSFuncDef = Class(TJSObject)
  private
    FBody: TJSFunctionBody;
    FCache: TJSObject;
    FCommon: TJSObject;
    FIsEmpty: Boolean;
    FName: String;
    FNext: TJSFuncDef;
    FParams: TStrings;
    FSec: TObject;
    procedure SetParams(const AValue: TStrings);
  Public
    Constructor Create;
    Destructor Destroy; override;
    Property Params : TStrings Read FParams Write SetParams;
    Property Body : TJSFunctionBody Read FBody Write FBody;
    Property Name : String Read FName Write FName;
    Property Common : TJSObject Read FCommon Write FCommon;
    Property Cache : TJSObject Read FCache write FCache;
    Property Next : TJSFuncDef Read FNext Write FNext;
    Property IsEmpty : Boolean Read FIsEmpty Write FIsEmpty;
    Property SecurityDomain : TObject Read FSec Write FSec;
  end;

  TJSString = WideString;

  TJSElement = Class (TJSObject)
  private
    FFlags: TJSElementFlags;
    FLine: Integer;
    FRow: Integer;
    FSource: String;
  Public
    Constructor Create(ALine,ARow : Integer; Const ASource : String = ''); virtual;
    Property Source : String Read FSource Write FSource;
    Property Row : Integer Read FRow Write FRow;
    Property Line : Integer Read FLine Write FLine;
    Property Flags : TJSElementFlags Read FFlags Write FFlags;
  end;
  TJSElementClass = Class of TJSElement;

  { TJSEmptyBlockStatement }

  TJSEmptyBlockStatement = Class(TJSElement);
  TJSEmptyStatement = Class(TJSElement);

  { TJSLiteral }

  TJSLiteral = Class(TJSElement)
  private
    FValue: TJSValue;
  Public
    Constructor Create(ALine,ARow : Integer; Const ASource : String = ''); override;
    Destructor Destroy; override;
    Property Value : TJSValue Read FValue Write FValue;
  end;

(*  { TJSStringLiteral }

  TJSStringLiteral = Class(TJSElement)
  private
    FValue: TJSString;
  Public
    Property Value : TJSString Read FValue Write FValue;
  end;
*)
  { TJSRegularExpressionLiteral }

  TJSRegularExpressionLiteral = Class(TJSElement)
  private
    FPattern: TJSValue;
    FPatternFlags: TJSValue;
    FArgv : Array[0..1] of TJSValue;
    function GetA(AIndex : integer): TJSValue;
    procedure SetA(AIndex : integer; const AValue: TJSValue);
  Public
    Constructor Create(ALine,ARow : Integer; Const ASource : String = ''); override;
    Destructor Destroy; override;
    Property Pattern : TJSValue Read FPattern Write FPattern;
    Property PatternFlags : TJSValue Read FPatternFlags Write FPatternFlags;
    Property Argv[AIndex : integer] : TJSValue Read GetA Write SetA;
  end;

  { TJSPrimaryExpressionIdent }
  TJSPrimaryExpression = Class(TJSElement);

  TJSPrimaryExpressionIdent = Class(TJSPrimaryExpression)
  private
    FName: TJSString;
  Public
    Property Name : TJSString Read FName Write FName;
  end;
  TJSPrimaryExpressionThis = Class(TJSPrimaryExpression);

  { TJSArrayLiteralElement }

  TJSArrayLiteralElement = Class(TCollectionItem)
  private
    FExpr: TJSelement;
    FFindex: Integer;
  Public
    Destructor Destroy; override;
    Property Expr : TJSelement Read FExpr Write FExpr;
    Property ElementIndex : Integer Read FFindex Write FFIndex;
  end;

  { TJSArrayLiteralElements }

  TJSArrayLiteralElements = Class(TCollection)
  private
    function GetE(AIndex : Integer): TJSArrayLiteralElement;
    procedure SetE(AIndex : Integer; const AValue: TJSArrayLiteralElement);
  Public
    Function AddElement : TJSArrayLiteralElement;
    Property Elements[AIndex : Integer] : TJSArrayLiteralElement Read GetE Write SetE; default;
  end;

  { TJSArrayLiteral }

  TJSArrayLiteral = Class(TJSElement)
  private
    FElements: TJSArrayLiteralElements;
  Public
    Constructor Create(ALine,ARow : Integer; const ASource : String = ''); override;
    Destructor Destroy; override;
    Property Elements : TJSArrayLiteralElements Read FElements;
  end;

  { TJSObjectLiteralElement }

  TJSObjectLiteralElement = Class(TCollectionItem)
  private
    FExpr: TJSelement;
    FName: TJSString;
  Public
    Destructor Destroy; override;
    Property Expr : TJSelement Read FExpr Write FExpr;
    Property Name : TJSString Read FName Write FName;
  end;

  { TJSObjectLiteralElements }

  TJSObjectLiteralElements = Class(TCollection)
  private
    function GetE(AIndex : Integer): TJSObjectLiteralElement;
    procedure SetE(AIndex : Integer; const AValue: TJSObjectLiteralElement);
  Public
    Function AddElement : TJSObjectLiteralElement;
    Property Elements[AIndex : Integer] : TJSObjectLiteralElement Read GetE Write SetE; default;
  end;

  { TJSObjectLiteral }

  TJSObjectLiteral = Class(TJSElement)
  private
    FElements: TJSObjectLiteralElements;
  Public
    Constructor Create(ALine,ARow : Integer; const ASource : String = ''); override;
    Destructor Destroy; override;
    Property Elements : TJSObjectLiteralElements Read FElements;
  end;

  { TJSArguments }

  TJSArguments = Class(TJSArrayLiteral);

  { TJSMemberExpression }

  TJSMemberExpression = Class(TJSElement)
  private
    FMexpr: TJSElement;
  Public
    Destructor Destroy; override;
    Property Mexpr : TJSElement Read FMexpr Write FMexpr;
  end;

  { TJSNewMemberExpression }

  TJSNewMemberExpression = Class(TJSMemberExpression)
  private
    FArgs: TJSArguments;
  Public
    Destructor Destroy; override;
    Property Args : TJSArguments Read FArgs Write FArgs;
  end;

  { TJSDotMemberExpression }

  TJSDotMemberExpression = Class(TJSMemberExpression)
  private
    FName: TJSString;
  Public
    Property Name : TJSString Read FName Write FName;
  end;

  { TJSBracketMemberExpression }

  TJSBracketMemberExpression = Class(TJSMemberExpression)
  private
    FName: TJSElement;
  Public
    Destructor Destroy; override;
    Property Name : TJSElement Read FName Write FName;
  end;

  { TJSCallExpression }

  TJSCallExpression = Class(TJSElement)
  private
    FArgs: TJSArguments;
    FExpr: TJSElement;
  Public
    Destructor Destroy; override;
    Property Expr : TJSElement Read FExpr Write FExpr;
    Property Args : TJSArguments Read FArgs Write FArgs;
  end;

  { TJSUnary }

  TJSUnary = Class(TJSElement)
  private
    FA: TJSElement;
  Public
    Destructor Destroy; override;
    Property A : TJSElement Read FA Write FA;
  end;

  { TJSVariableStatement }
  TJSVariableStatement = Class(TJSUnary);
  TJSExpressionStatement = Class(TJSUnary);
  TJSThrowStatement = Class(TJSUnary);
  TJSUnaryExpression = Class(TJSUnary);
  TJSUnaryDeleteExpression = Class(TJSUnaryExpression);
  TJSUnaryVoidExpression = Class(TJSUnaryExpression);
  TJSUnaryTypeOfExpression = Class(TJSUnaryExpression);
  TJSUnaryPrePlusPlusExpression = Class(TJSUnaryExpression);
  TJSUnaryPreMinusMinusExpression = Class(TJSUnaryExpression);
  TJSUnaryPlusExpression = Class(TJSUnaryExpression);
  TJSUnaryMinusExpression = Class(TJSUnaryExpression);
  TJSUnaryInvExpression = Class(TJSUnaryExpression);
  TJSUnaryNotExpression = Class(TJSUnaryExpression);
  TJSUnaryPostPlusPlusExpression = Class(TJSUnaryExpression);
  TJSUnaryPostMinusMinusExpression = Class(TJSUnaryExpression);



  { TJSBinary }

  TJSBinary = Class(TJSElement)
  private
    FA: TJSElement;
    FB: TJSElement;
  Public
    Destructor Destroy; override;
    Property A : TJSElement Read FA Write FA;
    Property B : TJSElement Read FB Write FB;
  end;

  { TJSStatementList }

  TJSStatementList = Class(TJSBinary); // A->first statement, B->next in list, chained.
  TJSVariableDeclarationList = Class(TJSBinary);
  TJSWithStatement = Class(TJSBinary); // A-> with expression, B->statement(s)
  TJSLogicalOrExpression = Class (TJSBinary);
  TJSLogicalAndExpression = Class (TJSBinary);
  TJSBitwiseAndExpression = Class (TJSBinary);
  TJSBitwiseOrExpression = Class (TJSBinary);
  TJSBitwiseXOrExpression = Class (TJSBinary);
  TJSEqualityExpression = Class (TJSBinary);
  TJSEqualityExpressionEQ = Class(TJSEqualityExpression);
  TJSEqualityExpressionNE = Class(TJSEqualityExpression);
  TJSEqualityExpressionSEQ = Class(TJSEqualityExpression);
  TJSEqualityExpressionSNE = Class(TJSEqualityExpression);
  TJSRelationalExpression = Class(TJSBinary);
  TJSRelationalExpressionLT = Class(TJSRelationalExpression);
  TJSRelationalExpressionGT = Class(TJSRelationalExpression);
  TJSRelationalExpressionLE = Class(TJSRelationalExpression);
  TJSRelationalExpressionGE = Class(TJSRelationalExpression);
  TJSRelationalExpressionIn = Class(TJSRelationalExpression);
  TJSRelationalExpressionInstanceOf = Class(TJSRelationalExpression);
  TJSShiftExpression = Class(TJSBinary);
  TJSLShiftExpression = Class(TJSShiftExpression);
  TJSRShiftExpression = Class(TJSShiftExpression);
  TJSURShiftExpression = Class(TJSShiftExpression);
  TJSAdditiveExpression = Class(TJSBinary);
  TJSAdditiveExpressionPlus = Class(TJSAdditiveExpression);
  TJSAdditiveExpressionMinus = Class(TJSAdditiveExpression);
  TJSMultiplicativeExpression = Class(TJSBinary);
  TJSMultiplicativeExpressionMul = Class(TJSMultiplicativeExpression);
  TJSMultiplicativeExpressionDiv = Class(TJSMultiplicativeExpression);
  TJSMultiplicativeExpressionMod = Class(TJSMultiplicativeExpression);
  TJSCommaExpression = Class(TJSBinary);

  { TJSConditionalExpression }

  TJSConditionalExpression = Class(TJSElement)
  private
    FA: TJSElement;
    FB: TJSElement;
    FC: TJSElement;
  Public
    Destructor Destroy; override;
    Property A : TJSElement Read FA Write FA;
    Property B : TJSElement Read FB Write FB;
    Property C : TJSElement Read FC Write FC;
  end;

  { TJSAssignStatement }

  TJSAssignStatement = Class(TJSElement)
  private
    FExpr: TJSElement;
    FLHS: TJSElement;
  Public
    Destructor Destroy; override;
    Property Expr : TJSElement Read FExpr Write FExpr;
    Property LHS : TJSElement Read FLHS Write FLHS;
  end;

  TJSSimpleAssignStatement = Class(TJSAssignStatement);
  TJSMulEqAssignStatement = Class(TJSAssignStatement);
  TJSDivEqAssignStatement = Class(TJSAssignStatement);
  TJSModEqAssignStatement = Class(TJSAssignStatement);
  TJSAddEqAssignStatement = Class(TJSAssignStatement);
  TJSSubEqAssignStatement = Class(TJSAssignStatement);
  TJSLShiftEqAssignStatement = Class(TJSAssignStatement);
  TJSRShiftEqAssignStatement = Class(TJSAssignStatement);
  TJSURShiftEqAssignStatement = Class(TJSAssignStatement);
  TJSANDEqAssignStatement = Class(TJSAssignStatement);
  TJSOREqAssignStatement = Class(TJSAssignStatement);
  TJSXOREqAssignStatement = Class(TJSAssignStatement);

  { TJSVarDeclaration }

  TJSVarDeclaration = Class(TJSElement)
  private
    FInit: TJSElement;
    FName: String;
  Public
    Destructor Destroy; override;
    Property Name : String Read FName Write FName;
    Property Init : TJSElement Read FInit Write FInit;
  end;

  { TJSIfStatement }

  TJSIfStatement = Class(TJSElement)
  private
    FBFalse: TJSElement;
    FBTrue: TJSElement;
    FCond: TJSElement;
  Public
    Destructor Destroy; override;
    Property Cond : TJSElement Read FCond Write FCond;
    Property btrue : TJSElement Read FBTrue Write FBTrue;
    Property bfalse : TJSElement Read FBFalse Write FBFalse;
  end;

  { TJSWhileStatement }

  { TJSTargetStatement }

  TJSTargetStatement = Class(TJSElement)
  private
    FTarget: Cardinal;
  Public
    Property Target : Cardinal Read FTarget Write FTarget;
  end;

  { TJSBodyStatement }

  TJSBodyStatement = Class(TJSTargetStatement)
  private
    FBody: TJSElement;
  Public
    Destructor Destroy; override;
    Property body : TJSElement Read FBody Write FBody;
  end;

  { TJSCondLoopStatement }

  TJSCondLoopStatement = Class(TJSBodyStatement)
  private
    FCond: TJSElement;
  Public
    Destructor Destroy; override;
    Property Cond : TJSElement Read FCond Write FCond;
  end;

  TJSWhileStatement = Class(TJSCondLoopStatement)
  end;

  { TJSForStatement }

  TJSForStatement = Class(TJSCondLoopStatement)
  private
    FIncr: TJSElement;
    FInit: TJSElement;
  Public
    Destructor Destroy; override;
    Property Incr : TJSElement Read FIncr Write FIncr;
    Property Init : TJSElement Read FInit Write FInit;
  end;

  { TJSForInStatement }

  TJSForInStatement = Class(TJSBodyStatement)
  private
    FLhs: TJSElement;
    FList: TJSElement;
  Public
    Destructor Destroy; override;
    Property LHS : TJSElement Read FLHS Write FLHS;
    Property List : TJSElement Read FList Write FList;
  end;

  TJSContinueStatement = Class(TJSTargetStatement);

  TJSBreakStatement = Class(TJSTargetStatement);

  { TJSReturn }

  TJSReturnStatement = Class(TJSElement)
  private
    FExpr: TJSElement;
  Public
    Destructor Destroy; override;
    Property Expr : TJSElement Read FExpr Write FExpr;
  end;

  { TJSCaseElement }

  TJSCaseElement = Class(TCollectionItem)
  private
    FBody: TJSElement;
    FExpr: TJSelement;
  Public
    Destructor Destroy; override;
    Property Expr : TJSelement Read FExpr Write FExpr;
    Property Body : TJSElement Read FBody Write FBody;
  end;

  { TJSCaseElements }

  TJSCaseElements = Class(TCollection)
  private
    function GetC(AIndex : Integer): TJSCaseElement;
    procedure SetC(AIndex : Integer; const AValue: TJSCaseElement);
  Public
    Function AddCase : TJSCaseElement;
    Property Cases[AIndex : Integer] : TJSCaseElement Read GetC Write SetC;
  end;

  { TJSSwitch }

  TJSSwitchStatement = Class(TJSTargetStatement)
  private
    FCases: TJSCaseElements;
    FCond: TJSelement;
    FDefault: TJSCaseElement;
  Public
    Constructor Create(ALine,ARow : Integer; const ASource : String = ''); override;
    Destructor Destroy; override;
    Property Cond : TJSelement Read FCond Write FCond;
    Property Cases : TJSCaseElements Read FCases;
    Property TheDefault : TJSCaseelement Read FDefault Write FDefault;
  end;

  { TJSLabeledStatement }

  TJSLabeledStatement = Class(TJSUnary)
  private
    FTarget: Integer;
  Public
    Property target: Integer Read FTarget Write FTarget;
  end;

  { TJSTryStatement }

  TJSTryStatement = Class(TJSElement)
  private
    FBCatch: TJSElement;
    FBFinally: TJSElement;
    FBlock: TJSElement;
    FIdent: TJSString;
  Public
    Destructor Destroy; override;
    Property Block : TJSElement Read FBlock Write FBlock;
    Property BCatch : TJSElement Read FBCatch Write FBCatch;
    Property BFinally : TJSElement Read FBFinally Write FBFinally;
    Property Ident : TJSString Read FIdent Write FIDent;
  end;

  TJSTryCatchFinallyStatement = Class(TJSTryStatement);
  TJSTryCatchStatement = Class(TJSTryStatement);
  TJSTryFinallyStatement = Class(TJSTryStatement);


  { TJSFunction }

  TJSFunctionDeclarationStatement = Class(TJSelement)
  private
    FFuncDef: TJSFuncDef;
  Public
    Destructor Destroy; override;
    Property AFunction : TJSFuncDef Read FFuncDef Write FFuncDef;
  end;

  { TJSFunctionBody }

  TJSFunctionBody = Class(TJSUnary)
  private
    FisProgram: Boolean;
  Public
    Property isProgram : Boolean Read FisProgram Write FIsProgram;
  end;

  { TJSElementNode }

  TJSElementNode = Class(TCollectionItem)
  private
    FNode: TJSElement;
  Public
    Destructor Destroy; override;
    Property Node : TJSElement Read FNode Write FNode;
  end;

  { TJSElementNodes }

  TJSElementNodes = Class(TCollection)
  private
    function GetN(AIndex : Integer): TJSElementNode;
    procedure SetN(AIndex : Integer; const AValue: TJSElementNode);
  Public
    Function AddNode : TJSElementNode;
    Property Nodes[AIndex : Integer] : TJSElementNode Read GetN Write SetN; default;
  end;

  { TJSSourceElements }
  TJSSourceElements = Class(TJSElement)
  private
    FFunctions: TJSElementNodes;
    FStatements: TJSElementNodes;
    FVars: TJSElementNodes;
  Public
    Constructor Create(ALine,ARow : Integer; const ASource : String = ''); override;
    Destructor Destroy; override;
    Property Statements : TJSElementNodes Read FStatements;
    Property functions : TJSElementNodes Read FFunctions;
    Property Vars : TJSElementNodes Read FVars;
  end;


implementation

{ TJSElement }

constructor TJSElement.Create(ALine, ARow: Integer; Const ASource: String = '');
begin
  FLine:=ALine;
  FRow:=ARow;
  FSource:=ASource;
end;

{ TJSRegularExpressionLiteral }

function TJSRegularExpressionLiteral.GetA(AIndex : integer): TJSValue;
begin
  Result:=FArgv[AIndex];
end;

procedure TJSRegularExpressionLiteral.SetA(AIndex : integer;
  const AValue: TJSValue);
begin
  FArgv[AIndex]:=Avalue;
end;

constructor TJSRegularExpressionLiteral.Create(ALine, ARow: Integer;
  const ASource: String);
begin
  inherited Create(ALine, ARow, ASource);
  FPattern:=TJSValue.Create;
  FPatternFlags:=TJSValue.Create;
end;

destructor TJSRegularExpressionLiteral.Destroy;
begin
  FreeAndNil(FPattern);
  FreeAndNil(FPatternFlags);
  Inherited Destroy;
end;

{ TJSArrayLiteralElements }

function TJSArrayLiteralElements.GetE(AIndex : Integer): TJSArrayLiteralElement;
begin
  Result:=TJSArrayLiteralElement(Items[AIndex]);
end;

procedure TJSArrayLiteralElements.SetE(AIndex : Integer;
  const AValue: TJSArrayLiteralElement);
begin
  Items[AIndex]:=AValue;
end;

function TJSArrayLiteralElements.AddElement: TJSArrayLiteralElement;
begin
  Result:=TJSArrayLiteralElement(Add);
end;

{ TJSArrayLiteral }

constructor TJSArrayLiteral.Create(ALine, ARow: Integer; Const ASource: String = '');
begin
  inherited Create(ALine, ARow, ASource);
  FElements:=TJSArrayLiteralElements.Create(TJSArrayLiteralElement);
end;

destructor TJSArrayLiteral.Destroy;
begin
  FreeAndNil(FElements);
  inherited Destroy;
end;

{ TJSObjectLiteralElements }

function TJSObjectLiteralElements.GetE(AIndex : Integer
  ): TJSObjectLiteralElement;
begin
  Result:=TJSObjectLiteralElement(Items[AIndex]);
end;

procedure TJSObjectLiteralElements.SetE(AIndex : Integer;
  const AValue: TJSObjectLiteralElement);
begin
  Items[AIndex]:=AValue;
end;

function TJSObjectLiteralElements.AddElement: TJSObjectLiteralElement;
begin
  Result:=TJSObjectLiteralElement(Add);
end;

{ TJSObjectLiteral }

constructor TJSObjectLiteral.Create(ALine, ARow: Integer; const ASource: String = '');
begin
  inherited Create(ALine, ARow, ASource);
  FElements:=TJSObjectLiteralElements.Create(TJSObjectLiteralElement);
end;

destructor TJSObjectLiteral.Destroy;
begin
  FreeAndNil(FElements);
  inherited Destroy;
end;

{ TJSObjectLiteralElement }

destructor TJSObjectLiteralElement.Destroy;
begin
  FreeAndNil(Fexpr);
  inherited Destroy;
end;

{ TJSArrayLiteralElement }

destructor TJSArrayLiteralElement.Destroy;
begin
  FreeAndNil(Fexpr);
  inherited Destroy;
end;

{ TJSNewMemberExpression }

destructor TJSNewMemberExpression.Destroy;
begin
  FreeAndNil(FArgs);
  inherited Destroy;
end;

{ TJSMemberExpression }

destructor TJSMemberExpression.Destroy;
begin
  FreeAndNil(FMExpr);
  inherited Destroy;
end;

{ TJSCallExpression }

destructor TJSCallExpression.Destroy;
begin
  FreeAndNil(FExpr);
  FreeAndNil(FArgs);
  inherited Destroy;
end;

{ TJSUnary }

destructor TJSUnary.Destroy;
begin
  FreeAndNil(FA);
  inherited Destroy;
end;

{ TJSBinary }

destructor TJSBinary.Destroy;
begin
  FreeAndNil(FB);
  FreeAndNil(FA);
  inherited Destroy;
end;

{ TJSConditionalExpression }

destructor TJSConditionalExpression.Destroy;
begin
  FreeAndNil(FB);
  FreeAndNil(FA);
  FreeAndNil(FC);
  inherited Destroy;
end;

{ TJSAssign }

destructor TJSAssignStatement.Destroy;
begin
  FreeAndNil(FLHS);
  FreeAndNil(FExpr);
  inherited Destroy;
end;

{ TJSVarDeclaration }


destructor TJSVarDeclaration.Destroy;
begin
  FreeAndNil(FInit);
  inherited Destroy;
end;

{ TJSIfStatement }

destructor TJSIfStatement.Destroy;
begin
  FreeAndNil(FCond);
  FreeAndNil(FBTrue);
  FreeAndNil(FBFalse);
  inherited Destroy;
end;

{ TJSBodyStatement }

destructor TJSBodyStatement.Destroy;
begin
  FreeAndNil(FBody);
  inherited Destroy;
end;

{ TJSCondLoopStatement }

destructor TJSCondLoopStatement.Destroy;
begin
  FreeAndNil(FCond);
  inherited Destroy;
end;

{ TJSForStatement }

destructor TJSForStatement.Destroy;
begin
  FreeAndNil(FIncr);
  FreeAndNil(FInit);
  inherited Destroy;
end;

{ TJSForInStatement }

destructor TJSForInStatement.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FLHS);
  inherited Destroy;
end;


{ TJSReturn }

destructor TJSReturnStatement.Destroy;
begin
  FreeAndNil(FExpr);
  inherited Destroy;
end;

{ TJSCaseElement }

destructor TJSCaseElement.Destroy;
begin
  FreeAndNil(FExpr);
  FreeAndNil(FBody);
  inherited Destroy;
end;

{ TJSSwitch }

constructor TJSSwitchStatement.Create(ALine, ARow: Integer; const ASource: String);
begin
  inherited Create(ALine, ARow, ASource);
  FCases:=TJSCaseElements.Create(TJSCaseElement);
end;

destructor TJSSwitchStatement.Destroy;
begin
  FreeAndNil(FCases);
  FreeAndNil(FDefault);
  FreeAndNil(FCond);
  inherited Destroy;
end;

{ TJSCaseElements }

function TJSCaseElements.GetC(AIndex : Integer): TJSCaseElement;
begin
  Result:=TJSCaseElement(Items[AIndex]);
end;

procedure TJSCaseElements.SetC(AIndex : Integer; const AValue: TJSCaseElement);
begin
  Items[AIndex]:=AValue;
end;

function TJSCaseElements.AddCase: TJSCaseElement;
begin
  Result:=TJSCaseElement(Add);
end;

{ TJSTryStatement }

destructor TJSTryStatement.Destroy;
begin
  FreeAndNil(FBlock);
  FreeAndNil(FBCatch);
  FreeAndNil(FBFinally);
  inherited Destroy;
end;

{ TJSSourceElements }

constructor TJSSourceElements.Create(ALine, ARow: Integer; const ASource: String
  );
begin
  inherited Create(ALine, ARow, ASource);
  FStatements:=TJSElementNodes.Create(TJSElementNode);
  FFunctions:=TJSElementNodes.Create(TJSElementNode);
  FVars:=TJSElementNodes.Create(TJSElementNode);
end;

destructor TJSSourceElements.Destroy;

Var
  i : integer;

begin
  FreeAndNil(FStatements);
  FreeAndNil(FFunctions);
  // Vars are owned by their statements, and those have been freed
  For I:=0 to FVars.Count-1 do
    FVars.Nodes[i].Node:=nil;
  FreeAndNil(FVars);
  inherited Destroy;
end;

{ TJSElementNodes }

function TJSElementNodes.GetN(AIndex : Integer): TJSElementNode;
begin
  Result:=TJSElementNode(Items[Aindex])
end;

procedure TJSElementNodes.SetN(AIndex : Integer; const AValue: TJSElementNode);
begin
  Items[AIndex]:=Avalue;
end;

function TJSElementNodes.AddNode: TJSElementNode;
begin
  Result:=TJSElementNode(Add);
end;

{ TJSFunction }

destructor TJSFunctionDeclarationStatement.Destroy;
begin
  FreeAndNil(FFuncDef);
  inherited Destroy;
end;

{ TJSElementNode }

destructor TJSElementNode.Destroy;
begin
  FreeAndNil(FNode);
  inherited Destroy;
end;

{ TJSFuncDef }

procedure TJSFuncDef.SetParams(const AValue: TStrings);
begin
  if FParams=AValue then exit;
  FParams.Assign(AValue);
end;

constructor TJSFuncDef.Create;
begin
  FParams:=TStringList.Create;
end;

destructor TJSFuncDef.Destroy;
begin
  FreeAndNil(FBody);
  FreeAndNil(FParams);
  inherited Destroy;
end;

{ TJSBracketMemberExpression }

destructor TJSBracketMemberExpression.Destroy;
begin
  FreeAndNil(FName);
  inherited Destroy;
end;

{ TJSLiteral }

constructor TJSLiteral.Create(ALine, ARow: Integer; const ASource: String);
begin
  FValue:=TJSValue.Create;
  inherited Create(ALine, ARow, ASource);
end;

destructor TJSLiteral.Destroy;
begin
  FreeAndNil(FValue);
  Inherited;
end;

end.

