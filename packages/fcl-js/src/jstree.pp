{ ********************************************************************* 
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2016 Michael Van Canneyt.
       
    Javascript syntax tree definitions
            
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
                   
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
                                
  **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit jstree;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}
{ $DEFINE NOCLASSES}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
{$IFNDEF NOCLASSES}  System.Classes, {$ENDIF} System.SysUtils, Js.Base, Js.Token;
{$ELSE FPC_DOTTEDUNITS}
uses
{$IFNDEF NOCLASSES}  Classes, {$ENDIF} SysUtils, jsbase, jstoken;
{$ENDIF FPC_DOTTEDUNITS}

Type
{$IFDEF NOCLASSES}

  { TStrings }
{$M+}
  TStrings = Class(TObject)
  private
    FCount: Integer;
    FStrings : Array of String;
    function GetCount: Integer;
    function GetS(AIndex : Integer): String;
    procedure SetS(AIndex : Integer; const AValue: String);
  Public
    Function Add(Const S : String) : Integer;
    Procedure Assign(Source : TStrings);
    Property Strings[AIndex : Integer] : String Read GetS Write SetS; default;
    Property Count : Integer Read GetCount;
  end;
  TStringList = TStrings;
  EListError = Class(Exception);

  TCollection = Class;

  { TCollectionItem }

  TCollectionItem = Class
  Private
    FCollection : TCollection;
  Public
    Constructor Create(ACollection : TCollection);
    Destructor Destroy; override;
  end;
  TCollectionItemClass = Class of TCollectionItem;

  { TCollection }

  TCollection = Class
  private
    FCount: Integer;
    FItems : Array of TCollectionItem;
    FItemClass : TCollectionItemClass;
    function GetCount: Integer;
    function GetI(AIndex : Integer): TCollectionItem;
  public
    Constructor Create(AItemClass : TCollectionItemClass);
    Destructor Destroy; override;
    Procedure Clear;
    Procedure Remove(AItem : TCollectionItem);
    Function Add : TCollectionItem;
    Property Items[AIndex : Integer] : TCollectionItem Read GetI;default;
    Property Count : Integer Read GetCount;
  end;

{$M-}
{$ENDIF}
  TJSElementFlag = (elIsConst,elIsConstValid);
  TJSElementFlags = set of TJSElementFlag;

  TJSFunctionBody = Class;
  TJSElementNodes = class;
  TJSObjectTypeDef = Class;

  { TJSElement }

  { TJSObject }

  TJSObject = Class(TObject);


  { TJSLabelSet }

  TJSLabelSet = Class(TJSObject)
  private
    FCont: Boolean;
    FNext: TJSLabelSet;
    FTarget: Cardinal;
  Public
    Property Target : Cardinal Read FTarget Write FTarget;
    Property Next : TJSLabelSet Read FNext Write FNext; // Linked list
    Property Continuable : Boolean Read FCont Write FCont;
  end;

  { TJSLabel }
  TJSString = {$IFDEF FPC_DOTTEDUNITS}Js.Base.{$ELSE}jsBase.{$ENDIF}TJSString; // beware of jstoken.tjsString
  TJSTreeString = {$IFDEF FPC_DOTTEDUNITS}Js.Base.{$ELSE}jsBase.{$ENDIF}TJSString; // beware of jstoken.tjsString

  TJSLabel = Class(TJSObject)
  private
    FLabelSet: TJSLabelSet;
    FLocationLine: Integer;
    FLocationPos: Integer;
    FLocationSource: String;
    FName: TJSTreeString;
    FNext: TJSLabel;
  Public
    Property Name : TJSTreeString Read FName Write FName;
    Property LabelSet : TJSLabelSet Read FLabelSet Write FLabelSet;
    Property LocationSource : String Read FLocationSource Write FLocationSource;
    Property LocationLine : Integer Read FLocationLine Write FLocationLine;
    Property LocationPos : Integer Read FLocationPos Write FLocationPos;
    Property Next : TJSLabel Read FNext Write FNext;
  end;


  { TJSFuncDef - part of TJSFunctionDeclarationStatement, e.g. 'function Name(Params)Body' }
  TJSTypedParams = Class;
  TJSTypeDef = Class;
  TJSEnumTypeDef = Class;

  TJSFuncDef = Class(TJSObject)
  private
    FBody: TJSFunctionBody;
    FIsAsserts: Boolean;
    FIsAsync: Boolean;
    FIsConstructor: Boolean;
    FIsEmpty: Boolean;
    FName: TJSString;
    FParams: TStrings;
    FResultType: TJSTypeDef;
    FTypedParams: TJSTypedParams;
    FGenericParams : TJSElementNodes;
  Public
    Constructor Create;
    Destructor Destroy; override;
    Procedure UpdateParams;
    Property TypedParams : TJSTypedParams Read FTypedParams;
    Property ResultType : TJSTypeDef Read FResultType Write FResultType;
    Property Params : TStrings Read FParams; deprecated 'use TypedParams instead';
    Property Body : TJSFunctionBody Read FBody Write FBody; // can be nil
    Property Name : TJSString Read FName Write FName;
    Property IsEmpty : Boolean Read FIsEmpty Write FIsEmpty;
    Property IsAsync : Boolean Read FIsAsync Write FIsAsync;
    Property IsAsserts : Boolean Read FIsAsserts Write FIsAsserts;
    Property IsConstructor : Boolean Read FIsConstructor Write FIsConstructor;
    Property GenericParams : TJSElementNodes Read FGenericParams Write FGenericParams;
  end;

  { TJSElement }
  
  TJSElement = Class(TJSObject)
  private
    FData: TObject;
    FFlags: TJSElementFlags;
    FLine: Integer;
    FColumn: Integer;
    FSource: String;
  Public
    Type
      TFreeNotifyEvent = Procedure(aEl : TJSElement) of object;
    class var
      GlobalFreeHook : TFreeNotifyEvent;
  Public
    Constructor Create(ALine,AColumn : Integer; Const ASource : String = ''); virtual;
    Destructor Destroy; override;
    Procedure AssignPosition(El: TJSElement); virtual;
    Property Source : String Read FSource Write FSource;
    Property Line : Integer Read FLine Write FLine;
    Property Column : Integer Read FColumn Write FColumn;
    Property Flags : TJSElementFlags Read FFlags Write FFlags;
    Property Data : TObject Read FData write FData;
  end;
  TJSElementClass = Class of TJSElement;
  TJSElementArray = array of TJSElement;

  { TJSEmptyBlockStatement - empty curly brackets }

  TJSStatement = Class(TJSElement);

  TJSEmptyBlockStatement = Class(TJSStatement);

  { TJSEmptyStatement - a dummy placeholder, needs sometimes a single semicolon }

  TJSEmptyStatement = Class(TJSStatement);

  { TJSLiteral }

  TJSLiteral = Class(TJSElement)
  private
    FValue: TJSValue;
  Public
    Constructor Create(ALine,AColumn : Integer; Const ASource : String = ''); override;
    Destructor Destroy; override;
    Property Value : TJSValue Read FValue Write FValue;
  end;

  { TJSRegularExpressionLiteral - /Pattern/PatternFlags }

  TJSRegularExpressionLiteral = Class(TJSElement)
  private
    FPattern: TJSValue;
    FPatternFlags: TJSValue;
    FArgv : Array[0..1] of TJSValue;
    function GetA(AIndex : integer): TJSValue;
    procedure SetA(AIndex : integer; const AValue: TJSValue);
  Public
    Constructor Create(ALine,AColumn : Integer; Const ASource : String = ''); override;
    Destructor Destroy; override;
    Property Pattern : TJSValue Read FPattern Write FPattern;
    Property PatternFlags : TJSValue Read FPatternFlags Write FPatternFlags;
    Property Argv[AIndex : integer] : TJSValue Read GetA Write SetA;
  end;

  TJSPrimaryExpression = Class(TJSElement);

  TJSPrimaryExpressionThis = Class(TJSPrimaryExpression); // 'this'

  { TJSPrimaryExpressionIdent }

  TJSPrimaryExpressionIdent = Class(TJSPrimaryExpression)
  private
    FName: TJSString;
  Public
    Property Name : TJSString Read FName Write FName;
  end;

  { TJSArrayLiteralElement - an item of a TJSArrayLiteralElements }

  TJSArrayLiteralElement = Class(TCollectionItem)
  private
    FExpr: TJSelement;
    FElementIndex: Integer;
  Public
    Destructor Destroy; override;
    Property Expr : TJSElement Read FExpr Write FExpr;
    Property ElementIndex : Integer Read FElementIndex Write FElementIndex;
  end;

  { TJSArrayLiteralElements - Elements property of TJSArrayLiteral }

  TJSArrayLiteralElements = Class(TCollection)
  private
    function GetE(AIndex : Integer): TJSArrayLiteralElement;
  Public
    Function AddElement : TJSArrayLiteralElement;
    Property Elements[AIndex : Integer] : TJSArrayLiteralElement Read GetE ; default;
  end;

  { TJSArrayLiteral - [element1,...] or Args of a function }

  TJSArrayLiteral = Class(TJSElement)
  private
    FElements: TJSArrayLiteralElements;
    function GetCount: Integer;
  Public
    Constructor Create(ALine,AColumn : Integer; const ASource : String = ''); override;
    procedure AddElement(El: TJSElement);
    Destructor Destroy; override;
    Property Elements : TJSArrayLiteralElements Read FElements;
    Property Count : Integer Read GetCount;
  end;

  { TJSObjectLiteralElement - an item of TJSObjectLiteralElements }

  TJSObjectLiteralElement = Class(TCollectionItem)
  private
    FExpr: TJSelement;
    FName: TJSString;
  Public
    Destructor Destroy; override;
    Property Expr : TJSElement Read FExpr Write FExpr;
    Property Name : TJSString Read FName Write FName;
  end;

  { TJSObjectLiteralElements - Elements property of TJSObjectLiteral }

  TJSObjectLiteralElements = Class(TCollection)
  private
    function GetE(AIndex : Integer): TJSObjectLiteralElement;
  Public
    Function AddElement : TJSObjectLiteralElement;
    Property Elements[AIndex : Integer] : TJSObjectLiteralElement Read GetE ; default;
  end;

  { TJSObjectLiteral }

  TJSObjectLiteral = Class(TJSElement)
  private
    FElements: TJSObjectLiteralElements;
  Public
    Constructor Create(ALine,AColumn : Integer; const ASource : String = ''); override;
    Destructor Destroy; override;
    Property Elements : TJSObjectLiteralElements Read FElements;
  end;

  { TJSArguments - (element1,...) }

  TJSArguments = Class(TJSArrayLiteral);

  { TJSMemberExpression - base class }

  TJSMemberExpression = Class(TJSElement)
  private
    FMExpr: TJSElement;
  Public
    Destructor Destroy; override;
    Property MExpr : TJSElement Read FMExpr Write FMExpr;
  end;

  { TJSNewMemberExpression - e.g. 'new MExpr(Args)' }

  TJSNewMemberExpression = Class(TJSMemberExpression)
  private
    FArgs: TJSArguments;
  Public
    Destructor Destroy; override;
    procedure AddArg(El: TJSElement);
    Property Args : TJSArguments Read FArgs Write FArgs;
  end;

  { TJSDotMemberExpression - e.g. 'MExpr.Name' }

  TJSDotMemberExpression = Class(TJSMemberExpression)
  private
    FName: TJSString;
  Public
    Property Name : TJSString Read FName Write FName;
  end;

  { TJSBracketMemberExpression - e.g. 'MExpr[Name]' }

  TJSBracketMemberExpression = Class(TJSMemberExpression)
  private
    FName: TJSElement;
  Public
    Destructor Destroy; override;
    Property Name : TJSElement Read FName Write FName;
  end;

  { TJSCallExpression - e.g. 'Expr(Args)'}

  TJSCallExpression = Class(TJSElement)
  private
    FArgs: TJSArguments;
    FExpr: TJSElement;
  Public
    Destructor Destroy; override;
    procedure AddArg(El: TJSElement);
    procedure InsertArg(Index: integer; El: TJSElement);
    Property Expr : TJSElement Read FExpr Write FExpr;
    Property Args : TJSArguments Read FArgs Write FArgs;
  end;

  { TJSUnary - e.g. 'PrefixOperator A PostFixOperator', '--i' }

  TJSUnary = Class(TJSElement)
  private
    FA: TJSElement;
  Public
    Class function PrefixOperatorToken : tjsToken; virtual;
    Class function PostFixOperatorToken : tjsToken; virtual;
    Class function PrefixOperator : String;
    Class function PostFixOperator : String;
    Destructor Destroy; override;
    Property A : TJSElement Read FA Write FA;
  end;
  TJSUnaryClass = class of TJSUnary;

  TJSDeclarationStatement = Class(TJSElement)
  Public
    Function GetDeclaration : TJSElement; virtual; abstract;
  end;


  { TJSVariableStatement - e.g. 'var A' 'let A', 'const a'}

  TJSVarType = (vtVar,vtLet,vtConst);
  TJSVarTypes = Set of TJSVarType;
  TJSVariableStatement = Class(TJSDeclarationStatement)
  private
    FA: TJSElement;
    FVarType: TJSVarType;
  Public
    Property VarType : TJSVarType Read FVarType Write FVarType;
    Destructor Destroy; override;
    Function GetDeclaration : TJSElement; override;
    // For backwards Compatibility
    Property A : TJSElement Read FA Write FA; deprecated 'Use VarDecl instead';
    Property VarDecl : TJSElement Read FA Write FA;
  end;


  { TJSExpressionStatement - A; }

  TJSExpressionStatement = Class(TJSUnary);

  { TJSThrowStatement - e.g. 'throw A' }

  TJSThrowStatement = Class(TJSUnary)
  Public
    Class function PrefixOperatorToken : tjsToken; Override;
  end;

  TJSUnaryExpression = Class(TJSUnary);

  { TJSUnaryDeleteExpression - e.g. 'delete A' }

  TJSUnaryDeleteExpression = Class(TJSUnaryExpression)
  Public
    Class function PrefixOperatorToken : tjsToken; override;
  end;

  { TJSUnaryVoidExpression - e.g. 'void A' }

  TJSUnaryVoidExpression = Class(TJSUnaryExpression)
  Public
    Class function PrefixOperatorToken : tjsToken; override;
  end;

  { TJSUnaryTypeOfExpression - e.g. 'typeof A' }

  TJSUnaryTypeOfExpression = Class(TJSUnaryExpression)
  Public
    Class function PrefixOperatorToken : tjsToken; override;
  end;

  { TJSAwaitExpression - e.g. 'await A' }

  TJSAwaitExpression = Class(TJSUnaryExpression)
  Public
    Class function PrefixOperatorToken : tjsToken; Override;
  end;

  { TJSYieldExpression }

  TJSYieldExpression = Class(TJSUnaryExpression)
  Public
    Class function PrefixOperatorToken : tjsToken; Override;
  end;


  { TJSUnaryPrePlusPlusExpression - e.g. '++A' }

  TJSUnaryPrePlusPlusExpression = Class(TJSUnaryExpression)
  Public
    Class function PrefixOperatorToken : tjsToken; override;
  end;

  { TJSUnaryPreMinusMinusExpression - e.g. '--A' }

  TJSUnaryPreMinusMinusExpression = Class(TJSUnaryExpression)
  Public
    Class function PrefixOperatorToken : tjsToken; override;
  end;

  { TJSUnaryPlusExpression - e.g. '+A' }

  TJSUnaryPlusExpression = Class(TJSUnaryExpression)
  Public
    Class function PrefixOperatorToken : tjsToken; override;
  end;

  { TJSUnaryMinusExpression - e.g. '-A' }

  TJSUnaryMinusExpression = Class(TJSUnaryExpression)
  Public
    Class function PrefixOperatorToken : tjsToken; override;
  end;

  { TJSUnaryInvExpression - e.g. '~A' }

  TJSUnaryInvExpression = Class(TJSUnaryExpression)
  Public
    Class function PrefixOperatorToken : tjsToken; override;
  end;

  { TJSUnaryNotExpression - e.g. '!A' }

  TJSUnaryNotExpression = Class(TJSUnaryExpression)
  Public
    Class function PrefixOperatorToken : tjsToken; override;
  end;

  { TJSUnaryPostPlusPlusExpression - e.g. 'A++' }

  TJSUnaryPostPlusPlusExpression = Class(TJSUnaryExpression)
  Public
    Class function PostFixOperatorToken : tjsToken; override;
  end;

  { TJSUnaryPostMinusMinusExpression - e.g. 'A--' }

  TJSUnaryPostMinusMinusExpression = Class(TJSUnaryExpression)
  Public
    Class function PostFixOperatorToken : tjsToken; override;
  end;

  { TJSUnaryBracketsExpression - e.g. '(A)' }

  TJSUnaryBracketsExpression = Class(TJSUnaryExpression)
  Public
    Class function PrefixOperatorToken : tjsToken; override;
    Class function PostFixOperatorToken : tjsToken; override;
  end;

  { TJSBinary - base class }

  TJSBinary = Class(TJSElement)
  private
    FA: TJSElement;
    FB: TJSElement;
  Public
    Destructor Destroy; override;
    Property A : TJSElement Read FA Write FA;
    Property B : TJSElement Read FB Write FB;
  end;
  TJSBinaryClass = Class of TJSBinary;

  { TJSStatementList - a list of statements enclosed in curly brackets }

  TJSStatementList = Class(TJSBinary); // A->first statement, B->next in list, chained.

  { TJSVariableDeclarationList }

  TJSVariableDeclarationList = Class(TJSBinary); // A->first variable, B->next in list, chained.

  { TJSWithStatement - with(A)do B; }

  TJSWithStatement = Class(TJSBinary); // A-> with expression, B->statement(s)

  { TJSBinaryExpression - e.g. A operator B }

  TJSBinaryExpression = Class(TJSBinary)
  Public
    Class function OperatorToken : tjsToken; virtual;
    Class function OperatorString : string;
    Class Function AllowCompact : Boolean; virtual;
  end;

  { TJSLogicalOrExpression - e.g. A || B }

  TJSLogicalOrExpression = Class (TJSBinaryExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSLogicalAndExpression - e.g. A && B }

  TJSLogicalAndExpression = Class (TJSBinaryExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSBitwiseAndExpression - e.g. A & B }

  TJSBitwiseAndExpression = Class (TJSBinaryExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSBitwiseOrExpression - e.g. A | B }

  TJSBitwiseOrExpression = Class (TJSBinaryExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSBitwiseXOrExpression - e.g. A ^ B }

  TJSBitwiseXOrExpression = Class (TJSBinaryExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  TJSEqualityExpression = Class (TJSBinaryExpression);

  { TJSEqualityExpressionEQ - e.g. A == B }

  TJSEqualityExpressionEQ = Class(TJSEqualityExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSEqualityExpressionNE - e.g. A != B }

  TJSEqualityExpressionNE = Class(TJSEqualityExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSEqualityExpressionSEQ strict equal - e.g. A === B }

  TJSEqualityExpressionSEQ = Class(TJSEqualityExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSEqualityExpressionSNE not strict equal - e.g. A !== B }

  TJSEqualityExpressionSNE = Class(TJSEqualityExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  TJSRelationalExpression = Class(TJSBinaryExpression);

  { TJSRelationalExpressionLT lower than - e.g. A < B }

  TJSRelationalExpressionLT = Class(TJSRelationalExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSRelationalExpressionGT greater than - e.g. A > B }

  TJSRelationalExpressionGT = Class(TJSRelationalExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSRelationalExpressionLE lower equal - e.g. A <= B }

  TJSRelationalExpressionLE = Class(TJSRelationalExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSRelationalExpressionGE greater equal - e.g. A >= B }

  TJSRelationalExpressionGE = Class(TJSRelationalExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSRelationalExpressionIn - e.g. A in B }

  TJSRelationalExpressionIn = Class(TJSRelationalExpression)
    Class function OperatorToken : tjsToken; override;
    Class Function AllowCompact : Boolean; override;
  end;

  { TJSRelationalExpressionInstanceOf - e.g. A instanceof B }

  TJSRelationalExpressionInstanceOf = Class(TJSRelationalExpression)
    Class function OperatorToken : tjsToken; override;
    Class Function AllowCompact : Boolean; override;
  end;

  TJSShiftExpression = Class(TJSBinaryExpression);

  { TJSLShiftExpression - e.g. A << B }

  TJSLShiftExpression = Class(TJSShiftExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSRShiftExpression right shift keep sign - e.g. A >> B }

  TJSRShiftExpression = Class(TJSShiftExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSURShiftExpression right shift unsigned, insert zeroes - e.g. A >>> B }

  TJSURShiftExpression = Class(TJSShiftExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  TJSAdditiveExpression = Class(TJSBinaryExpression);

  { TJSAdditiveExpressionPlus - e.g. A + B }

  TJSAdditiveExpressionPlus = Class(TJSAdditiveExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSAdditiveExpressionMinus - e.g. A - B }

  TJSAdditiveExpressionMinus = Class(TJSAdditiveExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  TJSMultiplicativeExpression = Class(TJSBinaryExpression);

  { TJSMultiplicativeExpressionMul - e.g. A * B }

  TJSMultiplicativeExpressionMul = Class(TJSMultiplicativeExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSMultiplicativeExpressionDiv - e.g. A / B }

  TJSMultiplicativeExpressionDiv = Class(TJSMultiplicativeExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSMultiplicativeExpressionMod - e.g. A % B }

  TJSMultiplicativeExpressionMod = Class(TJSMultiplicativeExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSPowerExpression - e.g. A ** B }

  TJSPowerExpression = Class(TJSBinaryExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSCommaExpression - e.g. A , B }

  TJSCommaExpression = Class(TJSBinaryExpression)
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSConditionalExpression - e.g. A ? B :C }

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

  TJSDebuggerStatement = Class(TJSStatement);

  { TJSAssignStatement - e.g. LHS operator Expr }

  TJSAssignStatement = Class(TJSStatement)
  private
    FExpr: TJSElement;
    FLHS: TJSElement;
  Public
    Destructor Destroy; override;
    Class function OperatorToken : tjsToken; virtual;
    Class function OperatorString : String;
    Property Expr : TJSElement Read FExpr Write FExpr;
    Property LHS : TJSElement Read FLHS Write FLHS;
  end;

  TJSAssignStatementClass = Class of TJSAssignStatement;

  { TJSSimpleAssignStatement - e.g. LHS=Expr }

  TJSSimpleAssignStatement = Class(TJSAssignStatement)
  Public
    Class function OperatorToken : tjsToken; override;
  end;

  { LHS => Expr }

  { TJSArrowFunction }

  TJSArrowFunction = Class(TJSAssignStatement)
  Public
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSMulEqAssignStatement - e.g. LHS*=Expr }

  TJSMulEqAssignStatement = Class(TJSAssignStatement)
  Public
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSDivEqAssignStatement - e.g. LHS/=Expr }

  TJSDivEqAssignStatement = Class(TJSAssignStatement)
  Public
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSModEqAssignStatement - e.g. LHS%=Expr }

  TJSModEqAssignStatement = Class(TJSAssignStatement)
  Public
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSAddEqAssignStatement - e.g. LHS+=Expr }

  TJSAddEqAssignStatement = Class(TJSAssignStatement)
  Public
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSSubEqAssignStatement - e.g. LHS-=Expr }

  TJSSubEqAssignStatement = Class(TJSAssignStatement)
  Public
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSLShiftEqAssignStatement - e.g. LHS<<=Expr }

  TJSLShiftEqAssignStatement = Class(TJSAssignStatement)
  Public
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSRShiftEqAssignStatement - e.g. LHS>>=Expr keep sign }

  TJSRShiftEqAssignStatement = Class(TJSAssignStatement)
  Public
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSURShiftEqAssignStatement - e.g. LHS>>>=Expr unsigned, insert zeroes }

  TJSURShiftEqAssignStatement = Class(TJSAssignStatement)
  Public
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSANDEqAssignStatement - e.g. LHS&=Expr }

  TJSANDEqAssignStatement = Class(TJSAssignStatement)
  Public
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSOREqAssignStatement - e.g. LHS|=Expr }

  TJSOREqAssignStatement = Class(TJSAssignStatement)
  Public
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSXOREqAssignStatement - e.g. LHS^=Expr }

  TJSXOREqAssignStatement = Class(TJSAssignStatement)
  Public
    Class function OperatorToken : tjsToken; override;
  end;

  { TJSVarDeclaration - e.g. Name=Init }

  TJSVarDeclaration = Class(TJSElement)
  private
    FInit: TJSElement;
    FIsUnique: Boolean;
    FName: TJSString;
    FOwnsType: Boolean;
    FTyped: TJSTypeDef;
    FVarType: TJSVarType;
    procedure SetTyped(AValue: TJSTypeDef);
  Public
    Destructor Destroy; override;
    procedure SetForeignType(AValue: TJSTypeDef);
    Property Name : TJSString Read FName Write FName;
    Property Init : TJSElement Read FInit Write FInit;
    // let, var, const
    Property VarType : TJSVarType Read FVarType Write FVarType;
    // Typescript type. Setting to non-nil value will set OwnsType
    Property Typed : TJSTypeDef Read FTyped Write SetTyped;
    Property OwnsType : Boolean Read FOwnsType;
    Property IsUnique : Boolean Read FIsUnique Write FIsUnique;
  end;

  { TJSIfStatement - e.g. if (Cond) btrue else bfalse }

  TJSIfStatement = Class(TJSStatement)
  private
    FBFalse: TJSElement;
    FBTrue: TJSElement;
    FCond: TJSElement;
  Public
    Destructor Destroy; override;
    Property Cond : TJSElement Read FCond Write FCond;
    Property BTrue : TJSElement Read FBTrue Write FBTrue;
    Property BFalse : TJSElement Read FBFalse Write FBFalse;
  end;

  { TJSTargetStatement
    - base class for statements targetable by continue and break
    - TargetName can be empty }

  TJSTargetStatement = Class(TJSStatement)
  private
    FTarget: Cardinal;
    FTargetName: TJSString;
  Public
    Property Target : Cardinal Read FTarget Write FTarget;
    Property TargetName : TJSString Read FTargetName Write FTargetName;
  end;

  { TJSBodyStatement - base class }

  TJSBodyStatement = Class(TJSTargetStatement)
  private
    FBody: TJSElement;
  Public
    Destructor Destroy; override;
    Property Body : TJSElement Read FBody Write FBody;
  end;

  { TJSCondLoopStatement - base class for do..while and while..do }

  TJSCondLoopStatement = Class(TJSBodyStatement)
  private
    FCond: TJSElement;
  Public
    Destructor Destroy; override;
    Property Cond : TJSElement Read FCond Write FCond;
  end;

  { TJSWhileStatement - e.g. 'while(Cond) Body' }

  TJSWhileStatement = Class(TJSCondLoopStatement);

  { TJSDoWhileStatement - e.g. 'do Body while(Cond)' }

  TJSDoWhileStatement = Class(TJSWhileStatement);

  { TJSForStatement - e.g. 'for(Init;Cond;Incr) Body' }

  TJSForStatement = Class(TJSCondLoopStatement)
  private
    FIncr: TJSElement;
    FInit: TJSElement;
  Public
    Destructor Destroy; override;
    Property Incr : TJSElement Read FIncr Write FIncr;
    Property Init : TJSElement Read FInit Write FInit;
  end;

  { TJSForInStatement - e.g. 'for(LHS in List) Body' }

  TJSForInStatement = Class(TJSBodyStatement)
  private
    FLhs: TJSElement;
    FList: TJSElement;
  Public
    Destructor Destroy; override;
    Property LHS : TJSElement Read FLHS Write FLHS;
    Property List : TJSElement Read FList Write FList;
  end;

  TJSAliasElement = Class(TCollectionItem)
  private
    FName : TJSString;
    FAlias : TJSString;
  Public
    Property Name : TJSString Read FName Write FName;
    Property Alias : TJSString Read FAlias Write FAlias;
  end;

  { TJSAliasElements }

  TJSAliasElements = Class(TCollection)
  private
    function GetA(AIndex : Integer): TJSAliasElement;
  Public
    Function AddAlias : TJSAliasElement;
    Property Imports[AIndex : Integer] : TJSAliasElement Read GetA; default;
  end;

  TJSNamedImportElement = Class(TJSAliasElement);


  { TJSNamedImportElements - NamedImports property of TJSImportStatement }

  TJSNamedImportElements = Class(TJSAliasElements)
  private
    function GetE(AIndex : Integer): TJSNamedImportElement;
  Public
    Function AddElement : TJSNamedImportElement;
    Property Imports[AIndex : Integer] : TJSNamedImportElement Read GetE; default;
  end;

  { TJSImportStatement }

  TJSImportStatement = class(TJSStatement)
  Private
    FDefaultBinding: TJSString;
    FExpression: TJSElement;
    FModuleName: TJSString;
    FNamedImports : TJSNamedImportElements;
    FNameSpaceImport: TJSString;
    function GetHaveNamedImports: Boolean;
    function GetNamedImports: TJSNamedImportElements;
  Public
    Destructor Destroy; override;
    Property ModuleName : TJSString Read FModuleName Write FModuleName;
    Property DefaultBinding : TJSString Read FDefaultBinding Write FDefaultBinding;
    Property NameSpaceImport : TJSString Read FNameSpaceImport Write FNameSpaceImport;
    Property HaveNamedImports : Boolean Read GetHaveNamedImports;
    Property NamedImports : TJSNamedImportElements Read GetNamedImports;
    // TypeScript : import A = require('a');
    Property Expression : TJSElement Read FExpression Write FExpression;
  end;

  TJSExportNameElement = Class(TJSAliasElement);

  { TJSExportNameElements - NamedExports property of TJSExportStatement
    e.g. 'Name' or 'Name as Alias'}

  TJSExportNameElements = Class(TJSAliasElements)
  private
    function GetE(AIndex : Integer): TJSExportNameElement;
  Public
    Function AddElement : TJSExportNameElement;
    Property ExportedNames[AIndex : Integer] : TJSExportNameElement Read GetE ;default;
  end;

  { TJSExportStatement - e.g. 'export Declaration' }
  // export [default] Declaration
  // export [default] NameSpaceExport [from ModuleName]
  // export [default] * [from ModuleName]
  // export { ExportNames[1], ExportNames[2], ... } [from ModuleName]

  TJSExportStatement = class(TJSStatement)
  Private
    FDeclaration: TJSElement;
    FIsDefault: Boolean;
    FModuleName: TJSString;
    FNamedExports: TJSExportNameElements;
    FNameSpaceExport: TJSString;
    function GetHaveNamedExports: Boolean;
    function GetNamedExports: TJSExportNameElements;
  Public
    Destructor Destroy; override;
    Property IsDefault : Boolean Read FIsDefault Write FIsDefault; // write "default"
    Property Declaration : TJSElement Read FDeclaration Write FDeclaration;
    Property NameSpaceExport : TJSString Read FNameSpaceExport Write FNameSpaceExport;// can be '*'
    Property ModuleName : TJSString Read FModuleName Write FModuleName;
    Property HaveExportNames : Boolean Read GetHaveNamedExports;
    Property ExportNames : TJSExportNameElements Read GetNamedExports;
  end;

  { TJSContinueStatement - e.g. 'continue'}

  TJSContinueStatement = Class(TJSTargetStatement);

  { TJSBreakStatement - e.g. 'break' }

  TJSBreakStatement = Class(TJSTargetStatement);

  { TJSReturn - e.g. 'return Expr'}

  TJSReturnStatement = Class(TJSStatement)
  private
    FExpr: TJSElement;
  Public
    Destructor Destroy; override;
    Property Expr : TJSElement Read FExpr Write FExpr;
  end;

  { TJSCaseElement - element of TJSCaseElements, e.g. 'case Expr: Body' }

  TJSCaseElement = Class(TCollectionItem)
  private
    FBody: TJSElement;
    FExpr: TJSelement;
  Public
    Destructor Destroy; override;
    Property Expr : TJSelement Read FExpr Write FExpr;
    Property Body : TJSElement Read FBody Write FBody;
  end;

  { TJSCaseElements - Cases property of TJSSwitch }

  TJSCaseElements = Class(TCollection)
  private
    function GetC(AIndex : Integer): TJSCaseElement;
  Public
    Function AddCase : TJSCaseElement;
    Property Cases[AIndex : Integer] : TJSCaseElement Read GetC ;default;
  end;

  { TJSSwitch - e.g. switch(Cond) Cases }

  TJSSwitchStatement = Class(TJSTargetStatement)
  private
    FCases: TJSCaseElements;
    FCond: TJSelement;
    FDefault: TJSCaseElement;
  Public
    Constructor Create(ALine,AColumn : Integer; const ASource : String = ''); override;
    Destructor Destroy; override;
    Property Cond : TJSElement Read FCond Write FCond;
    Property Cases : TJSCaseElements Read FCases;
    Property TheDefault : TJSCaseElement Read FDefault Write FDefault; // one of Cases
  end;

  { TJSLabeledStatement - e.g. 'TheLabel : A' }

  TJSLabeledStatement = Class(TJSUnary)
  private
    FLabel: TJSLabel;
    FTarget: Integer;
  Public
    Destructor Destroy; override;
    Property Target: Integer Read FTarget Write FTarget;
    Property TheLabel : TJSLabel Read FLabel Write Flabel;
  end;

  { TJSTryStatement - e.g. 'try Block catch(Ident) BCatch finally BFinally' }

  TJSTryStatement = Class(TJSStatement)
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


  { TJSFunctionStatement - same as TJSFuncDef, except as a TJSElement }

  TJSFunctionStatement = Class(TJSStatement)
  private
    FFuncDef: TJSFuncDef;
    FIsGenerator: Boolean;
  Public
    Destructor Destroy; override;
    Property AFunction : TJSFuncDef Read FFuncDef Write FFuncDef;
    Property IsGenerator : Boolean Read FIsGenerator Write FIsGenerator;
  end;
  TJSFunctionDeclarationStatement = TJSFunctionStatement;

  { TJSFunctionBody - the statement block of a function }

  TJSFunctionBody = Class(TJSUnary)
  private
    FIsProgram: Boolean;
  Public
    Property isProgram : Boolean Read FIsProgram Write FIsProgram;
  end;


  { TJSElementNode - element of TJSElementNodes }

  TJSElementNode = Class(TCollectionItem)
  private
    FIsAmbient: Boolean;
    FIsExport: Boolean;
    FNode: TJSElement;
  Public
    Destructor Destroy; override;
    Procedure Assign(aSource : TPersistent); override;
    Property Node : TJSElement Read FNode Write FNode;
    Property IsAmbient : Boolean Read FIsAmbient Write FIsAmbient;
    Property IsExport : Boolean Read FIsExport Write FIsExport;
  end;

  { TJSTransientElementNode }

  // Will not free the node.
  TJSTransientElementNode = Class(TJSElementNode)
  Public
    Destructor Destroy; override;
  end;
  { TElementNodeEnumerator }

  TElementNodeEnumerator = Class(TCollectionEnumerator)
  public
    function GetCurrent: TJSElementNode; reintroduce;
    property Current: TJSElementNode read GetCurrent;
  end;
  { TJSElementNodes - see TJSSourceElements }

  TJSElementNodes = Class(TCollection)
  private
    FClearNodes: Boolean;
    FNodeType: String;
    function GetE(AIndex : Integer): TJSElement;
    function GetN(AIndex : Integer): TJSElementNode;
  Public
    Destructor Destroy; override;
    Procedure ClearNodes;
    Function GetEnumerator : TElementNodeEnumerator; reintroduce;
    Function AddNode(aIsAmbient : Boolean = False; aIsExport : Boolean = False) : TJSElementNode;
    Function AddNode(aEl : TJSElement; aIsAmbient : Boolean = False; aIsExport : Boolean = False) : TJSElementNode;
    Function InsertNode(Index: integer) : TJSElementNode;
    Property Nodes[AIndex : Integer] : TJSElementNode Read GetN ; default;
    Property JSElements[AIndex : Integer] : TJSElement Read GetE ;
    Property NodeType : String Read FNodeType Write FNodeType;
    Property DoClearNodes : Boolean Read FClearNodes Write FClearNodes;
  end;

  { TJSTypedParam }

  TJSTypedParam = Class(TJSElementNode)
  private
    FDestructured: TJSObjectTypeDef;
    FIsInferred: Boolean;
    FIsOptional: Boolean;
    FIsSpread: Boolean;
    FName: TJSTreeString;
    function GetTypeDef: TJSTypeDef;
  Public
    Procedure Assign(Source : TPersistent); override;
    Destructor Destroy; override;
    Property Type_ : TJSTypeDef read GetTypeDef;
    Property Name : TJSTreeString Read FName Write FName;
    Property IsInferred : Boolean Read FIsInferred Write FIsInferred;
    Property IsOptional : Boolean Read FIsOptional Write FIsOptional;
    Property IsSpread : Boolean Read FIsSpread Write FIsSpread;
    Property Destructured : TJSObjectTypeDef Read FDestructured Write FDestructured;
  end;

  { TJSTransientParamType }

  TJSTransientParamType = Class(TJSTypedParam)
  Public
    Destructor Destroy; override;
  end;

  { TJSTypedParams }

  TJSTypedParams = class(TJSElementNodes)
  private
    function GetNames(aIndex : Integer): TJSString;
    function GetParams(aIndex : Integer): TJSTypedParam;
    function GetTypes(aIndex : Integer): TJSElement;
  Public
    Constructor Create; Reintroduce;
    Constructor CreateTransient;
    function AddParam(aName : TJSTreeString) : TJSTypedParam;
    Property Params[aIndex : Integer] : TJSTypedParam Read GetParams; default;
    Property Types[aIndex : Integer] : TJSElement Read GetTypes;
    Property Names[aIndex : Integer] : TJSString Read GetNames;
  end;


  { TJSSourceElements - a list of elements, every element ends in semicolon,
    first Vars, then Functions, finally Statements }

  TJSSourceElements = Class(TJSElement)
  private
    FClasses: TJSElementNodes;
    FEnums: TJSElementNodes;
    FFunctions: TJSElementNodes;
    FInterfaces : TJSElementNodes;
    FModules: TJSElementNodes;
    FNamespaces: TJSElementNodes;
    FStatements: TJSElementNodes;
    FTypes: TJSElementNodes;
    FVars: TJSElementNodes;
  Public
    Constructor Create(ALine,AColumn : Integer; const ASource : String = ''); override;
    Destructor Destroy; override;
    Property Vars : TJSElementNodes Read FVars;
    Property Functions : TJSElementNodes Read FFunctions;
    Property Statements : TJSElementNodes Read FStatements;
    Property Classes : TJSElementNodes Read FClasses;
    Property Modules : TJSElementNodes Read FModules;
    Property Types : TJSElementNodes Read FTypes;
    Property Interfaces : TJSElementNodes Read FInterfaces;
    Property Enums : TJSElementNodes Read FEnums;
    Property Namespaces : TJSElementNodes Read FNamespaces;
  end;

  { TJSFunctionBody - the statement block of a function }

  TJSNamedElement = class(TJSElement)
  Private
    FName: TJSString;
  Public
    Property Name : TJSString Read FName Write FName;
  end;

  { TJSTypeDef }
  TTypeGuardKind = (tgkIs,tgkExtendsCond,tgkExtendsEquals);
  TJSTypeDef = class(TJSElement)
  Private
    FIsInferred: Boolean;
    FIsKeyOf : Boolean;
    FIsExtends : Boolean;
    FIsReadonly: Boolean;
    FIsSpread: Boolean;
    FIsTypeOf: Boolean;
    FTypeGuard: TJSTypeDef;
    FExtendsCond : TJSTypeDef;
    FExtendsTrue : TJSTypeDef;
    FExtendsFalse : TJSTypeDef;
    FTypeGuardKind: TTypeGuardKind;
  Public
    Destructor destroy; override;
    Property IsKeyOf : Boolean Read FIsKeyOf Write FIsKeyof;
    Property IsReadonly : Boolean Read FIsReadonly Write FIsReadonly;
    Property IsInferred : Boolean Read FIsInferred Write FIsInferred;
    Property IsTypeOf : Boolean Read FIsTypeOf Write FIsTypeOf;
    Property IsExtends : Boolean Read FIsExtends Write FIsExtends;
    Property IsSpread : Boolean Read FIsSpread Write FIsSpread;
    property TypeGuard : TJSTypeDef Read FTypeGuard Write FTypeGuard;
    property TypeGuardKind : TTypeGuardKind Read FTypeGuardKind Write FTypeGuardKind;
    Property ExtendsCond : TJSTypeDef Read FExtendsCond Write FExtendsCond;
    Property ExtendsTrue : TJSTypeDef Read FExtendsTrue Write FExtendsTrue;
    Property ExtendsFalse : TJSTypeDef Read FExtendsFalse Write FExtendsFalse;
  end;

  { TJSNamedParamTypeDef }

  TJSNamedParamTypeDef = Class(TJSTypeDef)
  private
    FName: TJSTypeDef;
    FParamType: TJSTypeDef;
  Public
    Destructor Destroy; override;
    Property ParamName : TJSTypeDef Read FName Write FName;
    Property ParamType : TJSTypeDef Read FParamType Write FParamType;
  end;
  
  { TJSTypeReference }

  TJSTypeReference = class(TJSTypeDef)
  Private
    FName: TJSString;
  Public
    // Type name or filename in import('filename') when IsImport is True
    Property Name : TJSString Read FName Write FName;
  end;

  { TJSImportTypeRef }

  TJSImportTypeRef = Class(TJSTypeReference)
  private
    FFileName: TJSString;
  Public
    Property FileName : TJSString Read FFileName Write FFileName;
  end;


  { TJSTypeFuncCall }

  TJSTypeFuncCall = Class(TJSTypeReference)
  private
    FArgType: TJSTypeDef;
  Public
    Destructor Destroy; override;
    Property ArgType : TJSTypeDef Read FArgType Write FargType;
  end;

  { TJSFixedStringReference }

  { TJSFixedValueReference }

  TJSFixedValueReference = Class(TJSTypeDef)
  private
    FFixedValue: TJSLiteral;
  Public
    destructor Destroy; override;
    Property FixedValue: TJSLiteral Read FFixedValue Write FFixedValue;
  end;


  { TJSStructuredTypeDef }

  TJSStructuredTypeDef = class(TJSTypeDef)
  Private
    FTypeParams: TJSElementNodes;
    FValues: TJSElementNodes;
  Public
    Constructor Create(ALine, AColumn: Integer; const ASource: String=''); override;
    Destructor destroy; override;
    procedure AddValue(aElement : TJSElement); virtual;
    property TypeParams : TJSElementNodes Read FTypeParams Write FTypeParams;
    Property Values : TJSElementNodes Read FValues;
  end;

  { TJSEnumTypeDef }

  { TJSEnumElement }

  TJSEnumElement = Class(TJSElement)
  private
    FName: TJSString;
    FValue: TJSElement;
  Public
    Destructor Destroy; override;
    Property Name : TJSString Read FName Write FName;
    Property Value : TJSElement Read FValue Write FValue;
  end;

  TJSEnumTypeDef = class(TJSStructuredTypeDef)
  private
    FIsConst: Boolean;
    function GetElement(aIndex : Integer): TJSEnumElement;
    function GetName(aIndex : Integer): TJSTreeString;
    function GetNameCount: Integer;
  Public
    // names are TJSEnumElement.Name
    Function AddName(aName : TJSTreeString) : TJSEnumElement;
    Property IsConst : Boolean Read FIsConst Write FIsConst;
    Property NameCount : Integer Read GetNameCount;
    Property Names[aIndex : Integer] : TJSTreeString Read GetName;
    Property Elements[aIndex : Integer] : TJSEnumElement Read GetElement; default;
  end;
  { TJSArrowFunctionTypeDef }

  { TJSNamedTypedElement }

  TJSNamedTypedElement = class(TJSNamedElement)
  private
    FElementType: TJSTypeDef;
  Public
    Destructor Destroy; override;
    Property ElementType : TJSTypeDef Read FElementType Write FElementType;
  end;

  { TJSFunctionParamDef }



  TJSFunctionParamDef = class(TJSNamedTypedElement)
  private
    function GetParamType: TJSTypeDef;
    procedure SetParamType(AValue: TJSTypeDef);
  Public
    Property ParamType : TJSTypeDef Read GetParamType Write SetParamType;
  end;

  TJSArrowFunctionTypeDef = class(TJSTypeDef) // Params are in values.
  private
    FFunction: TJSFuncDef;
  Public
    Constructor Create(ALine,AColumn : Integer; Const ASource : String = ''); override;
    Destructor Destroy; override;
    Property aFunction : TJSFuncDef Read FFunction Write FFunction;
  end;

  { TJSUnionTypeDef }

  TJSUnionOrTupleTypeDef = Class(TJSStructuredTypeDef)
  private
    function GetType(aIndex : Integer): TJSTypeDef;
    function GetTypeCount: Integer;
  Public
    Property Types[aIndex : Integer] : TJSTypeDef Read GetType;
    property TypeCount : Integer Read GetTypeCount;
  end;

  { TJSTupleTypeDef }

  TJSTupleTypeDef = class(TJSUnionOrTupleTypeDef)
  Public
    Function GetEqualTypes : Boolean;
  end;

  TOnlyConstants = (ocFalse,ocTrue,ocAllSameTypes);

  { TJSUnionOrIntersectTypeDef }

  TJSUnionOrIntersectTypeDef = class(TJSUnionOrTupleTypeDef)
  private
    FAllowEmpty: Boolean;
  Public
    Function GetOnlyConstants : TOnlyConstants;
    Property AllowEmpty : Boolean Read FAllowEmpty Write FAllowEmpty;
  end;

  TJSUnionTypeDef = Class(TJSUnionOrIntersectTypeDef);
  TJSInterSectionTypeDef = class(TJSUnionOrIntersectTypeDef);

  { TJSArrayTypeDef }

  TJSArrayTypeDef = Class(TJSTypeDef)
  private
    FIndexType,
    FBaseType: TJSTypeDef;
    
  Public
    Destructor Destroy; override;
    Property BaseType : TJSTypeDef Read FBaseType Write FBaseType;
    Property IndexType : TJSTypeDef Read FIndexType Write FIndexType;
  end;


  { TJSGenericTypeRef }

  TJSGenericTypeRef = Class(TJSStructuredTypeDef)
  private
    FBaseType: TJSTypeDef;
    function GetType(aIndex : Integer): TJSTypeDef;
    function GetTypeCount: Integer;
  Public
    Destructor destroy; override;
    Property BaseType : TJSTypeDef Read FBaseType Write FBaseType;
    Property ParamTypes[aIndex : Integer] : TJSTypeDef Read GetType;
    property ParamTypeCount : Integer Read GetTypeCount;
  end;

  TAccessibility = (accDefault,accPrivate,accProtected,accPublic);

  { TJSObjectTypeElementDef }
  TKeyOptionality = (koDefault,koOptional,koForceOptional,koDisableOptional);
  TKeyOptionalities = Set of TKeyOptionality;

  TJSObjectTypeElementDef = Class(TJSNamedTypedElement)
  private
    FAccessibility: TAccessibility;
    FIsReadOnly: Boolean;
    FIsStatic: Boolean;
    FOptional: TKeyOptionality;
    FIsAbstract : Boolean;
    FIsSet : Boolean;
    FIsGet : Boolean;
  Public
    Destructor Destroy; override;
    Property Optional : TKeyOptionality Read FOptional Write FOptional;
    Property Accessibility : TAccessibility Read FAccessibility Write FAccessibility;
    Property IsStatic : Boolean Read FIsStatic Write FIsStatic;
    Property IsReadOnly : Boolean Read FIsReadOnly Write FIsReadOnly;
    Property IsAbstract : Boolean Read FIsAbstract Write FIsAbstract;
    Property IsGet : Boolean Read FIsGet Write FIsGet;
    Property IsSet : Boolean Read FIsSet Write FIsSet;
  end;

  { TJSPropertyDeclaration }

  TJSPropertyDeclaration = Class(TJSObjectTypeElementDef)
  private
    function GetFixedStringValue: TJSTreeString;
  Public
    Property FixedStringValue : TJSTreeString Read GetFixedStringValue;
  end;

  { TJSClassConstDeclaration }

  TJSClassConstDeclaration = Class(TJSObjectTypeElementDef)
  private
    FValue: TJSElement;
  Public
    Destructor Destroy; override;
    Property Value : TJSElement Read FValue Write FValue;
  end;

  { TJSIndexSignatureDeclaration }

  TJSIndexSignatureDeclaration = Class(TJSObjectTypeElementDef)
  private
    FIndexName: TJSString;
    FindexType: TJSString;
    FInIndexType: TJSTypeDef;
    FIsFunction: Boolean;
  Public
    Destructor Destroy; override;
    Property IndexName : TJSString Read FIndexName Write FindexName;
    Property IndexType : TJSString Read FindexType Write FindexType;
    Property InIndexType : TJSTypeDef Read FInIndexType Write FInIndexType;
    Property IsFunction : Boolean Read FIsFunction Write FIsFunction;
  end;

  { TJSMethodDeclaration }

  TJSMethodDeclaration = Class(TJSObjectTypeElementDef)
  private
    FFuncDef: TJSFuncDef;
    FTypeParams: TJSElementNodes;
  Public
    Destructor Destroy; override;
    Property TypeParams : TJSElementNodes Read FTypeParams Write FTypeParams;
    Property FuncDef : TJSFuncDef Read FFuncDef Write FFuncDef;
  end;

  { TJSObjectTypeDef }

  TJSObjectTypeDef = Class(TJSStructuredTypeDef)
  private
    FName: TJSString;
    function GetElement(aIndex : Integer): TJSObjectTypeElementDef;
    function GetElementCount: Integer;
  Public
    procedure AddElement(const aEl: TJSObjectTypeElementDef);
    function HasSetter(const aName: TJSString): Boolean;
    Function HasAccessMembers(aAccess : TAccessibility) : Boolean;
    Function HasProperties : Boolean;
    Function IsFunctionDef : Boolean;
    Function FunctionDef : TJSFuncDef;
    Property Name : TJSString Read FName Write FName;
    Property Elements[aIndex : Integer] : TJSObjectTypeElementDef Read GetElement;
    Property ElementCount : Integer Read GetElementCount;
  end;


  { TJSTypeDeclaration }

  TJSTypeDeclaration = class(TJSNamedElement)
  Private
    FTypeDef : TJSTypeDef;
    FTypeParams : TJSElementNodes;
  Public
    Destructor Destroy; override;
    Property TypeParams : TJSElementNodes Read FTypeParams Write FTypeParams;
    Property TypeDef : TJSTypeDef Read FTypeDef Write FTypeDef;
  end;



  { TJSTypeStatement }

  TJSTypeStatement = Class(TJSDeclarationStatement)
  private
    FTypeDecl: TJSTypeDeclaration;
  Public
    Destructor Destroy; override;
    Function GetDeclaration: TJSElement; override;
    Property TypeDecl : TJSTypeDeclaration Read FTypeDecl Write FTypeDecl;
  end;


  { TJSEnumDeclaration }

  TJSEnumDeclaration = Class(TJSTypeDeclaration)
  private
    function GetEnumDef: TJSEnumTypeDef;
    procedure SetEnumDef(AValue: TJSEnumTypeDef);
  Public
    Property EnumDef : TJSEnumTypeDef Read GetEnumDef Write SetEnumDef;
  end;

  { TJSTypeStatement }

  { TJSEnumStatement }

  TJSEnumStatement = Class(TJSDeclarationStatement)
  private
    FEnumDecl : TJSEnumDeclaration;
  Public
    Destructor Destroy; override;
    Function GetDeclaration: TJSElement; override;
    Property EnumDecl : TJSEnumDeclaration Read FEnumDecl Write FEnumDecl;
  end;


  { TJSClassDeclaration }

  TJSMembersDeclaration = Class(TJSElement)
  Private
    FMembers: TJSSourceElements;
    procedure SetMembers(AValue: TJSSourceElements);
  Public
    Destructor Destroy; override;
    property Members : TJSSourceElements Read FMembers Write SetMembers;
  end;

  TJSNamedMembersDeclaration = Class(TJSMembersDeclaration)
  Private
    FName: TJSString;
  Public
    Property Name : TJSString Read FName Write FName;
  end;

  TJSClassDeclaration = Class(TJSNamedMembersDeclaration)
  private
    FExtends: TJSTypeDef;
    FImplementsTypes: TJSElementNodes;
    FisAbstract: Boolean;
    FTypeParams: TJSElementNodes;
  Public
    Destructor Destroy; override;
    Property TypeParams : TJSElementNodes Read FTypeParams Write FTypeParams;
    Property ImplementsTypes : TJSElementNodes Read FImplementsTypes Write FImplementsTypes;
    Property Extends : TJSTypeDef Read FExtends Write FExtends;
    Property IsAbstract : Boolean Read FisAbstract Write FisAbstract;
  end;

  //

  { TJSAmbientClassDeclaration }

  TJSAmbientClassDeclaration = class(TJSClassDeclaration)
  private
    FClassDef: TJSObjectTypeDef;
  Public
    Destructor Destroy; override;
    Property ClassDef : TJSObjectTypeDef Read FClassDef Write FClassDef;
  end;
  TJSAmbientClassDeclarationArray = Array of TJSAmbientClassDeclaration;
  { TJSClassStatement }

  TJSClassStatement =  Class(TJSDeclarationStatement)
  private
    FDecl: TJSClassDeclaration;
  Public
    Destructor Destroy; override;
    Function GetDeclaration: TJSElement; override;
    Property Decl : TJSClassDeclaration Read FDecl Write FDecl;
  end;

  { TJSInterfaceDeclaration }

  TJSInterfaceDeclaration = Class(TJSObjectTypeDef)
  private
    FExtends: TJSElementNodes;
  Public
    Destructor Destroy; override;
    Procedure AddExtends(Const aName : TJSString);
    property Extends : TJSElementNodes Read FExtends;
  end;
  TJSInterfaceDeclarationArray = array of TJSInterfaceDeclaration;

  { TJSInterfaceStatement }

  TJSInterfaceStatement = Class(TJSDeclarationStatement)
  private
    FDecl: TJSInterfaceDeclaration;
  Public
    Destructor Destroy; override;
    Function GetDeclaration: TJSElement; override;
    Property Decl : TJSInterfaceDeclaration Read FDecl Write FDecl;
  end;

  { TJSIndexSignatureStatement }

  TJSIndexSignatureStatement = Class(TJSDeclarationStatement)
  private
    FDecl: TJSIndexSignatureDeclaration;
  Public
    Destructor Destroy; override;
    Function GetDeclaration: TJSElement; override;
    Property Decl : TJSIndexSignatureDeclaration Read FDecl Write FDecl;
  end;


  { TJSModuleDeclaration }

  TJSModuleDeclaration = Class(TJSNamedMembersDeclaration);

  { TJSModuleStatement }

  TJSModuleStatement = Class(TJSDeclarationStatement)
  private
    FDecl: TJSModuleDeclaration;
  Public
    Destructor Destroy; override;
    Function GetDeclaration: TJSElement; override;
    Property Decl : TJSModuleDeclaration Read FDecl Write FDecl;
  end;

  { TJSNamespaceDeclaration }

  TJSNamespaceDeclaration = Class(TJSNamedMembersDeclaration)
  Private
    FIsGlobal : Boolean;
  Public
    Property IsGlobal : Boolean Read FIsGLobal Write FIsGlobal;  
  end;

  { TJSNameSpaceStatement }

  TJSNameSpaceStatement = Class(TJSDeclarationStatement)
  private
    FDecl: TJSNameSpaceDeclaration;
  Public
    Destructor Destroy; override;
    Function GetDeclaration: TJSElement; override;
    Property Decl : TJSNameSpaceDeclaration Read FDecl Write FDecl;
  end;


implementation

{ TJSTupleTypeDef }

function TJSTupleTypeDef.GetEqualTypes: Boolean;

Var
  I : Integer;
  N : TJSString;

begin
  I:=Values.Count-1;
  Result:=True;
  While (I>=0) and (Result) do
    begin
    Result:=(Values[I].Node is TJSTypeReference);
    if Result then
      if I=Values.Count-1 then
        N:=(Values[I].Node as TJSTypeReference).Name
      else
        Result:=N=(Values[I].Node as TJSTypeReference).Name;
    Dec(i);
    end;
end;

{ TJSUnionOrIntersectTypeDef }

function TJSUnionOrIntersectTypeDef.GetOnlyConstants: TOnlyConstants;

Var
  I : integer;
  FT : TJSType;
  Ref : TJSFixedValueReference;

begin
  Result:=ocAllSameTypes;
  I:=Values.Count-1;
  While (I>=0) and (Result<>ocFalse) do
    begin
    if Not (Values[I].Node is TJSFixedValueReference) then
      Result:=ocFalse
    else
      begin
      Ref:=Values[I].Node as TJSFixedValueReference;
      If (I=Values.Count-1) then
        ft:=Ref.FixedValue.Value.ValueType
      else if (ft<>Ref.FixedValue.Value.ValueType) then
        Result:=ocTrue
      end;
    Dec(I);
    end;
end;

{ TJSTransientParamType }

destructor TJSTransientParamType.Destroy;
begin
  FDestructured:=Nil;
  FNode:=nil;
  inherited Destroy;
end;

{ TJSTransientElementNode }

destructor TJSTransientElementNode.Destroy;
begin
  Node:=Nil;
  inherited Destroy;
end;

{ TElementNodeEnumerator }

function TElementNodeEnumerator.GetCurrent: TJSElementNode;
begin
  Result:=(Inherited GetCurrent) as TJSElementNode
end;

{ TJSClassConstDeclaration }

destructor TJSClassConstDeclaration.Destroy;
begin
  FreeAndNil(FValue);
  inherited Destroy;
end;

{ TTJSTypeFuncCall }

destructor TJSTypeFuncCall.Destroy;
begin
  FreeAndNil(FArgType);
  inherited Destroy;
end;

{ TJSTypedParam }

function TJSTypedParam.GetTypeDef: TJSTypeDef;
begin
  if Assigned(Node) then
    Result:=Node as TJSTypeDef
  else
    Result:=Nil;
end;

procedure TJSTypedParam.Assign(Source: TPersistent);

Var
  aParam : TJSTypedParam absolute Source;

begin
  If Source is TJSTypedParam then
    begin
    FDestructured:=aParam.FDestructured;
    FIsInferred:=aParam.IsInferred;
    FIsOptional:=aParam.FIsOptional;
    FIsSpread:=aParam.FIsSpread;
    FName:=aParam.Name;
    end;
  inherited Assign(Source);
end;

destructor TJSTypedParam.Destroy;
begin
  FreeAndNil(FDestructured);
  inherited Destroy;
end;

{ TJSTypeDef }

destructor TJSTypeDef.destroy;
begin
  FreeAndNil(FTypeGuard);
  FreeAndNil(FExtendsCond);
  FreeAndNil(FExtendsTrue);
  FreeAndNil(FExtendsFalse);
  inherited destroy;
end;

{ TNamedParamTypeDef }

destructor TJSNamedParamTypeDef.Destroy;
begin
  FreeAndNil(FName);
  FreeAndNil(FParamType);
  inherited Destroy;
end;

{ TJSEnumElement }

destructor TJSEnumElement.Destroy;
begin
  FreeAndNil(FValue);
  inherited Destroy;
end;

{ TJSIndexSignatureDeclaration }

destructor TJSIndexSignatureDeclaration.Destroy;
begin
  FreeAndNil(FInIndexType);
  inherited Destroy;
end;

{ TJSVariableStatement }

destructor TJSVariableStatement.Destroy;
begin
  FreeAndNil(FA);
  inherited Destroy;
end;

function TJSVariableStatement.GetDeclaration: TJSElement;
begin
  Result:=FA;
end;

{ TJSNameSpaceStatement }

destructor TJSNameSpaceStatement.Destroy;
begin
  FreeAndNil(FDecl);
  inherited Destroy;
end;

function TJSNameSpaceStatement.GetDeclaration: TJSElement;
begin
  Result:=FDecl;
end;

{ TJSModuleStatement }

destructor TJSModuleStatement.Destroy;
begin
  FreeAndNil(FDecl);
  inherited Destroy;
end;

function TJSModuleStatement.GetDeclaration: TJSElement;
begin
  Result:=FDecl;
end;

{ TJSFixedValueReference }

destructor TJSFixedValueReference.destroy;
begin
  FreeAndNil(FFixedValue);
  Inherited;
end;

{ TJSAmbientClassDeclaration }


destructor TJSAmbientClassDeclaration.Destroy;
begin
  if Members<>nil then
    begin
    Members.Vars.ClearNodes;
    Members.Functions.ClearNodes;
    end;
  FreeAndNil(FClassDef);
  inherited Destroy;
end;

{ TJSClassDeclaration }

destructor TJSClassDeclaration.Destroy;
begin
  FreeAndNil(FImplementsTypes);
  FreeAndNil(FTypeParams);
  FreeAndNil(FExtends);
  inherited Destroy;
end;

{ TJSPropertyDeclaration }

function TJSPropertyDeclaration.GetFixedStringValue: TJSTreeString;
begin
  if ElementType is TJSFixedValueReference then
    Result:=TJSFixedValueReference(ElementType).FixedValue.Value.AsString
  else
    Result:='';
end;

{ TJSIndexSignatureStatement }

destructor TJSIndexSignatureStatement.Destroy;
begin
  FreeAndNil(FDecl);
  inherited Destroy;
end;

function TJSIndexSignatureStatement.GetDeclaration: TJSElement;
begin
  Result:=FDecl;
end;

{ TJSClassStatement }

destructor TJSClassStatement.Destroy;
begin
  FreeAndNil(FDecl);
  inherited Destroy;
end;

function TJSClassStatement.GetDeclaration: TJSElement;
begin
  Result:=FDecl;
end;

{ TJSMethodDeclaration }

destructor TJSMethodDeclaration.Destroy;
begin
  FreeAndNil(FTypeParams);
  FreeAndNil(FFuncDef);
  inherited Destroy;
end;

{ TJSNamedTypedElement }

destructor TJSNamedTypedElement.Destroy;
begin
  FreeAndNil(FElementType);
  inherited Destroy;
end;

{ TJSInterfaceDeclarationStatement }

destructor TJSInterfaceStatement.Destroy;
begin
  FreeAndNil(FDecl);
  inherited Destroy;
end;

function TJSInterfaceStatement.GetDeclaration: TJSElement;
begin
  Result:=FDecl;
end;

{ TJSInterfaceDeclaration }

destructor TJSInterfaceDeclaration.Destroy;
begin
  FreeAndNil(FExtends);
  inherited Destroy;
end;

procedure TJSInterfaceDeclaration.AddExtends(const aName: TJSString);

Var
  Lit : TJSLiteral;

begin
  if FExtends=Nil then
    FExtends:=TJSElementNodes.Create(TJSElementNode);
  Lit:=TJSLiteral.Create(0,0,'');
  Lit.Value.AsString:=aName;
  FExtends.AddNode().Node:=Lit;
end;

{ TJSEnumStatement }

destructor TJSEnumStatement.Destroy;
begin
  FreeAndNil(FEnumDecl);
  inherited Destroy;
end;

function TJSEnumStatement.GetDeclaration: TJSElement;
begin
  Result:=FEnumDecl;
end;

{ TJSEnumTypeDef }

function TJSEnumTypeDef.GetName(aIndex : Integer): TJSTreeString;
begin
  Result:=GetElement(aIndex).Name;
end;

function TJSEnumTypeDef.GetElement(aIndex : Integer): TJSEnumElement;
begin
  Result:=TJSEnumElement(Values.Nodes[aIndex].Node)
end;

function TJSEnumTypeDef.GetNameCount: Integer;
begin
  Result:=Values.Count;
end;

Function TJSEnumTypeDef.AddName(aName: TJSTreeString) : TJSEnumElement;


begin
  Result:=TJSEnumElement.Create(0,0,'');
  Result.Name:=aName;
  Values.AddNode().Node:=Result
end;

{ TJSEnumDeclaration }

function TJSEnumDeclaration.GetEnumDef: TJSEnumTypeDef;
begin
  Result:=Self.TypeDef as TJSEnumTypeDef;
end;

procedure TJSEnumDeclaration.SetEnumDef(AValue: TJSEnumTypeDef);
begin
  TypeDef:=aValue;
end;

{ TJSFunctionParamDef }

function TJSFunctionParamDef.GetParamType: TJSTypeDef;
begin
  Result:=ElementType;
end;

procedure TJSFunctionParamDef.SetParamType(AValue: TJSTypeDef);
begin
  ElementType:=aValue;
end;

{ TJSArrowFunctionTypeDef }

constructor TJSArrowFunctionTypeDef.Create(ALine, AColumn: Integer; const ASource: String);
begin
  inherited Create(ALine, AColumn, ASource);
  FFunction:=TJSFuncDef.Create;
end;

destructor TJSArrowFunctionTypeDef.Destroy;
begin
  FreeAndNil(FFunction);
  inherited Destroy;
end;

{ TJSArrowFunction }

class function TJSArrowFunction.OperatorToken: tjsToken;
begin
  Result:=tjsArrow;
end;

{ TJSObjectTypeDef }

function TJSObjectTypeDef.GetElement(aIndex : Integer): TJSObjectTypeElementDef;
begin
  Result:=Values[aIndex].Node as TJSObjectTypeElementDef;
end;

function TJSObjectTypeDef.GetElementCount: Integer;
begin
  Result:=Values.Count;
end;

Procedure TJSObjectTypeDef.AddElement(const aEl: TJSObjectTypeElementDef);
begin
  Values.AddNode(False).Node:=aEl;
end;

Function TJSObjectTypeDef.HasSetter(Const aName : TJSString) : Boolean;

Var
  I : Integer;
  aEl : TJSObjectTypeElementDef;

begin
  Result:=False;
  I:=ElementCount-1;
  While (Not Result) and (I>=0) do
    begin
    aEl:=Elements[i];
    Result:=(aName=aEl.Name) and (aEl is TJSMethodDeclaration) and (aEl.IsSet);
    Dec(I);
    end;
end;


function TJSObjectTypeDef.HasAccessMembers(aAccess: TAccessibility): Boolean;

Var
  I : integer;

begin
  Result:=False;
  I:=ElementCount-1;
  While (Not Result) and (I>=0) do
    begin
    Result:=Elements[i].Accessibility=aAccess;
    Dec(I);
    end;
end;

function TJSObjectTypeDef.HasProperties: Boolean;

Var
  I : Integer;

begin
  Result:=False;
  I:=ElementCount-1;
  While (not Result) and (I>=0) do
    begin
    Result:=Elements[I] is TJSPropertyDeclaration;
    Dec(i);
    end;
end;

function TJSObjectTypeDef.IsFunctionDef: Boolean;
begin
  Result:=(ElementCount=1) and (Elements[0] is TJSMethodDeclaration) and (TJSMethodDeclaration(Elements[0]).Name='');
end;

function TJSObjectTypeDef.FunctionDef: TJSFuncDef;
begin
  if IsFunctionDef then
    Result:=TJSMethodDeclaration(Elements[0]).FuncDef
  else
    Result:=Nil;
end;

{ TJSObjectTypeElementDef }

destructor TJSObjectTypeElementDef.Destroy;
begin
  FreeAndNil(FElementType);
  inherited Destroy;
end;

{ TJSArrayTypeDef }

destructor TJSArrayTypeDef.Destroy;
begin
  FreeAndNil(FIndexType);
  FreeAndNil(FBaseType);
  inherited Destroy;
end;

{ TJSUnionTypeDef }

function TJSUnionOrTupleTypeDef.GetType(aIndex : Integer): TJSTypeDef;
begin
  Result:=Values.Nodes[aIndex].Node as TJSTypeDef;
end;

function TJSUnionOrTupleTypeDef.GetTypeCount: Integer;
begin
  Result:=Values.Count;
end;

{ TJSTypeStatement }

destructor TJSTypeStatement.Destroy;
begin
  FreeAndNil(FTypeDecl);
  inherited Destroy;
end;

function TJSTypeStatement.GetDeclaration: TJSElement;
begin
  Result:=FTypeDecl;
end;

{ TJSTypeDeclaration }

destructor TJSTypeDeclaration.Destroy;
begin
  // Writeln('Destroying ',ClassName);
  FreeAndNil(FTypeDef);
  FreeAndNil(FTypeParams);
  inherited Destroy;
end;

{ TJSGenericTypeRef }

function TJSGenericTypeRef.GetType(aIndex : Integer): TJSTypeDef;
begin
  Result:=Values[aIndex].Node as TJSTypeDef;
end;

function TJSGenericTypeRef.GetTypeCount: Integer;
begin
  Result:=Values.Count;
end;

destructor TJSGenericTypeRef.destroy;
begin
  FreeAndNil(FBaseType);
  inherited destroy;
end;

{ TJSStructuredTypeDef }

constructor TJSStructuredTypeDef.Create(ALine, AColumn: Integer; const ASource: String);
begin
  inherited Create(ALine, AColumn, ASource);
  FValues:=TJSElementNodes.Create(TJSElementNode);
end;

destructor TJSStructuredTypeDef.destroy;
begin
  FreeAndNil(FTypeParams);
  FreeAndNil(FValues);
  Inherited;
end;

procedure TJSStructuredTypeDef.AddValue(aElement: TJSElement);
begin
  FValues.AddNode.Node:=aElement;
end;

{ TJSMembersDeclaration }

procedure TJSMembersDeclaration.SetMembers(AValue: TJSSourceElements);
begin
  if FMembers=AValue then Exit;
  FMembers:=AValue;
end;

destructor TJSMembersDeclaration.Destroy;
begin
  FreeAndNil(FMembers);
  inherited Destroy;
end;

{ TJSTypedParams }

function TJSTypedParams.GetNames(aIndex : Integer): TJSString;
begin
  Result:=Params[aIndex].Name;
end;

function TJSTypedParams.GetParams(aIndex : Integer): TJSTypedParam;
begin
  Result:=TJSTypedParam(Items[aIndex]);
end;

function TJSTypedParams.GetTypes(aIndex : Integer): TJSElement;
begin
  Result:=Params[aIndex].Node;
end;

constructor TJSTypedParams.Create;

begin
  Inherited Create(TJSTypedParam);
end;

constructor TJSTypedParams.CreateTransient;

begin
  Inherited Create(TJSTransientParamType);
end;

function TJSTypedParams.AddParam(aName: TJSTreeString): TJSTypedParam;
begin
  Result:=add as TJSTypedParam;
  Result.Name:=aName;
end;

{ TJSAliasElements }

function TJSAliasElements.GetA(AIndex: Integer): TJSAliasElement;
begin
  Result:=TJSAliasElement(Items[aIndex])
end;

function TJSAliasElements.AddAlias: TJSAliasElement;
begin
  Result:=TJSAliasElement(add);
end;

{ TJSNamedExportElements }

function TJSExportNameElements.GetE(AIndex: Integer): TJSExportNameElement;
begin
  Result:=TJSExportNameElement(Items[aIndex]);
end;

function TJSExportNameElements.AddElement: TJSExportNameElement;
begin
  Result:=TJSExportNameElement(Add);
end;

{ TJSExportStatement }

function TJSExportStatement.GetNamedExports: TJSExportNameElements;
begin
  If FNamedExports=Nil then
    FNamedExports:=TJSExportNameElements.Create(TJSExportNameElement);
  Result:=FNamedExports;
end;

function TJSExportStatement.GetHaveNamedExports: Boolean;
begin
  Result:=Assigned(FNamedExports)
end;

destructor TJSExportStatement.Destroy;
begin
  FreeAndNil(FNamedExports);
  FreeAndNil(FDeclaration);
  inherited Destroy;
end;

{ TJSImportStatement }

function TJSImportStatement.GetNamedImports: TJSNamedImportElements;
begin
  if FNamedImports=Nil then
    FNamedImports:=TJSNamedImportElements.Create(TJSNamedImportElement);
  Result:=FNamedImports;
end;

function TJSImportStatement.GetHaveNamedImports: Boolean;
begin
  Result:=Assigned(FNamedImports);
end;

destructor TJSImportStatement.Destroy;
begin
  FreeAndNil(FNamedImports);
  FreeAndNil(FExpression);
  inherited Destroy;
end;

{ TJSNamedImportElements }

function TJSNamedImportElements.GetE(aIndex: Integer): TJSNamedImportElement;
begin
  Result:=TJSNamedImportElement(Items[aIndex]);
end;

function TJSNamedImportElements.AddElement: TJSNamedImportElement;
begin
  Result:=TJSNamedImportElement(Add);
end;

{ TJSYieldExpression }

class function TJSYieldExpression.PrefixOperatorToken: tjsToken;
begin
  Result:=tjsYield;
end;

{$IFDEF NOCLASSES}
{ TCollectionItem }

Constructor TCollectionItem.Create(ACollection: TCollection);
begin
  FCollection:=ACollection;
end;

Destructor TCollectionItem.Destroy;
begin
  if Assigned(FCollection) then
    FCollection.Remove(Self);
  inherited Destroy;
end;

{ TCollection }

function TCollection.GetI(AIndex : Integer): TCollectionItem;
begin
  if (AIndex>=0) and (AIndex<FCount) then
    Result:=FItems[AIndex]
  else
    Raise EListError.CreateFmt('Collection index (%d) out of bounds.',[AIndex]);
end;

function TCollection.GetCount: Integer;
begin
  Result:=FCount;
end;

Procedure TCollection.Remove(AItem: TCollectionItem);

Var
  I,J : Integer;

begin
  if (AItem=Nil)  then exit;
  I:=Count-1;
  While (I>=0) and (FItems[I]<>AItem) do
    Dec(i);
  For J:=I to Count-2 do
    FItems[I]:=FItems[i+1];
  Dec(FCount);
end;

Constructor TCollection.Create(AItemClass: TCollectionItemClass);
begin
  FItemClass:=AItemClass;
end;

Destructor TCollection.Destroy;
begin
  Clear;
  inherited Destroy;
end;

Procedure TCollection.Clear;

Var
  I : Integer;

begin
  For I:=0 to Count-1 do
    begin
    FItems[i].FCollection:=Nil;
    FItems[i].Destroy;
    end;
  FCount:=0;
  SetLength(Fitems,0);
end;

Function TCollection.Add: TCollectionItem;
Var
  NL : Integer;
begin
  If FCount=Length(FItems) then
    begin
    NL:=Length(FItems)*3 div 2;
    if NL=0 then NL:=10;
    SetLength(FItems,NL);
    end;
  Result:=FItemClass.Create(Self);
  FItems[FCount]:=Result;
  Inc(FCount);
end;

{ TStrings }

function TStrings.GetCount: Integer;
begin
  Result:=FCount;
end;

function TStrings.GetS(AIndex : Integer): String;
begin
  if (AIndex>=0) and (AIndex<FCount) then
    Result:=FStrings[AIndex]
  else
    Raise EListError.CreateFmt('List index (%d) out of bounds.',[AIndex]);
end;

procedure TStrings.SetS(AIndex : Integer; const AValue: String);
begin
  if (AIndex>=0) and (AIndex<=FCount) then
    begin
    if (AIndex=FCount) then
      Add(AValue)
    else
      FStrings[AIndex]:=AValue;
    end
  else
    Raise EListError.CreateFmt('List index (%d) out of bounds.',[AIndex]);
end;

Function TStrings.Add(Const S: String): Integer;

Var
  NL : Integer;
begin
  If FCount=Length(FStrings) then
    begin
    NL:=Length(FStrings)*3 div 2;
    if NL=0 then NL:=10;
    SetLength(FStrings,NL);
    end;
  FStrings[FCount]:=S;
  Inc(FCount);
end;

Procedure TStrings.Assign(Source: TStrings);

Var
  I : Integer;

begin
  SetLength(FStrings,Length(Source.FStrings));
  FCount:=Source.FCount;
  For I:=0 to FCount-1 do
    FStrings[i]:=Source.FStrings[i];
end;
{$ENDIF NOCLASSES}

{ TJSXOREqAssignStatement }

Class function TJSXOREqAssignStatement.OperatorToken: tjsToken;
begin
  Result:=tjsXOREq;
end;

{ TJSOREqAssignStatement }

Class function TJSOREqAssignStatement.OperatorToken: tjsToken;
begin
  Result:=tjsOREQ;
end;

{ TJSANDEqAssignStatement }

Class function TJSANDEqAssignStatement.OperatorToken: tjsToken;
begin
  Result:=tjsAndEq;
end;

{ TJSURShiftEqAssignStatement }

Class function TJSURShiftEqAssignStatement.OperatorToken: tjsToken;
begin
  Result:=tjsURSHIFTEQ;
end;

{ TJSRShiftEqAssignStatement }

Class function TJSRShiftEqAssignStatement.OperatorToken: tjsToken;
begin
  Result:=tjsRSHIFTEQ;
end;

{ TJSLShiftEqAssignStatement }

Class function TJSLShiftEqAssignStatement.OperatorToken: tjsToken;
begin
   Result:=tjsLSHIFTEQ;
end;

{ TJSSubEqAssignStatement }

Class function TJSSubEqAssignStatement.OperatorToken: tjsToken;
begin
  Result:=tjsMINUSEQ;
end;

{ TJSAddEqAssignStatement }

Class function TJSAddEqAssignStatement.OperatorToken: tjsToken;
begin
  Result:=tjsPLUSEQ;
end;

{ TJSModEqAssignStatement }

Class function TJSModEqAssignStatement.OperatorToken: tjsToken;
begin
  Result:=tjsMODEQ;
end;

{ TJSDivEqAssignStatement }

Class function TJSDivEqAssignStatement.OperatorToken: tjsToken;
begin
  Result:=tjsDIVEQ;
end;

{ TJSMulEqAssignStatement }

Class function TJSMulEqAssignStatement.OperatorToken: tjsToken;
begin
  Result:=tjsMULEQ;
end;

{ TJSSimpleAssignStatement }

Class function TJSSimpleAssignStatement.OperatorToken: tjsToken;
begin
  Result:=tjsAssign;
end;

{ TJSLabeledStatement }

Destructor TJSLabeledStatement.Destroy;
begin
  FreeAndNil(FLabel);
  inherited Destroy;
end;

{ TJSCommaExpression }

Class function TJSCommaExpression.OperatorToken: tjsToken;
begin
  Result:=tjsComma;
end;

{ TJSMultiplicativeExpressionMod }

Class function TJSMultiplicativeExpressionMod.OperatorToken: tjsToken;
begin
  Result:=tjsMod;
end;

{ TJSPowerExpression }

class function TJSPowerExpression.OperatorToken: tjsToken;
begin
  Result:=tjsPower;
end;

{ TJSMultiplicativeExpressionDiv }

Class function TJSMultiplicativeExpressionDiv.OperatorToken: tjsToken;
begin
  Result:=tjsDiv;
end;

{ TJSMultiplicativeExpressionMul }

Class function TJSMultiplicativeExpressionMul.OperatorToken: tjsToken;
begin
  Result:=tjsMul;
end;

{ TJSAdditiveExpressionMinus }

Class function TJSAdditiveExpressionMinus.OperatorToken: tjsToken;
begin
  Result:=tjsMinus;
end;

{ TJSAdditiveExpressionPlus }

Class function TJSAdditiveExpressionPlus.OperatorToken: tjsToken;
begin
  Result:=tjsPlus;
end;

{ TJSURShiftExpression }

Class function TJSURShiftExpression.OperatorToken: tjsToken;
begin
  Result:=tjsURshift;
end;

{ TJSRShiftExpression }

Class function TJSRShiftExpression.OperatorToken: tjsToken;
begin
  Result:=tjsRSHIFT;
end;

{ TJSLShiftExpression }

Class function TJSLShiftExpression.OperatorToken: tjsToken;
begin
  Result:=tjsLSHIFT;
end;

{ TJSRelationalExpressionInstanceOf }

Class function TJSRelationalExpressionInstanceOf.OperatorToken: tjsToken;
begin
  Result:=tjsInstanceOf;
end;

Class Function TJSRelationalExpressionInstanceOf.AllowCompact: Boolean;
begin
  Result:=False;
end;

{ TJSRelationalExpressionIn }

Class function TJSRelationalExpressionIn.OperatorToken: tjsToken;
begin
  Result:=tjsIn;
end;

Class Function TJSRelationalExpressionIn.AllowCompact: Boolean;
begin
  Result:=False;
end;

{ TJSRelationalExpressionGE }

Class function TJSRelationalExpressionGE.OperatorToken: tjsToken;
begin
  Result:=tjsGE;
end;

{ TJSRelationalExpressionLE }

Class function TJSRelationalExpressionLE.OperatorToken: tjsToken;
begin
  Result:=tjsLE;
end;

{ TJSRelationalExpressionGT }

Class function TJSRelationalExpressionGT.OperatorToken: tjsToken;
begin
  Result:=tjsGT;
end;

{ TJSRelationalExpressionLT }

Class function TJSRelationalExpressionLT.OperatorToken: tjsToken;
begin
  Result:=tjsLT;
end;

{ TJSEqualityExpressionSNE }

Class function TJSEqualityExpressionSNE.OperatorToken: tjsToken;
begin
  Result:=tjsSNE;
end;

{ TJSEqualityExpressionSEQ }

Class function TJSEqualityExpressionSEQ.OperatorToken: tjsToken;
begin
  Result:=tjsSEQ;
end;

{ TJSEqualityExpressionNE }

Class function TJSEqualityExpressionNE.OperatorToken: tjsToken;
begin
  Result:=tjsNE;
end;

{ TJSEqualityExpressionEQ }

Class function TJSEqualityExpressionEQ.OperatorToken: tjsToken;
begin
  Result:=tjsEQ;
end;

{ TJSBinaryExpression }

Class function TJSBinaryExpression.OperatorToken: tjsToken;
begin
  Result:=tjsUnknown
end;

Class function TJSBinaryExpression.OperatorString: string;

Var
  T : TJSToken;
begin
  T:=OperatorToken;
  if (T<>tjsUnknown) then
    begin
    Result:=TokenInfos[T]
    end
  else
    Result:='';
end;

Class Function TJSBinaryExpression.AllowCompact: Boolean;
begin
  Result:=True
end;

{ TJSBitwiseXOrExpression }

Class function TJSBitwiseXOrExpression.OperatorToken : tjsToken;
begin
  Result:=tjsXor
end;

{ TJSBitwiseOrExpression }

Class function TJSBitwiseOrExpression.OperatorToken : tjsToken;
begin
  Result:=tjsOr
end;

{ TJSBitwiseAndExpression }

Class function TJSBitwiseAndExpression.OperatorToken : tjsToken;
begin
  Result:=tjsAnd
end;

{ TJSLogicalAndExpression }

Class function TJSLogicalAndExpression.OperatorToken : tjsToken;
begin
  Result:=tjsAndAnd
end;

{ TJSLogicalOrExpression }

Class function TJSLogicalOrExpression.OperatorToken : tjsToken;
begin
  Result:=tjsOrOr
end;

{ TJSUnaryVoidExpression }

Class function TJSUnaryVoidExpression.PrefixOperatorToken : tjsToken;
begin
  Result:=tjsvoid;
end;

{ TJSThrowStatement }

Class function TJSThrowStatement.PrefixOperatorToken: tjsToken;
begin
  Result:=tjsThrow;
end;

{ TJSUnaryBracketsExpression }

class function TJSUnaryBracketsExpression.PrefixOperatorToken: tjsToken;
begin
  Result:=tjsBraceOpen;
end;

class function TJSUnaryBracketsExpression.PostFixOperatorToken: tjsToken;
begin
  Result:=tjsBraceClose;
end;

{ TJSUnaryPostMinusMinusExpression }

Class function TJSUnaryPostMinusMinusExpression.PostFixOperatorToken : tjsToken;
begin
  Result:=tjsMinusMinus;
end;

{ TJSUnaryPostPlusPlusExpression }

Class function TJSUnaryPostPlusPlusExpression.PostFixOperatorToken : tjsToken;
begin
  Result:=tjsPlusPlus;
end;

{ TJSUnaryNotExpression }

Class function TJSUnaryNotExpression.PrefixOperatorToken : tjsToken;
begin
  Result:=tjsNot;
end;

{ TJSUnaryInvExpression }

Class function TJSUnaryInvExpression.PrefixOperatorToken : tjsToken;
begin
  Result:=tjsinv;
end;

{ TJSUnaryMinusExpression }

Class function TJSUnaryMinusExpression.PrefixOperatorToken : tjsToken;
begin
  Result:=tjsMinus;
end;

{ TJSUnaryPlusExpression }

Class function TJSUnaryPlusExpression.PrefixOperatorToken : tjsToken;
begin
  Result:=tjsPlus;
end;

{ TJSUnaryPreMinusMinusExpression }

Class function TJSUnaryPreMinusMinusExpression.PrefixOperatorToken : tjsToken;
begin
  Result:=tjsMinusMinus;
end;

{ TJSUnaryPrePlusPlusExpression }

Class function TJSUnaryPrePlusPlusExpression.PrefixOperatorToken : tjsToken;
begin
  Result:=tjsPlusPlus;
end;

{ TJSUnaryTypeOfExpression }

Class function TJSUnaryTypeOfExpression.PrefixOperatorToken : tjsToken;
begin
  Result:=tjsTypeOf;
end;

{ TJSAwaitExpression }

class function TJSAwaitExpression.PrefixOperatorToken: tjsToken;
begin
  Result:=tjsAwait;
end;

{ TJSUnaryDeleteExpression }

Class function TJSUnaryDeleteExpression.PrefixOperatorToken : tjsToken;
begin
  Result:=tjsdelete;
end;

{ TJSElement }

constructor TJSElement.Create(ALine, AColumn: Integer; const ASource: String);
begin
  FLine:=ALine;
  FColumn:=AColumn;
  FSource:=ASource;
end;

destructor TJSElement.Destroy;
begin
  if Assigned(GlobalFreeHook) then
    GlobalFreeHook(Self);
  inherited Destroy;
end;

procedure TJSElement.AssignPosition(El: TJSElement);
begin
  Source:=El.Source;
  Line:=El.Line;
  Column:=El.Column;
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

constructor TJSRegularExpressionLiteral.Create(ALine, AColumn: Integer;
  const ASource: String);
begin
  inherited Create(ALine, AColumn, ASource);
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

function TJSArrayLiteralElements.AddElement: TJSArrayLiteralElement;
begin
  Result:=TJSArrayLiteralElement(Add);
end;

{ TJSArrayLiteral }

function TJSArrayLiteral.GetCount: Integer;
begin
  Result:=Elements.Count;
end;

constructor TJSArrayLiteral.Create(ALine, AColumn: Integer; const ASource: String);
begin
  inherited Create(ALine, AColumn, ASource);
  FElements:=TJSArrayLiteralElements.Create(TJSArrayLiteralElement);
end;

procedure TJSArrayLiteral.AddElement(El: TJSElement);
var
  ArrEl: TJSArrayLiteralElement;
begin
  ArrEl:=Elements.AddElement;
  ArrEl.ElementIndex:=Elements.Count-1;
  ArrEl.Expr:=El;
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


function TJSObjectLiteralElements.AddElement: TJSObjectLiteralElement;
begin
  Result:=TJSObjectLiteralElement(Add);
end;

{ TJSObjectLiteral }

constructor TJSObjectLiteral.Create(ALine, AColumn: Integer; const ASource: String = '');
begin
  inherited Create(ALine, AColumn, ASource);
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
  FreeAndNil(FExpr);
  inherited Destroy;
end;

{ TJSArrayLiteralElement }

destructor TJSArrayLiteralElement.Destroy;
begin
  FreeAndNil(FExpr);
  inherited Destroy;
end;

{ TJSNewMemberExpression }

destructor TJSNewMemberExpression.Destroy;
begin
  FreeAndNil(FArgs);
  inherited Destroy;
end;

procedure TJSNewMemberExpression.AddArg(El: TJSElement);
begin
  if Args=nil then
    Args:=TJSArguments.Create(Line,Column,Source);
  Args.Elements.AddElement.Expr:=El;
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

procedure TJSCallExpression.AddArg(El: TJSElement);
begin
  Args.Elements.AddElement.Expr:=El;
end;

procedure TJSCallExpression.InsertArg(Index: integer; El: TJSElement);
var
  NewEl: TJSArrayLiteralElement;
begin
  NewEl:=TJSArrayLiteralElement(Args.Elements.Insert(Index));
  NewEl.Expr:=El;
end;

{ TJSUnary }

Class function TJSUnary.PrefixOperatorToken: tjsToken;
begin
  Result:=tjsUnknown;
end;

Class function TJSUnary.PostFixOperatorToken: tjsToken;
begin
  Result:=tjsUnknown;
end;

Class function TJSUnary.PrefixOperator: String;

Var
  T : TJSToken;

begin
  T:=PrefixOperatorToken;
  if (T=tjsUnknown) then
    Result:=''
  else
    begin
    Result:=TokenInfos[t];
    if t in [tjsTypeOf,tjsVoid,tjsDelete,tjsThrow,tjsAwait,tjsYield] then
      Result:=Result+' ';
    end;
end;

Class function TJSUnary.PostFixOperator: String;

Var
  T : TJSToken;

begin
  T:=PostFixOperatorToken;
  if (T=tjsUnknown) then
    Result:=''
  else
    Result:=TokenInfos[t];
end;

Destructor TJSUnary.Destroy;
begin
  FreeAndNil(FA);
  inherited Destroy;
end;

{ TJSBinary }

destructor TJSBinary.Destroy;

  procedure FreeListOfBins(El: TJSElement; ListA: boolean);
  var
    BinCnt: Integer;
    SubBin: TJSBinary;
    Bins: TJSElementArray;
  begin
    // free El binary chain without stack
    SetLength(Bins{%H-},8);
    BinCnt:=0;
    while El is TJSBinary do
      begin
      SubBin:=TJSBinary(El);
      if BinCnt=length(Bins) then
        SetLength(Bins,BinCnt*2);
      Bins[BinCnt]:=SubBin;
      inc(BinCnt);
      if ListA then
        El:=SubBin.FA
      else
        El:=SubBin.FB;
      end;
    while BinCnt>0 do
      begin
      dec(BinCnt);
      SubBin:=TJSBinary(Bins[BinCnt]);
      FreeAndNil(SubBin.FA);
      FreeAndNil(SubBin.FB);
      end;
  end;

begin
  if FA is TJSBinary then
    FreeListOfBins(FA,true);
  if FB is TJSBinary then
    FreeListOfBins(FB,false);

  FreeAndNil(FA);
  FreeAndNil(FB);
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

Destructor TJSAssignStatement.Destroy;
begin
  FreeAndNil(FLHS);
  FreeAndNil(FExpr);
  inherited Destroy;
end;

Class function TJSAssignStatement.OperatorToken: tjsToken;
begin
  Result:=tjsUnknown;
end;

Class function TJSAssignStatement.OperatorString: String;

Var
  t :  TJSToken;
begin
  T:=OperatorToken;
  if (tjsUnknown<>t) then
    Result:=TokenInfos[t]
  else
    Result:='';
end;

{ TJSVarDeclaration }

procedure TJSVarDeclaration.SetTyped(AValue: TJSTypeDef);
begin
  if FTyped=AValue then Exit;
  if FOwnsType then
    FreeAndNil(FTyped);
  FTyped:=AValue;
  FOwnsType:=aValue<>Nil;
end;

destructor TJSVarDeclaration.Destroy;
begin
  if FOwnsType then
    FreeAndNil(FTyped);
  FreeAndNil(FInit);
  inherited Destroy;
end;

procedure TJSVarDeclaration.SetForeignType(AValue: TJSTypeDef);
begin
  if FOwnsType then
    FreeAndNil(FTyped);
  FTyped:=aValue;
  FOwnsType:=False;
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

constructor TJSSwitchStatement.Create(ALine, AColumn: Integer; const ASource: String);
begin
  inherited Create(ALine, AColumn, ASource);
  FCases:=TJSCaseElements.Create(TJSCaseElement);
end;

destructor TJSSwitchStatement.Destroy;
begin
  FreeAndNil(FCases);
  FreeAndNil(FCond);
  inherited Destroy;
end;

{ TJSCaseElements }

function TJSCaseElements.GetC(AIndex : Integer): TJSCaseElement;
begin
  Result:=TJSCaseElement(Items[AIndex]);
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

constructor TJSSourceElements.Create(ALine, AColumn: Integer; const ASource: String);

  Function CN(const aName : String; DoClear : Boolean = True) : TJSElementNodes;
  begin
    Result:=TJSElementNodes.Create(TJSElementNode);
    Result.NodeType:=aName;
    Result.DoClearNodes:=DoClear;
  end;

begin
  inherited Create(ALine, AColumn, ASource);
  FClasses:=CN('Classes');
  FEnums:=CN('Enums');
  FFunctions:=CN('Functions',False);
  FInterfaces:=CN('Interfaces');
  FModules:=CN('Modules');
  FNamespaces:=CN('Namespaces');
  FStatements:=CN('Statements',False);
  FTypes:=CN('Types');
  FVars:=CN('Vars');
end;

destructor TJSSourceElements.Destroy;
begin
  // Vars, types, enums, classes, interfaces are owned by their statements, and those are freed later
  FreeAndNil(FVars);
  FreeAndNil(FTypes);
  FreeAndNil(FNamespaces);
  FreeAndNil(FModules);
  FreeAndNil(FInterfaces);
  FreeAndNil(FFunctions);
  FreeAndNil(FEnums);
  FreeAndNil(FClasses);
  // Must come last
  FreeAndNil(FStatements);
  inherited Destroy;
end;

{ TJSElementNodes }

function TJSElementNodes.GetN(AIndex : Integer): TJSElementNode;
begin
  Result:=TJSElementNode(Items[Aindex])
end;

function TJSElementNodes.GetE(AIndex : Integer): TJSElement;
begin
  Result:=Nodes[aIndex].Node;
end;

destructor TJSElementNodes.Destroy;
begin
  if FClearNodes then
    ClearNodes;
  inherited Destroy;
end;

procedure TJSElementNodes.ClearNodes;

Var
  I : Integer;

begin
  For I:=0 to Count-1 do
     begin
{     if Assigned(Nodes[i].Node) then
       begin
       Write(FNodeType,': Clearing node ',I,': ');
       WriteLn(Nodes[i].Node.ClassName)
       end
     else
       Writeln(FNodeType,': Node ',i,'is nil');}
     Nodes[i].Node:=Nil;
     end;
end;

function TJSElementNodes.GetEnumerator: TElementNodeEnumerator;
begin
  Result:=TElementNodeEnumerator.Create(Self);
end;

function TJSElementNodes.AddNode(aIsAmbient : Boolean = False; aIsExport : Boolean = False): TJSElementNode;
begin
  Result:=TJSElementNode(Add);
  Result.IsAmbient:=aIsAmbient;
  Result.IsExport:=aIsExport;
end;

function TJSElementNodes.AddNode(aEl: TJSElement; aIsAmbient: Boolean; aIsExport: Boolean): TJSElementNode;
begin
  Result:=AddNode(aIsAmbient,aIsExport);
  Result.Node:=aEl;
end;

function TJSElementNodes.InsertNode(Index: integer): TJSElementNode;
begin
  Result:=TJSElementNode(Insert(Index));
end;

{ TJSFunction }

destructor TJSFunctionStatement.Destroy;
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

procedure TJSElementNode.Assign(aSource: TPersistent);

Var
  aNode : TJSElementNode absolute aSource;

begin
  if aSource is TJSElementNode then
    begin
    FIsAmbient:=aNode.FIsAmbient;
    FIsExport:=aNode.FIsExport;
    FNode:=aNode.Node;
    end
  else
    inherited Assign(aSource);
end;

{ TJSFuncDef }

constructor TJSFuncDef.Create;
begin
  FParams:=TStringList.Create;
  FTypedParams:=TJSTypedParams.Create;
end;

destructor TJSFuncDef.Destroy;
begin
  FreeAndNil(FGenericParams);
  FreeAndNil(FTypedParams);
  FreeAndNil(FBody);
  FreeAndNil(FParams);
  FreeAndNil(FResultType);
  inherited Destroy;
end;

procedure TJSFuncDef.UpdateParams;

Var
  I : integer;

begin
  FParams.Clear;
  For I:=0 to TypedParams.Count-1 do
    FParams.Add({$ifdef FPC_HAS_CPSTRING}UTF8Encode(TypedParams.Names[i]){$ELSE}TypedParams.Names[i]{$ENDIF});
end;

{ TJSBracketMemberExpression }

destructor TJSBracketMemberExpression.Destroy;
begin
  FreeAndNil(FName);
  inherited Destroy;
end;

{ TJSLiteral }

constructor TJSLiteral.Create(ALine, AColumn: Integer; const ASource: String);
begin
  FValue:=TJSValue.Create;
  inherited Create(ALine, AColumn, ASource);
end;

destructor TJSLiteral.Destroy;
begin
  FreeAndNil(FValue);
  Inherited;
end;

end.

