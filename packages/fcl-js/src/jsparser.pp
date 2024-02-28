{ ********************************************************************* 
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2016 Michael Van Canneyt.
       
    Javascript parser
            
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
                   
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
                                
  **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit jsparser;
{$ENDIF FPC_DOTTEDUNITS}

{ $define debugparser}
{$mode objfpc}{$H+}
{$inline on}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, Js.Scanner, Js.Tree, Js.Token, Js.Base;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, jsscanner, jstree, jstoken, jsbase;
{$ENDIF FPC_DOTTEDUNITS}

Const
   SEmptyLabel = '';
   MinAsyncVersion = ecma2021;
   MinAwaitVersion = ecma2021;
   MinYieldVersion = ecma2021;
   minLetVersion = ecma2021;
   MinDebuggerVersion = ecma2021;
   MinImportVersion = ecma2021;
   MinExportVersion = ecma2021;
   MinClassVersion = ecma2021;
   MinGeneratorVersion = ecma2021;

Type
  TECMAVersion = {$IFDEF FPC_DOTTEDUNITS}Js.Scanner{$ELSE}jsscanner{$ENDIF}.TECMAVersion;
  TScopeType = (stFunction,stClass,stModule,stNamespace);
  TListType = (ltUnknown,ltTuple,ltTemplate);
  TFunctionFlag = (ffAmbient,ffConstructor,ffGenerator);
  TFunctionFlags = Set of TFunctionFlag;
  EJSParser = Class(Exception)
    ErrorRow : Integer;
    ErrorCol : integer;
    FileName : String;
  end;

  TParseTypeOption = (ptoAllowLiteral,ptoRefOnly,ptoUnion,ptoIntersection,ptoFunctionCall);
  TParseTypeOptions = set of TParseTypeOption;

  { TJSParser }

  TJSParser = Class(TObject)
  Private
    FFunctionDepth: Integer;
    FInput : TStream;
    FIsLHS: Boolean;
    FNoIn: Boolean;
    FScanner : TJSScanner;
    FPrevious,
    FCurrent : TJSToken;
    FCurrentString : {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString;
    FFreeScanner : Boolean;
    FCurrentVars : TJSElementNodes;
    FPeekToken: TJSToken;
    FPeekTokenString: {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString;
    FLabelSets,
    FCurrentLabelSet:TJSLabelSet;
    FLabels : TJSLabel;
    // these check that current token is identifier with specific value:
    // as, from, get, of, set, target
    function AddToElements(aSource: TJSSourceElements; aElement: TJSElement; aIsAmbient: Boolean=False; aIsExport: Boolean=False): Boolean;
    procedure ClassDefToMembers(aClass: TJSClassDeclaration; aClassDef: TJSObjectTypeDef);
    function CurrentTokenIsValidIdentifier: Boolean;
    function GetIsTypeScript: Boolean;
    function  IdentifierIsLiteral(const aValue : {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString) : Boolean;
    Procedure CheckIdentifierLiteral(const aValue : {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString);
    function ConsumeIdentifierLiteral(const aValue: {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString): TJSToken;
    function CheckSemiColonInsert(aToken: TJSToken; Consume: Boolean): Boolean;
    function EnterLabel(const ALabelName: {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString): TJSLabel;
    // Check that current token is aToken
    procedure Expect(aToken: TJSToken);
    // Check that current token is aToken and goto next token.
    procedure Consume(aToken: TJSToken; AllowSemicolonInsert : Boolean = False);
    procedure FreeCurrentLabelSet;
    function GetVersion: TECMAVersion;
    procedure LeaveLabel;
    function LookupLabel(const ALabelName: {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString; Kind: TJSToken): TJSLabel;
    function ParseAdditiveExpression: TJSElement;
    procedure ParseAliasElements(aElements: TJSAliasElements);
    procedure ParseAmbientClassBody(aObj: TJSObjectTypeDef);
    function ParseArguments: TJSarguments;
    function ParseArrayLiteral: TJSElement;
    procedure ParseArrowFunctionTypeDef(aDef: TJSArrowFunctionTypeDef; aFirstParam: TJSTypedParam); overload;
    function ParseArrowFunctionTypeDef(const  aArgName: {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString; aArgIsSpread: Boolean): TJSArrowFunctionTypeDef; overload;
    function ParseArrowFunctionTypeDef(aFirst: TJSObjectTypeDef; aArgIsSpread: Boolean): TJSArrowFunctionTypeDef;  overload;
    function ParseAssignmentExpression: TJSElement;
    function ParseBitwiseAndExpression: TJSElement;
    function ParseBitwiseORExpression: TJSElement;
    function ParseBitwiseXORExpression: TJSElement;
    function ParseBlock: TJSElement;
    function ParseBreakStatement: TJSElement;
    function ParseClassBody(isAmbient : Boolean = False): TJSSourceElements; inline;
    function ParseClassDeclaration(isAmbient : Boolean = False; IsAbstract : Boolean = False) : TJSClassDeclaration;
    function ParseClassStatement(isAmbient : Boolean = False; IsAbstract : Boolean = False) : TJSClassStatement;
    function ParseClassExpression: TJSClassDeclaration;
    function ParseConditionalExpression: TJSElement;
    function ParseContinueStatement: TJSElement;
    function ParseDebuggerStatement: TJSElement;
    function ParseDestructuredParam(aParams: TJSTypedParams) : TJSTypedParam;
    function ParseEmptyStatement: TJSElement;
    function ParseEnumDeclaration: TJSEnumDeclaration;
    function ParseEnumDeclarationStatement: TJSElement;
    function ParseEqualityExpression: TJSElement;
    function ParseExportStatement(IsAssign : Boolean): TJSElement;
    function ParseExpression: TJSElement;
    function ParseExpressionStatement: TJSElement;
    procedure ParseFormalParameterList(aParams : TJSTypedParams);
    function ParseFunctionDeclaration(aFlags : TFunctionFlags): TJSFunctionDeclarationStatement;
    function ParseFunctionExpression(aFlags : TFunctionFlags): TJSFunctionDeclarationStatement;
    function ParseFunctionStatement: TJSElement;
    function ParseFunctionBody: TJSFunctionBody;
    procedure ParseGenericParamList(aList: TJSElementNodes);
    function ParseIdentifier(AcceptAsIdentifier: TJSTokens=[]): {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString;
    function ParseIfStatement: TJSElement;
    function ParseImportStatement: TJSElement;
    function ParseIndexSignature: TJSObjectTypeElementDef;
    function ParseIndexSignatureStatement: TJSIndexSignatureStatement;
    function ParseInterfaceDeclaration: TJSInterfaceDeclaration;
    function ParseInterfaceDeclarationStatement: TJSInterfaceStatement;
    function ParseIterationStatement: TJSElement;
    function ParseLabeledStatement: TJSElement;
    function ParseLeftHandSideExpression: TJSElement;
    function ParseLiteral: TJSElement;
    function ParseLogicalAndExpression: TJSElement;
    function ParseLogicalORExpression: TJSElement;
    function ParseMemberExpression: TJSElement;
    function ParseModuleDeclarationStatement : TJSModuleStatement;
    function ParseModuleDeclaration : TJSModuleDeclaration;
    function ParseModuleBody: TJSSourceElements;
    function ParseMultiplicativeExpression: TJSElement;
    function ParseNamespaceDeclarationStatement : TJSNamespaceStatement;
    function ParseNamespaceDeclaration(IsAmbient: Boolean = False): TJSNamespaceDeclaration;
    function ParseNamespaceBody(IsAmbient: Boolean = False): TJSSourceElements;
    function ParseNumericLiteral: TJSElement;
    procedure ParseObjectBody(aObj: TJSObjectTypeDef);
    function ParseObjectLiteral: TJSElement;
    function ParsePostFixExpression: TJSElement;
    function ParsePrimaryExpression: TJSElement;
    function ParseRegularExpressionLiteral: TJSElement;
    function ParseRelationalExpression: TJSElement;
    function ParseReturnStatement: TJSElement;
    function ParseShiftExpression: TJSElement;
    function ParseStatement(IsAmbient : Boolean = False;IsExport : Boolean = False): TJSElement;
    function ParseStatementList: TJSElement;
    function ParseStringLiteral: TJSElement;
    function ParseSwitchStatement: TJSElement;
    function ParseThrowStatement: TJSElement;
    function ParseTryStatement: TJSElement;
    // Type parsing
    function ParseTypeSimple(aOptions: TParseTypeOptions): TJSTypeDef;
    function ParseTypeDeclaration: TJSTypeDeclaration;
    function ParseTypeDeclarationStatement : TJSElement;
    function ParseTypeDef(aOptions: TParseTypeOptions = []; StopTokens : TJSTokens = []): TJSTypeDef;
    procedure ParseTypeGuard(aType: TJSTypedef; aOptions: TParseTypeOptions; StopTokens: TJSTokens=[]);
    function ParseTypeArrayDef(aOptions: TParseTypeOptions; aSub: TJSTypeDef): TJSArrayTypeDef;
    function ParseTypeObjectDef: TJSObjectTypeDef;
    function ParseGenericType(aType: TJSTypeDef): TJSTypeDef;
    function ParseTypeTuple(aOptions: TParseTypeOptions): TJSTupleTypeDef;
    function ParseTypeParenthesised(aOptions : TParseTypeOptions): TJSTypeDef;
    procedure ParseTypeList(aList: TJSElementNodes; aTerminator: TJSToken; ListType : TListType = ltUnknown);
    function ParseTypeStatement(Elements: TJSSourceElements; ScopeType: TScopeType; aIsAmbient: Boolean): TJSElement;
    function ParseUnaryExpression: TJSElement;
    procedure ParseTypeUnionIntercept(aType: TJSUnionOrTupleTypeDef; aOptions: TParseTypeOptions; aSeparator: TJSToken);
    function ParseVariableDeclaration(aVarType: TJSVarType=vtVar; IsAmbient: Boolean=False; IsExport: Boolean=False): TJSElement;
    function ParseVariableDeclarationList(aVarType : TJSVarType = vtVar; IsAmbient: Boolean = False; IsExport: Boolean = False): TJSElement;
    function ParseVariableStatement(aVarType : TJSVarType = vtVar;IsAmbient : Boolean = False; IsExport : Boolean = False): TJSElement;
    function ParseWithStatement: TJSElement;
  Protected
    Procedure CheckParser;
    Function CurrentLabelSet : TJSLabelSet;
    function CurSource: String;
    Function CurLine : Integer;
    Function CurPos : Integer;
    Function CreateElement(AElementClass : TJSElementClass)  : TJSElement; virtual;
    Procedure FreeElement(aElement : TJSElement); virtual;
    Procedure FreeElement(aElements : TJSElementNodes); virtual;
    Procedure Error(const Msg : String);
    Procedure Error(const Fmt : String; Args : Array of const);
    // Parse functions
    Function IsIdentifier(const aIdentifier : {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString) : Boolean; inline;
    function ParseSourceElements(ScopeType : TScopeType = stFunction; ParentisAmbient : Boolean = False): TJSSourceElements;
    Property FunctionDepth : Integer Read FFunctionDepth Write FFunctionDepth;
    Property NoIn : Boolean Read FNoIn Write FNoIn;
    Property IsLHS : Boolean Read FIsLHS Write FIsLHS;
  Public
    Constructor Create(AInput: TStream; aVersion : TECMAVersion = ecma5; aIsTypeScript : Boolean = false; const aFileName : String = '');
    // Scanner has version
    Constructor Create(AScanner : TJSScanner);
    Destructor Destroy; override;
    Function Parse : TJSElement;
    Function ParseProgram : TJSFunctionDeclarationStatement;
    Function CurrentToken : TJSToken;
    Function CurrentTokenString : {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString;
    Function GetNextToken : TJSToken;
    Function PeekNextToken : TJSToken;
    Function IsEndOfLine : Boolean;
    Property ECMAVersion : TECMAVersion Read GetVersion;
    Property IsTypeScript : Boolean Read GetIsTypeScript;
    Property Scanner : TJSScanner Read FScanner;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.TypInfo;
{$ELSE FPC_DOTTEDUNITS}
uses typinfo;
{$ENDIF FPC_DOTTEDUNITS}

Resourcestring
  SErrUnmatchedCurlyBrace    = 'Unmatched }';
  SErrUnmatchedSquareBrace   = 'Unmatched ]';
  SErrUnmatchedBrace         = 'Unmatched )';
  SErrUnexpectedToken        = 'Unexpected token: ''%s''';
  SErrTokenMismatch          = 'Unexpected token: ''%s'', expected: ''%s''';
  SErrSemicolonOrInExpected  = 'Unexpected token: ''%s'', expected ; or ''in''';
  SErrSemicolonExpected      = 'Unexpected token: ''%s'', expected ;';
  SErrDuplicateLabelName     = 'Duplicate label name: ''%s''';
  SErrLabelNotContinuable    = 'Label ''%s'' is not suitable for continue.';
  SErrLabelNOtDefinedOrReachable = 'Label ''%s'' is not defined or not reachable.';
  SErrContinueNotInLoop      = 'Continue statement not in loop';
  SErrBreakNotInLoop         = 'Break statement not in loop';
  SErrReturnNotInFunction    = 'return statement not in a function body';
  SErrCaseEndExpected        = 'Unexpected token: Expected }, case or default clause';
  SErrDuplicateSwitchDefault = 'Duplicate default clause for switch statement';
  SErrNewlineAfterThrow      = 'Newline after throw not allowed';
  SErrCatchFinallyExpected   = 'Unexpected token: Expected ''catch'' or ''finally''';
  SErrArgumentsExpected      = 'Unexpected token: Expected '','' or '')'', got %s';
  SErrArrayEnd               = 'Unexpected token: Expected '','' or '']'', got %s';
  SErrExpectedColonBrace     = 'Unexpected token: Expected '':'' or ''('', got %s';
  //SErrObjectEnd              = 'Unexpected token: Expected '','' or ''}'', got %s';
  SErrObjectElement          = 'Unexpected token: Expected string, identifier or number after '','' got: %s';
  SErrLiteralExpected        = 'Unexpected token: Expected: null, true, false, number, string, or regex, got: %s';
  SErrInvalidnumber          = 'Invalid numerical value: %s';
  SErrInvalidRegularExpression = 'Invalid regular expression: %s';
  SErrFunctionNotAllowedHere = 'function keyword not allowed here';
  SErrExpectedButFound       = 'Unexpected token. Expected "%s" but found "%s"';
  SErrExpectedMulOrCurlyBrace = 'Unexpected token: Expected * or {, got: %s';
  SErrExpectedMulOrCurlyBraceOrDefault = 'Unexpected token: Expected * or { or default , got: %s';
  SErrTypeExpected = 'Type expected, but found: "%s"';
{
  SErrUnionWithoutType = 'Union type without type name';
  SErrIntersectionWithoutType = 'Intersection type without type name';
  SErrArrayWithoutType = 'Array type without type name';
  SErrGenericWithoutType = 'Generic type without base type';
  SErrGenericArray1Element = 'Generic array type can have only 1 element';
}
  SErrorInTypeDef = 'Error in type definition. Expected %s';
  SArrowFunction = 'Arrow function';
  SBraceClose = 'Closing brace ")"';
  SErrInvalidIndexSignature = 'Invalid index signature';
  SErrExtendsNeedsTypeName = 'extends can only be used after type reference';
  SErrObjConstMustBeStaticReadonly = 'Const must be static & readonly';
  SErrInvalidTupleLabel = 'Invalid tuple label: got %s type';

{ TJSScanner }

Function TJSParser.CurrentToken: TJSToken;

begin
  Result:=FCurrent;
end;

Function TJSParser.CurrentTokenString: {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString;
begin
  Result:=FCurrentString;
end;

Function TJSParser.GetNextToken: TJSToken;
begin
  FPrevious:=FCurrent;
  If (FPeekToken<>tjsunknown) then
     begin
     FCurrent:=FPeekToken;
     FCurrentString:=FPeekTokenString;
     FPeekToken:=tjsUnknown;
     FPeekTokenString:='';
     end
  else
    begin
    FCurrent:=FScanner.FetchToken;
    FCurrentString:={$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString(FScanner.CurTokenString);
    if (FCurrentString='') then
       FCurrentString:={$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString(TokenInfos[FCurrent]);
    end;
  Result:=FCurrent;
  {$ifdef debugparser}Writeln('GetNextToken (',FScanner.CurLine,',',FScanner.CurColumn,'): ',GetEnumName(TypeInfo(TJSToken),Ord(FCurrent)), ' As string: ',FCurrentString);{$endif debugparser}
end;

Function TJSParser.PeekNextToken: TJSToken;
begin
  If (FPeekToken=tjsUnknown) then
    begin
    FPeekToken:=FScanner.FetchToken;
    FPeekTokenString:={$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString(FScanner.CurTokenString);
    end;
  {$ifdef debugparser}Writeln('PeekNextToken : ',GetEnumName(TypeInfo(TJSToken),Ord(FPeekToken)), ' As string: ',FPeekTokenString);{$endif debugparser}
  Result:=FPeekToken;
end;

Function TJSParser.IsEndOfLine: Boolean;
begin
  Result:=FScanner.IsEndOfLine;
end;


Function TJSParser.CurPos: Integer;
begin
  If Assigned(FScanner) then
    Result:=FScanner.CurColumn
  else
    Result:=0;
end;

Function TJSParser.CurLine: Integer;
begin
  If Assigned(FScanner) then
    Result:=FScanner.CurRow
  else
    Result:=0;
end;

function TJSParser.CurSource: String;
begin
  If Assigned(FScanner) then
    Result:=FScanner.CurFileName
  else
    Result:='';
end;

Procedure TJSParser.CheckParser;
begin

end;

procedure TJSParser.LeaveLabel;

Var
  L : TJSLabel;

begin
  L:=FLabels;
  FLabels:=FLabels.Next;
  L.Free; // ??
end;

function TJSParser.LookupLabel(const ALabelName: {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString; Kind: TJSToken): TJSLabel;

Var
  L : TJSLabel;

begin
  L:=FLabels;
  Result:=Nil;
  While (L<>Nil) and (Result=Nil) do
    begin
    If (L.Name=ALabelName) then
      begin
      if (kind=tjsContinue) and (Not L.LabelSet.Continuable) and (ALabelName<>SEmptyLabel) then
        Error(SErrLabelNotContinuable,[ALabelName]);
      Result:=L;
      end;
    L:=L.Next;
    end;
  If (Result=Nil) then
    begin
    If (ALabelName<>'') then
      Error(SErrLabelNOtDefinedOrReachable,[ALabelName])
    else if kind=tjsCOntinue then
      Error(SErrContinueNotInLoop)
    else
      Error(SErrBreakNotInLoop);
    end;
end;

function TJSParser.EnterLabel(const ALabelName: {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString): TJSLabel;

Var
  L : TJSLabel;

begin
  If (ALAbelName<>SEmptyLabel) then
    begin
    L:=FLabels;
    While (L<>Nil) do
      begin
      If (L.Name=ALabelName) then
        Error(SErrDuplicateLabelName,[ALabelName]);
      L:=L.Next;
      end;
    end;
  L:=TJSLabel.Create;
  L.Name:=ALabelName;
  L.LabelSet:=CurrentLabelSet;
  L.LocationSource:=Self.CurSource;
  L.LocationLine:=CurLine;
  L.LocationPos:=CurPos;
  L.Next:=FLabels;
  FLabels:=L;
  Result:=L;
end;

Function TJSParser.CurrentLabelSet: TJSLabelSet;

Var
  LS : TJSLabelSet;

begin
  If (FCurrentLabelSet=Nil) then
    begin
    LS:=TJSLabelSet.Create;
    If (FLabelSets=Nil) then
      LS.Target:=1
    else
      LS.Target:=FLabelSets.Target;
    LS.Next:=FLabelSets;
    FLabelSets:=LS;
    FCurrentLabelSet:=LS;
    end;
  Result:=FCurrentLabelSet;
end;

Function TJSParser.CreateElement(AElementClass: TJSElementClass): TJSElement;
begin
  Result:=AElementClass.Create(CurLine,CurPos,CurSource);
end;

procedure TJSParser.FreeElement(aElement: TJSElement);
begin
  aElement.Free;
end;

procedure TJSParser.FreeElement(aElements: TJSElementNodes);

Var
  I : integer;

begin
  For I:=0 to aElements.Count-1 do
    FreeElement(aElements[i].Node);
  aElements.DoClearNodes:=True;
  aElements.Free;
end;

Procedure TJSParser.Error(const Msg: String);

Var
  ErrAt : String;
  E : EJSParser;

begin
  If Assigned(FScanner) then
    If FScanner.CurFilename<>'' then
      ErrAt:=Format('Error: file "%s" line %d, pos %d: ',[FScanner.CurFileName,FScanner.CurRow,FScanner.CurColumn])
    else
      ErrAt:=Format('Error: line %d, pos %d: ',[FScanner.Currow,FScanner.CurColumn]);
  E:=EJSParser.Create(ErrAt+Msg);
  E.ErrorCol:=FScanner.CurColumn;
  E.ErrorRow:=FScanner.CurRow;
  E.FileName:=FScanner.CurFileName;
  Raise E;
end;

Procedure TJSParser.Error(const Fmt: String; Args: Array of const);
begin
  Error(Format(Fmt,Args));
end;

function TJSParser.IsIdentifier(const aIdentifier: {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString): Boolean;
begin
  Result:=IdentifierIsLiteral(aIdentifier);
end;

constructor TJSParser.Create(AInput: TStream; aVersion: TECMAVersion; aIsTypeScript: Boolean; const aFileName : String = '');
begin
  FInput:=AInput;
  FCurrent:=TJSUnknown;
  FScanner:=TJSScanner.Create(FInput,aVersion,aFileName);
  FScanner.IsTypeScript:=aIsTypeScript;
  FFreeScanner:=True;
end;

Constructor TJSParser.Create(AScanner: TJSScanner);
begin
  FCurrent:=TJSUnknown;
  FScanner:=AScanner;
  FFreeScanner:=False;
end;

Destructor TJSParser.Destroy;
begin
  if FFreeScanner then
    FreeAndNil(FScanner);
  inherited;
end;



procedure TJSParser.Expect(aToken: TJSToken);

begin
  {$ifdef debugparser}  Writeln('Expecting : ',GetEnumName(TypeInfo(TJSToken),Ord(AToken)), ' As string: ',TokenInfos[AToken]);{$endif debugparser}
  If Not CheckSemiColonInsert(AToken,False) then
    if (CurrentToken<>aToken) then
      Error(SerrTokenMismatch,[CurrenttokenString,TokenInfos[aToken]]);
end;

function TJSParser.IdentifierIsLiteral(const aValue: {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString): Boolean;
begin
  Result:=(CurrentToken=tjsIdentifier) and (CurrentTokenString=aValue);
end;


function TJSParser.GetIsTypeScript: Boolean;
begin
  Result:=FScanner.IsTypeScript;
end;

procedure TJSParser.CheckIdentifierLiteral(const aValue: {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString);
begin
  if Not IdentifierIsLiteral(aValue) then
    Error(SErrExpectedButFound,[aValue,CurrentTokenString]);
end;

function TJSParser.ConsumeIdentifierLiteral(const aValue: {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString): TJSToken;
begin
  CheckidentifierLiteral(aValue);
  Result:=GetNextToken;
end;

function TJSParser.CheckSemiColonInsert(aToken : TJSToken; Consume : Boolean) : Boolean;

begin
  Result:=(AToken=tjsSemiColon);
  If Result then
    begin
    Result:=(CurrentToken=tjsCurlyBraceClose) or (FScanner.WasEndOfLine) or (CurrentToken=tjsEOF);
    If Result and Consume then
      FPrevious:=tjsSemiColon;
    end;
end;

procedure TJSParser.Consume(aToken: TJSToken; AllowSemicolonInsert: Boolean);
begin
  {$ifdef debugparser}  Writeln('Consuming : ',GetEnumName(TypeInfo(TJSToken),Ord(AToken)), ' As string: ',TokenInfos[AToken]);{$endif debugparser}
  Expect(aToken);
  If not (AllowSemiColonInsert and CheckSemiColonInsert(aToken,True)) then
    GetNextToken;
end;

function TJSParser.ParseIdentifier (AcceptAsIdentifier : TJSTokens = []): {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString;
{ On entry, on identifier,
  on exit, after identifier
}


begin
  Result:='';
  if (AcceptAsIdentifier<>[]) and (CurrentToken in AcceptAsIdentifier) then
    Result:=CurrentTokenString
  else
    begin
    Expect(tjsIdentifier);
    Result:=CurrentTokenString;
    end;
  GetNextToken;
  While (CurrentToken=tjsDot) do
    begin
    GetNextToken;
    Expect(tjsIdentifier);
    Result:=Result+'.'+CurrentTokenString;
    GetNextToken;
    end;
//  until (CurrentToken<>tkDot) or FScanner.WasEndOfLine;
end;

procedure TJSParser.ParseGenericParamList(aList : TJSElementNodes);
// On entry, we're on <
// On exit, we're on >

Var
  PrevRShift : Boolean;
begin
  PrevRShift:=FScanner.DisableRShift;
  Try
    FScanner.DisableRShift:=True;
    ParseTypeList(aList,{$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsGT,ltTemplate);
  Finally
    FScanner.DisableRShift:=PrevRShift;
  end;
end;

function TJSParser.ParseTypeObjectDef: TJSObjectTypeDef;
// on entry, on {
// on exit, on }
Var
  N : TJSObjectTypeDef;

begin
  N:=TJSObjectTypeDef(CreateElement(TJSObjectTypeDef));
  try
    Result:=N;
    ParseObjectBody(N);
    Consume(tjsCurlyBraceClose);
  except
    FreeElement(Result);
    Raise;
  end;
end;

Function TJSParser.ParseIndexSignature : TJSObjectTypeElementDef;

// On entry, we're on [
// On exit, we're on ;

Var
  SigDecl : TJSIndexSignatureDeclaration;

begin
  SigDecl:=TJSIndexSignatureDeclaration(CreateElement(TJSIndexSignatureDeclaration));
  try
    Result:=SigDecl;
    Consume(tjsSQuaredBraceOpen);
    if (CurrentToken in [TJsToken.tjsNumber,TjsToken.tjsString]) then
      begin
      SigDecl.IndexName:=CurrentTokenString;
      GetNextToken
      end
    else
      SigDecl.IndexName:=ParseIdentifier;
    case CurrentToken of
      tjsColon :
        begin
        consume(tjsColon);
        expect(tjsIdentifier);
        if not (IsIdentifier('number') or IsIdentifier('string')) then
          Error(SErrExpectedButFound,['number or string',CurrentTokenString]);
        SigDecl.IndexType:=CurrentTokenString;
        consume(tjsIdentifier);
        end;
      tjsIn :
        begin
        consume(tjsIn);
        SigDecl.InIndexType:=ParseTypeDef([ptoAllowLiteral]);
        end;
    else
      if (CurrentToken<>tjsSQuaredBraceClose) then
        Error(SErrInvalidIndexSignature);
    end;
    Consume(tjsSQuaredBraceClose);
    SigDecl.IsFunction:=(CurrentToken=tjsBraceOpen);
    if SigDecl.IsFunction then
      begin
      GetNextToken;
      Consume(tjsBraceClose);
      end;
    if (CurrentToken in [tjsMINUS,tjsPlus]) and (PeekNextToken=tjsConditional) then
      begin
      if (CurrentToken=tjsMINUS) then
        SigDecl.Optional:=koDisableOptional
      else
        SigDecl.Optional:=koForceOptional;
      GetNextToken; // put on conditional
      end
    else if CurrentToken=tjsConditional then
      SigDecl.Optional:=koOptional;
    // Move to after conditional
    if SigDecl.Optional<>koDefault then
      GetNextToken;
    consume(tjsColon);
    SigDecl.ElementType:=ParseTypeDef([ptoAllowLiteral]);
    if (not (CurrentToken in [tjsComma,tjsSemiColon])) or FScanner.WasEndOfLine then
      Expect(tjsSEMICOLON);
  except
    FreeElement(SigDecl);
    Raise;
  end;
end;

function TJSParser.ParseIndexSignatureStatement: TJSIndexSignatureStatement;
begin
  Result:=TJSIndexSignatureStatement(CreateElement(TJSIndexSignatureStatement));
  try
    Result.Decl:=ParseIndexSignature as TJSIndexSignatureDeclaration;
  except
    FreeElement(Result);
    Raise;
  end;
end;

Function TJSParser.CurrentTokenIsValidIdentifier : Boolean;

Const
   Valididents = [tjsIdentifier,{$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsString,{$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsnumber,tjsDelete,
                  tjsWith,tjsThrow,tjsDefault,tjsAwait,tjscase,tjsdebugger,tjsCatch,
                  tjsextends,tjsexport,tjsImport,tjsEnum,tjsClass,tjsFor,tjsReturn,
                  tjsDo,tjsFinally,tjsWhile,tjsIf,tjsYield,tjsvoid,tjsbreak, 
                  tjscontinue, tjsswitch, tjstrue, tjsNull, tjsThis,tjsFalse,
                  tjsVar,tjsConst, tjsin, tjsInstanceOf, tjsfunction, tjstry, 
                  tjselse, tjsSuper, tjsLet, tjsTypeOf, tjsNew];

begin
  Result:=CurrentToken in ValidIdents;
end;


Procedure TJSParser.ParseObjectBody(aObj: TJSObjectTypeDef);

// On entry on {
// On exit on }

  Function CreateAny : TJSTypeReference;

  begin
    Result:=TJSTypeReference(CreateElement(TJSTypeReference));
    Result.Name:='any';
  end;


var
  aName : {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString;
  IsMinus, isReadOnly, isOptional : Boolean;
  E : TJSObjectTypeElementDef;
  F : TJSMethodDeclaration ;
  FS : TJSFunctionDeclarationStatement;
  TP : TJSElementNodes;

begin
  Consume(tjsCurlyBraceOpen);
  While (CurrentToken<>tjsCurlyBraceClose) do
    begin
    aName:='';
    E:=Nil;
    While CurrentToken=tjsComma do
       GetNextToken;
    isMinus:=(CurrentToken=tjsMinus);
    if IsMinus then 
      GetNextToken;   
    isReadOnly:=IsIdentifier('readonly') and not (PeekNextToken in [tjsColon,tjsBraceOpen, tjsConditional]);
    if IsReadonly then
      GetNextToken
    else if IsMinus then
      Error(SErrExpectedColonBrace,[CurrentTokenString]);  
    If CurrentTokenIsValidIdentifier then
      begin
      aName:=CurrentTokenString;
      GetNextToken;
      end
    else if (Not (CurrentToken in [tjsLT,tjsBraceOpen,tjsSQuaredBraceOpen,tjsNew])) then
       Error(SErrObjectElement,[CurrentTokenString]);
    isOptional:=(CurrentToken=tjsConditional);
    if isOPtional then
      GetNextToken;
    case CurrentToken of
      tjsSQuaredBraceOpen:
        begin
        // [a:type] : Type
        E:=ParseIndexSignature;
        end;
      // PropertyName : PropertyType
      tjsColon:
        begin
        Consume(tjsColon);
        E:=TJSPropertyDeclaration(CreateElement(TJSPropertyDeclaration));
        E.Name:=aName;
        E.ElementType:=ParseTypeDef([ptoAllowLiteral],[tjsCurlyBraceClose]);
        end;
      // <T1> () : TypeName;
      // () : TypeName;
      tjsNew,
      tjsLT,
      tjsBraceOpen:
        begin
        if CurrentToken=tjsNew then
          begin
          aName:='new';
          GetNextToken;
          end;
        TP:=Nil;
        F:=TJSMethodDeclaration(CreateElement(TJSMethodDeclaration));
        F.Name:=aName;
        E:=F;
        if CurrentToken=tjsLT then
          begin
          TP:=TJSElementNodes.Create(TJSElementNode);
          ParseGenericParamList(TP);
          consume(tjsGT);
          end;
        FS:=ParseFunctionExpression([ffGenerator]);
        F.FuncDef:=FS.AFunction;
        FS.AFunction:=Nil;
        F.TypeParams:=TP;
        if Not Assigned(F.Funcdef.ResultType) then
          F.Funcdef.ResultType:=CreateAny;
        FreeElement(FS);
        end;
      tjsComma,tjsSemicolon,tjsCurlyBraceClose:
        begin
        // PropertyName ; Type is any
        E:=TJSPropertyDeclaration(CreateElement(TJSPropertyDeclaration));
        E.Name:=aName;
        E.ElementType:=CreateAny;
        end;
    else
      Error(SErrExpectedColonBrace,[CurrentTokenString]);
    end;
    if Assigned(E) then
      begin
      if IsOptional then
        E.Optional:=koOptional;
      E.IsReadOnly:=isReadOnly;
      aObj.AddElement(E);
      end;
    While CurrentToken in [tjsComma,tjsSemicolon] do
       GetNextToken;
    end;
  Expect(tjsCurlyBraceClose);
end;



procedure TJSParser.ParseTypeList(aList : TJSElementNodes; aTerminator : TJSToken; ListType: TListType = ltUnknown);

// Entry on < or [, exit on terminator

Var
  aSub,aType : TJSTypeDef;
  aParam : TJSNamedParamTypeDef;
  IsSpread,IsExtends : Boolean;
  PTO : TParseTypeOptions;


begin
  PTO:=[];
  if ListType in [ltTuple,ltTemplate] then
    Include(PTO,ptoAllowLiteral);
  GetNextToken;
  While (CurrentToken<>aTerminator) do
    begin
    IsSpread:=(CurrentToken=tjsEllipsis);
    if IsSpread then
      GetNextToken;
    IsExtends:=(CurrentToken=tjsExtends);
    if IsExtends then
      GetNextToken;
    aType:=Nil;
    aSub:=ParseTypeDef(PTO,[tjsGT,tjsAssign]+[aTerminator]);
    try
      aSub.IsExtends:=isExtends;
      aSub.IsSpread:=isSpread;
      if (ListType=ltTemplate) and (CurrentToken=tjsAssign) then
        begin
        GetNextToken;
        aType:=ParseTypeDef([ptoAllowLiteral]);
        aParam:=TJSNamedParamTypeDef(CreateElement(TJSNamedParamTypeDef));
        aParam.ParamName:=aSub;
        aParam.ParamType:=aType;
        aType:=Nil;
        aSub:=aParam;
        aList.AddNode.Node:=aSub;
        end
      else
        begin
        if (listType=ltTuple) then
          begin
          if (CurrentToken=tjsConditional) then
            GetNextToken
          else if (CurrentToken=tjsColon) then
            // Labeled tuple [a:type];
            begin
            if not (aSub is TJSTypeReference) then
              Error(SErrInvalidTupleLabel,[aSub.ClassName]);
            Consume(tjsColon);
            aParam:=TJSNamedParamTypeDef(CreateElement(TJSNamedParamTypeDef));
            aParam.ParamName:=aSub;
            aSub:=aParam;
            aParam.ParamType:=ParseTypeDef([ptoAllowLiteral]);
            end
          end;
        aList.AddNode.Node:=aSub;
        end;
    except
      FreeElement(aSub);
      FreeElement(aType);
      Raise;
    end;
    if CurrentToken=tjsComma then
      GetNextToken;
    end;
end;

function TJSParser.ParseArrowFunctionTypeDef(const aArgName: {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString; aArgIsSpread: Boolean): TJSArrowFunctionTypeDef;

var
  P : TJSTypedParam;

begin
  Result:=TJSArrowFunctionTypeDef(CreateElement(TJSArrowFunctionTypeDef));
  try
    P:=nil;
    if (AArgName<>'') then
      begin
      P:=Result.aFunction.TypedParams.AddParam(aArgName);
      P.IsSpread:=aArgIsSpread;
      end;
    ParseArrowFunctionTypeDef(Result,P);
  except
    FreeElement(Result);
    Raise;
  end;
end;



Function TJSParser.ParseArrowFunctionTypeDef (aFirst : TJSObjectTypeDef; aArgIsSpread: Boolean) : TJSArrowFunctionTypeDef;

var
  P : TJSTypedParam;

begin
  Result:=TJSArrowFunctionTypeDef(CreateElement(TJSArrowFunctionTypeDef));
  try
    P:=Result.aFunction.TypedParams.AddParam('');
    P.Destructured:=aFirst;
    P.IsSpread:=aArgIsSpread;
    ParseArrowFunctionTypeDef(Result,P);
  except
    FreeElement(Result);
    Raise
  end
end;

Procedure TJSParser.ParseArrowFunctionTypeDef(aDef : TJSArrowFunctionTypeDef; aFirstParam : TJSTypedParam);


// On entry, we are on
// )  of a () => Type
// ? or : from the first argument in a (argname : ) => Type
// On exit: first token after the result type

  Procedure ParseArgType(P : TJSTypedParam; aIsSpread : Boolean);
  // On entry we are on : or ?
  // On exit we are on , or )

  Var
    IsConditional : Boolean;

  begin
    IsConditional:=CurrentToken=tjsConditional;
    if IsConditional then
      GetNextToken;
    Consume(tjsColon);
    With P do
      begin
      Node:=ParseTypeDef([ptoAllowLiteral]);
      IsOptional:=IsConditional;
      IsSpread:=aIsSpread;
      end;
  end;

Var
  P : TJSTypedParam;
  aName : {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString;
  IsSpread : Boolean;

begin
  If aFirstParam <> nil then
    ParseArgType(aFirstParam,aFirstParam.IsSpread);
  While (CurrentToken<>tjsBraceClose) do
    begin
    if CurrentToken=tjsComma then
      Consume(tjscomma);
    if CurrentToken<>tjsBraceClose then
      begin
      isSpread:=(CurrentToken=tjsEllipsis);
      if IsSpread then
        Consume(tjsEllipsis);
      if (CurrentToken=tjsCurlyBraceOpen) then
        begin
        P:=ParseDestructuredParam(aDef.aFunction.TypedParams);
        consume(tjsCurlyBraceClose);
        end
      else
        begin
        aName:=ParseIdentifier;
        P:=aDef.aFunction.TypedParams.AddParam(aName);
        end;
      ParseArgType(P,IsSpread);
      end;
    end;
  Consume(tjsBraceClose);
  Consume(tjsArrow);
  aDef.aFunction.ResultType:=ParseTypeDef([ptoAllowLiteral]);
end;


Function TJSParser.ParseTypeParenthesised (aOptions : TParseTypeOptions): TJSTypeDef;

// On entry, we're on (
// On exit we're after the type:  ) or arrow function return type.

Var
  aDef : TJSTypeDef;
  aArgName : {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString;
  isSpread : Boolean;

begin
  Result:=nil;
  Consume(tjsBraceOpen);
  if CurrentToken=tjsBraceClose then
    begin
    // () => Type
    Result:=ParseArrowFunctionTypeDef('',False);
    end
  else
    begin
    // (...Ident : ) => Type
    isSpread:=(CurrentToken=tjsEllipsis);
    // Actually we know it is a function at this point
    if IsSpread then
      Consume(tjsEllipsis);
    aDef:=ParseTypeDef(aOptions - [ptoUnion,ptoIntersection],[tjsColon,tjsConditional]);
    // (Ident : ) => Type
    if (CurrentToken in [tjsColon,tjsConditional]) then
      begin
      if (aDef is TJSTypeReference) then
        begin
        aArgName:=TJSTypeReference(aDef).Name;
        FreeElement(aDef);
        Result:=ParseArrowFunctionTypeDef(aArgName,IsSpread );
        end
      else if (aDef is TJSObjectTypeDef) then
        Result:=ParseArrowFunctionTypeDef(TJSObjectTypeDef(aDef),IsSpread)
      else
        Error(SErrorInTypeDef,[SArrowFunction]);
      end
    // (Type)
    else if (CurrentToken=tjsBraceClose) then
      begin
      Result:=aDef;
      Consume(tjsBraceClose);
      end
    else
      Error(SErrorInTypeDef,[SBraceClose])
    end;
end;

function TJSParser.ParseTypeSimple(aOptions : TParseTypeOptions): TJSTypeDef;

// On entry, first token of type identifier, or ( or { .
// On exit, first token after type or ) or }

  Function ParseRef : TJSTypeReference;

  Var
    NeedNext : Boolean;
    aName : {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString;

  begin
    Result:=TJSTypeReference(CreateElement(TJSTypeReference));
    try
{      Result.IsTypeOf:=(CurrentToken=tjsTYPEOF);
      if Result.IsTypeof then
        GetNextToken;}
      NeedNext:=True;
      Case CurrentToken of
        tjsVoid : aName:='void';
        tjsThis : aName:='this';
        tjsNull : aName:='null';
        tjsNew : aName:='new';
      else
        aName:=ParseIdentifier;
        needNext:=False;
      end;
      Result.Name:=aName;
      if NeedNext then
        GetNextToken;
    except
      FreeElement(Result);
      Raise;
    end;
  end;

Var
  CurrT : TJSToken;
  isInferred,IsReadOnly,IsTypeOf,IsKeyOf : Boolean;
  aName : {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString;

begin
  Result:=Nil;
  try
    isInferred:=IsIdentifier('infer');
    if isInferred then
      GetNextToken;
    IsKeyOf:=IsIdentifier('keyof');
    if IsKeyOf then
      GetNextToken;
    IsReadOnly:=IsIdentifier('readonly');
    if IsReadOnly then
      GetNextToken;
    CurrT:=CurrentToken;
    IsTypeOf:=(CurrT=tjsTypeof);
    if IsTypeOf then
      begin
      GetNextToken;
      CurrT:=CurrentToken;
      end;
    Case CurrT of
      tjsImport :
        begin
        Consume(tjsImport);
        Consume(tjsBraceOpen);
        Expect(TjsToken.tjsString);
        Result:=TJSImportTypeRef(CreateElement(TJSImportTypeRef));
        TJSImportTypeRef(Result).FileName:=CurrentTokenString;
        getNextToken;
        Consume(tjsBraceClose);
        if CurrentToken=tjsDot then
          begin
          GetNextToken;
          TJSImportTypeRef(Result).Name:=ParseIdentifier([tjsDefault]);
          end;
        end;
      tjsVoid,tjsThis,tjsNull,tjsIdentifier:
        begin
        Result:=ParseRef;
        if (ptoFunctionCall in aOptions) then
          if CurrentToken=tjsBraceOpen then
            begin
            aName:=TJSTypeReference(Result).Name;
            FreeElement(Result);
            Result:=TJSTypeFuncCall(CreateElement(TJSTypeFuncCall));
            TJSTypeFuncCall(Result).Name:=aName;
            TJSTypeFuncCall(Result).ArgType:=ParseTypeParenthesised(aOptions);
            end;
        end;
      tjsTrue,tjsFalse,TjsToken.tjsNumber,TjsToken.tjsString :
        if (ptoAllowLiteral in aOptions) then
          begin
          Result:=TJSFixedValueReference(CreateElement(TJSFixedValueReference));
          TJSFixedValueReference(Result).FixedValue:=ParseLiteral as TJSLiteral;
          end;
      tjsNew:
        if PeekNextToken=tjsBraceOpen then
          begin
          GetNextToken;
          Result:=ParseTypeParenthesised(aOptions)
          end
        else
          Result:=ParseRef;
      tjsBraceOpen:
        Result:=ParseTypeParenthesised(aOptions);
      tjsCurlyBraceOpen:
        Result:=ParseTypeObjectDef;
    else
      Result:=Nil;
    end;
    if Assigned(Result) then
      begin
      Result.IsKeyOf:=IsKeyof;
      Result.IsTypeOf:=IsTypeof;
      Result.IsReadOnly:=IsReadonly;
      Result.IsInferred:=IsInferred;
      end;
  except
    FreeElement(Result);
    Raise;
  end
end;

Function TJSParser.ParseTypeArrayDef(aOptions : TParseTypeOptions; aSub : TJSTypeDef) : TJSArrayTypeDef;

// On entry, on [
// On exit, after ]

begin
  if aOptions=[] then ;
  Result:=TJSArrayTypeDef(CreateElement(TJSArrayTypeDef));
  Result.BaseType:=aSub;
  Consume(tjsSQuaredBraceOpen);
  if (CurrentToken<>tjsSQuaredBraceClose) then
    Result.IndexType:=ParseTypeDef([ptoAllowLiteral],[tjsSQuaredBraceClose]);
  Consume(tjsSQuaredBraceClose);
end;


Procedure TJSParser.ParseTypeUnionIntercept(aType : TJSUnionOrTupleTypeDef; aOptions : TParseTypeOptions; aSeparator : TJSToken);
{
  // On entry, we're on the | or &
  // On exit, we're after the last type
}
Var
  aUnionOptions : TParseTypeOptions;

begin
  aUnionOptions:=([ptoAllowLiteral] * aOptions);
  if aSeparator=tjsOR then
    Include(aUnionOptions,ptoUnion)
  else
    Include(aUnionOptions,ptoIntersection);
  While (CurrentToken=aSeparator) do
    begin
    Consume(aSeparator);
{$IFDEF debugParser} Writeln('Union, remains : ',FScanner.CurLine);{$endif}
    aType.AddValue(ParseTypeDef(aUnionOptions,[aSeparator]));
    end;
end;

Function TJSParser.ParseTypeTuple(aOptions : TParseTypeOptions) : TJSTupleTypeDef;
{
  On entry, on [
  on exit, after ]
}


begin
  if aOptions=[] then ;;
  Result:=TJSTupleTypeDef(CreateElement(TJSTupleTypeDef));
  ParseTypeList(Result.Values,tjsSQuaredBraceClose,ltTuple);
  Consume(tjsSQuaredBraceClose);
end;

function TJSParser.ParseGenericType(aType : TJSTypeDef): TJSTypeDef;
{
  On Entry : on <
  On Exit : after >
}
Var
  RShift: Boolean;

begin
  Result:=Nil;
  try
    if (aType is TJSTypeReference) and ((TJSTypeReference(aType).Name='Array') or (TJSTypeReference(aType).Name='ReadonlyArray')) then
      begin
      FreeElement(AType);
      Result:=TJSArrayTypeDef(CreateElement(TJSArrayTypeDef));
      consume(tjsLT);
      RShift:=FScanner.DisableRShift;
      FScanner.DisableRShift:=True;
      try
        TJSArrayTypeDef(Result).BaseType:=ParseTypeDef([ptoALlowLiteral],[tjsGT]);
      Finally
        FScanner.DisableRShift:=RShift;
      end;
      consume(tjsGT);
      end
    else
      begin
      Result:=TJSGenericTypeRef(CreateElement(TJSGenericTypeRef));
      TJSGenericTypeRef(Result).BaseType:=aType;
      ParseGenericParamList(TJSGenericTypeRef(Result).Values);
      consume(tjsGT);
      end;
  except
    FreeElement(Result);
    Raise;
  end;
end;

Procedure TJSParser.ParseTypeGuard(aType : TJSTypedef; aOptions: TParseTypeOptions; StopTokens : TJSTokens = []);
{
  on entry, on is or extends
  at exit, on first token after guard
}
begin
  if aType=Nil  then
    Error(SErrExtendsNeedsTypeName);
  if IsIdentifier('is') then
    begin
    GetNextToken;
    aType.TypeGuard:=ParseTypeDef(aOptions,[tjsComma,tjsAssign,tjsConditional]+StopTokens);
    aType.TypeGuardKind:=tgkIs;
    end
  else if CurrentToken=tjsEXTENDS then
    begin
    GetNextToken;
    aType.ExtendsCond:=ParseTypeDef(aOptions,[tjsComma,tjsAssign,tjsConditional]+StopTokens);
    if CurrentToken=tjsConditional then
      begin
      aType.TypeGuardKind:=tgkExtendsCond;
      Consume(tjsConditional);
      aType.ExtendsTrue:=ParseTypeDef(aOptions,[tjsColon]);
      Consume(tjsColon);
      aType.ExtendsFalse:=ParseTypeDef(aOptions,StopTokens);
      end
    else if CurrentToken=tjsAssign then
      begin
      aType.TypeGuardKind:=tgkExtendsEquals;
      Consume(tjsAssign);
      aType.ExtendsTrue:=ParseTypeDef(aOptions,StopTokens);
      end;
    end;
end;

function TJSParser.ParseTypeDef(aOptions : TParseTypeOptions; StopTokens : TJSTokens = []): TJSTypeDef;

// On entry, first token of the type definition
// On exit, we are on the first token after the type definition

Var
  aType,aSub : TJSTypeDef;
  aStruct : TJSStructuredTypeDef;
  CurrT : TJSToken;
  aStop : TJSTokens;

begin
  aStop:=StopTokens+[tjsbraceClose,tjsGT,tjsSemicolon,tjsComma];
  aStruct:=Nil;
  aType:=Nil;
  Result:=Nil;
  CurrT:=CurrentToken;
  if CurrT in aStop then
    Exit(aType);
  aType:=ParseTypeSimple(aOptions);
  try
    Repeat
      CurrT:=CurrentToken;
      if CurrT in aStop then
        Exit(aType);
      if (CurrT=tjsLT) then
        begin
        ASub:=aType;
        aType:=Nil;
        aType:=ParseGenericType(aSub);
        CurrT:=CurrentToken;
        // a : <>(b:c) => d;
        if (CurrT=tjsBraceOpen) then
          begin
          TJSGenericTypeRef(aType).BaseType:=ParseTypeDef(aOptions,StopTokens);
          CurrT:=CurrentToken;
          end;
        // {
        //   A : T<A>
        //   [B] : C
        // }
        if FScanner.WasEndOfLine then
          Exit(aType);
        end;
      if CurrT in aStop then
        Exit(aType);
      if (CurrT=tjsSquaredBraceOpen)  then
        begin
        if (aType<>Nil) then
          aType:=ParseTypeArrayDef([],aType)
        else
          begin
          aType:=ParseTypeTuple([]);
          if TJSTupleTypeDef(aType).Values.Count=0 then
            begin
            aSub:=aType;
            aType:=TJSArrayTypeDef.Create(aSub.Line,aSub.Column,aSub.Source);
            FreeElement(aSub);
            aSub:=TJSTypeReference.Create(aType.Line,aType.Column,aType.Source);
            TJSTypeReference(aSub).Name:='any';
            TJSArrayTypeDef(aType).BaseType:=aSub;
            end;
          end;
        CurrT:=CurrentToken;
        end;
      if CurrT in aStop then
        Exit(aType);
      case CurrT of
        tjsOR:
          begin
          aStruct:=TJSUnionTypeDef(CreateElement(TJSUnionTypeDef));
          TJSUnionTypeDef(aStruct).AllowEmpty:=aType=Nil;
          if Assigned(aType) then
            aStruct.AddValue(aType);
          aType:=aStruct;
          ParseTypeUnionIntercept(TJSUnionTypeDef(aStruct),aOptions,tjsOR);
          end;
        tjsAnd:
          begin
          aStruct:=TJSIntersectionTypeDef(CreateElement(TJSIntersectionTypeDef));
          TJSIntersectionTypeDef(aStruct).AllowEmpty:=aType=Nil;
          aStruct.AddValue(aType);
          aType:=aStruct;
          ParseTypeUnionIntercept(TJSIntersectionTypeDef(aStruct),aOptions,tjsAnd);
          end;
        tjsExtends :
          ParseTypeGuard(aType,aOptions,aStop);
      else
        if IsIdentifier('is') then
          ParseTypeGuard(aType,aOptions,aStop);
      end;
      CurrT:=CurrentToken;
    Until Not (CurrT in [tjsSquaredBraceOpen,tjsLT]);
    Result:=aType;
    if (Result=Nil) then
      Error(SErrTypeExpected,[CurrentTokenString])
  except
    if (aStruct<>aType) then
      FreeElement(aStruct);
    FreeElement(aType);
    Raise;
  end;
end;

Function TJSParser.ParseDestructuredParam(aParams : TJSTypedParams): TJSTypedParam;

begin
  Result:=aParams.AddParam('');
  try
    Result.Destructured:=TJSObjectTypeDef(CreateElement(TJSObjectTypeDef));
    ParseObjectBody(Result.Destructured);
  Except
    Result.Free;
    Raise;
  end;
end;


Procedure TJSParser.ParseFormalParameterList(aParams : TJSTypedParams);

Var
  P : TJSTypedParam;
  IsSpread : Boolean;
  allowed : TJSTokens;
  aName : {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString;

begin
  allowed:=[tjsEllipsis, tjsIdentifier];
  if IsTypeScript then
    begin
    Include(Allowed,tjsThis);
    Include(Allowed,tjsCurlyBraceOpen);
    end;
  While (CurrentToken in Allowed) do
    begin
    IsSpread:=(CurrentToken=tjsEllipsis);
    if IsSpread then
      Consume(tjsEllipsis);
    if IsTypeScript and (CurrentToken=tjsThis) then
      P:=aParams.AddParam('this')
    else if IsTypeScript and (CurrentToken=tjsCurlyBraceOpen) then
      P:=ParseDestructuredParam(aParams)
    else
      begin
      Expect(tjsIdentifier);
      aName:=CurrentTokenString;
      P:=aParams.AddParam(aName);
      end;
    P.IsSpread:=IsSpread;
    GetNextToken;
    if IsTypeScript then
      begin
      P.IsOptional:=(CurrentToken=tjsConditional);
      if P.IsOptional then
        Consume(tjsConditional);
      // Type is optional
      if (CurrentToken=tjsColon) then
        begin
        Consume(tjsCOLON);
        P.Node:=ParseTypeDef([ptoAllowLiteral],[tjsBraceClose]);
        end;
      end;
    while (CurrentToken=tjsComma) do
       GetNextToken;
    end;
end;


function TJSParser.ParseFunctionDeclaration(aFlags : TFunctionFlags) : TJSFunctionDeclarationStatement;

Var
  Id : {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString;
  D : TJSFuncDef;
  isGenerator : Boolean;
  TP : TJSElementNodes;
  
begin
  {$ifdef debugparser}  Writeln('>>> Entering ParseFunctionDeclaration');{$endif debugparser}
  TP:=nil;
  if (ffConstructor in aFlags) then
    consume(tjsIdentifier)
  else if IsTypeScript and IsIdentifier('static') then
    Consume(tjsIdentifier)
  else
    Consume(tjsFunction);
  isGenerator:=(CurrentToken=tjsMUL);
  if IsGenerator then
    Consume(tjsMul);
  if ffConstructor in aFlags then
    ID:='constructor'
  else if CurrentToken=tjsExtends then
    begin
    ID:='extends';
    GetNextToken;
    end
  else
    ID:=ParseIdentifier;
  if CurrentToken=tjsLT then
    begin
    TP:=TJSElementNodes.Create(TJSElementNode);
    ParseGenericParamList(TP);
    consume(tjsGT);
    end;  
  Result:=TJSFunctionDeclarationStatement(CreateElement(TJSFunctionDeclarationStatement));
  try
    Result.AFunction:=TJSFuncDef.Create;
    D:=Result.AFunction;
    D.GenericParams:=TP;
    D.Name:=ID;
    D.IsConstructor:=(ffConstructor in aFlags);
    Consume(tjsBraceOpen);
    ParseFormalParameterList(D.TypedParams);
    D.UpdateParams;
    Consume(tjsBraceClose);
    if IsTypeScript and (CurrentToken=tjsColon) then
      begin
      consume(tjsColon);
      D.IsAsserts:=IsIdentifier('asserts');
      if D.IsAsserts then
        Consume(tjsIdentifier);
      D.ResultType:=ParseTypeDef([ptoAllowLiteral]);
      end;
    if not (ffAmbient in aFlags) then
      begin
      Consume(tjsCurlyBraceOpen);
      Inc(FFunctionDepth);
      try
        D.Body:=ParseFunctionBody;
        // GetNextToken; not sure
        Consume(tjsCurlyBraceClose);
        Result.IsGenerator:=IsGenerator;
      finally
        Dec(FFunctionDepth);
      end;
      end;
  except
    FreeAndNil(D);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('>>> Exiting ParseFunctionDeclaration');{$endif debugparser}
end;

function TJSParser.ParseStatementList : TJSElement;

Var
  E : TJSElement;
  SL : TJSSTatementList;

begin
  {$ifdef debugparser}  Writeln('>>> ParseStatementList');{$endif debugparser}
  E:=ParseStatement;
  try
    if (CurrentToken in [tjsCurlyBraceClose,tjsEof,tjsCase,tjsDefault]) then
      Result:=E
    else
      begin
      SL:=TJSSTatementList(CreateElement(TJSStatementList));
      try
        SL.A:=E;
        SL.B:=ParseStatementlist();
        Result:=SL;
      except
        FreeElement(SL);
        Raise;
      end;
      end;
  except
    FreeElement(E);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('<<< ParseStatementList');{$endif debugparser}
end;

function TJSParser.ParseBlock : TJSElement;

begin
  {$ifdef debugparser}  Writeln('>>> ParseBlock');{$endif debugparser}
  Consume(tjsCurlyBraceOpen);
  If (CurrentToken=tjsCurlyBraceClose) then
    Result:=CreateElement(TJSEmptyBlockStatement)
  else
    result:=ParseStatementList;
  Consume(tjsCurlyBraceClose);
  {$ifdef debugparser}  Writeln('<<< ParseBlock');{$endif debugparser}
end;

function TJSParser.ParseArrayLiteral: TJSElement;

Var
  N : TJSArrayLiteral;
  E : TJSArrayLiteralElement;
  I : Integer;

begin
  Consume(tjsSquaredBraceOpen);
  N:=TJSArrayLiteral(CreateElement(TJSArrayLiteral));
  Result:=N;
  try
    I:=0;
    While (CurrentToken<>tjsSquaredBraceClose) do
      begin
      If (CurrentToken=tjsComma) then
         begin
         GetNextToken;
         Inc(I);
         end
      else
         begin
         E:=N.Elements.AddElement;
         E.ElementIndex:=I;
         Inc(I);
         E.Expr:=ParseAssignmentExpression;
         If Not (CurrentToken in [tjsComma,tjsSquaredBraceClose]) then
           Error(SErrArrayEnd,[CurrentTokenString])
         end;
      end;
    Consume(tjsSquaredBraceClose);
  except
    FreeElement(Result);
    Raise;
  end;
end;

function TJSParser.ParseObjectLiteral: TJSElement;

Var
  N : TJSObjectLiteral;
  NeedAssign : Boolean;
  E : TJSObjectLiteralElement;
begin
  Consume(tjsCurlyBraceOpen);
  N:=TJSObjectLiteral(CreateElement(TJSObjectLiteral));
  Result:=N;
  try
    While (CurrentToken<>tjsCurlyBraceClose) do
      begin
      While CurrentToken=tjsComma do
         GetNextToken;
      NeedAssign:=True;
      If (CurrentToken in [tjsIdentifier,{$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsString,{$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsnumber]) then
         begin
         E:=N.Elements.AddElement;
         E.Name:=CurrentTokenString;
         GetNextToken;
         end
      else If (CurrentToken=tjsMul) and (EcmaVersion>=MinGeneratorVersion) then
         begin
         E:=N.Elements.AddElement;
         E.Expr:= ParseFunctionExpression([ffGenerator]);
         E.Name:=TJSFunctionDeclarationStatement(E.Expr).AFunction.Name;
         NeedAssign:=False;
         end
      else
         Error(SErrObjectElement,[CurrentTokenString]);
      if needAssign then
        begin
        Consume(tjsColon);
        E.Expr:=ParseAssignmentExpression;
        end;
      While CurrentToken=tjsComma do
         GetNextToken;
{      If Not (CurrentToken in [tjsComma,tjsCurlyBraceClose]) then
        Error(SErrObjectEnd,[CurrentTokenString])}
      end;
    Consume(tjsCurlyBraceClose);
  except
    FreeElement(Result);
    Raise;
  end;
end;

function TJSParser.ParseNumericLiteral: TJSElement;

Var
  L : TJSLiteral;
  D : Double;
  I : Integer;

begin
  {$ifdef debugparser}  Writeln('Parsing numerical literal');{$endif debugparser}
  Result:=Nil;
  try
    if Pos('0x',CurrentTokenString) = 1 then
      begin
      D:=StrToInt('$'+UTF8Encode(Copy(CurrentTokenString,3,Length(CurrentTokenString)-2)));
      I:=0;
      end
    else
      Val(CurrentTokenString,D,I);
    If (I>0) then
      Error(SErrInvalidnumber,[CurrentTokenString]);
    L:=TJSLiteral(CreateElement(TJSLiteral));
    GetNextToken;
    L.Value.AsNumber:=D;
    Result:=L;
  except
    FreeElement(Result);
    Raise;
  end;
end;

function TJSParser.ParseStringLiteral: TJSElement;

Var
  L : TJSLiteral;
begin
    {$ifdef debugparser} Writeln('Parsing string literal');{$endif debugparser}
  Result:=Nil;
  try
    L:=TJSLiteral(CreateElement(TJSLiteral));
    L.Value.AsString:=CurrentTokenString;
    GetNextToken;
    Result:=L;
  except
    FreeElement(Result);
    Raise;
  end;
end;

function TJSParser.ParseRegularExpressionLiteral: TJSElement;

Var
  S,pa,fl : {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString;
  P : integer;
  R : TJSRegularExpressionLiteral;
begin
  Result:=Nil;
  If (CurrentToken=tjsRegex) then
    begin
    S:=CurrentTokenString;
    P:=Length(S);
    While (P>=1) and (S[P]<>'/') do
      Dec(P);
    If (P<=1) then
      Error(SErrInvalidRegularExpression,[CurrentTokenString]);
    pa:=Copy(S,2,P-1);
    fl:=Copy(S,P,Length(S)-P+1);
    R:=TJSRegularExpressionLiteral(CreateElement(TJSRegularExpressionLiteral));
    Result:=R;
    R.Pattern.AsString:=Pa;
    R.PatternFlags.AsString:=Fl;
    R.Argv[0]:=R.Pattern;
    R.Argv[1]:=R.PatternFlags;
    end;
  try
    Consume(tjsRegEx);
  except
    FreeElement(Result);
    Raise;
  end;
end;

function TJSParser.ParseLiteral: TJSElement;

Var
  L : TJSLiteral;

begin
  {$ifdef debugparser}Writeln('Parsing literal: ',GetEnumName(TypeInfo(TJSToken),Ord(CurrentToken)), ' As string: ',CurrentTokenString);{$endif debugparser}
  Result:=Nil;
  Case CurrentToken of
    tjsNull : begin
              L:=TJSLiteral(CreateElement(TJSLiteral));
              Result:=L;
              L.Value.IsNull:=True;
              GetNextToken;
              end;
    tjsTrue,
    tjsFalse: begin
              L:=TJSLiteral(CreateElement(TJSLiteral));
              Result:=L;
              L.Value.AsBoolean:=(CurrentToken=tjsTrue);
              GetNextToken;
              end;
    {$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsNumber : Result:=ParseNumericLiteral;
    {$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsString : Result:=ParseStringLiteral;
    tjsDiv,
    tjsDivEq : Result:=ParseRegularExpressionLiteral
  else
    Error(SErrLiteralExpected,[CurrentTokenString]);
  end;
end;

function TJSParser.ParsePrimaryExpression: TJSElement;

Var
  R : TJSPrimaryExpressionIdent;
  AYS : TJSUnaryExpression;

begin
  {$ifdef debugparser}  Writeln('ParsePrimaryExpression');{$endif debugparser}
  AYS:=Nil;
  Result:=Nil;
  try
    if (CurrentToken in [tjsYield,tjsAwait]) then
      begin
      if CurrentToken=tjsYield then
        AYS:=TJSUnaryExpression(CreateElement(TJSYieldExpression))
      else
        AYS:=TJSUnaryExpression(CreateElement(TJSAwaitExpression));
      GetNextToken;
      end;
    Case CurrentToken of
      tjsThis :
        begin
        Result:=TJSPrimaryExpressionThis(CreateElement(TJSPrimaryExpressionThis));
        GetNextToken;
        end;
      tjsIdentifier:
        begin
        R:=TJSPrimaryExpressionIdent(CreateElement(TJSPrimaryExpressionIdent));
        Result:=R;
        R.Name:=CurrentTokenString;
        GetNextToken;
        end;
      tjsSquaredBraceOpen: Result:=ParseArrayLiteral;
      tjsCurlyBraceOpen: Result:=ParseObjectLiteral;
      tjsBraceOpen:
        begin
        Consume(tjsBraceOpen);
        Result:=ParseExpression;
        Consume(tjsBraceClose);
        end;
    else
      Result:=ParseLiteral;
    end; // Case;
    if (AYS<>Nil) then
      begin
      AYS.A:=Result;
      Result:=AYS;
      end;
  except
    FreeElement(Result);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('Exit ParsePrimaryExpression');{$endif debugparser}
end;


function TJSParser.ParseMemberExpression: TJSElement;

Var
  M  : TJSDotMemberExpression;
  N  : TJSNewMemberExpression;
  B  : TJSBracketMemberExpression;
  Done : Boolean;

begin
  {$ifdef debugparser}  Writeln('ParseMemberExpression');{$endif debugparser}
  Case CurrentToken of
    tjsClass : Result:=ParseClassExpression();
    tjsFunction : Result:=ParseFunctionExpression([ffGenerator]);
    tjsNew      : begin
                  GetNextToken;
                  N:=TJSNewMemberExpression(CreateElement(TJSNewMemberExpression));
                  try
                    Result:=N;
                    N.MExpr:=ParseMemberExpression();
                    if (CurrentToken=tjsBraceOpen) then
                      N.Args:=ParseArguments;
                  except
                    FreeElement(N);
                    Raise;
                  end;
                  end;
  else
    Result:=ParsePrimaryExpression()
  end;
  try
    Done:=False;
    Repeat
      Case CurrentToken of
       tjsDot :
         begin
         M:=TJSDotMemberExpression(CreateElement(TJSDotMemberExpression));
         M.MExpr:=Result;
         Result:=M;
         GetNextToken;
         If (CurrentToken=tjsIdentifier) then
           M.Name:=CurrentTokenString;
         Consume(tjsIdentifier);
         end;
       tjsSquaredBraceOpen:
         begin
         B:=TJSBracketMemberExpression(CreateElement(TJSBracketMemberExpression));
         B.MExpr:=Result;
         Result:=B;
         GetNextToken;
         B.Name:=ParseExpression();
         Consume(tjsSquaredBraceClose);
         end;
      else
        Done:=True;
        isLHS:=True;
      end;
    Until Done;
  except
    FreeElement(Result);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('Exit ParseMemberExpression');{$endif debugparser}
end;

function TJSParser.ParseModuleDeclarationStatement: TJSModuleStatement;
begin
  Result:=TJSModuleStatement(CreateElement(TJSModuleStatement));
  try
    Result.Decl:=ParseModuleDeclaration;
  except
    FreeElement(Result);
    Raise;
  end;
end;

function TJSParser.ParseArguments: TJSarguments;

Var
  E : TJSArrayLiteralElement;

begin
  Consume(tjsBraceOpen);
  Result:=TJSArguments(CreateElement(TJSArguments));
  try
    While (CurrentToken<>tjsBraceClose) do
      begin
      E:=Result.Elements.AddElement;
      E.Expr:=ParseAssignmentExpression;
      If (CurrentToken<>tjsBraceClose) then
        If CurrentToken=tjsComma then
          GetNextToken
        else
          Error(SErrArgumentsExpected,[CurrentTokenString]);
      end;
    Consume(tjsBraceClose);
  except
    FreeElement(Result);
    Raise;
  end;
end;

function TJSParser.ParseLeftHandSideExpression: TJSElement;

Var
  M  : TJSDotMemberExpression;
  B  : TJSBracketMemberExpression;
  C : TJSCallExpression;
  Done : Boolean;

begin
  {$ifdef debugparser}  Writeln('ParseLeftHandSideExpression');{$endif debugparser}
  Case CurrentToken of
    tjsClass : Result:=ParseClassExpression;
    tjsFunction : Result:=ParseFunctionExpression([]);
    tjsNew      : Result:=ParseMemberExpression;
  else
    Result:=ParsePrimaryExpression
  end;
  try
    Done:=False;
    Repeat
      Case CurrentToken of
       tjsDot :
         begin
         M:=TJSDotMemberExpression(CreateElement(TJSDotMemberExpression));
         M.MExpr:=Result;
         Result:=M;
         GetNextToken;
         If (CurrentToken=tjsIdentifier) then
           M.Name:=CurrentTokenString;
         Consume(tjsIdentifier);
         end;
       tjsSquaredBraceOpen:
         begin
         B:=TJSBracketMemberExpression(CreateElement(TJSBracketMemberExpression));
         B.MExpr:=Result;
         Result:=B;
         GetNextToken;
         B.Name:=ParseExpression;
         Consume(tjsSquaredBraceClose);
         end;
       tjsBraceOpen:
         begin
         C:=TJSCallExpression(CreateElement(TJSCallExpression));
         if (Result is TJSUnaryExpression) and (TJSUnaryExpression(Result).PrefixOperatorToken in [tjsAwait,tjsYield]) then
           begin
           C.Expr:=TJSUnaryExpression(Result).A;
           TJSUnaryExpression(Result).A:=C;
           end
         else
           begin
           C.Expr:=Result;
           Result:=C;
           end;
         C.Args:=ParseArguments;
         end;
      else
        {$ifdef debugparser}Writeln('Leaving LHS');{$endif debugparser}
        Done:=True;
        isLHS:=True;
      end;
    Until Done;
  except
    FreeElement(Result);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('Exit ParseLeftHandSideExpression');{$endif debugparser}
end;

function TJSParser.ParsePostFixExpression: TJSElement;
Var
  R : TJSUnaryExpression;

begin
  {$ifdef debugparser}  Writeln('ParsePostfixExpression');{$endif debugparser}
  Result:=ParseLeftHandSideExpression;
  Try
  If (Not IsEndOfLine) and (CurrentToken in [tjsPlusPlus,tjsMinusMinus]) then
    begin
    If (CurrentToken=tjsPlusPLus) then
      R:=TJSUnaryExpression(CreateElement(TJSUnaryPostPlusPlusExpression))
    else
      R:=TJSUnaryExpression(CreateElement(TJSUnaryPostMinusMinusExpression));
    R.A:=Result;
    Result:=R;
    GetNextToken;
    isLHS:=False;
    end;
  except
    FreeElement(Result);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('Exit ParsePostfixExpression');{$endif debugparser}
end;

function TJSParser.ParseUnaryExpression: TJSElement;

Var
  C : TJSElementClass;
  R : TJSUnaryExpression;

begin
  {$ifdef debugparser} Writeln('ParseUnaryExpression');{$endif debugparser}
  C:=Nil;
  Result:=Nil;
  try
    Case CurrentToken of
      tjsDelete     : C:=TJSUnaryDeleteExpression;
      tjsVoid       : C:=TJSUnaryVoidExpression;
      tjsTypeOf     : C:=TJSUnaryTypeOfExpression;
      tjsPlusPlus   : C:=TJSUnaryPrePlusPlusExpression;
      tjsMinusMinus : C:=TJSUnaryPreMinusMinusExpression;
      tjsPlus       : C:=TJSUnaryPlusExpression;
      tjsMinus      : C:=TJSUnaryMinusExpression;
      tjsInv        : C:=TJSUnaryInvExpression;
      tjsNot        : C:=TJSUnaryNotExpression;
    else
      Result:=ParsePostFixExpression;
    end;
    If (Result=Nil) then
      begin
      {$ifdef debugparser} Writeln('Found Unary Expression',GetEnumName(TypeInfo(TJSToken),Ord(CurrentToken)), ' As string: ',CurrentTokenString);{$endif debugparser}
      R:=TJSUnaryExpression(CreateElement(C));
      Result:=R;
      GetNextToken;
      R.A:=ParseUnaryExpression();
      isLHS:=False;
      end;
  except
    FreeElement(Result);
    Raise;
  end;
  {$ifdef debugparser} Writeln('Exit ParseUnaryExpression');{$endif debugparser}
end;

function TJSParser.ParseDebuggerStatement: TJSElement;

begin
  Result:=CreateElement(TJSDebuggerStatement);
  try
    Consume(tjsDebugger);
  except
    FreeElement(Result);
  end;
end;

function TJSParser.ParseMultiplicativeExpression: TJSElement;

Var
  C : TJSElementClass;
  R : TJSMultiplicativeExpression;

begin
  {$ifdef debugparser}  Writeln('ParseMultiplicativeExpression');{$endif debugparser}
  Result:=ParseUnaryExpression;
  try
    While (CurrentToken in [tjsMul,tjsDiv,tjsMod]) do
      begin
      if CurrentToken=tjsMul then
        C:=TJSMultiplicativeExpressionMul
      else if CurrentToken=tjsDiv then
        C:=TJSMultiplicativeExpressionDiv
      else
        C:=TJSMultiplicativeExpressionMod;
      R:=TJSMultiplicativeExpression(CreateElement(C));
      GetNextToken;
      R.A:=Result;
      Result:=R;
      R.B:=ParseUnaryExpression;
      isLHS:=False;
      end;
  except
    FreeElement(Result);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('Exit ParseMultiplicativeExpression');{$endif debugparser}
end;

function TJSParser.ParseNamespaceDeclarationStatement: TJSNamespaceStatement;
begin
  Result:=TJSNamespaceStatement(CreateElement(TJSNameSpaceStatement));
  try
    Result.Decl:=ParseNameSpaceDeclaration(True);
  except
    FreeElement(Result);
    Raise;
  end;
end;

function TJSParser.ParseAdditiveExpression: TJSElement;

Var
  C : TJSElementClass;
  R : TJSAdditiveExpression;

begin
  {$ifdef debugparser}  Writeln('ParseAdditiveExpression');{$endif debugparser}
  Result:=ParseMultiplicativeExpression;
  try
    While (CurrentToken in [tjsPlus,tjsMinus]) do
      begin
      if CurrentToken=tjsPlus then
        C:=TJSAdditiveExpressionPlus
      else
        C:=TJSAdditiveExpressionMinus;
      R:=TJSAdditiveExpression(CreateElement(C));
      GetNextToken;
      R.A:=Result;
      Result:=R;
      R.B:=ParseMultiplicativeExpression;
      isLHS:=False;
      end;
  except
    FreeElement(Result);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('Exit ParseAdditiveExpression');{$endif debugparser}
end;

function TJSParser.ParseShiftExpression: TJSElement;

Var
  C : TJSElementClass;
  R : TJSShiftExpression;

begin
  {$ifdef debugparser}  Writeln('ParseShiftExpression');{$endif debugparser}
  Result:=ParseAdditiveExpression;
  try
    While (CurrentToken in [tjsLshift,tjsRshift,tjsURShift]) do
      begin
      Case CurrentToken of
        tjsLshift : C:=TJSLShiftExpression;
        tjsRshift : C:=TJSRShiftExpression;
        tjsURshift : C:=TJSURShiftExpression;
      end;
      R:=TJSShiftExpression(CreateElement(C));
      R.A:=Result;
      Result:=R;
      GetNextToken;
      R.B:=ParseAdditiveExpression;
      IsLHS:=False;
      end;
  except
    FreeElement(Result);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('Exit ParseShiftExpression');{$endif debugparser}
end;

function TJSParser.ParseRelationalExpression: TJSElement;

Var
  S : Set of TJSToken;
  C : TJSElementClass;
  R : TJSRelationalExpression;

begin
  {$ifdef debugparser}  Writeln('ParseRelationalExpression');{$endif debugparser}
  Result:=ParseShiftExpression;
  try
    S:=[tjsLT,tjsGT,tjsLE,tjsGE,tjsInstanceOf];
    If Not Noin then
      Include(S,tjsIn);
    While (CurrentToken in S) do
      begin
      Case CurrentToken of
        tjsLT : C:=TJSRelationalExpressionLT;
        tjsGT : C:=TJSRelationalExpressionGT;
        tjsLE : C:=TJSRelationalExpressionLE;
        tjsGE : C:=TJSRelationalExpressionGE;
        tjsInstanceOf :C:=TJSRelationalExpressionInstanceOf;
        tjsIn : C:=TJSRelationalExpressionIn;
      end;
      R:=TJSRelationalExpression(CreateElement(C));
      R.A:=Result;
      Result:=R;
      GetNextToken;
      R.B:=ParseRelationalExpression();
      IsLHS:=False;
      end;
  except
    FreeElement(Result);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('Exit ParseRelationalExpression');{$endif debugparser}
end;

function TJSParser.ParseEqualityExpression: TJSElement;

Var
  C : TJSElementClass;
  E : TJSEqualityExpression;

begin
  {$ifdef debugparser}  Writeln('ParseEqualityExpression');{$endif debugparser}
  Result:=ParseRelationalExpression;
  try
     While (CurrentToken in [tjsEq,tjsNE,tjsSEQ,tjsSNE]) do
       begin
       Case CurrentToken of
         tjsEq : C:=TJSEqualityExpressionEQ;
         tjsNE : C:=TJSEqualityExpressionNE;
         tjsSEQ : C:=TJSEqualityExpressionSEQ;
         tjsSNE : C:=TJSEqualityExpressionSNE;
       end;
       GetNextToken;
       E:=TJSEqualityExpression(CreateElement(C));
       Result:=E;
       E.A:=Result;
       E.B:=ParseEqualityExpression();
       E:=Nil;
       IsLHS:=False;
       end;
  except
    FreeElement(Result);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('Exit ParseEqualityExpression');{$endif debugparser}
end;

function TJSParser.ParseBitwiseAndExpression: TJSElement;

Var
  L : TJSBitwiseAndExpression;

begin
  {$ifdef debugparser}  Writeln('ParseBitwiseAndExpression');{$endif debugparser}
  Result:=ParseEqualityExpression;
  try
    If (CurrentToken<>tjsAnd) then
      exit;
    GetNextToken;
    L:=TJSBitwiseAndExpression(CreateElement(TJSBitwiseAndExpression));
    L.A:=Result;
    Result:=L;
    L.B:=ParseBitwiseAndExpression();
    IsLHS:=False;
  except
    FreeElement(Result);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('Exit ParseBitwiseAndExpression');{$endif debugparser}
end;

function TJSParser.ParseBitwiseXORExpression: TJSElement;

Var
  L : TJSBitwiseXOrExpression;

begin
  {$ifdef debugparser}  Writeln('ParseBitwiseXorExpression');{$endif debugparser}
  Result:=ParseBitwiseAndExpression;
  try
    If (CurrentToken<>tjsXOr) then
      exit;
    GetNextToken;
    L:=TJSBitwiseXOrExpression(CreateElement(TJSBitwiseXOrExpression));
    L.A:=Result;
    Result:=L;
    L.B:=ParseBitwiseXORExpression();
    IsLHS:=False;
  except
    FreeElement(Result);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('Exit ParseBitwiseXorExpression');{$endif debugparser}
end;

function TJSParser.ParseBitwiseORExpression: TJSElement;

Var
  L : TJSBitwiseOrExpression;

begin
  {$ifdef debugparser}  Writeln('ParseBitWiseOrExpression');{$endif debugparser}
    Result:=ParseBitwiseXORExpression;
    try
      If (CurrentToken<>tjsOr) then
        exit;
      GetNextToken;
      L:=TJSBitwiseOrExpression(CreateElement(TJSBitwiseOrExpression));
      L.A:=Result;
      Result:=L;
      L.B:=ParseBitwiseORExpression();
      IsLHS:=False;
    except
      FreeElement(Result);
      Raise;
    end;
    {$ifdef debugparser}  Writeln('Exit ParseBitWiseOrExpression');{$endif debugparser}
end;

function TJSParser.ParseLogicalAndExpression: TJSElement;

Var
  L : TJSLogicalAndExpression;

begin
  {$ifdef debugparser}  Writeln('ParseLogicalAndExpression');{$endif debugparser}
  Result:=ParseBitwiseORExpression;
  try
    If (CurrentToken<>tjsAndAnd) then
      exit;
    GetNextToken;
    L:=TJSLogicalAndExpression(CreateElement(TJSLogicalAndExpression));
    L.A:=Result;
    Result:=L;
    L.B:=ParseLogicalAndExpression();
    IsLHS:=False;
  except
    FreeElement(Result);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('Exit ParseLogicalAndExpression');{$endif debugparser}
end;

function TJSParser.ParseLogicalORExpression: TJSElement;

Var
  L : TJSLogicalOrExpression;

begin
  {$ifdef debugparser}  Writeln('ParseLogicalOrExpression');{$endif debugparser}
  Result:=ParseLogicalAndExpression;
  try
    If (CurrentToken<>tjsOROR) then
      exit;
    GetNextToken;
    L:=TJSLogicalOrExpression(CreateElement(TJSLogicalOrExpression));
    L.A:=Result;
    Result:=L;
    L.B:=ParseLogicalOrExpression();
    IsLHS:=False;
  except
    FreeElement(Result);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('Exit ParseLogicalOrExpression');{$endif debugparser}
end;

function TJSParser.ParseConditionalExpression: TJSElement;

Var
  N : TJSConditionalExpression;
  L : TJSElement;
begin
  {$ifdef debugparser}  Writeln('ParseConditionalExpression');{$endif debugparser}
  Result:=Nil;
  Result:=ParseLogicalORExpression;
  try
    If (CurrentToken=tjsConditional) then
      begin
      {$ifdef debugparser}  Writeln('ParseConditionalExpression : Detected conditional ');{$endif debugparser}
      GetNextToken;
      L:=Result;
      N:=TJSConditionalExpression(CreateElement(TJSConditionalExpression));
      Result:=N;
      N.A:=L;
      L:=Nil;
      N.B:=ParseAssignmentExpression;
      Consume(tjsColon);
      N.C:=ParseAssignmentExpression;
      IsLHS:=False;
      end;
  except
    FreeElement(Result);
  end;
  {$ifdef debugparser}  Writeln('Exit ParseConditionalExpression');{$endif debugparser}
end;

function TJSParser.ParseAssignmentExpression: TJSElement;

Var
  N : TJSElement;
  C : TJSElementClass;
  A : TJSAssignStatement;

begin
  {$ifdef debugparser}  Writeln('ParseAssignmentExpression');{$endif debugparser}
  Result:=Nil;
  C:=Nil;
  N:=ParseConditionalExpression;
  If not isLHS then
    Result:=N
  else
    Case CurrentToken of
      tjsAssign    : C:=TJSSimpleAssignStatement;
      tjsMulEq     : C:=TJSMulEqAssignStatement;
      tjsDivEq     : C:=TJSDivEqAssignStatement;
      tjsModEq     : C:=TJSModEqAssignStatement;
      tjsPlusEq    : C:=TJSAddEqAssignStatement;
      tjsMinusEq   : C:=TJSSubEqAssignStatement;
      tjsLShiftEq  : C:=TJSLShiftEqAssignStatement;
      tjsRShiftEq  : C:=TJSRShiftEqAssignStatement;
      tjsURShiftEq : C:=TJSURShiftEqAssignStatement;
      tjsANDEq     : C:=TJSANDEqAssignStatement;
      tjsOREq      : C:=TJSOREqAssignStatement;
      tjsXOREq     : C:=TJSXOREqAssignStatement;
      tjsArrow     : C:=TJSArrowFunction;
    else
//      writeln('Strange token',GetEnumName(TypeInfo(TJSToken),Ord(CurrentToken)), ' As string: ',CurrentTokenString);
      Result:=N
    end;
  If Result<>Nil then
    begin
    {$ifdef debugparser}  Writeln('Exit ParseAssignmentExpression - no assignment');{$endif debugparser}
    Exit;
    end;
  if C=Nil then
    C:=TJSAssignStatement;
  A:=TJSAssignStatement(CreateElement(C));
  try
    Result:=A;
    A.Lhs:=N;
    GetNextToken;
    {$ifdef debugparser}  Writeln('ParseAssignmentExpression - level 2');{$endif debugparser}
    N:=ParseAssignmentExpression();
    {$ifdef debugparser}  Writeln('Exit ParseAssignmentExpression - level 2');{$endif debugparser}
    A.Expr:=N;
    IsLhs:=False;
  except
    FreeElement(Result);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('Exit ParseAssignmentExpression');{$endif debugparser}
end;

function TJSParser.ParseVariableDeclaration(aVarType : TJSVarType = vtVar; IsAmbient: Boolean = False; IsExport: Boolean = False): TJSElement;

Var
  V : TJSVarDeclaration;

begin
  {$ifdef debugparser}  Writeln('ParseVariableDeclaration');{$endif debugparser}
  V:=TJSVarDeclaration(CreateElement(TJSVarDeclaration));
  try
    V.Name:=CurrenttokenString;
    V.VarType:=aVarType;
    Consume(tjsIdentifier);
    if IsTypeScript and (CurrentToken=tjsColon) then
      begin
      Consume(tjsColon);
      V.IsUnique:=IsIdentifier('unique');
      if V.IsUnique then
        GetNextToken;
      V.Typed:=ParseTypeDef([ptoAllowLiteral]);
      end;
    if (CurrentToken=tjsAssign) then
      begin
      GetNextToken;
      V.Init:=ParseAssignmentExpression;
      end;
    Result:=V;
    FCurrentVars.AddNode(Result,IsAmbient,IsExport);
  except
    FreeElement(V);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('Exit ParseVariableDeclaration');{$endif debugparser}
end;

function TJSParser.ParseVariableDeclarationList(aVarType : TJSVarType = vtVar; IsAmbient: Boolean = False; IsExport: Boolean = False): TJSElement;

Var
  E,N : TJSElement;
  L : TJSVariableDeclarationList;
  SL : TJSVariableDeclarationList absolute N;


begin
  {$ifdef debugparser}  Writeln('ParseVariableDeclarationList entry');{$endif debugparser}
  E:=ParseVariableDeclaration(aVarType,IsAmbient,IsExport);
  If (CurrentToken<>tjsComma) then
    Result:=E
  else
    begin
    L:=TJSVariableDeclarationList(CreateElement(TJSVariableDeclarationList));
    Result:=L;
    try
      Consume(tjsComma);
      N:=ParseVariableDeclarationList(aVarType,IsAmbient,IsExport);
      L.A:=E;
      L.B:=N;
      if IsTypeScript then
        if (E is TJSVarDeclaration) then
          if (N is TJSVarDeclaration) then
            TJSVarDeclaration(E).SetForeignType((N as TJSVarDeclaration).Typed)
          else if (N is TJSVariableDeclarationList) then
            if (SL.A is TJSVarDeclaration) then
              TJSVarDeclaration(E).SetForeignType((SL.A as TJSVarDeclaration).Typed);
    except
      FreeElement(Result);
      Raise;
    end;
    end;
  {$ifdef debugparser}  Writeln('ParseVariableDeclarationList exit');{$endif debugparser}
end;

function TJSParser.ParseVariableStatement(aVarType: TJSVarType; IsAmbient: Boolean = False; IsExport: Boolean = False): TJSElement;

Const
   InitialTokens : Array[TJSVarType] of TJSToken = (tjsVar,tjsLet,tjsConst);

Var
  V : TJSVariableStatement;

begin
  {$ifdef debugparser}  Writeln('ParseVariableStatement entry');{$endif debugparser}
  Result:=Nil;
  Consume(InitialTokens[aVarType]);
  Result:=ParseVariableDeclarationList(aVarType,IsAmbient,IsExport);
  try
    Consume(tjsSemicolon,true);
    V:=TJSVariableStatement(CreateElement(TJSVariableStatement));
    V.varType:=aVarType;
    V.VarDecl:=Result;
    Result:=V;
  except
    FreeElement(Result);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('ParseVariableStatement exit');{$endif debugparser}
end;

function TJSParser.ParseEmptyStatement : TJSElement;

begin
  Consume(tjsSemiColon,False);
  Result:=CreateElement(TJSEmptyStatement);
 end;

function TJSParser.ParseIfStatement : TJSElement;

Var
  C,BTrue,BFalse : TJSElement;
  I : TJSIFstatement;

begin
  C:=Nil;
  BTrue:=Nil;
  BFalse:=Nil;
  try
    Consume(tjsIF);
    Consume(tjsBraceOpen);
    C:=ParseExpression;
    Consume(tjsBraceClose);
    BTrue:=ParseStatement;
    If (CurrentToken=tjsElse) then
      begin
      Consume(tjsElse);
      BFalse:=ParseStatement;
      end;
    I:=TJSIfStatement(CreateElement(TJSIfStatement));
    I.Cond:=C;
    I.BTrue:=Btrue;
    I.BFalse:=BFalse;
    Result:=I;
  except
    FreeElement(C);
    FreeElement(BTrue);
    FreeElement(BFalse);
    Raise;
  end;
end;

function TJSParser.ParseIterationStatement : TJSElement;

Var
  F : TJSForStatement;
  FI : TJSForInStatement;
  W : TJSWhileStatement;
  N : TJSElement;

begin
  Result:=Nil;
  N:=Nil;
  CurrentLabelSet.Continuable:=True;
  EnterLabel(SEmptyLabel);
  try
    try
    Case CurrentToken of
      tjsDo :
        begin
        GetNextToken;
        W:=TJSDoWhileStatement(CreateElement(TJSDoWhileStatement));
        Result:=W;
        W.Body:=ParseStatement;
        Consume(tjsWhile);
        Consume(tjsBraceOpen);
        W.Cond:=ParseExpression;
        Consume(tjsBraceClose);
        Consume(tjsSemicolon,True);
        end;
      tjsWhile :
        begin
        GetNextToken;
        W:=TJSWhileStatement(CreateElement(TJSWhileStatement));
        Result:=W;
        Consume(tjsBraceOpen);
        W.Cond:=ParseExpression;
        Consume(tjsBraceClose);
        W.Body:=ParseStatement;
        Result:=W;
        end;
      else
        // For ?
        GetNextToken;
        Consume(tjsBraceopen);
        If (CurrentToken=tjsVar) then
          begin
          GetNextToken;
          N:=ParseVariableDeclarationList;
          // for (var in
          If (CurrentToken=tjsIn) and (N is tJSVarDeclaration) then
            begin
            Fi:=TJSForInStatement(CreateElement(TJSForInStatement));
            Result:=Fi;
            Fi.LHS:=N;
            GetNextToken;
            Fi.List:=ParseExpression;
            Consume(tjsBraceClose);
            Fi.Body:=ParseStatement;
            end;
          // for (var ;
          If (CurrentToken<>tjsSemicolon) then
            If (N is tJSVarDeclaration) then
              Error(SErrSemicolonOrInExpected,[CurrentTokenString])
            else
              Error(SErrSemicolonExpected,[CurrentTokenString]);
          GetNextToken;
          F:=TJSForStatement(CreateElement(TJSForStatement));
          Result:=F;
          If (CurrentToken<>tjsSemicolon) then
            F.Cond:=ParseExpression;
          Consume(tjsSemicolon);
          If (CurrentToken<>tjsBraceClose) then
            F.Incr:=ParseExpression;
          Consume(tjsBraceClose);
          F.Body:=ParseStatement;
          end
        else
          begin
          If (CurrentToken<>tjsSemicolon) then
            begin
            N:=ParseExpression;
            If (CurrentToken=tjsIn) then
              begin
              Fi:=TJSForInStatement(CreateElement(TJSForInStatement));
              Result:=Fi;
              Fi.LHS:=N;
              N:=Nil; // prevent freeing a second time in case of an exception.
              GetNextToken;
              Fi.List:=ParseExpression;
              Consume(tjsBraceClose);
              Fi.Body:=ParseStatement;
              Exit; // We must jump out here
              end
            end
          else
            N:=Nil;
          // For ( Init; Cond; incr)
          F:=TJSForStatement(CreateElement(TJSForStatement));
          Result:=F;
          F.Init:=N;
          N:=Nil; // prevent freeing a second time in case of an exception.
          Consume(tjsSemicolon);
          if (CurrentToken<>tjsSemicolon) then
            F.Cond:=ParseExpression;
          Consume(tjsSemicolon);
          If (CurrentToken<>tjsBraceClose) Then
            F.Incr:=ParseExpression;
          Consume(tjsBraceClose);
          F.Body:=ParseStatement;
          end;
      end; // Case
  Finally
    LeaveLabel;
    FreeCurrentLabelSet;
  end;
  except
    FreeElement(N);
    FreeElement(Result);
    Raise;
  end;
end;

Procedure TJSParser.ParseAliasElements(aElements : TJSAliasElements);
// Parse { N [as M] }. On entry, must be on {, on exit curtoken is token after }
Var
  aName,aAlias : {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString;
begin
  Consume(tjsCurlyBraceOpen);
  if (CurrentToken<>tjsCurlyBraceClose) then
    begin
    Repeat
      if IsTypeScript and (currentToken=tjsDEFAULT) then
        begin
        aName:='default';
        getNextToken
        end
      else
        begin
        Expect(tjsIdentifier);
        aName:=CurrentTokenString;
        Consume(tjsIdentifier);
        end;
      aAlias:='';
      if IdentifierIsLiteral('as') then
        begin
        Consume(tjsIdentifier);
        if IsTypeScript and (currentToken=tjsDEFAULT) then
          begin
          aAlias:='default';
          getNextToken;
          end
        else
          begin
          if IsTypeScript and CurrentTokenIsValidIdentifier then
            begin
            aAlias:=CurrentTokenString;
            GetNextToken
            end
          else
            begin
            Expect(tjsIdentifier);
            aAlias:=CurrentTokenString;
            Consume(tjsIdentifier);
            end;
          end;
        end;  
      With aElements.AddAlias do
        begin
        Name:=aName;
        Alias:=aAlias;
        end;
      if CurrentToken=tjsComma then
        GetNextToken;
    Until (CurrentToken=tjsCurlyBraceClose);
    end;
  Consume(tjsCurlyBraceClose);
end;

function TJSParser.ParseImportStatement: TJSElement;

Var
  Imp : TJSImportStatement;
  aExpectMore : Boolean;
  isAssigned : Boolean;

begin
  isAssigned:=False;
  aExpectMore:=True;
  Consume(tjsImport);
  Imp:=TJSImportStatement(CreateElement(TJSImportStatement));
  try
    Result:=Imp;
    // Just module name
    if CurrentToken = {$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsString then
      begin
      Imp.ModuleName:=CurrentTokenString;
      GetNextToken;
      Exit;
      end;
    // ImportedDefaultBinding
    if CurrentToken = tjsIdentifier then
      begin
      Imp.DefaultBinding:=CurrentTokenString;
      aExpectMore:=GetNextToken=tjsCOMMA;
      if aExpectMore then
        GetNextToken;
      end;
    Case CurrentToken of
      tjsMUL : // NameSpaceImport
        begin
        Consume(tjsMul);
        ConsumeIdentifierLiteral('as');
        Expect(tjsIdentifier);
        Imp.NameSpaceImport:=CurrentTokenString;
        Consume(tjsIdentifier);
        end;
      tjsCurlyBraceOpen:
        begin
        ParseAliasElements(Imp.NamedImports);
        end;
      tjsAssign:
        begin
        if IsTypeScript then
           begin
           Consume(tjsAssign);
           isAssigned:=True;
           Imp.Expression:=ParseExpression;
           end;
        end;
    else
      if aExpectMore then
        Error(SErrExpectedMulOrCurlyBrace,[CurrentTokenString]);
    end;
    if not IsAssigned then
      begin
      ConsumeIdentifierLiteral('from');
      Expect({$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsString);
      Imp.ModuleName:=CurrentTokenString;
      Consume({$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsString);
      end;
    Consume(tjsSemicolon,True);
  except
    FreeElement(Result);
    Raise;
  end;
end;

function TJSParser.ParseExportStatement(isAssign : Boolean): TJSElement;
Var
  Exp : TJSExportStatement;
  IsAbstract, IsAmbient, aSync, aExpectFrom : Boolean;
  F : TJSFunctionDeclarationStatement;
  FF : TFunctionFlags;

begin
   IsAmbient:=False;
  aSync:=False;
  aExpectFrom:=True;
  if IsAssign then
    Consume(tjsAssign)
  else
    Consume(tjsExport);
  if IsTypeScript then
    begin
    if CurrentToken=tjsAssign then
      begin
      isAssign:=True;
      getnexttoken;
      end;
    IsAmbient:=IsIdentifier('declare');
    if IsAmbient then
      GetNextToken;
    IsAbstract:=IsIdentifier('abstract');
    if IsAbstract then
      GetNextToken;
    end;
  Exp:=TJSExportStatement(CreateElement(TJSExportStatement));
  try
    Result:=Exp;
    Case CurrentToken of
      tjsMUL : // NameSpaceImport
        begin
        Consume(tjsMul);
        if not IdentifierIsLiteral('as') then
          Exp.NameSpaceExport:='*'
        else
          begin
          ConsumeIdentifierLiteral('as');
          Expect(tjsIdentifier);
          Exp.NameSpaceExport:=CurrentTokenString;
          Consume(tjsIdentifier);
          end
        end;
      tjsCurlyBraceOpen:
        begin
        ParseAliasElements(Exp.ExportNames);
        end;
      tjsVAR:
        Exp.Declaration:=ParseVariableStatement(vtVar);
      tjsEnum:
        Exp.Declaration:=ParseEnumDeclaration;
      tjsConst:
        if IsTypeScript and (PeekNextToken=tjsENUM) then
          Exp.Declaration:=ParseEnumDeclarationStatement
        else
          Exp.Declaration:=ParseVariableStatement(vtConst);
      tjsLet:
        Exp.Declaration:=ParseVariableStatement(vtLet);
      tjsFunction :
        if IsTypeScript or isAmbient then
          Exp.Declaration:=ParseFunctionDeclaration([ffAmbient])
        else
          Exp.Declaration:=ParseFunctionDeclaration([]);
      tjsClass :
        begin
        Exp.Declaration:=ParseClassDeclaration(IsAmbient or IsTypeScript,IsAbstract);
        end;
      tjsIMPORT :
        begin
        Exp.Declaration:=ParseImportStatement;
        end;
      tjsDEFAULT:
        begin
        Exp.IsDefault:=True;
        aExpectFrom:=False;
        Consume(tjsDefault);
        async:=IdentifierIsLiteral('async');
        if Async then
          GetNextToken;
        IsAbstract:=IdentifierIsLiteral('abstract') and (PeekNextToken=tjsClass);
        if IsAbstract then
          GetNextToken;
        case CurrentToken of
          tjsFunction :
            begin
            FF:=[];
            if IsTypeScript or IsAmbient then
              Include(FF,ffAmbient);
            F:=ParseFunctionExpression(FF);
            F.AFunction.IsAsync:=async;
            Exp.Declaration:=F;
            end;
          tjsClass : Exp.Declaration:=ParseClassDeclaration(True,True);
        else
          if IsTypeScript and IsIdentifier('interface') then
            Exp.Declaration:=ParseInterfaceDeclaration
          else
            Exp.Declaration:=ParseAssignmentExpression;
        end;
        end;
      {$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsAssign:
      begin
      if IsTypeScript then
        begin
        consume(tjsAssign);
        aExpectFrom:=False;
        Exp.Declaration:=ParseExpression;
        end;
      end;
      tjsIdentifier:
        begin
        if IsTypeScript then
          begin
          if IsIdentifier('interface') then
            Exp.Declaration:=ParseInterfaceDeclaration
          else if IsIdentifier('namespace') then
            Exp.Declaration:=ParseNamespaceDeclaration(True)
          else if IsIdentifier('module') then
            Exp.Declaration:=ParseModuleDeclaration
          else if IsIdentifier('class') then
            Exp.Declaration:=ParseClassDeclaration(True)
          else if IsIdentifier('abstract') then
            begin
            consume(tjsIdentifier);
            if (CurrentToken=tjsClass) then
              Exp.Declaration:=ParseClassDeclaration(True,True);
            end
          else if IsIdentifier('type') then
            Exp.Declaration:=ParseTypeDeclaration
          else if IsIdentifier('enum') then
            Exp.Declaration:=ParseEnumDeclaration
          else if IsIdentifier('as') then
            begin
            consume(tjsIdentifier);
            if Not IsIdentifier('namespace') then
              Error(SErrExpectedButFound,['namespace',CurrentTokenString]);
            consume(tjsIdentifier);
            expect(tjsIdentifier);
            Exp.NameSpaceExport:=CurrentTokenString;
            Consume(tjsIdentifier);
            end
          else if IsAssign then
            Exp.Declaration:=ParseExpression
          else
            Error(SErrExpectedMulOrCurlyBraceOrDefault,[CurrentTokenString]);
          end
        else
          Error(SErrExpectedMulOrCurlyBraceOrDefault,[CurrentTokenString]);
        end
    else
      Error(SErrExpectedMulOrCurlyBraceOrDefault,[CurrentTokenString]);
    end;
    if aExpectFrom and IdentifierIsLiteral('from') then
      begin
      ConsumeIdentifierLiteral('from');
      Expect({$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsString);
      Exp.ModuleName:=CurrentTokenString;
      Consume({$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsString);
      end;
    Consume(tjsSemicolon,True);
  except
    FreeElement(Result);
    Raise;
  end;
end;

function TJSParser.ParseContinueStatement : TJSElement;

Var
  L : TJSLabel;
  C : TJSContinueStatement;

begin
  C:=TJSContinueStatement(CreateElement(TJSContinueStatement));
  try
    Result:=C;
    Consume(tjsContinue);
    If (CurrentToken=tjsSemicolon) then
      L:=LookupLabel(SEmptyLabel,tjsContinue)
    else
      begin
      if (CurrentToken=tjsIdentifier) then
        L:=LookupLabel(CurrentTokenString,tjsContinue);
      Consume(tjsIdentifier);
      end;
    Consume(tjsSemicolon,True);
    C.Target:=L.Labelset.Target;
    C.TargetName:=L.Name;
  except
    FreeElement(C);
    Raise;
  end;
end;

function TJSParser.ParseBreakStatement : TJSElement;

Var
  L : TJSLabel;
  B : TJSBreakStatement;

begin
  B:=TJSBreakStatement(CreateElement(TJSBreakStatement));
  try
  Result:=B;
    Consume(tjsBreak);
    If (CurrentToken=tjsSemicolon) then
      L:=LookupLabel(SEmptyLabel,tjsBreak)
    else
      begin
      if (CurrentToken=tjsIdentifier) then
        L:=LookupLabel(CurrentTokenString,tjsBreak);
      Consume(tjsIdentifier);
      end;
    Consume(tjsSemicolon,True);
    B.Target:=L.Labelset.Target;
    B.TargetName:=L.Name;
  except
    FreeElement(B);
    Raise;
  end;
end;

function TJSParser.ParseReturnStatement : TJSElement;

Var
  R : TJSreturnStatement;

begin
  R:=TJSReturnStatement(CreateElement(TJSReturnStatement));
  try
    Result:=R;
    Consume(tjsReturn);
    If (FunctionDepth=0) then
      Error(SErrReturnNotInFunction);
    If Not (CurrentToken in [tjsSemicolon,tjsCurlyBraceClose]) then
      R.Expr:=ParseExpression;
    Consume(tjsSemicolon,True);
  except
    FreeElement(R);
    Raise;
  end;
end;

function TJSParser.ParseWithStatement : TJSElement;

Var
  W : TJSWithStatement;
begin
  W:=TJSWithStatement(CreateElement(TJSWithStatement));
  try
    Consume(tjsWith);
    Consume(tjsBraceOpen);
    W.A:=ParseExpression;
    Consume(tjsBraceClose);
    W.B:=ParseStatement;
  except
    FreeElement(W);
    Raise;
  end;
  Result:=W;
end;

function TJSParser.ParseSwitchStatement : TJSElement;


Var
  N : TJSSwitchStatement;
  Ca : TJSCaseElement;

begin
  N:=TJSSwitchStatement(CreateElement(TJSSwitchStatement));
  try
    N.Target:=CurrentLabelset.Target;
    EnterLabel(SEmptyLabel);
    try
      Consume(tjsSwitch);
      Consume(tjsBraceOpen);
      N.Cond:=ParseExpression;
      Consume(tjsBraceClose);
      Consume(tjsCurlyBraceOpen);
      While (CurrentToken<>tjsCurlyBraceClose) do
        begin
        If (CurrentToken=tjsCase) then
          begin
          GetNextToken;
          Ca:=N.Cases.AddCase;
          Ca.Expr:=ParseExpression;
          end
        else if (CurrentToken=tjsDefault) then
          begin
          If (N.TheDefault<>Nil) then
            Error(SerrDuplicateSwitchDefault);
          Ca:=N.Cases.AddCase;
          N.TheDefault:=Ca;
          GetNextToken;
          end
        else
          Error(SerrCaseEndExpected);
        Consume(tjsColon);
        If Not (CurrentToken in [tjsCurlyBraceClose,tjsCase,tjsDefault]) then
          Ca.Body:=ParseStatementList;
        end;
      Consume(tjsCurlyBraceClose);
    finally
      LeaveLabel;
      FreeCurrentLabelSet;
    end;
    Result:=N;
  except
    FreeElement(N);
    Raise;
  end;
end;

function TJSParser.ParseThrowStatement : TJSElement;

Var
  TS : TJSThrowStatement;

begin
  TS:=TJSThrowStatement(CreateElement(TJSThrowStatement));
  try
    Result:=TS;
    Consume(tjsThrow);
    If IsEndOfLine then
      Error(SErrNewlineAfterThrow);
    TS.A:=ParseExpression;
    Consume(tjsSemicolon,true);
  except
    FreeElement(TS);
    Raise;
  end;
end;

function TJSParser.ParseTryStatement : TJSElement;

Var
  BO,BC,BF : TJSElement;
  Id : {$IFDEF FPC_DOTTEDUNITS}Js.Tree{$ELSE}jsTree{$ENDIF}.TJSString;
  T : TJSTryStatement;

begin
  BO:=Nil;
  BC:=Nil;
  BF:=Nil;
  Result:=Nil;
  Consume(tjsTry);
  try
    Bo:=ParseBlock;
    if (CurrentToken=tjscatch) then
      begin
      Consume(tjsCatch);
      Consume(tjsBraceOpen);
      if (CurrentToken=tjsIdentifier) then
        id:=CurrentTokenString;
      Consume(tjsIdentifier);
      Consume(tjsBraceClose);
      BC:=ParseBlock;
      end;
    if (CurrentToken=tjsFinally) then
      begin
      consume(tjsFinally);
      BF:=ParseBlock;
      end;
    If (BF=Nil) and (BC=Nil) then
      Error(SErrCatchFinallyExpected);
    If Assigned(BC) AND Assigned(BF) then
      T:=TJSTryStatement(CreateElement(TJSTryCatchFinallyStatement))
    else if Assigned(BC) then
      T:=TJSTryStatement(CreateElement(TJSTryCatchStatement))
    else
      T:=TJSTryStatement(CreateElement(TJSTryFinallyStatement));
    Result:=T;
    T.Block:=Bo;
    Bo:=Nil;
    T.BCatch:=BC;
    BC:=Nil;
    T.BFinally:=BF;
    BF:=Nil;
    T.Ident:=ID;
  except
    FreeElement(Bo);
    FreeElement(BC);
    FreeElement(BF);
    FreeElement(Result);
    Raise;
  end;
end;

function TJSParser.ParseEnumDeclaration: TJSEnumDeclaration;
// We are on the 'enum' identifier on entry, last token of type on exit
var
  El : TJSEnumElement;

begin
  GetNextToken; // Skip 'enum'
  Result:=TJSEnumDeclaration(CreateElement(TJSEnumDeclaration));
  try
    Result.Name:=ParseIdentifier;
    consume(tjsCurlyBraceOpen);
    Result.EnumDef:=TJSEnumTypeDef(CreateElement(TJSEnumTypeDef));
    While not (CurrentToken=tjsCurlyBraceClose) do
      begin
      if (CurrentToken=tjsComma) then
        Consume(tjsComma);
      // Trailing ,
      if (CurrentToken<>tjsCurlyBraceClose) then
        begin
        if CurrentTokenIsValidIdentifier then
          begin
          EL:=Result.EnumDef.AddName(CurrentTokenString);
          GetNextToken;
          end
        else
          EL:=Result.EnumDef.AddName(ParseIdentifier);
        if CurrentToken=tjsAssign then
          begin
          Consume(tjsAssign);
          el.Value:=ParseLiteral;
          end;
        end;
      end;
    Consume(tjsCurlyBraceClose);
  except
    FreeElement(Result);
    Raise;
  end;
end;

function TJSParser.ParseEnumDeclarationStatement: TJSElement;
// We are on the 'const' token or 'enum' identifier on entry, last token of type on exit

Var
  aES : TJSEnumStatement;
  IsConst : Boolean;

begin
  IsConst:=(CurrentToken=tjsConst);
  if isConst then
    Consume(tjsConst);
  aES:=TJSEnumStatement(CreateElement(TJSEnumStatement));
  Result:=aES;
  try
    aES.EnumDecl:=ParseEnumDeclaration;
    aES.EnumDecl.EnumDef.IsConst:=IsConst;
  except
    FreeElement(Result);
    Raise;
  end;
end;

function TJSParser.ParseTypeDeclaration: TJSTypeDeclaration;

// We are on the 'type' identifier on entry, last token of type on exit

begin
  GetNextToken; // Skip 'type'
  Result:=TJSTypeDeclaration(CreateElement(TJSTypeDeclaration));
  try
    Result.Name:=ParseIdentifier;
    if (CurrentToken=tjsLT) then
      begin
      Result.TypeParams:=TJSElementNodes.Create(TJSElementNode);
      ParseGenericParamList(Result.TypeParams);
      Consume(tjsGT);
      end;
    Consume(tjsAssign);
    Result.TypeDef:=ParseTypeDef([ptoAllowLiteral]);
  except
    FreeElement(Result);
    Raise;
  end;
end;

function TJSParser.ParseTypeDeclarationStatement: TJSElement;
// We are on the 'type' identifier on entry, last token of type on exit

Var
  aTS : TJSTypeStatement;

begin
  aTS:=TJSTypeStatement(CreateElement(TJSTypeStatement));
  Result:=aTS;
  try
    aTS.TypeDecl:=ParseTypeDeclaration;
  except
    FreeElement(Result);
    Raise;
  end;
end;

function TJSParser.ParseFunctionExpression(aFlags : TFunctionFlags): TJSFunctionDeclarationStatement;

Var
  Oni,olhs: Boolean;
  F : TJSFunctionDeclarationStatement;
  TP : TJSElementNodes;
  N : {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString;

begin
  {$ifdef debugparser} Writeln('>>> ParseFunctionExpression');{$endif}
  oni:=NoIn;
  olhs:=IsLHS;
  F:=Nil;
  TP:=Nil;
  try
    NoIn:=False;
    IsLHS:=False;
    F:=TJSFunctionDeclarationStatement(CreateElement(TJSFunctionDeclarationStatement));
    try
      if not (ffGenerator in aFlags) then
        Consume(tjsFunction);
      F.isGenerator:=(CurrentToken=tjsMUL);
      if F.IsGenerator then
        Consume(tjsMul);
      if (CurrentToken=tjsIdentifier) then
        begin
        n:=CurrentTokenstring;
        GetNextToken;
        end
      else
        n:='';
      if n='' then ; // what to do with that?
      if CurrentToken=tjsLT then
        begin
        TP:=TJSElementNodes.Create(TJSElementNode);
        ParseGenericParamList(TP);
        consume(tjsGT);
        end;
      Consume(tjsBraceOpen);
      F.AFunction:= TJSFuncDef.Create;
      F.Afunction.GenericParams:=TP;
      F.AFunction.IsConstructor:=ffConstructor in aFlags;

      TP:=nil;
      ParseFormalParameterList(F.AFunction.TypedParams);
      Consume(tjsBraceClose);
      if CurrentToken=tjsColon then
        begin
        Consume(tjsColon);
        F.AFunction.ResultType:=ParseTypeDef([ptoAllowLiteral]);
        end;
      if (CurrentToken=tjsCurlyBraceOpen) then
        begin
        Consume(tjsCurlyBraceOpen);
        Inc(FFunctionDepth);
        try
          F.AFunction.Body:=ParseFunctionBody;
          F.AFunction.Name:=n;
        Finally
          Dec(FFunctionDepth);
        end;
        Consume(tjsCurlyBraceClose);
        end;
      Result:=F;
    except
      FreeElement(F);
      Raise;
    end;
  finally
    NoIn  := oni;
    IsLHS := olhs;
  end;
  {$ifdef debugparser} Writeln('<<< ParseFunctionExpression');{$endif}
end;

function TJSParser.ParseFunctionStatement : TJSElement;

Var
  F : TJSFunctionDeclarationStatement;
  I : TJSPrimaryExpressionIdent;
  A : TJSAssignStatement;
  E : TJSExpressionStatement;

begin
  {$ifdef debugparser} Writeln('>>> ParseFunctionStatement');{$endif}
  F:=Nil;
  I:=Nil;
  A:=Nil;
  try
    F:=ParseFunctionExpression([]);
    I:=TJSPrimaryExpressionIdent(CreateElement(TJSPrimaryExpressionIdent));
    I.Name:=F.AFunction.Name;
    A:=TJSAssignStatement(CreateElement(TJSAssignStatement));
    A.LHS:=I;
    I:=Nil;
    A.Expr:=F;
    F:=Nil;
    E:=TJSExpressionStatement(CreateElement(TJSExpressionStatement));
    E.A:=A;
    A:=Nil;
    Result:=E;
  except
    FreeElement(F);
    FreeElement(I);
    FreeElement(A);
    Raise;
  end;
  {$ifdef debugparser} Writeln('<<< ParseFunctionStatement');{$endif}
end;

function TJSParser.ParseLabeledStatement : TJSElement;

Var
  OL : TJSLabelSet;
  LS : TJSLabeledStatement;
begin
  LS:=TJSLabeledStatement(CreateElement(TJSLabeledStatement));
  try
    Result:=LS;
    OL:=FCurrentLabelSet;
    try
      FCurrentLabelSet:=Nil;
      LS.target:=CurrentLabelSet.Target;
      Repeat
        LS.TheLabel:=EnterLabel(CurrentTokenString);
        Consume(tjsIdentifier);
        Consume(tjsColon);
      Until (CurrentToken<>tjsIdentifier) or (PeekNextToken<>tjsColon);
      Case CurrentToken of
         tjsDo,tjsWhile,tjsFor : LS.A:=ParseIterationStatement;
         tjsswitch : LS.A:=ParseSwitchStatement;
      else
        LS.A:=ParseStatement;
      end;
    finally
      FreeCurrentLabelSet;
      FCurrentLabelSet:=Ol;
    end;
  except
    FreeElement(LS);
    Raise;
  end;
end;

procedure TJSParser.FreeCurrentLabelSet;

Var
  L : TJSLabelSet;

begin
  While Assigned(FCurrentLabelSet) do
    begin
    L:=FCurrentLabelset.Next;
    FCurrentLabelSet.Free;
    FCurrentLabelSet:=L;
    end;
end;

function TJSParser.GetVersion: TECMAVersion;
begin
  Result:=FSCanner.ECMAVersion;
end;

function TJSParser.ParseExpressionStatement : TJSElement;

Var
  E : TJSElement;
  R : TJSExpressionStatement;
begin
  {$ifdef debugparser}  Writeln('ParseExpressionStatement');{$endif debugparser}
  E:=ParseExpression;
  Consume(tjsSemicolon,True);
  R:=TJSExpressionStatement(CreateElement(TJSExpressionStatement));
  R.A:=E;
  Result:=R;
  {$ifdef debugparser}  Writeln('Exit ParseExpressionStatement');{$endif debugparser}
end;

function TJSParser.ParseExpression : TJSElement;

Var
  C : TJSCommaExpression;

begin
  {$ifdef debugparser}  Writeln('ParseExpression');{$endif debugparser}
  Result:=ParseAssignmentExpression;
  try
    If (CurrentToken=tjsComma) then
      begin
      C:=TJSCommaExpression(CreateElement(TJSCommaExpression));
      C.A:=Result;
      Result:=C;
      GetNextToken;
      C.B:=ParseExpression();
      end;
  except
    FreeElement(Result);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('Exit ParseExpression');{$endif debugparser}
end;

function TJSParser.ParseStatement(IsAmbient : Boolean = False;IsExport : Boolean = False): TJSElement;

Var
  FF : TFunctionFlags;

begin
  {$ifdef debugparser} Writeln('>>> Parsestatement');{$endif}
  Result:=Nil;
  FF:=[];
  if isAmbient then
    FF:=[ffAmbient];
  Case CurrentToken of
    tjsCurlyBraceOpen :
      Result:=ParseBlock;
    tjsLet:
      Result:=ParseVariableStatement(vtLet);
    tjsConst:
      if IsTypeScript and (PeekNextToken=tjsEnum) then
        Result:=ParseEnumDeclarationStatement
      else
        Result:=ParseVariableStatement(vtConst);
    tjsVar:
      Result:=ParseVariableStatement(vtVar,IsAmbient,IsExport);
    tjsSemicolon:
      Result:=ParseEmptyStatement;
    tjsIf:
      Result:=ParseIfStatement;
    tjsDo,tjsWhile,tjsFor:
      Result:=ParseIterationStatement;
    tjsClass:
      Result:=ParseClassStatement;
    tjsContinue:
      Result:=ParseContinueStatement;
    tjsImport:
      Result:=ParseImportStatement;
    tjsExport:
      Result:=ParseExportStatement(False);
    tjsBreak:
      Result:=ParseBreakStatement;
    tjsReturn:
      Result:=ParseReturnStatement;
    tjsWith:
      Result:=ParseWithStatement;
    tjsDebugger:
      Result:=ParseDebuggerStatement;
    tjsSwitch:
      Result:=ParseSwitchStatement;
    tjsThrow:
      Result:=ParseThrowStatement;
    tjsTry:
      Result:=ParseTryStatement;
    tjsFunction:
      begin
      If (PeekNextToken<>tjsBraceOpen) then
        Result:=ParseFunctionStatement;
      Error(SErrFunctionNotAllowedHere);
      end;
    tjsAwait,
    tjsYield,
    tjsIdentifier:
      If (PeekNextToken=tjsColon) then
        Result:=ParseLabeledStatement
      else if IsTypeScript then
        begin
        if IsIdentifier('type') then
          Result:=ParseTypeDeclarationStatement
        else if IsIdentifier('abstract') then
          begin
          consume(tjsIdentifier);
          expect(tjsClass);
          Result:=ParseClassStatement(True,True);
          end
        else if IsIdentifier('constructor') then
          begin
          Include(FF,ffConstructor);
          Result:=ParseFunctionDeclaration(FF);
          end
        else
          Result:=ParseExpressionStatement;
        end
      else
        Result:=ParseExpressionStatement;
  else
    Result:=ParseExpressionStatement;
  end;
  {$ifdef debugparser} If Assigned(Result) then Writeln('<<< Parsestatement ',Result.ClassName) else Writeln('<<< Parsestatement (null');{$endif}
end;

Function TJSParser.AddToElements(aSource : TJSSourceElements; aElement : TJSElement; aIsAmbient : Boolean = False; aIsExport : Boolean = False): Boolean;

Var
  N : TJSTransientElementNode;

begin
  Result:=True;
  if aElement is TJSInterfaceDeclaration then
    aSource.Interfaces.AddNode(aElement,aIsAmbient,aIsExport)
  else if aElement is TJSClassDeclaration then
    aSource.Classes.AddNode(aElement,aIsAmbient,aIsExport)
  else if aElement is TJSVarDeclaration then
    aSource.Vars.AddNode(aElement,aIsAmbient,aIsExport)
  else if aElement is TJSTypeDeclaration then
    aSource.Types.AddNode(aElement,aIsAmbient,aIsExport)
  else if aElement is TJSModuleDeclaration then
    aSource.Modules.AddNode(aElement,aIsAmbient,aIsExport)
  else if aElement is TJSNamespaceDeclaration then
    aSource.NameSpaces.AddNode(aElement,aIsAmbient,aIsExport)
  else if aElement is TJSFunctionStatement then
    begin
    N:=TJSTransientElementNode.Create(aSource.Functions);
    N.IsAmbient:=aIsAmbient;
    N.IsExport:=aIsExport;
    N.Node:=aElement;
    end
  else
    Result:=False;
end;

function TJSParser.ParseTypeStatement(Elements : TJSSourceElements; ScopeType : TScopeType; aIsAmbient : Boolean) : TJSElement;

begin
  Result:=Nil;
  if ScopeType=stClass then ;

  if IsIdentifier('module') then
    Result:=Self.ParseModuleDeclarationStatement
  else if IsIdentifier('namespace') or IsIdentifier('global') then
    Result:=Self.ParseNamespaceDeclarationStatement
  else if IsIdentifier('type') then
    Result:=Self.ParseTypeDeclarationStatement
  else if IsIdentifier('interface') then
    Result:=Self.ParseInterfaceDeclarationStatement
  else if (IsIdentifier('static')) then
    begin
    if aIsAmbient then
      begin
      Result:=Self.ParseFunctionDeclaration([ffAmbient]);
      Elements.Functions.AddNode(aIsAmbient).Node:=Result;
      end;
    end;
  if Assigned(Result) and (Result is TJSDeclarationStatement) then
    begin
    Elements.Statements.AddNode(aIsAmbient).Node:=Result;
    AddToElements(Elements,TJSDeclarationStatement(Result).GetDeclaration,aIsAmbient);
    end;
end;

function TJSParser.ParseSourceElements (ScopeType : TScopeType = stFunction; ParentisAmbient : Boolean = False): TJSSourceElements;

Const
  StatementTokens = [tjsNULL, tjsTRUE, tjsFALSE,
      tjsAWait, tjsTHIS, tjsIdentifier,{$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsSTRING,{$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsNUMBER,
      tjsBraceOpen,tjsCurlyBraceOpen,tjsSquaredBraceOpen,
      tjsLet, tjsConst, tjsDebugger, tjsImport, tjsExport,
      tjsNew,tjsDelete,tjsVoid,tjsTypeOf,
      tjsPlusPlus,tjsMinusMinus,
      tjsPlus,tjsMinus,tjsNot,tjsNE,tjsSNE,tjsSemicolon,
      tjsVAR,tjsIF,tjsDO,tjsWHILE,tjsFOR,{$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsCONTINUE,{$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsBREAK,{$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsReturn,
      tjsWith,{$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsSWITCH,tjsThrow,TjsTry,tjsDIV,tjsDIVEQ, tjsEnum];

Var
  F : TJSFunctionDeclarationStatement;
  C : TJSClassDeclaration;
  E : TJSElement;
  Done : Boolean;
  VS : TJSElementNodes;
  isAmbient, isAbstract, aSync : Boolean;
  FF : TFunctionFlags;

  procedure DefaultParsing;

  begin
    if CurrentToken in StatementTokens then
      begin
      E:=Self.ParseStatement(IsAmbient);
      Result.Statements.AddNode(IsAmbient).Node:=E;
      if E is TJSExportStatement then
        AddToElements(Result,TJSExportStatement(E).Declaration,IsAmbient,True);
      end
    else
      Done:=True;
  end;

begin
  {$ifdef debugparser} Writeln('>>> Entering source elements');{$endif}
  Result:=TJSSourceElements(CreateElement(TJSSourceElements));
  try
    Done:=False;
    aSync:=False;
    VS:=FCurrentVars;
    Try
      FCurrentVars:=Result.Vars;
      Repeat
        {$ifdef debugparser} Writeln('Sourceelements start:',GetEnumName(TypeInfo(TJSToken),Ord(CurrentToken)), ' As string: ',CurrentTokenString);{$endif debugparser}
        IsAmbient:=False;
        if IsTypeScript then
          begin
          isAmbient:= (ScopeType in [stFunction,stModule]) and IsIdentifier('declare');
          if IsAmbient then
            GetNextToken
          Else
            begin
            //isAmbient:= (ScopeType in [stModule,stNamespace]) and (CurrentToken=tjsEXPORT);
            // if IsAmbient then
            //  GetNextToken;
            end;
          if IsAmbient then
            begin
            IsAbstract:=IsIdentifier('abstract');
            if IsAbstract then
              GetNextToken;
            end;
          end;
        isAmbient:=isAmbient or ParentisAmbient;

        aSync:= (ECMAVersion>=MinAsyncVersion) and IsIdentifier('async');
        if aSync then
          GetNextToken;
        Case CurrentToken of
        {$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsEOF:
          done:=True;
        {$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsFunction:
          begin
          If (PeekNextToken<>tjsBraceOpen) then
            begin
            FF:=[];
            if isAmbient then
              FF:=[ffAmbient];
            F:=Self.ParseFunctionDeclaration(FF);
            F.AFunction.IsAsync:=aSync;
            Result.Functions.AddNode(IsAmbient,False).Node:=F;
            end
          else
            begin
            {$ifdef debugparser} Writeln('Function expression detected');{$endif}
            E:=Self.ParseStatement;
            Result.Statements.AddNode(IsAmbient).Node:=E;
            end;
          end;
        {$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsClass:
          begin
          E:=Self.ParseClassStatement(isAmbient,isAbstract);
          Result.Statements.AddNode(IsAmbient).Node:=E;
          C:=TJSClassStatement(E).Decl;
          Result.Classes.AddNode(IsAmbient).Node:=C;
          end;
        {$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsEnum:
          begin
          E:=Self.ParseEnumDeclarationStatement;
          Result.Statements.AddNode(IsAmbient).Node:=E;
          Result.Enums.AddNode(IsAmbient).Node:=TJSEnumStatement(E).EnumDecl;
          end;
        {$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsMul:
          begin
          if (ScopeType=stClass) then
            begin
            //
            end
          else
            DefaultParsing;
          end;
        {$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsSQuaredBraceOpen:
          begin
          if isTypeScript and (ScopeType=stClass) then
            begin
            E:=Self.ParseIndexSignatureStatement;
            Result.Statements.AddNode(IsAmbient).Node:=E;
            Result.Vars.AddNode(IsAmbient).Node:=TJSIndexSignatureStatement(E).Decl;
            end;
          end;

        else
          // else of Case
          if IsTypeScript then
            begin
            E:=ParseTypeStatement(Result,ScopeType,IsAmbient);
            if E=Nil then
              DefaultParsing;
            end
          else
            DefaultParsing;
        end;
        {$ifdef debugparser} Writeln('Sourceelements Done : ',Done);{$endif}
      Until Done;
    Finally
      FCurrentVars:=VS;
    end;
  except
    FreeElement(Result);
    Raise;
  end;
  {$ifdef debugparser}   Writeln('<<< Exiting source elements');{$endif}
end;

function TJSParser.ParseFunctionBody : TJSFunctionBody;

Var
  E : TJSElement;

begin
  {$ifdef debugparser} Writeln('>>> Entering FunctionBody');{$endif}
  Result:=TJSFunctionBody(CreateElement(TJSFunctionBody));
  try
    E:=Self.ParseSourceElements;
    Result.A:=E;
  except
    FreeElement(Result);
    Raise;
  end;
  {$ifdef debugparser} Writeln('<<< Exiting FunctionBody');{$endif}
end;

function TJSParser.ParseClassBody(isAmbient : Boolean): TJSSourceElements;

begin
  {$ifdef debugparser} Writeln('>>> Entering ParseClassBody');{$endif}
  Result:=Self.ParseSourceElements(stClass,IsAmbient);
  {$ifdef debugparser} Writeln('<<< Exiting ParseClassBody');{$endif}
end;

procedure TJSParser.ClassDefToMembers(aClass : TJSClassDeclaration; aClassDef : TJSObjectTypeDef);

Var
  El : TJSObjectTypeElementDef;
  I : Integer;

begin
  if (aClassDef.ElementCount>0) and (aClass.Members=Nil) then
    aClass.Members:=TJSSourceElements(CreateElement(TJSSourceElements));
  For I:=0 to aClassDef.ElementCount-1 do
    begin
    El:=aClassDef.Elements[i];
    if El is TJSPropertyDeclaration then
      aClass.Members.Vars.AddNode(True,False).Node:=El
    else if El is TJSMethodDeclaration then
      aClass.Members.Functions.AddNode(True,False).Node:=El;
    end;
end;

function TJSParser.ParseClassDeclaration(isAmbient: Boolean; IsAbstract: Boolean): TJSClassDeclaration;

Var
//  aName : {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString;
//  aTypeDef : TJSTypeDef;
  aClassDef : TJSObjectTypeDef;

begin
  //aTypeDef:=Nil;
  if IsAmbient then
    Result:=TJSAmbientClassDeclaration(CreateElement(TJSAmbientClassDeclaration))
  else
    Result:=TJSClassDeclaration(CreateElement(TJSClassDeclaration));
  try
    Result.IsAbstract:=IsAbstract;
    Consume(tjsClass);
    if CurrentToken=tjsIdentifier then
      Result.Name:=ParseIdentifier;

    if CurrentToken=tjsLT then
      begin
      Result.TypeParams:=TJSElementNodes.Create(TJSElementNode);
      ParseGenericParamList(Result.TypeParams);
      Consume(tjsGT);
      end;
    if CurrentToken=tjsExtends then
      begin
      Consume(tjsExtends);
      Result.Extends:=ParseTypeDef([ptoRefOnly,ptoFunctionCall],[tjsCurlyBraceOpen]);
      end;
    if IsIdentifier('implements') then
      begin
      Result.ImplementsTypes:=TJSElementNodes.Create(TJSElementNode);
      Consume(tjsIdentifier);
      Repeat
        if CurrentToken=tjsComma then
          GetNextToken;
        Result.ImplementsTypes.AddNode(ParseTypeDef());
      Until (CurrentToken<>tjsComma);
      end;
    if IsAmbient then
      begin
      aClassDef:=TJSObjectTypeDef(CreateElement(TJSObjectTypeDef));
      TJSAmbientClassDeclaration(Result).ClassDef:=aClassDef;
      ParseAmbientClassBody(aClassDef);
      Consume(tjsCurlyBraceClose);
      ClassDefToMembers(Result,aClassDef);
      end
    else
      begin
      Consume(tjsCurlyBraceOpen);
      Result.Members:=ParseClassBody(IsAmbient);
      Consume(tjsCurlyBraceClose);
      end;
  except
    FreeElement(Result);
    Raise;
  end;
end;

Procedure TJSParser.ParseAmbientClassBody(aObj: TJSObjectTypeDef);

// On entry on {
// On exit on }

  Function ParseAccessibility : TAccessibility;

  begin
    Result:=accDefault;
    if CurrentToken=tjsIdentifier then
      Case CurrentTokenString of
        'private' : Result:=accPrivate;
        'public' : Result:=accPublic;
        'protected' : Result:=accProtected;
      end;
    if Result<>accDefault then
      begin
      if PeekNextToken=tjsColon then
        Result:=accDefault
      else
        GetNextToken;
      end;
  end;

  Function CreateAny : TJSTypeReference;

  begin
    Result:=TJSTypeReference(CreateElement(TJSTypeReference));
    Result.Name:='any';
  end;
  
  Function CheckSpecial(aName : {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSstring) : Boolean;
  
  begin
    Result:=IsIdentifier(aName) and Not (PeekNextToken in [tjsConditional,tjsColon]);
    If Result then 
      GetNextToken;
  end;

var
  aName : {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString;
  IsSet, IsGet, IsAbstract,isStatic, isReadOnly, isOptional : Boolean;
  E : TJSObjectTypeElementDef;
  F : TJSMethodDeclaration ;
  FS : TJSFunctionDeclarationStatement;
  FuncF : TFunctionFlags;
  TP : TJSElementNodes;
  acc : TAccessibility;

begin
  Consume(tjsCurlyBraceOpen);
  While (CurrentToken<>tjsCurlyBraceClose) do
    begin
    aName:='';
    E:=Nil;
    acc:=ParseAccessibility;
    isStatic:=IsIdentifier('static');
    if IsStatic then
      GetNextToken;
    isAbstract:=IsIdentifier('abstract');
    if IsAbstract then
      GetNextToken;
    isReadOnly:=CheckSpecial('readonly');
    isGet:=CheckSpecial('get');
    isSet:=CheckSpecial('set');
    If CurrentTokenIsValidIdentifier then
       begin
       aName:=CurrentTokenString;
       GetNextToken;
       end
    else if (IsGet or IsSet) and (CurrentToken=tjsBraceOpen) then
       begin
       // Regular get/set methods.
       If IsGet then
         aName:='get'
       else
         aName:='set';
       isGet:=False;
       isSet:=False;
       end
    else
       if (Not (CurrentToken in [tjsBraceOpen,tjsLT,tjsSQuaredBraceOpen,tjsNew])) then
         Error(SErrObjectElement,[CurrentTokenString]);
    isOptional:=(CurrentToken=tjsConditional);
    if isOPtional then
      GetNextToken;
    case CurrentToken of
      // static readonly A = B;
      tjsAssign :
        begin
        if not (isStatic and isReadOnly) then
          Error(SErrObjConstMustBeStaticReadonly);
        GetNextToken;
        E:=TJSClassConstDeclaration(CreateElement(TJSClassConstDeclaration));
        E.Name:=aName;
        TJSClassConstDeclaration(E).Value:=ParseExpression;
        end;
      tjsSQuaredBraceOpen:
        begin
        // [a:type] : Type
        E:=ParseIndexSignature;
        end;
      // PropertyName : PropertyType
      tjsColon:
        begin
        Consume(tjsColon);
        E:=TJSPropertyDeclaration(CreateElement(TJSPropertyDeclaration));
        E.Name:=aName;
        E.ElementType:=ParseTypeDef([ptoAllowLiteral],[tjsComma,tjsSemicolon]);
        end;
      // <T1> () : TypeName;
      // () : TypeName;
      tjsNew,
      tjsWith,
      tjsLT,
      tjsBraceOpen:
        begin
        if CurrentToken=tjsNew then
          begin
          aName:='new';
          GetNextToken;
          end;
        if CurrentToken=tjsWith then
          begin
          aName:='with';
          GetNextToken;
          end;
        FuncF:=[ffGenerator];
        if aName='constructor' then
          Include(FuncF,ffConstructor);
        TP:=Nil;
        F:=TJSMethodDeclaration(CreateElement(TJSMethodDeclaration));
        F.Name:=aName;
        E:=F;
        if CurrentToken=tjsLT then
          begin
          TP:=TJSElementNodes.Create(TJSElementNode);
          ParseGenericParamList(TP);
          consume(tjsGT);
          end;
        FS:=ParseFunctionExpression(FuncF);
        FS.AFunction.Name:=aName;
        F.FuncDef:=FS.AFunction;
        FS.AFunction:=Nil;
        F.TypeParams:=TP;
        if Not Assigned(F.Funcdef.ResultType) then
          F.Funcdef.ResultType:=CreateAny;
        FreeElement(FS);
        end;
      tjsComma,tjsSemicolon:
        begin
        // PropertyName ; Type is any
        E:=TJSPropertyDeclaration(CreateElement(TJSPropertyDeclaration));
        E.Name:=aName;
        E.ElementType:=CreateAny;
        end;
    else
      Error(SErrExpectedColonBrace,[CurrentTokenString]);
    end;
    if Assigned(E) then
      begin
      E.Accessibility:=acc;
      if IsOptional then
        E.Optional:=koOptional;
      E.IsStatic:=IsStatic;
      E.IsAbstract:=IsAbstract;
      E.IsGet:=IsGet;
      E.IsSet:=IsSet;
      E.IsReadonly:=IsReadonly;
      aObj.AddElement(E);
      end;
    While CurrentToken in [tjsComma,tjsSemicolon] do
       GetNextToken;
    end;
  Expect(tjsCurlyBraceClose);
end;


function TJSParser.ParseClassStatement(isAmbient : Boolean; IsAbstract : Boolean = False): TJSClassStatement;
begin
  Result:=TJSClassStatement(CreateElement(TJSClassStatement));
  try
    Result.Decl:=ParseClassDeclaration(isAmbient,IsAbstract);
  except
    FreeElement(Result);
    Raise;
  end;
end;

function TJSParser.ParseClassExpression: TJSClassDeclaration;

Var
  Oni,olhs: Boolean;
  C : TJSClassDeclaration;
  aName : {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString;
  aType : TJSTypeDef;

begin
  {$ifdef debugparser} Writeln('>>> ParseClassExpression');{$endif}
  oni:=NoIn;
  olhs:=IsLHS;
  C:=Nil;
  aType:=Nil;
  try
    NoIn:=False;
    IsLHS:=False;
    C:=TJSClassDeclaration(CreateElement(TJSClassDeclaration));
    try
      Consume(tjsClass);
      if CurrentToken=tjsIdentifier then
        aName:=ParseIdentifier;
      if CurrentToken=tjsExtends then
        begin
        Consume(tjsExtends);
        aType:=ParseTypeDef;
        end;
      Consume(tjsCurlyBraceOpen);
      C.Name:=aName;
      C.Extends:=aType;
      C.Members:=ParseClassBody;
      Consume(tjsCurlyBraceClose);
      Result:=C;
    except
      FreeElement(C);
      Raise;
    end;
  finally
    NoIn  := oni;
    IsLHS := olhs;
  end;
  {$ifdef debugparser} Writeln('<<< ParseClassExpression');{$endif}
end;

function TJSParser.ParseModuleDeclaration: TJSModuleDeclaration;
// On entry we're on module keyword
// on exit, we're after closing }

Var
  aName : {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString;

begin
  Consume(tjsIdentifier);
  if CurrentToken=tjsIdentifier then
    begin
    aName:=CurrentTokenString;
    consume(tjsIdentifier);
    end
  else  
    begin
    Expect({$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsString);
    aname:=CurrentTokenString;
    Consume({$IFDEF FPC_DOTTEDUNITS}Js.Token{$ELSE}jsToken{$ENDIF}.tjsString);
    end;
  Result:=TJSModuleDeclaration(CreateElement(TJSModuleDeclaration));
  try
    Result.Name:=aName;
    Expect(tjsCurlyBraceOpen);
    Consume(tjsCurlyBraceOpen);
    Result.Members:=ParseModuleBody;
    Consume(tjsCurlyBraceClose);
  except
    FreeElement(Result);
    Raise;
  end;
end;



function TJSParser.ParseInterfaceDeclarationStatement: TJSInterfaceStatement;

begin
  Result:=TJSInterfaceStatement(CreateElement(TJSInterfaceStatement));
  try
    Result.Decl:=ParseInterfaceDeclaration;
  except
    FreeElement(Result);
    Raise;
  end;
end;

function TJSParser.ParseInterfaceDeclaration: TJSInterfaceDeclaration;

Var
  aName : {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString;
  PT : TJSElementNodes;

begin
  Consume(tjsIdentifier);
  aName:=ParseIdentifier;
  Result:=TJSInterfaceDeclaration(CreateElement(TJSInterfaceDeclaration));
  try
    Result.Name:=aName;
    if (CurrentToken=tjsLt) then
      begin
      Result.TypeParams:=TJSElementNodes.Create(TJSElementNode);
      ParseGenericParamList(Result.TypeParams);
      Consume(tjsGT);
      end;
    if (CurrentToken=tjsExtends) then
       begin
       Consume(tjsExtends);
       While CurrentToken<>tjsCurlyBraceOpen do
         begin
         PT:=Nil;
         aName:=ParseIdentifier;
         if CurrentToken=tjsLT then
           begin
           PT:=TJSElementNodes.Create(TJSElementNode);
           ParseGenericParamList(PT);
           Consume(tjsGT);
           FreeElement(PT);
           end;
         Result.AddExtends(aName);
         if CurrentToken=tjsComma then
           Consume(tjsComma);
         end;
       end;
    ParseObjectBody(Result);
    Consume(tjsCurlyBraceClose);
  except
    FreeElement(Result);
    Raise;
  end;
end;



function TJSParser.ParseModuleBody: TJSSourceElements;

begin
  {$ifdef debugparser} Writeln('>>> Entering ParseModuleBody');{$endif}
  Result:=Self.ParseSourceElements(stModule,IsTypeScript);
  {$ifdef debugparser} Writeln('<<< Exiting ParseModuleBody');{$endif}
end;

function TJSParser.ParseNamespaceDeclaration(IsAmbient : Boolean): TJSNamespaceDeclaration;
// On entry we're on namespace or global keyword
// on exit, we're after closing }

Var
  aName : {$IFDEF FPC_DOTTEDUNITS}Js.Base{$ELSE}jsBase{$ENDIF}.TJSString;
  IsGlobal : Boolean;
  
begin
  IsGlobal:=IsIdentifier('global');
  Consume(tjsIdentifier); // namespace
  if not IsGlobal then
    aname:=ParseIdentifier;
  Result:=TJSNamespaceDeclaration(CreateElement(TJSNamespaceDeclaration));
  try
    Result.IsGlobal:=IsGLobal;
    Result.Name:=aName;
    Consume(tjsCurlyBraceOpen);
    Result.Members:=ParseNamespaceBody(IsAmbient);
    Consume(tjsCurlyBraceClose);
  except
    FreeElement(Result);
    Raise;
  end;

end;

function TJSParser.ParseNamespaceBody(IsAmbient : Boolean): TJSSourceElements;
begin
  {$ifdef debugparser} Writeln('>>> Entering ParseModuleBody');{$endif}
  Result:=Self.ParseSourceElements(stNamespace,IsAmbient);
  {$ifdef debugparser} Writeln('<<< Exiting ParseModuleBody');{$endif}
end;

Function TJSParser.ParseProgram: TJSFunctionDeclarationStatement;

Var
  B : TJSElement;
begin
  {$ifdef debugparser} Writeln('>>> Entering FunctionDeclarationStatement');{$endif}
  B:=Parse;
  If Not (B is TJSFunctionBody) then
    Error('Parse did not result in functionbody');
  Result:=TJSFunctionDeclarationStatement(CreateElement(TJSFunctionDeclarationStatement));
  Result.AFunction:=TJSFuncDef.Create;
  Result.AFunction.Body:=TJSFunctionBody(B);
  {$ifdef debugparser} Writeln('<<< Exiting FunctionDeclarationStatement');{$endif}
end;

Function TJSParser.Parse: TJSElement;

Var
  Body : TJSElement;

begin
  {$ifdef debugparser} Writeln('>>> Parse');{$endif}
  Result:=Nil;
  CheckParser;
  GetNextToken;
  Body:=ParseFunctionBody;
  Result:=Body;
  try
    if (CurrentToken<>tjsEOF) then
      begin
      if (CurrentToken=tjsCurlyBraceClose) then
        Error(SErrUnmatchedCurlyBrace)
      else if (CurrentToken=tjsBraceClose) then
        Error(SerrUnmatchedBrace)
      else if (CurrentToken=tjsSquaredBraceClose) then
        Error(SerrUnmatchedSquareBrace);
      Error(SErrUnexpectedToken,[CurrentTokenString]);
      end;
    If (Body is TJSFunctionBody) then
      TJSFunctionBody(Body).isProgram:=True;
  except
    FreeElement(Result);
    Raise;
  end;
  {$ifdef debugparser} Writeln('<<< Parse');{$endif}
end;


end.

