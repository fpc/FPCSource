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
unit jsparser;

{ $define debugparser}
{$mode objfpc}{$H+}
{$inline on}

interface

uses
  Classes, SysUtils, jsscanner, jstree, jstoken, jsbase;

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
  TECMAVersion = jsScanner.TECMAVersion;
  TScopeType = (stFunction,stClass,stModule,stNamespace);
  TFunctionFlag = (ffAmbient);
  TFunctionFlags = Set of TFunctionFlag;
  EJSParser = Class(Exception);

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
    FCurrentString : JSBase.TJSString;
    FFreeScanner : Boolean;
    FCurrentVars : TJSElementNodes;
    FPeekToken: TJSToken;
    FPeekTokenString: JSBase.TJSString;
    FLabelSets,
    FCurrentLabelSet:TJSLabelSet;
    FLabels : TJSLabel;
    // these check that current token is identifier with specific value:
    // as, from, get, of, set, target
    function GetIsTypeScript: Boolean;
    function  IdentifierIsLiteral(aValue : String) : Boolean;
    Procedure CheckIdentifierLiteral(aValue : String);
    function ConsumeIdentifierLiteral(aValue: String): TJSToken;
    function CheckSemiColonInsert(aToken: TJSToken; Consume: Boolean): Boolean;
    function EnterLabel(ALabelName: String): TJSLabel;
    // Check that current token is aToken
    procedure Expect(aToken: TJSToken);
    // Check that current token is aToken and goto next token.
    procedure Consume(aToken: TJSToken; AllowSemicolonInsert : Boolean = False);
    procedure FreeCurrentLabelSet;
    function GetVersion: TECMAVersion;
    procedure LeaveLabel;
    function LookupLabel(ALabelName: String; Kind: TJSToken): TJSLabel;
    function ParseAdditiveExpression: TJSElement;
    procedure ParseAliasElements(aElements: TJSAliasElements);
    function ParseArguments: TJSarguments;
    function ParseArrayLiteral: TJSElement;
    function ParseArrowFunctionTypeDef(aArgName : jsBase.TJSString): TJSArrowFunctionTypeDef;
    function ParseAssignmentExpression: TJSElement;
    function ParseBitwiseAndExpression: TJSElement;
    function ParseBitwiseORExpression: TJSElement;
    function ParseBitwiseXORExpression: TJSElement;
    function ParseBlock: TJSElement;
    function ParseBreakStatement: TJSElement;
    function ParseConditionalExpression: TJSElement;
    function ParseContinueStatement: TJSElement;
    function ParseEmptyStatement: TJSElement;
    function ParseEnumDeclarationStatement: TJSElement;
    function ParseEqualityExpression: TJSElement;
    function ParseExpression: TJSElement;
    function ParseExpressionStatement: TJSElement;
    procedure ParseFormalParameterList(aParams : TJSTypedParams);
    function ParseFunctionDeclaration(aFlags : TFunctionFlags): TJSFunctionDeclarationStatement;
    function ParseFunctionExpression(IsGenerator : Boolean = False): TJSFunctionDeclarationStatement;
    function ParseFunctionStatement: TJSElement;
    function ParseFunctionBody: TJSFunctionBody;
    procedure ParseGenericParamList(aList: TJSElementNodes);
    function ParseClassBody: TJSSourceElements; inline;
    function ParseClassDeclaration : TJSClassDeclaration;
    function ParseClassStatement : TJSClassStatement;
    function ParseClassExpression: TJSClassDeclaration;
    function ParseInterfaceDeclaration: TJSInterfaceDeclaration;
    function ParseInterfaceDeclarationStatement: TJSInterfaceStatement;
    function ParseModuleDeclaration : TJSModuleDeclaration;
    function ParseModuleBody: TJSSourceElements; inline;
    function ParseNamespaceDeclaration : TJSNamespaceDeclaration;
    function ParseNamespaceBody: TJSSourceElements; inline;
    function ParseIdentifier: jsbase.TJSString;
    function ParseIfStatement: TJSElement;
    function ParseImportStatement: TJSElement;
    function ParseExportStatement: TJSElement;
    function ParseIterationStatement: TJSElement;
    function ParseLabeledStatement: TJSElement;
    function ParseLeftHandSideExpression: TJSElement;
    function ParseLiteral: TJSElement;
    function ParseLogicalAndExpression: TJSElement;
    function ParseLogicalORExpression: TJSElement;
    function ParseMemberExpression: TJSElement;
    function ParseMultiplicativeExpression: TJSElement;
    function ParseNumericLiteral: TJSElement;
    procedure ParseObjectBody(aObj: TJSObjectTypeDef);
    function ParseObjectTypeDef: TJSObjectTypeDef;
    function ParseObjectLiteral: TJSElement;
    function ParseParenthesisedDef: TJSTypeDef;
    function ParsePostFixExpression: TJSElement;
    function ParsePrimaryExpression: TJSElement;
    function ParseRegularExpressionLiteral: TJSElement;
    function ParseRelationalExpression: TJSElement;
    function ParseReturnStatement: TJSElement;
    function ParseShiftExpression: TJSElement;
    function ParseStatement: TJSElement;
    function ParseStatementList: TJSElement;
    function ParseStringLiteral: TJSElement;
    function ParseSwitchStatement: TJSElement;
    function ParseThrowStatement: TJSElement;
    function ParseTryStatement: TJSElement;
    function ParseTypeDeclarationStatement : TJSElement;
    procedure ParseTypeList(aList: TJSElementNodes; aTerminator: TJSToken);
    function ParseTypeRef : TJSTypeDef;
    function ParseUnaryExpression: TJSElement;
    function ParseDebuggerStatement: TJSElement;
    function ParseVariableDeclaration(aVarType : TJSVarType = vtVar): TJSElement;
    function ParseVariableDeclarationList(aVarType : TJSVarType = vtVar): TJSElement;
    function ParseVariableStatement(aVarType : TJSVarType = vtVar): TJSElement;
    function ParseWithStatement: TJSElement;
  Protected
    Procedure CheckParser;
    Function CurrentLabelSet : TJSLabelSet;
    function CurSource: String;
    Function CurLine : Integer;
    Function CurPos : Integer;
    Function CreateElement(AElementClass : TJSElementClass)  : TJSElement; virtual;
    Procedure Error(Msg : String);
    Procedure Error(Fmt : String; Args : Array of const);
    // Parse functions
    Function IsIdentifier(const aIdentifier : String) : Boolean; inline;
    function ParseSourceElements(ScopeType : TScopeType = stFunction): TJSSourceElements;
    Property FunctionDepth : Integer Read FFunctionDepth Write FFunctionDepth;
    Property NoIn : Boolean Read FNoIn Write FNoIn;
    Property IsLHS : Boolean Read FIsLHS Write FIsLHS;
  Public
    Constructor Create(AInput: TStream; aVersion : TECMAVersion = ecma5; aIsTypeScript : Boolean = false);
    // Scanner has version
    Constructor Create(AScanner : TJSScanner);
    Destructor Destroy; override;
    Function Parse : TJSElement;
    Function ParseProgram : TJSFunctionDeclarationStatement;
    Function CurrentToken : TJSToken;
    Function CurrentTokenString : jsBase.TJSString;
    Function GetNextToken : TJSToken;
    Function PeekNextToken : TJSToken;
    Function IsEndOfLine : Boolean;
    Property ECMAVersion : TECMAVersion Read GetVersion;
    Property IsTypeScript : Boolean Read GetIsTypeScript;
  end;

implementation

uses typinfo;

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
  SErrTypeExpected = 'Type expected';
  SErrUnionWithoutType = 'Union type without type name';
  SErrIntersectionWithoutType = 'Intersection type without type name';
  SErrArrayWithoutType = 'Array type without type name';
  SErrGenericWithoutType = 'Generic type without base type';
  SErrGenericArray1Element = 'Generic array type can have only 1 element';
  SErrorInTypeDef = 'Error in type definition. Expected %s';
  SArrowFunction = 'Arrow function';
  SBraceClose = 'Closing brace ")"';

{ TJSScanner }

Function TJSParser.CurrentToken: TJSToken;

begin
  Result:=FCurrent;
end;

Function TJSParser.CurrentTokenString: JSBase.TJSString;
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
    FCurrentString:=FScanner.CurTokenString;
    end;
  Result:=FCurrent;
  {$ifdef debugparser}Writeln('GetNextToken (',FScanner.CurLine,',',FScanner.CurColumn,'): ',GetEnumName(TypeInfo(TJSToken),Ord(FCurrent)), ' As string: ',FCurrentString);{$endif debugparser}
end;

Function TJSParser.PeekNextToken: TJSToken;
begin
  If (FPeekToken=tjsUnknown) then
    begin
    FPeekToken:=FScanner.FetchToken;
    FPeekTokenString:=FScanner.CurTokenString;
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

function TJSParser.LookupLabel(ALabelName : String; Kind : TJSToken) :TJSLabel;

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

function TJSParser.EnterLabel(ALabelName : String) :TJSLabel;

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

Procedure TJSParser.Error(Msg: String);

Var
  ErrAt : String;

begin
  If Assigned(FScanner) then
    If FScanner.CurFilename<>'' then
      ErrAt:=Format('Error: file "%s" line %d, pos %d: ',[FScanner.CurFileName,FScanner.CurRow,FScanner.CurColumn])
    else
      ErrAt:=Format('Error: line %d, pos %d: ',[FScanner.Currow,FScanner.CurColumn]);
  Raise Exception.Create(ErrAt+Msg)
end;

Procedure TJSParser.Error(Fmt: String; Args: Array of const);
begin
  Error(Format(Fmt,Args));
end;

function TJSParser.IsIdentifier(const aIdentifier: String): Boolean;
begin
  Result:=(CurrentToken=jsToken.tjsIdentifier) and (CurrentTokenString=aIdentifier);
end;

constructor TJSParser.Create(AInput: TStream; aVersion: TECMAVersion; aIsTypeScript: Boolean);
begin
  FInput:=AInput;
  FCurrent:=TJSUnknown;
  FScanner:=TJSScanner.Create(FInput,aVersion);
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

function TJSParser.IdentifierIsLiteral(aValue: String): Boolean;
begin
  Result:=(CurrentToken=tjsIdentifier) and (CurrentTokenString=aValue);
end;

function TJSParser.GetIsTypeScript: Boolean;
begin
  Result:=FScanner.IsTypeScript;
end;

procedure TJSParser.CheckIdentifierLiteral(aValue: String);
begin
  if Not IdentifierIsLiteral(aValue) then
    Error(SErrExpectedButFound,[aValue,CurrentTokenString]);
end;

Function TJSParser.ConsumeIdentifierLiteral(aValue: String) : TJSToken;
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

function TJSParser.ParseIdentifier : JSBase.TJSString;

begin
  Result:='';
  Repeat
    Expect(tjsIdentifier);
    Result:=Result+CurrentTokenString;
    GetNextToken;
    If (CurrentToken=tjsDot) then
      begin
      If (Result<>'') then
         Result:=Result+'.';
      GetNextToken;
      end;
  until (CurrentToken<>tjsIdentifier);
end;

procedure TJSParser.ParseGenericParamList(aList : TJSElementNodes);

begin
  ParseTypeList(aList,jstoken.tjsGT);
end;

function TJSParser.ParseObjectTypeDef: TJSObjectTypeDef;

Var
  N : TJSObjectTypeDef;

begin
  N:=TJSObjectTypeDef(CreateElement(TJSObjectTypeDef));
  try
    Result:=N;
    ParseObjectBody(N);
  except
    Result.Free;
    Raise;
  end;
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
  aName : jsBase.TJSString;
  isOptional : Boolean;
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
    If (CurrentToken in [tjsIdentifier,jstoken.tjsString,jstoken.tjsnumber]) then
       begin
       aName:=CurrentTokenString;
       GetNextToken;
       end
    else if (CurrentToken<>tjsBraceOpen) then
       Error(SErrObjectElement,[CurrentTokenString]);
    isOptional:=(CurrentToken=tjsConditional);
    if isOPtional then
      GetNextToken;
    case CurrentToken of
      // PropertyName : PropertyType
      tjsColon:
        begin
        Consume(tjsColon);
        E:=TJSPropertyDeclaration(CreateElement(TJSPropertyDeclaration));
        E.Name:=aName;
        E.ElementType:=ParseTypeRef;
        end;
      // <T1> () : TypeName;
      // () : TypeName;
      tjsLT,
      tjsBraceOpen:
        begin
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
        FS:=ParseFunctionExpression(True);
        F.FuncDef:=FS.AFunction;
        FS.AFunction:=Nil;
        F.TypeParams:=TP;
        if Not Assigned(F.Funcdef.ResultType) then
          F.Funcdef.ResultType:=CreateAny;
        FS.Free;
        end;
      tjsComma,tjsSemicolon:
        begin
        // PropertyName ; Type is any
        E:=TJSPropertyDeclaration(CreateElement(TJSPropertyDeclaration));
        E.Name:=aName;
        E.ElementType:=CreateAny;
        end;
    else
      Raise EJSParser.CreateFmt(SErrExpectedColonBrace,[CurrentTokenString]);
    end;
    if Assigned(E) then
      begin
      E.Optional:=IsOptional;
      aObj.AddElement(E);
      end;
    While CurrentToken in [tjsComma,tjsSemicolon] do
       GetNextToken;
    end;
  Expect(tjsCurlyBraceClose);
end;



procedure TJSParser.ParseTypeList(aList : TJSElementNodes; aTerminator : TJSToken);

// Entry on < or [, exit on terminator

Var
  aSub : TJSTypeDef;

begin
  GetNextToken;
  While (CurrentToken<>aTerminator) do
    begin
    aSub:=ParseTypeRef;
    aList.AddNode.Node:=aSub;
    if CurrentToken=tjsComma then
      GetNextToken;
    end;
end;

Function TJSParser.ParseArrowFunctionTypeDef (aArgName : jsBase.TJSString) : TJSArrowFunctionTypeDef;

// On entry, we are on
// )  of a () => Type
// : from the first argument in a (argname : ) => Type
// On exit: first token after the result type

  Procedure ParseArgType(const aName : jsBase.TJSString);
  // On entry we are on :
  // On exit we are on , or )

  begin
    Consume(tjsColon);
    Result.aFunction.TypedParams.AddParam(aName).Node:=ParseTypeRef;
  end;

Var
  aName : jsBase.TJSString;

begin
  Result:=TJSArrowFunctionTypeDef(CreateElement(TJSArrowFunctionTypeDef));
  try
    If aArgName <> '' then
      begin
      ParseArgType(aArgName);
      end;
    While CurrentToken<>tjsBraceClose do
      begin
      if CurrentToken=tjsComma then
        Consume(tjscomma);
      aName:=ParseIdentifier;
      ParseArgType(aName);
      end;
    Consume(tjsBraceClose);
    Consume(tjsArrow);
    Result.aFunction.ResultType:=ParseTypeRef;
  except
    FreeAndNil(Result);
    Raise;
  end;
end;


Function TJSParser.ParseParenthesisedDef : TJSTypeDef;

// On entry, we're on (
// On exit we're after the type.

Var
  aDef : TJSTypeDef;
  aArgName : jsbase.TJSString;

begin
  Result:=nil;
  Consume(tjsBraceOpen);
  if CurrentToken=tjsBraceClose then
    begin
    // () => Type
    Result:=ParseArrowFunctionTypeDef('');
    end
  else
    begin
    aDef:=ParseTypeRef;
    // (Ident : ) => Type
    if (CurrentToken=tjsColon) then
      begin
      if (aDef is TJSTypeReference) then
        begin
        aArgName:=TJSTypeReference(aDef).Name;
        FreeAndNil(aDef);
        Result:=ParseArrowFunctionTypeDef(aArgName);
        end
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

Function TJSParser.ParseTypeRef : TJSTypeDef;

  Function ParseRef : TJSTypeReference;

  Var
    TypeOf,NeedNext : Boolean;
    aName : jsbase.TJSString;

  begin
    Result:=TJSTypeReference(CreateElement(TJSTypeReference));
    try
      Result.IsTypeOf:=(CurrentToken=tjsTYPEOF);
      if Result.IsTypeof then
        begin
        GetNextToken;
        Expect(tjsIdentifier);
        end;
      NeedNext:=True;
      Case CurrentToken of
        tjsVoid : aName:='void';
        tjsThis : aName:='this';
      else
        aName:=ParseIdentifier;
        needNext:=False;
      end;
      Result.Name:=aName;
      if NeedNext then
        GetNextToken;
    except
      Result.Free;
      Raise;
    end;
  end;

// On entry, first token of the type definition
// On exit, we are on the first token after the type definition

Const
   StructTokens = [jsToken.tjsOR, jsToken.tjsLT, jsToken.tjsSQuaredBraceOpen];

Var
  Done : Boolean;
  aType,aSub : TJSTypeDef;
  aStruct : TJSStructuredTypeDef;

begin
  aStruct:=Nil;
  aType:=Nil;
  Result:=Nil;
  Done:=False;
  try
    Repeat
      Case CurrentToken of
        tjsTYPEOF,
        tjsVoid,
        tjsThis,
        tjsIdentifier :
          begin
          aType:=ParseRef;
          end;
        tjsOR :
          begin
          if aType=Nil then
            Error(SErrUnionWithoutType);
          aStruct:=TJSUnionTypeDef(CreateElement(TJSUnionTypeDef));
          aStruct.AddValue(aType);
          Repeat
            GetNextToken;
            aStruct.AddValue(ParseTypeRef());
          Until CurrentToken<>tjsOR;
          aType:=aStruct;
          end;
        tjsAnd :
          begin
          if aType=Nil then
            Error(SErrIntersectionWithoutType);
          aStruct:=TJSIntersectionTypeDef(CreateElement(TJSIntersectionTypeDef));
          aStruct.AddValue(aType);
          Repeat
            GetNextToken;
            aStruct.AddValue(ParseTypeRef());
          Until CurrentToken<>tjsOR;
          aType:=aStruct;
          end;
        tjsBraceOpen :
          begin
          AType:=ParseParenthesisedDef
          end;
        tjsCurlyBraceOpen :
          begin
          AType:=ParseObjectTypeDef;
          Consume(tjsCurlyBraceClose);
          end;
        tjsSQuaredBraceOpen :
          begin
          if aType<>Nil then
            begin
            aSub:=aType;
            aType:=TJSArrayTypeDef(CreateElement(TJSArrayTypeDef));
            TJSArrayTypeDef(aType).BaseType:=aSub;
            GetNextToken;
            end
          else
            begin // no earlier type yet, so tuple
            aStruct:=TJSTupleTypeDef(CreateElement(TJSTupleTypeDef));
            ParseTypeList(aStruct.Values,tjsSQuaredBraceClose);
            aType:=aStruct;
            end;
          Consume(tjsSQuaredBraceClose);
          end;
        tjsLT :
          begin
          if aType=Nil then
            Error(SErrGenericWithoutType);
          if (aType is TJSTypeReference) and (TJSTypeReference(aType).Name='Array') then
            begin
            FreeAndNil(AType);
            aType:=TJSArrayTypeDef(CreateElement(TJSArrayTypeDef));
            consume(tjsLT);
            TJSArrayTypeDef(aType).BaseType:=ParseTypeRef();
            // GetNextToken;
            consume(tjsGT);
            end
          else
            begin
            aStruct:=Nil;
            aStruct:=TJSGenericTypeRef(CreateElement(TJSGenericTypeRef));
            TJSGenericTypeRef(aStruct).BaseType:=aType;
            aType:=aStruct;
            ParseGenericParamList(aStruct.Values);
            consume(tjsGT);
            end;
          end;
      else
        done:=true;
      end;
      if Currenttoken=tjsBraceClose then
        Done:=True;
      if (not Done) and (PeekNextToken in StructTokens) then
          GetNextToken;
    until done;

    if (Result=Nil) then
      if (aType<>Nil) then
        Result:=aType
      else
        Error(SErrTypeExpected)
  except
    aType.Free;
    aStruct.Free;
    Raise;
  end;
end;

Procedure TJSParser.ParseFormalParameterList(aParams : TJSTypedParams);

Var
  P : TJSTypedParam;

begin
  While (CurrentToken=tjsIdentifier) do
    begin
    Expect(tjsIdentifier);
    P:=aParams.AddParam(CurrentTokenString);
    GetNextToken;
    if IsTypeScript then
      begin
      Expect(tjsCOLON);
      Consume(tjsCOLON);
      P.Node:=ParseTypeRef;
      end;
    If (CurrentToken=tjsComma) then
       GetNextToken;
    end;
end;


function TJSParser.ParseFunctionDeclaration(aFlags : TFunctionFlags) : TJSFunctionDeclarationStatement;

Var
  Id : jsBase.TJSString;
  D : TJSFuncDef;
  isGenerator : Boolean;

begin
  {$ifdef debugparser}  Writeln('>>> Entering ParseFunctionDeclaration');{$endif debugparser}
  Consume(tjsFunction);
  isGenerator:=(CurrentToken=tjsMUL);
  if IsGenerator then
    Consume(tjsMul);
  ID:=ParseIdentifier;
  Consume(tjsBraceOpen);
  Result:=TJSFunctionDeclarationStatement(CreateElement(TJSFunctionDeclarationStatement));
  Result.AFunction:=TJSFuncDef.Create;
  D:=Result.AFunction;
  try
    D.Name:=ID;
    ParseFormalParameterList(D.TypedParams);
    D.UpdateParams;
    Consume(tjsBraceClose);
    if IsTypeScript then
      begin
      consume(tjsColon);
      D.ResultType:=ParseTypeRef;
      GetNextToken;
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
        FreeAndNil(SL);
        Raise;
      end;
      end;
  except
    FreeAndNil(E);
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
    FreeAndNil(Result);
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
      If (CurrentToken in [tjsIdentifier,jstoken.tjsString,jstoken.tjsnumber]) then
         begin
         E:=N.Elements.AddElement;
         E.Name:=CurrentTokenString;
         GetNextToken;
         end
      else If (CurrentToken=tjsMul) and (EcmaVersion>=MinGeneratorVersion) then
         begin
         E:=N.Elements.AddElement;
         E.Expr:= ParseFunctionExpression(True);
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
    FreeAndNil(Result);
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
    Val(CurrentTokenString,D,I);
    If (I>0) then
      Error(SErrInvalidnumber,[CurrentTokenString]);
    L:=TJSLiteral(CreateElement(TJSLiteral));
    GetNextToken;
    L.Value.AsNumber:=D;
    Result:=L;
  except
    FreeAndNil(Result);
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
    FreeAndNil(Result);
    Raise;
  end;
end;

function TJSParser.ParseRegularExpressionLiteral: TJSElement;

Var
  S,pa,fl : String;
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
    FreeAndNil(Result);
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
    jstoken.tjsNumber : Result:=ParseNumericLiteral;
    jstoken.tjsString : Result:=ParseStringLiteral;
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
      tjsidentifier:
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
    FreeAndNil(Result);
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
    tjsFunction : Result:=ParseFunctionExpression();
    tjsNew      : begin
                  GetNextToken;
                  N:=TJSNewMemberExpression(CreateElement(TJSNewMemberExpression));
                  try
                    Result:=N;
                    N.MExpr:=ParseMemberExpression();
                    if (CurrentToken=tjsBraceOpen) then
                      N.Args:=ParseArguments;
                  except
                    FreeAndNil(N);
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
    FreeAndNil(Result);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('Exit ParseMemberExpression');{$endif debugparser}
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
    FreeAndNil(Result);
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
    tjsFunction : Result:=ParseFunctionExpression;
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
    FreeAndNil(Result);
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
    freeAndNil(Result);
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
    FreeAndNil(Result);
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
    FreeAndNil(Result);
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
    FreeAndNil(Result);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('Exit ParseMultiplicativeExpression');{$endif debugparser}
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
    FreeAndNil(Result);
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
    FreeAndNil(Result);
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
    FreeAndNil(Result);
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
    FreeAndNil(Result);
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
    FreeAndNil(Result);
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
    FreeAndNil(Result);
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
      FreeAndNil(Result);
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
    FreeAndNil(Result);
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
    FreeAndNil(Result);
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
    FreeandNil(Result);
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
    FreeAndNil(Result);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('Exit ParseAssignmentExpression');{$endif debugparser}
end;

function TJSParser.ParseVariableDeclaration(aVarType : TJSVarType = vtVar): TJSElement;

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
      V.Typed:=ParseTypeRef;
      end;
    if (CurrentToken=tjsAssign) then
      begin
      GetNextToken;
      V.Init:=ParseAssignmentExpression;
      end;
    Result:=V;
    FCurrentVars.AddNode.Node:=Result;
  except
    FreeAndNil(V);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('Exit ParseVariableDeclaration');{$endif debugparser}
end;

function TJSParser.ParseVariableDeclarationList(aVarType : TJSVarType = vtVar): TJSElement;

Var
  E,N : TJSElement;
  L : TJSVariableDeclarationList;
  SL : TJSVariableDeclarationList absolute N;


begin
  {$ifdef debugparser}  Writeln('ParseVariableDeclarationList entry');{$endif debugparser}
  E:=ParseVariableDeclaration(aVarType);
  If (CurrentToken<>tjsComma) then
    Result:=E
  else
    begin
    L:=TJSVariableDeclarationList(CreateElement(TJSVariableDeclarationList));
    Result:=L;
    try
      Consume(tjsComma);
      N:=ParseVariableDeclarationList();
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
      FreeAndNil(Result);
      Raise;
    end;
    end;
  {$ifdef debugparser}  Writeln('ParseVariableDeclarationList exit');{$endif debugparser}
end;

function TJSParser.ParseVariableStatement(aVarType : TJSVarType = vtVar): TJSElement;

Const
   InitialTokens : Array[TJSVarType] of TJSToken = (tjsVar,tjsLet,tjsConst);

Var
  V : TJSVariableStatement;
  aType : TJSTypeDef;

begin
  {$ifdef debugparser}  Writeln('ParseVariableStatement entry');{$endif debugparser}
  Result:=Nil;
  aType:=Nil;
  Consume(InitialTokens[aVarType]);
  Result:=ParseVariableDeclarationList(aVarType);
  try
    Consume(tjsSemicolon,true);
    V:=TJSVariableStatement(CreateElement(TJSVariableStatement));
    V.varType:=aVarType;
    V.A:=Result;

    Result:=V;
  except
    FreeAndNil(Result);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('ParseVariableStatement exit');{$endif debugparser}
end;

function TJSParser.ParseEmptyStatement : TJSElement;

begin
  Consume(tjsSemiColon,true);
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
    FreeAndNil(C);
    FreeAndNil(BTrue);
    FreeAndNil(BFalse);
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
    FreeAndNil(N);
    FreeAndNil(Result);
    Raise;
  end;
end;

Procedure TJSParser.ParseAliasElements(aElements : TJSAliasElements);
// Parse { N [as M] }. On entry, must be on {, on exit curtoken is token after }
Var
  aName,aAlias : String;
begin
  Consume(tjsCurlyBraceOpen);
  if (CurrentToken<>tjsCurlyBraceClose) then
    begin
    Repeat
      Expect(tjsIdentifier);
      aName:=CurrentTokenString;
      aAlias:='';
      Consume(tjsIdentifier);
      if IdentifierIsLiteral('as') then
        begin
        Consume(tjsIdentifier);
        Expect(tjsIdentifier);
        aAlias:=CurrentTokenString;
        Consume(tjsIdentifier);
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
begin
  aExpectMore:=True;
  Consume(tjsImport);
  Imp:=TJSImportStatement(CreateElement(TJSImportStatement));
  try
    Result:=Imp;
    // Just module name
    if CurrentToken = jstoken.tjsString then
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
        end
    else
      if aExpectMore then
        Error(SErrExpectedMulOrCurlyBrace,[CurrentTokenString]);
    end;
    ConsumeIdentifierLiteral('from');
    Expect(jstoken.tjsString);
    Imp.ModuleName:=CurrentTokenString;
    Consume(jstoken.tjsString);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

function TJSParser.ParseExportStatement: TJSElement;
Var
  Exp : TJSExportStatement;
  async, aExpectFrom : Boolean;
  F : TJSFunctionDeclarationStatement;

begin
  aExpectFrom:=True;
  Consume(tjsExport);
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
      tjsConst:
        Exp.Declaration:=ParseVariableStatement(vtConst);
      tjsLet:
        Exp.Declaration:=ParseVariableStatement(vtLet);
      tjsFunction :
        Exp.Declaration:=ParseFunctionDeclaration([]);
      tjsClass :
        Exp.Declaration:=ParseClassDeclaration;
      tjsDEFAULT:
        begin
        Exp.IsDefault:=True;
        aExpectFrom:=False;
        Consume(tjsDefault);
        async:=IdentifierIsLiteral('async');
        if Async then
          GetNextToken;
        case CurrentToken of
          tjsFunction :
            begin
            F:=ParseFunctionDeclaration([]);
            F.AFunction.IsAsync:=async;
            Exp.Declaration:=F;
            end;
          tjsClass : Exp.Declaration:=ParseClassDeclaration;
        else
          Exp.Declaration:=ParseAssignmentExpression;
        end;
        end;
    else
      Error(SErrExpectedMulOrCurlyBraceOrDefault,[CurrentTokenString]);
    end;
    if aExpectFrom and IdentifierIsLiteral('from') then
      begin
      ConsumeIdentifierLiteral('from');
      Expect(jstoken.tjsString);
      Exp.ModuleName:=CurrentTokenString;
      Consume(jstoken.tjsString);
      end;
  except
    FreeAndNil(Result);
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
    FreeAndNil(C);
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
    FreeAndNil(B);
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
    FreeAndNil(R);
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
    FreeAndNil(W);
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
    FreeAndNil(N);
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
    FreeAndNil(TS);
    Raise;
  end;
end;

function TJSParser.ParseTryStatement : TJSElement;

Var
  BO,BC,BF : TJSElement;
  Id : jstree.TJSString;
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
    FreeAndNil(Bo);
    FreeAndNil(BC);
    FreeAndNil(BF);
    FreeAndNil(Result);
    Raise;
  end;

end;

function TJSParser.ParseEnumDeclarationStatement: TJSElement;
// We are on the 'enum' identifier on entry, last token of type on exit

Var
  aES : TJSEnumStatement;
  aDecl : TJSEnumDeclaration;

begin
  GetNextToken; // Skip 'enum'
  aES:=TJSEnumStatement(CreateElement(TJSEnumStatement));
  Result:=aES;
  try
    aES.EnumDecl:=TJSEnumDeclaration(CreateElement(TJSEnumDeclaration));
    aDecl:=TJSEnumDeclaration(aES.EnumDecl);
    aDecl.Name:=ParseIdentifier;
    consume(tjsCurlyBraceOpen);
    aDecl.EnumDef:=TJSEnumTypeDef(CreateElement(TJSEnumTypeDef));
    While not (CurrentToken=tjsCurlyBraceClose) do
      begin
      if (CurrentToken=tjsComma) then
        Consume(tjsComma);
      aDecl.EnumDef.AddName(ParseIdentifier);
      end;
    Consume(tjsCurlyBraceClose);
  except
    Result.Free;
    Raise;
  end;
end;


function TJSParser.ParseTypeDeclarationStatement: TJSElement;
// We are on the 'type' identifier on entry, last token of type on exit

Var
  aTS : TJSTypeStatement;
  aTypeDecl : TJSTypeDeclaration;

begin
  GetNextToken; // Skip 'type'
  aTS:=TJSTypeStatement(CreateElement(TJSTypeStatement));
  Result:=aTS;
  try
    aTS.TypeDecl:=TJSTypeDeclaration(CreateElement(TJSTypeDeclaration));
    aTypeDecl:=TJSTypeDeclaration(aTS.TypeDecl);
    aTypeDecl.Name:=ParseIdentifier;
    if (CurrentToken=tjsLT) then
      begin
      aTypeDecl.TypeParams:=TJSElementNodes.Create(TJSElementNode);
      ParseGenericParamList(aTypeDecl.TypeParams);
      Consume(tjsGT);
      end;
    Consume(tjsAssign);
    aTypeDecl.TypeDef:=ParseTypeRef;
  except
    Result.Free;
    Raise;
  end;
end;

function TJSParser.ParseFunctionExpression(IsGenerator: Boolean): TJSFunctionDeclarationStatement;

Var
  Oni,olhs: Boolean;
  F : TJSFunctionDeclarationStatement;
  N : jsBase.TJSString;

begin
  {$ifdef debugparser} Writeln('>>> ParseFunctionExpression');{$endif}
  oni:=NoIn;
  olhs:=IsLHS;
  F:=Nil;
  try
    NoIn:=False;
    IsLHS:=False;
    F:=TJSFunctionDeclarationStatement(CreateElement(TJSFunctionDeclarationStatement));
    try
      if not IsGenerator then
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
      Consume(tjsBraceOpen);
      F.AFunction:= TJSFuncDef.Create;
      ParseFormalParameterList(F.AFunction.TypedParams);
      Consume(tjsBraceClose);
      if CurrentToken=tjsColon then
        begin
        Consume(tjsColon);
        F.AFunction.ResultType:=ParseTypeRef;
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
      FreeAndNil(F);
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
    F:=ParseFunctionExpression;
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
    FreeAndNil(F);
    FreeAndNil(I);
    FreeAndNil(A);
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
    FreeAndNil(LS);
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
    FreeAndNil(Result);
    Raise;
  end;
  {$ifdef debugparser}  Writeln('Exit ParseExpression');{$endif debugparser}
end;

function TJSParser.ParseStatement : TJSElement;

begin
  {$ifdef debugparser} Writeln('>>> Parsestatement');{$endif}
  Result:=Nil;
  Case CurrentToken of
    tjsCurlyBraceOpen :
      Result:=ParseBlock;
    tjsLet:
      Result:=ParseVariableStatement(vtLet);
    tjsConst:
      Result:=ParseVariableStatement(vtConst);
    tjsVar:
      Result:=ParseVariableStatement(vtVar);
    tjsSemicolon:
      Result:=ParseEmptyStatement;
    tjsIf:
      Result:=ParseIfStatement;
    tjsDo,tjsWhile,tjsFor:
      Result:=ParseIterationStatement;
    tjsContinue:
      Result:=ParseContinueStatement;
    tjsImport:
      Result:=ParseImportStatement;
    tjsExport:
      Result:=ParseExportStatement;
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
      else if IsTypeScript and IsIdentifier('type') then
        Result:=ParseTypeDeclarationStatement
      else
        Result:=ParseExpressionStatement;
  else
    Result:=ParseExpressionStatement;
  end;
  {$ifdef debugparser} If Assigned(Result) then Writeln('<<< Parsestatement ',Result.ClassName) else Writeln('<<< Parsestatement (null');{$endif}
end;

function TJSParser.ParseSourceElements (ScopeType : TScopeType = stFunction): TJSSourceElements;

Const
  StatementTokens = [tjsNULL, tjsTRUE, tjsFALSE,
      tjsAWait, tjsTHIS, tjsIdentifier,jstoken.tjsSTRING,jstoken.tjsNUMBER,
      tjsBraceOpen,tjsCurlyBraceOpen,tjsSquaredBraceOpen,
      tjsLet, tjsConst, tjsDebugger, tjsImport, tjsExport,
      tjsNew,tjsDelete,tjsVoid,tjsTypeOf,
      tjsPlusPlus,tjsMinusMinus,
      tjsPlus,tjsMinus,tjsNot,tjsNE,tjsSNE,tjsSemicolon,
      tjsVAR,tjsIF,tjsDO,tjsWHILE,tjsFOR,jstoken.tjsCONTINUE,jstoken.tjsBREAK,jstoken.tjsReturn,
      tjsWith,jstoken.tjsSWITCH,tjsThrow,TjsTry,tjsDIV,tjsDIVEQ, tjsEnum];

Var
  F : TJSFunctionDeclarationStatement;
  C : TJSClassDeclaration;
  E : TJSElement;
  Done : Boolean;
  VS : TJSElementNodes;
  isAmbient,isExport,aSync : Boolean;
  FF : TFunctionFlags;

  procedure DefaultParsing;

  begin
    if CurrentToken in StatementTokens then
       begin
       E:=Self.ParseStatement;
       Result.Statements.AddNode(IsAmbient).Node:=E;
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
        isAmbient:= (ScopeType in [stFunction,stModule]) and IsIdentifier('declare');
        isExport:= (ScopeType=stModule) and IsIdentifier('export');
        if IsAmbient then
          GetNextToken;
        aSync:= (ECMAVersion>=MinAsyncVersion) and IsIdentifier('async');
        if aSync then
          GetNextToken;
        Case CurrentToken of
        jstoken.tjsFunction:
          begin
          If (PeekNextToken<>tjsBraceOpen) then
            begin
            FF:=[];
            if isAmbient then
              FF:=[ffAmbient];
            F:=Self.ParseFunctionDeclaration(FF);
            F.AFunction.IsAsync:=aSync;
            Result.Functions.AddNode(IsAmbient,IsExport).Node:=F;
            end
          else
            begin
            {$ifdef debugparser} Writeln('Function expression detected');{$endif}
            E:=Self.ParseStatement;
            Result.Statements.AddNode(IsAmbient,IsExport).Node:=E;
            end;
          end;
        jstoken.tjsClass:
          begin
          E:=Self.ParseClassStatement;
          Result.Statements.AddNode(IsAmbient,IsExport).Node:=E;
          C:=TJSClassStatement(E).Decl;
          Result.Classes.AddNode(IsAmbient,IsExport).Node:=C;
          end;
        jstoken.tjsEnum:
          begin
          E:=Self.ParseEnumDeclarationStatement;
          Result.Statements.AddNode(IsAmbient,IsExport).Node:=E;
          Result.Enums.AddNode(IsAmbient,IsExport).Node:=TJSEnumStatement(E).EnumDecl;
          end;
        jsToken.tjsMul:
          begin
          if (ScopeType=stClass) then
            begin
            //
            end
          else
            DefaultParsing;
          end;
        else
          // else of Case
          if IsTypeScript and IsIdentifier('module') then
            begin
            E:=Self.ParseModuleDeclaration;
            Result.Modules.AddNode(IsAmbient,IsExport).Node:=E;
            end
          else if IsTypeScript and IsIdentifier('namespace') then
            begin
            E:=Self.ParseNamespaceDeclaration;
            Result.Namespaces.AddNode(IsAmbient,IsExport).Node:=E;
            end
          else if IsTypeScript and IsIdentifier('type') then
            begin
            E:=Self.ParseTypeDeclarationStatement;
            Result.Statements.AddNode(IsAmbient,IsExport).Node:=E;
            Result.Types.AddNode(IsAmbient,IsExport).Node:=TJStypeStatement(E).TypeDecl;
            end
          else if IsTypeScript and IsIdentifier('interface') then
            begin
            E:=Self.ParseInterfaceDeclarationStatement;
            Result.Statements.AddNode(IsAmbient,IsExport).Node:=E;
            Result.Types.AddNode(IsAmbient,IsExport).Node:=TJSInterfaceStatement(E).Decl;
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
    FreeAndNil(Result);
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
    FreeAndNil(Result);
    Raise;
  end;
  {$ifdef debugparser} Writeln('<<< Exiting FunctionBody');{$endif}
end;

function TJSParser.ParseClassBody: TJSSourceElements;

begin
  {$ifdef debugparser} Writeln('>>> Entering ParseClassBody');{$endif}
  Result:=Self.ParseSourceElements(stClass);
  {$ifdef debugparser} Writeln('<<< Exiting ParseClassBody');{$endif}
end;

function TJSParser.ParseClassDeclaration: TJSClassDeclaration;

Var
  aName,aExtends : jsBase.TJSString;

begin
  Consume(tjsClass);
  if CurrentToken=tjsIdentifier then
    aName:=ParseIdentifier;
  if CurrentToken=tjsExtends then
    begin
    Consume(tjsExtends);
    aExtends:=ParseIdentifier;
    end;
  Result:=TJSClassDeclaration(CreateElement(TJSClassDeclaration));
  try
    Consume(tjsCurlyBraceOpen);
    Result.Name:=aName;
    Result.Extends:=aExtends;
    Result.Members:=ParseClassBody;
    Consume(tjsCurlyBraceClose);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

function TJSParser.ParseClassStatement: TJSClassStatement;
begin
  Result:=TJSClassStatement(CreateElement(TJSClassStatement));
  try
    Result.Decl:=ParseClassDeclaration;
  except
    Result.Free;
    Raise;
  end;
end;

function TJSParser.ParseClassExpression: TJSClassDeclaration;

Var
  Oni,olhs: Boolean;
  C : TJSClassDeclaration;
  N : String;
  aName,aExtends : jsBase.TJSString;

begin
  {$ifdef debugparser} Writeln('>>> ParseClassExpression');{$endif}
  oni:=NoIn;
  olhs:=IsLHS;
  C:=Nil;
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
        aExtends:=ParseIdentifier;
        end;
      Consume(tjsCurlyBraceOpen);
      C.Name:=aName;
      C.Extends:=aExtends;
      C.Members:=ParseClassBody;
      Consume(tjsCurlyBraceClose);
      Result:=C;
    except
      FreeAndNil(C);
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
  aName : jsBase.TJSString;

begin
  Consume(tjsIdentifier);
  Expect(jstoken.tjsString);
  aname:=CurrentTokenString;
  Consume(jsToken.tjsString);
  Result:=TJSModuleDeclaration(CreateElement(TJSModuleDeclaration));
  try
    Result.Name:=aName;
    Expect(tjsCurlyBraceOpen);
    Consume(tjsCurlyBraceOpen);
    Result.Members:=ParseModuleBody;
    Consume(tjsCurlyBraceClose);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;



function TJSParser.ParseInterfaceDeclarationStatement: TJSInterfaceStatement;

begin
  Result:=TJSInterfaceStatement(CreateElement(TJSInterfaceStatement));
  try
    Result.Decl:=ParseInterfaceDeclaration;
  except
    Result.Free;
    Raise;
  end;
end;

function TJSParser.ParseInterfaceDeclaration: TJSInterfaceDeclaration;

Var
  aName : jsBase.TJSString;

begin
  Consume(tjsIdentifier);
  aName:=ParseIdentifier;
  Result:=TJSInterfaceDeclaration(CreateElement(TJSInterfaceDeclaration));
  try
    Result.Name:=aName;
    if (CurrentToken=tjsExtends) then
       begin
       Consume(tjsExtends);
       While CurrentToken<>tjsCurlyBraceOpen do
         begin
         aName:=ParseIdentifier;
         Result.AddExtends(aName);
         if CurrentToken=tjsComma then
           Consume(tjsComma);
         end;
       end;
    ParseObjectBody(Result);
    Consume(tjsCurlyBraceClose);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;



function TJSParser.ParseModuleBody: TJSSourceElements;

begin
  {$ifdef debugparser} Writeln('>>> Entering ParseModuleBody');{$endif}
  Result:=Self.ParseSourceElements(stModule);
  {$ifdef debugparser} Writeln('<<< Exiting ParseModuleBody');{$endif}
end;

function TJSParser.ParseNamespaceDeclaration: TJSNamespaceDeclaration;
// On entry we're on namespace keyword
// on exit, we're after closing }

Var
  aName : jsBase.TJSString;

begin
  Consume(tjsIdentifier); // namespace
  aname:=ParseIdentifier;
  Result:=TJSNamespaceDeclaration(CreateElement(TJSNamespaceDeclaration));
  try
    Result.Name:=aName;
    Consume(tjsCurlyBraceOpen);
    Result.Members:=ParseNamespaceBody;
    Consume(tjsCurlyBraceClose);
  except
    FreeAndNil(Result);
    Raise;
  end;

end;

function TJSParser.ParseNamespaceBody: TJSSourceElements;
begin
  {$ifdef debugparser} Writeln('>>> Entering ParseModuleBody');{$endif}
  Result:=Self.ParseSourceElements(stNamespace);
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
    FreeAndNil(Result);
    Raise;
  end;
  {$ifdef debugparser} Writeln('<<< Parse');{$endif}
end;


end.

