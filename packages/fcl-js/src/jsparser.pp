unit jsparser;

{ $define debugparser}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jsscanner, jstree, jstoken;

Const
   SEmptyLabel = '';

Type

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
    FCurrentString : String;
    FNextNewLine : Boolean;
    FNextBol : Boolean;
    FFreeScanner : Boolean;
    FCurrentVars : TJSElementNodes;
    FPeekToken: TJSToken;
    FPeekTokenString: String;
    FLabelSets,
    FCurrentLabelSet:TJSLabelSet;
    FLabels : TJSLabel;
    function CheckSemiColonInsert(aToken: TJSToken; Consume: Boolean): Boolean;
    function EnterLabel(ALabelName: String): TJSLabel;
    procedure Expect(aToken: TJSToken);
    procedure Consume(aToken: TJSToken; AllowSemicolonInsert : Boolean = False);
    procedure FreeCurrentLabelSet;
    procedure LeaveLabel;
    function LookupLabel(ALabelName: String; Kind: TJSToken): TJSLabel;
    function ParseAdditiveExpression: TJSElement;
    function ParseArguments: TJSarguments;
    function ParseArrayLiteral: TJSElement;
    function ParseAssignmentExpression: TJSElement;
    function ParseBitwiseAndExpression: TJSElement;
    function ParseBitwiseORExpression: TJSElement;
    function ParseBitwiseXORExpression: TJSElement;
    function ParseBlock: TJSElement;
    function ParseBreakStatement: TJSElement;
    function ParseConditionalExpression: TJSElement;
    function ParseContinueStatement: TJSElement;
    function ParseEmptyStatement: TJSElement;
    function ParseEqualityExpression: TJSElement;
    function ParseExpression: TJSElement;
    function ParseExpressionStatement: TJSElement;
    function ParseFormalParameterList: TStrings;
    function ParseFunctionDeclaration: TJSFunctionDeclarationStatement;
    function ParseFunctionExpression: TJSFunctionDeclarationStatement;
    function ParseFunctionStatement: TJSElement;
    function ParseFunctionBody: TJSFunctionBody;
    function ParseIdentifier: String;
    function ParseIfStatement: TJSElement;
    function ParseIterationStatement: TJSElement;
    function ParseLabeledStatement: TJSElement;
    function ParseLeftHandSideExpression: TJSElement;
    function ParseLiteral: TJSElement;
    function ParseLogicalAndExpression: TJSElement;
    function ParseLogicalORExpression: TJSElement;
    function ParseMemberExpression: TJSElement;
    function ParseMultiplicativeExpression: TJSElement;
    function ParseNumericLiteral: TJSElement;
    function ParseObjectLiteral: TJSElement;
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
    function ParseUnaryExpression: TJSElement;
    function ParseVariableDeclaration: TJSElement;
    function ParseVariableDeclarationList: TJSElement;
    function ParseVariableStatement: TJSElement;
    function ParseWithStatement: TJSElement;
  Protected
    Procedure CheckParser;
    Function CurrentLabelSet : TJSLabelSet;
    function CurSource: String;
    Function CurLine : Integer;
    Function CurPos : Integer;
    Function CreateElement(AElementClass : TJSElementClass)  : TJSElement;
    Procedure Error(Msg : String);
    Procedure Error(Fmt : String; Args : Array of const);
    // Parse functions
    function ParseSourceElements: TJSSourceElements;
    Property FunctionDepth : Integer Read FFunctionDepth Write FFunctionDepth;
    Property NoIn : Boolean Read FNoIn Write FNoIn;
    Property IsLHS : Boolean Read FIsLHS Write FIsLHS;
  Public
    Constructor Create(AInput: TStream);
    Constructor Create(AScanner : TJSScanner);
    Destructor Destroy; override;
    Function Parse : TJSElement;
    Function ParseProgram : TJSFunctionDeclarationStatement;
    Function CurrentToken : TJSToken;
    Function CurrentTokenString : String;
    Function GetNextToken : TJSToken;
    Function PeekNextToken : TJSToken;
    Function IsEndOfLine : Boolean;
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
  SErrObjectEnd              = 'Unexpected token: Expected '','' or ''}'', got %s';
  SErrObjectElement          = 'Unexpected token: Expected string, identifier or number after '','' got: %s';
  SErrLiteralExpected        = 'Unexpected token: Expected: null, true, false, number, string, or regex, got: %s';
  SErrInvalidnumber          = 'Invalid numerical value: %s';
  SErrInvalidRegularExpression = 'Invalid regular expression: %s';
  SErrFunctionNotAllowedHere = 'function keyword not allowed here';

{ TJSScanner }

Function TJSParser.CurrentToken: TJSToken;

begin
  Result:=FCurrent;
end;

Function TJSParser.CurrentTokenString: String;
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

Constructor TJSParser.Create(AInput: TStream);
begin
  FInput:=AInput;
  FCurrent:=TJSUnknown;
  FScanner:=TJSScanner.Create(FInput);
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

function TJSParser.ParseIdentifier : String;

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

function TJSParser.ParseFormalParameterList : TStrings;

begin
  Result:=Nil;
  While (CurrentToken=tjsIdentifier) do
    begin
    Expect(tjsIdentifier);
    If (Result=Nil) then
      Result:=TstringList.Create;
    Result.Add(CurrentTokenString);
    GetNextToken;
    If (CurrentToken=tjsComma) then
       GetNextToken;
    end;
end;


function TJSParser.ParseFunctionDeclaration : TJSFunctionDeclarationStatement;

Var
  Id : String;
  D : TJSFuncDef;
  args : TStrings;
  body : TJSFunctionBody;

begin
  {$ifdef debugparser}  Writeln('>>> Entering ParseFunctionDeclaration');{$endif debugparser}
  Consume(tjsFunction);
  ID:=ParseIdentifier;
  Consume(tjsBraceOpen);
  Args:=ParseFormalParameterList;
  try
    Consume(tjsBraceClose);
    Consume(tjsCurlyBraceOpen);
    Inc(FFunctionDepth);
    try
      Body:=ParseFunctionBody;
      try
        // GetNextToken; not sure
        Consume(tjsCurlyBraceClose);
        D:=TJSFuncDef.Create;
        try
          D.Name:=ID;
          If Assigned(Args)then
            D.Params.Assign(Args);
          D.Body:=Body;
          Result:=TJSFunctionDeclarationStatement(CreateElement(TJSFunctionDeclarationStatement));
          Result.AFunction:=D;
        except
          FreeAndNil(D);
          Raise;
        end;
      except
        FreeAndNil(Body);
        Raise;
      end;
    finally
      Dec(FFunctionDepth);
    end;
  finally
    FreeAndNil(Args);
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
  E : TJSObjectLiteralElement;
  I : Integer;

begin
  Consume(tjsCurlyBraceOpen);
  N:=TJSObjectLiteral(CreateElement(TJSObjectLiteral));
  Result:=N;
  try
    While (CurrentToken<>tjsCurlyBraceClose) do
      begin
      While CurrentToken=tjsComma do
         GetNextToken;
      If (CurrentToken in [tjsIdentifier,jstoken.tjsString,tjsnumber]) then
         begin
         E:=N.Elements.AddElement;
         E.Name:=CurrenttokenString;
         GetNextToken;
         end
      else
         Error(SErrObjectElement,[CurrentTokenString]);
      Consume(tjsColon);
      E.Expr:=ParseAssignmentExpression;
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
  D : Double;
  I : Integer;

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
    tjsNumber : Result:=ParseNumericLiteral;
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

begin
  {$ifdef debugparser}  Writeln('ParsePrimaryExpression');{$endif debugparser}
  Result:=Nil;
  try
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
  C : TJSCallExpression;
  Done : Boolean;

begin
  {$ifdef debugparser}  Writeln('ParseMemberExpression');{$endif debugparser}
  Case CurrentToken of
    tjsFunction : Result:=ParseFunctionExpression();
    tjsNew      : begin
                  GetNextToken;
                  N:=TJSNewMemberExpression(CreateElement(TJSNewMemberExpression));
                  try
                    Result:=N;
                    N.Mexpr:=ParseMemberExpression();
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
         C.Expr:=Result;
         Result:=C;
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

function TJSParser.ParseVariableDeclaration: TJSElement;

Var
  V : TJSVarDeclaration;

begin
  {$ifdef debugparser}  Writeln('ParseVariableDeclaration');{$endif debugparser}
  V:=TJSVarDeclaration(CreateElement(TJSVarDeclaration));;
  try
    V.Name:=CurrenttokenString;
    Consume(tjsIdentifier);
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

function TJSParser.ParseVariableDeclarationList: TJSElement;

Var
  E,N : TJSElement;
  L : TJSVariableDeclarationList;

begin
  {$ifdef debugparser}  Writeln('ParseVariableDeclarationList entry');{$endif debugparser}
  E:=ParseVariableDeclaration;
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
    except
      FreeAndNil(Result);
      Raise;
    end;
    end;
  {$ifdef debugparser}  Writeln('ParseVariableDeclarationList exit');{$endif debugparser}
end;

function TJSParser.ParseVariableStatement : TJSElement;

Var
  E : TJSElement;
  V : TJSVariableStatement;

begin
  {$ifdef debugparser}  Writeln('ParseVariableStatement entry');{$endif debugparser}
  Result:=Nil;
  Consume(tjsVar);
  Result:=ParseVariableDeclarationList;
  try
    Consume(tjsSemicolon,true);
    V:=TJSVariableStatement(CreateElement(TJSVariableStatement));
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
    I.bfalse:=BFalse;
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
  N : TJSElement;

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
end;

function TJSParser.ParseSwitchStatement : TJSElement;


Var
  N : TJSSwitchStatement;
  C : TJSElement;
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

function TJSParser.ParseFunctionExpression : TJSFunctionDeclarationStatement;

Var
  Oni,olhs: Boolean;
  F : TJSFunctionDeclarationStatement;
  N : String;
  Args : TStrings;

begin
  {$ifdef debugparser} Writeln('>>> ParseFunctionExpression');{$endif}
  oni:=NoIn;
  olhs:=IsLHS;
  F:=Nil;
  Args:=Nil;
  try
    NoIn:=False;
    IsLHS:=False;
    F:=TJSFunctionDeclarationStatement(CreateElement(TJSFunctionDeclarationStatement));
    try
      Consume(tjsFunction);
      if (CurrentToken=tjsIdentifier) then
        begin
        n:=CurrentTokenstring;
        GetNextToken;
        end
      else
        n:='';
      Consume(tjsBraceOpen);
      F.AFunction:= TJSFuncDef.Create;
      Args:=ParseFormalParameterList;
      try
        If Assigned(Args) then
          F.AFunction.Params.Assign(Args);
      finally
        FreeAndNil(Args);
      end;
      Consume(tjsBraceClose);
      Consume(tjsCurlyBraceOpen);
      Inc(FFunctionDepth);
      try
        F.AFunction.Body:=ParseFunctionBody;
      Finally
        Dec(FFunctionDepth);
      end;
      Consume(tjsCurlyBraceClose);
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
  LN : String;

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
    tjsVar:
      Result:=ParseVariableStatement;
    tjsSemicolon:
      Result:=ParseEmptyStatement;
    tjsIf:
      Result:=ParseIfStatement;
    tjsDo,tjsWhile,tjsFor:
      Result:=ParseIterationStatement;
    tjsContinue:
      Result:=ParseContinueStatement;
    tjsBreak:
      Result:=ParseBreakStatement;
    tjsReturn:
      Result:=ParseReturnStatement;
    tjsWith:
      Result:=ParseWithStatement;
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
    tjsIdentifier:
      If (PeekNextToken=tjsColon) then
        Result:=ParseLabeledStatement
      else
        Result:=ParseExpressionStatement;
  else
    Result:=ParseExpressionStatement;
  end;
  {$ifdef debugparser} If Assigned(Result) then Writeln('<<< Parsestatement ',Result.ClassName) else Writeln('<<< Parsestatement (null');{$endif}
end;

function TJSParser.ParseSourceElements : TJSSourceElements;

Const
  StatementTokens = [tjsNULL, tjsTRUE, tjsFALSE,
      tjsTHIS, tjsIdentifier,jstoken.tjsSTRING,tjsNUMBER,
      tjsBraceOpen,tjsCurlyBraceOpen,tjsSquaredBraceOpen,
      tjsNew,tjsDelete,tjsVoid,tjsTypeOf,
      tjsPlusPlus,tjsMinusMinus,
      tjsPlus,tjsMinus,tjsNot,tjsNE,tjsSNE,tjsSemicolon,
      tjsVAR,tjsIF,tjsDO,tjsWHILE,tjsFOR,jstoken.tjsCONTINUE,jstoken.tjsBREAK,jstoken.tjsReturn,
      tjsWith,jstoken.tjsSWITCH,tjsThrow,TjsTry,tjsDIV,tjsDIVEQ];

Var
  F : TJSFunctionDeclarationStatement;
  E : TJSElement;
  Done : Boolean;
  VS : TJSElementNodes;
begin
  {$ifdef debugparser} Writeln('>>> Entering source elements');{$endif}
  Result:=TJSSourceElements(CreateElement(TJSSourceElements));
  try
    Done:=False;
    VS:=FCurrentVars;
    Try
      FCurrentVars:=Result.Vars;
      Repeat
        {$ifdef debugparser} Writeln('Sourceelements start:',GetEnumName(TypeInfo(TJSToken),Ord(CurrentToken)), ' As string: ',CurrentTokenString);{$endif debugparser}
        If (CurrentToken=jstoken.tjsFunction) then
          begin
          If (PeekNextToken<>tjsBraceOpen) then
            begin
            F:=Self.ParseFunctionDeclaration;
            Result.functions.AddNode.Node:=F;
            end
          else
            begin
            {$ifdef debugparser} Writeln('Function expression detected');{$endif}
            E:=Self.ParseStatement;
            Result.Statements.AddNode.Node:=E;
            end;
          end
        else if CurrentToken in StatementTokens then
          begin
          E:=Self.ParseStatement;
          Result.Statements.AddNode.Node:=E;
          end
        else
          Done:=True;
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

Function TJSParser.ParseProgram: TJSFunctionDeclarationStatement;

Var
  F : TJSFunctionDeclarationStatement;
  FD : TJSFuncDef;
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

