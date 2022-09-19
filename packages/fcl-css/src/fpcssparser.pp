{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2022- by Michael Van Canneyt (michael@freepascal.org)

    This file contains a CSS parser

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpCSSParser;

{$mode ObjFPC}{$H+}

interface

uses
  TypInfo, Classes, SysUtils, fpcsstree, fpcssscanner;

Type
  ECSSParser = Class(Exception);
  { TCSSParser }

  TCSSParser = class(TObject)
  private
    FInput : TStream;
    FScanner: TCSSScanner;
    FPrevious : TCSSToken;
    FCurrent : TCSSToken;
    FCurrentTokenString : TCSSString;
    FPeekToken : TCSSToken;
    FPeekTokenString : TCSSString;
    FFreeScanner : Boolean;
    FRuleLevel : Integer;
    function CreateElement(aClass: TCSSElementClass): TCSSElement;
    class function GetAppendElement(aList: TCSSListElement): TCSSElement;
    function GetAtEOF: Boolean;
    function GetCurSource: TCSSString;
    Function GetCurLine : Integer;
    Function GetCurPos : Integer;
  protected
    Procedure DoWarn(const Msg : TCSSString);
    Procedure DoWarn(const Fmt : TCSSString; const Args : Array of const);
    Procedure DoWarnExpectedButGot(const Expected: string);
    Procedure DoError(const Msg : TCSSString);
    Procedure DoError(const Fmt : TCSSString; const Args : Array of const);
    Procedure DoErrorExpectedButGot(const Expected: string);
    Procedure Consume(aToken : TCSSToken);
    Procedure SkipWhiteSpace;
    function ParseComponentValueList(AllowRules: Boolean=True): TCSSElement;
    function ParseComponentValue: TCSSElement;
    function ParseExpression: TCSSElement;
    function ParseRule: TCSSElement;
    function ParseAtUnknownRule: TCSSElement;
    function ParseAtMediaRule: TCSSAtRuleElement;
    function ParseAtSimpleRule: TCSSAtRuleElement;
    function ParseMediaCondition: TCSSElement;
    function ParseRuleList(aStopOn : TCSStoken = ctkEOF): TCSSElement;
    function ParseSelector: TCSSElement;
    function ParseAttributeSelector: TCSSElement;
    function ParseWQName: TCSSElement;
    function ParseDeclaration(aIsAt : Boolean = false): TCSSDeclarationElement;
    function ParseCall(aName: TCSSString): TCSSElement;
    procedure ParseSelectorCommaList(aCall: TCSSCallElement);
    procedure ParseRelationalSelectorCommaList(aCall: TCSSCallElement);
    procedure ParseNthChildParams(aCall: TCSSCallElement);
    function ParseUnary: TCSSElement;
    function ParseUnit: TCSSUnits;
    function ParseIdentifier : TCSSIdentifierElement;
    function ParseHashIdentifier : TCSSHashIdentifierElement;
    function ParseClassName : TCSSClassNameElement;
    function ParseParenthesis: TCSSElement;
    function ParsePseudo: TCSSElement;
    Function ParseRuleBody(aRule: TCSSRuleElement; aIsAt : Boolean = False) : integer;
    function ParseInteger: TCSSElement;
    function ParseFloat: TCSSElement;
    function ParseString: TCSSElement;
    Function ParseUnicodeRange : TCSSElement;
    function ParseArray(aPrefix: TCSSElement): TCSSElement;
    function ParseURL: TCSSElement;
    function ParseInvalidToken: TCSSElement;
    Property CurrentSource : TCSSString Read GetCurSource;
    Property CurrentLine : Integer Read GetCurLine;
    Property CurrentPos : Integer Read GetCurPos;
  Public
    Constructor Create(AInput: TStream; ExtraScannerOptions : TCSSScannerOptions = []);
    Constructor Create(AScanner : TCSSScanner); virtual;
    Destructor Destroy; override;
    Function Parse : TCSSElement;
    Function ParseInline : TCSSElement;
    Property CurrentToken : TCSSToken Read FCurrent;
    Property CurrentTokenString : TCSSString Read FCurrentTokenString;
    Function GetNextToken : TCSSToken;
    Function PeekNextToken : TCSSToken;
    Property Scanner : TCSSScanner Read FScanner;
    Property atEOF : Boolean Read GetAtEOF;
  end;

Function TokenToBinaryOperation(aToken : TCSSToken) : TCSSBinaryOperation;
Function TokenToUnaryOperation(aToken : TCSSToken) : TCSSUnaryOperation;

implementation

Resourcestring
  SBinaryInvalidToken = 'Invalid token for binary operation: %s';
  SUnaryInvalidToken = 'Invalid token for unary operation: %s';
  SErrFileSource = 'Error: file "%s" line %d, pos %d: ';
  SErrSource = 'Error: line %d, pos %d: ';
  SErrUnexpectedToken = 'Unexpected token: Got %s (as string: "%s"), expected: %s ';
  SErrInvalidFloat = 'Invalid float: %s';
  SErrUnexpectedEndOfFile = 'Unexpected EOF while scanning function args: %s';

Function TokenToBinaryOperation(aToken : TCSSToken) : TCSSBinaryOperation;

begin
  Case aToken of
    ctkEquals : Result:=boEquals;
    ctkPlus : Result:=boPlus;
    ctkMinus:  Result:=boMinus;
    ctkAnd : result:=boAnd;
    ctkGE : Result:=boGE;
    ctkGT : Result:=boGT;
    ctkLE : Result:=boLE;
    ctkLT : Result:=boLT;
    ctkDIV : Result:=boDIV;
    ctkStar : Result:=boStar;
    ctkSTAREQUAL : Result:=boStarEqual;
    ctkTilde : Result:=boTilde;
    ctkTILDEEQUAL : Result:=boTildeEqual;
    ctkSquared : Result:=boSquared;
    ctkSQUAREDEQUAL : Result:=boSquaredEqual;
    ctkPIPE : Result:=boPipe;
    ctkPIPEEQUAL : Result:=boPipeEqual;
    ctkDOLLAR : Result:=boDollar;
    ctkDOLLAREQUAL : Result:=boDollarEqual;
    ctkColon : Result:=boCOLON;
    ctkDoubleColon : Result:=boDoubleColon;
  else
    Raise ECSSParser.CreateFmt(SBinaryInvalidToken,[GetEnumName(TypeInfo(aToken),Ord(aToken))]);
    // Result:=boEquals;
  end;
end;

Function TokenToUnaryOperation(aToken : TCSSToken) : TCSSUnaryOperation;

begin
  Case aToken of
    ctkDOUBLECOLON: Result:=uoDoubleColon;
    ctkMinus: Result:=uoMinus;
    ctkPlus: Result:=uoPlus;
    ctkDiv: Result:=uoDiv;
    ctkGT: Result:=uoGT;
    ctkTILDE: Result:=uoTilde;
  else
    Raise ECSSParser.CreateFmt(SUnaryInvalidToken,[GetEnumName(TypeInfo(aToken),Ord(aToken))]);
  end;
end;

{ TCSSParser }

function TCSSParser.GetAtEOF: Boolean;
begin
  Result:=(CurrentToken=ctkEOF);
end;

procedure TCSSParser.DoError(const Msg: TCSSString);
Var
  ErrAt : TCSSString;

begin
  If Assigned(FScanner) then
    If FScanner.CurFilename<>'' then
      ErrAt:=SafeFormat(SErrFileSource,[FScanner.CurFileName,FScanner.CurRow,FScanner.CurColumn])
    else
      ErrAt:=SafeFormat(SErrSource,[FScanner.Currow,FScanner.CurColumn]);
  Raise ECSSParser.Create(ErrAt+Msg)
end;

procedure TCSSParser.DoError(const Fmt: TCSSString; const Args: array of const);
begin
  DoError(SafeFormat(Fmt,Args));
end;

procedure TCSSParser.DoErrorExpectedButGot(const Expected: string);
begin
  DoError(SErrUnexpectedToken ,[
           GetEnumName(TypeInfo(TCSSToken),Ord(CurrentToken)),
           CurrentTokenString,
           Expected
           ]);
end;

procedure TCSSParser.Consume(aToken: TCSSToken);
begin
  if CurrentToken<>aToken then
    DoError(SErrUnexpectedToken ,[
             GetEnumName(TypeInfo(TCSSToken),Ord(CurrentToken)),
             CurrentTokenString,
             GetEnumName(TypeInfo(TCSSToken),Ord(aToken))
             ]);
  GetNextToken;
end;

procedure TCSSParser.SkipWhiteSpace;
begin
  while CurrentToken=ctkWHITESPACE do
    GetNextToken;
end;

function TCSSParser.GetCurSource: TCSSString;
begin
  If Assigned(FScanner) then
    Result:=FScanner.CurFileName
  else
    Result:='';
end;

function TCSSParser.GetCurLine: Integer;
begin
  if Assigned(FScanner) then
    Result:=FScanner.CurRow
  else
    Result:=0;
end;

function TCSSParser.GetCurPos: Integer;
begin
  if Assigned(FScanner) then
    Result:=FScanner.CurColumn
  else
    Result:=0;
end;

procedure TCSSParser.DoWarn(const Msg: TCSSString);
begin
  if Assigned(Scanner.OnWarn) then
    Scanner.OnWarn(Self,Msg)
  else
    DoError(Msg);
end;

procedure TCSSParser.DoWarn(const Fmt: TCSSString; const Args: array of const);
begin
  DoWarn(SafeFormat(Fmt,Args));
end;

procedure TCSSParser.DoWarnExpectedButGot(const Expected: string);
begin
  DoWarn(SErrUnexpectedToken ,[
           GetEnumName(TypeInfo(TCSSToken),Ord(CurrentToken)),
           CurrentTokenString,
           Expected
           ]);
end;

constructor TCSSParser.Create(AInput: TStream; ExtraScannerOptions : TCSSScannerOptions = []);
begin
  FInput:=AInput;
  Create(TCSSScanner.Create(FInput));
  FScanner.Options:=FScanner.Options+ExtraScannerOptions;
  FFreeScanner:=True;
end;

constructor TCSSParser.Create(AScanner: TCSSScanner);
begin
  FCurrent:=ctkUNKNOWN;
  FPeekToken:=ctkUNKNOWN;
  FPeekTokenString:='';
  FScanner:=aScanner;
end;

destructor TCSSParser.Destroy;
begin
  if FFreeScanner then
    FreeAndNil(FScanner);
  inherited Destroy;
end;

class function TCSSParser.GetAppendElement(aList: TCSSListElement): TCSSElement;

begin
  Case aList.ChildCount of
    0 : Result:=Nil;
    1 : Result:=aList.ExtractElement(0);
  else
    Result:=aList;
  end;
  if Result<>aList then
    aList.Free;
end;

function TCSSParser.ParseAtUnknownRule: TCSSElement;
// read unknown at-rule

Var
  aRule : TCSSRuleElement;
  aSel : TCSSElement;
  Term : TCSSTokens;
  aLast : TCSSToken;
  aList : TCSSListElement;
  {$ifdef VerboseCSSParser}
  aAt : TCSSString;
  {$endif}

begin
  Inc(FRuleLevel);
{$ifdef VerboseCSSParser}
  aAt:=Format(' Level %d at (%d:%d)',[FRuleLevel,CurrentLine,CurrentPos]);
  Writeln('Parse @ rule');
{$endif}
  Term:=[ctkLBRACE,ctkEOF,ctkSEMICOLON];
  aRule:=TCSSAtRuleElement(CreateElement(TCSSAtRuleElement));
  TCSSAtRuleElement(aRule).AtKeyWord:=CurrentTokenString;
  GetNextToken;
  aList:=nil;
  try
    aList:=TCSSListElement(CreateElement(TCSSListElement));
    While Not (CurrentToken in Term) do
      begin
      aSel:=ParseComponentValue;
      aList.AddChild(aSel);
      if CurrentToken=ctkCOMMA then
        begin
        Consume(ctkCOMMA);
        aRule.AddSelector(GetAppendElement(aList));
        aList:=TCSSListElement(CreateElement(TCSSListElement));
        end;
      end;
    aRule.AddSelector(GetAppendElement(aList));
    aList:=nil;
    aLast:=CurrentToken;
    if (aLast<>ctkSEMICOLON) then
      begin
      Consume(ctkLBRACE);
      aRule.AddChild(ParseRuleList(ctkRBRACE));
      Consume(ctkRBRACE);
      end;
    Result:=aRule;
    aRule:=nil;
{$ifdef VerboseCSSParser}  Writeln('Done Parse @ rule ',aAt); {$endif}
    Inc(FRuleLevel);
  finally
    aRule.Free;
  end;
end;

function TCSSParser.ParseAtMediaRule: TCSSAtRuleElement;

Var
  {$ifdef VerboseCSSParser}
  aAt : TCSSString;
  {$endif}
  aRule : TCSSAtRuleElement;
  Term : TCSSTokens;
  aLast , aToken: TCSSToken;
  aList : TCSSListElement;

begin
  Inc(FRuleLevel);
{$ifdef VerboseCSSParser}
  aAt:=Format(' Level %d at (%d:%d)',[FRuleLevel,CurrentLine,CurrentPos]);
  Writeln('Parse @media rule');
{$endif}
  Term:=[ctkLBRACE,ctkEOF,ctkSEMICOLON];
  aRule:=TCSSAtRuleElement(CreateElement(TCSSAtRuleElement));
  aRule.AtKeyWord:=CurrentTokenString;
  GetNextToken;
  aList:=nil;
  try
    aList:=TCSSListElement(CreateElement(TCSSListElement));
    While Not (CurrentToken in Term) do
      begin
      aToken:=CurrentToken;
      //  writeln('TCSSParser.ParseAtMediaRule Token=',CurrentToken);
      case aToken of
      ctkIDENTIFIER:
        aList.AddChild(ParseIdentifier);
      ctkLPARENTHESIS:
        aList.AddChild(ParseMediaCondition);
      else
        Consume(ctkIDENTIFIER);
      end;
      if CurrentToken=ctkCOMMA then
        begin
        Consume(ctkCOMMA);
        aRule.AddSelector(GetAppendElement(aList));
        aList:=TCSSListElement(CreateElement(TCSSListElement));
        end;
      end;
    aRule.AddSelector(GetAppendElement(aList));
    aList:=nil;
    aLast:=CurrentToken;
    if (aLast<>ctkSEMICOLON) then
      begin
      Consume(ctkLBRACE);
      aRule.AddChild(ParseRuleList(ctkRBRACE));
      Consume(ctkRBRACE);
      end;
    Result:=aRule;
    aRule:=nil;
{$ifdef VerboseCSSParser}  Writeln('Done Parse @ rule ',aAt); {$endif}
    Inc(FRuleLevel);
  finally
    aRule.Free;
  end;
end;

function TCSSParser.ParseAtSimpleRule: TCSSAtRuleElement;
var
  {$ifdef VerboseCSSParser}
  aAt : TCSSString;
  {$endif}
  aRule: TCSSAtRuleElement;
begin
  Result:=nil;
  Inc(FRuleLevel);
{$ifdef VerboseCSSParser}
  aAt:=Format(' Level %d at (%d:%d)',[FRuleLevel,CurrentLine,CurrentPos]);
  Writeln('Parse @font-face rule');
{$endif}
  aRule:=TCSSAtRuleElement(CreateElement(TCSSAtRuleElement));
  try
    aRule.AtKeyWord:=CurrentTokenString;
    GetNextToken;

    // read {
    repeat
      case CurrentToken of
      ctkEOF:
        DoErrorExpectedButGot('{');
      ctkRBRACE, ctkRPARENTHESIS, ctkSEMICOLON:
        begin
        DoWarnExpectedButGot('{');
        Result:=aRule;
        aRule:=nil;
        exit;
        end;
      ctkLBRACE:
        break;
      end;
    until false;
    GetNextToken;

    // read declarations
    ParseRuleBody(aRule);
    if CurrentToken=ctkRBRACE then
      GetNextToken;

    Result:=aRule;
    aRule:=nil;
    {$ifdef VerboseCSSParser}  Writeln('Done Parse @ rule ',aAt); {$endif}
    Inc(FRuleLevel);
  finally
    aRule.Free;
  end;
end;

function TCSSParser.ParseMediaCondition: TCSSElement;
// for example:
//   (color)
//   (color: #fff)
//   (30em <= width)
//   (30em >= width > 20em)
//   (not(MediaCondition))
var
  El: TCSSElement;
  Bin: TCSSBinaryElement;
  List: TCSSListElement;
  aToken: TCSSToken;
begin
  Consume(ctkLPARENTHESIS);
  {$IFDEF VerboseCSSParser}
  writeln('TCSSParser.ParseMediaCondition START ',CurrentToken);
  {$ENDIF}

  El:=nil;
  Bin:=nil;
  List:=nil;
  try
    case CurrentToken of
    ctkIDENTIFIER:
      begin
      El:=ParseIdentifier;
      if TCSSIdentifierElement(El).Value='not' then
        begin
        // (not(mediacondition))
        List:=TCSSListElement(CreateElement(TCSSListElement));
        List.AddChild(El);
        El:=nil;
        List.AddChild(ParseMediaCondition());
        Result:=List;
        List:=nil;
        exit;
        end
      else if CurrentToken=ctkCOLON then
        begin
        // (mediaproperty: value)
        Bin:=TCSSBinaryElement(CreateElement(TCSSBinaryElement));
        Bin.Left:=El;
        El:=nil;
        Consume(ctkCOLON);
        Bin.Right:=ParseComponentValue;
        Consume(ctkRPARENTHESIS);
        Result:=Bin;
        Bin:=nil;
        exit;
        end;
      end;
    ctkSTRING:
      El:=ParseString;
    ctkINTEGER:
      El:=ParseInteger;
    ctkFLOAT:
      El:=ParseFloat;
    else
      Consume(ctkIDENTIFIER);
    end;

    // read binaryoperator operand til bracket close
    repeat
      aToken:=CurrentToken;
      {$IFDEF VerboseCSSResolver}
      writeln('TCSSParser.ParseMediaCondition NEXT ',CurrentToken);
      {$ENDIF}
      case aToken of
      ctkRPARENTHESIS:
        begin
        Result:=El;
        GetNextToken;
        break;
        end;
      ctkEQUALS,
      ctkGE,ctkGT,ctkLE,ctkLT:
        begin
        Bin:=TCSSBinaryElement(CreateElement(TCSSBinaryElement));
        Bin.Left:=El;
        Bin.Operation:=TokenToBinaryOperation(aToken);
        GetNextToken;
        end;
      else
        Consume(ctkRPARENTHESIS);
      end;

      case CurrentToken of
      ctkIDENTIFIER:
        Bin.Right:=ParseIdentifier;
      ctkSTRING:
        Bin.Right:=ParseString;
      ctkINTEGER:
        Bin.Right:=ParseInteger;
      ctkFLOAT:
        Bin.Right:=ParseFloat;
      else
        Consume(ctkIDENTIFIER);
      end;
      El:=Bin;
      Bin:=nil;
    until false;

  finally
    List.Free;
    Bin.Free;
    El.Free;
  end;

  {$IFDEF VerboseCSSParser}
  writeln('TCSSParser.ParseMediaCondition END');
  {$ENDIF}
end;

function TCSSParser.ParseExpression: TCSSElement;

Const
  RuleTokens =
       [ctkIDENTIFIER,ctkCLASSNAME,ctkHASH,ctkINTEGER,
        ctkPSEUDO,ctkPSEUDOFUNCTION,
        ctkCOLON,ctkDOUBLECOLON,ctkSTAR,ctkTILDE,ctkLBRACKET];

begin
  if CurrentToken in RuleTokens then
    Result:=ParseRule
  else if CurrentToken=ctkATKEYWORD then
    case lowercase(CurrentTokenString) of
    '@media': Result:=ParseAtMediaRule;
    '@font-face',
    '@page': Result:=ParseAtSimpleRule;
    else
      Result:=ParseAtUnknownRule;
    end
  else
    Result:=ParseComponentValueList;
end;

function TCSSParser.ParseRuleList(aStopOn : TCSStoken = ctkEOF): TCSSElement;

Var
  aList : TCSSCompoundElement;
  aEl : TCSSElement;
  Terms : TCSSTokens;
begin
  Terms:=[ctkEOF,aStopOn];
  aList:=TCSSCompoundElement(CreateElement(TCSSCompoundElement));
  Try
    While not (CurrentToken in Terms) do
      begin
      aEl:=ParseExpression;
      aList.AddChild(aEl);
      if CurrentToken=ctkSEMICOLON then
        Consume(ctkSEMICOLON);
      end;
    Result:=aList;
    aList:=nil;
  finally
    aList.Free;
  end;
end;

function TCSSParser.Parse: TCSSElement;
begin
  GetNextToken;
  if CurrentToken=ctkLBRACE then
    Result:=ParseRule
  else
    Result:=ParseRuleList;
end;

function TCSSParser.ParseInline: TCSSElement;
var
  aRule: TCSSRuleElement;
begin
  GetNextToken;
  aRule:=TCSSRuleElement(CreateElement(TCSSRuleElement));
  try
    ParseRuleBody(aRule);
    Result:=aRule;
    aRule:=nil;
  finally
    aRule.Free;
  end;
end;

function TCSSParser.GetNextToken: TCSSToken;
begin
  FPrevious:=FCurrent;
  If (FPeekToken<>ctkUNKNOWN) then
    begin
    FCurrent:=FPeekToken;
    FCurrentTokenString:=FPeekTokenString;
    FPeekToken:=ctkUNKNOWN;
    FPeekTokenString:='';
    end
  else
    begin
    FCurrent:=FScanner.FetchToken;
    FCurrentTokenString:=FScanner.CurTokenString;
    end;
  Result:=FCurrent;
  {$ifdef VerboseCSSParser}
     Writeln('GetNextToken returns ',
       GetEnumName(TypeInfo(TCSSToken),Ord(FCurrent)),
       '(String: "',FCurrentTokenString,'")',
       ' at (',FScanner.CurRow,',',FScanner.CurColumn,'): ',
       FSCanner.CurLine);
  {$endif VerboseCSSParser}
end;

function TCSSParser.PeekNextToken: TCSSToken;
begin
  If (FPeekToken=ctkUNKNOWN) then
    begin
    FPeekToken:=FScanner.FetchToken;
    FPeekTokenString:=FScanner.CurTokenString;
    end;
  {$ifdef VerboseCSSParser}Writeln('PeekNextToken : ',GetEnumName(TypeInfo(TCSSToken),Ord(FPeekToken)), ' As TCSSString: ',FPeekTokenString);{$endif VerboseCSSParser}
  Result:=FPeekToken;
end;

function TCSSParser.ParseUnit : TCSSUnits;

begin
  Result:=cuNone;
  if (CurrentToken in [ctkIDENTIFIER,ctkPERCENTAGE]) then
    begin
    Case currentTokenString of
    '%'   : Result:=cuPERCENT;
    'px'  : Result:=cuPX;
    'rem' : Result:=cuREM;
    'em'  : Result:=cuEM;
    'fr'  : Result:=cuFR;
    'vw'  : Result:=cuVW;
    'vh'  : Result:=cuVH;
    'pt'  : Result:=cuPT;
    'deg' : Result:=cuDEG;
    else
      // Ignore. For instance margin: 0 auto
    end;
    if Result<>cuNone then
      Consume(CurrentToken);
    end;
end;

function TCSSParser.CreateElement(aClass : TCSSElementClass): TCSSElement;

begin
  Result:=aClass.Create(CurrentSource,CurrentLine,CurrentPos);
end;

function TCSSParser.ParseIdentifier: TCSSIdentifierElement;

Var
  aValue : TCSSString;

begin
  aValue:=CurrentTokenString;
  Result:=TCSSIdentifierElement(CreateElement(TCSSIdentifierElement));
  Result.Value:=aValue;
  GetNextToken;
end;

function TCSSParser.ParseHashIdentifier: TCSSHashIdentifierElement;

Var
  aValue : TCSSString;

begin
  aValue:=CurrentTokenString;
  system.delete(aValue,1,1);
  Result:=TCSSHashIdentifierElement(CreateElement(TCSSHashIdentifierElement));
  Result.Value:=aValue;
  GetNextToken;
end;

function TCSSParser.ParseClassName: TCSSClassNameElement;

Var
  aValue : TCSSString;

begin
  aValue:=CurrentTokenString;
  system.delete(aValue,1,1);
  Result:=TCSSClassNameElement(CreateElement(TCSSClassNameElement));
  Result.Value:=aValue;
  GetNextToken;
end;

function TCSSParser.ParseInteger: TCSSElement;

Var
  aValue : Integer;
  aInt : TCSSIntegerElement;

begin
  aValue:=StrToInt(CurrentTokenString);
  aInt:=TCSSIntegerElement(CreateElement(TCSSIntegerElement));
  try
    aInt.Value:=aValue;
    Consume(ctkINTEGER);
    aInt.Units:=ParseUnit;
    Result:=aInt;
    aInt:=nil;
  finally
    aInt.Free;
  end;
end;

function TCSSParser.ParseFloat: TCSSElement;
Var
  aCode : Integer;
  aValue : Double;
  aFloat : TCSSFloatElement;

begin
  Val(CurrentTokenString,aValue,aCode);
  if aCode<>0 then
    DoError(SErrInvalidFloat,[CurrentTokenString]);
  aFloat:=TCSSFloatElement(CreateElement(TCSSFloatElement));
  try
    Consume(ctkFloat);
    aFloat.Value:=aValue;
    aFloat.Units:=ParseUnit;
    Result:=aFloat;
    aFloat:=nil;
  finally
    aFloat.Free;
  end;
end;


function TCSSParser.ParseParenthesis: TCSSElement;

var
  aList: TCSSElement;
begin
  Consume(ctkLPARENTHESIS);
  aList:=ParseComponentValueList;
  try
    Consume(ctkRPARENTHESIS);
    Result:=aList;
    aList:=nil;
  finally
    aList.Free;
  end;
end;

function TCSSParser.ParseURL: TCSSElement;

Var
  aURL : TCSSURLElement;

begin
  aURL:=TCSSURLElement(CreateElement(TCSSURLElement));
  try
    aURL.Value:=CurrentTokenString;
    if CurrentToken=ctkURL then
      Consume(ctkURL)
    else
      Consume(ctkBADURL);
     Result:=aURL;
     aURL:=nil;
  finally
    aURL.Free;
  end;
end;

function TCSSParser.ParseInvalidToken: TCSSElement;
begin
  Result:=TCSSElement(CreateElement(TCSSElement));
  GetNextToken;
end;

function TCSSParser.ParsePseudo: TCSSElement;

Var
  aPseudo : TCSSPseudoClassElement;
  aValue : TCSSString;

begin
  aValue:=CurrentTokenString;
  aPseudo:=TCSSPseudoClassElement(CreateElement(TCSSPseudoClassElement));
  try
    Consume(ctkPseudo);
    aPseudo.Value:=aValue;
    Result:=aPseudo;
    aPseudo:=nil;
  finally
    aPseudo.Free;
  end;
end;

function TCSSParser.ParseRuleBody(aRule: TCSSRuleElement; aIsAt: Boolean = false): integer;

Var
  aDecl : TCSSElement;

begin
  aDecl:=nil;
  if not (CurrentToken in [ctkRBRACE,ctkSEMICOLON]) then
    begin
    aDecl:=ParseDeclaration(aIsAt);
    aRule.AddChild(aDecl);
    end;
  While Not (CurrentToken in [ctkEOF,ctkRBRACE]) do
    begin
    While CurrentToken=ctkSEMICOLON do
      Consume(ctkSEMICOLON);
    if Not (CurrentToken in [ctkEOF,ctkRBRACE]) then
      begin
      if CurrentToken=ctkATKEYWORD then
        aDecl:=ParseAtUnknownRule
      else
        aDecl:=ParseDeclaration(aIsAt);
      aRule.AddChild(aDecl);
      end;
    end;
  Result:=aRule.ChildCount;
end;

function TCSSParser.ParseRule: TCSSElement;

Var
  aRule : TCSSRuleElement;
  aSel : TCSSElement;
  Term : TCSSTokens;
  aLast : TCSSToken;
  aList: TCSSListElement;
{$IFDEF VerboseCSSParser}
  aAt : TCSSString;
{$ENDIF}

begin
  Inc(FRuleLevel);
{$IFDEF VerboseCSSParser}
  aAt:=Format(' Level %d at (%d:%d)',[FRuleLevel,CurrentLine,CurrentPos]);
  Writeln('Parse rule.: ',aAt);
{$ENDIF}
  case CurrentToken of
  ctkEOF: exit(nil);
  ctkSEMICOLON:
    begin
    Result:=TCSSRuleElement(CreateElement(TCSSRuleElement));
    exit;
    end;
  end;

  Term:=[ctkLBRACE,ctkEOF,ctkSEMICOLON];
  aRule:=TCSSRuleElement(CreateElement(TCSSRuleElement));
  aList:=nil;
  try
    aList:=TCSSListElement(CreateElement(TCSSListElement));
    While Not (CurrentToken in Term) do
      begin
      aSel:=ParseSelector;
      aRule.AddSelector(aSel);
      if CurrentToken=ctkCOMMA then
        begin
        Consume(ctkCOMMA);
        aRule.AddSelector(GetAppendElement(aList));
        aList:=TCSSListElement(CreateElement(TCSSListElement));
        end;
      end;
    // Note: no selectors is allowed
    aRule.AddSelector(GetAppendElement(aList));
    aList:=nil;
    aLast:=CurrentToken;
    if (aLast<>ctkSEMICOLON) then
      begin
      Consume(ctkLBrace);
      ParseRuleBody(aRule);
      Consume(ctkRBRACE);
      end;
    Result:=aRule;
    aRule:=nil;
    {$IFDEF VerboseCSSParser}
    Writeln('Rule started at ',aAt,' done');
    {$endif}
    Dec(FRuleLevel);
  finally
    aRule.Free;
    aList.Free;
  end;
end;

function TCSSParser.ParseUnary: TCSSElement;

var
  Un : TCSSUnaryElement;
  Op : TCSSUnaryOperation;

begin
  Result:=nil;
  if not (CurrentToken in [ctkDOUBLECOLON, ctkMinus, ctkPlus, ctkDiv, ctkGT, ctkTILDE]) then
    Raise ECSSParser.CreateFmt(SUnaryInvalidToken,[CurrentTokenString]);
  Un:=TCSSUnaryElement(CreateElement(TCSSUnaryElement));
  try
    op:=TokenToUnaryOperation(CurrentToken);
    Un.Operation:=op;
    Consume(CurrentToken);
    Un.Right:=ParseComponentValue;
    Result:=Un;
    Un:=nil;
  finally
    Un.Free;
  end;
end;

function TCSSParser.ParseComponentValueList(AllowRules : Boolean = True): TCSSElement;

Const
  TermSeps = [ctkEquals,ctkPlus,ctkMinus,ctkAnd,ctkLT,ctkDIV,
              ctkStar,ctkTilde,ctkColon, ctkDoubleColon,
              ctkSquared,ctkGT, ctkPIPE, ctkDOLLAR];
  ListTerms = [ctkEOF,ctkLBRACE,ctkATKEYWORD,ctkComma];

  function DoBinary(var aLeft : TCSSElement) : TCSSElement;
  var
    Bin : TCSSBinaryElement;
  begin
    Bin:=TCSSBinaryElement(CreateElement(TCSSBinaryElement));
    try
      Bin.Left:=ALeft;
      aLeft:=Nil;
      Bin.Operation:=TokenToBinaryOperation(CurrentToken);
      Consume(CurrentToken);
      Bin.Right:=ParseComponentValue;
      if Bin.Right=nil then
        DoError(SErrUnexpectedToken ,[
               GetEnumName(TypeInfo(TCSSToken),Ord(CurrentToken)),
               CurrentTokenString,
               'value'
               ]);
      Result:=Bin;
      Bin:=nil;
    finally
      Bin.Free;
    end;
  end;

Var
  List : TCSSListElement;
  aFactor : TCSSelement;

begin
  aFactor:=Nil;
  List:=TCSSListElement(CreateElement(TCSSListElement));
  try
    if AllowRules and (CurrentToken in [ctkLBRACE,ctkATKEYWORD]) then
      begin
      if CurrentToken=ctkATKEYWORD then
        aFactor:=ParseAtUnknownRule
      else
        aFactor:=ParseRule;
      end
    else
      aFactor:=ParseComponentValue;
    if aFactor=nil then
      DoError(SErrUnexpectedToken ,[
             GetEnumName(TypeInfo(TCSSToken),Ord(CurrentToken)),
             CurrentTokenString,
             'value'
             ]);
    While Assigned(aFactor) do
      begin
      While CurrentToken in TermSeps do
        aFactor:=DoBinary(aFactor);
      List.AddChild(aFactor);
      aFactor:=Nil;
      if not (CurrentToken in ListTerms) then
        aFactor:=ParseComponentValue;
      end;
    Result:=GetAppendElement(List);
    List:=nil;
  finally
    List.Free;
    aFactor.Free;
  end;
end;


function TCSSParser.ParseComponentValue: TCSSElement;

Const
  FinalTokens =
     [ctkLPARENTHESIS,ctkURL,ctkColon,ctkLBRACE, ctkLBRACKET,
      ctkDOUBLECOLON,ctkMinus,ctkPlus,ctkDiv,ctkSTAR,ctkTILDE];

var
  aToken : TCSSToken;

begin
  aToken:=CurrentToken;
  Case aToken of
    ctkLPARENTHESIS: Result:=ParseParenthesis;
    ctkURL: Result:=ParseURL;
    ctkPSEUDO: Result:=ParsePseudo;
    ctkLBRACE: Result:=ParseRule;
    ctkLBRACKET: Result:=ParseArray(Nil);
    ctkMinus,
    ctkPlus,
    ctkDiv,
    ctkGT,
    ctkTilde: Result:=ParseUnary;
    ctkUnicodeRange: Result:=ParseUnicodeRange;
    ctkSTRING,
    ctkHASH : Result:=ParseString;
    ctkINTEGER: Result:=ParseInteger;
    ctkFloat : Result:=ParseFloat;
    ctkPSEUDOFUNCTION,
    ctkFUNCTION : Result:=ParseCall('');
    ctkSTAR: Result:=ParseInvalidToken;
    ctkIDENTIFIER: Result:=ParseIdentifier;
    ctkCLASSNAME : Result:=ParseClassName;
  else
    Result:=nil;
//    Consume(aToken);// continue
  end;
  if aToken in FinalTokens then
    exit;
  if (CurrentToken=ctkLBRACKET) then
    Result:=ParseArray(Result);
end;

function TCSSParser.ParseSelector: TCSSElement;

  function ParseSub: TCSSElement;
  begin
    Result:=nil;
    Case CurrentToken of
      ctkSTAR,
      ctkIDENTIFIER : Result:=ParseIdentifier;
      ctkHASH : Result:=ParseHashIdentifier;
      ctkCLASSNAME : Result:=ParseClassName;
      ctkLBRACKET: Result:=ParseAttributeSelector;
      ctkPSEUDO: Result:=ParsePseudo;
      ctkPSEUDOFUNCTION: Result:=ParseCall('');
    else
      DoWarn(SErrUnexpectedToken ,[
               GetEnumName(TypeInfo(TCSSToken),Ord(CurrentToken)),
               CurrentTokenString,
               'selector'
               ]);
      case CurrentToken of
      ctkINTEGER: Result:=ParseInteger;
      ctkFLOAT: Result:=ParseFloat;
      else Result:=ParseInvalidToken;
      end;
    end;
  end;

var
  ok, OldReturnWhiteSpace: Boolean;
  Bin: TCSSBinaryElement;
  El: TCSSElement;
  List: TCSSListElement;
begin
  Result:=nil;
  if CurrentToken in [ctkLBRACE,ctkRBRACE,ctkRPARENTHESIS,ctkEOF] then
    exit;
  El:=nil;
  Bin:=nil;
  List:=nil;
  ok:=false;
  //writeln('TCSSParser.ParseSelector START ',CurrentToken);
  OldReturnWhiteSpace:=Scanner.ReturnWhiteSpace;
  Scanner.ReturnWhiteSpace:=true;
  try
    repeat
      {$IFDEF VerbosecSSParser}
      writeln('TCSSParser.ParseSelector LIST START ',CurrentToken,' ',CurrentTokenString);
      {$ENDIF}
      // read list
      List:=nil;
      El:=ParseSub;
      {$IFDEF VerbosecSSParser}
      writeln('TCSSParser.ParseSelector LIST NEXT ',CurrentToken,' ',CurrentTokenString,' El=',GetCSSObj(El));
      {$ENDIF}
      while CurrentToken in [ctkSTAR,ctkHASH,ctkIDENTIFIER,ctkCLASSNAME,ctkLBRACKET,ctkPSEUDO,ctkPSEUDOFUNCTION] do
        begin
        if List=nil then
          begin
          List:=TCSSListElement(CreateElement(TCSSListElement));
          List.AddChild(El);
          El:=List;
          end;
        List.AddChild(ParseSub);
        end;
      List:=nil;

      // use element
      if Bin<>nil then
        Bin.Right:=El
      else
        Result:=El;
      El:=nil;

      SkipWhiteSpace;
      {$IFDEF VerbosecSSParser}
      writeln('TCSSParser.ParseSelector LIST END ',CurrentToken,' ',CurrentTokenString);
      {$ENDIF}

      case CurrentToken of
      ctkLBRACE,ctkRBRACE,ctkRBRACKET,ctkRPARENTHESIS,ctkEOF,ctkSEMICOLON,ctkCOMMA:
        break;
      ctkGT,ctkPLUS,ctkTILDE,ctkPIPE:
        begin
        // combinator
        Bin:=TCSSBinaryElement(CreateElement(TCSSBinaryElement));
        Bin.Left:=Result;
        Result:=Bin;
        Bin.Operation:=TokenToBinaryOperation(CurrentToken);
        GetNextToken;
        SkipWhiteSpace;
        end;
      ctkSTAR,ctkHASH,ctkIDENTIFIER,ctkCLASSNAME,ctkLBRACKET,ctkPSEUDO,ctkPSEUDOFUNCTION:
        begin
        // decendant combinator
        Bin:=TCSSBinaryElement(CreateElement(TCSSBinaryElement));
        Bin.Left:=Result;
        Result:=Bin;
        Bin.Operation:=boWhiteSpace;
        end;
      else
        break;
      end;
    until false;
    ok:=true;
  finally
    Scanner.ReturnWhiteSpace:=OldReturnWhiteSpace;
    if not ok then
      begin
      Result.Free;
      El.Free;
      List.Free;
      Bin.Free;
      end;
  end;
end;

function TCSSParser.ParseAttributeSelector: TCSSElement;

Var
  aEl : TCSSElement;
  aArray : TCSSArrayElement;
  Bin: TCSSBinaryElement;
  StrEl: TCSSStringElement;
  aToken: TCSSToken;

begin
  Result:=Nil;
  aArray:=TCSSArrayElement(CreateElement(TCSSArrayElement));
  try
    Consume(ctkLBRACKET);
    SkipWhiteSpace;
    aEl:=ParseWQName;
    SkipWhiteSpace;
    aToken:=CurrentToken;
    case aToken of
    ctkEQUALS,ctkTILDEEQUAL,ctkPIPEEQUAL,ctkSQUAREDEQUAL,ctkDOLLAREQUAL,ctkSTAREQUAL:
      begin
      // parse attr-matcher
      Bin:=TCSSBinaryElement(CreateElement(TCSSBinaryElement));
      aArray.AddChild(Bin);
      Bin.Left:=aEl;
      Bin.Operation:=TokenToBinaryOperation(aToken);
      GetNextToken;
      SkipWhiteSpace;
      // parse value
      case CurrentToken of
      ctkIDENTIFIER:
        Bin.Right:=ParseIdentifier;
      ctkSTRING:
        begin
        StrEl:=TCSSStringElement(CreateElement(TCSSStringElement));
        StrEl.Value:=CurrentTokenString;
        Bin.Right:=StrEl;
        GetNextToken;
        end;
      ctkINTEGER:
        Bin.Right:=ParseInteger;
      ctkFLOAT:
        Bin.Right:=ParseFloat;
      else
        DoError(SErrUnexpectedToken ,[
                 GetEnumName(TypeInfo(TCSSToken),Ord(CurrentToken)),
                 CurrentTokenString,
                 'attribute value'
                 ]);
      end;
      end;
    else
      aArray.AddChild(aEl);
    end;
    SkipWhiteSpace;
    while CurrentToken=ctkIDENTIFIER do
      begin
      // attribute modifier
      // with CSS 5 there is only i and s, but for future compatibility read all
      aArray.AddChild(ParseIdentifier);
      SkipWhiteSpace;
      end;
    Consume(ctkRBRACKET);

    Result:=aArray;
    aArray:=nil;
  finally
    aArray.Free;
  end;
end;

function TCSSParser.ParseWQName: TCSSElement;
begin
  if CurrentToken<>ctkIDENTIFIER then
    DoError(SErrUnexpectedToken ,[
             GetEnumName(TypeInfo(TCSSToken),Ord(CurrentToken)),
             CurrentTokenString,
             'identifier'
             ]);
  Result:=ParseIdentifier;
  // todo: parse optional ns-prefix
end;

function TCSSParser.ParseDeclaration(aIsAt: Boolean = false): TCSSDeclarationElement;

Var
  aDecl : TCSSDeclarationElement;
  aKey,aValue : TCSSElement;
  aPrevDisablePseudo : Boolean;
  aList : TCSSListElement;

begin
  aList:=nil;
  aDecl:= TCSSDeclarationElement(CreateElement(TCSSDeclarationElement));
  try
    aPrevDisablePseudo:= Scanner.DisablePseudo;
    Scanner.DisablePseudo:=True;
    aKey:=ParseComponentValue;
    aDecl.AddKey(aKey);
    if aIsAt then
      begin
      While (CurrentToken=ctkCOMMA) do
        begin
        while (CurrentToken=ctkCOMMA) do
          Consume(ctkCOMMA);
        aKey:=ParseComponentValue;
        aDecl.AddKey(aKey);
        end;
      end;
    if Not aIsAt then
      begin
      aDecl.Colon:=True;
      Consume(ctkCOLON);
      end
    else
      begin
      aDecl.Colon:=CurrentToken=ctkColon;
      if aDecl.Colon then
        Consume(ctkColon)
      end;
    Scanner.DisablePseudo:=aPrevDisablePseudo;
    aValue:=ParseComponentValue;
    aList:=TCSSListElement(CreateElement(TCSSListElement));
    aList.AddChild(aValue);
    if aDecl.Colon then
      begin
      While not (CurrentToken in [ctkEOF,ctkSemicolon,ctkRBRACE,ctkImportant]) do
        begin
        While CurrentToken=ctkCOMMA do
          begin
          Consume(ctkCOMMA);
          aDecl.AddChild(GetAppendElement(aList));
          aList:=TCSSListElement(CreateElement(TCSSListElement));
          end;
        aValue:=ParseComponentValue;
        if aValue=nil then break;
        aList.AddChild(aValue);
        end;
      if CurrentToken=ctkImportant then
        begin
        Consume(ctkImportant);
        aDecl.IsImportant:=True;
        end;
      end;
    aDecl.AddChild(GetAppendElement(aList));
    aList:=nil;
    Result:=aDecl;
    aDecl:=nil;
  finally
    Scanner.DisablePseudo:=False;
    aDecl.Free;
    aList.Free;
  end;
end;

function TCSSParser.ParseCall(aName : TCSSString): TCSSElement;

var
  aCall : TCSSCallElement;
  l : Integer;
  OldReturnWhiteSpace: Boolean;
  aValue: TCSSElement;
begin
  OldReturnWhiteSpace:=Scanner.ReturnWhiteSpace;
  Scanner.ReturnWhiteSpace:=false;
  aCall:=TCSSCallElement(CreateELement(TCSSCallElement));
  try
    if (aName='') then
      aName:=CurrentTokenString;
    L:=Length(aName);
    if (L>0) and (aName[L]='(') then
      aName:=Copy(aName,1,L-1);
    aCall.Name:=aName;
    if CurrentToken=ctkPSEUDOFUNCTION then
      begin
      Consume(ctkPSEUDOFUNCTION);
      case aName of
      ':not',':is',':where':
        ParseSelectorCommaList(aCall);
      ':has':
        ParseRelationalSelectorCommaList(aCall);
      ':nth-child',':nth-last-child',':nth-of-type',':nth-last-of-type':
        ParseNthChildParams(aCall);
      end;
      end
    else
      Consume(ctkFUNCTION);
    // Call argument list can be empty: mask()
    While not (CurrentToken in [ctkRPARENTHESIS,ctkEOF]) do
      begin
      aValue:=ParseComponentValue;
      if aValue=nil then
      begin
        aValue:=TCSSElement(CreateElement(TCSSElement));
        GetNextToken;
      end;
      aCall.AddArg(aValue);
      if (CurrentToken=ctkCOMMA) then
        Consume(ctkCOMMA);
      end;
    if CurrentToken=ctkEOF then
      DoError(SErrUnexpectedEndOfFile,[aName]);
    Consume(ctkRPARENTHESIS);
    Result:=aCall;
    aCall:=nil;
  finally
    Scanner.ReturnWhiteSpace:=OldReturnWhiteSpace;
    aCall.Free;
  end;
end;

procedure TCSSParser.ParseSelectorCommaList(aCall: TCSSCallElement);
var
  El: TCSSElement;
begin
  while not (CurrentToken in [ctkEOF,ctkRBRACKET,ctkRBRACE,ctkRPARENTHESIS]) do
    begin
    El:=ParseSelector;
    if EL=nil then exit;
    aCall.AddArg(El);
    SkipWhiteSpace;
    if CurrentToken<>ctkCOMMA then
      exit;
    GetNextToken;
  end;
end;

procedure TCSSParser.ParseRelationalSelectorCommaList(aCall: TCSSCallElement);
var
  El: TCSSElement;
  aToken: TCSSToken;
  IsUnary: Boolean;
  Unary: TCSSUnaryElement;
begin
  while not (CurrentToken in [ctkEOF,ctkRBRACKET,ctkRBRACE,ctkRPARENTHESIS]) do
    begin
    IsUnary:=false;
    aToken:=CurrentToken;
    if aToken in [ctkGT,ctkPLUS,ctkTILDE] then
      begin
      IsUnary:=true;
      GetNextToken;
      end;
    El:=ParseSelector;
    if El=nil then exit;
    if IsUnary then
      begin
      Unary:=TCSSUnaryElement(CreateElement(TCSSUnaryElement));
      aCall.AddArg(Unary);
      Unary.Right:=El;
      Unary.Operation:=TokenToUnaryOperation(aToken);
      end
    else
      aCall.AddArg(El);
    if CurrentToken<>ctkCOMMA then
      exit;
    GetNextToken;
  end;
end;

procedure TCSSParser.ParseNthChildParams(aCall: TCSSCallElement);
// Examples:
// odd
// even
//  n
//  +n
// -2n
// 2n+1
//  even of :not(:hidden)
// 2n+1 of [:not(display=none)]
var
  aUnary: TCSSUnaryElement;
  IdentEl: TCSSIdentifierElement;
begin
  case CurrentToken of
  ctkIDENTIFIER:
    case lowercase(CurrentTokenString) of
    'odd','even','n':
      aCall.AddArg(ParseIdentifier);
    '-n':
      begin
        aUnary:=TCSSUnaryElement(CreateElement(TCSSUnaryElement));
        aCall.AddArg(aUnary);
        aUnary.Operation:=uoMinus;
        IdentEl:=TCSSIdentifierElement(CreateElement(TCSSIdentifierElement));
        aUnary.Right:=IdentEl;
        IdentEl.Value:='n';
        GetNextToken;
      end;
    else
      DoWarnExpectedButGot('An+B');
      aCall.AddArg(ParseIdentifier);
      exit;
    end;
  ctkINTEGER:
    begin
    aCall.AddArg(ParseInteger);
    if (CurrentToken<>ctkIDENTIFIER) then
      begin
      DoWarnExpectedButGot('An+B');
      exit;
      end;
    if (lowercase(CurrentTokenString)<>'n') then
      begin
      DoWarnExpectedButGot('An+B');
      exit;
      end;
    aCall.AddArg(ParseIdentifier);
    end;
  else
    DoWarnExpectedButGot('An+B');
    exit;
  end;

  if CurrentToken in [ctkMINUS,ctkPLUS] then
    aCall.AddArg(ParseUnary);
  if CurrentToken=ctkINTEGER then
    aCall.AddArg(ParseInteger);
  if (CurrentToken=ctkIDENTIFIER) and SameText(CurrentTokenString,'of') then
    begin
    aCall.AddArg(ParseIdentifier);
    aCall.AddArg(ParseSelector);
    end;
end;

function TCSSParser.ParseString: TCSSElement;

Var
  aValue : TCSSString;
  aEl : TCSSElement;
  aStr : TCSSStringElement;

begin
  aValue:=CurrentTokenString;
  aStr:=TCSSStringElement(CreateElement(TCSSStringElement));
  try
    if CurrentToken=ctkSTRING then
      Consume(ctkSTRING)
    else
      Consume(ctkHASH); // e.g. #rrggbb
    aStr.Value:=aValue;
    While (CurrentToken in [ctkIDENTIFIER,ctkSTRING,ctkINTEGER,ctkFLOAT,ctkHASH]) do
      begin
      aEl:=ParseComponentValue;
      aStr.Children.Add(aEl);
      end;
    Result:=aStr;
    aStr:=nil;
  finally
    aStr.Free;
  end;
end;

function TCSSParser.ParseUnicodeRange: TCSSElement;
Var
  aValue : TCSSString;
  aRange : TCSSUnicodeRangeElement;

begin
  aValue:=CurrentTokenString;
  aRange:=TCSSUnicodeRangeElement(CreateElement(TCSSUnicodeRangeElement));
  try
    Consume(ctkUnicodeRange);
    aRange.Value:=aValue;
    Result:=aRange;
    aRange:=nil;
  finally
    aRange.Free;
  end;
end;

function TCSSParser.ParseArray(aPrefix: TCSSElement): TCSSElement;

Var
  aEl : TCSSElement;
  aArray : TCSSArrayElement;

begin
  Result:=Nil;
  aArray:=TCSSArrayElement(CreateElement(TCSSArrayElement));
  try
    aArray.Prefix:=aPrefix;
    Consume(ctkLBRACKET);
    While CurrentToken<>ctkRBRACKET do
      begin
      aEl:=ParseComponentValueList;
      aArray.AddChild(aEl);
      end;
    Consume(ctkRBRACKET);
    Result:=aArray;
    aArray:=nil;
  finally
    aArray.Free;
  end;
end;

end.

